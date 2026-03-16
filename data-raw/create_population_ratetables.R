# ============================================================
# Population Mortality Rate Table Generator
# ============================================================
#
# Creates relsurv-compatible ratetable objects from multiple
# data sources for use in relative survival analysis.
#
# Data source hierarchy (best to worst resolution):
#   1. HLD  - Human Life-Table Database (single-age, gold standard)
#   2. Eurostat - EU/candidate countries (single-age, broad coverage)
#   3. WHO GHO - Global coverage (5-year age bands, interpolated)
#
# Usage:
#   Rscript data-raw/create_population_ratetables.R
#
# Run once or during release to update bundled rate tables.
# Requires: httr, jsonlite, relsurv
# Optional: eurostat (for Eurostat source)
# ============================================================

library(httr)
library(jsonlite)

# ============================================================
# SOURCE 1: Human Life-Table Database (HLD)
# ============================================================
#
# HLD (https://www.lifetable.de/) provides single-age life
# tables (TypeLT=1), which is the gold standard for ratetable
# construction. relsurv::transrate.hld() converts HLD files
# directly to ratetable objects.
#
# Turkey (TUR) data: 2013-2023, single-age resolution.
# HLD files are comma-separated with standard demographic
# columns including q(x) = probability of dying.
#
# The pooled CSV contains overlapping multi-year periods
# (rolling averages). We select non-overlapping 3-year
# windows to maximize temporal coverage without duplication.
# ============================================================

build_ratetable_from_hld <- function(country_code = "TUR",
                                     base_url = "https://www.lifetable.de/File/GetDocument/",
                                     year_min = 1960) {

    if (!requireNamespace("relsurv", quietly = TRUE)) {
        stop("relsurv package is required. Install with: install.packages('relsurv')")
    }

    cat("  [HLD] Attempting Human Life-Table Database for", country_code, "...\n")

    # Download the pooled zip file
    zip_url <- paste0(base_url, "data%5C", country_code, "%5C", country_code, ".zip")
    tmp_zip <- tempfile(fileext = ".zip")
    tmp_dir <- tempfile(pattern = "hld_")
    dir.create(tmp_dir, recursive = TRUE)

    cat("  [HLD] Downloading from:", zip_url, "\n")
    response <- tryCatch(
        httr::GET(zip_url, httr::timeout(60), httr::write_disk(tmp_zip, overwrite = TRUE)),
        error = function(e) {
            warning("  [HLD] Download failed: ", e$message)
            return(NULL)
        }
    )

    if (is.null(response) || httr::status_code(response) != 200) {
        warning("  [HLD] Download failed for ", country_code,
                " (status: ", if (!is.null(response)) httr::status_code(response) else "N/A", ")")
        unlink(tmp_zip)
        unlink(tmp_dir, recursive = TRUE)
        return(NULL)
    }

    # Check that we got a real zip file (not an error page)
    file_info <- file.info(tmp_zip)
    if (is.na(file_info$size) || file_info$size < 1000) {
        warning("  [HLD] Downloaded file too small, likely not a valid zip")
        unlink(tmp_zip)
        unlink(tmp_dir, recursive = TRUE)
        return(NULL)
    }

    # Extract
    unzip_result <- tryCatch(
        utils::unzip(tmp_zip, exdir = tmp_dir),
        error = function(e) {
            warning("  [HLD] Unzip failed: ", e$message)
            return(NULL)
        }
    )

    if (is.null(unzip_result) || length(unzip_result) == 0) {
        warning("  [HLD] No files extracted from zip")
        unlink(tmp_zip)
        unlink(tmp_dir, recursive = TRUE)
        return(NULL)
    }

    # Read the pooled CSV
    csv_file <- unzip_result[grepl("\\.csv$", unzip_result)]
    if (length(csv_file) == 0) {
        # Might be a .txt file instead
        csv_file <- unzip_result[1]
    }

    cat("  [HLD] Reading:", basename(csv_file), "\n")
    all_data <- read.csv(csv_file, stringsAsFactors = FALSE)

    # Filter to TypeLT == 1 (single-age intervals)
    if (!"TypeLT" %in% names(all_data)) {
        warning("  [HLD] No TypeLT column found in data")
        unlink(tmp_zip)
        unlink(tmp_dir, recursive = TRUE)
        return(NULL)
    }
    all_data <- all_data[all_data$TypeLT == 1, ]
    if (nrow(all_data) == 0) {
        warning("  [HLD] No TypeLT==1 (single-age) data found")
        unlink(tmp_zip)
        unlink(tmp_dir, recursive = TRUE)
        return(NULL)
    }

    # Remove rows with NA in Year1 or Year2 (corrupt/incomplete records)
    all_data <- all_data[!is.na(all_data$Year1) & !is.na(all_data$Year2), ]
    if (nrow(all_data) == 0) {
        warning("  [HLD] No valid data after removing NA years")
        unlink(tmp_zip)
        unlink(tmp_dir, recursive = TRUE)
        return(NULL)
    }

    # Filter to recent data only (year_min parameter, default 1960)
    # Historical data pre-1960 is rarely needed for clinical ratetables
    # and having too many periods can cause transrate.hld() issues.
    all_data <- all_data[all_data$Year1 >= year_min, ]
    if (nrow(all_data) == 0) {
        warning("  [HLD] No data from ", year_min, " onwards")
        unlink(tmp_zip)
        unlink(tmp_dir, recursive = TRUE)
        return(NULL)
    }

    # Deduplicate: some HLD files have multiple records for the same
    # (Year1, Year2, Sex, Age) combination from different versions.
    # Keep only the first occurrence (highest version/most recent).
    key_cols <- c("Year1", "Year2", "Sex", "Age")
    all_data <- all_data[!duplicated(all_data[, key_cols]), ]

    # Identify available periods
    periods <- unique(all_data[, c("Year1", "Year2")])
    periods <- periods[order(periods$Year1, periods$Year2), ]
    cat("  [HLD] Available periods (from", year_min, "):", nrow(periods), "\n")
    for (i in seq_len(min(nrow(periods), 15))) {
        cat("    ", periods$Year1[i], "-", periods$Year2[i], "\n")
    }
    if (nrow(periods) > 15) {
        cat("    ... and", nrow(periods) - 15, "more\n")
    }

    # Strategy: select non-overlapping periods for best temporal coverage.
    # HLD Turkey has rolling 3-year averages (2013-2015, 2014-2016, ..., 2021-2023)
    # plus some shorter periods (2013-2013, 2013-2014).
    # We pick non-overlapping windows: prefer wider periods for stability.
    #
    # transrate.hld() uses Year1 as the cut year. The rates between cut
    # points are held constant, so non-overlapping periods give the best
    # representation of temporal change.

    selected_periods <- select_nonoverlapping_periods(periods)
    cat("  [HLD] Selected non-overlapping periods:", nrow(selected_periods), "\n")

    # Write each selected period as a separate .txt file for transrate.hld()
    txt_files <- character(0)
    cut_years <- integer(0)

    for (i in seq_len(nrow(selected_periods))) {
        y1 <- selected_periods$Year1[i]
        y2 <- selected_periods$Year2[i]

        period_data <- all_data[all_data$Year1 == y1 & all_data$Year2 == y2, ]
        if (nrow(period_data) == 0) next

        fname <- file.path(tmp_dir, sprintf("%s_%d-%d.txt", country_code, y1, y2))

        # Write in HLD comma-separated format (transrate.hld reads with read.table sep=",")
        write.csv(period_data, fname, row.names = FALSE, quote = FALSE)
        txt_files <- c(txt_files, fname)
        cut_years <- c(cut_years, y1)
        cat("    Written:", basename(fname), "(", nrow(period_data), "rows )\n")
    }

    if (length(txt_files) == 0) {
        warning("  [HLD] No period files could be written")
        unlink(tmp_zip)
        unlink(tmp_dir, recursive = TRUE)
        return(NULL)
    }

    # Build ratetable using relsurv::transrate.hld()
    cat("  [HLD] Building ratetable with transrate.hld()...\n")
    cat("  [HLD] Cut years:", paste(cut_years, collapse = ", "), "\n")

    rt <- tryCatch({
        if (length(txt_files) > 1) {
            relsurv::transrate.hld(file = txt_files, cut.year = cut_years)
        } else {
            relsurv::transrate.hld(file = txt_files)
        }
    }, error = function(e) {
        warning("  [HLD] transrate.hld() failed: ", e$message)
        return(NULL)
    })

    # Clean up temp files
    unlink(tmp_zip)
    unlink(tmp_dir, recursive = TRUE)

    if (!is.null(rt)) {
        cat("  [HLD] Ratetable built successfully for", country_code, "\n")
        cat("  [HLD] Dimensions:", paste(dim(rt), collapse = " x "), "\n")
    }

    return(rt)
}


# ============================================================
# Helper: Select non-overlapping periods from HLD data
# ============================================================
#
# Given a data.frame of (Year1, Year2) periods, greedily pick
# non-overlapping windows that maximize temporal coverage.
# Prefer wider windows (3-year over 1-year) for stability.
# ============================================================

select_nonoverlapping_periods <- function(periods) {
    # Sort by Year1 then by width (wider first)
    periods$width <- periods$Year2 - periods$Year1 + 1
    periods <- periods[order(periods$Year1, -periods$width), ]

    selected <- data.frame(Year1 = integer(0), Year2 = integer(0))
    last_end <- -Inf

    for (i in seq_len(nrow(periods))) {
        y1 <- periods$Year1[i]
        y2 <- periods$Year2[i]
        # Non-overlapping: this period starts after the last selected one ends
        if (y1 > last_end) {
            selected <- rbind(selected, data.frame(Year1 = y1, Year2 = y2))
            last_end <- y2
        }
    }

    return(selected)
}


# ============================================================
# SOURCE 2: Eurostat Life Tables
# ============================================================
#
# Eurostat (https://ec.europa.eu/eurostat/) provides single-age
# life tables for EU member states, candidate countries, and
# some neighbouring countries via the demo_mlifetable dataset.
#
# Includes Turkey (TR) with data from 1960-2024.
# Indicator PROBDEATH = q(x), probability of dying.
# Ages: single-year from 0 to 94, plus 95+.
#
# Coverage: EU27, EFTA, candidates (TR, ME, MK, RS, AL), and
# others (UK, GE, AM, AZ, BY, MD, UA, RU).
# ============================================================

build_ratetable_from_eurostat <- function(country_code_iso2, max_age = 100,
                                          year_min = 2000) {

    if (!requireNamespace("eurostat", quietly = TRUE)) {
        warning("  [Eurostat] eurostat package not installed, skipping")
        return(NULL)
    }
    if (!requireNamespace("relsurv", quietly = TRUE)) {
        stop("relsurv package is required. Install with: install.packages('relsurv')")
    }

    cat("  [Eurostat] Attempting Eurostat for", country_code_iso2, "...\n")

    # Fetch probability of death (qx) for both sexes
    data_raw <- tryCatch({
        eurostat::get_eurostat(
            "demo_mlifetable",
            filters = list(
                geo = country_code_iso2,
                indic_de = "PROBDEATH"
            ),
            time_format = "num"
        )
    }, error = function(e) {
        warning("  [Eurostat] API query failed: ", e$message)
        return(NULL)
    })

    if (is.null(data_raw) || nrow(data_raw) == 0) {
        warning("  [Eurostat] No data returned for ", country_code_iso2)
        return(NULL)
    }

    # Filter years >= year_min and sex M/F
    data_raw <- data_raw[data_raw$time >= year_min & data_raw$sex %in% c("M", "F"), ]
    if (nrow(data_raw) == 0) {
        warning("  [Eurostat] No data after filtering for years >= ", year_min)
        return(NULL)
    }

    # Parse age column: Y_LT1 -> 0, Y1 -> 1, ..., Y94 -> 94, Y_GE85 -> 85, Y_GE95 -> 95
    data_raw$age_num <- NA_integer_
    data_raw$age_num[data_raw$age == "Y_LT1"] <- 0L

    # Standard single-year ages: Y1, Y2, ..., Y94
    idx_single <- grepl("^Y\\d+$", data_raw$age)
    data_raw$age_num[idx_single] <- as.integer(sub("^Y", "", data_raw$age[idx_single]))

    # Open-ended groups: Y_GE85, Y_GE95
    idx_ge <- grepl("^Y_GE\\d+$", data_raw$age)
    data_raw$age_num[idx_ge] <- as.integer(sub("^Y_GE", "", data_raw$age[idx_ge]))

    # Keep only rows with valid age and value
    data_raw <- data_raw[!is.na(data_raw$age_num) & !is.na(data_raw$values), ]

    # For ages with multiple entries (e.g., Y85 and Y_GE85), keep single-year
    data_raw <- data_raw[!grepl("^Y_GE", data_raw$age) | !data_raw$age_num %in% 85:94, ]

    if (nrow(data_raw) == 0) {
        warning("  [Eurostat] No valid data after age parsing")
        return(NULL)
    }

    years <- sort(unique(as.integer(data_raw$time)))
    cat("  [Eurostat] Years:", min(years), "-", max(years),
        "(", length(years), "years )\n")
    cat("  [Eurostat] Age range:", min(data_raw$age_num), "-", max(data_raw$age_num), "\n")

    # Build survival probability matrices for each sex
    # relsurv::transrate() expects px (survival prob) matrices:
    #   rows = ages 0..max_age, cols = years
    build_px <- function(sex_code) {
        sdata <- data_raw[data_raw$sex == sex_code, ]
        px <- matrix(NA_real_, nrow = max_age + 1, ncol = length(years))
        rownames(px) <- 0:max_age
        colnames(px) <- years

        for (yr_idx in seq_along(years)) {
            yr <- years[yr_idx]
            yr_data <- sdata[as.integer(sdata$time) == yr, ]
            yr_data <- yr_data[order(yr_data$age_num), ]

            for (i in seq_len(nrow(yr_data))) {
                a <- yr_data$age_num[i]
                qx <- yr_data$values[i]
                if (a <= max_age && !is.na(qx)) {
                    if (qx >= 1) qx <- 0.999
                    if (qx < 0) qx <- 0
                    px[a + 1, yr_idx] <- 1 - qx
                }
            }

            # Fill ages beyond what Eurostat provides (95+) by carrying forward
            # the last known qx value
            last_known <- NA_real_
            for (a in seq_len(nrow(px))) {
                if (!is.na(px[a, yr_idx])) {
                    last_known <- px[a, yr_idx]
                } else if (!is.na(last_known)) {
                    px[a, yr_idx] <- last_known
                } else {
                    px[a, yr_idx] <- 0.99  # safe default for young ages
                }
            }
        }
        return(px)
    }

    men_px <- build_px("M")
    women_px <- build_px("F")

    # Check both have data
    if (all(is.na(men_px)) || all(is.na(women_px))) {
        warning("  [Eurostat] Missing data for one or both sexes")
        return(NULL)
    }

    year_range <- range(years)
    cat("  [Eurostat] Building ratetable: years", year_range[1], "-", year_range[2], "\n")

    rt <- tryCatch(
        relsurv::transrate(
            men = men_px,
            women = women_px,
            yearlim = year_range,
            int.length = 1
        ),
        error = function(e) {
            warning("  [Eurostat] transrate() failed: ", e$message)
            return(NULL)
        }
    )

    if (!is.null(rt)) {
        cat("  [Eurostat] Ratetable built successfully for", country_code_iso2, "\n")
    }

    return(rt)
}


# ============================================================
# SOURCE 3: WHO Global Health Observatory (fallback)
# ============================================================
#
# WHO GHO API provides nqx (probability of dying between exact
# ages x and x+n) by 5-year age group, sex, and year.
# This is the broadest source but lowest resolution.
#
# Indicator: LIFE_0000000030 = nqx
# API docs: https://www.who.int/data/gho/info/gho-odata-api
# ============================================================

fetch_who_nqx <- function(country_code, indicator = "LIFE_0000000030") {
    base_url <- "https://ghoapi.azureedge.net/api/"
    # Build URL with proper encoding for OData query parameters
    filter_str <- paste0("SpatialDim eq '", country_code, "' and TimeDim ge 2000")
    orderby_str <- "TimeDim,Dim1,Dim2"

    cat("  [WHO] Fetching WHO data for", country_code, "...\n")

    url <- paste0(base_url, indicator)
    response <- httr::GET(
        url,
        query = list(
            `$filter` = filter_str,
            `$orderby` = orderby_str
        ),
        httr::timeout(60)
    )
    if (httr::status_code(response) != 200) {
        warning("  [WHO] API returned status ", httr::status_code(response),
            " for ", country_code)
        return(NULL)
    }

    content <- httr::content(response, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content)

    if (is.null(data$value) || nrow(data$value) == 0) {
        warning("  [WHO] No data returned for ", country_code)
        return(NULL)
    }

    df <- data$value
    cat("  [WHO] Retrieved", nrow(df), "records\n")
    return(df)
}

parse_who_age <- function(age_dim) {
    # WHO GHO uses format: "AGEGROUP_YEARSXX-YY" or "AGEGROUP_YEARS85PLUS"
    age_num <- rep(NA_real_, length(age_dim))

    m <- regmatches(age_dim, regexpr("YEARS(\\d+)", age_dim))
    idx <- nchar(m) > 0
    age_num[idx] <- as.numeric(sub("YEARS", "", m[idx]))

    idx_plus <- grepl("PLUS", age_dim)
    m_plus <- regmatches(age_dim[idx_plus], regexpr("\\d+", age_dim[idx_plus]))
    if (length(m_plus) > 0) {
        age_num[idx_plus] <- as.numeric(m_plus)
    }

    return(age_num)
}

build_survival_matrix <- function(who_data, sex_code, max_age = 100) {
    sex_data <- who_data[who_data$Dim1 == sex_code, ]

    if (nrow(sex_data) == 0) {
        warning("  [WHO] No data for sex code: ", sex_code)
        return(NULL)
    }

    sex_data$age_start <- parse_who_age(sex_data$Dim2)
    sex_data$year <- as.integer(sex_data$TimeDim)
    sex_data$nqx <- as.numeric(sex_data$NumericValue)

    sex_data <- sex_data[!is.na(sex_data$age_start) &
                         !is.na(sex_data$year) &
                         !is.na(sex_data$nqx), ]

    if (nrow(sex_data) == 0) {
        warning("  [WHO] No valid data after parsing")
        return(NULL)
    }

    years <- sort(unique(sex_data$year))
    ages_available <- sort(unique(sex_data$age_start))

    cat("  [WHO] Sex:", sex_code, "| Years:", min(years), "-", max(years),
        "| Age groups:", length(ages_available), "\n")

    # WHO provides nqx for 5-year age groups typically.
    # Interpolate to single-year ages:
    #   For age group [x, x+n): annual qx ~ 1 - (1 - nqx)^(1/n)
    px_matrix <- matrix(NA_real_, nrow = max_age + 1, ncol = length(years))
    rownames(px_matrix) <- 0:max_age
    colnames(px_matrix) <- years

    for (yr_idx in seq_along(years)) {
        yr <- years[yr_idx]
        yr_data <- sex_data[sex_data$year == yr, ]
        yr_data <- yr_data[order(yr_data$age_start), ]

        for (i in seq_len(nrow(yr_data))) {
            age_start <- yr_data$age_start[i]
            nqx <- yr_data$nqx[i]

            if (i < nrow(yr_data)) {
                n <- yr_data$age_start[i + 1] - age_start
            } else {
                n <- 5
            }
            if (n <= 0) n <- 1

            if (nqx >= 1) nqx <- 0.999
            if (nqx < 0) nqx <- 0

            px_annual <- (1 - nqx)^(1/n)

            for (a in age_start:min(age_start + n - 1, max_age)) {
                px_matrix[a + 1, yr_idx] <- px_annual
            }
        }

        for (a in seq_len(nrow(px_matrix))) {
            if (is.na(px_matrix[a, yr_idx])) {
                if (a > 1 && !is.na(px_matrix[a - 1, yr_idx])) {
                    px_matrix[a, yr_idx] <- px_matrix[a - 1, yr_idx]
                } else {
                    px_matrix[a, yr_idx] <- 0.99
                }
            }
        }
    }

    return(px_matrix)
}

build_ratetable_from_who <- function(country_code) {
    who_data <- fetch_who_nqx(country_code)
    if (is.null(who_data)) return(NULL)

    men_px <- build_survival_matrix(who_data, sex_code = "SEX_MLE")
    women_px <- build_survival_matrix(who_data, sex_code = "SEX_FMLE")

    if (is.null(men_px) || is.null(women_px)) {
        warning("  [WHO] Could not build rate table for ", country_code)
        return(NULL)
    }

    common_years <- intersect(colnames(men_px), colnames(women_px))
    if (length(common_years) == 0) {
        warning("  [WHO] No overlapping years between male and female data for ", country_code)
        return(NULL)
    }
    men_px <- men_px[, common_years, drop = FALSE]
    women_px <- women_px[, common_years, drop = FALSE]

    year_range <- range(as.integer(common_years))

    cat("  [WHO] Building ratetable: years", year_range[1], "-", year_range[2], "\n")

    if (!requireNamespace("relsurv", quietly = TRUE)) {
        stop("relsurv package is required. Install with: install.packages('relsurv')")
    }

    rt <- relsurv::transrate(
        men    = men_px,
        women  = women_px,
        yearlim    = year_range,
        int.length = 1
    )

    cat("  [WHO] Ratetable built successfully for", country_code, "\n")
    return(rt)
}


# ============================================================
# Multi-source orchestrator: try sources in priority order
# ============================================================
#
# For a given country, try data sources from best to worst:
#   1. HLD (if country_code_hld is provided)
#   2. Eurostat (if country_code_eurostat is provided)
#   3. WHO GHO (always available as fallback)
#
# Returns the first successful ratetable.
# ============================================================

build_ratetable_multisource <- function(country_code_who,
                                        country_code_hld = NULL,
                                        country_code_eurostat = NULL) {

    # Source 1: HLD (best resolution)
    if (!is.null(country_code_hld)) {
        rt <- tryCatch(
            build_ratetable_from_hld(country_code_hld),
            error = function(e) {
                cat("  [HLD] Error:", e$message, "\n")
                return(NULL)
            }
        )
        if (!is.null(rt)) {
            cat("  >> Used source: HLD (single-age life tables)\n")
            attr(rt, "source") <- "HLD"
            attr(rt, "source_detail") <- paste0("Human Life-Table Database (lifetable.de), country=", country_code_hld)
            return(rt)
        }
        cat("  [HLD] Failed, trying next source...\n")
    }

    # Source 2: Eurostat (single-age, good coverage for EU/candidates)
    if (!is.null(country_code_eurostat)) {
        rt <- tryCatch(
            build_ratetable_from_eurostat(country_code_eurostat),
            error = function(e) {
                cat("  [Eurostat] Error:", e$message, "\n")
                return(NULL)
            }
        )
        if (!is.null(rt)) {
            cat("  >> Used source: Eurostat (single-age life tables)\n")
            attr(rt, "source") <- "Eurostat"
            attr(rt, "source_detail") <- paste0("Eurostat demo_mlifetable, geo=", country_code_eurostat)
            return(rt)
        }
        cat("  [Eurostat] Failed, trying next source...\n")
    }

    # Source 3: WHO GHO (broadest coverage, 5-year bands interpolated)
    rt <- tryCatch(
        build_ratetable_from_who(country_code_who),
        error = function(e) {
            cat("  [WHO] Error:", e$message, "\n")
            return(NULL)
        }
    )
    if (!is.null(rt)) {
        cat("  >> Used source: WHO GHO (5-year age bands, interpolated)\n")
        attr(rt, "source") <- "WHO"
        attr(rt, "source_detail") <- paste0("WHO Global Health Observatory API, country=", country_code_who)
        return(rt)
    }

    warning("All sources failed for ", country_code_who)
    return(NULL)
}


# ============================================================
# Main: Build ratetables for selected countries
# ============================================================

cat("============================================================\n")
cat("Population Mortality Rate Table Generator\n")
cat("Sources: HLD > Eurostat > WHO GHO (in priority order)\n")
cat("============================================================\n\n")

# Country registry: WHO code, HLD code, Eurostat ISO2 code
# NULL means the source is not available for that country.
#
# HLD coverage: https://www.lifetable.de/ (check per country)
# Eurostat coverage: EU27 + EFTA + candidates + neighbours
# WHO coverage: nearly universal
countries <- list(
    turkey = list(
        who = "TUR",
        hld = "TUR",          # HLD has Turkey 2013-2023
        eurostat = "TR"        # Eurostat has Turkey 1960-2024
    ),
    germany = list(
        who = "DEU",
        hld = "DEU",          # HLD has Germany
        eurostat = "DE"
    ),
    uk = list(
        who = "GBR",
        hld = "GBR",          # HLD has United Kingdom
        eurostat = "UK"
    ),
    japan = list(
        who = "JPN",
        hld = "JPN",          # HLD has Japan
        eurostat = NULL        # Not in Eurostat
    ),
    italy = list(
        who = "ITA",
        hld = "ITA",          # HLD has Italy
        eurostat = "IT"
    ),
    spain = list(
        who = "ESP",
        hld = "ESP",          # HLD has Spain
        eurostat = "ES"
    ),
    brazil = list(
        who = "BRA",
        hld = NULL,            # Not in HLD
        eurostat = NULL         # Not in Eurostat
    ),
    south_korea = list(
        who = "KOR",
        hld = "KOR",          # HLD has South Korea
        eurostat = NULL
    ),
    china = list(
        who = "CHN",
        hld = NULL,            # Not in HLD
        eurostat = NULL
    ),
    india = list(
        who = "IND",
        hld = NULL,            # Not in HLD
        eurostat = NULL
    )
)

# ============================================================
# Build individual Turkey ratetable (primary use case)
# ============================================================
# Turkey is the primary use case for this package.
# HLD provides single-age data (2013-2023) -- much better than
# WHO's 5-year bands which require interpolation.
# Eurostat provides an even longer time series (1960-2024).
# ============================================================

cat("============================================================\n")
cat("--- Building Turkey ratetable (primary) ---\n")
cat("============================================================\n")

turkey_info <- countries[["turkey"]]
ratetable_turkey <- build_ratetable_multisource(
    country_code_who = turkey_info$who,
    country_code_hld = turkey_info$hld,
    country_code_eurostat = turkey_info$eurostat
)

if (!is.null(ratetable_turkey)) {
    save(ratetable_turkey, file = "data/ratetable_turkey.rda", compress = "xz")
    cat("Saved: data/ratetable_turkey.rda\n")
    cat("  Source:", attr(ratetable_turkey, "source"), "\n")
    cat("  Detail:", attr(ratetable_turkey, "source_detail"), "\n\n")
} else {
    cat("FAILED: Could not build Turkey ratetable from any source\n\n")
}

# ============================================================
# Build collection for all countries
# ============================================================

cat("============================================================\n")
cat("--- Building multi-source ratetable collection ---\n")
cat("============================================================\n")

ratetable_who_collection <- list()
source_log <- list()

for (name in names(countries)) {
    info <- countries[[name]]
    cat("\n--- ", toupper(name), " (WHO:", info$who, ") ---\n")

    rt <- tryCatch(
        build_ratetable_multisource(
            country_code_who = info$who,
            country_code_hld = info$hld,
            country_code_eurostat = info$eurostat
        ),
        error = function(e) {
            cat("  FATAL ERROR:", e$message, "\n")
            return(NULL)
        }
    )

    if (!is.null(rt)) {
        ratetable_who_collection[[name]] <- rt
        source_log[[name]] <- attr(rt, "source")
    } else {
        source_log[[name]] <- "FAILED"
    }
}

if (length(ratetable_who_collection) > 0) {
    save(ratetable_who_collection, file = "data/ratetable_who_collection.rda", compress = "xz")
    cat("\nSaved: data/ratetable_who_collection.rda\n")
    cat("Countries included:", paste(names(ratetable_who_collection), collapse = ", "), "\n")
} else {
    cat("\nWARNING: No ratetables were successfully built\n")
}

# ============================================================
# Summary
# ============================================================

cat("\n============================================================\n")
cat("Done. Summary:\n")
cat("============================================================\n")
cat(sprintf("  %-15s %-10s %s\n", "Country", "Status", "Source"))
cat(sprintf("  %-15s %-10s %s\n", "-------", "------", "------"))
for (name in names(countries)) {
    status <- if (name %in% names(ratetable_who_collection)) "OK" else "FAILED"
    src <- source_log[[name]]
    cat(sprintf("  %-15s %-10s %s\n", name, status, src))
}
cat("\n  Turkey (standalone):", if (!is.null(ratetable_turkey)) "OK" else "FAILED", "\n")
cat("  Collection:", length(ratetable_who_collection), "/", length(countries), "countries\n")
cat("============================================================\n")
