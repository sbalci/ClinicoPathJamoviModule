#' @title Relative Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

relativesurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "relativesurvivalClass",
    inherit = relativesurvivalBase,
    private = list(

        # ── Internal storage ──────────────────────────────────────────────
        # TODO (cleanup): .plot_data / .excess_plot_data / .prepared_data / .rate_table are
        # dead stores — written in .run()/.generatePlotData/.calculateExcessMortality but never
        # read back as fields (plots read image$state via setState; data/rate_table flow via
        # function args). They're harmless (no stale-cache bug since never read) but should be
        # dropped, or wired up if intended. .noticeList is the only field actually read (reset L92).
        .plot_data = NULL,
        .excess_plot_data = NULL,
        .prepared_data = NULL,
        .rate_table = NULL,
        .noticeList = list(),

        # ── .init ─────────────────────────────────────────────────────────
        .init = function() {

            # Show welcome message when no variables are selected
            if (is.null(self$options$time) ||
                is.null(self$options$status) ||
                is.null(self$options$age) ||
                is.null(self$options$sex) ||
                is.null(self$options$year)) {

                self$results$todo$setContent(
                    "<h3>Welcome to Relative Survival Analysis</h3>
                    <p>Relative survival compares observed patient survival to expected
                    survival in a matched general population.</p>

                    <h4>Key Concepts:</h4>
                    <ul>
                    <li><b>Observed Survival:</b> Actual survival in your patient cohort</li>
                    <li><b>Expected Survival:</b> Survival in matched general population</li>
                    <li><b>Relative Survival:</b> Ratio of observed to expected (proxy for disease-specific survival)</li>
                    <li><b>Excess Mortality:</b> Additional mortality due to disease</li>
                    </ul>

                    <h4>Required Variables:</h4>
                    <ul>
                    <li>Follow-up time (in days, months, or years)</li>
                    <li>Vital status (0 = alive/censored, 1 = dead)</li>
                    <li>Age at diagnosis (numeric, in years)</li>
                    <li>Sex (factor: male/female)</li>
                    <li>Calendar year of diagnosis (numeric)</li>
                    </ul>

                    <h4>Applications:</h4>
                    <ul>
                    <li>Cancer registry studies</li>
                    <li>Population-based survival comparisons</li>
                    <li>Net survival estimation (Pohar-Perme recommended)</li>
                    <li>International cancer survival comparisons</li>
                    </ul>

                    <p>Please select all five required variables to begin analysis.</p>"
                )
                return()
            }

            self$results$todo$setContent("")
        },

        # ── .run ──────────────────────────────────────────────────────────
        .run = function() {

            # Guard: all required variables must be selected
            if (is.null(self$options$time) ||
                is.null(self$options$status) ||
                is.null(self$options$age) ||
                is.null(self$options$sex) ||
                is.null(self$options$year)) {
                return()
            }

            # Check for relsurv package
            if (!requireNamespace('relsurv', quietly = TRUE)) {
                jmvcore::reject(
                    "The {relsurv} package is required for relative survival analysis. Please install it: install.packages('relsurv')"
                )
            }

            if (!requireNamespace('survival', quietly = TRUE)) {
                jmvcore::reject(
                    "The {survival} package is required. Please install it: install.packages('survival')"
                )
            }

            # Reset notices
            private$.noticeList <- list()

            # Prepare data
            data <- private$.prepareData()
            if (is.null(data)) return()
            private$.prepared_data <- data

            # Clinical threshold checks: events
            n_events <- sum(data$status_num, na.rm = TRUE)
            n_total <- nrow(data)

            if (n_events < 10) {
                jmvcore::reject(
                    "Only {n} events detected in {total} observations. Relative survival analysis requires at least 10 events for minimally reliable estimates.",
                    n = n_events, total = n_total
                )
            } else if (n_events < 20) {
                private$.addNotice("strong_warning", "Low Event Count",
                    paste0("Only ", n_events, " events in ", n_total,
                           " observations. Estimates may be unstable; interpret with caution."))
            } else if (n_events < 50) {
                private$.addNotice("warning", "Moderate Event Count",
                    paste0(n_events, " events in ", n_total,
                           " observations. Adequate for basic estimates; subgroup analyses may be unreliable."))
            }

            # EPV check for regression
            if (self$options$regression_model != "none" && length(self$options$covariates) > 0) {
                n_covariates <- length(self$options$covariates)
                epv <- n_events / n_covariates
                if (epv < 10) {
                    private$.addNotice("strong_warning", "Low Events Per Variable",
                        paste0("EPV = ", round(epv, 1), " (", n_events, " events / ",
                               n_covariates, " covariates). EPV < 10 may produce unreliable regression estimates; consider reducing covariates."))
                }
            }

            # Get population rate table
            rate_table <- private$.getRateTable()
            if (is.null(rate_table)) return()
            private$.rate_table <- rate_table

            # Calculate relative/net survival
            rel_surv <- private$.calculateRelativeSurvival(data, rate_table)
            if (is.null(rel_surv)) return()

            # Calculate observed (KM) and expected (survexp) for tables/plots
            km_fit <- private$.calculateObservedSurvival(data)
            exp_fit <- private$.calculateExpectedSurvival(data, rate_table)

            # Display main survival table
            private$.displaySurvivalTable(rel_surv, km_fit, exp_fit)

            # Net survival table
            if (self$options$net_survival) {
                private$.displayNetSurvivalTable(rel_surv)
            }

            # Excess mortality
            if (self$options$excess_mortality) {
                private$.calculateExcessMortality(data, rate_table, rel_surv)
            }

            # Crude probability
            if (self$options$crude_probability) {
                private$.calculateCrudeProbabilities(data, rate_table)
            }

            # Age standardization
            if (self$options$age_standardized) {
                private$.calculateAgeStandardized(data, rate_table)
            }

            # Period analysis
            if (self$options$period_analysis) {
                private$.calculatePeriodAnalysis(data, rate_table)
            }

            # Regression models
            if (self$options$regression_model != "none") {
                private$.fitRegressionModel(data, rate_table)
            }

            # Generate plot data (store as serializable data.frames)
            private$.generatePlotData(rel_surv, km_fit, exp_fit)

            # Summary and interpretation
            private$.displaySummary(rel_surv, data)
            private$.displayInterpretation(rel_surv)

            # Completion notice (n_events already computed above)
            private$.addNotice("info", "Analysis Complete",
                paste0("Relative survival analysis completed: ", n_total,
                       " patients, ", n_events, " events, method = ",
                       private$.methodLabel(), "."))

            # Render all notices
            private$.renderNotices()
        },

        # ── Data Preparation ──────────────────────────────────────────────
        .prepareData = function() {

            time_var <- self$options$time
            status_var <- self$options$status
            age_var <- self$options$age
            sex_var <- self$options$sex
            year_var <- self$options$year
            covariates <- self$options$covariates

            # Collect needed variables
            vars_needed <- c(time_var, status_var, age_var, sex_var, year_var)
            if (length(covariates) > 0) {
                vars_needed <- c(vars_needed, covariates)
            }

            # Get data and complete cases
            data <- jmvcore::select(self$data, vars_needed)
            data <- jmvcore::naOmit(data)

            if (nrow(data) < 30) {
                jmvcore::reject(
                    "Insufficient data for relative survival analysis. Found {n} complete cases, but a minimum of 30 is required.",
                    n = nrow(data)
                )
            }

            # Convert follow-up time to years
            time_scale <- self$options$time_scale
            data$time_years <- as.numeric(data[[time_var]])
            if (time_scale == "months") {
                data$time_years <- data$time_years / 12
            } else if (time_scale == "days") {
                data$time_years <- data$time_years / 365.25
            }

            # Ensure positive times
            if (any(data$time_years <= 0, na.rm = TRUE)) {
                data <- data[data$time_years > 0, ]
                self$results$survivalTable$setNote(
                    "time_filter",
                    "Some observations with non-positive follow-up times were removed."
                )
            }

            # Convert time to days for relsurv (ratetable expects days)
            data$time_days <- data$time_years * 365.25

            # Status variable: ensure numeric 0/1
            status_raw <- data[[status_var]]
            if (is.factor(status_raw)) {
                lvls <- levels(status_raw)
                if (length(lvls) == 2) {
                    data$status_num <- as.numeric(status_raw == lvls[2])
                    self$results$survivalTable$setNote(
                        "status_levels",
                        paste0("Vital status is a factor. Level '", htmltools::htmlEscape(lvls[2]),
                               "' treated as event (death); '", htmltools::htmlEscape(lvls[1]),
                               "' treated as censored.")
                    )
                } else {
                    jmvcore::reject(
                        "Vital status must be a binary variable (0/1 or two-level factor). Found {n} levels.",
                        n = length(lvls)
                    )
                }
            } else {
                data$status_num <- as.numeric(data[[status_var]])
                unique_vals <- sort(unique(data$status_num[!is.na(data$status_num)]))
                if (!all(unique_vals %in% c(0, 1))) {
                    jmvcore::reject(
                        "Vital status must contain only 0 (alive/censored) and 1 (dead). Found values: {vals}",
                        vals = paste(unique_vals, collapse = ", ")
                    )
                }
            }

            # Age: validate and convert to days for ratetable
            data$age_years <- as.numeric(data[[age_var]])
            if (any(data$age_years < 0 | data$age_years > 120, na.rm = TRUE)) {
                out_of_range <- sum(data$age_years < 0 | data$age_years > 120, na.rm = TRUE)
                jmvcore::reject(
                    "Age values must be between 0 and 120 years. Found {n} out-of-range values.",
                    n = out_of_range
                )
            }
            data$age_days <- data$age_years * 365.25

            # Sex: normalize to match ratetable levels ("male"/"female")
            sex_col <- data[[sex_var]]
            sex_char <- tolower(as.character(sex_col))

            male_patterns <- c("male", "m", "erkek", "1", "man", "homme", "masculino")
            female_patterns <- c("female", "f", "kadin", "2", "woman", "femme", "femenino")

            sex_mapped <- rep(NA_character_, length(sex_char))
            sex_mapped[sex_char %in% male_patterns] <- "male"
            sex_mapped[sex_char %in% female_patterns] <- "female"

            if (any(is.na(sex_mapped))) {
                n_unmapped <- sum(is.na(sex_mapped))
                unmapped_vals <- unique(sex_char[is.na(sex_mapped)])
                jmvcore::reject(
                    "Could not map sex values to male/female. Unrecognized values: {vals} ({n} observations). Please recode sex as 'male'/'female' or 'm'/'f'.",
                    vals = paste(unmapped_vals, collapse = ", "),
                    n = n_unmapped
                )
            }

            data$sex_relsurv <- factor(sex_mapped, levels = c("male", "female"))

            # Year: create diagnosis date
            year_num <- as.numeric(data[[year_var]])
            if (any(year_num < 1900 | year_num > 2100, na.rm = TRUE)) {
                jmvcore::reject(
                    "Calendar year values appear invalid. Expected years between 1900-2100."
                )
            }
            data$diagdate <- as.Date(paste0(round(year_num), "-07-01"))

            return(data)
        },

        # ── Rate Table ────────────────────────────────────────────────────
        .getRateTable = function() {

            ratetable_choice <- self$options$ratetable

            load_who_ratetable <- function(country_name) {
                rt <- NULL
                tryCatch({
                    env <- new.env()
                    data("ratetable_who_collection", package = "ClinicoPath", envir = env)
                    collection <- get("ratetable_who_collection", envir = env)
                    if (country_name %in% names(collection)) {
                        rt <- collection[[country_name]]
                    }
                }, error = function(e) {
                    # silently fail
                })
                return(rt)
            }

            rate_table <- switch(ratetable_choice,
                "us"          = relsurv::survexp.us,
                "mn"          = relsurv::survexp.mn,
                "fr"          = relsurv::survexp.fr,
                "slovenia"    = relsurv::slopop,
                "turkey"      = {
                    rt <- NULL
                    tryCatch({
                        env <- new.env()
                        data("ratetable_turkey", package = "ClinicoPath", envir = env)
                        rt <- get("ratetable_turkey", envir = env)
                    }, error = function(e) {
                        rt <<- load_who_ratetable("turkey")
                    })
                    rt
                },
                "germany"     = load_who_ratetable("germany"),
                "uk"          = load_who_ratetable("uk"),
                "italy"       = load_who_ratetable("italy"),
                "japan"       = load_who_ratetable("japan"),
                "spain"       = load_who_ratetable("spain"),
                "brazil"      = load_who_ratetable("brazil"),
                "south_korea" = load_who_ratetable("south_korea"),
                "china"       = load_who_ratetable("china"),
                "india"       = load_who_ratetable("india"),
                "custom"      = NULL
            )

            if (is.null(rate_table)) {
                rate_table <- relsurv::survexp.us
                self$results$survivalTable$setNote(
                    "ratetable_fallback",
                    "Selected population rate table was not available. Using US population table as default."
                )
            }

            return(rate_table)
        },

        # ── Relative/Net Survival Calculation ─────────────────────────────
        .calculateRelativeSurvival = function(data, rate_table) {

            method_map <- c(
                "poharperme" = "pohar-perme",
                "ederer2"    = "ederer2",
                "ederer1"    = "ederer1",
                "hakulinen"  = "hakulinen"
            )
            rs_method <- method_map[self$options$method]

            rel_surv <- tryCatch({
                relsurv::rs.surv(
                    survival::Surv(time_days, status_num) ~ 1,
                    rmap = list(
                        age = age_days,
                        sex = sex_relsurv,
                        year = diagdate
                    ),
                    ratetable = rate_table,
                    data = data,
                    method = rs_method,
                    conf.int = self$options$confidence_level
                )
            }, error = function(e) {
                jmvcore::reject(
                    "Relative survival calculation failed: {msg}",
                    msg = e$message
                )
                return(NULL)
            })

            return(rel_surv)
        },

        # ── Observed (Kaplan-Meier) Survival ──────────────────────────────
        .calculateObservedSurvival = function(data) {
            tryCatch({
                survival::survfit(
                    survival::Surv(time_days, status_num) ~ 1,
                    data = data
                )
            }, error = function(e) NULL)
        },

        # ── Expected (Population) Survival ────────────────────────────────
        .calculateExpectedSurvival = function(data, rate_table) {
            tryCatch({
                survival::survexp(
                    ~ 1,
                    rmap = list(
                        age = age_days,
                        sex = sex_relsurv,
                        year = diagdate
                    ),
                    ratetable = rate_table,
                    data = data
                )
            }, error = function(e) NULL)
        },

        # ── Display Survival Table ────────────────────────────────────────
        .displaySurvivalTable = function(rel_surv, km_fit, exp_fit) {

            table <- self$results$survivalTable

            timepoints <- private$.parseTimepoints(self$options$timepoints)
            if (length(timepoints) == 0) timepoints <- c(1, 3, 5, 10)

            # rel_surv times are in days
            max_time_years <- max(rel_surv$time / 365.25, na.rm = TRUE)
            timepoints <- timepoints[timepoints <= max_time_years * 1.1]
            if (length(timepoints) == 0) timepoints <- c(max_time_years)

            table$deleteRows()

            for (i in seq_along(timepoints)) {
                tp <- timepoints[i]
                tp_days <- tp * 365.25

                # Net/relative survival from rs.surv (step-function lookup)
                idx_rs <- private$.stepIdx(rel_surv$time, tp_days)
                net_surv <- if (!is.na(idx_rs)) rel_surv$surv[idx_rs] else NA
                ci_lower <- if (!is.null(rel_surv$lower) && !is.na(idx_rs)) rel_surv$lower[idx_rs] else NA
                ci_upper <- if (!is.null(rel_surv$upper) && !is.na(idx_rs)) rel_surv$upper[idx_rs] else NA

                # Observed (KM) survival
                obs_surv <- if (!is.null(km_fit))
                    private$.stepLookup(km_fit$time, km_fit$surv, tp_days) else NA

                # Expected (population) survival
                exp_surv <- if (!is.null(exp_fit))
                    private$.stepLookup(exp_fit$time, exp_fit$surv, tp_days) else NA

                table$addRow(rowKey = i, values = list(
                    time_point = tp,
                    observed = if (!is.na(obs_surv)) round(obs_surv, 4) else NA,
                    expected = if (!is.na(exp_surv)) round(exp_surv, 4) else NA,
                    relative = if (!is.na(net_surv)) round(net_surv, 4) else NA,
                    rel_ci_lower = if (!is.na(ci_lower)) round(ci_lower, 4) else NA,
                    rel_ci_upper = if (!is.na(ci_upper)) round(ci_upper, 4) else NA
                ))
            }

            method_names <- c(
                "poharperme" = "Pohar-Perme (Net Survival)",
                "ederer2"    = "Ederer II",
                "ederer1"    = "Ederer I",
                "hakulinen"  = "Hakulinen"
            )
            table$setNote(
                "method",
                paste0("Method: ", method_names[self$options$method], ". Time scale: years.")
            )
        },

        # ── Net Survival Table ────────────────────────────────────────────
        .displayNetSurvivalTable = function(rel_surv) {

            table <- self$results$netSurvivalTable

            timepoints <- private$.parseTimepoints(self$options$timepoints)
            if (length(timepoints) == 0) timepoints <- c(1, 3, 5, 10)

            max_time_years <- max(rel_surv$time / 365.25, na.rm = TRUE)
            timepoints <- timepoints[timepoints <= max_time_years * 1.1]
            if (length(timepoints) == 0) timepoints <- c(max_time_years)

            table$deleteRows()

            for (i in seq_along(timepoints)) {
                tp <- timepoints[i]
                tp_days <- tp * 365.25
                idx <- private$.stepIdx(rel_surv$time, tp_days)

                if (!is.na(idx)) {
                    net_surv <- rel_surv$surv[idx]
                    se <- if (!is.null(rel_surv$std.err)) rel_surv$std.err[idx] else NA
                    ci_lower <- if (!is.null(rel_surv$lower)) rel_surv$lower[idx] else NA
                    ci_upper <- if (!is.null(rel_surv$upper)) rel_surv$upper[idx] else NA

                    table$addRow(rowKey = i, values = list(
                        time_point = tp,
                        net_survival = round(net_surv, 4),
                        net_ci_lower = if (!is.na(ci_lower)) round(ci_lower, 4) else NA,
                        net_ci_upper = if (!is.na(ci_upper)) round(ci_upper, 4) else NA,
                        standard_error = if (!is.na(se)) round(se, 4) else NA
                    ))
                }
            }
        },

        # ── Excess Mortality ──────────────────────────────────────────────
        .calculateExcessMortality = function(data, rate_table, rel_surv) {

            table <- self$results$excessMortalityTable

            tryCatch({
                max_years <- max(data$time_years, na.rm = TRUE)
                yearly <- seq(1, min(floor(max_years), 10))

                if (length(yearly) == 0) {
                    table$setNote("info", "Follow-up too short for yearly excess mortality estimates.")
                    return()
                }

                table$deleteRows()

                excess_vals <- numeric(length(yearly))
                has_se <- !is.null(rel_surv$std.err) && length(rel_surv$std.err) > 0
                z_crit <- qnorm(1 - (1 - self$options$confidence_level) / 2)

                for (i in seq_along(yearly)) {
                    yr <- yearly[i]
                    yr_days <- yr * 365.25
                    prev_yr_days <- (yr - 1) * 365.25

                    idx <- private$.stepIdx(rel_surv$time, yr_days)
                    prev_idx <- private$.stepIdx(rel_surv$time, prev_yr_days)

                    surv_end <- if (!is.na(idx)) rel_surv$surv[idx] else NA
                    surv_start <- if (yr == 1) 1.0 else {
                        if (!is.na(prev_idx)) rel_surv$surv[prev_idx] else NA
                    }

                    # Interval excess hazard: -log(S(t)/S(t-1))
                    excess_haz <- NA
                    if (!is.na(surv_end) && !is.na(surv_start) &&
                        surv_start > 0 && surv_end > 0) {
                        excess_haz <- -log(surv_end / surv_start)
                    }
                    excess_vals[i] <- excess_haz

                    # Delta method CIs using rs.surv std.err (on log-survival scale)
                    ci_lower_val <- NA
                    ci_upper_val <- NA
                    p_val <- NA

                    if (has_se && !is.na(excess_haz)) {
                        se_end <- if (!is.na(idx) && idx <= length(rel_surv$std.err))
                            rel_surv$std.err[idx] else NA
                        se_start <- if (yr == 1) 0 else {
                            if (!is.na(prev_idx) && prev_idx <= length(rel_surv$std.err))
                                rel_surv$std.err[prev_idx] else NA
                        }

                        if (!is.na(se_end) && !is.na(se_start)) {
                            se_excess <- sqrt(se_end^2 + se_start^2)
                            if (se_excess > 0) {
                                ci_lower_val <- excess_haz - z_crit * se_excess
                                ci_upper_val <- excess_haz + z_crit * se_excess
                                p_val <- 2 * pnorm(-abs(excess_haz / se_excess))
                            }
                        }
                    }

                    interval_label <- paste0(yr - 1, "-", yr, " years")

                    table$addRow(rowKey = i, values = list(
                        time_interval = interval_label,
                        excess_hazard = if (!is.na(excess_haz)) round(excess_haz, 4) else NA,
                        hazard_ci_lower = if (!is.na(ci_lower_val)) round(ci_lower_val, 4) else NA,
                        hazard_ci_upper = if (!is.na(ci_upper_val)) round(ci_upper_val, 4) else NA,
                        p_value = p_val
                    ))
                }

                ci_note <- if (has_se) {
                    "Excess hazard = -log(S_net(t) / S_net(t-1)). CIs via delta method on log-survival SE; p-value tests H0: excess hazard = 0."
                } else {
                    "Excess hazard = -log(S_net(t) / S_net(t-1)). Standard errors not available for this method; CIs and p-values require a fitted regression model."
                }
                table$setNote("info", ci_note)

                # Store for excess plot
                excess_df <- data.frame(
                    interval = yearly,
                    excess_hazard = excess_vals,
                    stringsAsFactors = FALSE
                )
                private$.excess_plot_data <- excess_df
                self$results$excessPlot$setState(as.data.frame(excess_df))

            }, error = function(e) {
                table$setNote("error",
                    paste("Excess mortality calculation failed:", htmltools::htmlEscape(e$message)))
            })
        },

        # ── Crude Probabilities ───────────────────────────────────────────
        .calculateCrudeProbabilities = function(data, rate_table) {

            table <- self$results$crudeProbTable

            tryCatch({
                crud <- relsurv::cmp.rel(
                    survival::Surv(time_days, status_num) ~ 1,
                    rmap = list(
                        age = age_days,
                        sex = sex_relsurv,
                        year = diagdate
                    ),
                    ratetable = rate_table,
                    data = data,
                    conf.int = self$options$confidence_level
                )

                timepoints <- private$.parseTimepoints(self$options$timepoints)
                if (length(timepoints) == 0) timepoints <- c(1, 3, 5, 10)

                crud_times <- crud$time / 365.25
                max_time <- max(crud_times, na.rm = TRUE)
                timepoints <- timepoints[timepoints <= max_time * 1.1]
                if (length(timepoints) == 0) timepoints <- c(max_time)

                table$deleteRows()

                for (i in seq_along(timepoints)) {
                    tp <- timepoints[i]
                    tp_days <- tp * 365.25
                    idx <- private$.stepIdx(crud$time, tp_days)

                    disease_death <- NA
                    other_death <- NA
                    disease_ci_lower <- NA
                    disease_ci_upper <- NA

                    if (!is.na(idx) && !is.null(crud$est) && nrow(crud$est) >= idx) {
                        disease_death <- crud$est[idx, 1]
                        other_death <- crud$est[idx, 2]
                    }

                    if (!is.na(idx) && !is.null(crud$var) && nrow(crud$var) >= idx) {
                        se_disease <- sqrt(crud$var[idx, 1])
                        z <- qnorm(1 - (1 - self$options$confidence_level) / 2)
                        disease_ci_lower <- max(0, disease_death - z * se_disease)
                        disease_ci_upper <- min(1, disease_death + z * se_disease)
                    }

                    table$addRow(rowKey = i, values = list(
                        time_point = tp,
                        disease_death = if (!is.na(disease_death)) round(disease_death, 4) else NA,
                        other_death = if (!is.na(other_death)) round(other_death, 4) else NA,
                        disease_ci_lower = if (!is.na(disease_ci_lower)) round(disease_ci_lower, 4) else NA,
                        disease_ci_upper = if (!is.na(disease_ci_upper)) round(disease_ci_upper, 4) else NA
                    ))
                }

                table$setNote("info",
                    "Crude probabilities decompose total mortality into disease-specific and other-cause components."
                )

            }, error = function(e) {
                table$setNote("error",
                    paste("Crude probability calculation failed:", htmltools::htmlEscape(e$message)))
            })
        },

        # ── Age Standardization ───────────────────────────────────────────
        .calculateAgeStandardized = function(data, rate_table) {

            table <- self$results$ageStandardizedTable

            tryCatch({
                # ICSS weights
                icss_breaks <- c(0, 45, 55, 65, 75, Inf)
                icss_weights <- c(0.07, 0.12, 0.23, 0.29, 0.29)

                data$age_group <- cut(data$age_years,
                                      breaks = icss_breaks,
                                      right = FALSE,
                                      labels = c("0-44", "45-54", "55-64", "65-74", "75+"))

                present_groups <- levels(data$age_group)[levels(data$age_group) %in%
                                                          unique(as.character(data$age_group))]

                if (length(present_groups) < 2) {
                    table$setNote("info",
                        "Age standardization requires at least 2 age groups. Only one group present.")
                    return()
                }

                timepoints <- private$.parseTimepoints(self$options$timepoints)
                if (length(timepoints) == 0) timepoints <- c(1, 3, 5)

                max_time <- max(data$time_years, na.rm = TRUE)
                timepoints <- timepoints[timepoints <= max_time * 1.1]
                if (length(timepoints) == 0) timepoints <- c(1)

                table$deleteRows()

                for (i in seq_along(timepoints)) {
                    tp <- timepoints[i]
                    tp_days <- tp * 365.25

                    # Crude (overall) estimate
                    overall_surv <- tryCatch({
                        rs_overall <- relsurv::rs.surv(
                            survival::Surv(time_days, status_num) ~ 1,
                            rmap = list(age = age_days, sex = sex_relsurv, year = diagdate),
                            ratetable = rate_table, data = data,
                            method = "pohar-perme"
                        )
                        idx <- private$.stepIdx(rs_overall$time, tp_days)
                        if (!is.na(idx)) rs_overall$surv[idx] else NA
                    }, error = function(e) NA)

                    weighted_surv <- 0
                    total_weight <- 0
                    weighted_var <- 0

                    for (j in seq_along(levels(data$age_group))) {
                        grp <- levels(data$age_group)[j]
                        grp_data <- data[data$age_group == grp, ]

                        if (nrow(grp_data) < 5) next

                        w <- icss_weights[j]

                        grp_surv <- tryCatch({
                            rs_grp <- relsurv::rs.surv(
                                survival::Surv(time_days, status_num) ~ 1,
                                rmap = list(age = age_days, sex = sex_relsurv, year = diagdate),
                                ratetable = rate_table, data = grp_data,
                                method = "pohar-perme"
                            )
                            idx <- private$.stepIdx(rs_grp$time, tp_days)
                            if (is.na(idx)) return(NULL)
                            list(
                                surv = rs_grp$surv[idx],
                                se = if (!is.null(rs_grp$std.err)) rs_grp$std.err[idx] else 0
                            )
                        }, error = function(e) NULL)

                        if (!is.null(grp_surv) && !is.na(grp_surv$surv)) {
                            weighted_surv <- weighted_surv + w * grp_surv$surv
                            weighted_var <- weighted_var + (w * grp_surv$se)^2
                            total_weight <- total_weight + w
                        }
                    }

                    if (total_weight > 0) {
                        age_adj <- weighted_surv / total_weight
                        age_adj_se <- sqrt(weighted_var) / total_weight
                        z <- qnorm(1 - (1 - self$options$confidence_level) / 2)

                        table$addRow(rowKey = i, values = list(
                            time_point = tp,
                            crude_rate = if (!is.na(overall_surv)) round(overall_surv, 4) else NA,
                            age_adjusted = round(age_adj, 4),
                            adj_ci_lower = round(max(0, age_adj - z * age_adj_se), 4),
                            adj_ci_upper = round(min(1, age_adj + z * age_adj_se), 4)
                        ))
                    }
                }

                table$setNote("info",
                    "Age standardization uses ICSS weights: 0-44 (0.07), 45-54 (0.12), 55-64 (0.23), 65-74 (0.29), 75+ (0.29)."
                )

            }, error = function(e) {
                table$setNote("error",
                    paste("Age standardization failed:", htmltools::htmlEscape(e$message)))
            })
        },

        # ── Period Analysis ──────────────────────────────────────────────
        .calculatePeriodAnalysis = function(data, rate_table) {

            table <- self$results$periodAnalysisTable

            tryCatch({
                cohort_str <- self$options$cohort_year
                year_num <- as.numeric(format(data$diagdate, "%Y"))

                # Filter by cohort year range if specified
                if (!is.null(cohort_str) && nchar(trimws(cohort_str)) > 0) {
                    parts <- as.numeric(strsplit(trimws(cohort_str), "[-,;]")[[1]])
                    parts <- parts[!is.na(parts)]
                    if (length(parts) >= 2) {
                        yr_min <- min(parts)
                        yr_max <- max(parts)
                        data <- data[year_num >= yr_min & year_num <= yr_max, ]
                        year_num <- as.numeric(format(data$diagdate, "%Y"))
                    }
                }

                if (nrow(data) < 30) {
                    table$setNote("info",
                        "Insufficient data for period analysis after cohort filtering.")
                    return()
                }

                # Create 5-year diagnosis periods
                yr_range <- range(year_num, na.rm = TRUE)
                period_breaks <- seq(floor(yr_range[1] / 5) * 5,
                                     ceiling(yr_range[2] / 5) * 5 + 5, by = 5)
                data$period_group <- cut(year_num, breaks = period_breaks,
                                         right = FALSE, dig.lab = 4)

                periods <- levels(data$period_group)
                periods <- periods[periods %in% unique(as.character(data$period_group))]

                if (length(periods) < 1) {
                    table$setNote("info", "No valid diagnosis periods found.")
                    return()
                }

                table$deleteRows()

                for (i in seq_along(periods)) {
                    prd <- periods[i]
                    prd_data <- data[data$period_group == prd, ]
                    if (nrow(prd_data) < 10) next

                    rs_5y <- tryCatch({
                        rs_fit <- relsurv::rs.surv(
                            survival::Surv(time_days, status_num) ~ 1,
                            rmap = list(age = age_days, sex = sex_relsurv, year = diagdate),
                            ratetable = rate_table,
                            data = prd_data,
                            method = "pohar-perme"
                        )
                        tp_5y <- 5 * 365.25
                        idx <- private$.stepIdx(rs_fit$time, tp_5y)
                        if (is.na(idx)) return(NULL)
                        list(
                            surv = rs_fit$surv[idx],
                            lower = if (!is.null(rs_fit$lower)) rs_fit$lower[idx] else NA,
                            upper = if (!is.null(rs_fit$upper)) rs_fit$upper[idx] else NA
                        )
                    }, error = function(e) NULL)

                    if (!is.null(rs_5y)) {
                        table$addRow(rowKey = i, values = list(
                            period = prd,
                            n_patients = nrow(prd_data),
                            rel_survival_5y = if (!is.na(rs_5y$surv)) round(rs_5y$surv, 4) else NA,
                            rs_ci_lower = if (!is.na(rs_5y$lower)) round(rs_5y$lower, 4) else NA,
                            rs_ci_upper = if (!is.na(rs_5y$upper)) round(rs_5y$upper, 4) else NA
                        ))
                    }
                }

                table$setNote("info",
                    "Period analysis estimates 5-year net survival by diagnosis period using Pohar-Perme method.")

            }, error = function(e) {
                table$setNote("error",
                    paste("Period analysis failed:", htmltools::htmlEscape(e$message)))
            })
        },

        # ── Regression Model ──────────────────────────────────────────────
        .fitRegressionModel = function(data, rate_table) {

            reg_table <- self$results$regressionTable
            fit_table <- self$results$modelFit
            model_type <- self$options$regression_model
            covariates <- self$options$covariates

            if (length(covariates) == 0) {
                reg_table$setNote("info",
                    "Regression models require at least one covariate. Please add covariates.")
                return()
            }

            tryCatch({
                cov_terms <- paste(jmvcore::composeTerms(as.list(covariates)), collapse = " + ")
                formula_str <- paste0("survival::Surv(time_days, status_num) ~ ", cov_terms)
                # TODO (forward-looking): once jmvcore is upgraded to 2.7.27+, switch this to
                # jmvcore::asFormula(formula_str) for allowlist-validated parsing (Defense-2).
                # RCE is already closed by composeTerms backtick-quoting above (Defense-1); note
                # the namespaced survival::Surv(...) LHS would need handling in any asFormula move.
                fml <- as.formula(formula_str)

                reg_model <- NULL

                if (model_type == "additive") {
                    reg_model <- relsurv::rsadd(
                        fml,
                        rmap = list(age = age_days, sex = sex_relsurv, year = diagdate),
                        ratetable = rate_table,
                        data = data,
                        int.length = 365.25
                    )
                } else if (model_type == "multiplicative") {
                    # int = number of yearly follow-up intervals
                    n_int <- max(1, floor(max(data$time_years, na.rm = TRUE)))
                    reg_model <- relsurv::rsmul(
                        fml,
                        rmap = list(age = age_days, sex = sex_relsurv, year = diagdate),
                        ratetable = rate_table,
                        data = data,
                        int = n_int
                    )
                } else if (model_type == "flexible") {
                    if (!requireNamespace('rstpm2', quietly = TRUE)) {
                        jmvcore::reject(
                            "The {rstpm2} package is required for flexible parametric models. Install: install.packages('rstpm2')"
                        )
                    }
                    # Compute per-individual background hazard for relative survival
                    bhaz <- private$.computeExpectedHazard(data, rate_table)
                    if (!is.null(bhaz) && length(bhaz) == nrow(data)) {
                        data$bhazard <- pmax(as.numeric(bhaz), 1e-10)
                        reg_model <- rstpm2::stpm2(
                            fml,
                            data = data,
                            df = self$options$spline_df,
                            bhazard = data$bhazard
                        )
                    } else {
                        private$.addNotice("warning", "Background Hazard",
                            "Could not compute population expected hazard; flexible model fitted without background hazard (standard, not relative survival).")
                        reg_model <- rstpm2::stpm2(
                            fml,
                            data = data,
                            df = self$options$spline_df
                        )
                    }
                }

                if (!is.null(reg_model)) {
                    private$.displayRegressionResults(reg_model, model_type)
                }

            }, error = function(e) {
                reg_table$setNote("error",
                    paste("Regression model fitting failed:", htmltools::htmlEscape(e$message)))
            })
        },

        # ── Display Regression Results ────────────────────────────────────
        .displayRegressionResults = function(reg_model, model_type) {

            reg_table <- self$results$regressionTable
            fit_table <- self$results$modelFit

            tryCatch({
                coefs <- tryCatch(summary(reg_model)$coefficients, error = function(e) NULL)
                if (is.null(coefs)) {
                    coefs <- tryCatch(coef(summary(reg_model)), error = function(e) NULL)
                }

                if (is.null(coefs) || nrow(coefs) == 0) {
                    reg_table$setNote("info", "No coefficients extracted from model.")
                    return()
                }

                reg_table$deleteRows()

                for (i in 1:nrow(coefs)) {
                    var_name <- rownames(coefs)[i]
                    est <- coefs[i, 1]
                    se <- if (ncol(coefs) >= 2) coefs[i, 2] else NA
                    z_val <- if (ncol(coefs) >= 3) coefs[i, 3] else NA
                    p_val <- if (ncol(coefs) >= 4) coefs[i, 4] else NA

                    z <- qnorm(1 - (1 - self$options$confidence_level) / 2)
                    ci_lower <- if (!is.na(se)) est - z * se else NA
                    ci_upper <- if (!is.na(se)) est + z * se else NA

                    reg_table$addRow(rowKey = i, values = list(
                        variable = var_name,
                        coefficient = round(est, 4),
                        std_error = if (!is.na(se)) round(se, 4) else NA,
                        z_value = if (!is.na(z_val)) round(z_val, 3) else NA,
                        p_value = p_val,
                        coef_ci_lower = if (!is.na(ci_lower)) round(ci_lower, 4) else NA,
                        coef_ci_upper = if (!is.na(ci_upper)) round(ci_upper, 4) else NA
                    ))
                }

                model_names <- c(
                    "additive" = "Additive Excess Hazard",
                    "multiplicative" = "Multiplicative",
                    "flexible" = "Flexible Parametric"
                )
                reg_table$setNote("model",
                    paste0("Model type: ", model_names[model_type]))

                # Model fit
                fit_table$deleteRows()
                ll <- tryCatch(logLik(reg_model), error = function(e) NA)
                aic_val <- tryCatch(AIC(reg_model), error = function(e) NA)

                row_idx <- 0
                if (!is.na(ll)) {
                    row_idx <- row_idx + 1
                    fit_table$addRow(rowKey = row_idx, values = list(
                        metric = "Log-Likelihood",
                        value = round(as.numeric(ll), 2),
                        interpretation = ""
                    ))
                }
                if (!is.na(aic_val)) {
                    row_idx <- row_idx + 1
                    fit_table$addRow(rowKey = row_idx, values = list(
                        metric = "AIC",
                        value = round(aic_val, 2),
                        interpretation = "Lower is better"
                    ))
                }

            }, error = function(e) {
                reg_table$setNote("error",
                    paste("Could not display regression results:", htmltools::htmlEscape(e$message)))
            })
        },

        # ── Generate Plot Data ────────────────────────────────────────────
        .generatePlotData = function(rel_surv, km_fit, exp_fit) {

            # Build a clean data.frame with all survival curves
            times_years <- rel_surv$time / 365.25
            n <- length(times_years)

            net_surv <- rel_surv$surv
            ci_lower <- if (!is.null(rel_surv$lower)) rel_surv$lower else rep(NA_real_, n)
            ci_upper <- if (!is.null(rel_surv$upper)) rel_surv$upper else rep(NA_real_, n)

            # Observed (KM)
            obs_surv <- rep(NA_real_, n)
            if (!is.null(km_fit)) {
                km_times <- km_fit$time / 365.25
                km_surv_vals <- km_fit$surv
                for (k in seq_len(n)) {
                    matches <- which(km_times <= times_years[k])
                    if (length(matches) > 0) {
                        obs_surv[k] <- km_surv_vals[max(matches)]
                    }
                }
            }

            # Expected (population)
            exp_surv <- rep(NA_real_, n)
            if (!is.null(exp_fit)) {
                exp_times <- exp_fit$time / 365.25
                exp_surv_vals <- exp_fit$surv
                for (k in seq_len(n)) {
                    matches <- which(exp_times <= times_years[k])
                    if (length(matches) > 0) {
                        exp_surv[k] <- exp_surv_vals[max(matches)]
                    }
                }
            }

            plot_data <- data.frame(
                time = times_years,
                observed = obs_surv,
                expected = exp_surv,
                net_survival = net_surv,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                stringsAsFactors = FALSE
            )
            plot_data <- as.data.frame(plot_data)

            private$.plot_data <- plot_data

            # Set state for each plot
            self$results$observedPlot$setState(plot_data)
            self$results$expectedPlot$setState(plot_data)
            self$results$relativePlot$setState(plot_data)
            # excessPlot state set in .calculateExcessMortality
        },

        # ── Plot: Observed Survival ───────────────────────────────────────
        .plotObserved = function(image, ggtheme, theme, ...) {

            state <- image$state
            if (is.null(state)) return(FALSE)

            plot_data <- state[!is.na(state$observed), ]
            if (nrow(plot_data) == 0) return(FALSE)

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = observed)) +
                ggplot2::geom_step(color = "#2166AC", linewidth = 1) +
                ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                                    color = "grey50", alpha = 0.6) +
                ggplot2::scale_y_continuous(
                    limits = c(0, 1),
                    labels = scales::percent_format()
                ) +
                ggplot2::labs(
                    x = "Time (years)",
                    y = "Observed Survival",
                    title = "Observed (Overall) Survival"
                ) +
                ggtheme

            print(p)
            return(TRUE)
        },

        # ── Plot: Expected Survival ───────────────────────────────────────
        .plotExpected = function(image, ggtheme, theme, ...) {

            state <- image$state
            if (is.null(state)) return(FALSE)

            plot_data <- state[!is.na(state$expected), ]
            if (nrow(plot_data) == 0) return(FALSE)

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = expected)) +
                ggplot2::geom_step(color = "#4DAF4A", linewidth = 1) +
                ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                                    color = "grey50", alpha = 0.6) +
                ggplot2::scale_y_continuous(
                    limits = c(0, 1),
                    labels = scales::percent_format()
                ) +
                ggplot2::labs(
                    x = "Time (years)",
                    y = "Expected Survival",
                    title = "Expected (Population) Survival"
                ) +
                ggtheme

            print(p)
            return(TRUE)
        },

        # ── Plot: Relative/Net Survival ───────────────────────────────────
        .plotRelative = function(image, ggtheme, theme, ...) {

            state <- image$state
            if (is.null(state)) return(FALSE)

            plot_data <- state[!is.na(state$net_survival), ]
            if (nrow(plot_data) == 0) return(FALSE)

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = net_survival)) +
                ggplot2::geom_step(color = "#B2182B", linewidth = 1)

            if (any(!is.na(plot_data$ci_lower)) && any(!is.na(plot_data$ci_upper))) {
                p <- p + ggplot2::geom_ribbon(
                    ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                    fill = "#B2182B", alpha = 0.15
                )
            }

            y_max <- max(1.05, max(plot_data$net_survival, na.rm = TRUE) * 1.05)

            p <- p +
                ggplot2::geom_hline(yintercept = 1.0, linetype = "dashed",
                                    color = "grey50", alpha = 0.6) +
                ggplot2::geom_hline(yintercept = 0.5, linetype = "dotted",
                                    color = "grey70", alpha = 0.6) +
                ggplot2::scale_y_continuous(
                    limits = c(0, y_max),
                    labels = scales::percent_format()
                ) +
                ggplot2::labs(
                    x = "Time (years)",
                    y = "Net/Relative Survival",
                    title = paste0("Relative Survival (", private$.methodLabel(), ")")
                ) +
                ggtheme

            print(p)
            return(TRUE)
        },

        # ── Plot: Excess Mortality ────────────────────────────────────────
        .plotExcess = function(image, ggtheme, theme, ...) {

            state <- image$state
            if (is.null(state)) return(FALSE)

            plot_data <- state[!is.na(state$excess_hazard), ]
            if (nrow(plot_data) == 0) return(FALSE)

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = interval, y = excess_hazard)) +
                ggplot2::geom_col(fill = "#D6604D", alpha = 0.8, width = 0.7) +
                ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +
                ggplot2::scale_x_continuous(
                    breaks = plot_data$interval,
                    labels = paste0(plot_data$interval - 1, "-", plot_data$interval)
                ) +
                ggplot2::labs(
                    x = "Time Interval (years)",
                    y = "Excess Hazard",
                    title = "Excess Mortality by Year"
                ) +
                ggtheme

            print(p)
            return(TRUE)
        },

        # ── Display Summary ───────────────────────────────────────────────
        .displaySummary = function(rel_surv, data) {

            n_patients <- nrow(data)
            n_events <- sum(data$status_num, na.rm = TRUE)
            median_fu <- round(median(data$time_years, na.rm = TRUE), 1)
            method_label <- private$.methodLabel()

            ratetable_labels <- c(
                "us" = "US Population (survexp.us)",
                "mn" = "Minnesota Population (survexp.mn)",
                "fr" = "French Population (survexp.fr)",
                "slovenia" = "Slovenian Population (slopop)",
                "turkey" = "Turkey Population (WHO)",
                "germany" = "German Population (WHO)",
                "uk" = "UK Population (WHO)",
                "italy" = "Italian Population (WHO)",
                "japan" = "Japanese Population (WHO)",
                "spain" = "Spanish Population (WHO)",
                "brazil" = "Brazilian Population (WHO)",
                "south_korea" = "South Korean Population (WHO)",
                "china" = "Chinese Population (WHO)",
                "india" = "Indian Population (WHO)",
                "custom" = "Custom Rate Table"
            )
            rt_label <- ratetable_labels[self$options$ratetable]
            if (is.na(rt_label)) rt_label <- self$options$ratetable

            summary_html <- paste0(
                "<h3>Relative Survival Analysis Results</h3>",
                "<p><b>Method:</b> ", method_label, "</p>",
                "<p><b>Number of Patients:</b> ", n_patients, "</p>",
                "<p><b>Number of Deaths:</b> ", n_events, "</p>",
                "<p><b>Median Follow-up:</b> ", median_fu, " years</p>",
                "<p><b>Population Rate Table:</b> ", rt_label, "</p>",
                "<p><b>Confidence Level:</b> ", self$options$confidence_level * 100, "%</p>"
            )

            self$results$summary$setContent(summary_html)
        },

        # ── Display Interpretation ────────────────────────────────────────
        .displayInterpretation = function(rel_surv) {

            times_years <- rel_surv$time / 365.25
            five_yr_surv <- NA
            idx_5 <- private$.stepIdx(rel_surv$time, 5 * 365.25)
            if (!is.na(idx_5) && times_years[idx_5] >= 4) {
                five_yr_surv <- rel_surv$surv[idx_5]
            }

            interp_html <- "<h4>Clinical Interpretation</h4>"

            if (!is.na(five_yr_surv)) {
                pct <- round(five_yr_surv * 100, 1)
                interp_html <- paste0(interp_html,
                    "<p>The estimated 5-year net survival is <b>", pct, "%</b>. ",
                    "This means that, after removing the effect of other-cause mortality, ",
                    "approximately ", pct, "% of patients would survive 5 years ",
                    "if the disease under study were the only possible cause of death.</p>"
                )

                if (five_yr_surv > 0.90) {
                    interp_html <- paste0(interp_html,
                        "<p><b>Prognosis:</b> Excellent. The disease contributes minimally to overall mortality.</p>")
                } else if (five_yr_surv > 0.70) {
                    interp_html <- paste0(interp_html,
                        "<p><b>Prognosis:</b> Good. Moderate excess mortality attributable to the disease.</p>")
                } else if (five_yr_surv > 0.50) {
                    interp_html <- paste0(interp_html,
                        "<p><b>Prognosis:</b> Fair. Substantial excess mortality is present.</p>")
                } else {
                    interp_html <- paste0(interp_html,
                        "<p><b>Prognosis:</b> Poor. The disease accounts for a large proportion of mortality.</p>")
                }
            }

            interp_html <- paste0(interp_html,
                "<h4>Key Concepts:</h4>",
                "<ul>",
                "<li><b>Relative/Net Survival:</b> Ratio of observed to expected survival. ",
                "Values near 1.0 indicate that disease contributes little to mortality. ",
                "Values well below 1.0 indicate substantial disease-specific mortality.</li>",
                "<li><b>Excess Mortality:</b> The additional mortality attributable to the disease ",
                "beyond what would be expected in the general population.</li>",
                "<li><b>Crude Probability:</b> Decomposes total mortality into disease-related and ",
                "other-cause components, accounting for competing risks.</li>",
                "</ul>",
                "<p><i>Note: Relative survival is particularly useful when cause of death information ",
                "is unreliable or unavailable, as it does not require individual cause-of-death data.</i></p>"
            )

            self$results$interpretation$setContent(interp_html)
        },

        # ── Step-function lookup ─────────────────────────────────────
        .stepLookup = function(times, values, target) {
            # Left-continuous step-function lookup: last time <= target
            candidates <- which(times <= target)
            if (length(candidates) == 0) return(NA)
            values[max(candidates)]
        },

        .stepIdx = function(times, target) {
            # Return index of last time <= target (for multi-field lookups)
            candidates <- which(times <= target)
            if (length(candidates) == 0) return(NA_integer_)
            max(candidates)
        },

        # ── Expected Hazard from Rate Table ────────────────────────────
        .computeExpectedHazard = function(data, rate_table) {
            # Compute per-individual background hazard from population rate table
            # Used for flexible parametric relative survival models (stpm2 bhazard)
            tryCatch({
                # Method 1: individual cumulative hazard at 1 day
                exp_haz <- survival::survexp(
                    ~ 1,
                    rmap = list(
                        age = age_days,
                        sex = sex_relsurv,
                        year = diagdate
                    ),
                    ratetable = rate_table,
                    data = data,
                    times = 1,
                    method = "individual.h"
                )
                return(as.numeric(exp_haz))
            }, error = function(e) {
                # Fallback: compute from individual survival at 1 day
                tryCatch({
                    exp_surv <- survival::survexp(
                        ~ 1,
                        rmap = list(
                            age = age_days,
                            sex = sex_relsurv,
                            year = diagdate
                        ),
                        ratetable = rate_table,
                        data = data,
                        times = 1,
                        method = "individual.s"
                    )
                    return(-log(pmax(as.numeric(exp_surv), 1e-15)))
                }, error = function(e2) {
                    return(NULL)
                })
            })
        },

        # ── Notices helpers ──────────────────────────────────────────────
        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content
            )
        },

        .renderNotices = function() {
            notices <- private$.noticeList
            if (length(notices) == 0) {
                self$results$notices$setContent("")
                return()
            }
            type_styles <- list(
                error = "color:#d32f2f;border-left:4px solid #d32f2f;",
                strong_warning = "color:#e65100;border-left:4px solid #e65100;",
                warning = "color:#f57f17;border-left:4px solid #f57f17;",
                info = "color:#1565c0;border-left:4px solid #1565c0;"
            )
            type_icons <- list(
                error = "&#9888;",
                strong_warning = "&#9888;",
                warning = "&#9888;",
                info = "&#8505;"
            )
            order <- c("error", "strong_warning", "warning", "info")
            sorted <- notices[order(match(
                sapply(notices, function(n) n$type), order
            ))]
            parts <- vapply(sorted, function(n) {
                style <- type_styles[[n$type]]
                icon <- type_icons[[n$type]]
                paste0(
                    "<div style='", style,
                    "padding:4px 8px;margin:4px 0;background:#fafafa;'>",
                    # TODO (security): n$title / n$content are interpolated into notices HTML
                    # unescaped. No live XSS today — every .addNotice() call passes developer
                    # literals or numeric scalars — but if a future .addNotice ever passes a
                    # column name, factor label, or e$message, wrap title/content in
                    # htmltools::htmlEscape() here (defense-in-depth at the render boundary).
                    "<b>", icon, " ", n$title, ":</b> ", n$content,
                    "</div>"
                )
            }, character(1))
            self$results$notices$setContent(paste(parts, collapse = "\n"))
        },

        # ── Helper: Parse Timepoints ──────────────────────────────────────
        .parseTimepoints = function(tp_str) {
            if (is.null(tp_str) || tp_str == "") return(numeric(0))
            parts <- strsplit(tp_str, "[,;\\s]+")[[1]]
            parts <- parts[parts != ""]
            nums <- suppressWarnings(as.numeric(parts))
            nums <- nums[!is.na(nums) & nums > 0]
            sort(unique(nums))
        },

        # ── Helper: Method Label ──────────────────────────────────────────
        .methodLabel = function() {
            labels <- c(
                "poharperme" = "Pohar-Perme (Net Survival Estimator)",
                "ederer2"    = "Ederer II",
                "ederer1"    = "Ederer I",
                "hakulinen"  = "Hakulinen"
            )
            labels[self$options$method]
        }
    )
)
