#' @title Age Pyramid
#' @description Generates an age pyramid plot from the provided data.
#' The function allows customization of bin width (age group granularity) and plot title.
#' It creates a visually appealing plot showing the distribution of age by gender.
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom tidyr complete pivot_wider
#'
#' @seealso [agepyramid()] for the user-facing analysis function and its arguments.
#'
#' @return An \code{R6} class generator object for the \code{agepyramidClass} backend; used internally by the jamovi analysis wrapper and not called directly.

agepyramidClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "agepyramidClass",
    inherit = agepyramidBase,
    private = list(

        # Notice collection helpers. A single Preformatted (plain-text) output item:
        # avoids BOTH the jmvcore::Notice serialization error from
        # self$results$insert(999, Notice) AND any HTML in notices (project convention:
        # notice content must be plain text). ====
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            duplicate <- vapply(private$.noticeList, function(notice) {
                identical(notice$type, type) &&
                    identical(notice$title, title) &&
                    identical(notice$content, content)
            }, logical(1))
            if (any(duplicate))
                return()

            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type,
                title = title,
                content = content
            )
            # Render immediately so early-return validation aborts still display the notice
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }

            # Plain text only notices avoid HTML by project convention; the Preformatted
            # output item renders this literally (no markup, no injection surface).
            blocks <- vapply(private$.noticeList, function(notice) {
                prefix <- switch(notice$type,
                    ERROR          = paste0(.("ERROR"), ": "),
                    STRONG_WARNING = paste0(.("WARNING"), ": "),
                    WARNING        = paste0(.("WARNING"), ": "),
                    INFO           = paste0(.("NOTE"), ": "),
                    "")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))

            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },
        # A rejected run shows jamovi's error banner; any INFO/WARNING raised
        # before the abort would otherwise stay in the notices panel beside it
        # (e.g. "levels were read from the level names" next to "no valid rows").
        .rejectClean = function(message) {
            private$.noticeList <- list()
            private$.renderNotices()
            jmvcore::reject(message)
        },

        .run = function() {
            private$.noticeList <- list()
            # Clear the panel up front: .renderNotices() is otherwise only reached
            # from .addNotice(), so a run that raises nothing would leave the
            # previous run's notices on screen.
            private$.renderNotices()

            # Every user-visible string is wrapped in .() with {} placeholders
            # filled by jmvcore::format(); HTML structure stays outside the
            # wrappers, and translated text that lands inside HTML is escaped.
            # Plot callbacks do not expose a safe cancellation point, but the
            # data aggregation below can be interrupted between phases.
            # Check if required options (age and gender) are provided
            if (is.null(self$options$age) || is.null(self$options$gender)) {
                esc <- htmltools::htmlEscape
                li <- function(head, body = NULL) {
                    paste0("<li><strong>", esc(head), "</strong>",
                           if (is.null(body)) "" else paste0(" ", esc(body)), "</li>")
                }
                self$results$welcome$setContent(paste0(
                    "<div style='background-color: rgba(33, 152, 239, 0.13); padding: 20px; border-radius: 8px; border-left: 4px solid #2196F3; color: inherit;'>",
                    "<h3 style='color: #4a9eea; margin-top: 0;'>", esc(.("Age Pyramid Analysis")), "</h3>",
                    "<p style='font-size: 15px;'>", esc(.("Create demographic visualizations showing age distribution by gender.")), "</p>",
                    "<h4 style='color: #4a9eea; margin-bottom: 8px;'>", esc(.("Required Variables:")), "</h4>",
                    "<ol style='font-size: 14px; line-height: 1.6;'>",
                    li(.("Age:"), .("Continuous numeric variable (e.g., patient age in years)")),
                    li(.("Gender:"), .("Categorical variable (typically binary: Male/Female)")),
                    "</ol>",
                    "<h4 style='color: #4a9eea; margin-bottom: 8px;'>", esc(.("Features:")), "</h4>",
                    "<ul style='font-size: 14px; line-height: 1.6;'>",
                    li(.("Age group presets:"), .("WHO/UN standard five-year groups (0-4, 5-9, ... 85+), WHO abridged with infants separated (<1, 1-4, 5-9, ...), Pediatric (<18), Reproductive (15-50), Geriatric (65+), Life Course, or Custom")),
                    li(.("Custom age breaks:"), .("Define your own age boundaries (e.g., 0,18,25,50,65,100)")),
                    li(.("Customizable bin width"), .("for automatic age grouping")),
                    li(.("Percentages"), .("within each gender or of all observations, in the table and on the bars")),
                    li(.("Color palettes:"), .("Standard, Colorblind-friendly, Grayscale, or Custom colors")),
                    li(.("Readable age group labels"), .("using the demographic convention of left-closed bands (e.g., 0-4, 5-9, 85+), so an age on a boundary falls in the band that starts there")),
                    li(.("Table with counts and percentages")),
                    li(.("Gender level selection"), .("for flexible data structures")),
                    "</ul>",
                    "<p style='font-size: 13px; opacity: 0.75; margin-bottom: 0; font-style: italic;'>",
                    esc(.("Select your Age and Gender variables to begin.")),
                    "</p></div>"
                ))
                # `visible: (!age || !gender)` in the .r.yaml never worked - a
                # leading "!" is not recognised as an expression, so the box stayed
                # on screen (empty) once both variables were chosen. Drive it here.
                self$results$welcome$setVisible(TRUE)
                return()
            }
            self$results$welcome$setVisible(FALSE)

            if (nrow(self$data) == 0)
                private$.rejectClean(.("Data contains no (complete) rows"))

            # Read and prepare data ----
            mydata <- self$data

            age <- self$options$age
            gender <- self$options$gender

            # Select and clean the required columns
            mydata <- jmvcore::select(mydata, c(age, gender))
            mydata <- jmvcore::naOmit(mydata)

            # Convert age to numeric and gender to factor. Read BOTH source
            # columns before writing the working "Age"/"Gender" columns: a gender
            # variable literally named "Age" was overwritten by the numeric ages
            # before it was read, so every row was then "unrecognised gender".
            age_values <- jmvcore::toNumeric(mydata[[age]])
            gender_values <- mydata[[gender]]
            if (!is.numeric(age_values)) {
                age_values <- suppressWarnings(as.numeric(as.character(age_values)))
            }
            mydata[["Age"]] <- age_values
            mydata[["Gender"]] <- as.factor(gender_values)

            # Filter out invalid ages (created by conversion)
            n_before_age_filter <- nrow(mydata)
            mydata <- mydata %>% dplyr::filter(!is.na(Age), is.finite(Age), Age >= 0)
            n_invalid_age <- n_before_age_filter - nrow(mydata)
            if (n_invalid_age > 0) {
                # Always a data error (a negative age, or text in an age column),
                # so say so even when the share excluded is too small to trip the
                # 20% exclusion warning further down.
                private$.addNotice(
                    "WARNING",
                    .("Some ages could not be used"),
                    jmvcore::format(
                        .("{n} observation(s) had an age that is negative, infinite or not a number and were left out of every count, percentage and bar. Check the '{column}' column: a negative age is a data-entry error, and text such as 'n/a' should be a missing value. The Data Summary lists them under 'Unusable ages'."),
                        n = n_invalid_age, column = age)
                )
            }

            # Determine gender levels with smart defaults ----
            n_initial <- nrow(self$data)  # Track for data summary
            female_level <- self$options$female
            male_level <- self$options$male
            gender_levels <- levels(mydata[["Gender"]])

            # Apply smart defaults if levels not selected.
            #
            # Assigning by factor level ORDER alone is silently wrong for every
            # coding whose first level is the male one - c("M", "F"),
            # c("Male", "Female"), c("E", "K") - and the inversion runs all the
            # way through: the sides of the pyramid, the legend and the
            # "Female (n)" / "Male (n)" columns are all reversed with no visible
            # sign. So read the level LABELS first, and disclose the outcome as a
            # notice either way (see the assign_mode block below).
            assign_mode <- "user"
            auto_side <- NULL
            auto_level <- NULL
            if (is.null(female_level) && is.null(male_level)) {
                guess <- private$.guess_gender_levels(gender_levels)
                if (!is.null(guess)) {
                    female_level <- guess$female
                    male_level <- guess$male
                    assign_mode <- "label"
                } else if (length(gender_levels) >= 2) {
                    female_level <- gender_levels[1]
                    male_level <- gender_levels[2]
                    assign_mode <- "order"
                } else if (length(gender_levels) == 1) {
                    # Single level: put it on the side its own label names, so a
                    # male-only cohort is not reported in the Female column.
                    if (identical(private$.gender_side(gender_levels[1]), "male")) {
                        male_level <- gender_levels[1]
                    } else {
                        female_level <- gender_levels[1]
                    }
                    assign_mode <- "single"
                }
            } else if (is.null(female_level)) {
                # Only male selected - fill the female side from the remaining levels
                remaining <- gender_levels[gender_levels != male_level]
                female_level <- private$.pick_level(remaining, "female")
                assign_mode <- "partial"
                auto_side <- "female"
                auto_level <- female_level
            } else if (is.null(male_level)) {
                # Only female selected - fill the male side from the remaining levels
                remaining <- gender_levels[gender_levels != female_level]
                male_level <- private$.pick_level(remaining, "male")
                assign_mode <- "partial"
                auto_side <- "male"
                auto_level <- male_level
            }

            # Validate that female and male levels are different. This is a
            # configuration error, so it belongs in the notices panel at the top of
            # the results, not under the "Data Summary" heading.
            if (!is.null(female_level) && !is.null(male_level) && female_level == male_level) {
                private$.addNotice(
                    "ERROR",
                    .("Female and Male levels must be different"),
                    jmvcore::format(
                        .("The level '{level}' is selected for BOTH the female and the male side, so there is nothing to compare and no pyramid was drawn. Choose two different levels of '{gender}', or clear one or both selectors to let the analysis read the levels from their names."),
                        level = female_level, gender = gender)
                )
                self$results$dataInfo$setContent("")
                return()
            }

            # Detect single-gender cohort
            is_single_gender <- is.null(male_level) || is.null(female_level)
            single_gender_label <- if(!is.null(female_level)) female_level else male_level

            # State the assignment where it is read, not only as a table row.
            if (assign_mode == "label") {
                private$.addNotice(
                    "INFO",
                    .("Gender levels were read from the level names"),
                    jmvcore::format(
                        .("You did not set the gender levels, so they were matched on the level names of '{gender}': '{female}' was taken as female and '{male}' as male. The sides of the pyramid, the legend and the 'Female (n)' / 'Male (n)' table columns all follow that assignment. Use the 'Female level' and 'Male level' selectors to set it explicitly."),
                        gender = gender, female = female_level, male = male_level)
                )
            } else if (assign_mode == "order") {
                private$.addNotice(
                    "STRONG_WARNING",
                    .("Gender levels were assigned by level order, not by name"),
                    jmvcore::format(
                        .("The levels of '{gender}' were not recognised as gender labels, so the first level ('{female}') was treated as FEMALE and the second ('{male}') as MALE. If your data are coded the other way round, the whole analysis is reversed with them: the left and right sides of the pyramid, the legend, and the 'Female (n)' and 'Male (n)' table columns. Set the 'Female level' and 'Male level' selectors to state the coding explicitly."),
                        gender = gender, female = female_level, male = male_level)
                )
            } else if (assign_mode == "partial" && !is.null(auto_level)) {
                # Two complete sentences rather than one with the side spliced
                # in, so each can be translated with its own grammar.
                template <- if (identical(auto_side, "female")) {
                    .("Only one of the two gender selectors was set, so the level '{level}' was used as the female group. Check that this matches your coding; if it does not, the female side of the pyramid and the 'Female (n)' column describe the wrong patients. Set both the 'Female level' and 'Male level' selectors to remove the guess.")
                } else {
                    .("Only one of the two gender selectors was set, so the level '{level}' was used as the male group. Check that this matches your coding; if it does not, the male side of the pyramid and the 'Male (n)' column describe the wrong patients. Set both the 'Female level' and 'Male level' selectors to remove the guess.")
                }
                private$.addNotice(
                    "STRONG_WARNING",
                    .("One gender level was filled in automatically"),
                    jmvcore::format(template, level = auto_level)
                )
            }

            # Create standardized gender variable
            is_female <- if (!is.null(female_level)) {
                mydata[["Gender"]] == female_level
            } else {
                FALSE
            }
            is_male <- if (!is.null(male_level)) {
                mydata[["Gender"]] == male_level
            } else {
                FALSE
            }
            mydata <- mydata %>%
                dplyr::mutate(
                    Gender2 = dplyr::case_when(
                        is_female ~ "Female",
                        is_male ~ "Male",
                        TRUE ~ NA_character_  # Other values become NA
                    )
                )

            # Filter unrecognised genders and track exclusion
            n_before_gender_filter <- nrow(mydata)
            mydata <- mydata %>% dplyr::filter(!is.na(Gender2))
            n_missing_gender <- n_before_gender_filter - nrow(mydata)

            n_final <- nrow(mydata)  # Track for data summary

            if (n_final == 0) {
                private$.rejectClean(.("No valid rows remain after filtering age and gender values"))
            }

            # Determine age group breaks based on preset or custom bin width ----
            age_groups <- if (!is.null(self$options$age_groups)) self$options$age_groups else 'custom'
            max_age <- max(mydata[["Age"]], na.rm = TRUE)

            # An implausible age is not caught by the band-count guard further
            # down: a 999 sentinel with the default 5-year bin width produces
            # EXACTLY 200 bands (length(c(seq(0, 999, 5), Inf)) - 1 == 200), which
            # is not MORE than 200. Report it and leave the rows in place - only
            # the user can tell a sentinel from a genuine record.
            n_implausible_age <- sum(mydata[["Age"]] > 120)
            if (n_implausible_age > 0) {
                private$.addNotice(
                    "STRONG_WARNING",
                    .("Some ages are above 120 years"),
                    jmvcore::format(
                        .("{n} observation(s) have an age above 120 years, the highest being {maxAge}. Values that large are usually a missing-value code such as 999, or an age recorded in months rather than years. They were not removed: they are counted in the highest age band of the pyramid. While the bands come from the bin width they are also built out to {maxAge} years, so the figure gains a long empty tail. Check the age column before reading the figure."),
                        n = n_implausible_age, maxAge = round(max_age, 2))
                )
            }

            # Whole-year band labels ("6-10", "85+") describe a right-closed band
            # exactly only when every age is a whole number; ages computed from
            # dates are not. Used by .create_age_labels() below.
            age_whole <- all(abs(mydata[["Age"]] - round(mydata[["Age"]])) < 1e-8)

            # Track whether we are using the bin_width path so that .create_age_labels
            # can produce a finite last-band label (e.g. "15-19") for uniform band widths.
            using_bin_width_breaks <- FALSE

            if (age_groups == 'who') {
                # WHO/UN standard five-year age groups: 0-4, 5-9, ... 80-84, 85+.
                # These are the groups of the WHO World Standard Population
                # (Ahmad OB et al., "Age standardization of rates: a new WHO
                # standard", GPE Discussion Paper 31, WHO 2001) and the UN
                # convention for population pyramids. Left-closed bands make them
                # exact: [0,5) is "0-4", not "1-5".
                breaks_seq <- c(seq(0, 85, by = 5), Inf)
            } else if (age_groups == 'who_infant') {
                # WHO abridged life-table groups, which separate infants from
                # young children: <1, 1-4, 5-9, ... 85+. Used wherever infant
                # mortality is reported separately from early childhood.
                breaks_seq <- c(0, 1, seq(5, 85, by = 5), Inf)
            } else if (age_groups == 'pediatric') {
                # Pediatric: Birth to 18 years with developmental milestones
                breaks_seq <- c(0, 1, 2, 5, 10, 15, 18, Inf)
            } else if (age_groups == 'reproductive') {
                # Reproductive age: 15-50 with 5-year intervals
                breaks_seq <- c(0, 15, 20, 25, 30, 35, 40, 45, 50, Inf)
            } else if (age_groups == 'geriatric') {
                # Geriatric: 65+ with 5-year intervals
                breaks_seq <- c(0, 65, 70, 75, 80, 85, 90, 95, Inf)
            } else if (age_groups == 'lifecourse') {
                # Life course: Key developmental stages
                breaks_seq <- c(0, 5, 15, 25, 45, 65, 75, 85, Inf)
            } else {
                # Custom: custom_breaks wins over bin_width when it is filled in.
                #
                # The entries are parsed one by one instead of being handed to
                # as.numeric() wholesale: that dropped every unreadable entry in
                # silence (so "0,abc,50,x" quietly became two bands) and, when
                # nothing at all parsed, fell through to the bin width with no
                # message about the breaks the user had typed.
                custom_breaks <- self$options$custom_breaks
                breaks_seq <- NULL
                if (!is.null(custom_breaks) && nchar(trimws(custom_breaks)) > 0) {
                    breaks_str <- trimws(strsplit(custom_breaks, ",")[[1]])
                    breaks_str <- breaks_str[nzchar(breaks_str)]
                    # suppressWarnings: an unreadable entry must not reach jamovi's
                    # Analysis Notes panel as an unattached "NAs introduced by
                    # coercion"; it is reported by the notice below instead.
                    breaks_num <- suppressWarnings(as.numeric(breaks_str))
                    bad_entries <- breaks_str[is.na(breaks_num)]
                    breaks_num <- sort(unique(breaks_num[!is.na(breaks_num) & is.finite(breaks_num)]))

                    if (length(bad_entries) > 0) {
                        # Which breaks the bands actually come from is stated by
                        # the two notices below - saying it here as well was
                        # wrong when NOTHING parsed ("the remaining 0 break
                        # point(s)"), because the bands then come from bin width.
                        private$.addNotice(
                            "WARNING",
                            .("Some custom age breaks could not be read"),
                            jmvcore::format(
                                .("{nBad} of the {nAll} entries in 'Custom age breaks' are not numbers and were left out: {entries}. Enter the break points as plain numbers separated by commas, for example 0,18,45,65."),
                                nBad = length(bad_entries), nAll = length(breaks_str),
                                entries = paste(bad_entries, collapse = ", "))
                        )
                    }

                    if (length(breaks_num) == 0) {
                        private$.addNotice(
                            "WARNING",
                            .("The custom age breaks could not be used"),
                            jmvcore::format(
                                .("No entry in 'Custom age breaks' could be read as a number, so the age bands below come from the bin width ({width} years) and not from what you typed. Clear the box to remove this message, or enter the break points as plain numbers separated by commas, for example 0,18,45,65."),
                                width = self$options$bin_width)
                        )
                    } else {
                        # Open-ended top band, so the oldest patients are never
                        # dropped when the highest break sits below the oldest age.
                        breaks_seq <- c(breaks_num, Inf)
                        private$.addNotice(
                            "INFO",
                            .("Custom age breaks are in use"),
                            jmvcore::format(
                                .("The age bands come from the break points you typed ({breaks}, then open-ended). 'Bin width' is ignored while 'Custom age breaks' is filled in; clear that box to go back to bands of a fixed width. Ages below the lowest break are not shown in the pyramid and are counted under 'Outside age-break range' in the Data Summary."),
                                breaks = paste(base::format(breaks_num, trim = TRUE, scientific = FALSE),
                                               collapse = ", "))
                        )
                    }
                }
                if (is.null(breaks_seq)) {
                    breaks_seq <- private$.bin_width_breaks(max_age)
                    using_bin_width_breaks <- TRUE
                }
            }

            # Safeguard: ensure we have at least two unique breaks for cut()
            if (length(unique(breaks_seq)) < 2) {
                # Fallback to a single bin [0, max_age+1] if data is restricted
                breaks_seq <- c(0, max_age + 1)
            }

            # Which end of each band is closed is the user's choice, but only for
            # the custom paths. Every named preset is defined by its source as a
            # set of LEFT-closed bands - the WHO/UN five-year groups are 0-4, 5-9,
            # ... 85+, "geriatric" starts at exactly 65 - so applying right-closure
            # to a preset shifts every boundary by a year and turns the first WHO
            # band into a six-year band, while the UI still names the preset.
            use_right <- identical(self$options$age_interval, "right")
            if (use_right && age_groups != 'custom') {
                use_right <- FALSE
                preset_names <- c(who = .("WHO/UN standard"), who_infant = .("WHO abridged"),
                                  pediatric = .("Pediatric"), reproductive = .("Reproductive"),
                                  geriatric = .("Geriatric"), lifecourse = .("Life course"))
                preset_label <- if (age_groups %in% names(preset_names))
                    unname(preset_names[age_groups]) else age_groups
                # INFO, not WARNING: 'age_interval' is greyed out while a preset is
                # chosen (enable: (age_groups:custom) in the .u.yaml), so a user who
                # set right-closure before switching to a preset cannot clear a
                # warning from the visible UI. State what was used and how to get
                # the selector back instead.
                private$.addNotice(
                    "INFO",
                    .("Right-closed bands do not apply to a preset age grouping"),
                    jmvcore::format(
                        .("'Age band boundaries' is set to right-closed, but the {preset} preset is defined by its source as left-closed bands, so the bands below are the preset's own (for the WHO/UN preset: 0-4, 5-9, ... 85+). Applying right-closure would have moved every boundary by one year and the result could no longer be described as {preset} age groups. The 'Age band boundaries' selector is greyed out while a preset is chosen: set 'Age group preset' back to 'Custom (use bin width)' to use right-closed bands, or to set that selector back to left-closed and remove this note."),
                        preset = preset_label)
                )
            }

            # Guard the number of bands. bin_width deliberately has no upper bound
            # (0.25 gives three-month neonatal bands), but nothing capped what it
            # produced: a mistyped 0.01 over a 0-100 age range asks for 10,000
            # bands, i.e. 10,001 table rows and 20,000 bars, which locks the
            # session up and draws an unreadable figure. Custom breaks are
            # unbounded in the same way.
            n_bands <- length(breaks_seq) - 1
            if (n_bands > 200) {
                private$.addNotice(
                    "ERROR",
                    .("Too many age bands to draw"),
                    jmvcore::format(
                        .("The current settings produce {n} age bands over an age range ending at {maxAge} years. No table or figure was drawn: that many bands need one table row and two bars each, which takes a long time to render and cannot be read. Increase the bin width, type fewer custom breaks, or choose one of the age group presets - about 25 bands or fewer is readable at the size this plot is drawn."),
                        n = n_bands, maxAge = round(max_age, 2))
                )
                self$results$dataInfo$setContent("")
                return()
            }
            # (The "many narrow bands" warning is raised after cut(), where the
            # occupied band count and the final n are both known.)

            labels <- private$.create_age_labels(breaks_seq, right = use_right,
                                                 include_lowest = TRUE,
                                                 whole_ages = age_whole,
                                                 last_width = if (using_bin_width_breaks) self$options$bin_width else NULL)

            # Unequal band widths: the bars and the percentage columns are counts
            # PER BAND, so a band covering more single years collects more people
            # for that reason alone (the unequal-class-interval histogram error).
            # Only the WHO/UN preset and the fixed bin width give equal bands.
            finite_breaks <- breaks_seq[is.finite(breaks_seq)]
            band_widths <- if (length(finite_breaks) >= 2) diff(finite_breaks) else numeric(0)
            if (length(band_widths) >= 2 && (max(band_widths) - min(band_widths)) > 1e-8) {
                widest <- which.max(band_widths)
                narrowest <- which.min(band_widths)
                private$.addNotice(
                    "WARNING",
                    .("The age bands cover different numbers of years"),
                    jmvcore::format(
                        .("The bands are not all the same width: '{widest}' spans {widestYears} year(s) while '{narrowest}' spans {narrowestYears} year(s). The bar lengths and the 'Female (%)' / 'Male (%)' columns are counts per band, not per year of age, so a wider band collects more people simply by covering more years and its bar is longer for that reason alone. Compare bands of equal width with each other, or divide a band's count by its width in years before comparing. Bands of one fixed width come from the 'Custom (use bin width)' preset."),
                        widest = labels[widest],
                        widestYears = base::format(band_widths[widest], trim = TRUE, scientific = FALSE),
                        narrowest = labels[narrowest],
                        narrowestYears = base::format(band_widths[narrowest], trim = TRUE, scientific = FALSE))
                )
            }

            if (!age_whole && !use_right) {
                private$.addNotice(
                    "INFO",
                    .("Some ages are not whole years"),
                    .("Not every age is a whole number, so read the band labels as completed years of age (age at last birthday): a band labelled '10-14' holds every age from 10 up to just below 15, so 14.6 years is counted in it, and an age of exactly 15 starts the next band. Counts and percentages are computed from the ages as recorded, not from rounded ages.")
                )
            }

            mydata[["Pop"]] <- cut(mydata[["Age"]],
                                   include.lowest = TRUE,
                                   right = use_right,
                                   breaks = breaks_seq,
                                   labels = labels,
                                   ordered_result = FALSE)

            # Reconcile counts: ages that fall outside the break span (e.g. custom
            # breaks that do not start at 0) become NA here and were previously
            # dropped from the table/plot while still counted in "Final
            # observations". Count them, remove them so downstream outputs are
            # consistent, and recompute n_final so the Data Summary matches the
            # plotted/tabled Total (data-integrity).
            n_unbinned <- sum(is.na(mydata[["Pop"]]))
            if (n_unbinned > 0) {
                mydata <- mydata[!is.na(mydata[["Pop"]]), , drop = FALSE]
            }
            n_final <- nrow(mydata)  # Recompute after dropping out-of-range ages

            if (n_final == 0) {
                private$.rejectClean(.("No observations fall within the specified age break range. Adjust the custom age breaks or bin width so they cover the data."))
            }

            # Band-size notices belong HERE, not before cut(): n_final is only
            # final once the out-of-range rows are gone, and the table and both
            # plots draw only the bands that hold at least one observation
            # (dplyr::count() drops empty ones), so length(breaks_seq) - 1
            # overstates what the reader actually sees.
            band_totals <- as.integer(table(mydata[["Pop"]]))
            n_occupied_bands <- sum(band_totals > 0)
            if (n_occupied_bands > 25) {
                private$.addNotice(
                    "WARNING",
                    .("Many narrow age bands"),
                    jmvcore::format(
                        .("The current settings fill {nBands} age bands with {n} observations, an average of {perBand} observations per band. Bars that thin move by a large fraction of their height when one patient is added or removed, so the outline of the pyramid mostly reflects that variation. A wider bin width or one of the presets gives a more readable figure."),
                        nBands = n_occupied_bands, n = n_final,
                        perBand = round(n_final / n_occupied_bands, 1))
                )
            }

            # Prepare data for plotting and table output ----
            private$.checkpoint()
            plotData <- mydata %>%
                dplyr::select(Gender = Gender2, Pop) %>%
                dplyr::group_by(Gender, Pop) %>%
                dplyr::count() %>%
                dplyr::ungroup() %>%
                dplyr::arrange(Pop) %>%
                as.data.frame()

            # Percentage base, shared by the table and the percent axis of the
            # plots: within each gender (each side sums to 100, compares the SHAPE
            # of the two distributions) or of all analysed observations (both
            # sides together sum to 100, compares their SIZE as well).
            pct_base <- self$options$pct_base
            if (is.null(pct_base)) pct_base <- "within_gender"

            # Plot state carries a per-band percentage next to the count so the
            # renderers can switch axis without re-reading the data. Kept off
            # plotData itself: pivot_wider() below would treat `pct` as an id
            # column and produce one row per (band, pct) pair.
            plotState <- plotData
            gender_totals <- tapply(plotData$n, as.character(plotData$Gender), sum)
            plotState$pct <- if (pct_base == "total") {
                plotData$n / sum(plotData$n) * 100
            } else {
                plotData$n / as.numeric(gender_totals[as.character(plotData$Gender)]) * 100
            }

            # Save state for plot rendering; ensures plot gets updated when bin_width changes
            image <- self$results$plot
            image$setState(plotState)

            # Also save state for ggcharts plot (if enabled).
            # Store the raw grid; .plotGGCharts prepares it once at render time
            # (avoids preparing the data twice).
            if (self$options$enableGGCharts) {
                # Detectable here (unlike inside the renderer, which cannot write to
                # results), so raise a proper notice rather than only a placeholder panel.
                if (!requireNamespace("ggcharts", quietly = TRUE)) {
                    private$.addNotice(
                        "WARNING",
                        .("The ggcharts pyramid cannot be drawn"),
                        .("The R package 'ggcharts' is not installed, so the second pyramid (the 'Age Pyramid (ggcharts)' plot) is empty. The main Age Pyramid plot and the Population Data table above are complete and unaffected. To get the ggcharts version, install the package with install.packages(\"ggcharts\") and re-run; otherwise clear the 'ggcharts pyramid' checkbox to hide the empty plot.")
                    )
                }
                imageGGCharts <- self$results$plotGGCharts
                imageGGCharts$setState(plotState)
            }

            # Pivot data for table output ----
            plotData2 <- plotData %>%
                tidyr::pivot_wider(names_from = Gender,
                                   values_from = n,
                                   values_fill = list(n = 0)) %>%  # Fill missing counts with 0
                dplyr::arrange(dplyr::desc(Pop)) %>%
                dplyr::filter(!is.na(Pop)) %>%
                dplyr::mutate(Pop = as.character(Pop)) %>%
                as.data.frame()

            # Calculate totals and add percentages ----
            if (!("Female" %in% names(plotData2))) {
                plotData2$Female <- 0
            }
            if (!("Male" %in% names(plotData2))) {
                plotData2$Male <- 0
            }
            total_female <- sum(plotData2$Female, na.rm = TRUE)
            total_male <- sum(plotData2$Male, na.rm = TRUE)

            # Data-quality notices ----
            # These conditions were previously visible only as coloured text inside
            # the Data Summary table, which is easy to read past.
            if (total_female == 0 || total_male == 0) {
                # One complete sentence per case, so each translates on its own.
                template <- if (total_male == 0) {
                    .("All {n} analysed observations fall in the female group, so the figure is a one-sided age distribution rather than a two-sided pyramid, and the male column of the Population Data table is zero in every band. If the data do contain both groups, check the 'Female level' and 'Male level' selectors and the 'Missing/unrecognized gender' count in the Data Summary.")
                } else {
                    .("All {n} analysed observations fall in the male group, so the figure is a one-sided age distribution rather than a two-sided pyramid, and the female column of the Population Data table is zero in every band. If the data do contain both groups, check the 'Female level' and 'Male level' selectors and the 'Missing/unrecognized gender' count in the Data Summary.")
                }
                private$.addNotice(
                    "STRONG_WARNING",
                    .("Only one gender group is present"),
                    jmvcore::format(template, n = n_final)
                )
            }

            if (n_final < 30) {
                private$.addNotice(
                    "STRONG_WARNING",
                    .("Few observations behind each band"),
                    jmvcore::format(
                        .("The pyramid is built on {n} observations spread over {nBands} age band(s). Counts this small change by whole percentage points when a single case is added or removed, so the percentages in the table and the outline of the figure describe these particular cases and not a stable estimate of the age distribution they came from."),
                        n = n_final, nBands = n_occupied_bands)
                )
            } else {
                n_sparse <- sum(band_totals > 0 & band_totals < 5)
                if (n_sparse > 0) {
                    private$.addNotice(
                        "WARNING",
                        .("Some age bands hold very few observations"),
                        jmvcore::format(
                            .("{nSparse} of the {nBands} occupied age bands hold fewer than 5 observations. The percentages shown for those bands move by a large step per case, so differences between neighbouring short bars are not informative. Widening the bin width pools them into steadier bands."),
                            nSparse = n_sparse, nBands = n_occupied_bands)
                    )
                }
            }

            n_excluded_total <- n_initial - n_final
            if (n_initial > 0 && n_excluded_total > 0 &&
                    (n_excluded_total / n_initial) > 0.20) {
                exclusion_counts <- c(
                    max(0, n_excluded_total - n_invalid_age - n_missing_gender - n_unbinned),
                    n_invalid_age, n_missing_gender, n_unbinned)
                exclusion_labels <- c(
                    .("missing age or gender in the source data"),
                    .("ages that are not usable numbers (non-numeric, negative or infinite)"),
                    .("gender values matching neither the female nor the male level"),
                    .("ages outside the age-break range"))
                top <- which.max(exclusion_counts)
                private$.addNotice(
                    "STRONG_WARNING",
                    .("A large share of the rows was excluded"),
                    jmvcore::format(
                        .("{nExcluded} of the {nTotal} rows ({pct}%) are not in this pyramid. The largest single reason was {reason} ({nReason} rows). Every count and percentage shown describes only the {nFinal} rows that remained, so the figure is representative of the whole dataset only if the excluded rows have the same age and gender make-up as the kept ones. The full breakdown is in the Data Summary."),
                        nExcluded = n_excluded_total, nTotal = n_initial,
                        pct = round(n_excluded_total / n_initial * 100, 1),
                        reason = exclusion_labels[top], nReason = exclusion_counts[top],
                        nFinal = n_final)
                )
            }

            # Add percentage columns (safe division).
            #
            # NOT ifelse(): it is vectorised over its CONDITION, and
            # `total_female > 0` is length 1, so ifelse() returned only the FIRST
            # element of the percentage vector and recycled it down every row.
            # Every band was shown the first band's percentage - e.g. "0% female"
            # printed against bands containing 3 women, and "50% male" against a
            # band containing none - while the table's own note promised column
            # percentages summing to 100. Counts were unaffected, so the error was
            # visible only by checking the two columns against each other.
            female_base <- if (pct_base == "total") n_final else total_female
            male_base <- if (pct_base == "total") n_final else total_male
            pct_of <- function(x, base) {
                if (base > 0) round(x / base * 100, 1) else rep(0, length(x))
            }
            plotData2$Female_Pct <- pct_of(plotData2$Female, female_base)
            plotData2$Male_Pct <- pct_of(plotData2$Male, male_base)

            # Add summary row: 100/100 within gender, the two gender shares of
            # all observations otherwise.
            summary_row <- data.frame(
                Pop = "Total",
                Female = total_female,
                Male = total_male,
                Female_Pct = pct_of(total_female, female_base),
                Male_Pct = pct_of(total_male, male_base),
                stringsAsFactors = FALSE
            )

            plotData2 <- rbind(plotData2, summary_row)

            # Populate the results table ----
            pyramidTable <- self$results$pyramidTable
            # addRow() does not check for an existing rowKey, and the table is
            # only cleared by its clearWith options: a change to plot_title or
            # a colour re-runs .run() with the rows still in place, so without
            # this the table doubled on every such change.
            pyramidTable$deleteRows()
            for(i in seq_len(nrow(plotData2))) {
                pyramidTable$addRow(rowKey = i, values = plotData2[i,])
            }
            pyramidTable$setNote(
                "pct",
                if (pct_base == "total") {
                    jmvcore::format(
                        .("Percentages are of all {n} analysed observations (the Female and Male columns together sum to 100%; the Total row shows each gender's share). Rounded per-bin percentages may not total exactly 100."),
                        n = n_final)
                } else {
                    .("Percentages are column percentages within each gender (Female and Male each sum to 100%). Rounded per-bin percentages may not total exactly 100.")
                })

            # Build data summary HTML ----
            info_html <- private$.build_data_summary_html(
                n_initial = n_initial,
                n_final = n_final,
                is_single_gender = is_single_gender,
                female_level = female_level,
                male_level = male_level,
                single_gender_label = single_gender_label,
                n_invalid_age = n_invalid_age,
                n_missing_gender = n_missing_gender,
                n_unbinned = n_unbinned
            )
            self$results$dataInfo$setContent(info_html)
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Check if required options (age and gender) are provided
            if (is.null(self$options$age) || is.null(self$options$gender))
                return()

            # No self$data read here on purpose. `requiresData: true` on the Image
            # made jmvcore re-read the whole dataset from disk on every render -
            # including every window resize and every reopen of a saved .omv - and
            # the only thing that read was used for was an empty-data check that
            # the is.null(image$state) guard below already covers.
            plotData <- image$state

            # Return early if no plot data available (e.g., validation errors in .run())
            if (is.null(plotData))
                return(FALSE)

            # Ensure that the age bins (Pop) reflect the latest bin width:
            # Convert 'Pop' to character then back to factor with the order of appearance.
            plotData$Pop <- factor(as.character(plotData$Pop), levels = unique(as.character(plotData$Pop)))

            # Bar values: counts, or the per-band percentage computed in .run()
            # on the chosen percentage base (state saved before this option
            # existed carries no `pct`, so fall back to counts).
            use_pct <- identical(self$options$plot_values, "percent") && !is.null(plotData$pct)
            plotData$v <- if (use_pct) plotData$pct else plotData$n
            y_label <- if (!use_pct) {
                .("Population Count")
            } else if (identical(self$options$pct_base, "total")) {
                .("Percent of all observations")
            } else {
                .("Percent within gender")
            }
            y_max <- max(plotData$v, na.rm = TRUE)

            # Set plot title (using user option if provided)
            plot_title <- if (!is.null(self$options$plot_title)) self$options$plot_title else .("Age Pyramid")

            # Determine color palette ----
            color_palette <- self$options$color_palette
            if (is.null(color_palette) || length(color_palette) == 0) {
                color_palette <- 'standard'
            }

            # Set colors based on palette selection
            if (color_palette == 'colorblind') {
                # Orange/Blue palette (colorblind-friendly)
                color_female <- "#E69F00"  # Orange
                color_male <- "#0072B2"    # Blue
            } else if (color_palette == 'grayscale') {
                # Grayscale palette
                color_female <- "#666666"  # Dark gray
                color_male <- "#CCCCCC"    # Light gray
            } else if (color_palette == 'custom') {
                # Custom colors from user
                color_female <- self$options$female_color
                color_male <- self$options$male_color
                # Fallback to defaults if colors are empty or not valid colors
                # (prevents an unstructured crash in scale_fill_manual on bad hex)
                if (!private$.is_valid_color(color_female)) {
                    color_female <- "#E91E63"
                }
                if (!private$.is_valid_color(color_male)) {
                    color_male <- "#2196F3"
                }
            } else {
                # Standard palette (default pink/blue)
                color_female <- "#E91E63"  # Pink
                color_male <- "#2196F3"    # Blue
            }

            # Create a visually appealing age pyramid plot ----
            plot <- ggplot2::ggplot(data = plotData,
                                    mapping = ggplot2::aes(
                                        x = Pop,
                                        y = ifelse(Gender == "Female", -v, v),
                                        fill = Gender
                                    )) +
                ggplot2::geom_col(width = 0.7, color = "black", show.legend = TRUE) +  # Added border for clarity
                ggplot2::coord_flip() +
                ggplot2::scale_y_continuous(labels = if (use_pct) function(x) paste0(abs(x), "%") else abs,
                                            limits = c(-y_max, y_max)
                ) +
                ggplot2::scale_fill_manual(values = c("Female" = color_female, "Male" = color_male),
                                           labels = c("Female" = .("Female"), "Male" = .("Male"))) +
                ggplot2::labs(x = .("Age Group"),
                              y = y_label,
                              title = plot_title,
                              fill = .("Gender"))

            # Apply theme based on user preference
            if (!self$options$originaltheme) {
                # Use jamovi's theme
                plot <- plot + ggtheme
            } else {
                # Use original code's custom theme
                plot <- plot +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        axis.text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.position = "bottom"
                    )
            }

            print(plot)
            return(TRUE)
        },

        .plotGGCharts = function(image, ggtheme, theme, ...) {
            # Check if ggcharts plot is enabled
            if (!self$options$enableGGCharts)
                return(FALSE)

            # Check if required options (age and gender) are provided
            if (is.null(self$options$age) || is.null(self$options$gender))
                return(FALSE)

            # See .plot(): no self$data read here, the state guard below covers it.
            # Retrieve the prepared plot data from .run()
            # We'll use the same data preparation that was done for the main plot
            plotData <- image$state

            # Return early if no plot data available
            if (is.null(plotData))
                return(FALSE)

            # Percent axis: substitute the per-band percentage for the count
            # BEFORE the grid is completed (the filled-in zero rows have no pct).
            if (identical(self$options$plot_values, "percent") && !is.null(plotData$pct)) {
                plotData$n <- plotData$pct
            }
            plotData$pct <- NULL

            # ggcharts::pyramid_chart() assigns sides and colors from the first
            # appearance order of `group`. Use a completed Female/Male grid so
            # side assignment and age-bin alignment are deterministic.
            plotData <- private$.prepare_ggcharts_data(plotData)
            if (nrow(plotData) == 0)
                return(FALSE)

            # ggcharts pyramid_chart requires long-format data with:
            # - x: age groups (categorical)
            # - y: population counts (numeric)
            # - group: gender categories (exactly 2 unique values)

            # The plotData from .run() is already in the correct format:
            # columns: Gender, Pop, n

            # Determine bar colors based on color palette selection
            color_scheme <- self$options$ggcharts_colors
            if (is.null(color_scheme) || length(color_scheme) == 0) {
                color_scheme <- 'default'
            }

            # Set colors based on palette selection
            if (color_scheme == 'default') {
                # ggcharts default: Blue and Orange
                bar_colors <- c("#1F77B4", "#FF7F0E")
            } else if (color_scheme == 'standard') {
                # Standard: Pink and Blue (matching main plot)
                bar_colors <- c("#E91E63", "#2196F3")
            } else if (color_scheme == 'colorblind') {
                # Colorblind-friendly: Orange and Blue
                bar_colors <- c("#E69F00", "#0072B2")
            } else if (color_scheme == 'grayscale') {
                # Grayscale
                bar_colors <- c("#666666", "#CCCCCC")
            } else if (color_scheme == 'custom') {
                # Custom colors from user
                color1 <- self$options$ggcharts_color1
                color2 <- self$options$ggcharts_color2
                # Fallback to defaults if colors are empty or not valid colors
                if (!private$.is_valid_color(color1)) {
                    color1 <- "#1F77B4"
                }
                if (!private$.is_valid_color(color2)) {
                    color2 <- "#FF7F0E"
                }
                bar_colors <- c(color1, color2)
            } else {
                # Default fallback
                bar_colors <- c("#1F77B4", "#FF7F0E")
            }

            # Get other options
            sort_option <- if (!is.null(self$options$ggcharts_sort)) {
                self$options$ggcharts_sort
            } else {
                "no"
            }

            plot_title <- if (!is.null(self$options$ggcharts_title)) {
                self$options$ggcharts_title
            } else {
                .("Age Pyramid (ggcharts)")
            }

            xlab_text <- if (!is.null(self$options$ggcharts_xlab)) {
                self$options$ggcharts_xlab
            } else {
                .("Population")
            }

            # Create the ggcharts pyramid
            tryCatch({
                plot <- ggcharts::pyramid_chart(
                    data = plotData,
                    x = Pop,
                    y = n,
                    group = Gender,
                    bar_colors = bar_colors,
                    sort = sort_option,
                    xlab = xlab_text,
                    title = plot_title
                )

                print(plot)
                return(TRUE)
            }, error = function(e) {
                # A render-phase failure cannot write to a results element, so the
                # guidance is drawn into the plot panel itself (IN_PLOT_FALLBACK).
                # A bare warning() would surface only as unattached text in the
                # Analysis Notes panel, detached from the plot it describes.
                n_groups <- length(unique(as.character(plotData$Pop)))
                # One .() per sentence; line breaks are added here with strwrap so
                # the translated text still fits the panel.
                wrap <- function(x) paste(strwrap(x, width = 62), collapse = "\n")
                fallback <- ggplot2::ggplot() +
                    ggplot2::annotate(
                        "text", x = 0, y = 0, hjust = 0.5, vjust = 0.5, size = 3.5,
                        label = paste(
                            wrap(.("The ggcharts pyramid could not be drawn.")),
                            wrap(jmvcore::format(.("(ggcharts received {nRows} rows covering {nGroups} age group(s).)"),
                                                 nRows = nrow(plotData), nGroups = n_groups)),
                            "",
                            wrap(.("The main Age Pyramid plot and the Population Data table above are complete and unaffected - only this second, optional plot is missing.")),
                            "",
                            wrap(.("What to try next: widen the age bin width (or choose a preset) so more than one age group is produced, and check that any custom bar colors are valid color names or #RRGGBB codes. If you do not need this view, clear the 'ggcharts pyramid' checkbox to hide it.")),
                            "",
                            wrap(jmvcore::format(.("Technical detail from ggcharts: {message}"), message = e$message)),
                            sep = "\n"
                        )
                    ) +
                    ggplot2::theme_void()
                print(fallback)
                return(TRUE)
            })
        },

        .prepare_ggcharts_data = function(plotData) {
            if (is.null(plotData) || nrow(plotData) == 0)
                return(plotData)

            observed_pop <- unique(as.character(plotData$Pop[!is.na(plotData$Pop)]))
            pop_levels <- if (is.factor(plotData$Pop)) {
                levels(plotData$Pop)[levels(plotData$Pop) %in% observed_pop]
            } else {
                observed_pop
            }

            if (length(pop_levels) == 0)
                return(plotData[0, , drop = FALSE])

            plotData %>%
                dplyr::filter(!is.na(Pop)) %>%
                dplyr::mutate(
                    Gender = factor(as.character(Gender), levels = c("Female", "Male")),
                    Pop = factor(as.character(Pop), levels = pop_levels)
                ) %>%
                dplyr::filter(!is.na(Gender), !is.na(Pop)) %>%
                tidyr::complete(
                    Gender = factor(c("Female", "Male"), levels = c("Female", "Male")),
                    Pop = factor(pop_levels, levels = pop_levels),
                    fill = list(n = 0)
                ) %>%
                dplyr::arrange(Gender, Pop) %>%
                dplyr::mutate(Gender = as.character(Gender)) %>%
                as.data.frame()
        },

        .is_valid_color = function(x) {
            # TRUE if x is a single non-empty string that R recognizes as a color
            # (hex like #RRGGBB or a named color). Guards scale_fill_manual /
            # ggcharts bar_colors against unstructured crashes on invalid input.
            if (is.null(x) || length(x) != 1 || is.na(x))
                return(FALSE)
            x <- trimws(x)
            if (nchar(x) == 0)
                return(FALSE)
            tryCatch({
                grDevices::col2rgb(x)
                TRUE
            }, error = function(e) FALSE)
        },

        # Map a gender level label onto a side, or NA when it is not recognised.
        # Deliberately narrow: an unrecognised label falls back to level order and
        # is disclosed in a notice, which is safer than a loose guess.
        .gender_side = function(label) {
            key <- gsub("[^a-z]", "", tolower(trimws(as.character(label))))
            if (length(key) != 1 || is.na(key) || !nzchar(key))
                return(NA_character_)
            # "kadn" is "kadin"/"kad\u{131}n" after the non-letter strip
            if (key %in% c("f", "fem", "female", "females", "w", "woman", "women",
                           "k", "kadin", "kadn"))
                return("female")
            if (key %in% c("m", "male", "males", "man", "men", "e", "erkek"))
                return("male")
            NA_character_
        },

        # Both sides read from the level names, or NULL if that is not unambiguous.
        .guess_gender_levels = function(gender_levels) {
            if (length(gender_levels) < 2)
                return(NULL)
            sides <- vapply(gender_levels, private$.gender_side, character(1),
                            USE.NAMES = FALSE)
            female <- gender_levels[!is.na(sides) & sides == "female"]
            male <- gender_levels[!is.na(sides) & sides == "male"]
            if (length(female) == 1 && length(male) == 1)
                return(list(female = female, male = male))
            NULL
        },

        # Fill one side from the levels the user left over: prefer a level whose
        # own name says which side it is, otherwise take the first remaining one.
        .pick_level = function(candidates, side) {
            if (length(candidates) == 0)
                return(NULL)
            sides <- vapply(candidates, private$.gender_side, character(1),
                            USE.NAMES = FALSE)
            named <- candidates[!is.na(sides) & sides == side]
            if (length(named) == 1)
                return(named)
            candidates[1]
        },

        .bin_width_breaks = function(max_age) {
            bin_width <- if (!is.null(self$options$bin_width)) self$options$bin_width else 5
            if (!is.numeric(bin_width) || length(bin_width) != 1 ||
                    is.na(bin_width) || !is.finite(bin_width) || bin_width <= 0) {
                private$.rejectClean(.("Bin width must be a positive number"))
            }
            # The top band is open-ended. seq() stops at or below max_age and
            # include.lowest = TRUE then closes the top of the last FINITE band, so
            # ages 0-100 in 5-year bins used to end in a "95-100" band holding six
            # single years (95 to 100) while every other band held five.
            #
            # When max_age < bin_width, seq(0, max_age, bin_width) returns only c(0),
            # giving breaks c(0, Inf) and the label "0+" instead of "0-4". Ensure at
            # least one finite step exists by extending to at least bin_width.
            upper_bound <- max(max_age, bin_width)
            c(seq(from = 0, to = upper_bound, by = bin_width), Inf)
        },


        .create_age_labels = function(breaks, right = FALSE, include_lowest = TRUE,
                                      whole_ages = TRUE, last_width = NULL) {
            # Labels that describe exactly the ages their band contains, under
            # EITHER closure convention.
            #
            # right = FALSE (default, WHO/UN): bands are [lower, upper). Equal-width
            #   bands hold equal numbers of single years, and a boundary age starts
            #   the band named for it - 65 is geriatric, 18 is not paediatric.
            #   include_lowest closes the TOP of the final band, so the oldest
            #   observation is kept when the last break is finite.
            #
            # right = TRUE (pre-1.0.52 behaviour, offered for continuity): bands are
            #   (lower, upper]. include_lowest closes the BOTTOM of the FIRST band,
            #   making it [b1, b2] - one single year wider than the rest. That is why
            #   this convention inflates the youngest bar; the label says so honestly
            #   by naming the lower bound, which the original implementation did not
            #   (it labelled [0,5] as "1-5", hiding every age-0 infant).
            if (length(breaks) < 2) return(c())

            whole <- function(z) is.finite(z) && abs(z - round(z)) < .Machine$double.eps^0.5
            n_bands <- length(breaks) - 1

            labels <- character(n_bands)
            for (i in seq_len(n_bands)) {
                lower <- breaks[i]
                upper <- breaks[i + 1]
                is_first <- i == 1
                is_last <- i == n_bands

                if (right) {
                    # (lower, upper], except the first band which include_lowest
                    # widens to [lower, upper].
                    #
                    # The whole-year forms ("6-10", "85+") name the band by
                    # lower + 1, which is only the smallest age in the band when
                    # ages are whole numbers AND the bound is a whole number:
                    # (5, 10] holds 5.5, which is not "6-10", and (84, Inf] holds
                    # 84.5, which is not "85+". Fall back to the exact interval
                    # forms otherwise. The first band is closed at the bottom by
                    # include_lowest, so it is never named with ">".
                    closed_below <- is_first && include_lowest
                    if (is.infinite(upper)) {
                        if (closed_below) {
                            labels[i] <- paste0(lower, "+")
                        } else if (whole(lower) && whole_ages) {
                            labels[i] <- paste0(lower + 1, "+")
                        } else {
                            labels[i] <- paste0(">", lower)
                        }
                    } else if (closed_below) {
                        labels[i] <- paste(lower, upper, sep = "-")
                    } else if (whole(lower) && whole(upper) && whole_ages) {
                        # A one-year band is "5", not "5-5".
                        labels[i] <- if ((lower + 1) == upper) as.character(upper)
                                     else paste(lower + 1, upper, sep = "-")
                    } else {
                        labels[i] <- paste0(">", lower, "-", upper)
                    }
                } else {
                    # [lower, upper), except the last band which include_lowest
                    # closes at the top when `upper` is finite.
                    #
                    # When `upper` is Inf because the bin-width path stopped at
                    # the oldest observation, the band still spans one bin width
                    # (seq() stops at the last multiple at or below max_age, so
                    # max_age < lower + width). Label it lower-(lower+width-1),
                    # the same form as every band before it and as the presets
                    # ("70-74"), instead of lower-max_age ("70-73"), which read
                    # as a shorter band and changed whenever an older patient
                    # was added.
                    if (is.infinite(upper)) {
                        if (!is.null(last_width) && whole(lower) && whole(last_width) &&
                            whole_ages) {
                            labels[i] <- paste(lower, lower + last_width - 1, sep = "-")
                        } else {
                            labels[i] <- paste0(lower, "+")
                        }
                    } else if (is_last && include_lowest) {
                        labels[i] <- paste(lower, upper, sep = "-")
                    } else if (whole(lower) && whole(upper) && (upper - lower) == 1 && lower == 0) {
                        # WHO writes the infant band as "<1" rather than "0-0"
                        labels[i] <- "<1"
                    } else if (whole(lower) && whole(upper) && (upper - lower) == 1) {
                        # A one-year band is "1" (WHO abridged notation), not "1-1",
                        # which is what [1, 2) got from the branch below.
                        labels[i] <- as.character(lower)
                    } else if (whole(lower) && whole(upper) && (upper - lower) >= 1) {
                        labels[i] <- paste(lower, upper - 1, sep = "-")
                    } else {
                        labels[i] <- paste0(lower, "-<", upper)
                    }
                }
            }
            return(labels)
        },

        .build_data_summary_html = function(n_initial, n_final, is_single_gender,
                                           female_level, male_level, single_gender_label,
                                           n_invalid_age = 0, n_missing_gender = 0,
                                           n_unbinned = 0) {
            # Build informative HTML showing data quality and gender level info
            n_excluded <- n_initial - n_final
            # Rows removed by jmvcore::naOmit (source NA in age or gender) are not
            # captured by the age/gender/range counters; derive them so the
            # breakdown sub-items sum exactly to the total Excluded.
            n_source_na <- max(0, n_excluded - n_invalid_age - n_missing_gender - n_unbinned)

            esc <- htmltools::htmlEscape
            row <- function(label, value, value_style = NULL) {
                paste0("<tr><td><strong>", esc(label), "</strong></td><td",
                       if (is.null(value_style)) "" else paste0(" style='", value_style, "'"),
                       ">", value, "</td></tr>")
            }
            subrow <- function(label, value) {
                paste0("<tr><td style='padding-left: 20px; font-size: 13px;'>- ", esc(label),
                       "</td><td style='color: #e05252; font-size: 13px;'>", value, "</td></tr>")
            }

            html <- "<div style='background-color: rgba(33, 152, 239, 0.13); padding: 15px; border-radius: 8px; border-left: 4px solid #2196F3; color: inherit;'>"
            html <- paste0(html, "<h4 style='margin: 0 0 8px 0; color: #4a9eea;'>", esc(.("Data Summary")), "</h4>")
            html <- paste0(html, "<table style='width: 100%; font-size: 14px;'>")
            html <- paste0(html, row(.("Initial observations:"), n_initial))
            html <- paste0(html, row(.("Final observations:"), n_final))

            if (n_excluded > 0) {
                pct_excluded <- round(n_excluded / n_initial * 100, 1)
                html <- paste0(html, row(.("Excluded:"), paste0(n_excluded, " (", pct_excluded, "%)"),
                                         "color: #e05252;"))

                # Add breakdown (sub-items sum to the total Excluded above)
                if (n_source_na > 0)
                    html <- paste0(html, subrow(.("Missing age/gender (source NA):"), n_source_na))
                if (n_invalid_age > 0)
                    html <- paste0(html, subrow(.("Unusable ages (non-numeric, negative or infinite):"), n_invalid_age))
                if (n_missing_gender > 0)
                    html <- paste0(html, subrow(.("Missing/unrecognized gender:"), n_missing_gender))
                if (n_unbinned > 0)
                    html <- paste0(html, subrow(.("Outside age-break range:"), n_unbinned))
            }

            if (is_single_gender) {
                html <- paste0(html, row(.("Cohort type:"),
                    esc(jmvcore::format(.("Single-gender ({level})"), level = single_gender_label)),
                    "color: #ef8c2e;"))
            } else {
                html <- paste0(html, row(.("Female level:"), esc(female_level)))
                html <- paste0(html, row(.("Male level:"), esc(male_level)))
            }

            html <- paste0(html, "</table></div>")
            return(html)
        }
    )
)
