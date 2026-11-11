#' @title Segmented Total Bar Charts
#' @description
#' Create segmented total bar charts (100% stacked bars) that show proportional
#' breakdowns within categories. Perfect for displaying composition data where
#' each bar represents 100% and segments show relative proportions.
#'
#' @details
#' This module creates segmented total bar charts using ggplot2, where:
#' - Each bar represents 100% of the total for that category
#' - Segments within bars show relative proportions
#' - Colors distinguish different segments
#' - Labels can show percentages and/or raw counts
#'
#' The visualization approach is inspired by the ggsegmentedtotalbar package
#' (https://github.com/ozancanozdemir/ggsegmentedtotalbar) by Ozancan Ozdemir.
#' This implementation uses a clean ggplot2-based approach optimized for
#' clinical research workflows within the jamovi environment
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_col position_dodge coord_flip
#' @importFrom ggplot2 theme_minimal theme_classic theme_bw labs scale_fill_brewer
#' @importFrom ggplot2 scale_fill_viridis_d geom_text element_text element_blank theme element_rect
#' @importFrom ggplot2 element_line margin scale_y_continuous scale_fill_manual expansion unit
#' @importFrom ggplot2 facet_wrap vars
#' @importFrom dplyr group_by summarise mutate arrange desc count ungroup filter if_else n
#' @importFrom stats chisq.test fisher.test mantelhaen.test xtabs sd qnorm
#' @importFrom grDevices colorRampPalette
#' @importFrom scales percent
#' @importFrom stringr str_to_title
#' @importFrom rlang sym .data
#' @return An \code{R6} class generator object for the \code{jjsegmentedtotalbarClass} backend; used internally by the jamovi analysis wrapper and not called directly.

jjsegmentedtotalbarClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jjsegmentedtotalbarClass",
    inherit = jjsegmentedtotalbarBase,
    private = list(

        # Internal data storage (computed on-demand for better memory management)
        .processed_data = NULL,
        .composition_data = NULL,
        .preset_config = NULL,
        .var_labels = NULL,
        # The preset and variable labels .displayOpt() / .varLabel() resolve against.
        # .run() sets them; .plot() overwrites them from image$state for the duration of one
        # render, because on the export path .run() never ran and every private$ field
        # holds its initial value.
        .renderPreset = NULL,
        .renderVarLabels = NULL,
        # Rows actually analysed, after missing-value handling. This is the only
        # defensible "N" - the summed Value Variable is a quantity, not a count of
        # cases, unless the input happens to be pre-aggregated frequencies.
        .n_rows_analysed = NULL,

        # Initialize notice collection list
        .noticeList = list(),

        .sourcifyOption = function(option) {
            declared_opts <- c("data", setdiff(names(formals(self$options$initialize)), "..."))
            if (!option$name %in% declared_opts)
                return("")
            if (identical(option$name, "data"))
                return(super$.sourcifyOption(option))
            value <- option$value
            if (is.list(value) || length(value) > 1L)
                return("")
            if (inherits(option, "OptionVariable")) {
                if (is.null(value) || !is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value))
                    return("")
                if (make.names(value) == value && !grepl("^\\.\\.[0-9]+$", value))
                    return(super$.sourcifyOption(option))
                return(paste0(option$name, " = ", encodeString(value, quote = '"')))
            }
            if (is.character(value)) {
                if (length(value) != 1L || is.na(value) || identical(value, option$default))
                    return("")
                return(paste0(option$name, " = ", encodeString(value, quote = '"')))
            }
            super$.sourcifyOption(option)
        },

        # Sanitize user-supplied variable/level labels placed in table footnotes or
        # text cells: jamovi parses both as HTML (so break "<tag" openers without
        # entity-encoding), and Table$setNote() re-translates and cuts at " [" (so
        # map [] to () for notes while preserving real names in cells).
        .safeLabel = function(x, note = TRUE) {
            x <- as.character(x)
            if (note) x <- chartr("[]", "()", x)
            gsub("<([A-Za-z/!?])", "< \\1", x)
        },

        # Extract human-readable variable labels (e.g. haven / labelled "label" attributes)
        .extractVarLabels = function(raw_data, vars) {
            out <- list()
            if (is.null(raw_data)) return(out)
            for (v in unique(vars[!is.na(vars) & nzchar(vars)])) {
                if (v %in% names(raw_data)) {
                    lbl <- attr(raw_data[[v]], "label", exact = TRUE)
                    if (is.character(lbl) && length(lbl) == 1L && nzchar(trimws(lbl))) {
                        out[[v]] <- trimws(lbl)
                    }
                }
            }
            out
        },

        .varLabel = function(var_name) {
            if (is.null(var_name) || !nzchar(var_name)) return("")
            lbls <- private$.renderVarLabels
            if (!is.null(lbls) && !is.null(lbls[[var_name]]) && nzchar(lbls[[var_name]])) {
                return(lbls[[var_name]])
            }
            var_name
        },

        .metricVec = function(df, metric = "value") {
            internal_nm <- paste0("..", metric)
            if (internal_nm %in% names(df)) df[[internal_nm]] else df[[metric]]
        },

        # The Value Variable is `permitted: [numeric]` and `suggested: [continuous]`,
        # so a category total is very often NOT a whole number. Formatting one with
        # sprintf("%d") is a hard R error ("invalid format '%d'"), which took down
        # the whole analysis for every continuous input; as.integer() instead
        # silently truncated 1547.8 to "1547". Print whole totals as integers and
        # fractional totals to one decimal.
        .fmtTotal = function(x) {
            if (length(x) != 1 || !is.finite(x))
                return("NA")
            if (abs(x - round(x)) < 1e-9)
                formatC(round(x), format = "d", big.mark = "")
            else
                formatC(x, format = "f", digits = 1, big.mark = "")
        },

        # TRUE when every category total is a non-negative whole number, i.e. the
        # Value Variable behaves like a frequency. Chi-square is only defined on
        # counts, and the wording of the summaries depends on this too.
        .isCountData = function() {
            d <- private$.processed_data
            if (is.null(d) || !nrow(d))
                return(FALSE)
            cnts <- private$.metricVec(d, "count")
            all(is.finite(cnts)) &&
                all(abs(cnts - round(cnts)) < 1e-9) &&
                all(cnts >= 0)
        },

        # Read an option that may not exist in the compiled R/<fn>.h.R yet.
        # jmvcore's `$` ERRORS on an undeclared option ("options$x does not
        # exist") rather than returning NULL, so a .a.yaml addition that has not
        # been through jmvtools::prepare() takes down the whole analysis. Falling
        # back here means a stale header degrades to "the test did not run" - the
        # safe direction - instead of a crash.
        .optionOr = function(name, default) {
            tryCatch(self$options[[name]], error = function(e) default)
        },

        # Declared defaults, mirrored from jamovi/jjsegmentedtotalbar.a.yaml. Used
        # only to tell "the user left this alone" from "the user chose this".
        .presetDefaults = list(
            color_palette = "clinical", chart_style = "clinical",
            show_percentages = FALSE,   percentage_format = "integer"
        ),

        # Resolve a display option, honouring the selected Clinical Analysis Preset.
        #
        # The preset used to be stored in .preset_config and then read by nothing
        # except the guidance panel, so every preset produced a byte-identical
        # plot while its description promised a "predefined configuration".
        # self$options is read-only in jamovi, so the preset cannot be written
        # back into the options - it has to be applied where the value is read.
        # An explicit user choice always wins; the preset only fills in a control
        # the user has left at its default. These four options are purely
        # cosmetic, so this cannot change any reported number.
        .displayOpt = function(name) {
            user_value <- self$options[[name]]
            cfg <- private$.renderPreset
            if (is.null(cfg) || is.null(cfg[[name]]))
                return(user_value)
            if (identical(user_value, private$.presetDefaults[[name]]))
                return(cfg[[name]])
            user_value
        },

        # Collect notices into a single pre-declared `type: Html` item (`warnings`) so
        # multiple severity-sorted clinical and data-quality messages render in one
        # place without dynamic `Group$insert()` calls (see vignettes/jamovi_notices_guide.md).
        .addNotice = function(type, title, content) {
          private$.noticeList[[length(private$.noticeList) + 1]] <- list(
            type = type,
            title = title,
            content = content
          )
        },

        # Render collected notices as HTML ordered by severity with non-color text prefixes
        # (theme-safe: inherit text color so dark mode stays readable)
        .renderNotices = function() {
          if (length(private$.noticeList) == 0) {
            self$results$warnings$setContent("")
            self$results$warnings$setVisible(FALSE)
            return()
          }

          typeStyles <- list(
            ERROR = list(bgcolor = "rgba(220, 38, 38, 0.08)", border = "#dc2626", prefix = .("Error:")),
            STRONG_WARNING = list(bgcolor = "rgba(234, 88, 12, 0.08)", border = "#ea580c", prefix = .("Important:")),
            WARNING = list(bgcolor = "rgba(202, 138, 4, 0.08)", border = "#ca8a04", prefix = .("Warning:")),
            INFO = list(bgcolor = "rgba(37, 99, 235, 0.08)", border = "#2563eb", prefix = .("Note:"))
          )

          severity_rank <- match(
            vapply(private$.noticeList, function(x) if (is.null(x$type)) "INFO" else as.character(x$type), character(1)),
            names(typeStyles),
            nomatch = 4L
          )
          ordered_notices <- private$.noticeList[order(severity_rank)]

          html <- "<div style='margin: 10px 0; color: inherit;'>"

          for (notice in ordered_notices) {
            style <- typeStyles[[notice$type]]
            if (is.null(style)) {
              style <- typeStyles$INFO
            }
            raw_title <- if (is.null(notice$title)) "" else as.character(notice$title)
            pfx <- style$prefix
            header_txt <- if (nzchar(pfx) && !startsWith(tolower(trimws(raw_title)), tolower(sub(":$", "", pfx)))) {
              paste(pfx, raw_title)
            } else {
              raw_title
            }

            html <- paste0(html,
              "<div role='status' style='background-color: ", style$bgcolor, "; ",
              "border-left: 4px solid ", style$border, "; ",
              "padding: 12px; margin: 8px 0; border-radius: 4px; color: inherit;'>",
              "<strong style='color: inherit;'>",
              htmltools::htmlEscape(header_txt), "</strong><br>",
              "<span style='color: inherit;'>", htmltools::htmlEscape(notice$content), "</span>",
              "</div>"
            )
          }

          html <- paste0(html, "</div>")

          self$results$warnings$setContent(html)
          self$results$warnings$setVisible(TRUE)
        },

        .instructionsHtml = function() {
            has_x <- !is.null(self$options$x_var) && nzchar(self$options$x_var)
            has_y <- !is.null(self$options$y_var) && nzchar(self$options$y_var)
            has_fill <- !is.null(self$options$fill_var) && nzchar(self$options$fill_var)
            has_facet <- !is.null(self$options$facet_var) && nzchar(self$options$facet_var)

            chk <- function(ok, lbl, detail) {
                mark <- if (ok) "[x]" else "[ ]"
                val <- if (ok) paste0(" <em>(", htmltools::htmlEscape(detail), ")</em>") else ""
                paste0("<li style='margin: 4px 0;'><code>", mark, "</code> <strong>", lbl, "</strong>", val, "</li>")
            }

            paste0(
                "<div style='font-family: sans-serif; max-width: 780px; line-height: 1.5; color: inherit;'>",
                "<div style='background-color: rgba(88, 88, 88, 0.06); border-left: 4px solid #2563eb; padding: 16px; border-radius: 6px; margin-bottom: 12px; color: inherit;'>",
                "<h3 style='margin: 0 0 6px 0; color: inherit;'>", .("Segmented Total Bar Charts"), "</h3>",
                "<p style='margin: 0; color: inherit;'>",
                .("Compare proportional compositions across categories using 100% stacked bars or Flerlage segmented total bars."),
                "</p>",
                "</div>",
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 14px; border-radius: 6px; color: inherit;'>",
                "<h4 style='margin: 0 0 8px 0; color: inherit;'>", .("Setup Progress"), "</h4>",
                "<ul style='list-style: none; padding-left: 4px; margin: 0 0 10px 0;'>",
                chk(has_x, .("Category Variable (X-axis):"), self$options$x_var),
                chk(has_y, .("Value Variable (Y-axis):"), self$options$y_var),
                chk(has_fill, .("Segment Variable (Fill):"), self$options$fill_var),
                chk(has_facet, .("Panel Variable (Optional):"), self$options$facet_var),
                "</ul>",
                "<p style='margin: 0; font-size: 13px; color: inherit;'>",
                .("Select the three required variables in the left panel to compute composition tables and clinical summaries; tick 'Plot' to render the chart."),
                "</p>",
                "</div>",
                "</div>"
            )
        },

        .clearTableRows = function(table) {
            if (is.null(table)) return(invisible(NULL))
            table$deleteRows()
            if (length(table$names) > 0) {
                table$.__enclos_env__$private$.rowNames <- character()
            }
            invisible(table)
        },

        .syncPlotSize = function() {
            width_px <- self$options$plot_width * 72
            height_px <- self$options$plot_height * 72
            if (!is.null(self$results$plot) && is.function(self$results$plot$setSize)) {
                self$results$plot$setSize(width = width_px, height = height_px)
            }
        },

        # Clear all data-derived results so early returns or valid-to-invalid option
        # transitions on a reused R6 instance never leave stale outputs on screen.
        .resetResults = function() {
            private$.noticeList <- list()
            private$.processed_data <- NULL
            private$.composition_data <- NULL
            private$.var_labels <- NULL
            private$.renderVarLabels <- NULL
            private$.n_rows_analysed <- NULL

            if (!is.null(self$results$plot)) {
                self$results$plot$setState(NULL)
            }

            if (!is.null(self$results$summary)) {
                self$results$summary$setRow(
                    rowNo = 1,
                    values = list(
                        categories = NA_integer_,
                        segments = NA_integer_,
                        rows_analysed = NA_integer_,
                        total_observations = NA_real_,
                        chart_type = NA_character_
                    )
                )
                self$results$summary$setNote("summed_value", NULL)
            }

            private$.clearTableRows(self$results$composition_table)

            for (key in names(private$.detailedStatsLabels())) {
                self$results$detailed_stats$setRow(rowKey = key, values = list(value = NA_character_))
            }

            if (!is.null(self$results$statistical_tests)) {
                private$.clearTableRows(self$results$statistical_tests)
                self$results$statistical_tests$setNote("count_assumption", NULL)
                self$results$statistical_tests$setNote("stdres_note", NULL)
            }

            if (!is.null(self$results$interpretation)) {
                self$results$interpretation$setContent("")
            }
            if (!is.null(self$results$clinical_summary)) {
                self$results$clinical_summary$setContent("")
            }
            if (!is.null(self$results$explanations) && !isTRUE(self$options$showExplanations)) {
                self$results$explanations$setContent("")
            }
        },

        # library-audit 2026-09-16 meddecide [LOW] DONE (same class): detailed_stats has a fixed row set, so .init()
        # scaffolds the rows and .run() fills them with setRow()
        .detailedStatsLabels = function() {
            c(categories    = .("Total Categories"),
              segments      = .("Total Segments"),
              total         = .("Total Summed Value"),
              min_pct       = .("Min Percentage"),
              max_pct       = .("Max Percentage"),
              mean_pct      = .("Mean Percentage"),
              most_variable = .("Most Variable Segment"))
        },

        .init = function() {
            labels <- private$.detailedStatsLabels()
            for (key in names(labels))
                self$results$detailed_stats$addRow(rowKey = key,
                    values = list(measure = unname(labels[[key]])))
            self$results$detailed_stats$setNote(
                "most_variable_def",
                .("Most Variable Segment is ranked by the across-category coefficient of variation (SD / mean of within-category percentages, including 0% cells); reported as N/A when fewer than two non-empty categories are present."),
                init = TRUE
            )

            needs_vars <- is.null(self$options$x_var) ||
                is.null(self$options$y_var) ||
                is.null(self$options$fill_var)
            if (needs_vars) {
                self$results$instructions$setContent(private$.instructionsHtml())
                self$results$instructions$setVisible(TRUE)
            }

            # Set plot size based on user options
            private$.syncPlotSize()

            # Apply clinical presets if selected (visibility is declarative in .r.yaml)
            private$.applyPresetConfiguration()
            if (self$options$analysis_preset != "custom") {
                private$.updatePresetGuidance()
            }
        },

        .run = function() {
            # Sync plot dimensions and preset configuration on every run (jamovi may reuse R6 instance)
            private$.syncPlotSize()
            private$.applyPresetConfiguration()
            if (self$options$analysis_preset != "custom") {
                private$.updatePresetGuidance()
            } else if (!is.null(self$results$preset_guidance)) {
                self$results$preset_guidance$setContent("")
            }

            # Reset all outputs before checking inputs so invalid transitions leave no stale state
            private$.resetResults()

            # Check if required variables are specified
            if (is.null(self$options$x_var) ||
                is.null(self$options$y_var) ||
                is.null(self$options$fill_var)) {
                self$results$instructions$setContent(private$.instructionsHtml())
                self$results$instructions$setVisible(TRUE)
                private$.renderNotices()
                return()
            }

            # Get data
            data <- self$data

            # Extract variable names
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            fill_var <- self$options$fill_var
            facet_var <- self$options$facet_var
            if (is.null(facet_var) || identical(facet_var, "") || length(facet_var) == 0) {
                facet_var <- NULL
            }

            # Validate data
            if (nrow(data) == 0) {
                self$results$instructions$setContent(.("No data available for analysis."))
                self$results$instructions$setVisible(TRUE)
                private$.renderNotices()
                return()
            }

            # Hide onboarding instructions once active data processing begins
            self$results$instructions$setVisible(FALSE)

            # Prepare data
            private$.var_labels <- private$.extractVarLabels(data, c(x_var, y_var, fill_var, facet_var))
            private$.renderVarLabels <- private$.var_labels
            private$.processData(data, x_var, y_var, fill_var, facet_var)

            # Serialize processed data into the plot state so the plot re-renders
            # on option changes and survives reopening a saved .omv (which renders
            # without re-running .run(), leaving private$.processed_data NULL).
            if (!is.null(private$.processed_data)) {
                # The preset and variable labels have to TRAVEL WITH THE STATE too.
                # .displayOpt() / .varLabel() read fields that only .run() fills, so on the
                # export path preset-driven display options and variable labels would revert
                # without carrying them in image$state.
                self$results$plot$setState(list(
                    data = as.data.frame(private$.processed_data),
                    preset = private$.preset_config,
                    var_labels = private$.var_labels))

                # Update summary, composition, detailed stats, and opt-in narrative panels
                private$.updateSummary()
                private$.updateComposition()
                private$.updateDetailedStats()
                if (isTRUE(private$.optionOr("showInterpretation", FALSE))) {
                    private$.createInterpretation()
                }
                if (isTRUE(private$.optionOr("showSummary", FALSE))) {
                    private$.createClinicalSummary()
                }
            }

            # Perform statistical tests if requested.
            #
            # Two gates, because they catch different things. The data check
            # rejects what is demonstrably not a frequency (fractional or negative
            # totals). The user affirmation covers what no check can see: a
            # whole-number MEASUREMENT is indistinguishable from a count in the
            # data, and running chi-square on one manufactures significance
            # (chi2 = 277.5 on random costs in dollars, 27750.5 on the same money
            # in cents). The GUI disables the test until y_is_count is ticked;
            # this branch also covers callers coming in through the R wrapper.
            if (self$options$show_statistical_tests) {
                if (!isTRUE(private$.optionOr("y_is_count", FALSE))) {
                    private$.addNotice("STRONG_WARNING", "Chi-square not run",
                        .fmt(
                            .("The chi-square test needs the summed values to be frequencies. Tick 'Value Variable counts cases' to confirm that '{var}' counts cases, not a measurement such as cost, size or score. The test manufactures significance on a measurement because its value depends on the unit you measured in."),
                            var = self$options$y_var
                        ))
                } else if (!private$.isCountData()) {
                    private$.addNotice("STRONG_WARNING", "Chi-square not run",
                        .fmt(
                            .("'{var}' was declared to be a count, but its category totals are fractional or negative, so they cannot be frequencies. The chi-square test was skipped."),
                            var = self$options$y_var
                        ))
                } else {
                    if (identical(self$options$analysis_preset, "temporal")) {
                        private$.addNotice("STRONG_WARNING", "Repeated-measures caution",
                            .("Temporal template selected with statistical testing enabled: Pearson's chi-square and Cochran-Mantel-Haenszel tests require independent observations across time points. If the same patients are measured at multiple visits, p-values are invalid due to within-subject correlation; use repeated-measures models (e.g., McNemar/Bowker, GEE, or mixed-effects regression)."))
                    }
                    private$.performStatisticalTests(facet_var = facet_var)
                }
            }

            # Generate explanations if requested
            if (self$options$showExplanations) {
                private$.generateExplanations()
            }

            # Add completion info notice
            if (!is.null(private$.processed_data) && nrow(private$.processed_data) > 0) {
                n_total <- sum(private$.metricVec(private$.processed_data, "count"), na.rm = TRUE)
                n_categories <- length(unique(private$.processed_data[[self$options$x_var]]))
                n_segments <- length(unique(private$.processed_data[[self$options$fill_var]]))
                n_rows_val <- if (is.null(private$.n_rows_analysed)) NA_real_ else private$.n_rows_analysed

                private$.addNotice("INFO", "Analysis Info",
                    .fmt(
                        .("Analyzed {rows} rows across {cats} categories and {segs} segments. The bars total {tot} of '{var}' - a summed quantity, which is the sample size only if that variable counts cases."),
                        rows = private$.fmtTotal(n_rows_val),
                        cats = n_categories,
                        segs = n_segments,
                        tot = private$.fmtTotal(n_total),
                        var = self$options$y_var
                    ))
            }

            # Render all collected notices as HTML (errors, warnings, info).
            private$.renderNotices()
        },

        .generateExplanations = function() {
            self$results$explanations$setContent(
                "<div style='color: inherit; line-height: 1.5;'>
                <h3>Explanations</h3>
                <p>
                    <strong>Proportional Composition:</strong> A traditional segmented total bar chart (100% stacked bar) scales each category bar to 100% so segment heights represent within-category proportions, whether the Value Variable holds case counts or non-negative continuous quantities. The Flerlage variant plots raw segment totals side-by-side inside a background box showing each category's overall total.
                </p>
                <p>
                    <strong>Contingency Testing Assumptions:</strong> Pearson's chi-square test of independence (or the Cochran-Mantel-Haenszel test when a Panel Variable stratifies the data) is valid <em>only</em> when the summed Value Variable represents independent case counts (affirmed via <em>Value variable counts cases</em>). Applying chi-square to continuous measurements scales the test statistic with the measurement unit, and applying it to repeated longitudinal observations of the same patients violates independence.
                </p>
                <p>
                    <strong>Post-Hoc Haberman Adjusted Residuals:</strong> For significant tables larger than 2\u00d72, the <em>Standardized Residuals</em> row reports the cell with the largest magnitude Haberman (1973) adjusted standardized residual (<code>chisq.test()$stdres</code>). Unlike raw Pearson components, adjusted residuals account for row and column marginal totals and follow a standard normal <em>N</em>(0, 1) distribution asymptotically under independence. While |<em>z</em>| &gt; 1.96 corresponds to unadjusted two-sided <em>p</em> &lt; 0.05 for a single cell, screening across <em>K</em> \u00d7 <em>S</em> cells warrants a Bonferroni-adjusted threshold (<code>qnorm(1 - alpha / (2 * K * S))</code>).
                </p>
                </div>"
            )
        },

        .prepFactor = function(x, exclude_missing = TRUE) {
            # Decode haven_labelled / labelled value labels while preserving label definition order
            val_labels <- attr(x, "labels", exact = TRUE)
            if (!is.factor(x) && !is.null(val_labels) && length(val_labels) > 0 && !is.null(names(val_labels))) {
                raw_vals <- unclass(x)
                raw_chr <- as.character(raw_vals)
                lbl_map <- stats::setNames(names(val_labels), as.character(unclass(val_labels)))
                mapped <- unname(lbl_map[raw_chr])
                mapped[is.na(mapped)] <- raw_chr[is.na(mapped)]
                mapped[is.na(raw_vals)] <- NA_character_
                obs_non_na <- unique(mapped[!is.na(mapped)])
                ord_lvls <- c(intersect(names(val_labels), obs_non_na), setdiff(obs_non_na, names(val_labels)))
                f <- factor(mapped, levels = ord_lvls)
            } else {
                f <- as.factor(x)
            }
            present_lvls <- levels(droplevels(f))
            if (!exclude_missing && anyNA(f)) {
                missing_lbl <- "(Missing)"
                while (missing_lbl %in% present_lvls) {
                    missing_lbl <- paste0(missing_lbl, "_")
                }
                chr <- as.character(f)
                chr[is.na(chr)] <- missing_lbl
                return(factor(chr, levels = c(present_lvls, missing_lbl)))
            }
            droplevels(f)
        },

        .processData = function(data, x_var, y_var, fill_var, facet_var) {

            # Validate variables exist in data
            required_vars <- c(x_var, y_var, fill_var)
            missing_vars <- setdiff(required_vars, names(data))
            if (length(missing_vars) > 0) {
                private$.addNotice("ERROR", "Missing Variables",
                    .fmt(.("Missing variables in data: {vars}"), vars = paste(missing_vars, collapse = ", ")))
                return()
            }

            if (!is.null(facet_var) && !facet_var %in% names(data)) {
                private$.addNotice("ERROR", "Missing Facet Variable",
                    .fmt(.("Facet variable {var} not found in data"), var = facet_var))
                return()
            }

            # Reject duplicate role assignments before grouping
            role_vars <- c(x_var, y_var, fill_var, facet_var)
            if (anyDuplicated(role_vars) > 0L) {
                private$.addNotice("ERROR", "Duplicate Variables",
                    .("Selected variables must be distinct across Category, Value, Segment, and Panel roles."))
                return()
            }

            # Check if y_var is numeric before subsetting rows
            if (!is.numeric(data[[y_var]])) {
                private$.addNotice("ERROR", "Invalid Variable Type",
                    .fmt(.("{var} must be a numeric variable"), var = y_var))
                return()
            }

            # Check for NA values in categorical grouping variables (including facet_var)
            cat_vars <- unique(c(x_var, fill_var, facet_var))
            for (cv in cat_vars) {
                na_count <- sum(is.na(data[[cv]]))
                if (na_count > 0) {
                    if (isTRUE(self$options$exclude_missing)) {
                        private$.addNotice("WARNING", "Missing Data",
                            .fmt(.("Found {n} NA values in {var} - these will be removed"), n = na_count, var = cv))
                        data <- data[!is.na(data[[cv]]), , drop = FALSE]
                    } else {
                        private$.addNotice("INFO", "Missing Data Present",
                            .fmt(.("Found {n} NA values in {var} - these will be included as '(Missing)'"), n = na_count, var = cv))
                    }
                }
            }

            if (!nrow(data)) {
                private$.addNotice("ERROR", "No Valid Data",
                    .("No valid data after removing missing categorical values."))
                return()
            }

            # Convert variables to factors if needed.
            #
            # NOTE: a "numeric variable used as a category" warning cannot live
            # here. Both slots are `permitted: [factor]`, so the value is already
            # a factor by the time .processData runs - jamovi refuses a continuous
            # variable in the GUI outright, and the R wrapper coerces it upstream.
            # Verified 2026-08-13: is.numeric() on either column is FALSE for
            # every path a user can actually take. The many-levels consequence is
            # still caught downstream by the "Too Many Categories" notice.
            invalid_values <- !is.finite(data[[y_var]])
            if (any(invalid_values)) {
                private$.addNotice("WARNING", "Missing or non-finite values excluded",
                    .fmt(.("{n} rows lack a finite Value Variable and were excluded; missing values are not zero counts."), n = sum(invalid_values)))
                data <- data[!invalid_values, , drop = FALSE]
            }
            if (!nrow(data)) {
                private$.addNotice("ERROR", "No Valid Data",
                    .("No valid data after excluding missing or non-finite values."))
                return()
            }
            if (any(data[[y_var]] < 0)) {
                private$.addNotice("ERROR", "Negative values",
                    .("Composition charts require non-negative values on every row."))
                return()
            }
            if (isTRUE(private$.optionOr("y_is_count", FALSE)) &&
                any(abs(data[[y_var]] - round(data[[y_var]])) > 1e-9)) {
                private$.addNotice("ERROR", "Fractional case counts",
                    .("Case frequencies must be whole numbers on every row; fractional values cannot be made valid by summing them."))
                return()
            }

            excl_na <- isTRUE(self$options$exclude_missing)
            data[[x_var]] <- private$.prepFactor(data[[x_var]], excl_na)
            data[[fill_var]] <- private$.prepFactor(data[[fill_var]], excl_na)
            if (!is.null(facet_var)) {
                data[[facet_var]] <- private$.prepFactor(data[[facet_var]], excl_na)
            }

            # Check for sufficient levels
            if (length(levels(data[[x_var]])) < 1) {
                private$.addNotice("ERROR", "Empty Variable",
                    .fmt(.("{var} must have at least one category"), var = x_var))
                return()
            }

            if (length(levels(data[[fill_var]])) < 1) {
                private$.addNotice("ERROR", "Empty Variable",
                    .fmt(.("{var} must have at least one segment"), var = fill_var))
                return()
            }

            if (!is.null(facet_var) && length(levels(data[[facet_var]])) < 1) {
                private$.addNotice("ERROR", "Empty Variable",
                    .fmt(.("{var} must have at least one panel level"), var = facet_var))
                return()
            }

            private$.n_rows_analysed <- nrow(data)

            # Aggregate on collision-proof internal columns (..x, ..fill[, ..facet], ..y)
            # so user columns named 'count', 'value', 'percentage', 'total', etc., or
            # containing spaces/operators/Unicode never collide with summary column names.
            if (!is.null(facet_var)) {
                work_df <- data.frame(
                    ..x = data[[x_var]],
                    ..fill = data[[fill_var]],
                    ..facet = data[[facet_var]],
                    ..y = as.numeric(data[[y_var]]),
                    stringsAsFactors = FALSE
                )
                agg <- work_df %>%
                    dplyr::group_by(.data$..x, .data$..fill, .data$..facet, .drop = FALSE) %>%
                    dplyr::summarise(
                        ..value = sum(.data$..y, na.rm = TRUE),
                        ..count = sum(.data$..y, na.rm = TRUE),
                        .n_obs = dplyr::n(),
                        .groups = "drop"
                    )
            } else {
                work_df <- data.frame(
                    ..x = data[[x_var]],
                    ..fill = data[[fill_var]],
                    ..y = as.numeric(data[[y_var]]),
                    stringsAsFactors = FALSE
                )
                agg <- work_df %>%
                    dplyr::group_by(.data$..x, .data$..fill, .drop = FALSE) %>%
                    dplyr::summarise(
                        ..value = sum(.data$..y, na.rm = TRUE),
                        ..count = sum(.data$..y, na.rm = TRUE),
                        .n_obs = dplyr::n(),
                        .groups = "drop"
                    )
            }

            unobserved_cells <- sum(agg$.n_obs == 0L)
            agg$.n_obs <- NULL

            # Guard against negative summed values.
            if (any(agg$..value < 0, na.rm = TRUE)) {
                private$.addNotice("ERROR", "Negative Values",
                    .("Value Variable produced negative category totals. Segmented total bar charts require non-negative values because each bar represents a 100% composition. Please check your data."))
                return()
            }

            # Calculate percentages within each category, guarding zero-total bars against 0/0 NaN
            grand_total <- sum(agg$..value, na.rm = TRUE)
            if (!is.null(facet_var)) {
                agg <- agg %>%
                    dplyr::group_by(.data$..x, .data$..facet) %>%
                    dplyr::mutate(
                        ..total_in_category = sum(.data$..value, na.rm = TRUE),
                        ..percentage = dplyr::if_else(.data$..total_in_category > 0, .data$..value / .data$..total_in_category * 100, 0)
                    ) %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(
                        ..overall_percentage = if (grand_total > 0) .data$..value / grand_total * 100 else 0
                    )
            } else {
                agg <- agg %>%
                    dplyr::group_by(.data$..x) %>%
                    dplyr::mutate(
                        ..total_in_category = sum(.data$..value, na.rm = TRUE),
                        ..percentage = dplyr::if_else(.data$..total_in_category > 0, .data$..value / .data$..total_in_category * 100, 0)
                    ) %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(
                        ..overall_percentage = if (grand_total > 0) .data$..value / grand_total * 100 else 0
                    )
            }

            # Check for empty groups
            if (nrow(agg) == 0) {
                private$.addNotice("ERROR", "No Valid Data",
                    .("No valid data after processing. Please check your input variables."))
                return()
            }

            # Assemble processed_data with user variable names plus internal ..* metrics
            # (and standard value/count/percentage aliases whenever non-colliding).
            processed_data <- data.frame(..row_id = seq_len(nrow(agg)))
            processed_data[[x_var]] <- agg$..x
            processed_data[[fill_var]] <- agg$..fill
            if (!is.null(facet_var)) {
                processed_data[[facet_var]] <- agg$..facet
            }
            processed_data$..row_id <- NULL
            processed_data$..value <- agg$..value
            processed_data$..count <- agg$..count
            processed_data$..percentage <- agg$..percentage
            processed_data$..overall_percentage <- agg$..overall_percentage
            processed_data$..total_in_category <- agg$..total_in_category

            user_cols <- c(x_var, fill_var, facet_var)
            for (alias in c("value", "count", "percentage", "overall_percentage", "total_in_category")) {
                if (!alias %in% user_cols) {
                    processed_data[[alias]] <- processed_data[[paste0("..", alias)]]
                }
            }

            # Check for zero totals
            if (!is.null(facet_var)) {
                zero_totals <- processed_data %>%
                    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(facet_var)) %>%
                    dplyr::summarise(..tot = sum(.data$..value, na.rm = TRUE), .groups = "drop") %>%
                    dplyr::filter(.data$..tot == 0)
                if (nrow(zero_totals) > 0) {
                    zero_labels <- paste0(as.character(zero_totals[[x_var]]), " (", as.character(zero_totals[[facet_var]]), ")")
                    private$.addNotice("WARNING", "Zero Total Categories",
                        .fmt(
                            .("Category-panel bars with zero totals found: {bars} - these may appear empty in the plot"),
                            bars = paste(zero_labels, collapse = ", ")
                        ))
                }
            } else {
                zero_totals <- processed_data %>%
                    dplyr::group_by(!!rlang::sym(x_var)) %>%
                    dplyr::summarise(..tot = sum(.data$..value, na.rm = TRUE), .groups = "drop") %>%
                    dplyr::filter(.data$..tot == 0)
                if (nrow(zero_totals) > 0) {
                    private$.addNotice("WARNING", "Zero Total Categories",
                        .fmt(
                            .("Categories with zero totals found: {cats} - these may appear empty in the plot"),
                            cats = paste(zero_totals[[x_var]], collapse = ", ")
                        ))
                }
            }

            # Apply sorting if requested
            if (self$options$sort_categories != "none") {
                processed_data <- private$.applySorting(processed_data, x_var, fill_var, facet_var)
            }

            # Perform clinical data validation
            private$.validateClinicalData(processed_data, x_var, fill_var, facet_var, unobserved_cells)

            private$.processed_data <- processed_data

            # Create composition data for table without risking column-name collisions
            cat_chr <- as.character(processed_data[[x_var]])
            if (!is.null(facet_var)) {
                cat_chr <- paste0(cat_chr, " [", as.character(processed_data[[facet_var]]), "]")
            }
            composition_data <- data.frame(
                category = cat_chr,
                segment = as.character(processed_data[[fill_var]]),
                count = processed_data$..count,
                percentage = processed_data$..percentage,
                overall_percentage = processed_data$..overall_percentage,
                total_in_category = processed_data$..total_in_category,
                stringsAsFactors = FALSE
            )
            private$.composition_data <- composition_data
        },

        .applySorting = function(data, x_var, fill_var, facet_var) {

            sort_type <- self$options$sort_categories

            if (sort_type == "total") {
                category_totals <- data %>%
                    dplyr::group_by(!!rlang::sym(x_var)) %>%
                    dplyr::summarise(..tot = sum(.data$..value, na.rm = TRUE), .groups = "drop") %>%
                    dplyr::arrange(dplyr::desc(.data$..tot))

                unique_levels <- unique(as.character(category_totals[[x_var]]))
                data[[x_var]] <- factor(data[[x_var]], levels = unique_levels)

            } else if (sort_type == "largest_segment") {
                # Sort categories by largest within-category segment percentage (pooled across panels if faceted)
                largest_segments <- data %>%
                    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(fill_var)) %>%
                    dplyr::summarise(..seg_val = sum(.data$..value, na.rm = TRUE), .groups = "drop") %>%
                    dplyr::group_by(!!rlang::sym(x_var)) %>%
                    dplyr::mutate(
                        ..cat_tot = sum(.data$..seg_val, na.rm = TRUE),
                        ..seg_pct = dplyr::if_else(.data$..cat_tot > 0, .data$..seg_val / .data$..cat_tot * 100, 0)
                    ) %>%
                    dplyr::summarise(..max_seg_pct = max(.data$..seg_pct, na.rm = TRUE), .groups = "drop") %>%
                    dplyr::arrange(dplyr::desc(.data$..max_seg_pct))

                unique_levels <- unique(as.character(largest_segments[[x_var]]))
                data[[x_var]] <- factor(data[[x_var]], levels = unique_levels)

            } else if (sort_type == "alpha") {
                current_levels <- unique(as.character(data[[x_var]]))
                sorted_levels <- sort(current_levels)
                data[[x_var]] <- factor(data[[x_var]], levels = sorted_levels)
            }

            # Reorder rows so composition_table reflects the selected category sort order
            if (!is.null(facet_var)) {
                data <- data %>%
                    dplyr::arrange(!!rlang::sym(x_var), !!rlang::sym(fill_var), !!rlang::sym(facet_var))
            } else {
                data <- data %>%
                    dplyr::arrange(!!rlang::sym(x_var), !!rlang::sym(fill_var))
            }

            return(data)
        },

        .validateClinicalData = function(data, x_var, fill_var, facet_var = NULL, unobserved_cells = 0L) {

            # Check minimum sample sizes for statistical reliability
            if (!is.null(facet_var)) {
                category_counts <- data %>%
                    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(facet_var)) %>%
                    dplyr::summarise(..tot_cnt = sum(.data$..count, na.rm = TRUE), .groups = "drop")
                small_categories <- category_counts %>%
                    dplyr::filter(.data$..tot_cnt < 5)
                if (nrow(small_categories) > 0) {
                    small_labels <- paste0(as.character(small_categories[[x_var]]), " (", as.character(small_categories[[facet_var]]), ")")
                    private$.addNotice("STRONG_WARNING", "Small Sample Size",
                        .fmt(
                            .("Some categories have fewer than 5 observations. Results may be unreliable for small groups: {groups}"),
                            groups = paste(small_labels, collapse = ", ")
                        ))
                }
            } else {
                category_counts <- data %>%
                    dplyr::group_by(!!rlang::sym(x_var)) %>%
                    dplyr::summarise(..tot_cnt = sum(.data$..count, na.rm = TRUE), .groups = "drop")
                small_categories <- category_counts %>%
                    dplyr::filter(.data$..tot_cnt < 5)
                if (nrow(small_categories) > 0) {
                    private$.addNotice("STRONG_WARNING", "Small Sample Size",
                        .fmt(
                            .("Some categories have fewer than 5 observations. Results may be unreliable for small groups: {groups}"),
                            groups = paste(small_categories[[x_var]], collapse = ", ")
                        ))
                }
            }

            # Check for extreme proportions that may indicate data quality issues
            extreme_segments <- data %>%
                dplyr::filter(.data$..total_in_category > 0 & .data$..percentage > 95)

            if (nrow(extreme_segments) > 0) {
                private$.addNotice("STRONG_WARNING", "Extreme Proportions",
                    .("Some segments represent >95% of their category. Consider combining rare categories or checking data quality."))
            }

            # Check for very small non-zero segments
            tiny_segments <- data %>%
                dplyr::filter(.data$..count > 0 & .data$..percentage < 1 & .data$..count < 3)

            if (nrow(tiny_segments) > 0) {
                private$.addNotice("WARNING", "Tiny Segments",
                    .("Some segments represent <1% with very small counts. Rare events can still be clinically important; report their counts and interpret precision cautiously."))
            }

            # Check for reasonable number of categories for visualization
            n_categories <- length(unique(data[[x_var]]))
            if (n_categories > 10) {
                private$.addNotice("WARNING", "Too Many Categories",
                    .fmt(
                        .("Large number of categories ({n}) may make the chart difficult to interpret. Consider grouping similar categories."),
                        n = n_categories
                    ))
            }

            # Check for reasonable number of segments
            n_segments <- length(unique(data[[fill_var]]))
            if (n_segments > 8) {
                private$.addNotice("WARNING", "Too Many Segments",
                    .fmt(
                        .("Large number of segments ({n}) may make colors difficult to distinguish. Consider grouping similar segments."),
                        n = n_segments
                    ))
            }

            # Check for missing combinations (unobserved cells in the category x segment [x facet] grid)
            if (unobserved_cells > 0L) {
                private$.addNotice("WARNING", "Missing Data Combinations",
                    .fmt(
                        .("Missing data combinations detected: {n} empty category-segment pairs. This may indicate sparse data."),
                        n = unobserved_cells
                    ))
            }
        },

        # Centralized color palette definitions
        .getColorPalette = function(palette_name, n_colors) {

            palettes <- list(
                clinical = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E", "#7209B7", "#F72585", "#4361EE"),
                colorblind = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"),
                bbc_multi = c("#1380A1", "#FAAB18", "#990000", "#588300", "#990099", "#FF6600", "#809900", "#bcc4c1"),
                prism_colorblind_safe = c("#0000FF", "#FF0000", "#00BF00", "#FF00FF", "#FFFF00", "#00FFFF", "#800080", "#FFA500"),
                nature = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000"),
                science = c("#0C5BB0", "#FFA042", "#15983D", "#EC0000", "#8b7355", "#149EF3", "#FA6B09", "#A149FA")
            )

            if (palette_name %in% names(palettes)) {
                colors <- palettes[[palette_name]]
                if (n_colors <= length(colors)) {
                    return(colors[seq_len(n_colors)])
                }
                return(grDevices::colorRampPalette(colors)(n_colors))
            }

            return(NULL)
        },

        .applyColorPalette = function(p, data, fill_var, theme = NULL) {

            palette <- private$.displayOpt("color_palette")
            n_colors <- max(1L, length(unique(data[[fill_var]])))

            if (identical(palette, "jamovi")) {
                pal_name <- if (!is.null(theme) && !is.null(theme$palette) && nzchar(theme$palette)) theme$palette else "jmv"
                p <- p + ggplot2::scale_fill_manual(
                    values = jmvcore::colorPalette(n = n_colors, pal = pal_name, type = "fill")
                )
            } else if (identical(palette, "default")) {
                # Retain ggplot2's default hue discrete scale
                return(p)
            } else if (palette == "viridis") {
                p <- p + ggplot2::scale_fill_viridis_d()
            } else if (palette == "set1") {
                p <- p + ggplot2::scale_fill_brewer(type = "qual", palette = "Set1")
            } else if (palette == "dark2") {
                p <- p + ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2")
            } else if (palette == "paired") {
                p <- p + ggplot2::scale_fill_brewer(type = "qual", palette = "Paired")
            } else {
                custom_colors <- private$.getColorPalette(palette, n_colors)
                if (!is.null(custom_colors)) {
                    p <- p + ggplot2::scale_fill_manual(values = custom_colors)
                } else {
                    p <- p + ggplot2::scale_fill_viridis_d()
                }
            }

            return(p)
        },

        .applyTheme = function(p, ggtheme = NULL) {
            # Delegate to the single theme factory (.getChartTheme) to avoid
            # duplicated theme definitions.
            return(p + private$.getChartTheme(private$.displayOpt("chart_style"), ggtheme = ggtheme))
        },

        .getChartTheme = function(style, ggtheme = NULL) {
            # Returns a ggplot2 theme object based on chart style

            if (identical(style, "jamovi")) {
                if (!is.null(ggtheme)) {
                    return(ggtheme)
                }
                return(ggplot2::theme_minimal())
            } else if (style == "clean") {
                return(ggplot2::theme_minimal() +
                    ggplot2::theme(
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        text = ggplot2::element_text(size = 11),
                        axis.title = ggplot2::element_text(size = 12, face = "bold"),
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
                    ))
            } else if (style == "publication") {
                return(ggplot2::theme_classic() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 11, face = "bold"),
                        plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                        legend.text = ggplot2::element_text(size = 9),
                        axis.line = ggplot2::element_line(color = "black", linewidth = 0.5)
                    ))
            } else if (style == "presentation") {
                return(ggplot2::theme_minimal() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 14),
                        axis.title = ggplot2::element_text(size = 16, face = "bold"),
                        plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
                        legend.text = ggplot2::element_text(size = 12),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank()
                    ))
            } else if (style == "clinical") {
                return(ggplot2::theme_bw() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 11),
                        axis.title = ggplot2::element_text(size = 12, face = "bold"),
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        legend.position = "right",
                        legend.background = ggplot2::element_rect(fill = "white", color = "gray80"),
                        panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1)
                    ))
            } else if (style == "bbc_style") {
                return(ggplot2::theme_minimal() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 11),
                        plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0),
                        plot.subtitle = ggplot2::element_text(size = 14, hjust = 0),
                        axis.title = ggplot2::element_blank(),
                        axis.text = ggplot2::element_text(size = 10),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.background = ggplot2::element_blank(),
                        legend.position = "top",
                        legend.justification = "left",
                        legend.text = ggplot2::element_text(size = 10),
                        legend.title = ggplot2::element_blank()
                    ))
            } else if (style == "prism_style") {
                return(ggplot2::theme_classic() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 11),
                        plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                        axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
                        axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.5),
                        legend.text = ggplot2::element_text(size = 9),
                        legend.key.size = ggplot2::unit(0.8, "cm"),
                        panel.background = ggplot2::element_rect(fill = "white")
                    ))
            }

            # Default to minimal theme if unknown style
            return(ggplot2::theme_minimal())
        },

        .updateSummary = function() {

            data <- private$.processed_data

            if (is.null(data)) {
                return()
            }

            n_categories <- length(unique(data[[self$options$x_var]]))
            n_segments <- length(unique(data[[self$options$fill_var]]))
            total_obs <- sum(private$.metricVec(data, "count"), na.rm = TRUE)
            n_rows_int <- if (is.null(private$.n_rows_analysed)) NA_integer_ else as.integer(private$.n_rows_analysed)
            chart_label <- if (identical(self$options$plot_type, "flerlage")) {
                .("Flerlage Segmented Total Bar")
            } else {
                .("Segmented Total Bar (100% Stacked)")
            }

            # The column formerly titled "Total Observations" held sum(y_var), which
            # is a summed quantity - 26678 "observations" for 90 patients when y was
            # a cost. Report the row count separately and title the sum honestly.
            summary_row <- list(
                categories = n_categories,
                segments = n_segments,
                rows_analysed = n_rows_int,
                total_observations = total_obs,
                chart_type = chart_label
            )

            self$results$summary$setRow(rowNo = 1, values = summary_row)
            self$results$summary$setNote(
                "summed_value",
                .fmt(
                    .("Summed Value is the total of '{var}' across all bars. It equals the number of cases only when that variable counts cases."),
                    var = private$.safeLabel(self$options$y_var, note = TRUE)
                ),
                init = FALSE
            )
        },

        .updateComposition = function() {

            data <- private$.composition_data

            if (is.null(data)) {
                return()
            }

            for (i in seq_len(nrow(data))) {
                row <- list(
                    category = private$.safeLabel(data$category[i], note = FALSE),
                    segment = private$.safeLabel(data$segment[i], note = FALSE),
                    count = data$count[i],
                    percentage = data$percentage[i] / 100,  # Convert to proportion for percentage format
                    overall_percentage = data$overall_percentage[i] / 100,
                    total_in_category = data$total_in_category[i]
                )

                self$results$composition_table$addRow(rowKey = i, values = row)
            }
        },

        .createInterpretation = function() {

            data <- private$.processed_data
            composition_data <- private$.composition_data

            if (is.null(data) || is.null(composition_data)) {
                return()
            }

            n_categories <- length(unique(data[[self$options$x_var]]))
            n_segments <- length(unique(data[[self$options$fill_var]]))

            active_comp <- composition_data[composition_data$total_in_category > 0, , drop = FALSE]
            if (nrow(active_comp) == 0) {
                active_comp <- composition_data
            }

            n_rows_val <- if (is.null(private$.n_rows_analysed)) NA_real_ else private$.n_rows_analysed
            n_rows_str <- private$.fmtTotal(n_rows_val)
            total_val_str <- private$.fmtTotal(sum(composition_data$count, na.rm = TRUE))
            y_var_safe <- htmltools::htmlEscape(self$options$y_var)

            total_li <- if (isTRUE(private$.optionOr("y_is_count", FALSE)) && private$.isCountData()) {
                paste0("<li>Total cases (", y_var_safe, "): ", total_val_str, "</li>")
            } else {
                paste0("<li>Summed value (", y_var_safe, "): ", total_val_str, "</li>")
            }

            max_p <- max(active_comp$percentage, na.rm = TRUE)
            min_p <- min(active_comp$percentage, na.rm = TRUE)
            if (!is.finite(max_p) || abs(max_p - min_p) < 1e-9) {
                largest_li <- paste0("<li><strong>Segment distribution:</strong> Uniform across all segments (",
                                     round(max_p, 1), "% each)</li>")
            } else {
                top_rows <- active_comp[abs(active_comp$percentage - max_p) < 1e-9, , drop = FALSE]
                if (nrow(top_rows) == 1L) {
                    largest_li <- paste0(
                        "<li><strong>Largest segment:</strong> ",
                        htmltools::htmlEscape(as.character(top_rows$segment[1])),
                        " in ", htmltools::htmlEscape(as.character(top_rows$category[1])),
                        " (", round(max_p, 1), "%)</li>"
                    )
                } else {
                    pairs_str <- paste(
                        paste0(htmltools::htmlEscape(as.character(top_rows$segment)),
                               " in ", htmltools::htmlEscape(as.character(top_rows$category))),
                        collapse = "; "
                    )
                    largest_li <- paste0("<li><strong>Largest segments (tied):</strong> ",
                                         pairs_str, " (", round(max_p, 1), "%)</li>")
                }
            }

            cat_bal <- active_comp %>%
                dplyr::group_by(.data$category) %>%
                dplyr::summarise(
                    cv = dplyr::if_else(
                        dplyr::n() >= 2L & mean(.data$percentage, na.rm = TRUE) > 0,
                        stats::sd(.data$percentage, na.rm = TRUE) / mean(.data$percentage, na.rm = TRUE),
                        0
                    ),
                    .groups = "drop"
                )
            min_cv <- min(cat_bal$cv, na.rm = TRUE)
            bal_cats <- cat_bal$category[is.finite(cat_bal$cv) & abs(cat_bal$cv - min_cv) < 1e-9]
            if (length(bal_cats) == nrow(cat_bal) && nrow(cat_bal) > 1L) {
                balanced_li <- "<li><strong>Category balance:</strong> All categories show equally balanced distributions across segments</li>"
            } else {
                bal_names <- paste(htmltools::htmlEscape(as.character(bal_cats)), collapse = ", ")
                balanced_li <- paste0("<li><strong>Most balanced category:</strong> ", bal_names,
                                      " shows the most even distribution across segments</li>")
            }

            interpretation_html <- paste(
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 6px; margin: 10px 0; color: inherit;'>",
                "<h4 style='color: inherit; margin-top: 0;'>Chart Interpretation</h4>",
                "<p><strong>Data Overview:</strong></p>",
                "<ul>",
                paste0("<li>", n_categories, " categories with ", n_segments, " segments each</li>"),
                paste0("<li>Rows analyzed: ", n_rows_str, "</li>"),
                total_li,
                "</ul>",
                "<p><strong>Key Findings:</strong></p>",
                "<ul>",
                largest_li,
                balanced_li,
                "</ul>",
                "</div>"
            )

            self$results$interpretation$setContent(interpretation_html)
        },

        .updateDetailedStats = function() {

            data <- private$.processed_data
            composition_data <- private$.composition_data

            if (is.null(data) || is.null(composition_data)) {
                return()
            }

            # Calculate detailed statistics
            n_categories <- length(unique(data[[self$options$x_var]]))
            n_segments <- length(unique(data[[self$options$fill_var]]))
            total_obs <- sum(private$.metricVec(data, "count"), na.rm = TRUE)

            active_comp <- composition_data[composition_data$total_in_category > 0, , drop = FALSE]
            if (nrow(active_comp) == 0) {
                active_comp <- composition_data
            }

            # Calculate min, max, and mean percentages including 0% cells in non-empty bars
            min_pct <- round(min(active_comp$percentage, na.rm = TRUE), 1)
            max_pct <- round(max(active_comp$percentage, na.rm = TRUE), 1)
            mean_pct <- round(mean(active_comp$percentage, na.rm = TRUE), 1)

            # Calculate segment with highest cross-category variation (CV)
            n_active_cats <- length(unique(active_comp$category))
            if (n_active_cats < 2L) {
                most_var_str <- "N/A"
            } else {
                seg_cv <- active_comp %>%
                    dplyr::group_by(.data$segment) %>%
                    dplyr::summarise(
                        cv = dplyr::if_else(
                            mean(.data$percentage, na.rm = TRUE) > 0,
                            stats::sd(.data$percentage, na.rm = TRUE) / mean(.data$percentage, na.rm = TRUE),
                            0
                        ),
                        .groups = "drop"
                    )
                max_cv <- max(seg_cv$cv, na.rm = TRUE)
                if (!is.finite(max_cv) || max_cv < 1e-9) {
                    most_var_str <- .("None (uniform across categories)")
                } else {
                    top_segs <- seg_cv$segment[abs(seg_cv$cv - max_cv) < 1e-9]
                    most_var_str <- paste(private$.safeLabel(top_segs, note = FALSE), collapse = ", ")
                }
            }

            # Fill the rows .init() scaffolded (keys and labels: .detailedStatsLabels())
            stats_values <- list(
                categories    = as.character(n_categories),
                segments      = as.character(n_segments),
                total         = private$.fmtTotal(total_obs),
                min_pct       = paste0(min_pct, "%"),
                max_pct       = paste0(max_pct, "%"),
                mean_pct      = paste0(mean_pct, "%"),
                most_variable = most_var_str
            )

            for (key in names(stats_values)) {
                self$results$detailed_stats$setRow(rowKey = key, values = list(value = stats_values[[key]]))
            }
        },

        .plot = function(image, ggtheme, theme, ...) {

            # Plot builder supporting both traditional stacked bars and Flerlage-style plots

            # Check if this plot should be shown
            if (!self$options$show_plot) {
                return(FALSE)
            }

            # Validate required options
            if (is.null(self$options$x_var) ||
                is.null(self$options$y_var) ||
                is.null(self$options$fill_var)) {
                return(FALSE)
            }

            # State first, and state only: it is the one source that exists on the export
            # path, where .run() never ran and every private$ field holds its initial value.
            # Older .omv files stored a bare data.frame here, so accept both shapes.
            st <- image$state
            if (is.null(st)) {
                return(FALSE)
            }
            df <- if (is.data.frame(st)) st else if (is.list(st)) st$data else NULL
            if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
                return(FALSE)
            }
            # hand the renderer's preset and variable labels to .displayOpt() / .varLabel()
            prev_preset <- private$.renderPreset
            prev_var_labels <- private$.renderVarLabels
            if (is.list(st) && !is.data.frame(st)) {
                if ("preset" %in% names(st)) private$.renderPreset <- st$preset
                if ("var_labels" %in% names(st)) private$.renderVarLabels <- st$var_labels
            }
            on.exit({
                private$.renderPreset <- prev_preset
                private$.renderVarLabels <- prev_var_labels
            }, add = TRUE)

            # Route to appropriate plot type
            if (self$options$plot_type == "flerlage") {
                p <- private$.plotFlerlage(df, image, ggtheme, theme)
            } else {
                p <- private$.plotTraditional(df, image, ggtheme, theme)
            }

            # Print plot
            print(p)
            TRUE
        },

        .plotTraditional = function(df, image, ggtheme, theme, ...) {
            # Traditional 100% stacked bar chart implementation

            x_var <- self$options$x_var
            fill_var <- self$options$fill_var
            facet_col <- self$options$facet_var
            if (is.null(facet_col) || !nzchar(facet_col) || !facet_col %in% names(df)) {
                facet_col <- NULL
            }
            val_col <- if ("..value" %in% names(df)) "..value" else "value"

            # Ensure canonical factor-level row order within each bar so bar_layer,
            # label_data, and outline_layer share identical stacking order.
            df[[x_var]] <- as.factor(df[[x_var]])
            df[[fill_var]] <- as.factor(df[[fill_var]])
            if (!is.null(facet_col)) {
                df[[facet_col]] <- as.factor(df[[facet_col]])
                df <- df[order(as.integer(df[[facet_col]]), as.integer(df[[x_var]]), as.integer(df[[fill_var]])), , drop = FALSE]
            } else {
                df <- df[order(as.integer(df[[x_var]]), as.integer(df[[fill_var]])), , drop = FALSE]
            }

            # Basic stacked-to-100% bar chart with percent y-axis labels
            p <- ggplot2::ggplot(
                df,
                ggplot2::aes(
                    x = !!rlang::sym(x_var),
                    y = !!rlang::sym(val_col),
                    fill = !!rlang::sym(fill_var)
                )
            ) +
                ggplot2::geom_col(position = "fill", width = self$options$bar_width) +
                ggplot2::scale_y_continuous(labels = scales::percent)

            # Orientation
            if (identical(self$options$orientation, "horizontal"))
                p <- p + ggplot2::coord_flip()

            # Faceting
            if (!is.null(facet_col)) {
                p <- p + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(facet_col)), scales = "free_x")
            }

            # Percentage and/or count labels computed on exact pre-filter stacked coordinates
            want_pct <- isTRUE(private$.displayOpt("show_percentages"))
            want_cnt <- isTRUE(self$options$show_counts)
            if (want_pct || want_cnt) {
                if (!is.null(facet_col)) {
                    label_data <- df %>%
                        dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(facet_col))
                } else {
                    label_data <- df %>%
                        dplyr::group_by(!!rlang::sym(x_var))
                }

                label_data <- label_data %>%
                    dplyr::mutate(
                        ..bar_sum = sum(!!rlang::sym(val_col), na.rm = TRUE),
                        ..prop = dplyr::if_else(.data$..bar_sum > 0, (!!rlang::sym(val_col)) / .data$..bar_sum, 0),
                        ..ymax = rev(cumsum(rev(.data$..prop))),
                        ..ymin = .data$..ymax - .data$..prop,
                        ..y_mid = (.data$..ymin + .data$..ymax) / 2,
                        ..pct = .data$..prop * 100
                    ) %>%
                    dplyr::ungroup() %>%
                    dplyr::filter(
                        .data$..bar_sum > 0 &
                        .data$..pct > 0 &
                        .data$..pct >= self$options$label_threshold
                    )

                if (nrow(label_data) > 0) {
                    fmt <- switch(
                        private$.displayOpt("percentage_format"),
                        "decimal1" = function(z) paste0(base::format(round(z, 1), nsmall = 1), "%"),
                        "decimal2" = function(z) paste0(base::format(round(z, 2), nsmall = 2), "%"),
                        function(z) paste0(round(z), "%")
                    )
                    raw_vals <- label_data[[val_col]]
                    cnt_str <- vapply(raw_vals, private$.fmtTotal, character(1))
                    cnt_tag <- if (isTRUE(private$.optionOr("y_is_count", FALSE))) {
                        paste0("n=", cnt_str)
                    } else {
                        cnt_str
                    }

                    if (want_pct && want_cnt) {
                        label_data$..lab <- paste0(fmt(label_data$..pct), "\n(", cnt_tag, ")")
                    } else if (want_pct) {
                        label_data$..lab <- fmt(label_data$..pct)
                    } else {
                        label_data$..lab <- cnt_tag
                    }

                    p <- p + ggplot2::geom_text(
                        data = label_data,
                        ggplot2::aes(
                            x = !!rlang::sym(x_var),
                            y = .data$..y_mid,
                            label = .data$..lab
                        ),
                        size = 3, color = "white", fontface = "bold",
                        inherit.aes = FALSE
                    )
                }
            }

            # Segment outlines (grouped by fill_var so borders match every segment rectangle)
            if (isTRUE(self$options$add_outline) && !identical(self$options$outline_color, "none")) {
                outline_col <- switch(
                    self$options$outline_color,
                    black = "black",
                    gray = "gray60",
                    white = "white",
                    "white"
                )
                p <- p + ggplot2::geom_col(
                    ggplot2::aes(group = !!rlang::sym(fill_var)),
                    position = "fill",
                    width = self$options$bar_width,
                    color = outline_col,
                    fill = NA,
                    linewidth = 0.2
                )
            }

            # Titles and legend (falling back to variable labels when custom titles are blank)
            plot_title <- if (nzchar(self$options$plot_title)) self$options$plot_title else .("Segmented Total Bar Chart")
            x_title <- if (nzchar(self$options$x_title)) self$options$x_title else private$.varLabel(x_var)
            y_title <- if (nzchar(self$options$y_title)) self$options$y_title else .("Percentage")
            legend_title <- if (nzchar(self$options$legend_title)) self$options$legend_title else private$.varLabel(fill_var)

            p <- p + ggplot2::labs(title = plot_title, x = x_title, y = y_title, fill = legend_title)

            # Apply color palette before chart style so ggtheme never overrides scales
            p <- private$.applyColorPalette(p, df, fill_var, theme = theme)

            # Apply chart style
            p <- private$.applyTheme(p, ggtheme = ggtheme)

            # Restore explicit user titles if bbc_style blanked them out
            if (identical(private$.displayOpt("chart_style"), "bbc_style")) {
                if (nzchar(self$options$x_title)) {
                    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_text(size = 11, face = "bold"))
                }
                if (nzchar(self$options$y_title) && !identical(self$options$y_title, "Percentage")) {
                    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_text(size = 11, face = "bold"))
                }
                if (nzchar(self$options$legend_title)) {
                    p <- p + ggplot2::theme(legend.title = ggplot2::element_text(size = 10, face = "bold"))
                }
            }

            # Ensure title is centered and honor user legend_position unconditionally
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(hjust = 0.5),
                legend.position = self$options$legend_position
            )

            # Export-friendly tweaks
            if (isTRUE(self$options$export_ready)) {
                p <- p + ggplot2::theme(
                    plot.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    legend.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            }

            return(p)
        },

        .plotFlerlage = function(df, image, ggtheme, theme, ...) {
            # Flerlage-style segmented total bar chart using ggsegmentedtotalbar
            # (with native ggplot2 Flerlage geometry when faceting, flipping, custom sorting,
            # fractional values, or small totals would break ggsegmentedtotalbar's ylim/fct_reorder)

            x_var <- self$options$x_var
            fill_var <- self$options$fill_var
            facet_col <- self$options$facet_var
            if (is.null(facet_col) || !nzchar(facet_col) || !facet_col %in% names(df)) {
                facet_col <- NULL
            }
            val_col <- if ("..value" %in% names(df)) "..value" else "value"

            # Reuse category ordering already computed by .applySorting() in .processData();
            # only recompute if un-factored data enters .plotFlerlage() directly.
            if (self$options$sort_categories != "none" && !is.factor(df[[x_var]])) {
                df <- private$.applySorting(df, x_var, fill_var, facet_col)
            }

            if (!is.null(facet_col)) {
                plot_data <- df %>%
                    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(facet_col)) %>%
                    dplyr::mutate(..total = sum(!!rlang::sym(val_col), na.rm = TRUE)) %>%
                    dplyr::ungroup()
            } else {
                plot_data <- df %>%
                    dplyr::group_by(!!rlang::sym(x_var)) %>%
                    dplyr::mutate(..total = sum(!!rlang::sym(val_col), na.rm = TRUE)) %>%
                    dplyr::ungroup()
            }

            plot_data$..group <- as.factor(plot_data[[x_var]])
            plot_data$..segment <- as.factor(plot_data[[fill_var]])

            vals_num <- plot_data[[val_col]]
            max_tot <- max(plot_data$..total, na.rm = TRUE)
            vals_int <- all(is.finite(vals_num)) && all(abs(vals_num - round(vals_num)) < 1e-8)
            can_use_pkg <- requireNamespace("ggsegmentedtotalbar", quietly = TRUE) &&
                is.null(facet_col) &&
                identical(self$options$orientation, "vertical") &&
                (identical(self$options$sort_categories, "total") || length(levels(plot_data$..group)) <= 1L) &&
                is.finite(max_tot) && max_tot >= 4 &&
                (!isTRUE(self$options$flerlage_show_labels) || vals_int)

            if (can_use_pkg) {
                pkg_df <- data.frame(
                    group = plot_data$..group,
                    segment = plot_data$..segment,
                    value = vals_num,
                    total = plot_data$..total,
                    stringsAsFactors = FALSE
                )
                p <- ggsegmentedtotalbar::ggsegmentedtotalbar(
                    df = pkg_df,
                    group = "group",
                    segment = "segment",
                    value = "value",
                    total = "total",
                    alpha = self$options$flerlage_alpha,
                    color = self$options$flerlage_box_color,
                    label = isTRUE(self$options$flerlage_show_labels),
                    label_size = self$options$flerlage_label_size,
                    label_color = self$options$flerlage_label_color
                )
            } else {
                if (!is.null(facet_col)) {
                    bg_data <- plot_data %>%
                        dplyr::group_by(.data$..group, !!rlang::sym(facet_col)) %>%
                        dplyr::summarise(..total = max(.data$..total, na.rm = TRUE), .groups = "drop")
                } else {
                    bg_data <- plot_data %>%
                        dplyr::group_by(.data$..group) %>%
                        dplyr::summarise(..total = max(.data$..total, na.rm = TRUE), .groups = "drop")
                }

                p <- ggplot2::ggplot(
                    plot_data,
                    ggplot2::aes(x = .data$..group, y = !!rlang::sym(val_col), fill = .data$..segment)
                ) +
                    ggplot2::geom_col(
                        data = bg_data,
                        ggplot2::aes(x = .data$..group, y = .data$..total),
                        fill = self$options$flerlage_box_color,
                        alpha = self$options$flerlage_alpha,
                        width = 0.94,
                        inherit.aes = FALSE
                    ) +
                    ggplot2::geom_col(
                        position = ggplot2::position_dodge(width = 0.9),
                        width = 0.85
                    ) +
                    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))

                if (isTRUE(self$options$flerlage_show_labels)) {
                    plot_data$..val_lbl <- vapply(vals_num, private$.fmtTotal, character(1))
                    bg_data$..tot_lbl <- vapply(bg_data$..total, private$.fmtTotal, character(1))
                    is_horiz <- identical(self$options$orientation, "horizontal")
                    p <- p +
                        ggplot2::geom_text(
                            data = plot_data,
                            ggplot2::aes(
                                x = .data$..group,
                                y = !!rlang::sym(val_col),
                                label = .data$..val_lbl,
                                group = .data$..segment
                            ),
                            position = ggplot2::position_dodge(width = 0.9),
                            vjust = if (is_horiz) 0.5 else -0.3,
                            hjust = if (is_horiz) -0.15 else 0.5,
                            size = self$options$flerlage_label_size,
                            color = self$options$flerlage_label_color,
                            inherit.aes = FALSE
                        ) +
                        ggplot2::geom_text(
                            data = bg_data,
                            ggplot2::aes(
                                x = .data$..group,
                                y = .data$..total,
                                label = .data$..tot_lbl
                            ),
                            vjust = if (is_horiz) 0.5 else -0.5,
                            hjust = if (is_horiz) -0.15 else 0.5,
                            fontface = "bold",
                            size = self$options$flerlage_label_size,
                            color = self$options$flerlage_label_color,
                            inherit.aes = FALSE
                        )
                }

                if (!is.null(facet_col)) {
                    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(facet_col)), scales = "free_x")
                }
                if (identical(self$options$orientation, "horizontal")) {
                    p <- p + ggplot2::coord_flip()
                }
            }

            fler_y_title <- if (nzchar(self$options$y_title) && !identical(self$options$y_title, "Percentage")) {
                self$options$y_title
            } else {
                .("Total")
            }

            # Add custom titles (falling back to variable labels when custom titles are blank)
            p <- p + ggplot2::labs(
                title = if (nzchar(self$options$plot_title)) self$options$plot_title else .("Segmented Total Bar Chart"),
                x = if (nzchar(self$options$x_title)) self$options$x_title else private$.varLabel(x_var),
                y = fler_y_title,
                fill = if (nzchar(self$options$legend_title)) self$options$legend_title else private$.varLabel(fill_var)
            )

            # Apply color palette before chart style so ggtheme never overrides scales
            p <- private$.applyColorPalette(p, plot_data, "..segment", theme = theme)

            # Apply chart style
            chart_theme <- private$.getChartTheme(private$.displayOpt("chart_style"), ggtheme = ggtheme)
            if (!is.null(chart_theme)) {
                p <- p + chart_theme
            }

            if (identical(private$.displayOpt("chart_style"), "bbc_style")) {
                if (nzchar(self$options$x_title)) {
                    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_text(size = 11, face = "bold"))
                }
                if (nzchar(self$options$y_title) && !identical(self$options$y_title, "Percentage")) {
                    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_text(size = 11, face = "bold"))
                }
                if (nzchar(self$options$legend_title)) {
                    p <- p + ggplot2::theme(legend.title = ggplot2::element_text(size = 10, face = "bold"))
                }
            }

            # Legend position and centered title
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(hjust = 0.5),
                legend.position = self$options$legend_position
            )

            # Export-friendly tweaks
            if (isTRUE(self$options$export_ready)) {
                p <- p + ggplot2::theme(
                    plot.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    legend.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            }

            return(p)
        },

        .createClinicalSummary = function() {

            data <- private$.processed_data
            composition_data <- private$.composition_data

            if (is.null(data) || is.null(composition_data)) {
                return()
            }

            # Calculate key metrics
            total_obs <- sum(composition_data$count, na.rm = TRUE)
            n_categories <- length(unique(data[[self$options$x_var]]))
            n_segments <- length(unique(data[[self$options$fill_var]]))

            active_comp <- composition_data[composition_data$total_in_category > 0, , drop = FALSE]
            if (nrow(active_comp) == 0) {
                active_comp <- composition_data
            }

            # htmlEscape user column names + factor-level values before sprintf
            # so downstream setContent rendering is XSS-safe
            x_var_safe <- htmltools::htmlEscape(self$options$x_var)
            y_var_safe <- htmltools::htmlEscape(self$options$y_var)
            fill_var_safe <- htmltools::htmlEscape(self$options$fill_var)

            n_rows_val <- if (is.null(private$.n_rows_analysed)) NA_real_ else private$.n_rows_analysed
            n_rows_safe <- private$.fmtTotal(n_rows_val)
            total_obs_safe <- private$.fmtTotal(total_obs)

            max_p <- max(active_comp$percentage, na.rm = TRUE)
            min_p <- min(active_comp$percentage, na.rm = TRUE)
            within_uniform <- !is.finite(max_p) || (max_p - min_p) <= 1e-9

            n_active_cats <- length(unique(active_comp$category))
            cross_cat_rng <- if (n_active_cats < 2L) {
                0
            } else {
                seg_disp <- active_comp %>%
                    dplyr::group_by(.data$segment) %>%
                    dplyr::summarise(
                        seg_rng = max(.data$percentage, na.rm = TRUE) - min(.data$percentage, na.rm = TRUE),
                        .groups = "drop"
                    )
                max(seg_disp$seg_rng, na.rm = TRUE)
            }
            patterns_vary <- is.finite(cross_cat_rng) && cross_cat_rng > 1e-9
            max_p_fmt <- sprintf("%.1f", max_p)

            if (within_uniform) {
                cat_names_safe <- paste(
                    htmltools::htmlEscape(unique(as.character(data[[self$options$x_var]]))),
                    collapse = ", "
                )
                cat_clause <- if (n_categories <= 6L && nzchar(cat_names_safe)) {
                    paste0(" (", cat_names_safe, ")")
                } else {
                    ""
                }
                summary_text <- .fmt(
                    .("This analysis examined {rows} rows across {cats} categories of {xvar}{clause}, with data segmented by {fill} into {segs} distinct groups, summing {tot} of {yvar}. Segment proportions were uniform across all categories and segments ({pct}% per segment), with no single predominant outcome."),
                    rows = n_rows_safe,
                    cats = n_categories,
                    xvar = x_var_safe,
                    clause = cat_clause,
                    fill = fill_var_safe,
                    segs = n_segments,
                    tot = total_obs_safe,
                    yvar = y_var_safe,
                    pct = max_p_fmt
                )
                report_sentence <- .fmt(
                    .("Segmented total bar chart analysis of {yvar} by {xvar} (N={rows} rows; total {yvar} = {tot}): segment proportions were uniform across all categories ({pct}% each), with no between-category variation in composition."),
                    yvar = y_var_safe,
                    xvar = x_var_safe,
                    rows = n_rows_safe,
                    tot = total_obs_safe,
                    pct = max_p_fmt
                )
            } else {
                top_rows <- active_comp[abs(active_comp$percentage - max_p) < 1e-9, , drop = FALSE]
                cat_bal <- active_comp %>%
                    dplyr::group_by(.data$category) %>%
                    dplyr::summarise(
                        cv = dplyr::if_else(
                            dplyr::n() >= 2L & mean(.data$percentage, na.rm = TRUE) > 0,
                            stats::sd(.data$percentage, na.rm = TRUE) / mean(.data$percentage, na.rm = TRUE),
                            0
                        ),
                        .groups = "drop"
                    )
                min_cv <- min(cat_bal$cv, na.rm = TRUE)
                bal_cats <- cat_bal$category[is.finite(cat_bal$cv) & abs(cat_bal$cv - min_cv) < 1e-9]
                balanced_category_safe <- paste(htmltools::htmlEscape(as.character(bal_cats)), collapse = ", ")

                if (nrow(top_rows) == 1L) {
                    largest_segment_safe <- htmltools::htmlEscape(as.character(top_rows$segment[1]))
                    largest_category_safe <- htmltools::htmlEscape(as.character(top_rows$category[1]))
                    prominent_summary <- .fmt(
                        .("The most prominent finding was '{seg}' in the '{cat}' category, representing {pct}% of that group's total."),
                        seg = largest_segment_safe,
                        cat = largest_category_safe,
                        pct = max_p_fmt
                    )
                    prominent_report <- .fmt(
                        .("{seg} was the predominant segment in {cat} ({pct}%)."),
                        seg = largest_segment_safe,
                        cat = largest_category_safe,
                        pct = max_p_fmt
                    )
                } else {
                    tied_summary <- paste(
                        sprintf("'%s' in '%s'",
                                htmltools::htmlEscape(as.character(top_rows$segment)),
                                htmltools::htmlEscape(as.character(top_rows$category))),
                        collapse = ", "
                    )
                    tied_report <- paste(
                        sprintf("%s in %s",
                                htmltools::htmlEscape(as.character(top_rows$segment)),
                                htmltools::htmlEscape(as.character(top_rows$category))),
                        collapse = " and "
                    )
                    prominent_summary <- .fmt(
                        .("The highest segment share ({pct}%) was tied across {ties}."),
                        pct = max_p_fmt,
                        ties = tied_summary
                    )
                    prominent_report <- .fmt(
                        .("The highest segment share ({pct}%) was tied across {ties}."),
                        pct = max_p_fmt,
                        ties = tied_report
                    )
                }

                if (patterns_vary) {
                    balance_summary <- .fmt(
                        .("The '{cat}' category showed the most balanced distribution across all segments, indicating relatively equal representation of different outcomes within that group."),
                        cat = balanced_category_safe
                    )
                    balance_report <- .fmt(
                        .("Distribution patterns varied across categories, with {cat} showing the most balanced composition."),
                        cat = balanced_category_safe
                    )
                } else if (n_active_cats < 2L) {
                    balance_summary <- .("Only one non-empty category was analyzed, so between-category variation was not assessed.")
                    balance_report <- .("Only a single category was analyzed.")
                } else {
                    balance_summary <- .("Segment proportions were identical across all categories, indicating no between-category variation.")
                    balance_report <- .("Segment proportions were identical across categories, with no between-group variation.")
                }

                summary_text <- paste(
                    .fmt(
                        .("This analysis examined {rows} rows across {cats} categories of {xvar}, with data segmented by {fill} into {segs} distinct groups, summing {tot} of {yvar}."),
                        rows = n_rows_safe,
                        cats = n_categories,
                        xvar = x_var_safe,
                        fill = fill_var_safe,
                        segs = n_segments,
                        tot = total_obs_safe,
                        yvar = y_var_safe
                    ),
                    prominent_summary,
                    balance_summary
                )

                report_sentence <- paste(
                    .fmt(
                        .("Segmented total bar chart analysis of {yvar} by {xvar} (N={rows} rows; total {yvar} = {tot}):"),
                        yvar = y_var_safe,
                        xvar = x_var_safe,
                        rows = n_rows_safe,
                        tot = total_obs_safe
                    ),
                    prominent_report,
                    balance_report
                )
            }

            # Create clinical summary HTML (theme-safe text colors)
            clinical_summary_html <- paste(
                "<div style='background-color: rgba(33, 152, 255, 0.07); padding: 20px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #1e88e5; color: inherit;'>",
                paste0("<h4 style='color: inherit; margin-top: 0;'>", .("Clinical Summary"), "</h4>"),
                paste0("<p style='margin: 10px 0; line-height: 1.6;'>", summary_text, "</p>"),
                paste0("<div style='background-color: rgba(33, 152, 239, 0.13); padding: 12px; border-radius: 4px; margin: 15px 0; color: inherit;'>"),
                paste0("<h5 style='color: inherit; margin-top: 0;'>", .("Copy-Ready Report Sentence:"), "</h5>"),
                paste0("<p style='margin: 5px 0; font-style: italic; font-size: 14px;'>", report_sentence, "</p>"),
                paste0("</div>"),
                paste0("<div style='margin-top: 15px; font-size: 12px; color: inherit; opacity: 0.85;'>"),
                paste0("<strong>", .("Clinical Application:"), "</strong> ", .("Use this visualization to compare proportional outcomes across treatment groups, patient populations, or time periods. Ideal for showing response rates, demographic compositions, or biomarker distributions in clinical research.")),
                paste0("</div>"),
                "</div>"
            )

            self$results$clinical_summary$setContent(clinical_summary_html)
        },

        .applyPresetConfiguration = function() {

            preset <- self$options$analysis_preset

            if (preset == "custom") {
                # Clear any stale cached config so a prior preset does not leak
                # into a subsequent custom run on a reused R6 instance.
                private$.preset_config <- NULL
                private$.renderPreset <- NULL
                return()
            }

            # Store preset configuration for reference
            preset_configs <- list(
                treatment_response = list(
                    suggested_x_label = "Treatment Group",
                    suggested_y_label = "Response Rate (%)",
                    suggested_fill_label = "Response Type",
                    color_palette = "clinical",
                    chart_style = "clinical",
                    show_percentages = TRUE,
                    percentage_format = "integer",
                    guidance = "Ideal for comparing treatment efficacy across different patient groups. Each bar shows the proportion of complete, partial, and no response within each treatment arm."
                ),
                demographics = list(
                    suggested_x_label = "Disease Stage",
                    suggested_y_label = "Patient Distribution (%)",
                    suggested_fill_label = "Demographic Category",
                    color_palette = "colorblind",
                    chart_style = "publication",
                    show_percentages = TRUE,
                    percentage_format = "decimal1",
                    guidance = "Perfect for showing patient characteristics across disease stages or cross-sectional cohorts. Helps identify demographic patterns and potential confounders."
                ),
                biomarker = list(
                    suggested_x_label = "Mutation Status",
                    suggested_y_label = "Expression Distribution (%)",
                    suggested_fill_label = "Expression Level",
                    color_palette = "viridis",
                    chart_style = "clinical",
                    show_percentages = TRUE,
                    percentage_format = "decimal1",
                    guidance = "Excellent for biomarker analysis showing expression patterns across genetic variants. Useful for stratified medicine and precision oncology."
                ),
                quality = list(
                    suggested_x_label = "Department/Unit",
                    suggested_y_label = "Quality Score Distribution (%)",
                    suggested_fill_label = "Performance Rating",
                    color_palette = "bbc_multi",
                    chart_style = "presentation",
                    show_percentages = TRUE,
                    percentage_format = "integer",
                    guidance = "Ideal for quality improvement initiatives. Shows performance metrics distribution across departments or audit periods for benchmarking."
                ),
                temporal = list(
                    suggested_x_label = "Time Point",
                    suggested_y_label = "Patient Status (%)",
                    suggested_fill_label = "Outcome Status",
                    color_palette = "science",
                    chart_style = "clinical",
                    show_percentages = TRUE,
                    percentage_format = "decimal1",
                    guidance = "Designed for visualizing status proportions across time points. Caution: if the same patients are followed across visits (repeated measures), do not rely on Pearson chi-square or CMH p-values because longitudinal correlation violates independence."
                )
            )

            if (preset %in% names(preset_configs)) {
                private$.preset_config <- preset_configs[[preset]]
                private$.renderPreset <- private$.preset_config
            } else {
                private$.preset_config <- NULL
                private$.renderPreset <- NULL
            }
        },

        .updatePresetGuidance = function() {

            if (is.null(private$.preset_config)) {
                return()
            }

            config <- private$.preset_config
            preset_name <- htmltools::htmlEscape(
                stringr::str_to_title(gsub("_", " ", self$options$analysis_preset))
            )

            guidance_html <- paste(
                "<div style='background-color: rgba(33, 149, 236, 0.1); padding: 15px; border-radius: 6px; margin: 10px 0; border-left: 4px solid #2196f3; color: inherit;'>",
                paste0("<h4 style='color: inherit; margin-top: 0;'>", preset_name, " Template</h4>"),
                paste0("<p style='margin: 8px 0; line-height: 1.5;'>", config$guidance, "</p>"),
                "<div style='background-color: rgba(255, 255, 255, 0.06); padding: 10px; border-radius: 4px; margin: 10px 0; color: inherit;'>",
                "<h5 style='color: inherit; margin-top: 0;'>Suggested Variable Labels:</h5>",
                "<ul style='margin: 5px 0; padding-left: 20px;'>",
                paste0("<li><strong>X-axis:</strong> ", config$suggested_x_label, "</li>"),
                paste0("<li><strong>Y-axis:</strong> ", config$suggested_y_label, "</li>"),
                paste0("<li><strong>Segments:</strong> ", config$suggested_fill_label, "</li>"),
                "</ul>",
                "</div>",
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 10px; border-radius: 4px; margin: 10px 0; color: inherit;'>",
                "<h5 style='color: inherit; margin-top: 0;'>What this template changes:</h5>",
                "<ul style='font-size: 13px; line-height: 1.4; margin: 5px 0; padding-left: 20px;'>",
                paste0("<li>Chart style \u2192 <strong>", htmltools::htmlEscape(config$chart_style), "</strong></li>"),
                paste0("<li>Color palette \u2192 <strong>", htmltools::htmlEscape(config$color_palette), "</strong></li>"),
                paste0("<li>Percentage labels \u2192 <strong>",
                       if (isTRUE(config$show_percentages)) "on" else "off",
                       "</strong>, formatted as <strong>",
                       htmltools::htmlEscape(config$percentage_format), "</strong></li>"),
                "</ul>",
                "<p style='font-size: 13px; line-height: 1.4; margin: 5px 0;'>",
                "Each of these applies only while you leave that control at its default \u2013 ",
                "change one yourself and your choice is kept. The template does not change ",
                "your variables, the axis titles above (those are suggestions to type in), ",
                "or anything under Statistical Tests.",
                "</p>",
                "</div>",
                "</div>"
            )

            self$results$preset_guidance$setContent(guidance_html)
        },

        .performStatisticalTests = function(facet_var = NULL) {

            data <- private$.processed_data

            if (is.null(data)) {
                return()
            }

            # Clear existing results and dynamic footnotes
            private$.clearTableRows(self$results$statistical_tests)
            self$results$statistical_tests$setNote("count_assumption", NULL)
            self$results$statistical_tests$setNote("stdres_note", NULL)

            calc_cramers_v <- function(chi_stat, mat) {
                n_tot <- sum(mat, na.rm = TRUE)
                k_min <- min(nrow(mat), ncol(mat)) - 1L
                if (!is.finite(chi_stat) || !is.finite(n_tot) || n_tot <= 0 || k_min < 1L) {
                    return(NA_real_)
                }
                sqrt(as.numeric(chi_stat) / (n_tot * k_min))
            }

            build_fisher_row <- function(mat, alpha_lvl, conf_lvl, panel_lbl = NULL) {
                panel_lbl_safe <- if (is.null(panel_lbl)) NULL else private$.safeLabel(panel_lbl, note = FALSE)
                t_name <- if (is.null(panel_lbl_safe)) {
                    "Fisher's Exact Test"
                } else {
                    paste0("Fisher's Exact Test (", panel_lbl_safe, ")")
                }
                ft <- tryCatch(
                    stats::fisher.test(mat, workspace = 2e7, conf.level = conf_lvl),
                    error = function(err) err
                )
                if (inherits(ft, "error")) {
                    private$.addNotice(
                        "WARNING",
                        .("Fisher's Exact Workspace Exceeded"),
                        .fmt(
                            .("Exact Fisher network calculation exceeded memory limits ({msg}). Reduce table dimensions or collapse rare levels."),
                            msg = ft$message
                        )
                    )
                    return(list(
                        test_name = t_name,
                        statistic = NA_real_,
                        df = NA_integer_,
                        p_value = NA_real_,
                        cramers_v = NA_real_,
                        interpretation = private$.safeLabel(paste0("Exact calculation unavailable: ", ft$message), note = FALSE)
                    ))
                }

                f_sig <- is.finite(ft$p.value) && ft$p.value < alpha_lvl
                or_val <- NA_real_
                or_clause <- ""
                if (nrow(mat) == 2L && ncol(mat) == 2L && !is.null(ft$estimate) && length(ft$estimate) == 1L) {
                    or_val <- as.numeric(ft$estimate[[1]])
                    if (!is.null(ft$conf.int) && length(ft$conf.int) == 2L) {
                        ci_pct <- round(conf_lvl * 100)
                        or_clause <- sprintf(
                            "; conditional OR = %.2f (%d%% CI: %.2f-%.2f)",
                            or_val, ci_pct, ft$conf.int[1], ft$conf.int[2]
                        )
                    }
                }
                prefix_lbl <- if (is.null(panel_lbl_safe)) "" else paste0("Panel ", panel_lbl_safe, ": ")
                f_interp <- if (f_sig) {
                    paste0(prefix_lbl, "Exact test: significant association (p < ", alpha_lvl, ")", or_clause)
                } else {
                    paste0(prefix_lbl, "Exact test: no significant association detected (p >= ", alpha_lvl, ")", or_clause)
                }
                list(
                    test_name = t_name,
                    statistic = or_val,
                    df = NA_integer_,
                    p_value = as.numeric(ft$p.value),
                    cramers_v = NA_real_,
                    interpretation = f_interp
                )
            }

            tryCatch({
                x_var <- self$options$x_var
                fill_var <- self$options$fill_var
                x_var_cell <- private$.safeLabel(x_var, note = FALSE)
                fill_var_cell <- private$.safeLabel(fill_var, note = FALSE)
                conf_lvl <- self$options$confidence_level
                alpha <- 1 - conf_lvl
                use_fisher <- isTRUE(private$.optionOr("fisher_exact", FALSE))

                cnt_vec <- private$.metricVec(data, "count")

                if (!is.null(facet_var) && facet_var %in% names(data)) {
                    facet_var_cell <- private$.safeLabel(facet_var, note = FALSE)
                    # Stratified contingency analysis across facet_var panels (prevents Simpson's paradox)
                    strat_data <- data.frame(
                        r = data[[x_var]],
                        c = data[[fill_var]],
                        k = data[[facet_var]],
                        wt = cnt_vec,
                        stringsAsFactors = FALSE
                    )
                    xt3 <- stats::xtabs(wt ~ r + c + k, data = strat_data)

                    # Drop globally empty categories, segments, and panels
                    keep_r <- apply(xt3, 1, sum) > 0
                    keep_c <- apply(xt3, 2, sum) > 0
                    keep_k <- apply(xt3, 3, sum) > 0
                    xt3 <- xt3[keep_r, keep_c, keep_k, drop = FALSE]

                    if (dim(xt3)[1] < 2 || dim(xt3)[2] < 2 || dim(xt3)[3] < 1) {
                        stop("At least two nonempty categories and segments are required.")
                    }

                    self$results$statistical_tests$setNote(
                        "count_assumption",
                        .fmt(
                            .("Stratified by '{facet}' rather than pooling across split panels (which can induce Simpson's paradox). Cells are the summed Value Variable, treated as independent case frequencies; on a measurement the statistic scales with the unit of measurement and <b>overstates</b> significance."),
                            facet = private$.safeLabel(facet_var, note = TRUE)
                        ),
                        init = FALSE
                    )

                    row_idx <- 1L
                    total_low_cells <- 0L
                    total_checked_cells <- 0L

                    if (dim(xt3)[3] >= 2L) {
                        cmh_test <- tryCatch(
                            suppressWarnings(stats::mantelhaen.test(xt3, correct = FALSE)),
                            error = function(e) NULL
                        )
                        if (!is.null(cmh_test) && is.finite(cmh_test$p.value)) {
                            cmh_sig <- cmh_test$p.value < alpha
                            cmh_interp <- if (cmh_sig) {
                                paste0("Significant conditional association between ", x_var_cell, " and ", fill_var_cell,
                                       " stratified by ", facet_var_cell, " (p < ", alpha, ")")
                            } else {
                                paste0("No significant conditional association was detected between ", x_var_cell, " and ", fill_var_cell,
                                       " stratified by ", facet_var_cell, " (p >= ", alpha, "); this does not establish conditional independence")
                            }
                            self$results$statistical_tests$addRow(
                                rowKey = row_idx,
                                values = list(
                                    test_name = "Cochran-Mantel-Haenszel",
                                    statistic = as.numeric(cmh_test$statistic[[1]]),
                                    df = as.integer(cmh_test$parameter[[1]]),
                                    p_value = as.numeric(cmh_test$p.value),
                                    cramers_v = NA_real_,
                                    interpretation = cmh_interp
                                )
                            )
                            row_idx <- row_idx + 1L
                        }
                    }

                    panel_names <- dimnames(xt3)[[3]]
                    for (k in seq_len(dim(xt3)[3])) {
                        sub_m <- xt3[, , k, drop = TRUE]
                        if (!is.matrix(sub_m)) {
                            sub_m <- matrix(sub_m, nrow = dim(xt3)[1], ncol = dim(xt3)[2],
                                            dimnames = list(dimnames(xt3)[[1]], dimnames(xt3)[[2]]))
                        }
                        sub_m <- sub_m[rowSums(sub_m) > 0, colSums(sub_m) > 0, drop = FALSE]
                        p_lbl <- panel_names[k]
                        p_lbl_cell <- private$.safeLabel(p_lbl, note = FALSE)
                        if (any(dim(sub_m) < 2)) {
                            self$results$statistical_tests$addRow(
                                rowKey = row_idx,
                                values = list(
                                    test_name = paste0("Pearson's Chi-square (", p_lbl_cell, ")"),
                                    statistic = NA_real_,
                                    df = NA_integer_,
                                    p_value = NA_real_,
                                    cramers_v = NA_real_,
                                    interpretation = sprintf("Panel '%s' has fewer than two non-empty categories or segments.", p_lbl_cell)
                                )
                            )
                            row_idx <- row_idx + 1L
                            next
                        }

                        chi_k <- suppressWarnings(stats::chisq.test(sub_m, correct = FALSE))
                        low_k <- sum(chi_k$expected < 5)
                        total_low_cells <- total_low_cells + low_k
                        total_checked_cells <- total_checked_cells + length(chi_k$expected)

                        sig_k <- is.finite(chi_k$p.value) && chi_k$p.value < alpha
                        interp_k <- if (sig_k) {
                            paste0("Panel ", p_lbl_cell, ": significant association (p < ", alpha, ")")
                        } else {
                            paste0("Panel ", p_lbl_cell, ": no significant association detected (p >= ", alpha, ")")
                        }
                        if (low_k > 0) {
                            interp_k <- paste0(interp_k, " [", low_k, "/", length(chi_k$expected), " expected cells < 5]")
                        }
                        self$results$statistical_tests$addRow(
                            rowKey = row_idx,
                            values = list(
                                test_name = paste0("Pearson's Chi-square (", p_lbl_cell, ")"),
                                statistic = as.numeric(chi_k$statistic[[1]]),
                                df = as.integer(chi_k$parameter[[1]]),
                                p_value = as.numeric(chi_k$p.value),
                                cramers_v = calc_cramers_v(chi_k$statistic[[1]], sub_m),
                                interpretation = interp_k
                            )
                        )
                        row_idx <- row_idx + 1L

                        if (use_fisher) {
                            self$results$statistical_tests$addRow(
                                rowKey = row_idx,
                                values = build_fisher_row(sub_m, alpha, conf_lvl, panel_lbl = p_lbl)
                            )
                            row_idx <- row_idx + 1L
                        }
                    }

                    if (total_low_cells > 0L) {
                        private$.addNotice("WARNING", "Low expected counts",
                            .fmt(
                                .("{low} of {tot} within-panel cells have expected counts below 5, so chi-square approximations may be unreliable in sparse panels."),
                                low = total_low_cells,
                                tot = total_checked_cells
                            ))
                    }
                    return()
                }

                # Unfaceted contingency table for Pearson's chi-square test
                contingency_data <- data.frame(
                    r = data[[x_var]],
                    c = data[[fill_var]],
                    wt = cnt_vec,
                    stringsAsFactors = FALSE
                )

                contingency_matrix <- stats::xtabs(wt ~ r + c, data = contingency_data)
                contingency_matrix <- contingency_matrix[rowSums(contingency_matrix) > 0,
                    colSums(contingency_matrix) > 0, drop = FALSE]
                if (any(dim(contingency_matrix) < 2))
                    stop("At least two nonempty categories and segments are required.")
                chi_test <- suppressWarnings(stats::chisq.test(contingency_matrix, correct = FALSE))

                self$results$statistical_tests$setNote(
                    "count_assumption",
                    .("Cells are the summed Value Variable, treated as independent case frequencies. Valid only if that variable counts cases; on a measurement the statistic scales with the unit of measurement and <b>overstates</b> significance."),
                    init = FALSE
                )

                # Expected-count check
                low <- sum(chi_test$expected < 5)
                if (low > 0)
                    private$.addNotice("WARNING", "Low expected counts",
                        .fmt(
                            .("{low} of {tot} cells have expected counts below 5, so the chi-square approximation is unreliable here."),
                            low = low,
                            tot = length(chi_test$expected)
                        ))

                is_significant <- is.finite(chi_test$p.value) && chi_test$p.value < alpha

                interpretation <- if (is_significant) {
                    paste0("Significant association between ", x_var_cell, " and ", fill_var_cell, " (p < ", alpha, ")")
                } else {
                    paste0("No significant association was detected between ", x_var_cell, " and ", fill_var_cell,
                           " (p >= ", alpha, "); this does not establish that the variables are independent")
                }

                if (low > 0)
                    interpretation <- paste0(interpretation,
                        ". Chi-square approximation unreliable here (", low, " of ",
                        length(chi_test$expected), " cells with expected count < 5) - ",
                        "Fisher's exact test is the alternative")

                next_row_key <- 1L
                test_row <- list(
                    test_name = "Pearson's Chi-square",
                    statistic = chi_test$statistic[[1]],
                    df = as.integer(chi_test$parameter[[1]]),
                    p_value = chi_test$p.value,
                    cramers_v = calc_cramers_v(chi_test$statistic[[1]], contingency_matrix),
                    interpretation = interpretation
                )

                self$results$statistical_tests$addRow(rowKey = next_row_key, values = test_row)
                next_row_key <- next_row_key + 1L

                if (use_fisher) {
                    self$results$statistical_tests$addRow(
                        rowKey = next_row_key,
                        values = build_fisher_row(contingency_matrix, alpha, conf_lvl, panel_lbl = NULL)
                    )
                    next_row_key <- next_row_key + 1L
                }

                # Post-hoc Haberman (1973) adjusted standardized residuals for any significant table > 2x2
                if (is_significant && (nrow(contingency_matrix) > 2 || ncol(contingency_matrix) > 2)) {

                    residuals <- chi_test$stdres
                    max_residual_pos <- which(abs(residuals) == max(abs(residuals), na.rm = TRUE), arr.ind = TRUE)

                    if (length(max_residual_pos) > 0 && nrow(max_residual_pos) > 0) {
                        max_row <- private$.safeLabel(rownames(residuals)[max_residual_pos[1, 1]], note = FALSE)
                        max_col <- private$.safeLabel(colnames(residuals)[max_residual_pos[1, 2]], note = FALSE)
                        residual_value <- residuals[max_residual_pos[1, 1], max_residual_pos[1, 2]]

                        residual_interpretation <- if (residual_value > 0) {
                            paste0("Highest positive association: ", max_row, " \u2192 ", max_col)
                        } else {
                            paste0("Highest negative association: ", max_row, " \u2192 ", max_col)
                        }

                        residual_row <- list(
                            test_name = "Standardized Residuals",
                            statistic = residual_value,
                            df = NA_integer_,
                            p_value = NA_real_,
                            cramers_v = NA_real_,
                            interpretation = residual_interpretation
                        )

                        self$results$statistical_tests$addRow(rowKey = next_row_key, values = residual_row)

                        n_cells <- length(residuals)
                        z_bonf <- stats::qnorm(1 - alpha / (2 * n_cells))
                        self$results$statistical_tests$setNote(
                            "stdres_note",
                            .fmt(
                                .("Standardized Residuals reports the largest Haberman (1973) adjusted residual (chisq.test()$stdres), asymptotically N(0, 1) under independence (Bonferroni-adjusted critical |z| = {z} across {n} cells)."),
                                z = sprintf("%.2f", z_bonf),
                                n = n_cells
                            ),
                            init = FALSE
                        )
                    }
                }

            }, error = function(e) {
                private$.addNotice(
                    "WARNING",
                    .("Statistical Test Skipped"),
                    .fmt(
                        .("Contingency test could not be computed: {msg}"),
                        msg = e$message
                    )
                )
                error_row <- list(
                    test_name = "Chi-square Test",
                    statistic = NA_real_,
                    df = NA_integer_,
                    p_value = NA_real_,
                    cramers_v = NA_real_,
                    interpretation = private$.safeLabel(paste("Test failed:", e$message), note = FALSE)
                )

                self$results$statistical_tests$addRow(rowKey = 1L, values = error_row)
            })
            # Notices are rendered once at the end of .run() (not here), so they
            # appear whether or not statistical tests are requested.
        }
    )
)
