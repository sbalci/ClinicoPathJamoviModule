#' @title Diagnostic Test Meta-Analysis for Pathology
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom mada reitsma phm
#' @importFrom metafor rma
#' @importFrom htmltools tagList
#' @importFrom stats qnorm pnorm qt pt
#' @export
#' @return An \code{R6} class generator object for the \code{diagnosticmetaClass} backend; used internally by the jamovi analysis wrapper and not called directly.


diagnosticmetaClass <- R6::R6Class(
    "diagnosticmetaClass",
    inherit = diagnosticmetaBase,
    private = list(
        # Cache variables for performance optimization
        .continuity_correction = FALSE,
        .correction_method_used = "none",
        .corrected_study_names = character(0),
        .pooled_sensitivity = NULL,
        .pooled_specificity = NULL,
        # Confidence intervals for the pooled estimates, so the interpretation
        # text can qualify a claim the interval does not support.
        .pooled_sens_ci = NULL,
        .pooled_spec_ci = NULL,
        .pooled_sens_pi = NULL,
        .pooled_spec_pi = NULL,
        # TRUE only when the fit actually estimated between-study variance that
        # dominates the prediction interval - see .performBivariateMetaAnalysis.
        .heterogeneity_substantial = FALSE,
        .n_studies = 0,
        .biv_model = NULL,
        # Accumulated user-facing notices for the current run; reset at the top
        # of .run() and rendered into the `notices` Html item.
        .noticeList = NULL,


        .metaforMethod = function(method) {
            switch(
                tolower(method %||% "reml"),
                "fixed" = "FE",
                "ml" = "ML",
                "reml" = "REML",
                "mm" = "DL",
                "vc" = "HE",
                "REML"
            )
        },

        .metaforLevel = function() {
            level <- self$options$confidence_level %||% 95
            max(50, min(99, as.numeric(level)))
        },

        # Human-readable zero-cell-correction name for notices and summaries.
        .correctionLabel = function(key) {
            switch(key %||% "none",
                   constant = .("+0.5 to all cells of zero-cell studies"),
                   zero_cells = .("+0.5 to the zero cells only"),
                   reciprocal_n = .("+1/N to all cells (N = study size)"),
                   as.character(key))
        },

        # Human-readable estimation-method name for table notes. Notes
        # previously showed internal keys ("reml", "DL"), which read as debug
        # output to a clinician.
        # library-audit 2026-09-16 OncoPath [LOW] DEFERRED: the method name is spliced into sentences via %s;
        #   revisit when these sentences are next rewritten or translated (guide section 9)
        .methodTitle = function(method) {
            switch(
                tolower(method %||% "reml"),
                "fixed" = .("fixed-effects"),
                "ml" = .("maximum likelihood"),
                "reml" = "REML",
                "mm" = .("method-of-moments"),
                "vc" = .("variance-components"),
                as.character(method)
            )
        },

        # Wilson score interval for a proportion. Shared by the forest plot
        # renderer and the Individual Study Results table so both display the
        # same per-study intervals.
        .wilsonCI = function(x, n, z) {
            if (!is.finite(x) || !is.finite(n) || n == 0) {
                return(c(0, 1))
            }
            p <- x / n
            denominator <- 1 + z^2 / n
            center <- (p + z^2 / (2 * n)) / denominator
            margin <- z * sqrt((p * (1 - p) / n + z^2 / (4 * n^2))) / denominator
            c(max(0, center - margin), min(1, center + margin))
        },

        # ---- Notices --------------------------------------------------------
        # User-facing warnings and information banners. These previously went
        # through .appendInstructionMessage(), which appended them below ~95
        # lines of onboarding HTML in the Getting Started panel and never reset
        # between runs, so the same banner accumulated once per run cycle.
        # Notices now render into a dedicated always-visible `notices` Html
        # item, rebuilt from scratch each run.
        # library-audit 2026-09-16 OncoPath [INFO] REJECTED: no native notice element - type: Notice fails the
        #   .r.yaml schema, type: Notification builds no results object (guide section 13)
        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type,
                title = title,
                content = content
            )
            # Render immediately so a notice added just before an early return
            # is never lost.
            private$.renderNotices()
        },

        .renderNotices = function() {
            # Content-driven, not a failure signal: with nothing to say, the
            # panel would otherwise render as a titled empty box on every clean
            # run. There is no declarative `visible:` that can express
            # "the notice list is non-empty".
            self$results$notices$setVisible(length(private$.noticeList) > 0)

            if (length(private$.noticeList) == 0) {
                # Clear any content left from a previous run cycle.
                self$results$notices$setContent("")
                return()
            }

            # library-audit 2026-09-16 OncoPath [INFO] DONE: titles inherit the pane colour - fixed hues fell to
            #   2.7-2.9:1 on the dark theme; the translucent tint and the border carry the severity
            typeStyles <- list(
                ERROR = list(bgcolor = "rgba(220, 38, 38, 0.10)", border = "#fca5a5"),
                STRONG_WARNING = list(bgcolor = "rgba(234, 88, 12, 0.10)", border = "#fdba74"),
                WARNING = list(bgcolor = "rgba(202, 138, 4, 0.12)", border = "#fde047"),
                INFO = list(bgcolor = "rgba(37, 99, 235, 0.08)", border = "#93c5fd")
            )

            html <- "<div style='margin: 10px 0;'>"
            for (notice in private$.noticeList) {
                style <- typeStyles[[notice$type]] %||% typeStyles$INFO
                html <- paste0(
                    html,
                    "<div style='background-color: ", style$bgcolor, "; ",
                    "border-left: 4px solid ", style$border, "; ",
                    "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                    "<strong style='color: inherit;'>",
                    htmltools::htmlEscape(notice$title), "</strong><br>",
                    "<span style='color: inherit;'>",
                    htmltools::htmlEscape(notice$content), "</span>",
                    "</div>"
                )
            }
            html <- paste0(html, "</div>")

            self$results$notices$setContent(html)
        },

        .renderSymbols = function(text) {
            replacements <- c(
                "[[SUP2]]" = intToUtf8(0x00B2),
                "[[GE]]" = intToUtf8(0x2265),
                "[[APPROX]]" = intToUtf8(0x2248),
                "[[TIMES]]" = intToUtf8(0x00D7)
            )
            for (token in names(replacements)) {
                text <- gsub(token, replacements[[token]], text, fixed = TRUE)
            }
            text
        },

        # Helper function to get color palette for accessibility.
        # `theme` is the jamovi theme object handed to every render function;
        # only the "jamovi" choice consults it, so the accessibility palettes
        # (colorblind_safe, high_contrast) are never overridden by a document
        # theme.
        .getColorPalette = function(theme = NULL) {
            palette_option <- self$options$color_palette %||% "standard"

            if (identical(palette_option, "jamovi")) {
                pal <- try(jmvcore::colorPalette(4, theme$palette %||% "jmv", "color"),
                           silent = TRUE)
                if (!inherits(pal, "try-error") && length(pal) >= 4)
                    return(list(
                        primary      = pal[1],
                        secondary    = pal[2],
                        tertiary     = pal[3],
                        study_points = pal[4]
                    ))
            }

            switch(palette_option,
                "standard" = list(
                    primary = "darkblue",
                    secondary = "darkgreen",
                    tertiary = "darkred",
                    study_points = "gray"
                ),
                "colorblind_safe" = list(
                    primary = "#0173B2",     # Blue
                    secondary = "#029E73",   # Green
                    tertiary = "#CC78BC",    # Pink
                    study_points = "#56B4E9" # Light blue
                ),
                "high_contrast" = list(
                    primary = "#000000",     # Black
                    # secondary is used for the funnel-plot points and forest
                    # plot elements; white (#FFFFFF) made them invisible on the
                    # light-theme plot background.
                    secondary = "#4D4D4D",   # Dark gray
                    tertiary = "#808080",    # Gray
                    study_points = "#404040" # Darker gray
                ),
                "viridis" = list(
                    primary = "#440154",     # Dark purple
                    secondary = "#21908C",   # Teal
                    tertiary = "#FDE725",    # Yellow
                    study_points = "#35B779"  # Green
                ),
                "plasma" = list(
                    primary = "#0D0887",     # Dark blue
                    secondary = "#CC4678",   # Pink
                    tertiary = "#F0F921",    # Yellow
                    study_points = "#7E03A8"  # Purple
                ),
                # Default fallback
                list(
                    primary = "darkblue",
                    secondary = "darkgreen",
                    tertiary = "darkred",
                    study_points = "gray"
                )
            )
        },

        .init = function() {

            # Initialize content for all static Html outputs. Their visibility
            # is governed entirely by the declarative `visible:` expressions in
            # the .r.yaml; the imperative setVisible() overrides that used to
            # live here made the Analysis Summary and Clinical Interpretation
            # checkboxes dead until every variable was assigned (and left
            # interpretation hidden after a reject).
            private$.populateInstructions()

            # Each panel is built only when its own option asks for it. These
            # six together are about 31 KB of HTML, and they used to be built on
            # every run and written into every saved .omv even with all six
            # checkboxes off - the .r.yaml `visible:` expressions were the only
            # thing the options did. Building them here (rather than in .run())
            # keeps the reference panels available before any variable is
            # assigned, which is when a reader most wants the methodology.
            # The else branches matter: jmvcore's Html$fromProtoBuf restores
            # content unconditionally, so a panel that was ticked once would
            # otherwise keep its HTML in the results - and in every later save -
            # after the box is cleared.
            if (isTRUE(self$options$show_methodology))
                private$.populateAboutPanel()
            else self$results$about$setContent("")
            if (isTRUE(self$options$show_interpretation))
                private$.populateInterpretation()
            else self$results$interpretation$setContent("")

            # The three plot-explanation panels are static interpretation
            # guides: populate them here so a data change can never leave
            # a visible-but-empty titled panel behind.
            if (isTRUE(self$options$show_plot_explanations)) {
                private$.populateForestPlotExplanation()
                private$.populateSROCPlotExplanation()
                private$.populateFunnelPlotExplanation()
            } else {
                self$results$forestplot_explanation$setContent("")
                self$results$srocplot_explanation$setContent("")
                self$results$funnelplot_explanation$setContent("")
            }

            # Fixed row structure for the bivariate results table: the same five
            # parameters with the same labels on every run. Only the estimates
            # depend on the fitted model, so .run() fills them with setRow().
            biv_parameters <- list(
                sensitivity = .("Pooled Sensitivity"),
                specificity = .("Pooled Specificity"),
                plr         = .("Positive Likelihood Ratio"),
                nlr         = .("Negative Likelihood Ratio"),
                dor         = .("Diagnostic Odds Ratio")
            )
            for (biv_key in names(biv_parameters))
                self$results$bivariateresults$addRow(
                    rowKey = biv_key,
                    values = list(parameter = biv_parameters[[biv_key]]))

            # library-audit 2026-09-16 meddecide [LOW] DONE (same class): publicationbias has a fixed row set, so .init()
            # scaffolds the rows and .run() fills them with setRow()
            # One row, Deeks' test, whenever publication bias is requested. .run()
            # fills it, or leaves it blank beside the note that says why.
            if (isTRUE(self$options$publication_bias))
                self$results$publicationbias$addRow(
                    rowKey = "deeks_test",
                    values = list(test = .("Deeks' Funnel Plot Asymmetry Test")))

            # Same class of fix for the other two fixed row sets. Building these
            # in .run() with deleteRows() + addRow() made the tables appear
            # empty and then visibly restructure on every run.
            if (isTRUE(self$options$heterogeneity_analysis)) {
                het_rows <- list(sensitivity = .("Sensitivity"),
                                 specificity = .("Specificity"))
                for (het_key in names(het_rows))
                    self$results$heterogeneity$addRow(
                        rowKey = het_key,
                        values = list(measure = het_rows[[het_key]]))
            }

            # mada::phm always returns theta and taus_sq; the AUC row is derived
            # from theta. An unexpected coefficient name still falls back to
            # addRow() in .run().
            if (isTRUE(self$options$hsroc_analysis)) {
                phm_rows <- list(
                    theta   = .("Diagnostic accuracy parameter (theta)"),
                    taus_sq = .("Between-study variance (tau^2)"),
                    auc     = .("Area under the SROC curve (AUC = 1/(1 + theta))"))
                for (phm_key in names(phm_rows))
                    self$results$hsrocresults$addRow(
                        rowKey = phm_key,
                        values = list(parameter = phm_rows[[phm_key]]))
            }

        },
        
        .run = function() {

            # library-audit 2026-09-16 OncoPath [INFO] DEFERRED: six HTML panels are untranslated English literals
            #   (.populateInstructions/Interpretation/AboutPanel, forest/SROC/funnel plot explanations);
            #   revisit when diagnosticmeta's translation pass runs (/prepare-translation diagnosticmeta)

            # Reset the notices accumulator and clear any banner content left
            # from the previous run cycle; every .addNotice() below re-renders.
            private$.noticeList <- list()
            private$.renderNotices()

            private$.continuity_correction <- FALSE
            private$.pooled_sensitivity <- NULL
            private$.pooled_specificity <- NULL
            private$.biv_model <- NULL
            # The CI/PI fields are written late in the bivariate fit (and the PI
            # only inside a tryCatch that can fail), so without clearing them a
            # failed re-run would reuse the previous dataset's intervals in the
            # summary and heterogeneity text.
            private$.pooled_sens_ci <- NULL
            private$.pooled_spec_ci <- NULL
            private$.pooled_sens_pi <- NULL
            private$.pooled_spec_pi <- NULL
            private$.heterogeneity_substantial <- FALSE

            # Check if data is ready
            if (is.null(self$data) || nrow(self$data) == 0) {
                # Show instructions when no data
                self$results$instructions$setVisible(TRUE)
                return()
            }
            
            # Get variables with safe escaping for special characters
            # Use raw option values as column-name lookup keys. The previous
            # .escapeVar() helper mangled names containing spaces/punctuation
            # (e.g. "Study Name (2020)" became "Study_Name_2020_") and then
            # used the mangled string as a self$data[[...]] key, which silently
            # returned NULL and broke the analysis. The variable names never
            # flow into a formula in this function, so no escaping is needed.
            study_var <- self$options$study
            tp_var <- self$options$true_positives
            fp_var <- self$options$false_positives
            fn_var <- self$options$false_negatives
            tn_var <- self$options$true_negatives

            # Check if all required variables are provided
            all_provided <- !is.null(study_var) && !is.null(tp_var) &&
                          !is.null(fp_var) && !is.null(fn_var) && !is.null(tn_var)

            if (!all_provided) {
                # Show instructions if any variables are missing
                self$results$instructions$setVisible(TRUE)
                private$.addNotice("ERROR", .("Variables required"),
                    .("Select the study identifier and the TP, FP, FN and TN count variables to run the meta-analysis."))
                return()
            }

            # All variables provided - hide instructions and proceed with analysis
            self$results$instructions$setVisible(FALSE)
            
            # Extract data
            data <- self$data
            
            # A count column typed Nominal or Ordinal in jamovi arrives as a
            # FACTOR carrying a `values` attribute, and jmvcore's own
            # canBeNumeric() accepts it, so `permitted: numeric` lets it through.
            # as.numeric() on such a column returns level indices (1..L), not the
            # counts: a 12-study set read 55.6% pooled sensitivity instead of
            # 81.7%, with the first study's TP shown as 17 instead of 81, and no
            # warning anywhere. toNumeric() unwraps `values`; the factor fallback
            # covers a column jamovi did not label.
            count_of <- function(x) {
                x <- jmvcore::toNumeric(x)
                if (is.factor(x) || is.character(x))
                    x <- suppressWarnings(as.numeric(as.character(x)))
                as.numeric(x)
            }

            # Create diagnostic test data structure
            meta_data <- data.frame(
                row_id = seq_len(nrow(data)),
                study = as.character(data[[study_var]]),
                tp = count_of(data[[tp_var]]),
                fp = count_of(data[[fp_var]]),
                fn = count_of(data[[fn_var]]),
                tn = count_of(data[[tn_var]]),
                stringsAsFactors = FALSE
            )

            # Store original row count for validation
            original_n <- nrow(meta_data)

            # Exclude unusable studies and SAY SO.
            #
            # A single missing cell anywhere previously aborted the entire
            # analysis, and negative counts were dropped silently while the
            # original study count was never carried anywhere - so the
            # reported number of studies was the post-exclusion count with no
            # indication that anything had been removed.
            incomplete <- is.na(meta_data$tp) | is.na(meta_data$fp) |
                          is.na(meta_data$fn) | is.na(meta_data$tn)
            negative   <- !incomplete &
                          (meta_data$tp < 0 | meta_data$fp < 0 |
                           meta_data$fn < 0 | meta_data$tn < 0)
            # A study with no diseased or no non-diseased participants yields no
            # sensitivity or no specificity and cannot enter a bivariate model.
            empty_arm  <- !incomplete & !negative &
                          ((meta_data$tp + meta_data$fn) == 0 |
                           (meta_data$fp + meta_data$tn) == 0)

            drop <- incomplete | negative | empty_arm
            meta_data <- meta_data[!drop, , drop = FALSE]

            if (any(drop)) {
                reasons <- character(0)
                if (sum(incomplete) > 0)
                    reasons <- c(reasons, sprintf(.("%d with missing counts"), sum(incomplete)))
                if (sum(negative) > 0)
                    reasons <- c(reasons, sprintf(.("%d with negative counts"), sum(negative)))
                if (sum(empty_arm) > 0)
                    reasons <- c(reasons, sprintf(.("%d with no diseased or no non-diseased participants"), sum(empty_arm)))
                private$.addNotice("WARNING", .("Studies excluded"), sprintf(
                    .("%d of %d studies were excluded before analysis (%s). All results below are based on the remaining %d studies."),
                    sum(drop), original_n, paste(reasons, collapse = "; "), nrow(meta_data)))
            }

            # Counts must be whole numbers. Fractional values reach mada (which
            # rounds them silently), are truncated to integers in the study table,
            # and crash the no-bivariate summary path through sprintf("%d").
            if (nrow(meta_data) > 0) {
                counts <- as.matrix(meta_data[, c("tp", "fp", "fn", "tn")])
                fractional <- rowSums(abs(counts - round(counts)) > 1e-8) > 0
                if (any(fractional)) {
                    # Proportions in all four cells is a different mistake, and a
                    # fatal one: every "study" then has 1 or 2 participants.
                    if (max(meta_data$tp + meta_data$fn, meta_data$fp + meta_data$tn, na.rm = TRUE) <= 1)
                        jmvcore::reject(
                            .("The counts look like proportions: no study has more than one diseased or one non-diseased participant. Enter the number of participants in each cell of the 2x2 table (TP, FP, FN, TN)."),
                            code = "counts_are_proportions")
                    private$.addNotice("STRONG_WARNING", .("Counts are not whole numbers"), sprintf(
                        .("%d of %d studies have fractional counts. Cell counts are numbers of participants; check whether percentages or rates were entered. These studies are excluded."),
                        sum(fractional), nrow(meta_data)))
                    meta_data <- meta_data[!fractional, , drop = FALSE]
                    # Say WHY when the exclusion empties the analysis; the generic
                    # "at least 3 studies" message below would not mention counts.
                    if (nrow(meta_data) < 3)
                        jmvcore::reject(
                            .fmt(.("Cell counts must be whole numbers, and {n} study/studies have fractional counts, which leaves too few studies. Enter the number of participants in each cell of the 2x2 table (TP, FP, FN, TN)."),
                                 n = sum(fractional)),
                            code = "fractional_counts")
                }
            }

            # Two studies sharing an identifier (or none at all) are drawn on one
            # forest row, so one silently hides behind the other.
            if (nrow(meta_data) > 0) {
                unnamed <- is.na(meta_data$study) | !nzchar(trimws(meta_data$study))
                meta_data$study[unnamed] <- .("Unnamed study")
                duplicated_ids <- unique(meta_data$study[duplicated(meta_data$study)])
                if (length(duplicated_ids) > 0 || any(unnamed)) {
                    private$.addNotice("WARNING", .("Study identifiers are not unique"), sprintf(
                        .("%d study identifier(s) appear more than once or are missing (%s). Each row is analysed as its own study and the labels were made unique for the tables and the forest plot."),
                        length(duplicated_ids) + sum(unnamed),
                        paste(utils::head(c(duplicated_ids, if (any(unnamed)) .("Unnamed study")), 5), collapse = ", ")))
                    meta_data$study <- make.unique(meta_data$study, sep = " #")
                }
            }

            if (nrow(meta_data) < 3) {
                # Fatal: reject() so jamovi greys the results and reports an
                # analysis-level error. Writing this into the instructions panel
                # left the pane looking healthy, so a hard stop read as advice.
                jmvcore::reject(
                    .fmt(
                        .("At least 3 studies with complete diagnostic test data are required ({n} found)."),
                        n = nrow(meta_data)),
                    code = "insufficient_studies")
            }

            # Store number of studies for summary
            private$.n_studies <- nrow(meta_data)


            prepared_data <- private$.prepareAnalysisData(meta_data)
            analysis_data <- prepared_data$analysis_data
            mada_data <- prepared_data$mada_data

            # Disclose a user-selected zero-cell correction where it is always
            # visible. The corrected counts feed EVERY downstream analysis
            # (SROC, heterogeneity, meta-regression, forest plot), but the
            # previous disclosures lived only on the bivariate table (hidden
            # when that analysis is off) and in the optional summary panel.
            zero_cell_studies <- if (nrow(mada_data) > 0)
                sum(rowSums(mada_data[, c("TP", "FP", "FN", "TN"), drop = FALSE] == 0) > 0) else 0L
            if (!isTRUE(prepared_data$continuity_correction) && zero_cell_studies > 0) {
                private$.addNotice("INFO", .("Zero cells present"), sprintf(
                    .("%d study/studies contain a zero cell. No user correction is selected, so mada's default single-study 0.5 correction is applied inside the bivariate model only: the univariate heterogeneity and meta-regression models exclude those studies instead, so they rest on fewer studies than the pooled estimates. Choose a Zero-Cell Correction option to keep every study in every model."),
                    zero_cell_studies))
            }
            if (isTRUE(prepared_data$continuity_correction)) {
                corrected_names <- private$.corrected_study_names
                private$.addNotice("INFO", .("Zero-cell correction applied"), sprintf(
                    .("A zero-cell correction (%s) was applied to %d study/studies (%s). Corrected counts are used by all analyses and plots; the Individual Study Results table shows the raw counts."),
                    private$.correctionLabel(private$.correction_method_used),
                    length(corrected_names),
                    paste(corrected_names, collapse = ", ")))

                # The same disclosure as a note on the bivariate table. It used
                # to be written by .generateSummary(), which meant it was tied
                # to a panel the user may have switched off.
                #
                # Table notes are NOT a plain-text sink: jamovi renders a small
                # HTML allow-list in them (i, em, b, strong, sub, sup). These
                # are study labels straight from the user's data, so a study
                # named "<sup>MARKER</sup>" would render as a superscript in a
                # caveat printed beside the pooled estimates. Not XSS - script,
                # img and event handlers are outside the allow-list - but it
                # lets crafted data reshape a clinical disclosure, so strip the
                # six tags jamovi honours. Everything else, including a bare
                # "<" or "&", is left exactly as the user typed it.
                honoured_tags <- "</?[[:space:]]*(i|em|b|strong|sub|sup)([[:space:]][^>]*)?>"
                shown_names <- gsub(honoured_tags, "",
                                    utils::head(corrected_names, 3),
                                    ignore.case = TRUE)
                self$results$bivariateresults$setNote("zero_cell_warning", sprintf(
                    .("Zero-cell correction applied (%s) to %d of %d studies (%s). Results should be interpreted with caution as corrections can introduce bias, especially in large studies."),
                    private$.correctionLabel(private$.correction_method_used),
                    length(corrected_names), nrow(meta_data),
                    paste(shown_names, collapse = ", ")))
            }

            private$.checkpoint()

            # Perform bivariate meta-analysis when enabled
            if (isTRUE(self$options$bivariate_analysis)) {
                tryCatch({
                    private$.performBivariateMetaAnalysis(analysis_data = analysis_data,
                                                          mada_data = mada_data)
                    if (isTRUE(self$options$show_analysis_summary))
                        private$.generateSummary(meta_data)
                }, error = function(e) {
                    private$.pooled_sensitivity <- NULL
                    private$.pooled_specificity <- NULL
                    # Table notes are a plain-text sink (limited HTML allow-list),
                    # so the message must NOT be HTML-escaped: escaping turned
                    # "object 'x' not found" into "object &apos;x&apos; not found (numeric entity was rendered literally)".
                    self$results$bivariateresults$setNote("error", sprintf(.("Bivariate analysis error: %s"), e$message))
                    private$.addNotice("ERROR", .("Bivariate model failed"), sprintf(
                        .("The Reitsma bivariate model could not be fitted (%s). Pooled sensitivity and specificity are unavailable; the SROC plot and summary fall back to descriptive output."),
                        e$message))
                    if (isTRUE(self$options$show_analysis_summary))
                        private$.generateSummary(meta_data)
                })
            } else {
                private$.pooled_sensitivity <- NULL
                private$.pooled_specificity <- NULL
                if (isTRUE(self$options$show_analysis_summary))
                    private$.generateBasicSummary(meta_data)
            }
            # Html content survives an untick unless cleared - same reason as
            # the .init() panels.
            if (!isTRUE(self$options$show_analysis_summary))
                self$results$summary$setContent("")

            private$.checkpoint()

            # Perform the Holling proportional-hazards SROC analysis if requested
            if (isTRUE(self$options$hsroc_analysis)) {
                tryCatch({
                    private$.performPHMSROCAnalysis(
                        meta_data = meta_data,
                        mada_data = mada_data
                    )
                }, error = function(e) {
                    # Table notes are plain text - do not HTML-escape.
                    self$results$hsrocresults$setNote(
                        "error",
                        sprintf(
                            .("Proportional-hazards SROC analysis error: %s"),
                            e$message
                        )
                    )
                })
            }

            private$.checkpoint()

            # Perform heterogeneity analysis
            if (isTRUE(self$options$heterogeneity_analysis)) {
                tryCatch({
                    private$.performHeterogeneityAnalysis(analysis_data = analysis_data)
                }, error = function(e) {
                    # Table notes are plain text - do not HTML-escape.
                    self$results$heterogeneity$setNote("error", sprintf(.("Heterogeneity analysis error: %s"), e$message))
                })
            }

            private$.checkpoint()

            # Perform meta-regression if covariate specified
            if (isTRUE(self$options$meta_regression)) {
                tryCatch({
                    private$.performMetaRegression(meta_data = meta_data,
                                                   analysis_data = analysis_data)
                }, error = function(e) {
                    # Table notes are plain text - do not HTML-escape.
                    self$results$metaregression$setNote("error", sprintf(.("Meta-regression error: %s"), e$message))
                })
            }

            private$.checkpoint()

            # Perform publication bias assessment
            if (isTRUE(self$options$publication_bias)) {
                tryCatch({
                    private$.performPublicationBiasAssessment(analysis_data = analysis_data)
                }, error = function(e) {
                    # Table notes are plain text - do not HTML-escape.
                    self$results$publicationbias$setNote("error", sprintf(.("Publication bias analysis error: %s"), e$message))
                })
            }
            
            private$.checkpoint()

            # Generate plots
            if (self$options$forest_plot) {
                private$.populateForestPlot(analysis_data)
            }

            if (self$options$sroc_plot) {
                # Same continuity-corrected data as the forest plot and the
                # Reitsma fit behind the pooled point; the raw counts put
                # zero-cell studies at a different place on the two figures.
                private$.populateSROCPlot(analysis_data)
            }

            if (self$options$funnel_plot && self$options$publication_bias) {
                private$.populateFunnelPlot(analysis_data)
            }

            # Populate individual studies table
            if (self$options$show_individual_studies) {
                private$.populateIndividualStudies(meta_data)
            }

            # Every result item is gated by an option, so switching off the one
            # that is on by default left a valid dataset showing an entirely
            # empty results pane with nothing to explain it - the instructions
            # panel having been hidden the moment the variables were assigned.
            shows_something <- c(
                self$options$bivariate_analysis, self$options$hsroc_analysis,
                self$options$heterogeneity_analysis, self$options$meta_regression,
                self$options$publication_bias, self$options$show_individual_studies,
                self$options$forest_plot, self$options$sroc_plot,
                # the funnel item is visible: (funnel_plot && publication_bias)
                isTRUE(self$options$funnel_plot) && isTRUE(self$options$publication_bias),
                self$options$show_analysis_summary,
                self$options$show_interpretation, self$options$show_methodology)
            if (!any(vapply(shows_something, isTRUE, logical(1)))) {
                private$.addNotice("INFO", .("Nothing selected to display"), sprintf(
                    .("%d studies were read successfully, but every analysis and display option is switched off, so there is nothing to show. Tick Bivariate random-effects model for the pooled sensitivity and specificity, or any other option under Analysis, Visualization or Reporting."),
                    nrow(meta_data)))
            }

        },
        
        .performBivariateMetaAnalysis = function(analysis_data, mada_data) {

            # Check data availability
            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                self$results$bivariateresults$setNote("data_error", .("Analysis data is missing or empty"))
                return()
            }

            if (!requireNamespace("mada", quietly = TRUE)) {
                self$results$bivariateresults$setNote("package_error", .("mada package is not available"))
                return()
            }

            if (is.null(mada_data) || nrow(mada_data) < 3) {
                n_rows <- if (is.null(mada_data)) 0 else nrow(mada_data)
                self$results$bivariateresults$setNote("insufficient_data", sprintf(.("Insufficient data for meta-analysis. Found %d studies, need at least 3"), n_rows))
                return()
            }

            # The Reitsma model is a bivariate NORMAL approximation to the logit
            # sensitivity/specificity pair. With small cells that approximation
            # is biased, and a continuity correction is doing part of the work:
            # on an 8-study set with 7 zero cells it gave 90.3%/94.0% where a
            # binomial GLMM gave 94.3%/96.2% - 4.0 and 2.2 points apart. Say so
            # when the data are actually sparse; this module offers no GLMM.
            # Threshold calibrated so this does not fire on ordinary DTA data:
            # a cell of 2-4 is common and tolerable, a cell of 0 or 1 is where
            # the normal approximation and the continuity correction take over.
            sparse_rows <- rowSums(mada_data[, c("TP", "FP", "FN", "TN"), drop = FALSE] <= 1) > 0
            if (sum(sparse_rows) > 0 && sum(sparse_rows) / nrow(mada_data) >= 0.25) {
                private$.addNotice("WARNING", .("Sparse data: pooled estimates may be biased"), sprintf(
                    .("%d of %d studies have a cell below 5. The bivariate model used here approximates the logit sensitivity and specificity as normal, which is biased when cells are that small, and any zero cell additionally needs a continuity correction. Published comparisons put the gap against an exact binomial (generalised linear mixed) model at several percentage points on data like this. Treat the pooled pair as approximate, and confirm it in software offering a binomial bivariate model before relying on it."),
                    sum(sparse_rows), nrow(mada_data)))
            }

            # Three studies clear the hard floor above and are then pooled with
            # no caution at all, from a model that estimates FIVE parameters:
            # two means and three elements of the between-study covariance
            # matrix. The fit succeeds and prints a confident-looking interval
            # (85.7%, 79.2-90.4 on a three-study example), but the between-study
            # variance is barely identified and the interval is not to be
            # trusted at face value.
            if (nrow(mada_data) < 5) {
                private$.addNotice("STRONG_WARNING", .("Very few studies"), sprintf(
                    .("Only %d studies are pooled. The bivariate model estimates five parameters (two pooled means plus three between-study variance components), so with this many studies the between-study variance is barely identified and the confidence intervals are narrower than the evidence warrants. Treat the pooled pair as descriptive, report the individual studies alongside it, and do not use it to set a clinical threshold."),
                    nrow(mada_data)))
            }

            # No out-of-range check: `method` is a List whose five keys are
            # exactly the ones mada::reitsma accepts, and jmvcore rejects
            # anything else before .run() is reached ("Argument 'method' must be
            # one of 'reml', 'ml', 'fixed', 'mm', 'vc'"). The fallback that used
            # to sit here could never run, and its message could never be seen.
            method_used <- self$options$method %||% "reml"

            conf_level <- (self$options$confidence_level %||% 95) / 100
            conf_level <- max(min(conf_level, 0.999), 0.5)

            # Be explicit about the continuity correction rather than inheriting
            # mada's defaults (correction = 0.5, correction.control = "all").
            #
            # Under "all", a single zero cell anywhere causes mada to add 0.5 to
            # ALL FOUR cells of EVERY study. The option offering that is labelled
            # "None (Model-Based)" and is the default, so the setting a user picks
            # to AVOID a correction applied a heavier one than either option that
            # advertises a correction - and the disclosure block was gated on
            # correction_method != "none", making it the only setting that said
            # nothing. "single" corrects only the affected studies.
            correction_method <- self$options$zero_cell_correction %||% "none"
            zero_present <- any(mada_data[, c("TP", "FP", "FN", "TN")] == 0, na.rm = TRUE)

            # .quietly muffles only the named warning: mada's checkdata "non
            # zero decimal places" fires on the deliberately fractional counts
            # produced by the zero_cells / reciprocal_n corrections and would
            # land in jamovi's Analysis Notes on every run. Substantive fit
            # warnings still propagate.
            biv_model <- .quietly(mada::reitsma(
                mada_data, method = method_used,
                correction = 0.5,
                correction.control = if (identical(correction_method, "none")) "single" else "all"
            ), deprecation_pattern = "non zero decimal places")

            if (zero_present) {
                n_zero_studies <- sum(rowSums(mada_data[, c("TP", "FP", "FN", "TN")] == 0) > 0)
                bivariate_table_early <- self$results$bivariateresults
                bivariate_table_early$setNote("zero_cell", sprintf(
                    if (identical(correction_method, "none"))
                        .("%d study/studies contain a zero cell. A continuity correction of 0.5 was applied to those studies only so the model could be fitted. Continuity corrections shift the pooled estimate; compare the alternatives under Zero Cell Correction to see how sensitive your result is.")
                    else
                        .("%d study/studies contain a zero cell. A continuity correction of 0.5 was applied to all studies so the model could be fitted. Continuity corrections shift the pooled estimate; compare the alternatives under Zero Cell Correction to see how sensitive your result is."),
                    n_zero_studies))
            }
            private$.biv_model <- biv_model

            # Report the fitted between-study variance components: the
            # sens/spec correlation is the threshold-effect indicator and the
            # tau-squared values quantify the heterogeneity the model actually
            # estimated. Neither appeared anywhere in the output before.
            Psi_vc <- biv_model$Psi
            if (is.matrix(Psi_vc) && all(dim(Psi_vc) >= 2) &&
                all(is.finite(diag(Psi_vc)[1:2])) && all(diag(Psi_vc)[1:2] > 0)) {
                # mada parameterizes in (logit sens, logit FPR); logit(spec) =
                # -logit(FPR), so the sens/spec correlation is the NEGATIVE of
                # the fitted correlation.
                corr_ss <- -Psi_vc[1, 2] / sqrt(Psi_vc[1, 1] * Psi_vc[2, 2])

                # A correlation between two variance components that are
                # numerically zero is a ratio of two rounding residues. On
                # homogeneous data mada returned tau-squared of 6e-11 and 7e-10
                # and this note printed "correlation = -0.54. A strongly negative
                # correlation suggests a threshold effect", while metafor on the
                # same data gave +0.21 with a profile interval covering -1 to 1.
                # The number is not identifiable below this cut-off (a variance
                # of 1e-4 on the logit scale is a between-study SD of 1%).
                estimable <- all(diag(Psi_vc)[1:2] > 1e-4)
                vc_note <- sprintf(
                    .("Between-study variance components: tau-squared (logit sensitivity) = %.3f, tau-squared (logit false-positive rate) = %.3f."),
                    Psi_vc[1, 1], Psi_vc[2, 2])
                if (estimable) {
                    vc_note <- paste(vc_note, sprintf(
                        .("Correlation between logit sensitivity and logit specificity = %.2f. A strongly negative correlation suggests a threshold effect (studies trading sensitivity for specificity)."),
                        corr_ss))
                    # With few studies the correlation routinely lands on the +/-1
                    # boundary - a degenerate estimate, not evidence of a perfect
                    # relationship.
                    if (is.finite(corr_ss) && abs(corr_ss) > 0.99) {
                        vc_note <- paste(vc_note,
                            .("The correlation is estimated at the boundary (+/-1), which with few studies usually means it cannot be estimated reliably - do not over-interpret it."))
                    }
                } else if (all(diag(Psi_vc)[1:2] <= 1e-4)) {
                    vc_note <- paste(vc_note,
                        .("Both are effectively zero, so the studies are consistent with a single common sensitivity and specificity and the correlation between them is not identifiable - no threshold effect can be assessed from this fit."))
                } else {
                    # One margin carries real between-study variance and the
                    # other does not - the routine picture when specificity is
                    # pinned near 1. Saying "both are effectively zero" denied
                    # heterogeneity the model had just estimated and printed two
                    # lines above.
                    vc_note <- paste(vc_note,
                        .("One of the two is effectively zero, so the correlation between them is not identifiable and no threshold effect can be assessed from this fit; the other is not zero, so read the tau-squared values above as the heterogeneity on that margin."))
                }
                self$results$bivariateresults$setNote("variance_components", vc_note)
            }
            summary_results <- tryCatch(
                summary(biv_model, level = conf_level),
                error = function(e) NULL
            )
            coefficients <- if (!is.null(summary_results)) {
                summary_results[["coefficients"]]
            } else {
                NULL
            }

            # mada 0.5.12 fits method = "fixed" correctly, but its
            # summary.reitsma() assumes a random-effects covariance matrix and
            # errors. Build the same four summary rows directly from the fitted
            # fixed-effect coefficients and covariance matrix.
            if (is.null(coefficients) && method_used == "fixed") {
                fixed_coef <- biv_model$coefficients
                fixed_vcov <- biv_model$vcov
                if (is.matrix(fixed_coef) && nrow(fixed_coef) >= 1 &&
                    all(c("tsens", "tfpr") %in% colnames(fixed_coef)) &&
                    is.matrix(fixed_vcov) && nrow(fixed_vcov) >= 2) {
                    logit_estimate <- c(
                        tsens = fixed_coef[1, "tsens"],
                        tfpr = fixed_coef[1, "tfpr"]
                    )
                    standard_error <- sqrt(diag(fixed_vcov)[1:2])
                    z_value <- logit_estimate / standard_error
                    p_value <- 2 * stats::pnorm(-abs(z_value))
                    z_critical <- stats::qnorm(1 - (1 - conf_level) / 2)
                    logit_lower <- logit_estimate - z_critical * standard_error
                    logit_upper <- logit_estimate + z_critical * standard_error
                    ci_prefix <- paste0(100 * conf_level, "%ci.")

                    coefficients <- rbind(
                        "tsens.(Intercept)" = c(
                            logit_estimate["tsens"], standard_error[1],
                            z_value["tsens"], p_value["tsens"],
                            logit_lower["tsens"], logit_upper["tsens"]
                        ),
                        "tfpr.(Intercept)" = c(
                            logit_estimate["tfpr"], standard_error[2],
                            z_value["tfpr"], p_value["tfpr"],
                            logit_lower["tfpr"], logit_upper["tfpr"]
                        ),
                        "sensitivity" = c(
                            stats::plogis(logit_estimate["tsens"]),
                            NA_real_, NA_real_, NA_real_,
                            stats::plogis(logit_lower["tsens"]),
                            stats::plogis(logit_upper["tsens"])
                        ),
                        "false pos. rate" = c(
                            stats::plogis(logit_estimate["tfpr"]),
                            NA_real_, NA_real_, NA_real_,
                            stats::plogis(logit_lower["tfpr"]),
                            stats::plogis(logit_upper["tfpr"])
                        )
                    )
                    colnames(coefficients) <- c(
                        "Estimate", "Std. Error", "z", "Pr(>|z|)",
                        paste0(ci_prefix, "lb"), paste0(ci_prefix, "ub")
                    )
                    self$results$bivariateresults$setNote(
                        "fixed_summary",
                        .("Fixed-effect confidence intervals were computed from the fitted Reitsma covariance matrix.")
                    )
                }
            }

            if (is.null(coefficients) || !is.matrix(coefficients)) {
                self$results$bivariateresults$setNote("model_error", .("Reitsma model failed - coefficient matrix missing"))
                private$.addNotice("STRONG_WARNING", .("Bivariate output missing"),
                    .("The Reitsma model did not return coefficient estimates, so pooled sensitivity and specificity are unavailable."))
                return()
            }

            coef_rows <- rownames(coefficients)
            get_row <- function(target) {
                if (is.null(coef_rows)) {
                    return(NULL)
                }
                idx <- which(coef_rows == target)
                if (length(idx) == 0) {
                    idx <- which(tolower(coef_rows) == tolower(target))
                }
                if (length(idx) == 0) {
                    return(NULL)
                }
                coefficients[idx[1], , drop = FALSE]
            }

            sens_prob_row <- get_row("sensitivity")
            fpr_prob_row <- get_row("false pos. rate")
            sens_logit_row <- get_row("tsens.(Intercept)")
            fpr_logit_row <- get_row("tfpr.(Intercept)")

            if (is.null(sens_prob_row) || is.null(fpr_prob_row) ||
                is.null(sens_logit_row) || is.null(fpr_logit_row)) {
                self$results$bivariateresults$setNote(
                    "model_error",
                    .("Reitsma model returned unexpected coefficient structure")
                )
                private$.addNotice("STRONG_WARNING", .("Bivariate output missing"),
                    .("The Reitsma model did not return the expected coefficient estimates, so pooled sensitivity and specificity are unavailable."))
                return()
            }

            safe_lr <- function(numer, denom) {
                if (!is.finite(numer) || !is.finite(denom) || denom <= 0) {
                    return(NA_real_)
                }
                numer / denom
            }

            z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)

            bivariate_table <- self$results$bivariateresults

            ci_lower_col <- grep("ci\\.lb$", colnames(coefficients), value = TRUE)
            ci_upper_col <- grep("ci\\.ub$", colnames(coefficients), value = TRUE)
            ci_lower_col <- ci_lower_col[1]
            ci_upper_col <- ci_upper_col[1]

            pooled_sens <- sens_prob_row[1, "Estimate"]
            sens_ci <- c(NA_real_, NA_real_)
            # grep(...)[1] is NA_character_ (not NULL) when no column matches;
            # indexing with NA threw "subscript out of bounds" into the outer
            # tryCatch, replacing the whole bivariate table with an error note.
            if (!is.na(ci_lower_col) && !is.na(ci_upper_col)) {
                sens_ci <- c(sens_prob_row[1, ci_lower_col], sens_prob_row[1, ci_upper_col])
            }
            private$.pooled_sensitivity <- pooled_sens

            fpr_estimate <- fpr_prob_row[1, "Estimate"]
            pooled_spec <- if (is.finite(fpr_estimate)) 1 - fpr_estimate else NA_real_
            spec_ci <- c(NA_real_, NA_real_)
            if (!is.na(ci_lower_col) && !is.na(ci_upper_col)) {
                fpr_ci_lower <- fpr_prob_row[1, ci_lower_col]
                fpr_ci_upper <- fpr_prob_row[1, ci_upper_col]
                if (is.finite(fpr_ci_lower) && is.finite(fpr_ci_upper)) {
                    spec_ci <- c(1 - fpr_ci_upper, 1 - fpr_ci_lower)
                }
            }
            private$.pooled_specificity <- pooled_spec

            # SERIALIZATION FIX: Use table note instead of inserting Notice
            # The closing sentence is set later, once it is known whether a
            # prediction interval exists: under the fixed-effect method it does
            # not, and this note used to send the reader to it anyway.
            het_info_note <- .("I\u00b2 is not reported here: a univariate I\u00b2 ignores the within-study correlation between sensitivity and specificity and does not describe the bivariate model (Cochrane DTA Handbook, chapter 10). The Heterogeneity Assessment table reports Q, tau-squared and a univariate I\u00b2 computed SEPARATELY for sensitivity and for specificity - read those as descriptive summaries of each margin, not as the heterogeneity of the bivariate model.")

            # The p-values on the sensitivity/specificity rows are the Wald tests
            # of the logit intercepts, i.e. H0: sensitivity = 50% and
            # H0: false-positive rate = 50%. That is almost never the hypothesis a
            # reader assumes from an unlabelled "p" column, and it is trivially
            # significant for any usable test, so name it.
            # D29: the table reported five estimates with intervals and never
            # said what confidence level they were at, how many studies they
            # pooled, or how many participants those studies contributed - all
            # of which a reader needs before quoting the numbers.
            n_dis <- sum(mada_data$TP + mada_data$FN, na.rm = TRUE)
            n_non <- sum(mada_data$FP + mada_data$TN, na.rm = TRUE)
            bivariate_table$setNote("scope", sprintf(
                .("Pooled from %d studies comprising %s participants (%s with the target condition, %s without). Intervals are %d%% confidence intervals; sensitivity and specificity are percentages, the likelihood ratios and the diagnostic odds ratio are ratios."),
                nrow(mada_data),
                # base:: is required: the package imports jmvcore, whose format()
                # masks base::format() and silently ignores big.mark.
                base::format(n_dis + n_non, big.mark = ","),
                base::format(n_dis, big.mark = ","),
                base::format(n_non, big.mark = ","),
                self$options$confidence_level %||% 95))

            bivariate_table$setNote("pvalue_meaning",
                .("P-values on the sensitivity and specificity rows test the null hypothesis that the parameter equals 50% (no better than chance), not that the test meets any clinical threshold. They are significant for essentially any usable assay and should not be read as evidence of adequate accuracy - use the confidence and prediction intervals for that. Sensitivity and specificity rows are percentages; likelihood ratios and the diagnostic odds ratio are ratios."))

            private$.pooled_sens_ci <- sens_ci * 100
            private$.pooled_spec_ci <- spec_ci * 100

            # Prediction interval for a FUTURE study, and a heterogeneity warning.
            #
            # The pooled point plus its confidence interval describes how well the
            # AVERAGE is known; it says nothing about whether the assay performs
            # consistently. With substantial between-study variance the analysis
            # would previously report a tight pooled estimate with no indication
            # that a new laboratory could see something quite different. Built
            # from vcov + Psi, the same quantity mada uses for its prediction
            # region, so the note and the SROC plot agree.
            tryCatch({
                Sig_m <- stats::vcov(biv_model)
                Psi_m <- biv_model$Psi
                if (is.matrix(Sig_m) && is.matrix(Psi_m) &&
                    all(dim(Sig_m) >= 2) && all(dim(Psi_m) >= 2)) {
                    tot <- Sig_m[1:2, 1:2] + Psi_m[1:2, 1:2]
                    mu_l <- as.numeric(stats::coef(biv_model))
                    if (all(is.finite(diag(tot))) && all(diag(tot) >= 0) &&
                        length(mu_l) >= 2 && all(is.finite(mu_l[1:2]))) {
                        # t on k - 2 df, not z: with the small k typical of DTA
                        # meta-analyses a normal quantile understates exactly the
                        # between-study uncertainty the interval exists to show
                        # (Riley et al 2011, BMJ 342:d549).
                        k_studies <- nrow(mada_data)
                        crit <- if (is.finite(k_studies) && k_studies >= 3)
                            stats::qt(1 - (1 - conf_level) / 2, df = k_studies - 2)
                        else z_crit
                        sens_pi <- stats::plogis(mu_l[1] + c(-1, 1) * crit * sqrt(tot[1, 1])) * 100
                        # tfpr -> specificity: spec = 1 - plogis(tfpr)
                        spec_pi <- sort((1 - stats::plogis(mu_l[2] + c(-1, 1) * crit * sqrt(tot[2, 2]))) * 100)

                        pct <- self$options$confidence_level %||% 95
                        private$.pooled_sens_pi <- sens_pi
                        private$.pooled_spec_pi <- spec_pi

                        pred_note <- sprintf(
                            .("Prediction interval for a future study (%d%%, t distribution on %d df): sensitivity %.1f%%-%.1f%%, specificity %.1f%%-%.1f%%. This is where a NEW study or laboratory is expected to fall and includes between-study heterogeneity; the confidence intervals above describe only how precisely the pooled average is estimated."),
                            pct, max(k_studies - 2, 1), sens_pi[1], sens_pi[2], spec_pi[1], spec_pi[2])

                        # A wide prediction interval is not by itself evidence of
                        # heterogeneity. The interval has two sources of width:
                        # the between-study variance Psi, and the uncertainty in
                        # the pooled mean Sig inflated by t on k - 2 df, which is
                        # large whenever k is small (t_2 = 4.30). Three identical
                        # studies - Q = 0, I-squared = 0, tau-squared = 0 - still
                        # produced a 42%-98% interval, and this warning then told
                        # the reader that "studies differ more than sampling error
                        # explains". Require the between-study component to
                        # actually dominate before saying so.
                        het_share <- function(i) {
                            denom <- Sig_m[i, i] + Psi_m[i, i]
                            if (!is.finite(denom) || denom <= 0) return(0)
                            Psi_m[i, i] / denom
                        }
                        wide <- function(pi) is.finite(pi[2] - pi[1]) && (pi[2] - pi[1]) > 30
                        sens_het <- wide(sens_pi) && het_share(1) > 0.5
                        spec_het <- wide(spec_pi) && het_share(2) > 0.5
                        private$.heterogeneity_substantial <- sens_het || spec_het

                        if (private$.heterogeneity_substantial) {
                            private$.addNotice("STRONG_WARNING", .("Substantial between-study heterogeneity"), sprintf(
                                .("The prediction interval spans a wide range of accuracy (sensitivity %.0f%%-%.0f%%, specificity %.0f%%-%.0f%%). A single pooled sensitivity and specificity may not usefully describe this body of evidence: studies differ more than sampling error explains, commonly because of differing positivity thresholds, patient spectrum or reference standards. Prefer the SROC curve and the prediction region over the pooled point, and investigate the source of heterogeneity before applying these figures to your own practice."),
                                sens_pi[1], sens_pi[2], spec_pi[1], spec_pi[2]))
                        } else if (wide(sens_pi) || wide(spec_pi)) {
                            pred_note <- paste(pred_note, sprintf(
                                .("This interval is wide, but the estimated between-study variance (tau-squared = %.3f for logit sensitivity, %.3f for logit false-positive rate) is smaller than the uncertainty in the pooled mean itself: most of the width comes from having only %d studies, not from the studies disagreeing. More studies would narrow it."),
                                Psi_m[1, 1], Psi_m[2, 2], k_studies))
                        }
                        if (isTRUE(self$options$sroc_plot))
                            pred_note <- paste(pred_note,
                                .("The dotted prediction region on the SROC plot is the joint two-dimensional version of this interval, drawn at the large-sample radius mada uses (2.45 at 95%), while this interval uses t on k-2 df. They therefore do not coincide: below about eight studies the printed interval is the wider of the two, and above that the region is. Read them as two views of the same quantity rather than matching one to the other."))
                        bivariate_table$setNote("prediction", pred_note)
                    }
                }
            }, error = function(e) NULL)

            # No Psi means no between-study variance was estimated - the
            # fixed-effect option. Every heterogeneity safeguard in this analysis
            # is built from Psi, so choosing it silently removed the prediction
            # interval, the prediction region on the SROC plot and the
            # heterogeneity warning, leaving a narrow confidence interval and
            # nothing to say the studies disagree. Say it instead.
            self$results$bivariateresults$setNote("heterogeneity_info",
                paste(het_info_note, if (!is.null(private$.pooled_sens_pi))
                    .("For a model-consistent statement of how much studies differ, use the prediction interval above and the prediction region on the SROC plot.")
                else
                    .("This model provides no prediction interval and no SROC prediction region, so it offers no model-consistent statement of how much studies differ.")))

            if (is.null(private$.pooled_sens_pi)) {
                is_fixed <- identical(self$options$method, "fixed")
                bivariate_table$setNote("no_prediction", if (is_fixed)
                    .("No prediction interval is available: the fixed-effect method assumes every study estimates the SAME sensitivity and specificity, so it estimates no between-study variance. The confidence interval above is therefore narrower than a random-effects interval by construction, and no prediction region is drawn on the SROC plot. Check the Heterogeneity Assessment table before relying on it: if Q is significant or I-squared is large, a single fixed-effect estimate does not describe this body of evidence and REML should be used instead.")
                else
                    .("No prediction interval is available: the model did not return a between-study covariance matrix. The confidence interval above describes only how precisely the pooled average is estimated, not how much studies differ."))
                if (is_fixed)
                    private$.addNotice("STRONG_WARNING", .("Fixed-effect model: heterogeneity not assessed"),
                        .("The fixed-effect method assumes a single common sensitivity and specificity across studies. It produces no prediction interval, no SROC prediction region and no heterogeneity warning, and its confidence intervals are narrower than the random-effects equivalents whatever the studies actually show. Use it only when the Heterogeneity Assessment table gives no sign of heterogeneity; REML (the default) is the appropriate choice otherwise."))
            }

            bivariate_table$setRow(rowKey = "sensitivity", values = list(
                estimate = pooled_sens * 100,  # Convert to percentage
                ci_lower = sens_ci[1] * 100,   # Convert to percentage
                ci_upper = sens_ci[2] * 100,   # Convert to percentage
                p_value = sens_logit_row[1, "Pr(>|z|)"]
            ))

            bivariate_table$setRow(rowKey = "specificity", values = list(
                estimate = pooled_spec * 100,  # Convert to percentage
                ci_lower = spec_ci[1] * 100,   # Convert to percentage
                ci_upper = spec_ci[2] * 100,   # Convert to percentage
                p_value = fpr_logit_row[1, "Pr(>|z|)"]
            ))

            pooled_plr <- safe_lr(pooled_sens, 1 - pooled_spec)
            pooled_nlr <- safe_lr(1 - pooled_sens, pooled_spec)
            pooled_dor <- safe_lr(pooled_plr, pooled_nlr)

            lr_ci <- list(plr = c(NA_real_, NA_real_),
                          nlr = c(NA_real_, NA_real_),
                          dor = c(NA_real_, NA_real_))

            vcov_matrix <- biv_model$vcov
            if (is.matrix(vcov_matrix) && all(dim(vcov_matrix) >= 2) &&
                all(is.finite(vcov_matrix[1:2, 1:2])) &&
                is.finite(pooled_sens) && is.finite(pooled_spec)) {

                var_logit_sens <- vcov_matrix[1, 1]
                var_logit_spec <- vcov_matrix[2, 2]
                # mada's Reitsma model is parameterized in (tsens, tfpr) =
                # (logit sensitivity, logit FALSE-positive rate). Because
                # logit(spec) = logit(1 - FPR) = -tfpr, the covariance between
                # logit(sens) and logit(spec) is the NEGATIVE of vcov[1, 2].
                # The three delta-method variance formulas below add
                # 2 * grad_s * grad_p * cov_sens_spec, so using the correctly
                # signed covariance yields v1 + v2 - 2c for DOR, etc.
                cov_sens_spec <- -vcov_matrix[1, 2]

                if (var_logit_sens >= 0 && var_logit_spec >= 0) {
                    if (is.finite(pooled_plr) && pooled_plr > 0) {
                        var_log_plr <- ((1 - pooled_sens)^2 * var_logit_sens) +
                                       (pooled_spec^2 * var_logit_spec) +
                                       (2 * (1 - pooled_sens) * pooled_spec * cov_sens_spec)
                        if (is.finite(var_log_plr) && var_log_plr >= 0) {
                            se_log_plr <- sqrt(var_log_plr)
                            lr_ci$plr <- exp(log(pooled_plr) + c(-1, 1) * z_crit * se_log_plr)
                        }
                    }

                    if (is.finite(pooled_nlr) && pooled_nlr > 0) {
                        var_log_nlr <- (pooled_sens^2 * var_logit_sens) +
                                       ((1 - pooled_spec)^2 * var_logit_spec) +
                                       (2 * pooled_sens * (1 - pooled_spec) * cov_sens_spec)
                        if (is.finite(var_log_nlr) && var_log_nlr >= 0) {
                            se_log_nlr <- sqrt(var_log_nlr)
                            lr_ci$nlr <- exp(log(pooled_nlr) + c(-1, 1) * z_crit * se_log_nlr)
                        }
                    }

                    if (is.finite(pooled_dor) && pooled_dor > 0) {
                        var_log_dor <- var_logit_sens + var_logit_spec + 2 * cov_sens_spec
                        if (is.finite(var_log_dor) && var_log_dor >= 0) {
                            se_log_dor <- sqrt(var_log_dor)
                            lr_ci$dor <- exp(log(pooled_dor) + c(-1, 1) * z_crit * se_log_dor)
                        }
                    }
                }
            }

            bivariate_table$setRow(rowKey = "plr", values = list(
                estimate = pooled_plr,
                ci_lower = lr_ci$plr[1],
                ci_upper = lr_ci$plr[2],
                p_value = NA_real_
            ))

            bivariate_table$setRow(rowKey = "nlr", values = list(
                estimate = pooled_nlr,
                ci_lower = lr_ci$nlr[1],
                ci_upper = lr_ci$nlr[2],
                p_value = NA_real_
            ))

            bivariate_table$setRow(rowKey = "dor", values = list(
                estimate = pooled_dor,
                ci_lower = lr_ci$dor[1],
                ci_upper = lr_ci$dor[2],
                p_value = NA_real_
            ))

            bivariate_table$setNote("method", sprintf(.("Reitsma bivariate model, %s estimation."),
                private$.methodTitle(method_used)))
        },
        
        .performPHMSROCAnalysis = function(meta_data, mada_data) {

            if (!requireNamespace("mada", quietly = TRUE)) {
                self$results$hsrocresults$setNote("package_error",
                    .("The mada package is not available, so the proportional-hazards SROC model could not be fitted."))
                return()
            }

            {

                hsroc_table <- self$results$hsrocresults
                # Blank the rows scaffolded in .init() rather than deleting them.
                for (blank_key in c("theta", "taus_sq", "auc"))
                    hsroc_table$setRow(rowKey = blank_key, values = list(
                        estimate = NA_real_, std_error = NA_real_,
                        z_value = NA_real_, p_value = NA_real_))

                # Validate input data
                if (is.null(meta_data) || nrow(meta_data) == 0) {
                    self$results$hsrocresults$setNote(
                        "insufficient",
                        .("Insufficient data for proportional-hazards SROC analysis")
                    )
                    return()
                }

                # Check for required columns
                required_cols <- c("tp", "fp", "fn", "tn")
                if (!all(required_cols %in% names(meta_data))) {
                    self$results$hsrocresults$setNote(
                        "missing",
                        .("Missing required columns for proportional-hazards SROC analysis")
                    )
                    return()
                }

                if (is.null(mada_data) || nrow(mada_data) == 0) {
                    self$results$hsrocresults$setNote(
                        "invalid",
                        .("Processed data unavailable for proportional-hazards SROC analysis")
                    )
                    return()
                }

                # Check if we have enough data
                if (nrow(mada_data) < 3) {
                    self$results$hsrocresults$setNote(
                        "toofew",
                        .("Proportional-hazards SROC analysis requires at least 3 studies")
                    )
                    return()
                }

                # Apply the SAME continuity-correction policy as the bivariate
                # model. mada::phm's library default is correction.control =
                # "all" (0.5 to every study when any zero cell exists), while
                # reitsma is called with "single" under the default option -
                # letting phm use its default meant the two sections of one
                # output were fitted to different data.
                correction_method <- self$options$zero_cell_correction %||% "none"
                phm_correction_control <- if (identical(correction_method, "none")) "single" else "all"
                # Check the data the model actually receives: when a user-chosen
                # correction is active, mada_data is already zero-free and
                # mada's own 0.5 correction never fires (the applied correction
                # is disclosed in the notices panel instead).
                zero_cells <- any(mada_data[, c("TP", "FP", "FN", "TN")] == 0,
                                  na.rm = TRUE)
                if (zero_cells) {
                    self$results$hsrocresults$setNote(
                        "zerocells",
                        .("Zero cells detected; a continuity correction of 0.5 was applied to the affected studies (matching the bivariate model), so results should be interpreted cautiously.")
                    )
                }

                # Fit the Holling proportional-hazards SROC model.
                #
                # mada::phm stops after `l` profile-likelihood iterations and only
                # WARNS ("Reached maximum number of iterations!"). The previous
                # handler caught that warning, refitted the same model with
                # suppressWarnings() and reported the result as if it had
                # converged. On a 6-study set the l = 100 answer was theta 0.056,
                # AUC 0.947, while the fit that actually converges (811
                # iterations) gives theta 1.574, AUC 0.389 - the difference
                # between an excellent test and a useless one. With realistic
                # high-sensitivity data the default limit is reached in roughly
                # 45% of meta-analyses, so this was not an edge case.
                #
                # Retry with a large budget, use the converged fit, and refuse to
                # present the numbers when even that does not converge.
                phm_fit_once <- function(iter_limit) {
                    hit_max <- FALSE
                    model <- withCallingHandlers(
                        mada::phm(mada_data, correction = 0.5,
                                  correction.control = phm_correction_control,
                                  l = iter_limit),
                        warning = function(w) {
                            msg <- conditionMessage(w)
                            if (grepl("maximum number of iterations", msg, fixed = TRUE)) {
                                hit_max <<- TRUE
                                invokeRestart("muffleWarning")
                            }
                            if (grepl("non zero decimal places", msg, fixed = TRUE))
                                invokeRestart("muffleWarning")
                        })
                    list(model = model, converged = !hit_max)
                }

                phm_iter_note <- NULL
                hsroc_model <- tryCatch({
                    fit <- phm_fit_once(100)
                    if (!is.null(fit$model) && !fit$converged) {
                        retry <- phm_fit_once(5000)
                        if (!is.null(retry$model)) fit <- retry
                        phm_iter_note <- if (isTRUE(fit$converged))
                            .("The proportional-hazards SROC model needed more than mada's default 100 profile-likelihood iterations; it was refitted with a limit of 5000 and converged, and the estimates below come from that converged fit.")
                        else
                            .("The proportional-hazards SROC model did NOT converge within 5000 profile-likelihood iterations. The values below are the last iterate, not a maximum-likelihood solution: they can change substantially with a different iteration limit and should not be reported.")
                        if (!isTRUE(fit$converged))
                            private$.addNotice("STRONG_WARNING", .("HSROC model did not converge"),
                                .("The proportional-hazards SROC (HSROC) model did not converge. Its theta, tau-squared and area under the curve are the last iterate of an unfinished optimisation and are not trustworthy - on data like this the unconverged value and the converged one have differed by an order of magnitude. Use the bivariate model and the SROC curve instead, or reduce the heterogeneity by restricting the study set."))
                    }
                    if (is.null(fit$model)) {
                        stop("Proportional-hazards SROC model fitting returned NULL")
                    }
                    fit$model
                }, error = function(e) {
                    self$results$hsrocresults$setNote(
                        "error",
                        sprintf(.("Proportional-hazards SROC fitting error: %s"), e$message)
                    )
                    return(NULL)
                })
                if (!is.null(hsroc_model) && !is.null(phm_iter_note))
                    self$results$hsrocresults$setNote("convergence", phm_iter_note)

                # Validate model object
                if (is.null(hsroc_model)) {
                    self$results$hsrocresults$setNote(
                        "failed",
                        .("Proportional-hazards SROC fitting failed; check data quality")
                    )
                    return()
                }

                # Get summary with error handling
                hsroc_summary <- tryCatch({
                    result <- summary(
                        hsroc_model,
                        level = private$.metaforLevel() / 100
                    )
                    if (is.null(result)) {
                        stop("Proportional-hazards SROC summary is NULL")
                    }
                    result
                }, error = function(e) {
                    self$results$hsrocresults$setNote(
                        "summary_error",
                        sprintf(.("Proportional-hazards SROC summary error: %s"), e$message)
                    )
                    return(NULL)
                })

                if (is.null(hsroc_summary)) {
                    return()
                }

                # Extract model parameters from the mada summary object.
                coefficients <- NULL
                tryCatch({
                    if (!is.null(hsroc_summary$object) && "coefficients" %in% names(hsroc_summary$object)) {
                        coefficients <- hsroc_summary$object$coefficients
                    } else if (!is.null(hsroc_summary) && "coefficients" %in% names(hsroc_summary)) {
                        coefficients <- hsroc_summary$coefficients
                    } else if (!is.null(hsroc_summary) && "coef" %in% names(hsroc_summary)) {
                        coefficients <- hsroc_summary$coef
                    }
                }, error = function(e) {
                    self$results$hsrocresults$setNote("coef_access_error", sprintf(.("Cannot access coefficients: %s"), e$message))
                })

                if (is.null(coefficients)) {
                    self$results$hsrocresults$setNote(
                        "no_coefficients",
                        .("Proportional-hazards SROC summary contains no coefficients")
                    )
                    return()
                }

                # Validate coefficient structure
                if (length(coefficients) == 0) {
                    self$results$hsrocresults$setNote(
                        "empty_coefficients",
                        .("Proportional-hazards SROC coefficients are empty")
                    )
                    return()
                }

                # Handle coefficients as named vector (typical for mada phm)
                if (is.vector(coefficients) && !is.null(names(coefficients))) {
                    # Define parameter labels
                    param_labels <- list(
                        "theta" = .("Diagnostic accuracy parameter (theta)"),
                        "taus_sq" = .("Between-study variance (tau^2)")
                    )

                    # Note: hsroc_table$deleteRows() was already called at the
                    # top of .performPHMSROCAnalysis(); no need to clear again.

                    # Process each coefficient
                    theta_se <- NA_real_
                    for (param_name in names(coefficients)) {
                        display_name <- param_labels[[param_name]] %||% param_name
                        estimate <- coefficients[param_name]

                        # Get variance/SE from vcov matrix if available
                        std_error <- NA_real_
                        z_value <- NA_real_
                        p_value <- NA_real_

                        if (!is.null(hsroc_summary$object$vcov)) {
                            vcov_matrix <- hsroc_summary$object$vcov
                
                            # Find the index of the parameter in the vcov matrix
                            param_idx <- which(names(coefficients) == param_name)

                            if (length(param_idx) > 0 && param_idx <= nrow(vcov_matrix)) {
                                variance <- vcov_matrix[param_idx, param_idx]
                                if (is.finite(variance) && variance > 0) {
                                    std_error <- sqrt(variance)
                                    if (is.finite(std_error) && std_error > 0 && is.finite(estimate)) {
                                        # The former z = estimate/SE tested H0:
                                        # parameter = 0, which is the wrong null
                                        # for BOTH parameters: for theta the
                                        # no-accuracy null is theta = 1 (sens =
                                        # FPR chance diagonal; theta -> 0 is a
                                        # PERFECT test), and a Wald test of the
                                        # variance taus_sq against 0 sits on the
                                        # boundary of its parameter space and is
                                        # invalid - so no p is shown for it.
                                        if (identical(param_name, "theta")) {
                                            z_value <- (estimate - 1) / std_error
                                            p_value <- 2 * (1 - stats::pnorm(abs(z_value)))
                                        }
                                    }
                                }
                            }
                        }

                        if (identical(param_name, "theta")) theta_se <- std_error

                        # setRow into the scaffolded row; addRow only if mada
                        # ever returns a coefficient we did not anticipate.
                        phm_set <- if (param_name %in% c("theta", "taus_sq"))
                            hsroc_table$setRow else hsroc_table$addRow
                        phm_set(rowKey = param_name, values = list(
                            parameter = display_name,
                            estimate = estimate,
                            std_error = std_error,
                            z_value = z_value,
                            p_value = p_value
                        ))
                    }

                    # The AUC of the PHM SROC curve is 1/(1 + theta) (Holling,
                    # Boehning & Boehning 2012). The note below quoted the formula
                    # but the value itself was never shown. SE by the delta
                    # method: d/dtheta [1/(1 + theta)] = -1/(1 + theta)^2.
                    theta_val <- if ("theta" %in% names(coefficients)) unname(coefficients["theta"]) else NA_real_
                    if (is.finite(theta_val) && theta_val > 0) {
                        auc_val <- 1 / (1 + theta_val)
                        hsroc_table$setRow(rowKey = "auc", values = list(
                            parameter = .("Area under the SROC curve (AUC = 1/(1 + theta))"),
                            estimate = auc_val,
                            std_error = if (is.finite(theta_se)) theta_se / (1 + theta_val)^2 else NA_real_,
                            z_value = NA_real_,
                            p_value = NA_real_
                        ))
                        if (auc_val < 0.5) {
                            # theta > 1 puts the Lehmann-family curve below the
                            # diagonal. That is this model failing to describe
                            # the data, not evidence the test is worse than
                            # chance and not a sign the columns were swapped -
                            # the bivariate table on the same screen will show a
                            # sensible pair. Telling the user to recheck their
                            # column assignment sends them after a bug that is
                            # not there, and the converged-fit retry routes
                            # ordinary high-sensitivity data here routinely.
                            private$.addNotice("STRONG_WARNING", .("Proportional-hazards SROC model does not fit"), sprintf(
                                .("The proportional-hazards SROC curve falls below the diagonal (area %.2f, below 0.5). This means the Lehmann family used by this model does not describe these studies; it is not evidence that the test performs worse than chance, and the pooled sensitivity and specificity above are unaffected. Use the bivariate model and its SROC curve instead. If the pooled pair above is also below chance, check the TP/FP/FN/TN column assignment."), auc_val))
                        } else if (auc_val < 0.7) {
                            private$.addNotice("STRONG_WARNING", .("Low SROC AUC"), sprintf(
                                .("The proportional-hazards SROC AUC is %.2f (< 0.70), indicating poor overall discrimination."), auc_val))
                        }
                    }

                    hsroc_table$setNote(
                        "method",
                        .("Holling proportional-hazards SROC model fitted with adjusted profile maximum likelihood. The p-value for theta tests H0: theta = 1 (accuracy no better than chance); theta < 1 indicates discrimination and theta near 0 a near-perfect test (AUC = 1/(1 + theta)). No p-value is shown for the between-study variance: a Wald test of a variance against zero is invalid at the boundary of its parameter space.")
                    )
                } else {
                    self$results$hsrocresults$setNote(
                        "unsupported_format",
                        .("Proportional-hazards SROC coefficient format is not supported")
                    )
                }
            }
        },
        
        .performHeterogeneityAnalysis = function(analysis_data) {

            if (!requireNamespace("metafor", quietly = TRUE)) {
                self$results$heterogeneity$setNote("package_error",
                    .("The metafor package is not available, so heterogeneity could not be assessed."))
                return()
            }

            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                self$results$heterogeneity$setNote("data_error",
                    .("Analysis data is missing or empty, so heterogeneity could not be assessed."))
                return()
            }

            {

                analysis_data$sens <- analysis_data$tp / (analysis_data$tp + analysis_data$fn)
                analysis_data$spec <- analysis_data$tn / (analysis_data$tn + analysis_data$fp)

                analysis_data$logit_sens <- stats::qlogis(analysis_data$sens)
                analysis_data$logit_spec <- stats::qlogis(analysis_data$spec)

                analysis_data$var_logit_sens <- 1 / analysis_data$tp + 1 / analysis_data$fn
                analysis_data$var_logit_spec <- 1 / analysis_data$tn + 1 / analysis_data$fp

                het_table <- self$results$heterogeneity
                # Blank, don't delete: the rows are scaffolded in .init(), so a
                # run that cannot fit a margin leaves empty cells rather than the
                # previous run's numbers (same idiom as the Deeks row).
                for (blank_key in c("sensitivity", "specificity"))
                    het_table$setRow(rowKey = blank_key, values = list(
                        q_statistic = NA_real_, df = NA_integer_, p_value = NA_real_,
                        i_squared = NA_real_, tau_squared = NA_real_))

                sens_valid <- is.finite(analysis_data$logit_sens) &
                    is.finite(analysis_data$var_logit_sens) &
                    analysis_data$var_logit_sens > 0
                spec_valid <- is.finite(analysis_data$logit_spec) &
                    is.finite(analysis_data$var_logit_spec) &
                    analysis_data$var_logit_spec > 0

                dropped_sens <- sum(!sens_valid)
                dropped_spec <- sum(!spec_valid)
                if (dropped_sens > 0 || dropped_spec > 0) {
                    het_table$setNote(
                        "nonfinite_rows",
                        sprintf(
                            .("Excluded from the univariate heterogeneity models (sensitivity: %d; specificity: %d): studies whose logit is infinite, i.e. those with a zero cell - the 100%% sensitivity or 100%% specificity studies. The bivariate pooled estimates above keep them, so the two rest on different numbers of studies. Choose a zero-cell correction to retain them here."),
                            dropped_sens,
                            dropped_spec
                        )
                    )
                }

                rma_method <- private$.metaforMethod(self$options$method)
                rma_level <- private$.metaforLevel()

                # `key` is a stable ASCII identifier, `measure` the translated
                # label - see the same split in .performMetaRegression.
                fit_heterogeneity <- function(data, measure, key) {
                    if (nrow(data) < 2) {
                        het_table$setNote(
                            paste0("insufficient_", key),
                            sprintf(.("At least two finite studies are required for %s"), measure)
                        )
                        return(NULL)
                    }

                    tryCatch(
                        metafor::rma(
                            yi = data$effect,
                            vi = data$variance,
                            method = rma_method,
                            level = rma_level
                        ),
                        error = function(e) {
                            het_table$setNote(
                                paste0("error_", key),
                                sprintf(if (identical(key, "sens"))
                                            .("The sensitivity heterogeneity model failed: %s")
                                        else
                                            .("The specificity heterogeneity model failed: %s"),
                                        e$message)
                            )
                            NULL
                        }
                    )
                }

                sens_meta <- fit_heterogeneity(
                    data.frame(
                        effect = analysis_data$logit_sens[sens_valid],
                        variance = analysis_data$var_logit_sens[sens_valid]
                    ),
                    .("Sensitivity"),
                    "sens"
                )
                spec_meta <- fit_heterogeneity(
                    data.frame(
                        effect = analysis_data$logit_spec[spec_valid],
                        variance = analysis_data$var_logit_spec[spec_valid]
                    ),
                    .("Specificity"),
                    "spec"
                )

                add_heterogeneity_row <- function(row_key, measure, model) {
                    if (is.null(model)) {
                        return()
                    }
                    # metafor's I2 is derived from the fitted tau2 (REML by
                    # default), so it matches the tau2 column and what a reader
                    # reproducing the fit in metafor will see. The former Higgins
                    # (Q - df)/Q value disagreed with both.
                    i_squared <- if (is.finite(model$I2)) {
                        model$I2
                    } else if (is.finite(model$QE) && model$QE > 0) {
                        max(0, (model$QE - (model$k - 1)) / model$QE * 100)
                    } else {
                        0
                    }
                    het_table$setRow(rowKey = row_key, values = list(
                        measure = measure,
                        q_statistic = model$QE,
                        df = model$k - 1,
                        p_value = model$QEp,
                        i_squared = i_squared,
                        tau_squared = model$tau2
                    ))

                    # Name the estimator. Each row is a SEPARATE univariate
                    # random-effects model on logit sensitivity or logit
                    # specificity; the bivariate table's note used to send readers
                    # here for "proper evaluation", which was contradictory since
                    # this is exactly the univariate I-squared that note disowns.
                    het_table$setNote("i2_meaning", paste(
                        if (identical(self$options$method, "fixed"))
                            .("Each row is a separate univariate FIXED-effect model on the logit of that measure, because that is the method selected; a fixed-effect model assumes there is no between-study variance, so tau-squared is 0 by construction and the I-squared beside it is not an estimate from that model.")
                        else
                            .("Each row is a separate univariate random-effects model on the logit of that measure."),
                        .("The I-squared is the proportion of variance in THAT margin alone that is not attributable to sampling error; it does not describe the bivariate model and should not be read as an overall heterogeneity figure (Cochrane DTA Handbook, chapter 10). In diagnostic accuracy meta-analysis a high I-squared is expected whenever studies used different positivity thresholds - it signals a threshold effect to be modelled by the SROC curve, not necessarily a defect."),
                        if (identical(self$options$method, "fixed"))
                            .("A significant Q or a large I-squared here contradicts the fixed-effect assumption: switch to REML.")
                        else
                            .("Use the prediction interval and the SROC prediction region for a model-consistent statement of between-study variability.")))
                }

                add_heterogeneity_row("sensitivity", .("Sensitivity"), sens_meta)
                add_heterogeneity_row("specificity", .("Specificity"), spec_meta)
                het_table$setNote(
                    "method",
                    sprintf(.("Univariate auxiliary models used %s estimation."),
                            private$.methodTitle(self$options$method))
                )
            }
        },
        
        .performMetaRegression = function(meta_data, analysis_data) {

            # Clear first, before any early return, so a run that bails out
            # cannot leave rows from a previous configuration on screen.
            self$results$metaregression$deleteRows()

            if (!requireNamespace("metafor", quietly = TRUE)) {
                self$results$metaregression$setNote("package_error",
                    .("The metafor package is not available, so meta-regression could not be run."))
                return()
            }

            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                self$results$metaregression$setNote("data_error",
                    .("Analysis data is missing or empty, so meta-regression could not be run."))
                return()
            }

            covariate_var <- self$options$covariate
            if (is.null(covariate_var)) {
                # Add message when meta-regression is enabled but no covariate is selected
                if (isTRUE(self$options$meta_regression)) {
                    private$.addNotice("INFO", .("Meta-regression requires a covariate"),
                        .("To perform meta-regression analysis, please select a covariate variable (e.g., study year, population type, method) that may explain heterogeneity between studies. The covariate should be a study-level characteristic that varies across included studies."))
                }
                return()
            }

            covariate_values <- self$data[[covariate_var]]
            if (is.null(covariate_values)) return()

            if (!"row_id" %in% names(meta_data)) {
                private$.addNotice("WARNING", .("Meta-regression skipped"),
                    .("Row identifiers were not preserved during preprocessing, so the covariate could not be aligned with the filtered studies."))
                return()
            }

            analysis_data$covariate <- covariate_values[meta_data$row_id]
            # A study with no covariate value silently left the meta-regression
            # while the pooled estimates above still used it, so the two tables
            # described different study sets with nothing saying so.
            n_before_covariate <- nrow(analysis_data)
            analysis_data <- analysis_data[!is.na(analysis_data$covariate), , drop = FALSE]
            if (nrow(analysis_data) < n_before_covariate)
                self$results$metaregression$setNote("covariate_missing", sprintf(
                    .("%d of %d studies were excluded from the meta-regression because the covariate is missing; the pooled estimates above use all %d."),
                    n_before_covariate - nrow(analysis_data), n_before_covariate, n_before_covariate))

            if (nrow(analysis_data) < 3) {
                # SERIALIZATION FIX: Use table note instead of inserting Notice
                self$results$metaregression$setNote("insufficient_data",
                    .("Meta-regression not run: fewer than three studies remain after removing missing covariate values."))
                return()
            }

            # The degrees-of-freedom arithmetic belongs with the model that is
            # actually fitted, not with this set. The sensitivity and specificity
            # models each drop their own zero-cell studies further down, so a
            # guard computed here passed a set of 7 studies with 4 residual df
            # while the sensitivity model ran on 3 studies of a 3-level factor -
            # no residual df at all - and metafor's raw text ("Number of
            # parameters to be estimated is larger than the number of
            # observations") reached the user as a notice. The per-margin check
            # lives inside fit_meta_regression(), below.
            if (length(unique(stats::na.omit(as.character(analysis_data$covariate)))) < 2) {
                self$results$metaregression$setNote("constant_covariate",
                    .("Meta-regression not run: the covariate takes the same value in every study, so it cannot explain any between-study variation."))
                return()
            }

            if (requireNamespace("metafor", quietly = TRUE)) {

                analysis_data$sens <- analysis_data$tp / (analysis_data$tp + analysis_data$fn)
                analysis_data$spec <- analysis_data$tn / (analysis_data$tn + analysis_data$fp)

                analysis_data$logit_sens <- stats::qlogis(analysis_data$sens)
                analysis_data$logit_spec <- stats::qlogis(analysis_data$spec)

                analysis_data$var_logit_sens <- 1 / analysis_data$tp + 1 / analysis_data$fn
                analysis_data$var_logit_spec <- 1 / analysis_data$tn + 1 / analysis_data$fp

                metareg_table <- self$results$metaregression

                sens_valid <- is.finite(analysis_data$logit_sens) &
                    is.finite(analysis_data$var_logit_sens) &
                    analysis_data$var_logit_sens > 0
                spec_valid <- is.finite(analysis_data$logit_spec) &
                    is.finite(analysis_data$var_logit_spec) &
                    analysis_data$var_logit_spec > 0
                sens_data <- analysis_data[sens_valid, , drop = FALSE]
                spec_data <- analysis_data[spec_valid, , drop = FALSE]

                dropped_sens <- sum(!sens_valid)
                dropped_spec <- sum(!spec_valid)
                if (dropped_sens > 0 || dropped_spec > 0) {
                    metareg_table$setNote(
                        "nonfinite_rows",
                        sprintf(
                            .("Excluded from the meta-regression (sensitivity: %d; specificity: %d): studies whose logit is infinite, i.e. those with a zero cell - the 100%% sensitivity or 100%% specificity studies. Choose a zero-cell correction to retain them."),
                            dropped_sens,
                            dropped_spec
                        )
                    )
                }

                rma_method <- private$.metaforMethod(self$options$method)
                rma_level <- private$.metaforLevel()

                # `key` is a stable ASCII identifier; `measure` is the
                # translated label that goes in the message. Building the note
                # key out of the label made note identity depend on the
                # catalogue - in a Turkish session the keys became
                # "insufficient_duyarlilik" and friends.
                fit_meta_regression <- function(data, effect, variance, measure, key) {
                    if (nrow(data) < 3) {
                        metareg_table$setNote(
                            paste0("insufficient_", key),
                            sprintf(.("At least three finite studies are required for %s meta-regression."), measure)
                        )
                        return(NULL)
                    }

                    # Per-margin degrees of freedom: a categorical covariate
                    # contributes (levels - 1) parameters, and both the level
                    # count and the study count are those of THIS margin after
                    # its zero-cell studies were dropped.
                    cov_levels <- unique(stats::na.omit(as.character(data$covariate)))
                    n_param <- if (is.factor(data$covariate) || is.character(data$covariate))
                        max(length(cov_levels) - 1, 1) else 1
                    resid_df <- nrow(data) - n_param - 1

                    if (length(cov_levels) < 2) {
                        metareg_table$setNote(paste0("constant_", key), sprintf(
                            .("%s meta-regression not run: after excluding studies this model cannot use, the covariate takes the same value in every remaining study."),
                            measure))
                        return(NULL)
                    }
                    if (resid_df < 1) {
                        metareg_table$setNote(paste0("overparameterised_", key), sprintf(
                            .("%s meta-regression not run: the %d studies this model can use cannot support a covariate contributing %d model parameter(s) - no residual degrees of freedom remain, so the model would fit perfectly and its confidence intervals would be meaningless. At least %d studies are needed, and 10 per covariate is the usual recommendation."),
                            measure, nrow(data), n_param, n_param + 2))
                        return(NULL)
                    }
                    if (nrow(data) < 10 * n_param) {
                        metareg_table$setNote(paste0("small_sample_", key), sprintf(
                            .("%s meta-regression on %d studies with a covariate contributing %d model parameter(s) (%d residual degrees of freedom). The usual recommendation is at least 10 studies per covariate, so estimates may be unstable and confidence intervals unreliable. Report this as exploratory only."),
                            measure, nrow(data), n_param, resid_df))
                    }

                    tryCatch(
                        metafor::rma(
                            yi = data[[effect]],
                            vi = data[[variance]],
                            mods = ~ covariate,
                            data = data,
                            method = rma_method,
                            level = rma_level,
                            # Knapp-Hartung adjustment: t-based inference is
                            # materially less anticonservative than Wald z at
                            # the small k typical of DTA meta-regression. Not
                            # defined for fixed-effect models.
                            test = if (identical(rma_method, "FE")) "z" else "knha"
                        ),
                        error = function(e) {
                            # The bare library message used to be the whole
                            # notice ("Number of parameters to be estimated is
                            # larger than the number of observations"), with no
                            # statement of what is missing or what to do. It can
                            # also carry newlines, which a single-line notice
                            # must not. Frame it, as every sibling error path does.
                            private$.addNotice("WARNING",
                                if (identical(key, "sens"))
                                    .("Sensitivity meta-regression failed")
                                else
                                    .("Specificity meta-regression failed"),
                                sprintf(
                                    .("The %s meta-regression model could not be fitted, so no covariate effect is reported for that measure; the pooled estimates are unaffected. The underlying error was: %s"),
                                    measure, gsub("[\r\n]+", " ", trimws(conditionMessage(e)))))
                            NULL
                        }
                    )
                }

                sens_metareg <- fit_meta_regression(
                    sens_data,
                    "logit_sens",
                    "var_logit_sens",
                    .("Sensitivity"),
                    "sens"
                )
                spec_metareg <- fit_meta_regression(
                    spec_data,
                    "logit_spec",
                    "var_logit_spec",
                    .("Specificity"),
                    "spec"
                )
                is_fe <- identical(rma_method, "FE")
                # Both margins can now bail out with their own note (the
                # per-margin degrees-of-freedom guard returns NULL rather than
                # aborting the caller), and an empty table saying models "were
                # fitted" contradicts the two notes beside it saying why they
                # were not.
                if (!is.null(sens_metareg) || !is.null(spec_metareg))
                metareg_table$setNote(
                    "method",
                    sprintf(
                        if (is_fe)
                            .("Two separate UNIVARIATE meta-regression models were fitted (logit sensitivity and logit specificity), not a joint bivariate meta-regression, using %s estimation.")
                        else
                            .("Two separate UNIVARIATE meta-regression models were fitted (logit sensitivity and logit specificity), not a joint bivariate meta-regression, using %s estimation with the Knapp-Hartung adjustment (test statistics are t, not z)."),
                        private$.methodTitle(self$options$method)
                    )
                )

                # Omnibus (QM) test of the covariate effect: with a multi-level
                # factor covariate the per-contrast rows do not answer "does the
                # covariate explain heterogeneity at all".
                describe_omnibus <- function(model, measure) {
                    if (is.null(model) || is.null(model$QM) || !is.finite(model$QM))
                        return(NULL)
                    if (identical(model$test, "knha") || identical(model$test, "t")) {
                        sprintf(.("%s: F(%d, %.0f) = %.2f, p = %.4g"), measure,
                                model$QMdf[1], model$QMdf[2], model$QM, model$QMp)
                    } else {
                        sprintf(.("%s: QM(%d) = %.2f, p = %.4g"), measure,
                                model$QMdf[1], model$QM, model$QMp)
                    }
                }

                # jamovi table cells are plain text, so the covariate name is
                # shown as-is; escaping here rendered "Stain & Method" as
                # "Stain &amp; Method".
                safe_covariate_label <- covariate_var

                # Emit one row per non-intercept coefficient. A categorical
                # covariate with k > 2 levels produces k - 1 contrasts, so
                # iterate over 2:length(beta) instead of reporting only beta[2].
                # Label each row with the metafor coefficient name
                # (rownames(model$beta)); fall back to the covariate name.
                add_metareg_rows <- function(model, measure, key_prefix) {
                    if (is.null(model)) {
                        return()
                    }
                    metareg_table$addRow(
                        rowKey = paste0(key_prefix, "_intercept"),
                        values = list(
                            measure = measure,
                            parameter = .("Intercept"),
                            estimate = model$beta[1],
                            std_error = model$se[1],
                            # metafor already computes these at rma(level=);
                            # the table showed an estimate and an SE and left
                            # the reader to combine them with the right
                            # reference distribution (t under Knapp-Hartung).
                            ci_lower = model$ci.lb[1],
                            ci_upper = model$ci.ub[1],
                            z_value = model$zval[1],
                            p_value = model$pval[1]
                        )
                    )

                    n_coef <- length(model$beta)
                    if (n_coef < 2) {
                        return()
                    }

                    beta_names <- rownames(model$beta)
                    for (j in 2:n_coef) {
                        # Show the user's variable name. The model is fitted on an
                        # internal column literally named `covariate`, so
                        # rownames(model$beta) are "covariate", "covariateB", ...
                        # and the table said "covariate" instead of the variable
                        # the user actually chose.
                        coef_label <- if (!is.null(beta_names) &&
                            length(beta_names) >= j &&
                            !is.na(beta_names[j]) &&
                            nzchar(beta_names[j])) {
                            sub("^covariate", paste0(covariate_var, " "), beta_names[j])
                        } else {
                            safe_covariate_label
                        }
                        coef_label <- trimws(coef_label)
                        metareg_table$addRow(
                            rowKey = paste0(key_prefix, "_covariate_", j),
                            values = list(
                                measure = measure,
                                parameter = coef_label,
                                estimate = model$beta[j],
                                std_error = model$se[j],
                                ci_lower = model$ci.lb[j],
                                ci_upper = model$ci.ub[j],
                                z_value = model$zval[j],
                                p_value = model$pval[j]
                            )
                        )
                    }
                }

                add_metareg_rows(sens_metareg, .("Sensitivity"), "sens")
                add_metareg_rows(spec_metareg, .("Specificity"), "spec")

                # The table reports coefficients on the LOGIT scale with no
                # label, so "Intercept 95.26" read like a percentage. For a
                # categorical covariate the reference level was never named, so
                # a row labelled "grade MRI  -0.037" had no stated comparator.
                cov_levels_note <- if (is.factor(analysis_data$covariate) ||
                                       is.character(analysis_data$covariate)) {
                    lv <- sort(unique(stats::na.omit(as.character(analysis_data$covariate))))
                    sprintf(.("Each coefficient contrasts that level of %s against the reference level '%s'."),
                            covariate_var, lv[1])
                } else {
                    sprintf(.("The coefficient is the change per one-unit increase in %s."), covariate_var)
                }
                metareg_table$setNote("scale", paste(
                    .("Estimates, standard errors and confidence intervals are on the LOGIT scale, not the percentage scale used by the pooled results above: a coefficient of 0 means no association, and the intercept is the logit of the measure at the covariate's reference point, not a sensitivity or specificity."),
                    cov_levels_note))

                omnibus_txt <- c(describe_omnibus(sens_metareg, .("sensitivity")),
                                 describe_omnibus(spec_metareg, .("specificity")))
                if (length(omnibus_txt) > 0) {
                    metareg_table$setNote("omnibus", sprintf(
                        .("Omnibus test of the covariate effect - %s."),
                        paste(omnibus_txt, collapse = "; ")))
                }
            }
        },
        
        .performPublicationBiasAssessment = function(analysis_data) {

            # Blank first, before any early return. The Deeks row is scaffolded in
            # .init(), so deleteRows() would remove it; a run that cannot assess
            # bias leaves empty cells, not the previous run's result.
            self$results$publicationbias$setRow(rowKey = "deeks_test", values = list(
                statistic = NA_real_, p_value = NA_real_, interpretation = NA_character_))

            if (!requireNamespace("metafor", quietly = TRUE)) {
                self$results$publicationbias$setNote("package_error",
                    .("The metafor package is not available, so publication bias could not be assessed."))
                return()
            }

            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                self$results$publicationbias$setNote("data_error",
                    .("Analysis data is missing or empty, so publication bias could not be assessed."))
                return()
            }

            {

                if (nrow(analysis_data) < 10) {
                    private$.addNotice("WARNING", .("Publication bias caution"), sprintf(
                        .("Deeks' test is unreliable with fewer than 10 studies (k = %d here); treat the asymmetry result as descriptive only."),
                        nrow(analysis_data)))
                }

                # Effective sample size for Deeks' test (Deeks, Macaskill & Irwig
                # 2005, J Clin Epidemiol 58:882-93):
                #
                #     ESS = 4 * n1 * n0 / (n1 + n0)
                #
                # where n1 = TP + FN (diseased) and n0 = FP + TN (non-diseased) -
                # i.e. twice the harmonic mean of the two GROUP sizes.
                #
                # This previously used 4 / (1/TP + 1/FN + 1/FP + 1/TN), the
                # harmonic mean of the four CELL counts, which is a different and
                # non-monotone function of the table: two studies with identical
                # ESS under Deeks' definition can get different values here, and
                # the ranking of studies by ESS changes (Spearman ~0.3 on a
                # realistic set). Since 1/sqrt(ESS) is the regression predictor,
                # the asymmetry test statistic is materially wrong - on a 10-study
                # example the correct test gives z = -8.25, p < 0.0001 while this
                # gave z = +1.58, p = 0.11, i.e. "No significant asymmetry"
                # reported for strongly asymmetric data, with the sign reversed.
                n_diseased <- analysis_data$tp + analysis_data$fn
                n_healthy  <- analysis_data$fp + analysis_data$tn
                analysis_data$ess <- 4 * n_diseased * n_healthy / (n_diseased + n_healthy)

                # Deeks' test needs finite log DOR, so a zero cell must be handled
                # here regardless of the user's model-level choice. With the
                # default ("none") a single zero made log_dor and se_log_dor
                # infinite and the whole test returned NaN with no explanation.
                #
                # The correction goes on EVERY study, not only the zero-cell ones.
                # Correcting only the studies that need it shrinks their log DOR
                # while leaving the rest untouched, and the studies that need it
                # are the small, near-perfect ones - exactly the low-ESS end of
                # the regression. That builds a size-related trend into the test's
                # own outcome variable. With no publication bias whatsoever (every
                # simulated study kept), 400-run null simulations gave p < 0.05 in
                # 12% of meta-analyses at 39% zero-cell studies and 60% at 80%,
                # against a nominal 5%; the uniform correction brings those to 7%
                # and 42% (Sweeting, Sutton & Lambert 2004, Stat Med 23:1351-75,
                # make the same point for odds ratios).
                #
                # 42% is still not a test, which is why the verdict below is
                # withheld once more than a quarter of the studies were corrected:
                # the simulations stay at or under the nominal rate up to about
                # that share and deteriorate past it.
                cells <- c("tp", "fp", "fn", "tn")
                zero_rows <- rowSums(analysis_data[, cells] == 0) > 0
                n_zero <- sum(zero_rows)
                if (n_zero > 0) {
                    analysis_data[, cells] <- analysis_data[, cells] + 0.5
                }

                analysis_data$log_dor <- log((analysis_data$tp * analysis_data$tn) /
                                              (analysis_data$fp * analysis_data$fn))
                analysis_data$inv_root_ess <- 1 / sqrt(analysis_data$ess)

                # Deeks, Macaskill & Irwig (2005, J Clin Epidemiol 58:882-93):
                # regress log DOR on 1/sqrt(ESS), WEIGHTED BY ESS, and refer the
                # slope to a t distribution on k - 2 df.
                #
                # This previously used metafor::rma(vi = se_log_dor^2, method =
                # "FE") and reported a z. Inverse-variance weighting on
                # var(log DOR) = 1/TP+1/FP+1/FN+1/TN reintroduces exactly the
                # log-DOR/SE correlation Deeks' method exists to avoid - the same
                # defect as using Egger's, moved from the predictor into the
                # weights - and a normal reference is anticonservative at the
                # small k typical of DTA meta-analyses. On a 4-study example the
                # old form reported p = 0.0014 "Significant asymmetry" where
                # Deeks' own specification gives p = 0.26.
                ok_rows <- is.finite(analysis_data$log_dor) &
                           is.finite(analysis_data$inv_root_ess) &
                           is.finite(analysis_data$ess) & analysis_data$ess > 0
                fit_data <- analysis_data[ok_rows, , drop = FALSE]

                bias_table <- self$results$publicationbias

                if (nrow(fit_data) < 3) {
                    bias_table$setRow(rowKey = "deeks_test", values = list(
                        statistic = NA_real_, p_value = NA_real_,
                        interpretation = .("Not estimable: at least 3 studies with finite odds ratios are required")
                    ))
                } else {
                    deeks_fit <- stats::lm(log_dor ~ inv_root_ess,
                                           data = fit_data, weights = fit_data$ess)
                    cf <- summary(deeks_fit)$coefficients
                    if (nrow(cf) < 2) {
                        # Every study has the same effective sample size - six
                        # algorithms read on one set of slides, for instance - so
                        # the predictor is constant, the slope is aliased and the
                        # fit returns an intercept row only. cf[2, 3] then threw
                        # "subscript out of bounds" into the notes as a raw error.
                        bias_table$setRow(rowKey = "deeks_test", values = list(
                            statistic = NA_real_, p_value = NA_real_,
                            interpretation = .("Not estimable: all studies have the same effective sample size")))
                        bias_table$setNote("deeks_method",
                            .("Deeks' test regresses the log diagnostic odds ratio on 1/sqrt(effective sample size). Every study here has the same effective sample size, so there is no variation in study size to relate accuracy to and funnel-plot asymmetry cannot be assessed. This is expected when the studies are different tests or readers applied to one common sample."))
                        return()
                    }
                    deeks_t <- cf[2, 3]
                    deeks_p <- cf[2, 4]
                    deeks_df <- stats::df.residual(deeks_fit)

                    # Share of studies whose zero cell forced the correction.
                    zero_share <- n_zero / nrow(analysis_data)
                    too_many_zeros <- zero_share > 0.25

                    bias_table$setRow(rowKey = "deeks_test", values = list(
                        statistic = deeks_t,
                        p_value = deeks_p,
                        interpretation = if (too_many_zeros)
                            .("Not interpretable: too many zero cells")
                        else if (deeks_p < 0.05)
                            .("Asymmetry detected (p < 0.05)")
                        else
                            .("No asymmetry detected (p >= 0.05)")
                    ))

                    note <- sprintf(
                        .("Deeks' test: log diagnostic odds ratio regressed on 1/sqrt(effective sample size), weighted by effective sample size; slope referred to t on %d df (Deeks, Macaskill & Irwig 2005). The verdict uses alpha = 0.05; some implementations use 0.10 for asymmetry tests, so a p-value between the two is borderline either way. Asymmetry is not by itself evidence of publication bias - it can also arise from between-study heterogeneity, a threshold effect, or a genuine relation between study size and accuracy (Cochrane DTA Handbook, chapter 10)."), deeks_df)
                    if (n_zero > 0) {
                        note <- paste(note, sprintf(
                            .("A continuity correction of 0.5 was applied to every study so that the %d study/studies with a zero cell had a finite odds ratio; correcting only those studies would itself tilt the test."),
                            n_zero))
                    }
                    if (too_many_zeros) {
                        note <- paste(note, sprintf(
                            .("No verdict is given: %.0f%% of the studies have a zero cell, and at that share the continuity correction alone produces significant asymmetry far more often than 5%% of the time even when no study is missing. Read the statistic as descriptive only."),
                            100 * zero_share))
                    } else if (deeks_p < 0.05 && nrow(fit_data) < 10) {
                        note <- paste(note, sprintf(
                            .("With only %d studies the test is fragile: a single small study can drive the slope, so confirm the asymmetry visually on the funnel plot before acting on it."),
                            nrow(fit_data)))
                    } else if (deeks_p >= 0.05) {
                        note <- paste(note, if (nrow(fit_data) < 10) sprintf(
                            .("With only %d studies the test is underpowered (at least 10 are usually required), so a non-significant result does not establish that publication bias is absent."),
                            nrow(fit_data)) else
                            .("All asymmetry tests have low power, particularly when the diagnostic odds ratio is heterogeneous, so a non-significant result does not establish that publication bias is absent."))
                    }
                    bias_table$setNote("deeks_method", note)
                }
            }
        },
        
        .populateForestPlot = function(meta_data) {

            image <- self$results$forestplot
            # SERIALIZATION FIX: Ensure it's a plain data frame
            if (!is.null(meta_data) && is.data.frame(meta_data)) {
                meta_data <- as.data.frame(meta_data, stringsAsFactors = FALSE)
                # Remove any attributes that might contain functions
                attributes(meta_data) <- attributes(meta_data)[c("names", "row.names", "class")]
            }

            # RELOAD FIX: serialize the pooled sensitivity/specificity point and
            # their confidence intervals into the image state (mirroring
            # .populateSROCPlot) so the pooled diamond survives an .omv reload.
            # Reading private$.pooled_* / private$.biv_model at render time
            # returned NULL after a saved file was reopened without re-running.
            pooled_sens <- private$.pooled_sensitivity
            pooled_spec <- private$.pooled_specificity
            pooled_sens_ci <- c(NA_real_, NA_real_)
            pooled_spec_ci <- c(NA_real_, NA_real_)

            conf_level <- self$options$confidence_level
            if (is.null(conf_level) || length(conf_level) == 0 || !is.finite(conf_level)) {
                conf_level <- 95
            }
            conf_level <- min(max(conf_level, 50), 99) / 100

            if (!is.null(private$.biv_model)) {
                tryCatch({
                    summary_results <- summary(private$.biv_model, level = conf_level)
                    coefficients <- summary_results$coefficients

                    # CI FIX: build the Wald interval on the LOGIT scale using the
                    # tsens/tfpr intercept rows (estimate + SE both on link scale)
                    # and transform with plogis() exactly ONCE. The probability-
                    # scale rows ('sensitivity'/'false pos. rate') are already in
                    # [0, 1] and often carry NA SE, so applying plogis() to them
                    # double-squashed the interval.
                    get_logit_ci <- function(param_name) {
                        if (is.null(rownames(coefficients))) return(c(NA_real_, NA_real_))
                        idx <- which(rownames(coefficients) == param_name)
                        if (length(idx) == 0) return(c(NA_real_, NA_real_))
                        estimate <- coefficients[idx[1], 1]
                        se <- coefficients[idx[1], 2]
                        if (!is.finite(estimate) || !is.finite(se)) return(c(NA_real_, NA_real_))
                        z <- stats::qnorm(1 - (1 - conf_level) / 2)
                        c(estimate - z * se, estimate + z * se)
                    }

                    sens_ci_logit <- get_logit_ci("tsens.(Intercept)")
                    if (!any(is.na(sens_ci_logit))) {
                        pooled_sens_ci <- stats::plogis(sens_ci_logit)
                    }

                    # logit(spec) = -tfpr, so specificity CI is the reversed
                    # complement of the FPR interval to keep lower < upper.
                    fpr_ci_logit <- get_logit_ci("tfpr.(Intercept)")
                    if (!any(is.na(fpr_ci_logit))) {
                        pooled_spec_ci <- rev(1 - stats::plogis(fpr_ci_logit))
                    }
                }, error = function(e) {
                    # Silently fall back to NA CIs if extraction fails
                })
            }

            # summary() on a fixed-effect reitsma fit errors, so the block above
            # left both intervals NA and the forest plot drew a pooled diamond
            # with no error bars while the table beside it showed a confidence
            # interval. The table's own interval is already stored, on the
            # percentage scale - fall back to it so plot and table always agree.
            if (any(is.na(pooled_sens_ci)) && length(private$.pooled_sens_ci) == 2 &&
                all(is.finite(private$.pooled_sens_ci)))
                pooled_sens_ci <- private$.pooled_sens_ci / 100
            if (any(is.na(pooled_spec_ci)) && length(private$.pooled_spec_ci) == 2 &&
                all(is.finite(private$.pooled_spec_ci)))
                pooled_spec_ci <- private$.pooled_spec_ci / 100

            plot_state <- list(
                data = meta_data,
                pooled_sens = if (!is.null(pooled_sens)) as.numeric(pooled_sens) else NA_real_,
                pooled_spec = if (!is.null(pooled_spec)) as.numeric(pooled_spec) else NA_real_,
                pooled_sens_ci = pooled_sens_ci,
                pooled_spec_ci = pooled_spec_ci
            )

            image$setState(plot_state)
        },
        
        .forestplot = function(image, ggtheme, theme, ...) {

            state <- image$state

            if (is.null(state))
                return(FALSE)

            # Unpack state. New state is a list carrying the study data plus the
            # serialized pooled point and CIs. Legacy states (older saved .omv
            # files) stored only the data frame - fall back to private fields,
            # which are populated whenever .run() has just executed.
            pooled_sens <- NULL
            pooled_spec <- NULL
            pooled_sens_ci <- c(NA_real_, NA_real_)
            pooled_spec_ci <- c(NA_real_, NA_real_)

            if (is.data.frame(state)) {
                meta_data <- state
                pooled_sens <- private$.pooled_sensitivity
                pooled_spec <- private$.pooled_specificity
            } else if (is.list(state)) {
                meta_data <- state$data
                pooled_sens <- state$pooled_sens
                pooled_spec <- state$pooled_spec
                if (!is.null(state$pooled_sens_ci)) pooled_sens_ci <- state$pooled_sens_ci
                if (!is.null(state$pooled_spec_ci)) pooled_spec_ci <- state$pooled_spec_ci
            } else {
                return(FALSE)
            }

            # Validate meta_data
            if (is.null(meta_data) || !is.data.frame(meta_data) || nrow(meta_data) == 0) {
                return(FALSE)
            }

            if (requireNamespace("ggplot2", quietly = TRUE)) {

                # Ensure meta_data is a proper data frame
                meta_data <- as.data.frame(meta_data)
                
                # Calculate sens and spec with CIs
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
                
                conf_level <- self$options$confidence_level
                if (is.null(conf_level) || length(conf_level) == 0 || !is.finite(conf_level)) {
                    conf_level <- 95
                }
                conf_level <- min(max(conf_level, 50), 99) / 100
                alpha <- 1 - conf_level
                z_crit <- stats::qnorm(1 - alpha / 2)

                # Wilson score interval (proper CI for proportions, not Wald)
                # More accurate for extreme proportions near 0 or 1
                wilson_ci <- function(x, n, z) private$.wilsonCI(x, n, z)

                # Calculate Wilson CIs for sensitivity
                meta_data$sens_ci_lower <- NA_real_
                meta_data$sens_ci_upper <- NA_real_
                for (i in seq_len(nrow(meta_data))) {
                    ci <- wilson_ci(meta_data$tp[i], meta_data$tp[i] + meta_data$fn[i], z_crit)
                    meta_data$sens_ci_lower[i] <- ci[1]
                    meta_data$sens_ci_upper[i] <- ci[2]
                }

                # Calculate Wilson CIs for specificity
                meta_data$spec_ci_lower <- NA_real_
                meta_data$spec_ci_upper <- NA_real_
                for (i in seq_len(nrow(meta_data))) {
                    ci <- wilson_ci(meta_data$tn[i], meta_data$tn[i] + meta_data$fp[i], z_crit)
                    meta_data$spec_ci_lower[i] <- ci[1]
                    meta_data$spec_ci_upper[i] <- ci[2]
                }

                # Reshape data to long format for faceting
                sens_data <- meta_data[, c("study", "sens", "sens_ci_lower", "sens_ci_upper")]
                sens_data$metric <- .("Sensitivity")
                colnames(sens_data) <- c("study", "estimate", "ci_lower", "ci_upper", "metric")

                spec_data <- meta_data[, c("study", "spec", "spec_ci_lower", "spec_ci_upper")]
                spec_data$metric <- .("Specificity")
                colnames(spec_data) <- c("study", "estimate", "ci_lower", "ci_upper", "metric")

                plot_data <- rbind(sens_data, spec_data)

                # Get color palette for accessibility
                colors <- private$.getColorPalette(theme)

                # Add pooled estimates as diamond (standard meta-analysis
                # convention). Pooled point + CIs come from the serialized state
                # (see .populateForestPlot), so they persist across .omv reloads.
                pooled_data <- NULL
                pooled_label <- .("POOLED ESTIMATE")
                if (!is.null(pooled_sens) && !is.null(pooled_spec) &&
                    is.finite(pooled_sens) && is.finite(pooled_spec)) {

                    # Create pooled estimate rows
                    pooled_sens_row <- data.frame(
                        study = pooled_label,
                        estimate = pooled_sens,
                        ci_lower = pooled_sens_ci[1],
                        ci_upper = pooled_sens_ci[2],
                        metric = .("Sensitivity"),
                        stringsAsFactors = FALSE
                    )

                    pooled_spec_row <- data.frame(
                        study = pooled_label,
                        estimate = pooled_spec,
                        ci_lower = pooled_spec_ci[1],
                        ci_upper = pooled_spec_ci[2],
                        metric = .("Specificity"),
                        stringsAsFactors = FALSE
                    )

                    pooled_data <- rbind(pooled_sens_row, pooled_spec_row)
                }

                # Create forest plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = estimate, y = reorder(study, estimate))) +
                    ggplot2::geom_point(size = 3, color = colors$primary) +
                    # geom_errorbarh() is deprecated in ggplot2 4.0; the y
                    # orientation of geom_errorbar() draws the same bars.
                    ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                                           width = 0.2, orientation = "y", color = colors$primary) +
                    ggplot2::facet_wrap(~ metric) +
                    ggplot2::labs(
                        title = .("Forest Plot: Sensitivity and Specificity by Study"),
                        x = .("Estimate"),
                        y = .("Study")
                    ) +
                    ggplot2::xlim(0, 1) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10),
                        panel.spacing = ggplot2::unit(2, "lines")
                    )

                # Add pooled estimate diamond (standard meta-analysis convention)
                if (!is.null(pooled_data)) {
                    # Create diamond shape data for each metric
                    # Draw the pooled confidence interval as a horizontal bar,
                    # matching how the individual studies are drawn.
                    #
                    # This was a geom_polygon "diamond" whose four vertices were
                    # all given the SAME discrete y ("POOLED ESTIMATE"); the
                    # y_numeric half-height column was computed but never mapped
                    # in aes(). The polygon therefore had zero area and the pooled
                    # estimate appeared with no visible confidence interval at all
                    # - the one value on the plot a reader most needs the
                    # uncertainty for.
                    p <- p +
                        ggplot2::geom_errorbar(
                            data = pooled_data,
                            ggplot2::aes(xmin = ci_lower, xmax = ci_upper, y = pooled_label),
                            width = 0.35, orientation = "y",
                            linewidth = 1.1,
                            color = colors$secondary,
                            inherit.aes = FALSE
                        ) +
                        ggplot2::geom_point(
                            data = pooled_data,
                            ggplot2::aes(x = estimate, y = pooled_label),
                            size = 4,
                            color = colors$secondary,
                            shape = 18,  # Diamond shape
                            inherit.aes = FALSE
                        )
                }
                
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateSROCPlot = function(meta_data) {

            image <- self$results$srocplot

            # Check if bivariate model is available. The SROC pooled summary
            # point is derived from the bivariate (Reitsma) model, so without it
            # the plot would render blank with no explanation. Carry a message in
            # the state so .srocplot can draw an informative panel instead.
            if (is.null(private$.biv_model)) {
                image$setState(list(
                    message = .("The Summary ROC plot requires the bivariate random-effects (Reitsma) model. Enable 'Bivariate Analysis' to display the pooled summary point and study estimates.")
                ))
                return()
            }

            # CRITICAL FIX: Extract only serializable data from model
            # Do NOT store the model object itself (contains non-serializable functions)
            biv_model <- private$.biv_model
            # mada 0.5.12: summary.reitsma() errors for method = "fixed" (the
            # bivariate table reconstructs its rows for the same reason). Only
            # the pooled point is needed here, so fall back to the fitted
            # logit coefficients instead of aborting the whole analysis.
            summary_results <- tryCatch(summary(biv_model), error = function(e) NULL)
            coefficients <- if (!is.null(summary_results)) summary_results$coefficients else NULL

            # Helper to safely get coefficient by name
            get_coef <- function(target) {
                if (is.null(rownames(coefficients))) return(NULL)
                idx <- which(rownames(coefficients) == target)
                if (length(idx) == 0) idx <- which(tolower(rownames(coefficients)) == tolower(target))
                if (length(idx) > 0) coefficients[idx[1], 1] else NULL
            }

            sum_sens <- get_coef("sensitivity")
            sum_fpr <- get_coef("false pos. rate")

            # Fallback to intercept transformation if named rows missing
            if (is.null(sum_sens)) {
                tsens <- get_coef("tsens.(Intercept)")
                if (!is.null(tsens)) sum_sens <- stats::plogis(tsens)
            }

            if (is.null(sum_fpr)) {
                tfpr <- get_coef("tfpr.(Intercept)")
                if (!is.null(tfpr)) sum_fpr <- stats::plogis(tfpr)
            }

            # No usable summary (fixed-effects fit): use the fitted coefficients.
            if (is.null(sum_sens) || is.null(sum_fpr)) {
                fc <- biv_model$coefficients
                if (is.matrix(fc) && all(c("tsens", "tfpr") %in% colnames(fc))) {
                    sum_sens <- stats::plogis(fc[1, "tsens"])
                    sum_fpr  <- stats::plogis(fc[1, "tfpr"])
                }
            }

            # Store only serializable data (no model object!)
            # SERIALIZATION FIX: Ensure meta_data is a plain data frame
            if (!is.null(meta_data) && is.data.frame(meta_data)) {
                meta_data <- as.data.frame(meta_data, stringsAsFactors = FALSE)
                # Remove any attributes that might contain functions
                attributes(meta_data) <- attributes(meta_data)[c("names", "row.names", "class")]
            }

            # Pre-compute the SROC regression curve and the confidence region
            # (ellipse) here, where the Reitsma model object is available, and
            # store only the resulting (fpr, sens) coordinates so the plot state
            # stays serializable. The ellipse is the confidence region for the
            # pooled (logit-sensitivity, logit-fpr) mean derived from the bivariate
            # vcov, back-transformed to ROC space.
            sroc_curve <- NULL
            conf_ellipse <- NULL
            pred_ellipse <- NULL
            tryCatch({
                # mada::sroc() evaluates the curve at fpr = 1:99/100 whatever the
                # studies covered, so a set spanning FPR 0.02-0.27 got a curve
                # drawn out to 0.99: three quarters of the line was extrapolation
                # from a model fitted to none of that range, and it is the part a
                # reader uses to pick an operating point. mada's own
                # plot.reitsma() defaults to extrapolate = FALSE and bounds the
                # curve by min/max of the observed FPR - do the same, evaluating
                # on a grid inside that range so the line keeps its resolution.
                obs_fpr <- meta_data$fp / (meta_data$fp + meta_data$tn)
                obs_fpr <- obs_fpr[is.finite(obs_fpr)]
                fpr_grid <- if (length(obs_fpr) > 1 && diff(range(obs_fpr)) > 0)
                    seq(min(obs_fpr), max(obs_fpr), length.out = 99) else 1:99 / 100
                crv <- as.data.frame(mada::sroc(biv_model, fpr = fpr_grid))
                if (ncol(crv) >= 2) {
                    names(crv)[1:2] <- c("fpr", "sens")
                    crv <- crv[stats::complete.cases(crv[, c("fpr", "sens")]), c("fpr", "sens"), drop = FALSE]
                    if (nrow(crv) > 1) sroc_curve <- crv
                }

                mu  <- as.numeric(stats::coef(biv_model))   # (tsens, tfpr), logit scale
                Sig <- stats::vcov(biv_model)
                if (length(mu) >= 2 && all(is.finite(mu[1:2])) &&
                    is.matrix(Sig) && all(dim(Sig) >= 2) && all(is.finite(Sig[1:2, 1:2]))) {
                    theta <- seq(0, 2 * pi, length.out = 200)
                    L     <- t(chol(Sig[1:2, 1:2]))
                    # Follow the user's confidence_level. This was hard-coded at
                    # 0.95, so setting 99% gave 99% CIs in every table beside a
                    # 95% ellipse on the SROC plot, with nothing saying so.
                    ell_level <- (self$options$confidence_level %||% 95) / 100
                    ell_level <- max(min(ell_level, 0.999), 0.5)
                    rad   <- sqrt(stats::qchisq(ell_level, df = 2))
                    pts   <- L %*% (rad * rbind(cos(theta), sin(theta))) + mu[1:2]
                    conf_ellipse <- data.frame(
                        fpr  = stats::plogis(pts[2, ]),
                        sens = stats::plogis(pts[1, ])
                    )

                    # PREDICTION region: where the accuracy of a FUTURE study is
                    # expected to lie. Built from vcov + Psi (uncertainty in the
                    # pooled mean PLUS between-study heterogeneity), which is
                    # exactly what mada's own plot.reitsma(predict = TRUE) does:
                    #   Sigma <- x$Psi + vcov(x)
                    # The confidence region above describes only the precision of
                    # the summary point and is always the smaller of the two; with
                    # substantial heterogeneity they differ enormously, and showing
                    # only the confidence region invites reading a tight ellipse as
                    # "this assay performs consistently".
                    Psi <- biv_model$Psi
                    if (is.matrix(Psi) && all(dim(Psi) >= 2) && all(is.finite(Psi[1:2, 1:2]))) {
                        Sig_pred <- Sig[1:2, 1:2] + Psi[1:2, 1:2]
                        Lp <- tryCatch(t(chol(Sig_pred)), error = function(e) NULL)
                        if (!is.null(Lp)) {
                            # Radius: sqrt(qchisq(level, 2)), matching mada's own
                            # plot.reitsma(predict = TRUE). Note that the
                            # prediction INTERVAL in the table refers the same
                            # quantity to t on k - 2 df, so at small k the printed
                            # interval is wider than this region's marginal span
                            # (4.30 vs 2.45 at k = 4). They are different objects -
                            # a marginal small-sample interval and a joint
                            # large-sample region - and the note below says so
                            # rather than silently letting the reader match them.
                            pts_p <- Lp %*% (rad * rbind(cos(theta), sin(theta))) + mu[1:2]
                            pred_ellipse <- data.frame(
                                fpr  = stats::plogis(pts_p[2, ]),
                                sens = stats::plogis(pts_p[1, ])
                            )
                        }
                    }
                }
            }, error = function(e) NULL)

            # Under a fixed-effects fit mada::sroc() returns NaN: the
            # Rutter-Gatsonis curve is built from the between-study variance,
            # which fixed effects sets to zero. Say so on the plot rather than
            # leaving the reader to wonder where the curve went.
            curve_note <- NULL
            if (is.null(sroc_curve) && identical(self$options$method, "fixed")) {
                curve_note <- .("No SROC curve: the fixed-effects model assumes no between-study heterogeneity, so the curve is undefined. The summary point and its confidence region are shown. Choose REML for the curve.")
            }

            plot_state <- list(
                data = meta_data,
                pooled_sens = as.numeric(sum_sens),
                pooled_fpr = as.numeric(sum_fpr),
                sroc_curve = sroc_curve,
                conf_ellipse = conf_ellipse,
                pred_ellipse = pred_ellipse,
                curve_note = curve_note
            )

            image$setState(plot_state)
        },
        
        .srocplot = function(image, ggtheme, theme, ...) {

            # The SROC plot shows individual study points, the pooled summary
            # point, the SROC regression curve, and the confidence region
            # (ellipse). The curve and ellipse coordinates are pre-computed in
            # .populateSROCPlot from the Reitsma bivariate model and carried in
            # the (serializable) plot state.

            state <- image$state
            if (is.null(state)) {
                return(FALSE)
            }

            # Bivariate model unavailable: draw a centered explanatory panel
            # instead of a blank canvas.
            if (!is.null(state$message)) {
                if (requireNamespace("ggplot2", quietly = TRUE)) {
                    p <- ggplot2::ggplot() +
                        ggplot2::annotate(
                            "text", x = 0.5, y = 0.5,
                            label = state$message,
                            hjust = 0.5, vjust = 0.5, size = 5
                        ) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(p)
                    return(TRUE)
                }
                return(FALSE)
            }

            # Extract pre-computed values from state (no model object)
            meta_data <- state$data
            sum_sens <- state$pooled_sens
            sum_fpr <- state$pooled_fpr

            # Validate data
            if (is.null(meta_data) || !is.data.frame(meta_data) || nrow(meta_data) == 0) {
                return(FALSE)
            }

            # Check if we have summary point values. length-1 checks matter:
            # if exactly one pooled coordinate is missing from the coefficient
            # matrix it arrives as numeric(0), and data.frame(fpr, sens) then
            # dies with "arguments imply differing number of rows".
            has_summary <- length(sum_sens) == 1 && length(sum_fpr) == 1 &&
                is.finite(sum_sens) && is.finite(sum_fpr)

            if (requireNamespace("ggplot2", quietly = TRUE)) {

                # Individual study points
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
                meta_data$fpr <- 1 - meta_data$spec
                meta_data$n <- meta_data$tp + meta_data$fn + meta_data$fp + meta_data$tn

                # Get color palette for accessibility
                colors <- private$.getColorPalette(theme)

                # Base plot: individual study points sized by sample size
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = fpr, y = sens)) +
                    ggplot2::geom_point(ggplot2::aes(size = n), color = colors$study_points, alpha = 0.7)

                # Chance diagonal. The Getting Started guidance tells the reader
                # to judge the points against "the diagonal line representing
                # random chance", and until now there was no such line to judge
                # them against.
                p <- p + ggplot2::geom_abline(slope = 1, intercept = 0,
                                              colour = colors$tertiary,
                                              linewidth = 0.4, linetype = "dotdash",
                                              alpha = 0.6)

                # SROC regression curve (from the Reitsma bivariate model)
                if (!is.null(state$sroc_curve) && is.data.frame(state$sroc_curve) &&
                    nrow(state$sroc_curve) > 1) {
                    p <- p + ggplot2::geom_path(
                        data = state$sroc_curve,
                        ggplot2::aes(x = fpr, y = sens),
                        color = colors$primary, linewidth = 0.8, inherit.aes = FALSE
                    )
                }

                # Confidence region (ellipse) around the pooled estimate
                if (!is.null(state$conf_ellipse) && is.data.frame(state$conf_ellipse) &&
                    nrow(state$conf_ellipse) > 2) {
                    p <- p + ggplot2::geom_path(
                        data = state$conf_ellipse,
                        ggplot2::aes(x = fpr, y = sens),
                        color = colors$primary, linewidth = 0.6, linetype = "dashed",
                        inherit.aes = FALSE
                    )
                }

                # Prediction region (dotted) - drawn before the summary point
                if (!is.null(state$pred_ellipse) && is.data.frame(state$pred_ellipse) &&
                    nrow(state$pred_ellipse) > 2) {
                    p <- p + ggplot2::geom_path(
                        data = state$pred_ellipse,
                        ggplot2::aes(x = fpr, y = sens),
                        color = colors$primary, linewidth = 0.5, linetype = "dotted",
                        inherit.aes = FALSE
                    )
                }

                # Pooled summary point on top
                if (has_summary) {
                    p <- p + ggplot2::geom_point(
                        data = data.frame(fpr = sum_fpr, sens = sum_sens),
                        ggplot2::aes(x = fpr, y = sens),
                        color = colors$primary, size = 5, shape = 17, inherit.aes = FALSE
                    )
                }

                have_curve   <- !is.null(state$sroc_curve)
                have_ellipse <- !is.null(state$conf_ellipse)
                have_pred    <- !is.null(state$pred_ellipse)
                # Name the actual level - this said "95%" unconditionally, which
                # became wrong as soon as the ellipse started honouring
                # confidence_level. It is a CONFIDENCE region for the summary
                # point, not a prediction region for a future study; say which.
                ell_pct <- self$options$confidence_level %||% 95
                subtitle_txt <- paste(c(
                    .("Studies (circles), pooled estimate (triangle), chance line (dot-dash)"),
                    if (have_curve) .("SROC curve over the observed false-positive range only"),
                    if (have_ellipse) sprintf(.("%s%% confidence region (dashed)"), ell_pct),
                    if (have_pred) sprintf(.("%s%% prediction region (dotted)"), ell_pct)
                ), collapse = ", ")
                # A single line ran off the right edge of the panel once the
                # prediction region was added to it; wrap it instead.
                subtitle_txt <- paste(strwrap(subtitle_txt, width = 70), collapse = "\n")

                p <- p +
                    ggplot2::scale_x_continuous(limits = c(0, 1), name = .("False Positive Rate (1 - Specificity)")) +
                    ggplot2::scale_y_continuous(limits = c(0, 1), name = .("Sensitivity")) +
                    ggplot2::labs(
                        title = .("Summary ROC Plot"),
                        subtitle = subtitle_txt,
                        caption = state$curve_note,
                        size = .("Sample Size")
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10)
                    )

                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateFunnelPlot = function(meta_data) {

            image <- self$results$funnelplot
            # SERIALIZATION FIX: Ensure it's a plain data frame
            if (!is.null(meta_data) && is.data.frame(meta_data)) {
                meta_data <- as.data.frame(meta_data, stringsAsFactors = FALSE)
                # Remove any attributes that might contain functions
                attributes(meta_data) <- attributes(meta_data)[c("names", "row.names", "class")]
            }
            image$setState(meta_data)
        },
        
        .funnelplot = function(image, ggtheme, theme, ...) {

            meta_data <- image$state

            # Validate meta_data
            if (is.null(meta_data) || !is.data.frame(meta_data) || nrow(meta_data) == 0) {
                return(FALSE)
            }

            if (requireNamespace("ggplot2", quietly = TRUE)) {

                # Ensure meta_data is a proper data frame
                meta_data <- as.data.frame(meta_data)

                # Match Deeks' test: the test applies +0.5 so zero-cell studies
                # have a finite log DOR, but the plot previously computed log DOR
                # on raw counts, so those same studies became Inf and were
                # silently dropped by ggplot - plot and test disagreed on which
                # studies they describe. The correction goes on every study here
                # too, for the reason given in .performPublicationBiasAssessment.
                cells <- c("tp", "fp", "fn", "tn")
                zero_rows <- rowSums(meta_data[, cells] == 0) > 0
                n_zero_plot <- sum(zero_rows)
                if (n_zero_plot > 0) {
                    meta_data[, cells] <- meta_data[, cells] + 0.5
                }

                # Deeks' funnel plot: log DOR against 1/sqrt(ESS).
                #
                # This previously plotted precision = 1/SE(log DOR) on the y axis,
                # the conventional Egger-style funnel. Deeks, Macaskill & Irwig
                # (2005) showed that is misleading for diagnostic accuracy data
                # precisely because log DOR and its standard error are
                # intrinsically correlated, which induces asymmetry even with no
                # publication bias - that is why the accompanying test regresses
                # on 1/sqrt(ESS). Plot and test must use the same x/y, otherwise a
                # visibly symmetric funnel sits beside a significant asymmetry
                # p-value (or the reverse).
                meta_data$log_dor <- log((meta_data$tp * meta_data$tn) / (meta_data$fp * meta_data$fn))
                n_diseased <- meta_data$tp + meta_data$fn
                n_healthy  <- meta_data$fp + meta_data$tn
                meta_data$ess <- 4 * n_diseased * n_healthy / (n_diseased + n_healthy)
                meta_data$inv_root_ess <- 1 / sqrt(meta_data$ess)

                # Get color palette for accessibility
                colors <- private$.getColorPalette(theme)

                # The plot is read together with the test, so it carries the test:
                # the fitted regression line is the slope the p-value refers to.
                # Without it a reader judges symmetry by eye on a cloud of points
                # whose y axis is reversed, and eye and test routinely disagree.
                ok_plot <- is.finite(meta_data$log_dor) & is.finite(meta_data$inv_root_ess) &
                           is.finite(meta_data$ess) & meta_data$ess > 0
                fit_line <- NULL
                fit_label <- NULL
                if (sum(ok_plot) >= 3) {
                    fd <- meta_data[ok_plot, , drop = FALSE]
                    pfit <- stats::lm(log_dor ~ inv_root_ess, data = fd, weights = fd$ess)
                    fit_line <- data.frame(log_dor = stats::fitted(pfit),
                                           inv_root_ess = fd$inv_root_ess)
                    fit_line <- fit_line[order(fit_line$inv_root_ess), , drop = FALSE]
                    pcf <- summary(pfit)$coefficients
                    # Every study the same size - six readers on one set of
                    # slides - makes inv_root_ess constant, the slope aliased and
                    # this a one-row matrix. The test path guards it; the
                    # renderer has no tryCatch, so an unguarded pcf[2, 3] took
                    # the whole plot down with "subscript out of bounds".
                    if (nrow(pcf) >= 2) {
                        fit_label <- sprintf(.("Deeks' asymmetry test: t = %.2f on %d df, p = %.3f"),
                                             pcf[2, 3], stats::df.residual(pfit), pcf[2, 4])
                        # The table calls its verdict descriptive past a quarter
                        # zero-cell studies; say the same beside the line.
                        if (n_zero_plot / nrow(meta_data) > 0.25)
                            fit_label <- paste(fit_label,
                                .("- descriptive only: too many zero cells for a valid asymmetry test"))
                    } else {
                        # an intercept-only fit would draw a meaningless flat line
                        fit_line <- NULL
                    }
                }
                sub_parts <- c(fit_label, if (n_zero_plot > 0) sprintf(
                    .("+0.5 continuity correction applied to all %d studies (%d had a zero cell), matching Deeks' test"),
                    nrow(meta_data), n_zero_plot))

                # Create funnel plot
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = log_dor, y = inv_root_ess)) +
                    ggplot2::geom_point(size = 3, alpha = 0.7, color = colors$secondary)
                if (!is.null(fit_line))
                    p <- p + ggplot2::geom_line(data = fit_line, linewidth = 0.8,
                                                colour = colors$primary, linetype = "dashed")
                p <- p +
                    ggplot2::scale_y_reverse() +
                    ggplot2::labs(
                        title = .("Deeks' Funnel Plot: Publication Bias Assessment"),
                        subtitle = if (length(sub_parts) > 0)
                            paste(sub_parts, collapse = "\n") else NULL,
                        x = .("Log Diagnostic Odds Ratio"),
                        y = expression(1/sqrt("effective sample size"))
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10)
                    )
                
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        # Pure onboarding content. Runtime warnings and errors are delivered
        # through the dedicated `notices` Html item (.addNotice), never
        # appended here.
        .populateInstructions = function() {
            
            html <- "
            <h2>Diagnostic Test Meta-Analysis for Pathology</h2>
            
            <h3>Purpose</h3>
            <p>This module performs comprehensive meta-analysis of diagnostic test accuracy studies, specifically designed for pathology research including AI algorithm validation and biomarker diagnostic accuracy synthesis.</p>
            
            <h3>Required Data Structure</h3>
            <p><strong>Essential Variables (Required):</strong></p>
            <ul>
                <li><strong>Study identifier:</strong> Unique name or ID for each study (e.g., 'Smith_2020', 'Study_1')</li>
                <li><strong>True positives (TP):</strong> Number correctly identified as positive</li>
                <li><strong>False positives (FP):</strong> Number incorrectly identified as positive</li>
                <li><strong>False negatives (FN):</strong> Number incorrectly identified as negative</li>
                <li><strong>True negatives (TN):</strong> Number correctly identified as negative</li>
            </ul>
            
            <p><strong>Optional Variables for Meta-Regression:</strong></p>
            <ul>
                <li><strong>Patient population:</strong> Disease stage, demographics (e.g., 'early_stage', 'advanced', 'mixed')</li>
                <li><strong>Technical method:</strong> Staining protocol (e.g., 'automated', 'manual')</li>
                <li><strong>Geographic region:</strong> Study location for population analysis</li>
                <li><strong>Publication year:</strong> For temporal trend investigation</li>
            </ul>
            
            <h3>Data Preparation Checklist</h3>
            <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>
                <p><strong>Before running analysis, verify:</strong></p>
                <ul>
                    <li> No missing values in TP, FP, FN, TN columns</li>
                    <li> All values are non-negative integers</li>
                    <li> At least 3 studies with complete data</li>
                    <li> Study identifiers are unique</li>
                    <li> Sample sizes are realistic (TP+FP+FN+TN = total cases per study)</li>
                </ul>
            </div>
            
            <h3>Example Data Format</h3>
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: rgba(33, 33, 33, 0.06); color: inherit;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>study_name</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>true_positives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>false_positives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>false_negatives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>true_negatives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>population</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Smith_2020</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>47</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>101</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>9</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>738</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>mixed</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Johnson_2021</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>126</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>272</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>51</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>1543</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>early_stage</td>
                </tr>
            </table>
            
            <h3>Analysis Methods</h3>
            <ul>
                <li><strong>Bivariate Random-Effects Model (Recommended):</strong> Jointly analyzes sensitivity and specificity accounting for correlation</li>
                <li><strong>Proportional-Hazards SROC Analysis:</strong> Holling model estimated by adjusted profile maximum likelihood</li>
                <li><strong>Meta-Regression:</strong> Investigates sources of heterogeneity using study-level covariates</li>
                <li><strong>Publication Bias Assessment:</strong> Deeks' funnel plot asymmetry test</li>
            </ul>

            <h3>Statistical Method Selection Guide</h3>
            <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #28a745; margin: 10px 0; color: inherit;'>
                <p><strong>Choose the appropriate estimation method for your meta-analysis:</strong></p>
                <ul>
                    <li><strong>REML (Recommended):</strong> Default choice for most diagnostic meta-analyses. Most robust for random effects modeling with good performance across different scenarios.</li>
                    <li><strong>Maximum Likelihood:</strong> Alternative estimation approach when maximum likelihood estimation is specifically preferred or required by study protocol.</li>
                    <li><strong>Fixed Effects:</strong> Use when between-study heterogeneity is minimal or you want to assume all studies estimate the same underlying effect size.</li>
                    <li><strong>Method of Moments:</strong> Classical moment-based estimation method, useful for comparison with older meta-analyses or when computational resources are limited.</li>
                    <li><strong>Variance Components:</strong> Specialized approach for variance component estimation, typically used in advanced methodological research.</li>
                </ul>
                <p><strong> Recommendation:</strong> Start with REML unless you have specific methodological requirements. It provides the best balance of statistical properties and computational stability for diagnostic test meta-analysis.</p>
            </div>
            
            <h3>Clinical Applications</h3>
            <ul>
                <li>IHC marker validation across multiple pathology studies</li>
                <li>AI algorithm performance meta-analysis for clinical implementation</li>
                <li>Biomarker diagnostic accuracy synthesis for guideline development</li>
                <li>Cross-population comparison of diagnostic test performance</li>
                <li>Assessment of test performance heterogeneity and variation sources</li>
            </ul>
            "
            
            self$results$instructions$setContent(html)
        },
        
        .populateIndividualStudies = function(meta_data) {

            table <- self$results$individualstudies

            table$deleteRows()

            # Calculate sensitivity and specificity for each study
            meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
            meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
            meta_data$sample_size <- meta_data$tp + meta_data$fp + meta_data$fn + meta_data$tn

            conf_pct <- private$.metaforLevel()
            z_crit <- stats::qnorm(1 - (1 - conf_pct / 100) / 2)

            for (i in seq_len(nrow(meta_data))) {
                sens_ci <- private$.wilsonCI(meta_data$tp[i],
                                             meta_data$tp[i] + meta_data$fn[i], z_crit)
                spec_ci <- private$.wilsonCI(meta_data$tn[i],
                                             meta_data$tn[i] + meta_data$fp[i], z_crit)
                table$addRow(rowKey = i, values = list(
                    study = as.character(meta_data$study[i]),
                    # Percent, matching the pooled table. These were proportions
                    # (0.82) while the bivariate table held percentages (81.59),
                    # so the same quantity appeared on two scales on one screen.
                    sensitivity = meta_data$sens[i] * 100,
                    sens_ci_lower = sens_ci[1] * 100,
                    sens_ci_upper = sens_ci[2] * 100,
                    specificity = meta_data$spec[i] * 100,
                    spec_ci_lower = spec_ci[1] * 100,
                    spec_ci_upper = spec_ci[2] * 100,
                    tp = as.integer(meta_data$tp[i]),
                    fp = as.integer(meta_data$fp[i]),
                    fn = as.integer(meta_data$fn[i]),
                    tn = as.integer(meta_data$tn[i]),
                    sample_size = as.integer(meta_data$sample_size[i])
                ))
            }

            if (isTRUE(private$.continuity_correction)) {
                table$setNote("ci_method", sprintf(
                    .("Per-study intervals are Wilson score confidence intervals at the %d%% level, computed from the raw counts shown here."),
                    as.integer(conf_pct)))
            } else {
                table$setNote("ci_method", sprintf(
                    .("Per-study intervals are Wilson score confidence intervals at the %d%% level - the same intervals drawn on the forest plot."),
                    as.integer(conf_pct)))
            }

            if (isTRUE(private$.continuity_correction)) {
                table$setNote("raw_counts",
                    .("This table shows the RAW counts and the accuracy computed from them; the pooled analyses and the forest plot use the zero-cell-corrected counts, so values for corrected studies differ slightly."))
            }
        },

        .populateInterpretation = function() {
            
            html <- "
            <h2>Clinical Interpretation Guidelines</h2>
            
            <h3> Primary Results Interpretation</h3>
            
            <h4>Pooled Sensitivity and Specificity</h4>
            <ul>
                <li><strong>Pooled Sensitivity:</strong> Proportion of diseased cases correctly identified
                    <ul>
                        <li>Excellent sensitivity ([[GE]]90%): few diseased cases missed</li>
                        <li>Good sensitivity (80-89%): acceptable miss rate for screening</li>
                        <li>Moderate sensitivity (70-79%): appreciable number of cases missed</li>
                        <li>Limited sensitivity (&lt;70%): many cases missed</li>
                    </ul>
                </li>
                <li><strong>Pooled Specificity:</strong> Proportion of non-diseased cases correctly identified
                    <ul>
                        <li>Excellent specificity ([[GE]]90%): few false alarms</li>
                        <li>Good specificity (80-89%): some false positives</li>
                        <li>Moderate specificity (70-79%): appreciable false-positive rate</li>
                        <li>Limited specificity (&lt;70%): many false alarms</li>
                    </ul>
                </li>
            </ul>
            
            <h4>Likelihood Ratios for Clinical Decision-Making</h4>
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: rgba(33, 33, 33, 0.06); color: inherit;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Likelihood Ratio</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Value Range</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Clinical Interpretation</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>&gt;10</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Strong evidence FOR disease when test positive</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>5-10</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Moderate evidence for disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>2-5</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Weak evidence for disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>&lt;0.1</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Strong evidence AGAINST disease when test negative</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.1-0.2</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Moderate evidence against disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.2-0.5</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Weak evidence against disease</td>
                </tr>
            </table>
            
            <h4>Diagnostic Odds Ratio (DOR)</h4>
            <ul>
                <li><strong>DOR > 25:</strong> Excellent overall discriminative ability</li>
                <li><strong>DOR 10-25:</strong> Good discriminative ability</li>
                <li><strong>DOR 5-10:</strong> Moderate discriminative ability</li>
                <li><strong>DOR &lt; 5:</strong> Limited discriminative ability</li>
            </ul>
            
            <h3> Heterogeneity Assessment</h3>
            
            <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0; color: inherit;'>
                <h4>I[[SUP2]] Statistic Interpretation:</h4>
                <p><em>The bands below are Higgins' conventional cut-points from intervention
                meta-analysis. They are not established thresholds for diagnostic accuracy, where a
                high I[[SUP2]] commonly reflects studies using different positivity thresholds - the
                situation the SROC curve exists to model - rather than a reason to abandon the
                analysis. Treat them as rough orientation, and prefer the prediction region.</em></p>
                <ul>
                    <li><strong>I[[SUP2]] < 25%:</strong> Little heterogeneity detected on this margin. I[[SUP2]] is imprecisely estimated when few studies are pooled and does not describe the bivariate model, so on its own it does not establish that pooling is appropriate - inspect the prediction region and check that positivity thresholds, patient spectrum and reference standards are comparable</li>
                    <li><strong>I[[SUP2]] 25-50%:</strong> Moderate heterogeneity - investigate potential sources</li>
                    <li><strong>I[[SUP2]] 50-75%:</strong> Substantial heterogeneity - pooling questionable</li>
                    <li><strong>I[[SUP2]] &gt; 75%:</strong> Considerable heterogeneity - a single pooled point is unlikely to describe the evidence; investigate thresholds, spectrum and reference standards, and report the SROC curve and prediction region</li>
                </ul>
            </div>
            
            <h4>Common Sources of Heterogeneity:</h4>
            <ul>
                <li><strong>Patient Population:</strong> Disease stage, severity, demographics</li>
                <li><strong>Technical Factors:</strong> Staining protocols, antibody sources, automation</li>
                <li><strong>Methodological:</strong> Reference standards, blinding, cut-off thresholds</li>
                <li><strong>Geographic/Temporal:</strong> Population differences, technology evolution</li>
            </ul>
            
            <h3> Publication Bias Assessment</h3>
            
            <h4>Deeks' Funnel Plot Test:</h4>
            <ul>
                <li><strong>p [[GE]] 0.05:</strong> No statistically significant funnel-plot asymmetry was detected. This does NOT indicate that publication bias is unlikely: Deeks' test has low power and is unreliable with fewer than 10 studies, so a non-significant result is uninformative when few studies are pooled</li>
                <li><strong>p < 0.05:</strong> The funnel plot is asymmetric - small studies report systematically different accuracy from large ones. Publication bias is one explanation; heterogeneity, a threshold effect, and a genuine relation between study size and accuracy (smaller studies often use narrower spectra) are others. The test cannot tell them apart, so a significant result is a prompt to investigate, not a finding of publication bias</li>
                <li>With the default zero-cell setting (no correction), where more than a quarter of the studies have a zero cell, no verdict is reported at all: the continuity correction the test needs produces asymmetry by itself. Choosing any Zero-Cell Correction leaves no zero cell for this test, so a verdict is always given</li>
            </ul>
            
            <div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; border-left: 4px solid #dc3545; margin: 10px 0; color: inherit;'>
                <p><strong> When the Funnel Plot is Asymmetric:</strong></p>
                <ul>
                    <li>Ask first whether the small studies differ clinically from the large ones - spectrum, setting, threshold, reference standard</li>
                    <li>Search for unpublished studies or negative results</li>
                    <li>Consider contacting study authors for additional data</li>
                    <li>If no other explanation fits, pooled estimates may be overoptimistic</li>
                    <li>Report the asymmetry as a limitation and interpret results cautiously</li>
                </ul>
            </div>
            
            <h3> Clinical Application Guidance</h3>
            
            <h4>IHC Marker Validation:</h4>
            <ul>
                <li><strong>Screening Applications:</strong> Prioritize high sensitivity ([[GE]]90%)</li>
                <li><strong>Confirmatory Testing:</strong> Prioritize high specificity ([[GE]]90%)</li>
                <li><strong>Balanced Performance:</strong> Consider clinical costs of false positives vs false negatives</li>
            </ul>
            
            <h4>AI Algorithm Implementation:</h4>
            <ul>
                <li><strong>Consistent Performance:</strong> low measured heterogeneity means the included studies produced similar estimates; with few studies it is imprecisely estimated and does not by itself show that performance transfers to a new setting</li>
                <li><strong>Variable Performance:</strong> high heterogeneity indicates that accuracy differed across the included populations and protocols</li>
                <li><strong>Not external validation:</strong> a meta-analysis summarises existing published evidence and inherits any publication, spectrum or reference-standard bias of the included studies; it is not a prospective evaluation in an intended-use population with a pre-specified threshold</li>
            </ul>
            
            <h4>Predictive Values in Clinical Practice:</h4>
            <p><strong>Important:</strong> Sensitivity and specificity are test characteristics, but clinicians need predictive values that depend on disease prevalence in their population.</p>
            
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: rgba(33, 33, 33, 0.06); color: inherit;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Disease Prevalence</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>PPV (Sen=90%, Spe=80%)</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>NPV (Sen=90%, Spe=80%)</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>5%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>19%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>99%</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>20%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>53%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>97%</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>50%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>82%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>89%</td>
                </tr>
            </table>
            
            <h3> Reporting Recommendations</h3>
            
            <p>When reporting your meta-analysis results, include:</p>
            <ul>
                <li> <strong>Study Selection:</strong> Number of studies included and excluded</li>
                <li> <strong>Pooled Estimates:</strong> Sensitivity and specificity with confidence intervals at the level chosen in Advanced Options</li>
                <li> <strong>Likelihood Ratios:</strong> For clinical decision-making context</li>
                <li> <strong>Heterogeneity:</strong> I[[SUP2]] values and potential sources investigated</li>
                <li> <strong>Publication Bias:</strong> Deeks' test results and visual assessment</li>
                <li> <strong>Clinical Implications:</strong> Population-specific predictive values</li>
                <li> <strong>Limitations:</strong> Study quality, missing data, generalizability</li>
            </ul>
            
            <div style='background-color: rgba(33, 163, 188, 0.21); padding: 15px; border-left: 4px solid #17a2b8; margin: 10px 0; color: inherit;'>
                <p><strong> Pro Tip:</strong> Always interpret meta-analysis results in the context of your specific clinical population and intended use. A test excellent for one application may be inappropriate for another.</p>
            </div>
            "
            
            self$results$interpretation$setContent(private$.renderSymbols(html))
        },

        # Enhanced data validation with user-friendly warnings
        # Generate natural language summary
        .generateSummary = function(meta_data) {

            # If pooled estimates are not available, provide basic summary
            if (is.null(private$.pooled_sensitivity) || is.null(private$.pooled_specificity) ||
                !is.finite(private$.pooled_sensitivity) || !is.finite(private$.pooled_specificity)) {
                private$.generateBasicSummary(meta_data)
                return()
            }

            # Calculate confidence intervals (these should be set by bivariate analysis)
            sens_pct <- round(private$.pooled_sensitivity * 100, 1)
            spec_pct <- round(private$.pooled_specificity * 100, 1)

            # Calculate positive and negative likelihood ratios
            lr_pos <- NA_real_
            lr_neg <- NA_real_

            if (is.finite(private$.pooled_sensitivity) && is.finite(private$.pooled_specificity)) {
                denom_plr <- 1 - private$.pooled_specificity
                denom_nlr <- private$.pooled_specificity

                if (is.finite(denom_plr) && denom_plr > 0) {
                    lr_pos <- private$.pooled_sensitivity / denom_plr
                }

                if (is.finite(denom_nlr) && denom_nlr > 0) {
                    lr_neg <- (1 - private$.pooled_sensitivity) / denom_nlr
                }
            }

            inv_lr_neg <- if (is.finite(lr_neg) && lr_neg > 0) 1 / lr_neg else NA_real_

            # library-audit 2026-09-16 OncoPath [INFO] DONE: no " [" inside .() - jmvcore's translator reads
            #   " [..]" as a context marker and cut these sentences short in any language without a catalog
            plr_text <- if (is.finite(lr_pos)) {
                paste0("<p>", sprintf(.("<strong>Positive Likelihood Ratio:</strong> %.2f - A positive test is %.1fx more likely in disease than healthy"),
                        lr_pos, lr_pos), "</p>")
            } else {
                paste0("<p>", .("<strong>Positive Likelihood Ratio:</strong> Not estimable with the current data (specificity \u2248 100% or model unstable)."), "</p>")
            }

            nlr_text <- if (is.finite(lr_neg)) {
                if (is.finite(inv_lr_neg)) {
                    paste0("<p>", sprintf(.("<strong>Negative Likelihood Ratio:</strong> %.2f - A negative test is %.1fx more likely in healthy than disease"),
                            lr_neg, inv_lr_neg), "</p>")
                } else {
                    paste0("<p>", sprintf(
                        .("<strong>Negative Likelihood Ratio:</strong> %.2f - Interpretation unstable (sensitivity \u2248 100%%)."),
                        lr_neg
                    ), "</p>")
                }
            } else {
                private$.renderSymbols(paste0("<p>", .("<strong>Negative Likelihood Ratio:</strong> Not estimable with the current data. LR- is (1 - sensitivity) / specificity, so it needs a pooled specificity above zero and finite pooled estimates; here the pooled specificity is 0 or an estimate did not converge. Read the pooled specificity and its confidence interval in the summary above to see which."), "</p>"))
            }

            # This is the block a user pastes into a manuscript, and it used to
            # carry two point estimates and nothing else - no interval, no k
            # qualifier, no hint that a wide prediction interval sat in the
            # notices panel. Carry the uncertainty with the estimate.
            ci_fragment <- function(ci) {
                if (length(ci) == 2 && all(is.finite(ci)))
                    sprintf(" (%.1f%%-%.1f%%)", ci[1], ci[2]) else ""
            }
            copy_text <- sprintf(
                .("Meta-analysis of %d diagnostic accuracy studies shows pooled sensitivity of %.1f%%%s and specificity of %.1f%%%s, at the %d%% confidence level."),
                private$.n_studies, sens_pct, ci_fragment(private$.pooled_sens_ci),
                spec_pct, ci_fragment(private$.pooled_spec_ci),
                self$options$confidence_level %||% 95
            )
            # Guard BOTH margins: sprintf() with a zero-length argument returns
            # character(0) rather than erroring, and that propagates through the
            # panel's final sprintf and blanks it with nothing in the log.
            if (length(private$.pooled_sens_pi) == 2 && all(is.finite(private$.pooled_sens_pi)) &&
                length(private$.pooled_spec_pi) == 2 && all(is.finite(private$.pooled_spec_pi))) {
                copy_text <- sprintf(
                    .("%s A new study is predicted to fall between %.1f%% and %.1f%% for sensitivity and between %.1f%% and %.1f%% for specificity."),
                    copy_text, private$.pooled_sens_pi[1], private$.pooled_sens_pi[2],
                    private$.pooled_spec_pi[1], private$.pooled_spec_pi[2])
            }

            if (is.finite(lr_pos) && is.finite(lr_neg)) {
                copy_text <- sprintf(
                    .("%s Positive LR %.2f and negative LR %.2f."),
                    copy_text, lr_pos, lr_neg
                )
            }

            # Build zero-cell correction disclosure
            correction_disclosure <- ""
            if (!is.null(private$.correction_method_used) && private$.correction_method_used != "none") {
                n_corrected <- length(private$.corrected_study_names)
                if (n_corrected > 0) {
                    method_label <- private$.correctionLabel(private$.correction_method_used)

                    # This panel is HTML, so the study names are escaped. The
                    # plain-text version of this disclosure is a table note set
                    # in .run().
                    safe_studies_html <- paste(
                        htmltools::htmlEscape(head(private$.corrected_study_names, 5)),
                        collapse = ", "
                    )

                    correction_disclosure <- paste0(
                        "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                        "<h5> ", .("Zero-Cell Correction Applied"), "</h5>",
                        "<p>", sprintf(.("<strong>Method:</strong> %s"), method_label), "</p>",
                        "<p>", sprintf(.("<strong>Studies corrected:</strong> %d of %d (%s)"),
                                       n_corrected, private$.n_studies, safe_studies_html), "</p>",
                        "<p>", .("<em>Note:</em> Zero-cell corrections can introduce bias, especially in large studies. Results should be interpreted with caution."), "</p>",
                        "</div>"
                    )
                }
            }

            summary_html <- sprintf("
            <div class='analysis-summary' style='background-color: rgba(33, 149, 188, 0.1); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4>Meta-Analysis Summary</h4>
                <p><strong>Analysis Type:</strong> Diagnostic test accuracy meta-analysis of %d studies</p>

                <div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Pooled Test Performance</h5>
                    <p><strong>Sensitivity:</strong> %.1f%% - The test correctly identifies %.0f out of 100 patients with disease</p>
                    <p><strong>Specificity:</strong> %.1f%% - The test correctly identifies %.0f out of 100 healthy individuals</p>
                </div>

                %s

                <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Clinical Decision Metrics</h5>
                    %s
                    %s
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Interpretation Guide</h5>
                    <p>%s</p>
                </div>

                <div style='margin-top: 15px;'>
                    <button onclick='navigator.clipboard.writeText(this.getAttribute(\"data-text\"))'
                            data-text='%s'
                            style='background-color: #007bff; color: #ffffff; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer;'>
                        Copy Summary to Clipboard
                    </button>
                </div>
            </div>
            ",
            private$.n_studies,
            sens_pct, sens_pct,
            spec_pct, spec_pct,
            correction_disclosure,
            plr_text,
            nlr_text,
            private$.getInterpretationText(sens_pct, spec_pct, lr_pos, lr_neg,
                                          private$.pooled_sens_ci, private$.pooled_spec_ci),
            copy_text
            )

            self$results$summary$setContent(summary_html)
        },

        # Generate basic summary when pooled estimates are not available
        .generateBasicSummary = function(meta_data) {

            if (is.null(meta_data) || nrow(meta_data) == 0) {
                return()
            }

            # Calculate individual study statistics
            meta_data$sensitivity <- meta_data$tp / (meta_data$tp + meta_data$fn)
            meta_data$specificity <- meta_data$tn / (meta_data$tn + meta_data$fp)
            meta_data$sample_size <- meta_data$tp + meta_data$fp + meta_data$fn + meta_data$tn

            # Calculate basic descriptive statistics
            n_studies <- nrow(meta_data)
            total_sample <- sum(meta_data$sample_size, na.rm = TRUE)

            sens_mean <- mean(meta_data$sensitivity, na.rm = TRUE) * 100
            sens_range <- range(meta_data$sensitivity, na.rm = TRUE) * 100
            spec_mean <- mean(meta_data$specificity, na.rm = TRUE) * 100
            spec_range <- range(meta_data$specificity, na.rm = TRUE) * 100

            # Determine why pooled estimates failed
            reason <- ""
            if (!isTRUE(self$options$bivariate_analysis)) {
                reason <- .("Bivariate analysis was not enabled. Enable bivariate analysis for pooled estimates.")
            } else if (n_studies < 3) {
                reason <- sprintf(.("Only %d studies available. At least 3 studies are required for meta-analysis."), n_studies)
            } else {
                reason <- .("Bivariate meta-analysis encountered an error. Check individual study results and data quality.")
            }

            summary_html <- sprintf("
            <div class='analysis-summary' style='background-color: rgba(33, 149, 188, 0.1); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4>Meta-Analysis Summary</h4>
                <p><strong>Analysis Status:</strong> %s</p>

                <div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Study Overview</h5>
                    <p><strong>Number of Studies:</strong> %d</p>
                    <p><strong>Total Sample Size:</strong> %s participants</p>
                    <p><strong>Sample Size Range:</strong> %s - %s per study</p>
                </div>

                <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Individual Study Performance (Descriptive)</h5>
                    <p><strong>Sensitivity:</strong> Mean %.1f%% (Range: %.1f%% - %.1f%%)</p>
                    <p><strong>Specificity:</strong> Mean %.1f%% (Range: %.1f%% - %.1f%%)</p>
                    <p><em>Note: These are simple averages, not meta-analytic pooled estimates.</em></p>
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Recommendation</h5>
                    <p>%s</p>
                    <p>Individual study results are available in the table below for detailed examination.</p>
                </div>
            </div>
            ",
            reason,
            n_studies,
            # %s + format(): sample sizes are numeric, and "%d" aborted the whole
            # analysis ("invalid format '%d'") whenever a count was fractional.
            format(round(total_sample)),
            format(round(min(meta_data$sample_size, na.rm = TRUE))),
            format(round(max(meta_data$sample_size, na.rm = TRUE))),
            sens_mean, sens_range[1], sens_range[2],
            spec_mean, spec_range[1], spec_range[2],
            reason
            )

            self$results$summary$setContent(summary_html)
        },

        # Helper function for dynamic interpretation text with actual values
        .getInterpretationText = function(sens, spec, lr_pos, lr_neg,
                                         sens_ci = NULL, spec_ci = NULL) {
            # Classify against the standard bands, then check whether the
            # confidence interval actually supports the label. Every claim below
            # was previously made from the point estimate alone, so a pooled
            # sensitivity of 90.4% with a 95% CI of 71-97% was reported as
            # "excellent ... will detect 90 out of 100 patients", which the
            # interval does not support.
            band_key <- function(x) if (x >= 90) "excellent" else if (x >= 80) "good"
                                else if (x >= 70) "moderate" else "limited"

            # TRUE when the interval spans more than one performance band, i.e.
            # the data cannot distinguish "excellent" from something worse.
            ci_spans_bands <- function(ci) {
                if (is.null(ci) || length(ci) < 2 || any(!is.finite(ci))) return(FALSE)
                band_key(min(ci)) != band_key(max(ci))
            }
            sens_uncertain <- ci_spans_bands(sens_ci)
            spec_uncertain <- ci_spans_bands(spec_ci)

            ci_txt <- function(ci) {
                if (is.null(ci) || length(ci) < 2 || any(!is.finite(ci))) return("")
                sprintf(" (%.1f%%-%.1f%%)", min(ci), max(ci))
            }

            # library-audit 2026-09-16 OncoPath [LOW] DONE: every band is its own whole .() sentence and the
            #   branches test these keys, never a translated word (in Turkish LR+ = Inf printed; NaN crashed)
            plr_key <- if (!is.finite(lr_pos)) "not_estimable" else if (lr_pos > 10) "strong"
                       else if (lr_pos >= 5) "moderate" else if (lr_pos >= 2) "weak" else "minimal"
            nlr_key <- if (!is.finite(lr_neg)) "not_estimable" else if (lr_neg < 0.1) "strong"
                       else if (lr_neg <= 0.2) "moderate" else if (lr_neg <= 0.5) "weak" else "minimal"

            # Sentences are collected and joined at the end; no .() string carries the separator.
            parts <- sprintf(switch(band_key(sens),
                excellent = .("<strong>Your pooled sensitivity of %.1f%%%s</strong> falls in the <em>excellent</em> band on the conventional 90/80/70 scale."),
                good      = .("<strong>Your pooled sensitivity of %.1f%%%s</strong> falls in the <em>good</em> band on the conventional 90/80/70 scale."),
                moderate  = .("<strong>Your pooled sensitivity of %.1f%%%s</strong> falls in the <em>moderate</em> band on the conventional 90/80/70 scale."),
                limited   = .("<strong>Your pooled sensitivity of %.1f%%%s</strong> falls in the <em>limited</em> band on the conventional 90/80/70 scale.")),
                sens, ci_txt(sens_ci))
            if (sens_uncertain) {
                parts <- c(parts, .("<strong>Note:</strong> the confidence interval spans more than one performance category, so this classification is not firmly established by the pooled data."))
            }
            # The bands are a descriptive convention with no cited source. They
            # used to read as clearance for a clinical purpose ("excellent for
            # screening purposes"), which no meta-analysis of accuracy alone can
            # support - that depends on prevalence and on what each error costs.
            parts <- c(parts, .("<strong>These bands are a descriptive convention, not a fitness-for-use judgement.</strong> Whether an assay is adequate for screening, confirmation or any other role depends on the prevalence in the population you intend to test and on the clinical cost of a false negative versus a false positive - none of which this analysis knows. Read the prediction interval alongside the pooled pair before applying either to practice."))

            # Add sensitivity-specific guidance
            if (sens >= 90) {
                parts <- c(parts,
                    if (sens_uncertain) {
                        sprintf(.("On the pooled estimate this test would detect about %.0f of 100 patients with disease, but the interval%s admits materially worse performance."),
                                sens, ci_txt(sens_ci))
                    } else {
                        sprintf(.("With %.1f%% sensitivity, this test will detect %.0f out of 100 patients with disease, missing only %.0f."),
                                sens, sens, 100 - sens)
                    },
                    switch(nlr_key,
                        strong = sprintf(.("Rule-out power is governed by the negative likelihood ratio rather than by sensitivity alone; here LR- = %.2f, which lowers the post-test odds substantially (the SnNout pattern)."), lr_neg),
                        moderate = sprintf(.("Rule-out power is governed by the negative likelihood ratio rather than by sensitivity alone; here LR- = %.2f, which provides moderate evidence against disease after a negative result."), lr_neg),
                        weak = sprintf(.("Rule-out power is governed by the negative likelihood ratio rather than by sensitivity alone; here LR- = %.2f, which provides weak evidence against disease after a negative result."), lr_neg),
                        minimal = sprintf(.("Rule-out power is governed by the negative likelihood ratio rather than by sensitivity alone; here LR- = %.2f, which provides minimal evidence against disease after a negative result."), lr_neg),
                        not_estimable = .("Rule-out power is governed by the negative likelihood ratio rather than by sensitivity alone, and LR- is not estimable here.")
                    )
                )
            } else if (sens >= 80) {
                parts <- c(parts,
                    sprintf(.("With %.1f%% sensitivity, approximately %.0f out of 100 diseased patients will be correctly identified."),
                            sens, sens),
                    .("<strong>Interpretation:</strong> a negative result reduces, but does not remove, the possibility of disease at this sensitivity.")
                )
            } else {
                parts <- c(parts,
                    sprintf(.("With %.1f%% sensitivity, up to %.0f out of 100 diseased patients may be missed."),
                            sens, 100 - sens),
                    .("<strong>Interpretation:</strong> at this sensitivity a substantial proportion of diseased patients are expected to test negative.")
                )
            }

            # Add specificity interpretation
            parts <- c(parts, sprintf(switch(band_key(spec),
                excellent = .("<br><br><strong>Your pooled specificity of %.1f%%%s</strong> falls in the <em>excellent</em> band on the conventional 90/80/70 scale."),
                good      = .("<br><br><strong>Your pooled specificity of %.1f%%%s</strong> falls in the <em>good</em> band on the conventional 90/80/70 scale."),
                moderate  = .("<br><br><strong>Your pooled specificity of %.1f%%%s</strong> falls in the <em>moderate</em> band on the conventional 90/80/70 scale."),
                limited   = .("<br><br><strong>Your pooled specificity of %.1f%%%s</strong> falls in the <em>limited</em> band on the conventional 90/80/70 scale.")),
                spec, ci_txt(spec_ci)))
            if (spec_uncertain) {
                parts <- c(parts, .("<strong>Note:</strong> the confidence interval spans more than one performance category, so this classification is not firmly established by the pooled data."))
            }

            if (spec >= 90) {
                parts <- c(parts,
                    sprintf(.("With %.1f%% specificity, only %.0f out of 100 healthy individuals will test positive (false alarms)."),
                            spec, 100 - spec),
                    switch(plr_key,
                        strong = sprintf(.("Rule-in power is governed by the positive likelihood ratio rather than by specificity alone; here LR+ = %.2f, which raises the post-test odds substantially (the SpPin pattern)."), lr_pos),
                        moderate = sprintf(.("Rule-in power is governed by the positive likelihood ratio rather than by specificity alone; here LR+ = %.2f, which provides moderate evidence for disease after a positive result."), lr_pos),
                        weak = sprintf(.("Rule-in power is governed by the positive likelihood ratio rather than by specificity alone; here LR+ = %.2f, which provides weak evidence for disease after a positive result."), lr_pos),
                        minimal = sprintf(.("Rule-in power is governed by the positive likelihood ratio rather than by specificity alone; here LR+ = %.2f, which provides minimal evidence for disease after a positive result."), lr_pos),
                        not_estimable = .("Rule-in power is governed by the positive likelihood ratio rather than by specificity alone, and LR+ is not estimable here.")
                    )
                )
            } else if (spec >= 80) {
                parts <- c(parts,
                    sprintf(.("With %.1f%% specificity, approximately %.0f out of 100 healthy individuals will be correctly classified."),
                            spec, spec),
                    .("<strong>Interpretation:</strong> at this specificity an appreciable share of positive results are expected to be false positives.")
                )
            } else {
                parts <- c(parts,
                    sprintf(.("With %.1f%% specificity, up to %.0f out of 100 healthy individuals may test positive."),
                            spec, 100 - spec),
                    .("<strong>Interpretation:</strong> at this specificity a large share of positive results are expected to be false positives.")
                )
            }

            # Add likelihood ratio interpretation if available
            if (plr_key != "not_estimable") {
                parts <- c(parts,
                    sprintf(switch(plr_key,
                        strong   = .("<br><br><strong>Your positive LR of %.2f</strong> provides <em>strong</em> evidence FOR disease when test is positive."),
                        moderate = .("<br><br><strong>Your positive LR of %.2f</strong> provides <em>moderate</em> evidence FOR disease when test is positive."),
                        weak     = .("<br><br><strong>Your positive LR of %.2f</strong> provides <em>weak</em> evidence FOR disease when test is positive."),
                        minimal  = .("<br><br><strong>Your positive LR of %.2f</strong> provides <em>minimal</em> evidence FOR disease when test is positive.")),
                        lr_pos),
                    if (lr_pos > 10) {
                        sprintf(.("A positive result increases disease probability substantially (multiplies pre-test odds by %.1fx)."),
                                lr_pos)
                    } else if (lr_pos >= 5) {
                        .("A positive result moderately increases disease probability.")
                    } else {
                        sprintf(.("A positive result multiplies the pre-test odds by %.2fx; the post-test probability also depends on the pre-test probability."),
                                lr_pos)
                    }
                )
            }

            if (nlr_key != "not_estimable") {
                parts <- c(parts,
                    sprintf(switch(nlr_key,
                        strong   = .("<strong>Your negative LR of %.2f</strong> provides <em>strong</em> evidence AGAINST disease when test is negative."),
                        moderate = .("<strong>Your negative LR of %.2f</strong> provides <em>moderate</em> evidence AGAINST disease when test is negative."),
                        weak     = .("<strong>Your negative LR of %.2f</strong> provides <em>weak</em> evidence AGAINST disease when test is negative."),
                        minimal  = .("<strong>Your negative LR of %.2f</strong> provides <em>minimal</em> evidence AGAINST disease when test is negative.")),
                        lr_neg),
                    if (lr_neg < 0.1) {
                        sprintf(.("A negative result substantially decreases disease probability (divides pre-test odds by %.1fx)."),
                                1/lr_neg)
                    } else if (lr_neg <= 0.2) {
                        .("A negative result moderately decreases disease probability.")
                    } else {
                        sprintf(.("A negative result multiplies the pre-test odds by %.2fx; the post-test probability also depends on the pre-test probability."),
                                lr_neg)
                    }
                )
            }

            # Overall summary of the pooled estimates. This describes what was
            # computed; it deliberately issues no fitness-for-use verdict, and it
            # is qualified by the between-study heterogeneity the same analysis
            # measured - a single pooled pair does not describe any one setting
            # when the prediction interval is wide.
            if (sens >= 90 && spec >= 90) {
                parts <- c(parts,
                    sprintf(.("<br><br><strong>Overall Summary:</strong> pooled sensitivity (%.1f%%) and pooled specificity (%.1f%%) both fall in the <em>excellent</em> band."),
                            sens, spec)
                )
            } else if (sens >= 80 && spec >= 80) {
                parts <- c(parts,
                    sprintf(.("<br><br><strong>Overall Summary:</strong> pooled sensitivity (%.1f%%) and pooled specificity (%.1f%%) both fall in the <em>good</em> band or above."),
                            sens, spec)
                )
            } else if (sens >= 90 || spec >= 90) {
                parts <- c(parts,
                    sprintf(.("<br><br><strong>Overall Summary:</strong> performance is <em>asymmetric</em> - pooled sensitivity (%.1f%%) and pooled specificity (%.1f%%) fall in different performance bands."),
                            sens, spec)
                )
            } else {
                parts <- c(parts,
                    sprintf(.("<br><br><strong>Overall Summary:</strong> pooled sensitivity (%.1f%%) and pooled specificity (%.1f%%) do not both reach the <em>good</em> band."),
                            sens, spec)
                )
            }

            # Qualify the summary with the measured between-study heterogeneity.
            # The flag comes from the fitted between-study variance, not from the
            # width of the prediction interval: at small k the interval is wide
            # even when the studies are identical.
            if (isTRUE(private$.heterogeneity_substantial)) {
                parts <- c(parts,
                    .("Between-study heterogeneity is substantial - the prediction interval spans a wide range of accuracy - so this pooled pair does not describe any single population or laboratory. Read the prediction region and the subgroup or meta-regression results rather than the pooled point.")
                )
            } else if (!is.null(private$.pooled_sens_pi)) {
                parts <- c(parts,
                    .("These are pooled estimates from the included studies only; how far they carry to another setting depends on the prediction region and on how comparable the positivity thresholds, patient spectrum and reference standards are.")
                )
            } else {
                parts <- c(parts,
                    .("These are pooled estimates from the included studies only. This model produces no prediction interval, so nothing here describes how much the studies differ from each other; how far the estimates carry to another setting cannot be judged from them alone.")
                )
            }

            return(paste(parts, collapse = " "))
        },

        # Populate About This Analysis panel
        .populateAboutPanel = function() {

            html <- "
            <div class='about-panel' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4> About Diagnostic Test Meta-Analysis</h4>

                <div style='margin: 15px 0;'>
                    <h5>What This Analysis Does</h5>
                    <p>Combines results from multiple diagnostic accuracy studies to estimate overall test performance through:</p>
                    <ul>
                        <li> <strong>Bivariate modeling</strong> - Jointly analyzes sensitivity and specificity</li>
                        <li> <strong>Proportional-hazards SROC modeling</strong> - Models the trade-off between sensitivity and false-positive rate</li>
                        <li> <strong>Heterogeneity assessment</strong> - Evaluates consistency across studies</li>
                        <li> <strong>Publication bias</strong> - Checks for selective reporting</li>
                    </ul>
                </div>

                <div style='margin: 15px 0; background-color: rgba(33, 152, 239, 0.13); padding: 15px; border-radius: 5px; border-left: 4px solid #2196F3; color: inherit;'>
                    <h5> Understanding Bivariate and Proportional-Hazards SROC Models</h5>
                    <p><strong>These models answer related questions using different parameterizations:</strong></p>

                    <p><strong>Bivariate Random-Effects Model (Recommended Primary Approach):</strong></p>
                    <ul>
                        <li> Provides <em>intuitive</em> pooled sensitivity and specificity estimates</li>
                        <li> Accounts for correlation between sensitivity and specificity</li>
                        <li> Directly interpretable for clinical decision-making</li>
                        <li> Preferred for meta-analyses with <em>homogeneous thresholds</em></li>
                        <li> <strong>Use this when:</strong> Studies use the same diagnostic threshold</li>
                    </ul>

                    <p><strong>Holling Proportional-Hazards SROC Model:</strong></p>
                    <ul>
                        <li> Relates sensitivity (<em>p</em>) and false-positive rate (<em>u</em>) through <em>u</em><sup>theta</sup> = <em>p</em></li>
                        <li> Reports theta as the diagnostic accuracy parameter</li>
                        <li> Reports tau<sup>2</sup> as between-study variation in diagnostic accuracy</li>
                        <li> Uses adjusted profile maximum likelihood and is suitable for smaller study sets</li>
                        <li> Is distinct from the Rutter-Gatsonis HSROC model</li>
                    </ul>

                    <p><strong> Clinical Insight:</strong> The plotted SROC curve is derived from the <em>bivariate model</em>. The proportional-hazards SROC table is a separate compact model of diagnostic accuracy and between-study variation; it should not be interpreted as a Rutter-Gatsonis HSROC threshold/accuracy table.</p>
                </div>

                <div style='margin: 15px 0;'>
                    <h5>When to Use This Analysis</h5>
                    <ul>
                        <li> Evaluating AI algorithms for pathology diagnosis</li>
                        <li> Assessing biomarker diagnostic accuracy</li>
                        <li> Comparing imaging modalities</li>
                        <li> Synthesizing evidence for clinical guidelines</li>
                    </ul>
                </div>

                <div style='margin: 15px 0; background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; color: inherit;'>
                    <h5> Key Requirements & Assumptions</h5>
                    <ul>
                        <li>Minimum 3 studies with 2[[TIMES]]2 diagnostic data</li>
                        <li>Studies should evaluate the same test and target condition</li>
                        <li>Reference standard should be consistent across studies</li>
                        <li>Patient spectrum should be clinically relevant</li>
                    </ul>
                </div>

                <div style='margin: 15px 0;'>
                    <h5> Quick Start Guide</h5>
                    <ol>
                        <li><strong>Step 1:</strong> Select your study identifier variable</li>
                        <li><strong>Step 2:</strong> Assign TP, FP, FN, TN count variables</li>
                        <li><strong>Step 3:</strong> Choose analysis options (bivariate recommended)</li>
                        <li><strong>Step 4:</strong> Review pooled estimates and heterogeneity</li>
                        <li><strong>Step 5:</strong> Interpret in your clinical context</li>
                    </ol>
                </div>

                <div style='background-color: rgba(33, 163, 188, 0.21); padding: 15px; border-radius: 5px; margin: 15px 0; color: inherit;'>
                    <p><strong> Tip:</strong> Start with the bivariate model and forest plot to understand overall performance, then explore heterogeneity sources with meta-regression if needed.</p>
                </div>
            </div>
            "

            self$results$about$setContent(private$.renderSymbols(html))
        },

        # Optimized data preparation with caching
        .prepareAnalysisData = function(meta_data) {

            # No cache layer: .run() set .data_cache_valid <- FALSE on entry and
            # called this exactly once, so the cache-hit branch could never be
            # taken and the private fields behind it were a round-trip to
            # nowhere. The results are returned directly.
            if (is.null(meta_data) || nrow(meta_data) == 0) {
                private$.continuity_correction <- FALSE
                return(list(
                    analysis_data = meta_data,
                    mada_data = data.frame(),
                    continuity_correction = FALSE
                ))
            }

            analysis_data <- meta_data
            numeric_cols <- c("tp", "fp", "fn", "tn")

            for (col in numeric_cols) {
                analysis_data[[col]] <- as.numeric(analysis_data[[col]])
            }

            # Apply zero-cell correction based on user choice
            correction_method <- self$options$zero_cell_correction %||% "none"
            correction_flags <- rep(FALSE, nrow(analysis_data))
            corrected_studies <- character(0)

            for (i in seq_len(nrow(analysis_data))) {
                row_counts <- as.numeric(analysis_data[i, numeric_cols])
                if (any(!is.finite(row_counts))) {
                    next
                }
                if (any(row_counts < 0)) {
                    next
                }

                has_zero <- any(row_counts == 0)

                if (has_zero && correction_method != "none") {
                    if (correction_method == "constant") {
                        # +0.5 to all four cells of the affected study
                        analysis_data[i, numeric_cols] <- row_counts + 0.5
                        correction_flags[i] <- TRUE
                        corrected_studies <- c(corrected_studies, as.character(analysis_data[i, "study"]))

                    } else if (correction_method == "zero_cells") {
                        # Add 0.5 only to the zero cells themselves. NOTE: this
                        # is NOT Sweeting's "treatment-arm" correction (which
                        # adds to all cells of the affected study) - the option
                        # was renamed to say what it actually does.
                        corrected_row <- row_counts
                        corrected_row[row_counts == 0] <- 0.5
                        analysis_data[i, numeric_cols] <- corrected_row
                        correction_flags[i] <- TRUE
                        corrected_studies <- c(corrected_studies, as.character(analysis_data[i, "study"]))

                    } else if (correction_method == "reciprocal_n") {
                        # Add 1/N to all cells, N = total study size. NOT
                        # Sweeting's "empirical" correction - renamed to say
                        # what it actually does.
                        total_n <- sum(row_counts, na.rm = TRUE)
                        if (total_n > 0) {
                            correction <- 1 / total_n
                            analysis_data[i, numeric_cols] <- row_counts + correction
                            correction_flags[i] <- TRUE
                            corrected_studies <- c(corrected_studies, as.character(analysis_data[i, "study"]))
                        }
                    }
                }
            }

            # Store correction info for reporting
            private$.correction_method_used <- correction_method
            private$.corrected_study_names <- corrected_studies

            private$.continuity_correction <- any(correction_flags)

            # `corrected_rows` used to be returned here and nowhere else - no
            # caller read it, and the two early returns above omitted it, so the
            # function had an inconsistent shape for a field nobody wanted.
            list(
                analysis_data = analysis_data,
                mada_data = data.frame(
                    TP = analysis_data$tp,
                    FP = analysis_data$fp,
                    FN = analysis_data$fn,
                    TN = analysis_data$tn
                ),
                continuity_correction = private$.continuity_correction
            )
        },

        # Plot explanation functions
        .populateForestPlotExplanation = function() {
            html <- "
            <div class='plot-explanation' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4> Forest Plot Interpretation Guide</h4>

                <div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>What This Plot Shows</h5>
                    <p><strong>Forest Plot:</strong> Displays individual study results for sensitivity and specificity with confidence intervals. Each study is represented by a point (estimate) with horizontal lines (confidence intervals).</p>

                    <ul>
                        <li><strong>Left Panel (Sensitivity):</strong> Proportion of diseased cases correctly identified</li>
                        <li><strong>Right Panel (Specificity):</strong> Proportion of healthy cases correctly identified</li>
                        <li><strong>Horizontal Lines:</strong> Wilson score confidence intervals (at the chosen confidence level) showing precision of estimates</li>
                        <li><strong>Row order:</strong> studies are sorted by their mean of sensitivity and specificity, so each study keeps the same row in both panels and can be read across. Within one panel the rows are therefore <em>not</em> ranked by that panel's own metric</li>
                        <li><strong>Scale:</strong> both panels run from 0 to 1; the tables report the same quantities as percentages</li>
                    </ul>
                </div>

                <div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Clinical Interpretation</h5>
                    <ul>
                        <li><strong>Consistent Results:</strong> Points clustered together = low heterogeneity</li>
                        <li><strong>Wide Spread:</strong> Points scattered = high heterogeneity (investigate sources)</li>
                        <li><strong>Narrow CIs:</strong> Large studies with precise estimates</li>
                        <li><strong>Wide CIs:</strong> Small studies with less precise estimates</li>
                    </ul>
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> Quick Assessment Tips</h5>
                    <ul>
                        <li>Look for outlier studies (points far from others)</li>
                        <li>Check if confidence intervals overlap substantially</li>
                        <li>Consider whether variation reflects true differences or chance</li>
                        <li>Use this plot to identify studies for sensitivity analysis</li>
                    </ul>
                </div>
            </div>
            "

            self$results$forestplot_explanation$setContent(html)
        },

        .populateSROCPlotExplanation = function() {
            html <- "
            <div class='plot-explanation' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4> Summary ROC Plot Interpretation Guide</h4>

                <div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>What This Plot Shows</h5>
                    <p><strong>SROC Plot:</strong> Summary Receiver Operating Characteristic curve showing the trade-off between sensitivity and specificity across all studies.</p>

                    <ul>
                        <li><strong>X-axis:</strong> False Positive Rate (1 - Specificity) - lower is better</li>
                        <li><strong>Y-axis:</strong> True Positive Rate (Sensitivity) - higher is better</li>
                        <li><strong>Individual Studies:</strong> Circles sized by sample size</li>
                        <li><strong>Pooled Estimate:</strong> Large triangle showing meta-analytic summary</li>
                        <li><strong>Confidence region (dashed):</strong> how precisely the POOLED point is estimated. It shrinks as more studies are added.</li>
                        <li><strong>Prediction region (dotted):</strong> where the accuracy of a FUTURE study in a new setting is expected to fall. It includes between-study heterogeneity and does NOT shrink with more studies.</li>
                    </ul>
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Confidence region vs prediction region</h5>
                    <p>These answer different questions and are routinely confused. A tight
                    <strong>confidence</strong> region means the pooled estimate is well determined; it says
                    nothing about whether the assay will perform consistently. A wide
                    <strong>prediction</strong> region means that even though the average is known precisely,
                    the next study - or your laboratory - could see materially different sensitivity and
                    specificity. <strong>For deciding whether to adopt an assay, the prediction region is the
                    relevant one.</strong> When the two differ greatly, between-study heterogeneity dominates
                    and the pooled point alone should not drive the decision.</p>
                </div>

                <div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Clinical Interpretation</h5>
                    <ul>
                        <li><strong>Upper Left Corner:</strong> Ideal performance (high sensitivity, low false positive rate)</li>
                        <li><strong>Diagonal Line:</strong> Represents random chance (no discriminative ability)</li>
                        <li><strong>Above Diagonal:</strong> Better than chance performance</li>
                        <li><strong>Point Scatter:</strong> Studies clustered tightly = consistent test performance</li>
                    </ul>
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> Quick Assessment Tips</h5>
                    <ul>
                        <li>Closer to upper-left corner = better overall diagnostic accuracy</li>
                        <li>Wide scatter of points = substantial between-study heterogeneity</li>
                        <li>Triangle position shows where your pooled test performance lies</li>
                        <li>Compare triangle position to individual studies for consistency</li>
                    </ul>
                </div>

                <div style='background-color: rgba(33, 163, 188, 0.21); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> Clinical Decision Making</h5>
                    <p><strong>Use this plot to:</strong> Visualize test performance trade-offs, identify optimal operating points, and assess consistency across different study populations and settings.</p>
                </div>
            </div>
            "

            self$results$srocplot_explanation$setContent(html)
        },

        .populateFunnelPlotExplanation = function() {
            html <- "
            <div class='plot-explanation' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4> Funnel Plot Interpretation Guide</h4>

                <div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>What This Plot Shows</h5>
                    <p><strong>Funnel Plot:</strong> Assesses publication bias by plotting study precision against effect size (log diagnostic odds ratio).</p>

                    <ul>
                        <li><strong>X-axis:</strong> Log Diagnostic Odds Ratio (effect size)</li>
                        <li><strong>Y-axis:</strong> 1/\u221A(effective sample size), reversed so the most precise studies sit at the top (Deeks' funnel plot)</li>
                        <li><strong>Each Point:</strong> One study in your meta-analysis</li>
                        <li><strong>Expected Pattern:</strong> Inverted funnel shape if no bias present</li>
                    </ul>
                </div>

                <div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> Funnel Asymmetry Indicators</h5>
                    <ul>
                        <li><strong>Asymmetric Funnel:</strong> Studies concentrated on one side at the imprecise end of the plot</li>
                        <li><strong>Gap in Lower Region:</strong> Small studies with low accuracy absent</li>
                        <li><strong>Deeks' Test p < 0.05:</strong> Statistical evidence of funnel plot asymmetry - evidence of asymmetry, not of publication bias; see the causes listed below</li>
                    </ul>
                </div>

                <div style='background-color: rgba(33, 162, 64, 0.19); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> Features Consistent With a Symmetric Funnel</h5>
                    <ul>
                        <li><strong>Symmetric Funnel:</strong> Studies distributed evenly on both sides</li>
                        <li><strong>Deeks' Test p [[GE]] 0.05:</strong> No statistical evidence of asymmetry</li>
                        <li><strong>Small Studies Present:</strong> Range of precision levels represented</li>
                        <li><em>These features do not establish that publication bias is absent: asymmetry tests have low power, and are unreliable with fewer than 10 studies.</em></li>
                    </ul>
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> Interpretation Caveats</h5>
                    <ul>
                        <li><strong>Small Sample:</strong> Funnel plot unreliable with &lt;10 studies</li>
                        <li><strong>Heterogeneity:</strong> Clinical differences can mimic publication bias</li>
                        <li><strong>Other Causes:</strong> Different thresholds, narrower patient spectra in small studies, language or database bias, or chance</li>
                        <li><strong>Action Needed:</strong> If the funnel is asymmetric, look for a clinical explanation first, then for unpublished studies</li>
                    </ul>
                </div>
            </div>
            "

            self$results$funnelplot_explanation$setContent(
                private$.renderSymbols(html)
            )
        }
    ),
    public = list(
        #' @description
        #' Generate R source code for diagnostic test meta-analysis
        #' @return Character string with reproducible R syntax
        asSource = function() {
            required <- c(
                self$options$study,
                self$options$true_positives,
                self$options$false_positives,
                self$options$false_negatives,
                self$options$true_negatives
            )
            if (any(vapply(required, is.null, logical(1))))
                return("")

            # Variable options are emitted as quoted R strings. deparse() handles
            # spaces, quotes, backslashes and Unicode safely. Other options retain
            # jmvcore's canonical sourcification and default suppression.
            args <- character(0)
            for (option in private$.options$options) {
                if (identical(option$name, "data"))
                    next

                if (inherits(option, "OptionVariable") ||
                    inherits(option, "OptionVariables")) {
                    value <- option$value
                    if (!is.null(value)) {
                        args <- c(
                            args,
                            paste0(
                                option$name,
                                " = ",
                                paste0(deparse(value), collapse = "")
                            )
                        )
                    }
                } else {
                    source_arg <- private$.sourcifyOption(option)
                    if (!identical(source_arg, ""))
                        args <- c(args, source_arg)
                }
            }

            package <- utils::packageName()
            if (is.null(package))
                package <- "ClinicoPath"

            paste0(
                package,
                "::diagnosticmeta(\n    data = data,\n    ",
                paste(args, collapse = ",\n    "),
                ")"
            )
        }
    )
)
