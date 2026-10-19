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
        # Numeric NA is serialized as NaN by this jmvcore build. At the table
        # boundary, preserve named entries but encode unavailable cells as NULL.
        .tableValues = function(values) {
            lapply(values, function(value) {
                if (is.numeric(value) && length(value) == 1L && is.na(value))
                    NULL else value
            })
        },

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
        # TRUE when the bivariate fit did not converge even with 5000 iterations.
        .biv_unconverged = FALSE,
        # Ratio-scale confidence intervals of LR+ and LR- from the bivariate fit,
        # so the summary can qualify an LR grading the interval does not support.
        .pooled_lr_ci = NULL,
        # fp / fn: TRUE when at least 75% of studies have no false positives /
        # false negatives, so specificity, LR+ (or sensitivity, LR-) and the
        # DOR are set by the continuity correction. The summary then does not
        # grade that likelihood ratio.
        .zero_margin = list(fp = FALSE, fn = FALSE),
        # TRUE when two or more rows share a study identifier.
        .repeated_ids = FALSE,
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
        # library-audit 2026-09-22 OncoPath [LOW] REJECTED: Notice renders single-line plain text and dynamic
        #   insert() in .run() accumulates across runs (Group$remove cannot drop items) (guide section 13)
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

            # Fixed row structure for likelihood ratio reference guide
            private$.populateLikelihoodRatioGuide()

            # Pre-scaffold fixed row structure for predictive values table across benchmark prevalences
            prevalences <- c(0.05, 0.10, 0.20, 0.50)
            for (prev in prevalences) {
                self$results$predictiveValues$addRow(
                    rowKey = paste0("p_", as.integer(prev * 100)),
                    values = private$.tableValues(list(prevalence = prev, ppv = NA_real_, npv = NA_real_))
                )
            }

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
                    values = private$.tableValues(list(parameter = biv_parameters[[biv_key]])))

            # library-audit 2026-09-16 meddecide [LOW] DONE (same class): publicationbias has a fixed row set, so .init()
            # scaffolds the rows and .run() fills them with setRow()
            # One row, Deeks' test, whenever publication bias is requested. .run()
            # fills it, or leaves it blank beside the note that says why.
            if (isTRUE(self$options$publication_bias))
                self$results$publicationbias$addRow(
                    rowKey = "deeks_test",
                    values = private$.tableValues(list(test = .("Deeks' Funnel Plot Asymmetry Test"))))

            # Same class of fix for the other two fixed row sets. Building these
            # in .run() with deleteRows() + addRow() made the tables appear
            # empty and then visibly restructure on every run.
            if (isTRUE(self$options$heterogeneity_analysis)) {
                het_rows <- list(sensitivity = .("Sensitivity"),
                                 specificity = .("Specificity"))
                for (het_key in names(het_rows))
                    self$results$heterogeneity$addRow(
                        rowKey = het_key,
                        values = private$.tableValues(list(measure = het_rows[[het_key]])))
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
                        values = private$.tableValues(list(parameter = phm_rows[[phm_key]])))
            }

        },
        
        .run = function() {

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
            private$.biv_unconverged <- FALSE
            private$.pooled_lr_ci <- NULL
            private$.zero_margin <- list(fp = FALSE, fn = FALSE)
            private$.repeated_ids <- FALSE

            # Check if data is ready
            if (is.null(self$data)) {
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

            # library-audit 2026-09-22 OncoPath [MEDIUM] DONE: setting up an analysis is not an error.
            #   Nothing assigned emits no notice - the instructions panel already names every variable
            #   and what it is for. A partial selection gets an INFO naming the boxes that are still
            #   empty. The red ERROR that used to sit here greeted every user the moment they opened
            #   the analysis, telling them they had done something wrong (guide section 25).
            #   Each sentence is a whole translatable unit joined with paste(), not a spliced noun
            #   phrase, so it can be inflected correctly in every language.
            if (!all_provided) {
                self$results$instructions$setVisible(TRUE)
                n_set <- sum(!vapply(list(study_var, tp_var, fp_var, fn_var, tn_var),
                                     is.null, logical(1)))
                if (n_set > 0L) {
                    still_empty <- c(
                        if (is.null(study_var)) .("Study identifier is still empty."),
                        if (is.null(tp_var)) .("True positives is still empty."),
                        if (is.null(fp_var)) .("False positives is still empty."),
                        if (is.null(fn_var)) .("False negatives is still empty."),
                        if (is.null(tn_var)) .("True negatives is still empty.")
                    )
                    # With no rows, filling the remaining boxes cannot produce a
                    # result, so do not promise one; say why instead.
                    no_rows <- nrow(self$data) == 0
                    private$.addNotice("INFO", .("Keep going - a few variables to add"),
                        paste(c(still_empty,
                                if (!no_rows) .("Fill them in and the meta-analysis will appear.")),
                              collapse = " "))
                    if (no_rows)
                        private$.addNotice("WARNING", .("Data contains no rows"),
                            .("The dataset contains no rows. Add data (or clear the active filter) and re-run."))
                }
                return()
            }

            # Variables assigned but no rows (an empty dataset, or a filter that
            # excludes every row): stay idle, as for an incomplete selection, but
            # say why nothing appears.
            if (nrow(self$data) == 0) {
                self$results$instructions$setVisible(TRUE)
                private$.addNotice("WARNING", .("Data contains no rows"),
                    .("The dataset contains no rows. Add data (or clear the active filter) and re-run."))
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

            # Infinity is not a missing count and would produce NaN in the
            # whole-number check below. Reject it before any count arithmetic.
            count_vars <- c(tp = tp_var, fp = fp_var, fn = fn_var, tn = tn_var)
            infinite <- is.infinite(as.matrix(meta_data[, names(count_vars)]))
            if (any(infinite)) {
                bad_vars <- unname(count_vars[colSums(infinite) > 0])
                bad_rows <- which(rowSums(infinite) > 0)
                jmvcore::reject("{}", code = "nonfinite_counts", msg = sprintf(
                    .("Infinite diagnostic counts in variable(s) %s, analysis row(s) %s. Enter finite whole-number counts or mark unavailable values as missing."),
                    paste(bad_vars, collapse = ", "),
                    paste(utils::head(bad_rows, 10), collapse = ", ")))
            }

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

            # Each reason is a whole sentence: a "(%s)" list of "%d with ..."
            # fragments cannot be translated into a language whose word order
            # or case endings differ from English.
            reasons <- character(0)
            if (sum(incomplete) > 0)
                reasons <- c(reasons, sprintf(.("Missing counts: %d study/studies."), sum(incomplete)))
            if (sum(negative) > 0)
                reasons <- c(reasons, sprintf(.("Negative counts: %d study/studies."), sum(negative)))
            if (sum(empty_arm) > 0)
                reasons <- c(reasons, sprintf(.("No diseased or no non-diseased participants: %d study/studies."), sum(empty_arm)))

            # Counts must be whole numbers. Fractional values reach mada (which
            # rounds them silently), are truncated to integers in the study table,
            # and crash the no-bivariate summary path through sprintf("%d").
            n_fractional <- 0L
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
                        sum(fractional), original_n))
                    meta_data <- meta_data[!fractional, , drop = FALSE]
                    n_fractional <- sum(fractional)
                    reasons <- c(reasons, sprintf(.("Fractional counts: %d study/studies."), n_fractional))
                }
            }

            # One disclosure, written after every exclusion step: the notice used
            # to be written before the fractional-count step, so its "remaining"
            # count could exceed the number of studies actually analysed.
            n_excluded <- sum(drop) + n_fractional
            if (n_excluded > 0) {
                private$.addNotice("WARNING", .("Studies excluded"), paste(sprintf(
                    if (nrow(meta_data) >= 3)
                        .("%d of %d studies were excluded before analysis. All results below are based on the remaining %d studies.")
                    else
                        .("%d of %d studies were excluded before analysis, leaving %d - too few to pool."),
                    n_excluded, original_n, nrow(meta_data)), paste(reasons, collapse = " ")))
            }
            # Say WHY when the fractional exclusion empties the analysis; the
            # generic "at least 3 studies" message below would not mention counts.
            if (n_fractional > 0 && nrow(meta_data) < 3)
                jmvcore::reject(
                    .fmt(.("Cell counts must be whole numbers, and {n} study/studies have fractional counts, which leaves too few studies. Enter the number of participants in each cell of the 2x2 table (TP, FP, FN, TN)."),
                         n = n_fractional),
                    code = "fractional_counts")

            # Two studies sharing an identifier (or none at all) are drawn on one
            # forest row, so one silently hides behind the other.
            if (nrow(meta_data) > 0) {
                unnamed <- is.na(meta_data$study) | !nzchar(trimws(meta_data$study))
                # Repeats are counted among the NAMED studies, before the missing
                # identifiers are relabelled: as "Unnamed study" they repeated each
                # other, and were counted and listed twice.
                named <- meta_data$study[!unnamed]
                repeated_ids <- unique(named[duplicated(named)])
                if (length(repeated_ids) > 0) {
                    # Relabelling fixes the forest plot, not the analysis: rows that
                    # share an identifier are usually one cohort read several times,
                    # and pooling them as separate studies double-counts participants.
                    private$.repeated_ids <- TRUE
                    private$.addNotice("STRONG_WARNING", .("Repeated study identifiers"), sprintf(
                        .("%d rows share %d study identifier(s) (%s). Rows that share an identifier usually describe one cohort read several times - by several pathologists, at several cut-offs or by several algorithms - and so share participants. They are pooled here as separate studies, which counts those participants more than once and makes the confidence intervals too narrow. Keep one pre-specified 2x2 table per independent study, for example the primary reader or the pre-specified threshold."),
                        sum(named %in% repeated_ids), length(repeated_ids),
                        paste(utils::head(repeated_ids, 5), collapse = ", ")))
                }
                if (any(unnamed)) {
                    private$.addNotice("WARNING", .("Missing study identifiers"), sprintf(
                        .("%d study/studies have no identifier; they are labelled 'Unnamed study' in the tables and the forest plot."),
                        sum(unnamed)))
                }
                meta_data$study[unnamed] <- .("Unnamed study")
                if (anyDuplicated(meta_data$study) > 0)
                    meta_data$study <- make.unique(meta_data$study, sep = " #")
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

            # Under the default setting the bivariate and proportional-hazards
            # models add 0.5 to all four cells of each zero-cell study (mada,
            # correction.control = "single"). The univariate heterogeneity and
            # meta-regression models get the same counts, so every model rests on
            # every study. They used to drop those studies instead - exactly the
            # 100% sensitivity or specificity studies - which biased covariate
            # effects (toward the null, or away from it when one level consisted
            # of small series). Under a user correction no zero cell remains.
            model_data <- analysis_data
            count_cells <- c("tp", "fp", "fn", "tn")
            single_corrected <- rowSums(model_data[, count_cells, drop = FALSE] == 0) > 0
            if (any(single_corrected))
                model_data[single_corrected, count_cells] <- model_data[single_corrected, count_cells] + 0.5
            # Per row, so a table can count the studies it actually used.
            model_data$zero_corrected <- single_corrected

            # Disclose the zero-cell handling where it is always visible. The
            # previous disclosures lived only on the bivariate table (hidden when
            # that analysis is off) and in the optional summary panel.
            zero_cell_studies <- if (nrow(mada_data) > 0)
                sum(rowSums(mada_data[, c("TP", "FP", "FN", "TN"), drop = FALSE] == 0) > 0) else 0L
            if (!isTRUE(prepared_data$continuity_correction) && zero_cell_studies > 0) {
                private$.addNotice("INFO", .("Zero cells present"), sprintf(
                    .("%d study/studies contain a zero cell. No correction is applied to the data: every model (bivariate, proportional-hazards SROC, heterogeneity and meta-regression) adds 0.5 to all four cells of those studies, so each model uses every study. Deeks' test and the funnel plot use the raw counts and add 0.5 to every study."),
                    zero_cell_studies))
            }
            if (isTRUE(prepared_data$continuity_correction)) {
                corrected_names <- private$.corrected_study_names
                private$.addNotice("INFO", .("Zero-cell correction applied"), sprintf(
                    .("A zero-cell correction (%s) was applied to %d study/studies (%s). The corrected counts are used by the pooled models and by the forest and SROC plots. Deeks' test and the funnel plot use the raw counts with their own 0.5 correction, and the Individual Study Results table shows the raw counts."),
                    private$.correctionLabel(private$.correction_method_used),
                    length(corrected_names),
                    paste(corrected_names, collapse = ", ")))
                # +1/N is not a published correction. A cell of 1/N gives the
                # study a logit variance of about N, i.e. almost no weight, and
                # the zero-cell studies are the perfect-margin ones - so the
                # pooled accuracy drops. 2026-10-19 review (repro_DM-3.R, 200
                # meta-analyses per scenario, k = 15, tau = 0.5): sensitivity CI
                # coverage 0.55 / 0.31 against 0.69 / 0.58 under the default.
                if (identical(private$.correction_method_used, "reciprocal_n"))
                    private$.addNotice("WARNING", .("The +1/N correction down-weights zero-cell studies"), sprintf(
                        .("%d study/studies with a zero cell were corrected by adding 1/N to every cell, where N is the study size. A cell of 1/N gives such a study a logit variance of about N, so the models give it almost no weight, and these are the studies with 100%% sensitivity or specificity: the pooled accuracy is pulled toward lower values. In simulations of high-accuracy tests the 95%% confidence interval for pooled sensitivity covered the true value in only 31-55%% of meta-analyses under this option, against 58-69%% under the default. This is not a published correction; use the default unless you are deliberately running a sensitivity analysis."),
                        length(corrected_names)))

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
                # "&", is left as the user typed it. .rocLabel() then maps
                # "[ ]" to "( )" - setNote() translates its text, and a catalog
                # miss reads " [..." as a msgctxt and drops it with the rest of
                # the note - and breaks "<" before a letter, which the client
                # would parse as a tag and blank.
                honoured_tags <- "</?[[:space:]]*(i|em|b|strong|sub|sup)([[:space:]][^>]*)?>"
                shown_names <- .rocLabel(gsub(honoured_tags, "",
                                              utils::head(corrected_names, 3),
                                              ignore.case = TRUE), note = TRUE)
                # "..." when the list is cut: the count says 5 and three names
                # used to read as the whole list.
                if (length(corrected_names) > 3) shown_names <- c(shown_names, "...")
                zero_note <- sprintf(
                    .("Zero-cell correction applied (%s) to %d of %d studies (%s). Results should be interpreted with caution as corrections can introduce bias, especially in large studies."),
                    private$.correctionLabel(private$.correction_method_used),
                    length(corrected_names), nrow(meta_data),
                    paste(shown_names, collapse = ", "))
                # 'constant' and the default fit the same models (both add 0.5
                # to all four cells of each zero-cell study), so comparing them
                # shows no change and reads as robustness.
                if (identical(private$.correction_method_used, "constant"))
                    zero_note <- paste(zero_note, .("This setting fits the same models as the default, which also adds 0.5 to all four cells of each zero-cell study; only the study points of the forest and SROC plots differ. To see how sensitive the result is to the correction, compare with '+0.5 to the zero cells only'."))
                self$results$bivariateresults$setNote("zero_cell_warning", zero_note)
            }

            private$.checkpoint()

            # Perform bivariate meta-analysis when enabled
            if (isTRUE(self$options$bivariate_analysis)) {
                tryCatch({
                    private$.performBivariateMetaAnalysis(analysis_data = analysis_data,
                                                          mada_data = mada_data,
                                                          raw_data = meta_data)
                }, error = function(e) {
                    private$.pooled_sensitivity <- NULL
                    private$.pooled_specificity <- NULL
                    # The model is stored before its summary is read, so an error
                    # after that point left a model behind for the SROC plot to
                    # draw from.
                    private$.biv_model <- NULL
                    private$.pooled_lr_ci <- NULL
                    # Table notes are a plain-text sink (limited HTML allow-list),
                    # so the message must NOT be HTML-escaped: escaping turned
                    # "object 'x' not found" into "object &apos;x&apos; not found (numeric entity was rendered literally)".
                    self$results$bivariateresults$setNote("error", sprintf(.("Bivariate analysis error: %s"), e$message))
                    private$.addNotice("ERROR", .("Bivariate model failed"), sprintf(
                        .("The Reitsma bivariate model could not be fitted (%s). Pooled sensitivity and specificity are unavailable; the SROC plot and summary fall back to descriptive output."),
                        e$message))
                })
            } else {
                private$.pooled_sensitivity <- NULL
                private$.pooled_specificity <- NULL
            }
            # Outside the bivariate tryCatch: an error while rendering the
            # summary is not a failed model fit, and must not null the model
            # or sit as "Bivariate model failed" beside filled rows. With no
            # pooled pair .generateSummary() falls back to the basic summary.
            # Html content survives an untick unless cleared - same reason as
            # the .init() panels.
            # Its own guard: the summary raises no reject(), and a failure here
            # leaves every computed table and plot standing.
            if (isTRUE(self$options$show_analysis_summary))
                tryCatch(private$.generateSummary(meta_data), error = function(e) {
                    self$results$summary$setContent("")
                    private$.addNotice("WARNING", .("Analysis summary unavailable"), sprintf(
                        .("The Analysis Summary panel could not be written (%s). The tables and plots are not affected."),
                        e$message))
                })
            else
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
                    private$.performHeterogeneityAnalysis(analysis_data = model_data)
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
                                                   analysis_data = model_data)
                }, error = function(e) {
                    # Table notes are plain text - do not HTML-escape.
                    self$results$metaregression$setNote("error", sprintf(.("Meta-regression error: %s"), e$message))
                })
            }

            private$.checkpoint()

            # Perform publication bias assessment
            if (isTRUE(self$options$publication_bias)) {
                # Deeks' test works on the RAW validated counts under every
                # setting, with its own uniform 0.5 correction and its own
                # sparse-data rule. Given the user-corrected counts it saw no zero
                # cell, so the rule never fired and per-study corrections - which
                # build a size-related trend into the test's outcome - reported
                # "Asymmetry detected" in up to 55-73% of null meta-analyses.
                tryCatch({
                    private$.performPublicationBiasAssessment(analysis_data = meta_data)
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
                # Raw counts, as Deeks' test above: the plot carries that test.
                private$.populateFunnelPlot(meta_data)
            }

            # Populate individual studies table
            if (self$options$show_individual_studies) {
                private$.populateIndividualStudies(meta_data)
            }

            # Populate predictive values table
            if (isTRUE(self$options$show_interpretation)) {
                private$.populatePredictiveValues()
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
        
        .performBivariateMetaAnalysis = function(analysis_data, mada_data, raw_data) {

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
            #
            # mvmeta's "convergence not reached" warning never reached the
            # jamovi user, so a fit that stopped at the iteration limit was
            # reported as if it had converged. Detect it (warning or
            # converged = FALSE; the non-iterative methods report NULL, which is
            # not a failure), refit once with a large limit, and say so.
            fit_reitsma <- function(control = list()) {
                hit_limit <- FALSE
                model <- withCallingHandlers(
                    .quietly(mada::reitsma(
                        mada_data, method = method_used,
                        correction = 0.5,
                        correction.control = if (identical(correction_method, "none")) "single" else "all",
                        control = control
                    ), deprecation_pattern = "non zero decimal places"),
                    warning = function(w) {
                        if (grepl("convergence not reached", conditionMessage(w), fixed = TRUE)) {
                            hit_limit <<- TRUE
                            invokeRestart("muffleWarning")
                        }
                    })
                list(model = model, converged = !hit_limit && !isFALSE(model$converged))
            }
            biv_fit <- fit_reitsma()
            biv_retried <- FALSE
            if (!biv_fit$converged) {
                # An error here is a failed fit, reported as one by .run().
                biv_fit <- fit_reitsma(list(maxiter = 5000))
                biv_retried <- TRUE
            }
            biv_model <- biv_fit$model
            private$.biv_unconverged <- !biv_fit$converged
            if (biv_retried) {
                self$results$bivariateresults$setNote("convergence", if (biv_fit$converged)
                    .("The bivariate model needed more than the default number of iterations; it was refitted with a limit of 5000 and converged, and the estimates below come from that converged fit.")
                else
                    .("The bivariate model did NOT converge within 5000 iterations, so no estimates are reported: the last iterate is not the estimator's solution, and it changes with the iteration limit."))
                if (!biv_fit$converged)
                    private$.addNotice("STRONG_WARNING", .("Bivariate model did not converge"),
                        .("The bivariate (Reitsma) model did not converge, even with an iteration limit of 5000. No pooled sensitivity, specificity, likelihood ratio, prediction interval or SROC curve is reported, because they would come from an unfinished optimisation. Try the method-of-moments estimator (it is not iterative), check the studies for an extreme or repeated 2x2 table, or restrict the study set."))
            }

            # Same as the PHM model: an unconverged fit fills no cell, sets no
            # pooled value for the summary, interpretation and predictive
            # values, and leaves the SROC plot to draw the studies alone. The
            # notes and cautions about the pooled pair come after it for that
            # reason.
            if (!biv_fit$converged) return()

            # Only under "none": every user correction removes each zero cell
            # before this point (studies with no participants in an arm were
            # excluded), so mada's correction.control never meets a zero then.
            if (zero_present) {
                n_zero_studies <- sum(rowSums(mada_data[, c("TP", "FP", "FN", "TN")] == 0) > 0)
                bivariate_table_early <- self$results$bivariateresults
                bivariate_table_early$setNote("zero_cell", sprintf(
                    .("%d study/studies contain a zero cell. A continuity correction of 0.5 was applied to those studies only so the model could be fitted. Continuity corrections shift the pooled estimate. The option '+0.5 to all cells of zero-cell studies' fits the same model as this default, so to see how sensitive your result is, compare with '+0.5 to the zero cells only'."),
                    n_zero_studies))
            }

            # The Reitsma model is a bivariate NORMAL approximation to the logit
            # sensitivity/specificity pair. With small cells that approximation
            # is biased, and a continuity correction is doing part of the work:
            # on an 8-study set with 7 zero cells it gave 90.3%/94.0% where a
            # binomial GLMM gave 94.3%/96.2% - 4.0 and 2.2 points apart. Say so
            # when the data are actually sparse; this module offers no GLMM.
            #
            # Cells of 2-4 are NOT tolerable, as this comment used to say: a
            # study with few errors gets the largest within-study variance
            # (1/cell) and the least weight. 2026-10-19 review (repro_DM-1.R,
            # 300 meta-analyses per row, k = 15, tau = 0.5): sens 0.95 with a
            # mean of 5.3 false negatives per study gave 80% coverage of the
            # nominal 95% CI and the old 0-or-1 gate fired in 6% of them; sens
            # 0.90 with 4.0 gave 81% and 26%. Calibration of this gate
            # (dmfix/gate_calib.R): ">= 25% of studies with a cell < 5" fired in
            # 96-100% of those under-covering configurations and in 0-13% where
            # coverage was 0.90-0.95.
            sparse_cells <- mada_data[, c("TP", "FP", "FN", "TN"), drop = FALSE]
            tiny_rows <- rowSums(sparse_cells <= 1) > 0
            small_rows <- rowSums(sparse_cells < 5) > 0
            if (sum(tiny_rows) / nrow(mada_data) >= 0.25) {
                private$.addNotice("WARNING", .("Sparse data: pooled estimates may be biased"), sprintf(
                    .("%d of %d studies have a cell of 0 or 1. The bivariate model used here approximates the logit sensitivity and specificity as normal, which is biased when cells are that small, and any zero cell additionally needs a continuity correction. In simulations of high-accuracy tests with cells this small, the nominal 95%% confidence interval for the pooled value on the sparse margin covered the true value in only about 70%% of meta-analyses, or fewer, and the pooled value was biased toward lower accuracy. Treat the pooled pair and its interval as approximate, and confirm them in software offering a binomial bivariate (generalised linear mixed) model before relying on them."),
                    sum(tiny_rows), nrow(mada_data)))
            } else if (sum(small_rows) / nrow(mada_data) >= 0.25) {
                private$.addNotice("WARNING", .("Sparse data: pooled estimates may be biased"), sprintf(
                    .("%d of %d studies have a cell smaller than 5. The bivariate model used here approximates the logit sensitivity and specificity as normal, and with cells this small the studies with the fewest errors get the least weight. In simulations of high-accuracy tests with 2 to 8 false negatives per study, the nominal 95%% confidence interval for the pooled sensitivity covered the true value in only about 80%% of meta-analyses, and the pooled value was slightly too low. Read the interval as too narrow, and confirm the pooled pair in software offering a binomial bivariate (generalised linear mixed) model before relying on it."),
                    sum(small_rows), nrow(mada_data)))
            }

            # A margin with (almost) no errors: every such study enters the
            # model only after a continuity correction, so that margin's pooled
            # value, its CI, its tau-squared and the LR/DOR built on it are set
            # by the correction constant (0.5 -> 0.1 moves them by about an
            # order of magnitude). 2026-10-19 review, repro_DM-2.R: fp := 0 in
            # all 20 studies gave specificity 99.47 (99.02-99.71) and LR+ 153
            # (82-285), while 0 FP in 1983 non-diseased supports an exact lower
            # bound of 99.81%. The exact bound is computed from the raw totals
            # under a common value, which is what the fit itself claims there.
            raw_counts <- raw_data[, c("tp", "fp", "fn", "tn")]
            margin_share <- function(cell) mean(raw_counts[[cell]] == 0)
            exact_lower <- function(errors, total)
                100 * (1 - stats::qbeta(1 - (1 - conf_level) / 2, errors + 1, total - errors))
            level_txt <- format(100 * conf_level)
            private$.zero_margin <- list(fp = margin_share("fp") >= 0.75,
                                         fn = margin_share("fn") >= 0.75)
            if (private$.zero_margin$fp) {
                fp_tot <- sum(raw_counts$fp); nd_tot <- sum(raw_counts$fp + raw_counts$tn)
                private$.addNotice("STRONG_WARNING", .("Specificity set by the continuity correction"), sprintf(
                    .("%d of %d studies have no false positives. Each of them enters the model only after a continuity correction, so the pooled specificity, its confidence interval, its between-study variance, LR+ and the diagnostic odds ratio are set largely by the size of that correction rather than by the data: a smaller correction gives a higher specificity and a much larger LR+. The interval is not a data-derived precision. If all studies shared one specificity, the raw counts (%s false positives among %s non-diseased participants) would give an exact %s%% confidence interval starting at %.2f%%, and LR+ would have no upper limit the data can set. Report the raw counts and that bound alongside the pooled value, and do not read the LR+ grading as established."),
                    sum(raw_counts$fp == 0), nrow(raw_counts), format(fp_tot), format(nd_tot),
                    level_txt, exact_lower(fp_tot, nd_tot)))
                self$results$bivariateresults$setNote("zero_margin_fp",
                    .("Specificity, LR+ and DOR rest on the continuity correction: at least three quarters of the studies have no false positives, so their intervals are not data-derived (see the notices)."))
            }
            if (private$.zero_margin$fn) {
                fn_tot <- sum(raw_counts$fn); d_tot <- sum(raw_counts$fn + raw_counts$tp)
                private$.addNotice("STRONG_WARNING", .("Sensitivity set by the continuity correction"), sprintf(
                    .("%d of %d studies have no false negatives. Each of them enters the model only after a continuity correction, so the pooled sensitivity, its confidence interval, its between-study variance, LR- and the diagnostic odds ratio are set largely by the size of that correction rather than by the data: a smaller correction gives a higher sensitivity and a much smaller LR-. The interval is not a data-derived precision. If all studies shared one sensitivity, the raw counts (%s false negatives among %s diseased participants) would give an exact %s%% confidence interval starting at %.2f%%, and LR- would have no lower limit above zero the data can set. Report the raw counts and that bound alongside the pooled value, and do not read the LR- grading as established."),
                    sum(raw_counts$fn == 0), nrow(raw_counts), format(fn_tot), format(d_tot),
                    level_txt, exact_lower(fn_tot, d_tot)))
                self$results$bivariateresults$setNote("zero_margin_fn",
                    .("Sensitivity, LR- and DOR rest on the continuity correction: at least three quarters of the studies have no false negatives, so their intervals are not data-derived (see the notices)."))
            }

            # Three studies clear the hard floor above and are then pooled with
            # no caution at all, from a model that estimates FIVE parameters:
            # two means and three elements of the between-study covariance
            # matrix. The fit succeeds and prints a confident-looking interval
            # (85.7%, 79.2-90.4 on a three-study example), but the between-study
            # variance is barely identified and the interval is not to be
            # trusted at face value. The fixed-effect model estimates no
            # between-study variance, so it gets its own wording.
            if (nrow(mada_data) < 5) {
                private$.addNotice("STRONG_WARNING", .("Very few studies"), sprintf(
                    if (identical(method_used, "fixed"))
                        .("Only %d studies are pooled. The fixed-effect model estimates two pooled means and assumes there is no between-study variance, which this many studies cannot check. Treat the pooled pair as descriptive, report the individual studies alongside it, and do not use it to set a clinical threshold.")
                    else
                        .("Only %d studies are pooled. The bivariate model estimates five parameters (two pooled means plus three between-study variance components), so with this many studies the between-study variance is barely identified and the confidence intervals are narrower than the evidence warrants. Treat the pooled pair as descriptive, report the individual studies alongside it, and do not use it to set a clinical threshold."),
                    nrow(mada_data)))
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
                # Raised to 1e-3 (a logit SD of 0.03) and printed with %.4g
                # (2026-10-19 review, DM-5): Psi[FPR] = 0.00038 cleared 1e-4,
                # printed as "0.000" and sat beside "Correlation = 1.00".
                estimable <- all(diag(Psi_vc)[1:2] > 1e-3)
                vc_note <- sprintf(
                    .("Between-study variance components: tau-squared (logit sensitivity) = %.4g, tau-squared (logit false-positive rate) = %.4g."),
                    Psi_vc[1, 1], Psi_vc[2, 2])
                if (estimable) {
                    vc_note <- paste(vc_note, sprintf(
                        .("Correlation between logit sensitivity and logit specificity = %.2f. A strongly negative correlation suggests a threshold effect (studies trading sensitivity for specificity)."),
                        corr_ss))
                    # With few studies the correlation routinely lands on the +/-1
                    # boundary - a degenerate estimate, not evidence of a perfect
                    # relationship.
                    if (is.finite(corr_ss) && abs(corr_ss) > 0.99) {
                        vc_note <- paste(vc_note, if (nrow(mada_data) < 10)
                            .("The correlation is estimated at the boundary (+/-1), which with few studies usually means it cannot be estimated reliably - do not over-interpret it.")
                        else
                            .("The correlation is estimated at the boundary (+/-1), which usually means it cannot be estimated reliably - do not over-interpret it."))
                    }
                } else if (all(diag(Psi_vc)[1:2] <= 1e-3)) {
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
                # The model was stored above; leave no model for the SROC plot
                # to draw a pooled point from beside an empty table.
                private$.biv_model <- NULL
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
                private$.biv_model <- NULL
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
            het_info_note <- .("I\u00b2 is not reported here: a univariate I\u00b2 ignores the between-study correlation between sensitivity and specificity (the threshold effect) and does not describe the bivariate model (Cochrane DTA Handbook, chapter 10). The Heterogeneity Assessment table reports Q, tau-squared and a univariate I\u00b2 computed SEPARATELY for sensitivity and for specificity - read those as descriptive summaries of each margin, not as the heterogeneity of the bivariate model.")

            # The p-values on the sensitivity/specificity rows are the Wald tests
            # of the logit intercepts, i.e. H0: sensitivity = 50% and
            # H0: false-positive rate = 50%. That is almost never the hypothesis a
            # reader assumes from an unlabelled "p" column, and it is trivially
            # significant for any usable test, so name it.
            # D29: the table reported five estimates with intervals and never
            # said what confidence level they were at, how many studies they
            # pooled, or how many participants those studies contributed - all
            # of which a reader needs before quoting the numbers.
            # Raw counts, not mada_data: a zero-cell correction adds
            # pseudo-observations (one person per corrected study under
            # 'constant', fractions of one under +1/N), and they are not people.
            n_dis <- sum(raw_data$tp + raw_data$fn, na.rm = TRUE)
            n_non <- sum(raw_data$fp + raw_data$tn, na.rm = TRUE)
            scope_note <- sprintf(
                .("Pooled from %d studies comprising %s participants (%s with the target condition, %s without). Intervals are %d%% confidence intervals; sensitivity and specificity are percentages, the likelihood ratios and the diagnostic odds ratio are ratios."),
                nrow(mada_data),
                # base:: is required: the package imports jmvcore, whose format()
                # masks base::format() and silently ignores big.mark.
                base::format(n_dis + n_non, big.mark = ","),
                base::format(n_dis, big.mark = ","),
                base::format(n_non, big.mark = ","),
                self$options$confidence_level %||% 95)
            if (isTRUE(private$.repeated_ids))
                scope_note <- paste(scope_note,
                    .("Rows that share a study identifier are counted as separate studies, so participants they share are counted more than once."))
            bivariate_table$setNote("scope", scope_note)

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
                        if (isTRUE(self$options$sroc_plot)) {
                            # Which of the two is wider depends on k AND the
                            # level (they cross at k = 8 at 95%, 6 at 90%, 15 at
                            # 99%), so compare the multipliers actually used.
                            region_rad <- sqrt(stats::qchisq(conf_level, df = 2))
                            pred_note <- paste(pred_note, sprintf(if (crit > region_rad)
                                .("The dotted prediction region on the SROC plot is the joint two-dimensional version of this interval, drawn at the large-sample radius mada uses (%.3f), while this interval uses t on %d df (%.3f). With this many studies the printed interval is therefore the wider of the two. Read them as two views of the same quantity rather than matching one to the other.")
                            else
                                .("The dotted prediction region on the SROC plot is the joint two-dimensional version of this interval, drawn at the large-sample radius mada uses (%.3f), while this interval uses t on %d df (%.3f). With this many studies the region therefore extends further than the printed interval. Read them as two views of the same quantity rather than matching one to the other."),
                                region_rad, max(k_studies - 2, 1), crit))
                        }
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

            bivariate_table$setRow(rowKey = "sensitivity", values = private$.tableValues(list(
                estimate = pooled_sens * 100,  # Convert to percentage
                ci_lower = sens_ci[1] * 100,   # Convert to percentage
                ci_upper = sens_ci[2] * 100,   # Convert to percentage
                p_value = sens_logit_row[1, "Pr(>|z|)"]
            )))

            bivariate_table$setRow(rowKey = "specificity", values = private$.tableValues(list(
                estimate = pooled_spec * 100,  # Convert to percentage
                ci_lower = spec_ci[1] * 100,   # Convert to percentage
                ci_upper = spec_ci[2] * 100,   # Convert to percentage
                p_value = fpr_logit_row[1, "Pr(>|z|)"]
            )))

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

            # For the Analysis Summary, which grades the likelihood ratios.
            private$.pooled_lr_ci <- lr_ci

            bivariate_table$setRow(rowKey = "plr", values = private$.tableValues(list(
                estimate = pooled_plr,
                ci_lower = lr_ci$plr[1],
                ci_upper = lr_ci$plr[2],
                p_value = NULL
            )))

            bivariate_table$setRow(rowKey = "nlr", values = private$.tableValues(list(
                estimate = pooled_nlr,
                ci_lower = lr_ci$nlr[1],
                ci_upper = lr_ci$nlr[2],
                p_value = NULL
            )))

            bivariate_table$setRow(rowKey = "dor", values = private$.tableValues(list(
                estimate = pooled_dor,
                ci_lower = lr_ci$dor[1],
                ci_upper = lr_ci$dor[2],
                p_value = NULL
            )))

            bivariate_table$setNote("method", sprintf(.("Reitsma bivariate model, %s estimation."),
                private$.methodTitle(method_used)))
        },
        
        .performPHMSROCAnalysis = function(meta_data, mada_data) {

            if (!requireNamespace("mada", quietly = TRUE)) {
                self$results$hsrocresults$setNote("package_error",
                    .("The mada package is not available, so the proportional-hazards SROC model could not be fitted."))
                return()
            }

            hsroc_table <- self$results$hsrocresults
                # Blank the rows scaffolded in .init() rather than deleting them.
                for (blank_key in c("theta", "taus_sq", "auc"))
                    hsroc_table$setRow(rowKey = blank_key, values = private$.tableValues(list(
                        estimate = NULL, std_error = NULL,
                        z_value = NULL, p_value = NULL)))

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
                phm_converged <- TRUE
                # What a PHM notice can say about the bivariate model, which has
                # already run: recommending it is wrong when it is off, failed or
                # did not converge.
                biv_state <- if (!is.null(private$.pooled_sensitivity)) "pooled"
                    else if (isTRUE(self$options$bivariate_analysis)) "unusable" else "off"
                hsroc_model <- tryCatch({
                    fit <- phm_fit_once(100)
                    if (!is.null(fit$model) && !fit$converged) {
                        retry <- phm_fit_once(5000)
                        if (!is.null(retry$model)) fit <- retry
                        phm_converged <- isTRUE(fit$converged)
                        phm_iter_note <- if (phm_converged)
                            .("The proportional-hazards SROC model needed more than mada's default 100 profile-likelihood iterations; it was refitted with a limit of 5000 and converged, and the estimates below come from that converged fit.")
                        else
                            .("The proportional-hazards SROC model did NOT converge within 5000 profile-likelihood iterations, so no estimates are reported: the last iterate is not a maximum-likelihood solution, and it can change substantially with a different iteration limit.")
                        if (!phm_converged)
                            private$.addNotice("STRONG_WARNING", .("Proportional-hazards SROC model did not converge"), paste(
                                .("The proportional-hazards SROC model did not converge, so its theta, tau-squared and area under the curve are not reported: they would be the last iterate of an unfinished optimisation, and on data like this the unconverged value and the converged one have differed by an order of magnitude."),
                                switch(biv_state,
                                    pooled = .("Use the bivariate model and its SROC curve instead, or reduce the heterogeneity by restricting the study set."),
                                    off = .("Tick 'Bivariate random-effects model' for the bivariate SROC curve instead, or reduce the heterogeneity by restricting the study set."),
                                    unusable = .("The bivariate model gives no usable curve here either; reduce the heterogeneity by restricting the study set."))))
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
                # The rows were blanked above; an unconverged fit leaves them
                # blank and raises no AUC verdict from the last iterate.
                if (!is.null(hsroc_model) && !phm_converged) return()

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
                        phm_set(rowKey = param_name, values = private$.tableValues(list(
                            parameter = display_name,
                            estimate = estimate,
                            std_error = if (is.finite(std_error)) std_error else NULL,
                            z_value = if (is.finite(z_value)) z_value else NULL,
                            p_value = if (is.finite(p_value)) p_value else NULL
                        )))
                    }

                    # The AUC of the PHM SROC curve is 1/(1 + theta) (Holling,
                    # Boehning & Boehning 2012). The note below quoted the formula
                    # but the value itself was never shown. SE by the delta
                    # method: d/dtheta [1/(1 + theta)] = -1/(1 + theta)^2.
                    theta_val <- if ("theta" %in% names(coefficients)) unname(coefficients["theta"]) else NA_real_
                    if (is.finite(theta_val) && theta_val > 0) {
                        auc_val <- 1 / (1 + theta_val)
                        hsroc_table$setRow(rowKey = "auc", values = private$.tableValues(list(
                            parameter = .("Area under the SROC curve (AUC = 1/(1 + theta))"),
                            estimate = auc_val,
                            std_error = if (is.finite(theta_se)) theta_se / (1 + theta_val)^2 else NULL,
                            z_value = NULL,
                            p_value = NULL
                        )))
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
                            private$.addNotice("STRONG_WARNING", .("Proportional-hazards SROC model does not fit"), paste(
                                sprintf(.("The proportional-hazards SROC curve falls below the diagonal (area %.2f, below 0.5). This means the Lehmann family used by this model does not describe these studies; it is not evidence that the test performs worse than chance."), auc_val),
                                switch(biv_state,
                                    pooled = .("The pooled sensitivity and specificity above are unaffected. Use the bivariate model and its SROC curve instead. If the pooled pair above is also below chance, check the TP/FP/FN/TN column assignment."),
                                    off = .("Tick 'Bivariate random-effects model' for the bivariate SROC curve instead; if its pooled pair is also below chance, check the TP/FP/FN/TN column assignment."),
                                    unusable = .("The bivariate model gives no usable curve here either; if most study points also fall below the diagonal, check the TP/FP/FN/TN column assignment."))))
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
                    het_table$setRow(rowKey = blank_key, values = private$.tableValues(list(
                        q_statistic = NA_real_, df = NA_integer_, p_value = NA_real_,
                        i_squared = NA_real_, tau_squared = NA_real_)))

                sens_valid <- is.finite(analysis_data$logit_sens) &
                    is.finite(analysis_data$var_logit_sens) &
                    analysis_data$var_logit_sens > 0
                spec_valid <- is.finite(analysis_data$logit_spec) &
                    is.finite(analysis_data$var_logit_spec) &
                    analysis_data$var_logit_spec > 0

                n_kept_zero <- sum(analysis_data$zero_corrected %||% FALSE)
                if (n_kept_zero > 0) {
                    het_table$setNote("zero_cell", sprintf(
                        .("%d study/studies with a zero cell were kept by adding 0.5 to all four of their cells, as in the bivariate model."),
                        n_kept_zero))
                }
                # Defensive: zero cells are corrected before this point and empty
                # arms were excluded, so no logit should be infinite here.
                dropped_sens <- sum(!sens_valid)
                dropped_spec <- sum(!spec_valid)
                if (dropped_sens > 0 || dropped_spec > 0) {
                    het_table$setNote(
                        "nonfinite_rows",
                        sprintf(
                            .("Excluded from the univariate heterogeneity models (sensitivity: %d; specificity: %d): studies whose logit or its variance is not finite."),
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
                    het_table$setRow(rowKey = row_key, values = private$.tableValues(list(
                        measure = measure,
                        q_statistic = model$QE,
                        df = model$k - 1,
                        p_value = model$QEp,
                        i_squared = i_squared,
                        tau_squared = model$tau2
                    )))

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
        },
        
        .performMetaRegression = function(meta_data, analysis_data) {

            # Clear first, before any early return, so a run that bails out
            # cannot leave rows from a previous configuration on screen.
            .deleteTableRows(self$results$metaregression)

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
            # models each drop their own non-finite studies further down (until
            # 2026-10-19 that meant every zero-cell study), so a guard computed
            # here passed a set of 7 studies with 4 residual df while the
            # sensitivity model ran on 3 studies of a 3-level factor - no
            # residual df at all - and metafor's raw text ("Number of
            # parameters to be estimated is larger than the number of
            # observations") reached the user as a notice. The per-margin check
            # lives inside fit_meta_regression(), below.
            if (length(unique(stats::na.omit(as.character(analysis_data$covariate)))) < 2) {
                self$results$metaregression$setNote("constant_covariate",
                    .("Meta-regression not run: the covariate takes the same value in every study, so it cannot explain any between-study variation."))
                return()
            }

            # The power caution is a note on each fitted margin (small_sample_*,
            # below), with that margin's own study and parameter counts. A
            # set-level notice here quoted counts from before the per-margin
            # exclusions, so it could disagree with the model it described.

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

                n_kept_zero <- sum(analysis_data$zero_corrected %||% FALSE)
                if (n_kept_zero > 0) {
                    metareg_table$setNote("zero_cell", sprintf(
                        .("%d study/studies with a zero cell were kept by adding 0.5 to all four of their cells, as in the bivariate model."),
                        n_kept_zero))
                }
                # Defensive: zero cells are corrected before this point and empty
                # arms were excluded, so no logit should be infinite here.
                dropped_sens <- sum(!sens_valid)
                dropped_spec <- sum(!spec_valid)
                if (dropped_sens > 0 || dropped_spec > 0) {
                    metareg_table$setNote(
                        "nonfinite_rows",
                        sprintf(
                            .("Excluded from the meta-regression (sensitivity: %d; specificity: %d): studies whose logit or its variance is not finite."),
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

                    if (is.factor(data$covariate) &&
                        any(!levels(data$covariate) %in% as.character(data$covariate))) {
                        if (!is.null(attr(data$covariate, "contrasts"))) {
                            metareg_table$setNote(paste0("contrasts_reset_", key), sprintf(
                                .("%s: unused factor levels were dropped. Explicit factor contrasts were regenerated using the session contrast settings for the retained levels; the fitted coding is reported below."),
                                measure))
                        }
                        data$covariate <- droplevels(data$covariate)
                    }

                    # Per-margin degrees of freedom: a categorical covariate
                    # contributes (levels - 1) parameters, and both the level
                    # count and the study count are those of THIS margin after
                    # its own exclusions.
                    cov_levels <- unique(stats::na.omit(as.character(data$covariate)))
                    n_param <- if (is.factor(data$covariate) || is.character(data$covariate))
                        max(length(cov_levels) - 1, 1) else 1
                    resid_df <- nrow(data) - n_param - 1

                    if (length(cov_levels) < 2) {
                        # One whole sentence per margin: a translated noun
                        # spliced into "%s meta-regression ..." cannot take the
                        # case ending another language needs.
                        metareg_table$setNote(paste0("constant_", key), if (identical(key, "sens"))
                            .("Sensitivity meta-regression not run: after excluding studies this model cannot use, the covariate takes the same value in every remaining study.")
                        else
                            .("Specificity meta-regression not run: after excluding studies this model cannot use, the covariate takes the same value in every remaining study."))
                        return(NULL)
                    }
                    if (resid_df < 1) {
                        metareg_table$setNote(paste0("overparameterised_", key), sprintf(
                            if (identical(key, "sens"))
                                .("Sensitivity meta-regression not run: the %d studies this model can use cannot support a covariate contributing %d model parameter(s) - no residual degrees of freedom remain, so the model would fit perfectly and its confidence intervals would be meaningless. At least %d studies are needed, and 10 per covariate is the usual recommendation.")
                            else
                                .("Specificity meta-regression not run: the %d studies this model can use cannot support a covariate contributing %d model parameter(s) - no residual degrees of freedom remain, so the model would fit perfectly and its confidence intervals would be meaningless. At least %d studies are needed, and 10 per covariate is the usual recommendation."),
                            nrow(data), n_param, n_param + 2))
                        return(NULL)
                    }
                    if (nrow(data) < 10 * n_param) {
                        metareg_table$setNote(paste0("small_sample_", key), sprintf(
                            .("%s meta-regression on %d studies with a covariate contributing %d model parameter(s) (%d residual degrees of freedom). The usual recommendation is at least 10 studies per covariate, so estimates may be unstable and confidence intervals unreliable. Report this as exploratory only."),
                            measure, nrow(data), n_param, resid_df))
                    }

                    model <- tryCatch(
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
                    # Keep the actual design coding for THIS fitted margin. The
                    # two margins can retain different levels after exclusions.
                    if (!is.null(model)) {
                        model$oncopath_covariate <- data$covariate
                        # Knapp-Hartung scales the covariance by the weighted
                        # residual variance. Studies that fit the covariate exactly
                        # (one 2x2 table entered several times per level, say) make it zero,
                        # and every standard error, interval and p-value collapses
                        # with it: z = Inf and p = 0 read as overwhelming evidence.
                        se_ok <- all(is.finite(model$se)) && all(model$se > 0)
                        model$oncopath_degenerate <- !se_ok
                        if (!se_ok) {
                            metareg_table$setNote(paste0("degenerate_", key), sprintf(
                                .("%s: the studies fit the covariate exactly (no residual variation, as when the same 2x2 table is entered several times at each level), so the Knapp-Hartung standard errors are zero. Standard errors, intervals, test statistics and p-values are not reported; the coefficients are descriptive."),
                                measure))
                        }
                    }
                    model
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
                    if (is.null(model) || isTRUE(model$oncopath_degenerate) ||
                        is.null(model$QM) || !is.finite(model$QM))
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
                    # No inference from a fit whose standard errors collapsed.
                    infer <- !isTRUE(model$oncopath_degenerate)
                    metareg_table$addRow(
                        rowKey = paste0(key_prefix, "_intercept"),
                        values = private$.tableValues(list(
                            measure = measure,
                            parameter = .("Intercept"),
                            estimate = model$beta[1],
                            std_error = if (infer) model$se[1] else NULL,
                            # metafor already computes these at rma(level=);
                            # the table showed an estimate and an SE and left
                            # the reader to combine them with the right
                            # reference distribution (t under Knapp-Hartung).
                            ci_lower = if (infer) model$ci.lb[1] else NULL,
                            ci_upper = if (infer) model$ci.ub[1] else NULL,
                            z_value = if (infer) model$zval[1] else NULL,
                            p_value = if (infer) model$pval[1] else NULL
                        ))
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
                            paste0(covariate_var, " ", substring(beta_names[j], nchar("covariate") + 1L))
                        } else {
                            safe_covariate_label
                        }
                        # A table cell is parsed as HTML: a level "<Negative>" would blank.
                        coef_label <- .rocLabel(trimws(coef_label), note = FALSE)
                        metareg_table$addRow(
                            rowKey = paste0(key_prefix, "_covariate_", j),
                            values = private$.tableValues(list(
                                measure = measure,
                                parameter = coef_label,
                                estimate = model$beta[j],
                                std_error = if (infer) model$se[j] else NULL,
                                ci_lower = if (infer) model$ci.lb[j] else NULL,
                                ci_upper = if (infer) model$ci.ub[j] else NULL,
                                z_value = if (infer) model$zval[j] else NULL,
                                p_value = if (infer) model$pval[j] else NULL
                            ))
                        )
                    }
                }

                add_metareg_rows(sens_metareg, .("Sensitivity"), "sens")
                add_metareg_rows(spec_metareg, .("Specificity"), "spec")

                # The table reports coefficients on the LOGIT scale with no
                # label, so "Intercept 95.26" read like a percentage. For a
                # categorical covariate the reference level was never named, so
                # a row labelled "grade MRI  -0.037" had no stated comparator.
                # The covariate name and levels are the user's: clean them at
                # this note sink only (" [" would truncate the note as a
                # msgctxt; "<" before a letter would blank text).
                covariate_note <- .rocLabel(covariate_var, note = TRUE)
                describe_coding <- function(model, measure) {
                    if (is.null(model)) return(NULL)
                    cv <- model$oncopath_covariate
                    if (!is.factor(cv) && !is.character(cv)) {
                        return(sprintf(
                            .("%s: the coefficient is the change per one-unit increase in %s; the intercept is the logit at zero."),
                            measure, covariate_note))
                    }
                    lv <- if (is.factor(cv)) levels(cv) else sort(unique(cv))
                    # X is the fitted design, including any ordered or custom
                    # contrasts. A reference exists only for treatment coding.
                    coding <- model$X[match(lv, as.character(cv)), -1, drop = FALSE]
                    zero <- which(rowSums(abs(coding)) < 1e-10)
                    treatment <- length(zero) == 1L &&
                        ncol(coding) == length(lv) - 1L &&
                        all(abs(coding - round(coding)) < 1e-10) &&
                        all(coding >= 0 & coding <= 1) &&
                        all(rowSums(coding[-zero, , drop = FALSE]) == 1) &&
                        all(colSums(coding) == 1)
                    if (treatment) {
                        return(sprintf(
                            .("%s: each coefficient contrasts that level of %s against the reference level '%s'; the intercept is the logit at that reference level."),
                            measure, covariate_note, .rocLabel(lv[zero], note = TRUE)))
                    }
                    columns <- vapply(seq_len(ncol(coding)), function(j) {
                        paste0(.rocLabel(colnames(coding)[j], note = TRUE), ": ",
                            paste(paste0(.rocLabel(lv, note = TRUE), "=", format(signif(coding[, j], 6),
                                                       trim = TRUE)), collapse = ", "))
                    }, character(1))
                    sprintf(
                        .("%s: %s uses contrast coding, not level-versus-reference coefficients. The intercept is the fitted logit when all contrast columns are zero. Fitted design columns (level=coding): %s."),
                        measure, covariate_note, paste(columns, collapse = "; "))
                }
                metareg_table$setNote("scale", paste(
                    .("Estimates, standard errors and confidence intervals are on the LOGIT scale, not the percentage scale used by the pooled results above: a coefficient of 0 means no association."),
                    paste(c(describe_coding(sens_metareg, .("Sensitivity")),
                            describe_coding(spec_metareg, .("Specificity"))),
                          collapse = " ")))

                omnibus_txt <- c(describe_omnibus(sens_metareg, .("sensitivity")),
                                 describe_omnibus(spec_metareg, .("specificity")))
                if (length(omnibus_txt) > 0) {
                    metareg_table$setNote("omnibus", sprintf(
                        .("Omnibus test of the covariate effect - %s."),
                        paste(omnibus_txt, collapse = "; ")))
                }
            }
        },
        
        # Funnel-plot caption for Deeks' test. Each variant is one whole
        # translatable sentence (the qualifiers used to be appended fragments),
        # and a p below 0.001 reads "p < 0.001", not "p = 0.000". The table
        # calls its verdict descriptive past a quarter zero-cell studies; the
        # caption says the same.
        .deeksFitLabel = function(t, df, p, k, zero_share) {
            p_txt <- if (is.finite(p) && p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)
            if (zero_share > 0.25)
                sprintf(.("Deeks' asymmetry test: t = %.2f on %d df, %s. Descriptive only: too many zero cells for a valid asymmetry test."), t, as.integer(df), p_txt)
            else if (k < 10)
                sprintf(.("Deeks' asymmetry test: t = %.2f on %d df, %s. Descriptive only: k = %d < 10 gives low statistical power."), t, as.integer(df), p_txt, as.integer(k))
            else
                sprintf(.("Deeks' asymmetry test: t = %.2f on %d df, %s"), t, as.integer(df), p_txt)
        },

        # Deeks ESS uses study group sizes before the additional continuity
        # correction needed for finite log DORs. Both the table and renderer use
        # this preparation, including when the model-level correction is none.
        .prepareDeeksData = function(data) {
            n_diseased <- data$tp + data$fn
            n_healthy <- data$fp + data$tn
            data$ess <- 4 * n_diseased * n_healthy / (n_diseased + n_healthy)
            cells <- c("tp", "fp", "fn", "tn")
            if (any(data[, cells] == 0)) data[, cells] <- data[, cells] + 0.5
            data$log_dor <- log((data$tp * data$tn) / (data$fp * data$fn))
            data$inv_root_ess <- 1 / sqrt(data$ess)
            data
        },

        .performPublicationBiasAssessment = function(analysis_data) {

            # Blank first, before any early return. The Deeks row is scaffolded in
            # .init(), so deleteRows() would remove it; a run that cannot assess
            # bias leaves empty cells, not the previous run's result.
            self$results$publicationbias$setRow(rowKey = "deeks_test", values = private$.tableValues(list(
                statistic = NA_real_, p_value = NA_real_, interpretation = NA_character_)))

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

            if (nrow(analysis_data) < 10) {
                    private$.addNotice("WARNING", .("Publication bias caution"), sprintf(
                        .("Deeks' test is unreliable with fewer than 10 studies (k = %d here); treat the asymmetry result as descriptive only."),
                        nrow(analysis_data)))
                }

                n_zero <- sum(rowSums(analysis_data[, c("tp", "fp", "fn", "tn")] == 0) > 0)
                analysis_data <- private$.prepareDeeksData(analysis_data)

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
                    bias_table$setRow(rowKey = "deeks_test", values = private$.tableValues(list(
                        statistic = NA_real_, p_value = NA_real_,
                        interpretation = .("Not estimable: at least 3 studies with finite odds ratios are required")
                    )))
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
                        bias_table$setRow(rowKey = "deeks_test", values = private$.tableValues(list(
                            statistic = NA_real_, p_value = NA_real_,
                            interpretation = .("Not estimable: all studies have the same effective sample size"))))
                        bias_table$setNote("deeks_method",
                            .("Deeks' test regresses the log diagnostic odds ratio on 1/sqrt(effective sample size). Every study here has the same effective sample size, so there is no variation in study size to relate accuracy to and funnel-plot asymmetry cannot be assessed. This happens when the rows are different tests or readers applied to one common sample, and such rows are not independent studies."))
                        return()
                    }
                    deeks_t <- cf[2, 3]
                    deeks_p <- cf[2, 4]
                    deeks_df <- stats::df.residual(deeks_fit)

                    # Share of studies whose zero cell forced the correction.
                    zero_share <- n_zero / nrow(analysis_data)
                    too_many_zeros <- zero_share > 0.25

                    bias_table$setRow(rowKey = "deeks_test", values = private$.tableValues(list(
                        statistic = deeks_t,
                        p_value = deeks_p,
                        interpretation = if (too_many_zeros)
                            .("Not interpretable: too many zero cells")
                        else if (deeks_p < 0.05)
                            .("Asymmetry detected (p < 0.05)")
                        else
                            .("No asymmetry detected (p >= 0.05)")
                    )))

                    note <- sprintf(
                        .("Deeks' test: log diagnostic odds ratio regressed on 1/sqrt(effective sample size), weighted by effective sample size; slope referred to t on %d df (Deeks, Macaskill & Irwig 2005). The verdict uses alpha = 0.05; some implementations use 0.10 for asymmetry tests, so a p-value between the two is borderline either way. Asymmetry is not by itself evidence of publication bias - it can also arise from between-study heterogeneity, a threshold effect, or a genuine relation between study size and accuracy (Cochrane DTA Handbook, chapter 10)."), deeks_df)
                    if (n_zero > 0) {
                        note <- paste(note, sprintf(
                            .("A continuity correction of 0.5 was applied to every study so that the %d study/studies with a zero cell had a finite odds ratio; correcting only those studies would itself tilt the test."),
                            n_zero))
                    }
                    if (!identical(self$options$zero_cell_correction %||% "none", "none")) {
                        note <- paste(note,
                            .("The test uses the raw counts: the Zero-Cell Correction selected for the pooled models is not applied here, because a correction confined to the zero-cell studies would build a size-related trend into the test's own outcome."))
                    }
                    if (too_many_zeros) {
                        note <- paste(note, sprintf(
                            .("No verdict is given: %.0f%% of the studies have a zero cell. Above a quarter, the continuity correction the test needs can produce funnel asymmetry by itself - in this module's null simulations up to 42%% of meta-analyses at 80%% zero-cell studies, against a nominal 5%% - so the verdict is withheld as a precaution. Read the statistic as descriptive only."),
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
                # render-state: .pooled_sensitivity
                # render-state: .pooled_specificity
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

            # Store only serializable data (no model object!)
            # SERIALIZATION FIX: Ensure meta_data is a plain data frame
            if (!is.null(meta_data) && is.data.frame(meta_data)) {
                meta_data <- as.data.frame(meta_data, stringsAsFactors = FALSE)
                # Remove any attributes that might contain functions
                attributes(meta_data) <- attributes(meta_data)[c("names", "row.names", "class")]
            }

            # The summary point, curve and regions come from the bivariate
            # (Reitsma) model. Without one, draw the studies and say why. This
            # was a one-line text panel, cut off at both ends, that asked the
            # user to enable 'Bivariate Analysis' even when the box was ticked
            # and the fit had failed.
            if (is.null(private$.biv_model)) {
                image$setState(list(
                    data = meta_data,
                    curve_note = if (!isTRUE(self$options$bivariate_analysis))
                        .("No summary point or SROC curve: they come from the bivariate model, which is switched off. Tick 'Bivariate random-effects model' to add them; only the individual studies are shown.")
                    else if (isTRUE(private$.biv_unconverged))
                        .("No summary point or SROC curve: the bivariate model did not converge, so they would come from an unfinished fit. Only the individual studies are shown.")
                    else
                        .("No summary point or SROC curve: the bivariate model could not be fitted. Only the individual studies are shown.")
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
                    if (has_summary) .("Studies (circles), pooled estimate (triangle), chance line (dot-dash)")
                    else .("Studies (circles), chance line (dot-dash)"),
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
                        caption = if (!is.null(state$curve_note)) paste(strwrap(state$curve_note, width = 70), collapse = "\n") else NULL,
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

                n_zero_plot <- sum(rowSums(meta_data[, c("tp", "fp", "fn", "tn")] == 0) > 0)
                meta_data <- private$.prepareDeeksData(meta_data)

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
                        fit_label <- private$.deeksFitLabel(
                            pcf[2, 3], stats::df.residual(pfit), pcf[2, 4],
                            k = nrow(meta_data), zero_share = n_zero_plot / nrow(meta_data))
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
                        y = .("1 / sqrt(effective sample size)")
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
            
            html <- paste0(
                "\n            <h2>",
                .("Diagnostic Test Meta-Analysis for Pathology"),
                "</h2>\n            \n            <h3>",
                .("Purpose"),
                "</h3>\n            <p>",
                .("This module performs comprehensive meta-analysis of diagnostic test accuracy studies, specifically designed for pathology research including AI algorithm validation and biomarker diagnostic accuracy synthesis."),
                "</p>\n            \n            <h3>",
                .("Required Data Structure"),
                "</h3>\n            <p>",
                .("<strong>Essential Variables (Required):</strong>"),
                "</p>\n            <ul>\n                <li>",
                .("<strong>Study identifier:</strong> Unique name or ID for each study (e.g., 'Smith_2020', 'Study_1')"),
                "</li>\n                <li>",
                .("<strong>True positives (TP):</strong> Number correctly identified as positive"),
                "</li>\n                <li>",
                .("<strong>False positives (FP):</strong> Number incorrectly identified as positive"),
                "</li>\n                <li>",
                .("<strong>False negatives (FN):</strong> Number incorrectly identified as negative"),
                "</li>\n                <li>",
                .("<strong>True negatives (TN):</strong> Number correctly identified as negative"),
                "</li>\n            </ul>\n            \n            <p>",
                .("<strong>Optional Variables for Meta-Regression:</strong>"),
                "</p>\n            <ul>\n                <li>",
                .("<strong>Patient population:</strong> Disease stage, demographics (e.g., 'early_stage', 'advanced', 'mixed')"),
                "</li>\n                <li>",
                .("<strong>Technical method:</strong> Staining protocol (e.g., 'automated', 'manual')"),
                "</li>\n                <li>",
                .("<strong>Geographic region:</strong> Study location for population analysis"),
                "</li>\n                <li>",
                .("<strong>Publication year:</strong> For temporal trend investigation"),
                "</li>\n            </ul>\n            \n            <h3>",
                .("Data Preparation Checklist"),
                "</h3>\n            <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>\n                <p>",
                .("<strong>Before running analysis, verify:</strong>"),
                "</p>\n                <ul>\n                    <li>",
                .("No missing values in TP, FP, FN, TN columns"),
                "</li>\n                    <li>",
                .("All values are non-negative integers"),
                "</li>\n                    <li>",
                .("At least 3 studies with complete data"),
                "</li>\n                    <li>",
                .("One row per independent study: each row is its own 2x2 table from its own participants, with a unique identifier (several readers, cut-offs or algorithms on one cohort are not separate studies)"),
                "</li>\n                    <li>",
                .("Sample sizes are realistic (TP+FP+FN+TN = total cases per study)"),
                "</li>\n                </ul>\n            </div>\n            \n            <h3>",
                .("Example Data Format"),
                "</h3>\n            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>\n                <tr style='background-color: rgba(33, 33, 33, 0.06); color: inherit;'>\n                    <th style='border: 1px solid #ddd; padding: 8px;'>",
                .("study_name"),
                "</th>\n                    <th style='border: 1px solid #ddd; padding: 8px;'>",
                .("true_positives"),
                "</th>\n                    <th style='border: 1px solid #ddd; padding: 8px;'>",
                .("false_positives"),
                "</th>\n                    <th style='border: 1px solid #ddd; padding: 8px;'>",
                .("false_negatives"),
                "</th>\n                    <th style='border: 1px solid #ddd; padding: 8px;'>",
                .("true_negatives"),
                "</th>\n                    <th style='border: 1px solid #ddd; padding: 8px;'>population</th>\n                </tr>\n                <tr>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>Smith_2020</td>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>47</td>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>101</td>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>9</td>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>738</td>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>mixed</td>\n                </tr>\n                <tr>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>Johnson_2021</td>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>126</td>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>272</td>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>51</td>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>1543</td>\n                    <td style='border: 1px solid #ddd; padding: 8px;'>early_stage</td>\n                </tr>\n            </table>\n            \n            <h3>",
                .("Analysis Methods"),
                "</h3>\n            <ul>\n                <li>",
                .("<strong>Bivariate Random-Effects Model (Recommended):</strong> Jointly analyzes sensitivity and specificity accounting for correlation"),
                "</li>\n                <li>",
                .("<strong>Proportional-Hazards SROC Analysis:</strong> Holling model estimated by adjusted profile maximum likelihood"),
                "</li>\n                <li>",
                .("<strong>Meta-Regression:</strong> Investigates sources of heterogeneity using study-level covariates"),
                "</li>\n                <li>",
                .("<strong>Publication Bias Assessment:</strong> Deeks' funnel plot asymmetry test"),
                "</li>\n            </ul>\n\n            <h3>",
                .("Statistical Method Selection Guide"),
                "</h3>\n            <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #28a745; margin: 10px 0; color: inherit;'>\n                <p>",
                .("<strong>Choose the appropriate estimation method for your meta-analysis:</strong>"),
                "</p>\n                <ul>\n                    <li>",
                .("<strong>REML (Recommended):</strong> Default choice for most diagnostic meta-analyses. Most robust for random effects modeling with good performance across different scenarios."),
                "</li>\n                    <li>",
                .("<strong>Maximum Likelihood:</strong> Alternative estimation approach when maximum likelihood estimation is specifically preferred or required by study protocol."),
                "</li>\n                    <li>",
                .("<strong>Fixed Effects:</strong> Use when between-study heterogeneity is minimal or you want to assume all studies estimate the same underlying effect size."),
                "</li>\n                    <li>",
                .("<strong>Method of Moments:</strong> Classical moment-based estimation method, useful for comparison with older meta-analyses or when computational resources are limited."),
                "</li>\n                    <li>",
                .("<strong>Variance Components:</strong> Specialized approach for variance component estimation, typically used in advanced methodological research."),
                "</li>\n                </ul>\n                <p>",
                .("<strong> Recommendation:</strong> Start with REML unless you have specific methodological requirements. It provides the best balance of statistical properties and computational stability for diagnostic test meta-analysis."),
                "</p>\n            </div>\n            \n            <h3>",
                .("Clinical Applications"),
                "</h3>\n            <ul>\n                <li>",
                .("IHC marker validation across multiple pathology studies"),
                "</li>\n                <li>",
                .("AI algorithm performance meta-analysis for clinical implementation"),
                "</li>\n                <li>",
                .("Biomarker diagnostic accuracy synthesis for guideline development"),
                "</li>\n                <li>",
                .("Cross-population comparison of diagnostic test performance"),
                "</li>\n                <li>",
                .("Assessment of test performance heterogeneity and variation sources"),
                "</li>\n            </ul>\n            "
            )
            
            self$results$instructions$setContent(html)
        },
        
        .populateIndividualStudies = function(meta_data) {

            table <- self$results$individualstudies

            .deleteTableRows(table)

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
                table$addRow(rowKey = i, values = private$.tableValues(list(
                    study = .rocLabel(meta_data$study[i], note = FALSE),
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
                )))
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
            
            html <- paste0(
                "\n            <h2>",
                .("Clinical Interpretation Guidelines"),
                "</h2>\n            \n            <h3>",
                .("Primary Results Interpretation"),
                "</h3>\n            \n            <h4>",
                .("Pooled Sensitivity and Specificity"),
                "</h4>\n            <ul>\n                <li>",
                .("<strong>Pooled Sensitivity:</strong> Proportion of diseased cases correctly identified <ul> <li>Excellent sensitivity (\u226590%): few diseased cases missed"),
                "</li>\n                        <li>",
                .("Good sensitivity (80-89%): about 11-20 of every 100 diseased cases missed"),
                "</li>\n                        <li>",
                .("Moderate sensitivity (70-79%): appreciable number of cases missed"),
                "</li>\n                        <li>",
                .("Limited sensitivity (&lt;70%): many cases missed"),
                "</li>\n                    </ul>\n                </li>\n                <li>",
                .("<strong>Pooled Specificity:</strong> Proportion of non-diseased cases correctly identified <ul> <li>Excellent specificity (\u226590%): few false alarms"),
                "</li>\n                        <li>",
                .("Good specificity (80-89%): some false positives"),
                "</li>\n                        <li>",
                .("Moderate specificity (70-79%): appreciable false-positive rate"),
                "</li>\n                        <li>",
                .("Limited specificity (&lt;70%): many false alarms"),
                "</li>\n                    </ul>\n                </li>\n            </ul>\n            \n            <h4>",
                .("Likelihood Ratios for Clinical Decision-Making"),
                "</h4>\n            <p>",
                .("Refer to the <em>Likelihood Ratios for Clinical Decision-Making</em> table below for standard interpretation ranges for positive and negative likelihood ratios."),
                "</p>\n            \n            <h4>",
                .("Diagnostic Odds Ratio (DOR)"),
                "</h4>\n            <ul>\n                <li>",
                .("<strong>DOR > 25:</strong> Excellent overall discriminative ability"),
                "</li>\n                <li>",
                .("<strong>DOR 10-25:</strong> Good discriminative ability"),
                "</li>\n                <li>",
                .("<strong>DOR 5-10:</strong> Moderate discriminative ability"),
                "</li>\n                <li>",
                .("<strong>DOR &lt; 5:</strong> Limited discriminative ability"),
                "</li>\n            </ul>\n            \n            <h3>",
                .("Heterogeneity Assessment"),
                "</h3>\n            \n            <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0; color: inherit;'>\n                <h4>",
                .("I[[SUP2]] Statistic Interpretation:"),
                "</h4>\n                <p>",
                .("<em>The bands below are Higgins' conventional cut-points from intervention meta-analysis. They are not established thresholds for diagnostic accuracy, where a high I[[SUP2]] commonly reflects studies using different positivity thresholds - the situation the SROC curve exists to model - rather than a reason to abandon the analysis. Treat them as rough orientation, and prefer the prediction region.</em>"),
                "</p>\n                <ul>\n                    <li>",
                .("<strong>I[[SUP2]] < 25%:</strong> Little heterogeneity detected on this margin. I[[SUP2]] is imprecisely estimated when few studies are pooled and does not describe the bivariate model, so on its own it does not establish that pooling is appropriate - inspect the prediction region and check that positivity thresholds, patient spectrum and reference standards are comparable"),
                "</li>\n                    <li>",
                .("<strong>I[[SUP2]] 25-50%:</strong> Moderate heterogeneity - investigate potential sources"),
                "</li>\n                    <li>",
                .("<strong>I[[SUP2]] 50-75%:</strong> Substantial heterogeneity - pooling questionable"),
                "</li>\n                    <li>",
                .("<strong>I[[SUP2]] &gt; 75%:</strong> Considerable heterogeneity - a single pooled point is unlikely to describe the evidence; investigate thresholds, spectrum and reference standards, and report the SROC curve and prediction region"),
                "</li>\n                </ul>\n            </div>\n            \n            <h4>",
                .("Common Sources of Heterogeneity:"),
                "</h4>\n            <ul>\n                <li>",
                .("<strong>Patient Population:</strong> Disease stage, severity, demographics"),
                "</li>\n                <li>",
                .("<strong>Technical Factors:</strong> Staining protocols, antibody sources, automation"),
                "</li>\n                <li>",
                .("<strong>Methodological:</strong> Reference standards, blinding, cut-off thresholds"),
                "</li>\n                <li>",
                .("<strong>Geographic/Temporal:</strong> Population differences, technology evolution"),
                "</li>\n            </ul>\n            \n            <h3>",
                .("Publication Bias Assessment"),
                "</h3>\n            \n            <h4>",
                .("Deeks' Funnel Plot Test:"),
                "</h4>\n            <ul>\n                <li>",
                .("<strong>p \u2265 0.05:</strong> No statistically significant funnel-plot asymmetry was detected. This does NOT indicate that publication bias is unlikely: Deeks' test has low power and is unreliable with fewer than 10 studies, so a non-significant result is uninformative when few studies are pooled"),
                "</li>\n                <li>",
                .("<strong>p < 0.05:</strong> The funnel plot is asymmetric - small studies report systematically different accuracy from large ones. Publication bias is one explanation; heterogeneity, a threshold effect, and a genuine relation between study size and accuracy (smaller studies often use narrower spectra) are others. The test cannot tell them apart, so a significant result is a prompt to investigate, not a finding of publication bias"),
                "</li>\n                <li>",
                .("Where more than a quarter of the studies have a zero cell, no verdict is reported, as a precaution: the continuity correction the test needs can produce asymmetry by itself (up to 42% of null meta-analyses at 80% zero-cell studies in this module's simulations). The test always uses the raw counts, so the Zero-Cell Correction option does not change this"),
                "</li>\n            </ul>\n            \n            <div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; border-left: 4px solid #dc3545; margin: 10px 0; color: inherit;'>\n                <p>",
                .("<strong> When the Funnel Plot is Asymmetric:</strong>"),
                "</p>\n                <ul>\n                    <li>",
                .("Ask first whether the small studies differ clinically from the large ones - spectrum, setting, threshold, reference standard"),
                "</li>\n                    <li>",
                .("Search for unpublished studies or negative results"),
                "</li>\n                    <li>",
                .("Consider contacting study authors for additional data"),
                "</li>\n                    <li>",
                .("If no other explanation fits, pooled estimates may be overoptimistic"),
                "</li>\n                    <li>",
                .("Report the asymmetry as a limitation and interpret results cautiously"),
                "</li>\n                </ul>\n            </div>\n            \n            <h3>",
                .("Clinical Application Guidance"),
                "</h3>\n            \n            <h4>",
                .("IHC Marker Validation:"),
                "</h4>\n            <ul>\n                <li>",
                .("<strong>Screening Applications:</strong> Prioritize sensitivity; the level needed depends on prevalence and on the cost of a missed case, not on a fixed band"),
                "</li>\n                <li>",
                .("<strong>Confirmatory Testing:</strong> Prioritize specificity; the level needed depends on prevalence and on the cost of a false positive, not on a fixed band"),
                "</li>\n                <li>",
                .("<strong>Balanced Performance:</strong> Consider clinical costs of false positives vs false negatives"),
                "</li>\n            </ul>\n            \n            <h4>",
                .("AI Algorithm Implementation:"),
                "</h4>\n            <ul>\n                <li>",
                .("<strong>Consistent Performance:</strong> low measured heterogeneity means the included studies produced similar estimates; with few studies it is imprecisely estimated and does not by itself show that performance transfers to a new setting"),
                "</li>\n                <li>",
                .("<strong>Variable Performance:</strong> high heterogeneity indicates that accuracy differed across the included populations and protocols"),
                "</li>\n                <li>",
                .("<strong>Not external validation:</strong> a meta-analysis summarises existing published evidence and inherits any publication, spectrum or reference-standard bias of the included studies; it is not a prospective evaluation in an intended-use population with a pre-specified threshold"),
                "</li>\n            </ul>\n            \n            <h4>",
                .("Predictive Values in Clinical Practice:"),
                "</h4>\n            <p>",
                .("<strong>Important:</strong> Sensitivity and specificity are test characteristics, but clinicians need predictive values that depend on disease prevalence in their population. See the <em>Predictive Values by Prevalence</em> table below for estimated PPV and NPV across typical prevalence settings based on the pooled accuracy of this meta-analysis."),
                "</p>\n            \n            <h3>",
                .("Reporting Recommendations"),
                "</h3>\n            \n            <p>",
                .("When reporting your meta-analysis results, include:"),
                "</p>\n            <ul>\n                <li>",
                .("<strong>Study Selection:</strong> Number of studies included and excluded"),
                "</li>\n                <li>",
                .("<strong>Pooled Estimates:</strong> Sensitivity and specificity with confidence intervals at the level chosen in Advanced Options"),
                "</li>\n                <li>",
                .("<strong>Likelihood Ratios:</strong> For clinical decision-making context"),
                "</li>\n                <li>",
                .("<strong>Heterogeneity:</strong> I[[SUP2]] values and potential sources investigated"),
                "</li>\n                <li>",
                .("<strong>Publication Bias:</strong> Deeks' test results and visual assessment"),
                "</li>\n                <li>",
                .("<strong>Clinical Implications:</strong> Population-specific predictive values"),
                "</li>\n                <li>",
                .("<strong>Limitations:</strong> Study quality, missing data, generalizability"),
                "</li>\n            </ul>\n            \n            <div style='background-color: rgba(33, 163, 188, 0.21); padding: 15px; border-left: 4px solid #17a2b8; margin: 10px 0; color: inherit;'>\n                <p>",
                .("<strong> Pro Tip:</strong> Always interpret meta-analysis results in the context of your specific clinical population and intended use. A test excellent for one application may be inappropriate for another."),
                "</p>\n            </div>\n            "
            )
            
            self$results$interpretation$setContent(private$.renderSymbols(html))
        },

        # library-audit 2026-09-22 OncoPath [MEDIUM] DONE: reference tables belong in Table results, not hand-built HTML (guide section 26)
        .populateLikelihoodRatioGuide = function() {
            table <- self$results$likelihoodRatioGuide
            lr_guide <- list(
                list(type = .("Positive LR"), range = ">10", interp = .("Strong evidence FOR disease when test positive")),
                list(type = .("Positive LR"), range = "5-10", interp = .("Moderate evidence for disease")),
                list(type = .("Positive LR"), range = "2-5", interp = .("Weak evidence for disease")),
                list(type = .("Negative LR"), range = "<0.1", interp = .("Strong evidence AGAINST disease when test negative")),
                list(type = .("Negative LR"), range = "0.1-0.2", interp = .("Moderate evidence against disease")),
                list(type = .("Negative LR"), range = "0.2-0.5", interp = .("Weak evidence against disease"))
            )
            for (i in seq_along(lr_guide)) {
                table$setRow(rowNo = i, values = private$.tableValues(list(
                    lr_type = lr_guide[[i]]$type,
                    range = lr_guide[[i]]$range,
                    interpretation = lr_guide[[i]]$interp
                )))
            }
        },

        # library-audit 2026-09-22 OncoPath [MEDIUM] DONE: computed clinical tables belong in Table results, derived from pooled estimates (guide section 26)
        .populatePredictiveValues = function() {
            table <- self$results$predictiveValues

            sens <- private$.pooled_sensitivity
            spec <- private$.pooled_specificity

            has_estimates <- !is.null(sens) && !is.null(spec) &&
                             is.finite(sens) && is.finite(spec) &&
                             sens >= 0 && sens <= 1 && spec >= 0 && spec <= 1

            prevalences <- c(0.05, 0.10, 0.20, 0.50)
            for (i in seq_along(prevalences)) {
                prev <- prevalences[i]
                if (has_estimates) {
                    denom_ppv <- (sens * prev) + ((1 - spec) * (1 - prev))
                    denom_npv <- ((1 - sens) * prev) + (spec * (1 - prev))
                    ppv <- if (denom_ppv > 0) (sens * prev) / denom_ppv else NA_real_
                    npv <- if (denom_npv > 0) (spec * (1 - prev)) / denom_npv else NA_real_
                } else {
                    ppv <- NA_real_
                    npv <- NA_real_
                }
                table$setRow(rowKey = paste0("p_", as.integer(prev * 100)), values = private$.tableValues(list(
                    prevalence = prev,
                    ppv = ppv,
                    npv = npv
                )))
            }

            if (has_estimates) {
                table$setNote("explanation", sprintf(
                    .("Sensitivity and specificity are test characteristics; predictive values depend on disease prevalence. Values calculated from pooled sensitivity (%s%%) and specificity (%s%%)."),
                    sprintf("%.1f", sens * 100),
                    sprintf("%.1f", spec * 100)
                ))
            } else {
                table$setNote("explanation",
                    .("Pooled sensitivity and specificity estimates are unavailable; predictive values cannot be computed.")
                )
            }
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

            if (isTRUE(private$.repeated_ids))
                copy_text <- sprintf(
                    .("%s Rows that share a study identifier were pooled as separate studies, so some participants are counted more than once."),
                    copy_text)

            lr_ci <- private$.pooled_lr_ci
            lr_fragment <- function(ci) {
                if (length(ci) == 2 && all(is.finite(ci)))
                    sprintf(" (%.2f-%.2f)", ci[1], ci[2]) else ""
            }
            if (is.finite(lr_pos) && is.finite(lr_neg)) {
                copy_text <- sprintf(
                    .("%s Positive LR %.2f%s and negative LR %.2f%s."),
                    copy_text, lr_pos, lr_fragment(lr_ci$plr), lr_neg, lr_fragment(lr_ci$nlr)
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

            # Per 100, a value above 99% rounds to "100 out of 100" (and its
            # complement to "0"), which the table's 99.5% contradicts; count per
            # 1,000 there instead (2026-10-19 review, DM-4).
            per_1000 <- function(pct) pct > 99 || pct < 1

            # Every sentence goes through .() whole; the HTML scaffold carries
            # only %s slots. copy_text sits inside a single-quoted attribute, so
            # a translation containing ' or " must be escaped for that context.
            summary_html <- sprintf("
            <div class='analysis-summary' style='background-color: rgba(33, 149, 188, 0.1); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4>%s</h4>
                <p>%s</p>

                <div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>%s</h5>
                    <p>%s</p>
                    <p>%s</p>
                </div>

                %s

                <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>%s</h5>
                    %s
                    %s
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>%s</h5>
                    <p>%s</p>
                </div>

                <div style='margin-top: 15px;'>
                    <button onclick='navigator.clipboard.writeText(this.getAttribute(\"data-text\"))'
                            data-text='%s'
                            style='background-color: #007bff; color: #ffffff; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer;'>
                        %s
                    </button>
                </div>
            </div>
            ",
            .("Meta-Analysis Summary"),
            sprintf(.("<strong>Analysis Type:</strong> Diagnostic test accuracy meta-analysis of %d studies"), private$.n_studies),
            .("Pooled Test Performance"),
            if (per_1000(sens_pct))
                sprintf(.("<strong>Sensitivity:</strong> %.1f%% - The test correctly identifies %.0f out of 1,000 patients with disease"), sens_pct, 10 * sens_pct)
            else
                sprintf(.("<strong>Sensitivity:</strong> %.1f%% - The test correctly identifies %.0f out of 100 patients with disease"), sens_pct, sens_pct),
            if (per_1000(spec_pct))
                sprintf(.("<strong>Specificity:</strong> %.1f%% - The test correctly identifies %.0f out of 1,000 healthy individuals"), spec_pct, 10 * spec_pct)
            else
                sprintf(.("<strong>Specificity:</strong> %.1f%% - The test correctly identifies %.0f out of 100 healthy individuals"), spec_pct, spec_pct),
            correction_disclosure,
            .("Clinical Decision Metrics"),
            plr_text,
            nlr_text,
            .("Interpretation Guide"),
            private$.getInterpretationText(sens_pct, spec_pct, lr_pos, lr_neg,
                                          private$.pooled_sens_ci, private$.pooled_spec_ci,
                                          lr_ci),
            htmltools::htmlEscape(copy_text, attribute = TRUE),
            .("Copy Summary to Clipboard")
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
                reason <- .("The bivariate model is switched off. Tick 'Bivariate random-effects model' for pooled estimates.")
            } else if (isTRUE(private$.biv_unconverged)) {
                reason <- .("The bivariate model did not converge, so no pooled estimates are reported.")
            } else if (n_studies < 3) {
                reason <- sprintf(.("Only %d studies available. At least 3 studies are required for meta-analysis."), n_studies)
            } else {
                reason <- .("Bivariate meta-analysis encountered an error. Check individual study results and data quality.")
            }

            # The status says WHAT is shown, the recommendation WHY and what to
            # do; both used to print `reason`. The pointer to the study table is
            # given only when that table is on.
            table_pointer <- if (isTRUE(self$options$show_individual_studies))
                sprintf("<p>%s</p>", .("Individual study results are available in the table below for detailed examination.")) else ""
            summary_html <- sprintf("
            <div class='analysis-summary' style='background-color: rgba(33, 149, 188, 0.1); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4>%s</h4>
                <p>%s</p>

                <div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>%s</h5>
                    <p>%s</p>
                    <p>%s</p>
                    <p>%s</p>
                </div>

                <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>%s</h5>
                    <p>%s</p>
                    <p>%s</p>
                    <p>%s</p>
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>%s</h5>
                    <p>%s</p>
                    %s
                </div>
            </div>
            ",
            .("Meta-Analysis Summary"),
            .("<strong>Analysis Status:</strong> Pooled estimates are not available; the figures below are descriptive."),
            .("Study Overview"),
            sprintf(.("<strong>Number of Studies:</strong> %d"), n_studies),
            # %s + format(): sample sizes are numeric, and "%d" aborted the whole
            # analysis ("invalid format '%d'") whenever a count was fractional.
            sprintf(.("<strong>Total Sample Size:</strong> %s participants"), format(round(total_sample))),
            sprintf(.("<strong>Sample Size Range:</strong> %s - %s per study"),
                    format(round(min(meta_data$sample_size, na.rm = TRUE))),
                    format(round(max(meta_data$sample_size, na.rm = TRUE)))),
            .("Individual Study Performance (Descriptive)"),
            sprintf(.("<strong>Sensitivity:</strong> Mean %.1f%% (Range: %.1f%% - %.1f%%)"), sens_mean, sens_range[1], sens_range[2]),
            sprintf(.("<strong>Specificity:</strong> Mean %.1f%% (Range: %.1f%% - %.1f%%)"), spec_mean, spec_range[1], spec_range[2]),
            .("<em>Note:</em> These are simple averages, not meta-analytic pooled estimates."),
            .("Recommendation"),
            reason,
            table_pointer
            )

            self$results$summary$setContent(summary_html)
        },

        # Helper function for dynamic interpretation text with actual values
        .getInterpretationText = function(sens, spec, lr_pos, lr_neg,
                                         sens_ci = NULL, spec_ci = NULL,
                                         lr_ci = NULL) {
            # Classify against the standard bands, then check whether the
            # confidence interval actually supports the label. Every claim below
            # was previously made from the point estimate alone, so a pooled
            # sensitivity of 90.4% with a 95% CI of 71-97% was reported as
            # "excellent ... will detect 90 out of 100 patients", which the
            # interval does not support.
            band_key <- function(x) {
                if (x >= 90) {
                    "excellent"
                } else if (x >= 80) {
                    "good"
                } else if (x >= 70) {
                    "moderate"
                } else {
                    "limited"
                }
            }

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
            plr_band <- function(x) if (!is.finite(x)) "not_estimable" else if (x > 10) "strong"
                        else if (x >= 5) "moderate" else if (x >= 2) "weak" else "minimal"
            nlr_band <- function(x) if (!is.finite(x)) "not_estimable" else if (x < 0.1) "strong"
                        else if (x <= 0.2) "moderate" else if (x <= 0.5) "weak" else "minimal"
            # A likelihood ratio whose margin has (almost) no errors is set by
            # the continuity correction (2026-10-19 review, DM-2); it is shown
            # but not graded.
            zero_margin <- private$.zero_margin
            plr_key <- if (isTRUE(zero_margin$fp) && is.finite(lr_pos)) "correction" else plr_band(lr_pos)
            nlr_key <- if (isTRUE(zero_margin$fn) && is.finite(lr_neg)) "correction" else nlr_band(lr_neg)
            # The same check the sensitivity/specificity bands get: a grading
            # the confidence interval does not support is said to be so.
            lr_spans <- function(ci, band) length(ci) == 2 && all(is.finite(ci)) &&
                band(min(ci)) != band(max(ci))
            lr_note <- function(ci) sprintf(
                .("<strong>Note:</strong> its confidence interval (%.2f-%.2f) spans more than one evidence category, so this grading is not firmly established by the pooled data."),
                min(ci), max(ci))

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
                        if (sens > 99)
                            sprintf(.("With %.1f%% sensitivity, this test will detect %.0f out of 1,000 patients with disease, missing only %.0f."),
                                    sens, 10 * sens, 10 * (100 - sens))
                        else
                            sprintf(.("With %.1f%% sensitivity, this test will detect %.0f out of 100 patients with disease, missing only %.0f."),
                                    sens, sens, 100 - sens)
                    },
                    switch(nlr_key,
                        strong = sprintf(.("Rule-out power is governed by the negative likelihood ratio rather than by sensitivity alone; here LR- = %.2f, which lowers the post-test odds substantially (the SnNout pattern)."), lr_neg),
                        moderate = sprintf(.("Rule-out power is governed by the negative likelihood ratio rather than by sensitivity alone; here LR- = %.2f, which provides moderate evidence against disease after a negative result."), lr_neg),
                        weak = sprintf(.("Rule-out power is governed by the negative likelihood ratio rather than by sensitivity alone; here LR- = %.2f, which provides weak evidence against disease after a negative result."), lr_neg),
                        minimal = sprintf(.("Rule-out power is governed by the negative likelihood ratio rather than by sensitivity alone; here LR- = %.2f, which provides minimal evidence against disease after a negative result."), lr_neg),
                        not_estimable = .("Rule-out power is governed by the negative likelihood ratio rather than by sensitivity alone, and LR- is not estimable here."),
                        correction = sprintf(.("Rule-out power is governed by the negative likelihood ratio rather than by sensitivity alone; here LR- = %.2f, but with no false negatives in most studies this value and its interval are set by the continuity correction rather than by the data, so it is not graded."), lr_neg)
                    ),
                    if (nlr_key != "correction" && lr_spans(lr_ci$nlr, nlr_band)) lr_note(lr_ci$nlr)
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
                    if (spec > 99)
                        sprintf(.("With %.1f%% specificity, only %.0f out of 1,000 healthy individuals will test positive (false alarms)."),
                                spec, 10 * (100 - spec))
                    else
                        sprintf(.("With %.1f%% specificity, only %.0f out of 100 healthy individuals will test positive (false alarms)."),
                                spec, 100 - spec),
                    switch(plr_key,
                        strong = sprintf(.("Rule-in power is governed by the positive likelihood ratio rather than by specificity alone; here LR+ = %.2f, which raises the post-test odds substantially (the SpPin pattern)."), lr_pos),
                        moderate = sprintf(.("Rule-in power is governed by the positive likelihood ratio rather than by specificity alone; here LR+ = %.2f, which provides moderate evidence for disease after a positive result."), lr_pos),
                        weak = sprintf(.("Rule-in power is governed by the positive likelihood ratio rather than by specificity alone; here LR+ = %.2f, which provides weak evidence for disease after a positive result."), lr_pos),
                        minimal = sprintf(.("Rule-in power is governed by the positive likelihood ratio rather than by specificity alone; here LR+ = %.2f, which provides minimal evidence for disease after a positive result."), lr_pos),
                        not_estimable = .("Rule-in power is governed by the positive likelihood ratio rather than by specificity alone, and LR+ is not estimable here."),
                        correction = sprintf(.("Rule-in power is governed by the positive likelihood ratio rather than by specificity alone; here LR+ = %.2f, but with no false positives in most studies this value and its interval are set by the continuity correction rather than by the data, so it is not graded."), lr_pos)
                    ),
                    if (plr_key != "correction" && lr_spans(lr_ci$plr, plr_band)) lr_note(lr_ci$plr)
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
            if (!plr_key %in% c("not_estimable", "correction")) {
                parts <- c(parts,
                    sprintf(switch(plr_key,
                        strong   = .("<br><br><strong>Your positive LR of %.2f</strong> provides <em>strong</em> evidence FOR disease when test is positive."),
                        moderate = .("<br><br><strong>Your positive LR of %.2f</strong> provides <em>moderate</em> evidence FOR disease when test is positive."),
                        weak     = .("<br><br><strong>Your positive LR of %.2f</strong> provides <em>weak</em> evidence FOR disease when test is positive."),
                        minimal  = .("<br><br><strong>Your positive LR of %.2f</strong> provides <em>minimal</em> evidence FOR disease when test is positive.")),
                        lr_pos),
                    if (lr_spans(lr_ci$plr, plr_band)) lr_note(lr_ci$plr),
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

            if (!nlr_key %in% c("not_estimable", "correction")) {
                parts <- c(parts,
                    sprintf(switch(nlr_key,
                        strong   = .("<strong>Your negative LR of %.2f</strong> provides <em>strong</em> evidence AGAINST disease when test is negative."),
                        moderate = .("<strong>Your negative LR of %.2f</strong> provides <em>moderate</em> evidence AGAINST disease when test is negative."),
                        weak     = .("<strong>Your negative LR of %.2f</strong> provides <em>weak</em> evidence AGAINST disease when test is negative."),
                        minimal  = .("<strong>Your negative LR of %.2f</strong> provides <em>minimal</em> evidence AGAINST disease when test is negative.")),
                        lr_neg),
                    if (lr_spans(lr_ci$nlr, nlr_band)) lr_note(lr_ci$nlr),
                    if (lr_neg < 0.1) {
                        sprintf(.("A negative result substantially decreases disease probability (divides pre-test odds by %.1fx)."),
                                1 / lr_neg)
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

            html <- paste0(
                "\n            <div class='about-panel' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>\n                <h4>",
                .("About Diagnostic Test Meta-Analysis"),
                "</h4>\n\n                <div style='margin: 15px 0;'>\n                    <h5>",
                .("What This Analysis Does"),
                "</h5>\n                    <p>",
                .("Combines results from multiple diagnostic accuracy studies to estimate overall test performance through:"),
                "</p>\n                    <ul>\n                        <li>",
                .("<strong>Bivariate modeling</strong> - Jointly analyzes sensitivity and specificity"),
                "</li>\n                        <li>",
                .("<strong>Proportional-hazards SROC modeling</strong> - Models the trade-off between sensitivity and false-positive rate"),
                "</li>\n                        <li>",
                .("<strong>Heterogeneity assessment</strong> - Evaluates consistency across studies"),
                "</li>\n                        <li>",
                .("<strong>Publication bias</strong> - Checks for selective reporting"),
                "</li>\n                    </ul>\n                </div>\n\n                <div style='margin: 15px 0; background-color: rgba(33, 152, 239, 0.13); padding: 15px; border-radius: 5px; border-left: 4px solid #2196F3; color: inherit;'>\n                    <h5>",
                .("Understanding Bivariate and Proportional-Hazards SROC Models"),
                "</h5>\n                    <p>",
                .("<strong>These models answer related questions using different parameterizations:</strong>"),
                "</p>\n\n                    <p>",
                .("<strong>Bivariate Random-Effects Model (Recommended Primary Approach):</strong>"),
                "</p>\n                    <ul>\n                        <li>",
                .("Provides <em>intuitive</em> pooled sensitivity and specificity estimates"),
                "</li>\n                        <li>",
                .("Accounts for correlation between sensitivity and specificity"),
                "</li>\n                        <li>",
                .("Directly interpretable for clinical decision-making"),
                "</li>\n                        <li>",
                .("Preferred for meta-analyses with <em>homogeneous thresholds</em>"),
                "</li>\n                        <li>",
                .("<strong>Use this when:</strong> Studies use the same diagnostic threshold"),
                "</li>\n                    </ul>\n\n                    <p>",
                .("<strong>Holling Proportional-Hazards SROC Model:</strong>"),
                "</p>\n                    <ul>\n                        <li>",
                .("Relates sensitivity (<em>p</em>) and false-positive rate (<em>u</em>) through <em>u</em><sup>theta</sup> = <em>p</em>"),
                "</li>\n                        <li>",
                .("Reports theta as the diagnostic accuracy parameter"),
                "</li>\n                        <li>",
                .("Reports tau<sup>2</sup> as between-study variation in diagnostic accuracy"),
                "</li>\n                        <li>",
                .("Uses adjusted profile maximum likelihood and is suitable for smaller study sets"),
                "</li>\n                        <li>",
                .("Is distinct from the Rutter-Gatsonis HSROC model"),
                "</li>\n                    </ul>\n\n                    <p>",
                .("<strong> Clinical Insight:</strong> The plotted SROC curve is derived from the <em>bivariate model</em>. The proportional-hazards SROC table is a separate compact model of diagnostic accuracy and between-study variation; it should not be interpreted as a Rutter-Gatsonis HSROC threshold/accuracy table."),
                "</p>\n                </div>\n\n                <div style='margin: 15px 0;'>\n                    <h5>",
                .("When to Use This Analysis"),
                "</h5>\n                    <ul>\n                        <li>",
                .("Evaluating AI algorithms for pathology diagnosis"),
                "</li>\n                        <li>",
                .("Assessing biomarker diagnostic accuracy"),
                "</li>\n                        <li>",
                .("Comparing imaging modalities"),
                "</li>\n                        <li>",
                .("Synthesizing evidence for clinical guidelines"),
                "</li>\n                    </ul>\n                </div>\n\n                <div style='margin: 15px 0; background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; color: inherit;'>\n                    <h5>",
                .("Key Requirements & Assumptions"),
                "</h5>\n                    <ul>\n                        <li>",
                .("Minimum 3 studies with 2[[TIMES]]2 diagnostic data"),
                "</li>\n                        <li>",
                .("One 2[[TIMES]]2 table per independent study: rows that share participants (several readers, cut-offs or algorithms on one cohort) are not separate studies"),
                "</li>\n                        <li>",
                .("Studies should evaluate the same test and target condition"),
                "</li>\n                        <li>",
                .("Reference standard should be consistent across studies"),
                "</li>\n                        <li>",
                .("Patient spectrum should be clinically relevant"),
                "</li>\n                    </ul>\n                </div>\n\n                <div style='margin: 15px 0;'>\n                    <h5>",
                .("Quick Start Guide"),
                "</h5>\n                    <ol>\n                        <li>",
                .("<strong>Step 1:</strong> Select your study identifier variable"),
                "</li>\n                        <li>",
                .("<strong>Step 2:</strong> Assign TP, FP, FN, TN count variables"),
                "</li>\n                        <li>",
                .("<strong>Step 3:</strong> Choose analysis options (bivariate recommended)"),
                "</li>\n                        <li>",
                .("<strong>Step 4:</strong> Review pooled estimates and heterogeneity"),
                "</li>\n                        <li>",
                .("<strong>Step 5:</strong> Interpret in your clinical context"),
                "</li>\n                    </ol>\n                </div>\n\n                <div style='background-color: rgba(33, 163, 188, 0.21); padding: 15px; border-radius: 5px; margin: 15px 0; color: inherit;'>\n                    <p>",
                .("<strong> Tip:</strong> Start with the bivariate model and forest plot to understand overall performance, then explore heterogeneity sources with meta-regression if needed."),
                "</p>\n                </div>\n            </div>\n            "
            )

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
            html <- paste0(
                "\n            <div class='plot-explanation' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>\n                <h4>",
                .("Forest Plot Interpretation Guide"),
                "</h4>\n\n                <div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 5px; margin: 10px 0;'>\n                    <h5>",
                .("What This Plot Shows"),
                "</h5>\n                    <p>",
                .("<strong>Forest Plot:</strong> Displays individual study results for sensitivity and specificity with confidence intervals. Each study is represented by a point (estimate) with horizontal lines (confidence intervals)."),
                "</p>\n\n                    <ul>\n                        <li>",
                .("<strong>Left Panel (Sensitivity):</strong> Proportion of diseased cases correctly identified"),
                "</li>\n                        <li>",
                .("<strong>Right Panel (Specificity):</strong> Proportion of healthy cases correctly identified"),
                "</li>\n                        <li>",
                .("<strong>Horizontal Lines:</strong> Wilson score confidence intervals (at the chosen confidence level) showing precision of estimates"),
                "</li>\n                        <li>",
                .("<strong>Row order:</strong> studies are sorted by their mean of sensitivity and specificity, so each study keeps the same row in both panels and can be read across. Within one panel the rows are therefore <em>not</em> ranked by that panel's own metric"),
                "</li>\n                        <li>",
                .("<strong>Scale:</strong> both panels run from 0 to 1; the tables report the same quantities as percentages"),
                "</li>\n                    </ul>\n                </div>\n\n                <div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>\n                    <h5>",
                .("Clinical Interpretation"),
                "</h5>\n                    <ul>\n                        <li>",
                .("<strong>Consistent Results:</strong> Points clustered together = low heterogeneity"),
                "</li>\n                        <li>",
                .("<strong>Wide Spread:</strong> Points scattered = high heterogeneity (investigate sources)"),
                "</li>\n                        <li>",
                .("<strong>Narrow CIs:</strong> Large studies with precise estimates"),
                "</li>\n                        <li>",
                .("<strong>Wide CIs:</strong> Small studies with less precise estimates"),
                "</li>\n                    </ul>\n                </div>\n\n                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>\n                    <h5>",
                .("Quick Assessment Tips"),
                "</h5>\n                    <ul>\n                        <li>",
                .("Look for outlier studies (points far from others)"),
                "</li>\n                        <li>",
                .("Check if confidence intervals overlap substantially"),
                "</li>\n                        <li>",
                .("Consider whether variation reflects true differences or chance"),
                "</li>\n                        <li>",
                .("Use this plot to identify studies for sensitivity analysis"),
                "</li>\n                    </ul>\n                </div>\n            </div>\n            "
            )

            self$results$forestplot_explanation$setContent(html)
        },

        .populateSROCPlotExplanation = function() {
            html <- paste0(
                "\n            <div class='plot-explanation' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>\n                <h4>",
                .("Summary ROC Plot Interpretation Guide"),
                "</h4>\n\n                <div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 5px; margin: 10px 0;'>\n                    <h5>",
                .("What This Plot Shows"),
                "</h5>\n                    <p>",
                .("<strong>SROC Plot:</strong> Summary Receiver Operating Characteristic curve showing the trade-off between sensitivity and specificity across all studies."),
                "</p>\n\n                    <ul>\n                        <li>",
                .("<strong>X-axis:</strong> False Positive Rate (1 - Specificity) - lower is better"),
                "</li>\n                        <li>",
                .("<strong>Y-axis:</strong> True Positive Rate (Sensitivity) - higher is better"),
                "</li>\n                        <li>",
                .("<strong>Individual Studies:</strong> Circles sized by sample size"),
                "</li>\n                        <li>",
                .("<strong>Pooled Estimate:</strong> Large triangle showing meta-analytic summary"),
                "</li>\n                        <li>",
                .("<strong>Confidence region (dashed):</strong> how precisely the POOLED point is estimated. It shrinks as more studies are added."),
                "</li>\n                        <li>",
                .("<strong>Prediction region (dotted):</strong> where the accuracy of a FUTURE study in a new setting is expected to fall. It includes between-study heterogeneity and does NOT shrink with more studies."),
                "</li>\n                    </ul>\n                </div>\n\n                <div style='background-color: rgba(33, 99, 235, 0.08); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>\n                    <h5>",
                .("Model Choice: Random vs Fixed Effects"),
                "</h5>\n                    <p>",
                .("<strong>REML (Random Effects, Default):</strong> Estimates between-study variance (\u03c4\u00b2) to construct the SROC regression curve and prediction region. Recommended for diagnostic meta-analysis because test thresholds and patient populations inherently vary across studies."),
                "</p>\n                    <p>",
                .("<strong>Fixed Effects:</strong> Assumes every study shares the exact same sensitivity and specificity (\u03c4\u00b2 = 0). Under this assumption, the SROC curve and prediction region are undefined, and confidence intervals are narrower by construction."),
                "</p>\n                </div>\n\n                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>\n                    <h5>",
                .("Confidence region vs prediction region"),
                "</h5>\n                    <p>",
                .("These answer different questions and are routinely confused. A tight <strong>confidence</strong> region means the pooled estimate is well determined; it says nothing about whether the assay will perform consistently. A wide <strong>prediction</strong> region means that even though the average is known precisely, the next study - or your laboratory - could see materially different sensitivity and specificity. <strong>For deciding whether to adopt an assay, the prediction region is the relevant one.</strong> When the two differ greatly, between-study heterogeneity dominates and the pooled point alone should not drive the decision."),
                "</p>\n                </div>\n\n                <div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>\n                    <h5>",
                .("Clinical Interpretation"),
                "</h5>\n                    <ul>\n                        <li>",
                .("<strong>Upper Left Corner:</strong> Ideal performance (high sensitivity, low false positive rate)"),
                "</li>\n                        <li>",
                .("<strong>Diagonal Line:</strong> Represents random chance (no discriminative ability)"),
                "</li>\n                        <li>",
                .("<strong>Above Diagonal:</strong> Better than chance performance"),
                "</li>\n                        <li>",
                .("<strong>Point Scatter:</strong> Studies clustered tightly = consistent test performance"),
                "</li>\n                    </ul>\n                </div>\n\n                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>\n                    <h5>",
                .("Quick Assessment Tips"),
                "</h5>\n                    <ul>\n                        <li>",
                .("Closer to upper-left corner = better overall diagnostic accuracy"),
                "</li>\n                        <li>",
                .("Wide scatter of points = substantial between-study heterogeneity"),
                "</li>\n                        <li>",
                .("Triangle position shows where your pooled test performance lies"),
                "</li>\n                        <li>",
                .("Compare triangle position to individual studies for consistency"),
                "</li>\n                    </ul>\n                </div>\n\n                <div style='background-color: rgba(33, 163, 188, 0.21); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>\n                    <h5>",
                .("Clinical Decision Making"),
                "</h5>\n                    <p>",
                .("<strong>Use this plot to:</strong> Compare the studies' operating points, see the trade-off between sensitivity and specificity across them, and assess consistency across study populations and settings. The curve summarises studies with unknown and differing positivity thresholds and carries no threshold values, so it does not identify a cut-off for a laboratory to adopt."),
                "</p>\n                </div>\n            </div>\n            "
            )

            self$results$srocplot_explanation$setContent(html)
        },

        .populateFunnelPlotExplanation = function() {
            html <- paste0(
                "\n            <div class='plot-explanation' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>\n                <h4>",
                .("Funnel Plot Interpretation Guide"),
                "</h4>\n\n                <div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 5px; margin: 10px 0;'>\n                    <h5>",
                .("What This Plot Shows"),
                "</h5>\n                    <p>",
                .("<strong>Funnel Plot:</strong> Assesses publication bias by plotting study precision against effect size (log diagnostic odds ratio)."),
                "</p>\n\n                    <ul>\n                        <li>",
                .("<strong>X-axis:</strong> Log Diagnostic Odds Ratio (effect size)"),
                "</li>\n                        <li>",
                .("<strong>Y-axis:</strong> 1/\u221a(effective sample size), reversed so the most precise studies sit at the top (Deeks' funnel plot)"),
                "</li>\n                        <li>",
                .("<strong>Each Point:</strong> One study in your meta-analysis"),
                "</li>\n                        <li>",
                .("<strong>Expected Pattern:</strong> Inverted funnel shape if no bias present"),
                "</li>\n                    </ul>\n                </div>\n\n                <div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>\n                    <h5>",
                .("Funnel Asymmetry Indicators"),
                "</h5>\n                    <ul>\n                        <li>",
                .("<strong>Asymmetric Funnel:</strong> Studies concentrated on one side at the imprecise end of the plot"),
                "</li>\n                        <li>",
                .("<strong>Gap in Lower Region:</strong> Small studies with low accuracy absent"),
                "</li>\n                        <li>",
                .("<strong>Deeks' Test p < 0.05:</strong> Statistical evidence of funnel plot asymmetry - evidence of asymmetry, not of publication bias; see the causes listed below"),
                "</li>\n                    </ul>\n                </div>\n\n                <div style='background-color: rgba(33, 162, 64, 0.19); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>\n                    <h5>",
                .("Features Consistent With a Symmetric Funnel"),
                "</h5>\n                    <ul>\n                        <li>",
                .("<strong>Symmetric Funnel:</strong> Studies distributed evenly on both sides"),
                "</li>\n                        <li>",
                .("<strong>Deeks' Test p \u2265 0.05:</strong> No statistical evidence of asymmetry"),
                "</li>\n                        <li>",
                .("<strong>Small Studies Present:</strong> Range of precision levels represented"),
                "</li>\n                        <li>",
                .("<em>These features do not establish that publication bias is absent: asymmetry tests have low power, and are unreliable with fewer than 10 studies.</em>"),
                "</li>\n                    </ul>\n                </div>\n\n                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>\n                    <h5>",
                .("Interpretation Caveats"),
                "</h5>\n                    <ul>\n                        <li>",
                .("<strong>Small Sample:</strong> Funnel plot unreliable with &lt;10 studies"),
                "</li>\n                        <li>",
                .("<strong>Heterogeneity:</strong> Clinical differences can mimic publication bias"),
                "</li>\n                        <li>",
                .("<strong>Other Causes:</strong> Different thresholds, narrower patient spectra in small studies, language or database bias, or chance"),
                "</li>\n                        <li>",
                .("<strong>Action Needed:</strong> If the funnel is asymmetric, look for a clinical explanation first, then for unpublished studies"),
                "</li>\n                    </ul>\n                </div>\n            </div>\n            "
            )

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
