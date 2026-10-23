#' @title Decision Curve Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_vline geom_hline
#' @importFrom ggplot2 labs theme_minimal scale_color_brewer annotate xlim ylim
#' @importFrom ggplot2 scale_x_continuous geom_text geom_bar facet_wrap scale_fill_manual
#' @importFrom ggplot2 scale_x_discrete element_text
#' @importFrom dplyr filter mutate group_by summarise arrange
#' @importFrom tidyr gather
#' @importFrom stats quantile complete.cases
#' @return An \code{R6} class generator object for the \code{decisioncurveClass} backend; used internally by the jamovi analysis wrapper and not called directly.

decisioncurveClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "decisioncurveClass",
    inherit = decisioncurveBase,
    private = list(
            .sourcifyOption = function(option) {
                source <- .sourcifyDiagnosticOption(option)
                if (!is.null(source)) return(source)
                super$.sourcifyOption(option)
            },


        # Store analysis results
        .dcaResults = NULL,
        .treatAllNB = NULL,
        .plotThinning = NULL,
        .plotData = NULL,
        # Outcome prevalence of the analysis cohort, and the clinical impact plot's per-100
        # TP/FP rates at the selected thresholds: what the optional plots draw from (see
        # .plotStateSpec). .analysisData removed: assigned, never read.
        .prevalence = NULL,
        .impactData = NULL,
        .analysisOutcomes = NULL,
        .outcomePositive = NULL,
        # Per-run memo of the pairwise bootstrap comparison, shared by the two comparison
        # tables (see .bootstrapComparisonCached). Reset in .run().
        .bootCompCache = NULL,

        # The positive outcome level actually used by the analysis. Falls back to the raw
        # option only before .run() has resolved it (e.g. an early return).
        # Plot state. The renderers read private fields, which only .run() populates -- and
        # jamovi's render path (.createPlotObject -> do.call(private[[funName]], ...)) NEVER
        # calls .run(). So on any render against an object that has not executed .run() in this
        # process -- reopening a saved .omv, engine recycling, a window resize after an early
        # return -- every renderer hit its NULL guard and returned FALSE: five blank panes, no
        # error, nothing to tell the user why. Publishing the fields as image state and
        # rehydrating from it on render fixes that without touching a line of plotting code.
        #
        # What each renderer actually reads, and nothing per patient. The slices used to carry
        # .dcaResults (a prediction vector per model) and the outcome vector: ~517 KB x 5 in
        # the .omv on a 5,000-row, four-model analysis before slicing, and still 33 KB per
        # optional plot at n = 1000 after it, although relative utility and standardized net
        # benefit used them only to compute ONE number, the prevalence (/review-function
        # 2026-10-13). Every curve, Treat All included, is already in plotData, and the
        # clinical impact plot gets its rates precomputed in .run(). Only visible images get
        # state (.publishPlotStates): jmvcore saves hidden images' state too.
        # i18n: every user-facing string is wrapped in .() with {placeholder} values via .fmt();
        # the "Treat All"/"Treat None" labels are strategy KEYS compared in code and stay literal.
        .plotStateSpec = function() {
            list(
                dcaPlot                    = "plotData",
                clinicalImpactPlot         = "impactData",
                interventionsAvoidedPlot   = "plotData",
                relativeUtilityPlot        = c("plotData", "prevalence"),
                standardizedNetBenefitPlot = c("plotData", "prevalence")
            )
        },

        .plotStateFields = function() {
            list(plotData   = private$.plotData,
                 prevalence = private$.prevalence,
                 impactData = private$.impactData)
        },

        # .modelColourScale() removed: it hardcoded ColorBrewer "Set1" (with a viridis
        # fallback past nine strategies) and, being added BEFORE `+ ggtheme`, was overwritten
        # anyway -- a jamovi ggtheme is a complete theme PLUS discrete fill/colour scales.
        # Those scales call jmvcore::colorPalette(n, theme$palette), which ramps to any n, so
        # simply letting ggtheme supply the colour scale both follows the user's global
        # palette and keeps the >9-strategy guard the helper existed for.

        .plotImageNames = function() {
            c("dcaPlot", "clinicalImpactPlot", "interventionsAvoidedPlot",
              "relativeUtilityPlot", "standardizedNetBenefitPlot")
        },

        .publishPlotStates = function() {
            st   <- private$.plotStateFields()
            spec <- private$.plotStateSpec()
            for (nm in private$.plotImageNames()) {
                img <- self$results[[nm]]
                # Hidden images keep the NULL state .clearPlotStates() gave them: jmvcore saves
                # every non-NULL state into the .omv whether or not the image is visible. Each
                # image's visibility is option-driven, and any option change re-runs .run(),
                # which publishes the state once it is shown.
                if (is.null(img) || !isTRUE(img$visible)) next
                img$setState(st[spec[[nm]]])
            }
        },

        # Drop any state left over from a previous run. Called at the top of .run() so that a
        # run which fails or returns early leaves NO state behind: the renderers then hit their
        # NULL guard and show an empty pane, which is correct, instead of the previous cohort.
        .clearPlotStates = function() {
            for (nm in private$.plotImageNames()) {
                img <- self$results[[nm]]
                if (!is.null(img)) img$setState(NULL)
            }
        },

        # Called first in every renderer. A no-op when .run() has just populated the fields in
        # this process; otherwise it restores them from the state jamovi persisted.
        .restoreFromState = function(image) {
            st <- tryCatch(image$state, error = function(e) NULL)
            if (is.null(st)) return(invisible(NULL))
            # Fill in each field ONLY if it is still empty, and only from the slice this image
            # carries (see .plotStateSpec). Two things depend on that:
            #   - a live .run() has already populated these fields, and must never be
            #     overwritten by whatever state the image happens to be holding;
            #   - the five renderers share one private environment, so a single early-return
            #     guard on any one field made every renderer after the first skip restoring and
            #     return FALSE -- four blank panes with the fifth drawn.
            take <- function(field, value) {
                if (is.null(private[[field]]) && !is.null(value)) private[[field]] <- value
            }
            take(".plotData",   st$plotData)
            take(".prevalence", st$prevalence)
            take(".impactData", st$impactData)

            # ponytail: migration for state saved by the meddecide build of 2026-10-08, whose
            # slices carried per-patient vectors instead of the fields above. Without it a
            # resize or export of such an .omv draws a blank optional plot until an option is
            # changed. Delete once those files no longer matter.
            if (!is.null(st$analysisOutcomes) && !is.null(st$outcomePositive)) {
                take(".prevalence", mean(st$analysisOutcomes == st$outcomePositive))
                if (!is.null(st$dcaResults))
                    take(".impactData", private$.buildImpactData(
                        st$dcaResults, st$analysisOutcomes, st$outcomePositive,
                        private$.parseSelectedThresholdsQuietly()))
            }
            if (!is.null(st$dcaResults) && !is.null(st$treatAllNB)) {
                th <- st$dcaResults[[1]]$thresholds
                take(".plotData", do.call(rbind, c(
                    lapply(names(st$dcaResults), function(m) data.frame(
                        threshold = th, net_benefit = st$dcaResults[[m]]$net_benefits,
                        model = m, stringsAsFactors = FALSE)),
                    list(data.frame(threshold = th, net_benefit = st$treatAllNB,
                                    model = "Treat All", stringsAsFactors = FALSE)))))
            }
            invisible(NULL)
        },

        # Per-100 true- and false-positive rates for every strategy at each table threshold,
        # computed at the EXACT threshold (not the nearest grid point), in strategy order.
        # The clinical impact plot draws these; it scales them by populationSize itself.
        .buildImpactData = function(dca_results, outcomes, positive, thresholds) {
            rows <- list()
            for (m in names(dca_results)) for (t in thresholds) {
                r <- private$.calculateNetBenefit(dca_results[[m]]$predictions, outcomes, t, positive)
                rows[[length(rows) + 1]] <- data.frame(
                    threshold = t, model = m,
                    true_positives_per_100 = r$true_positives_per_100,
                    false_positives_per_100 = r$false_positives_per_100,
                    stringsAsFactors = FALSE)
            }
            do.call(rbind, rows)
        },

        # .parseSelectedThresholds() for a caller with nowhere to show its notices (a renderer,
        # which runs on every resize without .renderNotices()): keep the notice list as it was.
        .parseSelectedThresholdsQuietly = function() {
            saved <- private$.noticeList
            on.exit(private$.noticeList <- saved)
            private$.parseSelectedThresholds()
        },

        # The results table's model columns depend only on which variables the user picked, so
        # they belong here rather than in .run(): built in .run() the table painted a bare
        # three-column skeleton and then visibly restructured on every run cycle.
        # .modelColumnNames() is a pure function of the names, and the names .run() uses
        # (names(private$.dcaResults)) are exactly self$options$models, so both paths agree.
        .init = function() {
            # .parseModelNames(), NOT self$options$models. .run() keys the table columns off
            # names(private$.dcaResults), which are the parsed DISPLAY labels -- so when
            # modelNames is set, building the columns from the raw variable names here left the
            # table with two sets: the .init() ones permanently blank ("." in jamovi, which a
            # clinician reads as "not computable") beside the populated ones.
            models <- private$.parseModelNames()
            if (is.null(models) || length(models) == 0) return(invisible(NULL))
            # ...and the clinical rule, which is a strategy column like any model.
            # .run() keys the columns off names(private$.dcaResults), which gains the rule
            # label after the model names, so scaffolding only the models here made the
            # table visibly grow a column when the run finished -- the exact flicker this
            # .init() scaffolding exists to remove. .ruleStrategyLabel() is the single
            # source of that label, shared with .run(), so the two cannot disagree.
            rule_label <- private$.ruleStrategyLabel()
            # A label that collides with a model name is rejected by .run() with an ERROR,
            # so do not scaffold a column that will never be filled.
            if (!is.null(rule_label) &&
                !tolower(rule_label) %in% tolower(c(models, "Treat All", "Treat None")))
                models <- c(models, rule_label)
            tbl <- self$results$resultsTable
            cols <- private$.modelColumnNames(models)
            existing <- vapply(tbl$columns, function(c) c$name, character(1))
            for (i in seq_along(models)) {
                if (unname(cols[i]) %in% existing) next
                tbl$addColumn(name = unname(cols[i]), title = models[i],
                              type = "number", format = "zto")
            }

            # The ROWS are option-determined too: one per table threshold, strategy, or pair of
            # strategies. Built here with their labels; .run() fills them with setRow(). Added
            # in .run() they made every table appear empty and then restructure on each run.
            # No rows while the positive level is unset either: .run() then stops at the pending
            # notice, and rows laid out here would sit under it with every value cell blank.
            if (is.null(self$options$outcome) ||
                private$.levelUnset(self$options$outcomePositive)) return(invisible(NULL))
            thr <- private$.parseSelectedThresholdsQuietly()
            table_names <- c(
                "resultsTable", "benefitRangeTable", "weightedAUCTable", "comparisonTable",
                "modelComparisonEnhanced", "clinicalImpactTable", "costBenefitTable",
                "decisionConsequencesTable", "resourceUtilizationTable"
            )
            # Pair generation is quadratic; neither pairs nor their translated labels are
            # needed while both comparison tables are hidden.
            pairs <- NULL
            per_strategy_threshold <- NULL
            for (nm in table_names) {
                table <- self$results[[nm]]
                if (!isTRUE(table$visible)) next
                if (nm %in% c("comparisonTable", "modelComparisonEnhanced") && is.null(pairs))
                    pairs <- if (length(models) >= 2)
                        utils::combn(models, 2, simplify = FALSE) else list()
                if (nm %in% c("clinicalImpactTable", "costBenefitTable",
                              "decisionConsequencesTable", "resourceUtilizationTable") &&
                    is.null(per_strategy_threshold)) {
                    per_strategy_threshold <- lapply(
                        seq_len(length(models) * length(thr)), function(k) {
                            m <- (k - 1) %/% length(thr) + 1
                            list(model = models[m], threshold = thr[(k - 1) %% length(thr) + 1])
                        })
                }
                labels <- switch(nm,
                    resultsTable = lapply(thr, function(t) list(threshold = t)),
                    benefitRangeTable = lapply(models, function(m) list(model = m)),
                    weightedAUCTable = lapply(models, function(m) list(model = m)),
                    comparisonTable = lapply(pairs, function(p)
                        list(comparison = .fmt(.('{a} vs {b}'), a = p[1], b = p[2]))),
                    modelComparisonEnhanced = lapply(pairs, function(p)
                        list(model1 = p[1], model2 = p[2])),
                    per_strategy_threshold
                )
                keys <- private$.tableRowKeys(nm, length(models), length(thr))
                for (k in seq_along(keys)) table$addRow(rowKey = keys[k], values = labels[[k]])
            }
            invisible(NULL)
        },

        # Row keys of each table, shared by .init() (which builds the rows) and .run() (which
        # fills them); strategy-by-threshold tables run strategy-major, as their populators do.
        # sprintf(), not paste0(): paste0("pair_", integer(0)) is "pair_", one phantom key for a
        # lone model with a comparison ticked; sprintf() returns character(0).
        .tableRowKeys = function(table_name, n_strategies, n_thresholds) {
            switch(table_name,
                resultsTable = sprintf("thresh_%d", seq_len(n_thresholds)),
                benefitRangeTable = ,
                weightedAUCTable = sprintf("model_%d", seq_len(n_strategies)),
                comparisonTable = ,
                modelComparisonEnhanced = sprintf("pair_%d", seq_len(choose(n_strategies, 2))),
                sprintf("model_%d_%d", rep(seq_len(n_strategies), each = n_thresholds),
                        rep(seq_len(n_thresholds), times = n_strategies)))
        },

        # Make `table` hold exactly the rows `keys`, in order, before .run() setRow()s them.
        # .init() normally built them already, so this does nothing; a direct .run() without
        # .init(), or a reused object whose row set changed, is rebuilt instead of setRow()
        # rejecting a key it cannot find.
        .syncRows = function(table, keys) {
            have <- vapply(table$rowKeys, function(k)
                if (is.character(k) && length(k) == 1) k else NA_character_, character(1))
            if (!identical(have, keys)) {
                .deleteTableRows(table)
                for (k in keys) table$addRow(rowKey = k)
            }
            invisible(NULL)
        },

        # The label the clinical decision rule appears under, as a strategy, in the results
        # table columns and in .dcaResults. Returns NULL when there is no rule to label.
        # Both .init() (which scaffolds the columns) and .run() (which keys them off
        # names(.dcaResults)) call this, so the two can never resolve a different label.
        # NULL too while the rule's positive level is still empty (allowNone): the rule is then
        # left out of the run, so .init() must not scaffold a column for it.
        .ruleStrategyLabel = function() {
            if (!isTRUE(self$options$clinicalDecisionRule)) return(NULL)
            if (is.null(self$options$decisionRuleVar)) return(NULL)
            rp <- self$options$decisionRulePositive
            if (private$.levelUnset(rp)) return(NULL)
            lbl <- self$options$decisionRuleLabel
            # .rocLabel(note = FALSE): a display label only (see .parseModelNames).
            .rocLabel(if (!is.null(lbl) && nzchar(trimws(lbl))) trimws(lbl)
                      else .fmt(.('Clinical Rule ({level})'), level = rp), note = FALSE)
        },

        # Which interval the plot draws. NULL-safe: the option is absent from the compiled
        # Options class until jmvtools::prepare() has been run after adding it.
        .ciBand = function() {
            band <- tryCatch(self$options$ciBand, error = function(e) NULL)
            if (is.null(band) || !band %in% c("pointwise", "simultaneous")) "pointwise" else band
        },

        # Bootstrap intervals are drawn only on the decision curve plot, so they exist only while
        # that plot is on. The plot used to be visible on (showPlot || confidenceIntervals), so
        # unticking "Decision curve plot" did nothing while the intervals were ticked. A disabled
        # jamovi control keeps its value, so the .u.yaml enable alone would still have run
        # bootReps resamples per strategy, and described them in the footnotes, for no plot.
        # Where the Range of Benefit's uncertainty can be read (DC-21-01). The range itself is a
        # point-estimate statement; the bootstrap bands are drawn only on the decision curve plot.
        .rangeUncertaintyPointer = function() {
            if (private$.ciDrawn())
                .("Check whether the model's bootstrap interval on the decision curve plot stays above both reference lines over this range.")
            else
                .("Turn on Bootstrap confidence intervals (with the Decision curve plot) to see the uncertainty of the net benefit over this range.")
        },

        .ciDrawn = function() {
            isTRUE(self$options$confidenceIntervals) && isTRUE(self$options$showPlot)
        },

        .positiveLevel = function() {
            # The else branch used to read `private$.positiveLevel()`, i.e. this
            # method calling itself -- an unconditional infinite recursion that
            # ends in "C stack usage is too close to the limit". It is reachable
            # exactly on the path this comment describes: .run() sets
            # .outcomePositive to NULL before resolving it, so any early return
            # that still consults the positive level blows the stack.
            if (!is.null(private$.outcomePositive)) private$.outcomePositive
            else self$options$outcomePositive
        },

        # An NA cell is never blank in jamovi: jmvcore sends a numeric NA as a double, printed
        # "NaN", and a logical NA as missing, printed "." (Cell$asProtoBuf). "" is the only blank
        # cell, and $asDF still reads it back as NA. Call after setRow(); the same pattern as
        # .footnoteNA in decisioncalculator.b.R. `note` NULL when a table note already says why.
        .footnoteNA = function(table, rowKey, values, note = NULL) {
            for (col in names(values))
                if (isTRUE(is.na(values[[col]]))) {
                    table$setCell(col = col, value = "", rowKey = rowKey)
                    if (!is.null(note)) table$addFootnote(col = col, note = note, rowKey = rowKey)
                }
        },

        # HTML sanitization for security
        .safeHtmlOutput = function(text) {
          if (is.null(text) || length(text) == 0) return("")
          text <- as.character(text)
          # Sanitize potentially dangerous characters
          text <- gsub("&", "&amp;", text, fixed = TRUE)
          text <- gsub("<", "&lt;", text, fixed = TRUE)
          text <- gsub(">", "&gt;", text, fixed = TRUE)
          text <- gsub("\"", "&quot;", text, fixed = TRUE)
          text <- gsub("'", "&#x27;", text, fixed = TRUE)
          text <- gsub("/", "&#x2F;", text, fixed = TRUE)
          return(text)
        },

        # Initialize notice collection list
        .noticeList = list(),

        # Add a notice to the collection
        .addNotice = function(type, title, content) {
          # Drop an exact repeat. .parseSelectedThresholds() raises its warnings and is
          # called from five separate .run()-path methods, so one unparseable entry in
          # "Thresholds for table" printed the identical warning five times over and pushed
          # the genuine clinical warnings above it off the visible area. Two notices equal
          # in type, title AND content carry no information the first one did not.
          for (existing in private$.noticeList) {
            if (identical(existing$type, type) &&
                identical(existing$title, title) &&
                identical(existing$content, content))
              return(invisible(NULL))
          }
          private$.noticeList[[length(private$.noticeList) + 1]] <- list(
            type = type,
            title = title,
            content = content
          )
          invisible(NULL)
        },

        # A fatal input problem: jamovi's own error state, not an ERROR banner beside the tables
        # .init() has already laid out, which read as an empty but otherwise normal analysis
        # (review guide §25). "{}" + msg: the text carries variable, model and level names, and
        # reject() would otherwise read a brace in one as a placeholder.
        .fatal = function(title, content) {
            jmvcore::reject("{}", msg = paste0(title, ": ", content))
        },

        # A Level option left empty. allowNone gives NULL; "" and NA are treated the same.
        .levelUnset = function(x) {
            is.null(x) || length(x) != 1 || is.na(x) || !nzchar(as.character(x))
        },

        # Render collected notices as HTML
        .renderNotices = function() {
          if (length(private$.noticeList) == 0) {
            return()
          }

          # Map notice types to colors and icons
          typeStyles <- list(
            ERROR = list(bgcolor = "rgba(220, 38, 38, 0.10)", border = "#fca5a5", icon = ""),
            STRONG_WARNING = list(bgcolor = "rgba(234, 88, 12, 0.10)", border = "#fdba74", icon = ""),
            WARNING = list(bgcolor = "rgba(202, 138, 4, 0.12)", border = "#fde047", icon = ""),
            INFO = list(bgcolor = "rgba(37, 99, 235, 0.08)", border = "#93c5fd", icon = "")
          )

          html <- "<div style='margin: 10px 0;'>"

          # Most severe first, insertion order within a severity (order() is stable). Insertion
          # order alone put e.g. the Complete-Case WARNING above the small-sample STRONG_WARNING.
          severity <- match(vapply(private$.noticeList, function(n) n$type, character(1)),
                            c("ERROR", "STRONG_WARNING", "WARNING", "INFO"))
          for (notice in private$.noticeList[order(severity)]) {
            style <- typeStyles[[notice$type]] %||% typeStyles$INFO

            html <- paste0(html,
              "<div style='background-color: ", style$bgcolor, "; ",
              "border-left: 4px solid ", style$border, "; ",
              "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
              "<strong>",
              style$icon, " ", private$.safeHtmlOutput(notice$title), "</strong><br>",
              "<span style='color: inherit;'>", private$.safeHtmlOutput(notice$content), "</span>",
              "</div>"
            )
          }

          html <- paste0(html, "</div>")

          self$results$notices$setContent(html)
        },

        # Constants for default values and thresholds
        DECISIONCURVE_DEFAULTS = list(
            selected_thresholds = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30),
            max_models_full_plot = 10           # Plot optimization threshold
        ),

        # Calculate net benefit for a model at given threshold
        .calculateNetBenefit = function(predictions, outcomes, threshold, positive_outcome) {
            # Convert outcomes to binary (1 = positive, 0 = negative)
            binary_outcomes <- as.numeric(outcomes == positive_outcome)

            # Calculate predictions at threshold
            predicted_positive <- predictions >= threshold

            # Calculate confusion matrix elements
            tp <- sum(predicted_positive & binary_outcomes == 1)
            fp <- sum(predicted_positive & binary_outcomes == 0)
            tn <- sum(!predicted_positive & binary_outcomes == 0)
            fn <- sum(!predicted_positive & binary_outcomes == 1)

            n <- length(outcomes)
            prevalence <- sum(binary_outcomes) / n

            # Calculate net benefit
            if (tp + fn == 0) {
                sensitivity <- 0
            } else {
                sensitivity <- tp / (tp + fn)
            }

            if (fp + tn == 0) {
                specificity <- 1
            } else {
                specificity <- tn / (fp + tn)
            }

            # Net benefit formula
            nb <- (tp / n) - (fp / n) * (threshold / (1 - threshold))

            return(list(
                net_benefit = nb,
                sensitivity = sensitivity,
                specificity = specificity,
                tp = tp, fp = fp, tn = tn, fn = fn,
                prevalence = prevalence,
                interventions_per_100 = sum(predicted_positive) / n * 100,
                true_positives_per_100 = tp / n * 100,
                false_positives_per_100 = fp / n * 100
            ))
        },

        # Calculate net benefit for treat all strategy
        .calculateTreatAllNetBenefit = function(outcomes, threshold, positive_outcome) {
            binary_outcomes <- as.numeric(outcomes == positive_outcome)
            prevalence <- mean(binary_outcomes)

            # For treat all: sensitivity = 1, specificity = 0
            nb <- prevalence - (1 - prevalence) * (threshold / (1 - threshold))
            return(nb)
        },

        # Calculate net benefit for treat none strategy (always 0)
        .calculateTreatNoneNetBenefit = function() {
            return(0)
        },

        # Net reduction in interventions compared with treating everyone. This is not the
        # raw proportion classified negative. The false-positive trade-off represented by
        # the threshold odds must be removed from the net-benefit difference first.
        .calculateNetInterventionsAvoided = function(model_nb, treat_all_nb, threshold,
                                                      population = 100) {
            if (length(threshold) != 1 || is.na(threshold) || threshold <= 0 ||
                threshold >= 1 || length(population) != 1 || is.na(population) ||
                population <= 0) {
                return(NA_real_)
            }

            threshold_odds <- threshold / (1 - threshold)
            (model_nb - treat_all_nb) / threshold_odds * population
        },

        # AUC of `pred` for the event and its DeLong (1988) standard error, from mid-rank
        # placement values (Sun & Xu 2014), so it is O(n log n) and handles ties - a binary
        # clinical rule is all ties. V10[i] is the share of non-events below event i (ties
        # half), V01[j] the share of events above non-event j. Matches
        # pROC::var(roc, method = "delong"). NULL with fewer than two of either class.
        .aucDeLong = function(pred, is_event) {
            ok <- !is.na(pred) & !is.na(is_event)
            x <- pred[ok & is_event]
            y <- pred[ok & !is_event]
            n1 <- length(x)
            n0 <- length(y)
            if (n1 < 2 || n0 < 2) return(NULL)
            r_all <- rank(c(x, y))
            v10 <- (r_all[seq_len(n1)] - rank(x)) / n0
            v01 <- 1 - (r_all[n1 + seq_len(n0)] - rank(y)) / n1
            auc <- mean(v10)
            se <- sqrt(stats::var(v10) / n1 + stats::var(v01) / n0)
            z <- stats::qnorm(0.975)
            list(auc = auc, se = se, lower = auc - z * se, upper = auc + z * se)
        },

        # One-sided permutation p-value for AUC < 0.5: the share of random relabellings whose
        # event rank sum is at most the observed one, (b + 1) / (B + 1) so it is a valid test at
        # every n and under any ties. wilcox.test() was not enough: it switches to the normal
        # approximation with ties or a group above 49, and 2 events among 20 tied zeros and 80
        # tied ones gave p = 0.0024 where the exact probability is 0.038 (Codex review,
        # 2026-10-13). Called only after the DeLong bound has already flagged a strategy, and
        # inside its own seed substream, so it costs nothing on ordinary runs and repeats exactly.
        .aucLessPermutationP = function(pred, is_event, n_perm = 2000L) {
            ok <- !is.na(pred) & !is.na(is_event)
            r <- rank(pred[ok])
            ev <- is_event[ok]
            n1 <- sum(ev)
            observed <- sum(r[ev])
            perm <- vapply(seq_len(n_perm),
                           function(i) sum(r[sample.int(length(r), n1)]), numeric(1))
            (sum(perm <= observed + 1e-9) + 1) / (n_perm + 1)
        },

        # Calculate all model metrics at the exact requested threshold. Storing predictions
        # in .dcaResults also lets this work for a binary clinical rule, which has no source
        # model column. This prevents a row labelled with one threshold from silently using
        # the nearest point on the plotted threshold grid.
        .calculateModelAtThreshold = function(model_name, threshold) {
            model_result <- private$.dcaResults[[model_name]]
            if (is.null(model_result) || is.null(model_result$predictions) ||
                is.null(private$.analysisOutcomes)) {
                return(NULL)
            }

            private$.calculateNetBenefit(
                model_result$predictions,
                private$.analysisOutcomes,
                threshold,
                private$.positiveLevel()
            )
        },

        # Dynamic result columns need identifiers as well as display labels. Distinct labels
        # such as "Model A" and "Model-A" sanitize to the same identifier, so make the
        # identifiers unique while leaving the user-visible model names unchanged.
        .modelColumnNames = function(model_names) {
            safe_names <- gsub("[^A-Za-z0-9]", "_", model_names)
            safe_names[!nzchar(safe_names)] <- "model"
            stats::setNames(
                make.unique(paste0("model_", safe_names), sep = "_"),
                model_names
            )
        },
        
        # Net benefit at every threshold from ONE sort of the predictions, instead of one pass
        # over all patients per threshold: O(n log n + T log n), not O(n T). Both bootstraps call
        # this once per replicate. findInterval(left.open = TRUE) counts the predictions strictly
        # below each threshold, so a prediction equal to the threshold is still positive (`>=`,
        # as .calculateNetBenefit and dcurves). Callers pass complete-case vectors (no NA).
        .calculateNetBenefitsVectorized = function(predictions, outcomes, thresholds, positive_outcome) {
            n <- length(outcomes)
            ord <- order(predictions)
            event_sorted <- (outcomes == positive_outcome)[ord]
            n_below <- findInterval(thresholds, predictions[ord], left.open = TRUE)
            tp <- sum(event_sorted) - c(0L, cumsum(event_sorted))[n_below + 1L]
            fp <- (n - n_below) - tp
            (tp / n) - (fp / n) * (thresholds / (1 - thresholds))
        },

        # Generate threshold sequence with enhanced validation
        .generateThresholds = function() {
            range_type <- self$options$thresholdRange
            step <- self$options$thresholdStep

            # Keep the CONFIGURED bounds. Once seq() has collapsed to a single point,
            # min(thresholds) and max(thresholds) are both that point, so a message built from
            # them told a user who typed 20% to 21% that "the range from 20.0% to 20.0% is
            # narrower than the step" -- which is not the range they entered and gives them
            # nothing to correct.
            if (range_type == "auto") {
                range_lo <- 0.01
                range_hi <- 0.99
                thresholds <- seq(range_lo, range_hi, by = step)
            } else if (range_type == "clinical") {
                range_lo <- 0.05
                range_hi <- 0.50
                thresholds <- seq(range_lo, range_hi, by = step)
            } else { # custom
                min_thresh <- self$options$thresholdMin
                max_thresh <- self$options$thresholdMax

                # Enhanced threshold range validation with clinical guidance
                private$.validateThresholdRange(min_thresh, max_thresh)

                range_lo <- min_thresh
                range_hi <- max_thresh
                thresholds <- seq(range_lo, range_hi, by = step)
            }

            # seq() accumulates roundoff, so a grid point LABELLED 15% is 0.15000000000000002
            # (likewise 6%, 12%, 21%, 22% on the default grid). Predictions are compared with
            # `>=`, so every patient whose risk is stored as exactly 0.15 dropped out of the curve
            # at 15% while the table row at 0.15 counted them: on a lookup-table score the curve
            # put the model below treat-all at 12% where the table had it above. Snap a point to
            # its 10-decimal value only when the gap is roundoff-sized, so a user-entered bound
            # with more than 10 decimals is never moved (VAL-decisioncurve-01, 2026-10-12).
            rounded <- round(thresholds, 10)
            snap <- abs(thresholds - rounded) <= 8 * .Machine$double.eps
            thresholds[snap] <- rounded[snap]
            thresholds <- unique(thresholds)

            # A range narrower than the step yields a single point. Nothing downstream guarded
            # against that: the analysis reported success, the weighted-AUC table printed
            # "20% - 20%" with a blank average net benefit, and every statistic in the model
            # comparison came back empty -- while the notice affirmed the threshold range. One
            # threshold cannot describe a curve, so refuse it and say what to change.
            pct <- function(x) sprintf("%.1f", x * 100)
            if (length(thresholds) < 2) {
                narrow_msg <- paste(
                    .fmt(.('The threshold range you entered, {lo}% to {hi}%, is narrower than the step size of {step}%, so it produces only one threshold ({one}%) and no decision curve can be drawn.'),
                         lo = pct(range_lo), hi = pct(range_hi), step = pct(step), one = pct(thresholds[1])),
                    .fmt(.('Reduce the step size to {maxstep}% or less, or widen the range.'),
                         maxstep = pct(max(0.001, (range_hi - range_lo))))
                )
                private$.addNotice(
                    type = "ERROR",
                    title = .("Threshold Range Too Narrow"),
                    content = narrow_msg
                )
                private$.renderNotices()
                # jmvcore::reject() rather than a bare stop(): it is the documented entry
                # point and is what the rest of the module uses. It is NOT a behaviour change
                # -- reject() IS stop(createError(...)) and createError() ends with
                # error$code <- NULL, so the two conditions are identical (measured:
                # simpleError/error/condition, same message, conditionCall NULL in both,
                # same field names). The bare stop() already carried this message verbatim.
                jmvcore::reject(narrow_msg)
            }

            # seq() stops at the last whole step, so a step that does not divide the range quietly
            # shortened it: 10% to 29% by 10% analysed 10% to 20%, and the range of benefit, the
            # average net benefit and both comparisons answered that shorter range (Codex review
            # 2026-10-14). The grid is kept - Stata's dca steps the same way - and the cut named.
            if (range_hi - max(thresholds) > 1e-9) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Threshold Range Shortened"),
                    content = paste(
                        .fmt(.('With a step of {step}%, the threshold grid starting at {lo}% stops at {last}%, so thresholds above it, up to the requested maximum of {hi}%, are not analysed.'),
                             step = pct(step), lo = pct(range_lo), last = pct(max(thresholds)), hi = pct(range_hi)),
                        .('Choose a step size that divides the threshold range to analyse all of it.')
                    )
                )
            }

            return(thresholds)
        },
        
        # "Thresholds for table" is independent of the analysed threshold range, so a row can be
        # requested at a probability the curves were never computed over. Each such row IS
        # computed correctly, at the exact threshold asked for -- but it sits in a table whose
        # neighbours all come from the plotted range, with nothing on screen to say so, and the
        # curve above it does not extend that far. Now that the table is shown by default this
        # is the first thing many users will see.
        .warnOnThresholdsOutsideRange = function(selected) {
            grid <- tryCatch(private$.dcaResults[[1]]$thresholds, error = function(e) NULL)
            if (is.null(grid) || !length(grid) || is.null(selected) || !length(selected))
                return(invisible(NULL))
            lo <- min(grid)
            hi <- max(grid)
            outside <- selected[selected < lo - 1e-12 | selected > hi + 1e-12]
            if (!length(outside)) return(invisible(NULL))

            values <- paste0(sprintf("%.1f%%", outside * 100), collapse = ", ")
            lo_pct <- sprintf("%.1f", lo * 100)
            hi_pct <- sprintf("%.1f", hi * 100)
            content <- if (length(outside) == 1) {
                paste(
                    .fmt(.('The table reports a row at {value}, which falls outside the analysed threshold range of {lo}% to {hi}%.'),
                         value = values, lo = lo_pct, hi = hi_pct),
                    .('It is computed correctly at that exact value, but it is not shown on the decision curve, and the surrounding rows come from the analysed range.'),
                    .('Widen the threshold range, or remove it from "Thresholds for table".')
                )
            } else {
                paste(
                    .fmt(.('The table reports rows at {values}, which fall outside the analysed threshold range of {lo}% to {hi}%.'),
                         values = values, lo = lo_pct, hi = hi_pct),
                    .('They are computed correctly at those exact values, but they are not shown on the decision curve, and the surrounding rows come from the analysed range.'),
                    .('Widen the threshold range, or remove them from "Thresholds for table".')
                )
            }
            private$.addNotice(
                type = "WARNING",
                title = .("Table thresholds outside the analysed range"),
                content = content
            )
            invisible(NULL)
        },

        # Net benefit at every threshold is driven by the true positives, so the EVENT count
        # bounds the precision of the whole curve regardless of how many patients were enrolled.
        # Thresholds follow the usual events-per-variable rules of thumb for a binary outcome.
        .warnOnLowEventCount = function(data, outcome_var, complete_cases) {
            outcomes <- tryCatch(data[[outcome_var]][complete_cases], error = function(e) NULL)
            if (is.null(outcomes)) return(invisible(NULL))
            positive <- private$.positiveLevel()
            if (is.null(positive)) return(invisible(NULL))
            n_events <- sum(as.character(outcomes) == as.character(positive), na.rm = TRUE)
            if (!is.finite(n_events) || n_events >= 25) return(invisible(NULL))

            first <- if (n_events == 1)
                .('Only 1 event of the outcome is present in the analysed cases.')
            else
                .fmt(.('Only {n} events of the outcome are present in the analysed cases.'), n = n_events)
            second <- if (n_events < 10)
                .('Net benefit at every threshold is driven by these events, so the curves and any confidence intervals are imprecise and may be unstable.')
            else
                .('Net benefit at every threshold is driven by these events, so the curves and any confidence intervals are imprecise.')
            private$.addNotice(
                type = if (n_events < 10) "STRONG_WARNING" else "WARNING",
                title = .("Few Outcome Events"),
                content = paste(first, second,
                    .('Interpret differences between models with caution and do not choose a threshold from this curve alone.'))
            )
            invisible(NULL)
        },

        # The false-positive penalty, and the whole treat-all line, rest on the patients WITHOUT
        # the outcome, so they bound precision exactly as the events do. A 90%-prevalence cohort of
        # 200 has 20 of them and used to pass with no notice at all (VAL-decisioncurve-03).
        .warnOnLowNonEventCount = function(outcomes, positive) {
            n_nonevents <- sum(as.character(outcomes) != as.character(positive), na.rm = TRUE)
            if (!is.finite(n_nonevents) || n_nonevents >= 25) return(invisible(NULL))
            private$.addNotice(
                type = if (n_nonevents < 10) "STRONG_WARNING" else "WARNING",
                title = .("Few Patients Without the Outcome"),
                content = paste(
                    .fmt(.('Only {n} of the analysed cases do not have the outcome.'), n = n_nonevents),
                    .('The false-positive penalty in net benefit and the treat-all reference rest on these patients, so the curves and any confidence intervals are imprecise.'),
                    .('Interpret differences between models with caution and do not choose a threshold from this curve alone.'))
            )
            invisible(NULL)
        },

        # Validate threshold ranges with clinical context and guidance
        .validateThresholdRange = function(min_thresh, max_thresh) {
            # Basic validation
            min_pct <- sprintf("%.1f", min_thresh * 100)
            max_pct <- sprintf("%.1f", max_thresh * 100)
            if (min_thresh >= max_thresh) {
                msg <- paste(
                    .fmt(.('Minimum threshold ({min}%) must be less than maximum threshold ({max}%).'),
                         min = min_pct, max = max_pct),
                    .('Please adjust the threshold range in Analysis Options.')
                )
                private$.addNotice(type = "ERROR", title = .("Invalid Threshold Range"), content = msg)
                private$.renderNotices()
                # The banner replaces the pane that holds the notice above, so it has to carry
                # the message itself -- "Validation failed" told the clinician nothing.
                jmvcore::reject(msg)
            }

            if (min_thresh <= 0 || max_thresh >= 1) {
                msg <- paste(
                    .('Threshold probabilities must lie strictly between 0 and 1.'),
                    .fmt(.('Current settings: Min = {min}%, Max = {max}%; the valid range is 0.1% to 99.9%.'),
                         min = min_pct, max = max_pct)
                )
                private$.addNotice(type = "ERROR", title = .("Threshold Out of Bounds"), content = msg)
                private$.renderNotices()
                jmvcore::reject(msg)
            }
            
            # Context-neutral guidance for unusual ranges. There is no universally valid
            # threshold range for screening, treatment or surgery: the range must represent
            # the actual harm-benefit trade-off for the decision under study.
            if (max_thresh > 0.8) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("High Threshold Range"),
                    content = paste(
                        .fmt(.('The maximum threshold is {max}%.'), max = max_pct),
                        .('Confirm that a risk this high genuinely represents the point at which the intended intervention becomes worthwhile in the target clinical setting.')
                    )
                )
            }

            if (min_thresh < 0.01) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Low Threshold Range"),
                    content = paste(
                        .fmt(.('The minimum threshold is {min}%.'), min = min_pct),
                        .('Confirm that a risk this low genuinely represents the point at which the intended intervention becomes worthwhile in the target clinical setting.')
                    )
                )
            }

            # Range size warnings
            range_size <- max_thresh - min_thresh
            span_txt <- .fmt(.('The selected threshold range spans {span} percentage points.'),
                             span = sprintf("%.1f", range_size * 100))
            # 1e-9 margins: 0.80 - 0.10 is 0.7000000000000001 and 0.25 - 0.20 is
            # 0.04999999999999999, so a range of exactly 70 or 5 points tripped the strict test.
            if (range_size > 0.7 + 1e-9) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Wide Threshold Range"),
                    content = paste(span_txt,
                        .('Interpret the curve within a prespecified clinically plausible subrange; summaries that average across the full range weight every threshold equally.'))
                )
            }

            if (range_size < 0.05 - 1e-9) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Narrow Range"),
                    content = paste(span_txt,
                        .('Confirm that this narrow interval was prespecified from the clinical decision and is wide enough to cover plausible treatment preferences.'))
                )
            }
        },

        # Parse selected thresholds for table
        .parseSelectedThresholds = function() {
            threshold_str <- self$options$selectedThresholds
            # is.null first: `NULL == ""` is logical(0), and `if (logical(0))` is the raw
            # R error "argument is of length zero" rather than a message anyone can act on.
            if (is.null(threshold_str) || !nzchar(threshold_str)) {
                return(private$DECISIONCURVE_DEFAULTS$selected_thresholds)
            }

            # Split on commas, semicolons or whitespace. This used to read "[,;\\s]+":
            # inside a POSIX bracket expression TRE treats \s as the literal characters
            # backslash and s, so space-separated entry produced one unparseable token, every
            # value became NA, and the analysis silently fell back to the default thresholds
            # while showing the user's own text in the box.
            raw <- unlist(strsplit(threshold_str, "[,;[:space:]]+"))
            raw <- raw[nzchar(raw)]
            parsed <- suppressWarnings(as.numeric(raw))

            unparsed <- raw[is.na(parsed)]
            kept <- parsed[!is.na(parsed)]
            out_of_range <- kept[kept <= 0 | kept >= 1]
            thresholds <- kept[kept > 0 & kept < 1]

            if (length(unparsed) > 0 || length(out_of_range) > 0) {
                # Percentages are the natural way for a clinician to think about a
                # threshold, so name that mistake explicitly rather than leaving them
                # to infer it from "between 0 and 1".
                looks_like_pct <- length(unparsed) == 0 && length(out_of_range) > 0 &&
                    all(out_of_range > 1 & out_of_range <= 100)
                private$.addNotice(
                    type = "WARNING",
                    title = .("Some thresholds ignored"),
                    content = paste(
                        .fmt(.('Ignored {values}.'),
                             values = paste(c(unparsed, base::format(out_of_range)), collapse = ", ")),
                        if (looks_like_pct)
                            .fmt(.('These look like percentages: enter {values} instead.'),
                                 values = paste(base::format(out_of_range / 100, trim = TRUE), collapse = ", "))
                        else NULL,
                        .('Threshold probabilities must be numbers strictly between 0 and 1, separated by commas or spaces.')
                    )
                )
            }

            if (length(thresholds) == 0) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Using default thresholds"),
                    content = .("No usable threshold probabilities were found in the list, so the default 5%, 10%, 15%, 20%, 25% and 30% are used.")
                )
                return(private$DECISIONCURVE_DEFAULTS$selected_thresholds)
            }

            return(sort(unique(thresholds)))
        },

        # Parse model names. These are DISPLAY labels only -- data columns are read through
        # self$options$models -- so they pass through .rocLabel(note = FALSE) once here: table
        # cells and column titles are parsed as HTML by the jamovi client, which swallowed a
        # "<" before a letter ("Risk<High>" showed as "Risk"; security audit 2026-10-14).
        # Every consumer gets the same label, so keys and comparisons stay consistent.
        .parseModelNames = function() {
            model_names_str <- self$options$modelNames
            model_vars <- self$options$models

            # is.null FIRST: `NULL == ""` is logical(0), and `logical(0) || x` is an error
            # in R >= 4.3 rather than a usable FALSE.
            if (is.null(model_names_str) || !nzchar(model_names_str)) {
                return(.rocLabel(model_vars, note = FALSE))
            }

            # Parse comma-separated names
            parsed_names <- trimws(unlist(strsplit(model_names_str, ",", fixed = TRUE)))

            # If the number of names does not match, or any label is empty, use the source
            # variable names. .run() reports this substitution to the user.
            if (length(parsed_names) != length(model_vars) || any(!nzchar(parsed_names))) {
                return(.rocLabel(model_vars, note = FALSE))
            }

            return(.rocLabel(parsed_names, note = FALSE))
        },
        
        # NOTE: a .calculateBootstrapCIChunked() path used to live here and has been removed.
        # It was unreachable except as a crash: it delegated back to .calculateBootstrapCI()
        # when n_boot <= 10000 while .calculateBootstrapCI() delegated to it when
        # n_boot >= 10000, so bootReps at its own documented maximum of 10000 satisfied both
        # guards and the two recursed into each other until R aborted with "evaluation nested
        # too deeply". Above 10000 it was unreachable because the option caps there. It was
        # also statistically wrong where it did run: it averaged the per-chunk quantiles
        # rather than taking quantiles of the pooled replicates, which understates the
        # interval width.

        # The seed the user asked for, with the documented fallback. Every note that prints
        # "seed N" and every substream below reads it here, so the number on screen is always
        # the number the draws came from.
        .seedValue = function() {
            s <- self$options$seed
            if (is.null(s) || is.na(s)) 42L else as.integer(s)
        },

        # Each stochastic consumer draws from its OWN substream derived from that seed.
        # Before this, .run() seeded once and every bootstrap continued the same stream, so
        # what a consumer drew depended on which OTHER consumers had run ahead of it: ticking
        # "Confidence intervals" consumed bootReps x n_models resamples before the comparison
        # tables and moved their p-values, though nothing about the comparison had changed --
        # while the table note promised that the same seed reproduces the numbers exactly.
        #
        # stream: a fixed id per consumer (1 model CIs, 2 clinical-rule CI, 3 pairwise
        # comparisons, 4 inversion-guard permutations). index: position within the consumer
        # (model number, pair number, strategy number), so
        # adding a model cannot shift an earlier model's draws either. Streams sit 100000
        # apart and the seed option maxes out at 999999, so no two (stream, index) pairs
        # collide within a run.
        .bootSeed = function(stream, index = 0L) {
            private$.seedValue() + as.integer(stream) * 100000L + as.integer(index)
        },

        # Both comparison tables bootstrap the SAME pair with the same inputs and the same
        # derived seed (stream 3, pair index), so the second one recomputes a byte-identical
        # answer. With 5 models and bootReps = 10000 that is 200000 resamples where 100000
        # suffice. Memoised per .run() (.bootCompCache is cleared there with the other
        # analysis state), keyed on everything that can change the result: the pair, the
        # substream index, the replicate count and the threshold grid.
        .bootstrapComparisonCached = function(m1, m2, pair_index, pred1, pred2, outcomes,
                                              thresholds, n_boot) {
            key <- paste(m1, m2, pair_index, n_boot,
                         length(thresholds), thresholds[1], thresholds[length(thresholds)],
                         sep = "|")
            hit <- private$.bootCompCache[[key]]
            if (!is.null(hit))
                return(hit)

            res <- withr::with_seed(
                private$.bootSeed(3L, pair_index),
                private$.calculateBootstrapComparison(
                    pred1, pred2, outcomes, thresholds, private$.positiveLevel(),
                    n_boot = n_boot
                )
            )
            private$.bootCompCache[[key]] <- res
            res
        },

        # Bootstrap confidence intervals with enhanced error handling and progress reporting
        .calculateBootstrapCI = function(predictions, outcomes, thresholds, positive_outcome, n_boot = 1000) {

            # Validate inputs
            if (length(predictions) != length(outcomes)) {
                msg <- paste(
                    .fmt(.('Bootstrap CI calculation error: predictions and outcomes have different lengths ({n1} vs {n2}).'),
                         n1 = length(predictions), n2 = length(outcomes)),
                    .('This indicates a data processing error; please report it.')
                )
                private$.addNotice(type = "ERROR", title = .("Bootstrap CI Calculation Error"), content = msg)
                private$.renderNotices()
                jmvcore::reject(msg)
            }

            if (n_boot < 100) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Low Bootstrap Replications"),
                    content = paste(
                        .fmt(.('Low bootstrap replications ({n}).'), n = n_boot),
                        .('Using fewer than 100 replications may give unreliable confidence intervals; consider at least 1000 replications for stable estimates.')
                    )
                )
            }

            n <- length(outcomes)
            boot_results <- array(NA, dim = c(n_boot, length(thresholds)))
            
            tryCatch({
                for (i in seq_len(n_boot)) {
                    # Let jamovi interrupt a long run. Without this the replicate loop is
                    # uninterruptible: 2,000 replications on 1,000 rows with two models measured
                    # 13.8 s with no way to stop it, and a fine threshold step on a larger
                    # cohort runs for minutes. message() was the old progress channel and jamovi
                    # never displays it (see the note in .plotDCA).
                    if (i %% 50 == 0) private$.checkpoint()

                    # Bootstrap sample with error checking
                    boot_idx <- sample(n, n, replace = TRUE)
                    boot_pred <- predictions[boot_idx]
                    boot_out <- outcomes[boot_idx]

                    # An all-one-class resample used to be discarded here. Net benefit is
                    # TP/n - FP/n * odds, which is perfectly well defined when a resample
                    # contains no events (it is simply -FP/n * odds) or no controls, so the
                    # skip threw away legitimate draws -- and only from the LOWER tail, biasing
                    # the lower confidence limit upward. Measured on n = 30 with 3 events at
                    # B = 4000: 5.05% of draws discarded and the lower limit shifted up by as
                    # much as 0.026, with the upper limit unchanged. Keep every resample.

                    # The whole curve in one sorted sweep. A per-threshold loop over
                    # .calculateNetBenefit() took 2.23 s at n = 1000, B = 1000, 46 thresholds;
                    # this takes ~0.1 s with identical draws, so bootReps = 10000 on a fine step
                    # no longer runs for tens of minutes (/review-function 2026-10-13).
                    boot_results[i, ] <- private$.calculateNetBenefitsVectorized(
                        boot_pred, boot_out, thresholds, positive_outcome
                    )
                }

                # Pointwise percentile intervals, one per threshold.
                ci_lower <- apply(boot_results, 2, function(x) {
                    if (sum(!is.na(x)) < 10) return(NA)
                    quantile(x, probs = (1 - self$options$ciLevel) / 2, na.rm = TRUE)
                })
                
                ci_upper <- apply(boot_results, 2, function(x) {
                    if (sum(!is.na(x)) < 10) return(NA)
                    quantile(x, probs = 1 - (1 - self$options$ciLevel) / 2, na.rm = TRUE)
                })

                # Simultaneous sup-t band from the SAME replicates: the critical-value-based
                # bootstrap band, Montiel Olea & Plagborg-Moller (2019) supplement Appendix A.2,
                # Algorithm 3 (it was attributed to Mandel & Betensky 2008, whose band is built
                # from bootstrap ranks instead). Standardise each replicate's deviation from the
                # observed curve by the bootstrap SE at that threshold, take the largest
                # deviation across the whole curve per replicate, and use its (1 - alpha)
                # quantile as a common multiplier. Coverage of the whole curve is approximate:
                # 93% at n = 200 but 80% at n = 80 with ~10 events in the 2026-10-12 simulation.
                # Where the SE is zero (no one in the sample is classified positive) the band
                # collapses onto NB = 0; that is a property of the sample, not certainty about
                # the population, which is why sparse thresholds are flagged in .run().
                nb_hat <- private$.calculateNetBenefitsVectorized(
                    predictions, outcomes, thresholds, positive_outcome
                )
                se_boot <- apply(boot_results, 2, stats::sd, na.rm = TRUE)
                usable <- is.finite(se_boot) & se_boot > 0
                # Zero-SE thresholds collapse onto the estimate, as the percentile interval does. With
                # NO usable threshold (a strategy that classifies nobody positive anywhere in the
                # range) the band used to stay all NA and silently vanished from the plot, beside a
                # footnote saying it was drawn for every strategy (Codex review 2026-10-14).
                sim_lower <- sim_upper <- nb_hat
                if (any(usable)) {
                    dev <- abs(sweep(boot_results[, usable, drop = FALSE], 2, nb_hat[usable], "-"))
                    dev <- sweep(dev, 2, se_boot[usable], "/")
                    max_dev <- apply(dev, 1, max, na.rm = TRUE)
                    max_dev <- max_dev[is.finite(max_dev)]
                    if (length(max_dev) >= 10) {
                        q_sim <- unname(quantile(max_dev, probs = self$options$ciLevel))
                        sim_lower[usable] <- nb_hat[usable] - q_sim * se_boot[usable]
                        sim_upper[usable] <- nb_hat[usable] + q_sim * se_boot[usable]
                    } else {
                        sim_lower[] <- NA_real_
                        sim_upper[] <- NA_real_
                    }
                }

                return(list(lower = ci_lower, upper = ci_upper,
                            sim_lower = sim_lower, sim_upper = sim_upper))
                
            }, error = function(e) {
                # .checkpoint() signals a restart by stop()ping with a condition carrying
                # code == "restart" (jmvcore::createError("restarting", "restart")). It is
                # called inside the replicate loop above, so this handler sees it -- and would
                # turn "the user changed an option, abandon this run" into a permanent
                # "Bootstrap CI Failed" warning with NA intervals. Re-raise it untouched.
                if (identical(e$code, "restart")) stop(e)
                private$.addNotice(
                    type = "WARNING",
                    title = .("Bootstrap CI Failed"),
                    content = paste(
                        .fmt(.('Bootstrap confidence interval calculation failed: {error}.'), error = conditionMessage(e)),
                        .('The analysis continues without confidence intervals; the point estimates are unaffected.')
                    )
                )
                return(list(
                    lower = rep(NA, length(thresholds)),
                    upper = rep(NA, length(thresholds)),
                    sim_lower = rep(NA, length(thresholds)),
                    sim_upper = rep(NA, length(thresholds))
                ))
            })
        },

        # Find optimal threshold for a model
        # Range of threshold probabilities over which a model is the best available
        # strategy, i.e. its net benefit exceeds BOTH reference strategies.
        #
        # This deliberately replaces a "maximum net benefit / optimal threshold" summary.
        # Net benefit need not be monotone because the set of patients classified positive
        # changes with the threshold. More importantly, threshold probability expresses the
        # decision-maker's weighting of a missed case against an unnecessary intervention; it
        # should be prespecified from the clinical decision rather than optimized on these
        # data. The range below is descriptive and does not estimate an optimal threshold.
        #
        # Comparison against treat-none alone is not enough: at low thresholds nearly any
        # model clears treat-none while still being worse than simply treating everyone.
        .findBenefitRange = function(net_benefits, thresholds, treat_all_nb) {
            treat_none_nb <- 0

            if (is.null(treat_all_nb) || length(treat_all_nb) != length(net_benefits)) {
                reference <- rep(treat_none_nb, length(net_benefits))
            } else {
                reference <- pmax(treat_all_nb, treat_none_nb)
            }

            # Tolerance, not a bare `>`. The model's net benefit and the treat-all reference are
            # computed by algebraically identical but not bitwise identical routes -- tp/n and
            # (n - sum)/n versus 1 - sum/n -- so a model that IS treat-all (every prediction
            # above every plotted threshold) differed from it by ~1e-16 and was counted superior
            # at 15 of 40 thresholds. That produced a "Range of Benefit: 5% to 19%" claim in the
            # default Clinical Interpretation for a predictor generated independently of the
            # outcome. A difference this small is not a clinical benefit at any sample size.
            nb_tol <- 1e-10
            superior <- !is.na(net_benefits) & (net_benefits - reference) > nb_tol

            if (!any(superior)) {
                return(list(
                    range_start = NA_real_,
                    range_end = NA_real_,
                    width = NA_real_,
                    contiguous = NA
                ))
            }

            idx <- which(superior)
            range_start <- thresholds[min(idx)]
            range_end <- thresholds[max(idx)]

            # A model can beat both references over two separated stretches. Reporting only
            # the endpoints would then imply benefit across a gap where there is none.
            contiguous <- identical(as.integer(idx), as.integer(seq(min(idx), max(idx))))

            return(list(
                range_start = range_start,
                range_end = range_end,
                width = range_end - range_start,
                contiguous = contiguous
            ))
        },

        # Calculate weighted AUC
        .calculateWeightedAUC = function(net_benefits, thresholds) {
            # Remove any missing values
            valid_idx <- !is.na(net_benefits) & !is.na(thresholds)
            nb_clean <- net_benefits[valid_idx]
            th_clean <- thresholds[valid_idx]

            if (length(nb_clean) < 2) {
                return(NA)
            }

            # Calculate AUC using trapezoidal rule
            # Sort by threshold
            ord <- order(th_clean)
            nb_sorted <- nb_clean[ord]
            th_sorted <- th_clean[ord]

            # Trapezoidal integration
            auc <- 0
            for (i in 2:length(th_sorted)) {
                width <- th_sorted[i] - th_sorted[i-1]
                height <- (nb_sorted[i] + nb_sorted[i-1]) / 2
                auc <- auc + width * height
            }

            # Normalize by range
            total_range <- max(th_sorted) - min(th_sorted)
            return(auc / total_range)
        },

        # Bootstrap comparison for Weighted AUC difference AND Mean NB difference
        .calculateBootstrapComparison = function(pred1, pred2, outcomes, thresholds, positive_outcome, n_boot = 1000) {
            
            n <- length(outcomes)
            
            wauc_diff_results <- numeric(n_boot)
            nb_diff_results <- numeric(n_boot)
            
            valid_boot <- 0
            
            for (i in seq_len(n_boot)) {
                # Interruptible, like the CI bootstrap. This loop runs the full bootReps and is
                # the slower of the two (two vectorised net-benefit sweeps per replicate).
                if (i %% 50 == 0) private$.checkpoint()

                # Bootstrap sample
                boot_idx <- sample(n, n, replace = TRUE)
                b_pred1 <- pred1[boot_idx]
                b_pred2 <- pred2[boot_idx]
                b_out <- outcomes[boot_idx]

                # No single-class skip here either. The CI bootstrap above stopped discarding
                # these draws because net benefit is well defined without events or without
                # controls; leaving the guard here would have left the two bootstraps in the
                # same analysis using different resample-inclusion rules, so the interval and
                # the p-value beside it would answer slightly different questions.
                
                # Calculate Net Benefits (using vectorized method)
                nb1_vals <- private$.calculateNetBenefitsVectorized(b_pred1, b_out, thresholds, positive_outcome)
                nb2_vals <- private$.calculateNetBenefitsVectorized(b_pred2, b_out, thresholds, positive_outcome)
                
                # Calculate wAUC
                wauc1 <- private$.calculateWeightedAUC(nb1_vals, thresholds)
                wauc2 <- private$.calculateWeightedAUC(nb2_vals, thresholds)
                
                # Calculate Mean NB Difference
                nb_diff_vals <- nb1_vals - nb2_vals
                mean_nb_diff <- mean(nb_diff_vals, na.rm = TRUE)
                
                if (!is.na(wauc1) && !is.na(wauc2) && !is.na(mean_nb_diff)) {
                    valid_boot <- valid_boot + 1
                    wauc_diff_results[valid_boot] <- wauc1 - wauc2
                    nb_diff_results[valid_boot] <- mean_nb_diff
                }
            }
            
            # Truncate to valid results
            if (valid_boot < 50) return(list(
                wauc = list(ci_lower=NA, ci_upper=NA, p_value=NA),
                nb = list(ci_lower=NA, ci_upper=NA, p_value=NA)
            ))
            
            wauc_diff_results <- wauc_diff_results[1:valid_boot]
            nb_diff_results <- nb_diff_results[1:valid_boot]
            
            alpha <- 1 - self$options$ciLevel
            
            # Helper for stats.
            # The two-sided bootstrap p-value uses the (b + 1) / (B + 1) convention of
            # Davison & Hinkley (1997, Sec. 4.2). Without the +1 the p-value is exactly 0
            # whenever every replicate falls on one side of the null, which is routine at
            # the default B and would be reported as an impossible p = 0.
            calc_stats <- function(vals) {
                ci_l <- quantile(vals, probs = alpha / 2, na.rm = TRUE)
                ci_u <- quantile(vals, probs = 1 - alpha / 2, na.rm = TRUE)
                n_valid <- sum(!is.na(vals))
                if (n_valid == 0) {
                    return(list(ci_lower = NA, ci_upper = NA, p_value = NA))
                }
                p_pos <- (sum(vals >= 0, na.rm = TRUE) + 1) / (n_valid + 1)
                p_neg <- (sum(vals <= 0, na.rm = TRUE) + 1) / (n_valid + 1)
                p_val <- min(1, 2 * min(p_pos, p_neg))
                return(list(ci_lower = ci_l, ci_upper = ci_u, p_value = p_val))
            }
            
            return(list(
                wauc = calc_stats(wauc_diff_results),
                nb = calc_stats(nb_diff_results)
            ))
        },

        # Main analysis function
        .run = function() {

            # jamovi reuses this R6 object across run cycles, so the notice list survives
            # from one run to the next. Without this reset every notice is re-appended on
            # each option change and the panel fills with duplicates.
            private$.noticeList <- list()
            private$.plotThinning <- NULL
            private$.outcomePositive <- NULL

            # Clear the previous run's NARRATIVE. jamovi persists Html result content across
            # .run() invocations: jmvcore:::Html$fromProtoBuf restores private$.content
            # UNCONDITIONALLY -- its clearWith block only sets .stale and breaks -- whereas
            # Table$fromProtoBuf return()s on a clearWith hit. procedureNotes and summaryText
            # are written only on the success path, after all eleven early returns, so without
            # this reset a failed re-run left the PREVIOUS dataset's "Analysis Complete" and
            # full clinical interpretation on screen next to the new error. On a data-only
            # change (an edit or a filter) they were not even greyed. Same fix as
            # R/survival.b.R:280-286.
            self$results$procedureNotes$setContent("<div></div>")
            self$results$summaryText$setContent("<div></div>")
            self$results$notices$setContent("<div></div>")  # Html: the client skips ""

            # Clear the previous run's analysis state. The five .plot* renderers read these
            # private fields directly, so without this an early return - a bad variable, a
            # non-probability column, an invalid threshold range - left the PREVIOUS run's
            # curves on screen beside the new error notice, and the clinician saw a decision
            # curve that did not come from the data they were looking at.
            private$.dcaResults <- NULL
            private$.plotData <- NULL
            private$.treatAllNB <- NULL
            private$.prevalence <- NULL
            private$.impactData <- NULL
            private$.analysisOutcomes <- NULL
            # Never serve a pairwise bootstrap from a previous data set or option set.
            private$.bootCompCache <- list()

            # ...and the IMAGE STATE with them. Clearing only the private fields is not enough
            # now that the renderers rehydrate from state: jamovi persists image state across
            # run cycles (jmvcore:::Image$fromProtoBuf only drops it when a clearWith OPTION
            # changed, and none of the five images lists `data`), so a data-only change -- a row
            # filter, an edited cell -- that makes this run bail out early would leave the
            # PREVIOUS cohort's state on the images, and .restoreFromState() would faithfully
            # redraw the previous cohort's curves beside the new error notice. That is the exact
            # failure the private-field reset above exists to prevent.
            private$.clearPlotStates()

            # Show instructions if needed
            if (is.null(self$options$outcome) || is.null(self$options$models) ||
                length(self$options$models) == 0) {

                instructions <- paste0(
                    "<html><head></head><body><div class='instructions'>",
                    "<p><b>", .("Decision Curve Analysis"), "</b></p>",
                    "<p>", .("Decision Curve Analysis evaluates the clinical utility of prediction models by calculating net benefit across different threshold probabilities."), "</p>",
                    "<p>", .("To get started:"), "</p>",
                    "<ol>",
                    "<li>", .("Select a binary Outcome Variable (the condition you want to predict)."), "</li>",
                    "<li>", .("Specify which level represents the positive outcome."), "</li>",
                    "<li>", .("Add one or more Prediction Variables/Models containing predicted probabilities from 0 to 1; raw scores, logits and linear predictors are not valid threshold probabilities."), "</li>",
                    "<li>", .("Configure the threshold range and other analysis options."), "</li>",
                    "</ol>",
                    "<p>", .("The analysis will show whether using your prediction model(s) provides more clinical benefit than treating all patients or treating no patients."), "</p>",
                    "</div></body></html>"
                )

                self$results$instructions$setVisible(TRUE)
                self$results$instructions$setContent(instructions)
                return()
            }

            # Hide instructions when analysis can proceed. The matching setVisible(TRUE)
            # lives in the guard above: without it the panel stayed hidden for the rest of
            # the session once one analysis had succeeded, so a user who then cleared the
            # outcome variable was left with a blank pane and no guidance.
            self$results$instructions$setVisible(FALSE)

            # No positive level chosen yet. allowNone stops jamovi pre-selecting the first level,
            # which is usually the non-event ("No", "0"), so this is the state every new analysis
            # starts in: unconfigured, not an error (review guide §25). List the levels and stop.
            if (private$.levelUnset(self$options$outcomePositive)) {
                lv <- table(self$data[[self$options$outcome]])
                private$.addNotice(
                    type = "INFO",
                    title = .("Choose the Positive Outcome Level"),
                    content = paste(
                        .fmt(.('Select the level of {var} that is the event the models predict, under "Positive Outcome".'),
                             var = self$options$outcome),
                        .fmt(.('Levels found: {levels}.'),
                             levels = paste(sprintf("%s (n = %d)", names(lv), as.integer(lv)), collapse = ", ")),
                        .('The positive level is never taken from the order of the levels.')
                    )
                )
                private$.renderNotices()
                return()
            }

            # Fix the RNG for every bootstrap in this run. Unseeded, the same data and the
            # same options gave a different confidence interval and a different p-value on
            # each run: across eight identical reruns at the default 1000 replications the
            # comparison p-value moved between 0.030 and 0.060 and the 95% CI crossed zero
            # in two of them. A clinician who reruns an analysis must get the same numbers.
            # withr::local_seed() restores the caller's RNG state when .run() exits, so neither an
            # R-API user's stream nor the next analysis in the shared jamovi process is disturbed.
            # This is the floor: each bootstrap consumer additionally re-seeds its own substream
            # via private$.bootSeed(), so one consumer cannot shift another's draws.
            withr::local_seed(private$.seedValue())

            # Check if required packages are available
            required_packages <- "dplyr"
            if (any(vapply(private$.plotImageNames(), function(nm)
                isTRUE(self$results[[nm]]$visible), logical(1))))
                required_packages <- c(required_packages, "ggplot2")
            if (self$options$calculateClinicalImpact || self$options$showClinicalImpactPlot)
                required_packages <- c(required_packages, "tidyr")
            missing_packages <- character(0)

            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                }
            }

            if (length(missing_packages) > 0) {
                private$.fatal(
                    .("Missing Packages"),
                    paste(
                        .fmt(.('Required packages missing: {pkgs}.'), pkgs = paste(missing_packages, collapse = ", ")),
                        .fmt(.('Install with install.packages(c({quoted})).'),
                             quoted = paste0('"', paste(missing_packages, collapse = '", "'), '"'))
                    )
                )
            }

            # Get data and variables
            data <- self$data
            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive
            model_vars <- self$options$models

            # Parse model names
            model_names <- private$.parseModelNames()
            custom_names_text <- self$options$modelNames
            if (!is.null(custom_names_text) && nzchar(trimws(custom_names_text))) {
                requested_names <- trimws(unlist(strsplit(custom_names_text, ",", fixed = TRUE)))
                if (length(requested_names) != length(model_vars) || any(!nzchar(requested_names))) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .("Model Names Replaced"),
                        content = paste(
                            .fmt(.('The custom model-name list must contain exactly {n} non-empty comma-separated names.'), n = length(model_vars)),
                            .('Source variable names are used instead.')
                        )
                    )
                }
            }

            if (anyDuplicated(model_names)) {
                duplicate_names <- unique(model_names[duplicated(model_names)])
                private$.fatal(
                    .("Duplicate Model Names"),
                    paste(
                        .('Each model needs a unique name.'),
                        .fmt(.('Rename the duplicated label(s): {names}.'), names = paste(duplicate_names, collapse = ", "))
                    )
                )
            }

            reserved_names <- c("treat all", "treat none")
            if (any(tolower(model_names) %in% reserved_names)) {
                private$.fatal(
                    .("Reserved Model Name"),
                    paste(
                        .("Model names cannot be 'Treat All' or 'Treat None' because those labels identify the reference strategies."),
                        .("Choose distinct model names.")
                    )
                )
            }

            # Clinical decision rule variable (optional)
            rule_var <- NULL
            rule_positive <- NULL
            rule_label <- NULL
            if (isTRUE(self$options$clinicalDecisionRule) && !is.null(private$.ruleStrategyLabel())) {
                rule_var <- self$options$decisionRuleVar
                rule_positive <- self$options$decisionRulePositive
            } else if (isTRUE(self$options$clinicalDecisionRule)) {
                # Ticked, but the variable or its positive level is still empty: a half-configured
                # rule, not an error (review guide §25), so the models still run. Say so, or the
                # green "Analysis Complete" reads as if the rule were in the comparison.
                private$.addNotice(
                    type = "INFO",
                    title = .("Clinical Decision Rule Not Included Yet"),
                    content = paste(
                        if (is.null(self$options$decisionRuleVar))
                            .('"Clinical decision rule" is enabled but no rule variable has been selected: drop a binary variable into "Clinical decision rule (binary)".')
                        else
                            .('"Clinical decision rule" is enabled but its positive level has not been chosen: select the level that means the rule recommends intervention.'),
                        .('Nothing about the clinical rule is shown until then.')
                    )
                )
            }

            # Get complete cases
            complete_vars <- c(outcome_var, model_vars)
            if (!is.null(rule_var)) {
                complete_vars <- c(complete_vars, rule_var)
            }
            complete_cases <- complete.cases(data[complete_vars])

            if (sum(complete_cases) < 10) {
                private$.fatal(
                    .("Insufficient Cases"),
                    paste(
                        .fmt(.('Insufficient complete cases for analysis ({n} cases available; this implementation requires at least 10 to run).'), n = sum(complete_cases)),
                        .('This computational safeguard does not imply that 10 observations are statistically adequate.'),
                        .('Net-benefit precision depends on event counts, prevalence and thresholds; use an adequately sized validation sample.')
                    )
                )
            }

            # Filter data to complete cases
            analysis_data <- data[complete_cases, ]
            outcomes <- analysis_data[[outcome_var]]
            private$.analysisOutcomes <- outcomes

            # Event count, not just row count: the guards key on n and on prevalence, so a
            # cohort of 110 patients with 6 events (5.5% prevalence) cleared both and was
            # reported as a clean success -- yet every threshold's net benefit rests on those
            # six true positives. Deliberately placed AFTER the insufficient-cases guard above:
            # raised before it, this fired on runs that were being refused anyway and told a
            # user whose analysis had already stopped that it also had too few events.
            # (Called after the positive level is validated below, so a run refused for a
            # non-binary outcome is not also told about its class counts.)

            n_excluded <- sum(!complete_cases)
            if (n_excluded > 0) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Complete-Case Analysis"),
                    content = .fmt(
                        .('{excluded} of {total} rows ({pct}%) were excluded because the outcome, at least one model prediction, or the enabled clinical rule was missing; every strategy is evaluated on the same {kept} retained rows.'),
                        excluded = n_excluded,
                        total = length(complete_cases),
                        pct = sprintf("%.1f", 100 * n_excluded / length(complete_cases)),
                        kept = sum(complete_cases)
                    )
                )
            }

            # Clinical Profile Notices: Sample Size Adequacy
            n_total <- sum(complete_cases)
            if (n_total < 100) {
                notice_type <- if (n_total < 50) "STRONG_WARNING" else "WARNING"
                size_txt <- if (n_total < 50)
                    .fmt(.('Very small sample size (n = {n}).'), n = n_total)
                else
                    .fmt(.('Small sample size (n = {n}).'), n = n_total)

                private$.addNotice(
                    type = notice_type,
                    title = .("Small Sample Size"),
                    content = paste(size_txt,
                        .('Net-benefit precision depends on the number of events, prevalence and the selected thresholds; inspect the uncertainty bands and validate the predictions on independent or properly resampled data.'))
                )
            }

            # Check outcome is binary
            unique_outcomes <- unique(outcomes)
            if (length(unique_outcomes) != 2) {
                private$.fatal(
                    .("Outcome Not Binary"),
                    paste(
                        .('The outcome variable must be binary (exactly 2 levels).'),
                        .fmt(.('The current outcome "{var}" has {n} levels: {levels}.'),
                             var = outcome_var, n = length(unique_outcomes),
                             levels = paste(unique_outcomes, collapse = ", ")),
                        .('Decision curve analysis requires a binary outcome (diseased vs healthy, event vs no event); recode to binary or select a different outcome variable.')
                    )
                )
            }

            # A chosen level absent from the analysed rows (an unset one stopped above).
            if (!(outcome_positive %in% unique_outcomes)) {
                private$.fatal(
                    .("Positive Outcome Level Required"),
                    paste(
                        .('Select exactly one positive outcome level that is present in the analysis data.'),
                        .fmt(.('Available levels: {levels}.'), levels = paste(unique_outcomes, collapse = ", ")),
                        .('The analysis was stopped to avoid reversing event and non-event status.')
                    )
                )
            }

            # Persist the validated level so every downstream table and plot uses the same
            # definition of the event.
            private$.outcomePositive <- outcome_positive
            private$.warnOnLowEventCount(data, outcome_var, complete_cases)
            private$.warnOnLowNonEventCount(outcomes, outcome_positive)

            # Clinical Profile Notices: Extreme Prevalence
            n_diseased <- sum(outcomes == outcome_positive)
            prevalence <- n_diseased / n_total

            if (prevalence < 0.05 || prevalence > 0.95) {
                private$.addNotice(
                    type = "STRONG_WARNING",
                    title = .("Extreme Prevalence"),
                    content = paste(
                        .fmt(.('Extreme outcome prevalence: {pct}% ({events}/{n} cases).'),
                             pct = sprintf("%.1f", prevalence * 100), events = n_diseased, n = n_total),
                        .('Decision curves may be less interpretable with very low or very high event rates, and net benefit is sensitive to prevalence extremes.'),
                        .('Consider whether the sample represents the target clinical population; results may not generalize to populations with different event rates.')
                    )
                )
            }

            # An exact 1:k split (k = 1..4) is what a case-control design with k controls per case
            # produces, and its 50%-20% prevalence passes the extreme-prevalence check above (the
            # 2026-10-21 review's 1:3 sample read NB 0.101 at 20% against a cohort 0.037 with no
            # notice, DC-21-02). On a case-control sample net benefit and
            # treat-all are not the source population's: in the independent review a calibrated
            # score read NB 0.260 at 20% against a cohort value of 0.037, and treat-all at 30% read
            # +0.286 against -0.224 (IR-decisioncurve-01). Worded conditionally: a cohort can
            # also split evenly by chance - in 25% of 10-patient cohorts at a true 50% prevalence
            # (Codex review) - so it waits for 20 per class, below which the small-sample and
            # few-events notices already speak.
            n_smaller <- min(n_diseased, n_total - n_diseased)
            design_ratio <- (n_total - n_smaller) / n_smaller
            if (n_smaller >= 20 && design_ratio %in% 1:4) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Outcome Split Matches a Case-Control Ratio"),
                    content = paste(
                        # with:without order, so 60 events and 20 non-events read 3:1, not 1:3
                        .fmt(.('The analysed cases contain {events} patients with the outcome and {nonevents} without it, an exact {ratio} ratio of patients with the outcome to patients without it, as a case-control design that samples a fixed number of controls per case (or of cases per control) would produce.'),
                             events = n_diseased, nonevents = n_total - n_diseased,
                             ratio = if (n_diseased <= n_total - n_diseased) paste0("1:", design_ratio) else paste0(design_ratio, ":1")),
                        .fmt(.('If cases and controls were sampled separately, the {pct}% prevalence is a design choice rather than the outcome rate of the target population, and the net benefit, the treat-all line and every projection here do not apply to that population.'),
                             pct = sprintf("%.1f", prevalence * 100)),
                        .('Decision curves need a cohort or consecutive series from the target population.')
                    )
                )
            }

            # Validate clinical decision rule variable if provided
            rule_data <- NULL
            if (!is.null(rule_var)) {
                rule_data <- analysis_data[[rule_var]]
                rule_levels <- unique(rule_data)
                # The values a rule can take: its two declared levels when it has them, so a rule
                # that recommends the same action for every analysed patient - after filtering,
                # say - is still a strategy (treat-all or treat-none) rather than a refused run.
                rule_domain <- if (is.factor(rule_data) && nlevels(rule_data) == 2)
                    levels(rule_data) else as.character(rule_levels)
                if (length(rule_levels) > 2 || length(rule_domain) != 2) {
                    private$.fatal(
                        .("Rule Not Binary"),
                        paste(
                            .('The clinical decision rule variable must be binary (exactly 2 levels).'),
                            .fmt(.('The current rule variable "{var}" has {n} levels: {levels}.'),
                                 var = rule_var, n = length(rule_levels), levels = paste(rule_levels, collapse = ", ")),
                            .('Select a binary rule variable or disable "Clinical decision rule".')
                        )
                    )
                }
                if (!(as.character(rule_positive) %in% rule_domain)) {
                    private$.fatal(
                        .("Rule Positive Level Required"),
                        paste(
                            .('Select exactly one positive rule level that is present in the analysis data.'),
                            .fmt(.('Available levels: {levels}.'), levels = paste(rule_domain, collapse = ", ")),
                            .('The analysis was stopped to avoid reversing intervention recommendations.')
                        )
                    )
                }
                if (length(rule_levels) == 1) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .("Clinical Rule Is Constant"),
                        content = if (as.character(rule_levels) == as.character(rule_positive))
                            .('In the analysed rows the clinical rule recommends intervention for every patient, so its curve is the treat-all line.')
                        else
                            .('In the analysed rows the clinical rule recommends intervention for no patient, so its curve is the treat-none line at zero.')
                    )
                }

                # Same resolution as .init() uses to scaffold the table columns, by
                # construction: one helper, two callers (see .ruleStrategyLabel).
                rule_label <- private$.ruleStrategyLabel()

                if (tolower(rule_label) %in% tolower(c(model_names, "Treat All", "Treat None"))) {
                    private$.fatal(
                        .("Duplicate Strategy Name"),
                        paste(
                            .fmt(.('The clinical decision rule label "{label}" duplicates a model or reference-strategy name.'), label = rule_label),
                            .('Choose a unique rule label.')
                        )
                    )
                }
            }

            # Generate threshold sequence
            thresholds <- private$.generateThresholds()

            # Initialize results storage
            dca_results <- list()
            plot_data <- data.frame()

            # Calculate decision curves for each model
            for (i in seq_along(model_vars)) {
                model_var <- model_vars[i]
                model_name <- model_names[i]
                predictions <- analysis_data[[model_var]]

                # The GUI restricts this box to numeric columns, but a programmatic caller
                # can still pass a factor or a character column. min() on a factor raises a
                # bare R error before any notice can be shown, so reject it explicitly.
                if (!is.numeric(predictions)) {
                    private$.fatal(
                        .fmt(.('Not a numeric column: {model}'), model = model_name),
                        paste(
                            .fmt(.('Model "{model}" is a {class} column.'), model = model_name,
                                 class = paste(class(predictions), collapse = "/")),
                            .('Decision curve analysis needs predicted probabilities between 0 and 1 as a numeric column; convert it before running the analysis, because a categorical column cannot express a predicted risk.')
                        )
                    )
                }

                # (An "entirely missing" check used to sit here. It could not fire: every row of
                # analysis_data is complete in every model column.)

                # Validate that predictions are probabilities between 0 and 1. This range
                # check cannot establish calibration; it only prevents raw score scales from
                # being interpreted as clinically meaningful threshold probabilities.
                pred_min <- min(predictions, na.rm = TRUE)
                pred_max <- max(predictions, na.rm = TRUE)

                if (pred_min < 0 || pred_max > 1) {
                    private$.fatal(
                        .fmt(.('Invalid Probabilities: {model}'), model = model_name),
                        paste(
                            .fmt(.('Model "{model}" contains values outside 0 to 1 (min = {min}, max = {max}).'),
                                 model = model_name, min = sprintf("%.3f", pred_min), max = sprintf("%.3f", pred_max)),
                            .('Decision curve analysis requires predicted probabilities for the clinical outcome and prediction horizon being evaluated, not raw scores, logits or linear predictors.'),
                            .('Use model-based probabilities and assess their calibration separately in the target population; min-max scaling does not create calibrated risks.')
                        )
                    )
                }

                # Warn if probabilities are suspiciously concentrated. The margin keeps a range of
                # exactly 5% (0.35 - 0.30 = 0.04999999999999999) on the documented side.
                if (pred_max - pred_min < 0.05 - 1e-9) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = .fmt(.('Narrow Probability Range: {model}'), model = model_name),
                        content = paste(
                            .fmt(.('Model "{model}" has a very narrow probability range ({min} to {max}).'),
                                 model = model_name, min = sprintf("%.3f", pred_min), max = sprintf("%.3f", pred_max)),
                            .('Decision curve analysis may not be informative with such limited variation (range below 5%); check model calibration and discrimination, because models with poor discrimination may not show clinical utility.')
                        )
                    )
                }

                # Optimized threshold calculations - vectorize when possible
                net_benefits <- private$.calculateNetBenefitsVectorized(
                    predictions, outcomes, thresholds, outcome_positive
                )
                
                # `detailed_results` removed. It repeated the whole vectorized sweep above
                # one threshold at a time -- a second full O(n_thresholds x n) pass per model
                # -- and nothing ever read it: it was only carried into .dcaResults, and from
                # there into four of the five images' state, where it was the largest single
                # item written to the .omv (~0.6 MB of the 1.2 MB state on a 5,000-row,
                # 4-model, 99-threshold analysis). Tables that need a value at one exact
                # threshold call .calculateModelAtThreshold(), which computes it on demand.
                dca_results[[model_name]] <- list(
                    net_benefits = net_benefits,
                    thresholds = thresholds,
                    predictions = predictions
                )

                # Add to plot data
                model_plot_data <- data.frame(
                    threshold = thresholds,
                    net_benefit = net_benefits,
                    model = model_name,
                    stringsAsFactors = FALSE
                )

                # Add confidence intervals if requested
                if (private$.ciDrawn()) {
                    # Own substream per model (see .bootSeed): this model's interval does not
                    # depend on how many models precede it, nor the p-values below on whether
                    # this box is ticked at all.
                    ci_results <- withr::with_seed(
                        private$.bootSeed(1L, i),
                        private$.calculateBootstrapCI(
                            predictions, outcomes, thresholds, outcome_positive,
                            self$options$bootReps
                        )
                    )
                    model_plot_data$ci_lower <- ci_results$lower
                    model_plot_data$ci_upper <- ci_results$upper
                    model_plot_data$sim_lower <- ci_results$sim_lower
                    model_plot_data$sim_upper <- ci_results$sim_upper
                }

                plot_data <- rbind(plot_data, model_plot_data)
            }

            # Calculate net benefit for treat all strategy
            treat_all_nb <- numeric(length(thresholds))
            treat_none_nb <- numeric(length(thresholds))

            for (j in seq_along(thresholds)) {
                treat_all_nb[j] <- private$.calculateTreatAllNetBenefit(
                    outcomes, thresholds[j], outcome_positive
                )
                treat_none_nb[j] <- private$.calculateTreatNoneNetBenefit()
            }

            # Add reference strategies to plot data
            ref_data <- rbind(
                data.frame(
                    threshold = thresholds,
                    net_benefit = treat_all_nb,
                    model = "Treat All",
                    stringsAsFactors = FALSE
                ),
                data.frame(
                    threshold = thresholds,
                    net_benefit = treat_none_nb,
                    model = "Treat None",
                    stringsAsFactors = FALSE
                )
            )

            plot_data <- dplyr::bind_rows(plot_data, ref_data)
            
            # Add clinical decision rule if requested and provided
            if (self$options$clinicalDecisionRule && !is.null(rule_data)) {
                # Convert rule to numeric prediction (1 = intervene/recommend)
                rule_pred <- as.numeric(rule_data == rule_positive)

                # Net benefit across thresholds
                rule_net <- private$.calculateNetBenefitsVectorized(rule_pred, outcomes, thresholds, outcome_positive)

                # `detailed_results`/`is_rule` removed: both were write-only (see the model
                # loop above). `is_rule` in particular was present only on the rule entry and
                # absent -- not FALSE -- on every model, so any future reader of it would have
                # been wrong about every model.
                dca_results[[rule_label]] <- list(
                    net_benefits = rule_net,
                    thresholds = thresholds,
                    predictions = rule_pred
                )

                rule_plot_data <- data.frame(
                    threshold = thresholds,
                    net_benefit = rule_net,
                    model = rule_label,
                    stringsAsFactors = FALSE
                )

                # The rule is a strategy like any model and its curve carries the same
                # sampling uncertainty; it used to be the only curve drawn without a band.
                if (private$.ciDrawn()) {
                    rule_ci <- withr::with_seed(
                        private$.bootSeed(2L),
                        private$.calculateBootstrapCI(
                            rule_pred, outcomes, thresholds, outcome_positive,
                            self$options$bootReps
                        )
                    )
                    rule_plot_data$ci_lower <- rule_ci$lower
                    rule_plot_data$ci_upper <- rule_ci$upper
                    rule_plot_data$sim_lower <- rule_ci$sim_lower
                    rule_plot_data$sim_upper <- rule_ci$sim_upper
                }

                plot_data <- dplyr::bind_rows(plot_data, rule_plot_data)
            }

            # A column of risks for the OTHER level is a valid input that answers the wrong
            # question: scored against "none", a model with AUC 0.74 for the event read "Range of
            # Benefit: none" with no notice (IR-decisioncurve-03). Net benefit needs the
            # probability OF the selected level, so flag a strategy whose patients with that
            # level rank LOWER - as a possibility, never as a verdict on the coding.
            # Gate: the displayed 95% DeLong interval wholly below 0.5 (so the interval shown
            # always agrees with the alarm) AND a one-sided permutation test of AUC < 0.5 at
            # 2.5%, which holds its level at any n and under ties. A DeLong-only gate is
            # anti-conservative in sparse classes (11% false alarms at 2 events / 8 non-events by
            # exact enumeration, Codex review 2026-10-13); a point-estimate gate fires on about
            # half of uninformative models.
            is_event <- as.character(outcomes) == as.character(outcome_positive)
            for (k in seq_along(dca_results)) {
                nm <- names(dca_results)[k]
                pred <- dca_results[[nm]]$predictions
                a <- private$.aucDeLong(pred, is_event)
                if (is.null(a) || !is.finite(a$upper) || a$upper >= 0.5) next
                # Own substream (stream 4, one per strategy): see .bootSeed.
                p_less <- withr::with_seed(private$.bootSeed(4L, k),
                                           private$.aucLessPermutationP(pred, is_event))
                if (!is.finite(p_less) || p_less >= 0.025) next
                stats_txt <- list(level = outcome_positive, model = nm,
                                  auc = sprintf("%.2f", a$auc),
                                  lo = sprintf("%.2f", max(0, a$lower)),
                                  hi = sprintf("%.2f", a$upper),
                                  p = sprintf("%.4f", p_less),
                                  seed = private$.seedValue())
                # The clinical rule is a treat/do-not-treat recommendation, not a risk.
                first <- if (!is.null(rule_label) && identical(nm, rule_label))
                    do.call(.fmt, c(list(.('{model} recommended intervention less often for patients with "{level}" than for patients without it (AUC {auc}, 95% CI {lo} to {hi}; one-sided permutation p = {p}, seed {seed}).')), stats_txt))
                else
                    do.call(.fmt, c(list(.('Patients with "{level}" have lower values of {model} than patients without it (AUC {auc}, 95% CI {lo} to {hi}; one-sided permutation p = {p}, seed {seed}).')), stats_txt))
                private$.addNotice(
                    type = "STRONG_WARNING",
                    title = .fmt(.('Possible Inversion: {model}'), model = nm),
                    content = paste(
                        first,
                        .('Decision curve analysis needs the predicted probability of the selected positive level; if these values describe the other level, the curves answer a different question.'),
                        .('Check that "Positive Outcome" is the level the model predicts and that the column is not one minus the risk.')
                    )
                )
            }

            # Bootstrap intervals collapse where few patients are classified positive: in the
            # 2026-10-12 simulation (n = 80, ~10 events) 95% intervals covered 82% at 35% and 66%
            # at 40%, and a threshold no one reaches gives a zero-width interval at NB = 0. The
            # positive count only falls as the threshold rises, so the sparse thresholds are the
            # top of the range; name where they start (VAL-decisioncurve-06). 10 is a heuristic.
            if (private$.ciDrawn()) {
                for (nm in names(dca_results)) {
                    z <- dca_results[[nm]]
                    n_pos <- vapply(z$thresholds, function(t) sum(z$predictions >= t), numeric(1))
                    sparse <- which(n_pos < 10)
                    if (length(sparse) == 0) next
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.('Few Positive Classifications: {model}'), model = nm),
                        content = paste(
                            .fmt(.('From the {lo}% threshold upwards, fewer than 10 patients are classified positive by {model} ({k} of the {total} thresholds analysed).'),
                                 lo = sprintf("%.1f", min(z$thresholds[sparse]) * 100), model = nm,
                                 k = length(sparse), total = length(z$thresholds)),
                            .('Bootstrap intervals there under-cover and can collapse to a single value; a narrow interval at those thresholds does not mean precision.')
                        )
                    )
                }
            }

            # Store results for plotting
            private$.dcaResults <- dca_results
            private$.plotData <- plot_data
            private$.treatAllNB <- treat_all_nb
            private$.prevalence <- prevalence
            # The clinical impact plot is visible with either option on. Building its rates
            # here also parses "Thresholds for table" in .run(), where the parser's notices are
            # shown, so plot-only mode reports an unusable entry instead of silently drawing the
            # default thresholds (Codex review, 2026-10-13).
            if (self$options$calculateClinicalImpact || self$options$showClinicalImpactPlot)
                private$.impactData <- private$.buildImpactData(
                    dca_results, outcomes, outcome_positive, private$.parseSelectedThresholds())
            # ... and publish them as image state so the renderers survive a render-only
            # invocation (see .publishPlotStates).
            private$.publishPlotStates()

            # Create procedure notes
            li <- function(label, value) paste0("<p><strong>", label, ":</strong> ", value, "</p>")
            procedure_notes <- paste0(
                "<html><body>",
                "<h4>", .("Decision Curve Analysis Summary"), "</h4>",
                li(.("Outcome Variable"),
                   .fmt(.('{var} ({level} = positive)'),
                        var = private$.safeHtmlOutput(outcome_var), level = private$.safeHtmlOutput(outcome_positive))),
                li(.("Models Analyzed"), paste(private$.safeHtmlOutput(model_names), collapse = ", ")),
                li(.("Sample Size"),
                   .fmt(.('{kept} complete cases; {excluded} of {total} rows excluded for missing required values'),
                        kept = sum(complete_cases), excluded = n_excluded, total = length(complete_cases))),
                # The prevalence is not just reported: treat-all and every net benefit are built on
                # it. Say so beside the number, where a case-control 50% is most likely to be
                # noticed (IR-decisioncurve-01).
                li(.("Prevalence"), paste(
                   paste0(round(mean(outcomes == outcome_positive) * 100, 1), "%"),
                   .("Net benefit and the treat-all line use this sample prevalence, so they are valid only when the analysed rows have the outcome rate of the target population (a cohort or consecutive series)."),
                   .("In a case-control or otherwise enriched sample the curves do not describe the source population."))),
                li(.("Unit of Analysis"),
                   .("Each row is treated as an independent patient, so the data must have one row per patient; several specimens, blocks or lesions from one patient make the bootstrap intervals and p-values too narrow.")),
                li(.("Threshold Range"),
                   .fmt(.('{lo}% to {hi}%'), lo = round(min(thresholds) * 100, 1), hi = round(max(thresholds) * 100, 1))),
                li(.("Prediction Requirement"), paste(
                   .("Inputs must be probabilities for the clinical outcome and prediction horizon being evaluated."),
                   .("A value range of 0 to 1 does not establish calibration; calibration must be assessed separately in the target population."),
                   .("This binary analysis does not account for censoring."))),
                "</body></html>"
            )

            self$results$procedureNotes$setContent(procedure_notes)

            # .plotDCA draws the highlight only when min < max; say why it is missing otherwise.
            if (self$options$highlightRange && isTRUE(self$results$dcaPlot$visible) &&
                self$options$highlightMin >= self$options$highlightMax) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Highlight Range Not Drawn"),
                    content = paste(
                        .fmt(.('The highlight range minimum ({min}%) is not below its maximum ({max}%), so no range is highlighted on the decision curve plot.'),
                             min = sprintf("%.1f", self$options$highlightMin * 100),
                             max = sprintf("%.1f", self$options$highlightMax * 100)),
                        .('Set the range minimum below the range maximum under Plots.')
                    )
                )
            }

            # Warn once per run about "Thresholds for table" rows outside the analysed
            # range. This used to be called only from .populateResultsTable(), so with the
            # results table switched off the four other tables that consume the same list
            # (cost-benefit, decision consequences, resource utilization, clinical impact)
            # still showed out-of-range rows while the explanation written for exactly that
            # case was never displayed.
            if (self$options$showTable || self$options$costBenefitAnalysis ||
                self$options$showDecisionConsequences || self$options$resourceUtilization ||
                self$options$calculateClinicalImpact) {
                private$.warnOnThresholdsOutsideRange(private$.parseSelectedThresholds())
            }
            # (Plot-only mode, showClinicalImpactPlot alone, is parsed with the impact data
            # above; the out-of-range explanation talks about table rows, so it stays off there.)

            # Populate results table
            if (self$options$showTable) {
                private$.populateResultsTable()
            }

            # Populate range-of-benefit table
            if (self$options$showBenefitRange) {
                private$.populateBenefitRangeTable()
            }

            # Calculate clinical impact if requested
            if (self$options$calculateClinicalImpact) {
                private$.calculateClinicalImpactMetrics(outcomes, outcome_positive)
            }

            # Calculate weighted AUC if requested
            if (self$options$weightedAUC) {
                private$.populateWeightedAUCTable()
            }

            # Model comparison if requested. Strategies, not model columns: one model plus a
            # clinical rule is a comparable pair.
            # The .r.yaml gates visibility on the checkbox alone, so with a single model the
            # table is on screen but never populated. An empty jamovi table reads as "the
            # computation failed", so say what is actually missing.
            if (self$options$compareModels) {
                if (length(private$.dcaResults) > 1) {
                    private$.performModelComparison()
                } else {
                    private$.addNotice(
                        type = "INFO",
                        title = .("Model comparison needs two models"),
                        content = paste(
                            .('"Statistical comparison" is enabled but only one prediction model is selected, so the Exploratory Model Comparison table stays empty.'),
                            .('Add a second model, or a clinical decision rule, to compare.')
                        )
                    )
                }
            }
            
            # Enhanced Analysis Options
            
            # Cost-Benefit Analysis
            if (self$options$costBenefitAnalysis) {
                private$.populateCostBenefitTable()
            }
            
            # Decision Consequences
            if (self$options$showDecisionConsequences) {
                private$.populateDecisionConsequencesTable()
            }
            
            # Resource Utilization
            if (self$options$resourceUtilization) {
                private$.populateResourceUtilizationTable()
            }
            
            # Enhanced Model Comparison. Same visibility mismatch as compareModels above.
            if (self$options$multiModelComparison) {
                if (length(private$.dcaResults) > 1) {
                    private$.performEnhancedModelComparison()
                } else {
                    private$.addNotice(
                        type = "INFO",
                        title = .("Pairwise comparison needs two models"),
                        content = paste(
                            .('"Enhanced pairwise comparison" is enabled but only one prediction model is selected, so the Exploratory Pairwise Model Comparison table stays empty.'),
                            .('Add a second model, or a clinical decision rule, to compare.')
                        )
                    )
                }
            }

            # Clinical interpretation: opt-in, and never written to the hidden panel (the reset at
            # the top of .run() still empties it, so re-enabling it never shows a stale run).
            if (isTRUE(self$options$showInterpretation))
                private$.generateClinicalInterpretation()

            # Apparent net benefit is not validated net benefit. The analysis is handed a
            # column of predicted risks and has no way to know whether they were fitted on
            # these same rows; for the common case of a marker developed on this dataset
            # every curve here is optimistically biased in the model's favour.
            # WARNING, not STRONG_WARNING: it fires on every run whatever the data, and a
            # data-independent alarm at the top severity trains users to skip that band.
            # Worded as unknown provenance: the old title "apparent, not validated" was false
            # for a user who supplies external-validation predictions (Codex review).
            # The clinical rule's p-values in the comparison tables carry the same caveat: a rule
            # derived on these outcomes is resampled as frozen decisions.
            private$.addNotice(
                type = "WARNING",
                title = .("Net Benefit Is Not Optimism-Corrected"),
                content = paste(
                    .("This analysis cannot tell whether the predicted risks, or a clinical rule, were developed on these same rows, and it applies no optimism correction."),
                    .("If they were - including a cutpoint, a score, or a regression developed here - the net benefit is apparent and optimistically biased, and the model can appear to beat treat-all when it does not."),
                    .("Predictions from an external dataset or from cross-validation avoid this; report which was used.")
                )
            )

            # Success Completion Notice
            n_models <- length(model_names)
            n_cases <- sum(complete_cases)
            n_diseased_final <- sum(outcomes == outcome_positive)
            threshold_min <- min(thresholds) * 100
            threshold_max <- max(thresholds) * 100

            private$.addNotice(
                type = "INFO",
                title = .("Analysis Complete"),
                content = paste(
                    .fmt(.('Decision curve analysis completed: {models} model(s) evaluated using {n} complete cases.'),
                         models = n_models, n = n_cases),
                    .fmt(.('Outcome prevalence: {pct}% ({events}/{n}).'),
                         pct = sprintf("%.1f", prevalence * 100), events = n_diseased_final, n = n_cases),
                    .fmt(.('Threshold range: {lo}% to {hi}%.'),
                         lo = sprintf("%.1f", threshold_min), hi = sprintf("%.1f", threshold_max)),
                    .('Review the decision curves and the range of benefit below.')
                )
            )

            # Render all collected notices as HTML
            private$.renderNotices()
        },

        .populateResultsTable = function() {
            selected_thresholds <- private$.parseSelectedThresholds()
            # .warnOnThresholdsOutsideRange() moved to .run(); see the call there.
            results_table <- self$results$resultsTable

            # Add columns for each model dynamically
            model_names <- names(private$.dcaResults)
            model_columns <- private$.modelColumnNames(model_names)
            row_keys <- private$.tableRowKeys("resultsTable", length(model_names),
                                              length(selected_thresholds))
            private$.syncRows(results_table, row_keys)

            # .init() has normally created these already; only add what is genuinely missing
            # (a model name that changed shape between init and run).
            existing_cols <- vapply(results_table$columns, function(c) c$name, character(1))
            for (i in seq_along(model_names)) {
                model_name <- model_names[i]
                if (unname(model_columns[i]) %in% existing_cols) next
                results_table$addColumn(
                    name = unname(model_columns[i]),
                    title = model_name,
                    type = "number",
                    format = "zto"
                )
            }

            # Populate table
            for (i in seq_along(selected_thresholds)) {
                thresh <- selected_thresholds[i]

                # Create row values
                row_values <- list(
                    threshold = thresh,
                    treat_all = private$.calculateTreatAllNetBenefit(
                        private$.analysisOutcomes, thresh, private$.positiveLevel()
                    ),
                    treat_none = 0
                )
                
                # Add model values
                for (j in seq_along(model_names)) {
                    model_name <- model_names[j]
                    exact_result <- private$.calculateModelAtThreshold(model_name, thresh)
                    col_name <- unname(model_columns[j])
                    row_values[[col_name]] <- if (is.null(exact_result)) {
                        NA_real_
                    } else {
                        exact_result$net_benefit
                    }
                }
                
                results_table$setRow(rowKey = row_keys[i], values = row_values)
            }

            results_table$setNote(
                "thresholds",
                .("Each row is calculated at the exact displayed threshold; selected thresholds do not need to coincide with the plotting grid.")
            )
        },

        .populateCostBenefitTable = function() {
            table <- self$results$costBenefitTable
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            # Rows were built by .init() (see .syncRows); appending them here used to grow a
            # three-model table to six rows, then nine, on every run cycle.
            private$.syncRows(table, private$.tableRowKeys(
                "costBenefitTable", length(model_names), length(selected_thresholds)))
            
            # Costs and benefits
            test_cost <- self$options$testCost
            treat_cost <- self$options$treatmentCost
            benefit_tp <- self$options$benefitCorrectTreatment
            harm_fp <- self$options$harmFalseTreatment
            outcomes <- private$.analysisOutcomes
            pop_size <- self$options$populationSize
            n_analysis <- length(outcomes)
            scale_factor <- if (!is.null(pop_size) && !is.na(pop_size) && pop_size > 0) pop_size / n_analysis else 1
            
            # Calculate for each model at each threshold
            for (model_index in seq_along(model_names)) {
                model_name <- model_names[model_index]
                for (i in seq_along(selected_thresholds)) {
                    thresh <- selected_thresholds[i]
                    res <- private$.calculateModelAtThreshold(model_name, thresh)
                    if (is.null(res)) next
                    
                    tp_scaled <- res$tp * scale_factor
                    fp_scaled <- res$fp * scale_factor
                    tn_scaled <- res$tn * scale_factor
                    fn_scaled <- res$fn * scale_factor
                    n_scaled <- tp_scaled + fp_scaled + tn_scaled + fn_scaled
                    
                    # Exploratory monetary payoff accounting. All four user inputs must use
                    # the same monetary unit. Everyone is assumed tested once; treat-all is
                    # assumed to treat everyone without a test.
                    total_cost <- (n_scaled * test_cost) + ((tp_scaled + fp_scaled) * treat_cost)
                    total_benefit <- (tp_scaled * benefit_tp) - (fp_scaled * harm_fp)
                    net_payoff <- total_benefit - total_cost

                    # Treat-all comparator under the same assigned payoffs.
                    prevalence <- res$prevalence
                    tp_all <- prevalence * n_scaled
                    fp_all <- (1 - prevalence) * n_scaled
                    cost_all <- n_scaled * treat_cost
                    benefit_all <- (tp_all * benefit_tp) - (fp_all * harm_fp)
                    payoff_all <- benefit_all - cost_all

                    inc_cost <- total_cost - cost_all
                    inc_benefit <- total_benefit - benefit_all

                    table$setRow(rowKey = paste0("model_", model_index, "_", i), values = list(
                        model = model_name,
                        threshold = thresh,
                        total_cost = total_cost,
                        total_benefit = total_benefit,
                        net_monetary_benefit = net_payoff,
                        incremental_cost = inc_cost,
                        incremental_benefit = inc_benefit,
                        incremental_net_payoff = net_payoff - payoff_all
                    ))
                }
            }

            table$setNote(
                "scope",
                paste(
                    .fmt(.('Exploratory monetary payoff projection for a population of {n}.'),
                         n = base::format(pop_size, scientific = FALSE, trim = TRUE)),
                    .("All inputs must use the same currency and the calculation assumes one test per person, treatment after a positive model decision, and treat-all without testing."),
                    .("It is not an ICER, QALY, cost-effectiveness or net-monetary-benefit analysis and does not model time horizon, discounting, false-negative consequences or uncertainty in costs and values."),
                    .("Net payoff versus treat-all is the difference between assigned monetary benefits and direct costs under these assumptions.")
                )
            )
        },

        .populateDecisionConsequencesTable = function() {
            table <- self$results$decisionConsequencesTable
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            # Rows were built by .init() (see .syncRows); appending them here used to grow a
            # three-model table to six rows, then nine, on every run cycle.
            private$.syncRows(table, private$.tableRowKeys(
                "decisionConsequencesTable", length(model_names), length(selected_thresholds)))
            
            for (model_index in seq_along(model_names)) {
                model_name <- model_names[model_index]
                for (i in seq_along(selected_thresholds)) {
                    thresh <- selected_thresholds[i]
                    res <- private$.calculateModelAtThreshold(model_name, thresh)
                    if (is.null(res)) next
                    
                    # Calculate PPV/NPV
                    ppv <- if ((res$tp + res$fp) > 0) res$tp / (res$tp + res$fp) else NA
                    npv <- if ((res$tn + res$fn) > 0) res$tn / (res$tn + res$fn) else NA
                    
                    key <- paste0("model_", model_index, "_", i)
                    table$setRow(rowKey = key, values = list(
                        model = model_name,
                        threshold = thresh,
                        true_positive = res$tp,
                        false_positive = res$fp,
                        true_negative = res$tn,
                        false_negative = res$fn,
                        sensitivity = res$sensitivity,
                        specificity = res$specificity,
                        ppv = ppv,
                        npv = npv
                    ))
                    private$.footnoteNA(table, key, list(ppv = ppv),
                        .("No patient is classified positive at this threshold, so PPV is undefined."))
                    private$.footnoteNA(table, key, list(npv = npv),
                        .("No patient is classified negative at this threshold, so NPV is undefined."))
                }
            }
        },

        .populateResourceUtilizationTable = function() {
            table <- self$results$resourceUtilizationTable
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            # Rows were built by .init() (see .syncRows); appending them here used to grow a
            # three-model table to six rows, then nine, on every run cycle.
            private$.syncRows(table, private$.tableRowKeys(
                "resourceUtilizationTable", length(model_names), length(selected_thresholds)))
            pop_size <- self$options$populationSize
            
            for (model_index in seq_along(model_names)) {
                model_name <- model_names[model_index]
                for (i in seq_along(selected_thresholds)) {
                    thresh <- selected_thresholds[i]
                    res <- private$.calculateModelAtThreshold(model_name, thresh)
                    if (is.null(res)) next
                    
                    n_total <- res$tp + res$fp + res$tn + res$fn
                    scale_factor <- pop_size / n_total

                    treatments_per_1000 <- (res$tp + res$fp) * scale_factor
                    unnecessary_treatments <- res$fp * scale_factor
                    missed_cases <- res$fn * scale_factor
                    
                    # Reduction vs Treat All
                    # Treat All: All treated
                    treatments_all <- n_total * scale_factor
                    reduction <- (treatments_all - treatments_per_1000) / treatments_all
                    
                    table$setRow(rowKey = paste0("model_", model_index, "_", i), values = list(
                        model = model_name,
                        threshold = thresh,
                        treatments_per_1000 = treatments_per_1000,
                        unnecessary_treatments = unnecessary_treatments,
                        missed_cases = missed_cases,
                        reduction_vs_treat_all = reduction
                    ))
                }
            }

            table$setNote(
                "population",
                .fmt(.('Counts are projections to the selected population size of {n}, assuming every person is tested once; they are not observed patient counts.'),
                     n = base::format(pop_size, scientific = FALSE, trim = TRUE))
            )
        },
        
        .performEnhancedModelComparison = function() {
            table <- self$results$modelComparisonEnhanced
            model_names <- names(private$.dcaResults)
            outcomes <- private$.analysisOutcomes
            thresholds <- private$.dcaResults[[1]]$thresholds

            if (length(model_names) < 2) return()
            private$.syncRows(table, private$.tableRowKeys(
                "modelComparisonEnhanced", length(model_names), length(thresholds)))
            
            # Pairwise comparisons. Rows are collected first so the family of pairwise
            # tests can be Holm-adjusted before any "Significant Difference" verdict is
            # printed - this table used to declare significance from an unadjusted p while
            # the comparisonTable beside it adjusted the same family.
            # combn() enumerates the pairs in the same order as the nested loops in
            # .performModelComparison(), so pair number p means the same pair in both tables
            # and both can seed the same substream for it (see .bootSeed).
            pairs <- combn(model_names, 2, simplify = FALSE)
            rows <- list()

            for (p in seq_along(pairs)) {
                pair <- pairs[[p]]
                m1 <- pair[1]
                m2 <- pair[2]

                # Each strategy's per-patient values on the analysis cohort: a model's
                # complete-case column, or the clinical rule's 0/1 recommendation. The rule
                # used to be skipped as having "no values to resample", but it is resampled
                # exactly like a 0/1 predictor column (same pair index, same substream).
                pred1 <- private$.dcaResults[[m1]]$predictions
                pred2 <- private$.dcaResults[[m2]]$predictions

                nb1 <- private$.dcaResults[[m1]]$net_benefits
                nb2 <- private$.dcaResults[[m2]]$net_benefits

                diff <- nb1 - nb2
                mean_diff <- mean(diff, na.rm = TRUE)
                median_diff <- median(diff, na.rm = TRUE)

                private$.checkpoint()

                # Full bootReps, like the comparisonTable beside it. This table used to cap
                # silently at 1000, so the two tables on the same screen tested the same
                # pairs with different numbers of resamples. Whichever table runs first pays
                # for the resampling; the other reads the memo (see .bootstrapComparisonCached).
                res_boot <- private$.bootstrapComparisonCached(
                    m1, m2, p, pred1, pred2, outcomes,
                    thresholds, self$options$bootReps
                )

                rows[[length(rows) + 1]] <- list(
                    key = paste0("pair_", length(rows) + 1),
                    model1 = m1,
                    model2 = m2,
                    nb_difference_mean = mean_diff,
                    nb_difference_median = median_diff,
                    p_value = res_boot$nb$p_value
                )
            }

            raw_p <- vapply(rows, function(r) as.numeric(r$p_value %||% NA_real_), numeric(1))
            adj_p <- stats::p.adjust(raw_p, method = "holm")

            # No "Conclusion" column. It printed "Exploratory adjusted p < 0.05" as a verdict on
            # each pair, beside the note that tells the reader not to decide by these p-values
            # (Vickers et al. 2023), and it said nothing the p column does not (/review-function
            # 2026-10-13). An untestable pair now shows a blank p with a footnote.
            for (k in seq_along(rows)) {
                r <- rows[[k]]
                values <- list(
                    model1 = r$model1,
                    model2 = r$model2,
                    nb_difference_mean = r$nb_difference_mean,
                    nb_difference_median = r$nb_difference_median,
                    p_value = adj_p[k]
                )
                table$setRow(rowKey = r$key, values = values)
                private$.footnoteNA(table, r$key, values["p_value"],
                    .("Fewer than 50 bootstrap resamples gave a usable difference, so no p-value is reported."))
            }

            table$setNote(
                "method",
                paste(
                    .fmt(.('Exploratory mean difference in net benefit across the selected threshold range, with an approximate case-resampling bootstrap p-value from {reps} resamples, Holm-adjusted across all {k} pairwise comparisons (seed {seed}).'),
                         reps = self$options$bootReps,
                         k = length(rows),
                         seed = private$.seedValue()),
                    .("Every evaluated threshold, including both ends of the range, gets equal weight, so this differs slightly from the Difference in Average Net Benefit in the Exploratory Model Comparison table, which is the area under the curve divided by the range width."),
                    .("This is not a confirmatory test.")
                )
            )
            table$setNote("caution", private$.netBenefitInferenceCaution())
        },

        # The paper cited for net-benefit inference argues AGAINST deciding by it: "Application
        # of inference for net benefit would generally be harmful" (Vickers, Van Calster, Wynants
        # & Steyerberg 2023, Diagn Progn Res 7:11, abstract). Both comparison tables show p-values,
        # so both carry this sentence; one msgid for the two (IR-decisioncurve-06).
        .netBenefitInferenceCaution = function() {
            .("Vickers et al. (2023) caution that significance tests and confidence intervals for net benefit are of questionable value for a treatment decision: the strategy with the highest net benefit at the prespecified threshold is preferred whatever the p-value, so read these p-values only as a description of sampling variability.")
        },

        .populateBenefitRangeTable = function() {
            range_table <- self$results$benefitRangeTable
            model_names <- names(private$.dcaResults)
            row_keys <- private$.tableRowKeys("benefitRangeTable", length(model_names), 0)
            private$.syncRows(range_table, row_keys)
            has_gap <- character(0)
            none_beneficial <- character(0)

            for (i in seq_along(model_names)) {
                model_name <- model_names[i]
                model_results <- private$.dcaResults[[model_name]]

                info <- private$.findBenefitRange(
                    model_results$net_benefits,
                    model_results$thresholds,
                    private$.treatAllNB
                )

                if (is.na(info$range_start)) {
                    none_beneficial <- c(none_beneficial, model_name)
                } else if (isFALSE(info$contiguous)) {
                    has_gap <- c(has_gap, model_name)
                }

                row_values <- list(
                    model = model_name,
                    range_start = info$range_start,
                    range_end = info$range_end,
                    range_width = info$width
                )
                range_table$setRow(rowKey = row_keys[i], values = row_values)
                # No footnote: the "none" table note below names these models and says why.
                private$.footnoteNA(range_table, row_keys[i], row_values)
            }


            range_table$setNote(
                "definition",
                paste(
                    .("Observed range of threshold probabilities over which the model's apparent (in-sample) net benefit exceeds both treat-all and treat-none. It carries no allowance for sampling variation: near the outcome prevalence even a predictor unrelated to the outcome often shows a short range by chance, so a narrow range or a small margin is not evidence of benefit."),
                    private$.rangeUncertaintyPointer(),
                    .("Threshold probability is set by clinical judgement about the relative cost of a missed case versus an unnecessary treatment; it is not estimated from the data, so there is no optimal value to report.")
                )
            )

            # .rocLabel(): setNote() re-translates the composed note and cuts it at " [", so a
            # model named "Model [v2]" left the note reading just "Model" (security audit
            # 2026-10-14); it also breaks a "<" that the note renderer would read as a tag.
            if (length(none_beneficial) > 0) {
                range_table$setNote(
                    "none",
                    .fmt(.('{models} never exceeds both reference strategies anywhere in the threshold range examined, so no range is shown.'),
                         models = .rocLabel(paste(none_beneficial, collapse = ", ")))
                )
            }

            if (length(has_gap) > 0) {
                range_table$setNote(
                    "gap",
                    paste(
                        .fmt(.('{models} is superior over more than one separate stretch of thresholds.'),
                             models = .rocLabel(paste(has_gap, collapse = ", "))),
                        .("The start and end shown span a gap where the model is not superior - read the curve rather than the endpoints.")
                    )
                )
            }
        },

        .calculateClinicalImpactMetrics = function(outcomes, outcome_positive) {
            clinical_impact_table <- self$results$clinicalImpactTable
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            private$.syncRows(clinical_impact_table, private$.tableRowKeys(
                "clinicalImpactTable", length(model_names), length(selected_thresholds)))
            pop_size <- self$options$populationSize
            per_100_to_population <- pop_size / 100

            # Calculate for each model at each selected threshold
            for (model_index in seq_along(model_names)) {
                model_name <- model_names[model_index]
                for (i in seq_along(selected_thresholds)) {
                    thresh <- selected_thresholds[i]
                    row_key <- paste0("model_", model_index, "_", i)
                    detailed_result <- private$.calculateModelAtThreshold(model_name, thresh)
                    if (is.null(detailed_result)) next

                    treat_all_nb <- private$.calculateTreatAllNetBenefit(
                        outcomes, thresh, outcome_positive
                    )
                    interventions_avoided_per_100 <- private$.calculateNetInterventionsAvoided(
                        detailed_result$net_benefit,
                        treat_all_nb,
                        thresh,
                        population = 100
                    )

                    # Reciprocal of the observed true-positive yield among all screened.
                    nns <- if (detailed_result$true_positives_per_100 > 0)
                        100 / detailed_result$true_positives_per_100 else NA

                    clinical_impact_table$setRow(rowKey = row_key, values = list(
                        model = model_name,
                        threshold = thresh,
                        interventions_per_100 = detailed_result$interventions_per_100 * per_100_to_population,
                        true_positives_per_100 = detailed_result$true_positives_per_100 * per_100_to_population,
                        false_positives_per_100 = detailed_result$false_positives_per_100 * per_100_to_population,
                        interventions_avoided = interventions_avoided_per_100 * per_100_to_population,
                        number_needed_to_screen = nns
                    ))
                    private$.footnoteNA(clinical_impact_table, row_key,
                        list(number_needed_to_screen = nns),
                        .("There is no true positive at this threshold, so patients screened per true positive is undefined."))
                }
            }

            clinical_impact_table$setNote(
                "population",
                paste(
                    .fmt(.('Counts are projected to a population of {n} from the complete-case cohort.'),
                         n = base::format(pop_size, scientific = FALSE, trim = TRUE)),
                    .("Net interventions avoided is derived from the net-benefit difference versus treat-all after accounting for threshold odds."),
                    .("Patients screened per true positive is the reciprocal of the observed true-positive yield, not a causal number-needed-to-screen effect measure.")
                )
            )
        },

        .populateWeightedAUCTable = function() {
            weighted_auc_table <- self$results$weightedAUCTable
            model_names <- names(private$.dcaResults)
            thresholds <- private$.dcaResults[[1]]$thresholds
            row_keys <- private$.tableRowKeys("weightedAUCTable", length(model_names), 0)
            private$.syncRows(weighted_auc_table, row_keys)

            # Reuse the treat-all curve computed in .run() on the analysis cohort. This
            # method used to rebuild its own complete-case set from self$data using only
            # outcome + models, which excluded the clinical-decision-rule variable and so
            # produced a treat-all baseline drawn from MORE rows than the model curves it
            # was compared against. It also re-read the raw outcomePositive option rather
            # than the level the analysis actually resolved.
            treat_all_nb <- private$.treatAllNB
            if (is.null(treat_all_nb) || length(treat_all_nb) != length(thresholds)) {
                return()
            }

            # The comparator is the BEST DEFAULT STRATEGY at each threshold, not treat-all
            # alone. Treat-all net benefit goes sharply negative above the prevalence, so
            # measuring against it credited a model for beating a strategy no clinician
            # would ever adopt: on a 15.6%-prevalence cohort over the default 5-50% range
            # the gain came out at 0.248, which the table's own note reads as 24.8 extra
            # true positives per 100 patients when only 15.6 cases exist per 100. Against
            # pmax(treat-all, treat-none) the honest gain on that cohort is 0.030.
            reference_nb <- pmax(treat_all_nb, 0)
            reference_wauc <- private$.calculateWeightedAUC(reference_nb, thresholds)

            for (i in seq_along(model_names)) {
                model_name <- model_names[i]
                model_results <- private$.dcaResults[[model_name]]

                # Calculate weighted AUC
                wauc <- private$.calculateWeightedAUC(
                    model_results$net_benefits,
                    model_results$thresholds
                )
                wauc <- if (is.finite(wauc)) wauc else NA_real_

                # Gain over treating everyone, as a DIFFERENCE in weighted net benefit.
                # This used to be reported as the ratio (wauc - treat_all) / |treat_all|.
                # Treat-all net benefit crosses zero at a threshold equal to the outcome
                # prevalence, so whenever the threshold range brackets the prevalence the
                # denominator is near zero and the ratio explodes - percentages in the
                # hundreds were being displayed for ordinary differences.
                if (is.finite(wauc) && is.finite(reference_wauc)) {
                    benefit_gain <- wauc - reference_wauc
                } else {
                    benefit_gain <- NA_real_
                }

                row_vals <- list(
                    model = model_name,
                    weighted_auc = wauc,
                    auc_range = paste0(round(min(thresholds) * 100, 1), "% - ",
                                       round(max(thresholds) * 100, 1), "%"),
                    benefit_gain = benefit_gain
                )
                weighted_auc_table$setRow(rowKey = row_keys[i], values = row_vals)
                private$.footnoteNA(
                    weighted_auc_table, row_keys[i],
                    row_vals[c("weighted_auc", "benefit_gain")],
                    .("Weighted AUC requires at least two threshold points with positive range width.")
                )
            }

            weighted_auc_table$setNote(
                "wauc",
                paste(
                    .fmt(.('Average net benefit over the {lo}% to {hi}% threshold range: the area under the decision curve divided by the width of that range.'),
                         lo = sprintf("%.1f", min(thresholds) * 100), hi = sprintf("%.1f", max(thresholds) * 100)),
                    .("Every threshold in the range counts equally, so the value depends on the range you chose - report the range with it."),
                    .("Gain vs Best Default is the difference against the better of treating everyone and treating no one at each threshold, on the net-benefit scale: 0.01 means one extra true positive per 100 patients at no extra cost in unnecessary treatment."),
                    .("A gain at or below zero means a default strategy serves these patients at least as well as the model.")
                )
            )
        },

        .performModelComparison = function() {
            comparison_table <- self$results$comparisonTable
            model_names <- names(private$.dcaResults)
            outcomes <- private$.analysisOutcomes
            thresholds <- private$.dcaResults[[1]]$thresholds
            row_keys <- private$.tableRowKeys("comparisonTable", length(model_names), length(thresholds))
            private$.syncRows(comparison_table, row_keys)

            # Compare each pair of strategies (models and any clinical rule).
            # Rows are collected first so that the family of k(k-1)/2 pairwise tests can be
            # Holm-adjusted together before anything is displayed. Reporting only nominal
            # p-values here invites reading a five-model screen as if it were one test.
            rows <- list()

            # Counts every pair in the same order as the combn() in
            # .performEnhancedModelComparison(); it is the substream index, so both tables
            # resample a given pair identically (see .bootSeed).
            pair_no <- 0L

            for (i in 1:(length(model_names) - 1)) {
                for (j in (i + 1):length(model_names)) {
                    model1_name <- model_names[i]
                    model2_name <- model_names[j]
                    pair_no <- pair_no + 1L

                    # Per-patient values on the analysis cohort, the rule's 0/1 included
                    # (see .performEnhancedModelComparison).
                    pred1 <- private$.dcaResults[[model1_name]]$predictions
                    pred2 <- private$.dcaResults[[model2_name]]$predictions

                    # Calculate weighted AUC difference (observed)
                    wauc1 <- private$.calculateWeightedAUC(
                        private$.dcaResults[[model1_name]]$net_benefits,
                        thresholds
                    )
                    wauc2 <- private$.calculateWeightedAUC(
                        private$.dcaResults[[model2_name]]$net_benefits,
                        thresholds
                    )
                    wauc_diff <- if (is.finite(wauc1) && is.finite(wauc2)) wauc1 - wauc2 else NA_real_

                    private$.checkpoint()

                    res_boot <- private$.bootstrapComparisonCached(
                        model1_name, model2_name, pair_no, pred1, pred2, outcomes,
                        thresholds, self$options$bootReps
                    )

                    rows[[length(rows) + 1]] <- list(
                        comparison = .fmt(.('{a} vs {b}'), a = model1_name, b = model2_name),
                        weighted_auc_diff = wauc_diff,
                        ci_lower = res_boot$wauc$ci_lower,
                        ci_upper = res_boot$wauc$ci_upper,
                        p_value = res_boot$wauc$p_value
                    )
                }
            }

            if (length(rows) == 0) {
                return()
            }

            raw_p <- vapply(rows, function(r) {
                if (is.null(r$p_value)) NA_real_ else as.numeric(r$p_value)
            }, numeric(1))
            adj_p <- stats::p.adjust(raw_p, method = "holm")

            for (k in seq_along(rows)) {
                r <- rows[[k]]
                r$p_value_adj <- adj_p[k]
                comparison_table$setRow(rowKey = row_keys[k], values = r)
                private$.footnoteNA(
                    comparison_table, row_keys[k],
                    r[c("weighted_auc_diff", "ci_lower", "ci_upper", "p_value", "p_value_adj")],
                    .("Fewer than 50 bootstrap resamples gave a usable difference, so no interval or p-value is reported.")
                )
            }

            comparison_table$setNote(
                "boot",
                paste(
                    .fmt(.('Exploratory bootstrap comparison of average net benefit under each decision curve, {reps} resamples, seed {seed}.'),
                         reps = self$options$bootReps,
                         seed = private$.seedValue()),
                    .fmt(.('Intervals are {level}% percentile intervals and p-values are approximate; results depend on the selected threshold range and equal weighting of its thresholds.'),
                         level = sprintf("%.0f", self$options$ciLevel * 100)),
                    .("Re-running with the same seed reproduces these numbers exactly.")
                )
            )
            comparison_table$setNote("caution", private$.netBenefitInferenceCaution())

            if (length(rows) > 1) {
                comparison_table$setNote(
                    "holm",
                    paste(
                        .fmt(.('p (Holm) controls the family-wise error rate across all {k} pairwise comparisons.'), k = length(rows)),
                        .("Interpret the unadjusted p only for a single comparison specified before the data were seen.")
                    )
                )
            }
        },

        .generateClinicalInterpretation = function() {
            model_names <- names(private$.dcaResults)

            # Identify the highest descriptive average over the selected range. This is not a
            # global model ranking: curves can cross, and the average gives every threshold
            # equal weight regardless of how plausible it is clinically.
            # Every model within 1e-10 of the top average is named: a strict `>` kept only the
            # first of two tied models, and tied averages can still hide different curves, so
            # each tied model gets its own range sentence (VAL-decisioncurve-07).
            averages <- vapply(model_names, function(m) private$.calculateWeightedAUC(
                private$.dcaResults[[m]]$net_benefits,
                private$.dcaResults[[m]]$thresholds), numeric(1))
            best_models <- character(0)
            if (any(is.finite(averages))) {
                top <- max(averages[is.finite(averages)])
                best_models <- model_names[is.finite(averages) & abs(averages - top) <= 1e-10]
            }

            # Generate interpretation text
            interpretation <- paste0(
                "<html><body>",
                "<h4>", .("Clinical Interpretation"), "</h4>"
            )

            # Whole translatable units: "(tied)" and "none." used to be .() fragments glued on with
            # paste0(), which a translator cannot place in a Turkish word order.
            if (length(best_models) > 0) {
                best_txt <- paste(private$.safeHtmlOutput(best_models), collapse = ", ")
                if (length(best_models) > 1)
                    best_txt <- .fmt(.('{models} (tied)'), models = best_txt)
                interpretation <- paste0(
                    interpretation,
                    "<p><strong>", .("Highest Average Net Benefit Over the Selected Range:"), "</strong> ",
                    best_txt, " (",
                    .("This only ranks the models against each other; it does not mean the model is useful."), " ",
                    .("Whether it beats treating everyone or no one is the Range of Benefit below."), " ",
                    .("Inspect the curves at prespecified clinical thresholds because curves may cross."),
                    ")</p>"
                )
            }

            for (best_model in best_models) {
                # Range over which the leading model beats both reference strategies.
                best_results <- private$.dcaResults[[best_model]]
                range_info <- private$.findBenefitRange(
                    best_results$net_benefits,
                    best_results$thresholds,
                    private$.treatAllNB
                )
                range_label <- if (length(best_models) > 1)
                    .fmt(.('Range of Benefit ({model}):'), model = private$.safeHtmlOutput(best_model))
                else
                    .("Range of Benefit:")

                if (!is.na(range_info$range_start)) {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>", range_label, "</strong> ",
                        .fmt(.('{lo}% to {hi}% threshold probability - the observed range over which this model\'s apparent (in-sample) net benefit exceeds both treating everyone and treating no one. It carries no allowance for sampling variation, and a narrow range can arise by chance.'),
                             lo = round(range_info$range_start * 100, 1), hi = round(range_info$range_end * 100, 1)),
                        " ", private$.rangeUncertaintyPointer(),
                        if (isFALSE(range_info$contiguous))
                            paste0(" ", .("This range contains a gap where the model is not superior; read the curve."))
                        else "",
                        "</p>"
                    )
                } else {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>", range_label, "</strong> ",
                        .("None: across every threshold examined, treating everyone or treating no one does at least as well as this model."),
                        "</p>"
                    )
                }
            }

            interpretation <- paste0(
                interpretation,
                "<p><strong>", .("Interpretation Guidelines:"), "</strong></p>",
                "<ul>",
                "<li>", .("A model is useful only where its curve sits above BOTH reference lines."), "</li>",
                "<li>", .("Decide the threshold range from clinical judgement first, then read the curves there - not the other way round."), "</li>",
                "<li>", .("Net benefit is on the scale of true positives per patient; multiply by 100 to read it as true positives per 100 patients, at no additional cost in unnecessary treatment."), "</li>",
                "<li>", .("Do not select a model solely from its average net benefit: curve crossings and the plausibility of each threshold determine the clinically relevant comparison."), "</li>",
                "</ul>",
                private$.generateMethodologicalFootnotes(),
                "</body></html>"
            )

            self$results$summaryText$setContent(interpretation)
        },
        
        # Generate methodological footnotes for enhanced clinical understanding
        .generateMethodologicalFootnotes = function() {
            footnotes <- "<div style='margin-top: 20px; font-size: 0.9em; color: inherit; opacity: 0.85;'>"
            footnotes <- paste0(footnotes, "<p><strong>", .("Methodological Notes:"), "</strong></p>")
            footnotes <- paste0(footnotes, "<ul style='font-size: 0.85em;'>")
            item <- function(label, text) paste0("<li><strong>", label, ":</strong> ", text, "</li>")

            footnotes <- paste0(footnotes, item(.("Net Benefit Formula"),
                .("NB = (TP/n) - (FP/n) \u00D7 pt/(1-pt), where pt is the threshold probability.")))
            footnotes <- paste0(footnotes, item(.("Reference Strategies"),
                .("'Treat All' assumes all patients receive the intervention; 'Treat None' assumes no intervention.")))
            footnotes <- paste0(footnotes, item(.("Threshold Probability"),
                .("The minimum probability at which a patient would choose the intervention over no intervention.")))

            if (private$.ciDrawn()) {
                reps  <- self$options$bootReps
                level <- self$options$ciLevel * 100
                footnotes <- paste0(footnotes,
                    if (private$.ciBand() == "simultaneous")
                        item(.("Confidence Band"), paste(
                            .fmt(.('Simultaneous sup-t bootstrap band from {reps} replications at the nominal {level}% level (Montiel Olea and Plagborg-Moller 2019, supplementary Appendix A.2, Algorithm 3).'),
                                 reps = reps, level = level),
                            .("It is computed separately for each model and any clinical rule and covers that whole curve only approximately; coverage falls below nominal in small samples and where few patients are classified positive.")))
                    else
                        item(.("Confidence Intervals"), paste(
                            .fmt(.('Pointwise percentile-bootstrap intervals from {reps} replications at the {level}% level, drawn for every model and any clinical rule.'),
                                 reps = reps, level = level),
                            .("They cover each threshold separately and are not a simultaneous band; choose the simultaneous band to read the whole curve at once."),
                            .("Coverage is approximate and falls below nominal where few patients are classified positive."))))
                # The bootstrap resamples ROWS, so it is only right when each row is a patient.
                # Duplicating every row narrows the interval by 1/sqrt(2) and nothing else in the
                # output says so (IR-decisioncurve-04).
                footnotes <- paste0(footnotes, item(.("Unit of Resampling"),
                    .("Rows are resampled as independent patients, so the data must have one row per patient; several specimens, blocks or lesions from the same patient make these intervals and any comparison p-values too narrow.")))
            }

            if (self$options$calculateClinicalImpact) {
                # base::format: .fmt() prints a round double such as 100000 as "1e+05", and a
                # Number option is always a double (IR-decisioncurve-08). Same as the table note.
                footnotes <- paste0(footnotes, item(.("Clinical Impact"),
                    .fmt(.('Observed complete-case proportions projected to a population of {n} patients.'),
                         n = base::format(self$options$populationSize, scientific = FALSE, trim = TRUE))))
            }

            if (!is.null(private$.ruleStrategyLabel())) {
                # The label the rule carries in the tables and plots, not the raw option: with the
                # label box left blank the strategy is "Clinical Rule (<level>)", and the raw option
                # printed "()" here (IR-decisioncurve-07).
                footnotes <- paste0(footnotes, item(.("Clinical Decision Rule"),
                    .fmt(.('Applied as provided in the data ({label}).'),
                         label = private$.safeHtmlOutput(private$.ruleStrategyLabel()))))
            }

            # Net benefit and treat-all take the outcome prevalence from these rows. That is only
            # the population's prevalence for a cohort or consecutive series (IR-decisioncurve-01).
            footnotes <- paste0(footnotes, item(.("Sampling Design"), paste(
                .("Net benefit and the treat-all line use the outcome prevalence of the analysed sample, so they describe the target population only when the sample is a cohort or consecutive series from it."),
                .("For a case-control or otherwise enriched sample the curves do not apply to the source population; this analysis does not accept an external prevalence."))))

            footnotes <- paste0(footnotes, item(.("Prediction Scale"), paste(
                .("Values between 0 and 1 are accepted as probabilities, but this analysis does not verify calibration."),
                .("For time-to-event predictions, supply risks and a binary outcome defined at the same fixed horizon; censoring is not handled here."))))
            
            footnotes <- paste0(footnotes, "</ul></div>")
            
            return(footnotes)
        },
        
        # Optimize plot data for many models to improve performance and readability
        .optimizePlotDataForManyModels = function(plot_data, n_models) {
            # Strategies for handling many models:
            # 1. Reduce line thickness
            # 2. Sample data points for smoother rendering
            # 3. Consider highlighting top-performing models
            
            # Sample data points if there are many thresholds
            n_thresholds_per_model <- nrow(plot_data) / n_models
            if (n_thresholds_per_model > 100) {
                # Sample every nth point to reduce rendering load
                sample_rate <- ceiling(n_thresholds_per_model / 50)  # Target ~50 points per model
                
                optimized_data <- data.frame()
                for (model in unique(plot_data$model)) {
                    model_data <- plot_data[plot_data$model == model, ]
                    model_data <- model_data[seq(1, nrow(model_data), by = sample_rate), ]
                    optimized_data <- rbind(optimized_data, model_data)
                }
                
                # jamovi never surfaces message(), so this used to be silent: the curve on
                # screen was not the curve that was computed. Record it for the plot caption.
                private$.plotThinning <- list(
                    from = nrow(plot_data),
                    to = nrow(optimized_data)
                )

                return(optimized_data)
            }
            
            return(plot_data)
        },
        
        # Optimized plotting functions with performance enhancements for many models
        .plotDCA = function(image, ggtheme, theme, ...) {
            private$.restoreFromState(image)
            if (is.null(private$.plotData) || nrow(private$.plotData) == 0) {
                return(FALSE)
            }

            plot_data <- private$.plotData
            
            # Performance optimization for many models
            n_models <- length(unique(plot_data$model))
            max_models_threshold <- private$DECISIONCURVE_DEFAULTS$max_models_full_plot
            
            if (n_models > max_models_threshold) {
                plot_data <- private$.optimizePlotDataForManyModels(plot_data, n_models)
            }

            caption <- if (!is.null(private$.plotThinning))
                .fmt(.('Curve drawn from {to} of {from} computed points for rendering speed; tables and statistics use all of them.'),
                     to = private$.plotThinning$to, from = private$.plotThinning$from)

            # Are the reference strategies redrawn dashed further down? If so they must be
            # left OUT of the base layer: it used to draw every row solid and the dashed
            # layer was then painted on top of identical solid lines in the same colour, so
            # in the default plotStyle the dashed convention never actually took effect.
            dash_references <- self$options$plotStyle %in% c("standard", "detailed")
            is_reference <- plot_data$model %in% c("Treat All", "Treat None")

            # Create base plot with optimized aesthetics
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = threshold, y = net_benefit, color = model)) +
                # linewidth, not size: `size` for lines was deprecated in ggplot2 3.4.0 and emits a
                # deprecation warning into jamovi's Analysis Notes on every render.
                ggplot2::geom_line(
                    data = if (dash_references) plot_data[!is_reference, , drop = FALSE] else plot_data,
                    linewidth = if(n_models > max_models_threshold) 0.8 else 1) +
                ggplot2::labs(
                    title = .("Decision Curve Analysis"),
                    x = .("Threshold Probability"),
                    y = .("Net Benefit"),
                    color = .("Strategy"),
                    caption = caption
                ) +
                ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
                ggtheme

            # Limits of whatever interval is actually drawn; the y-zoom below must contain them.
            band_limits <- numeric(0)

            # Add confidence intervals if calculated and display requested
            if (private$.ciDrawn() &&
                "ci_lower" %in% names(plot_data)) {
                band <- private$.ciBand()
                lo_col <- if (band == "simultaneous") "sim_lower" else "ci_lower"
                hi_col <- if (band == "simultaneous") "sim_upper" else "ci_upper"
                model_data <- plot_data[!plot_data$model %in% c("Treat All", "Treat None"), ]
                # Drop rows with no interval (a failed bootstrap), otherwise geom_ribbon()
                # warns "Removed n rows containing missing values" into Analysis Notes.
                if (!lo_col %in% names(model_data)) model_data[[lo_col]] <- NA_real_
                if (!hi_col %in% names(model_data)) model_data[[hi_col]] <- NA_real_
                model_data <- model_data[!is.na(model_data[[lo_col]]) & !is.na(model_data[[hi_col]]), ]
                if (nrow(model_data) > 0) {
                    # The band description moves to the caption and the fill legend is
                    # dropped. geom_ribbon's fill = model built a SECOND legend listing the
                    # same model names as the colour legend; giving it its own title through
                    # labs(fill=) is precisely what stopped ggplot2 merging the two, and the
                    # pair disagreed (only the colour one lists Treat All/Treat None). On a
                    # four-strategy plot the legend strip measured 3.38 cm wide with both and
                    # 2.74 cm with this fix. The caption names the band ACTUALLY drawn -
                    # `band` is the same value that chose lo_col/hi_col above.
                    band_text <- if (band == "simultaneous")
                        .fmt(.('Shaded: {level}% simultaneous band'), level = sprintf("%.0f", self$options$ciLevel * 100))
                    else
                        .fmt(.('Shaded: {level}% pointwise CI'), level = sprintf("%.0f", self$options$ciLevel * 100))
                    p <- p + ggplot2::geom_ribbon(
                        data = model_data,
                        ggplot2::aes(ymin = .data[[lo_col]], ymax = .data[[hi_col]], fill = model),
                        alpha = 0.2, color = NA
                    ) +
                    ggplot2::guides(fill = "none") +
                    # the bands are bootstrap intervals: name the seed that drew them. One part
                    # per line: joined with a space it read "Shaded: 95% pointwise CI Random seed: 42".
                    ggplot2::labs(caption = paste(c(caption, band_text,
                        .fmt(.("Random seed: {seed}"),
                            seed = private$.seedValue())),
                        collapse = "\n"))
                    band_limits <- c(model_data[[lo_col]], model_data[[hi_col]])
                }
            }

            # Highlight clinical range if requested. A reversed range is not drawn; .run() says so.
            if (self$options$highlightRange &&
                self$options$highlightMin < self$options$highlightMax) {
                p <- p + ggplot2::annotate(
                    "rect",
                    xmin = self$options$highlightMin,
                    xmax = self$options$highlightMax,
                    ymin = -Inf, ymax = Inf,
                    alpha = 0.1, fill = "yellow"
                )
            }

            # Optimize legend and colors for many models
            if (n_models > max_models_threshold) {
                # Use more efficient legend positioning and reduce legend size
                p <- p + ggplot2::theme(
                    legend.position = "bottom",
                    legend.text = ggplot2::element_text(size = 8),
                    legend.title = ggplot2::element_text(size = 9),
                    legend.key.size = ggplot2::unit(0.4, "cm")
                )
                
                # Consider using fewer distinct colors and rely more on line patterns
                if (n_models > 15) {
                    p <- p + ggplot2::guides(color = ggplot2::guide_legend(ncol = 3))
                }
            }

            # Style reference lines differently
            if (dash_references) {
                # Make treat all/none lines dashed. These rows were excluded from the base
                # layer above, so this REPLACES them rather than overprinting them.
                treat_lines <- plot_data[is_reference, , drop = FALSE]
                if (nrow(treat_lines) > 0) {
                    p <- p + ggplot2::geom_line(
                        data = treat_lines,
                        linetype = "dashed", 
                        linewidth = if(n_models > max_models_threshold) 0.6 else 0.8
                    )
                }
            }

            # Add annotations for detailed style
            if (self$options$plotStyle == "detailed") {
                # Add horizontal line at 0
                p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5)
            }

            # Label the reference strategies at the right-most plotted threshold. This
            # option previously reached an empty stub and therefore did nothing.
            if (self$options$showReferenceLinesLabels) {
                reference_data <- plot_data[
                    plot_data$model %in% c("Treat All", "Treat None"),
                    , drop = FALSE
                ]
                if (nrow(reference_data) > 0) {
                    label_rows <- do.call(rbind, lapply(
                        split(reference_data, reference_data$model),
                        function(x) x[which.max(x$threshold), , drop = FALSE]
                    ))
                    p <- p + ggplot2::geom_text(
                        data = label_rows,
                        ggplot2::aes(label = model),
                        hjust = 1.05,
                        vjust = -0.5,
                        show.legend = FALSE
                    )
                }
            }

            # Zoom the y-axis to the decision curves. Treat-all is
            # prevalence - (1 - prevalence) * t/(1 - t), which dives towards minus infinity as
            # t approaches 1: with thresholdRange = "auto" the grid runs to 0.99, where that
            # line reaches about -76 on a 27%-prevalence cohort and squashes every model curve
            # into 0.4% of the panel height -- the clinically informative part of the figure
            # becomes an unreadable flat band. .plotRelativeUtility already solved this; use
            # coord_cartesian (a zoom) and NOT ylim (a scale limit, which DROPS rows and would
            # silently truncate the reference lines).
            # The drawn band counts too. Zoomed on the curves alone, coord_cartesian() cut the
            # ribbon off at the panel edge: on n = 80 the upper limit at low thresholds read as
            # the top of the plot, 0.170 where it was 0.221, and the intervals are shown nowhere
            # else (IR-decisioncurve-05). Treat-all is still left out on purpose (see above).
            model_nb <- c(plot_data$net_benefit[!plot_data$model %in% c("Treat All", "Treat None")],
                          band_limits)
            model_nb <- model_nb[is.finite(model_nb)]
            if (length(model_nb) > 0) {
                # Pad OUTWARDS. `max * 1.1` moves the ceiling DOWN when the maximum is
                # negative -- for a model worse than treat-none across the whole plotted range
                # that clipped the curve and pushed the y = 0 reference line off the panel, so
                # the reader lost the very line that shows the model is harmful. Always keep
                # zero visible: it is the treat-none strategy.
                span      <- max(model_nb) - min(model_nb)
                pad       <- max(0.02, 0.1 * span)
                y_floor   <- min(-0.05, min(model_nb) - pad)
                y_ceiling <- max(0.05, max(model_nb) + pad)
                if (is.finite(y_floor) && is.finite(y_ceiling) && y_ceiling > y_floor)
                    p <- p + ggplot2::coord_cartesian(ylim = c(y_floor, y_ceiling))
            }

            print(p)
            return(TRUE)
        },

        .plotClinicalImpact = function(image, ggtheme, theme, ...) {
            private$.restoreFromState(image)
            impact_data <- private$.impactData
            if (is.null(impact_data) || nrow(impact_data) == 0) return(FALSE)

            # Rates per 100 from .run() (see .buildImpactData), projected to the population here.
            pop_size <- self$options$populationSize
            rate_cols <- c("true_positives_per_100", "false_positives_per_100")
            impact_data[rate_cols] <- impact_data[rate_cols] * (pop_size / 100)

            # Reshape data for stacked bar chart (tidyr::gather namespaced below)
            # No magrittr pipe: %>% is not imported by this package's NAMESPACE on its own
            # account, so this line resolved only by accident of what other analyses import.
            # It failed outright when the renderer ran from a restored state.
            plot_data <- tidyr::gather(impact_data, key = "outcome_type", value = "count",
                                       true_positives_per_100, false_positives_per_100)
            tp_label <- .("True Positives")
            fp_label <- .("False Positives")
            plot_data <- dplyr::mutate(
                plot_data,
                outcome_type = factor(outcome_type,
                                      levels = c("true_positives_per_100", "false_positives_per_100"),
                                      labels = c(tp_label, fp_label))
            )
            # Okabe-Ito blue / vermillion. The old darkgreen/darkred collapsed under deuteranopia
            # to #5D5311 vs #574C00 (L* 35 vs 32, colorspace::deutan): one olive bar, the
            # benefit/harm split invisible (/review-function 2026-10-13).
            fill_values <- stats::setNames(c("#0072B2", "#D55E00"), c(tp_label, fp_label))

            # Create stacked bar chart showing clinical impact
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(threshold), y = count, fill = outcome_type)) +
                ggplot2::geom_bar(stat = "identity", position = "stack") +
                # scales = "fixed": y is a projected patient count, so a per-panel scale made
                # two models look alike when their counts differed by a factor of two.
                ggplot2::facet_wrap(~ model, scales = "fixed") +
                ggplot2::labs(
                    title = .fmt(.('Clinical Impact: Projected Outcomes in a Population of {n}'),
                                 n = base::format(pop_size, scientific = FALSE, trim = TRUE)),
                    x = .("Threshold Probability"),
                    y = .("Projected Number of Patients"),
                    fill = .("Outcome Type")
                ) +
                ggplot2::scale_x_discrete(labels = function(x) paste0(as.numeric(x) * 100, "%")) +
                ggtheme +
                # AFTER ggtheme: a jamovi ggtheme is a COMPLETE theme and replaces every
                # earlier theme() call, so this rotation was silently dropped and the
                # percentage x-labels overlapped. Same ordering as .plotNetBenefit above.
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
            # Also after ggtheme, and for the same reason: blue/vermillion here is not
            # decoration but the meaning of the two bar segments (benefit vs harm), so it
            # is a deliberate override of the global palette. Before ggtheme it was dead,
            # and the legend was titled with the raw column name "outcome_type".
            # ggplot2 says "Scale for fill is already present" when the `+` replaces the
            # ggtheme fill scale; the replacement is the point, so silence that one message.
            p <- suppressMessages(p + ggplot2::scale_fill_manual(values = fill_values))

            print(p)
            return(TRUE)
        },

        .plotInterventionsAvoided = function(image, ggtheme, theme, ...) {
            # This used to build a plotState and setState() it HERE, inside the renderer, where
            # nothing can ever read it back -- a dead write that only bloated the .omv. State is
            # published from .run() now; this restores from it.
            private$.restoreFromState(image)
            plot_data <- private$.plotData
            if (is.null(plot_data) || nrow(plot_data) == 0) return(FALSE)

            # Every strategy's curve, Treat All included, is in plotData; match treat-all by
            # threshold value rather than by position.
            treat_all <- plot_data[plot_data$model == "Treat All", , drop = FALSE]
            avoided_data <- plot_data[!plot_data$model %in% c("Treat All", "Treat None"),
                                      c("threshold", "net_benefit", "model"), drop = FALSE]
            if (nrow(avoided_data) == 0 || nrow(treat_all) == 0) return(FALSE)
            avoided_data$interventions_avoided <- mapply(
                private$.calculateNetInterventionsAvoided,
                avoided_data$net_benefit,
                treat_all$net_benefit[match(avoided_data$threshold, treat_all$threshold)],
                avoided_data$threshold,
                MoreArgs = list(population = 100))

            # A missed case costs 1/odds interventions here: 99 at the 1% threshold the automatic
            # range starts from, so one curve could dive to several hundred below zero and flatten
            # the others. Zoom (coord_cartesian keeps every point) no lower than -100 per 100 and
            # name the strategies that leave the panel. -100 is a display floor, not a bound.
            caption <- .("Positive values favour the model; negative values favour treating everyone.")
            y_floor <- -100
            y_vals <- avoided_data$interventions_avoided[is.finite(avoided_data$interventions_avoided)]
            zoom <- length(y_vals) > 0 && min(y_vals) < y_floor
            if (zoom) {
                cut_off <- unique(avoided_data$model[
                    is.finite(avoided_data$interventions_avoided) &
                        avoided_data$interventions_avoided < y_floor])
                caption <- paste(caption, .fmt(
                    .('{models} falls below -100 per 100 patients at some thresholds and is cut off there.'),
                    models = paste(cut_off, collapse = ", ")), sep = "\n")
            }

            # Create line plot showing interventions avoided
            p <- ggplot2::ggplot(avoided_data, ggplot2::aes(x = threshold, y = interventions_avoided, color = model)) +
                ggplot2::geom_line(linewidth = 1) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
                ggplot2::labs(
                    title = .("Net Interventions Avoided vs Treat All"),
                    subtitle = .("Derived from the net-benefit gain after accounting for threshold odds"),
                    x = .("Threshold Probability"),
                    y = .("Net Interventions Avoided per 100 Patients"),
                    color = .("Model"),
                    caption = caption
                ) +
                ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
                ggtheme
            if (zoom)
                p <- p + ggplot2::coord_cartesian(ylim = c(y_floor, max(c(y_vals, 0))))

            print(p)
            return(TRUE)
        },

        .plotRelativeUtility = function(image, ggtheme, theme, ...) {
            # State is published from .run() now (see .publishPlotStates); the setState that
            # used to sit here ran during render, where nothing could read it back.
            private$.restoreFromState(image)

            plot_data <- private$.plotData
            prevalence <- private$.prevalence
            if (is.null(plot_data) || is.null(prevalence)) return(FALSE)

            # Calculate Relative Utility
            # RU = (NB_model - NB_all) / (NB_perfect - NB_all)

            plot_data$relative_utility <- NA
            
            for (i in seq_len(nrow(plot_data))) {
                thresh <- plot_data$threshold[i]
                nb <- plot_data$net_benefit[i]
                
                # NB_perfect (Sensitivity=1, Specificity=1)
                nb_perfect <- prevalence

                # The baseline is the BEST default strategy at this threshold: treat
                # everyone below the prevalence, treat no one above it. Using raw treat-all
                # made the denominator explode above the prevalence, where treat-all net
                # benefit dives towards minus infinity - the do-nothing line then scored
                # 95-99% of "perfect" at high thresholds, which reads as an excellent
                # strategy when it is simply the absence of one.
                nb_all <- prevalence - (1 - prevalence) * (thresh / (1 - thresh))
                nb_baseline <- max(nb_all, 0)

                # nb_perfect - nb_baseline, written out: (1 - prevalence) x odds below the
                # prevalence, the prevalence above it. It is positive for every threshold in
                # (0, 1); the old abs(denom) > 1e-6 cut-off blanked valid values once the
                # prevalence neared 1 (Codex review 2026-10-14).
                denom <- if (nb_all > 0) (1 - prevalence) * thresh / (1 - thresh) else nb_perfect
                ru <- if (is.finite(denom) && denom > 0) (nb - nb_baseline) / denom else NA_real_
                
                plot_data$relative_utility[i] <- ru
            }
            
            # The curve used to be truncated twice - rows outside (-0.5, 1.1] were dropped
            # and then ylim() dropped more, because ylim() sets a scale limit rather than a
            # viewport. A model performing badly therefore had its line simply stop, with no
            # indication that anything had been removed. Zoom with coord_cartesian instead,
            # which keeps every observation and only changes what is in view, and say so
            # when a model actually runs off the bottom.
            y_floor <- -0.2
            y_ceiling <- 1.05
            # Reference strategies are excluded from this caption. Treat None has NB = 0 by
            # definition, so its RU is (0 - nb_baseline)/(prevalence - nb_baseline), which is
            # large and negative at every threshold below the prevalence: at prevalence 0.20
            # and t = 0.05 it is -3.75. Since the default range starts at 5%, the caption
            # fired on essentially every run and named a reference strategy as a model,
            # which is both wrong and loud enough to stop the warning being read at all.
            below_view <- plot_data[
                !is.na(plot_data$relative_utility) &
                    plot_data$relative_utility < y_floor &
                    !plot_data$model %in% c("Treat All", "Treat None"), ]
            off_view_models <- unique(below_view$model)

            plot_caption <- if (length(off_view_models) > 0) {
                .fmt(.('{models} falls below the visible range at some thresholds: relative utility there is worse than shown.'),
                     models = paste(off_view_models, collapse = ", "))
            } else {
                NULL
            }
            # Say what is plotted. Above the prevalence D = 0 and RU equals the standardized net
            # benefit (Kerr 2016 calls NB/prevalence "relative utility"); below it the two curves
            # differ, and the plot did not say which definition it used (VAL-decisioncurve-08).
            # Attributed on its own line, so the formula sentence above keeps its translation:
            # Baker, Cook, Vickers & Kramer 2009, JRSS-A 172:729-748, section 9 (IR-decisioncurve-10).
            plot_caption <- paste(c(
                .("RU = (NB - D) / (prevalence - D), where D is the better of treat-all and treat-none; it equals the standardized net benefit at thresholds above the prevalence."),
                .("Relative utility as defined by Baker et al. (2009)."),
                plot_caption), collapse = "\n")

            plot <- ggplot2::ggplot(plot_data,
                        ggplot2::aes(x = threshold, y = relative_utility, color = model)) +
                ggplot2::geom_line(linewidth = 1) +
                ggplot2::labs(title = .("Relative Utility Curve"),
                     x = .("Threshold Probability"),
                     y = .("Relative Utility (vs best default strategy)"),
                     color = .("Model"),
                     # Wrapped: as long single lines the caption overflowed and was clipped at the left
                     # edge at the default plot size (2026-10-21).
                     caption = paste(strwrap(unlist(strsplit(plot_caption, "\n", fixed = TRUE)), width = 90),
                                     collapse = "\n")) +
                # theme_minimal() removed: it sat before `+ ggtheme`, which replaces every
                # earlier theme, so it was dead code that only hid the global theme.
                ggtheme +
                ggplot2::coord_cartesian(ylim = c(y_floor, y_ceiling))

            print(plot)
            return(TRUE)
        },
        
        .plotStandardizedNetBenefit = function(image, ggtheme, theme, ...) {
            # State is published from .run() now (see .publishPlotStates); the setState that
            # used to sit here ran during render, where nothing could read it back.
            private$.restoreFromState(image)

            plot_data <- private$.plotData
            prevalence <- private$.prevalence
            if (is.null(plot_data) || is.null(prevalence)) return(FALSE)

            # Standardized Net Benefit (sNB) = NB / Prevalence
            plot_data$snb <- plot_data$net_benefit / prevalence
            
            plot <- ggplot2::ggplot(plot_data,
                        ggplot2::aes(x = threshold, y = snb, color = model)) +
                # linewidth, not size: `size` for lines was deprecated in ggplot2 3.4.0 and
                # emits a deprecation warning on every render.
                ggplot2::geom_line(linewidth = 1) +
                ggplot2::labs(title = .("Standardized Net Benefit"),
                     subtitle = .("Net benefit divided by outcome prevalence (dimensionless)"),
                     x = .("Threshold Probability"),
                     y = .("Standardized Net Benefit (NB / Prevalence)"),
                     color = .("Model"),
                     caption = paste(strwrap(.("A value of 1 corresponds to the maximum net benefit of perfect classification; values are not counts per 100 patients."),
                                             width = 90), collapse = "\n")) +
                # theme_minimal() removed: dead code before `+ ggtheme` (see above).
                ggtheme

            # Same y-zoom as .plotDCA and .plotRelativeUtility. Dividing by the prevalence
            # magnifies the treat-all dive: at a prevalence of 0.047 with thresholdRange="auto"
            # the panel spanned -2122 to 102 and the model curve occupied 0.04% of its height.
            snb_model <- plot_data$snb[!plot_data$model %in% c("Treat All", "Treat None")]
            snb_model <- snb_model[is.finite(snb_model)]
            if (length(snb_model) > 0) {
                span      <- max(snb_model) - min(snb_model)
                pad       <- max(0.02, 0.1 * span)
                y_floor   <- min(-0.05, min(snb_model) - pad)
                y_ceiling <- max(0.05, max(snb_model) + pad)
                if (is.finite(y_floor) && is.finite(y_ceiling) && y_ceiling > y_floor)
                    plot <- plot + ggplot2::coord_cartesian(ylim = c(y_floor, y_ceiling))
            }
            
            print(plot)
            return(TRUE)
        }
    )
)
