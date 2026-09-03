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

        # Store analysis results
        .dcaResults = NULL,
        .treatAllNB = NULL,
        .plotThinning = NULL,
        .plotData = NULL,
        .clinicalImpactData = NULL,
        .analysisData = NULL,
        .analysisOutcomes = NULL,
        .outcomePositive = NULL,

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
        # Only the six fields the renderers actually use are carried; .analysisData (the whole
        # analysis frame) is deliberately NOT among them, so the state stays small in the .omv.
        # What each renderer actually reads. Publishing the whole blob to all five images cost
        # ~517 KB x 5 = 2.6 MB in the .omv on a 5,000-row, four-model analysis, because
        # .dcaResults carries a prediction vector per model. .plotDCA -- the only plot on by
        # default -- needs none of that, so slicing takes the common case from 517 KB to ~8 KB.
        # i18n: every user-facing string is wrapped in .() with {placeholder} values via .fmt();
        # the "Treat All"/"Treat None" labels are strategy KEYS compared in code and stay literal.

        .plotStateSpec = function() {
            list(
                dcaPlot                    = c("plotData", "plotThinning"),
                # ...plus analysisOutcomes/outcomePositive, which .calculateModelAtThreshold
                # reads transitively -- omitting them made this the one plot that still came
                # back blank from state.
                clinicalImpactPlot         = c("dcaResults", "analysisOutcomes",
                                               "outcomePositive"),
                interventionsAvoidedPlot   = c("dcaResults", "treatAllNB"),
                relativeUtilityPlot        = c("dcaResults", "plotData", "analysisOutcomes",
                                               "outcomePositive"),
                standardizedNetBenefitPlot = c("dcaResults", "plotData", "analysisOutcomes",
                                               "outcomePositive")
            )
        },

        .plotStateFields = function() {
            list(dcaResults       = private$.dcaResults,
                 plotData         = private$.plotData,
                 plotThinning     = private$.plotThinning,
                 treatAllNB       = private$.treatAllNB,
                 analysisOutcomes = private$.analysisOutcomes,
                 outcomePositive  = private$.outcomePositive)
        },

        # ColorBrewer "Set1" has exactly nine colours; ggplot2 assigns NA past the ninth and
        # the extra curves vanish with only a console warning, which jamovi never shows. Nine
        # strategies is reachable: models plus Treat All, Treat None and a clinical rule. Fall
        # back to viridis, which is generated for any n and is colour-blind safe.
        .modelColourScale = function(plot_data) {
            n <- length(unique(plot_data$model))
            if (n <= 9)
                ggplot2::scale_color_brewer(palette = "Set1")
            else
                ggplot2::scale_color_viridis_d(option = "turbo", end = 0.92)
        },

        .plotImageNames = function() {
            c("dcaPlot", "clinicalImpactPlot", "interventionsAvoidedPlot",
              "relativeUtilityPlot", "standardizedNetBenefitPlot")
        },

        .publishPlotStates = function() {
            st   <- private$.plotStateFields()
            spec <- private$.plotStateSpec()
            for (nm in private$.plotImageNames()) {
                img <- self$results[[nm]]
                if (is.null(img)) next
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
            take(".dcaResults",       st$dcaResults)
            take(".plotData",         st$plotData)
            take(".plotThinning",     st$plotThinning)
            take(".treatAllNB",       st$treatAllNB)
            take(".analysisOutcomes", st$analysisOutcomes)
            take(".outcomePositive",  st$outcomePositive)
            invisible(NULL)
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
            tbl <- self$results$resultsTable
            cols <- private$.modelColumnNames(models)
            existing <- vapply(tbl$columns, function(c) c$name, character(1))
            for (i in seq_along(models)) {
                if (unname(cols[i]) %in% existing) next
                tbl$addColumn(name = unname(cols[i]), title = models[i],
                              type = "number", format = "zto")
            }
            invisible(NULL)
        },

        # Which interval the plot draws. NULL-safe: the option is absent from the compiled
        # Options class until jmvtools::prepare() has been run after adding it.
        .ciBand = function() {
            band <- tryCatch(self$options$ciBand, error = function(e) NULL)
            if (is.null(band) || !band %in% c("pointwise", "simultaneous")) "pointwise" else band
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

        # Helper method to escape variable names for notice IDs
        .escapeVar = function(varName) {
            gsub("[^A-Za-z0-9]", "_", varName)
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
          private$.noticeList[[length(private$.noticeList) + 1]] <- list(
            type = type,
            title = title,
            content = content
          )
        },

        # Render collected notices as HTML
        .renderNotices = function() {
          if (length(private$.noticeList) == 0) {
            return()
          }

          # Map notice types to colors and icons
          typeStyles <- list(
            ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5", icon = ""),
            STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74", icon = ""),
            WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047", icon = ""),
            INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd", icon = "")
          )

          html <- "<div style='margin: 10px 0;'>"

          for (notice in private$.noticeList) {
            style <- typeStyles[[notice$type]] %||% typeStyles$INFO

            html <- paste0(html,
              "<div style='background-color: ", style$bgcolor, "; ",
              "border-left: 4px solid ", style$border, "; ",
              "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
              "<strong style='color: ", style$color, ";'>",
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
            bootstrap_progress_threshold = 5000,
            performance_threshold_count = 1000,  # Threshold count for performance optimization
            bootstrap_chunk_size = 10000,       # Memory-efficient chunking threshold
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
        
        # Vectorized net benefit calculation for performance optimization
        .calculateNetBenefitsVectorized = function(predictions, outcomes, thresholds, positive_outcome) {
            # Convert outcomes to binary once
            binary_outcomes <- as.numeric(outcomes == positive_outcome)
            n <- length(outcomes)
            
            # Pre-allocate result vector
            net_benefits <- numeric(length(thresholds))
            
            # Calculate for each threshold (still a loop but optimized inner calculations)
            for (j in seq_along(thresholds)) {
                thresh <- thresholds[j]
                
                # Vectorized threshold comparison
                predicted_positive <- predictions >= thresh
                
                # Vectorized confusion matrix calculation
                tp <- sum(predicted_positive & binary_outcomes == 1)
                fp <- sum(predicted_positive & binary_outcomes == 0)
                
                # Net benefit formula
                net_benefits[j] <- (tp / n) - (fp / n) * (thresh / (1 - thresh))
            }
            
            return(net_benefits)
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

            # A range narrower than the step yields a single point. Nothing downstream guarded
            # against that: the analysis reported success, the weighted-AUC table printed
            # "20% - 20%" with a blank average net benefit, and every statistic in the model
            # comparison came back empty -- while the notice affirmed the threshold range. One
            # threshold cannot describe a curve, so refuse it and say what to change.
            if (length(thresholds) < 2) {
                pct <- function(x) sprintf("%.1f", x * 100)
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
                stop(narrow_msg, call. = FALSE)
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
                stop(msg, call. = FALSE)
            }

            if (min_thresh <= 0 || max_thresh >= 1) {
                msg <- paste(
                    .('Threshold probabilities must lie strictly between 0 and 1.'),
                    .fmt(.('Current settings: Min = {min}%, Max = {max}%; the valid range is 0.1% to 99.9%.'),
                         min = min_pct, max = max_pct)
                )
                private$.addNotice(type = "ERROR", title = .("Threshold Out of Bounds"), content = msg)
                private$.renderNotices()
                stop(msg, call. = FALSE)
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
            if (range_size > 0.7) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Wide Threshold Range"),
                    content = paste(span_txt,
                        .('Interpret the curve within a prespecified clinically plausible subrange; summaries that average across the full range weight every threshold equally.'))
                )
            }

            if (range_size < 0.05) {
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

        # Parse model names
        .parseModelNames = function() {
            model_names_str <- self$options$modelNames
            model_vars <- self$options$models

            # is.null FIRST: `NULL == ""` is logical(0), and `logical(0) || x` is an error
            # in R >= 4.3 rather than a usable FALSE.
            if (is.null(model_names_str) || !nzchar(model_names_str)) {
                return(model_vars)
            }

            # Parse comma-separated names
            parsed_names <- trimws(unlist(strsplit(model_names_str, ",", fixed = TRUE)))

            # If the number of names does not match, or any label is empty, use the source
            # variable names. .run() reports this substitution to the user.
            if (length(parsed_names) != length(model_vars) || any(!nzchar(parsed_names))) {
                return(model_vars)
            }

            return(parsed_names)
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
                stop(msg, call. = FALSE)
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
            
            # Progress reporting for large bootstrap runs
            if (n_boot >= private$DECISIONCURVE_DEFAULTS$bootstrap_progress_threshold) {
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

                    # Calculate net benefits for this bootstrap sample
                    for (j in seq_along(thresholds)) {
                        thresh <- thresholds[j]
                        nb_result <- private$.calculateNetBenefit(
                            boot_pred, boot_out, thresh, positive_outcome
                        )
                        boot_results[i, j] <- nb_result$net_benefit
                    }
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

                # Simultaneous sup-t band from the SAME replicates (Mandel & Betensky 2008):
                # standardise each replicate's deviation from the observed curve by the
                # bootstrap SE at that threshold, take the largest deviation across the
                # whole curve per replicate, and use its (1 - alpha) quantile as a common
                # multiplier. The band then covers the entire curve with probability
                # 1 - alpha, whereas the pointwise intervals cover each threshold separately
                # and reading them jointly overstates confidence. Where the SE is zero (no
                # one is classified positive, so net benefit is identically 0) the band
                # collapses onto the curve, which is exact.
                nb_hat <- private$.calculateNetBenefitsVectorized(
                    predictions, outcomes, thresholds, positive_outcome
                )
                se_boot <- apply(boot_results, 2, stats::sd, na.rm = TRUE)
                usable <- is.finite(se_boot) & se_boot > 0
                sim_lower <- sim_upper <- rep(NA_real_, length(thresholds))
                if (any(usable)) {
                    dev <- abs(sweep(boot_results[, usable, drop = FALSE], 2, nb_hat[usable], "-"))
                    dev <- sweep(dev, 2, se_boot[usable], "/")
                    max_dev <- apply(dev, 1, max, na.rm = TRUE)
                    max_dev <- max_dev[is.finite(max_dev)]
                    if (length(max_dev) >= 10) {
                        q_sim <- unname(quantile(max_dev, probs = self$options$ciLevel))
                        sim_lower <- nb_hat - q_sim * se_boot
                        sim_upper <- nb_hat + q_sim * se_boot
                        sim_lower[!usable] <- nb_hat[!usable]
                        sim_upper[!usable] <- nb_hat[!usable]
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
            self$results$procedureNotes$setContent("")
            self$results$summaryText$setContent("")
            self$results$notices$setContent("")

            # Clear the previous run's analysis state. The five .plot* renderers read these
            # private fields directly, so without this an early return - a bad variable, a
            # non-probability column, an invalid threshold range - left the PREVIOUS run's
            # curves on screen beside the new error notice, and the clinician saw a decision
            # curve that did not come from the data they were looking at.
            private$.dcaResults <- NULL
            private$.plotData <- NULL
            private$.treatAllNB <- NULL
            private$.analysisData <- NULL
            private$.analysisOutcomes <- NULL
            private$.clinicalImpactData <- NULL

            # ...and the IMAGE STATE with them. Clearing only the private fields is not enough
            # now that the renderers rehydrate from state: jamovi persists image state across
            # run cycles (jmvcore:::Image$fromProtoBuf only drops it when a clearWith OPTION
            # changed, and none of the five images lists `data`), so a data-only change -- a row
            # filter, an edited cell -- that makes this run bail out early would leave the
            # PREVIOUS cohort's state on the images, and .restoreFromState() would faithfully
            # redraw the previous cohort's curves beside the new error notice. That is the exact
            # failure the private-field reset above exists to prevent.
            private$.clearPlotStates()

            # Fix the RNG for every bootstrap in this run. Unseeded, the same data and the
            # same options gave a different confidence interval and a different p-value on
            # each run: across eight identical reruns at the default 1000 replications the
            # comparison p-value moved between 0.030 and 0.060 and the 95% CI crossed zero
            # in two of them. A clinician who reruns an analysis must get the same numbers.
            # The caller's RNG state is restored on exit so an R-API user's stream is not
            # disturbed by running this analysis.
            seed_val <- self$options$seed
            if (is.null(seed_val) || is.na(seed_val)) seed_val <- 42
            .had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
            .saved_seed <- if (.had_seed) {
                get(".Random.seed", envir = globalenv(), inherits = FALSE)
            } else {
                NULL
            }
            on.exit({
                if (.had_seed) {
                    assign(".Random.seed", .saved_seed, envir = globalenv())
                } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
                    base::remove(list = ".Random.seed", envir = globalenv())
                }
            }, add = TRUE)
            set.seed(seed_val)

            # Check if required packages are available
            required_packages <- c("ggplot2", "dplyr", "tidyr")
            missing_packages <- character(0)

            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                }
            }

            if (length(missing_packages) > 0) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("Missing Packages"),
                    content = paste(
                        .fmt(.('Required packages missing: {pkgs}.'), pkgs = paste(missing_packages, collapse = ", ")),
                        .fmt(.('Install with install.packages(c({quoted})).'),
                             quoted = paste0('"', paste(missing_packages, collapse = '", "'), '"'))
                    )
                )
                private$.renderNotices()
                return()
            }

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
                private$.addNotice(
                    type = "ERROR",
                    title = .("Duplicate Model Names"),
                    content = paste(
                        .('Each model needs a unique name.'),
                        .fmt(.('Rename the duplicated label(s): {names}.'), names = paste(duplicate_names, collapse = ", "))
                    )
                )
                private$.renderNotices()
                return()
            }

            reserved_names <- c("treat all", "treat none")
            if (any(tolower(model_names) %in% reserved_names)) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("Reserved Model Name"),
                    content = paste(
                        .("Model names cannot be 'Treat All' or 'Treat None' because those labels identify the reference strategies."),
                        .("Choose distinct model names.")
                    )
                )
                private$.renderNotices()
                return()
            }

            # Clinical decision rule variable (optional)
            rule_var <- NULL
            rule_positive <- NULL
            rule_label <- NULL
            if (self$options$clinicalDecisionRule && !is.null(self$options$decisionRuleVar)) {
                rule_var <- self$options$decisionRuleVar
                rule_positive <- self$options$decisionRulePositive
            }

            # Get complete cases
            complete_vars <- c(outcome_var, model_vars)
            if (!is.null(rule_var)) {
                complete_vars <- c(complete_vars, rule_var)
            }
            complete_cases <- complete.cases(data[complete_vars])

            if (sum(complete_cases) < 10) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("Insufficient Cases"),
                    content = paste(
                        .fmt(.('Insufficient complete cases for analysis ({n} cases available; this implementation requires at least 10 to run).'), n = sum(complete_cases)),
                        .('This computational safeguard does not imply that 10 observations are statistically adequate.'),
                        .('Net-benefit precision depends on event counts, prevalence and thresholds; use an adequately sized validation sample.')
                    )
                )
                private$.renderNotices()
                return()
            }

            # Filter data to complete cases
            analysis_data <- data[complete_cases, ]
            outcomes <- analysis_data[[outcome_var]]
            private$.analysisData <- analysis_data
            private$.analysisOutcomes <- outcomes

            # Event count, not just row count: the guards key on n and on prevalence, so a
            # cohort of 110 patients with 6 events (5.5% prevalence) cleared both and was
            # reported as a clean success -- yet every threshold's net benefit rests on those
            # six true positives. Deliberately placed AFTER the insufficient-cases guard above:
            # raised before it, this fired on runs that were being refused anyway and told a
            # user whose analysis had already stopped that it also had too few events.
            private$.warnOnLowEventCount(data, outcome_var, complete_cases)

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
                private$.addNotice(
                    type = "ERROR",
                    title = .("Outcome Not Binary"),
                    content = paste(
                        .('The outcome variable must be binary (exactly 2 levels).'),
                        .fmt(.('The current outcome "{var}" has {n} levels: {levels}.'),
                             var = outcome_var, n = length(unique_outcomes),
                             levels = paste(unique_outcomes, collapse = ", ")),
                        .('Decision curve analysis requires a binary outcome (diseased vs healthy, event vs no event); recode to binary or select a different outcome variable.')
                    )
                )
                private$.renderNotices()
                return()
            }

            # Validate positive outcome level
            if (is.null(outcome_positive) || length(outcome_positive) != 1 ||
                is.na(outcome_positive) || !(outcome_positive %in% unique_outcomes)) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("Positive Outcome Level Required"),
                    content = paste(
                        .('Select exactly one positive outcome level that is present in the analysis data.'),
                        .fmt(.('Available levels: {levels}.'), levels = paste(unique_outcomes, collapse = ", ")),
                        .('The analysis was stopped to avoid reversing event and non-event status.')
                    )
                )
                private$.renderNotices()
                return()
            }

            # Persist the validated level so every downstream table and plot uses the same
            # definition of the event.
            private$.outcomePositive <- outcome_positive

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

            # Validate clinical decision rule variable if provided
            rule_data <- NULL
            if (!is.null(rule_var)) {
                rule_data <- analysis_data[[rule_var]]
                rule_levels <- unique(rule_data)
                if (length(rule_levels) != 2) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("Rule Not Binary"),
                        content = paste(
                            .('The clinical decision rule variable must be binary (exactly 2 levels).'),
                            .fmt(.('The current rule variable "{var}" has {n} levels: {levels}.'),
                                 var = rule_var, n = length(rule_levels), levels = paste(rule_levels, collapse = ", ")),
                            .('Select a binary rule variable or disable "Clinical Decision Rule Integration".')
                        )
                    )
                    private$.renderNotices()
                    return()
                }
                if (is.null(rule_positive) || length(rule_positive) != 1 ||
                    is.na(rule_positive) || !(rule_positive %in% rule_levels)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("Rule Positive Level Required"),
                        content = paste(
                            .('Select exactly one positive rule level that is present in the analysis data.'),
                            .fmt(.('Available levels: {levels}.'), levels = paste(rule_levels, collapse = ", ")),
                            .('The analysis was stopped to avoid reversing intervention recommendations.')
                        )
                    )
                    private$.renderNotices()
                    return()
                }

                rule_label <- self$options$decisionRuleLabel
                if (is.null(rule_label) || !nzchar(trimws(rule_label))) {
                    rule_label <- .fmt(.('Clinical Rule ({level})'), level = rule_positive)
                }
                rule_label <- trimws(rule_label)

                if (tolower(rule_label) %in% tolower(c(model_names, "Treat All", "Treat None"))) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("Duplicate Strategy Name"),
                        content = paste(
                            .fmt(.('The clinical decision rule label "{label}" duplicates a model or reference-strategy name.'), label = rule_label),
                            .('Choose a unique rule label.')
                        )
                    )
                    private$.renderNotices()
                    return()
                }
            }

            # Generate threshold sequence
            thresholds <- private$.generateThresholds()

            # Performance monitoring for large analyses
            n_calculations <- length(model_vars) * length(thresholds)
            if (n_calculations >= private$DECISIONCURVE_DEFAULTS$performance_threshold_count) {
            }

            # Initialize results storage
            dca_results <- list()
            plot_data <- data.frame()

            # Calculate decision curves for each model
            for (i in seq_along(model_vars)) {
                model_var <- model_vars[i]
                model_name <- model_names[i]
                predictions <- analysis_data[[model_var]]

                # Progress reporting for multiple models
                if (length(model_vars) > 3) {
                }

                # The GUI restricts this box to numeric columns, but a programmatic caller
                # can still pass a factor or a character column. min() on a factor raises a
                # bare R error before any notice can be shown, so reject it explicitly.
                if (!is.numeric(predictions)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.('Not a numeric column: {model}'), model = model_name),
                        content = paste(
                            .fmt(.('Model "{model}" is a {class} column.'), model = model_name,
                                 class = paste(class(predictions), collapse = "/")),
                            .('Decision curve analysis needs predicted probabilities between 0 and 1 as a numeric column; convert it before running the analysis, because a categorical column cannot express a predicted risk.')
                        )
                    )
                    private$.renderNotices()
                    return()
                }

                if (all(is.na(predictions))) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.('No usable values: {model}'), model = model_name),
                        content = .fmt(.('Model "{model}" is entirely missing after complete-case filtering.'), model = model_name)
                    )
                    private$.renderNotices()
                    return()
                }

                # Validate that predictions are probabilities between 0 and 1. This range
                # check cannot establish calibration; it only prevents raw score scales from
                # being interpreted as clinically meaningful threshold probabilities.
                pred_min <- min(predictions, na.rm = TRUE)
                pred_max <- max(predictions, na.rm = TRUE)

                if (pred_min < 0 || pred_max > 1) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.('Invalid Probabilities: {model}'), model = model_name),
                        content = paste(
                            .fmt(.('Model "{model}" contains values outside 0 to 1 (min = {min}, max = {max}).'),
                                 model = model_name, min = sprintf("%.3f", pred_min), max = sprintf("%.3f", pred_max)),
                            .('Decision curve analysis requires predicted probabilities for the clinical outcome and prediction horizon being evaluated, not raw scores, logits or linear predictors.'),
                            .('Use model-based probabilities and assess their calibration separately in the target population; min-max scaling does not create calibrated risks.')
                        )
                    )
                    private$.renderNotices()
                    return()
                }

                # Warn if probabilities are suspiciously concentrated
                if (pred_max - pred_min < 0.05) {
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
                
                # Detailed results for specific calculations (fallback to individual calculations)
                detailed_results <- list()
                for (j in seq_along(thresholds)) {
                    thresh <- thresholds[j]
                    detailed_results[[j]] <- private$.calculateNetBenefit(
                        predictions, outcomes, thresh, outcome_positive
                    )
                }

                # Store results
                dca_results[[model_name]] <- list(
                    net_benefits = net_benefits,
                    detailed_results = detailed_results,
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
                if (self$options$confidenceIntervals || self$options$showNetBenefitCI) {
                    ci_results <- private$.calculateBootstrapCI(
                        predictions, outcomes, thresholds, outcome_positive,
                        self$options$bootReps
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

                rule_detailed <- lapply(thresholds, function(thresh) {
                    private$.calculateNetBenefit(rule_pred, outcomes, thresh, outcome_positive)
                })

                dca_results[[rule_label]] <- list(
                    net_benefits = rule_net,
                    detailed_results = rule_detailed,
                    thresholds = thresholds,
                    predictions = rule_pred,
                    is_rule = TRUE
                )

                rule_plot_data <- data.frame(
                    threshold = thresholds,
                    net_benefit = rule_net,
                    model = rule_label,
                    stringsAsFactors = FALSE
                )

                # The rule is a strategy like any model and its curve carries the same
                # sampling uncertainty; it used to be the only curve drawn without a band.
                if (self$options$confidenceIntervals || self$options$showNetBenefitCI) {
                    rule_ci <- private$.calculateBootstrapCI(
                        rule_pred, outcomes, thresholds, outcome_positive,
                        self$options$bootReps
                    )
                    rule_plot_data$ci_lower <- rule_ci$lower
                    rule_plot_data$ci_upper <- rule_ci$upper
                    rule_plot_data$sim_lower <- rule_ci$sim_lower
                    rule_plot_data$sim_upper <- rule_ci$sim_upper
                }

                plot_data <- dplyr::bind_rows(plot_data, rule_plot_data)
            }

            # Store results for plotting
            private$.dcaResults <- dca_results
            private$.plotData <- plot_data
            private$.treatAllNB <- treat_all_nb
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
                li(.("Prevalence"), paste0(round(mean(outcomes == outcome_positive) * 100, 1), "%")),
                li(.("Threshold Range"),
                   .fmt(.('{lo}% to {hi}%'), lo = round(min(thresholds) * 100, 1), hi = round(max(thresholds) * 100, 1))),
                li(.("Prediction Requirement"), paste(
                   .("Inputs must be probabilities for the clinical outcome and prediction horizon being evaluated."),
                   .("A value range of 0 to 1 does not establish calibration; calibration must be assessed separately in the target population."),
                   .("This binary analysis does not account for censoring."))),
                "</body></html>"
            )

            self$results$procedureNotes$setContent(procedure_notes)

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

            # Model comparison if requested
            if (self$options$compareModels && length(model_vars) > 1) {
                private$.performModelComparison()
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
            
            # Enhanced Model Comparison
            if (self$options$multiModelComparison && length(model_vars) > 1) {
                private$.performEnhancedModelComparison()
            }

            # Generate clinical interpretation
            private$.generateClinicalInterpretation()

            # Apparent net benefit is not validated net benefit. The analysis is handed a
            # column of predicted risks and has no way to know whether they were fitted on
            # these same rows; for the common case of a marker developed on this dataset
            # every curve here is optimistically biased in the model's favour.
            private$.addNotice(
                type = "STRONG_WARNING",
                title = .("Net benefit shown here is apparent, not validated"),
                content = paste(
                    .("These curves are computed on the same rows that supplied the predicted risks."),
                    .("If those risks came from a model fitted on this dataset - including a cutpoint, a score, or a regression developed here - the net benefit is optimistically biased and the model can appear to beat treat-all when it does not."),
                    .("For a defensible clinical claim, supply predictions from an external dataset or from cross-validation, and report which was used.")
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
            private$.warnOnThresholdsOutsideRange(selected_thresholds)
            results_table <- self$results$resultsTable

            # Clear existing rows
            results_table$deleteRows()

            # Add columns for each model dynamically
            model_names <- names(private$.dcaResults)
            model_columns <- private$.modelColumnNames(model_names)

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
                
                results_table$addRow(rowKey = paste0("thresh_", i), values = row_values)
            }

            results_table$setNote(
                "thresholds",
                .("Each row is calculated at the exact displayed threshold; selected thresholds do not need to coincide with the plotting grid.")
            )
        },

        .populateCostBenefitTable = function() {
            table <- self$results$costBenefitTable
            # Without this the rows appended below accumulate on every run cycle:
            # a three-model comparison becomes six rows, then nine.
            table$deleteRows()
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            
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

                    table$addRow(rowKey = paste0("model_", model_index, "_", i), values = list(
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
            # Without this the rows appended below accumulate on every run cycle:
            # a three-model comparison becomes six rows, then nine.
            table$deleteRows()
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            
            for (model_index in seq_along(model_names)) {
                model_name <- model_names[model_index]
                for (i in seq_along(selected_thresholds)) {
                    thresh <- selected_thresholds[i]
                    res <- private$.calculateModelAtThreshold(model_name, thresh)
                    if (is.null(res)) next
                    
                    # Calculate PPV/NPV
                    ppv <- if ((res$tp + res$fp) > 0) res$tp / (res$tp + res$fp) else NA
                    npv <- if ((res$tn + res$fn) > 0) res$tn / (res$tn + res$fn) else NA
                    
                    table$addRow(rowKey = paste0("model_", model_index, "_", i), values = list(
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
                }
            }
        },

        .populateResourceUtilizationTable = function() {
            table <- self$results$resourceUtilizationTable
            # Without this the rows appended below accumulate on every run cycle:
            # a three-model comparison becomes six rows, then nine.
            table$deleteRows()
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            pop_size <- self$options$populationSize
            
            for (model_index in seq_along(model_names)) {
                model_name <- model_names[model_index]
                for (i in seq_along(selected_thresholds)) {
                    thresh <- selected_thresholds[i]
                    res <- private$.calculateModelAtThreshold(model_name, thresh)
                    if (is.null(res)) next
                    
                    n_total <- res$tp + res$fp + res$tn + res$fn
                    scale_factor <- pop_size / n_total

                    tests_per_1000 <- n_total * scale_factor
                    treatments_per_1000 <- (res$tp + res$fp) * scale_factor
                    unnecessary_treatments <- res$fp * scale_factor
                    missed_cases <- res$fn * scale_factor
                    
                    # Reduction vs Treat All
                    # Treat All: All treated
                    treatments_all <- n_total * scale_factor
                    reduction <- (treatments_all - treatments_per_1000) / treatments_all
                    
                    table$addRow(rowKey = paste0("model_", model_index, "_", i), values = list(
                        model = model_name,
                        threshold = thresh,
                        tests_per_1000 = tests_per_1000,
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
            # Without this the rows appended below accumulate on every run cycle:
            # a three-model comparison becomes six rows, then nine.
            table$deleteRows()
            model_names <- names(private$.dcaResults)
            analysis_data <- private$.analysisData
            outcomes <- private$.analysisOutcomes
            thresholds <- private$.dcaResults[[1]]$thresholds
            model_vars_map <- private$.parseModelNames()
            
            if (length(model_names) < 2) return()
            
            # Pairwise comparisons. Rows are collected first so the family of pairwise
            # tests can be Holm-adjusted before any "Significant Difference" verdict is
            # printed - this table used to declare significance from an unadjusted p while
            # the comparisonTable beside it adjusted the same family.
            pairs <- combn(model_names, 2, simplify = FALSE)
            rows <- list()
            capped <- FALSE

            for (pair in pairs) {
                m1 <- pair[1]
                m2 <- pair[2]

                idx1 <- which(model_vars_map == m1)
                idx2 <- which(model_vars_map == m2)

                if (length(idx1) == 0 || length(idx2) == 0) next
                var1 <- self$options$models[idx1]
                var2 <- self$options$models[idx2]
                pred1 <- analysis_data[[var1]]
                pred2 <- analysis_data[[var2]]

                nb1 <- private$.dcaResults[[m1]]$net_benefits
                nb2 <- private$.dcaResults[[m2]]$net_benefits

                diff <- nb1 - nb2
                mean_diff <- mean(diff, na.rm = TRUE)
                median_diff <- median(diff, na.rm = TRUE)

                n_boot <- self$options$bootReps
                n_boot_used <- min(n_boot, 1000)   # capped for performance
                if (n_boot_used < n_boot) capped <- TRUE

                private$.checkpoint()

                res_boot <- private$.calculateBootstrapComparison(
                    pred1, pred2, outcomes, thresholds, private$.positiveLevel(),
                    n_boot = n_boot_used
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

            if (length(rows) == 0) return()

            raw_p <- vapply(rows, function(r) as.numeric(r$p_value %||% NA_real_), numeric(1))
            adj_p <- stats::p.adjust(raw_p, method = "holm")

            for (k in seq_along(rows)) {
                r <- rows[[k]]
                conclusion <- if (is.na(adj_p[k])) {
                    .("Not testable")
                } else if (adj_p[k] < 0.05) {
                    .("Exploratory adjusted p < 0.05")
                } else {
                    .("Exploratory adjusted p >= 0.05")
                }
                table$addRow(rowKey = r$key, values = list(
                    model1 = r$model1,
                    model2 = r$model2,
                    nb_difference_mean = r$nb_difference_mean,
                    nb_difference_median = r$nb_difference_median,
                    p_value = adj_p[k],
                    conclusion = conclusion
                ))
            }

            table$setNote(
                "method",
                paste(
                    .fmt(.('Exploratory mean difference in net benefit across the selected threshold range, with an approximate case-resampling bootstrap p-value Holm-adjusted across all {k} pairwise comparisons (seed {seed}).'),
                         k = length(rows),
                         seed = if (is.null(self$options$seed) || is.na(self$options$seed)) 42 else self$options$seed),
                    .("This is not a confirmatory test and depends on giving every threshold equal weight.")
                )
            )
            if (capped) {
                table$setNote(
                    "cap",
                    .fmt(.('Bootstrap replications for this table are capped at 1000 for speed; the {n} you requested are not used for this table.'),
                         n = self$options$bootReps)
                )
            }
        },

        .populateBenefitRangeTable = function() {
            range_table <- self$results$benefitRangeTable
            range_table$deleteRows()

            model_names <- names(private$.dcaResults)
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

                range_table$addRow(rowKey = i, values = list(
                    model = model_name,
                    range_start = info$range_start,
                    range_end = info$range_end,
                    range_width = info$width
                ))
            }


            range_table$setNote(
                "definition",
                paste(
                    .("Range of threshold probabilities over which the model's net benefit exceeds both treat-all and treat-none; a model is only worth using within this range."),
                    .("Threshold probability is set by clinical judgement about the relative cost of a missed case versus an unnecessary treatment; it is not estimated from the data, so there is no optimal value to report.")
                )
            )

            if (length(none_beneficial) > 0) {
                range_table$setNote(
                    "none",
                    .fmt(.('{models} never exceeds both reference strategies anywhere in the threshold range examined, so no range is shown.'),
                         models = paste(none_beneficial, collapse = ", "))
                )
            }

            if (length(has_gap) > 0) {
                range_table$setNote(
                    "gap",
                    paste(
                        .fmt(.('{models} is superior over more than one separate stretch of thresholds.'),
                             models = paste(has_gap, collapse = ", ")),
                        .("The start and end shown span a gap where the model is not superior - read the curve rather than the endpoints.")
                    )
                )
            }
        },

        .calculateClinicalImpactMetrics = function(outcomes, outcome_positive) {
            clinical_impact_table <- self$results$clinicalImpactTable
            clinical_impact_table$deleteRows()

            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            pop_size <- self$options$populationSize
            per_100_to_population <- pop_size / 100

            # Calculate for each model at each selected threshold
            row_counter <- 1
            for (model_name in model_names) {
                for (thresh in selected_thresholds) {
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
                    if (detailed_result$true_positives_per_100 > 0) {
                        nns <- 100 / detailed_result$true_positives_per_100
                    } else {
                        nns <- Inf
                    }

                    clinical_impact_table$addRow(rowKey = row_counter, values = list(
                        model = model_name,
                        threshold = thresh,
                        interventions_per_100 = detailed_result$interventions_per_100 * per_100_to_population,
                        true_positives_per_100 = detailed_result$true_positives_per_100 * per_100_to_population,
                        false_positives_per_100 = detailed_result$false_positives_per_100 * per_100_to_population,
                        interventions_avoided = interventions_avoided_per_100 * per_100_to_population,
                        number_needed_to_screen = if(is.finite(nns)) nns else NA
                    ))

                    row_counter <- row_counter + 1
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
            weighted_auc_table$deleteRows()

            model_names <- names(private$.dcaResults)
            thresholds <- private$.dcaResults[[1]]$thresholds

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

                # Gain over treating everyone, as a DIFFERENCE in weighted net benefit.
                # This used to be reported as the ratio (wauc - treat_all) / |treat_all|.
                # Treat-all net benefit crosses zero at a threshold equal to the outcome
                # prevalence, so whenever the threshold range brackets the prevalence the
                # denominator is near zero and the ratio explodes - percentages in the
                # hundreds were being displayed for ordinary differences.
                if (!is.na(wauc) && !is.na(reference_wauc)) {
                    benefit_gain <- wauc - reference_wauc
                } else {
                    benefit_gain <- NA
                }

                weighted_auc_table$addRow(rowKey = i, values = list(
                    model = model_name,
                    weighted_auc = wauc,
                    auc_range = paste0(round(min(thresholds) * 100, 1), "% - ",
                                       round(max(thresholds) * 100, 1), "%"),
                    benefit_gain = benefit_gain
                ))
            }

            weighted_auc_table$setNote(
                "wauc",
                paste(
                    .fmt(.('Average net benefit over the {lo}% to {hi}% threshold range: the area under the decision curve divided by the width of that range.'),
                         lo = sprintf("%.1f", min(thresholds) * 100), hi = sprintf("%.1f", max(thresholds) * 100)),
                    .("Every threshold in the range counts equally, so the value depends on the range you chose - report the range with it."),
                    .("Gain vs Default is the difference against the better of treating everyone and treating no one at each threshold, on the net-benefit scale: 0.01 means one extra true positive per 100 patients at no extra cost in unnecessary treatment."),
                    .("A gain at or below zero means a default strategy serves these patients at least as well as the model.")
                )
            )
        },

        .performModelComparison = function() {
            comparison_table <- self$results$comparisonTable
            comparison_table$deleteRows()

            model_names <- names(private$.dcaResults)
            analysis_data <- private$.analysisData
            outcomes <- private$.analysisOutcomes
            thresholds <- private$.dcaResults[[1]]$thresholds
            
            # Map model names to variable names
            model_vars_map <- private$.parseModelNames()

            # Compare each pair of models.
            # Rows are collected first so that the family of k(k-1)/2 pairwise tests can be
            # Holm-adjusted together before anything is displayed. Reporting only nominal
            # p-values here invites reading a five-model screen as if it were one test.
            rows <- list()
            skipped <- character(0)

            for (i in 1:(length(model_names) - 1)) {
                for (j in (i + 1):length(model_names)) {
                    model1_name <- model_names[i]
                    model2_name <- model_names[j]

                    # Find corresponding variables
                    idx1 <- which(model_vars_map == model1_name)
                    idx2 <- which(model_vars_map == model2_name)

                    # Derived strategies (the clinical decision rule, treat-all/treat-none)
                    # have no input column to resample, so they cannot enter the bootstrap
                    # comparison. Record the omission rather than dropping it silently.
                    if (length(idx1) == 0 || length(idx2) == 0) {
                        skipped <- c(skipped, .fmt(.('{a} vs {b}'), a = model1_name, b = model2_name))
                        next
                    }

                    var1 <- self$options$models[idx1]
                    var2 <- self$options$models[idx2]

                    pred1 <- analysis_data[[var1]]
                    pred2 <- analysis_data[[var2]]

                    # Calculate weighted AUC difference (observed)
                    wauc1 <- private$.calculateWeightedAUC(
                        private$.dcaResults[[model1_name]]$net_benefits,
                        thresholds
                    )
                    wauc2 <- private$.calculateWeightedAUC(
                        private$.dcaResults[[model2_name]]$net_benefits,
                        thresholds
                    )
                    wauc_diff <- wauc1 - wauc2

                    # Reuse bootReps from options
                    n_boot <- self$options$bootReps

                    private$.checkpoint()

                    res_boot <- private$.calculateBootstrapComparison(
                        pred1, pred2, outcomes, thresholds, private$.positiveLevel(),
                        n_boot = n_boot
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
                comparison_table$addRow(rowKey = k, values = r)
            }

            comparison_table$setNote(
                "boot",
                paste(
                    .fmt(.('Exploratory bootstrap comparison of average net benefit under each decision curve, {reps} resamples, seed {seed}.'),
                         reps = self$options$bootReps,
                         seed = if (is.null(self$options$seed) || is.na(self$options$seed)) 42 else self$options$seed),
                    .fmt(.('Intervals are {level}% percentile intervals and p-values are approximate; results depend on the selected threshold range and equal weighting of its thresholds.'),
                         level = sprintf("%.0f", self$options$ciLevel * 100)),
                    .("Re-running with the same seed reproduces these numbers exactly.")
                )
            )

            if (length(rows) > 1) {
                comparison_table$setNote(
                    "holm",
                    paste(
                        .fmt(.('p (Holm) controls the family-wise error rate across all {k} pairwise comparisons.'), k = length(rows)),
                        .("Interpret the unadjusted p only for a single comparison specified before the data were seen.")
                    )
                )
            }

            if (length(skipped) > 0) {
                private$.addNotice(
                    type = "INFO",
                    title = .("Comparisons not tested"),
                    content = paste(
                        .fmt(.('{pairs} could not be bootstrap-tested because at least one side is a derived strategy rather than a predictor column, so it has no values to resample.'),
                             pairs = paste(skipped, collapse = "; ")),
                        .("Its curve is still shown in the plot.")
                    )
                )
            }
        },

        .generateClinicalInterpretation = function() {
            model_names <- names(private$.dcaResults)

            # Identify the highest descriptive average over the selected range. This is not a
            # global model ranking: curves can cross, and the average gives every threshold
            # equal weight regardless of how plausible it is clinically.
            best_wauc <- -Inf
            best_model <- NULL

            for (model_name in model_names) {
                wauc <- private$.calculateWeightedAUC(
                    private$.dcaResults[[model_name]]$net_benefits,
                    private$.dcaResults[[model_name]]$thresholds
                )
                if (!is.na(wauc) && wauc > best_wauc) {
                    best_wauc <- wauc
                    best_model <- model_name
                }
            }

            # Generate interpretation text
            interpretation <- paste0(
                "<html><body>",
                "<h4>", .("Clinical Interpretation"), "</h4>"
            )

            if (!is.null(best_model)) {
                interpretation <- paste0(
                    interpretation,
                    "<p><strong>", .("Highest Average Net Benefit Over the Selected Range:"), "</strong> ",
                    private$.safeHtmlOutput(best_model), " (",
                    .("This only ranks the models against each other; it does not mean the model is useful."), " ",
                    .("Whether it beats treating everyone or no one is the Range of Benefit below."), " ",
                    .("Inspect the curves at prespecified clinical thresholds because curves may cross."),
                    ")</p>"
                )

                # Range over which the leading model beats both reference strategies.
                best_results <- private$.dcaResults[[best_model]]
                range_info <- private$.findBenefitRange(
                    best_results$net_benefits,
                    best_results$thresholds,
                    private$.treatAllNB
                )

                if (!is.na(range_info$range_start)) {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>", .("Range of Benefit:"), "</strong> ",
                        .fmt(.('{lo}% to {hi}% threshold probability - the range over which this model beats both treating everyone and treating no one.'),
                             lo = round(range_info$range_start * 100, 1), hi = round(range_info$range_end * 100, 1)),
                        if (isFALSE(range_info$contiguous))
                            paste0(" ", .("This range contains a gap where the model is not superior; read the curve."))
                        else "",
                        "</p>"
                    )
                } else {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>", .("Range of Benefit:"), "</strong> ",
                        .("none."), " ",
                        .("Across every threshold examined, treating everyone or treating no one does at least as well as this model."),
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
                .("NB = (TP/n) - (FP/n) \u{00D7} pt/(1-pt), where pt is the threshold probability.")))
            footnotes <- paste0(footnotes, item(.("Reference Strategies"),
                .("'Treat All' assumes all patients receive the intervention; 'Treat None' assumes no intervention.")))
            footnotes <- paste0(footnotes, item(.("Threshold Probability"),
                .("The minimum probability at which a patient would choose the intervention over no intervention.")))

            if (self$options$confidenceIntervals || self$options$showNetBenefitCI) {
                reps  <- self$options$bootReps
                level <- self$options$ciLevel * 100
                footnotes <- paste0(footnotes,
                    if (private$.ciBand() == "simultaneous")
                        item(.("Confidence Band"), paste(
                            .fmt(.('Simultaneous sup-t bootstrap band from {reps} replications at the {level}% level (Mandel and Betensky 2008).'),
                                 reps = reps, level = level),
                            .("The whole curve, including any clinical rule, lies within the band with this probability.")))
                    else
                        item(.("Confidence Intervals"), paste(
                            .fmt(.('Pointwise percentile-bootstrap intervals from {reps} replications at the {level}% level, drawn for every model and any clinical rule.'),
                                 reps = reps, level = level),
                            .("They cover each threshold separately and are not a simultaneous band; choose the simultaneous band to read the whole curve at once."))))
            }

            if (self$options$calculateClinicalImpact) {
                footnotes <- paste0(footnotes, item(.("Clinical Impact"),
                    .fmt(.('Observed complete-case proportions projected to a population of {n} patients.'),
                         n = self$options$populationSize)))
            }

            if (self$options$clinicalDecisionRule && !is.null(self$options$decisionRuleVar)) {
                footnotes <- paste0(footnotes, item(.("Clinical Decision Rule"),
                    .fmt(.('Applied as provided in the data ({label}).'),
                         label = private$.safeHtmlOutput(self$options$decisionRuleLabel))))
            }

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

            # Create base plot with optimized aesthetics
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = threshold, y = net_benefit, color = model)) +
                # linewidth, not size: `size` for lines was deprecated in ggplot2 3.4.0 and emits a
                # deprecation warning into jamovi's Analysis Notes on every render.
                ggplot2::geom_line(linewidth = if(n_models > max_models_threshold) 0.8 else 1) +
                ggplot2::labs(
                    title = .("Decision Curve Analysis"),
                    x = .("Threshold Probability"),
                    y = .("Net Benefit"),
                    color = .("Strategy"),
                    caption = if (!is.null(private$.plotThinning))
                        .fmt(.('Curve drawn from {to} of {from} computed points for rendering speed; tables and statistics use all of them.'),
                             to = private$.plotThinning$to, from = private$.plotThinning$from)
                    else NULL
                ) +
                ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
                ggtheme

            # Add confidence intervals if calculated and display requested
            if ((self$options$confidenceIntervals || self$options$showNetBenefitCI) &&
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
                    p <- p + ggplot2::geom_ribbon(
                        data = model_data,
                        ggplot2::aes(ymin = .data[[lo_col]], ymax = .data[[hi_col]], fill = model),
                        alpha = 0.2, color = NA
                    ) +
                    ggplot2::labs(fill = if (band == "simultaneous")
                        .fmt(.('{level}% simultaneous band'), level = sprintf("%.0f", self$options$ciLevel * 100))
                    else
                        .fmt(.('{level}% pointwise CI'), level = sprintf("%.0f", self$options$ciLevel * 100)))
                }
            }

            # Highlight clinical range if requested
            if (self$options$highlightRange) {
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
            if (self$options$plotStyle == "standard" || self$options$plotStyle == "detailed") {
                # Make treat all/none lines dashed
                treat_lines <- plot_data[plot_data$model %in% c("Treat All", "Treat None"), ]
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
            model_nb <- plot_data$net_benefit[!plot_data$model %in% c("Treat All", "Treat None")]
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
            if (is.null(private$.dcaResults) || (!self$options$calculateClinicalImpact && !self$options$showClinicalImpactPlot)) {
                return(FALSE)
            }

            # Get selected thresholds and models.
            # .parseSelectedThresholds() calls .addNotice() on a malformed entry, and renderers
            # run on every window resize without ever calling .renderNotices() -- so those
            # notices piled up in private$.noticeList, invisible, growing one per resize. Keep
            # the list exactly as .run() left it.
            saved_notices <- private$.noticeList
            selected_thresholds <- private$.parseSelectedThresholds()
            private$.noticeList <- saved_notices
            model_names <- names(private$.dcaResults)
            pop_size <- self$options$populationSize

            # Prepare data for clinical impact plot
            impact_data <- data.frame()

            for (model_name in model_names) {
                for (thresh in selected_thresholds) {
                    detailed_result <- private$.calculateModelAtThreshold(model_name, thresh)
                    if (is.null(detailed_result)) next
                    scale_factor <- pop_size / 100

                    # Add to plot data
                    impact_data <- rbind(impact_data, data.frame(
                        threshold = thresh,
                        model = model_name,
                        interventions_per_100 = detailed_result$interventions_per_100 * scale_factor,
                        true_positives_per_100 = detailed_result$true_positives_per_100 * scale_factor,
                        false_positives_per_100 = detailed_result$false_positives_per_100 * scale_factor,
                        stringsAsFactors = FALSE
                    ))
                }
            }

            if (nrow(impact_data) == 0) return(FALSE)

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
            fill_values <- stats::setNames(c("darkgreen", "darkred"), c(tp_label, fp_label))

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
                ggplot2::scale_fill_manual(values = fill_values) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggtheme

            print(p)
            return(TRUE)
        },

        .plotInterventionsAvoided = function(image, ggtheme, theme, ...) {
            # This used to build a plotState and setState() it HERE, inside the renderer, where
            # nothing can ever read it back -- a dead write that only bloated the .omv. State is
            # published from .run() now; this restores from it.
            private$.restoreFromState(image)

            if (is.null(private$.dcaResults)) {
                return(FALSE)
            }

            # Calculate interventions avoided compared to "treat all" strategy
            thresholds <- private$.dcaResults[[1]]$thresholds
            model_names <- names(private$.dcaResults)

            # Prepare data
            avoided_data <- data.frame()

            for (model_name in model_names) {
                model_results <- private$.dcaResults[[model_name]]
                interventions_avoided <- numeric(length(thresholds))

                for (j in seq_along(thresholds)) {
                    interventions_avoided[j] <- private$.calculateNetInterventionsAvoided(
                        model_results$net_benefits[j],
                        private$.treatAllNB[j],
                        thresholds[j],
                        population = 100
                    )
                }

                avoided_data <- rbind(avoided_data, data.frame(
                    threshold = thresholds,
                    interventions_avoided = interventions_avoided,
                    model = model_name,
                    stringsAsFactors = FALSE
                ))
            }

            if (nrow(avoided_data) == 0) return(FALSE)

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
                    caption = .("Positive values favour the model; negative values favour treating everyone.")
                ) +
                ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
                ggtheme

            print(p)
            return(TRUE)
        },

        .plotRelativeUtility = function(image, ggtheme, theme, ...) {
            # State is published from .run() now (see .publishPlotStates); the setState that
            # used to sit here ran during render, where nothing could read it back.
            private$.restoreFromState(image)

            if (is.null(private$.dcaResults)) return(FALSE)

            plot_data <- private$.plotData

            # Calculate Relative Utility
            # RU = (NB_model - NB_all) / (NB_perfect - NB_all)
            
            prevalence <- mean(private$.analysisOutcomes == private$.positiveLevel(), na.rm=TRUE)
            
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

                denom <- nb_perfect - nb_baseline

                if (abs(denom) > 1e-6) {
                    ru <- (nb - nb_baseline) / denom
                } else {
                    ru <- NA_real_
                }
                
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
            below_view <- plot_data[
                !is.na(plot_data$relative_utility) & plot_data$relative_utility < y_floor, ]
            off_view_models <- unique(below_view$model)

            plot_caption <- if (length(off_view_models) > 0) {
                .fmt(.('{models} falls below the visible range at some thresholds: relative utility there is worse than shown.'),
                     models = paste(off_view_models, collapse = ", "))
            } else {
                NULL
            }

            plot <- ggplot2::ggplot(plot_data,
                        ggplot2::aes(x = threshold, y = relative_utility, color = model)) +
                ggplot2::geom_line(linewidth = 1) +
                private$.modelColourScale(plot_data) +
                ggplot2::labs(title = .("Relative Utility Curve"),
                     x = .("Threshold Probability"),
                     y = .("Relative Utility (vs best default strategy)"),
                     color = .("Model"),
                     caption = plot_caption) +
                ggplot2::theme_minimal() +
                ggtheme +
                ggplot2::coord_cartesian(ylim = c(y_floor, y_ceiling))

            print(plot)
            return(TRUE)
        },
        
        .plotStandardizedNetBenefit = function(image, ggtheme, theme, ...) {
            # State is published from .run() now (see .publishPlotStates); the setState that
            # used to sit here ran during render, where nothing could read it back.
            private$.restoreFromState(image)

            if (is.null(private$.dcaResults)) return(FALSE)

            plot_data <- private$.plotData

            # Standardized Net Benefit (sNB) = NB / Prevalence
            prevalence <- mean(private$.analysisOutcomes == private$.positiveLevel(), na.rm=TRUE)
            
            plot_data$snb <- plot_data$net_benefit / prevalence
            
            plot <- ggplot2::ggplot(plot_data,
                        ggplot2::aes(x = threshold, y = snb, color = model)) +
                # linewidth, not size: `size` for lines was deprecated in ggplot2 3.4.0 and
                # emits a deprecation warning on every render.
                ggplot2::geom_line(linewidth = 1) +
                private$.modelColourScale(plot_data) +
                ggplot2::labs(title = .("Standardized Net Benefit"),
                     subtitle = .("Net benefit divided by outcome prevalence (dimensionless)"),
                     x = .("Threshold Probability"),
                     y = .("Standardized Net Benefit (NB / Prevalence)"),
                     color = .("Model"),
                     caption = .("A value of 1 corresponds to the maximum net benefit of perfect classification; values are not counts per 100 patients.")) +
                ggplot2::theme_minimal() +
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
