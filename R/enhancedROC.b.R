#' Enhanced ROC Analysis Class
#'
#' Comprehensive ROC analysis implementation inspired by BlueSky's createROCTable
#' with enhanced features for clinical diagnostic performance evaluation.
#'
#' @import jmvcore
#' @importFrom caret confusionMatrix
#' @importFrom boot boot boot.ci
#' @importFrom pROC roc
#' @importFrom stats quantile binom.test glm predict
#' @importFrom splines ns
#' @export
#' @return An \code{R6} class generator object for the \code{enhancedROCClass} backend; used internally by the jamovi analysis wrapper and not called directly.

enhancedROCClass <- R6::R6Class(
    "enhancedROCClass",
    inherit = enhancedROCBase,
    private = list(
        .rocResults = NULL,
        # NOT named .data: jmvcore::Analysis owns a private .data (self$data is an active
        # binding onto it) and Analysis$run() clears it when the caller did not supply the
        # data frame. Shadowing it made the cleaned analysis frame vanish between .run()
        # and the plot renderers, which read it after run() has returned.
        .analysisData = NULL,
        .binaryData = NULL,
        .outcome = NULL,
        .predictors = NULL,
        .rocObjects = NULL,
        # Smoothed curves are kept SEPARATE from .rocObjects on purpose. pROC::smooth() returns a
        # "smooth.roc", which carries no thresholds at all (0 thresholds against 514 sensitivity /
        # specificity points) and does not even inherit from "roc". Overwriting the empirical
        # object with it broke every cut-point, coordinate and confusion-matrix calculation
        # downstream. Smoothing is a display choice; it must never feed a reported statistic.
        .rocSmoothed = NULL,
        .positiveClass = NULL,
        .negativeLevels = NULL, # analysed levels other than the positive one (named in the report)
        .multiClassOutcome = NULL,
        # FALSE in multi-class mode without a positive class: only the multi-class tables run.
        .binaryEnabled = TRUE,
        .presetConfig = NULL,
        # one preset-override notice per run, not one per predictor
        .presetNoticeShown = FALSE,
        .prevalenceNoticeShown = FALSE, # extreme prevalence is a property of the sample, not of each predictor
        .sampleNoticesShown = FALSE, # class counts and n belong to the complete-case sample, shared by every predictor
        .metricPValueFailures = character(0), # metric comparisons whose test failed (reported once)
        .rocTestCache = list(), # one roc.test() per predictor pair and run, see .pairTest()
        # A BCa interval silently degraded to percentile because some resamples had no AUC
        # (see .bootstrapAucCi); disclosed once per run rather than once per predictor.
        .bcaFellBackToPercentile = FALSE,
        # The DeLong small-sample WARNING fired this run, so the Methods reference list carries
        # the two papers it cites.
        .delongWarned = FALSE,
        # The calibration slope cell printed its 'Above 1' reading, which cites Austin & Steyerberg
        # (2019), so the Methods reference list carries it even with the spline curve off.
        .underfitCited = FALSE,
        # Plot-ready frames for the calibration plot and the decision / clinical impact curves,
        # computed in .run() and published as image state: those renderers draw from state alone.
        .calibrationPlot = NULL,
        .decisionCurves = NULL,
        .instructionsHtml = "", # Accumulator for instructions panel content
        .analysisSummaryHtml = "", # Accumulator for analysis summary content

        # Preserve simple legacy keys. All other names use a reserved prefix and
        # UTF-8 hex: no two names collapse, including names resembling an encoded
        # key. Only the prefix contains an underscore, keeping joined keys distinct.
        .escapeVar = function(x) {
            if (is.character(x)) {
                x <- vapply(enc2utf8(x), function(value) {
                    if (grepl("^[A-Za-z][A-Za-z0-9]*$", value)) return(value)
                    paste0("_x", paste(sprintf("%02x", as.integer(charToRaw(value))), collapse = ""))
                }, character(1), USE.NAMES = FALSE)
            }
            return(x)
        },

        # HTML sanitization for security
        .safeHtmlOutput = function(text) {
            if (is.null(text) || length(text) == 0) {
                return("")
            }
            text <- as.character(text)
            # Sanitize potentially dangerous characters
            text <- gsub("&", "&amp;", text, fixed = TRUE)
            text <- gsub("<", "&lt;", text, fixed = TRUE)
            text <- gsub(">", "&gt;", text, fixed = TRUE)
            text <- gsub("\"", "&quot;", text, fixed = TRUE)
            text <- gsub("'", "&#x27;", text, fixed = TRUE)
            return(text)
        },

        # Every resampling call starts from the user's seed, so one predictor's interval, band or
        # test does not depend on which predictors or options were resampled before it - with one
        # stream seeded per run it did, and the seed printed beside it could not reproduce it.
        # with_seed() restores the run's stream afterwards.
        .seed = 0,
        .withSeed = function(expr) withr::with_seed(private$.seed, expr),

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
            # An empty list must CLEAR the panel, not leave it alone: the Html item keeps
            # whatever the PREVIOUS run wrote until something overwrites it, and jamovi's Html
            # view skips "" (see the narrative items in .run()), so the panel is hidden. Reached
            # by a clean Frozen-model validation report and by a throw before the first
            # .addNotice() - where the last run's ERROR panel would otherwise stand above tables
            # computed from new options.
            notices <- self$results$results$notices
            if (length(private$.noticeList) == 0) {
                notices$setContent("")
                notices$setVisible(FALSE)
                return()
            }
            notices$setVisible(TRUE)

            # Severity is said in words as well as colour (a grey-scale print or a colour-blind
            # reader cannot tell the fills apart), and notices are shown most severe first: they
            # used to appear in the order the code raised them, so an INFO could head the panel
            # above an ERROR. order() is stable, so each band keeps that order.
            typeStyles <- list(
                ERROR = list(bgcolor = "rgba(220, 38, 38, 0.10)", border = "#fca5a5", label = .("Error")),
                STRONG_WARNING = list(bgcolor = "rgba(234, 88, 12, 0.10)", border = "#fdba74", label = .("Important")),
                WARNING = list(bgcolor = "rgba(202, 138, 4, 0.12)", border = "#fde047", label = .("Warning")),
                INFO = list(bgcolor = "rgba(37, 99, 235, 0.08)", border = "#93c5fd", label = .("Note"))
            )
            types <- vapply(private$.noticeList, function(n) n$type, character(1))
            ranked <- private$.noticeList[order(match(types, names(typeStyles), nomatch = 4L))]

            html <- "<div style='margin: 10px 0;'>"

            for (notice in ranked) {
                style <- typeStyles[[notice$type]] %||% typeStyles$INFO

                html <- paste0(
                    html,
                    "<div style='background-color: ", style$bgcolor, "; ",
                    "border-left: 4px solid ", style$border, "; ",
                    "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                    "<strong>",
                    private$.safeHtmlOutput(paste0(style$label, ": ", notice$title)), "</strong><br>",
                    "<span style='color: inherit;'>", private$.safeHtmlOutput(notice$content), "</span>",
                    "</div>"
                )
            }

            html <- paste0(html, "</div>")

            self$results$results$notices$setContent(html)
        },

        # Draw an explanatory message on the plot canvas.
        #
        # Renderers run AFTER .run() has returned, so private$.addNotice() from inside a renderer
        # is unreachable: .renderNotices() is only ever called from .run(), and .noticeList is
        # cleared at the top of the next run. Twelve notices across the eleven renderers were
        # written this way and none of them could ever reach a user. Anything a renderer needs to
        # say has to be drawn on the image itself.
        #
        # Returns TRUE so jamovi keeps the panel: returning FALSE leaves an empty panel with no
        # explanation at all, which is the failure mode this replaces.
        .plotMessage = function(ggtheme, title, message) {
            tryCatch(
                {
                    # jamovi's `ggtheme` is a LIST of [theme, palette scales], so `ggtheme$text`
                    # is ALWAYS NULL and every on-plot message was drawn in black - unreadable on
                    # jamovi's dark theme. Pull the theme element out of the list and resolve the
                    # "text" element against it, which is where the real foreground colour lives.
                    txt_col <- tryCatch(
                        {
                            els <- if (inherits(ggtheme, "theme")) list(ggtheme) else as.list(ggtheme)
                            thm <- Filter(function(el) inherits(el, "theme"), els)[[1]]
                            ggplot2::calc_element("text", thm)$colour
                        },
                        error = function(e) NULL
                    ) %||% "black"
                    p <- ggplot2::ggplot() +
                        ggplot2::annotate("text",
                            x = 0.5, y = 0.62, label = title,
                            fontface = "bold", size = 4.5, colour = txt_col
                        ) +
                        ggplot2::annotate("text",
                            x = 0.5, y = 0.52, size = 3.4, colour = txt_col,
                            vjust = 1, lineheight = 1.1,
                            label = paste(strwrap(message, width = 70), collapse = "\n")
                        ) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggtheme +
                        ggplot2::theme(
                            axis.title = ggplot2::element_blank(),
                            axis.text = ggplot2::element_blank(),
                            axis.ticks = ggplot2::element_blank(),
                            panel.grid = ggplot2::element_blank()
                        )
                    print(p)
                    TRUE
                },
                error = function(e) FALSE
            )
        },

        # Unsupported advanced options remain in the R API for compatibility but are not
        # exposed as jamovi controls. If selected programmatically, .run() names them in a
        # warning instead of implying that an uncomputed analysis succeeded.

        .init = function() {
            # Initialize error handling
            if (exists("clinicopath_init", mode = "function")) {
                clinicopath_init("enhancedROC", self$options$clinicalContext)
            }

            private$.analysisData <- self$data
            private$.outcome <- self$options$outcome
            private$.predictors <- self$options$predictors

            # Initialize instructions accumulator from base template
            private$.instructionsHtml <- private$.getInstructions()
            self$results$results$instructions$setContent(private$.instructionsHtml)

            # Initialize ROC objects list
            private$.rocObjects <- list()

            # Apply the user-selected pixel size to every image. The 600x600 declared in
            # enhancedROC.r.yaml is only a default; without setSize() the plotWidth/plotHeight
            # options have no effect on the rendered output at all. The renderers used to call
            # image$setState(list(width=, height=)) instead, which only stores render-time
            # payload and never reaches jamovi's image sizing. .init() re-runs on every option
            # change, so this tracks the options.
            plot_w <- self$options$plotWidth %||% 600
            plot_h <- self$options$plotHeight %||% 600
            for (imgName in private$.plotImageNames()) {
                self$results$results$get(imgName)$setSize(plot_w, plot_h)
            }

            # The sensitivity/specificity limits are Clopper-Pearson intervals at the user's
            # Confidence level (see .calculateBinomialCI()), so the heading has to say which
            # level rather than the hard-coded "95%" it used to carry.
            conf_pct <- format(self$options$confidenceLevel %||% 95)
            diagTable <- self$results$results$diagnosticPerformance
            diagTable$getColumn("sensitivity_ci")$setTitle(
                .fmt(.("Sensitivity {pct}% CI"), pct = conf_pct))
            diagTable$getColumn("specificity_ci")$setTitle(
                .fmt(.("Specificity {pct}% CI"), pct = conf_pct))
        },

        .plotImageNames = function() {
            c("rocCurvePlot", "prcPlot", "comparativeROCPlot", "cutoffAnalysisPlot",
              "youdenIndexPlot", "clinicalDecisionPlot", "crocCurvePlot", "convexHullPlot",
              "calibrationPlotImage", "multiClassROCPlot", "clinicalUtilityPlot",
              "decisionImpactPlot")
        },

        # Store only coordinates and displayed summaries. No individual outcomes,
        # predictors, fitted models or environments belong in saved image state.
        .plotStatePayload = function() {
            private$.bandFailures <- character(0)
            if (is.null(private$.analysisData)) return(NULL)
            withr::local_seed(if (is.finite(self$options$seed)) self$options$seed else 0)
            num_fields <- c("cutoff", "sensitivity", "specificity", "youden_index", "accuracy",
                            "true_positive", "true_negative", "false_positive", "false_negative")
            roc <- lapply(names(private$.rocResults), function(predictor) {
                r <- private$.rocResults[[predictor]]
                curve <- r$roc
                smoothed <- private$.rocSmoothed[[predictor]]
                oc <- r$optimal_cutoff
                oc_num <- if (is.null(oc)) NULL else lapply(oc[intersect(num_fields, names(oc))], as.numeric)
                if (!is.null(oc) && !is.null(oc$method)) oc_num$method <- oc$method
                cc <- r$custom_cutoffs
                if (!is.null(cc)) { cc <- as.data.frame(cc); rownames(cc) <- NULL }
                bands <- NULL
                band_error <- NULL
                if (isTRUE(self$options$rocCurve) && isTRUE(self$options$showConfidenceBands)) {
                    bands <- tryCatch({
                        private$.checkpoint()
                        spec <- seq(0.1, 0.9, by = 0.1)
                        # F-31: Stratified bootstrap reaches the bands too (pROC defaults it to TRUE).
                        ci <- private$.withSeed(pROC::ci.coords(curve, x = spec, input = "specificity",
                            ret = "sensitivity", boot.n = self$options$bootstrapSamples,
                            boot.stratified = !isFALSE(self$options$stratifiedBootstrap),
                            conf.level = self$options$confidenceLevel / 100))$sensitivity
                        band <- data.frame(FPR = 1 - spec, TPR_lower = ci[, 1], TPR_upper = ci[, 3])
                        # all-NA limits used to be filtered away in the renderer without a word
                        if (!any(stats::complete.cases(band))) stop(.("no bootstrap resample gave finite limits"))
                        band
                    }, error = function(e) {
                        if (identical(e$code, "restart")) stop(e)
                        band_error <<- conditionMessage(e)
                        NULL
                    })
                }
                list(roc = list(sensitivities = as.numeric(curve$sensitivities),
                                specificities = as.numeric(curve$specificities),
                                thresholds = as.numeric(curve$thresholds),
                                direction = curve$direction, auc = as.numeric(curve$auc)),
                     smoothed = if (is.null(smoothed)) NULL else
                         list(sensitivities = as.numeric(smoothed$sensitivities),
                              specificities = as.numeric(smoothed$specificities)),
                     optimal_cutoff = oc_num, custom_cutoffs = cc, constant = isTRUE(r$constant),
                     bands = bands, band_error = band_error)
            })
            names(roc) <- names(private$.rocResults)
            failed <- Filter(function(r) !is.null(r$band_error), roc)
            private$.bandFailures <- vapply(failed, function(r) r$band_error, character(1))
            list(schema = 2L, sampleSize = nrow(private$.analysisData),
                 predictors = private$.predictors, roc = roc,
                 prevalence = if (length(roc)) private$.effectivePrevalence(
                     mean(private$.binaryData == private$.positiveClass)) else NULL)
        },

        .prPlotData = function() {
            if (!isTRUE(self$options$detectImbalance) || !length(private$.rocResults)) return(NULL)
            curves <- lapply(names(private$.rocResults), function(predictor) {
                pr <- private$.computePRMetrics(private$.analysisData[[predictor]],
                    private$.analysisData[[private$.outcome]], private$.positiveClass,
                    private$.rocObjects[[predictor]]$direction)
                if (is.null(pr)) return(NULL)
                list(data = data.frame(Recall = pr$curve$recall, Precision = pr$curve$precision,
                                       Predictor = predictor), baseline = pr$prevalence)
            })
            curves <- Filter(Negate(is.null), curves)
            if (!length(curves)) return(NULL)
            list(data = do.call(rbind, lapply(curves, `[[`, "data")), baseline = curves[[1]]$baseline)
        },

        .multiClassPlotData = function() {
            if (!isTRUE(self$options$multiClassROC) || is.null(private$.analysisData)) return(NULL)
            outcome <- private$.multiClassOutcome %||% private$.analysisData[[private$.outcome]]
            result <- list(nlevels = nlevels(outcome), predictors = private$.predictors, data = NULL)
            if (nlevels(outcome) < 3 || self$options$multiClassStrategy != "ovr") return(result)
            curves <- list()
            for (predictor in private$.predictors) {
                values <- private$.analysisData[[predictor]]
                direction <- private$.ovrDirection(outcome, values)
                for (level in levels(outcome)) {
                    binary <- factor(ifelse(outcome == level, "Positive", "Negative"),
                                     levels = c("Negative", "Positive"))
                    roc <- pROC::roc(binary, values, direction = direction, quiet = TRUE)
                    curves[[length(curves) + 1L]] <- data.frame(
                        FPR = 1 - roc$specificities, TPR = roc$sensitivities,
                        Class = level, Predictor = predictor)
                }
            }
            result$data <- do.call(rbind, curves)
            result
        },

        .publishPlotStates = function() {
            st <- private$.plotStatePayload()
            pr <- private$.prPlotData()
            multi <- private$.multiClassPlotData()
            for (nm in private$.plotImageNames()) {
                payload <- switch(nm,
                    calibrationPlotImage = list(calibration = private$.calibrationPlot),
                    clinicalUtilityPlot = list(curves = private$.decisionCurves),
                    decisionImpactPlot = list(curves = private$.decisionCurves),
                    prcPlot = list(pr = pr),
                    multiClassROCPlot = list(multi = multi),
                    st)
                if (!is.null(payload)) payload$schema <- 2L
                # These two images are the only consumers of threshold coordinates.
                if (!is.null(payload$roc)) {
                    payload$roc <- lapply(payload$roc, function(r) {
                        if (!nm %in% c("cutoffAnalysisPlot", "youdenIndexPlot")) r$roc$thresholds <- NULL
                        if (nm != "rocCurvePlot") { r$bands <- NULL; r$band_error <- NULL }
                        if (nm == "clinicalDecisionPlot") { r$roc <- NULL; r$smoothed <- NULL }
                        r
                    })
                }
                img <- self$results$results$get(nm)
                img$setState(payload)
            }
        },

        .clearPlotStates = function() {
            for (nm in private$.plotImageNames()) {
                img <- tryCatch(self$results$results$get(nm), error = function(e) NULL)
                if (!is.null(img)) img$setState(NULL)
            }
        },

        # Restore the current image's coordinates, without a fit or a sibling image.
        # These are plain plotting lists, not partially reconstructed pROC models.
        .restoreFromState = function(image) {
            st <- tryCatch(image$state, error = function(e) NULL)
            if (is.null(st) || !identical(st$schema, 2L) || !length(st$roc)) return(FALSE)
            private$.predictors <- st$predictors
            private$.rocResults <- st$roc
            private$.rocObjects <- lapply(st$roc, `[[`, "roc")
            private$.rocSmoothed <- lapply(st$roc, `[[`, "smoothed")
            TRUE
        },

        .plotStateUnavailable = function(image, ggtheme) {
            st <- image$state
            if (!is.null(st) && !identical(st$schema, 2L)) {
                return(private$.plotMessage(ggtheme, .("Saved Plot Needs Recalculation"),
                    .("This plot was saved by an earlier analysis version. Re-run the analysis to create updated plotting coordinates, then save the document again.")))
            }
            FALSE
        },
        .run = function() {
            # Reset notice list, instructions, summary, and preset config at start of every run
            private$.noticeList <- list()
            # Notices are rendered on EVERY exit path: the early returns below, normal
            # completion, and any stop() raised after the last .addNotice() - a throw inside
            # private$.publishPlotStates(), say. Such a run used to skip the tail call entirely
            # and leave the PREVIOUS run's panel on screen above tables it did not produce.
            # Same idiom as R/stagemigration.b.R.
            on.exit(private$.renderNotices(), add = TRUE)
            # Table notes are opt-in (Table notes); stripped on every exit path, like the notices.
            on.exit(private$.applyTableNotesOption(), add = TRUE)
            # jmvcore restores an Html item's previous content whatever its clearWith says
            # (Html$fromProtoBuf only marks it stale), and jamovi's Html view neither greys a
            # stale item nor clears one whose new content is "". So empty these narrative items
            # now and hide whichever this run leaves empty - an early return (no positive class
            # chosen yet, an ERROR) would otherwise show the previous outcome's report. Not in
            # .init(): that also runs on render-only cycles (resize, .omv reopen).
            narrative <- c("analysisSummary", "clinicalReport", "methodsExplanation", "clinicalInterpretationGuide")
            for (nm in narrative) self$results$results$get(nm)$setContent("")
            on.exit(for (nm in narrative) {
                item <- self$results$results$get(nm)
                if (!nzchar(item$content)) item$setVisible(FALSE)
            }, add = TRUE)
            private$.instructionsHtml <- private$.getInstructions()
            private$.presetConfig <- NULL
            private$.presetNoticeShown <- FALSE
            private$.analysisSummaryHtml <- ""
            # Assigned only on the >2-level branch of .prepareData(). Without clearing it here,
            # switching the outcome to a two-level variable left every multi-class table quietly
            # computing on the PREVIOUS outcome.
            private$.multiClassOutcome <- NULL
            private$.positiveClass <- NULL
            private$.negativeLevels <- NULL
            private$.binaryEnabled <- TRUE
            private$.prevalenceNoticeShown <- FALSE
            private$.sampleNoticesShown <- FALSE
            private$.metricPValueFailures <- character(0)
            private$.rocTestCache <- list()
            private$.bcaFellBackToPercentile <- FALSE
            private$.delongWarned <- FALSE
            private$.underfitCited <- FALSE
            private$.calibrationPlot <- NULL
            private$.decisionCurves <- NULL
            # These used to be reset inside .runROCAnalysis() only. When .prepareData() returns
            # NULL the run aborts BEFORE that point, so the previous run's ROC objects stayed in
            # the R6 instance and .restoreFromState() short-circuited on them - a resize redrew
            # every plot from the aborted run's predecessor.
            private$.rocResults <- list()
            private$.rocObjects <- list()
            private$.rocSmoothed <- list()
            # A run that fails or returns early must leave no plot state behind, or the
            # renderers would restore the PREVIOUS run's data.
            private$.clearPlotStates()

            # Every resampling call (bootstrap intervals and bands, comparison tests, internal
            # validation) starts from this seed through .withSeed(), so a result does not depend
            # on what was resampled before it. withr::local_seed() restores the caller's RNG state
            # when .run() exits: every analysis in a jamovi session shares one R process.
            seed_val <- self$options$seed
            if (is.null(seed_val) || !is.finite(seed_val)) seed_val <- 0
            withr::local_seed(seed_val)
            private$.seed <- seed_val

            # The Outcome coding table appears as soon as an outcome is chosen, counting every row
            # with an outcome value; .prepareData() refills it with the analysed rows.
            .deleteTableRows(self$results$results$outcomeCoding)
            if (!is.null(private$.outcome)) {
                coding <- .rocOutcomeCoding(self$data[[private$.outcome]], self$options$positiveClass)
                private$.fillOutcomeCoding(coding, coding$n_missing)
            }

            # Check if we have required data
            if (is.null(private$.outcome) || is.null(private$.predictors) || length(private$.predictors) == 0) {
                private$.addNotice(
                    type = "INFO",
                    title = .("Missing Variables"),
                    content = .("Please select an outcome variable and at least one predictor variable for ROC analysis. \u2022 Outcome variable: required (binary/factor). \u2022 Predictor variables: at least one numeric variable required for ROC curve calculation.")
                )
                return()
            }

            for (name in c("validationContext", "denominatorLedger", "frozenMetrics", "screeningWorkload")) {
                .deleteTableRows(self$results$results$get(name))
            }
            if (isTRUE(self$options$validationPanel)) {
                # Translate the helper's input error, then reject outside the catch so
                # jamovi records a failed analysis and retains the explanatory notice.
                # The helper calls no jmvcore::reject() or .checkpoint().
                report <- tryCatch(.enhancedROCValidation(self$data, self$options), error = function(e) e)
                if (inherits(report, "error")) {
                    private$.addNotice(type = "ERROR", title = .("Frozen-Model Evaluation Not Run"),
                                       content = private$.frozenError(conditionMessage(report)))
                    .diagRejectInputErrors(private$.noticeList)
                    return()
                }
                for (pair in list(c("validationContext", "context"), c("denominatorLedger", "ledger"),
                                  c("frozenMetrics", "metrics"), c("screeningWorkload", "workload"))) {
                    table <- self$results$results$get(pair[1])
                    frame <- report[[pair[2]]]
                    # NA would print as "NaN"; frozenMetrics' status column already says why a cell is empty
                    reasons <- if (identical(pair[1], "screeningWorkload")) private$.undefinedRatioNotes() else list()
                    for (i in seq_len(nrow(frame))) {
                        values <- private$.frozenRow(pair[1], as.list(frame[i, ]))   # display labels
                        table$addRow(rowKey = as.character(i), values = values)
                        private$.blankUndefined(table, as.character(i), values, reasons)
                    }
                }
                # F-09: the frozen panel evaluates the same fixed cutoffs (direction is explicit here).
                scores <- as.list(report$scores)
                private$.warnDegenerateCutoffs(scores, .dvNumbers(self$options$impactCutoffs),
                    stats::setNames(rep(identical(self$options$direction, "higher"), length(scores)), names(scores)))
                # F-47: the metrics weight every analysed row equally, whatever unit the user
                # declared; "weighted by slides" contradicted 'Unit of observation: patient'.
                unit <- trimws(self$options$analysisUnit %||% "")
                if (!nzchar(unit)) unit <- "row"
                self$results$results$frozenMetrics$setNote("scope", sprintf(.("Percentile intervals resample whole patients when an ID is supplied, jointly across models and within fixed sites. These approximate intervals can under-cover, especially with rare outcomes or unequal cluster sizes. Counts and metrics weight every analysed row equally (unit of observation: %s). Undefined or degenerate intervals are unavailable. Calibration logits require probabilities strictly between 0 and 1 and a converged nonseparated fit. Positive-only cohorts cannot estimate specificity or discrimination. No model is refitted and no cutoff is optimized."), .rocLabel(unit)))
                self$results$results$frozenMetrics$setNote("seed", if (isTRUE(self$options$validationBootstrap))
                    .fmt(.("Random seed: {seed}"), seed = self$options$seed))
                return()
            }

            # Apply clinical presets if selected
            private$.applyClinicalPresets()

            # Prepare and validate data
            analysisData <- private$.prepareData()
            if (is.null(analysisData)) {
                .diagRejectInputErrors(private$.noticeList)
                return()
            }

            # Every panel below treats a row as an independent patient (row-level DeLong, row
            # bootstrap, Clopper-Pearson, paired roc.test). With four slides per patient at ICC 0.5
            # a 95% DeLong interval covered 78.5% (independent review 2026-10-09, F-02; Obuchowski
            # 1997, PMID 9192452), and the analysis holds the IDs that show the rows repeat. So a
            # repeat is a WARNING naming the assumption and its consequence, counted on the
            # analysed rows (matched by row name: naOmit() and the outcome filter keep them).
            # ponytail: no patient-cluster interval on the main path; .dvBootstrap() needs one
            # outcome per patient, so lesion-level designs would need Obuchowski's variance.
            pid_name <- self$options$patientID
            if (!is.null(pid_name) && nzchar(pid_name)) {
                pid <- as.character(self$data[[pid_name]][match(rownames(analysisData), rownames(self$data))])
                pid <- pid[!is.na(pid) & nzchar(pid)]
                if (anyDuplicated(pid) > 0L) {
                    private$.addNotice(type = "WARNING", title = .("Several Rows per Patient"),
                        content = sprintf(.("The %1$s analysed rows that carry a patient identifier (%2$s) come from %3$s patients, so some patients contribute more than one row (slides, cores or lesions). Every panel except the Frozen-model validation report assumes that each row is an independent patient. Rows from one patient are correlated, so the AUC confidence intervals, the tests comparing markers and the confidence intervals for sensitivity and specificity are too narrow, and p-values too small. \u2022 Analyse one row per patient (for example a patient-level score), or \u2022 for a fixed model with a prespecified cut-off, enable the Frozen-model validation report, whose intervals resample whole patients (every row of a patient must then share one outcome)."),
                            length(pid), pid_name, length(unique(pid))))
                } else {
                    private$.addNotice(type = "INFO", title = .("Patient clustering is inactive"),
                        content = .("Enable Frozen-model validation report to use patient-cluster intervals. Other ROC panels use their ordinary independent-row methods."))
                }
            }

            # CRITICAL FIX: Store cleaned data in private$.analysisData
            # This ensures all downstream confusion matrix and metric calculations
            # use the same sample as the ROC analysis (same NA removal, same outcome re-leveling)
            private$.analysisData <- analysisData
            private$.binaryData <- analysisData[[private$.outcome]]

            # Checkpoint before expensive ROC analysis
            private$.checkpoint()

            # Multi-class mode without a positive class runs the multi-class tables only:
            # every binary table, plot and summary needs a positive class (E1b).
            if (private$.binaryEnabled) {
                # Run ROC analysis for each predictor
                n_notices <- length(private$.noticeList)
                private$.runROCAnalysis(analysisData)
                # Each failure is a WARNING naming its predictor; when none is left nothing below
                # has a result, so the analysis is rejected (outside the per-predictor tryCatch,
                # which would swallow reject()) with those reasons in its message.
                if (length(private$.rocResults) == 0) {
                    reasons <- vapply(private$.noticeList[seq_along(private$.noticeList) > n_notices],
                                      function(n) paste(n$title, n$content, sep = ": "), character(1))
                    private$.addNotice(type = "ERROR", title = .("No Predictor Could Be Analysed"),
                        content = paste(c(.("ROC analysis failed for every selected predictor, so there are no results."), reasons), collapse = " "))
                    .diagRejectInputErrors(private$.noticeList)
                    return()
                }
                # The Outcome coding table also says which way each marker was read.
                private$.noteDirection(self$results$results$outcomeCoding, private$.rocObjects, "reading")

                # DeLong's interval is a large-sample interval (VAL-26), and its under-coverage grows
                # as the AUC nears 1 (or 0): binormal simulation, 95% nominal, coverage tracks
                # m = n_min * (1 - AUC) - 0.92 at m = 4, 0.92-0.94 at m = 6, 0.93-0.94 at m = 8 for
                # AUC 0.90 / 0.95 / 0.97 (R = 4000 per cell, unbalanced designs alike). Warned once
                # per run, naming every predictor whose DeLong interval is shown and at risk; the AUC
                # is taken as displayed (3 dp) and compared in integers, so "fewer than 60 at an AUC
                # of 0.90" is exact at the edge.
                n_class <- table(private$.binaryData)
                n_min <- min(n_class)
                delong_auc <- unlist(lapply(private$.rocResults, function(r)
                    if (is.null(r$ci_issue) && !identical(attr(r$roc$ci, "method"), "bootstrap")) as.numeric(r$roc$auc)))
                k <- round(1000 * pmax(delong_auc, 1 - delong_auc))
                at_risk <- delong_auc[n_min < 30 | n_min * (1000 - k) < 6000]
                if (length(at_risk) > 0) {
                    private$.delongWarned <- TRUE
                    # Feng et al. report diverging interval methods, not which way DeLong errs. F-22/F-38:
                # the quoted coverages are design-level averages for equal-variance binormal data; case
                # SD 2 or 0.5 and exponential data gave 0.902-0.907, 0.838-0.843 and 0.915-0.918
                # (review AUD-F, R = 20,000). A conditional (warned-only) coverage is not quoted:
                # conditioning on a high observed AUC lowers any interval's coverage.
                    private$.addNotice(
                        type = "WARNING",
                        title = .("DeLong confidence intervals may be too narrow"),
                        content = sprintf(.("The smaller outcome group has %1$s patients (%2$s positive, %3$s negative); affected: %4$s. DeLong's interval is a large-sample approximation: for a single AUC it covers the true value less often than its stated level when the smaller group is small, and more so the closer the AUC is to 1 or 0 (Obuchowski & Lieber 1998: more than 200 patients for high-accuracy areas). Feng, Cortese & Baumgartner (2017) found that interval methods for the AUC disagree most when the AUC is high and the sample small. In this module's validation simulation, which drew equal-variance binormal data, a 95%% DeLong interval covered a true AUC of 0.80 in 91%% of datasets with 15 positive and 15 negative cases, and a true AUC of 0.95 in 87%% with 30 and 30 and in 93%% with 100 and 100; these are averages over all datasets of each size, and with unequal variances or skewed data they were about 90%%, 84%% and 92%%. Coverage followed the smaller group's size times the distance of the AUC from 1 (from 0 for an AUC below 0.5): it was about 93%% wherever that product was 6, so this warning appears when the smaller group has fewer than 30 patients or that product is below 6 (fewer than 60 patients at an AUC of 0.90, fewer than 120 at 0.95); the cut-off is a heuristic. \u2022 Read the interval as approximate and probably too narrow. \u2022 Bootstrap confidence intervals are an alternative, but no method is exact at this size, and no bootstrap interval can be formed when the groups are perfectly separated."),
                            n_min, n_class[[2]], n_class[[1]],
                            paste(sprintf("%s (AUC %.3f)", names(at_risk), at_risk), collapse = ", "))
                    )
                }

                # Smoothing changes the drawn curve only. Say so once, rather than letting a reader
                # assume the cut-points and confidence intervals came from the smooth curve they see.
                if (self$options$smoothMethod != "none" && length(private$.rocSmoothed) > 0) {
                    private$.addNotice(
                        type = "INFO",
                        title = .("Smoothing Applies to the Curve Only"),
                        content = sprintf(.("The displayed ROC curve is smoothed (%s), but every reported statistic \u2014 AUC, confidence intervals, optimal cut-points and all sensitivity/specificity values \u2014 is computed from the empirical curve. A smoothed curve carries no thresholds, so it cannot produce a cut-point."), if (self$options$smoothMethod == "binormal") "binormal" else "kernel density")
                    )
                }

                # F-10: with Youden optimisation off the cut-point is the closest-to-corner one over
                # every threshold (.calculateOptimalCutoff); the minimums belong to the Youden search
                # and were dropped with no word on the page, so a screening user who asked for
                # sensitivity >= 0.90 got 0.60. Announced, not applied: the documented criterion stays.
                sens_min <- self$options$sensitivityThreshold %||% 0
                spec_min <- self$options$specificityThreshold %||% 0
                corner <- vapply(private$.rocResults, function(r) identical(r$optimal_cutoff$method, "closest_topleft"), logical(1))
                if (any(corner) && (sens_min > 0 || spec_min > 0)) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .("Minimum Sensitivity and Specificity Not Applied"),
                        content = sprintf(.("Youden index optimization is off, so each reported cutoff is the one closest to the top-left corner of the ROC curve, searched over all thresholds. The minimum sensitivity (%1$s) and minimum specificity (%2$s) you set apply only to Youden optimization and were not used, so a reported cutoff may fall below them. Turn Youden index optimization on to restrict the search to cutoffs that meet these minimums."), sens_min, spec_min)
                    )
                }

                # F-29: .evaluateCustomCutoffs() drops an entry that is not a number ('abc', '0.5%').
                # Name what was dropped. '0,5' is not dropped: it is the two cutoffs 0 and 5.
                cc_text <- self$options$customCutoffs
                if (!is.null(cc_text) && nzchar(trimws(cc_text))) {
                    tokens <- trimws(strsplit(cc_text, ",", fixed = TRUE)[[1]])
                    dropped <- tokens[nzchar(tokens) & is.na(suppressWarnings(as.numeric(tokens)))]
                    if (length(dropped) > 0) {
                        private$.addNotice(
                            type = "WARNING",
                            title = .("Custom Cutoffs Not Read"),
                            content = sprintf(.("These entries under Custom cutoffs are not numbers and were ignored: %s. Separate cutoffs with commas and use a point as the decimal mark (0,5 is read as the two cutoffs 0 and 5); leave out units and percent signs."),
                                paste0("'", dropped, "'", collapse = ", "))
                        )
                    }
                }

                # Check for class imbalance. Detection is unconditional: a display checkbox may
                # decide whether a panel is SHOWN, never whether a condition that invalidates the
                # AUC is LOOKED FOR. detectImbalance and showImbalanceWarning both default to false,
                # so a 20:1 outcome used to produce an AUC, a confidence interval and an optimal
                # cut-point with no caution anywhere on the page. The metrics table this fills is
                # still `visible: (detectImbalance)`, so nothing new appears unless asked for -
                # only the notice is now guaranteed. The precision-recall panel stays opt-in: it is
                # extra computation for an extra table, not a safety check.
                private$.checkClassImbalance(analysisData)
                if (self$options$detectImbalance) {
                    private$.populatePrecisionRecall(analysisData)
                }

                # Checkpoint after ROC analysis, before populating tables
                private$.checkpoint()

                # Populate results tables
                if (self$options$aucTable) {
                    private$.populateAUCSummary()
                }

                if (self$options$optimalCutoffs) {
                    private$.checkpoint()
                    private$.populateOptimalCutoffs()
                }

                if (self$options$cutoffTable) {
                    private$.checkpoint()
                    private$.populateCutoffAnalysis()
                }

                if (self$options$diagnosticMetrics) {
                    private$.checkpoint()
                    private$.populateDiagnosticPerformance()
                }

                if (self$options$clinicalMetrics) {
                    private$.checkpoint()
                    private$.populateClinicalMetrics()
                }

                # All three comparison outputs require analysisType == "comparative". The UI offers
                # them as plain checkboxes with no dependency on Analysis Type, so ticking one while
                # Analysis Type is left at its default produced no output and no explanation.
                # The checkbox and Analysis Type labels as the options panel shows them (.a.yaml titles).
                comparison_opts <- c(isTRUE(self$options$pairwiseComparisons),
                                     isTRUE(self$options$showMetricsDiff),
                                     isTRUE(self$options$statisticalComparison))
                names(comparison_opts) <- c(.("Pairwise comparisons"), .("Metrics differences"), .("Statistical model comparison"))
                if (any(comparison_opts) && self$options$analysisType != "comparative") {
                    type_label <- switch(self$options$analysisType,
                        single = .("Single ROC Analysis"),
                        comprehensive = .("Comprehensive Diagnostic Performance"),
                        self$options$analysisType)
                    private$.addNotice(
                        type = "WARNING",
                        title = .("Comparison Options Need Comparative Analysis"),
                        content = sprintf(.("%1$s produced no output because Analysis Type is set to \"%2$s\". Set Analysis Type to \"Comparative ROC Analysis\" to compare predictors."), paste(names(comparison_opts)[comparison_opts], collapse = ", "), type_label)
                    )
                }

                if (any(comparison_opts) && self$options$analysisType == "comparative" && length(private$.rocResults) < 2) {
                    private$.addNotice(
                        type = "WARNING", title = .("Insufficient Predictors"),
                        content = sprintf(.("ROC comparisons need at least 2 predictors with ROC results; %s predictor has ROC results, so the comparison tables are empty. A selected predictor whose ROC analysis failed is named in its own warning."), length(private$.rocResults))
                    )
                }

                if (self$options$pairwiseComparisons && self$options$analysisType == "comparative") {
                    private$.checkpoint()
                    private$.populateROCComparisons()
                }

                if (self$options$showMetricsDiff && self$options$analysisType == "comparative") {
                    private$.checkpoint()
                    private$.populateDetailedComparison()
                }

                if (self$options$statisticalComparison && self$options$analysisType == "comparative") {
                    private$.checkpoint()
                    private$.populateStatisticalSummary()
                }

                if (self$options$partialAuc) {
                    private$.checkpoint()
                    private$.populatePartialAUC()
                }

                if (self$options$crocAnalysis) {
                    private$.checkpoint()
                    private$.populateCROCAnalysis()
                }

                if (self$options$convexHull) {
                    private$.checkpoint()
                    private$.populateConvexHull()
                }

                if (self$options$comprehensive_output) {
                    private$.checkpoint()
                    private$.populateComprehensiveAnalysis()
                }

                if (self$options$clinical_interpretation) {
                    private$.checkpoint()
                    private$.populateClinicalInterpretation()
                }

                # Everything that reads a predicted risk (see .riskProbabilities()): its notices
                # once per run, then the tables and the plot-ready frames.
                private$.riskNotices()
                if (self$options$calibrationAnalysis) {
                    private$.checkpoint()
                    private$.populateCalibrationAnalysis()
                }
                private$.computeDecisionCurves()
                # After the decision curves: the reference list includes what their subtitle cites.
                if (self$options$comprehensive_output) private$.populateMethodsExplanation()
            }

            if (self$options$multiClassROC) {
                private$.checkpoint()
                private$.populateMultiClassROC()
            }

            if (self$options$clinicalImpact && private$.binaryEnabled) {
                private$.checkpoint()
                private$.populateClinicalImpact()
            }


            # Nineteen of the twenty "not yet implemented" checkboxes are gone: seven were
            # implemented, and twelve are now commented out in jamovi/enhancedROC.a.yaml.
            # self$options$<name> RAISES for an option that no longer exists, so a commented-out
            # option must not be referenced here at all. Weighted AUC averaging is the one case
            # that is still genuinely unavailable, and only for the One-vs-One strategy.
            if (isTRUE(self$options$multiClassROC) &&
                identical(self$options$multiClassAveraging, "weighted") &&
                identical(self$options$multiClassStrategy, "ovo")) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Weighted Averaging Is Not Available with the One-vs-One Strategy"),
                    content = .("The one-vs-one pairwise AUC is defined as an unweighted average over class PAIRS, so there is no prevalence-weighted version of it and none was computed; the Multi-Class Average AUC table reports the unweighted pairwise value instead. Choose the One-vs-Rest strategy if you want a prevalence-weighted average.")
                )
            }

            # Natural-language panels are opt-in (default off)
            if (length(private$.rocResults) > 0) {
                if (isTRUE(self$options$showAnalysisSummary)) private$.generateAnalysisSummary()
                if (isTRUE(self$options$showClinicalReport)) private$.generateClinicalReport()
            }

            if (self$options$internalValidation && private$.binaryEnabled) {
                private$.checkpoint()
                private$.populateInternalValidation()
            }

            # Success completion notice
            if (length(private$.rocResults) > 0) {
                n_predictors <- length(private$.rocResults)
                n_obs <- nrow(private$.analysisData)

                # Get best AUC (from -Inf: an AUC of 0 is still a result, a marker read backwards)
                best_auc <- -Inf
                best_predictor <- ""
                for (pred in names(private$.rocResults)) {
                    auc_val <- as.numeric(private$.rocObjects[[pred]]$auc)
                    if (auc_val > best_auc) {
                        best_auc <- auc_val
                        best_predictor <- pred
                    }
                }

                predictor_text <- if (n_predictors == 1) {
                    sprintf(.("1 predictor (%1$s, AUC=%2$s)"), best_predictor, round(best_auc, 3))
                } else {
                    sprintf(.("%1$s predictors (best: %2$s, AUC=%3$s)"), n_predictors, best_predictor, round(best_auc, 3))
                }

                # F-07: no 'completed successfully' beside an ERROR (e.g. Low Per-Class Count) that
                # calls these same results unreliable.
                types <- vapply(private$.noticeList, function(n) n$type, character(1))
                if ("ERROR" %in% types) {
                    private$.addNotice(
                        type = "INFO",
                        title = .("Analysis Finished with Errors"),
                        content = sprintf(.("ROC analysis finished, but at least one error is reported above. \u2022 Analyzed %1$s with n=%2$s observations. \u2022 Read the errors and warnings above before using the AUC values, confidence intervals and cutoffs in the tables below."), predictor_text, n_obs)
                    )
                } else if ("STRONG_WARNING" %in% types) {
                    # Not "completed successfully" beside a strong warning that qualifies these results.
                    private$.addNotice(
                        type = "INFO",
                        title = .("Analysis Finished with Warnings"),
                        content = sprintf(.("ROC analysis finished with at least one important warning above. \u2022 Analyzed %1$s with n=%2$s observations. \u2022 Read the warnings above before using the AUC values, confidence intervals and cutoffs in the tables below."), predictor_text, n_obs)
                    )
                } else {
                    private$.addNotice(
                        type = "INFO",
                        title = .("Analysis Complete"),
                        content = sprintf(.("ROC analysis completed successfully. \u2022 Analyzed %1$s with n=%2$s observations. \u2022 Review AUC values, confidence intervals, and optimal cutoffs in results tables below. \u2022 Check any warnings or recommendations above for data quality concerns."), predictor_text, n_obs)
                    )
                }
            }

            # Disclosed once per run: the interval on screen is not the one the user asked for.
            if (isTRUE(private$.bcaFellBackToPercentile)) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Bias-Corrected Interval Not Available"),
                    content = .("Some bootstrap resamples contained only one outcome class, so no AUC could be computed for them. The bias-corrected and accelerated (BCa) method needs every resample to line up with its own index vector, which is no longer true once those resamples are dropped, so the percentile interval is reported instead. \u2022 The AUC confidence limits shown are percentile bootstrap limits. \u2022 This usually means one class is rare; stratified resampling or a larger sample will restore BCa.")
                )
            }

            private$.publishPlotStates()
            private$.noteBandFailures()
        },

        # Said from .run(): a renderer cannot add a notice. The curve is still drawn, without its band.
        .bandFailures = character(0),
        .noteBandFailures = function() {
            if (!length(private$.bandFailures)) return(invisible(NULL))
            private$.addNotice(type = "WARNING", title = .("Confidence Band Not Drawn"),
                content = sprintf(.("The bootstrap confidence band could not be computed for %s, so the ROC curve is drawn without it."),
                    paste0(names(private$.bandFailures), " (", private$.bandFailures, ")", collapse = "; ")))
        },
        .prepareData = function() {
            # Validate data using enhanced error handling if available.
            # Initialize to NULL first so the warnings block below is safe even
            # when validate_clinical_data() is not on the search path.
            validation_result <- NULL
            if (exists("validate_clinical_data", mode = "function")) {
                validation_result <- validate_clinical_data(
                    data = self$data,
                    required_vars = c(private$.outcome, private$.predictors),
                    # Enforce the analysis minimum on complete cases below, not raw rows.
                    min_observations = 1,
                    clinical_checks = TRUE
                )

                if (!validation_result$valid) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("Data Validation Failed"),
                        content = sprintf(.("The data did not pass clinical validation. \u2022 %s"), paste(validation_result$errors, collapse = " \u{2022} "))
                    )
                    return(NULL)
                }
            }

            # Validate partialRange only when partial AUC is actually being computed. This used
            # to run unconditionally, so leftover text in a box the user had since switched off
            # aborted the entire analysis. A bad range skips the partial AUC only (F-51), as a bad
            # impactCutoffs / prevalenceScenarios skips only its own tables.
            if (isTRUE(self$options$partialAuc) && is.null(private$.partialRangeValues())) {
                range_parts <- trimws(strsplit(self$options$partialRange %||% "", ",", fixed = TRUE)[[1]])
                range_values <- suppressWarnings(as.numeric(range_parts))
                problem <- if (length(range_parts) != 2) {
                    .("Partial AUC range must contain exactly two comma-separated numbers, for example 0.8,1.0.")
                } else if (anyNA(range_values)) {
                    .("Partial AUC range values must be numeric, for example 0.8,1.0.")
                } else {
                    .("Partial AUC range must be valid: the first value must be less than the second, and both must lie between 0 and 1, for example 0.8,1.0.")
                }
                private$.addNotice(
                    type = "WARNING",
                    title = .("Invalid Partial AUC Range"),
                    content = paste(problem, .("The partial AUC is not computed; the rest of the analysis is."))
                )
            }

            # validate_clinical_data()'s warnings (untranslated, pasted into the instructions panel)
            # are not shown: each has a notice of its own (Rows Excluded, Constant Predictor, Small
            # Sample Size), and its outlier screen does not bear on a rank-based AUC.

            # Prepare analysis dataset - jmvcore::naOmit preserves jamovi
            # column attributes (measureType, values, labels) that base
            # na.omit would strip, breaking downstream labelled-factor handling.
            vars <- c(private$.outcome, private$.predictors)
            data <- self$data[vars]
            data <- jmvcore::naOmit(data)
            # F-23: an integer column whose measure type is Nominal or Ordinal passes
            # `permitted: numeric` but reaches R as a factor whose 'values' attribute holds the
            # integers. Read it by those values (jmvcore::toNumeric), here: the outcome filter
            # below subsets with `[.factor`, which drops the attribute.
            coded <- Filter(function(p) is.factor(data[[p]]) && !is.null(attr(data[[p]], "values")), private$.predictors)
            for (p in coded) data[[p]] <- jmvcore::toNumeric(data[[p]])
            if (length(coded)) {
                private$.addNotice(type = "INFO", title = .("Integer Codes Read as Numbers"),
                    content = sprintf(.("%s: an integer column set to Nominal or Ordinal was analysed by its numeric values, as ordered codes read in the direction set under Direction. This is right for an ordinal rating scale; if the numbers merely label unordered categories, the AUC depends on the arbitrary numbering and is meaningless."),
                        paste(coded, collapse = ", ")))
            }

            # Outcome coding (R3): the positive class is never taken from level order; only a
            # numeric 0/1 outcome defaults to 1 (.rocOutcomeCoding() in R/utils.R).
            outcome_var <- data[[private$.outcome]]
            # Whole column, the rows the Outcome coding preview counts. A numeric outcome with more
            # than two values is refused with the same sentence as the table's note, so the two
            # never give different advice (one value falls through to 'single' below).
            full <- .rocOutcomeCoding(self$data[[private$.outcome]], self$options$positiveClass)
            if (full$numeric && length(full$levels) > 2L) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("Outcome Variable Not Binary"),
                    content = private$.unsetNote(full)
                )
                return(NULL)
            }
            coding <- .rocOutcomeCoding(outcome_var, self$options$positiveClass)
            # Listwise deletion can remove EVERY row of a level that the column does hold. The
            # analysed coding would then call the positive class absent ('invalid') or the outcome
            # single-valued, and advise choosing another level - a different question. Name the
            # level and the missing predictors instead; the table keeps the whole-column preview.
            lost <- setdiff(full$levels, coding$levels)
            if (length(lost) > 0L && (length(coding$levels) < 2L || isTRUE(full$positive %in% lost))) {
                gone <- if (isTRUE(full$positive %in% lost)) full$positive else lost
                rows <- !is.na(full$values) & full$values %in% gone
                miss <- vapply(private$.predictors, function(p) sum(is.na(self$data[[p]][rows])), numeric(1))
                miss <- miss[miss > 0]
                private$.addNotice(
                    type = "ERROR",
                    title = .("Outcome Level Lost to Missing Predictor Values"),
                    content = sprintf(.("Outcome level %1$s (%2$s rows) has no row with a value for every selected predictor, so it cannot be analysed: rows are used only when the outcome and ALL selected predictors are present. \u2022 Missing values among those rows: %3$s. \u2022 Remove that predictor, analyse the predictors in separate runs, or supply the missing values; choosing a different positive class would answer a different question."),
                        paste(gone, collapse = ", "), sum(rows),
                        paste0(names(miss), " (", miss, "/", sum(rows), ")", collapse = ", "))
                )
                return(NULL)
            }

            # naOmit() keeps an explicit NA level (addNA) and a blank "" (R path): that outcome is
            # missing, not a category to be pooled as negative (VAL-15). Counted over the whole column,
            # as the table's (missing) row is: naOmit() has already dropped such a row that also lacks
            # a predictor, so `coding` alone would report fewer. The filter needs only the rows left.
            n_na_level <- sum(is.na(full$values) & !is.na(self$data[[private$.outcome]]))
            if (n_na_level > 0) {
                data <- data[!is.na(coding$values), , drop = FALSE]
            }
            # Said before the early returns below on purpose: a run refused for too few rows needs
            # to know how many were dropped. Hence "can be analysed", not "were analysed".
            private$.noteExcludedRows(full, nrow(data))
            y <- coding$values[!is.na(coding$values)]
            private$.fillOutcomeCoding(coding, .rocOutcomeCoding(self$data[[private$.outcome]])$n_missing,
                                       c(nrow(data), nrow(self$data)))

            # F-30: an all-missing outcome has no level at all; coding$levels[1] printed it as 'NA'.
            if (length(coding$levels) == 0L) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("Insufficient Outcome Variable Levels"),
                    content = sprintf(.("Outcome variable '%s' has no non-missing values. ROC analysis needs at least two different outcome values (for example Disease / No Disease). \u2022 Check whether a data filter or the missing-value setting removed every value. \u2022 Or choose a different outcome variable."), private$.outcome)
                )
                return(NULL)
            }
            if (identical(coding$status, "single")) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("Insufficient Outcome Variable Levels"),
                    content = sprintf(.("Outcome variable '%1$s' has only one value: '%2$s'. ROC analysis needs at least two different outcome values (for example Disease / No Disease). \u2022 Check whether a data filter removed one of the outcome categories. \u2022 Verify the variable contains the values you expect. \u2022 Or choose a different outcome variable."), private$.outcome, coding$levels[1])
                )
                return(NULL)
            }
            if (identical(coding$status, "invalid")) {
                # Neutral wording, as psychopdaROC's: a typo in R syntax, a level with no rows (a
                # filter), or a level kept from the previous outcome - the Positive class box
                # (allowNone) then shows '- None -' while the option still holds the old level.
                private$.addNotice(
                    type = "ERROR",
                    title = .("Invalid Positive Class Selection"),
                    content = paste(
                        sprintf(.("Positive class is set to %1$s, but no row of %2$s has that level (its observed levels are: %3$s). If the Positive class box shows '- None -', the outcome variable was changed after a level was chosen."),
                                coding$positive, private$.outcome, paste(full$levels, collapse = ", ")),
                        "\u2022", .("Choose one of the observed levels in Positive class."))
                )
                return(NULL)
            }
            if (private$.multiClassOnly(coding)) {
                # E1b: the multi-class tables need no positive class; the binary ones are skipped.
                private$.binaryEnabled <- FALSE
                private$.multiClassOutcome <- y
                private$.addNotice(
                    type = "INFO",
                    title = .("Binary ROC tables need a positive class"),
                    content = private$.unsetNote(coding)
                )
            } else if (identical(coding$status, "unset")) {
                # The normal first step, not an error: nothing is computed until a level is chosen.
                private$.addNotice(
                    type = "INFO",
                    title = .("Choose the positive class"),
                    content = paste(c(
                        sprintf(.("Outcome %1$s has the levels %2$s."), private$.outcome,
                                paste0(coding$levels, " (n = ", coding$counts, ")", collapse = ", ")),
                        private$.unsetNote(coding),
                        .("Sensitivity, PPV, LR+, every cut-point and the AUC describe the level you choose; every other level is analysed as negative."),
                        if (length(coding$levels) > 2) .("To analyse every level without choosing one, tick Multi-class ROC analysis.")
                    ), collapse = " ")
                )
                return(NULL)
            } else {
                pos <- coding$positive
                if (length(coding$levels) > 2) {
                    if (isTRUE(self$options$multiClassROC)) private$.multiClassOutcome <- y
                    # One-vs-rest; "Not Other" when the positive level is itself called "Other".
                    neg <- if (identical(pos, "Other")) "Not Other" else "Other"
                    y <- factor(ifelse(y == pos, pos, neg), levels = c(neg, pos))
                } else {
                    y <- factor(y, levels = c(setdiff(coding$levels, pos), pos))
                }
                private$.positiveClass <- pos
                private$.negativeLevels <- setdiff(coding$levels, pos)
            }
            # Negative first, positive second: every downstream levels(x)[2] reads the positive
            # class from this ordering, never from the data's own level order.
            data[[private$.outcome]] <- y

            # Check predictor variables are numeric
            non_numeric_preds <- c()
            for (pred in private$.predictors) {
                if (!is.numeric(data[[pred]])) {
                    non_numeric_preds <- c(non_numeric_preds, pred)
                }
            }

            if (length(non_numeric_preds) > 0) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("Non-numeric Predictor Variables"),
                    content = sprintf(.("The following predictor variable(s) are not numeric: %s. ROC analysis requires continuous or ordinal numeric predictors, such as biomarker levels, test scores or measurements. \u2022 If the variable is ORDINAL, recode it to numbers in rank order (for example Low=1, Medium=2, High=3). \u2022 Do not number a NOMINAL variable such as tumour site or histological subtype: the AUC would then be decided by the order you happened to assign rather than by the marker. \u2022 For a nominal predictor use contingency table analysis instead."), paste(non_numeric_preds, collapse = ", "))
                )
                return(NULL)
            }

            # One analysis policy, independent of the optional validation helper:
            # adding missing rows cannot make an undersized cohort eligible.
            if (nrow(data) < 20) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("Data Validation Failed"),
                    content = sprintf(.("Only %s complete observations remain. This analysis requires at least 20 observations with a recorded outcome and every selected predictor. This is a minimum sample-size policy, not a guarantee of reliable estimates; substantially larger samples may be needed. Check whether missing values are removing more rows than expected."), nrow(data))
                )
                return(NULL)
            }

            private$.validateClinicalAssumptions(data)

            return(data)
        },

        # Every excluded row, said once and visibly (the Outcome coding note that also says it is a
        # table note, hidden by default). Missing outcome counts NA, an NA level and "" alike, as the
        # table's (missing) row does; a missing predictor is counted among rows with an outcome.
        # Listwise deletion is named because a second marker with missing values shrinks the first
        # marker's sample and changes its AUC.
        .noteExcludedRows = function(full, n_analysed) {
            n_total <- nrow(self$data)
            if (n_analysed >= n_total) return(invisible(NULL))
            has_outcome <- !is.na(full$values)
            preds <- private$.predictors
            miss_pred <- has_outcome & !stats::complete.cases(self$data[preds])
            per_pred <- vapply(preds, function(p) sum(has_outcome & is.na(self$data[[p]])), numeric(1))
            per_pred <- per_pred[per_pred > 0]
            n_single <- max(vapply(preds, function(p) sum(has_outcome & !is.na(self$data[[p]])), numeric(1)))
            private$.addNotice(type = "INFO", title = .("Rows Excluded"), content = paste(c(
                sprintf(.("%1$s of %2$s rows can be analysed: a row is used only when the outcome and every selected predictor are present."), n_analysed, n_total),
                if (full$n_missing > 0)
                    sprintf(.("%s rows had a missing outcome (a blank value or a missing-value level is missing, not a category)."), full$n_missing),
                if (any(miss_pred))
                    sprintf(.("%1$s further rows had a missing value of a selected predictor (%2$s)."), sum(miss_pred),
                            paste0(names(per_pred), ": ", per_pred, collapse = ", ")),
                if (length(preds) > 1 && n_single > n_analysed)
                    sprintf(.("A predictor analysed on its own could use up to %s rows."), n_single)
            ), collapse = " "))
        },

        # Multi-class mode without a positive class (E1b): only the multi-class tables run. Never for
        # a numeric outcome: .prepareData() refuses one with more than two values.
        .multiClassOnly = function(coding) {
            identical(coding$status, "unset") && length(coding$levels) > 2 && !coding$numeric && isTRUE(self$options$multiClassROC)
        },

        # Why nothing binary is computed yet, for the Outcome coding note and the notice (and the
        # ERROR refusing a numeric outcome with more than two values, so the two say the same).
        .unsetNote = function(coding) {
            if (private$.multiClassOnly(coding)) {
                .("No positive class is chosen, so only the multi-class tables are computed. Choose the level that means the condition is present in Positive class to get the binary ROC tables as well.")
            } else if (coding$numeric && length(coding$levels) > 10L) {
                sprintf(.("This outcome is numeric with %s distinct values, so it looks like a measurement rather than a two-level outcome (condition present / absent). Choose a different outcome variable, or a 0/1 variable made from this one at a clinically justified cut-off."), length(coding$levels))
            } else if (coding$numeric) {
                sprintf(.("This outcome is numeric with values %s; only a 0/1 outcome is read without a choice (1 = positive). Set its measure type to Nominal in Data > Setup so Positive class lists its values, or recode it 0/1."), paste(.rocLabel(coding$levels), collapse = ", "))
            } else {
                .("Choose the level that means the condition is present in Positive class. Nothing is computed until you do: levels are never chosen from their order.")
            }
        },

        # Outcome coding table (R2): every observed level in level order with its count and role,
        # then the rows excluded for a missing outcome. `analysed` = c(rows analysed, rows in the
        # data) once .prepareData() has run; NULL for the preview, which counts every row with an
        # outcome value.
        .fillOutcomeCoding = function(coding, n_missing, analysed = NULL) {
            table <- self$results$results$outcomeCoding
            .deleteTableRows(table)
            lv <- coding$levels
            # .prepareData() refuses a numeric outcome with more than two values: no role is in force
            # even when a positive class was passed from R.
            if (coding$numeric && length(lv) > 2L) coding$status <- "unset"
            pos <- coding$positive
            in_force <- coding$status %in% c("selected", "default01")
            mc_only <- private$.multiClassOnly(coding)
            if (coding$numeric && length(lv) > 10L) {
                # A measurement picked as the outcome: one summary row, not one row per value
                # (factor() sorts numeric levels numerically, so lv[1] / lv[K] are min / max).
                table$addRow(rowKey = "numeric", values = list(
                    level = sprintf(.("%1$s distinct values (%2$s to %3$s)"), length(lv), lv[1], lv[length(lv)]),
                    n = sum(coding$counts), role = .("Not usable: the outcome needs two values")))
            } else for (i in seq_along(lv)) {
                role <- if (mc_only) .("Class in the multi-class analysis")
                    else if (!in_force) .("Not assigned yet")
                    else if (identical(lv[i], pos)) .("Positive (condition present)")
                    else if (length(lv) == 2L) .("Negative")
                    else .("Negative (pooled with the other non-positive levels)")
                table$addRow(rowKey = paste0("lvl:", lv[i]),
                             values = list(level = .rocLabel(lv[i], note = FALSE), n = coding$counts[i], role = role))
            }
            if (n_missing > 0) {
                table$addRow(rowKey = "missing",
                             values = list(level = .("(missing)"), n = n_missing, role = .("Excluded: missing outcome")))
            }
            others <- .rocLabel(setdiff(lv, pos))
            table$setNote("positive", if (in_force) {
                if (length(lv) == 2L)
                    sprintf(.("Positive class: %1$s. Sensitivity, PPV, LR+, every cut-point and the AUC describe how well each test detects %1$s; specificity and NPV refer to %2$s."), .rocLabel(pos), others)
                else
                    sprintf(.("Positive class: %1$s. Sensitivity, PPV, LR+, every cut-point and the AUC describe how well each test detects %1$s; specificity and NPV refer to the other levels combined (%2$s)."), .rocLabel(pos), paste(others, collapse = ", "))
            })
            table$setNote("source", switch(coding$status,
                selected = .("Chosen in Positive class."),
                default01 = .("The outcome is numeric and coded 0/1, so 1 is read as the positive class. If 0 marks the condition, recode the outcome or set it to Nominal in Data > Setup and choose 0 in Positive class."),
                NULL))
            table$setNote("unset", if (identical(coding$status, "unset")) private$.unsetNote(coding))
            table$setNote("n", if (is.null(analysed)) {
                .("N counts every row with an outcome value.")
            } else {
                sprintf(.("N counts the rows analysed: outcome and every selected predictor present (%1$s of %2$s rows)."), analysed[1], analysed[2])
            })
            table$setNote("reading", NULL)   # set after the ROC fit, see .run()
        },
        # Bootstrap confidence interval for an AUC, honouring the user's `bootstrapMethod`
        # choice. pROC::ci.auc() cannot do this: its bootstrap branch (pROC:::ci_auc_bootstrap)
        # always returns quantile()-based PERCENTILE limits, and the function has no argument
        # for the interval type at all - a `boot.ci.type=` handed to it is swallowed by `...`,
        # which is why all three settings produced byte-identical limits. Resample here and let
        # boot::boot.ci() (already an Import; used the same way in R/idi.b.R) apply the requested
        # interval type. Returns a length-3 numeric c(lower, auc, upper) shaped like pROC's own
        # ci.auc object, or NULL when no resample yielded a usable AUC.
        #
        # `auc_fun` is the statistic to resample; it defaults to the full AUC, and
        # .populatePartialAUC passes a partial-AUC closure so the user's interval type is honoured
        # there too. `clamp` is the range the statistic can legitimately occupy - [0, 1] for an
        # AUC, [0, range width] for a partial AUC - because the basic bootstrap reflects limits
        # about the point estimate and can push them outside it.
        .bootstrapAucCi = function(roc_obj,
                                   auc_fun = function(r) as.numeric(pROC::auc(r)),
                                   clamp = c(0, 1)) {
            # One case or one control (F-04): every resample repeats that patient, so the
            # bootstrap sets that group's variance term (DeLong's S10 or S01, 0 df) to zero - a
            # nominal 95% interval covered about 20% of true AUCs with 1 case vs 99 controls.
            # DeLong forms no interval here either. NA limits make every caller blank the cells
            # (.aucIntervalIssue(), .multiClassCi(), the partial AUC table); returning before
            # boot() also keeps a BCa fallback from being reported for an interval not shown.
            if (min(length(roc_obj$cases), length(roc_obj$controls)) < 2L) {
                ci <- c(NA_real_, as.numeric(auc_fun(roc_obj)), NA_real_)
                attr(ci, "method") <- "bootstrap"
                return(ci)
            }
            boot_n <- self$options$bootstrapSamples %||% 200
            boot_strat <- if (is.null(self$options$stratifiedBootstrap)) TRUE else isTRUE(self$options$stratifiedBootstrap)
            boot_type <- switch(self$options$bootstrapMethod,
                bca = "bca",
                percentile = "perc",
                basic = "basic",
                "bca"
            )
            conf_level <- self$options$confidenceLevel / 100

            boot_data <- data.frame(resp = roc_obj$response, pred = roc_obj$predictor)
            roc_levels <- roc_obj$levels
            roc_direction <- roc_obj$direction
            auc_statistic <- function(d, idx) {
                s <- d[idx, , drop = FALSE]
                # A resample that lost one of the two classes has no AUC.
                if (length(unique(s$resp)) < 2L) {
                    return(NA_real_)
                }
                as.numeric(auc_fun(.quietly(pROC::roc(
                    response = s$resp,
                    predictor = s$pred,
                    levels = roc_levels,
                    direction = roc_direction,
                    quiet = TRUE
                ))))
            }

            boot_res <- private$.withSeed(boot::boot(
                data = boot_data,
                statistic = auc_statistic,
                R = boot_n,
                strata = if (boot_strat) factor(boot_data$resp) else rep(1L, nrow(boot_data))
            ))
            ok <- is.finite(boot_res$t[, 1])
            if (!any(ok)) {
                return(NULL)
            }
            if (!all(ok)) {
                boot_res$t <- boot_res$t[ok, , drop = FALSE]
                boot_res$R <- sum(ok)
                # BCa gets its acceleration constant from empinf() -> boot.array(), which
                # REGENERATES the first boot_res$R index rows from the stored seed. After the
                # line above those are the first sum(ok) resamples, not the sum(ok) that
                # survived, so every (index, statistic) pair feeding the acceleration constant
                # is mismatched and the limits are neither BCa nor anything else defensible
                # (verified: 92 of 100 kept -> boot.array() returns a 92-row matrix rebuilt
                # from the seed; BCa [-0.3885, 0.2127] vs percentile [-0.3849, 0.2206]).
                # The percentile interval reads t directly and is unaffected by the reordering.
                if (identical(boot_type, "bca")) {
                    boot_type <- "perc"
                    private$.bcaFellBackToPercentile <- TRUE
                }
            }

            boot_ci <- suppressWarnings(boot::boot.ci(boot_res, conf = conf_level, type = boot_type))
            limits <- switch(boot_type,
                bca = boot_ci$bca[4:5],
                perc = boot_ci$percent[4:5],
                basic = boot_ci$basic[4:5]
            )
            # The basic bootstrap reflects limits about the point estimate and can push them
            # outside the range the statistic can occupy. An AUC cannot live above 1, so clamp
            # rather than show a pathologist an upper confidence limit of 1.16.
            limits <- pmin(pmax(as.numeric(limits), clamp[1]), clamp[2])

            point_est <- as.numeric(auc_fun(roc_obj))
            ci <- c(limits[1], point_est, limits[2])
            attr(ci, "conf.level") <- conf_level
            attr(ci, "method") <- "bootstrap"
            attr(ci, "boot.type") <- boot_type
            attr(ci, "boot.n") <- boot_n
            attr(ci, "boot.used") <- sum(ok)
            attr(ci, "boot.stratified") <- boot_strat
            attr(ci, "auc") <- point_est
            class(ci) <- c("ci.auc", "ci", "numeric")
            ci
        },

        # Report the method that produced an interval, including BCa downgrades.
        .bootstrapCiText = function(ci) {
            type <- switch(attr(ci, "boot.type"),
                perc = .("percentile"), basic = .("basic"), bca = "BCa", .("unspecified"))
            sprintf(if (isTRUE(attr(ci, "boot.stratified"))) {
                .("%1$s%% confidence intervals for the AUC are bootstrap intervals (%2$s) from %3$s usable resamples of %4$s attempted, drawn within each outcome class (boot package; Davison & Hinkley, 1997), random seed %5$s.")
            } else {
                .("%1$s%% confidence intervals for the AUC are bootstrap intervals (%2$s) from %3$s usable resamples of %4$s attempted, drawn from all rows (boot package; Davison & Hinkley, 1997), random seed %5$s.")
            }, 100 * attr(ci, "conf.level"), type, attr(ci, "boot.used"),
               attr(ci, "boot.n"), self$options$seed)
        },

        .aucCiText = function() {
            vapply(names(private$.rocResults), function(predictor) {
                result <- private$.rocResults[[predictor]]
                ci <- result$roc$ci
                text <- if (!is.null(result$ci_issue)) {
                    .("No AUC confidence interval is reported because the interval is not estimable.")
                } else if (identical(attr(ci, "method"), "bootstrap")) {
                    private$.bootstrapCiText(ci)
                } else {
                    paste(sprintf(.("%s%% confidence intervals for the AUC use DeLong's method (DeLong et al., 1988)."), self$options$confidenceLevel),
                          .("They are Wald intervals on the AUC scale: the AUC plus or minus z times DeLong's standard error, truncated to 0-1."))
                }
                if (isTRUE(attr(ci, "bootstrap.failed"))) {
                    text <- paste(text, .("The requested bootstrap interval could not be computed; DeLong's method was used as the fallback."))
                }
                paste0(predictor, ": ", text)
            }, character(1), USE.NAMES = FALSE)
        },

        # Why no AUC interval can be shown (VAL-04), or NULL when one can. DeLong's variance is 0
        # at perfect separation and for a constant marker, so its interval has zero width; boot.ci()
        # forms none when every resample has the same AUC. `why` describes the data, `blank` the
        # interval method (it is also the footnote on the blank cells).
        .aucIntervalIssue = function(roc_obj) {
            ci <- suppressWarnings(as.numeric(roc_obj$ci))
            boot <- identical(attr(roc_obj$ci, "method"), "bootstrap")
            se <- if (boot) NA_real_ else tryCatch(sqrt(pROC::var(roc_obj)), error = function(e) NA_real_)
            # A finite interval of zero width (every resample gave the same AUC) is no interval either.
            if (length(ci) == 3 && all(is.finite(ci[c(1, 3)])) && ci[3] > ci[1] &&
                (boot || (is.finite(se) && se > 0))) return(NULL)
            n1 <- length(roc_obj$cases)
            n0 <- length(roc_obj$controls)
            auc <- as.numeric(roc_obj$auc)
            why <- if (length(unique(c(roc_obj$cases, roc_obj$controls))) == 1L) {
                .("The marker has the same value for every patient analysed, so it cannot separate the groups: its AUC is 0.5 by construction.")
            } else if (auc == 1) {
                sprintf(.("The marker separates the two groups perfectly in these data (AUC = 1); with %1$s positive and %2$s negative cases the true AUC could still be well below 1."), n1, n0)
            } else if (auc == 0) {
                sprintf(.("The marker separates the two groups perfectly in these data, in the opposite direction to the one set (AUC = 0); with %1$s positive and %2$s negative cases the true AUC could still be well above 0."), n1, n0)
            } else {
                sprintf(.("No AUC interval can be estimated with %1$s positive and %2$s negative cases."), n1, n0)
            }
            blank <- if (boot && min(n1, n0) < 2L) {
                private$.singlePatientBootNote()
            } else if (boot) {
                .("Every bootstrap resample gave the same AUC, so no bootstrap interval can be formed; the limits are left blank.")
            } else {
                .("DeLong's standard error is 0 or undefined here, so its interval would have zero width; the interval and the standard error are left blank.")
            }
            list(why = why, blank = blank, separated = auc %in% c(0, 1))
        },
        # Apply the display choice on every exit, including restored results and early returns.
        # Kept whatever the choice: the random seed (a resampled result always shows the seed
        # that produced it) and the notes without which a number is misread - the cut-off rule
        # (>= or <=, midpoints), that the cut-point was searched for on these data, what
        # Venkatraman's test tests, and that p-values are unadjusted.
        # The multi-class orientation notes stay too: without them a perfect marker read against
        # the declared Direction shows one-vs-one AUC 0.000 "Below chance", and the one-vs-rest
        # averages 0.5 by construction, with nothing on screen saying why.
        # "ci_method" too (cross-analysis review XA-05, 2026-10-21): an interval is not read
        # correctly without its method, and the default page never named DeLong.
        .keptTableNotes = c("seed", "rule", "cutpoint_optimism", "venkatraman", "multiplicity",
                            "ovr_average", "ovr_direction", "ovo_direction", "pair_order", "ci_method"),
        .applyTableNotesOption = function() {
            if (isTRUE(self$options$showTableNotes)) return(invisible(NULL))
            strip <- function(item) {
                if (inherits(item, "Table")) {
                    for (key in setdiff(names(item$notes), private$.keptTableNotes)) item$setNote(key, NULL)
                    for (column in item$columns) {
                        for (row in seq_len(item$rowCount)) {
                            cell <- column$getCell(row)
                            # a blanked statistic keeps the one sentence that says why it is blank
                            if (!length(cell$footnotes) || identical(cell$value, "")) next
                            cell$footnotes <- character()
                            cell$sups <- integer()
                            # Invalidate the cached footnote list without adding a note.
                            item$addFootnote(column$name, character(), rowNo = row)
                        }
                    }
                } else if (inherits(item, "Group") || inherits(item, "Array")) {
                    for (child in item$items) strip(child)
                }
            }
            strip(self$results)
            invisible(NULL)
        },
        # Footnote for a bootstrap interval withheld because one group has a single patient (F-04).
        .singlePatientBootNote = function() {
            .("One of the two groups has a single patient, so every bootstrap resample repeats that patient and the interval would reflect only the other group's variation; no interval is shown.")
        },

        # jamovi prints a NULL cell as "." and NA as "NaN"; only "" is blank (as .footnoteNA in
        # R/decisioncalculator.b.R). Call after addRow().
        .blankCells = function(table, rowKey, cols, note = NULL) {
            for (col in cols) {
                table$setCell(col = col, value = "", rowKey = rowKey)
                if (!is.null(note)) table$addFootnote(col = col, note = note, rowKey = rowKey)
            }
        },

        # An option value as its UI title (the keys are not for display), reusing the .a.yaml titles' msgids.
        .choiceLabel = function(option, value) {
            labels <- switch(option,
                clinicalContext = list(screening = .("Screening"), diagnosis = .("Diagnosis"),
                    prognosis = .("Prognosis"), monitoring = .("Monitoring"), general = .("General")),
                analysisType = list(single = .("Single ROC Analysis"), comparative = .("Comparative ROC Analysis"),
                    comprehensive = .("Comprehensive Diagnostic Performance")),
                clinicalPresets = list(custom = .("Custom Configuration"),
                    biomarker_screening = .("Biomarker Screening (High Sensitivity)"),
                    diagnostic_validation = .("Diagnostic Test Validation (Balanced)"),
                    confirmatory_testing = .("Confirmatory Testing (High Specificity)"),
                    research_comprehensive = .("Research (Youden with Your Minimums)")),
                list())
            labels[[value]] %||% value
        },

        # The Detailed comparison metrics are keyed in English (row keys); this is what the user reads.
        .metricLabel = function(metric) {
            switch(metric, AUC = .("AUC"), Sensitivity = .("Sensitivity"), Specificity = .("Specificity"),
                   Accuracy = .("Accuracy"), metric)
        },

        # jamovi prints a numeric NA as "NaN". Blank each undefined value of a just-added row and
        # footnote why; `reasons` maps a column to its sentence (NULL: blank, no footnote).
        .blankUndefined = function(table, rowKey, values, reasons) {
            for (col in names(values)) {
                v <- values[[col]]
                if (length(v) != 1L || !is.numeric(v) || is.finite(v)) next
                private$.blankCells(table, rowKey, col, reasons[[col]])
            }
        },

        # The frozen-model helper (R/enhancedROC-validation.R, R/utils-diagnostic-validation.R) is
        # file level and cannot call .(): it returns fixed English labels, metric keys (pr_auc,
        # flags_per_1000) and option keys (frozen, prespecified). Each text cell of a frozen table is
        # turned into its display label here, matched exactly; anything else (numbers, user text,
        # predictor names) passes through unchanged.
        # Same idea for the helpers' stop() messages: each fixed English message, matched exactly, to
        # its translation; anything else (an unexpected error) is shown as it is.
        .frozenError = function(message) {
            prefix <- "Unmapped reference levels; explicitly recode before evaluation: "
            if (startsWith(message, prefix))
                return(.fmt(.("Unmapped reference levels; explicitly recode before evaluation: {levels}"),
                            levels = substring(message, nchar(prefix) + 1L)))
            known <- list(
                "Select a positive reference level and enter a distinct negative reference level." =
                    .("Select a positive reference level and enter a distinct negative reference level."),
                "Frozen-model evaluation requires an explicit score direction." =
                    .("Frozen-model evaluation requires an explicit score direction."),
                "Frozen evaluation requires a model version and an independently selected threshold. Use Development / exploratory for data-selected thresholds." =
                    .("Frozen evaluation requires a model version and an independently selected threshold. Use Development / exploratory for data-selected thresholds."),
                "Enter at least one fixed score cutoff." = .("Enter at least one fixed score cutoff."),
                "Enter comma-separated numbers within the allowed range." =
                    .("Enter comma-separated numbers within the allowed range."),
                "At most 100 scenario values or cutoffs are allowed." =
                    .("At most 100 scenario values or cutoffs are allowed."),
                "Patient IDs must be complete; none were silently dropped." =
                    .("Patient IDs must be complete; none were silently dropped."),
                "Site IDs must be complete; none were silently dropped." =
                    .("Site IDs must be complete; none were silently dropped."),
                "A partition audit requires patient identifiers." = .("A partition audit requires patient identifiers."),
                "Reference status is inconsistent within a patient." =
                    .("Reference status is inconsistent within a patient."),
                "Patient IDs span sites; use globally unique patient IDs and resolve cross-site duplicates." =
                    .("Patient IDs span sites; use globally unique patient IDs and resolve cross-site duplicates."),
                "Partition labels must be complete." = .("Partition labels must be complete."),
                "Patient overlap across partitions detected. Resolve leakage before evaluation." =
                    .("Patient overlap across partitions detected. Resolve leakage before evaluation."),
                "No complete reference and score rows remain." = .("No complete reference and score rows remain."),
                "Probability inputs must be in [0,1] for the selected positive class; direction must be higher." =
                    # no " [" inside .(): the translator reads it as a context suffix and drops the rest
                    .("Probability inputs must lie between 0 and 1 for the selected positive class, and the direction must be higher."))
            known[[message]] %||% message
        },
        .frozenRow = function(table, values) {
            o <- self$options
            unit <- if (length(o$analysisUnit) && nzchar(trimws(o$analysisUnit))) o$analysisUnit else "row"
            preds <- o$predictors
            pick <- function(x, map) if (length(x) == 1L && !is.na(x) && !is.null(map[[x]])) map[[x]] else x
            status <- list(
                "CI not requested" = .("CI not requested"),
                "Metric undefined" = .("Metric undefined"),
                "Too few independent clusters per site" = .("Too few independent clusters per site"),
                "Insufficient valid replicates" = .("Insufficient valid replicates"),
                "Degenerate bootstrap; interval unavailable" = .("Degenerate bootstrap; interval unavailable"),
                "Percentile; fixed model and sites" = .("Percentile; fixed model and sites"))
            if (identical(table, "frozenMetrics")) {
                metric <- list(auc = .("AUC"), ap = .("Average precision"), pr_auc = .("AUC-PR (interpolated)"),
                    sensitivity = .("Sensitivity"), specificity = .("Specificity"), ppv = .("PPV"), npv = .("NPV"),
                    f1 = .("F1 score"), flags_per_1000 = .("Flagged per 1000"), brier = .("Brier score"),
                    calibration_intercept = .("Calibration intercept"), calibration_slope = .("Calibration slope"),
                    calibration_in_large = .("Calibration-in-the-large"))
                values$metric <- pick(as.character(values$metric), metric)
                values$status <- pick(as.character(values$status), status)
                if (length(preds) > 1) {
                    minus <- stats::setNames(lapply(preds[-1], function(p) .fmt(.("{a} minus {b}"), a = p, b = preds[1])),
                                             paste(preds[-1], "minus", preds[1]))
                    values$model <- pick(as.character(values$model), minus)
                }
                site <- as.character(values$site)
                values$site <- if (identical(site, "All")) .("All") else if (isTRUE(startsWith(site, "Site: ")))
                    .fmt(.("Site: {site}"), site = substring(site, 7)) else site
            } else if (identical(table, "denominatorLedger")) {
                values$status <- pick(as.character(values$status), list(
                    "Input rows" = .("Input rows"), "Missing reference" = .("Missing reference"),
                    "Missing / nonfinite score" = .("Missing / nonfinite score"),
                    "Analyzed common cases" = .("Analyzed common cases"),
                    "Analyzed positive" = .("Analyzed positive"), "Analyzed negative" = .("Analyzed negative")))
            } else if (identical(table, "validationContext")) {
                setting <- as.character(values$setting)
                value <- as.character(values$value)
                titles <- list(
                    "Cohort role (declared, not verified)" = list(development = .("Development / exploratory"),
                        frozen = .("Frozen external / temporal evaluation")),
                    "Sampling design" = list(consecutive = .("Consecutive"), enriched = .("Enriched"),
                        positive_only = .("Positive only"), other = .("Other / unknown")),
                    "Threshold source" = list(prespecified = .("Prespecified independently"),
                        development = .("Selected in development"), exploratory = .("Selected in evaluation data")),
                    "Prediction scale" = list(score = .("Score"), probability = .("Probability of selected positive class")))
                if (!is.null(titles[[setting]])) {
                    default <- isTRUE(endsWith(value, " (default setting)"))
                    key <- if (default) substring(value, 1, nchar(value) - nchar(" (default setting)")) else value
                    title <- pick(key, titles[[setting]])
                    value <- if (default) .fmt(.("{value} (default setting)"), value = title) else title
                } else if (identical(setting, "Reference standard") && (is.na(value) || !nzchar(value))) {
                    value <- .("Not stated")
                } else {
                    value <- pick(value, stats::setNames(list(
                        .("Row; independence assumed"),
                        .("Conditional on observed sites; no site-population extrapolation"),
                        .("No patient overlap in supplied partition labels"),
                        .("Not assessed; no partition manifest"),
                        .fmt(.("Patient; all of the patient's rows ({unit}) retained"), unit = unit),
                        .fmt(.("Frozen scores; no refitting or threshold optimization. Every analysed row ({unit}) weighs equally in the metrics; paired differences use common cases."), unit = unit)),
                        c("Row; independence assumed",
                          "Conditional on observed sites; no site-population extrapolation",
                          "No patient overlap in supplied partition labels",
                          "Not assessed; no partition manifest",
                          paste0("Patient; all of the patient's rows (", unit, ") retained"),
                          paste0("Frozen scores; no refitting or threshold optimization. Every analysed row (", unit, ") weighs equally in the metrics; paired differences use common cases."))))
                }
                values$value <- value
                values$setting <- pick(setting, list(
                    "Cohort role (declared, not verified)" = .("Cohort role (declared, not verified)"),
                    "Sampling design" = .("Sampling design"), "Model version" = .("Model version"),
                    "Reference standard" = .("Reference standard"), "Positive / negative" = .("Positive / negative"),
                    "Threshold source" = .("Threshold source"), "Unit of observation" = .("Unit of observation"),
                    "Prediction scale" = .("Prediction scale"),
                    "Analyzed independent units" = .("Analyzed independent units"),
                    "Positive independent units" = .("Positive independent units"),
                    "Negative independent units" = .("Negative independent units"),
                    "Resampling unit" = .("Resampling unit"), "Site inference" = .("Site inference"),
                    "Partition audit" = .("Partition audit"), "Interpretation" = .("Interpretation")))
            } else if (identical(table, "screeningWorkload")) {
                values$source <- pick(as.character(values$source), list(Observed = .("Observed"), Projected = .("Projected")))
            }
            # User text (model version, reference standard, unit, levels, site and predictor names)
            # is parsed as HTML in a cell: "<Histology> review" showed " review". After matching.
            values[] <- lapply(values, function(v) if (is.character(v)) .rocLabel(v, note = FALSE) else v)
            values
        },

        # Why each impact / workload ratio can be undefined at a fixed cutoff (.dvWorkload() gives NA).
        .undefinedRatioNotes = function() {
            no_tp <- .("Undefined: no true positive is expected at this cutoff.")
            list(
                nnt = .("Undefined: the net benefit at this cutoff is 0 or negative, so there is no number needed to treat."),
                nnd = .("Undefined: sensitivity + specificity is 1 or less at this cutoff, so the marker does no better than chance here."),
                ppv = .("Undefined: no patient is classified as positive at this cutoff."),
                npv = .("Undefined: every patient is classified as positive at this cutoff."),
                flags_per_detected = no_tp, tests_per_detected = no_tp)
        },

        # One roc.test() per predictor pair and run (VAL-05): ROC Curve Comparisons, the Statistical
        # Comparison Summary and the AUC row of Detailed Model Comparison quote the same test, so one
        # pair has one p-value. boot.n = Bootstrap samples: resamples for the bootstrap test,
        # permutations for Venkatraman's; DeLong ignores it. A failure is cached and re-raised.
        .pairTest = function(pred1, pred2) {
            key <- paste(private$.escapeVar(pred1), private$.escapeVar(pred2), sep = "_vs_")
            res <- private$.rocTestCache[[key]]
            if (is.null(res)) {
                r1 <- private$.rocResults[[pred1]]$roc
                r2 <- private$.rocResults[[pred2]]$roc
                # pROC's paired Venkatraman statistic ranks the raw predictor as if higher = case and
                # ignores $direction (roc.test only warns, which jamovi never shows), so a marker read
                # '>' was tested as its point-reflected curve. Hand it the same curve read '<'. Pairs
                # read the same way are exact already (reflecting both leaves the statistic alone) and
                # keep their seeded p-values. Pairing holds: every $response is the same complete-case
                # outcome (.prepareData()).
                if (identical(self$options$comparisonMethod, "venkatraman") && !identical(r1$direction, r2$direction)) {
                    asRead <- function(r) if (identical(r$direction, ">"))
                        pROC::roc(r$response, -r$predictor, levels = r$levels, direction = "<", quiet = TRUE) else r
                    r1 <- asRead(r1)
                    r2 <- asRead(r2)
                }
                # Two perfect curves: the AUC difference and its standard error are both 0, and
                # roc.test() returns a hard-coded Z = 0, p = 1 with a warning (pROC's own rule:
                # max(Se + Sp) = 2). Venkatraman's permutation p for two identical curves is
                # computed, so it is left alone. NA cells are blanked by every caller.
                perfect <- function(r) abs(max(r$sensitivities + r$specificities) - 2) < sqrt(.Machine$double.eps)
                if (!identical(self$options$comparisonMethod, "venkatraman") && perfect(r1) && perfect(r2)) {
                    res <- list(statistic = NA_real_, p.value = NA_real_,
                        why = .("Both markers separate the groups perfectly in these data (AUC = 1), so their AUC difference and its standard error are both 0 and no test statistic exists."))
                    private$.rocTestCache[[key]] <- res
                    return(res)
                }
                res <- tryCatch(
                    private$.withSeed(pROC::roc.test(r1, r2,
                                   method = self$options$comparisonMethod,
                                   boot.n = self$options$bootstrapSamples,
                                   # F-31: pROC defaults this to TRUE whatever the option said
                                   boot.stratified = !isFALSE(self$options$stratifiedBootstrap))),
                    error = function(e) e)
                # pROC returns the share of permutations at least as extreme, which is 0 when none is.
                # B permutations support no p below 1/(B + 1) (Phipson & Smyth 2010): with 200 the
                # table printed p = 0 and "Highly significant (p < 0.001)".
                if (!inherits(res, "error") && identical(self$options$comparisonMethod, "venkatraman")) {
                    b <- self$options$bootstrapSamples
                    res$p.value <- (res$p.value * b + 1) / (b + 1)
                }
                private$.rocTestCache[[key]] <- res
            }
            if (inherits(res, "error")) stop(res)
            res
        },

        # Venkatraman's test compares whole curves, not AUCs (VAL-22): said on every table that shows
        # its p-value. NULL removes the note under the AUC tests.
        .venkatramanNote = function() {
            if (!identical(self$options$comparisonMethod, "venkatraman")) return(NULL)
            sprintf(.("Venkatraman's permutation test (Venkatraman & Begg 1996, paired data; %s permutations, the Bootstrap samples setting) tests whether the two ROC curves are identical over their whole range, not whether the AUCs differ: curves with equal AUCs can still differ, for example when they cross. The AUC difference is descriptive."), self$options$bootstrapSamples)
        },

        .runROCAnalysis = function(data) {
            private$.rocResults <- list()
            private$.rocSmoothed <- list()

            # Clean up old ROC objects to manage memory
            for (predictor in private$.predictors) {
                # Checkpoint before each expensive ROC computation
                private$.checkpoint()

                # Comprehensive error handling for ROC analysis
                tryCatch(
                    {
                        # CRITICAL FIX: Determine direction
                        # Let pROC auto-detect direction when "auto" is selected
                        # This prevents AUC inversion for biomarkers where higher values indicate disease
                        direction_param <- self$options$direction
                        if (direction_param == "auto") {
                            direction <- "auto" # pROC picks "<" or ">" by comparing the two groups' medians
                        } else if (direction_param == "higher") {
                            direction <- "<" # pROC: "<" means controls < cases (higher values = positive)
                        } else if (direction_param == "lower") {
                            direction <- ">" # pROC: ">" means controls > cases (lower values = positive)
                        } else {
                            direction <- "auto" # Fallback to auto-detection
                        }

                        # .prepareData() has already made the outcome binary: levels c(negative, positive).
                        response_var <- data[[private$.outcome]]

                        # Configure CI method and bootstrap behavior. The resampling parameters
                        # now live in .bootstrapAucCi(), which is the only thing that uses them.
                        use_boot <- isTRUE(self$options$useBootstrap)

                        # Create ROC object - always use DeLong CI initially
                        # Bootstrap CI is computed separately via pROC::ci.auc() so
                        # the user's bootstrap method choice (BCa/percentile/basic) takes effect
                        roc_obj <- .quietly(pROC::roc(
                            response = response_var,
                            predictor = data[[predictor]],
                            direction = direction,
                            ci = TRUE,
                            conf.level = self$options$confidenceLevel / 100,
                            quiet = TRUE
                        ))

                        # If a bootstrap CI was requested, recompute it with the interval type the
                        # user actually chose. See .bootstrapAucCi() for why pROC cannot do this.
                        if (use_boot) {
                            boot_ci <- tryCatch(
                                private$.bootstrapAucCi(roc_obj),
                                error = function(e) e
                            )
                            if (inherits(boot_ci, "error") || is.null(boot_ci)) {
                                attr(roc_obj$ci, "bootstrap.failed") <- TRUE
                                private$.addNotice(
                                    type = "WARNING",
                                    title = sprintf(.("Bootstrap CI Failed: %s"), predictor),
                                    # Two complete sentences instead of splicing an untranslated
                                    # English fragment through %s.
                                    content = if (inherits(boot_ci, "error")) {
                                        sprintf(.("Bootstrap confidence interval computation failed for %1$s: %2$s. DeLong's method is used instead; limits are shown only when estimable."), predictor, conditionMessage(boot_ci))
                                    } else {
                                        sprintf(.("Bootstrap confidence interval computation failed for %s because no resample contained both outcome classes. DeLong's method is used instead; limits are shown only when estimable."), predictor)
                                    }
                                )
                            } else {
                                roc_obj$ci <- boot_ci
                            }
                        }

                        # Apply smoothing if requested. The smoothed curve is stored alongside the
                        # empirical one and used ONLY for drawing - see the .rocSmoothed comment.
                        if (self$options$smoothMethod != "none") {
                            smooth_method <- switch(self$options$smoothMethod,
                                binormal = "binormal",
                                kernel = "density",
                                NULL
                            )
                            if (!is.null(smooth_method)) {
                                smoothed <- tryCatch(
                                    pROC::smooth(roc_obj, method = smooth_method),
                                    error = function(e) e
                                )
                                if (inherits(smoothed, "error")) {
                                    private$.addNotice(
                                        type = "WARNING",
                                        title = sprintf(.("Smoothing Failed: %s"), predictor),
                                        content = sprintf(.("ROC smoothing could not be applied to %1$s (%2$s). The empirical ROC curve is shown instead; all reported statistics are unaffected."), predictor, conditionMessage(smoothed))
                                    )
                                } else {
                                    private$.rocSmoothed[[predictor]] <- smoothed
                                }
                            }
                        }

                        private$.rocObjects[[predictor]] <- roc_obj

                        # Surface the auto-detected direction for clinical safety
                        if (direction_param == "auto") {
                            # Whole sentences naming the positive level (it was an untranslated
                            # fragment calling every positive class "disease").
                            dir_sentence <- if (identical(roc_obj$direction, "<")) {
                                sprintf(.("ROC direction for %1$s was chosen automatically: higher values of %1$s were taken to indicate %2$s (the positive class)."), predictor, private$.positiveClass)
                            } else {
                                sprintf(.("ROC direction for %1$s was chosen automatically: lower values of %1$s were taken to indicate %2$s (the positive class)."), predictor, private$.positiveClass)
                            }
                            # Quantify the upward bias at THIS sample size.
                            #
                            # pROC's direction = "auto" compares the two groups' MEDIANS; it does
                            # not maximise the AUC, and the reported AUC is therefore not floored
                            # at 0.5 (verified: on 1000 pure-noise observations auto chose "<" and
                            # reported 0.4838 where ">" would have given 0.5162). But because the
                            # direction is read from the same data used to compute the AUC, the
                            # result is still biased upward, and badly so in small samples --
                            # simulating a marker with no information at all gives a mean reported
                            # AUC of about 0.60 at n = 20 and about 0.57 at n = 40, against 0.50
                            # with the direction fixed in advance, and exceeds 0.60 in roughly 45
                            # of every 100 runs at n = 20 (1000 reps; the same figures the
                            # Direction option's description quotes).
                            # 0.5 + se * sqrt(2/pi) with the Hanley-McNeil null standard error is
                            # E[max(AUC, 1 - AUC)], an UPPER approximation to that mean (0.606 at
                            # n = 20, against a simulated 0.598; review F-37), so it is quoted as
                            # "up to about". This bias is what turns a null pilot study into an
                            # apparently promising biomarker.
                            n_case <- length(roc_obj$cases)
                            n_ctrl <- length(roc_obj$controls)
                            null_auc <- if (n_case > 0 && n_ctrl > 0) {
                                se_null <- sqrt((n_case + n_ctrl + 1) / (12 * n_case * n_ctrl))
                                0.5 + se_null * sqrt(2 / pi)
                            } else {
                                NA_real_
                            }
                            size_note <- if (!is.na(null_auc) && null_auc > 0.52) {
                                sprintf(.("With %1$d positive and %2$d negative cases, a marker carrying no information at all would still be reported, on average, at an AUC of up to about %3$.2f under automatic detection, so treat a modest AUC here with caution."), n_case, n_ctrl, null_auc)
                            }
                            private$.addNotice(
                                type = "WARNING",
                                title = sprintf(.("Direction Auto-Detected: %s"), predictor),
                                content = paste(c(dir_sentence,
                                    .("The direction was read from these data \u2014 by comparing the two groups' median values \u2014 rather than assumed in advance. Because the same data then supply the AUC, the AUC is biased upward, and a marker pointing the wrong way is reported as though it pointed the right way."),
                                    size_note,   # NULL when not needed; c() drops it
                                    .("If you know which way the marker should point, set Direction explicitly; a low AUC is then a real finding rather than an artefact of the direction being fitted.")),
                                    collapse = " ")
                            )
                        }

                        # Calculate optimal cutoff using Youden Index
                        optimal_cutoff <- private$.calculateOptimalCutoff(roc_obj, data, predictor)

                        # Evaluate custom cutoffs if specified
                        custom_cutoffs <- private$.evaluateCustomCutoffs(roc_obj, data, predictor)

                        # Store results
                        ci_issue <- private$.aucIntervalIssue(roc_obj)
                        private$.rocResults[[predictor]] <- list(
                            roc = roc_obj,
                            optimal_cutoff = optimal_cutoff,
                            custom_cutoffs = custom_cutoffs,
                            predictor = predictor,
                            ci_issue = ci_issue
                        )

                        # Clinical threshold notices for this predictor
                        auc_value <- as.numeric(roc_obj$auc)
                        n_obs <- length(roc_obj$response)
                        n_positive <- sum(roc_obj$response == levels(roc_obj$response)[2])
                        n_negative <- sum(roc_obj$response == levels(roc_obj$response)[1])
                        prevalence <- n_positive / n_obs

                        # Per-class event count guard. ERROR, not STRONG_WARNING: the clinical
                        # threshold checklist puts fewer than 10 events in a class at ERROR, and
                        # the pane this sits above is a full results pane - AUC, confidence
                        # interval and an optimal cut-point - computed from as few as 4 events in
                        # a 200-patient dataset. None of those quantities means anything there,
                        # so the banner has to read as a stop sign rather than a caution.
                        # Every predictor is fitted on the same complete-case rows, so these two
                        # are said once per run, not repeated for each predictor.
                        first_fit <- !isTRUE(private$.sampleNoticesShown)
                        private$.sampleNoticesShown <- TRUE
                        if (first_fit && (n_positive < 10 || n_negative < 10)) {
                            private$.addNotice(
                                type = "ERROR",
                                title = .("Low Per-Class Count"),
                                content = sprintf(.("Very few events in one or both classes of the analysed rows: %1$s positive, %2$s negative. ROC estimates, confidence intervals, and optimal cutoffs are unreliable with fewer than 10 events per class. Collect more data before drawing clinical conclusions."), n_positive, n_negative)
                            )
                        }

                        # Small sample size notice (fewer than 20 complete rows never get here:
                        # .prepareData() refuses them)
                        if (first_fit && n_obs < 30) {
                            private$.addNotice(
                                type = "WARNING",
                                title = .("Small Sample Size"),
                                # F-39: a bootstrap changes only the interval, never the AUC, so it
                                # cannot make an estimate "more stable".
                                content = sprintf(.("Small sample size: n=%1$s analysed rows (%2$s positive, %3$s negative). \u2022 ROC curve confidence intervals may be unreliable with limited data. \u2022 Bootstrap intervals are an alternative to DeLong's, but no interval method is exact at this size, and only more data make the estimates more stable. \u2022 Results should be interpreted cautiously and validated in larger samples."), n_obs, n_positive, n_negative)
                            )
                        }

                        # A marker with one value cannot discriminate: its AUC is 0.5 by construction,
                        # so the interval and AUC-band notices below would describe an artefact, and
                        # "completed successfully" would follow them.
                        constant <- length(unique(roc_obj$predictor)) < 2L
                        # the cut-point tables skip it: their -Inf row would contradict this ERROR
                        private$.rocResults[[predictor]]$constant <- constant
                        if (constant) {
                            private$.addNotice(
                                type = "ERROR",
                                title = sprintf(.("Constant Predictor: %s"), predictor),
                                content = sprintf(.("%1$s has the same value (%2$s) in every analysed row, so it cannot separate the groups: its AUC is 0.5 by construction and no cut-point exists. Choose a predictor that varies, or check how the data were imported."), predictor, format(roc_obj$predictor[1]))
                            )
                        }

                        # A zero-width or missing interval is never presented as an estimate (VAL-04).
                        if (!constant && !is.null(ci_issue)) {
                            private$.addNotice(
                                type = "WARNING",
                                title = sprintf(.("No AUC Interval: %s"), predictor),
                                content = paste(ci_issue$why, ci_issue$blank)
                            )
                        }

                        # Low AUC notice. Below 0.5 is a DIFFERENT problem from merely weak
                        # discrimination and needs different words: the marker is almost always
                        # being read the wrong way round rather than being useless. Titling that
                        # "Limited Diagnostic Performance" buried the actionable part, and an AUC
                        # of 0.175 is not limited performance - it is a strong marker pointing the
                        # other way. Same order as psychopdaROC (PS4): first the positive class (a
                        # wrong level gives exactly 1 - AUC), then Direction, which makes the AUC
                        # 1 - AUC and RE-ESTIMATES the cut-point, sensitivity and specificity - they
                        # are not swapped. Both edges compare the AUC as displayed (VAL-07).
                        if (!constant && round(auc_value, 3) < 0.5) {
                            pos <- private$.positiveClass   # raw: notice content is escaped when rendered
                            # Was an untranslated English fragment spliced through %s, so a
                            # translated notice carried an English clause mid-sentence and a
                            # word order fixed to English grammar. Translate the whole clause.
                            dir_now <- if (identical(roc_obj$direction, "<")) {
                                .("higher values indicate the positive class")
                            } else {
                                .("lower values indicate the positive class")
                            }
                            dir_alt <- if (identical(roc_obj$direction, "<")) .("lower") else .("higher")

                            # Only claim the marker is REVERSED when the interval supports it.
                            # Under the null the AUC is symmetric about 0.5, so roughly half of
                            # all uninformative markers land below it by chance (simulated: 0.492
                            # of 2000 null markers at n = 200). Telling those users to flip
                            # Direction would flip the cutpoint, sensitivity and specificity on
                            # noise. An interval lying wholly below 0.5 is the evidence that
                            # distinguishes a genuinely backwards marker from a useless one.
                            ci_vals <- tryCatch(as.numeric(roc_obj$ci), error = function(e) NULL)
                            ci_upper <- if (length(ci_vals) == 3) ci_vals[3] else NA_real_
                            # At AUC 0 (perfect separation the other way) there is no interval to
                            # cite - DeLong's has zero width, the bootstrap forms none - but the
                            # marker is reversed in these data all the same.
                            separated <- isTRUE(ci_issue$separated)
                            reversed <- separated || (is.null(ci_issue) && is.finite(ci_upper) && ci_upper < 0.5)

                            if (reversed) {
                                # F-36: at small n DeLong under-covers, so this rule fires on 3.6% of
                                # null markers at 10 + 10 (exact enumeration) against the 2.5% a 95%
                                # interval implies. Said where the DeLong interval is the one read.
                                small_delong <- !separated && min(n_positive, n_negative) < 30 &&
                                    !identical(attr(roc_obj$ci, "method"), "bootstrap")
                                private$.addNotice(
                                    type = "ERROR",
                                    title = sprintf(.("Marker Reads Backwards: %s"), predictor),
                                    content = if (separated) {
                                        sprintf(.("%1$s has an AUC of 0: in these data it separates the groups perfectly, but in the opposite direction to the one assumed (%2$s). \u2022 First check Positive class: is %3$s the level that means the condition is present? Naming the wrong level turns the AUC into 1 minus its true value, exactly this pattern. \u2022 Only if it is, setting Direction to '%4$s' gives an AUC of 1 and re-estimates the cut-point, sensitivity and specificity. \u2022 Change Direction only if that matches what the marker means clinically."), predictor, dir_now, pos, dir_alt)
                                    } else {
                                        paste(c(sprintf(.("%1$s has an AUC of %2$s and its whole confidence interval lies below 0.5: it separates the groups, but in the opposite direction to the one assumed (%3$s). \u2022 First check Positive class: is %4$s the level that means the condition is present? Naming the wrong level turns the AUC into 1 minus its true value, exactly this pattern. \u2022 Only if it is, setting Direction to '%5$s' gives an AUC of %6$s and re-estimates the cut-point, sensitivity and specificity. \u2022 Change Direction only if that matches what the marker means clinically."), predictor, round(auc_value, 3), dir_now, pos, dir_alt, round(1 - auc_value, 3)),
                                                # The 3.6-in-100 figure was simulated at 95%; at another level only the direction of the error is known.
                                                if (small_delong) paste("\u2022", if (isTRUE(all.equal(self$options$confidenceLevel, 95)))
                                                    sprintf(.("With %1$s patients in the smaller group DeLong's interval is approximate and tends to be too narrow, so a marker carrying no information at all is flagged here somewhat more often than the confidence level implies: about 3.6 in 100 such markers at 10 positive and 10 negative patients, against 2.5 in 100 for a 95%% interval."), min(n_positive, n_negative))
                                                    else sprintf(.("With %1$s patients in the smaller group DeLong's interval is approximate and tends to be too narrow, so a marker carrying no information at all is flagged here somewhat more often than the confidence level implies."), min(n_positive, n_negative)))),
                                              collapse = " ")
                                    }
                                )
                            } else {
                                # F-15: under Direction = auto the direction comes from the same data
                                # (median rule), so a null marker is reported below 0.5 about 1 in 9
                                # times at 10 + 10 (simulated 0.1106), 0.140 at 20 + 20 and 0.167 at
                                # 100 + 100; the limit is 1/6 (the median and rank statistics
                                # correlate sqrt(3)/2 under the null). With a fixed direction it is
                                # about half (exact 0.4853 at 10 + 10).
                                # F-extra (Codex): no interval at all is not an interval that crosses 0.5.
                                below_text <- if (!is.null(ci_issue)) {
                                    sprintf(.("%1$s has an AUC of %2$s, below the 0.5 expected from a coin toss, and no confidence interval could be formed for it, so these data cannot show whether the marker runs backwards. \u2022 If you expected it to run the other way, first check Positive class (is %3$s the level that means the condition is present?), then Direction, before drawing any conclusion."), predictor, round(auc_value, 3), pos)
                                } else if (identical(self$options$direction, "auto")) {
                                    sprintf(.("%1$s has an AUC of %2$s, below the 0.5 expected from a coin toss, but its confidence interval still crosses 0.5. \u2022 With Direction set to auto the direction is read from these same data, so a marker carrying no information at all falls below 0.5 less often than with a fixed direction: about 1 in 9 at 10 positive and 10 negative patients, rising toward 1 in 6 in large samples. This is still not evidence that the marker runs backwards. \u2022 It is consistent with a marker that simply does not discriminate. \u2022 If you expected it to run the other way, first check Positive class (is %3$s the level that means the condition is present?), then Direction, before drawing any conclusion."), predictor, round(auc_value, 3), pos)
                                } else {
                                    sprintf(.("%1$s has an AUC of %2$s, below the 0.5 expected from a coin toss, but its confidence interval still crosses 0.5. \u2022 About half of markers carrying no information at all fall below 0.5 by chance, so this is not evidence that the marker runs backwards. \u2022 It is consistent with a marker that simply does not discriminate. \u2022 If you expected it to run the other way, first check Positive class (is %3$s the level that means the condition is present?), then Direction, before drawing any conclusion."), predictor, round(auc_value, 3), pos)
                                }
                                private$.addNotice(
                                    type = "STRONG_WARNING",
                                    title = sprintf(.("AUC Below Chance: %s"), predictor),
                                    content = below_text
                                )
                            }
                        } else if (!constant && round(auc_value, 3) < 0.7) {
                            # No longer suggests switching Direction: at 0.5 <= AUC < 0.7 that gives
                            # 1 - AUC, below chance. F-44: the 0.7 edge is the Hosmer-Lemeshow banding
                            # the AUC table quotes, so the bands caveat goes with it; the advice to add
                            # predictors or build multivariable models does not belong in a single-
                            # marker accuracy analysis. A clause when the interval includes 0.5.
                            ci_lim <- tryCatch(as.numeric(roc_obj$ci), error = function(e) NULL)
                            ci_has_half <- is.null(ci_issue) && length(ci_lim) == 3 && is.finite(ci_lim[1]) && round(ci_lim[1], 3) <= 0.5
                            private$.addNotice(
                                type = "STRONG_WARNING",
                                title = sprintf(.("Limited Diagnostic Performance: %s"), predictor),
                                content = paste(c(sprintf(.("Limited diagnostic performance for %1$s: AUC=%2$s. \u2022 AUC below 0.7 indicates limited discriminative ability. \u2022 Check data quality and the coding of the outcome and the marker."), predictor, round(auc_value, 3)),
                                                  if (ci_has_half) paste("\u2022", .("The confidence interval includes 0.5, so these data are compatible with no discrimination.")),
                                                  paste("\u2022", private$.aucBandsCaveat())),
                                                collapse = " ")
                            )
                        }

                        # Extreme prevalence notice
                        # Prevalence belongs to the complete-case sample, so one banner, not
                        # one per predictor.
                        if ((prevalence < 0.05 || prevalence > 0.95) && !isTRUE(private$.prevalenceNoticeShown)) {
                            private$.prevalenceNoticeShown <- TRUE
                            # F-17: one whole sentence per side (the band word and the PPV/NPV clause
                            # were spliced in). ROC, AUC, sensitivity and specificity are computed
                            # within one outcome group each, so prevalence does not bias them; the
                            # predictive value on the rare side is LOW (not "unreliable"), and the
                            # small group makes its own rate imprecise. "The ROC curve can look
                            # reassuring" holds for rare positives only (Saito & Rehmsmeier 2015).
                            private$.addNotice(
                                type = "STRONG_WARNING",
                                title = .("Extreme Prevalence"),
                                content = if (prevalence < 0.05) {
                                    sprintf(.("Only %1$s%% of the analysed patients are positive. \u2022 Sensitivity, specificity and the AUC do not depend on prevalence, but with so few positives sensitivity is imprecise, and the PPV will be low unless specificity is very high. \u2022 When negatives far outnumber positives the ROC curve can look reassuring while most positive results are false positives; the Precision-Recall curve shows precision directly and complements the ROC curve."), round(prevalence * 100, 1))
                                } else {
                                    sprintf(.("%1$s%% of the analysed patients are positive. \u2022 Sensitivity, specificity and the AUC do not depend on prevalence, but with so few negatives specificity is imprecise, and the NPV will be low unless sensitivity is very high."), round(prevalence * 100, 1))
                                }
                            )
                        }
                    },
                    error = function(e) {
                        # Provide specific error messages based on common ROC analysis issues
                        error_msg <- if (grepl("No controls", e$message, ignore.case = TRUE)) {
                            sprintf(.("No control observations found for predictor '%s'. Check that your outcome variable has both positive and negative cases."), predictor)
                        } else if (grepl("No cases", e$message, ignore.case = TRUE)) {
                            sprintf(.("No case observations found for predictor '%s'. Check that your outcome variable has both positive and negative cases."), predictor)
                        } else if (grepl("identical", e$message, ignore.case = TRUE)) {
                            sprintf(.("Predictor '%s' has identical values across all observations. ROC analysis requires variation in predictor values."), predictor)
                        } else if (grepl("missing", e$message, ignore.case = TRUE)) {
                            sprintf(.("Missing values detected in predictor '%s' or outcome variable. Please check your data."), predictor)
                        } else {
                            sprintf(.("ROC analysis failed for predictor '%1$s': %2$s. Please check your data quality and variable selection."), predictor, conditionMessage(e))
                        }

                        # Previously written only into the instructions panel, so a failed
                        # predictor left "Analysis Complete" as the only banner.
                        private$.addNotice(
                            type = "WARNING",
                            title = sprintf(.("ROC Analysis Failed: %s"), predictor),
                            content = error_msg
                        )
                    }
                )
            }
        },

        # Helper function to apply cutoff respecting pROC direction convention
        .applyDirectionCutoff = function(predictor_values, cutoff, roc_direction) {
            # pROC direction "<" means controls < cases: HIGHER values = positive (disease)
            # pROC direction ">" means controls > cases: LOWER values = positive (disease)
            # pROC's own rule (see pROC::coords): direction "<" classifies predictor >= t as
            # positive; direction ">" classifies predictor <= t as positive. This used to test
            # `< cutoff` for ">", so at a cutoff equal to an observed value the confusion matrix
            # disagreed with the sensitivity/specificity pROC printed beside it.
            if (roc_direction == "<") {
                return(predictor_values >= cutoff)
            } else {
                return(predictor_values <= cutoff)
            }
        },
        # The best cut-point and every cut-point tied with it (VAL-08). `cr` is coords("all") with
        # the 2x2 counts; `keep` marks the admissible rows. Ties are found exactly from the counts,
        # J = 1 - (fn*n0 + fp*n1) / (n1*n0): a floating-point J split two equal optima by rounding
        # (0.29333333333333345 vs ...322). This module's convention, stated in the tie notice and
        # the Methods text: among tied optima the point closest to the top-left corner, then the
        # smallest cutoff - unchanged when the positive class and Direction are swapped.
        .pickCutoff = function(cr, n1, n0, keep, criterion) {
            u <- as.numeric(cr$fn) * n0   # (1 - Se) * n1 * n0, an integer held as a double (no overflow)
            v <- as.numeric(cr$fp) * n1   # (1 - Sp) * n1 * n0
            if (identical(criterion, "youden")) {
                key <- -(u + v)
                near <- abs(u - v)   # at equal u + v the corner distance grows with |u - v|
            } else {
                # ponytail: exact while (n1*n0)^2 < 2^52 (about 8,000 per class); beyond that a
                # corner tie is decided by rounding.
                key <- -(u^2 + v^2)
                near <- numeric(length(u))
            }
            key[!keep] <- -Inf
            tied <- which(key == max(key))
            list(index = tied[order(near[tied], cr$threshold[tied])][1], tied = tied)
        },
        .calculateOptimalCutoff = function(roc_obj, data, predictor) {
            # Determine thresholds based on presets or options
            sens_threshold <- self$options$sensitivityThreshold %||% 0
            spec_threshold <- self$options$specificityThreshold %||% 0
            use_optimization <- isTRUE(self$options$youdenOptimization)

            if (!is.null(private$.presetConfig)) {
                config <- private$.presetConfig
                user_sens <- sens_threshold
                user_spec <- spec_threshold
                user_youden <- use_optimization
                # Force optimization when using presets
                use_optimization <- TRUE

                if (config$focus == "sensitivity") {
                    sens_threshold <- config$threshold
                    spec_threshold <- 0
                } else if (config$focus == "specificity") {
                    sens_threshold <- 0
                    spec_threshold <- config$threshold
                } else if (config$focus == "balanced") {
                    sens_threshold <- config$sens_threshold
                    spec_threshold <- config$spec_threshold
                }

                # A preset overwrites the minimum-sensitivity and minimum-specificity boxes and
                # forces Youden optimisation on, but those controls stay visible and keep showing
                # the user's own values - so the analysis silently ignored what they typed. Say
                # what the preset actually did, once, and only when it really changed something.
                changed <- character(0)
                if (!isTRUE(all.equal(user_sens, sens_threshold))) {
                    changed <- c(changed, sprintf(.("minimum sensitivity %1$s -> %2$s"), user_sens, sens_threshold))
                }
                if (!isTRUE(all.equal(user_spec, spec_threshold))) {
                    changed <- c(changed, sprintf(.("minimum specificity %1$s -> %2$s"), user_spec, spec_threshold))
                }
                if (!isTRUE(user_youden)) {
                    changed <- c(changed, .("Youden index optimization turned on"))
                }
                preset_label <- private$.choiceLabel("clinicalPresets", self$options$clinicalPresets)
                if (length(changed) > 0 && !isTRUE(private$.presetNoticeShown)) {
                    private$.presetNoticeShown <- TRUE
                    private$.addNotice(
                        type = "WARNING",
                        title = .("Clinical Preset Overrode Your Settings"),
                        content = sprintf(.("The '%1$s' preset replaced settings you can still see in the options panel: %2$s. Choose the Custom Configuration preset if you want the values you entered to be used."), preset_label, paste(changed, collapse = "; "))
                    )
                }
            }

            # Every threshold with its 2x2 counts: tied cut-points are found exactly from the counts
            # (VAL-08), never from a floating-point J or distance.
            cr <- pROC::coords(roc_obj, "all", ret = c("threshold", "sensitivity", "specificity", "tp", "tn", "fp", "fn"))
            # pROC rebuilds the counts from the rates (tp <- se * ncases), e.g. fn = 10.999999999999998;
            # round them once so every comparison below is exact.
            cr[c("tp", "tn", "fp", "fn")] <- round(cr[c("tp", "tn", "fp", "fn")])
            n1 <- length(roc_obj$cases)
            n0 <- length(roc_obj$controls)
            J <- cr$sensitivity + cr$specificity - 1
            everything <- rep(TRUE, nrow(cr))
            min_sens <- 0
            min_spec <- 0

            if (!use_optimization) {
                # With Youden optimisation off, the default best.method of coords("best") was
                # STILL youden, so the toggle changed nothing except dropping the constraints.
                # Use the other standard criterion and label the row accordingly.
                method <- "closest_topleft"
                pick <- private$.pickCutoff(cr, n1, n0, everything, method)
            } else {
                method <- "youden"
                # Sensitivity/specificity thresholds are hard constraints on the search.
                valid_indices <- everything
                if (sens_threshold > 0) {
                    valid_indices <- valid_indices & (cr$sensitivity >= sens_threshold)
                }
                if (spec_threshold > 0) {
                    valid_indices <- valid_indices & (cr$specificity >= spec_threshold)
                }
                # The +Inf row (test nobody, Sp 1) and the -Inf row (test everybody, Se 1) meet any
                # single minimum, so the fallback below could never fire and Inf was reported.
                if (sens_threshold > 0 || spec_threshold > 0) {
                    valid_indices <- valid_indices & is.finite(cr$threshold)
                }
                min_sens <- sens_threshold
                min_spec <- spec_threshold

                # If no thresholds meet constraints, use unconstrained optimization
                if (!any(valid_indices)) {
                    private$.addNotice(type = "WARNING", title = sprintf(.("Constraints Not Met: %s"), predictor), content = sprintf(.("No cutoffs meet the specified sensitivity/specificity constraints for %s. Using unconstrained Youden optimization instead."), predictor))
                    valid_indices <- everything
                    min_sens <- 0
                    min_spec <- 0
                }
                pick <- private$.pickCutoff(cr, n1, n0, valid_indices, method)

                # When the unconstrained Youden maximum satisfies the constraints, nothing changes
                # and there is nothing to say. When it does not, the row still prints under the
                # heading "Youden Index" while holding a smaller, constrained value - measured on
                # skewed marker distributions this lowered J by up to 0.09 (0.718 -> 0.630) with
                # nothing on screen saying a constraint had been applied. The free optimum follows
                # the same tie rule; "higher J" is compared exactly (smaller fn*n0 + fp*n1).
                opt <- pick$index
                free <- private$.pickCutoff(cr, n1, n0, everything, method)$index
                if (cr$fn[free] * n0 + cr$fp[free] * n1 < cr$fn[opt] * n0 + cr$fp[opt] * n1) {
                    private$.addNotice(
                        type = "WARNING",
                        title = sprintf(.("Cutoff Constrained by Your Thresholds: %s"), predictor),
                        content = sprintf(.("The reported cutoff for %1$s is the best one meeting your minimum sensitivity (%2$s) and specificity (%3$s), not the overall Youden optimum. Reported: cutoff %4$s, sensitivity %5$s, specificity %6$s, Youden index %7$s. Unconstrained optimum: cutoff %8$s, sensitivity %9$s, specificity %10$s, Youden index %11$s. Set both minimums to 0 to search without constraints."), predictor, sens_threshold, spec_threshold, signif(cr$threshold[opt], 4), round(cr$sensitivity[opt], 3), round(cr$specificity[opt], 3), round(J[opt], 3), signif(cr$threshold[free], 4), round(cr$sensitivity[free], 3), round(cr$specificity[free], 3), round(J[free], 3))
                    )
                }
            }

            i <- pick$index
            optimal_cutoff <- cr$threshold[i]
            # Tied optima are different clinical rules (score >= 2 vs >= 3): say which one was
            # reported and why. Not when the cut-point is no better than chance - the 'chance' note
            # under the table covers that, and a tie between useless cut-points says nothing.
            if (length(pick$tied) > 1 && round(J[i], 3) > 0) {
                tied_txt <- paste(signif(sort(cr$threshold[pick$tied]), 4), collapse = ", ")
                private$.addNotice(
                    type = "INFO",
                    title = sprintf(.("Tied Best Cutoff: %s"), predictor),
                    content = if (identical(method, "youden")) {
                        sprintf(.("%1$s cutoffs for %2$s reach the same highest Youden index (%3$s): %4$s. By this module's convention the one closest to the top-left corner of the ROC curve is reported (cutoff %5$s), and the smallest cutoff if that still ties; the others are equally good by the Youden criterion."), length(pick$tied), predictor, round(J[i], 3), tied_txt, signif(optimal_cutoff, 4))
                    } else {
                        sprintf(.("%1$s cutoffs for %2$s are equally close to the top-left corner of the ROC curve: %3$s. By this module's convention the smallest of them is reported (cutoff %4$s); the others are equally good by this criterion."), length(pick$tied), predictor, tied_txt, signif(optimal_cutoff, 4))
                    }
                )
            }

            # CRITICAL FIX: Calculate confusion matrix at optimal cutoff respecting ROC direction
            predictions <- ifelse(
                private$.applyDirectionCutoff(data[[predictor]], optimal_cutoff, roc_obj$direction),
                levels(data[[private$.outcome]])[2],
                levels(data[[private$.outcome]])[1]
            )
            predictions <- factor(predictions, levels = levels(data[[private$.outcome]]))

            cm <- .quietly(caret::confusionMatrix(predictions, data[[private$.outcome]],
                positive = levels(data[[private$.outcome]])[2]
            ))

            # Access confusion matrix by level name for robustness
            pos_lvl <- levels(data[[private$.outcome]])[2]
            neg_lvl <- levels(data[[private$.outcome]])[1]
            return(list(
                method = method,
                cutoff = optimal_cutoff,
                sensitivity = cr$sensitivity[i],
                specificity = cr$specificity[i],
                youden_index = J[i],
                # the minimums the search actually applied (0 when none, or none could be met)
                min_sens = min_sens,
                min_spec = min_spec,
                accuracy = as.numeric(cm$overall["Accuracy"]),
                confusion_matrix = cm,
                true_positive = cm$table[pos_lvl, pos_lvl],
                true_negative = cm$table[neg_lvl, neg_lvl],
                false_positive = cm$table[pos_lvl, neg_lvl],
                false_negative = cm$table[neg_lvl, pos_lvl]
            ))
        },
        .evaluateCustomCutoffs = function(roc_obj, data, predictor) {
            # Evaluate custom cutoffs if specified
            if (is.null(self$options$customCutoffs) || self$options$customCutoffs == "") {
                return(NULL)
            }

            tryCatch(
                {
                    # Parse custom cutoffs
                    custom_cuts <- as.numeric(trimws(strsplit(self$options$customCutoffs, ",")[[1]]))
                    custom_cuts <- custom_cuts[!is.na(custom_cuts)] # Remove invalid values

                    if (length(custom_cuts) == 0) {
                        return(NULL)
                    }

                    # Evaluate each custom cutoff
                    custom_results <- data.frame(
                        cutoff = numeric(0),
                        sensitivity = numeric(0),
                        specificity = numeric(0),
                        youden_index = numeric(0),
                        accuracy = numeric(0),
                        ppv = numeric(0),
                        npv = numeric(0)
                    )

                    outcome_var <- data[[private$.outcome]]
                    predictor_var <- data[[predictor]]

                    for (cutoff in custom_cuts) {
                        # CRITICAL FIX: Create predictions respecting ROC direction
                        predictions <- ifelse(
                            private$.applyDirectionCutoff(predictor_var, cutoff, roc_obj$direction),
                            levels(outcome_var)[2],
                            levels(outcome_var)[1]
                        )
                        predictions <- factor(predictions, levels = levels(outcome_var))

                        cm <- .quietly(caret::confusionMatrix(predictions, outcome_var,
                            positive = levels(outcome_var)[2]
                        ))

                        pos_lvl <- levels(outcome_var)[2]
                        neg_lvl <- levels(outcome_var)[1]
                        tp <- as.numeric(cm$table[pos_lvl, pos_lvl])
                        tn <- as.numeric(cm$table[neg_lvl, neg_lvl])
                        fp <- as.numeric(cm$table[pos_lvl, neg_lvl])
                        fn <- as.numeric(cm$table[neg_lvl, pos_lvl])
                        sens <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
                        spec <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
                        ppv_val <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
                        npv_val <- if ((tn + fn) > 0) tn / (tn + fn) else NA_real_

                        # Store results
                        custom_results <- rbind(custom_results, data.frame(
                            cutoff = cutoff,
                            sensitivity = sens,
                            specificity = spec,
                            youden_index = sens + spec - 1,
                            accuracy = as.numeric(cm$overall["Accuracy"]),
                            ppv = ppv_val,
                            npv = npv_val
                        ))
                    }

                    return(custom_results)
                },
                error = function(e) {
                    private$.addNotice(type = "WARNING", title = sprintf(.("Custom Cutoff Error: %s"), predictor), content = sprintf(.("Failed to evaluate custom cutoffs for %1$s: %2$s"), predictor, e$message))
                    return(NULL)
                }
            )
        },
        .populateAUCSummary = function() {
            aucTable <- self$results$results$aucSummary
            .deleteTableRows(aucTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates
            private$.noteDirection(aucTable, private$.rocObjects)
            aucTable$setNote("seed", if (isTRUE(self$options$useBootstrap)) .fmt(.("Random seed: {seed}"), seed = self$options$seed))
            # What the AUC means depends on the direction each marker was read in (VAL-06): one
            # sentence when all markers share it, one per marker otherwise.
            dirs <- vapply(private$.rocResults, function(r) r$roc$direction, character(1))
            meaning <- if (length(unique(dirs)) == 1L) {
                private$.aucMeaning(dirs[[1]])
            } else {   # several directions, or none when every predictor failed
                vapply(names(dirs), function(p) private$.aucMeaning(dirs[[p]], .rocLabel(p)), character(1))
            }
            aucTable$setNote(
                "context_reference",
                paste(c(meaning,
                      .("The AUC in Context column restates it as a percentage, and the AUC Interpretation column places the AUC as displayed in a band that includes its lower edge: Excellent from 0.9, Good from 0.8, Fair from 0.7, Poor from 0.6, Minimal from 0.5, Below chance under 0.5."),
                      private$.aucBandsCaveat(),
                      .("Both are in-sample estimates from these data; the AUC Lower CI and AUC Upper CI columns show how precisely this sample pins the AUC down.")), collapse = " ")
            )

            # A failed bootstrap can leave some rows with DeLong limits. Match the
            # standard error and the method note to each row's actual interval.
            has_boot <- any(vapply(private$.rocResults, function(r) {
                identical(attr(r$roc$ci, "method"), "bootstrap")
            }, logical(1)))
            aucTable$setNote("ci_method", paste(.rocLabel(private$.aucCiText()), collapse = "\n\n"))
            aucTable$setNote(
                "std_error_blank",
                if (has_boot) {
                    .("Std. Error is empty for rows whose confidence limits are bootstrap limits. DeLong's standard error is shown only beside an estimable DeLong interval, including a DeLong fallback after bootstrap failure. Read bootstrap precision from the AUC Lower CI and AUC Upper CI columns.")
                } else {
                    NULL
                }
            )

            for (predictor in names(private$.rocResults)) {
                result <- private$.rocResults[[predictor]]
                roc_obj <- result$roc
                use_boot <- identical(attr(roc_obj$ci, "method"), "bootstrap")

                # AUC interpretation
                auc_value <- as.numeric(roc_obj$auc)
                auc_interp <- private$.interpretAUC(auc_value)
                clinical_utility <- private$.assessClinicalUtility(auc_value, roc_obj$direction)

                # TODO (UX): predictor names reach cells raw, and a cell is parsed as HTML: a column
                # named "<LOD flag" shows as blank (no XSS - the client keeps only em/i/sub/sup). Wrap
                # with .rocLabel(x, note = FALSE) at the cell, never at the row key or a lookup, as
                # outcomeCoding, .frozenRow() and the multi-class class cells do. Same at L2367,
                # L2446, L2466, L2504, L2546, L2727, L2797-2798, L2899 (a/b), L3014 (a/b), L3132, L3183,
                # L3222, L3469, L3981, L5517, L5623, L6050, L6120, L6325, L6333, L6341 (18 tables).
                row <- list(
                    predictor = predictor,
                    auc = auc_value,
                    auc_lower = if (!is.null(roc_obj$ci)) roc_obj$ci[1] else NA,
                    auc_upper = if (!is.null(roc_obj$ci)) roc_obj$ci[3] else NA,
                    std_error = if (use_boot) NA_real_ else tryCatch(sqrt(pROC::var(roc_obj)), error = function(e) NA_real_),
                    auc_interpretation = auc_interp,
                    clinical_utility = clinical_utility
                )

                key <- private$.escapeVar(predictor)
                aucTable$addRow(rowKey = key, values = row)
                # No zero-width or missing interval as an estimate (VAL-04); the SE under bootstrap is
                # blank by design (std_error_blank note).
                if (!is.null(result$ci_issue)) {
                    private$.blankCells(aucTable, key, c("auc_lower", "auc_upper", if (!use_boot) "std_error"), result$ci_issue$blank)
                }
                if (use_boot) private$.blankCells(aucTable, key, "std_error")
            }

            # Ties count one half in the AUC (VAL-14). Report the share of tied case-control pairs
            # and the AUC if every tie went to the cases or to the non-cases. Tied pairs are
            # counted per distinct value (cases x non-cases), never by an O(n^2) pair loop.
            tie_text <- character(0)
            for (predictor in names(private$.rocResults)) {
                roc_obj <- private$.rocResults[[predictor]]$roc
                u <- unique(c(roc_obj$cases, roc_obj$controls))
                tied <- sum(as.numeric(tabulate(match(roc_obj$cases, u), length(u))) *
                            tabulate(match(roc_obj$controls, u), length(u)))
                p_tie <- tied / (length(roc_obj$cases) * length(roc_obj$controls))
                if (is.finite(p_tie) && p_tie > 0) {
                    auc_value <- as.numeric(roc_obj$auc)
                    tie_text <- c(tie_text, sprintf(.("%1$s: %2$s%% of case-control pairs are tied; ties count one half. If every tie favoured the cases the AUC would be %3$s, if every tie favoured the non-cases %4$s."),
                        .rocLabel(predictor), base::format(signif(100 * p_tie, 2), scientific = FALSE),
                        sprintf("%.3f", auc_value + p_tie / 2), sprintf("%.3f", auc_value - p_tie / 2)))
                }
            }
            if (length(tie_text)) tie_text <- c(tie_text, .("These are bounds for how ties are resolved, not a confidence interval."))
            aucTable$setNote("ties", if (length(tie_text)) paste(tie_text, collapse = " "))
        },
        .populateOptimalCutoffs = function() {
            cutoffTable <- self$results$results$optimalCutoffSummary
            .deleteTableRows(cutoffTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates
            cutoffTable$setNote(
                "cutpoint_optimism",
                paste(c(.("Sensitivity, specificity and accuracy are reported at a cutpoint that was searched for on these same data, so they are optimistic; they are not cross-validated or externally validated estimates."),
                        # the intervals ignore the search too (method-choice concern 3)
                        if (isTRUE(self$options$diagnosticMetrics)) .("The Clopper-Pearson intervals for sensitivity and specificity in the Diagnostic Performance table treat this cutpoint as fixed in advance, so they are centred on these optimistic values and understate the uncertainty.")),
                      collapse = " ")
            )
            # Which side of the cutoff is positive, and what a cutoff is: pROC's thresholds are the
            # midpoints between adjacent observed values (6.5 on an integer score), and the counts
            # use >= (higher = positive) or <= (lower = positive), as .applyDirectionCutoff() does.
            dirs <- vapply(private$.rocObjects[names(private$.rocResults)], function(r) r$direction, character(1))
            cutoffTable$setNote("rule", paste(
                if (all(dirs == "<")) .("A patient is test-positive when the marker is at or above the cutoff.")
                else if (all(dirs == ">")) .("A patient is test-positive when the marker is at or below the cutoff.")
                else .("A patient is test-positive when the marker is at or above the cutoff for a marker read with higher values positive, and at or below it for one read with lower values positive."),
                .("Cutoffs lie midway between adjacent observed values, so a reported cutoff need not occur in the data.")))

            for (predictor in names(private$.rocResults)) {
                result <- private$.rocResults[[predictor]]
                if (isTRUE(result$constant)) next   # no cut-point exists; its ERROR says so
                optimal <- result$optimal_cutoff

                # Descriptive performance summary for the selected context (no clinical advice)
                clinical_rec <- private$.generateClinicalRecommendation(
                    optimal, self$options$clinicalContext, predictor
                )

                row <- list(
                    predictor = predictor,
                    optimal_cutoff = optimal$cutoff,
                    youden_index = optimal$youden_index,
                    sensitivity = optimal$sensitivity,
                    specificity = optimal$specificity,
                    accuracy = optimal$accuracy,
                    clinical_recommendation = clinical_rec
                )

                cutoffTable$addRow(rowKey = private$.escapeVar(predictor), values = row)
            }

            # A reported cutoff with J <= 0 at the displayed precision is no better than chance;
            # say so, and why, per predictor (VAL-04).
            chance <- character(0)
            for (predictor in names(private$.rocResults)) {
                r <- private$.rocResults[[predictor]]
                if (isTRUE(r$constant)) next   # no cut-point exists; its ERROR says so
                if (!isTRUE(round(r$optimal_cutoff$youden_index, 3) <= 0)) next
                chance <- c(chance, sprintf(.("%s: the selected cutoff has J <= 0 (no better than chance)."), .rocLabel(predictor)))
                if (round(max(r$roc$sensitivities + r$roc$specificities - 1), 3) <= 0) {
                    chance <- c(chance, .("No threshold separates the groups better than chance in the direction used."))
                }
                if (is.infinite(r$optimal_cutoff$cutoff)) {
                    chance <- c(chance, .("The cutoff is infinite, so everyone is classified the same way."))
                }
            }
            cutoffTable$setNote("chance", if (length(chance)) paste(chance, collapse = " "))
        },

        # Helper function to calculate confusion matrix values for a given cutoff
        .calculateConfusionMatrix = function(predictor, cutoff) {
            # Get the actual data for this predictor
            predictor_data <- private$.analysisData[[predictor]]
            outcome_data <- private$.analysisData[[private$.outcome]]

            # Get the ROC result to determine direction
            roc_result <- private$.rocResults[[predictor]]
            roc_obj <- roc_result$roc

            # Use ROC direction to determine how to apply cutoff
            # pROC direction "<" means controls < cases: higher values = positive
            # pROC direction ">" means controls > cases: lower values = positive
            predicted_positive <- private$.applyDirectionCutoff(predictor_data, cutoff, roc_obj$direction)

            # .prepareData() always sets the positive class before any binary table is filled
            actual_positive <- outcome_data == private$.positiveClass

            # Calculate confusion matrix components
            tp <- sum(predicted_positive & actual_positive, na.rm = TRUE)
            tn <- sum(!predicted_positive & !actual_positive, na.rm = TRUE)
            fp <- sum(predicted_positive & !actual_positive, na.rm = TRUE)
            fn <- sum(!predicted_positive & actual_positive, na.rm = TRUE)

            return(list(
                true_positive = as.integer(tp),
                true_negative = as.integer(tn),
                false_positive = as.integer(fp),
                false_negative = as.integer(fn)
            ))
        },
        .populateCutoffAnalysis = function() {
            cutoffTable <- self$results$results$cutoffAnalysis
            # This table still builds its rows in .run(): the row count is not knowable in
            # advance, so deleteRows() is required or every run would stack duplicates.
            .deleteTableRows(cutoffTable)

            for (predictor in names(private$.rocResults)) {
                result <- private$.rocResults[[predictor]]
                if (isTRUE(result$constant)) next   # no cut-point exists; its ERROR says so
                roc_obj <- result$roc
                optimal <- result$optimal_cutoff
                custom_cutoffs <- result$custom_cutoffs

                # Calculate confusion matrix for optimal cutoff
                optimal_cm <- private$.calculateConfusionMatrix(predictor, optimal$cutoff)

                # Add optimal cutoff first
                row <- list(
                    predictor = predictor,
                    cutoff = optimal$cutoff,
                    cutoff_type = if (identical(optimal$method, "closest_topleft")) .("Optimal (closest to top-left)") else .("Optimal (Youden)"),
                    sensitivity = optimal$sensitivity,
                    specificity = optimal$specificity,
                    one_minus_specificity = 1 - optimal$specificity,
                    youden_index = optimal$youden_index,
                    true_positive = optimal_cm$true_positive,
                    true_negative = optimal_cm$true_negative,
                    false_positive = optimal_cm$false_positive,
                    false_negative = optimal_cm$false_negative
                )
                cutoffTable$addRow(rowKey = paste(private$.escapeVar(predictor), "optimal", sep = "_"), values = row)

                # Add custom cutoffs if available
                if (!is.null(custom_cutoffs) && nrow(custom_cutoffs) > 0) {
                    for (i in seq_len(nrow(custom_cutoffs))) {
                        custom_cm <- private$.calculateConfusionMatrix(predictor, custom_cutoffs$cutoff[i])

                        custom_row <- list(
                            predictor = predictor,
                            cutoff = custom_cutoffs$cutoff[i],
                            cutoff_type = .fmt(.("Custom {i}"), i = i),
                            sensitivity = custom_cutoffs$sensitivity[i],
                            specificity = custom_cutoffs$specificity[i],
                            one_minus_specificity = 1 - custom_cutoffs$specificity[i],
                            youden_index = custom_cutoffs$youden_index[i],
                            true_positive = custom_cm$true_positive,
                            true_negative = custom_cm$true_negative,
                            false_positive = custom_cm$false_positive,
                            false_negative = custom_cm$false_negative
                        )
                        cutoffTable$addRow(rowKey = paste(private$.escapeVar(predictor), "custom", i, sep = "_"), values = custom_row)
                    }
                }

                # Get additional representative cutoffs from ROC curve
                coords_result <- pROC::coords(roc_obj, "all",
                    ret = c("threshold", "sensitivity", "specificity")
                )
                # pROC's first and last thresholds are -Inf / Inf ("test everyone" / "test nobody"):
                # curve ends, not cut-offs a clinician can apply, and the even sampling below always hit them.
                coords_result <- coords_result[is.finite(coords_result$threshold), , drop = FALSE]

                # Sample additional cutoffs for display (max 15 more rows per predictor)
                n_cutoffs <- min(nrow(coords_result), 15)
                sample_indices <- seq(1, nrow(coords_result), length.out = n_cutoffs)
                sample_indices <- round(sample_indices)

                for (i in sample_indices) {
                    sens <- coords_result$sensitivity[i]
                    spec <- coords_result$specificity[i]
                    cutoff <- coords_result$threshold[i]

                    # Calculate Youden Index
                    youden <- sens + spec - 1

                    # Calculate confusion matrix values using helper function
                    cm <- private$.calculateConfusionMatrix(predictor, cutoff)

                    row <- list(
                        predictor = predictor,
                        cutoff = cutoff,
                        cutoff_type = .("Sampled"),
                        sensitivity = sens,
                        specificity = spec,
                        one_minus_specificity = 1 - spec,
                        youden_index = youden,
                        true_positive = cm$true_positive,
                        true_negative = cm$true_negative,
                        false_positive = cm$false_positive,
                        false_negative = cm$false_negative
                    )

                    cutoffTable$addRow(rowKey = paste(private$.escapeVar(predictor), i, sep = "_"), values = row)
                }
            }
        },
        .populateDiagnosticPerformance = function() {
            diagTable <- self$results$results$diagnosticPerformance
            .deleteTableRows(diagTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates
            diagTable$setNote("ci_method", sprintf(.("Sensitivity and specificity intervals are %s%% Clopper-Pearson exact binomial intervals (Clopper & Pearson 1934). They treat the cutoff as fixed in advance, although it was chosen on these data, so they understate the uncertainty."), self$options$confidenceLevel))

            for (predictor in names(private$.rocResults)) {
                result <- private$.rocResults[[predictor]]
                if (isTRUE(result$constant)) next   # no cut-point exists; its ERROR says so
                optimal <- result$optimal_cutoff
                cm <- optimal$confusion_matrix

                # Calculate confidence intervals for sensitivity and specificity (exact)
                sens_ci <- private$.calculateBinomialCI(
                    optimal$true_positive,
                    optimal$true_positive + optimal$false_negative
                )
                spec_ci <- private$.calculateBinomialCI(
                    optimal$true_negative,
                    optimal$true_negative + optimal$false_positive
                )

                # Calculate balanced accuracy
                balanced_acc <- (optimal$sensitivity + optimal$specificity) / 2

                row <- list(
                    predictor = predictor,
                    cutoff = optimal$cutoff,
                    sensitivity = optimal$sensitivity,
                    sensitivity_ci = paste0("(", round(sens_ci[1], 3), ", ", round(sens_ci[2], 3), ")"),
                    specificity = optimal$specificity,
                    specificity_ci = paste0("(", round(spec_ci[1], 3), ", ", round(spec_ci[2], 3), ")"),
                    accuracy = optimal$accuracy,
                    balanced_accuracy = balanced_acc
                )

                diagTable$addRow(rowKey = private$.escapeVar(predictor), values = row)
            }
        },
        # Single source of truth for the predicted risk behind calibration, Brier, E/O,
        # Hosmer-Lemeshow and the decision and clinical impact curves. Prediction scale declares
        # what the values are (VAL-12): any marker inside [0,1] - a Ki-67 fraction - used to be
        # graded as a calibrated risk, and flipped by Direction. Probability: the values as
        # supplied, never reversed; `probs` is NULL when one lies outside 0-1, so that predictor
        # gets none of those outputs. Score: glm(y ~ x) fitted to these data (in-sample), `lp` its
        # linear predictor. Raises no notice: .riskNotices() speaks once per run.
        # A one-predictor logistic fit is invariant to Direction (glm(y ~ x) and glm(y ~ -x) give
        # the same risks), so a slope against Direction is still the risk model the data support.
        # Until 2026-10-21 such a slope was replaced by the sample prevalence for everyone: an
        # AUC-0.918 marker with one outlying control drew its decision curve on treat-all and a
        # scaled Brier of 0, under its own name. .riskNotices() now discloses the reversal
        # (.riskReversed()); only a non-estimable slope (a constant marker) gets the prevalence.
        # Returns $probs, $lp, $assumed_probability (TRUE: the values are used as supplied) and
        # $constant (TRUE: the slope was not estimable).
        .riskProbabilities = function(pred_vals, y_binary) {
            if (identical(self$options$predictionScale, "probability")) {
                ok <- all(pred_vals >= 0 & pred_vals <= 1, na.rm = TRUE)
                return(list(probs = if (ok) pred_vals, assumed_probability = TRUE))
            }
            model <- stats::glm(y_binary ~ pred_vals, family = stats::binomial())
            slope <- unname(stats::coef(model)[2])
            if (!is.finite(slope)) {
                p_bar <- mean(y_binary, na.rm = TRUE)
                lp_bar <- stats::qlogis(pmin(pmax(p_bar, 1e-6), 1 - 1e-6))
                n_obs <- length(stats::fitted(model))
                return(list(
                    probs = rep(p_bar, n_obs),
                    lp = rep(lp_bar, n_obs),
                    assumed_probability = FALSE,
                    constant = TRUE
                ))
            }
            list(probs = stats::fitted(model), lp = stats::predict(model, type = "link"),
                 assumed_probability = FALSE, constant = FALSE)
        },

        # Sign of the fitted slope of glm(y ~ x), exactly: the profile log-likelihood is concave,
        # so it is the sign of the score at zero, n0 * sum(x of cases) - n1 * sum(x of controls)
        # (F-25). glm() returned -3.7e-17 for an exactly zero slope; a zero slope is 0 here.
        # ponytail: the 1e-12 relative tolerance absorbs summation rounding for non-integer data.
        .slopeSign = function(y, x) {
            n1 <- sum(y == 1)
            n0 <- sum(y == 0)
            s <- n0 * sum(x[y == 1]) - n1 * sum(x[y == 0])
            if (abs(s) <= 1e-12 * n0 * n1 * max(abs(x))) 0 else sign(s)
        },

        # Score scale: the predictors whose fitted logistic risk runs against the direction the
        # ROC tables read them in (Direction, or the direction auto chose). Named AUC vector.
        .riskReversed = function() {
            if (identical(self$options$predictionScale, "probability")) return(numeric())
            data <- private$.analysisData
            y <- as.integer(data[[private$.outcome]] == levels(data[[private$.outcome]])[2])
            out <- numeric()
            for (p in names(private$.rocResults)) {
                if (isTRUE(private$.rocResults[[p]]$constant)) next
                s <- private$.slopeSign(y, data[[p]])
                higher <- !identical(private$.rocObjects[[p]]$direction, ">")
                if (s != 0 && (s > 0) != higher) out[p] <- as.numeric(private$.rocResults[[p]]$roc$auc)
            }
            out
        },

        # The notices for .riskProbabilities(), once per run and only when an output reads a risk.
        .riskNotices = function() {
            o <- self$options
            if (!(isTRUE(o$calibrationAnalysis) || isTRUE(o$decisionImpactCurves) ||
                  (isTRUE(o$clinicalImpact) && isTRUE(o$clinicalUtilityCurve)) ||
                  isTRUE(o$internalValidation))) return()
            preds <- names(private$.rocResults)
            if (!length(preds)) return()
            in_unit <- vapply(preds, function(p) all(private$.analysisData[[p]] >= 0 & private$.analysisData[[p]] <= 1, na.rm = TRUE), logical(1))
            pos <- private$.positiveClass
            # The fitted risk runs against the direction of the ROC tables (.riskProbabilities()
            # keeps it). Below 0.5 the ranks agree with the fit: the data run opposite to the
            # direction read, and the risk outputs flatter it. At or above 0.5 the ranks (AUC) and
            # the means (slope) disagree - a skewed marker or an outlier - and the risk outputs
            # describe the reversed rule, not the marker as the ROC tables read it.
            reversed <- private$.riskReversed()
            for (p in names(reversed)) {
                private$.addNotice(
                    type = "WARNING",
                    title = sprintf(.("Risk Model Runs Against the ROC Direction: %s"), p),
                    content = if (reversed[[p]] < 0.5) {
                        sprintf(.("In these data the fitted logistic risk of %2$s runs opposite to the direction in which the ROC tables read %1$s (AUC %3$.3f in that direction). A one-marker logistic model does not depend on Direction, so calibration, the Brier score, the Hosmer-Lemeshow test, the decision and clinical impact curves and internal validation use the risk model the data support. Under the direction selected, %1$s performs worse than those outputs show. Check Direction and Positive class."), p, pos, reversed[[p]])
                    } else {
                        sprintf(.("The ROC tables read %1$s in the direction in which it ranks patients well (AUC %3$.3f), but its group means point the other way, so the slope of the fitted logistic risk of %2$s runs opposite to that direction. This happens with a skewed marker or an outlying value. Calibration, the Brier score, the Hosmer-Lemeshow test, the decision and clinical impact curves and internal validation use that fitted model, the risk model the data support, which ranks patients the opposite way to the ROC, cut-point and fixed-cutoff tables; they do not describe %1$s as those tables read it, and can disagree with them. A log or rank transformation of the marker usually removes the conflict."), p, pos, reversed[[p]])
                    })
            }
            if (!identical(o$predictionScale, "probability")) {
                if (any(in_unit)) {
                    private$.addNotice(
                        type = "INFO",
                        title = .("Values between 0 and 1 analysed as scores"),
                        content = sprintf(.("Every value of %1$s lies between 0 and 1. Prediction scale is Score, so calibration and the decision and clinical impact curves use a logistic model of the marker fitted to these data, not the values themselves. If the values are predicted probabilities of %2$s, set Prediction scale to Probability."), paste(preds[in_unit], collapse = ", "), pos))
                }
                return()
            }
            if (any(!in_unit)) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Values outside 0 to 1 are not probabilities"),
                    content = sprintf(.("Prediction scale is Probability, but these predictors have values outside 0 to 1: %s. Calibration, the Brier score, E/O ratio, Hosmer-Lemeshow test and the decision and clinical impact curves are not shown for them. If the values are scores, set Prediction scale to Score: those outputs then use a logistic model of the marker fitted to these data."), paste(preds[!in_unit], collapse = ", ")))
            }
            # A probability is never reversed, so a Direction that reads it backwards is a warning.
            lower <- preds[in_unit & vapply(preds, function(p) identical(private$.rocObjects[[p]]$direction, ">"), logical(1))]
            if (length(lower)) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Probabilities read with lower values as positive"),
                    content = sprintf(.("Prediction scale is Probability, so the values of %1$s are used as supplied, as predicted probabilities of %2$s, and are never reversed. Direction, however, reads lower values as indicating %2$s, whereas a probability of %2$s is higher in the patients more likely to have it. Check Positive class first (is %2$s the class these probabilities refer to?), then Direction."), paste(lower, collapse = ", "), pos))
            }
        },

        # Single source of truth for "which prevalence do the clinical outputs use?".
        # useObservedPrevalence = TRUE -> the prevalence in this dataset; otherwise the value
        # typed in the Disease Prevalence box. That option carries an .a.yaml default, so it is
        # never NULL - never write `self$options$prevalence %||% observed` and expect the
        # fallback to fire. This was duplicated in three methods and omitted from a fourth.
        .effectivePrevalence = function(observed) {
            if (isTRUE(self$options$useObservedPrevalence)) {
                return(observed)
            }
            user_prev <- self$options$prevalence
            if (is.null(user_prev) || is.na(user_prev)) {
                return(observed)
            }
            user_prev
        },
        .populateClinicalMetrics = function() {
            clinTable <- self$results$results$clinicalApplicationMetrics
            .deleteTableRows(clinTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates
            clinTable$setNote("lr_bands", .("The likelihood-ratio wording follows Jaeschke, Guyatt & Sackett (1994): LR+ above 10 or LR- below 0.1 gives a large change in the probability of the condition, 5-10 or 0.1-0.2 a moderate one, 2-5 or 0.2-0.5 a small but sometimes important one, and 1-2 or 0.5-1 a small, rarely important one. The bands are applied to the ratios at 3 significant figures (jamovi's default number format); how much a change matters depends on the probability before testing."))

            observed_prevalence <- mean(private$.binaryData == levels(private$.binaryData)[2])
            prevalence <- private$.effectivePrevalence(observed_prevalence)
            # F-08: the sample prevalence is a population prevalence only for a consecutive or
            # cross-sectional sample. In a case-control or enriched series it is set by design, and
            # the PPV and NPV at it are not the population's (PPV overstated, NPV understated, when
            # cases are over-sampled).
            clinTable$setNote("prevalence", if (isTRUE(self$options$useObservedPrevalence)) {
                sprintf(.("PPV and NPV use the prevalence observed in these data (%s). That is the prevalence of the target population only if the sample is consecutive or cross-sectional; in a case-control or enriched sample it is set by the design, and these PPV and NPV hold only for a population with that prevalence. Untick Observed prevalence and enter the target population's value under Disease prevalence to obtain its predictive values."),
                        sprintf("%.3f", observed_prevalence))
            })
            if (!is.na(prevalence) && abs(prevalence - observed_prevalence) > 0.05 && !isTRUE(self$options$useObservedPrevalence)) {
                private$.addNotice(type = "INFO", title = .("User-Specified Prevalence"),
                    content = sprintf(.("PPV, NPV and the other prevalence-dependent metrics use the prevalence you entered (%1$s), not the %2$s observed in the analysed rows."),
                        sprintf("%.3g", prevalence), sprintf("%.3g", observed_prevalence)))
            }

            for (predictor in names(private$.rocResults)) {
                result <- private$.rocResults[[predictor]]
                if (isTRUE(result$constant)) next   # no cut-point exists; its ERROR says so
                optimal <- result$optimal_cutoff

                # Calculate predictive values
                sens <- optimal$sensitivity
                spec <- optimal$specificity

                ppv_denom <- (sens * prevalence) + ((1 - spec) * (1 - prevalence))
                ppv <- if (ppv_denom > 1e-10) (sens * prevalence) / ppv_denom else NA_real_
                npv_denom <- ((1 - sens) * prevalence) + (spec * (1 - prevalence))
                npv <- if (npv_denom > 1e-10) (spec * (1 - prevalence)) / npv_denom else NA_real_

                # Calculate likelihood ratios (with division-by-zero guards). A cutoff that calls
                # nobody positive (TP = FP = 0, e.g. -Inf read 'lower') has LR+ = 0/0: not
                # estimable, not infinite; the DOR below then follows as NA.
                lr_pos <- if (abs(1 - spec) < 1e-10 && abs(sens) < 1e-10) {
                    NA_real_
                } else if (abs(1 - spec) < 1e-10) {
                    Inf
                } else {
                    sens / (1 - spec)
                }
                lr_neg <- if (abs(spec) < 1e-10 && abs(1 - sens) < 1e-10) {
                    NA_real_
                } else if (abs(spec) < 1e-10) {
                    Inf
                } else {
                    (1 - sens) / spec
                }

                # Diagnostic odds ratio
                dor <- if (is.na(lr_pos) || is.na(lr_neg)) {
                    NA_real_
                } else if (is.infinite(lr_pos) && is.infinite(lr_neg)) {
                    NA_real_ # Inf/Inf is indeterminate
                } else if (is.infinite(lr_pos) && lr_neg > 0) {
                    Inf # Perfect rule-in (spec=1)
                } else if (!is.infinite(lr_neg) && abs(lr_neg) < 1e-10 && lr_pos > 0) {
                    Inf # Perfect discrimination (sens=1)
                } else if (is.infinite(lr_neg)) {
                    0 # LR+/Inf = 0
                } else {
                    lr_pos / lr_neg
                }

                # Clinical interpretation
                clinical_interp <- private$.interpretClinicalMetrics(
                    ppv, npv, lr_pos, lr_neg,
                    self$options$clinicalContext
                )

                # Handle Inf LR values for numeric table columns
                # Cap at large value and annotate clinical interpretation
                lr_pos_display <- lr_pos
                lr_neg_display <- lr_neg
                lr_notes <- ""
                # An infinite likelihood ratio must not be printed as a NUMBER. 9999 reads as a
                # measured value in a numeric column - a clinician has no way to tell it apart
                # from a real estimate - and it is not even a bound, since the true value is
                # unbounded. Blank the cell and say why in the interpretation text instead.
                if (is.infinite(lr_pos) && lr_pos > 0) {
                    lr_pos_display <- NA_real_
                    lr_notes <- paste(lr_notes, .("LR+ is infinite (no false positives at this cutoff), so it cannot be quoted as a number."))
                }
                if (is.infinite(lr_neg)) {
                    lr_neg_display <- NA_real_
                    lr_notes <- paste(lr_notes, .("LR- is infinite (no true negatives at this cutoff), so it cannot be quoted as a number."))
                }
                dor_display <- dor
                if (!is.na(dor) && is.infinite(dor)) {
                    dor_display <- NA_real_
                    lr_notes <- paste(lr_notes, .("The diagnostic odds ratio is infinite (a zero cell in the 2x2 table), so it cannot be quoted as a number."))
                }
                if (nchar(lr_notes) > 0) {
                    clinical_interp <- trimws(paste0(clinical_interp, lr_notes))
                }

                row <- list(
                    predictor = predictor,
                    prevalence = prevalence,
                    ppv = ppv,
                    npv = npv,
                    lr_positive = lr_pos_display,
                    lr_negative = lr_neg_display,
                    diagnostic_odds_ratio = dor_display,
                    clinical_interpretation = clinical_interp
                )

                key <- private$.escapeVar(predictor)
                clinTable$addRow(rowKey = key, values = row)
                # An NA cell prints "NaN"; blank it and say why (A1). ppv/npv are NA only when
                # nobody / everybody tests positive; a displayed NA ratio is 0/0 or infinite.
                why <- list(
                    ppv = .("Not estimable: no patient tests positive at this cutoff."),
                    npv = .("Not estimable: no patient tests negative at this cutoff."),
                    lr_positive = if (is.na(lr_pos)) .("Not estimable (0/0): no patient tests positive at this cutoff.") else .("Infinite: no false positives at this cutoff."),
                    lr_negative = if (is.na(lr_neg)) .("Not estimable (0/0): every patient tests positive at this cutoff.") else .("Infinite: no true negatives at this cutoff."),
                    diagnostic_odds_ratio = if (is.na(dor)) .("Not estimable (0/0 in the 2x2 table).") else .("Infinite: a zero cell in the 2x2 table."))
                for (col in names(why)) {
                    if (is.na(row[[col]])) private$.blankCells(clinTable, key, col, why[[col]])
                }
            }
        },
        .populateROCComparisons = function() {
            if (length(private$.rocResults) < 2) return()   # said once in .run() for all three tables

            compTable <- self$results$results$rocComparisons
            .deleteTableRows(compTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates
            # bootstrap and Venkatraman (permutation) tests resample; DeLong does not
            compTable$setNote("seed", if (self$options$comparisonMethod != "delong") .fmt(.("Random seed: {seed}"), seed = self$options$seed))
            compTable$setNote(
                "no_equivalence",
                .("\"Not significantly different\" means no difference was detected in this sample; it does not establish that the two predictors perform equally, because the test may lack power to detect a difference of the size that would matter here.")
            )
            compTable$setNote("venkatraman", private$.venkatramanNote())
            predictors <- names(private$.rocResults)

            # k predictors give k(k-1)/2 pairwise tests here, each row printing a categorical
            # verdict, and nothing on the page said how many verdicts were generated. The verdict
            # word carries more weight with a clinical reader than the p-value beside it.
            n_tests <- length(predictors) * (length(predictors) - 1) / 2
            if (n_tests > 1) {
                compTable$setNote("multiplicity", .fmt(
                    .("Every pair of predictors is tested separately, so with {k} predictors this table reports {m} tests. At the 5% level roughly one verdict in twenty is expected to read \u201csignificant\u201d by chance alone, so with several pairs on screen treat an isolated significant row as a hypothesis to confirm rather than as a finding. No multiplicity adjustment is applied to these p-values."),
                    k = length(predictors), m = n_tests
                ))
            }

            for (i in 1:(length(predictors) - 1)) {
                for (j in (i + 1):length(predictors)) {
                    pred1 <- predictors[i]
                    pred2 <- predictors[j]

                    tryCatch(
                        {
                            roc1 <- private$.rocResults[[pred1]]$roc
                            roc2 <- private$.rocResults[[pred2]]$roc

                            comparison <- private$.pairTest(pred1, pred2)

                            auc_diff <- as.numeric(roc1$auc) - as.numeric(roc2$auc)

                            untested <- is.na(comparison$p.value)
                            result <- if (untested) .("Not tested") else if (comparison$p.value < 0.05) .("Significantly different") else .("Not significantly different")

                            # No clinical-significance column: its 0.05 / 0.10 AUC-difference bands
                            # had no source (removed from the .r.yaml).
                            row <- list(
                                predictor1 = pred1,
                                predictor2 = pred2,
                                auc_difference = auc_diff,
                                test_statistic = comparison$statistic,
                                p_value = comparison$p.value,
                                result = result
                            )

                            key <- paste(private$.escapeVar(pred1), private$.escapeVar(pred2), sep = "_vs_")
                            compTable$addRow(rowKey = key, values = row)
                            if (untested) private$.blankCells(compTable, key, c("test_statistic", "p_value"), comparison$why)
                        },
                        error = function(e) {
                            private$.addNotice(
                                type = "WARNING", title = sprintf(.("Comparison Error: %1$s vs %2$s"), pred1, pred2),
                                content = sprintf(.("ROC comparison failed: %s"), e$message)
                            )
                        }
                    )
                }
            }
        },
        .populateDetailedComparison = function() {
            if (length(private$.rocResults) < 2) {
                return()
            }

            detailTable <- self$results$results$detailedComparison
            .deleteTableRows(detailTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates

            # Get all predictor pairs
            predictors <- names(private$.rocResults)

            # k predictors give k(k-1)/2 pairwise tests here, each row printing a categorical
            # verdict, and nothing on the page said how many verdicts were generated. The verdict
            # word carries more weight with a clinical reader than the p-value beside it.
            n_tests <- 4 * length(predictors) * (length(predictors) - 1) / 2
            if (n_tests > 1) {
                detailTable$setNote("multiplicity", .fmt(
                    .("Each metric is tested separately for every pair of predictors, so with {k} predictors this table reports up to {m} tests. At the 5% level roughly one verdict in twenty is expected to read \u201csignificant\u201d by chance alone, so with several pairs on screen treat an isolated significant row as a hypothesis to confirm rather than as a finding. No multiplicity adjustment is applied to these p-values."),
                    k = length(predictors), m = n_tests
                ))
            }
            detailTable$setNote("sign", .("Difference (1 - 2) = value 1 - value 2, as in ROC Curve Comparisons; Relative difference (%) = 100 \u00d7 (value 1 - value 2) / value 2. Value 1 belongs to the first predictor named in Metric."))
            # method-choice concern 2: both cut-points were chosen on these same patients
            detailTable$setNote("cutpoints", .("Sensitivity, specificity and accuracy compare each marker at its own cut-point, chosen on these same patients. McNemar's test (asymptotic, no continuity correction) treats both cut-points as fixed in advance, so its p-values ignore that selection. Rows with fewer than 10 discordant patients are not tested."))
            venkatraman <- identical(self$options$comparisonMethod, "venkatraman")
            detailTable$setNote("venkatraman", private$.venkatramanNote())
            # the AUC row's p comes from the same resampling test as ROC Curve Comparisons
            detailTable$setNote("seed", if (self$options$comparisonMethod != "delong") .fmt(.("Random seed: {seed}"), seed = self$options$seed))

            for (i in 1:(length(predictors) - 1)) {
                for (j in (i + 1):length(predictors)) {
                    pred1 <- predictors[i]
                    pred2 <- predictors[j]

                    tryCatch(
                        {
                            result1 <- private$.rocResults[[pred1]]
                            result2 <- private$.rocResults[[pred2]]

                            # Compare various metrics
                            metrics <- c("AUC", "Sensitivity", "Specificity", "Accuracy")

                            for (metric in metrics) {
                                # a constant marker has an AUC but no cut-point to compare
                                if (metric != "AUC" && (isTRUE(result1$constant) || isTRUE(result2$constant))) next
                                if (metric == "AUC") {
                                    val1 <- as.numeric(result1$roc$auc)
                                    val2 <- as.numeric(result2$roc$auc)
                                } else if (metric == "Sensitivity") {
                                    val1 <- result1$optimal_cutoff$sensitivity
                                    val2 <- result2$optimal_cutoff$sensitivity
                                } else if (metric == "Specificity") {
                                    val1 <- result1$optimal_cutoff$specificity
                                    val2 <- result2$optimal_cutoff$specificity
                                } else if (metric == "Accuracy") {
                                    val1 <- result1$optimal_cutoff$accuracy
                                    val2 <- result2$optimal_cutoff$accuracy
                                }

                                if (!is.na(val1) && !is.na(val2)) {
                                    # value 1 - value 2, the sign ROC Curve Comparisons uses (VAL-18)
                                    diff <- val1 - val2
                                    percent_change <- if (abs(val2) < 1e-10) NA_real_ else 100 * diff / val2

                                    # Calculate p-value using appropriate statistical test
                                    p_value <- private$.calculateMetricPValue(metric, result1, result2, pred1, pred2)

                                    # The test decides, never the size of a relative change (VAL-23). Under
                                    # Venkatraman the AUC row tests the whole curves, so it ranks no AUC.
                                    interpretation <- if (is.na(p_value)) {
                                        .("Not tested")
                                    } else if (metric == "AUC" && venkatraman) {
                                        if (p_value < 0.05) .("ROC curves differ (p < 0.05)") else .("No difference between the curves detected")
                                    } else if (p_value < 0.05) {
                                        sprintf(.("%s higher (p < 0.05)"), .rocLabel(if (diff >= 0) pred1 else pred2, note = FALSE))
                                    } else {
                                        .("No difference detected (not evidence of equal performance)")
                                    }

                                    row <- list(
                                        metric = .fmt(.("{metric} ({a} vs {b})"), metric = private$.metricLabel(metric), a = pred1, b = pred2),
                                        model1_value = val1,
                                        model2_value = val2,
                                        difference = diff,
                                        percent_change = percent_change,
                                        p_value = p_value,
                                        # `effect_size` was abs(diff) - the same number as the
                                        # Difference column, read by clinicians against the
                                        # small/medium/large bands of a STANDARDISED effect
                                        # size. No standardised effect size is computed here,
                                        # so the column is gone rather than mislabelled.
                                        interpretation = interpretation
                                    )

                                    key <- paste(private$.escapeVar(pred1), private$.escapeVar(pred2), metric, sep = "_")
                                    detailTable$addRow(rowKey = key, values = row)
                                    if (is.na(percent_change)) {
                                        private$.blankCells(detailTable, key, "percent_change", .("Undefined because the Model 2 value is 0."))
                                    }
                                    if (is.na(p_value)) {
                                        private$.blankCells(detailTable, key, "p_value", .("Not tested; the notice above says why."))
                                    }
                                }
                            }
                        },
                        error = function(e) {
                            private$.addNotice(
                                type = "WARNING", title = sprintf(.("Detailed Comparison Error: %1$s vs %2$s"), pred1, pred2),
                                content = sprintf(.("Detailed comparison failed: %s"), e$message)
                            )
                        }
                    )
                }
            }

            if (length(private$.metricPValueFailures) > 0) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Some Comparison P-Values Could Not Be Computed"),
                    content = sprintf(.("The p-value is blank for: %s. The differences are still shown."), paste(unique(private$.metricPValueFailures), collapse = "; "))
                )
            }
        },
        .populateStatisticalSummary = function() {
            if (length(private$.rocResults) < 2) {
                return()
            }

            statSummaryTable <- self$results$results$statisticalSummary
            .deleteTableRows(statSummaryTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates
            statSummaryTable$setNote("seed", if (self$options$comparisonMethod != "delong") .fmt(.("Random seed: {seed}"), seed = self$options$seed))
            statSummaryTable$setNote(
                "no_equivalence",
                .("\"No significant difference (p >= 0.05)\" means no difference was detected in this sample; it does not establish that the two predictors perform equally, because the test may lack the power to detect a difference of the size that would matter here. The p-value is the probability of a result at least this extreme if there were no difference, not the probability that there is none. The Observed AUC difference column reports only how far apart the two observed AUCs are and which is higher, so a wide observed gap and a large p-value can appear on the same row when the sample is small.")
            )
            statSummaryTable$setNote("venkatraman", private$.venkatramanNote())

            # Get all predictor pairs
            predictors <- names(private$.rocResults)

            # k predictors give k(k-1)/2 pairwise tests here, each row printing a categorical
            # verdict, and nothing on the page said how many verdicts were generated. The verdict
            # word carries more weight with a clinical reader than the p-value beside it.
            n_tests <- length(predictors) * (length(predictors) - 1) / 2
            if (n_tests > 1) {
                statSummaryTable$setNote("multiplicity", .fmt(
                    .("Every pair of predictors is tested separately, so with {k} predictors this table reports {m} tests. At the 5% level roughly one verdict in twenty is expected to read \u201csignificant\u201d by chance alone, so with several pairs on screen treat an isolated significant row as a hypothesis to confirm rather than as a finding. No multiplicity adjustment is applied to these p-values."),
                    k = length(predictors), m = n_tests
                ))
            }

            for (i in 1:(length(predictors) - 1)) {
                for (j in (i + 1):length(predictors)) {
                    pred1 <- predictors[i]
                    pred2 <- predictors[j]

                    tryCatch(
                        {
                            roc1 <- private$.rocResults[[pred1]]$roc
                            roc2 <- private$.rocResults[[pred2]]$roc

                            # The same test, and so the same p-value, as ROC Curve Comparisons.
                            test_result <- private$.pairTest(pred1, pred2)
                            test_name <- switch(self$options$comparisonMethod,
                                delong = .("DeLong Test for AUC Comparison"),
                                bootstrap = .("Bootstrap Test for AUC Comparison"),
                                .("Venkatraman permutation test of ROC curve equality"))
                            test_stat <- test_result$statistic
                            p_val <- test_result$p.value

                            # Determine statistical conclusion
                            conclusion <- if (is.na(p_val)) {
                                .("Not tested")
                            } else if (p_val < 0.001) {
                                .("Highly significant difference (p < 0.001)")
                            } else if (p_val < 0.01) {
                                .("Significant difference (p < 0.01)")
                            } else if (p_val < 0.05) {
                                .("Significant difference (p < 0.05)")
                            } else {
                                .("No significant difference (p >= 0.05)")
                            }

                            # The observed gap and which predictor is ahead, with no size verdict:
                            # the Large/Medium/Small bands had no source (VAL-23).
                            auc_diff <- as.numeric(roc1$auc) - as.numeric(roc2$auc)
                            effect_magnitude <- if (abs(auc_diff) < 1e-12) {
                                .("Equal")
                            } else {
                                sprintf(.("%1$s (%2$s higher)"),
                                        if (abs(auc_diff) < 0.0005) "< 0.001" else sprintf("%.3f", abs(auc_diff)),
                                        .rocLabel(if (auc_diff > 0) pred1 else pred2, note = FALSE))
                            }

                            row <- list(
                                test_name = .fmt(.("{test} ({a} vs {b})"), test = test_name, a = pred1, b = pred2),
                                test_statistic = test_stat,
                                p_value = p_val,
                                conclusion = conclusion,
                                effect_magnitude = effect_magnitude
                            )

                            key <- paste(private$.escapeVar(pred1), private$.escapeVar(pred2), "stat", sep = "_")
                            statSummaryTable$addRow(rowKey = key, values = row)
                            if (is.na(p_val)) private$.blankCells(statSummaryTable, key, c("test_statistic", "p_value"), test_result$why)
                        },
                        error = function(e) {
                            private$.addNotice(
                                type = "WARNING", title = sprintf(.("Statistical Summary Error: %1$s vs %2$s"), pred1, pred2),
                                content = sprintf(.("Statistical comparison failed: %s"), e$message)
                            )
                        }
                    )
                }
            }
        },
        # The Partial AUC range as c(min, max), or NULL when it does not parse to 0 <= min < max <= 1.
        .partialRangeValues = function() {
            x <- suppressWarnings(as.numeric(trimws(strsplit(self$options$partialRange %||% "", ",", fixed = TRUE)[[1]])))
            if (length(x) != 2L || anyNA(x) || x[1] >= x[2] || x[1] < 0 || x[2] > 1) return(NULL)
            x
        },
        .populatePartialAUC = function() {
            paTable <- self$results$results$partialAucAnalysis
            .deleteTableRows(paTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates
            range_values <- private$.partialRangeValues()
            if (is.null(range_values)) return()   # .prepareData() raised the WARNING
            use_pa_boot <- isTRUE(self$options$bootstrapPartialAUC)
            # The interval is seed-dependent, so the seed that produced it has to be on the table.
            paTable$setNote("seed", if (use_pa_boot) .fmt(.("Random seed: {seed}"), seed = self$options$seed))
            paTable$setNote(
                "pauc_ci",
                # "requested", not asserted: .bootstrapAucCi() silently downgrades BCa to
                # percentile when resamples had to be dropped (:907-909), and this note is
                # written BEFORE the bootstrap runs, so it cannot know which one produced the
                # interval. Naming the requested type and the downgrade condition is true in
                # both cases; the notice raised at that point says when it actually happened.
                if (use_pa_boot) sprintf(
                    .("The partial AUC confidence interval is a bootstrap interval of the requested type '%1$s' from %2$s resamples. If any resample contained a single outcome class it is dropped, and a BCa interval is then reported as a percentile interval instead \u2014 a notice appears when that happens. The interval is for the raw partial AUC over the chosen focus range, NOT for the McClish-normalised value in the next column."),
                    self$options$bootstrapMethod, self$options$bootstrapSamples))

            range_min <- range_values[1]
            range_max <- range_values[2]

            # Determine if using sensitivity or specificity range
            range_type_name <- self$options$partialAucType

            for (predictor in names(private$.rocResults)) {
                tryCatch(
                    {
                        roc_obj <- private$.rocObjects[[predictor]]

                        # Calculate partial AUC based on type
                        # pROC requires partial.auc in DESCENDING order: c(max, min)
                        if (range_type_name == "sensitivity") {
                            pauc <- pROC::auc(roc_obj,
                                partial.auc = c(range_max, range_min),
                                partial.auc.focus = "sensitivity"
                            )
                            range_display <- .("Sensitivity")
                        } else {
                            pauc <- pROC::auc(roc_obj,
                                partial.auc = c(range_max, range_min),
                                partial.auc.focus = "specificity"
                            )
                            range_display <- .("Specificity")
                        }

                        # McClish (1989) normalized partial AUC via pROC's built-in correction
                        # This maps pAUC to [0.5, 1.0] scale where 0.5 = chance, 1.0 = perfect
                        normalized_pauc <- tryCatch(
                            {
                                pauc_corrected <- pROC::auc(roc_obj,
                                    partial.auc = c(range_max, range_min),
                                    partial.auc.focus = range_type_name,
                                    partial.auc.correct = TRUE
                                )
                                as.numeric(pauc_corrected)
                            },
                            error = function(e) NA_real_
                        )

                        # Bootstrap CI for the raw partial AUC. pROC::ci.auc(method = "bootstrap")
                        # cannot do this: it always returns percentile limits and silently discards
                        # the user's bootstrapMethod choice (see .bootstrapAucCi), so the shared
                        # helper is reused with a partial-AUC statistic instead of the full AUC.
                        pa_ci <- if (use_pa_boot) {
                            tryCatch(
                                private$.bootstrapAucCi(
                                    roc_obj,
                                    auc_fun = function(r) {
                                        as.numeric(pROC::auc(r,
                                            partial.auc = c(range_max, range_min),
                                            partial.auc.focus = range_type_name
                                        ))
                                    },
                                    # A partial AUC is bounded by the width of the focus range,
                                    # not by 1.
                                    clamp = c(0, range_max - range_min)
                                ),
                                error = function(e) NULL
                            )
                        } else {
                            NULL
                        }

                        clinical_relevance <- private$.assessPartialAUCRelevance(
                            pauc, range_min, range_max, self$options$clinicalContext, range_type_name
                        )

                        row <- list(
                            predictor = predictor,
                            range_type = range_display,
                            range_min = range_min,
                            range_max = range_max,
                            partial_auc = as.numeric(pauc),
                            # Blanked after addRow() when no interval was formed: jamovi prints
                            # NULL as "." and NA as "NaN"; only "" is blank (A1).
                            partial_auc_lower = if (is.null(pa_ci)) NA_real_ else as.numeric(pa_ci[1]),
                            partial_auc_upper = if (is.null(pa_ci)) NA_real_ else as.numeric(pa_ci[3]),
                            normalized_pauc = as.numeric(normalized_pauc),
                            clinical_relevance = clinical_relevance
                        )

                        key <- private$.escapeVar(predictor)
                        paTable$addRow(rowKey = key, values = row)
                        # F-28: pROC returns NA (with a warning) for a curve below the diagonal over
                        # the focus range; jamovi prints that NA as "NaN".
                        if (!is.finite(row$normalized_pauc)) {
                            private$.blankCells(paTable, key, "normalized_pauc",
                                .("Not defined: the McClish correction rescales a partial AUC to 0.5 (chance) to 1 (perfect) only when the curve lies above the chance diagonal over the focus range, and this curve lies below it. The raw partial AUC is shown."))
                        }
                        # Only when the interval was asked for: otherwise pa_ci is NULL by design and
                        # the columns are hidden.
                        if (use_pa_boot && (is.null(pa_ci) || !all(is.finite(as.numeric(pa_ci)[c(1, 3)])))) {
                            private$.blankCells(paTable, key, c("partial_auc_lower", "partial_auc_upper"),
                                if (min(length(roc_obj$cases), length(roc_obj$controls)) < 2L) private$.singlePatientBootNote()
                                else .("No bootstrap interval could be formed for this partial AUC: every resample gave the same value, or the requested interval type could not be computed from these resamples. The limits are left blank."))
                        }
                    },
                    error = function(e) {
                        private$.addNotice(type = "WARNING", title = sprintf(.("Partial AUC Error: %s"), predictor), content = sprintf(.("Partial AUC calculation failed for %1$s: %2$s. Check that the partial range is valid and data has sufficient variation."), predictor, e$message))
                    }
                )
            }
        },
        .populateCROCAnalysis = function() {
            crocTable <- self$results$results$crocAnalysisTable
            .deleteTableRows(crocTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates
            alpha <- self$options$crocAlpha
            crocTable$setNote("croc", .("The CROC AUC weights the ROC curve toward its low false-positive end with an exponential magnifier of strength alpha (Swamidass et al. 2010). It is never larger than the ROC AUC, so read it against the random-ranking value, 1/alpha - 1/(exp(alpha) - 1), not against the ROC AUC, and report alpha with the area. Gain over random = CROC AUC - random-ranking CROC AUC. Tied scores are integrated along the straight ROC segment that joins their operating points."))

            for (predictor in names(private$.rocResults)) {
                tryCatch(
                    {
                        roc_obj <- private$.rocObjects[[predictor]]
                        croc_result <- private$.calculateCROC(roc_obj, alpha)
                        # Gain over a RANDOM ranking: CROC <= ROC AUC for every classifier (f(x) >= x),
                        # so the old CROC - ROC "gain" was never positive (VAL-21).
                        gain <- croc_result$croc_auc - croc_result$random

                        row <- list(
                            predictor = predictor,
                            croc_auc = croc_result$croc_auc,
                            random_croc = croc_result$random,
                            roc_auc = as.numeric(roc_obj$auc),
                            alpha = alpha,
                            early_retrieval_gain = gain,
                            interpretation = private$.interpretCROC(gain, croc_result$random, alpha)
                        )

                        crocTable$addRow(rowKey = private$.escapeVar(predictor), values = row)
                    },
                    error = function(e) {
                        private$.addNotice(type = "WARNING", title = sprintf(.("CROC Error: %s"), predictor), content = sprintf(.("CROC calculation failed for %1$s: %2$s"), predictor, e$message))
                    }
                )
            }
        },
        .populateConvexHull = function() {
            hullTable <- self$results$results$convexHullTable
            .deleteTableRows(hullTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates

            for (predictor in names(private$.rocResults)) {
                tryCatch(
                    {
                        roc_obj <- private$.rocObjects[[predictor]]

                        # Calculate convex hull
                        hull_result <- private$.calculateConvexHull(roc_obj)

                        # Get empirical AUC
                        empirical_auc <- as.numeric(roc_obj$auc)

                        # Calculate performance gap
                        performance_gap <- hull_result$hull_auc - empirical_auc

                        # Interpretation
                        interpretation <- private$.interpretConvexHull(performance_gap)

                        row <- list(
                            predictor = predictor,
                            hull_auc = hull_result$hull_auc,
                            empirical_auc = empirical_auc,
                            performance_gap = performance_gap,
                            n_hull_points = hull_result$n_hull_points,
                            interpretation = interpretation
                        )

                        hullTable$addRow(rowKey = private$.escapeVar(predictor), values = row)
                    },
                    error = function(e) {
                        private$.addNotice(type = "WARNING", title = sprintf(.("Convex Hull Error: %s"), predictor), content = sprintf(.("Convex hull calculation failed for %1$s: %2$s"), predictor, e$message))
                    }
                )
            }
        },
        # CROC AUC (Swamidass et al. 2010): the integral of TPR d f(FPR) along the empirical ROC
        # path, f(x) = (1 - exp(-a x)) / (1 - exp(-a)), exact on each straight segment (integration
        # by parts); vertical runs contribute nothing. pROC lists TPR in descending order inside a
        # vertical run, so the path is ordered by (FPR, TPR): a trapezoid over pROC's order gave a
        # perfect classifier 0.81 (VAL-01). Also returns the random-ranking CROC AUC (the diagonal)
        # and the plotting path in transformed coordinates.
        .calculateCROC = function(roc_obj, alpha = 7.0) {
            x <- 1 - roc_obj$specificities
            y <- roc_obj$sensitivities
            o <- order(x, y); x <- x[o]; y <- y[o]
            f <- function(v) (1 - exp(-alpha * v)) / (1 - exp(-alpha))
            i <- which(diff(x) > 0)
            x0 <- x[i]; x1 <- x[i + 1]; y0 <- y[i]; y1 <- y[i + 1]
            f_int <- ((x1 - x0) + (exp(-alpha * x1) - exp(-alpha * x0)) / alpha) / (1 - exp(-alpha))
            croc_auc <- sum(y1 * f(x1) - y0 * f(x0) - (y1 - y0) / (x1 - x0) * f_int)
            # Diagonal (tied-score) segments are densified BEFORE the transform, so the drawn
            # line is the transformed path rather than a chord between transformed vertices.
            d <- which(diff(x) > 0 & diff(y) != 0)
            frac <- seq_len(24) / 25
            px <- c(x, unlist(lapply(d, function(k) x[k] + frac * (x[k + 1] - x[k]))))
            py <- c(y, unlist(lapply(d, function(k) y[k] + frac * (y[k + 1] - y[k]))))
            o <- order(px, py)
            list(croc_auc = croc_auc, random = 1 / alpha - 1 / (exp(alpha) - 1),
                 croc_fpr = f(px[o]), croc_tpr = py[o], alpha = alpha)
        },
        # Upper convex hull of the empirical ROC vertices plus (0,0) and (1,1), by Andrew's
        # monotone chain with collinear points dropped, so n_hull_points is the vertex count.
        # chull() + aggregating by FPR lost (0,0) whenever the hull began with a vertical edge
        # (VAL-17).
        .calculateConvexHull = function(roc_obj) {
            x <- c(0, 1 - roc_obj$specificities, 1)
            y <- c(0, roc_obj$sensitivities, 1)
            o <- order(x, y); x <- x[o]; y <- y[o]
            h <- integer(0)
            for (i in seq_along(x)) {
                # pop the last vertex unless (previous, last, i) turns clockwise
                while (length(h) >= 2) {
                    a <- h[length(h) - 1]; b <- h[length(h)]
                    if ((x[b] - x[a]) * (y[i] - y[a]) - (y[b] - y[a]) * (x[i] - x[a]) < -1e-12) break
                    h <- h[-length(h)]
                }
                h <- c(h, i)
            }
            hx <- x[h]; hy <- y[h]
            list(hull_auc = sum(diff(hx) * (head(hy, -1) + tail(hy, -1)) / 2),
                 hull_fpr = hx, hull_tpr = hy, n_hull_points = length(h))
        },
        # Descriptive, no verdict bands: how far the CROC AUC sits from a random ranking toward a
        # perfect one, at the displayed (whole-percent) precision.
        .interpretCROC = function(gain, random, alpha) {
            pct <- round(100 * gain / (1 - random))
            if (pct < 0) {
                .fmt(.("Below a random ranking (alpha {a})"), a = alpha)
            } else {
                .fmt(.("{pct}% of the way from a random ranking to a perfect one (alpha {a})"), pct = pct, a = alpha)
            }
        },
        # A hull gap is not an improvement that threshold tuning or recalibration can deliver: a
        # point on a hull edge is reached only by randomly mixing the two cut-offs at its ends
        # (Provost & Fawcett 2001), and the hull is drawn through these data's own points (VAL-25g).
        # Banded at the displayed 3 dp.
        .interpretConvexHull = function(performance_gap) {
            gap <- round(performance_gap, 3)
            if (gap < 0.005) {
                .("Empirical curve is (almost) convex; the hull adds nothing")
            } else {
                .fmt(.("The hull is {gap} above the empirical AUC; points between empirical operating points are reachable only by randomly mixing two cut-offs (Provost & Fawcett 2001); in-sample and optimistic"), gap = sprintf("%.3f", gap))
            }
        },
        .checkClassImbalance = function(analysisData) {
            # Get outcome variable
            outcome <- private$.binaryData
            if (is.null(outcome)) {
                outcome <- analysisData[[private$.outcome]]
            }

            # Convert to factor if needed
            if (!is.factor(outcome)) {
                outcome <- as.factor(outcome)
            }

            # Count classes
            class_counts <- table(outcome)
            if (length(class_counts) != 2) {
                return() # Only check binary classification
            }

            # .prepareData() always sets the positive class before any binary output runs
            positive_class_label <- private$.positiveClass

            n_positive <- as.integer(class_counts[positive_class_label])
            n_negative <- sum(class_counts) - n_positive

            # If counts are missing or zero, exit gracefully
            if (length(n_positive) == 0 || length(n_negative) == 0 ||
                is.na(n_positive) || is.na(n_negative) ||
                n_positive == 0 || n_negative == 0) {
                return()
            }

            # Calculate ratio (always express as larger:smaller). Every band below uses the ratio
            # as displayed (2 dp), so the text and the bands cannot disagree (VAL-19).
            ratio_shown <- round(max(n_positive, n_negative) / min(n_positive, n_negative), 2)
            ratio_text <- if (n_positive > n_negative) sprintf("%.2f:1", ratio_shown) else sprintf("1:%.2f", ratio_shown)

            # Calculate prevalence and PRC baseline
            prevalence <- n_positive / (n_positive + n_negative)
            prc_baseline <- prevalence

            # Determine if imbalanced
            threshold <- self$options$imbalanceThreshold
            if (is.null(threshold) || length(threshold) == 0 || is.na(threshold)) {
                threshold <- 3
            }
            # Severity, recommendation and notice all key on this one comparison. The bands were
            # fixed at 5 and 10 while the decision used the threshold, so a 5.5:1 outcome under
            # threshold 6 read "High imbalance" beside "ROC analysis is appropriate" (VAL-19).
            is_imbalanced <- ratio_shown >= threshold
            severity <- if (!is_imbalanced) {
                sprintf(.("Below your imbalance threshold (%s:1)"), threshold)
            } else if (ratio_shown >= max(10, threshold)) {
                .("Severe imbalance")
            } else if (ratio_shown >= max(5, threshold)) {
                .("High imbalance")
            } else {
                .("Moderate imbalance")
            }

            # Generate recommendation. F-17: the PR curve complements the ROC curve (Davis & Goadrich
            # 2006 call it more informative for skewed data); it is not a more reliable replacement.
            if (is_imbalanced && self$options$recommendPRC) {
                recommendation <- .("Add the Precision-Recall curve (PRC) alongside the ROC curve")
            } else if (is_imbalanced) {
                recommendation <- .("Consider class imbalance in interpretation")
            } else {
                recommendation <- .("ROC analysis is appropriate")
            }

            # Populate metrics table
            metricsTable <- self$results$results$imbalanceMetrics
            metricsTable$setRow(rowNo = 1, values = list(
                n_positive = n_positive,
                n_negative = n_negative,
                ratio = ratio_text,
                prevalence = prevalence,
                prc_baseline = prc_baseline,
                imbalance_severity = severity,
                recommendation = recommendation
            ))

            # Generate Notice if imbalanced (replaces HTML warning). DETECTION is never gated by
            # a display option: `showImbalanceWarning` used to sit in this condition with
            # `default: false`, so a 20:1 outcome produced an AUC, a confidence interval and an
            # optimal cut-point with no caution anywhere on the page. That checkbox now decides
            # only how much the warning EXPLAINS; the warning itself always fires.
            if (is_imbalanced) {
                notice_type <- if (ratio_shown >= max(5, threshold)) "STRONG_WARNING" else "WARNING"

                # Both clauses were untranslated English spliced through %s, so a translated
                # notice carried an English sentence mid-line. Translate the whole clause, and
                # keep the leading separator OUTSIDE .() - a leading space in a msgid does not
                # survive the catalog.
                prc_recommendation <- paste0(" \u2022 ", if (self$options$recommendPRC) {
                    .("Consider adding the Precision-Recall curve (PRC), which shows precision (PPV) directly; it complements the ROC curve rather than replacing it.")
                } else {
                    .("Interpret ROC results cautiously given class imbalance.")
                })

                # The explanatory bullets are what `showImbalanceWarning` now controls. F-17: the
                # old text said specificity is "dominated by the majority class" (it is computed
                # within the negatives only) and that ROC curves "may be optimistic"; replicating
                # every negative five times leaves the AUC unchanged.
                imbalance_detail <- if (isTRUE(self$options$showImbalanceWarning)) {
                    paste(" \u2022", if (n_positive < n_negative) {
                        .("ROC curves, the AUC, sensitivity and specificity do not depend on the class proportions: each rate is computed within one outcome group. With few positives, sensitivity is imprecise, and a high AUC can sit beside a low PPV, because most positive results may still be false positives.")
                    } else {
                        .("ROC curves, the AUC, sensitivity and specificity do not depend on the class proportions: each rate is computed within one outcome group. With few negatives, specificity is imprecise, and a high AUC can sit beside a low NPV.")
                    })
                } else {
                    ""
                }

                private$.addNotice(
                    type = notice_type,
                    title = .("Class Imbalance Detected"),
                    content = paste0(
                        sprintf(.("Class imbalance detected: %1$s ratio (%2$s positive, %3$s negative, prevalence %4$s%%). \u2022 %5$s.%6$s"), ratio_text, n_positive, n_negative, round(prevalence * 100, 1), severity, prc_recommendation),
                        imbalance_detail
                    )
                )
            }
        },
        .populateComprehensiveAnalysis = function() {
            compTable <- self$results$results$comprehensiveAnalysisSummary
            .deleteTableRows(compTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates

            # Summary statistics
            n_predictors <- length(private$.rocResults)
            n_observations <- nrow(private$.analysisData)

            measures <- list(
                list(
                    .("Number of Predictors"), as.character(n_predictors),
                    .("Number of predictor variables analyzed")
                ),
                list(
                    .("Sample Size"), as.character(n_observations),
                    .("Total number of observations in analysis")
                ),
                list(
                    .("Clinical Context"), private$.choiceLabel("clinicalContext", self$options$clinicalContext),
                    .("Clinical application context for interpretation")
                ),
                list(
                    .("Analysis Type"), private$.choiceLabel("analysisType", self$options$analysisType),
                    .("Type of ROC analysis performed")
                )
            )

            # Add best performing predictor
            if (n_predictors > 0) {
                best_auc <- -Inf   # an AUC of 0 is still a result (a marker read backwards)
                best_predictor <- ""
                for (pred in names(private$.rocResults)) {
                    auc_val <- as.numeric(private$.rocObjects[[pred]]$auc)
                    if (auc_val > best_auc) {
                        best_auc <- auc_val
                        best_predictor <- pred
                    }
                }
                measures[[length(measures) + 1]] <- list(
                    .("Best Predictor"), paste(best_predictor, "(AUC =", round(best_auc, 3), ")"),
                    .("Predictor with highest AUC value")
                )
            }

            for (i in seq_along(measures)) {
                measure <- measures[[i]]
                row <- list(
                    measure = measure[[1]],
                    value = measure[[2]],
                    interpretation = measure[[3]],
                    clinical_significance = .("Descriptive statistic")
                )
                compTable$addRow(rowKey = i, values = row)
            }
        },
        # Methods text built from what actually ran, with a reference list that matches it (VAL-10).
        # It used to be one static paragraph: "maximizing the Youden Index" with Youden
        # optimisation off, and Youden 1950 cited for choosing the cut-point (VAL-25a).
        .populateMethodsExplanation = function() {
            o <- self$options
            cuts <- lapply(private$.rocResults, function(r) r$optimal_cutoff)
            para <- function(title, text) paste0("<p><b>", title, ":</b> ", paste(text, collapse = " "), "</p>")
            # Ties: Mann & Whitney assume none; the half credit is Hanley & McNeil's (1982, p. 31).
            cited <- c("robin", "hanley", "mannwhitney")
            out <- c(paste0("<h3>", .("Statistical Methods and References"), "</h3>"),
                     para(.("ROC Curve Analysis"), c(.("Empirical ROC curves were computed with the pROC package (Robin et al., 2011). The AUC is the Mann-Whitney probability that a randomly chosen case lies further than a randomly chosen non-case in the direction that indicates the condition, with ties counted as one half (Hanley & McNeil, 1982); it equals the Mann-Whitney U statistic divided by the number of case/non-case pairs (Mann & Whitney, 1947)."),
                         # F-14 and F-00: the direction rule and the independence assumption
                         private$.directionMethodsText(),
                         .("Each row was analysed as a separate, independent patient; the confidence intervals and tests assume this."))))

            ci <- private$.safeHtmlOutput(private$.aucCiText())
            methods <- vapply(private$.rocResults, function(r) {
                if (identical(attr(r$roc$ci, "method"), "bootstrap")) "davison" else "delong"
            }, character(1))
            cited <- c(cited, unique(methods))
            if (isTRUE(private$.delongWarned)) cited <- c(cited, "obuchowski", "feng")   # cited by the DeLong WARNING
            out <- c(out, para(.("AUC Confidence Interval"), ci))

            if (length(cuts) > 0) {
                if (identical(cuts[[1]]$method, "closest_topleft")) {
                    cp <- .("Cut-points minimise the squared distance to the top-left corner of the ROC plot, (1 - sensitivity)^2 + (1 - specificity)^2, over the thresholds midway between adjacent observed values (Perkins & Schisterman, 2006); Youden optimisation was switched off. A patient is test-positive at or above the cutoff (at or below it when lower values are positive).")
                    if (isTRUE(o$sensitivityThreshold > 0 || o$specificityThreshold > 0)) {
                        cp <- c(cp, .("The minimum sensitivity and specificity settings apply only to Youden optimisation and were not used."))
                    }
                    # F-53b: the cited source's own conclusion goes with the citation.
                    cp <- c(cp, .("Perkins & Schisterman (2006) show that this criterion and the Youden index can choose different cut-points, and that only the Youden index is optimal with respect to the overall misclassification rate."))
                    cited <- c(cited, "perkins")
                } else {
                    cp <- .("Cut-points maximise the Youden index, J = sensitivity + specificity - 1 (Youden, 1950), over the thresholds midway between adjacent observed values: the empirical estimator of the Youden-optimal cut-point (Fluss et al., 2005). A patient is test-positive at or above the cutoff (at or below it when lower values are positive).")
                    # F-53a: what the cited comparison found for this estimator.
                    cp <- c(cp, .("In the simulations of Fluss et al. (2005) this empirical estimator, the most commonly used, had the worst overall bias and root mean square error of the four estimators compared (normal, transformed-normal, empirical and kernel); for the cut-point itself their results were more variable."))
                    applied <- Filter(function(k) isTRUE(k$min_sens > 0 || k$min_spec > 0), cuts)
                    if (length(applied) > 0) {
                        cp <- c(cp, sprintf(.("The search was restricted to cut-points with sensitivity of at least %1$s and specificity of at least %2$s; where none met both, the unconstrained optimum was used and a notice says so."), applied[[1]]$min_sens, applied[[1]]$min_spec))
                    }
                    if (!is.null(private$.presetConfig)) {
                        cp <- c(cp, sprintf(.("Clinical preset in force: %s (it turns Youden optimisation on and may replace the minimum sensitivity and specificity)."), private$.choiceLabel("clinicalPresets", o$clinicalPresets)))
                    }
                    cited <- c(cited, "youden", "fluss")
                }
                cp <- c(cp, .("Where several cut-points tie on this criterion (ties are found exactly from the counts), this module reports the one closest to the top-left corner and then the smallest cutoff; a notice lists the alternatives."),
                        .("The cut-point was chosen on these same data, so the sensitivity, specificity and accuracy reported at it are optimistic."))
                out <- c(out, para(.("Optimal Cutoff Selection"), cp))
            }

            if (isTRUE(o$diagnosticMetrics)) {
                out <- c(out, para(.("Sensitivity and Specificity"), sprintf(.("%s%% intervals for sensitivity and specificity are Clopper-Pearson exact binomial intervals (Clopper & Pearson, 1934); they treat the cut-point as fixed and do not reflect its selection on these data."), o$confidenceLevel)))
                cited <- c(cited, "clopper")
            }

            # Only a range the partial AUC table used: a cleared box printed "from NA to NA", a
            # reversed one described a partial AUC that was never computed.
            if (isTRUE(o$partialAuc) && !is.null(private$.partialRangeValues())) {
                # the user's own notation ("1.0", not "1")
                rng <- private$.safeHtmlOutput(trimws(strsplit(o$partialRange, ",", fixed = TRUE)[[1]]))
                out <- c(out, para(.("Partial AUC"), sprintf(if (identical(o$partialAucType, "sensitivity")) {
                    .("The partial AUC was computed over sensitivities from %1$s to %2$s and standardised with the McClish correction (McClish, 1989) to a 0.5-1 scale.")
                } else {
                    .("The partial AUC was computed over specificities from %1$s to %2$s and standardised with the McClish correction (McClish, 1989) to a 0.5-1 scale.")
                }, rng[1], rng[2])))
                cited <- c(cited, "mcclish")
            }

            if (isTRUE(o$calibrationAnalysis)) {
                cal <- if (identical(o$predictionScale, "probability")) {
                    .("Calibration treats each predictor's values as predicted probabilities of the positive class and uses them as supplied (Prediction scale: Probability).")
                } else {
                    .("Calibration uses risks from a logistic model of each marker fitted to these same data (Prediction scale: Score), so it describes that in-sample model, not an external prediction rule.")
                }
                if (isTRUE(o$brierScore)) {
                    cal <- c(cal, .("The Brier score is the mean squared difference between the predicted risk and the outcome."))
                }
                # risks fitted here have slope 1 and intercept 0 by construction (not reported)
                if (isTRUE(o$calibrationMetrics) && identical(o$predictionScale, "probability")) {
                    # Van Calster's "calibration intercept" IS calibration-in-the-large; the column
                    # holds the free intercept of the slope model (as rms::val.prob does).
                    cal <- c(cal, .("The calibration slope comes from a logistic regression of the outcome on the logit of the predicted risk, and the Intercept (slope model) column is that model's intercept; calibration-in-the-large is the intercept with the slope fixed at 1, which Van Calster et al. (2019) call the calibration intercept."))
                    cited <- c(cited, "vancalster")
                    if (isTRUE(private$.underfitCited)) cited <- c(cited, "austin")   # cited by the 'Above 1' slope reading
                }
                if (isTRUE(o$splineCalibration)) {
                    cal <- c(cal, sprintf(.("A flexible calibration curve gives ICI, E50, E90 and Emax as defined by Austin & Steyerberg (2019); it is fitted here as a natural cubic spline of the logit of the predicted risk with %s knots, a variant of their loess and restricted cubic spline curves on the probability scale."), o$splineKnots))
                    cited <- c(cited, "austin")
                }
                if (isTRUE(o$hosmerLemeshow)) {
                    # tied risks merge quantile groups; the HL table's Groups column has the count used
                    cal <- c(cal, sprintf(.("The Hosmer-Lemeshow test (Hosmer, Lemeshow & Sturdivant, 2013) used up to %s groups of predicted risk (fewer where predicted risks tie; the Hosmer-Lemeshow table gives the number used); a non-significant result is not evidence of good calibration (Van Calster et al., 2019)."), o$hlGroups))
                    cited <- c(cited, "hosmer", "vancalster")
                }
                out <- c(out, para(.("Calibration"), cal))
            }

            if (isTRUE(o$internalValidation)) {
                iv <- character(0)
                if (o$validationMethod %in% c("bootstrap", "both")) {
                    iv <- c(sprintf(.("Harrell's bootstrap optimism correction (Harrell, 2015) with %s resamples was applied to a logistic model of each marker: the corrected AUC is the model's apparent AUC minus the mean optimism."), min(o$bootstrapSamples, 200)),
                            .("The interval shown is the apparent AUC's DeLong interval; none is given for the corrected AUC (Noma et al., 2021)."))
                    cited <- c(cited, "harrell", "noma")
                }
                if (o$validationMethod %in% c("cv", "both")) {
                    # F-24/F-26: k as in .populateInternalValidation(); the SD is across folds.
                    k <- min(10L, table(private$.binaryData))
                    if (k >= 2L) iv <- c(iv, sprintf(.("Stratified %s-fold cross-validation of a logistic model of each marker (10 folds, or one per patient of the smaller outcome group when it has fewer than 10) gives the mean of the fold AUCs and their SD across folds, which is not a standard error."), k))
                }
                out <- c(out, para(.("Internal Validation"), iv))
            }

            if (isTRUE(o$clinicalMetrics)) {
                out <- c(out, para(.("Clinical Metrics"), .("Predictive values use Bayes' theorem at the prevalence shown in the table; likelihood ratios are worded with the bands of Jaeschke, Guyatt & Sackett (1994).")))
                cited <- c(cited, "jaeschke")
            }
            # cited by the decision / clinical impact curve subtitle (see .curveSubtitle())
            if (isTRUE(private$.decisionCurves$shifted)) cited <- c(cited, "prentice")

            refs <- c(
                robin = "Robin X, Turck N, Hainard A, et al. (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. BMC Bioinformatics, 12:77.",
                hanley = "Hanley JA, McNeil BJ (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve. Radiology, 143(1):29-36.",
                mannwhitney = "Mann HB, Whitney DR (1947). On a test of whether one of two random variables is stochastically larger than the other. Annals of Mathematical Statistics, 18(1):50-60.",
                delong = "DeLong ER, DeLong DM, Clarke-Pearson DL (1988). Comparing the areas under two or more correlated receiver operating characteristic curves: a nonparametric approach. Biometrics, 44(3):837-845.",
                obuchowski = "Obuchowski NA, Lieber ML (1998). Confidence intervals for the receiver operating characteristic area in studies with small samples. Academic Radiology, 5(8):561-571.",
                feng = "Feng D, Cortese G, Baumgartner R (2017). A comparison of confidence/credible interval methods for the area under the ROC curve for continuous diagnostic tests with small sample size. Statistical Methods in Medical Research, 26(6):2603-2621.",
                davison = "Davison AC, Hinkley DV (1997). Bootstrap Methods and their Application. Cambridge University Press.",
                youden = "Youden WJ (1950). Index for rating diagnostic tests. Cancer, 3(1):32-35.",
                fluss = "Fluss R, Faraggi D, Reiser B (2005). Estimation of the Youden Index and its associated cutoff point. Biometrical Journal, 47(4):458-472.",
                perkins = "Perkins NJ, Schisterman EF (2006). The inconsistency of 'optimal' cutpoints obtained using two criteria based on the receiver operating characteristic curve. American Journal of Epidemiology, 163(7):670-675.",
                clopper = "Clopper CJ, Pearson ES (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. Biometrika, 26(4):404-413.",
                mcclish = "McClish DK (1989). Analyzing a portion of the ROC curve. Medical Decision Making, 9(3):190-195.",
                hosmer = "Hosmer DW, Lemeshow S, Sturdivant RX (2013). Applied Logistic Regression. 3rd ed. Wiley.",
                vancalster = "Van Calster B, McLernon DJ, van Smeden M, Wynants L, Steyerberg EW (2019). Calibration: the Achilles heel of predictive analytics. BMC Medicine, 17:230.",
                austin = "Austin PC, Steyerberg EW (2019). The Integrated Calibration Index (ICI) and related metrics for quantifying the calibration of logistic regression models. Statistics in Medicine, 38(21):4051-4065.",
                harrell = "Harrell FE (2015). Regression Modeling Strategies. 2nd ed. Springer.",
                noma = "Noma H, Shinozaki T, Iba K, Teramukai S, Furukawa TA (2021). Confidence intervals of prediction accuracy measures for multivariable prediction models based on the bootstrap-based optimism correction methods. Statistics in Medicine, 40(26):5691-5701.",
                jaeschke = "Jaeschke R, Guyatt GH, Sackett DL (1994). Users' guides to the medical literature. III. How to use an article about a diagnostic test. B. What are the results and will they help me in caring for my patients? JAMA, 271(9):703-707.",
                prentice = "Prentice RL, Pyke R (1979). Logistic disease incidence models and case-control studies. Biometrika, 66(3):403-411."
            )
            out <- c(out, paste0("<h4>", .("References"), "</h4>"), "<ul>", paste0("<li>", refs[unique(cited)], "</li>"), "</ul>")
            self$results$results$methodsExplanation$setContent(paste(out, collapse = ""))
        },
        .populateClinicalInterpretation = function() {
            html <- self$results$results$clinicalInterpretationGuide

            interpretation <- paste0("<h3>", .("Clinical Application Guidance"), "</h3>")

            if (length(private$.rocResults) > 0) {
                context <- self$options$clinicalContext
                # Direction-aware (VAL-06): one sentence when every marker was read the same way.
                dirs <- unique(vapply(private$.rocResults, function(r) r$roc$direction, character(1)))
                meaning <- if (length(dirs) == 1L) {
                    private$.aucMeaning(dirs)
                } else {
                    .("For each marker the AUC is the probability that a randomly chosen case has a value further in the direction that indicates the condition (higher or lower, as each marker was read) than a randomly chosen non-case (ties count one half).")
                }

                interpretation <- paste0(
                    interpretation,
                    "<p><strong>", .("Clinical context"), ":</strong> ",
                    private$.safeHtmlOutput(private$.choiceLabel("clinicalContext", context)), "</p>"
                )

                interpretation <- paste0(
                    interpretation,
                    "<h4>", .("AUC Interpretation Guidelines"), "</h4>",
                    # Same edges as the AUC table note and .interpretAUC(): each band includes its
                    # lower edge, and an AUC is banded as displayed (3 dp), so 0.895 is Good.
                    "<ul>",
                    "<li><strong>", .("Excellent discrimination"), ":</strong> ", .("AUC 0.90 or more"), "</li>",
                    "<li><strong>", .("Good discrimination"), ":</strong> ", .("AUC from 0.80 to below 0.90"), "</li>",
                    "<li><strong>", .("Fair discrimination"), ":</strong> ", .("AUC from 0.70 to below 0.80"), "</li>",
                    "<li><strong>", .("Poor discrimination"), ":</strong> ", .("AUC from 0.60 to below 0.70"), "</li>",
                    "<li><strong>", .("Minimal discrimination"), ":</strong> ", .("AUC from 0.50 to below 0.60 - little separation on its own, although the marker may still contribute inside a multivariable model"), "</li>",
                    # F-15: the null share below 0.5 depends on how the direction was set.
                    "<li><strong>", .("Below chance"), ":</strong> AUC &lt; 0.50 - ", if (identical(self$options$direction, "auto")) {
                        .("with Direction set to auto a marker carrying no information lands here about 1 in 9 times at 10 positive and 10 negative patients, rising toward 1 in 6 in large samples; only a confidence interval wholly below 0.50 shows that the marker runs the other way (check Positive class, then Direction)")
                    } else {
                        .("about half of markers carrying no information land here by chance; only a confidence interval wholly below 0.50 shows that the marker runs the other way (check Positive class, then Direction)")
                    }, "</li>",
                    "</ul>",
                    "<p>", meaning, " ", private$.aucBandsCaveat(), " ",
                    .("They describe that probability; they are not a judgement of whether a marker is fit for patient care."), "</p>"
                )

                # Context-specific guidance
                if (context == "screening") {
                    interpretation <- paste0(
                        interpretation,
                        "<h4>", .("Screening Application Guidelines"), "</h4>",
                        "<ul>",
                        "<li>", .("High sensitivity (above 0.90) is preferred, to minimise false negatives."), "</li>",
                        "<li>", .("Specificity should be balanced to avoid excessive false positives."), "</li>",
                        "<li>", .("Consider how prevalence affects the positive predictive value."), "</li>",
                        "</ul>"
                    )
                } else if (context == "diagnosis") {
                    interpretation <- paste0(
                        interpretation,
                        "<h4>", .("Diagnostic Application Guidelines"), "</h4>",
                        "<ul>",
                        "<li>", .("Balance sensitivity and specificity according to the clinical consequences of each kind of error."), "</li>",
                        "<li>", .("High specificity is preferred for confirmatory testing."), "</li>",
                        "<li>", .("Consider likelihood ratios for clinical decision-making."), "</li>",
                        "</ul>"
                    )
                }

                interpretation <- paste0(
                    interpretation,
                    "<h4>", .("Clinical Decision Points"), "</h4>",
                    "<ul>",
                    "<li><strong>", .("Youden index"), ":</strong> ", .("J = sensitivity + specificity - 1. The cutoff that maximises it weighs sensitivity and specificity equally; at prevalence p that counts one missed case as (1 - p)/p false alarms, the same cost only when half the patients have the condition. That is a convention, not a clinical optimum."), "</li>",
                    "<li><strong>", .("Clinical thresholds"), ":</strong> ", .("Weigh the cost of a false positive against the cost of a false negative."), "</li>",
                    "<li><strong>", .("Prevalence"), ":</strong> ", .("A higher prevalence raises the positive predictive value."), "</li>",
                    "</ul>"
                )
            }

            html$setContent(interpretation)
        },

        # Helper functions for interpretations
        # State, in one plain sentence, which way each marker was read. psychopdaROC sits in the
        # same menu and defaults to a FIXED direction while this analysis defaults to reading the
        # direction off the data, so the same column can come back as AUC 0.8999 here and 0.1001
        # there. Neither is miscalculating, but nothing on screen said why. The advice follows
        # psychopdaROC's (PS4): positive class first, then Direction, which makes the AUC 1 - AUC
        # and re-estimates the cutpoint, sensitivity and specificity (each analysis names its own
        # option labels). `key`: "direction_used" on the AUC table, "reading" on the Outcome coding
        # table. Labels go through .rocLabel(): the client shows HTML entities in notes literally.
        .noteDirection = function(table, roc_objects, key = "direction_used") {
            if (length(roc_objects) == 0) return(invisible(NULL))
            opt <- tryCatch(self$options$direction, error = function(e) "auto")
            pos <- .rocLabel(private$.positiveClass)   # always set before a binary ROC is fitted
            # Whole translatable sentences, one msgid per reading (all higher / all lower / mixed),
            # joined outside the msgids: "HIGHER values of %s" used to be spliced in as a noun
            # phrase. <i>, not <b>: the client's table-note renderer keeps only em, i, sub and sup.
            higher <- vapply(roc_objects, function(r) identical(r$direction, "<"), logical(1))
            hi <- paste(.rocLabel(names(roc_objects)[higher]), collapse = ", ")
            lo <- paste(.rocLabel(names(roc_objects)[!higher]), collapse = ", ")
            reading <- if (all(higher)) {
                sprintf(.("Reading of the test values: <i>HIGHER values of %1$s were taken to indicate %2$s</i>."), hi, pos)
            } else if (!any(higher)) {
                sprintf(.("Reading of the test values: <i>LOWER values of %1$s were taken to indicate %2$s</i>."), lo, pos)
            } else {
                sprintf(.("Reading of the test values: <i>HIGHER values of %1$s and LOWER values of %2$s were taken to indicate %3$s</i>."), hi, lo, pos)
            }
            chosen <- if (identical(opt, "auto")) {
                .("This was read from the data, not specified in advance \u2014 set Direction explicitly to pin it.")
            } else {
                sprintf(.("This is what you specified (Direction = \"%s\")."), opt)
            }
            advice <- sprintf(.("If that reading looks reversed for a marker, first check Positive class: is %s the level that means the condition is present? Only if it is, change Direction: the AUC becomes 1 minus the value shown, and the cutpoint, sensitivity and specificity are re-estimated."), pos)
            tryCatch(table$setNote(key, paste(reading, chosen, advice)), error = function(e) NULL)
        },

        # What the AUC is, for the direction pROC used (VAL-06). With Direction = lower it is the
        # probability that a case has the LOWER value; "scores above" stated 1 - AUC for such markers.
        # Whole sentences, one msgid each (a noun phrase spliced into "The AUC is %s." left the
        # translator two halves of one sentence); `marker` gives the per-marker variant.
        .aucMeaning = function(direction, marker = NULL) {
            lower <- isTRUE(direction == ">")
            if (is.null(marker)) {
                if (lower) .("The AUC is the probability that a randomly chosen case has a LOWER value than a randomly chosen non-case (ties count one half).")
                else .("The AUC is the probability that a randomly chosen case has a HIGHER value than a randomly chosen non-case (ties count one half).")
            } else if (lower) {
                sprintf(.("%s: the AUC is the probability that a randomly chosen case has a LOWER value than a randomly chosen non-case (ties count one half)."), marker)
            } else {
                sprintf(.("%s: the AUC is the probability that a randomly chosen case has a HIGHER value than a randomly chosen non-case (ties count one half)."), marker)
            }
        },
        # F-14: how the direction was set, one whole sentence per setting, for both Methods texts.
        # pROC's "auto" puts cases above controls when median(controls) <= median(cases).
        .directionMethodsText = function() {
            switch(self$options$direction,
                auto = .("The direction of each marker (whether higher or lower values indicate the positive class) was chosen from these same data by comparing the two groups' medians, which biases the AUC upward."),
                lower = .("Lower values of each marker were taken to indicate the positive class, as specified in the analysis options rather than estimated from these data."),
                .("Higher values of each marker were taken to indicate the positive class, as specified in the analysis options rather than estimated from these data."))
        },
        # Where the AUC bands come from (VAL-25c); shared by the AUC table note, the clinical guide
        # and the instructions so the three say the same thing.
        .aucBandsCaveat = function() {
            .("These bands are a rough teaching scale, not a clinical standard; other authors label the same ranges differently (Hosmer, Lemeshow & Sturdivant 2013 call 0.7-0.8 acceptable, 0.8-0.9 excellent and 0.9 or more outstanding).")
        },

        .interpretAUC = function(auc) {
            # Band the AUC as displayed (3 dp): an AUC of exactly 80/100 computes as
            # 0.7999999999999999 and read "Fair" beside a printed 0.800 (VAL-07). Same bands and
            # words as psychopdaROC.
            auc <- round(auc, 3)
            if (auc >= 0.90) {
                return(.("Excellent"))
            }
            if (auc >= 0.80) {
                return(.("Good"))
            }
            if (auc >= 0.70) {
                return(.("Fair"))
            }
            if (auc >= 0.60) {
                return(.("Poor"))
            }
            if (auc >= 0.50) {
                return(.("Minimal"))
            }
            # Not "(reversed)": direction can be a fixed user choice, and under the null the
            # AUC is symmetric about 0.5, so about half of all uninformative markers fall below
            # it by chance (simulated: 0.492 of 2000 null markers at n = 200). Reversal is
            # asserted only where the confidence interval supports it - see the notice raised
            # alongside this, which checks exactly that.
            return(.("Below chance"))
        },
        # The AUC restated as the ranking probability it is, in the direction the marker was read
        # (VAL-06). The 0.75 (screening) / 0.80 (diagnosis) "conventional" reference levels that used
        # to follow had no locatable source (VAL-24).
        .assessClinicalUtility = function(auc, direction) {
            if (is.na(auc)) {
                return(.("AUC not available"))
            }
            # "(ties count one half)": with a tied marker the strict share is lower by P(tie) / 2.
            if (isTRUE(direction == ">")) {
                sprintf(.("A random case has a lower value than a random non-case %.0f%% of the time (ties count one half)"), auc * 100)
            } else {
                sprintf(.("A random case has a higher value than a random non-case %.0f%% of the time (ties count one half)"), auc * 100)
            }
        },
        .generateClinicalRecommendation = function(optimal, context, predictor) {
            # Compared and printed at the same 2 dp, so a band never disagrees with its number (VAL-07).
            sens <- round(optimal$sensitivity, 2)
            spec <- round(optimal$specificity, 2)

            if (context == "screening") {
                # PPV lives in the separate Clinical Metrics table, which is off by default,
                # and its prevalence is either the observed one or the value the user typed.
                prev_txt <- if (isTRUE(self$options$useObservedPrevalence)) {
                    .("the prevalence observed in these data")
                } else {
                    # 3 significant digits: "%.3f" showed a screening prevalence of 0.0004 as 0.000
                    sprintf(.("the prevalence you entered (%s)"), sprintf("%.3g", self$options$prevalence))
                }
                ppv_hint <- if (isTRUE(self$options$clinicalMetrics)) {
                    sprintf(.("The Clinical Application Metrics table reports the PPV at %s."), prev_txt)
                } else {
                    sprintf(.("Switch on 'Clinical metrics' to see the PPV at %s in the Clinical Application Metrics table."), prev_txt)
                }
                if (sens >= 0.90 && spec >= 0.70) {
                    return(paste(sprintf(.("Sensitivity %1$.2f, specificity %2$.2f: at this cutpoint few cases were missed in this sample. What share of the positive results are true positives is not fixed by these two numbers; it also depends on prevalence."), sens, spec), ppv_hint))
                } else if (sens >= 0.85) {
                    return(paste(sprintf(.("Sensitivity %1$.2f, specificity %2$.2f: the false-positive burden depends on prevalence."), sens, spec), ppv_hint))
                } else {
                    return(sprintf(.("Sensitivity %1$.2f, specificity %2$.2f: false negatives were not rare in this sample."), sens, spec))
                }
            } else if (context == "diagnosis") {
                if (sens >= 0.80 && spec >= 0.80) {
                    return(sprintf(.("Sensitivity %1$.2f and specificity %2$.2f, both at or above 0.80 at this cutpoint."), sens, spec))
                } else if (spec >= 0.90) {
                    return(sprintf(.("Specificity %1$.2f with sensitivity %2$.2f: few false positives in this sample."), spec, sens))
                } else {
                    return(sprintf(.("Sensitivity %1$.2f, specificity %2$.2f at this cutpoint."), sens, spec))
                }
            } else {
                # F-18: the 0.6 / 0.4 edges behind 'strong / moderate / limited separation' had no
                # source; the value is restated, not graded (the table note says it is optimistic).
                return(sprintf(.("Youden index %1$.2f at this cutpoint (sensitivity %2$.2f, specificity %3$.2f)."),
                               round(optimal$youden_index, 2), sens, spec))
            }
        },
        .interpretClinicalMetrics = function(ppv, npv, lr_pos, lr_neg, context) {
            # Bands of Jaeschke, Guyatt & Sackett (1994) - large above 10 / below 0.1, so exactly 10
            # and 0.1 are moderate ("5-10", "0.1-0.2"), as in decision's .lrBand() - worded as
            # changes in the probability of the condition - the source's "large / moderate / small
            # (sometimes important)" - not as "evidence for a diagnosis" (VAL-25d). Banded on the
            # ratio at jamovi's default number format, 3 significant figures (2 dp put LR- 0.104,
            # shown as 0.104, in "large"); a column holding several predictors, or a changed
            # Number format, can still show other digits. An infinite ratio is explained by the
            # caller ("LR+ is infinite ..."), so it gets no band here.
            p <- signif(lr_pos, 3)
            pos_txt <- if (is.na(p)) {
                .("LR+ not estimable.")
            } else if (is.infinite(p)) {
                ""
            } else if (p > 10) {
                .("LR+ above 10: large increase in the probability of the condition.")
            } else if (p >= 5) {
                .("LR+ 5 to 10: moderate increase in the probability of the condition.")
            } else if (p >= 2) {
                .("LR+ 2 to 5: small but sometimes important increase in the probability of the condition.")
            } else if (p > 1) {
                .("LR+ 1 to 2: small, rarely important increase in the probability of the condition.")
            } else if (p == 1) {
                .("LR+ 1: a positive result does not change the probability of the condition.")
            } else {
                .("LR+ below 1: a positive result LOWERS the probability of the condition.")
            }
            n <- signif(lr_neg, 3)
            neg_txt <- if (is.na(n)) {
                .("LR- not estimable.")
            } else if (is.infinite(n)) {
                ""
            } else if (n < 0.1) {
                .("LR- below 0.1: large decrease in the probability of the condition.")
            } else if (n <= 0.2) {
                .("LR- 0.1 to 0.2: moderate decrease in the probability of the condition.")
            } else if (n <= 0.5) {
                .("LR- 0.2 to 0.5: small but sometimes important decrease in the probability of the condition.")
            } else if (n < 1) {
                .("LR- 0.5 to 1: small, rarely important decrease in the probability of the condition.")
            } else if (n == 1) {
                .("LR- 1: a negative result does not change the probability of the condition.")
            } else {
                .("LR- above 1: a negative result RAISES the probability of the condition.")
            }
            interpretation <- trimws(paste(pos_txt, neg_txt))

            # Add NPV/PPV context
            if (context == "screening") {
                lr_neg_txt <- if (is.na(lr_neg)) {
                    .("not estimable")
                } else if (is.infinite(lr_neg)) {
                    .("infinite")
                } else {
                    # the precision the band above was decided on (3 significant figures)
                    sub("\\.$", "", formatC(signif(lr_neg, 3), digits = 3, format = "fg", flag = "#", decimal.mark = "."))
                }
                interpretation <- paste(interpretation, sprintf(
                    .("- NPV %1$.2f at the prevalence used in this table; at low prevalence a high NPV is expected even for a weak test, so LR- (%2$s) is the prevalence-independent rule-out measure."),
                    npv, lr_neg_txt
                ))
            }

            return(interpretation)
        },
        .assessPartialAUCRelevance = function(pauc, range_min, range_max, context, range_type = "specificity") {
            # Provide context-aware relevance assessment
            if (range_type == "sensitivity") {
                if (context == "screening" && range_min >= 0.8) {
                    return(.("Relevant for high-sensitivity screening applications (minimizing false negatives)"))
                } else if (range_min >= 0.9) {
                    return(.("Very high sensitivity range - critical for screening/case-finding"))
                } else {
                    return(.("Partial AUC over sensitivity range"))
                }
            } else {
                # Specificity-based assessment
                if (context == "screening" && range_min >= 0.8) {
                    return(.("Relevant for high-specificity screening applications (minimizing false positives)"))
                } else if (context == "diagnosis" && range_min >= 0.9) {
                    return(.("Confirmatory diagnostic range with high specificity"))
                } else if (context == "diagnosis") {
                    return(.("Partial AUC for specific diagnostic range"))
                } else {
                    return(.("Partial area under curve analysis"))
                }
            }
        },
        .calculateBinomialCI = function(successes, n) {
            # Exact (Clopper-Pearson) confidence interval at the level the user asked for.
            # binom.test()'s conf.level defaults to 0.95, so omitting it printed 95% limits for
            # sensitivity and specificity in the same table as an AUC interval that DID honour
            # the Confidence level option - a user who set 99% got a silently mixed table with
            # no indication which column was which. The column titles carry the level too; see
            # the getColumn()$setTitle() calls in .init().
            conf <- (self$options$confidenceLevel %||% 95) / 100
            bt <- suppressWarnings(binom.test(successes, n, conf.level = conf))
            return(bt$conf.int)
        },
        .computePRMetrics = function(scores, labels, positive_label, roc_direction = "<") {
            pr <- .dvPR(scores, labels, positive_label, higher = roc_direction != ">")
            if (is.null(pr)) return(NULL)
            # F-05: .dvPR()'s AUC-PR is the PR-space trapezoid, which overstates the area with
            # tied scores (a constant marker read 0.75 against a 0.5 baseline). The interpolated
            # area (.dvAuprc(), Davis & Goadrich 2006) replaces it here and in the frozen panel.
            pr$auprc <- .dvAuprc(scores, labels, positive_label, higher = roc_direction != ">")
            pr
        },
        .populatePrecisionRecall = function(analysisData) {
            if (!self$options$detectImbalance) {
                return()
            }

            prTable <- self$results$results$precisionRecallTable
            .deleteTableRows(prTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates
            prTable$setNote("auc_pr", .("AUC-PR is the area under the precision-recall curve interpolated between neighbouring thresholds linearly in true- and false-positive counts, which traces achievable operating points (Davis & Goadrich 2006), integrated exactly (Keilwagen, Grosse & Grau 2014). It is not a trapezoid in precision-recall space: straight lines there, and a start at precision 1 when the highest score is tied between classes, overstate the area. A marker with no information scores about the prevalence. Average precision is the step-wise estimate: the area under the step curve in the Precision-Recall plot."))
            outcome <- analysisData[[private$.outcome]]

            for (predictor in private$.predictors) {
                scores <- analysisData[[predictor]]
                roc_dir <- if (!is.null(private$.rocObjects[[predictor]])) private$.rocObjects[[predictor]]$direction else "<"
                pr <- private$.computePRMetrics(
                    scores, outcome,
                    private$.positiveClass,
                    roc_dir
                )
                if (!is.null(pr)) {
                    prTable$addRow(rowKey = private$.escapeVar(predictor), values = list(
                        predictor = predictor,
                        auc_pr = pr$auprc,
                        f1_score = pr$max_f1,
                        precision = pr$precision,
                        recall = pr$recall,
                        average_precision = pr$avg_precision
                    ))
                }
            }
        },
        # Nine renderers used to start from ggplot2::theme_minimal() here and never added their
        # `ggtheme` argument at all, so jamovi's own theme (including its dark mode) was thrown
        # away for those plots. They now all read `ggtheme + private$.plotThemeFor() + theme(...)`,
        # the order .plotROCCurve already used: ggtheme first because it REPLACES what precedes it.
        #
        # Maps the plotTheme option onto a ggplot2 theme. Returned as an ADDITION to whatever the
        # renderer already set, never as a replacement: jamovi's `ggtheme` is a LIST of
        # [theme, ggPalette scales] (jmvcore::theme_default), so substituting a bare theme object
        # for it would silently throw away the colour and fill palettes and the transparent panel
        # background that jamovi's PNG rendering assumes. A partial theme() merges, so jamovi's
        # colours and background survive every choice.
        .plotThemeFor = function() {
            # theme_classic() and theme_light() are COMPLETE themes, so returning them here
            # replaced `ggtheme` just as thoroughly as theme_minimal() used to - white panels
            # and black text came straight back on jamovi's dark theme for any user who picked
            # Classic or Modern, which is the very thing the ggtheme change set out to fix.
            # Return only the incremental deltas that distinguish each look. An incomplete
            # theme() MERGES into ggtheme, so jamovi's colours and panel background survive.
            # jamovi's own theme has no grid and no border, so Classic (that look) and Clinical (a
            # theme() no-op) drew identical plots everywhere but the three renderers that painted
            # a white panel and grey grid for Clinical - a white box on jamovi's dark theme. The
            # Clinical grid now lives here, for all twelve plots, in translucent grey that reads on
            # either background; nothing sets a panel fill.
            grid_major <- ggplot2::element_line(colour = "#80808040")
            switch(as.character(self$options$plotTheme),
                classic = ggplot2::theme(
                    panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.border     = ggplot2::element_blank(),
                    axis.line        = ggplot2::element_line(colour = NULL)
                ),
                modern = ggplot2::theme(
                    panel.grid.major = grid_major,
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.border     = ggplot2::element_rect(fill = NA, colour = NULL),
                    axis.line        = ggplot2::element_blank()
                ),
                ggplot2::theme(
                    panel.grid.major = grid_major,
                    panel.grid.minor = ggplot2::element_line(colour = "#80808020")
                )
            )
        },

        .getColorblindSafePalette = function(n) {
            # Colorblind-safe palette based on Cynthia Brewer's research
            # These colors are distinguishable for most types of color vision deficiency
            cb_palette <- c(
                "#0173B2", # Blue
                "#DE8F05", # Orange
                "#029E73", # Green
                "#CC78BC", # Pink
                "#CA9161", # Brown
                "#FBAFE4", # Light pink
                "#949494", # Grey
                "#ECE133", # Yellow
                "#56B4E9" # Light blue
            )

            if (n <= length(cb_palette)) {
                return(cb_palette[1:n])
            } else {
                # For more than 9 colors, interpolate
                return(colorRampPalette(cb_palette)(n))
            }
        },
        .applyClinicalPresets = function() {
            # Apply predefined clinical settings based on selected preset
            preset <- self$options$clinicalPresets

            if (preset == "custom") {
                return() # No changes for custom configuration
            }

            # Apply preset-specific configurations (these would be applied programmatically)
            # Note: In jamovi, we can't directly modify options from backend,
            # but we can use the preset to inform our analysis approach

            if (preset == "biomarker_screening") {
                # High sensitivity configuration
                private$.presetConfig <- list(
                    focus = "sensitivity",
                    threshold = 0.90,
                    clinical_priority = "screening",
                    recommendation = .("Optimized for biomarker screening with high sensitivity")
                )
            } else if (preset == "diagnostic_validation") {
                # Balanced sensitivity and specificity
                private$.presetConfig <- list(
                    focus = "balanced",
                    sens_threshold = 0.80,
                    spec_threshold = 0.80,
                    clinical_priority = "diagnosis",
                    recommendation = .("Balanced approach for diagnostic test validation")
                )
            } else if (preset == "confirmatory_testing") {
                # High specificity configuration
                private$.presetConfig <- list(
                    focus = "specificity",
                    threshold = 0.95,
                    clinical_priority = "diagnosis",
                    recommendation = .("Optimized for confirmatory testing with high specificity")
                )
            } else if (preset == "research_comprehensive") {
                # Comprehensive analysis
                private$.presetConfig <- list(
                    focus = "comprehensive",
                    clinical_priority = "research",
                    recommendation = .("Comprehensive analysis for research applications")
                )
            }
        },
        .generateAnalysisSummary = function() {
            html <- self$results$results$analysisSummary

            # Generate plain language summary
            n_predictors <- length(private$.rocResults)
            n_obs <- nrow(private$.analysisData)
            context <- self$options$clinicalContext

            # Find best performing predictor (from -Inf: an AUC of 0 is a marker read backwards,
            # not a missing result)
            best_auc <- -Inf
            best_predictor <- ""
            for (pred in names(private$.rocResults)) {
                auc_val <- as.numeric(private$.rocObjects[[pred]]$auc)
                if (auc_val > best_auc) {
                    best_auc <- auc_val
                    best_predictor <- pred
                }
            }

            # Generate summary paragraph
            summary_text <- paste0(
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>",
                "<h4 style='margin-top: 0; color: inherit;'>", .("Analysis Summary"), "</h4>",
                "<p><strong>", .("ROC Analysis Results:"), "</strong> ",
                sprintf(
                    .("This analysis evaluated %1$d predictor(s) using %2$d observations in a %3$s context."),
                    n_predictors, n_obs, private$.safeHtmlOutput(private$.choiceLabel("clinicalContext", context))
                ), " "
            )

            if (nzchar(best_predictor)) {
                clinical_utility <- private$.assessClinicalUtility(best_auc, private$.rocObjects[[best_predictor]]$direction)

                # .assessClinicalUtility() returns a full sentence fragment, so it is appended as
                # its own sentence rather than folded into the parenthesis.
                utility_clause <- if (nzchar(clinical_utility)) {
                    paste0(" ", private$.safeHtmlOutput(clinical_utility), ".")
                } else {
                    ""
                }
                summary_text <- paste0(
                    summary_text,
                    sprintf(
                        # No band word here: a spliced "(Good performance)" cannot be translated as one
                        # sentence, and the AUC table already carries the band with its caveat.
                        .("The best performing predictor was '%1$s' with an AUC of %2$.3f.%3$s"),
                        private$.safeHtmlOutput(best_predictor), best_auc, utility_clause
                    ),
                    "</p>"
                )

                # Describe what was estimated; no fitness-for-care verdict is issued here.
                summary_text <- paste0(
                    summary_text,
                    "<p><strong>", .("Note"), ":</strong> ",
                    .("This AUC is apparent (in-sample), and where a cutpoint was searched for on these data the cutpoint metrics are optimistic. Independent validation is needed before these figures describe performance in new patients."), "</p>"
                )
            }

            summary_text <- paste0(summary_text, "</div>")
            private$.analysisSummaryHtml <- summary_text
            html$setContent(summary_text)
        },
        .generateClinicalReport = function() {
            html <- self$results$results$clinicalReport

            # Generate copy-ready clinical report sentences
            n_predictors <- length(private$.rocResults)
            n_obs <- nrow(private$.analysisData)

            # Find best performing predictor (from -Inf: an AUC-0 marker must reach the withheld-report
            # branch below, not 'No Valid ROC Results')
            best_auc <- -Inf
            best_predictor <- ""
            best_cutoff <- NULL
            for (pred in names(private$.rocResults)) {
                auc_val <- as.numeric(private$.rocObjects[[pred]]$auc)
                if (auc_val > best_auc) {
                    best_auc <- auc_val
                    best_predictor <- pred
                    best_cutoff <- private$.rocResults[[pred]]$optimal_cutoff
                }
            }

            if (!nzchar(best_predictor)) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("No Valid ROC Results"),
                    content = .("No valid ROC results available for report generation. \u2022 ROC analysis may have failed for all predictors. \u2022 Check that outcome variable is binary and predictors are numeric. \u2022 Verify sufficient data for each predictor-outcome combination.")
                )
                return()
            }

            # This block is explicitly labelled "copy and paste into your clinical reports or
            # publications", so it must not emit a sentence that is wrong. When the best AUC is
            # below chance, or the chosen cut-point is degenerate, every threshold has Youden
            # J <= 0, which.max() lands on the +/-Inf endpoint, and the wording rule
            # (sensitivity >= 0.8 -> "high") turned that into: "At the optimal cutoff of Inf, the
            # test achieved high sensitivity (100.0%) and low specificity (0.0%)" for a marker
            # with AUC 0.219. Refuse to write the report rather than write that.
            best_cut_value <- if (is.null(best_cutoff)) NA_real_ else best_cutoff$cutoff
            best_youden <- if (is.null(best_cutoff)) NA_real_ else best_cutoff$youden_index
            # Both edges compare the values as displayed (VAL-07).
            report_unsafe <- round(best_auc, 3) < 0.5 ||
                !is.finite(best_cut_value) ||
                !is.finite(best_youden) || round(best_youden, 3) <= 0
            if (isTRUE(report_unsafe)) {
                # Same splicing defect: this clause is dropped into two translated sentences.
                # Neutral about the cause: the interval may still cross 0.5 (see the notices).
                reason <- if (round(best_auc, 3) < 0.5) {
                    .fmt(.("the best AUC is {auc}, below 0.5 - Positive class may name the wrong level, Direction may not match the marker, or the marker may not discriminate at all"),
                         auc = round(best_auc, 3))
                } else {
                    .("no usable cut-point exists - every threshold gives a Youden index of zero or less")
                }
                self$results$results$clinicalReport$setContent(paste0(
                    "<div style='background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; ",
                    "padding: 15px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'>", .("Clinical Report Sentences Not Generated"), "</h4>",
                    "<p>", sprintf(.("No report sentences were written, because %s."), private$.safeHtmlOutput(reason)), " ",
                    .("Reporting a cut-point and a sensitivity from this analysis would misdescribe the marker."),
                    "</p></div>"
                ))
                private$.addNotice(
                    type = "WARNING",
                    title = .("Clinical Report Withheld"),
                    content = sprintf(.("Copy-ready report sentences were not generated because %1$s. Check Positive class (is %2$s the level that means the condition is present?), then Direction, before quoting any cut-point from this analysis."), reason, private$.positiveClass)
                )
                return()
            }

            # The best predictor's interval, or "interval not estimable" where the AUC table leaves
            # it blank (VAL-04) - never "NA" or a zero-width "1--1". Each sentence that quotes it
            # has two whole msgids (with / without the interval) rather than a spliced fragment.
            ci <- suppressWarnings(as.numeric(private$.rocObjects[[best_predictor]]$ci))
            ci_ok <- is.null(private$.rocResults[[best_predictor]]$ci_issue) && length(ci) == 3
            conf <- format(self$options$confidenceLevel)
            ci_lo <- round(ci[1], 3)
            ci_hi <- round(ci[3], 3)

            # Generate report sections
            report_html <- "<div style='background-color: rgba(33, 152, 255, 0.07); border: 1px solid #0066cc; padding: 15px; margin: 10px 0; color: inherit;'>"
            report_html <- paste0(report_html, "<h4 style='color: inherit; margin-top: 0;'>", .("Clinical Report Sentences"), "</h4>")
            report_html <- paste0(report_html, "<p><em>", .("Copy and paste the sections below into your clinical reports or publications"), ":</em></p>")

            # Methods section
            report_html <- paste0(report_html, "<h5>", .("Methods Section"), ":</h5>")
            report_html <- paste0(report_html, "<div style='background-color: rgba(127, 127, 127, 0.06); color: inherit; padding: 10px; border-left: 4px solid #0066cc; margin: 5px 0;'>")
            methods_text <- sprintf(
                .("ROC analysis was performed to evaluate the diagnostic performance of %1$s in predicting %2$s using %3$d observations. Empirical AUCs were calculated using the pROC package in R."),
                ifelse(n_predictors == 1, paste0("'", private$.safeHtmlOutput(best_predictor), "'"), .fmt(.("{n} predictors"), n = n_predictors)),
                private$.safeHtmlOutput(private$.outcome),
                n_obs
            )
            methods_text <- paste(methods_text,
                paste(private$.safeHtmlOutput(private$.aucCiText()), collapse = " "))
            # Which level is positive travels with the pasted text (the Outcome coding table does
            # not): "case" and "non-case" below mean these levels.
            pos_html <- private$.safeHtmlOutput(private$.positiveClass)
            neg_html <- private$.safeHtmlOutput(private$.negativeLevels)
            coding_text <- if (length(neg_html) == 1L) {
                sprintf(.("The positive class (cases) was %1$s and the negative class (non-cases) was %2$s."), pos_html, neg_html)
            } else {
                sprintf(.("The positive class (cases) was %1$s; the other levels (%2$s) were combined as the negative class (non-cases)."), pos_html, paste(neg_html, collapse = ", "))
            }
            # F-14: how the direction was set travels with the pasted Methods (auto reads it from
            # these data, which biases the AUC upward). F-00: every interval and test here treats a
            # row as an independent patient.
            report_html <- paste0(report_html, methods_text, " ", coding_text, " ", private$.directionMethodsText(), " ",
                                  .("Each row was analysed as a separate, independent patient; the confidence intervals and tests assume this."), "</div>")

            # Results section
            report_html <- paste0(report_html, "<h5>", .("Results Section"), ":</h5>")
            report_html <- paste0(report_html, "<div style='background-color: rgba(127, 127, 127, 0.06); color: inherit; padding: 10px; border-left: 4px solid #28a745; margin: 5px 0;'>")

            # F-06: the AUC and its interval, with no band verdict ('demonstrated Good diagnostic
            # performance' was printed whatever the interval); a clause when the interval includes
            # 0.5, and the reason when there is no interval (perfect separation names the sample size).
            pred_html <- private$.safeHtmlOutput(best_predictor)
            results_text <- if (!ci_ok) {
                paste(c(sprintf(.("The AUC of %1$s was %2$.3f (interval not estimable)."), pred_html, best_auc),
                        private$.safeHtmlOutput(private$.rocResults[[best_predictor]]$ci_issue$why)), collapse = " ")
            } else if (ci_lo <= 0.5) {
                sprintf(.("The AUC of %1$s was %2$.3f (%3$s%% CI: %4$s--%5$s); the interval includes 0.5, so these data are compatible with no discrimination."),
                        pred_html, best_auc, conf, ci_lo, ci_hi)
            } else {
                sprintf(.("The AUC of %1$s was %2$.3f (%3$s%% CI: %4$s--%5$s)."), pred_html, best_auc, conf, ci_lo, ci_hi)
            }
            # F-07: the 'Low Per-Class Count' ERROR calls these estimates unreliable; the pasted text says so.
            n_case <- length(private$.rocObjects[[best_predictor]]$cases)
            n_ctrl <- length(private$.rocObjects[[best_predictor]]$controls)
            if (min(n_case, n_ctrl) < 10) {
                results_text <- paste(sprintf(.("With only %1$s positive and %2$s negative patients (fewer than 10 in a class), the estimates below are unreliable."), n_case, n_ctrl),
                                      results_text)
            }
            # Name the criterion that chose the cutoff, and no unsourced "high / moderate / low"
            # words for sensitivity and specificity (VAL-25h).
            cut_fmt <- if (identical(best_cutoff$method, "closest_topleft")) {
                .("At the cutoff closest to the top-left corner of the ROC curve, %1$s, sensitivity was %2$.1f%% and specificity %3$.1f%% (Youden index %4$.3f).")
            } else if (isTRUE(best_cutoff$min_sens > 0 || best_cutoff$min_spec > 0)) {
                .("At the cutoff with the highest Youden index among those meeting the minimum sensitivity and specificity set for this analysis, %1$s, sensitivity was %2$.1f%% and specificity %3$.1f%% (Youden index %4$.3f).")
            } else {
                .("At the cutoff that maximised the Youden index, %1$s, sensitivity was %2$.1f%% and specificity %3$.1f%% (Youden index %4$.3f).")
            }
            # F-12: four significant digits, as the notices print a cut-point; "%.3f" printed 9.55e-12 as
            # 0.000 and 6.3e-4 as 0.001.
            results_text <- paste(results_text, sprintf(cut_fmt, signif(best_cutoff$cutoff, 4), best_cutoff$sensitivity * 100,
                                                        best_cutoff$specificity * 100, best_cutoff$youden_index))

            # Number needed to diagnose = 1/J, unrounded as Linn & Grunau (2006) report it (VAL-25e);
            # report_unsafe above guarantees J > 0 here.
            nnd_text <- paste0(" ", .fmt(
                .("The number needed to diagnose (1/J; Linn & Grunau 2006) was {nnd}. This is a prevalence-free index of discriminating power, not a count of patients to test: the number that must actually be tested to find one additional case also depends on disease prevalence in the population."),
                nnd = sprintf("%.2f", 1 / best_cutoff$youden_index)
            ))

            report_html <- paste0(report_html, results_text, nnd_text, "</div>")

            # Clinical interpretation
            report_html <- paste0(report_html, "<h5>", .("Clinical Interpretation"), ":</h5>")
            report_html <- paste0(report_html, "<div style='background-color: rgba(127, 127, 127, 0.06); color: inherit; padding: 10px; border-left: 4px solid #ffc107; margin: 5px 0;'>")

            # Whole sentences joined by paste(), as the AUC-table note is. F-06: the band sentence
            # ('good to excellent discrimination', a second band vocabulary) is gone; the Results
            # sentence above carries the AUC and its interval.
            validated <- isTRUE(self$options$internalValidation)
            interpretation_text <- paste(
                private$.aucMeaning(private$.rocObjects[[best_predictor]]$direction),
                .("It is computed over all cutpoints and does not depend on which cutpoint was selected."),
                # F-41: with Internal validation on, the panel below the summary validates it.
                if (n_predictors > 1 && validated) {
                    sprintf(.("This is an in-sample estimate with no external validation behind it, and it is additionally optimistic because this predictor was picked as the highest-scoring of %d; the Internal Validation section reports its internal validation."), n_predictors)
                } else if (n_predictors > 1) {
                    sprintf(.("This is an in-sample estimate with no internal or external validation behind it, and it is additionally optimistic because this predictor was picked as the highest-scoring of %d."), n_predictors)
                } else if (validated) {
                    .("This is an in-sample estimate with no external validation behind it; the Internal Validation section reports its internal validation.")
                } else {
                    .("This is an in-sample estimate with no internal or external validation behind it.")
                },
                .("The sensitivity and specificity quoted at the selected cutpoint are a separate matter: those ARE optimistic, because that cutpoint was searched for on these same data."))

            report_html <- paste0(report_html, interpretation_text, "</div>")

            # Statistical reporting
            if (n_predictors > 1) {
                report_html <- paste0(report_html, "<h5>", .("Comparative Analysis"), ":</h5>")
                report_html <- paste0(report_html, "<div style='background-color: rgba(127, 127, 127, 0.06); color: inherit; padding: 10px; border-left: 4px solid #dc3545; margin: 5px 0;'>")
                if (isTRUE(self$options$pairwiseComparisons) && identical(self$options$analysisType, "comparative") &&
                    identical(self$options$comparisonMethod, "venkatraman")) {
                    # a whole-curve test cannot show that one AUC is higher (VAL-22)
                    comparative_text <- sprintf(
                        .("Among the %1$d predictors evaluated, %2$s had the highest observed AUC (%3$.3f). The Venkatraman permutation test in the ROC Curve Comparisons table asks whether two ROC curves differ anywhere along their range, not whether their AUCs differ, so even a significant result does not show that one AUC is higher; a result that is not significant does not establish that two predictors perform equally."),
                        n_predictors,
                        private$.safeHtmlOutput(best_predictor),
                        best_auc
                    )
                } else if (isTRUE(self$options$pairwiseComparisons) && identical(self$options$analysisType, "comparative")) {
                    comparative_text <- sprintf(
                        .("Among the %1$d predictors evaluated, %2$s had the highest observed AUC (%3$.3f). Pairwise comparisons using %4$s methodology are reported in the ROC Curve Comparisons table; a higher observed AUC is not a demonstrated difference unless the corresponding comparison is statistically significant, and a comparison that is not significant does not establish that two predictors perform equally."),
                        n_predictors,
                        private$.safeHtmlOutput(best_predictor),
                        best_auc,
                        private$.safeHtmlOutput(self$options$comparisonMethod)
                    )
                } else {
                    comparative_text <- sprintf(
                        .("Among the %1$d predictors evaluated, %2$s had the highest observed AUC (%3$.3f). No pairwise test was run, so this ranking describes this sample only; enable pairwise comparisons, with Analysis Type set to comparative, to test whether the AUC differences are statistically significant."),
                        n_predictors,
                        private$.safeHtmlOutput(best_predictor),
                        best_auc
                    )
                }
                report_html <- paste0(report_html, comparative_text, "</div>")
            }

            # the sentences above quote the AUC interval, which is a bootstrap one here
            if (isTRUE(self$options$useBootstrap))
                report_html <- paste0(report_html, "<p><em>", jmvcore::htmlEscape(.fmt(.("Random seed: {seed}"), seed = self$options$seed)), "</em></p>")
            report_html <- paste0(report_html, "</div>")

            html$setContent(report_html)
        },
        # The one assumption check no notice already makes (sample size, class balance and a
        # constant predictor each have their own). It used to be HTML appended to the
        # instructions panel, unsorted and outside the notice list.
        .validateClinicalAssumptions = function(data) {
            # F-45: the POSITIVE share, not the smaller group's.
            pos_share <- if (is.null(private$.positiveClass)) NA_real_ else mean(data[[private$.outcome]] == private$.positiveClass)
            if (self$options$clinicalContext == "screening" && isTRUE(pos_share > 0.30)) {
                private$.addNotice(type = "WARNING", title = .("Screening Context Note"),
                    content = sprintf(.("High disease prevalence (%s%% positive) is unusual for screening populations. Consider whether this sample reflects the population to be screened: PPV and NPV depend on prevalence."),
                        round(100 * pos_share, 1)))
            }
            invisible(NULL)
        },
        .calculateMetricPValue = function(metric, result1, result2, pred1, pred2) {
            # Calculate p-value for metric comparison based on the metric type
            tryCatch(
                {
                    if (metric == "AUC") {
                        # the pair's one test, shared with the other comparison tables
                        test <- private$.pairTest(pred1, pred2)
                        if (!is.null(test$why)) private$.metricPValueFailures <- c(private$.metricPValueFailures,
                            .fmt(.("{metric} ({a} vs {b}): {reason}"), metric = .("AUC"), a = pred1, b = pred2, reason = test$why))
                        return(test$p.value)
                    } else {
                        # For paired metrics (same subjects, different predictors), use McNemar's test
                        if (metric == "Sensitivity" || metric == "Specificity" || metric == "Accuracy") {
                            cutoff1 <- result1$optimal_cutoff
                            cutoff2 <- result2$optimal_cutoff

                            # Build paired classification table from raw data
                            data <- private$.analysisData
                            outcome <- data[[private$.outcome]]
                            pred1_vals <- data[[pred1]]
                            pred2_vals <- data[[pred2]]

                            # Classify each subject by both predictors at their optimal cutoffs
                            # pROC: "<" means controls < cases (higher values = positive)
                            dir1 <- result1$roc$direction
                            dir2 <- result2$roc$direction
                            class1 <- private$.applyDirectionCutoff(pred1_vals, cutoff1$cutoff, dir1)
                            class2 <- private$.applyDirectionCutoff(pred2_vals, cutoff2$cutoff, dir2)

                            # Subset based on metric
                            pos_class <- levels(result1$roc$response)[2]
                            if (metric == "Sensitivity") {
                                # Restrict to true positives
                                mask <- outcome == pos_class
                            } else if (metric == "Specificity") {
                                # Restrict to true negatives; correct = pred NOT positive
                                mask <- outcome != pos_class
                                class1 <- !class1
                                class2 <- !class2
                            } else {
                                # Accuracy: all subjects, correct = match
                                mask <- rep(TRUE, length(outcome))
                                true_pos <- outcome == pos_class
                                class1 <- (class1 & true_pos) | (!class1 & !true_pos)
                                class2 <- (class2 & true_pos) | (!class2 & !true_pos)
                            }

                            c1 <- class1[mask]
                            c2 <- class2[mask]

                            # The guard used to be on total N. The asymptotic McNemar statistic
                            # is (b - c)^2 / (b + c): it depends ONLY on the DISCORDANT pairs,
                            # so `length(c1) >= 2` protected nothing that mattered - 3 discordant
                            # pairs out of 206 patients still printed a normal-looking p-value.
                            # Ten discordant pairs is the usual minimum for the uncorrected
                            # normal approximation; below that leave the cell blank and let the
                            # existing "Some Comparison P-Values Could Not Be Computed" notice
                            # say why, rather than show a number that is not a valid test.
                            n_discordant <- sum(c1 != c2, na.rm = TRUE)
                            if (n_discordant >= 10) {
                                # McNemar's test: are the two predictors equally accurate on paired data?
                                # Fixed levels keep the table 2 x 2 when one marker calls every patient
                                # the same way (table() would drop that level and the test was skipped).
                                cont_table <- table(Pred1 = factor(c1, levels = c(FALSE, TRUE)),
                                                    Pred2 = factor(c2, levels = c(FALSE, TRUE)))
                                return(stats::mcnemar.test(cont_table, correct = FALSE)$p.value)
                            }
                            private$.metricPValueFailures <- c(
                                private$.metricPValueFailures,
                                .fmt(.("{metric} ({a} vs {b}): only {n} patients were classified differently by the two markers, too few for McNemar\u2019s test"),
                                     metric = private$.metricLabel(metric), a = pred1, b = pred2, n = n_discordant)
                            )
                            return(NA)
                        }
                    }

                    # If specific test fails, return NA
                    return(NA)
                },
                error = function(e) {
                    private$.metricPValueFailures <- c(private$.metricPValueFailures,
                        .fmt(.("{metric} ({a} vs {b}): {reason}"), metric = private$.metricLabel(metric), a = pred1, b = pred2,
                             reason = conditionMessage(e)))
                    return(NA)
                }
            )
        },
        .getInstructions = function() {
            instructions <- "<div style='font-family: Arial, sans-serif;'>"
            instructions <- paste0(
                instructions,
                "<h2 style='color: inherit;'>", .("Clinical ROC Analysis"), "</h2>",
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #28a745; margin: 10px 0; color: inherit;'>",
                "<p><strong>", .("Purpose"), ":</strong> ", .("ROC (receiver operating characteristic) analysis evaluates how well a continuous variable, such as a biomarker or a test score, distinguishes two outcome groups, for example disease and no disease."), "</p>",
                "</div>",
                "<h3 style='color: inherit;'>", .("Setup Instructions"), "</h3>",
                "<ol style='margin-left: 20px;'>",
                # F-19: vital status at last follow-up is not a binary outcome here (a patient alive
                # with short follow-up counts as a negative); F-00: rows are independent patients.
                "<li><strong>", .("Outcome"), ":</strong> ", .("Select the outcome, for example disease status or malignant versus benign."), "</li>",
                "<ul style='margin-left: 20px; color: inherit;'>",
                "<li>", .("If the outcome has more than two levels, choose the level that represents positive cases; the other levels are combined as negatives."), "</li>",
                "<li>", .("Status at last follow-up (for example dead of disease versus alive) is not a suitable outcome: a patient alive after a short follow-up would count as a negative. Use an outcome fixed at a time horizon that every patient was followed to, or a time-dependent ROC analysis for censored survival data."), "</li>",
                "<li>", .("Each row must be a different patient: several slides, cores or lesions from one patient make the confidence intervals too narrow."), "</li>",
                "</ul>",
                "<li><strong>", .("Predictors"), ":</strong> ", .("Choose one or more continuous predictors, for example a biomarker level or a test score."), "</li>",
                "<li><strong>", .("Positive class"), ":</strong> ", .("Choose the outcome level that represents the condition of interest."), "</li>",
                "<li><strong>", .("Clinical context"), ":</strong> ", .("Choose the context in which the test would be used; it changes the interpretation text, not the numbers."), "</li>",
                "</ol>",
                "<h3 style='color: inherit;'>", .("Quick Start Presets"), "</h3>",
                "<div style='background-color: rgba(33, 144, 255, 0.11); padding: 10px; border-radius: 4px; margin: 10px 0; color: inherit;'>",
                # F-43: each preset described by what .calculateOptimalCutoff() does with it; no
                # preset changes any other option.
                "<p>", .("Clinical presets change only how the optimal cutoff is searched for: each turns Youden index optimization on and sets the minimum sensitivity and specificity."), "</p>",
                "<ul style='margin-left: 20px;'>",
                "<li><strong>", .("Biomarker Screening (High Sensitivity)"), ":</strong> ", .("minimum sensitivity 0.90, no minimum specificity"), "</li>",
                "<li><strong>", .("Diagnostic Test Validation (Balanced)"), ":</strong> ", .("minimum sensitivity 0.80 and minimum specificity 0.80"), "</li>",
                "<li><strong>", .("Confirmatory Testing (High Specificity)"), ":</strong> ", .("minimum specificity 0.95, no minimum sensitivity"), "</li>",
                "<li><strong>", .("Research (Youden with Your Minimums)"), ":</strong> ", .("keeps the minimum sensitivity and specificity you entered"), "</li>",
                "</ul>",
                "</div>",
                "<h3 style='color: inherit;'>", .("Key Output Metrics"), "</h3>",
                "<div style='display: flex; flex-wrap: wrap; gap: 10px; margin: 10px 0;'>",
                "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-radius: 4px; flex: 1; min-width: 250px; color: inherit;'>",
                "<strong>", .("AUC (area under the ROC curve)"), ":</strong><br>",
                # same edges as .interpretAUC() and the AUC table note (lower edge included)
                "\u2022 ", .("Below 0.50 = Below chance (check Positive class, then Direction)"), "<br>",
                "\u2022 ", .("0.50 to below 0.60 = Minimal"), "<br>",
                "\u2022 ", .("0.60 to below 0.70 = Poor"), "<br>",
                "\u2022 ", .("0.70 to below 0.80 = Fair"), "<br>",
                "\u2022 ", .("0.80 to below 0.90 = Good"), "<br>",
                "\u2022 ", .("0.90 or more = Excellent"), "<br>",
                private$.aucBandsCaveat(),
                "</div>",
                "<div style='background-color: rgba(33, 163, 188, 0.21); padding: 10px; border-radius: 4px; flex: 1; min-width: 250px; color: inherit;'>",
                "<strong>", .("Youden index"), ":</strong><br>",
                .("J = sensitivity + specificity - 1. The cutoff that maximises it weighs sensitivity and specificity equally; at prevalence p that counts one missed case as (1 - p)/p false alarms, the same cost only when half the patients have the condition. That is a convention, not a clinical optimum."),
                "</div>",
                "</div>",
                "<details style='margin: 15px 0; padding: 10px; background-color: rgba(138, 155, 172, 0.06); border-radius: 4px; color: inherit;'>",
                "<summary style='cursor: pointer; font-weight: bold; color: inherit;'>", .("Statistical Terms Glossary"), "</summary>",
                "<div style='margin-top: 10px; padding-left: 20px;'>",
                "<p><strong>", .("Sensitivity (true positive rate)"), ":</strong> ", .("The proportion of patients with the condition whom the test calls positive."), "</p>",
                "<p><strong>", .("Specificity (true negative rate)"), ":</strong> ", .("The proportion of patients without the condition whom the test calls negative."), "</p>",
                "<p><strong>", .("PPV (positive predictive value)"), ":</strong> ", .("When the test is positive, the probability that the patient has the condition."), "</p>",
                # F-42: the negative class can pool other diagnoses; those patients are not 'healthy'.
                "<p><strong>", .("NPV (negative predictive value)"), ":</strong> ", .("When the test is negative, the probability that the patient does not have the condition named as the positive class"), "</p>",
                "<p><strong>", .("LR+ (positive likelihood ratio)"), ":</strong> ", .("How many times a positive result multiplies the odds of the condition."), "</p>",
                "<p><strong>", .("LR- (negative likelihood ratio)"), ":</strong> ", .("How many times a negative result multiplies the odds of the condition (a value below 1 lowers them)."), "</p>",
                "</div>",
                "</details>",
                "<div style='background-color: rgba(33, 162, 64, 0.19); padding: 10px; border: 1px solid #c3e6cb; border-radius: 4px; margin: 10px 0; color: inherit;'>",
                "<p style='margin: 0;'><strong>", .("Tip"), ":</strong> ", .("For clinical decisions, weigh the clinical relevance of a cutoff as well as its statistical significance."), "</p>",
                "</div>",
                "</div>"
            )
            return(instructions)
        },

        # Plotting functions
        .plotROCCurve = function(image, ggtheme, theme, ...) {
            # Seeded pointwise bands were computed with the analysis and saved as
            # coordinates. Resizing or exporting never resamples or refits.
            if (!private$.restoreFromState(image)) return(private$.plotStateUnavailable(image, ggtheme))
            # Check if ROC curve plotting is enabled
            if (!self$options$rocCurve) {
                return(FALSE)
            }

            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Plot size is applied with setSize() in .init(). setState() here only stored
            # render-time payload and never reached jamovi's image sizing, so plotWidth /
            # plotHeight had no effect on the output at all.

            tryCatch(
                {
                    # Create ROC curve plot
                    if (!requireNamespace("ggplot2", quietly = TRUE)) {
                        return(FALSE)
                    }

                    # Prepare data for plotting
                    plot_data <- data.frame()

                    for (predictor in names(private$.rocResults)) {
                        roc_obj <- private$.rocResults[[predictor]]$roc

                        # Validate ROC object
                        if (is.null(roc_obj) || !length(roc_obj$sensitivities)) {
                            next
                        }

                        # Draw the smoothed curve when one was produced. A "smooth.roc" does not
                        # inherit from "roc", so it has to be picked up after the guard above.
                        curve <- private$.rocSmoothed[[predictor]] %||% roc_obj

                        # Extract ROC curve coordinates
                        coords <- data.frame(
                            FPR = 1 - curve$specificities,
                            TPR = curve$sensitivities,
                            Predictor = predictor
                        )
                        plot_data <- rbind(plot_data, coords)
                    }

                    # Check if we have valid plot data
                    if (nrow(plot_data) == 0) {
                        return(FALSE)
                    }
                    # Path order (VAL-02): geom_line() sorts by FPR only and keeps the input order
                    # inside a vertical run, where pROC lists TPR descending - a sawtooth. Sorting
                    # by (FPR, TPR) first draws the staircase the tables describe.
                    plot_data <- plot_data[order(plot_data$Predictor, plot_data$FPR, plot_data$TPR), ]

                    # Curves and confidence ribbons share jamovi's selected palette.
                    n_colors <- length(unique(plot_data$Predictor))
                    cb_colors <- tryCatch(
                        jmvcore::colorPalette(n_colors, theme$palette, "color"),
                        error = function(e) private$.getColorblindSafePalette(n_colors))

                    # Create the plot
                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = FPR, y = TPR, color = Predictor)) +
                        ggplot2::geom_line(linewidth = 1.2) +
                        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                        ggplot2::labs(
                            x = .("False Positive Rate (1 - Specificity)"),
                            y = .("True Positive Rate (Sensitivity)"),
                            title = .("ROC Curve Analysis"),
                            color = .("Predictor")
                        ) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggplot2::scale_color_manual(values = cb_colors) +
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            legend.position = "bottom"
                        )

                    # Pointwise bootstrap intervals for sensitivity at nine fixed specificities, not a
                    # simultaneous band: ONE ci.coords() call per predictor (all nine from the same
                    # resamples) with the user's Bootstrap samples rather than pROC's default 2000.
                    if (self$options$showConfidenceBands) {
                        # A marker whose band failed loses its band, not the whole plot (it used to
                        # stop() into the error panel, so one failed band hid every ROC curve).
                        band_failed <- Filter(function(predictor) !is.null(private$.rocResults[[predictor]]$band_error),
                                              names(private$.rocResults))
                        band_list <- lapply(setdiff(names(private$.rocResults), band_failed), function(predictor) {
                            band <- private$.rocResults[[predictor]]$bands
                            if (is.null(band)) return(NULL)
                            band$Predictor <- predictor
                            band
                        })
                        captions <- if (length(band_failed)) .fmt(.("Confidence band not drawn for {preds}: {why}"),
                            preds = paste(band_failed, collapse = ", "),
                            why = paste(unique(vapply(band_failed, function(predictor) private$.rocResults[[predictor]]$band_error, character(1))), collapse = "; "))
                        ci_data <- do.call(rbind, band_list)
                        if (!is.null(ci_data)) ci_data <- ci_data[stats::complete.cases(ci_data), , drop = FALSE]
                        if (!is.null(ci_data) && nrow(ci_data) > 0) {
                            p <- p + ggplot2::geom_ribbon(
                                data = ci_data,
                                ggplot2::aes(x = FPR, ymin = TPR_lower, ymax = TPR_upper, fill = Predictor),
                                alpha = 0.2, show.legend = FALSE, inherit.aes = FALSE
                            ) +
                                # each band in its curve's colour (the colour scale maps sorted names)
                                ggplot2::scale_fill_manual(values = stats::setNames(cb_colors, sort(unique(plot_data$Predictor))))
                            captions <- c(.fmt(.("Pointwise {level}% bootstrap intervals for sensitivity at specificity 0.1-0.9. Random seed: {seed}"), level = self$options$confidenceLevel, seed = self$options$seed), captions)
                        }
                        if (length(captions)) p <- p + ggplot2::labs(caption = paste(captions, collapse = "\n"))
                    }

                    # Add optimal cutoff points if requested
                    if (self$options$showCutoffPoints) {
                        cutoff_data <- data.frame()
                        for (predictor in names(private$.rocResults)) {
                            if (isTRUE(private$.rocResults[[predictor]]$constant)) next   # no cut-point (constant marker)
                            optimal <- private$.rocResults[[predictor]]$optimal_cutoff
                            cutoff_data <- rbind(cutoff_data, data.frame(
                                FPR = 1 - optimal$specificity,
                                TPR = optimal$sensitivity,
                                Predictor = predictor
                            ))
                        }
                        if (nrow(cutoff_data) > 0) p <- p + ggplot2::geom_point(data = cutoff_data, size = 3, shape = 16)
                    }


                    print(p)
                    TRUE
                },
                error = function(e) {
                    # .checkpoint() signals a restart by stop()ping with a condition carrying
                    # code == "restart" (jmvcore::createError("restarting", "restart")). It is
                    # called inside the confidence-band loop above, and .checkpointCB is still
                    # live while .createImage() runs, so this handler sees it -- and would turn
                    # "the user changed an option, abandon this render" into a permanent
                    # "ROC Curve Plot Error ... restarting" panel. Re-raise it untouched.
                    if (identical(e$code, "restart")) stop(e)
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("ROC Curve Plot Error"),
                        sprintf(.("Failed to create the ROC curve plot: %s"), conditionMessage(e))
                    )
                }
            )
        },
        .plotComparativeROC = function(image, ggtheme, theme, ...) {
            if (!private$.restoreFromState(image)) return(private$.plotStateUnavailable(image, ggtheme))
            if (is.null(private$.rocResults) || length(private$.rocResults) < 2) {
                return(FALSE)
            }

            # Plot size is applied with setSize() in .init(). setState() here only stored
            # render-time payload and never reached jamovi's image sizing, so plotWidth /
            # plotHeight had no effect on the output at all.

            tryCatch(
                {
                    if (!requireNamespace("ggplot2", quietly = TRUE)) {
                        return(FALSE)
                    }

                    # Prepare comparative data for plotting
                    plot_data <- data.frame()
                    auc_data <- data.frame()

                    for (predictor in names(private$.rocResults)) {
                        roc_obj <- private$.rocResults[[predictor]]$roc

                        # Draw the smoothed curve when one was produced; the AUC below still comes
                        # from the empirical object.
                        curve <- private$.rocSmoothed[[predictor]] %||% roc_obj

                        # Use the ROC object's built-in coordinates instead of coords()
                        predictor_data <- data.frame(
                            FPR = 1 - curve$specificities,
                            TPR = curve$sensitivities,
                            Predictor = predictor
                        )

                        plot_data <- rbind(plot_data, predictor_data)

                        # Store AUC for legend
                        auc_data <- rbind(auc_data, data.frame(
                            Predictor = predictor,
                            AUC = round(as.numeric(roc_obj$auc), 3),
                            raw = as.numeric(roc_obj$auc)
                        ))
                    }

                    # Create legend labels with AUC values
                    auc_data$Legend <- paste0(auc_data$Predictor, " (AUC = ", auc_data$AUC, ")")
                    plot_data <- merge(plot_data, auc_data[c("Predictor", "Legend")], by = "Predictor")
                    # Path order after merge() (which reorders rows): see .plotROCCurve (VAL-02).
                    plot_data <- plot_data[order(plot_data$Legend, plot_data$FPR, plot_data$TPR), ]

                    # Create comparative ROC plot
                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = FPR, y = TPR, color = Legend)) +
                        ggplot2::geom_line(linewidth = 1.2, alpha = 0.8) +
                        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", alpha = 0.7) +
                        ggplot2::coord_equal() +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggplot2::labs(
                            x = .("1 - Specificity (False Positive Rate)"),
                            y = .("Sensitivity (True Positive Rate)"),
                            title = .("Comparative ROC Analysis"),
                            subtitle = .("ROC curves with AUC values for comparison"),
                            color = .("Predictor (AUC)")
                        ) +
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11),
                            legend.position = "right",
                            legend.title = ggplot2::element_text(size = 10, face = "bold"),
                            axis.text = ggplot2::element_text(size = 10),
                            axis.title = ggplot2::element_text(size = 11)
                        )

                    # Add optimal cutoff points if enabled
                    if (self$options$showCutoffPoints) {
                        optimal_points <- data.frame()
                        for (predictor in names(private$.rocResults)) {
                            optimal <- private$.rocResults[[predictor]]$optimal_cutoff
                            if (!is.null(optimal) && !isTRUE(private$.rocResults[[predictor]]$constant)) {
                                optimal_points <- rbind(optimal_points, data.frame(
                                    FPR = 1 - optimal$specificity,
                                    TPR = optimal$sensitivity,
                                    Predictor = predictor,
                                    Legend = paste0(predictor, " (AUC = ", round(as.numeric(private$.rocResults[[predictor]]$roc$auc), 3), ")")
                                ))
                            }
                        }

                        if (nrow(optimal_points) > 0) {
                            p <- p + ggplot2::geom_point(
                                data = optimal_points,
                                ggplot2::aes(x = FPR, y = TPR, color = Legend),
                                size = 3, shape = 16, alpha = 0.8, inherit.aes = FALSE
                            )
                        }
                    }

                    # Add comparison annotations
                    if (length(auc_data$AUC) >= 2) {
                        # Find best and worst AUC. F-27: from the unrounded AUCs, as the comparison
                        # tables do; the rounded ones gave +0.145 where the tables print 0.146.
                        best_idx <- which.max(auc_data$raw)
                        worst_idx <- which.min(auc_data$raw)

                        if (best_idx != worst_idx) {
                            auc_diff <- auc_data$raw[best_idx] - auc_data$raw[worst_idx]
                            comparison_text <- paste(
                                .fmt(.("Best: {pred} (AUC = {auc})"), pred = auc_data$Predictor[best_idx], auc = auc_data$AUC[best_idx]),
                                .fmt(.("Difference: +{diff}"), diff = round(auc_diff, 3)), sep = "\n")

                            p <- p + ggplot2::annotate("text",
                                x = 0.7, y = 0.2, label = comparison_text,
                                hjust = 0, vjust = 0, size = 3.5,
                                colour = "gray50", fontface = "italic"   # reads on light and dark themes
                            )
                        }
                    }


                    print(p)
                    TRUE
                },
                error = function(e) {
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("Comparative ROC Plot Error"),
                        sprintf(.("Failed to create the comparative ROC plot: %s"), conditionMessage(e))
                    )
                }
            )
        },
        .plotCutoffAnalysis = function(image, ggtheme, theme, ...) {
            if (!private$.restoreFromState(image)) return(private$.plotStateUnavailable(image, ggtheme))
            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Plot size is applied with setSize() in .init(). setState() here only stored
            # render-time payload and never reached jamovi's image sizing, so plotWidth /
            # plotHeight had no effect on the output at all.

            tryCatch(
                {
                    # ggplot2 available via package Imports

                    # Create cutoff analysis plot showing sensitivity/specificity vs threshold
                    plot_data <- data.frame()

                    for (predictor in names(private$.rocResults)) {
                        roc_obj <- private$.rocResults[[predictor]]$roc
                        coords <- data.frame(threshold = roc_obj$thresholds, sensitivity = roc_obj$sensitivities,
                                             specificity = roc_obj$specificities)

                        # Prepare data for long format
                        threshold_data <- data.frame(
                            Threshold = coords$threshold,
                            Sensitivity = coords$sensitivity,
                            Specificity = coords$specificity,
                            Predictor = predictor
                        )

                        # Convert to long format for plotting (base R, no reshape2 dependency)
                        sens_df <- data.frame(Threshold = threshold_data$Threshold, Predictor = threshold_data$Predictor, Metric = "Sensitivity", Value = threshold_data$Sensitivity, stringsAsFactors = FALSE)
                        spec_df <- data.frame(Threshold = threshold_data$Threshold, Predictor = threshold_data$Predictor, Metric = "Specificity", Value = threshold_data$Specificity, stringsAsFactors = FALSE)
                        threshold_long <- rbind(sens_df, spec_df)

                        plot_data <- rbind(plot_data, threshold_long)
                    }

                    # Create the plot
                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Threshold, y = Value, color = Metric)) +
                        ggplot2::geom_line(linewidth = 1) +
                        ggplot2::labs(
                            x = .("Threshold"),
                            y = .("Performance Metric"),
                            title = .("Sensitivity and Specificity vs Threshold"),
                            color = .("Metric")
                        ) +
                        ggplot2::ylim(0, 1) +
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            legend.position = "bottom"
                        )

                    if (length(names(private$.rocResults)) > 1) {
                        p <- p + ggplot2::facet_wrap(~Predictor, scales = "free_x")
                    }

                    print(p)
                    TRUE
                },
                error = function(e) {
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("Cutoff Analysis Plot Error"),
                        sprintf(.("Failed to create the cutoff analysis plot: %s"), conditionMessage(e))
                    )
                }
            )
        },
        .plotYoudenIndex = function(image, ggtheme, theme, ...) {
            if (!private$.restoreFromState(image)) return(private$.plotStateUnavailable(image, ggtheme))
            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Plot size is applied with setSize() in .init(). setState() here only stored
            # render-time payload and never reached jamovi's image sizing, so plotWidth /
            # plotHeight had no effect on the output at all.

            tryCatch(
                {
                    # ggplot2 available via package Imports

                    # Create Youden Index plot
                    plot_data <- data.frame()

                    for (predictor in names(private$.rocResults)) {
                        roc_obj <- private$.rocResults[[predictor]]$roc
                        coords <- data.frame(threshold = roc_obj$thresholds, sensitivity = roc_obj$sensitivities,
                                             specificity = roc_obj$specificities)
                        youden <- coords$sensitivity + coords$specificity - 1

                        youden_data <- data.frame(
                            Threshold = coords$threshold,
                            YoudenIndex = youden,
                            Predictor = predictor
                        )

                        plot_data <- rbind(plot_data, youden_data)
                    }

                    # Create the plot
                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Threshold, y = YoudenIndex, color = Predictor)) +
                        ggplot2::geom_line(linewidth = 1.2) +
                        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                        ggplot2::labs(
                            x = .("Threshold"),
                            y = .("Youden Index (Sensitivity + Specificity - 1)"),
                            title = .("Youden Index vs Threshold"),
                            color = .("Predictor")
                        ) +
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            legend.position = "bottom"
                        )

                    # Mark optimal points
                    optimal_data <- data.frame()
                    for (predictor in names(private$.rocResults)) {
                        if (isTRUE(private$.rocResults[[predictor]]$constant)) next   # no cut-point (constant marker)
                        optimal <- private$.rocResults[[predictor]]$optimal_cutoff
                        optimal_data <- rbind(optimal_data, data.frame(
                            Threshold = optimal$cutoff,
                            YoudenIndex = optimal$youden_index,
                            Predictor = predictor
                        ))
                    }
                    if (nrow(optimal_data) > 0) p <- p + ggplot2::geom_point(data = optimal_data, size = 3, shape = 16)

                    print(p)
                    TRUE
                },
                error = function(e) {
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("Youden Index Plot Error"),
                        sprintf(.("Failed to create the Youden Index plot: %s"), conditionMessage(e))
                    )
                }
            )
        },
        .plotClinicalDecision = function(image, ggtheme, theme, ...) {
            if (!private$.restoreFromState(image)) return(private$.plotStateUnavailable(image, ggtheme))
            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Plot size is applied with setSize() in .init(). setState() here only stored
            # render-time payload and never reached jamovi's image sizing, so plotWidth /
            # plotHeight had no effect on the output at all.

            tryCatch(
                {
                    # ggplot2 available via package Imports

                    # Create clinical decision plot showing PPV/NPV vs prevalence
                    plot_data <- data.frame()
                    prevalence_range <- sort(unique(c(10^seq(-9, -2, length.out = 100),
                        seq(.01, .99, length.out = 100), self$options$prevalence)))

                    for (predictor in names(private$.rocResults)) {
                        if (isTRUE(private$.rocResults[[predictor]]$constant)) next   # no cut-point (constant marker)
                        optimal <- private$.rocResults[[predictor]]$optimal_cutoff
                        sens <- optimal$sensitivity
                        spec <- optimal$specificity

                        # Calculate PPV and NPV across prevalence range
                        ppv_vals <- (sens * prevalence_range) /
                            (sens * prevalence_range + (1 - spec) * (1 - prevalence_range))
                        npv_vals <- (spec * (1 - prevalence_range)) /
                            (spec * (1 - prevalence_range) + (1 - sens) * prevalence_range)

                        ppv_data <- data.frame(
                            Prevalence = prevalence_range,
                            Value = ppv_vals,
                            Metric = "PPV",
                            Predictor = predictor
                        )

                        npv_data <- data.frame(
                            Prevalence = prevalence_range,
                            Value = npv_vals,
                            Metric = "NPV",
                            Predictor = predictor
                        )

                        plot_data <- rbind(plot_data, ppv_data, npv_data)
                    }

                    # Create the plot
                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Prevalence, y = Value, color = Metric, linetype = Predictor)) +
                        ggplot2::geom_line(linewidth = 1) +
                        ggplot2::labs(
                            x = .("Disease Prevalence"),
                            y = .("Predictive Value"),
                            title = .("Predictive Values vs Disease Prevalence"),
                            color = .("Metric"),
                            linetype = .("Predictor")
                        ) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            legend.position = "bottom"
                        )

                    # Mark the prevalence the clinical tables actually use, not the raw option -
                    # otherwise the reference line contradicts them whenever "use observed
                    # prevalence" is ticked.
                    ref_prev <- tryCatch(
                        image$state$prevalence,
                        error = function(e) NULL
                    )
                    if (!is.null(ref_prev) && !is.na(ref_prev)) {
                        p <- p + ggplot2::geom_vline(
                            xintercept = ref_prev,
                            linetype = "dashed", color = "red", alpha = 0.7
                        )
                    }

                    print(p)
                    TRUE
                },
                error = function(e) {
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("Clinical Decision Plot Error"),
                        sprintf(.("Failed to create the clinical decision plot: %s"), conditionMessage(e))
                    )
                }
            )
        },
        .plotPRC = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (!isTRUE(self$options$detectImbalance)) return(FALSE)
            if (is.null(st) || !identical(st$schema, 2L) || is.null(st[["pr"]])) {
                return(private$.plotStateUnavailable(image, ggtheme))
            }
            tryCatch(
                {
                    plot_data <- as.data.frame(st$pr$data)
                    baseline <- st$pr$baseline

                    if (nrow(plot_data) == 0) {
                        return(FALSE)
                    }

                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Recall, y = Precision, color = Predictor)) +
                        ggplot2::geom_step(direction = "vh", linewidth = 1.2) +
                        ggplot2::geom_hline(yintercept = baseline, linetype = "dashed", color = "gray50") +
                        ggplot2::labs(
                            x = .("Recall (Sensitivity)"),
                            y = .("Precision (PPV)"),
                            title = .("Precision-Recall Curve"),
                            subtitle = .fmt(.("Baseline (Prevalence) = {b}"), b = round(baseline, 3))
                        ) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            legend.position = "bottom"
                        )

                    print(p)
                    TRUE
                },
                error = function(e) {
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("Precision-Recall Plot Error"),
                        sprintf(.("Failed to create the Precision-Recall plot: %s"), conditionMessage(e))
                    )
                }
            )
        },
        .plotCROC = function(image, ggtheme, theme, ...) {
            if (!private$.restoreFromState(image)) return(private$.plotStateUnavailable(image, ggtheme))
            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Plot size is applied with setSize() in .init(). setState() here only stored
            # render-time payload and never reached jamovi's image sizing, so plotWidth /
            # plotHeight had no effect on the output at all.

            tryCatch(
                {
                    # ggplot2 available via package Imports

                    # Prepare CROC data for plotting
                    plot_data <- data.frame()
                    alpha <- self$options$crocAlpha

                    for (predictor in names(private$.rocResults)) {
                        roc_obj <- private$.rocObjects[[predictor]]

                        # The transformed exact path, already in (FPR, TPR) order (.calculateCROC)
                        croc_result <- private$.calculateCROC(roc_obj, alpha)
                        croc_df <- data.frame(
                            CROC_FPR = croc_result$croc_fpr,
                            TPR = croc_result$croc_tpr,
                            Predictor = predictor,
                            Type = "CROC"
                        )

                        # Also add original ROC for comparison, in path order (VAL-02)
                        roc_df <- data.frame(
                            CROC_FPR = (1 - roc_obj$specificities),
                            TPR = roc_obj$sensitivities,
                            Predictor = predictor,
                            Type = "ROC"
                        )
                        roc_df <- roc_df[order(roc_df$CROC_FPR, roc_df$TPR), ]

                        plot_data <- rbind(plot_data, croc_df, roc_df)
                    }

                    # A random ranking is y = x for the ROC overlay but (f(s), s) in CROC coordinates,
                    # which lies below y = x everywhere: against the diagonal alone a CROC curve well
                    # above random was drawn mostly under "chance". Its area is the table's
                    # random-ranking value. Added as the LAST layer (layer 1 stays the curves).
                    s <- seq(0, 1, length.out = 201)
                    random_df <- data.frame(CROC_FPR = (1 - exp(-alpha * s)) / (1 - exp(-alpha)), TPR = s)
                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = CROC_FPR, y = TPR, color = Predictor, linetype = Type)) +
                        ggplot2::geom_line(linewidth = 1) +
                        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                        ggplot2::geom_line(data = random_df, ggplot2::aes(x = CROC_FPR, y = TPR), inherit.aes = FALSE,
                                           linetype = "dotted", color = "gray50", linewidth = 0.8) +
                        ggplot2::labs(
                            x = .fmt(.("False positive rate (ROC curve); f(FPR) with \u03b1={a} (CROC curve)"), a = alpha),
                            y = .("True Positive Rate (Sensitivity)"),
                            title = .("CROC (Concentrated ROC) Curve Analysis"),
                            subtitle = .("Exponential magnifier emphasizes early retrieval performance"),
                            # wrapped: on the fixed-aspect plot the one-line caption was clipped (2026-10-21)
                            caption = paste(strwrap(.fmt(.("Random ranking: dashed diagonal for the ROC curve, dotted curve for the CROC curve (CROC area {r})."),
                                           r = sprintf("%.3f", 1 / alpha - 1 / (exp(alpha) - 1))), width = 80), collapse = "\n"),
                            color = .("Predictor"),
                            linetype = .("Curve Type")
                        ) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggplot2::coord_fixed() +
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
                            legend.position = "bottom"
                        )

                    print(p)
                    TRUE
                },
                error = function(e) {
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("CROC Plot Error"),
                        sprintf(.("Failed to create the CROC plot: %s"), conditionMessage(e))
                    )
                }
            )
        },
        .plotConvexHull = function(image, ggtheme, theme, ...) {
            if (!private$.restoreFromState(image)) return(private$.plotStateUnavailable(image, ggtheme))
            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Plot size is applied with setSize() in .init(). setState() here only stored
            # render-time payload and never reached jamovi's image sizing, so plotWidth /
            # plotHeight had no effect on the output at all.

            tryCatch(
                {
                    # ggplot2 available via package Imports

                    # Prepare convex hull data for plotting
                    plot_data <- data.frame()
                    hull_data <- data.frame()

                    for (predictor in names(private$.rocResults)) {
                        roc_obj <- private$.rocObjects[[predictor]]

                        # Calculate convex hull
                        hull_result <- private$.calculateConvexHull(roc_obj)

                        # Original ROC curve, in path order (VAL-02)
                        roc_df <- data.frame(
                            FPR = 1 - roc_obj$specificities,
                            TPR = roc_obj$sensitivities,
                            Predictor = predictor,
                            Type = "Empirical ROC"
                        )
                        roc_df <- roc_df[order(roc_df$FPR, roc_df$TPR), ]

                        # Convex hull vertices from (0,0) to (1,1); the polygon closes along the diagonal
                        hull_df <- data.frame(
                            FPR = hull_result$hull_fpr,
                            TPR = hull_result$hull_tpr,
                            Predictor = predictor,
                            Type = "Convex Hull"
                        )

                        plot_data <- rbind(plot_data, roc_df)
                        hull_data <- rbind(hull_data, hull_df)
                    }

                    # Create the plot
                    p <- ggplot2::ggplot() +
                        ggplot2::geom_line(
                            data = plot_data, ggplot2::aes(x = FPR, y = TPR, color = Predictor),
                            linewidth = 1, alpha = 0.7
                        ) +
                        ggplot2::geom_polygon(
                            data = hull_data, ggplot2::aes(x = FPR, y = TPR, fill = Predictor),
                            alpha = 0.2
                        ) +
                        ggplot2::geom_line(
                            data = hull_data, ggplot2::aes(x = FPR, y = TPR, color = Predictor),
                            linewidth = 1.2, linetype = "dashed"
                        ) +
                        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                        ggplot2::labs(
                            x = .("False Positive Rate (1 - Specificity)"),
                            y = .("True Positive Rate (Sensitivity)"),
                            title = .("ROC Curve with Convex Hull"),
                            # Not "optimal achievable performance": the hull marks potentially optimal
                            # points, its edges need random mixing, and it is in-sample.
                            subtitle = paste(strwrap(.("Convex hull: operating points reachable by randomly mixing two cut-offs (Provost & Fawcett 2001); in-sample, so optimistic"), width = 80), collapse = "\n"),
                            color = .("Predictor"),
                            fill = .("Predictor")
                        ) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggplot2::coord_fixed() +
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
                            legend.position = "bottom"
                        )

                    print(p)
                    TRUE
                },
                error = function(e) {
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("Convex Hull Plot Error"),
                        sprintf(.("Failed to create the convex hull plot: %s"), conditionMessage(e))
                    )
                }
            )
        },
        # Flexible calibration curve, summarised by the ICI / E50 / E90 / Emax of Austin & Steyerberg
        # (2019), who fitted loess or restricted cubic splines on the probability scale; here a
        # logistic regression of the outcome on a natural cubic spline of logit(p) with `knots`
        # knots (knots - 1 df).
        # Returns the smoothed observed probability at every observation, a plotting grid,
        # and the calibration-error summaries: ICI = mean |smoothed - p|, E50/E90 = its
        # median / 90th percentile, Emax = its maximum. NULL when the curve cannot be fitted
        # (too few events, a degenerate predictor).
        .splineCalibrationCurve = function(probs, y_binary, knots) {
            ok <- is.finite(probs) & !is.na(y_binary)
            p <- probs[ok]; y <- y_binary[ok]
            if (length(p) < 20 || sum(y) < knots || sum(1 - y) < knots) return(NULL)
            lp <- stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
            if (length(unique(lp)) <= knots) return(NULL)
            fit <- tryCatch(
                suppressWarnings(stats::glm(y ~ splines::ns(lp, df = knots - 1), family = stats::binomial)),
                error = function(e) NULL
            )
            if (is.null(fit) || !isTRUE(fit$converged)) return(NULL)
            smoothed <- as.numeric(stats::fitted(fit))
            grid_lp <- seq(min(lp), max(lp), length.out = 100)
            curve <- as.numeric(stats::predict(fit, newdata = data.frame(lp = grid_lp), type = "response"))
            abs_err <- abs(smoothed - p)
            list(
                grid_p = stats::plogis(grid_lp), curve = curve,
                ici = mean(abs_err), e50 = unname(stats::median(abs_err)),
                e90 = unname(stats::quantile(abs_err, 0.9)), emax = max(abs_err),
                n_events = sum(y), knots = knots
            )
        },

        .populateCalibrationAnalysis = function() {
            if (!self$options$calibrationAnalysis) {
                return()
            }

            calTable <- self$results$results$calibrationSummary
            .deleteTableRows(calTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates
            # Every measure has its own checkbox, all off by default: ticking Calibration analysis
            # alone gave a table with a predictor name and nothing else, and no word why.
            measures <- c("brierScore", "calibrationMetrics", "hosmerLemeshow", "eoRatio", "splineCalibration", "calibrationPlot")
            if (!any(vapply(measures, function(m) isTRUE(self$options[[m]]), logical(1)))) {
                private$.addNotice(type = "INFO", title = .("No Calibration Measure Selected"),
                    content = .("Calibration analysis is on, but no measure is ticked, so the calibration table has no values. Tick one or more of Brier score, Calibration metrics, Hosmer-Lemeshow test, E/O ratio, Spline calibration curve or Calibration plot."))
            }
            calTable$setNote(
                "eo",
                if (isTRUE(self$options$eoRatio)) .("E/O is the sum of the predicted probabilities divided by the number of events actually observed; 1 means the marker predicts the right number of events overall, above 1 means it over-predicts them. It is reported only when Prediction scale is Probability. With Score the risks come from a logistic model fitted to these data, which forces the predicted and observed event totals to be equal, so E/O would be exactly 1 for any data at all; the cell is left blank there rather than report a perfect calibration that is an algebraic identity. No confidence interval is given."))
            hlTable <- self$results$results$hosmerLemeshowTable
            .deleteTableRows(hlTable)   # jamovi re-runs .run() on the same object; addRow() would stack duplicates
            supplied <- identical(self$options$predictionScale, "probability")
            n_groups <- self$options$hlGroups
            excluded <- character(0)    # Probability scale, a value outside 0-1: no outputs at all
            dropped <- character(0)     # Hosmer-Lemeshow groups left out (see below)
            bins <- list(); spline_df <- list(); dens_df <- list(); spline_failed <- character(0)

            for (predictor in names(private$.rocResults)) {
                tryCatch(
                    {
                        # Get data
                        data <- private$.analysisData
                        outcome <- data[[private$.outcome]]
                        pred_vals <- data[[predictor]]

                        # Ensure outcome is binary 0/1
                        y_binary <- as.numeric(outcome == levels(outcome)[2])

                        # Risks as Prediction scale declares them; .riskNotices() has said why a
                        # predictor gets none.
                        risk <- private$.riskProbabilities(pred_vals, y_binary)
                        if (is.null(risk$probs)) {
                            excluded <- c(excluded, predictor)
                            next
                        }
                        probs <- risk$probs

                        # Brier Score (only compute if brierScore option is enabled)
                        brier <- NA
                        scaled_brier <- NA
                        if (isTRUE(self$options$brierScore)) {
                            brier <- mean((probs - y_binary)^2)
                            # Scaled Brier Score (1 - Brier / Brier_max)
                            prev <- mean(y_binary)
                            brier_max <- prev * (1 - prev)^2 + (1 - prev) * prev^2
                            scaled_brier <- if (brier_max > 1e-10) 1 - (brier / brier_max) else NA_real_
                        }

                        # Calibration Slope, Intercept, and Calibration-in-the-large
                        slope <- NA
                        intercept <- NA
                        cal_in_large <- NA
                        interpretation <- ""
                        slope_note <- NULL
                        if (isTRUE(self$options$calibrationMetrics)) {
                            # Calibration slope and intercept are only meaningful when the
                            # probabilities came from SOMEWHERE ELSE. When we derived them
                            # ourselves by fitting glm(y ~ pred_vals) a moment ago, regressing y on
                            # qlogis(fitted) is an algebraic re-parameterisation of that same fit:
                            # the MLE returns slope = 1 and intercept = 0 for any data whatsoever.
                            # Printing those and grading them "Good calibration slope" told every
                            # user their marker was perfectly calibrated, on no evidence at all.
                            # Report them only on the branch where the user supplied the risks.
                            if (!isTRUE(risk$assumed_probability)) {
                                # Was an untranslated English literal in a user-visible cell.
                                interpretation <- .("Not estimable - a model was fitted to these data, so the slope and intercept are 1 and 0 by construction")
                            } else {
                                logit_probs <- qlogis(probs)
                                # Handle infinite logits if probs are 0 or 1
                                n_clamped <- sum(probs == 0 | probs == 1, na.rm = TRUE)
                                logit_probs[probs == 0] <- -10
                                logit_probs[probs == 1] <- 10
                                # +-10 is an arbitrary numerical convenience, and it is a very
                                # high-leverage point: it moves the reported slope and intercept.
                                # Exact 0.00 / 1.00 values are common when risks are rounded to
                                # two decimals, so say when it happened instead of letting the
                                # Interpretation cell grade an artefact of the clamp.
                                if (n_clamped > 0) {
                                    private$.addNotice(
                                        type = "WARNING",
                                        title = sprintf(.("Extreme Predicted Risks Clamped: %s"), predictor),
                                        content = .fmt(.("{n} of the predicted risks for {pred} are exactly 0 or exactly 1, whose log-odds are infinite. They were set to log-odds of -10 and +10 so the calibration regression could run. Those are arbitrary values sitting far from the rest of the data, so they pull the calibration slope and intercept - and the Interpretation cell beside them - towards themselves. Check whether these values are genuine certainties or rounded-off probabilities."), n = n_clamped, pred = predictor)
                                    )
                                }

                                cal_model <- suppressWarnings(glm(y_binary ~ logit_probs, family = binomial))
                                intercept <- coef(cal_model)[1]
                                slope <- coef(cal_model)[2]

                                # Calibration-in-the-large (intercept when slope is fixed to 1)
                                cal_large_model <- glm(y_binary ~ offset(logit_probs), family = binomial)
                                cal_in_large <- coef(cal_large_model)[1]

                                # A slope only "suggests" over- or under-fitting (Van Calster et al.
                                # 2019); 0.9-1.1 is a heuristic band with no source (VAL-25f). With one
                                # regressor the slope MLE is infinite exactly when the outcomes do not
                                # overlap on the logits that were fitted - complete or quasi-complete
                                # separation (Albert & Anderson 1984). glm can still report convergence
                                # at a finite slope there (0/1 risks clamped to +-10, two risk levels),
                                # and glm.fit's 10*eps fitted-value check is a numerical warning, not a
                                # separation test, so test the data. logit_probs, not probs: the clamp
                                # is not order-preserving (0 -> -10, but 1e-6 -> -13.8).
                                lp0 <- logit_probs[y_binary == 0]
                                lp1 <- logit_probs[y_binary == 1]
                                separated <- max(lp0, na.rm = TRUE) <= min(lp1, na.rm = TRUE) ||
                                             max(lp1, na.rm = TRUE) <= min(lp0, na.rm = TRUE)
                                s <- round(slope, 3)
                                interpretation <- if (!isTRUE(cal_model$converged) || !is.finite(slope) || separated) {
                                    # the slope and intercept cells are blanked after addRow()
                                    slope_note <- .("Not estimable: the recalibration fit did not converge or the predicted risks separate the outcomes, so no finite slope exists.")
                                    .("No reading: the recalibration fit did not converge or the predicted risks separate the outcomes")
                                } else if (s <= 0) {
                                    .("Slope 0 or below: no or reversed association between predicted risk and outcome")
                                } else if (s >= 0.9 && s <= 1.1) {   # the rounded value against the same literals: [0.900, 1.100]
                                    .("Close to 1 (within 0.1, a heuristic band)")
                                } else if (s > 1) {
                                    private$.underfitCited <- TRUE   # read by .populateMethodsExplanation()
                                    .("Above 1: predictions too moderate (Van Calster et al. 2019), suggesting under-fitting (Austin & Steyerberg 2019)")
                                } else {
                                    .("Below 1: predictions too extreme, suggesting over-fitting (Van Calster et al. 2019)")
                                }
                            }
                        }

                        # Flexible (spline) calibration: ICI and its percentiles
                        ici <- e50 <- e90 <- emax <- NA_real_
                        sp <- NULL
                        if (isTRUE(self$options$splineCalibration)) {
                            sp <- private$.splineCalibrationCurve(probs, y_binary, self$options$splineKnots)
                            if (is.null(sp)) {
                                private$.addNotice(
                                    type = "WARNING",
                                    title = sprintf(.("Spline calibration not estimable: %s"), predictor),
                                    content = sprintf(.("The spline calibration curve for '%1$s' could not be fitted with %2$s knots: it needs at least %3$s events and %4$s non-events, more distinct predicted probabilities than knots, and a converging fit. Reduce the number of knots or check the predictor. ICI, E50, E90 and Emax are left blank."), predictor, self$options$splineKnots, self$options$splineKnots, self$options$splineKnots)
                                )
                            } else {
                                ici <- sp$ici; e50 <- sp$e50; e90 <- sp$e90; emax <- sp$emax
                                if (!isTRUE(risk$assumed_probability)) {
                                    private$.addNotice(
                                        type = "INFO",
                                        title = sprintf(.("Spline calibration: %s"), predictor),
                                        content = sprintf(.("Because the probabilities for '%s' were derived here by a linear logistic fit, the spline curve only tests whether that logit-linear form is adequate for this marker; it says nothing about the calibration of an external model."), predictor)
                                    )
                                }
                            }
                        }

                        # Expected/Observed ratio. Gated on the same condition as the calibration
                        # slope and intercept above, and for the same reason: a glm(y ~ pred_vals)
                        # fitted here has a binomial score equation for the intercept that forces
                        # sum(fitted) == sum(y), so E/O is identically 1.000 whatever the data.
                        # Printing that would tell every pathologist their marker is perfectly
                        # calibrated on no evidence. The cell is blanked; the table note says why.
                        eo <- NA_real_
                        if (isTRUE(self$options$eoRatio) && isTRUE(risk$assumed_probability)) {
                            # Sum over the SAME rows on both sides - dropping NAs independently
                            # would divide an expected count by an observed count from a
                            # different set of patients.
                            ok <- is.finite(probs) & is.finite(y_binary)
                            expected <- sum(probs[ok])
                            observed <- sum(y_binary[ok])
                            eo <- if (observed > 0) expected / observed else NA_real_
                        }

                        # Populate Calibration Summary Table
                        row <- list(
                            predictor = predictor,
                            brier_score = brier,
                            scaled_brier = scaled_brier,
                            calibration_slope = slope,
                            calibration_intercept = intercept,
                            calibration_in_large = cal_in_large,
                            ici = ici, e50 = e50, e90 = e90, emax = emax,
                            eo_ratio = eo,
                            interpretation = interpretation
                        )
                        key <- private$.escapeVar(predictor)
                        calTable$addRow(rowKey = key, values = row)
                        # NA prints "NaN" in jamovi; only "" is blank (amendment A1).
                        if (!isTRUE(self$options$brierScore)) {
                            private$.blankCells(calTable, key, c("brier_score", "scaled_brier"))
                        }
                        if (isTRUE(self$options$calibrationMetrics) && !isTRUE(risk$assumed_probability)) {
                            private$.blankCells(calTable, key, c("calibration_slope", "calibration_intercept", "calibration_in_large"),
                                                .("Not an estimate: risks from a model fitted to these data have slope 1, intercept 0 and calibration-in-the-large 0 by construction."))
                        }
                        if (!is.null(slope_note)) {
                            private$.blankCells(calTable, key, c("calibration_slope", "calibration_intercept"), slope_note)
                        }
                        if (isTRUE(self$options$eoRatio) && !is.finite(eo)) {
                            private$.blankCells(calTable, key, "eo_ratio",
                                                if (!isTRUE(risk$assumed_probability)) .("Not an estimate: E/O is exactly 1 for risks from a model fitted to these data."))
                        }
                        if (isTRUE(self$options$splineCalibration) && is.null(sp)) {
                            private$.blankCells(calTable, key, c("ici", "e50", "e90", "emax"), .("Not estimable; see the notice above the results."))
                        }

                        # Risk-quantile groups shared by the Hosmer-Lemeshow test and the plot, so
                        # the plotted points and the chi-square describe one grouping (hlGroups).
                        breaks <- unique(stats::quantile(probs, probs = seq(0, 1, length.out = n_groups + 1), na.rm = TRUE))
                        groups <- if (length(breaks) >= 2) cut(probs, breaks = breaks, include.lowest = TRUE, labels = FALSE)

                        # Plot-ready frames: the renderer draws from image state alone.
                        if (isTRUE(self$options$calibrationPlot)) {
                            if (!is.null(groups)) {
                                bins[[predictor]] <- data.frame(prob = as.numeric(tapply(probs, groups, mean)),
                                                                outcome = as.numeric(tapply(y_binary, groups, mean)),
                                                                Predictor = predictor, stringsAsFactors = FALSE)
                            }
                            if (isTRUE(self$options$splineCalibration)) {
                                if (is.null(sp)) {
                                    spline_failed <- c(spline_failed, predictor)
                                } else {
                                    spline_df[[predictor]] <- data.frame(prob = sp$grid_p, outcome = sp$curve,
                                                                         Predictor = predictor, stringsAsFactors = FALSE)
                                }
                            }
                            # Marginal density of the risks, as geom_density() would draw it (nrd0
                            # bandwidth over the data range), scaled into the bottom 15% of the panel.
                            if (isTRUE(self$options$calibrationDensity) && length(probs) >= 10) {
                                d <- tryCatch(stats::density(probs, n = 512, from = min(probs), to = max(probs)), error = function(e) NULL)
                                if (!is.null(d) && max(d$y) > 0) {
                                    dens_df[[predictor]] <- data.frame(prob = d$x, dens = 0.15 * d$y / max(d$y),
                                                                       Predictor = predictor, stringsAsFactors = FALSE)
                                }
                            }
                        }

                        # Hosmer-Lemeshow Test
                        if (self$options$hosmerLemeshow) {
                            if (length(breaks) < 3) {
                                # Too few distinct predicted risks to group; say which cause applies.
                                private$.addNotice(
                                    type = "WARNING",
                                    title = sprintf(.("HL Test Skipped: %s"), predictor),
                                    content = if (isTRUE(risk$constant)) {
                                        sprintf(.("The logistic slope of %s is not estimable because the marker is constant in the analysed rows, so every patient receives the same predicted risk (the sample prevalence) and there are no risk groups to compare."), predictor)
                                    } else {
                                        sprintf(.("The predicted risks of %1$s take only %2$s distinct values, too few for Hosmer-Lemeshow grouping, which needs at least 3 distinct quantile break points. This happens when the marker has very few distinct values or is heavily tied."), predictor, length(unique(probs)))
                                    }
                                )
                            } else {
                                actual_groups <- length(unique(groups))
                                obs <- tapply(y_binary, groups, sum)
                                exp <- tapply(probs, groups, sum)
                                n_cnt <- tapply(y_binary, groups, length)
                                denom <- exp * (1 - exp / n_cnt)
                                # A group whose predicted risks are all 0, or all 1, has no binomial
                                # variance. If its outcomes match exactly it carries no information:
                                # it is left out and df drops by one. If not, the prediction was
                                # contradicted and the statistic is infinite.
                                flat <- denom <= 1e-10
                                contradicted <- any(flat & abs(obs - exp) > 0.5)
                                # g - 2 df only when the two parameters of a logistic model were
                                # estimated on THESE data; supplied probabilities are tested on g df
                                # (external validation). g - 2 there makes p too small: at chi-square
                                # 16 with 10 groups, p = 0.042 on 8 df against 0.100 on 10 df.
                                df <- sum(!flat) - if (isTRUE(risk$assumed_probability)) 0L else 2L
                                hl_stat <- if (contradicted) NA_real_ else sum(((obs - exp)^2 / denom)[!flat])
                                p_val <- if (is.finite(hl_stat) && df > 0) stats::pchisq(hl_stat, df, lower.tail = FALSE) else NA_real_
                                if (any(flat) && !contradicted) {
                                    dropped <- c(dropped, sprintf(.("%1$s: risk groups whose predicted risks were all 0 or all 1 matched their outcomes exactly (%2$s of them); they carry no information, so they were left out and the degrees of freedom reduced by the same number."), .rocLabel(predictor), sum(flat)))
                                }

                                # p >= 0.05 is absence of evidence of misfit, not evidence of fit (VAL-09).
                                conclusion <- if (contradicted) {
                                    .("Evidence of lack of fit (a predicted 0 or 1 was contradicted)")
                                } else if (!is.na(p_val) && p_val < 0.05) {
                                    .("Evidence of lack of fit (p < 0.05)")
                                } else if (!is.na(p_val)) {
                                    .("No evidence of lack of fit (p >= 0.05)")
                                } else {
                                    .("Could not compute")
                                }

                                hlTable$addRow(rowKey = key, values = list(
                                    predictor = predictor,
                                    chi_square = hl_stat,
                                    df = df,
                                    p_value = p_val,
                                    n_groups = actual_groups,
                                    conclusion = conclusion
                                ))
                                if (contradicted) {
                                    private$.blankCells(hlTable, key, c("chi_square", "df", "p_value"),
                                                        .("Infinite: a group whose predicted risks are all 0 contains an event, or one whose predicted risks are all 1 contains a non-event."))
                                } else if (is.na(p_val)) {
                                    private$.blankCells(hlTable, key, c("chi_square", "df", "p_value"),
                                                        .("No test: too few informative risk groups remain."))
                                }
                            }
                        }
                    },
                    error = function(e) {
                        private$.addNotice(type = "WARNING", title = sprintf(.("Calibration Error: %s"), predictor), content = sprintf(.("Calibration analysis failed for %1$s: %2$s"), predictor, e$message))
                    }
                )
            }

            calTable$setNote("scale", if (supplied) {
                paste(c(.("Prediction scale is Probability: each predictor's values are used as supplied, as predicted probabilities of the positive class."),
                        if (length(excluded)) sprintf(.("Not shown, because they have values outside 0 to 1: %s."), paste(.rocLabel(excluded), collapse = ", "))),
                      collapse = " ")
            } else {
                paste(c(.("Prediction scale is Score: the risks come from a logistic model of each marker fitted to these same data, so this table describes that in-sample model, not an external prediction rule."),
                        if (isTRUE(self$options$calibrationMetrics) || isTRUE(self$options$eoRatio))
                            .("For such a model the calibration slope, intercept, calibration-in-the-large and E/O are 1, 0, 0 and 1 by construction, so they are left blank.")),
                      collapse = " ")
            })
            # F-21: ICI, E50, E90 and Emax are point estimates of a smoother fitted to these data,
            # with no interval. Noise inflates them: under perfect calibration (4-knot spline) the
            # mean ICI was 0.095 at n = 50 and 0.044 at n = 200 (independent review 2026-10-09).
            # Van Calster et al. 2019 (PMC6912996): at least 200 with and 200 without the event.
            y_cal <- private$.analysisData[[private$.outcome]]
            n_ev <- sum(y_cal == levels(y_cal)[2])
            calTable$setNote("spline_precision", if (isTRUE(self$options$splineCalibration)) paste(c(
                .("ICI, E50, E90 and Emax are point estimates with no confidence interval."),
                if (min(n_ev, length(y_cal) - n_ev) < 200)
                    sprintf(.("They are imprecise below about 200 patients with and 200 without the event (Van Calster et al. 2019); this sample has %1$s with and %2$s without. Sampling noise alone inflates them, so a well-calibrated model can look miscalibrated, and a small value is no proof of good calibration either."),
                            n_ev, length(y_cal) - n_ev)), collapse = " "))
            # Notes outlive deleteRows(), so a run without HL rows removes them.
            hl_rows <- isTRUE(self$options$hosmerLemeshow) && hlTable$rowCount > 0
            hlTable$setNote("hl_df", if (hl_rows && supplied) {
                .("Degrees of freedom are the number of groups because the probabilities were supplied, not estimated from these patients (Prediction scale: Probability). Supplied does not prove external: if they come from a model fitted to these same patients, g - 2 degrees of freedom apply and the p-value shown is too large.")
            } else if (hl_rows) {
                .("Degrees of freedom are the number of groups minus 2, because a logistic model with an intercept and a slope was fitted to these same data to obtain the predicted probabilities.")
            })
            hlTable$setNote("hl_caution", if (hl_rows) .("A non-significant Hosmer-Lemeshow test is not evidence of good calibration: its power depends on the sample size and on the number of groups, so a small sample rarely rejects and a large one rejects small misfits. Van Calster et al. (2019) advise against relying on it; the calibration slope, calibration-in-the-large and the calibration plot say more."))
            hlTable$setNote("hl_dropped", if (hl_rows && length(dropped)) paste(dropped, collapse = " "))
            if (isTRUE(self$options$calibrationPlot)) {
                private$.calibrationPlot <- list(
                    bins = do.call(rbind, unname(bins)), spline = do.call(rbind, unname(spline_df)),
                    density = do.call(rbind, unname(dens_df)), spline_failed = spline_failed,
                    excluded = excluded, predictors = names(private$.rocResults), supplied = supplied)
            }
        },
        .plotCalibration = function(image, ggtheme, theme, ...) {
            # Draws the frames .populateCalibrationAnalysis() left in this image's state, so a
            # resize or a reopened .omv shows the same points without refitting anything.
            cal <- tryCatch(image$state$calibration, error = function(e) NULL)
            if (!self$options$calibrationAnalysis || !self$options$calibrationPlot || is.null(cal)) {
                return(FALSE)
            }

            # Plot size is applied with setSize() in .init(). setState() here only stored
            # render-time payload and never reached jamovi's image sizing, so plotWidth /
            # plotHeight had no effect on the output at all.

            tryCatch(
                {
                    plot_data <- cal$bins
                    spline_data <- cal$spline
                    spline_failed <- cal$spline_failed

                    if (is.null(plot_data) || nrow(plot_data) == 0) {
                        return(private$.plotMessage(
                            ggtheme,
                            .("Calibration Plot Unavailable"),
                            if (length(cal$excluded) && setequal(cal$excluded, cal$predictors)) {
                                .("Prediction scale is Probability, but no predictor has all its values between 0 and 1, so there are no risks to plot.")
                            } else {
                                .("Every predictor produced a single distinct predicted probability, so there is nothing to group into calibration bins. That happens only when a predictor is constant within the analysed rows: with Prediction scale Score its logistic slope is then not estimable, and every patient receives the sample prevalence as risk.")
                            }
                        ))
                    }

                    # Label the plot with the number of groups ACTUALLY drawn, not the number
                    # requested. Tied predictor values collapse duplicate quantile breaks and
                    # empty bins are dropped, so a discrete predictor (an ordinal score,
                    # an IHC 0-3+ grade) yields fewer groups than hlGroups - and different
                    # predictors can collapse to different counts. The count shown here has to
                    # agree with the Groups column of the Hosmer-Lemeshow table above it.
                    drawn <- as.integer(table(plot_data$Predictor))
                    grp_label <- if (length(unique(drawn)) == 1) {
                        .fmt(.("{n} risk-quantile groups"), n = drawn[1])
                    } else {
                        .fmt(.("{lo}-{hi} risk-quantile groups"), lo = min(drawn), hi = max(drawn))
                    }

                    use_spline <- isTRUE(self$options$splineCalibration) && !is.null(spline_data) && nrow(spline_data) > 0
                    # Create plot. With a spline curve the grouped points become the data markers
                    # and the smooth curve is the estimate; without it the points are joined.
                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = prob, y = outcome, color = Predictor, group = Predictor)) +
                        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                        ggplot2::geom_point(size = 3, alpha = 0.7)
                    if (use_spline) {
                        p <- p + ggplot2::geom_line(data = spline_data, linewidth = 1)
                    } else {
                        p <- p + ggplot2::geom_line(linewidth = 1, alpha = 0.7)
                    }
                    # Marginal density of the predicted probabilities, so a calibration point
                    # backed by two patients is not read like one backed by two hundred. Scaled
                    # into the bottom 15% of the 0-1 panel so it cannot be mistaken for the
                    # calibration curve, and added BEFORE the theme calls below so it inherits
                    # whichever theme wins.
                    dens_data <- cal$density
                    show_density <- isTRUE(self$options$calibrationDensity) && !is.null(dens_data) && nrow(dens_data) > 0
                    if (show_density) {
                        p <- p + ggplot2::geom_ribbon(
                            data = dens_data,
                            mapping = ggplot2::aes(x = prob, ymin = 0, ymax = dens, colour = Predictor, fill = Predictor),
                            inherit.aes = FALSE, alpha = 0.15, linewidth = 0.4, outline.type = "upper"
                        )
                    }

                    # These four strings are the only explanation of what the plot shows and
                    # were shipping untranslated alongside translated axis labels.
                    subtitle <- if (use_spline) {
                        .fmt(.("Points: observed proportion in {grp}; curve: natural cubic spline with {k} knots on the logit scale"),
                             grp = grp_label, k = self$options$splineKnots)
                    } else {
                        .fmt(.("Observed vs Predicted Probabilities ({grp})"), grp = grp_label)
                    }
                    # Explain the extra ink rather than leaving an unlabelled band at the foot.
                    if (show_density) {
                        # "; " is a separator, not translatable text - keep it OUTSIDE the
                        # msgid. A translator cannot see a leading punctuation run, and a
                        # catalog that trims it silently runs the clause into the subtitle.
                        subtitle <- paste0(subtitle, "; ",
                            .("shaded band at the foot: density of the predicted probabilities"))
                    }
                    # Where the risks came from (Prediction scale), then what is missing and why.
                    caption <- c(
                        if (isTRUE(cal$supplied)) .("Predicted probabilities as supplied")
                        else .("Risks from a logistic model of each marker fitted to these data (in-sample, optimistic)"),
                        if (length(cal$excluded)) .fmt(.("Not shown (values outside 0 to 1): {who}"), who = paste(cal$excluded, collapse = ", ")),
                        if (length(spline_failed) > 0) .fmt(.("Spline curve not estimable for: {who} (too few events or distinct probabilities for {k} knots)"),
                                                            who = paste(spline_failed, collapse = ", "), k = self$options$splineKnots))
                    p <- p +
                        ggplot2::labs(
                            x = .("Predicted Probability"),
                            y = .("Observed Proportion"),
                            title = .("Calibration Plot"),
                            subtitle = paste(strwrap(subtitle, width = 100), collapse = "\n"),
                            caption = paste(caption, collapse = "\n"),
                            color = .("Predictor")
                        ) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggplot2::coord_fixed() +
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
                            legend.position = "bottom"
                        )


                    print(p)
                    TRUE
                },
                error = function(e) {
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("Calibration Plot Error"),
                        sprintf(.("Failed to create the calibration plot: %s"), conditionMessage(e))
                    )
                }
            )
        },
        # One-vs-rest ROCs share one reading of the marker. With direction "auto" chosen
        # per class, an ordinal outcome and a monotone marker force the middle class's AUC
        # to be >= 0.5 whichever way it points, inflating the macro average. Use the user's
        # direction, or, when it is "auto", the direction that separates the outcome's FIRST
        # and LAST factor levels. Those are the extremes only for an ordered outcome; for a
        # nominal one they are just the alphabetically first and last, so the pick is arbitrary.
        .ovrDirection = function(outcome, pred_vals) {
            opt <- self$options$direction %||% "auto"
            if (identical(opt, "higher")) return("<")
            if (identical(opt, "lower")) return(">")
            lv <- levels(outcome)
            ext <- outcome %in% c(lv[1], lv[length(lv)])
            resp <- factor(ifelse(outcome[ext] == lv[length(lv)], "Positive", "Negative"),
                           levels = c("Negative", "Positive"))
            tryCatch(.quietly(pROC::roc(response = resp, predictor = pred_vals[ext],
                                        direction = "auto", quiet = TRUE))$direction,
                     error = function(e) "<")
        },

        # One-vs-one AUCs: one AUC per class pair, every pair read in the SAME declared
        # direction. Shared by both multi-class branches so the number quoted in the
        # One-vs-Rest table is the same statistic the One-vs-One table prints. It used to be
        # taken from pROC::multiclass.roc(), which orients each pair with "auto".
        #
        # This is deliberately NOT pROC's number, and it is deliberately no longer called
        # Hand and Till's M anywhere the user can see. Hand and Till define M against a
        # classifier's own per-class probabilities; with a bare marker that scoring rule has to
        # be supplied from outside, and pROC's "auto" supplies it per pair by comparing the two
        # groups' MEDIANS - literally `direction == "auto" && median(controls) <= median(cases)`
        # in pROC:::roc.default. That orientation is fitted to the same data that then supply the
        # AUC, so it is biased upward: the exact behaviour the binary path raises a STRONG notice
        # about. It is NOT floored at 0.5, because a median split can disagree with the rank
        # statistic (measured on lognormal noise, n = 60, 3 classes: per-pair auto AUCs
        # 0.4750 / 0.4600 / 0.3950, multiclass.roc() = 0.4433; and on 3000 null binary samples at
        # n = 20, auto still reported below 0.5 in 12 per cent of runs, min 0.35). Taking the
        # orientation from the user's Direction instead makes the estimate honest rather than
        # merely different, at the cost that it falls below 0.5 when the marker reads backwards.
        # Because it can, it is labelled "one-vs-one pairwise AUC", not "Hand-Till".
        #
        # The price of one declared direction: a marker that orders the classes
        # non-monotonically averages toward 0.5 even when single pairs separate strongly
        # (measured: pairs 0.9500 / 0.4750 / 0.0025 -> mean 0.4758, which .interpretAUC() calls
        # "Below chance", for a marker that tells B from A at 0.95 and B from C at 0.9975 read
        # the other way). Both callers therefore print the RANGE of pair AUCs beside the mean,
        # and the avgTable "ovo_definition" note explains it.
        #
        # Column order follows combn(levels(outcome), 2), which is what the One-vs-One row
        # loop below also uses, so pair i here is pair i there.
        # min()/max() without na.rm print "pairs range NA to NA" beside a finite mean the
        # moment ONE pair fails; with na.rm an all-NA vector returns -Inf/Inf plus a warning.
        # Report the range of the pairs that actually computed, or NA when none did.
        .pairRange = function(x) {
            x <- x[is.finite(x)]
            if (!length(x)) c(NA_real_, NA_real_) else c(min(x), max(x))
        },

        .pairwiseAUCs = function(outcome, pred_vals, direction) {
            pairs <- combn(levels(outcome), 2)
            vapply(seq_len(ncol(pairs)), function(i) {
                as.numeric(private$.pairROC(outcome, pred_vals, pairs[, i], direction)$auc)
            }, numeric(1))
        },
        # The ROC of one class pair: the later level of `pair` is the case.
        .pairROC = function(outcome, pred_vals, pair, direction) {
            keep <- outcome %in% pair
            pROC::roc(response = factor(outcome[keep], levels = pair), predictor = pred_vals[keep],
                      direction = direction, quiet = TRUE)
        },
        # One multi-class row's AUC limits (VAL-20): the bootstrap interval for a one-vs-rest row
        # when Bootstrap is on, DeLong otherwise (one-vs-one rows always). Returns c(lower, upper),
        # or the footnote saying why the cells are blank - never a zero-width interval.
        .multiClassCi = function(roc_obj, boot) {
            ci <- if (boot) {
                tryCatch(private$.bootstrapAucCi(roc_obj), error = function(e) NULL)
            } else {
                suppressWarnings(pROC::ci.auc(roc_obj, conf.level = self$options$confidenceLevel / 100, method = "delong"))
            }
            lim <- as.numeric(ci)[c(1, 3)]
            if (length(ci) == 3 && all(is.finite(lim)) && lim[2] > lim[1]) {
                if (boot) attr(lim, "method_note") <- private$.bootstrapCiText(ci)
                return(lim)
            }
            if (boot && min(length(roc_obj$cases), length(roc_obj$controls)) < 2L) {
                private$.singlePatientBootNote()
            } else if (boot) {
                .("No bootstrap interval could be formed: resamples lacked variation or both groups, or the requested interval type could not be computed.")
            } else if (min(length(roc_obj$cases), length(roc_obj$controls)) < 2) {
                # pROC returns no DeLong interval at all then; with both groups of 2 or more a
                # rejected interval means a variance of exactly 0 (the branch below).
                .("One of the two groups has a single patient, so DeLong's standard error cannot be estimated and no interval is shown.")
            } else {
                .("DeLong's standard error is 0 here (the groups are perfectly separated, or the marker is constant), so the interval would have zero width.")
            }
        },

        .populateMultiClassROC = function() {
            if (!self$options$multiClassROC) {
                return()
            }

            aucTable <- self$results$results$multiClassAUC
            # Which interval each row carries (VAL-20); only one-vs-rest rows resample, so only
            # they name the seed.
            ovr <- identical(self$options$multiClassStrategy, "ovr")
            boot <- isTRUE(self$options$useBootstrap) && ovr
            level <- self$options$confidenceLevel
            aucTable$setNote("seed", if (boot) .fmt(.("Random seed: {seed}"), seed = self$options$seed))
            aucTable$setNote("ci", if (boot) {
                sprintf(.("AUC limits use %1$s%% bootstrap intervals when estimable (requested type: %2$s; %3$s attempted resamples). Each interval's footnote identifies the actual type and usable resample count."), level,
                        switch(self$options$bootstrapMethod, percentile = .("percentile"), basic = .("basic"), "BCa"), self$options$bootstrapSamples)
            } else if (ovr) {
                sprintf(.("AUC limits are %s%% DeLong intervals (DeLong et al. 1988)."), level)
            } else {
                paste(c(sprintf(.("AUC limits are %s%% DeLong intervals for each pair of classes (DeLong et al. 1988)."), level),
                        if (isTRUE(self$options$useBootstrap)) .("Bootstrap confidence intervals apply to the one-vs-rest strategy only.")),
                      collapse = " ")
            })
            # This table still builds its rows in .run(): the row count is not knowable in
            # advance, so deleteRows() is required or every run would stack duplicates.
            .deleteTableRows(aucTable)
            avgTable <- self$results$results$multiClassAverage
            avgTable$setNote(
                "weighted",
                .("Weighted AUC averages the one-vs-rest AUCs in proportion to how many cases each class contains; Macro AUC weights every class equally. The One-vs-One strategy is defined as an unweighted average over class PAIRS, so no weighted value is given for it."))
            # Which class of a pair is the case comes from the level order (combn() in .pairwiseAUCs()).
            pair_order <- .("Each one-vs-one pair is oriented by the outcome's level order: the later level of the pair is read as the case. For a nominal (unordered) outcome that order is arbitrary, so the pairwise AUCs depend on how the levels happen to be ordered; order the levels in Data > Setup when the classes have a natural order.")
            aucTable$setNote("pair_order", if (identical(self$options$multiClassStrategy, "ovo")) pair_order)
            avgTable$setNote("pair_order", pair_order)
            # Says why this is not the figure pROC::multiclass.roc() prints, why it is not
            # called Hand and Till's M, and what one declared direction costs: see .pairwiseAUCs().
            avgTable$setNote(
                "ovo_definition",
                .("The one-vs-one pairwise AUC averages the AUC of every class pair, reading all pairs in ONE direction: the one set under Direction, or, when Direction is auto, the one that separates the outcome's first and last factor levels (the extremes only if the outcome is ordered \u2014 for an unordered outcome those are merely the alphabetically first and last levels, so the orientation is arbitrary and Direction should be set explicitly). A pair below 0.5 separates that pair in the opposite order to the one declared. Because the pairs are averaged unweighted, a marker that orders the classes non-monotonically averages toward 0.5 even when single pairs separate strongly, so read the range of pair AUCs printed beside the mean rather than the mean alone. This is not the figure pROC's multiclass.roc() prints: that function orients each pair by comparing the two groups' median values on these same data, which biases it upward but does not floor it at 0.5 either."))
            # Why the one-vs-rest averages carry no band (F-03; see the macro computation below).
            avgTable$setNote("ovr_average", if (ovr)
                .("The one-vs-rest Macro and Weighted AUCs are not a summary of discrimination and carry no performance band. Every class is read in one shared direction, so with equal class sizes the one-vs-rest AUCs always add up to half the number of classes and both averages are exactly 0.5 for any marker, a perfect one included; with unequal class sizes even a perfect marker can average below 0.5. Judge discrimination from the one-vs-one pairwise AUCs quoted in the Interpretation column (mean and range), or choose the One-vs-One strategy to list every class pair."))
            # Declared `rows: 1` in the .r.yaml, but this method loops over predictors. Writing
            # setRow(rowNo = 1) inside that loop meant each predictor overwrote the last, so the
            # panel silently showed whichever marker happened to sit last in the Predictor
            # Variables box - macro AUC 0.4938 or 0.7975 on identical data depending purely on
            # list order, with no column saying which marker it belonged to. Clear the declared
            # row and add one per predictor, naming the predictor in the method column.
            .deleteTableRows(avgTable)

            # Check if outcome has > 2 levels
            outcome <- if (!is.null(private$.multiClassOutcome)) {
                private$.multiClassOutcome
            } else {
                private$.analysisData[[private$.outcome]]
            }
            if (nlevels(outcome) < 3) {
                # This used to setContent() the shared instructions item, wiping the whole
                # welcome/glossary panel - and it did so on a SUCCESSFUL run, where nothing else
                # warned the user. This method is called from .run() before .renderNotices(), so
                # a notice reaches the user here.
                private$.addNotice(
                    type = "WARNING",
                    title = .("Multi-Class ROC Needs 3 or More Outcome Levels"),
                    content = sprintf(.("Multi-class ROC analysis was requested, but the outcome variable has %s levels. \u2022 The Multi-Class ROC Summary, Multi-Class Average AUC and Multi-Class ROC Curves panels are therefore empty. \u2022 For a two-level outcome the standard ROC results above already give the complete analysis. \u2022 Clear \"Multi-class ROC analysis\" to remove the empty panels, or choose an outcome with 3 or more levels."), nlevels(outcome))
                )
                return()
            }

            # The direction note used to be written with setNote() INSIDE this loop, so with
            # two predictors the last one's direction silently overwrote the first's while the
            # note still claimed it held for every marker. Collect them and write once, below.
            dir_by_pred <- character(0)

            for (predictor in private$.predictors) {
                tryCatch(
                    {
                        # Get data
                        data <- private$.analysisData
                        pred_vals <- data[[predictor]]

                        # If we want One-vs-Rest, compute OVR macro average separately
                        if (self$options$multiClassStrategy == "ovr") {
                            ovr_aucs <- numeric(nlevels(outcome))
                            ovr_dir <- private$.ovrDirection(outcome, pred_vals)
                            dir_by_pred[[predictor]] <- ovr_dir
                            for (j in seq_along(levels(outcome))) {
                                lvl <- levels(outcome)[j]
                                # Create binary outcome: Class vs Rest
                                binary_outcome <- factor(ifelse(outcome == lvl, "Positive", "Negative"),
                                    levels = c("Negative", "Positive")
                                )

                                # Calculate ROC
                                roc_obj <- pROC::roc(
                                    response = binary_outcome, predictor = pred_vals,
                                    direction = ovr_dir, quiet = TRUE
                                )
                                ovr_aucs[j] <- as.numeric(roc_obj$auc)

                                # Bootstrap limits when asked for, DeLong otherwise (VAL-20: the
                                # cells used to be empty with no note); computed once per row.
                                class_ci <- private$.multiClassCi(roc_obj, boot)
                                key <- paste0(private$.escapeVar(predictor), "_", private$.escapeVar(lvl))

                                row <- list(
                                    # No predictor column exists in this table, so with more than
                                    # one predictor the rows were indistinguishable - six rows
                                    # showing three class labels twice over. Name it here.
                                    class = .rocLabel(if (length(private$.predictors) > 1) paste0(predictor, ": ", lvl) else lvl, note = FALSE),
                                    strategy = .("One-vs-Rest"),
                                    auc = ovr_aucs[j],
                                    auc_lower = if (is.character(class_ci)) NA else class_ci[1],
                                    auc_upper = if (is.character(class_ci)) NA else class_ci[2],
                                    n_positive = sum(outcome == lvl),
                                    n_negative = sum(outcome != lvl)
                                )
                                aucTable$addRow(rowKey = key, values = row)
                                if (is.character(class_ci)) private$.blankCells(aucTable, key, c("auc_lower", "auc_upper"), class_ci)
                                if (!is.null(attr(class_ci, "method_note"))) {
                                    for (col in c("auc_lower", "auc_upper"))
                                        aucTable$addFootnote(col = col, rowKey = key,
                                            note = attr(class_ci, "method_note"))
                                }
                            }

                            # OVR Macro Average
                            ovr_macro_auc <- mean(ovr_aucs, na.rm = TRUE)
                            # Prevalence-weighted average of the SAME one-vs-rest AUCs: each class
                            # weighted by how many cases it contains (sklearn's average="weighted").
                            # ovr_aucs is indexed by j over levels(outcome), so the explicit
                            # [levels(outcome)] reindex keeps the weights lined up element for
                            # element even if table() ever reordered.
                            class_n <- as.numeric(table(outcome)[levels(outcome)])
                            ovr_weighted_auc <- stats::weighted.mean(ovr_aucs, w = class_n, na.rm = TRUE)
                            # No band on these two (F-03): read in one shared direction, the
                            # one-vs-rest AUCs of K equal-sized classes sum to K/2, so the macro is
                            # 0.5 for ANY marker - a perfect A < B < C marker read 'Minimal' - and
                            # class sizes alone move it (perfect marker, 20/25/30: 0.467, 'Below
                            # chance'). The "ovr_average" note says so; the cell quotes the pairwise AUCs.
                            # Same computation as the One-vs-One branch below (same helper,
                            # same direction), so the two panels can no longer print two
                            # different numbers under one name.
                            ovr_pair_aucs <- private$.pairwiseAUCs(outcome, pred_vals, ovr_dir)
                            ht_quote <- mean(ovr_pair_aucs, na.rm = TRUE)
                            ovr_pair_range <- private$.pairRange(ovr_pair_aucs)
                            avgTable$addRow(rowKey = private$.escapeVar(predictor), values = list(
                                averaging_method = paste0(predictor, ": ", .("OVR Macro Average")),
                                macro_auc = ovr_macro_auc,
                                # This column used to be filled with the PAIRWISE AUC, which is
                                # an unweighted statistic over class pairs - printing it under a
                                # "Weighted AUC" heading mislabelled it. The pairwise value is
                                # still reported, correctly named, in the interpretation cell;
                                # this is now the actual prevalence-weighted average.
                                weighted_auc = ovr_weighted_auc,
                                # paste0() around a translated word left the parenthetical
                                # untranslatable and unnamed; the msgid is now a literal and
                                # says which statistic the number is. Not named "Hand-Till": it
                                # is oriented by the declared Direction, see .pairwiseAUCs().
                                # The range goes with the mean because this panel prints no
                                # per-pair rows: without it, a non-monotone marker's collapse
                                # toward 0.5 is invisible here.
                                interpretation = sprintf(
                                    .("One-vs-one pairwise AUC, unweighted mean over class pairs: %1$.3f; pairs range %2$.3f to %3$.3f"),
                                    ht_quote, ovr_pair_range[1], ovr_pair_range[2])
                            ))
                        } else {
                            # One-vs-One (Pairwise)
                            # It is DEFINED as an unweighted average over class PAIRS, so
                            # there is no prevalence weighting of it that is the same statistic.
                            # The summary row is now written AFTER the pair loop and from the
                            # pairwise AUCs actually shown, instead of from pROC's own
                            # multiclass.roc value: the two agree exactly when the directions
                            # agree (verified: 0.72679 both ways) but not once Direction is
                            # honoured below, and a summary must be the same computation as the
                            # rows it summarises.

                            # Per-pair AUC breakdown
                            # direction = "auto" was hard-coded here, so the user's Direction
                            # setting was silently discarded and every pairwise AUC was oriented
                            # by pROC from the same data it is computed on (a median comparison,
                            # not an AUC maximisation, so not floored at 0.5 either) - the exact
                            # fitted-orientation bias the binary path raises a STRONG notice
                            # about. Use the same resolution as the one-vs-rest branch:
                            # one orientation for all pairs, honouring Direction, and for
                            # "auto" derived once from the outcome's first and last factor
                            # levels (arbitrary unless the outcome is ordered).
                            pair_dir <- private$.ovrDirection(outcome, pred_vals)
                            dir_by_pred[[predictor]] <- pair_dir
                            pairs <- combn(levels(outcome), 2)
                            pair_rocs <- lapply(seq_len(ncol(pairs)), function(i) private$.pairROC(outcome, pred_vals, pairs[, i], pair_dir))
                            pair_aucs <- vapply(pair_rocs, function(r) as.numeric(r$auc), numeric(1))
                            for (i in seq_len(ncol(pairs))) {
                                class1 <- pairs[1, i]
                                class2 <- pairs[2, i]
                                # DeLong for every pair, Bootstrap or not (see the "ci" note)
                                pair_ci <- private$.multiClassCi(pair_rocs[[i]], FALSE)
                                key <- paste0(private$.escapeVar(predictor), "_", private$.escapeVar(class1), "_", private$.escapeVar(class2))

                                row <- list(
                                    class = .rocLabel(if (length(private$.predictors) > 1)
                                        paste0(predictor, ": ", class1, " vs ", class2)
                                    else paste(class1, "vs", class2), note = FALSE),
                                    strategy = .("One-vs-One"),
                                    auc = pair_aucs[i],
                                    auc_lower = if (is.character(pair_ci)) NA else pair_ci[1],
                                    auc_upper = if (is.character(pair_ci)) NA else pair_ci[2],
                                    n_positive = sum(outcome == class2), # class2, the later level, is the case
                                    n_negative = sum(outcome == class1)
                                )
                                aucTable$addRow(rowKey = key, values = row)
                                if (is.character(pair_ci)) private$.blankCells(aucTable, key, c("auc_lower", "auc_upper"), pair_ci)
                            }

                            ht_auc <- mean(pair_aucs, na.rm = TRUE)
                            pair_range <- private$.pairRange(pair_aucs)
                            avgTable$addRow(rowKey = private$.escapeVar(predictor), values = list(
                                averaging_method = paste0(predictor, ": ", .("One-vs-One Pairwise")),
                                macro_auc = ht_auc,
                                weighted_auc = NA,
                                # The bucket word describes the unweighted mean, which collapses
                                # toward 0.5 for a non-monotone marker; the range is what tells
                                # the reader whether that is what happened. See .pairwiseAUCs().
                                interpretation = sprintf(
                                    .("%1$s (pairs range %2$.3f to %3$.3f)"),
                                    private$.interpretAUC(ht_auc), pair_range[1], pair_range[2])
                            ))
                        }
                    },
                    error = function(e) {
                        private$.addNotice(type = "WARNING", title = sprintf(.("Multi-Class ROC Error: %s"), predictor), content = sprintf(.("Multi-class ROC analysis failed for %1$s: %2$s"), predictor, e$message))
                    }
                )
            }

            # One note for the whole table, written after every predictor has resolved its
            # direction. Two markers can resolve differently under Direction = auto, so the
            # note names them when they disagree instead of quoting whichever ran last.
            if (length(dir_by_pred)) {
                word_higher <- .("higher")
                word_lower <- .("lower")
                dir_words <- ifelse(dir_by_pred == "<", word_higher, word_lower)
                why <- if (identical(self$options$direction, "auto"))
                    .("chosen from the outcome's first and last factor levels because Direction is set to auto; for an unordered outcome those are merely the alphabetically first and last levels, so the orientation is arbitrary")
                else
                    .("as specified by Direction")
                same_dir <- length(unique(dir_words)) == 1L
                per_marker <- paste(paste0(names(dir_by_pred), ": ", dir_words), collapse = "; ")
                note_key <- if (identical(self$options$multiClassStrategy, "ovr")) "ovr_direction" else "ovo_direction"
                note_txt <- if (identical(note_key, "ovr_direction")) {
                    if (same_dir)
                        sprintf(.("One-vs-rest curves read %1$s values of every marker as indicating the class, the same way for every class (%2$s)."), dir_words[[1]], why)
                    else
                        sprintf(.("One-vs-rest curves read each marker the same way for every class, but the markers do not all read the same way (%1$s). The orientation was %2$s."), per_marker, why)
                } else {
                    if (same_dir)
                        sprintf(.("One-vs-one curves read %1$s values of every marker as indicating the later class of the pair, the same way for every pair (%2$s)."), dir_words[[1]], why)
                    else
                        sprintf(.("One-vs-one curves read each marker the same way for every pair, but the markers do not all read the same way (%1$s). The orientation was %2$s."), per_marker, why)
                }
                tryCatch(aucTable$setNote(note_key, note_txt, init = FALSE), error = function(e) NULL)
            }
        },
        .plotMultiClassROC = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (!isTRUE(self$options$multiClassROC)) return(FALSE)
            if (is.null(st) || !identical(st$schema, 2L) || is.null(st[["multi"]])) {
                return(private$.plotStateUnavailable(image, ggtheme))
            }
            tryCatch(
                {
                    plot_data <- as.data.frame(st$multi$data)
                    n_levels <- st$multi$nlevels

                    if (n_levels < 3) {
                        # The panel is shown whenever multiClassROC is ticked, because the
                        # declarative visible: rule in the .r.yaml cannot count outcome levels.
                        # Say why it is empty rather than leaving a blank panel.
                        return(private$.plotMessage(
                            ggtheme,
                            .("Multi-Class ROC Needs 3 or More Outcome Levels"),
                            .fmt(.("The outcome variable has {n} levels, so there is no multi-class curve to draw. For a two-level outcome the standard ROC results already give the complete analysis."), n = n_levels)
                        ))
                    }

                    if (self$options$multiClassStrategy == "ovo") {
                        # Do NOT simply advise switching to OVR: that changes the estimand, not
                        # just the picture. OVO reports the pairwise AUC over class
                        # pairs; OVR reports the macro average of one-vs-rest AUCs, which
                        # penalises an intermediate class. The two differ materially on the same
                        # data, so the trade-off has to be stated, not hidden behind "switch to".
                        return(private$.plotMessage(
                            ggtheme,
                            .("Multi-Class ROC Curves Not Drawn for One-vs-One"),
                            .("One-vs-One (OVO) ROC curve plots are not yet supported. The pairwise AUC values in the tables are unaffected. Switching to One-vs-Rest would draw the curves, but it also changes the reported statistic: OVR reports the macro average of one-vs-rest AUCs rather than the one-vs-one pairwise AUC, and the two do not agree.")
                        ))
                    }

                    if (nrow(plot_data) == 0) {
                        return(FALSE)
                    }
                    # Path order: see .plotROCCurve (VAL-02).
                    plot_data <- plot_data[order(plot_data$Predictor, plot_data$Class, plot_data$FPR, plot_data$TPR), ]

                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = FPR, y = TPR, color = Class)) +
                        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                        ggplot2::geom_line(linewidth = 1) +
                        ggplot2::labs(
                            x = .("False Positive Rate"),
                            y = .("True Positive Rate"),
                            title = .("Multi-Class ROC Curves (One-vs-Rest)"),
                            color = .("Class")
                        ) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggplot2::coord_fixed() +
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            legend.position = "bottom"
                        )

                    if (length(st$multi$predictors) > 1) {
                        p <- p + ggplot2::facet_wrap(~Predictor)
                    }

                    print(p)
                    TRUE
                },
                error = function(e) {
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("Multi-Class ROC Plot Error"),
                        sprintf(.("Failed to create the multi-class ROC plot: %s"), conditionMessage(e))
                    )
                }
            )
        },
        # F-09: a fixed score cutoff that puts every analysed patient on one side - the default
        # 0.5 applied to a marker on its own scale, typically - turns the impact rows into
        # flag-everyone (sensitivity 1, specificity 0, net benefit of treating all) or flag-no-one.
        # Warned with the cutoff and the marker's analysed range. `scores`: named list of the
        # analysed values; `higher`: per predictor, TRUE when a score at or above the cutoff is
        # positive (.dvCounts()).
        .warnDegenerateCutoffs = function(scores, cuts, higher) {
            everyone <- character(0)
            nobody <- character(0)
            for (p in names(scores)) {
                x <- scores[[p]]
                x <- x[is.finite(x)]
                if (!length(x)) next
                for (cut in cuts) {
                    flagged <- sum(if (higher[[p]]) x >= cut else x <= cut)
                    if (flagged > 0 && flagged < length(x)) next
                    item <- sprintf(.("%1$s at cutoff %2$s (analysed values %3$s to %4$s)"),
                        p, signif(cut, 4), signif(min(x), 4), signif(max(x), 4))
                    if (flagged > 0) everyone <- c(everyone, item) else nobody <- c(nobody, item)
                }
            }
            if (length(everyone) > 0) {
                private$.addNotice(type = "WARNING", title = .("Fixed Cutoff Flags Every Patient"),
                    content = sprintf(.("At these fixed score cutoffs every analysed patient is classified as positive: %s. The clinical impact and workload rows at such a cutoff describe testing everyone (sensitivity 1, specificity 0, the net benefit of treating all), not the marker. Enter cutoffs on the marker's own scale under Fixed score cutoffs; the default 0.5 suits predicted probabilities only."),
                        paste(everyone, collapse = "; ")))
            }
            if (length(nobody) > 0) {
                private$.addNotice(type = "WARNING", title = .("Fixed Cutoff Flags No Patient"),
                    content = sprintf(.("At these fixed score cutoffs no analysed patient is classified as positive: %s. The clinical impact and workload rows at such a cutoff describe testing no one (sensitivity 0, specificity 1, net benefit 0), not the marker. Enter cutoffs on the marker's own scale under Fixed score cutoffs; the default 0.5 suits predicted probabilities only."),
                        paste(nobody, collapse = "; ")))
            }
        },
        .populateClinicalImpact = function() {
            impact <- self$results$results$clinicalImpactTable
            decision <- self$results$results$decisionImpactSummary
            workload <- self$results$results$screeningWorkload
            .deleteTableRows(impact)
            .deleteTableRows(decision)
            .deleteTableRows(workload)
            # F-51: a blank or malformed box used to stop() the whole analysis - every table and
            # plot gone, the message untranslated. Now it empties only these three tables and says
            # which box to fix. Each box is read on its own; .dvNumbers() is shared, so it is not
            # changed. A blank prevalence list is valid (no extra scenarios).
            cuts <- tryCatch(.dvNumbers(self$options$impactCutoffs), error = function(e) numeric())
            if (!length(cuts)) {
                private$.addNotice(type = "WARNING", title = .("Fixed Score Cutoffs Not Read"),
                    content = sprintf(.("Fixed score cutoffs ('%s') must be one or more numbers on the marker's own scale, separated by commas, for example 0.3, 0.5. The clinical impact, decision impact and workload tables are left empty; every other result is unaffected."),
                        self$options$impactCutoffs))
                return()
            }
            scenario <- tryCatch(.dvNumbers(self$options$prevalenceScenarios, 0, 1), error = function(e) {
                private$.addNotice(type = "WARNING", title = .("Additional Prevalences Not Read"),
                    content = sprintf(.("Additional target prevalences ('%s') must be proportions between 0 and 1, separated by commas, for example 0.05, 0.2. They were ignored; the workload table shows the main prevalence only."),
                        self$options$prevalenceScenarios))
                numeric()
            })
            utility <- self$options$utilityThreshold
            population <- self$options$workloadPopulationSize
            note <- .("Workload uses fixed score cutoffs, not prevalence-selected thresholds. Population prevalence and the utility harm/benefit threshold are separate inputs. Projected counts are expectations, conditional on transportable sensitivity/specificity. Inverse net benefit and inverse Youden are not numbers of tests or reviews. This replaces the legacy prevalence-as-threshold calculation.")
            impact$setNote("threshold_source", note)
            workload$setNote("source", note)
            decision$setNote("population", if (self$options$useObservedPrevalence)
                .("Observed sample counts and PPV; unit is given in the workload table.") else
                .("Projected target-population counts and PPV; fractional expected counts are retained."))
            undefined <- private$.undefinedRatioNotes()
            data <- private$.analysisData
            y <- as.integer(data[[private$.outcome]] == levels(data[[private$.outcome]])[2])
            preds <- names(private$.rocResults)
            private$.warnDegenerateCutoffs(lapply(stats::setNames(preds, preds), function(p) data[[p]]), cuts,
                vapply(preds, function(p) private$.rocObjects[[p]]$direction != ">", logical(1)))
            for (predictor in names(private$.rocResults)) {
                higher <- private$.rocObjects[[predictor]]$direction != ">"
                scores <- data[[predictor]]
                for (cut in cuts) {
                    z <- .dvCounts(y, scores, cut, higher)
                    se <- if (sum(y == 1)) unname(z['tp'] / sum(y == 1)) else NA_real_
                    sp <- if (sum(y == 0)) unname(z['tn'] / sum(y == 0)) else NA_real_
                    observed <- isTRUE(self$options$useObservedPrevalence)
                    prevalence <- if (observed) mean(y) else self$options$prevalence
                    size <- if (observed) length(y) else population
                    w <- .dvWorkload(se, sp, prevalence, size, utility)
                    key <- paste(predictor, cut, sep = "_")
                    values <- list(predictor = predictor,
                        threshold = cut, nnt = if (is.finite(w$net_benefit) && w$net_benefit > 0)
                            1 / w$net_benefit else NA_real_,
                        nnd = if (is.finite(se+sp) && se+sp > 1) 1 / (se+sp-1) else NA_real_,
                        tested_positive = w$flags / size, true_positive_rate = se,
                        false_positive_rate = 1-sp, net_benefit_per_100 = 100*w$net_benefit)
                    impact$addRow(rowKey = key, values = values)
                    private$.blankUndefined(impact, key, values, undefined)
                    values <- list(predictor = predictor, threshold = cut,
                        n_high_risk = w$flags, n_high_risk_with_event = w$tp,
                        n_high_risk_without_event = w$fp, ppv = w$ppv, npv = w$npv)
                    decision$addRow(rowKey = key, values = values)
                    private$.blankUndefined(decision, key, values, undefined)
                    scenarios <- c(prevalence, scenario)
                    for (i in seq_along(scenarios)) {
                        row <- .dvWorkload(se, sp, scenarios[i], if (i == 1) size else population, utility)
                        values <- c(list(predictor = predictor, cutoff = cut,
                                         source = if (i == 1 && observed) .("Observed") else .("Projected"),
                                         unit = .rocLabel(self$options$analysisUnit, note = FALSE)), as.list(row))
                        workload$addRow(rowKey = paste(key, i, sep = "_"), values = values)
                        private$.blankUndefined(workload, paste(key, i, sep = "_"), values, undefined)
                    }
                }
            }
        },
        # Decision curve (Vickers et al. 2019) and clinical impact curve (Kerr et al. 2016) over
        # the threshold-probability grid 0.01-0.99 (VAL-11: they used to be drawn at the fixed SCORE
        # cutoffs, one point per line and so no line at all). Computed here because the renderers
        # draw from image state alone. Positive means risk >= pt: the risk is already oriented, so
        # Direction is not applied again. Fitted risks reproduce the sample prevalence; at an
        # assumed prevalence the fitted intercept moves by logit(prev) - logit(sample prevalence)
        # (Prentice & Pyke 1979). Supplied probabilities are used as supplied.
        .computeDecisionCurves = function() {
            o <- self$options
            if (!(isTRUE(o$decisionImpactCurves) || (isTRUE(o$clinicalImpact) && isTRUE(o$clinicalUtilityCurve)))) return()
            data <- private$.analysisData
            y <- as.integer(data[[private$.outcome]] == levels(data[[private$.outcome]])[2])
            prev_hat <- mean(y)
            prev <- private$.effectivePrevalence(prev_hat)
            supplied <- identical(o$predictionScale, "probability")
            shifted <- !supplied && !isTRUE(all.equal(prev, prev_hat))
            pt <- (1:99) / 100   # the doubles nearest 0.01, ..., 0.99 (seq(by = 0.01) drifts: its 10th is not 0.1)
            grid <- list(); excluded <- character(0)
            for (predictor in names(private$.rocResults)) {
                risk <- private$.riskProbabilities(data[[predictor]], y)
                if (is.null(risk$probs)) {
                    excluded <- c(excluded, predictor)
                    next
                }
                r <- if (shifted) stats::plogis(risk$lp + stats::qlogis(prev) - stats::qlogis(prev_hat)) else risk$probs
                # per 1000 tested at prevalence prev: tp = 1000 Se prev, flags = tp + 1000 (1 - Sp)(1 - prev)
                w <- do.call(rbind, lapply(pt, function(t) .dvWorkload(mean(r[y == 1] >= t), mean(r[y == 0] < t), prev, 1000, t)))
                grid[[predictor]] <- data.frame(Predictor = predictor, Threshold = pt, NetBenefit = w$net_benefit,
                                                HighRisk = w$flags, TruePositive = w$tp, stringsAsFactors = FALSE)
            }
            private$.decisionCurves <- list(
                grid = do.call(rbind, unname(grid)), thresholds = pt, treat_all = prev - (1 - prev) * pt / (1 - pt),
                prevalence = prev, observed = isTRUE(o$useObservedPrevalence), shifted = shifted,
                supplied = supplied, excluded = excluded, utility = o$utilityThreshold)
        },

        # Subtitle and caption lines shared by the two curves: where the risks came from, the
        # prevalence they assume, and who is not drawn.
        .curveSubtitle = function(cv) {
            parts <- c(
                if (isTRUE(cv$supplied)) .("Predicted probabilities as supplied") else .("Risks from a logistic model of each marker fitted to these data (in-sample, optimistic)"),
                if (isTRUE(cv$observed)) .fmt(.("observed prevalence {prev}"), prev = sprintf("%.3g", cv$prevalence)) else .fmt(.("assumed prevalence {prev}"), prev = sprintf("%.3g", cv$prevalence)),
                if (isTRUE(cv$shifted)) .("model intercept shifted to that prevalence (Prentice & Pyke 1979)"))
            paste(strwrap(paste(parts, collapse = "; "), width = 100), collapse = "\n")
        },
        .curveExcluded = function(cv) {
            if (length(cv$excluded)) .fmt(.("Not shown (values outside 0 to 1): {who}"), who = paste(cv$excluded, collapse = ", "))
        },

        # Clinical impact curve (Kerr et al. 2016): of 1000 people tested, how many are called high
        # risk at each threshold probability, and how many of those have the event.
        .plotDecisionImpact = function(image, ggtheme, theme, ...) {
            cv <- tryCatch(image$state$curves, error = function(e) NULL)
            if (!isTRUE(self$options$decisionImpactCurves) || is.null(cv)) {
                return(FALSE)
            }

            tryCatch(
                {
                    g <- cv$grid
                    if (is.null(g) || nrow(g) == 0) {
                        return(private$.plotMessage(
                            ggtheme,
                            .("Clinical Impact Curve Unavailable"),
                            .("Prediction scale is Probability, but no predictor has all its values between 0 and 1, so there are no risks to plot.")
                        ))
                    }
                    lab_high <- .("Called high risk")
                    lab_event <- .("High risk and has the event")
                    plot_data <- data.frame(
                        Threshold = rep(g$Threshold, 2L),
                        Count = c(g$HighRisk, g$TruePositive),
                        Predictor = rep(g$Predictor, 2L),
                        Group = rep(c(lab_high, lab_event), each = nrow(g)),
                        stringsAsFactors = FALSE
                    )

                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
                            x = Threshold, y = Count, colour = Predictor, linetype = Group
                        )) +
                        ggplot2::geom_line(linewidth = 1) +
                        ggplot2::labs(
                            x = .("Threshold probability"),
                            y = .("Number per 1000 tested"),
                            title = .("Clinical Impact Curve"),
                            subtitle = private$.curveSubtitle(cv),
                            caption = private$.curveExcluded(cv),
                            colour = .("Predictor"),
                            linetype = .("Group")
                        ) +
                        # ggtheme REPLACES every theme()/scale_*() that precedes it, so it goes on
                        # first and the tweaks that must survive go after it.
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9),
                            legend.position = "bottom",
                            legend.box = "vertical"
                        )

                    # Follow jamovi's global palette so this plot agrees with the rest of the
                    # document; after ggtheme, or ggtheme's own scale would win.
                    cols <- tryCatch(
                        jmvcore::colorPalette(length(unique(plot_data$Predictor)), theme$palette, "color"),
                        error = function(e) NULL
                    )
                    if (!is.null(cols)) {
                        p <- p + ggplot2::scale_colour_manual(values = cols)
                    }

                    print(p)
                    TRUE
                },
                error = function(e) {
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("Clinical Impact Curve Error"),
                        sprintf(.("Failed to create the clinical impact curve: %s"), conditionMessage(e))
                    )
                }
            )
        },
        # Decision curve (Vickers et al. 2019): net benefit = Se prev - (1 - Sp)(1 - prev) pt/(1 - pt)
        # against threshold probability pt, beside treating everyone and treating no one.
        .plotClinicalUtility = function(image, ggtheme, theme, ...) {
            cv <- tryCatch(image$state$curves, error = function(e) NULL)
            if (!self$options$clinicalImpact || !self$options$clinicalUtilityCurve || is.null(cv)) {
                return(FALSE)
            }

            # Plot size is applied with setSize() in .init(). setState() here only stored
            # render-time payload and never reached jamovi's image sizing, so plotWidth /
            # plotHeight had no effect on the output at all.

            tryCatch(
                {
                    g <- cv$grid
                    if (is.null(g) || nrow(g) == 0) {
                        return(private$.plotMessage(
                            ggtheme,
                            .("Decision Curve Unavailable"),
                            .("Prediction scale is Probability, but no predictor has all its values between 0 and 1, so there are no risks to plot.")
                        ))
                    }
                    lab_model <- .("Model")
                    lab_ref <- .("Reference")
                    plot_data <- rbind(
                        data.frame(Threshold = g$Threshold, NetBenefit = g$NetBenefit, Strategy = g$Predictor,
                                   Type = lab_model, stringsAsFactors = FALSE),
                        data.frame(Threshold = cv$thresholds, NetBenefit = cv$treat_all, Strategy = .("Treat all"),
                                   Type = lab_ref, stringsAsFactors = FALSE),
                        data.frame(Threshold = cv$thresholds, NetBenefit = 0, Strategy = .("Treat none"),
                                   Type = lab_ref, stringsAsFactors = FALSE)
                    )

                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Threshold, y = NetBenefit, color = Strategy, linetype = Type)) +
                        ggplot2::geom_line(linewidth = 1) +
                        # after the curves, so layer 1 stays the curves
                        ggplot2::geom_vline(xintercept = cv$utility, linetype = "dashed", colour = "gray50") +
                        ggplot2::labs(
                            x = .("Threshold probability"),
                            y = .("Net Benefit"),
                            title = .("Decision Curve Analysis"),
                            subtitle = private$.curveSubtitle(cv),
                            caption = paste(c(.fmt(.("Dashed line: the utility threshold used in the tables ({pt})"), pt = cv$utility),
                                              private$.curveExcluded(cv)), collapse = "\n"),
                            color = .("Strategy"),
                            linetype = .("Type")
                        ) +
                        ggplot2::coord_cartesian(ylim = c(-0.05, max(c(cv$prevalence, g$NetBenefit), na.rm = TRUE) + 0.05)) +
                        ggtheme +
                        private$.plotThemeFor() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9),
                            legend.position = "bottom",
                            # stacked: side by side, the Strategy and Type legends ran past the right
                            # edge at the default size and clipped "Reference" (2026-10-21)
                            legend.box = "vertical"
                        )

                    print(p)
                    TRUE
                },
                error = function(e) {
                    # A renderer cannot raise a notice - see .plotMessage().
                    private$.plotMessage(
                        ggtheme,
                        .("Clinical Utility Plot Error"),
                        sprintf(.("Failed to create the clinical utility plot: %s"), conditionMessage(e))
                    )
                }
            )
        },
        .populateInternalValidation = function() {
            if (!self$options$internalValidation) {
                return()
            }

            # F-16: 'for a single raw biomarker, optimism ... is minimal' was false for weak markers.
            # With one marker the model fits only the direction (slope sign), and the data choose it:
            # true optimism of a null marker is 0.102 at 10 + 10, 0.071 at 20 + 20 and 0.032 at
            # 100 + 100, but 0.004 at a true AUC of 0.70 with 20 + 20 (review simulations).
            private$.addNotice(
                type = "INFO",
                title = .("Internal Validation Note"),
                content = .("The correction shown is the optimism of a logistic regression of the outcome on each marker, fitted to these data. With one marker the only part of that model that changes the AUC is its direction (the sign of the slope), which these data choose. For a clearly discriminating marker the optimism is negligible, but for a weak marker in a small sample it can be several AUC points: about 0.07 for a marker carrying no information with 20 positive and 20 negative patients.")
            )

            val_method <- self$options$validationMethod

            # The optimism-correction resample count is capped at 200 for runtime. A user who
            # set Bootstrap samples to 2000 for a publication silently got 200, and the panel
            # printed the seed but not B - so the run was not reproducible from what is shown.
            validation_B <- min(self$options$bootstrapSamples %||% 100, 200)
            summary_text <- paste0("<h3>", jmvcore::htmlEscape(.("Internal Validation")), " (", private$.safeHtmlOutput(val_method), ")</h3>",
                                   "<p>", jmvcore::htmlEscape(.fmt(.("Random seed: {seed}"), seed = self$options$seed)), "</p>")
            if (val_method == "bootstrap" || val_method == "both") {
                summary_text <- paste0(summary_text, "<p>",
                    jmvcore::htmlEscape(.fmt(
                        .("Optimism correction uses {b} bootstrap resamples (capped at 200 regardless of the Bootstrap samples setting, which is {req})."),
                        b = validation_B, req = self$options$bootstrapSamples %||% 100)),
                    "</p>")
            }

            data <- private$.analysisData
            outcome <- data[[private$.outcome]]
            y_binary <- as.numeric(outcome == levels(outcome)[2])
            n <- length(y_binary)
            # F-25: a one-marker logistic model ranks patients by sign(slope) * x (.slopeSign());
            # a zero slope ranks every patient equal. The same fitted model as calibration and the
            # decision curves (.riskProbabilities()), and .riskNotices() discloses a reversal.
            slope_sign <- private$.slopeSign
            # A model points one way by construction, so every AUC reads it with direction "<";
            # "auto" let a small resample flip it and inflate the validated AUC.
            lp_auc <- function(y, score) as.numeric(pROC::roc(y, score, direction = "<", quiet = TRUE)$auc)
            # fixed decimals: jmvcore's format() prints 0.0001 as "1e-04"
            f3 <- function(x) sprintf("%.3f", x)
            f4 <- function(x) sprintf("%.4f", x)
            failed <- function(predictor, e) paste0("<p>", jmvcore::htmlEscape(.fmt(
                .("Validation failed for {pred}: {msg}"), pred = predictor, msg = conditionMessage(e))), "</p>")

            # F-24: 10 folds whenever both outcome groups have 10 patients, otherwise one fold per
            # patient of the smaller group, so that every test fold holds both groups and has an
            # AUC (a fold without a case used to abort the panel, bootstrap line included).
            n_min <- min(sum(y_binary == 1), sum(y_binary == 0))
            k <- min(10L, n_min)
            if (val_method %in% c("cv", "both")) {
                if (k < 2L) {
                    private$.addNotice(type = "WARNING", title = .("Cross-Validation Not Run"),
                        content = sprintf(.("Cross-validation needs at least 2 patients in each outcome group, so that every fold holds both groups; the smaller group has %s. No cross-validated AUC is shown."), n_min))
                } else if (k < 10L) {
                    private$.addNotice(type = "INFO", title = .("Fewer Cross-Validation Folds"),
                        content = sprintf(.("The smaller outcome group has %1$s patients, so cross-validation used %2$s folds instead of 10: every fold must hold patients from both groups for its AUC to exist. With so few patients per fold the fold AUCs vary widely."), n_min, k))
                }
            }

            boot_shown <- FALSE
            for (predictor in names(private$.rocResults)) {
                pred_vals <- data[[predictor]]
                pred_text <- ""

                # F-24: each method has its own error handler, so a CV failure no longer discards
                # the bootstrap result.
                if (val_method == "bootstrap" || val_method == "both") {
                    pred_text <- paste0(pred_text, tryCatch({
                        # Harrell's (2015) optimism correction of a logistic model of the marker:
                        # corrected = the FULL-DATA model's apparent AUC - mean optimism, where a
                        # resample's optimism = its model's AUC on the resample - that model's AUC
                        # on the original rows. It used to start from the marker's AUC under
                        # Direction, which the model need not share (VAL-13: 0.222 - 0.006 for a
                        # marker read backwards).
                        B <- validation_B
                        optimism_vals <- numeric(B)
                        full_sign <- slope_sign(y_binary, pred_vals)
                        app_roc <- pROC::roc(y_binary, full_sign * pred_vals, direction = "<", ci = TRUE,
                                             conf.level = self$options$confidenceLevel / 100, quiet = TRUE)
                        apparent_auc <- as.numeric(app_roc$auc)

                        private$.withSeed(for (i in 1:B) {
                            indices <- sample(1:n, replace = TRUE)
                            y_boot <- y_binary[indices]
                            # F-24: a resample holding one outcome group has no AUC (pROC stops,
                            # which aborted the whole panel); it is dropped and counted.
                            if (length(unique(y_boot)) < 2L) {
                                optimism_vals[i] <- NA
                                next
                            }
                            s <- slope_sign(y_boot, pred_vals[indices])
                            optimism_vals[i] <- lp_auc(y_boot, s * pred_vals[indices]) - lp_auc(y_binary, s * pred_vals)
                        })
                        dropped <- sum(is.na(optimism_vals))
                        if (dropped == B) stop(.("no bootstrap resample held both outcome groups"))

                        mean_optimism <- mean(optimism_vals, na.rm = TRUE)
                        corrected_auc <- apparent_auc - mean_optimism
                        sd_optimism <- sd(optimism_vals, na.rm = TRUE)
                        # The SD of the per-resample optimism is not a standard error of the
                        # corrected AUC. The interval shown is the apparent AUC's own DeLong
                        # interval, labelled so; none is shown for the corrected AUC (see the
                        # note after the loop). One msgid per sentence so a translator can
                        # reorder it; no " [" (jmvcore's Translator truncates there).
                        ci <- suppressWarnings(as.numeric(app_roc$ci))
                        boot_line <- if (length(ci) == 3 && all(is.finite(ci)) && ci[3] > ci[1]) {
                            .fmt(.("{pred} (Bootstrap): Apparent AUC = {app} (logistic model of the marker; {level}% DeLong interval of the apparent AUC {lo} to {hi}), Optimism = {opt} (SD across resamples {sd}), Corrected AUC = {corr}"),
                                 pred = predictor, app = f3(apparent_auc), level = self$options$confidenceLevel,
                                 lo = f3(ci[1]), hi = f3(ci[3]), opt = f4(mean_optimism),
                                 sd = f4(sd_optimism), corr = f3(corrected_auc))
                        } else {
                            # 0 at perfect separation, undefined (NaN) with a single case
                            .fmt(.("{pred} (Bootstrap): Apparent AUC = {app} (logistic model of the marker; no DeLong interval, its standard error is 0 or undefined), Optimism = {opt} (SD across resamples {sd}), Corrected AUC = {corr}"),
                                 pred = predictor, app = f3(apparent_auc), opt = f4(mean_optimism),
                                 sd = f4(sd_optimism), corr = f3(corrected_auc))
                        }
                        out <- paste0("<p>", jmvcore::htmlEscape(boot_line), "</p>")
                        if (dropped > 0) {
                            out <- paste0(out, "<p>", jmvcore::htmlEscape(.fmt(
                                .("{dropped} of {b} resamples held only one outcome group and were dropped; the optimism is the mean over the other {kept}."),
                                dropped = as.integer(dropped), b = as.integer(B), kept = as.integer(B - dropped))), "</p>")
                        }
                        marker_auc <- as.numeric(private$.rocResults[[predictor]]$roc$auc)
                        if (abs(apparent_auc - marker_auc) > 1e-9) {
                            out <- paste0(out, "<p>", jmvcore::htmlEscape(.fmt(
                                .("The model's apparent AUC differs from the AUC of {pred} in the AUC table ({auc}) because the model points the marker the way these data do (the sign of its fitted slope), while the AUC table reads it in the direction set under Direction."),
                                pred = predictor, auc = f3(marker_auc))), "</p>")
                        }
                        boot_shown <- TRUE
                        out
                    }, error = function(e) failed(predictor, e)))
                }

                if ((val_method == "cv" || val_method == "both") && k >= 2L) {
                    pred_text <- paste0(pred_text, tryCatch({
                        # Stratified CV: each group is spread over the k folds, so with
                        # k <= the smaller group's size every test fold holds both groups.
                        pos_idx <- which(y_binary == 1)
                        neg_idx <- which(y_binary == 0)
                        folds <- integer(n)
                        private$.withSeed({
                            folds[pos_idx] <- sample(rep(1:k, length.out = length(pos_idx)))
                            folds[neg_idx] <- sample(rep(1:k, length.out = length(neg_idx)))
                        })

                        cv_aucs <- numeric(k)
                        for (i in 1:k) {
                            test_idx <- which(folds == i)
                            train_idx <- which(folds != i)
                            # the training-fold model, applied to the test fold
                            s <- slope_sign(y_binary[train_idx], pred_vals[train_idx])
                            cv_aucs[i] <- lp_auc(y_binary[test_idx], s * pred_vals[test_idx])
                        }

                        # F-26: the SD of the fold AUCs describes how they scatter, not the
                        # uncertainty of their mean (it understates it); 3 decimals, as the AUCs.
                        paste0("<p>", jmvcore::htmlEscape(.fmt(
                            .("{pred} ({k}-fold CV): Mean AUC = {mean} (SD across folds {sd}, not a standard error)"),
                            pred = predictor, k = as.integer(k),
                            mean = f3(mean(cv_aucs)), sd = f3(sd(cv_aucs))
                        )), "</p>")
                    }, error = function(e) failed(predictor, e)))
                }

                summary_text <- paste0(summary_text, pred_text)
            }
            if (boot_shown) {
                summary_text <- paste0(summary_text, "<p>", jmvcore::htmlEscape(
                    .("No interval is given for the corrected AUC. The DeLong interval shown belongs to the apparent AUC; intervals that ignore the optimism cover the model's performance in new patients less often than stated, and Noma et al. (2021) describe bootstrap intervals for the corrected value.")), "</p>")
            }

            # Append validation results to existing analysis summary (not overwrite)
            separator <- if (nchar(private$.analysisSummaryHtml) > 0) "<br>" else ""
            private$.analysisSummaryHtml <- paste0(private$.analysisSummaryHtml, separator, summary_text)
            self$results$results$analysisSummary$setContent(private$.analysisSummaryHtml)
        },

        # F-34: jmvcore writes a Variable(s) value bare (`outcome = Tumour status`), which does
        # not parse for a non-syntactic column name. Such names are written as strings, which the
        # wrapper's resolveQuo() returns as they are (it keeps the backticks of vars(`a b`), so
        # backticks do not round-trip). Syntactic names keep jmvcore's bare form.
        .sourcifyOption = function(option) {
            value <- option$value
            if ((inherits(option, "OptionVariable") || inherits(option, "OptionVariables")) &&
                is.character(value) && length(value) > 0L && any(make.names(value) != value)) {
                quoted <- encodeString(value, quote = '"')
                return(paste0(option$name, " = ",
                    if (length(quoted) == 1L) quoted else paste0("c(", paste(quoted, collapse = ", "), ")")))
            }
            # jmvcore's sourcify() quotes free text and level labels (OptionLevel is an OptionString)
            # without escaping them, so a `"` closed the literal and the rest of the value became R
            # code in the syntax pane. Skipped at its default, as jmvcore's own method does.
            # OptionVariable is an OptionString too: a syntactic name keeps the bare form (F-34).
            if (inherits(option, "OptionString") && !inherits(option, "OptionVariable") &&
                is.character(value) && length(value) == 1L && !identical(value, option$default))
                return(paste0(option$name, " = ", encodeString(value, quote = '"')))
            super$.sourcifyOption(option)
        }
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for enhancedROC analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            outcome <- self$options$outcome
            predictors <- self$options$predictors

            if (is.null(outcome) || is.null(predictors) || length(predictors) == 0) {
                return("")
            }

            # Get arguments
            args <- ""
            if (!is.null(private$.asArgs)) {
                # .asArgs() starts with "\n    "; untrimmed it left a blank line after `data = data,`
                args <- trimws(private$.asArgs(incData = FALSE), which = "left")
            }
            if (args != "") {
                args <- paste0(",\n    ", args)
            }

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath" # fallback

            # Build complete function call
            paste0(pkg_name, "::enhancedROC(\n    data = data", args, ")")
        }
    ) # End of public list
)
