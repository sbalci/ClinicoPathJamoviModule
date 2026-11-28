#' @title Box-Violin Plots to Compare Within Groups
#'
#' @description
#' Creates violin plots for within-subjects (repeated measures) analysis using
#' ggstatsplot::ggwithinstats. Compares 2-4 measurements from the same subjects
#' with statistical testing and pairwise comparisons. Ideal for biomarker tracking,
#' treatment response monitoring, and longitudinal clinical studies.
#'
#' @param data Data frame in wide format (one row per subject, columns for time points)
#' @param dep1 First measurement variable (required, numeric)
#' @param dep2 Second measurement variable (required, numeric)
#' @param dep3 Third measurement variable (optional, numeric)
#' @param dep4 Fourth measurement variable (optional, numeric)
#' @param typestatistics Type of statistical test: "parametric" (repeated measures ANOVA),
#'   "nonparametric" (Friedman test), "robust" (trimmed means), or "bayes" (Bayesian)
#' @param pairwisecomparisons Logical, whether to display pairwise comparisons between
#'   time points (default: FALSE)
#' @param pairwisedisplay Which pairwise comparisons to show: "significant",
#'   "non-significant", or "everything" (default: "significant")
#' @param padjustmethod P-value adjustment method for multiple comparisons:
#'   "holm" (default), "hochberg", "bonferroni", "BH", "fdr", etc.
#' @param effsizetype Effect size type for parametric tests: "biased" (Cohen's d),
#'   "unbiased" (Hedge's g), "eta", or "omega"
#' @param centralityplotting Logical, whether to display mean/median trend lines
#' @param centralitytype Type of centrality measure: "parametric" (mean),
#'   "nonparametric" (median), "robust" (trimmed mean), or "bayes"
#' @param pointpath Logical, whether to show individual subject trajectories
#' @param centralitypath Logical, whether to connect centrality points across measurements
#' @param violin Logical, display violin plot layer
#' @param boxplot Logical, display box plot layer
#' @param point Logical, display individual data points
#' @param mytitle Plot title (default: "Within Group Comparison")
#' @param xtitle X-axis label (default: auto from measurement names)
#' @param ytitle Y-axis label (default: auto from value variable)
#' @param originaltheme Logical, use ggstatsplot theme (TRUE) or jamovi theme (FALSE)
#' @param resultssubtitle Logical, display statistical results as subtitle
#' @param bfmessage Logical, display Bayes factor caption when Statistical results is enabled with Parametric tests
#' @param conflevel Confidence level for intervals (default: 0.95)
#' @param k Number of decimal places for statistics (default: 2)
#' @param plotwidth Plot width in pixels (default: 650)
#' @param plotheight Plot height in pixels (default: 450)
#' @param clinicalpreset Advisory clinical guidance preset: "custom", "biomarker",
#'   "treatment", or "laboratory" (emits checklist guidance without overriding test settings)
#' @param addGGPubrPlot Logical, add publication-ready ggpubr variant plot
#' @param ggpubrPlotType Type of ggpubr plot: "boxplot", "violin", "paired", or "line"
#' @param ggpubrPalette Color palette for ggpubr: "jco", "npg", "aaas", "lancet", etc.
#' @param ggpubrAddStats Logical, add statistical comparisons to ggpubr plot
#' @param ggpubrShowLines Logical, show connecting lines in paired plot
#' @param ggpubrAddPoints Logical, overlay individual points on ggpubr plot
#' @param showExplanations Logical, display explanations of statistical methods
#'
#' @return A jamovi analysis object with violin plots, statistical tests, and
#'   clinical interpretation
#'
#' @details
#' **Data Requirements:**
#' - Wide format required (one row per independent subject/case; do not stack multiple cores, lesions, or readers per subject)
#' - Each column represents a different time point or condition
#' - Complete finite data required across selected measurements (listwise deletion)
#' - Minimum 3 subjects with complete data across all measurements
#'
#' **Statistical Tests:**
#' - Parametric: Paired t-test (2 measurements) or Greenhouse-Geisser repeated-measures ANOVA (3-4 measurements)
#' - Nonparametric: Wilcoxon signed-rank test (2 measurements) or Friedman test with Durbin-Conover post-hoc (3-4 measurements)
#' - Robust: 20%-trimmed means (Yuen's paired test or Wilcox heteroscedastic repeated-measures ANOVA)
#' - Bayesian: Default Cauchy-prior Bayes factor (prior scale 0.707)
#'
#' **Clinical Guidance Presets (Advisory Only):**
#' - Biomarker: Checklist guidance for skewed biomarker tracking (suggests Nonparametric)
#' - Treatment: Checklist guidance for multi-visit response monitoring (suggests Pairwise comparisons)
#' - Laboratory: Checklist guidance for outlier-prone lab values (suggests Robust)
#'
#' @section Performance Optimization:
#' The function implements sophisticated caching:
#' - Data preparation cached based on variable selection and data content
#' - Options cached separately to minimize reprocessing
#' - Plot state management prevents unnecessary regeneration
#' - Checkpoint calls before expensive operations for responsiveness
#'
#' @section Clinical Validation:
#' The function performs comprehensive data quality checks:
#' - Validates paired design requirements (complete cases)
#' - Detects small sample sizes (< 10 subjects)
#' - Identifies potential outliers (> 10% outliers)
#' - Warns about skewed data for parametric tests
#' - Alerts to high missing data rates (> 50%)
#'
#' @examples
#' \dontrun{
#' # Basic within-subjects analysis
#' data(iris)
#' iris_wide <- data.frame(
#'     Subject = 1:50,
#'     Baseline = iris$Sepal.Length[1:50],
#'     Month3 = iris$Sepal.Width[1:50] * 2.5,
#'     Month6 = iris$Petal.Length[1:50] * 1.8
#' )
#'
#' jjwithinstats(
#'     data = iris_wide,
#'     dep1 = "Baseline",
#'     dep2 = "Month3",
#'     dep3 = "Month6",
#'     typestatistics = "parametric",
#'     pairwisecomparisons = TRUE,
#'     centralityplotting = TRUE,
#'     pointpath = TRUE
#' )
#'
#' # Clinical biomarker tracking (nonparametric)
#' jjwithinstats(
#'     data = clinical_data,
#'     dep1 = "CRP_baseline",
#'     dep2 = "CRP_week4",
#'     dep3 = "CRP_week12",
#'     clinicalpreset = "biomarker",
#'     typestatistics = "nonparametric",
#'     pairwisecomparisons = TRUE,
#'     mytitle = "C-Reactive Protein Levels During Treatment"
#' )
#'
#' # Robust analysis with publication plot
#' jjwithinstats(
#'     data = lab_data,
#'     dep1 = "ALT_pre",
#'     dep2 = "ALT_post",
#'     typestatistics = "robust",
#'     centralityplotting = TRUE,
#'     centralitytype = "robust",
#'     addGGPubrPlot = TRUE,
#'     ggpubrPlotType = "paired",
#'     ggpubrAddStats = TRUE
#' )
#' }
#'
#' @references
#' Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot' approach.
#'   Journal of Open Source Software, 6(61), 3167. \doi{10.21105/joss.03167}
#'
#' @seealso
#' \code{\link[ggstatsplot]{ggwithinstats}} for the underlying plotting function
#'
#' @family JJStatsPlot functions
#' @keywords hplot htest
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @importFrom rlang sym
#' @importFrom digest digest
#' @importFrom ggstatsplot ggwithinstats theme_ggstatsplot
#' @importFrom ggpubr ggpaired ggboxplot ggviolin ggline stat_compare_means theme_pubr
jjwithinstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjwithinstatsClass",
    inherit = jjwithinstatsBase,
    private = list(
        # Set by .subtitleExpr when the takeover was attempted and failed.
        .subtitleFallback = NULL,

        # Fixed seed for the sampling-based paths (Bayesian MCMC, robust
        # bootstrap), so the same analysis reports the same numbers on every
        # render rather than drifting between refreshes.

        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .messages = NULL,
        .info_messages = NULL,
        .data_messages = NULL,
        .data_info_messages = NULL,
        .subtitle_cache = NULL,
        .subtitle_cache_key = NULL,

        # Shared welcome / getting-started message (used by .init and .run) ----
        .welcomeMessage = function() {
            paste0(
                "<br><strong>Welcome to ClinicoPath</strong><br><br>",
                "This tool generates Violin Plots for repeated measurements (e.g., biomarker levels over time, treatment responses).<br><br>",
                "<strong>Data Format:</strong> Wide format required - each row = one subject, columns = separate time points/measurements.<br><br>",
                "<strong>Requirements:</strong><br>",
                "\u2022 Select at least 2 measurements (First and Second Measurement fields)<br>",
                "\u2022 At least 3 independent subjects with complete finite values across all selected measurements (incomplete rows are excluded listwise)<br>",
                "\u2022 Use for comparing 2-4 measurements from the same subjects<br><br>",
                "See documentation: <a href='https://www.indrapatil.com/ggstatsplot/reference/ggwithinstats.html' target='_blank'>ggwithinstats</a><br>",
                "Please cite jamovi and packages as instructed.<br><hr>"
            )
        },

        # init ----

        .init = function() {
            # Use configurable plot dimensions with smart scaling
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 650
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450
            
            # Only setup plot if variables are selected
            if (!is.null(self$options$dep1) && !is.null(self$options$dep2)) {
                # Calculate proportional scaling based on number of measurements
                num_measurements <- sum(!is.null(self$options$dep1), !is.null(self$options$dep2),
                                      !is.null(self$options$dep3), !is.null(self$options$dep4))
                
                if (num_measurements > 2) {
                    # Proportional scaling instead of magic numbers
                    scale_factor_width <- 1 + (num_measurements - 2) * 0.15  # 15% per additional measurement
                    scale_factor_height <- 1 + (num_measurements - 2) * 0.1   # 10% per additional measurement
                    plotwidth <- plotwidth * scale_factor_width
                    plotheight <- plotheight * scale_factor_height
                }
                
                self$results$plot$setSize(plotwidth, plotheight)
                
                # Apply clinical presets if selected
                private$.applyClinicalPresets()
                
                # Pre-prepare data and options for performance
                private$.prepareData(emit_messages = FALSE)
                private$.prepareOptions()
                
                # Clear any welcome message when variables are selected
                self$results$todo$setContent("")
            } else {
                # Hide plot when insufficient variables selected
                self$results$plot$setVisible(visible = FALSE)
                
                # Show welcome message when insufficient variables
                welcome_msg <- private$.welcomeMessage()

                self$results$todo$setVisible(visible = TRUE)
                self$results$todo$setContent(welcome_msg)
            }

        },
        
        # WARNING-grade messages only. These are the ones a user has to act on,
        # so the "Messages" panel is reserved for them; procedural progress notes
        # go to .accumulateInfoMessage() and are rendered under the summary
        # instead. When every run filled this panel with "Processing 3
        # measurements..." there was no visual difference between a healthy run
        # and one reporting 60% missing data.
        .accumulateMessage = function(message) {
            if (is.null(private$.messages)) {
                private$.messages <- character()
            }
            private$.messages <- append(private$.messages, message)

            # Render accumulated messages into the HTML warnings output
            if (!is.null(self$results$warnings)) {
                self$results$warnings$setContent(paste(private$.messages, collapse = ""))
                self$results$warnings$setVisible(TRUE)
            }
        },

        # INFO-grade progress notes (how many measurements, how many subjects
        # retained). Collected here and appended to the Analysis Summary by
        # .run(); never routed to the warning panel.
        .accumulateInfoMessage = function(message) {
            if (is.null(private$.info_messages)) {
                private$.info_messages <- character()
            }
            private$.info_messages <- append(private$.info_messages, message)
        },

        # Muted block appended to the Analysis Summary panel.
        .renderInfoBlock = function() {
            if (length(private$.info_messages) == 0) return("")
            paste0(
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding:8px; ",
                "margin-top:8px; color: inherit; font-size: 0.95em;'>",
                paste(private$.info_messages, collapse = ""),
                "</div>")
        },

        # Reset messages for new analysis run
        .resetMessages = function() {
            private$.messages <- character()
            private$.info_messages <- character()
            # Don't clear the todo panel here, as it might hold "Welcome" or "Ready"
            # warning content is cleared
            if (!is.null(self$results$warnings)) {
                self$results$warnings$setContent("")
                # Don't hide yet, wait until run to decide visibility or let specific checks set it
            }
        },

        # HTML sanitization for security
        .safeHtmlOutput = function(text) {
            if (is.null(text) || length(text) == 0) return("")
            htmltools::htmlEscape(as.character(text), attribute = TRUE)
        },

        # Helper to cache data-specific messages
        .accumulateDataMessage = function(message) {
            private$.data_messages <- append(private$.data_messages, message)
            private$.accumulateMessage(message)
        },

        # Same, for INFO-grade progress notes (cached separately so a cache hit
        # replays each message at its own severity).
        .accumulateDataInfoMessage = function(message) {
            private$.data_info_messages <- append(private$.data_info_messages, message)
            private$.accumulateInfoMessage(message)
        },

        .populatePairedEffects = function(prepared) {
            table <- self$results$pairedChanges
            .deleteTableRows(table)
            table$setVisible(isTRUE(self$options$pairedEffects))
            for (key in c("scope", "intervals", "testing", "ratios", "unavailable", "warnings"))
                table$setNote(key, "")
            if (!isTRUE(self$options$pairedEffects)) return()
            table$setNote("scope", .("Each row must be an independent case. Do not stack multiple readers of the same cases. Changes are later selected measurement minus earlier; all contrasts use the same complete cases across every selected measurement. A fixed measurement order cannot separate modality from period effects."))
            if (is.null(prepared)) {
                table$setNote("unavailable", .("Paired changes require at least three complete independent cases. See Messages for data exclusions."))
                return()
            }
            vars <- levels(prepared$measurement)
            ids <- unique(prepared$rowid)
            wide <- lapply(vars, function(v) {
                d <- prepared[prepared$measurement == v, , drop = FALSE]
                d$value[match(ids, d$rowid)]
            })
            names(wide) <- vars
            wide <- as.data.frame(wide, check.names = FALSE)
            effects <- tryCatch(.jjWithinPairedEffects(
                wide, estimator = self$options$pairedEstimator,
                ci_method = self$options$pairedCIMethod,
                confidence = self$options$conflevel, samples = self$options$pairedBootstrap,
                seed = self$options$seed, ratios = self$options$pairedTimeRatios,
                checkpoint = private$.checkpoint), error = function(e) e)
            if (inherits(effects, "error")) {
                table$setNote("unavailable", sprintf(
                    .("Paired changes could not be calculated: %s"),
                    private$.safeHtmlOutput(conditionMessage(effects))))
                return()
            }
            pvalues <- .jjWithinPairedPValues(
                wide, type = self$options$typestatistics,
                pairwise = self$options$pairwisecomparisons,
                adjustment = self$options$padjustmethod)
            for (i in seq_len(nrow(effects))) {
                e <- effects[i, ]
                p <- NULL
                test <- ""
                if (!e$ratio && !is.null(pvalues)) {
                    index <- which(pvalues$first == e$first & pvalues$second == e$second)
                    if (length(index) == 1L) {
                        if (is.finite(pvalues$p[index])) p <- pvalues$p[index]
                        test <- pvalues$test[index]
                    }
                }
                table$addRow(rowKey = i, values = list(
                    contrast = paste(e$second, if (e$ratio) "/" else "\u2212", e$first),
                    effect = .(e$effect), pairs = e$n,
                    units = if (e$ratio) .("ratio") else self$options$pairedUnits,
                    estimate = if (is.finite(e$estimate)) e$estimate else NULL,
                    lower = if (is.finite(e$lower)) e$lower else NULL,
                    upper = if (is.finite(e$upper)) e$upper else NULL,
                    method = .(e$method), test = test, p = p, note = .(e$note)))
            }
            used_boot <- any(grepl("bootstrap", effects$method, ignore.case = TRUE))
            boot_caution <- if (used_boot && nrow(wide) < 15L)
                paste0(" ", .("Percentile bootstrap intervals can under-cover nominal confidence levels when fewer than 15 complete pairs are available."))
            else ""
            table$setNote("intervals", paste0(sprintf(
                .("Intervals are pointwise %.1f%% intervals, without multiplicity adjustment. Bootstrap intervals (when selected or for medians) use %d whole-case resamples (seed %d); a constant resampling distribution does not provide an interval. The median paired change differs from the difference of marginal medians. The Hodges-Lehmann estimate is the median Walsh average of paired differences; its shift interpretation assumes symmetry."),
                100 * self$options$conflevel, self$options$pairedBootstrap, self$options$seed), boot_caution))
            table$setNote("testing", sprintf(
                .("The p column reuses the selected paired test, not a separate test of the displayed estimator or time ratio. With three or more measurements it is available only when Pairwise comparisons is enabled, with adjustment '%s' across all measurement pairs; nonparametric post-hoc tests are Durbin-Conover. With two measurements it is the unadjusted paired t or signed-rank test. Robust/Bayesian tests remain in the plots; their p-values are not supplied here."),
                self$options$padjustmethod))
            if (isTRUE(self$options$pairedTimeRatios))
                table$setNote("ratios", .("The time ratio is exp(mean(log(later) - log(earlier))), the geometric mean of within-case ratios. It is not the ratio of arithmetic means. Ratios below 1 indicate shorter later measurements. Each contrast must contain only positive values; no additional pairs are dropped to obtain a ratio."))
            if (!is.null(pvalues) && length(attr(pvalues, "warnings")) > 0L)
                table$setNote("warnings", private$.safeHtmlOutput(
                    paste(attr(pvalues, "warnings"), collapse = " ")))
            if (is.null(pvalues) && self$options$typestatistics %in% c("parametric", "nonparametric") &&
                (length(vars) == 2L || isTRUE(self$options$pairwisecomparisons)))
                table$setNote("unavailable", .("The selected paired test could not be evaluated; p-values are unavailable. Effect estimates and confidence intervals are reported independently."))
        },
        
        # Name of the test that is ACTUALLY run. With TWO measurements the
        # analysis is a paired t-test / Wilcoxon signed-rank, not an ANOVA; the
        # summary panel called every configuration a "Repeated measures ANOVA",
        # so a two-timepoint comparison was described as an ANOVA in text offered
        # for a methods section.
        .testLabel = function(typestatistics, num_measurements) {
            two <- !is.na(num_measurements) && num_measurements <= 2
            switch(typestatistics,
                "parametric"    = if (two) .("Paired samples t-test") else .("Repeated measures ANOVA"),
                "nonparametric" = if (two) .("Wilcoxon signed-rank test") else .("Friedman test"),
                "robust"        = if (two) .("Robust paired test (Yuen's trimmed means)") else .("Robust repeated measures test"),
                "bayes"         = if (two) .("Bayesian paired samples t-test") else .("Bayesian repeated measures analysis"),
                .("Within-subjects comparison"))
        },

        # ggstatsplot 1.0.0 removed `pairwise.comparisons`; "none" is how the
        # significance brackets are switched off now. Without this, unticking the
        # box left the brackets on the plot.
        .pairwiseDisplay = function(opts) {
            if (isTRUE(opts$pairwisecomparisons)) opts$pairwisedisplay else "none"
        },

        # Build the plot subtitle ourselves.
        #
        # ggstatsplot 1.0.0 also removed `effsize.type` from ggwithinstats, so
        # the effect-size selector had no effect: all four choices produced
        # Hedges' g. statsExpressions - which ggstatsplot calls internally - still
        # honours it, and crucially still applies the Greenhouse-Geisser
        # sphericity correction for 3+ measurements (verified: the takeover
        # reproduces F_Fisher(1.79, 69.71) = 94.54, p = 1.54e-19 exactly, the
        # same fractional degrees of freedom the untouched plot shows).
        #
        # Returns NULL when we should not take over, in which case the caller
        # leaves ggstatsplot to build its own subtitle.
        # Memoised wrapper. .run() probes the takeover to decide whether to warn
        # the user, and .plot() needs the expression itself; without this the
        # robust bootstrap and the Bayesian sampler ran twice per cycle.
        .subtitleExpr = function(long_data, opts, num_measurements) {
            key <- paste(private$.data_hash, private$.options_hash,
                         num_measurements, sep = "|")
            if (identical(private$.subtitle_cache_key, key)) {
                private$.subtitleFallback <- private$.subtitle_cache$fallback
                return(private$.subtitle_cache$expr)
            }
            private$.subtitleFallback <- NULL
            expr <- private$.computeSubtitleExpr(long_data, opts, num_measurements)
            private$.subtitle_cache <- list(expr = expr,
                                            fallback = private$.subtitleFallback)
            private$.subtitle_cache_key <- key
            expr
        },

        .computeSubtitleExpr = function(long_data, opts, num_measurements) {
            if (!isTRUE(opts$resultssubtitle)) return(NULL)
            if (is.na(num_measurements) || num_measurements < 2) return(NULL)
            # Leave the Bayesian path to ggstatsplot: taking the subtitle over
            # sets results.subtitle = FALSE, which also suppresses the
            # bf.message caption, silently disabling the 'Bayes factor message'
            # checkbox.
            if (identical(opts$typestatistics, "bayes")) return(NULL)
            # Taking the subtitle over sets results.subtitle = FALSE, which also
            # removes ggstatsplot's Bayes-factor caption. If the user asked for
            # that caption, leave the whole subtitle to ggstatsplot rather than
            # silently dropping it.
            if (isTRUE(opts$bfmessage)) {
                private$.subtitleFallback <-
                    .("the Bayes factor message is switched on, which ggstatsplot renders alongside its own subtitle")
                return(NULL)
            }

            # two_sample_test accepts only g/d/unbiased/biased; the ANOVA-only
            # names error, so map them onto the equivalent two-group family.
            eff <- opts$effsizetype
            if (num_measurements <= 2) {
                if (identical(eff, "eta"))   eff <- "biased"
                if (identical(eff, "omega")) eff <- "unbiased"
            }

            fn <- if (num_measurements <= 2) statsExpressions::two_sample_test
                  else                       statsExpressions::oneway_anova

            # Seed HERE rather than at the call site. The Bayesian sampler and
            # the robust bootstrap are stochastic, and this is reached from two
            # places: .run()'s takeover probe and .plot(). Only .plot() used to
            # set the seed, so once the result became memoised the cached value
            # came from whichever caller ran first - the unseeded probe - and
            # the "same analysis reports the same numbers" guarantee was lost.
            if (statsSeedMatters(opts$typestatistics, "within", num_measurements))
                withr::local_seed(self$options$seed)

            res <- tryCatch(
                rlang::inject(fn(
                    data         = long_data,
                    x            = !!rlang::sym("measurement"),
                    y            = !!rlang::sym("value"),
                    subject.id   = !!rlang::sym("rowid"),
                    paired       = TRUE,
                    type         = opts$typestatistics,
                    effsize.type = eff,
                    digits       = opts$digits,
                    conf.level   = opts$conflevel)),
                error = function(e) e)

            if (inherits(res, "condition")) {
                private$.subtitleFallback <- conditionMessage(res)
                return(NULL)
            }
            if (is.null(res$expression) || length(res$expression) == 0) return(NULL)
            res$expression[[1]]
        },

        # Apply clinical presets for common scenarios (advisory checklist only)
        .applyClinicalPresets = function() {
            if (is.null(self$options$clinicalpreset) || self$options$clinicalpreset == "custom") {
                return()  # No preset to apply
            }

            n_m <- sum(!is.null(self$options$dep1), !is.null(self$options$dep2),
                       !is.null(self$options$dep3), !is.null(self$options$dep4))
            cur_test <- private$.testLabel(self$options$typestatistics, n_m)

            switch(self$options$clinicalpreset,
                "biomarker" = {
                    if (self$options$typestatistics == "parametric") {
                        private$.accumulateMessage(.("Biomarker preset: Consider using Nonparametric test for skewed biomarker data<br>"))
                    }
                    private$.accumulateMessage(sprintf(
                        .("<strong>Biomarker tracking (guidance only):</strong> Presets do not change settings automatically (currently running: %s). Select 'Nonparametric' if biomarker distributions are skewed.<br>"),
                        private$.safeHtmlOutput(cur_test)))
                },
                "treatment" = {
                    if (!self$options$pairwisecomparisons) {
                        private$.accumulateMessage(.("Treatment preset: Enable pairwise comparisons to identify when treatment effects occur<br>"))
                    }
                    private$.accumulateMessage(sprintf(
                        .("<strong>Treatment response (guidance only):</strong> Presets do not change settings automatically (currently running: %s). Enable 'Pairwise comparisons' to inspect changes between specific visits.<br>"),
                        private$.safeHtmlOutput(cur_test)))
                },
                "laboratory" = {
                    if (self$options$typestatistics == "parametric") {
                        private$.accumulateMessage(.("Laboratory preset: Consider Robust test to handle outliers common in lab values<br>"))
                    }
                    if (!self$options$centralityplotting) {
                        private$.accumulateMessage(.("Laboratory preset: Enable centrality plotting to see overall trends<br>"))
                    }
                    private$.accumulateMessage(sprintf(
                        .("<strong>Laboratory values (guidance only):</strong> Presets do not change settings automatically (currently running: %s). Select 'Robust' to reduce outlier influence common in clinical laboratory values.<br>"),
                        private$.safeHtmlOutput(cur_test)))
                }
            )
        },

        # Shared numeric coercion helper: unwraps jamovi integer-backed factor
        # attributes (`values`) and haven_labelled vectors via jmvcore::toNumeric()
        # before falling back to character-to-numeric parsing.
        .coerceNumericCol = function(col) {
            num <- jmvcore::toNumeric(col)
            if (!is.numeric(num))
                num <- suppressWarnings(as.numeric(as.character(num)))
            num
        },
        
        # Enhanced input validation with comprehensive checks
        .validateInputs = function() {
            # Basic requirement check
            if (is.null(self$options$dep1) || is.null(self$options$dep2))
                return(FALSE)
            
            # Comprehensive data structure validation
            private$.validateDataStructure()
            
            # Variable-specific validation
            vars <- c(self$options$dep1, self$options$dep2, 
                      self$options$dep3, self$options$dep4)
            vars <- vars[!sapply(vars, is.null)]
            
            private$.validateVariableTypes(vars)
            
            return(TRUE)
        },
        
        # Comprehensive data structure validation
        .validateDataStructure = function() {
            if (!is.data.frame(self$data))
                jmvcore::reject(.("Data must be a data frame"))
            if (nrow(self$data) == 0)
                jmvcore::reject(.("Data contains no (complete) rows"))
            if (ncol(self$data) < 2)
                jmvcore::reject(.("Data must contain at least 2 columns"))
            if (!is.finite(nrow(self$data)) || !is.finite(ncol(self$data)))
                jmvcore::reject(.("Data dimensions are invalid"))
        },

        # Variable type and existence validation
        .validateVariableTypes = function(vars) {
            for (var in vars) {
                if (!(var %in% names(self$data)))
                    jmvcore::reject(.("Variable '{name}' not found in data"), name = var)

                # Check if variable can be converted to numeric (including jamovi integer-backed factors)
                test_vals <- self$data[[var]]
                if (!is.numeric(test_vals)) {
                    converted_vals <- private$.coerceNumericCol(test_vals)
                    if (all(is.na(converted_vals)) && !all(is.na(test_vals)))
                        jmvcore::reject(.("Variable '{name}' cannot be converted to numeric values"), name = var)
                }
            }
        },
        
        # Data quality validation helper (called AFTER complete-case listwise filtering)
        .validateDataQuality = function(mydata, vars) {
            # Checkpoint before expensive validation loop
            private$.checkpoint()
            
            for (var in vars) {
                num_vals <- private$.coerceNumericCol(mydata[[var]])
                num_vals <- num_vals[is.finite(num_vals)]
                # htmlEscape user column name before flowing into legacy warnings$setContent
                var_safe <- private$.safeHtmlOutput(var)

                if (length(num_vals) < 3) {
                    private$.accumulateDataMessage(
                        glue::glue(.("<br> <strong>Warning:</strong> {var_safe} has less than 3 valid observations<br>"))
                    )
                }
                if (length(unique(num_vals)) < 2) {
                    private$.accumulateDataMessage(
                        glue::glue(.("<br> <strong>Warning:</strong> {var_safe} has no variation (all values identical)<br>"))
                    )
                }
            }
            
            # Additional clinical misuse detection on complete analyzed subjects
            private$.detectClinicalMisuse(mydata, vars)
        },
        
        # Clinical misuse detection and guidance (evaluates complete-case subjects and paired differences)
        .detectClinicalMisuse = function(mydata, vars) {
            total_subjects <- nrow(mydata)
            
            # Small sample size warning on retained complete cases
            if (total_subjects < 10) {
                private$.accumulateDataMessage(
                    .("<br> <strong>Small Sample Size:</strong> With fewer than 10 subjects, results may be unreliable. Consider larger sample or descriptive analysis.<br>")
                )
            }
            
            pair_idx <- utils::combn(seq_along(vars), 2L)
            diffs_list <- lapply(seq_len(ncol(pair_idx)), function(j) {
                d <- private$.coerceNumericCol(mydata[[vars[pair_idx[2L, j]]]]) -
                     private$.coerceNumericCol(mydata[[vars[pair_idx[1L, j]]]])
                d[is.finite(d)]
            })

            # Check for potential outliers in within-subject paired differences
            for (diff_vals in diffs_list) {
                if (length(diff_vals) > 4) {
                    q75 <- stats::quantile(diff_vals, 0.75, na.rm = TRUE)
                    q25 <- stats::quantile(diff_vals, 0.25, na.rm = TRUE)
                    iqr <- q75 - q25
                    outliers <- sum(diff_vals > (q75 + 1.5 * iqr) | diff_vals < (q25 - 1.5 * iqr))
                    
                    if (outliers > length(diff_vals) * 0.1) {  # More than 10% outliers
                        private$.accumulateDataMessage(
                            .("<br> <strong>Many Outliers Detected:</strong> Consider Robust test type to reduce outlier influence<br>")
                        )
                        break  # Only show this message once
                    }
                }
            }
            
            # Check for substantially skewed within-subject differences under parametric tests
            if (self$options$typestatistics == "parametric") {
                for (diff_vals in diffs_list) {
                    if (length(diff_vals) > 5) {
                        # Unbiased Fisher-Pearson sample skewness (G1) on paired differences.
                        n_v <- length(diff_vals)
                        mean_val <- mean(diff_vals)
                        sd_val <- stats::sd(diff_vals)
                        skewness <- if (is.finite(sd_val) && sd_val > 0)
                            (n_v / ((n_v - 1) * (n_v - 2))) * sum(((diff_vals - mean_val) / sd_val)^3)
                        else
                            0

                        if (is.finite(skewness) && abs(skewness) > 1) {  # Substantially skewed
                            private$.accumulateDataMessage(
                                .("<br> <strong>Skewed Data Detected:</strong> Consider Nonparametric test for skewed biomarker or clinical data<br>")
                            )
                            break  # Only show this message once
                        }
                    }
                }
            }
        },

        # Optimized data preparation with robust caching
        .prepareData = function(force_refresh = FALSE, emit_messages = TRUE) {
            # IMMEDIATE CHECK: Return NULL if minimum variables not selected
            if (is.null(self$options$dep1) || is.null(self$options$dep2)) {
                return(NULL)
            }
            
            # Checkpoint before expensive hash computation for large datasets
            private$.checkpoint(flush = FALSE)

            # CRITICAL FIX: Create robust hash including ACTUAL DATA VALUES
            # Bug: Old code only hashed variable names and dimensions
            # Result: Editing data values didn't invalidate cache, showing outdated results
            vars <- Filter(Negate(is.null), c(self$options$dep1, self$options$dep2,
                                             self$options$dep3, self$options$dep4))

            # Extract actual data for selected variables using the RAW variable
            # names. data.frame column access requires the raw name; a
            # composeTerm()/backtick-quoted name (e.g. `CRP baseline`) yields an
            # "undefined columns selected" error for names with spaces/symbols.
            data_subset <- self$data[, vars, drop = FALSE]

            current_hash <- digest::digest(list(
                dep1 = self$options$dep1, dep2 = self$options$dep2,
                dep3 = self$options$dep3, dep4 = self$options$dep4,
                typestatistics = self$options$typestatistics,
                data_dim = dim(self$data), col_names = names(self$data),
                # CRITICAL FIX: Include actual data content in hash
                data_content = data_subset
            ), algo = "md5")
            
            # Only reprocess if data has changed or forced refresh
            if (!is.null(private$.prepared_data) &&
                private$.data_hash == current_hash &&
                !force_refresh) {
                # Re-emit cached data messages only when the caller wants them
                # (i.e. .run(), which calls .resetMessages() first). Render calls
                # (.plot()/.init()) pass emit_messages = FALSE so warnings are
                # not appended a second time and duplicated in the output.
                if (emit_messages) {
                    for (msg in private$.data_messages) {
                        private$.accumulateMessage(msg)
                    }
                    for (msg in private$.data_info_messages) {
                        private$.accumulateInfoMessage(msg)
                    }
                }
                return(private$.prepared_data)
            }
            
            # Reset data messages for new processing
            private$.data_messages <- character()
            private$.data_info_messages <- character()
            
            # Add processing feedback
            private$.accumulateDataInfoMessage(
                glue::glue(.("<br>Processing {length(vars)} measurements for within-subjects analysis.<br>"))
            )
            
            # Variables already validated above, proceed with data preparation
            mydata <- self$data[, vars, drop = FALSE]
            
            # Check for empty dataset
            if (nrow(mydata) == 0) {
                private$.accumulateDataMessage(
                    .("<br> Dataset is empty (0 rows). Please load data first.<br>")
                )
                private$.prepared_data <- NULL
                return(NULL)
            }
            
            # Two slots holding the same variable produce duplicate column names
            # and a raw "factor level [2] is duplicated" crash out of
            # pivot_longer(), reached uncaught from .init(). Reject it with a
            # message that says what to do about it.
            if (anyDuplicated(vars)) {
                dup <- unique(vars[duplicated(vars)])
                jmvcore::reject(
                    .("Each measurement must be a different variable ('{var}' is selected more than once)."),
                    var = paste(dup, collapse = ", "))
            }

            subject_ids <- seq_len(nrow(mydata))

            # Check if required variables exist in dataset (raw names)
            missing_vars <- vars[!vars %in% names(mydata)]
            if (length(missing_vars) > 0) {
                private$.accumulateDataMessage(
                    paste0(
                        "<br>",
                        .fmt(
                            .("Variables not found in the dataset: {vars}."),
                            vars = private$.safeHtmlOutput(paste(missing_vars, collapse = ", "))),
                        "<br>")
                )
                private$.prepared_data <- NULL
                return(NULL)
            }
            
            # Checkpoint before expensive data conversion loop
            private$.checkpoint()

            # Convert variables to numeric with labelled awareness
            non_numeric <- character()
            for (var in vars) {
                col <- mydata[[var]]

                # jmvcore::toNumeric() extracts the underlying numeric values,
                # including for haven_labelled columns (SPSS/Stata imports).
                # Do NOT route haven_labelled through haven::as_factor(levels =
                # "both") first: that replaces the real measurement values with
                # factor position codes (e.g. 10, 20, 30 -> 1, 2, 3), silently
                # corrupting the within-subjects analysis.
                num <- private$.coerceNumericCol(col)
                if (all(is.na(num)) && !all(is.na(col)))
                    non_numeric <- c(non_numeric, var)

                mydata[[var]] <- num
            }
            if (length(non_numeric)) {
                jmvcore::reject(
                    .("Each measurement must be a numeric variable ('{var}' contains no usable numbers)."),
                    var = paste(non_numeric, collapse = ", "))
            }
            
            # Enhanced validation for optional parameters
            if (!is.null(self$options$dep3) && is.null(self$options$dep2)) {
                jmvcore::reject(.('Second measurement required when third is specified'))
            }
            if (!is.null(self$options$dep4) && is.null(self$options$dep3)) {
                jmvcore::reject(.('Third measurement required when fourth is specified'))
            }

            # CRITICAL FIX: Validate paired design structure
            # Within-subjects analysis requires complete data for all subjects across measurements
            # Check for excessive missing data that would break paired tests
            n_rows <- nrow(mydata)
            complete_cases <- sum(Reduce(`&`, lapply(mydata[, vars, drop = FALSE], is.finite)))
            missing_pct <- (1 - complete_cases/n_rows) * 100

            if (complete_cases < 3) {
                warning_msg <- paste0(
                    "<div style='background-color: rgba(255, 202, 33, 0.23); border-left:4px solid #ff9800; padding:15px; margin:10px 0; color: inherit;'>",
                    "<h4 style='color:#ff6f00; margin-top:0;'> Insufficient Complete Cases for Paired Analysis</h4>",
                    "<p><strong>Within-subjects analysis requires complete data across all measurements.</strong></p>",
                    "<p>Current status:</p>",
                    "<ul>",
                    "<li>Total subjects: ", n_rows, "</li>",
                    "<li>Complete cases (all measurements present): ", complete_cases, "</li>",
                    "<li>Missing data: ", round(missing_pct, 1), "%</li>",
                    "</ul>",
                    "<p><strong>Minimum required:</strong> At least 3 subjects with complete data across all measurements.</p>",
                    "</div>"
                )
                private$.accumulateDataMessage(warning_msg)
                private$.prepared_data <- NULL
                return(NULL)
            }

            if (missing_pct > 50) {
                warning_msg <- paste0(
                    "<div style='background-color: rgba(255, 202, 33, 0.23); border-left:4px solid #ffc107; padding:15px; margin:10px 0; color: inherit;'>",
                    "<h4 style='color:#ff9800; margin-top:0;'> High Missing Data Rate</h4>",
                    "<p><strong>Warning:</strong> ", round(missing_pct, 1), "% of subjects have incomplete measurements.</p>",
                    "<p>Paired analysis will only use the ", complete_cases, " subjects with complete data.</p>",
                    "<p>Consider investigating why so much data is missing.</p>",
                    "</div>"
                )
                # Route it through the accumulator like every other message.
                # The direct setContent() here was overwritten moments later by
                # .accumulateMessage(), which rebuilds the panel from
                # private$.messages - so this warning never reached the user.
                private$.accumulateDataMessage(warning_msg)
            }
            
            # Remove NA and non-finite values once BEFORE running data-quality and
            # small-sample checks so diagnostics evaluate the retained complete cases.
            n_nonfinite <- sum(vapply(vars, function(v) {
                x <- mydata[[v]]
                sum(!is.na(x) & !is.finite(x))
            }, numeric(1)))
            finite_rows <- Reduce(`&`, lapply(vars, function(v) {
                is.finite(mydata[[v]])
            }))
            subject_ids <- subject_ids[finite_rows]
            mydata <- mydata[finite_rows, , drop = FALSE]

            # Report N retained
            final_n <- nrow(mydata)
            dropped_n <- n_rows - final_n

            if (dropped_n > 0) {
                 private$.accumulateDataInfoMessage(
                    glue::glue(.("<br><strong>Data Processing:</strong> {final_n} subjects retained. {dropped_n} incomplete cases removed (complete-case listwise deletion assumes missingness is unrelated to unobserved changes).<br>"))
                )
                 if (n_nonfinite > 0)
                     private$.accumulateDataMessage(
                        glue::glue(.("<br> <strong>Non-finite values:</strong> {n_nonfinite} infinite or undefined measurement(s) were treated as missing. Check the source data for division by zero or out-of-range entries.<br>"))
                    )
            } else {
                 private$.accumulateDataInfoMessage(
                    glue::glue(.("<br><strong>Data Processing:</strong> All {final_n} subjects retained (complete data).<br>"))
                )
            }
            
            if (nrow(mydata) == 0) {
                private$.accumulateDataMessage(
                    .("<br> No complete observations after removing missing values<br>")
                )
                private$.prepared_data <- NULL
                return(NULL)
            }

            # Validate data quality on the complete-case cohort actually analyzed
            private$.validateDataQuality(mydata, vars)
            
            # Checkpoint before expensive data transformation
            private$.checkpoint()

            # Perform pivot_longer transformation once using the RAW variable
            # names. Passing raw column names (via unname() to avoid the
            # "Can't rename variables" error from a named vector) keeps
            # data access correct for names with spaces/special characters.
            long_data <- tidyr::pivot_longer(
                mydata,
                cols = unname(vars),
                names_to = "measurement",
                values_to = "value"
            )

            # Preserve measurement ordering for display
            long_data$rowid <- rep(subject_ids, each = length(vars))
            long_data$measurement <- factor(long_data$measurement, levels = vars)
            
            private$.prepared_data <- long_data
            private$.data_hash <- current_hash
            
            return(private$.prepared_data)
        },
        
        .prepareOptions = function(force_refresh = FALSE) {
            # Checkpoint before options processing
            private$.checkpoint(flush = FALSE)
            
            # Create robust hash of current options to detect changes
            current_options_hash <- digest::digest(list(
                typestatistics = self$options$typestatistics,
                pairwisecomparisons = self$options$pairwisecomparisons,
                pairwisedisplay = self$options$pairwisedisplay,
                padjustmethod = self$options$padjustmethod,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                pointpath = self$options$pointpath,
                centralitypath = self$options$centralitypath,
                violin = self$options$violin,
                boxplot = self$options$boxplot,
                point = self$options$point,
                titles = list(self$options$mytitle, self$options$xtitle, self$options$ytitle),
                display = list(self$options$resultssubtitle, self$options$originaltheme),
                advanced = list(self$options$bfmessage, self$options$conflevel, self$options$k, self$options$seed)
            ), algo = "md5")
            
            # Only reprocess if options have changed or forced refresh
            if (!is.null(private$.prepared_options) && 
                private$.options_hash == current_options_hash && 
                !force_refresh) {
                return(private$.prepared_options)
            }
            
            # Process options directly (no formula construction needed for simple strings)
            typestatistics <- self$options$typestatistics
            pairwisedisplay <- self$options$pairwisedisplay
            padjustmethod <- self$options$padjustmethod
            
            # Process titles once
            mytitle <- self$options$mytitle
            xtitle <- if (self$options$xtitle == '') NULL else self$options$xtitle
            ytitle <- if (self$options$ytitle == '') NULL else self$options$ytitle
            
            # Process plot component arguments once
            violinargs <- if (self$options$violin) {
                list(width = 0.5, alpha = 0.2, na.rm = TRUE)
            } else {
                list(width = 0)
            }
            
            # Updated API parameter names for current ggstatsplot
            boxplotargs <- if (self$options$boxplot) {
                list(width = 0.2, alpha = 0.5, na.rm = TRUE)
            } else {
                list(width = 0)
            }
            
            pointargs <- if (self$options$point) {
                list(size = 3, alpha = 0.5, na.rm = TRUE)
            } else {
                list(alpha = 0)
            }
            
            pointpathargs <- if (self$options$pointpath) {
                list(alpha = 0.5, linetype = "dashed")
            } else {
                list(alpha = 0)
            }
            
            centralitypointargs <- if (self$options$centralityplotting) {
                list(size = 5, color = "darkred")
            } else {
                list(size = 0)
            }
            
            centralitypathargs <- if (self$options$centralitypath && self$options$centralityplotting) {
                list(linewidth = 1, color = "red", alpha = 0.5)
            } else {
                list(linewidth = 0)
            }
            
            private$.prepared_options <- list(
                typestatistics = typestatistics,
                pairwisecomparisons = self$options$pairwisecomparisons,
                pairwisedisplay = pairwisedisplay,
                padjustmethod = padjustmethod,
                pointpath = self$options$pointpath,
                mytitle = mytitle,
                xtitle = xtitle,
                ytitle = ytitle,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                centralitypath = self$options$centralitypath,
                violinargs = violinargs,
                boxplotargs = boxplotargs,
                pointargs = pointargs,
                pointpathargs = pointpathargs,
                centralitypointargs = centralitypointargs,
                centralitypathargs = centralitypathargs,
                originaltheme = self$options$originaltheme,
                resultssubtitle = self$options$resultssubtitle,
                # Add missing parameters with current API names
                bfmessage = if (!is.null(self$options$bfmessage)) self$options$bfmessage else TRUE,
                conflevel = if (!is.null(self$options$conflevel)) self$options$conflevel else 0.95,
                digits = if (!is.null(self$options$k)) self$options$k else 2
            )
            
            private$.options_hash <- current_options_hash
            
            return(private$.prepared_options)
        },
        
        # Clinical interpretation helper
        .generateClinicalInterpretation = function(num_measurements, test_type) {
            interpretation_parts <- list()
            
            # Test explanation
            test_explanation <- switch(test_type,
                "parametric" = if (num_measurements == 2) .("The paired t-test compares the mean within-subject difference with zero; normality concerns the differences.") else .("Repeated-measures ANOVA compares means across conditions, accounting for subjects and the sphericity correction."),
                "nonparametric" = if (num_measurements == 2) .("The Wilcoxon signed-rank test assesses paired differences and assumes a symmetric difference distribution for a location-shift interpretation.") else .("The Friedman test compares within-subject ranks across conditions. A median-shift interpretation requires comparable distribution shapes."),
                "robust" = .("Robust test uses trimmed means, reducing influence of outliers common in clinical measurements."),
                "bayes" = .("Bayesian analysis provides evidence for/against differences, useful when traditional p-values are inconclusive."),
                .("Statistical test compares measurements across time points.")
            )
            
            # Sample interpretation
            clinical_context <- sprintf(
                .("This analysis compares %d measurements from the same subjects. Useful for: biomarker tracking, treatment response monitoring, disease progression assessment."),
                num_measurements
            )
            
            # What to look for
            guidance <- .("<strong>What to look for:</strong><br>\u2022 Each row must represent one independent case or subject (do not stack multiple cores, lesions, slides, or readers from the same subject)<br>\u2022 A small p-value is evidence against the null under the model assumptions; it does not prove a treatment effect<br>\u2022 Compare effect estimates and uncertainty with a prespecified clinically important difference<br>\u2022 Two-measurement plot subtitle statistics evaluate First minus Second measurement, whereas Paired Changes reports Second minus First<br>\u2022 Individual trajectories reveal response patterns and potential measurement errors")
            
            interpretation_parts <- list(
                paste0("<div style='background-color: rgba(138, 155, 172, 0.06);padding:15px;margin:10px 0;border-left:4px solid #007bff; color: inherit;'>"),
                paste0("<h4 style='margin-top:0;color:#007bff;'>", .("Clinical Context"), "</h4>"),
                paste0("<p>", clinical_context, "</p>"),
                paste0("<p><strong>", .("Test Used:"), "</strong> ", test_explanation, "</p>"),
                guidance,
                "</div>"
            )
            
            return(paste(interpretation_parts, collapse = ""))
        },
        
        # Generate analysis summary
        .generateAnalysisSummary = function(opts, num_measurements) {
            summary_parts <- list()
            
            # Analysis overview
            analysis_type <- private$.testLabel(opts$typestatistics, num_measurements)
            
            summary_header <- sprintf(
                .("<strong>Analysis:</strong> %s with %d measurements per subject"),
                analysis_type, num_measurements
            )
            
            # Configuration summary
            config_items <- character()
            if (opts$pairwisecomparisons) {
                config_items <- c(config_items, .("Pairwise comparisons enabled"))
            }
            if (opts$centralityplotting) {
                config_items <- c(config_items, .("Central tendency displayed"))
            }
            if (opts$pointpath) {
                config_items <- c(config_items, .("Individual trajectories shown"))
            }
            
            if (length(config_items) > 0) {
                config_summary <- paste0("<br><strong>", .("Configuration:"), "</strong><br>", 
                                       paste(config_items, collapse = "<br>"))
            } else {
                config_summary <- ""
            }
            
            summary_parts <- list(
                paste0("<div style='background-color: rgba(33, 152, 33, 0.07);padding:10px;border:1px solid #28a745; color: inherit;'>"),
                summary_header,
                config_summary,
                "</div>"
            )
            
            return(paste(summary_parts, collapse = ""))
        },

        # Generate explanations ----
        .generateExplanations = function() {
            # One complete sentence per .() call - a translator needs the whole
            # sentence, and .() must not contain newlines.
            self$results$explanations$setContent(paste0(
                "<h3>", .("Explanations"), "</h3>",
                "<p>", .("This analysis compares repeated measurements taken from the same subjects (a within-subjects design)."),
                " ", .("Each row must represent one independent case; do not stack multiple cores, lesions, slides, or readers from the same subject."), "</p>",
                "<p><strong>", .("Violin plots:"), "</strong> ",
                .("These show the distribution of the data; wider sections contain more observations."),
                "<br><strong>", .("Box plots:"), "</strong> ",
                .("These show the median (middle line) and the quartiles (box edges)."), "</p>",
                "<p><strong>", .("Statistical tests:"), "</strong></p>",
                "<ul>",
                "<li><strong>", .("Parametric:"), "</strong> ",
                .("A paired t-test for two measurements, or a repeated-measures ANOVA with a Greenhouse-Geisser sphericity correction for three or more. This assumes the within-subject differences are approximately normal."), "</li>",
                "<li><strong>", .("Nonparametric:"), "</strong> ",
                .("A Wilcoxon signed-rank test for two measurements (assuming symmetric paired differences for a location-shift interpretation), or a Friedman test for three or more (requiring comparable distribution shapes across conditions for a median-shift interpretation)."), "</li>",
                "<li><strong>", .("Robust:"), "</strong> ",
                .("A 20%-trimmed means test (tr = 0.20) that resists outliers common in clinical laboratory values."), "</li>",
                "<li><strong>", .("Bayesian:"), "</strong> ",
                .("This reports a default Cauchy-prior Bayes factor (prior scale 0.707), quantifying relative evidence for and against the null hypothesis."), "</li>",
                "</ul>",
                "<p><strong>", .("Contrast direction and effect-size intervals:"), "</strong> ",
                .("Two-measurement plot subtitle statistics (t, V, Cohen's d, Hedges' g, rank-biserial r, and Bayesian posterior delta) evaluate First Measurement minus Second Measurement (negative when values increase over time), whereas the Paired Changes table reports Later minus Earlier (Second minus First, positive when values increase)."),
                " ", .("For three or more measurements, confidence intervals reported in the plot subtitle for partial eta-squared, partial omega-squared, and Kendall's W (100 bootstrap iterations) are one-sided lower-bound intervals with fixed upper bound 1.00."), "</p>",
                "<p><strong>", .("Missing data:"), "</strong> ",
                .("Only subjects with finite values across every selected measurement are analyzed (complete-case listwise deletion, which assumes missingness is unrelated to unobserved outcome changes)."), "</p>",
                "<p><strong>", .("Note on the Wilcoxon p-value:"), "</strong> ",
                .("The signed-rank p-value is computed from the normal approximation with a continuity correction, not from the exact permutation distribution that stats::wilcox.test uses by default, so it may differ from base R for small samples without ties."), "</p>"
            ))
        },

        # run ----
        .run = function() {

            # Clear optional results even when inputs were removed or data preparation fails.
            private$.populatePairedEffects(NULL)
            # The explanations panel holds STATIC method text and its visibility
            # is governed declaratively by `visible: (showExplanations)` in
            # .r.yaml. Populate it unconditionally - when it was filled only in
            # the variables-selected branch, the welcome screen showed an
            # "Explanations" heading with an empty body (showExplanations
            # defaults to true).
            private$.generateExplanations()

            ## Initial Message ----
            if ( is.null(self$options$dep1) || is.null(self$options$dep2)) {

                ### todo ----

                # Welcome / getting-started message
                todo <- private$.welcomeMessage()

                # Ensure todo is visible and set content
                self$results$todo$setVisible(visible = TRUE)
                self$results$todo$setContent(todo)
                
                # Hide plot output when no variables selected
                self$results$plot$setVisible(visible = FALSE)
                self$results$ggpubrPlot$setVisible(visible = FALSE)
                
                # Hide other outputs as well
                self$results$interpretation$setVisible(visible = FALSE)
                if (!is.null(self$results$summary)) {
                    self$results$summary$setVisible(visible = FALSE)
                }
                if (!is.null(self$results$warnings)) {
                    self$results$warnings$setVisible(visible = FALSE)
                }

                return()

            } else {
                # Clear messages for actual analysis
                private$.resetMessages()
                
                # Check messages from presets and data quality each run
                private$.applyClinicalPresets()
                # Ensure data is prepared (and N messages generated).
                # NULL means a blocking condition was hit (fewer than 3 complete
                # cases, no rows left after cleaning, missing columns); the
                # reason has been written to the Messages panel.
                prepared <- private$.prepareData()
                private$.populatePairedEffects(prepared)

                # Probe the subtitle takeover HERE, not in .plot(): .run() is the
                # only place that composes the warnings HTML, so a flag set later
                # by .plot() would never be rendered. When the takeover stands
                # down, 'Effect size type' and 'Decimal places' do not reach the
                # reported statistic, which the user should be told rather than
                # left to discover.
                private$.subtitleFallback <- NULL
                local({
                    ld <- private$.prepareData(emit_messages = FALSE)
                    if (!is.null(ld)) {
                        opts_now <- private$.prepareOptions()
                        invisible(private$.subtitleExpr(
                            ld, opts_now, length(unique(ld$measurement))))
                    }
                })
                if (!is.null(private$.subtitleFallback))
                    private$.accumulateMessage(paste0(
                        "<br><strong>", .("Note:"), "</strong> ",
                        .fmt(
                            .("the plot subtitle uses the package default ({reason}), so 'Effect size type' and 'Decimal places' do not affect the reported statistic."),
                            reason = private$.safeHtmlOutput(private$.subtitleFallback)),
                        "<br>"))
                
                # Only display plots, summary, and clinical interpretation when prepared data is valid
                has_data <- !is.null(prepared)
                self$results$todo$setVisible(visible = TRUE)
                self$results$plot$setVisible(visible = has_data)
                self$results$ggpubrPlot$setVisible(visible = has_data && isTRUE(self$options$addGGPubrPlot))
                self$results$interpretation$setVisible(visible = has_data)
                if (!is.null(self$results$summary)) {
                    self$results$summary$setVisible(visible = has_data)
                }
                # Visibility is decided at the end of .run(), once we know
                # whether any warning-grade message was actually raised.
                
                # Generate clinical guidance for active analysis
                vars <- c(self$options$dep1, self$options$dep2, 
                         self$options$dep3, self$options$dep4)
                num_measurements <- sum(!sapply(vars, is.null))

                # The effect-size selector offers four choices, but eta and omega
                # are ANOVA-only quantities that statsExpressions' two-group test
                # does not accept, so with TWO measurements they are coerced onto
                # biased/unbiased (see .computeSubtitleExpr). Verified: at n = 40,
                # 'eta' and 'omega' return values identical to 'biased' (Cohen's
                # d_z = 1.27052) and 'unbiased' (Hedges' g_z = 1.24591). Say so,
                # rather than letting the user believe they selected partial
                # eta-squared and silently receive Cohen's d.
                if (isTRUE(self$options$resultssubtitle) &&
                    num_measurements <= 2 &&
                    self$options$effsizetype %in% c("eta", "omega")) {
                    private$.accumulateMessage(paste0(
                        "<br><strong>", .("Note:"), "</strong> ",
                        .fmt(
                            .("with two measurements the comparison is a paired t-test, for which partial eta-squared and omega-squared are not defined; '{requested}' has been reported as {reported} instead."),
                            requested = private$.safeHtmlOutput(self$options$effsizetype),
                            reported = if (identical(self$options$effsizetype, "eta"))
                                .("Cohen's d") else .("Hedges' g")),
                        "<br>"))
                }

                
                # Generate clinical interpretation with error protection
                tryCatch({
                    clinical_interp <- private$.generateClinicalInterpretation(
                        num_measurements, self$options$typestatistics
                    )
                    self$results$interpretation$setContent(clinical_interp)
                }, error = function(e) {
                    self$results$interpretation$setContent(paste0("<br>", .("Clinical interpretation temporarily unavailable"), "<br>"))
                })
                
                # Generate analysis summary with error protection
                tryCatch({
                    opts <- private$.prepareOptions()
                    analysis_summary <- private$.generateAnalysisSummary(opts, num_measurements)
                    # Procedural progress notes ride along under the summary
                    # rather than filling the warning panel.
                    self$results$summary$setContent(
                        paste0(analysis_summary, private$.renderInfoBlock()))
                }, error = function(e) {
                    self$results$summary$setContent(paste0("<br>", .("Analysis summary temporarily unavailable"), "<br>"))
                })

                ### todo ----
                # Do not announce readiness when the data never made it through
                # preparation - the plot stays blank and the only explanation
                # sits in the Messages panel, so "Ready for analysis" read as a
                # contradiction of the warning directly above it.
                todo <- if (is.null(prepared))
                    paste0("<br>", .("Analysis cannot proceed with the current data. See the Messages panel for the reason."), "<br><hr>")
                else
                    paste0("<br>", .fmt(.("Ready for analysis: Violin Plot comparing {n} repeated measurements."), n = num_measurements), "<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    jmvcore::reject(.("Data contains no (complete) rows"))

                # Show the Messages panel only when something needs acting on.
                if (!is.null(self$results$warnings))
                    self$results$warnings$setVisible(
                        visible = length(private$.messages) > 0)

            }
        }


        # the plot function ----
        ,
        .plot = function(image, ggtheme, theme, ...) {
            # Use shared validation helper ----
            if (!private$.validateInputs())
                return()

            # Use prepared data and options ----
            long_data <- private$.prepareData(emit_messages = FALSE)
            opts <- private$.prepareOptions()
            
            if (is.null(long_data)) {
                return()
            }

            # Additional validation
            if (nrow(long_data) == 0) {
                warning_msg <- paste0(
                    "<br>", .("No complete data rows available after removing missing values."),
                    "<br>", .("Please check your data for missing values in the selected variables."),
                    "<br><hr>"
                )
                self$results$todo$setContent(warning_msg)
                return()
            }


            # Checkpoint before expensive plot creation
            private$.checkpoint()

            # Create plot using optimized data and options ----
            # Honour the effect-size selector (see .subtitleExpr): ggstatsplot
            # 1.0.0 no longer accepts effsize.type, so compute the subtitle here.
            # The Bayesian (BayesFactor MCMC) and robust (bootstrap) paths in
            # ggstatsplot sample, so the same analysis rendered twice reported
            # different numbers. Seed them; withr restores the caller's stream.
            n_meas <- length(unique(long_data$measurement))
            bf_active <- isTRUE(opts$resultssubtitle) &&
                captionSeedMatters(opts$typestatistics, "within", n_meas, opts$bfmessage)
            if ((isTRUE(opts$resultssubtitle) && statsSeedMatters(opts$typestatistics, "within", n_meas)) ||
                bf_active)
                withr::local_seed(self$options$seed)

            sub_expr <- private$.subtitleExpr(long_data, opts, n_meas)

            tryCatch({
                plot <- ggstatsplot::ggwithinstats(
                    data = long_data,
                    x = measurement,
                    y = value,
                    # ggwithinstats is always within-subjects; there is no
                    # `paired` formal. Pass the subject identifier explicitly
                    # via subject.id so pairing does not rely on incidental
                    # row order (the previous `id = "rowid"` was silently
                    # dropped into ...).
                    subject.id = "rowid",
                    title = opts$mytitle,
                    xlab = opts$xtitle,
                    ylab = opts$ytitle,
                    type = opts$typestatistics,
                    # `pairwise.comparisons` was removed in ggstatsplot 1.0.0
                    # and silently swallowed by `...`, so unticking the box left
                    # the brackets on the plot. "none" is the surviving control.
                    pairwise.display = private$.pairwiseDisplay(opts),
                    p.adjust.method = opts$padjustmethod,
                    # effsize.type was likewise removed; the selector is
                    # honoured through the subtitle we attach below instead.
                    centrality.plotting = opts$centralityplotting,
                    centrality.type = opts$centralitytype,
                    point.path = opts$pointpath,
                    centrality.path = opts$centralitypath,
                    violin.args = opts$violinargs,
                    boxplot.args = opts$boxplotargs,
                    point.args = opts$pointargs,
                    point.path.args = opts$pointpathargs,
                    centrality.point.args = opts$centralitypointargs,
                    centrality.path.args = opts$centralitypathargs,
                    # When we supply our own subtitle (below) ggstatsplot must
                    # not also compute one.
                    results.subtitle = if (is.null(sub_expr)) opts$resultssubtitle else FALSE,
                    # Updated parameter names for current API
                    bf.message = opts$bfmessage,
                    conf.level = opts$conflevel,
                    digits = opts$digits
                )

                # Attach the subtitle to the FINISHED plot. It is a plotmath
                # language object: passing it through the call would make R try
                # to evaluate italic()/widehat() as ordinary functions.
                if (!is.null(sub_expr))
                    plot <- plot + ggplot2::labs(subtitle = sub_expr)

                # ggwithinstats draws point.path only for two measurements, so
                # the "Show individual trajectories" checkbox silently did
                # nothing with 3 or 4 - exactly the case where following a
                # subject across time matters most. Draw the paths ourselves.
                if (isTRUE(opts$pointpath) && n_meas > 2)
                    plot <- plot + ggplot2::geom_path(
                        ggplot2::aes(group = .data[["rowid"]]),
                        alpha = 0.4, linewidth = 0.3, colour = "grey40")

                # Checkpoint before theme application
                private$.checkpoint()
                
                # Apply theme
                if (!opts$originaltheme) {
                    plot <- plot + ggtheme
                } else {
                    plot <- plot + ggstatsplot::theme_ggstatsplot()
                }

                if (isTRUE(opts$resultssubtitle) &&
                    (statsSeedMatters(opts$typestatistics, "within", n_meas) || bf_active))
                    plot <- addSeedCaption(plot, self, self$options$seed)

                # Print Plot ----
                print(plot)
                TRUE
                
            }, error = function(e) {
                # htmlEscape e$message - ggwithinstats errors may include user column-name fragments
                error_msg <- paste0(
                    "<br>",
                    .fmt(
                        .("The within-subjects plot could not be created: {error}"),
                        error = private$.safeHtmlOutput(e$message)),
                    "<br><br>", .("Please check that:"),
                    "<br>\u2022 ", .("All measurement variables contain numeric values"),
                    "<br>\u2022 ", .("Data has at least 3 complete rows"),
                    "<br>\u2022 ", .("Variables have sufficient variance for statistical tests"),
                    "<br>\u2022 ", .("No extreme outliers that might affect analysis"),
                    "<br><hr>"
                )
                self$results$todo$setContent(error_msg)
            })

        }

        ,
        .plotGGPubr = function(image, ...) {
            # Validate inputs
            if (is.null(self$options$dep1) || is.null(self$options$dep2))
                return()

            # Skip if ggpubr plot not requested
            if (!self$options$addGGPubrPlot)
                return()

            tryCatch({
                # Prepare data - convert wide to long format
                deps <- c(self$options$dep1, self$options$dep2)
                if (!is.null(self$options$dep3)) deps <- c(deps, self$options$dep3)
                if (!is.null(self$options$dep4)) deps <- c(deps, self$options$dep4)

                # CRITICAL FIX: Paired analysis requires listwise deletion and
                # numeric unwrapping (including jamovi integer-backed factors).
                mydata <- self$data[, deps, drop = FALSE]
                for (v in deps) {
                    mydata[[v]] <- private$.coerceNumericCol(mydata[[v]])
                }
                finite_rows <- Reduce(`&`, lapply(deps, function(v) is.finite(mydata[[v]])))
                mydata <- mydata[finite_rows, , drop = FALSE]

                # Require at least 3 complete cases, matching .prepareData()
                if (nrow(mydata) < 3) {
                     return()
                }

                # Create subject ID for paired analysis
                subject_ids <- seq_len(nrow(mydata))

                # Convert to long format using raw variable names and numeric values
                long_data <- data.frame(
                    Subject_ID  = rep(subject_ids, times = length(deps)),
                    Measurement = factor(rep(deps, each = nrow(mydata)), levels = deps),
                    Value       = as.numeric(unlist(mydata[, deps, drop = FALSE], use.names = FALSE)),
                    stringsAsFactors = FALSE
                )

                # Start Plot Construction
                palette <- self$options$ggpubrPalette

                # Create plot based on type
                if (self$options$ggpubrPlotType == "paired") {
                    # Paired plot with connecting lines
                    # `fill` (or `color` for the line plot) has to be mapped for
                    # the palette to have anything to colour. Without it every
                    # ggpubrPalette choice rendered a byte-identical figure.
                    args <- list(
                        data = long_data,
                        x = "Measurement",
                        y = "Value",
                        id = "Subject_ID",
                        fill = "Measurement",
                        palette = palette,
                        line.color = "gray",
                        line.size = 0.4,
                        label = NULL
                    )

                    if (!self$options$ggpubrShowLines) {
                        args$line.color <- NA
                    }

                    plot <- do.call(ggpubr::ggpaired, args)

                } else if (self$options$ggpubrPlotType == "boxplot") {
                    args <- list(
                        data = long_data,
                        x = "Measurement",
                        y = "Value",
                        fill = "Measurement",
                        palette = palette,
                        add = if (self$options$ggpubrAddPoints) "jitter" else NULL
                    )
                    plot <- do.call(ggpubr::ggboxplot, args)

                } else if (self$options$ggpubrPlotType == "violin") {
                    args <- list(
                        data = long_data,
                        x = "Measurement",
                        y = "Value",
                        fill = "Measurement",
                        palette = palette,
                        add = if (self$options$ggpubrAddPoints) "jitter" else NULL
                    )
                    plot <- do.call(ggpubr::ggviolin, args)

                } else if (self$options$ggpubrPlotType == "line") {
                    # ggpubr's `add = "mean_se"` builds
                    # stat_summary(fun.data = "mean_se_") and resolves that name
                    # from the SEARCH PATH at draw time. It therefore works in a
                    # session that has done library(ggpubr) and fails here and in
                    # jamovi, where ggpubr is only namespace-loaded:
                    #   Error in stat_summary(): object 'mean_se_' of mode
                    #   'function' was not found
                    # The failure surfaces in ggplot_build(), not at construction,
                    # so a "did print() error?" check reports success while the
                    # panel renders nothing. Summarise here instead, so the layer
                    # carries a real function rather than a name to look up.
                    #
                    # No colour aesthetic is mapped on purpose: this plot type
                    # draws a SINGLE mean trajectory, so a categorical palette
                    # has nothing to colour. The option description says so.
                    agg <- stats::aggregate(
                        Value ~ Measurement, data = long_data,
                        FUN = function(v) c(mean = mean(v),
                                            se = stats::sd(v) / sqrt(length(v))))
                    agg <- data.frame(
                        Measurement = agg$Measurement,
                        Value = agg$Value[, "mean"],
                        se    = agg$Value[, "se"])
                    agg$Measurement <- factor(agg$Measurement, levels = deps)

                    plot <- ggpubr::ggline(
                        agg, x = "Measurement", y = "Value",
                        group = 1, palette = palette) +
                        ggplot2::geom_errorbar(
                            ggplot2::aes(ymin = .data[["Value"]] - .data[["se"]],
                                         ymax = .data[["Value"]] + .data[["se"]]),
                            width = 0.1)
                }

                # Add statistical comparisons
                if (self$options$ggpubrAddStats && self$options$ggpubrPlotType != "line") {
                    # ggpubr::stat_compare_means can only express the tests base R
                    # exposes (t.test / wilcox.test), on TWO groups. It has no
                    # robust or Bayesian test and no paired omnibus test. Use it
                    # ONLY where it runs exactly the test the user selected;
                    # otherwise annotate with the p-value from the same engine the
                    # main figure uses, so the companion can never contradict it.
                    ggpubr_method <- if (length(deps) == 2)
                        switch(self$options$typestatistics,
                               "parametric"    = "t.test",
                               "nonparametric" = "wilcox.test",
                               NULL)
                    else
                        NULL

                    if (!is.null(ggpubr_method)) {
                        method_args <- if (identical(ggpubr_method, "wilcox.test"))
                            list(exact = FALSE, correct = TRUE)
                        else
                            list()
                        plot <- plot + ggpubr::stat_compare_means(
                            method = ggpubr_method,
                            paired = TRUE,
                            label = "p.signif",
                            method.args = method_args
                        )
                    } else {
                        stat_fn <- if (length(deps) > 2)
                            statsExpressions::oneway_anova
                        else
                            statsExpressions::two_sample_test

                        if (statsSeedMatters(self$options$typestatistics, "within", length(deps)))
                            withr::local_seed(self$options$seed)

                        rm_res <- tryCatch(
                            rlang::inject(stat_fn(
                                data       = transform(long_data,
                                                       .m = factor(Measurement, levels = deps),
                                                       .s = factor(Subject_ID)),
                                x          = !!rlang::sym(".m"),
                                y          = !!rlang::sym("Value"),
                                subject.id = !!rlang::sym(".s"),
                                paired     = TRUE,
                                type       = self$options$typestatistics,
                                digits     = self$options$k,
                                conf.level = self$options$conflevel)),
                            error = function(e) NULL)

                        # as.data.frame first: `$` on an absent tibble column
                        # warns, and the Bayesian result carries no p.value.
                        if (!is.null(rm_res)) rm_res <- as.data.frame(rm_res)

                        if (!is.null(rm_res) && "p.value" %in% names(rm_res) &&
                            is.finite(rm_res$p.value[1])) {
                            p_val <- rm_res$p.value[1]
                            p_str <- if (p_val < 1e-4) "p < 0.0001" else paste0("p = ", formatC(p_val, digits = 3, format = "fg"))
                            plot <- plot + ggplot2::labs(subtitle = sprintf(
                                .("%s: %s (n = %d subjects)"),
                                private$.testLabel(self$options$typestatistics, length(deps)),
                                p_str,
                                length(unique(long_data$Subject_ID))))
                        } else {
                            plot <- plot + ggplot2::labs(subtitle = sprintf(
                                .("%s - see the main figure for the test statistic"),
                                private$.testLabel(self$options$typestatistics, length(deps))))
                        }
                    }
                }

                # Apply theme. The subtitle is set here only if nothing more
                # informative was attached above - this labs() call used to be
                # unconditional and silently discarded the repeated-measures
                # statistic written into the subtitle a few lines earlier.
                plot <- plot + ggpubr::theme_pubr()
                if (is.null(plot$labels$subtitle))
                    plot <- plot + ggplot2::labs(subtitle = .("Descriptive Plot (ggpubr)"))

                print(plot)
                TRUE

            }, error = function(e) {
                # Surface the (html-escaped) error instead of failing silently.
                # ggpubr messages may embed user column-name fragments.
                error_msg <- paste0(
                    "<br>",
                    .fmt(
                        .("The ggpubr plot could not be created: {error}"),
                        error = private$.safeHtmlOutput(e$message)),
                    "<br>", .("The primary within-subjects plot above is unaffected."), "<br><hr>"
                )
                self$results$todo$setContent(error_msg)
            })
        },

        # Safe syntax-pane serializer: drop undeclared option names (which can
        # arrive from saved .omv/.omt files), drop list values (jmvcore::sourcify
        # emits list names raw), and escape character scalars with encodeString().
        .sourcifyOption = function(option) {
            if (!option$name %in% names(formals(jjwithinstats)))
                return('')
            value <- option$value
            if (is.list(value))
                return('')
            if (is.character(value) && length(value) == 1L && !is.na(value) &&
                !identical(value, option$default)) {
                return(paste0(option$name, ' = ', encodeString(value, quote = '"')))
            }
            super$.sourcifyOption(option)
        }

    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for Within-Groups Statistics analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            dep1 <- self$options$dep1
            dep2 <- self$options$dep2

            if (is.null(dep1) || is.null(dep2))
                return('')

            # Build the argument list in option-declaration order.
            #
            # Every variable-name option (dep1, dep2, dep3, dep4) is emitted as a
            # properly escaped string literal so names containing spaces, quotes,
            # or backslashes yield valid R syntax; jmvcore's default sourcify
            # would emit these as bare, unquoted symbols.
            #
            # Undeclared option names and list values (which can arrive unchecked
            # from saved .omv/.omt files) are dropped, and all other non-default
            # character scalars are escaped via private$.sourcifyOption().
            args <- character(0)
            allowed <- names(formals(jjwithinstats))
            for (option in private$.options$options) {
                if (option$name == 'data' || !option$name %in% allowed)
                    next
                if (inherits(option, 'OptionVariable') || inherits(option, 'OptionVariables')) {
                    val <- option$value
                    if (!is.null(val) && !is.list(val)) {
                        encoded <- if (is.character(val) && length(val) == 1L && !is.na(val))
                            encodeString(val, quote = '"')
                        else
                            paste0(deparse(val), collapse = '')
                        args <- c(args, paste0(option$name, ' = ', encoded))
                    }
                } else {
                    as <- private$.sourcifyOption(option)
                    if (!identical(as, ''))
                        args <- c(args, as)
                }
            }

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::jjwithinstats(\n    data = data,\n    ',
                   paste(args, collapse = ',\n    '), ')')
        }
    ) # End of public list
)
