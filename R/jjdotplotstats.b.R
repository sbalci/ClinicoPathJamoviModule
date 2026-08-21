#' @title Horizontal Box-Violin Comparison
#' @importFrom R6 R6Class
#' @importFrom rlang sym
#' @importFrom digest digest
#'
#' @return An \code{R6} class generator object for the \code{jjdotplotstatsClass} backend; used internally by the jamovi analysis wrapper and not called directly.


jjdotplotstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjdotplotstatsClass",
    inherit = jjdotplotstatsBase,
    private = list(
        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .messages = NULL,
        .noticesList = NULL,
        # Cached result of .validateInputs(); computed once per .run() so the
        # render paths (.plot / .plot2) gate silently instead of re-validating.
        .inputsValid = FALSE,
        # Set by .subtitleExpr() when the statsExpressions takeover could not be
        # used, so .run() can say that the effect-size selection was ignored.
        .subtitleFallback = NULL,
        # Subtitle expression computed in .run() (see .subtitleExpr) and read by
        # .plot(). Computing it during rendering was useless for reporting: any
        # notice raised there is discarded, because jamovi has already composed
        # the results panel by the time a figure is drawn.
        .subtitleCache = NULL,
        # Rows dropped by .prepareData() for holding Inf/-Inf, reported separately
        # from ordinary missingness because a non-finite measurement signals a
        # data-entry or divide-by-zero problem rather than an absent observation.
        .nonFiniteDropped = 0L,
        # Messages produced by .prepareData(), kept so a cache hit can re-emit
        # them (see .prepareData).
        .data_messages = NULL,

        # Notice accumulation system (HTML-based, avoids serialization issues)
        .addNotice = function(message, type = "INFO") {
            if (is.null(private$.noticesList)) {
                private$.noticesList <- list()
            }

            # Determine styling based on type
            style_info <- switch(type,
                "ERROR" = list(
                    color = "#721c24",
                    bg = "#f8d7da",
                    border = "#f5c6cb",
                    icon = ""
                ),
                "STRONG_WARNING" = list(
                    color = "#856404",
                    bg = "#fff3cd",
                    border = "#ffeaa7",
                    icon = ""
                ),
                "WARNING" = list(
                    color = "#856404",
                    bg = "#fff3cd",
                    border = "#ffeaa7",
                    icon = ""
                ),
                "INFO" = list(
                    color = "#004085",
                    bg = "#cce5ff",
                    border = "#b8daff",
                    icon = ""
                ),
                # Default
                list(
                    color = "#004085",
                    bg = "#cce5ff",
                    border = "#b8daff",
                    icon = ""
                )
            )

            notice_html <- glue::glue(
                "<div style='background-color: {style_info$bg}; ",
                "border-left: 4px solid {style_info$border}; ",
                "padding: 12px; margin: 8px 0; color: {style_info$color};'>",
                "<strong>{style_info$icon} {type}:</strong> {message}",
                "</div>"
            )

            private$.noticesList <- append(private$.noticesList, notice_html)
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (is.null(private$.noticesList) || length(private$.noticesList) == 0) {
                return()
            }

            notices_html <- paste(private$.noticesList, collapse = "\n")
            self$results$notices$setContent(notices_html)
        },

        .clearNotices = function() {
            private$.noticesList <- NULL
            self$results$notices$setContent("")
        },

        # init ----

        .init = function() {
            # Since dep is single variable, use fixed size
            # Use configurable plot dimensions
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 650
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450
            
            self$results$plot$setSize(plotwidth, plotheight)


            if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    droplevels(as.factor(mydata[[grvar]]))
                )

                # num_levels * plotwidth is unbounded: a Split By variable with
                # 10 levels asked for a 6500-pixel canvas. Cap the total and let
                # the panels narrow instead, and use droplevels() so empty
                # levels of a filtered factor do not reserve space for panels
                # that are never drawn.
                self$results$plot2$setSize(
                    min(max(num_levels, 1L) * plotwidth, 3000L), plotheight)

            }

        }


,
        # Shared validation helper
        .validateInputs = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return(FALSE)

            if (nrow(self$data) == 0) {
                private$.addNotice('Data contains no complete rows. Please check for missing values in your selected variables.', "ERROR")
                return(FALSE)
            }

            # Check variable existence with better context
            if (!(self$options$dep %in% names(self$data))) {
                available_vars <- htmltools::htmlEscape(paste(names(self$data), collapse=", "))
                private$.addNotice(sprintf('Variable "%s" not found in data. Available variables: %s. Please select a valid continuous variable for the dependent variable.', htmltools::htmlEscape(self$options$dep), available_vars), "ERROR")
                return(FALSE)
            }

            if (!(self$options$group %in% names(self$data))) {
                available_vars <- htmltools::htmlEscape(paste(names(self$data), collapse=", "))
                private$.addNotice(sprintf('Variable "%s" not found in data. Available variables: %s. Please select a valid grouping variable.', htmltools::htmlEscape(self$options$group), available_vars), "ERROR")
                return(FALSE)
            }

            # Require at least two groups with complete data
            relevant_cols <- c(self$options$dep, self$options$group)
            if (!is.null(self$options$grvar))
                relevant_cols <- c(relevant_cols, self$options$grvar)
            complete_rows <- complete.cases(self$data[relevant_cols])
            group_levels <- nlevels(droplevels(as.factor(self$data[[self$options$group]][complete_rows])))
            if (group_levels < 2) {
                private$.addNotice(sprintf('At least two groups with data are required for comparison. Found %d group(s) with complete data. Please check for missing values or select different variables.', group_levels), "ERROR")
                return(FALSE)
            }

            # Confidence level must lie strictly inside (0, 1). The boundary
            # values 0 and 1 make ggstatsplot's interval computation fail with an
            # opaque error, so guard against them explicitly here.
            conf_level <- self$options$conflevel
            if (!is.null(conf_level) && (conf_level <= 0 || conf_level >= 1)) {
                private$.addNotice(sprintf('Confidence level must be greater than 0 and less than 1 (received %s). A typical value is 0.95.', base::format(conf_level)), "ERROR")
                return(FALSE)
            }

            # Check total sample size
            n_total <- sum(complete_rows)
            if (n_total < 30) {
                private$.addNotice(sprintf('Small total sample size (N = %d). Statistical tests may be unreliable with N < 30. Consider interpreting results cautiously or collecting more data.', n_total), "STRONG_WARNING")
            }

            # Check minimum group size
            group_data <- self$data[[self$options$group]][complete_rows]
            group_sizes <- table(droplevels(as.factor(group_data)))
            min_group_n <- min(group_sizes)
            if (min_group_n < 10) {
                min_group_name <- names(which.min(group_sizes))
                private$.addNotice(sprintf('Very small group sizes detected (minimum n = %d in group "%s"). Groups with n < 10 may produce unreliable test results. Consider combining groups or collecting more data.', min_group_n, htmltools::htmlEscape(min_group_name)), "STRONG_WARNING")
            }

            # A dependent variable with no spread makes ggstatsplot die inside its
            # own layout code ("arguments imply differing number of rows: 0, 1")
            # and hand back an EMPTY plot box. Catching it here, in .run(), is the
            # difference between an actionable message and a blank figure - the
            # error raised at render time cannot reach the results panel, which
            # jamovi has already composed by then.
            dep_vals <- jmvcore::toNumeric(self$data[[self$options$dep]][complete_rows])
            dep_vals <- dep_vals[is.finite(dep_vals)]
            if (length(dep_vals) > 0 && length(unique(dep_vals)) < 2) {
                private$.addNotice(sprintf('"%s" takes the same value (%s) in every row, so there is no variation to compare between groups. Check that the correct variable is selected.',
                                           htmltools::htmlEscape(self$options$dep),
                                           base::format(dep_vals[1])), "ERROR")
                return(FALSE)
            }

            # Validate centrality parameter consistency
            private$.validateCentralityOptions()

            return(TRUE)
        },
        
        # Centrality parameter validation helper
        .validateCentralityOptions = function() {
            # centralityk was a ggdotplotstats argument (centrality.k) that no
            # longer exists anywhere in ggstatsplot 1.0.0. Verified by rendering:
            # centralityk = 0 and centralityk = 5 both label the means 9.92 /
            # 13.16 / 11.10, while the "Statistical Precision" box (k) does move
            # them (k = 0 gives 10 / 13 / 11). The control is removed from the
            # UI; R-API callers who still pass it get told where the real knob is.
            if (self$options$centralityk != 2) {
                private$.addNotice('"Central Tendency Precision" no longer has any effect - the statistics package dropped that setting. Use "Statistical Precision (Decimal Places)" instead; it controls the centrality labels too.', "INFO")
            }

            if (self$options$centralityplotting && self$options$centralityparameter == "none") {
                private$.addNotice('Centrality plotting enabled but centrality parameter is "none". No centrality lines will be displayed.', "WARNING")
            }

            if (!self$options$centralityplotting && self$options$centralitytype != "parametric") {
                private$.addNotice('Centrality type specified but centrality plotting is disabled. The type setting will have no effect.', "INFO")
            }
        },
        
        # Restore base R's as.character() for formulas while `expr` runs.
        #
        # This is not defensive programming, it is a live bug. `logistf` - a
        # runtime dependency of the odds-ratio analysis, and therefore in this
        # package's Imports - pulls in `formula.tools`, whose
        # as.character.formula returns ONE deparsed string ("v ~ g") where base R
        # returns c("~", "v", "g"). stats::oneway.test does
        #     dp <- as.character(formula)
        # and rejects anything of length != 3 with "a two-sided formula is
        # required", so merely loading ClinicoPath breaks Welch's ANOVA for the
        # whole R session.
        #
        # Measured consequence: with ClinicoPath loaded,
        # ggstatsplot::ggbetweenstats() on three groups returns subtitle = NULL.
        # The user ticks "Statistical results in plot" and gets a figure with no
        # statistics on it and no warning that anything failed.
        #
        # The S3 methods table is an ordinary unlocked environment, so swap the
        # method for the duration of the call and put it back on exit.
        # Single implementation lives in R/ggstatsplot_utils.R.
        .withBaseFormulaChar = function(expr) {
            withBaseFormulaChar(expr)
        },

        # How many group levels actually carry data.
        .nGroupLevels = function(data, group_var) {
            if (is.null(group_var) || !group_var %in% names(data)) return(NA_integer_)
            nlevels(droplevels(as.factor(data[[group_var]])))
        },

        # Build the plot subtitle ourselves.
        #
        # ggstatsplot 1.0.0 dropped `effsize.type` from ggbetweenstats: it is
        # swallowed by `...`, so the four-way "Effect Size Measure" selector was
        # completely inert. Measured on three groups of 40 - Cohen's d, Hedge's
        # g, eta-squared and omega-squared all returned the identical subtitle
        # reporting omega-squared (0.37). statsExpressions, which ggstatsplot
        # calls internally, still honours the argument, so compute the
        # expression here and hand it to the plot with results.subtitle = FALSE.
        #
        # Returns NULL when the takeover cannot be used, in which case
        # ggstatsplot produces its own subtitle:
        #   - subtitles switched off
        #   - Bayesian type (statsExpressions errors on this combination)
        #   - fewer than two groups with data
        .subtitleExpr = function(data, group_var, dep_var, opts) {
            if (!isTRUE(opts$resultssubtitle)) return(NULL)
            if (identical(opts$typestatistics, "bayes")) return(NULL)

            n_lev <- private$.nGroupLevels(data, group_var)
            if (is.na(n_lev) || n_lev < 2) return(NULL)

            # two_sample_test rejects "eta"/"omega" (they are ANOVA-only); map
            # them onto the equivalent two-group family so the selector still
            # means something when there are exactly two groups.
            eff <- opts$effsizetype
            if (n_lev == 2 && identical(eff, "eta"))   eff <- "biased"
            if (n_lev == 2 && identical(eff, "omega")) eff <- "unbiased"

            fn <- if (n_lev == 2) statsExpressions::two_sample_test
                  else            statsExpressions::oneway_anova

            res <- tryCatch(
                private$.withBaseFormulaChar(rlang::inject(fn(
                    data         = data,
                    x            = !!rlang::sym(group_var),
                    y            = !!rlang::sym(dep_var),
                    type         = opts$typestatistics,
                    effsize.type = eff,
                    digits       = opts$digits,
                    conf.level   = opts$conflevel))),
                error = function(e) e)

            # Not hypothetical: `formula.tools` registers an
            # `as.character.formula` returning one deparsed string where base R
            # returns c("~", "y", "g"), which makes stats::oneway.test reject
            # every valid formula with "a two-sided formula is required". It
            # arrives transitively via logistf, so any session that has run a
            # Firth regression loses the three-or-more-group takeover. Falling
            # back to ggstatsplot's subtitle is safe but silently ignores the
            # effect-size choice, so record it and say so in .run().
            if (inherits(res, "condition")) {
                private$.subtitleFallback <- conditionMessage(res)
                return(NULL)
            }
            if (is.null(res$expression) || length(res$expression) == 0) {
                private$.subtitleFallback <- "the statistics engine returned no expression"
                return(NULL)
            }
            res$expression[[1]]
        },

        # Draw a failure message INTO the plot panel.
        #
        # private$.addNotice() cannot help here: jamovi composes and sends the
        # results panel when .run() returns, so anything a render callback writes
        # to an Html item is discarded. Painting the reason where the figure
        # should have been is the only way the user learns why the box is empty.
        .plotFailure = function(msg) {
            print(
                ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0, y = 0, hjust = 0.5, vjust = 0.5,
                                      size = 4, colour = "#721c24",
                                      label = paste(strwrap(msg, width = 60), collapse = "\n")) +
                    ggplot2::theme_void()
            )
            TRUE
        },

        # Message accumulation helper
        .accumulateMessage = function(message) {
            if (is.null(private$.messages)) {
                private$.messages <- character()
            }
            private$.messages <- append(private$.messages, message)
            self$results$todo$setContent(paste(private$.messages, collapse = ""))
        },

        # Same, but also records the message so .prepareData() can replay it on a
        # cache hit. Without this the exclusion disclosures vanish: .run() clears
        # private$.messages on every run, while .prepareData() is keyed on the
        # variables and data dimensions only - so changing any OPTION (test type,
        # confidence level, a title) is a cache hit that skips re-emission, and
        # "N rows excluded due to missing values" silently disappears from a
        # panel whose analysis still excludes them.
        .accumulateDataMessage = function(message) {
            private$.data_messages <- c(private$.data_messages, message)
            private$.accumulateMessage(message)
        },
        
        
        
        
        # Data quality validation helper
        .validateDataQuality = function(mydata, dep_var) {
            num_vals <- jmvcore::toNumeric(mydata[[dep_var]])
            num_vals <- num_vals[!is.na(num_vals)]
            
            if (length(num_vals) < 3) {
                private$.accumulateMessage(
                    glue::glue("<br> Warning: {dep_var} has less than 3 valid observations<br>",
                               dep_var = htmltools::htmlEscape(dep_var))
                )
            }
            if (length(unique(num_vals)) < 2) {
                private$.accumulateMessage(
                    glue::glue("<br> Warning: {dep_var} has no variation (all values are the same)<br>",
                               dep_var = htmltools::htmlEscape(dep_var))
                )
            }
        },
        
        # Outlier detection helper
        .detectOutliers = function(data, var) {
            vals <- jmvcore::toNumeric(data[[var]])
            vals <- vals[!is.na(vals)]
            if (length(vals) > 0) {
                # Checkpoint before expensive quantile calculations
                private$.checkpoint()
                Q1 <- quantile(vals, 0.25, na.rm = TRUE)
                Q3 <- quantile(vals, 0.75, na.rm = TRUE)
                IQR <- Q3 - Q1
                outliers <- which(data[[var]] < (Q1 - 1.5 * IQR) | data[[var]] > (Q3 + 1.5 * IQR))
                if (length(outliers) > 0) {
                    private$.accumulateMessage(
                        glue::glue("<br> {length(outliers)} potential outlier(s) detected in {var}<br>",
                                   var = htmltools::htmlEscape(var))
                    )
                }
            }
        },
        
        # Statistical summary helper
        .addDataSummary = function(data, dep_var, group_var) {
            if (!is.null(dep_var) && !is.null(group_var)) {
                tryCatch({
                    # Checkpoint before expensive tapply operation
                    private$.checkpoint()
                    summary_stats <- tapply(data[[dep_var]], data[[group_var]], 
                                           function(x) c(mean = mean(x, na.rm = TRUE), 
                                                        n = sum(!is.na(x))))
                    n_groups <- length(summary_stats)
                    total_n <- sum(sapply(summary_stats, function(x) x["n"]), na.rm = TRUE)
                    private$.accumulateMessage(
                        glue::glue("<br> Analysis summary: {n_groups} groups, {total_n} total observations<br>")
                    )
                }, error = function(e) {
                    # Silently handle errors in summary calculation
                })
            }
        },

        # Optimized data preparation with robust caching
        .prepareData = function(force_refresh = FALSE) {
            # Create a hash of the current data to detect changes. This keys on
            # the analysis variables plus the data's dimensions and column names
            # rather than the individual cell values. jamovi re-instantiates the
            # analysis object whenever the underlying data is edited, so within a
            # single object lifecycle identical dims + names imply identical
            # values; add a value digest here if this helper is ever reused
            # outside jamovi's lifecycle.
            current_hash <- digest::digest(list(
                dep = self$options$dep,
                group = self$options$group,
                data_dim = dim(self$data),
                col_names = names(self$data),
                grvar = self$options$grvar
            ), algo = "md5")
            
            # Only reprocess if data has changed or forced refresh
            if (!is.null(private$.processedData) && 
                private$.data_hash == current_hash && 
                !force_refresh) {
                for (msg in private$.data_messages)
                    private$.accumulateMessage(msg)
                return(private$.processedData)
            }

            # Clear previous messages and add processing feedback
            private$.messages <- NULL
            private$.data_messages <- NULL
            private$.accumulateDataMessage(
                glue::glue("<br>Processing data for dot plot analysis...<br><hr>")
            )
            
            # Track processing time for large datasets
            start_time <- Sys.time()

            mydata <- self$data

            # Convert dependent variable to numeric (single variable)
            dep_var <- self$options$dep
            if (!is.null(dep_var)) {
                mydata[[dep_var]] <- jmvcore::toNumeric(mydata[[dep_var]])
            }

            # SELECTIVE NA OMISSION - only remove rows with NAs in analysis variables
            # This prevents dropping patients with NAs in unused columns
            if (!is.null(dep_var) && !is.null(self$options$group)) {
                relevant_cols <- c(dep_var, self$options$group)

                # Add grouping variable if present
                if (!is.null(self$options$grvar)) {
                    relevant_cols <- c(relevant_cols, self$options$grvar)
                }

                private$.checkpoint()

                # Count rows before and after NA removal
                n_before <- nrow(mydata)
                mydata <- mydata[complete.cases(mydata[relevant_cols]), ]
                n_after <- nrow(mydata)

                # complete.cases() follows is.na(), which is TRUE for NaN but
                # FALSE for Inf, so an infinite measurement survived into
                # ggstatsplot and killed the whole figure with "'from' must be a
                # finite number" - an EMPTY plot box sitting under this module's
                # own "Analysis completed successfully" notice. Measured on 120
                # rows with a single Inf: zero text elements in the rendered SVG.
                private$.nonFiniteDropped <- 0L
                finite_rows <- is.finite(jmvcore::toNumeric(mydata[[dep_var]]))
                if (any(!finite_rows)) {
                    private$.nonFiniteDropped <- sum(!finite_rows)
                    mydata <- mydata[finite_rows, , drop = FALSE]
                    private$.accumulateDataMessage(
                        glue::glue("<br> Info: {n_inf} row(s) excluded because {dep_safe} held an infinite value (Inf or -Inf). Infinite values usually indicate a division by zero or an out-of-range entry - check the source data.<br>",
                                   n_inf = private$.nonFiniteDropped,
                                   dep_safe = htmltools::htmlEscape(dep_var))
                    )
                    n_after <- nrow(mydata)
                }

                # A group whose measurements are ALL missing disappears from the
                # comparison entirely. The row count alone does not reveal that:
                # a clinician who selected Control/DrugA/DrugB and lost Control
                # to missingness would otherwise read a two-group result as
                # though that is what they asked for. Name what went.
                before_lv <- levels(droplevels(as.factor(self$data[[self$options$group]])))
                after_lv  <- levels(droplevels(as.factor(mydata[[self$options$group]])))
                gone <- setdiff(before_lv, after_lv)
                if (length(gone) > 0) {
                    private$.accumulateDataMessage(
                        glue::glue("<br> <strong>Group(s) dropped:</strong> {lost} had no usable measurements and {verb} excluded from the comparison entirely.<br>",
                                   lost = htmltools::htmlEscape(paste(gone, collapse = ", ")),
                                   verb = if (length(gone) == 1) "was" else "were")
                    )
                }

                # Report NA removal if any occurred
                if (n_before > n_after) {
                    n_dropped <- n_before - n_after
                    private$.accumulateDataMessage(
                        glue::glue("<br> Info: {n_dropped} rows excluded due to missing values in analysis variables.<br>",
                                  "Rows with data: {n_after} of {n_before} ({round(100 * n_after / n_before, 1)}%)<br>")
                    )
                }
            }
            
            # Validate data quality
            if (!is.null(dep_var)) {
                private$.validateDataQuality(mydata, dep_var)
            }
            
            # Detect outliers for datasets with sufficient size
            if (nrow(mydata) > 10 && !is.null(dep_var)) {
                private$.detectOutliers(mydata, dep_var)
            }
            
            # Add statistical summary
            private$.addDataSummary(mydata, dep_var, self$options$group)
            
            # Add processing time feedback for large datasets
            elapsed <- difftime(Sys.time(), start_time, units = "secs")
            if (nrow(mydata) > 1000) {
                private$.accumulateMessage(
                    glue::glue("<br> Large dataset processed in {round(elapsed, 2)} seconds<br>")
                )
            }

            # Cache the processed data with hash
            private$.processedData <- mydata
            private$.data_hash <- current_hash
            return(mydata)
        },

        # Optimized options preparation with robust caching
        .prepareOptions = function(force_refresh = FALSE) {
            # Create robust hash of current options to detect changes
            current_options_hash <- digest::digest(list(
                dep = self$options$dep,
                group = self$options$group,
                grvar = self$options$grvar,
                typestatistics = self$options$typestatistics,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                testvalue = self$options$testvalue,
                bfmessage = self$options$bfmessage,
                conflevel = self$options$conflevel,
                k = self$options$k,
                testvalueline = self$options$testvalueline,
                centralityparameter = self$options$centralityparameter,
                centralityk = self$options$centralityk,
                titles = list(self$options$mytitle, self$options$xtitle, self$options$ytitle),
                display = list(self$options$resultssubtitle, self$options$originaltheme)
            ), algo = "md5")
            
            # Only reprocess if options have changed or forced refresh
            if (!is.null(private$.processedOptions) && 
                private$.options_hash == current_options_hash && 
                !force_refresh) {
                return(private$.processedOptions)
            }

            # Add options preparation feedback if not already processing
            if (is.null(private$.messages)) {
                private$.accumulateMessage(
                    glue::glue("<br>Preparing dot plot analysis options...<br><hr>")
                )
            }

            # Process type of statistics
            typestatistics <- self$options$typestatistics

            # Process variables
            dep <- self$options$dep
            group <- self$options$group

            # Centrality settings mapped to ggstatsplot arguments.
            #
            # There are two controls for one thing, in two different collapse
            # boxes: "Central Tendency Display" (centralityparameter:
            # mean/median/none) and "Central Tendency Measure" (centralitytype:
            # mean/median/trimmed/Bayesian). They used to contradict each other
            # silently - measured with centralityparameter = "mean" and
            # centralitytype = "nonparametric", the plot drew and labelled the
            # MEDIANS (9.80, 13.56, 11.05) while the user's Display control read
            # "Mean". centralitytype is the richer control and is the one the UI
            # enables alongside the plotting checkbox, so it decides; the
            # Display control keeps only its unique power, which is switching
            # centrality off. A disagreement is now stated rather than resolved
            # in silence.
            centrality_plotting <- isTRUE(self$options$centralityplotting) && self$options$centralityparameter != "none"
            centrality_type <- self$options$centralitytype
            if (is.null(centrality_type) || centrality_type == "")
                centrality_type <- typestatistics

            if (centrality_plotting) {
                implied <- switch(self$options$centralityparameter,
                                  mean = "parametric", median = "nonparametric", NULL)
                if (!is.null(implied) && !identical(implied, centrality_type)) {
                    shown <- switch(centrality_type,
                                    parametric = "mean", nonparametric = "median",
                                    robust = "trimmed mean", bayes = "Bayesian (MAP) estimate",
                                    centrality_type)
                    private$.addNotice(sprintf('Your two central-tendency settings disagree: "Central Tendency Display" is set to %s while "Central Tendency Measure" is set to %s. The plot shows the %s, which is what "Central Tendency Measure" selects.',
                                               self$options$centralityparameter,
                                               switch(centrality_type, parametric = "Mean",
                                                      nonparametric = "Median", robust = "Trimmed Mean",
                                                      bayes = "Bayesian Estimate", centrality_type),
                                               shown), "WARNING")
                }
            }

            # Compute axis labels respecting orientation flip (values on x-axis)
            xlab <- self$options$ytitle
            if (xlab == '') xlab <- group
            ylab <- self$options$xtitle
            if (ylab == '') ylab <- dep
            
            # Process titles
            mytitle <- self$options$mytitle
            if (mytitle == '') mytitle <- NULL
            
            # Cache the processed options with all parameters
            options_list <- list(
                typestatistics = typestatistics,
                dep = dep,
                group = group,
                mytitle = mytitle,
                xlab = xlab,
                ylab = ylab,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                testvalue = self$options$testvalue,
                bfmessage = self$options$bfmessage,
                conflevel = self$options$conflevel,
                digits = self$options$k,
                testvalueline = self$options$testvalueline,
                centralityparameter = self$options$centralityparameter,
                centralityk = self$options$centralityk,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme
            )

            # Process centrality parameters if enabled
            options_list$centrality.plotting <- centrality_plotting
            options_list$centrality.type <- centrality_type
            options_list$ggplot.component <- list(ggplot2::coord_flip())
            if (isTRUE(self$options$testvalueline)) {
                options_list$ggplot.component <- c(
                    options_list$ggplot.component,
                    list(ggplot2::geom_hline(
                        yintercept = self$options$testvalue,
                        linetype = "dashed",
                        color = "red"
                    ))
                )
            }
            
            private$.processedOptions <- options_list
            private$.options_hash <- current_options_hash
            return(options_list)
        },

        # run ----
        .run = function() {
            # Clear messages, notices, and cached validity at start of new run
            private$.messages <- NULL
            private$.clearNotices()
            private$.inputsValid <- FALSE
            private$.subtitleFallback <- NULL
            private$.subtitleCache <- NULL

            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool compares a continuous variable across groups and draws
                the comparison horizontally as a box-violin figure with the individual
                points shown.
                <br><br>
                Looking for a Cleveland dot chart - one summary point per group, tested
                against a reference value? Use <b>Dot Chart (Summary vs Reference Value)</b>
                instead. This analysis uses every observation and compares the groups
                with each other.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations for <a href = 'https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.html' target='_blank'>ggbetweenstats</a> and <a href = 'https://www.indrapatil.com/ggstatsplot/reference/grouped_ggbetweenstats.html' target='_blank'>grouped_ggbetweenstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # todo ----
                todo <- glue::glue(
                    "<br>You have selected to use a Dot Plot to compare continuous variables by groups.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0) {
                    private$.addNotice('Data contains no complete rows after filtering. Please check for missing values.', "ERROR")
                    return()
                }

                # Pre-process data and options for performance with enhanced validation
                private$.inputsValid <- FALSE
                tryCatch({
                    mydata <- private$.prepareData()
                    options_data <- private$.prepareOptions()

                    # Validate inputs once per run. All validation notices are
                    # emitted here (not in the render paths) so they neither
                    # duplicate when a Split-By variable makes both plots render,
                    # nor accumulate on plot-only re-renders such as resizing.
                    private$.inputsValid <- private$.validateInputs()

                    # Describe the analysis; do NOT claim it succeeded. .run()
                    # finishes before a single pixel is drawn, so the old
                    # "Analysis completed successfully" notice was published
                    # while the figure could still fail - and it regularly did,
                    # leaving a confident success message above an empty plot
                    # box (measured with one Inf value, and with a constant
                    # dependent variable).
                    if (isTRUE(private$.inputsValid)) {
                        n_obs <- nrow(mydata)
                        n_groups <- length(unique(mydata[[options_data$group]]))
                        test_name <- switch(options_data$typestatistics,
                            "parametric" = "parametric (t-test/ANOVA)",
                            "nonparametric" = "nonparametric (Mann-Whitney/Kruskal-Wallis)",
                            "robust" = "robust (trimmed means)",
                            "bayes" = "Bayesian",
                            "selected"
                        )
                        private$.addNotice(sprintf('Comparing %d groups with N = %d observations using a %s test.', n_groups, n_obs, test_name), "INFO")

                        # Computed HERE, not in .plot(): a notice raised during
                        # rendering is thrown away, so this is the only place the
                        # user can be told the effect-size choice was dropped.
                        private$.subtitleCache <- private$.subtitleExpr(
                            mydata, options_data$group, options_data$dep, options_data)

                        # The statsExpressions takeover is what makes the effect
                        # size selector work; say so when it could not be used.
                        if (!is.null(private$.subtitleFallback) && isTRUE(self$options$resultssubtitle))
                            private$.addNotice(sprintf('The effect size measure you selected could not be applied (%s), so the plot shows the statistics package default instead.',
                                                       htmltools::htmlEscape(private$.subtitleFallback)), "WARNING")
                    }
                }, error = function(e) {
                    private$.addNotice(sprintf('Data processing failed: %s. Please check your variable selections and try again.', htmltools::htmlEscape(e$message)), "ERROR")
                    return()
                })

            }
        }


        ,
        .plot = function(image, ggtheme, theme, ...) {
            # Inputs are validated once in .run(); the render path only reads the
            # cached result so validation notices are not re-emitted per render. ----
            if (!isTRUE(private$.inputsValid))
                return()

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()


            # ggbetweenstats ----
            # https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.html

            # Checkpoint before expensive ggstatsplot computation
            private$.checkpoint()

            # effsize.type is inert in ggstatsplot 1.0.0, so the subtitle is
            # computed through statsExpressions in .run() and switched off here
            # when the takeover succeeded.
            sub_expr <- private$.subtitleCache

            plot <- tryCatch({
                p <- private$.withBaseFormulaChar(ggstatsplot::ggbetweenstats(
                    data = mydata,
                    x = !!rlang::sym(options_data$group),
                    y = !!rlang::sym(options_data$dep),
                    title = options_data$mytitle,
                    xlab = options_data$xlab,
                    ylab = options_data$ylab,
                    type = options_data$typestatistics,
                    conf.level = options_data$conflevel,
                    digits = options_data$digits,
                    bf.message = options_data$bfmessage,
                    centrality.plotting = options_data$centrality.plotting,
                    centrality.type = options_data$centrality.type,
                    results.subtitle = if (is.null(sub_expr)) options_data$resultssubtitle else FALSE,
                    ggplot.component = options_data$ggplot.component,
                    ggtheme = if (options_data$originaltheme) ggstatsplot::theme_ggstatsplot() else ggtheme
                ))
                # Attach after construction: do.call()/quote-based routes either
                # evaluate the plotmath language object ("could not find function
                # 'italic'") or break the rlang::sym() arguments.
                if (!is.null(sub_expr)) p <- p + ggplot2::labs(subtitle = sub_expr)
                p
            }, error = function(e) e)

            if (inherits(plot, "condition"))
                return(private$.plotFailure(sprintf(
                    "The plot could not be drawn: %s. Check the dependent variable for constant values, extreme outliers or too few observations per group, or try a different statistical test.",
                    conditionMessage(plot))))
            if (is.null(plot)) return()

            # Print Plot ----

            print(plot)
            TRUE

        }


        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            # Inputs are validated once in .run(); the render path only reads the
            # cached result. The Split-By variable must also be present. ----
            if (!isTRUE(private$.inputsValid) || is.null(self$options$grvar))
                return()

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()


            # grouped_ggbetweenstats ----
            # https://www.indrapatil.com/ggstatsplot/reference/grouped_ggbetweenstats.html



            if (!is.null(self$options$grvar)) {
                grvar <- self$options$grvar

                # Checkpoint before expensive grouped ggstatsplot computation
                private$.checkpoint()

                plot2 <- tryCatch({
                    # No takeover here: grouped_ggbetweenstats computes one
                    # subtitle per panel internally and there is no supported way
                    # to hand it a list of expressions. effsize.type is therefore
                    # still inert on this figure - .run() says so.
                    private$.withBaseFormulaChar(ggstatsplot::grouped_ggbetweenstats(
                        data = mydata,
                        x = !!rlang::sym(options_data$group),
                        y = !!rlang::sym(options_data$dep),
                        grouping.var = !!rlang::sym(grvar),
                        type = options_data$typestatistics,
                        conf.level = options_data$conflevel,
                        digits = options_data$digits,
                        bf.message = options_data$bfmessage,
                        results.subtitle = options_data$resultssubtitle,
                        centrality.plotting = options_data$centrality.plotting,
                        centrality.type = options_data$centrality.type,
                        ggplot.component = options_data$ggplot.component,
                        ggtheme = if (options_data$originaltheme) ggstatsplot::theme_ggstatsplot() else ggtheme,
                        xlab = options_data$xlab,
                        ylab = options_data$ylab,
                        # NOT `title =`. grouped_ggbetweenstats sets the title of
                        # each panel to that panel's level name, so passing one
                        # through `...` collided with its own argument and threw
                        # "formal argument \"title\" matched by multiple actual
                        # arguments" for EVERY Split By analysis - the whole
                        # feature had never produced a figure. The error went to a
                        # notice raised at render time, which jamovi discards, so
                        # the user saw an empty panel under a success message.
                        # The overall title belongs to the patchwork annotation.
                        annotation.args = list(title = options_data$mytitle)
                    ))
                }, error = function(e) e)

                if (inherits(plot2, "condition"))
                    return(private$.plotFailure(sprintf(
                        "The split figure could not be drawn: %s. Check that every level of the Split By variable has enough data in at least two groups.",
                        conditionMessage(plot2))))
                if (is.null(plot2)) return()
            }


            # Print Plot ----

            print(plot2)
            TRUE

        }





    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for jjdotplotstats analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            dep <- self$options$dep
            group <- self$options$group

            if (is.null(dep) || is.null(group))
                return('')

            # Build the argument list in option-declaration order.
            #
            # Every variable-name option (dep, group, grvar) is emitted as a
            # deparse()'d string literal. deparse() produces valid, fully-escaped
            # R for names containing spaces, quotes or backslashes (e.g.
            # `Tumor Grade`); jmvcore's default sourcify would emit these as bare,
            # unquoted symbols and yield invalid syntax. Detecting OptionVariable
            # by class (rather than by name) means any variable option added later
            # is escaped automatically.
            #
            # data/dep/group are NOT re-emitted through private$.asArgs() - doing
            # so previously duplicated dep and group in the generated syntax (the
            # "double variables" bug). All non-variable options keep jmvcore's
            # per-option sourcify so formatting stays consistent with jamovi.
            args <- character(0)
            for (option in private$.options$options) {
                if (option$name == 'data')
                    next
                if (inherits(option, 'OptionVariable') || inherits(option, 'OptionVariables')) {
                    val <- option$value
                    if (!is.null(val))
                        args <- c(args, paste0(option$name, ' = ',
                                               paste0(deparse(val), collapse = '')))
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
            paste0(pkg_name, '::jjdotplotstats(\n    data = data,\n    ',
                   paste(args, collapse = ',\n    '), ')')
        }
    ) # End of public list
)
