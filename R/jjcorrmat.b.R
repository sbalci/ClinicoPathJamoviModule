#' @title Correlation Matrix
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @importFrom rlang sym
#'
#' @return An \code{R6} class generator object for the \code{jjcorrmatClass} backend; used internally by the jamovi analysis wrapper and not called directly.


jjcorrmatClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjcorrmatClass",
    inherit = jjcorrmatBase,
    private = list(


      # R6 locks the private environment, so a field must be DECLARED here before
      # anything may assign to it. These were only ever assigned (in .init() and
      # friends), which threw "cannot add bindings to a locked environment" and
      # aborted the analysis during init - before any result was produced.
      .preset_recommendations = NULL,
        # Seed for the Bayesian sampler. Shared by the table and both plot
        # methods so a Bayesian correlation matrix is reproducible and the two
        # outputs report the same draw.
        .BAYES_SEED = 20250101L,

        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,
        # Rows before/after listwise deletion, cached alongside .processedData.
        # The exclusion count used to be written straight into the `todo` panel
        # from .prepareData(), and .prepareOptions() overwrote it with a
        # progress string two lines later, so the user never saw how many rows
        # had been dropped. Held as state and re-emitted from .run() on every
        # cycle, because .prepareData() is memoised and would otherwise skip the
        # message whenever only an option changed.
        .n_before = NA_integer_,
        .n_after = NA_integer_,
        # Per-pair N and the number of non-NA coefficients actually produced,
        # filled in by .populateTable and reused by the summary panel and the
        # completion notice. Under pairwise deletion nrow(data) is NOT the
        # sample size of any correlation in the table.
        .pair_n = integer(0),
        .n_valid_pairs = 0L,
        .data_hash = NULL,
        .options_hash = NULL,
        # .preset_recommendations = NULL,  # Commented out - clinical preset feature disabled
        .warnings = list(),  # Collect warnings for HTML display (avoids Notice serialization errors)

        # Helper function to add warnings without Notice objects
        .addWarning = function(type, message) {
            private$.warnings[[length(private$.warnings) + 1]] <- list(
                type = type,
                message = message
            )
        },

        # Display all collected warnings as HTML
        .displayWarnings = function() {
            if (length(private$.warnings) == 0) {
                self$results$warnings$setVisible(FALSE)
                return()
            }

            warning_html <- "<div style='margin: 10px 0;'>"

            for (warning in private$.warnings) {
                if (warning$type == "ERROR") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; padding: 10px; margin: 5px 0; border-radius: 4px; color: inherit;'>",
                        "<strong style='color: #721c24;'> ERROR:</strong> <span style='color: #721c24;'>", warning$message, "</span>",
                        "</div>")
                } else if (warning$type == "STRONG_WARNING") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ff9800; padding: 10px; margin: 5px 0; border-radius: 4px; color: inherit;'>",
                        "<strong style='color: #856404;'> STRONG WARNING:</strong> <span style='color: #856404;'>", warning$message, "</span>",
                        "</div>")
                } else if (warning$type == "WARNING") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: rgba(255, 203, 33, 0.14); border-left: 4px solid #ffc107; padding: 10px; margin: 5px 0; border-radius: 4px; color: inherit;'>",
                        "<strong style='color: #664d03;'> WARNING:</strong> <span style='color: #664d03;'>", warning$message, "</span>",
                        "</div>")
                } else if (warning$type == "INFO") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: rgba(33, 163, 188, 0.21); border-left: 4px solid #0c5460; padding: 10px; margin: 5px 0; border-radius: 4px; color: inherit;'>",
                        "<strong style='color: #0c5460;'> INFO:</strong> <span style='color: #0c5460;'>", warning$message, "</span>",
                        "</div>")
                }
            }

            warning_html <- paste0(warning_html, "</div>")
            self$results$warnings$setContent(warning_html)
            self$results$warnings$setVisible(TRUE)
        },

        # Resolve possible B64-encoded column names supplied by jamovi.
        # Row filtering never changes column names, so resolving against
        # self$data (default) or a prepared subset is equivalent. Single
        # source of truth used by every code path (validation, data prep,
        # table, interpretation) so B64 handling stays consistent.
        .resolveName = function(var, data = NULL) {
            if (is.null(var)) return(NULL)
            if (is.null(data)) data <- self$data
            if (var %in% names(data)) return(var)
            b64 <- jmvcore::toB64(var)
            if (b64 %in% names(data)) return(b64)
            var
        },

        # Single source of truth for the getting-started welcome HTML,
        # shared by .init and .run to avoid drift between two copies.
        .welcomeMessage = function() {
            .("<br>Welcome to ClinicoPath Correlation Matrix Analysis
            <br><br>
            <strong>What this does:</strong> Analyzes relationships between continuous variables (e.g., biomarker levels, lab values, imaging metrics)
            <br><br>
            <strong>When to use:</strong> When examining associations between 2+ continuous clinical variables
            <br><br>
            <strong>Quick Start:</strong>
            <br>1. Select 2 or more continuous variables
            <br>2. Choose correlation method (Pearson for normal data, Spearman for non-normal)
            <br>3. Optionally group by categorical variable (e.g., tumor grade, treatment group)
            <br>4. Use partial correlations (3+ variables) to control for confounding effects
            <br><br>
            <strong>Correlation Types:</strong>
            <br>\u2022 <strong>Zero-order (regular):</strong> Direct relationship between two variables
            <br>\u2022 <strong>Partial:</strong> Relationship while controlling for all other variables (reduces confounding)
            <br><br>
            This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://www.indrapatil.com/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> and <a href = 'https://www.indrapatil.com/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a>.
            <br>
            Please cite jamovi and the packages as given below.
            <br><hr>")
        },

        # init ----
        .init = function() {

            # Show welcome message when no variables or insufficient variables are selected
            if (is.null(self$options$dep) || length(self$options$dep) < 2) {

                self$results$todo$setContent(private$.welcomeMessage())
                return()
            }



            deplen <- length(self$options$dep)

            # Use configurable plot dimensions
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$plot$setSize(plotwidth, plotheight)

            if (!is.null(self$options$grvar)) {

                mydata <- self$data

                # Resolve the possibly B64-encoded column name, as every other
                # code path does. Indexing with the raw name returns NULL when
                # jamovi encodes it, and nlevels(as.factor(NULL)) is 0, which
                # would size the grouped plot to zero width.
                grvar <- private$.resolveName(self$options$grvar, mydata)

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                self$results$plot2$setSize(max(num_levels, 1L) * plotwidth, plotheight)

            }


        },

        # Optimized data preparation with caching
        .prepareData = function(force_refresh = FALSE) {
            # Hash-based invalidation - mirrors .options_hash pattern.
            # Without this, stale filtered data could be returned when dep/grvar/
            # naHandling change between .run() invocations on the same R6 instance.
            current_data_hash <- digest::digest(list(
                dep = self$options$dep,
                grvar = self$options$grvar,
                naHandling = self$options$naHandling,
                data_dim = dim(self$data),
                col_names = names(self$data)
            ), algo = "md5")

            if (!is.null(private$.processedData) &&
                !is.null(private$.data_hash) &&
                private$.data_hash == current_data_hash &&
                !force_refresh) {
                return(private$.processedData)
            }

            mydata <- self$data
            # Resolve possible B64 column names from jamovi (shared helper)
            resolve_name <- function(var) private$.resolveName(var, mydata)
            for (v in self$options$dep) {
                nm <- resolve_name(v)
                x <- jmvcore::toNumeric(mydata[[nm]])
                if (!is.numeric(x)) jmvcore::reject("Correlation variables must be numeric.")
                x[!is.finite(x)] <- NA_real_
                mydata[[nm]] <- x
            }


            # SELECTIVE NA OMISSION - only remove rows with NAs in selected correlation variables
            # This prevents dropping patients with NAs in unused columns
            if (!is.null(self$options$dep) && length(self$options$dep) >= 2) {
                relevant_cols <- vapply(self$options$dep, resolve_name, character(1))

                # Add grouping variable if present
                if (!is.null(self$options$grvar)) {
                    relevant_cols <- c(relevant_cols, resolve_name(self$options$grvar))
                }

                private$.checkpoint()

                if (self$options$naHandling == "listwise") {
                    private$.n_before <- nrow(mydata)
                    mydata <- mydata[complete.cases(mydata[relevant_cols]), ]
                    private$.n_after <- nrow(mydata)
                } else {
                    private$.n_before <- nrow(mydata)
                    private$.n_after <- nrow(mydata)
                }
            }

            # Cache the processed data
            private$.processedData <- mydata
            private$.data_hash <- current_data_hash
            return(mydata)
        },

        # Shared validation helper
        .validateInputs = function() {
            if (length(self$options$dep) < 2)
                return(FALSE)
            if (nrow(self$data) == 0) {
                private$.addWarning("ERROR", 'Data contains no complete rows. Please check for missing values in selected variables.')
                return(FALSE)
            }

            # Enhanced validation for correlation analysis
            mydata <- self$data
            resolve_name <- function(var) private$.resolveName(var, mydata)

            # Check if variables exist in data
            for (var in self$options$dep) {
                varname <- resolve_name(var)
                if (!(varname %in% names(mydata))) {
                    private$.addWarning("ERROR", sprintf('Variable "%s" not found in data. Please select valid variables and re-run.', htmltools::htmlEscape(var)))
                    return(FALSE)
                }
            }

            # VALIDATE NUMERIC VARIABLES - check for categorical
            numeric_vars <- 0
            factor_warnings <- character()
            constant_vars <- character()

            for (var in unique(self$options$dep)) {
                private$.checkpoint()  # Before numeric conversion operations

                # Check if variable is a factor BEFORE conversion
                varname <- resolve_name(var)

                if (is.factor(mydata[[varname]])) {
                    factor_warnings <- c(factor_warnings, var)
                }

                num_vals <- jmvcore::toNumeric(mydata[[varname]])
                num_vals <- num_vals[!is.na(num_vals)]

                if (length(num_vals) >= 3) {  # Minimum observations for correlation
                    if (length(unique(num_vals)) >= 2) {  # Must have variation
                        numeric_vars <- numeric_vars + 1
                    } else {
                        constant_vars <- c(constant_vars, var)
                    }
                }
            }

            # A constant column is undefined for correlation: it produces a row
            # of NAs in the table and is silently dropped from the figure, so
            # say so instead of leaving the user to guess.
            if (length(constant_vars) > 0)
                private$.addWarning("WARNING", sprintf(
                    .('%s has no variation (a single value), so its correlations are undefined: those rows are blank in the table and the variable is omitted from the plot.'),
                    htmltools::htmlEscape(paste(constant_vars, collapse = ', '))))

            # Stop if correlating category codes
            if (length(factor_warnings) > 0) {
                private$.addWarning("ERROR", sprintf('Correlation analysis requires numeric variables. The following are categorical: %s. Please select continuous numeric variables.', htmltools::htmlEscape(paste(factor_warnings, collapse = ', '))))
                return(FALSE)
            }

            if (numeric_vars < 2) {
                private$.addWarning("ERROR", sprintf('Correlation analysis requires at least 2 numeric variables with sufficient variation. Found %d valid variable(s). Please select additional variables.', numeric_vars))
                return(FALSE)
            }

            return(TRUE)
        },

        # Optimized options preparation with caching
        .prepareOptions = function(force_refresh = FALSE) {
            # Create hash of current options to detect changes
            current_options_hash <- paste(
                paste(self$options$dep, collapse = ","),
                self$options$typestatistics, self$options$matrixtype, self$options$matrixmethod,
                self$options$siglevel, self$options$conflevel, self$options$padjustmethod, self$options$naHandling,
                self$options$k, self$options$partial, # self$options$clinicalpreset,  # Commented out - clinical preset disabled
                self$options$lowcolor, self$options$midcolor, self$options$highcolor,
                self$options$title, self$options$subtitle, self$options$caption,
                self$options$plotwidth, self$options$plotheight,
                collapse = "_"
            )

            if (!is.null(private$.processedOptions) && private$.options_hash == current_options_hash && !force_refresh) {
                return(private$.processedOptions)
            }

            # Apply clinical preset configurations
            # private$.applyClinicalPreset()  # Commented out - clinical preset feature disabled

            # Process type of statistics
            typestatistics <- self$options$typestatistics

            # Process variables - dep is already a list of variables.
            # De-duplicate: ggcorrmat's tidyselect drops repeats, so a variable
            # selected twice produced a structural r = 1.000 row in the table
            # (and an extra test in the multiplicity correction) that the figure
            # never showed.
            myvars <- unique(self$options$dep)

            # Adjust partial flag if insufficient variables
            partial_flag <- self$options$partial
            if (partial_flag && length(myvars) < 3) {
                partial_flag <- FALSE
            }

            # Process text parameters
            title <- if (self$options$title != '') self$options$title else NULL
            subtitle <- if (self$options$subtitle != '') self$options$subtitle else NULL
            caption <- if (self$options$caption != '') self$options$caption else NULL

            # Process colors
            colors <- c(self$options$lowcolor, self$options$midcolor, self$options$highcolor)

            # Process ggcorrplot.args
            # pch.cex must be carried over from ggstatsplot's own default
            # (list(method = "square", outline.color = "black", pch.cex = 14)):
            # passing this list REPLACES that default, and ggcorrplot's own
            # formal is 5, which draws the non-significance cross small enough
            # to sit on top of the printed coefficient ("0.02" renders "0X2").
            ggcorrplot.args <- list(
                method = self$options$matrixmethod,
                outline.color = "black",
                pch.cex = 14
            )

            # Cache the processed options
            options_list <- list(
                typestatistics = typestatistics,
                myvars = myvars,
                matrixtype = self$options$matrixtype,
                ggcorrplot.args = ggcorrplot.args,
                siglevel = self$options$siglevel,
                conflevel = self$options$conflevel,
                padjustmethod = self$options$padjustmethod,
                k = self$options$k,
                partial = partial_flag,
                naHandling = self$options$naHandling,
                # clinicalpreset = self$options$clinicalpreset,  # Commented out - clinical preset disabled
                colors = colors,
                title = title,
                subtitle = subtitle,
                caption = caption
            )
            private$.processedOptions <- options_list
            private$.options_hash <- current_options_hash
            return(options_list)
        },

        # Clinical interpretation helper for correlation results.
        # `symbol` names the coefficient actually computed (r / rho / winsorized
        # r), `alpha` is the user's significance level and `adjusted` says
        # whether p_value has been corrected for multiple comparisons - all
        # three were previously hard-coded to Pearson, 0.05 and "unadjusted".
        .interpretCorrelation = function(r, p_value, bf = NA_real_, method = "Pearson",
                                         symbol = "r", alpha = 0.05, adjusted = FALSE,
                                         var1 = "Variable 1", var2 = "Variable 2") {
            if (is.na(r)) return(.("Unable to interpret correlation"))

            # Determine correlation strength
            strength <- if (abs(r) >= 0.7) .("strong")
                       else if (abs(r) >= 0.5) .("moderate-to-strong")
                       else if (abs(r) >= 0.3) .("moderate")
                       else if (abs(r) >= 0.1) .("weak-to-moderate")
                       else .("very weak")

            # Determine direction
            direction <- if (r > 0) .("positive") else .("negative")

            # Determine the evidence statement. Bayesian analysis has no
            # p-value, so the Bayes factor carries the evidence instead.
            p_word <- if (adjusted) .("adjusted p") else .("p")
            if (!is.na(bf) && is.na(p_value)) {
                is_notable <- bf >= 3
                significance <- sprintf(.("supported by a Bayes factor of %s"),
                                        base::format(signif(bf, 3), scientific = bf >= 1e5))
            } else if (is.na(p_value)) {
                is_notable <- FALSE
                significance <- .("of undetermined significance")
            } else {
                is_notable <- p_value < alpha
                significance <- if (p_value < 0.001)
                        sprintf(.("statistically significant (%s < 0.001)"), p_word)
                    else
                        sprintf(.("%s (%s = %s)"),
                                if (is_notable) .("statistically significant") else .("not statistically significant"),
                                p_word, sprintf("%.3f", p_value))
            }

            # Generate clinical interpretation
            interpretation <- sprintf(
                .("A %s %s correlation (%s = %s) between %s and %s that is %s, using %s."),
                strength, direction, symbol, sprintf("%.3f", r),
                htmltools::htmlEscape(var1), htmltools::htmlEscape(var2),
                significance, htmltools::htmlEscape(method)
            )

            # Add clinical guidance
            if (abs(r) >= 0.3 && is_notable) {
                guidance <- .("<br><strong>Clinical Note:</strong> This suggests a meaningful association that may warrant further investigation.")
            } else if (abs(r) < 0.3) {
                guidance <- .("<br><strong>Clinical Note:</strong> The estimated correlation is weak by this descriptive convention. Clinical importance depends on context, uncertainty and a prespecified clinical criterion.")
            } else {
                guidance <- .("<br><strong>Clinical Note:</strong> Although the correlation appears moderate-to-strong, the evidence for it is not conclusive at the chosen threshold.")
            }

            return(paste0(interpretation, guidance))
        },

        # Validation with clinical warnings
        .validateClinicalInputs = function() {
            # Check minimum sample size for reliable correlations.
            # Use the effective complete-case N over the selected variables
            # (after numeric coercion) rather than the raw row count, so a
            # large dataset with heavy missingness in the selected variables
            # is not falsely cleared. Complete-case N across all selected
            # variables is the lower bound of any single pair's N, making it
            # a conservative guard for both listwise and pairwise handling.
            effective_n <- tryCatch({
                dep_cols <- vapply(self$options$dep,
                                   function(v) private$.resolveName(v),
                                   character(1))
                dep_cols <- dep_cols[dep_cols %in% names(self$data)]
                if (length(dep_cols) >= 2) {
                    sub <- self$data[, dep_cols, drop = FALSE]
                    for (cc in names(sub)) sub[[cc]] <- jmvcore::toNumeric(sub[[cc]])
                    sum(complete.cases(sub))
                } else {
                    nrow(self$data)
                }
            }, error = function(e) nrow(self$data))

            if (effective_n < 20) {
                private$.addWarning("STRONG_WARNING", sprintf('Small sample size (N = %d complete cases). Correlations with N < 20 may be unreliable. Consider collecting more data or interpreting results cautiously.', effective_n))
            }

            # Check for too many variables (interpretation complexity)
            if (length(self$options$dep) > 10) {
                private$.addWarning("WARNING", sprintf('Correlation matrix with %d variables may be complex to interpret. Consider focusing on key variables of interest.', length(self$options$dep)))
            }

            # Check partial correlations requirements
            if (self$options$partial && length(self$options$dep) < 3) {
                private$.addWarning("WARNING", sprintf('Partial correlations require at least 3 variables to control for confounding. Found %d variable(s). Computing zero-order correlations instead.', length(self$options$dep)))
            }

            return(TRUE)
        },

        # Apply clinical preset configurations
        # COMMENTED OUT - Clinical preset feature disabled
        # .applyClinicalPreset = function() {
        #     preset <- self$options$clinicalpreset
        #
        #     if (is.null(preset) || preset == "custom") {
        #         return()  # No preset modifications for custom analysis
        #     }
        #
        #     if (preset == "biomarker") {
        #         self$options$typestatistics <- "robust"
        #         private$.preset_recommendations <- .("For biomarker correlations, robust correlation methods are recommended to handle outliers.")
        #
        #     } else if (preset == "labvalues") {
        #         self$options$typestatistics <- "parametric"
        #         private$.preset_recommendations <- .("For lab values, parametric correlations are often appropriate if distributions are normal.")
        #
        #     } else if (preset == "imaging") {
        #         self$options$typestatistics <- "nonparametric"
        #         private$.preset_recommendations <- .("For imaging metrics, consider nonparametric correlations due to potentially skewed distributions.")
        #     }
        # },

        # Generate clinical interpretation of correlation results
        # Name of the coefficient each `typestatistics` value produces in
        # ggstatsplot 1.0.0. "robust" is Winsorized Pearson (correlation::
        # correlation(winsorize = 0.2), identical to WRS2::wincor(tr = 0.2)) -
        # NOT the percentage-bend coefficient that older ggstatsplot used and
        # that this module's documentation still described.
        .methodName = function(typestatistics) {
            switch(typestatistics,
                "parametric"    = .("Pearson"),
                "nonparametric" = .("Spearman"),
                "robust"        = .("Winsorized Pearson"),
                "bayes"         = .("Bayesian Pearson"),
                typestatistics)
        },

        .methodLabel = function(typestatistics) {
            sprintf(.("%s correlations"), private$.methodName(typestatistics))
        },

        # Sample-size phrase built from the per-pair N recorded by
        # .populateTable, so it matches the table's N column under both
        # listwise and pairwise deletion.
        .pairNLabel = function() {
            if (length(private$.pair_n) == 0)
                return(sprintf(.("%d observations"), nrow(private$.prepareData())))
            lo <- min(private$.pair_n); hi <- max(private$.pair_n)
            if (lo == hi) return(sprintf(.("%d observations"), hi))
            # .pair_n pools the per-GROUP Ns when a Split By variable is set, so a
            # spread there means unequal group sizes, NOT pairwise deletion. Saying
            # "pairwise deletion" for a complete, grouped dataset misattributes the
            # split to missing data - and it fired under the listwise DEFAULT.
            if (!is.null(self$options$grvar))
                sprintf(.("%d to %d observations per group"), lo, hi)
            else if (identical(self$options$naHandling, "pairwise"))
                sprintf(.("%d to %d observations per pair (pairwise deletion)"), lo, hi)
            else
                sprintf(.("%d to %d observations per pair"), lo, hi)
        },

        # Coefficient symbol for the method actually run. Printing "r" for a
        # Spearman or Bayesian estimate mislabels the statistic.
        .coefSymbol = function(typestatistics) {
            switch(typestatistics,
                "parametric"    = "r",
                "nonparametric" = "rho",
                "robust"        = "r (winsorized)",
                "bayes"         = "rho (median posterior)",
                "r")
        },

        # Generate clinical interpretation of correlation results.
        #
        # Consumes the same .computeCorrelations() result the table does, so the
        # narrative, the table and the plot describe one set of numbers. The
        # previous version ran its own stats::cor.test loop, which reported
        # Pearson values under the robust and Bayes labels, emitted "Unable to
        # calculate correlations with the selected options" for EVERY partial
        # correlation, counted significance against a hard-coded 0.05 rather
        # than the user's significance level, and printed "r =" for every
        # method.
        .generateInterpretation = function(mydata, options_data) {
            if (length(options_data$myvars) < 2) return()

            # With a Split By variable the plot and the table are computed PER
            # GROUP, but this panel pooled every row. Groups that correlate in
            # opposite directions cancel: +0.99 and -0.99 pool to +0.38, so the
            # narrative could report a weak, non-significant association over a
            # figure showing two near-perfect ones. Summarise each group.
            if (!is.null(self$options$grvar)) {
                grp_var  <- private$.resolveName(self$options$grvar, mydata)
                grp_vals <- mydata[[grp_var]]
                lvls <- unique(grp_vals[!is.na(grp_vals)])
                if (is.factor(grp_vals)) lvls <- lvls[order(as.integer(lvls))]

                parts <- character()
                for (lvl in lvls) {
                    keep <- !is.na(grp_vals) & grp_vals == lvl
                    sub_res <- private$.computeCorrelations(
                        mydata[keep, , drop = FALSE], options_data)
                    if (is.null(sub_res) || nrow(sub_res) == 0) next
                    sig <- if (identical(options_data$typestatistics, "bayes"))
                               (!is.na(sub_res$bf) & sub_res$bf >= 3)
                           else (!is.na(sub_res$p_adj) & sub_res$p_adj < options_data$siglevel)
                    strong <- !is.na(sub_res$r) & abs(sub_res$r) >= 0.5
                    parts <- c(parts, sprintf(
                        .("<li><strong>%s</strong> (n = %s): %d of %d pairs strong (|%s| \u2265 0.5), %d meeting the significance threshold. Strongest: %s = %s (%s vs %s).</li>"),
                        htmltools::htmlEscape(as.character(lvl)),
                        base::format(max(sub_res$n, na.rm = TRUE)),
                        sum(strong), nrow(sub_res),
                        private$.coefSymbol(options_data$typestatistics),
                        sum(sig),
                        private$.coefSymbol(options_data$typestatistics),
                        sprintf("%.3f", sub_res$r[which.max(abs(sub_res$r))]),
                        htmltools::htmlEscape(sub_res$var1[which.max(abs(sub_res$r))]),
                        htmltools::htmlEscape(sub_res$var2[which.max(abs(sub_res$r))])))
                }

                self$results$interpretation$setContent(paste0(
                    "<h4>", .("Correlation Analysis Summary"), "</h4>",
                    "<p>", sprintf(.("Correlations are computed separately within each level of <strong>%s</strong>, matching the figure and the table. Pooling the groups would be misleading whenever they differ in direction."),
                                   htmltools::htmlEscape(self$options$grvar)), "</p>",
                    "<ul>", paste(parts, collapse = ""), "</ul>",
                    "<p><strong>", .("Interpretation Notes:"), "</strong><br>",
                    "\u2022 ", .("Compare the groups deliberately: a correlation present in one group and absent (or reversed) in another is a finding in itself, not noise."),
                    "<br>\u2022 ", .("Remember that correlation does not imply causation."), "</p>"))
                return()
            }

            res <- private$.computeCorrelations(mydata, options_data)
            if (is.null(res) || nrow(res) == 0) {
                self$results$interpretation$setContent(
                    .("<p>Unable to calculate correlations with the selected options.</p>"))
                return()
            }
            if (all(is.na(res$n)) || max(res$n, na.rm = TRUE) < 3) {
                self$results$interpretation$setContent(
                    .("Insufficient data for correlation interpretation."))
                return()
            }

            is_bayes   <- options_data$typestatistics == "bayes"
            alpha      <- options_data$siglevel
            # correlation::correlation reports the method it actually ran, so
            # the label cannot drift from the computation.
            method_display <- unique(res$method)[1]
            symbol     <- private$.coefSymbol(options_data$typestatistics)
            n_vars     <- length(options_data$myvars)
            n_obs      <- max(res$n, na.rm = TRUE)
            n_min      <- min(res$n, na.rm = TRUE)
            n_corr     <- nrow(res)
            partial_on <- isTRUE(options_data$partial) && n_vars >= 3

            # Significance is judged on the ADJUSTED p-value at the user's
            # significance level - the same rule the plot uses to cross out
            # cells. Bayesian output has no p-value; BF10 >= 3 is the
            # conventional threshold for "moderate evidence".
            sig <- if (is_bayes) (!is.na(res$bf) & res$bf >= 3)
                   else (!is.na(res$p_adj) & res$p_adj < alpha)
            strong <- !is.na(res$r) & abs(res$r) >= 0.5

            sig_label <- if (is_bayes)
                .("Pairs with at least moderate evidence (BF<sub>10</sub> \u2265 3)")
            else if (options_data$padjustmethod == "none")
                sprintf(.("Significant correlations (unadjusted p &lt; %s)"), base::format(alpha))
            else
                sprintf(.("Significant correlations (%s-adjusted p &lt; %s)"),
                        private$.padjustLabel(options_data$padjustmethod), base::format(alpha))

            n_label <- if (n_min == n_obs) sprintf(.("%d observations"), n_obs)
                       else if (identical(options_data$naHandling, "pairwise"))
                           sprintf(.("%d to %d observations per pair (pairwise deletion)"), n_min, n_obs)
                       else sprintf(.("%d to %d observations per pair"), n_min, n_obs)

            correlation_type_info <- ""
            if (partial_on) {
                correlation_type_info <- paste0(
                    "<p><strong>", .("Partial Correlations Explained:"), "</strong><br>",
                    "\u2022 ", .("Partial correlations show the relationship between two variables while controlling for all other variables in the analysis"), "<br>",
                    "\u2022 ", .("Unlike zero-order (regular) correlations, partial correlations remove the influence of confounding variables"), "<br>",
                    "\u2022 ", .("Values closer to zero indicate that the relationship is largely explained by other variables"), "<br>",
                    "\u2022 ", .("Strong partial correlations suggest a direct relationship that persists even after controlling for other factors"), "</p>"
                )
            } else if (isTRUE(options_data$partial) && n_vars < 3) {
                correlation_type_info <- paste0(
                    "<p><strong>", .("Partial Correlations Note:"), "</strong><br>",
                    "\u2022 ", .("Partial correlations require at least 3 variables to control for confounding effects"), "<br>",
                    "\u2022 ", .("With fewer than 3 variables, regular (zero-order) correlations are computed instead"), "</p>"
                )
            }

            interpretation <- paste0(
                "<h4>", .("Correlation Analysis Summary"), "</h4>",
                "<p><strong>", .("Analysis Details:"), "</strong><br>",
                "\u2022 ", sprintf(.("Variables analyzed: %d"), n_vars), "<br>",
                "\u2022 ", sprintf(.("Sample size: %s"), n_label), "<br>",
                "\u2022 ", sprintf(.("Method: %s"), htmltools::htmlEscape(method_display)), "<br>",
                "\u2022 ", sprintf(.("Correlation type: %s"), if (partial_on) .("Partial") else .("Zero-order")), "<br>",
                "\u2022 ", sprintf(.("Total correlations: %d"), n_corr), "</p>",

                correlation_type_info,

                "<p><strong>", .("Key Findings:"), "</strong><br>",
                "\u2022 ", sprintf(.("Strong correlations (|%s| \u2265 0.5): %d"), symbol, sum(strong)), "<br>",
                "\u2022 ", sig_label, ": ", sum(sig), "</p>"
            )

            # Details for the strongest correlations
            if (any(strong)) {
                top <- res[strong, , drop = FALSE]
                top <- top[order(abs(top$r), decreasing = TRUE), , drop = FALSE]
                top <- utils::head(top, 5)

                interpretation <- paste0(interpretation,
                                         "<p><strong>", .("Notable Correlations:"), "</strong></p><ul>")
                for (i in seq_len(nrow(top))) {
                    interpretation <- paste0(
                        interpretation, "<li>",
                        private$.interpretCorrelation(
                            r          = top$r[i],
                            p_value    = if (is_bayes) NA_real_ else top$p_adj[i],
                            bf         = top$bf[i],
                            method     = method_display,
                            symbol     = symbol,
                            alpha      = alpha,
                            adjusted   = options_data$padjustmethod != "none",
                            var1       = top$var1[i],
                            var2       = top$var2[i]
                        ),
                        "</li>")
                }
                interpretation <- paste0(interpretation, "</ul>")
            }

            interpretation <- paste0(
                interpretation,
                "<p><strong>", .("Interpretation Notes:"), "</strong><br>",
                if (sum(sig) > 0) {
                    .("\u2022 These correlations are exploratory and hypothesis-generating; they describe association within this sample only.")
                } else {
                    .("\u2022 No correlations reached the chosen threshold. Consider a larger sample size or different variables.")
                },
                "<br>\u2022 ", .("Remember that correlation does not imply causation."),
                "<br>\u2022 ", .("Consider potential confounding variables in your analysis."),
                if (partial_on) {
                    paste0("<br>\u2022 ", .("Partial correlations help identify direct relationships by controlling for confounding variables in your dataset."))
                } else {
                    ""
                },
                "</p>"
            )

            self$results$interpretation$setContent(interpretation)
        },

        # run ----
        
.run = function() {

    # Initialize warnings list (avoid Notice serialization errors)
    private$.warnings <- list()

    # Initial Message ----
    if ( is.null(self$options$dep) || length(self$options$dep) < 2 ) {

        # TODO ----

        self$results$todo$setContent(private$.welcomeMessage())

        return()

    } else {

        # Clear welcome message and show processing message
        todo <- .("<br>You have selected to use a correlation matrix to compare continuous variables.<br><hr>")

        self$results$todo$setContent(todo)

        if (nrow(self$data) == 0) {
            private$.addWarning("ERROR", 'Data contains no complete rows after filtering. Please check for missing values.')
            private$.displayWarnings()
            return()
        }

        # Validate inputs before processing
        if (!private$.validateInputs()) {
            private$.displayWarnings()
            return()
        }
        private$.validateClinicalInputs()

        # Pre-process data and options for performance
        mydata <- private$.prepareData()
        options_data <- private$.prepareOptions()

        # Populate the correlation table exactly once here (grouped when a
        # grouping variable is set, ungrouped otherwise). Previously both
        # .plot and .plot2 populated it, each doing deleteRows() first, which
        # made the final contents order-dependent (overwritten) when grvar was
        # set.
        private$.populateTable(
            mydata, options_data,
            group = if (!is.null(self$options$grvar)) self$options$grvar else NULL
        )

        if (self$options$showexplanations) {
            private$.generateAboutContent()
            private$.generateSummary(options_data)
            private$.checkAssumptions(options_data)
            private$.generateInterpretation(mydata, options_data)

        }

        # Missing-data disclosure. Emitted here rather than from the memoised
        # .prepareData() so it survives an option-only change (cache hit).
        if (!is.na(private$.n_before) && private$.n_before > private$.n_after) {
            n_dropped <- private$.n_before - private$.n_after
            private$.addWarning("WARNING", sprintf(
                .('%d of %d rows (%s%%) were excluded because they had a missing value in at least one selected variable. %d rows were analysed.'),
                n_dropped, private$.n_before,
                base::format(round(100 * n_dropped / private$.n_before, 1)),
                private$.n_after))
        } else if (identical(self$options$naHandling, "pairwise") &&
                   length(private$.pair_n) > 0 &&
                   min(private$.pair_n) < private$.n_before) {
            # Under pairwise deletion no row is dropped from the frame, so the
            # branch above can never fire - but each correlation is still
            # computed on fewer rows than the data set holds.
            private$.addWarning("WARNING", sprintf(
                .('Pairwise deletion: each correlation uses only the rows complete for that pair, so the per-pair sample size ranges from %d to %d of %d rows.'),
                min(private$.pair_n), max(private$.pair_n), private$.n_before))
        }

        # Completion notice. This lives here rather than in .plot() because
        # .run() is the only place that renders private$.warnings.
        # Count the coefficients actually produced, not the table rows: a
        # constant variable yields a row of NAs that was being counted as a
        # computed correlation.
        if (private$.n_valid_pairs > 0) {
            corr_type <- if (isTRUE(options_data$partial))
                .("partial") else .("zero-order")
            private$.addWarning("INFO", sprintf(
                .('Computed %d %s %s of %d variables.'),
                private$.n_valid_pairs, corr_type,
                private$.methodLabel(self$options$typestatistics),
                length(options_data$myvars)))
        }

        # Display all collected warnings at the end
        private$.displayWarnings()

    }
},

.generateAboutContent = function() {
    about_content <- glue::glue("
    <h3>About Correlation Matrix</h3>
    <hr>
    <p><b>Purpose:</b> This analysis creates a correlation matrix to visualize the
    relationships between multiple continuous variables. It helps in understanding
    the direction, magnitude, and significance of the associations between pairs of
    variables.</p>

    <p><b>When to Use:</b></p>
    <ul>
        <li><b>Exploratory Data Analysis:</b> To get a quick overview of the
        relationships between a set of variables.</li>
        <li><b>Feature Selection:</b> To identify highly correlated variables that
        may be redundant in a predictive model.</li>
        <li><b>Publication:</b> To create a publication-ready summary of the
        associations between your variables of interest.</li>
    </ul>

    <p><b>Key Features:</b></p>
    <ul>
        <li>Supports Pearson, Spearman, robust, and Bayesian correlation methods.</li>
        <li>Can be split by a grouping variable to compare correlations across
        subgroups.</li>
        <li>Can compute partial correlations to control for confounding variables.</li>
        <li>Provides options for p-value adjustment, theming, and customizing the
        plot.</li>
    </ul>
    <hr>
    ")
    self$results$about$setContent(about_content)
},

        .generateSummary = function(options_data) {
    
    n_vars <- length(options_data$myvars)
    # Report the N the correlations were actually computed on. Under pairwise
    # deletion nrow() of the (unfiltered) frame is larger than every pair's N.
    n_label <- private$.pairNLabel()
    # Say whether that N is pooled or per group: with a Split By variable the
    # figure and table are per group, so a bare pooled count misdescribes them.
    if (!is.null(self$options$grvar))
        n_label <- sprintf(.("%s, analysed separately within each level of %s"),
                           n_label, htmltools::htmlEscape(self$options$grvar))
    method_name <- private$.methodName(options_data$typestatistics)

    summary_text <- glue::glue("
    <h4>Analysis Summary</h4>
    <p><b>Variables analyzed:</b> {n_vars}</p>
    <p><b>Sample size:</b> {n_label}</p>
    <p><b>Method:</b> {method_name} correlation</p>
    <p><b>Correlation type:</b> {if(options_data$partial && n_vars >= 3) 'Partial' else 'Zero-order'}</p>
    ")
    
    self$results$summary$setContent(summary_text)
},

# Single correlation engine, shared by the table and the clinical
# interpretation. This is deliberately the SAME call ggstatsplot::ggcorrmat
# makes internally (correlation::correlation with winsorize/bayesian/partial
# switched off the `type` argument), so the numbers in the table cannot drift
# from the numbers drawn in the plot. The previous implementation hand-rolled
# stats::cor.test, which silently reported Pearson coefficients for the
# "robust" and "bayes" methods, ignored `partial`, and never applied the
# p-value adjustment the plot uses to mark non-significant cells.
#
# p_adjust is requested as "none" and the adjustment applied here instead, so
# both the unadjusted and the adjusted p-value are available; this reproduces
# correlation::correlation's own adjustment exactly (verified against
# stats::p.adjust for holm and bonferroni).
.computeCorrelations = function(df, options_data) {
    myvars <- options_data$myvars
    resolved <- vapply(myvars, function(v) private$.resolveName(v, df), character(1))
    if (!all(resolved %in% names(df))) return(NULL)

    cor_data <- df[, resolved, drop = FALSE]
    for (cc in names(cor_data)) cor_data[[cc]] <- jmvcore::toNumeric(cor_data[[cc]])
    # Report user-facing variable names, not jamovi's internal (possibly
    # B64-encoded) column names.
    names(cor_data) <- myvars

    type <- options_data$typestatistics

    # Bayesian estimates come from BayesFactor's MCMC sampler: unseeded, two
    # calls on the same data differ in the third decimal, so the table and the
    # plot were two independent draws of the same quantity and re-running the
    # same analysis produced different numbers. A fixed seed makes both
    # reproducible and identical; withr restores the caller's RNG stream.
    res <- tryCatch(
        suppressWarnings(private$.withBayesSeed(type, correlation::correlation(
            data             = cor_data,
            method           = if (type == "nonparametric") "spearman" else "pearson",
            p_adjust         = "none",
            ci               = options_data$conflevel,
            bayesian         = type == "bayes",
            bayesian_prior   = 0.707,
            partial          = options_data$partial,
            partial_bayesian = type == "bayes" && options_data$partial,
            winsorize        = if (type == "robust") 0.2 else FALSE
        ))),
        error = function(e) NULL
    )
    if (is.null(res) || nrow(as.data.frame(res)) == 0) return(NULL)

    res <- as.data.frame(res)
    # Bayesian output names the estimate `rho` and carries no p-value.
    est   <- if ("r" %in% names(res)) res$r else res$rho
    p_raw <- if ("p" %in% names(res)) res$p else rep(NA_real_, nrow(res))

    data.frame(
        var1      = as.character(res$Parameter1),
        var2      = as.character(res$Parameter2),
        n         = if ("n_Obs" %in% names(res)) as.integer(res$n_Obs) else NA_integer_,
        r         = as.numeric(est),
        conf_low  = if ("CI_low"  %in% names(res)) as.numeric(res$CI_low)  else NA_real_,
        conf_high = if ("CI_high" %in% names(res)) as.numeric(res$CI_high) else NA_real_,
        p         = as.numeric(p_raw),
        p_adj     = if (all(is.na(p_raw))) as.numeric(p_raw)
                    else stats::p.adjust(p_raw, method = options_data$padjustmethod),
        bf        = if ("BF" %in% names(res)) as.numeric(res$BF) else NA_real_,
        method    = as.character(res$Method),
        stringsAsFactors = FALSE
    )
},

.populateTable = function(mydata, options_data, group = NULL) {
    table <- self$results$table
    # Clear existing rows - jamovi tables use deleteRows(), not clear()
    table$deleteRows()
    private$.pair_n <- integer(0)
    private$.n_valid_pairs <- 0L

    # Missing data: correlation::correlation performs pairwise complete-case
    # deletion, and when naHandling == "listwise" the incoming data has already
    # had incomplete rows dropped in .prepareData. The reported N is per pair,
    # so the two settings are distinguishable in the output.
    add_rows_for_subset <- function(df, grp_label = "All") {
        res <- private$.computeCorrelations(df, options_data)
        if (is.null(res)) return(invisible(NULL))
        private$.pair_n <- c(private$.pair_n, res$n[!is.na(res$n)])
        private$.n_valid_pairs <- private$.n_valid_pairs + sum(!is.na(res$r))
        for (i in seq_len(nrow(res))) {
            table$addRow(
                rowKey = table$rowCount + 1L,
                values = list(
                    var1      = res$var1[i],
                    var2      = res$var2[i],
                    n         = res$n[i],
                    r         = res$r[i],
                    conf_low  = res$conf_low[i],
                    conf_high = res$conf_high[i],
                    p         = res$p[i],
                    p_adj     = res$p_adj[i],
                    bf        = res$bf[i],
                    method    = res$method[i],
                    group     = grp_label
                )
            )
        }
    }

    if (!is.null(group)) {
        grp_var <- private$.resolveName(group, mydata)
        grp_vals <- mydata[[grp_var]]
        # Rows with a missing group value belong to no subgroup. Dropping the
        # NA level matters under pairwise handling, where .prepareData leaves
        # those rows in place: `x == NA` yields an all-NA index, and NA-index
        # subsetting produced a phantom "NA" group of all-NA correlations.
        n_missing_group <- sum(is.na(grp_vals))
        if (n_missing_group > 0)
            private$.addWarning("WARNING", sprintf(
                '%d row(s) have no value for the grouping variable and are excluded from the grouped correlation table.',
                n_missing_group))

        lvls <- unique(grp_vals[!is.na(grp_vals)])
        if (is.factor(grp_vals)) lvls <- lvls[order(as.integer(lvls))]
        for (lvl in lvls) {
            keep <- !is.na(grp_vals) & grp_vals == lvl
            add_rows_for_subset(mydata[keep, , drop = FALSE], grp_label = as.character(lvl))
        }
    } else {
        add_rows_for_subset(mydata, grp_label = "All")
    }

    # Say which p-value the plot uses to cross out cells, so the table and the
    # figure cannot be read as disagreeing.
    if (options_data$typestatistics == "bayes") {
        table$setNote("padj", .("Bayesian correlations report the median posterior estimate and BF<sub>10</sub>; no p-value is defined."))
    } else if (options_data$padjustmethod == "none") {
        table$setNote("padj", .("No correction for multiple comparisons was applied, so <b>p (adjusted)</b> repeats the unadjusted p-value. Each additional variable adds several pairwise tests."))
    } else {
        table$setNote("padj", sprintf(
            .("<b>p (adjusted)</b> applies the %s correction across pairwise tests within each split panel (not across panels). This is the p-value the plot uses to mark cells as non-significant at %s."),
            private$.padjustLabel(options_data$padjustmethod),
            base::format(options_data$siglevel)))
    }
},

# Evaluate `expr` under the shared Bayesian seed when the selected method
# samples, and untouched otherwise (nothing else here is stochastic, so
# seeding it would only disturb the caller's RNG).
.withBayesSeed = function(typestatistics, expr) {
    if (identical(typestatistics, "bayes"))
        withr::with_seed(private$.BAYES_SEED, expr)
    else
        expr
},

# Human-readable name for a stats::p.adjust method.
.padjustLabel = function(method) {
    switch(method,
        holm       = .("Holm"),
        hochberg   = .("Hochberg"),
        hommel     = .("Hommel"),
        bonferroni = .("Bonferroni"),
        BH         = .("Benjamini-Hochberg (FDR)"),
        BY         = .("Benjamini-Yekutieli"),
        method)
},

.checkAssumptions = function(options_data) {
    
    # Name the method that actually ran, and show only its assumptions: the
    # panel used to print the raw option token ("robust correlation") and then
    # all four bullets regardless of the selection.
    method_name <- private$.methodName(options_data$typestatistics)
    method_bullet <- switch(options_data$typestatistics,
        "parametric" = "<li><b>Pearson:</b> Assumes that the variables are approximately
        normally distributed and that their relationship is linear.</li>",
        "nonparametric" = "<li><b>Spearman:</b> Does not assume a specific distribution.
        It is based on the ranks of the data and can detect monotonic (but not
        necessarily linear) relationships.</li>",
        "robust" = "<li><b>Winsorized Pearson:</b> Pearson's r computed after
        winsorizing the most extreme 20% of observations in each tail, so
        outliers are pulled in rather than removed. Less sensitive to outliers
        than Pearson, but it still measures a linear association.</li>",
        "bayes" = "<li><b>Bayesian Pearson:</b> Provides a measure of evidence for the
        presence of a correlation, but the interpretation depends on the chosen prior.</li>",
        "")

    assumptions_content <- glue::glue("
    <h3>Statistical Assumptions & Warnings</h3>
    <hr>
    <p><b>For {method_name} correlation:</b></p>
    <ul>
        {method_bullet}
    </ul>
    <p><b>General Warnings:</b></p>
    <ul>
        <li>Correlation does not imply causation.</li>
        <li>Outliers can have a large influence on the correlation coefficient,
        especially for Pearson correlation.</li>
        <li>Restricting the range of the variables can artificially lower the
        correlation coefficient.</li>
    </ul>
    <hr>
    ")
    
    self$results$assumptions$setContent(assumptions_content)
},

.plot = function(image, ggtheme, theme, ...) {
            # Check for sufficient variables before any processing
            if (is.null(self$options$dep) || length(self$options$dep) < 2)
                return()
            
            # Use shared validation ----
            if (!private$.validateInputs())
                return()
        
            # Add clinical validation warnings
            private$.validateClinicalInputs()
        
            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()
        
            typestatistics <- options_data$typestatistics
            myvars <- options_data$myvars
        
        
            # ggcorrmat ----
            # https://www.indrapatil.com/ggstatsplot/reference/ggcorrmat.html

            # Checkpoint before expensive correlation computation
            private$.checkpoint()

            # Skip heavy plotting in testthat runs; the table is populated once
            # in .run(), interpretation is still generated here.
            if (Sys.getenv("TESTTHAT") == "true") {
                private$.generateInterpretation(mydata, options_data)
                return(TRUE)
            }

            plot <- private$.withBayesSeed(options_data$typestatistics, ggstatsplot::ggcorrmat(
                data = mydata,
                cor.vars = myvars,
                cor.vars.names = NULL,
                matrix.type = options_data$matrixtype,
                type = options_data$typestatistics,
                partial = options_data$partial,
                # `tr` (trim, default 0.2) is what controls the robust
                # estimator here. The former `beta = 0.1` was the bending
                # constant of the percentage-bend coefficient used by older
                # ggstatsplot and is silently discarded by 1.0.0.
                # Decimal places: newer ggstatsplot renamed `k` -> `digits`.
                # Pass both so the option is honored regardless of version
                # (the unused name is harmlessly absorbed by `...`).
                k = options_data$k,
                digits = options_data$k,
                sig.level = options_data$siglevel,
                conf.level = options_data$conflevel,
                bf.prior = 0.707,
                p.adjust.method = options_data$padjustmethod,
                pch = "cross",
                ggcorrplot.args = options_data$ggcorrplot.args,
                package = "RColorBrewer",
                palette = "Dark2",
                colors = options_data$colors,
                ggplot.component = NULL,
                title = options_data$title,
                subtitle = options_data$subtitle,
                caption = options_data$caption
            ))

            # Correlation table is populated once in .run(); only the plot and
            # interpretation are produced here.
            # Generate clinical interpretation ----
            private$.generateInterpretation(mydata, options_data)

            # The completion notice is emitted from .run(), which is the only
            # place that renders private$.warnings. Adding it here appended to a
            # list that .displayWarnings() had already consumed, so it never
            # reached the user.

            # Print Plot ----

            print(plot)
            TRUE

        },
        

        .plot2 = function(image, ggtheme, theme, ...) {
            # Check for sufficient variables before any processing
            if (is.null(self$options$dep) || length(self$options$dep) < 2)
                return()
                
            # Use shared validation with additional grouping variable check ----
            if (!private$.validateInputs() || is.null(self$options$grvar))
                return()

            # Add clinical validation warnings
            private$.validateClinicalInputs()

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()

            typestatistics <- options_data$typestatistics
            myvars <- options_data$myvars


            # grouped_ggcorrmat ----
            # https://www.indrapatil.com/ggstatsplot/reference/grouped_ggcorrmat.html

            if ( !is.null(self$options$grvar) ) {

                grvar <- self$options$grvar

                # Prepare annotation arguments for modern patchwork API
                annotation_args <- list()
                if (!is.null(options_data$title)) {
                    annotation_args$title <- options_data$title
                }
                if (!is.null(options_data$subtitle)) {
                    annotation_args$subtitle <- options_data$subtitle
                }
                if (!is.null(options_data$caption)) {
                    annotation_args$caption <- options_data$caption
                }

                # Checkpoint before expensive grouped correlation computation
                private$.checkpoint()

                if (Sys.getenv("TESTTHAT") == "true") {
                    private$.generateInterpretation(mydata, options_data)
                    return(TRUE)
                }

                plot2 <- private$.withBayesSeed(options_data$typestatistics, ggstatsplot::grouped_ggcorrmat(
                    data = mydata,
                    cor.vars = myvars,
                    cor.vars.names = NULL,
                    grouping.var = !!rlang::sym(grvar),
                    plotgrid.args = list(),
                    annotation.args = annotation_args,
                    type = options_data$typestatistics,
                    matrix.type = options_data$matrixtype,
                    partial = options_data$partial,
                    # See the note in .plot(): `beta` no longer applies.
                    # Decimal places: newer ggstatsplot renamed `k` -> `digits`.
                    # Pass both so the option is honored regardless of version
                    # (the unused name is harmlessly absorbed by `...`).
                    k = options_data$k,
                    digits = options_data$k,
                    sig.level = options_data$siglevel,
                    conf.level = options_data$conflevel,
                    bf.prior = 0.707,
                    p.adjust.method = options_data$padjustmethod,
                    pch = "cross",
                    ggcorrplot.args = options_data$ggcorrplot.args,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    colors = options_data$colors,
                    ggplot.component = NULL
                ))

            }

            # Correlation table is populated once in .run(); only the grouped
            # plot and interpretation are produced here.
            # Generate clinical interpretation ----
            private$.generateInterpretation(mydata, options_data)

            # Print Plot ----

            print(plot2)
            TRUE

        }

    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for Correlation Matrix analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            dep <- self$options$dep

            if (is.null(dep) || length(dep) == 0)
                return('')

            # Build the argument list in option-declaration order.
            #
            # Every variable-name option (single OptionVariable or multi-variable
            # OptionVariables) is emitted as a deparse()'d string literal. deparse()
            # produces valid, fully-escaped R for names containing spaces, quotes or
            # backslashes (e.g. `Tumor Grade`); jmvcore's default sourcify would emit
            # some of these as bare, unquoted symbols and yield invalid syntax.
            # Detecting the option by CLASS (not by name) means any variable option
            # added later is escaped automatically.
            #
            # Variables are NOT re-emitted through private$.asArgs() - doing so
            # previously duplicated them in the generated syntax (the "double
            # variables" bug). All non-variable options keep jmvcore's per-option
            # sourcify so formatting stays consistent with jamovi.
            args <- character(0)
            for (option in private$.options$options) {
                if (option$name == 'data')
                    next
                if (inherits(option, 'OptionVariable') || inherits(option, 'OptionVariables')) {
                    val <- option$value
                    if (!is.null(val) && length(val) > 0)
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
            paste0(pkg_name, '::jjcorrmat(\n    data = data,\n    ',
                   paste(args, collapse = ',\n    '), ')')
        }
    ) # End of public list
) else NULL
