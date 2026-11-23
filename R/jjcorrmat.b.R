#' @title Correlation Matrix
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#'
#' @return An \code{R6} class generator object for the \code{jjcorrmatClass} backend; used internally by the jamovi analysis wrapper and not called directly.


jjcorrmatClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjcorrmatClass",
    inherit = jjcorrmatBase,
    private = list(

        # Seed for the Bayesian sampler. Shared by the table and both plot
        # methods so a Bayesian correlation matrix is reproducible and the two
        # outputs report the same draw.
        .BAYES_SEED = 20250101L,

        # The seed actually used: the user's `bayesseed` when set, otherwise the
        # historical literal above so existing saved analyses reproduce exactly.
        .bayesSeed = function() {
            s <- self$options$bayesseed
            if (is.null(s) || is.na(s)) private$.BAYES_SEED else as.integer(s)
        },

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
        .winsor_collapsed = character(0),
        .effective_partial = FALSE,
        .invalid_colors = character(0),
        .corr_cache = list(),
        .data_hash = NULL,
        .options_hash = NULL,
        .warnings = list(),  # Collect warnings for HTML display (avoids Notice serialization errors)

        # Clear tabular and narrative outputs so an early error exit never leaves
        # stale results from a previous run on a reused R6 analysis instance.
        .clearOutputs = function() {
            .deleteTableRows(self$results$table)
            self$results$interpretation$setContent("")
            self$results$summary$setContent("")
            self$results$assumptions$setContent("")
            self$results$about$setContent("")
        },

        # Helper function to add warnings without Notice objects
        .addWarning = function(type, message) {
            private$.warnings[[length(private$.warnings) + 1]] <- list(
                type = type,
                message = message
            )
        },

        # Display all collected warnings as HTML, ordered by severity priority
        # (ERROR -> STRONG_WARNING -> WARNING -> INFO) while preserving insertion
        # order within each tier.
        .displayWarnings = function() {
            if (length(private$.warnings) == 0) {
                self$results$warnings$setVisible(FALSE)
                return()
            }

            severity_rank <- c(ERROR = 1L, STRONG_WARNING = 2L, WARNING = 3L, INFO = 4L)
            ranks <- vapply(private$.warnings, function(w) {
                r <- severity_rank[[w$type]]
                if (is.null(r)) 5L else r
            }, integer(1))
            ordered_warnings <- private$.warnings[order(ranks, seq_along(ranks))]

            warning_html <- "<div style='margin: 10px 0;'>"

            for (warning in ordered_warnings) {
                if (warning$type == "ERROR") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; padding: 10px; margin: 5px 0; border-radius: 4px; color: inherit;'>",
                        "<strong style='color: inherit;'> ERROR:</strong> <span style='color: inherit;'>", warning$message, "</span>",
                        "</div>")
                } else if (warning$type == "STRONG_WARNING") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ff9800; padding: 10px; margin: 5px 0; border-radius: 4px; color: inherit;'>",
                        "<strong style='color: inherit;'> STRONG WARNING:</strong> <span style='color: inherit;'>", warning$message, "</span>",
                        "</div>")
                } else if (warning$type == "WARNING") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: rgba(255, 203, 33, 0.14); border-left: 4px solid #ffc107; padding: 10px; margin: 5px 0; border-radius: 4px; color: inherit;'>",
                        "<strong style='color: inherit;'> WARNING:</strong> <span style='color: inherit;'>", warning$message, "</span>",
                        "</div>")
                } else if (warning$type == "INFO") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: rgba(33, 163, 188, 0.21); border-left: 4px solid #0c5460; padding: 10px; margin: 5px 0; border-radius: 4px; color: inherit;'>",
                        "<strong style='color: inherit;'> INFO:</strong> <span style='color: inherit;'>", warning$message, "</span>",
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
            paste0(
                "<div style='background-color: rgba(33, 163, 188, 0.10); border-left: 4px solid #1976d2; padding: 14px 16px; margin: 10px 0; border-radius: 4px; color: inherit;'>",
                "<h3 style='margin-top: 0; color: inherit;'>", .("Correlation Matrix Analysis"), "</h3>",
                "<p><strong>", .("What this does:"), "</strong> ", .("Analyzes linear, rank-based, robust, or Bayesian relationships between continuous variables (e.g., biomarker levels, lab values, imaging metrics)."), "</p>",
                "<p><strong>", .("Quick Start:"), "</strong></p>",
                "<ol style='margin: 6px 0 10px 18px;'>",
                "<li>", .("Select 2 or more continuous variables."), "</li>",
                "<li>", .("Choose a correlation method (Pearson, Spearman, 20% Winsorized Pearson, or Bayesian Pearson)."), "</li>",
                "<li>", .("Optionally split by a categorical variable (e.g., tumor grade or treatment arm)."), "</li>",
                "<li>", .("Enable Partial correlations (with 3+ variables) to control for confounding variables."), "</li>",
                "</ol>",
                "<p><strong>", .("Correlation Types:"), "</strong><br>",
                "\u2022 <strong>", .("Zero-order:"), "</strong> ", .("Direct association between each pair of variables."), "<br>",
                "\u2022 <strong>", .("Partial:"), "</strong> ", .("Association between each pair while controlling for all other selected variables."), "</p>",
                "<p style='margin-bottom: 0; font-size: 0.92em;'>", .("Please cite jamovi and the packages listed in the References section below."), "</p>",
                "</div>"
            )
        },

        # init ----
        .init = function() {
            # Hide the pooled chart when Split By (grvar) is active so only the
            # grouped chart (plot2) is shown, matching the per-group table.
            self$results$plot$setVisible(is.null(self$options$grvar))

            # Show welcome message when no variables or insufficient variables are selected
            if (is.null(self$options$dep) || length(self$options$dep) < 2) {

                self$results$todo$setContent(private$.welcomeMessage())
                return()
            }



            # Use configurable plot dimensions with safe numeric bounds
            plotwidth <- suppressWarnings(as.integer(self$options$plotwidth))
            if (length(plotwidth) != 1L || is.na(plotwidth) || plotwidth < 300L || plotwidth > 1200L)
                plotwidth <- 600L
            plotheight <- suppressWarnings(as.integer(self$options$plotheight))
            if (length(plotheight) != 1L || is.na(plotheight) || plotheight < 300L || plotheight > 800L)
                plotheight <- 450L

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
            # Hash-based invalidation including selected column values so
            # in-place cell edits with unchanged dim/names never return stale data.
            mydata <- self$data
            resolve_name <- function(var) private$.resolveName(var, mydata)
            sel_cols <- character(0)
            if (!is.null(self$options$dep) && length(self$options$dep) > 0L)
                sel_cols <- c(sel_cols, vapply(self$options$dep, resolve_name, character(1)))
            if (!is.null(self$options$grvar))
                sel_cols <- c(sel_cols, resolve_name(self$options$grvar))
            sel_cols <- unique(sel_cols[sel_cols %in% names(mydata)])
            data_slice <- if (length(sel_cols) > 0L) mydata[, sel_cols, drop = FALSE] else NULL

            current_data_hash <- digest::digest(list(
                dep = self$options$dep,
                grvar = self$options$grvar,
                naHandling = self$options$naHandling,
                data_dim = dim(mydata),
                col_names = names(mydata),
                data_slice = data_slice
            ), algo = "md5")

            if (!is.null(private$.processedData) &&
                !is.null(private$.data_hash) &&
                private$.data_hash == current_data_hash &&
                !force_refresh) {
                return(private$.processedData)
            }

            for (v in self$options$dep) {
                nm <- resolve_name(v)
                x <- jmvcore::toNumeric(mydata[[nm]])
                if (!is.numeric(x)) jmvcore::reject(.("Correlation variables must be numeric."))
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
                private$.addWarning("ERROR", .('Data contains no complete rows. Please check for missing values in selected variables.'))
                return(FALSE)
            }

            # Enhanced validation for correlation analysis
            mydata <- self$data
            resolve_name <- function(var) private$.resolveName(var, mydata)

            # Check if variables exist in data
            for (var in self$options$dep) {
                varname <- resolve_name(var)
                if (!(varname %in% names(mydata))) {
                    private$.addWarning("ERROR", sprintf(.('Variable "%s" not found in data. Please select valid variables and re-run.'), htmltools::htmlEscape(var)))
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
                private$.addWarning("ERROR", sprintf(.('Correlation analysis requires numeric variables. The following are categorical: %s. Please select continuous numeric variables.'), htmltools::htmlEscape(paste(factor_warnings, collapse = ', '))))
                return(FALSE)
            }

            if (numeric_vars < 2) {
                private$.addWarning("ERROR", sprintf(.('Correlation analysis requires at least 2 numeric variables with sufficient variation. Found %d valid variable(s). Please select additional variables.'), numeric_vars))
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
                self$options$k, self$options$partial,
                self$options$lowcolor, self$options$midcolor, self$options$highcolor,
                self$options$title, self$options$subtitle, self$options$caption,
                self$options$plotwidth, self$options$plotheight,
                self$options$bayesseed, self$options$originaltheme,
                collapse = "_"
            )

            if (!is.null(private$.processedOptions) && private$.options_hash == current_options_hash && !force_refresh) {
                return(private$.processedOptions)
            }

            # Process and validate statistical options (backend backstops for
            # direct R callers invoking run(noThrow = TRUE) past option checks).
            valid_types <- c("parametric", "nonparametric", "robust", "bayes")
            valid_mtype <- c("upper", "lower", "full")
            valid_mmeth <- c("square", "circle")
            valid_padj  <- c("holm", "none", "hochberg", "hommel", "bonferroni", "BH", "BY")
            valid_na    <- c("listwise", "pairwise")

            typestatistics <- self$options$typestatistics
            if (!is.character(typestatistics) || length(typestatistics) != 1L || !(typestatistics %in% valid_types))
                jmvcore::reject(.("Invalid correlation method selected."))

            siglevel <- self$options$siglevel
            if (!is.numeric(siglevel) || length(siglevel) != 1L || !is.finite(siglevel) || siglevel < 0 || siglevel > 1)
                jmvcore::reject(.("Significance level must be between 0 and 1."))

            conflevel <- self$options$conflevel
            if (!is.numeric(conflevel) || length(conflevel) != 1L || !is.finite(conflevel) || conflevel < 0.5 || conflevel > 0.999)
                jmvcore::reject(.("Confidence / credible level must be between 0.5 and 0.999."))

            padjustmethod <- self$options$padjustmethod
            if (!is.character(padjustmethod) || length(padjustmethod) != 1L || !(padjustmethod %in% valid_padj))
                jmvcore::reject(.("Invalid p-value adjustment method selected."))

            matrixtype <- if (is.character(self$options$matrixtype) && length(self$options$matrixtype) == 1L && self$options$matrixtype %in% valid_mtype) self$options$matrixtype else "upper"
            matrixmethod <- if (is.character(self$options$matrixmethod) && length(self$options$matrixmethod) == 1L && self$options$matrixmethod %in% valid_mmeth) self$options$matrixmethod else "square"
            naHandling <- if (is.character(self$options$naHandling) && length(self$options$naHandling) == 1L && self$options$naHandling %in% valid_na) self$options$naHandling else "listwise"
            k_val <- suppressWarnings(as.integer(self$options$k))
            if (length(k_val) != 1L || is.na(k_val) || k_val < 0L || k_val > 5L) k_val <- 2L

            # Process variables - dep is already a list of variables.
            # De-duplicate: ggcorrmat's tidyselect drops repeats, so a variable
            # selected twice produced a structural r = 1.000 row in the table
            # (and an extra test in the multiplicity correction) that the figure
            # never showed.
            myvars <- unique(self$options$dep)

            # Adjust partial flag if insufficient variables
            partial_flag <- isTRUE(self$options$partial)
            if (partial_flag && length(myvars) < 3) {
                partial_flag <- FALSE
            }

            # Process text parameters
            title <- if (is.character(self$options$title) && length(self$options$title) == 1L && nzchar(self$options$title)) self$options$title else NULL
            subtitle <- if (is.character(self$options$subtitle) && length(self$options$subtitle) == 1L && nzchar(self$options$subtitle)) self$options$subtitle else NULL
            caption <- if (is.character(self$options$caption) && length(self$options$caption) == 1L && nzchar(self$options$caption)) self$options$caption else NULL

            # Process and validate colors so an invalid color string falls back
            # to the default with a warning instead of crashing plot rendering.
            raw_cols <- c(low = self$options$lowcolor, mid = self$options$midcolor, high = self$options$highcolor)
            def_cols <- c(low = "#E69F00", mid = "white", high = "#009E73")
            bad_cols <- character(0)
            valid_cols <- character(3)
            for (ci_idx in seq_along(raw_cols)) {
                col_val <- raw_cols[ci_idx]
                ok_col <- is.character(col_val) && length(col_val) == 1L && nzchar(col_val) &&
                    tryCatch({ grDevices::col2rgb(col_val); TRUE }, error = function(e) FALSE)
                if (ok_col) {
                    valid_cols[ci_idx] <- col_val
                } else {
                    valid_cols[ci_idx] <- def_cols[ci_idx]
                    bad_cols <- c(bad_cols, sprintf("%s='%s'", names(raw_cols)[ci_idx], as.character(col_val)))
                }
            }
            private$.invalid_colors <- bad_cols
            colors <- unname(valid_cols)

            # Process ggcorrplot.args
            # pch.cex must be carried over from ggstatsplot's own default
            # (list(method = "square", outline.color = "black", pch.cex = 14)):
            # passing this list REPLACES that default, and ggcorrplot's own
            # formal is 5, which draws the non-significance cross small enough
            # to sit on top of the printed coefficient ("0.02" renders "0X2").
            ggcorrplot.args <- list(
                method = matrixmethod,
                outline.color = "black",
                pch.cex = 14
            )

            # Cache the processed options
            options_list <- list(
                typestatistics = typestatistics,
                myvars = myvars,
                matrixtype = matrixtype,
                ggcorrplot.args = ggcorrplot.args,
                siglevel = siglevel,
                conflevel = conflevel,
                padjustmethod = padjustmethod,
                k = k_val,
                partial = partial_flag,
                naHandling = naHandling,
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

            # Strength and direction are ONE translatable phrase, not two words
            # glued by sprintf. Exact zero (within floating-point tolerance) is
            # neutral rather than "negative".
            abs_r <- abs(r)
            positive <- r > 0
            strength_phrase <-
                if (abs_r < 1e-12)      .("negligible (zero)")
                else if (abs_r >= 0.7)  { if (positive) .("strong positive") else .("strong negative") }
                else if (abs_r >= 0.5)  { if (positive) .("moderate-to-strong positive") else .("moderate-to-strong negative") }
                else if (abs_r >= 0.3)  { if (positive) .("moderate positive") else .("moderate negative") }
                else if (abs_r >= 0.1)  { if (positive) .("weak-to-moderate positive") else .("weak-to-moderate negative") }
                else                    { if (positive) .("very weak positive") else .("very weak negative") }

            # Determine the evidence statement. Bayesian analysis has no
            # p-value, so the Bayes factor carries the evidence using Jeffreys
            # (1961) / Lee & Wagenmakers (2013) conventions. Frequentist
            # significance is evaluated against `alpha` FIRST (so alpha < 0.001
            # cannot contradict itself), and `<` is HTML-escaped as `&lt;`.
            if (!is.na(bf) && is.na(p_value)) {
                is_notable <- bf >= 3
                bf_str <- base::format(signif(bf, 3), scientific = (bf >= 1e5 || (bf > 0 && bf < 1e-3)))
                significance <-
                    if (bf >= 10)        sprintf(.("with strong evidence for a correlation (BF<sub>10</sub> = %s)"), bf_str)
                    else if (bf >= 3)    sprintf(.("with moderate evidence for a correlation (BF<sub>10</sub> = %s)"), bf_str)
                    else if (bf > 1 / 3) sprintf(.("with anecdotal/inconclusive evidence (BF<sub>10</sub> = %s)"), bf_str)
                    else                 sprintf(.("with evidence favoring no correlation (BF<sub>10</sub> = %s)"), bf_str)
            } else if (is.na(p_value)) {
                is_notable <- FALSE
                significance <- .("of undetermined significance")
            } else {
                is_notable <- p_value < alpha
                # Format p with enough precision to distinguish p vs alpha even
                # when the user sets alpha < 0.001.
                pv <- if (p_value < 0.001 && alpha < 0.001) {
                    base::format(signif(p_value, 3), scientific = p_value < 1e-4)
                } else {
                    sprintf("%.3f", p_value)
                }
                significance <-
                    if (is_notable) {
                        if (p_value < 0.001 && alpha >= 0.001) {
                            if (adjusted) .("statistically significant (adjusted p &lt; 0.001)")
                            else          .("statistically significant (p &lt; 0.001)")
                        } else {
                            if (adjusted) sprintf(.("statistically significant (adjusted p = %s)"), pv)
                            else          sprintf(.("statistically significant (p = %s)"), pv)
                        }
                    } else {
                        if (adjusted) sprintf(.("not statistically significant (adjusted p = %s)"), pv)
                        else          sprintf(.("not statistically significant (p = %s)"), pv)
                    }
            }

            # Generate clinical interpretation
            interpretation <- sprintf(
                .("A %s correlation (%s = %s) between %s and %s that is %s, using %s."),
                strength_phrase, symbol, sprintf("%.3f", r),
                htmltools::htmlEscape(var1), htmltools::htmlEscape(var2),
                significance, htmltools::htmlEscape(method)
            )

            # Add clinical guidance (wording matches the strength band above)
            if (abs_r >= 0.3 && is_notable) {
                guidance <- .("<br><strong>Clinical Note:</strong> This suggests a meaningful association that may warrant further investigation.")
            } else if (abs_r < 0.3) {
                guidance <- .("<br><strong>Clinical Note:</strong> The estimated correlation is weak by this descriptive convention. Clinical importance depends on context, uncertainty and a prespecified clinical criterion.")
            } else if (abs_r >= 0.5) {
                guidance <- .("<br><strong>Clinical Note:</strong> Although the correlation appears moderate-to-strong, the evidence for it is not conclusive at the chosen threshold.")
            } else {
                guidance <- .("<br><strong>Clinical Note:</strong> Although the correlation appears moderate, the evidence for it is not conclusive at the chosen threshold.")
            }

            return(paste0(interpretation, guidance))
        },

        # Validation with clinical warnings
        .validateClinicalInputs = function() {
            # Check minimum sample size for reliable correlations.
            # Use the effective complete-case N over the selected variables
            # (and grouping variable when set) rather than the raw row count,
            # and also check per-group complete-case sizes so a tiny subgroup
            # (e.g. n = 5 inside a N = 30 cohort) is never silently cleared.
            dep_cols <- vapply(unique(self$options$dep),
                               function(v) private$.resolveName(v),
                               character(1))
            dep_cols <- dep_cols[dep_cols %in% names(self$data)]
            grp_col <- if (!is.null(self$options$grvar)) private$.resolveName(self$options$grvar) else NULL
            has_grp <- !is.null(grp_col) && grp_col %in% names(self$data)

            effective_n <- tryCatch({
                if (length(dep_cols) >= 2) {
                    cols <- if (has_grp) c(dep_cols, grp_col) else dep_cols
                    sub <- self$data[, cols, drop = FALSE]
                    for (cc in dep_cols) {
                        x <- jmvcore::toNumeric(sub[[cc]])
                        x[!is.finite(x)] <- NA_real_
                        sub[[cc]] <- x
                    }
                    sum(stats::complete.cases(sub))
                } else {
                    nrow(self$data)
                }
            }, error = function(e) nrow(self$data))

            if (effective_n < 20) {
                private$.addWarning("STRONG_WARNING", sprintf(.('Small sample size (N = %d complete cases). Correlations with N &lt; 20 may be unreliable. Consider collecting more data or interpreting results cautiously.'), effective_n))
            } else if (has_grp && length(dep_cols) >= 2) {
                # Check per-group effective N when total N >= 20
                small_grps <- tryCatch({
                    sub <- self$data[, c(dep_cols, grp_col), drop = FALSE]
                    for (cc in dep_cols) {
                        x <- jmvcore::toNumeric(sub[[cc]])
                        x[!is.finite(x)] <- NA_real_
                        sub[[cc]] <- x
                    }
                    grp_vals <- sub[[grp_col]]
                    lvls <- unique(grp_vals[!is.na(grp_vals)])
                    if (is.factor(grp_vals)) lvls <- lvls[order(as.integer(lvls))]
                    out_small <- character(0)
                    for (lvl in lvls) {
                        keep <- !is.na(grp_vals) & grp_vals == lvl
                        n_g <- if (identical(self$options$naHandling, "listwise") || isTRUE(self$options$partial)) {
                            sum(stats::complete.cases(sub[keep, dep_cols, drop = FALSE]))
                        } else {
                            # Minimum pairwise N in this group
                            min_n <- Inf
                            sub_g <- sub[keep, dep_cols, drop = FALSE]
                            for (i in seq_len(length(dep_cols) - 1L)) {
                                for (j in seq.int(i + 1L, length(dep_cols))) {
                                    nij <- sum(stats::complete.cases(sub_g[[i]], sub_g[[j]]))
                                    if (nij < min_n) min_n <- nij
                                }
                            }
                            if (is.finite(min_n)) min_n else 0L
                        }
                        if (n_g > 0L && n_g < 20L) {
                            out_small <- c(out_small, sprintf("%s (n = %d)", htmltools::htmlEscape(as.character(lvl)), n_g))
                        }
                    }
                    out_small
                }, error = function(e) character(0))

                if (length(small_grps) > 0L) {
                    private$.addWarning("STRONG_WARNING", sprintf(
                        .('Small sample size in subgroup(s): %s. Correlations with n &lt; 20 may be unreliable. Consider collecting more data or interpreting subgroup results cautiously.'),
                        paste(small_grps, collapse = ', ')))
                }
            }

            # Check for too many variables (interpretation complexity)
            if (length(self$options$dep) > 10) {
                private$.addWarning("WARNING", sprintf(.('Correlation matrix with %d variables may be complex to interpret. Consider focusing on key variables of interest.'), length(self$options$dep)))
            }

            # Check partial correlations requirements
            if (self$options$partial && length(self$options$dep) < 3) {
                private$.addWarning("WARNING", sprintf(.('Partial correlations require at least 3 variables to control for confounding. Found %d variable(s). Computing zero-order correlations instead.'), length(self$options$dep)))
            }

            return(TRUE)
        },

        # Human-readable method name for each `typestatistics` option.
        # "robust" is 20% Winsorized Pearson (Wilcox, 2012; matching WRS2::wincor).
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
                    if (is.null(sub_res) || nrow(sub_res) == 0 || all(is.na(sub_res$r))) {
                        parts <- c(parts, sprintf(
                            .("<li><strong>%s</strong>: no computable correlations (constant variable or insufficient complete observations).</li>"),
                            htmltools::htmlEscape(as.character(lvl))))
                        next
                    }
                    valid_idx <- !is.na(sub_res$r)
                    n_valid <- sum(valid_idx)
                    sig <- if (identical(options_data$typestatistics, "bayes"))
                               (!is.na(sub_res$bf) & sub_res$bf >= 3)
                           else (!is.na(sub_res$p_adj) & sub_res$p_adj < options_data$siglevel)
                    strong <- valid_idx & abs(sub_res$r) >= 0.5
                    n_lo <- min(sub_res$n[valid_idx], na.rm = TRUE)
                    n_hi <- max(sub_res$n[valid_idx], na.rm = TRUE)
                    n_str <- if (n_lo == n_hi) base::format(n_hi) else sprintf("%d to %d", n_lo, n_hi)
                    best_i <- which.max(abs(sub_res$r))
                    parts <- c(parts, sprintf(
                        .("<li><strong>%s</strong> (n = %s): %d of %d pairs strong (|%s| \u2265 0.5), %d meeting the significance threshold. Strongest: %s = %s (%s vs %s).</li>"),
                        htmltools::htmlEscape(as.character(lvl)),
                        n_str,
                        sum(strong), n_valid,
                        private$.coefSymbol(options_data$typestatistics),
                        sum(sig),
                        private$.coefSymbol(options_data$typestatistics),
                        sprintf("%.3f", sub_res$r[best_i]),
                        htmltools::htmlEscape(sub_res$var1[best_i]),
                        htmltools::htmlEscape(sub_res$var2[best_i])))
                }

                self$results$interpretation$setContent(paste0(
                    "<h4>", .("Correlation Analysis Summary"), "</h4>",
                    "<p>", sprintf(.("Correlations are computed separately within each level of <strong>%s</strong>, matching the figure and the table. Pooling the groups would be misleading whenever they differ in direction."),
                                   htmltools::htmlEscape(self$options$grvar)), "</p>",
                    "<ul>", paste(parts, collapse = ""), "</ul>",
                    "<p><strong>", .("Interpretation Notes:"), "</strong><br>",
                    "\u2022 ", .("Compare the groups deliberately: a correlation present in one group and absent (or reversed) in another is a finding in itself, not noise."),
                    "<br>\u2022 ", .("Strength bands (0.1/0.3/0.5/0.7) and Bayes factor thresholds (BF<sub>10</sub> \u2265 3; Jeffreys, 1961) are descriptive conventions, not clinical decision cut-offs (Schober et al., 2018; Cohen, 1988)."),
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
            partial_on <- any(grepl("^Partial ", res$method))

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
                "\u2022 ", if (partial_on) .("Correlation type: Partial") else .("Correlation type: Zero-order"), "<br>",
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
                "<br>\u2022 ", .("Strength bands (0.1/0.3/0.5/0.7) and Bayes factor thresholds (BF<sub>10</sub> \u2265 3; Jeffreys, 1961) are descriptive conventions, not clinical decision cut-offs (Schober et al., 2018; Cohen, 1988)."),
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

    # Keep pooled chart hidden whenever a grouping variable is active
    self$results$plot$setVisible(is.null(self$options$grvar))

    # Initialize warnings and per-run tracking state
    private$.warnings <- list()
    private$.winsor_collapsed <- character(0)
    private$.effective_partial <- FALSE

    # Initial Message ----
    if ( is.null(self$options$dep) || length(self$options$dep) < 2 ) {

        # Clear any stale outputs from a previous run on a reused R6 object
        private$.clearOutputs()
        self$results$todo$setContent(private$.welcomeMessage())
        private$.displayWarnings()

        return()

    } else {

        # Clear welcome message and show processing message
        todo <- .("<br>You have selected to use a correlation matrix to compare continuous variables.<br><hr>")

        self$results$todo$setContent(todo)

        if (nrow(self$data) == 0) {
            private$.clearOutputs()
            private$.addWarning("ERROR", .('Data contains no complete rows after filtering. Please check for missing values.'))
            private$.displayWarnings()
            return()
        }

        # Validate inputs before processing
        if (!private$.validateInputs()) {
            private$.clearOutputs()
            private$.displayWarnings()
            return()
        }
        private$.validateClinicalInputs()

        # Pre-process data and options for performance
        mydata <- private$.prepareData()
        options_data <- private$.prepareOptions()

        if (length(private$.invalid_colors) > 0L) {
            private$.addWarning("WARNING", sprintf(
                .('Invalid color value(s) (%s) were replaced with default palette colors.'),
                htmltools::htmlEscape(paste(private$.invalid_colors, collapse = ", "))))
        }

        # Populate the correlation table exactly once here (grouped when a
        # grouping variable is set, ungrouped otherwise).
        private$.populateTable(
            mydata, options_data,
            group = if (!is.null(self$options$grvar)) self$options$grvar else NULL
        )

        if (length(private$.winsor_collapsed) > 0L) {
            private$.addWarning("WARNING", sprintf(
                .('20%% Winsorization collapsed %s to zero variance (common when at least 80%% of observations share one value, e.g., zero-inflated scores). Correlations involving this variable are undefined under Robust; consider Spearman (Nonparametric) instead.'),
                htmltools::htmlEscape(paste(unique(private$.winsor_collapsed), collapse = ", "))))
        }

        if (isTRUE(options_data$partial) && private$.n_valid_pairs > 0L && !isTRUE(private$.effective_partial)) {
            private$.addWarning("WARNING", .('Fewer than 3 variables retained non-zero variation after preprocessing, so no control variable remained (q = 0). Zero-order correlations were computed instead of partial correlations.'))
        }

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
                   isTRUE(options_data$partial) &&
                   length(private$.pair_n) > 0 &&
                   min(private$.pair_n) < private$.n_before) {
            # Partial correlations control for all selected variables at once
            # and therefore require complete cases across all selected variables.
            n_excl <- private$.n_before - max(private$.pair_n)
            if (is.null(self$options$grvar) && n_excl > 0) {
                private$.addWarning("WARNING", sprintf(
                    .('Partial correlations control for all selected variables simultaneously and require complete cases across all selected variables: %d of %d rows were excluded despite pairwise selection (%d complete rows analysed).'),
                    n_excl, private$.n_before, max(private$.pair_n)))
            } else {
                private$.addWarning("WARNING", .('Partial correlations control for all selected variables simultaneously and use complete cases across all selected variables within each group.'))
            }
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

        # Multiplicity burden. A correlation matrix runs one test per PAIR, so
        # the test count grows quadratically with the number of variables while
        # the panel still shows a single "p" column per row. With 15 variables
        # and no correction that is 105 tests, and on pure noise about six of
        # them clear p < 0.05 - which reads as six findings. The table footnote
        # said so quietly; anyone screening biomarkers needs it as a notice.
        # Bayesian analysis has no p-value, so there is nothing to correct.
        if (!identical(options_data$typestatistics, "bayes") &&
            identical(options_data$padjustmethod, "none") &&
            private$.n_valid_pairs > 1) {
            expected_fp <- private$.n_valid_pairs * options_data$siglevel
            private$.addWarning("STRONG_WARNING", sprintf(
                .('No correction for multiple comparisons is applied across %d correlation tests at a threshold of %s, so about %s of them are expected to reach significance by chance alone. Select a p-value adjustment (Holm is the default) or report these as exploratory rather than confirmatory.'),
                private$.n_valid_pairs,
                base::format(options_data$siglevel),
                base::format(round(expected_fp, 1))))
        }

        # Completion notice. This lives here rather than in .plot() because
        # .run() is the only place that renders private$.warnings.
        # Count the coefficients actually produced, not the table rows: a
        # constant variable yields a row of NAs that was being counted as a
        # computed correlation.
        if (private$.n_valid_pairs > 0) {
            private$.addWarning("INFO", if (isTRUE(private$.effective_partial))
                sprintf(.('Computed %d partial %s of %d variables.'),
                        private$.n_valid_pairs,
                        private$.methodLabel(self$options$typestatistics),
                        length(options_data$myvars))
                else
                sprintf(.('Computed %d zero-order %s of %d variables.'),
                        private$.n_valid_pairs,
                        private$.methodLabel(self$options$typestatistics),
                        length(options_data$myvars)))
        }

        # Display all collected warnings at the end
        private$.displayWarnings()

    }
},

.generateAboutContent = function() {
    about_content <- paste0(
        "<h3>", .("About Correlation Matrix"), "</h3>",
        "<hr>",
        "<p><b>", .("Purpose:"), "</b> ",
        .("This analysis creates a correlation matrix to visualize the relationships between multiple continuous variables. It helps in understanding the direction, magnitude, and significance of the associations between pairs of variables."), "</p>",
        "<p><b>", .("When to Use:"), "</b></p>",
        "<ul>",
        "<li><b>", .("Exploratory Data Analysis:"), "</b> ", .("To get a quick overview of the relationships between a set of variables."), "</li>",
        "<li><b>", .("Feature Selection:"), "</b> ", .("To identify highly correlated variables that may be redundant in a predictive model."), "</li>",
        "<li><b>", .("Publication:"), "</b> ", .("To create a publication-ready summary of the associations between your variables of interest."), "</li>",
        "</ul>",
        "<p><b>", .("Key Features:"), "</b></p>",
        "<ul>",
        "<li>", .("Supports Pearson, Spearman, 20% Winsorized robust, and Bayesian correlation methods."), "</li>",
        "<li>", .("Can be split by a grouping variable to compare correlations across subgroups."), "</li>",
        "<li>", .("Can compute partial correlations to control for confounding variables."), "</li>",
        "<li>", .("Provides options for p-value adjustment, theming, and customizing the plot."), "</li>",
        "</ul>",
        "<hr>"
    )
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
        n_label <- sprintf(.("%s, analyzed separately within each level of %s"),
                           n_label, htmltools::htmlEscape(self$options$grvar))
    method_name <- private$.methodName(options_data$typestatistics)
    corr_type <- if (isTRUE(private$.effective_partial)) .("Partial") else .("Zero-order")

    summary_text <- paste0(
        "<h4>", .("Analysis Summary"), "</h4>",
        "<p><b>", .("Variables analyzed:"), "</b> ", n_vars, "</p>",
        "<p><b>", .("Sample size:"), "</b> ", n_label, "</p>",
        "<p><b>", .("Method:"), "</b> ", sprintf(.("%s correlation"), method_name), "</p>",
        "<p><b>", .("Correlation type:"), "</b> ", corr_type, "</p>"
    )

    self$results$summary$setContent(summary_text)
},

# Subset a prepared frame to the selected variables and restore the
# user-facing names, resolving possibly B64-encoded column names via
# .resolveName so downstream helpers see identical variable labels.
.plotFrame = function(df, myvars, extra = NULL) {
    resolved <- vapply(myvars, function(v) private$.resolveName(v, df), character(1))
    if (!is.null(extra))
        resolved <- c(resolved, private$.resolveName(extra, df))
    out <- df[, resolved, drop = FALSE]
    names(out) <- c(myvars, if (!is.null(extra)) extra)
    out
},

# 20% tail-clamping Winsorization matching WRS2::winval(..., tr = 0.2)
.winsorizeVec = function(v, tr = 0.2) {
    n <- length(v)
    g <- floor(tr * n)
    if (g <= 0L) return(v)
    sv <- sort(v)
    lo <- sv[g + 1L]
    hi <- sv[n - g]
    v[v < lo] <- lo
    v[v > hi] <- hi
    v
},

# Single unified correlation engine shared by the table, the clinical
# interpretation, and both plot methods (.plot and .plot2).
#
# Uses safe internal syntactic names (v1..vp) so spaces, symbols and non-ASCII
# column names never break formula-based helpers; enforces complete cases and
# exact df = n - 2 - q (and CI SE = 1 / sqrt(n - 3 - q)) for partial
# correlations; rank-transforms BEFORE partialing for partial Spearman and
# winsorizes BEFORE partialing for partial robust; applies Wilcox's df = h - 2
# test and t_{h-2}-scaled Fisher-z CI for Winsorized Pearson; and passes
# conflevel directly to parameters::model_parameters() for Bayesian HDIs.
.computeCorrelations = function(df, options_data) {
    myvars <- options_data$myvars
    p_vars <- length(myvars)
    if (p_vars < 2L) return(NULL)
    resolved <- vapply(myvars, function(v) private$.resolveName(v, df), character(1))
    if (!all(resolved %in% names(df))) return(NULL)

    cor_data <- df[, resolved, drop = FALSE]
    for (cc in names(cor_data)) {
        x <- jmvcore::toNumeric(cor_data[[cc]])
        x[!is.finite(x)] <- NA_real_
        cor_data[[cc]] <- x
    }
    # Safe syntactic column names internally; original myvars restored on output
    safe_names <- paste0("v", seq_len(p_vars))
    names(cor_data) <- safe_names

    type       <- options_data$typestatistics
    ci_lvl     <- options_data$conflevel
    partial_on <- isTRUE(options_data$partial) && p_vars >= 3L
    padj_meth  <- options_data$padjustmethod
    b_seed     <- if (identical(type, "bayes")) private$.bayesSeed() else NULL

    # Memoize across table, interpretation, and plot consumers so identical
    # data/options (especially expensive Bayesian MCMC draws) run once per cycle.
    cache_key <- digest::digest(list(
        myvars     = myvars,
        cor_data   = cor_data,
        type       = type,
        ci_lvl     = ci_lvl,
        partial_on = partial_on,
        padj_meth  = padj_meth,
        b_seed     = b_seed
    ), algo = "md5")
    if (cache_key %in% names(private$.corr_cache)) {
        cached <- private$.corr_cache[[cache_key]]
        if (length(cached$winsor_collapsed) > 0L)
            private$.winsor_collapsed <- unique(c(private$.winsor_collapsed, cached$winsor_collapsed))
        if (isTRUE(cached$effective_partial))
            private$.effective_partial <- TRUE
        return(cached$res)
    }

    z_crit     <- stats::qnorm(1 - (1 - ci_lvl) / 2)

    base_method <- switch(type,
        "parametric"    = "Pearson correlation",
        "nonparametric" = "Spearman correlation",
        "robust"        = "Winsorized Pearson correlation",
        "bayes"         = "Bayesian Pearson correlation",
        "Pearson correlation"
    )
    effective_partial <- FALSE
    local_collapsed   <- character(0)

    # Pair index table in canonical upper-triangle order
    pair_i <- integer(0)
    pair_j <- integer(0)
    for (i in seq_len(p_vars - 1L)) {
        for (j in seq.int(i + 1L, p_vars)) {
            pair_i <- c(pair_i, i)
            pair_j <- c(pair_j, j)
        }
    }
    n_pairs <- length(pair_i)
    n_vec   <- rep(NA_integer_, n_pairs)
    r_vec   <- rep(NA_real_, n_pairs)
    lo_vec  <- rep(NA_real_, n_pairs)
    hi_vec  <- rep(NA_real_, n_pairs)
    p_vec   <- rep(NA_real_, n_pairs)
    bf_vec  <- rep(NA_real_, n_pairs)

    eval_bayes_r_n <- function(r_obs, n_eff) {
        if (is.na(r_obs) || is.na(n_eff) || n_eff < 3L) {
            return(list(r = NA_real_, lo = NA_real_, hi = NA_real_, bf = NA_real_))
        }
        if (abs(r_obs) >= 1 - 1e-12) {
            return(list(r = sign(r_obs), lo = sign(r_obs), hi = sign(r_obs), bf = Inf))
        }
        # Exact sufficient-statistic representation of (r_obs, n_eff) for
        # BayesFactor::correlationBF (depends on x, y solely via r and n).
        # For partial correlations controlling for q covariates, n_eff = n - q
        # yields the exact Fisher (1924) / Wetzels & Wagenmakers (2012) / Ly et al.
        # (2018) marginal partial correlation likelihood.
        u <- seq_len(n_eff) - (n_eff + 1) / 2
        u <- u / sqrt(sum(u^2))
        w <- u^2 - mean(u^2)
        w <- w - sum(w * u) * u
        w <- w / sqrt(sum(w^2))
        v <- r_obs * u + sqrt(max(0, 1 - r_obs^2)) * w
        tryCatch({
            bf_obj <- suppressWarnings(BayesFactor::correlationBF(u, v, rscale = 0.707))
            mp <- suppressWarnings(parameters::model_parameters(
                bf_obj,
                ci = ci_lvl,
                ci_method = "hdi",
                test = "bf",
                dispersion = FALSE
            ))
            list(
                r  = as.numeric(mp$Median[1]),
                lo = as.numeric(mp$CI_low[1]),
                hi = as.numeric(mp$CI_high[1]),
                bf = as.numeric(mp$BF[1])
            )
        }, error = function(e) list(r = NA_real_, lo = NA_real_, hi = NA_real_, bf = NA_real_))
    }

    eval_bayes_pair <- function(xi, yi) {
        if (length(xi) < 3L || stats::sd(xi) == 0 || stats::sd(yi) == 0) {
            return(list(r = NA_real_, lo = NA_real_, hi = NA_real_, bf = NA_real_))
        }
        r_raw <- suppressWarnings(stats::cor(xi, yi))
        eval_bayes_r_n(r_raw, length(xi))
    }

    if (identical(type, "bayes")) {
        withr::local_seed(b_seed)
    }

    if (!partial_on) {
        for (k in seq_len(n_pairs)) {
            xi_all <- cor_data[[pair_i[k]]]
            yi_all <- cor_data[[pair_j[k]]]
            ok <- is.finite(xi_all) & is.finite(yi_all)
            n_ij <- sum(ok)
            n_vec[k] <- as.integer(n_ij)
            if (n_ij < 3L) next
            xi <- xi_all[ok]
            yi <- yi_all[ok]
            if (stats::sd(xi) == 0 || stats::sd(yi) == 0) next

            if (type == "parametric") {
                ct <- suppressWarnings(stats::cor.test(xi, yi, method = "pearson", conf.level = ci_lvl))
                rk <- unname(as.numeric(ct$estimate))
                r_vec[k] <- rk
                p_vec[k] <- as.numeric(ct$p.value)
                if (n_ij > 3L) {
                    if (abs(rk) >= 1 - 1e-15) {
                        lo_vec[k] <- sign(rk)
                        hi_vec[k] <- sign(rk)
                    } else {
                        lo_vec[k] <- as.numeric(ct$conf.int[1])
                        hi_vec[k] <- as.numeric(ct$conf.int[2])
                    }
                }
            } else if (type == "nonparametric") {
                ct <- suppressWarnings(stats::cor.test(xi, yi, method = "spearman", exact = FALSE))
                rk <- unname(as.numeric(ct$estimate))
                r_vec[k] <- rk
                p_vec[k] <- as.numeric(ct$p.value)
                if (n_ij > 3L) {
                    if (abs(rk) >= 1 - 1e-15) {
                        lo_vec[k] <- sign(rk)
                        hi_vec[k] <- sign(rk)
                    } else {
                        # Fieller, Hartley & Pearson (1957) SE_z = sqrt(1.06 / (n - 3))
                        moe <- z_crit * sqrt(1.06 / (n_ij - 3))
                        lo_vec[k] <- tanh(atanh(rk) - moe)
                        hi_vec[k] <- tanh(atanh(rk) + moe)
                    }
                }
            } else if (type == "robust") {
                g <- floor(0.2 * n_ij)
                h <- n_ij - 2L * g
                df_w <- h - 2L
                xw <- private$.winsorizeVec(xi, tr = 0.2)
                yw <- private$.winsorizeVec(yi, tr = 0.2)
                if (stats::sd(xw) == 0 || stats::sd(yw) == 0) {
                    if (stats::sd(xw) == 0)
                        local_collapsed <- unique(c(local_collapsed, myvars[pair_i[k]]))
                    if (stats::sd(yw) == 0)
                        local_collapsed <- unique(c(local_collapsed, myvars[pair_j[k]]))
                    next
                }
                rk <- stats::cor(xw, yw)
                r_vec[k] <- rk
                if (abs(rk) >= 1 - 1e-15) {
                    p_vec[k] <- 0
                    lo_vec[k] <- sign(rk)
                    hi_vec[k] <- sign(rk)
                } else if (df_w > 0L) {
                    # Wilcox (2012, Sec 9.3.2, eq 9.6): T_w with df = h - 2
                    t_w <- rk * sqrt((n_ij - 2) / (1 - rk^2))
                    p_vec[k] <- 2 * stats::pt(-abs(t_w), df = df_w)
                    if (n_ij > 3L) {
                        moe <- stats::qt(1 - (1 - ci_lvl) / 2, df = df_w) / sqrt(n_ij - 3)
                        lo_vec[k] <- tanh(atanh(rk) - moe)
                        hi_vec[k] <- tanh(atanh(rk) + moe)
                    }
                }
            } else if (type == "bayes") {
                private$.checkpoint()
                b_res <- eval_bayes_pair(xi, yi)
                r_vec[k]  <- b_res$r
                lo_vec[k] <- b_res$lo
                hi_vec[k] <- b_res$hi
                bf_vec[k] <- b_res$bf
            }
        }
    } else {
        # Partial correlations control for all other variables simultaneously
        # and require complete cases across all selected variables.
        cc <- cor_data[stats::complete.cases(cor_data), , drop = FALSE]
        n_cc <- nrow(cc)
        if (n_cc < 3L) return(NULL)

        active <- vapply(cc, function(v) stats::sd(v) > 0, logical(1))
        act_idx <- which(active)
        p_act <- length(act_idx)
        if (p_act < 2L) return(NULL)

        X <- cc[, act_idx, drop = FALSE]

        # Transform BEFORE partialing so partial Spearman is rank-based
        # (monotone-invariant) and partial robust resists control-var outliers.
        if (type == "nonparametric") {
            X_work <- as.data.frame(lapply(X, rank, ties.method = "average"))
        } else if (type == "robust") {
            X_work <- as.data.frame(lapply(X, private$.winsorizeVec, tr = 0.2))
            still_var <- vapply(X_work, function(v) stats::sd(v) > 0, logical(1))
            if (!all(still_var)) {
                collapsed_names <- myvars[act_idx[!still_var]]
                local_collapsed <- unique(c(local_collapsed, collapsed_names))
                act_idx <- act_idx[still_var]
                X_work <- X_work[, still_var, drop = FALSE]
                p_act <- length(act_idx)
                if (p_act < 2L) {
                    if (length(local_collapsed) > 0L)
                        private$.winsor_collapsed <- unique(c(private$.winsor_collapsed, local_collapsed))
                    return(NULL)
                }
            }
        } else {
            X_work <- X
        }

        q <- max(p_act - 2L, 0L)
        effective_partial <- q >= 1L
        if (effective_partial) private$.effective_partial <- TRUE
        df_p <- n_cc - 2L - q
        # Refuse saturated / over-saturated partial systems (df <= 0)
        if (df_p <= 0L) return(NULL)

        R_mat <- tryCatch(stats::cor(X_work), error = function(e) NULL)
        if (is.null(R_mat) || any(!is.finite(R_mat))) return(NULL)
        P_mat <- tryCatch(solve(R_mat), error = function(e) NULL)
        if (is.null(P_mat) || any(!is.finite(P_mat)) || any(diag(P_mat) <= 0)) return(NULL)

        pcor_mat <- -P_mat / sqrt(outer(diag(P_mat), diag(P_mat)))
        pcor_mat <- pmin(pmax(pcor_mat, -1), 1)

        if (type == "bayes") {
            # Exact Fisher (1924) / Ly et al. (2018) marginal partial correlation
            # likelihood uses effective sample size n_eff = n - q (df_p = n_eff - 2 >= 1).
            n_eff <- n_cc - q
            for (k in seq_len(n_pairs)) {
                private$.checkpoint()
                n_vec[k] <- as.integer(n_cc)
                ai <- match(pair_i[k], act_idx)
                aj <- match(pair_j[k], act_idx)
                if (is.na(ai) || is.na(aj)) next
                rk <- as.numeric(pcor_mat[ai, aj])
                b_res <- eval_bayes_r_n(rk, n_eff)
                r_vec[k]  <- b_res$r
                lo_vec[k] <- b_res$lo
                hi_vec[k] <- b_res$hi
                bf_vec[k] <- b_res$bf
            }
        } else {
            g <- if (type == "robust") floor(0.2 * n_cc) else 0L
            h <- n_cc - 2L * g
            df_test <- if (type == "robust") (h - 2L - q) else df_p
            if (df_test <= 0L) return(NULL)

            for (k in seq_len(n_pairs)) {
                n_vec[k] <- as.integer(n_cc)
                ai <- match(pair_i[k], act_idx)
                aj <- match(pair_j[k], act_idx)
                if (is.na(ai) || is.na(aj)) next
                rk <- as.numeric(pcor_mat[ai, aj])
                r_vec[k] <- rk
                if (abs(rk) >= 1 - 1e-15) {
                    p_vec[k] <- 0
                    lo_vec[k] <- sign(rk)
                    hi_vec[k] <- sign(rk)
                } else {
                    # Exact Fisher (1924) df = n - 2 - q (and Wilcox df = h - 2 - q for robust)
                    t_val <- rk * sqrt(df_p / (1 - rk^2))
                    p_vec[k] <- 2 * stats::pt(-abs(t_val), df = df_test)
                    denom_ci <- n_cc - 3L - q
                    if (denom_ci > 0L) {
                        moe <- if (type == "nonparametric") {
                            z_crit * sqrt(1.06 / denom_ci)
                        } else if (type == "robust") {
                            stats::qt(1 - (1 - ci_lvl) / 2, df = df_test) / sqrt(denom_ci)
                        } else {
                            z_crit / sqrt(denom_ci)
                        }
                        lo_vec[k] <- tanh(atanh(rk) - moe)
                        hi_vec[k] <- tanh(atanh(rk) + moe)
                    }
                }
            }
        }
    }

    if (length(local_collapsed) > 0L) {
        private$.winsor_collapsed <- unique(c(private$.winsor_collapsed, local_collapsed))
    }

    method_label <- if (effective_partial) paste0("Partial ", base_method) else base_method

    p_adj <- if (all(is.na(p_vec))) as.numeric(p_vec)
             else stats::p.adjust(p_vec, method = padj_meth)

    res_df <- data.frame(
        var1      = myvars[pair_i],
        var2      = myvars[pair_j],
        n         = n_vec,
        r         = r_vec,
        conf_low  = lo_vec,
        conf_high = hi_vec,
        p         = p_vec,
        p_adj     = p_adj,
        bf        = bf_vec,
        method    = rep(method_label, n_pairs),
        stringsAsFactors = FALSE
    )

    if (length(private$.corr_cache) >= 32L) {
        private$.corr_cache <- list()
    }
    private$.corr_cache[[cache_key]] <- list(
        res               = res_df,
        winsor_collapsed  = local_collapsed,
        effective_partial = effective_partial
    )
    res_df
},

.populateTable = function(mydata, options_data, group = NULL) {
    table <- self$results$table
    # Clear existing rows - jamovi tables use deleteRows(), not clear()
    .deleteTableRows(table)
    # Bayesian estimates are posterior draws: name the seed they came from
    table$setNote("seed", if (identical(options_data$typestatistics, "bayes"))
        .fmt(.("Random seed: {seed}"), seed = private$.bayesSeed()))
    private$.pair_n <- integer(0)
    private$.n_valid_pairs <- 0L

    add_rows_for_subset <- function(df, grp_label = "All") {
        res <- private$.computeCorrelations(df, options_data)
        if (is.null(res) || nrow(res) == 0 || all(is.na(res$r))) {
            if (is.null(group))
                private$.addWarning("ERROR", .('No correlations could be computed with the selected options. If partial correlations are switched on, check that the variables are not collinear and that complete observations exceed the number of variables (n &gt; p, and effective h &gt; p for robust).'))
            else
                private$.addWarning("WARNING", sprintf(
                    .('No correlations could be computed for group "%s"; it is omitted from the table.'),
                    htmltools::htmlEscape(as.character(grp_label))))
            return(invisible(NULL))
        }
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
                    cred_low  = res$conf_low[i],
                    cred_high = res$conf_high[i],
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
        n_missing_group <- sum(is.na(grp_vals))
        if (n_missing_group > 0)
            private$.addWarning("WARNING", sprintf(
                .('%d row(s) have no value for the grouping variable and are excluded from the grouped correlation table.'),
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

    # Method-specific uncertainty & multiplicity disclosure note
    method_note <- if (options_data$typestatistics == "bayes") {
        if (isTRUE(private$.effective_partial)) {
            .("Bayesian partial correlations use the exact Fisher/Ly effective sample size (n_eff = n - q, where q is the number of controlled variables) under a fixed shifted Beta prior (rscale = 0.707, wide). Reports median posterior estimate, HDI credible interval at the selected level, and BF<sub>10</sub>; no p-value is defined.")
        } else {
            .("Bayesian correlations use a fixed shifted Beta prior (rscale = 0.707, wide) and report the median posterior estimate, an HDI credible interval at the selected level (not a confidence interval), and BF<sub>10</sub>; no p-value is defined.")
        }
    } else {
        padj_txt <- if (options_data$padjustmethod == "none") {
            .("No correction for multiple comparisons was applied, so <b>p (adjusted)</b> repeats the unadjusted p-value. Each additional variable adds several pairwise tests.")
        } else {
            sprintf(
                .("<b>p (adjusted)</b> applies the %s correction across pairwise tests within each split panel (not across panels). This is the p-value the plot uses to mark cells as non-significant at %s."),
                private$.padjustLabel(options_data$padjustmethod),
                base::format(options_data$siglevel))
        }
        extra_txt <- switch(options_data$typestatistics,
            "nonparametric" = .("Spearman confidence intervals use the Fieller, Hartley & Pearson (1957) approximation and p-values use the asymptotic t-approximation (exact = FALSE)."),
            "robust"        = .("Winsorized Pearson (20% per tail) uses Wilcox's (2012) effective degrees of freedom (df = h - 2, reduced by control variables when partial) and t-scaled Fisher-z confidence intervals."),
            "")
        if (nzchar(extra_txt)) paste(padj_txt, extra_txt) else padj_txt
    }
    table$setNote("padj", method_note)
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
    method_name <- private$.methodName(options_data$typestatistics)
    method_bullet <- switch(options_data$typestatistics,
        "parametric" = paste0(
            "<li><b>", .("Pearson:"), "</b> ",
            .("Assumes that the variables are approximately normally distributed and that their relationship is linear."),
            "</li>"
        ),
        "nonparametric" = paste0(
            "<li><b>", .("Spearman:"), "</b> ",
            .("Does not assume normality; it is based on ranks and detects monotonic relationships. Confidence intervals use the Fieller, Hartley &amp; Pearson (1957) approximation (best for |rho| \u2264 0.8), and p-values use the asymptotic t-approximation."),
            "</li>"
        ),
        "robust" = paste0(
            "<li><b>", .("Winsorized Pearson:"), "</b> ",
            .("Pearson's r computed after winsorizing the most extreme 20% of observations in each tail (outliers are pulled in rather than removed), tested with Wilcox's (2012) effective degrees of freedom (df = h - 2). Less sensitive to outliers than Pearson, but it still measures a linear association."),
            "</li>"
        ),
        "bayes" = paste0(
            "<li><b>", .("Bayesian Pearson:"), "</b> ",
            .("Evaluates evidence via BF<sub>10</sub> using a fixed shifted Beta prior with scale <code>rscale = 0.707</code> (wide, Beta(1.414, 1.414); not user-configurable) and reports the posterior median and HDI credible interval at the chosen level."),
            if (isTRUE(private$.effective_partial))
                paste0(" ", .("Partial Bayesian correlations use the exact Fisher/Ly effective sample size (n_eff = n - q) controlling for q covariates under multivariate normality."))
            else "",
            "</li>"
        ),
        "")

    assumptions_content <- paste0(
        "<h3>", .("Statistical Assumptions &amp; Warnings"), "</h3>",
        "<hr>",
        "<p><b>", sprintf(.("For %s correlation:"), method_name), "</b></p>",
        "<ul>", method_bullet, "</ul>",
        "<p><b>", .("General Warnings:"), "</b></p>",
        "<ul>",
        "<li><b>", .("Independence:"), "</b> ",
        .("Standard tests and intervals assume independent observations (one row per patient). Repeated measures, multiple biopsies, TMA cores, or bilateral specimens per patient violate independence and can inflate false positives."),
        "</li>",
        "<li>", .("Correlation does not imply causation."), "</li>",
        "<li>", .("Outliers can have a large influence on the correlation coefficient, especially for Pearson correlation."), "</li>",
        "<li>", .("Restricting the range of the variables can artificially lower the correlation coefficient."), "</li>",
        "</ul>",
        "<hr>"
    )

    self$results$assumptions$setContent(assumptions_content)
},

# Build a single correlation matrix ggplot directly from .computeCorrelations()
# so the plotted numbers, cross-out markers, Bayesian MCMC draws, partial
# correlations, and Winsorized p-values are guaranteed identical to the table.
.buildCorrPlot = function(df, options_data, ggtheme,
                          title = NULL, subtitle = NULL, caption = NULL) {
    res <- private$.computeCorrelations(df, options_data)
    if (is.null(res) || nrow(res) == 0L || all(is.na(res$r))) return(NULL)

    myvars <- options_data$myvars
    # Omit constant/all-NA variables from the plotted matrix (consistent with
    # .validateInputs warning), keeping original variable order.
    valid_rows <- res[!is.na(res$r), , drop = FALSE]
    keep_vars <- myvars[myvars %in% unique(c(valid_rows$var1, valid_rows$var2))]
    if (length(keep_vars) < 2L) return(NULL)

    pv <- length(keep_vars)
    corr_mat <- diag(1, nrow = pv, ncol = pv)
    dimnames(corr_mat) <- list(keep_vars, keep_vars)
    p_mat <- matrix(0, nrow = pv, ncol = pv, dimnames = list(keep_vars, keep_vars))

    for (k in seq_len(nrow(valid_rows))) {
        v1 <- valid_rows$var1[k]
        v2 <- valid_rows$var2[k]
        if (v1 %in% keep_vars && v2 %in% keep_vars) {
            corr_mat[v1, v2] <- valid_rows$r[k]
            corr_mat[v2, v1] <- valid_rows$r[k]
            pk <- if (is.na(valid_rows$p_adj[k])) 0 else valid_rows$p_adj[k]
            p_mat[v1, v2] <- pk
            p_mat[v2, v1] <- pk
        }
    }

    is_bayes <- identical(options_data$typestatistics, "bayes")
    r_method_text <- private$.methodName(options_data$typestatistics)
    n_heading <- .("sample sizes:")
    r_type <- if (any(grepl("^Partial ", valid_rows$method))) .("correlation (partial):") else .("correlation:")
    ns_valid <- valid_rows$n[!is.na(valid_rows$n)]
    n_lo <- min(ns_valid)
    n_hi <- max(ns_valid)
    if (n_lo == n_hi) {
        legend_title <- bquote(atop(
            atop(scriptstyle(bold(.(n_heading))), italic(n) ~ "=" ~ .(format(n_hi, big.mark = ","))),
            atop(scriptstyle(bold(.(r_type))), .(r_method_text))
        ))
    } else {
        legend_title <- bquote(atop(
            atop(scriptstyle(bold(.(n_heading))),
                 italic(n) ~ "=" ~ .(paste0(format(n_lo, big.mark = ","), " - ", format(n_hi, big.mark = ",")))),
            atop(scriptstyle(bold(.(r_type))), .(r_method_text))
        ))
    }

    plot_theme <- if (isTRUE(self$options$originaltheme)) {
        ggstatsplot::theme_ggstatsplot()
    } else if (!is.null(ggtheme)) {
        ggtheme
    } else {
        ggplot2::theme_bw()
    }

    p <- ggcorrplot::ggcorrplot(
        corr = corr_mat,
        method = options_data$ggcorrplot.args$method,
        type = options_data$matrixtype,
        ggtheme = plot_theme,
        colors = options_data$colors,
        outline.color = options_data$ggcorrplot.args$outline.color,
        lab = TRUE,
        digits = options_data$k,
        p.mat = if (is_bayes) NULL else p_mat,
        sig.level = if (is_bayes) Inf else options_data$siglevel,
        insig = "pch",
        pch = "cross",
        pch.cex = options_data$ggcorrplot.args$pch.cex,
        legend.title = legend_title
    )

    # Non-significance cross legend caption (matches ggstatsplot::ggcorrmat)
    cap_out <- caption
    if (!is_bayes) {
        p_label <- if (identical(options_data$padjustmethod, "none")) {
            substitute(italic(p)[unadj.] ~ "< " ~ sig.level, list(sig.level = options_data$siglevel))
        } else {
            substitute(italic(p)[adj.text - adj.] ~ "< " ~ sig.level,
                       list(sig.level = options_data$siglevel,
                            adj.text = private$.padjustLabel(options_data$padjustmethod)))
        }
        cap_out <- if (is.null(caption) || !nzchar(caption)) {
            substitute(bold("X") ~ "= non-significant at" ~ p_label, list(p_label = p_label))
        } else {
            substitute(atop(displaystyle(top.text), bold("X") ~ "= non-significant at" ~ p_label),
                       list(top.text = caption, p_label = p_label))
        }
    }

    p +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            legend.title = ggplot2::element_text(size = 15)
        ) +
        ggplot2::labs(title = title, subtitle = subtitle, caption = cap_out, x = NULL, y = NULL)
},

.buildPlot = function(mydata, options_data, ggtheme = NULL) {
    plot <- private$.buildCorrPlot(
        df = mydata,
        options_data = options_data,
        ggtheme = ggtheme,
        title = options_data$title,
        subtitle = options_data$subtitle,
        caption = options_data$caption
    )
    if (is.null(plot)) return(NULL)
    if (identical(options_data$typestatistics, "bayes"))
        plot <- addSeedCaption(plot, self, private$.bayesSeed())
    plot
},

.buildPlot2 = function(mydata, options_data, ggtheme = NULL) {
    if (is.null(self$options$grvar)) return(NULL)
    grp_var <- private$.resolveName(self$options$grvar, mydata)
    grp_vals <- mydata[[grp_var]]
    lvls <- unique(grp_vals[!is.na(grp_vals)])
    if (is.factor(grp_vals)) lvls <- lvls[order(as.integer(lvls))]

    plots <- list()
    for (lvl in lvls) {
        keep <- !is.na(grp_vals) & grp_vals == lvl
        p_sub <- private$.buildCorrPlot(
            df = mydata[keep, , drop = FALSE],
            options_data = options_data,
            ggtheme = ggtheme,
            title = paste0(self$options$grvar, ": ", as.character(lvl)),
            subtitle = NULL,
            caption = NULL
        )
        if (!is.null(p_sub)) plots[[as.character(lvl)]] <- p_sub
    }
    if (length(plots) == 0L) return(NULL)

    plot2 <- patchwork::wrap_plots(plots) +
        patchwork::plot_annotation(
            title = options_data$title,
            subtitle = options_data$subtitle,
            caption = options_data$caption
        )

    if (identical(options_data$typestatistics, "bayes"))
        plot2 <- addSeedCaption(plot2, self, private$.bayesSeed())
    plot2
},

.plot = function(image, ggtheme, theme, ...) {
            # Check for sufficient variables before any processing
            if (is.null(self$options$dep) || length(self$options$dep) < 2)
                return()

            # Pooled plot is hidden when a grouping variable is active
            if (!is.null(self$options$grvar))
                return()

            # Use shared validation ----
            if (!private$.validateInputs())
                return()

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()

            # Checkpoint before expensive correlation computation
            private$.checkpoint()

            if (Sys.getenv("TESTTHAT") == "true")
                return(TRUE)

            plot <- private$.buildPlot(mydata, options_data, ggtheme = ggtheme)
            if (is.null(plot))
                return(FALSE)

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

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()

            # Checkpoint before expensive grouped correlation computation
            private$.checkpoint()

            if (Sys.getenv("TESTTHAT") == "true")
                return(TRUE)

            plot2 <- private$.buildPlot2(mydata, options_data, ggtheme = ggtheme)
            if (is.null(plot2))
                return(FALSE)

            print(plot2)
            TRUE
        },

        # Hardened per-option syntax serializer: drops undeclared option names
        # and list payloads that can arrive from crafted .omv/.omt files, and
        # properly escapes character scalars via encodeString() so quotes,
        # backslashes, or newlines cannot break out of string literals.
        .sourcifyOption = function(option) {
            if (!option$name %in% names(formals(jjcorrmat)))
                return("")
            value <- option$value
            if (is.list(value))
                return("")
            if (is.character(value)) {
                if (length(value) != 1L || is.na(value) || identical(value, option$default))
                    return("")
                return(paste0(option$name, " = ", encodeString(value, quote = '"')))
            }
            super$.sourcifyOption(option)
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

            # Build the argument list in option-declaration order, filtering
            # strictly to declared jjcorrmat formals and encoding all string
            # values via encodeString() so crafted .omv options or variable
            # names with quotes/backslashes/newlines cannot inject arguments.
            allowed_args <- setdiff(names(formals(jjcorrmat)), "data")
            args <- character(0)
            for (option in private$.options$options) {
                if (!option$name %in% allowed_args)
                    next
                if (inherits(option, 'OptionVariable') || inherits(option, 'OptionVariables')) {
                    val <- option$value
                    if (is.character(val) && length(val) > 0L && !any(is.na(val))) {
                        encoded <- encodeString(unname(val), quote = '"')
                        rhs <- if (length(encoded) == 1L) encoded else paste0("c(", paste(encoded, collapse = ", "), ")")
                        args <- c(args, paste0(option$name, ' = ', rhs))
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
            paste0(pkg_name, '::jjcorrmat(\n    data = data,\n    ',
                   paste(args, collapse = ',\n    '), ')')
        }
    ) # End of public list
) else NULL
