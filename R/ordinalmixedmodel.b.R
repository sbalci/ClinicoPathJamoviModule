#' @title Ordinal Mixed Models (Cumulative Link)
#' @importFrom R6 R6Class
#' @import jmvcore

ordinalmixedmodelClass <- R6::R6Class(
    "ordinalmixedmodelClass",
    inherit = ordinalmixedmodelBase,
    private = list(

        .init = function() {

            if (is.null(self$data) || is.null(self$options$dep) ||
                is.null(self$options$randomTerms) ||
                length(self$options$randomTerms) == 0) {
                self$results$instructions$setContent(
                    "<html><body>
                    <div style='margin: 2em; color: #3E4053;'>
                        <h2>Ordinal Mixed Models (Cumulative Link)</h2>
                        <h3>Overview</h3>
                        <p>Fits cumulative link mixed models (CLMM) for ordinal response data
                        with random effects. This is the appropriate model when:</p>
                        <ul>
                            <li>The outcome is ordinal (e.g., Likert scales, tumor grades, severity levels)</li>
                            <li>Observations are clustered (e.g., multiple ratings per pathologist, repeated measures)</li>
                            <li>You need to account for both fixed predictors and random grouping effects</li>
                        </ul>
                        <h3>Required Input</h3>
                        <ul>
                            <li><strong>Dependent Variable:</strong> Ordinal outcome (will be converted to ordered factor)</li>
                            <li><strong>Fixed Factors/Covariates:</strong> Predictor variables (at least one)</li>
                            <li><strong>Random Effects Grouping:</strong> Clustering variable(s) (e.g., subject, rater, site)</li>
                        </ul>
                        <h3>Proportional Odds Assumption</h3>
                        <p>The default logit link assumes proportional odds: the effect of predictors is
                        constant across all category thresholds. The nominal test checks this assumption.</p>
                    </div>
                    </body></html>"
                )
                return()
            }
        },

        .run = function() {

            if (is.null(self$options$dep) || is.null(self$options$randomTerms) ||
                length(self$options$randomTerms) == 0)
                return()

            hasFixed <- (length(self$options$fixedFactors) > 0 || length(self$options$fixedCovs) > 0)
            if (!hasFixed) {
                self$results$instructions$setContent(
                    "<p>Please specify at least one fixed factor or covariate.</p>"
                )
                return()
            }

            if (!requireNamespace("ordinal", quietly = TRUE)) {
                self$results$instructions$setContent(
                    "<p>The 'ordinal' R package is required. Install it with: install.packages('ordinal')</p>"
                )
                return()
            }

            # Prepare data
            data <- private$.prepareData()
            if (is.null(data)) return()

            # Fit model
            model <- private$.fitModel(data)
            if (is.null(model)) return()

            # Populate results
            if (self$options$modelFit)
                private$.populateModelFit(model)

            private$.populateFixedEffects(model)

            if (self$options$thresholdsTable)
                private$.populateThresholds(model)

            if (self$options$randomEffectsTable)
                private$.populateRandomEffects(model)

            if (self$options$propOddsTest)
                private$.populatePropOddsTest(model)

            if (self$options$conditionMeans)
                private$.populateConditionProbs(model, data)

            private$.generateMethodExplanation(model)
        },

        .prepareData = function() {

            tryCatch({
                dep_name <- self$options$dep
                data <- self$data

                # Build data frame with needed columns
                cols <- c(dep_name, self$options$fixedFactors,
                          self$options$fixedCovs, self$options$randomTerms)
                cols <- unique(cols)

                analysis_data <- data.frame(stringsAsFactors = FALSE)
                for (col in cols) {
                    if (!is.null(data[[col]])) {
                        analysis_data[[col]] <- data[[col]]
                    }
                }

                # Remove incomplete cases
                analysis_data <- analysis_data[complete.cases(analysis_data), ]
                if (nrow(analysis_data) < 10) {
                    self$results$instructions$setContent(
                        "<p>Insufficient complete cases for model fitting (need at least 10).</p>"
                    )
                    return(NULL)
                }

                # Convert dependent to ordered factor
                dep_vals <- analysis_data[[dep_name]]
                if (is.numeric(dep_vals)) {
                    analysis_data[[dep_name]] <- as.ordered(dep_vals)
                } else if (is.factor(dep_vals)) {
                    analysis_data[[dep_name]] <- as.ordered(dep_vals)
                } else {
                    analysis_data[[dep_name]] <- as.ordered(as.character(dep_vals))
                }

                n_levels <- nlevels(analysis_data[[dep_name]])
                if (n_levels < 3) {
                    self$results$instructions$setContent(
                        "<p>The dependent variable must have at least 3 ordered levels. Consider logistic regression for binary outcomes.</p>"
                    )
                    return(NULL)
                }

                # Ensure factors for fixed factors and random terms
                for (ff in self$options$fixedFactors) {
                    analysis_data[[ff]] <- as.factor(analysis_data[[ff]])
                }
                for (rt in self$options$randomTerms) {
                    analysis_data[[rt]] <- as.factor(analysis_data[[rt]])
                }
                # TODO (correctness, ⚠ behavior risk): `as.numeric()` on a factor returns
                # level indices, not the jamovi values-attribute coding. fixedCovs is
                # permitted:numeric so should not arrive as factor in practice, but if a
                # user picks a factor that R coerces, downstream estimates would be wrong.
                # Consider guarding with `is.factor(analysis_data[[cv]])` → reject, or
                # swap to `jmvcore::toNumeric()`. Needs manual review of expected semantics.
                # Ensure numeric for covariates
                for (cv in self$options$fixedCovs) {
                    analysis_data[[cv]] <- as.numeric(analysis_data[[cv]])
                }

                return(analysis_data)
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste("<p>Error preparing data:", htmltools::htmlEscape(conditionMessage(e)), "</p>")
                )
                return(NULL)
            })
        },

        .fitModel = function(data) {

            tryCatch({
                dep_name <- self$options$dep

                # Build fixed effects formula
                fixed_parts <- c()
                for (ff in self$options$fixedFactors) {
                    fixed_parts <- c(fixed_parts, jmvcore::composeTerm(ff))
                }
                for (cv in self$options$fixedCovs) {
                    fixed_parts <- c(fixed_parts, jmvcore::composeTerm(cv))
                }

                # Build random effects
                random_parts <- c()
                for (rt in self$options$randomTerms) {
                    random_parts <- c(random_parts,
                        paste0("(1|", jmvcore::composeTerm(rt), ")"))
                }

                # Full formula
                formula_str <- paste0(
                    jmvcore::composeTerm(dep_name), " ~ ",
                    paste(fixed_parts, collapse = " + "), " + ",
                    paste(random_parts, collapse = " + ")
                )

                # `|` allow-listed for lme4-style random-effects bar `(1|group)` (built at L177).
                # composeTerm has already backtick-quoted all variable names (Defense 1).
                formula <- jmvcore::asFormula(formula_str, additional_allowed_functions = c("|"))

                # Fit CLMM
                model <- ordinal::clmm(
                    formula,
                    data = data,
                    link = self$options$link,
                    threshold = self$options$threshold
                )

                return(model)

            }, error = function(e) {
                self$results$instructions$setContent(
                    paste("<p>Error fitting model:", htmltools::htmlEscape(conditionMessage(e)),
                          "</p><p>This may occur with sparse data, empty cells, or separation.",
                          "Try simplifying the model or checking data balance.</p>")
                )
                return(NULL)
            })
        },

        .populateModelFit = function(model) {

            table <- self$results$modelFitTable
            summ <- summary(model)

            table$addRow(rowKey = 1, values = list(
                stat = "AIC",
                value = AIC(model)
            ))
            table$addRow(rowKey = 2, values = list(
                stat = "BIC",
                value = BIC(model)
            ))
            table$addRow(rowKey = 3, values = list(
                stat = "Log-likelihood",
                value = as.numeric(logLik(model))
            ))
            table$addRow(rowKey = 4, values = list(
                stat = "N observations",
                value = nobs(model)
            ))
        },

        .populateFixedEffects = function(model) {

            table <- self$results$fixedEffects
            summ <- summary(model)
            coefs <- summ$coefficients

            # Only the fixed effect rows (not thresholds)
            beta_names <- names(model$beta)
            if (is.null(beta_names) || length(beta_names) == 0) {
                table$addRow(rowKey = 1, values = list(
                    term = "(no fixed effects estimated)",
                    estimate = NA, se = NA, z = NA, p = NA
                ))
                return()
            }

            ci_level <- self$options$confLevel
            z_crit <- qnorm((1 + ci_level) / 2)

            for (i in seq_along(beta_names)) {
                nm <- beta_names[i]
                row_idx <- which(rownames(coefs) == nm)
                if (length(row_idx) == 0) next

                est <- coefs[row_idx, "Estimate"]
                se <- coefs[row_idx, "Std. Error"]
                z_val <- coefs[row_idx, "z value"]
                p_val <- coefs[row_idx, "Pr(>|z|)"]

                vals <- list(
                    term = nm,
                    estimate = est,
                    se = se,
                    z = z_val,
                    p = p_val
                )

                if (self$options$oddsRatios) {
                    vals$or <- exp(est)
                }
                if (self$options$confInt) {
                    vals$ciLower <- exp(est - z_crit * se)
                    vals$ciUpper <- exp(est + z_crit * se)
                }

                table$addRow(rowKey = i, values = vals)
            }
        },

        .populateThresholds = function(model) {

            table <- self$results$thresholds
            summ <- summary(model)
            coefs <- summ$coefficients

            alpha_names <- names(model$alpha)
            if (is.null(alpha_names)) return()

            for (i in seq_along(alpha_names)) {
                nm <- alpha_names[i]
                row_idx <- which(rownames(coefs) == nm)
                if (length(row_idx) == 0) next

                table$addRow(rowKey = i, values = list(
                    threshold = nm,
                    estimate = coefs[row_idx, "Estimate"],
                    se = coefs[row_idx, "Std. Error"],
                    z = coefs[row_idx, "z value"],
                    p = coefs[row_idx, "Pr(>|z|)"]
                ))
            }
        },

        .populateRandomEffects = function(model) {

            table <- self$results$randomEffects
            summ <- summary(model)

            re_info <- summ$ST
            re_names <- names(re_info)

            for (i in seq_along(re_names)) {
                grp_name <- re_names[i]
                sd_val <- as.numeric(re_info[[i]])
                var_val <- sd_val^2
                n_lev <- summ$dims$nlev.re[grp_name]

                table$addRow(rowKey = i, values = list(
                    group = grp_name,
                    variance = var_val,
                    sd = sd_val,
                    nLevels = as.integer(n_lev)
                ))
            }
        },

        .populatePropOddsTest = function(model) {

            table <- self$results$propOddsTestTable

            tryCatch({
                nom_test <- ordinal::nominal_test(model)

                test_df <- as.data.frame(nom_test)
                test_names <- rownames(test_df)

                row_key <- 1
                for (i in seq_along(test_names)) {
                    nm <- test_names[i]
                    if (nm == "<none>") next

                    # Extract values safely
                    chisq_col <- grep("stat|Chisq", colnames(test_df), value = TRUE)
                    df_col <- grep("^df$", colnames(test_df), value = TRUE)
                    p_col <- grep("Pr|p", colnames(test_df), value = TRUE)

                    chisq_val <- if (length(chisq_col) > 0) test_df[i, chisq_col[1]] else NA
                    df_val <- if (length(df_col) > 0) test_df[i, df_col[1]] else NA
                    p_val <- if (length(p_col) > 0) test_df[i, p_col[1]] else NA

                    conclusion <- if (!is.na(p_val) && p_val < 0.05) {
                        "Proportional odds assumption may be violated (p < 0.05)"
                    } else if (is.na(p_val)) {
                        "Could not determine"
                    } else {
                        "Proportional odds assumption is tenable"
                    }

                    table$addRow(rowKey = row_key, values = list(
                        term = nm,
                        chisq = as.numeric(chisq_val),
                        df = as.integer(df_val),
                        p = as.numeric(p_val),
                        conclusion = conclusion
                    ))
                    row_key <- row_key + 1
                }
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    term = "Test could not be performed",
                    chisq = NA,
                    df = NA,
                    p = NA,
                    conclusion = paste("Error:", e$message)
                ))
            })
        },

        .populateConditionProbs = function(model, data) {

            table <- self$results$conditionProbs

            tryCatch({
                dep_name <- self$options$dep
                resp_levels <- levels(data[[dep_name]])
                row_key <- 1

                for (ff in self$options$fixedFactors) {
                    factor_levels <- levels(data[[ff]])
                    for (lv in factor_levels) {
                        newdata <- data[1, , drop = FALSE]
                        newdata[[ff]] <- factor(lv, levels = factor_levels)

                        pred <- tryCatch(
                            predict(model, newdata = newdata, type = "prob"),
                            error = function(e) NULL
                        )

                        if (!is.null(pred) && !is.null(pred$fit)) {
                            prob_str <- paste(
                                sapply(seq_along(resp_levels), function(j) {
                                    sprintf("%s: %.3f", resp_levels[j], pred$fit[j])
                                }),
                                collapse = ", "
                            )
                        } else {
                            prob_str <- "Could not compute"
                        }

                        table$addRow(rowKey = row_key, values = list(
                            factor = ff,
                            level = lv,
                            probabilities = prob_str
                        ))
                        row_key <- row_key + 1
                    }
                }
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    factor = "Error",
                    level = "",
                    probabilities = e$message
                ))
            })
        },

        .generateMethodExplanation = function(model) {

            link_desc <- switch(self$options$link,
                "logit" = "logit (proportional odds)",
                "probit" = "probit",
                "cloglog" = "complementary log-log",
                "loglog" = "log-log",
                "cauchit" = "cauchit",
                self$options$link
            )

            random_desc <- paste(htmltools::htmlEscape(self$options$randomTerms), collapse = ", ")

            content <- paste0(
                "<html><body>",
                "<h3>Method: Cumulative Link Mixed Model (CLMM)</h3>",
                "<p>This analysis fits a <strong>cumulative link mixed model</strong> with a ",
                "<strong>", link_desc, "</strong> link function. ",
                "Random intercepts are estimated for: <strong>", random_desc, "</strong>.</p>",

                "<h4>Model Specification</h4>",
                "<p>The cumulative model assumes that the ordinal response Y has categories ",
                "1 &lt; 2 &lt; ... &lt; J, and models:</p>",
                "<p style='font-family: monospace; background: #f5f5f5; padding: 8px;'>",
                "logit[P(Y &le; j)] = &alpha;<sub>j</sub> - X&beta; - Zu</p>",
                "<p>where &alpha;<sub>j</sub> are threshold parameters, X&beta; are fixed effects, ",
                "and Zu are random effects.</p>",

                "<h4>Interpretation</h4>",
                "<ul>",
                "<li><strong>Positive coefficients</strong>: Higher values of the predictor are associated ",
                "with higher ordinal categories</li>",
                "<li><strong>Odds Ratios &gt; 1</strong>: The odds of being in a higher category increase ",
                "with the predictor</li>",
                "<li><strong>Proportional odds</strong>: The effect is assumed constant across all ",
                "category thresholds (test this assumption)</li>",
                "</ul>",

                "<h4>Clinical Applications</h4>",
                "<ul>",
                "<li>Comparing Likert-scale ratings across groups with rater random effects</li>",
                "<li>Analyzing tumor grades with patient clustering</li>",
                "<li>Multi-site studies with ordinal outcomes and site-level variability</li>",
                "<li>Repeated ordinal assessments (e.g., disease severity over time)</li>",
                "</ul>",

                "<h4>References</h4>",
                "<p>Christensen RHB (2019). ordinal - Regression Models for Ordinal Data. ",
                "R package. CRAN.</p>",
                "</body></html>"
            )

            self$results$methodExplanation$setContent(content)
        }
    )
)
