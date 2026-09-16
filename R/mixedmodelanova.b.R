mixedmodelanovaClass <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "mixedmodelanovaClass",
    inherit = mixedmodelanovaBase,
    private = list(
        .run = function() {

            # Check if required packages are available
            if (!requireNamespace("lme4", quietly = TRUE)) {
                jmvcore::reject("Package 'lme4' is required but not installed. Please install it.")
            }
            if (!requireNamespace("lmerTest", quietly = TRUE)) {
                jmvcore::reject("Package 'lmerTest' is required but not installed. Please install it.")
            }

            # Get variables
            dep <- self$options$dependent
            fixed_fac <- self$options$fixed_factors
            random_fac <- self$options$random_factors
            covars <- self$options$covariates

            # Check if we have the minimum required variables
            if (is.null(dep) || length(random_fac) == 0) {
                return()
            }

            # Get data
            data <- self$data

            # Prepare data
            for (var in random_fac) {
                if (!is.factor(data[[var]])) {
                    data[[var]] <- as.factor(data[[var]])
                }
            }
            for (var in fixed_fac) {
                if (!is.factor(data[[var]])) {
                    data[[var]] <- as.factor(data[[var]])
                }
            }

            # Build formula
            formula_str <- private$.buildFormula()
            if (is.null(formula_str)) {
                return()
            }

            # Fit model
            # TODO (correctness): this tryCatch swallows any jmvcore::reject()
            # raised from inside lmer/lmerTest and rewraps it as "Model fitting
            # failed: <msg>", which loses the original notice category. Consider
            # rethrowing jmvcore-classed errors unchanged and only reformatting
            # generic R errors.
            model <- tryCatch({
                reml <- self$options$estimation_method == "reml"
                lmerTest::lmer(jmvcore::asFormula(formula_str), data = data, REML = reml)
            }, error = function(e) {
                jmvcore::reject("Model fitting failed: {}", e$message)
            })

            # Store model for use by other methods
            private$.model <- model

            # Populate results
            private$.populateModelInfo(model, formula_str)
            private$.populateFixedEffects(model)
            private$.populateAnovaTable(model)
            private$.populateRandomEffects(model)
            private$.populateModelFit(model)
            private$.populateICC(model)
            private$.populateEffectSizes(model)
            if (self$options$show_posthoc) {
                private$.populatePostHoc(model)
            }
            private$.populateAssumptions(model)
            private$.populateInterpretation()
            private$.populateMethodology()
            private$.populateReferences()
        },

        .model = NULL,

        .buildFormula = function() {
            dep <- self$options$dependent
            fixed_fac <- self$options$fixed_factors
            random_fac <- self$options$random_factors
            covars <- self$options$covariates
            model_type <- self$options$model_type
            interaction_terms <- self$options$interaction_terms

            # Build fixed effects part
            fixed_terms <- c(fixed_fac, covars)
            if (length(fixed_terms) == 0) {
                fixed_part <- "1"
            } else if (interaction_terms && length(fixed_fac) > 1) {
                fixed_part <- paste(vapply(fixed_fac, jmvcore::composeTerm, character(1)), collapse = " * ")
                if (length(covars) > 0) {
                    fixed_part <- paste(c(fixed_part, vapply(covars, jmvcore::composeTerm, character(1))), collapse = " + ")
                }
            } else {
                fixed_part <- paste(vapply(fixed_terms, jmvcore::composeTerm, character(1)), collapse = " + ")
            }

            # Build random effects part
            if (model_type == "random_intercept") {
                random_part <- paste0("(1 | ", jmvcore::composeTerm(random_fac[1]), ")")
            } else if (model_type == "random_slope") {
                if (length(fixed_fac) == 0) {
                    jmvcore::reject("Random slope model requires at least one fixed factor")
                }
                random_part <- paste0("(", jmvcore::composeTerm(fixed_fac[1]), " | ", jmvcore::composeTerm(random_fac[1]), ")")
            } else if (model_type == "nested") {
                if (length(random_fac) < 2) {
                    jmvcore::reject("Nested design requires at least two random factors")
                }
                random_part <- paste0("(1 | ", jmvcore::composeTerm(random_fac[1]), "/", jmvcore::composeTerm(random_fac[2]), ")")
            }

            # Combine
            formula_str <- paste(jmvcore::composeTerm(dep), "~", fixed_part, "+", random_part)
            return(formula_str)
        },

        .populateModelInfo = function(model, formula_str) {
            table <- self$results$model_info

            n_obs <- nrow(model@frame)
            n_groups <- length(unique(model@frame[[names(model@flist)[1]]]))
            estimation <- toupper(self$options$estimation_method)

            table$setRow(rowNo = 1, values = list(
                formula = formula_str,
                estimation = estimation,
                n_obs = n_obs,
                n_groups = n_groups
            ))
        },

        .populateFixedEffects = function(model) {
            if (!self$options$show_fixed_effects) return()

            table <- self$results$fixed_effects
            coef_summary <- summary(model)$coefficients

            for (i in seq_len(nrow(coef_summary))) {
                table$addRow(rowKey = i, values = list(
                    term = rownames(coef_summary)[i],
                    estimate = coef_summary[i, "Estimate"],
                    se = coef_summary[i, "Std. Error"],
                    df = coef_summary[i, "df"],
                    t_value = coef_summary[i, "t value"],
                    p_value = coef_summary[i, "Pr(>|t|)"]
                ))
            }
        },

        .populateAnovaTable = function(model) {
            if (!self$options$show_fixed_effects) return()

            table <- self$results$anova_table
            anova_result <- anova(model)

            for (i in seq_len(nrow(anova_result))) {
                table$addRow(rowKey = i, values = list(
                    term = rownames(anova_result)[i],
                    sumsq = anova_result[i, "Sum Sq"],
                    meansq = anova_result[i, "Mean Sq"],
                    numdf = anova_result[i, "NumDF"],
                    dendf = anova_result[i, "DenDF"],
                    f_value = anova_result[i, "F value"],
                    p_value = anova_result[i, "Pr(>F)"]
                ))
            }
        },

        .populateRandomEffects = function(model) {
            if (!self$options$show_random_effects) return()

            table <- self$results$random_effects
            vc <- as.data.frame(lme4::VarCorr(model))

            for (i in seq_len(nrow(vc))) {
                if (!is.na(vc$grp[i])) {
                    # TODO (cleanup): when var1 is NA this paste() leaves a
                    # trailing space ("Subject "). Use paste0 or trimws to clean it.
                    group_name <- paste(vc$grp[i], if (!is.na(vc$var1[i])) paste0(" (", vc$var1[i], ")") else "")
                    table$addRow(rowKey = i, values = list(
                        group = group_name,
                        variance = vc$vcov[i],
                        std_dev = vc$sdcor[i]
                    ))
                }
            }
        },

        .populateModelFit = function(model) {
            if (!self$options$show_model_fit) return()

            table <- self$results$model_fit
            table$setRow(rowNo = 1, values = list(
                aic = AIC(model),
                bic = BIC(model),
                loglik = as.numeric(logLik(model)),
                deviance = deviance(model)
            ))
        },

        .populateICC = function(model) {
            if (!self$options$show_icc) return()

            table <- self$results$icc_table

            # Calculate ICC
            # TODO (correctness): if VarCorr() does not return a "Residual" row
            # (some fits, e.g. GLMM families, omit it) then var_residual is
            # numeric(0) and the ICC becomes numeric(0)/NaN. Guard with a
            # length check and surface a notice instead of populating NaN.
            vc <- as.data.frame(lme4::VarCorr(model))
            var_random <- sum(vc$vcov[!is.na(vc$grp) & vc$grp != "Residual"])
            var_residual <- vc$vcov[vc$grp == "Residual"]
            icc <- var_random / (var_random + var_residual)

            interpretation <- if (icc < 0.05) {
                "Very low clustering"
            } else if (icc < 0.10) {
                "Low clustering"
            } else if (icc < 0.15) {
                "Moderate clustering"
            } else {
                "High clustering"
            }

            table$addRow(rowKey = 1, values = list(
                group = names(model@flist)[1],
                icc = icc,
                interpretation = interpretation
            ))
        },

        .populateEffectSizes = function(model) {
            if (!self$options$show_effect_sizes) return()

            table <- self$results$effect_sizes
            anova_result <- anova(model)

            for (i in seq_len(nrow(anova_result))) {
                # Calculate partial eta squared
                f_val <- anova_result[i, "F value"]
                df1 <- anova_result[i, "NumDF"]
                df2 <- anova_result[i, "DenDF"]
                partial_eta_sq <- (f_val * df1) / (f_val * df1 + df2)

                interpretation <- if (partial_eta_sq < 0.01) {
                    "Negligible"
                } else if (partial_eta_sq < 0.06) {
                    "Small"
                } else if (partial_eta_sq < 0.14) {
                    "Medium"
                } else {
                    "Large"
                }

                table$addRow(rowKey = i, values = list(
                    term = rownames(anova_result)[i],
                    partial_eta_sq = partial_eta_sq,
                    interpretation = interpretation
                ))
            }
        },

        .populatePostHoc = function(model) {
            if (!requireNamespace("emmeans", quietly = TRUE)) {
                return()
            }

            table <- self$results$posthoc
            fixed_fac <- self$options$fixed_factors
            if (length(fixed_fac) == 0) return()

            tryCatch({
                emm <- emmeans::emmeans(model, specs = fixed_fac[1])
                pairs <- emmeans::pairs(emm, adjust = self$options$posthoc_method)
                pairs_summary <- summary(pairs)

                for (i in seq_len(nrow(pairs_summary))) {
                    table$addRow(rowKey = i, values = list(
                        contrast = as.character(pairs_summary$contrast[i]),
                        estimate = pairs_summary$estimate[i],
                        se = pairs_summary$SE[i],
                        df = pairs_summary$df[i],
                        t_ratio = pairs_summary$t.ratio[i],
                        p_value = pairs_summary$p.value[i]
                    ))
                }
            }, error = function(e) {
                # Silently fail if emmeans doesn't work
            })
        },

        .populateAssumptions = function(model) {
            if (!self$options$show_assumptions) return()

            table <- self$results$assumptions
            residuals <- residuals(model)

            # Normality test (Shapiro-Wilk on sample if > 5000 observations)
            if (length(residuals) > 5000) {
                residuals_sample <- sample(residuals, 5000)
            } else {
                residuals_sample <- residuals
            }

            shapiro <- shapiro.test(residuals_sample)
            table$addRow(rowKey = 1, values = list(
                test = "Shapiro-Wilk (Normality of Residuals)",
                statistic = shapiro$statistic,
                p_value = shapiro$p.value,
                result = if (shapiro$p.value > 0.05) "Assumption met" else "Assumption violated"
            ))
        },

        .diagnosticPlots = function(image, ggtheme, theme, ...) {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar), add = TRUE)
            if (!self$options$show_plots) return()
            if (is.null(private$.model)) return()

            model <- private$.model

            # TODO (UX): the ggtheme/theme arguments jamovi passes in are ignored
            # because we render via base graphics. Migrate to a ggplot2-based
            # diagnostic panel (e.g. four panels via patchwork/cowplot applying
            # ggtheme + theme) so the plot honors the user's chosen jamovi theme.
            # Create 2x2 diagnostic plot
            par(mfrow = c(2, 2))
            plot(model)

            TRUE
        },

        .populateInterpretation = function() {
            html <- self$results$interpretation

            interpretation_text <- "
            <h3>How to Interpret Mixed Model ANOVA Results</h3>
            <p><strong>Fixed Effects:</strong> Test whether predictors have significant effects on the outcome.</p>
            <p><strong>Random Effects:</strong> Show variance at different levels (e.g., between subjects).</p>
            <p><strong>ICC (Intraclass Correlation):</strong> Proportion of variance due to grouping.
               Values > 0.10 indicate substantial clustering.</p>
            <p><strong>Effect Sizes (Partial \u{03B7}\u{00B2}):</strong> 0.01 = small, 0.06 = medium, 0.14 = large.</p>
            "

            html$setContent(interpretation_text)
        },

        .populateMethodology = function() {
            if (!self$options$show_methodology) return()

            html <- self$results$methodology

            methodology_text <- "
            <h3>Methodology</h3>
            <p>Mixed model ANOVA (also called multilevel modeling or hierarchical linear modeling) analyzes
            data with both fixed and random effects. This approach accounts for non-independence in nested
            or repeated measures data.</p>
            <p><strong>Model:</strong> Linear mixed effects model fitted using lme4 and lmerTest packages.</p>
            <p><strong>Estimation:</strong> REML (Restricted Maximum Likelihood) for unbiased variance estimates,
            or ML (Maximum Likelihood) for model comparisons.</p>
            <p><strong>Type III ANOVA:</strong> Tests each effect while controlling for all other effects.</p>
            "

            html$setContent(methodology_text)
        },

        .populateReferences = function() {
            if (!self$options$show_references) return()

            html <- self$results$references

            references_text <- "
            <h3>References</h3>
            <ul>
            <li>Bates, D., M\u{00E4}chler, M., Bolker, B., & Walker, S. (2015). Fitting Linear Mixed-Effects Models Using lme4.
                <em>Journal of Statistical Software</em>, 67(1), 1-48.</li>
            <li>Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). lmerTest Package: Tests in Linear Mixed Effects Models.
                <em>Journal of Statistical Software</em>, 82(13), 1-26.</li>
            <li>Snijders, T. A. B., & Bosker, R. J. (2012). <em>Multilevel Analysis: An Introduction to Basic and Advanced Multilevel Modeling</em> (2nd ed.).
                Sage Publications.</li>
            </ul>
            "

            html$setContent(references_text)
        }
    )
)
