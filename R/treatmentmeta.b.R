
# This file is a generated template, your changes will not be overwritten

treatmentmetaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "treatmentmetaClass",
    inherit = treatmentmetaBase,
    private = list(
        .init = function() {
            todo <- "
            <br><h3>Welcome to Treatment Effect Meta-Analysis</h3>
            <br><h4>How to use:</h4>
            <ol>
            <li><b>Select Outcome Type:</b> Choose between Continuous, Binary, Correlation, Survival, or Generic outcomes.</li>
            <li><b>Assign Variables:</b> Move the appropriate variables into the specific boxes for your outcome type.</li>
            <li><b>Choose Model:</b> Select Fixed-Effect or various Random-Effects models.</li>
            <li><b>Review Results:</b> The pooled effect estimate and forest plot will appear automatically.</li>
            </ol>
            
            <br><h4>Key Features:</h4>
            <ul>
            <li>Comprehensive effect measures for medical research</li>
            <li>Multiple random-effects estimators (REML, DL, PM, HKSJ)</li>
            <li>Advanced heterogeneity, subgroup, and moderator analysis</li>
            <li>Rigorous publication bias assessment suite</li>
            <li>Publication-ready forest and funnel plots</li>
            </ul>
            "
            self$results$instructions$setContent(todo)
        },

        .run = function() {
            # Check for required data
            if (is.null(self$options$study_id) || is.null(self$data)) {
                return()
            }

            # Package check
            if (!requireNamespace('meta', quietly=TRUE)) {
                stop('The "meta" package is required but not installed.')
            }

            # Prepare data and options
            data <- self$data
            outcome_type <- self$options$outcome_type
            
            # Dispatch to specific meta-analysis functions
            tryCatch({
                meta_res <- private$.performMetaAnalysis(data, outcome_type)
                
                if (!is.null(meta_res)) {
                    # Populate Results Tables
                    private$.populateDataSummary(meta_res)
                    private$.populatePooledEffects(meta_res)
                    private$.populateIndividualStudies(meta_res)
                    
                    if (self$options$heterogeneity_test) {
                        private$.populateHeterogeneity(meta_res)
                    }
                    
                    if (!is.null(self$options$subgroup_var)) {
                        private$.populateSubgroupAnalysis(meta_res)
                    }
                    
                    if (length(self$options$moderator_vars) > 0) {
                        private$.populateMetaRegression(meta_res)
                    }
                    
                    if (self$options$sensitivity_analysis) {
                        private$.populateSensitivity(meta_res)
                    }
                    
                    if (self$options$publication_bias) {
                        private$.populatePublicationBias(meta_res)
                    }
                    
                    if (self$options$trim_fill) {
                        private$.populateTrimFill(meta_res)
                    }
                    
                    if (self$options$pcurve) {
                        private$.populatePCurve(meta_res)
                    }
                    
                    if (self$options$bayesian_analysis) {
                        private$.populateBayesian(meta_res)
                    }
                    
                    if (self$options$cumulative_meta) {
                        private$.populateCumulativeSummary(meta_res)
                    }
                    
                    if (!is.null(self$options$quality_score)) {
                        private$.populateQualityAssessment(meta_res)
                    }
                    
                    if (self$options$influence_diagnostics) {
                        private$.populateInfluence(meta_res)
                    }
                    
                    if (self$options$show_interpretation) {
                        private$.populateInterpretation(meta_res)
                    }
                    
                    # Store meta_res for plots
                    private$.meta_res <- meta_res
                }
                
            }, error = function(e) {
                jmvcore::reject(paste("Meta-analysis error: ", e$message))
            })
        },

        .meta_res = NULL,

        .performMetaAnalysis = function(data, outcome_type) {
            # Common options
            sm <- self$options$effect_measure
            level <- self$options$confidence_level
            
            # Small study correction (Hartung-Knapp)
            # Jamovi option 'small_study_correction' maps to adhoc.hakn in meta
            adhoc_hakn_ci <- if (self$options$small_study_correction) "ci" else ""
            
            method_tau <- switch(self$options$model_type,
                                "fixed" = "FE",
                                "random" = "REML",
                                "random_dl" = "DL",
                                "random_pm" = "PM",
                                "random_hksj" = "REML") # HKSJ is handled by adhoc.hakn
            
            # If random_hksj is selected, force adhoc_hakn_ci to "ci"
            if (self$options$model_type == "random_hksj") {
                adhoc_hakn_ci <- "ci"
            }
            
            common <- self$options$model_type == "fixed"
            random <- self$options$model_type != "fixed"
            
            res <- NULL
            
            if (outcome_type == "continuous") {
                if (is.null(self$options$mean_treatment) || is.null(self$options$sd_treatment) || is.null(self$options$n_treatment) ||
                    is.null(self$options$mean_control) || is.null(self$options$sd_control) || is.null(self$options$n_control)) {
                    return(NULL)
                }
                res <- meta::metacont(
                    n.e = data[[self$options$n_treatment]],
                    mean.e = data[[self$options$mean_treatment]],
                    sd.e = data[[self$options$sd_treatment]],
                    n.c = data[[self$options$n_control]],
                    mean.c = data[[self$options$mean_control]],
                    sd.c = data[[self$options$sd_control]],
                    studlab = data[[self$options$study_id]],
                    data = data,
                    sm = sm,
                    common = common,
                    random = random,
                    level = level,
                    method.tau = method_tau,
                    adhoc.hakn.ci = adhoc_hakn_ci,
                    subgroup = if (!is.null(self$options$subgroup_var)) data[[self$options$subgroup_var]] else NULL,
                    weights = if (self$options$weight_by_quality && !is.null(self$options$quality_score)) data[[self$options$quality_score]] else NULL
                )
            } else if (outcome_type == "binary") {
                if (is.null(self$options$events_treatment) || is.null(self$options$n_treatment) ||
                    is.null(self$options$events_control) || is.null(self$options$n_control)) {
                    return(NULL)
                }
                res <- meta::metabin(
                    event.e = data[[self$options$events_treatment]],
                    n.e = data[[self$options$n_treatment]],
                    event.c = data[[self$options$events_control]],
                    n.c = data[[self$options$n_control]],
                    studlab = data[[self$options$study_id]],
                    data = data,
                    sm = sm,
                    common = common,
                    random = random,
                    level = level,
                    method.tau = method_tau,
                    adhoc.hakn.ci = adhoc_hakn_ci,
                    subgroup = if (!is.null(self$options$subgroup_var)) data[[self$options$subgroup_var]] else NULL,
                    weights = if (self$options$weight_by_quality && !is.null(self$options$quality_score)) data[[self$options$quality_score]] else NULL
                )
            } else if (outcome_type == "correlation") {
                if (is.null(self$options$correlation) || is.null(self$options$sample_size)) {
                    return(NULL)
                }
                res <- meta::metacor(
                    cor = data[[self$options$correlation]],
                    n = data[[self$options$sample_size]],
                    studlab = data[[self$options$study_id]],
                    data = data,
                    sm = sm,
                    common = common,
                    random = random,
                    level = level,
                    method.tau = method_tau,
                    adhoc.hakn.ci = adhoc_hakn_ci,
                    subgroup = if (!is.null(self$options$subgroup_var)) data[[self$options$subgroup_var]] else NULL,
                    weights = if (self$options$weight_by_quality && !is.null(self$options$quality_score)) data[[self$options$quality_score]] else NULL
                )
            } else if (outcome_type == "generic" || outcome_type == "survival") {
                eff <- NULL
                se <- NULL
                
                if (outcome_type == "survival") {
                    if (!is.null(self$options$hazard_ratio)) {
                        eff <- log(data[[self$options$hazard_ratio]])
                        if (!is.null(self$options$log_hr_se)) {
                            se <- data[[self$options$log_hr_se]]
                        } else if (!is.null(self$options$ci_lower) && !is.null(self$options$ci_upper)) {
                            # Calculate SE from CI
                            z <- qnorm(1 - (1 - level)/2)
                            se <- (log(data[[self$options$ci_upper]]) - log(data[[self$options$ci_lower]])) / (2 * z)
                        }
                    }
                } else if (outcome_type == "generic") {
                    if (!is.null(self$options$effect_size)) {
                        eff <- data[[self$options$effect_size]]
                        if (!is.null(self$options$standard_error)) {
                            se <- data[[self$options$standard_error]]
                        } else if (!is.null(self$options$ci_lower) && !is.null(self$options$ci_upper)) {
                            # Calculate SE from CI
                            z <- qnorm(1 - (1 - level)/2)
                            se <- (data[[self$options$ci_upper]] - data[[self$options$ci_lower]]) / (2 * z)
                        }
                    }
                }
                
                if (is.null(eff) || is.null(se)) {
                    return(NULL)
                }
                
                res <- meta::metagen(
                    TE = eff,
                    seTE = se,
                    studlab = data[[self$options$study_id]],
                    data = data,
                    sm = sm,
                    common = common,
                    random = random,
                    level = level,
                    method.tau = method_tau,
                    adhoc.hakn.ci = adhoc_hakn_ci,
                    subgroup = if (!is.null(self$options$subgroup_var)) data[[self$options$subgroup_var]] else NULL,
                    weights = if (self$options$weight_by_quality && !is.null(self$options$quality_score)) data[[self$options$quality_score]] else NULL
                )
            }
            return(res)
        },

        .populateDataSummary = function(res) {
            table <- self$results$data_summary
            
            table$addRow(rowKey="studies", values=list(
                metric="Number of studies",
                value=as.character(res$k),
                details="Total number of studies included in meta-analysis"
            ))
            
            # Observations
            obs_val <- NULL
            if (!is.null(res$n.e) && !is.null(res$n.c)) {
                obs_val <- sum(res$n.e + res$n.c, na.rm=TRUE)
            } else if (!is.null(res$n)) {
                obs_val <- sum(res$n, na.rm=TRUE)
            }
            
            if (!is.null(obs_val)) {
                table$addRow(rowKey="obs", values=list(
                    metric="Total observations",
                    value=as.character(obs_val),
                    details="Total number of participants across all groups or sample sizes"
                ))
            }
            
            # Events
            if (!is.null(res$event.e) && !is.null(res$event.c)) {
                e_tot <- sum(res$event.e, na.rm=TRUE)
                c_tot <- sum(res$event.c, na.rm=TRUE)
                table$addRow(rowKey="events", values=list(
                    metric="Total events",
                    value=as.character(e_tot + c_tot),
                    details=paste0("Treatment: ", e_tot, ", Control: ", c_tot)
                ))
            } else if (!is.null(res$event)) {
                 table$addRow(rowKey="events", values=list(
                    metric="Total events",
                    value=as.character(sum(res$event, na.rm=TRUE)),
                    details="Total number of events across all studies"
                ))
            }
        },

        .populatePooledEffects = function(res) {
            table <- self$results$pooled_effects
            
            # Fixed effect
            if (res$common) {
                table$addRow(rowKey="common", values=list(
                    model="Fixed Effect",
                    effect_size=res$TE.common,
                    se=res$seTE.common,
                    ci_lower=res$lower.common,
                    ci_upper=res$upper.common,
                    z_value=res$zval.common,
                    p_value=res$pval.common,
                    interpretation=private$.getInterpretation(res$TE.common, res$pval.common)
                ))
            }
            
            # Random effect
            if (res$random) {
                table$addRow(rowKey="random", values=list(
                    model=paste0("Random Effects (", res$method.tau, ")"),
                    effect_size=res$TE.random,
                    se=res$seTE.random,
                    ci_lower=res$lower.random,
                    ci_upper=res$upper.random,
                    z_value=res$zval.random,
                    p_value=res$pval.random,
                    interpretation=private$.getInterpretation(res$TE.random, res$pval.random)
                ))
            }
            
            # Prediction interval
            if (self$options$prediction_interval && !is.null(res$lower.predict)) {
                table$addRow(rowKey="predict", values=list(
                    model="Prediction Interval",
                    effect_size=res$TE.random,
                    ci_lower=res$lower.predict,
                    ci_upper=res$upper.predict,
                    interpretation="Estimated range for future studies"
                ))
            }
        },

        .populateIndividualStudies = function(res) {
            table <- self$results$individual_studies
            
            for (i in seq_along(res$studlab)) {
                # Get year if available
                year_val <- if (!is.null(self$options$year)) self$data[[self$options$year]][i] else NA
                
                # Use index as rowKey to ensure uniqueness even if study labels are weird
                table$addRow(rowKey=as.character(i), values=list(
                    study=as.character(res$studlab[i]),
                    year=as.integer(year_val),
                    effect_size=res$TE[i],
                    se=res$seTE[i],
                    ci_lower=res$lower[i],
                    ci_upper=res$upper[i],
                    weight_fixed=if (!is.null(res$w.common)) res$w.common[i] / sum(res$w.common, na.rm=TRUE) else NA,
                    weight_random=if (!is.null(res$w.random)) res$w.random[i] / sum(res$w.random, na.rm=TRUE) else NA
                ))
            }
        },

        .populateHeterogeneity = function(res) {
            table <- self$results$heterogeneity
            
            # Q-test
            table$addRow(rowKey="Q", values=list(
                measure="Cochran Q",
                value=as.character(round(res$Q, 2)),
                p_value=as.character(round(res$pval.Q, 4)),
                interpretation=if (res$pval.Q < 0.05) "Significant heterogeneity" else "No significant heterogeneity"
            ))
            
            # I-squared
            table$addRow(rowKey="I2", values=list(
                measure="I squared",
                value=paste0(round(res$I2 * 100, 1), "%"),
                ci_lower=paste0(round(res$lower.I2 * 100, 1), "%"),
                ci_upper=paste0(round(res$upper.I2 * 100, 1), "%"),
                interpretation=private$.getI2Interpretation(res$I2)
            ))
            
            # Tau-squared
            table$addRow(rowKey="tau2", values=list(
                measure="tau squared",
                value=as.character(round(res$tau2, 4)),
                ci_lower=as.character(round(res$lower.tau2, 4)),
                ci_upper=as.character(round(res$upper.tau2, 4))
            ))
        },

        .populateSubgroupAnalysis = function(res) {
            if (is.null(res$subgroup)) return()
            
            table <- self$results$subgroup_analysis
            
            # Subgroup results
            for (i in seq_along(res$subgroup.levels)) {
                level <- res$subgroup.levels[i]
                table$addRow(rowKey = level, values = list(
                    subgroup = as.character(level),
                    n_studies = res$k.subgroup[i],
                    effect_size = res$TE.random.subgroup[i],
                    ci_lower = res$lower.random.subgroup[i],
                    ci_upper = res$upper.random.subgroup[i],
                    p_value = res$pval.random.subgroup[i],
                    i_squared = res$I2.subgroup[i]
                ))
            }
            
            # Test for subgroup differences
            test_table <- self$results$subgroup_test
            test_table$addRow(rowKey = "between", values = list(
                test = "Between-subgroups Q-test",
                q_statistic = res$Q.b.random,
                df = res$df.Q.b,
                p_value = res$pval.Q.b.random,
                interpretation = if (res$pval.Q.b.random < 0.05) "Significant difference between subgroups" else "No significant difference"
            ))
        },
        
        .populateMetaRegression = function(res) {
            if (length(self$options$moderator_vars) == 0) return()
            
            try({
                # Perform meta-regression
                mods <- paste(self$options$moderator_vars, collapse = " + ")
                form <- as.formula(paste("~", mods))
                
                # Use metareg from meta package
                mr <- meta::metareg(res, form)
                
                table <- self$results$meta_regression
                
                for (i in seq_along(mr$beta)) {
                    mod_name <- names(mr$beta)[i]
                    if (is.null(mod_name)) mod_name <- paste0("Term ", i)
                    
                    table$addRow(rowKey = as.character(i), values = list(
                        moderator = mod_name,
                        coefficient = mr$beta[i],
                        se = mr$se[i],
                        ci_lower = mr$lower[i],
                        ci_upper = mr$upper[i],
                        z_value = mr$zval[i],
                        p_value = mr$pval[i]
                    ))
                }
            })
        },
        
        .populateSensitivity = function(res) {
            if (!self$options$sensitivity_analysis) return()
            
            try({
                # Leave-one-out analysis
                inf <- meta::metainf(res, pooled = "random")
                
                table <- self$results$sensitivity
                base_te <- res$TE.random
                
                for (i in seq_along(res$studlab)) {
                    te_omit <- inf$TE[i]
                    change <- if(!is.na(base_te) && base_te != 0) (te_omit - base_te) / abs(base_te) * 100 else 0
                    
                    table$addRow(rowKey = as.character(i), values = list(
                        omitted_study = as.character(res$studlab[i]),
                        effect_size = te_omit,
                        ci_lower = inf$lower[i],
                        ci_upper = inf$upper[i],
                        i_squared = inf$I2[i],
                        change_pct = change
                    ))
                }
            })
        },
        
        .populatePublicationBias = function(res) {
            if (!self$options$publication_bias) return()
            if (res$k < 3) return() # Bias tests need at least 3 studies
            
            table <- self$results$publication_bias_tests
            
            # Egger's test
            tryCatch({
                # Use direct TE and seTE to avoid subgroup issues in meta package
                egg <- meta::metabias(res$TE, res$seTE, method = "Egger", k.min = 3)
                p_val <- if (!is.null(egg$p.value)) egg$p.value else egg$pval
                
                if (!is.null(p_val) && length(p_val) > 0 && !is.na(p_val)) {
                    table$addRow(rowKey = "egger", values = list(
                        test = "Egger's regression test",
                        statistic = egg$statistic,
                        p_value = p_val,
                        interpretation = if (p_val < 0.05) "Evidence of small-study effects (asymmetry)" else "No evidence of small-study effects",
                        recommendation = if (p_val < 0.05) "Use trim-and-fill or interpret with caution" else "Results appear robust"
                    ))
                }
            }, error = function(e) {
                table$setNote("egger_error", paste("Egger's test error:", e$message))
            })
            
            # Begg's test
            tryCatch({
                # Use direct TE and seTE
                begg <- meta::metabias(res$TE, res$seTE, method = "Begg", k.min = 3)
                p_val_begg <- if (!is.null(begg$p.value)) begg$p.value else begg$pval
                
                if (!is.null(p_val_begg) && length(p_val_begg) > 0 && !is.na(p_val_begg)) {
                    table$addRow(rowKey = "begg", values = list(
                        test = "Begg's rank correlation test",
                        statistic = begg$statistic,
                        p_value = p_val_begg,
                        interpretation = if (p_val_begg < 0.05) "Significant asymmetry detected" else "No significant asymmetry",
                        recommendation = "Check funnel plot for visual confirmation"
                    ))
                }
            }, error = function(e) {
                table$setNote("begg_error", paste("Begg's test error:", e$message))
            })
        },

        .populateTrimFill = function(res) {
            if (!self$options$trim_fill) return()
            
            try({
                tf <- meta::trimfill(res)
                table <- self$results$trim_fill_results
                
                # Original
                table$addRow(rowKey = "original", values = list(
                    analysis = "Original (Random Effects)",
                    n_studies = res$k,
                    n_filled = 0,
                    effect_size = res$TE.random,
                    ci_lower = res$lower.random,
                    ci_upper = res$upper.random,
                    change_pct = 0
                ))
                
                # Adjusted
                change <- if(!is.na(res$TE.random) && res$TE.random != 0) (tf$TE.random - res$TE.random) / abs(res$TE.random) * 100 else 0
                
                table$addRow(rowKey = "adjusted", values = list(
                    analysis = "Trim and Fill Adjusted",
                    n_studies = tf$k,
                    n_filled = tf$k0,
                    effect_size = tf$TE.random,
                    ci_lower = tf$lower.random,
                    ci_upper = tf$upper.random,
                    change_pct = change
                ))
            })
        },
        
        .populateInfluence = function(res) {
            if (!self$options$influence_diagnostics) return()
            
            try({
                # Manually reconstruct metafor::rma model for influence diagnostics
                # Using the effect sizes and standard errors from the meta result
                m_rma <- metafor::rma(yi = res$TE, 
                                     vi = res$seTE^2, 
                                     method = res$method.tau,
                                     test = if (res$adhoc.hakn.ci == "ci") "knha" else "z")
                
                # Using the generic influence function which will dispatch to metafor::influence.rma.uni
                inf_rma <- influence(m_rma)
                
                table <- self$results$influence
                
                # Extract diagnostics from metafor influence object
                d <- as.data.frame(inf_rma$inf)
                
                for (i in seq_along(res$studlab)) {
                    is_outlier <- inf_rma$is.inf[i]
                    
                    table$addRow(rowKey = as.character(i), values = list(
                        study = as.character(res$studlab[i]),
                        dffits = d$dffits[i],
                        cooks_d = d$cook.d[i],
                        hatvalues = d$hat[i],
                        residual = d$rstudent[i],
                        outlier = if (is_outlier) "Influential" else "Robust"
                    ))
                }
            })
        },
        
        .populateInterpretation = function(res) {
            if (!self$options$show_interpretation) return()
            
            # Pooled Effect Synthesis
            te <- res$TE.random
            p <- res$pval.random
            sm <- res$sm
            
            interp <- "<h3>Clinical Interpretation</h3>"
            interp <- paste0(interp, "<p>Based on the random-effects model pool (REML):</p>")
            
            if (p < 0.05) {
                direction <- if (te > 0) "positive" else "negative"
                interp <- paste0(interp, "<ul><li>There is a <b>statistically significant ", direction, " effect</b> (p = ", round(p, 4), ").</li>")
            } else {
                interp <- paste0(interp, "<ul><li>The pooled effect is <b>not statistically significant</b> (p = ", round(p, 4), ").</li>")
            }
            
            # Heterogeneity Synthesis
            i2 <- res$I2
            interp <- paste0(interp, "<li>Heterogeneity is <b>", private$.getI2Interpretation(i2), "</b> (I² = ", round(i2*100, 1), "%).")
            if (i2 > 0.5) {
                interp <- paste0(interp, " High heterogeneity suggests that the effect varies considerably across settings.</li>")
            } else {
                interp <- paste0(interp, " Low heterogeneity suggests consistent results across studies.</li>")
            }
            interp <- paste0(interp, "</ul>")
            
            self$results$clinical_interpretation$setContent(interp)
            
            # Methods section
            methods <- "<h3>Suggested Methods Text</h3>"
            methods <- paste0(methods, "<p>We performed a meta-analysis using the 'meta' package. ")
            methods <- paste0(methods, "Pooled effect sizes were calculated using a <b>", 
                             ifelse(res$common, "fixed-effect", "random-effects"), "</b> model with the <b>", 
                             res$method.tau, "</b> estimator for tau-squared. ")
            methods <- paste0(methods, "Heterogeneity was assessed using Cochran's Q and the I² statistic. ")
            if (self$options$publication_bias) {
                methods <- paste0(methods, "Publication bias was evaluated using Egger's regression and Begg's rank correlation tests. ")
            }
            methods <- paste0(methods, "</p>")
            
            self$results$methods_section$setContent(methods)
        },

        .populatePCurve = function(res) {
            if (!self$options$pcurve) return()
            
            # P-curve analysis looks at the distribution of p-values < 0.05
            p_vals <- res$pval
            sig_p <- p_vals[p_vals < 0.05 & !is.na(p_vals)]
            
            table <- self$results$pcurve_results
            
            if (length(sig_p) < 3) {
                table$setNote("pcurve_note", "P-curve analysis requires at least 3 significant studies (p < 0.05).")
                return()
            }
            
            # Evidential value: More p-values in 0.00-0.025 than 0.025-0.05
            n_below_025 <- sum(sig_p < 0.025)
            n_above_025 <- sum(sig_p >= 0.025)
            
            # Binomial test
            b_test <- binom.test(n_below_025, length(sig_p), p = 0.5, alternative = "greater")
            
            table$addRow(rowKey = "evidential", values = list(
                test = "Test for Evidential Value (Right-skew)",
                statistic = paste0(n_below_025, " vs ", n_above_025),
                p_value = b_test$p.value,
                interpretation = if (b_test$p.value < 0.05) "Evidential value present" else "Inadequate evidence"
            ))
            
            # Test for p-hacking (Flatness/Left-skew)
            b_test_left <- binom.test(n_below_025, length(sig_p), p = 0.5, alternative = "less")
            table$addRow(rowKey = "hacking", values = list(
                test = "Test for P-hacking (Left-skew)",
                statistic = paste0(n_below_025, " vs ", n_above_025),
                p_value = b_test_left$p.value,
                interpretation = if (b_test_left$p.value < 0.05) "Possible p-hacking" else "No clear signal of p-hacking"
            ))
        },

        .pcurve_plot = function(image, ...) {
            if (is.null(private$.meta_res)) return(FALSE)
            if (!self$options$pcurve) return(FALSE)
            
            p_vals <- private$.meta_res$pval
            sig_p <- p_vals[p_vals < 0.05 & !is.na(p_vals)]
            
            if (length(sig_p) < 3) return(FALSE)
            
            df <- data.frame(p = sig_p)
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x = p)) +
                ggplot2::geom_histogram(breaks = seq(0, 0.05, by = 0.01), fill = "#4477AA", color = "white") +
                ggplot2::labs(title = "P-Curve: Distribution of Significant P-values",
                             x = "P-value",
                             y = "Frequency") +
                ggplot2::theme_minimal() +
                ggplot2::xlim(0, 0.05)
            
            print(p)
            TRUE
        },
        
        .forest_plot = function(image, ...) {
            if (is.null(private$.meta_res)) return(FALSE)
            meta::forest(private$.meta_res)
            TRUE
        },

        .funnel_plot = function(image, ...) {
            if (is.null(private$.meta_res)) return(FALSE)
            meta::funnel(private$.meta_res)
            TRUE
        },

        .baujat_plot = function(image, ...) {
            if (is.null(private$.meta_res)) return(FALSE)
            meta::baujat(private$.meta_res)
            TRUE
        },

        .radial_plot = function(image, ...) {
            if (is.null(private$.meta_res)) return(FALSE)
            meta::radial(private$.meta_res)
            TRUE
        },

        .cumulative_plot = function(image, ...) {
            if (is.null(private$.meta_res)) return(FALSE)
            res_cum <- meta::metacum(private$.meta_res)
            meta::forest(res_cum)
            TRUE
        },

        .populateBayesian = function(res) {
            if (!self$options$bayesian_analysis) return()
            if (!requireNamespace("metaBMA", quietly = TRUE)) {
                self$results$bayesian_results$setNote("bma_pkg", "Package 'metaBMA' is required for Bayesian analysis.")
                return()
            }
            
            try({
                # Perform Bayesian meta-analysis using metaBMA
                m_bma <- metaBMA::meta_bma(y = res$TE, SE = res$seTE, labels = res$studlab)
                
                table <- self$results$bayesian_results
                
                # Model Averaged Effect
                est <- m_bma$estimates
                if ("averaged" %in% rownames(est)) {
                    table$addRow(rowKey = "averaged", values = list(
                        parameter = "BMA Pooled Effect (Averaged)",
                        estimate = est["averaged", "mean"],
                        sd = est["averaged", "sd"],
                        hpd_lower = est["averaged", "2.5%"],
                        hpd_upper = est["averaged", "97.5%"]
                    ))
                }
                
                # Fixed Effect Model (if available)
                if ("fixed" %in% rownames(est)) {
                    table$addRow(rowKey = "fixed", values = list(
                        parameter = "Bayesian Fixed Effect",
                        estimate = est["fixed", "mean"],
                        sd = est["fixed", "sd"],
                        hpd_lower = est["fixed", "2.5%"],
                        hpd_upper = est["fixed", "97.5%"]
                    ))
                }
                
                # Random Effect Model (if available)
                if ("random" %in% rownames(est)) {
                    table$addRow(rowKey = "random", values = list(
                        parameter = "Bayesian Random Effect (d)",
                        estimate = est["random", "mean"],
                        sd = est["random", "sd"],
                        hpd_lower = est["random", "2.5%"],
                        hpd_upper = est["random", "97.5%"]
                    ))
                }
                
                # Heterogeneity (Tau)
                if (!is.null(m_bma$meta$random_H1)) {
                    tau_est <- m_bma$meta$random_H1$estimates
                    if ("tau" %in% rownames(tau_est)) {
                         table$addRow(rowKey = "tau", values = list(
                            parameter = "Between-study SD (Tau)",
                            estimate = tau_est["tau", "mean"],
                            sd = tau_est["tau", "sd"],
                            hpd_lower = tau_est["tau", "2.5%"],
                            hpd_upper = tau_est["tau", "97.5%"]
                        ))
                    }
                }
            })
        },

        .populateCumulativeSummary = function(res) {
            if (!self$options$cumulative_meta) return()
            
            try({
                # If year is provided, we should sort by it first? 
                # meta::metacum usually respects internal sorting of 'res'
                # If 'res' was created with 'data' where we sorted by year, it would be chronological.
                
                res_cum <- meta::metacum(res)
                table <- self$results$cumulative_results
                
                for (i in 1:res_cum$k) {
                    table$addRow(rowKey = i, values = list(
                        study = res_cum$studlab[i],
                        effect_size = res_cum$TE[i],
                        ci_lower = res_cum$lower[i],
                        ci_upper = res_cum$upper[i],
                        i_squared = res_cum$I2[i]
                    ))
                }
            })
        },

        .populateQualityAssessment = function(res) {
            if (is.null(self$options$quality_score)) return()
            
            table <- self$results$quality_assessment
            
            # Extract quality scores matched to studies in res
            # Need to match because some studies might have been excluded/reordered
            q_var <- self$options$quality_score
            d_subset <- self$data[self$data[[self$options$study_id]] %in% res$studlab, ]
            # Match by studlab
            q_scores <- d_subset[match(res$studlab, d_subset[[self$options$study_id]]), q_var]
            effects <- res$TE
            
            if (length(q_scores) == length(effects) && any(!is.na(q_scores))) {
                try({
                    cor_res <- cor.test(q_scores, effects, use="complete.obs")
                    table$addRow(rowKey = "cor", values = list(
                        metric = "Quality-Effect Correlation",
                        value = as.character(round(cor_res$estimate, 3)),
                        details = paste0("p-value: ", round(cor_res$p.value, 4), 
                                        ". Significant correlation may indicate quality-related bias.")
                    ))
                })
            }
            
            table$addRow(rowKey = "weighting", values = list(
                metric = "Quality Weighting",
                value = if (self$options$weight_by_quality) "Enabled" else "Disabled",
                details = "If enabled, studies with higher quality scores contribute more to the pooled estimate."
            ))
        },

        .getInterpretation = function(te, p) {
            if (is.na(p) || p > 0.05) return("No significant effect")
            if (te > 0) return("Significant positive effect")
            return("Significant negative effect")
        },

        .getI2Interpretation = function(i2) {
            if (is.na(i2)) return("N/A")
            if (i2 < 0.25) return("Low heterogeneity")
            if (i2 < 0.50) return("Moderate heterogeneity")
            if (i2 < 0.75) return("Substantial heterogeneity")
            return("High heterogeneity")
        }
    )
)
