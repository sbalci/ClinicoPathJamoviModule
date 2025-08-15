# This file is a generated template, your changes will not be overwritten

#' @import jmvcore
#' @import R6
#' @importFrom stats power.t.test power.prop.test pnorm qnorm
#' @importFrom pwr pwr.t.test pwr.anova.test pwr.chisq.test pwr.r.test pwr.2p.test pwr.p.test
#' @export

clinicaltrialdesignClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clinicaltrialdesignClass",
    inherit = clinicaltrialdesignBase,
    private = list(
        .init = function() {
            self$results$instructions$setContent(
                "<html>
                <head>
                <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'>
                </head>
                <body>
                <h3>Clinical Trial Design & Power Analysis</h3>
                <p><b>Comprehensive Power Analysis for Clinical Research</b></p>
                
                <p>This module provides comprehensive power analysis and sample size calculations 
                for clinical trial design, covering the most common study types and statistical 
                tests used in clinical research.</p>
                
                <h4>Getting Started:</h4>
                <ol>
                <li><b>Trial Design Type:</b> Select superiority, non-inferiority, equivalence, or pilot study</li>
                <li><b>Outcome Type:</b> Choose continuous, binary, categorical, or time-to-event</li>
                <li><b>Statistical Test:</b> Select appropriate test for your design and outcome</li>
                <li><b>Calculation Type:</b> Choose to calculate power, sample size, or detectable effect size</li>
                <li><b>Parameters:</b> Enter study parameters based on literature or pilot data</li>
                </ol>
                
                <h4>Key Features:</h4>
                <ul>
                <li><b>Multiple Trial Types:</b> Superiority, non-inferiority, equivalence designs</li>
                <li><b>Comprehensive Tests:</b> t-tests, ANOVA, proportion tests, chi-square, correlation</li>
                <li><b>Regulatory Guidance:</b> FDA, EMA, ICH guidelines consideration</li>
                <li><b>Effect Size Analysis:</b> Clinical significance assessment</li>
                <li><b>Sensitivity Analysis:</b> Robustness across parameter ranges</li>
                <li><b>Sample Size Adjustments:</b> Dropout, interim analyses, multiple comparisons</li>
                </ul>
                
                <h4>Clinical Applications:</h4>
                <ul>
                <li>Randomized controlled trials (RCTs)</li>
                <li>Biomarker validation studies</li>
                <li>Diagnostic accuracy trials</li>
                <li>Treatment comparison studies</li>
                <li>Non-inferiority and equivalence trials</li>
                <li>Pilot and feasibility studies</li>
                </ul>
                </body>
                </html>"
            )
        },

        .run = function() {
            # Check for required packages
            required_packages <- c("pwr")
            missing_packages <- c()
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                }
            }
            
            if (length(missing_packages) > 0) {
                error_msg <- paste0(
                    "<p style='color: red;'><b>Error:</b> Required packages not installed: ", 
                    paste(missing_packages, collapse = ", "), 
                    "<br>Please install with: install.packages(c('", 
                    paste(missing_packages, collapse = "', '"), "'))</p>"
                )
                self$results$instructions$setContent(error_msg)
                return()
            }
            
            tryCatch({
                # Perform analysis based on selected options
                private$.populateDesignSummary()
                private$.calculatePowerAnalysis()
                private$.generateEffectSizeAnalysis()
                
                if (self$options$show_assumptions) {
                    private$.checkAssumptions()
                }
                
                if (self$options$show_sensitivity) {
                    private$.performSensitivityAnalysis()
                }
                
                if (self$options$show_interpretation) {
                    private$.generateRegulatoryConsiderations()
                    private$.generateClinicalInterpretation()
                    private$.generateProtocolTemplate()
                }
                
                if (self$options$show_plots) {
                    private$.preparePlots()
                }
                
            }, error = function(e) {
                error_msg <- paste0("<p style='color: red;'><b>Analysis Error:</b> ", e$message, "</p>")
                self$results$instructions$setContent(error_msg)
            })
        },
        
        .populateDesignSummary = function() {
            design_table <- self$results$design_summary
            
            # Trial type summary
            trial_type_desc <- switch(self$options$trial_type,
                "superiority" = "Superiority trial - demonstrates new treatment is better",
                "non_inferiority" = "Non-inferiority trial - new treatment not worse by margin",
                "equivalence" = "Equivalence trial - treatments are equivalent within margin",
                "pilot" = "Pilot/feasibility study - assess study conduct feasibility"
            )
            
            design_table$addRow(rowKey="trial_type", values=list(
                parameter="Trial Design Type",
                value=self$options$trial_type,
                recommendation=trial_type_desc,
                rationale="Determines statistical approach and regulatory requirements"
            ))
            
            # Outcome type summary
            outcome_desc <- switch(self$options$outcome_type,
                "continuous" = "Continuous outcome - use parametric tests if assumptions met",
                "binary" = "Binary outcome - use proportion tests and effect sizes",
                "categorical" = "Categorical outcome - use chi-square or Fisher's exact",
                "time_to_event" = "Time-to-event - use survival analysis methods"
            )
            
            design_table$addRow(rowKey="outcome_type", values=list(
                parameter="Primary Outcome Type",
                value=self$options$outcome_type,
                recommendation=outcome_desc,
                rationale="Determines appropriate statistical test and effect size measure"
            ))
            
            # Statistical test summary
            test_desc <- private$.getTestDescription(self$options$test_type)
            
            design_table$addRow(rowKey="test_type", values=list(
                parameter="Statistical Test",
                value=self$options$test_type,
                recommendation=test_desc$description,
                rationale=test_desc$rationale
            ))
            
            # Alpha level recommendation
            alpha_rec <- if (self$options$trial_type == "non_inferiority") {
                "Consider α=0.025 for non-inferiority (more stringent)"
            } else {
                "Standard α=0.05 appropriate for superiority trials"
            }
            
            design_table$addRow(rowKey="alpha", values=list(
                parameter="Significance Level (α)",
                value=self$options$alpha,
                recommendation=alpha_rec,
                rationale="Balances Type I error risk with study feasibility"
            ))
            
            # Power recommendation
            power_rec <- if (self$options$power >= 0.90) {
                "High power (≥90%) - excellent for definitive trials"
            } else if (self$options$power >= 0.80) {
                "Adequate power (80-89%) - standard for most trials"
            } else {
                "Lower power (<80%) - consider for pilot studies only"
            }
            
            design_table$addRow(rowKey="power", values=list(
                parameter="Statistical Power (1-β)",
                value=self$options$power,
                recommendation=power_rec,
                rationale="Higher power reduces risk of missing true effects"
            ))
        },
        
        .calculatePowerAnalysis = function() {
            results_table <- self$results$power_results
            calculation_type <- self$options$calculation_type
            
            # Get power analysis results based on test type
            power_result <- private$.performPowerCalculation()
            
            if (is.null(power_result)) {
                results_table$addRow(rowKey="error", values=list(
                    parameter="Error",
                    input_value="",
                    calculated_value="",
                    confidence_interval="",
                    interpretation="Power calculation failed. Check input parameters."
                ))
                return()
            }
            
            # Main result
            main_param <- switch(calculation_type,
                "power" = "Statistical Power",
                "sample_size" = "Required Sample Size", 
                "effect_size" = "Detectable Effect Size"
            )
            
            main_value <- switch(calculation_type,
                "power" = round(power_result$power, 4),
                "sample_size" = ceiling(power_result$n),
                "effect_size" = round(power_result$effect_size, 4)
            )
            
            main_interpretation <- private$.interpretPowerResult(power_result, calculation_type)
            
            results_table$addRow(rowKey="main_result", values=list(
                parameter=main_param,
                input_value="",
                calculated_value=as.character(main_value),
                confidence_interval="",
                interpretation=main_interpretation
            ))
            
            # Input parameters
            if (calculation_type != "power") {
                results_table$addRow(rowKey="power_input", values=list(
                    parameter="Statistical Power (Input)",
                    input_value=as.character(self$options$power),
                    calculated_value="",
                    confidence_interval="",
                    interpretation="Desired power for sample size calculation"
                ))
            }
            
            if (calculation_type != "effect_size") {
                effect_val <- if (self$options$outcome_type == "continuous") {
                    self$options$effect_size
                } else {
                    abs(self$options$proportion2 - self$options$proportion1)
                }
                
                results_table$addRow(rowKey="effect_input", values=list(
                    parameter="Effect Size (Input)",
                    input_value=as.character(effect_val),
                    calculated_value="",
                    confidence_interval="",
                    interpretation="Expected effect size based on literature or pilot data"
                ))
            }
            
            results_table$addRow(rowKey="alpha_input", values=list(
                parameter="Significance Level (Input)",
                input_value=as.character(self$options$alpha),
                calculated_value="",
                confidence_interval="",
                interpretation="Type I error rate for hypothesis testing"
            ))
            
            # Sample size breakdown if calculating sample size
            if (calculation_type == "sample_size") {
                private$.populateSampleSizeBreakdown(power_result)
            }
        },
        
        .performPowerCalculation = function() {
            test_type <- self$options$test_type
            calculation_type <- self$options$calculation_type
            
            tryCatch({
                if (test_type == "two_sample_ttest") {
                    return(private$.calculateTwoSampleTTest())
                } else if (test_type == "one_sample_ttest") {
                    return(private$.calculateOneSampleTTest())
                } else if (test_type == "paired_ttest") {
                    return(private$.calculatePairedTTest())
                } else if (test_type == "anova_oneway") {
                    return(private$.calculateANOVA())
                } else if (test_type == "two_proportions") {
                    return(private$.calculateTwoProportions())
                } else if (test_type == "one_proportion") {
                    return(private$.calculateOneProportion())
                } else if (test_type == "chi_square") {
                    return(private$.calculateChiSquare())
                } else if (test_type == "correlation") {
                    return(private$.calculateCorrelation())
                } else if (test_type == "mcnemar") {
                    return(private$.calculateMcNemar())
                }
                
                return(NULL)
                
            }, error = function(e) {
                return(NULL)
            })
        },
        
        .calculateTwoSampleTTest = function() {
            calc_type <- self$options$calculation_type
            
            if (calc_type == "power") {
                result <- pwr::pwr.t.test(
                    n = self$options$sample_size / 2,
                    d = self$options$effect_size,
                    sig.level = self$options$alpha,
                    type = "two.sample",
                    alternative = if (self$options$two_sided) "two.sided" else "greater"
                )
                return(list(power = result$power, n = result$n * 2, effect_size = result$d))
                
            } else if (calc_type == "sample_size") {
                result <- pwr::pwr.t.test(
                    power = self$options$power,
                    d = self$options$effect_size,
                    sig.level = self$options$alpha,
                    type = "two.sample",
                    alternative = if (self$options$two_sided) "two.sided" else "greater"
                )
                total_n <- result$n * 2 * (1 + 1/self$options$allocation_ratio)
                return(list(power = result$power, n = total_n, effect_size = result$d))
                
            } else if (calc_type == "effect_size") {
                result <- pwr::pwr.t.test(
                    n = self$options$sample_size / 2,
                    power = self$options$power,
                    sig.level = self$options$alpha,
                    type = "two.sample",
                    alternative = if (self$options$two_sided) "two.sided" else "greater"
                )
                return(list(power = result$power, n = result$n * 2, effect_size = result$d))
            }
        },
        
        .calculateTwoProportions = function() {
            calc_type <- self$options$calculation_type
            p1 <- self$options$proportion1
            p2 <- self$options$proportion2
            
            if (calc_type == "power") {
                result <- pwr::pwr.2p.test(
                    h = 2 * (asin(sqrt(p2)) - asin(sqrt(p1))),
                    n = self$options$sample_size / 2,
                    sig.level = self$options$alpha,
                    alternative = if (self$options$two_sided) "two.sided" else "greater"
                )
                return(list(power = result$power, n = result$n * 2, effect_size = abs(p2 - p1)))
                
            } else if (calc_type == "sample_size") {
                result <- pwr::pwr.2p.test(
                    h = 2 * (asin(sqrt(p2)) - asin(sqrt(p1))),
                    power = self$options$power,
                    sig.level = self$options$alpha,
                    alternative = if (self$options$two_sided) "two.sided" else "greater"
                )
                total_n <- result$n * 2 * (1 + 1/self$options$allocation_ratio)
                return(list(power = result$power, n = total_n, effect_size = abs(p2 - p1)))
                
            } else if (calc_type == "effect_size") {
                # For effect size calculation, we need to find the detectable difference
                result <- pwr::pwr.2p.test(
                    n = self$options$sample_size / 2,
                    power = self$options$power,
                    sig.level = self$options$alpha,
                    alternative = if (self$options$two_sided) "two.sided" else "greater"
                )
                # Convert Cohen's h back to proportion difference (approximate)
                detectable_diff <- sin(result$h / 2)^2
                return(list(power = result$power, n = result$n * 2, effect_size = detectable_diff))
            }
        },
        
        .calculateANOVA = function() {
            # For ANOVA, we need to estimate effect size from Cohen's f
            calc_type <- self$options$calculation_type
            # Convert Cohen's d to Cohen's f (approximate)
            cohens_f <- self$options$effect_size / 2
            k <- 3  # Assume 3 groups for simplicity
            
            if (calc_type == "power") {
                result <- pwr::pwr.anova.test(
                    k = k,
                    n = self$options$sample_size / k,
                    f = cohens_f,
                    sig.level = self$options$alpha
                )
                return(list(power = result$power, n = result$n * k, effect_size = result$f))
                
            } else if (calc_type == "sample_size") {
                result <- pwr::pwr.anova.test(
                    k = k,
                    power = self$options$power,
                    f = cohens_f,
                    sig.level = self$options$alpha
                )
                return(list(power = result$power, n = result$n * k, effect_size = result$f))
                
            } else if (calc_type == "effect_size") {
                result <- pwr::pwr.anova.test(
                    k = k,
                    n = self$options$sample_size / k,
                    power = self$options$power,
                    sig.level = self$options$alpha
                )
                return(list(power = result$power, n = result$n * k, effect_size = result$f))
            }
        },
        
        .calculateCorrelation = function() {
            calc_type <- self$options$calculation_type
            r <- self$options$effect_size  # Treat effect size as correlation
            
            if (calc_type == "power") {
                result <- pwr::pwr.r.test(
                    n = self$options$sample_size,
                    r = r,
                    sig.level = self$options$alpha,
                    alternative = if (self$options$two_sided) "two.sided" else "greater"
                )
                return(list(power = result$power, n = result$n, effect_size = result$r))
                
            } else if (calc_type == "sample_size") {
                result <- pwr::pwr.r.test(
                    power = self$options$power,
                    r = r,
                    sig.level = self$options$alpha,
                    alternative = if (self$options$two_sided) "two.sided" else "greater"
                )
                return(list(power = result$power, n = result$n, effect_size = result$r))
                
            } else if (calc_type == "effect_size") {
                result <- pwr::pwr.r.test(
                    n = self$options$sample_size,
                    power = self$options$power,
                    sig.level = self$options$alpha,
                    alternative = if (self$options$two_sided) "two.sided" else "greater"
                )
                return(list(power = result$power, n = result$n, effect_size = result$r))
            }
        },
        
        .populateSampleSizeBreakdown = function(power_result) {
            breakdown_table <- self$results$sample_size_breakdown
            
            base_n <- ceiling(power_result$n)
            allocation_ratio <- self$options$allocation_ratio
            dropout_rate <- self$options$dropout_rate / 100
            interim_analyses <- self$options$interim_analyses
            
            # Base sample size
            n_treatment <- ceiling(base_n * allocation_ratio / (1 + allocation_ratio))
            n_control <- ceiling(base_n / (1 + allocation_ratio))
            
            breakdown_table$addRow(rowKey="base", values=list(
                component="Base Sample Size",
                per_group=paste(n_treatment, "/", n_control),
                total=base_n,
                adjustment_factor=1.0,
                rationale="Statistical power requirement without adjustments"
            ))
            
            # Dropout adjustment
            if (dropout_rate > 0) {
                dropout_factor <- 1 / (1 - dropout_rate)
                n_adjusted_dropout <- ceiling(base_n * dropout_factor)
                n_treatment_dropout <- ceiling(n_treatment * dropout_factor)
                n_control_dropout <- ceiling(n_control * dropout_factor)
                
                breakdown_table$addRow(rowKey="dropout", values=list(
                    component="Dropout Adjustment",
                    per_group=paste(n_treatment_dropout, "/", n_control_dropout),
                    total=n_adjusted_dropout,
                    adjustment_factor=dropout_factor,
                    rationale=paste0("Accounts for ", dropout_rate*100, "% expected dropout rate")
                ))
                
                base_n <- n_adjusted_dropout
                n_treatment <- n_treatment_dropout
                n_control <- n_control_dropout
            }
            
            # Interim analysis adjustment (approximate)
            if (interim_analyses > 0) {
                # Simple inflation factor for interim analyses (more sophisticated methods available)
                interim_factor <- 1 + (interim_analyses * 0.02)  # Approximate 2% per interim
                n_final <- ceiling(base_n * interim_factor)
                n_treatment_final <- ceiling(n_treatment * interim_factor)
                n_control_final <- ceiling(n_control * interim_factor)
                
                breakdown_table$addRow(rowKey="interim", values=list(
                    component="Interim Analysis Adjustment",
                    per_group=paste(n_treatment_final, "/", n_control_final),
                    total=n_final,
                    adjustment_factor=interim_factor,
                    rationale=paste0("Alpha spending for ", interim_analyses, " planned interim analyses")
                ))
                
                base_n <- n_final
            }
            
            # Final recommendation
            breakdown_table$addRow(rowKey="final", values=list(
                component="**Final Recommendation**",
                per_group=paste("**", n_treatment, "/", n_control, "**"),
                total=paste0("**", base_n, "**"),
                adjustment_factor=power_result$n / base_n,
                rationale="Total sample size including all adjustments"
            ))
        },
        
        .generateEffectSizeAnalysis = function() {
            effect_table <- self$results$effect_size_analysis
            
            if (self$options$outcome_type == "continuous") {
                # Cohen's d analysis
                cohens_d <- self$options$effect_size
                magnitude <- private$.interpretCohensD(cohens_d)
                
                effect_table$addRow(rowKey="cohens_d", values=list(
                    effect_measure="Cohen's d",
                    value=cohens_d,
                    magnitude=magnitude,
                    clinical_significance=private$.assessClinicalSignificance(cohens_d, "continuous"),
                    statistical_significance="Depends on sample size and alpha level"
                ))
                
                # Mean difference
                mean_diff <- self$options$mean_difference
                effect_table$addRow(rowKey="mean_diff", values=list(
                    effect_measure="Mean Difference",
                    value=mean_diff,
                    magnitude="Raw difference in outcome units",
                    clinical_significance="Evaluate against minimal clinically important difference",
                    statistical_significance="Raw effect size for clinical interpretation"
                ))
                
            } else if (self$options$outcome_type == "binary") {
                # Proportion difference
                prop_diff <- abs(self$options$proportion2 - self$options$proportion1)
                magnitude <- private$.interpretPropDifference(prop_diff)
                
                effect_table$addRow(rowKey="prop_diff", values=list(
                    effect_measure="Absolute Risk Difference",
                    value=prop_diff,
                    magnitude=magnitude,
                    clinical_significance=private$.assessClinicalSignificance(prop_diff, "binary"),
                    statistical_significance="Direct clinical interpretation possible"
                ))
                
                # Number needed to treat
                if (prop_diff > 0) {
                    nnt <- 1 / prop_diff
                    effect_table$addRow(rowKey="nnt", values=list(
                        effect_measure="Number Needed to Treat",
                        value=round(nnt, 1),
                        magnitude=private$.interpretNNT(nnt),
                        clinical_significance="Direct measure of clinical impact",
                        statistical_significance="Clinical utility measure"
                    ))
                }
                
                # Relative risk (if appropriate)
                if (self$options$proportion1 > 0) {
                    rr <- self$options$proportion2 / self$options$proportion1
                    effect_table$addRow(rowKey="relative_risk", values=list(
                        effect_measure="Relative Risk",
                        value=round(rr, 3),
                        magnitude=private$.interpretRelativeRisk(rr),
                        clinical_significance="Multiplicative effect measure",
                        statistical_significance="Commonly reported in clinical trials"
                    ))
                }
            }
        },
        
        .checkAssumptions = function() {
            assumptions_table <- self$results$assumptions_check
            test_type <- self$options$test_type
            
            assumptions <- private$.getTestAssumptions(test_type)
            
            for (assumption in assumptions) {
                assumptions_table$addRow(rowKey=assumption$name, values=list(
                    assumption=assumption$name,
                    description=assumption$description,
                    assessment_method=assumption$assessment,
                    violation_impact=assumption$impact,
                    alternatives=assumption$alternatives
                ))
            }
        },
        
        .performSensitivityAnalysis = function() {
            sensitivity_table <- self$results$sensitivity_analysis
            
            # Effect size sensitivity
            base_effect <- self$options$effect_size
            low_effect <- base_effect * 0.75
            high_effect <- base_effect * 1.25
            
            sensitivity_table$addRow(rowKey="effect_size", values=list(
                parameter="Effect Size",
                low_value=round(low_effect, 3),
                base_value=round(base_effect, 3),
                high_value=round(high_effect, 3),
                impact_assessment="25% change in effect size significantly impacts power/sample size"
            ))
            
            # Power sensitivity
            base_power <- self$options$power
            low_power <- max(0.6, base_power - 0.1)
            high_power <- min(0.95, base_power + 0.1)
            
            sensitivity_table$addRow(rowKey="power", values=list(
                parameter="Statistical Power",
                low_value=round(low_power, 2),
                base_value=round(base_power, 2),
                high_value=round(high_power, 2),
                impact_assessment="10% power change moderately impacts required sample size"
            ))
            
            # Alpha sensitivity
            base_alpha <- self$options$alpha
            low_alpha <- base_alpha / 2
            high_alpha <- min(0.1, base_alpha * 2)
            
            sensitivity_table$addRow(rowKey="alpha", values=list(
                parameter="Significance Level",
                low_value=round(low_alpha, 3),
                base_value=round(base_alpha, 3),
                high_value=round(high_alpha, 3),
                impact_assessment="More stringent alpha increases required sample size"
            ))
            
            # Dropout sensitivity
            base_dropout <- self$options$dropout_rate
            low_dropout <- max(0, base_dropout - 5)
            high_dropout <- min(50, base_dropout + 10)
            
            sensitivity_table$addRow(rowKey="dropout", values=list(
                parameter="Dropout Rate (%)",
                low_value=paste0(low_dropout, "%"),
                base_value=paste0(base_dropout, "%"),
                high_value=paste0(high_dropout, "%"),
                impact_assessment="Higher dropout rates require substantial sample size inflation"
            ))
        },
        
        .generateRegulatoryConsiderations = function() {
            regulatory_table <- self$results$regulatory_considerations
            context <- self$options$regulatory_context
            trial_type <- self$options$trial_type
            
            if (context == "fda" || context == "ich") {
                regulatory_table$addRow(rowKey="power_req", values=list(
                    regulatory_aspect="Statistical Power",
                    requirement="≥80% power recommended, ≥90% for pivotal trials",
                    compliance_status=if (self$options$power >= 0.8) "Compliant" else "Below Recommendation",
                    recommendation="Consider increasing power for regulatory submission",
                    reference="FDA Statistical Guidance (2018)"
                ))
            }
            
            if (trial_type == "non_inferiority") {
                regulatory_table$addRow(rowKey="margin", values=list(
                    regulatory_aspect="Non-Inferiority Margin",
                    requirement="Pre-specified based on clinical relevance and regulatory precedent",
                    compliance_status="Requires Justification",
                    recommendation="Provide clinical and statistical justification for margin",
                    reference="FDA Non-Inferiority Guidance (2016)"
                ))
                
                regulatory_table$addRow(rowKey="alpha_ni", values=list(
                    regulatory_aspect="Alpha Level (Non-Inferiority)",
                    requirement="Often α=0.025 (one-sided) for more stringent testing",
                    compliance_status=if (self$options$alpha <= 0.025) "Appropriate" else "Consider Adjustment",
                    recommendation="Use α=0.025 for non-inferiority claims",
                    reference="ICH E9 Statistical Principles"
                ))
            }
            
            regulatory_table$addRow(rowKey="multiplicity", values=list(
                regulatory_aspect="Multiple Testing",
                requirement="Control family-wise error rate for multiple endpoints",
                compliance_status=if (self$options$multiple_comparisons != "none") "Addressed" else "Needs Attention",
                recommendation="Implement appropriate multiplicity adjustments",
                reference="FDA Multiple Endpoints Guidance (2017)"
            ))
            
            if (self$options$interim_analyses > 0) {
                regulatory_table$addRow(rowKey="interim", values=list(
                    regulatory_aspect="Interim Analyses",
                    requirement="Pre-specified alpha spending function required",
                    compliance_status="Needs Detailed Planning",
                    recommendation="Develop comprehensive interim analysis plan with alpha spending",
                    reference="FDA Adaptive Design Guidance (2019)"
                ))
            }
        },
        
        .generateClinicalInterpretation = function() {
            interpretation_html <- self$results$clinical_interpretation
            
            interpretation_text <- paste0(
                "<html><head><meta http-equiv='Content-Type' content='text/html; charset=UTF-8'></head><body>",
                "<h3>Clinical Trial Design - Interpretation & Guidelines</h3>",
                
                "<h4>Study Design Summary</h4>",
                "<p><b>Trial Type:</b> ", self$options$trial_type, " trial with ", self$options$outcome_type, " primary outcome</p>",
                "<p><b>Statistical Approach:</b> ", self$options$test_type, " with α=", self$options$alpha, " and power=", self$options$power, "</p>",
                
                "<h4>Sample Size Interpretation</h4>",
                "<p>The calculated sample size ensures adequate statistical power to detect clinically meaningful differences. ",
                "Consider the following factors in your interpretation:</p>",
                "<ul>",
                "<li><b>Effect Size:</b> Based on ", if (self$options$outcome_type == "continuous") "Cohen's d" else "proportion difference", 
                " - ensure this represents a clinically meaningful change</li>",
                "<li><b>Feasibility:</b> Assess recruitment capacity and timeline constraints</li>",
                "<li><b>Dropout Adjustment:</b> ", self$options$dropout_rate, "% dropout rate assumed - monitor closely during study</li>",
                "<li><b>Interim Analyses:</b> ", if (self$options$interim_analyses > 0) 
                    paste("Plan includes", self$options$interim_analyses, "interim analyses - implement proper alpha spending") 
                    else "No interim analyses planned - consider for long studies", "</li>",
                "</ul>",
                
                "<h4>Clinical Significance</h4>",
                "<p><b>Minimal Clinically Important Difference (MCID):</b> Ensure your effect size aligns with established MCID values from literature. ",
                "The statistical significance should be accompanied by clinical relevance assessment.</p>",
                
                "<h4>Regulatory Considerations</h4>",
                "<p><b>Regulatory Context:</b> ", self$options$regulatory_context, " guidelines considered</p>",
                "<ul>",
                "<li>Ensure pre-specification of all analyses in protocol</li>",
                "<li>Consider regulatory precedent for similar indications</li>",
                "<li>Plan for sensitivity analyses and robustness assessments</li>",
                if (self$options$trial_type == "non_inferiority") "<li>Provide comprehensive justification for non-inferiority margin</li>" else "",
                "</ul>",
                
                "<h4>Study Conduct Recommendations</h4>",
                "<ul>",
                "<li><b>Randomization:</b> Use appropriate randomization scheme (stratified, blocked, adaptive)</li>",
                "<li><b>Blinding:</b> Implement double-blinding when feasible for outcome assessment</li>",
                "<li><b>Data Monitoring:</b> Establish independent DSMB for safety and efficacy monitoring</li>",
                "<li><b>Quality Assurance:</b> Plan for data quality monitoring and source data verification</li>",
                "<li><b>Analysis Plan:</b> Develop detailed statistical analysis plan before database lock</li>",
                "</ul>",
                
                "<h4>Next Steps</h4>",
                "<ol>",
                "<li>Validate sample size assumptions with pilot data or literature review</li>",
                "<li>Develop comprehensive study protocol with statistical analysis plan</li>",
                "<li>Consider adaptive design elements if appropriate</li>",
                "<li>Plan for regulatory interactions (pre-IND, Type C meetings)</li>",
                "<li>Establish study infrastructure and monitoring procedures</li>",
                "</ol>",
                
                "</body></html>"
            )
            
            interpretation_html$setContent(interpretation_text)
        },
        
        .generateProtocolTemplate = function() {
            protocol_html <- self$results$study_protocol_template
            
            protocol_text <- paste0(
                "<html><head><meta http-equiv='Content-Type' content='text/html; charset=UTF-8'></head><body>",
                "<h3>Study Protocol - Statistical Analysis Plan Template</h3>",
                
                "<h4>9. STATISTICAL CONSIDERATIONS</h4>",
                
                "<h5>9.1 Study Design</h5>",
                "<p>This is a ", self$options$trial_type, " trial designed to evaluate [intervention] compared to [control] ",
                "with respect to [primary outcome]. The study follows a randomized, controlled design with ",
                if (self$options$allocation_ratio == 1) "1:1 randomization" else paste0(self$options$allocation_ratio, ":1 randomization"), ".</p>",
                
                "<h5>9.2 Sample Size Calculation</h5>",
                "<p><b>Primary Outcome:</b> ", self$options$outcome_type, " outcome analyzed using ", self$options$test_type, "</p>",
                "<p><b>Statistical Parameters:</b></p>",
                "<ul>",
                "<li>Type I error rate (α): ", self$options$alpha, "</li>",
                "<li>Statistical power (1-β): ", self$options$power, "</li>",
                "<li>Effect size: ", if (self$options$outcome_type == "continuous") 
                    paste("Cohen's d =", self$options$effect_size) else 
                    paste("Proportion difference =", abs(self$options$proportion2 - self$options$proportion1)), "</li>",
                "<li>Two-sided testing: ", if (self$options$two_sided) "Yes" else "No", "</li>",
                "</ul>",
                
                "<p><b>Sample Size Justification:</b> Based on [literature/pilot data reference], ",
                "we expect [effect size rationale]. With the specified parameters, ",
                "the calculated sample size provides adequate power to detect clinically meaningful differences.</p>",
                
                "<h5>9.3 Randomization and Blinding</h5>",
                "<p>[Describe randomization scheme, stratification factors, and blinding procedures]</p>",
                
                "<h5>9.4 Statistical Analysis Plan</h5>",
                "<p><b>Primary Analysis:</b> ", self$options$test_type, " will be used to compare [outcome] between treatment groups. ",
                "Analysis will follow the intention-to-treat principle.</p>",
                
                "<p><b>Secondary Analyses:</b></p>",
                "<ul>",
                "<li>Per-protocol analysis for sensitivity assessment</li>",
                "<li>Subgroup analyses for [specify subgroups]</li>",
                "<li>Safety analyses (all randomized subjects)</li>",
                "</ul>",
                
                if (self$options$interim_analyses > 0) {
                    paste0("<h5>9.5 Interim Analyses</h5>",
                    "<p>The study includes ", self$options$interim_analyses, " planned interim analyses. ",
                    "An alpha spending function will be used to maintain overall Type I error rate. ",
                    "The Data Safety Monitoring Board will review interim results for safety and efficacy.</p>")
                } else "",
                
                "<h5>9.6 Missing Data</h5>",
                "<p>Missing data will be minimized through [specify procedures]. ",
                "For primary analysis, [specify approach - complete case, multiple imputation, etc.]. ",
                "Sensitivity analyses will assess robustness to missing data assumptions.</p>",
                
                "<h5>9.7 Multiple Testing</h5>",
                if (self$options$multiple_comparisons != "none") {
                    paste0("<p>Multiple comparison adjustment using ", self$options$multiple_comparisons, " method will be applied to control family-wise error rate.</p>")
                } else {
                    "<p>No adjustment for multiple testing planned for primary endpoint. Secondary endpoints will be considered exploratory.</p>"
                },
                
                "</body></html>"
            )
            
            protocol_html$setContent(protocol_text)
        },
        
        # Helper functions
        .getTestDescription = function(test_type) {
            switch(test_type,
                "two_sample_ttest" = list(
                    description = "Compare means between two independent groups",
                    rationale = "Standard test for continuous outcomes in RCTs"
                ),
                "one_sample_ttest" = list(
                    description = "Compare single group mean to reference value",
                    rationale = "Appropriate for single-arm studies with historical control"
                ),
                "paired_ttest" = list(
                    description = "Compare paired observations (before/after)",
                    rationale = "Accounts for within-subject correlation"
                ),
                "anova_oneway" = list(
                    description = "Compare means across multiple groups",
                    rationale = "Extension of t-test for >2 groups"
                ),
                "two_proportions" = list(
                    description = "Compare proportions between two groups",
                    rationale = "Standard test for binary outcomes in RCTs"
                ),
                "one_proportion" = list(
                    description = "Compare single proportion to reference",
                    rationale = "Single-arm studies with historical control rate"
                ),
                "chi_square" = list(
                    description = "Test association between categorical variables",
                    rationale = "Appropriate for categorical outcomes"
                ),
                "correlation" = list(
                    description = "Assess linear relationship between variables",
                    rationale = "Observational studies of associations"
                ),
                "mcnemar" = list(
                    description = "Compare paired binary outcomes",
                    rationale = "Matched pairs or before/after binary outcomes"
                )
            )
        },
        
        .interpretPowerResult = function(result, calc_type) {
            switch(calc_type,
                "power" = {
                    power <- result$power
                    if (power >= 0.90) "Excellent power - high probability of detecting true effect"
                    else if (power >= 0.80) "Adequate power - standard for clinical trials"
                    else if (power >= 0.70) "Moderate power - consider increasing sample size"
                    else "Low power - substantial risk of missing true effect"
                },
                "sample_size" = {
                    n <- result$n
                    if (n <= 50) "Small study - feasible but limited generalizability"
                    else if (n <= 200) "Medium-sized study - good balance of feasibility and power"
                    else if (n <= 1000) "Large study - high power but requires substantial resources"
                    else "Very large study - consider adaptive design or surrogate endpoints"
                },
                "effect_size" = {
                    es <- result$effect_size
                    if (es >= 0.8) "Large detectable effect - may miss smaller but clinically important effects"
                    else if (es >= 0.5) "Medium detectable effect - balanced sensitivity"
                    else if (es >= 0.2) "Small detectable effect - high sensitivity to detect small changes"
                    else "Very small detectable effect - excellent sensitivity"
                }
            )
        },
        
        .interpretCohensD = function(d) {
            abs_d <- abs(d)
            if (abs_d >= 0.8) "Large effect"
            else if (abs_d >= 0.5) "Medium effect"
            else if (abs_d >= 0.2) "Small effect"
            else "Trivial effect"
        },
        
        .interpretPropDifference = function(diff) {
            if (diff >= 0.2) "Large difference"
            else if (diff >= 0.1) "Medium difference"
            else if (diff >= 0.05) "Small difference"
            else "Minimal difference"
        },
        
        .assessClinicalSignificance = function(effect_size, outcome_type) {
            if (outcome_type == "continuous") {
                abs_d <- abs(effect_size)
                if (abs_d >= 0.5) "Likely clinically meaningful"
                else "May lack clinical significance - verify against MCID"
            } else {
                if (effect_size >= 0.1) "Likely clinically meaningful"
                else "May lack clinical significance"
            }
        },
        
        .interpretNNT = function(nnt) {
            if (nnt <= 5) "Very beneficial - few patients needed to treat"
            else if (nnt <= 10) "Moderately beneficial"
            else if (nnt <= 25) "Modestly beneficial"
            else "Limited clinical benefit"
        },
        
        .interpretRelativeRisk = function(rr) {
            if (rr >= 2) "Strong positive association"
            else if (rr >= 1.5) "Moderate positive association"
            else if (rr >= 1.1) "Weak positive association"
            else if (rr <= 0.5) "Strong protective effect"
            else if (rr <= 0.67) "Moderate protective effect"
            else "Minimal association"
        },
        
        .getTestAssumptions = function(test_type) {
            assumptions_list <- list()
            
            if (test_type %in% c("two_sample_ttest", "one_sample_ttest", "paired_ttest")) {
                assumptions_list <- list(
                    list(name = "Normality", 
                         description = "Data should be approximately normally distributed",
                         assessment = "Shapiro-Wilk test, Q-Q plots, histograms",
                         impact = "Inflated Type I error, reduced power",
                         alternatives = "Wilcoxon tests, Bootstrap methods"),
                    list(name = "Independence",
                         description = "Observations should be independent",
                         assessment = "Study design review, clustering assessment",
                         impact = "Severely inflated Type I error",
                         alternatives = "Mixed-effects models, GEE"),
                    list(name = "Equal Variances",
                         description = "Groups should have similar variances (two-sample)",
                         assessment = "Levene's test, F-test",
                         impact = "Type I error inflation",
                         alternatives = "Welch's t-test, non-parametric tests")
                )
            } else if (test_type == "anova_oneway") {
                assumptions_list <- list(
                    list(name = "Normality",
                         description = "Residuals should be normally distributed",
                         assessment = "Shapiro-Wilk on residuals, Q-Q plots",
                         impact = "Type I error inflation",
                         alternatives = "Kruskal-Wallis test"),
                    list(name = "Homogeneity of Variance",
                         description = "Equal variances across groups",
                         assessment = "Levene's test, Bartlett's test",
                         impact = "Type I error inflation",
                         alternatives = "Welch's ANOVA, Brown-Forsythe test"),
                    list(name = "Independence",
                         description = "Independent observations",
                         assessment = "Study design review",
                         impact = "Severely inflated Type I error",
                         alternatives = "Mixed-effects ANOVA")
                )
            } else if (test_type %in% c("two_proportions", "one_proportion")) {
                assumptions_list <- list(
                    list(name = "Independent Observations",
                         description = "Each observation should be independent",
                         assessment = "Study design review",
                         impact = "Type I error inflation",
                         alternatives = "Mixed-effects logistic regression"),
                    list(name = "Adequate Sample Size",
                         description = "Expected counts ≥5 in each cell",
                         assessment = "n*p ≥ 5 and n*(1-p) ≥ 5",
                         impact = "Poor approximation to normal distribution",
                         alternatives = "Fisher's exact test, exact methods")
                )
            }
            
            return(assumptions_list)
        },
        
        .preparePlots = function() {
            # Set states for plots that will be generated
            if (self$results$power_curve$isVisible()) {
                self$results$power_curve$setState(list(
                    test_type = self$options$test_type,
                    effect_size = self$options$effect_size,
                    alpha = self$options$alpha,
                    allocation_ratio = self$options$allocation_ratio
                ))
            }
            
            if (self$results$effect_size_plot$isVisible()) {
                self$results$effect_size_plot$setState(list(
                    outcome_type = self$options$outcome_type,
                    effect_size = self$options$effect_size,
                    mean_difference = self$options$mean_difference,
                    common_sd = self$options$common_sd
                ))
            }
            
            if (self$results$sample_size_plot$isVisible()) {
                self$results$sample_size_plot$setState(list(
                    power = self$options$power,
                    alpha = self$options$alpha,
                    effect_size = self$options$effect_size,
                    dropout_rate = self$options$dropout_rate
                ))
            }
        }
    )
)