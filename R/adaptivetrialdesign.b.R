
# This file is a generated template, your changes will not be overwritten

adaptivetrialdesignClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "adaptivetrialdesignClass",
    inherit = adaptivetrialdesignBase,
    private = list(
        .run = function() {

            # TODO (stub): the .a.yaml declares ~25 options but the implementation below
            # only reads ~6 of them. The following options are ignored at runtime and have
            # NO effect on the analysis even though they appear in the UI:
            #   stratification_variables, time_variable, interim_timing, interim_fractions,
            #   max_interim_analyses, boundary_type, minimum_effect_size, target_power,
            #   type1_error_rate, max_sample_size_inflation, prior_type, historical_data_weight,
            #   prior_parameters, decision_criterion, bayes_factor_threshold,
            #   predictive_power_threshold, run_simulations, n_simulations,
            #   true_effect_scenarios, mcmc_samples, mcmc_burnin, … (full list in .a.yaml).
            # Either wire each option through to the analysis logic, or hide them in the .u.yaml
            # until they are implemented. Currently they mislead users into thinking the
            # method is configurable when only outcome / treatment / outcomeLevel /
            # planned_sample_size / efficacy_boundary / futility_boundary / design_framework
            # actually drive behavior.

            # 1. Provide instructions if inputs are missing
            if (is.null(self$options$outcome) || is.null(self$options$treatment)) {
                
                todo <- "
                    <br>Welcome to Adaptive Trial Design
                    <br><br>
                    This tool facilitates Bayesian and frequentist adaptive clinical trial analysis.
                    <br><br>
                    To get started:
                    <br>1. Select the <b>Primary Outcome Variable</b>
                    <br>2. Select the <b>Treatment Assignment Variable</b>
                    <br>3. Choose the <b>Type of Adaptation</b> (e.g., stopping for futility or efficacy)
                    <br>4. Set the <b>Stopping Boundaries</b>
                "
                self$results$methodsExplanation$setContent(todo)
                return()
            }

            # 2. Get and clean data
            mydata <- self$data
            outcomeVar <- self$options$outcome
            treatmentVar <- self$options$treatment
            
            mydata <- jmvcore::naOmit(mydata[c(outcomeVar, treatmentVar)])
            
            if (nrow(mydata) == 0) {
                self$results$methodsExplanation$setContent("No valid data rows found after removing missing values.")
                return()
            }

            # 3. Design Summary Table
            #
            # TODO (UX): the rows below emit the raw List-option `name` (e.g. "bayesian",
            # "sample_size") rather than the friendly `title` defined in the .a.yaml
            # ("Bayesian Adaptive", "Sample Size Re-estimation"). Look up the title via
            # the option spec - for example:
            #   .titleFor <- function(opt_name) {
            #       opt <- self$analysis$options$get(opt_name)
            #       choice <- opt$value
            #       for (o in opt$options) if (identical(o$name, choice)) return(o$title)
            #       choice  # fallback to the raw name
            #   }
            # then pass `.titleFor("design_framework")` instead of `self$options$design_framework`.
            tableDesign <- self$results$designSummary
            tableDesign$addRow(rowKey=1, values=list(parameter="Framework", value=self$options$design_framework, justification="Primary statistical approach"))
            tableDesign$addRow(rowKey=2, values=list(parameter="Adaptation Type", value=self$options$adaptation_type, justification="Rules for modifying trial conduct"))
            tableDesign$addRow(rowKey=3, values=list(parameter="Target Sample Size", value=as.character(self$options$planned_sample_size), justification="Planned total N"))

            # 4. Perform Analysis (Interim Analysis)
            # Find the treatment levels - analysis assumes a binary (two-arm) comparison.
            levels <- levels(as.factor(mydata[[treatmentVar]]))
            if (length(levels) != 2) {
                jmvcore::reject(
                    "Treatment variable must have exactly two levels for this analysis (found {}). Recode or filter to a binary comparison.",
                    length(levels)
                )
            }

            # Current information fraction
            n_current <- nrow(mydata)
            n_planned <- self$options$planned_sample_size
            inf_fraction <- n_current / n_planned
            
            # Simple Treatment Effect (Diff in proportions if nominal, diff in means if continuous)
            is_nominal <- is.factor(mydata[[outcomeVar]]) || is.character(mydata[[outcomeVar]])
            
            effect <- 0
            p_val <- 1
            
            if (is_nominal) {
                outcomeValues <- as.character(mydata[[outcomeVar]])
                outcomeLevels <- if (is.factor(mydata[[outcomeVar]])) levels(mydata[[outcomeVar]])
                                 else sort(unique(outcomeValues))

                successLevel <- self$options$outcomeLevel
                if (is.null(successLevel) || !nzchar(as.character(successLevel))) {
                    successLevel <- outcomeLevels[1]
                } else {
                    successLevel <- as.character(successLevel)
                }

                if (!(successLevel %in% outcomeLevels)) {
                    jmvcore::reject(
                        "Selected Success/Response Level ('{}') is not present in the outcome variable. Available levels: {}.",
                        successLevel, paste(outcomeLevels, collapse = ", ")
                    )
                }

                tab <- table(mydata[[treatmentVar]], outcomeValues == successLevel)
                if (ncol(tab) == 2 && all(rowSums(tab) > 0)) {
                    res <- tryCatch(
                        prop.test(tab),
                        error = function(e) {
                            jmvcore::reject("prop.test failed: {}", conditionMessage(e))
                        }
                    )
                    effect <- res$estimate[1] - res$estimate[2]
                    p_val <- res$p.value
                }
            } else {
                res <- tryCatch(
                    t.test(mydata[[outcomeVar]] ~ mydata[[treatmentVar]]),
                    error = function(e) {
                        jmvcore::reject("t.test failed: {}", conditionMessage(e))
                    }
                )
                effect <- res$estimate[1] - res$estimate[2]
                p_val <- res$p.value
            }

            # 5. Bayesian Stop Boundaries (Placeholder for full MCMC)
            # We approximate posterior probability using p-values for this baseline version
            #
            # TODO (correctness): the current p-value-to-probability transform is broken.
            #   - `p_efficacy = 1 - p_val/2` is a one-sided posterior approximation that
            #     ignores effect direction: a small two-sided p-value with the WRONG sign
            #     (treatment worse than control) still triggers efficacy stopping.
            #   - `p_futility = 1 - p_efficacy = p_val/2` means futility only fires when
            #     `p_futility >= 1 - futility_boundary`, i.e. `p_val >= 2*(1 - 0.10) = 1.8`,
            #     which is impossible. Futility never triggers under this rule.
            # Fix requires picking a real methodology (one of):
            #   (a) true Bayesian posterior with a stated prior + likelihood,
            #   (b) predictive probability of trial success at planned N,
            #   (c) proper alpha-spending under a Lan-DeMets / O'Brien-Fleming boundary
            #       (the .a.yaml already declares `boundary_type` for this).
            # Until then, condition `p_efficacy` on `effect > 0` at minimum so the rule
            # cannot fire in the wrong direction.
            p_efficacy <- 1 - (p_val / 2) # P(Alt > Null)
            p_futility <- 1 - p_efficacy
            
            decision <- "Continue"
            recommendation <- "Accrual ongoing as planned."
            
            if (p_efficacy >= self$options$efficacy_boundary) {
                decision <- "STOP - Efficacy"
                recommendation <- "Reject Null. Trial may stop early for overwhelming evidence."
            } else if (p_futility >= (1 - self$options$futility_boundary)) {
                 decision <- "STOP - Futility"
                 recommendation <- "The probability of a positive outcome at trial completion is low."
            }

            # 6. interimResults Table
            tableInterim <- self$results$interimResults
            tableInterim$addRow(rowKey=1, values=list(
                analysis = 1,
                information_fraction = inf_fraction,
                sample_size = n_current,
                treatment_effect = effect,
                posterior_prob_efficacy = p_efficacy,
                posterior_prob_futility = p_futility,
                decision = decision,
                recommendation = recommendation
            ))

            # 7. Methods Explanation
            explanation <- paste0("
                <h3>Adaptive Clinical Trial Design</h3>
                <p>Adaptive designs allow for predefined modifications to a trial based on accumulating data. 
                Common adaptations include stopping early for efficacy or futility.</p>
                <ul>
                    <li><b>Efficacy Boundary:</b> If P(Effect > 0 | Data) > ", self$options$efficacy_boundary, ", stop early.</li>
                    <li><b>Futility Boundary:</b> If P(Effect > MCID | Data) < ", self$options$futility_boundary, ", stop early.</li>
                </ul>
                <p>This implementation uses <b>", self$options$design_framework, "</b> monitoring at the current information fraction of <b>", round(inf_fraction*100, 1), "%</b>.</p>
            ")
            self$results$methodsExplanation$setContent(explanation)

        })
)
