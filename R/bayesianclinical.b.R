
# This file is a generated template, your changes will not be overwritten

bayesianclinicalClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bayesianclinicalClass",
    inherit = bayesianclinicalBase,
    private = list(
        .run = function() {

            # 1. Provide instructions if inputs are missing
            if (is.null(self$options$outcome_var) || is.null(self$options$treatment_var)) {
                
                todo <- "
                    <br>Welcome to Bayesian Clinical Analysis
                    <br><br>
                    This tool provides Bayesian hypothesis testing and parameter estimation for clinical research.
                    <br><br>
                    To get started:
                    <br>1. Select the <b>Primary Outcome Variable</b>
                    <br>2. Select the <b>Treatment Variable</b> (grouping factor)
                    <br>3. (Optional) Select <b>Covariates</b>
                    <br>4. Choose the <b>Outcome Data Type</b> (Continuous, Binary, etc.)
                    <br><br>
                    Currently, Continuous outcomes are supported using BayesFactor analysis.
                "
                self$results$comprehensive_report$setContent(todo)
                return()
            }

            # 2. Get and clean data
            mydata <- self$data
            outcomeVar <- self$options$outcome_var
            treatmentVar <- self$options$treatment_var
            covs <- self$options$covariates
            
            allVars <- unique(c(outcomeVar, treatmentVar, covs))
            mydata <- jmvcore::naOmit(mydata[allVars])
            
            if (nrow(mydata) == 0) {
                self$results$comprehensive_report$setContent("No valid data rows found after removing missing values.")
                return()
            }

            # 3. Overview Table
            tableOverview <- self$results$bayesian_overview
            tableOverview$addRow(rowKey=1, values=list(characteristic="Total valid observations", value=as.character(nrow(mydata))))
            tableOverview$addRow(rowKey=2, values=list(characteristic="Analysis Type", value=self$options$analysis_type))
            tableOverview$addRow(rowKey=3, values=list(characteristic="Outcome Type", value=self$options$outcome_type))

            # 4. Perform Analysis
            if (self$options$outcome_type == "continuous") {
                
                # Check if treatment is factor
                mydata[[treatmentVar]] <- as.factor(mydata[[treatmentVar]])
                
                # Using BayesFactor package
                if (requireNamespace('BayesFactor', quietly=TRUE)) {
                    
                    formula <- as.formula(paste(jmvcore::composeTerm(outcomeVar), "~", jmvcore::composeTerm(treatmentVar)))
                    
                    # Compute Bayes Factor for treatment effect
                    bf <- try(BayesFactor::ttestBF(formula = formula, data = mydata), silent = TRUE)
                    
                    if (!inherits(bf, "try-error")) {
                        
                        # 5. Bayes Factor Table
                        tableBF <- self$results$bayes_factors
                        val_bf <- as.vector(bf)
                        
                        # Interpretation
                        strength <- "Anecdotal"
                        if (val_bf > 100) strength <- "Extreme"
                        else if (val_bf > 30) strength <- "Very Strong"
                        else if (val_bf > 10) strength <- "Strong"
                        else if (val_bf > 3) strength <- "Substantial"
                        
                        tableBF$addRow(rowKey=1, values=list(
                            hypothesis = paste("Effect of", treatmentVar),
                            bayes_factor = val_bf,
                            log_bf = log(val_bf),
                            evidence_strength = strength,
                            interpretation = if (val_bf > 1) "Evidence for Alternative" else "Evidence for Null"
                        ))
                        
                        # 6. Posterior Summary
                        # Sample from posterior
                        samples <- try(BayesFactor::posterior(bf, iterations = self$options$mcmc_iterations), silent = TRUE)
                        
                        if (!inherits(samples, "try-error")) {
                            tablePost <- self$results$posterior_summary
                            
                            # Extract column for treatment effect (usually named after the factor levels)
                            # ttestBF posterior has specific column names
                            s_df <- as.data.frame(samples)
                            
                            # Identify the effect column (delta or beta)
                            # For ttestBF, the effect is often in 'delta' or 'mu' relative columns
                            # Let's look for 'difference' or similar
                            
                            params <- colnames(s_df)
                            for (p in params) {
                                if (p == "g") next # Skip g parameter
                                
                                p_data <- s_df[[p]]
                                q <- quantile(p_data, probs = c(0.025, 0.975))
                                prob_pos <- mean(p_data > 0)
                                
                                tablePost$addRow(rowKey=p, values=list(
                                    parameter = p,
                                    mean = mean(p_data),
                                    sd = sd(p_data),
                                    ci_lower = q[1],
                                    ci_upper = q[2],
                                    prob_positive = prob_pos
                                ))
                            }
                        }
                    } else {
                        self$results$comprehensive_report$setContent(paste("BayesFactor Error:", attr(bf, "condition")$message))
                    }
                } else {
                    self$results$comprehensive_report$setContent("Package 'BayesFactor' is required but not installed.")
                }
            } else {
                self$results$comprehensive_report$setContent(paste("Outcome type", self$options$outcome_type, "is not yet implemented in this preview."))
            }

            # 7. Clinical Interpretation
            if (self$options$clinical_interpretation) {
                summary_text <- "
                    <h3>Clinical Interpretation Note</h3>
                    <p>Bayesian analysis focuses on the probability of hypotheses given the data. 
                    Unlike p-values, Bayes Factors quantify the relative evidence for the alternative hypothesis versus the null.</p>
                    <p>A Bayes Factor > 3 is often considered 'substantial' evidence. 
                    The posterior summary provides credible intervals (CrI), which have a direct probabilistic interpretation: 
                    there is a 95% probability that the true parameter lies within the interval, given the data and priors.</p>
                "
                self$results$clinical_interpretation_report$setContent(summary_text)
            }
        })
)
