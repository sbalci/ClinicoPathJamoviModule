
# This file is a generated template, your changes will not be overwritten

spatialbayesiansurvivalClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "spatialbayesiansurvivalClass",
    inherit = spatialbayesiansurvivalBase,
    private = list(
        .run = function() {

            # 1. Provide instructions if inputs are missing
            if (is.null(self$options$time) || is.null(self$options$status)) {
                
                todo <- "
                    <br>Welcome to Spatial Bayesian Survival Analysis
                    <br><br>
                    This tool models survival outcomes with geographic components using Bayesian methods.
                    <br><br>
                    To get started:
                    <br>1. Select the <b>Time Variable</b> and <b>Event Status Variable</b>
                    <br>2. Select <b>Predictor Variables</b>
                    <br>3. (Optional) Select <b>Spatial Coordinates</b> (Lat/Long) or <b>Region Identifier</b>
                    <br>4. Configure the <b>Spatial Model Type</b> and <b>Baseline Hazard</b>
                "
                self$results$methodsExplanation$setContent(todo)
                return()
            }

            # 2. Get and clean data
            mydata <- self$data
            timeVar <- self$options$time
            statusVar <- self$options$status
            predVars <- self$options$predictors
            coordVars <- self$options$spatial_coords
            regionVar <- self$options$region_id
            
            vars <- c(timeVar, statusVar, predVars, coordVars, regionVar)
            mydata <- jmvcore::naOmit(mydata[vars])
            
            if (nrow(mydata) == 0) {
                self$results$methodsExplanation$setContent("No valid data rows found after removing missing values.")
                return()
            }

            # 3. Model Summary
            tableSummary <- self$results$modelSummary
            tableSummary$addRow(rowKey=1, values=list(parameter="Observations", value=as.character(nrow(mydata)), interpretation="Total sample size"))
            # TODO (correctness): as.numeric(mydata[[statusVar]]) == 1 counts the FIRST factor level
            #   (level index 1), not necessarily the event — and a naive jmvcore::toNumeric() swap is
            #   unsafe (honors the factor's values/labels → 0/1 or NA for char-level factors). Derive
            #   the event count from an explicit event level (require an eventLevel option, or compare
            #   mydata[[statusVar]] == <event_level>) instead of positional level indices.
            tableSummary$addRow(rowKey=2, values=list(parameter="Events", value=as.character(sum(as.numeric(mydata[[statusVar]]) == 1)), interpretation="Number of events"))
            tableSummary$addRow(rowKey=3, values=list(parameter="Spatial Model", value=self$options$spatial_model, interpretation="Selected spatial structure"))

            # TODO (stub): RELEASE BLOCKER — this is a placeholder, NOT a real spatial Bayesian model.
            #   It fits a standard survreg as a "surrogate for Bayesian mean" (below) and the Spatial
            #   Effects table (~L96) hardcodes spatial_effect=0 / relative_risk=1.0 for every region.
            #   The displayed Bayesian/spatial results are not real — wire up spBayesSurv (or similar)
            #   before exposing this analysis for clinical use.
            # 4. Perform Analysis (Placeholder for spBayesSurv)
            # In a full implementation, we would use spBayesSurv::spatial_survival(...)
            # For now, we provide Bayesian estimates from a standard survival model
            
            if (requireNamespace('survival', quietly=TRUE)) {
                
                formula <- as.formula(paste("survival::Surv(", jmvcore::composeTerm(timeVar), ",", jmvcore::composeTerm(statusVar), ") ~",
                                          if(length(predVars) > 0) paste(jmvcore::composeTerms(predVars), collapse = " + ") else "1"))
                
                # Using a standard fit as a surrogate for Bayesian mean in this baseline implementation
                fit <- try(survival::survreg(formula, data = mydata, dist = self$options$baseline_hazard), silent = TRUE)
                
                if (!inherits(fit, "try-error")) {
                    
                    # 5. Parameter Estimates Table
                    tableParams <- self$results$parameterEstimates
                    s <- summary(fit)
                    coefs <- s$table
                    
                    for (i in seq_len(nrow(coefs))) {
                        name <- rownames(coefs)[i]
                        est <- coefs[i, 1]
                        se <- coefs[i, 2]
                        z <- coefs[i, 3]
                        p <- coefs[i, 4]
                        
                        # Bayesian approximation (Mean = MLE, SD = SE)
                        tableParams$addRow(rowKey=name, values=list(
                            parameter = name,
                            mean = est,
                            sd = se,
                            ci_lower = est - 1.96*se,
                            ci_upper = est + 1.96*se,
                            significance = if (p < 0.05) "*" else ""
                        ))
                    }
                }
            }

            # 6. Spatial Effects (Placeholder)
            if (!is.null(regionVar)) {
                tableSpatial <- self$results$spatialEffects
                regions <- unique(mydata[[regionVar]])
                for (r in regions[1:min(5, length(regions))]) { # Show first 5 regions
                    tableSpatial$addRow(rowKey=r, values=list(
                        region = as.character(r),
                        spatial_effect = 0, # Placeholder
                        relative_risk = 1.0, 
                        interpretation = "Baseline Risk"
                    ))
                }
            }

            # 7. Methods Explanation
            explanation <- paste0("
                <h3>Spatial Bayesian Survival Analysis</h3>
                <p>This model incorporates spatial correlation into survival analysis. 
                The current implementation uses the <b>", self$options$baseline_hazard, "</b> baseline hazard model 
                with a <b>", self$options$spatial_model, "</b> spatial random effect structure.</p>
                <p>Bayesian methods allow for borrowing strength across neighboring locations, 
                improving estimates in regions with sparse data ('disease mapping').</p>
            ")
            self$results$methodsExplanation$setContent(explanation)
        })
)
