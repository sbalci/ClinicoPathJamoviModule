#' Clinical Calculators Class
#' @name clinicalcalculatorsClass
#' @importFrom R6 R6Class
clinicalcalculatorsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(

    "clinicalcalculatorsClass",
    inherit = clinicalcalculatorsBase,
    private = list(
        .run = function() {

            # 1. Provide instructions if inputs are missing
            if (is.null(self$options$outcome_variable) || is.null(self$options$predictor_variables)) {
                
                todo <- "
                    <br>Welcome to Clinical Risk Calculators & Nomograms
                    <br><br>
                    This tool develops and validates clinical prediction models and risk calculators.
                    <br><br>
                    To get started:
                    <br>1. Select the <b>Outcome Variable</b> (binary for Logistic, time-to-event for Cox)
                    <br>2. Select the <b>Predictor Variables</b>
                    <br>3. Choose the <b>Statistical Model</b>
                    <br>4. (Optional) Choose <b>Validation Method</b> (e.g., Bootstrap)
                "
                self$results$instructions$setContent(todo)
                return()
            } else {
                self$results$instructions$setVisible(FALSE)
            }

            # 2. Get and clean data
            mydata <- self$data
            outcomeVar <- self$options$outcome_variable
            predictorVars <- self$options$predictor_variables
            
            # Handle missing data based on user selection
            if (self$options$missing_data_method == 'complete_case') {
                mydata <- jmvcore::naOmit(mydata[c(outcomeVar, predictorVars)])
            } else if (self$options$missing_data_method == 'mean_imputation') {
                # Simple mean imputation for numeric predictors
                for (var in predictorVars) {
                    if (is.numeric(mydata[[var]])) {
                        mydata[[var]][is.na(mydata[[var]])] <- mean(mydata[[var]], na.rm = TRUE)
                    }
                }
                # Use naOmit for remaining NAs (e.g. in outcome or factors where mean isn't applicable)
                mydata <- jmvcore::naOmit(mydata[c(outcomeVar, predictorVars)])
            } else if (self$options$missing_data_method == 'mode_imputation') {
                 # Mode imputation
                 get_mode <- function(v) {
                    uniqv <- unique(v)
                    uniqv[which.max(tabulate(match(v, uniqv)))]
                 }
                 for (var in predictorVars) {
                    if (any(is.na(mydata[[var]]))) {
                        mode_val <- get_mode(mydata[[var]][!is.na(mydata[[var]])])
                        mydata[[var]][is.na(mydata[[var]])] <- mode_val
                    }
                 }
                 mydata <- jmvcore::naOmit(mydata[c(outcomeVar, predictorVars)])
            } else {
                 # Default to complete case for other unimplemented methods with a warning
                 if (self$options$missing_data_method != 'complete_case') {
                     self$results$instructions$setContent(paste("Note: method", self$options$missing_data_method, "not fully implemented yet. Using complete case analysis."))
                     self$results$instructions$setVisible(TRUE)
                 }
                 mydata <- jmvcore::naOmit(mydata[c(outcomeVar, predictorVars)])
            }
            
            if (nrow(mydata) == 0) {
                self$results$instructions$setContent("No valid data rows found after removing missing values.")
                self$results$instructions$setVisible(TRUE)
                return()
            }

            # 3. Data Summary Table
            tableSummary <- self$results$data_summary
            tableSummary$setRow(rowNo=1, values=list(
                characteristic = "Observations for Model Development",
                value = as.character(nrow(mydata))
            ))

            # 4. Perform Model Fitting
            model_type <- self$options$model_type
            model <- NULL
            
            if (model_type == "logistic_regression") {
                if (is.numeric(mydata[[outcomeVar]])) {
                    mydata[[outcomeVar]] <- as.factor(mydata[[outcomeVar]])
                }
                
                # Feature Selection (Basic Stepwise AIC)
                if (self$options$feature_selection && self$options$feature_selection_method == 'stepwise') {
                    full_formula <- as.formula(paste(jmvcore::composeTerm(outcomeVar), "~", paste(jmvcore::composeTerms(predictorVars), collapse = " + ")))
                    null_formula <- as.formula(paste(jmvcore::composeTerm(outcomeVar), "~ 1"))
                    
                    full_model <- glm(full_formula, data = mydata, family = binomial(link = "logit"))
                    null_model <- glm(null_formula, data = mydata, family = binomial(link = "logit"))
                    
                    # Perform stepwise selection
                    selected_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "both", trace = 0)
                    
                    # Update predictor variables based on selection
                    selected_vars <- names(coef(selected_model))[-1] # Remove intercept
                    # Clean up variable names if needed (step might change them slightly for factors)
                    # For simple matching:
                    predictorVars <- intersect(predictorVars, selected_vars)
                    
                    if (length(predictorVars) == 0) {
                         self$results$instructions$setContent("Feature selection removed all variables. Reverting to full model.")
                         self$results$instructions$setVisible(TRUE)
                         predictorVars <- self$options$predictor_variables 
                    }
                }

                
                formula <- as.formula(paste(jmvcore::composeTerm(outcomeVar), "~", paste(jmvcore::composeTerms(predictorVars), collapse = " + ")))
                model <- try(glm(formula, data = mydata, family = binomial(link = "logit")), silent = TRUE)
                
                if (inherits(model, "try-error")) {
                    self$results$instructions$setContent(paste("Error in Logistic model fitting:", attr(model, "condition")$message))
                    self$results$instructions$setVisible(TRUE)
                    return()
                }
                
                # 5. Model Summary Table
                tableModel <- self$results$model_summary
                tableModel$deleteRows()
                
                s <- summary(model)
                coefs <- s$coefficients
                
                for (i in seq_len(nrow(coefs))) {
                    name <- rownames(coefs)[i]
                    est <- coefs[i, 1]
                    se <- coefs[i, 2]
                    z <- coefs[i, 3]
                    p <- coefs[i, 4]
                    or <- exp(est)
                    
                    tableModel$addRow(rowKey=name, values=list(
                        predictor = name,
                        estimate = est,
                        std_error = se,
                        z_value = z,
                        p_value = p,
                        odds_ratio = or
                    ))
                }
            } else if (model_type == "cox_regression") {
                if (requireNamespace('survival', quietly = TRUE)) {
                    timeVar <- self$options$time_variable
                    eventVar <- self$options$event_variable
                    
                    if (is.null(timeVar) || is.null(eventVar)) {
                        self$results$instructions$setContent("Cox regression requires Time and Event variables.")
                        self$results$instructions$setVisible(TRUE)
                        return()
                    }
                    
                    formula_cox <- as.formula(paste("survival::Surv(", jmvcore::composeTerm(timeVar), ",", jmvcore::composeTerm(eventVar), ") ~", 
                                                 paste(jmvcore::composeTerms(predictorVars), collapse = " + ")))
                    
                    model <- try(survival::coxph(formula_cox, data = mydata), silent = TRUE)
                    
                    if (inherits(model, "try-error")) {
                        self$results$instructions$setContent(paste("Error in Cox model fitting:", attr(model, "condition")$message))
                        self$results$instructions$setVisible(TRUE)
                        return()
                    }
                    
                    tableModel <- self$results$model_summary
                    tableModel$deleteRows()
                    
                    s <- summary(model)
                    coefs <- s$coefficients
                    
                    for (i in seq_len(nrow(coefs))) {
                        name <- rownames(coefs)[i]
                        est <- coefs[i, 1]
                        hr <- coefs[i, 2]
                        se <- coefs[i, 3]
                        z <- coefs[i, 4]
                        p <- coefs[i, 5]
                        
                        tableModel$addRow(rowKey=name, values=list(
                            predictor = name,
                            estimate = est,
                            std_error = se,
                            z_value = z,
                            p_value = p,
                            odds_ratio = hr # Using HR where OR is expected
                        ))
                    }
                }
            }

            # 6. Risk Scoring (Simplified Point System)
            if (!is.null(model)) {
                tableRisk <- self$results$risk_table
                tableRisk$deleteRows()
                
                # Simple rounding of coefficients to create a point system
                # For Cox, coefs are in 'model$coefficients'
                c_vals <- if(inherits(model, "coxph")) model$coefficients else coef(model)
                
                for (name in names(c_vals)) {
                    if (name == "(Intercept)") next
                    est <- c_vals[[name]]
                    points <- round(est * 10)
                    
                    tableRisk$addRow(rowKey=name, values=list(
                        factor = name,
                        level = "Increase per unit",
                        points = points
                    ))
                }
            }

            # 7. Method Explanation
            explanation <- paste0("
                <h3>Clinical Risk Calculators</h3>
                <p>Developing a <b>", gsub("_", " ", model_type), "</b> clinical risk model using selected predictors.</p>
                <ul>
                    <li><b>Model Fitting:</b> The model weights each predictor to maximize prediction accuracy.</li>
                    <li><b>Risk Scores:</b> Traditional risk scores (points) are derived by scaling regression coefficients.</li>
                </ul>
                <p>Triple validation (Internal, Temporal, External) is recommended before clinical implementation.</p>
            ")
            self$results$method_explanation$setContent(explanation)

        },
        .plotNomogram = function(image, ...) {
            if (is.null(self$options$outcome_variable) || is.null(self$options$predictor_variables))
                return(FALSE)
                
            # Nomograms usually require 'rms' package
            if (requireNamespace('rms', quietly = TRUE)) {
                # Placeholder for full rms implementation
                # This would require converting data to rms datadist and using lrm/cph
                # For now, we fall back to our simplified visualization
            }
            
            # Simplified "Nomogram" - Variable Importance / Points Plot
            if (!is.null(self$results$risk_table$state)) {
                # We can't easily access the state of another result element directly due to timing
                # But we can re-calculate or assume tableRisk is populated if model exists
                # Actually, easier to just check if we have a valid model object stored in private
                
                # For now, plotting coefficients as a simple proxy
                # This requires that 'model' is accessible or re-computable
                # We will just print a message that a full Nomogram requires the 'rms' package
                # and that this feature is coming soon in full detail.
                
                plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
                text(0, 0, "Nomogram visualization requires 'rms' integration.\n(Implemented in future update)\n\nPlease refer to the Risk Score Table\nfor points-based calculation.", cex=1.2)
                return(TRUE)
            }
            
            return(FALSE)
        },
        .plotCalibration = function(image, ...) {
            if (is.null(image$state))
                 return(FALSE)
            
            # State should ideally contain predictions and truth
            # But currently we haven't stored state in image. 
            # We need to modify .run to store predictions in image$setState() logic or similar.
            # Since we can't easily pass state between .run and .plotCalibration in standard jamovi without explicit setState,
            # we will check if we can compute it or if we should rely on image state populated in .run
            
            # For now, placeholder with more detail
            plot(c(0, 1), c(0, 1), type="n", xlab="Predicted Probability", ylab="Observed Frequency", main="Calibration Plot")
            abline(0, 1, lty=2, col="gray")
            text(0.5, 0.5, "Calibration Plot requires model predictions state.\n(Validation module coming soon)", cex=1)
            return(TRUE)
        }
    )
)
