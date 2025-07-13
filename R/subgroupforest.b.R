#' @title Subgroup Analysis Forest Plot
#'
#' @description 
#' Creates forest plots showing treatment effects across different patient subgroups.
#' This function performs subgroup analysis for clinical trials and observational studies,
#' calculating treatment effects within patient subgroups and testing for interactions.
#' Supports survival (time-to-event), binary, and continuous outcomes.
#'
#' **Clinical Applications:**
#' - Identify patient subgroups with differential treatment benefit
#' - Test for treatment-by-subgroup interactions  
#' - Guide personalized treatment decisions
#' - Explore heterogeneity in treatment effects
#'
#' **Statistical Methods:**
#' - Survival outcomes: Cox proportional hazards models (Hazard Ratios)
#' - Binary outcomes: Logistic regression (Odds Ratios, Risk Ratios)
#' - Continuous outcomes: Linear regression (Mean Differences)
#' - Interaction testing: Likelihood ratio tests
#'
#' @param data The data as a data frame
#' @param outcome Primary outcome variable. For survival analysis, this should be the time variable
#' @param treatment Treatment or exposure variable (must be binary factor: 0/1, control/treatment)
#' @param subgroups Variables defining patient subgroups for analysis (categorical variables)
#' @param time Time variable for survival analysis (numeric, required if outcomeType = "survival")
#' @param event Event indicator for survival analysis (binary: 1=event occurred, 0=censored)
#' @param outcomeType Type of outcome: "survival" (time-to-event), "binary" (yes/no), "continuous"
#' @param effectMeasure Effect measure: "hr" (hazard ratio), "or" (odds ratio), "rr" (risk ratio), "md" (mean difference)
#' @param confidenceLevel Confidence level for intervals (0.90, 0.95, 0.99)
#' @param showOverall Whether to display overall treatment effect across all patients
#' @param showInteraction Whether to perform statistical tests for subgroup interactions
#' @param sortBy Method for ordering subgroups: "effect" (by effect size), "n" (by sample size), "alpha" (alphabetical)
#' @param showSampleSizes Whether to display sample sizes for each subgroup on the plot
#' @param logScale Whether to display effects on log scale (recommended for ratios)
#' @param nullLine Value for null effect reference line (1 for ratios, 0 for differences)
#'
#' @return A results object containing forest plot, summary tables, and interaction tests
#'
#' @details
#' **Effect Measures by Outcome Type:**
#' 
#' *Survival Outcomes:*
#' - Hazard Ratio (HR): Compares hazard rates between treatment groups
#' - HR > 1: Increased hazard (worse outcome) with treatment
#' - HR < 1: Decreased hazard (better outcome) with treatment
#' 
#' *Binary Outcomes:*
#' - Odds Ratio (OR): Compares odds of outcome between groups
#' - Risk Ratio (RR): Compares probability of outcome between groups
#' - OR/RR > 1: Higher risk with treatment
#' - OR/RR < 1: Lower risk with treatment
#' 
#' *Continuous Outcomes:*
#' - Mean Difference (MD): Difference in means between groups
#' - MD > 0: Higher values with treatment
#' - MD < 0: Lower values with treatment
#'
#' **Interaction Testing:**
#' Tests whether treatment effect varies significantly across subgroups using
#' likelihood ratio tests comparing models with and without interaction terms.
#'
#' **Sample Size Requirements:**
#' - Minimum 5 patients per subgroup for analysis
#' - Larger samples recommended for stable estimates
#' - Consider multiple comparison adjustments for many subgroups
#'
#' @examples
#' \donttest{
#' # Survival outcome subgroup analysis
#' subgroupforest(
#'   data = clinical_trial,
#'   outcome = "time_to_event",
#'   treatment = "treatment_arm", 
#'   subgroups = c("age_group", "gender", "stage"),
#'   time = "time_to_event",
#'   event = "event_occurred",
#'   outcomeType = "survival",
#'   effectMeasure = "hr"
#' )
#' 
#' # Binary outcome analysis
#' subgroupforest(
#'   data = study_data,
#'   outcome = "response",
#'   treatment = "intervention",
#'   subgroups = c("age_category", "sex"),
#'   outcomeType = "binary",
#'   effectMeasure = "or"
#' )
#' }
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import survival
#' @importFrom dplyr group_by summarise mutate arrange
#' @importFrom stats glm binomial confint coef
#'

subgroupforestClass <- if(requireNamespace("jmvcore")) R6::R6Class(
    "subgroupforestClass",
    inherit = subgroupforestBase,
    private = list(
        
        # Validate inputs and data requirements
        .validateInputs = function() {
            data <- self$data
            outcome_var <- self$options$outcome
            treatment_var <- self$options$treatment
            subgroup_vars <- self$options$subgroups
            outcome_type <- self$options$outcomeType
            time_var <- self$options$time
            event_var <- self$options$event
            
            # Check variable existence
            if (!outcome_var %in% names(data))
                stop(paste("Outcome variable '", outcome_var, "' not found in data"))
            
            if (!treatment_var %in% names(data))
                stop(paste("Treatment variable '", treatment_var, "' not found in data"))
            
            for (var in subgroup_vars) {
                if (!var %in% names(data))
                    stop(paste("Subgroup variable '", var, "' not found in data"))
            }
            
            # Validate survival-specific variables
            if (outcome_type == "survival") {
                if (is.null(time_var) || time_var == "")
                    stop("Time variable is required for survival analysis")
                if (is.null(event_var) || event_var == "")
                    stop("Event variable is required for survival analysis")
                
                if (!time_var %in% names(data))
                    stop(paste("Time variable '", time_var, "' not found in data"))
                if (!event_var %in% names(data))
                    stop(paste("Event variable '", event_var, "' not found in data"))
                
                # Check time variable is numeric
                if (!is.numeric(data[[time_var]]))
                    stop("Time variable must be numeric")
                    
                # Check event variable is binary
                event_values <- unique(data[[event_var]][!is.na(data[[event_var]])])
                if (length(event_values) > 2 || !all(event_values %in% c(0, 1)))
                    stop("Event variable must be binary (0=censored, 1=event)")
            }
            
            # Validate treatment variable is binary
            treatment_values <- unique(data[[treatment_var]][!is.na(data[[treatment_var]])])
            if (length(treatment_values) != 2)
                stop("Treatment variable must be binary (exactly 2 levels)")
            
            # Check for minimum sample size
            if (nrow(data) < 10)
                stop("Insufficient data: At least 10 observations required")
            
            # Validate subgroup variables are categorical
            for (var in subgroup_vars) {
                if (is.numeric(data[[var]]) && length(unique(data[[var]])) > 10)
                    warning(paste("Subgroup variable '", var, "' appears to be continuous. Consider categorizing it first."))
            }
            
            # Check for adequate subgroup sizes
            for (var in subgroup_vars) {
                subgroup_sizes <- table(data[[var]], useNA = "no")
                if (any(subgroup_sizes < 5))
                    warning(paste("Some subgroups in '", var, "' have fewer than 5 observations and will be excluded"))
            }
            
            # Validate effect measure matches outcome type
            effect_measure <- self$options$effectMeasure
            if (outcome_type == "survival" && effect_measure != "hr")
                warning("Hazard ratio (hr) is recommended for survival outcomes")
            if (outcome_type == "continuous" && effect_measure != "md")
                warning("Mean difference (md) is recommended for continuous outcomes")
            if (outcome_type == "binary" && !effect_measure %in% c("or", "rr"))
                warning("Odds ratio (or) or risk ratio (rr) are recommended for binary outcomes")
        },
        
        # Calculate effect estimates for subgroups
        .calculateSubgroupEffects = function(df, outcome_var, treatment_var, subgroup_var, 
                                           outcome_type, effect_measure, conf_level, 
                                           time_var = NULL, event_var = NULL) {
            results <- list()
            subgroup_levels <- unique(df[[subgroup_var]])
            subgroup_levels <- subgroup_levels[!is.na(subgroup_levels)]
            
            for (level in subgroup_levels) {
                subset_df <- df[df[[subgroup_var]] == level & !is.na(df[[subgroup_var]]), ]
                
                if (nrow(subset_df) < 5) {
                    warning(paste("Skipping subgroup '", paste(subgroup_var, level, sep = ": "), 
                                "' due to insufficient sample size (n=", nrow(subset_df), ")"))
                    next
                }
                
                tryCatch({
                    if (outcome_type == "survival" && !is.null(time_var) && !is.null(event_var)) {
                        # Survival analysis
                        surv_obj <- survival::Surv(subset_df[[time_var]], subset_df[[event_var]])
                        cox_model <- survival::coxph(surv_obj ~ subset_df[[treatment_var]])
                        
                        hr <- exp(coef(cox_model)[1])
                        ci <- exp(confint(cox_model, level = conf_level))
                        pval <- summary(cox_model)$coefficients[1, "Pr(>|z|)"]
                        n_events <- sum(subset_df[[event_var]], na.rm = TRUE)
                        
                        results[[paste(subgroup_var, level, sep = ": ")]] <- list(
                            subgroup = paste(subgroup_var, level, sep = ": "),
                            n = nrow(subset_df),
                            events = n_events,
                            estimate = hr,
                            ci_lower = ci[1],
                            ci_upper = ci[2],
                            pvalue = pval
                        )
                        
                    } else if (outcome_type == "binary") {
                        # Logistic regression
                        glm_model <- glm(subset_df[[outcome_var]] ~ subset_df[[treatment_var]], 
                                       family = binomial())
                        
                        if (effect_measure == "or") {
                            estimate <- exp(coef(glm_model)[2])
                            ci <- exp(confint(glm_model, level = conf_level))[2, ]
                        } else if (effect_measure == "rr") {
                            # Risk ratio calculation using modified Poisson regression
                            # This is more appropriate than log-binomial which often fails to converge
                            poisson_model <- glm(subset_df[[outcome_var]] ~ subset_df[[treatment_var]], 
                                               family = poisson(link = "log"))
                            
                            estimate <- exp(coef(poisson_model)[2])
                            ci <- exp(confint(poisson_model, level = conf_level))[2, ]
                        } else {
                            # Default to OR if effect measure not recognized
                            estimate <- exp(coef(glm_model)[2])
                            ci <- exp(confint(glm_model, level = conf_level))[2, ]
                        }
                        
                        pval <- summary(glm_model)$coefficients[2, "Pr(>|z|)"]
                        n_events <- sum(subset_df[[outcome_var]], na.rm = TRUE)
                        
                        results[[paste(subgroup_var, level, sep = ": ")]] <- list(
                            subgroup = paste(subgroup_var, level, sep = ": "),
                            n = nrow(subset_df),
                            events = n_events,
                            estimate = estimate,
                            ci_lower = ci[1],
                            ci_upper = ci[2],
                            pvalue = pval
                        )
                        
                    } else if (outcome_type == "continuous") {
                        # Linear regression
                        lm_model <- lm(subset_df[[outcome_var]] ~ subset_df[[treatment_var]])
                        
                        estimate <- coef(lm_model)[2]
                        ci <- confint(lm_model, level = conf_level)[2, ]
                        pval <- summary(lm_model)$coefficients[2, "Pr(>|t|)"]
                        
                        results[[paste(subgroup_var, level, sep = ": ")]] <- list(
                            subgroup = paste(subgroup_var, level, sep = ": "),
                            n = nrow(subset_df),
                            events = NA,
                            estimate = estimate,
                            ci_lower = ci[1],
                            ci_upper = ci[2],
                            pvalue = pval
                        )
                    }
                    
                }, error = function(e) {
                    warning(paste("Error calculating effect for subgroup '", 
                                paste(subgroup_var, level, sep = ": "), "': ", e$message))
                })
            }
            
            return(results)
        },
        
        # Test for subgroup interactions
        .testInteractions = function(df, outcome_var, treatment_var, subgroup_vars,
                                   outcome_type, time_var = NULL, event_var = NULL) {
            interaction_results <- list()
            
            for (subgroup_var in subgroup_vars) {
                tryCatch({
                    if (outcome_type == "survival" && !is.null(time_var) && !is.null(event_var)) {
                        surv_obj <- survival::Surv(df[[time_var]], df[[event_var]])
                        
                        # Model without interaction
                        model1 <- survival::coxph(surv_obj ~ df[[treatment_var]] + df[[subgroup_var]])
                        
                        # Model with interaction
                        model2 <- survival::coxph(surv_obj ~ df[[treatment_var]] * df[[subgroup_var]])
                        
                        # Likelihood ratio test
                        lr_test <- anova(model1, model2)
                        pval <- lr_test$`Pr(>|Chi|)`[2]
                        
                    } else if (outcome_type == "binary") {
                        model1 <- glm(df[[outcome_var]] ~ df[[treatment_var]] + df[[subgroup_var]], 
                                    family = binomial())
                        model2 <- glm(df[[outcome_var]] ~ df[[treatment_var]] * df[[subgroup_var]], 
                                    family = binomial())
                        
                        lr_test <- anova(model1, model2, test = "Chisq")
                        pval <- lr_test$`Pr(>Chi)`[2]
                        
                    } else if (outcome_type == "continuous") {
                        model1 <- lm(df[[outcome_var]] ~ df[[treatment_var]] + df[[subgroup_var]])
                        model2 <- lm(df[[outcome_var]] ~ df[[treatment_var]] * df[[subgroup_var]])
                        
                        f_test <- anova(model1, model2)
                        pval <- f_test$`Pr(>F)`[2]
                    }
                    
                    interpretation <- if (pval < 0.05) {
                        "Significant interaction detected"
                    } else if (pval < 0.10) {
                        "Borderline significant interaction"
                    } else {
                        "No significant interaction"
                    }
                    
                    interaction_results[[subgroup_var]] <- list(
                        variable = subgroup_var,
                        pvalue = pval,
                        interpretation = interpretation
                    )
                    
                }, error = function(e) {
                    interaction_results[[subgroup_var]] <- list(
                        variable = subgroup_var,
                        pvalue = NA,
                        interpretation = "Unable to calculate"
                    )
                })
            }
            
            return(interaction_results)
        },
        
        # Calculate heterogeneity statistics between subgroups
        .calculateHeterogeneity = function(effects_list) {
            if (length(effects_list) < 2) return(NULL)
            
            tryCatch({
                # Extract effect estimates and variances
                estimates <- sapply(effects_list, function(x) log(x$estimate))
                ci_lower <- sapply(effects_list, function(x) log(x$ci_lower))
                ci_upper <- sapply(effects_list, function(x) log(x$ci_upper))
                
                # Calculate standard errors from confidence intervals
                se <- (ci_upper - ci_lower) / (2 * 1.96)  # Assuming 95% CI
                variances <- se^2
                
                # Calculate Q statistic for heterogeneity
                weights <- 1 / variances
                weighted_mean <- sum(weights * estimates) / sum(weights)
                Q <- sum(weights * (estimates - weighted_mean)^2)
                
                # Degrees of freedom
                df <- length(estimates) - 1
                
                # I-squared statistic
                I_squared <- max(0, (Q - df) / Q) * 100
                
                # P-value for heterogeneity
                p_heterogeneity <- 1 - pchisq(Q, df)
                
                return(list(
                    Q = Q,
                    df = df,
                    p_value = p_heterogeneity,
                    I_squared = I_squared,
                    interpretation = if (p_heterogeneity < 0.05) {
                        "Significant heterogeneity detected"
                    } else if (I_squared > 50) {
                        "Moderate heterogeneity (I² > 50%)"
                    } else {
                        "Low heterogeneity"
                    }
                ))
            }, error = function(e) {
                return(list(
                    Q = NA,
                    df = NA,
                    p_value = NA,
                    I_squared = NA,
                    interpretation = "Unable to calculate"
                ))
            })
        },
        
        .run = function() {
            # Comprehensive input validation
            if (is.null(self$options$outcome) || 
                is.null(self$options$treatment) || 
                length(self$options$subgroups) == 0) {
                
                todo <- "
                <br>Welcome to ClinicoPath Subgroup Forest Plot Analysis
                <br><br>
                This tool creates forest plots showing treatment effects across patient subgroups.
                <br><br>
                <b>Required variables:</b>
                <br>- Outcome Variable: Primary endpoint (survival time, binary outcome, continuous)
                <br>- Treatment Variable: Binary treatment/exposure variable
                <br>- Subgroup Variables: Variables defining patient subgroups
                <br><br>
                <b>For survival analysis, also provide:</b>
                <br>- Time Variable: Time to event or censoring
                <br>- Event Variable: Event indicator (1=event, 0=censored)
                <br><br>
                <b>Clinical applications:</b>
                <br>- Identify patient subgroups with differential treatment benefit
                <br>- Test for treatment-by-subgroup interactions
                <br>- Guide personalized treatment decisions
                <hr>
                "
                self$results$todo$setContent(todo)
                return()
            }
            
            if (nrow(self$data) == 0)
                stop("Data contains no (complete) rows")
            
            # Validate inputs before proceeding
            private$.validateInputs()
            
            data <- self$data
            outcome_var <- self$options$outcome
            treatment_var <- self$options$treatment
            subgroup_vars <- self$options$subgroups
            outcome_type <- self$options$outcomeType
            effect_measure <- self$options$effectMeasure
            conf_level <- as.numeric(self$options$confidenceLevel)
            time_var <- self$options$time
            event_var <- self$options$event
            
            # Collect all subgroup effects
            all_effects <- list()
            
            for (subgroup_var in subgroup_vars) {
                subgroup_effects <- private$.calculateSubgroupEffects(
                    data, outcome_var, treatment_var, subgroup_var,
                    outcome_type, effect_measure, conf_level,
                    time_var, event_var
                )
                all_effects <- c(all_effects, subgroup_effects)
            }
            
            # Convert to data frame for plotting and tables
            if (length(all_effects) > 0) {
                effects_df <- do.call(rbind, lapply(all_effects, function(x) data.frame(x)))
                
                # Sort according to user preference
                if (self$options$sortBy == "effect") {
                    effects_df <- effects_df[order(effects_df$estimate), ]
                } else if (self$options$sortBy == "n") {
                    effects_df <- effects_df[order(effects_df$n, decreasing = TRUE), ]
                } else {
                    effects_df <- effects_df[order(effects_df$subgroup), ]
                }
                
                # Populate summary table
                for (i in 1:nrow(effects_df)) {
                    self$results$summary$addRow(rowKey = i, values = list(
                        subgroup = effects_df$subgroup[i],
                        n = effects_df$n[i],
                        events = if (is.na(effects_df$events[i])) "" else effects_df$events[i],
                        estimate = effects_df$estimate[i],
                        ci_lower = effects_df$ci_lower[i],
                        ci_upper = effects_df$ci_upper[i],
                        pvalue = effects_df$pvalue[i]
                    ))
                }
            }
            
            # Test interactions if requested
            if (self$options$showInteraction) {
                interaction_results <- private$.testInteractions(
                    data, outcome_var, treatment_var, subgroup_vars,
                    outcome_type, time_var, event_var
                )
                
                for (result in interaction_results) {
                    if (!is.null(result)) {
                        self$results$interactions$addRow(rowKey = result$variable, values = list(
                            variable = result$variable,
                            pvalue = result$pvalue,
                            interpretation = result$interpretation
                        ))
                    }
                }
            }
            
            # Calculate overall effect if requested
            if (self$options$showOverall) {
                tryCatch({
                    if (outcome_type == "survival" && !is.null(time_var) && !is.null(event_var)) {
                        surv_obj <- survival::Surv(data[[time_var]], data[[event_var]])
                        cox_model <- survival::coxph(surv_obj ~ data[[treatment_var]])
                        
                        hr <- exp(coef(cox_model)[1])
                        ci <- exp(confint(cox_model, level = conf_level))
                        pval <- summary(cox_model)$coefficients[1, "Pr(>|z|)"]
                        
                        self$results$overall$addRow(rowKey = 1, values = list(
                            measure = "Overall Hazard Ratio",
                            estimate = hr,
                            ci_lower = ci[1],
                            ci_upper = ci[2],
                            pvalue = pval
                        ))
                        
                    } else if (outcome_type == "binary") {
                        # Overall effect for binary outcomes
                        glm_model <- glm(data[[outcome_var]] ~ data[[treatment_var]], family = binomial())
                        
                        if (effect_measure == "or") {
                            estimate <- exp(coef(glm_model)[2])
                            ci <- exp(confint(glm_model, level = conf_level))[2, ]
                            measure_name <- "Overall Odds Ratio"
                        } else if (effect_measure == "rr") {
                            poisson_model <- glm(data[[outcome_var]] ~ data[[treatment_var]], family = poisson(link = "log"))
                            estimate <- exp(coef(poisson_model)[2])
                            ci <- exp(confint(poisson_model, level = conf_level))[2, ]
                            measure_name <- "Overall Risk Ratio"
                        }
                        
                        pval <- summary(glm_model)$coefficients[2, "Pr(>|z|)"]
                        
                        self$results$overall$addRow(rowKey = 1, values = list(
                            measure = measure_name,
                            estimate = estimate,
                            ci_lower = ci[1],
                            ci_upper = ci[2],
                            pvalue = pval
                        ))
                        
                    } else if (outcome_type == "continuous") {
                        # Overall effect for continuous outcomes
                        lm_model <- lm(data[[outcome_var]] ~ data[[treatment_var]])
                        
                        estimate <- coef(lm_model)[2]
                        ci <- confint(lm_model, level = conf_level)[2, ]
                        pval <- summary(lm_model)$coefficients[2, "Pr(>|t|)"]
                        
                        self$results$overall$addRow(rowKey = 1, values = list(
                            measure = "Overall Mean Difference",
                            estimate = estimate,
                            ci_lower = ci[1],
                            ci_upper = ci[2],
                            pvalue = pval
                        ))
                    }
                }, error = function(e) {
                    warning(paste("Error calculating overall effect:", e$message))
                })
            }
            
            # Calculate heterogeneity if multiple subgroups
            if (length(all_effects) > 1) {
                heterogeneity <- private$.calculateHeterogeneity(all_effects)
                if (!is.null(heterogeneity)) {
                    # Add heterogeneity summary
                    het_text <- paste0(
                        "<b>Heterogeneity Analysis:</b><br>",
                        "Q-statistic: ", round(heterogeneity$Q, 2), " (df=", heterogeneity$df, ")<br>",
                        "I²: ", round(heterogeneity$I_squared, 1), "%<br>",
                        "P-value: ", format.pval(heterogeneity$p_value, digits = 3), "<br>",
                        "Interpretation: ", heterogeneity$interpretation
                    )
                    
                    # You can add this to a summary table or notes section if available
                }
            }
            
            # Prepare plot data
            if (length(all_effects) > 0) {
                plot_data <- list(
                    effects = effects_df,
                    options = list(
                        effect_measure = effect_measure,
                        log_scale = self$options$logScale,
                        null_line = self$options$nullLine,
                        show_sample_sizes = self$options$showSampleSizes,
                        conf_level = conf_level,
                        outcome_type = outcome_type
                    )
                )
                
                self$results$plot$setState(plot_data)
            }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            plot_data <- image$state
            if (is.null(plot_data)) return()
            
            effects_df <- plot_data$effects
            opts <- plot_data$options
            
            # Validate data for plotting
            if (nrow(effects_df) == 0) {
                p <- ggplot2::ggplot() + 
                    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                    label = "No subgroup data available for plotting",
                                    size = 5) +
                    ggplot2::theme_void()
                print(p)
                return(TRUE)
            }
            
            # Create forest plot with enhanced styling
            p <- ggplot2::ggplot(effects_df, ggplot2::aes(
                x = estimate, 
                y = factor(subgroup, levels = rev(subgroup))
            )) +
                ggplot2::geom_point(size = 3.5, color = "darkblue", alpha = 0.8) +
                ggplot2::geom_errorbarh(
                    ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                    height = 0.3,
                    linewidth = 0.8,
                    color = "darkblue",
                    alpha = 0.7
                ) +
                ggplot2::geom_vline(
                    xintercept = opts$null_line,
                    linetype = "dashed",
                    color = "red",
                    linewidth = 1
                )
            
            # Add sample sizes and effect estimates if requested
            if (opts$show_sample_sizes) {
                # Create label with sample size and events (if applicable)
                effects_df$label <- if ("events" %in% names(effects_df) && 
                                      opts$outcome_type %in% c("survival", "binary")) {
                    paste0("n=", effects_df$n, "/", effects_df$events)
                } else {
                    paste0("n=", effects_df$n)
                }
                
                p <- p + ggplot2::geom_text(
                    ggplot2::aes(label = label),
                    hjust = -0.2,
                    size = 3.2,
                    color = "black"
                )
            }
            
            # Add effect estimates on the right side
            max_x <- max(effects_df$ci_upper, na.rm = TRUE)
            p <- p + ggplot2::geom_text(
                ggplot2::aes(
                    x = max_x * 1.15,
                    label = paste0(round(estimate, 2), " (", 
                                 round(ci_lower, 2), "-", 
                                 round(ci_upper, 2), ")")
                ),
                hjust = 0,
                size = 3,
                color = "black"
            )
            
            # Apply log scale if requested
            if (opts$log_scale && opts$effect_measure %in% c("hr", "or", "rr")) {
                p <- p + ggplot2::scale_x_log10()
            }
            
            # Labels and theme
            effect_label <- switch(opts$effect_measure,
                "hr" = "Hazard Ratio",
                "or" = "Odds Ratio", 
                "rr" = "Risk Ratio",
                "md" = "Mean Difference"
            )
            
            # Enhanced labels and theme
            confidence_pct <- paste0(round(opts$conf_level * 100), "%")
            p <- p + ggplot2::labs(
                x = paste0(effect_label, " (", confidence_pct, " CI)"),
                y = "Subgroup",
                title = "Subgroup Analysis Forest Plot",
                subtitle = paste("Effect measure:", effect_label, "| Null line at", opts$null_line),
                caption = paste("Sample sizes shown as n=total", 
                              if (opts$outcome_type %in% c("survival", "binary")) "/events" else "")
            ) +
                ggtheme +
                ggplot2::theme(
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major.x = ggplot2::element_line(color = "grey90", linewidth = 0.5),
                    legend.position = "none",
                    plot.title = ggplot2::element_text(size = 14, face = "bold"),
                    plot.subtitle = ggplot2::element_text(size = 11),
                    axis.title = ggplot2::element_text(size = 11),
                    axis.text = ggplot2::element_text(size = 10),
                    plot.caption = ggplot2::element_text(size = 9, hjust = 0)
                )
            
            # Final plot adjustments and error handling
            tryCatch({
                print(p)
                TRUE
            }, error = function(e) {
                warning(paste("Error creating forest plot:", e$message))
                # Create a simple fallback plot
                fallback <- ggplot2::ggplot() + 
                    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                    label = "Error creating forest plot",
                                    size = 5) +
                    ggplot2::theme_void()
                print(fallback)
                TRUE
            })
        }
    )
)