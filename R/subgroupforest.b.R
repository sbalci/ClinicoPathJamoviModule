#' @title Subgroup Analysis Forest Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import survival
#' @importFrom dplyr group_by summarise mutate arrange
#' @description Creates forest plots showing treatment effects across different patient subgroups

subgroupforestClass <- if(requireNamespace("jmvcore")) R6::R6Class(
    "subgroupforestClass",
    inherit = subgroupforestBase,
    private = list(
        
        # Calculate effect estimates for subgroups
        .calculateSubgroupEffects = function(df, outcome_var, treatment_var, subgroup_var, 
                                           outcome_type, effect_measure, conf_level, 
                                           time_var = NULL, event_var = NULL) {
            results <- list()
            subgroup_levels <- unique(df[[subgroup_var]])
            subgroup_levels <- subgroup_levels[!is.na(subgroup_levels)]
            
            for (level in subgroup_levels) {
                subset_df <- df[df[[subgroup_var]] == level & !is.na(df[[subgroup_var]]), ]
                
                if (nrow(subset_df) < 5) next  # Skip small subgroups
                
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
                        } else {
                            # Risk ratio calculation would go here
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
                    # Skip subgroups with errors
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
        
        .run = function() {
            # Check required variables
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
                    }
                }, error = function(e) {
                    # Handle errors in overall calculation
                })
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
                        conf_level = conf_level
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
            
            # Create forest plot
            p <- ggplot2::ggplot(effects_df, ggplot2::aes(
                x = estimate, 
                y = factor(subgroup, levels = rev(subgroup))
            )) +
                ggplot2::geom_point(size = 3) +
                ggplot2::geom_errorbarh(
                    ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                    height = 0.2
                ) +
                ggplot2::geom_vline(
                    xintercept = opts$null_line,
                    linetype = "dashed",
                    color = "red"
                )
            
            # Add sample sizes if requested
            if (opts$show_sample_sizes) {
                p <- p + ggplot2::geom_text(
                    ggplot2::aes(label = paste0("n=", n)),
                    hjust = -0.5,
                    size = 3
                )
            }
            
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
            
            p <- p + ggplot2::labs(
                x = paste0(effect_label, " (", opts$conf_level * 100, "% CI)"),
                y = "Subgroup",
                title = "Subgroup Analysis Forest Plot",
                subtitle = paste("Effect measure:", effect_label)
            ) +
                ggtheme +
                ggplot2::theme(
                    panel.grid.minor = ggplot2::element_blank(),
                    legend.position = "none"
                )
            
            print(p)
            TRUE
        }
    )
)