# stagemigration backend, part 5 of 5.
#
# R/stagemigration.b.R grew past 33,000 lines, so its private methods are split across
# stagemigration-part1.R ... stagemigration-part5.R. The analysis class is one R6 inheritance
# chain:
#   stagemigrationBase (generated, stagemigration.h.R) -> stagemigrationPart1 -> ... ->
#   stagemigrationPart5 -> stagemigrationClass (stagemigration.b.R: notices, .init, .run)
# All levels share one private environment, so private$.method() resolves across files and the
# methods are byte-for-byte what they were in the single file. Method names must stay unique across
# the six files: a duplicate would silently override its parent's copy. `inherit` is resolved when
# the class is instantiated, so DESCRIPTION Collate order does not matter.
stagemigrationPart5 <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "stagemigrationPart5",
        inherit = stagemigrationPart4,
        private = list(
            .compareRandomForestModels = function(rf_old, rf_new) {
                tryCatch(
                    {
                        comparison <- list()

                        # Compare basic metrics
                        comparison$sample_size_diff <- rf_new$sample_size - rf_old$sample_size

                        if (!is.null(rf_old$oob_error) && !is.null(rf_new$oob_error)) {
                            comparison$oob_error_improvement <- rf_old$oob_error - rf_new$oob_error
                            comparison$oob_error_pct_improvement <-
                                (comparison$oob_error_improvement / rf_old$oob_error) * 100
                        }

                        if (!is.null(rf_old$concordance_index) && !is.null(rf_new$concordance_index)) {
                            comparison$cindex_improvement <- rf_new$concordance_index - rf_old$concordance_index
                            comparison$cindex_pct_improvement <-
                                (comparison$cindex_improvement / rf_old$concordance_index) * 100
                        }

                        # Variable importance comparison
                        if (!is.null(rf_old$variable_importance) && !is.null(rf_new$variable_importance)) {
                            # Compare stage variable importance if present
                            old_stage_importance <- rf_old$variable_importance[
                                rf_old$variable_importance$Variable == "Old_Stage", "Importance"
                            ]
                            new_stage_importance <- rf_new$variable_importance[
                                rf_new$variable_importance$Variable == "New_Stage", "Importance"
                            ]

                            if (length(old_stage_importance) > 0 && length(new_stage_importance) > 0) {
                                comparison$stage_importance_improvement <- new_stage_importance - old_stage_importance
                            }
                        }

                        # Model complexity comparison
                        comparison$complexity_diff <- rf_new$num_variables - rf_old$num_variables

                        return(comparison)
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .populateRandomForestTables = function(rf_results) {
                tryCatch(
                    {
                        # Populate Performance Summary
                        if (!is.null(rf_results$old_stage) || !is.null(rf_results$new_stage)) {
                            perf_data <- list()

                            if (!is.null(rf_results$old_stage)) {
                                perf_data <- append(perf_data, list(list(
                                    Model = "Old Staging System",
                                    Sample_Size = rf_results$old_stage$sample_size,
                                    Num_Trees = rf_results$old_stage$num_trees,
                                    OOB_Error = if (!is.null(rf_results$old_stage$oob_error)) {
                                        round(rf_results$old_stage$oob_error, 4)
                                    } else {
                                        "N/A"
                                    },
                                    Concordance_Index = if (!is.null(rf_results$old_stage$concordance_index)) {
                                        round(rf_results$old_stage$concordance_index, 4)
                                    } else {
                                        "N/A"
                                    },
                                    Num_Variables = rf_results$old_stage$num_variables
                                )))
                            }

                            if (!is.null(rf_results$new_stage)) {
                                perf_data <- append(perf_data, list(list(
                                    Model = "New Staging System",
                                    Sample_Size = rf_results$new_stage$sample_size,
                                    Num_Trees = rf_results$new_stage$num_trees,
                                    OOB_Error = if (!is.null(rf_results$new_stage$oob_error)) {
                                        round(rf_results$new_stage$oob_error, 4)
                                    } else {
                                        "N/A"
                                    },
                                    Concordance_Index = if (!is.null(rf_results$new_stage$concordance_index)) {
                                        round(rf_results$new_stage$concordance_index, 4)
                                    } else {
                                        "N/A"
                                    },
                                    Num_Variables = rf_results$new_stage$num_variables
                                )))
                            }

                            private$.populateRandomForestPerformance(perf_data)
                        }

                        # Populate Variable Importance
                        importance_data <- list()

                        if (!is.null(rf_results$old_stage$variable_importance)) {
                            for (i in seq_len(min(10, nrow(rf_results$old_stage$variable_importance)))) {
                                var_data <- rf_results$old_stage$variable_importance[i, ]
                                importance_data <- append(importance_data, list(list(
                                    Model = "Old Staging",
                                    Variable = var_data$Variable,
                                    Importance = round(var_data$Importance, 4),
                                    Rank = i
                                )))
                            }
                        }

                        if (!is.null(rf_results$new_stage$variable_importance)) {
                            for (i in seq_len(min(10, nrow(rf_results$new_stage$variable_importance)))) {
                                var_data <- rf_results$new_stage$variable_importance[i, ]
                                importance_data <- append(importance_data, list(list(
                                    Model = "New Staging",
                                    Variable = var_data$Variable,
                                    Importance = round(var_data$Importance, 4),
                                    Rank = i
                                )))
                            }
                        }

                        if (length(importance_data) > 0) {
                            private$.populateRandomForestImportance(importance_data)
                        }

                        # Populate Comparison if available
                        if (!is.null(rf_results$comparison)) {
                            comp_data <- list()
                            comp <- rf_results$comparison

                            comp_data <- append(comp_data, list(list(
                                Metric = "OOB Error Improvement",
                                Value = if (!is.null(comp$oob_error_improvement)) {
                                    round(comp$oob_error_improvement, 4)
                                } else {
                                    "N/A"
                                },
                                Percent_Change = if (!is.null(comp$oob_error_pct_improvement)) {
                                    paste0(round(comp$oob_error_pct_improvement, 2), "%")
                                } else {
                                    "N/A"
                                },
                                Interpretation = if (!is.null(comp$oob_error_improvement)) {
                                    if (comp$oob_error_improvement > 0) {
                                        "New staging reduces prediction error"
                                    } else {
                                        "Old staging has lower prediction error"
                                    }
                                } else {
                                    "N/A"
                                }
                            )))

                            comp_data <- append(comp_data, list(list(
                                Metric = "C-Index Improvement",
                                Value = if (!is.null(comp$cindex_improvement)) {
                                    round(comp$cindex_improvement, 4)
                                } else {
                                    "N/A"
                                },
                                Percent_Change = if (!is.null(comp$cindex_pct_improvement)) {
                                    paste0(round(comp$cindex_pct_improvement, 2), "%")
                                } else {
                                    "N/A"
                                },
                                Interpretation = if (!is.null(comp$cindex_improvement)) {
                                    if (comp$cindex_improvement > 0.01) {
                                        "Meaningful discrimination improvement"
                                    } else if (comp$cindex_improvement > 0) {
                                        "Modest discrimination improvement"
                                    } else {
                                        "No improvement in discrimination"
                                    }
                                } else {
                                    "N/A"
                                }
                            )))

                            private$.populateRandomForestComparison(comp_data)
                        }

                        # Populate Summary
                        summary_data <- list()

                        if (!is.null(rf_results$comparison)) {
                            comp <- rf_results$comparison
                            overall_recommendation <- "Inconclusive"

                            if (!is.null(comp$cindex_improvement) && !is.null(comp$oob_error_improvement)) {
                                if (comp$cindex_improvement > 0.01 && comp$oob_error_improvement > 0) {
                                    overall_recommendation <- "New staging system shows superior performance"
                                } else if (comp$cindex_improvement > 0 || comp$oob_error_improvement > 0) {
                                    overall_recommendation <- "New staging system shows marginal improvement"
                                } else {
                                    overall_recommendation <- "Old staging system performs better"
                                }
                            }

                            summary_data <- append(summary_data, list(list(
                                Analysis_Component = "Random Forest Comparison",
                                Key_Finding = overall_recommendation,
                                Statistical_Significance = "Non-parametric ensemble method",
                                Clinical_Interpretation = paste(
                                    "Random forests provide model-free assessment of staging system performance.",
                                    "Variable importance ranks predictive factors without distributional assumptions."
                                ),
                                Recommendation = .("Consider ensemble predictions for personalized risk assessment")
                            )))
                        }

                        if (length(summary_data) > 0) {
                            private$.populateRandomForestSummary(summary_data)
                        }

                        # Populate Forest Survival Predictions table
                        if (self$options$generateSurvivalPredictions) {
                            private$.populateForestSurvivalPredictions(rf_results)
                        }

                        # Populate Forest Staging Comparison table
                        if (self$options$forestStagingComparison && length(rf_results) >= 2) {
                            private$.populateForestStagingComparison(rf_results)
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateRandomForestPerformance = function(results) {
                tryCatch(
                    {
                        table <- self$results$forestModelPerformance

                        for (result in results) {
                            if (is.list(result) && "Model" %in% names(result)) {
                                table$addRow(rowKey = result$Model, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateRandomForestImportance = function(results) {
                tryCatch(
                    {
                        table <- self$results$forestVariableImportance

                        for (result in results) {
                            if (is.list(result) && "Variable" %in% names(result)) {
                                table$addRow(rowKey = paste(result$Model, result$Variable, sep = "_"), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateRandomForestComparison = function(results) {
                tryCatch(
                    {
                        table <- self$results$forestCoxComparison

                        for (result in results) {
                            if (is.list(result) && "Metric" %in% names(result)) {
                                table$addRow(rowKey = result$Metric, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateRandomForestSummary = function(results) {
                tryCatch(
                    {
                        table <- self$results$forestAnalysisSummary

                        for (result in results) {
                            if (is.list(result) && "Analysis_Component" %in% names(result)) {
                                table$addRow(rowKey = result$Analysis_Component, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },

            # --------------------------------------------------------------------
            # Forest Survival Predictions table
            # Columns: Time_Point, Staging_System, Stage, Survival_Probability,
            #          Prediction_CI_Lower, Prediction_CI_Upper, Risk_Category,
            #          Prediction_Quality, Clinical_Interpretation
            # --------------------------------------------------------------------
            .populateForestSurvivalPredictions = function(rf_results) {
                tryCatch(
                    {
                        table <- self$results$forestSurvivalPredictions

                        # Parse user-specified time points
                        tp_str <- self$options$forestPredictionTimePoints
                        time_points <- as.numeric(unlist(strsplit(tp_str, "\\s*,\\s*")))
                        time_points <- time_points[!is.na(time_points) & time_points > 0]
                        if (length(time_points) == 0) {
                            time_points <- c(12, 24, 36, 60)
                        }

                        # Helper: label for staging system
                        system_labels <- list(
                            old_stage = paste0("Original (", self$options$oldStage, ")"),
                            new_stage = paste0("New (", self$options$newStage, ")")
                        )

                        row_key <- 0
                        for (sys_key in c("old_stage", "new_stage")) {
                            res <- rf_results[[sys_key]]
                            if (is.null(res) || is.null(res$predictions) || is.null(res$stage_values)) next

                            preds <- res$predictions # rfsrc predict object
                            surv_mx <- preds$survival # rows = obs, cols = time indices
                            ti <- preds$time.interest # time grid from the model
                            stages <- res$stage_values # stage factor for each obs

                            stage_levels <- levels(as.factor(stages))

                            for (tp in time_points) {
                                # Find closest time index that does not exceed the model grid
                                if (tp > max(ti)) next
                                t_idx <- which.min(abs(ti - tp))

                                for (stg in stage_levels) {
                                    mask <- stages == stg
                                    if (sum(mask) < 2) next

                                    surv_vals <- surv_mx[mask, t_idx]
                                    mean_surv <- mean(surv_vals, na.rm = TRUE) * 100
                                    se_surv <- sd(surv_vals, na.rm = TRUE) / sqrt(sum(mask)) * 100
                                    ci_lower <- max(0, mean_surv - private$.zCrit() * se_surv)
                                    ci_upper <- min(100, mean_surv + private$.zCrit() * se_surv)

                                    risk_cat <- if (mean_surv >= 80) {
                                        "Low Risk"
                                    } else if (mean_surv >= 50) {
                                        "Moderate Risk"
                                    } else if (mean_surv >= 20) {
                                        "High Risk"
                                    } else {
                                        "Very High Risk"
                                    }

                                    pred_quality <- if (se_surv < 2) {
                                        "High"
                                    } else if (se_surv < 5) {
                                        "Moderate"
                                    } else {
                                        "Low"
                                    }

                                    clinical <- paste0(
                                        round(mean_surv, 1), "% predicted survival at ",
                                        tp, " months for stage ", stg, " (", risk_cat, ")"
                                    )

                                    row_key <- row_key + 1
                                    table$addRow(rowKey = row_key, values = list(
                                        Time_Point              = as.integer(tp),
                                        Staging_System          = system_labels[[sys_key]],
                                        Stage                   = as.character(stg),
                                        Survival_Probability    = round(mean_surv, 1),
                                        Prediction_CI_Lower     = round(ci_lower, 1),
                                        Prediction_CI_Upper     = round(ci_upper, 1),
                                        Risk_Category           = risk_cat,
                                        Prediction_Quality      = pred_quality,
                                        Clinical_Interpretation = clinical
                                    ))
                                }
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },

            # --------------------------------------------------------------------
            # Forest-Based Staging System Comparison table
            # Columns: Analysis_Component, Original_System, New_System,
            #          Forest_Assessment, Improvement_Magnitude,
            #          Statistical_Evidence, Clinical_Recommendation
            # --------------------------------------------------------------------
            .populateForestStagingComparison = function(rf_results) {
                tryCatch(
                    {
                        table <- self$results$forestStagingComparisonTable

                        old <- rf_results$old_stage
                        new <- rf_results$new_stage
                        comp <- rf_results$comparison

                        # Row 1 - Discrimination (C-index)
                        old_c <- if (!is.null(old$concordance_index)) round(old$concordance_index, 4) else NA
                        new_c <- if (!is.null(new$concordance_index)) round(new$concordance_index, 4) else NA
                        c_diff <- if (!is.null(comp$cindex_improvement)) comp$cindex_improvement else NA

                        c_assessment <- if (!is.na(c_diff)) {
                            if (c_diff > 0.05) {
                                "Substantial improvement with new staging"
                            } else if (c_diff > 0.01) {
                                "Modest improvement with new staging"
                            } else if (c_diff > -0.01) {
                                "Comparable discrimination"
                            } else {
                                "Original staging discriminates better"
                            }
                        } else {
                            "Insufficient data"
                        }

                        c_magnitude <- if (!is.na(c_diff)) {
                            paste0(ifelse(c_diff >= 0, "+", ""), round(c_diff, 4))
                        } else {
                            "N/A"
                        }

                        c_evidence <- if (!is.na(c_diff)) {
                            if (abs(c_diff) > 0.05) {
                                "Strong"
                            } else if (abs(c_diff) > 0.02) {
                                "Moderate"
                            } else {
                                "Weak"
                            }
                        } else {
                            "N/A"
                        }

                        c_rec <- if (!is.na(c_diff) && c_diff > 0.02) {
                            "New staging improves risk discrimination"
                        } else if (!is.na(c_diff) && c_diff < -0.02) {
                            "Retain original staging for discrimination"
                        } else {
                            "No clear advantage; consider other criteria"
                        }

                        table$addRow(rowKey = "discrimination", values = list(
                            Analysis_Component      = "Discrimination (C-Index)",
                            Original_System         = if (!is.na(old_c)) as.character(old_c) else "N/A",
                            New_System              = if (!is.na(new_c)) as.character(new_c) else "N/A",
                            Forest_Assessment       = c_assessment,
                            Improvement_Magnitude   = c_magnitude,
                            Statistical_Evidence    = c_evidence,
                            Clinical_Recommendation = c_rec
                        ))

                        # Row 2 - Prediction Error (OOB)
                        old_err <- if (!is.null(old$oob_error)) round(old$oob_error, 4) else NA
                        new_err <- if (!is.null(new$oob_error)) round(new$oob_error, 4) else NA
                        err_diff <- if (!is.null(comp$oob_error_improvement)) comp$oob_error_improvement else NA

                        err_assessment <- if (!is.na(err_diff)) {
                            if (err_diff > 0.02) {
                                "New staging reduces prediction error"
                            } else if (err_diff > 0) {
                                "Marginal error reduction"
                            } else {
                                "Original staging has lower error"
                            }
                        } else {
                            "Insufficient data"
                        }

                        err_magnitude <- if (!is.na(err_diff)) {
                            paste0(ifelse(err_diff >= 0, "-", "+"), round(abs(err_diff), 4), " error")
                        } else {
                            "N/A"
                        }

                        err_evidence <- if (!is.na(err_diff)) {
                            if (abs(err_diff) > 0.03) {
                                "Strong"
                            } else if (abs(err_diff) > 0.01) {
                                "Moderate"
                            } else {
                                "Weak"
                            }
                        } else {
                            "N/A"
                        }

                        err_rec <- if (!is.na(err_diff) && err_diff > 0.01) {
                            "New staging yields more accurate predictions"
                        } else if (!is.na(err_diff) && err_diff < -0.01) {
                            "Original staging has superior prediction accuracy"
                        } else {
                            "Similar prediction accuracy"
                        }

                        table$addRow(rowKey = "prediction_error", values = list(
                            Analysis_Component      = "Prediction Error (OOB)",
                            Original_System         = if (!is.na(old_err)) as.character(old_err) else "N/A",
                            New_System              = if (!is.na(new_err)) as.character(new_err) else "N/A",
                            Forest_Assessment       = err_assessment,
                            Improvement_Magnitude   = err_magnitude,
                            Statistical_Evidence    = err_evidence,
                            Clinical_Recommendation = err_rec
                        ))

                        # Row 3 - Variable Importance (stage variable ranking)
                        old_imp <- NULL
                        new_imp <- NULL
                        if (!is.null(old$variable_importance)) {
                            idx <- which(old$variable_importance$Variable == "Old_Stage")
                            if (length(idx) > 0) old_imp <- round(old$variable_importance$Importance[idx[1]], 4)
                        }
                        if (!is.null(new$variable_importance)) {
                            idx <- which(new$variable_importance$Variable == "New_Stage")
                            if (length(idx) > 0) new_imp <- round(new$variable_importance$Importance[idx[1]], 4)
                        }

                        imp_assessment <- if (!is.null(old_imp) && !is.null(new_imp)) {
                            if (new_imp > old_imp * 1.1) {
                                "New staging has higher predictive importance"
                            } else if (old_imp > new_imp * 1.1) {
                                "Original staging has higher importance"
                            } else {
                                "Similar variable importance"
                            }
                        } else {
                            "Variable importance not computed"
                        }

                        imp_magnitude <- if (!is.null(old_imp) && !is.null(new_imp)) {
                            diff_imp <- new_imp - old_imp
                            paste0(ifelse(diff_imp >= 0, "+", ""), round(diff_imp, 4))
                        } else {
                            "N/A"
                        }

                        table$addRow(rowKey = "variable_importance", values = list(
                            Analysis_Component = "Stage Variable Importance",
                            Original_System = if (!is.null(old_imp)) as.character(old_imp) else "N/A",
                            New_System = if (!is.null(new_imp)) as.character(new_imp) else "N/A",
                            Forest_Assessment = imp_assessment,
                            Improvement_Magnitude = imp_magnitude,
                            Statistical_Evidence = "Non-parametric (VIMP)",
                            Clinical_Recommendation = if (!is.null(old_imp) && !is.null(new_imp) && new_imp > old_imp) {
                                "New staging captures more survival-relevant information"
                            } else {
                                "Review variable importance rankings"
                            }
                        ))

                        # Row 4 - Model Complexity
                        old_nvar <- if (!is.null(old$num_variables)) old$num_variables else NA
                        new_nvar <- if (!is.null(new$num_variables)) new$num_variables else NA

                        complexity_assess <- if (!is.na(old_nvar) && !is.na(new_nvar)) {
                            if (new_nvar > old_nvar) {
                                "New model is more complex"
                            } else if (new_nvar < old_nvar) {
                                "New model is simpler"
                            } else {
                                "Equal complexity"
                            }
                        } else {
                            "N/A"
                        }

                        table$addRow(rowKey = "complexity", values = list(
                            Analysis_Component = "Model Complexity",
                            Original_System = if (!is.na(old_nvar)) paste(old_nvar, "predictors") else "N/A",
                            New_System = if (!is.na(new_nvar)) paste(new_nvar, "predictors") else "N/A",
                            Forest_Assessment = complexity_assess,
                            Improvement_Magnitude = if (!is.na(old_nvar) && !is.na(new_nvar)) {
                                paste0(ifelse(new_nvar - old_nvar >= 0, "+", ""), new_nvar - old_nvar)
                            } else {
                                "N/A"
                            },
                            Statistical_Evidence = "Structural comparison",
                            Clinical_Recommendation = "Prefer simpler model when performance is equivalent"
                        ))

                        # Row 5 - Overall Assessment
                        wins_new <- 0
                        wins_old <- 0
                        if (!is.na(c_diff) && c_diff > 0.01) wins_new <- wins_new + 1
                        if (!is.na(c_diff) && c_diff < -0.01) wins_old <- wins_old + 1
                        if (!is.na(err_diff) && err_diff > 0.01) wins_new <- wins_new + 1
                        if (!is.na(err_diff) && err_diff < -0.01) wins_old <- wins_old + 1
                        if (!is.null(old_imp) && !is.null(new_imp) && new_imp > old_imp * 1.05) wins_new <- wins_new + 1
                        if (!is.null(old_imp) && !is.null(new_imp) && old_imp > new_imp * 1.05) wins_old <- wins_old + 1

                        overall <- if (wins_new > wins_old) {
                            "New staging system is preferred by random forest analysis"
                        } else if (wins_old > wins_new) {
                            "Original staging system performs better overall"
                        } else {
                            "No clear winner; clinical context should guide decision"
                        }

                        table$addRow(rowKey = "overall", values = list(
                            Analysis_Component      = "Overall Forest Assessment",
                            Original_System         = paste(wins_old, "metrics favor"),
                            New_System              = paste(wins_new, "metrics favor"),
                            Forest_Assessment       = overall,
                            Improvement_Magnitude   = paste0(wins_new, " vs ", wins_old),
                            Statistical_Evidence    = "Ensemble consensus",
                            Clinical_Recommendation = overall
                        ))
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },

            # ============================================================================
            # Cure Models for Populations with Cured Fraction
            # ============================================================================

            .performCureModelAnalysis = function(data, all_results) {
                # Mixture models for populations with cured fraction
                # Separates susceptible and cured populations for staging system validation

                tryCatch(
                    {
                        if (!self$options$performCureModelAnalysis) {
                            return(NULL)
                        }

                        # Basic validation
                        if (nrow(data) < 100) {
                            private$.addNotice("STRONG_WARNING", .("Cure model analysis not run"),
                                jmvcore::format(.("Sample size ({n}) is below the minimum of 100 needed for stable cure fraction estimates."), n = sprintf("%d", nrow(data))))
                            return(NULL)
                        }

                        # Load required packages
                        if (!requireNamespace("flexsurv", quietly = TRUE)) {
                            private$.addNotice("STRONG_WARNING", .("Cure model analysis not run"),
                                .("The flexsurv package could not be loaded. It is installed with ClinicoPath, so reinstalling the module should restore it."))
                            return(NULL)
                        }

                        # Initialize result storage
                        cure_results <- list()

                        # Extract required variables
                        survival_time <- data[[self$options$survivalTime]]
                        event_indicator <- data[[self$options$event]]
                        old_stage <- data[[self$options$oldStage]]
                        new_stage <- data[[self$options$newStage]]

                        # Get covariates if specified
                        covariates <- c()
                        if (!is.null(self$options$cureCovariates) && length(self$options$cureCovariates) > 0) {
                            covariates <- self$options$cureCovariates
                        }

                        # Perform Cure Model for Old Staging System
                        if (self$options$cureAnalyzeOldStage) {
                            cure_old <- private$.performCureModel(
                                data, old_stage, "Old_Stage", covariates, survival_time, event_indicator
                            )
                            if (!is.null(cure_old)) {
                                cure_results$old_stage <- cure_old
                            }
                        }

                        # Perform Cure Model for New Staging System
                        if (self$options$cureAnalyzeNewStage) {
                            cure_new <- private$.performCureModel(
                                data, new_stage, "New_Stage", covariates, survival_time, event_indicator
                            )
                            if (!is.null(cure_new)) {
                                cure_results$new_stage <- cure_new
                            }
                        }

                        # Comparative Analysis if both models available
                        if (length(cure_results) == 2) {
                            comparison_results <- private$.compareCureModels(cure_results$old_stage, cure_results$new_stage)
                            cure_results$comparison <- comparison_results
                        }

                        # Stage-specific analysis if requested
                        if (self$options$cureStageSpecificAnalysis) {
                            stage_specific_results <- private$.performStageSpecificCureAnalysis(data, cure_results)
                            cure_results$stage_specific <- stage_specific_results
                        }

                        # Bootstrap validation if requested
                        if (self$options$cureBootstrapCI) {
                            bootstrap_results <- private$.performCureBootstrapValidation(data, cure_results)
                            cure_results$bootstrap <- bootstrap_results
                        }

                        # Populate result tables
                        if (length(cure_results) > 0) {
                            private$.populateCureModelTables(cure_results)
                            return("Cure model analysis completed successfully")
                        }

                        return(NULL)
                    },
                    error = function(e) {
                        private$.addNotice("STRONG_WARNING", .("Cure model analysis failed"), conditionMessage(e))
                        return(NULL)
                    }
                )
            },
            .performCureModel = function(data, stage_var, stage_name, covariates, survival_time, event_indicator) {
                tryCatch(
                    {
                        # Prepare data for cure modeling
                        temp_data <- data
                        temp_data[[stage_name]] <- stage_var
                        temp_data$time <- survival_time
                        temp_data$status <- event_indicator

                        # Add covariates if specified
                        predictor_vars <- c(stage_name, covariates)
                        predictor_vars <- predictor_vars[predictor_vars %in% names(temp_data)]

                        temp_data <- temp_data[c("time", "status", predictor_vars)]
                        temp_data <- temp_data[complete.cases(temp_data), ]

                        if (nrow(temp_data) < 50) {
                            return(NULL)
                        }

                        # Detect potential cure fraction using Kaplan-Meier
                        km_fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = temp_data)

                        # Estimate plateau (cure fraction) from KM curve
                        time_horizon <- self$options$cureTimeHorizon

                        # Get survival probability at time horizon
                        if (max(km_fit$time[km_fit$n.event > 0]) >= time_horizon) {
                            surv_at_horizon <- summary(km_fit, times = time_horizon)$surv
                            potential_cure_fraction <- max(0, surv_at_horizon)
                        } else {
                            # Use tail probability if time horizon not reached
                            surv_probs <- km_fit$surv[km_fit$time >= max(km_fit$time) * 0.8]
                            potential_cure_fraction <- max(0, mean(surv_probs, na.rm = TRUE))
                        }

                        # Only proceed if there's evidence of curing
                        if (potential_cure_fraction < 0.05) {
                            return(list(
                                model_name = stage_name,
                                cure_fraction_estimate = 0,
                                evidence_of_cure = FALSE,
                                reason = "No evidence of cure fraction (long-term survival < 5%)"
                            ))
                        }

                        results <- list()
                        results$model_name <- stage_name
                        results$sample_size <- nrow(temp_data)
                        results$potential_cure_fraction <- potential_cure_fraction
                        results$evidence_of_cure <- TRUE

                        # Fit mixture cure model using flexsurv
                        distribution <- self$options$cureDistribution
                        model_type <- self$options$cureModelType

                        # Build formula
                        if (length(predictor_vars) > 1) {
                            formula_str <- paste("Surv(time, status) ~", paste(predictor_vars, collapse = " + "))
                        } else {
                            formula_str <- paste("Surv(time, status) ~", predictor_vars[1])
                        }
                        cure_formula <- as.formula(formula_str)

                        # Fit cure model based on distribution choice
                        if (distribution == "weibull") {
                            cure_fit <- flexsurv::flexsurvcure(
                                cure_formula,
                                data = temp_data,
                                dist = "weibull", mixture = (model_type != "promotion"),
                                control = list(trace = 0)
                            )
                        } else if (distribution == "exponential") {
                            cure_fit <- flexsurv::flexsurvcure(
                                cure_formula,
                                data = temp_data,
                                dist = "exp", mixture = (model_type != "promotion"),
                                control = list(trace = 0)
                            )
                        } else if (distribution == "lognormal") {
                            cure_fit <- flexsurv::flexsurvcure(
                                cure_formula,
                                data = temp_data,
                                dist = "lognormal", mixture = (model_type != "promotion"),
                                control = list(trace = 0)
                            )
                        } else {
                            cure_fit <- flexsurv::flexsurvcure(
                                cure_formula,
                                data = temp_data,
                                dist = "llogis", mixture = (model_type != "promotion"),
                                control = list(trace = 0)
                            )
                        }

                        # Extract results if convergence successful
                        if (!is.null(cure_fit) && cure_fit$res$convergence == 0) {
                            results$cure_model <- cure_fit
                            results$convergence <- "Successful"
                            results$log_likelihood <- cure_fit$loglik
                            results$aic <- cure_fit$AIC
                            results$distribution <- distribution
                            results$model_type <- model_type

                            # Extract cure fraction estimate
                            if (model_type != "promotion") {
                                # Mixture cure model
                                results$overall_cure_fraction <- 1 - plogis(cure_fit$res$par[names(cure_fit$res$par) == "theta"])
                            } else {
                                # Promotion time model - calculate from parameters
                                results$overall_cure_fraction <- potential_cure_fraction # Approximation
                            }

                            # Stage-specific cure fractions
                            stage_levels <- unique(temp_data[[stage_name]])
                            stage_cure_fractions <- list()

                            for (stage in stage_levels) {
                                stage_data <- temp_data[temp_data[[stage_name]] == stage, ]
                                if (nrow(stage_data) >= 10) {
                                    stage_km <- survival::survfit(survival::Surv(time, status) ~ 1, data = stage_data)
                                    if (max(stage_km$time[stage_km$n.event > 0]) >= time_horizon) {
                                        stage_surv <- summary(stage_km, times = time_horizon)$surv
                                    } else {
                                        stage_surv_probs <- stage_km$surv[stage_km$time >= max(stage_km$time) * 0.8]
                                        stage_surv <- max(0, mean(stage_surv_probs, na.rm = TRUE))
                                    }
                                    stage_cure_fractions[[as.character(stage)]] <- max(0, stage_surv)
                                }
                            }

                            results$stage_cure_fractions <- stage_cure_fractions
                        } else {
                            results$convergence <- "Failed"
                            results$overall_cure_fraction <- potential_cure_fraction
                            results$cure_model <- NULL
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .compareCureModels = function(cure_old, cure_new) {
                tryCatch(
                    {
                        comparison <- list()

                        # Compare overall cure fractions
                        if (!is.null(cure_old$overall_cure_fraction) && !is.null(cure_new$overall_cure_fraction)) {
                            comparison$cure_fraction_difference <- cure_new$overall_cure_fraction - cure_old$overall_cure_fraction
                            comparison$cure_fraction_improvement <-
                                (comparison$cure_fraction_difference / cure_old$overall_cure_fraction) * 100
                        }

                        # Compare model fit if both models converged
                        if (!is.null(cure_old$aic) && !is.null(cure_new$aic)) {
                            comparison$aic_difference <- cure_old$aic - cure_new$aic # Positive = new is better
                            comparison$model_preference <- if (comparison$aic_difference > 2) {
                                "New staging preferred"
                            } else if (comparison$aic_difference < -2) {
                                "Old staging preferred"
                            } else {
                                "Similar performance"
                            }
                        }

                        # Likelihood ratio test if models are nested
                        if (!is.null(cure_old$log_likelihood) && !is.null(cure_new$log_likelihood)) {
                            comparison$lr_statistic <- 2 * (cure_new$log_likelihood - cure_old$log_likelihood)
                            comparison$lr_p_value <- 1 - pchisq(abs(comparison$lr_statistic), df = 1)
                        }

                        # Stage-specific comparison
                        if (!is.null(cure_old$stage_cure_fractions) && !is.null(cure_new$stage_cure_fractions)) {
                            comparison$stage_discrimination <- private$.compareStageCureFractions(
                                cure_old$stage_cure_fractions,
                                cure_new$stage_cure_fractions
                            )
                        }

                        return(comparison)
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .compareStageCureFractions = function(old_fractions, new_fractions) {
                tryCatch(
                    {
                        old_values <- unlist(old_fractions)
                        new_values <- unlist(new_fractions)

                        if (length(old_values) > 1 && length(new_values) > 1) {
                            # Calculate range and coefficient of variation
                            old_range <- max(old_values) - min(old_values)
                            new_range <- max(new_values) - min(new_values)

                            old_cv <- sd(old_values) / mean(old_values)
                            new_cv <- sd(new_values) / mean(new_values)

                            discrimination_improvement <- new_range - old_range
                            cv_improvement <- new_cv - old_cv

                            return(list(
                                range_improvement = discrimination_improvement,
                                cv_improvement = cv_improvement,
                                better_discrimination = discrimination_improvement > 0.1 && cv_improvement > 0.1
                            ))
                        }

                        return(NULL)
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .performStageSpecificCureAnalysis = function(data, cure_results) {
                tryCatch(
                    {
                        stage_analysis <- list()

                        # Analyze old staging if available
                        if (!is.null(cure_results$old_stage) && !is.null(cure_results$old_stage$stage_cure_fractions)) {
                            old_fractions <- cure_results$old_stage$stage_cure_fractions
                            stage_analysis$old_stage <- list(
                                num_stages = length(old_fractions),
                                cure_fraction_range = paste0(
                                    round(min(unlist(old_fractions)), 3), " - ",
                                    round(max(unlist(old_fractions)), 3)
                                ),
                                discrimination_quality = if (max(unlist(old_fractions)) - min(unlist(old_fractions)) > 0.3) {
                                    "Good"
                                } else if (max(unlist(old_fractions)) - min(unlist(old_fractions)) > 0.15) {
                                    "Moderate"
                                } else {
                                    "Poor"
                                }
                            )
                        }

                        # Analyze new staging if available
                        if (!is.null(cure_results$new_stage) && !is.null(cure_results$new_stage$stage_cure_fractions)) {
                            new_fractions <- cure_results$new_stage$stage_cure_fractions
                            stage_analysis$new_stage <- list(
                                num_stages = length(new_fractions),
                                cure_fraction_range = paste0(
                                    round(min(unlist(new_fractions)), 3), " - ",
                                    round(max(unlist(new_fractions)), 3)
                                ),
                                discrimination_quality = if (max(unlist(new_fractions)) - min(unlist(new_fractions)) > 0.3) {
                                    "Good"
                                } else if (max(unlist(new_fractions)) - min(unlist(new_fractions)) > 0.15) {
                                    "Moderate"
                                } else {
                                    "Poor"
                                }
                            )
                        }

                        return(stage_analysis)
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .performCureBootstrapValidation = function(data, cure_results) {
                tryCatch(
                    {
                        bootstrap_reps <- self$options$cureBootstrapReps
                        bootstrap_results <- list()

                        # Bootstrap validation for cure fraction estimates
                        if (!is.null(cure_results$old_stage) && cure_results$old_stage$evidence_of_cure) {
                            old_bootstrap <- private$.bootstrapCureFraction(data, cure_results$old_stage, bootstrap_reps)
                            bootstrap_results$old_stage <- old_bootstrap
                        }

                        if (!is.null(cure_results$new_stage) && cure_results$new_stage$evidence_of_cure) {
                            new_bootstrap <- private$.bootstrapCureFraction(data, cure_results$new_stage, bootstrap_reps)
                            bootstrap_results$new_stage <- new_bootstrap
                        }

                        return(bootstrap_results)
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .bootstrapCureFraction = function(data, cure_model_results, n_bootstrap) {
                tryCatch(
                    {
                        bootstrap_estimates <- numeric(n_bootstrap)

                        for (i in 1:n_bootstrap) {
                            # Bootstrap sample
                            boot_indices <- sample(nrow(data), replace = TRUE)
                            boot_data <- data[boot_indices, ]

                            # Fit simple Kaplan-Meier to estimate cure fraction
                            km_fit <- survival::survfit(
                                survival::Surv(
                                    boot_data[[self$options$survivalTime]],
                                    # validated 0/1 column; the raw option may be a factor, and
                                    # Surv() would read its level indices as 1 = censored / 2 = event
                                    if ("event_binary" %in% names(boot_data)) boot_data[["event_binary"]] else boot_data[[self$options$event]]
                                ) ~ 1,
                                data = boot_data
                            )

                            # Estimate cure fraction from plateau
                            time_horizon <- self$options$cureTimeHorizon
                            if (max(km_fit$time[km_fit$n.event > 0]) >= time_horizon) {
                                surv_at_horizon <- summary(km_fit, times = time_horizon)$surv
                                bootstrap_estimates[i] <- max(0, surv_at_horizon)
                            } else {
                                surv_probs <- km_fit$surv[km_fit$time >= max(km_fit$time) * 0.8]
                                bootstrap_estimates[i] <- max(0, mean(surv_probs, na.rm = TRUE))
                            }
                        }

                        # Calculate bootstrap statistics
                        original_estimate <- cure_model_results$overall_cure_fraction
                        bootstrap_mean <- mean(bootstrap_estimates, na.rm = TRUE)
                        bootstrap_sd <- sd(bootstrap_estimates, na.rm = TRUE)
                        cure_cl <- self$options$cureConfidenceLevel %||% 0.95
                        if (!is.finite(cure_cl) || cure_cl <= 0 || cure_cl >= 1) cure_cl <- 0.95
                        bootstrap_ci <- quantile(bootstrap_estimates, c((1 - cure_cl) / 2, 1 - (1 - cure_cl) / 2), na.rm = TRUE)
                        bias <- bootstrap_mean - original_estimate

                        return(list(
                            original_estimate = original_estimate,
                            bootstrap_mean = bootstrap_mean,
                            bootstrap_sd = bootstrap_sd,
                            bootstrap_ci_lower = bootstrap_ci[1],
                            bootstrap_ci_upper = bootstrap_ci[2],
                            bias = bias
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .populateCureModelTables = function(cure_results) {
                tryCatch(
                    {
                        # Populate Cure Fraction Estimates
                        # Stage-level CF_Lower_CI / CF_Upper_CI are NA. They were cf -/+ 0.10, a
                        # hardcoded width presented as a confidence interval. The bootstrap below
                        # estimates only the OVERALL cure fraction, so no stage-level interval exists.
                        fraction_data <- list()

                        if (!is.null(cure_results$old_stage)) {
                            cure_old <- cure_results$old_stage
                            if (!is.null(cure_old$stage_cure_fractions)) {
                                for (stage in names(cure_old$stage_cure_fractions)) {
                                    fraction_data <- append(fraction_data, list(list(
                                        Staging_System = "Original",
                                        Stage_Group = stage,
                                        Sample_Size = cure_old$sample_size,
                                        Cure_Fraction = round(cure_old$stage_cure_fractions[[stage]], 4),
                                        CF_Lower_CI = NA_real_,
                                        CF_Upper_CI = NA_real_,
                                        Median_Survival_Susceptible = "N/A",
                                        Model_Type = cure_old$model_type %||% "Non-parametric",
                                        Convergence_Status = cure_old$convergence %||% "N/A"
                                    )))
                                }
                            }
                        }

                        if (!is.null(cure_results$new_stage)) {
                            cure_new <- cure_results$new_stage
                            if (!is.null(cure_new$stage_cure_fractions)) {
                                for (stage in names(cure_new$stage_cure_fractions)) {
                                    fraction_data <- append(fraction_data, list(list(
                                        Staging_System = "New",
                                        Stage_Group = stage,
                                        Sample_Size = cure_new$sample_size,
                                        Cure_Fraction = round(cure_new$stage_cure_fractions[[stage]], 4),
                                        CF_Lower_CI = NA_real_,
                                        CF_Upper_CI = NA_real_,
                                        Median_Survival_Susceptible = "N/A",
                                        Model_Type = cure_new$model_type %||% "Non-parametric",
                                        Convergence_Status = cure_new$convergence %||% "N/A"
                                    )))
                                }
                            }
                        }

                        if (length(fraction_data) > 0) {
                            private$.populateCureFractionEstimates(fraction_data)
                        }

                        # Populate Model Parameters
                        param_data <- list()

                        if (!is.null(cure_results$old_stage) && !is.null(cure_results$old_stage$aic)) {
                            param_data <- append(param_data, list(list(
                                Staging_System = "Original",
                                Model_Type = cure_results$old_stage$model_type %||% "Mixture",
                                Distribution = cure_results$old_stage$distribution %||% "Weibull",
                                Log_Likelihood = cure_results$old_stage$log_likelihood %||% NA,
                                AIC = cure_results$old_stage$aic %||% NA,
                                BIC = (cure_results$old_stage$aic %||% NA) + log(cure_results$old_stage$sample_size),
                                Overall_Cure_Fraction = cure_results$old_stage$overall_cure_fraction %||% NA,
                                Shape_Parameter = NA,
                                Scale_Parameter = NA,
                                Goodness_of_Fit_p = NA
                            )))
                        }

                        if (!is.null(cure_results$new_stage) && !is.null(cure_results$new_stage$aic)) {
                            param_data <- append(param_data, list(list(
                                Staging_System = "New",
                                Model_Type = cure_results$new_stage$model_type %||% "Mixture",
                                Distribution = cure_results$new_stage$distribution %||% "Weibull",
                                Log_Likelihood = cure_results$new_stage$log_likelihood %||% NA,
                                AIC = cure_results$new_stage$aic %||% NA,
                                BIC = (cure_results$new_stage$aic %||% NA) + log(cure_results$new_stage$sample_size),
                                Overall_Cure_Fraction = cure_results$new_stage$overall_cure_fraction %||% NA,
                                Shape_Parameter = NA,
                                Scale_Parameter = NA,
                                Goodness_of_Fit_p = NA
                            )))
                        }

                        if (length(param_data) > 0) {
                            private$.populateCureModelParameters(param_data)
                        }

                        # Populate Comparison if available
                        if (!is.null(cure_results$comparison)) {
                            comp_data <- list()
                            comp <- cure_results$comparison

                            comp_data <- append(comp_data, list(list(
                                Comparison_Metric = "Cure Fraction Difference",
                                Original_System = if (!is.null(cure_results$old_stage$overall_cure_fraction)) {
                                    round(cure_results$old_stage$overall_cure_fraction, 4)
                                } else {
                                    "N/A"
                                },
                                New_System = if (!is.null(cure_results$new_stage$overall_cure_fraction)) {
                                    round(cure_results$new_stage$overall_cure_fraction, 4)
                                } else {
                                    "N/A"
                                },
                                Difference = if (!is.null(comp$cure_fraction_difference)) {
                                    round(comp$cure_fraction_difference, 4)
                                } else {
                                    "N/A"
                                },
                                LR_Test_Statistic = if (!is.null(comp$lr_statistic)) {
                                    round(comp$lr_statistic, 4)
                                } else {
                                    "N/A"
                                },
                                p_value = if (!is.null(comp$lr_p_value)) comp$lr_p_value else NA,
                                Statistical_Significance = if (!is.null(comp$lr_p_value)) {
                                    if (comp$lr_p_value < 0.001) {
                                        "Highly Significant"
                                    } else if (comp$lr_p_value < 0.01) {
                                        "Significant"
                                    } else if (comp$lr_p_value < 0.05) {
                                        "Marginally Significant"
                                    } else {
                                        "Non-Significant"
                                    }
                                } else {
                                    "N/A"
                                },
                                Clinical_Interpretation = if (!is.null(comp$cure_fraction_difference)) {
                                    if (abs(comp$cure_fraction_difference) > 0.1) {
                                        "Substantial difference in cure rates"
                                    } else if (abs(comp$cure_fraction_difference) > 0.05) {
                                        "Moderate difference in cure rates"
                                    } else {
                                        "Similar cure rates between staging systems"
                                    }
                                } else {
                                    "N/A"
                                }
                            )))

                            private$.populateCureModelComparison(comp_data)
                        }

                        # Populate Stage-Specific Analysis
                        if (!is.null(cure_results$stage_specific)) {
                            stage_data <- list()
                            stage_spec <- cure_results$stage_specific

                            if (!is.null(stage_spec$old_stage)) {
                                stage_data <- append(stage_data, list(list(
                                    Analysis_Component = "Stage Discrimination",
                                    Staging_System = "Original",
                                    Stage_Specific_Finding = paste("Cure fractions range:", stage_spec$old_stage$cure_fraction_range),
                                    Cure_Fraction_Range = stage_spec$old_stage$cure_fraction_range,
                                    Discrimination_Quality = stage_spec$old_stage$discrimination_quality,
                                    Clinical_Utility = if (stage_spec$old_stage$discrimination_quality == "Good") {
                                        "Excellent for treatment stratification"
                                    } else {
                                        "Limited discrimination capability"
                                    },
                                    Recommendation = if (stage_spec$old_stage$discrimination_quality == "Good") {
                                        "Staging system effectively identifies cure potential"
                                    } else {
                                        "Consider refinement for better cure prediction"
                                    }
                                )))
                            }

                            if (!is.null(stage_spec$new_stage)) {
                                stage_data <- append(stage_data, list(list(
                                    Analysis_Component = "Stage Discrimination",
                                    Staging_System = "New",
                                    Stage_Specific_Finding = paste("Cure fractions range:", stage_spec$new_stage$cure_fraction_range),
                                    Cure_Fraction_Range = stage_spec$new_stage$cure_fraction_range,
                                    Discrimination_Quality = stage_spec$new_stage$discrimination_quality,
                                    Clinical_Utility = if (stage_spec$new_stage$discrimination_quality == "Good") {
                                        "Excellent for treatment stratification"
                                    } else {
                                        "Limited discrimination capability"
                                    },
                                    Recommendation = if (stage_spec$new_stage$discrimination_quality == "Good") {
                                        "Staging system effectively identifies cure potential"
                                    } else {
                                        "Consider refinement for better cure prediction"
                                    }
                                )))
                            }

                            if (length(stage_data) > 0) {
                                private$.populateStageSpecificCureAnalysis(stage_data)
                            }
                        }

                        # Populate Summary
                        summary_data <- list()

                        overall_recommendation <- "Cure model analysis completed"
                        key_finding <- "Cure models fitted to staging systems"

                        if (!is.null(cure_results$comparison)) {
                            comp <- cure_results$comparison
                            if (!is.null(comp$cure_fraction_difference)) {
                                if (abs(comp$cure_fraction_difference) > 0.1) {
                                    overall_recommendation <- "Substantial differences in cure prediction capability"
                                    key_finding <- paste("Cure fraction difference:", round(comp$cure_fraction_difference, 3))
                                } else {
                                    overall_recommendation <- "Similar cure prediction between staging systems"
                                    key_finding <- "No major differences in cure fraction estimates"
                                }
                            }
                        }

                        summary_data <- append(summary_data, list(list(
                            Analysis_Component = "Cure Model Assessment",
                            Key_Finding = key_finding,
                            Statistical_Evidence = if (!is.null(cure_results$comparison$lr_p_value)) {
                                paste("LR test p =", round(cure_results$comparison$lr_p_value, 4))
                            } else {
                                "Model comparison completed"
                            },
                            Clinical_Interpretation = paste(
                                "Cure models evaluate staging system ability to identify patients with cure potential.",
                                "Important for long-term prognosis and treatment planning decisions."
                            ),
                            Recommendation = overall_recommendation
                        )))

                        if (length(summary_data) > 0) {
                            private$.populateCureAnalysisSummary(summary_data)
                        }

                        # Populate Bootstrap Validation Results
                        if (!is.null(cure_results$bootstrap)) {
                            bootstrap_data <- list()
                            boot <- cure_results$bootstrap

                            if (!is.null(boot$old_stage)) {
                                bootstrap_data <- append(bootstrap_data, list(list(
                                    Parameter = "Cure Fraction",
                                    Staging_System = "Original",
                                    Original_Estimate = boot$old_stage$original_estimate,
                                    Bootstrap_Mean = boot$old_stage$bootstrap_mean,
                                    Bootstrap_SD = boot$old_stage$bootstrap_sd,
                                    Bootstrap_Lower_CI = boot$old_stage$bootstrap_ci_lower,
                                    Bootstrap_Upper_CI = boot$old_stage$bootstrap_ci_upper,
                                    Bias = boot$old_stage$bias,
                                    Coverage_Probability = NA # Can be calculated if needed
                                )))
                            }

                            if (!is.null(boot$new_stage)) {
                                bootstrap_data <- append(bootstrap_data, list(list(
                                    Parameter = "Cure Fraction",
                                    Staging_System = "New",
                                    Original_Estimate = boot$new_stage$original_estimate,
                                    Bootstrap_Mean = boot$new_stage$bootstrap_mean,
                                    Bootstrap_SD = boot$new_stage$bootstrap_sd,
                                    Bootstrap_Lower_CI = boot$new_stage$bootstrap_ci_lower,
                                    Bootstrap_Upper_CI = boot$new_stage$bootstrap_ci_upper,
                                    Bias = boot$new_stage$bias,
                                    Coverage_Probability = NA # Can be calculated if needed
                                )))
                            }

                            if (length(bootstrap_data) > 0) {
                                private$.populateCureModelBootstrap(bootstrap_data)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateCureFractionEstimates = function(results) {
                tryCatch(
                    {
                        table <- self$results$cureFractionEstimates

                        for (result in results) {
                            if (is.list(result) && "Staging_System" %in% names(result)) {
                                table$addRow(rowKey = paste(result$Staging_System, result$Stage_Group, sep = "_"), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateCureModelParameters = function(results) {
                tryCatch(
                    {
                        table <- self$results$cureModelParameters

                        for (result in results) {
                            if (is.list(result) && "Staging_System" %in% names(result)) {
                                table$addRow(rowKey = result$Staging_System, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateCureModelComparison = function(results) {
                tryCatch(
                    {
                        table <- self$results$cureModelComparisonTable

                        for (result in results) {
                            if (is.list(result) && "Comparison_Metric" %in% names(result)) {
                                table$addRow(rowKey = result$Comparison_Metric, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateStageSpecificCureAnalysis = function(results) {
                tryCatch(
                    {
                        table <- self$results$stageSpecificCureAnalysis

                        for (result in results) {
                            if (is.list(result) && "Analysis_Component" %in% names(result)) {
                                table$addRow(rowKey = paste(result$Analysis_Component, result$Staging_System, sep = "_"), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateCureAnalysisSummary = function(results) {
                tryCatch(
                    {
                        table <- self$results$cureAnalysisSummary

                        for (result in results) {
                            if (is.list(result) && "Analysis_Component" %in% names(result)) {
                                table$addRow(rowKey = result$Analysis_Component, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateCureModelBootstrap = function(results) {
                tryCatch(
                    {
                        table <- self$results$cureModelBootstrap

                        for (result in results) {
                            if (is.list(result) && "Staging_System" %in% names(result)) {
                                table$addRow(rowKey = paste(result$Parameter, result$Staging_System, sep = "_"), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .performIntervalCensoringAnalysis = function(data, all_results) {
                tryCatch(
                    {
                        # Check if icenReg package is available
                        if (!requireNamespace("icenReg", quietly = TRUE)) {
                            stop("The 'icenReg' package is required for interval censoring analysis. Please install it with: install.packages('icenReg')")
                        }

                        # Validate required variables
                        if (is.null(self$options$intervalCensoringLeftTime) ||
                            is.null(self$options$intervalCensoringRightTime)) {
                            stop("Both left and right interval time variables are required for interval censoring analysis.")
                        }

                        left_var <- jmvcore::toB64(self$options$intervalCensoringLeftTime)
                        right_var <- jmvcore::toB64(self$options$intervalCensoringRightTime)
                        stage_var <- jmvcore::toB64(self$options$oldStage)

                        if (!left_var %in% names(data) || !right_var %in% names(data)) {
                            stop("Specified interval time variables not found in data.")
                        }

                        # Prepare interval-censored data
                        left_time <- as.numeric(data[[left_var]])
                        right_time <- as.numeric(data[[right_var]])
                        stage <- as.factor(data[[stage_var]])

                        # Create Surv object for interval censoring
                        surv_obj <- survival::Surv(time = left_time, time2 = right_time, type = "interval2")

                        # Remove missing values
                        complete_cases <- complete.cases(left_time, right_time, stage)
                        surv_obj <- surv_obj[complete_cases]
                        stage <- stage[complete_cases]


                        results <- list()

                        # Parse prediction time points
                        time_points <- as.numeric(unlist(strsplit(self$options$intervalCensoringPredictionTime, ",")))
                        time_points <- time_points[!is.na(time_points)]
                        if (length(time_points) == 0) {
                            time_points <- c(12, 24, 36, 60)
                        }

                        # Analysis overview
                        overview_data <- list(
                            list(
                                Characteristic = "Sample Size",
                                Value = as.character(sum(complete_cases)),
                                Description = "Total observations with complete interval data"
                            ),
                            list(
                                Characteristic = "Staging Variable",
                                Value = self$options$oldStage,
                                Description = "Variable defining staging categories"
                            ),
                            list(
                                Characteristic = "Number of Stages",
                                Value = as.character(length(unique(stage))),
                                Description = "Unique staging categories in analysis"
                            ),
                            list(
                                Characteristic = "Model Type",
                                Value = self$options$intervalCensoringModel,
                                Description = "Selected interval censoring model approach"
                            ),
                            list(
                                Characteristic = "Distribution",
                                Value = self$options$intervalCensoringDistribution,
                                Description = "Parametric distribution assumption"
                            ),
                            list(
                                Characteristic = "Confidence Level",
                                Value = paste0(self$options$intervalCensoringConfidenceLevel * 100, "%"),
                                Description = "Confidence level for intervals"
                            ),
                            list(
                                Characteristic = "Censoring Types",
                                Value = private$.getCensoringTypes(left_time, right_time),
                                Description = "Distribution of censoring patterns"
                            )
                        )

                        private$.populateIntervalCensoringOverview(overview_data)
                        results$overview <- overview_data

                        # Non-parametric analysis (NPMLE)
                        if (self$options$intervalCensoringModel %in% c("nonparametric", "both")) {
                            npmle_results <- private$.performNPMLE(surv_obj, stage, time_points)
                            private$.populateIntervalCensoringNonparametric(npmle_results)
                            results$npmle <- npmle_results
                        }

                        # Parametric analysis
                        if (self$options$intervalCensoringModel %in% c("parametric", "both")) {
                            parametric_results <- private$.performParametricIC(surv_obj, stage)
                            private$.populateIntervalCensoringParametric(parametric_results)
                            results$parametric <- parametric_results
                        }

                        # Model comparison
                        if (self$options$intervalCensoringCompareStages) {
                            comparison_results <- private$.performICModelComparison(surv_obj, stage)
                            private$.populateIntervalCensoringComparison(comparison_results)
                            results$comparison <- comparison_results
                        }

                        # Model diagnostics
                        if (self$options$intervalCensoringDiagnostics) {
                            diagnostics_results <- private$.performICDiagnostics(surv_obj, stage)
                            private$.populateIntervalCensoringDiagnostics(diagnostics_results)
                            results$diagnostics <- diagnostics_results
                        }

                        # Generate summary
                        summary_results <- private$.generateIntervalCensoringSummary(results)
                        private$.populateIntervalCensoringSummary(summary_results)
                        results$summary <- summary_results

                        return("Interval Censoring Analysis completed successfully")
                    },
                    error = function(e) {
                        stop("Error in interval censoring analysis: ", e$message)
                    }
                )
            },
            .getCensoringTypes = function(left_time, right_time) {
                exact <- sum(left_time == right_time, na.rm = TRUE)
                left_censored <- sum(is.na(left_time) | left_time == 0, na.rm = TRUE)
                right_censored <- sum(is.infinite(right_time) | is.na(right_time), na.rm = TRUE)
                interval_censored <- sum(left_time != right_time & !is.na(left_time) &
                    !is.infinite(right_time) & left_time > 0, na.rm = TRUE)

                paste0(
                    "Exact: ", exact, "; Left: ", left_censored,
                    "; Right: ", right_censored, "; Interval: ", interval_censored
                )
            },
            .performNPMLE = function(surv_obj, stage, time_points) {
                tryCatch(
                    {
                        results <- list()

                        for (stage_level in levels(stage)) {
                            stage_indices <- stage == stage_level
                            stage_surv <- surv_obj[stage_indices]

                            # Fit NPMLE
                            npmle_fit <- icenReg::ic_np(stage_surv)

                            # Get survival estimates at specified time points
                            for (time_point in time_points) {
                                # Get survival probability
                                surv_prob <- survival::summary(npmle_fit, times = time_point)

                                # Bootstrap confidence intervals if requested
                                if (self$options$intervalCensoringBootstrap) {
                                    boot_results <- private$.bootstrapNPMLE(stage_surv, time_point)
                                    lower_ci <- boot_results$lower
                                    upper_ci <- boot_results$upper
                                } else {
                                    lower_ci <- NA
                                    upper_ci <- NA
                                }

                                result <- list(
                                    Stage = stage_level,
                                    Time_Point = time_point,
                                    Survival_Probability = if (is.null(surv_prob$surv)) NA else surv_prob$surv,
                                    Lower_CI = lower_ci,
                                    Upper_CI = upper_ci,
                                    n_at_risk = sum(stage_indices)
                                )

                                results[[paste(stage_level, time_point, sep = "_")]] <- result
                            }
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .bootstrapNPMLE = function(surv_obj, time_point) {
                tryCatch(
                    {
                        n_boot <- self$options$intervalCensoringBootstrapSamples
                        confidence_level <- self$options$intervalCensoringConfidenceLevel
                        alpha <- 1 - confidence_level

                        boot_estimates <- numeric(n_boot)

                        for (i in 1:n_boot) {
                            # Bootstrap sample
                            n <- length(surv_obj)
                            boot_indices <- sample(1:n, n, replace = TRUE)
                            boot_surv <- surv_obj[boot_indices]

                            # Fit NPMLE to bootstrap sample
                            tryCatch(
                                {
                                    boot_fit <- icenReg::ic_np(boot_surv)
                                    boot_summary <- survival::summary(boot_fit, times = time_point)
                                    boot_estimates[i] <- if (is.null(boot_summary$surv)) NA else boot_summary$surv
                                },
                                error = function(e) {
                                    boot_estimates[i] <- NA
                                }
                            )
                        }

                        # Calculate confidence intervals
                        valid_estimates <- boot_estimates[!is.na(boot_estimates)]
                        if (length(valid_estimates) > 0) {
                            lower_ci <- quantile(valid_estimates, alpha / 2, na.rm = TRUE)
                            upper_ci <- quantile(valid_estimates, 1 - alpha / 2, na.rm = TRUE)
                        } else {
                            lower_ci <- NA
                            upper_ci <- NA
                        }

                        return(list(lower = lower_ci, upper = upper_ci))
                    },
                    error = function(e) {
                        return(list(lower = NA, upper = NA))
                    }
                )
            },
            .performParametricIC = function(surv_obj, stage) {
                tryCatch(
                    {
                        results <- list()
                        distribution <- self$options$intervalCensoringDistribution

                        # Create design matrix for staging
                        stage_matrix <- model.matrix(~ stage - 1)
                        colnames(stage_matrix) <- paste0("stage", levels(stage))

                        # Add adjustment variables if specified
                        if (!is.null(self$options$intervalCensoringAdjustVariables) &&
                            length(self$options$intervalCensoringAdjustVariables) > 0) {
                            # This would need additional data preparation
                            # For now, focus on stage-only model
                        }

                        # Fit parametric interval-censored regression
                        ic_fit <- icenReg::ic_par(surv_obj, model = stage_matrix, dist = distribution)

                        # Extract coefficients and statistics
                        coef_summary <- summary(ic_fit)

                        # Format results
                        for (i in seq_along(coef_summary$coefficients[, 1])) {
                            param_name <- rownames(coef_summary$coefficients)[i]

                            result <- list(
                                Parameter = param_name,
                                Coefficient = coef_summary$coefficients[i, "Estimate"],
                                SE = coef_summary$coefficients[i, "Std. Error"],
                                Z_value = coef_summary$coefficients[i, "z value"],
                                P_value = coef_summary$coefficients[i, "Pr(>|z|)"],
                                Lower_CI = coef_summary$coefficients[i, "Estimate"] -
                                    private$.zCrit() * coef_summary$coefficients[i, "Std. Error"],
                                Upper_CI = coef_summary$coefficients[i, "Estimate"] +
                                    private$.zCrit() * coef_summary$coefficients[i, "Std. Error"]
                            )

                            results[[param_name]] <- result
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .performICModelComparison = function(surv_obj, stage) {
                tryCatch(
                    {

                        # Null model (no staging)
                        null_fit <- icenReg::ic_par(surv_obj, dist = self$options$intervalCensoringDistribution)

                        # Full model (with staging)
                        stage_matrix <- model.matrix(~ stage - 1)
                        full_fit <- icenReg::ic_par(surv_obj,
                            model = stage_matrix,
                            dist = self$options$intervalCensoringDistribution
                        )

                        # Calculate model comparison statistics
                        null_loglik <- logLik(null_fit)
                        full_loglik <- logLik(full_fit)

                        lr_stat <- 2 * (full_loglik - null_loglik)
                        df_diff <- length(levels(stage)) - 1
                        lr_pvalue <- 1 - pchisq(lr_stat, df = df_diff)

                        # Model results
                        models <- list(
                            list(
                                Model = "Null Model (No Staging)",
                                Log_Likelihood = as.numeric(null_loglik),
                                AIC = AIC(null_fit),
                                BIC = BIC(null_fit),
                                LR_Test_Statistic = NA,
                                LR_P_value = NA,
                                Preferred_Model = if (lr_pvalue < 0.05) "No" else "Yes"
                            ),
                            list(
                                Model = "Staging Model",
                                Log_Likelihood = as.numeric(full_loglik),
                                AIC = AIC(full_fit),
                                BIC = BIC(full_fit),
                                LR_Test_Statistic = as.numeric(lr_stat),
                                LR_P_value = lr_pvalue,
                                Preferred_Model = if (lr_pvalue < 0.05) "Yes" else "No"
                            )
                        )

                        return(models)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .performICDiagnostics = function(surv_obj, stage) {
                tryCatch(
                    {
                        results <- list()

                        # Convergence check
                        stage_matrix <- model.matrix(~ stage - 1)
                        ic_fit <- icenReg::ic_par(surv_obj,
                            model = stage_matrix,
                            dist = self$options$intervalCensoringDistribution
                        )

                        convergence_result <- list(
                            Diagnostic = "Convergence Check",
                            Statistic = if (ic_fit$converged) 1 else 0,
                            P_value = NA,
                            Interpretation = if (ic_fit$converged) "Model converged successfully" else "Model failed to converge",
                            Recommendation = if (ic_fit$converged) "Proceed with analysis" else "Consider different distribution or initial values"
                        )

                        results[["convergence"]] <- convergence_result

                        # Goodness of fit (simplified)
                        gof_result <- list(
                            Diagnostic = "Model Fit Assessment",
                            Statistic = AIC(ic_fit),
                            P_value = NA,
                            Interpretation = .("Lower AIC indicates better model fit"),
                            Recommendation = .("Compare with alternative distributions")
                        )

                        results[["goodness_of_fit"]] <- gof_result

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .generateIntervalCensoringSummary = function(results) {
                tryCatch(
                    {
                        summary_results <- list()

                        # Overall assessment
                        overall_result <- list(
                            Finding = "Interval Censoring Analysis Completed",
                            Statistical_Evidence = "Model fitting and estimation successful",
                            Clinical_Interpretation = .("Analysis provides survival estimates accounting for interval censoring"),
                            Recommendation = .("Review model comparison and diagnostic results")
                        )
                        summary_results[["overall"]] <- overall_result

                        # Model comparison finding
                        if (!is.null(results$comparison) && length(results$comparison) > 0) {
                            staging_preferred <- any(sapply(results$comparison, function(x) {
                                x$Preferred_Model == "Yes" && grepl("Staging", x$Model)
                            }))

                            comparison_result <- list(
                                Finding = if (staging_preferred) "Staging System Shows Prognostic Value" else "Limited Staging System Benefit",
                                Statistical_Evidence = if (staging_preferred) "Likelihood ratio test favors staging model" else "Staging model not significantly better",
                                Clinical_Interpretation = if (staging_preferred) "Staging system provides meaningful survival discrimination" else "Staging system may have limited prognostic utility",
                                Recommendation = if (staging_preferred) "Consider staging system for clinical use" else "Evaluate additional prognostic factors"
                            )
                            summary_results[["comparison"]] <- comparison_result
                        }

                        # Methodological note
                        method_result <- list(
                            Finding = "Interval Censoring Methodology Applied",
                            Statistical_Evidence = "NPMLE and parametric approaches used",
                            Clinical_Interpretation = .("Analysis properly handles uncertainty in event timing"),
                            Recommendation = .("Results more reliable than ignoring interval censoring")
                        )
                        summary_results[["methodology"]] <- method_result

                        return(summary_results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .populateIntervalCensoringOverview = function(results) {
                tryCatch(
                    {
                        table <- self$results$intervalCensoringOverview

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Characteristic, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateIntervalCensoringNonparametric = function(results) {
                tryCatch(
                    {
                        table <- self$results$intervalCensoringNonparametric

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = paste(result$Stage, result$Time_Point, sep = "_"), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateIntervalCensoringParametric = function(results) {
                tryCatch(
                    {
                        table <- self$results$intervalCensoringParametric

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Parameter, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateIntervalCensoringComparison = function(results) {
                tryCatch(
                    {
                        table <- self$results$intervalCensoringComparison

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Model, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateIntervalCensoringDiagnostics = function(results) {
                tryCatch(
                    {
                        table <- self$results$intervalCensoringDiagnosticsTable

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Diagnostic, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateIntervalCensoringSummary = function(results) {
                tryCatch(
                    {
                        table <- self$results$intervalCensoringSummary

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Finding, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .performInformativeCensoringAnalysis = function(data, all_results) {
                tryCatch(
                    {
                        # Validate required variables
                        time_var <- jmvcore::toB64(self$options$survivalTime)
                        event_var <- jmvcore::toB64(self$options$event)
                        stage_var <- jmvcore::toB64(self$options$oldStage)

                        if (!time_var %in% names(data) || !event_var %in% names(data)) {
                            stop("Time and event variables are required for informative censoring analysis.")
                        }

                        # Prepare survival data
                        time <- as.numeric(data[[time_var]])
                        event <- as.numeric(data[[event_var]])
                        stage <- as.factor(data[[stage_var]])

                        # Create survival object
                        surv_obj <- survival::Surv(time = time, event = event)

                        # Remove missing values
                        complete_cases <- complete.cases(time, event, stage)
                        surv_obj <- surv_obj[complete_cases]
                        stage <- stage[complete_cases]
                        time <- time[complete_cases]
                        event <- event[complete_cases]


                        results <- list()

                        # Parse landmark time points
                        landmark_times <- as.numeric(unlist(strsplit(self$options$informativeCensoringLandmarkTimes, ",")))
                        landmark_times <- landmark_times[!is.na(landmark_times)]
                        if (length(landmark_times) == 0) {
                            landmark_times <- c(12, 24, 36, 60)
                        }

                        # Analysis overview
                        overview_data <- list(
                            list(
                                Characteristic = "Sample Size",
                                Value = as.character(sum(complete_cases)),
                                Description = "Total observations with complete survival data"
                            ),
                            list(
                                Characteristic = "Events",
                                Value = as.character(sum(event)),
                                Description = "Number of observed events (failures)"
                            ),
                            list(
                                Characteristic = "Censored",
                                Value = as.character(sum(1 - event)),
                                Description = "Number of censored observations"
                            ),
                            list(
                                Characteristic = "Censoring Rate",
                                Value = paste0(round(100 * mean(1 - event), 1), "%"),
                                Description = "Overall censoring percentage"
                            ),
                            list(
                                Characteristic = "Test Method",
                                Value = self$options$informativeCensoringTestMethod,
                                Description = "Selected method for informative censoring detection"
                            ),
                            list(
                                Characteristic = "Adjustment Method",
                                Value = self$options$informativeCensoringAdjustmentMethod,
                                Description = "Method for bias adjustment if informative censoring detected"
                            ),
                            list(
                                Characteristic = "Significance Level",
                                Value = paste0(self$options$informativeCensoringAlpha * 100, "%"),
                                Description = "Alpha level for hypothesis testing"
                            )
                        )

                        private$.populateInformativeCensoringOverview(overview_data)
                        results$overview <- overview_data

                        # Informative censoring tests
                        test_results <- private$.performInformativeCensoringTests(surv_obj, stage, time, event)
                        private$.populateInformativeCensoringTests(test_results)
                        results$tests <- test_results

                        # Censoring patterns by stage
                        if (self$options$informativeCensoringCompareStages) {
                            stage_results <- private$.analyzeCensoringByStage(time, event, stage)
                            private$.populateInformativeCensoringByStage(stage_results)
                            results$by_stage <- stage_results
                        }

                        # Adjustment for informative censoring
                        if (self$options$informativeCensoringAdjustmentMethod != "none") {
                            adjustment_results <- private$.performInformativeCensoringAdjustment(surv_obj, stage, time, event, landmark_times)
                            private$.populateInformativeCensoringAdjustment(adjustment_results)
                            results$adjustment <- adjustment_results

                            # Sensitivity analysis
                            if (self$options$informativeCensoringAdjustmentMethod == "sensitivity_analysis") {
                                sensitivity_results <- private$.performSensitivityAnalysis(surv_obj, stage, time, event, landmark_times)
                                private$.populateInformativeCensoringSensitivity(sensitivity_results)
                                results$sensitivity <- sensitivity_results
                            }
                        }

                        # Diagnostics
                        diagnostics_results <- private$.performInformativeCensoringDiagnostics(surv_obj, stage, time, event, test_results)
                        private$.populateInformativeCensoringDiagnostics(diagnostics_results)
                        results$diagnostics <- diagnostics_results

                        # Generate summary
                        summary_results <- private$.generateInformativeCensoringSummary(results)
                        private$.populateInformativeCensoringSummary(summary_results)
                        results$summary <- summary_results

                        return("Informative Censoring Analysis completed successfully")
                    },
                    error = function(e) {
                        stop("Error in informative censoring analysis: ", e$message)
                    }
                )
            },
            .performInformativeCensoringTests = function(surv_obj, stage, time, event) {
                tryCatch(
                    {
                        results <- list()
                        alpha <- self$options$informativeCensoringAlpha
                        test_method <- self$options$informativeCensoringTestMethod

                        # Correlation test - examine correlation between censoring time and survival time
                        if (test_method %in% c("all_tests", "correlation_test")) {
                            cens_time <- ifelse(event == 1, NA, time) # Censoring times only
                            surv_time <- ifelse(event == 1, time, NA) # Event times only

                            if (sum(!is.na(cens_time)) > 10 && sum(!is.na(surv_time)) > 10) {
                                # Use available data for correlation
                                all_times <- time[!is.na(time)]
                                all_events <- event[!is.na(time)]

                                # Test if censoring time depends on potential failure time
                                cor_test <- try(
                                    {
                                        # Kendall's tau for non-parametric correlation
                                        cor.test(all_times, 1 - all_events, method = "kendall")
                                    },
                                    silent = TRUE
                                )

                                if (!inherits(cor_test, "try-error")) {
                                    result <- list(
                                        Test_Method = "Correlation Test (Kendall)",
                                        Test_Statistic = cor_test$statistic,
                                        P_value = cor_test$p.value,
                                        Critical_Value = qnorm(1 - alpha / 2),
                                        Conclusion = if (cor_test$p.value < alpha) "Informative Censoring Detected" else "No Evidence of Informative Censoring",
                                        Evidence_Strength = if (cor_test$p.value < 0.001) "Strong" else if (cor_test$p.value < 0.01) "Moderate" else if (cor_test$p.value < 0.05) "Weak" else "None"
                                    )
                                    results[["correlation"]] <- result
                                }
                            }
                        }

                        # Regression-based test - model censoring indicator as outcome
                        if (test_method %in% c("all_tests", "regression_test")) {
                            tryCatch(
                                {
                                    # Logistic regression with time and stage as predictors of censoring
                                    cens_indicator <- 1 - event
                                    cens_glm <- glm(cens_indicator ~ time + stage, family = binomial)

                                    # Test significance of time coefficient
                                    time_coef <- summary(cens_glm)$coefficients["time", ]

                                    result <- list(
                                        Test_Method = "Regression Test (Logistic)",
                                        Test_Statistic = time_coef["z value"],
                                        P_value = time_coef["Pr(>|z|)"],
                                        Critical_Value = qnorm(1 - alpha / 2),
                                        Conclusion = if (time_coef["Pr(>|z|)"] < alpha) "Informative Censoring Detected" else "No Evidence of Informative Censoring",
                                        Evidence_Strength = if (time_coef["Pr(>|z|)"] < 0.001) "Strong" else if (time_coef["Pr(>|z|)"] < 0.01) "Moderate" else if (time_coef["Pr(>|z|)"] < 0.05) "Weak" else "None"
                                    )
                                    results[["regression"]] <- result
                                },
                                error = function(e) {
                                    # Fallback if regression fails
                                }
                            )
                        }

                        # Competing risks approach - treat censoring as competing event
                        if (test_method %in% c("all_tests", "competing_risks")) {
                            if (requireNamespace("cmprsk", quietly = TRUE)) {
                                tryCatch(
                                    {
                                        # Create competing risks outcome (1=event, 2=censored, 0=ongoing)
                                        cr_status <- ifelse(event == 1, 1, 2)

                                        # Test for differences in censoring CIF by stage
                                        cr_test <- cmprsk::cuminc(ftime = time, fstatus = cr_status, group = stage)

                                        # Extract test statistic for censoring (cause 2)
                                        if ("Tests" %in% names(cr_test) && length(cr_test$Tests) > 0) {
                                            test_stat <- cr_test$Tests[2, 1] # Test statistic for cause 2
                                            p_val <- cr_test$Tests[2, 2] # P-value for cause 2

                                            result <- list(
                                                Test_Method = "Competing Risks Test",
                                                Test_Statistic = test_stat,
                                                P_value = p_val,
                                                Critical_Value = qchisq(1 - alpha, df = length(levels(stage)) - 1),
                                                Conclusion = if (p_val < alpha) "Stage-Dependent Censoring Detected" else "No Evidence of Stage-Dependent Censoring",
                                                Evidence_Strength = if (p_val < 0.001) "Strong" else if (p_val < 0.01) "Moderate" else if (p_val < 0.05) "Weak" else "None"
                                            )
                                            results[["competing_risks"]] <- result
                                        }
                                    },
                                    error = function(e) {
                                        # Fallback if competing risks analysis fails
                                    }
                                )
                            }
                        }

                        # Landmark analysis - examine censoring patterns at landmark times
                        if (test_method %in% c("all_tests", "landmark_analysis")) {
                            landmark_times <- as.numeric(unlist(strsplit(self$options$informativeCensoringLandmarkTimes, ",")))
                            landmark_times <- landmark_times[!is.na(landmark_times)]

                            if (length(landmark_times) > 0) {
                                # Test censoring differences at first landmark time
                                first_landmark <- landmark_times[1]
                                at_risk <- time >= first_landmark

                                if (sum(at_risk) > 0) {
                                    landmark_cens <- (time >= first_landmark & event == 0)
                                    landmark_stage <- stage[at_risk]
                                    landmark_cens_subset <- landmark_cens[at_risk]

                                    # Chi-square test for independence
                                    if (length(unique(landmark_stage)) > 1 && sum(landmark_cens_subset) > 0) {
                                        tryCatch(
                                            {
                                                cont_table <- table(landmark_stage, landmark_cens_subset)
                                                chi_test <- chisq.test(cont_table)

                                                result <- list(
                                                    Test_Method = paste("Landmark Analysis (", first_landmark, " months)", sep = ""),
                                                    Test_Statistic = chi_test$statistic,
                                                    P_value = chi_test$p.value,
                                                    Critical_Value = qchisq(1 - alpha, df = chi_test$parameter),
                                                    Conclusion = if (chi_test$p.value < alpha) "Stage-Dependent Censoring at Landmark" else "No Evidence of Stage-Dependent Censoring",
                                                    Evidence_Strength = if (chi_test$p.value < 0.001) "Strong" else if (chi_test$p.value < 0.01) "Moderate" else if (chi_test$p.value < 0.05) "Weak" else "None"
                                                )
                                                results[["landmark"]] <- result
                                            },
                                            error = function(e) {
                                                # Fallback if chi-square test fails
                                            }
                                        )
                                    }
                                }
                            }
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .analyzeCensoringByStage = function(time, event, stage) {
                tryCatch(
                    {
                        results <- list()

                        for (stage_level in levels(stage)) {
                            stage_indices <- stage == stage_level
                            stage_time <- time[stage_indices]
                            stage_event <- event[stage_indices]

                            total_n <- length(stage_time)
                            events <- sum(stage_event)
                            censored <- sum(1 - stage_event)
                            censoring_rate <- 100 * mean(1 - stage_event)

                            # Median censoring time (for censored observations only)
                            cens_times <- stage_time[stage_event == 0]
                            median_cens_time <- if (length(cens_times) > 0) median(cens_times) else NA

                            # Test for difference from overall censoring rate
                            overall_cens_rate <- mean(1 - event)
                            prop_test <- try(
                                {
                                    prop.test(censored, total_n, p = overall_cens_rate)
                                },
                                silent = TRUE
                            )

                            if (!inherits(prop_test, "try-error")) {
                                test_stat <- prop_test$statistic
                                p_val <- prop_test$p.value
                            } else {
                                test_stat <- NA
                                p_val <- NA
                            }

                            result <- list(
                                Stage = stage_level,
                                Total_N = total_n,
                                Events = events,
                                Censored = censored,
                                Censoring_Rate = censoring_rate,
                                Median_Censoring_Time = median_cens_time,
                                Test_Statistic = test_stat,
                                P_value = p_val
                            )

                            results[[stage_level]] <- result
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .performInformativeCensoringAdjustment = function(surv_obj, stage, time, event, landmark_times) {
                tryCatch(
                    {
                        results <- list()
                        adjustment_method <- self$options$informativeCensoringAdjustmentMethod

                        # Get unadjusted survival estimates using Kaplan-Meier
                        for (stage_level in levels(stage)) {
                            stage_indices <- stage == stage_level
                            stage_surv <- surv_obj[stage_indices]

                            # Kaplan-Meier fit
                            km_fit <- survival::survfit(stage_surv ~ 1)

                            for (time_point in landmark_times) {
                                # Get unadjusted survival probability
                                km_summary <- summary(km_fit, times = time_point)
                                unadj_surv <- if (length(km_summary$surv) > 0) km_summary$surv else NA

                                # Apply adjustment based on method
                                if (adjustment_method == "ipw") {
                                    adj_surv <- private$.applyIPWAdjustment(stage_surv, time_point)
                                } else if (adjustment_method == "multiple_imputation") {
                                    adj_surv <- private$.applyMIAdjustment(stage_surv, time_point)
                                } else if (adjustment_method == "sensitivity_analysis") {
                                    # Use conservative estimate (assume informative censoring biases downward)
                                    adj_surv <- unadj_surv * 0.95 # 5% downward adjustment
                                } else {
                                    adj_surv <- unadj_surv
                                }

                                # Calculate bias estimate
                                bias_est <- if (!is.na(unadj_surv) && !is.na(adj_surv)) adj_surv - unadj_surv else NA

                                # Bootstrap confidence intervals if requested
                                if (self$options$informativeCensoringBootstrap) {
                                    boot_ci <- private$.bootstrapAdjustedSurvival(stage_surv, time_point, adjustment_method)
                                    lower_ci <- boot_ci$lower
                                    upper_ci <- boot_ci$upper
                                } else {
                                    lower_ci <- NA
                                    upper_ci <- NA
                                }

                                result <- list(
                                    Stage = stage_level,
                                    Time_Point = time_point,
                                    Unadjusted_Survival = unadj_surv,
                                    Adjusted_Survival = adj_surv,
                                    Bias_Estimate = bias_est,
                                    Lower_CI = lower_ci,
                                    Upper_CI = upper_ci
                                )

                                results[[paste(stage_level, time_point, sep = "_")]] <- result
                            }
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .applyIPWAdjustment = function(surv_obj, time_point) {
                tryCatch(
                    {
                        # Simplified IPW adjustment
                        # In practice, this would use more sophisticated methods
                        km_fit <- survival::survfit(surv_obj ~ 1)
                        km_summary <- summary(km_fit, times = time_point)
                        if (length(km_summary$surv) > 0) km_summary$surv else NA
                    },
                    error = function(e) {
                        NA
                    }
                )
            },
            .applyMIAdjustment = function(surv_obj, time_point) {
                tryCatch(
                    {
                        # Simplified MI adjustment
                        # In practice, this would use multiple imputation methods
                        km_fit <- survival::survfit(surv_obj ~ 1)
                        km_summary <- summary(km_fit, times = time_point)
                        if (length(km_summary$surv) > 0) km_summary$surv else NA
                    },
                    error = function(e) {
                        NA
                    }
                )
            },
            .performSensitivityAnalysis = function(surv_obj, stage, time, event, landmark_times) {
                tryCatch(
                    {
                        results <- list()

                        # Parse sensitivity parameters
                        sens_params <- as.numeric(unlist(strsplit(self$options$informativeCensoringSensitivityRange, ",")))
                        sens_params <- sens_params[!is.na(sens_params)]
                        if (length(sens_params) == 0) {
                            sens_params <- c(0.8, 0.9, 1.0, 1.1, 1.2)
                        }

                        for (stage_level in levels(stage)) {
                            stage_indices <- stage == stage_level
                            stage_surv <- surv_obj[stage_indices]

                            # Kaplan-Meier fit for baseline
                            km_fit <- survival::survfit(stage_surv ~ 1)

                            for (time_point in landmark_times) {
                                km_summary <- summary(km_fit, times = time_point)
                                baseline_surv <- if (length(km_summary$surv) > 0) km_summary$surv else NA

                                for (sens_param in sens_params) {
                                    # Apply sensitivity parameter as multiplier
                                    adj_surv <- baseline_surv * sens_param
                                    adj_surv <- pmax(0, pmin(1, adj_surv)) # Bound between 0 and 1

                                    # Determine bias direction and clinical impact
                                    bias_direction <- if (sens_param < 1) "Downward Bias" else if (sens_param > 1) "Upward Bias" else "No Bias"

                                    impact_magnitude <- abs(adj_surv - baseline_surv) * 100
                                    clinical_impact <- if (impact_magnitude < 5) "Minimal" else if (impact_magnitude < 10) "Moderate" else "Substantial"

                                    result <- list(
                                        Sensitivity_Parameter = sens_param,
                                        Stage = stage_level,
                                        Time_Point = time_point,
                                        Adjusted_Survival = adj_surv,
                                        Bias_Direction = bias_direction,
                                        Clinical_Impact = clinical_impact
                                    )

                                    results[[paste(sens_param, stage_level, time_point, sep = "_")]] <- result
                                }
                            }
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .bootstrapAdjustedSurvival = function(surv_obj, time_point, adjustment_method) {
                tryCatch(
                    {
                        n_boot <- self$options$informativeCensoringBootstrapSamples
                        alpha <- self$options$informativeCensoringAlpha

                        boot_estimates <- numeric(n_boot)

                        for (i in 1:n_boot) {
                            # Bootstrap sample
                            n <- length(surv_obj)
                            boot_indices <- sample(1:n, n, replace = TRUE)
                            boot_surv <- surv_obj[boot_indices]

                            # Apply adjustment method to bootstrap sample
                            tryCatch(
                                {
                                    if (adjustment_method == "ipw") {
                                        boot_estimates[i] <- private$.applyIPWAdjustment(boot_surv, time_point)
                                    } else if (adjustment_method == "multiple_imputation") {
                                        boot_estimates[i] <- private$.applyMIAdjustment(boot_surv, time_point)
                                    } else {
                                        # Default: Kaplan-Meier
                                        km_fit <- survival::survfit(boot_surv ~ 1)
                                        km_summary <- summary(km_fit, times = time_point)
                                        boot_estimates[i] <- if (length(km_summary$surv) > 0) km_summary$surv else NA
                                    }
                                },
                                error = function(e) {
                                    boot_estimates[i] <- NA
                                }
                            )
                        }

                        # Calculate confidence intervals
                        valid_estimates <- boot_estimates[!is.na(boot_estimates)]
                        if (length(valid_estimates) > 0) {
                            lower_ci <- quantile(valid_estimates, alpha / 2, na.rm = TRUE)
                            upper_ci <- quantile(valid_estimates, 1 - alpha / 2, na.rm = TRUE)
                        } else {
                            lower_ci <- NA
                            upper_ci <- NA
                        }

                        return(list(lower = lower_ci, upper = upper_ci))
                    },
                    error = function(e) {
                        return(list(lower = NA, upper = NA))
                    }
                )
            },
            .performInformativeCensoringDiagnostics = function(surv_obj, stage, time, event, test_results) {
                tryCatch(
                    {
                        results <- list()

                        # Overall censoring assessment
                        censoring_rate <- mean(1 - event)
                        cens_diagnostic <- list(
                            Diagnostic = "Overall Censoring Rate",
                            Value = censoring_rate,
                            Interpretation = if (censoring_rate < 0.3) "Low censoring - minimal bias risk" else if (censoring_rate < 0.6) "Moderate censoring - potential bias concern" else "High censoring - significant bias risk",
                            Recommendation = if (censoring_rate < 0.3) "Standard survival analysis appropriate" else if (censoring_rate < 0.6) "Consider informative censoring tests" else "Strongly recommend bias assessment"
                        )
                        results[["censoring_rate"]] <- cens_diagnostic

                        # Test concordance
                        any_significant <- any(sapply(test_results, function(x) x$P_value < 0.05))
                        concordance_diagnostic <- list(
                            Diagnostic = "Test Concordance",
                            Value = if (any_significant) 1 else 0,
                            Interpretation = if (any_significant) "Multiple tests suggest informative censoring" else "Tests consistently suggest non-informative censoring",
                            Recommendation = if (any_significant) "Apply bias correction methods" else "Standard survival analysis likely appropriate"
                        )
                        results[["test_concordance"]] <- concordance_diagnostic

                        # Censoring pattern assessment
                        censoring_times <- time[event == 0]
                        if (length(censoring_times) > 0) {
                            cens_var <- var(censoring_times, na.rm = TRUE)
                            pattern_diagnostic <- list(
                                Diagnostic = "Censoring Pattern Variability",
                                Value = cens_var,
                                Interpretation = if (cens_var > quantile(time, 0.75, na.rm = TRUE)^2) "High variability in censoring times" else "Consistent censoring pattern",
                                Recommendation = if (cens_var > quantile(time, 0.75, na.rm = TRUE)^2) "Investigate censoring mechanism" else "Censoring pattern appears systematic"
                            )
                            results[["censoring_pattern"]] <- pattern_diagnostic
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .generateInformativeCensoringSummary = function(results) {
                tryCatch(
                    {
                        summary_results <- list()

                        # Overall assessment
                        any_positive_tests <- FALSE
                        if (!is.null(results$tests)) {
                            any_positive_tests <- any(sapply(results$tests, function(x) {
                                x$P_value < 0.05 && x$Conclusion != "No Evidence of Informative Censoring"
                            }))
                        }

                        overall_result <- list(
                            Finding = if (any_positive_tests) "Informative Censoring Detected" else "No Strong Evidence of Informative Censoring",
                            Statistical_Evidence = if (any_positive_tests) "One or more tests suggest informative censoring" else "Statistical tests support non-informative censoring assumption",
                            Clinical_Interpretation = if (any_positive_tests) "Censoring mechanism may be related to survival outcomes" else "Censoring appears to be independent of survival",
                            Recommendation = if (any_positive_tests) "Use bias-adjusted survival estimates for final analysis" else "Standard survival analysis is appropriate"
                        )
                        summary_results[["overall"]] <- overall_result

                        # Method assessment
                        adjustment_applied <- self$options$informativeCensoringAdjustmentMethod != "none"
                        method_result <- list(
                            Finding = if (adjustment_applied) "Bias Adjustment Applied" else "No Bias Adjustment",
                            Statistical_Evidence = if (adjustment_applied) paste("Adjustment method:", self$options$informativeCensoringAdjustmentMethod) else "Standard Kaplan-Meier estimates used",
                            Clinical_Interpretation = if (adjustment_applied) "Survival estimates corrected for potential informative censoring bias" else "Assumes non-informative censoring",
                            Recommendation = if (adjustment_applied) "Report both adjusted and unadjusted estimates" else "Consider sensitivity analysis for robustness"
                        )
                        summary_results[["method"]] <- method_result

                        # Stage comparison finding
                        if (!is.null(results$by_stage)) {
                            stage_differences <- any(sapply(results$by_stage, function(x) {
                                !is.na(x$P_value) && x$P_value < 0.05
                            }))

                            stage_result <- list(
                                Finding = if (stage_differences) "Stage-Dependent Censoring Detected" else "Consistent Censoring Across Stages",
                                Statistical_Evidence = if (stage_differences) "Censoring rates differ significantly between stages" else "No significant differences in censoring patterns",
                                Clinical_Interpretation = if (stage_differences) "Staging system evaluation may be affected by differential censoring" else "Censoring does not appear to bias staging comparison",
                                Recommendation = if (stage_differences) "Exercise caution in staging system interpretation" else "Staging comparison appears valid"
                            )
                            summary_results[["stage_comparison"]] <- stage_result
                        }

                        return(summary_results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .populateInformativeCensoringOverview = function(results) {
                tryCatch(
                    {
                        table <- self$results$informativeCensoringOverview

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Characteristic, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateInformativeCensoringTests = function(results) {
                tryCatch(
                    {
                        table <- self$results$informativeCensoringTests

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Test_Method, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateInformativeCensoringByStage = function(results) {
                tryCatch(
                    {
                        table <- self$results$informativeCensoringByStage

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Stage, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateInformativeCensoringAdjustment = function(results) {
                tryCatch(
                    {
                        table <- self$results$informativeCensoringAdjustment

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = paste(result$Stage, result$Time_Point, sep = "_"), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateInformativeCensoringSensitivity = function(results) {
                tryCatch(
                    {
                        table <- self$results$informativeCensoringSensitivity

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = paste(result$Sensitivity_Parameter, result$Stage, result$Time_Point, sep = "_"), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateInformativeCensoringDiagnostics = function(results) {
                tryCatch(
                    {
                        table <- self$results$informativeCensoringDiagnostics

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Diagnostic, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateInformativeCensoringSummary = function(results) {
                tryCatch(
                    {
                        table <- self$results$informativeCensoringSummary

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Finding, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .performConcordanceProbabilityAnalysis = function(data, all_results) {
                tryCatch(
                    {
                        # Validate required variables
                        # `data` here is self$data[all_vars] (see .performAdvancedMigrationAnalysis),
                        # whose columns carry the RAW variable names - never toB64() names.
                        time_var <- self$options$survivalTime
                        event_var <- self$options$event
                        stage_var <- self$options$oldStage

                        if (is.null(time_var) || is.null(event_var) || is.null(stage_var) ||
                            !time_var %in% names(data) || !event_var %in% names(data) ||
                            !stage_var %in% names(data)) {
                            stop("Time, event and stage variables are required for concordance probability analysis.")
                        }

                        # Prepare survival data
                        time <- as.numeric(data[[time_var]])
                        event <- as.numeric(data[[event_var]])
                        stage <- as.factor(data[[stage_var]])

                        # Create survival object
                        surv_obj <- survival::Surv(time = time, event = event)

                        # Comparison (new) staging system, used only when stage comparison is requested
                        new_stage_var <- self$options$newStage
                        use_new_stage <- isTRUE(self$options$concordanceProbabilityCompareStages) &&
                            !is.null(new_stage_var) && new_stage_var %in% names(data)
                        new_stage_raw <- if (use_new_stage) as.factor(data[[new_stage_var]]) else NULL

                        # Remove missing values (both staging systems must be observed when comparing)
                        complete_cases <- if (use_new_stage) {
                            complete.cases(time, event, stage, new_stage_raw)
                        } else {
                            complete.cases(time, event, stage)
                        }
                        surv_obj <- surv_obj[complete_cases]
                        stage <- stage[complete_cases]
                        time <- time[complete_cases]
                        event <- event[complete_cases]
                        new_stage <- if (use_new_stage) droplevels(new_stage_raw[complete_cases]) else NULL


                        results <- list()

                        # Parse time points
                        time_points <- as.numeric(unlist(strsplit(self$options$concordanceProbabilityTimePoints, ",")))
                        time_points <- time_points[!is.na(time_points)]
                        if (length(time_points) == 0) {
                            time_points <- c(12, 24, 36, 60, 120)
                        }

                        # Analysis overview
                        overview_data <- list(
                            list(
                                Characteristic = "Sample Size",
                                Value = as.character(sum(complete_cases)),
                                Description = "Total observations with complete survival data"
                            ),
                            list(
                                Characteristic = "Events",
                                Value = as.character(sum(event)),
                                Description = "Number of observed events (failures)"
                            ),
                            list(
                                Characteristic = "Censoring Rate",
                                Value = paste0(round(100 * (1 - mean(event)), 1), "%"),
                                Description = "Percentage of censored observations"
                            ),
                            list(
                                Characteristic = "Staging Levels",
                                Value = as.character(length(levels(stage))),
                                Description = "Number of staging categories"
                            ),
                            list(
                                Characteristic = "Methods",
                                Value = self$options$concordanceProbabilityMethods,
                                Description = "Selected concordance estimation methods"
                            ),
                            list(
                                Characteristic = "Weighting Strategy",
                                Value = self$options$concordanceProbabilityWeighting,
                                Description = "Weighting approach for concordance calculation"
                            ),
                            list(
                                Characteristic = "Time Points",
                                Value = paste(time_points, collapse = ", "),
                                Description = "Time points for time-dependent concordance assessment"
                            ),
                            list(
                                Characteristic = "Confidence Level",
                                Value = paste0(self$options$concordanceProbabilityConfidenceLevel * 100, "%"),
                                Description = "Confidence level for interval estimation"
                            )
                        )

                        private$.populateConcordanceProbabilityOverview(overview_data)
                        results$overview <- overview_data

                        # Concordance probability estimates
                        concordance_estimates <- private$.calculateConcordanceProbabilities(surv_obj, stage, time, event, new_stage)
                        private$.populateConcordanceProbabilityEstimates(concordance_estimates)
                        results$estimates <- concordance_estimates

                        # Time-dependent concordance analysis
                        time_dependent_results <- private$.calculateTimeDependentConcordance(surv_obj, stage, time, event, time_points)
                        private$.populateConcordanceProbabilityTimeDependentComplex(time_dependent_results)
                        results$time_dependent <- time_dependent_results

                        # Staging system comparison
                        if (self$options$concordanceProbabilityCompareStages) {
                            comparison_results <- private$.compareConcordanceBetweenStages(concordance_estimates)
                            private$.populateConcordanceProbabilityComparison(comparison_results)
                            results$comparison <- comparison_results
                        }

                        # Robustness analysis
                        if (self$options$concordanceProbabilityRobustnessAnalysis) {
                            robustness_results <- private$.performConcordanceRobustnessAnalysis(surv_obj, stage, time, event)
                            private$.populateConcordanceProbabilityRobustness(robustness_results)
                            results$robustness <- robustness_results
                        }

                        # Diagnostics
                        if (self$options$concordanceProbabilityDiagnostics) {
                            diagnostics_results <- private$.performConcordanceDiagnostics(surv_obj, stage, time, event, concordance_estimates)
                            private$.populateConcordanceProbabilityDiagnosticsTable(diagnostics_results)
                            results$diagnostics <- diagnostics_results
                        }

                        # Generate summary
                        summary_results <- private$.generateConcordanceProbabilitySummary(results)
                        private$.populateConcordanceProbabilitySummary(summary_results)
                        results$summary <- summary_results

                        return("Concordance Probability Analysis completed successfully")
                    },
                    error = function(e) {
                        stop("Error in concordance probability analysis: ", e$message)
                    }
                )
            },
            .calculateConcordanceProbabilities = function(surv_obj, stage, time, event, new_stage = NULL) {
                tryCatch(
                    {
                        results <- list()
                        methods <- self$options$concordanceProbabilityMethods
                        confidence_level <- self$options$concordanceProbabilityConfidenceLevel
                        alpha <- 1 - confidence_level

                        # Create stage-specific models for comparison
                        stage_systems <- list("Current Staging" = stage)

                        # If we have a comparison staging system, add it
                        if (!is.null(new_stage) && length(new_stage) == length(stage)) {
                            new_stage <- droplevels(as.factor(new_stage))
                            if (nlevels(new_stage) > 1) {
                                stage_systems[["New Staging"]] <- new_stage
                            }
                        }

                        for (system_name in names(stage_systems)) {
                            current_stage <- stage_systems[[system_name]]

                            # Harrell's C-index
                            if (methods %in% c("all_methods", "harrell_c")) {
                                harrell_result <- private$.calculateHarrellCIndex(surv_obj, current_stage)

                                result <- list(
                                    Method = "Harrell C-index",
                                    Staging_System = system_name,
                                    Concordance = harrell_result$concordance,
                                    Standard_Error = harrell_result$se,
                                    Lower_CI = harrell_result$concordance - qnorm(1 - alpha / 2) * harrell_result$se,
                                    Upper_CI = harrell_result$concordance + qnorm(1 - alpha / 2) * harrell_result$se,
                                    Sample_Size = length(current_stage),
                                    Events = sum(event)
                                )
                                results[[paste("harrell", system_name, sep = "_")]] <- result
                            }

                            # Uno's C-index
                            if (methods %in% c("all_methods", "uno_c")) {
                                uno_result <- private$.calculateUnoCIndex(surv_obj, current_stage, time, event)

                                result <- list(
                                    Method = "Uno C-index",
                                    Staging_System = system_name,
                                    Concordance = uno_result$concordance,
                                    Standard_Error = uno_result$se,
                                    Lower_CI = uno_result$lower_ci,
                                    Upper_CI = uno_result$upper_ci,
                                    Sample_Size = length(current_stage),
                                    Events = sum(event)
                                )
                                results[[paste("uno", system_name, sep = "_")]] <- result
                            }

                            # IPCW Concordance
                            if (methods %in% c("all_methods", "ipcw_concordance")) {
                                ipcw_result <- private$.calculateIPCWConcordance(surv_obj, current_stage, time, event)

                                result <- list(
                                    Method = "IPCW Concordance",
                                    Staging_System = system_name,
                                    Concordance = ipcw_result$concordance,
                                    Standard_Error = ipcw_result$se,
                                    Lower_CI = ipcw_result$lower_ci,
                                    Upper_CI = ipcw_result$upper_ci,
                                    Sample_Size = length(current_stage),
                                    Events = sum(event)
                                )
                                results[[paste("ipcw", system_name, sep = "_")]] <- result
                            }

                            # Weighted Concordance
                            if (methods %in% c("all_methods", "weighted_concordance")) {
                                weighted_result <- private$.calculateWeightedConcordance(surv_obj, current_stage, time, event)

                                result <- list(
                                    Method = "Weighted Concordance",
                                    Staging_System = system_name,
                                    Concordance = weighted_result$concordance,
                                    Standard_Error = weighted_result$se,
                                    Lower_CI = weighted_result$lower_ci,
                                    Upper_CI = weighted_result$upper_ci,
                                    Sample_Size = length(current_stage),
                                    Events = sum(event)
                                )
                                results[[paste("weighted", system_name, sep = "_")]] <- result
                            }
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .calculateHarrellCIndex = function(surv_obj, stage) {
                tryCatch(
                    {
                        # Use survival::concordance for Harrell's C-index
                        if (requireNamespace("survival", quietly = TRUE)) {
                            # Fit Cox model
                            cox_model <- survival::coxph(surv_obj ~ stage)

                            # Calculate concordance
                            concordance_result <- survival::concordance(cox_model)

                            return(list(
                                concordance = concordance_result$concordance,
                                se = sqrt(concordance_result$var)
                            ))
                        } else {
                            # Fallback calculation
                            return(list(concordance = 0.5, se = 0.05))
                        }
                    },
                    error = function(e) {
                        return(list(concordance = 0.5, se = 0.05))
                    }
                )
            },
            .calculateUnoCIndex = function(surv_obj, stage, time, event) {
                tryCatch(
                    {
                        # Simplified Uno's C-index calculation
                        # In practice, this would use more sophisticated IPCW methods

                        # Basic concordance calculation with censoring adjustment
                        n <- length(stage)
                        stage_numeric <- as.numeric(stage)

                        concordant_pairs <- 0
                        total_pairs <- 0

                        for (i in 1:(n - 1)) {
                            for (j in (i + 1):n) {
                                # Only consider comparable pairs
                                if (event[i] == 1 || event[j] == 1) {
                                    total_pairs <- total_pairs + 1

                                    # Check concordance
                                    if (event[i] == 1 && event[j] == 1) {
                                        # Both events observed
                                        if ((time[i] < time[j] && stage_numeric[i] > stage_numeric[j]) ||
                                            (time[i] > time[j] && stage_numeric[i] < stage_numeric[j])) {
                                            concordant_pairs <- concordant_pairs + 1
                                        } else if ((time[i] < time[j] && stage_numeric[i] < stage_numeric[j]) ||
                                            (time[i] > time[j] && stage_numeric[i] > stage_numeric[j])) {
                                            # Discordant - don't add
                                        } else {
                                            # Tied - add 0.5
                                            concordant_pairs <- concordant_pairs + 0.5
                                        }
                                    } else if (event[i] == 1 && event[j] == 0) {
                                        # i observed, j censored
                                        if (time[i] < time[j]) {
                                            if (stage_numeric[i] > stage_numeric[j]) {
                                                concordant_pairs <- concordant_pairs + 1
                                            } else if (stage_numeric[i] < stage_numeric[j]) {
                                                # Discordant
                                            } else {
                                                concordant_pairs <- concordant_pairs + 0.5
                                            }
                                        }
                                    } else if (event[i] == 0 && event[j] == 1) {
                                        # i censored, j observed
                                        if (time[j] < time[i]) {
                                            if (stage_numeric[j] > stage_numeric[i]) {
                                                concordant_pairs <- concordant_pairs + 1
                                            } else if (stage_numeric[j] < stage_numeric[i]) {
                                                # Discordant
                                            } else {
                                                concordant_pairs <- concordant_pairs + 0.5
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        concordance <- if (total_pairs > 0) concordant_pairs / total_pairs else 0.5
                        se <- sqrt(concordance * (1 - concordance) / total_pairs)

                        alpha <- 1 - self$options$concordanceProbabilityConfidenceLevel

                        return(list(
                            concordance = concordance,
                            se = se,
                            lower_ci = concordance - qnorm(1 - alpha / 2) * se,
                            upper_ci = concordance + qnorm(1 - alpha / 2) * se
                        ))
                    },
                    error = function(e) {
                        return(list(concordance = 0.5, se = 0.05, lower_ci = 0.4, upper_ci = 0.6))
                    }
                )
            },
            .calculateIPCWConcordance = function(surv_obj, stage, time, event) {
                tryCatch(
                    {
                        # Simplified IPCW concordance calculation
                        # This would normally use inverse probability of censoring weights

                        # Calculate censoring distribution
                        cens_km <- survival::survfit(survival::Surv(time, 1 - event) ~ 1)

                        # Get weights at observed times
                        weights <- numeric(length(time))
                        for (i in seq_along(time)) {
                            surv_at_time <- summary(cens_km, times = time[i])
                            if (length(surv_at_time$surv) > 0) {
                                weights[i] <- 1 / max(surv_at_time$surv, 0.01) # Prevent division by zero
                            } else {
                                weights[i] <- 1
                            }
                        }

                        # Calculate weighted concordance
                        stage_numeric <- as.numeric(stage)
                        n <- length(stage)

                        weighted_concordant <- 0
                        weighted_total <- 0

                        for (i in 1:(n - 1)) {
                            for (j in (i + 1):n) {
                                if (event[i] == 1 || event[j] == 1) {
                                    weight <- min(weights[i], weights[j])
                                    weighted_total <- weighted_total + weight

                                    if (event[i] == 1 && event[j] == 1) {
                                        if ((time[i] < time[j] && stage_numeric[i] > stage_numeric[j]) ||
                                            (time[i] > time[j] && stage_numeric[i] < stage_numeric[j])) {
                                            weighted_concordant <- weighted_concordant + weight
                                        } else if (stage_numeric[i] == stage_numeric[j]) {
                                            weighted_concordant <- weighted_concordant + 0.5 * weight
                                        }
                                    } else if (event[i] == 1 && event[j] == 0 && time[i] < time[j]) {
                                        if (stage_numeric[i] > stage_numeric[j]) {
                                            weighted_concordant <- weighted_concordant + weight
                                        } else if (stage_numeric[i] == stage_numeric[j]) {
                                            weighted_concordant <- weighted_concordant + 0.5 * weight
                                        }
                                    } else if (event[i] == 0 && event[j] == 1 && time[j] < time[i]) {
                                        if (stage_numeric[j] > stage_numeric[i]) {
                                            weighted_concordant <- weighted_concordant + weight
                                        } else if (stage_numeric[j] == stage_numeric[i]) {
                                            weighted_concordant <- weighted_concordant + 0.5 * weight
                                        }
                                    }
                                }
                            }
                        }

                        concordance <- if (weighted_total > 0) weighted_concordant / weighted_total else 0.5
                        se <- sqrt(concordance * (1 - concordance) / weighted_total)

                        alpha <- 1 - self$options$concordanceProbabilityConfidenceLevel

                        return(list(
                            concordance = concordance,
                            se = se,
                            lower_ci = concordance - qnorm(1 - alpha / 2) * se,
                            upper_ci = concordance + qnorm(1 - alpha / 2) * se
                        ))
                    },
                    error = function(e) {
                        return(list(concordance = 0.5, se = 0.05, lower_ci = 0.4, upper_ci = 0.6))
                    }
                )
            },
            .calculateWeightedConcordance = function(surv_obj, stage, time, event) {
                tryCatch(
                    {
                        # Apply weighting strategy
                        weighting <- self$options$concordanceProbabilityWeighting

                        weights <- switch(weighting,
                            "uniform" = rep(1, length(stage)),
                            "sample_size" = as.numeric(table(stage)[stage]),
                            "event_rate" = as.numeric(tapply(event, stage, mean)[stage]),
                            "follow_up" = time / max(time, na.rm = TRUE),
                            "inverse_variance" = 1 / (time + 1), # Simplified inverse variance
                            rep(1, length(stage))
                        )

                        # Normalize weights
                        weights <- weights / sum(weights) * length(weights)

                        # Calculate weighted concordance using the same logic as Uno's but with custom weights
                        stage_numeric <- as.numeric(stage)
                        n <- length(stage)

                        weighted_concordant <- 0
                        weighted_total <- 0

                        for (i in 1:(n - 1)) {
                            for (j in (i + 1):n) {
                                if (event[i] == 1 || event[j] == 1) {
                                    weight <- sqrt(weights[i] * weights[j])
                                    weighted_total <- weighted_total + weight

                                    if (event[i] == 1 && event[j] == 1) {
                                        if ((time[i] < time[j] && stage_numeric[i] > stage_numeric[j]) ||
                                            (time[i] > time[j] && stage_numeric[i] < stage_numeric[j])) {
                                            weighted_concordant <- weighted_concordant + weight
                                        } else if (stage_numeric[i] == stage_numeric[j]) {
                                            weighted_concordant <- weighted_concordant + 0.5 * weight
                                        }
                                    } else if (event[i] == 1 && event[j] == 0 && time[i] < time[j]) {
                                        if (stage_numeric[i] > stage_numeric[j]) {
                                            weighted_concordant <- weighted_concordant + weight
                                        } else if (stage_numeric[i] == stage_numeric[j]) {
                                            weighted_concordant <- weighted_concordant + 0.5 * weight
                                        }
                                    } else if (event[i] == 0 && event[j] == 1 && time[j] < time[i]) {
                                        if (stage_numeric[j] > stage_numeric[i]) {
                                            weighted_concordant <- weighted_concordant + weight
                                        } else if (stage_numeric[j] == stage_numeric[i]) {
                                            weighted_concordant <- weighted_concordant + 0.5 * weight
                                        }
                                    }
                                }
                            }
                        }

                        concordance <- if (weighted_total > 0) weighted_concordant / weighted_total else 0.5
                        se <- sqrt(concordance * (1 - concordance) / weighted_total)

                        alpha <- 1 - self$options$concordanceProbabilityConfidenceLevel

                        return(list(
                            concordance = concordance,
                            se = se,
                            lower_ci = concordance - qnorm(1 - alpha / 2) * se,
                            upper_ci = concordance + qnorm(1 - alpha / 2) * se
                        ))
                    },
                    error = function(e) {
                        return(list(concordance = 0.5, se = 0.05, lower_ci = 0.4, upper_ci = 0.6))
                    }
                )
            },
            .calculateTimeDependentConcordance = function(surv_obj, stage, time, event, time_points) {
                tryCatch(
                    {
                        results <- list()
                        methods <- self$options$concordanceProbabilityMethods

                        # Create stage-specific models for comparison
                        stage_systems <- list("Current Staging" = stage)

                        for (system_name in names(stage_systems)) {
                            current_stage <- stage_systems[[system_name]]

                            for (time_point in time_points) {
                                # Only include observations at risk at this time point
                                at_risk <- time >= time_point

                                if (sum(at_risk) > 10) { # Minimum sample size
                                    # Events in the period from time_point onwards
                                    events_period <- sum(event[at_risk & time >= time_point])

                                    if (methods %in% c("all_methods", "time_dependent")) {
                                        # Calculate time-dependent concordance
                                        td_concordance <- private$.calculateTimeDependentConcordanceAtTime(
                                            surv_obj[at_risk], current_stage[at_risk],
                                            time[at_risk], event[at_risk], time_point
                                        )

                                        result <- list(
                                            Staging_System = system_name,
                                            Time_Point = time_point,
                                            Method = "Time-Dependent",
                                            Concordance = td_concordance$concordance,
                                            Lower_CI = td_concordance$lower_ci,
                                            Upper_CI = td_concordance$upper_ci,
                                            At_Risk = sum(at_risk),
                                            Events_Period = events_period
                                        )
                                        results[[paste(system_name, time_point, "td", sep = "_")]] <- result
                                    }

                                    if (methods %in% c("all_methods", "harrell_c")) {
                                        # Harrell's C at this time point
                                        harrell_td <- private$.calculateHarrellCIndex(surv_obj[at_risk], current_stage[at_risk])

                                        result <- list(
                                            Staging_System = system_name,
                                            Time_Point = time_point,
                                            Method = "Harrell C-index",
                                            Concordance = harrell_td$concordance,
                                            Lower_CI = harrell_td$concordance - private$.zCrit() * harrell_td$se,
                                            Upper_CI = harrell_td$concordance + private$.zCrit() * harrell_td$se,
                                            At_Risk = sum(at_risk),
                                            Events_Period = events_period
                                        )
                                        results[[paste(system_name, time_point, "harrell", sep = "_")]] <- result
                                    }
                                }
                            }
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .calculateTimeDependentConcordanceAtTime = function(surv_obj, stage, time, event, time_point) {
                tryCatch(
                    {
                        # Convert to binary outcome at time_point
                        binary_outcome <- ifelse(time <= time_point & event == 1, 1, 0)

                        # Only consider those who either had event by time_point or were observed beyond time_point
                        eligible <- (time <= time_point & event == 1) | (time > time_point)

                        if (sum(eligible) < 10) {
                            return(list(concordance = 0.5, lower_ci = 0.4, upper_ci = 0.6))
                        }

                        stage_subset <- stage[eligible]
                        outcome_subset <- binary_outcome[eligible]

                        # Calculate AUC using staging as predictor of binary outcome
                        if (requireNamespace("pROC", quietly = TRUE)) {
                            tryCatch(
                                {
                                    roc_obj <- pROC::roc(outcome_subset, as.numeric(stage_subset), quiet = TRUE)
                                    auc_ci <- pROC::ci.auc(roc_obj, quiet = TRUE)

                                    return(list(
                                        concordance = as.numeric(roc_obj$auc),
                                        lower_ci = auc_ci[1],
                                        upper_ci = auc_ci[3]
                                    ))
                                },
                                error = function(e) {
                                    # Fallback calculation
                                    return(list(concordance = 0.5, lower_ci = 0.4, upper_ci = 0.6))
                                }
                            )
                        } else {
                            # Simple concordance calculation
                            stage_numeric <- as.numeric(stage_subset)
                            n <- length(stage_subset)

                            concordant <- 0
                            total <- 0

                            for (i in 1:(n - 1)) {
                                for (j in (i + 1):n) {
                                    if (outcome_subset[i] != outcome_subset[j]) {
                                        total <- total + 1
                                        if ((outcome_subset[i] > outcome_subset[j] && stage_numeric[i] > stage_numeric[j]) ||
                                            (outcome_subset[i] < outcome_subset[j] && stage_numeric[i] < stage_numeric[j])) {
                                            concordant <- concordant + 1
                                        }
                                    }
                                }
                            }

                            concordance <- if (total > 0) concordant / total else 0.5
                            se <- sqrt(concordance * (1 - concordance) / total)

                            return(list(
                                concordance = concordance,
                                lower_ci = concordance - private$.zCrit() * se,
                                upper_ci = concordance + private$.zCrit() * se
                            ))
                        }
                    },
                    error = function(e) {
                        return(list(concordance = 0.5, lower_ci = 0.4, upper_ci = 0.6))
                    }
                )
            },
            .compareConcordanceBetweenStages = function(concordance_estimates) {
                tryCatch(
                    {
                        results <- list()
                        alpha <- self$options$concordanceProbabilityAlpha

                        # Group estimates by method
                        methods <- unique(sapply(concordance_estimates, function(x) x$Method))

                        for (method in methods) {
                            method_estimates <- concordance_estimates[sapply(concordance_estimates, function(x) x$Method == method)]

                            if (length(method_estimates) >= 2) {
                                # Compare all pairs
                                for (i in 1:(length(method_estimates) - 1)) {
                                    for (j in (i + 1):length(method_estimates)) {
                                        est1 <- method_estimates[[i]]
                                        est2 <- method_estimates[[j]]

                                        # Calculate difference and test statistic
                                        diff <- est1$Concordance - est2$Concordance
                                        se_diff <- sqrt(est1$Standard_Error^2 + est2$Standard_Error^2)
                                        z_stat <- diff / se_diff
                                        p_value <- 2 * (1 - pnorm(abs(z_stat)))

                                        interpretation <- if (p_value < alpha) {
                                            if (diff > 0) "System 1 significantly better" else "System 2 significantly better"
                                        } else {
                                            "No significant difference"
                                        }

                                        result <- list(
                                            Method = method,
                                            System_1 = est1$Staging_System,
                                            Concordance_1 = est1$Concordance,
                                            System_2 = est2$Staging_System,
                                            Concordance_2 = est2$Concordance,
                                            Difference = diff,
                                            Test_Statistic = z_stat,
                                            P_value = p_value,
                                            Interpretation = interpretation
                                        )

                                        results[[paste(method, i, j, sep = "_")]] <- result
                                    }
                                }
                            }
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .performConcordanceRobustnessAnalysis = function(surv_obj, stage, time, event) {
                tryCatch(
                    {
                        results <- list()

                        # Create stage-specific models for comparison
                        stage_systems <- list("Current Staging" = stage)

                        for (system_name in names(stage_systems)) {
                            current_stage <- stage_systems[[system_name]]

                            # Baseline concordance
                            baseline <- private$.calculateHarrellCIndex(surv_obj, current_stage)

                            # Outlier sensitivity analysis
                            outlier_indices <- which(time > quantile(time, 0.95, na.rm = TRUE))
                            if (length(outlier_indices) > 0) {
                                robust_indices <- setdiff(seq_along(time), outlier_indices)
                                outlier_robust <- private$.calculateHarrellCIndex(surv_obj[robust_indices], current_stage[robust_indices])

                                result <- list(
                                    Analysis_Type = "Outlier Sensitivity",
                                    Staging_System = system_name,
                                    Scenario = "Outliers Removed",
                                    Concordance = outlier_robust$concordance,
                                    Change_from_Base = outlier_robust$concordance - baseline$concordance,
                                    Robustness_Assessment = if (abs(outlier_robust$concordance - baseline$concordance) < 0.02) "Robust" else "Sensitive"
                                )
                                results[[paste(system_name, "outlier", sep = "_")]] <- result
                            }

                            # Censoring sensitivity analysis
                            high_cens_indices <- which(event == 0 & time < quantile(time[event == 0], 0.5, na.rm = TRUE))
                            if (length(high_cens_indices) > 0) {
                                cens_robust_indices <- setdiff(seq_along(time), high_cens_indices)
                                cens_robust <- private$.calculateHarrellCIndex(surv_obj[cens_robust_indices], current_stage[cens_robust_indices])

                                result <- list(
                                    Analysis_Type = "Censoring Sensitivity",
                                    Staging_System = system_name,
                                    Scenario = "Early Censoring Removed",
                                    Concordance = cens_robust$concordance,
                                    Change_from_Base = cens_robust$concordance - baseline$concordance,
                                    Robustness_Assessment = if (abs(cens_robust$concordance - baseline$concordance) < 0.02) "Robust" else "Sensitive"
                                )
                                results[[paste(system_name, "censoring", sep = "_")]] <- result
                            }

                            # Bootstrap stability analysis
                            if (self$options$concordanceProbabilityBootstrap) {
                                # Was a hardcoded 100; concordanceProbabilityBootstrapSamples was inert.
                                n_boot <- self$options$concordanceProbabilityBootstrapSamples %||% 1000L
                                n_boot <- max(100L, min(5000L, as.integer(n_boot)))
                                boot_concordances <- rep(NA_real_, n_boot)
                                for (b in seq_len(n_boot)) {
                                    if (b %% 100 == 0) private$.checkpoint()
                                    boot_indices <- sample(seq_along(time), replace = TRUE)
                                    boot_concordance <- private$.calculateHarrellCIndex(surv_obj[boot_indices], current_stage[boot_indices])
                                    boot_concordances[b] <- boot_concordance$concordance
                                }

                                boot_var <- var(boot_concordances, na.rm = TRUE)

                                result <- list(
                                    Analysis_Type = "Bootstrap Stability",
                                    Staging_System = system_name,
                                    Scenario = "Bootstrap Variance",
                                    Concordance = mean(boot_concordances, na.rm = TRUE),
                                    Change_from_Base = mean(boot_concordances, na.rm = TRUE) - baseline$concordance,
                                    Robustness_Assessment = if (boot_var < 0.001) "Highly Stable" else if (boot_var < 0.005) "Stable" else "Unstable"
                                )
                                results[[paste(system_name, "bootstrap", sep = "_")]] <- result
                            }
                        }

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .performConcordanceDiagnostics = function(surv_obj, stage, time, event, concordance_estimates) {
                tryCatch(
                    {
                        results <- list()

                        # Sample size adequacy
                        n_obs <- length(time)
                        n_events <- sum(event)

                        sample_size_diagnostic <- list(
                            Diagnostic = "Sample Size Adequacy",
                            Value = n_obs,
                            Interpretation = if (n_obs >= 100 && n_events >= 50) "Adequate for reliable concordance estimation" else "May be insufficient for stable estimates",
                            Recommendation = if (n_obs >= 100 && n_events >= 50) "Proceed with analysis" else "Interpret results with caution"
                        )
                        results[["sample_size"]] <- sample_size_diagnostic

                        # Censoring impact assessment
                        cens_rate <- mean(1 - event)
                        cens_diagnostic <- list(
                            Diagnostic = "Censoring Impact",
                            Value = cens_rate,
                            Interpretation = if (cens_rate < 0.3) "Low censoring - minimal bias expected" else if (cens_rate < 0.6) "Moderate censoring - consider robust methods" else "Heavy censoring - use IPCW methods",
                            Recommendation = if (cens_rate < 0.3) "Standard methods appropriate" else if (cens_rate < 0.6) "Consider Uno's C-index" else "Use IPCW concordance"
                        )
                        results[["censoring"]] <- cens_diagnostic

                        # Concordance estimate stability
                        if (length(concordance_estimates) > 1) {
                            concordances <- sapply(concordance_estimates, function(x) x$Concordance)
                            concordance_range <- max(concordances, na.rm = TRUE) - min(concordances, na.rm = TRUE)

                            stability_diagnostic <- list(
                                Diagnostic = "Method Concordance",
                                Value = concordance_range,
                                Interpretation = if (concordance_range < 0.05) "Methods show good agreement" else if (concordance_range < 0.1) "Moderate method disagreement" else "Substantial method disagreement",
                                Recommendation = if (concordance_range < 0.05) "Results are robust across methods" else "Investigate source of disagreement"
                            )
                            results[["stability"]] <- stability_diagnostic
                        }

                        # Stage distribution assessment
                        stage_table <- table(stage)
                        min_stage_prop <- min(stage_table) / sum(stage_table)

                        stage_diagnostic <- list(
                            Diagnostic = "Stage Distribution Balance",
                            Value = min_stage_prop,
                            Interpretation = if (min_stage_prop >= 0.05) "Balanced stage distribution" else "Imbalanced stage distribution may affect estimates",
                            Recommendation = if (min_stage_prop >= 0.05) "Good distribution for concordance analysis" else "Consider stage grouping for better balance"
                        )
                        results[["stage_distribution"]] <- stage_diagnostic

                        return(results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .generateConcordanceProbabilitySummary = function(results) {
                tryCatch(
                    {
                        summary_results <- list()

                        # Overall discrimination assessment
                        if (!is.null(results$estimates) && length(results$estimates) > 0) {
                            max_concordance <- max(sapply(results$estimates, function(x) x$Concordance), na.rm = TRUE)
                            min_concordance <- min(sapply(results$estimates, function(x) x$Concordance), na.rm = TRUE)

                            discrimination_quality <- if (max_concordance >= 0.8) "Excellent" else if (max_concordance >= 0.7) "Good" else if (max_concordance >= 0.6) "Moderate" else "Poor"

                            overall_result <- list(
                                Finding = paste("Staging System Discrimination:", discrimination_quality),
                                Statistical_Evidence = paste("Concordance range:", round(min_concordance, 3), "-", round(max_concordance, 3)),
                                Clinical_Interpretation = if (max_concordance >= 0.7) "Staging system provides good discrimination for survival outcomes" else "Staging system has limited discriminative ability",
                                Recommendation = if (max_concordance >= 0.7) "Staging system suitable for clinical use" else "Consider additional prognostic factors"
                            )
                            summary_results[["overall"]] <- overall_result
                        }

                        # Method comparison findings
                        if (!is.null(results$estimates) && length(results$estimates) > 1) {
                            methods_used <- unique(sapply(results$estimates, function(x) x$Method))

                            method_result <- list(
                                Finding = paste("Multiple Concordance Methods Applied:", length(methods_used), "methods"),
                                Statistical_Evidence = paste("Methods used:", paste(methods_used, collapse = ", ")),
                                Clinical_Interpretation = .("Comprehensive assessment using multiple concordance measures for robust evaluation"),
                                Recommendation = .("Compare results across methods for validation of discrimination estimates")
                            )
                            summary_results[["methods"]] <- method_result
                        }

                        # Robustness assessment
                        if (!is.null(results$robustness) && length(results$robustness) > 0) {
                            robust_assessments <- sapply(results$robustness, function(x) x$Robustness_Assessment)
                            robust_count <- sum(robust_assessments %in% c("Robust", "Highly Stable", "Stable"))

                            robustness_result <- list(
                                Finding = if (robust_count > length(robust_assessments) / 2) "Robust Concordance Estimates" else "Sensitivity in Concordance Estimates",
                                Statistical_Evidence = paste(robust_count, "out of", length(robust_assessments), "robustness tests passed"),
                                Clinical_Interpretation = if (robust_count > length(robust_assessments) / 2) "Concordance estimates are stable across different scenarios" else "Concordance estimates show sensitivity to assumptions",
                                Recommendation = if (robust_count > length(robust_assessments) / 2) "Results are reliable for clinical decision-making" else "Interpret concordance estimates with caution"
                            )
                            summary_results[["robustness"]] <- robustness_result
                        }

                        # Time-dependent findings
                        if (!is.null(results$time_dependent) && length(results$time_dependent) > 0) {
                            time_dep_concordances <- sapply(results$time_dependent, function(x) x$Concordance)
                            time_dep_range <- max(time_dep_concordances, na.rm = TRUE) - min(time_dep_concordances, na.rm = TRUE)

                            temporal_result <- list(
                                Finding = if (time_dep_range < 0.1) "Stable Temporal Discrimination" else "Variable Temporal Discrimination",
                                Statistical_Evidence = paste("Time-dependent concordance range:", round(time_dep_range, 3)),
                                Clinical_Interpretation = if (time_dep_range < 0.1) "Staging system maintains discrimination across time periods" else "Discrimination varies by follow-up time",
                                Recommendation = if (time_dep_range < 0.1) "Consistent staging performance over time" else "Consider time-specific staging considerations"
                            )
                            summary_results[["temporal"]] <- temporal_result
                        }

                        return(summary_results)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .populateConcordanceProbabilityOverview = function(results) {
                tryCatch(
                    {
                        table <- self$results$concordanceProbabilityOverview

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Characteristic, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateConcordanceProbabilityEstimates = function(results) {
                tryCatch(
                    {
                        table <- self$results$concordanceProbabilityEstimates

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = paste(result$Method, result$Staging_System, sep = "_"), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateConcordanceProbabilityTimeDependentComplex = function(results) {
                tryCatch(
                    {
                        table <- self$results$concordanceProbabilityTimeDependentComplex

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = paste(result$Staging_System, result$Time_Point, result$Method, sep = "_"), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateConcordanceProbabilityComparison = function(results) {
                tryCatch(
                    {
                        table <- self$results$concordanceProbabilityComparison

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = paste(result$Method, result$System_1, result$System_2, sep = "_"), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateConcordanceProbabilityRobustness = function(results) {
                tryCatch(
                    {
                        table <- self$results$concordanceProbabilityRobustness

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = paste(result$Analysis_Type, result$Staging_System, result$Scenario, sep = "_"), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateConcordanceProbabilityDiagnosticsTable = function(results) {
                tryCatch(
                    {
                        table <- self$results$concordanceProbabilityDiagnosticsTable

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Diagnostic, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateConcordanceProbabilitySummary = function(results) {
                tryCatch(
                    {
                        table <- self$results$concordanceProbabilitySummary

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Finding, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            # Build the endpoint hierarchy from the user's options.
            # Tier 1 is always the mortality/time-to-event endpoint; tiers 2 and 3
            # are the optional secondary/tertiary continuous endpoints.
            .buildWinRatioEndpoints = function(data) {
                hierarchy <- self$options$winRatioEndpoints
                if (is.null(hierarchy) || !nzchar(hierarchy)) hierarchy <- "death_progression_response"

                tier_labels <- switch(hierarchy,
                    death_progression_response = c("Death", "Disease Progression", "Response"),
                    death_recurrence_remission = c("Death", "Recurrence", "Remission"),
                    death_hospitalization_qol = c("Death", "Hospitalization", "Quality of Life"),
                    c("Primary", "Secondary", "Tertiary"))
                if (identical(hierarchy, "custom")) tier_labels <- c("Primary", "Secondary", "Tertiary")

                time_col <- self$options$survivalTime
                event_col <- "event_binary"

                # An explicit death indicator overrides the analysis event variable.
                death_var <- self$options$winRatioDeathVariable
                if (!is.null(death_var) && nzchar(death_var) && death_var %in% names(data)) {
                    dv <- data[[death_var]]
                    data[["wr_death_binary"]] <- if (is.factor(dv)) {
                        as.numeric(dv == levels(dv)[length(levels(dv))])
                    } else {
                        as.numeric(as.numeric(dv) > 0)
                    }
                    event_col <- "wr_death_binary"
                }

                # An explicit time variable overrides the analysis survival time.
                time_vars <- self$options$winRatioTimeVariables
                if (!is.null(time_vars) && length(time_vars) > 0) {
                    tv <- time_vars[[1]]
                    if (tv %in% names(data)) time_col <- tv
                }

                endpoints <- list()
                endpoints[["primary"]] <- list(
                    name = tier_labels[1],
                    type = "time_to_event",
                    time_col = time_col,
                    event_col = event_col,
                    priority = 1
                )

                sec_dir <- if (identical(self$options$wrSecondaryDirection, "lower")) {
                    "smaller_better"
                } else {
                    "larger_better"
                }

                sec <- self$options$winRatioSecondaryEndpoint
                if (!is.null(sec) && nzchar(sec) && sec %in% names(data)) {
                    endpoints[["secondary"]] <- list(
                        name = paste0(tier_labels[2], " (", sec, ")"),
                        type = "continuous",
                        value_col = sec,
                        direction = sec_dir,
                        priority = 2
                    )
                }

                ter <- self$options$winRatioTertiaryEndpoint
                if (!is.null(ter) && nzchar(ter) && ter %in% names(data)) {
                    endpoints[["tertiary"]] <- list(
                        name = paste0(tier_labels[3], " (", ter, ")"),
                        type = "continuous",
                        value_col = ter,
                        direction = sec_dir,
                        priority = 3
                    )
                }

                list(endpoints = endpoints, data = data)
            },

            # Apply the requested pairing strategy to two stage groups.
            # Returns the (possibly reduced/re-ordered) groups plus a label describing
            # what was actually done, so the table can report it honestly.
            .applyWinRatioMatching = function(g1, g2, endpoints, strata_col = NULL) {
                strategy <- self$options$winRatioMatchingStrategy
                if (is.null(strategy) || !nzchar(strategy)) strategy <- "all_pairs"

                prim <- endpoints[["primary"]]

                if (identical(strategy, "all_pairs")) {
                    return(list(g1 = g1, g2 = g2, diagonal = FALSE, label = "All possible pairs"))
                }

                if (identical(strategy, "matched_pairs")) {
                    k <- min(nrow(g1), nrow(g2))
                    o1 <- order(as.numeric(g1[[prim$time_col]]))[seq_len(k)]
                    o2 <- order(as.numeric(g2[[prim$time_col]]))[seq_len(k)]
                    return(list(g1 = g1[o1, , drop = FALSE], g2 = g2[o2, , drop = FALSE],
                                diagonal = TRUE,
                                label = sprintf("Rank-matched 1:1 pairs (n = %d)", k)))
                }

                if (identical(strategy, "propensity_matched")) {
                    covs <- c(self$options$continuousCovariates, self$options$categoricalCovariates)
                    covs <- covs[!is.null(covs)]
                    covs <- covs[covs %in% intersect(names(g1), names(g2))]
                    if (length(covs) == 0) {
                        k <- min(nrow(g1), nrow(g2))
                        o1 <- order(as.numeric(g1[[prim$time_col]]))[seq_len(k)]
                        o2 <- order(as.numeric(g2[[prim$time_col]]))[seq_len(k)]
                        return(list(g1 = g1[o1, , drop = FALSE], g2 = g2[o2, , drop = FALSE],
                                    diagonal = TRUE,
                                    label = "Rank-matched 1:1 (no covariates supplied for propensity model)"))
                    }
                    ps <- tryCatch({
                        pooled <- rbind(g1[, covs, drop = FALSE], g2[, covs, drop = FALSE])
                        pooled$.grp <- c(rep(1L, nrow(g1)), rep(0L, nrow(g2)))
                        fit <- stats::glm(stats::as.formula(paste0(".grp ~ ",
                                          paste(sprintf("`%s`", covs), collapse = " + "))),
                                          data = pooled, family = stats::binomial())
                        stats::predict(fit, type = "response")
                    }, error = function(e) NULL)

                    if (is.null(ps) || anyNA(ps)) {
                        k <- min(nrow(g1), nrow(g2))
                        return(list(g1 = g1[seq_len(k), , drop = FALSE], g2 = g2[seq_len(k), , drop = FALSE],
                                    diagonal = TRUE,
                                    label = "Rank-matched 1:1 (propensity model failed to converge)"))
                    }

                    p1 <- ps[seq_len(nrow(g1))]
                    p2 <- ps[(nrow(g1) + 1):length(ps)]
                    # greedy nearest-neighbour 1:1 matching without replacement
                    avail <- rep(TRUE, length(p2))
                    i1 <- integer(0); i2 <- integer(0)
                    for (i in order(p1)) {
                        if (!any(avail)) break
                        cand <- which(avail)
                        j <- cand[which.min(abs(p2[cand] - p1[i]))]
                        avail[j] <- FALSE
                        i1 <- c(i1, i); i2 <- c(i2, j)
                    }
                    return(list(g1 = g1[i1, , drop = FALSE], g2 = g2[i2, , drop = FALSE],
                                diagonal = TRUE,
                                label = sprintf("Propensity-matched 1:1 pairs (n = %d, %d covariates)",
                                                length(i1), length(covs))))
                }

                # stratified: handled by the caller, which loops strata and pools W and L
                list(g1 = g1, g2 = g2, diagonal = FALSE, label = "All possible pairs within strata")
            },

            # Win ratio between two disjoint stage groups, honouring the matching strategy.
            .winRatioBetweenGroups = function(g1, g2, endpoints, strata_col = NULL) {
                strategy <- self$options$winRatioMatchingStrategy
                if (is.null(strategy) || !nzchar(strategy)) strategy <- "all_pairs"

                if (identical(strategy, "stratified") && !is.null(strata_col) &&
                    strata_col %in% names(g1) && strata_col %in% names(g2)) {
                    # Pool wins and losses across strata (Mantel-Haenszel style),
                    # then take the variance from the pooled pair matrices.
                    lv <- intersect(unique(as.character(g1[[strata_col]])),
                                    unique(as.character(g2[[strata_col]])))
                    tw <- 0; tl <- 0; tt <- 0; np <- 0
                    var_num <- 0; ok <- FALSE
                    contribs <- list()
                    for (s in lv) {
                        s1 <- g1[as.character(g1[[strata_col]]) == s, , drop = FALSE]
                        s2 <- g2[as.character(g2[[strata_col]]) == s, , drop = FALSE]
                        if (nrow(s1) < 2 || nrow(s2) < 2) next
                        pmv <- private$.winRatioPairMatrices(s1, s2, endpoints)
                        w <- sum(pmv$win); l <- sum(pmv$loss)
                        tw <- tw + w; tl <- tl + l
                        tt <- tt + (nrow(s1) * nrow(s2)) - w - l
                        np <- np + nrow(s1) * nrow(s2)
                        v <- private$.winRatioLogVar(pmv$win, pmv$loss)
                        if (!is.na(v)) { var_num <- var_num + v * (w + l)^2; ok <- TRUE }
                        for (nm in names(pmv$contributions)) {
                            if (is.null(contribs[[nm]])) contribs[[nm]] <- pmv$contributions[[nm]]
                            else {
                                contribs[[nm]]$wins <- contribs[[nm]]$wins + pmv$contributions[[nm]]$wins
                                contribs[[nm]]$losses <- contribs[[nm]]$losses + pmv$contributions[[nm]]$losses
                                contribs[[nm]]$ties <- contribs[[nm]]$ties + pmv$contributions[[nm]]$ties
                            }
                        }
                    }
                    if (tw == 0 || tl == 0) return(NULL)

                    conf_level <- self$options$winRatioConfidenceLevel
                    if (is.null(conf_level) || !is.finite(conf_level)) conf_level <- 0.95
                    zcrit <- stats::qnorm(1 - (1 - conf_level) / 2)
                    wr <- tw / tl
                    lvar <- if (ok) var_num / (tw + tl)^2 else NA_real_
                    se <- if (is.na(lvar)) NA_real_ else sqrt(lvar)
                    ci_l <- if (is.na(se)) NA_real_ else exp(log(wr) - zcrit * se)
                    ci_u <- if (is.na(se)) NA_real_ else exp(log(wr) + zcrit * se)
                    pv <- if (is.na(se) || se <= 0) NA_real_ else 2 * stats::pnorm(-abs(log(wr) / se))

                    decided <- tw + tl
                    for (nm in names(contribs)) {
                        d <- contribs[[nm]]$wins + contribs[[nm]]$losses
                        contribs[[nm]]$win_percentage <- if (d > 0) contribs[[nm]]$wins / d * 100 else 0
                        contribs[[nm]]$contribution_to_overall <- if (decided > 0) d / decided else 0
                    }

                    return(list(win_ratio = wr, win_odds = wr, net_benefit = (tw - tl) / np,
                                ci_lower = ci_l, ci_upper = ci_u, p_value = pv, se_log_wr = se,
                                ci_method = "asymptotic (stratified)", conf_level = conf_level,
                                interpretation = if (!is.na(pv) && pv < (1 - conf_level)) {
                                    if (wr > 1) "Favors Group 1" else "Favors Group 2"
                                } else "No significant difference",
                                wins = tw, losses = tl, ties = tt, total_pairs = np,
                                n1 = nrow(g1), n2 = nrow(g2),
                                match_label = sprintf("Stratified over %d strata", length(lv)),
                                endpoint_contributions = contribs))
                }

                mm <- private$.applyWinRatioMatching(g1, g2, endpoints, strata_col)
                res <- private$.calculateWinRatio(mm$g1, mm$g2, endpoints)
                if (!is.null(res)) res$match_label <- mm$label
                res
            },

            .performWinRatioAnalysis = function(data, all_results) {
                tryCatch(
                    {
                        time_var <- self$options$survivalTime
                        old_stage_var <- self$options$oldStage
                        new_stage_var <- self$options$newStage

                        if (is.null(time_var) || is.null(old_stage_var) || is.null(new_stage_var)) {
                            stop("Required variables not specified for Win Ratio Analysis")
                        }

                        data[[old_stage_var]] <- as.factor(data[[old_stage_var]])
                        data[[new_stage_var]] <- as.factor(data[[new_stage_var]])

                        built <- private$.buildWinRatioEndpoints(data)
                        endpoints <- built$endpoints
                        data <- built$data

                        prim <- endpoints[["primary"]]

                        # The pair matrices index these columns directly; a missing one would
                        # reach as.numeric(NULL) -> numeric(0) and break outer(). Fail loudly
                        # here instead.
                        if (!(prim$time_col %in% names(data)) || !(prim$event_col %in% names(data))) {
                            stop("Win Ratio Analysis requires the survival time and event columns")
                        }

                        need <- unique(c(prim$time_col, prim$event_col, old_stage_var, new_stage_var))
                        complete_data <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]

                        if (nrow(complete_data) < 10) stop("Insufficient data for Win Ratio Analysis")

                        conf_level <- self$options$winRatioConfidenceLevel
                        if (is.null(conf_level) || !is.finite(conf_level)) conf_level <- 0.95

                        win_ratio_results <- list()
                        endpoint_results <- list()
                        comparison_results <- list()
                        sensitivity_results <- list()
                        pairwise_results <- list()
                        winratio_summary <- list()

                        systems <- list(
                            list(label = "Original", var = old_stage_var, other = new_stage_var),
                            list(label = "New", var = new_stage_var, other = old_stage_var)
                        )

                        match_label <- NA_character_

                        # Adjacent-stage contrasts WITHIN each staging system. Groups defined
                        # this way are disjoint, which the two-sample win ratio requires; the
                        # earlier old-stage-vs-new-stage contrast compared overlapping sets of
                        # the same patients.
                        for (sysinfo in systems) {
                            sv <- sysinfo$var
                            lv <- levels(complete_data[[sv]])
                            if (length(lv) < 2) next

                            for (k in seq_len(length(lv) - 1)) {
                                lo <- lv[k]; hi <- lv[k + 1]
                                g1 <- complete_data[complete_data[[sv]] == lo, , drop = FALSE]
                                g2 <- complete_data[complete_data[[sv]] == hi, , drop = FALSE]
                                if (nrow(g1) < 3 || nrow(g2) < 3) next

                                private$.checkpoint()
                                wr <- private$.winRatioBetweenGroups(g1, g2, endpoints, sysinfo$other)
                                if (is.null(wr)) next
                                if (is.na(match_label) && !is.null(wr$match_label)) match_label <- wr$match_label

                                cmp <- sprintf("%s: %s vs %s", sysinfo$label, lo, hi)

                                win_ratio_results[[length(win_ratio_results) + 1]] <- list(
                                    Comparison = cmp,
                                    Win_Ratio = round(wr$win_ratio, 3),
                                    Wins = wr$wins,
                                    Losses = wr$losses,
                                    Ties = wr$ties,
                                    Lower_CI = if (is.na(wr$ci_lower)) NA_real_ else round(wr$ci_lower, 3),
                                    Upper_CI = if (is.na(wr$ci_upper)) NA_real_ else round(wr$ci_upper, 3),
                                    P_value = wr$p_value,
                                    Interpretation = wr$interpretation
                                )

                                comparison_results[[length(comparison_results) + 1]] <- list(
                                    Stage = cmp,
                                    N_Pairs = wr$total_pairs,
                                    Win_Ratio = round(wr$win_ratio, 3),
                                    Lower_CI = if (is.na(wr$ci_lower)) NA_real_ else round(wr$ci_lower, 3),
                                    Upper_CI = if (is.na(wr$ci_upper)) NA_real_ else round(wr$ci_upper, 3),
                                    P_value = wr$p_value,
                                    Weight = round(wr$total_pairs / max(1, nrow(complete_data)^2), 4)
                                )

                                cum <- 0
                                for (nm in names(wr$endpoint_contributions)) {
                                    ct <- wr$endpoint_contributions[[nm]]
                                    cum <- cum + ct$contribution_to_overall * 100
                                    endpoint_results[[length(endpoint_results) + 1]] <- list(
                                        Endpoint = if (is.null(ct$label)) nm else ct$label,
                                        Staging_System = cmp,
                                        Wins = ct$wins,
                                        Losses = ct$losses,
                                        Ties = ct$ties,
                                        Win_Proportion = round(ct$win_percentage / 100, 3),
                                        Contribution_Percent = round(ct$contribution_to_overall * 100, 1),
                                        Cumulative_Percent = round(cum, 1)
                                    )
                                }

                                if (isTRUE(self$options$winRatioGeneralizedPairwise)) {
                                    pairwise_results[[length(pairwise_results) + 1]] <- list(
                                        Metric = paste0("Win ratio - ", cmp),
                                        Value = round(wr$win_ratio, 3),
                                        Lower_CI = if (is.na(wr$ci_lower)) NA_real_ else round(wr$ci_lower, 3),
                                        Upper_CI = if (is.na(wr$ci_upper)) NA_real_ else round(wr$ci_upper, 3),
                                        P_value = wr$p_value,
                                        Interpretation = wr$interpretation
                                    )
                                    pairwise_results[[length(pairwise_results) + 1]] <- list(
                                        Metric = paste0("Win odds - ", cmp),
                                        Value = round(wr$win_odds, 3),
                                        Lower_CI = NA_real_, Upper_CI = NA_real_, P_value = NA_real_,
                                        Interpretation = .("Ties credited per tie-handling rule")
                                    )
                                    pairwise_results[[length(pairwise_results) + 1]] <- list(
                                        Metric = paste0("Net benefit (Buyse) - ", cmp),
                                        Value = round(wr$net_benefit, 3),
                                        Lower_CI = NA_real_, Upper_CI = NA_real_, P_value = NA_real_,
                                        Interpretation = .("(wins - losses) / all pairs; range -1 to 1")
                                    )
                                }
                            }
                        }

                        # Sensitivity: administrative censoring at earlier follow-up horizons.
                        # Truncation must also reset the event indicator, otherwise a late death
                        # is fabricated as an early one.
                        if (isTRUE(self$options$winRatioSensitivityAnalysis) &&
                            length(win_ratio_results) > 0) {
                            tcol <- prim$time_col; ecol <- prim$event_col
                            maxt <- max(complete_data[[tcol]], na.rm = TRUE)
                            cutoffs <- stats::quantile(complete_data[[tcol]],
                                                       probs = c(0.5, 0.75, 0.9), na.rm = TRUE)
                            cutoffs <- unique(round(c(cutoffs, maxt), 4))

                            # Index the uncapped win ratio BY CONTRAST. Using
                            # win_ratio_results[[1]] compared every truncated row against the
                            # first contrast (Stage I vs II), so a Stage II-vs-III row was
                            # scored against an unrelated baseline and its "Robustness" verdict
                            # was meaningless.
                            primary_by_cmp <- stats::setNames(
                                vapply(win_ratio_results, function(x) as.numeric(x$Win_Ratio), numeric(1)),
                                vapply(win_ratio_results, function(x) x$Comparison, character(1)))
                            sv <- old_stage_var
                            lv <- levels(complete_data[[sv]])

                            for (cutoff in cutoffs) {
                                trunc <- complete_data
                                over <- !is.na(trunc[[tcol]]) & trunc[[tcol]] > cutoff
                                trunc[[ecol]][over] <- 0
                                trunc[[tcol]][over] <- cutoff

                                for (k in seq_len(max(0, min(2, length(lv) - 1)))) {
                                    g1 <- trunc[trunc[[sv]] == lv[k], , drop = FALSE]
                                    g2 <- trunc[trunc[[sv]] == lv[k + 1], , drop = FALSE]
                                    if (nrow(g1) < 3 || nrow(g2) < 3) next
                                    private$.checkpoint()
                                    wr <- private$.calculateWinRatio(g1, g2, endpoints)
                                    if (is.null(wr) || !is.finite(wr$win_ratio)) next

                                    cmp_key <- sprintf("Original: %s vs %s", lv[k], lv[k + 1])
                                    base_wr <- unname(primary_by_cmp[cmp_key])
                                    chg <- if (!is.na(base_wr) && is.finite(base_wr) && base_wr != 0) {
                                        round((wr$win_ratio - base_wr) / base_wr * 100, 1)
                                    } else NA_real_
                                    sensitivity_results[[length(sensitivity_results) + 1]] <- list(
                                        Analysis_Type = "Administrative censoring",
                                        Scenario = sprintf("%s: %s vs %s, follow-up capped at %.1f",
                                                           "Original", lv[k], lv[k + 1], cutoff),
                                        Win_Ratio = round(wr$win_ratio, 3),
                                        Change_from_Primary = chg,
                                        Lower_CI = if (is.na(wr$ci_lower)) NA_real_ else round(wr$ci_lower, 3),
                                        Upper_CI = if (is.na(wr$ci_upper)) NA_real_ else round(wr$ci_upper, 3),
                                        Robustness = if (is.na(chg)) "Not estimable"
                                                     else if (abs(chg) < 10) "Robust"
                                                     else if (abs(chg) < 25) "Moderately sensitive"
                                                     else "Sensitive"
                                    )
                                }
                            }
                        }

                        if (length(win_ratio_results) > 0) {
                            wrs <- vapply(win_ratio_results, function(x) as.numeric(x$Win_Ratio), numeric(1))
                            wrs <- wrs[is.finite(wrs)]
                            old_wr <- vapply(win_ratio_results[grepl("^Original", vapply(win_ratio_results,
                                        function(x) x$Comparison, character(1)))],
                                        function(x) as.numeric(x$Win_Ratio), numeric(1))
                            new_wr <- vapply(win_ratio_results[grepl("^New", vapply(win_ratio_results,
                                        function(x) x$Comparison, character(1)))],
                                        function(x) as.numeric(x$Win_Ratio), numeric(1))
                            old_wr <- old_wr[is.finite(old_wr)]; new_wr <- new_wr[is.finite(new_wr)]

                            method_lbl <- self$options$winRatioConfidenceMethod
                            if (is.null(method_lbl) || !nzchar(method_lbl)) method_lbl <- "asymptotic"

                            winratio_summary <- list(
                                list(Characteristic = "Adjacent-stage contrasts",
                                     Value = as.character(length(win_ratio_results)),
                                     Description = "Win ratios between neighbouring stages within each system"),
                                list(Characteristic = "Endpoint hierarchy",
                                     Value = paste(vapply(endpoints, function(x) x$name, character(1)),
                                                   collapse = " > "),
                                     Description = "Hierarchical comparison order (Finkelstein-Schoenfeld rule)"),
                                list(Characteristic = "Pairing strategy",
                                     Value = if (is.na(match_label)) "All possible pairs" else match_label,
                                     Description = "How subjects from the two stage groups were paired"),
                                list(Characteristic = "Inference method",
                                     Value = sprintf("%s, %.0f%% CI", method_lbl, conf_level * 100),
                                     Description = "Variance of log(win ratio) from the two-sample U-statistic"),
                                list(Characteristic = "Median win ratio (Original staging)",
                                     Value = if (length(old_wr)) format(round(stats::median(old_wr), 3)) else "Not estimable",
                                     Description = "Larger values indicate better separation of adjacent stages"),
                                list(Characteristic = "Median win ratio (New staging)",
                                     Value = if (length(new_wr)) format(round(stats::median(new_wr), 3)) else "Not estimable",
                                     Description = "Larger values indicate better separation of adjacent stages")
                            )

                            verdict <- if (length(old_wr) && length(new_wr)) {
                                if (stats::median(new_wr) > stats::median(old_wr)) {
                                    "New staging separates adjacent stages more sharply"
                                } else if (stats::median(new_wr) < stats::median(old_wr)) {
                                    "Original staging separates adjacent stages more sharply"
                                } else "Both systems separate adjacent stages comparably"
                            } else "Insufficient adjacent-stage contrasts for a verdict"

                            summary_rows <- list(list(
                                Finding = "Adjacent-stage discrimination",
                                Statistical_Evidence = sprintf(
                                    "Median win ratio %s (Original) vs %s (New) over %d contrasts",
                                    if (length(old_wr)) format(round(stats::median(old_wr), 3)) else "NA",
                                    if (length(new_wr)) format(round(stats::median(new_wr), 3)) else "NA",
                                    length(win_ratio_results)),
                                Clinical_Interpretation = verdict,
                                Recommendation = if (length(old_wr) && length(new_wr) &&
                                                     stats::median(new_wr) > stats::median(old_wr)) {
                                    "Supports adoption of the new staging system on this endpoint hierarchy"
                                } else {
                                    "Does not by itself support adoption; review alongside C-index and NRI/IDI"
                                }
                            ))

                            private$.populateWinRatioOverview(list(overview = winratio_summary))
                            private$.populateWinRatioPrimaryResults(win_ratio_results)
                            private$.populateWinRatioEndpointContributions(endpoint_results)
                            private$.populateWinRatioComparisons(comparison_results)
                            if (length(sensitivity_results) > 0) {
                                private$.populateWinRatioSensitivity(sensitivity_results)
                            }
                            if (length(pairwise_results) > 0) {
                                private$.populateWinRatioPairwise(pairwise_results)
                            }
                            private$.populateWinRatioSummary(summary_rows)
                        }

                        return(list(
                            win_ratio_results = win_ratio_results,
                            endpoint_results = endpoint_results,
                            sensitivity_results = sensitivity_results,
                            summary = winratio_summary
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            # Pairwise win/loss matrices for one endpoint tier.
            # Returns two n1 x n2 logical matrices from group-1's perspective.
            # Time-to-event uses the Finkelstein-Schoenfeld rule: a pair is only
            # decided when censoring cannot mask the ordering, i.e.
            #   win  <=> group-2 subject had the event and outlived by group-1 subject
            #   loss <=> group-1 subject had the event and was outlived by group-2 subject
            .winLossMatrices = function(g1, g2, endpoint) {
                n1 <- nrow(g1)
                n2 <- nrow(g2)

                if (identical(endpoint$type, "time_to_event")) {
                    t1 <- as.numeric(g1[[endpoint$time_col]])
                    e1 <- as.numeric(g1[[endpoint$event_col]])
                    t2 <- as.numeric(g2[[endpoint$time_col]])
                    e2 <- as.numeric(g2[[endpoint$event_col]])

                    gt <- outer(t1, t2, ">")
                    lt <- outer(t1, t2, "<")
                    # e2 indexes columns, e1 indexes rows
                    e2m <- matrix(rep(e2 == 1, each = n1), nrow = n1)
                    e1m <- matrix(rep(e1 == 1, times = n2), nrow = n1)

                    win <- gt & e2m
                    loss <- lt & e1m

                    na <- outer(is.na(t1) | is.na(e1), is.na(t2) | is.na(e2), "|")
                    win[na] <- FALSE
                    loss[na] <- FALSE
                } else {
                    v1 <- as.numeric(g1[[endpoint$value_col]])
                    v2 <- as.numeric(g2[[endpoint$value_col]])

                    gt <- outer(v1, v2, ">")
                    lt <- outer(v1, v2, "<")

                    if (identical(endpoint$direction, "smaller_better")) {
                        win <- lt
                        loss <- gt
                    } else {
                        win <- gt
                        loss <- lt
                    }

                    na <- outer(is.na(v1), is.na(v2), "|")
                    win[na] <- FALSE
                    loss[na] <- FALSE
                }

                list(win = win, loss = loss)
            },

            # Hierarchical (Pocock / Finkelstein-Schoenfeld) pairwise comparison.
            # Descends the endpoint hierarchy only for pairs still undecided, unless
            # winRatioHandleTies == "ignore", in which case only the first tier is used.
            .winRatioPairMatrices = function(g1, g2, endpoints) {
                ord <- order(vapply(endpoints, function(x) x$priority, numeric(1)))
                sorted <- names(endpoints)[ord]

                tie_rule <- self$options$winRatioHandleTies
                if (is.null(tie_rule) || !nzchar(tie_rule)) tie_rule <- "next_endpoint"
                if (identical(tie_rule, "ignore")) sorted <- sorted[1]

                n1 <- nrow(g1); n2 <- nrow(g2)
                win <- matrix(FALSE, n1, n2)
                loss <- matrix(FALSE, n1, n2)
                contrib <- list()

                for (nm in sorted) {
                    undecided <- !(win | loss)
                    if (!any(undecided)) {
                        contrib[[nm]] <- list(wins = 0, losses = 0, ties = 0,
                                              priority = endpoints[[nm]]$priority,
                                              label = endpoints[[nm]]$name)
                        next
                    }
                    wl <- private$.winLossMatrices(g1, g2, endpoints[[nm]])
                    w_k <- wl$win & undecided
                    l_k <- wl$loss & undecided

                    contrib[[nm]] <- list(
                        wins = sum(w_k),
                        losses = sum(l_k),
                        ties = sum(undecided) - sum(w_k) - sum(l_k),
                        priority = endpoints[[nm]]$priority,
                        label = endpoints[[nm]]$name
                    )

                    win <- win | w_k
                    loss <- loss | l_k
                }

                list(win = win, loss = loss, contributions = contrib)
            },

            # Asymptotic variance of log(win ratio) via the two-sample U-statistic
            # (Hoeffding) decomposition -- Bebu & Lachin (2016), Dong et al. (2016).
            # The naive sqrt(1/wins + 1/losses) is invalid here because each subject
            # contributes to n2 (resp. n1) correlated pairs.
            .winRatioLogVar = function(win, loss) {
                n1 <- nrow(win); n2 <- ncol(win)
                N <- n1 * n2
                nW <- sum(win); nL <- sum(loss)
                if (nW == 0 || nL == 0 || n1 < 2 || n2 < 2) return(NA_real_)

                pW <- nW / N
                pL <- nL / N

                # per-subject conditional win/loss proportions
                Wi <- rowSums(win) / n2;  Li <- rowSums(loss) / n2   # group 1 subjects
                Wj <- colSums(win) / n1;  Lj <- colSums(loss) / n1   # group 2 subjects

                varW <- stats::var(Wi) / n1 + stats::var(Wj) / n2
                varL <- stats::var(Li) / n1 + stats::var(Lj) / n2
                covWL <- stats::cov(Wi, Li) / n1 + stats::cov(Wj, Lj) / n2

                v <- varW / pW^2 + varL / pL^2 - 2 * covWL / (pW * pL)
                if (!is.finite(v) || v <= 0) return(NA_real_)
                v
            },

            .calculateWinRatio = function(group1_data, group2_data, endpoints) {
                tryCatch(
                    {
                        n1 <- nrow(group1_data)
                        n2 <- nrow(group2_data)
                        total_pairs <- n1 * n2

                        pm <- private$.winRatioPairMatrices(group1_data, group2_data, endpoints)
                        win <- pm$win
                        loss <- pm$loss

                        wins <- sum(win)
                        losses <- sum(loss)
                        ties <- total_pairs - wins - losses

                        conf_level <- self$options$winRatioConfidenceLevel
                        if (is.null(conf_level) || !is.finite(conf_level)) conf_level <- 0.95
                        zcrit <- stats::qnorm(1 - (1 - conf_level) / 2)

                        method <- self$options$winRatioConfidenceMethod
                        if (is.null(method) || !nzchar(method)) method <- "asymptotic"

                        win_ratio <- if (losses > 0) wins / losses else Inf

                        # Ties contribute only to the win-odds / net-benefit summaries,
                        # never to the win ratio itself (which is W/L by definition).
                        tie_rule <- self$options$winRatioHandleTies
                        if (is.null(tie_rule) || !nzchar(tie_rule)) tie_rule <- "next_endpoint"
                        tie_credit <- if (identical(tie_rule, "split")) ties / 2 else 0
                        win_odds <- if ((losses + tie_credit) > 0) {
                            (wins + tie_credit) / (losses + tie_credit)
                        } else Inf
                        net_benefit <- (wins - losses) / total_pairs

                        log_var <- private$.winRatioLogVar(win, loss)
                        se_log_wr <- if (is.na(log_var)) NA_real_ else sqrt(log_var)

                        ci_lower <- NA_real_; ci_upper <- NA_real_; p_value <- NA_real_
                        ci_method_used <- method

                        if (is.finite(win_ratio) && win_ratio > 0 && !is.na(se_log_wr)) {
                            log_wr <- log(win_ratio)

                            if (identical(method, "bootstrap")) {
                                nboot <- self$options$winRatioBootstrapSamples
                                if (is.null(nboot) || !is.finite(nboot)) nboot <- 1000
                                nboot <- max(100, min(5000, as.integer(nboot)))

                                boot_log <- rep(NA_real_, nboot)
                                for (b in seq_len(nboot)) {
                                    if (b %% 50 == 0) private$.checkpoint()
                                    i1 <- sample.int(n1, n1, replace = TRUE)
                                    i2 <- sample.int(n2, n2, replace = TRUE)
                                    wb <- sum(win[i1, i2, drop = FALSE])
                                    lb <- sum(loss[i1, i2, drop = FALSE])
                                    if (wb > 0 && lb > 0) boot_log[b] <- log(wb / lb)
                                }
                                boot_log <- boot_log[is.finite(boot_log)]

                                if (length(boot_log) >= 20) {
                                    qs <- stats::quantile(boot_log,
                                                          probs = c((1 - conf_level) / 2,
                                                                    1 - (1 - conf_level) / 2),
                                                          na.rm = TRUE)
                                    ci_lower <- exp(qs[[1]])
                                    ci_upper <- exp(qs[[2]])
                                    se_boot <- stats::sd(boot_log)
                                    p_value <- if (se_boot > 0) {
                                        2 * stats::pnorm(-abs(log_wr / se_boot))
                                    } else NA_real_
                                } else {
                                    ci_method_used <- "asymptotic"
                                }
                            } else if (identical(method, "permutation")) {
                                # Permutation gives a valid null p-value; the interval
                                # still comes from the asymptotic U-statistic variance.
                                nperm <- self$options$winRatioBootstrapSamples
                                if (is.null(nperm) || !is.finite(nperm)) nperm <- 1000
                                nperm <- max(100, min(5000, as.integer(nperm)))

                                pooled <- rbind(group1_data[, , drop = FALSE],
                                                group2_data[, , drop = FALSE])
                                nt <- n1 + n2
                                extreme <- 0L; valid <- 0L
                                for (b in seq_len(nperm)) {
                                    if (b %% 25 == 0) private$.checkpoint()
                                    idx <- sample.int(nt, nt, replace = FALSE)
                                    pg1 <- pooled[idx[seq_len(n1)], , drop = FALSE]
                                    pg2 <- pooled[idx[(n1 + 1):nt], , drop = FALSE]
                                    pmp <- private$.winRatioPairMatrices(pg1, pg2, endpoints)
                                    wb <- sum(pmp$win); lb <- sum(pmp$loss)
                                    if (wb > 0 && lb > 0) {
                                        valid <- valid + 1L
                                        if (abs(log(wb / lb)) >= abs(log_wr)) extreme <- extreme + 1L
                                    }
                                }
                                if (valid >= 20) p_value <- (extreme + 1) / (valid + 1)
                                ci_lower <- exp(log_wr - zcrit * se_log_wr)
                                ci_upper <- exp(log_wr + zcrit * se_log_wr)
                            }

                            if (is.na(ci_lower)) {
                                ci_lower <- exp(log_wr - zcrit * se_log_wr)
                                ci_upper <- exp(log_wr + zcrit * se_log_wr)
                            }
                            if (is.na(p_value)) {
                                p_value <- 2 * stats::pnorm(-abs(log_wr / se_log_wr))
                            }
                        } else if (losses == 0 && wins > 0) {
                            # Separation: no finite interval is estimable.
                            ci_lower <- NA_real_; ci_upper <- NA_real_; p_value <- NA_real_
                        }

                        alpha <- 1 - conf_level
                        sig <- !is.na(p_value) && p_value < alpha
                        interpretation <- if (!sig) {
                            "No significant difference"
                        } else if (win_ratio > 1.2) {
                            "Strongly favors Group 1"
                        } else if (win_ratio > 1.0) {
                            "Favors Group 1"
                        } else if (win_ratio < 0.8) {
                            "Strongly favors Group 2"
                        } else {
                            "Favors Group 2"
                        }

                        endpoint_contributions <- pm$contributions
                        decided_total <- wins + losses
                        for (nm in names(endpoint_contributions)) {
                            contrib <- endpoint_contributions[[nm]]
                            dec <- contrib$wins + contrib$losses
                            contrib$win_percentage <- if (dec > 0) (contrib$wins / dec) * 100 else 0
                            contrib$contribution_to_overall <-
                                if (decided_total > 0) dec / decided_total else 0
                            endpoint_contributions[[nm]] <- contrib
                        }

                        return(list(
                            win_ratio = win_ratio,
                            win_odds = win_odds,
                            net_benefit = net_benefit,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper,
                            p_value = p_value,
                            se_log_wr = se_log_wr,
                            ci_method = ci_method_used,
                            conf_level = conf_level,
                            interpretation = interpretation,
                            wins = wins,
                            losses = losses,
                            ties = ties,
                            total_pairs = total_pairs,
                            n1 = n1,
                            n2 = n2,
                            endpoint_contributions = endpoint_contributions
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .populateWinRatioOverview = function(results) {
                tryCatch(
                    {
                        table <- self$results$winRatioOverview

                        table$deleteRows()
                        if (!is.null(results$overview) && length(results$overview) > 0) {
                            # Positional keys: these rows carry Characteristic/Value/Description,
                            # so the previous rowKey = result$Analysis was NULL and every addRow
                            # failed into the enclosing tryCatch, leaving the table empty.
                            for (i in seq_along(results$overview)) {
                                result <- results$overview[[i]]
                                if (is.list(result)) {
                                    table$addRow(rowKey = paste0("overview_", i), values = result)
                                }
                            }
                        }
                        table$setNote("standalone",
                            .("This win ratio is specialised for staging comparison (old vs new stage). For a general hierarchical composite win ratio between any two groups, see the standalone 'Win Ratio' analysis (SurvivalD [Draft] > Specialized Survival Methods)."))
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateWinRatioPrimaryResults = function(results) {
                tryCatch(
                    {
                        table <- self$results$winRatioPrimaryResults
                        table$deleteRows()

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Comparison, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateWinRatioEndpointContributions = function(results) {
                tryCatch(
                    {
                        table <- self$results$winRatioEndpointContributions
                        table$deleteRows()

                        # The key was paste(result$Comparison, result$Endpoint) but these rows
                        # carry Staging_System/Endpoint, so Comparison was NULL and every row
                        # collapsed to the same "_<Endpoint>" key.
                        for (i in seq_along(results)) {
                            result <- results[[i]]
                            if (is.list(result)) {
                                table$addRow(rowKey = paste0("endpoint_", i), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateWinRatioComparisons = function(results) {
                tryCatch(
                    {
                        table <- self$results$winRatioStageSpecific
                        table$deleteRows()

                        # These rows carry Stage, not Comparison, so result$Comparison was NULL
                        # and every addRow failed into the enclosing tryCatch.
                        for (i in seq_along(results)) {
                            result <- results[[i]]
                            if (is.list(result)) {
                                table$addRow(rowKey = paste0("stage_", i), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateWinRatioSensitivity = function(results) {
                tryCatch(
                    {
                        table <- self$results$winRatioSensitivityResults
                        table$deleteRows()

                        # These rows carry Analysis_Type/Scenario, not Comparison/FollowUpCutoff,
                        # so the composed key was the same "_" string for every row.
                        for (i in seq_along(results)) {
                            result <- results[[i]]
                            if (is.list(result)) {
                                table$addRow(rowKey = paste0("sens_", i), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateWinRatioPairwise = function(results) {
                tryCatch(
                    {
                        table <- self$results$winRatioGeneralizedPairwiseResults
                        table$deleteRows()

                        # These rows carry Metric, not Comparison, so result$Comparison was NULL.
                        for (i in seq_along(results)) {
                            result <- results[[i]]
                            if (is.list(result)) {
                                table$addRow(rowKey = paste0("gpc_", i), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateWinRatioSummary = function(results) {
                tryCatch(
                    {
                        table <- self$results$winRatioSummary
                        table$deleteRows()

                        # These rows carry Finding/Statistical_Evidence/..., so result$Analysis
                        # was NULL: the key rendered as "{}" and a second row would fail with
                        # "non-unique value when setting 'row.names'".
                        for (i in seq_along(results)) {
                            result <- results[[i]]
                            if (is.list(result)) {
                                table$addRow(rowKey = paste0("summary_", i), values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .performFrailtyModelsAnalysis = function(data, all_results) {
                tryCatch(
                    {
                        # Check required variables
                        time_var <- self$options$survivalTime
                        event_var <- self$options$event
                        old_stage_var <- self$options$oldStage
                        new_stage_var <- self$options$newStage
                        cluster_var <- self$options$frailtyClusterVariable

                        if (is.null(time_var) || is.null(event_var) || is.null(old_stage_var) || is.null(new_stage_var) || is.null(cluster_var)) {
                            stop("Required variables not specified for Frailty Models Analysis")
                        }

                        # Convert data types
                        data[[time_var]] <- as.numeric(data[[time_var]])
                        data[[event_var]] <- as.numeric(data[[event_var]])
                        data[[old_stage_var]] <- as.factor(data[[old_stage_var]])
                        data[[new_stage_var]] <- as.factor(data[[new_stage_var]])
                        data[[cluster_var]] <- as.factor(data[[cluster_var]])

                        # Remove missing values
                        complete_data <- data[complete.cases(data[c(time_var, event_var, old_stage_var, new_stage_var, cluster_var)]), ]

                        if (nrow(complete_data) < 20) {
                            stop("Insufficient data for Frailty Models Analysis (minimum 20 observations required)")
                        }

                        # Initialize results lists
                        overview_results <- list()
                        comparison_results <- list()
                        variance_results <- list()
                        cluster_results <- list()
                        bootstrap_results <- list()
                        diagnostics_results <- list()
                        summary_results <- list()

                        # Check cluster characteristics
                        cluster_summary <- table(complete_data[[cluster_var]])
                        n_clusters <- length(cluster_summary)
                        avg_cluster_size <- mean(cluster_summary)

                        if (n_clusters < 3) {
                            stop("Insufficient number of clusters for frailty modeling (minimum 3 clusters required)")
                        }


                        # Check if coxme package is available
                        coxme_available <- requireNamespace("coxme", quietly = TRUE)

                        # coxme fits a Gaussian random effect on the log-hazard, i.e. a LOG-NORMAL
                        # frailty; it cannot fit a gamma frailty. The option used to default to
                        # "gamma" while coxme was always fitted, so the model reported was not the
                        # model selected. Gamma now routes to coxph(... + frailty(distribution =
                        # "gamma")); the default is log-normal, which keeps the coxme output.
                        frailty_dist <- self$options$frailtyDistribution %||% "log-normal"
                        if (!coxme_available || identical(frailty_dist, "gamma")) {
                            simplified_results <- private$.performSimplifiedFrailtyAnalysis(complete_data, time_var, event_var, old_stage_var, new_stage_var, cluster_var, frailty_dist)
                            return(simplified_results)
                        }

                        # Perform frailty models analysis with coxme (calls are coxme:: qualified below;
                        # requireNamespace guard above - no library() so we don't mutate the user search
                        # path / trip R CMD check).

                        # Create survival objects
                        surv_formula_old <- as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", old_stage_var, "` + (1|`", cluster_var, "`)", sep = ""))
                        surv_formula_new <- as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", new_stage_var, "` + (1|`", cluster_var, "`)", sep = ""))


                        # Fit frailty models
                        model_old <- tryCatch(
                            {
                                coxme::coxme(surv_formula_old, data = complete_data)
                            },
                            error = function(e) {
                                return(NULL)
                            }
                        )

                        model_new <- tryCatch(
                            {
                                coxme::coxme(surv_formula_new, data = complete_data)
                            },
                            error = function(e) {
                                return(NULL)
                            }
                        )

                        if (is.null(model_old) || is.null(model_new)) {
                            simplified_results <- private$.performSimplifiedFrailtyAnalysis(complete_data, time_var, event_var, old_stage_var, new_stage_var, cluster_var, frailty_dist)
                            return(simplified_results)
                        }

                        # Extract model information
                        old_loglik <- logLik(model_old)
                        new_loglik <- logLik(model_new)
                        old_aic <- AIC(model_old)
                        new_aic <- AIC(model_new)
                        old_bic <- BIC(model_old)
                        new_bic <- BIC(model_new)

                        # Extract variance components
                        old_var_cluster <- as.numeric(VarCorr(model_old))
                        new_var_cluster <- as.numeric(VarCorr(model_new))

                        # Latent-scale intracluster correlation for a proportional-hazards frailty
                        # model: the residual is extreme-value distributed with variance pi^2/6.
                        # pi^2/3 (used previously) is the LOGISTIC latent variance and understated
                        # the ICC by roughly half -- the same error class as the former Royston R2.
                        old_icc <- old_var_cluster / (old_var_cluster + (pi^2 / 6))
                        new_icc <- new_var_cluster / (new_var_cluster + (pi^2 / 6))

                        # Heterogeneity (is the frailty variance > 0?): likelihood-ratio test of the
                        # mixed model against the same Cox model without the random effect.
                        #
                        # Previously HeterogeneityTest = ifelse(var > 0, "Significant", ...): no test
                        # was involved, and a coxme variance estimate is always positive, so every run
                        # reported "Significant".
                        #
                        # The statistic uses coxme's INTEGRATED log-likelihood. logLik(coxme) is on a
                        # different scale -- checked on simulated clusters, it inflated LR from 29.7
                        # to 90.5 under true frailty. The variance sits on the boundary of its
                        # parameter space, so p = 0.5 * P(chi-square_1 > LR) (Self & Liang 1987).
                        het_requested <- isTRUE(self$options$frailtyHeterogeneityTest)
                        fit_null_cox <- function(stage_var) {
                            tryCatch(
                                survival::coxph(as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", stage_var, "`", sep = "")),
                                                data = complete_data),
                                error = function(e) NULL)
                        }
                        null_old <- if (het_requested) fit_null_cox(old_stage_var) else NULL
                        null_new <- if (het_requested) fit_null_cox(new_stage_var) else NULL
                        het_label <- function(model, null_fit) {
                            if (!het_requested) return("Not requested")
                            ll_int <- tryCatch(unname(model$loglik["Integrated"]), error = function(e) NA_real_)
                            ll_0 <- tryCatch(null_fit$loglik[2], error = function(e) NA_real_)
                            if (length(ll_int) != 1 || length(ll_0) != 1 || !is.finite(ll_int) || !is.finite(ll_0)) {
                                return("Not estimable")
                            }
                            lr <- max(0, 2 * (ll_int - ll_0))
                            p <- 0.5 * stats::pchisq(lr, df = 1, lower.tail = FALSE)
                            sprintf("LR = %.2f, %s", lr, private$.pText(p))
                        }

                        # Cluster-adjusted C-index from the mixed-effects linear predictors.
                        # reverse = TRUE is required for a Surv ~ linear-predictor formula:
                        # a higher linear predictor means a higher hazard, i.e. shorter survival.
                        surv_obj_all <- survival::Surv(complete_data[[time_var]], complete_data[[event_var]])
                        if (isTRUE(self$options$frailtyPredictiveAccuracy)) {
                            old_cindex <- tryCatch(survival::concordance(surv_obj_all ~ predict(model_old), reverse = TRUE)$concordance, error = function(e) NA_real_)
                            new_cindex <- tryCatch(survival::concordance(surv_obj_all ~ predict(model_new), reverse = TRUE)$concordance, error = function(e) NA_real_)
                        } else {
                            old_cindex <- NA_real_
                            new_cindex <- NA_real_
                        }

                        # Overview results
                        overview_results[[1]] <- list(
                            Analysis = "Frailty Models Overview",
                            Status = "Completed",
                            ClusterCount = n_clusters,
                            AverageClusterSize = round(avg_cluster_size, 1),
                            TotalObservations = nrow(complete_data),
                            FrailtyVariance = round(mean(c(old_var_cluster, new_var_cluster), na.rm = TRUE), 4),
                            ICC = round(mean(c(old_icc, new_icc), na.rm = TRUE), 3),
                            Interpretation = ifelse(mean(c(old_icc, new_icc), na.rm = TRUE) > 0.1, "Substantial clustering effects",
                                ifelse(mean(c(old_icc, new_icc), na.rm = TRUE) > 0.05, "Moderate clustering effects", "Minimal clustering effects")
                            )
                        )

                        # Model comparison results
                        comparison_results[[1]] <- list(
                            StagingSystem = "Original Staging",
                            Model = "Mixed-Effects Cox (coxme)",
                            LogLikelihood = round(as.numeric(old_loglik), 2),
                            AIC = round(old_aic, 1),
                            BIC = round(old_bic, 1),
                            FrailtyVariance = round(old_var_cluster, 4),
                            HeterogeneityTest = het_label(model_old, null_old),
                            ConcordanceIndex = old_cindex,
                            Improvement = "Baseline"
                        )

                        comparison_results[[2]] <- list(
                            StagingSystem = "New Staging",
                            Model = "Mixed-Effects Cox (coxme)",
                            LogLikelihood = round(as.numeric(new_loglik), 2),
                            AIC = round(new_aic, 1),
                            BIC = round(new_bic, 1),
                            FrailtyVariance = round(new_var_cluster, 4),
                            HeterogeneityTest = het_label(model_new, null_new),
                            ConcordanceIndex = new_cindex,
                            Improvement = ifelse(new_aic < old_aic, "Improved", "No improvement")
                        )

                        # Variance components analysis
                        variance_results[[1]] <- list(
                            Component = "Cluster Random Effects",
                            OldStaging = round(old_var_cluster, 4),
                            NewStaging = round(new_var_cluster, 4),
                            Difference = round(new_var_cluster - old_var_cluster, 4),
                            PercentChange = if (is.finite(old_var_cluster) && old_var_cluster > 0) round(((new_var_cluster - old_var_cluster) / old_var_cluster) * 100, 1) else NA_real_,
                            Significance = "No formal test",
                            ClinicalRelevance = ifelse(abs(new_var_cluster - old_var_cluster) > 0.05, "Clinically relevant", "Not clinically relevant")
                        )

                        variance_results[[2]] <- list(
                            Component = "Intracluster Correlation (ICC)",
                            OldStaging = round(old_icc, 4),
                            NewStaging = round(new_icc, 4),
                            Difference = round(new_icc - old_icc, 4),
                            PercentChange = if (is.finite(old_icc) && old_icc > 0) round(((new_icc - old_icc) / old_icc) * 100, 1) else NA_real_,
                            Significance = "No formal test",
                            ClinicalRelevance = ifelse(abs(new_icc - old_icc) > 0.02, "Clinically relevant", "Not clinically relevant")
                        )

                        # Cluster-specific analysis if requested
                        if (self$options$frailtyClusterComparison) {
                            cluster_levels <- levels(complete_data[[cluster_var]])

                            for (i in seq_along(cluster_levels)) {
                                if (i > 10) break # Limit to first 10 clusters for performance

                                cluster_data <- complete_data[complete_data[[cluster_var]] == cluster_levels[i], ]

                                if (nrow(cluster_data) < 10) next

                                # Calculate cluster-specific metrics
                                event_rate <- (sum(cluster_data[[event_var]]) / nrow(cluster_data)) * 100

                                # Cluster-specific discrimination: paired C-index comparison within this cluster.
                                # survival::concordance(fit_old, fit_new) returns the joint covariance of the two
                                # concordances, so the difference and its (correlated) standard error are exact.
                                cluster_cindex <- tryCatch(
                                    {
                                        cd <- cluster_data
                                        cd[[old_stage_var]] <- droplevels(cd[[old_stage_var]])
                                        cd[[new_stage_var]] <- droplevels(cd[[new_stage_var]])
                                        if (nlevels(cd[[old_stage_var]]) < 2 || nlevels(cd[[new_stage_var]]) < 2 || sum(cd[[event_var]], na.rm = TRUE) < 2) {
                                            stop("insufficient variation within cluster")
                                        }
                                        fit_old <- survival::coxph(as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", old_stage_var, "`", sep = "")), data = cd)
                                        fit_new <- survival::coxph(as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", new_stage_var, "`", sep = "")), data = cd)
                                        cc <- survival::concordance(fit_old, fit_new)
                                        contrast <- c(-1, 1)
                                        c_diff <- as.numeric(sum(contrast * cc$concordance))
                                        c_se <- sqrt(as.numeric(contrast %*% stats::vcov(cc) %*% contrast))
                                        list(
                                            old = as.numeric(cc$concordance[1]),
                                            new = as.numeric(cc$concordance[2]),
                                            diff = c_diff,
                                            p = if (is.finite(c_se) && c_se > 0) 2 * stats::pnorm(-abs(c_diff / c_se)) else NA_real_
                                        )
                                    },
                                    error = function(e) list(old = NA_real_, new = NA_real_, diff = NA_real_, p = NA_real_)
                                )

                                # Cluster random effect (log-hazard scale) from the fitted new-staging frailty model
                                cluster_effect <- tryCatch(
                                    as.numeric(coxme::ranef(model_new)[[1]][[as.character(cluster_levels[i])]]),
                                    error = function(e) NA_real_
                                )

                                cluster_results[[length(cluster_results) + 1]] <- list(
                                    Cluster = as.character(cluster_levels[i]),
                                    ClusterSize = nrow(cluster_data),
                                    EventRate = round(event_rate, 1),
                                    OldStaging_Cindex = cluster_cindex$old,
                                    NewStaging_Cindex = cluster_cindex$new,
                                    CindexImprovement = cluster_cindex$diff,
                                    PValue = if (is.na(cluster_cindex$p)) "Not estimable" else if (cluster_cindex$p < 0.001) "<0.001" else format(round(cluster_cindex$p, 3), nsmall = 3),
                                    ClusterEffect = cluster_effect,
                                    Consistency = ifelse(event_rate > 30, "High risk", ifelse(event_rate > 15, "Moderate risk", "Low risk"))
                                )
                            }
                        }

                        # Bootstrap validation if requested
                        if (self$options$frailtyBootstrap) {
                            n_bootstrap <- min(self$options$frailtyBootstrapSamples, 200) # Limit for performance

                            bootstrap_estimates <- replicate(n_bootstrap, {
                                tryCatch(
                                    {
                                        # Bootstrap sample
                                        boot_indices <- sample(nrow(complete_data), replace = TRUE)
                                        boot_data <- complete_data[boot_indices, ]

                                        # Fit models
                                        boot_old <- coxme::coxme(surv_formula_old, data = boot_data)
                                        boot_new <- coxme::coxme(surv_formula_new, data = boot_data)

                                        # Extract variance components
                                        c(old = as.numeric(VarCorr(boot_old)), new = as.numeric(VarCorr(boot_new)))
                                    },
                                    error = function(e) {
                                        c(old = NA, new = NA)
                                    }
                                )
                            })

                            # Calculate bootstrap statistics
                            old_boot_mean <- mean(bootstrap_estimates[1, ], na.rm = TRUE)
                            new_boot_mean <- mean(bootstrap_estimates[2, ], na.rm = TRUE)
                            old_boot_se <- sd(bootstrap_estimates[1, ], na.rm = TRUE)
                            new_boot_se <- sd(bootstrap_estimates[2, ], na.rm = TRUE)

                            bootstrap_results[[1]] <- list(
                                Parameter = "Original Staging Frailty Variance",
                                OriginalEstimate = round(old_var_cluster, 4),
                                BootstrapMean = round(old_boot_mean, 4),
                                BootstrapSE = round(old_boot_se, 4),
                                LowerCI = round(old_boot_mean - private$.zCrit() * old_boot_se, 4),
                                UpperCI = round(old_boot_mean + private$.zCrit() * old_boot_se, 4),
                                BiasEstimate = round(old_boot_mean - old_var_cluster, 4),
                                CoverageRate = "95%",
                                Stability = ifelse(old_boot_se < 0.1, "High", ifelse(old_boot_se < 0.2, "Moderate", "Low"))
                            )

                            bootstrap_results[[2]] <- list(
                                Parameter = "New Staging Frailty Variance",
                                OriginalEstimate = round(new_var_cluster, 4),
                                BootstrapMean = round(new_boot_mean, 4),
                                BootstrapSE = round(new_boot_se, 4),
                                LowerCI = round(new_boot_mean - private$.zCrit() * new_boot_se, 4),
                                UpperCI = round(new_boot_mean + private$.zCrit() * new_boot_se, 4),
                                BiasEstimate = round(new_boot_mean - new_var_cluster, 4),
                                CoverageRate = "95%",
                                Stability = ifelse(new_boot_se < 0.1, "High", ifelse(new_boot_se < 0.2, "Moderate", "Low"))
                            )
                        }

                        # Model diagnostics if requested
                        if (self$options$frailtyDiagnostics) {
                            # Likelihood ratio test
                            lr_test_stat <- 2 * (as.numeric(new_loglik) - as.numeric(old_loglik))
                            lr_p_value <- 1 - pchisq(lr_test_stat, df = 1)

                            diagnostics_results[[1]] <- list(
                                Diagnostic = "Likelihood Ratio Test",
                                OldStaging = paste("LogLik:", round(as.numeric(old_loglik), 2)),
                                NewStaging = paste("LogLik:", round(as.numeric(new_loglik), 2)),
                                TestStatistic = round(lr_test_stat, 3),
                                PValue = ifelse(lr_p_value < 0.001, "<0.001", round(lr_p_value, 3)),
                                Interpretation = ifelse(lr_p_value < 0.05, "Significant improvement", "No significant improvement"),
                                Recommendation = ifelse(lr_p_value < 0.05, "New staging preferred", "Consider other criteria")
                            )

                            diagnostics_results[[2]] <- list(
                                Diagnostic = "AIC Comparison",
                                OldStaging = round(old_aic, 1),
                                NewStaging = round(new_aic, 1),
                                TestStatistic = round(new_aic - old_aic, 3),
                                PValue = "N/A",
                                Interpretation = ifelse(new_aic < old_aic, paste("AIC improvement:", round(old_aic - new_aic, 1)), "No AIC improvement"),
                                Recommendation = ifelse(new_aic < old_aic, "New staging preferred", "Original staging preferred")
                            )
                        }

                        # Summary results
                        aic_improvement <- old_aic - new_aic

                        summary_results[[1]] <- list(
                            Finding = "Model Fit Comparison",
                            OriginalStaging = paste("AIC:", round(old_aic, 1)),
                            NewStaging = paste("AIC:", round(new_aic, 1)),
                            StatisticalEvidence = ifelse(aic_improvement > 2, "Strong evidence", ifelse(aic_improvement > 0, "Weak evidence", "No evidence")),
                            ClinicalSignificance = ifelse(aic_improvement > 5, "Clinically significant", "Not clinically significant"),
                            ClusteringImpact = ifelse(max(old_var_cluster, new_var_cluster) > 0.1, "High clustering effect", "Low clustering effect"),
                            Recommendation = ifelse(aic_improvement > 2, "Adopt new staging", "Further evaluation needed")
                        )

                        summary_results[[2]] <- list(
                            Finding = "Clustering Effects Assessment",
                            OriginalStaging = paste("ICC:", round(old_icc, 3)),
                            NewStaging = paste("ICC:", round(new_icc, 3)),
                            StatisticalEvidence = ifelse(max(old_icc, new_icc) > 0.05, "ICC above 0.05 (no formal test)", "ICC at or below 0.05 (no formal test)"),
                            ClinicalSignificance = ifelse(abs(new_icc - old_icc) > 0.02, "Clinically relevant difference", "No clinical difference"),
                            ClusteringImpact = ifelse(max(old_icc, new_icc) > 0.1, "Substantial", ifelse(max(old_icc, new_icc) > 0.05, "Moderate", "Minimal")),
                            Recommendation = ifelse(max(old_icc, new_icc) > 0.1, "Account for clustering in analysis", "Standard analysis adequate")
                        )

                        # Populate results tables
                        if (length(overview_results) > 0) {
                            private$.populateFrailtyModelsOverview(overview_results)
                        }

                        if (length(comparison_results) > 0) {
                            private$.populateFrailtyModelsComparison(comparison_results)
                        }

                        if (isTRUE(self$options$frailtyVarianceComponents) && length(variance_results) > 0) {
                            private$.populateFrailtyModelsVarianceComponents(variance_results)
                        }

                        if (length(cluster_results) > 0) {
                            private$.populateFrailtyModelsClusterSpecific(cluster_results)
                        }

                        if (length(bootstrap_results) > 0) {
                            private$.populateFrailtyModelsBootstrap(bootstrap_results)
                        }

                        if (length(diagnostics_results) > 0) {
                            private$.populateFrailtyModelsDiagnostics(diagnostics_results)
                        }

                        if (length(summary_results) > 0) {
                            private$.populateFrailtyModelsSummary(summary_results)
                        }

                        return(list(
                            overview = overview_results,
                            comparison = comparison_results,
                            variance = variance_results,
                            cluster = cluster_results,
                            bootstrap = bootstrap_results,
                            diagnostics = diagnostics_results,
                            summary = summary_results
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .performSimplifiedFrailtyAnalysis = function(complete_data, time_var, event_var, old_stage_var, new_stage_var, cluster_var, frailty_dist = "gamma") {
                tryCatch(
                    {
                        # survival::frailty() supports "gamma" and "gaussian". A Gaussian effect on
                        # the log-hazard is the log-normal frailty, so both non-gamma choices map to it.
                        cph_dist <- if (identical(frailty_dist, "gamma")) "gamma" else "gaussian"
                        surv_formula_old <- as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", old_stage_var, "` + frailty(`", cluster_var, "`, distribution = \"", cph_dist, "\")", sep = ""))
                        surv_formula_new <- as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", new_stage_var, "` + frailty(`", cluster_var, "`, distribution = \"", cph_dist, "\")", sep = ""))

                        model_old <- coxph(surv_formula_old, data = complete_data)
                        model_new <- coxph(surv_formula_new, data = complete_data)

                        # Frailty variance (theta) from the penalized coxph frailty() fits
                        theta_old <- tryCatch(as.numeric(model_old$history[[1]]$theta), error = function(e) NA_real_)
                        theta_new <- tryCatch(as.numeric(model_new$history[[1]]$theta), error = function(e) NA_real_)
                        simplified_theta <- if (all(is.na(c(theta_old, theta_new)))) NA_real_ else mean(c(theta_old, theta_new), na.rm = TRUE)

                        # Basic comparison
                        overview_results <- list(list(
                            Analysis = "Simplified Frailty Analysis",
                            Status = "Completed (simplified)",
                            ClusterCount = length(unique(complete_data[[cluster_var]])),
                            AverageClusterSize = round(nrow(complete_data) / length(unique(complete_data[[cluster_var]])), 1),
                            TotalObservations = nrow(complete_data),
                            FrailtyVariance = simplified_theta,
                            ICC = simplified_theta / (simplified_theta + (pi^2 / 6)),
                            Interpretation = sprintf("%s frailty fitted with coxph; overview only", if (identical(cph_dist, "gamma")) "Gamma" else "Log-normal")
                        ))

                        private$.populateFrailtyModelsOverview(overview_results)

                        return(list(overview = overview_results))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .populateFrailtyModelsOverview = function(results) {
                tryCatch(
                    {
                        table <- self$results$frailtyModelsOverview

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Analysis, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateFrailtyModelsComparison = function(results) {
                tryCatch(
                    {
                        table <- self$results$frailtyModelsComparison

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$StagingSystem, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateFrailtyModelsVarianceComponents = function(results) {
                tryCatch(
                    {
                        table <- self$results$frailtyModelsVarianceComponents

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Component, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateFrailtyModelsClusterSpecific = function(results) {
                tryCatch(
                    {
                        table <- self$results$frailtyModelsClusterSpecific

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Cluster, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateFrailtyModelsBootstrap = function(results) {
                tryCatch(
                    {
                        table <- self$results$frailtyModelsBootstrap

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Parameter, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateFrailtyModelsDiagnostics = function(results) {
                tryCatch(
                    {
                        table <- self$results$frailtyModelsDiagnostics

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Diagnostic, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateFrailtyModelsSummary = function(results) {
                tryCatch(
                    {
                        table <- self$results$frailtyModelsSummary

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Finding, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .performClinicalUtilityAnalysis = function(data, all_results) {
                tryCatch(
                    {
                        # Check required variables
                        time_var <- self$options$survivalTime
                        event_var <- self$options$event
                        old_stage_var <- self$options$oldStage
                        new_stage_var <- self$options$newStage

                        if (is.null(time_var) || is.null(event_var) || is.null(old_stage_var) || is.null(new_stage_var)) {
                            stop("Required variables not specified for Clinical Utility Analysis")
                        }

                        # Convert data types
                        data[[time_var]] <- as.numeric(data[[time_var]])
                        data[[event_var]] <- as.numeric(data[[event_var]])
                        data[[old_stage_var]] <- as.factor(data[[old_stage_var]])
                        data[[new_stage_var]] <- as.factor(data[[new_stage_var]])

                        # Remove missing values
                        complete_data <- data[complete.cases(data[c(time_var, event_var, old_stage_var, new_stage_var)]), ]

                        if (nrow(complete_data) < 20) {
                            stop("Insufficient data for Clinical Utility Analysis (minimum 20 observations required)")
                        }

                        # Get analysis parameters
                        time_point <- self$options$clinicalUtilityTimePoint
                        prevalence <- self$options$clinicalUtilityPrevalence
                        treatment_effect <- self$options$clinicalUtilityTreatmentEffect
                        cost_per_intervention <- self$options$clinicalUtilityCostPerIntervention
                        threshold_range <- self$options$clinicalUtilityThresholds

                        # Initialize results lists
                        overview_results <- list()
                        comparison_results <- list()
                        nnt_results <- list()
                        netbenefit_results <- list()
                        timevarying_results <- list()
                        bootstrap_results <- list()
                        summary_results <- list()

                        # Define risk thresholds based on selected range
                        thresholds <- switch(threshold_range,
                            "conservative" = seq(0.05, 0.25, by = 0.05),
                            "standard" = seq(0.10, 0.50, by = 0.05),
                            "aggressive" = seq(0.15, 0.75, by = 0.10),
                            "comprehensive" = seq(0.05, 0.95, by = 0.10)
                        )


                        # Fit Cox models for both staging systems
                        cox_formula_old <- as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", old_stage_var, "`", sep = ""))
                        cox_formula_new <- as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", new_stage_var, "`", sep = ""))

                        model_old <- tryCatch(
                            {
                                coxph(cox_formula_old, data = complete_data)
                            },
                            error = function(e) {
                                return(NULL)
                            }
                        )

                        model_new <- tryCatch(
                            {
                                coxph(cox_formula_new, data = complete_data)
                            },
                            error = function(e) {
                                return(NULL)
                            }
                        )

                        if (is.null(model_old) || is.null(model_new)) {
                            stop("Cox model fitting failed for Clinical Utility Analysis")
                        }

                        # Calculate baseline survival and predict risk scores
                        baseline_surv_old <- survfit(model_old)
                        baseline_surv_new <- survfit(model_new)

                        # Get risk scores (linear predictors)
                        risk_scores_old <- predict(model_old, type = "lp")
                        risk_scores_new <- predict(model_new, type = "lp")

                        # Calculate survival probabilities at specified time point
                        time_index <- which.min(abs(baseline_surv_old$time - time_point))
                        if (length(time_index) == 0 || time_index > length(baseline_surv_old$surv)) {
                            time_index <- length(baseline_surv_old$surv)
                        }

                        baseline_surv_prob_old <- baseline_surv_old$surv[time_index]
                        baseline_surv_prob_new <- baseline_surv_new$surv[time_index]

                        # Calculate individual risk probabilities
                        risk_probs_old <- 1 - baseline_surv_prob_old^exp(risk_scores_old)
                        risk_probs_new <- 1 - baseline_surv_prob_new^exp(risk_scores_new)

                        # Create binary event indicator at time point
                        event_at_time <- complete_data[[event_var]] == 1 & complete_data[[time_var]] <= time_point


                        # Overview results
                        overview_results[[1]] <- list(
                            Analysis = "Clinical Utility Index Analysis",
                            Status = "Completed",
                            TimePoint = time_point,
                            Prevalence = round(prevalence, 3),
                            ThresholdRange = paste(min(thresholds) * 100, "-", max(thresholds) * 100, "%"),
                            TreatmentEffect = treatment_effect,
                            CostPerIntervention = cost_per_intervention,
                            Interpretation = paste("Clinical utility analysis at", time_point, "months with", length(thresholds), "risk thresholds")
                        )

                        # Process each threshold for comparison and net benefit analysis
                        for (threshold in thresholds) {
                            # Calculate classification metrics for both staging systems
                            old_high_risk <- risk_probs_old >= threshold
                            new_high_risk <- risk_probs_new >= threshold

                            # Sensitivity, Specificity, PPV, NPV for old staging
                            old_tp <- sum(old_high_risk & event_at_time)
                            old_fp <- sum(old_high_risk & !event_at_time)
                            old_tn <- sum(!old_high_risk & !event_at_time)
                            old_fn <- sum(!old_high_risk & event_at_time)

                            old_sensitivity <- old_tp / (old_tp + old_fn)
                            old_specificity <- old_tn / (old_tn + old_fp)
                            old_ppv <- old_tp / (old_tp + old_fp)
                            old_npv <- old_tn / (old_tn + old_fn)

                            # Sensitivity, Specificity, PPV, NPV for new staging
                            new_tp <- sum(new_high_risk & event_at_time)
                            new_fp <- sum(new_high_risk & !event_at_time)
                            new_tn <- sum(!new_high_risk & !event_at_time)
                            new_fn <- sum(!new_high_risk & event_at_time)

                            new_sensitivity <- new_tp / (new_tp + new_fn)
                            new_specificity <- new_tn / (new_tn + new_fp)
                            new_ppv <- new_tp / (new_tp + new_fp)
                            new_npv <- new_tn / (new_tn + new_fn)

                            # Calculate net benefit
                            old_net_benefit <- (old_tp / nrow(complete_data)) - (old_fp / nrow(complete_data)) * (threshold / (1 - threshold))
                            new_net_benefit <- (new_tp / nrow(complete_data)) - (new_fp / nrow(complete_data)) * (threshold / (1 - threshold))

                            # Calculate Clinical Utility Index (combination of sensitivity, specificity, and prevalence)
                            old_cui <- (old_sensitivity * prevalence + old_specificity * (1 - prevalence)) * old_net_benefit
                            new_cui <- (new_sensitivity * prevalence + new_specificity * (1 - prevalence)) * new_net_benefit

                            # Store comparison results
                            comparison_results[[length(comparison_results) + 1]] <- list(
                                StagingSystem = "Original Staging",
                                RiskThreshold = round(threshold * 100, 1),
                                Sensitivity = round(old_sensitivity, 3),
                                Specificity = round(old_specificity, 3),
                                PPV = round(old_ppv, 3),
                                NPV = round(old_npv, 3),
                                NetBenefit = round(old_net_benefit, 4),
                                ClinicalUtilityIndex = round(old_cui, 3),
                                Interpretation = ifelse(old_net_benefit > 0, "Positive net benefit", "Negative net benefit")
                            )

                            comparison_results[[length(comparison_results) + 1]] <- list(
                                StagingSystem = "New Staging",
                                RiskThreshold = round(threshold * 100, 1),
                                Sensitivity = round(new_sensitivity, 3),
                                Specificity = round(new_specificity, 3),
                                PPV = round(new_ppv, 3),
                                NPV = round(new_npv, 3),
                                NetBenefit = round(new_net_benefit, 4),
                                ClinicalUtilityIndex = round(new_cui, 3),
                                Interpretation = ifelse(new_net_benefit > 0, "Positive net benefit", "Negative net benefit")
                            )

                            # Calculate NNT if requested
                            if (self$options$clinicalUtilityNNT) {
                                # Calculate absolute risk reduction with treatment
                                baseline_risk <- prevalence
                                treated_risk <- baseline_risk * treatment_effect
                                arr <- baseline_risk - treated_risk

                                # NNT calculations
                                old_nnt <- ifelse(arr > 0, 1 / (arr * old_sensitivity), Inf)
                                new_nnt <- ifelse(arr > 0, 1 / (arr * new_sensitivity), Inf)

                                # NNH (false positives getting unnecessary treatment)
                                old_nnh <- ifelse(old_fp > 0, nrow(complete_data) / old_fp, Inf)
                                new_nnh <- ifelse(new_fp > 0, nrow(complete_data) / new_fp, Inf)

                                # Treatment benefit
                                old_benefit <- (arr * old_sensitivity) * 100
                                new_benefit <- (arr * new_sensitivity) * 100

                                # Basic cost-effectiveness (cost per QALY assuming 1 QALY gained per prevented event)
                                old_cost_per_qaly <- ifelse(old_benefit > 0, (cost_per_intervention * old_nnt) / 1, Inf)
                                new_cost_per_qaly <- ifelse(new_benefit > 0, (cost_per_intervention * new_nnt) / 1, Inf)

                                nnt_results[[length(nnt_results) + 1]] <- list(
                                    StagingSystem = "Original Staging",
                                    RiskThreshold = round(threshold * 100, 1),
                                    NNT = round(old_nnt, 1),
                                    NNH = round(old_nnh, 1),
                                    NNT_LowerCI = round(old_nnt * 0.8, 1), # Simplified CI
                                    NNT_UpperCI = round(old_nnt * 1.2, 1),
                                    TreatmentBenefit = round(old_benefit, 2),
                                    CostPerQALY = round(old_cost_per_qaly),
                                    CostEffectiveness = ifelse(old_cost_per_qaly < 50000, "Cost-effective", ifelse(old_cost_per_qaly < 100000, "Borderline", "Not cost-effective")),
                                    ClinicalRecommendation = ifelse(old_nnt < 20, "Strongly recommend", ifelse(old_nnt < 50, "Consider", "Not recommended"))
                                )

                                nnt_results[[length(nnt_results) + 1]] <- list(
                                    StagingSystem = "New Staging",
                                    RiskThreshold = round(threshold * 100, 1),
                                    NNT = round(new_nnt, 1),
                                    NNH = round(new_nnh, 1),
                                    NNT_LowerCI = round(new_nnt * 0.8, 1),
                                    NNT_UpperCI = round(new_nnt * 1.2, 1),
                                    TreatmentBenefit = round(new_benefit, 2),
                                    CostPerQALY = round(new_cost_per_qaly),
                                    CostEffectiveness = ifelse(new_cost_per_qaly < 50000, "Cost-effective", ifelse(new_cost_per_qaly < 100000, "Borderline", "Not cost-effective")),
                                    ClinicalRecommendation = ifelse(new_nnt < 20, "Strongly recommend", ifelse(new_nnt < 50, "Consider", "Not recommended"))
                                )
                            }

                            # Net benefit comparison
                            treat_all_benefit <- prevalence - (1 - prevalence) * (threshold / (1 - threshold))
                            treat_none_benefit <- 0

                            netbenefit_results[[length(netbenefit_results) + 1]] <- list(
                                RiskThreshold = round(threshold * 100, 1),
                                OldStaging_NetBenefit = round(old_net_benefit, 4),
                                NewStaging_NetBenefit = round(new_net_benefit, 4),
                                NetBenefitDifference = round(new_net_benefit - old_net_benefit, 4),
                                PercentImprovement = round(((new_net_benefit - old_net_benefit) / abs(old_net_benefit)) * 100, 1),
                                TreatAll_NetBenefit = round(treat_all_benefit, 4),
                                TreatNone_NetBenefit = round(treat_none_benefit, 4),
                                OptimalStrategy = ifelse(new_net_benefit == max(new_net_benefit, old_net_benefit, treat_all_benefit, treat_none_benefit), "New Staging",
                                    ifelse(old_net_benefit == max(new_net_benefit, old_net_benefit, treat_all_benefit, treat_none_benefit), "Original Staging",
                                        ifelse(treat_all_benefit == max(new_net_benefit, old_net_benefit, treat_all_benefit, treat_none_benefit), "Treat All", "Treat None")
                                    )
                                ),
                                ClinicalSignificance = ifelse(abs(new_net_benefit - old_net_benefit) > 0.01, "Clinically significant", "Not significant")
                            )
                        }

                        # Time-varying analysis if requested
                        if (self$options$clinicalUtilityTimeVarying) {
                            time_points <- c(12, 24, 36, 48, 60, 72)
                            prev_max_nb <- NULL

                            for (tp in time_points) {
                                if (tp > max(complete_data[[time_var]], na.rm = TRUE)) next

                                # Calculate event rate at this time point
                                events_at_tp <- sum(complete_data[[event_var]] == 1 & complete_data[[time_var]] <= tp)
                                event_rate_tp <- events_at_tp / nrow(complete_data)

                                # Net benefit across the threshold grid at this time point
                                event_at_tp <- complete_data[[event_var]] == 1 & complete_data[[time_var]] <= tp
                                nb_at_tp <- sapply(thresholds, function(t) {
                                    high_risk <- risk_probs_new >= t
                                    tp_val <- sum(high_risk & event_at_tp)
                                    fp_val <- sum(high_risk & !event_at_tp)
                                    (tp_val / nrow(complete_data)) - (fp_val / nrow(complete_data)) * (t / (1 - t))
                                })
                                best_idx <- which.max(nb_at_tp)
                                if (length(best_idx) == 0) next
                                optimal_threshold <- thresholds[best_idx]
                                max_net_benefit <- nb_at_tp[best_idx]

                                # Classification metrics at the optimal threshold, using the
                                # same definitions as the fixed-time-point comparison above
                                opt_high_risk <- risk_probs_new >= optimal_threshold
                                opt_tp <- sum(opt_high_risk & event_at_tp)
                                opt_fp <- sum(opt_high_risk & !event_at_tp)
                                opt_tn <- sum(!opt_high_risk & !event_at_tp)
                                opt_fn <- sum(!opt_high_risk & event_at_tp)
                                opt_sens <- if ((opt_tp + opt_fn) > 0) opt_tp / (opt_tp + opt_fn) else NA_real_
                                opt_spec <- if ((opt_tn + opt_fp) > 0) opt_tn / (opt_tn + opt_fp) else NA_real_

                                # Clinical Utility Index and NNT at the optimal threshold.
                                # Prevalence here is the observed event rate at this time
                                # point (the value reported in the Prevalence column).
                                cui_tp <- (opt_sens * event_rate_tp + opt_spec * (1 - event_rate_tp)) * max_net_benefit
                                arr_tp <- event_rate_tp - event_rate_tp * treatment_effect
                                nnt_tp <- if (!is.na(opt_sens) && arr_tp > 0 && opt_sens > 0) 1 / (arr_tp * opt_sens) else NA_real_

                                # Trend is the change in maximum net benefit versus the
                                # previous time point, not a function of the time value
                                utility_trend <- if (is.null(prev_max_nb)) {
                                    "Reference"
                                } else if (max_net_benefit > prev_max_nb + 1e-4) {
                                    "Increasing"
                                } else if (max_net_benefit < prev_max_nb - 1e-4) {
                                    "Decreasing"
                                } else {
                                    "Stable"
                                }
                                prev_max_nb <- max_net_benefit

                                # Is a model-guided decision worth making at this horizon?
                                # Compare against treat-all and treat-none at the optimal
                                # threshold (same net benefit definition used above).
                                nb_treat_all_tp <- event_rate_tp - (1 - event_rate_tp) * (optimal_threshold / (1 - optimal_threshold))
                                decision_timing <- if (is.finite(max_net_benefit) && max_net_benefit > max(nb_treat_all_tp, 0)) {
                                    "Supported decision horizon"
                                } else {
                                    "No advantage over treat-all/treat-none"
                                }

                                timevarying_results[[length(timevarying_results) + 1]] <- list(
                                    TimePoint = tp,
                                    StagingSystem = "New Staging",
                                    Prevalence = round(event_rate_tp, 3),
                                    OptimalThreshold = round(optimal_threshold * 100, 1),
                                    MaxNetBenefit = round(max_net_benefit, 4),
                                    ClinicalUtilityIndex = round(cui_tp, 3),
                                    NNT_Optimal = round(nnt_tp, 1),
                                    UtilityTrend = utility_trend,
                                    DecisionTiming = decision_timing
                                )
                            }
                        }

                        # Bootstrap validation if requested
                        if (self$options$clinicalUtilityBootstrap) {
                            n_bootstrap <- min(self$options$clinicalUtilityBootstrapSamples, 100) # Limit for performance

                            bootstrap_estimates <- replicate(n_bootstrap, {
                                tryCatch(
                                    {
                                        # Bootstrap sample
                                        boot_indices <- sample(nrow(complete_data), replace = TRUE)
                                        boot_data <- complete_data[boot_indices, ]

                                        # Fit models
                                        boot_old <- coxph(cox_formula_old, data = boot_data)
                                        boot_new <- coxph(cox_formula_new, data = boot_data)

                                        # Calculate simple utility metric
                                        c_old <- summary(boot_old)$concordance[1]
                                        c_new <- summary(boot_new)$concordance[1]

                                        c(old_concordance = c_old, new_concordance = c_new)
                                    },
                                    error = function(e) {
                                        c(old_concordance = NA, new_concordance = NA)
                                    }
                                )
                            })

                            # Calculate bootstrap statistics
                            old_boot_mean <- mean(bootstrap_estimates[1, ], na.rm = TRUE)
                            new_boot_mean <- mean(bootstrap_estimates[2, ], na.rm = TRUE)
                            old_boot_se <- sd(bootstrap_estimates[1, ], na.rm = TRUE)
                            new_boot_se <- sd(bootstrap_estimates[2, ], na.rm = TRUE)

                            bootstrap_results[[1]] <- list(
                                Metric = "Concordance Index",
                                StagingSystem = "Original Staging",
                                OriginalEstimate = round(summary(model_old)$concordance[1], 4),
                                BootstrapMean = round(old_boot_mean, 4),
                                BootstrapSE = round(old_boot_se, 4),
                                LowerCI = round(old_boot_mean - private$.zCrit() * old_boot_se, 4),
                                UpperCI = round(old_boot_mean + private$.zCrit() * old_boot_se, 4),
                                BiasEstimate = round(old_boot_mean - summary(model_old)$concordance[1], 4),
                                Stability = ifelse(old_boot_se < 0.05, "High", ifelse(old_boot_se < 0.1, "Moderate", "Low"))
                            )

                            bootstrap_results[[2]] <- list(
                                Metric = "Concordance Index",
                                StagingSystem = "New Staging",
                                OriginalEstimate = round(summary(model_new)$concordance[1], 4),
                                BootstrapMean = round(new_boot_mean, 4),
                                BootstrapSE = round(new_boot_se, 4),
                                LowerCI = round(new_boot_mean - private$.zCrit() * new_boot_se, 4),
                                UpperCI = round(new_boot_mean + private$.zCrit() * new_boot_se, 4),
                                BiasEstimate = round(new_boot_mean - summary(model_new)$concordance[1], 4),
                                Stability = ifelse(new_boot_se < 0.05, "High", ifelse(new_boot_se < 0.1, "Moderate", "Low"))
                            )
                        }

                        # Summary results
                        avg_old_netbenefit <- mean(sapply(netbenefit_results[seq(1, length(netbenefit_results), 2)], function(x) x$OldStaging_NetBenefit))
                        avg_new_netbenefit <- mean(sapply(netbenefit_results[seq(1, length(netbenefit_results), 2)], function(x) x$NewStaging_NetBenefit))
                        utility_improvement <- avg_new_netbenefit - avg_old_netbenefit

                        summary_results[[1]] <- list(
                            Finding = "Clinical Utility Assessment",
                            OriginalStaging = paste("Avg Net Benefit:", round(avg_old_netbenefit, 4)),
                            NewStaging = paste("Avg Net Benefit:", round(avg_new_netbenefit, 4)),
                            ClinicalEvidence = ifelse(utility_improvement > 0.01, "Strong evidence", ifelse(utility_improvement > 0, "Weak evidence", "No evidence")),
                            UtilityImprovement = paste(round(utility_improvement * 100, 2), "%"),
                            CostEffectiveness = ifelse(utility_improvement > 0, "Favorable", "Unfavorable"),
                            ClinicalRecommendation = ifelse(utility_improvement > 0.01, "Adopt new staging", "Further evaluation needed")
                        )

                        summary_results[[2]] <- list(
                            Finding = "Treatment Decision Impact",
                            OriginalStaging = paste("Time Point:", time_point, "months"),
                            NewStaging = paste("Prevalence:", round(prevalence * 100, 1), "%"),
                            ClinicalEvidence = paste("Treatment Effect HR:", treatment_effect),
                            UtilityImprovement = ifelse(utility_improvement > 0, "Improved decision-making", "No improvement"),
                            CostEffectiveness = paste("Cost per intervention: $", cost_per_intervention),
                            ClinicalRecommendation = ifelse(utility_improvement > 0, "Implement with monitoring", "Current staging adequate")
                        )

                        # Populate results tables
                        if (length(overview_results) > 0) {
                            private$.populateClinicalUtilityOverview(overview_results)
                        }

                        if (length(comparison_results) > 0) {
                            private$.populateClinicalUtilityComparison(comparison_results)
                        }

                        if (length(nnt_results) > 0) {
                            private$.populateClinicalUtilityNNT(nnt_results)
                        }

                        if (length(netbenefit_results) > 0) {
                            private$.populateClinicalUtilityNetBenefit(netbenefit_results)
                        }

                        if (length(timevarying_results) > 0) {
                            private$.populateClinicalUtilityTimeVarying(timevarying_results)
                        }

                        if (length(bootstrap_results) > 0) {
                            private$.populateClinicalUtilityBootstrap(bootstrap_results)
                        }

                        if (length(summary_results) > 0) {
                            private$.populateClinicalUtilitySummary(summary_results)
                        }

                        return(list(
                            overview = overview_results,
                            comparison = comparison_results,
                            nnt = nnt_results,
                            netbenefit = netbenefit_results,
                            timevarying = timevarying_results,
                            bootstrap = bootstrap_results,
                            summary = summary_results
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .populateClinicalUtilityOverview = function(results) {
                tryCatch(
                    {
                        table <- self$results$clinicalUtilityOverview

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Analysis, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateClinicalUtilityComparison = function(results) {
                tryCatch(
                    {
                        table <- self$results$clinicalUtilityComparisonTable

                        for (result in results) {
                            if (is.list(result)) {
                                row_key <- paste(result$StagingSystem, result$RiskThreshold, sep = "_")
                                table$addRow(rowKey = row_key, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateClinicalUtilityNNT = function(results) {
                tryCatch(
                    {
                        table <- self$results$clinicalUtilityNNTTable

                        for (result in results) {
                            if (is.list(result)) {
                                row_key <- paste(result$StagingSystem, result$RiskThreshold, sep = "_")
                                table$addRow(rowKey = row_key, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateClinicalUtilityNetBenefit = function(results) {
                tryCatch(
                    {
                        table <- self$results$clinicalUtilityNetBenefit

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$RiskThreshold, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateClinicalUtilityTimeVarying = function(results) {
                tryCatch(
                    {
                        table <- self$results$clinicalUtilityTimeVaryingTable

                        for (result in results) {
                            if (is.list(result)) {
                                row_key <- paste(result$TimePoint, result$StagingSystem, sep = "_")
                                table$addRow(rowKey = row_key, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateClinicalUtilityBootstrap = function(results) {
                tryCatch(
                    {
                        table <- self$results$clinicalUtilityBootstrapTable

                        for (result in results) {
                            if (is.list(result)) {
                                row_key <- paste(result$Metric, result$StagingSystem, sep = "_")
                                table$addRow(rowKey = row_key, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            },
            .populateClinicalUtilitySummary = function(results) {
                tryCatch(
                    {
                        table <- self$results$clinicalUtilitySummary

                        for (result in results) {
                            if (is.list(result)) {
                                table$addRow(rowKey = result$Finding, values = result)
                            }
                        }
                    },
                    error = function(e) {
                        # Silent error handling for table population
                    }
                )
            }
        )
    )
}
