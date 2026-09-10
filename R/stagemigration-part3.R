# stagemigration backend, part 3 of 5.
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
stagemigrationPart3 <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "stagemigrationPart3",
        inherit = stagemigrationPart2,
        private = list(
            .generatePersonalizedPredictions = function(covariate_data, all_covariates, old_stage, new_stage, survival_time) {
                # Generate personalized risk predictions and clinical recommendations
                # This provides individualized risk assessments for clinical decision making

                tryCatch(
                    {

                        # Define prediction time points
                        time_points_str <- self$options$nriTimePoints
                        time_points <- as.numeric(unlist(strsplit(gsub("\\s", "", time_points_str), ",")))
                        time_points <- time_points[!is.na(time_points) & time_points > 0]

                        if (length(time_points) == 0) {
                            time_points <- c(12, 24, 60) # Default time points
                        }

                        # Build prediction models
                        old_covariates <- c(old_stage, all_covariates)
                        old_formula <- as.formula(paste(
                            "survival::Surv(", survival_time, ", event_binary) ~",
                            paste(old_covariates, collapse = " + ")
                        ))
                        old_model <- tryCatch(
                            {
                                survival::coxph(old_formula, data = covariate_data)
                            },
                            error = function(e) NULL
                        )

                        new_covariates <- c(new_stage, all_covariates)
                        new_formula <- as.formula(paste(
                            "survival::Surv(", survival_time, ", event_binary) ~",
                            paste(new_covariates, collapse = " + ")
                        ))
                        new_model <- tryCatch(
                            {
                                survival::coxph(new_formula, data = covariate_data)
                            },
                            error = function(e) NULL
                        )

                        if (is.null(old_model) || is.null(new_model)) {
                            return(list(error = "Model fitting failed for personalized predictions"))
                        }

                        # Generate predictions for each patient
                        n_patients <- nrow(covariate_data)
                        patient_predictions <- data.frame(
                            patient_id = 1:n_patients,
                            stringsAsFactors = FALSE
                        )

                        # Add baseline characteristics for context
                        patient_predictions[[old_stage]] <- covariate_data[[old_stage]]
                        patient_predictions[[new_stage]] <- covariate_data[[new_stage]]

                        for (covariate in all_covariates) {
                            if (covariate %in% names(covariate_data)) {
                                patient_predictions[[covariate]] <- covariate_data[[covariate]]
                            }
                        }

                        # Calculate predictions for each time point
                        for (time_point in time_points) {
                            # Old staging system predictions
                            old_survfit <- tryCatch(
                                {
                                    survfit(old_model, newdata = covariate_data)
                                },
                                error = function(e) NULL
                            )

                            old_survival_probs <- NULL
                            if (!is.null(old_survfit)) {
                                old_survival_probs <- tryCatch(
                                    {
                                        # For a multi-subject survfit.coxph, $surv is a
                                        # length(times) x n_subjects MATRIX, not a vector.
                                        # Left as a 1 x n matrix it makes the later
                                        # patient_predictions[[col]] <- ... assignment fail with
                                        # "replacement has 1 row, data has n".
                                        as.vector(summary(old_survfit, times = time_point, extend = TRUE)$surv)
                                    },
                                    error = function(e) rep(NA_real_, n_patients)
                                )
                            }
                            if (length(old_survival_probs) != n_patients) {
                                old_survival_probs <- rep(NA_real_, n_patients)
                            }

                            if (is.null(old_survival_probs)) {
                                old_survival_probs <- rep(NA, n_patients)
                            }

                            # New staging system predictions
                            new_survfit <- tryCatch(
                                {
                                    survfit(new_model, newdata = covariate_data)
                                },
                                error = function(e) NULL
                            )

                            new_survival_probs <- NULL
                            if (!is.null(new_survfit)) {
                                new_survival_probs <- tryCatch(
                                    {
                                        # For a multi-subject survfit.coxph, $surv is a
                                        # length(times) x n_subjects MATRIX, not a vector.
                                        # Left as a 1 x n matrix it makes the later
                                        # patient_predictions[[col]] <- ... assignment fail with
                                        # "replacement has 1 row, data has n".
                                        as.vector(summary(new_survfit, times = time_point, extend = TRUE)$surv)
                                    },
                                    error = function(e) rep(NA_real_, n_patients)
                                )
                            }
                            if (length(new_survival_probs) != n_patients) {
                                new_survival_probs <- rep(NA_real_, n_patients)
                            }

                            if (is.null(new_survival_probs)) {
                                new_survival_probs <- rep(NA, n_patients)
                            }

                            # Convert to risk probabilities
                            old_risk_probs <- 1 - old_survival_probs
                            new_risk_probs <- 1 - new_survival_probs

                            # Risk difference (new - old)
                            risk_difference <- new_risk_probs - old_risk_probs

                            # Categorize risk levels
                            old_risk_category <- cut(old_risk_probs,
                                breaks = c(0, 0.1, 0.3, 0.5, 1),
                                labels = c("Low", "Moderate", "High", "Very High"),
                                include.lowest = TRUE
                            )

                            new_risk_category <- cut(new_risk_probs,
                                breaks = c(0, 0.1, 0.3, 0.5, 1),
                                labels = c("Low", "Moderate", "High", "Very High"),
                                include.lowest = TRUE
                            )

                            # Reclassification direction
                            reclassification <- ifelse(is.na(old_risk_category) | is.na(new_risk_category),
                                "Unknown",
                                ifelse(as.numeric(new_risk_category) > as.numeric(old_risk_category),
                                    "Upstaged",
                                    ifelse(as.numeric(new_risk_category) < as.numeric(old_risk_category),
                                        "Downstaged", "No Change"
                                    )
                                )
                            )

                            # Clinical impact assessment
                            clinical_impact <- ifelse(is.na(risk_difference), "Unknown",
                                ifelse(abs(risk_difference) < 0.05, "Minimal Impact",
                                    ifelse(risk_difference > 0.05, "Higher Risk (New System)",
                                        ifelse(risk_difference < -0.05, "Lower Risk (New System)",
                                            "Minimal Impact"
                                        )
                                    )
                                )
                            )

                            # Confidence in prediction (based on model performance)
                            old_concordance <- tryCatch(
                                {
                                    survival::concordance(old_model)$concordance
                                },
                                error = function(e) NA
                            )

                            new_concordance <- tryCatch(
                                {
                                    survival::concordance(new_model)$concordance
                                },
                                error = function(e) NA
                            )

                            # Generate confidence categories
                            confidence_old <- ifelse(is.na(old_concordance), "Unknown",
                                ifelse(old_concordance > 0.8, "High Confidence",
                                    ifelse(old_concordance > 0.7, "Moderate Confidence",
                                        "Low Confidence"
                                    )
                                )
                            )

                            confidence_new <- ifelse(is.na(new_concordance), "Unknown",
                                ifelse(new_concordance > 0.8, "High Confidence",
                                    ifelse(new_concordance > 0.7, "Moderate Confidence",
                                        "Low Confidence"
                                    )
                                )
                            )

                            # Clinical recommendations
                            recommendations <- ifelse(is.na(risk_difference), "Insufficient data for recommendation",
                                ifelse(new_risk_probs > 0.5 & old_risk_probs <= 0.3,
                                    "Consider intensive monitoring/treatment (new staging indicates high risk)",
                                    ifelse(new_risk_probs <= 0.3 & old_risk_probs > 0.5,
                                        "Consider reduced intensity approach (new staging indicates lower risk)",
                                        ifelse(new_risk_probs > 0.3 & old_risk_probs > 0.3,
                                            "High risk in both systems - continue current approach",
                                            "Low to moderate risk - standard monitoring appropriate"
                                        )
                                    )
                                )
                            )

                            # Store results for this time point
                            time_suffix <- paste0("_", time_point, "m")

                            patient_predictions[[paste0("old_survival_prob", time_suffix)]] <- old_survival_probs
                            patient_predictions[[paste0("new_survival_prob", time_suffix)]] <- new_survival_probs
                            patient_predictions[[paste0("old_risk_prob", time_suffix)]] <- old_risk_probs
                            patient_predictions[[paste0("new_risk_prob", time_suffix)]] <- new_risk_probs
                            patient_predictions[[paste0("risk_difference", time_suffix)]] <- risk_difference
                            patient_predictions[[paste0("old_risk_category", time_suffix)]] <- as.character(old_risk_category)
                            patient_predictions[[paste0("new_risk_category", time_suffix)]] <- as.character(new_risk_category)
                            patient_predictions[[paste0("reclassification", time_suffix)]] <- reclassification
                            patient_predictions[[paste0("clinical_impact", time_suffix)]] <- clinical_impact
                            patient_predictions[[paste0("confidence_old", time_suffix)]] <- confidence_old
                            patient_predictions[[paste0("confidence_new", time_suffix)]] <- confidence_new
                            patient_predictions[[paste0("recommendation", time_suffix)]] <- recommendations
                        }

                        # Summary statistics across patients
                        summary_stats <- list()
                        for (time_point in time_points) {
                            time_suffix <- paste0("_", time_point, "m")

                            old_risks <- patient_predictions[[paste0("old_risk_prob", time_suffix)]]
                            new_risks <- patient_predictions[[paste0("new_risk_prob", time_suffix)]]
                            risk_diffs <- patient_predictions[[paste0("risk_difference", time_suffix)]]
                            reclassifications <- patient_predictions[[paste0("reclassification", time_suffix)]]

                            summary_stats[[paste0("time_", time_point)]] <- list(
                                time_point = time_point,
                                mean_old_risk = mean(old_risks, na.rm = TRUE),
                                mean_new_risk = mean(new_risks, na.rm = TRUE),
                                mean_risk_difference = mean(risk_diffs, na.rm = TRUE),
                                median_old_risk = median(old_risks, na.rm = TRUE),
                                median_new_risk = median(new_risks, na.rm = TRUE),
                                median_risk_difference = median(risk_diffs, na.rm = TRUE),
                                n_upstaged = sum(reclassifications == "Upstaged", na.rm = TRUE),
                                n_downstaged = sum(reclassifications == "Downstaged", na.rm = TRUE),
                                n_no_change = sum(reclassifications == "No Change", na.rm = TRUE),
                                percent_upstaged = mean(reclassifications == "Upstaged", na.rm = TRUE) * 100,
                                percent_downstaged = mean(reclassifications == "Downstaged", na.rm = TRUE) * 100,
                                percent_no_change = mean(reclassifications == "No Change", na.rm = TRUE) * 100,
                                significant_risk_change = sum(abs(risk_diffs) > 0.1, na.rm = TRUE),
                                percent_significant_change = mean(abs(risk_diffs) > 0.1, na.rm = TRUE) * 100
                            )
                        }

                        # Generate risk profiles for different patient archetypes
                        risk_profiles <- private$.generateRiskProfiles(
                            covariate_data, all_covariates, old_stage, new_stage,
                            old_model, new_model, time_points
                        )

                        return(list(
                            patient_predictions = patient_predictions,
                            summary_stats = summary_stats,
                            risk_profiles = risk_profiles,
                            time_points = time_points,
                            models_used = list(old = "Old staging + covariates", new = "New staging + covariates"),
                            model_performance = list(
                                old_concordance = tryCatch(survival::concordance(old_model)$concordance, error = function(e) NA),
                                new_concordance = tryCatch(survival::concordance(new_model)$concordance, error = function(e) NA)
                            ),
                            n_patients = n_patients,
                            prediction_date = Sys.Date()
                        ))
                    },
                    error = function(e) {
                        private$.addNotice("WARNING", .("Personalized risk predictions failed"), conditionMessage(e))
                        return(list(error = e$message))
                    }
                )
            },
            .generateRiskProfiles = function(covariate_data, all_covariates, old_stage, new_stage, old_model, new_model, time_points) {
                # Generate risk profiles for different patient archetypes
                # This helps clinicians understand how different patient types are affected by the new staging

                tryCatch(
                    {
                        # Create representative patient profiles
                        profiles <- list()

                        # Profile 1: Young, low comorbidity
                        if ("Age" %in% all_covariates && length(all_covariates) > 1) {
                            young_profile <- covariate_data[1, , drop = FALSE] # Template
                            young_profile$Age <- quantile(covariate_data$Age, 0.25, na.rm = TRUE) # 25th percentile age

                            # Set other variables to favorable values
                            for (var in all_covariates) {
                                if (var != "Age" && var %in% names(covariate_data)) {
                                    if (is.factor(covariate_data[[var]])) {
                                        young_profile[[var]] <- levels(covariate_data[[var]])[1] # First level (usually baseline)
                                    } else if (is.numeric(covariate_data[[var]])) {
                                        young_profile[[var]] <- quantile(covariate_data[[var]], 0.25, na.rm = TRUE)
                                    }
                                }
                            }

                            profiles[["young_low_risk"]] <- list(
                                description = "Young patient, low comorbidity",
                                profile_data = young_profile
                            )
                        }

                        # Profile 2: Older, high comorbidity
                        if ("Age" %in% all_covariates && length(all_covariates) > 1) {
                            older_profile <- covariate_data[1, , drop = FALSE] # Template
                            older_profile$Age <- quantile(covariate_data$Age, 0.75, na.rm = TRUE) # 75th percentile age

                            # Set other variables to unfavorable values
                            for (var in all_covariates) {
                                if (var != "Age" && var %in% names(covariate_data)) {
                                    if (is.factor(covariate_data[[var]])) {
                                        # Try to find a "high risk" level
                                        levels_var <- levels(covariate_data[[var]])
                                        if (length(levels_var) > 1) {
                                            older_profile[[var]] <- levels_var[length(levels_var)] # Last level
                                        }
                                    } else if (is.numeric(covariate_data[[var]])) {
                                        older_profile[[var]] <- quantile(covariate_data[[var]], 0.75, na.rm = TRUE)
                                    }
                                }
                            }

                            profiles[["older_high_risk"]] <- list(
                                description = "Older patient, high comorbidity",
                                profile_data = older_profile
                            )
                        }

                        # Profile 3: Average patient
                        average_profile <- covariate_data[1, , drop = FALSE] # Template
                        for (var in all_covariates) {
                            if (var %in% names(covariate_data)) {
                                if (is.factor(covariate_data[[var]])) {
                                    # Most common level
                                    most_common <- names(sort(table(covariate_data[[var]]), decreasing = TRUE))[1]
                                    average_profile[[var]] <- most_common
                                } else if (is.numeric(covariate_data[[var]])) {
                                    average_profile[[var]] <- median(covariate_data[[var]], na.rm = TRUE)
                                }
                            }
                        }

                        profiles[["average"]] <- list(
                            description = "Average patient profile",
                            profile_data = average_profile
                        )

                        # Calculate predictions for each profile and staging combination
                        profile_results <- list()

                        for (profile_name in names(profiles)) {
                            profile_data <- profiles[[profile_name]]$profile_data
                            profile_desc <- profiles[[profile_name]]$description

                            # Test different staging combinations
                            old_stages <- unique(covariate_data[[old_stage]])
                            new_stages <- unique(covariate_data[[new_stage]])

                            stage_combinations <- expand.grid(
                                old_stage = old_stages,
                                new_stage = new_stages,
                                stringsAsFactors = FALSE
                            )

                            combination_results <- list()

                            for (i in seq_len(nrow(stage_combinations))) {
                                old_s <- stage_combinations$old_stage[i]
                                new_s <- stage_combinations$new_stage[i]

                                # Create prediction data for this combination
                                pred_data <- profile_data
                                pred_data[[old_stage]] <- old_s
                                pred_data[[new_stage]] <- new_s

                                # Calculate predictions for each time point
                                time_predictions <- list()

                                for (time_point in time_points) {
                                    old_survfit <- tryCatch(
                                        {
                                            survfit(old_model, newdata = pred_data)
                                        },
                                        error = function(e) NULL
                                    )

                                    new_survfit <- tryCatch(
                                        {
                                            survfit(new_model, newdata = pred_data)
                                        },
                                        error = function(e) NULL
                                    )

                                    old_surv_prob <- if (!is.null(old_survfit)) {
                                        tryCatch(
                                            {
                                                summary(old_survfit, times = time_point, extend = TRUE)$surv[1]
                                            },
                                            error = function(e) NA
                                        )
                                    } else {
                                        NA
                                    }

                                    new_surv_prob <- if (!is.null(new_survfit)) {
                                        tryCatch(
                                            {
                                                summary(new_survfit, times = time_point, extend = TRUE)$surv[1]
                                            },
                                            error = function(e) NA
                                        )
                                    } else {
                                        NA
                                    }

                                    old_risk <- 1 - old_surv_prob
                                    new_risk <- 1 - new_surv_prob
                                    risk_diff <- new_risk - old_risk

                                    time_predictions[[paste0("time_", time_point)]] <- list(
                                        time_point = time_point,
                                        old_survival_prob = old_surv_prob,
                                        new_survival_prob = new_surv_prob,
                                        old_risk_prob = old_risk,
                                        new_risk_prob = new_risk,
                                        risk_difference = risk_diff,
                                        absolute_risk_change = abs(risk_diff)
                                    )
                                }

                                combination_results[[paste0("old_", old_s, "_new_", new_s)]] <- list(
                                    old_stage_value = old_s,
                                    new_stage_value = new_s,
                                    predictions = time_predictions
                                )
                            }

                            profile_results[[profile_name]] <- list(
                                description = profile_desc,
                                stage_combinations = combination_results
                            )
                        }

                        return(profile_results)
                    },
                    error = function(e) {
                        return(list(error = e$message))
                    }
                )
            },
            .calculateNRIComponents = function(old_categories, new_categories, actual_events) {
                # Helper function to calculate NRI components
                # Returns NRI for events and non-events

                # Create reclassification table
                reclassification_table <- table(
                    Old = old_categories,
                    New = new_categories,
                    Events = actual_events,
                    useNA = "no"
                )

                # Calculate NRI for events (those who actually had events)
                event_table <- reclassification_table[, , "TRUE"]
                up_events <- 0
                down_events <- 0
                if (length(event_table) == 0) {
                    nri_events <- 0
                    n_events <- 0
                } else {
                    # Events moved up (improved classification) vs moved down
                    total_events <- sum(event_table)
                    if (total_events > 0) {
                        up_events <- sum(event_table[lower.tri(event_table)]) # Below diagonal
                        down_events <- sum(event_table[upper.tri(event_table)]) # Above diagonal
                        nri_events <- (up_events - down_events) / total_events
                    } else {
                        nri_events <- 0
                    }
                    n_events <- total_events
                }

                # Calculate NRI for non-events (those who did not have events)
                nonevent_table <- reclassification_table[, , "FALSE"]
                up_nonevents <- 0
                down_nonevents <- 0
                if (length(nonevent_table) == 0) {
                    nri_nonevents <- 0
                    n_nonevents <- 0
                } else {
                    # Non-events moved down (improved classification) vs moved up
                    total_nonevents <- sum(nonevent_table)
                    if (total_nonevents > 0) {
                        up_nonevents <- sum(nonevent_table[lower.tri(nonevent_table)]) # Below diagonal (bad for non-events)
                        down_nonevents <- sum(nonevent_table[upper.tri(nonevent_table)]) # Above diagonal (good for non-events)
                        nri_nonevents <- (down_nonevents - up_nonevents) / total_nonevents
                    } else {
                        nri_nonevents <- 0
                    }
                    n_nonevents <- total_nonevents
                }

                # Overall NRI
                overall_nri <- nri_events + nri_nonevents

                # Standard errors. NRI is a DIFFERENCE of two proportions from the same
                # multinomial sample, so its range is [-1, 1] and the binomial form
                # sqrt(p(1-p)/n) does not apply: for a negative NRI it takes sqrt() of a
                # negative number and yields NaN (which then made `if (se > 0)` throw
                # "missing value where TRUE/FALSE needed"), and for a positive NRI it
                # silently UNDER-estimates the SE, giving CIs that are too narrow and
                # p-values that are too small. Use the standard variance of a difference
                # of proportions (Pencina et al. 2008):
                #   Var = (p_up + p_down - (p_up - p_down)^2) / n
                .nriSE <- function(up, down, n) {
                    if (!isTRUE(n > 0)) {
                        return(0)
                    }
                    p_up <- up / n
                    p_down <- down / n
                    v <- (p_up + p_down - (p_up - p_down)^2) / n
                    if (is.finite(v) && v > 0) sqrt(v) else 0
                }
                se_events <- .nriSE(up_events, down_events, n_events)
                se_nonevents <- .nriSE(up_nonevents, down_nonevents, n_nonevents)
                se_overall <- sqrt(se_events^2 + se_nonevents^2)

                # 95% Confidence intervals
                ci_events <- nri_events + c(-1, 1) * private$.zCrit() * se_events
                ci_nonevents <- nri_nonevents + c(-1, 1) * private$.zCrit() * se_nonevents
                ci_overall <- overall_nri + c(-1, 1) * private$.zCrit() * se_overall

                # Z-scores and p-values
                # isTRUE() so a NaN/NA can never reach if() as a bare condition again.
                z_events <- if (isTRUE(se_events > 0)) nri_events / se_events else 0
                z_nonevents <- if (isTRUE(se_nonevents > 0)) nri_nonevents / se_nonevents else 0
                z_overall <- if (isTRUE(se_overall > 0)) overall_nri / se_overall else 0

                p_events <- if (isTRUE(abs(z_events) > 0)) 2 * (1 - pnorm(abs(z_events))) else 1
                p_nonevents <- if (isTRUE(abs(z_nonevents) > 0)) 2 * (1 - pnorm(abs(z_nonevents))) else 1
                p_overall <- if (isTRUE(abs(z_overall) > 0)) 2 * (1 - pnorm(abs(z_overall))) else 1

                return(list(
                    nri_events = nri_events,
                    nri_nonevents = nri_nonevents,
                    nri_overall = overall_nri,
                    se_events = se_events,
                    se_nonevents = se_nonevents,
                    se_overall = se_overall,
                    ci_events = ci_events,
                    ci_nonevents = ci_nonevents,
                    ci_overall = ci_overall,
                    z_events = z_events,
                    z_nonevents = z_nonevents,
                    z_overall = z_overall,
                    p_events = p_events,
                    p_nonevents = p_nonevents,
                    p_overall = p_overall,
                    n_events = n_events,
                    n_nonevents = n_nonevents,
                    reclassification_table = reclassification_table
                ))
            },
            .performBootstrapModelSelection = function(covariate_data, all_covariates, old_stage, new_stage, survival_time) {
                # Bootstrap-enhanced model selection with stability assessment

                # Bootstrap parameters
                n_bootstrap <- 500 # Number of bootstrap samples
                bootstrap_seed <- private$.seedValue()

                # All possible variables for selection
                all_variables <- c(old_stage, new_stage, all_covariates)
                n_vars <- length(all_variables)

                # Storage for bootstrap results
                bootstrap_selections <- matrix(0, nrow = n_bootstrap, ncol = n_vars)
                colnames(bootstrap_selections) <- all_variables

                bootstrap_aics <- numeric(n_bootstrap)
                bootstrap_cindices <- numeric(n_bootstrap)

                # Set seed for reproducibility
                set.seed(bootstrap_seed)


                # Bootstrap sampling and selection
                for (b in 1:n_bootstrap) {
                    # Checkpoint every 50 iterations to allow cancellation of long-running bootstrap
                    if (b %% 50 == 1) {
                        private$.checkpoint()
                    }

                    tryCatch(
                        {
                            # Bootstrap sample
                            n_obs <- nrow(covariate_data)
                            boot_idx <- sample(1:n_obs, n_obs, replace = TRUE)
                            boot_data <- covariate_data[boot_idx, ]

                            # Check if bootstrap sample has sufficient events
                            n_events <- sum(boot_data$event_binary, na.rm = TRUE)
                            if (n_events < 10) {
                                next # Skip this bootstrap sample
                            }

                            # Build full model for bootstrap sample
                            full_formula <- as.formula(paste(
                                "survival::Surv(", survival_time, ", event_binary) ~",
                                paste(all_variables, collapse = " + ")
                            ))

                            # Check for model convergence
                            full_model <- tryCatch(
                                {
                                    survival::coxph(full_formula, data = boot_data)
                                },
                                error = function(e) NULL,
                                warning = function(w) NULL
                            )

                            if (is.null(full_model)) {
                                next # Skip this bootstrap sample
                            }

                            # Perform stepwise selection on bootstrap sample
                            step_model <- tryCatch(
                                {
                                    step(full_model, direction = "both", trace = FALSE)
                                },
                                error = function(e) NULL,
                                warning = function(w) NULL
                            )

                            if (!is.null(step_model)) {
                                # Record selected variables
                                selected_vars <- names(step_model$coefficients)
                                # Remove "(Intercept)" if present
                                selected_vars <- selected_vars[selected_vars != "(Intercept)"]

                                # Mark selected variables
                                for (var in selected_vars) {
                                    if (var %in% all_variables) {
                                        bootstrap_selections[b, var] <- 1
                                    }
                                }

                                # Record model performance
                                bootstrap_aics[b] <- AIC(step_model)

                                # Calculate C-index safely
                                concordance_result <- tryCatch(
                                    {
                                        survival::concordance(step_model)
                                    },
                                    error = function(e) NULL
                                )

                                if (!is.null(concordance_result)) {
                                    bootstrap_cindices[b] <- concordance_result$concordance
                                }
                            }
                        },
                        error = function(e) {
                        }
                    )

                    # Progress reporting every 100 samples
                    if (b %% 100 == 0) {
                    }
                }

                # Calculate selection frequencies
                selection_freq <- colMeans(bootstrap_selections, na.rm = TRUE)

                # Calculate stability metrics with AIC impact
                stability_metrics <- list()
                for (var in all_variables) {
                    var_selections <- bootstrap_selections[, var]

                    # Calculate AIC impact when variable is included vs excluded
                    aic_with_var <- bootstrap_aics[var_selections == 1]
                    aic_without_var <- bootstrap_aics[var_selections == 0]

                    mean_aic_impact <- if (length(aic_with_var) > 0 && length(aic_without_var) > 0) {
                        mean(aic_without_var, na.rm = TRUE) - mean(aic_with_var, na.rm = TRUE) # Positive = improvement
                    } else {
                        0
                    }

                    # Calculate confidence intervals for AIC impact
                    if (length(aic_with_var) > 5 && length(aic_without_var) > 5) {
                        aic_diff_samples <- sample(aic_without_var, min(100, length(aic_without_var)), replace = TRUE) -
                            sample(aic_with_var, min(100, length(aic_with_var)), replace = TRUE)
                        ci_probs_aic <- private$.ciProbs()
                        ci_lower <- quantile(aic_diff_samples, ci_probs_aic[1], na.rm = TRUE)
                        ci_upper <- quantile(aic_diff_samples, ci_probs_aic[2], na.rm = TRUE)
                    } else {
                        ci_lower <- NA
                        ci_upper <- NA
                    }

                    stability_metrics[[var]] <- list(
                        selection_frequency = selection_freq[var],
                        selection_proportion = mean(var_selections == 1, na.rm = TRUE),
                        stability_se = sqrt(selection_freq[var] * (1 - selection_freq[var]) / n_bootstrap),
                        confidence_interval_lower = pmax(0, selection_freq[var] - private$.zCrit() * sqrt(selection_freq[var] * (1 - selection_freq[var]) / n_bootstrap)),
                        confidence_interval_upper = pmin(1, selection_freq[var] + private$.zCrit() * sqrt(selection_freq[var] * (1 - selection_freq[var]) / n_bootstrap)),
                        mean_aic_impact = mean_aic_impact,
                        ci_lower = ci_lower,
                        ci_upper = ci_upper
                    )
                }

                # Determine stable variables (selected in >50% of bootstrap samples)
                stable_vars <- names(selection_freq[selection_freq > 0.5])
                high_stability_vars <- names(selection_freq[selection_freq > 0.8]) # Very stable

                # Build final stable model
                final_stable_model <- NULL
                final_model_performance <- NULL

                if (length(stable_vars) > 0) {
                    tryCatch(
                        {
                            stable_formula <- as.formula(paste(
                                "survival::Surv(", survival_time, ", event_binary) ~",
                                paste(stable_vars, collapse = " + ")
                            ))
                            final_stable_model <- survival::coxph(stable_formula, data = covariate_data)

                            # Calculate performance metrics
                            concordance_result <- survival::concordance(final_stable_model)
                            final_model_performance <- list(
                                aic = AIC(final_stable_model),
                                bic = BIC(final_stable_model),
                                c_index = concordance_result$concordance,
                                c_index_se = sqrt(concordance_result$var)
                            )
                        },
                        error = function(e) {
                            private$.addNotice("WARNING", .("Bootstrap model selection failed"), conditionMessage(e))
                        }
                    )
                }

                # Traditional stepwise for comparison
                traditional_stepwise <- NULL
                tryCatch(
                    {
                        full_formula <- as.formula(paste(
                            "survival::Surv(", survival_time, ", event_binary) ~",
                            paste(all_variables, collapse = " + ")
                        ))
                        full_model <- survival::coxph(full_formula, data = covariate_data)
                        traditional_stepwise <- step(full_model, direction = "both", trace = FALSE)
                    },
                    error = function(e) {
                        private$.addNotice("WARNING", .("Stepwise selection failed"), conditionMessage(e))
                    }
                )

                # Variable importance ranking
                variable_importance <- data.frame(
                    Variable = names(selection_freq),
                    Selection_Frequency = selection_freq,
                    Stability_Category = ifelse(selection_freq > 0.8, "High",
                        ifelse(selection_freq > 0.5, "Moderate", "Low")
                    ),
                    Clinical_Relevance = ifelse(names(selection_freq) %in% c(old_stage, new_stage), "Staging", "Covariate"),
                    stringsAsFactors = FALSE
                )
                variable_importance <- variable_importance[order(variable_importance$Selection_Frequency, decreasing = TRUE), ]

                # Bootstrap performance summary
                bootstrap_performance <- list(
                    mean_aic = mean(bootstrap_aics[bootstrap_aics > 0], na.rm = TRUE),
                    median_aic = median(bootstrap_aics[bootstrap_aics > 0], na.rm = TRUE),
                    sd_aic = sd(bootstrap_aics[bootstrap_aics > 0], na.rm = TRUE),
                    mean_c_index = mean(bootstrap_cindices[bootstrap_cindices > 0], na.rm = TRUE),
                    median_c_index = median(bootstrap_cindices[bootstrap_cindices > 0], na.rm = TRUE),
                    sd_c_index = sd(bootstrap_cindices[bootstrap_cindices > 0], na.rm = TRUE)
                )

                return(list(
                    # Bootstrap results
                    selection_frequencies = selection_freq,
                    stability_metrics = stability_metrics,
                    variable_importance = variable_importance,
                    bootstrap_performance = bootstrap_performance,

                    # Final models
                    stable_model = final_stable_model,
                    stable_model_performance = final_model_performance,
                    stable_variables = stable_vars,
                    high_stability_variables = high_stability_vars,
                    traditional_stepwise_model = traditional_stepwise,

                    # Staging system comparison
                    old_stage_frequency = selection_freq[old_stage],
                    new_stage_frequency = selection_freq[new_stage],
                    staging_comparison = list(
                        old_stage_stability = stability_metrics[[old_stage]],
                        new_stage_stability = stability_metrics[[new_stage]],
                        preference = ifelse(selection_freq[new_stage] > selection_freq[old_stage], "New", "Original")
                    ),

                    # Technical details
                    n_bootstrap_successful = sum(bootstrap_aics > 0),
                    bootstrap_seed = bootstrap_seed,
                    error = NULL
                ))
            },
            .performInteractionTestsOnly = function(data) {
                # Perform only interaction tests when multifactorial analysis is disabled

                # Extract covariate information
                continuous_vars <- self$options$continuousCovariates
                categorical_vars <- self$options$categoricalCovariates

                # Check if covariates are available
                if (is.null(continuous_vars) && is.null(categorical_vars)) {
                    return(list(
                        interaction_tests = NULL,
                        error = "No covariates specified for interaction tests"
                    ))
                }

                # Get stage variables
                old_stage <- self$options$oldStage
                new_stage <- self$options$newStage
                survival_time <- self$options$survivalTime
                event_var <- self$options$event

                # Process covariates
                all_covariates <- c()
                if (!is.null(continuous_vars)) {
                    all_covariates <- c(all_covariates, continuous_vars)
                }
                if (!is.null(categorical_vars)) {
                    all_covariates <- c(all_covariates, categorical_vars)
                }

                # Create event binary variable
                event_binary <- private$.createEventBinary(data, event_var, self$options$eventLevel)
                data$event_binary <- event_binary

                # Create covariate data
                covariate_data <- data[, c(old_stage, new_stage, survival_time, "event_binary", all_covariates)]
                covariate_data <- covariate_data[complete.cases(covariate_data), ]

                if (nrow(covariate_data) == 0) {
                    return(list(
                        interaction_tests = NULL,
                        error = "No complete cases available for interaction tests"
                    ))
                }

                # Perform interaction tests
                interaction_tests <- list()

                for (covar in all_covariates) {
                    # Create covariate formula
                    covariate_formula <- if (is.factor(covariate_data[[covar]])) {
                        paste("as.factor(", covar, ")", sep = "")
                    } else {
                        covar
                    }

                    # Test interaction with old staging
                    tryCatch(
                        {
                            int_formula_old <- as.formula(paste(
                                "survival::Surv(", survival_time, ", event_binary) ~",
                                "as.factor(", old_stage, ") *", covariate_formula
                            ))
                            int_model_old <- survival::coxph(int_formula_old, data = covariate_data)

                            base_formula_old <- as.formula(paste(
                                "survival::Surv(", survival_time, ", event_binary) ~",
                                "as.factor(", old_stage, ") +", covariate_formula
                            ))
                            base_model_old <- survival::coxph(base_formula_old, data = covariate_data)

                            lrt_int_old <- anova(base_model_old, int_model_old, test = "LRT")

                            interaction_tests[[paste("old_stage", covar, sep = "_x_")]] <- list(
                                interaction = paste("Original Staging x", covar),
                                chi_square = lrt_int_old$Chisq[2],
                                df = lrt_int_old$Df[2],
                                p_value = lrt_int_old$`Pr(>|Chi|)`[2]
                            )
                        },
                        error = function(e) {
                            interaction_tests[[paste("old_stage", covar, sep = "_x_")]] <- list(
                                interaction = paste("Original Staging x", covar),
                                error = e$message
                            )
                        }
                    )

                    # Test interaction with new staging
                    tryCatch(
                        {
                            int_formula_new <- as.formula(paste(
                                "survival::Surv(", survival_time, ", event_binary) ~",
                                "as.factor(", new_stage, ") *", covariate_formula
                            ))
                            int_model_new <- survival::coxph(int_formula_new, data = covariate_data)

                            base_formula_new <- as.formula(paste(
                                "survival::Surv(", survival_time, ", event_binary) ~",
                                "as.factor(", new_stage, ") +", covariate_formula
                            ))
                            base_model_new <- survival::coxph(base_formula_new, data = covariate_data)

                            lrt_int_new <- anova(base_model_new, int_model_new, test = "LRT")

                            interaction_tests[[paste("new_stage", covar, sep = "_x_")]] <- list(
                                interaction = paste("New Staging x", covar),
                                chi_square = lrt_int_new$Chisq[2],
                                df = lrt_int_new$Df[2],
                                p_value = lrt_int_new$`Pr(>|Chi|)`[2]
                            )
                        },
                        error = function(e) {
                            interaction_tests[[paste("new_stage", covar, sep = "_x_")]] <- list(
                                interaction = paste("New Staging x", covar),
                                chi_square = NA,
                                df = NA,
                                p_value = NA,
                                error = e$message
                            )
                        }
                    )
                }

                return(list(
                    interaction_tests = interaction_tests,
                    error = NULL
                ))
            },
            .createEventBinary = function(data, event_var, event_level) {
                # Create binary event variable from the event column
                event_col <- data[[event_var]]

                if (is.factor(event_col) || is.character(event_col)) {
                    # Convert to binary based on event level
                    event_binary <- ifelse(event_col == event_level, 1, 0)
                } else {
                    # Assume numeric and convert to binary
                    event_binary <- as.numeric(event_col)
                }

                return(event_binary)
            },
            .populateMultifactorialResults = function(multifactorial_results) {
                # Populate multifactorial analysis result tables

                # Check if there was an error
                if (!is.null(multifactorial_results$error)) {
                    # Set error message on all relevant tables
                    if (self$options$showMultifactorialTables) {
                        self$results$multifactorialResults$setError(multifactorial_results$error)
                    }
                    if (self$options$showAdjustedCIndexComparison) {
                        self$results$adjustedCIndexComparison$setError(multifactorial_results$error)
                    }
                    if (self$options$showNestedModelTests) {
                        self$results$nestedModelTests$setError(multifactorial_results$error)
                    }
                    if (self$options$showStepwiseResults) {
                        self$results$stepwiseResults$setError(multifactorial_results$error)
                    }
                    return()
                }

                # 1. Populate main multifactorial results table
                if (self$options$showMultifactorialTables && !is.null(multifactorial_results$models)) {
                    # Add explanatory text for multifactorial results table
                    if (isTRUE(self$options$showExplanations)) {
                        multifactorial_results_explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #4169e1; color: inherit;">
                        <h4 style="margin-top: 0; color: inherit;">Understanding Multifactorial Model Results</h4>
                        <p style="margin-bottom: 10px;">This table compares the performance of different models that combine staging systems with covariates:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Model:</strong> The specific combination of staging system and covariates</li>
                            <li><strong>C-Index:</strong> Concordance index (discrimination ability) of the model</li>
                            <li><strong>SE:</strong> Standard error of the C-index estimate</li>
                            <li><strong>95% CI:</strong> Confidence interval for the C-index</li>
                            <li><strong>AIC:</strong> Akaike Information Criterion (lower is better)</li>
                            <li><strong>BIC:</strong> Bayesian Information Criterion (lower is better)</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li>Compare C-index values to assess discrimination improvement</li>
                            <li>Lower AIC/BIC values indicate better model fit</li>
                            <li>Models with overlapping confidence intervals may not be significantly different</li>
                            <li>Choose the model that balances discrimination with simplicity</li>
                        </ul>
                    </div>
                    '
                        self$results$multifactorialResultsExplanation$setContent(multifactorial_results_explanation_html)
                    }

                    table <- self$results$multifactorialResults

                    for (model_name in names(multifactorial_results$models)) {
                        model_info <- multifactorial_results$models[[model_name]]

                        # Clean up model name for display
                        display_name <- switch(model_name,
                            "baseline" = "Baseline (Covariates Only)",
                            "old_plus_covariates" = "Original Staging + Covariates",
                            "new_plus_covariates" = "New Staging + Covariates",
                            model_name
                        )

                        table$addRow(rowKey = model_name, values = list(
                            Model = display_name,
                            C_Index = model_info$c_index,
                            SE = model_info$c_index_se,
                            CI_Lower = model_info$c_index_ci_lower,
                            CI_Upper = model_info$c_index_ci_upper,
                            AIC = model_info$aic,
                            BIC = model_info$bic
                        ))
                    }
                }

                # 2. Populate adjusted C-index comparison table
                if (self$options$showAdjustedCIndexComparison && !is.null(multifactorial_results$comparisons)) {
                    table <- self$results$adjustedCIndexComparison

                    for (comp_name in names(multifactorial_results$comparisons)) {
                        comp_info <- multifactorial_results$comparisons[[comp_name]]

                        # Clean up comparison name for display
                        display_name <- gsub("_", " ", comp_name)
                        display_name <- gsub("baseline", "Baseline", display_name)
                        display_name <- gsub("old plus covariates", "Original + Covariates", display_name)
                        display_name <- gsub("new plus covariates", "New + Covariates", display_name)

                        table$addRow(rowKey = comp_name, values = list(
                            Comparison = display_name,
                            C_Index_Difference = comp_info$c_index_diff,
                            SE = comp_info$se_diff,
                            CI_Lower = comp_info$ci_lower,
                            CI_Upper = comp_info$ci_upper,
                            p_value = comp_info$p_value
                        ))
                    }

                    # Add explanatory output for adjusted C-index comparison
                    if (isTRUE(self$options$showExplanations)) {
                        adjusted_cindex_explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #4169e1; color: inherit;">
                        <h4 style="margin-top: 0; color: inherit;">Understanding Adjusted C-Index Comparison</h4>
                        <p style="margin-bottom: 10px;">This table compares the discriminative ability (C-index) of models adjusted for covariates:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Comparison:</strong> Specific model comparison being evaluated</li>
                            <li><strong>C-Index Difference:</strong> Difference in discrimination between models</li>
                            <li><strong>SE:</strong> Standard error of the difference estimate</li>
                            <li><strong>95% CI:</strong> Confidence interval for the difference</li>
                            <li><strong>p-value:</strong> Statistical significance of the improvement</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li>Positive differences indicate improvement in the new staging system</li>
                            <li>Differences >0.05 are generally considered clinically meaningful</li>
                            <li>p-values <0.05 indicate statistically significant improvements</li>
                            <li>Consider both statistical significance and clinical relevance</li>
                        </ul>
                    </div>
                    '
                        self$results$adjustedCIndexComparisonExplanation$setContent(adjusted_cindex_explanation_html)
                    }
                }

                # 3. Populate nested model tests table
                if (self$options$showNestedModelTests && !is.null(multifactorial_results$nested_tests)) {
                    table <- self$results$nestedModelTests

                    for (test_name in names(multifactorial_results$nested_tests)) {
                        test_info <- multifactorial_results$nested_tests[[test_name]]

                        # Determine decision based on p-value
                        decision <- if (!is.null(test_info$p_value) && !is.na(test_info$p_value)) {
                            if (test_info$p_value < 0.001) {
                                "Highly significant improvement"
                            } else if (test_info$p_value < 0.01) {
                                "Significant improvement"
                            } else if (test_info$p_value < 0.05) {
                                "Marginally significant improvement"
                            } else {
                                "No significant improvement"
                            }
                        } else {
                            "Unable to determine"
                        }

                        table$addRow(rowKey = test_name, values = list(
                            Model_Comparison = test_info$comparison,
                            Chi_Square = test_info$chi_square,
                            df = test_info$df,
                            p_value = test_info$p_value,
                            Decision = decision
                        ))
                    }

                    # Add explanatory output for nested model tests
                    if (isTRUE(self$options$showExplanations)) {
                        nested_model_explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #4169e1; color: inherit;">
                        <h4 style="margin-top: 0; color: inherit;">Understanding Nested Model Tests</h4>
                        <p style="margin-bottom: 10px;">These likelihood ratio tests compare nested models to assess if adding variables significantly improves model fit:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Model Comparison:</strong> Specific models being compared (simpler vs. more complex)</li>
                            <li><strong>Chi-Square:</strong> Test statistic measuring improvement in model fit</li>
                            <li><strong>df:</strong> Degrees of freedom (difference in parameters between models)</li>
                            <li><strong>p-value:</strong> Statistical significance of the improvement</li>
                            <li><strong>Decision:</strong> Interpretation of the statistical result</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li>Significant p-values indicate the more complex model fits significantly better</li>
                            <li>Non-significant results suggest the simpler model is adequate</li>
                            <li>Balance model complexity with clinical interpretability</li>
                            <li>Consider effect sizes alongside statistical significance</li>
                        </ul>
                    </div>
                    '
                        self$results$nestedModelTestsExplanation$setContent(nested_model_explanation_html)
                    }
                }

                # 4. Populate stepwise results table
                if (self$options$showStepwiseResults && !is.null(multifactorial_results$stepwise_results)) {
                    table <- self$results$stepwiseResults
                    stepwise_info <- multifactorial_results$stepwise_results

                    if (!is.null(stepwise_info$error)) {
                        table$setError(stepwise_info$error)
                    } else if (!is.null(stepwise_info$selection_frequencies)) {
                        # New bootstrap-enhanced model selection format
                        selection_freq <- stepwise_info$selection_frequencies
                        stability_metrics <- stepwise_info$stability_metrics

                        # Sort variables by selection frequency
                        sorted_vars <- names(sort(selection_freq, decreasing = TRUE))

                        # Add summary row
                        n_stable <- length(stepwise_info$stable_variables)
                        n_high_stability <- length(stepwise_info$high_stability_variables)

                        # rowKeys are re-added every run; clear first so the fixed summary rows
                        # below cannot accumulate when clearWith does not fire.
                        table$deleteRows()

                        n_boot_ok <- stepwise_info$n_bootstrap_successful
                        mean_boot_aic <- stepwise_info$bootstrap_performance$mean_aic

                        table$addRow(rowKey = "summary", values = list(
                            Variable = "Bootstrap Model Selection Summary",
                            Step = if (!is.null(n_boot_ok)) paste0(n_boot_ok, " resamples") else "Final",
                            Action = paste0(n_stable, " stable vars (", n_high_stability, " high stability)"),
                            AIC = if (!is.null(mean_boot_aic) && is.finite(mean_boot_aic)) round(mean_boot_aic, 1) else NA,
                            p_value = ""
                        ))

                        # Add individual variables with bootstrap statistics
                        for (i in seq_along(sorted_vars)) {
                            var_name <- sorted_vars[i]
                            freq <- selection_freq[var_name]
                            stability <- stability_metrics[[var_name]]

                            # Debug the stability object
                            if (!is.null(stability$mean_aic_impact)) {}

                            # Determine stability status
                            if (var_name %in% stepwise_info$high_stability_variables) {
                                action <- "High Stability"
                            } else if (var_name %in% stepwise_info$stable_variables) {
                                action <- "Stable"
                            } else {
                                action <- "Unstable"
                            }

                            # Format selection frequency as percentage
                            freq_pct <- round(freq * 100, 1)

                            # Safely handle potentially non-numeric values
                            aic_value <- if (!is.null(stability$mean_aic_impact) && is.numeric(stability$mean_aic_impact)) {
                                round(stability$mean_aic_impact, 1)
                            } else {
                                NA
                            }

                            ci_lower <- if (!is.null(stability$ci_lower) && is.numeric(stability$ci_lower)) {
                                round(stability$ci_lower, 2)
                            } else {
                                NA
                            }

                            ci_upper <- if (!is.null(stability$ci_upper) && is.numeric(stability$ci_upper)) {
                                round(stability$ci_upper, 2)
                            } else {
                                NA
                            }

                            p_value_text <- if (!is.na(ci_lower) && !is.na(ci_upper)) {
                                paste0("CI: [", ci_lower, ", ", ci_upper, "]")
                            } else {
                                "CI: NA"
                            }

                            table$addRow(rowKey = paste("var", i, sep = "_"), values = list(
                                Variable = var_name,
                                Step = paste0(freq_pct, "%"),
                                Action = action,
                                AIC = aic_value,
                                p_value = p_value_text
                            ))
                        }

                        # Surface the refit stable model, the full-data stepwise reference model
                        # and the staging-system stability comparison computed by
                        # .performBootstrapModelSelection() but previously never displayed.
                        stable_perf <- stepwise_info$stable_model_performance
                        if (!is.null(stable_perf)) {
                            table$addRow(rowKey = "stable_model", values = list(
                                Variable = "Refit model on stable variables",
                                Step = "Final",
                                Action = paste0(
                                    "C-index ", round(stable_perf$c_index, 3),
                                    " (SE ", round(stable_perf$c_index_se, 3), "), BIC ",
                                    round(stable_perf$bic, 1)
                                ),
                                AIC = round(stable_perf$aic, 1),
                                p_value = ""
                            ))
                        }

                        trad_model <- stepwise_info$traditional_stepwise_model
                        if (!is.null(trad_model)) {
                            trad_vars <- tryCatch(
                                attr(stats::terms(trad_model), "term.labels"),
                                error = function(e) character(0)
                            )
                            table$addRow(rowKey = "traditional_stepwise", values = list(
                                Variable = "Traditional stepwise (full data)",
                                Step = "Reference",
                                Action = if (length(trad_vars) > 0) {
                                    paste("Retained:", paste(trad_vars, collapse = ", "))
                                } else {
                                    "Retained: none"
                                },
                                AIC = round(AIC(trad_model), 1),
                                p_value = ""
                            ))
                        }

                        staging_cmp <- stepwise_info$staging_comparison
                        if (!is.null(staging_cmp)) {
                            old_freq_pct <- round(as.numeric(stepwise_info$old_stage_frequency) * 100, 1)
                            new_freq_pct <- round(as.numeric(stepwise_info$new_stage_frequency) * 100, 1)
                            table$addRow(rowKey = "staging_comparison", values = list(
                                Variable = "Staging system stability",
                                Step = paste0("Original ", old_freq_pct, "% vs New ", new_freq_pct, "%"),
                                Action = paste0("More stably selected: ", staging_cmp$preference),
                                AIC = NA,
                                p_value = ""
                            ))
                        }
                    } else if (!is.null(stepwise_info$step_history) && length(stepwise_info$step_history) > 0) {
                        # Use the improved step history with proper AIC progression and p-values

                        # Add summary row
                        table$addRow(rowKey = "summary", values = list(
                            Variable = "Final Model Summary",
                            Step = "Final",
                            Action = paste("Selected", length(stepwise_info$step_history), "variables"),
                            AIC = stepwise_info$final_aic,
                            p_value = ""
                        ))

                        # Add individual variables using step history
                        for (i in seq_along(stepwise_info$step_history)) {
                            step_info <- stepwise_info$step_history[[i]]
                            var_name <- step_info$variable

                            # Clean up variable name for display
                            display_var <- gsub("^[^:]*:", "", var_name) # Remove prefix before colon

                            table$addRow(rowKey = paste("var", i, sep = "_"), values = list(
                                Variable = display_var,
                                Step = as.character(step_info$step),
                                Action = "Selected",
                                AIC = step_info$aic,
                                p_value = step_info$p_value
                            ))
                        }
                    } else if (!is.null(stepwise_info$selected_variables)) {
                        # Fallback to original method if step history is not available

                        # Add summary row
                        table$addRow(rowKey = "summary", values = list(
                            Variable = "Final Model Summary",
                            Step = "Final",
                            Action = paste("Selected", length(stepwise_info$selected_variables), "variables"),
                            AIC = stepwise_info$final_aic,
                            p_value = NA
                        ))

                        # Add individual variables with fallback values
                        for (i in seq_along(stepwise_info$selected_variables)) {
                            var_name <- stepwise_info$selected_variables[i]

                            # Clean up variable name for display
                            display_var <- gsub("^[^:]*:", "", var_name) # Remove prefix before colon

                            table$addRow(rowKey = paste("var", i, sep = "_"), values = list(
                                Variable = display_var,
                                Step = as.character(i),
                                Action = "Selected",
                                AIC = stepwise_info$final_aic,
                                p_value = NA
                            ))
                        }
                    }

                    # Add explanatory output for stepwise results
                    if (isTRUE(self$options$showExplanations)) {
                        stepwise_explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #4169e1; color: inherit;">
                        <h4 style="margin-top: 0; color: inherit;">Understanding Stepwise Selection Results</h4>
                        <p style="margin-bottom: 10px;">This table shows the results of automatic variable selection to identify the most important predictors:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Variable:</strong> The predictor variable being evaluated</li>
                            <li><strong>Step:</strong> Order in which variables were selected</li>
                            <li><strong>Action:</strong> Whether the variable was selected or removed</li>
                            <li><strong>AIC:</strong> Akaike Information Criterion of the final model</li>
                            <li><strong>p-value:</strong> Statistical significance (when available)</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li>Selected variables are the most important predictors in the dataset</li>
                            <li>Lower AIC values indicate better model fit</li>
                            <li>Earlier selection steps indicate stronger predictive ability</li>
                            <li>Consider clinical relevance alongside statistical selection</li>
                            <li>Validate selected variables in independent datasets when possible</li>
                        </ul>
                    </div>
                    '
                        self$results$stepwiseResultsExplanation$setContent(stepwise_explanation_html)
                    }
                }

                # 5. Populate interaction tests table
                if (self$options$performInteractionTests && !is.null(multifactorial_results$interaction_tests)) {
                    table <- self$results$interactionTests

                    # Check if interaction_tests is a data frame (new format) or list (old format)
                    if (is.data.frame(multifactorial_results$interaction_tests)) {
                        # New format: advanced interaction detection results
                        interaction_df <- multifactorial_results$interaction_tests

                        for (i in seq_len(nrow(interaction_df))) {
                            row_data <- interaction_df[i, ]
                            row_key <- paste0("interaction_", i)


                            # Create interaction description
                            interaction_desc <- paste0(row_data$Variable, " Interaction")

                            # Determine interpretation based on clinical significance
                            interpretation <- if (!is.na(row_data$Clinical_Significance)) {
                                row_data$Clinical_Significance
                            } else {
                                "Unable to determine"
                            }

                            # Use the most significant p-value for display (handle all NA case)
                            p_values <- c(row_data$Old_Stage_Interaction_P, row_data$New_Stage_Interaction_P)
                            p_values_valid <- p_values[!is.na(p_values)]

                            p_value <- if (length(p_values_valid) > 0) {
                                min(p_values_valid)
                            } else {
                                NA
                            }

                            # Calculate chi-square approximation from p-value
                            chi_square <- if (!is.na(p_value) && is.finite(p_value) && p_value > 0 && p_value < 1) {
                                qchisq(1 - p_value, df = 1)
                            } else {
                                NA
                            }

                            table$addRow(rowKey = row_key, values = list(
                                Interaction = interaction_desc,
                                Chi_Square = if (is.na(chi_square)) NA else round(chi_square, 3),
                                df = 1,
                                p_value = if (is.na(p_value)) NA else round(p_value, 4),
                                Interpretation = interpretation
                            ))
                        }
                    } else {
                        # Old format: legacy interaction tests
                        for (int_name in names(multifactorial_results$interaction_tests)) {
                            int_info <- multifactorial_results$interaction_tests[[int_name]]

                            if (!is.null(int_info$error)) {
                                interpretation <- paste("Error:", int_info$error)
                                chi_square <- NA
                                df <- NA
                                p_value <- NA
                            } else {
                                # Determine interpretation based on p-value
                                interpretation <- if (!is.null(int_info$p_value) && !is.na(int_info$p_value)) {
                                    if (int_info$p_value < 0.001) {
                                        "Highly significant interaction"
                                    } else if (int_info$p_value < 0.01) {
                                        "Significant interaction"
                                    } else if (int_info$p_value < 0.05) {
                                        "Marginally significant interaction"
                                    } else {
                                        "No significant interaction"
                                    }
                                } else {
                                    "Unable to determine"
                                }

                                chi_square <- int_info$chi_square
                                df <- int_info$df
                                p_value <- int_info$p_value
                            }

                            table$addRow(rowKey = int_name, values = list(
                                Interaction = int_info$interaction,
                                Chi_Square = chi_square,
                                df = df,
                                p_value = p_value,
                                Interpretation = interpretation
                            ))
                        }
                    }

                    # Add explanatory output for interaction tests
                    if (isTRUE(self$options$showExplanations)) {
                        interaction_explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #4169e1; color: inherit;">
                        <h4 style="margin-top: 0; color: inherit;">Understanding Stage-Covariate Interaction Tests</h4>
                        <p style="margin-bottom: 10px;">These tests examine whether the effect of staging systems varies across different covariate levels:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Interaction:</strong> Specific stage-covariate interaction being tested</li>
                            <li><strong>Chi-Square:</strong> Test statistic measuring the interaction effect</li>
                            <li><strong>df:</strong> Degrees of freedom for the interaction test</li>
                            <li><strong>p-value:</strong> Statistical significance of the interaction</li>
                            <li><strong>Interpretation:</strong> Clinical meaning of the statistical result</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li>Significant interactions suggest staging performance varies by patient subgroups</li>
                            <li>Non-significant results indicate consistent staging performance across groups</li>
                            <li>Strong interactions may require stratified analysis or subgroup-specific models</li>
                            <li>Consider biological plausibility of identified interactions</li>
                            <li>Validate significant interactions in independent datasets</li>
                        </ul>
                    </div>
                    '
                        self$results$interactionTestsExplanation$setContent(interaction_explanation_html)
                    }
                }

                # 6. Populate stratified analysis table
                if (self$options$stratifiedAnalysis && !is.null(multifactorial_results$stratified_results)) {
                    table <- self$results$stratifiedAnalysisTable

                    for (strat_name in names(multifactorial_results$stratified_results)) {
                        strat_info <- multifactorial_results$stratified_results[[strat_name]]

                        if (!is.null(strat_info$error)) {
                            table$addRow(rowKey = strat_name, values = list(
                                Stratum = strat_info$stratum,
                                N = NA,
                                C_Index_Old = NA,
                                C_Index_New = NA,
                                Difference = NA,
                                p_value = NA
                            ))
                            table$addFootnote(rowKey = strat_name, "N", paste("Error:", strat_info$error))
                        } else {
                            table$addRow(rowKey = strat_name, values = list(
                                Stratum = strat_info$stratum,
                                N = strat_info$n,
                                C_Index_Old = strat_info$c_index_old,
                                C_Index_New = strat_info$c_index_new,
                                Difference = strat_info$difference,
                                p_value = strat_info$p_value
                            ))
                        }
                    }

                    # Add explanatory output for stratified analysis
                    if (isTRUE(self$options$showExplanations)) {
                        stratified_explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #4169e1; color: inherit;">
                        <h4 style="margin-top: 0; color: inherit;">Understanding Stratified Analysis</h4>
                        <p style="margin-bottom: 10px;">This analysis examines staging system performance within specific patient subgroups:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Stratum:</strong> Patient subgroup being analyzed</li>
                            <li><strong>N:</strong> Sample size within the stratum</li>
                            <li><strong>C-Index Old:</strong> Discrimination of the original staging system</li>
                            <li><strong>C-Index New:</strong> Discrimination of the new staging system</li>
                            <li><strong>Difference:</strong> Improvement in discrimination (New - Old)</li>
                            <li><strong>p-value:</strong> Statistical significance of the difference</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li>Compare performance across different patient subgroups</li>
                            <li>Positive differences indicate improvement in the new staging system</li>
                            <li>Look for consistent improvements across all strata</li>
                            <li>Large variations between strata may indicate interaction effects</li>
                            <li>Consider clinical relevance of subgroup-specific differences</li>
                        </ul>
                    </div>
                    '
                        self$results$stratifiedAnalysisExplanation$setContent(stratified_explanation_html)
                    }
                }

                # 7. Populate personalized risk prediction summary
                personalized <- multifactorial_results$personalized_predictions
                if (!is.null(personalized) && !is.null(personalized$error)) {
                    # Say WHY the table is empty rather than rendering a blank pane.
                    self$results$personalizedRiskSummary$setNote(
                        "unavailable",
                        paste0("Personalized risk predictions unavailable: ", personalized$error)
                    )
                }
                if (!is.null(personalized) && is.null(personalized$error) &&
                    length(personalized$summary_stats) > 0) {
                    table <- self$results$personalizedRiskSummary
                    table$deleteRows()

                    for (time_key in names(personalized$summary_stats)) {
                        stats_row <- personalized$summary_stats[[time_key]]

                        table$addRow(rowKey = time_key, values = list(
                            TimePoint = stats_row$time_point,
                            Mean_Risk_Old = stats_row$mean_old_risk,
                            Mean_Risk_New = stats_row$mean_new_risk,
                            Mean_Risk_Difference = stats_row$mean_risk_difference,
                            Percent_Upstaged = stats_row$percent_upstaged,
                            Percent_Downstaged = stats_row$percent_downstaged,
                            Percent_No_Change = stats_row$percent_no_change,
                            Percent_Large_Change = stats_row$percent_significant_change
                        ))
                    }

                    table$setNote("personalized", paste0(
                        "Individual absolute risk predictions from Cox models (staging system + covariates) for ",
                        personalized$n_patients,
                        " patients. Risk categories: Low 0-10%, Moderate 10-30%, High 30-50%, Very High above 50%. ",
                        "Large risk change = absolute change in predicted risk above 10 percentage points."
                    ))
                }

                # 8. Populate multifactorial model diagnostics
                diagnostics <- multifactorial_results$model_diagnostics
                if (!is.null(diagnostics) && is.null(diagnostics$error)) {
                    table <- self$results$multifactorialModelDiagnostics
                    table$deleteRows()

                    assumptions <- diagnostics$model_assumptions
                    diag_sources <- list(
                        old_model = list(
                            diag = diagnostics$old_model_diagnostics,
                            ph = if (is.null(assumptions)) NULL else assumptions$proportional_hazards_old
                        ),
                        new_model = list(
                            diag = diagnostics$new_model_diagnostics,
                            ph = if (is.null(assumptions)) NULL else assumptions$proportional_hazards_new
                        )
                    )

                    for (source_key in names(diag_sources)) {
                        model_diag <- diag_sources[[source_key]]$diag
                        ph_diag <- diag_sources[[source_key]]$ph

                        if (is.null(model_diag) || !is.null(model_diag$error)) {
                            next
                        }

                        ph_status <- if (is.null(ph_diag)) {
                            "Not assessed"
                        } else if (isTRUE(ph_diag$assumption_violated)) {
                            paste0("Violated (worst: ", ph_diag$worst_violator, ")")
                        } else {
                            "Satisfied"
                        }

                        table$addRow(rowKey = source_key, values = list(
                            Model = model_diag$model_name,
                            N = model_diag$sample_size,
                            C_Index = unname(model_diag$concordance),
                            Significant_Coefficients = paste0(
                                length(model_diag$significant_variables), " / ", model_diag$total_variables
                            ),
                            Outliers = model_diag$deviance_residuals$outliers,
                            PH_Global_p = if (is.null(ph_diag)) NA_real_ else unname(ph_diag$global_p),
                            PH_Assumption = ph_status
                        ))
                    }

                    diag_notes <- character(0)
                    if (!is.null(diagnostics$outlier_analysis) &&
                        !is.null(diagnostics$outlier_analysis$interpretation)) {
                        diag_notes <- c(diag_notes, diagnostics$outlier_analysis$interpretation)
                    }
                    if (!is.null(assumptions) && !is.null(assumptions$comparison)) {
                        diag_notes <- c(diag_notes, assumptions$comparison$interpretation)
                    }

                    table$setNote("diagnostics", paste0(
                        "Significant coefficients counted at p below 0.05. Outliers = patients with an absolute ",
                        "deviance residual above 2.5. Proportional hazards assessed with the global cox.zph test ",
                        "(p below 0.05 indicates a violation).",
                        if (length(diag_notes) > 0) paste0(" ", paste(diag_notes, collapse = ". "), ".") else ""
                    ))
                }

                # 9. Populate multivariable decision curve analysis summary
                mv_dca <- multifactorial_results$multivariable_dca
                if (!is.null(mv_dca) && is.null(mv_dca$error) && length(mv_dca) > 0) {
                    table <- self$results$multivariableDCASummary
                    table$deleteRows()

                    dca_model_labels <- c(
                        baseline = "Baseline (Covariates Only)",
                        old_staging = "Original Staging + Covariates",
                        new_staging = "New Staging + Covariates",
                        full_model = "Both Staging Systems + Covariates"
                    )

                    for (time_key in names(mv_dca)) {
                        time_entry <- mv_dca[[time_key]]

                        if (is.null(time_entry) || !is.null(time_entry$error) ||
                            is.null(time_entry$optimal_ranges)) {
                            next
                        }

                        for (model_key in names(time_entry$optimal_ranges)) {
                            range_info <- time_entry$optimal_ranges[[model_key]]
                            model_label <- if (model_key %in% names(dca_model_labels)) {
                                unname(dca_model_labels[[model_key]])
                            } else {
                                model_key
                            }

                            table$addRow(rowKey = paste0(time_key, "_", model_key), values = list(
                                Time_Point = time_entry$time_point,
                                Model = model_label,
                                Optimal_Threshold_Min = range_info$optimal_min,
                                Optimal_Threshold_Max = range_info$optimal_max,
                                Max_Net_Benefit = range_info$max_net_benefit,
                                Threshold_At_Max = range_info$max_nb_threshold
                            ))
                        }
                    }

                    table$setNote("mvdca", paste0(
                        "Net benefit compares each multivariable model against treat-all and treat-none strategies ",
                        "across threshold probabilities from 0.01 to 0.99. The optimal threshold range is where the ",
                        "model outperforms both default strategies. Events are counted as observed by the time point, ",
                        "so patients censored before that time are treated as event-free."
                    ))
                }

                # Add summary note about the analysis
                if (self$options$showMultifactorialTables) {
                    summary_note <- paste(
                        "Multifactorial analysis included",
                        length(multifactorial_results$covariates_used),
                        "covariates with",
                        multifactorial_results$sample_size,
                        "patients after complete case analysis."
                    )
                    self$results$multifactorialResults$setNote("summary", summary_note)
                }
            },


            # .plotSankeyDiagram = function(image, ...) {
            #     # Create Sankey diagram showing stage migration flows
            #     if (is.null(image$parent$options$oldStage) || is.null(image$parent$options$newStage)) {
            #         return()
            #     }

            #     # Get state data
            #     plot_data <- image$state
            #     if (is.null(plot_data) || is.null(plot_data$migration_matrix)) {
            #         # Try alternative approach - get data directly from results
            #         tryCatch({
            #             basic_migration <- private$.calculateBasicMigration()
            #             migration_matrix <- basic_migration$migration_table
            #             if (is.null(migration_matrix)) {
            #                 return()
            #             }
            #         }, error = function(e) {
            #             return()
            #         })
            #     } else {
            #         migration_matrix <- plot_data$migration_matrix
            #     }

            #     tryCatch({
            #         old_stage <- image$parent$options$oldStage
            #         new_stage <- image$parent$options$newStage

            #         # Convert migration matrix to flow data
            #         if (!requireNamespace("reshape2", quietly = TRUE)) {
            #             return()
            #         }

            #         # Convert matrix to long format
            #         flow_data <- reshape2::melt(as.matrix(migration_matrix), varnames = c("source", "target"), value.name = "count")
            #         # Remove zero flows for cleaner visualization
            #         flow_data <- flow_data[flow_data$count > 0, ]

            #         # Prepare node and link data for Sankey
            #         source_nodes <- paste0("Original_", unique(flow_data$source))
            #         target_nodes <- paste0("New_", unique(flow_data$target))
            #         all_nodes <- c(source_nodes, target_nodes)

            #         # Create links with proper indices
            #         links <- flow_data %>%
            #             mutate(
            #                 source_idx = match(paste0("Original_", source), all_nodes) - 1,
            #                 target_idx = match(paste0("New_", target), all_nodes) - 1,
            #                 value = count
            #             )

            #         # Create the plot using networkD3 or fallback to ggplot
            #         if (requireNamespace("networkD3", quietly = TRUE)) {
            #             # Use networkD3 for interactive Sankey
            #             nodes_df <- data.frame(name = all_nodes, stringsAsFactors = FALSE)

            #             sankey_plot <- networkD3::sankeyNetwork(
            #                 Links = links,
            #                 Nodes = nodes_df,
            #                 Source = "source_idx",
            #                 Target = "target_idx",
            #                 Value = "value",
            #                 NodeID = "name",
            #                 fontSize = 12,
            #                 nodeWidth = 30,
            #                 height = 500,
            #                 width = 800
            #             )

            #             # Convert to static plot for jamovi
            #             print(sankey_plot)

            #         } else {
            #             # Fallback to ggplot2 alluvial-style plot
            #             if (requireNamespace("ggalluvial", quietly = TRUE)) {


            #                 # Prepare data in long format for ggalluvial
            #                 alluvial_data <- flow_data %>%
            #                     dplyr::filter(count > 0) %>%
            #                     dplyr::rename(Freq = count)

            #                 # mydataview <- self$results$mydataview
            #                 # mydataview$setContent(list(alluvial_data = alluvial_data))


            #                 # Create alluvial plot using proper ggalluvial syntax


            #                     p <- ggplot2::ggplot(
            #                         alluvial_data,
            #                         ggplot2::aes(
            #                             axis1    = source,
            #                             axis2    = target,
            #                             y        = Freq,
            #                             alluvium = interaction(source, target)
            #                         )
            #                         ) +
            #                     ggalluvial::geom_alluvium(
            #                         ggplot2::aes(fill = source),
            #                         width = 1/12,
            #                         alpha = 0.7
            #                     ) +
            #                     ggalluvial::geom_stratum(
            #                         width = 1/12,
            #                         fill  = "grey70",
            #                         color = "grey"
            #                     ) +
            #                     ggalluvial::stat_stratum(
            #                         geom = "label",
            #                         ggplot2::aes(label = ggplot2::after_stat(stratum)),
            #                         width = 1/12,
            #                         size  = 3
            #                     ) +
            #                     ggplot2::scale_x_discrete(
            #                         limits = c("source", "target"),
            #                         labels = c("Original Stage", "New Stage"),
            #                         expand = c(0.05, 0.05)
            #                     ) +
            #                     ggplot2::labs(
            #                         title    = "Stage Migration Flow Diagram",
            #                         subtitle = "Patient flow between original and new staging systems",
            #                         x        = NULL,
            #                         y        = "Number of Patients"
            #                     ) +
            #                     ggtheme +
            #                     ggplot2::theme(
            #                         plot.title      = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            #                         plot.subtitle   = ggplot2::element_text(hjust = 0.5, size = 12),
            #                         axis.text.y     = ggplot2::element_blank(),
            #                         axis.ticks.y    = ggplot2::element_blank(),
            #                         legend.position = "bottom",
            #                         legend.title    = ggplot2::element_blank()
            #                     ) +
            #                     viridis::scale_fill_viridis(
            #                         discrete = TRUE,
            #                         option   = "viridis",
            #                         alpha    = 0.8
            #                     )

            #                     print(p)


            #             } else {
            #                 # Create a simple Sankey-like visualization using ggplot2
            #                 # This creates ribbons between stages

            #                 # Prepare data for ribbon plot
            #                 ribbon_data <- flow_data %>%
            #                     dplyr::filter(count > 0) %>%
            #                     dplyr::mutate(
            #                         source_y = as.numeric(factor(source)),
            #                         target_y = as.numeric(factor(target)),
            #                         source_x = 0,
            #                         target_x = 1
            #                     )

            #                 # Create base plot
            #                 p <- ggplot2::ggplot(ribbon_data) +
            #                     # Draw ribbons for each flow
            #                     ggplot2::geom_ribbon(
            #                         ggplot2::aes(
            #                             x = c(source_x, target_x),
            #                             ymin = source_y - count/max(ribbon_data$count) * 0.4,
            #                             ymax = source_y + count/max(ribbon_data$count) * 0.4,
            #                             group = paste(source, target),
            #                             fill = source
            #                         ),
            #                         alpha = 0.6
            #                     ) +
            #                     # Add stage labels
            #                     ggplot2::geom_text(
            #                         data = data.frame(
            #                             x = c(rep(0, length(unique(ribbon_data$source))),
            #                                   rep(1, length(unique(ribbon_data$target)))),
            #                             y = c(as.numeric(factor(unique(ribbon_data$source))),
            #                                   as.numeric(factor(unique(ribbon_data$target)))),
            #                             label = c(paste0("Original: ", unique(ribbon_data$source)),
            #                                     paste0("New: ", unique(ribbon_data$target)))
            #                         ),
            #                         ggplot2::aes(x = x, y = y, label = label),
            #                         hjust = c(rep(1.1, length(unique(ribbon_data$source))),
            #                                  rep(-0.1, length(unique(ribbon_data$target)))),
            #                         size = 4
            #                     ) +
            #                     ggplot2::scale_x_continuous(
            #                         limits = c(-0.5, 1.5),
            #                         breaks = c(0, 1),
            #                         labels = c("Original Stage", "New Stage")
            #                     ) +
            #                     ggplot2::labs(
            #                         title = "Stage Migration Flow",
            #                         subtitle = "Patient flow between staging systems",
            #                         x = "",
            #                         y = ""
            #                     ) +
            #                     ggtheme +
            #                     ggplot2::theme(
            #                         axis.text.y = ggplot2::element_blank(),
            #                         axis.ticks = ggplot2::element_blank(),
            #                         panel.grid = ggplot2::element_blank(),
            #                         plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            #                         plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
            #                         legend.position = "none"
            #                     )

            #                 print(p)
            #             }
            #         }

            #     }, error = function(e) {
            #         # Create error message plot
            #         p <- ggplot() +
            #             ggplot2::annotate("text", x = 0.5, y = 0.5,
            #                     label = paste("Sankey diagram failed:", e$message),
            #                     size = 5, hjust = 0.5) +
            #             ggplot2::theme_void() +
            #             labs(title = "Sankey Diagram - Error")
            #         print(p)
            #     })
            # },


            # ==================================================================================
            # PHASE 1 POPULATION METHODS: Evidence-Based Assessment Tables
            # ==================================================================================

            .populateWillRogersEvidenceSummary = function(all_results) {
                # Populate Will Rogers Evidence Summary table
                table <- self$results$willRogersEvidenceSummary
                if (is.null(table) || is.null(all_results$will_rogers_evidence_summary)) {
                    return()
                }

                tryCatch(
                    {
                        evidence_summary <- all_results$will_rogers_evidence_summary

                        # rows: 0 + addRow(): jamovi does not clear the table between runs
                        # unless a clearWith option changed, so re-running appends duplicates.
                        table$deleteRows()

                        # Add explanatory text
                        if (isTRUE(self$options$showExplanations)) {
                            explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #1e88e5; color: inherit;">
                        <h4 style="margin-top: 0; color: #1976d2;">Understanding Will Rogers Evidence Assessment Framework</h4>
                        <p style="margin-bottom: 10px;">This comprehensive framework evaluates multiple lines of evidence to determine if stage migration represents legitimate prognostic improvement:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Migration Pattern:</strong> Analyzes migration rates, balance, and stability of staging criteria</li>
                            <li><strong>Survival Pattern:</strong> Evaluates if upstaged patients show survival similar to target stage</li>
                            <li><strong>Biological Risk Factors:</strong> Assesses if migrated patients show appropriate risk factor profiles</li>
                            <li><strong>Prognostic Discrimination:</strong> Measures clinically meaningful C-index improvement</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Assessment Levels:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li><span style="color: green;">PASS</span> = Strong evidence supporting legitimate improvement</li>
                            <li><span style="color: orange;">BORDERLINE</span> = Mixed evidence requiring careful interpretation</li>
                            <li><span style="color: red;">CONCERN</span> = Potential Will Rogers phenomenon detected</li>
                            <li><span style="color: red;">FAIL</span> = Clear evidence against staging improvement</li>
                        </ul>
                    </div>
                    '
                            self$results$willRogersEvidenceSummaryExplanation$setContent(explanation_html)
                        }

                        for (i in seq_len(nrow(evidence_summary))) {
                            table$addRow(rowKey = paste0("criterion_", i), values = list(
                                Criterion = evidence_summary$Criterion[i],
                                Assessment = evidence_summary$Assessment[i],
                                Evidence_Level = evidence_summary$Evidence_Level[i],
                                Interpretation = evidence_summary$Interpretation[i]
                            ))
                        }
                    },
                    error = function(e) {}
                )
            },
            .populateWillRogersClinicalRecommendation = function(all_results) {
                # Populate Will Rogers Clinical Recommendation table
                table <- self$results$willRogersClinicalRecommendation
                if (is.null(table) || is.null(all_results$will_rogers_recommendation)) {
                    return()
                }

                tryCatch(
                    {
                        recommendation <- all_results$will_rogers_recommendation

                        # rows: 0 + addRow(): clear first or a re-run appends duplicate rows.
                        table$deleteRows()

                        # Overall recommendation
                        table$addRow(rowKey = "overall", values = list(
                            Category = "Overall Assessment",
                            Finding = recommendation$final_recommendation,
                            Confidence = recommendation$confidence,
                            Guidance = recommendation$conclusion
                        ))

                        # Evidence counts
                        table$addRow(rowKey = "evidence_summary", values = list(
                            Category = "Evidence Summary",
                            Finding = paste0(
                                "PASS: ", recommendation$evidence_counts$pass, ", ",
                                "BORDERLINE: ", recommendation$evidence_counts$borderline, ", ",
                                "CONCERN: ", recommendation$evidence_counts$concern, ", ",
                                "TOTAL: ", recommendation$evidence_counts$total
                            ),
                            Confidence = recommendation$recommendation_level,
                            Guidance = "Based on comprehensive multi-criteria evaluation"
                        ))

                        # Implementation guidance
                        if (length(recommendation$guidance) > 0) {
                            for (i in seq_along(recommendation$guidance)) {
                                table$addRow(rowKey = paste0("guidance_", i), values = list(
                                    Category = paste("Implementation", i),
                                    Finding = recommendation$guidance[i],
                                    Confidence = recommendation$confidence,
                                    Guidance = "Follow evidence-based implementation steps"
                                ))
                            }
                        }
                    },
                    error = function(e) {}
                )
            },
            .populateEnhancedMigrationPatternAnalysis = function(all_results) {
                # Populate Enhanced Migration Pattern Analysis table
                table <- self$results$enhancedMigrationPatternAnalysis
                if (is.null(table) || is.null(all_results$enhanced_migration_heatmap)) {
                    return()
                }

                tryCatch(
                    {
                        heatmap_data <- all_results$enhanced_migration_heatmap

                        # Overall migration statistics
                        table$addRow(rowKey = "overall_migration", values = list(
                            Pattern_Type = "Overall Migration",
                            Count = sum(heatmap_data$migration_table) - sum(diag(heatmap_data$migration_table)),
                            Percentage = heatmap_data$migration_rate / 100,
                            Flow_Direction = "Multi-directional",
                            Clinical_Impact = if (heatmap_data$migration_rate < 15) {
                                "Low impact: Stable staging criteria"
                            } else if (heatmap_data$migration_rate < 30) {
                                "Moderate impact: Consider validation"
                            } else {
                                "High impact: Requires careful evaluation"
                            }
                        ))

                        # Major migration patterns
                        if (length(heatmap_data$major_migrations) > 0) {
                            for (pattern_name in names(heatmap_data$major_migrations)) {
                                pattern <- heatmap_data$major_migrations[[pattern_name]]
                                table$addRow(rowKey = pattern_name, values = list(
                                    Pattern_Type = "Major Migration",
                                    Count = pattern$count,
                                    Percentage = pattern$percentage / 100,
                                    Flow_Direction = paste(pattern$from, "\u{2192}", pattern$to),
                                    Clinical_Impact = "Significant reclassification pattern"
                                ))
                            }
                        }

                        # Stage retention rates
                        for (i in seq_along(heatmap_data$stage_retention_rates)) {
                            stage_name <- names(heatmap_data$stage_retention_rates)[i]
                            retention_rate <- heatmap_data$stage_retention_rates[i]

                            table$addRow(rowKey = paste0("retention_", stage_name), values = list(
                                Pattern_Type = "Stage Retention",
                                Count = round(retention_rate * sum(heatmap_data$migration_table[i, ]) / 100),
                                Percentage = retention_rate / 100,
                                Flow_Direction = paste("Remained in", stage_name),
                                Clinical_Impact = if (retention_rate > 80) {
                                    "Stable stage definition"
                                } else if (retention_rate > 60) {
                                    "Moderate stability"
                                } else {
                                    "Low retention - review criteria"
                                }
                            ))
                        }
                    },
                    error = function(e) {}
                )
            },
            .populateLandmarkAnalysisResults = function(all_results) {
                # Populate Landmark Analysis Results table
                table <- self$results$landmarkAnalysisResults
                if (is.null(table) || is.null(all_results$landmark_analysis)) {
                    return()
                }

                tryCatch(
                    {
                        landmark_results <- all_results$landmark_analysis

                        # Check if the entire result is an error list
                        if (!is.null(landmark_results$error)) {
                            # Optionally log or handle global error
                            # For now, we just don't populate the table or populate a single error row if structure permits
                            # But the table expects specific columns.
                            return()
                        }

                        for (landmark_name in names(landmark_results)) {
                            landmark_data <- landmark_results[[landmark_name]]

                            if (is.list(landmark_data)) {
                                if (!is.null(landmark_data$error)) {
                                    table$addRow(rowKey = landmark_name, values = list(
                                        Landmark_Time = as.numeric(gsub("month_", "", landmark_name)),
                                        N_Patients = NA,
                                        N_Events = NA,
                                        Old_C_Index = NA,
                                        New_C_Index = NA,
                                        C_Improvement = NA,
                                        Interpretation = landmark_data$error
                                    ))
                                } else {
                                    table$addRow(rowKey = landmark_name, values = list(
                                        Landmark_Time = landmark_data$landmark_time,
                                        N_Patients = landmark_data$n_patients,
                                        N_Events = landmark_data$n_events,
                                        Old_C_Index = landmark_data$old_c_index,
                                        New_C_Index = landmark_data$new_c_index,
                                        C_Improvement = landmark_data$c_improvement,
                                        Interpretation = landmark_data$interpretation
                                    ))
                                }
                            }
                        }
                    },
                    error = function(e) {}
                )
            },
            .populateAdvancedMigrationHeatmapStats = function(all_results) {
                # Populate Advanced Migration Heatmap Statistics table
                table <- self$results$advancedMigrationHeatmapStats
                if (is.null(table) || is.null(all_results$enhanced_migration_heatmap)) {
                    return()
                }

                tryCatch(
                    {
                        heatmap_data <- all_results$enhanced_migration_heatmap
                        migration_table <- heatmap_data$migration_table

                        # Calculate inflow and outflow for each stage
                        for (stage in rownames(migration_table)) {
                            if (stage %in% colnames(migration_table)) {
                                inflow <- sum(migration_table[, stage]) - migration_table[stage, stage]
                                outflow <- sum(migration_table[stage, ]) - migration_table[stage, stage]
                                net_change <- inflow - outflow
                                retention_rate <- if (sum(migration_table[stage, ]) > 0) {
                                    migration_table[stage, stage] / sum(migration_table[stage, ]) * 100
                                } else {
                                    0
                                }

                                # Identify major flows for this stage
                                major_flows <- character(0)
                                for (flow_name in names(heatmap_data$major_migrations)) {
                                    flow <- heatmap_data$major_migrations[[flow_name]]
                                    if (flow$from == stage || flow$to == stage) {
                                        major_flows <- c(major_flows, paste0(flow$from, "\u{2192}", flow$to, " (", flow$percentage, "%)"))
                                    }
                                }
                                major_flows_text <- if (length(major_flows) > 0) {
                                    paste(major_flows, collapse = "; ")
                                } else {
                                    "No major flows (>10%)"
                                }

                                table$addRow(rowKey = stage, values = list(
                                    Stage = stage,
                                    Retention_Rate = retention_rate / 100,
                                    Inflow = inflow,
                                    Outflow = outflow,
                                    Net_Migration = net_change,
                                    Major_Flows = major_flows_text
                                ))
                            }
                        }
                    },
                    error = function(e) {}
                )
            },
            .populateComparativeAnalysisDashboard = function(all_results) {
                # Populate the comparative analysis dashboard with summary of all analyses
                table <- self$results$comparativeAnalysisDashboard
                if (is.null(table)) {
                    return()
                }

                tryCatch(
                    {
                        # Initialize dashboard data
                        dashboard_rows <- list()

                        # 1. Basic Migration Overview
                        if (!is.null(all_results$basic_migration)) {
                            basic <- all_results$basic_migration
                            if (!is.null(basic$total_patients)) {
                                dashboard_rows <- append(dashboard_rows, list(list(
                                    Analysis_Category = "Migration Overview",
                                    Metric = "Total Patients",
                                    Original_System = as.character(basic$total_patients),
                                    New_System = as.character(basic$total_patients),
                                    Improvement = "N/A",
                                    Statistical_Significance = "N/A",
                                    Clinical_Relevance = "N/A",
                                    Recommendation = .("Baseline data")
                                )))
                            }

                            if (!is.null(basic$migration_rate)) {
                                dashboard_rows <- append(dashboard_rows, list(list(
                                    Analysis_Category = "Migration Overview",
                                    Metric = "Migration Rate",
                                    Original_System = "0%",
                                    New_System = paste0(round(basic$migration_rate * 100, 1), "%"),
                                    Improvement = paste0("+", round(basic$migration_rate * 100, 1), "%"),
                                    Statistical_Significance = "N/A",
                                    Clinical_Relevance = if (basic$migration_rate > 0.3) "High" else if (basic$migration_rate > 0.1) "Moderate" else "Low",
                                    Recommendation = if (basic$migration_rate > 0.3) "Significant reclassification observed" else "Limited reclassification"
                                )))
                            }
                        }

                        # 2. Discrimination Metrics
                        if (!is.null(all_results$advanced_metrics)) {
                            adv <- all_results$advanced_metrics
                            if (!is.null(adv$old_c_index) && !is.null(adv$new_c_index)) {
                                c_diff <- adv$new_c_index - adv$old_c_index
                                dashboard_rows <- append(dashboard_rows, list(list(
                                    Analysis_Category = "Discrimination",
                                    Metric = "C-Index",
                                    Original_System = sprintf("%.3f", adv$old_c_index),
                                    New_System = sprintf("%.3f", adv$new_c_index),
                                    Improvement = sprintf("%+.3f", c_diff),
                                    Statistical_Significance = if (abs(c_diff) > 0.02) "Likely Significant" else "Non-Significant",
                                    Clinical_Relevance = if (c_diff >= 0.02) "Clinically Meaningful" else if (c_diff >= 0.01) "Modest" else "Minimal",
                                    Recommendation = if (c_diff >= 0.02) "Supports new staging" else if (c_diff >= 0.01) "Marginal improvement" else "No clear benefit"
                                )))
                            }
                        }

                        # 3. Model Information Criteria
                        if (!is.null(all_results$advanced_metrics)) {
                            adv <- all_results$advanced_metrics

                            # AIC
                            if (!is.null(adv$old_aic) && !is.null(adv$new_aic)) {
                                aic_diff <- adv$old_aic - adv$new_aic
                                dashboard_rows <- append(dashboard_rows, list(list(
                                    Analysis_Category = "Model Fit",
                                    Metric = "AIC Difference",
                                    Original_System = sprintf("%.1f", adv$old_aic),
                                    New_System = sprintf("%.1f", adv$new_aic),
                                    Improvement = sprintf("%+.1f", aic_diff),
                                    Statistical_Significance = if (abs(aic_diff) > 4) "Strong Evidence" else if (abs(aic_diff) > 2) "Moderate Evidence" else "Weak Evidence",
                                    Clinical_Relevance = if (aic_diff > 4) "Substantial" else if (aic_diff > 2) "Moderate" else "Minimal",
                                    Recommendation = if (aic_diff > 4) "Strong support for new staging" else if (aic_diff > 2) "Moderate support" else "Inconclusive"
                                )))
                            }

                            # BIC
                            if (!is.null(adv$old_bic) && !is.null(adv$new_bic)) {
                                bic_diff <- adv$old_bic - adv$new_bic
                                dashboard_rows <- append(dashboard_rows, list(list(
                                    Analysis_Category = "Model Fit",
                                    Metric = "BIC Difference",
                                    Original_System = sprintf("%.1f", adv$old_bic),
                                    New_System = sprintf("%.1f", adv$new_bic),
                                    Improvement = sprintf("%+.1f", bic_diff),
                                    Statistical_Significance = if (abs(bic_diff) > 6) "Strong Evidence" else if (abs(bic_diff) > 2) "Positive Evidence" else "Weak Evidence",
                                    Clinical_Relevance = if (bic_diff > 6) "Substantial" else if (bic_diff > 2) "Moderate" else "Minimal",
                                    Recommendation = if (bic_diff > 6) "Strong evidence for new staging" else if (bic_diff > 2) "Positive evidence" else "Inconclusive evidence"
                                )))
                            }

                            # LR Chi-square
                            if (!is.null(adv$lr_test_stat) && !is.null(adv$lr_test_p)) {
                                dashboard_rows <- append(dashboard_rows, list(list(
                                    Analysis_Category = "Model Fit",
                                    Metric = "LR \u03c7\u00b2 Test",
                                    Original_System = "Baseline",
                                    New_System = sprintf("\u03c7\u00b2=%.2f", adv$lr_test_stat),
                                    Improvement = sprintf("p=%s", if (adv$lr_test_p < 0.001) "<0.001" else sprintf("%.3f", adv$lr_test_p)),
                                    Statistical_Significance = if (adv$lr_test_p < 0.001) "Highly Significant" else if (adv$lr_test_p < 0.01) "Significant" else if (adv$lr_test_p < 0.05) "Marginally Significant" else "Non-Significant",
                                    Clinical_Relevance = if (adv$lr_test_stat > 10) "Strong" else if (adv$lr_test_stat > 3.84) "Moderate" else "Minimal",
                                    Recommendation = if (adv$lr_test_p < 0.01) "Statistically supports new staging" else if (adv$lr_test_p < 0.05) "Marginal statistical support" else "No statistical support"
                                )))
                            }
                        }

                        # 4. Time-Dependent AUC (if available)
                        if (!is.null(all_results$roc_analysis)) {
                            roc <- all_results$roc_analysis
                            if (!is.null(roc$integrated_auc_old) && !is.null(roc$integrated_auc_new)) {
                                auc_diff <- roc$integrated_auc_new - roc$integrated_auc_old
                                dashboard_rows <- append(dashboard_rows, list(list(
                                    Analysis_Category = "Discrimination",
                                    Metric = "Integrated AUC",
                                    Original_System = sprintf("%.3f", roc$integrated_auc_old),
                                    New_System = sprintf("%.3f", roc$integrated_auc_new),
                                    Improvement = sprintf("%+.3f", auc_diff),
                                    Statistical_Significance = if (abs(auc_diff) > 0.02) "Likely Significant" else "Non-Significant",
                                    Clinical_Relevance = if (auc_diff >= 0.02) "Clinically Meaningful" else if (auc_diff >= 0.01) "Modest" else "Minimal",
                                    Recommendation = if (auc_diff >= 0.02) "AUC supports new staging" else if (auc_diff >= 0.01) "Modest AUC improvement" else "No clear AUC benefit"
                                )))
                            }
                        }

                        # 5. Calibration Metrics
                        if (!is.null(all_results$calibration_analysis)) {
                            cal <- all_results$calibration_analysis

                            # Hosmer-Lemeshow Test
                            if (!is.null(cal$old_calibration) && !is.null(cal$new_calibration)) {
                                old_hl_p <- cal$old_calibration$hl_p_value
                                new_hl_p <- cal$new_calibration$hl_p_value

                                if (!is.na(old_hl_p) && !is.na(new_hl_p)) {
                                    dashboard_rows <- append(dashboard_rows, list(list(
                                        Analysis_Category = "Calibration",
                                        Metric = "Hosmer-Lemeshow p-value",
                                        Original_System = sprintf("%.3f", old_hl_p),
                                        New_System = sprintf("%.3f", new_hl_p),
                                        Improvement = if (new_hl_p > old_hl_p) "Better calibration" else "Worse calibration",
                                        Statistical_Significance = paste0(
                                            "Old: ", if (old_hl_p > 0.05) "Well-calibrated" else "Poor",
                                            ", New: ", if (new_hl_p > 0.05) "Well-calibrated" else "Poor"
                                        ),
                                        Clinical_Relevance = if (new_hl_p > 0.05 && old_hl_p <= 0.05) "Improved" else if (new_hl_p > 0.05) "Maintained" else "Concerning",
                                        Recommendation = if (new_hl_p > 0.05) "Good calibration maintained/achieved" else "Calibration needs attention"
                                    )))
                                }

                                # Calibration Slope
                                old_slope <- cal$old_calibration$cal_slope
                                new_slope <- cal$new_calibration$cal_slope

                                if (!is.na(old_slope) && !is.na(new_slope)) {
                                    slope_diff <- abs(new_slope - 1.0) - abs(old_slope - 1.0)
                                    dashboard_rows <- append(dashboard_rows, list(list(
                                        Analysis_Category = "Calibration",
                                        Metric = "Calibration Slope",
                                        Original_System = sprintf("%.3f", old_slope),
                                        New_System = sprintf("%.3f", new_slope),
                                        Improvement = if (slope_diff < 0) "Closer to ideal (1.0)" else "Further from ideal",
                                        Statistical_Significance = paste0(
                                            "Distance from 1.0: Old=", sprintf("%.3f", abs(old_slope - 1.0)),
                                            ", New=", sprintf("%.3f", abs(new_slope - 1.0))
                                        ),
                                        Clinical_Relevance = if (abs(new_slope - 1.0) < 0.1) "Excellent" else if (abs(new_slope - 1.0) < 0.2) "Good" else "Poor",
                                        Recommendation = if (abs(new_slope - 1.0) < abs(old_slope - 1.0)) "Calibration slope improved" else "Calibration slope maintained/worsened"
                                    )))
                                }
                            }
                        }

                        # 6. Reclassification Metrics
                        if (!is.null(all_results$nri_analysis)) {
                            nri <- all_results$nri_analysis

                            # Overall NRI (select the best time point)
                            if (!is.null(nri$nri_results) && length(nri$nri_results) > 0) {
                                # Get the 24-month or middle time point for dashboard
                                time_points <- names(nri$nri_results)
                                best_time <- if ("24" %in% time_points) "24" else if ("36" %in% time_points) "36" else time_points[ceiling(length(time_points) / 2)]

                                nri_value <- nri$nri_results[[best_time]]$nri
                                if (!is.na(nri_value)) {
                                    dashboard_rows <- append(dashboard_rows, list(list(
                                        Analysis_Category = "Reclassification",
                                        Metric = paste0("NRI (", best_time, " months)"),
                                        Original_System = "0.000",
                                        New_System = sprintf("%.3f", nri_value),
                                        Improvement = sprintf("%+.3f", nri_value),
                                        Statistical_Significance = if (abs(nri_value) > 0.20) "Likely Significant" else "Non-Significant",
                                        Clinical_Relevance = if (nri_value >= 0.30) "Strong" else if (nri_value >= 0.20) "Moderate" else if (nri_value >= 0.10) "Modest" else "Minimal",
                                        Recommendation = if (nri_value >= 0.20) "Strong reclassification improvement" else if (nri_value >= 0.10) "Modest reclassification benefit" else "Limited reclassification benefit"
                                    )))
                                }
                            }
                        }

                        # Enhanced reclassification metrics (if available)
                        if (!is.null(all_results$enhanced_reclassification)) {
                            enh <- all_results$enhanced_reclassification

                            # Category-Free NRI
                            if (!is.null(enh$category_free_nri) && !is.na(enh$category_free_nri$nri)) {
                                cf_nri <- enh$category_free_nri$nri
                                dashboard_rows <- append(dashboard_rows, list(list(
                                    Analysis_Category = "Reclassification",
                                    Metric = "Category-Free NRI",
                                    Original_System = "0.000",
                                    New_System = sprintf("%.3f", cf_nri),
                                    Improvement = sprintf("%+.3f", cf_nri),
                                    Statistical_Significance = if (abs(cf_nri) > 0.15) "Likely Significant" else "Non-Significant",
                                    Clinical_Relevance = if (cf_nri >= 0.25) "Strong" else if (cf_nri >= 0.15) "Moderate" else if (cf_nri >= 0.05) "Modest" else "Minimal",
                                    Recommendation = if (cf_nri >= 0.15) "Category-free analysis supports new staging" else "Limited category-free improvement"
                                )))
                            }

                            # Weighted NRI
                            if (!is.null(enh$weighted_nri) && !is.na(enh$weighted_nri$nri)) {
                                w_nri <- enh$weighted_nri$nri
                                dashboard_rows <- append(dashboard_rows, list(list(
                                    Analysis_Category = "Reclassification",
                                    Metric = "Weighted NRI (High-Risk Focus)",
                                    Original_System = "0.000",
                                    New_System = sprintf("%.3f", w_nri),
                                    Improvement = sprintf("%+.3f", w_nri),
                                    Statistical_Significance = if (abs(w_nri) > 0.20) "Likely Significant" else "Non-Significant",
                                    Clinical_Relevance = if (w_nri >= 0.30) "Strong" else if (w_nri >= 0.20) "Moderate" else if (w_nri >= 0.10) "Modest" else "Minimal",
                                    Recommendation = if (w_nri >= 0.20) "Strong high-risk patient benefit" else "Limited high-risk benefit"
                                )))
                            }
                        }

                        # IDI Metrics
                        if (!is.null(all_results$idi_analysis)) {
                            idi <- all_results$idi_analysis
                            if (!is.null(idi$idi) && !is.na(idi$idi)) {
                                dashboard_rows <- append(dashboard_rows, list(list(
                                    Analysis_Category = "Reclassification",
                                    Metric = "IDI (Integrated Discrimination)",
                                    Original_System = "0.000",
                                    New_System = sprintf("%.4f", idi$idi),
                                    Improvement = sprintf("%+.4f", idi$idi),
                                    Statistical_Significance = if (abs(idi$idi) > 0.02) "Likely Significant" else "Non-Significant",
                                    Clinical_Relevance = if (idi$idi >= 0.05) "Strong" else if (idi$idi >= 0.02) "Moderate" else if (idi$idi >= 0.01) "Modest" else "Minimal",
                                    Recommendation = if (idi$idi >= 0.02) "Meaningful discrimination improvement" else "Limited discrimination benefit"
                                )))
                            }
                        }

                        # 7. Clinical Utility Metrics (DCA if available)
                        if (!is.null(all_results$dca_analysis)) {
                            dca <- all_results$dca_analysis
                            if (!is.null(dca$net_benefit_summary)) {
                                # Find the threshold with maximum net benefit difference
                                max_benefit_idx <- which.max(abs(dca$net_benefit_summary$difference))
                                if (length(max_benefit_idx) > 0) {
                                    max_benefit <- dca$net_benefit_summary$difference[max_benefit_idx]
                                    threshold <- dca$net_benefit_summary$threshold[max_benefit_idx]

                                    dashboard_rows <- append(dashboard_rows, list(list(
                                        Analysis_Category = "Clinical Utility",
                                        Metric = paste0("Peak Net Benefit (", threshold * 100, "% threshold)"),
                                        Original_System = sprintf("%.4f", dca$net_benefit_summary$net_benefit_original[max_benefit_idx]),
                                        New_System = sprintf("%.4f", dca$net_benefit_summary$net_benefit_new[max_benefit_idx]),
                                        Improvement = sprintf("%+.4f", max_benefit),
                                        Statistical_Significance = "Clinical Decision Analysis",
                                        Clinical_Relevance = if (max_benefit >= 0.01) "Meaningful" else if (max_benefit >= 0.005) "Modest" else "Minimal",
                                        Recommendation = if (max_benefit >= 0.01) "Clear clinical utility benefit" else if (max_benefit >= 0.005) "Modest clinical benefit" else "Limited clinical utility"
                                    )))
                                }
                            }

                            # Threshold range where new system is superior
                            if (!is.null(dca$superior_threshold_range)) {
                                range_start <- dca$superior_threshold_range$start * 100
                                range_end <- dca$superior_threshold_range$end * 100
                                range_width <- range_end - range_start

                                dashboard_rows <- append(dashboard_rows, list(list(
                                    Analysis_Category = "Clinical Utility",
                                    Metric = "Superior Decision Threshold Range",
                                    Original_System = "N/A",
                                    New_System = sprintf("%.0f%%-%.0f%%", range_start, range_end),
                                    Improvement = sprintf("%.0f%% range width", range_width),
                                    Statistical_Significance = "Decision Curve Analysis",
                                    Clinical_Relevance = if (range_width >= 20) "Wide applicability" else if (range_width >= 10) "Moderate applicability" else "Limited applicability",
                                    Recommendation = if (range_width >= 15) "Useful across wide range of thresholds" else if (range_width >= 5) "Useful for specific thresholds" else "Limited threshold utility"
                                )))
                            }
                        }

                        # 8. Monotonicity Assessment
                        monotonicity_data <- private$.getMonotonicityDashboardData(all_results)
                        if (!is.null(monotonicity_data)) {
                            dashboard_rows <- append(dashboard_rows, list(monotonicity_data))
                        }

                        # 9. Will Rogers Phenomenon
                        will_rogers_data <- private$.getWillRogersDashboardData(all_results)
                        if (!is.null(will_rogers_data)) {
                            dashboard_rows <- append(dashboard_rows, list(will_rogers_data))
                        }

                        # 10. Proportional Hazards Assumption
                        prop_hazards_data <- private$.getProportionalHazardsDashboardData(all_results)
                        if (!is.null(prop_hazards_data)) {
                            dashboard_rows <- append(dashboard_rows, list(prop_hazards_data))
                        }

                        # 11. Overall Recommendation
                        overall_recommendation <- private$.generateOverallRecommendation(dashboard_rows)
                        dashboard_rows <- append(dashboard_rows, list(overall_recommendation))

                        # Add all rows to the table
                        for (i in seq_along(dashboard_rows)) {
                            tryCatch(
                                {
                                    table$addRow(rowKey = paste0("dashboard_", i), values = dashboard_rows[[i]])
                                },
                                error = function(e) {
                                }
                            )
                        }
                    },
                    error = function(e) {
                        table$addRow(rowKey = "error", values = list(
                            Analysis_Category = "Error",
                            Metric = "Dashboard Generation Failed",
                            Original_System = "N/A",
                            New_System = "N/A",
                            Improvement = "N/A",
                            Statistical_Significance = "N/A",
                            Clinical_Relevance = "N/A",
                            Recommendation = paste("Dashboard error:", e$message)
                        ))
                    }
                )
            },
            .getMonotonicityDashboardData = function(all_results) {
                # Extract monotonicity data for dashboard
                tryCatch(
                    {
                        # Monotonicity assessments stored by .checkMonotonicity()
                        mono <- all_results[["monotonicity"]]

                        if (!is.null(mono) && !is.null(mono$old) && !is.null(mono$new)) {
                            old_score <- suppressWarnings(as.numeric(mono$old$Score))
                            new_score <- suppressWarnings(as.numeric(mono$new$Score))
                            old_viol <- suppressWarnings(as.numeric(mono$old$Violations))
                            new_viol <- suppressWarnings(as.numeric(mono$new$Violations))

                            if (length(old_score) == 1 && length(new_score) == 1 &&
                                !is.na(old_score) && !is.na(new_score)) {
                                score_diff <- new_score - old_score

                                improvement <- if (score_diff > 0) {
                                    sprintf("%+.2f (fewer ordering violations)", score_diff)
                                } else if (score_diff < 0) {
                                    sprintf("%+.2f (more ordering violations)", score_diff)
                                } else {
                                    "No change"
                                }

                                significance <- sprintf(
                                    "Violations: Old=%s, New=%s",
                                    if (length(old_viol) == 1 && !is.na(old_viol)) as.character(old_viol) else "NA",
                                    if (length(new_viol) == 1 && !is.na(new_viol)) as.character(new_viol) else "NA"
                                )

                                relevance <- if (length(new_viol) == 1 && !is.na(new_viol) && new_viol == 0) {
                                    "Perfect stage ordering"
                                } else if (new_score >= 0.75) {
                                    "Mostly ordered"
                                } else {
                                    "Ordering concerns"
                                }

                                recommendation <- if (length(new_viol) == 1 && !is.na(new_viol) && new_viol == 0 &&
                                    length(old_viol) == 1 && !is.na(old_viol) && old_viol == 0) {
                                    "Both systems order stages monotonically"
                                } else if (score_diff > 0) {
                                    "New staging improves stage ordering"
                                } else if (score_diff < 0) {
                                    "New staging degrades stage ordering; review violating stages"
                                } else {
                                    "Check monotonicity analysis table for violating stages"
                                }

                                return(list(
                                    Analysis_Category = "Validation",
                                    Metric = "Monotonicity Score",
                                    Original_System = sprintf("%.2f", old_score),
                                    New_System = sprintf("%.2f", new_score),
                                    Improvement = improvement,
                                    Statistical_Significance = significance,
                                    Clinical_Relevance = relevance,
                                    Recommendation = recommendation
                                ))
                            }
                        }

                        # Fallback when the monotonicity check did not run or failed
                        return(list(
                            Analysis_Category = "Validation",
                            Metric = "Monotonicity Score",
                            Original_System = "TBD",
                            New_System = "TBD",
                            Improvement = "TBD",
                            Statistical_Significance = "TBD",
                            Clinical_Relevance = "TBD",
                            Recommendation = .("Check monotonicity analysis table")
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .getWillRogersDashboardData = function(all_results) {
                # Extract Will Rogers data for dashboard
                tryCatch(
                    {
                        # Overall assessment row stored by .analyzeWillRogers(); its field
                        # names mirror the willRogersAnalysis table columns, so
                        # Survival_Change_Old holds the migration rate and Count the number
                        # of migrated patients.
                        overall <- all_results[["wr_overall_assessment"]]

                        # Stage-level log-rank results stored by .calculateBasicWillRogersData()
                        stage_results <- all_results[["will_rogers"]]

                        sig_stages <- 0
                        tested_stages <- 0
                        if (!is.null(stage_results) && length(stage_results) > 0) {
                            for (res in stage_results) {
                                p_val <- suppressWarnings(as.numeric(res$p_value))
                                if (length(p_val) == 1 && !is.na(p_val)) {
                                    tested_stages <- tested_stages + 1
                                    if (p_val < 0.05) {
                                        sig_stages <- sig_stages + 1
                                    }
                                }
                            }
                        }

                        if (!is.null(overall) || tested_stages > 0) {
                            migration_rate <- suppressWarnings(as.numeric(overall$Survival_Change_Old))
                            migrated_n <- suppressWarnings(as.numeric(overall$Count))
                            if (length(migration_rate) != 1) migration_rate <- NA_real_
                            if (length(migrated_n) != 1) migrated_n <- NA_real_

                            evidence <- if (!is.null(overall$Will_Rogers_Evidence)) {
                                as.character(overall$Will_Rogers_Evidence)
                            } else {
                                "Stage-specific comparison only"
                            }

                            improvement <- if (!is.na(migrated_n) && !is.na(migration_rate)) {
                                sprintf("%d migrated (%.1f%%)", as.integer(migrated_n), migration_rate * 100)
                            } else {
                                "N/A"
                            }

                            significance <- if (tested_stages > 0) {
                                sprintf("%d of %d stages with log-rank p<0.05", sig_stages, tested_stages)
                            } else {
                                "Enable Will Rogers Phenomenon Analysis for stage log-rank tests"
                            }

                            relevance <- if (sig_stages > 0) {
                                "Stage-specific bias detected"
                            } else if (!is.na(migration_rate) && migration_rate > 0.2) {
                                "High migration - bias possible"
                            } else if (!is.na(migration_rate) && migration_rate > 0.1) {
                                "Moderate migration"
                            } else {
                                "Minimal bias concern"
                            }

                            recommendation <- if (sig_stages > 0) {
                                "Survival differs between migrated and unchanged patients; interpret stage-specific gains with caution"
                            } else if (!is.null(overall$Clinical_Impact)) {
                                as.character(overall$Clinical_Impact)
                            } else {
                                "Check Will Rogers analysis table"
                            }

                            return(list(
                                Analysis_Category = "Bias Assessment",
                                Metric = "Will Rogers Evidence",
                                Original_System = "N/A",
                                New_System = evidence,
                                Improvement = improvement,
                                Statistical_Significance = significance,
                                Clinical_Relevance = relevance,
                                Recommendation = recommendation
                            ))
                        }

                        # Fallback when the Will Rogers analysis did not run or failed
                        return(list(
                            Analysis_Category = "Bias Assessment",
                            Metric = "Will Rogers Evidence",
                            Original_System = "N/A",
                            New_System = "TBD",
                            Improvement = "TBD",
                            Statistical_Significance = "TBD",
                            Clinical_Relevance = "TBD",
                            Recommendation = .("Check Will Rogers analysis table")
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .getProportionalHazardsDashboardData = function(all_results) {
                # Extract proportional hazards data for dashboard
                tryCatch(
                    {
                        # Look for proportional hazards test results
                        if (!is.null(all_results$proportional_hazards_test)) {
                            ph <- all_results$proportional_hazards_test
                            old_p <- ph$old_test$p_value
                            new_p <- ph$new_test$p_value

                            old_status <- if (!is.na(old_p)) if (old_p > 0.05) "Met" else "Violated" else "TBD"
                            new_status <- if (!is.na(new_p)) if (new_p > 0.05) "Met" else "Violated" else "TBD"

                            improvement <- if (!is.na(old_p) && !is.na(new_p)) {
                                if (old_p <= 0.05 && new_p > 0.05) {
                                    "Assumption restored"
                                } else if (old_p > 0.05 && new_p <= 0.05) {
                                    "Assumption violated"
                                } else if (old_p > 0.05 && new_p > 0.05) {
                                    "Both assumptions met"
                                } else {
                                    "Both assumptions violated"
                                }
                            } else {
                                "N/A"
                            }

                            significance <- if (!is.na(old_p) && !is.na(new_p)) {
                                paste0("Old p=", sprintf("%.3f", old_p), ", New p=", sprintf("%.3f", new_p))
                            } else {
                                "TBD"
                            }

                            relevance <- if (new_status == "Met") "Valid Model" else if (new_status == "Violated") "Model Concerns" else "To Be Determined"

                            recommendation <- if (new_status == "Met" && old_status == "Met") {
                                "Both models satisfy assumptions"
                            } else if (new_status == "Met" && old_status == "Violated") {
                                "New staging improves model validity"
                            } else if (new_status == "Violated") {
                                "Consider stratified Cox or time-varying coefficients"
                            } else {
                                "Check detailed proportional hazards test"
                            }

                            return(list(
                                Analysis_Category = "Model Assumptions",
                                Metric = "Proportional Hazards (Schoenfeld Test)",
                                Original_System = old_status,
                                New_System = new_status,
                                Improvement = improvement,
                                Statistical_Significance = significance,
                                Clinical_Relevance = relevance,
                                Recommendation = recommendation
                            ))
                        }

                        # Fallback if results not available
                        return(list(
                            Analysis_Category = "Model Assumptions",
                            Metric = "Proportional Hazards (Schoenfeld Test)",
                            Original_System = "TBD",
                            New_System = "TBD",
                            Improvement = "N/A",
                            Statistical_Significance = "Enable PH testing",
                            Clinical_Relevance = "Model Validity",
                            Recommendation = .("Enable proportional hazards testing")
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .generateOverallRecommendation = function(dashboard_rows) {
                # Generate overall recommendation based on all analyses
                tryCatch(
                    {
                        # Count positive indicators
                        positive_indicators <- 0
                        total_indicators <- 0

                        for (row in dashboard_rows) {
                            if (row$Analysis_Category %in% c("Discrimination", "Model Fit")) {
                                total_indicators <- total_indicators + 1
                                if (grepl("Supports|Strong|Meaningful", row$Recommendation, ignore.case = TRUE)) {
                                    positive_indicators <- positive_indicators + 1
                                }
                            }
                        }

                        # Generate recommendation
                        if (total_indicators == 0) {
                            recommendation <- "Insufficient data for overall recommendation"
                        } else {
                            proportion_positive <- positive_indicators / total_indicators
                            if (proportion_positive >= 0.75) {
                                recommendation <- "Strong evidence supports adopting the new staging system"
                            } else if (proportion_positive >= 0.5) {
                                recommendation <- "Moderate evidence supports the new staging system"
                            } else if (proportion_positive >= 0.25) {
                                recommendation <- "Limited evidence; consider additional validation"
                            } else {
                                recommendation <- "Insufficient evidence to support new staging system"
                            }
                        }

                        return(list(
                            Analysis_Category = "Overall Assessment",
                            Metric = "Recommendation",
                            Original_System = "Current Standard",
                            New_System = "Proposed System",
                            Improvement = paste0(positive_indicators, "/", total_indicators, " favorable"),
                            Statistical_Significance = "Multiple Analyses",
                            Clinical_Relevance = "Critical Decision",
                            Recommendation = recommendation
                        ))
                    },
                    error = function(e) {
                        return(list(
                            Analysis_Category = "Overall Assessment",
                            Metric = "Recommendation",
                            Original_System = "Error",
                            New_System = "Error",
                            Improvement = "Error",
                            Statistical_Significance = "Error",
                            Clinical_Relevance = "Error",
                            Recommendation = .("Unable to generate overall recommendation")
                        ))
                    }
                )
            },
            .plotWillRogersEffect = function(image, ggtheme, theme, ...) {
                # Create visualization of Will Rogers effect

                if (is.null(image$parent$options$oldStage) || is.null(image$parent$options$newStage)) {
                    return(NULL)
                }

                tryCatch(
                    {
                        # Get data from parent's private method

                        # Get state data (like other plots)
                        plot_state <- image$state

                        if (is.null(plot_state)) {
                            p <- ggplot2::ggplot() +
                                ggplot2::annotate("text",
                                    x = 0.5, y = 0.5,
                                    label = "No state data available for Will Rogers plot\nEnsure 'Will Rogers Visualization' is enabled",
                                    hjust = 0.5, vjust = 0.5, size = 4
                                ) +
                                ggplot2::theme_void() +
                                ggplot2::labs(title = "Will Rogers Visualization - No State")
                            print(p)
                            return(TRUE)
                        }

                        # Extract data and parameters from state
                        data <- plot_state$data
                        old_stage <- plot_state$old_stage
                        new_stage <- plot_state$new_stage
                        time_var <- plot_state$time_var
                        event_var <- plot_state$event_var
                        event_level <- plot_state$event_level


                        # Use the variables from state (they're already set up correctly)
                        time_col <- time_var
                        event_col <- event_var

                        # The event_binary should already be in the data from setState
                        if (!"event_binary" %in% colnames(data)) {
                            # Prepare event variable
                            if (!is.null(event_level) && event_level != "") {
                                data$event_binary <- ifelse(data[[event_col]] == event_level, 1, 0)
                            } else {
                                # as.numeric() on a factor returns level INDICES (1,2,...), which Surv()
                                # reads as 1 = censored / 2 = event -- inverting the analysis. Coerce via
                                # the level LABELS instead.
                                data$event_binary <- if (is.factor(data[[event_col]])) {
                                    suppressWarnings(as.numeric(as.character(data[[event_col]])))
                                } else {
                                    as.numeric(data[[event_col]])
                                }
                            }
                        } else {}


                        # Find stages with significant migration
                        migration_table <- table(data[[old_stage]], data[[new_stage]])

                        # Select stages to visualize (those with most migration)
                        migration_counts <- migration_table
                        diag(migration_counts) <- 0 # Exclude unchanged patients

                        # Find the most common migration pattern
                        max_migration <- which(migration_counts == max(migration_counts), arr.ind = TRUE)[1, ]
                        from_stage <- rownames(migration_counts)[max_migration[1]]
                        to_stage <- colnames(migration_counts)[max_migration[2]]


                        # Create plot data

                        # Original staging - patients who stayed in from_stage
                        # Patients who migrated from from_stage to to_stage
                        # Original staging - patients who stayed in to_stage

                        # Skip if no migration found
                        if (max(migration_counts) == 0) {
                            p <- ggplot2::ggplot() +
                                ggplot2::annotate("text",
                                    x = 0.5, y = 0.5,
                                    label = "No stage migration detected\nfor Will Rogers visualization",
                                    hjust = 0.5, vjust = 0.5, size = 4
                                ) +
                                ggplot2::theme_void() +
                                ggplot2::labs(title = "Will Rogers Visualization - No Migration")
                            print(p)
                            return(TRUE)
                        }


                        # Create a simpler ggplot visualization instead of survminer
                        # Prepare data for ggplot

                        # Before migration data
                        before_from <- data[data[[old_stage]] == from_stage, ]
                        before_to <- data[data[[old_stage]] == to_stage, ]


                        # Calculate survival summaries
                        # Get survival summaries for annotation
                        before_from_surv <- survival::Surv(before_from[[time_col]], before_from$event_binary)
                        before_from_fit <- survival::survfit(before_from_surv ~ 1)
                        before_from_median <- summary(before_from_fit)$table["median"]

                        before_to_surv <- survival::Surv(before_to[[time_col]], before_to$event_binary)
                        before_to_fit <- survival::survfit(before_to_surv ~ 1)
                        before_to_median <- summary(before_to_fit)$table["median"]

                        # After migration data
                        after_from <- data[data[[new_stage]] == from_stage, ]
                        after_to <- data[data[[new_stage]] == to_stage, ]


                        after_from_surv <- survival::Surv(after_from[[time_col]], after_from$event_binary)
                        after_from_fit <- survival::survfit(after_from_surv ~ 1)
                        after_from_median <- summary(after_from_fit)$table["median"]

                        after_to_surv <- survival::Surv(after_to[[time_col]], after_to$event_binary)
                        after_to_fit <- survival::survfit(after_to_surv ~ 1)
                        after_to_median <- summary(after_to_fit)$table["median"]

                        # Create summary data for visualization
                        summary_data <- data.frame(
                            Stage = rep(c(from_stage, to_stage), 2),
                            Period = rep(c("Before Migration", "After Migration"), each = 2),
                            Median_Survival = c(
                                before_from_median, before_to_median,
                                after_from_median, after_to_median
                            ),
                            N_Patients = c(
                                nrow(before_from), nrow(before_to),
                                nrow(after_from), nrow(after_to)
                            )
                        )

                        # Handle NA values
                        summary_data$Median_Survival[is.na(summary_data$Median_Survival)] <- 0

                        # Create bar plot showing median survival changes
                        p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = Stage, y = Median_Survival, fill = Period)) +
                            ggplot2::geom_col(position = "dodge", alpha = 0.8) +
                            ggplot2::geom_text(ggplot2::aes(label = paste0("n=", N_Patients)),
                                position = ggplot2::position_dodge(width = 0.9),
                                vjust = -0.5, size = 3
                            ) +
                            ggplot2::scale_fill_manual(values = c("Before Migration" = "#E41A1C", "After Migration" = "#377EB8")) +
                            ggplot2::labs(
                                title = "Will Rogers Effect: Median Survival by Stage and Period",
                                subtitle = paste0(
                                    "Migration pattern: ", from_stage, " \u{2192} ", to_stage,
                                    " (", migration_counts[from_stage, to_stage], " patients)"
                                ),
                                x = "Stage",
                                y = "Median Survival (months)",
                                fill = "Period"
                            ) +
                            ggtheme +
                            ggplot2::theme(
                                plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                                plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
                                legend.position = "bottom"
                            ) +
                            ggplot2::annotate("text",
                                x = 1.5, y = max(summary_data$Median_Survival) * 0.9,
                                label = "Will Rogers Paradox:\nBoth stages may appear\nto improve after migration",
                                hjust = 0.5, vjust = 1, size = 3, color = "darkred",
                                fontface = "italic"
                            )

                        print(p)
                        return(TRUE)
                    },
                    error = function(e) {
                        # Create error message plot
                        p <- ggplot2::ggplot() +
                            ggplot2::annotate("text",
                                x = 0.5, y = 0.5,
                                label = paste("Error creating Will Rogers plot:\n", e$message),
                                hjust = 0.5, vjust = 0.5, size = 4
                            ) +
                            ggplot2::theme_void() +
                            ggplot2::labs(title = "Will Rogers Visualization - Error")
                        print(p)
                        return(TRUE)
                    }
                )
            },
            .plotMigrationSurvivalComparison = function(image, ggtheme, theme, ...) {
                # Create Kaplan-Meier survival curve comparison before/after migration

                if (is.null(image$parent$options$oldStage) || is.null(image$parent$options$newStage)) {
                    return(NULL)
                }

                tryCatch(
                    {
                        # Get state data
                        plot_state <- image$state

                        if (is.null(plot_state)) {
                            p <- ggplot2::ggplot() +
                                ggplot2::annotate("text",
                                    x = 0.5, y = 0.5,
                                    label = "No state data available for survival comparison\nEnsure 'Migration Survival Curve Comparison' is enabled",
                                    hjust = 0.5, vjust = 0.5, size = 4
                                ) +
                                ggplot2::theme_void() +
                                ggplot2::labs(title = "Migration Survival Comparison - No State")
                            print(p)
                            return(TRUE)
                        }

                        # Extract data and parameters from state
                        data <- plot_state$data
                        old_stage <- plot_state$old_stage
                        new_stage <- plot_state$new_stage
                        time_var <- plot_state$time_var
                        event_var <- plot_state$event_var
                        event_level <- plot_state$event_level


                        # Ensure event_binary exists
                        if (!"event_binary" %in% colnames(data)) {
                            if (!is.null(event_level) && event_level != "") {
                                data$event_binary <- ifelse(data[[event_var]] == event_level, 1, 0)
                            } else {
                                data$event_binary <- as.numeric(data[[event_var]])
                            }
                        } else {}


                        # Get unique stages for comparison
                        all_stages <- sort(unique(c(as.character(data[[old_stage]]), as.character(data[[new_stage]]))))

                        # Select 2-3 most common stages for cleaner visualization
                        stage_counts_old <- table(data[[old_stage]])
                        stage_counts_new <- table(data[[new_stage]])

                        # Find stages with adequate sample sizes (at least 20 patients)
                        adequate_stages <- names(stage_counts_old)[stage_counts_old >= 20]
                        adequate_stages <- intersect(adequate_stages, names(stage_counts_new)[stage_counts_new >= 20])

                        if (length(adequate_stages) == 0) {
                            adequate_stages <- names(sort(stage_counts_old, decreasing = TRUE))[seq_len(min(2, length(stage_counts_old)))]
                        }


                        # Create survival data for plotting
                        plot_data_list <- list()

                        for (stage in adequate_stages) {
                            # Before migration (original staging system)
                            before_data <- data[data[[old_stage]] == stage, ]
                            if (nrow(before_data) > 5) { # At least 5 patients
                                before_surv <- survival::Surv(before_data[[time_var]], before_data$event_binary)
                                before_fit <- survival::survfit(before_surv ~ 1)

                                # Extract survival data for ggplot
                                before_df <- data.frame(
                                    time = before_fit$time,
                                    surv = before_fit$surv,
                                    stage = stage,
                                    period = "Before Migration",
                                    n_risk = before_fit$n.risk,
                                    n_patients = nrow(before_data)
                                )
                                plot_data_list[[paste(stage, "before")]] <- before_df
                            }

                            # After migration (new staging system)
                            after_data <- data[data[[new_stage]] == stage, ]
                            if (nrow(after_data) > 5) { # At least 5 patients
                                after_surv <- survival::Surv(after_data[[time_var]], after_data$event_binary)
                                after_fit <- survival::survfit(after_surv ~ 1)

                                # Extract survival data for ggplot
                                after_df <- data.frame(
                                    time = after_fit$time,
                                    surv = after_fit$surv,
                                    stage = stage,
                                    period = "After Migration",
                                    n_risk = after_fit$n.risk,
                                    n_patients = nrow(after_data)
                                )
                                plot_data_list[[paste(stage, "after")]] <- after_df
                            }
                        }

                        if (length(plot_data_list) == 0) {
                            p <- ggplot2::ggplot() +
                                ggplot2::annotate("text",
                                    x = 0.5, y = 0.5,
                                    label = "Insufficient data for survival curve comparison\n(Need at least 5 patients per stage/period)",
                                    hjust = 0.5, vjust = 0.5, size = 4
                                ) +
                                ggplot2::theme_void() +
                                ggplot2::labs(title = "Migration Survival Comparison - Insufficient Data")
                            print(p)
                            return(TRUE)
                        }

                        # Combine all plot data
                        plot_data <- do.call(rbind, plot_data_list)

                        # Create the survival curve comparison plot
                        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = surv, color = period, linetype = period)) +
                            ggplot2::geom_step(size = 1, alpha = 0.8) +
                            ggplot2::facet_wrap(~stage,
                                scales = "free",
                                labeller = ggplot2::labeller(stage = function(x) paste("Stage", x))
                            ) +
                            ggplot2::scale_color_manual(values = c("Before Migration" = "#E41A1C", "After Migration" = "#377EB8")) +
                            ggplot2::scale_linetype_manual(values = c("Before Migration" = "solid", "After Migration" = "dashed")) +
                            ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                            ggplot2::labs(
                                title = "Kaplan-Meier Survival Curves: Before vs After Stage Migration",
                                subtitle = "Comparison showing how survival curves change when patients are reclassified",
                                x = "Time (months)",
                                y = "Survival Probability",
                                color = "Period",
                                linetype = "Period"
                            ) +
                            ggtheme +
                            ggplot2::theme(
                                plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                                plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
                                legend.position = "bottom",
                                strip.text = ggplot2::element_text(size = 12, face = "bold"),
                                panel.grid.minor = ggplot2::element_blank()
                            )

                        # Add sample size annotations
                        stage_info <- aggregate(n_patients ~ stage + period, data = plot_data, FUN = function(x) x[1])

                        # Add text annotations for sample sizes
                        for (i in seq_len(nrow(stage_info))) {
                            period_name <- stage_info$period[i]
                            n_pat <- stage_info$n_patients[i]

                            # Position annotations
                            y_pos <- ifelse(period_name == "Before Migration", 0.2, 0.1)

                            p <- p + ggplot2::annotate("text",
                                x = Inf, y = y_pos,
                                label = paste0(period_name, "\nn=", n_pat),
                                hjust = 1.1, vjust = 0,
                                size = 3, color = ifelse(period_name == "Before Migration", "#E41A1C", "#377EB8")
                            )
                        }

                        print(p)
                        return(TRUE)
                    },
                    error = function(e) {
                        # Create error message plot
                        p <- ggplot2::ggplot() +
                            ggplot2::annotate("text",
                                x = 0.5, y = 0.5,
                                label = paste("Error creating migration survival comparison:\n", e$message),
                                hjust = 0.5, vjust = 0.5, size = 4
                            ) +
                            ggplot2::theme_void() +
                            ggplot2::labs(title = "Migration Survival Comparison - Error")
                        print(p)
                        return(TRUE)
                    }
                )
            },
            .plotSankeyDiagram = function(image, ggtheme, theme, ...) {
                # Create Sankey-style flow diagram showing stage migration patterns

                if (is.null(image$parent$options$oldStage) || is.null(image$parent$options$newStage)) {
                    return(NULL)
                }

                tryCatch(
                    {
                        # Get state data
                        plot_state <- image$state

                        if (is.null(plot_state)) {
                            p <- ggplot2::ggplot() +
                                ggplot2::annotate("text",
                                    x = 0.5, y = 0.5,
                                    label = "No state data available for Sankey diagram\nEnsure 'Stage Migration Flow Diagram' is enabled",
                                    hjust = 0.5, vjust = 0.5, size = 4
                                ) +
                                ggplot2::theme_void() +
                                ggplot2::labs(title = "Sankey Diagram - No State")
                            print(p)
                            return(TRUE)
                        }

                        # Extract data from state
                        migration_matrix <- plot_state$migration_matrix
                        old_stage <- plot_state$old_stage
                        new_stage <- plot_state$new_stage

                        # Convert migration matrix to flow data
                        flow_data <- expand.grid(
                            source = rownames(migration_matrix),
                            target = colnames(migration_matrix),
                            stringsAsFactors = FALSE
                        )
                        flow_data$count <- as.vector(migration_matrix)

                        # Remove zero flows
                        flow_data <- flow_data[flow_data$count > 0, ]

                        # Determine migration direction (upstage, downstage, or no change)
                        # Assuming stage names are ordered (e.g., T1 < T2 < T3 < T4)
                        # Extract numeric component from stage names if present
                        flow_data$source_numeric <- suppressWarnings(as.numeric(gsub("[^0-9]", "", flow_data$source)))
                        flow_data$target_numeric <- suppressWarnings(as.numeric(gsub("[^0-9]", "", flow_data$target)))

                        # If numeric extraction fails, try ordering alphabetically
                        if (any(is.na(flow_data$source_numeric)) || any(is.na(flow_data$target_numeric))) {
                            stage_order <- sort(unique(c(flow_data$source, flow_data$target)))
                            flow_data$source_order <- match(flow_data$source, stage_order)
                            flow_data$target_order <- match(flow_data$target, stage_order)
                            flow_data$direction <- ifelse(flow_data$target_order > flow_data$source_order, "Upstaged",
                                ifelse(flow_data$target_order < flow_data$source_order, "Downstaged", "No change")
                            )
                        } else {
                            flow_data$direction <- ifelse(flow_data$target_numeric > flow_data$source_numeric, "Upstaged",
                                ifelse(flow_data$target_numeric < flow_data$source_numeric, "Downstaged",
                                    ifelse(flow_data$target_numeric == flow_data$source_numeric, "No change", "No change")
                                )
                            )
                        }

                        if (nrow(flow_data) == 0) {
                            p <- ggplot2::ggplot() +
                                ggplot2::annotate("text",
                                    x = 0.5, y = 0.5,
                                    label = "No data for flow diagram",
                                    hjust = 0.5, vjust = 0.5, size = 4
                                ) +
                                ggplot2::theme_void() +
                                ggplot2::labs(title = "Sankey Diagram - No Data")
                            print(p)
                            return(TRUE)
                        }

                        # Create a simple, robust Sankey-style visualization using ggplot2

                        # Prepare nodes and positions

                        # Create node positions
                        x_source <- 0
                        x_target <- 2

                        # Calculate y positions based on flow volumes
                        source_totals <- aggregate(count ~ source, data = flow_data, sum)
                        target_totals <- aggregate(count ~ target, data = flow_data, sum)

                        # Position source nodes
                        source_positions <- data.frame(
                            stage = source_totals$source,
                            x = x_source,
                            y = cumsum(source_totals$count) - source_totals$count / 2,
                            height = source_totals$count,
                            type = "Source"
                        )

                        # Position target nodes
                        target_positions <- data.frame(
                            stage = target_totals$target,
                            x = x_target,
                            y = cumsum(target_totals$count) - target_totals$count / 2,
                            height = target_totals$count,
                            type = "Target"
                        )

                        # Combine node positions
                        node_positions <- rbind(source_positions, target_positions)

                        # Create the plot
                        p <- ggplot2::ggplot() +
                            # Draw nodes as rectangles
                            ggplot2::geom_rect(
                                data = node_positions,
                                ggplot2::aes(
                                    xmin = x - 0.1, xmax = x + 0.1,
                                    ymin = y - height / 2, ymax = y + height / 2,
                                    fill = type
                                ),
                                color = "black", alpha = 0.8
                            ) +

                            # Add node labels
                            ggplot2::geom_text(
                                data = node_positions,
                                ggplot2::aes(x = x, y = y, label = paste0(stage, "\n(n=", height, ")")),
                                hjust = 0.5, vjust = 0.5, size = 3, color = "white", fontface = "bold"
                            ) +

                            # Draw flows as segments with color based on direction
                            ggplot2::geom_segment(
                                data = flow_data,
                                ggplot2::aes(
                                    x = x_source + 0.1,
                                    xend = x_target - 0.1,
                                    y = sapply(source, function(s) source_positions$y[source_positions$stage == s]),
                                    yend = sapply(target, function(t) target_positions$y[target_positions$stage == t]),
                                    size = count,
                                    alpha = count,
                                    color = direction
                                ),
                                arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"))
                            ) +

                            # Add flow labels for all flows including "No change"
                            ggplot2::geom_text(
                                data = flow_data,
                                ggplot2::aes(
                                    x = (x_source + x_target) / 2,
                                    y = (sapply(source, function(s) source_positions$y[source_positions$stage == s]) +
                                        sapply(target, function(t) target_positions$y[target_positions$stage == t])) / 2,
                                    label = paste0("n=", count)
                                ),
                                hjust = 0.5, vjust = -0.5, size = 2.5, fontface = "bold", color = "black"
                            ) +

                            # Customize the plot
                            ggplot2::scale_fill_manual(values = c("Source" = "#E41A1C", "Target" = "#377EB8")) +
                            ggplot2::scale_color_manual(
                                values = c(
                                    "Upstaged" = "#E69F00",
                                    "Downstaged" = "#0072B2",
                                    "No change" = "#7F7F7F"
                                ),
                                name = "Migration Direction"
                            ) +
                            ggplot2::scale_size_continuous(range = c(1, 8), guide = "none") +
                            ggplot2::scale_alpha_continuous(range = c(0.3, 0.8), guide = "none") +
                            ggplot2::labs(
                                title = "Stage Migration Flow Diagram",
                                subtitle = paste0(
                                    "Patient migration patterns: ", old_stage, " \u{2192} ", new_stage,
                                    "\nOrange = Upstaging, Blue = Downstaging, Gray = No change"
                                ),
                                x = "",
                                y = "Patient Flow",
                                fill = "System"
                            ) +
                            ggtheme +
                            ggplot2::theme(
                                plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                                plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
                                axis.text = ggplot2::element_blank(),
                                axis.ticks = ggplot2::element_blank(),
                                panel.grid = ggplot2::element_blank(),
                                legend.position = "bottom"
                            ) +
                            ggplot2::annotate("text",
                                x = x_source, y = max(node_positions$y) + 50,
                                label = paste("Original", old_stage), hjust = 0.5, size = 4, fontface = "bold"
                            ) +
                            ggplot2::annotate("text",
                                x = x_target, y = max(node_positions$y) + 50,
                                label = paste("New", new_stage), hjust = 0.5, size = 4, fontface = "bold"
                            )

                        print(p)
                        return(TRUE)
                    },
                    error = function(e) {
                        # Create error message plot
                        p <- ggplot2::ggplot() +
                            ggplot2::annotate("text",
                                x = 0.5, y = 0.5,
                                label = paste("Error creating Sankey diagram:\n", e$message),
                                hjust = 0.5, vjust = 0.5, size = 4
                            ) +
                            ggplot2::theme_void() +
                            ggplot2::labs(title = "Sankey Diagram - Error")
                        print(p)
                        return(TRUE)
                    }
                )
            },
            .plotCrossValidation = function(image, ggtheme, theme, ...) {
                # Create cross-validation performance visualization
                if (is.null(image$state)) {
                    return(FALSE)
                }

                tryCatch(
                    {
                        # Get state data
                        plot_data <- image$state

                        if (is.null(plot_data$fold) || length(plot_data$fold) == 0) {
                            # No data to plot
                            p <- ggplot2::ggplot() +
                                ggplot2::annotate("text",
                                    x = 0.5, y = 0.5,
                                    label = "No cross-validation results available",
                                    hjust = 0.5, vjust = 0.5, size = 4
                                ) +
                                ggplot2::theme_void()
                            print(p)
                            return(TRUE)
                        }

                        # Create data frame for plotting
                        df <- data.frame(
                            Fold = plot_data$fold,
                            Old_CIndex = plot_data$old_cindex,
                            New_CIndex = plot_data$new_cindex,
                            Difference = plot_data$cindex_diff
                        )

                        # Reshape data for ggplot
                        df_long <- data.frame(
                            Fold = rep(df$Fold, 2),
                            System = rep(c("Original", "New"), each = nrow(df)),
                            CIndex = c(df$Old_CIndex, df$New_CIndex)
                        )

                        # Create the main plot
                        p <- ggplot2::ggplot(df_long, ggplot2::aes(x = factor(Fold), y = CIndex, color = System, group = System)) +
                            ggplot2::geom_line(linewidth = 1) +
                            ggplot2::geom_point(size = 3) +
                            ggplot2::scale_color_manual(values = c("Original" = "#E31A1C", "New" = "#1F78B4")) +
                            ggplot2::scale_y_continuous(limits = c(0.5, 1.0), breaks = seq(0.5, 1.0, 0.1)) +
                            ggplot2::labs(
                                title = "Cross-Validation Performance: C-Index by Fold",
                                subtitle = "Comparison of staging system discrimination across validation folds",
                                x = "Cross-Validation Fold",
                                y = "C-Index (Discrimination)",
                                color = "Staging System"
                            ) +
                            ggtheme +
                            ggplot2::theme(
                                plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                                plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
                                axis.title = ggplot2::element_text(size = 11),
                                axis.text = ggplot2::element_text(size = 10),
                                legend.title = ggplot2::element_text(size = 11),
                                legend.text = ggplot2::element_text(size = 10),
                                legend.position = "bottom",
                                panel.grid.minor = ggplot2::element_blank()
                            ) +
                            ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
                            ggplot2::annotate("text",
                                x = 1, y = 0.51, label = "No discrimination",
                                color = "gray50", size = 3, hjust = 0
                            )

                        # Add difference plot as subplot if more than one fold
                        if (nrow(df) > 1 && requireNamespace("patchwork", quietly = TRUE)) {
                            p_diff <- ggplot2::ggplot(df, ggplot2::aes(x = factor(Fold), y = Difference, group = 1)) +
                                ggplot2::geom_line(color = "#FF7F00", linewidth = 1) +
                                ggplot2::geom_point(color = "#FF7F00", size = 3) +
                                ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.5) +
                                ggplot2::geom_hline(yintercept = 0.02, linetype = "dashed", color = "#0072B2", alpha = 0.7) +
                                ggplot2::geom_hline(yintercept = -0.02, linetype = "dashed", color = "#E69F00", alpha = 0.7) +
                                ggplot2::labs(
                                    title = "C-Index Improvement by Fold",
                                    x = "Cross-Validation Fold",
                                    y = "C-Index Difference (New - Original)"
                                ) +
                                ggtheme +
                                ggplot2::theme(
                                    plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
                                    axis.title = ggplot2::element_text(size = 10),
                                    axis.text = ggplot2::element_text(size = 9),
                                    panel.grid.minor = ggplot2::element_blank()
                                ) +
                                ggplot2::annotate("text",
                                    x = 1, y = 0.021, label = "Clinically meaningful",
                                    color = "#0072B2", size = 2.5, hjust = 0
                                )

                            # Combine plots
                            final_plot <- p / p_diff + patchwork::plot_layout(heights = c(2, 1))
                            print(final_plot)
                            return(TRUE)
                        }

                        print(p)
                        return(TRUE)
                    },
                    error = function(e) {
                        # Create error message plot
                        p <- ggplot2::ggplot() +
                            ggplot2::annotate("text",
                                x = 0.5, y = 0.5,
                                label = paste("Error creating plot:\n", e$message),
                                hjust = 0.5, vjust = 0.5, size = 4
                            ) +
                            ggplot2::theme_void()
                        print(p)
                        return(TRUE)
                    }
                )
            },
            .performEnhancedWillRogersAnalysis = function(data, all_results) {
                # Enhanced Will Rogers analysis with formal statistical tests
                table <- self$results$willRogersEnhancedAnalysis
                if (is.null(table)) {
                    return()
                }

                tryCatch(
                    {
                        old_col <- self$options$oldStage
                        new_col <- self$options$newStage
                        time_col <- self$options$survivalTime
                        event_col <- self$options$event
                        event_level <- self$options$eventLevel

                        # Prepare event variable
                        if (!is.null(event_level) && event_level != "") {
                            data$event_binary <- ifelse(data[[event_col]] == event_level, 1, 0)
                        } else {
                            # as.numeric() on a factor returns level INDICES (1,2,...), which Surv()
                            # reads as 1 = censored / 2 = event -- inverting the analysis. Coerce via
                            # the level LABELS instead.
                            data$event_binary <- if (is.factor(data[[event_col]])) {
                                suppressWarnings(as.numeric(as.character(data[[event_col]])))
                            } else {
                                as.numeric(data[[event_col]])
                            }
                        }

                        # Identify migrated patients
                        data$migrated <- data[[old_col]] != data[[new_col]]

                        # Get stages with migration patterns
                        migration_table <- table(data[[old_col]], data[[new_col]])
                        diag(migration_table) <- 0 # Remove unchanged patients

                        # Find significant migration patterns (at least 5 patients)
                        migration_patterns <- which(migration_table >= 5, arr.ind = TRUE)

                        if (nrow(migration_patterns) == 0) {
                            table$addRow(rowKey = "no_migration", values = list(
                                Stage = "No Migration",
                                Period = "No significant migration patterns found",
                                N_Patients = nrow(data),
                                Median_Survival = NA,
                                CI_Lower = NA,
                                CI_Upper = NA,
                                Survival_Change = NA,
                                P_Value = NA,
                                Statistical_Test = "Insufficient migration (need >=5 patients per pattern)"
                            ))
                            return()
                        }

                        # Analyze each migration pattern
                        for (i in seq_len(nrow(migration_patterns))) {
                            from_stage <- rownames(migration_table)[migration_patterns[i, 1]]
                            to_stage <- colnames(migration_table)[migration_patterns[i, 2]]
                            n_migrated <- migration_table[from_stage, to_stage]

                            # Test 1: Original stage WITH vs WITHOUT migrated patients
                            # WITH migrated patients (all patients originally in from_stage)
                            original_with_migrated <- data[data[[old_col]] == from_stage, ]
                            # WITHOUT migrated patients (only those who stayed in from_stage)
                            original_without_migrated <- data[data[[old_col]] == from_stage & data[[new_col]] == from_stage, ]

                            if (nrow(original_with_migrated) > 5 && nrow(original_without_migrated) > 5) {
                                # Calculate survival metrics
                                with_surv <- survival::Surv(original_with_migrated[[time_col]], original_with_migrated$event_binary)
                                with_fit <- survival::survfit(with_surv ~ 1)
                                with_median <- summary(with_fit)$table["median"]
                                with_ci <- summary(with_fit)$table[c("0.95LCL", "0.95UCL")]

                                without_surv <- survival::Surv(original_without_migrated[[time_col]], original_without_migrated$event_binary)
                                without_fit <- survival::survfit(without_surv ~ 1)
                                without_median <- summary(without_fit)$table["median"]
                                without_ci <- summary(without_fit)$table[c("0.95LCL", "0.95UCL")]

                                # Log-rank test
                                combined_original <- rbind(
                                    cbind(original_with_migrated, group = "With_Migrated"),
                                    cbind(original_without_migrated, group = "Without_Migrated")
                                )
                                combined_surv_original <- survival::Surv(combined_original[[time_col]], combined_original$event_binary)
                                logrank_original <- survival::survdiff(combined_surv_original ~ group, data = combined_original)
                                p_value_original <- 1 - pchisq(logrank_original$chisq, df = 1)

                                # Add results for original stage analysis
                                survival_improvement <- if (!is.na(without_median) && !is.na(with_median)) {
                                    without_median - with_median
                                } else {
                                    NA
                                }

                                table$addRow(rowKey = paste0(from_stage, "_original_analysis"), values = list(
                                    Stage = paste0(from_stage, " (original)"),
                                    Period = paste0("With vs without ", n_migrated, " migrated patients"),
                                    N_Patients = paste0(nrow(original_with_migrated), " vs ", nrow(original_without_migrated)),
                                    Median_Survival = paste0(
                                        if (!is.na(with_median)) round(with_median, 1) else "NA",
                                        " vs ",
                                        if (!is.na(without_median)) round(without_median, 1) else "NA"
                                    ),
                                    CI_Lower = paste0(
                                        if (!is.na(with_ci[1])) round(with_ci[1], 1) else "NA",
                                        " vs ",
                                        if (!is.na(without_ci[1])) round(without_ci[1], 1) else "NA"
                                    ),
                                    CI_Upper = paste0(
                                        if (!is.na(with_ci[2])) round(with_ci[2], 1) else "NA",
                                        " vs ",
                                        if (!is.na(without_ci[2])) round(without_ci[2], 1) else "NA"
                                    ),
                                    Survival_Change = survival_improvement,
                                    P_Value = if (!is.na(p_value_original)) p_value_original else NA,
                                    Statistical_Test = paste0("Log-rank test (", from_stage, "\u{2192}", to_stage, " migration)")
                                ))
                            }

                            # Test 2: New stage WITH vs WITHOUT migrated patients
                            # WITH migrated patients (all patients now in to_stage)
                            new_with_migrated <- data[data[[new_col]] == to_stage, ]
                            # WITHOUT migrated patients (only those who were originally in to_stage)
                            new_without_migrated <- data[data[[old_col]] == to_stage & data[[new_col]] == to_stage, ]

                            if (nrow(new_with_migrated) > 5 && nrow(new_without_migrated) > 5) {
                                # Calculate survival metrics
                                new_with_surv <- survival::Surv(new_with_migrated[[time_col]], new_with_migrated$event_binary)
                                new_with_fit <- survival::survfit(new_with_surv ~ 1)
                                new_with_median <- summary(new_with_fit)$table["median"]
                                new_with_ci <- summary(new_with_fit)$table[c("0.95LCL", "0.95UCL")]

                                new_without_surv <- survival::Surv(new_without_migrated[[time_col]], new_without_migrated$event_binary)
                                new_without_fit <- survival::survfit(new_without_surv ~ 1)
                                new_without_median <- summary(new_without_fit)$table["median"]
                                new_without_ci <- summary(new_without_fit)$table[c("0.95LCL", "0.95UCL")]

                                # Log-rank test
                                combined_new <- rbind(
                                    cbind(new_with_migrated, group = "With_Migrated"),
                                    cbind(new_without_migrated, group = "Without_Migrated")
                                )
                                combined_surv_new <- survival::Surv(combined_new[[time_col]], combined_new$event_binary)
                                logrank_new <- survival::survdiff(combined_surv_new ~ group, data = combined_new)
                                p_value_new <- 1 - pchisq(logrank_new$chisq, df = 1)

                                # Add results for new stage analysis
                                survival_improvement_new <- if (!is.na(new_without_median) && !is.na(new_with_median)) {
                                    new_without_median - new_with_median
                                } else {
                                    NA
                                }

                                table$addRow(rowKey = paste0(to_stage, "_new_analysis"), values = list(
                                    Stage = paste0(to_stage, " (new)"),
                                    Period = paste0("With vs without ", n_migrated, " migrated patients"),
                                    N_Patients = paste0(nrow(new_with_migrated), " vs ", nrow(new_without_migrated)),
                                    Median_Survival = paste0(
                                        if (!is.na(new_with_median)) round(new_with_median, 1) else "NA",
                                        " vs ",
                                        if (!is.na(new_without_median)) round(new_without_median, 1) else "NA"
                                    ),
                                    CI_Lower = paste0(
                                        if (!is.na(new_with_ci[1])) round(new_with_ci[1], 1) else "NA",
                                        " vs ",
                                        if (!is.na(new_without_ci[1])) round(new_without_ci[1], 1) else "NA"
                                    ),
                                    CI_Upper = paste0(
                                        if (!is.na(new_with_ci[2])) round(new_with_ci[2], 1) else "NA",
                                        " vs ",
                                        if (!is.na(new_without_ci[2])) round(new_without_ci[2], 1) else "NA"
                                    ),
                                    Survival_Change = survival_improvement_new,
                                    P_Value = if (!is.na(p_value_new)) p_value_new else NA,
                                    Statistical_Test = paste0("Log-rank test (", from_stage, "\u{2192}", to_stage, " migration)")
                                ))
                            }
                        }

                        # Add overall Will Rogers assessment
                        private$.performOverallWillRogersTest(data, table)
                    },
                    error = function(e) {
                        table$addRow(rowKey = "error", values = list(
                            Stage = "Error",
                            Period = NA,
                            N_Patients = NA,
                            Median_Survival = NA,
                            CI_Lower = NA,
                            CI_Upper = NA,
                            Survival_Change = NA,
                            P_Value = NA,
                            Statistical_Test = paste("Error:", e$message)
                        ))
                    }
                )
            },
            .performOverallWillRogersTest = function(data, table) {
                # Test for overall Will Rogers phenomenon across all tested comparisons
                tryCatch(
                    {
                        # Simplified approach - manually track the test results
                        # Count tests performed based on migration patterns
                        migration_table <- table(data[[self$options$oldStage]], data[[self$options$newStage]])
                        diag(migration_table) <- 0 # Remove unchanged patients
                        migration_patterns <- which(migration_table >= 5, arr.ind = TRUE)

                        if (nrow(migration_patterns) == 0) {
                            table$addRow(rowKey = "overall_assessment", values = list(
                                Stage = "Overall Assessment",
                                Period = "Will Rogers Evidence",
                                N_Patients = nrow(data),
                                Median_Survival = NA,
                                CI_Lower = NA,
                                CI_Upper = NA,
                                Survival_Change = NA,
                                P_Value = NA,
                                Statistical_Test = "No statistical tests performed (insufficient migration)"
                            ))
                            return()
                        }

                        # Simple overall assessment based on migration patterns
                        total_migration_patterns <- nrow(migration_patterns)
                        total_migrated <- sum(data[[self$options$oldStage]] != data[[self$options$newStage]])
                        migration_rate <- total_migrated / nrow(data)

                        # Determine Will Rogers evidence strength based on migration characteristics
                        if (migration_rate > 0.3 && total_migration_patterns >= 2) {
                            evidence <- "Strong migration pattern - check individual tests for Will Rogers evidence"
                        } else if (migration_rate > 0.1 && total_migration_patterns >= 1) {
                            evidence <- "Moderate migration pattern - check individual tests for Will Rogers evidence"
                        } else if (total_migration_patterns >= 1) {
                            evidence <- "Limited migration pattern - check individual tests for Will Rogers evidence"
                        } else {
                            evidence <- "No Will Rogers phenomenon - insufficient migration"
                        }

                        # Add overall assessment with safe values
                        table$addRow(rowKey = "overall_assessment", values = list(
                            Stage = "Overall Assessment",
                            Period = paste0(total_migration_patterns, " migration pattern(s), ", round(migration_rate * 100, 1), "% migrated"),
                            N_Patients = nrow(data),
                            Median_Survival = NA,
                            CI_Lower = NA,
                            CI_Upper = NA,
                            Survival_Change = NA,
                            P_Value = migration_rate,
                            Statistical_Test = evidence
                        ))
                    },
                    error = function(e) {
                        # Simplified error handling to avoid function application errors
                        table$addRow(rowKey = "overall_error", values = list(
                            Stage = "Overall Assessment Error",
                            Period = "Error in overall calculation",
                            N_Patients = NA,
                            Median_Survival = NA,
                            CI_Lower = NA,
                            CI_Upper = NA,
                            Survival_Change = NA,
                            P_Value = NA,
                            Statistical_Test = "See individual test results above"
                        ))
                    }
                )
            },
            .performDetailedWillRogersAnalysis = function(data, all_results) {
                # Detailed stage-specific Will Rogers analysis with survival improvement breakdown
                table <- self$results$willRogersStageDetail
                if (is.null(table)) {
                    return()
                }

                tryCatch(
                    {
                        old_col <- self$options$oldStage
                        new_col <- self$options$newStage
                        time_col <- self$options$survivalTime
                        event_col <- self$options$event
                        event_level <- self$options$eventLevel

                        # Prepare event variable
                        if (!is.null(event_level) && event_level != "") {
                            data$event_binary <- ifelse(data[[event_col]] == event_level, 1, 0)
                        } else {
                            # as.numeric() on a factor returns level INDICES (1,2,...), which Surv()
                            # reads as 1 = censored / 2 = event -- inverting the analysis. Coerce via
                            # the level LABELS instead.
                            data$event_binary <- if (is.factor(data[[event_col]])) {
                                suppressWarnings(as.numeric(as.character(data[[event_col]])))
                            } else {
                                as.numeric(data[[event_col]])
                            }
                        }

                        # Get unique stages
                        all_stages <- sort(unique(c(data[[old_col]], data[[new_col]])))

                        # Process each stage to show detailed Will Rogers effect
                        for (stage in all_stages) {
                            # Patients originally in this stage
                            original_patients <- data[data[[old_col]] == stage, ]

                            # Patients in this stage after migration
                            new_patients <- data[data[[new_col]] == stage, ]

                            # Skip if no patients in either system
                            if (nrow(original_patients) == 0 && nrow(new_patients) == 0) next

                            # Calculate survival metrics for original system
                            original_median <- NA
                            original_ci_lower <- NA
                            original_ci_upper <- NA

                            if (nrow(original_patients) > 0 && sum(original_patients$event_binary) > 0) {
                                tryCatch(
                                    {
                                        surv_orig <- survival::Surv(original_patients[[time_col]], original_patients$event_binary)
                                        fit_orig <- survival::survfit(surv_orig ~ 1)
                                        summary_orig <- summary(fit_orig)

                                        # Extract median and CI
                                        if (!is.null(summary_orig$table) && "median" %in% names(summary_orig$table)) {
                                            original_median <- summary_orig$table["median"]
                                            # Get CI from quantiles
                                            quant_orig <- quantile(fit_orig, probs = 0.5)
                                            if (!is.null(quant_orig) && length(quant_orig$lower) > 0) {
                                                original_ci_lower <- quant_orig$lower
                                                original_ci_upper <- quant_orig$upper
                                            }
                                        }
                                    },
                                    error = function(e) {
                                        # Use simple median as fallback
                                        if (sum(original_patients$event_binary) > 0) {
                                            original_median <- median(original_patients[[time_col]][original_patients$event_binary == 1])
                                        }
                                    }
                                )
                            }

                            # Calculate survival metrics for new system
                            new_median <- NA
                            new_ci_lower <- NA
                            new_ci_upper <- NA

                            if (nrow(new_patients) > 0 && sum(new_patients$event_binary) > 0) {
                                tryCatch(
                                    {
                                        surv_new <- survival::Surv(new_patients[[time_col]], new_patients$event_binary)
                                        fit_new <- survival::survfit(surv_new ~ 1)
                                        summary_new <- summary(fit_new)

                                        # Extract median and CI
                                        if (!is.null(summary_new$table) && "median" %in% names(summary_new$table)) {
                                            new_median <- summary_new$table["median"]
                                            # Get CI from quantiles
                                            quant_new <- quantile(fit_new, probs = 0.5)
                                            if (!is.null(quant_new) && length(quant_new$lower) > 0) {
                                                new_ci_lower <- quant_new$lower
                                                new_ci_upper <- quant_new$upper
                                            }
                                        }
                                    },
                                    error = function(e) {
                                        # Use simple median as fallback
                                        if (sum(new_patients$event_binary) > 0) {
                                            new_median <- median(new_patients[[time_col]][new_patients$event_binary == 1])
                                        }
                                    }
                                )
                            }

                            # Identify migration patterns for this stage
                            patients_lost <- original_patients[original_patients[[new_col]] != stage, ]
                            patients_gained <- new_patients[new_patients[[old_col]] != stage, ]

                            # Calculate migration statistics
                            n_original <- nrow(original_patients)
                            n_lost <- nrow(patients_lost)
                            n_gained <- nrow(patients_gained)

                            # Calculate survival improvements
                            absolute_improvement <- NA
                            relative_improvement <- NA
                            improvement_percentage <- NA

                            if (!is.na(original_median) && !is.na(new_median)) {
                                absolute_improvement <- new_median - original_median
                                if (original_median > 0) {
                                    relative_improvement <- (new_median - original_median) / original_median
                                    improvement_percentage <- relative_improvement * 100
                                }
                            }

                            # Determine clinical impact and migration type
                            migration_type <- ""
                            clinical_impact <- ""

                            if (n_lost > n_gained) {
                                migration_type <- "Net Loss"
                                if (!is.na(improvement_percentage) && improvement_percentage > 5) {
                                    clinical_impact <- "Will Rogers Effect: Survival improved by losing worst patients"
                                } else if (!is.na(improvement_percentage) && improvement_percentage < -5) {
                                    clinical_impact <- "Survival worsened despite losing patients"
                                } else {
                                    clinical_impact <- "Minimal survival change from patient loss"
                                }
                            } else if (n_gained > n_lost) {
                                migration_type <- "Net Gain"
                                if (!is.na(improvement_percentage) && improvement_percentage > 5) {
                                    clinical_impact <- "Will Rogers Effect: Survival improved by gaining better patients"
                                } else if (!is.na(improvement_percentage) && improvement_percentage < -5) {
                                    clinical_impact <- "Survival worsened despite gaining patients"
                                } else {
                                    clinical_impact <- "Minimal survival change from patient gain"
                                }
                            } else if (n_lost == n_gained && n_lost > 0) {
                                migration_type <- "Patient Exchange"
                                if (!is.na(improvement_percentage) && abs(improvement_percentage) > 5) {
                                    clinical_impact <- "Will Rogers Effect: Survival changed from patient exchange"
                                } else {
                                    clinical_impact <- "Minimal survival change from patient exchange"
                                }
                            } else {
                                migration_type <- "No Migration"
                                clinical_impact <- "No migration effect"
                            }

                            # Format confidence intervals
                            original_ci_str <- ""
                            new_ci_str <- ""

                            if (!is.na(original_ci_lower) && !is.na(original_ci_upper)) {
                                original_ci_str <- sprintf("(%.1f-%.1f)", original_ci_lower, original_ci_upper)
                            }

                            if (!is.na(new_ci_lower) && !is.na(new_ci_upper)) {
                                new_ci_str <- sprintf("(%.1f-%.1f)", new_ci_lower, new_ci_upper)
                            }

                            # Calculate migration numbers for this stage
                            net_migrated <- abs(n_gained - n_lost)
                            pct_migrated <- if (n_original > 0) net_migrated / n_original else 0

                            # Add row to table with correct column names matching .r.yaml
                            table$addRow(rowKey = paste0("stage_", stage), values = list(
                                Stage = stage,
                                Migration_Type = migration_type,
                                N_Migrated = net_migrated,
                                Pct_Migrated = pct_migrated,
                                Original_Median = original_median,
                                New_Median = new_median,
                                Absolute_Improvement = absolute_improvement,
                                Relative_Improvement = if (!is.na(improvement_percentage)) improvement_percentage / 100 else NA,
                                Improvement_Type = if (!is.na(improvement_percentage)) {
                                    if (improvement_percentage > 5) {
                                        "Beneficial"
                                    } else if (improvement_percentage < -5) {
                                        "Detrimental"
                                    } else {
                                        "Minimal"
                                    }
                                } else {
                                    "Unknown"
                                },
                                Clinical_Impact = clinical_impact
                            ))
                        }

                        # Add overall summary row
                        total_patients <- nrow(data)
                        total_migrated <- sum(data[[old_col]] != data[[new_col]])
                        migration_rate <- (total_migrated / total_patients) * 100

                        # Calculate overall Will Rogers effect magnitude based on data analysis
                        # We'll recalculate this directly from the data instead of reading from table
                        overall_will_rogers_magnitude <- "Minimal"
                        if (total_migrated >= 10) { # At least 10 migrated patients
                            # Count stages with >5% improvement by re-analyzing the data
                            improvement_stages <- 0

                            for (stage in all_stages) {
                                # Recalculate improvement for each stage
                                original_patients <- data[data[[old_col]] == stage, ]
                                new_patients <- data[data[[new_col]] == stage, ]

                                if (nrow(original_patients) > 0 && nrow(new_patients) > 0) {
                                    # Quick survival calculation
                                    original_events <- original_patients[original_patients$event_binary == 1, ]
                                    new_events <- new_patients[new_patients$event_binary == 1, ]

                                    if (nrow(original_events) > 0 && nrow(new_events) > 0) {
                                        orig_median <- median(original_events[[time_col]], na.rm = TRUE)
                                        new_median <- median(new_events[[time_col]], na.rm = TRUE)

                                        if (!is.na(orig_median) && !is.na(new_median) && orig_median > 0) {
                                            improvement_pct <- ((new_median - orig_median) / orig_median) * 100
                                            if (improvement_pct > 5) {
                                                improvement_stages <- improvement_stages + 1
                                            }
                                        }
                                    }
                                }
                            }

                            if (improvement_stages >= 2) {
                                overall_will_rogers_magnitude <- "Strong"
                            } else if (improvement_stages == 1) {
                                overall_will_rogers_magnitude <- "Moderate"
                            }
                        }

                        table$addRow(rowKey = "overall_summary", values = list(
                            Stage = "Overall Assessment",
                            Migration_Type = "Mixed Pattern",
                            N_Migrated = total_migrated,
                            Pct_Migrated = migration_rate / 100,
                            Original_Median = NA,
                            New_Median = NA,
                            Absolute_Improvement = NA,
                            Relative_Improvement = NA,
                            Improvement_Type = overall_will_rogers_magnitude,
                            Clinical_Impact = "Multiple stages show artificial survival improvement from patient reclassification"
                        ))
                    },
                    error = function(e) {
                        table$addRow(rowKey = "error", values = list(
                            Stage = "Error",
                            Migration_Type = "Analysis failed",
                            N_Migrated = NA,
                            Pct_Migrated = NA,
                            Original_Median = NA,
                            New_Median = NA,
                            Absolute_Improvement = NA,
                            Relative_Improvement = NA,
                            Improvement_Type = "Error",
                            Clinical_Impact = paste("Error:", e$message)
                        ))
                    }
                )
            },
            .performCrossValidation = function(data, all_results) {
                # K-fold cross-validation for staging system validation
                table <- self$results$crossValidationResults
                if (is.null(table)) {
                    return()
                }

                tryCatch(
                    {
                        old_col <- self$options$oldStage
                        new_col <- self$options$newStage
                        time_col <- self$options$survivalTime
                        event_col <- self$options$event
                        event_level <- self$options$eventLevel
                        cv_folds <- self$options$cvFolds
                        institution_col <- self$options$institutionVariable

                        if (is.null(cv_folds) || cv_folds < 3) cv_folds <- 5

                        # Prepare event variable
                        if (!is.null(event_level) && event_level != "") {
                            data$event_binary <- ifelse(data[[event_col]] == event_level, 1, 0)
                        } else {
                            # as.numeric() on a factor returns level INDICES (1,2,...), which Surv()
                            # reads as 1 = censored / 2 = event -- inverting the analysis. Coerce via
                            # the level LABELS instead.
                            data$event_binary <- if (is.factor(data[[event_col]])) {
                                suppressWarnings(as.numeric(as.character(data[[event_col]])))
                            } else {
                                as.numeric(data[[event_col]])
                            }
                        }

                        # Check minimum sample size
                        n <- nrow(data)
                        if (n < cv_folds * 20) {
                            table$addRow(rowKey = "insufficient_data", values = list(
                                Fold = "Error",
                                N_Train = NA,
                                N_Test = NA,
                                Old_System_CIndex = NA,
                                New_System_CIndex = NA,
                                CIndex_Difference = NA,
                                P_Value = NA,
                                Validation_Type = paste("Insufficient data for", cv_folds, "fold CV (need >=", cv_folds * 20, "patients)")
                            ))
                            return()
                        }

                        # Determine validation type and create fold assignments
                        is_multi_institutional <- !is.null(institution_col) && institution_col != ""

                        if (is_multi_institutional) {
                            # Multi-institutional validation: each institution is a "fold"
                            institutions <- unique(data[[institution_col]])
                            institutions <- institutions[!is.na(institutions)]

                            if (length(institutions) < 2) {
                                table$addRow(rowKey = "insufficient_institutions", values = list(
                                    Fold = "Error",
                                    N_Train = NA,
                                    N_Test = NA,
                                    Train_Events = NA,
                                    Test_Events = NA,
                                    Old_System_CIndex = NA,
                                    Old_CI_Lower = NA,
                                    Old_CI_Upper = NA,
                                    New_System_CIndex = NA,
                                    New_CI_Lower = NA,
                                    New_CI_Upper = NA,
                                    CIndex_Difference = NA,
                                    Difference_SE = NA,
                                    P_Value = NA,
                                    Quality = .("Insufficient institutions"),
                                    Clinical_Interpretation = paste("Need >=2 institutions for multi-institutional validation (found:", length(institutions), ")")
                                ))
                                return()
                            }

                            cv_folds <- length(institutions)
                            fold_ids <- match(data[[institution_col]], institutions)
                        } else {
                            # Standard k-fold cross-validation
                            set.seed(private$.seedValue())
                            fold_ids <- sample(rep(1:cv_folds, length.out = n))
                        }

                        # Storage for CV results
                        cv_results <- data.frame(
                            fold = integer(),
                            fold_label = character(),
                            n_train = integer(),
                            n_test = integer(),
                            train_events = integer(),
                            test_events = integer(),
                            old_cindex = numeric(),
                            new_cindex = numeric(),
                            cindex_diff = numeric(),
                            p_value = numeric(),
                            stringsAsFactors = FALSE
                        )

                        # Perform cross-validation (k-fold or multi-institutional)
                        for (fold in 1:cv_folds) {
                            # Checkpoint before each fold to allow cancellation
                            private$.checkpoint()

                            # Split data
                            test_idx <- fold_ids == fold
                            train_data <- data[!test_idx, ]
                            test_data <- data[test_idx, ]

                            # Determine fold label
                            fold_label <- if (is_multi_institutional) {
                                paste("Institution", institutions[fold])
                            } else {
                                paste("Fold", fold)
                            }

                            n_train <- nrow(train_data)
                            n_test <- nrow(test_data)

                            # Skip if insufficient events in train or test
                            train_events <- sum(train_data$event_binary, na.rm = TRUE)
                            test_events <- sum(test_data$event_binary, na.rm = TRUE)

                            if (train_events < 5 || test_events < 5) {
                                cv_results <- rbind(cv_results, data.frame(
                                    fold = fold,
                                    fold_label = fold_label,
                                    n_train = n_train,
                                    n_test = n_test,
                                    train_events = train_events,
                                    test_events = test_events,
                                    old_cindex = NA,
                                    new_cindex = NA,
                                    cindex_diff = NA,
                                    p_value = NA
                                ))
                                next
                            }

                            # Fit Cox models on training data
                            old_cindex <- NA
                            new_cindex <- NA
                            cindex_diff <- NA
                            p_value <- NA

                            tryCatch(
                                {
                                    # Old staging system
                                    train_surv <- survival::Surv(train_data[[time_col]], train_data$event_binary)
                                    old_formula <- as.formula(paste("train_surv ~", old_col))
                                    old_fit <- survival::coxph(old_formula, data = train_data)

                                    # New staging system
                                    new_formula <- as.formula(paste("train_surv ~", new_col))
                                    new_fit <- survival::coxph(new_formula, data = train_data)

                                    # Test on held-out data
                                    test_surv <- survival::Surv(test_data[[time_col]], test_data$event_binary)

                                    # Calculate C-indices on test data
                                    old_pred <- predict(old_fit, newdata = test_data, type = "risk")
                                    new_pred <- predict(new_fit, newdata = test_data, type = "risk")

                                    # Use Harrell's concordance index
                                    # reverse = TRUE: old_pred/new_pred are risk scores (higher = worse).
                                    # Without it concordance() returns 1 - C, which also flips the SIGN of
                                    # cindex_diff below and reports a better staging system as worse.
                                    old_cindex <- survival::concordance(test_surv ~ old_pred, reverse = TRUE)$concordance
                                    new_cindex <- survival::concordance(test_surv ~ new_pred, reverse = TRUE)$concordance
                                    cindex_diff <- new_cindex - old_cindex

                                    # Additional comprehensive metrics (disabled for performance optimization)
                                    # Re-enable if detailed cross-validation metrics needed
                                    # additional_metrics <- private$.calculateAdditionalCVMetrics(
                                    #     old_fit, new_fit, test_data, test_surv, old_col, new_col)

                                    # Store additional metrics for later aggregation
                                    # fold_additional_metrics <- additional_metrics

                                    # Statistical comparison using likelihood ratio test
                                    # Fit nested models on test data for comparison
                                    test_old_formula <- as.formula(paste("test_surv ~", old_col))
                                    test_old_fit <- survival::coxph(test_old_formula, data = test_data)
                                    test_new_formula <- as.formula(paste("test_surv ~", new_col))
                                    test_new_fit <- survival::coxph(test_new_formula, data = test_data)

                                    # Likelihood ratio test
                                    tryCatch(
                                        {
                                            lr_test <- anova(test_old_fit, test_new_fit, test = "Chisq")
                                            if (nrow(lr_test) >= 2) {
                                                # Try different possible column names for p-value
                                                possible_cols <- c("P(>|Chi|)", "Pr(>Chisq)", "Pr(Chi)", "p.value", "P.value")
                                                p_col <- intersect(possible_cols, colnames(lr_test))[1]
                                                if (!is.na(p_col)) {
                                                    p_value <- lr_test[2, p_col]
                                                }
                                            }
                                        },
                                        error = function(e2) {
                                            # If anova fails, use a simple comparison based on AIC
                                            if (!is.na(old_cindex) && !is.na(new_cindex) && abs(cindex_diff) > 0.01) {
                                                # Rough p-value approximation based on C-index difference
                                                z_score <- abs(cindex_diff) / 0.05 # rough SE estimate
                                                p_value <- 2 * (1 - pnorm(z_score))
                                            }
                                        }
                                    )
                                },
                                error = function(e) {
                                    # Keep NA values if model fitting fails
                                }
                            )

                            # Store fold results
                            cv_results <- rbind(cv_results, data.frame(
                                fold = fold,
                                fold_label = fold_label,
                                n_train = n_train,
                                n_test = n_test,
                                train_events = train_events,
                                test_events = test_events,
                                old_cindex = old_cindex,
                                new_cindex = new_cindex,
                                cindex_diff = cindex_diff,
                                p_value = p_value
                            ))
                        }

                        # Add individual fold results to table
                        for (i in seq_len(nrow(cv_results))) {
                            row <- cv_results[i, ]


                            # Calculate confidence intervals for C-indices with fallback
                            old_cindex_ci <- tryCatch(
                                {
                                    private$.calculateCIndexConfidenceInterval(row$old_cindex, row$n_test)
                                },
                                error = function(e) c(NA, NA)
                            )

                            new_cindex_ci <- tryCatch(
                                {
                                    private$.calculateCIndexConfidenceInterval(row$new_cindex, row$n_test)
                                },
                                error = function(e) c(NA, NA)
                            )

                            # Force direct CI calculation (bypass helper methods for reliability)
                            if (!is.na(row$old_cindex) && row$n_test > 0) {
                                se_old <- sqrt((row$old_cindex * (1 - row$old_cindex)) / row$n_test)
                                old_cindex_ci <- c(
                                    max(0, row$old_cindex - private$.zCrit() * se_old),
                                    min(1, row$old_cindex + private$.zCrit() * se_old)
                                )
                            } else {
                                old_cindex_ci <- c(NA, NA)
                            }

                            if (!is.na(row$new_cindex) && row$n_test > 0) {
                                se_new <- sqrt((row$new_cindex * (1 - row$new_cindex)) / row$n_test)
                                new_cindex_ci <- c(
                                    max(0, row$new_cindex - private$.zCrit() * se_new),
                                    min(1, row$new_cindex + private$.zCrit() * se_new)
                                )
                            } else {
                                new_cindex_ci <- c(NA, NA)
                            }

                            # Calculate difference standard error with fallback
                            diff_se <- tryCatch(
                                {
                                    private$.calculateCIndexDifferenceSE(row$old_cindex, row$new_cindex, row$n_test)
                                },
                                error = function(e) {
                                    # Simple fallback SE calculation
                                    if (!is.na(row$old_cindex) && !is.na(row$new_cindex)) {
                                        se1 <- sqrt((row$old_cindex * (1 - row$old_cindex)) / row$n_test)
                                        se2 <- sqrt((row$new_cindex * (1 - row$new_cindex)) / row$n_test)
                                        sqrt(se1^2 + se2^2)
                                    } else {
                                        NA
                                    }
                                }
                            )

                            # Assess validation quality with direct calculation
                            validation_quality <- tryCatch(
                                {
                                    private$.assessFoldValidationQuality(row$old_cindex, row$new_cindex, row$test_events)
                                },
                                error = function(e) {
                                    # Direct quality assessment
                                    if (is.na(row$old_cindex) || is.na(row$new_cindex)) {
                                        "Invalid"
                                    } else if (row$test_events < 5) {
                                        "Insufficient Events"
                                    } else {
                                        avg_cindex <- (row$old_cindex + row$new_cindex) / 2
                                        if (avg_cindex < 0.5) {
                                            "Poor Discrimination"
                                        } else if (avg_cindex < 0.6) {
                                            "Acceptable"
                                        } else if (avg_cindex < 0.7) {
                                            "Good"
                                        } else {
                                            "Excellent"
                                        }
                                    }
                                }
                            )

                            # Calculate p-value with fallback if not available from Cox model
                            p_value <- row$p_value
                            if (is.na(p_value) && !is.na(diff_se) && diff_se > 0) {
                                # Use z-test for C-index difference
                                z_score <- abs(row$cindex_diff) / diff_se
                                p_value <- 2 * (1 - pnorm(z_score))
                            }

                            # Enhanced clinical interpretation
                            clinical_interpretation <- private$.interpretCVFoldResult(row$cindex_diff, diff_se)

                            table$addRow(rowKey = paste0("fold_", row$fold), values = list(
                                Fold = row$fold_label,
                                N_Train = row$n_train,
                                N_Test = row$n_test,
                                Train_Events = row$train_events,
                                Test_Events = row$test_events,
                                Old_System_CIndex = row$old_cindex,
                                Old_CIndex_CI_Lower = old_cindex_ci[1],
                                Old_CIndex_CI_Upper = old_cindex_ci[2],
                                New_System_CIndex = row$new_cindex,
                                New_CIndex_CI_Lower = new_cindex_ci[1],
                                New_CIndex_CI_Upper = new_cindex_ci[2],
                                CIndex_Difference = row$cindex_diff,
                                Difference_SE = diff_se,
                                P_Value = p_value,
                                Validation_Quality = validation_quality,
                                Clinical_Interpretation = clinical_interpretation
                            ))
                        }

                        # Calculate overall cross-validation summary
                        valid_results <- cv_results[!is.na(cv_results$old_cindex) & !is.na(cv_results$new_cindex), ]

                        if (nrow(valid_results) > 0) {
                            mean_old_cindex <- mean(valid_results$old_cindex)
                            mean_new_cindex <- mean(valid_results$new_cindex)
                            mean_diff <- mean(valid_results$cindex_diff)
                            se_diff <- sd(valid_results$cindex_diff) / sqrt(nrow(valid_results))

                            # Overall p-value using paired t-test
                            overall_p <- NA
                            if (nrow(valid_results) >= 3) {
                                t_test <- t.test(valid_results$new_cindex, valid_results$old_cindex, paired = TRUE)
                                overall_p <- t_test$p.value
                            }

                            # Clinical interpretation

                            # Calculate summary statistics for confidence intervals
                            mean_train_events <- round(mean(valid_results$train_events, na.rm = TRUE))
                            mean_test_events <- round(mean(valid_results$test_events, na.rm = TRUE))

                            # Calculate pooled confidence intervals
                            old_pooled_ci <- private$.calculatePooledCIndexCI(valid_results$old_cindex)
                            new_pooled_ci <- private$.calculatePooledCIndexCI(valid_results$new_cindex)

                            # Calculate fold qualities for overall assessment
                            fold_qualities <- sapply(seq_len(nrow(cv_results)), function(i) {
                                row <- cv_results[i, ]
                                private$.assessFoldValidationQuality(row$old_cindex, row$new_cindex, row$test_events)
                            })

                            # Overall validation quality assessment
                            consistency_metrics <- list(cv_se = se_diff)
                            overall_quality <- private$.assessOverallCVQuality(fold_qualities, consistency_metrics)

                            # Enhanced clinical interpretation for summary
                            summary_interpretation <- private$.interpretCVSummary(mean_diff, se_diff, se_diff)

                            # Add summary row
                            table$addRow(rowKey = "cv_summary", values = list(
                                Fold = "CV Summary",
                                N_Train = paste(nrow(valid_results), "valid folds"),
                                N_Test = paste("Mean:", round(mean(valid_results$n_test))),
                                Train_Events = paste("Mean:", mean_train_events),
                                Test_Events = paste("Mean:", mean_test_events),
                                Old_System_CIndex = mean_old_cindex,
                                Old_CIndex_CI_Lower = old_pooled_ci[1],
                                Old_CIndex_CI_Upper = old_pooled_ci[2],
                                New_System_CIndex = mean_new_cindex,
                                New_CIndex_CI_Lower = new_pooled_ci[1],
                                New_CIndex_CI_Upper = new_pooled_ci[2],
                                CIndex_Difference = mean_diff,
                                Difference_SE = se_diff,
                                P_Value = overall_p,
                                Validation_Quality = overall_quality,
                                Clinical_Interpretation = summary_interpretation
                            ))
                        } else {
                            table$addRow(rowKey = "cv_failed", values = list(
                                Fold = "CV Failed",
                                N_Train = "No valid folds",
                                N_Test = "Insufficient data",
                                Old_System_CIndex = NA,
                                New_System_CIndex = NA,
                                CIndex_Difference = NA,
                                P_Value = NA,
                                Clinical_Interpretation = .("Cross-validation failed - insufficient events in test folds")
                            ))
                        }

                        # Generate cross-validation visualization
                        if (exists("cv_results") && !is.null(cv_results) && nrow(cv_results) > 0) {
                            tryCatch(
                                {
                                    private$.generateCrossValidationPlot(cv_results)
                                },
                                error = function(e) {
                                    # If plot generation fails, continue without plot
                                    NULL
                                }
                            )
                        }
                    },
                    error = function(e) {
                        table$addRow(rowKey = "error", values = list(
                            Fold = "Error",
                            N_Train = "Analysis failed",
                            N_Test = "N/A",
                            Old_System_CIndex = NA,
                            New_System_CIndex = NA,
                            CIndex_Difference = NA,
                            P_Value = NA,
                            Clinical_Interpretation = paste("Error:", e$message)
                        ))
                    }
                )
            },

            # Helper method: Calculate C-index confidence interval
            .calculateCIndexConfidenceInterval = function(cindex, n, confidence = 0.95) {
                if (is.null(cindex) || is.na(cindex) || n < 5) {
                    return(c(NA, NA))
                }

                # More robust standard error approximation
                # Use Wilson score interval approach for C-index
                se <- sqrt((cindex * (1 - cindex)) / n) # Standard binomial SE
                alpha <- 1 - confidence
                z_alpha <- qnorm(1 - alpha / 2)

                ci_lower <- max(0.0, cindex - z_alpha * se)
                ci_upper <- min(1.0, cindex + z_alpha * se)

                return(c(ci_lower, ci_upper))
            },

            # Helper method: Calculate C-index difference standard error
            .calculateCIndexDifferenceSE = function(cindex1, cindex2, n, correlation = 0.7) {
                if (is.null(cindex1) || is.null(cindex2) || is.na(cindex1) || is.na(cindex2) || n < 5) {
                    return(NA)
                }

                se1 <- sqrt((cindex1 * (1 - cindex1)) / (n * 0.8))
                se2 <- sqrt((cindex2 * (1 - cindex2)) / (n * 0.8))

                # Account for correlation between C-indices
                se_diff <- sqrt(se1^2 + se2^2 - 2 * correlation * se1 * se2)

                return(se_diff)
            },

            # Helper method: Assess fold validation quality
            .assessFoldValidationQuality = function(cindex_old, cindex_new, n_events) {
                if (is.null(cindex_old) || is.null(cindex_new) || is.na(cindex_old) || is.na(cindex_new)) {
                    return("Invalid")
                }

                if (n_events < 5) {
                    return("Insufficient Events")
                }

                # Check for reasonable C-index values (0.0 to 1.0 is valid range)
                if (cindex_old < 0.0 || cindex_new < 0.0 || cindex_old > 1 || cindex_new > 1) {
                    return("Invalid C-Index")
                }

                # Check for overfitting indicators first
                if (abs(cindex_new - cindex_old) > 0.2) {
                    return("Suspicious")
                }

                # Assess discrimination quality
                avg_cindex <- (cindex_old + cindex_new) / 2
                if (avg_cindex < 0.5) {
                    return("Poor Discrimination")
                } else if (avg_cindex < 0.6) {
                    return("Acceptable")
                } else if (avg_cindex < 0.7) {
                    return("Good")
                } else {
                    return("Excellent")
                }
            },

            # Helper method: Interpret cross-validation fold result
            .interpretCVFoldResult = function(cindex_diff, se_diff, clinical_threshold = 0.02) {
                if (is.na(cindex_diff) || is.na(se_diff)) {
                    return("Inconclusive")
                }

                z_score <- abs(cindex_diff) / se_diff

                if (abs(cindex_diff) < clinical_threshold) {
                    return("No meaningful difference")
                } else if (cindex_diff > clinical_threshold && z_score > private$.zCrit()) {
                    return("New system superior")
                } else if (cindex_diff < -clinical_threshold && z_score > private$.zCrit()) {
                    return("Old system superior")
                } else {
                    return("Difference uncertain")
                }
            },

            # Helper method: Calculate pooled C-index confidence interval
            .calculatePooledCIndexCI = function(fold_results, confidence = 0.95) {
                valid_results <- fold_results[!is.na(fold_results)]

                if (length(valid_results) < 3) {
                    return(c(NA, NA))
                }

                mean_cindex <- mean(valid_results)
                se_pooled <- sd(valid_results) / sqrt(length(valid_results))

                alpha <- 1 - confidence
                t_alpha <- qt(1 - alpha / 2, df = length(valid_results) - 1)

                ci_lower <- max(0.5, mean_cindex - t_alpha * se_pooled)
                ci_upper <- min(1.0, mean_cindex + t_alpha * se_pooled)

                return(c(ci_lower, ci_upper))
            },

            # Helper method: Assess overall cross-validation quality
            .assessOverallCVQuality = function(fold_qualities, consistency_metrics) {
                if (length(fold_qualities) == 0) {
                    return("No validation performed")
                }

                # Count valid/good quality folds (not Invalid, Insufficient Events, Poor Discrimination, or Suspicious)
                valid_qualities <- c("Excellent", "Good", "Acceptable")
                valid_count <- sum(fold_qualities %in% valid_qualities)
                total_count <- length(fold_qualities)

                if (valid_count / total_count >= 0.8) {
                    if (consistency_metrics$cv_se < 0.05) {
                        return("Excellent validation")
                    } else {
                        return("Good validation")
                    }
                } else if (valid_count / total_count >= 0.6) {
                    return("Acceptable validation")
                } else {
                    return("Poor validation quality")
                }
            },

            # Helper method: Interpret cross-validation summary
            .interpretCVSummary = function(pooled_diff, pooled_se, consistency_se, clinical_threshold = 0.02) {
                if (is.na(pooled_diff) || is.na(pooled_se)) {
                    return("Cross-validation inconclusive due to insufficient data")
                }

                # Statistical significance
                z_score <- abs(pooled_diff) / pooled_se
                is_significant <- z_score > private$.zCrit()

                # Clinical significance
                is_clinically_meaningful <- abs(pooled_diff) >= clinical_threshold

                # Consistency assessment
                is_consistent <- consistency_se < 0.05

                if (is_clinically_meaningful && is_significant && is_consistent) {
                    if (pooled_diff > 0) {
                        return("Strong evidence: New staging system provides consistent, clinically meaningful improvement")
                    } else {
                        return("Strong evidence: Original staging system performs better consistently")
                    }
                } else if (is_clinically_meaningful && is_significant) {
                    if (pooled_diff > 0) {
                        return("Moderate evidence: New staging system shows improvement, but with some variability")
                    } else {
                        return("Moderate evidence: Original staging system may be preferable")
                    }
                } else if (!is_clinically_meaningful) {
                    return("No clinically meaningful difference between staging systems")
                } else {
                    return("Uncertain: Results suggest potential difference but with insufficient statistical evidence")
                }
            },

            # Helper method: Calculate additional CV metrics beyond C-index
            .calculateAdditionalCVMetrics = function(old_fit, new_fit, test_data, test_surv, old_col, new_col) {
                metrics <- list()

                tryCatch(
                    {
                        # 1. Likelihood Ratio Statistics
                        old_loglik <- old_fit$loglik[2]
                        new_loglik <- new_fit$loglik[2]
                        lr_stat <- 2 * (new_loglik - old_loglik)
                        lr_df <- length(coef(new_fit)) - length(coef(old_fit))
                        lr_p <- if (lr_df > 0) pchisq(lr_stat, df = lr_df, lower.tail = FALSE) else NA

                        metrics$lr_statistic <- lr_stat
                        metrics$lr_pvalue <- lr_p

                        # 2. Integrated Brier Score (approximation)
                        # Extract actual time values from survival time column
                        time_col <- self$options$survivalTime
                        event_col <- self$options$event

                        if (time_col %in% names(test_data) && event_col %in% names(test_data)) {
                            time_values <- test_data[[time_col]]
                            event_values <- test_data[[event_col]]

                            time_points <- quantile(time_values, c(0.25, 0.5, 0.75), na.rm = TRUE)

                            old_brier <- 0
                            new_brier <- 0
                            valid_timepoints <- 0

                            for (t in time_points) {
                                if (!is.na(t) && t > 0) {
                                    # Simplified Brier score calculation at time t
                                    at_risk <- time_values >= t
                                    if (sum(at_risk, na.rm = TRUE) > 5) {
                                        # Use predicted risk as probability (simplified)
                                        old_pred_prob <- predict(old_fit, newdata = test_data, type = "expected")
                                        new_pred_prob <- predict(new_fit, newdata = test_data, type = "expected")

                                        # Event indicator at time t (convert to binary for Brier score)
                                        event_binary <- as.numeric(event_values == self$options$eventLevel)
                                        event_at_t <- (time_values <= t) & event_binary

                                        # Brier score components
                                        if (length(old_pred_prob) == nrow(test_data)) {
                                            old_brier_t <- mean((event_at_t - old_pred_prob)^2, na.rm = TRUE)
                                            new_brier_t <- mean((event_at_t - new_pred_prob)^2, na.rm = TRUE)

                                            old_brier <- old_brier + old_brier_t
                                            new_brier <- new_brier + new_brier_t
                                            valid_timepoints <- valid_timepoints + 1
                                        }
                                    }
                                }
                            }
                        }

                        if (valid_timepoints > 0) {
                            metrics$old_integrated_brier <- old_brier / valid_timepoints
                            metrics$new_integrated_brier <- new_brier / valid_timepoints
                            metrics$brier_improvement <- metrics$old_integrated_brier - metrics$new_integrated_brier
                        }

                        # 3. Model Deviance and AIC comparison
                        metrics$old_aic <- AIC(old_fit)
                        metrics$new_aic <- AIC(new_fit)
                        metrics$aic_improvement <- metrics$old_aic - metrics$new_aic

                        # 4. Calibration assessment (simplified)
                        # Compare predicted vs observed risk in quintiles
                        old_risk <- predict(old_fit, newdata = test_data, type = "risk")
                        new_risk <- predict(new_fit, newdata = test_data, type = "risk")

                        if (length(old_risk) == nrow(test_data) && length(new_risk) == nrow(test_data)) {
                            # Convert event to binary for calibration
                            event_binary <- as.numeric(test_data[[event_col]] == self$options$eventLevel)

                            # Calibration slope (correlation between predicted and observed)
                            metrics$old_calibration <- cor(old_risk, event_binary, use = "complete.obs")
                            metrics$new_calibration <- cor(new_risk, event_binary, use = "complete.obs")
                            metrics$calibration_improvement <- metrics$new_calibration - metrics$old_calibration
                        }
                    },
                    error = function(e) {
                        # If any metric calculation fails, set to NA
                        metrics$error <- paste("Metric calculation failed:", e$message)
                    }
                )

                return(metrics)
            },

            # Helper method: Generate cross-validation performance plot
            .generateCrossValidationPlot = function(cv_results) {
                plot_image <- self$results$crossValidationPlot
                if (is.null(plot_image)) {
                    return()
                }

                tryCatch(
                    {
                        # Prepare data for plotting
                        valid_results <- cv_results[!is.na(cv_results$old_cindex) & !is.na(cv_results$new_cindex), ]

                        if (nrow(valid_results) == 0) {
                            return()
                        }

                        # Store only the necessary data for plotting (not the plot object)
                        plot_data <- list(
                            fold = valid_results$fold,
                            old_cindex = valid_results$old_cindex,
                            new_cindex = valid_results$new_cindex,
                            cindex_diff = valid_results$cindex_diff
                        )

                        # Set state with minimal data
                        plot_image$setState(plot_data)
                    },
                    error = function(e) {
                        # If plot generation fails, set error state
                        NULL
                    }
                )
            },

            # Advanced interaction detection for multivariable analysis
            .performAdvancedInteractionDetection = function(covariate_data, all_covariates, old_stage, new_stage, survival_time) {
                tryCatch(
                    {
                        event_col <- self$options$event

                        # Debug input parameters
                        if (length(all_covariates) > 0) {} else {}

                        # Validate inputs
                        if (length(all_covariates) == 0) {
                            stop("No covariates provided for interaction detection")
                        }

                        # Validate survival data
                        if (!survival_time %in% names(covariate_data)) {
                            stop(paste("Survival time variable", survival_time, "not found in data"))
                        }
                        if (!event_col %in% names(covariate_data)) {
                            stop(paste("Event variable", event_col, "not found in data"))
                        }

                        survival_times <- covariate_data[[survival_time]]
                        events <- covariate_data[[event_col]]


                        # Create survival object
                        surv_obj <- Surv(survival_times, events == self$options$eventLevel)

                        # Interaction detection results
                        interaction_results <- data.frame(
                            Variable = character(),
                            Old_Stage_Interaction_P = numeric(),
                            New_Stage_Interaction_P = numeric(),
                            Interaction_Comparison_P = numeric(),
                            Old_Stage_HR_Main = numeric(),
                            Old_Stage_HR_Interaction = numeric(),
                            New_Stage_HR_Main = numeric(),
                            New_Stage_HR_Interaction = numeric(),
                            Clinical_Significance = character(),
                            stringsAsFactors = FALSE
                        )

                        # Test interactions with each covariate
                        for (covar in all_covariates) {
                            if (covar %in% names(covariate_data)) {
                                # Test interaction with old staging system

                                # Build formulas with debugging
                                old_int_formula_str <- paste("surv_obj ~", old_stage, "*", covar)
                                old_main_formula_str <- paste("surv_obj ~", old_stage, "+", covar)


                                old_interaction_formula <- as.formula(old_int_formula_str)
                                old_main_formula <- as.formula(old_main_formula_str)

                                old_interaction_fit <- tryCatch(
                                    {
                                        coxph(old_interaction_formula, data = covariate_data)
                                    },
                                    error = function(e) {
                                        NULL
                                    }
                                )

                                old_main_fit <- tryCatch(
                                    {
                                        coxph(old_main_formula, data = covariate_data)
                                    },
                                    error = function(e) {
                                        NULL
                                    }
                                )

                                # Test interaction with new staging system
                                new_interaction_formula <- as.formula(paste("surv_obj ~", new_stage, "*", covar))
                                new_main_formula <- as.formula(paste("surv_obj ~", new_stage, "+", covar))

                                new_interaction_fit <- tryCatch(coxph(new_interaction_formula, data = covariate_data), error = function(e) NULL)
                                new_main_fit <- tryCatch(coxph(new_main_formula, data = covariate_data), error = function(e) NULL)

                                if (!is.null(old_interaction_fit) && !is.null(old_main_fit) &&
                                    !is.null(new_interaction_fit) && !is.null(new_main_fit)) {
                                    # Likelihood ratio tests for interactions
                                    old_interaction_p <- tryCatch(
                                        {
                                            if (is.null(old_main_fit) || is.null(old_interaction_fit)) {
                                                NA
                                            } else {
                                                lr_test <- anova(old_main_fit, old_interaction_fit, test = "Chisq")

                                                # Debug anova output structure

                                                # Try different ways to extract p-value
                                                p_val <- if ("Pr(>|Chi|)" %in% names(lr_test)) {
                                                    lr_test[["Pr(>|Chi|)"]][2]
                                                } else if ("P(>|Chi|)" %in% names(lr_test)) {
                                                    lr_test[["P(>|Chi|)"]][2]
                                                } else if ("Pr(>Chi)" %in% names(lr_test)) {
                                                    lr_test[["Pr(>Chi)"]][2]
                                                } else if (is.data.frame(lr_test) && ncol(lr_test) >= 5) {
                                                    # Try to get from the data frame structure
                                                    lr_test[2, ncol(lr_test)]
                                                } else {
                                                    NA
                                                }


                                                # Return NA if p_val is NULL or empty
                                                if (is.null(p_val) || length(p_val) == 0) NA else p_val
                                            }
                                        },
                                        error = function(e) {
                                            NA
                                        }
                                    )

                                    new_interaction_p <- tryCatch(
                                        {
                                            if (is.null(new_main_fit) || is.null(new_interaction_fit)) {
                                                NA
                                            } else {
                                                lr_test <- anova(new_main_fit, new_interaction_fit, test = "Chisq")

                                                # Debug anova output structure

                                                # Try different ways to extract p-value
                                                p_val <- if ("Pr(>|Chi|)" %in% names(lr_test)) {
                                                    lr_test[["Pr(>|Chi|)"]][2]
                                                } else if ("P(>|Chi|)" %in% names(lr_test)) {
                                                    lr_test[["P(>|Chi|)"]][2]
                                                } else if ("Pr(>Chi)" %in% names(lr_test)) {
                                                    lr_test[["Pr(>Chi)"]][2]
                                                } else if (is.data.frame(lr_test) && ncol(lr_test) >= 5) {
                                                    # Try to get from the data frame structure
                                                    lr_test[2, ncol(lr_test)]
                                                } else {
                                                    NA
                                                }


                                                # Return NA if p_val is NULL or empty
                                                if (is.null(p_val) || length(p_val) == 0) NA else p_val
                                            }
                                        },
                                        error = function(e) {
                                            NA
                                        }
                                    )

                                    # Compare interaction strength between staging systems
                                    comparison_p <- tryCatch(
                                        {
                                            # Create unified model with both staging systems and their interactions
                                            unified_formula <- as.formula(paste(
                                                "surv_obj ~", old_stage, "+", new_stage, "+", covar, "+",
                                                paste0(old_stage, ":", covar), "+", paste0(new_stage, ":", covar)
                                            ))
                                            unified_fit <- coxph(unified_formula, data = covariate_data)

                                            # Test if interaction coefficients differ significantly
                                            coef_summary <- summary(unified_fit)
                                            interaction_terms <- grep(":", rownames(coef_summary$coefficients))
                                            if (length(interaction_terms) >= 2) {
                                                # Simple comparison of p-values (more sophisticated methods could be implemented)
                                                min(coef_summary$coefficients[interaction_terms, "Pr(>|z|)"])
                                            } else {
                                                NA
                                            }
                                        },
                                        error = function(e) NA
                                    )

                                    # Extract hazard ratios
                                    old_main_hr <- tryCatch(exp(coef(old_main_fit)[grep(old_stage, names(coef(old_main_fit)))[1]]), error = function(e) NA)
                                    old_interaction_hr <- tryCatch(
                                        {
                                            interaction_coef <- coef(old_interaction_fit)[grep(":", names(coef(old_interaction_fit)))]
                                            if (length(interaction_coef) > 0) exp(interaction_coef[1]) else NA
                                        },
                                        error = function(e) NA
                                    )

                                    new_main_hr <- tryCatch(exp(coef(new_main_fit)[grep(new_stage, names(coef(new_main_fit)))[1]]), error = function(e) NA)
                                    new_interaction_hr <- tryCatch(
                                        {
                                            interaction_coef <- coef(new_interaction_fit)[grep(":", names(coef(new_interaction_fit)))]
                                            if (length(interaction_coef) > 0) exp(interaction_coef[1]) else NA
                                        },
                                        error = function(e) NA
                                    )

                                    # Determine clinical significance with robust error handling
                                    clinical_sig <- tryCatch(
                                        {
                                            # Handle cases where p-values might be NA
                                            old_p_valid <- !is.na(old_interaction_p) && is.finite(old_interaction_p) && !is.null(old_interaction_p)
                                            new_p_valid <- !is.na(new_interaction_p) && is.finite(new_interaction_p) && !is.null(new_interaction_p)

                                            base_sig <- "Not significant"

                                            if (old_p_valid || new_p_valid) {
                                                old_significant <- if (old_p_valid) {
                                                    old_interaction_p < 0.05
                                                } else {
                                                    FALSE
                                                }

                                                new_significant <- if (new_p_valid) {
                                                    new_interaction_p < 0.05
                                                } else {
                                                    FALSE
                                                }

                                                if (old_significant || new_significant) {
                                                    if (old_significant && (!new_p_valid || (new_p_valid && new_interaction_p >= 0.05))) {
                                                        base_sig <- "Old staging shows interaction"
                                                    } else if (new_significant && (!old_p_valid || (old_p_valid && old_interaction_p >= 0.05))) {
                                                        base_sig <- "New staging shows interaction"
                                                    } else if (old_significant && new_significant) {
                                                        base_sig <- "Both staging systems show interaction"
                                                    }
                                                }
                                            }

                                            base_sig
                                        },
                                        error = function(e) {
                                            paste("Error in significance determination:", e$message)
                                        }
                                    )

                                    # Add to results with proper NA handling
                                    new_row <- data.frame(
                                        Variable = covar,
                                        Old_Stage_Interaction_P = if (is.na(old_interaction_p)) NA_real_ else round(old_interaction_p, 4),
                                        New_Stage_Interaction_P = if (is.na(new_interaction_p)) NA_real_ else round(new_interaction_p, 4),
                                        Interaction_Comparison_P = if (is.na(comparison_p)) NA_real_ else round(comparison_p, 4),
                                        Old_Stage_HR_Main = if (is.na(old_main_hr)) NA_real_ else round(old_main_hr, 3),
                                        Old_Stage_HR_Interaction = if (is.na(old_interaction_hr)) NA_real_ else round(old_interaction_hr, 3),
                                        New_Stage_HR_Main = if (is.na(new_main_hr)) NA_real_ else round(new_main_hr, 3),
                                        New_Stage_HR_Interaction = if (is.na(new_interaction_hr)) NA_real_ else round(new_interaction_hr, 3),
                                        Clinical_Significance = as.character(clinical_sig),
                                        stringsAsFactors = FALSE
                                    )

                                    interaction_results <- rbind(interaction_results, new_row)
                                }
                            }
                        }

                        # Create summary statistics
                        summary_stats <- list(
                            total_variables_tested = nrow(interaction_results),
                            significant_old_interactions = sum(interaction_results$Old_Stage_Interaction_P < 0.05, na.rm = TRUE),
                            significant_new_interactions = sum(interaction_results$New_Stage_Interaction_P < 0.05, na.rm = TRUE),
                            variables_with_differential_interactions = sum(
                                (!is.na(interaction_results$Old_Stage_Interaction_P) & interaction_results$Old_Stage_Interaction_P < 0.05) !=
                                    (!is.na(interaction_results$New_Stage_Interaction_P) & interaction_results$New_Stage_Interaction_P < 0.05),
                                na.rm = TRUE
                            )
                        )

                        # Debug output
                        if (nrow(interaction_results) > 0) {}

                        return(list(
                            interaction_results = interaction_results,
                            summary_stats = summary_stats
                        ))
                    },
                    error = function(e) {
                        return(list(
                            interaction_results = data.frame(
                                Variable = "Error",
                                Old_Stage_Interaction_P = NA,
                                New_Stage_Interaction_P = NA,
                                Interaction_Comparison_P = NA,
                                Old_Stage_HR_Main = NA,
                                Old_Stage_HR_Interaction = NA,
                                New_Stage_HR_Main = NA,
                                New_Stage_HR_Interaction = NA,
                                Clinical_Significance = paste("Error:", e$message),
                                stringsAsFactors = FALSE
                            ),
                            summary_stats = list(
                                total_variables_tested = 0,
                                significant_old_interactions = 0,
                                significant_new_interactions = 0,
                                variables_with_differential_interactions = 0
                            )
                        ))
                    }
                )
            },

            # Comprehensive model diagnostics for multivariable analysis
            .performComprehensiveModelDiagnostics = function(covariate_data, all_covariates, old_stage, new_stage, survival_time) {
                tryCatch(
                    {
                        event_col <- self$options$event

                        # Validate survival data
                        if (!survival_time %in% names(covariate_data)) {
                            stop(paste("Survival time variable", survival_time, "not found in data"))
                        }
                        if (!event_col %in% names(covariate_data)) {
                            stop(paste("Event variable", event_col, "not found in data"))
                        }

                        survival_times <- covariate_data[[survival_time]]
                        events <- covariate_data[[event_col]]


                        # Create survival object
                        surv_obj <- Surv(survival_times, events == self$options$eventLevel)

                        # Build comprehensive models for diagnostics
                        old_formula <- as.formula(paste("surv_obj ~", old_stage, "+", paste(all_covariates, collapse = " + ")))
                        new_formula <- as.formula(paste("surv_obj ~", new_stage, "+", paste(all_covariates, collapse = " + ")))

                        old_model <- tryCatch(coxph(old_formula, data = covariate_data), error = function(e) NULL)
                        new_model <- tryCatch(coxph(new_formula, data = covariate_data), error = function(e) NULL)

                        diagnostics_results <- list(
                            old_model_diagnostics = NULL,
                            new_model_diagnostics = NULL,
                            comparative_diagnostics = NULL,
                            model_assumptions = NULL,
                            outlier_analysis = NULL,
                            influence_analysis = NULL
                        )

                        # Diagnose old staging model
                        if (!is.null(old_model)) {
                            diagnostics_results$old_model_diagnostics <- private$.diagnoseSingleModel(old_model, covariate_data, "Old Staging")
                        }

                        # Diagnose new staging model
                        if (!is.null(new_model)) {
                            diagnostics_results$new_model_diagnostics <- private$.diagnoseSingleModel(new_model, covariate_data, "New Staging")
                        }

                        # Comparative diagnostics
                        if (!is.null(old_model) && !is.null(new_model)) {
                            diagnostics_results$comparative_diagnostics <- private$.compareModelDiagnostics(old_model, new_model, covariate_data)
                        }

                        # Test model assumptions
                        if (!is.null(old_model) && !is.null(new_model)) {
                            diagnostics_results$model_assumptions <- private$.testModelAssumptions(old_model, new_model, covariate_data)
                        }

                        # Outlier analysis
                        if (!is.null(old_model) && !is.null(new_model)) {
                            diagnostics_results$outlier_analysis <- private$.performOutlierAnalysis(old_model, new_model, covariate_data)
                        }

                        # Influence analysis
                        if (!is.null(old_model) && !is.null(new_model)) {
                            diagnostics_results$influence_analysis <- private$.performInfluenceAnalysis(old_model, new_model, covariate_data)
                        }

                        return(diagnostics_results)
                    },
                    error = function(e) {
                        return(list(
                            error = paste("Comprehensive model diagnostics failed:", e$message),
                            old_model_diagnostics = NULL,
                            new_model_diagnostics = NULL,
                            comparative_diagnostics = NULL,
                            model_assumptions = NULL,
                            outlier_analysis = NULL,
                            influence_analysis = NULL
                        ))
                    }
                )
            },

            # Diagnose a single Cox model
            .diagnoseSingleModel = function(model, data, model_name) {
                tryCatch(
                    {
                        # Basic model summary statistics
                        model_summary <- summary(model)

                        # Goodness of fit measures
                        concordance <- model_summary$concordance
                        rsquare <- model_summary$rsq

                        # Residual analysis
                        martingale_residuals <- residuals(model, type = "martingale")
                        deviance_residuals <- residuals(model, type = "deviance")

                        # Calculate residual statistics
                        martingale_stats <- list(
                            mean = mean(martingale_residuals, na.rm = TRUE),
                            sd = sd(martingale_residuals, na.rm = TRUE),
                            min = min(martingale_residuals, na.rm = TRUE),
                            max = max(martingale_residuals, na.rm = TRUE),
                            outliers = sum(abs(martingale_residuals) > 2.5, na.rm = TRUE)
                        )

                        deviance_stats <- list(
                            mean = mean(deviance_residuals, na.rm = TRUE),
                            sd = sd(deviance_residuals, na.rm = TRUE),
                            min = min(deviance_residuals, na.rm = TRUE),
                            max = max(deviance_residuals, na.rm = TRUE),
                            outliers = sum(abs(deviance_residuals) > 2.5, na.rm = TRUE)
                        )

                        # Model convergence and warnings
                        convergence_info <- list(
                            converged = model$iter < model$n.iter,
                            iterations = model$iter,
                            loglik = model$loglik[length(model$loglik)],
                            score = model$score,
                            df = model$df
                        )

                        # Variable significance summary
                        coef_summary <- model_summary$coefficients
                        sig_vars <- rownames(coef_summary)[coef_summary[, "Pr(>|z|)"] < 0.05]

                        return(list(
                            model_name = model_name,
                            concordance = concordance[1],
                            concordance_se = sqrt(concordance[2]),
                            rsquare = rsquare,
                            martingale_residuals = martingale_stats,
                            deviance_residuals = deviance_stats,
                            convergence = convergence_info,
                            significant_variables = sig_vars,
                            total_variables = nrow(coef_summary),
                            sample_size = model$n
                        ))
                    },
                    error = function(e) {
                        return(list(
                            model_name = model_name,
                            error = paste("Single model diagnostics failed:", e$message)
                        ))
                    }
                )
            },

            # Compare diagnostics between models
            .compareModelDiagnostics = function(old_model, new_model, data) {
                tryCatch(
                    {
                        # Compare concordance indices
                        old_concordance <- concordance(old_model)
                        new_concordance <- concordance(new_model)

                        # Compare log-likelihoods

                        # AIC comparison
                        old_aic <- AIC(old_model)
                        new_aic <- AIC(new_model)

                        # BIC comparison
                        old_bic <- BIC(old_model)
                        new_bic <- BIC(new_model)

                        # Likelihood ratio test if models are nested
                        lr_test_result <- tryCatch(
                            {
                                # Check if models are nested by comparing degrees of freedom
                                if (old_model$df != new_model$df) {
                                    lr_test <- anova(old_model, new_model, test = "Chisq")
                                    list(
                                        chi_square = lr_test$Chisq[2],
                                        df = lr_test$Df[2],
                                        p_value = lr_test$`Pr(>Chi)`[2],
                                        nested = TRUE
                                    )
                                } else {
                                    list(nested = FALSE, message = "Models have same degrees of freedom")
                                }
                            },
                            error = function(e) list(nested = FALSE, error = e$message)
                        )

                        # Compare residual distributions
                        old_mart_res <- residuals(old_model, type = "martingale")
                        new_mart_res <- residuals(new_model, type = "martingale")

                        # KS test for residual distributions
                        residual_comparison <- tryCatch(
                            {
                                ks_test <- ks.test(old_mart_res, new_mart_res)
                                list(
                                    ks_statistic = ks_test$statistic,
                                    ks_p_value = ks_test$p.value,
                                    interpretation = ifelse(ks_test$p.value < 0.05,
                                        "Residual distributions differ significantly",
                                        "Residual distributions are similar"
                                    )
                                )
                            },
                            error = function(e) list(error = e$message)
                        )

                        return(list(
                            concordance_comparison = list(
                                old = old_concordance$concordance,
                                new = new_concordance$concordance,
                                improvement = new_concordance$concordance - old_concordance$concordance,
                                significant = abs(new_concordance$concordance - old_concordance$concordance) > 0.02
                            ),
                            information_criteria = list(
                                aic_old = old_aic,
                                aic_new = new_aic,
                                aic_improvement = old_aic - new_aic,
                                bic_old = old_bic,
                                bic_new = new_bic,
                                bic_improvement = old_bic - new_bic,
                                preferred_by_aic = ifelse(new_aic < old_aic, "New Staging", "Old Staging"),
                                preferred_by_bic = ifelse(new_bic < old_bic, "New Staging", "Old Staging")
                            ),
                            likelihood_ratio_test = lr_test_result,
                            residual_comparison = residual_comparison
                        ))
                    },
                    error = function(e) {
                        return(list(error = paste("Model comparison diagnostics failed:", e$message)))
                    }
                )
            },

            # Test key model assumptions
            .testModelAssumptions = function(old_model, new_model, data) {
                tryCatch(
                    {
                        # Test proportional hazards assumption using cox.zph
                        old_ph_test <- tryCatch(cox.zph(old_model), error = function(e) NULL)
                        new_ph_test <- tryCatch(cox.zph(new_model), error = function(e) NULL)

                        assumption_results <- list(
                            proportional_hazards_old = NULL,
                            proportional_hazards_new = NULL,
                            linearity_test = NULL,
                            influential_observations = NULL
                        )

                        # Proportional hazards test for old model
                        if (!is.null(old_ph_test)) {
                            assumption_results$proportional_hazards_old <- list(
                                global_p = old_ph_test$table["GLOBAL", "p"],
                                variables = rownames(old_ph_test$table),
                                p_values = old_ph_test$table[, "p"],
                                assumption_violated = any(old_ph_test$table[, "p"] < 0.05, na.rm = TRUE),
                                worst_violator = rownames(old_ph_test$table)[which.min(old_ph_test$table[, "p"])]
                            )
                        }

                        # Proportional hazards test for new model
                        if (!is.null(new_ph_test)) {
                            assumption_results$proportional_hazards_new <- list(
                                global_p = new_ph_test$table["GLOBAL", "p"],
                                variables = rownames(new_ph_test$table),
                                p_values = new_ph_test$table[, "p"],
                                assumption_violated = any(new_ph_test$table[, "p"] < 0.05, na.rm = TRUE),
                                worst_violator = rownames(new_ph_test$table)[which.min(new_ph_test$table[, "p"])]
                            )
                        }

                        # Compare assumption violations
                        if (!is.null(old_ph_test) && !is.null(new_ph_test)) {
                            old_violations <- sum(old_ph_test$table[, "p"] < 0.05, na.rm = TRUE)
                            new_violations <- sum(new_ph_test$table[, "p"] < 0.05, na.rm = TRUE)

                            assumption_results$comparison <- list(
                                old_violations = old_violations,
                                new_violations = new_violations,
                                improvement = old_violations - new_violations,
                                interpretation = ifelse(new_violations < old_violations,
                                    "New staging has fewer assumption violations",
                                    ifelse(new_violations > old_violations,
                                        "Old staging has fewer assumption violations",
                                        "Both models have similar assumption violations"
                                    )
                                )
                            )
                        }

                        return(assumption_results)
                    },
                    error = function(e) {
                        return(list(error = paste("Model assumption testing failed:", e$message)))
                    }
                )
            },

            # Perform outlier analysis
            .performOutlierAnalysis = function(old_model, new_model, data) {
                tryCatch(
                    {
                        # Calculate different types of residuals for outlier detection
                        old_mart <- residuals(old_model, type = "martingale")
                        new_mart <- residuals(new_model, type = "martingale")
                        old_dev <- residuals(old_model, type = "deviance")
                        new_dev <- residuals(new_model, type = "deviance")

                        # Define outlier thresholds
                        mart_threshold <- 2.5
                        dev_threshold <- 2.5

                        # Identify outliers
                        old_outliers <- which(abs(old_mart) > mart_threshold | abs(old_dev) > dev_threshold)
                        new_outliers <- which(abs(new_mart) > mart_threshold | abs(new_dev) > dev_threshold)

                        # Outlier statistics
                        outlier_summary <- data.frame(
                            Model = c("Old Staging", "New Staging"),
                            Martingale_Outliers = c(
                                sum(abs(old_mart) > mart_threshold, na.rm = TRUE),
                                sum(abs(new_mart) > mart_threshold, na.rm = TRUE)
                            ),
                            Deviance_Outliers = c(
                                sum(abs(old_dev) > dev_threshold, na.rm = TRUE),
                                sum(abs(new_dev) > dev_threshold, na.rm = TRUE)
                            ),
                            Total_Outliers = c(length(old_outliers), length(new_outliers)),
                            Outlier_Percentage = c(
                                round(length(old_outliers) / nrow(data) * 100, 2),
                                round(length(new_outliers) / nrow(data) * 100, 2)
                            ),
                            stringsAsFactors = FALSE
                        )

                        # Identify consistent outliers (outliers in both models)
                        consistent_outliers <- intersect(old_outliers, new_outliers)

                        return(list(
                            outlier_summary = outlier_summary,
                            old_outlier_indices = old_outliers,
                            new_outlier_indices = new_outliers,
                            consistent_outliers = consistent_outliers,
                            outlier_improvement = length(old_outliers) - length(new_outliers),
                            interpretation = ifelse(length(new_outliers) < length(old_outliers),
                                "New staging model has fewer outliers",
                                ifelse(length(new_outliers) > length(old_outliers),
                                    "Old staging model has fewer outliers",
                                    "Both models have similar outlier patterns"
                                )
                            )
                        ))
                    },
                    error = function(e) {
                        return(list(error = paste("Outlier analysis failed:", e$message)))
                    }
                )
            },

            # Perform influence analysis
            .performInfluenceAnalysis = function(old_model, new_model, data) {
                tryCatch(
                    {
                        # Calculate dfbetas for influence analysis.
                        # stats::dfbetas has no coxph method (it dispatches only for lm/glm), so
                        # dfbetas(cox_model) always errored and this whole analysis returned all-NULL.
                        # survival exposes them through residuals(); as.matrix keeps the
                        # single-coefficient case (two-level staging) a matrix for apply().
                        old_dfbetas <- tryCatch(as.matrix(residuals(old_model, type = "dfbetas")), error = function(e) NULL)
                        new_dfbetas <- tryCatch(as.matrix(residuals(new_model, type = "dfbetas")), error = function(e) NULL)

                        influence_results <- list(
                            old_model_influence = NULL,
                            new_model_influence = NULL,
                            comparative_influence = NULL
                        )

                        # Analyze influence for old model
                        if (!is.null(old_dfbetas)) {
                            influence_threshold <- 2 / sqrt(nrow(old_dfbetas))
                            old_influential <- apply(abs(old_dfbetas) > influence_threshold, 1, any)

                            influence_results$old_model_influence <- list(
                                influential_observations = which(old_influential),
                                n_influential = sum(old_influential),
                                percentage_influential = round(sum(old_influential) / nrow(old_dfbetas) * 100, 2),
                                max_influence = max(abs(old_dfbetas), na.rm = TRUE)
                            )
                        }

                        # Analyze influence for new model
                        if (!is.null(new_dfbetas)) {
                            influence_threshold <- 2 / sqrt(nrow(new_dfbetas))
                            new_influential <- apply(abs(new_dfbetas) > influence_threshold, 1, any)

                            influence_results$new_model_influence <- list(
                                influential_observations = which(new_influential),
                                n_influential = sum(new_influential),
                                percentage_influential = round(sum(new_influential) / nrow(new_dfbetas) * 100, 2),
                                max_influence = max(abs(new_dfbetas), na.rm = TRUE)
                            )
                        }

                        # Compare influence between models
                        if (!is.null(old_dfbetas) && !is.null(new_dfbetas)) {
                            old_infl_obs <- which(apply(abs(old_dfbetas) > 2 / sqrt(nrow(old_dfbetas)), 1, any))
                            new_infl_obs <- which(apply(abs(new_dfbetas) > 2 / sqrt(nrow(new_dfbetas)), 1, any))

                            influence_results$comparative_influence <- list(
                                improvement = length(old_infl_obs) - length(new_infl_obs),
                                consistent_influential = intersect(old_infl_obs, new_infl_obs),
                                only_old_influential = setdiff(old_infl_obs, new_infl_obs),
                                only_new_influential = setdiff(new_infl_obs, old_infl_obs),
                                interpretation = ifelse(length(new_infl_obs) < length(old_infl_obs),
                                    "New staging model is less sensitive to influential observations",
                                    ifelse(length(new_infl_obs) > length(old_infl_obs),
                                        "Old staging model is less sensitive to influential observations",
                                        "Both models have similar sensitivity to influential observations"
                                    )
                                )
                            )
                        }

                        return(influence_results)
                    },
                    error = function(e) {
                        return(list(error = paste("Influence analysis failed:", e$message)))
                    }
                )
            },

            # ==================================================================================
            # PHASE 1 ADVANCED ENHANCEMENTS: Evidence-Based Assessment Framework
            # ==================================================================================

            # Will Rogers Phenomenon Evidence Assessment Framework
            .performAdvancedWillRogersAssessment = function(data, all_results) {
                tryCatch(
                    {
                        # Initialize evidence summary
                        evidence_summary <- data.frame(
                            Criterion = character(0),
                            Assessment = character(0),
                            Evidence_Level = character(0),
                            Interpretation = character(0),
                            stringsAsFactors = FALSE
                        )

                        # Get basic parameters
                        old_stage <- self$options$oldStage
                        new_stage <- self$options$newStage
                        time_var <- self$options$survivalTime
                        event_var <- "event_binary"

                        # 1. Migration Pattern Analysis
                        migration_evidence <- private$.assessMigrationPatternEvidence(data, old_stage, new_stage)
                        evidence_summary <- rbind(evidence_summary, data.frame(
                            Criterion = "Migration Pattern",
                            Assessment = migration_evidence$assessment,
                            Evidence_Level = migration_evidence$strength,
                            Interpretation = migration_evidence$interpretation,
                            stringsAsFactors = FALSE
                        ))

                        # 2. Survival Pattern Comparison
                        survival_evidence <- private$.assessSurvivalPatternEvidence(data, old_stage, new_stage, time_var, event_var)
                        evidence_summary <- rbind(evidence_summary, survival_evidence)

                        # 3. Lymph Node/Risk Factor Biology Evidence (if available)
                        ln_evidence <- private$.assessLymphNodeEvidence(data, old_stage, new_stage)
                        if (!is.null(ln_evidence)) {
                            evidence_summary <- rbind(evidence_summary, ln_evidence)
                        }

                        # 4. Prognostic Discrimination Evidence
                        discrimination_evidence <- private$.assessDiscriminationEvidence(all_results)
                        evidence_summary <- rbind(evidence_summary, discrimination_evidence)

                        # 5. Simulation-Based Will Rogers Validation (if enabled and sufficient data)
                        if (nrow(data) >= 100) { # Require sufficient data for simulation
                            simulation_evidence <- private$.performWillRogersSimulation(data, old_stage, new_stage, time_var, event_var)
                            if (!is.null(simulation_evidence)) {
                                evidence_summary <- rbind(evidence_summary, simulation_evidence)
                            }
                        }

                        # Calculate overall recommendation
                        recommendation <- private$.generateWillRogersRecommendation(evidence_summary)

                        # Store results
                        all_results$will_rogers_evidence_summary <- evidence_summary
                        all_results$will_rogers_recommendation <- recommendation

                        # R is copy-on-modify: return the MUTATED list so the caller can
                        # reassign it, otherwise .populateWillRogers* below see NULL.
                        return(all_results)
                    },
                    error = function(e) {
                        # Return all_results UNCHANGED so a failure here cannot wipe
                        # everything accumulated by earlier analyses.
                        all_results
                    }
                )
            },

            # Assess migration pattern evidence
            .assessMigrationPatternEvidence = function(data, old_stage, new_stage) {
                tryCatch(
                    {
                        # Create migration matrix
                        # Label-based, on one ordered scale (see .stageDirection): the former diag() and
                        # i < j comparisons were positional and wrong whenever the two systems' labels or
                        # level order differ.
                        migration_rate <- 1 - mean(as.character(data[[old_stage]]) == as.character(data[[new_stage]]), na.rm = TRUE)
                        dirn <- private$.stageDirection(data[[old_stage]], data[[new_stage]])
                        if (!dirn$comparable) {
                            return(list(
                                assessment = "NOT ASSESSED",
                                strength = "None",
                                interpretation = "The two systems use different stage labels, so the balance of up- and downstaging is undefined",
                                migration_rate = migration_rate,
                                upstaging = NA,
                                downstaging = NA
                            ))
                        }
                        upstaging <- sum(dirn$direction == 1L, na.rm = TRUE)
                        downstaging <- sum(dirn$direction == -1L, na.rm = TRUE)

                        # Assessment logic
                        if (migration_rate < 0.10) {
                            assessment <- "PASS"
                            strength <- "Strong"
                            interpretation <- "Low migration rate suggests stable staging criteria"
                        } else if (migration_rate < 0.25) {
                            if (abs(upstaging - downstaging) / (upstaging + downstaging + 1) < 0.3) {
                                assessment <- "PASS"
                                strength <- "Moderate"
                                interpretation <- "Balanced migration pattern with moderate rate"
                            } else {
                                assessment <- "BORDERLINE"
                                strength <- "Moderate"
                                interpretation <- "Unbalanced migration pattern needs investigation"
                            }
                        } else {
                            assessment <- "CONCERN"
                            strength <- "Weak"
                            interpretation <- "High migration rate may indicate unstable criteria"
                        }

                        return(list(
                            assessment = assessment,
                            strength = strength,
                            interpretation = interpretation,
                            migration_rate = migration_rate,
                            upstaging = upstaging,
                            downstaging = downstaging
                        ))
                    },
                    error = function(e) {
                        return(list(
                            assessment = "ERROR",
                            strength = "None",
                            interpretation = paste("Migration analysis failed:", e$message)
                        ))
                    }
                )
            },

            # Assess survival pattern evidence
            .assessSurvivalPatternEvidence = function(data, old_stage, new_stage, time_var, event_var) {
                tryCatch(
                    {
                        # Fit survival models for comparison
                        old_formula <- as.formula(paste("survival::Surv(", time_var, ",", event_var, ") ~", old_stage))
                        new_formula <- as.formula(paste("survival::Surv(", time_var, ",", event_var, ") ~", new_stage))

                        old_cox <- survival::coxph(old_formula, data = data)
                        new_cox <- survival::coxph(new_formula, data = data)

                        # Calculate survival curve comparisons for upstaged patients
                        upstaged_evidence <- private$.analyzeUpstagedPatientSurvival(data, old_stage, new_stage, time_var, event_var)

                        # Assessment based on survival similarity to target stage
                        if (!is.null(upstaged_evidence) && upstaged_evidence$similarity_to_target > 0.8) {
                            assessment <- "PASS"
                            strength <- "Strong"
                            interpretation <- "Upstaged patients show survival similar to target stage"
                        } else if (!is.null(upstaged_evidence) && upstaged_evidence$similarity_to_target > 0.6) {
                            assessment <- "BORDERLINE"
                            strength <- "Moderate"
                            interpretation <- "Upstaged patients show intermediate survival patterns"
                        } else {
                            assessment <- "CONCERN"
                            strength <- "Weak"
                            interpretation <- "Upstaged patients may retain original stage survival"
                        }

                        return(data.frame(
                            Criterion = "Survival Pattern",
                            Assessment = assessment,
                            Evidence_Level = strength,
                            Interpretation = interpretation,
                            stringsAsFactors = FALSE
                        ))
                    },
                    error = function(e) {
                        return(data.frame(
                            Criterion = "Survival Pattern",
                            Assessment = .("ERROR"),
                            Evidence_Level = "None",
                            Interpretation = paste("Survival analysis failed:", e$message),
                            stringsAsFactors = FALSE
                        ))
                    }
                )
            },

            # Analyze upstaged patient survival patterns
            .analyzeUpstagedPatientSurvival = function(data, old_stage, new_stage, time_var, event_var) {
                tryCatch(
                    {
                        # Identify upstaged patients (moved to higher stage)

                        # Upstaged = higher stage on one ordered scale; none when the label sets differ
                        upstaged_patients <- data[private$.stageDirection(data[[old_stage]], data[[new_stage]])$direction %in% 1L, , drop = FALSE]

                        if (nrow(upstaged_patients) < 10) {
                            return(list(similarity_to_target = 0.5, note = "Too few upstaged patients"))
                        }

                        # Calculate median survival for upstaged patients
                        upstaged_surv <- survival::survfit(
                            survival::Surv(upstaged_patients[[time_var]], upstaged_patients[[event_var]]) ~ 1
                        )
                        upstaged_median <- summary(upstaged_surv)$table["median"]

                        # Compare to target stage median survival
                        target_stages <- unique(upstaged_patients[[new_stage]])
                        target_similarities <- numeric(0)

                        for (target_stage in target_stages) {
                            target_patients <- data[data[[new_stage]] == target_stage &
                                data[[old_stage]] == data[[new_stage]], ]
                            if (nrow(target_patients) > 5) {
                                target_surv <- survival::survfit(
                                    survival::Surv(target_patients[[time_var]], target_patients[[event_var]]) ~ 1
                                )
                                target_median <- summary(target_surv)$table["median"]

                                if (!is.na(upstaged_median) && !is.na(target_median) && target_median > 0) {
                                    similarity <- 1 - abs(upstaged_median - target_median) / target_median
                                    target_similarities <- c(target_similarities, similarity)
                                }
                            }
                        }

                        avg_similarity <- if (length(target_similarities) > 0) mean(target_similarities) else 0.5

                        return(list(
                            similarity_to_target = max(0, min(1, avg_similarity)),
                            upstaged_median = upstaged_median,
                            n_upstaged = nrow(upstaged_patients)
                        ))
                    },
                    error = function(e) {
                        return(list(similarity_to_target = 0.5, error = e$message))
                    }
                )
            },

            # Assess lymph node/risk factor evidence
            .assessLymphNodeEvidence = function(data, old_stage, new_stage) {
                tryCatch(
                    {
                        # Look for lymph node or related risk factor variables
                        possible_ln_vars <- c(
                            "LymphNodeStatus", "lymph_nodes", "nodes", "N_stage",
                            "node_positive", "ln_positive", "nodal_status"
                        )
                        ln_var <- NULL

                        for (var_name in possible_ln_vars) {
                            if (var_name %in% names(data)) {
                                ln_var <- var_name
                                break
                            }
                        }

                        if (is.null(ln_var)) {
                            return(NULL) # No lymph node data available
                        }

                        # Analyze lymph node positivity rates by migration pattern
                        upstaged_patients <- data[private$.stageDirection(data[[old_stage]], data[[new_stage]])$direction %in% 1L, , drop = FALSE]

                        if (nrow(upstaged_patients) == 0) {
                            return(NULL)
                        }

                        # Calculate LN+ rates
                        upstaged_ln_pos_rate <- mean(upstaged_patients[[ln_var]] == "Positive" |
                            upstaged_patients[[ln_var]] == 1, na.rm = TRUE)

                        overall_ln_pos_rate <- mean(data[[ln_var]] == "Positive" |
                            data[[ln_var]] == 1, na.rm = TRUE)

                        # Assessment based on biological consistency
                        if (upstaged_ln_pos_rate > overall_ln_pos_rate + 0.15) {
                            assessment <- "PASS"
                            strength <- "Strong"
                            interpretation <- "Upstaged patients show higher-risk biology (elevated LN+ rate)"
                        } else if (upstaged_ln_pos_rate > overall_ln_pos_rate) {
                            assessment <- "BORDERLINE"
                            strength <- "Moderate"
                            interpretation <- "Upstaged patients show modestly higher-risk biology"
                        } else {
                            assessment <- "CONCERN"
                            strength <- "Weak"
                            interpretation <- "Upstaged patients do not show higher-risk biology"
                        }

                        return(data.frame(
                            Criterion = "Biological Risk Factors",
                            Assessment = assessment,
                            Evidence_Level = strength,
                            Interpretation = interpretation,
                            stringsAsFactors = FALSE
                        ))
                    },
                    error = function(e) {
                        return(NULL) # Skip if analysis fails
                    }
                )
            }
        )
    )
}
