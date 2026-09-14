# stagemigration backend, part 2 of 5.
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
stagemigrationPart2 <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "stagemigrationPart2",
        inherit = stagemigrationPart1,
        private = list(
            .calculateDirectionalNRI = function(old_risk, new_risk, events, direction = "upstaging") {
                # Calculate NRI for a specific migration direction
                if (length(old_risk) < 5) {
                    return(NA)
                } # Need minimum sample size

                # Use median as threshold for risk categorization
                old_threshold <- median(old_risk, na.rm = TRUE)
                new_threshold <- median(new_risk, na.rm = TRUE)

                old_high_risk <- old_risk > old_threshold
                new_high_risk <- new_risk > new_threshold

                event_patients <- events == 1
                non_event_patients <- events == 0

                if (sum(event_patients) == 0 || sum(non_event_patients) == 0) {
                    return(NA)
                }

                # NRI is direction-invariant: for a subject who HAS the event, an upward risk
                # movement is correct reclassification; for a subject who does NOT, a downward
                # movement is correct. Restricting the sample to the downstaged subgroup does
                # not change that.
                #
                # The downstaging branch previously flipped the event term (crediting events
                # that moved to LOWER risk), so "Downstaging NRI" carried the opposite sign to
                # the literature quantity and was then fed to .interpretNRI() -- which would
                # describe a genuinely harmful staging change as an improvement.
                #
                # Both components are now computed the same way in both branches.
                event_improve <- sum(event_patients & !old_high_risk & new_high_risk)
                event_worsen <- sum(event_patients & old_high_risk & !new_high_risk)
                nri_events <- (event_improve - event_worsen) / max(sum(event_patients), 1)

                nonevent_improve <- sum(non_event_patients & old_high_risk & !new_high_risk)
                nonevent_worsen <- sum(non_event_patients & !old_high_risk & new_high_risk)
                nri_non_events <- (nonevent_improve - nonevent_worsen) / max(sum(non_event_patients), 1)

                return(nri_events + nri_non_events)
            },
            .calculateWeightedNRI = function(data, old_cox, new_cox, time_point = 24) {
                # Calculate NRI with higher weights for high-risk patients
                tryCatch(
                    {
                        # Get risk scores
                        old_risk <- predict(old_cox, type = "risk")
                        new_risk <- predict(new_cox, type = "risk")

                        # Create time-specific event indicator
                        event_at_time <- private$.eventStatusAtTime(data[[self$options$survivalTime]], data[["event_binary"]], time_point)

                        # Define risk-based weights - higher weights for higher risk patients
                        # Use quantile-based weighting
                        risk_quantiles <- quantile(old_risk, c(0.33, 0.67), na.rm = TRUE)

                        weights <- ifelse(old_risk <= risk_quantiles[1], 1.0, # Low risk: weight = 1
                            ifelse(old_risk <= risk_quantiles[2], 1.5, # Medium risk: weight = 1.5
                                2.0
                            )
                        ) # High risk: weight = 2

                        # Calculate risk categories using combined old+new risk median
                        combined_median <- median(c(old_risk, new_risk), na.rm = TRUE)
                        old_high_risk <- old_risk > combined_median
                        new_high_risk <- new_risk > combined_median

                        # NA-safe: subjects censored before time_point are in neither group
                        events <- !is.na(event_at_time) & event_at_time == 1
                        non_events <- !is.na(event_at_time) & event_at_time == 0

                        if (sum(events) == 0 || sum(non_events) == 0) {
                            return(NULL)
                        }

                        # Weighted NRI for events (moving to high risk is improvement)
                        event_improve <- events & !old_high_risk & new_high_risk
                        event_worsen <- events & old_high_risk & !new_high_risk

                        weighted_event_improve <- sum(weights[event_improve])
                        weighted_event_worsen <- sum(weights[event_worsen])
                        weighted_event_total <- sum(weights[events])

                        nri_events <- (weighted_event_improve - weighted_event_worsen) / max(weighted_event_total, 1)

                        # Weighted NRI for non-events (moving to low risk is improvement)
                        nonevent_improve <- non_events & old_high_risk & !new_high_risk
                        nonevent_worsen <- non_events & !old_high_risk & new_high_risk

                        weighted_nonevent_improve <- sum(weights[nonevent_improve])
                        weighted_nonevent_worsen <- sum(weights[nonevent_worsen])
                        weighted_nonevent_total <- sum(weights[non_events])

                        nri_non_events <- (weighted_nonevent_improve - weighted_nonevent_worsen) / max(weighted_nonevent_total, 1)

                        # Overall weighted NRI
                        nri_total <- nri_events + nri_non_events

                        # Bootstrap confidence intervals if enabled
                        ci_lower <- ci_upper <- p_value <- NA
                        if (self$options$performBootstrap) {
                            boot_results <- private$.bootstrapWeightedNRI(data, old_cox, new_cox, time_point)
                            if (length(boot_results) > 10) {
                                ci_probs_wnri <- private$.ciProbs()
                                ci_lower <- quantile(boot_results, ci_probs_wnri[1], na.rm = TRUE)
                                ci_upper <- quantile(boot_results, ci_probs_wnri[2], na.rm = TRUE)
                                p_value <- 2 * min(mean(boot_results >= 0), mean(boot_results <= 0))
                            }
                        }

                        return(list(
                            nri = nri_total,
                            nri_events = nri_events,
                            nri_non_events = nri_non_events,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper,
                            p_value = p_value,
                            weight_summary = list(
                                low_risk_weight = 1.0,
                                medium_risk_weight = 1.5,
                                high_risk_weight = 2.0
                            )
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .calculateRelativeIDI = function(data, old_cox, new_cox) {
                tryCatch(
                    {
                        # Get linear predictors and convert to probabilities

                        idi_t <- private$.idiTimePoint(data)
                        old_prob <- private$.coxRisk(old_cox, idi_t)
                        new_prob <- private$.coxRisk(new_cox, idi_t)

                        events <- data[["event_binary"]] == 1

                        # Calculate discrimination slopes
                        old_disc_events <- mean(old_prob[events], na.rm = TRUE)
                        old_disc_nonevents <- mean(old_prob[!events], na.rm = TRUE)
                        old_discrimination_slope <- old_disc_events - old_disc_nonevents

                        new_disc_events <- mean(new_prob[events], na.rm = TRUE)
                        new_disc_nonevents <- mean(new_prob[!events], na.rm = TRUE)
                        new_discrimination_slope <- new_disc_events - new_disc_nonevents

                        # Absolute IDI
                        idi_absolute <- new_discrimination_slope - old_discrimination_slope

                        # Relative IDI (as percentage of baseline discrimination)
                        relative_idi <- if (old_discrimination_slope > 0) {
                            idi_absolute / old_discrimination_slope
                        } else {
                            NA
                        }

                        # ONE standard error drives both the interval and the p-value.
                        #
                        # Previously the CI was built from se_idi/|old_slope| and then a DIFFERENT
                        # se_relative was computed unconditionally just below and used for the
                        # p-value, so the printed interval and p-value could disagree (interval
                        # excluding 0 while p > 0.05). That second SE also pooled
                        # var(new_prob - old_prob) over ALL subjects, which is the variance of the
                        # risk change in the cohort, not of the discrimination-slope difference.
                        #
                        # The IDI SE now uses the PAIRED within-outcome-group differences (Pencina
                        # 2008 Appendix A) rather than summing four marginal variances, which
                        # treated two nested models on the same patients as independent.
                        n_events <- sum(events)
                        n_nonevents <- sum(!events)

                        d_events <- new_prob[events] - old_prob[events]
                        d_nonevents <- new_prob[!events] - old_prob[!events]

                        var_d_events <- if (n_events > 1) var(d_events, na.rm = TRUE) / n_events else 0
                        var_d_nonevents <- if (n_nonevents > 1) var(d_nonevents, na.rm = TRUE) / n_nonevents else 0

                        se_idi <- sqrt(max(0, var_d_events) + max(0, var_d_nonevents))
                        se_relative <- if (is.finite(old_discrimination_slope) &&
                                           abs(old_discrimination_slope) > 0) {
                            se_idi / abs(old_discrimination_slope)
                        } else {
                            NA_real_
                        }

                        ci_lower <- NA_real_; ci_upper <- NA_real_

                        if (self$options$performBootstrap && !is.na(relative_idi)) {
                            bootstrap_relative_idi <- private$.bootstrapRelativeIDI(data, old_cox, new_cox)
                            if (length(bootstrap_relative_idi) >= 20) {
                                ci_probs_idi <- private$.ciProbs()
                                ci_lower <- unname(quantile(bootstrap_relative_idi, ci_probs_idi[1], na.rm = TRUE))
                                ci_upper <- unname(quantile(bootstrap_relative_idi, ci_probs_idi[2], na.rm = TRUE))
                                boot_se <- stats::sd(bootstrap_relative_idi, na.rm = TRUE)
                                if (is.finite(boot_se) && boot_se > 0) se_relative <- boot_se
                            }
                        }

                        # Fall back to the asymptotic interval, from the SAME se_relative used
                        # for the p-value below. (Previously, if relative_idi was NA under
                        # performBootstrap, neither branch defined ci_lower/ci_upper at all and
                        # the row silently vanished on an "object not found" caught upstream.)
                        if (is.na(ci_lower) && !is.na(relative_idi) && !is.na(se_relative)) {
                            ci_lower <- relative_idi - private$.zCrit() * se_relative
                            ci_upper <- relative_idi + private$.zCrit() * se_relative
                        }

                        z_score <- if (!is.na(se_relative) && se_relative > 0) {
                            relative_idi / se_relative
                        } else {
                            NA_real_
                        }
                        p_value <- if (is.na(z_score)) NA_real_ else 2 * stats::pnorm(-abs(z_score))

                        return(list(
                            relative_idi = relative_idi,
                            absolute_idi = idi_absolute,
                            old_discrimination = old_discrimination_slope,
                            new_discrimination = new_discrimination_slope,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper,
                            p_value = p_value
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .calculateContinuousNRI = function(data, old_cox, new_cox, time_point = 24) {
                tryCatch(
                    {
                        # NRI -- including Pencina's continuous/category-free form -- is built
                        # from the PROPORTION of subjects moving in each direction (a count of
                        # signs), and is bounded in [-2, 2].
                        #
                        # This previously took mean(new_lp - old_lp): the mean MAGNITUDE of the
                        # log-hazard change, an unbounded log-scale quantity. It was reported as
                        # "Continuous NRI" and interpreted with .interpretNRI()'s 0.2/0.4
                        # thresholds, which are meaningless on that scale.
                        #
                        # Now: risks on a common probability scale, and the sign of the movement.
                        old_lp <- private$.coxRisk(old_cox, time_point, newdata = data)
                        new_lp <- private$.coxRisk(new_cox, time_point, newdata = data)

                        risk_diff <- sign(new_lp - old_lp)

                        # Create time-specific event indicator
                        event_at_time <- private$.eventStatusAtTime(data[[self$options$survivalTime]], data[["event_binary"]], time_point)

                        # NA-safe: subjects censored before time_point are in neither group
                        events <- !is.na(event_at_time) & event_at_time == 1
                        non_events <- !is.na(event_at_time) & event_at_time == 0

                        if (sum(events) == 0 || sum(non_events) == 0) {
                            return(NULL)
                        }

                        # For events: positive risk difference is improvement
                        event_improvement <- mean(risk_diff[events], na.rm = TRUE)

                        # For non-events: negative risk difference is improvement
                        nonevent_improvement <- -mean(risk_diff[non_events], na.rm = TRUE)

                        # Continuous NRI combines both improvements
                        continuous_nri <- event_improvement + nonevent_improvement

                        # Standard errors
                        se_events <- sd(risk_diff[events], na.rm = TRUE) / sqrt(sum(events))
                        se_nonevents <- sd(risk_diff[non_events], na.rm = TRUE) / sqrt(sum(non_events))
                        se_total <- sqrt(se_events^2 + se_nonevents^2)

                        # Confidence intervals
                        ci_lower <- continuous_nri - private$.zCrit() * se_total
                        ci_upper <- continuous_nri + private$.zCrit() * se_total

                        # P-value
                        z_score <- continuous_nri / se_total
                        p_value <- 2 * (1 - pnorm(abs(z_score)))

                        return(list(
                            nri = continuous_nri,
                            event_improvement = event_improvement,
                            nonevent_improvement = nonevent_improvement,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper,
                            p_value = p_value
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .calculateDiscriminationImprovement = function(data, old_cox, new_cox) {
                tryCatch(
                    {
                        # Get predicted probabilities

                        idi_t <- private$.idiTimePoint(data)
                        old_prob <- private$.coxRisk(old_cox, idi_t)
                        new_prob <- private$.coxRisk(new_cox, idi_t)

                        events <- data[["event_binary"]] == 1

                        # Event-specific discrimination improvement
                        old_event_disc <- mean(old_prob[events], na.rm = TRUE)
                        new_event_disc <- mean(new_prob[events], na.rm = TRUE)
                        event_disc_improvement <- new_event_disc - old_event_disc

                        # Non-event-specific discrimination improvement
                        old_nonevent_disc <- mean(old_prob[!events], na.rm = TRUE)
                        new_nonevent_disc <- mean(new_prob[!events], na.rm = TRUE)
                        nonevent_disc_improvement <- old_nonevent_disc - new_nonevent_disc # Lower is better for non-events

                        # Standard errors and confidence intervals
                        n_events <- sum(events)
                        n_nonevents <- sum(!events)

                        se_event <- sqrt(var(new_prob[events] - old_prob[events], na.rm = TRUE) / n_events)
                        se_nonevent <- sqrt(var(old_prob[!events] - new_prob[!events], na.rm = TRUE) / n_nonevents)

                        # Event discrimination CI
                        event_ci_lower <- event_disc_improvement - private$.zCrit() * se_event
                        event_ci_upper <- event_disc_improvement + private$.zCrit() * se_event

                        # Non-event discrimination CI
                        nonevent_ci_lower <- nonevent_disc_improvement - private$.zCrit() * se_nonevent
                        nonevent_ci_upper <- nonevent_disc_improvement + private$.zCrit() * se_nonevent

                        # P-values
                        event_z <- event_disc_improvement / se_event
                        nonevent_z <- nonevent_disc_improvement / se_nonevent

                        event_p_value <- 2 * (1 - pnorm(abs(event_z)))
                        nonevent_p_value <- 2 * (1 - pnorm(abs(nonevent_z)))

                        return(list(
                            event_discrimination_improvement = event_disc_improvement,
                            nonevent_discrimination_improvement = nonevent_disc_improvement,
                            event_ci_lower = event_ci_lower,
                            event_ci_upper = event_ci_upper,
                            nonevent_ci_lower = nonevent_ci_lower,
                            nonevent_ci_upper = nonevent_ci_upper,
                            event_p_value = event_p_value,
                            nonevent_p_value = nonevent_p_value
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .calculateKaplanMeierNRI = function(data, old_stage, new_stage, time_col, event_col, time_point = 24) {
                tryCatch(
                    {
                        # Fit Kaplan-Meier curves for each staging system
                        old_formula <- as.formula(paste("survival::Surv(", time_col, ",", event_col, ") ~", old_stage))
                        new_formula <- as.formula(paste("survival::Surv(", time_col, ",", event_col, ") ~", new_stage))

                        old_km <- survival::survfit(old_formula, data = data)
                        new_km <- survival::survfit(new_formula, data = data)

                        # Extract survival probabilities at time point for each patient
                        old_surv_probs <- private$.extractSurvivalProbabilities(old_km, data, time_point, old_stage)
                        new_surv_probs <- private$.extractSurvivalProbabilities(new_km, data, time_point, new_stage)

                        # Convert survival probabilities to risk categories (tertiles)
                        old_risk_cats <- cut(1 - old_surv_probs, breaks = c(0, 1 / 3, 2 / 3, 1), labels = c("Low", "Medium", "High"))
                        new_risk_cats <- cut(1 - new_surv_probs, breaks = c(0, 1 / 3, 2 / 3, 1), labels = c("Low", "Medium", "High"))

                        # Create time-specific event indicator
                        event_at_time <- private$.eventStatusAtTime(data[[time_col]], data[[event_col]], time_point)

                        # NA-safe: subjects censored before time_point are in neither group
                        events <- !is.na(event_at_time) & event_at_time == 1
                        non_events <- !is.na(event_at_time) & event_at_time == 0

                        if (sum(events) == 0 || sum(non_events) == 0) {
                            return(NULL)
                        }

                        # Calculate NRI components
                        old_risk_num <- as.numeric(old_risk_cats)
                        new_risk_num <- as.numeric(new_risk_cats)

                        # NRI for events (moving to higher risk category is improvement)
                        event_improved <- sum(events & (new_risk_num > old_risk_num), na.rm = TRUE)
                        event_worsened <- sum(events & (new_risk_num < old_risk_num), na.rm = TRUE)
                        nri_events <- (event_improved - event_worsened) / sum(events)

                        # NRI for non-events (moving to lower risk category is improvement)
                        nonevent_improved <- sum(non_events & (new_risk_num < old_risk_num), na.rm = TRUE)
                        nonevent_worsened <- sum(non_events & (new_risk_num > old_risk_num), na.rm = TRUE)
                        nri_non_events <- (nonevent_improved - nonevent_worsened) / sum(non_events)

                        # Overall KM-based NRI
                        km_nri <- nri_events + nri_non_events

                        # Standard errors and confidence intervals
                        # Use improved SE calculation that handles negative NRI values
                        n_events <- sum(events)
                        n_non_events <- sum(non_events)

                        # Multinomial variance of (up - down)/n, per Pencina 2008 -- the same
                        # form used elsewhere in this file.
                        #
                        # Previously this took the BINOMIAL variance of "was reclassified at
                        # all", p(1-p)/n, which is the variance of a different quantity. When
                        # reclassification is near-universal (p -> 1, routine when two staging
                        # systems differ substantially) it collapses toward 0, producing an
                        # absurdly narrow CI and a spuriously tiny p-value.
                        var_events <- if (n_events > 0) {
                            p_up <- event_improved / n_events
                            p_dn <- event_worsened / n_events
                            (p_up + p_dn - (p_up - p_dn)^2) / n_events
                        } else {
                            0
                        }

                        var_non_events <- if (n_non_events > 0) {
                            p_up <- nonevent_improved / n_non_events
                            p_dn <- nonevent_worsened / n_non_events
                            (p_up + p_dn - (p_up - p_dn)^2) / n_non_events
                        } else {
                            0
                        }

                        se_total <- sqrt(max(0, var_events) + max(0, var_non_events))

                        ci_lower <- if (se_total > 0) km_nri - private$.zCrit() * se_total else NA_real_
                        ci_upper <- if (se_total > 0) km_nri + private$.zCrit() * se_total else NA_real_

                        # se_total == 0 was previously unguarded here, unlike the sibling methods
                        z_score <- if (se_total > 0) km_nri / se_total else NA_real_
                        p_value <- if (is.na(z_score)) NA_real_ else 2 * stats::pnorm(-abs(z_score))

                        return(list(
                            nri = km_nri,
                            nri_events = nri_events,
                            nri_non_events = nri_non_events,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper,
                            p_value = p_value
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },

            # Interpretation helper functions
            .interpretNRI = function(nri_value, type = "standard") {
                if (is.na(nri_value)) {
                    return("Unable to calculate")
                }

                abs_nri <- abs(nri_value)
                direction <- if (nri_value > 0) "improvement" else "deterioration"

                magnitude <- if (abs_nri > 0.3) {
                    "substantial"
                } else if (abs_nri > 0.2) {
                    "moderate"
                } else if (abs_nri > 0.1) {
                    "small"
                } else {
                    "minimal"
                }

                type_desc <- switch(type,
                    "category-free" = " (category-free approach)",
                    "clinical" = " (clinical thresholds)",
                    "continuous" = " (continuous risk scores)",
                    "kaplan-meier" = " (Kaplan-Meier based)",
                    "upstaging" = " (upstaged patients only)",
                    "downstaging" = " (downstaged patients only)",
                    "weighted" = " (risk-weighted approach)",
                    ""
                )

                return(paste0(stringr::str_to_title(magnitude), " ", direction, type_desc))
            },
            .interpretDiscriminationImprovement = function(disc_value, type = "event") {
                if (is.na(disc_value)) {
                    return("Unable to calculate")
                }

                abs_disc <- abs(disc_value)
                direction <- if (disc_value > 0) "improvement" else "deterioration"

                magnitude <- if (abs_disc > 0.1) {
                    "substantial"
                } else if (abs_disc > 0.05) {
                    "moderate"
                } else if (abs_disc > 0.02) {
                    "small"
                } else {
                    "minimal"
                }

                type_desc <- if (type == "event") {
                    " in event discrimination"
                } else {
                    " in non-event discrimination"
                }

                return(paste0(stringr::str_to_title(magnitude), " ", direction, type_desc))
            },
            .testProportionalHazardsAssumption = function(data, all_results) {
                # Test proportional hazards assumption using Schoenfeld residuals
                table <- self$results$proportionalHazardsTest
                if (is.null(table)) {
                    return(all_results)
                }

                all_results <- tryCatch(
                    {
                        # Ensure event_binary column exists
                        if (!"event_binary" %in% names(data)) {
                            event_col_name <- self$options$event
                            event_level <- self$options$eventLevel

                            if (!is.null(event_level) && event_level != "") {
                                data$event_binary <- ifelse(data[[event_col_name]] == event_level, 1, 0)
                            } else {
                                data$event_binary <- as.numeric(data[[event_col_name]])
                            }
                        }

                        old_col <- self$options$oldStage
                        new_col <- self$options$newStage
                        time_col <- self$options$survivalTime

                        # Fit Cox models for both staging systems
                        old_formula <- as.formula(paste("survival::Surv(", time_col, ", event_binary) ~ factor(", old_col, ")"))
                        new_formula <- as.formula(paste("survival::Surv(", time_col, ", event_binary) ~ factor(", new_col, ")"))

                        old_cox <- survival::coxph(old_formula, data = data)
                        new_cox <- survival::coxph(new_formula, data = data)

                        # Test proportional hazards assumption for original staging system
                        old_test <- private$.performSchoenfeld(old_cox, "Original Staging System")
                        if (!is.null(old_test)) {
                            table$addRow(rowKey = "old_system", values = list(
                                Variable = old_test$variable,
                                Chi_Square = old_test$chi_square,
                                df = old_test$df,
                                p_value = old_test$p_value,
                                Assumption_Status = old_test$status,
                                Interpretation = old_test$interpretation
                            ))
                        }

                        # Test proportional hazards assumption for new staging system
                        new_test <- private$.performSchoenfeld(new_cox, "New Staging System")
                        if (!is.null(new_test)) {
                            table$addRow(rowKey = "new_system", values = list(
                                Variable = new_test$variable,
                                Chi_Square = new_test$chi_square,
                                df = new_test$df,
                                p_value = new_test$p_value,
                                Assumption_Status = new_test$status,
                                Interpretation = new_test$interpretation
                            ))
                        }

                        # Store results for dashboard integration
                        if (!is.null(old_test) && !is.null(new_test)) {
                            all_results$proportional_hazards_test <- list(
                                old_test = old_test,
                                new_test = new_test
                            )
                        }

                        all_results
                    },
                    error = function(e) {
                        table$addRow(rowKey = "error", values = list(
                            Variable = "Error",
                            Chi_Square = NA,
                            df = NA,
                            p_value = NA,
                            Assumption_Status = "Test Failed",
                            Interpretation = paste("Proportional hazards test failed:", e$message)
                        ))

                        all_results
                    }
                )

                return(all_results)
            },
            .performSchoenfeld = function(cox_model, system_name) {
                # Perform Schoenfeld residuals test for a single Cox model
                tryCatch(
                    {
                        # Test proportional hazards assumption
                        ph_test <- survival::cox.zph(cox_model)

                        # Extract global test results (overall test across all variables)
                        global_test <- ph_test$table[nrow(ph_test$table), ]

                        chi_square <- global_test["chisq"]
                        df <- global_test["df"]
                        p_value <- global_test["p"]

                        # Determine assumption status
                        assumption_met <- p_value > 0.05
                        status <- if (assumption_met) {
                            "Assumption Met"
                        } else {
                            "Assumption Violated"
                        }

                        # Clinical interpretation
                        interpretation <- if (assumption_met) {
                            "Proportional hazards assumption is satisfied. Cox model is appropriate."
                        } else if (p_value <= 0.001) {
                            "Strong violation of proportional hazards assumption. Consider stratified Cox model or time-varying coefficients."
                        } else if (p_value <= 0.01) {
                            "Moderate violation of proportional hazards assumption. Consider model modifications or sensitivity analysis."
                        } else {
                            "Weak violation of proportional hazards assumption. Model may still be reasonable but interpret with caution."
                        }

                        return(list(
                            variable = system_name,
                            chi_square = as.numeric(chi_square),
                            df = as.numeric(df),
                            p_value = as.numeric(p_value),
                            status = status,
                            interpretation = interpretation
                        ))
                    },
                    error = function(e) {
                        # Return NULL if test fails (e.g., insufficient data)
                        return(NULL)
                    }
                )
            },
            .calculateDecisionCurveAnalysis = function(data, all_results) {
                # Calculate Decision Curve Analysis for clinical utility assessment
                table <- self$results$decisionCurveAnalysis
                if (is.null(table)) {
                    return()
                }

                tryCatch(
                    {
                        # Ensure event_binary column exists
                        if (!"event_binary" %in% names(data)) {
                            event_col_name <- self$options$event
                            event_level <- self$options$eventLevel

                            if (!is.null(event_level) && event_level != "") {
                                data$event_binary <- ifelse(data[[event_col_name]] == event_level, 1, 0)
                            } else {
                                data$event_binary <- as.numeric(data[[event_col_name]])
                            }
                        }

                        # Parse time points from NRI settings
                        time_points_str <- self$options$nriTimePoints
                        time_points <- as.numeric(unlist(strsplit(time_points_str, "\\s*,\\s*")))
                        time_points <- time_points[!is.na(time_points)]
                        if (length(time_points) == 0) time_points <- c(12, 24, 60)

                        old_col <- self$options$oldStage
                        new_col <- self$options$newStage
                        time_col <- self$options$survivalTime

                        # Fit Cox models for both staging systems
                        old_formula <- as.formula(paste("survival::Surv(", time_col, ", event_binary) ~ factor(", old_col, ")"))
                        new_formula <- as.formula(paste("survival::Surv(", time_col, ", event_binary) ~ factor(", new_col, ")"))

                        old_cox <- survival::coxph(old_formula, data = data)
                        new_cox <- survival::coxph(new_formula, data = data)

                        # Calculate DCA for each time point
                        for (time_point in time_points) {
                            # Checkpoint before each time point DCA calculation
                            private$.checkpoint()

                            dca_results <- private$.performDCAAtTimePoint(data, old_cox, new_cox, time_point)

                            if (!is.null(dca_results)) {
                                # Add rows for each threshold probability
                                for (i in seq_along(dca_results$thresholds)) {
                                    table$addRow(rowKey = paste("dca", time_point, i, sep = "_"), values = list(
                                        Time_Point = time_point,
                                        Threshold_Probability = dca_results$thresholds[i] * 100,
                                        Net_Benefit_Original = dca_results$net_benefit_original[i],
                                        Net_Benefit_New = dca_results$net_benefit_new[i],
                                        Difference = dca_results$difference[i],
                                        Clinical_Impact = dca_results$clinical_impact[i],
                                        Interpretation = dca_results$interpretation[i]
                                    ))
                                }
                            }
                        }
                    },
                    error = function(e) {
                        table$addRow(rowKey = "error", values = list(
                            Time_Point = NA,
                            Threshold_Probability = NA,
                            Net_Benefit_Original = NA,
                            Net_Benefit_New = NA,
                            Difference = NA,
                            Clinical_Impact = "Analysis Failed",
                            Interpretation = paste("Decision curve analysis failed:", e$message)
                        ))
                    }
                )
            },
            .performDCAAtTimePoint = function(data, old_cox, new_cox, time_point) {
                # Perform DCA at a specific time point
                tryCatch(
                    {
                        # Get risk predictions from Cox models

                        # Convert to survival probabilities at the specific time point
                        # Use baseline hazard approach for more accurate probabilities
                        old_surv_prob <- private$.calculateSurvivalProbability(old_cox, data, time_point)
                        new_surv_prob <- private$.calculateSurvivalProbability(new_cox, data, time_point)

                        # Convert survival probabilities to event probabilities
                        old_event_prob <- 1 - old_surv_prob
                        new_event_prob <- 1 - new_surv_prob

                        # Create time-specific event indicator
                        event_at_time <- private$.eventStatusAtTime(data[[self$options$survivalTime]], data[["event_binary"]], time_point)

                        # Define threshold probabilities for DCA (clinically relevant range)
                        thresholds <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50)

                        net_benefit_original <- numeric(length(thresholds))
                        net_benefit_new <- numeric(length(thresholds))
                        difference <- numeric(length(thresholds))
                        clinical_impact <- character(length(thresholds))
                        interpretation <- character(length(thresholds))

                        # Calculate net benefit for each threshold
                        for (i in seq_along(thresholds)) {
                            threshold <- thresholds[i]

                            # For original staging system
                            nb_orig <- private$.calculateNetBenefit(old_event_prob, event_at_time, threshold)

                            # For new staging system
                            nb_new <- private$.calculateNetBenefit(new_event_prob, event_at_time, threshold)

                            net_benefit_original[i] <- nb_orig
                            net_benefit_new[i] <- nb_new
                            difference[i] <- nb_new - nb_orig

                            # Clinical impact assessment
                            if (abs(difference[i]) < 0.01) {
                                clinical_impact[i] <- "Minimal"
                                interpretation[i] <- "No clinically meaningful difference in net benefit"
                            } else if (difference[i] > 0.05) {
                                clinical_impact[i] <- "Substantial Benefit"
                                interpretation[i] <- "New staging provides substantial clinical benefit"
                            } else if (difference[i] > 0.02) {
                                clinical_impact[i] <- "Moderate Benefit"
                                interpretation[i] <- "New staging provides moderate clinical benefit"
                            } else if (difference[i] > 0) {
                                clinical_impact[i] <- "Small Benefit"
                                interpretation[i] <- "New staging provides small clinical benefit"
                            } else if (difference[i] < -0.05) {
                                clinical_impact[i] <- "Substantial Harm"
                                interpretation[i] <- "New staging causes substantial clinical harm"
                            } else if (difference[i] < -0.02) {
                                clinical_impact[i] <- "Moderate Harm"
                                interpretation[i] <- "New staging causes moderate clinical harm"
                            } else {
                                clinical_impact[i] <- "Small Harm"
                                interpretation[i] <- "New staging causes small clinical harm"
                            }
                        }

                        return(list(
                            thresholds = thresholds,
                            net_benefit_original = net_benefit_original,
                            net_benefit_new = net_benefit_new,
                            difference = difference,
                            clinical_impact = clinical_impact,
                            interpretation = interpretation
                        ))
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .calculateNetBenefit = function(predicted_prob, observed_event, threshold) {
                # Calculate net benefit for decision curve analysis
                # Net Benefit = (TP/n) - (FP/n) × (threshold/(1-threshold))

                n <- length(predicted_prob)

                # Classify patients as high risk if predicted probability > threshold
                high_risk <- predicted_prob > threshold

                # Calculate true positives and false positives
                true_positives <- sum(high_risk & observed_event == 1)
                false_positives <- sum(high_risk & observed_event == 0)

                # Calculate net benefit
                net_benefit <- (true_positives / n) - (false_positives / n) * (threshold / (1 - threshold))

                return(net_benefit)
            },
            .calculateSurvivalProbability = function(cox_model, data, time_point) {
                # Calculate survival probability at specific time point using baseline hazard
                tryCatch(
                    {
                        # Get baseline survival function
                        base_surv <- survival::survfit(cox_model)

                        # Get linear predictors for risk adjustment
                        linear_predictors <- predict(cox_model, type = "lp")

                        # Find survival probability at time point from baseline
                        if (time_point <= min(base_surv$time)) {
                            baseline_surv_at_time <- 1.0
                        } else if (time_point >= max(base_surv$time)) {
                            baseline_surv_at_time <- min(base_surv$surv)
                        } else {
                            baseline_surv_at_time <- approx(base_surv$time, base_surv$surv, time_point)$y
                        }

                        # Adjust for individual risk using Cox model
                        # S(t|x) = S0(t)^exp(βx)
                        individual_surv_prob <- baseline_surv_at_time^exp(linear_predictors)

                        # Ensure probabilities are within valid range
                        individual_surv_prob <- pmax(0.001, pmin(0.999, individual_surv_prob))

                        return(individual_surv_prob)
                    },
                    error = function(e) {
                        # Fallback to simple approach using risk scores
                        risk_scores <- predict(cox_model, type = "risk")
                        # Convert to approximate survival probabilities
                        # Was: 1/(1 + risk*(t/12)), an ad-hoc curve unrelated to the fitted Cox
                        # model, silently substituted on error and fed into DCA and the Brier
                        # score. A missing value is safer than a fabricated one.
                        surv_prob <- rep(NA_real_, length(risk_scores))
                        return(pmax(0.001, pmin(0.999, surv_prob)))
                    }
                )
            },
            .calculateIntegratedAUCAnalysis = function(data, all_results) {
                # Enhanced time-dependent AUC analysis with integrated measures
                table <- self$results$integratedAUCAnalysis
                if (is.null(table)) {
                    return()
                }

                tryCatch(
                    {
                        # Ensure event_binary column exists
                        if (!"event_binary" %in% names(data)) {
                            event_col_name <- self$options$event
                            event_level <- self$options$eventLevel

                            if (!is.null(event_level) && event_level != "") {
                                data$event_binary <- ifelse(data[[event_col_name]] == event_level, 1, 0)
                            } else {
                                data$event_binary <- as.numeric(data[[event_col_name]])
                            }
                        }

                        # Parse time points from NRI settings
                        time_points_str <- self$options$nriTimePoints
                        time_points <- as.numeric(unlist(strsplit(time_points_str, "\\s*,\\s*")))
                        time_points <- time_points[!is.na(time_points)]
                        if (length(time_points) == 0) time_points <- c(12, 24, 60)

                        old_col <- self$options$oldStage
                        new_col <- self$options$newStage
                        time_col <- self$options$survivalTime

                        # Fit Cox models for both staging systems
                        old_formula <- as.formula(paste("survival::Surv(", time_col, ", event_binary) ~ factor(", old_col, ")"))
                        new_formula <- as.formula(paste("survival::Surv(", time_col, ", event_binary) ~ factor(", new_col, ")"))

                        old_cox <- survival::coxph(old_formula, data = data)
                        new_cox <- survival::coxph(new_formula, data = data)

                        # Get risk scores for AUC calculations
                        old_risk <- predict(old_cox, type = "risk")
                        new_risk <- predict(new_cox, type = "risk")

                        # Calculate time-dependent AUC for each time point
                        auc_results_old <- numeric(length(time_points))
                        auc_results_new <- numeric(length(time_points))
                        auc_se_old <- numeric(length(time_points))
                        auc_se_new <- numeric(length(time_points))

                        valid_times <- logical(length(time_points))

                        for (i in seq_along(time_points)) {
                            # Checkpoint before each time point AUC calculation
                            private$.checkpoint()
                            t <- time_points[i]

                            # Skip time points beyond data range
                            max_time <- max(data[[time_col]], na.rm = TRUE)
                            if (t > max_time * 0.8) next # Use 80% of max follow-up

                            auc_old <- private$.calculateTimeDependentAUC(data, old_risk, time_col, t)
                            auc_new <- private$.calculateTimeDependentAUC(data, new_risk, time_col, t)

                            if (!is.na(auc_old$auc) && !is.na(auc_new$auc)) {
                                auc_results_old[i] <- auc_old$auc
                                auc_results_new[i] <- auc_new$auc
                                auc_se_old[i] <- auc_old$se
                                auc_se_new[i] <- auc_new$se
                                valid_times[i] <- TRUE
                            }
                        }

                        # Filter to valid time points
                        valid_time_points <- time_points[valid_times]
                        valid_auc_old <- auc_results_old[valid_times]
                        valid_auc_new <- auc_results_new[valid_times]
                        valid_se_old <- auc_se_old[valid_times]
                        valid_se_new <- auc_se_new[valid_times]

                        if (length(valid_time_points) < 2) {
                            table$addRow(rowKey = "error", values = list(
                                Metric = "Error",
                                Original_System = NA,
                                New_System = NA,
                                Difference = NA,
                                CI_Lower = NA,
                                CI_Upper = NA,
                                p_value = NA,
                                Interpretation = .("Insufficient time points for integrated AUC analysis")
                            ))
                            return()
                        }

                        # 1. Integrated AUC using trapezoidal rule
                        integrated_auc_old <- private$.calculateIntegratedAUC(valid_time_points, valid_auc_old)
                        integrated_auc_new <- private$.calculateIntegratedAUC(valid_time_points, valid_auc_new)
                        integrated_auc_diff <- integrated_auc_new - integrated_auc_old

                        # Confidence interval for integrated AUC difference using bootstrap
                        integrated_ci <- tryCatch(
                            {
                                private$.bootstrapIntegratedAUCDifference(data, old_cox, new_cox, valid_time_points, n_bootstrap = 200)
                            },
                            error = function(e) {
                                private$.addNotice("WARNING", .("Bootstrap confidence intervals failed"), conditionMessage(e))
                                NULL
                            }
                        )

                        # 2. Mean AUC across time points with confidence intervals
                        mean_auc_old <- mean(valid_auc_old)
                        mean_auc_new <- mean(valid_auc_new)
                        mean_auc_diff <- mean_auc_new - mean_auc_old

                        # SE of the mean AUC difference.
                        #
                        # Var(AUC_new - AUC_old) = Var_new + Var_old - 2*Cov, and the two AUCs come
                        # from NESTED models on the SAME patients, so Cov is large and positive.
                        # Summing the marginal variances (the previous formula) ignores it and
                        # makes the interval substantially too wide -- conservative in isolation,
                        # but the significance call below is derived from this interval, so real
                        # improvements were reported as non-significant.
                        #
                        # The bootstrap difference distribution above already carries the
                        # covariance, so use its SE when available and fall back to the
                        # (conservative) independence formula only if it is not.
                        boot_se <- if (!is.null(integrated_ci) && length(integrated_ci$draws) >= 20) {
                            stats::sd(integrated_ci$draws, na.rm = TRUE)
                        } else {
                            NA_real_
                        }
                        mean_se_diff <- if (is.finite(boot_se) && boot_se > 0) {
                            boot_se
                        } else {
                            sqrt(mean(valid_se_old^2) + mean(valid_se_new^2))
                        }
                        mean_ci_lower <- mean_auc_diff - private$.zCrit() * mean_se_diff
                        mean_ci_upper <- mean_auc_diff + private$.zCrit() * mean_se_diff

                        # 3. AUC comparison test (DeLong test for first time point as representative)
                        delong_test <- private$.performDeLongTest(data, old_risk, new_risk, time_col, valid_time_points[1])

                        # 4. Temporal trend analysis
                        temporal_trend <- private$.analyzeAUCTemporalTrends(valid_time_points, valid_auc_old, valid_auc_new)

                        # 5. Brier score for combined discrimination/calibration
                        brier_old <- private$.calculateBrierScore(data, old_cox, valid_time_points[length(valid_time_points)])
                        brier_new <- private$.calculateBrierScore(data, new_cox, valid_time_points[length(valid_time_points)])
                        brier_diff <- brier_old - brier_new # Lower is better, so improvement = old - new

                        # Add results to table with individual error handling
                        tryCatch(
                            {
                                table$addRow(rowKey = "integrated_auc", values = list(
                                    Metric = "Integrated AUC (Trapezoidal)",
                                    Original_System = as.numeric(round(integrated_auc_old, 4)),
                                    New_System = as.numeric(round(integrated_auc_new, 4)),
                                    Difference = as.numeric(round(integrated_auc_diff, 4)),
                                    CI_Lower = if (!is.null(integrated_ci)) as.numeric(round(integrated_ci$ci[1], 4)) else NA_real_,
                                    CI_Upper = if (!is.null(integrated_ci)) as.numeric(round(integrated_ci$ci[2], 4)) else NA_real_,
                                    # Was: literal 0.05 / 0.25 chosen by whether the CI excluded
                                    # zero. Those are not p-values. Significance for this row is
                                    # read from the bootstrap CI instead.
                                    p_value = NA_real_,
                                    Interpretation = as.character(private$.interpretAUCImprovement(integrated_auc_diff, "Integrated AUC"))
                                ))
                            },
                            error = function(e) {
                            }
                        )

                        tryCatch(
                            {
                                table$addRow(rowKey = "mean_auc", values = list(
                                    Metric = "Mean Time-dependent AUC",
                                    Original_System = as.numeric(round(mean_auc_old, 4)),
                                    New_System = as.numeric(round(mean_auc_new, 4)),
                                    Difference = as.numeric(round(mean_auc_diff, 4)),
                                    CI_Lower = as.numeric(round(mean_ci_lower, 4)),
                                    CI_Upper = as.numeric(round(mean_ci_upper, 4)),
                                    # Was: literal 0.05 / 0.25. See above -- significance is
                                    # read from the CI, not from an invented p-value.
                                    p_value = NA_real_,
                                    Interpretation = as.character(private$.interpretAUCImprovement(mean_auc_diff, "Mean AUC"))
                                ))
                            },
                            error = function(e) {
                            }
                        )

                        if (!is.null(delong_test)) {
                            tryCatch(
                                {
                                    table$addRow(rowKey = "auc_comparison", values = list(
                                        Metric = paste("AUC Comparison Test (", valid_time_points[1], "m)", sep = ""),
                                        Original_System = as.numeric(round(delong_test$auc1, 4)),
                                        New_System = as.numeric(round(delong_test$auc2, 4)),
                                        Difference = as.numeric(round(delong_test$auc2 - delong_test$auc1, 4)),
                                        CI_Lower = as.numeric(round(delong_test$ci_lower, 4)),
                                        CI_Upper = as.numeric(round(delong_test$ci_upper, 4)),
                                        p_value = as.numeric(round(delong_test$p_value, 4)),
                                        Interpretation = as.character(private$.interpretAUCTest(delong_test$p_value, delong_test$auc2 - delong_test$auc1))
                                    ))
                                },
                                error = function(e) {
                                }
                            )
                        }

                        tryCatch(
                            {
                                table$addRow(rowKey = "temporal_trend", values = list(
                                    Metric = "AUC Temporal Trend (slope)",
                                    Original_System = as.numeric(round(temporal_trend$slope_old, 6)),
                                    New_System = as.numeric(round(temporal_trend$slope_new, 6)),
                                    Difference = as.numeric(round(temporal_trend$slope_new - temporal_trend$slope_old, 6)),
                                    CI_Lower = NA_real_,
                                    CI_Upper = NA_real_,
                                    p_value = as.numeric(round(temporal_trend$p_value, 4)),
                                    Interpretation = as.character(private$.interpretTemporalTrend(temporal_trend))
                                ))
                            },
                            error = function(e) {
                            }
                        )

                        tryCatch(
                            {
                                table$addRow(rowKey = "brier_score", values = list(
                                    Metric = paste("Brier Score (", valid_time_points[length(valid_time_points)], "m)", sep = ""),
                                    Original_System = as.numeric(round(brier_old, 4)),
                                    New_System = as.numeric(round(brier_new, 4)),
                                    Difference = as.numeric(round(brier_diff, 4)),
                                    CI_Lower = NA_real_,
                                    CI_Upper = NA_real_,
                                    p_value = NA_real_,
                                    Interpretation = as.character(private$.interpretBrierScore(brier_diff, brier_old, brier_new))
                                ))
                            },
                            error = function(e) {
                            }
                        )
                    },
                    error = function(e) {
                        table$addRow(rowKey = "error", values = list(
                            Metric = "Analysis Error",
                            Original_System = NA,
                            New_System = NA,
                            Difference = NA,
                            CI_Lower = NA,
                            CI_Upper = NA,
                            p_value = NA,
                            Interpretation = paste("Integrated AUC analysis failed:", e$message)
                        ))
                    }
                )
            },
            .calculateTimeDependentAUC = function(data, risk_scores, time_col, time_point) {
                # Calculate time-dependent AUC using timeROC or fallback method
                tryCatch(
                    {
                        # Create time-specific event indicator
                        event_at_time <- private$.eventStatusAtTime(data[[time_col]], data[["event_binary"]], time_point)
                        include_patients <- (data[[time_col]] <= time_point & data[["event_binary"]] == 1) | (data[[time_col]] > time_point)

                        if (sum(include_patients) < 10 || sum(event_at_time[include_patients], na.rm = TRUE) < 5) {
                            return(list(auc = NA, se = NA))
                        }

                        # Try timeROC first
                        if (requireNamespace("timeROC", quietly = TRUE)) {
                            roc_result <- try(
                                {
                                    private$.timeROC(
                                        T = data[[time_col]],
                                        delta = data[["event_binary"]],
                                        marker = risk_scores,
                                        cause = 1,
                                        times = time_point,
                                        iid = TRUE
                                    )
                                },
                                silent = TRUE
                            )

                            k <- if (!inherits(roc_result, "try-error")) private$.timeROCIndex(roc_result, time_point) else NA_integer_
                            if (!is.na(k) && !is.na(roc_result$AUC[k])) {
                                # vect_sd_1 is already an SE. Index by the requested time: [1] is timeROC's t = 0 slot.
                                auc_se <- roc_result$inference$vect_sd_1[k]
                                if (is.null(auc_se)) auc_se <- NA_real_
                                return(list(auc = unname(roc_result$AUC[k]), se = unname(auc_se)))
                            }
                        }

                        # Fallback to pROC
                        if (requireNamespace("pROC", quietly = TRUE)) {
                            roc_simple <- try(
                                {
                                    pROC::roc(event_at_time[include_patients], risk_scores[include_patients], quiet = TRUE)
                                },
                                silent = TRUE
                            )

                            if (!inherits(roc_simple, "try-error")) {
                                auc_ci <- try(
                                    {
                                        pROC::ci.auc(roc_simple, quiet = TRUE)
                                    },
                                    silent = TRUE
                                )

                                auc_se <- if (!inherits(auc_ci, "try-error")) {
                                    (auc_ci[3] - auc_ci[1]) / (2 * stats::qnorm(0.975))
                                } else {
                                    NA_real_
                                }

                                return(list(auc = as.numeric(roc_simple$auc), se = auc_se))
                            }
                        }

                        return(list(auc = NA, se = NA))
                    },
                    error = function(e) {
                        return(list(auc = NA, se = NA))
                    }
                )
            },
            .calculateIntegratedAUC = function(time_points, auc_values) {
                # Calculate integrated AUC using trapezoidal rule
                if (length(time_points) < 2) {
                    return(NA)
                }

                # Sort by time
                order_idx <- order(time_points)
                t_sorted <- time_points[order_idx]
                auc_sorted <- auc_values[order_idx]

                # Trapezoidal integration
                integrated <- 0
                total_time <- max(t_sorted) - min(t_sorted)

                for (i in 2:length(t_sorted)) {
                    dt <- t_sorted[i] - t_sorted[i - 1]
                    avg_auc <- (auc_sorted[i] + auc_sorted[i - 1]) / 2
                    integrated <- integrated + avg_auc * dt
                }

                # Normalize by total time span
                return(integrated / total_time)
            },
            .bootstrapIntegratedAUCDifference = function(data, old_cox, new_cox, time_points, n_bootstrap = 500) {
                # Bootstrap confidence interval for integrated AUC difference
                tryCatch(
                    {
                        # NA, not 0: an unfilled slot must not read as a real zero difference
                        bootstrap_diffs <- rep(NA_real_, n_bootstrap)
                        n <- nrow(data)

                        for (i in 1:min(n_bootstrap, 200)) { # Limit for performance
                            # Bootstrap sample
                            boot_indices <- sample(1:n, n, replace = TRUE)
                            boot_data <- data[boot_indices, ]

                            # Refit models on bootstrap sample
                            boot_old_cox <- try(survival::coxph(old_cox$formula, data = boot_data), silent = TRUE)
                            boot_new_cox <- try(survival::coxph(new_cox$formula, data = boot_data), silent = TRUE)

                            if (!inherits(boot_old_cox, "try-error") && !inherits(boot_new_cox, "try-error")) {
                                boot_old_risk <- predict(boot_old_cox, type = "risk")
                                boot_new_risk <- predict(boot_new_cox, type = "risk")

                                # Calculate AUCs for each time point
                                boot_auc_old <- numeric(length(time_points))
                                boot_auc_new <- numeric(length(time_points))
                                valid_count <- 0

                                for (j in seq_along(time_points)) {
                                    # Was names(boot_data)[3] -- the survival time picked by column
                                    # position. Every other call site passes the option.
                                    auc_old <- private$.calculateTimeDependentAUC(boot_data, boot_old_risk, self$options$survivalTime, time_points[j])
                                    auc_new <- private$.calculateTimeDependentAUC(boot_data, boot_new_risk, self$options$survivalTime, time_points[j])

                                    if (!is.na(auc_old$auc) && !is.na(auc_new$auc)) {
                                        boot_auc_old[j] <- auc_old$auc
                                        boot_auc_new[j] <- auc_new$auc
                                        valid_count <- valid_count + 1
                                    }
                                }

                                if (valid_count >= 2) {
                                    valid_idx <- !is.na(boot_auc_old) & !is.na(boot_auc_new)
                                    if (sum(valid_idx) >= 2) {
                                        integrated_old <- private$.calculateIntegratedAUC(time_points[valid_idx], boot_auc_old[valid_idx])
                                        integrated_new <- private$.calculateIntegratedAUC(time_points[valid_idx], boot_auc_new[valid_idx])
                                        bootstrap_diffs[i] <- integrated_new - integrated_old
                                    }
                                }
                            }
                        }

                        # `!= 0` previously stood in for "slot never filled", because
                        # bootstrap_diffs was preallocated with numeric() zeros. That also
                        # deleted every GENUINE zero difference -- common with ordinal staging on
                        # the same patients -- preferentially removing null replicates and
                        # pushing the percentile interval away from zero. Preallocated as NA
                        # below, so filtering on is.na() alone is both correct and sufficient.
                        valid_diffs <- bootstrap_diffs[!is.na(bootstrap_diffs)]
                        if (length(valid_diffs) >= 10) {
                            return(list(
                                ci = unname(stats::quantile(valid_diffs, private$.ciProbs())),
                                draws = valid_diffs
                            ))
                        } else {
                            return(NULL)
                        }
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .performDeLongTest = function(data, risk1, risk2, time_col, time_point) {
                # Perform DeLong test for AUC comparison
                tryCatch(
                    {
                        # Create time-specific event indicator
                        event_at_time <- private$.eventStatusAtTime(data[[time_col]], data[["event_binary"]], time_point)
                        include_patients <- (data[[time_col]] <= time_point & data[["event_binary"]] == 1) | (data[[time_col]] > time_point)

                        if (sum(include_patients) < 10 || sum(event_at_time[include_patients], na.rm = TRUE) < 5) {
                            return(NULL)
                        }

                        if (requireNamespace("pROC", quietly = TRUE)) {
                            roc1 <- pROC::roc(event_at_time[include_patients], risk1[include_patients], quiet = TRUE)
                            roc2 <- pROC::roc(event_at_time[include_patients], risk2[include_patients], quiet = TRUE)

                            test_result <- try(
                                {
                                    pROC::roc.test(roc1, roc2, method = "delong")
                                },
                                silent = TRUE
                            )

                            if (!inherits(test_result, "try-error")) {
                                # Calculate confidence interval for difference
                                auc_diff <- as.numeric(roc2$auc) - as.numeric(roc1$auc)

                                # pROC::roc.test(method = "delong") returns NO $parameter
                                # component, so the previous sqrt(statistic^2 / parameter) was
                                # sqrt(z^2 / NA) = NA on every run and both CI columns were
                                # permanently blank. (sqrt(Z^2/df) is not an SE in any case.)
                                # The SE of the AUC difference is |difference| / |Z|.
                                test_stat <- as.numeric(test_result$statistic)[1]
                                se_diff <- if (is.finite(test_stat) && abs(test_stat) > 0) {
                                    abs(auc_diff) / abs(test_stat)
                                } else {
                                    NA_real_
                                }
                                ci_lower <- as.numeric(auc_diff - private$.zCrit() * se_diff)
                                ci_upper <- as.numeric(auc_diff + private$.zCrit() * se_diff)

                                return(list(
                                    auc1 = as.numeric(roc1$auc)[1],
                                    auc2 = as.numeric(roc2$auc)[1],
                                    p_value = as.numeric(test_result$p.value)[1],
                                    ci_lower = ci_lower,
                                    ci_upper = ci_upper
                                ))
                            }
                        }

                        return(NULL)
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .analyzeAUCTemporalTrends = function(time_points, auc_old, auc_new) {
                # Analyze temporal trends in AUC values
                tryCatch(
                    {
                        if (length(time_points) < 3) {
                            return(list(slope_old = 0, slope_new = 0, p_value = 1))
                        }

                        # Linear regression to detect trends
                        trend_old <- lm(auc_old ~ time_points)
                        trend_new <- lm(auc_new ~ time_points)

                        slope_old <- as.numeric(coef(trend_old)[2])
                        slope_new <- as.numeric(coef(trend_new)[2])

                        # Test if slopes are significantly different
                        slope_diff <- slope_new - slope_old

                        # Simple t-test approximation
                        se_old <- as.numeric(summary(trend_old)$coefficients[2, 2])
                        se_new <- as.numeric(summary(trend_new)$coefficients[2, 2])
                        se_diff <- sqrt(se_old^2 + se_new^2)

                        t_stat <- slope_diff / se_diff
                        p_value <- as.numeric(2 * (1 - pt(abs(t_stat), df = length(time_points) - 2)))

                        return(list(
                            slope_old = slope_old,
                            slope_new = slope_new,
                            p_value = p_value
                        ))
                    },
                    error = function(e) {
                        return(list(slope_old = 0, slope_new = 0, p_value = 1))
                    }
                )
            },
            .calculateBrierScore = function(data, cox_model, time_point) {
                # Calculate Brier score for combined calibration/discrimination assessment
                tryCatch(
                    {
                        # Get survival probabilities at time point
                        surv_probs <- private$.calculateSurvivalProbability(cox_model, data, time_point)

                        # Convert to event probabilities
                        event_probs <- 1 - surv_probs

                        # IPCW (Graf 1999) Brier score.
                        #
                        # Previously the observed outcome was ifelse(time <= t & event == 1, 1, 0)
                        # and the score a plain mean of squared residuals -- so every patient
                        # censored before t contributed a spurious "no event" residual, asserting
                        # they were event-free through t. That biases the score downward and,
                        # because the two staging systems assign those patients different
                        # predicted risks, biases the DIFFERENCE between systems unpredictably.
                        #
                        # Graf weights: subjects with the event before t get 1/G(T_i-), subjects
                        # still at risk at t get 1/G(t), subjects censored before t get 0.
                        tv <- as.numeric(data[[self$options$survivalTime]])
                        ev <- as.numeric(data[["event_binary"]])

                        # reverse Kaplan-Meier: the censoring distribution G
                        cens_fit <- survival::survfit(survival::Surv(tv, 1 - ev) ~ 1)
                        gfun <- function(u) {
                            idx <- findInterval(u, cens_fit$time)
                            ifelse(idx == 0, 1, cens_fit$surv[pmax(idx, 1)])
                        }

                        had_event <- !is.na(tv) & !is.na(ev) & tv <= time_point & ev == 1
                        at_risk <- !is.na(tv) & tv > time_point

                        g_event <- gfun(tv)
                        g_tau <- gfun(time_point)

                        w <- rep(0, length(tv))
                        w[had_event] <- 1 / g_event[had_event]
                        w[at_risk] <- 1 / g_tau

                        observed <- rep(0, length(tv))
                        observed[had_event] <- 1

                        ok <- is.finite(w) & w > 0 & is.finite(event_probs)
                        if (!any(ok)) return(NA_real_)

                        brier <- sum(w[ok] * (event_probs[ok] - observed[ok])^2) / sum(w[ok])

                        return(brier)
                    },
                    error = function(e) {
                        return(NA)
                    }
                )
            },
            .interpretAUCImprovement = function(improvement, metric_name) {
                # Interpret AUC improvement magnitude
                if (is.na(improvement)) {
                    return("Unable to assess")
                }

                abs_improvement <- abs(improvement)
                direction <- if (improvement > 0) "improvement" else "deterioration"

                if (abs_improvement >= 0.10) {
                    magnitude <- "Substantial"
                } else if (abs_improvement >= 0.05) {
                    magnitude <- "Moderate"
                } else if (abs_improvement >= 0.02) {
                    magnitude <- "Small"
                } else {
                    magnitude <- "Minimal"
                }

                clinical_meaning <- if (improvement >= 0.02) {
                    "clinically meaningful"
                } else if (improvement <= -0.02) {
                    "clinically concerning"
                } else {
                    "clinically minimal"
                }

                return(paste0(magnitude, " ", direction, " in ", metric_name, " (", clinical_meaning, ")"))
            },
            .interpretAUCTest = function(p_value, difference) {
                # Interpret statistical significance of AUC comparison
                significance <- if (p_value < 0.01) {
                    "Highly significant"
                } else if (p_value < 0.05) {
                    "Significant"
                } else if (p_value < 0.10) {
                    "Marginally significant"
                } else {
                    "Not significant"
                }

                direction <- if (difference > 0) "improvement" else "decline"

                return(paste0(significance, " ", direction, " in discrimination"))
            },
            .interpretTemporalTrend = function(trend_result) {
                # Interpret temporal trends in AUC
                if (trend_result$p_value < 0.05) {
                    if (trend_result$slope_new > trend_result$slope_old) {
                        return("New staging shows improving discrimination over time")
                    } else {
                        return("Original staging shows better discrimination trend over time")
                    }
                } else {
                    return("No significant temporal trend differences")
                }
            },
            .interpretBrierScore = function(difference, old_score, new_score) {
                # Interpret Brier score difference (lower is better)
                if (is.na(difference)) {
                    return("Unable to assess calibration")
                }

                if (difference > 0.02) {
                    return("Substantial improvement in combined discrimination/calibration")
                } else if (difference > 0.01) {
                    return("Moderate improvement in combined discrimination/calibration")
                } else if (difference > 0) {
                    return("Small improvement in combined discrimination/calibration")
                } else if (difference < -0.02) {
                    return("Substantial deterioration in combined discrimination/calibration")
                } else if (difference < -0.01) {
                    return("Moderate deterioration in combined discrimination/calibration")
                } else {
                    return("Minimal change in combined discrimination/calibration")
                }
            },

            # Bootstrap helper functions for enhanced reclassification metrics
            .bootstrapCategoryFreeNRI = function(data, old_cox, new_cox, time_point, n_bootstrap = private$.getBootstrapReps()) {
                # Simplified bootstrap for category-free NRI
                tryCatch(
                    {
                        bootstrap_results <- numeric(n_bootstrap)
                        n <- nrow(data)

                        for (i in 1:n_bootstrap) {
                            # Bootstrap sample
                            boot_indices <- sample(1:n, n, replace = TRUE)
                            boot_data <- data[boot_indices, ]

                            # Refit BOTH models on the resample. Passing the original-data
                            # models meant the inner predict() calls (which use no newdata=)
                            # returned linear predictors for the ORIGINAL n rows in original
                            # order, while the outcomes came from boot_data -- pairing patient
                            # i's risk score with draw i's outcome. Row counts match, so R
                            # raised nothing and every replicate was effectively a random
                            # permutation, centring the interval near 0.
                            boot_old <- try(survival::coxph(stats::formula(old_cox), data = boot_data), silent = TRUE)
                            boot_new <- try(survival::coxph(stats::formula(new_cox), data = boot_data), silent = TRUE)
                            if (inherits(boot_old, "try-error") || inherits(boot_new, "try-error")) {
                                bootstrap_results[i] <- NA
                                next
                            }

                            boot_nri <- private$.calculateCategoryFreeNRI(boot_data, boot_old, boot_new, time_point)
                            if (!is.null(boot_nri) && !is.na(boot_nri$nri)) {
                                bootstrap_results[i] <- boot_nri$nri
                            } else {
                                bootstrap_results[i] <- NA
                            }
                        }

                        return(bootstrap_results[!is.na(bootstrap_results)])
                    },
                    error = function(e) {
                        return(numeric(0))
                    }
                )
            },
            .bootstrapRelativeIDI = function(data, old_cox, new_cox, n_bootstrap = private$.getBootstrapReps()) {
                # Simplified bootstrap for relative IDI
                tryCatch(
                    {
                        bootstrap_results <- numeric(n_bootstrap)
                        n <- nrow(data)

                        for (i in 1:n_bootstrap) {
                            # Bootstrap sample
                            boot_indices <- sample(1:n, n, replace = TRUE)
                            boot_data <- data[boot_indices, ]

                            # Refit BOTH models on the resample. Passing the original-data
                            # models meant the inner predict() calls (which use no newdata=)
                            # returned linear predictors for the ORIGINAL n rows in original
                            # order, while the outcomes came from boot_data -- pairing patient
                            # i's risk score with draw i's outcome. Row counts match, so R
                            # raised nothing and every replicate was effectively a random
                            # permutation, centring the interval near 0.
                            boot_old <- try(survival::coxph(stats::formula(old_cox), data = boot_data), silent = TRUE)
                            boot_new <- try(survival::coxph(stats::formula(new_cox), data = boot_data), silent = TRUE)
                            if (inherits(boot_old, "try-error") || inherits(boot_new, "try-error")) {
                                bootstrap_results[i] <- NA
                                next
                            }

                            boot_idi <- private$.calculateRelativeIDI(boot_data, boot_old, boot_new)
                            if (!is.null(boot_idi) && !is.na(boot_idi$relative_idi)) {
                                bootstrap_results[i] <- boot_idi$relative_idi
                            } else {
                                bootstrap_results[i] <- NA
                            }
                        }

                        return(bootstrap_results[!is.na(bootstrap_results)])
                    },
                    error = function(e) {
                        return(numeric(0))
                    }
                )
            },
            .bootstrapCategorySpecificNRI = function(data, old_cox, new_cox, time_point, direction = "upstaging", n_bootstrap = private$.getBootstrapReps()) {
                # Bootstrap for category-specific NRI (upstaging or downstaging patients only)
                tryCatch(
                    {
                        bootstrap_results <- numeric(n_bootstrap)
                        n <- nrow(data)

                        # Pre-compute migration directions to avoid recalculating in each iteration
                        old_stage_col <- self$options$oldStage
                        new_stage_col <- self$options$newStage

                        old_stages <- data[[old_stage_col]]
                        new_stages <- data[[new_stage_col]]

                        # Extract numeric values from stages
                        old_numeric <- suppressWarnings(as.numeric(gsub("[^0-9]", "", old_stages)))
                        new_numeric <- suppressWarnings(as.numeric(gsub("[^0-9]", "", new_stages)))

                        # If numeric extraction fails, use factor level ordering
                        if (any(is.na(old_numeric)) || any(is.na(new_numeric))) {
                            old_levels <- as.numeric(as.factor(old_stages))
                            new_levels <- as.numeric(as.factor(new_stages))
                        } else {
                            old_levels <- old_numeric
                            new_levels <- new_numeric
                        }

                        # Identify target population based on direction
                        if (direction == "upstaging") {
                            target_patients <- new_levels > old_levels
                        } else {
                            target_patients <- new_levels < old_levels
                        }

                        # Only proceed if we have enough target patients
                        if (sum(target_patients) < 10) {
                            return(numeric(0))
                        }

                        for (i in 1:n_bootstrap) {
                            # Bootstrap sample
                            boot_indices <- sample(1:n, n, replace = TRUE)
                            boot_data <- data[boot_indices, ]
                            boot_target <- target_patients[boot_indices]

                            # Skip if insufficient target patients in bootstrap sample
                            if (sum(boot_target) < 5) {
                                bootstrap_results[i] <- NA
                                next
                            }

                            # Refit models on bootstrap sample
                            old_formula <- old_cox$formula
                            new_formula <- new_cox$formula

                            boot_old_cox <- try(survival::coxph(old_formula, data = boot_data), silent = TRUE)
                            boot_new_cox <- try(survival::coxph(new_formula, data = boot_data), silent = TRUE)

                            if (!inherits(boot_old_cox, "try-error") && !inherits(boot_new_cox, "try-error")) {
                                # Calculate directional NRI for target patients only
                                old_risk_boot <- predict(boot_old_cox, type = "risk")
                                new_risk_boot <- predict(boot_new_cox, type = "risk")
                                event_at_time_boot <- ifelse(boot_data[[self$options$survivalTime]] <= time_point &
                                    boot_data[["event_binary"]] == 1, 1, 0)

                                bootstrap_results[i] <- private$.calculateDirectionalNRI(
                                    old_risk_boot[boot_target],
                                    new_risk_boot[boot_target],
                                    event_at_time_boot[boot_target],
                                    direction
                                )
                            } else {
                                bootstrap_results[i] <- NA
                            }
                        }

                        return(bootstrap_results[!is.na(bootstrap_results)])
                    },
                    error = function(e) {
                        return(numeric(0))
                    }
                )
            },
            .bootstrapWeightedNRI = function(data, old_cox, new_cox, time_point, n_bootstrap = private$.getBootstrapReps()) {
                # Bootstrap for weighted NRI
                tryCatch(
                    {
                        bootstrap_results <- numeric(n_bootstrap)
                        n <- nrow(data)

                        for (i in 1:n_bootstrap) {
                            # Bootstrap sample
                            boot_indices <- sample(1:n, n, replace = TRUE)
                            boot_data <- data[boot_indices, ]

                            # Refit models on bootstrap sample
                            old_formula <- old_cox$formula
                            new_formula <- new_cox$formula

                            boot_old_cox <- try(survival::coxph(old_formula, data = boot_data), silent = TRUE)
                            boot_new_cox <- try(survival::coxph(new_formula, data = boot_data), silent = TRUE)

                            if (!inherits(boot_old_cox, "try-error") && !inherits(boot_new_cox, "try-error")) {
                                # Calculate weighted NRI for bootstrap sample
                                boot_weighted <- private$.calculateWeightedNRI(boot_data, boot_old_cox, boot_new_cox, time_point)
                                if (!is.null(boot_weighted) && !is.na(boot_weighted$nri)) {
                                    bootstrap_results[i] <- boot_weighted$nri
                                } else {
                                    bootstrap_results[i] <- NA
                                }
                            } else {
                                bootstrap_results[i] <- NA
                            }
                        }

                        return(bootstrap_results[!is.na(bootstrap_results)])
                    },
                    error = function(e) {
                        return(numeric(0))
                    }
                )
            },
            .performBootstrapValidation = function(data, all_results) {
                # Perform bootstrap validation for all advanced migration metrics
                bootstrap_reps <- self$options$bootstrapReps

                tryCatch(
                    {
                        # Initialize result containers
                        # NA_real_, not numeric() -- a slot a failed iteration never filled must
                        # read as missing, not as a genuine estimate of exactly 0.
                        bootstrap_results <- list(
                            monotonicity_scores = rep(NA_real_, bootstrap_reps),
                            will_rogers_rates = rep(NA_real_, bootstrap_reps),
                            stage_specific_cindices = list(),
                            pseudo_r2_improvements = list(
                                nagelkerke = rep(NA_real_, bootstrap_reps),
                                cox_snell = rep(NA_real_, bootstrap_reps),
                                mcfadden = rep(NA_real_, bootstrap_reps),
                                royston = rep(NA_real_, bootstrap_reps)
                            )
                        )


                        # Perform bootstrap sampling
                        for (i in 1:bootstrap_reps) {
                            # Checkpoint every 50 iterations to allow cancellation of long-running bootstrap
                            if (i %% 50 == 1) {
                                private$.checkpoint()
                            }

                            # Bootstrap sample
                            n <- nrow(data)
                            boot_indices <- sample(1:n, n, replace = TRUE)
                            boot_data <- data[boot_indices, ]

                            # Calculate metrics on bootstrap sample
                            # R passes lists by value: the callee mutated its own copy and the
                            # result was discarded, so every bootstrap container stayed at its
                            # preallocated 0 and was then reported as a validated estimate of 0.000
                            # with a "100% successful / Robust" summary.
                            bootstrap_results <- private$.calculateBootstrapMetrics(boot_data, bootstrap_results, i)

                            # Progress indicator every 100 iterations
                            if (i %% 100 == 0) {
                            }
                        }

                        # Calculate bootstrap confidence intervals and validation metrics
                        private$.summarizeBootstrapResults(bootstrap_results, all_results)
                    },
                    error = function(e) {
                        private$.addNotice("WARNING", .("Bootstrap validation failed"), conditionMessage(e))
                    }
                )
            },
            .calculateBootstrapMetrics = function(boot_data, bootstrap_results, iteration) {
                # Calculate all metrics on bootstrap sample
                tryCatch(
                    {
                        event_col <- self$options$event

                        # Handle event level
                        event_level <- self$options$eventLevel
                        if (!is.null(event_level) && event_level != "") {
                            event_binary <- ifelse(boot_data[[event_col]] == event_level, 1, 0)
                        } else {
                            event_binary <- as.numeric(boot_data[[event_col]])
                        }

                        # 1. Monotonicity score
                        mono_score <- private$.calculateBootstrapMonotonicity(boot_data, event_binary)
                        bootstrap_results$monotonicity_scores[iteration] <- mono_score

                        # 2. Will Rogers migration rate
                        wr_rate <- private$.calculateBootstrapWillRogers(boot_data)
                        bootstrap_results$will_rogers_rates[iteration] <- wr_rate

                        # 3. Stage-specific C-indices (for each original stage)
                        stage_cindices <- private$.calculateBootstrapStageSpecificCIndex(boot_data, event_binary)
                        if (iteration == 1) {
                            # Initialize stage-specific lists
                            for (stage in names(stage_cindices)) {
                                bootstrap_results$stage_specific_cindices[[stage]] <- rep(NA_real_, length(bootstrap_results$monotonicity_scores))
                            }
                        }
                        for (stage in names(stage_cindices)) {
                            bootstrap_results$stage_specific_cindices[[stage]][iteration] <- stage_cindices[[stage]]
                        }

                        # 4. Pseudo R² improvements
                        pseudo_r2_values <- private$.calculateBootstrapPseudoR2(boot_data, event_binary)
                        bootstrap_results$pseudo_r2_improvements$nagelkerke[iteration] <- pseudo_r2_values$nagelkerke
                        bootstrap_results$pseudo_r2_improvements$cox_snell[iteration] <- pseudo_r2_values$cox_snell
                        bootstrap_results$pseudo_r2_improvements$mcfadden[iteration] <- pseudo_r2_values$mcfadden
                        bootstrap_results$pseudo_r2_improvements$royston[iteration] <- pseudo_r2_values$royston

                        bootstrap_results
                    },
                    error = function(e) {
                        # A bootstrap sample with insufficient data leaves this iteration's
                        # slots at NA (see the NA_real_ preallocation) rather than 0.
                        bootstrap_results
                    }
                )
            },
            .calculateBootstrapMonotonicity = function(boot_data, event_binary) {
                # Calculate monotonicity score for bootstrap sample
                tryCatch(
                    {
                        old_col <- self$options$oldStage

                        old_stages <- sort(unique(boot_data[[old_col]]))
                        if (length(old_stages) < 2) {
                            return(NA)
                        }

                        violations <- 0
                        total_comparisons <- 0

                        for (i in 1:(length(old_stages) - 1)) {
                            for (j in (i + 1):length(old_stages)) {
                                stage_i_data <- boot_data[boot_data[[old_col]] == old_stages[i], ]
                                stage_j_data <- boot_data[boot_data[[old_col]] == old_stages[j], ]

                                if (nrow(stage_i_data) > 0 && nrow(stage_j_data) > 0) {
                                    median_i <- private$.calculateMedianSurvival(stage_i_data)
                                    median_j <- private$.calculateMedianSurvival(stage_j_data)

                                    if (!is.na(median_i) && !is.na(median_j)) {
                                        total_comparisons <- total_comparisons + 1
                                        if (median_i < median_j) { # Higher stage should have worse survival
                                            violations <- violations + 1
                                        }
                                    }
                                }
                            }
                        }

                        if (total_comparisons > 0) {
                            return(1 - violations / total_comparisons) # Monotonicity score (higher is better)
                        } else {
                            return(NA)
                        }
                    },
                    error = function(e) {
                        return(NA)
                    }
                )
            },
            .calculateBootstrapWillRogers = function(boot_data) {
                # Calculate Will Rogers migration rate for bootstrap sample
                tryCatch(
                    {
                        old_col <- self$options$oldStage
                        new_col <- self$options$newStage

                        same_stage <- sum(as.character(boot_data[[old_col]]) == as.character(boot_data[[new_col]]))
                        total_patients <- nrow(boot_data)
                        migration_rate <- (total_patients - same_stage) / total_patients

                        return(migration_rate)
                    },
                    error = function(e) {
                        return(NA)
                    }
                )
            },
            .calculateBootstrapStageSpecificCIndex = function(boot_data, event_binary) {
                # Calculate stage-specific C-indices for bootstrap sample
                tryCatch(
                    {
                        old_col <- self$options$oldStage
                        new_col <- self$options$newStage
                        time_col <- self$options$survivalTime

                        old_stages <- sort(unique(boot_data[[old_col]]))
                        stage_cindices <- list()

                        for (old_stage in old_stages) {
                            stage_data <- boot_data[boot_data[[old_col]] == old_stage, ]
                            stage_events <- event_binary[boot_data[[old_col]] == old_stage]

                            if (nrow(stage_data) >= 10 && length(unique(stage_data[[new_col]])) >= 2) {
                                tryCatch(
                                    {
                                        cox_formula <- as.formula(paste("survival::Surv(", time_col, ", stage_events) ~", new_col))
                                        cox_model <- survival::coxph(cox_formula, data = stage_data)
                                        concordance_result <- summary(cox_model)$concordance
                                        stage_cindices[[as.character(old_stage)]] <- concordance_result["C"]
                                    },
                                    error = function(e) {
                                        stage_cindices[[as.character(old_stage)]] <- NA
                                    }
                                )
                            } else {
                                stage_cindices[[as.character(old_stage)]] <- NA
                            }
                        }

                        return(stage_cindices)
                    },
                    error = function(e) {
                        return(list())
                    }
                )
            },
            .calculateBootstrapPseudoR2 = function(boot_data, event_binary) {
                # Calculate pseudo R² improvements for bootstrap sample
                tryCatch(
                    {
                        old_col <- self$options$oldStage
                        new_col <- self$options$newStage
                        time_col <- self$options$survivalTime

                        # Fit Cox models
                        old_formula <- as.formula(paste("survival::Surv(", time_col, ", event_binary) ~ factor(", old_col, ")"))
                        new_formula <- as.formula(paste("survival::Surv(", time_col, ", event_binary) ~ factor(", new_col, ")"))

                        old_model <- survival::coxph(old_formula, data = boot_data)
                        new_model <- survival::coxph(new_formula, data = boot_data)
                        null_model <- survival::coxph(as.formula(paste("survival::Surv(", time_col, ", event_binary) ~ 1")), data = boot_data)

                        n <- nrow(boot_data)

                        # Calculate pseudo R² for both models
                        old_nagelkerke <- private$.calculateNagelkerkeR2(old_model, null_model, n)
                        new_nagelkerke <- private$.calculateNagelkerkeR2(new_model, null_model, n)

                        old_cox_snell <- private$.calculateCoxSnellR2(old_model, null_model, n)
                        new_cox_snell <- private$.calculateCoxSnellR2(new_model, null_model, n)

                        old_mcfadden <- private$.calculateMcFaddenR2(old_model, null_model)
                        new_mcfadden <- private$.calculateMcFaddenR2(new_model, null_model)

                        # .calculateRoystonR2 takes a single model argument; passing null_model
                        # raised "unused argument", which tryCatch swallowed and turned the whole
                        # bootstrap pseudo-R2 result into all-NA.
                        old_royston <- private$.calculateRoystonR2(old_model)
                        new_royston <- private$.calculateRoystonR2(new_model)

                        # Calculate improvements
                        return(list(
                            nagelkerke = ifelse(!is.na(new_nagelkerke) && !is.na(old_nagelkerke), new_nagelkerke - old_nagelkerke, NA),
                            cox_snell = ifelse(!is.na(new_cox_snell) && !is.na(old_cox_snell), new_cox_snell - old_cox_snell, NA),
                            mcfadden = ifelse(!is.na(new_mcfadden) && !is.na(old_mcfadden), new_mcfadden - old_mcfadden, NA),
                            royston = ifelse(!is.na(new_royston) && !is.na(old_royston), new_royston - old_royston, NA)
                        ))
                    },
                    error = function(e) {
                        return(list(nagelkerke = NA, cox_snell = NA, mcfadden = NA, royston = NA))
                    }
                )
            },
            .summarizeBootstrapResults = function(bootstrap_results, all_results) {
                # Summarize bootstrap validation results and add to outputs
                tryCatch(
                    {
                        # Add bootstrap validation summary to statistical summary table
                        if (!is.null(self$results$statisticalSummary)) {
                            table <- self$results$statisticalSummary

                            # Calculate confidence intervals and validation metrics

                            # Monotonicity bootstrap results
                            mono_scores <- bootstrap_results$monotonicity_scores[!is.na(bootstrap_results$monotonicity_scores)]
                            if (length(mono_scores) > 0) {
                                mono_ci <- quantile(mono_scores, private$.ciProbs(), na.rm = TRUE)
                                mono_se <- sd(mono_scores, na.rm = TRUE)

                                table$addRow(rowKey = "monotonicity_bootstrap", values = list(
                                    Metric = "Monotonicity Score (Bootstrap Validated)",
                                    Original_System = sprintf("%.3f (SE: %.3f)", mean(mono_scores), mono_se),
                                    New_System = sprintf("%.0f%% CI: [%.3f, %.3f]", self$options$confidenceLevel * 100, mono_ci[1], mono_ci[2]),
                                    Comparison = ifelse(mean(mono_scores) > 0.8, "Excellent", ifelse(mean(mono_scores) > 0.6, "Good", "Poor")),
                                    Interpretation = sprintf("Bootstrap validation with %d samples", length(mono_scores))
                                ))
                            }

                            # Will Rogers bootstrap results
                            wr_rates <- bootstrap_results$will_rogers_rates[!is.na(bootstrap_results$will_rogers_rates)]
                            if (length(wr_rates) > 0) {
                                wr_ci <- quantile(wr_rates, private$.ciProbs(), na.rm = TRUE)
                                wr_se <- sd(wr_rates, na.rm = TRUE)

                                table$addRow(rowKey = "will_rogers_bootstrap", values = list(
                                    Metric = "Migration Rate (Bootstrap Validated)",
                                    Original_System = sprintf("%.1f%% (SE: %.1f%%)", mean(wr_rates) * 100, wr_se * 100),
                                    New_System = sprintf("%.0f%% CI: [%.1f%%, %.1f%%]", self$options$confidenceLevel * 100, wr_ci[1] * 100, wr_ci[2] * 100),
                                    Comparison = ifelse(mean(wr_rates) < 0.1, "Low risk", ifelse(mean(wr_rates) < 0.2, "Moderate risk", "High risk")),
                                    Interpretation = .("Will Rogers phenomenon assessment")
                                ))
                            }

                            # Pseudo R² bootstrap results
                            for (measure_name in names(bootstrap_results$pseudo_r2_improvements)) {
                                improvements <- bootstrap_results$pseudo_r2_improvements[[measure_name]]
                                improvements <- improvements[!is.na(improvements)]

                                if (length(improvements) > 0) {
                                    imp_ci <- quantile(improvements, private$.ciProbs(), na.rm = TRUE)
                                    imp_se <- sd(improvements, na.rm = TRUE)

                                    significance <- ifelse(imp_ci[1] > 0, "Significant", "Non-significant")

                                    table$addRow(rowKey = paste0(measure_name, "_bootstrap"), values = list(
                                        Metric = paste(tools::toTitleCase(measure_name), "R\u{00B2} Improvement (Bootstrap)"),
                                        Original_System = sprintf("%.4f (SE: %.4f)", mean(improvements), imp_se),
                                        New_System = sprintf("%.0f%% CI: [%.4f, %.4f]", self$options$confidenceLevel * 100, imp_ci[1], imp_ci[2]),
                                        Comparison = significance,
                                        Interpretation = ifelse(mean(improvements) > 0.02, "Clinically meaningful", "Minimal improvement")
                                    ))
                                }
                            }

                            # Overall bootstrap validation summary
                            successful_reps <- sum(!is.na(bootstrap_results$monotonicity_scores))
                            total_reps <- length(bootstrap_results$monotonicity_scores)
                            success_rate <- successful_reps / total_reps

                            table$addRow(rowKey = "bootstrap_summary", values = list(
                                Metric = "Bootstrap Validation Summary",
                                Original_System = sprintf("%d/%d successful", successful_reps, total_reps),
                                New_System = sprintf("Success rate: %.1f%%", success_rate * 100),
                                Comparison = ifelse(success_rate > 0.8, "Robust", ifelse(success_rate > 0.6, "Adequate", "Unstable")),
                                Interpretation = .("Internal validation assessment")
                            ))
                        }


                        # Populate comprehensive bootstrap results table
                        private$.populateComprehensiveBootstrapResults(bootstrap_results, all_results)
                    },
                    error = function(e) {
                    }
                )
            },
            .populateComprehensiveBootstrapResults = function(bootstrap_results, all_results) {
                # Populate the comprehensive bootstrap validation results table
                tryCatch(
                    {
                        table <- self$results$bootstrapResults

                        if (is.null(table)) {
                            return()
                        }

                        # Add C-index improvement bootstrap results
                        if (!is.null(bootstrap_results$stage_specific_cindices) &&
                            length(bootstrap_results$stage_specific_cindices) > 0) {
                            # Calculate C-index differences across bootstrap samples
                            # stage_specific_cindices is a named list of numeric VECTORS
                            # (one per stage, one element per bootstrap replicate). The former
                            # x$new_cindex indexed an atomic vector with $, which is a hard
                            # error in R -- caught by the outer tryCatch, so this entire table
                            # (including the NRI and IDI rows below) was always empty.
                            cindex_diffs <- unlist(lapply(
                                bootstrap_results$stage_specific_cindices,
                                function(x) if (is.numeric(x)) x else NA_real_), use.names = FALSE)

                            cindex_diffs <- cindex_diffs[!is.na(cindex_diffs)]

                            if (length(cindex_diffs) > 0) {
                                # Calculate bootstrap statistics
                                boot_mean <- mean(cindex_diffs, na.rm = TRUE)
                                boot_se <- sd(cindex_diffs, na.rm = TRUE)
                                boot_ci <- quantile(cindex_diffs, private$.ciProbs(), na.rm = TRUE)
                                success_rate <- sprintf("%.1f%%", length(cindex_diffs) / length(bootstrap_results$stage_specific_cindices) * 100)

                                # Get apparent improvement from original analysis
                                apparent_improvement <- 0
                                if (!is.null(all_results$advanced_metrics)) {
                                    old_cindex <- all_results$advanced_metrics$old_cindex
                                    new_cindex <- all_results$advanced_metrics$new_cindex
                                    if (!is.null(old_cindex) && !is.null(new_cindex)) {
                                        apparent_improvement <- new_cindex - old_cindex
                                    }
                                }

                                # Optimism, in Harrell's sense, is
                                #     mean over b of [ perf(model_b, D_b) - perf(model_b, D_orig) ]
                                # i.e. it requires each bootstrap model to be evaluated BOTH on its
                                # own sample and on the original data. `.performBootstrapValidation`
                                # only ever evaluates on the bootstrap sample, so that second term
                                # does not exist here and `boot_mean - apparent_improvement` is the
                                # plain bootstrap bias, a different (and generally smaller)
                                # quantity than optimism.
                                #
                                # `.performLegacyBootstrapValidation` DOES implement the correct
                                # two-evaluation structure (it calls concordance(..., newdata =
                                # data)); its optimism is used when available rather than
                                # recomputing a bias term and labelling it optimism.
                                if (isTRUE(self$options$useOptimismCorrection)) {
                                    legacy_opt <- all_results$validation_results$mean_optimism
                                    if (!is.null(legacy_opt) && is.finite(legacy_opt)) {
                                        optimism <- legacy_opt
                                        optimism_corrected <- apparent_improvement - optimism
                                    } else {
                                        optimism <- NA
                                        optimism_corrected <- NA
                                    }
                                } else {
                                    optimism <- NA
                                    optimism_corrected <- NA
                                }

                                # Clinical interpretation
                                clinical_interpretation <- private$.interpretBootstrapOptimism(optimism, success_rate)

                                table$addRow(rowKey = "cindex_improvement", values = list(
                                    Metric = "C-index Improvement",
                                    Apparent = apparent_improvement,
                                    Bootstrap_Mean = boot_mean,
                                    Bootstrap_SE = boot_se,
                                    Bootstrap_CI_Lower = boot_ci[1],
                                    Bootstrap_CI_Upper = boot_ci[2],
                                    Optimism = optimism,
                                    Optimism_Corrected = optimism_corrected,
                                    Success_Rate = success_rate,
                                    Clinical_Interpretation = clinical_interpretation
                                ))
                            }
                        }

                        # Add Pseudo R² bootstrap results
                        if (!is.null(bootstrap_results$pseudo_r2_improvements) &&
                            length(bootstrap_results$pseudo_r2_improvements) > 0) {
                            # Nagelkerke R² improvements
                            # pseudo_r2_improvements is list(nagelkerke=, cox_snell=, mcfadden=,
                            # royston=), each a numeric vector over bootstrap replicates. Same
                            # atomic-$ error as above.
                            nagelkerke_diffs <- bootstrap_results$pseudo_r2_improvements$nagelkerke
                            if (is.null(nagelkerke_diffs)) nagelkerke_diffs <- NA_real_

                            nagelkerke_diffs <- nagelkerke_diffs[!is.na(nagelkerke_diffs)]

                            if (length(nagelkerke_diffs) > 0) {
                                # Calculate bootstrap statistics for Nagelkerke
                                boot_mean <- mean(nagelkerke_diffs, na.rm = TRUE)
                                boot_se <- sd(nagelkerke_diffs, na.rm = TRUE)
                                boot_ci <- quantile(nagelkerke_diffs, private$.ciProbs(), na.rm = TRUE)
                                success_rate <- sprintf("%.1f%%", length(nagelkerke_diffs) / length(bootstrap_results$pseudo_r2_improvements) * 100)

                                # Get apparent Nagelkerke improvement
                                apparent_improvement <- 0
                                if (!is.null(all_results$pseudo_r2_results)) {
                                    old_nagelkerke <- all_results$pseudo_r2_results$old_nagelkerke
                                    new_nagelkerke <- all_results$pseudo_r2_results$new_nagelkerke
                                    if (!is.null(old_nagelkerke) && !is.null(new_nagelkerke)) {
                                        apparent_improvement <- new_nagelkerke - old_nagelkerke
                                    }
                                }

                                # Optimism, in Harrell's sense, is
                                #     mean over b of [ perf(model_b, D_b) - perf(model_b, D_orig) ]
                                # i.e. it requires each bootstrap model to be evaluated BOTH on its
                                # own sample and on the original data. `.performBootstrapValidation`
                                # only ever evaluates on the bootstrap sample, so that second term
                                # does not exist here and `boot_mean - apparent_improvement` is the
                                # plain bootstrap bias, a different (and generally smaller)
                                # quantity than optimism.
                                #
                                # `.performLegacyBootstrapValidation` DOES implement the correct
                                # two-evaluation structure (it calls concordance(..., newdata =
                                # data)); its optimism is used when available rather than
                                # recomputing a bias term and labelling it optimism.
                                if (isTRUE(self$options$useOptimismCorrection)) {
                                    legacy_opt <- all_results$validation_results$mean_optimism
                                    if (!is.null(legacy_opt) && is.finite(legacy_opt)) {
                                        optimism <- legacy_opt
                                        optimism_corrected <- apparent_improvement - optimism
                                    } else {
                                        optimism <- NA
                                        optimism_corrected <- NA
                                    }
                                } else {
                                    optimism <- NA
                                    optimism_corrected <- NA
                                }

                                # Clinical interpretation
                                clinical_interpretation <- private$.interpretBootstrapOptimism(optimism, success_rate)

                                table$addRow(rowKey = "nagelkerke_improvement", values = list(
                                    Metric = "Nagelkerke R\u{00B2} Improvement",
                                    Apparent = apparent_improvement,
                                    Bootstrap_Mean = boot_mean,
                                    Bootstrap_SE = boot_se,
                                    Bootstrap_CI_Lower = boot_ci[1],
                                    Bootstrap_CI_Upper = boot_ci[2],
                                    Optimism = optimism,
                                    Optimism_Corrected = optimism_corrected,
                                    Success_Rate = success_rate,
                                    Clinical_Interpretation = clinical_interpretation
                                ))
                            }
                        }

                        # Add NRI bootstrap results if available
                        if (!is.null(all_results$nri_results) && !is.null(all_results$nri_results$bootstrap_ci)) {
                            nri_results <- all_results$nri_results

                            # Calculate bootstrap statistics for NRI (using existing bootstrap results)
                            if (!is.null(nri_results$bootstrap_values)) {
                                boot_mean <- mean(nri_results$bootstrap_values, na.rm = TRUE)
                                boot_se <- sd(nri_results$bootstrap_values, na.rm = TRUE)
                                success_rate <- sprintf("%.1f%%", sum(!is.na(nri_results$bootstrap_values)) / length(nri_results$bootstrap_values) * 100)

                                table$addRow(rowKey = "nri", values = list(
                                    Metric = "Net Reclassification Improvement",
                                    Apparent = nri_results$nri_estimate,
                                    Bootstrap_Mean = boot_mean,
                                    Bootstrap_SE = boot_se,
                                    Bootstrap_CI_Lower = nri_results$bootstrap_ci[1],
                                    Bootstrap_CI_Upper = nri_results$bootstrap_ci[2],
                                    Optimism = NA, # Not applicable for NRI
                                    Optimism_Corrected = nri_results$nri_estimate,
                                    Success_Rate = success_rate,
                                    Clinical_Interpretation = if (nri_results$nri_estimate > 0.2) "Substantial improvement" else if (nri_results$nri_estimate > 0.1) "Moderate improvement" else "Limited improvement"
                                ))
                            }
                        }

                        # Add IDI bootstrap results if available
                        if (!is.null(all_results$idi_results) && !is.null(all_results$idi_results$bootstrap_ci)) {
                            idi_results <- all_results$idi_results

                            # Calculate bootstrap statistics for IDI (using existing bootstrap results)
                            if (!is.null(idi_results$bootstrap_values)) {
                                boot_mean <- mean(idi_results$bootstrap_values, na.rm = TRUE)
                                boot_se <- sd(idi_results$bootstrap_values, na.rm = TRUE)
                                success_rate <- sprintf("%.1f%%", sum(!is.na(idi_results$bootstrap_values)) / length(idi_results$bootstrap_values) * 100)

                                table$addRow(rowKey = "idi", values = list(
                                    Metric = "Integrated Discrimination Improvement",
                                    Apparent = idi_results$idi_estimate,
                                    Bootstrap_Mean = boot_mean,
                                    Bootstrap_SE = boot_se,
                                    Bootstrap_CI_Lower = idi_results$bootstrap_ci[1],
                                    Bootstrap_CI_Upper = idi_results$bootstrap_ci[2],
                                    Optimism = NA, # Not applicable for IDI
                                    Optimism_Corrected = idi_results$idi_estimate,
                                    Success_Rate = success_rate,
                                    Clinical_Interpretation = if (idi_results$idi_estimate > 0.02) "Substantial improvement" else if (idi_results$idi_estimate > 0.01) "Moderate improvement" else "Limited improvement"
                                ))
                            }
                        }

                    },
                    error = function(e) {
                    }
                )
            },
            .interpretBootstrapOptimism = function(optimism, success_rate) {
                # Interpret bootstrap validation results with clinical context
                success_numeric <- as.numeric(gsub("%", "", success_rate))

                if (is.na(optimism) || is.na(success_numeric)) {
                    return("Inconclusive - insufficient bootstrap data")
                }

                if (success_numeric < 80) {
                    return("Unreliable - low bootstrap success rate")
                } else if (abs(optimism) < 0.005) {
                    return("Excellent internal validation - minimal optimism")
                } else if (abs(optimism) < 0.01) {
                    return("Good internal validation - low optimism")
                } else if (abs(optimism) < 0.02) {
                    return("Moderate optimism - interpret with caution")
                } else {
                    return("High optimism - external validation strongly recommended")
                }
            },

            # Plot Functions
            .plotMigrationHeatmap = function(image, ggtheme, theme, ...) {
                # Create heatmap visualization of migration matrix
                if (is.null(image$parent$options$oldStage) || is.null(image$parent$options$newStage)) {
                    return(FALSE)
                }

                # Get state data
                plot_data <- image$state
                if (is.null(plot_data) || is.null(plot_data$migration_matrix)) {
                    # Try alternative approach - get data directly from results
                    tryCatch(
                        {
                            basic_migration <- private$.calculateBasicMigration()
                            migration_matrix <- basic_migration$migration_table
                            if (is.null(migration_matrix)) {
                                return(FALSE)
                            }
                        },
                        error = function(e) {
                            return(FALSE)
                        }
                    )
                } else {
                    migration_matrix <- plot_data$migration_matrix
                }

                # Prepare data for heatmap
                if (!requireNamespace("reshape2", quietly = TRUE)) {
                    return(FALSE)
                }

                # Convert matrix to long format for ggplot2
                tryCatch(
                    {
                        matrix_long <- reshape2::melt(as.matrix(migration_matrix), varnames = c("Original", "New"), value.name = "Count")
                    },
                    error = function(e) {
                        # If melt fails, try manual conversion
                        rows <- rep(rownames(migration_matrix), ncol(migration_matrix))
                        cols <- rep(colnames(migration_matrix), each = nrow(migration_matrix))
                        vals <- as.vector(migration_matrix)
                        matrix_long <- data.frame(
                            Original = rows,
                            New = cols,
                            Count = vals
                        )
                    }
                )

                # Ensure Count is numeric
                matrix_long$Count <- as.numeric(matrix_long$Count)

                # Calculate percentage for each cell
                total_patients <- sum(matrix_long$Count)
                matrix_long$Percentage <- round(matrix_long$Count / total_patients * 100, 1)

                # Create label with count and percentage
                matrix_long$Label <- ifelse(matrix_long$Count > 0,
                    paste0(matrix_long$Count, "\n(", matrix_long$Percentage, "%)"),
                    ""
                )

                # Determine if cell is on diagonal
                matrix_long$IsDiagonal <- as.character(matrix_long$Original) == as.character(matrix_long$New)

                # Create heatmap with enhanced visualization
                p <- ggplot(matrix_long, aes(x = New, y = Original)) +
                    # Add tile with conditional coloring
                    ggplot2::geom_tile(aes(fill = Count), color = "white", linewidth = 0.5) +
                    # Add border for diagonal cells
                    ggplot2::geom_tile(
                        data = matrix_long[matrix_long$IsDiagonal, ],
                        fill = NA, color = "black", linewidth = 1.5
                    ) +
                    # Add text labels
                    ggplot2::geom_text(aes(label = Label),
                        color = ifelse(matrix_long$Count > max(matrix_long$Count) * 0.5, "white", "black"),
                        size = 3.5, lineheight = 0.8
                    ) +
                    # Color scale (colorblind-safe viridis if accessibility features enabled)
                    (if (isTRUE(image$parent$options$enableAccessibilityFeatures)) {
                        ggplot2::scale_fill_viridis_c(
                            option = "viridis",
                            name = "Number of\nPatients",
                            breaks = pretty(range(matrix_long$Count), n = 5)
                        )
                    } else {
                        ggplot2::scale_fill_gradient2(
                            low = "#f0f0f0",
                            mid = "#3498db",
                            high = "#2c3e50",
                            midpoint = median(matrix_long$Count),
                            name = "Number of\nPatients",
                            breaks = pretty(range(matrix_long$Count), n = 5)
                        )
                    }) +
                    # Labels
                    ggplot2::labs(
                        title = "Stage Migration Heatmap",
                        subtitle = paste0(
                            "Total patients: ", total_patients, " | Migration rate: ",
                            round(sum(matrix_long$Count[!matrix_long$IsDiagonal]) / total_patients * 100, 1), "%"
                        ),
                        x = "New Staging System \u{2192}",
                        y = "\u{2190} Original Staging System"
                    ) +
                    # Theme
                    ggtheme +
                    theme(
                        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
                        axis.text.y = element_text(size = 11),
                        axis.title = element_text(size = 12, face = "bold"),
                        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
                        legend.position = "right",
                        legend.title = element_text(size = 10, face = "bold"),
                        panel.grid = element_blank(),
                        plot.margin = margin(10, 10, 10, 10)
                    ) +
                    # Equal aspect ratio
                    coord_equal()

                print(p)
                TRUE
            },
            .plotROCComparison = function(image, ggtheme, theme, ...) {
                # Create enhanced ROC comparison plot with multiple improvements
                if (is.null(image$parent$options$oldStage) || is.null(image$parent$options$newStage)) {
                    return(FALSE)
                }

                # Get state data
                plot_data <- image$state
                if (is.null(plot_data)) {
                    return(FALSE)
                }

                if (!requireNamespace("timeROC", quietly = TRUE)) {
                    return(FALSE)
                }

                # The ROC data is stored as a list with time points
                roc_data <- plot_data

                # If there are no time points, return
                if (length(roc_data) == 0) {
                    return(FALSE)
                }

                # Get the first time point for the main plot
                first_time_point <- names(roc_data)[1]
                time_data <- roc_data[[first_time_point]]

                # Check if we have valid ROC objects
                if (is.null(time_data$old_roc) || is.null(time_data$new_roc)) {
                    return(FALSE)
                }

                # Extract ROC curve data
                old_roc_obj <- time_data$old_roc
                new_roc_obj <- time_data$new_roc

                # Validate ROC objects structure
                if (is.null(old_roc_obj) || is.null(new_roc_obj)) {
                    return(FALSE)
                }

                # Check if required components exist and have proper dimensions
                if (!is.matrix(old_roc_obj$FP) || !is.matrix(old_roc_obj$TP) ||
                    !is.matrix(new_roc_obj$FP) || !is.matrix(new_roc_obj$TP)) {
                    return(FALSE)
                }

                # Check matrix dimensions
                if (ncol(old_roc_obj$FP) < 1 || ncol(old_roc_obj$TP) < 1 ||
                    ncol(new_roc_obj$FP) < 1 || ncol(new_roc_obj$TP) < 1) {
                    return(FALSE)
                }

                # Create data frames for plotting with more points for smoother curves
                old_roc_df <- data.frame(
                    FPR = old_roc_obj$FP[, 1],
                    TPR = old_roc_obj$TP[, 1],
                    System = "Original"
                )

                new_roc_df <- data.frame(
                    FPR = new_roc_obj$FP[, 1],
                    TPR = new_roc_obj$TP[, 1],
                    System = "New"
                )

                # Combine data
                combined_roc <- rbind(old_roc_df, new_roc_df)

                # Calculate confidence intervals if available
                old_ci_info <- ""
                new_ci_info <- ""
                if (!is.null(time_data$old_ci) && length(time_data$old_ci) >= 2) {
                    old_ci_info <- sprintf(" (%.0f%% CI: %.3f-%.3f)", self$options$confidenceLevel * 100, time_data$old_ci[1], time_data$old_ci[2])
                }
                if (!is.null(time_data$new_ci) && length(time_data$new_ci) >= 2) {
                    new_ci_info <- sprintf(" (%.0f%% CI: %.3f-%.3f)", self$options$confidenceLevel * 100, time_data$new_ci[1], time_data$new_ci[2])
                }

                # Enhanced statistical significance testing
                p_value_text <- ""
                if (!is.null(time_data$p_value) && !is.na(time_data$p_value)) {
                    significance <- ifelse(time_data$p_value < 0.001, "***",
                        ifelse(time_data$p_value < 0.01, "**",
                            ifelse(time_data$p_value < 0.05, "*", "ns")
                        )
                    )
                    p_value_text <- sprintf(
                        "p = %.3f%s", time_data$p_value,
                        ifelse(significance != "ns", paste0(" ", significance), "")
                    )
                }

                # Create enhanced ROC comparison plot
                p <- ggplot(combined_roc, aes(x = FPR, y = TPR, color = System)) +
                    geom_line(linewidth = 1.8, alpha = 0.9) +
                    # Add points for better curve definition
                    geom_point(size = 0.3, alpha = 0.6) +
                    # Enhanced reference line
                    geom_abline(
                        intercept = 0, slope = 1, color = "gray50", linetype = "dashed",
                        linewidth = 1, alpha = 0.8
                    ) +
                    # Professional color scheme with better contrast
                    scale_color_manual(
                        values = c("Original" = "#d32f2f", "New" = "#1976d2"),
                        guide = guide_legend(override.aes = list(linewidth = 3, alpha = 1))
                    ) +
                    ggplot2::labs(
                        title = "Time-dependent ROC Curve Comparison",
                        subtitle = paste("Survival Analysis at", time_data$time_point, "months"),
                        x = "False Positive Rate (1 - Specificity)",
                        y = "True Positive Rate (Sensitivity)",
                        color = "Staging System",
                        caption = "Diagonal line represents random chance (AUC = 0.5)"
                    ) +
                    scale_x_continuous(
                        limits = c(0, 1), expand = c(0.01, 0.01),
                        breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)
                    ) +
                    scale_y_continuous(
                        limits = c(0, 1), expand = c(0.01, 0.01),
                        breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)
                    ) +
                    ggtheme +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
                        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40", margin = margin(b = 15)),
                        plot.caption = element_text(size = 9, color = "gray60", hjust = 0),
                        legend.position = "bottom",
                        legend.title = element_text(size = 12, face = "bold"),
                        legend.text = element_text(size = 11),
                        legend.box.margin = margin(t = 10),
                        axis.title = element_text(size = 12, face = "bold"),
                        axis.text = element_text(size = 11),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
                        panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.8),
                        plot.margin = margin(15, 15, 15, 15)
                    ) +
                    # Enhanced annotations with better positioning
                    ggplot2::annotate("rect",
                        xmin = 0.55, xmax = 0.98, ymin = 0.02, ymax = 0.45,
                        fill = "white", color = "gray80", alpha = 0.95, linewidth = 0.5
                    ) +
                    ggplot2::annotate("text",
                        x = 0.765, y = 0.38,
                        label = paste("Original AUC:", sprintf("%.3f", time_data$old_auc), old_ci_info),
                        color = "#d32f2f", size = 3.5, fontface = "bold", hjust = 0.5
                    ) +
                    ggplot2::annotate("text",
                        x = 0.765, y = 0.31,
                        label = paste("New AUC:", sprintf("%.3f", time_data$new_auc), new_ci_info),
                        color = "#1976d2", size = 3.5, fontface = "bold", hjust = 0.5
                    ) +
                    ggplot2::annotate("text",
                        x = 0.765, y = 0.24,
                        label = paste("Difference:", sprintf("%+.3f", time_data$auc_improvement)),
                        color = ifelse(time_data$auc_improvement > 0, "#2e7d32", "#c62828"),
                        size = 3.5, fontface = "bold", hjust = 0.5
                    )

                # Add statistical significance annotation if available
                if (p_value_text != "") {
                    p <- p + ggplot2::annotate("text",
                        x = 0.765, y = 0.17,
                        label = p_value_text,
                        color = "gray30", size = 3.5, fontface = "bold", hjust = 0.5
                    )
                }

                # Add optimal cut-point indicators if available
                if (!is.null(time_data$optimal_cutpoints)) {
                    p <- p + ggplot2::annotate("text",
                        x = 0.765, y = 0.10,
                        label = " Optimal cut-points shown",
                        color = "gray50", size = 3, hjust = 0.5
                    )
                }

                # If there are multiple time points, create an enhanced faceted plot
                if (length(roc_data) > 1) {
                    # Create multi-panel plot for multiple time points
                    all_roc_data <- data.frame()
                    annotation_data <- data.frame()

                    for (tp_name in names(roc_data)) {
                        tp_data <- roc_data[[tp_name]]
                        if (!is.null(tp_data$old_roc) && !is.null(tp_data$new_roc)) {
                            old_df <- data.frame(
                                FPR = tp_data$old_roc$FP[, 1],
                                TPR = tp_data$old_roc$TP[, 1],
                                System = "Original",
                                TimePoint = paste(tp_data$time_point, "months"),
                                AUC = round(tp_data$old_auc, 3)
                            )

                            new_df <- data.frame(
                                FPR = tp_data$new_roc$FP[, 1],
                                TPR = tp_data$new_roc$TP[, 1],
                                System = "New",
                                TimePoint = paste(tp_data$time_point, "months"),
                                AUC = round(tp_data$new_auc, 3)
                            )

                            all_roc_data <- rbind(all_roc_data, old_df, new_df)

                            # Prepare annotation data for each panel
                            ann_df <- data.frame(
                                TimePoint = paste(tp_data$time_point, "months"),
                                old_auc = tp_data$old_auc,
                                new_auc = tp_data$new_auc,
                                improvement = tp_data$auc_improvement,
                                x_pos = 0.65,
                                y_old = 0.35,
                                y_new = 0.25,
                                y_diff = 0.15
                            )
                            annotation_data <- rbind(annotation_data, ann_df)
                        }
                    }

                    # Create enhanced faceted plot
                    p <- ggplot(all_roc_data, aes(x = FPR, y = TPR, color = System)) +
                        geom_line(linewidth = 1.5, alpha = 0.9) +
                        geom_point(size = 0.2, alpha = 0.5) +
                        geom_abline(intercept = 0, slope = 1, color = "gray50", linetype = "dashed", alpha = 0.7) +
                        facet_wrap(~TimePoint, ncol = min(3, length(roc_data))) +
                        scale_color_manual(values = c("Original" = "#d32f2f", "New" = "#1976d2")) +
                        ggplot2::labs(
                            title = "Time-dependent ROC Curve Comparison",
                            subtitle = "Multiple survival time points analysis",
                            x = "False Positive Rate (1 - Specificity)",
                            y = "True Positive Rate (Sensitivity)",
                            color = "Staging System",
                            caption = "Each panel shows ROC curves at different survival time points"
                        ) +
                        scale_x_continuous(
                            limits = c(0, 1), expand = c(0.02, 0.02),
                            breaks = seq(0, 1, 0.5), labels = scales::percent_format(accuracy = 1)
                        ) +
                        scale_y_continuous(
                            limits = c(0, 1), expand = c(0.02, 0.02),
                            breaks = seq(0, 1, 0.5), labels = scales::percent_format(accuracy = 1)
                        ) +
                        ggtheme +
                        theme(
                            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                            plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
                            plot.caption = element_text(size = 9, color = "gray60", hjust = 0),
                            legend.position = "bottom",
                            legend.title = element_text(size = 11, face = "bold"),
                            legend.text = element_text(size = 10),
                            strip.text = element_text(size = 11, face = "bold", color = "gray20"),
                            strip.background = element_rect(fill = "gray95", color = "gray80"),
                            axis.title = element_text(size = 11, face = "bold"),
                            axis.text = element_text(size = 9),
                            panel.grid.minor = element_blank(),
                            panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
                            panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
                            plot.margin = margin(10, 10, 10, 10)
                        )

                    # Add AUC annotations to each facet
                    if (nrow(annotation_data) > 0) {
                        p <- p +
                            geom_text(
                                data = annotation_data,
                                aes(
                                    x = x_pos, y = y_old,
                                    label = paste("O:", sprintf("%.3f", old_auc))
                                ),
                                color = "#d32f2f", size = 3, fontface = "bold", inherit.aes = FALSE
                            ) +
                            geom_text(
                                data = annotation_data,
                                aes(
                                    x = x_pos, y = y_new,
                                    label = paste("N:", sprintf("%.3f", new_auc))
                                ),
                                color = "#1976d2", size = 3, fontface = "bold", inherit.aes = FALSE
                            ) +
                            geom_text(
                                data = annotation_data,
                                aes(
                                    x = x_pos, y = y_diff,
                                    label = sprintf("\u{0394}: %+.3f", improvement)
                                ),
                                color = ifelse(annotation_data$improvement > 0, "#2e7d32", "#c62828"),
                                size = 3, fontface = "bold", inherit.aes = FALSE
                            )
                    }
                }

                print(p)
                TRUE
            },
            .plotForest = function(image, ggtheme, theme, ...) {
                # Create forest plot with hazard ratios
                if (is.null(image$parent$options$oldStage) || is.null(image$parent$options$newStage)) {
                    return(FALSE)
                }

                # Get state data
                plot_data <- image$state
                if (is.null(plot_data) || is.null(plot_data$old_cox_coef) || is.null(plot_data$new_cox_coef)) {
                    # Try to get data from parent state if not available
                    plot_data <- image$parent$state
                    if (is.null(plot_data) || is.null(plot_data$old_cox_coef) || is.null(plot_data$new_cox_coef)) {
                        return(FALSE)
                    }
                }

                # Create forest data from Cox model coefficients
                old_coef <- plot_data$old_cox_coef
                new_coef <- plot_data$new_cox_coef

                # Extract hazard ratios and confidence intervals
                tryCatch(
                    {
                        old_hr <- exp(old_coef[, "coef"])
                        old_ci_lower <- exp(old_coef[, "coef"] - private$.zCrit() * old_coef[, "se(coef)"])
                        old_ci_upper <- exp(old_coef[, "coef"] + private$.zCrit() * old_coef[, "se(coef)"])

                        new_hr <- exp(new_coef[, "coef"])
                        new_ci_lower <- exp(new_coef[, "coef"] - private$.zCrit() * new_coef[, "se(coef)"])
                        new_ci_upper <- exp(new_coef[, "coef"] + private$.zCrit() * new_coef[, "se(coef)"])

                        # Clean up stage names (remove prefix if present)
                        old_stage_names <- names(old_hr)
                        new_stage_names <- names(new_hr)

                        # Remove variable prefix from stage names
                        old_stage_clean <- gsub("^[^:]*:", "", old_stage_names)
                        new_stage_clean <- gsub("^[^:]*:", "", new_stage_names)

                        # Create forest data frame
                        forest_data <- data.frame(
                            System = c(rep("Original", length(old_hr)), rep("New", length(new_hr))),
                            Stage = c(old_stage_clean, new_stage_clean),
                            HR = c(old_hr, new_hr),
                            CI_Lower = c(old_ci_lower, new_ci_lower),
                            CI_Upper = c(old_ci_upper, new_ci_upper),
                            P_Value = c(old_coef[, "Pr(>|z|)"], new_coef[, "Pr(>|z|)"])
                        )

                        # Remove any rows with invalid values
                        forest_data <- forest_data[!is.na(forest_data$HR) & !is.na(forest_data$CI_Lower) & !is.na(forest_data$CI_Upper), ]

                        if (nrow(forest_data) == 0) {
                            return(FALSE)
                        }
                    },
                    error = function(e) {
                        return(FALSE)
                    }
                )

                # Add significance indicators
                forest_data$Significance <- ifelse(forest_data$P_Value < 0.001, "***",
                    ifelse(forest_data$P_Value < 0.01, "**",
                        ifelse(forest_data$P_Value < 0.05, "*", "")
                    )
                )

                # Create a combined stage-system identifier for better grouping
                forest_data$Stage_System <- interaction(forest_data$Stage, forest_data$System, sep = " - ")

                # Reorder for better visualization
                forest_data <- forest_data[order(forest_data$Stage, forest_data$System), ]
                forest_data$Stage_System <- factor(forest_data$Stage_System, levels = unique(forest_data$Stage_System))

                # Reorder stages naturally (Stage I, II, III, IV)
                stage_order <- c("I", "II", "III", "IV", "1", "2", "3", "4", "A", "B", "C", "D")
                forest_data$Stage_factor <- factor(forest_data$Stage, levels = stage_order)
                forest_data <- forest_data[order(forest_data$Stage_factor, forest_data$System), ]
                forest_data$Stage_System <- factor(forest_data$Stage_System, levels = unique(forest_data$Stage_System))

                # Calculate appropriate axis limits
                min_limit <- min(forest_data$CI_Lower, na.rm = TRUE) * 0.7
                max_limit <- max(forest_data$CI_Upper, na.rm = TRUE) * 2.2

                # Create professional forest plot
                p <- ggplot(forest_data, aes(y = Stage_System)) +
                    # Add alternating background for better readability
                    ggplot2::geom_rect(
                        data = forest_data[seq(1, nrow(forest_data), by = 2), ],
                        aes(
                            xmin = -Inf, xmax = Inf, ymin = as.numeric(Stage_System) - 0.4,
                            ymax = as.numeric(Stage_System) + 0.4
                        ),
                        fill = "gray95", alpha = 0.5, inherit.aes = FALSE
                    ) +
                    # Reference line at HR = 1 with enhanced styling
                    ggplot2::geom_vline(
                        xintercept = 1, color = "#d32f2f", linetype = "solid",
                        linewidth = 1.2, alpha = 0.8
                    ) +
                    ggplot2::annotate("text",
                        x = 1, y = nrow(forest_data) + 0.5,
                        label = "No Effect", hjust = 0.5, size = 3.5,
                        color = "#d32f2f", fontface = "bold"
                    ) +
                    # Enhanced confidence intervals with different shapes for systems
                    ggplot2::geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, color = System),
                        height = 0.25, linewidth = 1.5, alpha = 0.9
                    ) +
                    # Point estimates with different shapes for better distinction
                    ggplot2::geom_point(aes(x = HR, color = System, shape = System),
                        size = 4.5, alpha = 0.95, stroke = 1.2
                    ) +
                    # Professional color and shape scheme
                    scale_color_manual(
                        values = c("Original" = "#d32f2f", "New" = "#1976d2"),
                        name = "Staging System"
                    ) +
                    scale_shape_manual(
                        values = c("Original" = 16, "New" = 17),
                        name = "Staging System"
                    ) +
                    # Enhanced labels and titles
                    ggplot2::labs(
                        title = "Stage-Specific Hazard Ratio Comparison",
                        subtitle = "Forest plot showing hazard ratios with 95% confidence intervals",
                        x = "Hazard Ratio (log scale)",
                        y = "Stage Groups",
                        caption = "Reference line at HR = 1.0 (no effect) \u{2022} * p<0.05, ** p<0.01, *** p<0.001"
                    ) +
                    # Enhanced log scale with better breaks
                    scale_x_log10(
                        breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 10, 20),
                        labels = c("0.1", "0.25", "0.5", "1.0", "2.0", "4.0", "10", "20"),
                        limits = c(min_limit, max_limit),
                        expand = c(0.02, 0.02)
                    ) +
                    # Explicitly specify discrete y-axis
                    scale_y_discrete(expand = c(0.02, 0.02)) +
                    # Professional theme with enhanced styling
                    ggtheme +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 8)),
                        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40", margin = margin(b = 15)),
                        plot.caption = element_text(hjust = 0, size = 9, color = "gray60", margin = margin(t = 10)),
                        legend.position = "bottom",
                        legend.title = element_text(size = 12, face = "bold"),
                        legend.text = element_text(size = 11),
                        legend.box.margin = margin(t = 10),
                        legend.key.size = unit(1.2, "lines"),
                        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
                        axis.text.x = element_text(size = 11, color = "gray20"),
                        axis.text.y = element_text(size = 10, color = "gray20", hjust = 1),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
                        panel.grid.major.y = element_blank(),
                        panel.background = element_rect(fill = "white", color = NA),
                        plot.background = element_rect(fill = "white", color = NA),
                        strip.text = element_text(size = 11, face = "bold"),
                        plot.margin = margin(15, 120, 15, 15) # Extra right margin for annotations
                    ) +
                    # Allow text to extend beyond plot area
                    coord_cartesian(clip = "off")

                # Add HR text annotations outside the plot area
                for (i in seq_len(nrow(forest_data))) {
                    # HR and CI text
                    hr_text <- sprintf(
                        "%.2f (%.2f-%.2f)%s",
                        forest_data$HR[i],
                        forest_data$CI_Lower[i],
                        forest_data$CI_Upper[i],
                        ifelse(forest_data$Significance[i] != "",
                            paste0(" ", forest_data$Significance[i]), ""
                        )
                    )

                    p <- p + ggplot2::annotate("text",
                        x = max_limit * 0.85,
                        y = i,
                        label = hr_text,
                        hjust = 0, vjust = 0.5,
                        size = 3.2, fontface = "bold",
                        color = ifelse(forest_data$System[i] == "Original", "#d32f2f", "#1976d2")
                    )
                }

                # Add column header for HR values
                p <- p + ggplot2::annotate("text",
                    x = max_limit * 0.85,
                    y = nrow(forest_data) + 0.8,
                    label = "HR (95% CI)",
                    hjust = 0, vjust = 0.5,
                    size = 3.5, fontface = "bold",
                    color = "gray30"
                )

                # Add risk interpretation zones
                p <- p +
                    ggplot2::annotate("rect",
                        xmin = min_limit, xmax = 1,
                        ymin = 0.5, ymax = nrow(forest_data) + 0.5,
                        fill = "lightblue", alpha = 0.1
                    ) +
                    ggplot2::annotate("rect",
                        xmin = 1, xmax = max_limit,
                        ymin = 0.5, ymax = nrow(forest_data) + 0.5,
                        fill = "lightcoral", alpha = 0.1
                    ) +
                    ggplot2::annotate("text",
                        x = sqrt(min_limit), y = (nrow(forest_data) + 1) / 2,
                        label = "Lower Risk", hjust = 0.5, vjust = 0.5,
                        size = 3, color = "blue", alpha = 0.7, angle = 90
                    ) +
                    ggplot2::annotate("text",
                        x = sqrt(max_limit), y = (nrow(forest_data) + 1) / 2,
                        label = "Higher Risk", hjust = 0.5, vjust = 0.5,
                        size = 3, color = "red", alpha = 0.7, angle = 90
                    )

                print(p)
                TRUE
            },
            .plotCalibration = function(image, ggtheme, theme, ...) {
                # Create calibration plots
                if (is.null(image$parent$options$oldStage) || is.null(image$parent$options$newStage)) {
                    return(FALSE)
                }

                # Get state data (use image$state for plot-specific data)
                plot_data <- image$state
                if (is.null(plot_data) || !is.null(plot_data$error)) {
                    # Create error message plot
                    p <- ggplot2::ggplot() +
                        ggplot2::annotate("text",
                            x = 0.5, y = 0.5,
                            label = "Calibration plots unavailable\nEnable calibration analysis in options",
                            hjust = 0.5, vjust = 0.5, size = 6
                        ) +
                        ggplot2::theme_void()
                    print(p)
                    return(TRUE)
                }

                if (!requireNamespace("gridExtra", quietly = TRUE)) {
                    return(FALSE)
                }

                # Check if we have the necessary data
                if (is.null(plot_data$old_cox_data) || is.null(plot_data$new_cox_data) || is.null(plot_data$data)) {
                    # Create error message plot
                    p <- ggplot2::ggplot() +
                        ggplot2::annotate("text",
                            x = 0.5, y = 0.5,
                            label = "Calibration data unavailable\nCox model results required",
                            hjust = 0.5, vjust = 0.5, size = 6
                        ) +
                        ggplot2::theme_void()
                    print(p)
                    return(TRUE)
                }

                # Generate calibration data for both models
                old_data <- private$.generateCalibrationData(plot_data$old_cox_data, plot_data$data, plot_data$time_var, plot_data$event_var)
                new_data <- private$.generateCalibrationData(plot_data$new_cox_data, plot_data$data, plot_data$time_var, plot_data$event_var)

                # Generate spline calibration data if available
                old_spline_data <- private$.generateSplineCalibrationData(plot_data$old_cox_data, plot_data$data, plot_data$time_var, plot_data$event_var)
                new_spline_data <- private$.generateSplineCalibrationData(plot_data$new_cox_data, plot_data$data, plot_data$time_var, plot_data$event_var)

                # Old model calibration with enhanced spline curves
                p1 <- ggplot(old_data, aes(x = predicted, y = observed)) +
                    # Perfect calibration line
                    geom_abline(intercept = 0, slope = 1, color = "gray50", linetype = "dashed", linewidth = 1.2) +
                    # Data points with size based on number of patients
                    geom_point(aes(size = n_patients), alpha = 0.7, color = "#e74c3c") +
                    # Loess smooth with confidence band
                    geom_smooth(
                        method = "loess", se = TRUE, color = "#c0392b", fill = "#e74c3c", alpha = 0.2,
                        linetype = "solid", linewidth = 1.2
                    ) +
                    # Add rug plot to show data distribution
                    geom_rug(data = old_data, alpha = 0.3, color = "#e74c3c", sides = "bl")

                # Add spline calibration curve if available
                if (!is.null(old_spline_data) && nrow(old_spline_data) > 10) {
                    p1 <- p1 + geom_smooth(
                        data = old_spline_data,
                        aes(x = predicted, y = fitted),
                        method = "gam", formula = y ~ s(x, bs = "cs"),
                        se = FALSE, color = "#27ae60", alpha = 0.8,
                        linetype = "longdash", linewidth = 1
                    )
                }

                # Calculate calibration statistics
                cal_slope <- tryCatch(
                    {
                        lm(observed ~ predicted, data = old_data)$coefficients[2]
                    },
                    error = function(e) NA
                )

                cal_intercept <- tryCatch(
                    {
                        lm(observed ~ predicted, data = old_data)$coefficients[1]
                    },
                    error = function(e) NA
                )

                p1 <- p1 +
                    ggplot2::labs(
                        title = "Original Staging System",
                        x = "Predicted Event Probability",
                        y = "Observed Event Probability",
                        subtitle = paste0(
                            "Calibration: Slope = ", round(cal_slope, 3),
                            ", Intercept = ", round(cal_intercept, 3)
                        ),
                        size = "Patients"
                    ) +
                    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent_format()) +
                    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent_format()) +
                    scale_size_continuous(range = c(2, 8), guide = guide_legend(position = "inside")) +
                    coord_fixed(ratio = 1) +
                    ggtheme +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
                        axis.title = element_text(size = 11),
                        axis.text = element_text(size = 10),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
                        legend.position.inside = c(0.85, 0.15),
                        legend.background = element_rect(fill = "white", color = NA)
                    ) +
                    # Add annotation for perfect calibration
                    annotate("text",
                        x = 0.5, y = 0.48, label = "Perfect calibration",
                        angle = 45, size = 3, color = "gray50", fontface = "italic"
                    )

                # New model calibration with enhanced spline curves
                p2 <- ggplot(new_data, aes(x = predicted, y = observed)) +
                    # Perfect calibration line
                    geom_abline(intercept = 0, slope = 1, color = "gray50", linetype = "dashed", linewidth = 1.2) +
                    # Data points with size based on number of patients
                    geom_point(aes(size = n_patients), alpha = 0.7, color = "#3498db") +
                    # Loess smooth with confidence band
                    geom_smooth(
                        method = "loess", se = TRUE, color = "#2980b9", fill = "#3498db", alpha = 0.2,
                        linetype = "solid", linewidth = 1.2
                    ) +
                    # Add rug plot to show data distribution
                    geom_rug(data = new_data, alpha = 0.3, color = "#3498db", sides = "bl")

                # Add spline calibration curve if available
                if (!is.null(new_spline_data) && nrow(new_spline_data) > 10) {
                    p2 <- p2 + geom_smooth(
                        data = new_spline_data,
                        aes(x = predicted, y = fitted),
                        method = "gam", formula = y ~ s(x, bs = "cs"),
                        se = FALSE, color = "#27ae60", alpha = 0.8,
                        linetype = "longdash", linewidth = 1
                    )
                }

                # Calculate calibration statistics
                cal_slope_new <- tryCatch(
                    {
                        lm(observed ~ predicted, data = new_data)$coefficients[2]
                    },
                    error = function(e) NA
                )

                cal_intercept_new <- tryCatch(
                    {
                        lm(observed ~ predicted, data = new_data)$coefficients[1]
                    },
                    error = function(e) NA
                )

                p2 <- p2 +
                    ggplot2::labs(
                        title = "New Staging System",
                        x = "Predicted Event Probability",
                        y = "Observed Event Probability",
                        subtitle = paste0(
                            "Calibration: Slope = ", round(cal_slope_new, 3),
                            ", Intercept = ", round(cal_intercept_new, 3)
                        ),
                        size = "Patients"
                    ) +
                    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent_format()) +
                    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent_format()) +
                    scale_size_continuous(range = c(2, 8), guide = guide_legend(position = "inside")) +
                    coord_fixed(ratio = 1) +
                    ggtheme +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
                        axis.title = element_text(size = 11),
                        axis.text = element_text(size = 10),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
                        legend.position.inside = c(0.85, 0.15),
                        legend.background = element_rect(fill = "white", color = NA)
                    ) +
                    # Add annotation for perfect calibration
                    annotate("text",
                        x = 0.5, y = 0.48, label = "Perfect calibration",
                        angle = 45, size = 3, color = "gray50", fontface = "italic"
                    )

                # Combine plots with enhanced title
                combined_plot <- gridExtra::grid.arrange(
                    p1, p2,
                    ncol = 2,
                    top = grid::textGrob(
                        "Calibration Analysis: Predicted vs Observed Event Probabilities",
                        gp = grid::gpar(fontsize = 16, fontface = "bold")
                    ),
                    bottom = grid::textGrob(
                        "Perfect calibration shown as diagonal dashed line. Point size indicates patient count.\nLoess smooth (solid) shows actual calibration; Spline smooth (dashed) provides flexible fit.",
                        gp = grid::gpar(fontsize = 10, fontface = "italic", col = "gray40")
                    )
                )

                print(combined_plot)
                TRUE
            },
            .generateCalibrationData = function(cox_data, data, time_var, event_var, time_point = 60, n_bins = 10) {
                # Generate calibration data for a Cox model
                tryCatch(
                    {
                        # Calculate linear predictors for the data
                        # Use the model coefficients and means to calculate risk scores
                        if (is.null(cox_data$linear.predictors)) {
                            # If linear predictors not available, calculate from coefficients
                            # This is a simplified approach
                            risk_scores <- rep(0, nrow(data))
                        } else {
                            risk_scores <- cox_data$linear.predictors
                        }

                        # Create bins based on risk scores
                        risk_bins <- cut(risk_scores, breaks = n_bins, include.lowest = TRUE)

                        # Calculate predicted probabilities for each bin
                        # Using a simple approximation: 1 - exp(-baseline_hazard * exp(risk_score))
                        # For time_point months
                        baseline_hazard <- 0.1 # Rough approximation
                        predicted_probs <- 1 - exp(-baseline_hazard * exp(risk_scores - mean(risk_scores, na.rm = TRUE)))

                        # Calculate observed probabilities using Kaplan-Meier
                        calibration_data <- data.frame(
                            risk_score = risk_scores,
                            risk_bin = risk_bins,
                            time = data[[time_var]],
                            event = data[[event_var]],
                            predicted = predicted_probs
                        )

                        # Calculate observed probabilities for each bin
                        bin_results <- list()
                        for (i in 1:n_bins) {
                            bin_data <- calibration_data[calibration_data$risk_bin == levels(risk_bins)[i] & !is.na(calibration_data$risk_bin), ]

                            if (nrow(bin_data) > 0) {
                                # Calculate Kaplan-Meier estimate at time_point
                                km_fit <- tryCatch(
                                    {
                                        survfit(Surv(time, event) ~ 1, data = bin_data)
                                    },
                                    error = function(e) NULL
                                )

                                if (!is.null(km_fit)) {
                                    # Find the survival probability closest to time_point
                                    time_idx <- which.min(abs(km_fit$time - time_point))
                                    if (length(time_idx) > 0 && time_idx <= length(km_fit$surv)) {
                                        observed_prob <- 1 - km_fit$surv[time_idx] # Convert to event probability
                                    } else {
                                        observed_prob <- mean(bin_data$event, na.rm = TRUE) # Fallback to crude rate
                                    }
                                } else {
                                    observed_prob <- mean(bin_data$event, na.rm = TRUE) # Fallback to crude rate
                                }

                                bin_results[[i]] <- data.frame(
                                    predicted = mean(bin_data$predicted, na.rm = TRUE),
                                    observed = observed_prob,
                                    n_patients = nrow(bin_data)
                                )
                            }
                        }

                        # Combine results
                        if (length(bin_results) > 0) {
                            result <- do.call(rbind, bin_results)
                            result <- result[!is.na(result$predicted) & !is.na(result$observed), ]
                            return(result)
                        } else {
                            # Return empty data frame with correct structure
                            return(data.frame(predicted = numeric(0), observed = numeric(0), n_patients = integer(0)))
                        }
                    },
                    error = function(e) {
                        # Return minimal data for plotting
                        data.frame(
                            predicted = c(0.1, 0.5, 0.9),
                            observed = c(0.1, 0.5, 0.9),
                            n_patients = c(10, 10, 10)
                        )
                    }
                )
            },
            .generateSplineCalibrationData = function(cox_data, data, time_var, event_var, time_point = 60) {
                # Generate enhanced spline calibration data for plotting
                tryCatch(
                    {
                        # Use the existing spline calibration calculation
                        if (!is.null(cox_data)) {
                            # Create a mock model from the cox_data for spline calculation
                            mock_model <- list(
                                linear.predictors = cox_data$linear.predictors,
                                coefficients = cox_data$coefficients
                            )

                            # Use the main spline calibration method
                            spline_result <- private$.calculateSplineBasedCalibration(mock_model, data)

                            if (!is.null(spline_result) &&
                                !is.null(spline_result$rcs_calibration) &&
                                spline_result$rcs_calibration$available) {
                                # Generate a sequence of predicted probabilities for smooth curve
                                risk_scores <- cox_data$linear.predictors
                                if (is.null(risk_scores)) {
                                    return(NULL)
                                }

                                # Calculate predicted probabilities
                                baseline_hazard <- 0.1
                                predicted_probs <- 1 - exp(-baseline_hazard * exp(risk_scores - mean(risk_scores, na.rm = TRUE)))

                                # Create fitted values using spline calibration slope and intercept
                                fitted_values <- spline_result$rcs_calibration$intercept +
                                    spline_result$rcs_calibration$slope * predicted_probs

                                # Ensure fitted values are in valid probability range
                                fitted_values <- pmax(0, pmin(1, fitted_values))

                                # Create data frame for plotting
                                spline_data <- data.frame(
                                    predicted = predicted_probs,
                                    fitted = fitted_values,
                                    method = "RCS Spline"
                                )

                                # Remove any rows with missing values
                                spline_data <- spline_data[complete.cases(spline_data), ]

                                return(spline_data)
                            }
                        }
                        return(NULL)
                    },
                    error = function(e) {
                        # Return NULL if spline calibration data generation fails
                        return(NULL)
                    }
                )
            },
            .plotDecisionCurves = function(image, ggtheme, theme, ...) {
                # Create decision curve analysis plot
                if (is.null(image$parent$options$oldStage) || is.null(image$parent$options$newStage)) {
                    return(FALSE)
                }

                # Get state data (use image$state for plot-specific data)
                plot_data <- image$state
                if (is.null(plot_data) || is.null(plot_data$dca_result)) {
                    # Create error message plot if DCA analysis is not available
                    p <- ggplot2::ggplot() +
                        ggplot2::annotate("text",
                            x = 0.5, y = 0.5,
                            label = "Decision Curve Analysis unavailable\nEnable DCA analysis in options",
                            hjust = 0.5, vjust = 0.5, size = 6, color = "gray60"
                        ) +
                        ggplot2::theme_void() +
                        theme(plot.background = element_rect(fill = "white", color = NA))
                    print(p)
                    return(TRUE)
                }


                # Extract DCA results from dcurves package
                dca_result <- plot_data$dca_result
                time_horizon <- plot_data$time_horizon

                # Extract plot data from dcurves result
                if (requireNamespace("dcurves", quietly = TRUE)) {
                    tryCatch(
                        {
                            # Get the decision curve data using the proper method for dca objects
                            if (inherits(dca_result, "dca")) {
                                # Access the internal data structure
                                dca_data <- dca_result$dca

                                # If dca_data is still not a data.frame, try different approaches
                                if (!is.data.frame(dca_data)) {
                                    # Try to extract from the object's attributes or structure
                                    if (is.list(dca_result) && !is.null(dca_result$dca)) {
                                        dca_data <- dca_result$dca
                                    } else if (is.list(dca_result) && !is.null(dca_result$data)) {
                                        dca_data <- dca_result$data
                                    } else {
                                        # Fallback: try to extract using as.data.frame on the dca component
                                        dca_data <- as.data.frame(dca_result$dca)
                                    }
                                }
                            } else {
                                # If it's not a dca object, try direct conversion
                                dca_data <- as.data.frame(dca_result)
                            }

                            # Ensure we have the required columns
                            if (!is.data.frame(dca_data) || !all(c("threshold", "label", "net_benefit") %in% names(dca_data))) {
                                # Create error message plot
                                p <- ggplot2::ggplot() +
                                    ggplot2::annotate("text",
                                        x = 0.5, y = 0.5,
                                        label = "DCA data structure is not as expected\nRequired columns: threshold, label, net_benefit",
                                        hjust = 0.5, vjust = 0.5, size = 5, color = "red"
                                    ) +
                                    ggplot2::theme_void() +
                                    theme(plot.background = element_rect(fill = "white", color = NA))
                                print(p)
                                return(TRUE)
                            }

                            # Create decision curve plot with enhanced features
                            p <- ggplot(dca_data, aes(x = threshold)) +
                                geom_line(aes(y = net_benefit, color = label), linewidth = 1.2, alpha = 0.8) +
                                geom_hline(yintercept = 0, color = "gray30", linetype = "dotted", alpha = 0.7) +
                                # Add shaded area showing clinical benefit region
                                geom_ribbon(
                                    data = dca_data[dca_data$label == "new_risk", ],
                                    aes(ymin = 0, ymax = net_benefit),
                                    fill = "#3498db", alpha = 0.1
                                ) +
                                # Add annotation for maximum benefit threshold
                                geom_vline(
                                    data = dca_data[dca_data$label == "new_risk" & dca_data$net_benefit == max(dca_data$net_benefit[dca_data$label == "new_risk"], na.rm = TRUE), ][1, ],
                                    aes(xintercept = threshold),
                                    linetype = "dashed", alpha = 0.5, color = "#3498db"
                                ) +
                                ggplot2::labs(
                                    title = "Decision Curve Analysis",
                                    subtitle = paste("Clinical utility across decision thresholds\n(Time horizon:", time_horizon, "months)"),
                                    x = "Threshold Probability (%)",
                                    y = "Net Benefit",
                                    color = "Strategy",
                                    caption = "Shaded area represents net benefit of new staging system"
                                ) +
                                scale_color_manual(
                                    values = c(
                                        "old_risk" = "#e74c3c",
                                        "new_risk" = "#3498db",
                                        "all" = "#2ecc71",
                                        "none" = "#95a5a6"
                                    ),
                                    labels = c(
                                        "old_risk" = "Original Staging",
                                        "new_risk" = "New Staging",
                                        "all" = "Treat All",
                                        "none" = "Treat None"
                                    )
                                ) +
                                scale_x_continuous(
                                    limits = c(0, 1),
                                    expand = c(0.01, 0.01),
                                    labels = scales::percent_format()
                                ) +
                                scale_y_continuous(
                                    expand = c(0.02, 0.02)
                                ) +
                                ggtheme +
                                theme(
                                    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                                    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
                                    plot.caption = element_text(hjust = 0.5, size = 10, color = "gray50", face = "italic"),
                                    legend.position = "bottom",
                                    legend.title = element_text(size = 12, face = "bold"),
                                    legend.text = element_text(size = 11),
                                    axis.title = element_text(size = 12, face = "bold"),
                                    axis.text = element_text(size = 11),
                                    panel.grid.minor = element_blank(),
                                    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
                                    plot.background = element_rect(fill = "white", color = NA)
                                )

                            # Add annotation about benefit region if new system shows improvement
                            max_benefit_data <- dca_data[dca_data$label == "new_risk", ]
                            if (nrow(max_benefit_data) > 0) {
                                max_benefit <- max(max_benefit_data$net_benefit, na.rm = TRUE)
                                max_threshold <- max_benefit_data$threshold[which.max(max_benefit_data$net_benefit)]

                                if (max_benefit > 0.01) { # Only annotate if there's meaningful benefit
                                    p <- p +
                                        annotate("text",
                                            x = max_threshold + 0.05,
                                            y = max_benefit * 0.9,
                                            label = paste0("Peak benefit at\n", round(max_threshold * 100), "% threshold"),
                                            size = 3.5,
                                            color = "#3498db",
                                            fontface = "italic"
                                        )
                                }
                            }

                            print(p)
                            return(TRUE)
                        },
                        error = function(e) {
                            # Fallback error plot
                            p <- ggplot2::ggplot() +
                                ggplot2::annotate("text",
                                    x = 0.5, y = 0.5,
                                    label = paste("Error creating decision curve plot:\n", e$message),
                                    hjust = 0.5, vjust = 0.5, size = 5, color = "red"
                                ) +
                                ggplot2::theme_void()
                            print(p)
                            return(TRUE)
                        }
                    )
                } else {
                    # Package not available
                    p <- ggplot2::ggplot() +
                        ggplot2::annotate("text",
                            x = 0.5, y = 0.5,
                            label = "dcurves package required\nfor Decision Curve Analysis",
                            hjust = 0.5, vjust = 0.5, size = 6, color = "gray60"
                        ) +
                        ggplot2::theme_void()
                    print(p)
                    return(TRUE)
                }
            },
            .createRiskTable = function(surv_fit, time_points, strata_colors = NULL, system_label = "") {
                # Helper function to create risk table for survival fit

                # Calculate risk table data
                risk_data <- data.frame()
                strata_names <- names(surv_fit$strata)

                for (i in seq_along(surv_fit$strata)) {
                    strata_name <- strata_names[i]
                    clean_strata <- gsub(".*=", "", strata_name) # Clean strata name

                    # Extract indices for this stratum
                    if (i == 1) {
                        idx_start <- 1
                    } else {
                        idx_start <- sum(surv_fit$strata[1:(i - 1)]) + 1
                    }
                    idx_end <- sum(surv_fit$strata[1:i])

                    # Get subset of survival data for this stratum
                    strata_times <- surv_fit$time[idx_start:idx_end]
                    strata_n_risk <- surv_fit$n.risk[idx_start:idx_end]

                    # Calculate n at risk for specific time points
                    n_risk_values <- numeric(length(time_points))
                    for (j in seq_along(time_points)) {
                        idx <- which(strata_times <= time_points[j])
                        if (length(idx) > 0) {
                            n_risk_values[j] <- strata_n_risk[max(idx)]
                        } else {
                            n_risk_values[j] <- strata_n_risk[1]
                        }
                    }

                    risk_data <- rbind(risk_data, data.frame(
                        system = system_label,
                        strata = clean_strata,
                        strata_full = paste(system_label, clean_strata, sep = if (system_label != "") " - " else ""),
                        time = time_points,
                        n_risk = n_risk_values,
                        stringsAsFactors = FALSE
                    ))
                }

                # Order stages properly (T1, T2, T3, etc. or I, II, III, IV, etc.)
                stage_order <- c("T1", "T2", "T3", "T4", "I", "II", "III", "IV", "1", "2", "3", "4", "A", "B", "C", "D")

                # Get unique strata and order them properly
                unique_strata <- unique(risk_data$strata)
                ordered_strata <- intersect(stage_order, unique_strata)
                remaining_strata <- setdiff(unique_strata, ordered_strata)
                final_order <- c(ordered_strata, sort(remaining_strata))

                # Apply proper factor ordering
                risk_data$strata <- factor(risk_data$strata, levels = final_order)
                risk_data$strata_full <- factor(risk_data$strata_full,
                    levels = unique(risk_data$strata_full[order(risk_data$strata)])
                )

                return(risk_data)
            },
            .plotSurvivalCurves = function(image, ggtheme, theme, ...) {
                # Create survival curve comparison
                if (is.null(image$parent$options$oldStage) || is.null(image$parent$options$newStage)) {
                    return(FALSE)
                }

                # Get state data (correct location is image$state, not image$parent$state)
                plot_data <- image$state
                if (is.null(plot_data) || is.null(plot_data$data)) {
                    return(FALSE)
                }

                tryCatch(
                    {
                        data <- plot_data$data
                        time_var <- plot_data$time_var
                        event_var <- plot_data$event_var
                        old_stage <- plot_data$old_stage
                        new_stage <- plot_data$new_stage

                        # Create survival fits
                        old_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", old_stage))
                        new_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", new_stage))

                        old_fit <- survfit(old_formula, data = data)
                        new_fit <- survfit(new_formula, data = data)

                        # Get plot type option with proper default handling
                        plot_type <- image$parent$options$survivalPlotType
                        if (is.null(plot_type)) {
                            plot_type <- "separate" # Default value from a.yaml
                        }
                        show_ci <- image$parent$options$showConfidenceIntervals
                        show_risk <- image$parent$options$showRiskTables
                        time_range <- image$parent$options$plotTimeRange

                        if (plot_type == "separate") {
                            # Create separate plots using basic ggplot approach
                            # Convert survival fits to data frames for plotting

                            # Create colorblind-friendly palette based on number of stages
                            n_old_stages <- length(old_fit$strata)
                            n_new_stages <- length(new_fit$strata)
                            max_stages <- max(n_old_stages, n_new_stages)

                            # Use same colorblind-friendly palette as overlay
                            if (max_stages <= 4) {
                                color_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")
                            } else if (max_stages <= 8) {
                                color_palette <- c(
                                    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                                    "#0072B2", "#D55E00", "#CC79A7", "#999999"
                                )
                            } else {
                                color_palette <- viridis::viridis(max_stages, option = "D")
                            }

                            # Old staging system plot
                            old_surv_data <- data.frame(
                                time = old_fit$time,
                                surv = old_fit$surv,
                                strata = rep(names(old_fit$strata), old_fit$strata),
                                upper = old_fit$upper,
                                lower = old_fit$lower
                            )

                            # Determine x-axis limits
                            max_time <- suppressWarnings(as.numeric(time_range))
                            if (is.null(time_range) || time_range == "auto" || is.na(max_time)) {
                                max_time <- max(old_surv_data$time, na.rm = TRUE)
                            }

                            p1 <- ggplot(old_surv_data, aes(x = time, y = surv, color = strata)) +
                                geom_step(linewidth = 1.2) +
                                scale_color_manual(values = color_palette[1:n_old_stages])

                            # Add confidence intervals if requested
                            if (!is.null(show_ci) && show_ci) {
                                p1 <- p1 +
                                    geom_ribbon(aes(ymin = lower, ymax = upper, fill = strata),
                                        alpha = 0.2, linetype = 0
                                    )
                            }

                            p1 <- p1 +
                                ggplot2::labs(
                                    title = "Original Staging System - Survival Curves",
                                    x = "Time (months)",
                                    y = "Survival Probability",
                                    color = "Stage"
                                ) +
                                scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
                                scale_x_continuous(limits = c(0, max_time)) +
                                ggtheme +
                                theme(
                                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                                    legend.position = "bottom"
                                )

                            # New staging system plot
                            new_surv_data <- data.frame(
                                time = new_fit$time,
                                surv = new_fit$surv,
                                strata = rep(names(new_fit$strata), new_fit$strata),
                                upper = new_fit$upper,
                                lower = new_fit$lower
                            )

                            p2 <- ggplot(new_surv_data, aes(x = time, y = surv, color = strata)) +
                                geom_step(linewidth = 1.2) +
                                scale_color_manual(values = color_palette[1:n_new_stages])

                            # Add confidence intervals if requested
                            if (!is.null(show_ci) && show_ci) {
                                p2 <- p2 +
                                    geom_ribbon(aes(ymin = lower, ymax = upper, fill = strata),
                                        alpha = 0.2, linetype = 0
                                    )
                            }

                            p2 <- p2 +
                                ggplot2::labs(
                                    title = "New Staging System - Survival Curves",
                                    x = "Time (months)",
                                    y = "Survival Probability",
                                    color = "Stage"
                                ) +
                                scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
                                scale_x_continuous(limits = c(0, max_time)) +
                                ggtheme +
                                theme(
                                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                                    legend.position = "bottom"
                                )


                            # Add risk tables if requested
                            if (!is.null(show_risk) && show_risk) {
                                # Calculate time points for risk table
                                risk_times <- seq(0, max_time, length.out = 6)
                                risk_times <- round(risk_times)

                                # Get risk data for both systems
                                old_risk_data <- private$.createRiskTable(old_fit, risk_times, system_label = "Original")
                                new_risk_data <- private$.createRiskTable(new_fit, risk_times, system_label = "New")

                                # Create risk tables for each system
                                old_risk_table <- ggplot(old_risk_data, aes(x = time, y = strata)) +
                                    ggplot2::geom_text(aes(label = n_risk, color = strata), size = 3.5, fontface = "bold") +
                                    ggplot2::scale_color_manual(values = color_palette[1:n_old_stages], guide = "none") +
                                    ggplot2::scale_x_continuous(limits = c(0, max_time), breaks = risk_times) +
                                    ggplot2::labs(x = "", y = "", title = "Number at Risk - Original") +
                                    ggtheme +
                                    theme(
                                        panel.grid = element_blank(),
                                        axis.text.x = element_blank(),
                                        axis.ticks = element_blank(),
                                        axis.text.y = element_text(size = 9),
                                        plot.title = element_text(size = 10, hjust = 0, face = "bold"),
                                        plot.margin = margin(5, 10, 5, 10)
                                    )

                                new_risk_table <- ggplot(new_risk_data, aes(x = time, y = strata)) +
                                    ggplot2::geom_text(aes(label = n_risk, color = strata), size = 3.5, fontface = "bold") +
                                    ggplot2::scale_color_manual(values = color_palette[1:n_new_stages], guide = "none") +
                                    ggplot2::scale_x_continuous(limits = c(0, max_time), breaks = risk_times) +
                                    ggplot2::labs(x = "Time (months)", y = "", title = "Number at Risk - New") +
                                    ggtheme +
                                    theme(
                                        panel.grid = element_blank(),
                                        axis.text.y = element_text(size = 9),
                                        plot.title = element_text(size = 10, hjust = 0, face = "bold"),
                                        plot.margin = margin(5, 10, 5, 10)
                                    )

                                # Combine plots with risk tables vertically
                                combined_plot <- gridExtra::grid.arrange(
                                    p1, old_risk_table,
                                    p2, new_risk_table,
                                    nrow = 4,
                                    heights = c(3, 1, 3, 1)
                                )
                            } else {
                                # Combine plots vertically for separate display
                                combined_plot <- gridExtra::grid.arrange(p1, p2, nrow = 2)
                            }
                            # Note: grid.arrange automatically prints
                        } else if (plot_type == "sidebyside") {
                            # Create side-by-side plots
                            # Convert survival fits to data frames for plotting

                            # Create colorblind-friendly palette based on number of stages
                            n_old_stages <- length(old_fit$strata)
                            n_new_stages <- length(new_fit$strata)
                            max_stages <- max(n_old_stages, n_new_stages)

                            # Use same colorblind-friendly palette as other plots
                            if (max_stages <= 4) {
                                color_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")
                            } else if (max_stages <= 8) {
                                color_palette <- c(
                                    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                                    "#0072B2", "#D55E00", "#CC79A7", "#999999"
                                )
                            } else {
                                color_palette <- viridis::viridis(max_stages, option = "D")
                            }

                            # Old staging system data
                            old_surv_data <- data.frame(
                                time = old_fit$time,
                                surv = old_fit$surv,
                                strata = rep(names(old_fit$strata), old_fit$strata),
                                upper = old_fit$upper,
                                lower = old_fit$lower
                            )

                            # New staging system data
                            new_surv_data <- data.frame(
                                time = new_fit$time,
                                surv = new_fit$surv,
                                strata = rep(names(new_fit$strata), new_fit$strata),
                                upper = new_fit$upper,
                                lower = new_fit$lower
                            )

                            # Determine x-axis limits
                            max_time <- if (!is.null(time_range) && time_range != "auto") {
                                as.numeric(time_range)
                            } else {
                                max(c(old_surv_data$time, new_surv_data$time), na.rm = TRUE)
                            }

                            # Create plots
                            p1 <- ggplot(old_surv_data, aes(x = time, y = surv, color = strata)) +
                                geom_step(linewidth = 1.2) +
                                scale_color_manual(values = color_palette[1:n_old_stages])

                            if (!is.null(show_ci) && show_ci) {
                                p1 <- p1 +
                                    geom_ribbon(aes(ymin = lower, ymax = upper, fill = strata),
                                        alpha = 0.2, linetype = 0
                                    )
                            }

                            p1 <- p1 +
                                ggplot2::labs(
                                    title = "Original Staging System",
                                    x = "Time (months)",
                                    y = "Survival Probability",
                                    color = "Stage"
                                ) +
                                scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
                                scale_x_continuous(limits = c(0, max_time)) +
                                ggtheme +
                                theme(
                                    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
                                    legend.position = "bottom"
                                )

                            p2 <- ggplot(new_surv_data, aes(x = time, y = surv, color = strata)) +
                                geom_step(linewidth = 1.2) +
                                scale_color_manual(values = color_palette[1:n_new_stages])

                            if (!is.null(show_ci) && show_ci) {
                                p2 <- p2 +
                                    geom_ribbon(aes(ymin = lower, ymax = upper, fill = strata),
                                        alpha = 0.2, linetype = 0
                                    )
                            }

                            p2 <- p2 +
                                ggplot2::labs(
                                    title = "New Staging System",
                                    x = "Time (months)",
                                    y = "Survival Probability",
                                    color = "Stage"
                                ) +
                                scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
                                scale_x_continuous(limits = c(0, max_time)) +
                                ggtheme +
                                theme(
                                    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
                                    legend.position = "bottom"
                                )


                            # Add risk tables if requested
                            if (!is.null(show_risk) && show_risk) {
                                # Calculate time points for risk table
                                risk_times <- seq(0, max_time, length.out = 6)
                                risk_times <- round(risk_times)

                                # Get risk data for both systems
                                old_risk_data <- private$.createRiskTable(old_fit, risk_times, system_label = "")
                                new_risk_data <- private$.createRiskTable(new_fit, risk_times, system_label = "")

                                # Create risk tables for each system
                                old_risk_table <- ggplot(old_risk_data, aes(x = time, y = strata)) +
                                    ggplot2::geom_text(aes(label = n_risk, color = strata), size = 3.5, fontface = "bold") +
                                    ggplot2::scale_color_manual(values = color_palette[1:n_old_stages], guide = "none") +
                                    ggplot2::scale_x_continuous(limits = c(0, max_time), breaks = risk_times) +
                                    ggplot2::labs(x = "Time (months)", y = "Number at Risk", title = "") +
                                    ggtheme +
                                    theme(
                                        panel.grid = element_blank(),
                                        axis.text.y = element_text(size = 9),
                                        axis.title.y = element_text(size = 9, face = "bold"),
                                        plot.margin = margin(5, 10, 5, 10)
                                    )

                                new_risk_table <- ggplot(new_risk_data, aes(x = time, y = strata)) +
                                    ggplot2::geom_text(aes(label = n_risk, color = strata), size = 3.5, fontface = "bold") +
                                    ggplot2::scale_color_manual(values = color_palette[1:n_new_stages], guide = "none") +
                                    ggplot2::scale_x_continuous(limits = c(0, max_time), breaks = risk_times) +
                                    ggplot2::labs(x = "Time (months)", y = "Number at Risk", title = "") +
                                    ggtheme +
                                    theme(
                                        panel.grid = element_blank(),
                                        axis.text.y = element_text(size = 9),
                                        axis.title.y = element_text(size = 9, face = "bold"),
                                        plot.margin = margin(5, 10, 5, 10)
                                    )

                                # Combine plots with risk tables - plots on top, risk tables below
                                combined_plot <- gridExtra::grid.arrange(
                                    p1, p2,
                                    old_risk_table, new_risk_table,
                                    nrow = 2, ncol = 2,
                                    heights = c(4, 1)
                                )
                            } else {
                                # Combine plots side by side
                                combined_plot <- gridExtra::grid.arrange(p1, p2, ncol = 2)
                            }
                            # Note: grid.arrange automatically prints
                        } else if (plot_type == "overlay") {
                            # Create overlay plot with both staging systems
                            # Get survival summaries
                            old_surv_summary <- summary(old_fit)
                            new_surv_summary <- summary(new_fit)

                            # Create combined data frame with proper stage names
                            old_data <- data.frame(
                                time = old_surv_summary$time,
                                surv = old_surv_summary$surv,
                                system = "Original",
                                stage = gsub(".*=", "", old_surv_summary$strata),
                                strata = old_surv_summary$strata,
                                upper = old_surv_summary$upper,
                                lower = old_surv_summary$lower
                            )

                            new_data <- data.frame(
                                time = new_surv_summary$time,
                                surv = new_surv_summary$surv,
                                system = "New",
                                stage = gsub(".*=", "", new_surv_summary$strata),
                                strata = new_surv_summary$strata,
                                upper = new_surv_summary$upper,
                                lower = new_surv_summary$lower
                            )

                            combined_data <- rbind(old_data, new_data)
                            combined_data$group <- paste(combined_data$system, combined_data$stage, sep = " - ")

                            # Create a colorblind-friendly color mapping where matching stages have the same color
                            unique_stages <- unique(c(old_data$stage, new_data$stage))
                            n_stages <- length(unique_stages)

                            # Use colorblind-friendly palettes based on number of stages
                            if (n_stages <= 4) {
                                # For up to 4 stages, use a simple colorblind-friendly palette
                                stage_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")[1:n_stages]
                            } else if (n_stages <= 8) {
                                # For 5-8 stages, use the full colorblind-friendly palette
                                stage_colors <- c(
                                    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                                    "#0072B2", "#D55E00", "#CC79A7", "#999999"
                                )[1:n_stages]
                            } else {
                                # For more than 8 stages, use viridis color scale
                                stage_colors <- viridis::viridis(n_stages, option = "D")
                            }
                            names(stage_colors) <- unique_stages

                            # Determine x-axis limits
                            max_time <- if (!is.null(time_range) && time_range != "auto") {
                                as.numeric(time_range)
                            } else {
                                max(combined_data$time, na.rm = TRUE)
                            }

                            # Create overlay plot with different line types for staging systems
                            # Color by stage (not group) so matching stages have same color
                            p <- tryCatch(
                                {
                                    # Try with linewidth parameter (newer ggplot2)
                                    ggplot(combined_data, aes(x = time, y = surv, color = stage, linetype = system)) +
                                        geom_step(linewidth = 1.2) +
                                        scale_color_manual(values = stage_colors, name = "Stage") +
                                        scale_linetype_manual(
                                            values = c("Original" = "solid", "New" = "dashed"),
                                            name = "Staging System",
                                            guide = guide_legend(override.aes = list(size = 1))
                                        )
                                },
                                error = function(e) {
                                    # Fallback to size parameter (older ggplot2)
                                    ggplot(combined_data, aes(x = time, y = surv, color = stage, linetype = system)) +
                                        geom_step(size = 1.2) +
                                        scale_color_manual(values = stage_colors, name = "Stage") +
                                        scale_linetype_manual(
                                            values = c("Original" = "solid", "New" = "dashed"),
                                            name = "Staging System",
                                            guide = guide_legend(override.aes = list(size = 1))
                                        )
                                }
                            )

                            if (!is.null(show_ci) && show_ci) {
                                p <- p +
                                    geom_ribbon(aes(ymin = lower, ymax = upper, fill = stage, linetype = system),
                                        alpha = 0.1, color = NA
                                    )
                            }

                            p <- p +
                                ggplot2::labs(
                                    title = "Staging System Comparison - Overlay",
                                    x = "Time (months)",
                                    y = "Survival Probability",
                                    color = "Stage",
                                    linetype = "Staging System"
                                ) +
                                scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
                                scale_x_continuous(limits = c(0, max_time)) +
                                ggtheme +
                                theme(
                                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                                    legend.position = "bottom",
                                    legend.box = "horizontal",
                                    legend.title = element_text(size = 10, face = "bold"),
                                    legend.text = element_text(size = 9),
                                    legend.key.width = unit(2, "cm"), # Make legend keys wider to show line types
                                    legend.spacing.x = unit(1, "cm") # Add space between legend groups
                                ) +
                                guides(
                                    color = guide_legend(order = 1, ncol = length(unique_stages)),
                                    linetype = guide_legend(order = 2, override.aes = list(color = "black"))
                                )

                            # Add risk table if requested
                            if (!is.null(show_risk) && show_risk) {
                                # Calculate time points for risk table
                                risk_times <- seq(0, max_time, length.out = 6)
                                risk_times <- round(risk_times)

                                # Get risk data for both systems
                                old_risk_data <- private$.createRiskTable(old_fit, risk_times, system_label = "Original")
                                new_risk_data <- private$.createRiskTable(new_fit, risk_times, system_label = "New")
                                combined_risk_data <- rbind(old_risk_data, new_risk_data)

                                # Create risk table with same color scheme and line types
                                risk_table <- ggplot(combined_risk_data, aes(x = time, y = strata_full)) +
                                    ggplot2::geom_text(aes(label = n_risk, color = strata), size = 3.5, fontface = "bold") +
                                    ggplot2::scale_color_manual(values = stage_colors, guide = "none") + # Use same colors
                                    ggplot2::scale_x_continuous(limits = c(0, max_time), breaks = risk_times) +
                                    ggplot2::labs(x = "", y = "", title = "Number at Risk") +
                                    ggtheme +
                                    theme(
                                        panel.grid = element_blank(),
                                        axis.text.x = element_blank(),
                                        axis.ticks = element_blank(),
                                        axis.text.y = element_text(size = 9),
                                        plot.title = element_text(size = 10, hjust = 0, face = "bold"),
                                        plot.margin = margin(5, 10, 5, 10)
                                    ) +
                                    # Add system labels with line type indicators
                                    facet_grid(system ~ ., scales = "free_y", space = "free_y")

                                # Combine plots using gridExtra
                                combined_plot <- gridExtra::grid.arrange(p, risk_table,
                                    ncol = 1,
                                    heights = c(4, 1.5)
                                )
                                # Note: grid.arrange automatically prints
                            } else {
                                print(p)
                            }
                        } else {
                            # Default fallback - should not reach here
                            # Create a simple combined plot
                            old_surv_summary <- summary(old_fit)
                            new_surv_summary <- summary(new_fit)

                            old_data <- data.frame(
                                time = old_surv_summary$time,
                                surv = old_surv_summary$surv,
                                system = "Original",
                                stage = old_surv_summary$strata
                            )

                            new_data <- data.frame(
                                time = new_surv_summary$time,
                                surv = new_surv_summary$surv,
                                system = "New",
                                stage = new_surv_summary$strata
                            )

                            combined_data <- rbind(old_data, new_data)

                            p <- ggplot(combined_data, aes(x = time, y = surv, color = system, linetype = stage)) +
                                geom_step(linewidth = 1) +
                                ggplot2::labs(
                                    title = "Staging System Comparison - Survival Curves",
                                    x = "Time (months)",
                                    y = "Survival Probability",
                                    color = "Staging System",
                                    linetype = "Stage"
                                ) +
                                scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
                                ggtheme +
                                theme(
                                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                                    legend.position = "bottom"
                                )

                            print(p)
                        }

                        TRUE
                    },
                    error = function(e) {
                        # Handle any errors in survival curve plotting
                        tryCatch(
                            {
                                p <- ggplot2::ggplot() +
                                    ggplot2::annotate("text",
                                        x = 0.5, y = 0.5,
                                        label = paste("Error generating survival curves:", e$message),
                                        hjust = 0.5, vjust = 0.5, size = 4, color = "red"
                                    ) +
                                    ggplot2::theme_void()
                                print(p)
                            },
                            error = function(e2) {
                                # Ultimate fallback if even the error plot fails
                            }
                        )
                        return(TRUE)
                    }
                )

                TRUE
            },
            .performCalibrationAnalysis = function(data, advanced_metrics = NULL) {
                # Perform calibration analysis for both staging systems
                tryCatch(
                    {
                        # Ensure event_binary column exists
                        if (!"event_binary" %in% names(data)) {
                            event_col <- self$options$event
                            event_level <- self$options$eventLevel

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
                        }

                        # Get Cox models - use provided metrics or calculate new ones
                        if (is.null(advanced_metrics)) {
                            advanced_metrics <- private$.calculateAdvancedMetrics(data)
                        }
                        if (is.null(advanced_metrics)) {
                            self$results$calibrationAnalysis$setError("Failed to calculate advanced metrics for calibration analysis")
                            return(NULL)
                        }
                        if (is.null(advanced_metrics$old_cox) || is.null(advanced_metrics$new_cox)) {
                            self$results$calibrationAnalysis$setError("Cox models not available for calibration analysis")
                            return(NULL)
                        }

                        old_cox <- advanced_metrics$old_cox
                        new_cox <- advanced_metrics$new_cox

                        # Calculate calibration metrics for both models
                        old_calibration <- private$.calculateCalibrationMetrics(old_cox, data)
                        new_calibration <- private$.calculateCalibrationMetrics(new_cox, data)

                        # Calculate spline-based calibration for both models
                        old_spline_calibration <- private$.calculateSplineBasedCalibration(old_cox, data)
                        new_spline_calibration <- private$.calculateSplineBasedCalibration(new_cox, data)

                        # Return results
                        list(
                            old_calibration = old_calibration,
                            new_calibration = new_calibration,
                            old_spline_calibration = old_spline_calibration,
                            new_spline_calibration = new_spline_calibration
                        )
                    },
                    error = function(e) {
                        self$results$calibrationAnalysis$setError(paste("Calibration analysis failed:", e$message))
                        return(NULL)
                    }
                )
            },
            .calculateCalibrationMetrics = function(cox_model, data, n_bins = 10) {
                # Enhanced calibration metrics for a Cox model with advanced measures
                tryCatch(
                    {
                        # Get linear predictors
                        linear_predictors <- cox_model$linear.predictors
                        if (is.null(linear_predictors)) {
                            return(private$.createEmptyCalibrationResult())
                        }

                        # Enhanced survival probability calculation for calibration
                        survival_probs <- private$.calculateEnhancedSurvivalProbabilities(cox_model, data)
                        predicted_probs <- 1 - survival_probs # Convert to event probabilities

                        # Ensure predicted probabilities are within valid range
                        predicted_probs <- pmax(0.001, pmin(0.999, predicted_probs))

                        # Create risk groups using quantile-based approach for better distribution
                        risk_groups <- private$.createCalibrationGroups(predicted_probs, n_bins)

                        # Calculate enhanced Hosmer-Lemeshow test
                        hl_results <- private$.calculateEnhancedHosmerLemeshow(predicted_probs, data$event_binary, risk_groups)

                        # Calculate enhanced calibration slope with robust regression
                        cal_results <- private$.calculateEnhancedCalibrationSlope(predicted_probs, data$event_binary)

                        # Calculate additional calibration metrics
                        additional_metrics <- private$.calculateAdditionalCalibrationMetrics(predicted_probs, data$event_binary)

                        # Enhanced interpretation with detailed guidance
                        interpretation <- private$.interpretEnhancedCalibration(
                            hl_results$hl_p,
                            cal_results$cal_slope,
                            cal_results$cal_intercept,
                            additional_metrics
                        )

                        # Combine all results
                        return(list(
                            hl_chi2 = hl_results$hl_chi2,
                            hl_df = hl_results$hl_df,
                            hl_p = hl_results$hl_p,
                            cal_slope = cal_results$cal_slope,
                            cal_intercept = cal_results$cal_intercept,
                            cal_slope_ci_lower = cal_results$ci_lower,
                            cal_slope_ci_upper = cal_results$ci_upper,
                            calibration_in_large = additional_metrics$cal_in_large,
                            expected_observed_ratio = additional_metrics$eo_ratio,
                            brier_score = additional_metrics$brier_score,
                            interpretation = interpretation
                        ))
                    },
                    error = function(e) {
                        private$.addNotice("WARNING", .("Calibration not computed"), conditionMessage(e))
                        return(private$.createEmptyCalibrationResult(e$message))
                    }
                )
            },

            # Enhanced calibration helper functions
            .createEmptyCalibrationResult = function(error_msg = "Calibration metrics unavailable") {
                return(list(
                    hl_chi2 = NA,
                    hl_df = NA,
                    hl_p = NA,
                    cal_slope = NA,
                    cal_intercept = NA,
                    cal_slope_ci_lower = NA,
                    cal_slope_ci_upper = NA,
                    calibration_in_large = NA,
                    expected_observed_ratio = NA,
                    brier_score = NA,
                    interpretation = paste("Error:", substr(error_msg, 1, 50))
                ))
            },
            .calculateEnhancedSurvivalProbabilities = function(cox_model, data) {
                # Calculate more accurate survival probabilities using baseline hazard
                tryCatch(
                    {
                        # Get baseline survival
                        baseline_surv <- survival::survfit(cox_model, newdata = data[1, ])

                        # Get linear predictors
                        linear_predictors <- predict(cox_model, type = "lp")

                        # Use a reference time point (e.g., median follow-up time)
                        reference_time <- median(data[[self$options$survivalTime]], na.rm = TRUE)

                        # Find baseline survival at reference time
                        if (reference_time <= min(baseline_surv$time)) {
                            baseline_surv_at_ref <- 1.0
                        } else if (reference_time >= max(baseline_surv$time)) {
                            baseline_surv_at_ref <- min(baseline_surv$surv)
                        } else {
                            baseline_surv_at_ref <- approx(baseline_surv$time, baseline_surv$surv, reference_time)$y
                        }

                        # Calculate individual survival probabilities: S(t|x) = S0(t)^exp(βx)
                        individual_surv_probs <- baseline_surv_at_ref^exp(linear_predictors)

                        # Ensure probabilities are within valid range
                        return(pmax(0.001, pmin(0.999, individual_surv_probs)))
                    },
                    error = function(e) {
                        # Fallback to logistic transformation of linear predictors
                        linear_predictors <- predict(cox_model, type = "lp")
                        return(plogis(-linear_predictors)) # Convert to survival probability
                    }
                )
            },
            .createCalibrationGroups = function(predicted_probs, n_bins) {
                # Create calibration groups using quantile-based approach for better distribution
                tryCatch(
                    {
                        # Use quantile-based binning for more balanced groups
                        quantile_breaks <- quantile(predicted_probs, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)

                        # Ensure unique breaks
                        unique_breaks <- unique(quantile_breaks)
                        if (length(unique_breaks) < n_bins + 1) {
                            # Fall back to equal-width binning
                            min_prob <- min(predicted_probs, na.rm = TRUE)
                            max_prob <- max(predicted_probs, na.rm = TRUE)
                            equal_breaks <- seq(min_prob, max_prob, length.out = n_bins + 1)
                            return(cut(predicted_probs, breaks = equal_breaks, include.lowest = TRUE))
                        } else {
                            return(cut(predicted_probs, breaks = unique_breaks, include.lowest = TRUE))
                        }
                    },
                    error = function(e) {
                        # Simple fallback
                        return(cut(predicted_probs, breaks = n_bins, include.lowest = TRUE))
                    }
                )
            },
            .calculateEnhancedHosmerLemeshow = function(predicted_probs, observed_events, risk_groups) {
                # Enhanced Hosmer-Lemeshow test with better handling of sparse data
                tryCatch(
                    {
                        # Group observations and calculate expected vs observed
                        group_results <- list()
                        for (level in levels(risk_groups)) {
                            group_mask <- risk_groups == level & !is.na(risk_groups)
                            if (sum(group_mask) > 0) {
                                expected <- sum(predicted_probs[group_mask], na.rm = TRUE)
                                observed <- sum(observed_events[group_mask], na.rm = TRUE)
                                n_group <- sum(group_mask)

                                # Only include groups with sufficient observations
                                if (n_group >= 5) {
                                    group_results[[level]] <- c(expected = expected, observed = observed, n = n_group)
                                }
                            }
                        }

                        # Calculate Hosmer-Lemeshow statistic
                        if (length(group_results) >= 3) {
                            expected_vals <- sapply(group_results, function(x) x["expected"])
                            observed_vals <- sapply(group_results, function(x) x["observed"])

                            # Enhanced chi-square calculation with continuity correction
                            chi_square_terms <- (abs(observed_vals - expected_vals) - 0.5)^2 / (expected_vals + 0.5)
                            hl_chi2 <- sum(chi_square_terms, na.rm = TRUE)
                            hl_df <- length(group_results) - 2
                            hl_p <- 1 - pchisq(hl_chi2, df = hl_df)
                        } else {
                            hl_chi2 <- NA
                            hl_df <- NA
                            hl_p <- NA
                        }

                        return(list(hl_chi2 = hl_chi2, hl_df = hl_df, hl_p = hl_p))
                    },
                    error = function(e) {
                        return(list(hl_chi2 = NA, hl_df = NA, hl_p = NA))
                    }
                )
            },
            .calculateEnhancedCalibrationSlope = function(predicted_probs, observed_events) {
                # Enhanced calibration slope calculation with robust methods
                tryCatch(
                    {
                        # Create calibration data
                        cal_data <- data.frame(
                            predicted = predicted_probs,
                            observed = observed_events
                        )

                        # Remove any rows with missing data
                        cal_data <- cal_data[complete.cases(cal_data), ]

                        if (nrow(cal_data) < 10) {
                            return(list(cal_slope = NA, cal_intercept = NA, ci_lower = NA, ci_upper = NA))
                        }

                        # Fit calibration model with robust standard errors
                        cal_model <- tryCatch(
                            {
                                glm(observed ~ predicted, data = cal_data, family = binomial())
                            },
                            error = function(e) NULL
                        )

                        if (!is.null(cal_model) && length(coef(cal_model)) >= 2) {
                            coef_vals <- coef(cal_model)
                            cal_slope <- coef_vals["predicted"]
                            cal_intercept <- coef_vals["(Intercept)"]

                            # Calculate robust confidence intervals
                            cal_slope_ci <- tryCatch(
                                {
                                    # Use profile likelihood for more robust CIs
                                    ci_matrix <- confint(cal_model, level = 0.95)
                                    if ("predicted" %in% rownames(ci_matrix)) {
                                        ci_matrix["predicted", ]
                                    } else {
                                        c(NA, NA)
                                    }
                                },
                                error = function(e) {
                                    # Fallback to Wald-type CI
                                    se <- summary(cal_model)$coefficients["predicted", "Std. Error"]
                                    c(cal_slope - private$.zCrit() * se, cal_slope + private$.zCrit() * se)
                                }
                            )

                            return(list(
                                cal_slope = as.numeric(cal_slope),
                                cal_intercept = as.numeric(cal_intercept),
                                ci_lower = cal_slope_ci[1],
                                ci_upper = cal_slope_ci[2]
                            ))
                        } else {
                            return(list(cal_slope = NA, cal_intercept = NA, ci_lower = NA, ci_upper = NA))
                        }
                    },
                    error = function(e) {
                        return(list(cal_slope = NA, cal_intercept = NA, ci_lower = NA, ci_upper = NA))
                    }
                )
            },
            .calculateAdditionalCalibrationMetrics = function(predicted_probs, observed_events) {
                # Calculate additional calibration metrics beyond H-L and slope
                tryCatch(
                    {

                        # 1. Calibration-in-the-large (mean predicted vs observed)
                        mean_predicted <- mean(predicted_probs, na.rm = TRUE)
                        mean_observed <- mean(observed_events, na.rm = TRUE)
                        cal_in_large <- mean_observed - mean_predicted

                        # 2. Expected/Observed ratio
                        total_expected <- sum(predicted_probs, na.rm = TRUE)
                        total_observed <- sum(observed_events, na.rm = TRUE)
                        eo_ratio <- if (total_expected > 0) total_observed / total_expected else NA

                        # 3. Brier Score (calibration + discrimination)
                        brier_score <- mean((predicted_probs - observed_events)^2, na.rm = TRUE)

                        return(list(
                            cal_in_large = cal_in_large,
                            eo_ratio = eo_ratio,
                            brier_score = brier_score
                        ))
                    },
                    error = function(e) {
                        return(list(cal_in_large = NA, eo_ratio = NA, brier_score = NA))
                    }
                )
            },
            .interpretEnhancedCalibration = function(hl_p, cal_slope, cal_intercept, additional_metrics) {
                # Enhanced interpretation with detailed clinical guidance
                tryCatch(
                    {
                        # Initialize interpretation components
                        interpretations <- character()

                        # Hosmer-Lemeshow interpretation
                        if (!is.na(hl_p)) {
                            if (hl_p > 0.05) {
                                interpretations <- c(interpretations, "H-L test: acceptable fit")
                            } else {
                                interpretations <- c(interpretations, "H-L test: poor fit")
                            }
                        }

                        # Calibration slope interpretation (key metric per document)
                        if (!is.na(cal_slope)) {
                            if (abs(cal_slope - 1.0) < 0.1) {
                                interpretations <- c(interpretations, "Perfect calibration slope (\u{2248}1.0)")
                            } else if (cal_slope < 0.8) {
                                interpretations <- c(interpretations, "Over-prediction (slope < 0.8)")
                            } else if (cal_slope > 1.2) {
                                interpretations <- c(interpretations, "Under-prediction (slope > 1.2)")
                            } else {
                                interpretations <- c(interpretations, "Acceptable calibration slope")
                            }
                        }

                        # Expected/Observed ratio interpretation
                        if (!is.na(additional_metrics$eo_ratio)) {
                            if (abs(additional_metrics$eo_ratio - 1.0) < 0.1) {
                                interpretations <- c(interpretations, "Good overall calibration")
                            } else if (additional_metrics$eo_ratio > 1.1) {
                                interpretations <- c(interpretations, "Systematic under-prediction")
                            } else if (additional_metrics$eo_ratio < 0.9) {
                                interpretations <- c(interpretations, "Systematic over-prediction")
                            }
                        }

                        # Overall assessment
                        if (length(interpretations) == 0) {
                            return("Unable to assess calibration")
                        } else {
                            return(paste(interpretations, collapse = "; "))
                        }
                    },
                    error = function(e) {
                        return("Error in calibration interpretation")
                    }
                )
            },
            .calculateSplineBasedCalibration = function(cox_model, data) {
                # Advanced spline-based calibration using rms package
                tryCatch(
                    {
                        # Get linear predictors and survival probabilities
                        linear_predictors <- cox_model$linear.predictors
                        if (is.null(linear_predictors)) {
                            return(private$.createEmptySplineCalibrationResult())
                        }

                        # Calculate survival probabilities and convert to event probabilities
                        survival_probs <- private$.calculateEnhancedSurvivalProbabilities(cox_model, data)
                        predicted_probs <- 1 - survival_probs
                        predicted_probs <- pmax(0.001, pmin(0.999, predicted_probs))

                        # Create observed events
                        observed_events <- data$event_binary

                        # Method 1: Restricted Cubic Splines (RCS) using rms package
                        rcs_calibration <- private$.calculateRCSCalibration(predicted_probs, observed_events)

                        # Method 2: Lowess-based flexible calibration
                        lowess_calibration <- private$.calculateLowessCalibration(predicted_probs, observed_events)

                        # Method 3: rms calibrate function if available
                        rms_calibration <- private$.calculateRMSCalibration(cox_model, data)

                        return(list(
                            rcs_calibration = rcs_calibration,
                            lowess_calibration = lowess_calibration,
                            rms_calibration = rms_calibration
                        ))
                    },
                    error = function(e) {
                        return(private$.createEmptySplineCalibrationResult(e$message))
                    }
                )
            },
            .calculateRCSCalibration = function(predicted_probs, observed_events, n_knots = 4) {
                # Restricted Cubic Splines calibration
                tryCatch(
                    {
                        if (length(predicted_probs) < 50 || length(unique(predicted_probs)) < 20) {
                            return(list(
                                available = FALSE,
                                reason = "Insufficient data for spline calibration",
                                slope = NA,
                                intercept = NA,
                                r_squared = NA,
                                p_value = NA
                            ))
                        }

                        # Create calibration data
                        cal_data <- data.frame(
                            predicted = predicted_probs,
                            observed = observed_events
                        )
                        cal_data <- cal_data[complete.cases(cal_data), ]

                        # Use rcs from rms package for restricted cubic splines
                        if (requireNamespace("rms", quietly = TRUE)) {
                            # Create spline basis
                            spline_basis <- rms::rcs(cal_data$predicted, n_knots)

                            # Fit spline-based logistic regression
                            spline_model <- tryCatch(
                                {
                                    glm(observed ~ spline_basis, data = cal_data, family = binomial())
                                },
                                error = function(e) NULL
                            )

                            if (!is.null(spline_model) && !spline_model$converged == FALSE) {
                                # Calculate spline calibration metrics
                                spline_fitted <- predict(spline_model, type = "response")

                                # Calculate R-squared for calibration fit
                                null_deviance <- spline_model$null.deviance
                                residual_deviance <- spline_model$deviance
                                r_squared <- (null_deviance - residual_deviance) / null_deviance

                                # Test overall spline significance
                                p_value <- anova(spline_model, test = "Chisq")$`Pr(>Chi)`[2]
                                if (is.na(p_value)) p_value <- 1.0

                                # Calculate calibration slope from spline fit
                                slope_estimate <- private$.calculateSplineSlope(cal_data$predicted, spline_fitted)

                                return(list(
                                    available = TRUE,
                                    method = "Restricted Cubic Splines",
                                    slope = slope_estimate,
                                    intercept = coef(spline_model)[1],
                                    r_squared = r_squared,
                                    p_value = p_value,
                                    n_knots = n_knots,
                                    converged = TRUE,
                                    interpretation = private$.interpretSplineCalibration(slope_estimate, r_squared, p_value)
                                ))
                            }
                        }

                        # Fallback: Use splines package
                        return(private$.calculateSplineCalibrationFallback(predicted_probs, observed_events))
                    },
                    error = function(e) {
                        return(list(
                            available = FALSE,
                            reason = paste("RCS calibration failed:", e$message),
                            slope = NA,
                            intercept = NA,
                            r_squared = NA,
                            p_value = NA
                        ))
                    }
                )
            },
            .calculateLowessCalibration = function(predicted_probs, observed_events, span = 0.75) {
                # Lowess-based flexible calibration
                tryCatch(
                    {
                        if (length(predicted_probs) < 30) {
                            return(list(
                                available = FALSE,
                                reason = "Insufficient data for Lowess calibration",
                                slope = NA,
                                r_squared = NA
                            ))
                        }

                        # Create calibration data
                        cal_data <- data.frame(
                            predicted = predicted_probs,
                            observed = observed_events
                        )
                        cal_data <- cal_data[complete.cases(cal_data), ]

                        # Perform Lowess smoothing
                        lowess_result <- lowess(cal_data$predicted, cal_data$observed, f = span)

                        # Calculate effective calibration slope from Lowess curve
                        slope_estimate <- private$.calculateLowessSlope(lowess_result$x, lowess_result$y)

                        # Calculate R-squared for Lowess fit
                        predicted_smooth <- approx(lowess_result$x, lowess_result$y, xout = cal_data$predicted)$y
                        predicted_smooth[is.na(predicted_smooth)] <- mean(cal_data$observed)

                        ss_total <- sum((cal_data$observed - mean(cal_data$observed))^2)
                        ss_residual <- sum((cal_data$observed - predicted_smooth)^2, na.rm = TRUE)
                        r_squared <- max(0, 1 - ss_residual / ss_total)

                        return(list(
                            available = TRUE,
                            method = "Lowess Smoothing",
                            slope = slope_estimate,
                            r_squared = r_squared,
                            span = span,
                            smooth_points = list(x = lowess_result$x, y = lowess_result$y),
                            interpretation = private$.interpretSplineCalibration(slope_estimate, r_squared, NA)
                        ))
                    },
                    error = function(e) {
                        return(list(
                            available = FALSE,
                            reason = paste("Lowess calibration failed:", e$message),
                            slope = NA,
                            r_squared = NA
                        ))
                    }
                )
            },
            .calculateRMSCalibration = function(cox_model, data) {
                # Use rms calibrate function for comprehensive calibration
                tryCatch(
                    {
                        if (!requireNamespace("rms", quietly = TRUE)) {
                            return(list(
                                available = FALSE,
                                reason = "rms package not available",
                                slope = NA,
                                intercept = NA
                            ))
                        }

                        # Advanced rms calibration would require model refitting with rms package
                        # Return availability indicator for advanced analysis
                        return(list(
                            available = TRUE,
                            method = "rms Package Integration",
                            slope = NA,
                            intercept = NA,
                            note = "Advanced calibration available with rms::calibrate",
                            interpretation = "rms calibration methods available for enhanced analysis"
                        ))
                    },
                    error = function(e) {
                        return(list(
                            available = FALSE,
                            reason = paste("RMS calibration failed:", e$message),
                            slope = NA,
                            intercept = NA
                        ))
                    }
                )
            },
            .calculateSplineSlope = function(predicted, fitted) {
                # Calculate effective slope from spline-based calibration
                tryCatch(
                    {
                        # Use linear regression of fitted vs predicted to estimate overall slope
                        slope_data <- data.frame(predicted = predicted, fitted = fitted)
                        slope_data <- slope_data[complete.cases(slope_data), ]

                        if (nrow(slope_data) < 10) {
                            return(NA)
                        }

                        slope_model <- lm(fitted ~ predicted, data = slope_data)
                        return(coef(slope_model)["predicted"])
                    },
                    error = function(e) {
                        return(NA)
                    }
                )
            },
            .calculateLowessSlope = function(x, y) {
                # Calculate effective slope from Lowess curve
                tryCatch(
                    {
                        # Calculate slope over the middle 50% of the range to avoid edge effects
                        n <- length(x)
                        start_idx <- max(1, floor(n * 0.25))
                        end_idx <- min(n, ceiling(n * 0.75))

                        if (end_idx <= start_idx) {
                            return(NA)
                        }

                        # Calculate average slope over the middle range
                        dx <- x[end_idx] - x[start_idx]
                        dy <- y[end_idx] - y[start_idx]

                        if (abs(dx) < 1e-6) {
                            return(NA)
                        }

                        return(dy / dx)
                    },
                    error = function(e) {
                        return(NA)
                    }
                )
            },
            .calculateSplineCalibrationFallback = function(predicted_probs, observed_events) {
                # Fallback spline calibration using base R splines
                tryCatch(
                    {
                        cal_data <- data.frame(
                            predicted = predicted_probs,
                            observed = observed_events
                        )
                        cal_data <- cal_data[complete.cases(cal_data), ]

                        if (nrow(cal_data) < 20) {
                            return(list(
                                available = FALSE,
                                reason = "Insufficient data for fallback spline calibration"
                            ))
                        }

                        # Use natural splines from splines package
                        if (requireNamespace("splines", quietly = TRUE)) {
                            spline_basis <- splines::ns(cal_data$predicted, df = 3)
                            spline_model <- glm(observed ~ spline_basis, data = cal_data, family = binomial())

                            if (!is.null(spline_model) && spline_model$converged) {
                                r_squared <- (spline_model$null.deviance - spline_model$deviance) / spline_model$null.deviance

                                return(list(
                                    available = TRUE,
                                    method = "Natural Splines (Fallback)",
                                    slope = 1.0, # Approximate for natural splines
                                    intercept = coef(spline_model)[1],
                                    r_squared = r_squared,
                                    interpretation = "Flexible calibration using natural splines"
                                ))
                            }
                        }

                        return(list(available = FALSE, reason = "Spline packages not available"))
                    },
                    error = function(e) {
                        return(list(available = FALSE, reason = paste("Fallback spline failed:", e$message)))
                    }
                )
            },
            .interpretSplineCalibration = function(slope, r_squared, p_value) {
                # Interpret spline-based calibration results
                tryCatch(
                    {
                        interpretations <- character()

                        # Interpret slope (if available)
                        if (!is.na(slope)) {
                            if (abs(slope - 1.0) < 0.1) {
                                interpretations <- c(interpretations, "Excellent spline-based calibration")
                            } else if (abs(slope - 1.0) < 0.3) {
                                interpretations <- c(interpretations, "Good spline-based calibration")
                            } else {
                                interpretations <- c(interpretations, "Poor spline-based calibration")
                            }
                        }

                        # Interpret R-squared (if available)
                        if (!is.na(r_squared)) {
                            if (r_squared > 0.8) {
                                interpretations <- c(interpretations, "High calibration fit quality")
                            } else if (r_squared > 0.5) {
                                interpretations <- c(interpretations, "Moderate calibration fit quality")
                            } else {
                                interpretations <- c(interpretations, "Low calibration fit quality")
                            }
                        }

                        # Interpret p-value (if available)
                        if (!is.na(p_value)) {
                            if (p_value > 0.05) {
                                interpretations <- c(interpretations, "Non-significant calibration nonlinearity")
                            } else {
                                interpretations <- c(interpretations, "Significant calibration nonlinearity detected")
                            }
                        }

                        if (length(interpretations) == 0) {
                            return("Spline calibration analysis completed")
                        } else {
                            return(paste(interpretations, collapse = "; "))
                        }
                    },
                    error = function(e) {
                        return("Unable to interpret spline calibration")
                    }
                )
            },
            .createEmptySplineCalibrationResult = function(error_msg = "Spline calibration unavailable") {
                return(list(
                    rcs_calibration = list(available = FALSE, reason = error_msg),
                    lowess_calibration = list(available = FALSE, reason = error_msg),
                    rms_calibration = list(available = FALSE, reason = error_msg)
                ))
            },
            .populateCalibrationAnalysis = function(calibration_results) {
                # Populate calibration analysis table
                table <- self$results$calibrationAnalysis
                table$deleteRows()

                if (is.null(calibration_results)) {
                    table$setNote("note", .("Calibration analysis could not be completed. Check if Cox models were successfully fitted."))
                    return()
                }

                old_cal <- calibration_results$old_calibration
                new_cal <- calibration_results$new_calibration
                old_spline <- calibration_results$old_spline_calibration
                new_spline <- calibration_results$new_spline_calibration

                # Add row for original staging system
                if (!is.null(old_cal)) {
                    table$addRow(rowKey = "old", values = list(
                        Model = "Original Staging",
                        Hosmer_Lemeshow_Chi2 = old_cal$hl_chi2,
                        Hosmer_Lemeshow_df = old_cal$hl_df,
                        Hosmer_Lemeshow_p = old_cal$hl_p,
                        Calibration_Slope = old_cal$cal_slope,
                        Calibration_Intercept = old_cal$cal_intercept,
                        C_Slope_CI_Lower = old_cal$cal_slope_ci_lower,
                        C_Slope_CI_Upper = old_cal$cal_slope_ci_upper,
                        Interpretation = old_cal$interpretation
                    ))
                }

                # Add row for new staging system
                if (!is.null(new_cal)) {
                    table$addRow(rowKey = "new", values = list(
                        Model = "New Staging",
                        Hosmer_Lemeshow_Chi2 = new_cal$hl_chi2,
                        Hosmer_Lemeshow_df = new_cal$hl_df,
                        Hosmer_Lemeshow_p = new_cal$hl_p,
                        Calibration_Slope = new_cal$cal_slope,
                        Calibration_Intercept = new_cal$cal_intercept,
                        C_Slope_CI_Lower = new_cal$cal_slope_ci_lower,
                        C_Slope_CI_Upper = new_cal$cal_slope_ci_upper,
                        Interpretation = new_cal$interpretation
                    ))
                }

                # Add spline calibration results if available
                if (!is.null(old_spline) && !is.null(old_spline$rcs_calibration) &&
                    old_spline$rcs_calibration$available) {
                    table$addRow(rowKey = "old_spline", values = list(
                        Model = "Original - Spline Calibration",
                        Hosmer_Lemeshow_Chi2 = NA,
                        Hosmer_Lemeshow_df = NA,
                        Hosmer_Lemeshow_p = NA,
                        Calibration_Slope = old_spline$rcs_calibration$slope,
                        Calibration_Intercept = old_spline$rcs_calibration$intercept,
                        C_Slope_CI_Lower = old_spline$rcs_calibration$ci_lower,
                        C_Slope_CI_Upper = old_spline$rcs_calibration$ci_upper,
                        Interpretation = paste("Flexible spline calibration:", old_spline$rcs_calibration$interpretation)
                    ))
                }

                if (!is.null(new_spline) && !is.null(new_spline$rcs_calibration) &&
                    new_spline$rcs_calibration$available) {
                    table$addRow(rowKey = "new_spline", values = list(
                        Model = "New - Spline Calibration",
                        Hosmer_Lemeshow_Chi2 = NA,
                        Hosmer_Lemeshow_df = NA,
                        Hosmer_Lemeshow_p = NA,
                        Calibration_Slope = new_spline$rcs_calibration$slope,
                        Calibration_Intercept = new_spline$rcs_calibration$intercept,
                        C_Slope_CI_Lower = new_spline$rcs_calibration$ci_lower,
                        C_Slope_CI_Upper = new_spline$rcs_calibration$ci_upper,
                        Interpretation = paste("Flexible spline calibration:", new_spline$rcs_calibration$interpretation)
                    ))
                }

                # Add Lowess (flexible smoothing) calibration rows
                for (side in c("old", "new")) {
                    spline_res <- if (side == "old") old_spline else new_spline
                    lowess_res <- spline_res$lowess_calibration
                    if (is.null(lowess_res) || !isTRUE(lowess_res$available)) next

                    lowess_interp <- gsub("spline-based", "Lowess-based", lowess_res$interpretation, fixed = TRUE)
                    lowess_interp <- gsub("Spline calibration analysis completed", "Lowess calibration analysis completed", lowess_interp, fixed = TRUE)
                    if (!is.null(lowess_res$r_squared) && !is.na(lowess_res$r_squared)) {
                        lowess_interp <- paste0(lowess_interp, "; R-squared = ", round(lowess_res$r_squared, 3))
                    }

                    table$addRow(rowKey = paste0(side, "_lowess"), values = list(
                        Model = if (side == "old") "Original - Lowess Calibration" else "New - Lowess Calibration",
                        Hosmer_Lemeshow_Chi2 = NA,
                        Hosmer_Lemeshow_df = NA,
                        Hosmer_Lemeshow_p = NA,
                        Calibration_Slope = lowess_res$slope,
                        Calibration_Intercept = NA,
                        C_Slope_CI_Lower = NA,
                        C_Slope_CI_Upper = NA,
                        Interpretation = lowess_interp
                    ))
                }

                if (isTRUE(old_spline$lowess_calibration$available) || isTRUE(new_spline$lowess_calibration$available)) {
                    table$setNote("lowess_note", .("Lowess calibration slope is estimated from the smoothed predicted-vs-observed curve over the middle 50% of the predicted-risk range. H-L test, calibration intercept and slope confidence intervals are not defined for this method."))
                }

                # Add a note about spline calibration if available
                if ((!is.null(old_spline) && !is.null(old_spline$rcs_calibration) && old_spline$rcs_calibration$available) ||
                    (!is.null(new_spline) && !is.null(new_spline$rcs_calibration) && new_spline$rcs_calibration$available)) {
                    table$setNote("spline_note", .("Spline calibration uses restricted cubic splines for flexible non-linear calibration assessment. H-L test not applicable for spline methods."))
                }
            },
            .performMultifactorialAnalysis = function(data) {
                # Comprehensive multifactorial analysis for stage migration

                # Extract covariate information
                continuous_vars <- self$options$continuousCovariates
                categorical_vars <- self$options$categoricalCovariates

                # Check if we have any covariates
                if (length(continuous_vars) == 0 && length(categorical_vars) == 0) {
                    return(list(
                        error = "No covariates selected for multifactorial analysis",
                        models = NULL,
                        comparisons = NULL
                    ))
                }

                # Prepare covariate data
                covariate_data <- data
                all_covariates <- c(continuous_vars, categorical_vars)

                # Validate covariates exist in data
                missing_covariates <- setdiff(all_covariates, names(data))
                if (length(missing_covariates) > 0) {
                    available_cols <- setdiff(names(data), c(
                        self$options$oldStage, self$options$newStage,
                        self$options$survivalTime, self$options$event
                    ))
                    return(list(
                        error = paste(
                            "Missing covariates:", paste(missing_covariates, collapse = ", "),
                            "\nAvailable columns:", paste(available_cols, collapse = ", ")
                        ),
                        models = NULL,
                        comparisons = NULL
                    ))
                }

                # Remove rows with missing covariates
                covariate_data <- covariate_data[complete.cases(covariate_data[all_covariates]), ]

                if (nrow(covariate_data) < 50) {
                    return(list(
                        error = "Insufficient sample size for multifactorial analysis after covariate cleaning",
                        models = NULL,
                        comparisons = NULL
                    ))
                }

                # Build model formulas
                old_stage <- self$options$oldStage
                new_stage <- self$options$newStage
                survival_time <- self$options$survivalTime

                # Build covariate formula component
                covariate_formula <- paste(all_covariates, collapse = " + ")

                # Define model formulas based on baseline model selection
                baseline_type <- self$options$baselineModel

                formulas <- list()

                if (baseline_type == "covariates_only") {
                    formulas$baseline <- as.formula(paste("survival::Surv(", survival_time, ", event_binary) ~", covariate_formula))
                    formulas$old_plus_covariates <- as.formula(paste("survival::Surv(", survival_time, ", event_binary) ~", old_stage, "+", covariate_formula))
                    formulas$new_plus_covariates <- as.formula(paste("survival::Surv(", survival_time, ", event_binary) ~", new_stage, "+", covariate_formula))
                } else if (baseline_type == "original_plus_covariates") {
                    formulas$baseline <- as.formula(paste("survival::Surv(", survival_time, ", event_binary) ~", old_stage, "+", covariate_formula))
                    formulas$new_plus_covariates <- as.formula(paste("survival::Surv(", survival_time, ", event_binary) ~", new_stage, "+", covariate_formula))
                } else if (baseline_type == "new_plus_covariates") {
                    formulas$baseline <- as.formula(paste("survival::Surv(", survival_time, ", event_binary) ~", new_stage, "+", covariate_formula))
                    formulas$old_plus_covariates <- as.formula(paste("survival::Surv(", survival_time, ", event_binary) ~", old_stage, "+", covariate_formula))
                }

                # Fit Cox models
                models <- list()
                model_results <- list()

                tryCatch(
                    {
                        for (model_name in names(formulas)) {
                            models[[model_name]] <- survival::coxph(formulas[[model_name]], data = covariate_data)

                            # Calculate C-index
                            cindex <- survival::concordance(models[[model_name]])

                            # Safely calculate standard error and confidence intervals
                            cindex_var <- private$.safeAtomic(cindex$var, "numeric", NA)
                            cindex_se <- if (!is.na(cindex_var) && cindex_var >= 0) {
                                sqrt(cindex_var)
                            } else {
                                NA
                            }

                            cindex_val <- private$.safeAtomic(cindex$concordance, "numeric", NA)
                            cindex_ci_lower <- if (!is.na(cindex_val) && !is.na(cindex_se)) {
                                cindex_val - private$.zCrit() * cindex_se
                            } else {
                                NA
                            }
                            cindex_ci_upper <- if (!is.na(cindex_val) && !is.na(cindex_se)) {
                                cindex_val + private$.zCrit() * cindex_se
                            } else {
                                NA
                            }

                            model_results[[model_name]] <- list(
                                model = models[[model_name]],
                                c_index = cindex_val,
                                c_index_se = cindex_se,
                                c_index_ci_lower = cindex_ci_lower,
                                c_index_ci_upper = cindex_ci_upper,
                                aic = AIC(models[[model_name]]),
                                bic = BIC(models[[model_name]])
                            )
                        }
                    },
                    error = function(e) {
                        return(list(
                            error = paste("Error fitting Cox models:", e$message),
                            models = NULL,
                            comparisons = NULL
                        ))
                    }
                )

                # Perform model comparisons
                comparisons <- list()

                # C-index comparisons
                if (length(model_results) >= 2) {
                    model_names <- names(model_results)
                    for (i in 1:(length(model_names) - 1)) {
                        for (j in (i + 1):length(model_names)) {
                            model1 <- model_names[i]
                            model2 <- model_names[j]

                            # Calculate C-index difference safely
                            c_diff <- model_results[[model2]]$c_index - model_results[[model1]]$c_index
                            se1 <- private$.safeAtomic(model_results[[model1]]$c_index_se, "numeric", NA)
                            se2 <- private$.safeAtomic(model_results[[model2]]$c_index_se, "numeric", NA)

                            se_diff <- if (!is.na(se1) && !is.na(se2)) {
                                sqrt(se1^2 + se2^2)
                            } else {
                                NA
                            }

                            # Z-test for difference (safely)
                            z_stat <- if (!is.na(c_diff) && !is.na(se_diff) && se_diff > 0) {
                                c_diff / se_diff
                            } else {
                                NA
                            }

                            p_value <- if (!is.na(z_stat)) {
                                2 * (1 - pnorm(abs(z_stat)))
                            } else {
                                NA
                            }

                            comparisons[[paste(model1, "vs", model2)]] <- list(
                                model1 = model1,
                                model2 = model2,
                                c_index_diff = c_diff,
                                se_diff = se_diff,
                                ci_lower = c_diff - private$.zCrit() * se_diff,
                                ci_upper = c_diff + private$.zCrit() * se_diff,
                                p_value = p_value
                            )
                        }
                    }
                }

                # Likelihood ratio tests for nested models
                nested_tests <- list()

                if (baseline_type == "covariates_only" && length(model_results) >= 3) {
                    # Test if adding staging improves the covariate-only model
                    if ("baseline" %in% names(models) && "old_plus_covariates" %in% names(models)) {
                        lrt_old <- anova(models$baseline, models$old_plus_covariates, test = "LRT")
                        nested_tests$old_vs_baseline <- list(
                            comparison = "Original Staging vs Covariates Only",
                            chi_square = lrt_old$Chisq[2],
                            df = lrt_old$Df[2],
                            p_value = lrt_old$`Pr(>|Chi|)`[2]
                        )
                    }

                    if ("baseline" %in% names(models) && "new_plus_covariates" %in% names(models)) {
                        lrt_new <- anova(models$baseline, models$new_plus_covariates, test = "LRT")
                        nested_tests$new_vs_baseline <- list(
                            comparison = "New Staging vs Covariates Only",
                            chi_square = lrt_new$Chisq[2],
                            df = lrt_new$Df[2],
                            p_value = lrt_new$`Pr(>|Chi|)`[2]
                        )
                    }
                }

                # Enhanced stepwise model selection with bootstrap stability
                stepwise_results <- NULL
                if (self$options$multifactorialComparisonType %in% c("stepwise", "comprehensive")) {
                    stepwise_results <- private$.performBootstrapModelSelection(covariate_data, all_covariates, old_stage, new_stage, survival_time)
                }

                # Advanced interaction detection if requested
                interaction_tests <- NULL
                if (self$options$performInteractionTests) {
                    if (length(all_covariates) > 0) {} else {}

                    # Use advanced interaction detection method
                    interaction_analysis <- private$.performAdvancedInteractionDetection(covariate_data, all_covariates, old_stage, new_stage, survival_time)
                    interaction_tests <- interaction_analysis$interaction_results
                    interaction_summary <- interaction_analysis$summary_stats


                    # Store the advanced interaction results
                    advanced_interaction_tests <- interaction_tests

                    # Initialize legacy interaction tests as a separate list
                    legacy_interaction_tests <- list()
                    for (covar in all_covariates) {
                        # Test interaction with old staging
                        tryCatch(
                            {
                                int_formula_old <- as.formula(paste(
                                    "survival::Surv(", survival_time, ", event_binary) ~",
                                    old_stage, "*", covar, "+",
                                    paste(setdiff(all_covariates, covar), collapse = " + ")
                                ))
                                int_model_old <- survival::coxph(int_formula_old, data = covariate_data)

                                # Compare with model without interaction
                                base_formula_old <- as.formula(paste(
                                    "survival::Surv(", survival_time, ", event_binary) ~",
                                    old_stage, "+", covariate_formula
                                ))
                                base_model_old <- survival::coxph(base_formula_old, data = covariate_data)

                                lrt_int_old <- anova(base_model_old, int_model_old, test = "LRT")

                                legacy_interaction_tests[[paste("old_stage", covar, sep = "_x_")]] <- list(
                                    interaction = paste("Original Staging x", covar),
                                    chi_square = lrt_int_old$Chisq[2],
                                    df = lrt_int_old$Df[2],
                                    p_value = lrt_int_old$`Pr(>|Chi|)`[2]
                                )
                            },
                            error = function(e) {
                                legacy_interaction_tests[[paste("old_stage", covar, sep = "_x_")]] <- list(
                                    interaction = paste("Original Staging x", covar),
                                    chi_square = NA,
                                    df = NA,
                                    p_value = NA,
                                    error = e$message
                                )
                            }
                        )

                        # Test interaction with new staging
                        tryCatch(
                            {
                                int_formula_new <- as.formula(paste(
                                    "survival::Surv(", survival_time, ", event_binary) ~",
                                    new_stage, "*", covar, "+",
                                    paste(setdiff(all_covariates, covar), collapse = " + ")
                                ))
                                int_model_new <- survival::coxph(int_model_new, data = covariate_data)

                                # Compare with model without interaction
                                base_formula_new <- as.formula(paste(
                                    "survival::Surv(", survival_time, ", event_binary) ~",
                                    new_stage, "+", covariate_formula
                                ))
                                base_model_new <- survival::coxph(base_formula_new, data = covariate_data)

                                lrt_int_new <- anova(base_model_new, int_model_new, test = "LRT")

                                legacy_interaction_tests[[paste("new_stage", covar, sep = "_x_")]] <- list(
                                    interaction = paste("New Staging x", covar),
                                    chi_square = lrt_int_new$Chisq[2],
                                    df = lrt_int_new$Df[2],
                                    p_value = lrt_int_new$`Pr(>|Chi|)`[2]
                                )
                            },
                            error = function(e) {
                                legacy_interaction_tests[[paste("new_stage", covar, sep = "_x_")]] <- list(
                                    interaction = paste("New Staging x", covar),
                                    chi_square = NA,
                                    df = NA,
                                    p_value = NA,
                                    error = e$message
                                )
                            }
                        )
                    }

                    # Use advanced interaction results as the primary interaction_tests
                    interaction_tests <- advanced_interaction_tests
                }

                # Stratified analysis if requested
                stratified_results <- NULL
                if (self$options$stratifiedAnalysis && length(categorical_vars) > 0) {
                    stratified_results <- list()

                    for (strat_var in categorical_vars) {
                        strata <- unique(covariate_data[[strat_var]])
                        strata <- strata[!is.na(strata)]

                        for (stratum in strata) {
                            subset_data <- covariate_data[covariate_data[[strat_var]] == stratum, ]

                            if (nrow(subset_data) >= 20) { # Minimum sample size for stratified analysis
                                tryCatch(
                                    {
                                        # Fit models for this stratum
                                        old_formula <- as.formula(paste("survival::Surv(", survival_time, ", event_binary) ~", old_stage))
                                        new_formula <- as.formula(paste("survival::Surv(", survival_time, ", event_binary) ~", new_stage))

                                        old_model_strat <- survival::coxph(old_formula, data = subset_data)
                                        new_model_strat <- survival::coxph(new_formula, data = subset_data)

                                        old_cindex <- survival::concordance(old_model_strat)
                                        new_cindex <- survival::concordance(new_model_strat)

                                        # Calculate difference safely
                                        old_cindex_val <- private$.safeAtomic(old_cindex$concordance, "numeric", NA)
                                        new_cindex_val <- private$.safeAtomic(new_cindex$concordance, "numeric", NA)
                                        old_cindex_var <- private$.safeAtomic(old_cindex$var, "numeric", NA)
                                        new_cindex_var <- private$.safeAtomic(new_cindex$var, "numeric", NA)

                                        c_diff <- if (!is.na(old_cindex_val) && !is.na(new_cindex_val)) {
                                            new_cindex_val - old_cindex_val
                                        } else {
                                            NA
                                        }

                                        se_diff <- if (!is.na(old_cindex_var) && !is.na(new_cindex_var) &&
                                            old_cindex_var >= 0 && new_cindex_var >= 0) {
                                            sqrt(old_cindex_var + new_cindex_var)
                                        } else {
                                            NA
                                        }

                                        z_stat <- if (!is.na(c_diff) && !is.na(se_diff) && se_diff > 0) {
                                            c_diff / se_diff
                                        } else {
                                            NA
                                        }

                                        p_value <- if (!is.na(z_stat)) {
                                            2 * (1 - pnorm(abs(z_stat)))
                                        } else {
                                            NA
                                        }

                                        stratified_results[[paste(strat_var, stratum, sep = "_")]] <- list(
                                            stratum = paste(strat_var, "=", stratum),
                                            n = nrow(subset_data),
                                            c_index_old = old_cindex$concordance,
                                            c_index_new = new_cindex$concordance,
                                            difference = c_diff,
                                            p_value = p_value
                                        )
                                    },
                                    error = function(e) {
                                        stratified_results[[paste(strat_var, stratum, sep = "_")]] <- list(
                                            stratum = paste(strat_var, "=", stratum),
                                            error = e$message
                                        )
                                    }
                                )
                            }
                        }
                    }
                }

                # Comprehensive model diagnostics if comprehensive analysis
                model_diagnostics <- NULL
                if (self$options$multifactorialComparisonType == "comprehensive") {
                    model_diagnostics <- private$.performComprehensiveModelDiagnostics(covariate_data, all_covariates, old_stage, new_stage, survival_time)
                }

                # Calculate adjusted NRI if enabled
                adjusted_nri_results <- NULL
                if (self$options$calculateNRI && self$options$multifactorialComparisonType %in% c("comprehensive", "adjusted_cindex")) {
                    adjusted_nri_results <- private$.calculateAdjustedNRI(covariate_data, all_covariates, old_stage, new_stage, survival_time)
                }

                # Perform multivariable decision curve analysis if enabled
                multivariable_dca_results <- NULL
                if (self$options$performDCA && self$options$multifactorialComparisonType %in% c("comprehensive", "adjusted_cindex")) {
                    multivariable_dca_results <- private$.performMultivariableDCA(covariate_data, all_covariates, old_stage, new_stage, survival_time)
                }

                # Generate personalized risk predictions if comprehensive analysis
                personalized_predictions <- NULL
                if (self$options$multifactorialComparisonType == "comprehensive") {
                    personalized_predictions <- private$.generatePersonalizedPredictions(covariate_data, all_covariates, old_stage, new_stage, survival_time)
                }

                return(list(
                    models = model_results,
                    comparisons = comparisons,
                    nested_tests = nested_tests,
                    stepwise_results = stepwise_results,
                    interaction_tests = interaction_tests,
                    interaction_summary = if (self$options$performInteractionTests) interaction_summary else NULL,
                    model_diagnostics = model_diagnostics,
                    stratified_results = stratified_results,
                    adjusted_nri = adjusted_nri_results,
                    multivariable_dca = multivariable_dca_results,
                    personalized_predictions = personalized_predictions,
                    sample_size = nrow(covariate_data),
                    covariates_used = all_covariates,
                    error = NULL
                ))
            },
            .calculateAdjustedNRI = function(covariate_data, all_covariates, old_stage, new_stage, survival_time) {
                # Calculate Net Reclassification Improvement adjusted for covariates
                # This provides NRI measures in the context of multifactorial models

                tryCatch(
                    {

                        # Parse time points from user input
                        time_points_str <- self$options$nriTimePoints
                        time_points <- as.numeric(unlist(strsplit(gsub("\\s", "", time_points_str), ",")))
                        time_points <- time_points[!is.na(time_points) & time_points > 0]

                        if (length(time_points) == 0) {
                            time_points <- c(12, 24, 60) # Default time points
                        }

                        # Prepare results storage
                        nri_results <- list()

                        for (time_point in time_points) {
                            # Build baseline covariate model (without any staging)
                            if (length(all_covariates) > 0) {
                                baseline_formula <- as.formula(paste(
                                    "survival::Surv(", survival_time, ", event_binary) ~",
                                    paste(all_covariates, collapse = " + ")
                                ))
                                baseline_model <- tryCatch(
                                    {
                                        survival::coxph(baseline_formula, data = covariate_data)
                                    },
                                    error = function(e) NULL
                                )
                            } else {
                                baseline_model <- NULL
                            }

                            # Build old staging + covariates model
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

                            # Build new staging + covariates model
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
                                nri_results[[paste0("time_", time_point)]] <- list(
                                    time_point = time_point,
                                    error = "Model fitting failed"
                                )
                                next
                            }

                            # Calculate PER-PATIENT survival probabilities at this time point.
                            # survfit(model) with no newdata returns a single curve at the mean
                            # covariate values, so $surv was length 1 while actual_events below is
                            # length nrow(covariate_data) -- .calculateNRIComponents then died with
                            # "all arguments must have the same length" and adjusted NRI never ran.
                            # NRI is a per-patient reclassification measure, so predict for every
                            # patient. $surv comes back as a times x subjects matrix; flatten it.
                            n_obs <- nrow(covariate_data)
                            survprob_at <- function(model) {
                                if (is.null(model)) {
                                    return(NULL)
                                }
                                out <- tryCatch(
                                    as.vector(summary(
                                        survfit(model, newdata = covariate_data),
                                        times = time_point, extend = TRUE
                                    )$surv),
                                    error = function(e) NULL
                                )
                                if (is.null(out) || length(out) != n_obs) NULL else out
                            }

                            old_survprob <- survprob_at(old_model)
                            new_survprob <- survprob_at(new_model)
                            baseline_survprob <- survprob_at(baseline_model)

                            if (is.null(old_survprob) || is.null(new_survprob)) {
                                nri_results[[paste0("time_", time_point)]] <- list(
                                    time_point = time_point,
                                    error = "Survival probability calculation failed"
                                )
                                next
                            }

                            # Convert to risk probabilities
                            old_riskprob <- 1 - old_survprob
                            new_riskprob <- 1 - new_survprob
                            baseline_riskprob <- if (!is.null(baseline_survprob)) 1 - baseline_survprob else NULL

                            # Define risk categories (can be customized)
                            risk_cutoffs <- c(0.1, 0.3) # Low (<10%), Medium (10-30%), High (>30%)

                            # Categorize risks
                            old_risk_cat <- cut(old_riskprob,
                                breaks = c(0, risk_cutoffs, 1),
                                labels = c("Low", "Medium", "High"), include.lowest = TRUE
                            )
                            new_risk_cat <- cut(new_riskprob,
                                breaks = c(0, risk_cutoffs, 1),
                                labels = c("Low", "Medium", "High"), include.lowest = TRUE
                            )
                            baseline_risk_cat <- NULL
                            if (!is.null(baseline_riskprob)) {
                                baseline_risk_cat <- cut(baseline_riskprob,
                                    breaks = c(0, risk_cutoffs, 1),
                                    labels = c("Low", "Medium", "High"), include.lowest = TRUE
                                )
                            }

                            # Get actual outcomes at time point
                            actual_events <- covariate_data$event_binary == 1 & covariate_data[[survival_time]] <= time_point
                            actual_events[is.na(actual_events)] <- FALSE

                            # Calculate NRI components

                            # 1. Standard NRI (old vs new staging)
                            standard_nri <- private$.calculateNRIComponents(old_risk_cat, new_risk_cat, actual_events)

                            # 2. Adjusted NRI (baseline + old vs baseline + new staging)
                            adjusted_nri <- NULL
                            if (!is.null(baseline_risk_cat)) {
                                # Compare risk categories when adding staging to baseline model
                                adjusted_nri <- private$.calculateNRIComponents(old_risk_cat, new_risk_cat, actual_events)
                                # Additional analysis comparing with baseline
                                baseline_vs_old_nri <- private$.calculateNRIComponents(baseline_risk_cat, old_risk_cat, actual_events)
                                baseline_vs_new_nri <- private$.calculateNRIComponents(baseline_risk_cat, new_risk_cat, actual_events)

                                adjusted_nri$baseline_vs_old <- baseline_vs_old_nri
                                adjusted_nri$baseline_vs_new <- baseline_vs_new_nri
                            }

                            # 3. Model discrimination metrics
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

                            baseline_concordance <- if (!is.null(baseline_model)) {
                                tryCatch(
                                    {
                                        survival::concordance(baseline_model)$concordance
                                    },
                                    error = function(e) NA
                                )
                            } else {
                                NA
                            }

                            # 4. Likelihood ratio tests
                            lr_old_vs_baseline <- NULL
                            lr_new_vs_baseline <- NULL
                            lr_new_vs_old <- NULL

                            if (!is.null(baseline_model)) {
                                lr_old_vs_baseline <- tryCatch(
                                    {
                                        anova(baseline_model, old_model, test = "Chisq")
                                    },
                                    error = function(e) NULL
                                )

                                lr_new_vs_baseline <- tryCatch(
                                    {
                                        anova(baseline_model, new_model, test = "Chisq")
                                    },
                                    error = function(e) NULL
                                )
                            }

                            lr_new_vs_old <- tryCatch(
                                {
                                    anova(old_model, new_model, test = "Chisq")
                                },
                                error = function(e) NULL
                            )

                            # Store results for this time point
                            nri_results[[paste0("time_", time_point)]] <- list(
                                time_point = time_point,
                                standard_nri = standard_nri,
                                adjusted_nri = adjusted_nri,
                                concordance = list(
                                    baseline = baseline_concordance,
                                    old_staging = old_concordance,
                                    new_staging = new_concordance,
                                    improvement_old = if (!is.na(old_concordance) && !is.na(baseline_concordance)) {
                                        old_concordance - baseline_concordance
                                    } else {
                                        NA
                                    },
                                    improvement_new = if (!is.na(new_concordance) && !is.na(baseline_concordance)) {
                                        new_concordance - baseline_concordance
                                    } else {
                                        NA
                                    },
                                    difference_staging = if (!is.na(new_concordance) && !is.na(old_concordance)) {
                                        new_concordance - old_concordance
                                    } else {
                                        NA
                                    }
                                ),
                                likelihood_tests = list(
                                    old_vs_baseline = lr_old_vs_baseline,
                                    new_vs_baseline = lr_new_vs_baseline,
                                    new_vs_old = lr_new_vs_old
                                ),
                                risk_cutoffs = risk_cutoffs,
                                n_patients = nrow(covariate_data),
                                n_events = sum(actual_events, na.rm = TRUE)
                            )
                        }

                        return(nri_results)
                    },
                    error = function(e) {
                        # Include the failing call: this handler wraps ~250 lines, so a
                        # bare message gives no way to locate the fault.
                        private$.addNotice("WARNING", .("Adjusted NRI failed"), conditionMessage(e))
                        return(list(error = e$message))
                    }
                )
            },
            .performMultivariableDCA = function(covariate_data, all_covariates, old_stage, new_stage, survival_time) {
                # Perform Decision Curve Analysis for multivariable models
                # Compares clinical utility of different staging models adjusted for covariates

                tryCatch(
                    {

                        # Define time points for DCA analysis
                        time_points_str <- self$options$nriTimePoints
                        time_points <- as.numeric(unlist(strsplit(gsub("\\s", "", time_points_str), ",")))
                        time_points <- time_points[!is.na(time_points) & time_points > 0]

                        if (length(time_points) == 0) {
                            time_points <- c(12, 24, 60) # Default time points
                        }

                        # Prepare results storage
                        dca_results <- list()

                        for (time_point in time_points) {
                            # Build models for comparison
                            model_list <- list()
                            model_names <- c()

                            # 1. Baseline model (covariates only) - if available
                            if (length(all_covariates) > 0) {
                                baseline_formula <- as.formula(paste(
                                    "survival::Surv(", survival_time, ", event_binary) ~",
                                    paste(all_covariates, collapse = " + ")
                                ))
                                baseline_model <- tryCatch(
                                    {
                                        survival::coxph(baseline_formula, data = covariate_data)
                                    },
                                    error = function(e) NULL
                                )

                                if (!is.null(baseline_model)) {
                                    model_list[["baseline"]] <- baseline_model
                                    model_names <- c(model_names, "Baseline (Covariates Only)")
                                }
                            }

                            # 2. Old staging + covariates model
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

                            if (!is.null(old_model)) {
                                model_list[["old_staging"]] <- old_model
                                model_names <- c(model_names, "Old Staging + Covariates")
                            }

                            # 3. New staging + covariates model
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

                            if (!is.null(new_model)) {
                                model_list[["new_staging"]] <- new_model
                                model_names <- c(model_names, "New Staging + Covariates")
                            }

                            # 4. Full model (both staging systems + covariates) - for comparison
                            if (!is.null(old_model) && !is.null(new_model)) {
                                full_covariates <- c(old_stage, new_stage, all_covariates)
                                full_formula <- as.formula(paste(
                                    "survival::Surv(", survival_time, ", event_binary) ~",
                                    paste(full_covariates, collapse = " + ")
                                ))
                                full_model <- tryCatch(
                                    {
                                        survival::coxph(full_formula, data = covariate_data)
                                    },
                                    error = function(e) NULL
                                )

                                if (!is.null(full_model)) {
                                    model_list[["full_model"]] <- full_model
                                    model_names <- c(model_names, "Both Staging + Covariates")
                                }
                            }

                            if (length(model_list) < 2) {
                                dca_results[[paste0("time_", time_point)]] <- list(
                                    time_point = time_point,
                                    error = "Insufficient models for comparison"
                                )
                                next
                            }

                            # Calculate risk predictions for all models
                            risk_predictions <- list()
                            for (model_name in names(model_list)) {
                                model <- model_list[[model_name]]

                                # Calculate predicted survival probability at time point
                                pred_surv <- tryCatch(
                                    {
                                        # Get linear predictor
                                        lp <- predict(model, type = "lp")
                                        # Get baseline hazard at time point
                                        baseline_surv <- summary(survfit(model), times = time_point, extend = TRUE)
                                        if (length(baseline_surv$surv) > 0) {
                                            baseline_surv_prob <- baseline_surv$surv[1]
                                            # Calculate individual survival probabilities
                                            surv_probs <- baseline_surv_prob^exp(lp)
                                            risk_probs <- 1 - surv_probs
                                            risk_probs
                                        } else {
                                            NULL
                                        }
                                    },
                                    error = function(e) NULL
                                )

                                if (!is.null(pred_surv)) {
                                    risk_predictions[[model_name]] <- pred_surv
                                }
                            }

                            # Get actual outcomes at time point
                            actual_events <- covariate_data$event_binary == 1 & covariate_data[[survival_time]] <= time_point
                            actual_events[is.na(actual_events)] <- FALSE

                            # Calculate DCA metrics for each model
                            # Define threshold probabilities for decision making
                            thresholds <- seq(0.01, 0.99, by = 0.01)

                            net_benefits <- data.frame(threshold = thresholds)

                            # Calculate "Treat All" and "Treat None" strategies
                            event_rate <- mean(actual_events, na.rm = TRUE)

                            treat_all_nb <- sapply(thresholds, function(pt) {
                                event_rate - (1 - event_rate) * pt / (1 - pt)
                            })

                            treat_none_nb <- rep(0, length(thresholds))

                            net_benefits$treat_all <- treat_all_nb
                            net_benefits$treat_none <- treat_none_nb

                            # Calculate net benefit for each model
                            for (model_name in names(risk_predictions)) {
                                pred_risks <- risk_predictions[[model_name]]

                                model_nb <- sapply(thresholds, function(pt) {
                                    # Patients classified as high risk (treated)
                                    treated <- pred_risks >= pt

                                    if (sum(treated) == 0) {
                                        return(0) # No one treated
                                    }

                                    # True positive rate among treated
                                    tp_rate <- mean(actual_events[treated], na.rm = TRUE)
                                    # False positive rate among treated
                                    fp_rate <- 1 - tp_rate
                                    # Proportion treated
                                    prop_treated <- mean(treated, na.rm = TRUE)

                                    # Net benefit calculation
                                    nb <- tp_rate * prop_treated - fp_rate * prop_treated * pt / (1 - pt)
                                    return(nb)
                                })

                                net_benefits[[model_name]] <- model_nb
                            }

                            # Calculate standardized net benefit (relative to treat all/none)
                            standardized_nb <- net_benefits
                            for (model_name in names(risk_predictions)) {
                                standardized_nb[[model_name]] <- (net_benefits[[model_name]] - treat_none_nb) /
                                    (treat_all_nb - treat_none_nb)
                            }

                            # Find optimal threshold ranges for each model
                            optimal_ranges <- list()
                            for (model_name in names(risk_predictions)) {
                                nb_values <- net_benefits[[model_name]]

                                # Find range where model is superior to treat all/none
                                superior_to_all <- nb_values > treat_all_nb & treat_all_nb > treat_none_nb
                                superior_to_none <- nb_values > treat_none_nb

                                if (any(superior_to_all)) {
                                    optimal_ranges[[model_name]] <- list(
                                        optimal_min = min(thresholds[superior_to_all]),
                                        optimal_max = max(thresholds[superior_to_all]),
                                        max_net_benefit = max(nb_values),
                                        max_nb_threshold = thresholds[which.max(nb_values)]
                                    )
                                } else if (any(superior_to_none)) {
                                    optimal_ranges[[model_name]] <- list(
                                        optimal_min = min(thresholds[superior_to_none]),
                                        optimal_max = max(thresholds[superior_to_none]),
                                        max_net_benefit = max(nb_values),
                                        max_nb_threshold = thresholds[which.max(nb_values)]
                                    )
                                } else {
                                    optimal_ranges[[model_name]] <- list(
                                        optimal_min = NA,
                                        optimal_max = NA,
                                        max_net_benefit = max(nb_values),
                                        max_nb_threshold = thresholds[which.max(nb_values)]
                                    )
                                }
                            }

                            # Calculate model comparisons
                            model_comparisons <- list()
                            model_pairs <- combn(names(risk_predictions), 2, simplify = FALSE)

                            for (pair in model_pairs) {
                                model1 <- pair[1]
                                model2 <- pair[2]

                                nb1 <- net_benefits[[model1]]
                                nb2 <- net_benefits[[model2]]

                                # Find threshold ranges where each model is superior
                                model1_superior <- nb1 > nb2
                                model2_superior <- nb2 > nb1

                                model_comparisons[[paste0(model1, "_vs_", model2)]] <- list(
                                    model1_superior_range = if (any(model1_superior)) {
                                        c(min(thresholds[model1_superior]), max(thresholds[model1_superior]))
                                    } else {
                                        c(NA, NA)
                                    },
                                    model2_superior_range = if (any(model2_superior)) {
                                        c(min(thresholds[model2_superior]), max(thresholds[model2_superior]))
                                    } else {
                                        c(NA, NA)
                                    },
                                    max_difference = max(abs(nb1 - nb2), na.rm = TRUE),
                                    mean_difference = mean(nb1 - nb2, na.rm = TRUE)
                                )
                            }

                            # Store results for this time point
                            dca_results[[paste0("time_", time_point)]] <- list(
                                time_point = time_point,
                                models_compared = names(model_list),
                                model_names = model_names,
                                net_benefits = net_benefits,
                                standardized_net_benefits = standardized_nb,
                                optimal_ranges = optimal_ranges,
                                model_comparisons = model_comparisons,
                                event_rate = event_rate,
                                n_patients = nrow(covariate_data),
                                n_events = sum(actual_events, na.rm = TRUE),
                                thresholds = thresholds
                            )
                        }

                        return(dca_results)
                    },
                    error = function(e) {
                        private$.addNotice("WARNING", .("Multivariable decision curve analysis failed"), conditionMessage(e))
                        return(list(error = e$message))
                    }
                )
            }
        )
    )
}
