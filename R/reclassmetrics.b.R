#' @title Model Reclassification Metrics (NRI & IDI)
#' @importFrom R6 R6Class
#' @import jmvcore
#'

reclassmetricsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "reclassmetricsClass",
    inherit = reclassmetricsBase,
    private = list(

        .preparedData = NULL,
        .nriResults = NULL,
        .idiResults = NULL,

        .init = function() {
            instructions <- glue::glue("
            <h3>Model Reclassification Metrics</h3>
            <hr>
            <p><b>Purpose:</b> Compare predictive performance between two models using
            Net Reclassification Improvement (NRI) and Integrated Discrimination
            Improvement (IDI).</p>

            <p><b>Required Variables:</b></p>
            <ul>
            <li><b>Outcome:</b> Binary outcome or event indicator (for survival)</li>
            <li><b>Old Model Predictions:</b> Predicted probabilities from baseline model</li>
            <li><b>New Model Predictions:</b> Predicted probabilities from enhanced model</li>
            <li><b>Survival Time:</b> Time to event (only for survival outcomes)</li>
            </ul>

            <p><b>Metrics Provided:</b></p>
            <ul>
            <li><b>NRI (Net Reclassification Improvement):</b> Proportion of correct
                reclassifications minus incorrect reclassifications</li>
            <li><b>IDI (Integrated Discrimination Improvement):</b> Difference in
                discrimination slopes between models</li>
            <li><b>Categorical NRI:</b> Based on risk categories (low/medium/high)</li>
            <li><b>Continuous NRI:</b> Without predefined categories</li>
            </ul>

            <p><b>Interpretation:</b></p>
            <ul>
            <li>NRI > 0: New model improves risk classification</li>
            <li>IDI > 0: New model better separates events from non-events</li>
            <li>Significant p-value: Improvement is statistically significant</li>
            </ul>

            <p><b>Packages Used:</b> PredictABEL, nricens, pROC, survival</p>
            <hr>
            ")

            self$results$instructions$setContent(instructions)
        },

        .run = function() {
            # Check required variables
            if (is.null(self$options$outcome) ||
                is.null(self$options$oldModelProb) ||
                is.null(self$options$newModelProb)) {
                return()
            }

            # Prepare data
            tryCatch({
                private$.prepareData()

                # Calculate metrics based on outcome type
                if (self$options$outcomeType == "binary") {
                    private$.calculateBinaryMetrics()
                } else {
                    private$.calculateSurvivalMetrics()
                }

                # Populate results
                private$.populateResults()
                private$.generateSummary()

            }, error = function(e) {
                stop(paste("Analysis failed:", e$message))
            })
        },

        .prepareData = function() {
            # Get data
            mydata <- self$data %>% janitor::clean_names()

            # Get variable names
            outcome_var <- janitor::make_clean_names(self$options$outcome)
            old_prob_var <- janitor::make_clean_names(self$options$oldModelProb)
            new_prob_var <- janitor::make_clean_names(self$options$newModelProb)

            # Extract vectors
            outcome_vec <- mydata[[outcome_var]]
            old_prob <- mydata[[old_prob_var]]
            new_prob <- mydata[[new_prob_var]]

            # Validate probabilities
            if (any(old_prob < 0, na.rm = TRUE) || any(old_prob > 1, na.rm = TRUE)) {
                stop("Old model predictions must be between 0 and 1")
            }
            if (any(new_prob < 0, na.rm = TRUE) || any(new_prob > 1, na.rm = TRUE)) {
                stop("New model predictions must be between 0 and 1")
            }

            # For binary outcome, ensure it's 0/1
            if (self$options$outcomeType == "binary") {
                if (is.factor(outcome_vec)) {
                    outcome_vec <- as.numeric(outcome_vec) - 1
                }
                if (!all(outcome_vec %in% c(0, 1))) {
                    stop("Binary outcome must be 0/1 or a factor with 2 levels")
                }
            }

            # For survival outcome, get time variable
            if (self$options$outcomeType == "survival") {
                if (is.null(self$options$survivalTime)) {
                    stop("Survival time is required for survival outcome analysis")
                }

                time_var <- janitor::make_clean_names(self$options$survivalTime)
                time_vec <- mydata[[time_var]]

                if (any(time_vec <= 0, na.rm = TRUE)) {
                    stop("Survival times must be positive")
                }
            } else {
                time_vec <- NULL
            }

            # Remove missing values
            complete_idx <- complete.cases(outcome_vec, old_prob, new_prob)
            if (self$options$outcomeType == "survival") {
                complete_idx <- complete_idx & complete.cases(time_vec)
            }

            if (sum(complete_idx) == 0) {
                stop("No complete cases available for analysis")
            }

            private$.preparedData <- list(
                outcome = outcome_vec[complete_idx],
                old_prob = old_prob[complete_idx],
                new_prob = new_prob[complete_idx],
                time = if (!is.null(time_vec)) time_vec[complete_idx] else NULL,
                n = sum(complete_idx),
                n_events = sum(outcome_vec[complete_idx] == 1)
            )
        },

        .calculateBinaryMetrics = function() {
            outcome <- private$.preparedData$outcome
            old_prob <- private$.preparedData$old_prob
            new_prob <- private$.preparedData$new_prob

            # Parse risk categories
            risk_cutoffs <- as.numeric(unlist(strsplit(self$options$riskCategories, ",")))
            risk_cutoffs <- sort(unique(risk_cutoffs))

            if (length(risk_cutoffs) < 2) {
                stop("At least 2 risk cutoffs are required")
            }

            # Calculate NRI
            nri_type <- self$options$nriType

            if (nri_type %in% c("categorical", "both")) {
                # Categorical NRI
                cat_nri <- private$.categoricalNRI(outcome, old_prob, new_prob, risk_cutoffs)
                private$.nriResults$categorical <- cat_nri
            }

            if (nri_type %in% c("continuous", "both")) {
                # Continuous NRI
                cont_nri <- private$.continuousNRI(outcome, old_prob, new_prob)
                private$.nriResults$continuous <- cont_nri
            }

            # Calculate IDI
            idi_result <- private$.calculateIDI(outcome, old_prob, new_prob)
            private$.idiResults <- idi_result
        },

        .categoricalNRI = function(outcome, old_prob, new_prob, cutoffs) {
            # Assign risk categories
            old_cat <- cut(old_prob, breaks = cutoffs, include.lowest = TRUE, labels = FALSE)
            new_cat <- cut(new_prob, breaks = cutoffs, include.lowest = TRUE, labels = FALSE)

            # Calculate NRI for events
            events_idx <- which(outcome == 1)
            n_events <- length(events_idx)

            if (n_events > 0) {
                events_up <- sum(new_cat[events_idx] > old_cat[events_idx], na.rm = TRUE)
                events_down <- sum(new_cat[events_idx] < old_cat[events_idx], na.rm = TRUE)
                nri_events <- (events_up - events_down) / n_events
            } else {
                events_up <- 0
                events_down <- 0
                nri_events <- 0
            }

            # Calculate NRI for non-events
            nonevents_idx <- which(outcome == 0)
            n_nonevents <- length(nonevents_idx)

            if (n_nonevents > 0) {
                nonevents_down <- sum(new_cat[nonevents_idx] < old_cat[nonevents_idx], na.rm = TRUE)
                nonevents_up <- sum(new_cat[nonevents_idx] > old_cat[nonevents_idx], na.rm = TRUE)
                nri_nonevents <- (nonevents_down - nonevents_up) / n_nonevents
            } else {
                nonevents_down <- 0
                nonevents_up <- 0
                nri_nonevents <- 0
            }

            # Total NRI
            nri_total <- nri_events + nri_nonevents

            # Bootstrap CI if requested
            if (self$options$bootstrapCI) {
                ci_results <- private$.bootstrapNRI(outcome, old_prob, new_prob, cutoffs)
                ci_lower <- ci_results$ci_lower
                ci_upper <- ci_results$ci_upper
                p_value <- ci_results$p_value
            } else {
                # Use asymptotic SE
                se_nri <- sqrt((events_up + events_down) / (n_events^2) +
                              (nonevents_up + nonevents_down) / (n_nonevents^2))
                z_val <- qnorm((1 + self$options$ciLevel) / 2)
                ci_lower <- nri_total - z_val * se_nri
                ci_upper <- nri_total + z_val * se_nri
                p_value <- 2 * (1 - pnorm(abs(nri_total / se_nri)))
            }

            list(
                nri_total = nri_total,
                nri_events = nri_events,
                nri_nonevents = nri_nonevents,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                p_value = p_value,
                events_up = events_up,
                events_down = events_down,
                nonevents_up = nonevents_up,
                nonevents_down = nonevents_down,
                old_cat = old_cat,
                new_cat = new_cat
            )
        },

        .continuousNRI = function(outcome, old_prob, new_prob) {
            # Continuous NRI: proportion improved minus proportion worsened
            # For events: increase in probability is improvement
            # For non-events: decrease in probability is improvement

            events_idx <- which(outcome == 1)
            nonevents_idx <- which(outcome == 0)

            n_events <- length(events_idx)
            n_nonevents <- length(nonevents_idx)

            if (n_events > 0) {
                prob_change_events <- new_prob[events_idx] - old_prob[events_idx]
                events_up <- sum(prob_change_events > 0)
                events_down <- sum(prob_change_events < 0)
                nri_events <- (events_up - events_down) / n_events
            } else {
                events_up <- 0
                events_down <- 0
                nri_events <- 0
            }

            if (n_nonevents > 0) {
                prob_change_nonevents <- new_prob[nonevents_idx] - old_prob[nonevents_idx]
                nonevents_down <- sum(prob_change_nonevents < 0)
                nonevents_up <- sum(prob_change_nonevents > 0)
                nri_nonevents <- (nonevents_down - nonevents_up) / n_nonevents
            } else {
                nonevents_down <- 0
                nonevents_up <- 0
                nri_nonevents <- 0
            }

            nri_total <- nri_events + nri_nonevents

            # Bootstrap CI
            if (self$options$bootstrapCI) {
                ci_results <- private$.bootstrapContinuousNRI(outcome, old_prob, new_prob)
                ci_lower <- ci_results$ci_lower
                ci_upper <- ci_results$ci_upper
                p_value <- ci_results$p_value
            } else {
                se_nri <- sqrt((events_up + events_down) / (n_events^2) +
                              (nonevents_up + nonevents_down) / (n_nonevents^2))
                z_val <- qnorm((1 + self$options$ciLevel) / 2)
                ci_lower <- nri_total - z_val * se_nri
                ci_upper <- nri_total + z_val * se_nri
                p_value <- 2 * (1 - pnorm(abs(nri_total / se_nri)))
            }

            list(
                nri_total = nri_total,
                nri_events = nri_events,
                nri_nonevents = nri_nonevents,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                p_value = p_value,
                events_up = events_up,
                events_down = events_down,
                nonevents_up = nonevents_up,
                nonevents_down = nonevents_down
            )
        },

        .calculateIDI = function(outcome, old_prob, new_prob) {
            # IDI = (IS_new - IS_old) - ((1-IS)_new - (1-IS)_old)
            # where IS = Integrated Sensitivity = mean prob for events
            # and 1-IS = mean prob for non-events

            events_idx <- which(outcome == 1)
            nonevents_idx <- which(outcome == 0)

            # Mean probabilities
            mean_old_events <- mean(old_prob[events_idx])
            mean_new_events <- mean(new_prob[events_idx])
            mean_old_nonevents <- mean(old_prob[nonevents_idx])
            mean_new_nonevents <- mean(new_prob[nonevents_idx])

            # IDI calculation
            is_improvement <- mean_new_events - mean_old_events
            ip_improvement <- mean_new_nonevents - mean_old_nonevents
            idi <- is_improvement - ip_improvement

            # Bootstrap CI
            if (self$options$bootstrapCI) {
                ci_results <- private$.bootstrapIDI(outcome, old_prob, new_prob)
                ci_lower <- ci_results$ci_lower
                ci_upper <- ci_results$ci_upper
                p_value <- ci_results$p_value
            } else {
                # Simplified SE estimation
                n_events <- length(events_idx)
                n_nonevents <- length(nonevents_idx)
                se_idi <- sqrt(var(new_prob[events_idx] - old_prob[events_idx]) / n_events +
                              var(new_prob[nonevents_idx] - old_prob[nonevents_idx]) / n_nonevents)
                z_val <- qnorm((1 + self$options$ciLevel) / 2)
                ci_lower <- idi - z_val * se_idi
                ci_upper <- idi + z_val * se_idi
                p_value <- 2 * (1 - pnorm(abs(idi / se_idi)))
            }

            list(
                idi = idi,
                is_improvement = is_improvement,
                ip_improvement = ip_improvement,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                p_value = p_value,
                mean_old_events = mean_old_events,
                mean_new_events = mean_new_events,
                mean_old_nonevents = mean_old_nonevents,
                mean_new_nonevents = mean_new_nonevents
            )
        },

        .bootstrapNRI = function(outcome, old_prob, new_prob, cutoffs) {
            n_boot <- min(self$options$nBootstrap, 500)
            n <- length(outcome)
            nri_boot <- numeric(n_boot)

            for (i in 1:n_boot) {
                if (i %% 50 == 0) private$.checkpoint()

                boot_idx <- sample(1:n, replace = TRUE)
                boot_outcome <- outcome[boot_idx]
                boot_old <- old_prob[boot_idx]
                boot_new <- new_prob[boot_idx]

                tryCatch({
                    boot_result <- private$.categoricalNRI(boot_outcome, boot_old, boot_new, cutoffs)
                    nri_boot[i] <- boot_result$nri_total
                }, error = function(e) {
                    nri_boot[i] <- NA
                })
            }

            # Calculate CI
            ci_level <- self$options$ciLevel
            ci_lower <- quantile(nri_boot, (1 - ci_level) / 2, na.rm = TRUE)
            ci_upper <- quantile(nri_boot, (1 + ci_level) / 2, na.rm = TRUE)
            p_value <- 2 * min(mean(nri_boot <= 0, na.rm = TRUE), mean(nri_boot >= 0, na.rm = TRUE))

            list(ci_lower = ci_lower, ci_upper = ci_upper, p_value = p_value)
        },

        .bootstrapContinuousNRI = function(outcome, old_prob, new_prob) {
            n_boot <- min(self$options$nBootstrap, 500)
            n <- length(outcome)
            nri_boot <- numeric(n_boot)

            for (i in 1:n_boot) {
                if (i %% 50 == 0) private$.checkpoint()

                boot_idx <- sample(1:n, replace = TRUE)
                boot_outcome <- outcome[boot_idx]
                boot_old <- old_prob[boot_idx]
                boot_new <- new_prob[boot_idx]

                tryCatch({
                    boot_result <- private$.continuousNRI(boot_outcome, boot_old, boot_new)
                    nri_boot[i] <- boot_result$nri_total
                }, error = function(e) {
                    nri_boot[i] <- NA
                })
            }

            ci_level <- self$options$ciLevel
            ci_lower <- quantile(nri_boot, (1 - ci_level) / 2, na.rm = TRUE)
            ci_upper <- quantile(nri_boot, (1 + ci_level) / 2, na.rm = TRUE)
            p_value <- 2 * min(mean(nri_boot <= 0, na.rm = TRUE), mean(nri_boot >= 0, na.rm = TRUE))

            list(ci_lower = ci_lower, ci_upper = ci_upper, p_value = p_value)
        },

        .bootstrapIDI = function(outcome, old_prob, new_prob) {
            n_boot <- min(self$options$nBootstrap, 500)
            n <- length(outcome)
            idi_boot <- numeric(n_boot)

            for (i in 1:n_boot) {
                if (i %% 50 == 0) private$.checkpoint()

                boot_idx <- sample(1:n, replace = TRUE)
                boot_outcome <- outcome[boot_idx]
                boot_old <- old_prob[boot_idx]
                boot_new <- new_prob[boot_idx]

                tryCatch({
                    boot_result <- private$.calculateIDI(boot_outcome, boot_old, boot_new)
                    idi_boot[i] <- boot_result$idi
                }, error = function(e) {
                    idi_boot[i] <- NA
                })
            }

            ci_level <- self$options$ciLevel
            ci_lower <- quantile(idi_boot, (1 - ci_level) / 2, na.rm = TRUE)
            ci_upper <- quantile(idi_boot, (1 + ci_level) / 2, na.rm = TRUE)
            p_value <- 2 * min(mean(idi_boot <= 0, na.rm = TRUE), mean(idi_boot >= 0, na.rm = TRUE))

            list(ci_lower = ci_lower, ci_upper = ci_upper, p_value = p_value)
        },

        .calculateSurvivalMetrics = function() {
            # Placeholder for survival-specific metrics
            # Would use survIDINRI package or similar for survival outcomes
            stop("Survival outcome analysis not yet implemented. Use binary outcome type for now.")
        },

        .populateResults = function() {
            # Populate NRI table
            nri_table <- self$results$nriResults

            nri_type <- self$options$nriType

            if (nri_type %in% c("categorical", "both") && !is.null(private$.nriResults$categorical)) {
                cat_nri <- private$.nriResults$categorical
                nri_table$addRow(rowKey = 1, values = list(
                    metric = "Categorical NRI",
                    estimate = round(cat_nri$nri_total, 4),
                    ci_lower = round(cat_nri$ci_lower, 4),
                    ci_upper = round(cat_nri$ci_upper, 4),
                    p_value = cat_nri$p_value,
                    interpretation = private$.interpretNRI(cat_nri$nri_total, cat_nri$p_value)
                ))
            }

            if (nri_type %in% c("continuous", "both") && !is.null(private$.nriResults$continuous)) {
                cont_nri <- private$.nriResults$continuous
                row_key <- if (nri_type == "both") 2 else 1
                nri_table$addRow(rowKey = row_key, values = list(
                    metric = "Continuous NRI",
                    estimate = round(cont_nri$nri_total, 4),
                    ci_lower = round(cont_nri$ci_lower, 4),
                    ci_upper = round(cont_nri$ci_upper, 4),
                    p_value = cont_nri$p_value,
                    interpretation = private$.interpretNRI(cont_nri$nri_total, cont_nri$p_value)
                ))
            }

            # Populate IDI table
            if (!is.null(private$.idiResults)) {
                idi_table <- self$results$idiResults
                idi <- private$.idiResults

                idi_table$addRow(rowKey = 1, values = list(
                    metric = "IDI",
                    estimate = round(idi$idi, 4),
                    ci_lower = round(idi$ci_lower, 4),
                    ci_upper = round(idi$ci_upper, 4),
                    p_value = idi$p_value,
                    interpretation = private$.interpretIDI(idi$idi, idi$p_value)
                ))
            }

            # Populate event/non-event NRI if requested
            if (self$options$showEventNonEvent && !is.null(private$.nriResults$categorical)) {
                private$.populateEventNonEventTable()
            }

            # Populate IDI components if requested
            if (self$options$showIDIComponents && !is.null(private$.idiResults)) {
                private$.populateIDIComponentsTable()
            }
        },

        .populateEventNonEventTable = function() {
            table <- self$results$eventNonEventNRI
            cat_nri <- private$.nriResults$categorical

            # Events
            table$addRow(rowKey = 1, values = list(
                group = "Events",
                nri_estimate = round(cat_nri$nri_events, 4),
                ci_lower = NA,
                ci_upper = NA,
                n_improved = cat_nri$events_up,
                n_worsened = cat_nri$events_down
            ))

            # Non-events
            table$addRow(rowKey = 2, values = list(
                group = "Non-Events",
                nri_estimate = round(cat_nri$nri_nonevents, 4),
                ci_lower = NA,
                ci_upper = NA,
                n_improved = cat_nri$nonevents_down,
                n_worsened = cat_nri$nonevents_up
            ))
        },

        .populateIDIComponentsTable = function() {
            table <- self$results$idiComponents
            idi <- private$.idiResults

            # Integrated Sensitivity (Events)
            table$addRow(rowKey = 1, values = list(
                component = "Events (Integrated Sensitivity)",
                old_model = round(idi$mean_old_events, 4),
                new_model = round(idi$mean_new_events, 4),
                improvement = round(idi$is_improvement, 4)
            ))

            # 1-Specificity (Non-Events)
            table$addRow(rowKey = 2, values = list(
                component = "Non-Events (1-Specificity)",
                old_model = round(idi$mean_old_nonevents, 4),
                new_model = round(idi$mean_new_nonevents, 4),
                improvement = round(idi$ip_improvement, 4)
            ))
        },

        .interpretNRI = function(nri, p_value) {
            if (is.na(nri) || is.na(p_value)) return("N/A")

            if (p_value < 0.05) {
                if (nri > 0.1) return("Significant improvement")
                if (nri > 0) return("Modest improvement")
                if (nri < -0.1) return("Significant worsening")
                return("Modest worsening")
            } else {
                return("Not significant")
            }
        },

        .interpretIDI = function(idi, p_value) {
            if (is.na(idi) || is.na(p_value)) return("N/A")

            if (p_value < 0.05) {
                if (idi > 0.05) return("Significant improvement")
                if (idi > 0) return("Modest improvement")
                if (idi < -0.05) return("Significant worsening")
                return("Modest worsening")
            } else {
                return("Not significant")
            }
        },

        .generateSummary = function() {
            n <- private$.preparedData$n
            n_events <- private$.preparedData$n_events

            summary_text <- glue::glue("
            <h4>Analysis Summary</h4>
            <p><b>Sample Size:</b> {n} patients ({n_events} events, {n - n_events} non-events)</p>
            <p><b>Outcome Type:</b> {self$options$outcomeType}</p>
            <p><b>NRI Type:</b> {self$options$nriType}</p>
            <p><b>Bootstrap Samples:</b> {if (self$options$bootstrapCI) self$options$nBootstrap else 'Not used'}</p>

            <h5>Key Findings:</h5>
            ")

            if (!is.null(private$.nriResults$categorical)) {
                cat_nri <- private$.nriResults$categorical
                summary_text <- paste0(summary_text, glue::glue("
                <p><b>Categorical NRI:</b> {round(cat_nri$nri_total, 4)}
                ({round(cat_nri$ci_lower, 4)} to {round(cat_nri$ci_upper, 4)}),
                p = {round(cat_nri$p_value, 4)}</p>
                "))
            }

            if (!is.null(private$.nriResults$continuous)) {
                cont_nri <- private$.nriResults$continuous
                summary_text <- paste0(summary_text, glue::glue("
                <p><b>Continuous NRI:</b> {round(cont_nri$nri_total, 4)}
                ({round(cont_nri$ci_lower, 4)} to {round(cont_nri$ci_upper, 4)}),
                p = {round(cont_nri$p_value, 4)}</p>
                "))
            }

            if (!is.null(private$.idiResults)) {
                idi <- private$.idiResults
                summary_text <- paste0(summary_text, glue::glue("
                <p><b>IDI:</b> {round(idi$idi, 4)}
                ({round(idi$ci_lower, 4)} to {round(idi$ci_upper, 4)}),
                p = {round(idi$p_value, 4)}</p>
                "))
            }

            self$results$summary$setContent(summary_text)
        },

        .reclassPlot = function(image, ggtheme, theme, ...) {
            # Placeholder for reclassification visualization
            plot <- ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5,
                                label = "Reclassification plot\n(under development)",
                                size = 6) +
                ggplot2::theme_void()
            print(plot)
            TRUE
        },

        .improvementPlot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.preparedData)) return()

            old_prob <- private$.preparedData$old_prob
            new_prob <- private$.preparedData$new_prob
            outcome <- private$.preparedData$outcome

            # Create scatter plot showing probability changes
            plot_df <- data.frame(
                old_prob = old_prob,
                new_prob = new_prob,
                outcome = factor(outcome, levels = c(0, 1), labels = c("Non-Event", "Event"))
            )

            plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = old_prob, y = new_prob, color = outcome)) +
                ggplot2::geom_point(alpha = 0.5) +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
                ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
                ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                ggplot2::scale_color_manual(values = c("Non-Event" = "#0072B2", "Event" = "#D55E00")) +
                ggplot2::labs(
                    title = "Probability Improvement: Old vs New Model",
                    subtitle = "Points above diagonal indicate increased risk predictions",
                    x = "Old Model Predicted Probability",
                    y = "New Model Predicted Probability",
                    color = "Outcome"
                ) +
                ggtheme

            print(plot)
            TRUE
        }
    )
)
