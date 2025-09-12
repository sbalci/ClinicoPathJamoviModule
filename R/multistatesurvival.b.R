#' @title Multistate Survival Models
#' @importFrom R6 R6Class
#' @import jmvcore
#'

multistatesurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "multistatesurvivalClass",
    inherit = multistatesurvivalBase,
    private = list(

        .init = function() {
            # Check for required packages
            if (!requireNamespace('mstate', quietly = TRUE)) {
                self$results$warnings$setContent(
                    "The mstate package is required but not installed.
                    Please install it using: install.packages('mstate')"
                )
            }

            if (!requireNamespace('msm', quietly = TRUE)) {
                self$results$warnings$setContent(
                    "The msm package is recommended for continuous-time models.
                    Install using: install.packages('msm')"
                )
            }

            if (!requireNamespace('survival', quietly = TRUE)) {
                stop("The survival package is required")
            }
        },

        .run = function() {

            # Check if variables are selected
            if (is.null(self$options$id) || is.null(self$options$time_stop)) {
                self$results$instructions$setContent(
                    "<h3>Welcome to Multistate Survival Analysis</h3>
                    <p>Multistate models analyze progression through multiple disease states over time.</p>

                    <h4>Common Applications:</h4>
                    <ul>
                    <li><b>Disease Progression:</b> Healthy → Disease → Death</li>
                    <li><b>Treatment Response:</b> Treatment → Response → Relapse → Death</li>
                    <li><b>Transplantation:</b> Transplant → Rejection → Re-transplant → Death</li>
                    <li><b>Cancer Staging:</b> Stage I → Stage II → Stage III → Stage IV → Death</li>
                    </ul>

                    <h4>Required Data Format:</h4>
                    <ul>
                    <li>One row per transition/interval per patient</li>
                    <li>Start and stop times for each interval</li>
                    <li>From and to states for each transition</li>
                    <li>Patient ID to link transitions</li>
                    </ul>

                    <p>Please select required variables to begin analysis.</p>"
                )
                return()
            }

            # Get data and prepare for analysis
            prepared_data <- private$.prepareData()
            if (is.null(prepared_data)) return()

            # Define transition matrix
            trans_matrix <- private$.defineTransitionMatrix(prepared_data)
            if (is.null(trans_matrix)) return()

            # Fit multistate model
            ms_model <- private$.fitMultistateModel(prepared_data, trans_matrix)
            if (is.null(ms_model)) return()

            # Calculate requested analyses
            if (self$options$transition_probabilities) {
                private$.calculateTransitionProbabilities(ms_model, trans_matrix)
            }

            if (self$options$state_probabilities) {
                private$.calculateStateProbabilities(ms_model, trans_matrix)
            }

            if (self$options$sojourn_times) {
                private$.calculateSojournTimes(ms_model, trans_matrix)
            }

            if (self$options$hazard_ratios) {
                private$.calculateHazardRatios(ms_model, trans_matrix)
            }

            # Generate visualizations
            if (self$options$plot_transitions) {
                private$.plotTransitionDiagram(trans_matrix)
            }

            if (self$options$plot_probabilities) {
                private$.plotStateProbabilities(ms_model, trans_matrix)
            }

            if (self$options$plot_cumhazard) {
                private$.plotCumulativeHazards(ms_model, trans_matrix)
            }
        },

        .prepareData = function() {

            tryCatch({
                # Get variables
                id_var <- self$options$id
                time_start_var <- self$options$time_start
                time_stop_var <- self$options$time_stop
                from_var <- self$options$state_from
                to_var <- self$options$state_to
                covariates <- self$options$covariates

                # Select relevant columns
                vars_needed <- c(id_var, time_stop_var)
                if (!is.null(time_start_var)) vars_needed <- c(vars_needed, time_start_var)
                if (!is.null(from_var)) vars_needed <- c(vars_needed, from_var)
                if (!is.null(to_var)) vars_needed <- c(vars_needed, to_var)
                if (!is.null(covariates)) vars_needed <- c(vars_needed, covariates)

                # Get complete cases
                data <- self$data[, vars_needed]
                data <- data[complete.cases(data), ]

                if (nrow(data) < 10) {
                    stop("Insufficient data for multistate analysis (minimum 10 transitions required)")
                }

                # If no start time, assume 0
                if (is.null(time_start_var)) {
                    data$time_start <- 0
                    time_start_var <- "time_start"
                }

                # If no from/to states, try to infer from data structure
                if (is.null(from_var) || is.null(to_var)) {
                    # Attempt to create simple progression model
                    data <- private$.inferStates(data, id_var, time_stop_var)
                    from_var <- "from"
                    to_var <- "to"
                }

                # Ensure states are factors
                data[[from_var]] <- as.factor(data[[from_var]])
                data[[to_var]] <- as.factor(data[[to_var]])

                # Add status indicator (1 = transition, 0 = censored)
                data$status <- ifelse(data[[to_var]] != data[[from_var]], 1, 0)

                # Store prepared data
                private$prepared_data <- data
                private$variable_names <- list(
                    id = id_var,
                    time_start = time_start_var,
                    time_stop = time_stop_var,
                    from = from_var,
                    to = to_var,
                    covariates = covariates
                )

                return(data)

            }, error = function(e) {
                self$results$errors$setContent(
                    paste("Data preparation failed:", e$message)
                )
                return(NULL)
            })
        },

        .defineTransitionMatrix = function(data) {

            tryCatch({
                if (!requireNamespace('mstate', quietly = TRUE)) {
                    stop("mstate package is required")
                }

                # Get unique states
                from_var <- private$variable_names$from
                to_var <- private$variable_names$to

                all_states <- unique(c(
                    as.character(data[[from_var]]),
                    as.character(data[[to_var]])
                ))
                all_states <- sort(all_states)
                n_states <- length(all_states)

                # Check for custom transition matrix
                if (self$options$transition_matrix != "auto") {
                    trans_matrix <- private$.parseTransitionMatrix(
                        self$options$transition_matrix,
                        all_states
                    )
                } else {
                    # Build transition matrix from data
                    trans_matrix <- matrix(NA, n_states, n_states)
                    rownames(trans_matrix) <- colnames(trans_matrix) <- all_states

                    # Find all observed transitions
                    trans_counter <- 1
                    for (i in 1:n_states) {
                        for (j in 1:n_states) {
                            if (i != j) {
                                # Check if transition exists in data
                                trans_exists <- any(
                                    data[[from_var]] == all_states[i] &
                                    data[[to_var]] == all_states[j]
                                )
                                if (trans_exists) {
                                    trans_matrix[i, j] <- trans_counter
                                    trans_counter <- trans_counter + 1
                                }
                            }
                        }
                    }
                }

                # Apply custom state names if provided
                if (self$options$state_names != "") {
                    custom_names <- trimws(strsplit(self$options$state_names, ",")[[1]])
                    if (length(custom_names) == n_states) {
                        rownames(trans_matrix) <- colnames(trans_matrix) <- custom_names
                    }
                }

                # Store transition matrix
                private$trans_matrix <- trans_matrix

                # Display transition matrix
                private$.displayTransitionMatrix(trans_matrix)

                return(trans_matrix)

            }, error = function(e) {
                self$results$errors$setContent(
                    paste("Transition matrix definition failed:", e$message)
                )
                return(NULL)
            })
        },

        .fitMultistateModel = function(data, trans_matrix) {

            tryCatch({
                if (!requireNamespace('mstate', quietly = TRUE)) {
                    stop("mstate package is required")
                }

                # Prepare data in mstate format
                vars <- private$variable_names

                # Convert to long format for mstate
                ms_data <- mstate::msprep(
                    time = c(NA, data[[vars$time_stop]]),
                    status = c(NA, data$status),
                    data = data,
                    trans = trans_matrix,
                    id = data[[vars$id]]
                )

                # Expand covariates if provided
                if (!is.null(vars$covariates)) {
                    ms_data <- mstate::expand.covs(
                        ms_data,
                        vars$covariates,
                        append = TRUE
                    )
                }

                # Fit Cox models for each transition
                model_type <- self$options$model_type %||% "markov"

                if (model_type == "markov") {
                    # Markov model - clock resets at each transition
                    cox_formula <- "Surv(Tstart, Tstop, status) ~ strata(trans)"
                } else if (model_type == "clock_forward") {
                    # Clock-forward model - time since study entry
                    cox_formula <- "Surv(time, status) ~ strata(trans)"
                } else {
                    # Semi-Markov or clock-reset
                    cox_formula <- "Surv(Tstart, Tstop, status) ~ strata(trans)"
                }

                # Add covariates to formula
                if (!is.null(vars$covariates)) {
                    covar_terms <- paste(vars$covariates, collapse = " + ")
                    cox_formula <- paste(cox_formula, "+", covar_terms)
                }

                # Fit the model
                ms_model <- survival::coxph(
                    as.formula(cox_formula),
                    data = ms_data,
                    method = "breslow"
                )

                # Store model and data
                private$ms_model <- ms_model
                private$ms_data <- ms_data

                # Display model summary
                private$.displayModelSummary(ms_model, trans_matrix)

                return(ms_model)

            }, error = function(e) {
                self$results$errors$setContent(
                    paste("Model fitting failed:", e$message)
                )
                return(NULL)
            })
        },

        .calculateTransitionProbabilities = function(model, trans_matrix) {

            tryCatch({
                if (!requireNamespace('mstate', quietly = TRUE)) {
                    stop("mstate package is required")
                }

                # Get prediction times
                pred_times_str <- self$options$prediction_times %||% "6,12,24,60"
                pred_times <- as.numeric(strsplit(pred_times_str, ",")[[1]])

                # Calculate transition probabilities
                # This requires the cumulative hazards first
                ms_fit <- mstate::msfit(
                    model,
                    trans = trans_matrix
                )

                # Calculate transition probabilities from each state
                n_states <- nrow(trans_matrix)
                trans_probs <- list()

                for (from_state in 1:n_states) {
                    # Probabilities starting from this state
                    pt <- mstate::probtrans(
                        ms_fit,
                        predt = pred_times,
                        direction = "forward",
                        variance = TRUE
                    )

                    trans_probs[[from_state]] <- pt
                }

                # Store results
                private$trans_probs <- trans_probs

                # Display transition probabilities
                private$.displayTransitionProbabilities(trans_probs, trans_matrix, pred_times)

            }, error = function(e) {
                message("Transition probability calculation failed: ", e$message)
            })
        },

        .calculateStateProbabilities = function(model, trans_matrix) {

            tryCatch({
                if (!requireNamespace('mstate', quietly = TRUE)) {
                    stop("mstate package is required")
                }

                # Calculate state occupation probabilities over time
                ms_fit <- mstate::msfit(model, trans = trans_matrix)

                # Get initial state
                initial_state <- self$options$initial_state %||% "1"
                if (is.character(initial_state)) {
                    initial_state <- which(rownames(trans_matrix) == initial_state)
                } else {
                    initial_state <- as.numeric(initial_state)
                }

                # Calculate probabilities
                pt <- mstate::probtrans(
                    ms_fit,
                    predt = 0,  # Starting from time 0
                    direction = "forward"
                )

                # Extract state probabilities at different times
                times <- seq(0, max(private$ms_data$Tstop, na.rm = TRUE), length.out = 100)
                state_probs <- matrix(NA, length(times), nrow(trans_matrix))

                for (i in seq_along(times)) {
                    pt_t <- mstate::probtrans(ms_fit, predt = times[i])
                    state_probs[i, ] <- pt_t[[initial_state]]$pstate
                }

                # Store results
                private$state_probs <- list(
                    times = times,
                    probs = state_probs,
                    states = rownames(trans_matrix)
                )

                # Display results
                private$.displayStateProbabilities(state_probs, times, rownames(trans_matrix))

            }, error = function(e) {
                message("State probability calculation failed: ", e$message)
            })
        },

        .calculateSojournTimes = function(model, trans_matrix) {

            tryCatch({
                # Calculate expected time spent in each state
                n_states <- nrow(trans_matrix)
                state_names <- rownames(trans_matrix)

                # For each non-absorbing state, calculate mean sojourn time
                sojourn_times <- numeric(n_states)
                names(sojourn_times) <- state_names

                # Identify absorbing states
                absorbing <- apply(trans_matrix, 1, function(x) all(is.na(x)))

                for (i in 1:n_states) {
                    if (!absorbing[i]) {
                        # Calculate mean time in state before transition
                        state_data <- private$ms_data[private$ms_data$from == i, ]
                        if (nrow(state_data) > 0) {
                            sojourn_times[i] <- mean(
                                state_data$Tstop - state_data$Tstart,
                                na.rm = TRUE
                            )
                        }
                    } else {
                        sojourn_times[i] <- Inf  # Absorbing state
                    }
                }

                # Display results
                private$.displaySojournTimes(sojourn_times)

            }, error = function(e) {
                message("Sojourn time calculation failed: ", e$message)
            })
        },

        .calculateHazardRatios = function(model, trans_matrix) {

            tryCatch({
                # Extract hazard ratios for each transition
                if (length(coef(model)) > 0) {
                    hrs <- exp(coef(model))
                    ci <- exp(confint(model, level = self$options$confidence_level))
                    p_values <- summary(model)$coefficients[, "Pr(>|z|)"]

                    # Create results table
                    hr_table <- self$results$hazardRatiosTable

                    for (i in seq_along(hrs)) {
                        hr_table$addRow(rowKey = i, values = list(
                            transition = names(hrs)[i],
                            hr = round(hrs[i], 3),
                            ci_lower = round(ci[i, 1], 3),
                            ci_upper = round(ci[i, 2], 3),
                            p_value = round(p_values[i], 4)
                        ))
                    }
                }

                # Generate interpretation
                private$.interpretHazardRatios(model)

            }, error = function(e) {
                message("Hazard ratio calculation failed: ", e$message)
            })
        },

        .displayTransitionMatrix = function(trans_matrix) {

            # Format transition matrix for display
            matrix_html <- "<h4>Transition Matrix</h4>"
            matrix_html <- paste0(matrix_html, "<table border='1' style='border-collapse: collapse;'>")
            matrix_html <- paste0(matrix_html, "<tr><th>From \\ To</th>")

            # Column headers
            for (col_name in colnames(trans_matrix)) {
                matrix_html <- paste0(matrix_html, "<th>", col_name, "</th>")
            }
            matrix_html <- paste0(matrix_html, "</tr>")

            # Matrix cells
            for (i in 1:nrow(trans_matrix)) {
                matrix_html <- paste0(matrix_html, "<tr><th>", rownames(trans_matrix)[i], "</th>")
                for (j in 1:ncol(trans_matrix)) {
                    cell_value <- if (is.na(trans_matrix[i, j])) "-" else trans_matrix[i, j]
                    matrix_html <- paste0(matrix_html, "<td style='text-align: center;'>", cell_value, "</td>")
                }
                matrix_html <- paste0(matrix_html, "</tr>")
            }
            matrix_html <- paste0(matrix_html, "</table>")

            matrix_html <- paste0(matrix_html,
                "<p><em>Numbers indicate transition IDs; '-' indicates no direct transition allowed.</em></p>")

            self$results$transitionMatrix$setContent(matrix_html)
        },

        .displayModelSummary = function(model, trans_matrix) {

            n_trans <- max(trans_matrix, na.rm = TRUE)
            n_states <- nrow(trans_matrix)
            n_patients <- length(unique(private$prepared_data[[private$variable_names$id]]))
            n_events <- sum(private$prepared_data$status == 1)

            summary_html <- glue::glue(
                "<h3>Multistate Model Summary</h3>
                <p><b>Model Type:</b> {self$options$model_type}</p>
                <p><b>Number of States:</b> {n_states}</p>
                <p><b>Number of Transitions:</b> {n_trans}</p>
                <p><b>Number of Patients:</b> {n_patients}</p>
                <p><b>Number of Events:</b> {n_events}</p>
                <p><b>Log-likelihood:</b> {round(model$loglik[2], 2)}</p>

                <h4>Model Description:</h4>
                <p>This multistate model analyzes progression through {n_states} disease states
                with {n_trans} possible transitions. The model accounts for competing risks
                when multiple transitions are possible from the same state.</p>"
            )

            self$results$summary$setContent(summary_html)
        },

        .plotTransitionDiagram = function(trans_matrix) {
            # This would create a visual diagram of the state transitions
            # Implementation would use diagram or DiagrammeR package
            image <- self$results$transitionPlot
            image$setState(list(trans_matrix = trans_matrix))
        },

        .plotStateProbabilities = function(model, trans_matrix) {
            # Plot state occupation probabilities over time
            if (!is.null(private$state_probs)) {
                image <- self$results$probabilityPlot
                image$setState(private$state_probs)
            }
        },

        .plotCumulativeHazards = function(model, trans_matrix) {
            # Plot cumulative hazards for each transition
            image <- self$results$cumhazardPlot
            image$setState(list(model = model, trans_matrix = trans_matrix))
        }
    )
)
