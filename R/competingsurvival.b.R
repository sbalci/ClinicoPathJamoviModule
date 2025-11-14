#' @title Competing Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

competingsurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "competingsurvivalClass",
    inherit = competingsurvivalBase,
    private = list(

        .escapeVar = function(x) {
            # Escape variable names for safe use (handle spaces, special chars)
            if (is.null(x) || length(x) == 0) return(x)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        .getData = function() {
            # Clean and label data following survival module pattern
            mydata <- self$data
            mydata$row_names <- rownames(mydata)
            original_names <- names(mydata)
            labels <- setNames(original_names, original_names)
            
            mydata <- mydata %>% janitor::clean_names()
            corrected_labels <- setNames(original_names, names(mydata))
            mydata <- labelled::set_variable_labels(.data = mydata, .labels = corrected_labels)
            all_labels <- labelled::var_label(mydata)
            
            # Get variable names from labels
            mytime <- names(all_labels)[all_labels == self$options$overalltime]
            myoutcome <- names(all_labels)[all_labels == self$options$outcome]
            myexplanatory <- names(all_labels)[all_labels == self$options$explanatory]
            
            return(list(
                "mydata_labelled" = mydata,
                "mytime_labelled" = mytime,
                "myoutcome_labelled" = myoutcome, 
                "myexplanatory_labelled" = myexplanatory
            ))
        },
        
        .run = function() {
            # Check required packages
            if (!requireNamespace('finalfit', quietly = TRUE)) {
                stop('The finalfit package is required but not installed')
            }
            if (!requireNamespace('cmprsk', quietly = TRUE)) {
                stop('The cmprsk package is required but not installed')
            }
            
            # If no variable selected Initial Message ----
            if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$overalltime)) {
                todo <- glue::glue("
<br>Welcome to Competing Survival Analysis<br><hr><br>
<br>
The explanation below is adopted from <a href = 'https://finalfit.org/articles/survival.html#death-status'>finalfit website documentation</a>.
<br><br>
<b>Outcome</b> is the status at the time of study:<br>
• <b>Dead of Disease</b>: Patients had died from disease<br>
• <b>Dead of Other</b>: Patients had died from other causes<br>
• <b>Alive w Disease</b>: Patients are alive and still have disease<br>
• <b>Alive w/o Disease</b>: Patients are alive and free of disease<br>
<br>
<b>Analysis Types:</b><br>
• <b>Overall survival</b>: All-cause mortality (Alive vs All Deaths)<br>
• <b>Cause-specific survival</b>: Disease-specific mortality only<br>
• <b>Competing risks</b>: Disease death accounting for other deaths<br>
<br>
<b>Required Variables:</b><br>
• Explanatory Variable: Treatment group or other factor<br>
• Overall Time: Follow-up time in months<br>
• Outcome: Multi-level factor with death/alive status<br>
<br>
This function uses survival, survminer, finalfit, and cmprsk packages.
<br><hr>
                ")
                
                html <- self$results$todo
                html$setContent(todo)
                return()
            }
            
            # Validate data
            if (nrow(self$data) == 0) {
                stop('Data contains no (complete) rows')
            }
            
            # Get processed data
            labelled_data <- private$.getData()
            mydata <- labelled_data$mydata_labelled
            mytime <- labelled_data$mytime_labelled
            myoutcome <- labelled_data$myoutcome_labelled
            myexplanatory <- labelled_data$myexplanatory_labelled
            
            # Get analysis options
            dod <- self$options$dod
            dooc <- self$options$dooc
            awd <- self$options$awd
            awod <- self$options$awod
            analysistype <- self$options$analysistype

            # Validate event levels based on analysis type
            if (analysistype == "overall") {
                # Need at least one death type defined
                if (is.null(dod) && is.null(dooc)) {
                    stop("For overall survival, please specify at least one death type (Dead of Disease or Dead of Other)")
                }
            } else if (analysistype == "cause") {
                # Need disease death defined
                if (is.null(dod)) {
                    stop("For cause-specific survival, please specify 'Dead of Disease' level")
                }
            } else if (analysistype == "compete") {
                # Need both death types defined
                if (is.null(dod)) {
                    stop("For competing risks analysis, please specify 'Dead of Disease' level")
                }
                if (is.null(dooc)) {
                    stop("For competing risks analysis, please specify 'Dead of Other' level")
                }
            }
            
            # Clean data and handle missing values
            # Only filter explanatory if it's provided
            if (!is.null(myexplanatory) && myexplanatory != "") {
                mydata <- mydata %>%
                    dplyr::filter(!is.na(.data[[mytime]]), !is.na(.data[[myoutcome]]), !is.na(.data[[myexplanatory]]))
            } else {
                mydata <- mydata %>%
                    dplyr::filter(!is.na(.data[[mytime]]), !is.na(.data[[myoutcome]]))
            }
            
            if (nrow(mydata) == 0) {
                stop('No complete cases available for analysis')
            }
            
            # Perform analysis based on type
            private$.performAnalysis(mydata, mytime, myoutcome, myexplanatory, analysistype, dod, dooc, awd, awod)
        },
        
        .performAnalysis = function(mydata, mytime, myoutcome, myexplanatory, analysistype, dod, dooc, awd, awod) {
            
            # Overall survival analysis
            if (analysistype == "overall") {
                private$.overallSurvival(mydata, mytime, myoutcome, myexplanatory, dod, dooc, awd, awod)
            } else if (analysistype == "cause") {
                private$.causeSpecificSurvival(mydata, mytime, myoutcome, myexplanatory, dod, dooc, awd, awod)
            } else if (analysistype == "compete") {
                private$.competingRisksSurvival(mydata, mytime, myoutcome, myexplanatory, dod, dooc, awd, awod)
            }
        },
        
        .overallSurvival = function(mydata, mytime, myoutcome, myexplanatory, dod, dooc, awd, awod) {
            # Create overall survival outcome: all deaths = 1, alive = 0
            # Build death levels vector (only non-NULL values)
            death_levels <- c(dod, dooc)
            death_levels <- death_levels[!sapply(death_levels, is.null)]

            mydata$status_os <- ifelse(
                mydata[[myoutcome]] %in% death_levels, 1, 0
            )
            
            # Create survival object
            dependent_os <- paste0("Surv(", mytime, ", status_os)")
            
            # Perform survival analysis
            tryCatch({
                # Handle case where no explanatory variable is provided
                if (!is.null(myexplanatory) && myexplanatory != "") {
                    result <- mydata %>%
                        finalfit::finalfit(dependent_os, myexplanatory, add_dependent_label = FALSE)
                } else {
                    # Simple survival without groups
                    surv_obj <- survival::Surv(mydata[[mytime]], mydata$status_os)
                    fit <- survival::survfit(surv_obj ~ 1)
                    
                    # Create a simple result table
                    result <- data.frame(
                        label = "Overall",
                        levels = "All patients",
                        n = length(surv_obj),
                        events = sum(mydata$status_os),
                        median = round(summary(fit)$table["median"], 1),
                        stringsAsFactors = FALSE
                    )
                }
                
                # Extract and format results
                private$.formatSurvivalResults(result, "Overall Survival")
                
            }, error = function(e) {
                stop(paste("Overall survival analysis failed:", e$message))
            })
        },
        
        .causeSpecificSurvival = function(mydata, mytime, myoutcome, myexplanatory, dod, dooc, awd, awod) {
            # Create cause-specific survival outcome: disease death = 1, others = 0
            mydata$status_dss <- ifelse(
                mydata[[myoutcome]] == dod, 1, 0
            )
            
            # Create survival object
            dependent_dss <- paste0("Surv(", mytime, ", status_dss)")
            
            # Perform survival analysis
            tryCatch({
                # Handle case where no explanatory variable is provided
                if (!is.null(myexplanatory) && myexplanatory != "") {
                    result <- mydata %>%
                        finalfit::finalfit(dependent_dss, myexplanatory, add_dependent_label = FALSE)
                } else {
                    # Simple survival without groups
                    surv_obj <- survival::Surv(mydata[[mytime]], mydata$status_dss)
                    fit <- survival::survfit(surv_obj ~ 1)
                    
                    # Create a simple result table
                    result <- data.frame(
                        label = "Disease-Specific",
                        levels = "All patients",
                        n = length(surv_obj),
                        events = sum(mydata$status_dss),
                        median = round(summary(fit)$table["median"], 1),
                        stringsAsFactors = FALSE
                    )
                }
                
                # Extract and format results
                private$.formatSurvivalResults(result, "Cause-Specific Survival")
                
            }, error = function(e) {
                stop(paste("Cause-specific survival analysis failed:", e$message))
            })
        },
 
        .competingRisksSurvival = function(mydata, mytime, myoutcome, myexplanatory, dod, dooc, awd, awod) {
            # Create competing risks outcome: alive=0, disease death=1, other death=2
            # Build censored levels vector (only non-NULL values)
            censored_levels <- c(awd, awod)
            censored_levels <- censored_levels[!sapply(censored_levels, is.null)]

            mydata$status_crr <- dplyr::case_when(
                !is.null(dod) & mydata[[myoutcome]] == dod ~ 1,              # disease death
                !is.null(dooc) & mydata[[myoutcome]] == dooc ~ 2,            # other death
                length(censored_levels) > 0 & mydata[[myoutcome]] %in% censored_levels ~ 0,  # alive/censored
                TRUE ~ 0  # default to censored
            )
            
            # Get analysis options
            graystest <- self$options$graystest %||% FALSE
            subdistribution <- self$options$subdistribution %||% FALSE
            timepoints_str <- self$options$timepoints %||% "12,24,36,60"
            conf_level <- self$options$confidencelevel %||% 0.95
            showrisksets <- self$options$showrisksets %||% FALSE
            
            # Parse time points
            timepoints <- as.numeric(unlist(strsplit(timepoints_str, ",")))
            
            # Create survival object for competing risks
            dependent_crr <- paste0("Surv(", mytime, ", status_crr)")
            
            tryCatch({
                # Extract variables
                time_var <- mydata[[mytime]]
                status_var <- mydata$status_crr
                group_var <- if (!is.null(myexplanatory) && myexplanatory != "") {
                    mydata[[myexplanatory]]
                } else {
                    NULL
                }
                
                # Calculate cumulative incidence function
                if (!is.null(group_var)) {
                    cuminc_result <- cmprsk::cuminc(
                        ftime = time_var,
                        fstatus = status_var,
                        group = group_var,
                        cencode = 0
                    )
                } else {
                    cuminc_result <- cmprsk::cuminc(
                        ftime = time_var,
                        fstatus = status_var,
                        cencode = 0
                    )
                }
                
                # Perform Gray's test if requested
                grays_test_result <- NULL
                if (graystest) {
                    grays_test_result <- tryCatch({
                        if (!is.null(cuminc_result) && is.list(cuminc_result) && !is.null(cuminc_result$Tests)) {
                            private$.performGraysTest(cuminc_result)
                        } else {
                            NULL
                        }
                    }, error = function(e) {
                        message("Error performing Gray's test: ", e$message)
                        NULL
                    })
                }
                
                # Subdistribution hazard model (Fine-Gray model)
                subdist_result <- NULL
                if (subdistribution) {
                    subdist_result <- private$.performSubdistributionAnalysis(
                        mydata, time_var, status_var, group_var, 
                        myexplanatory, conf_level
                    )
                }
                
                # Traditional competing risks regression using finalfit
                result_crr <- NULL
                if (!is.null(myexplanatory) && myexplanatory != "") {
                    result_crr <- mydata %>%
                        finalfit::crrmulti(dependent_crr, myexplanatory) %>%
                        finalfit::fit2df(estimate_suffix = " (Competing Risks)")
                } else {
                    # Create simple summary without groups
                    result_crr <- data.frame(
                        label = "Competing Risks",
                        levels = "All patients",
                        n = nrow(mydata),
                        events_primary = sum(mydata$status_crr == 1),
                        events_competing = sum(mydata$status_crr == 2),
                        stringsAsFactors = FALSE
                    )
                }
                
                # Calculate cumulative incidence at specific time points
                cuminc_timepoints <- tryCatch({
                    private$.calculateCumIncAtTimepoints(cuminc_result, timepoints)
                }, error = function(e) {
                    message("Error calculating cumulative incidence at timepoints: ", e$message)
                    NULL
                })
                
                # Format all results
                private$.formatEnhancedCompetingRisksResults(
                    result_crr, cuminc_result, grays_test_result,
                    subdist_result, cuminc_timepoints, timepoints
                )
                
                # Add notice if showrisksets is enabled but not yet implemented
                if (showrisksets) {
                    notice_text <- "<p><strong>Note:</strong> The 'Show Number at Risk' option is currently in development. Risk tables will be added in a future update.</p>"
                    current_summary <- self$results$summary$content
                    self$results$summary$setContent(paste0(current_summary, notice_text))
                }

                # Store data for plotting
                self$results$comprisksPlot$setState(list(
                    "data" = mydata,
                    "time_var" = mytime,
                    "status_var" = "status_crr",
                    "group_var" = myexplanatory,
                    "cuminc" = cuminc_result,
                    "showrisksets" = showrisksets,
                    "timepoints" = timepoints
                ))
                
            }, error = function(e) {
                stop(paste("Competing risks analysis failed:", e$message))
            })
        },
        
        .performGraysTest = function(cuminc_result) {
            # Extract Gray's test results from cuminc object
            tryCatch({
                tests <- cuminc_result$Tests
                
                if (!is.null(tests)) {
                    # Format test results
                    test_results <- list()
                    
                    # Disease death comparison
                    if (!is.null(tests) && nrow(tests) > 0) {
                        test_results$disease_death <- list(
                            statistic = tests[1, "stat"],
                            p_value = tests[1, "pv"],
                            df = tests[1, "df"]
                        )
                    }
                    
                    # Other cause death comparison if available
                    if (!is.null(tests) && nrow(tests) > 1) {
                        test_results$other_death <- list(
                            statistic = tests[2, "stat"],
                            p_value = tests[2, "pv"],
                            df = tests[2, "df"]
                        )
                    }
                    
                    return(test_results)
                }
                
                return(NULL)
                
            }, error = function(e) {
                message("Gray's test could not be performed: ", e$message)
                return(NULL)
            })
        },
        
        .performSubdistributionAnalysis = function(mydata, time_var, status_var,
                                                  group_var, myexplanatory, conf_level) {
            # Perform Fine-Gray subdistribution hazard model
            tryCatch({
                # Only perform if we have a group variable
                if (is.null(group_var)) {
                    return(NULL)
                }

                # Prepare data for crr function
                # Handle categorical variables properly (create contrasts for multi-level factors)
                if (is.factor(group_var) || is.character(group_var)) {
                    # Convert to factor if character
                    if (is.character(group_var)) {
                        group_var <- factor(group_var)
                    }

                    # Check number of levels
                    n_levels <- length(levels(group_var))

                    if (n_levels < 2) {
                        message("Not enough groups for subdistribution analysis")
                        return(NULL)
                    } else if (n_levels == 2) {
                        # Binary factor: safe to convert to 0/1
                        group_matrix <- as.numeric(group_var) - 1
                        group_labels <- paste0(levels(group_var)[2], " vs ", levels(group_var)[1], " (ref)")
                    } else {
                        # Multi-level factor: create dummy variables (treatment contrasts)
                        # WARNING: cmprsk::crr expects a matrix for multiple covariates
                        # For now, we'll warn the user that only binary comparisons are supported
                        warning(paste0(
                            "Fine-Gray model in this module only supports binary comparisons. ",
                            "Variable '", myexplanatory, "' has ", n_levels, " levels. ",
                            "Results will compare all groups against the reference (", levels(group_var)[1], ") ",
                            "but interpretation may be complex. Consider creating a binary variable."
                        ))

                        # Create contrast matrix (treatment coding)
                        group_matrix <- model.matrix(~ group_var)[, -1, drop = FALSE]
                        group_labels <- paste0("Multi-level comparison (", n_levels, " groups)")
                    }
                } else {
                    # Numeric variable
                    group_matrix <- as.matrix(group_var)
                    group_labels <- "Continuous predictor (per unit increase)"
                }

                # Ensure matrix format
                if (!is.matrix(group_matrix)) {
                    group_matrix <- as.matrix(group_matrix)
                }

                # Check if we have valid data
                if (nrow(group_matrix) == 0 || ncol(group_matrix) == 0) {
                    message("Invalid covariate matrix for subdistribution analysis")
                    return(NULL)
                }

                # Fit Fine-Gray model for disease death (failcode = 1)
                fg_model <- cmprsk::crr(
                    ftime = time_var,
                    fstatus = status_var,
                    cov1 = group_matrix,
                    failcode = 1,  # Disease death
                    cencode = 0    # Censored
                )

                # Check if model fitted successfully
                if (is.null(fg_model$coef) || length(fg_model$coef) == 0) {
                    return(NULL)
                }

                # Extract coefficients and confidence intervals
                # For binary factors, report the single coefficient
                # For multi-level, report first coefficient with warning that full results need interpretation
                coef_val <- fg_model$coef[1]
                se_val <- sqrt(fg_model$var[1, 1])
                z_val <- qnorm((1 + conf_level) / 2)

                # Calculate HR and CI
                hr <- exp(coef_val)
                ci_lower <- exp(coef_val - z_val * se_val)
                ci_upper <- exp(coef_val + z_val * se_val)
                p_value <- 2 * (1 - pnorm(abs(coef_val / se_val)))

                subdist_result <- list(
                    hr = as.numeric(hr),
                    ci_lower = as.numeric(ci_lower),
                    ci_upper = as.numeric(ci_upper),
                    p_value = as.numeric(p_value),
                    model = fg_model,
                    comparison = group_labels,
                    n_coef = length(fg_model$coef)  # Track if multiple coefficients
                )

                return(subdist_result)

            }, error = function(e) {
                message("Subdistribution hazard model could not be fitted: ", e$message)
                return(NULL)
            })
        },
        
        .calculateCumIncAtTimepoints = function(cuminc_result, timepoints) {
            # Calculate cumulative incidence at specific time points
            tryCatch({
                # Validate input
                if (is.null(cuminc_result) || !is.list(cuminc_result) || length(cuminc_result) == 0) {
                    return(NULL)
                }
                
                # Check if timepoints is valid
                if (is.null(timepoints) || length(timepoints) == 0) {
                    return(NULL)
                }
                
                results <- list()
                
                # Get group names (excluding test results)
                all_names <- names(cuminc_result)
                if (is.null(all_names)) {
                    return(NULL)
                }
                
                group_names <- all_names[!all_names %in% c("Tests")]
                
                if (length(group_names) == 0) {
                    return(NULL)
                }
                
                for (tp in timepoints) {
                    tp_results <- list()
                    
                    for (gname in group_names) {
                        # Check if the group exists and has the expected structure
                        if (!gname %in% names(cuminc_result)) {
                            next
                        }
                        
                        group_data <- cuminc_result[[gname]]
                        
                        # Validate group data structure
                        if (!is.list(group_data) || 
                            is.null(group_data$time) || 
                            is.null(group_data$est) || 
                            is.null(group_data$var)) {
                            next
                        }
                        
                        # Extract time and estimates
                        times <- group_data$time
                        ests <- group_data$est
                        vars <- group_data$var
                        
                        # Validate that all vectors have the same length
                        if (length(times) != length(ests) || length(times) != length(vars)) {
                            next
                        }
                        
                        # Find closest time point
                        if (length(times) > 0) {
                            idx <- which.min(abs(times - tp))
                            
                            if (length(idx) > 0 && idx <= length(times)) {
                                tp_results[[gname]] <- list(
                                    time = tp,
                                    estimate = ests[idx],
                                    se = sqrt(max(vars[idx], 0)),  # Ensure non-negative variance
                                    ci_lower = ests[idx] - 1.96 * sqrt(max(vars[idx], 0)),
                                    ci_upper = ests[idx] + 1.96 * sqrt(max(vars[idx], 0))
                                )
                            }
                        }
                    }
                    
                    if (length(tp_results) > 0) {
                        results[[as.character(tp)]] <- tp_results
                    }
                }
                
                return(results)
                
            }, error = function(e) {
                message("Could not calculate cumulative incidence at timepoints: ", e$message)
                return(NULL)
            })
        },
        
        .formatEnhancedCompetingRisksResults = function(result_crr, cuminc_result,
                                                       grays_test_result, subdist_result,
                                                       cuminc_timepoints, timepoints = NULL) {
            # Format traditional competing risks results
            if (is.data.frame(result_crr) && nrow(result_crr) > 0) {
                hr_col <- grep("HR", names(result_crr), value = TRUE)[1]
                if (!is.null(hr_col) && !is.na(hr_col)) {
                    hr_text <- result_crr[[hr_col]][1]
                    parsed <- private$.parseHRText(hr_text)
                    
                    table <- self$results$survivalTable
                    table$addRow(rowKey = 1, values = list(
                        term = paste(result_crr[[1]][1], "(Standard CR)"),
                        hr = parsed$hr,
                        ci_lower = parsed$ci_lower,
                        ci_upper = parsed$ci_upper,
                        p_value = parsed$p_value
                    ))
                }
            }
            
            # Add subdistribution hazard results
            if (!is.null(subdist_result) && is.list(subdist_result)) {
                # Check if all required elements are present and numeric
                if (!is.null(subdist_result$hr) && !is.null(subdist_result$ci_lower) &&
                    !is.null(subdist_result$ci_upper) && !is.null(subdist_result$p_value)) {

                    # Build term name with comparison info
                    term_name <- if (!is.null(subdist_result$comparison)) {
                        paste0("Subdistribution HR (Fine-Gray): ", subdist_result$comparison)
                    } else {
                        "Subdistribution HR (Fine-Gray)"
                    }

                    table <- self$results$survivalTable
                    table$addRow(rowKey = 2, values = list(
                        term = term_name,
                        hr = round(as.numeric(subdist_result$hr), 3),
                        ci_lower = round(as.numeric(subdist_result$ci_lower), 3),
                        ci_upper = round(as.numeric(subdist_result$ci_upper), 3),
                        p_value = round(as.numeric(subdist_result$p_value), 4)
                    ))
                } else {
                    # Add a row indicating subdistribution analysis failed
                    table <- self$results$survivalTable
                    table$addRow(rowKey = 2, values = list(
                        term = "Subdistribution HR (Fine-Gray)",
                        hr = NA,
                        ci_lower = NA,
                        ci_upper = NA,
                        p_value = NA
                    ))
                }
            }
            
            # Format Gray's test results
            if (!is.null(grays_test_result)) {
                grays_text <- "<h4>Gray's Test Results</h4><ul>"
                
                if (!is.null(grays_test_result$disease_death)) {
                    grays_text <- paste0(grays_text,
                        "<li>Disease Death: χ² = ", 
                        round(grays_test_result$disease_death$statistic, 2),
                        ", df = ", grays_test_result$disease_death$df,
                        ", p = ", round(grays_test_result$disease_death$p_value, 4),
                        "</li>"
                    )
                }
                
                if (!is.null(grays_test_result$other_death)) {
                    grays_text <- paste0(grays_text,
                        "<li>Other Cause Death: χ² = ", 
                        round(grays_test_result$other_death$statistic, 2),
                        ", df = ", grays_test_result$other_death$df,
                        ", p = ", round(grays_test_result$other_death$p_value, 4),
                        "</li>"
                    )
                }
                
                grays_text <- paste0(grays_text, "</ul>")
                
                # Add to summary
                current_summary <- self$results$summary$content
                self$results$summary$setContent(paste0(current_summary, grays_text))
            }
            
            # Format cumulative incidence at timepoints
            if (!is.null(cuminc_timepoints)) {
                private$.formatCumulativeIncidenceTimepoints(cuminc_timepoints)
            }

            # Format cumulative incidence results
            if (!is.null(cuminc_result)) {
                private$.formatCumulativeIncidence(cuminc_result, timepoints)
            }

            # Note: Advanced risk stratification and time-dependent CIF functions
            # are available (.performRiskStratification, .performTimeDependentCIF)
            # but not currently integrated into output to reduce computation cost

            # Generate summary (only list features actually implemented and displayed)
            summary_text <- glue::glue(
                "<h3>Competing Risks Analysis Results</h3>
                <p>Analysis includes:</p>
                <ul>
                <li>Fine-Gray subdistribution hazard model for competing risks regression</li>
                <li>Gray's test for comparing cumulative incidence functions between groups</li>
                <li>Cumulative incidence estimates at specified time points</li>
                </ul>
                <p>Results account for the competing nature of different death causes.</p>"
            )

            self$results$summary$setContent(summary_text)
            private$.generateInterpretation("Competing Risks")
        },
        
        .formatCumulativeIncidenceTimepoints = function(cuminc_timepoints) {
            # Create a formatted table of cumulative incidence at specific timepoints
            if (!is.null(cuminc_timepoints) && length(cuminc_timepoints) > 0) {
                timepoint_text <- "<h4>Cumulative Incidence at Key Time Points</h4>"
                timepoint_text <- paste0(timepoint_text, "<table border='1' style='border-collapse: collapse;'>")
                timepoint_text <- paste0(timepoint_text, "<tr><th>Time (months)</th><th>Group</th><th>Estimate</th><th>95% CI</th></tr>")
                
                for (tp_name in names(cuminc_timepoints)) {
                    tp_data <- cuminc_timepoints[[tp_name]]
                    
                    for (group_name in names(tp_data)) {
                        group_data <- tp_data[[group_name]]
                        
                        timepoint_text <- paste0(timepoint_text,
                            "<tr><td>", group_data$time, "</td>",
                            "<td>", group_name, "</td>",
                            "<td>", round(group_data$estimate, 3), "</td>",
                            "<td>(", round(group_data$ci_lower, 3), ", ", 
                            round(group_data$ci_upper, 3), ")</td></tr>"
                        )
                    }
                }
                
                timepoint_text <- paste0(timepoint_text, "</table>")
                
                # Add to summary
                current_summary <- self$results$summary$content
                self$results$summary$setContent(paste0(current_summary, timepoint_text))
            }
        },
 
        .formatSurvivalResults = function(result, analysis_type) {
            # Extract coefficients from finalfit result
            if (is.data.frame(result) && nrow(result) > 0) {
                table <- self$results$survivalTable
                
                # finalfit returns a dataframe with columns like "label", "levels", "all", "HR (univariable)", etc.
                # The first row often contains the reference category (shown as dots)
                # We need to handle this structure carefully
                
                # Look for the HR column
                hr_col <- grep("HR", names(result), value = TRUE)
                
                if (length(hr_col) > 0) {
                    row_counter <- 0
                    variable_name <- NULL
                    
                    # Process each row of results
                    for (i in 1:nrow(result)) {
                        label <- if ("label" %in% names(result)) result$label[i] else result[[1]][i]
                        levels <- if ("levels" %in% names(result)) result$levels[i] else result[[2]][i]
                        hr_text <- result[[hr_col[1]]][i]
                        
                        # Capture the variable name from the first non-dot label
                        if (!is.na(label) && label != "." && label != "" && is.null(variable_name)) {
                            variable_name <- label
                        }
                        
                        # Skip reference categories (usually marked with "-" in HR column)
                        # But process rows with actual HR values
                        if (!is.na(hr_text) && hr_text != "" && hr_text != "-") {
                            parsed <- private$.parseHRText(hr_text)
                            
                            if (!is.null(parsed) && !is.na(parsed$hr)) {
                                # Determine the term name
                                if (!is.na(label) && label != "." && label != "") {
                                    # This row has a proper label
                                    if (!is.na(levels) && levels != "" && levels != "." && levels != label) {
                                        term_name <- paste0(label, ": ", levels)
                                    } else {
                                        term_name <- label
                                    }
                                } else if (!is.na(levels) && levels != "" && levels != ".") {
                                    # No label but has levels - use the variable name we found
                                    if (!is.null(variable_name)) {
                                        term_name <- paste0(variable_name, ": ", levels)
                                    } else {
                                        term_name <- levels
                                    }
                                } else {
                                    # Can't determine a proper name, skip
                                    next
                                }
                                
                                # Add the row to the table
                                row_counter <- row_counter + 1
                                table$addRow(rowKey = row_counter, values = list(
                                    term = term_name,
                                    hr = parsed$hr,
                                    ci_lower = parsed$ci_lower,
                                    ci_upper = parsed$ci_upper,
                                    p_value = parsed$p_value
                                ))
                            }
                        } else if (!is.na(hr_text) && hr_text == "-") {
                            # This is a reference category
                            # Determine the reference category name for display
                            if (!is.na(label) && label != "." && label != "") {
                                ref_name <- label
                                if (!is.na(levels) && levels != "" && levels != ".") {
                                    ref_name <- paste0(label, ": ", levels, " (Reference)")
                                }
                            } else if (!is.na(levels) && levels != "" && levels != ".") {
                                if (!is.null(variable_name)) {
                                    ref_name <- paste0(variable_name, ": ", levels, " (Reference)")
                                } else {
                                    ref_name <- paste0(levels, " (Reference)")
                                }
                            } else {
                                ref_name <- NULL
                            }
                            
                            # Add reference category row with HR = 1.00
                            if (!is.null(ref_name)) {
                                row_counter <- row_counter + 1
                                table$addRow(rowKey = row_counter, values = list(
                                    term = ref_name,
                                    hr = 1.00,
                                    ci_lower = NA,
                                    ci_upper = NA,
                                    p_value = NA
                                ))
                            }
                        }
                    }
                    
                    # If no valid rows were added, show descriptive message
                    if (row_counter == 0) {
                        # Check if this is because there's no variation in the data
                        if (nrow(result) > 0) {
                            table$addRow(rowKey = 1, values = list(
                                term = "No hazard ratios computed (possible: no events or no variation)",
                                hr = NA,
                                ci_lower = NA,
                                ci_upper = NA,
                                p_value = NA
                            ))
                        } else {
                            table$addRow(rowKey = 1, values = list(
                                term = "No results available",
                                hr = NA,
                                ci_lower = NA,
                                ci_upper = NA,
                                p_value = NA
                            ))
                        }
                    }
                } else {
                    # If no HR column found, check if we have descriptive survival data
                    if ("median" %in% names(result) && nrow(result) > 0) {
                        # No explanatory variable - show median survival
                        levels_val <- if ("levels" %in% names(result)) result$levels[1] else "All patients"
                        n_val <- if ("n" %in% names(result)) result$n[1] else NA
                        events_val <- if ("events" %in% names(result)) result$events[1] else NA
                        median_val <- result$median[1]

                        term_text <- paste0(levels_val, " (n=", n_val, ", events=", events_val, ")")

                        table$addRow(rowKey = 1, values = list(
                            term = term_text,
                            hr = median_val,  # Use HR column to show median survival
                            ci_lower = NA,
                            ci_upper = NA,
                            p_value = NA
                        ))

                        # Update summary to clarify we're showing median, not HR
                        # Set a flag so we don't overwrite this summary later
                        summary_text <- glue::glue(
                            "<h3>{analysis_type} Analysis Results</h3>
                            <p>Analysis completed for all patients (no group comparison).</p>
                            <p>Median survival time shown in months.</p>"
                        )
                        self$results$summary$setContent(summary_text)
                        return()  # Exit early to avoid overwriting summary
                    } else {
                        # No HR and no descriptive data - show message
                        table$addRow(rowKey = 1, values = list(
                            term = "Analysis did not produce hazard ratios",
                            hr = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            p_value = NA
                        ))
                    }
                }
            }
            
            # Generate summary
            summary_text <- glue::glue(
                "<h3>{analysis_type} Analysis Results</h3>
                <p>Analysis completed using finalfit package with Cox proportional hazards regression.</p>
                <p>Results show hazard ratios (HR) with 95% confidence intervals and p-values.</p>"
            )
            
            self$results$summary$setContent(summary_text)
            
            # Generate interpretation
            private$.generateInterpretation(analysis_type)
        },
        
        .formatCompetingRisksResults = function(result_crr, cuminc_result) {
            # Format competing risks regression results
            if (is.data.frame(result_crr) && nrow(result_crr) > 0) {
                hr_col <- grep("HR", names(result_crr), value = TRUE)[1]
                if (!is.null(hr_col) && !is.na(hr_col)) {
                    hr_text <- result_crr[[hr_col]][1]
                    parsed <- private$.parseHRText(hr_text)
                    
                    table <- self$results$survivalTable
                    table$addRow(rowKey = 1, values = list(
                        term = result_crr[[1]][1],
                        hr = parsed$hr,
                        ci_lower = parsed$ci_lower,
                        ci_upper = parsed$ci_upper,
                        p_value = parsed$p_value
                    ))
                }
            }

            # Format cumulative incidence results (Note: this function appears to be unused)
            if (!is.null(cuminc_result)) {
                # Would need timepoints from caller if this code is ever activated
                private$.formatCumulativeIncidence(cuminc_result, NULL)
            }

            # Generate summary for competing risks
            summary_text <- glue::glue(
                "<h3>Competing Risks Analysis Results</h3>
                <p>Analysis using Fine-Gray subdistribution hazard model (cmprsk package).</p>
                <p>Cumulative incidence functions calculated for disease-specific and competing events.</p>
                <p>Results account for the competing nature of different death causes.</p>"
            )
            
            self$results$summary$setContent(summary_text)
            private$.generateInterpretation("Competing Risks")
        },
        
        .formatCumulativeIncidence = function(cuminc_result, timepoints = NULL) {
            # Extract cumulative incidence estimates at user-specified timepoints
            if (!is.null(cuminc_result) && length(cuminc_result) > 0) {
                table <- self$results$cuminc

                # Get time points and estimates for first group
                first_group <- names(cuminc_result)[1]
                times <- cuminc_result[[first_group]]$time
                est1 <- cuminc_result[[first_group]]$est
                var1 <- cuminc_result[[first_group]]$var

                # Find estimates for competing risk if available
                competing_group <- names(cuminc_result)[length(cuminc_result)]
                est2 <- if(length(cuminc_result) > 1) cuminc_result[[competing_group]]$est else rep(NA, length(times))
                var2 <- if(length(cuminc_result) > 1) cuminc_result[[competing_group]]$var else rep(NA, length(times))

                # Use user-specified timepoints or default to 1, 2, 3, 5 years
                key_times <- if (!is.null(timepoints) && length(timepoints) > 0) {
                    timepoints
                } else {
                    c(12, 24, 36, 60)  # default: months
                }
                for (i in seq_along(key_times)) {
                    target_time <- key_times[i]
                    closest_idx <- which.min(abs(times - target_time))
                    
                    if (length(closest_idx) > 0) {
                        table$addRow(rowKey = i, values = list(
                            time = target_time,
                            est_1 = round(est1[closest_idx], 3),
                            est_2 = round(est2[closest_idx], 3),
                            var_1 = round(var1[closest_idx], 6),
                            var_2 = round(var2[closest_idx], 6)
                        ))
                    }
                }
            }
        },
        
        .parseHRText = function(hr_text) {
            # Handle various finalfit output formats
            # Common formats:
            # "1.23 (0.89-1.67, p=0.045)"
            # "1.23 (0.89, 1.67, p=0.045)"
            # "1.23 (0.89 to 1.67, p=0.045)"
            # "1.23 (0.89-1.67)"
            
            # Extract HR (the number before parentheses)
            hr_match <- regmatches(hr_text, regexpr("^[0-9.]+", hr_text))
            hr <- if(length(hr_match) > 0) as.numeric(hr_match) else NA
            
            # Extract content within parentheses
            ci_match <- regmatches(hr_text, regexpr("\\(.*\\)", hr_text))
            
            if (length(ci_match) > 0) {
                ci_content <- gsub("[\\(\\)]", "", ci_match)
                
                # Try to extract CI and p-value
                # First try splitting by comma
                parts <- strsplit(ci_content, ",")[[1]]
                
                if (length(parts) >= 1) {
                    # Parse CI range - could be separated by "-", " to ", or comma
                    ci_part <- trimws(parts[1])
                    
                    # Try different separators
                    if (grepl("-", ci_part)) {
                        ci_range <- strsplit(ci_part, "-")[[1]]
                    } else if (grepl(" to ", ci_part)) {
                        ci_range <- strsplit(ci_part, " to ")[[1]]
                    } else if (length(parts) >= 2 && !grepl("p=|p<|p>", parts[2])) {
                        # CI might be comma-separated
                        ci_range <- c(ci_part, trimws(parts[2]))
                    } else {
                        ci_range <- c(NA, NA)
                    }
                    
                    ci_lower <- suppressWarnings(as.numeric(trimws(ci_range[1])))
                    ci_upper <- suppressWarnings(as.numeric(trimws(ci_range[2])))
                    
                    # Extract p-value if present
                    p_value <- NA
                    for (part in parts) {
                        if (grepl("p[=<>]", part)) {
                            p_text <- trimws(part)
                            # Extract numeric value after p=, p<, or p>
                            p_value <- suppressWarnings(as.numeric(gsub("p[=<>]", "", p_text)))
                            break
                        }
                    }
                    
                    # Return parsed values
                    if (!is.na(hr)) {
                        return(list(
                            hr = round(hr, 3),
                            ci_lower = if(!is.na(ci_lower)) round(ci_lower, 3) else NA,
                            ci_upper = if(!is.na(ci_upper)) round(ci_upper, 3) else NA,
                            p_value = if(!is.na(p_value)) round(p_value, 4) else NA
                        ))
                    }
                }
            }
            
            # Return NA values if parsing failed
            return(list(hr = NA, ci_lower = NA, ci_upper = NA, p_value = NA))
        },
        
        .generateInterpretation = function(analysis_type) {
            interpretation <- switch(analysis_type,
                "Overall Survival" = "<p><b>Clinical Interpretation:</b><br>Overall survival considers all deaths as events, regardless of cause. This provides the most comprehensive view of mortality but may not distinguish disease-specific effects.</p>",
                "Cause-Specific Survival" = "<p><b>Clinical Interpretation:</b><br>Cause-specific survival focuses only on disease-related deaths. Other deaths are treated as censored observations. This approach may overestimate survival if competing risks are substantial.</p>",
                "Competing Risks" = "<p><b>Clinical Interpretation:</b><br>Competing risks analysis accounts for the fact that patients can die from multiple causes. The cumulative incidence function shows the probability of each event type over time, accounting for competition between causes.</p>",
                "<p>Analysis completed successfully.</p>"
            )
            
            self$results$interpretation$setContent(interpretation)
        },
        
        .performRiskStratification = function(data, time_var, status_var, group_var) {
            # Risk stratification for competing events using quantile-based approach
            tryCatch({
                if (is.null(time_var) || is.null(status_var)) {
                    return(list(message = "Time and status variables required for risk stratification"))
                }
                
                # Calculate risk scores based on time to event and event type
                data$risk_score <- ifelse(data[[status_var]] == 1, 
                                        1 / (data[[time_var]] + 0.1),  # Higher risk for earlier disease death
                                        ifelse(data[[status_var]] == 2,
                                               0.5 / (data[[time_var]] + 0.1),  # Moderate risk for competing death
                                               0))  # No risk for censored
                
                # Create risk categories using quantiles
                risk_quantiles <- quantile(data$risk_score[data$risk_score > 0], 
                                         probs = c(0, 0.33, 0.67, 1), 
                                         na.rm = TRUE)
                
                data$risk_group <- cut(data$risk_score, 
                                     breaks = c(-Inf, risk_quantiles[2], risk_quantiles[3], Inf),
                                     labels = c("Low Risk", "Moderate Risk", "High Risk"),
                                     include.lowest = TRUE)
                
                # Calculate group statistics
                risk_summary <- data.frame(
                    Risk_Group = c("Low Risk", "Moderate Risk", "High Risk"),
                    Count = as.numeric(table(data$risk_group)),
                    Disease_Death_Rate = c(
                        mean(data$risk_score[data$risk_group == "Low Risk"], na.rm = TRUE),
                        mean(data$risk_score[data$risk_group == "Moderate Risk"], na.rm = TRUE),
                        mean(data$risk_score[data$risk_group == "High Risk"], na.rm = TRUE)
                    ),
                    stringsAsFactors = FALSE
                )
                
                return(list(
                    stratified_data = data,
                    summary = risk_summary,
                    method = "Quantile-based risk stratification for competing events"
                ))
                
            }, error = function(e) {
                return(list(error = paste("Risk stratification error:", e$message)))
            })
        },
        
        .performTimeDependentCIF = function(data, time_var, status_var, group_var) {
            # Time-dependent cumulative incidence function analysis
            tryCatch({
                if (is.null(time_var) || is.null(status_var)) {
                    return(list(message = "Time and status variables required for time-dependent CIF"))
                }
                
                # Define time points for analysis (monthly intervals up to median follow-up)
                max_time <- max(data[[time_var]], na.rm = TRUE)
                time_points <- seq(0, max_time, by = max(1, max_time / 20))
                
                # Calculate time-dependent cumulative incidence
                time_dependent_results <- list()
                
                for (t in time_points) {
                    # Subset data to those at risk at time t
                    at_risk <- data[data[[time_var]] >= t, ]
                    
                    if (nrow(at_risk) > 0) {
                        # Calculate events in the next time interval
                        next_t <- t + max(1, max_time / 20)
                        events_in_interval <- at_risk[at_risk[[time_var]] < next_t & at_risk[[time_var]] >= t, ]
                        
                        # Count different event types
                        disease_deaths <- sum(events_in_interval[[status_var]] == 1, na.rm = TRUE)
                        competing_deaths <- sum(events_in_interval[[status_var]] == 2, na.rm = TRUE)
                        total_at_risk <- nrow(at_risk)
                        
                        time_dependent_results[[as.character(t)]] <- list(
                            time = t,
                            at_risk = total_at_risk,
                            disease_deaths = disease_deaths,
                            competing_deaths = competing_deaths,
                            disease_rate = if(total_at_risk > 0) disease_deaths / total_at_risk else 0,
                            competing_rate = if(total_at_risk > 0) competing_deaths / total_at_risk else 0
                        )
                    }
                }
                
                # Convert to data frame for easy interpretation
                time_dep_df <- do.call(rbind, lapply(time_dependent_results, function(x) {
                    data.frame(
                        Time = x$time,
                        At_Risk = x$at_risk,
                        Disease_Deaths = x$disease_deaths,
                        Competing_Deaths = x$competing_deaths,
                        Disease_Rate = round(x$disease_rate, 4),
                        Competing_Rate = round(x$competing_rate, 4),
                        stringsAsFactors = FALSE
                    )
                }))
                
                return(list(
                    time_dependent_cif = time_dep_df,
                    time_points = time_points,
                    max_follow_up = max_time,
                    method = "Time-dependent cumulative incidence analysis with interval-based event rates"
                ))
                
            }, error = function(e) {
                return(list(error = paste("Time-dependent CIF error:", e$message)))
            })
        },
        
        .plotCompetingRisks = function(image, ggtheme, theme, ...) {
            if (!self$options$analysistype == "compete") return()

            plotData <- image$state
            if (is.null(plotData)) return()

            # Get color scheme option
            color_scheme <- self$options$cifColors %||% "default"

            # Create competing risks plot using ggplot2
            tryCatch({
                cuminc_data <- plotData$cuminc
                if (!is.null(cuminc_data)) {
                    # Convert cuminc result to plottable format
                    plot_df <- data.frame()

                    for (i in seq_along(cuminc_data)) {
                        group_name <- names(cuminc_data)[i]
                        times <- cuminc_data[[i]]$time
                        est <- cuminc_data[[i]]$est

                        temp_df <- data.frame(
                            time = times,
                            estimate = est,
                            group = group_name,
                            stringsAsFactors = FALSE
                        )
                        plot_df <- rbind(plot_df, temp_df)
                    }

                    # Select color palette based on option
                    color_values <- switch(color_scheme,
                        "colorblind" = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"),
                        "grayscale" = c("#000000", "#404040", "#808080", "#C0C0C0"),
                        c("#D55E00", "#0072B2", "#CC79A7", "#009E73")  # default
                    )

                    # Create the plot
                    plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = estimate, color = group)) +
                        ggplot2::geom_step(size = 1) +
                        ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                        ggplot2::scale_color_manual(values = color_values) +
                        ggplot2::labs(
                            title = "Cumulative Incidence Function",
                            x = "Time (months)",
                            y = "Cumulative Incidence",
                            color = "Event Type"
                        ) +
                        ggtheme

                    print(plot)
                }
            }, error = function(e) {
                # If plotting fails, create a simple message
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                                    label = "Competing risks plot\navailable after analysis",
                                    size = 6) +
                    ggplot2::theme_void()
                print(plot)
            })

            TRUE
        },

        .stackedPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$analysistype == "compete") return()
            if (!self$options$showStackedPlot) return()

            plotData <- image$state
            if (is.null(plotData)) return()

            # Get color scheme option
            color_scheme <- self$options$cifColors %||% "default"

            tryCatch({
                cuminc_data <- plotData$cuminc
                if (!is.null(cuminc_data) && length(cuminc_data) >= 2) {
                    # Extract CIF1 (disease death) and CIF2 (competing death)
                    # Assume first two groups are the two event types
                    group_names <- names(cuminc_data)[!names(cuminc_data) %in% c("Tests")]

                    if (length(group_names) >= 2) {
                        cif1_data <- cuminc_data[[group_names[1]]]
                        cif2_data <- cuminc_data[[group_names[2]]]

                        # Get times (use times from CIF1)
                        times <- cif1_data$time
                        cif1_est <- cif1_data$est

                        # Match CIF2 estimates to same time points
                        cif2_est <- approx(cif2_data$time, cif2_data$est, xout = times,
                                          method = "constant", rule = 2)$y

                        # Calculate survival probability: S(t) = 1 - CIF1(t) - CIF2(t)
                        survival_prob <- pmax(0, 1 - cif1_est - cif2_est)

                        # Create data frame for stacked area plot
                        plot_df <- data.frame(
                            time = rep(times, 3),
                            probability = c(cif1_est, cif2_est, survival_prob),
                            event_type = rep(c("CIF Disease Death", "CIF Competing Death", "Survival"),
                                           each = length(times)),
                            stringsAsFactors = FALSE
                        )

                        # Set factor levels for proper stacking order
                        plot_df$event_type <- factor(plot_df$event_type,
                                                     levels = c("Survival", "CIF Competing Death", "CIF Disease Death"))

                        # Select color palette
                        fill_values <- switch(color_scheme,
                            "colorblind" = c("#56B4E9", "#E69F00", "#D55E00"),
                            "grayscale" = c("#C0C0C0", "#808080", "#404040"),
                            c("#0072B2", "#CC79A7", "#D55E00")  # default
                        )

                        # Create stacked area plot
                        plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = probability, fill = event_type)) +
                            ggplot2::geom_area(alpha = 0.7) +
                            ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                            ggplot2::scale_fill_manual(values = fill_values) +
                            ggplot2::labs(
                                title = "Stacked Probability Plot: CIF1 + CIF2 + Survival",
                                subtitle = "Total probability sums to 100% at each time point",
                                x = "Time (months)",
                                y = "Cumulative Probability",
                                fill = "Event"
                            ) +
                            ggtheme

                        print(plot)
                        return(TRUE)
                    }
                }

                # If data not available, show message
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                                    label = "Stacked plot requires\ncompeting risks analysis",
                                    size = 6) +
                    ggplot2::theme_void()
                print(plot)

            }, error = function(e) {
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                                    label = paste("Error creating plot:\n", e$message),
                                    size = 5) +
                    ggplot2::theme_void()
                print(plot)
            })

            TRUE
        },

        .kmvscifPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$analysistype == "compete") return()
            if (!self$options$showKMvsCIF) return()

            plotData <- image$state
            if (is.null(plotData)) return()

            # Get color scheme option
            color_scheme <- self$options$cifColors %||% "default"

            tryCatch({
                # Get the stored data
                mydata <- plotData$data
                time_var <- plotData$time_var
                status_var <- plotData$status_var
                cuminc_data <- plotData$cuminc

                if (!is.null(mydata) && !is.null(time_var) && !is.null(status_var) && !is.null(cuminc_data)) {
                    # Calculate naive 1-KM estimate (treating competing risks as censored)
                    # For KM, we treat disease death (status=1) as event, all others as censored
                    time_vec <- mydata[[time_var]]
                    status_vec <- mydata[[status_var]]

                    # Create binary status: 1 = disease death, 0 = censored/competing
                    km_status <- ifelse(status_vec == 1, 1, 0)

                    # Fit Kaplan-Meier
                    km_fit <- survival::survfit(survival::Surv(time_vec, km_status) ~ 1)

                    # Calculate 1-KM (naive cumulative incidence)
                    km_times <- km_fit$time
                    km_surv <- km_fit$surv
                    naive_cif <- 1 - km_surv

                    # Get proper CIF for disease death (first event type)
                    group_names <- names(cuminc_data)[!names(cuminc_data) %in% c("Tests")]
                    if (length(group_names) >= 1) {
                        cif_data <- cuminc_data[[group_names[1]]]
                        cif_times <- cif_data$time
                        cif_est <- cif_data$est

                        # Create comparison data frame
                        # Combine time points
                        all_times <- sort(unique(c(km_times, cif_times)))

                        # Interpolate both curves to same time points
                        naive_interp <- approx(km_times, naive_cif, xout = all_times,
                                              method = "constant", rule = 2, f = 0)$y
                        cif_interp <- approx(cif_times, cif_est, xout = all_times,
                                            method = "constant", rule = 2, f = 0)$y

                        plot_df <- data.frame(
                            time = rep(all_times, 2),
                            estimate = c(naive_interp, cif_interp),
                            method = rep(c("Naive 1-KM (Biased)", "Proper CIF (Unbiased)"),
                                        each = length(all_times)),
                            stringsAsFactors = FALSE
                        )

                        # Select color palette
                        color_values <- switch(color_scheme,
                            "colorblind" = c("#E69F00", "#009E73"),
                            "grayscale" = c("#808080", "#000000"),
                            c("#CC79A7", "#D55E00")  # default
                        )

                        # Create comparison plot
                        plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = estimate, color = method, linetype = method)) +
                            ggplot2::geom_step(size = 1) +
                            ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                            ggplot2::scale_color_manual(values = color_values) +
                            ggplot2::scale_linetype_manual(values = c("dashed", "solid")) +
                            ggplot2::labs(
                                title = "Comparison: Naive 1-KM vs. Proper CIF",
                                subtitle = "Naive 1-KM overestimates cumulative incidence when competing risks are present",
                                x = "Time (months)",
                                y = "Cumulative Incidence of Disease Death",
                                color = "Method",
                                linetype = "Method"
                            ) +
                            ggtheme +
                            ggplot2::theme(legend.position = "bottom")

                        print(plot)
                        return(TRUE)
                    }
                }

                # If data not available, show message
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                                    label = "KM vs CIF comparison requires\ncompeting risks analysis",
                                    size = 6) +
                    ggplot2::theme_void()
                print(plot)

            }, error = function(e) {
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                                    label = paste("Error creating plot:\n", e$message),
                                    size = 5) +
                    ggplot2::theme_void()
                print(plot)
            })

            TRUE
        }
    )
)