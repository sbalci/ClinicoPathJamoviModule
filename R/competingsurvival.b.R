#' @title Competing Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

competingsurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "competingsurvivalClass",
    inherit = competingsurvivalBase,
    private = list(
        
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
            
            # Clean data and handle missing values
            mydata <- mydata %>%
                dplyr::filter(!is.na(.data[[mytime]]), !is.na(.data[[myoutcome]]), !is.na(.data[[myexplanatory]]))
            
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
            mydata$status_os <- ifelse(
                mydata[[myoutcome]] %in% c(dod, dooc), 1, 0
            )
            
            # Create survival object
            dependent_os <- paste0("Surv(", mytime, ", status_os)")
            
            # Perform survival analysis
            tryCatch({
                result <- mydata %>%
                    finalfit::finalfit(dependent_os, myexplanatory, add_dependent_label = FALSE)
                
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
                result <- mydata %>%
                    finalfit::finalfit(dependent_dss, myexplanatory, add_dependent_label = FALSE)
                
                # Extract and format results
                private$.formatSurvivalResults(result, "Cause-Specific Survival")
                
            }, error = function(e) {
                stop(paste("Cause-specific survival analysis failed:", e$message))
            })
        },
 
        .competingRisksSurvival = function(mydata, mytime, myoutcome, myexplanatory, dod, dooc, awd, awod) {
            # Create competing risks outcome: alive=0, disease death=1, other death=2
            mydata$status_crr <- dplyr::case_when(
                mydata[[myoutcome]] %in% c(awd, awod) ~ 0,  # alive/censored
                mydata[[myoutcome]] == dod ~ 1,            # disease death
                mydata[[myoutcome]] == dooc ~ 2,           # other death
                TRUE ~ 0
            )
            
            # Create survival object for competing risks
            dependent_crr <- paste0("Surv(", mytime, ", status_crr)")
            
            tryCatch({
                # Competing risks regression using finalfit
                result_crr <- mydata %>%
                    finalfit::crrmulti(dependent_crr, myexplanatory) %>%
                    finalfit::fit2df(estimate_suffix = " (Competing Risks)")
                
                # Cumulative incidence analysis
                time_var <- mydata[[mytime]]
                status_var <- mydata$status_crr
                group_var <- mydata[[myexplanatory]]
                
                # Calculate cumulative incidence function
                cuminc_result <- cmprsk::cuminc(
                    ftime = time_var,
                    fstatus = status_var,
                    group = group_var
                )
                
                # Format results
                private$.formatCompetingRisksResults(result_crr, cuminc_result)
                
                # Store data for plotting
                self$results$comprisksPlot$setState(list(
                    "data" = mydata,
                    "time_var" = mytime,
                    "status_var" = "status_crr",
                    "group_var" = myexplanatory,
                    "cuminc" = cuminc_result
                ))
                
            }, error = function(e) {
                stop(paste("Competing risks analysis failed:", e$message))
            })
        },
 
        .formatSurvivalResults = function(result, analysis_type) {
            # Extract coefficients from finalfit result
            if (is.data.frame(result) && nrow(result) > 0) {
                # Parse HR and CI from finalfit output
                hr_col <- grep("HR", names(result), value = TRUE)[1]
                if (!is.null(hr_col) && !is.na(hr_col)) {
                    hr_text <- result[[hr_col]][1]
                    
                    # Extract values from "HR (CI, p)" format
                    if (!is.na(hr_text) && grepl("\\(", hr_text)) {
                        parsed <- private$.parseHRText(hr_text)
                        
                        table <- self$results$survivalTable
                        table$addRow(rowKey = 1, values = list(
                            term = result[[1]][1],
                            hr = parsed$hr,
                            ci_lower = parsed$ci_lower,
                            ci_upper = parsed$ci_upper,
                            p_value = parsed$p_value
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
            
            # Format cumulative incidence results
            if (!is.null(cuminc_result)) {
                private$.formatCumulativeIncidence(cuminc_result)
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
        
        .formatCumulativeIncidence = function(cuminc_result) {
            # Extract cumulative incidence estimates at key timepoints
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
                
                # Add key time points (1, 2, 3, 5 years)
                key_times <- c(12, 24, 36, 60)  # months
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
            # Parse "1.23 (0.89-1.67, p=0.045)" format
            hr <- as.numeric(gsub("\\s*\\(.*", "", hr_text))
            ci_match <- regmatches(hr_text, regexpr("\\(.*?\\)", hr_text))
            
            if (length(ci_match) > 0) {
                ci_content <- gsub("[\\(\\)]", "", ci_match)
                parts <- strsplit(ci_content, ",")[[1]]
                
                if (length(parts) >= 2) {
                    ci_range <- strsplit(trimws(parts[1]), "-")[[1]]
                    ci_lower <- as.numeric(ci_range[1])
                    ci_upper <- as.numeric(ci_range[2])
                    
                    p_text <- trimws(parts[2])
                    p_value <- as.numeric(gsub("p=", "", p_text))
                    
                    return(list(hr = hr, ci_lower = ci_lower, ci_upper = ci_upper, p_value = p_value))
                }
            }
            
            return(list(hr = hr, ci_lower = NA, ci_upper = NA, p_value = NA))
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
        
        .plotCompetingRisks = function(image, ggtheme, theme, ...) {
            if (!self$options$analysistype == "compete") return()
            
            plotData <- image$state
            if (is.null(plotData)) return()
            
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
                    
                    # Create the plot
                    plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = estimate, color = group)) +
                        ggplot2::geom_step(size = 1) +
                        ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
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
        }
    )
)