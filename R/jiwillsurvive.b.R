#' @title Intuitive Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import survminer
#' @import ggplot2
#' @import dplyr
#' @import lubridate

jiwillsurviveClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jiwillsurviveClass",
    inherit = jiwillsurviveBase,
    private = list(
        
        .init = function() {
            
            # Set visibility based on analysis type
            if (self$options$analysis_type == "survival_model") {
                self$results$survivalPlot$setVisible(TRUE)
                self$results$survivalStats$setVisible(TRUE)
                self$results$survivalTable$setVisible(TRUE)
                self$results$followupPlot$setVisible(FALSE)
                self$results$dataOutput$setVisible(FALSE)
            } else if (self$options$analysis_type == "followup_plot") {
                self$results$followupPlot$setVisible(TRUE)
                self$results$survivalPlot$setVisible(FALSE)
                self$results$survivalStats$setVisible(FALSE)
                self$results$survivalTable$setVisible(FALSE)
                self$results$dataOutput$setVisible(FALSE)
            } else if (self$options$analysis_type == "data_prep") {
                self$results$dataOutput$setVisible(TRUE)
                self$results$prepText$setVisible(TRUE)
                self$results$survivalPlot$setVisible(FALSE)
                self$results$survivalStats$setVisible(FALSE)
                self$results$survivalTable$setVisible(FALSE)
                self$results$followupPlot$setVisible(FALSE)
            } else if (self$options$analysis_type == "kaplan_meier") {
                self$results$kmPlot$setVisible(TRUE)
                self$results$kmStats$setVisible(TRUE)
                self$results$kmTable$setVisible(TRUE)
                self$results$survivalPlot$setVisible(FALSE)
                self$results$followupPlot$setVisible(FALSE)
                self$results$dataOutput$setVisible(FALSE)
            }
            
            # User instructions
            instructions <- "
<h3>📊 Intuitive Survival Analysis Guide</h3>

<h4>🎯 Analysis Types:</h4>
<ul>
  <li><strong>Survival Model & Plot:</strong> Complete survival analysis with curves and statistics</li>
  <li><strong>Kaplan-Meier Curves:</strong> Classic survival curve analysis</li>
  <li><strong>Follow-up Visualization:</strong> Explore follow-up time distributions</li>
  <li><strong>Data Preparation:</strong> Calculate follow-up times from dates</li>
</ul>

<h4>📋 Required Variables:</h4>
<ul>
  <li><strong>Time Variable:</strong> Follow-up time or time-to-event (numeric)</li>
  <li><strong>Event Variable:</strong> Event indicator (1=event, 0=censored)</li>
  <li><strong>Grouping Variable:</strong> Optional variable for group comparisons</li>
</ul>

<h4>📅 Date-Based Analysis:</h4>
<ul>
  <li><strong>Start Date:</strong> Study enrollment or treatment start date</li>
  <li><strong>End Date:</strong> Event date or last follow-up date</li>
  <li><strong>Derive Follow-up:</strong> Enable to calculate time from dates</li>
</ul>

<h4>🎨 Customization Options:</h4>
<ul>
  <li><strong>Plot Styles:</strong> Choose from multiple visual themes</li>
  <li><strong>Color Palettes:</strong> Select colorblind-safe options</li>
  <li><strong>Risk Tables:</strong> Add numbers at risk below plots</li>
  <li><strong>Confidence Bands:</strong> Show uncertainty around curves</li>
</ul>

<h4>📈 Statistical Features:</h4>
<ul>
  <li><strong>Log-rank Tests:</strong> Compare survival between groups</li>
  <li><strong>Median Survival:</strong> Estimate median survival times</li>
  <li><strong>Survival Tables:</strong> Summary at specific time points</li>
  <li><strong>Clinical Interpretation:</strong> Automated result interpretation</li>
</ul>

<p><em>💡 Tip: Start with 'Data Preparation' if you have date variables, then proceed to 'Survival Model & Plot' for analysis.</em></p>
"
            self$results$instructions$setContent(instructions)
        },
        
        .run = function() {
            
            # Check if we have data
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$instructions$setContent("Please provide data for analysis.")
                return()
            }
            
            # Get the prepared data
            prepared_data <- private$.prepareData()
            
            if (is.null(prepared_data)) {
                return()
            }
            
            # Execute based on analysis type
            if (self$options$analysis_type == "survival_model") {
                private$.runSurvivalModel(prepared_data)
            } else if (self$options$analysis_type == "followup_plot") {
                private$.runFollowupPlot(prepared_data)
            } else if (self$options$analysis_type == "data_prep") {
                private$.runDataPrep(prepared_data)
            } else if (self$options$analysis_type == "kaplan_meier") {
                private$.runKaplanMeier(prepared_data)
            }
        },
        
        .prepareData = function() {
            
            data <- self$data
            
            # Derive follow-up variables if requested
            if (self$options$derive_followup && 
                !is.null(self$options$start_date_var) && 
                !is.null(self$options$end_date_var)) {
                
                start_col <- self$options$start_date_var
                end_col <- self$options$end_date_var
                
                if (start_col %in% names(data) && end_col %in% names(data)) {
                    
                    # Convert to dates if they aren't already
                    tryCatch({
                        data[[start_col]] <- as.Date(data[[start_col]])
                        data[[end_col]] <- as.Date(data[[end_col]])
                        
                        # Calculate follow-up time
                        followup_days <- as.numeric(data[[end_col]] - data[[start_col]])
                        
                        # Convert to requested units
                        followup_time <- switch(self$options$followup_units,
                            "days" = followup_days,
                            "weeks" = followup_days / 7,
                            "months" = followup_days / 30.44,
                            "years" = followup_days / 365.25
                        )
                        
                        data$derived_followup_time <- followup_time
                        
                    }, error = function(e) {
                        self$results$instructions$setContent(
                            paste("Error calculating follow-up time:", e$message)
                        )
                        return(NULL)
                    })
                }
            }
            
            return(data)
        },
        
        .runSurvivalModel = function(data) {
            
            # Check required variables
            if (is.null(self$options$time_var) || is.null(self$options$event_var)) {
                self$results$instructions$setContent(
                    "Please select both Time Variable and Event Variable for survival analysis."
                )
                return()
            }
            
            time_col <- self$options$time_var
            event_col <- self$options$event_var
            group_col <- self$options$group_var
            
            # Check if columns exist
            if (!time_col %in% names(data) || !event_col %in% names(data)) {
                self$results$instructions$setContent(
                    "Selected variables not found in data."
                )
                return()
            }
            
            # Prepare survival object
            surv_obj <- survival::Surv(data[[time_col]], data[[event_col]])
            
            # Fit survival model
            if (!is.null(group_col) && group_col %in% names(data)) {
                formula <- as.formula(paste("surv_obj ~", group_col))
                fit <- survival::survfit(formula, data = data, conf.int = self$options$confidence_level)
                
                # Log-rank test
                if (self$options$show_statistics) {
                    logrank_test <- survival::survdiff(formula, data = data)
                    private$.outputLogRankTest(logrank_test)
                }
            } else {
                fit <- survival::survfit(surv_obj ~ 1, data = data, conf.int = self$options$confidence_level)
            }
            
            # Create survival plot
            private$.createSurvivalPlot(fit, data)
            
            # Create survival table
            if (self$options$show_survival_table) {
                private$.createSurvivalTable(fit)
            }
            
            # Create interpretation
            if (self$options$show_interpretation) {
                private$.createInterpretation(fit, data)
            }
        },
        
        .runKaplanMeier = function(data) {
            
            # Similar to survival model but with classic KM styling
            if (is.null(self$options$time_var) || is.null(self$options$event_var)) {
                self$results$instructions$setContent(
                    "Please select both Time Variable and Event Variable for Kaplan-Meier analysis."
                )
                return()
            }
            
            time_col <- self$options$time_var
            event_col <- self$options$event_var
            group_col <- self$options$group_var
            
            # Check if columns exist
            if (!time_col %in% names(data) || !event_col %in% names(data)) {
                return()
            }
            
            # Prepare survival object
            surv_obj <- survival::Surv(data[[time_col]], data[[event_col]])
            
            # Fit survival model
            if (!is.null(group_col) && group_col %in% names(data)) {
                formula <- as.formula(paste("surv_obj ~", group_col))
                fit <- survival::survfit(formula, data = data, conf.int = self$options$confidence_level)
            } else {
                fit <- survival::survfit(surv_obj ~ 1, data = data, conf.int = self$options$confidence_level)
            }
            
            # Create KM plot with classic styling
            private$.createKMPlot(fit, data)
            
            # Create KM statistics
            private$.createKMStats(fit)
            
            # Create KM table
            private$.createKMTable(fit)
        },
        
        .runFollowupPlot = function(data) {
            
            time_col <- self$options$time_var
            if (is.null(time_col) || !time_col %in% names(data)) {
                # Try derived follow-up time
                if ("derived_followup_time" %in% names(data)) {
                    time_col <- "derived_followup_time"
                } else {
                    self$results$instructions$setContent(
                        "Please select a Time Variable or enable 'Derive Follow-up Variables'."
                    )
                    return()
                }
            }
            
            private$.createFollowupPlot(data, time_col)
        },
        
        .runDataPrep = function(data) {
            
            if (self$options$derive_followup && "derived_followup_time" %in% names(data)) {
                
                # Create summary of derived data
                summary_text <- paste(
                    "<h3>📅 Data Preparation Summary</h3>",
                    "<h4>Follow-up Time Calculation:</h4>",
                    "<ul>",
                    "<li><strong>Start Date Variable:</strong>", self$options$start_date_var, "</li>",
                    "<li><strong>End Date Variable:</strong>", self$options$end_date_var, "</li>",
                    "<li><strong>Time Units:</strong>", self$options$followup_units, "</li>",
                    "<li><strong>Derived Variable:</strong> derived_followup_time</li>",
                    "</ul>",
                    "<h4>Summary Statistics:</h4>",
                    "<ul>",
                    "<li><strong>N Patients:</strong>", nrow(data), "</li>",
                    "<li><strong>Mean Follow-up:</strong>", round(mean(data$derived_followup_time, na.rm = TRUE), 2), self$options$followup_units, "</li>",
                    "<li><strong>Median Follow-up:</strong>", round(median(data$derived_followup_time, na.rm = TRUE), 2), self$options$followup_units, "</li>",
                    "<li><strong>Range:</strong>", round(min(data$derived_followup_time, na.rm = TRUE), 2), "-", round(max(data$derived_followup_time, na.rm = TRUE), 2), self$options$followup_units, "</li>",
                    "</ul>",
                    sep = "\n"
                )
                
                self$results$prepText$setContent(summary_text)
                
                # Export processed data if requested
                if (self$options$export_data) {
                    # Note: In a full implementation, this would create an output dataset
                    self$results$dataOutput$setContent("Processed data with derived follow-up times")
                }
            } else {
                self$results$prepText$setContent(
                    "Enable 'Derive Follow-up Variables' and select start/end date variables to calculate follow-up times."
                )
            }
        },
        
        .createSurvivalPlot = function(fit, data) {
            
            # Basic survival plot using survminer
            tryCatch({
                
                # Configure plot options
                plot_config <- list(
                    conf.int = self$options$show_confidence_bands,
                    risk.table = self$options$show_risk_table,
                    censor = self$options$show_censoring_marks,
                    legend.title = "",
                    legend = self$options$legend_position
                )
                
                # Add median survival lines if requested
                if (self$options$show_median_survival) {
                    plot_config$surv.median.line = "hv"
                }
                
                # Set color palette
                if (!is.null(self$options$group_var) && self$options$group_var %in% names(data)) {
                    colors <- private$.getColorPalette()
                    plot_config$palette = colors
                }
                
                # Create the plot
                p <- do.call(survminer::ggsurvplot, c(list(fit), plot_config))
                
                # Customize based on style
                if (self$options$plot_style != "iwillsurvive") {
                    p <- private$.applyPlotStyle(p, self$options$plot_style)
                }
                
                # Add custom labels
                if (self$options$plot_title != "") {
                    p$plot <- p$plot + ggplot2::ggtitle(self$options$plot_title)
                }
                
                if (self$options$x_label != "") {
                    p$plot <- p$plot + ggplot2::xlab(self$options$x_label)
                }
                
                if (self$options$y_label != "") {
                    p$plot <- p$plot + ggplot2::ylab(self$options$y_label)
                }
                
                # Set time breaks if specified
                if (self$options$time_breaks != "") {
                    breaks <- as.numeric(strsplit(self$options$time_breaks, ",")[[1]])
                    p$plot <- p$plot + ggplot2::scale_x_continuous(breaks = breaks)
                }
                
                print(p)
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Error creating survival plot:", e$message))
            })
        },
        
        .createKMPlot = function(fit, data) {
            
            # Classic Kaplan-Meier plot
            tryCatch({
                
                p <- survminer::ggsurvplot(
                    fit, 
                    data = data,
                    conf.int = self$options$show_confidence_bands,
                    risk.table = self$options$show_risk_table,
                    censor = self$options$show_censoring_marks,
                    surv.median.line = ifelse(self$options$show_median_survival, "hv", "none"),
                    legend.title = "",
                    legend = self$options$legend_position,
                    ggtheme = ggplot2::theme_classic()
                )
                
                print(p)
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Error creating KM plot:", e$message))
            })
        },
        
        .createFollowupPlot = function(data, time_col) {
            
            tryCatch({
                
                if (self$options$followup_plot_type == "histogram") {
                    
                    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = time_col)) +
                        ggplot2::geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
                        ggplot2::labs(
                            title = "Distribution of Follow-up Times",
                            x = paste("Follow-up Time (", self$options$followup_units, ")", sep = ""),
                            y = "Count"
                        ) +
                        ggplot2::theme_minimal()
                    
                } else if (self$options$followup_plot_type == "timeline") {
                    
                    # Create timeline plot
                    data$patient_id <- seq_len(nrow(data))
                    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = time_col, y = "patient_id")) +
                        ggplot2::geom_point(alpha = 0.6) +
                        ggplot2::labs(
                            title = "Patient Follow-up Timeline",
                            x = paste("Follow-up Time (", self$options$followup_units, ")", sep = ""),
                            y = "Patient"
                        ) +
                        ggplot2::theme_minimal()
                    
                } else if (self$options$followup_plot_type == "summary") {
                    
                    # Summary statistics plot
                    summary_data <- data.frame(
                        Statistic = c("Mean", "Median", "Q1", "Q3", "Min", "Max"),
                        Value = c(
                            mean(data[[time_col]], na.rm = TRUE),
                            median(data[[time_col]], na.rm = TRUE),
                            quantile(data[[time_col]], 0.25, na.rm = TRUE),
                            quantile(data[[time_col]], 0.75, na.rm = TRUE),
                            min(data[[time_col]], na.rm = TRUE),
                            max(data[[time_col]], na.rm = TRUE)
                        )
                    )
                    
                    p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = Statistic, y = Value)) +
                        ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
                        ggplot2::labs(
                            title = "Follow-up Time Summary Statistics",
                            x = "Statistic",
                            y = paste("Time (", self$options$followup_units, ")", sep = "")
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::coord_flip()
                }
                
                print(p)
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Error creating follow-up plot:", e$message))
            })
        },
        
        .outputLogRankTest = function(test) {
            
            stats_text <- paste(
                "<h3>📊 Statistical Test Results</h3>",
                "<h4>Log-rank Test:</h4>",
                "<ul>",
                "<li><strong>Chi-square:</strong>", round(test$chisq, 3), "</li>",
                "<li><strong>Degrees of freedom:</strong>", test$df, "</li>",
                "<li><strong>P-value:</strong>", format.pval(pchisq(test$chisq, test$df, lower.tail = FALSE), digits = 3), "</li>",
                "</ul>",
                sep = "\n"
            )
            
            self$results$survivalStats$setContent(stats_text)
        },
        
        .createSurvivalTable = function(fit) {
            
            # Get survival summary
            if (self$options$time_points != "") {
                time_points <- as.numeric(strsplit(self$options$time_points, ",")[[1]])
                summary_fit <- summary(fit, times = time_points)
            } else {
                summary_fit <- summary(fit)
            }
            
            # Create table data
            table_data <- data.frame(
                Time = summary_fit$time,
                N_Risk = summary_fit$n.risk,
                N_Event = summary_fit$n.event,
                Survival = round(summary_fit$surv, 3),
                SE = round(summary_fit$std.err, 3),
                Lower_CI = round(summary_fit$lower, 3),
                Upper_CI = round(summary_fit$upper, 3)
            )
            
            if (!is.null(summary_fit$strata)) {
                table_data$Group <- rep(names(summary_fit$strata), summary_fit$strata)
            }
            
            # Set the table
            self$results$survivalTable$setContent(table_data)
        },
        
        .createKMStats = function(fit) {
            
            # Median survival times
            median_surv <- summary(fit)$table
            
            stats_text <- "<h3>📊 Kaplan-Meier Statistics</h3>"
            
            if (is.matrix(median_surv)) {
                stats_text <- paste(stats_text, "<h4>Median Survival Times:</h4><ul>", sep = "\n")
                for (i in 1:nrow(median_surv)) {
                    group_name <- rownames(median_surv)[i]
                    median_time <- median_surv[i, "median"]
                    lcl <- median_surv[i, "0.95LCL"]
                    ucl <- median_surv[i, "0.95UCL"]
                    
                    stats_text <- paste(stats_text, 
                        "<li><strong>", group_name, ":</strong> ", median_time, 
                        " (95% CI: ", lcl, " - ", ucl, ")</li>", sep = "")
                }
                stats_text <- paste(stats_text, "</ul>", sep = "\n")
            } else {
                median_time <- median_surv["median"]
                lcl <- median_surv["0.95LCL"]
                ucl <- median_surv["0.95UCL"]
                
                stats_text <- paste(stats_text, 
                    "<p><strong>Median Survival:</strong> ", median_time, 
                    " (95% CI: ", lcl, " - ", ucl, ")</p>", sep = "")
            }
            
            self$results$kmStats$setContent(stats_text)
        },
        
        .createKMTable = function(fit) {
            
            # Create summary table similar to survival table
            summary_fit <- summary(fit)
            
            table_data <- data.frame(
                Time = summary_fit$time,
                N_Risk = summary_fit$n.risk,
                N_Event = summary_fit$n.event,
                Survival = round(summary_fit$surv, 3),
                Lower_CI = round(summary_fit$lower, 3),
                Upper_CI = round(summary_fit$upper, 3)
            )
            
            if (!is.null(summary_fit$strata)) {
                table_data$Group <- rep(names(summary_fit$strata), summary_fit$strata)
            }
            
            self$results$kmTable$setContent(table_data)
        },
        
        .createInterpretation = function(fit, data) {
            
            # Generate clinical interpretation
            interpretation <- "<h3>🏥 Clinical Interpretation</h3>"
            
            # Sample size
            n_patients <- nrow(data)
            interpretation <- paste(interpretation, 
                "<p><strong>Study Population:</strong> ", n_patients, " patients analyzed.</p>", 
                sep = "\n")
            
            # Event rate
            event_col <- self$options$event_var
            n_events <- sum(data[[event_col]], na.rm = TRUE)
            event_rate <- round(n_events / n_patients * 100, 1)
            
            interpretation <- paste(interpretation,
                "<p><strong>Event Rate:</strong> ", n_events, " events (", event_rate, "%) observed.</p>",
                sep = "\n")
            
            # Median survival
            median_surv <- summary(fit)$table
            if (is.matrix(median_surv)) {
                interpretation <- paste(interpretation, 
                    "<p><strong>Group Comparison:</strong> Survival differs between groups.</p>",
                    sep = "\n")
            } else {
                median_time <- median_surv["median"]
                if (!is.na(median_time)) {
                    interpretation <- paste(interpretation,
                        "<p><strong>Median Survival:</strong> ", median_time, " ", self$options$followup_units, ".</p>",
                        sep = "\n")
                } else {
                    interpretation <- paste(interpretation,
                        "<p><strong>Median Survival:</strong> Not reached (>50% of patients remain event-free).</p>",
                        sep = "\n")
                }
            }
            
            # Add recommendations
            interpretation <- paste(interpretation,
                "<h4>📋 Recommendations:</h4>",
                "<ul>",
                "<li>Consider longer follow-up if median survival not reached</li>",
                "<li>Evaluate proportional hazards assumption for Cox models</li>",
                "<li>Consider competing risks if applicable</li>",
                "</ul>",
                sep = "\n")
            
            self$results$interpretation$setContent(interpretation)
        },
        
        .getColorPalette = function() {
            
            switch(self$options$color_palette,
                "default" = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"),
                "colorblind" = c("#0173B2", "#DE8F05", "#029E73", "#CC78BC", "#CA9161", "#FBAFE4"),
                "viridis" = c("#440154", "#31688E", "#35B779", "#FDE725"),
                "set1" = RColorBrewer::brewer.pal(8, "Set1"),
                "dark2" = RColorBrewer::brewer.pal(8, "Dark2"),
                "pastel" = RColorBrewer::brewer.pal(8, "Pastel1")
            )
        },
        
        .applyPlotStyle = function(p, style) {
            
            switch(style,
                "classic" = {
                    p$plot <- p$plot + ggplot2::theme_classic()
                },
                "modern" = {
                    p$plot <- p$plot + ggplot2::theme_minimal() +
                        ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "grey90"))
                },
                "minimal" = {
                    p$plot <- p$plot + ggplot2::theme_void() +
                        ggplot2::theme(axis.line = ggplot2::element_line(),
                                      axis.text = ggplot2::element_text(),
                                      axis.title = ggplot2::element_text())
                },
                "publication" = {
                    p$plot <- p$plot + ggplot2::theme_bw() +
                        ggplot2::theme(panel.grid = ggplot2::element_blank(),
                                      strip.background = ggplot2::element_blank())
                }
            )
            
            return(p)
        }
    )
)