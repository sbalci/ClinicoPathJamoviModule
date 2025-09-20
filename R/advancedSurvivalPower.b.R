#' @title Power Analysis for Survival Studies
#' 
#' @description
#' This module provides comprehensive power analysis for survival studies,
#' allowing researchers to calculate statistical power, required sample size,
#' or minimum detectable hazard ratio for survival analysis designs.
#' 
#' @details
#' The powersurvival function implements power calculations based on the
#' Freedman method for survival studies with Cox proportional hazards models.
#' It supports both simple designs (fixed follow-up) and complex designs
#' with accrual periods, variable follow-up times, and loss to follow-up.
#' 
#' Key features:
#' \itemize{
#'   \item Three calculation types: power, sample size, or hazard ratio
#'   \item Simple and complex study designs
#'   \item Interactive plots showing parameter relationships
#'   \item Comprehensive educational output with interpretation
#'   \item Input validation and warning system
#' }
#' 
#' For complex designs, the function accounts for:
#' \itemize{
#'   \item Accrual period and variable follow-up times
#'   \item Loss to follow-up as competing risk
#'   \item Baseline hazard estimation from median survival
#'   \item Average follow-up time calculations
#' }
#' 
#' @references
#' Freedman, L.S. (1982). Tables of the number of patients required in clinical
#' trials using the log-rank test. Statistics in Medicine, 1, 121-129.
#' 
#' Rosner, B. (2006). Fundamentals of Biostatistics (6th edition). 
#' Thomson Brooks/Cole.
#' 
#' @author Serdar Balci \email{serdarbalci@serdarbalci.com}
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom powerSurvEpi powerCT.default
#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_vline labs theme_bw ylim scale_x_reverse
#'

advancedSurvivalPowerClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "advancedSurvivalPowerClass",
    inherit = advancedSurvivalPowerBase,
    private = list(
        # Initialize function to set up the analysis
        .init = function() {
            if (self$options$calc_type == "power") {
                self$results$sample_size_result$setVisible(FALSE)
                self$results$power_result$setVisible(TRUE)
                self$results$hazard_ratio_result$setVisible(FALSE)
            } else if (self$options$calc_type == "sample_size") {
                self$results$sample_size_result$setVisible(TRUE)
                self$results$power_result$setVisible(FALSE)
                self$results$hazard_ratio_result$setVisible(FALSE)
            } else if (self$options$calc_type == "hazard_ratio") {
                self$results$sample_size_result$setVisible(FALSE)
                self$results$power_result$setVisible(FALSE)
                self$results$hazard_ratio_result$setVisible(TRUE)
            }
        },

        # Main analysis function
        .run = function() {
            # Check if required packages are installed
            if (!requireNamespace("powerSurvEpi", quietly = TRUE)) {
                message <- "
                    <br>This analysis requires the 'powerSurvEpi' package.
                    <br>Please install it by running the following command in R:
                    <br><code>install.packages('powerSurvEpi')</code>
                    <br>Alternatively, ask your system administrator to install the package.
                "
                html <- self$results$message
                html$setContent(message)
                return()
            }

            # Hide message if packages are available
            self$results$message$setVisible(FALSE)

            # Get analysis parameters
            alpha <- self$options$alpha
            calc_type <- self$options$calc_type
            study_design <- self$options$study_design
            hr <- self$options$hazard_ratio
            power <- self$options$power
            n <- self$options$sample_size
            k <- self$options$allocation_ratio
            prob_event <- self$options$prob_event

            # Additional parameters for more complex designs
            accrual_time <- self$options$accrual_time
            follow_up_time <- self$options$follow_up_time
            median_survival <- self$options$median_survival
            loss_to_followup <- self$options$loss_followup

            # Input validation
            validation_errors <- character(0)
            
            # Check for logical consistency
            if (study_design == "complex") {
                if (accrual_time <= 0) {
                    validation_errors <- c(validation_errors, "Accrual time must be positive for complex designs")
                }
                if (follow_up_time < 0) {
                    validation_errors <- c(validation_errors, "Follow-up time cannot be negative")
                }
                if (median_survival <= 0) {
                    validation_errors <- c(validation_errors, "Median survival must be positive")
                }
                if (loss_to_followup < 0 || loss_to_followup >= 1) {
                    validation_errors <- c(validation_errors, "Loss to follow-up rate must be between 0 and 1")
                }
                # Check that total study time is reasonable
                total_time <- accrual_time + follow_up_time
                if (total_time > median_survival * 5) {
                    validation_errors <- c(validation_errors, 
                        "Warning: Total study time is very long compared to median survival. Consider shorter study duration or check parameters.")
                }
            }
            
            # Check for extreme parameter combinations
            if (prob_event <= 0.01 && n < 1000) {
                validation_errors <- c(validation_errors, 
                    "Warning: Very low event probability with small sample size may lead to unreliable estimates.")
            }
            
            if (calc_type == "sample_size" && power >= 0.99) {
                validation_errors <- c(validation_errors, 
                    "Warning: Very high power requirements may result in impractically large sample sizes.")
            }
            
            # Display validation errors/warnings if any
            if (length(validation_errors) > 0) {
                error_html <- paste(
                    "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; border-radius: 5px; margin-bottom: 15px;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>Parameter Validation</h4>",
                    paste("<p style='color: #856404; margin: 5px 0;'>", validation_errors, "</p>", collapse = ""),
                    "</div>",
                    collapse = ""
                )
                self$results$message$setContent(error_html)
                self$results$message$setVisible(TRUE)
                
                # Return early if there are critical errors (not just warnings)
                critical_errors <- validation_errors[!grepl("^Warning:", validation_errors)]
                if (length(critical_errors) > 0) {
                    return()
                }
            }

            # Initialize results
            result <- NULL
            plot_data <- NULL

            # Calculate group sizes based on allocation ratio
            nE <- n / (1 + k)  # Size of experimental group
            nC <- (n * k) / (1 + k)  # Size of control group

            # Perform appropriate power analysis based on calculation type
            if (calc_type == "power") {
                # Calculate power based on specified params
                if (study_design == "simple") {
                    # For basic design, we'll assume equal event probabilities in both groups
                    # then apply the hazard ratio to modify the experimental group's hazard
                    result <- powerSurvEpi::powerCT.default(
                        nE = nE,
                        nC = nC,
                        pE = prob_event,
                        pC = prob_event,
                        RR = hr,
                        alpha = alpha
                    )

                    # Generate range of sample sizes for the plot
                    sample_sizes <- seq(n * 0.2, n * 2, length.out = 100)
                    plot_data <- data.frame(
                        sample_size = sample_sizes,
                        power = sapply(sample_sizes, function(nn) {
                            # Calculate new group sizes for each total sample size
                            sample_nE <- nn / (1 + k)
                            sample_nC <- (nn * k) / (1 + k)

                            powerSurvEpi::powerCT.default(
                                nE = sample_nE,
                                nC = sample_nC,
                                pE = prob_event,
                                pC = prob_event,
                                RR = hr,
                                alpha = alpha
                            )
                        })
                    )

                } else if (study_design == "complex") {
                    # Enhanced complex design implementation
                    # Calculate effective event probability accounting for accrual pattern,
                    # variable follow-up times, and loss to follow-up
                    
                    # Convert median survival to hazard rate (assuming exponential distribution)
                    baseline_hazard <- log(2) / median_survival
                    
                    # Average follow-up time accounting for accrual pattern
                    # For uniform accrual, average follow-up = accrual_time/2 + follow_up_time
                    avg_followup <- accrual_time / 2 + follow_up_time
                    
                    # Adjust for loss to follow-up (competing risk)
                    # Effective hazard includes both event and loss to follow-up
                    competing_hazard <- loss_to_followup
                    total_hazard <- baseline_hazard + competing_hazard
                    
                    # Probability of observing the event (not censored by loss to follow-up)
                    event_prob_given_endpoint <- baseline_hazard / total_hazard
                    
                    # Probability of reaching any endpoint during study period
                    prob_any_endpoint <- 1 - exp(-total_hazard * avg_followup)
                    
                    # Effective probability of observing the event of interest
                    effective_prob <- prob_any_endpoint * event_prob_given_endpoint

                    result <- powerSurvEpi::powerCT.default(
                        nE = nE,
                        nC = nC,
                        pE = effective_prob,
                        pC = effective_prob,
                        RR = hr,
                        alpha = alpha
                    )

                    # Generate plot data
                    sample_sizes <- seq(n * 0.2, n * 2, length.out = 100)
                    plot_data <- data.frame(
                        sample_size = sample_sizes,
                        power = sapply(sample_sizes, function(nn) {
                            sample_nE <- nn / (1 + k)
                            sample_nC <- (nn * k) / (1 + k)

                            powerSurvEpi::powerCT.default(
                                nE = sample_nE,
                                nC = sample_nC,
                                pE = effective_prob,
                                pC = effective_prob,
                                RR = hr,
                                alpha = alpha
                            )
                        })
                    )
                }

                # Display power result
                self$results$power_result$setContent(paste(
                    "<p><strong>Power Analysis Results:</strong></p>",
                    "<p>Based on the specified parameters, the estimated power is:</p>",
                    "<p style='font-size: 1.2em; text-align: center;'>",
                    round(result, 4),
                    " (", round(result*100, 1), "%)",
                    "</p>",
                    "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 15px;'>",
                    "<p><strong>Interpretation:</strong> With a sample size of ", n,
                    " and a hazard ratio of ", hr,
                    ", the study has a ", round(result*100, 1),
                    "% chance of detecting this effect if it truly exists.</p>",

                    "<p><strong>What does this mean?</strong> Power represents the probability of rejecting the null hypothesis when it is indeed false. In survival analysis, this means detecting a true difference in survival between groups.</p>",

                    "<ul style='margin-top: 10px;'>",
                    "<li>Power of <strong>80% or higher</strong> is generally considered adequate for most clinical studies.</li>",
                    "<li>Power of <strong>90% or higher</strong> provides greater assurance but requires larger sample sizes.</li>",
                    "<li>Power below <strong>70%</strong> indicates a high risk of failing to detect true effects.</li>",
                    "</ul>",

                    if(result < 0.8) paste("<p><strong>Recommendation:</strong> Your calculated power (", round(result*100, 1), "%) is below the conventional threshold of 80%. Consider increasing your sample size, extending follow-up time to observe more events, or focusing on detecting a larger effect size (more extreme hazard ratio).</p>") else if(result >= 0.9) paste("<p><strong>Recommendation:</strong> Your calculated power (", round(result*100, 1), "%) exceeds 90%, indicating a robust study design with excellent ability to detect the specified effect.</p>") else paste("<p><strong>Recommendation:</strong> Your calculated power (", round(result*100, 1), "%) meets the conventional threshold of 80%. This indicates an adequate study design, though increasing sample size would provide additional assurance.</p>"),
                    "</div>",

                    "<div style='background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin-top: 15px;'>",
                    "<p><strong>Key Factors Affecting Power:</strong></p>",
                    "<ul>",
                    "<li><strong>Sample size:</strong> Larger samples increase power</li>",
                    "<li><strong>Effect size (hazard ratio):</strong> Larger differences between groups are easier to detect</li>",
                    "<li><strong>Event rate:</strong> Higher event rates provide more information for analysis</li>",
                    "<li><strong>Follow-up duration:</strong> Longer follow-up allows more events to occur</li>",
                    "<li><strong>Loss to follow-up:</strong> Higher dropout rates reduce effective sample size</li>",
                    "</ul>",
                    "<p>The plot below shows how power changes with different sample sizes while keeping other parameters constant.</p>",
                    "</div>"
                ))

            } else if (calc_type == "sample_size") {
                # Calculate required sample size
                if (study_design == "simple") {
                    # Use binary search to find the required sample size
                    min_n <- 10
                    max_n <- 10000

                    # Function to calculate power given total sample size
                    calculate_power <- function(total_n) {
                        group_nE <- total_n / (1 + k)
                        group_nC <- (total_n * k) / (1 + k)

                        return(powerSurvEpi::powerCT.default(
                            nE = group_nE,
                            nC = group_nC,
                            pE = prob_event,
                            pC = prob_event,
                            RR = hr,
                            alpha = alpha
                        ))
                    }

                    while ((max_n - min_n) > 1) {
                        mid_n <- floor((min_n + max_n) / 2)
                        current_power <- calculate_power(mid_n)

                        if (current_power < power) {
                            min_n <- mid_n
                        } else {
                            max_n <- mid_n
                        }
                    }

                    result <- max_n

                    # Generate range of hazard ratios for the plot
                    hazard_ratios <- seq(
                        max(hr * 0.5, 0.1),
                        min(hr * 2, 3),
                        length.out = 100
                    )
                    plot_data <- data.frame(
                        hazard_ratio = hazard_ratios,
                        sample_size = sapply(hazard_ratios, function(h) {
                            # Define power calculation for binary search
                            calculate_hr_power <- function(total_n, hazard_ratio) {
                                group_nE <- total_n / (1 + k)
                                group_nC <- (total_n * k) / (1 + k)

                                return(powerSurvEpi::powerCT.default(
                                    nE = group_nE,
                                    nC = group_nC,
                                    pE = prob_event,
                                    pC = prob_event,
                                    RR = hazard_ratio,
                                    alpha = alpha
                                ))
                            }

                            # Search for sample size at each HR
                            min_n <- 10
                            max_n <- 10000

                            while ((max_n - min_n) > 1) {
                                mid_n <- floor((min_n + max_n) / 2)
                                current_power <- calculate_hr_power(mid_n, h)

                                if (current_power < power) {
                                    min_n <- mid_n
                                } else {
                                    max_n <- mid_n
                                }
                            }

                            return(max_n)
                        })
                    )

                } else if (study_design == "complex") {
                    # Enhanced complex design implementation for sample size calculation
                    baseline_hazard <- log(2) / median_survival
                    avg_followup <- accrual_time / 2 + follow_up_time
                    competing_hazard <- loss_to_followup
                    total_hazard <- baseline_hazard + competing_hazard
                    event_prob_given_endpoint <- baseline_hazard / total_hazard
                    prob_any_endpoint <- 1 - exp(-total_hazard * avg_followup)
                    effective_prob <- prob_any_endpoint * event_prob_given_endpoint

                    # Define power calculation for binary search
                    calculate_power <- function(total_n) {
                        group_nE <- total_n / (1 + k)
                        group_nC <- (total_n * k) / (1 + k)

                        return(powerSurvEpi::powerCT.default(
                            nE = group_nE,
                            nC = group_nC,
                            pE = effective_prob,
                            pC = effective_prob,
                            RR = hr,
                            alpha = alpha
                        ))
                    }

                    min_n <- 10
                    max_n <- 10000

                    while ((max_n - min_n) > 1) {
                        mid_n <- floor((min_n + max_n) / 2)
                        current_power <- calculate_power(mid_n)

                        if (current_power < power) {
                            min_n <- mid_n
                        } else {
                            max_n <- mid_n
                        }
                    }

                    result <- max_n

                    # Generate plot data
                    hazard_ratios <- seq(
                        max(hr * 0.5, 0.1),
                        min(hr * 2, 3),
                        length.out = 100
                    )
                    plot_data <- data.frame(
                        hazard_ratio = hazard_ratios,
                        sample_size = sapply(hazard_ratios, function(h) {
                            # Define power calculation for binary search
                            calculate_hr_power <- function(total_n, hazard_ratio) {
                                group_nE <- total_n / (1 + k)
                                group_nC <- (total_n * k) / (1 + k)

                                return(powerSurvEpi::powerCT.default(
                                    nE = group_nE,
                                    nC = group_nC,
                                    pE = effective_prob,
                                    pC = effective_prob,
                                    RR = hazard_ratio,
                                    alpha = alpha
                                ))
                            }

                            min_n <- 10
                            max_n <- 10000

                            while ((max_n - min_n) > 1) {
                                mid_n <- floor((min_n + max_n) / 2)
                                current_power <- calculate_hr_power(mid_n, h)

                                if (current_power < power) {
                                    min_n <- mid_n
                                } else {
                                    max_n <- mid_n
                                }
                            }

                            return(max_n)
                        })
                    )
                }

                # Adjust sample size based on equal or unequal allocation
                if (k != 1) {
                    n_treatment <- round(result / (1 + k))
                    n_control <- round(result - n_treatment)

                    sample_size_text <- paste(
                        "<p><strong>Sample Size Calculation Results:</strong></p>",
                        "<p>Based on the specified parameters, the required total sample size is:</p>",
                        "<p style='font-size: 1.2em; text-align: center;'>",
                        result,
                        "</p>",
                        "<p>With allocation ratio ", k, ":</p>",
                        "<ul>",
                        "<li>Treatment group: ", n_treatment, " participants</li>",
                        "<li>Control group: ", n_control, " participants</li>",
                        "</ul>",

                        "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 15px;'>",
                        "<p><strong>Interpretation:</strong> This sample size provides ", round(power*100, 1),
                        "% power to detect a hazard ratio of ", hr,
                        " at a significance level of ", alpha, ".</p>",

                        "<p><strong>What does this mean?</strong> In survival analysis, the sample size depends not just on the number of participants but also on the number of events observed. This calculation estimates how many participants you need to recruit to observe enough events to achieve your desired statistical power.</p>",

                        "<p><strong>Unequal allocation:</strong> You've specified an allocation ratio of ", k, ", meaning you'll recruit ", k, " times as many participants in the control group as in the treatment group. Unequal allocation may be useful when:</p>",
                        "<ul>",
                        "<li>The treatment is expensive or has potential side effects</li>",
                        "<li>You want to gain more experience with the new treatment</li>",
                        "<li>Historical or external controls are being used</li>",
                        "</ul>",
                        "<p>Note that while unequal allocation can be efficient in some cases, it typically requires a larger total sample size than equal allocation.</p>",
                        "</div>",

                        "<div style='background-color: #f0f8eb; padding: 15px; border-radius: 5px; margin-top: 15px;'>",
                        "<p><strong>Practical Considerations:</strong></p>",
                        "<ul>",
                        "<li><strong>Recruitment feasibility:</strong> Consider whether recruiting ", result, " participants is realistic within your timeframe and budget.</li>",
                        "<li><strong>Expected dropout rate:</strong> It's often wise to recruit 10-15% more participants to account for potential dropouts.</li>",
                        "<li><strong>Clinical significance:</strong> Ensure the hazard ratio of ", hr, " represents a clinically meaningful difference worth detecting.</li>",
                        "</ul>",
                        "</div>",

                        "<p>The plot below shows how the required sample size changes with different hazard ratios while maintaining ", round(power*100, 1), "% power.</p>"
                    )
                } else {
                    n_per_group <- round(result / 2)

                    sample_size_text <- paste(
                        "<p><strong>Sample Size Calculation Results:</strong></p>",
                        "<p>Based on the specified parameters, the required total sample size is:</p>",
                        "<p style='font-size: 1.2em; text-align: center;'>",
                        result,
                        "</p>",
                        "<p>With equal allocation:</p>",
                        "<ul>",
                        "<li>Treatment group: ", n_per_group, " participants</li>",
                        "<li>Control group: ", n_per_group, " participants</li>",
                        "</ul>",

                        "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 15px;'>",
                        "<p><strong>Interpretation:</strong> This sample size provides ", round(power*100, 1),
                        "% power to detect a hazard ratio of ", hr,
                        " at a significance level of ", alpha, ".</p>",

                        "<p><strong>What does this mean?</strong> In survival analysis, the sample size depends not just on the number of participants but also on the number of events observed. This calculation estimates how many participants you need to recruit to observe enough events to achieve your desired statistical power.</p>",

                        "<p><strong>Equal allocation:</strong> You've specified an equal allocation ratio, which is generally the most statistically efficient approach and requires the smallest total sample size to achieve the desired power.</p>",
                        "</div>",

                        "<div style='background-color: #f0f8eb; padding: 15px; border-radius: 5px; margin-top: 15px;'>",
                        "<p><strong>Practical Considerations:</strong></p>",
                        "<ul>",
                        "<li><strong>Recruitment feasibility:</strong> Consider whether recruiting ", result, " participants is realistic within your timeframe and budget.</li>",
                        "<li><strong>Expected dropout rate:</strong> It's often wise to recruit 10-15% more participants to account for potential dropouts.</li>",
                        "<li><strong>Clinical significance:</strong> Ensure the hazard ratio of ", hr, " represents a clinically meaningful difference worth detecting.</li>",
                        "</ul>",
                        "</div>",

                        "<p>The plot below shows how the required sample size changes with different hazard ratios while maintaining ", round(power*100, 1), "% power.</p>"
                    )
                }

                self$results$sample_size_result$setContent(sample_size_text)

            } else if (calc_type == "hazard_ratio") {
                # Calculate minimum detectable hazard ratio
                if (study_design == "simple") {
                    # Function to calculate power given hazard ratio
                    calculate_hr_power <- function(hazard_ratio) {
                        return(powerSurvEpi::powerCT.default(
                            nE = nE,
                            nC = nC,
                            pE = prob_event,
                            pC = prob_event,
                            RR = hazard_ratio,
                            alpha = alpha
                        ))
                    }

                    # Binary search for minimum detectable hazard ratio
                    min_hr <- 0.1
                    max_hr <- 5.0

                    if (hr < 1) {
                        # For protective effects (HR < 1)
                        min_hr <- 0.1
                        max_hr <- 0.99
                    } else {
                        # For harmful effects (HR > 1)
                        min_hr <- 1.01
                        max_hr <- 5.0
                    }

                    iterations <- 0
                    max_iter <- 100

                    while (iterations < max_iter && (max_hr - min_hr) > 0.001) {
                        mid_hr <- (min_hr + max_hr) / 2
                        current_power <- calculate_hr_power(mid_hr)

                        if (current_power < power) {
                            if (hr < 1) {
                                # For protective effects, decrease HR to increase effect size
                                max_hr <- mid_hr
                            } else {
                                # For harmful effects, increase HR to increase effect size
                                min_hr <- mid_hr
                            }
                        } else {
                            if (hr < 1) {
                                # For protective effects, increase HR to decrease effect size
                                min_hr <- mid_hr
                            } else {
                                # For harmful effects, decrease HR to decrease effect size
                                max_hr <- mid_hr
                            }
                        }

                        iterations <- iterations + 1
                    }

                    result <- mid_hr

                    # Generate plot data for different sample sizes
                    sample_sizes <- seq(max(n * 0.2, 10), n * 2, length.out = 100)
                    plot_data <- data.frame(
                        sample_size = sample_sizes,
                        hazard_ratio = numeric(length(sample_sizes))
                    )

                    for (i in seq_along(sample_sizes)) {
                        current_n <- sample_sizes[i]
                        current_nE <- current_n / (1 + k)
                        current_nC <- (current_n * k) / (1 + k)

                        # Function to calculate power for current sample size and HR
                        calculate_sample_hr_power <- function(hazard_ratio) {
                            return(powerSurvEpi::powerCT.default(
                                nE = current_nE,
                                nC = current_nC,
                                pE = prob_event,
                                pC = prob_event,
                                RR = hazard_ratio,
                                alpha = alpha
                            ))
                        }

                        min_hr <- 0.1
                        max_hr <- 5.0

                        if (hr < 1) {
                            min_hr <- 0.1
                            max_hr <- 0.99
                        } else {
                            min_hr <- 1.01
                            max_hr <- 5.0
                        }

                        iterations <- 0

                        while (iterations < max_iter && (max_hr - min_hr) > 0.001) {
                            mid_hr <- (min_hr + max_hr) / 2
                            current_power <- calculate_sample_hr_power(mid_hr)

                            if (current_power < power) {
                                if (hr < 1) {
                                    max_hr <- mid_hr
                                } else {
                                    min_hr <- mid_hr
                                }
                            } else {
                                if (hr < 1) {
                                    min_hr <- mid_hr
                                } else {
                                    max_hr <- mid_hr
                                }
                            }

                            iterations <- iterations + 1
                        }

                        plot_data$hazard_ratio[i] <- mid_hr
                    }

                } else if (study_design == "complex") {
                    # Enhanced complex design implementation for hazard ratio calculation
                    baseline_hazard <- log(2) / median_survival
                    avg_followup <- accrual_time / 2 + follow_up_time
                    competing_hazard <- loss_to_followup
                    total_hazard <- baseline_hazard + competing_hazard
                    event_prob_given_endpoint <- baseline_hazard / total_hazard
                    prob_any_endpoint <- 1 - exp(-total_hazard * avg_followup)
                    effective_prob <- prob_any_endpoint * event_prob_given_endpoint

                    # Function to calculate power given hazard ratio for complex design
                    calculate_hr_power <- function(hazard_ratio) {
                        return(powerSurvEpi::powerCT.default(
                            nE = nE,
                            nC = nC,
                            pE = effective_prob,
                            pC = effective_prob,
                            RR = hazard_ratio,
                            alpha = alpha
                        ))
                    }

                    min_hr <- 0.1
                    max_hr <- 5.0

                    if (hr < 1) {
                        min_hr <- 0.1
                        max_hr <- 0.99
                    } else {
                        min_hr <- 1.01
                        max_hr <- 5.0
                    }

                    iterations <- 0
                    max_iter <- 100

                    while (iterations < max_iter && (max_hr - min_hr) > 0.001) {
                        mid_hr <- (min_hr + max_hr) / 2
                        current_power <- calculate_hr_power(mid_hr)

                        if (current_power < power) {
                            if (hr < 1) {
                                max_hr <- mid_hr
                            } else {
                                min_hr <- mid_hr
                            }
                        } else {
                            if (hr < 1) {
                                min_hr <- mid_hr
                            } else {
                                max_hr <- mid_hr
                            }
                        }

                        iterations <- iterations + 1
                    }

                    result <- mid_hr

                    # Generate plot data
                    sample_sizes <- seq(max(n * 0.2, 10), n * 2, length.out = 100)
                    plot_data <- data.frame(
                        sample_size = sample_sizes,
                        hazard_ratio = numeric(length(sample_sizes))
                    )

                    for (i in seq_along(sample_sizes)) {
                        current_n <- sample_sizes[i]
                        current_nE <- current_n / (1 + k)
                        current_nC <- (current_n * k) / (1 + k)

                        # Function to calculate power for current sample size and HR
                        calculate_sample_hr_power <- function(hazard_ratio) {
                            return(powerSurvEpi::powerCT.default(
                                nE = current_nE,
                                nC = current_nC,
                                pE = effective_prob,
                                pC = effective_prob,
                                RR = hazard_ratio,
                                alpha = alpha
                            ))
                        }

                        min_hr <- 0.1
                        max_hr <- 5.0

                        if (hr < 1) {
                            min_hr <- 0.1
                            max_hr <- 0.99
                        } else {
                            min_hr <- 1.01
                            max_hr <- 5.0
                        }

                        iterations <- 0

                        while (iterations < max_iter && (max_hr - min_hr) > 0.001) {
                            mid_hr <- (min_hr + max_hr) / 2
                            current_power <- calculate_sample_hr_power(mid_hr)

                            if (current_power < power) {
                                if (hr < 1) {
                                    max_hr <- mid_hr
                                } else {
                                    min_hr <- mid_hr
                                }
                            } else {
                                if (hr < 1) {
                                    min_hr <- mid_hr
                                } else {
                                    max_hr <- mid_hr
                                }
                            }

                            iterations <- iterations + 1
                        }

                        plot_data$hazard_ratio[i] <- mid_hr
                    }
                }

                # Display hazard ratio result
                if (result < 1) {
                    hr_text <- paste(
                        "<p><strong>Hazard Ratio Analysis Results:</strong></p>",
                        "<p>Based on the specified parameters, the minimum detectable hazard ratio is:</p>",
                        "<p style='font-size: 1.2em; text-align: center;'>",
                        round(result, 3),
                        "</p>",

                        "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 15px;'>",
                        "<p><strong>Interpretation:</strong> With a sample size of ", n,
                        " and power of ", round(power*100, 1),
                        "%, your study can detect a protective effect (hazard ratio < 1) of at least this magnitude.</p>",

                        "<p><strong>What does this mean?</strong> The treatment group would need to have at least a ", round((1-result)*100, 1),
                        "% reduction in hazard (risk) compared to the control group for your study to detect it reliably.</p>",

                        "<p><strong>Hazard ratio of ", round(result, 3), " means:</strong></p>",
                        "<ul>",
                        "<li>At any point in time, the treatment group has ", round((1-result)*100, 1), "% lower risk of experiencing the event than the control group</li>",
                        "<li>If the median survival time in the control group is 1 year, the median survival in the treatment group would be approximately ", round(1/result, 2), " years</li>",
                        "</ul>",
                        "</div>",

                        "<div style='background-color: #f0f8eb; padding: 15px; border-radius: 5px; margin-top: 15px;'>",
                        "<p><strong>Clinical Relevance Assessment:</strong></p>",
                        if(result > 0.8) paste("<p><strong>Caution:</strong> Your study is only powered to detect relatively large effects (≥", round((1-result)*100, 1), "% risk reduction). Consider whether such a large effect is plausible in your research context. If smaller effects would be clinically important, you might need to increase your sample size.</p>") else paste("<p><strong>Favorable:</strong> Your study can detect moderate to small protective effects (", round((1-result)*100, 1), "% risk reduction), which are often clinically meaningful in survival studies.</p>"),
                        "</div>",

                        "<p>The plot below shows how the minimum detectable hazard ratio changes with different sample sizes while maintaining ", round(power*100, 1), "% power.</p>"
                    )
                } else {
                    hr_text <- paste(
                        "<p><strong>Hazard Ratio Analysis Results:</strong></p>",
                        "<p>Based on the specified parameters, the minimum detectable hazard ratio is:</p>",
                        "<p style='font-size: 1.2em; text-align: center;'>",
                        round(result, 3),
                        "</p>",

                        "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 15px;'>",
                        "<p><strong>Interpretation:</strong> With a sample size of ", n,
                        " and power of ", round(power*100, 1),
                        "%, your study can detect a harmful effect (hazard ratio > 1) of at least this magnitude.</p>",

                        "<p><strong>What does this mean?</strong> The treatment group would need to have at least a ", round((result-1)*100, 1),
                        "% increase in hazard (risk) compared to the control group for your study to detect it reliably.</p>",

                        "<p><strong>Hazard ratio of ", round(result, 3), " means:</strong></p>",
                        "<ul>",
                        "<li>At any point in time, the treatment group has ", round((result-1)*100, 1), "% higher risk of experiencing the event than the control group</li>",
                        "<li>If the median survival time in the control group is 1 year, the median survival in the treatment group would be approximately ", round(1/result, 2), " years</li>",
                        "</ul>",
                        "</div>",

                        "<div style='background-color: #f0f8eb; padding: 15px; border-radius: 5px; margin-top: 15px;'>",
                        "<p><strong>Clinical Relevance Assessment:</strong></p>",
                        if(result < 1.25) paste("<p><strong>Caution:</strong> Your study is only powered to detect relatively large harmful effects (≥", round((result-1)*100, 1), "% risk increase). Consider whether such a large effect is plausible in your research context. If smaller harmful effects would be important to detect, you might need to increase your sample size.</p>") else paste("<p><strong>Note:</strong> Your study can detect moderate to large harmful effects (", round((result-1)*100, 1), "% risk increase). This sensitivity is important for safety monitoring, though you may need a larger sample to detect more subtle harmful effects.</p>"),
                        "</div>",

                        "<p>The plot below shows how the minimum detectable hazard ratio changes with different sample sizes while maintaining ", round(power*100, 1), "% power.</p>"
                    )
                }

                self$results$hazard_ratio_result$setContent(hr_text)
            }

            # Set state for plotting
            image1 <- self$results$power_plot
            image1$setState(list(
                plot_data = plot_data,
                calc_type = calc_type,
                hr = hr,
                n = n,
                power = power
            ))
        },

        # Plot function
        .power_plot = function(image, ggtheme, theme, ...) {
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return(FALSE)
            }

            state <- image$state
            if (is.null(state) || is.null(state$plot_data)) {
                return(FALSE)
            }

            plot_data <- state$plot_data
            calc_type <- state$calc_type
            hr <- state$hr
            n <- state$n
            power <- state$power

            plot <- NULL

            # Enhanced plot titles and captions
            if (calc_type == "power") {
                # Plot power vs sample size
                plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = sample_size, y = power)) +
                    ggplot2::geom_line(color = "blue", size = 1) +
                    ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
                    ggplot2::geom_vline(xintercept = n, linetype = "dashed") +
                    ggplot2::labs(
                        title = "Statistical Power by Sample Size",
                        subtitle = paste("For HR =", hr, "| Red line indicates 80% power threshold"),
                        caption = "Increasing sample size improves power, but with diminishing returns",
                        x = "Total Sample Size",
                        y = "Statistical Power"
                    ) +
                    ggplot2::ylim(0, 1) +
                    ggplot2::theme_bw()

            } else if (calc_type == "sample_size") {
                # Plot sample size vs hazard ratio
                if (hr < 1) {
                    # For protective effects, reverse the x-axis
                    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = hazard_ratio, y = sample_size)) +
                        ggplot2::geom_line(color = "blue", size = 1) +
                        ggplot2::geom_hline(yintercept = n, linetype = "dashed") +
                        ggplot2::geom_vline(xintercept = hr, linetype = "dashed", color = "red") +
                        ggplot2::labs(
                            title = "Required Sample Size by Hazard Ratio",
                            subtitle = paste("For", power*100, "% power | Red line shows HR =", hr),
                            caption = "Smaller effects (HR closer to 1) require larger sample sizes",
                            x = "Hazard Ratio (protective effect)",
                            y = "Required Total Sample Size"
                        ) +
                        ggplot2::scale_x_reverse() +
                        ggplot2::theme_bw()
                } else {
                    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = hazard_ratio, y = sample_size)) +
                        ggplot2::geom_line(color = "blue", size = 1) +
                        ggplot2::geom_hline(yintercept = n, linetype = "dashed") +
                        ggplot2::geom_vline(xintercept = hr, linetype = "dashed", color = "red") +
                        ggplot2::labs(
                            title = "Required Sample Size by Hazard Ratio",
                            subtitle = paste("For", power*100, "% power | Red line shows HR =", hr),
                            caption = "Smaller effects (HR closer to 1) require larger sample sizes",
                            x = "Hazard Ratio (harmful effect)",
                            y = "Required Total Sample Size"
                        ) +
                        ggplot2::theme_bw()
                }

            } else if (calc_type == "hazard_ratio") {
                # Plot minimum detectable hazard ratio vs sample size
                plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = sample_size, y = hazard_ratio)) +
                    ggplot2::geom_line(color = "blue", size = 1) +
                    ggplot2::geom_hline(yintercept = hr, linetype = "dashed", color = "red") +
                    ggplot2::geom_vline(xintercept = n, linetype = "dashed") +
                    ggplot2::labs(
                        title = "Minimum Detectable Hazard Ratio by Sample Size",
                        subtitle = paste("For", power*100, "% power | Red line shows current HR =", hr),
                        caption = "Larger sample sizes can detect smaller effects (HR values closer to 1)",
                        x = "Total Sample Size",
                        y = "Minimum Detectable Hazard Ratio"
                    ) +
                    ggplot2::theme_bw()

                if (hr < 1) {
                    # For protective effects, adjust y-axis to focus on values < 1
                    plot <- plot + ggplot2::ylim(0, 1)
                }
            }

            print(plot)
            return(TRUE)
        }
    )
)
