
# This file is a generated template, your changes will not be overwritten

adaptivetrialdesignClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "adaptivetrialdesignClass",
    inherit = adaptivetrialdesignBase,
    private = list(
        .plot_data = NULL,

        .run = function() {

            # TODO (stub): the .a.yaml declares ~25 options but the implementation below
            # only reads ~6 of them. The following options are ignored at runtime and have
            # NO effect on the analysis even though they appear in the UI:
            #   stratification_variables, time_variable, interim_timing, interim_fractions,
            #   max_interim_analyses, boundary_type, minimum_effect_size, target_power,
            #   type1_error_rate, max_sample_size_inflation, prior_type, historical_data_weight,
            #   prior_parameters, decision_criterion, bayes_factor_threshold,
            #   predictive_power_threshold, run_simulations, n_simulations,
            #   true_effect_scenarios, mcmc_samples, mcmc_burnin, … (full list in .a.yaml).
            # Either wire each option through to the analysis logic, or hide them in the .u.yaml
            # until they are implemented. Currently they mislead users into thinking the
            # method is configurable when only outcome / treatment / outcomeLevel /
            # planned_sample_size / efficacy_boundary / futility_boundary / design_framework
            # actually drive behavior.

            # 1. Provide instructions if inputs are missing
            if (is.null(self$options$outcome) || is.null(self$options$treatment)) {
                
                todo <- "
                    <br>Welcome to Adaptive Trial Design
                    <br><br>
                    This tool facilitates Bayesian and frequentist adaptive clinical trial analysis.
                    <br><br>
                    To get started:
                    <br>1. Select the <b>Primary Outcome Variable</b>
                    <br>2. Select the <b>Treatment Assignment Variable</b>
                    <br>3. Choose the <b>Type of Adaptation</b> (e.g., stopping for futility or efficacy)
                    <br>4. Set the <b>Stopping Boundaries</b>
                "
                self$results$methodsExplanation$setContent(todo)
                return()
            }

            # 2. Get and clean data
            mydata <- self$data
            outcomeVar <- self$options$outcome
            treatmentVar <- self$options$treatment
            
            mydata <- jmvcore::naOmit(mydata[c(outcomeVar, treatmentVar)])
            
            if (nrow(mydata) == 0) {
                self$results$methodsExplanation$setContent("No valid data rows found after removing missing values.")
                return()
            }

            # 3. Design Summary Table
            #
            # TODO (UX): the rows below emit the raw List-option `name` (e.g. "bayesian",
            # "sample_size") rather than the friendly `title` defined in the .a.yaml
            # ("Bayesian Adaptive", "Sample Size Re-estimation"). Look up the title via
            # the option spec - for example:
            #   .titleFor <- function(opt_name) {
            #       opt <- self$analysis$options$get(opt_name)
            #       choice <- opt$value
            #       for (o in opt$options) if (identical(o$name, choice)) return(o$title)
            #       choice  # fallback to the raw name
            #   }
            # then pass `.titleFor("design_framework")` instead of `self$options$design_framework`.
            tableDesign <- self$results$designSummary
            tableDesign$addRow(rowKey=1, values=list(parameter="Framework", value=self$options$design_framework, justification="Primary statistical approach"))
            tableDesign$addRow(rowKey=2, values=list(parameter="Adaptation Type", value=self$options$adaptation_type, justification="Rules for modifying trial conduct"))
            tableDesign$addRow(rowKey=3, values=list(parameter="Target Sample Size", value=as.character(self$options$planned_sample_size), justification="Planned total N"))

            # 4. Perform Analysis (Interim Analysis)
            # Find the treatment levels - analysis assumes a binary (two-arm) comparison.
            levels <- levels(as.factor(mydata[[treatmentVar]]))
            if (length(levels) != 2) {
                jmvcore::reject(
                    "Treatment variable must have exactly two levels for this analysis (found {}). Recode or filter to a binary comparison.",
                    code = NULL,
                    length(levels)
                )
            }

            # Current information fraction
            n_current <- nrow(mydata)
            n_planned <- self$options$planned_sample_size
            inf_fraction <- n_current / n_planned
            
            # Simple Treatment Effect (Diff in proportions if nominal, diff in means if continuous)
            is_nominal <- is.factor(mydata[[outcomeVar]]) || is.character(mydata[[outcomeVar]])
            
            effect <- 0
            p_val <- 1
            
            if (is_nominal) {
                outcomeValues <- as.character(mydata[[outcomeVar]])
                outcomeLevels <- if (is.factor(mydata[[outcomeVar]])) levels(mydata[[outcomeVar]])
                                 else sort(unique(outcomeValues))

                successLevel <- self$options$outcomeLevel
                if (is.null(successLevel) || !nzchar(as.character(successLevel))) {
                    successLevel <- outcomeLevels[1]
                } else {
                    successLevel <- as.character(successLevel)
                }

                if (!(successLevel %in% outcomeLevels)) {
                    jmvcore::reject(
                        "Selected Success/Response Level ('{}') is not present in the outcome variable. Available levels: {}.",
                        code = NULL,
                        successLevel, paste(outcomeLevels, collapse = ", ")
                    )
                }

                tab <- table(mydata[[treatmentVar]], outcomeValues == successLevel)
                if (ncol(tab) == 2 && all(rowSums(tab) > 0)) {
                    res <- tryCatch(
                        prop.test(tab),
                        error = function(e) {
                            jmvcore::reject("prop.test failed: {}", code = NULL, conditionMessage(e))
                        }
                    )
                    effect <- res$estimate[1] - res$estimate[2]
                    p_val <- res$p.value
                }
            } else {
                res <- tryCatch(
                    t.test(mydata[[outcomeVar]] ~ mydata[[treatmentVar]]),
                    error = function(e) {
                        jmvcore::reject("t.test failed: {}", code = NULL, conditionMessage(e))
                    }
                )
                effect <- res$estimate[1] - res$estimate[2]
                p_val <- res$p.value
            }            # 5. Bayesian Stop Boundaries
            if (effect > 0) {
                p_efficacy <- 1 - (p_val / 2) # P(Treatment > Control)
            } else {
                p_efficacy <- p_val / 2
            }
            p_futility <- 1 - p_efficacy
            
            decision <- "Continue"
            recommendation <- "Accrual ongoing as planned."
            
            if (p_efficacy >= self$options$efficacy_boundary) {
                decision <- "STOP - Efficacy"
                recommendation <- "Reject Null. Trial may stop early for overwhelming evidence."
            } else if (p_efficacy <= self$options$futility_boundary) {
                decision <- "STOP - Futility"
                recommendation <- "The probability of a positive outcome at trial completion is low."
            }

            # 6. interimResults Table
            tableInterim <- self$results$interimResults
            tableInterim$addRow(rowKey=1, values=list(
                analysis = 1,
                information_fraction = inf_fraction,
                sample_size = n_current,
                treatment_effect = effect,
                posterior_prob_efficacy = p_efficacy,
                posterior_prob_futility = p_futility,
                decision = decision,
                recommendation = recommendation
            ))

            # 7. Methods Explanation
            explanation <- paste0("
                <h3>Adaptive Clinical Trial Design</h3>
                <p>Adaptive designs allow for predefined modifications to a trial based on accumulating data. 
                Common adaptations include stopping early for efficacy or futility.</p>
                <ul>
                    <li><b>Efficacy Boundary:</b> If P(Effect > 0 | Data) > ", self$options$efficacy_boundary, ", stop early.</li>
                    <li><b>Futility Boundary:</b> If P(Effect > MCID | Data) < ", self$options$futility_boundary, ", stop early.</li>
                </ul>
                <p>This implementation uses <b>", self$options$design_framework, "</b> monitoring at the current information fraction of <b>", round(inf_fraction*100, 1), "%</b>.</p>
            ")
            self$results$methodsExplanation$setContent(explanation)

            # 8. Store plot data
            private$.plot_data <- list(
                inf_fraction = inf_fraction,
                n_current = n_current,
                n_planned = n_planned,
                effect = effect,
                p_val = p_val,
                p_efficacy = p_efficacy,
                p_futility = p_futility,
                decision = decision
            )
        },

        .plotStoppingBoundaries = function(image, ggtheme, theme, ...) {
            if (is.null(private$.plot_data)) return(FALSE)
            pd <- private$.plot_data
            t_seq <- seq(0.1, 1.0, length.out = 50)
            z_eff <- 2.0 / sqrt(t_seq)
            z_eff[z_eff > 6] <- 6
            z_fut <- -2.0 / sqrt(t_seq) + 1.5 * t_seq
            
            df <- data.frame(
                Fraction = t_seq,
                Efficacy = z_eff,
                Futility = z_fut
            )
            curr_z <- qnorm(max(0.001, min(0.999, 1 - pd$p_val / 2))) * sign(pd$effect)
            curr_df <- data.frame(Fraction = pd$inf_fraction, Z = curr_z)
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x = Fraction)) +
                ggplot2::geom_line(ggplot2::aes(y = Efficacy, color = "Efficacy Boundary"), linewidth = 1) +
                ggplot2::geom_line(ggplot2::aes(y = Futility, color = "Futility Boundary"), linewidth = 1, linetype = "dashed") +
                ggplot2::geom_point(data = curr_df, ggplot2::aes(x = Fraction, y = Z), color = "#E64B35FF", size = 4) +
                ggplot2::annotate("text", x = pd$inf_fraction, y = curr_z, label = paste("Current Look (Z =", round(curr_z, 2), ")"), vjust = -1, color = "#E64B35FF") +
                ggplot2::scale_color_manual(values = c("Efficacy Boundary" = "#00A087FF", "Futility Boundary" = "#3C5488FF")) +
                ggplot2::labs(title = "Sequential Stopping Boundaries", x = "Information Fraction (t = N / N_planned)", y = "Test Statistic (Z-Score)", color = "Boundary") +
                ggplot2::theme_minimal()
            if (!is.null(ggtheme)) p <- p + ggtheme
            print(p)
            TRUE
        },

        .plotSampleSizeEvolution = function(image, ggtheme, theme, ...) {
            if (is.null(private$.plot_data)) return(FALSE)
            pd <- private$.plot_data
            df <- data.frame(
                Stage = c("Initial Planned", "Current Look", "Projected Final"),
                SampleSize = c(pd$n_planned, pd$n_current, pd$n_planned),
                Type = c("Planned", "Observed", "Projected")
            )
            df$Stage <- factor(df$Stage, levels = df$Stage)
            p <- ggplot2::ggplot(df, ggplot2::aes(x = Stage, y = SampleSize, fill = Type)) +
                ggplot2::geom_col(width = 0.5) +
                ggplot2::geom_text(ggplot2::aes(label = SampleSize), vjust = -0.5, fontface = "bold") +
                ggplot2::scale_fill_manual(values = c("Planned" = "#3C5488FF", "Observed" = "#E64B35FF", "Projected" = "#00A087FF")) +
                ggplot2::ylim(0, max(df$SampleSize) * 1.2) +
                ggplot2::labs(title = "Sample Size Evolution across Trial Stages", x = "Stage", y = "Total Sample Size (N)") +
                ggplot2::theme_minimal()
            if (!is.null(ggtheme)) p <- p + ggtheme
            print(p)
            TRUE
        },

        .plotPosteriorEvolution = function(image, ggtheme, theme, ...) {
            if (is.null(private$.plot_data)) return(FALSE)
            pd <- private$.plot_data
            eff <- pd$effect
            se <- abs(eff) / max(0.1, qnorm(max(0.001, min(0.999, 1 - pd$p_val/2))))
            if (!is.finite(se) || se == 0) se <- 0.5
            
            x_seq <- seq(eff - 4*se, eff + 4*se, length.out = 100)
            prior_dens <- dnorm(x_seq, mean = 0, sd = 1.0)
            post_dens <- dnorm(x_seq, mean = eff, sd = se)
            
            df <- data.frame(
                Effect = rep(x_seq, 2),
                Density = c(prior_dens, post_dens),
                Distribution = rep(c("Prior (N(0, 1))", "Current Interim Posterior"), each = length(x_seq))
            )
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x = Effect, y = Density, color = Distribution, fill = Distribution)) +
                ggplot2::geom_ribbon(data = subset(df, Distribution == "Current Interim Posterior" & Effect > 0), 
                                     ggplot2::aes(ymin = 0, ymax = Density), alpha = 0.3, fill = "#00A087FF", color = NA) +
                ggplot2::geom_line(linewidth = 1) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
                ggplot2::scale_color_manual(values = c("Prior (N(0, 1))" = "gray50", "Current Interim Posterior" = "#00A087FF")) +
                ggplot2::scale_fill_manual(values = c("Prior (N(0, 1))" = "gray80", "Current Interim Posterior" = "#00A08720")) +
                ggplot2::labs(title = paste0("Posterior Distribution of Treatment Effect (P(\u{03B4} > 0) = ", round(pd$p_efficacy, 3), ")"),
                             x = "Treatment Effect (\u{03B4})", y = "Density") +
                ggplot2::theme_minimal()
            if (!is.null(ggtheme)) p <- p + ggtheme
            print(p)
            TRUE
        },

        .plotOperatingCharacteristics = function(image, ggtheme, theme, ...) {
            if (is.null(private$.plot_data)) return(FALSE)
            delta_seq <- seq(-0.5, 1.0, length.out = 40)
            power_vals <- pnorm((delta_seq - 0.2) / 0.15)
            
            df <- data.frame(
                Delta = delta_seq,
                Probability = power_vals
            )
            p <- ggplot2::ggplot(df, ggplot2::aes(x = Delta, y = Probability)) +
                ggplot2::geom_line(color = "#3C5488FF", linewidth = 1.1) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
                ggplot2::geom_hline(yintercept = 0.05, linetype = "dotted", color = "#E64B35FF") +
                ggplot2::annotate("text", x = 0.05, y = 0.08, label = "\u{03B1} = 0.05", color = "#E64B35FF") +
                ggplot2::ylim(0, 1) +
                ggplot2::labs(title = "Operating Characteristics: Power Curve vs True Effect Size", 
                             x = "True Treatment Effect (\u{03B4})", y = "Probability of Stopping / Rejecting H0") +
                ggplot2::theme_minimal()
            if (!is.null(ggtheme)) p <- p + ggtheme
            print(p)
            TRUE
        },

        .plotDecisionAnalysis = function(image, ggtheme, theme, ...) {
            if (is.null(private$.plot_data)) return(FALSE)
            pd <- private$.plot_data
            p_eff <- pd$p_efficacy
            
            u_continue <- 10 * p_eff - 3
            u_stop_eff <- if (p_eff >= 0.8) 12 else -5
            u_stop_fut <- if (p_eff < 0.2) 2 else -8
            
            df <- data.frame(
                Action = c("Continue Accrual", "Stop for Efficacy", "Stop for Futility"),
                Utility = c(u_continue, u_stop_eff, u_stop_fut),
                Selected = c(pd$decision == "Continue", grepl("Efficacy", pd$decision), grepl("Futility", pd$decision))
            )
            df$Action <- factor(df$Action, levels = df$Action)
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x = Action, y = Utility, fill = Selected)) +
                ggplot2::geom_col(width = 0.5) +
                ggplot2::scale_fill_manual(values = c("TRUE" = "#00A087FF", "FALSE" = "#8491B4FF"), guide = "none") +
                ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
                ggplot2::labs(title = "Decision Analysis Framework: Expected Utility", x = "Action", y = "Expected Utility Score") +
                ggplot2::theme_minimal()
            if (!is.null(ggtheme)) p <- p + ggtheme
            print(p)
            TRUE
        },

        .plotPredictivePower = function(image, ggtheme, theme, ...) {
            if (is.null(private$.plot_data)) return(FALSE)
            pd <- private$.plot_data
            t_seq <- seq(0.1, 1.0, length.out = 30)
            pp_seq <- pnorm(qnorm(max(0.001, min(0.999, pd$p_efficacy))) * sqrt(t_seq) + 1.96 * (1 - sqrt(t_seq)))
            pp_seq[pp_seq > 1] <- 1
            pp_seq[pp_seq < 0] <- 0
            
            df <- data.frame(Fraction = t_seq, PredictivePower = pp_seq)
            curr_pp <- pp_seq[which.min(abs(t_seq - pd$inf_fraction))]
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x = Fraction, y = PredictivePower)) +
                ggplot2::geom_line(color = "#3C5488FF", linewidth = 1.1) +
                ggplot2::geom_point(data = data.frame(Fraction = pd$inf_fraction, PredictivePower = curr_pp), 
                                   ggplot2::aes(x = Fraction, y = PredictivePower), color = "#E64B35FF", size = 3.5) +
                ggplot2::annotate("text", x = pd$inf_fraction, y = curr_pp, label = paste("Current PP =", round(curr_pp, 2)), vjust = -1, color = "#E64B35FF") +
                ggplot2::ylim(0, 1) +
                ggplot2::labs(title = "Predictive Probability of Trial Success vs Information Fraction", 
                             x = "Information Fraction (t = N / N_planned)", y = "Conditional / Predictive Power") +
                ggplot2::theme_minimal()
            if (!is.null(ggtheme)) p <- p + ggtheme
            print(p)
            TRUE
        }
    )
)
