#' @title Analysis Without Gold Standard
#' @importFrom R6 R6Class
#' @import jmvcore

nogoldstandardClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "nogoldstandardClass",
    inherit = nogoldstandardBase,
    private = list(

        .init = function() {
                if (is.null(self$options$test1) || is.null(self$options$test2))
                    return()

                table <- self$results$test_metrics
                tests <- private$.getTestVariables()
                for (test in tests) {
                    table$addRow(rowKey=test, values=list(
                        test = test
                    ))
                }
            },

        .getTestVariables = function() {
                vars <- c()
                for (i in 1:5) {
                    var_name <- paste0("test", i)
                    if (!is.null(self$options[[var_name]])) {
                        vars <- c(vars, self$options[[var_name]])
                    }
                }
                return(vars)
            },

        .run = function() {
                if (is.null(self$options$test1) || is.null(self$options$test2)) {
                    return()
                }

                if (nrow(self$data) == 0) {
                    stop('Data contains no rows')
                }

                # Get test variables and their positive levels
                tests <- list()
                test_levels <- list()

                for (i in 1:5) {
                    var_name <- paste0("test", i)
                    level_name <- paste0("test", i, "Positive")

                    if (!is.null(self$options[[var_name]])) {
                        test_var <- jmvcore::constructFormula(terms = self$options[[var_name]])
                        test_var <- jmvcore::decomposeFormula(formula = test_var)
                        test_var <- unlist(test_var)

                        pos_level <- self$options[[level_name]]

                        if (!is.null(pos_level)) {
                            tests[[length(tests) + 1]] <- test_var
                            test_levels[[length(test_levels) + 1]] <- pos_level
                        }
                    }
                }

                # Data preparation
                data <- self$data
                test_data <- data[unlist(tests)]
                test_data <- jmvcore::naOmit(test_data)

                if (nrow(test_data) == 0) {
                    stop('No complete cases available')
                }

                # Store original data
                # self$results$text1$setContent(list(
                #     test_data = test_data
                # ))

                # Convert to LCA format
                lca_data <- data.frame(matrix(nrow=nrow(test_data), ncol=length(tests)))
                names(lca_data) <- unlist(tests)

                # Convert each variable
                for (i in seq_along(tests)) {
                    test_name <- tests[[i]]
                    pos_level <- test_levels[[i]]

                    var <- test_data[[test_name]]
                    lca_data[[test_name]] <- factor(
                        var == pos_level,
                        levels = c(FALSE, TRUE),
                        labels = c("no", "yes")
                    )

                    # Check for variation
                    if (length(table(lca_data[[test_name]])) < 2) {
                        stop(sprintf("Variable '%s' shows no variation", test_name))
                    }
                }

                # Store processed data
                # self$results$text2$setContent(list(
                #     lca_data = lca_data,
                #     tables = lapply(lca_data, table)
                # ))

                # Run LCA
                results <- private$.runLCA(lca_data)


                # html <- self$results$todo
                # html$setContent(list(
                #     results
                # )
                # )


                # Update results
                if (!is.null(results)) {
                    private$.populatePrevalence(results)
                    private$.populateTestMetrics(results)
                    private$.populateModelFit(results)
                }
            },

        .populatePrevalence = function(model) {
                if(is.null(model))
                    return()

                # Model$P contains class membership probabilities
                prevalence <- model$P[2]  # Second class probability

                table <- self$results$prevalence
                table$setRow(rowNo=1, values=list(
                    estimate = prevalence,
                    ci_lower = NA,
                    ci_upper = NA
                ))

                # html <- self$results$todo
                # html$setContent(list(
                #     model,
                #     model$P[2],
                #     model$P[1],
                #     prevalence = prevalence
                # )
                # )

                # Debug output
                message("\nPrevalence values:")
                print(model$P)
            },

        .populateTestMetrics = function(model) {
                if(is.null(model))
                    return()

                # Get test variables
                tests <- private$.getTestVariables()

                # Extract probabilities
                probs <- model$probs

                # Debug output
                message("\nProbability matrices:")
                print(probs)

                # For each test
                for (i in seq_along(tests)) {
                    # Extract probability matrix for this test
                    test_probs <- probs[[i]]

                    # Debug output
                    message("\nTest ", i, " probabilities:")
                    print(test_probs)

                    # Calculate sensitivity and specificity using matrix indices
                    # test_probs[2,2] is P(positive|class 2) - sensitivity
                    # test_probs[1,1] is P(negative|class 1) - specificity
                    sensitivity <- test_probs[2,2]  # P(yes|class 2)
                    specificity <- test_probs[1,1]  # P(no|class 1)

                    message("\nTest ", i, " metrics:")
                    message("Sensitivity: ", sensitivity)
                    message("Specificity: ", specificity)

                    # Update table
                    self$results$test_metrics$setRow(
                        rowKey=tests[i],
                        values=list(
                            test = tests[i],
                            sensitivity = sensitivity,
                            specificity = specificity,
                            sens_ci_lower = NA,
                            sens_ci_upper = NA,
                            spec_ci_lower = NA,
                            spec_ci_upper = NA
                        )
                    )
                }
            },

        .populateModelFit = function(model) {
                if(is.null(model))
                    return()

                table <- self$results$model_fit

                # Add basic fit statistics
                fit_stats <- list(
                    BIC = model$bic,
                    AIC = model$aic,
                    "Log-Likelihood" = model$llik,
                    "Number of Iterations" = model$numiter,
                    "Chi-Square" = model$Chisq
                )

                # Debug output
                message("\nModel fit statistics:")
                print(fit_stats)

                # Add each available statistic to table
                for (name in names(fit_stats)) {
                    if (!is.null(fit_stats[[name]])) {
                        table$addRow(rowKey=name, values=list(
                            statistic = name,
                            value = fit_stats[[name]]
                        ))
                    }
                }
            },

        .runLCA = function(lca_data) {
                if (!requireNamespace("poLCA", quietly = TRUE)) {
                    stop("Package 'poLCA' is required for latent class analysis")
                }

                # Create formula
                var_names <- names(lca_data)
                f <- stats::as.formula(paste("cbind(", paste(var_names, collapse=","), ")~1"))

                # Debug info
                message("\nLCA Analysis Data:")
                message("Formula: ", deparse(f))
                message("\nData structure:")
                str(lca_data)
                message("\nData summary:")
                for(var in names(lca_data)) {
                    message("\nTable for ", var, ":")
                    print(table(lca_data[[var]]))
                }

                tryCatch({
                    model <- poLCA::poLCA(
                        formula = f,
                        data = lca_data,
                        nclass = 2,
                        maxiter = 1000,
                        graphs = FALSE,
                        verbose = TRUE  # Set to TRUE for debugging
                    )

                    if (is.null(model)) {
                        stop("Model fitting failed - returned NULL")
                    }

                    message("\nModel output structure:")
                    str(model)
                    message("\nProbability matrices:")
                    print(model$probs)


                    # html <- self$results$todo2
                    # html$setContent(list(
                    #     lca_data,
                    #     var_names,
                    #     f,
                    #     model = model,
                    #     unlistmodel = unlist(model)
                    # )
                    # )



                    return(model)

                }, error = function(e) {
                    message("\nLCA Error:")
                    message(e$message)
                    message("\nInput data:")
                    print(head(lca_data))
                    return(NULL)
                })
            },

        .runComposite = function(test_data) {
            # Create composite reference from majority vote
            composite <- rowMeans(test_data, na.rm = TRUE) >= 0.5

            # Calculate metrics for each test
            results <- list(
                metrics = lapply(seq_along(test_data), function(i) {
                    test_result <- test_data[[i]] == 1
                    tp <- sum(test_result & composite, na.rm = TRUE)
                    tn <- sum(!test_result & !composite, na.rm = TRUE)
                    fp <- sum(test_result & !composite, na.rm = TRUE)
                    fn <- sum(!test_result & composite, na.rm = TRUE)

                    list(
                        sensitivity = tp/(tp + fn),
                        specificity = tn/(tn + fp)
                    )
                })
            )

            names(results$metrics) <- names(test_data)
            results$prevalence <- mean(composite, na.rm = TRUE)

            return(results)
        },

        .runBayesian = function(test_data) {
            if (!requireNamespace("rjags", quietly = TRUE)) {
                stop("Package 'rjags' is required for Bayesian analysis")
            }

            # Define JAGS model
            model_string <- "
            model {
                # Priors for prevalence
                prev ~ dbeta(1, 1)

                # Priors for sensitivities and specificities
                for(j in 1:n_tests) {
                    sens[j] ~ dbeta(1, 1)
                    spec[j] ~ dbeta(1, 1)
                }

                # Likelihood
                for(i in 1:n_subj) {
                    disease[i] ~ dbern(prev)
                    for(j in 1:n_tests) {
                        y[i,j] ~ dbern(p[i,j])
                        p[i,j] <- disease[i] * sens[j] + (1-disease[i]) * (1-spec[j])
                    }
                }
            }
            "

            # Prepare data for JAGS
            jags_data <- list(
                y = as.matrix(test_data),
                n_subj = nrow(test_data),
                n_tests = ncol(test_data)
            )

            # Run MCMC
            tryCatch({
                # Initialize model
                model <- rjags::jags.model(
                    textConnection(model_string),
                    data = jags_data,
                    n.chains = 3
                )

                # Burn-in
                update(model, 1000)

                # Sample from posterior
                samples <- rjags::coda.samples(
                    model,
                    variable.names = c("prev", "sens", "spec"),
                    n.iter = 5000
                )

                # Process results
                combined_chains <- do.call(rbind, samples)
                results <- list(
                    prevalence = mean(combined_chains[, "prev"]),
                    sensitivities = colMeans(combined_chains[, grep("sens", colnames(combined_chains))]),
                    specificities = colMeans(combined_chains[, grep("spec", colnames(combined_chains))])
                )

                return(results)

            }, error = function(e) {
                message("Bayesian analysis failed:")
                print(e)
                return(list(
                    prevalence = NA,
                    sensitivities = rep(NA, ncol(test_data)),
                    specificities = rep(NA, ncol(test_data))
                ))
            })
        },

        .bootstrapCI = function(point_estimate) {
            # Simple percentile bootstrap CI
            alpha <- self$options$alpha
            ci <- list(
                lower = max(0, point_estimate - 1.96 * sqrt(point_estimate * (1-point_estimate) / self$options$nboot)),
                upper = min(1, point_estimate + 1.96 * sqrt(point_estimate * (1-point_estimate) / self$options$nboot))
            )
            return(ci)
        },

        .plot = function(image, ggtheme, ...) {
            if (is.null(image$state))
                return(FALSE)

            test_data <- image$state$data
            original_names <- image$state$names

            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                stop("Package 'ggplot2' is required for plotting")
            }

            # Convert data to long format for plotting
            plot_data <- data.frame(
                test = rep(original_names, each = nrow(test_data)),
                result = factor(unlist(test_data), levels = c(0, 1),
                                labels = c("Negative", "Positive"))
            )

            # Create plot
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = test, fill = result)) +
                ggplot2::geom_bar(position = "fill") +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::labs(y = "Proportion", x = "Test", fill = "Result") +
                ggplot2::theme_minimal() +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

            print(p)
            TRUE
        }


            )
)

