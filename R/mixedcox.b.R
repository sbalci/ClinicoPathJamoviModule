#' @title Mixed-Effects Cox Regression Implementation
#' @description
#' Backend implementation class for Cox proportional hazards regression with
#' random effects for clustered survival data. This R6 class provides comprehensive
#' functionality for mixed-effects survival analysis accounting for correlation
#' within clusters using the coxme package.
#' 
#' @details
#' The mixedcoxClass implements mixed-effects Cox regression methods with:
#' 
#' \strong{Random Effects Modeling:}
#' - Random intercepts for cluster-specific baseline hazards
#' - Random slopes for cluster-specific covariate effects
#' - Nested clustering structures (e.g., patients within hospitals)
#' 
#' \strong{Clinical Applications:}
#' - Multi-center clinical trials with hospital effects
#' - Recurrent events analysis with patient clustering
#' - Family-based survival studies with genetic clustering
#' - Longitudinal survival data with repeated measurements
#' 
#' \strong{Statistical Features:}
#' - Variance components estimation for random effects
#' - Approximate variance fraction on the latent log-hazard scale
#' - Likelihood-ratio comparison with a standard Cox model
#' 
#' @seealso \code{\link{mixedcox}} for the main user interface function
#' @importFrom R6 R6Class
#' @import jmvcore
#' @keywords internal
#' @return An \code{R6} class generator object for the \code{mixedcoxClass} backend; used internally by the jamovi analysis wrapper and not called directly.

mixedcoxClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "mixedcoxClass",
    inherit = mixedcoxBase,
    private = list(
      
      # Model objects and results storage
      .coxme_model = NULL,
      .standard_cox = NULL,
      
      # Constants for analysis
      MIN_CLUSTERS = 5,
      MIN_OBS_PER_CLUSTER = 2,
      
      # Core initialization method
      .init = function() {
        # Initialize results with informative messages
        self$results$todo$setContent(
          paste0(
            "<h3>Mixed-Effects Cox Regression</h3>",
            "<p>This analysis requires:</p>",
            "<ul>",
            "<li><b>Time variable:</b> Follow-up time or dates</li>",
            "<li><b>Outcome variable:</b> Event indicator</li>",
            "<li><b>Clustering variable:</b> Variable defining clusters</li>",
            "<li><b>Fixed effects:</b> Variables for population-level effects</li>",
            "</ul>",
            "<p>Select variables to begin mixed-effects Cox regression analysis.</p>"
          )
        )
        
        # Early return if no data
        if (is.null(self$data) || nrow(self$data) == 0) {
          return()
        }
        
        # Validate minimum required inputs
        validation <- private$.validateInputs()
        if (!validation$valid) {
          return()
        }
      },
      
      # Main analysis execution
      .run = function() {
        # Early validation
        validation <- private$.validateInputs()
        if (!validation$valid) {
          return()
        }
        
        # Clear todo message
        self$results$todo$setContent("")
        
        # Check package availability
        if (!requireNamespace("coxme", quietly = TRUE)) {
          self$results$todo$setContent(
            "<h3>Package Required</h3><p>The 'coxme' package is required for mixed-effects Cox regression.</p>"
          )
          return()
        }
        
        # Prepare data and fit. When either step stops, blank the model outputs so a
        # previous run's results are not shown next to the new message.
        prepared_data <- private$.prepareData()
        if (is.null(prepared_data)) return(private$.clearModelOutputs())
        
        model_results <- private$.fitMixedCox(prepared_data)
        if (is.null(model_results)) return(private$.clearModelOutputs())
        
        # Fit standard Cox model for comparison
        if (self$options$show_model_comparison) {
          standard_results <- private$.fitStandardCox(prepared_data)
          private$.standard_cox <- standard_results
          if (is.null(standard_results)) {
            self$results$modelComparison$setContent(
              "<p>Standard Cox fit unavailable; model comparison could not be calculated.</p>"
            )
          }
        }
        
        # Display results
        private$.displayResults(model_results, prepared_data)
        
      },

      # Blank every model output (used when a run stops before a model is fitted)
      .clearModelOutputs = function() {
        self$results$modelSummary$setContent("")
        self$results$fixedEffectsTable$deleteRows()
        self$results$randomEffectsSummary$setContent("")
        self$results$modelComparison$setContent("")
      },
      
      # Input validation
      .validateInputs = function() {
        has_time <- !is.null(self$options$elapsedtime) || 
                   (self$options$tint && !is.null(self$options$dxdate) && !is.null(self$options$fudate))
        has_outcome <- !is.null(self$options$outcome)
        has_cluster <- !is.null(self$options$cluster_var)
        has_fixed <- !is.null(self$options$fixed_effects) || !is.null(self$options$continuous_effects)
        
        result <- list(
          valid = has_time && has_outcome && has_cluster && has_fixed,
          has_time = has_time,
          has_outcome = has_outcome,
          has_cluster = has_cluster,
          has_fixed = has_fixed
        )
        
        if (!result$valid) {
          missing_items <- c()
          if (!has_time) missing_items <- c(missing_items, "Time variable")
          if (!has_outcome) missing_items <- c(missing_items, "Outcome variable")
          if (!has_cluster) missing_items <- c(missing_items, "Clustering variable")
          if (!has_fixed) missing_items <- c(missing_items, "Fixed effects variables")
          
          self$results$todo$setContent(paste0(
            "<h3>Missing Required Variables</h3>",
            "<p>Please specify: ", paste(missing_items, collapse = ", "), "</p>"
          ))
        }
        
        return(result)
      },
      
      # Prepare data for analysis
      .prepareData = function() {
        tryCatch({
          data <- self$data
          opts <- self$options
          
          # Survival time, from the time variable or from dates
          if (opts$tint) {
            time <- private$.calculateTimeFromDates()
            if (is.null(time)) return(NULL)
          } else {
            time <- data[[opts$elapsedtime]]
          }
          event <- data[[opts$outcome]] == opts$outcomeLevel

          # Random slope variable (slope models only)
          slope_var <- NULL
          if (opts$random_effects != "intercept") {
            slope_var <- opts$random_slope_var
            if (is.null(slope_var)) {
              self$results$todo$setContent(
                "<h3>Missing Random Slope Variable</h3><p>Please specify a variable for random slopes.</p>"
              )
              return(NULL)
            }
          }
          
          notes <- character()
          fixed_vars <- c(opts$fixed_effects, opts$continuous_effects)
          # Without its fixed effect a random slope is centred on 0, and the slope
          # variance absorbs the population slope.
          if (!is.null(slope_var) && !(slope_var %in% fixed_vars)) {
            fixed_vars <- c(fixed_vars, slope_var)
            notes <- c(notes, paste0(
              "The random-slope variable '", slope_var, "' was added to the fixed effects, ",
              "so the cluster slopes vary around a population slope."
            ))
          }
          
          # Nesting is built only for a random intercept; otherwise it is ignored and
          # takes no part in the complete-case filter.
          nested_var <- NULL
          if (opts$nested_clustering) {
            if (opts$random_effects != "intercept") {
              notes <- c(notes, paste0(
                "Nested clustering is fitted only with a random intercept; it was ignored for ",
                "this model and did not affect which rows were used."
              ))
            } else if (is.null(opts$nested_cluster_var)) {
              notes <- c(notes, paste0(
                "Nested clustering is selected but no higher-level cluster variable was chosen; ",
                "a single-level random intercept was fitted."
              ))
            } else {
              nested_var <- opts$nested_cluster_var
            }
          }
          
          # The model is fitted on internal syntactic names: coxme cannot fit a random
          # slope on a non-syntactic column name, and it reports grouping names in
          # make.names() form. The "v<i>_" names are prefix-free, so a coefficient name
          # maps back to its variable with startsWith().
          fixed_ids <- paste0("v", seq_along(fixed_vars), "_")
          model_data <- data[c(fixed_vars, opts$cluster_var, nested_var)]
          names(model_data) <- c(fixed_ids, "cl_", if (!is.null(nested_var)) "nest_")

          slope_id <- if (!is.null(slope_var)) fixed_ids[match(slope_var, fixed_vars)]
          # Numeric-only variables (jmvcore rejects other types): an integer column with
          # value labels arrives as a factor carrying 'values', which toNumeric() unwraps.
          for (id in fixed_ids[fixed_vars %in% c(opts$continuous_effects, slope_var)]) {
            model_data[[id]] <- jmvcore::toNumeric(model_data[[id]])
          }

          # TODO (jamovify): consider `jmvcore::naOmit(model_data)` instead of `complete.cases` +
          # boolean indexing - preserves jamovi column attributes (measureType, values, labels) that
          # downstream coxme/survival modeling may rely on for labelled-factor handling.
          complete_rows <- complete.cases(model_data) & !is.na(time) & !is.na(event)
          
          if (sum(complete_rows) < 50) {  # Minimum for mixed-effects models
            self$results$todo$setContent(
              "<h3>Insufficient Data</h3><p>Too few complete observations for mixed-effects Cox regression.</p>"
            )
            return(NULL)
          }
          
          # Clusters are counted after complete-case removal. droplevels(): an unused
          # level is not a cluster, and an empty factor level stops coxme. A nested
          # cluster is a (higher level, cluster) pair, as coxme fits it.
          model_data <- droplevels(model_data[complete_rows, , drop = FALSE])
          cluster_sizes <- table(if (is.null(nested_var)) model_data$cl_ else
            interaction(model_data$nest_, model_data$cl_, drop = TRUE))

          if (length(cluster_sizes) < private$MIN_CLUSTERS) {
            self$results$todo$setContent(paste0(
              "<h3>Insufficient Clusters</h3><p>Need at least ", private$MIN_CLUSTERS,
              " clusters with complete data for mixed-effects modeling (found ",
              length(cluster_sizes), ").</p>"
            ))
            return(NULL)
          }

          small_clusters <- sum(cluster_sizes < private$MIN_OBS_PER_CLUSTER)
          if (small_clusters > length(cluster_sizes) * 0.5) {
            self$results$todo$setContent(
              "<h3>Small Clusters Warning</h3><p>Many clusters have very few observations. Consider combining clusters.</p>"
            )
          }
          
          return(list(
            data = model_data,
            surv = survival::Surv(time[complete_rows], event[complete_rows]),
            fixed_vars = fixed_vars,
            fixed_ids = fixed_ids,
            slope_var = slope_var,
            slope_id = slope_id,
            nested_var = nested_var,
            notes = notes,
            n_obs = sum(complete_rows),
            n_clusters = length(cluster_sizes)
          ))
          
        }, error = function(e) {
          # TODO (UX): file-wide pattern - `.prepareData` / `.calculateTimeFromDates` / `.fitMixedCox`
          # surface validation + runtime errors by writing raw HTML into `self$results$todo`. This
          # mixes the "instructions" surface with the "error" surface and bypasses jamovi's structured
          # error UI. Prefer `jmvcore::reject(...)` for user-facing failures so the analysis is marked
          # errored (consistent icon, log path, syntax-mode behavior). Keep the todo surface for the
          # initial "fill in these variables" guidance only. (Security note: every `e$message`
          # interpolation goes through `htmltools::htmlEscape`, so XSS is closed; the migration is
          # UX-only.)
          self$results$todo$setContent(paste0(
            "<h3>Data Preparation Error</h3><p>", htmltools::htmlEscape(e$message), "</p>"
          ))
          return(NULL)
        })
      },

      # Calculate time from dates
      .calculateTimeFromDates = function() {
        tryCatch({
          dx_dates <- self$data[[self$options$dxdate]]
          fu_dates <- self$data[[self$options$fudate]]
          
          # Parse dates based on format
          format_map <- list(
            "ymd" = "%Y-%m-%d",
            "mdy" = "%m/%d/%Y", 
            "dmy" = "%d/%m/%Y"
          )
          
          format_str <- format_map[[self$options$timetypedata]]
          
          dx_parsed <- as.Date(dx_dates, format = format_str)
          fu_parsed <- as.Date(fu_dates, format = format_str)
          
          if (any(is.na(dx_parsed)) || any(is.na(fu_parsed))) {
            self$results$todo$setContent(
              "<h3>Date Parse Error</h3><p>Unable to parse dates. Check date format.</p>"
            )
            return(NULL)
          }
          
          time_diff <- as.numeric(fu_parsed - dx_parsed)
          
          # Convert to requested output units
          time_units <- self$options$timetypeoutput
          if (time_units == "weeks") {
            time_diff <- time_diff / 7
          } else if (time_units == "months") {
            time_diff <- time_diff / 30.44
          } else if (time_units == "years") {
            time_diff <- time_diff / 365.25
          }
          
          return(time_diff)
          
        }, error = function(e) {
          self$results$todo$setContent(paste0(
            "<h3>Date Calculation Error</h3><p>", htmltools::htmlEscape(e$message), "</p>"
          ))
          return(NULL)
        })
      },
      
      # Random-effect term: (1 | g), (x | g), (1 + x | g) or (1 | h/g)
      .randomTerm = function(slope, cluster, nested = NULL) {
        lhs <- switch(self$options$random_effects,
                      intercept = "1", slope = slope, both = paste("1 +", slope))
        grouping <- if (is.null(nested)) cluster else paste0(nested, "/", cluster)
        paste0("(", lhs, " | ", grouping, ")")
      },

      # Fit mixed-effects Cox model
      .fitMixedCox = function(prepared_data) {
        tryCatch({
          pd <- prepared_data

          # Fitted on the internal names (see .prepareData); shown with the user's names
          surv_obj <- pd$surv
          full_formula <- jmvcore::asFormula(paste(
            "surv_obj ~", paste(pd$fixed_ids, collapse = " + "), "+",
            private$.randomTerm(pd$slope_id, "cl_", if (!is.null(pd$nested_var)) "nest_")
          ))
          display_formula <- paste(
            "Surv(time, event) ~",
            paste(jmvcore::composeTerms(as.list(pd$fixed_vars)), collapse = " + "), "+",
            private$.randomTerm(
              if (!is.null(pd$slope_var)) jmvcore::composeTerm(pd$slope_var),
              jmvcore::composeTerm(self$options$cluster_var),
              if (!is.null(pd$nested_var)) jmvcore::composeTerm(pd$nested_var)
            )
          )
          
          # Fit model
          coxme_fit <- coxme::coxme(
            formula = full_formula,
            data = pd$data,
            control = coxme::coxme.control(
              sparse = if (self$options$sparse_matrix) c(50, 0.02) else c(Inf, 0)
            )
          )
          
          # Fixed effects from fixef()/vcov(); summary.coxme() rounds z to 2 decimals
          beta <- coxme::fixef(coxme_fit)
          coef_names <- names(beta)
          beta <- unname(beta)
          se <- unname(sqrt(diag(as.matrix(stats::vcov(coxme_fit)))))
          z <- beta / se
          ci_half_width <- stats::qnorm(0.975) * se

          # Label each coefficient with its variable (and factor level) as the user named it
          term <- vapply(coef_names, function(nm) which(startsWith(nm, pd$fixed_ids))[1],
                         integer(1), USE.NAMES = FALSE)
          level <- substring(coef_names, nchar(pd$fixed_ids[term]) + 1)
          fixed_effects <- data.frame(
            variable = ifelse(nzchar(level), paste0(pd$fixed_vars[term], ": ", level), pd$fixed_vars[term]),
            coefficient = beta,
            se = se,
            z_statistic = z,
            p_value = 2 * stats::pnorm(-abs(z)),
            hazard_ratio = exp(beta),
            hr_lower = exp(beta - ci_half_width),
            hr_upper = exp(beta + ci_half_width),
            row.names = coef_names,
            stringsAsFactors = FALSE
          )

          variance_components <- coxme::VarCorr(coxme_fit)
          
          # Latent-scale variance fraction (not defined for a random slope alone)
          icc_value <- NULL
          if (self$options$icc_calculation && self$options$random_effects != "slope") {
            icc_value <- private$.calculateICC(variance_components)
          }
          
          # Store results
          private$.coxme_model <- coxme_fit
          
          return(list(
            model = coxme_fit,
            fixed_effects = fixed_effects,
            variance_components = variance_components,
            icc = icc_value,
            formula = display_formula,
            loglik = coxme_fit$loglik,
            n_obs = pd$n_obs,
            n_clusters = pd$n_clusters
          ))
          
        }, error = function(e) {
          self$results$todo$setContent(paste0(
            "<h3>Model Fitting Error</h3><p>", htmltools::htmlEscape(e$message), "</p>"
          ))
          return(NULL)
        })
      },
      
      # Fit standard Cox model for comparison (same rows and fixed effects)
      .fitStandardCox = function(prepared_data) {
        tryCatch({
          surv_obj <- prepared_data$surv
          cox_formula <- jmvcore::asFormula(
            paste("surv_obj ~", paste(prepared_data$fixed_ids, collapse = " + "))
          )
          
          # Fit standard Cox model
          cox_fit <- survival::coxph(cox_formula, data = prepared_data$data)
          
          return(cox_fit)
          
        }, error = function(e) {
          return(NULL)
        })
      },
      
      # Approximate variance fraction on the latent log-hazard scale. With a log-normal
      # frailty b, log Lambda0(T) = -(x'beta + b) + e, where e is standard (minimum)
      # extreme-value with variance pi^2/6 (pi^2/3 is the logistic-model constant).
      .calculateICC = function(variance_components) {
        resid_var <- pi^2 / 6
        if (!is.null(variance_components[["nest_"]])) {
          # (1 | h/g): "nest_/cl_" is the cluster-within-higher-level variance
          inner <- unname(variance_components[["nest_/cl_"]][1])
          outer <- unname(variance_components[["nest_"]][1])
          total <- inner + outer + resid_var
          return(c(same_cluster = (inner + outer) / total, same_higher_level = outer / total))
        }
        s2 <- variance_components[["cl_"]]
        # Intercept variance; with a random slope it is the variance at slope variable = 0
        s2 <- if (is.matrix(s2)) s2[1, 1] else unname(s2[1])
        c(same_cluster = s2 / (s2 + resid_var))
      },
      
      # Display analysis results
      .displayResults = function(model_results, prepared_data) {
        tryCatch({
          opts <- self$options
          cluster_name <- htmltools::htmlEscape(opts$cluster_var)
          nested_name <- if (!is.null(prepared_data$nested_var)) htmltools::htmlEscape(prepared_data$nested_var)
          slope_name <- if (!is.null(prepared_data$slope_var)) htmltools::htmlEscape(prepared_data$slope_var)

          # Model summary
          model_text <- paste0(
            "<h3>Mixed-Effects Cox Regression Results</h3>",
            # The display formula embeds user column names; HTML-escape before render.
            "<p><b>Model:</b> ", htmltools::htmlEscape(model_results$formula), "</p>",
            "<p><b>Observations:</b> ", model_results$n_obs, "</p>",
            "<p><b>Clusters:</b> ", model_results$n_clusters, "</p>",
            "<p><b>Integrated log partial likelihood:</b> ",
            sprintf("%.4f", model_results$loglik[["Integrated"]]), "</p>"
          )
          
          icc <- model_results$icc
          if (!is.null(icc)) {
            if (length(icc) == 2) {
              fraction_text <- paste0(
                sprintf("%.4f", icc[["same_cluster"]]), " for the same ", cluster_name, "; ",
                sprintf("%.4f", icc[["same_higher_level"]]), " for the same ", nested_name,
                " but a different ", cluster_name
              )
            } else {
              fraction_text <- sprintf("%.4f", icc[["same_cluster"]])
              if (opts$random_effects == "both") {
                fraction_text <- paste0(fraction_text, " (at ", slope_name, " = 0)")
              }
            }
            model_text <- paste0(model_text, 
              "<p><b>Approximate latent-scale variance fraction:</b> ", fraction_text, "</p>",
              "<p><i>Random-effect variance / (random-effect variance + \u03c0\u00b2/6) on the ",
              "latent log-hazard scale, where \u03c0\u00b2/6 is the variance of the standard ",
              "extreme-value error of a proportional-hazards model. This is an approximation, ",
              "not an intracluster correlation of observed event times.</i></p>"
            )
          }

          for (note in prepared_data$notes) {
            model_text <- paste0(model_text, "<p><i>Note: ", htmltools::htmlEscape(note), "</i></p>")
          }
          
          self$results$modelSummary$setContent(model_text)
          
          # Fixed effects table
          if (opts$show_fixed_effects) {
            fixed_table <- self$results$fixedEffectsTable
            fixed_table$deleteRows()
            fixed_effects <- model_results$fixed_effects
            
            for (i in seq_len(nrow(fixed_effects))) {
              fixed_table$addRow(
                rowKey = rownames(fixed_effects)[i],
                values = as.list(fixed_effects[i, ])
              )
            }
          }
          
          # Random effects summary
          if (opts$show_random_effects) {
            random_text <- "<h3>Random Effects Variance Components</h3>"
            variance_components <- model_results$variance_components
            for (component in names(variance_components)) {
              var_comp <- variance_components[[component]]
              group <- switch(component,
                              cl_ = cluster_name,
                              nest_ = nested_name,
                              "nest_/cl_" = paste0(cluster_name, " within ", nested_name),
                              htmltools::htmlEscape(component))

              if (is.matrix(var_comp)) {
                # (1 + x | g): variances on the diagonal, correlation off the diagonal
                random_text <- paste0(random_text, 
                  "<p><b>", group, " (intercept variance):</b> ", signif(var_comp[1, 1], 4), "</p>",
                  "<p><b>", group, " (slope variance, ", slope_name, "):</b> ", signif(var_comp[2, 2], 4), "</p>",
                  "<p><b>", group, " (intercept-slope correlation):</b> ", signif(var_comp[1, 2], 4), "</p>"
                )
              } else {
                what <- if (identical(names(var_comp), prepared_data$slope_id)) {
                  paste0("slope variance, ", slope_name)
                } else {
                  "intercept variance"
                }
                random_text <- paste0(random_text, 
                  "<p><b>", group, " (", what, "):</b> ", signif(unname(var_comp), 4), "</p>"
                )
              }
            }
            
            self$results$randomEffectsSummary$setContent(random_text)
          }
          
          # Model comparison (integrated log partial likelihood vs Cox, same rows and fixed effects)
          if (opts$show_model_comparison && !is.null(private$.standard_cox)) {
            ll_cox <- private$.standard_cox$loglik[2]
            ll_mixed <- model_results$loglik[["Integrated"]]
            # With a variance at its boundary the Laplace-approximated integrated
            # likelihood can fall a hair below the Cox fit; the statistic is truncated at 0.
            lr_stat <- max(0, 2 * (ll_mixed - ll_cox))

            n_re_params <- switch(opts$random_effects,
                                  intercept = if (is.null(prepared_data$nested_var)) 1 else 2,
                                  slope = 1,
                                  both = 3)
            if (n_re_params == 1) {
              # One variance tested at its boundary of 0: 50:50 mixture of chi-square(0)
              # and chi-square(1) (Self & Liang 1987)
              p_value <- 0.5 * stats::pchisq(lr_stat, df = 1, lower.tail = FALSE)
              p_text <- paste0(
                "<p><b>Boundary-corrected p-value:</b> ",
                if (p_value < 0.001) "&lt; 0.001" else sprintf("%.3f", p_value),
                " (half the \u03c7\u00b2(1) tail probability, because the variance is tested ",
                "at its boundary of 0)</p>"
              )
            } else {
              p_text <- paste0(
                "<p><i>No p-value: this model has ", n_re_params, " random-effect parameters, so ",
                "without clustering the statistic follows a non-standard chi-square mixture; a ",
                "chi-square(1) p-value would be miscalibrated.</i></p>"
              )
            }
            
            comparison_text <- paste0(
              "<h3>Model Comparison</h3>",
              "<p><b>Standard Cox log partial likelihood:</b> ", sprintf("%.4f", ll_cox), "</p>",
              "<p><b>Mixed-effects Cox integrated log partial likelihood:</b> ", sprintf("%.4f", ll_mixed), "</p>",
              "<p><b>Likelihood-ratio statistic:</b> ", sprintf("%.4f", lr_stat),
              " (2 \u00d7 log-likelihood difference, truncated at 0)</p>",
              p_text
            )
            
            self$results$modelComparison$setContent(comparison_text)
          }
          
        }, error = function(e) {
          # Surface display errors; a silent handler once hid a failing column lookup
          self$results$todo$setContent(paste0(
            "<h3>Display Error</h3><p>", htmltools::htmlEscape(e$message), "</p>"
          ))
        })
      }
    )
  )
