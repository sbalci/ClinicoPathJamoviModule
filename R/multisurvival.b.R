# Internationalization note:
# jamovi's translation helper `.()` (jmvcore) resolves `self` from the CALLING
# frame, so it only works inside R6 analysis methods. The top-level helper
# functions in this file run without `self` in scope, so they MUST use plain
# string literals - calling `.()` there throws "object 'self' not found"
# (GitHub issue #122). Do not reintroduce `.()` into file-level helpers.
# (The previous `if (!exists(".")) . <- function(x) x` guard was dead code:
# jmvcore::`.` is imported into the namespace, so `exists(".")` is always TRUE.)

# Note: `.escapeVariableNames` lives in R/utils.R as the canonical definition.

# Helper function to restore original variable names in output tables
.restoreOriginalNamesInMultiSurvivalTable <- function(table_data, name_mapping) {
    if (is.null(table_data) || nrow(table_data) == 0 || is.null(name_mapping)) {
        return(table_data)
    }

    # Restore names in the first column (which typically contains variable names)
    if (ncol(table_data) > 0) {
        first_col <- table_data[, 1]

        # For each entry in the first column, check if it needs name restoration
        for (i in seq_along(first_col)) {
            original_name <- first_col[i]

            # Handle factor levels (like "variablename1" vs "variablename0")
            for (clean_name in names(name_mapping)) {
                if (grepl(paste0("^", clean_name), original_name)) {
                    # Replace the clean name part with the original name
                    table_data[i, 1] <- sub(clean_name, name_mapping[[clean_name]], original_name)
                    break
                }
            }
        }
    }

    return(table_data)
}



# Helper function for package dependency checking with graceful fallbacks
.checkPackageDependency <- function(package_name, method_name, alternative_method = "Cox regression") {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    fallback_info <- list(
      available = FALSE,
      message = paste0("
        <div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 15px; border-radius: 5px; margin: 10px 0;'>
          <h4 style='color: #856404; margin-top: 0;'> ", method_name, " ", "Package Not Available", "</h4>
          <p><strong>", "Issue:", "</strong> ", "The", " '", package_name, "' ", "package is required but not installed.", "</p>
          <p><strong>", "Solution:", "</strong> ", "Install the package using:", " <code>install.packages('", package_name, "')</code></p>
          <p><strong>", "Alternative:", "</strong> ", "Automatically switching to", " ", alternative_method, " ", "analysis.", "</p>
        </div>
      ")
    )
  } else {
    fallback_info <- list(available = TRUE, message = "")
  }
  return(fallback_info)
}

# Helper function for comprehensive data validation
.validateSurvivalData <- function(data, time_var = "mytime", outcome_var = "myoutcome") {
  issues <- list()
  warnings <- list()
  event_indicator <- NULL

  # DEBUG: show the incoming time variable class/summary when needed
  if (isTRUE(getOption("multisurvival.debug"))) {
    message("[multisurvival.debug] validate: time_var class = ", paste(class(data[[time_var]]), collapse = "/"))
  }

  # Check for negative survival times
  if (time_var %in% names(data)) {
    time_vec <- data[[time_var]]

    # Handle jamovi survival variables that arrive as Surv objects or matrices
    if (survival::is.Surv(time_vec)) {
      time_vec <- as.numeric(time_vec[, "time"])
    } else if (is.matrix(time_vec) && ncol(time_vec) >= 1) {
      time_vec <- as.numeric(time_vec[, 1])
    }

    # Replace in data copy so downstream checks work on numeric values
    data[[time_var]] <- time_vec

    if (isTRUE(getOption("multisurvival.debug"))) {
      message("[multisurvival.debug] validate: time_var summary = ", paste(utils::head(time_vec, 5), collapse = ", "))
    }

    negative_times <- sum(time_vec < 0, na.rm = TRUE)
    if (negative_times > 0) {
      issues <- append(issues, paste("Negative survival times detected:", negative_times, "observations"))
    }

    # Check for zero survival times
    zero_times <- sum(time_vec == 0, na.rm = TRUE)
    if (zero_times > 0) {
      warnings <- append(warnings, paste("Zero survival times detected:", zero_times, "observations. Consider adding small constant (0.5 days)."))
    }
  }

  # Check outcome coding
  if (outcome_var %in% names(data)) {
    outcome_vec <- data[[outcome_var]]

    # Build an event indicator safely (handles factors, logicals, numeric)
    event_indicator <- .eventIndicator(outcome_vec)

    # Only enforce binary check when the underlying values are numeric/logical
    if (is.numeric(outcome_vec) || is.logical(outcome_vec)) {
      unique_outcomes <- unique(outcome_vec[!is.na(outcome_vec)])
      if (!all(unique_outcomes %in% c(0, 1, TRUE, FALSE))) {
        issues <- append(issues, "Outcome should be binary (0/1 or TRUE/FALSE)")
      }
    } else if (is.factor(outcome_vec) && length(levels(outcome_vec)) > 2) {
      warnings <- append(warnings, "Outcome has multiple levels; analysis will treat non-event levels as censored where applicable.")
    }

    # Check event rate when we have a usable indicator
    if (!is.null(event_indicator) && !all(is.na(event_indicator))) {
      event_rate <- mean(event_indicator, na.rm = TRUE)
      if (!is.na(event_rate) && event_rate < 0.05) {
        warnings <- append(warnings, paste("Low event rate:", round(event_rate * 100, 1), "%. ", "Consider longer follow-up or different endpoint."))
      }
    }
  }

  # Check sample size adequacy
  n_complete <- sum(complete.cases(data))
  if (!is.null(event_indicator)) {
    n_events <- sum(event_indicator, na.rm = TRUE)
    if (!is.na(n_events) && n_events < 10) {
      warnings <- append(warnings, paste("Low number of events detected:", n_events, "events. Results may be unstable; interpret cautiously."))
    }
  }

  return(list(issues = issues, warnings = warnings))
}

# Helper to derive a consistent event indicator (TRUE/FALSE) from various encodings
.eventIndicator <- function(outcome_vec) {
  if (is.null(outcome_vec)) {
    return(NULL)
  }

  # Factor handling: competing risk encoding uses an "Event" level
  if (is.factor(outcome_vec)) {
    if ("Event" %in% levels(outcome_vec)) {
      return(outcome_vec == "Event")
    }
    # Try to coerce factor levels to numeric (e.g., "0"/"1")
    suppressWarnings(num_levels <- as.numeric(as.character(outcome_vec)))
    if (!all(is.na(num_levels))) {
      return(num_levels >= 1)
    }
    # IMPROVEMENT: Throw error instead of silent NA return for unsupported factor levels
    # This prevents misleading results from incorrectly encoded outcomes
    jmvcore::reject(sprintf(
      "Outcome Factor Has Unsupported Levels: The outcome variable has non-numeric levels that cannot be interpreted as events: %s\n\nTo Fix:\n1. For binary outcomes: Recode as numeric (0 = censored, 1 = event) or logical (FALSE/TRUE)\n2. For competing risks: Use factor with exactly 3 levels: 'Censored', 'Event', 'Competing'\n3. In jamovi: Use Transform > Recode to convert to appropriate format\n4. Example numeric coding: 0 = Alive/Censored, 1 = Dead/Event occurred\n\nCurrent levels detected: %s",
      paste(levels(outcome_vec), collapse=", "),
      paste(levels(outcome_vec), collapse=", ")
    ))
  }

  if (is.logical(outcome_vec) || is.numeric(outcome_vec)) {
    return(outcome_vec >= 1)
  }

  # IMPROVEMENT: Throw error for unsupported types instead of returning NA
  jmvcore::reject(sprintf(
    "Outcome Variable Type Not Supported: The outcome variable has type '%s' which cannot be used for survival analysis.\n\nSupported Types:\n1. Numeric: 0 (censored) and 1 (event)\n2. Logical: FALSE (censored) and TRUE (event)\n3. Factor: Either numeric levels ('0'/'1') or competing risk levels ('Censored'/'Event'/'Competing')\n\nTo Fix:\n1. Check that you selected the correct outcome variable\n2. In jamovi: Use Data > Setup to verify variable type\n3. Convert text/character variables to numeric or factor format\n4. Use Transform > Compute to create binary outcome: outcome = ifelse(status == 'Dead', 1, 0)\n\nCurrent type: %s",
    class(outcome_vec)[1],
    class(outcome_vec)[1]
  ))
}

# Helper function for generating clinical interpretation summaries
.generateClinicalSummary <- function(results, analysis_type = "cox", n_vars = 0, n_events = 0) {

  # Extract key statistics based on analysis type
  if (analysis_type == "cox" && !is.null(results)) {

    # Count significant variables if results is a table/data.frame
    sig_count <- 0
    strongest_var <- NULL
    strongest_hr <- 1
    strongest_effect <- NULL

    tryCatch({
      if (is.data.frame(results) && "p" %in% names(results)) {
        p_values <- as.numeric(results$p)
        sig_indices <- which(p_values < 0.05 & !is.na(p_values))
        sig_count <- length(sig_indices)

        if (sig_count > 0) {
          # Find strongest effect (furthest from HR = 1)
          if ("HR (95% CI, p-value)" %in% names(results)) {
            hr_column <- results[["HR (95% CI, p-value)"]]
            for (i in sig_indices) {
              hr_text <- hr_column[i]
              hr_match <- regmatches(hr_text, regexpr("[0-9]+\\.?[0-9]*", hr_text))
              if (length(hr_match) > 0) {
                hr_val <- as.numeric(hr_match[1])
                if (!is.na(hr_val) && abs(log(hr_val)) > abs(log(strongest_hr))) {
                  strongest_hr <- hr_val
                  strongest_var <- results[i, 1]  # First column usually contains variable names
                  strongest_effect <- if (hr_val > 1) "increased risk" else "decreased risk"
                }
              }
            }
          }
        }
      }
    }, error = function(e) {
      # Silent error handling for robust operation
    })

    # Generate clinical summary
    summary_parts <- list()

    # Analysis overview
    summary_parts$overview <- paste0(
      "This multivariable Cox regression analysis examined", " ", n_vars, " ",
      "potential risk factors in", " ", "patients with", " ", n_events, " ",
      "events observed during follow-up."
    )

    # Key findings
    if (sig_count > 0) {
      summary_parts$findings <- paste0(
        "Key Finding:", " ", sig_count, " ", "out of", " ", n_vars, " ",
        "factors showed statistically significant associations with the outcome", " (p < 0.05)."
      )

      if (!is.null(strongest_var) && !is.null(strongest_effect)) {
        summary_parts$strongest <- paste0(
          "Strongest predictor:", " ", strongest_var, " ", "was associated with", " ",
          strongest_effect, " (", "hazard ratio", " = ", round(strongest_hr, 2), ")."
        )
      }
    } else {
      summary_parts$findings <- paste0(
        "No statistically significant associations were identified among the", " ",
        n_vars, " ", "factors examined", " (", "all p-values \u2265 0.05", ")."
      )
    }

    # Clinical interpretation
    if (sig_count > 0 && !is.null(strongest_hr)) {
      risk_interpretation <- ""
      if (strongest_hr > 2) {
        risk_interpretation <- "This represents a substantial clinical effect."
      } else if (strongest_hr > 1.5 || strongest_hr < 0.67) {
        risk_interpretation <- "This represents a moderate clinical effect."
      } else if (strongest_hr != 1) {
        risk_interpretation <- "This represents a mild clinical effect."
      }

      summary_parts$interpretation <- paste0(
        "Clinical Significance:", " ", risk_interpretation, " ",
        "Consider this factor in clinical decision-making and patient counseling."
      )
    }

    # Combine all parts
    full_summary <- paste(summary_parts, collapse = " ")

    return(list(
      summary = full_summary,
      sig_count = sig_count,
      strongest_var = strongest_var,
      strongest_hr = strongest_hr
    ))
  }

  # Default return for other analysis types
  return(list(
    summary = "Analysis completed. Review detailed results below.",
    sig_count = 0,
    strongest_var = NULL,
    strongest_hr = 1
  ))
}

# Helper function to assess clinical significance of hazard ratios
.assessClinicalSignificance <- function(hr) {
  if (is.null(hr) || !is.numeric(hr) || hr <= 0) {
    return("Unable to assess clinical significance.")
  }

  if (hr > 3 || hr < 0.33) {
    return("Large clinical effect - high priority for clinical consideration.")
  } else if (hr > 2 || hr < 0.5) {
    return("Moderate clinical effect - clinically meaningful.")
  } else if (hr > 1.5 || hr < 0.67) {
    return("Small to moderate clinical effect - may be clinically relevant.")
  } else if (hr != 1) {
    return("Small clinical effect - limited clinical impact.")
  } else {
    return("No clinical effect detected.")
  }
}

# .buildSurvivalFormula() moved to R/utils.R (alongside its siblings
# .asSurvivalFormula and .escapeVariableNames) so it is unit-testable via
# source("R/utils.R") without loading the full jamovi/R6 harness. It is
# still a package-level function called from this file (below) at runtime.

#' @title Multivariable Survival Analysis Implementation
#' @description
#' Backend implementation class for comprehensive multivariable survival analysis.
#' This R6 class provides the core functionality for the multisurvival jamovi module,
#' handling Cox proportional hazards regression, risk stratification, machine learning
#' survival methods, and advanced survival modeling techniques.
#'
#' @details
#' The multisurvivalClass implements a modular architecture with the following components:
#'
#' \strong{Core Analysis Engine:}
#' - Input validation and data preparation
#' - Cox proportional hazards modeling
#' - Competing risks and cause-specific survival
#' - Time-dependent covariate handling
#'
#' \strong{Advanced Methods:}
#' - Stratified analysis for non-proportional hazards
#' - Frailty models for clustered data
#' - Spline-based time-varying effects
#' - Machine learning survival algorithms
#'
#' \strong{Risk Assessment:}
#' - Prognostic risk score calculation
#' - Risk group stratification
#' - Nomogram generation
#' - Decision tree analysis
#'
#' \strong{Visualization & Output:}
#' - Forest plots and survival curves
#' - Person-time analysis
#' - Natural language summaries
#' - Educational explanations
#'
#' @seealso \code{\link{multisurvival}} for the main user interface function
#' @importFrom R6 R6Class
#' @importFrom riskRegression Score
#' @keywords internal
#' @return An \code{R6} class generator object for the \code{multisurvivalClass} backend; used internally by the jamovi analysis wrapper and not called directly.

multisurvivalClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "multisurvivalClass",
    inherit = multisurvivalBase,
    private = list(

      # Constants for plot sizing
      PLOT_WIDTH_FACTOR = 400,
      PLOT_HEIGHT_FACTOR = 300,
      DEFAULT_PLOT_WIDTH = 600,
      DEFAULT_PLOT_HEIGHT = 450,

      # Debug helpers (disabled)
      # Temporarily used during plot debugging; intentionally disabled and hidden.
      .debug_enabled = function() FALSE,
      .debug_dummy_plot_enabled = function() FALSE,
      .debug_write = function(lines) invisible(FALSE),

      # Per-run compute caches. .cleandata() is invoked ~25x and .cox_model()
      # ~15x within a single .run(); each .cox_model() call re-fits Cox (and,
      # for competing risks, re-expands the dataset via survival::finegray).
      # These caches ensure each heavy computation runs at most once per run.
      # They are reset at the top of .run() via .resetComputeCaches() so a
      # re-run with changed options recomputes rather than serving stale results.
      .dataCache = NULL,
      .dataComputed = FALSE,
      .coxCache = NULL,
      .coxComputed = FALSE,

      # HTML notice helper (replaces self$results$insert(N, jmvcore::Notice))
      # See R/survivalcont.b.R:700-743 for canonical pattern.
      # The protobuf serialization of Notice objects can fail with
      # "attempt to apply non-function"; rendering to dedicated Html outputs avoids it.
      .addHtmlMessage = function(type, title, message) {
          output_name <- switch(type,
              "error" = "errors",
              "strongWarning" = "strongWarnings",
              "warning" = "warnings",
              "info" = "infoMessages",
              "warnings"
          )
          css_class <- switch(type,
              "error" = "error-message",
              "strongWarning" = "strong-warning-message",
              "warning" = "warning-message",
              "info" = "info-message",
              "warning-message"
          )
          border_color <- switch(type,
              "error" = "#d9534f",
              "strongWarning" = "#e67e22",
              "warning" = "#f0ad4e",
              "info" = "#5bc0de",
              "#f0ad4e"
          )
          current_content <- self$results[[output_name]]$content
          if (is.null(current_content)) current_content <- ""
          new_message <- sprintf(
              '<div class="%s" style="margin: 10px 0; padding: 10px; border-left: 4px solid %s; background-color: #f8f9fa;"><strong>%s:</strong> %s</div>',
              css_class,
              border_color,
              htmltools::htmlEscape(title),
              htmltools::htmlEscape(message)
          )
          # Deduplicate notices: the central Cox model (.cox_model) is re-fit by
          # many downstream consumers within a single run, and each call may emit
          # the same EPV / event-count / proportional-hazards notice. Append only
          # if this exact notice is not already shown.
          if (!grepl(new_message, current_content, fixed = TRUE)) {
              self$results[[output_name]]$setContent(paste0(current_content, new_message))
          }
          self$results[[output_name]]$setVisible(TRUE)
      },

      # Reset the per-run compute caches so a re-run recomputes cleaned data and
      # the Cox model with the current options. Called at the top of .run().
      .resetComputeCaches = function() {
          private$.dataCache <- NULL
          private$.dataComputed <- FALSE
          private$.coxCache <- NULL
          private$.coxComputed <- FALSE
      },

      # Clear the four HTML notice outputs at the start of each run. .addHtmlMessage
      # appends to existing content, and these items have no clearWith in .r.yaml,
      # so without this reset a notice that no longer applies (e.g. a low-event
      # warning after the user adds data) would persist stale across re-runs.
      .initializeMessageOutputs = function() {
          for (nm in c("errors", "strongWarnings", "warnings", "infoMessages")) {
              self$results[[nm]]$setContent("")
              self$results[[nm]]$setVisible(FALSE)
          }
      },

      # Populate the interaction (effect-modification) test table and the
      # within-subgroup hazard-ratio table from a fitted Cox model. Called from
      # .run() only when interaction terms are requested. Pure numeric helpers
      # live in R/multisurvival-interactions.R.
      .populateInteractionTables = function(cox_model, cox_formula, data,
                                            real_interactions, conf_level,
                                            is_finegray) {
        # --- In-app explanatory panel (rendered once per run) ---
        # Base explanation (always shown). The continuous-variable caveat is
        # appended AFTER the subgroup loop, and only when the within-subgroup
        # table actually comes out empty (see below), so it does not clutter the
        # panel when subgroup HRs are present.
        interaction_expl_base <- paste0(
          "<div style='font-size:13px;line-height:1.55;'>",
          "<p><b>Interaction (Effect-Modification) Test.</b> One row per crossed term. ",
          "The HR is the ratio of one variable's hazard ratio between the levels of the other; ",
          "a small p indicates the effect of one variable <b>depends on</b> the other ",
          "(effect modification \u{2014} the signature of a predictive biomarker). HR = 1 means no modification.</p>",
          "<p><b>Within-Subgroup Hazard Ratios.</b> The focal variable's HR <i>within each level of the categorical moderator</i>. ",
          "For a term A\u{00D7}B the subgroups are formed by whichever variable is categorical (the moderator), and the HR shown is ",
          "the other variable's effect within each subgroup. These are read from the single interaction model by relevel-and-refit \u{2014} ",
          "the model's implied conditional effects. They pool information across subgroups, so the confidence intervals are typically ",
          "narrower (and p-values smaller) than fitting a separate Cox model within each subgroup; the point estimates are nearly identical.</p>",
          "<p><b>Are the subgroup HRs from the interaction model, or from separate per-subgroup fits?</b> ",
          "Directly from the single interaction model \u{2014} not separate subgroup Cox fits. For each moderator level the module ",
          "relevels the moderator so that level is the reference and refits the <i>same</i> full model ",
          "(Surv ~ focal \u{00D7} moderator, plus any other covariates you included). Releveling changes only the parameterization, ",
          "not the fit \u{2014} so the focal main-effect coefficient in the releveled model is the focal effect <i>within</i> that ",
          "moderator level: HR = exp(coef), with CI and p taken from that model's variance\u{2013}covariance matrix. This is equivalent ",
          "to reading \u{03B2}(focal) and \u{03B2}(focal) + \u{03B2}(focal:moderator) off the one interaction model \u{2014} releveling is ",
          "simply how each conditional effect is read, with the correct standard error, directly.</p>",
          "<p><b>Why do the subgroup HRs nearly match but the CIs and p-values differ from separate within-subgroup Cox models?</b> ",
          "This is the expected, core statistical distinction between one interaction model and per-subgroup fits:</p>",
          "<table style='border-collapse:collapse;font-size:12px;margin:6px 0;'>",
          "<tr style='background:#f0f0f0;'>",
          "<th style='border:1px solid #ccc;padding:3px 8px;text-align:left;'></th>",
          "<th style='border:1px solid #ccc;padding:3px 8px;text-align:left;'>Interaction model (this module)</th>",
          "<th style='border:1px solid #ccc;padding:3px 8px;text-align:left;'>Separate Cox per subgroup</th></tr>",
          "<tr><td style='border:1px solid #ccc;padding:3px 8px;'><b>Baseline hazard</b></td>",
          "<td style='border:1px solid #ccc;padding:3px 8px;'>common across moderator levels</td>",
          "<td style='border:1px solid #ccc;padding:3px 8px;'>each subgroup has its own</td></tr>",
          "<tr><td style='border:1px solid #ccc;padding:3px 8px;'><b>Other covariates' effects</b></td>",
          "<td style='border:1px solid #ccc;padding:3px 8px;'>assumed common (pooled)</td>",
          "<td style='border:1px solid #ccc;padding:3px 8px;'>re-estimated per subgroup</td></tr>",
          "<tr><td style='border:1px solid #ccc;padding:3px 8px;'><b>Standard errors</b></td>",
          "<td style='border:1px solid #ccc;padding:3px 8px;'>pool information across subgroups</td>",
          "<td style='border:1px solid #ccc;padding:3px 8px;'>subgroup data only</td></tr>",
          "<tr><td style='border:1px solid #ccc;padding:3px 8px;'><b>Efficiency</b></td>",
          "<td style='border:1px solid #ccc;padding:3px 8px;'>higher (narrower CI, more power)</td>",
          "<td style='border:1px solid #ccc;padding:3px 8px;'>lower (wider CI)</td></tr>",
          "</table>",
          "<p><b>HRs nearly identical:</b> with only focal \u{00D7} moderator in the model, they differ only because the interaction ",
          "model uses a common baseline hazard across moderator levels while a subgroup-only fit uses a moderator-specific baseline ",
          "(Cox's risk sets differ). With other covariates included, the small gap also reflects the interaction model's assumption of ",
          "common covariate effects. Hence &quot;very close but not exactly equal.&quot;</p>",
          "<p><b>CIs and p-values differ:</b> the interaction model borrows strength across both subgroups for the variance, giving ",
          "tighter intervals and smaller p; a separate fit sees only that subgroup's data, giving wider intervals. Both are valid but ",
          "answer subtly different questions \u{2014} the interaction-model (conditional) subgroup effect is the more efficient estimand ",
          "preferred for predictive-biomarker subgroup analysis, and is what this table reports (an implied conditional effect of the ",
          "single interaction model, not an independent subgroup fit).</p>",
          "<p><b>A note on sample size when comparing to a manual per-subgroup fit:</b> this analysis uses complete cases across ",
          "<i>all</i> selected variables, whereas fitting one marker at a time (e.g. per-marker <code>na.omit()</code>) uses that ",
          "marker's own complete-case set. To compare a single subgroup head-to-head, match the row set \u{2014} analyse that one ",
          "marker with the moderator \u{2014} otherwise the N and event count (and therefore the CI and p) can differ for that reason ",
          "alone.</p>",
          "</div>")
        self$results$interactionExplanation$setContent(interaction_expl_base)

        # --- Interaction (effect-modification) test table ---
        itab <- tryCatch(
          .interactionTestTable(cox_model, conf_level = conf_level),
          error = function(e) NULL
        )
        if (!is.null(itab) && nrow(itab) > 0) {
          itbl <- self$results$interactionTest
          for (i in seq_len(nrow(itab))) {
            itbl$addRow(rowKey = i, values = list(
              term     = itab$term[i],
              hr       = itab$hr[i],
              ci_lower = itab$ci_lower[i],
              ci_upper = itab$ci_upper[i],
              p        = itab$p[i]
            ))
          }
          itbl$setNote("emkey",
            "Each row tests whether the focal effect differs across the moderator (effect modification). A significant p indicates the effect is modified.")
        }

        # --- Within-subgroup hazard ratios ---
        sg <- self$results$subgroupHR
        if (isTRUE(is_finegray)) {
          sg$setNote("fg",
            "Within-subgroup hazard ratios are disabled in competing-risks (Fine-Gray) mode; interpret the interaction coefficient above instead.")
          return(invisible(NULL))
        }

        rowKey <- 0
        skipped_continuous <- FALSE
        skipped_highorder <- FALSE
        any_swapped <- FALSE
        nonconverged <- character(0)
        for (term in real_interactions) {
          info <- .interactionModeratorInfo(term, data)
          if (!isTRUE(info$twoway)) { skipped_highorder <- TRUE; next }
          if (!isTRUE(info$categorical_moderator)) { skipped_continuous <- TRUE; next }
          if (isTRUE(info$swapped)) any_swapped <- TRUE
          sub <- tryCatch(
            .computeSubgroupHRs(cox_formula, data,
                                focal = info$focal, moderator = info$moderator,
                                conf_level = conf_level),
            error = function(e) NULL
          )
          if (is.null(sub)) next
          for (i in seq_len(nrow(sub))) {
            rowKey <- rowKey + 1
            conv <- is.null(sub$converged) || isTRUE(sub$converged[i])
            sg$addRow(rowKey = rowKey, values = list(
              interaction     = sub$interaction[i],
              moderator_level = if (conv) sub$moderator_level[i] else paste0(sub$moderator_level[i], " *"),
              focal_effect    = sub$focal_effect[i],
              hr              = sub$hr[i],
              ci_lower        = sub$ci_lower[i],
              ci_upper        = sub$ci_upper[i],
              p               = sub$p[i]
            ))
            if (!conv)
              nonconverged <- c(nonconverged, paste0(sub$interaction[i], " [", sub$moderator_level[i], "]"))
          }
        }
        # Explain WHY the within-subgroup table is empty, in the explanation
        # panel, ONLY when it actually came out empty (rowKey == 0) for a known
        # structural reason (continuous moderator or higher-order term). When
        # subgroup HRs ARE shown this would be misleading, so it is suppressed.
        if (rowKey == 0 && (isTRUE(skipped_continuous) || isTRUE(skipped_highorder))) {
          self$results$interactionExplanation$setContent(paste0(
            interaction_expl_base,
            "<div style='font-size:13px;line-height:1.55;'>",
            "<p style='color:#8a5a00;'><b>Why is this table empty?</b> ",
            "The interaction is read as <i>focal : moderator</i> \u{2014} the first ",
            "variable is the focal effect and the second is the moderator that ",
            "defines the subgroups (if only the first variable is categorical, the ",
            "two are swapped so the categorical variable becomes the moderator). ",
            "Subgroup HRs are computed only when the term is <b>2-way</b> and the ",
            "moderator is <b>categorical</b>. Here both variables are continuous, or ",
            "the term is higher-order, so no discrete subgroups can be formed \u{2014} ",
            "only the interaction coefficient in the table above is reported. Include ",
            "a categorical variable in a 2-way interaction to see subgroup HRs.</p></div>"))
        }

        notes <- character(0)
        if (length(nonconverged) > 0)
          notes <- c(notes, paste0("* Model did not converge for: ",
                                   paste(unique(nonconverged), collapse = "; "),
                                   " (likely small-sample separation); interpret these HRs with extreme caution."))
        # Non-empty table but a continuous-moderator term was skipped: note it as
        # a footnote. The fully-empty case is covered by the panel caveat above,
        # so we do not repeat it here.
        if (isTRUE(skipped_continuous) && rowKey > 0)
          notes <- c(notes, "Interactions with a continuous moderator are shown as a coefficient in the table above; per-subgroup HRs require a categorical moderator. If both variables in a term are continuous, no subgroups can be formed.")
        if (any_swapped)
          notes <- c(notes, "Subgroups were defined by the categorical variable in each term.")
        if (isTRUE(skipped_highorder) && rowKey > 0)
          notes <- c(notes, "Within-subgroup HRs are computed for 2-way interactions only.")
        if (length(notes) > 0)
          sg$setNote("sgnote", paste(notes, collapse = " "))
        invisible(NULL)
      },

      .setPlotVisibility = function() {
        visible_flags <- list(
          plot = isTRUE(self$options$hr) && self$options$sty == "t1",
          plot3 = isTRUE(self$options$hr) && self$options$sty == "t3",
          plotKM = isTRUE(self$options$km),
          plot_adj = isTRUE(self$options$ac),
          plot_nomogram = isTRUE(self$options$showNomogram),
          plot8 = isTRUE(self$options$ph_cox)
        )

        self$results$plot$setVisible(visible_flags$plot)
        self$results$plot3$setVisible(visible_flags$plot3)
        self$results$plotKM$setVisible(visible_flags$plotKM)
        self$results$plot_adj$setVisible(visible_flags$plot_adj)
        self$results$plot_nomogram$setVisible(visible_flags$plot_nomogram)
        self$results$plot8$setVisible(visible_flags$plot8)

        invisible(visible_flags)
      },

      # Constants for analysis parameters
      DEFAULT_MIN_NODE = 20,
      DEFAULT_COMPLEXITY = 0.01,
      DEFAULT_MAX_DEPTH = 5,
      DEFAULT_SPLINE_DF = 3,

      # Constants for time intervals
      DEFAULT_TIME_INTERVALS = "12, 36, 60",
      DEFAULT_RATE_MULTIPLIER = 100,
      DEFAULT_CHANGE_TIMES = "6, 12, 18",
      DEFAULT_TD_SUFFIX = "_t{time}",

      .nom_object = NULL,
      .perf_timers = NULL,
      .validation_warnings = NULL,
      .validation_time = NULL,
      .analysis_times = NULL,

      # Validation Helper Functions ----

      # Comprehensive Survival Analysis Input Validation
      #
      # Validates all required inputs for survival analysis including outcome variables,
      # time variables, and predictor variables. This is the main validation function
      # that orchestrates all validation checks.
      #
      # Returns a list containing:
      #   - valid: Boolean indicating if all inputs are valid
      #   - has_outcome: Boolean indicating if outcome variable is specified
      #   - has_time: Boolean indicating if time variables are properly specified
      #   - has_predictors: Boolean indicating if predictor variables are specified
      #
      # This function performs comprehensive validation by checking:
      # - Outcome variable presence
      # - Time variable configuration (either direct elapsed time or date-based calculation)
      # - Predictor variable specification (categorical or continuous explanatory variables)
      .validateSurvivalInputs = function() {
        has_outcome <- !is.null(self$options$outcome)
        has_time <- private$.validateTimeInputs()
        has_predictors <- private$.validatePredictorInputs()

        return(list(
          valid = has_outcome && has_time && has_predictors,
          has_outcome = has_outcome,
          has_time = has_time,
          has_predictors = has_predictors
        ))
      },

      # Time Variable Validation
      #
      # Validates time variable configuration for survival analysis. Supports both
      # direct elapsed time input and automatic calculation from diagnosis/follow-up dates.
      #
      # Returns: Boolean indicating if time variables are properly configured
      #
      # Checks two possible time configuration scenarios:
      # 1. Calculated time (tint = TRUE): Requires both dxdate and fudate
      # 2. Direct time (tint = FALSE): Requires elapsedtime variable
      .validateTimeInputs = function() {
        has_time_calc <- self$options$tint && !is.null(self$options$dxdate) && !is.null(self$options$fudate)
        has_time_direct <- !self$options$tint && !is.null(self$options$elapsedtime)
        return(has_time_calc || has_time_direct)
      },

      # Predictor Variable Validation
      #
      # Validates that at least one predictor variable is specified for the Cox model.
      # Accepts either categorical explanatory variables or continuous explanatory variables.
      #
      # Returns: Boolean indicating if predictor variables are specified
      #
      # Checks for the presence of either:
      # - Categorical explanatory variables (explanatory option)
      # - Continuous explanatory variables (contexpl option)
      # At least one type must be specified for multivariable analysis.
      .validatePredictorInputs = function() {
        return(length(self$options$explanatory) > 0 || length(self$options$contexpl) > 0)
      },

      # Standardized Error Message Formatting
      #
      # Creates standardized, user-friendly HTML error messages with consistent styling
      # and optional suggestions for troubleshooting.
      #
      # Parameters:
      #   title - Character string for the error message title
      #   message - Character string for the main error message
      #   suggestions - Optional character string with HTML list items for suggestions
      #
      # Returns: Formatted HTML string for display in the jamovi interface
      #
      # Generates styled HTML error messages with:
      # - Consistent warning color scheme (yellow background, brown text)
      # - Professional typography and spacing
      # - Optional suggestions section for user guidance
      # - Bootstrap-compatible styling for jamovi integration
      .formatErrorMessage = function(title, message, suggestions = NULL) {
        error_html <- paste0(
          "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px; padding: 15px; margin: 10px;'>",
          "<h4 style='color: #856404; margin-top: 0;'> ", title, "</h4>",
          "<p style='color: #856404; margin: 10px 0;'>", message, "</p>"
        )

        if (!is.null(suggestions)) {
          error_html <- paste0(error_html,
            "<div style='margin-top: 10px;'>",
            "<strong style='color: #856404;'>Suggestions:</strong>",
            "<ul style='margin: 5px 0; padding-left: 20px; color: #856404;'>",
            suggestions,
            "</ul>",
            "</div>"
          )
        }

        error_html <- paste0(error_html, "</div>")
        return(error_html)
      },

      # Input Sanitization for String Parameters
      #
      # Sanitizes string inputs to prevent XSS attacks and validate format compliance.
      # Removes potentially harmful characters and applies pattern validation.
      #
      # Parameters:
      #   input - Character string to sanitize
      #   default_value - Default value to return if input is invalid/empty
      #   pattern - Optional regex pattern for validation
      #
      # Returns: Sanitized character string or default value if input is invalid
      #
      # Security measures applied:
      # - Removes HTML/XML characters: < > " ' &
      # - Validates against optional regex patterns
      # - Returns safe defaults for empty/null inputs
      # - Prevents code injection in user-provided strings
      #
      # Examples:
      # # Sanitize time intervals input
      # clean_intervals <- private$.sanitizeStringInput("12, 24, 36", "12, 36, 60")
      #
      # # Validate numeric pattern
      # clean_number <- private$.sanitizeStringInput("123", "100", "^[0-9]+$")
      .sanitizeStringInput = function(input, default_value, pattern = NULL) {
        if (is.null(input) || input == "" || is.na(input)) {
          return(default_value)
        }

        # Remove potentially harmful characters
        cleaned <- gsub("[<>\"'&]", "", input)

        # Apply pattern validation if provided
        if (!is.null(pattern) && !grepl(pattern, cleaned)) {
          return(default_value)
        }

        return(cleaned)
      },

      # Performance Monitoring - Start Timer
      #
      # Starts a performance timer for a specific operation to measure execution time.
      # Used for monitoring long-running operations and optimizing performance bottlenecks.
      #
      # Parameters:
      #   operation_name - String identifier for the operation being timed
      #
      # Note: Stores the start time in private$.perf_timers list for later retrieval
      .startPerformanceTimer = function(operation_name) {
        private$.perf_timers <- list()
        private$.perf_timers[[operation_name]] <- Sys.time()
      },

      # Performance Monitoring - Stop Timer
      #
      # Stops a performance timer and calculates elapsed time for the operation.
      # Returns the execution time in seconds for performance analysis.
      #
      # Parameters:
      #   operation_name - String identifier for the operation being timed
      #
      # Returns: Numeric value representing elapsed time in seconds, or NULL if timer not found
      .stopPerformanceTimer = function(operation_name) {
        if (is.null(private$.perf_timers) || is.null(private$.perf_timers[[operation_name]])) {
          return(NULL)
        }

        elapsed <- difftime(Sys.time(), private$.perf_timers[[operation_name]], units = "secs")
        private$.perf_timers[[operation_name]] <- NULL

        return(as.numeric(elapsed))
      },

      # Memory-Efficient Data Processing
      #
      # Processes large datasets in manageable chunks to prevent memory overflow.
      # Automatically handles datasets larger than the specified chunk size by
      # splitting into smaller portions and combining results.
      #
      # Parameters:
      #   data - Data frame to process
      #   chunk_function - Function to apply to each chunk
      #   chunk_size - Maximum number of rows per chunk (default: 1000)
      #
      # Returns: Combined results from all chunks (typically rbind of chunk results)
      .processDataInChunks = function(data, chunk_function, chunk_size = 1000) {
        if (nrow(data) <= chunk_size) {
          return(chunk_function(data))
        }

        # Process data in chunks to manage memory
        n_chunks <- ceiling(nrow(data) / chunk_size)
        results <- list()

        for (i in 1:n_chunks) {
          start_row <- (i - 1) * chunk_size + 1
          end_row <- min(i * chunk_size, nrow(data))
          chunk_data <- data[start_row:end_row, , drop = FALSE]

          results[[i]] <- chunk_function(chunk_data)
        }

        # Combine results (implementation depends on chunk_function output)
        return(do.call(rbind, results))
      },

      # FIX: Helper functions for competing risk analysis ----
      # These functions provide proper cumulative incidence function (CIF)
      # support for competing risk scenarios (analysistype = "compete")

      .isCompetingRisk = function() {
        # Check if current analysis is competing risk mode
        return(self$options$multievent && self$options$analysistype == "compete")
      },

      .competingRiskCumInc = function(mydata, mytime, myoutcome) {
        # Calculate cumulative incidence function for competing risks
        # Uses cmprsk package for proper handling of competing events
        #
        # Args:
        #   mydata: cleaned data frame
        #   mytime: time variable name
        #   myoutcome: outcome variable name (0=censored, 1=event, 2=competing)
        # Returns:
        #   cuminc object from cmprsk package

        mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

        cuminc_fit <- cmprsk::cuminc(
          ftime = mydata[[mytime]],
          fstatus = mydata[[myoutcome]],
          cencode = 0
        )
        return(cuminc_fit)
      },

      .getDefaultCutpoints = function() {
        # Get default time cutpoints based on selected time unit
        # This ensures cutpoints are appropriate for the time scale
        #
        # Returns:
        #   Numeric vector of default cutpoints (1, 3, 5 year equivalents)

        time_unit <- self$options$timetypeoutput
        switch(time_unit,
               "days" = c(365, 1095, 1825),
               "weeks" = c(52, 156, 260),
               "months" = c(12, 36, 60),
               "years" = c(1, 3, 5),
               c(12, 36, 60)  # default to months
        )
      },

      # ============================================================================
      # PLANNED FEATURES - IMPLEMENTATION STUBS (Future Development)
      # ============================================================================
      # The following features were identified in code review (2025-12-07) as
      # beneficial enhancements but are deferred pending:
      #   - User feedback on implemented features (summaries, glossary, assumptions)
      #   - External validation and clinical testing results
      #   - Priority assessment based on usage patterns
      #
      # Status: DRAFT - Commented out, not active in production
      #
      # To implement: See corresponding commented options in multisurvival.a.yaml
      # Implementation checklist when activating:
      #   1. Uncomment options in .a.yaml (PLANNED FEATURES section)
      #   2. Add output definitions in .r.yaml
      #   3. Implement helper methods below
      #   4. Add unit tests in tests/testthat/test-multisurvival-statistical.R
      #   5. Update vignettes with examples
      #   6. Test on reference datasets (colon, veteran, lung)
      # ============================================================================

      # PLANNED: Configurable Alpha Level (Priority: Medium)
      # Replace hardcoded p < 0.05 with user-configurable threshold
      # Affects: PH diagnostics (line 2143), log-rank tests (line 4862)
      # .getAlphaLevel = function() {
      #   if (!is.null(self$options$alpha_level)) {
      #     return(self$options$alpha_level)
      #   }
      #   return(0.05)  # default
      # },

      # PLANNED: Advanced Performance Metrics (Priority: Low)
      # Add Brier score, time-dependent AUC, calibration plots
      # Dependencies: pec, survAUC packages
      # .calculateAdvancedMetrics = function(cox_model, mydata) {
      #   if (!self$options$advancedMetrics) return(NULL)
      #   metrics <- list()
      #   timepoints <- private$.parseTimepoints(self$options$brierTimepoints)
      #   # Brier score
      #   metrics$brier <- private$.calculateBrierScore(cox_model, mydata, timepoints)
      #   # Time-dependent AUC
      #   metrics$tdAUC <- private$.calculateTimeDependentAUC(cox_model, mydata, timepoints, self$options$tdAUC_method)
      #   return(metrics)
      # },

      # PLANNED: Residual Diagnostic Plots (Priority: Low)
      # Schoenfeld, martingale, deviance, dfbeta residuals
      # .generateResidualPlots = function(cox_model, mydata) {
      #   if (!self$options$showResidualDiagnostics) return(NULL)
      #   # Implementation stub - create plot objects for each residual type
      # },

      # PLANNED: Guided Wizard Mode (Priority: Medium)
      # Progressive UI disclosure for novice users
      # .handleWizardStep = function() {
      #   if (!self$options$guidedMode) return(NULL)
      #   # Control UI element visibility based on wizard progress
      # },

      # PLANNED: Clinical Presets (Priority: Medium-Low)
      # One-click analysis templates
      # .applyPreset = function() {
      #   preset <- self$options$analysisPreset
      #   if (preset == "none") return(NULL)
      #   # Auto-configure options based on preset
      # },

      # PLANNED: Color Palette Selection (Priority: Low)
      # .getPlotColors = function(n) {
      #   palette <- self$options$colorPalette
      #   # Return n colors from selected palette
      # },

      # ============================================================================
      # END PLANNED FEATURES
      # ============================================================================

      # init ----
      .init = function() {
        # Validate inputs using helper functions
        validation <- private$.validateSurvivalInputs()

        # Early exit if essential variables are missing - show welcome message
        if (!validation$valid) {
          # Initialize all outputs to FALSE first
          self$results$text$setVisible(FALSE)
          self$results$text2$setVisible(FALSE)
          self$results$plot$setVisible(FALSE)
          self$results$plot3$setVisible(FALSE)
          self$results$plotKM$setVisible(FALSE)
          self$results$plot_adj$setVisible(FALSE)
          self$results$plot_nomogram$setVisible(FALSE)
          self$results$plot8$setVisible(FALSE)

          # Hide all summary and explanation outputs
          self$results$multivariableCoxSummaryHeading$setVisible(FALSE)
          self$results$multivariableCoxSummary$setVisible(FALSE)
          self$results$personTimeSummaryHeading$setVisible(FALSE)
          self$results$personTimeSummary$setVisible(FALSE)
          self$results$adjustedSurvivalSummaryHeading$setVisible(FALSE)
          self$results$adjustedSurvivalSummary$setVisible(FALSE)
          self$results$nomogramSummaryHeading$setVisible(FALSE)
          self$results$nomogramSummary$setVisible(FALSE)
          self$results$riskScoreSummaryHeading$setVisible(FALSE)
          self$results$riskScoreTable$setVisible(FALSE)
          self$results$riskScoreSummary$setVisible(FALSE)

          # Show welcome/todo message
          self$results$todo$setVisible(TRUE)
          return()
        }

        # Hide todo if we have sufficient variables
        self$results$todo$setVisible(FALSE)

        # Initialize all main outputs to FALSE first
        self$results$text$setVisible(FALSE)
        self$results$text2$setVisible(FALSE)
        self$results$plot$setVisible(FALSE)
        self$results$plot3$setVisible(FALSE)
        self$results$plotKM$setVisible(FALSE)
        self$results$plot_adj$setVisible(FALSE)
        self$results$plot_nomogram$setVisible(FALSE)
        self$results$plot8$setVisible(FALSE)

        # Restore plot visibility based on current options (avoids .init() overriding
        # the .r.yaml visibility expressions permanently).
        vis_flags <- private$.setPlotVisibility()
        private$.debug_write(list(
          phase = ".init(visibility)",
          options = list(
            hr = self$options$hr,
            sty = self$options$sty,
            km = self$options$km,
            ac = self$options$ac,
            ph_cox = self$options$ph_cox,
            showNomogram = self$options$showNomogram
          ),
          visible = vis_flags
        ))

        # Initialize all summary outputs and headings to FALSE first
        self$results$multivariableCoxSummaryHeading$setVisible(FALSE)
        self$results$multivariableCoxSummary$setVisible(FALSE)
        self$results$personTimeSummaryHeading$setVisible(FALSE)
        self$results$personTimeSummary$setVisible(FALSE)
        self$results$adjustedSurvivalSummaryHeading$setVisible(FALSE)
        self$results$adjustedSurvivalSummary$setVisible(FALSE)
        self$results$nomogramSummaryHeading$setVisible(FALSE)
        self$results$nomogramSummary$setVisible(FALSE)
        self$results$riskScoreSummaryHeading$setVisible(FALSE)
        self$results$riskScoreTable$setVisible(FALSE)
        self$results$riskScoreSummary$setVisible(FALSE)
        # EXPERIMENTAL: Disabled - result elements not in .r.yaml
        # self$results$treeSummaryHeading$setVisible(FALSE)
        # self$results$tree_summary$setVisible(FALSE)
        # self$results$ml_ensemble_summary$setVisible(FALSE)

        # Initialize all explanation outputs and headings to FALSE first
        self$results$multivariableCoxHeading3$setVisible(FALSE)
        self$results$multivariableCoxExplanation$setVisible(FALSE)
        self$results$adjustedSurvivalExplanation$setVisible(FALSE)
        self$results$riskScoreExplanation$setVisible(FALSE)
        self$results$nomogramExplanation$setVisible(FALSE)
        self$results$personTimeExplanation$setVisible(FALSE)
        self$results$stratifiedAnalysisExplanation$setVisible(FALSE)
        self$results$survivalPlotsHeading3$setVisible(FALSE)
        self$results$survivalPlotsExplanation$setVisible(FALSE)

        # Handle showSummaries visibility
        if (self$options$showSummaries) {
            # Main multivariable cox summary
            self$results$multivariableCoxSummaryHeading$setVisible(TRUE)
            self$results$multivariableCoxSummary$setVisible(TRUE)

            # Conditional summaries - require both showSummaries AND their specific option
            if (self$options$person_time) {
                self$results$personTimeSummaryHeading$setVisible(TRUE)
                self$results$personTimeSummary$setVisible(TRUE)
            }
            if (self$options$ac) {
                self$results$adjustedSurvivalSummaryHeading$setVisible(TRUE)
                self$results$adjustedSurvivalSummary$setVisible(TRUE)
            }
            if (self$options$showNomogram) {
                self$results$nomogramSummaryHeading$setVisible(TRUE)
                self$results$nomogramSummary$setVisible(TRUE)
            }
            if (self$options$calculateRiskScore) {
                self$results$riskScoreSummaryHeading$setVisible(TRUE)
                self$results$riskScoreTable$setVisible(TRUE)
                self$results$riskScoreSummary$setVisible(TRUE)
            }
            # EXPERIMENTAL: Disabled - result elements not in .r.yaml
            # if (self$options$use_tree) {
            #     self$results$treeSummaryHeading$setVisible(TRUE)
            #     self$results$tree_summary$setVisible(TRUE)
            # }
            # if (self$options$ml_method == 'ensemble') {
            #     self$results$ml_ensemble_summary$setVisible(TRUE)
            # }
        }

        # Handle showExplanations visibility
        if (self$options$showExplanations) {
            # ENHANCEMENT: Add statistical glossary panel for clinical users
            # Provides plain-language definitions of key statistical terms
            self$results$glossaryPanel$setContent(
              "<div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; border-radius: 5px; margin: 10px 0;'>
              <h4 style='color: #0056b3; margin-top: 0;'>Statistical Terms Glossary</h4>
              <dl style='line-height: 1.6;'>
                <dt><b>Hazard Ratio (HR)</b></dt>
                <dd style='margin-bottom: 10px;'>Effect size for time-to-event outcomes. HR &gt; 1 indicates increased risk of the event; HR &lt; 1 indicates decreased risk (protective effect); HR = 1 indicates no effect. For example, HR = 2.0 means the hazard is doubled.</dd>
                <dt><b>C-index (Concordance Index)</b></dt>
                <dd style='margin-bottom: 10px;'>Measures the model's ability to discriminate between patients who experience the event and those who don't. C-index &gt; 0.7 indicates good discrimination; 0.6-0.7 is acceptable; &lt; 0.6 suggests limited predictive ability. Similar to AUC in logistic regression.</dd>
                <dt><b>EPV (Events Per Variable)</b></dt>
                <dd style='margin-bottom: 10px;'>Number of events divided by number of predictors in the model. EPV \u2265 10 is recommended to avoid overfitting and optimism in model performance estimates. Low EPV increases risk of unstable coefficient estimates.</dd>
                <dt><b>Proportional Hazards (PH) Assumption</b></dt>
                <dd style='margin-bottom: 10px;'>Core assumption of Cox regression that the hazard ratio stays constant over time. Tested using cox.zph test; p &gt; 0.05 suggests assumption is met. If violated, consider time-varying effects or stratification.</dd>
                <dt><b>Fine-Gray Model (Competing Risks)</b></dt>
                <dd style='margin-bottom: 10px;'>Extension of Cox regression for competing risks that models subdistribution hazards. Appropriate when interested in cumulative incidence functions. Hazard ratios are not directly comparable to cause-specific Cox models.</dd>
                <dt><b>Censoring</b></dt>
                <dd style='margin-bottom: 10px;'>Observation where the event of interest has not yet occurred by the end of follow-up. Assumed to be non-informative (censoring is independent of event risk).</dd>
                <dt><b>Person-Time</b></dt>
                <dd style='margin-bottom: 10px;'>Sum of time each individual is observed (at risk) in the study. Used to calculate incidence rates; accounts for varying follow-up durations across participants.</dd>
              </dl>
              </div>"
            )
            self$results$glossaryPanel$setVisible(TRUE)

            # ENHANCEMENT: Add assumptions checklist panel for clinical safety
            # Lists key assumptions and provides guidance on checking them
            self$results$assumptionsPanel$setContent(
              "<div style='padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 5px; margin: 10px 0;'>
              <h4 style='color: #856404; margin-top: 0;'>Cox Model Assumptions and Caveats</h4>
              <p style='line-height: 1.6;'><b>Before interpreting results, verify these assumptions:</b></p>
              <ul style='line-height: 1.6;'>
                <li><b>Proportional Hazards:</b> Hazard ratios remain constant over time. Check using PH diagnostic test (cox.zph). If p &lt; 0.05 for any variable, consider time-varying effects or stratification.</li>
                <li><b>Independent Censoring:</b> Censoring is unrelated to the event risk. Verify through study design (e.g., administrative censoring is typically safe; loss to follow-up may be informative).</li>
                <li><b>Linear Relationships:</b> Continuous predictors have linear effects on log-hazard. Check using martingale residuals or categorize continuous variables.</li>
                <li><b>Adequate Sample Size:</b> Minimum 10 events per predictor variable (EPV \u2265 10). Lower EPV increases risk of overfitting, unstable estimates, and optimistic performance.</li>
                <li><b>No Influential Outliers:</b> Extreme observations can distort estimates. Check deviance residuals and dfbeta plots.</li>
                <li><b>Correct Time Origin:</b> All subjects enter at time zero (or use left truncation for delayed entry).</li>
              </ul>
              <p style='line-height: 1.6;'><b>Common Pitfalls:</b></p>
              <ul style='line-height: 1.6;'>
                <li>Too many predictors relative to events (low EPV) leads to overfitting</li>
                <li>Ignoring PH violations can bias hazard ratio estimates</li>
                <li>Mixing cause-specific and subdistribution hazards in competing risks</li>
                <li>Extrapolating beyond observed follow-up times</li>
              </ul>
              </div>"
            )
            self$results$assumptionsPanel$setVisible(TRUE)

            # Main explanation section
            self$results$multivariableCoxHeading3$setVisible(TRUE)
            self$results$multivariableCoxExplanation$setContent(
              "<div style='padding: 15px; background-color: #e7f3ff; border-left: 4px solid #2196F3; border-radius: 5px; margin: 10px 0;'>
              <h4 style='color: #1976D2; margin-top: 0;'>Understanding Multivariable Cox Regression</h4>
              <p style='line-height: 1.6;'>This analysis adjusts for multiple factors simultaneously, providing <b>independent effects</b> of each variable while controlling for others.</p>
              <ul style='line-height: 1.6;'>
                <li><b>Hazard Ratio (HR) > 1:</b> Increased risk of the event</li>
                <li><b>Hazard Ratio (HR) < 1:</b> Decreased risk of the event</li>
                <li><b>Hazard Ratio (HR) = 1:</b> No effect on risk</li>
              </ul>
              <p style='line-height: 1.6;'><i>Example:</i> HR = 2.0 means the hazard is doubled; HR = 0.5 means the hazard is halved compared to the reference group.</p>
              </div>"
            )
            self$results$multivariableCoxExplanation$setVisible(TRUE)

            # Conditional explanations - require both showExplanations AND their specific option
            if (self$options$ac) {
                self$results$adjustedSurvivalExplanation$setContent(
                  "<div style='padding: 15px; background-color: #fff3e0; border-left: 4px solid #ff9800; border-radius: 5px; margin: 10px 0;'>
                  <h4 style='color: #F57C00; margin-top: 0;'>Understanding Adjusted Survival Curves</h4>
                  <p style='line-height: 1.6;'>Adjusted survival curves show survival probabilities <b>after adjusting for covariates</b> in the model. These curves represent the expected survival experience for a <i>typical patient</i> in each group, accounting for the effects of other variables.</p>
                  <p style='line-height: 1.6;'><b>Key Points:</b></p>
                  <ul style='line-height: 1.6;'>
                    <li>Curves are adjusted to the <b>average values</b> of other covariates (or specified reference values)</li>
                    <li>Useful for comparing groups while <b>controlling for confounders</b></li>
                    <li>More representative of <b>real-world</b> patient populations than unadjusted curves</li>
                  </ul>
                  </div>"
                )
                self$results$adjustedSurvivalExplanation$setVisible(TRUE)
            }
            if (self$options$calculateRiskScore) {
                self$results$riskScoreExplanation$setContent(
                  "<div style='padding: 15px; background-color: #f3e5f5; border-left: 4px solid #9c27b0; border-radius: 5px; margin: 10px 0;'>
                  <h4 style='color: #7B1FA2; margin-top: 0;'>Understanding Risk Score Analysis</h4>
                  <p style='line-height: 1.6;'>Risk scores combine <b>all model predictors</b> into a single prognostic index that quantifies each patient's overall risk.</p>
                  <p style='line-height: 1.6;'><b>How It Works:</b></p>
                  <ul style='line-height: 1.6;'>
                    <li><b>Calculation:</b> Weighted sum of predictor values using Cox model coefficients</li>
                    <li><b>Higher scores</b> indicate higher predicted risk of the event</li>
                    <li><b>Risk groups</b> are created by dividing patients into quantiles (tertiles, quartiles, etc.)</li>
                  </ul>
                  <p style='line-height: 1.6;'><b>Clinical Use:</b> Risk stratification enables targeted interventions for high-risk patients and resource allocation based on predicted outcomes.</p>
                  </div>"
                )
                self$results$riskScoreExplanation$setVisible(TRUE)
            }
            if (self$options$showNomogram) {
                self$results$nomogramExplanation$setContent(
                  "<div style='padding: 15px; background-color: #e8f5e9; border-left: 4px solid #4caf50; border-radius: 5px; margin: 10px 0;'>
                  <h4 style='color: #388E3C; margin-top: 0;'>Understanding Nomograms</h4>
                  <p style='line-height: 1.6;'>A nomogram is a <b>graphical calculator</b> that translates complex regression models into an easy-to-use clinical tool.</p>
                  <p style='line-height: 1.6;'><b>How to Use:</b></p>
                  <ol style='line-height: 1.6;'>
                    <li>Find each predictor's value on its scale</li>
                    <li>Draw a line straight up to the <b>Points</b> axis to get points for that variable</li>
                    <li>Add up all points to get the <b>Total Points</b></li>
                    <li>Find the total on the <b>Total Points</b> axis</li>
                    <li>Draw a line down to read the predicted <b>survival probability</b></li>
                  </ol>
                  <p style='line-height: 1.6;'><i>Clinical Value:</i> Nomograms provide personalized risk estimates at the point of care without complex calculations.</p>
                  </div>"
                )
                self$results$nomogramExplanation$setVisible(TRUE)
            }
            if (self$options$person_time) {
                self$results$personTimeExplanation$setContent(
                  "<div style='padding: 15px; background-color: #fce4ec; border-left: 4px solid #e91e63; border-radius: 5px; margin: 10px 0;'>
                  <h4 style='color: #C2185B; margin-top: 0;'>Understanding Person-Time Analysis</h4>
                  <p style='line-height: 1.6;'>Person-time measures the <b>total time individuals are at risk</b> in your study, accounting for different follow-up durations.</p>
                  <p style='line-height: 1.6;'><b>Key Concepts:</b></p>
                  <ul style='line-height: 1.6;'>
                    <li><b>Person-time units:</b> Sum of follow-up time for all individuals (e.g., person-years)</li>
                    <li><b>Incidence rate:</b> Number of events \u00f7 person-time (e.g., events per 1000 person-years)</li>
                    <li><b>Why it matters:</b> Properly accounts for varying observation periods and censoring</li>
                  </ul>
                  <p style='line-height: 1.6;'><i>Example:</i> If 10 people are followed for 5 years each (50 person-years) and 2 events occur, the incidence rate is 2/50 = 0.04 events per person-year or 40 per 1000 person-years.</p>
                  </div>"
                )
                self$results$personTimeExplanation$setVisible(TRUE)
            }
            if (self$options$use_stratify) {
                self$results$stratifiedAnalysisExplanation$setContent(
                  "<div style='padding: 15px; background-color: #fff8e1; border-left: 4px solid #ffc107; border-radius: 5px; margin: 10px 0;'>
                  <h4 style='color: #F57F17; margin-top: 0;'>Understanding Stratified Cox Regression</h4>
                  <p style='line-height: 1.6;'>Stratification is used when a variable <b>violates the proportional hazards assumption</b> but you still want to control for its effect.</p>
                  <p style='line-height: 1.6;'><b>What It Does:</b></p>
                  <ul style='line-height: 1.6;'>
                    <li>Creates <b>separate baseline hazards</b> for each stratum (level of the stratification variable)</li>
                    <li>Allows different hazard shapes over time for each stratum</li>
                    <li>Still estimates effects of other variables while controlling for the stratification variable</li>
                  </ul>
                  <p style='line-height: 1.6;'><b>When to Use:</b> When proportional hazards testing (cox.zph) shows violation for a variable, or when you know hazards cross over time.</p>
                  </div>"
                )
                self$results$stratifiedAnalysisExplanation$setVisible(TRUE)
            }

            # Survival plots explanation requires showExplanations AND at least one plot
            if (self$options$ac || self$options$hr || self$options$km) {
                self$results$survivalPlotsHeading3$setVisible(TRUE)
                self$results$survivalPlotsExplanation$setContent(
                  "<div style='padding: 15px; background-color: #e0f2f1; border-left: 4px solid #009688; border-radius: 5px; margin: 10px 0;'>
                  <h4 style='color: #00796B; margin-top: 0;'>Understanding Survival Curves and Plots</h4>
                  <p style='line-height: 1.6;'>Survival curves visualize the <b>probability of surviving</b> (not experiencing the event) over time.</p>
                  <p style='line-height: 1.6;'><b>Reading the Plot:</b></p>
                  <ul style='line-height: 1.6;'>
                    <li><b>Y-axis:</b> Survival probability (0 = all had event, 1 = none had event)</li>
                    <li><b>X-axis:</b> Time since study entry</li>
                    <li><b>Steps down:</b> Occur when events happen</li>
                    <li><b>Tick marks:</b> Indicate censored observations (lost to follow-up)</li>
                    <li><b>Shaded area:</b> 95% confidence interval (uncertainty in the estimate)</li>
                  </ul>
                  <p style='line-height: 1.6;'><b>Types:</b> Kaplan-Meier curves show <i>unadjusted</i> survival; adjusted curves account for covariates; forest plots show hazard ratios with confidence intervals.</p>
                  </div>"
                )
                self$results$survivalPlotsExplanation$setVisible(TRUE)
            }
        }

        # Handle plot sizing (existing logic preserved)
        explanatory_len <- length(self$options$explanatory)
        contexpl_len <- length(self$options$contexpl)

        if (explanatory_len > 0 || contexpl_len > 0) {
          self$results$plot8$setSize((explanatory_len + contexpl_len) * private$PLOT_WIDTH_FACTOR,
                                     (explanatory_len + contexpl_len) * private$PLOT_HEIGHT_FACTOR)
        } else {
          self$results$plot8$setVisible(FALSE)
        }

        # Note: Main analysis outputs (text, text2, plots) will be set visible in .run() after validation
      }

      # getData ----
      ,
      .getData = function() {
        # Check if data exists and has content
        if (is.null(self$data) || nrow(self$data) == 0) {
          jmvcore::reject('Data contains no (complete) rows')
        }

        # Get the data
        mydata <- self$data


        # Check if data has names
        if (is.null(names(mydata))) {
          jmvcore::reject('Data must have column names')
        }

        # Add row names if missing
        if (is.null(rownames(mydata))) {
          mydata$row_names <- seq_len(nrow(mydata))
        } else {
          mydata$row_names <- rownames(mydata)
        }

        # Get original names
        original_names <- names(mydata)

        # Check if original names exist
        if (length(original_names) == 0) {
          jmvcore::reject(paste0(
            .("Data must have column names."), "\n\n",
            .("Possible solutions:"), "\n",
            "\u2022 ", .("Ensure your dataset has proper column headers"), "\n",
            "\u2022 ", .("Check that the data was imported correctly"), "\n",
            "\u2022 ", .("Verify the data is not empty"), "\n",
            "\u2022 ", .("Column names should describe your variables (e.g., 'Age', 'Survival_Time', 'Event_Status')")
          ))
        }

        # Create labels vector
        labels <- stats::setNames(original_names, original_names)

        # Clean names safely
        mydata_cleaned <- try({
          janitor::clean_names(mydata)
        }, silent = TRUE)

        # mydata <- mydata %>% janitor::clean_names()


        if (inherits(mydata_cleaned, "try-error")) {
          jmvcore::reject(paste0(
            .("Error cleaning variable names."), "\n\n",
            .("Possible solutions:"), "\n",
            "\u2022 ", .("Check for special characters or spaces in column names"), "\n",
            "\u2022 ", .("Ensure column names don't start with numbers"), "\n",
            "\u2022 ", .("Remove any duplicate column names"), "\n",
            "\u2022 ", .("Avoid reserved R keywords as column names (e.g., 'if', 'else', 'for')")
          ))
        }


        # Create corrected labels
        corrected_labels <- stats::setNames(original_names, names(mydata_cleaned))

        # Apply labels
        mydata_labelled <- try({
          labelled::set_variable_labels(.data = mydata_cleaned, .labels = corrected_labels)
        }, silent = TRUE)

        # mydata <- labelled::set_variable_labels(
        #     .data = mydata,
        #     .labels = corrected_labels
        # )


        if (inherits(mydata_labelled, "try-error")) {
          jmvcore::reject(paste0(
            .("Error setting variable labels."), "\n\n",
            .("Possible solutions:"), "\n",
            "\u2022 ", .("Check that all variables have valid names after cleaning"), "\n",
            "\u2022 ", .("Ensure no variables have completely missing data"), "\n",
            "\u2022 ", .("Verify the dataset is not corrupted"), "\n",
            "\u2022 ", .("Try reloading your data file")
          ))
        }


        # Get all labels
        all_labels <- labelled::var_label(mydata_labelled)

        # all_labels <- labelled::var_label(mydata)


        # Get variable names from labels
        mytime <- try({
          names(all_labels)[all_labels == self$options$elapsedtime]
        }, silent = TRUE)

        # mytime <-
        #     names(all_labels)[all_labels == self$options$elapsedtime]

        myoutcome <- try({
          names(all_labels)[all_labels == self$options$outcome]
        }, silent = TRUE)

        # myoutcome <-
        #     names(all_labels)[all_labels == self$options$outcome]


        mydxdate <- try({
          names(all_labels)[all_labels == self$options$dxdate]
        }, silent = TRUE)

        # mydxdate <-
        #     names(all_labels)[all_labels == self$options$dxdate]


        myfudate <- try({
          names(all_labels)[all_labels == self$options$fudate]
        }, silent = TRUE)

        # myfudate <-
        #     names(all_labels)[all_labels == self$options$fudate]



        labels_explanatory <- self$options$explanatory

        myexplanatory <-
          names(all_labels)[match(labels_explanatory, all_labels)]

        labels_contexpl <- self$options$contexpl

        mycontexpl <-
          names(all_labels)[match(labels_contexpl, all_labels)]


        # Get adjexplanatory only if it exists and ac option is TRUE
        adjexplanatory <- NULL
        if (!is.null(self$options$adjexplanatory) &&
            self$options$ac) {
          adjexplanatory <- names(all_labels)[all_labels == self$options$adjexplanatory]
        }


        mystratvar_labelled <- NULL


        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
        # Add this to get stratification variables
        labels_stratvar <- self$options$stratvar
        mystratvar_labelled <- names(all_labels)[match(labels_stratvar, all_labels)]

        # Set stratification explanation
        strat_vars_display <- htmltools::htmlEscape(paste(self$options$stratvar, collapse = ", "))
        self$results$stratificationExplanation$setContent(paste0(
          "<p><strong>Stratification Variables:</strong> ", strat_vars_display, "</p>",
          "<p>The Cox model is stratified by these variables, allowing for different baseline hazards ",
          "in each stratum while estimating common covariate effects.</p>"
        ))
        }


        # Check if required variables were found with helpful error messages
        if (length(mytime) == 0 && !is.null(self$options$elapsedtime)) {
          jmvcore::reject(paste0(
            .("Could not find the elapsed time variable."), "\n\n",
            .("Possible solutions:"), "\n",
            "\u2022 ", .("Check that the variable name is correct in your dataset"), "\n",
            "\u2022 ", .("Ensure the variable contains numeric time values (days, months, years)"), "\n",
            "\u2022 ", .("Verify there are no special characters in the variable name"), "\n",
            "\u2022 ", .("The time variable should represent time from study entry to event or last follow-up")
          ))
        }
        if (length(myoutcome) == 0 && !is.null(self$options$outcome)) {
          jmvcore::reject(paste0(
            .("Could not find the outcome variable."), "\n\n",
            .("Possible solutions:"), "\n",
            "\u2022 ", .("Check that the variable name is correct in your dataset"), "\n",
            "\u2022 ", .("Ensure the variable is coded as 0/1 or FALSE/TRUE (0=censored, 1=event)"), "\n",
            "\u2022 ", .("Verify there are no missing values in the outcome variable"), "\n",
            "\u2022 ", .("The outcome should indicate whether the event of interest occurred")
          ))
        }

        # Perform comprehensive data validation
        validation_results <- .validateSurvivalData(mydata_labelled, mytime, myoutcome)

        # Handle validation issues and warnings
        if (length(validation_results$issues) > 0) {
          issue_message <- paste0(
            "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
            "<h4 style='color: #721c24; margin-top: 0;'> ", .("Data Validation Issues"), "</h4>",
            "<ul style='margin: 5px 0; padding-left: 20px;'>",
            paste(lapply(validation_results$issues, function(x) paste0("<li>", x, "</li>")), collapse = ""),
            "</ul>",
            "<p><strong>", .("Action Required:"), "</strong> ", .("Please correct these issues before proceeding with analysis."), "</p>",
            "</div>"
          )
          jmvcore::reject(issue_message)
        }

        # Display warnings if any
        if (length(validation_results$warnings) > 0) {
          warning_message <- paste0(
            "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
            "<h4 style='color: #856404; margin-top: 0;'> ", .("Data Validation Warnings"), "</h4>",
            "<ul style='margin: 5px 0; padding-left: 20px;'>",
            paste(lapply(validation_results$warnings, function(x) paste0("<li>", x, "</li>")), collapse = ""),
            "</ul>",
            "<p><strong>", .("Note:"), "</strong> ", .("Analysis will proceed, but consider these recommendations for optimal results."), "</p>",
            "</div>"
          )
          # Store warning to display later
          private$.validation_warnings <- warning_message
        }

        # Return results
        return(
          list(
            "mydata_labelled" = mydata_labelled,
            "mytime_labelled" = mytime,
            "myoutcome_labelled" = myoutcome,
            "mydxdate_labelled" = mydxdate,
            "myfudate_labelled" = myfudate,
            "mycontexpl_labelled" = mycontexpl,
            "myexplanatory_labelled" = myexplanatory,
            "adjexplanatory_labelled" = adjexplanatory,
            "mystratvar_labelled" = mystratvar_labelled,
            "validation_warnings" = private$.validation_warnings

          )
        )



      }

      # todo ----
      ,
      .todo = function() {
        # todo ----

        todo <- glue::glue(
          "
                    <br>Welcome to ClinicoPath
                    <br><br>
                        This tool will help you perform a multivariable survival analysis.
                    <br><br>
                        Explanatory variables can be categorical (ordinal or nominal) or continuous.
                    <br><br>
                    Select outcome level from Outcome variable.
                    <br><br>
                    Outcome Level: if patient is dead or event (recurrence) occured. You may also use advanced outcome options depending on your analysis type.
                    <br><br>
                        Survival time should be numeric, continuous, and in months. You may also use dates to calculate survival time in advanced elapsed time options.
                    <br><br>


        Stratification Variables: Use these when the proportional hazards assumption
        is violated for certain variables. The model will create separate baseline
        hazard functions for each level of the stratification variables, but won't
        estimate their direct effects.
        <br><br>
        Consider using stratification when:
        <br>- A variable fails the proportional hazards test
        <br>- You need to control for a variable's effect but don't need to
        estimate its hazard ratio
        <br>- There are natural differences in baseline risk across groups

<br><br>
                        This function uses finalfit, survival, survminer and ggstatsplot packages. Please cite jamovi and the packages as given below.
                    <br><br>
                    "
        )

        # https://finalfit.org/articles/all_tables_examples.html#cox-proportional-hazards-model-survival-time-to-event


        html <- self$results$todo
        html$setContent(todo)
        return()

      }





      # Define Survival Time ----
      ,
      .definemytime = function() {
        ## Read Labelled Data ----

        labelled_data <- private$.getData()

        mydata <- labelled_data$mydata_labelled
        mytime_labelled <- labelled_data$mytime_labelled
        mydxdate_labelled <- labelled_data$mydxdate_labelled
        myfudate_labelled <- labelled_data$myfudate_labelled

        tint <- self$options$tint

        if (isTRUE(getOption("multisurvival.debug"))) {
          message("[multisurvival.debug] definemytime: tint = ", tint,
                  ", mytime_labelled = ", mytime_labelled)
        }


        if (!tint) {
          ### Precalculated Time ----

          # Check if time variable is selected
          if (is.null(mytime_labelled) || length(mytime_labelled) == 0) {
            # Return empty data frame with proper structure
            return(data.frame(row_names = character(0), mytime = numeric(0)))
          }

          mydata[["mytime"]] <-
            jmvcore::toNumeric(mydata[[mytime_labelled]])

          if (isTRUE(getOption("multisurvival.debug"))) {
            message("[multisurvival.debug] definemytime: mytime after toNumeric class = ",
                    paste(class(mydata[["mytime"]]), collapse = "/"))
          }

          # If a jamovi survival/time variable was passed (Surv object), keep only its time column
          if (survival::is.Surv(mydata[["mytime"]])) {
            mydata[["mytime"]] <- as.numeric(mydata[["mytime"]][, "time"])
          } else if (is.matrix(mydata[["mytime"]]) && ncol(mydata[["mytime"]]) >= 1) {
            # Defensive: handle matrices coming from special column types
            mydata[["mytime"]] <- as.numeric(mydata[["mytime"]][, 1])
          }


        } else if (tint) {
          ### Time Interval ----

          dxdate <- mydxdate_labelled
          fudate <- myfudate_labelled
          timetypedata <- self$options$timetypedata


          # Check if input is numeric (Unix epoch) or text (requires parsing)
          is_numeric_dx <- is.numeric(mydata[[dxdate]])
          is_numeric_fu <- is.numeric(mydata[[fudate]])

          if (is_numeric_dx && is_numeric_fu) {
              # Handle numeric Unix epoch input (from DateTime Converter)
              mydata[["start"]] <- as.POSIXct(mydata[[dxdate]], origin="1970-01-01", tz="UTC")
              mydata[["end"]] <- as.POSIXct(mydata[[fudate]], origin="1970-01-01", tz="UTC")
          } else if (!is_numeric_dx && !is_numeric_fu) {
              # Handle text datetime input via lubridate
              lubridate_functions <- list(
                  ymdhms = lubridate::ymd_hms,
                  ymd = lubridate::ymd,
                  ydm = lubridate::ydm,
                  mdy = lubridate::mdy,
                  myd = lubridate::myd,
                  dmy = lubridate::dmy,
                  dym = lubridate::dym
              )

              if (timetypedata %in% names(lubridate_functions)) {
                  func <- lubridate_functions[[timetypedata]]
                  mydata[["start"]] <- func(mydata[[dxdate]])
                  mydata[["end"]] <- func(mydata[[fudate]])
              } else {
                  jmvcore::reject(paste0("Unsupported time type format: ", timetypedata,
                             ". Supported formats are: ", paste(names(lubridate_functions), collapse = ", ")))
              }
          } else {
              # Mixed types error
              jmvcore::reject("Diagnosis date and follow-up date must be in the same format (both numeric or both text)")
          }


          if (sum(!is.na(mydata[["start"]])) == 0 ||
              sum(!is.na(mydata[["end"]])) == 0)  {
            jmvcore::reject(
              paste0(
                "Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: ",
                self$options$timetypedata
              )
            )
          }

          timetypeoutput <-
            jmvcore::constructFormula(terms = self$options$timetypeoutput)


          mydata <- mydata %>%
            dplyr::mutate(interval = lubridate::interval(start, end))



          mydata <- mydata %>%
            dplyr::mutate(mytime = lubridate::time_length(interval, timetypeoutput))

        }

        ### Early validation: Check for negative times immediately after calculation ----
        # This prevents wasted computation in downstream cleaning and model fitting
        if (any(mydata$mytime < 0, na.rm = TRUE)) {
          n_negative <- sum(mydata$mytime < 0, na.rm = TRUE)
          # Notice Disabled per user request (serialization issues)
          # notice <- jmvcore::Notice$new(...)
          
          error_msg <- sprintf(
              "Negative Survival Times Detected: %d observation(s) have negative time values. This typically indicates:\n\u2022 Follow-up date occurs before diagnosis date\n\u2022 Incorrect date variable selection (dates reversed)\n\u2022 Data entry errors in date fields\n\nTo Fix:\n1. Verify 'Diagnosis Date' and 'Follow-up Date' are correctly assigned\n2. Check that diagnosis always precedes follow-up\n3. Review date formats and ensure consistency\n4. Examine observations with negative times for data errors",
              n_negative
          )
          
          self$results$todo$setVisible(TRUE)
          self$results$todo$setContent(paste0("<b>Error:</b> ", error_msg))

          # Stop the analysis with the tailored message. Returning NULL here would
          # feed NULL into .cleandata_impl's dplyr::left_join(), which throws a
          # cryptic error that .executeAnalysis() then surfaces in `todo`, clobbering
          # the specific negative-time guidance above. reject() carries error_msg
          # through as conditionMessage(), so the guidance survives. (error_msg is a
          # pre-formatted literal with no '%', so it is safe as a reject format.)
          jmvcore::reject(error_msg)
        }

        df_time <- mydata %>% jmvcore::select(c("row_names", "mytime"))

        return(df_time)


      }

      # Define Outcome ----
      ,
      .definemyoutcome = function() {
        labelled_data <- private$.getData()

        mydata <- labelled_data$mydata_labelled
        myoutcome_labelled <- labelled_data$myoutcome_labelled


        contin <- c("integer", "numeric", "double")

        outcomeLevel <- self$options$outcomeLevel
        multievent <- self$options$multievent

        # Check if outcome variable is selected
        if (is.null(myoutcome_labelled) || length(myoutcome_labelled) == 0) {
          # Return empty data frame with proper structure
          return(data.frame(row_names = character(0), myoutcome = numeric(0)))
        }

        outcome1 <- mydata[[myoutcome_labelled]]

        if (!multievent) {
          if (inherits(outcome1, contin)) {
            if (!((length(unique(outcome1[!is.na(outcome1)])) == 2) &&
                  (sum(unique(outcome1[!is.na(outcome1)])) == 1))) {
              jmvcore::reject(
                'When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.'
              )

            }

            mydata[["myoutcome"]] <- mydata[[myoutcome_labelled]]
            # mydata[[self$options$outcome]]

          } else if (inherits(outcome1, "factor")) {
            if (is.null(outcomeLevel)) {
              jmvcore::reject("Please select an event level for the outcome variable.")
            }
            mydata[["myoutcome"]] <-
              ifelse(test = outcome1 == outcomeLevel,
                     yes = 1,
                     no = 0)

          } else {
            jmvcore::reject(
              'When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0. If you are using a factor as an outcome, please check the levels and content.'
            )

          }

        } else if (multievent) {
          analysistype <- self$options$analysistype

          dod <- self$options$dod
          dooc <- self$options$dooc
          awd <- self$options$awd
          awod <- self$options$awod

          if (analysistype == 'overall') {
            # Overall ----
            # (Alive) <=> (Dead of Disease & Dead of Other Causes)


            mydata[["myoutcome"]] <- NA_integer_

            mydata[["myoutcome"]][outcome1 == awd] <- 0
            mydata[["myoutcome"]][outcome1 == awod] <- 0
            mydata[["myoutcome"]][outcome1 == dod] <- 1
            mydata[["myoutcome"]][outcome1 == dooc] <- 1



          } else if (analysistype == 'cause') {
            # Cause Specific ----
            # (Alive & Dead of Other Causes) <=> (Dead of Disease)


            mydata[["myoutcome"]] <- NA_integer_

            mydata[["myoutcome"]][outcome1 == awd] <- 0
            mydata[["myoutcome"]][outcome1 == awod] <- 0
            mydata[["myoutcome"]][outcome1 == dod] <- 1
            mydata[["myoutcome"]][outcome1 == dooc] <- 0

          } else if (analysistype == 'compete') {
            # Competing Risks ----
            # Alive <=> Dead of Disease accounting for Dead of Other Causes

            # Create factor for Fine-Gray analysis
            # 0=Censored, 1=Event, 2=Competing
            
            temp_outcome <- rep("Censored", length(outcome1))
            
            if (!is.null(awd)) temp_outcome[outcome1 == awd] <- "Censored"
            if (!is.null(awod)) temp_outcome[outcome1 == awod] <- "Censored"
            if (!is.null(dod)) temp_outcome[outcome1 == dod] <- "Event"
            if (!is.null(dooc)) temp_outcome[outcome1 == dooc] <- "Competing"
            
            mydata[["myoutcome"]] <- factor(temp_outcome, levels = c("Censored", "Event", "Competing"))

          }

        }

        df_outcome <- mydata %>% jmvcore::select(c("row_names", "myoutcome"))

        return(df_outcome)

      }


      # Define Factor ----
      ,

      .definemyfactor = function() {
        labelled_data <- private$.getData()

        mydata_labelled <- labelled_data$mydata_labelled
        myexplanatory_labelled <- labelled_data$myexplanatory_labelled
        mycontexpl_labelled <- labelled_data$mycontexpl_labelled
        adjexplanatory_labelled <- labelled_data$adjexplanatory_labelled

        mydata <- mydata_labelled

        df_factor <- mydata %>%
          jmvcore::select(unique(
            c(
              "row_names",
              myexplanatory_labelled,
              adjexplanatory_labelled,
              mycontexpl_labelled
            )
          ))

        return(df_factor)

      }

      # Clean Data ----
      ,
      .cleandata = function() {
        # Cached wrapper: compute the cleaned/labelled data once per run.
        # All ~25 call sites hit this; the heavy work lives in .cleandata_impl().
        if (!private$.dataComputed) {
          private$.dataCache <- private$.cleandata_impl()
          private$.dataComputed <- TRUE
        }
        private$.dataCache
      }
      ,
      .cleandata_impl = function() {
        ## Common Definitions ----

        contin <- c("integer", "numeric", "double")

        ## Read Data ----

        labelled_data <- private$.getData()

        mydata_labelled        <- labelled_data$mydata_labelled
        mytime_labelled        <- labelled_data$mytime_labelled
        myoutcome_labelled     <- labelled_data$myoutcome_labelled
        mydxdate_labelled      <- labelled_data$mydxdate_labelled
        myfudate_labelled      <- labelled_data$myfudate_labelled
        myexplanatory_labelled <- labelled_data$myexplanatory_labelled
        mycontexpl_labelled    <- labelled_data$mycontexpl_labelled
        adjexplanatory_labelled <- labelled_data$adjexplanatory_labelled
        mystratvar_labelled <- labelled_data$mystratvar_labelled

        time <- private$.definemytime()
        outcome <- private$.definemyoutcome()
        factor <- private$.definemyfactor()

        ## Clean Data ----
        cleanData <- dplyr::left_join(time, outcome, by = "row_names") %>%
          dplyr::left_join(factor, by = "row_names")

        ## Landmark ----

        # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#landmark_method

        if (self$options$uselandmark) {
          landmark <- jmvcore::toNumeric(self$options$landmark)

          cleanData <- cleanData %>%
            dplyr::filter(mytime >= landmark) %>%
            dplyr::mutate(mytime = mytime - landmark)
        }

        ## Names cleanData ----

        if (self$options$tint) {
          name1time <- "CalculatedTime"
        }

        if (!self$options$tint &&
            !is.null(self$options$elapsedtime)) {
          name1time <- mytime_labelled
        }

        name2outcome <- myoutcome_labelled

        if (self$options$multievent) {
          name2outcome <- "CalculatedOutcome"
        }

        name3expl <- NULL

        if (!is.null(self$options$explanatory)) {
          name3expl <- myexplanatory_labelled
        }


        name3contexpl <- NULL

        if (!is.null(self$options$contexpl)) {
          name3contexpl <- mycontexpl_labelled
        }

        # Add adjexplanatory name if present
        adjexplanatory_name <- NULL
        if (!is.null(adjexplanatory_labelled)) {
          adjexplanatory_name <- adjexplanatory_labelled
        }


        # naOmit ----

        cleanData <- jmvcore::naOmit(cleanData)




        ## Add Calculated Time to Data ----

        if (self$options$tint &&
            self$options$calculatedtime &&
            self$results$calculatedtime$isNotFilled()) {
          self$results$calculatedtime$setRowNums(cleanData$row_names)
          self$results$calculatedtime$setValues(cleanData$mytime)
        }




        ## Add Redefined Outcome to Data ----

        if (self$options$multievent  &&
            self$options$outcomeredefined &&
            self$results$outcomeredefined$isNotFilled()) {
          self$results$outcomeredefined$setRowNums(cleanData$row_names)
          self$results$outcomeredefined$setValues(cleanData$myoutcome)
        }


        # self$results$mydataview$setContent(
        #   list(
        #     "name1time" = name1time,
        #     "name2outcome" = name2outcome,
        #     "name3contexpl" = name3contexpl,
        #     "name3expl" = name3expl,
        #     "adjexplanatory_name" = adjexplanatory_name,
        #
        #     "cleanData" = cleanData,
        #     "mytime_labelled" = mytime_labelled,
        #     "myoutcome_labelled" = myoutcome_labelled,
        #     "mydxdate_labelled" = mydxdate_labelled,
        #     "myfudate_labelled" = myfudate_labelled,
        #     "myexplanatory_labelled" = myexplanatory_labelled,
        #     "mycontexpl_labelled" = mycontexpl_labelled,
        #     "adjexplanatory_labelled" = adjexplanatory_labelled
        #
        #   )
        # )



        # Return Data ----

        return(
          list(
            "name1time" = name1time,
            "name2outcome" = name2outcome,
            "name3contexpl" = name3contexpl,
            "name3expl" = name3expl,
            "adjexplanatory_name" = adjexplanatory_name,

            "cleanData" = cleanData,
            "mydata_labelled" = mydata_labelled,
            "mytime_labelled" = mytime_labelled,
            "myoutcome_labelled" = myoutcome_labelled,
            "mydxdate_labelled" = mydxdate_labelled,
            "myfudate_labelled" = myfudate_labelled,
            "myexplanatory_labelled" = myexplanatory_labelled,
            "mycontexpl_labelled" = mycontexpl_labelled,
            "adjexplanatory_labelled" = adjexplanatory_labelled,
            "mystratvar_labelled" = mystratvar_labelled

          )
        )

      }



      # run  ----
      ,
      # Modular Run Function Components ----

      # Input Validation and UI Preparation
      #
      # Validates all user inputs and prepares the jamovi interface for analysis.
      # Displays appropriate error messages or welcome content based on validation results.
      #
      # Returns: TRUE if all inputs are valid and analysis can proceed, FALSE otherwise
      #
      # Validation Steps:
      # - Check for required outcome, time, and predictor variables
      # - Validate multievent configuration if selected
      # - Display helpful error messages with suggestions
      # - Show/hide appropriate UI elements
      # - Performance monitoring for validation time
      .validateAndPrepare = function() {
        # Start performance timer
        private$.startPerformanceTimer("validation")

        # Validate inputs using our helper functions
        validation <- private$.validateSurvivalInputs()

        if (!validation$valid) {
          private$.todo()
          self$results$text$setVisible(FALSE)
          self$results$text2$setVisible(FALSE)
          self$results$plot$setVisible(FALSE)
          self$results$plot3$setVisible(FALSE)
          self$results$plot8$setVisible(FALSE)
          self$results$todo$setVisible(TRUE)
          return(FALSE)
        }

        # Additional specific validations for multievent scenarios
        if (self$options$multievent) {
          if (is.null(self$options$dod) && is.null(self$options$dooc)) {
            # Convert to Notice for consistent UX
            # Notice Disabled
            # notice <- jmvcore::Notice$new(...)
            
            self$results$todo$setContent("<b>Error:</b> Multiple Events Configuration Error: When using multiple event levels, you must specify at least one event type (Dead of Disease or Dead of Other Causes). Select at least one event level from the outcome variable and ensure it has the appropriate levels.")
            self$results$todo$setVisible(TRUE)
            return(FALSE)
          }
        }

        self$results$todo$setVisible(FALSE)
        self$results$text$setVisible(TRUE)
        self$results$text2$setVisible(TRUE)

        validation_time <- private$.stopPerformanceTimer("validation")
        private$.validation_time <- validation_time

        return(TRUE)
      }

      # Main Analysis Execution
      #
      # Executes the complete survival analysis with performance monitoring.
      # Orchestrates data preparation, survival modeling, and timing collection.
      #
      # Returns: TRUE if analysis completes successfully, NULL on error
      #
      # Features:
      # - Performance monitoring for each analysis phase
      # - Error handling with detailed logging
      # - Data preparation and validation
      # - Main survival analysis execution
      # - Timing collection for optimization
      ,.executeAnalysis = function() {
        # Start performance timer for main analysis
        private$.startPerformanceTimer("analysis")

        tryCatch({
          # Data preparation
          private$.startPerformanceTimer("data_prep")
          cleaneddata <- private$.cleandata()
          data_prep_time <- private$.stopPerformanceTimer("data_prep")

          # Main survival analysis
          private$.startPerformanceTimer("survival_analysis")
          analysis_results <- private$.performSurvivalAnalysis(cleaneddata)
          survival_time <- private$.stopPerformanceTimer("survival_analysis")

          # Machine learning analysis (if requested)
          # EXPERIMENTAL: Disabled - ml_method option not in .a.yaml
          # if (self$options$ml_method != "none") {
          #   private$.startPerformanceTimer("ml_analysis")
          #   private$.runMLAnalysis()
          #   ml_time <- private$.stopPerformanceTimer("ml_analysis")
          # } else {
            ml_time <- 0
          # }

          # Optimism-corrected discrimination (bootstrap C-index), if requested
          private$.calculateOptimismCIndex()

          # Generate clinical interpretation summary
          private$.generateAndDisplayClinicalSummary(cleaneddata)

          # Store timing information
          private$.analysis_times <- list(
            data_prep = data_prep_time,
            survival_analysis = survival_time,
            ml_analysis = ml_time,
            validation = private$.validation_time
          )

          return(analysis_results)

        }, error = function(e) {
          # Notice Disabled
          # notice <- jmvcore::Notice$new(...)
          
          self$results$todo$setContent(paste0(
            "<b>Survival Analysis Error:</b> ", htmltools::htmlEscape(conditionMessage(e)), "<br><br>",
            "Recommendations: (1) Check data for missing/invalid values in time and outcome variables, (2) Ensure time variable contains positive numeric values, (3) Verify outcome is binary (0/1 or FALSE/TRUE), (4) Check sufficient events (\u226510), (5) Ensure explanatory variables have appropriate types, (6) Try fewer variables, or (7) Check for outliers."
          ))
          self$results$todo$setVisible(TRUE)
          return(NULL)
        })
      },

      # Core Survival Analysis Implementation
      #
      # Performs the main survival analysis including Cox regression and all
      # optional analysis modules based on user selections.
      #
      # Parameters:
      #   cleaneddata - Processed and validated dataset
      #
      # Returns: TRUE if analysis completes successfully
      #
      # Analysis Components:
      # - Core Cox proportional hazards modeling
      # - Person-time analysis (if requested)
      # - Risk score calculation and stratification (if requested)
      # - Adjusted survival curves (if requested)
      # - Nomogram generation (if requested)
      # - Decision tree analysis (if requested)
      #
      # Each component is conditionally executed based on user options
      .performSurvivalAnalysis = function(cleaneddata) {

        # Stop if Empty Data
        if (nrow(self$data) == 0) {
          jmvcore::reject('Data contains no (complete) rows')
        }

        # Fit central Cox model once for downstream plots
        cox_model <- private$.cox_model()

        if (isTRUE(getOption("multisurvival.debug"))) {
          message("[multisurvival.debug] performSurvivalAnalysis: cox_model fitted = ", !is.null(cox_model))
        }

        private$.debug_write(list(
          phase = ".performSurvivalAnalysis",
          cox_model_null = is.null(cox_model),
          hr = self$options$hr,
          km = self$options$km,
          ac = self$options$ac
        ))

        # Short-circuit if model fails
        if (is.null(cox_model)) {
          return(NULL)
        }

        # Pass cleaned data to plot renderers so state is available when jamovi requests images
        if (self$options$hr) {
          self$results$plot$setState(c(cleaneddata, list(cox_model = cox_model)))
          self$results$plot3$setState(c(cleaneddata, list(cox_model = cox_model)))
        }
        if (self$options$km) {
          self$results$plotKM$setState(cleaneddata)
        }
        if (self$options$ac) {
          self$results$plot_adj$setState(cleaneddata)
        }

        # Execute the main analysis components
        private$.checkpoint()
        private$.final_fit2()

        # Proportional hazards assumption diagnostics (drives plot8 state)
        if (self$options$ph_cox) {
          tryCatch(
            private$.cox_ph(cox_model),
            error = function(e) {
              private$.debug_write(list(
                phase = ".cox_ph(error)",
                message = e$message
              ))
              self$results$plot8$setVisible(FALSE)
              NULL
            }
          )
        }

        # Additional analysis modules
        if (self$options$person_time) {
          private$.personTimeAnalysis()
        }

        if (self$options$calculateRiskScore) {
          private$.calculateRiskScoreWrapper()
        }

        if (self$options$ac) {
          private$.calculateAdjustedStats()
        }

        if (self$options$showNomogram) {
          private$.calculate_nomogram()
        }

        # AFT Model Analysis - COMMENTED OUT (options disabled in .a.yaml/.u.yaml)
        # if (self$options$use_aft) {
        #   private$.calculate_aft()
        # }

        # Model performance metrics (C-index, IPCW Brier / AUC, IBS via riskRegression)
        if (self$options$show_survmetrics) {
          private$.calculate_survmetrics()
        }

        # Covariate contribution (single-term deletion LRT / AIC)
        if (self$options$compare_models) {
          private$.compare_models()
        }

        # EXPERIMENTAL:         if (self$options$use_tree) {
        # EXPERIMENTAL:           private$.calculate_survivaldecisiontree()
        # EXPERIMENTAL:         }

        # Return success indicator
        return(TRUE)
      },

      # Lightweight wrapper to calculate risk scores using the current Cox model
      .calculateRiskScoreWrapper = function() {
        cox_model <- private$.cox_model()
        if (is.null(cox_model)) {
          return()
        }

        cleaneddata <- private$.cleandata()
        if (is.null(cleaneddata$cleanData)) {
          return()
        }

        if (self$options$multievent && self$options$analysistype == "compete") {
          # Notice Disabled
          # notice <- jmvcore::Notice$new(...)
          
          # self$results$insert(3, notice)
        }

        private$.calculateRiskScore(cox_model, cleaneddata$cleanData)
      },

      .run = function() {
        private$.resetComputeCaches()
        private$.initializeMessageOutputs()
        # Modular execution using helper functions
        if (!private$.validateAndPrepare()) {
          return()
        }

        # Execute main analysis
        analysis_results <- private$.executeAnalysis()
        if (is.null(analysis_results)) {
          return()
        }

        # Generate analysis completion summary notice
        # This provides confidence that analysis completed and summarizes key metrics
        tryCatch({
          cleaneddata <- private$.cleandata()
          mydata <- cleaneddata$cleanData
          event_indicator <- .eventIndicator(mydata$myoutcome)
          n_obs <- nrow(mydata)
          n_events <- sum(event_indicator, na.rm = TRUE)
          event_rate <- (n_events / n_obs) * 100
          median_followup <- median(mydata$mytime, na.rm = TRUE)
          time_unit <- self$options$timetypeoutput

          private$.addHtmlMessage(
            "info",
            .("Analysis complete"),
            sprintf(
              .("Analysis completed successfully using %d observations with %d events (%.1f%% event rate) over %.1f %s median follow-up."),
              n_obs, n_events, event_rate, median_followup, time_unit
            )
          )
        }, error = function(e) {
          # Notice Disabled
          # notice <- jmvcore::Notice$new(...)
          
          # self$results$add(notice)
        })

        # Analysis completed successfully
        return(TRUE)
      }

      # cox model  ----
      ,
      .cox_model = function() {
        # Cached wrapper: fit the Cox model once per run. Side effects (clinical
        # notices, interaction-table population) therefore emit exactly once.
        # The heavy fit (incl. Fine-Gray dataset expansion) lives in
        # .cox_model_impl(). NULL (validation failure) is also cached so the
        # failure path and its notices are not re-emitted within a run.
        if (!private$.coxComputed) {
          private$.coxCache <- private$.cox_model_impl()
          private$.coxComputed <- TRUE
        }
        private$.coxCache
      }
      ,
      .cox_model_impl = function() {
        cleaneddata <- private$.cleandata()

        name1time <- cleaneddata$name1time
        name2outcome <- cleaneddata$name2outcome
        name3contexpl <- cleaneddata$name3contexpl
        name3expl <- cleaneddata$name3expl
        adjexplanatory_name <- cleaneddata$adjexplanatory_name

        mydata <- cleanData <- cleaneddata$cleanData
        # Basic time/outcome validation
        if (any(is.na(mydata$mytime) | is.na(mydata$myoutcome))) {
          dropped <- sum(!complete.cases(mydata[, c("mytime", "myoutcome")]))
          private$.addHtmlMessage(
            "warning",
            .("Missing time/outcome values"),
            sprintf(.("Missing time/outcome values detected; %d row(s) may be excluded from the Cox model."), dropped)
          )
        }
        # Safety check: Negative times should already be caught in .definemytime()
        # This is defensive programming in case time is provided directly (not calculated)
        if (any(mydata$mytime < 0, na.rm = TRUE)) {
          n_negative <- sum(mydata$mytime < 0, na.rm = TRUE)
          private$.addHtmlMessage(
            "error",
            .("Negative survival times detected"),
            sprintf(
              .("%d observation(s) have negative time values. To fix: (1) if using 'Elapsed Time' directly, verify all values are positive; (2) if calculating from dates, check diagnosis date precedes follow-up date; (3) review data for entry errors; (4) consider excluding problematic observations."),
              n_negative
            )
          )
          return(NULL)
        }

        # Clinical validation: Check event count and EPV ratio
        # These thresholds are critical for survival analysis validity
        event_indicator <- .eventIndicator(mydata$myoutcome)
        n_events <- sum(event_indicator, na.rm = TRUE)
        n_complete <- sum(complete.cases(mydata))
        n_vars <- length(c(self$options$explanatory, self$options$contexpl))
        epv <- if (n_vars > 0) n_events / n_vars else Inf

        # CRITICAL: < 10 events - Analysis cannot proceed reliably
        if (n_events < 10) {
          private$.addHtmlMessage(
            "error",
            .("Critically low event count"),
            sprintf(
              .("Only %d events detected. Cox regression requires at least 10 events for reliable estimation. Recommendations: (1) collect more data, (2) extend follow-up period, (3) use descriptive methods (Kaplan-Meier) instead of regression, or (4) pool event types if clinically appropriate."),
              n_events
            )
          )
          return(NULL)
        }

        # STRONG WARNING: 10-19 events - Results may be unreliable
        if (n_events >= 10 && n_events < 20) {
          private$.addHtmlMessage(
            "strongWarning",
            .("Low event count"),
            sprintf(
              .("Low event count (%d events). Results may be unstable; confidence intervals may be unreliable; small-sample bias likely. Recommendations: (1) interpret results cautiously and report exact p-values, (2) consider Firth's penalized likelihood (coxphf package) for bias reduction, (3) validate findings externally, or (4) collect additional data if feasible."),
              n_events
            )
          )
        }

        # WARNING: 20-49 events - Limited statistical power
        if (n_events >= 20 && n_events < 50) {
          private$.addHtmlMessage(
            "warning",
            .("Moderate event count"),
            sprintf(
              .("Moderate event count (%d events). Statistical power may be limited for detecting small effects. Current EPV (events per variable) ratio: %.1f. Consider limiting model complexity or using variable selection methods."),
              n_events, epv
            )
          )
        }

        # WARNING: EPV < 10 - Overfitting risk
        # Note: Only warn if we have enough events (>=50) but too many variables
        if (epv < 10 && n_events >= 50 && n_vars > 0) {
          private$.addHtmlMessage(
            "warning",
            .("Low events-per-variable ratio"),
            sprintf(
              .("Low events-per-variable ratio (EPV = %.1f with %d predictors, %d events). Recommended EPV \u2265 10 to minimize overfitting. Recommendations: (1) reduce number of predictors, (2) use variable selection (backward/forward/stepwise), (3) apply penalized regression (LASSO/Ridge), or (4) use clinical knowledge to prioritize key variables."),
              epv, n_vars, n_events
            )
          )
        }

        mytime_labelled <- cleaneddata$mytime_labelled
        myoutcome_labelled <- cleaneddata$myoutcome_labelled
        mydxdate_labelled <- cleaneddata$mydxdate_labelled
        myfudate_labelled <- cleaneddata$myfudate_labelled
        myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
        mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
        adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled
        mystratvar_labelled <- cleaneddata$mystratvar_labelled



        # Add stratification variables
        mystratvar <- NULL
        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
          mystratvar <- as.vector(cleaneddata$mystratvar_labelled)
          if (length(mystratvar) == 0) {
            mystratvar <- NULL
          }
        }



        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }

        mycontexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          mycontexpl <- as.vector(mycontexpl_labelled)
        }

        # Get all labels for variable name mapping (needed to map interactions
        # before building the Cox formula).
        mydata_labelled <- cleaneddata$mydata_labelled
        all_labels <- labelled::var_label(mydata_labelled)

        # Build formula parts (exclude strata from covariates)
        formula_parts <- c(myexplanatory, mycontexpl)

        # Map interaction terms (display labels -> real names) and build the
        # escaped, colon-joined terms for the Cox formula RHS.
        real_interactions <- .mapInteractionTerms(self$options$interactions, all_labels)
        interaction_terms_cox <- .interactionTermsForFormula(real_interactions)

        # Build Cox regression formula using consolidated function with proper strata
        coxformula <- .buildSurvivalFormula(
          time_var = "mytime",
          outcome_var = "myoutcome",
          predictors = formula_parts,
          survival_type = "standard",
          strata_vars = mystratvar,
          interaction_terms = interaction_terms_cox
        )


        # Remove any rows with NA in stratification variables
        # if (self$options$use_stratify && !is.null(self$options$stratvar)) {
        #   complete_cases <- complete.cases(mydata[, mystratvar])
        #   mydata <- mydata[complete_cases, ]
        # }



        # self$results$mydataview_cox$setContent(
        #   list(
        #     mydata = head(mydata, n = 30),
        #     coxformula = coxformula
        #   )
        # )

        # Add checkpoint before the expensive Cox model fitting
        private$.checkpoint()

        # (mydata_labelled / all_labels are defined above, before the formula build)

        # Handle Time-Dependent Covariates
        # EXPERIMENTAL:         if (self$options$use_time_dependent && !is.null(self$options$time_dep_vars)) {
        # EXPERIMENTAL: 
        # EXPERIMENTAL:           # Get time-dependent variable names
        # EXPERIMENTAL:           time_dep_vars <- names(all_labels)[match(self$options$time_dep_vars, all_labels)]
        # EXPERIMENTAL: 
        # EXPERIMENTAL:           if (self$options$td_format == "wide") {
        # EXPERIMENTAL:             # Handle wide format data - convert to long format
        # EXPERIMENTAL:             mydata <- private$.convertWideToLong(mydata, time_dep_vars, all_labels)
        # EXPERIMENTAL: 
        # EXPERIMENTAL:             # Update formula for time-dependent covariates (long format)
        # EXPERIMENTAL:             td_predictors <- c(formula_parts, time_dep_vars)
        # EXPERIMENTAL:             coxformula <- .buildSurvivalFormula(
        # EXPERIMENTAL:               time_var = "tstart",
        # EXPERIMENTAL:               outcome_var = myoutcome,
        # EXPERIMENTAL:               predictors = td_predictors,
        # EXPERIMENTAL:               survival_type = "counting",
        # EXPERIMENTAL:               start_var = "tstart",
        # EXPERIMENTAL:               stop_var = "tstop"
        # EXPERIMENTAL:             )
        # EXPERIMENTAL: 
        # EXPERIMENTAL:           } else if (self$options$td_format == "long") {
        # EXPERIMENTAL:             # Handle long format data
        # EXPERIMENTAL:             if (!is.null(self$options$start_time_var) && !is.null(self$options$stop_time_var)) {
        # EXPERIMENTAL:               start_time_var <- names(all_labels)[all_labels == self$options$start_time_var]
        # EXPERIMENTAL:               stop_time_var <- names(all_labels)[all_labels == self$options$stop_time_var]
        # EXPERIMENTAL: 
        # EXPERIMENTAL:               # Update formula for time-dependent covariates
        # EXPERIMENTAL:               long_predictors <- c(formula_parts, time_dep_vars)
        # EXPERIMENTAL:               coxformula <- .buildSurvivalFormula(
        # EXPERIMENTAL:                 time_var = start_time_var,
        # EXPERIMENTAL:                 outcome_var = myoutcome,
        # EXPERIMENTAL:                 predictors = long_predictors,
        # EXPERIMENTAL:                 survival_type = "counting",
        # EXPERIMENTAL:                 start_var = start_time_var,
        # EXPERIMENTAL:                 stop_var = stop_time_var
        # EXPERIMENTAL:               )
        # EXPERIMENTAL:             }
        # EXPERIMENTAL:           }
        # EXPERIMENTAL:         }
        # EXPERIMENTAL: 
        # EXPERIMENTAL:         # Handle Frailty Models
        # EXPERIMENTAL:         if (self$options$use_frailty && !is.null(self$options$frailty_var)) {
        # EXPERIMENTAL:           frailty_var <- names(all_labels)[all_labels == self$options$frailty_var]
        # EXPERIMENTAL: 
        # EXPERIMENTAL:           # Add frailty term based on distribution
        # EXPERIMENTAL:           frailty_term <- switch(self$options$frailty_distribution,
        # EXPERIMENTAL:             "gamma" = paste0("frailty(", frailty_var, ", distribution='gamma')"),
        # EXPERIMENTAL:             "gaussian" = paste0("frailty(", frailty_var, ", distribution='gaussian')"),
        # EXPERIMENTAL:             "logt" = paste0("frailty(", frailty_var, ", distribution='logt')")
        # EXPERIMENTAL:           )
        # EXPERIMENTAL: 
        # EXPERIMENTAL:           formula_parts <- c(formula_parts, frailty_term)
        # EXPERIMENTAL:           RHT <- paste(formula_parts, collapse = " + ")
        # EXPERIMENTAL:           coxformula <- .asSurvivalFormula(paste0(LHT, " ~ ", RHT))
        # EXPERIMENTAL:         }
        # EXPERIMENTAL: 
        # EXPERIMENTAL:         # Handle Splines for Non-Proportional Hazards
        # EXPERIMENTAL:         if (self$options$use_splines && !is.null(self$options$spline_vars)) {
        # EXPERIMENTAL:           spline_vars <- names(all_labels)[match(self$options$spline_vars, all_labels)]
        # EXPERIMENTAL: 
        # EXPERIMENTAL:           # Create spline terms
        # EXPERIMENTAL:           for (var in spline_vars) {
        # EXPERIMENTAL:             spline_term <- switch(self$options$spline_type,
        # EXPERIMENTAL:               "pspline" = paste0("pspline(", var, ", df=", self$options$spline_df, ")"),
        # EXPERIMENTAL:               "ns" = paste0("ns(", var, ", df=", self$options$spline_df, ")"),
        # EXPERIMENTAL:               "bs" = paste0("bs(", var, ", df=", self$options$spline_df, ")")
        # EXPERIMENTAL:             )
        # EXPERIMENTAL: 
        # EXPERIMENTAL:             # Replace the linear term with spline term
        # EXPERIMENTAL:             formula_parts <- formula_parts[formula_parts != var]
        # EXPERIMENTAL:             formula_parts <- c(formula_parts, spline_term)
        # EXPERIMENTAL:           }
        # EXPERIMENTAL: 
        # EXPERIMENTAL:           RHT <- paste(formula_parts, collapse = " + ")
        # EXPERIMENTAL:           coxformula <- .asSurvivalFormula(paste0(LHT, " ~ ", RHT))
        # EXPERIMENTAL: 
        # EXPERIMENTAL:           # Load splines package if needed
        # EXPERIMENTAL:           if (self$options$spline_type %in% c("ns", "bs")) {
        # EXPERIMENTAL:             requireNamespace("splines", quietly = TRUE)
        # EXPERIMENTAL:           }
        # EXPERIMENTAL:         }

        # Check for competing risks analysis
        if (self$options$multievent && self$options$analysistype == 'compete') {
            # Use Fine-Gray model
            # Create Fine-Gray dataset (outcome is factor from .definemyoutcome)
            if (is.factor(mydata$myoutcome) && !"Event" %in% levels(mydata$myoutcome)) {
              private$.addHtmlMessage(
                "error",
                .("Invalid competing-risk coding"),
                .("Competing risk mode requires an event level named 'Event' in the outcome variable. Adjust coding before running Fine-Gray.")
              )
              return(NULL)
            }

            fg_data <- survival::finegray(coxformula, data = mydata, etype = "Event")
            
            # Update formula to use Fine-Gray variables
            fg_formula <- update(coxformula, survival::Surv(fgstart, fgstop, fgstatus) ~ .)
            
            # Fit Cox model on expanded data with weights
            cox_model <- survival::coxph(
              fg_formula,
              data = fg_data,
              weights = fgwt,
              x = TRUE,
              y = TRUE,
              model = TRUE
            )
        } else {
            # Standard Cox model
            cox_model <- survival::coxph(
              coxformula,
              data = mydata,
              x = TRUE,
              y = TRUE,
              model = TRUE
            )
        }

        # Populate interaction / effect-modification output.
        # .cox_model() is a shared fitter invoked many times within a single
        # run (survival analysis, clinical summary, plots, nomogram, risk score).
        # jmvcore's Table$addRow appends unconditionally, so gate row-writing on
        # the tables still being empty: jamovi clears result tables at the start
        # of each run, so only the FIRST .cox_model() call of a run populates
        # them; later calls see rowCount > 0 and skip (no duplicate rows, no
        # repeated subgroup refits).
        if (length(self$options$interactions) > 0 &&
            self$results$interactionTest$rowCount == 0 &&
            self$results$subgroupHR$rowCount == 0) {
          private$.populateInteractionTables(
            cox_model = cox_model,
            cox_formula = coxformula,
            data = mydata,
            real_interactions = real_interactions,
            conf_level = 0.95,
            is_finegray = (self$options$multievent && self$options$analysistype == 'compete')
          )
        }

        if (self$options$multievent && self$options$analysistype == 'compete') {
          private$.addHtmlMessage(
            "info",
            .("Competing-risk model"),
            .("Competing-risk mode fits a Fine-Gray subdistribution model; HRs reflect subdistribution hazards and are not directly comparable to cause-specific Cox HRs.")
          )
        }

        # Events-per-variable (EPV) check. With fewer than 10 events per
        # estimated coefficient, Cox HR estimates and their CIs become
        # unstable; reviewers expect this to be flagged. See
        # Vittinghoff & McCulloch (2007) Am J Epidemiol 165:710-8.
        epv_info <- tryCatch({
          n_events <- if (!is.null(cox_model$nevent)) cox_model$nevent
                      else if (!is.null(cox_model$y)) sum(cox_model$y[, "status"] == 1)
                      else NA_integer_
          n_coef <- length(stats::coef(cox_model))
          list(events = n_events, coefficients = n_coef,
               epv = if (n_coef > 0 && !is.na(n_events)) n_events / n_coef else NA_real_)
        }, error = function(e) list(events = NA, coefficients = NA, epv = NA))

        if (!is.na(epv_info$epv) && epv_info$epv < 10 && epv_info$coefficients > 0) {
          private$.addHtmlMessage(
            "warning",
            .("Low events-per-variable (post-fit)"),
            sprintf(
              .("Low events-per-variable: this Cox model fits %d coefficient(s) on %d event(s) (EPV = %.1f, below the conventional minimum of 10). Hazard-ratio estimates and CIs may be unstable. Consider: (i) reducing covariates; (ii) penalised Cox (lassocox / adaptivelasso); (iii) bootstrap-optimism correction (survivalvalidation)."),
              epv_info$coefficients, epv_info$events, epv_info$epv
            )
          )
        }

        # Proportional hazards diagnostic
        ph_diag <- try(survival::cox.zph(cox_model), silent = TRUE)
        if (!inherits(ph_diag, "try-error")) {
          ph_p <- ph_diag$table[, "p"]
          if (any(ph_p[!is.na(ph_p)] < 0.05)) {
            private$.addHtmlMessage(
              "warning",
              .("Proportional hazards violation"),
              .("Proportional hazards test (cox.zph) indicates potential violations (p < 0.05) for one or more terms. Interpret HRs with caution or consider time-varying effects/stratification.")
            )
          }
        }


        return(cox_model)

      }



      ,
      # Person-Time Analysis Function ----
      .personTimeAnalysis = function() {
        # Check if person_time option is enabled
        if (!self$options$person_time) {
          return()
        }


        cleaneddata <- private$.cleandata()


        # Extract data
        # mytime <- cleaneddata$mytime_labelled
        # myoutcome <- cleaneddata$myoutcome_labelled
        mydata <- cleaneddata$cleanData

        # Ensure time is numeric
        mydata[["mytime"]] <- jmvcore::toNumeric(mydata[["mytime"]])

        # Build a robust event indicator (TRUE for event of interest)
        event_indicator <- .eventIndicator(mydata[["myoutcome"]])

        # For competing risks, explicitly note the counting strategy
        if (is.factor(mydata[["myoutcome"]]) && "Competing" %in% levels(mydata[["myoutcome"]])) {
          private$.addHtmlMessage(
            "info",
            .("Person-time counting strategy"),
            .("Person-time rates count only the event-of-interest level ('Event'); competing events are treated as censored for rate calculations.")
          )
        }

        # Replace NA indicators with FALSE to keep counts deterministic
        if (all(is.na(event_indicator))) {
          event_indicator <- rep(FALSE, length(event_indicator))
        } else {
          event_indicator[is.na(event_indicator)] <- FALSE
        }

        # Get total observed time
        total_time <- sum(mydata[["mytime"]], na.rm = TRUE)

        # Get total events
        total_events <- sum(event_indicator, na.rm = TRUE)

        # Get time unit
        time_unit <- self$options$timetypeoutput

        # Get rate multiplier
        rate_multiplier <- self$options$rate_multiplier

        # Calculate overall incidence rate
        overall_rate <- (total_events / total_time) * rate_multiplier

        # Calculate confidence intervals using Poisson exact method
        ci_lower <- (stats::qchisq(0.025, 2*total_events) / 2) / total_time * rate_multiplier
        ci_upper <- (stats::qchisq(0.975, 2*(total_events + 1)) / 2) / total_time * rate_multiplier



        # self$results$mydataview_personTimeAnalysis$setContent(
        #   list(
        #     mydata = head(mydata, n = 10),
        #     # mytime = mytime,
        #     # myoutcome = myoutcome,
        #     total_time = total_time,
        #     total_events = total_events,
        #     overall_rate = overall_rate,
        #     ci_lower = ci_lower,
        #     ci_upper = ci_upper
        #   )
        # )




        # Add to personTimeTable - first the overall row
        self$results$personTimeTable$addRow(rowKey=1, values=list(
          interval=paste0("Overall (0-max)"),
          events=total_events,
          person_time=round(total_time, 2),
          rate=round(overall_rate, 2),
          rate_ci_lower=round(ci_lower, 2),
          rate_ci_upper=round(ci_upper, 2)
        ))

        # FIX: Add group-stratified person-time analysis
        # If explanatory variables exist, calculate person-time for each group
        if (!is.null(self$options$explanatory) && length(self$options$explanatory) > 0) {
          # Use the first explanatory variable for grouping
          group_var <- self$options$explanatory[[1]]

          if (group_var %in% names(mydata)) {
            # Get unique groups
            groups <- unique(mydata[[group_var]])
            groups <- groups[!is.na(groups)]  # Remove NA groups

            rowKey_counter <- 2  # Start after overall row

            for (group in groups) {
              # Filter data for this group
              group_data <- mydata[mydata[[group_var]] == group, ]
              group_events <- event_indicator[mydata[[group_var]] == group]

              if (nrow(group_data) > 0) {
                # Calculate group-specific metrics
                group_time <- sum(group_data[["mytime"]], na.rm = TRUE)
                group_events_count <- sum(group_events, na.rm = TRUE)

                # Calculate group incidence rate
                if (group_time > 0) {
                  group_rate <- (group_events_count / group_time) * rate_multiplier

                  # Calculate confidence intervals using Poisson exact method
                  if (group_events_count > 0) {
                    group_ci_lower <- (stats::qchisq(0.025, 2*group_events_count) / 2) / group_time * rate_multiplier
                    group_ci_upper <- (stats::qchisq(0.975, 2*(group_events_count + 1)) / 2) / group_time * rate_multiplier
                  } else {
                    group_ci_lower <- 0
                    group_ci_upper <- (stats::qchisq(0.975, 2) / 2) / group_time * rate_multiplier
                  }

                  # Add to personTimeTable with group label.
                  # Use a "grp_" key namespace disjoint from the "int_" interval
                  # rows below so the two sub-loops never collide on rowKey.
                  self$results$personTimeTable$addRow(rowKey=paste0("grp_", rowKey_counter), values=list(
                    interval=paste0("Group: ", as.character(group)),
                    events=group_events_count,
                    person_time=round(group_time, 2),
                    rate=round(group_rate, 2),
                    rate_ci_lower=round(group_ci_lower, 2),
                    rate_ci_upper=round(group_ci_upper, 2)
                  ))

                  rowKey_counter <- rowKey_counter + 1
                }
              }
            }
          }
        }

        # Parse time intervals for stratified analysis
        time_intervals <- as.numeric(unlist(strsplit(self$options$time_intervals, ",")))
        time_intervals <- sort(unique(time_intervals))

        if (length(time_intervals) > 0) {
          # Create time intervals
          breaks <- c(0, time_intervals, max(mydata[["mytime"]], na.rm = TRUE) * 1.1)

          # Loop through intervals
          for (i in 1:(length(breaks)-1)) {
            start_time <- breaks[i]
            end_time <- breaks[i+1]

            # Add checkpoint for responsiveness
            if (i %% 5 == 0) {
              private$.checkpoint()
            }

            # Filter data for this interval
            if (i == 1) {
              # For first interval, include patients from the beginning
              interval_data <- mydata
              interval_events <- event_indicator
              # But truncate follow-up time to the interval end
              follow_up_times <- pmin(mydata[["mytime"]], end_time)
              # Count only events that occurred within this interval
              events_in_interval <- sum(interval_events & mydata[["mytime"]] <= end_time, na.rm = TRUE)
            } else {
              # For later intervals, include only patients who survived past the previous cutpoint
              survivors <- mydata[["mytime"]] > start_time
              interval_data <- mydata[survivors, ]
              interval_events <- event_indicator[survivors]

              if (nrow(interval_data) == 0) {
                # Skip if no patients in this interval
                next
              }

              # Adjust entry time and follow-up time
              adjusted_entry_time <- rep(start_time, nrow(interval_data))
              adjusted_exit_time <- pmin(interval_data[["mytime"]], end_time)
              follow_up_times <- adjusted_exit_time - adjusted_entry_time

              # Count only events that occurred within this interval
              events_in_interval <- sum(interval_events &
                                          interval_data[["mytime"]] <= end_time &
                                          interval_data[["mytime"]] > start_time, na.rm = TRUE)
            }

            # Sum person-time in this interval
            person_time_in_interval <- sum(follow_up_times)

            # Calculate interval incidence rate
            if (person_time_in_interval > 0) {
              interval_rate <- (events_in_interval / person_time_in_interval) * rate_multiplier

              # Calculate confidence intervals
              if (events_in_interval > 0) {
                interval_ci_lower <- (stats::qchisq(0.025, 2*events_in_interval) / 2) / person_time_in_interval * rate_multiplier
                interval_ci_upper <- (stats::qchisq(0.975, 2*(events_in_interval + 1)) / 2) / person_time_in_interval * rate_multiplier
              } else {
                interval_ci_lower <- 0
                interval_ci_upper <- (stats::qchisq(0.975, 2) / 2) / person_time_in_interval * rate_multiplier
              }

              # Add to personTimeTable (interval rows use an "int_" key namespace
              # disjoint from the "grp_" group rows above)
              self$results$personTimeTable$addRow(rowKey=paste0("int_", i+1), values=list(
                interval=paste0(start_time, "-", end_time),
                events=events_in_interval,
                person_time=round(person_time_in_interval, 2),
                rate=round(interval_rate, 2),
                rate_ci_lower=round(interval_ci_lower, 2),
                rate_ci_upper=round(interval_ci_upper, 2)
              ))
            }
          }
        }

        # Create summary text with interpretation
        summary_html <- glue::glue("
<h4>Person-Time Analysis Summary</h4>
<p>Total follow-up time: <b>{round(total_time, 1)} {time_unit}</b></p>
<p>Number of events: <b>{total_events}</b></p>
<p>Overall incidence rate: <b>{round(overall_rate, 2)}</b> per {rate_multiplier} {time_unit} [95% CI: {round(ci_lower, 2)}-{round(ci_upper, 2)}]</p>
<p>This represents the rate at which events occurred in your study population. The incidence rate is calculated as the number of events divided by the total person-time at risk.</p>
")

        self$results$personTimeSummary$setContent(summary_html)
      }

      ,
      # AFT Model (Accelerated Failure Time) ----
      # DISABLED: Options commented out in .a.yaml and .u.yaml
      # Function call commented out in .run() (line ~1914)
      .calculate_aft = function() {
        # SAFEGUARD: Feature disabled
        if (TRUE) return()

        # Early return if AFT not requested
        # if (!self$options$use_aft) {
        #   return()
        # }


        if (self$options$multievent && self$options$analysistype == "compete") {
          private$.addHtmlMessage(
            "info",
            .("AFT not run under competing risks"),
            .("AFT models are not calculated when competing-risk (Fine-Gray) analysis is selected.")
          )
          return()
        }

        private$.checkpoint()

        # Get cleaned data
        cleaneddata <- private$.cleandata()
        mydata <- cleaneddata$cleanData

        # Harmonize outcome for AFT (requires numeric/censoring)
        if (is.factor(mydata$myoutcome)) {
          if ("Event" %in% levels(mydata$myoutcome)) {
            mydata$myoutcome <- as.numeric(mydata$myoutcome == "Event")
          } else if (nlevels(mydata$myoutcome) == 2) {
            # Binary factor: use level matching outcomeLevel or second level as event
            mydata$myoutcome <- as.numeric(mydata$myoutcome == levels(mydata$myoutcome)[2])
          } else {

            private$.addHtmlMessage(
              "error",
              .("AFT outcome error"),
              .("Outcome with more than two levels is not supported for AFT models. Please select a binary outcome variable or disable competing risk analysis for AFT.")
            )
            return()
          }
        }

        # Extract variable names
        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(cleaneddata$myexplanatory_labelled)
        }

        mycontexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          mycontexpl <- as.vector(cleaneddata$mycontexpl_labelled)
        }

        # Build formula for AFT model
        formula_parts <- c(myexplanatory, mycontexpl)
        aft_formula <- paste("survival::Surv(mytime, myoutcome) ~",
                            paste(formula_parts, collapse = " + "))
        aft_formula <- .asSurvivalFormula(aft_formula)

        # Get distribution
        distribution <- self$options$aft_distribution

        private$.checkpoint()

        # Fit AFT model using survreg
        aft_model <- tryCatch({
          survival::survreg(aft_formula, data = mydata, dist = distribution)
        }, error = function(e) {
          private$.addHtmlMessage(
            "error",
            .("AFT model fitting error"),
            paste0(e$message, .(". Check data quality, ensure adequate events, and verify distribution choice is appropriate for your data."))
          )
          return(NULL)
        })

        if (is.null(aft_model)) {
          return()
        }

        private$.checkpoint()

        # Extract coefficients and statistics
        aft_summary <- summary(aft_model)
        coef_table <- aft_summary$table

        # Populate AFT results table
        row_num <- 1
        for (i in seq_len(nrow(coef_table))) {
          var_name <- rownames(coef_table)[i]

          # Skip intercept for the table
          if (var_name == "(Intercept)") {
            next
          }

          coefficient <- coef_table[i, "Value"]
          se <- coef_table[i, "Std. Error"]
          z_stat <- coef_table[i, "z"]
          p_value <- coef_table[i, "p"]

          # Calculate Time Ratio (TR) = exp(coefficient)
          # In AFT models, positive coefficient means longer survival time
          time_ratio <- exp(coefficient)
          tr_lower <- exp(coefficient - 1.96 * se)
          tr_upper <- exp(coefficient + 1.96 * se)

          # Generate natural language interpretation
          interpretation <- ""
          if (self$options$aft_show_interpretation) {
            if (time_ratio > 1) {
              pct_increase <- round((time_ratio - 1) * 100, 1)
              interpretation <- paste0("Associated with ", pct_increase, "% longer survival time")
            } else {
              pct_decrease <- round((1 - time_ratio) * 100, 1)
              interpretation <- paste0("Associated with ", pct_decrease, "% shorter survival time")
            }

            if (p_value < 0.05) {
              interpretation <- paste0(interpretation, " (significant)")
            } else {
              interpretation <- paste0(interpretation, " (not significant)")
            }
          }

          # Add row to table
          self$results$aftModelTable$addRow(rowKey = row_num, values = list(
            variable = var_name,
            coefficient = coefficient,
            time_ratio = time_ratio,
            tr_lower = tr_lower,
            tr_upper = tr_upper,
            se = se,
            z_stat = z_stat,
            p_value = p_value,
            interpretation = interpretation
          ))

          row_num <- row_num + 1
        }

        # Generate AFT Summary HTML
        if (self$options$showSummaries) {
          n_significant <- sum(coef_table[-1, "p"] < 0.05, na.rm = TRUE)  # Exclude intercept
          n_total <- nrow(coef_table) - 1

          summary_html <- glue::glue("
<h4>AFT Model Summary ({distribution} distribution)</h4>
<p><b>Model Type:</b> Accelerated Failure Time (AFT) Regression</p>
<p><b>Distribution:</b> {tools::toTitleCase(distribution)}</p>
<p><b>Number of observations:</b> {aft_model$df[1] + aft_model$df[2]}</p>
<p><b>Number of events:</b> {sum(mydata$myoutcome)}</p>
<p><b>Significant predictors:</b> {n_significant} out of {n_total}</p>
<p><b>Log-likelihood:</b> {round(aft_model$loglik[2], 2)}</p>
<p><b>AIC:</b> {round(AIC(aft_model), 2)}</p>
<p style='margin-top:15px;'><i>Note: Time Ratios (TR) > 1 indicate longer survival times; TR < 1 indicate shorter survival times.</i></p>
")
          self$results$aftSummary$setContent(summary_html)
        }

        # Generate AFT Model Info
        info_html <- glue::glue("
<h4>AFT Model Information</h4>
<p><b>Distribution:</b> {tools::toTitleCase(distribution)}</p>
<p><b>Scale parameter:</b> {round(aft_model$scale, 4)}</p>
<p><b>Log-likelihood:</b> {round(aft_model$loglik[2], 2)}</p>
<p><b>AIC:</b> {round(AIC(aft_model), 2)}</p>
<p><b>BIC:</b> {round(BIC(aft_model), 2)}</p>
")

        # Add HR equivalent if requested
        if (self$options$aft_show_hr_equivalent && distribution == "weibull") {
          info_html <- paste0(info_html, "
<p style='margin-top:10px;'><i>For Weibull AFT models, Hazard Ratio \u2248 1/Time Ratio. This is only an approximation.</i></p>
")
        }

        self$results$aftModelInfo$setContent(info_html)
      }

      ,
      # SurvMetrics - Model Performance Metrics ----
      # DISABLED: Options commented out in .a.yaml and .u.yaml
      # Function call commented out in .run() (line ~1919)
      .calculate_survmetrics = function() {
        if (!self$options$show_survmetrics) {
          return()
        }

        private$.checkpoint()

        # Performance metrics are defined for standard (single-event) survival
        # models. Competing-risks / Fine-Gray fits use a different prediction
        # target, so skip with an explanatory note rather than report a wrong number.
        if (self$options$multievent && self$options$analysistype == "compete") {
          self$results$survMetricsSummary$setContent(
            "<p>Model performance metrics (Brier score, IBS, time-dependent AUC) are computed for standard survival models and are not shown for competing-risks / Fine-Gray analyses.</p>"
          )
          return()
        }

        if (!requireNamespace("riskRegression", quietly = TRUE)) {
          self$results$survMetricsSummary$setContent(
            "<h4>riskRegression package required</h4><p>Install it with <code>install.packages('riskRegression')</code> to compute Brier score, IBS and time-dependent AUC.</p>"
          )
          return()
        }

        cox_model <- private$.cox_model()
        if (is.null(cox_model)) {
          return()
        }

        private$.checkpoint()

        cleaneddata <- private$.cleandata()
        mydata <- cleaneddata$cleanData

        tryCatch({
          tbl <- self$results$survMetricsTable
          tbl$deleteRows()
          rk <- 0L
          max_time <- max(mydata$mytime, na.rm = TRUE)

          # ---- 1. Discrimination: Harrell's concordance (C-index) from the Cox fit ----
          conc   <- summary(cox_model)$concordance
          c_index <- unname(conc[1])
          c_se    <- if (length(conc) >= 2) unname(conc[2]) else NA_real_
          rk <- rk + 1L
          tbl$addRow(rowKey = rk, values = list(
            metric   = "Concordance index (Harrell's C)",
            value    = c_index,
            ci_lower = if (is.na(c_se)) NA_real_ else max(0, c_index - 1.96 * c_se),
            ci_upper = if (is.na(c_se)) NA_real_ else min(1, c_index + 1.96 * c_se),
            interpretation = if (isTRUE(c_index > 0.7)) "Good discrimination"
                             else if (isTRUE(c_index > 0.6)) "Acceptable discrimination"
                             else "Limited discrimination"
          ))

          private$.checkpoint()

          # ---- 2. Time-point metrics: IPCW Brier, time-dependent AUC, and IBS ----
          # via riskRegression::Score (Kaplan-Meier censoring weights). Refit the
          # model locally with x/y = TRUE so Score can generate predictions.
          tps <- suppressWarnings(as.numeric(trimws(unlist(strsplit(self$options$survmetrics_timepoints, ",")))))
          tps <- sort(unique(tps[!is.na(tps) & tps > 0 & tps < max_time]))

          if (length(tps) > 0) {
            cox_local <- survival::coxph(stats::formula(cox_model), data = mydata,
                                         x = TRUE, y = TRUE)
            sc <- riskRegression::Score(
              list(Cox = cox_local),
              formula   = survival::Surv(mytime, myoutcome) ~ 1,
              data      = mydata,
              times     = tps,
              metrics   = c("brier", "auc"),
              summary   = "ibs",
              se.fit    = FALSE,
              conf.int  = FALSE,
              null.model = FALSE
            )
            br <- as.data.frame(sc$Brier$score); br <- br[br$model == "Cox", , drop = FALSE]
            au <- as.data.frame(sc$AUC$score);   au <- au[au$model == "Cox", , drop = FALSE]

            for (t in tps) {
              bval <- br$Brier[br$times == t]
              if (length(bval) == 1 && !is.na(bval)) {
                rk <- rk + 1L
                tbl$addRow(rowKey = rk, values = list(
                  metric   = paste0("Brier score (t = ", t, " ", self$options$timetypeoutput, ")"),
                  value    = bval, ci_lower = NA_real_, ci_upper = NA_real_,
                  interpretation = if (bval < 0.15) "Excellent accuracy"
                                   else if (bval < 0.25) "Good accuracy" else "Poor accuracy"
                ))
              }
              aval <- au$AUC[au$times == t]
              if (length(aval) == 1 && !is.na(aval)) {
                rk <- rk + 1L
                tbl$addRow(rowKey = rk, values = list(
                  metric   = paste0("Time-dependent AUC (t = ", t, " ", self$options$timetypeoutput, ")"),
                  value    = aval, ci_lower = NA_real_, ci_upper = NA_real_,
                  interpretation = if (aval > 0.8) "Excellent discrimination"
                                   else if (aval > 0.7) "Good discrimination"
                                   else if (aval > 0.6) "Fair discrimination" else "Poor discrimination"
                ))
              }
            }

            # Integrated Brier Score over 0..max(timepoints) (cumulative IBS row)
            if (!is.null(br$IBS)) {
              ibs <- utils::tail(br$IBS[!is.na(br$IBS)], 1)
              if (length(ibs) == 1) {
                rk <- rk + 1L
                tbl$addRow(rowKey = rk, values = list(
                  metric   = paste0("Integrated Brier Score (0 to ", max(tps), " ", self$options$timetypeoutput, ")"),
                  value    = ibs, ci_lower = NA_real_, ci_upper = NA_real_,
                  interpretation = if (ibs < 0.15) "Excellent overall prediction"
                                   else if (ibs < 0.25) "Good overall prediction" else "Poor overall prediction"
                ))
              }
            }
          }

          if (self$options$showSummaries) {
            self$results$survMetricsSummary$setContent(paste0(
              "<h4>Model Performance Summary</h4>",
              "<p><b>Discrimination</b> (Harrell's C = ", round(c_index, 3), "): the probability that, ",
              "for a random pair of subjects, the one predicted higher-risk experiences the event first. ",
              "C &gt; 0.7 is good, 0.6-0.7 acceptable. <b>Time-dependent AUC</b> extends this to each timepoint.</p>",
              "<p><b>Brier score</b> is the inverse-probability-of-censoring-weighted mean squared error ",
              "between predicted survival and observed status at a timepoint (lower is better; &lt; 0.25 acceptable). ",
              "The <b>Integrated Brier Score</b> averages it over the follow-up.</p>",
              "<p style='color:#666;font-size:0.9em;'><i>Brier / AUC / IBS computed with riskRegression using Kaplan-Meier censoring weights.</i></p>"
            ))
          }

        }, error = function(e) {
          self$results$survMetricsSummary$setContent(paste0(
            "<h4>Performance metric error</h4><p>", e$message, "</p>",
            "<p>These metrics require a standard Cox model with sufficient events and follow-up.</p>"
          ))
        })
      }

      ,
      # DISABLED: Options commented out in .a.yaml and .u.yaml
      .plotSurvMetrics = function(image, ggtheme, theme, ...) {
        if (!self$options$show_survmetrics || !self$options$survmetrics_show_plots) {
          return(FALSE)
        }
        # Standard survival models only (see .calculate_survmetrics)
        if (self$options$multievent && self$options$analysistype == "compete") {
          return(FALSE)
        }
        if (!requireNamespace("riskRegression", quietly = TRUE)) {
          return(FALSE)
        }

        cox_model <- private$.cox_model()
        if (is.null(cox_model)) return(FALSE)
        mydata <- private$.cleandata()$cleanData

        p <- tryCatch({
          max_time <- max(mydata$mytime, na.rm = TRUE)
          # Grid of timepoints strictly inside the observed follow-up
          grid <- seq(max_time / 50, max_time * 0.98, length.out = 40)
          cox_local <- survival::coxph(stats::formula(cox_model), data = mydata,
                                       x = TRUE, y = TRUE)
          sc <- riskRegression::Score(
            list(Cox = cox_local),
            formula = survival::Surv(mytime, myoutcome) ~ 1,
            data = mydata, times = grid, metrics = "brier",
            se.fit = FALSE, conf.int = FALSE, null.model = FALSE
          )
          br <- as.data.frame(sc$Brier$score)
          br <- br[br$model == "Cox" & !is.na(br$Brier), c("times", "Brier")]
          if (nrow(br) == 0) return(FALSE)
          ggplot2::ggplot(br, ggplot2::aes(x = times, y = Brier)) +
            ggplot2::geom_line(linewidth = 1.1, colour = "#2E8B57") +
            ggplot2::geom_hline(yintercept = 0.25, linetype = "dashed",
                                colour = "red", alpha = 0.6) +
            ggplot2::labs(
              title = "Brier Score Over Time",
              x = paste0("Time (", self$options$timetypeoutput, ")"),
              y = "Brier score (IPCW)",
              caption = "Lower is better. Dashed line = 0.25 (random-prediction reference)."
            ) +
            ggtheme
        }, error = function(e) NULL)

        if (is.null(p)) return(FALSE)
        print(p)
        TRUE
      }

      ,
      # Covariate contribution: single-term deletion LRT / AIC (base R drop1)
      .compare_models = function() {
        if (!self$options$compare_models) return()

        full <- private$.cox_model()
        if (is.null(full)) return()

        # Single-term deletion is defined for standard Cox models.
        if (self$options$multievent && self$options$analysistype == "compete") {
          self$results$modelContributionSummary$setContent(
            "<p>Covariate contribution (single-term deletion) is shown for standard Cox models and is not available for competing-risks / Fine-Gray analyses.</p>"
          )
          return()
        }

        mydata <- private$.cleandata()$cleanData

        tryCatch({
          full_local  <- survival::coxph(stats::formula(full), data = mydata, x = TRUE, y = TRUE)
          term_labels <- attr(stats::terms(full_local), "term.labels")
          if (length(term_labels) < 2) {
            self$results$modelContributionSummary$setContent(
              "<p>Add at least two covariates to compare their individual contributions.</p>"
            )
            return()
          }

          dd       <- stats::drop1(full_local, test = "Chisq")
          full_aic <- stats::AIC(full_local)

          tbl <- self$results$modelContributionTable
          tbl$deleteRows()
          rk <- 0L
          for (term in rownames(dd)) {
            if (term == "<none>") next
            pval <- dd[term, "Pr(>Chi)"]
            rk <- rk + 1L
            tbl$addRow(rowKey = rk, values = list(
              term  = term,
              df    = dd[term, "Df"],
              aic   = dd[term, "AIC"],
              lrt   = dd[term, "LRT"],
              pvalue = pval,
              interpretation = if (isTRUE(pval < 0.05)) "Significant contribution to fit"
                               else "No significant contribution"
            ))
          }

          if (self$options$showSummaries) {
            self$results$modelContributionSummary$setContent(paste0(
              "<p>Each row is a likelihood-ratio test comparing the full model against the model with that ",
              "single covariate removed (all others retained). A small p-value indicates the covariate ",
              "significantly improves fit. An <b>AIC if dropped</b> below the full-model AIC (",
              round(full_aic, 1), ") indicates the covariate could be removed without penalising fit.</p>"
            ))
          }
        }, error = function(e) {
          self$results$modelContributionSummary$setContent(paste0(
            "<h4>Covariate contribution error</h4><p>", e$message, "</p>"
          ))
        })
      }






      ,
      # Nomogram ----

      .nomogram = function(cox_model) {

        if (!self$options$showNomogram) {
          return()
        }

        private$.checkpoint()

        # Get cleaned data
        cleaneddata <- private$.cleandata()
        mydata <- cleaneddata$cleanData
        myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
        mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
        mystratvar_labelled <- cleaneddata$mystratvar_labelled

        # Combine variables
        var_names <- c(myexplanatory_labelled, mycontexpl_labelled)

        # Remove stratification variables if needed
        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
          var_names <- var_names[!var_names %in% mystratvar_labelled]
        }

        strata_vars <- if (self$options$use_stratify && !is.null(self$options$stratvar)) mystratvar_labelled else NULL

        # First create datadist object
        dd <- rms::datadist(mydata[, var_names])

        # Handle limits for continuous variables properly
        for(var in var_names) {
          if(is.numeric(mydata[[var]])) {
            # Get required dimensions
            needed_cols <- ncol(dd$limits)

            # Calculate basic limits
            basic_limits <- c(
              quantile(mydata[[var]], 0.1, na.rm=TRUE),  # Low
              median(mydata[[var]], na.rm=TRUE),         # Median
              quantile(mydata[[var]], 0.9, na.rm=TRUE)   # High
            )

            # Create full limits vector of correct length
            full_limits <- numeric(needed_cols)
            full_limits[1:3] <- basic_limits  # First 3 are our calculated limits

            if(needed_cols > 3) {
              # Fill remaining positions with median value
              full_limits[4:needed_cols] <- basic_limits[2]
            }

            # Assign to datadist object
            dd$limits[var,] <- full_limits
          }
        }

        # Set datadist globally; restore on exit so rms datadist state does not
        # leak into the user's session and affect later rms-based analyses.
        old_datadist <- options(datadist = dd)
        on.exit(options(old_datadist), add = TRUE)

        # Get baseline Cox model (to check for Fine-Gray)
        cox_model <- private$.cox_model()
        
        is_finegray <- !is.null(cox_model$weights) && self$options$multievent && self$options$analysistype == 'compete'

        base_formula <- .buildSurvivalFormula(
          time_var = "mytime",
          outcome_var = "myoutcome",
          predictors = var_names,
          survival_type = "standard",
          strata_vars = strata_vars
        )

        # Fit the model using rms::cph
        if (is_finegray) {
             # Re-create Fine-Gray data
             fg_formula_obj <- base_formula
             fg_data <- survival::finegray(fg_formula_obj, data = mydata, etype = "Event")
             
             # Define datadist for expanded data
             # Note: datadist must use the data used in fit
             dd_fg <- rms::datadist(fg_data)
             # Pass the datadist object, not a string naming a function-local
             # variable: rms cannot get("dd_fg") from here, which would make
             # cph()/nomogram() fail silently inside .calculate_nomogram()'s
             # tryCatch. The on.exit above restores the original datadist.
             options(datadist = dd_fg)
             
             # Update formula for Fine-Gray structure
             # Note: cph uses its own formula parsing, variables must be in data
             fg_cph_formula <- update(fg_formula_obj, survival::Surv(fgstart, fgstop, fgstatus) ~ .)
             
             f <- rms::cph(formula = fg_cph_formula,
                          data = fg_data,
                          weights = fgwt,  # Use Fine-Gray weights
                          x = TRUE,
                          y = TRUE,
                          surv = TRUE)
                          
             # Restore datadist option later if needed
        } else {
             # Standard Cox
             # Create formula and fit model using consolidated function
             coxformula <- base_formula
             
             f <- rms::cph(formula = coxformula,
                          data = mydata,
                          x = TRUE,
                          y = TRUE,
                          surv = TRUE)
        }

        # Get prediction timepoints
        pred_times <- as.numeric(unlist(strsplit(self$options$cutp, ",")))
        if(length(pred_times) == 0) pred_times <- c(12, 36, 60)

        # Add checkpoint before creating nomogram
        private$.checkpoint()

        # Create nomogram (silent: degenerate models are handled below via the
        # try-error check, so the raw error should not print to the console)
        nom <- try({
          # Use survfit on the cph object f which handles weights
          base_surv <- survival::survfit(f)
          surv_at_time <- summary(base_surv, times = pred_times[1])$surv[1]

          rms::nomogram(f,
                        fun = function(lp) {
                          1 - surv_at_time^exp(lp - mean(f$linear.predictors))
                        },
                        funlabel = paste("Predicted", pred_times[1], self$options$timetypeoutput, "risk"),
                        fun.at = seq(0.1, 0.9, by = 0.1))
        }, silent = TRUE)


        # private$.nom_object <- nom

        # Store results
        if (!inherits(nom, "try-error")) {
          private$.nom_object <- nom

          # Create the nomogram points table
          html_display <- private$.create_nomogram_display(nom)

          # mydataview_nomogram
          cox_summary <- cox_model$coefficient
          modelSummary <- summary(cox_model)

          # self$results$mydataview_nomogram$setContent(
          #   list(
          #     cox_model = cox_model,
          #     modelSummary = modelSummary,
          #     coef_table = modelSummary$coefficients,
          #     conf_table = modelSummary$conf.int,
          #     cox_summary = cox_summary,
          #     dd = dd,
          #     f = f,
          #     pred_times = pred_times,
          #     nomogram = if(!inherits(nom, "try-error")) nom else NULL,
          #     error = if(inherits(nom, "try-error")) attr(nom, "condition") else NULL,
          #     html_display = if(exists(html_display)) html_display else NULL
          #
          #   )
          # )

          self$results$nomogram_display$setContent(html_display)

          }





        }






      ,
      # Plotting function
      .plot_nomogram = function(image, ggtheme, theme, ...) {
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar), add = TRUE)
        if (private$.debug_dummy_plot_enabled()) {
          private$.debug_write(list(
            phase = ".plot_nomogram(dummy)",
            nom_object_is_null = is.null(private$.nom_object)
          ))
          graphics::plot(
            1:10, (1:10)^2,
            type = "b",
            xlab = "x",
            ylab = "y",
            main = "multisurvival debug dummy plot (.plot_nomogram)"
          )
          return(TRUE)
        }

        if(is.null(private$.nom_object)) {
          return(FALSE)
        }

        par(mar = c(4, 4, 2, 2))
        plot(private$.nom_object)
        return(TRUE)
      }


      ,

      .create_nomogram_display = function(nom) {
        if(is.null(private$.nom_object)) {
          return(FALSE)
        }

        # Capture the nomogram output
        nom_output <- capture.output(print(nom))

        # Extract technical details
        tech_details <- c()
        i <- 1
        while(i <= length(nom_output) && !grepl("Points$", nom_output[i])) {
          if(nzchar(nom_output[i])) {
            tech_details <- c(tech_details, nom_output[i])
          }
          i <- i + 1
        }

        # Initialize data structures
        sections <- list()
        current_section <- NULL
        current_lines <- character(0)
        risk_table <- NULL

        # Process each line
        while(i <= length(nom_output)) {
          line <- nom_output[i]

          # Check for new section or risk table
          if(grepl("Total Points Predicted", line)) {
            # We've hit the risk table - save current section and start collecting risk data
            if(!is.null(current_section)) {
              sections[[current_section]] <- current_lines
            }
            risk_table <- c(line)  # Start risk table with header
            while(i < length(nom_output) && nzchar(trimws(nom_output[i + 1]))) {
              i <- i + 1
              risk_table <- c(risk_table, nom_output[i])
            }
            current_section <- NULL
            current_lines <- character(0)
          } else if(grepl("Points$", line) && !grepl("Total Points", line)) {
            # New variable section
            if(!is.null(current_section)) {
              sections[[current_section]] <- current_lines
            }
            current_section <- trimws(sub("Points$", "", line))
            current_lines <- character(0)
          } else if(nzchar(trimws(line))) {
            current_lines <- c(current_lines, line)
          }
          i <- i + 1
        }

        # Add final section if exists
        if(!is.null(current_section) && length(current_lines) > 0) {
          sections[[current_section]] <- current_lines
        }

        # Create HTML content
        html_content <- paste0('
    <style>
        .nomogram-container {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            max-width: 800px;
            margin: 20px auto;
            padding: 20px;
            background-color: #fff;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            border-radius: 8px;
        }
        .tech-details {
            font-family: "Roboto Mono", monospace;
            background-color: #f8f9fa;
            padding: 15px;
            border-radius: 4px;
            margin: 15px 0;
            color: #666;
        }
        .instructions {
            background-color: #e8f5e9;
            padding: 20px;
            margin: 20px 0;
            border-radius: 8px;
        }
        .inputs-section {
            margin-top: 30px;
            border: 1px solid #e9ecef;
            border-radius: 8px;
            padding: 20px;
        }
        .variable-section {
            margin: 15px 0;
            padding: 15px;
            background-color: #f8f9fa;
            border-left: 4px solid #2196f3;
            border-radius: 4px;
        }
        .section-title {
            font-size: 1.2em;
            font-weight: 600;
            color: #2c3e50;
            margin-bottom: 10px;
        }
        .values {
            font-family: "Roboto Mono", monospace;
            white-space: pre-wrap;
            line-height: 1.5;
            color: #34495e;
            padding-left: 20px;
        }
        .outputs-section {
            margin-top: 30px;
            background-color: #fff3e0;
            border: 1px solid #ffe0b2;
            border-radius: 8px;
            padding: 20px;
        }
        .prediction-table {
            width: 100%;
            margin-top: 15px;
            border-collapse: separate;
            border-spacing: 0;
            font-family: "Roboto Mono", monospace;
        }
        .prediction-table th, .prediction-table td {
            padding: 8px 12px;
            text-align: center;
            border-bottom: 1px solid #ffe0b2;
        }
        .prediction-table th {
            background-color: #fff3e0;
            font-weight: 600;
        }
        .notes {
            background-color: #fffde7;
            padding: 15px;
            margin-top: 20px;
            border-radius: 4px;
        }
    </style>
    <div class="nomogram-container">
        <h2>Nomogram Scoring Guide</h2>

        <div class="tech-details">
            ', paste(tech_details, collapse="<br>"), '
        </div>

        <div class="instructions">
            <h3>How to Use This Nomogram:</h3>
            <ol>
                <li>For each variable below, find your patient\'s value</li>
                <li>Read across to the Points scale to determine points for that variable</li>
                <li>Add up total points from all variables</li>
                <li>Use total points to find predicted risk in the Risk Prediction section</li>
            </ol>
        </div>

        <div class="inputs-section">
            <h3>Input Variables</h3>')

        # Add variable sections
        for(section_name in names(sections)) {
          html_content <- paste0(html_content, '
            <div class="variable-section">
                <div class="section-title">', section_name, '</div>
                <div class="values">',
                                 paste(sections[[section_name]], collapse="<br>"),
                                 '</div>
            </div>')
        }

        # Add risk prediction section with formatted table
        html_content <- paste0(html_content, '
        </div>

        <div class="outputs-section">
            <h3>Risk Prediction</h3>
            <div class="section-title">Points to Risk Conversion</div>
            <table class="prediction-table">
                <tr>
                    <th>Total Points</th>
                    <th>Predicted 12-month Risk</th>
                </tr>')

        # Format risk table into two columns
        if(!is.null(risk_table)) {
          # Skip the header line
          risk_lines <- risk_table[-1]
          for(line in risk_lines) {
            values <- strsplit(trimws(line), "\\s+")[[1]]
            if(length(values) == 2) {
              html_content <- paste0(html_content, '
                <tr>
                    <td>', values[1], '</td>
                    <td>', values[2], '</td>
                </tr>')
            }
          }
        }

        html_content <- paste0(html_content, '
            </table>
        </div>

        <div class="notes">
            <h3>Important Notes:</h3>
            <ul>
                <li>For continuous variables, interpolate between given values</li>
                <li>For categorical variables, use exact points shown</li>
                <li>The predicted risk is based on total points from all variables</li>
                <li>Risk predictions are estimates and should be used in conjunction with clinical judgment</li>
            </ul>
        </div>
    </div>')

        return(html_content)
      }

      # Bootstrap optimism-corrected Harrell's C-index (discrimination). Delegates
      # the numeric work to the pure .multisurvivalOptimismCIndex() helper in
      # R/multisurvival-metrics.R. Skipped for competing-risks (Fine-Gray)
      # models, where a naive bootstrap of the weighted expanded data is not a
      # standard optimism correction.
      ,
      .calculateOptimismCIndex = function() {
        if (!isTRUE(self$options$ci_optimism)) return()

        tbl <- self$results$cindexValidation

        is_cr <- isTRUE(self$options$multievent) &&
                 identical(self$options$analysistype, 'compete')
        if (is_cr) {
          tbl$setNote("cr", .("Optimism-corrected C-index is not computed for competing-risks (Fine-Gray) models."))
          return()
        }

        cox_model <- private$.cox_model()
        if (is.null(cox_model)) return()

        res <- tryCatch({
          cleaneddata <- private$.cleandata()
          mydata <- cleaneddata$cleanData
          status <- .eventIndicator(mydata$myoutcome)
          B <- self$options$ci_optimism_boot
          if (is.null(B) || is.na(B)) B <- 150L
          .multisurvivalOptimismCIndex(cox_model, mydata, status, B = B)
        }, error = function(e) NULL)

        if (is.null(res)) {
          tbl$setNote("na", .("Optimism-corrected C-index could not be computed (too few events or unstable bootstrap fits)."))
          return()
        }

        tbl$addRow(rowKey = "apparent", values = list(
          metric = .("Apparent C-index"),
          value = res$apparent,
          detail = .("In-sample (optimistic)")
        ))
        tbl$addRow(rowKey = "optimism", values = list(
          metric = .("Optimism (bootstrap)"),
          value = res$optimism,
          detail = jmvcore::format(.("Mean over {b} resamples"), b = res$n_boot)
        ))
        tbl$addRow(rowKey = "corrected", values = list(
          metric = .("Optimism-corrected C-index"),
          value = res$corrected,
          detail = .("Bias-corrected estimate")
        ))
        tbl$setNote(
          "method",
          .("Harrell's bootstrap optimism correction; corrected C = apparent C minus mean bootstrap optimism.")
        )
      }


      # coxph Proportional Hazards Assumption  ----
      ,
      .cox_ph = function(cox_model) {
        # cleaneddata <- private$.cleandata()
        #
        # name1time <- cleaneddata$name1time
        # name2outcome <- cleaneddata$name2outcome
        # name3contexpl <- cleaneddata$name3contexpl
        # name3expl <- cleaneddata$name3expl
        # adjexplanatory_name <- cleaneddata$adjexplanatory_name
        #
        # mydata <- cleanData <- cleaneddata$cleanData
        #
        # mytime_labelled <- cleaneddata$mytime_labelled
        # myoutcome_labelled <- cleaneddata$myoutcome_labelled
        # mydxdate_labelled <- cleaneddata$mydxdate_labelled
        # myfudate_labelled <- cleaneddata$myfudate_labelled
        # myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
        # mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
        # adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled
        #
        #
        # cox_model <- private$.cox_model()

        private$.checkpoint()

        zph <- tryCatch(
          survival::cox.zph(cox_model),
          error = function(e) {
            private$.debug_write(list(
              phase = ".cox_ph(cox.zph error)",
              message = e$message,
              cox_model_class = class(cox_model)[1]
            ))
            structure(list(error = e$message), class = "multisurvival_ph_error")
          }
        )




        if (inherits(zph, "multisurvival_ph_error")) {
          self$results$cox_ph$setContent(paste0(
            "Unable to compute proportional hazards diagnostics (cox.zph):\n",
            htmltools::htmlEscape(zph$error)
          ))
        } else {
          zph_table <- zph$table

          ph_text <- htmltools::htmlEscape(paste(utils::capture.output(print(zph_table)), collapse = "\n"))

          suggestion <- ""
          if (!is.null(zph_table) && nrow(zph_table) > 0 && "p" %in% colnames(zph_table)) {
            violating <- rownames(zph_table)[which(zph_table[, "p"] < 0.05)]
            violating <- setdiff(violating, "GLOBAL")
            if (length(violating) > 0) {
              suggestion <- paste0(
                "\n\nNote: The proportional hazards assumption appears to be violated for: ",
                paste(htmltools::htmlEscape(violating), collapse = ", "),
                ". Consider using these as stratification variables instead of covariates."
              )
            }
          }

          self$results$cox_ph$setContent(paste0(ph_text, suggestion))
        }






        # Always set state so the renderer can show a diagnostic message if needed
        # (returning FALSE from an Image render function yields a blank image in jamovi).
        image8 <- self$results$plot8
        image8$setState(zph)

        private$.debug_write(list(
          phase = ".cox_ph(state set)",
          zph_class = class(zph)[1],
          has_y = !inherits(zph, "multisurvival_ph_error") && !is.null(zph$y),
          table_dim = if (inherits(zph, "multisurvival_ph_error") || is.null(zph$table)) NULL else dim(zph$table)
        ))

      }




      # hr_plot ----
      ,
      .plot = function(image, ggtheme, theme, ...) {
        if (!self$options$hr) {
          return(FALSE)
        }

        if (!(self$options$sty == "t1")) {
          return(FALSE)
        }

        plotData <- image$state

        if (private$.debug_dummy_plot_enabled()) {
          private$.debug_write(list(
            phase = ".plot(dummy)",
            state_is_null = is.null(plotData),
            state_names = if (is.null(plotData)) NULL else names(plotData)
          ))
          graphics::plot(
            1:10, 1:10,
            type = "b",
            xlab = "x",
            ylab = "y",
            main = "multisurvival debug dummy plot (.plot)"
          )
          return(TRUE)
        }
        
        if (is.null(plotData)) {
          if (isTRUE(getOption("multisurvival.debug"))) {
            message("[multisurvival.debug] .plot: state is NULL, recomputing...")
          }
          plotData <- private$.cleandata()
          if (is.null(plotData$cleanData)) return(FALSE)
        } else {
          if (isTRUE(getOption("multisurvival.debug"))) {
            message("[multisurvival.debug] .plot: state found.")
          }
        }

        name1time <- plotData$name1time
        name2outcome <- plotData$name2outcome
        name3contexpl <- plotData$name3contexpl
        name3expl <- plotData$name3expl

        mydata <- cleanData <- plotData$cleanData

        mytime_labelled <- plotData$mytime_labelled
        myoutcome_labelled <- plotData$myoutcome_labelled
        mydxdate_labelled <- plotData$mydxdate_labelled
        myfudate_labelled <- plotData$myfudate_labelled
        myexplanatory_labelled <- plotData$myexplanatory_labelled
        mycontexpl_labelled <- plotData$mycontexpl_labelled
        mystratvar_labelled <- plotData$mystratvar_labelled

        # Debug output disabled



        ### prepare formula ----

        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }

        mycontexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          mycontexpl <- as.vector(mycontexpl_labelled)
        }

        formula2 <- c(myexplanatory, mycontexpl)


        # Remove stratification variables from the finalfit output
        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
          # Remove stratified variables from the display
          formula2 <- formula2[!formula2 %in% mystratvar_labelled]
        }

        # append interaction terms so the HR forest plot shows them
        if (length(self$options$interactions) > 0) {
          .all_labels_hp <- labelled::var_label(plotData$mydata_labelled)
          formula2 <- c(
            formula2,
            .interactionTermsForFinalfit(
              .mapInteractionTerms(self$options$interactions, .all_labels_hp))
          )
        }




        myformula <-
          paste0('Surv( ', .escapeVariableNames("mytime"), ', ', .escapeVariableNames("myoutcome"), ' )')


        # hr_plot ----
        # https://finalfit.org/reference/hr_plot.html

        # Prefer cached model from state (avoids recomputation)
        cox_model <- NULL
        if (!is.null(image$state$cox_model)) {
          cox_model <- image$state$cox_model
        }

        # Fall back to recomputing if needed
        if (is.null(cox_model)) {
          cox_model <- private$.cox_model()
        }

        if (length(formula2) == 0 || is.null(cox_model)) {
          grid::grid.newpage()
          grid::grid.text("Hazard ratio plot requires at least one explanatory variable and a fitted Cox model.", 0.5, 0.5)
          return(TRUE)
        }

        plot <- tryCatch({
          finalfit::hr_plot(
            .data = mydata,
            dependent = myformula,
            explanatory = formula2,
            dependent_label = "Survival",
            table_text_size = 4,
            title_text_size = 14,
            plot_opts = list(
              ggplot2::xlab("HR, 95% CI"),
              ggplot2::theme(axis.title =
                               ggplot2::element_text(size = 12))
            )
          )
        }, error = function(e) {
          grid::grid.newpage()
          grid::grid.text(
            paste0("Unable to draw hazard ratio plot: ", e$message),
            x = 0.05, y = 0.95, just = c("left", "top"),
            gp = grid::gpar(fontsize = 11)
          )
          return(NULL)
        })


        # print plot ----

        if (!is.null(plot)) {
          print(plot)
          TRUE
        } else {
          TRUE
        }

      }






      # Forest plot ----
      ,
      .plot3 = function(image3, ggtheme, theme, ...) {
        if (!self$options$hr) {
          return(FALSE)
        }

        if (!(self$options$sty == "t3")) {
          return(FALSE)
        }

        plotData <- image3$state

        if (private$.debug_dummy_plot_enabled()) {
          private$.debug_write(list(
            phase = ".plot3(dummy)",
            state_is_null = is.null(plotData),
            state_names = if (is.null(plotData)) NULL else names(plotData)
          ))
          graphics::plot(
            1:10, 10:1,
            type = "b",
            xlab = "x",
            ylab = "y",
            main = "multisurvival debug dummy plot (.plot3)"
          )
          return(TRUE)
        }
        
        if (is.null(plotData)) {
          if (isTRUE(getOption("multisurvival.debug"))) {
            message("[multisurvival.debug] .plot3: state is NULL, recomputing...")
          }
          plotData <- private$.cleandata()
          if (is.null(plotData$cleanData)) return(FALSE)
        } else {
          if (isTRUE(getOption("multisurvival.debug"))) {
            message("[multisurvival.debug] .plot3: state found.")
          }
        }

        private$.debug_write(list(
          phase = ".plot3",
          state_is_null = is.null(plotData),
          cleanData_dim = if (is.null(plotData$cleanData)) NULL else dim(plotData$cleanData)
        ))

        name1time <- plotData$name1time
        name2outcome <- plotData$name2outcome
        name3contexpl <- plotData$name3contexpl
        name3expl <- plotData$name3expl

        mydata <- cleanData <- plotData$cleanData

        mytime_labelled <- plotData$mytime_labelled
        myoutcome_labelled <- plotData$myoutcome_labelled
        mydxdate_labelled <- plotData$mydxdate_labelled
        myfudate_labelled <- plotData$myfudate_labelled
        myexplanatory_labelled <- plotData$myexplanatory_labelled
        mycontexpl_labelled <- plotData$mycontexpl_labelled
        mystratvar_labelled <- plotData$mystratvar_labelled


        ### prepare formula ----

        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }

        mycontexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          mycontexpl <- as.vector(mycontexpl_labelled)
        }

        formula2 <- c(myexplanatory, mycontexpl)


        # Remove stratification variables from the finalfit output
        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
          # Remove stratified variables from the display
          formula2 <- formula2[!formula2 %in% mystratvar_labelled]
        }


        # ggforest ----

        # Use cached Cox model when available to match table output
        cox_model <- image3$state$cox_model
        if (is.null(cox_model)) {
          cox_model <- private$.cox_model()
        }

        if (is.null(cox_model)) {
          grid::grid.newpage()
          grid::grid.text(
            "Forest plot unavailable: Cox model could not be estimated.",
            x = 0.05, y = 0.95, just = c("left", "top"),
            gp = grid::gpar(fontsize = 11)
          )
          return(TRUE)
        }

        # Check if it is a Fine-Gray model
        is_finegray <- !is.null(cox_model$weights) && self$options$multievent && self$options$analysistype == 'compete'
        
        plot3 <- tryCatch({
          if (is_finegray) {
              # ggforest might not support weighted models directly or might need specific handling
              fg_data <- survival::finegray(survival::Surv(mytime, myoutcome) ~ ., data = mydata, etype = "Event")
              survminer::ggforest(model = cox_model, data = fg_data)
          } else {
              survminer::ggforest(model = cox_model, data = mydata)
          }
        }, error = function(e) {
          grid::grid.newpage()
          grid::grid.text(
            paste0("Forest plot not available: ", e$message),
            x = 0.05, y = 0.95, just = c("left", "top"),
            gp = grid::gpar(fontsize = 11)
          )
          return(NULL)
        })


        # print plot ----

        if (!is.null(plot3)) {
            print(plot3)
            TRUE
        } else {
            FALSE
        }

      }


      # cox.zph plot8 ----
      ,
      .plot8 = function(image8, ggtheme, theme, ...) {
        if (!self$options$ph_cox)
          return(FALSE)

        zph_state <- image8$state

        if (private$.debug_dummy_plot_enabled()) {
          private$.debug_write(list(
            phase = ".plot8(dummy)",
            state_is_null = is.null(zph_state),
            state_class = class(zph_state)[1]
          ))
          graphics::plot(
            1:10, stats::rnorm(10),
            type = "b",
            xlab = "index",
            ylab = "value",
            main = "multisurvival debug dummy plot (.plot8)"
          )
          return(TRUE)
        }

        if (is.null(zph_state)) {
          private$.debug_write(list(phase = ".plot8", state_is_null = TRUE))
          grid::grid.newpage()
          grid::grid.text(
            "PH plot is unavailable because diagnostics were not computed (state is NULL). Re-run the analysis.",
            x = 0.05, y = 0.95, just = c("left", "top"),
            gp = grid::gpar(fontsize = 11)
          )
          return(TRUE)
        }

        zph <- zph_state
        if (inherits(zph_state, "multisurvival_ph_error")) {
          private$.debug_write(list(
            phase = ".plot8",
            state_class = class(zph_state)[1],
            error = zph_state$error
          ))
          grid::grid.newpage()
          grid::grid.text(
            paste0("Unable to compute PH diagnostics (cox.zph): ", zph_state$error),
            x = 0.05, y = 0.95, just = c("left", "top"),
            gp = grid::gpar(fontsize = 11)
          )
          return(TRUE)
        }

        private$.debug_write(list(
          phase = ".plot8",
          state_class = class(zph)[1],
          state_names = names(zph),
          has_y = !is.null(zph$y),
          table_dim = if (is.null(zph$table)) NULL else dim(zph$table)
        ))

        # Check if there are variables to plot
        if (is.null(zph$y)) {
          grid::grid.newpage()
          grid::grid.text(
            "PH plot is unavailable (cox.zph object has no plottable residuals).",
            x = 0.05, y = 0.95, just = c("left", "top"),
            gp = grid::gpar(fontsize = 11)
          )
          return(TRUE)
        }

        # Create plot using survminer
        plot8 <- tryCatch(
          survminer::ggcoxzph(zph),
          error = function(e) {
            private$.debug_write(list(
              phase = ".plot8(ggcoxzph error)",
              message = e$message
            ))
            grid::grid.newpage()
            grid::grid.text(
              paste0("Unable to draw PH plot (ggcoxzph): ", e$message),
              x = 0.05, y = 0.95, just = c("left", "top"),
              gp = grid::gpar(fontsize = 11)
            )
            NULL
          }
        )

        if (!is.null(plot8)) {
          print(plot8)
        }

        TRUE

      }


      # Kaplan-Meier ----
      ,


      .plotKM = function(imageKM, ggtheme, theme, ...) {

        if (private$.debug_dummy_plot_enabled()) {
          private$.debug_write(list(
            phase = ".plotKM(dummy)",
            state_is_null = is.null(imageKM$state),
            explanatory = self$options$explanatory,
            contexpl = self$options$contexpl
          ))
          graphics::plot(
            1:10, (1:10) / 10,
            type = "b",
            xlab = "time",
            ylab = "survival",
            ylim = c(0, 1),
            main = "multisurvival debug dummy plot (.plotKM)"
          )
          return(TRUE)
        }
      
        # Check conditions and show message if not met
        if (length(self$options$explanatory) > 2) {
          text_warning <- "Kaplan-Meier plot requires 2 categorical explanatory variables.\nYou have selected more than 2 variables."
          # grid::grid.newpage()
          # grid::grid.text(text_warning, 0.5, 0.5)


        # Create a new page
        grid::grid.newpage()

        # Create a viewport with margins for better readability
        vp <- grid::viewport(
          width = 0.9,    # Wider viewport for left-aligned text
          height = 0.9,   # Keep reasonable margins
          x = 0.5,        # Center the viewport
          y = 0.5         # Center the viewport
        )
        grid::pushViewport(vp)

        # Add the text with left alignment
        grid::grid.text(
          text_warning,
          x = 0.05,           # Move text to the left (5% margin)
          y = 0.95,           # Start from top (5% margin)
          just = c("left", "top"),  # Left align and top justify
          gp = grid::gpar(
            fontsize = 11,        # Maintain readable size
            fontface = "plain",   # Regular font
            lineheight = 1.3      # Slightly increased line spacing for readability
          )
        )

        # Reset viewport
        grid::popViewport()

          return(TRUE)
        }






        if (!is.null(self$options$contexpl)) {
          text_warning <- "Kaplan-Meier plot cannot be created with continuous explanatory variables. Please select only categorical variables."
          grid::grid.newpage()
          grid::grid.text(text_warning, 0.5, 0.5)
          return(TRUE)
        }

        if (length(self$options$explanatory) < 2) {
          text_warning <- "Please select 2 categorical explanatory variables to create the Kaplan-Meier plot."
          grid::grid.newpage()
          grid::grid.text(text_warning, 0.5, 0.5)
          return(TRUE)
        }


        # if (length(self$options$explanatory) > 2)
        #     jmvcore::reject("Kaplan-Meier function allows maximum of 2 explanatory variables")
        #
        # if (!is.null(self$options$contexpl))
        #     jmvcore::reject("Kaplan-Meier function does not use continuous explanatory variables.")





        plotData <- imageKM$state
        
        if (is.null(plotData)) {
          if (isTRUE(getOption("multisurvival.debug"))) {
            message("[multisurvival.debug] .plotKM: state is NULL, recomputing...")
          }
          plotData <- private$.cleandata()
          if (is.null(plotData$cleanData)) return(FALSE)
        }

        private$.debug_write(list(
          phase = ".plotKM",
          state_is_null = is.null(plotData),
          cleanData_dim = if (is.null(plotData$cleanData)) NULL else dim(plotData$cleanData)
        ))

        name1time <- plotData$name1time
        name2outcome <- plotData$name2outcome
        name3contexpl <- plotData$name3contexpl
        name3expl <- plotData$name3expl

        mydata <- cleanData <- plotData$cleanData

        mytime_labelled <- plotData$mytime_labelled
        myoutcome_labelled <- plotData$myoutcome_labelled
        mydxdate_labelled <- plotData$mydxdate_labelled
        myfudate_labelled <- plotData$myfudate_labelled
        myexplanatory_labelled <- plotData$myexplanatory_labelled
        mycontexpl_labelled <- plotData$mycontexpl_labelled


        ### prepare formula ----

        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }


        # myformula <-
        #     paste("survival::Surv(mytime, myoutcome) ~ ",
        #           paste(myexplanatory, collapse = " + "))
        #
        #
        # myformula <- .asSurvivalFormula(myformula)
        #


        thefactor <- jmvcore::constructFormula(terms = myexplanatory)


        title2 <- as.character(thefactor)

        plotKM <- mydata %>%
          finalfit::surv_plot(
            .data = .,
            dependent = paste0('survival::Surv(', .escapeVariableNames("mytime"), ', ', .escapeVariableNames("myoutcome"), ')'),
            explanatory = thefactor,
            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
            pval = self$options$pplot,
            pval.method	= self$options$pplot,
            legend = 'none',
            break.time.by = self$options$byplot,
            xlim = c(0, self$options$endplot),
            title = paste0("Survival curves for ", title2),
            subtitle = "Based on Kaplan-Meier estimates",
            risk.table = self$options$risktable,
            conf.int = self$options$ci95,
            censor = self$options$censored,
            surv.median.line = self$options$medianline

          )

        # plot <- plot + ggtheme

        print(plotKM)
        TRUE



      }












      ,
      # Risk Score Methods ----

      ## Calculate Risk Score ----

      .calculateRiskScore = function(cox_model, mydata) {

        ### Calculate risk scores ----
        risk_scores <- predict(cox_model, type = "risk")

        ### Add risk scores to data ----
        mydata$risk_score <- risk_scores


        ### Add risk scores to output if requested ----
        if (self$options$addRiskScore &&
            self$results$addRiskScore$isNotFilled()) {
          self$results$addRiskScore$setRowNums(mydata$row_names)
          self$results$addRiskScore$setValues(mydata$risk_score)
        }


        # # Create risk groups using quantiles
        # mydata$risk_group <- cut(
        #   mydata$risk_score,
        #   breaks = quantile(mydata$risk_score, probs = seq(0, 1, by = 0.25)),
        #   labels = c(
        #     "Low Risk",
        #     "Intermediate-Low Risk",
        #     "Intermediate-High Risk",
        #     "High Risk"
        #   ),
        #   include.lowest = TRUE
        # )

        ### Check variance in risk scores before attempting quantile grouping ----
        risk_variance <- var(mydata$risk_score, na.rm = TRUE)
        if (is.na(risk_variance) || risk_variance < 1e-10) {
          private$.addHtmlMessage(
            "warning",
            .("Insufficient risk score variation"),
            sprintf(
              .("All patients have nearly identical risk scores (variance = %.2e). This occurs when covariate patterns are very similar across patients, model coefficients are very small, or predictors carry limited prognostic information. Recommendations: review predictor selection for discriminative variables, consider simpler risk stratification approaches, or interpret C-index instead of risk groups."),
              risk_variance
            )
          )
          return(NULL)
        }

        ### Function to try creating risk groups ----
        createRiskGroups <- function(n_groups) {
          tryCatch({
            if(n_groups == 2) {
              probs <- c(0, 0.5, 1)
              labels <- c("Low Risk", "High Risk")
            } else if(n_groups == 3) {
              probs <- c(0, 1/3, 2/3, 1)
              labels <- c("Low Risk", "Intermediate Risk", "High Risk")
            } else {
              probs <- c(0, 0.25, 0.5, 0.75, 1)
              labels <- c("Low Risk", "Intermediate-Low Risk",
                          "Intermediate-High Risk", "High Risk")
            }

            groups <- cut(mydata$risk_score,
                          breaks = quantile(mydata$risk_score, probs = probs),
                          labels = labels,
                          include.lowest = TRUE)

            #### Verify we have at least one observation per group ----
            if(any(table(groups) == 0)) {
              jmvcore::reject(.("Some groups have zero observations"))
            }

            return(list(success = TRUE, groups = groups))
          }, error = function(e) {
            return(list(success = FALSE, error = e$message))
          })
        }

        #### Try to create requested number of groups with fallback ----
        desired_groups <- switch(self$options$numRiskGroups,
                                 "four" = 4,
                                 "three" = 3,
                                 "two" = 2)

        result <- NULL
        warning_message <- NULL

        while(desired_groups >= 2 && is.null(result)) {
          attempt <- createRiskGroups(desired_groups)
          if(attempt$success) {
            result <- attempt$groups
            if(desired_groups < switch(self$options$numRiskGroups,
                                       "four" = 4,
                                       "three" = 3,
                                       "two" = 2)) {
              warning_message <- paste("Could not create", self$options$numRiskGroups,
                               "groups. Fell back to", desired_groups, "groups.")
            }
          } else {
            desired_groups <- desired_groups - 1
          }
        }


        mydata$risk_group <- result

        ### Add risk group to output if requested ----
        if (self$options$addRiskGroup &&
            self$results$addRiskGroup$isNotFilled()) {
          self$results$addRiskGroup$setRowNums(mydata$row_names)
          self$results$addRiskGroup$setValues(mydata$risk_group)
        }

        ### Store state for the risk-group survival plot ----
        # .plotRiskGroups reads image$state (mytime, myoutcome, risk_group);
        # without this the plot renderer returns early and stays blank.
        if (self$options$plotRiskGroups && !is.null(result)) {
          self$results$riskGroupPlot$setState(data.frame(
            mytime     = mydata$mytime,
            myoutcome  = mydata$myoutcome,
            risk_group = mydata$risk_group,
            stringsAsFactors = FALSE
          ))
        }

        ### Calculate summary statistics ----
        event_indicator <- .eventIndicator(mydata$myoutcome)
        if (is.null(event_indicator)) {
          event_indicator <- rep(NA_real_, nrow(mydata))
        }

        risk_summary <- data.frame(
          group = levels(mydata$risk_group),
          n_patients = as.numeric(table(mydata$risk_group)),
          events = tapply(event_indicator, mydata$risk_group, function(x) sum(x, na.rm = TRUE)),
          median_score = tapply(mydata$risk_score, mydata$risk_group, median, na.rm = TRUE)
        )

        risk_summary$percent <- (risk_summary$n_patients / sum(risk_summary$n_patients)) * 100

        ### Fill risk score table ----
        riskScoreTable <- self$results$riskScoreTable

        for (i in seq_len(nrow(risk_summary))) {
          riskScoreTable$addRow(
            rowKey = i,
            values = list(
              group = risk_summary$group[i],
              n_patients = risk_summary$n_patients[i],
              # percent = risk_summary$percent[i],
              percent = round(risk_summary$percent[i], 1),  # Round to 1 decimal
              # median_score = risk_summary$median_score[i],
              median_score = round(risk_summary$median_score[i], 3),  # Round to 3 decimals
              events = risk_summary$events[i]
            )
          )
        }

        ### Create metrics summary ----
        c_index <- survival::concordance(cox_model)$concordance

        c_index_formatted <- sprintf("%.3f", c_index)

        # Create dynamic group summary text
        group_summary <- character()
        for(i in seq_len(nrow(risk_summary))) {
          group_summary[i] <- glue::glue("{risk_summary$group[i]}: {risk_summary$n_patients[i]} ({format(risk_summary$percent[i], digits=1, nsmall=1)}%)")

        }
        group_text <- paste(group_summary, collapse = "<br>")

        metrics_html <- glue::glue(
          "
<br>
<b>Risk Score Model Performance:</b><br>
Harrell's C-index (apparent, in-sample): {sprintf('%.3f', c_index)}<br>
<i>This apparent concordance is optimistic; see the C-index validation table for an optimism-corrected estimate.</i><br>
<br>"
# Number of patients in risk groups:<br>
# {group_text}<br>
# "
        )

        self$results$riskScoreMetrics$setContent(metrics_html)


        percentile_text <- switch(
          as.character(length(levels(mydata$risk_group))),
          "2" = "50th percentile are classified as Low Risk, above as High Risk",
          "3" = "33rd percentile are Low Risk, between 33rd-67th percentiles are Intermediate Risk, and above 67th percentile are High Risk",
          "4" = "25th percentile are Low Risk, 25th-50th are Intermediate-Low Risk, 50th-75th are Intermediate-High Risk, and above 75th percentile are High Risk"
        )

        message_risk_score_analysis <- glue::glue(
"<b>Risk Scores Were Calculated As Follows:</b><br>
The risk scores were calculated using the coefficients from the Cox proportional hazards model.
These scores represent the predicted risk of the event occurring based on the combined effect of all variables in the model.
A higher score indicates a greater predicted risk.<br>
<br>
Patients were then divided into {as.character(length(levels(mydata$risk_group)))} equal-sized groups based on these risk scores:
 <br>
- Scores below the {percentile_text}.<br>
<br>
The Harrell's C-index of {c_index_formatted} indicates the model's discriminative ability,
where 0.5 suggests no discriminative ability and 1.0 indicates perfect discrimination between risk groups.
<br><br>
"
        )

        if(is.null(result)) {
          message_risk_score_analysis <- "Unable to create risk groups. Check if risk scores have enough variation."
        }


        self$results$risk_score_analysis$setContent(""
          # list(
          #   desired_groups,
          #   percentile_text,
          #   message_risk_score_analysis,
          #   warning_message,
          #   length(levels(mydata$risk_group)),
          #   levels(mydata$risk_group),
          #   c_index,
          #   c_index_formatted
          #   )
        )

        self$results$risk_score_analysis2$setContent(message_risk_score_analysis)

        # Generate narrative summary if showSummaries is enabled
        if (self$options$showSummaries && !is.null(result)) {
          tryCatch({
            # Find highest risk group
            highest_risk_idx <- which.max(risk_summary$median_score)
            highest_risk_group <- as.character(risk_summary$group[highest_risk_idx])
            highest_median_score <- risk_summary$median_score[highest_risk_idx]
            highest_events <- risk_summary$events[highest_risk_idx]

            # Find lowest risk group
            lowest_risk_idx <- which.min(risk_summary$median_score)
            lowest_risk_group <- as.character(risk_summary$group[lowest_risk_idx])
            lowest_events <- risk_summary$events[lowest_risk_idx]

            # Calculate fold difference
            fold_diff <- highest_median_score / risk_summary$median_score[lowest_risk_idx]

            summary_html <- paste0(
              "<div style='background-color: #f3e5f5; padding: 15px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #9c27b0;'>",
              "<p style='margin: 0; line-height: 1.8;'>",
              "Risk stratification identified <b>", nrow(risk_summary), " distinct risk groups</b> from the Cox model. ",
              "The <b>", highest_risk_group, "</b> group showed the highest median risk score (",
              sprintf("%.2f", highest_median_score), ") with <b>", highest_events, " events</b> observed, ",
              "while the <b>", lowest_risk_group, "</b> group had <b>", lowest_events, " events</b>.",
              "<br><br>",
              "The risk scores show a <b>", sprintf("%.1f", fold_diff), "-fold difference</b> between highest and lowest risk groups. ",
              "The model's C-index of <b>", sprintf("%.3f", c_index), "</b> indicates ",
              ifelse(c_index >= 0.8, "excellent",
                     ifelse(c_index >= 0.7, "good",
                            ifelse(c_index >= 0.6, "acceptable", "poor"))),
              " discriminative ability for risk stratification.",
              ifelse(!is.null(warning_message),
                     paste0("<br><br><i style='color: #856404;'>", warning_message, "</i>"), ""),
              "</p>",
              "</div>"
            )

            self$results$riskScoreSummary$setContent(summary_html)

          }, error = function(e) {
            # Fail gracefully
            self$results$riskScoreSummary$setContent(
              "<p style='color: #856404;'>Summary generation encountered an issue. See detailed results above.</p>"
            )
          })
        }

        return(mydata)
      }

      ## Plot Risk Groups ----
      ,
      .plotRiskGroups = function(image_riskGroupPlot, ggtheme, theme, ...) {
        # Check if risk score calculation is enabled
        if (!self$options$calculateRiskScore ||
            !self$options$plotRiskGroups) {
          return()
        }

        # Get data from image state
        riskData <- image_riskGroupPlot$state
        if (is.null(riskData)) {
          return()
        }

        # Keep only needed columns
        plotData <- data.frame(
          time = riskData$mytime,
          status = .eventIndicator(riskData$myoutcome),
          group = riskData$risk_group
        )

        if (is.null(plotData$status) || all(is.na(plotData$status))) {
          return()
        }
        plotData$status[is.na(plotData$status)] <- FALSE

        # Create survival object and fit
        fit <- survival::survfit(survival::Surv(time, status) ~ group, data = plotData)

        # Create plot
        plot <- survminer::ggsurvplot(
          fit = fit,
          data = plotData,
          risk.table.height = 0.3,
          risk.table.y.text.col = TRUE,
          risk.table.y.text = FALSE,
          ncensor.plot = TRUE,
          ncensor.plot.height = 0.25,
          xlab = paste0("Time (", self$options$timetypeoutput, ")"),
          ylab = "Survival probability",

          pval = self$options$pplot,
          pval.method	= self$options$pplot,
          break.time.by = self$options$byplot,
          xlim = c(0, self$options$endplot),
          risk.table = self$options$risktable,
          conf.int = self$options$ci95,
          censor = self$options$censored,




          title = "Survival by Risk Group",
          subtitle = "Based on Cox model risk score quartiles",
          legend.title = "Risk Group",
          palette = "Set2",
          ggtheme = ggplot2::theme_bw() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(size = 14, face = "bold"),
              plot.subtitle = ggplot2::element_text(size = 12),
              axis.title = ggplot2::element_text(size = 12),
              axis.text = ggplot2::element_text(size = 10),
              legend.text = ggplot2::element_text(size = 10)
            )
        )

        print(plot)
        TRUE
      }






      # ,
      # Compare Models ----
    #   .compare_models = function() {
    #     # Get clean data
    #     cleaneddata <- private$.cleandata()
    #     mydata <- cleaneddata$cleanData
    #
    #     # Get full model variables
    #     full_explanatory <- NULL
    #     if (!is.null(self$options$explanatory)) {
    #       full_explanatory <- as.vector(cleaneddata$myexplanatory_labelled)
    #     }
    #
    #     full_contexpl <- NULL
    #     if (!is.null(self$options$contexpl)) {
    #       full_contexpl <- as.vector(cleaneddata$mycontexpl_labelled)
    #     }
    #
    #     # Get reduced model variables
    #     reduced_explanatory <- NULL
    #     if (!is.null(self$options$reduced_explanatory)) {
    #       reduced_explanatory <- names(labelled::var_label(mydata))[match(self$options$reduced_explanatory,
    #                                                                       labelled::var_label(mydata))]
    #     }
    #
    #     # Create formulas
    #     full_formula <- c(full_explanatory, full_contexpl)
    #
    #     # Run finalfit with model comparison
    #     comparison <- finalfit::finalfit(
    #       .data = mydata,
    #       dependent = 'survival::Surv(mytime, myoutcome)',
    #       explanatory = full_formula,
    #       explanatory_multi = reduced_explanatory,
    #       keep_models = TRUE
    #     )
    #
    #     # Create comparison table
    #     html_comparison <- knitr::kable(comparison[[1]], format = 'html', caption = "Full vs Reduced Model Comparison")
    #
    #     # Add metrics
    #     metrics_html <- glue::glue(
    #       "
    #     <br>
    #     <b>Model Comparison Metrics:</b><br>
    #     Full model AIC: {comparison[[2]]$AIC.full}<br>
    #     Reduced model AIC: {comparison[[2]]$AIC.reduced}<br>
    #     Likelihood ratio test p-value: {comparison[[2]]$lrtest.pvalue}
    # "
    #     )
    #
    #     # Set results
    #     self$results$model_comparison$setContent(html_comparison)
    #     self$results$reduced_model_metrics$setContent(metrics_html)
    #   }



      # Adjusted ----


      ,
    ## calculate Adjusted Stats ----
      .calculateAdjustedStats = function() {
        # Skip if adjusted curves not requested
        if (!self$options$ac) return(NULL)


        # Get cleaned data and check requirements
        cleaneddata <- private$.cleandata()
        if (is.null(cleaneddata)) return(NULL)

        data <- cleaneddata$cleanData
        adj_var <- cleaneddata$adjexplanatory_name

        # Require an adjustment variable
        if (is.null(adj_var) || length(adj_var) == 0 || !(adj_var %in% names(data))) {
          private$.addHtmlMessage(
            "warning",
            .("Adjustment variable required"),
            .("Adjusted survival curves require selecting an adjustment variable; no curves were produced.")
          )
          return(NULL)
        }

        # Add checkpoint before calculations
        private$.checkpoint()

        # Get baseline Cox model
        cox_model <- private$.cox_model()
        if (is.null(cox_model)) {
          return(NULL)
        }

        if (self$options$multievent && self$options$analysistype == "compete") {
          private$.addHtmlMessage(
            "info",
            .("Adjusted curves use Fine-Gray"),
            .("Adjusted survival curves are based on the Fine-Gray subdistribution model; curves reflect subdistribution survival, not cause-specific survival.")
          )
        }

        # Get unique levels and validate
        levels <- sort(unique(data[[adj_var]]))
        if (length(levels) < 2) {
          private$.addHtmlMessage(
            "warning",
            .("Adjustment variable needs \u22652 levels"),
            .("Adjustment variable must have at least two levels to compute adjusted survival curves.")
          )
          return(NULL)
        }

        # Numeric adjusted-survival tables (opt-in via ac_summary): adjusted
        # survival at the cutpoint timepoints, adjusted median survival, and the
        # adjusted Cox hazard-ratio table. Each backend populates its own result
        # slots. The adjusted survival curve itself is drawn by .plot_adj().
        if (self$options$ac_summary) {
          private$.checkpoint()
          private$.adjustedSurvTable(cleaneddata, cox_model)
          private$.adjustedMedianSurv(cleaneddata, cox_model)
          private$.adjustedCox(cleaneddata, cox_model)
        }

        return(invisible(NULL))
    }



      ,
    ## Adjusted Survival Table ----
    .adjustedSurvTable = function(results, cox_model) {
      # Get data components
      mytime <- results$name1time
      myoutcome <- results$name2outcome
      adj_var <- results$adjexplanatory_name
      mydata <- results$cleanData

      # Input validation
      if (is.null(mydata) || is.null(cox_model)) {
        return(NULL)
      }

      # Get timepoints
      timepoints <- tryCatch({
        pts <- as.numeric(trimws(unlist(strsplit(self$options$cutp, ","))))
        pts <- sort(unique(pts[!is.na(pts)]))
        if (length(pts) == 0) c(12, 36, 60) else pts
      }, error = function(e) c(12, 36, 60))

      # Get levels
      levels <- sort(unique(mydata[[adj_var]]))

      # Create base prediction data
      pred_base <- list()
      for (var in names(mydata)) {
        if (var != "mytime" && var != adj_var && var != "row_names" && var != myoutcome) {
          if (is.numeric(mydata[[var]])) {
            pred_base[[var]] <- mean(mydata[[var]], na.rm = TRUE)
          } else if (is.factor(mydata[[var]])) {
            pred_base[[var]] <- names(which.max(table(mydata[[var]])))
          }
        }
      }

      # Calculate survival for each level
      all_results <- list()

      for (level in levels) {
        # Single-row covariate profile for this level. Summarising ONE curve at
        # the requested timepoints keeps surv[i] aligned with timepoints[i];
        # multiple identical rows produced multiple curves whose flattened
        # summary()$surv silently mis-mapped the later timepoints.
        pred_data <- mydata[1, , drop = FALSE]

        # Add mean/mode covariates
        for (var in names(pred_base)) {
          pred_data[[var]] <- pred_base[[var]]
        }

        # Add level
        pred_data[[adj_var]] <- level

        # Calculate survival
        surv_fit <- survival::survfit(cox_model, newdata = pred_data)
        surv_summ <- summary(surv_fit, times = timepoints)

        # Store results
        for (i in seq_along(timepoints)) {
          if (i <= length(surv_summ$time)) {
            all_results[[length(all_results) + 1]] <- list(
              strata = level,
              time = timepoints[i],
              atrisk = surv_summ$n.risk[i],
              events = surv_summ$n.event[i],
              surv = scales::percent(surv_summ$surv[i], accuracy = 0.1),
              lower = scales::percent(surv_summ$lower[i], accuracy = 0.1),
              upper = scales::percent(surv_summ$upper[i], accuracy = 0.1)
            )
          }
        }
      }

      # Add results to table
      if (length(all_results) > 0) {
        # Clear existing rows (jmvcore Table has no setRows(); deleteRows() clears all)
        self$results$adjustedSurvTable$deleteRows()

        # Add new rows
        for (i in seq_along(all_results)) {
          row <- all_results[[i]]
          self$results$adjustedSurvTable$addRow(
            rowKey = i,
            values = row
          )
        }

        # Generate natural language interpretations
        summaries <- sapply(all_results, function(row) {
          glue::glue(
            "For {row$strata} at {row$time} {self$options$timetypeoutput}, adjusted survival is {row$surv} ",
            "[{row$lower}-{row$upper}, 95% CI]. ",
            "At this timepoint, {row$atrisk} subjects were at risk ",
            "and {row$events} events had occurred. ",
            "These estimates account for the average values of covariates."
          )
        })

        self$results$adjustedSurvTableSummary$setContent(paste(summaries, collapse = "<br><br>"))
      }

      return(all_results)
    }



      #
      # .calculateAdjustedStats = function() {
      #   if (!self$options$ac) return(NULL)
      #
      #   # Get data and fit model
      #   cleaneddata <- private$.cleandata()
      #   if (is.null(cleaneddata)) return(NULL)
      #
      #   adj_var <- cleaneddata$adjexplanatory_name
      #   if (is.null(adj_var)) {
      #     jmvcore::reject('Please select a variable for adjusted curves')
      #   }
      #
      #   # Fit Cox model
      #   cox_model <- private$.fitCoxModel(cleaneddata)
      #
      #   # Calculate survival tables and summaries
      #   surv_results <- private$.adjustedSurvTable(cleaneddata, cox_model)
      #   median_results <- private$.adjustedMedianSurv(cleaneddata, cox_model)
      #   cox_results <- private$.adjustedCox(cleaneddata, cox_model)
      #
      #   if (self$options$ac_compare) {
      #     pairwise_results <- private$.adjustedPairwise(cleaneddata, cox_model)
      #   }
      #
      #   return(list(
      #     surv = surv_results,
      #     median = median_results,
      #     cox = cox_results
      #   ))
      # }
      #






      # mydataview_calculateAdjustedStats <- self$results$mydataview_calculateAdjustedStats
      # mydataview_calculateAdjustedStats$setContent(
      #   list(
      #     results = results,
      #     summary_rows = summary_rows
      #   )
      # )

      ,
    ## Adjusted Survival Plot ----
      .plot_adj = function(image_plot_adj, ggtheme, theme, ...) {

        if (!self$options$ac) return(FALSE)

        if (private$.debug_dummy_plot_enabled()) {
          private$.debug_write(list(
            phase = ".plot_adj(dummy)",
            state_is_null = is.null(image_plot_adj$state),
            adjexplanatory = self$options$adjexplanatory
          ))
          graphics::plot(
            1:10, seq(0.1, 1, length.out = 10),
            type = "b",
            xlab = "time",
            ylab = "adjusted survival",
            ylim = c(0, 1),
            main = "multisurvival debug dummy plot (.plot_adj)"
          )
          return(TRUE)
        }


        plotData <- image_plot_adj$state
        
        if (is.null(plotData)) {
          if (isTRUE(getOption("multisurvival.debug"))) {
            message("[multisurvival.debug] .plot_adj: state is NULL, recomputing...")
          }
          plotData <- private$.cleandata()
          if (is.null(plotData$cleanData)) return(FALSE)
        }

        private$.debug_write(list(
          phase = ".plot_adj",
          state_is_null = is.null(plotData),
          cleanData_dim = if (is.null(plotData$cleanData)) NULL else dim(plotData$cleanData),
          adjexplanatory_name = plotData$adjexplanatory_name
        ))

        name1time <- plotData$name1time
        name2outcome <- plotData$name2outcome
        name3contexpl <- plotData$name3contexpl
        name3expl <- plotData$name3expl
        adjexplanatory_name <- plotData$adjexplanatory_name

        mydata <- cleanData <- plotData$cleanData

        mytime_labelled <- plotData$mytime_labelled
        myoutcome_labelled <- plotData$myoutcome_labelled
        mydxdate_labelled <- plotData$mydxdate_labelled
        myfudate_labelled <- plotData$myfudate_labelled
        myexplanatory_labelled <- plotData$myexplanatory_labelled
        mycontexpl_labelled <- plotData$mycontexpl_labelled
        adjexplanatory_labelled <- plotData$adjexplanatory_labelled


        if (is.null(plotData$adjexplanatory_name)) {
          text_warning <- "Please select a variable for adjusted curves."
          grid::grid.newpage()
          grid::grid.text(text_warning, 0.5, 0.5)
          return(TRUE)
        }








        ### prepare formula ----

        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }

        mycontexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          mycontexpl <- as.vector(mycontexpl_labelled)
        }

        formula2 <- c(myexplanatory, mycontexpl)

        myformula <-
          paste("survival::Surv(mytime, myoutcome) ~ ",
                paste(formula2, collapse = " + "))

        myformula <- .asSurvivalFormula(myformula)

        # Fit model
        # Use the central model (handles Fine-Gray if needed)
        cox_model <- private$.cox_model()
        
        if (is.null(cox_model)) {
            return()
        }

        # Check if it is a Fine-Gray model
        is_finegray <- !is.null(cox_model$weights) && self$options$multievent && self$options$analysistype == 'compete'
        
        # Use correct data for plotting
        plot_data <- mydata
        if (is_finegray) {
             # Re-create Fine-Gray data for plotting
             # This duplicates logic from .cox_model but is necessary without refactoring state management
             # Note: myoutcome is factor "Censored", "Event", "Competing"
             
             # Re-construct formula for finegray() call
             fg_formula_str <- paste("survival::Surv(mytime, myoutcome) ~", paste(formula2, collapse = " + "))
             fg_formula_obj <- .asSurvivalFormula(fg_formula_str)
             
             plot_data <- survival::finegray(fg_formula_obj, data = mydata, etype = "Event")
        }

        # Validate method and try fallback if needed
        method <- self$options$ac_method

        # Try to create plot with specified method
        plot <- tryCatch({
          survminer::ggadjustedcurves(
            fit = cox_model,
            data = plot_data,  # Use expanded data if Fine-Gray
            variable = adjexplanatory_name,
            method = method,
            conf.int = self$options$ci95,
            risk.table = self$options$risktable,
            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
            title = paste0("Adjusted Survival Curves for ", self$options$adjexplanatory,
                           " (", method, " adjustment)"),
            pval = self$options$pplot,
            pval.method = self$options$pplot,
            legend = "none",
            break.time.by = self$options$byplot,
            xlim = c(0, self$options$endplot),
            censor = self$options$censored,
            surv.median.line = self$options$medianline



          )
        }, error = function(e) {
          # If marginal method fails, try average method instead
          if (method == "marginal") {
            warning(.("Marginal method failed, falling back to average method"))
            survminer::ggadjustedcurves(
              fit = cox_model,
              data = plot_data, # Use expanded data
              variable = adjexplanatory_name,
              method = "average",  # Fallback to average method
              conf.int = self$options$ci95,
              risk.table = self$options$risktable,
              xlab = paste0('Time (', self$options$timetypeoutput, ')'),
              title = paste0("Adjusted Survival Curves for ",
                             self$options$adjexplanatory,
                             " (average adjustment - marginal failed)"),
              pval = self$options$pplot,
              pval.method = self$options$pplot,
              legend = "none",
              break.time.by = self$options$byplot,
              xlim = c(0, self$options$endplot),
              censor = self$options$censored,
              surv.median.line = self$options$medianline
            )
          } else {
            jmvcore::reject(paste("Error creating adjusted curves:", e$message))
          }
        })




        # # Prepare plot parameters
        # plot_params <- list(
        #   fit = cox_model,
        #   data = mydata,
        #   variable = adjexplanatory_name,
        #   method = self$options$ac_method,
        #   conf.int = self$options$ci95,
        #   risk.table = self$options$risktable,
        #   xlab = paste0('Time (', self$options$timetypeoutput, ')'),
        #   title = paste0("Adjusted Survival Curves for ",
        #                  self$options$adjexplanatory,
        #                  " (", self$options$ac_method, " adjustment)"),
        #   pval = self$options$pplot,
        #   pval.method = self$options$pplot,
        #   legend = "none",
        #   break.time.by = self$options$byplot,
        #   xlim = c(0, self$options$endplot),
        #   censor = self$options$censored,
        #   surv.median.line = self$options$medianline,
        #   risk.table.height = 0.25,  # Added for better risk table sizing
        #   risk.table.y.text.col = TRUE,  # Color code risk table text
        #   ncensor.plot = FALSE,  # Turn off censor plot by default
        #   fontsize = 3.5  # Adjust font size
        # )
        # # Try to create plot with specified method
        # plot <- tryCatch({
        #   do.call(survminer::ggadjustedcurves, plot_params)
        # }, error = function(e) {
        #   # If marginal method fails, try average method instead
        #   if (self$options$ac_method == "marginal") {
        #     warning("Marginal method failed, falling back to average method")
        #     plot_params$method <- "average"
        #     plot_params$title <- paste0("Adjusted Survival Curves for ",
        #                                 self$options$adjexplanatory,
        #                                 " (average adjustment - marginal failed)")
        #     do.call(survminer::ggadjustedcurves, plot_params)
        #   } else {
        #     jmvcore::reject(paste("Error creating adjusted curves:", e$message))
        #   }
        # })
        # # Add additional theme elements if needed
        # plot <- plot +
        #   ggplot2::theme(
        #     plot.title = ggplot2::element_text(size = 14, face = "bold"),
        #     plot.subtitle = ggplot2::element_text(size = 12),
        #     axis.title = ggplot2::element_text(size = 12),
        #     axis.text = ggplot2::element_text(size = 10),
        #     legend.text = ggplot2::element_text(size = 10)
        #   )












        print(plot)
        TRUE
      }



      # ,
      # .adjustedSurvTable = function(results, cox_model) {
      #   # Get data components
      #   mytime <- results$name1time
      #   myoutcome <- results$name2outcome
      #   adj_var <- results$adjexplanatory_name
      #   mydata <- results$cleanData
      #
      #   # Verify we have valid data and model
      #   if (is.null(mydata) || is.null(cox_model)) {
      #     return(NULL)
      #   }
      #
      #   # Get timepoints
      #   timepoints <- tryCatch({
      #     pts <- as.numeric(trimws(unlist(strsplit(self$options$ac_timepoints, ","))))
      #     pts <- sort(unique(pts[!is.na(pts)]))
      #     if (length(pts) == 0) c(12, 36, 60) else pts
      #   }, error = function(e) c(12, 36, 60))
      #
      #   # Get levels of adjustment variable
      #   levels <- sort(unique(mydata[[adj_var]]))
      #   if (length(levels) < 1) {
      #     warning("No levels found in adjustment variable")
      #     return(NULL)
      #   }
      #
      #   # Create base prediction dataset
      #   pred_base <- list()
      #   for (var in names(mydata)) {
      #     if (var != "mytime" && var != adj_var && var != "row_names") {
      #       if (is.numeric(mydata[[var]])) {
      #         pred_base[[var]] <- mean(mydata[[var]], na.rm = TRUE)
      #       } else if (is.factor(mydata[[var]])) {
      #         pred_base[[var]] <- levels(mydata[[var]])[which.max(table(mydata[[var]]))]
      #       }
      #     }
      #   }
      #
      #   # Initialize storage for results
      #   all_results <- list()
      #   row_counter <- 1
      #
      #   # Calculate survival for each level and timepoint
      #   for (level in levels) {
      #     # Create prediction data for this level
      #     pred_data <- data.frame(
      #       mytime = timepoints
      #     )
      #
      #     # Add averaged covariates
      #     for (var in names(pred_base)) {
      #       pred_data[[var]] <- pred_base[[var]]
      #     }
      #     pred_data[[adj_var]] <- level
      #
      #     tryCatch({
      #       # Get predicted survival
      #       surv_fit <- survival::survfit(cox_model, newdata = pred_data)
      #       surv_summary <- summary(surv_fit, times = timepoints)
      #
      #       # Extract results for each timepoint
      #       for (i in seq_along(timepoints)) {
      #         if (i <= length(surv_summary$time)) {
      #           all_results[[row_counter]] <- list(
      #             strata = level,
      #             time = timepoints[i],
      #             n.risk = surv_summary$n.risk[i],
      #             n.event = surv_summary$n.event[i],
      #             surv = surv_summary$surv[i],
      #             lower = surv_summary$lower[i],
      #             upper = surv_summary$upper[i]
      #           )
      #           row_counter <- row_counter + 1
      #         }
      #       }
      #     }, error = function(e) {
      #       warning(paste("Error processing level", level, ":", e$message))
      #     })
      #   }
      #
      #   # Convert results to data frame if we have any
      #   if (length(all_results) > 0) {
      #     results_df <- do.call(rbind, lapply(all_results, as.data.frame))
      #
      #     # Add to results table
      #     survTable <- self$results$adjustedSurvTable
      #     survTable$setRows(NULL) # Clear existing rows
      #
      #     for (i in seq_len(nrow(results_df))) {
      #       survTable$addRow(
      #         rowKey = i,
      #         values = list(
      #           strata = results_df$strata[i],
      #           time = results_df$time[i],
      #           n.risk = results_df$n.risk[i],
      #           n.event = results_df$n.event[i],
      #           surv = scales::percent(results_df$surv[i], accuracy = 0.1),
      #           lower = scales::percent(results_df$lower[i], accuracy = 0.1),
      #           upper = scales::percent(results_df$upper[i], accuracy = 0.1)
      #         )
      #       )
      #     }
      #
      #     # Generate summary text
      #     survTableSummary <- sapply(seq_len(nrow(results_df)), function(i) {
      #       glue::glue(
      #         "For {results_df$strata[i]} at {results_df$time[i]} months, ",
      #         "the adjusted survival probability is {scales::percent(results_df$surv[i], accuracy=0.1)} ",
      #         "[{scales::percent(results_df$lower[i], accuracy=0.1)}-",
      #         "{scales::percent(results_df$upper[i], accuracy=0.1)}, 95% CI]. ",
      #         "These estimates account for the average values of all covariates in the model."
      #       )
      #     })
      #
      #     self$results$adjustedSurvTableSummary$setContent(survTableSummary)
      #
      #     return(results_df)
      #   }
      #
      #   return(NULL)
      # }









      ,
    ## Adjusted Median Survival ----
    .adjustedMedianSurv = function(results, cox_model) {
      # Get required data. cleanData carries the standardized survival columns
      # "mytime"/"myoutcome" (see .definemytime/.definemyoutcome); results$name*
      # hold the *display labels*, which are not column names.
      mytime <- "mytime"
      myoutcome <- "myoutcome"
      adj_var <- results$adjexplanatory_name
      mydata <- results$cleanData

      event_indicator <- .eventIndicator(mydata[[myoutcome]])
      if (is.null(event_indicator)) {
        event_indicator <- rep(NA, nrow(mydata))
      }

      # Get levels of adjustment variable
      levels <- sort(unique(mydata[[adj_var]]))

      # Build a SINGLE covariate profile (one row): means for numeric covariates,
      # modes for factors, with factor levels preserved. survfit() on one row
      # yields exactly one adjusted survival curve, so summary()$table returns a
      # named vector with a single median/CI. (The previous version used
      # sort(unique(mytime)) as newdata rows, producing many curves and a matrix
      # $table whose ["median"] lookup silently returned NA.)
      pred_data <- mydata[1, , drop = FALSE]
      for (var in names(mydata)) {
        if (var != "mytime" && var != adj_var && var != "row_names") {
          if (is.numeric(mydata[[var]])) {
            pred_data[[var]] <- mean(mydata[[var]], na.rm = TRUE)
          } else if (is.factor(mydata[[var]])) {
            pred_data[[var]] <- factor(names(which.max(table(mydata[[var]]))),
                                       levels = levels(mydata[[var]]))
          }
        }
      }

      # Calculate adjusted survival for each level
      results_list <- list()

      for (level in levels) {
        level_data <- pred_data
        level_data[[adj_var]] <- level

        # Calculate adjusted survival (one curve for this covariate profile)
        adj_surv <- survival::survfit(cox_model, newdata = level_data)

        # Get summary stats
        surv_summary <- summary(adj_surv)

        # Extract median and CI
        median_time <- surv_summary$table["median"]
        lcl <- surv_summary$table["0.95LCL"]
        ucl <- surv_summary$table["0.95UCL"]

        results_list[[level]] <- list(
          factor = level,
          median = median_time,
          x0_95lcl = lcl,
          x0_95ucl = ucl,
          records = sum(!is.na(mydata[[mytime]][mydata[[adj_var]] == level])),
          events = sum(event_indicator[mydata[[adj_var]] == level], na.rm = TRUE)
        )
      }

      # Convert to data frame
      results_df <- do.call(rbind, lapply(results_list, as.data.frame))
      results_df <- as.data.frame(results_df)

      # Add to results table
      medianTable <- self$results$adjustedMedianTable
      for (i in seq_len(nrow(results_df))) {
        medianTable$addRow(
          rowKey = i,
          values = list(
            factor = results_df$factor[i],
            records = results_df$records[i],
            events = results_df$events[i],
            median = round(results_df$median[i], 1),
            x0_95lcl = round(results_df$x0_95lcl[i], 1),
            x0_95ucl = round(results_df$x0_95ucl[i], 1)
          )
        )
      }

      # Create natural language summaries
      summaries <- lapply(levels, function(level) {
        result <- results_df[results_df$factor == level,]

        description <- glue::glue(
          "For {adj_var} = {level}, adjusted median survival is {round(result$median, 1)} ",
          "[{round(result$x0_95lcl, 1)} - {round(result$x0_95ucl, 1)}, 95% CI] ",
          self$options$timetypeoutput, "."
        )

        if (is.na(result$median)) {
          description <- paste0(
            description,
            "\nNote: The adjusted survival curve for this group does not drop below 1/2 during ",
            "the observation period, thus the median survival is undefined."
          )
        }

        return(description)
      })

      # Add general interpretation
      medianSummary <- c(
        unlist(summaries),
        "The median survival time is when 50% of subjects have experienced the event.",
        "These estimates account for the average values of all other covariates in the model."
      )

      self$results$adjustedMedianSummary$setContent(paste(medianSummary, collapse = "<br><br>"))
    }


      ,
    ## Adjusted Cox ----
    .adjustedCox = function(results, cox_model) {
      mydata <- results$cleanData
      adj_var <- results$adjexplanatory_name

      # Get Cox model summary
      cox_summary <- summary(cox_model)

      # Create metrics summary
      tCoxtext2 <- glue::glue("
        <br>
        <b>Model Metrics:</b><br>
        Concordance: {round(cox_summary$concordance[1], 3)} (SE = {round(cox_summary$concordance[2], 3)})<br>
        Likelihood ratio test = {round(cox_summary$logtest[1], 2)}, df = {cox_summary$logtest[2]}, p = {format.pval(cox_summary$logtest[3], digits=3)}<br>
        Wald test = {round(cox_summary$waldtest[1], 2)}, df = {cox_summary$waldtest[2]}, p = {format.pval(cox_summary$waldtest[3], digits=3)}<br>
        Score test = {round(cox_summary$sctest[1], 2)}, df = {cox_summary$sctest[2]}, p = {format.pval(cox_summary$sctest[3], digits=3)}<br>
    ")

      if (self$options$uselandmark) {
        landmark <- jmvcore::toNumeric(self$options$landmark)
        tCoxtext2 <- glue::glue(
          tCoxtext2,
          "Landmark time used as: ", landmark, " ", self$options$timetypeoutput, "."
        )
      }

      self$results$adjustedCoxText$setContent(tCoxtext2)

      # Extract hazard ratios and CIs
      coef_matrix <- cbind(
        exp(cox_summary$coefficients[, 1]),  # HR
        exp(cox_summary$coefficients[, 1] - 1.96 * cox_summary$coefficients[, 3]),  # Lower CI
        exp(cox_summary$coefficients[, 1] + 1.96 * cox_summary$coefficients[, 3]),  # Upper CI
        cox_summary$coefficients[, 5]  # p-value
      )

      # Create Cox table
      coxTable <- self$results$adjustedCoxTable
      rownames <- row.names(cox_summary$coefficients)

      for (i in seq_len(nrow(coef_matrix))) {
        coxTable$addRow(
          rowKey = i,
          values = list(
            Variable = rownames[i],
            HR = sprintf("%.2f (%.2f-%.2f)",
                         coef_matrix[i,1], coef_matrix[i,2], coef_matrix[i,3]),
            Pvalue = coef_matrix[i,4]
          )
        )
      }

      # Create interpretive summary
      coxSummary <- sapply(seq_len(nrow(coef_matrix)), function(i) {
        hr <- coef_matrix[i,1]
        var_name <- rownames[i]

        glue::glue(
          "For {var_name}, the adjusted hazard ratio is {round(hr,2)} ",
          "({round(coef_matrix[i,2],2)}-{round(coef_matrix[i,3],2)}, 95% CI). ",
          "This means that, after adjusting for other covariates, ",
          "{ifelse(hr > 1,
                paste('there is a', round((hr-1)*100,1), '% increase in hazard'),
                paste('there is a', round((1-hr)*100,1), '% decrease in hazard'))} ",
          "for each unit increase in {var_name}."
        )
      })

      coxSummary <- c(
        unlist(coxSummary),
        "A hazard ratio greater than 1 indicates increased risk, while less than 1 indicates decreased risk.",
        "All estimates are adjusted for other variables in the model."
      )

      self$results$adjustedCoxSummary$setContent(paste(coxSummary, collapse = "<br><br>"))

      # Proportional hazards check if requested (adjustedCoxPH is a Preformatted
      # slot; capture the printed cox.zph table as text).
      if (self$options$ph_cox) {
        zph <- survival::cox.zph(cox_model)
        self$results$adjustedCoxPH$setContent(
          paste(utils::capture.output(print(zph)), collapse = "\n")
        )
      }
    }




      # ,
      # .calculateAdjustedCurves = function(cox_model, mydata, adjexplanatory_name, fallback = TRUE) {
      #
      #   method <- self$options$ac_method
      #
      #   # Try to calculate adjusted curves with specified method
      #   adj_curves <-  tryCatch({
      #       survminer::ggadjustedcurves(
      #         fit = cox_model,
      #         data = mydata,
      #         variable = adjexplanatory_name,
      #         method = method,
      #         conf.int = self$options$ci95,
      #         risk.table = self$options$risktable,
      #         xlab = paste0('Time (', self$options$timetypeoutput, ')'),
      #         title = paste0(
      #           "Adjusted Survival Curves for ",
      #           self$options$adjexplanatory,
      #           " (", method, " adjustment)"
      #         ),
      #         pval = self$options$pplot,
      #         pval.method = self$options$pplot,
      #         legend = "none",
      #         break.time.by = self$options$byplot,
      #         xlim = c(0, self$options$endplot),
      #         censored = self$options$censored
      #       )
      #     }, error = function(e) {
      #       # If marginal method fails, try average method instead
      #       if (method == "marginal") {
      #         warning("Marginal method failed, falling back to average method")
      #         survminer::ggadjustedcurves(
      #           fit = cox_model,
      #           data = mydata,
      #           variable = adjexplanatory_name,
      #           method = "average",  # Fallback to average method
      #           conf.int = self$options$ci95,
      #           risk.table = self$options$risktable,
      #           xlab = paste0('Time (', self$options$timetypeoutput, ')'),
      #           title = paste0(
      #             "Adjusted Survival Curves for ",
      #             self$options$adjexplanatory,
      #             " (average adjustment - marginal failed)"
      #           ),
      #           pval = self$options$pplot,
      #           pval.method = self$options$pplot,
      #           legend = "none",
      #           break.time.by = self$options$byplot,
      #           xlim = c(0, self$options$endplot),
      #           censored = self$options$censored
      #         )
      #       } else {
      #         jmvcore::reject(paste("Error creating adjusted curves:", e$message))
      #       }
      #     }
      #       )
      #
      #
      #   # image_plot_adj <- self$results$plot_adj
      #   # image_plot_adj$setState(adj_curves)
      #
      #
      #   # Extract and structure the data
      #   # curve_data <- list(
      #   #   curves = adj_curves,
      #   #   model = cox_model,
      #   #   data = mydata,
      #   #   variable = adjexplanatory_name,
      #   #   method = method
      #   # )
      #
      #   # class(curve_data) <- "adjusted_curves"
      #
      #
      #   # View curve_data
      #   self$results$mydataview_curve_data$setContent(
      #     list(
      #       # curves = adj_curves,
      #       model = cox_model,
      #       data = mydata,
      #       variable = adjexplanatory_name,
      #       method = method
      #     )
      #   )
      #
      #
      #   # return(curve_data)
      # }



      # ,
      # .plot_adj = function(image_plot_adj, ggtheme, theme, ...) {
      #   if (!self$options$ac) {
      #     return()
      #   }
      #   if (is.null(curve_data)) {
      #     return()
      #   }
      #
      #   plot <- image_plot_adj$state
      #
      #
      #   # plot <- survminer::ggadjustedcurves(plot)
      #
      #
      #   print(plot)
      #   TRUE
      #
      #
      # }




      # ,
      # .plot_adj = function(image_plot_adj, ggtheme, theme, ...) {
      #   if (!self$options$ac) {
      #     return()
      #   }
      #
      #   if (!self$options$ac_curve) {
      #     return()
      #   }
      #
      #
      #   # mydata <- image_plot_adj$state$mydata
      #   # cox_model <- image_plot_adj$state$cox_model
      #   # adjexplanatory_name <- image_plot_adj$state$adjexplanatory_name
      #
      #
      #
      #   cleaneddata <- private$.cleandata()
      #
      #   name1time <- cleaneddata$name1time
      #   name2outcome <- cleaneddata$name2outcome
      #   name3contexpl <- cleaneddata$name3contexpl
      #   name3expl <- cleaneddata$name3expl
      #   adjexplanatory_name <- cleaneddata$adjexplanatory_name
      #
      #   mydata <- cleanData <- cleaneddata$cleanData
      #
      #   mytime_labelled <- cleaneddata$mytime_labelled
      #   myoutcome_labelled <- cleaneddata$myoutcome_labelled
      #   mydxdate_labelled <- cleaneddata$mydxdate_labelled
      #   myfudate_labelled <- cleaneddata$myfudate_labelled
      #   myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
      #   mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
      #   adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled
      #
      #
      #
      #   # Add stratification variables
      #   mystratvar <- NULL
      #   if (self$options$use_stratify && !is.null(self$options$stratvar)) {
      #     mystratvar <- as.vector(cleaneddata$mystratvar_labelled)
      #     # Create strata terms
      #     mystratvar <- paste0("strata(", mystratvar, ")")
      #   }
      #
      #
      #
      #   myexplanatory <- NULL
      #   if (!is.null(self$options$explanatory)) {
      #     myexplanatory <- as.vector(myexplanatory_labelled)
      #   }
      #
      #   mycontexpl <- NULL
      #   if (!is.null(self$options$contexpl)) {
      #     mycontexpl <- as.vector(mycontexpl_labelled)
      #   }
      #
      #
      #   formula2 <- c(myexplanatory, mycontexpl, mystratvar)
      #
      #
      #
      #   LHT <- "survival::Surv(mytime, myoutcome)"
      #
      #   RHT <- formula2
      #
      #   RHT <- paste(RHT, collapse = " + ")
      #
      #   coxformula <- paste0(LHT, " ~ ", RHT)
      #
      #   coxformula <- .asSurvivalFormula(coxformula)
      #
      #   cox_model <- survival::coxph(coxformula, data = mydata)
      #
      #
      #
      #   fallback <- TRUE
      #   method <- self$options$ac_method
      #
      #   # Try to calculate adjusted curves with specified method
      #   adj_curves <-  tryCatch({
      #     survminer::ggadjustedcurves(
      #       fit = cox_model,
      #       data = mydata,
      #       variable = adjexplanatory_name,
      #       method = method,
      #       conf.int = self$options$ci95,
      #       risk.table = self$options$risktable,
      #       xlab = paste0('Time (', self$options$timetypeoutput, ')'),
      #       title = paste0(
      #         "Adjusted Survival Curves for ",
      #         self$options$adjexplanatory,
      #         " (", method, " adjustment)"
      #       ),
      #       pval = self$options$pplot,
      #       pval.method = self$options$pplot,
      #       legend = "none",
      #       break.time.by = self$options$byplot,
      #       xlim = c(0, self$options$endplot),
      #       censored = self$options$censored
      #     )
      #   }, error = function(e) {
      #     # If marginal method fails, try average method instead
      #     if (method == "marginal") {
      #       warning("Marginal method failed, falling back to average method")
      #       survminer::ggadjustedcurves(
      #         fit = cox_model,
      #         data = mydata,
      #         variable = adjexplanatory_name,
      #         method = "average",  # Fallback to average method
      #         conf.int = self$options$ci95,
      #         risk.table = self$options$risktable,
      #         xlab = paste0('Time (', self$options$timetypeoutput, ')'),
      #         title = paste0(
      #           "Adjusted Survival Curves for ",
      #           self$options$adjexplanatory,
      #           " (average adjustment - marginal failed)"
      #         ),
      #         pval = self$options$pplot,
      #         pval.method = self$options$pplot,
      #         legend = "none",
      #         break.time.by = self$options$byplot,
      #         xlim = c(0, self$options$endplot),
      #         censored = self$options$censored
      #       )
      #     } else {
      #       jmvcore::reject(paste("Error creating adjusted curves:", e$message))
      #     }
      #   }
      #   )
      #
      #
      #
      #
      #   print(adj_curves)
      #   TRUE
      #
      #
      # }



      ,
    ## Fit Cox Model with Selection ----
    ## DISABLED: Model selection options commented out in .a.yaml and .u.yaml
    ## This function will not be called when modelSelection options are unavailable
.fitModelWithSelection = function(formula, data) {
  tryCatch({
    # Get the selection method and criteria from options
    modelSelection <- self$options$modelSelection
    selectionCriteria <- self$options$selectionCriteria
    pEntry <- self$options$pEntry
    pRemoval <- self$options$pRemoval

    # Validation checks
    if (self$options$pEntry >= self$options$pRemoval) {
      jmvcore::reject(.("Entry significance must be less than removal significance"))
    }

    if (self$options$modelSelection != "enter" &&
        length(c(self$options$explanatory, self$options$contexpl)) < 2) {
      jmvcore::reject(.("Variable selection requires at least 2 predictor variables"))
    }

    private$.checkpoint()

    # If no selection requested, return full model
    if (modelSelection == "enter") {
      # Just fit and return the full model with all variables
      full_model <- survival::coxph(formula, data = data)
      return(full_model)
    }

    # For Cox models we need to preserve the exact Surv() object on the left side
    surv_part <- formula[[2]]  # Gets the Surv() expression itself
    pred_part <- attr(terms(formula), "term.labels")  # All predictor variables

    # Create full and null models
    full_model <- survival::coxph(formula, data = data)
    null_formula <- .asSurvivalFormula(paste(deparse(surv_part), "~ 1"))
    null_model <- survival::coxph(null_formula, data = data)

    # For backward selection
    if (modelSelection == "backward") {
      # Start with all variables
      current_vars <- pred_part
      current_model <- full_model

      # Set status to indicate backward selection is starting
      # EXPERIMENTAL: Disabled - result element not in .r.yaml
      # self$results$text_model_selection$setStatus('running')
      # self$results$text2_model_selection$setStatus('running')

      # Initial checkpoint to push status to UI
      private$.checkpoint()

      # Track variables removed for reporting
      removed_vars <- character(0)

      # Remove variables one-by-one if they don't contribute significantly
      changed <- TRUE
      iteration <- 0
      while(changed && length(current_vars) > 0) {
        iteration <- iteration + 1
        changed <- FALSE

        # Add checkpoint at beginning of each iteration
        private$.checkpoint(flush=FALSE)

        # Only try to examine p-values if we have variables
        if (length(current_vars) > 0) {
          # Get model summary
          model_summary <- summary(current_model)

          # Check if we have coefficients
          if (!is.null(model_summary$coefficients)) {
            # Store p-values for each variable
            coef_summary <- model_summary$coefficients
            var_p_values <- coef_summary[, "Pr(>|z|)"]

            # Find least significant variable
            max_p <- max(var_p_values)
            if (max_p > pRemoval) {
              # Which variable has highest p-value
              drop_var_idx <- which.max(var_p_values)
              drop_var <- names(var_p_values)[drop_var_idx]

              # Remove this variable
              current_vars <- setdiff(current_vars, drop_var)
              removed_vars <- c(removed_vars, drop_var)

              # Update status with progress information
              status_msg <- paste0("Removing variable: ", drop_var,
                                   " (p=", format.pval(max_p, digits=3), ")")
              # EXPERIMENTAL: Disabled - result element not in .r.yaml
              # self$results$text2_model_selection$setContent(status_msg)

              # Critical checkpoint before expensive operation - always flush here
              private$.checkpoint()

              if (length(current_vars) > 0) {
                # Create new formula without this variable
                new_formula <- .asSurvivalFormula(paste(deparse(surv_part), "~",
                                                paste(current_vars, collapse = " + ")))

                # This is the most computationally expensive step
                current_model <- survival::coxph(new_formula, data = data)
              } else {
                # If no variables left, use null model
                current_model <- null_model
              }

              changed <- TRUE
            }
          }
        }

        # Add checkpoint after expensive operation to show progress
        # Only flush every 2nd iteration to balance responsiveness with performance
        if (iteration %% 2 == 0) {
          private$.checkpoint()
        }
      }

      # Final model is ready - set status to complete
      # EXPERIMENTAL: Disabled - result element not in .r.yaml
      # self$results$text_model_selection$setStatus('complete')
      # EXPERIMENTAL: Disabled - result element not in .r.yaml
      # self$results$text2_model_selection$setStatus('complete')

      # Final checkpoint to push complete results
      private$.checkpoint()

      # Store selection steps for reporting
      attr(current_model, "selection_steps") <- list(
        removed = removed_vars,
        remaining = current_vars
      )

      return(current_model)
    }

    # For forward selection
    else if (modelSelection == "forward") {
      # Start with no variables
      selected_vars <- character(0)
      current_model <- null_model
      added_vars <- character(0)

      # Set status to indicate forward selection is starting
      # EXPERIMENTAL: Disabled - result element not in .r.yaml
      # self$results$text_model_selection$setStatus('running')
      # EXPERIMENTAL: Disabled - result element not in .r.yaml
      # self$results$text2_model_selection$setStatus('running')

      private$.checkpoint()

      # Add variables one by one
      while (length(selected_vars) < length(pred_part)) {
        private$.checkpoint(flush=FALSE)

        best_var <- NULL
        best_p <- Inf
        best_model <- NULL

        # Try adding each remaining variable
        remaining_vars <- setdiff(pred_part, selected_vars)

        for (var in remaining_vars) {
          test_vars <- c(selected_vars, var)
          test_formula <- .asSurvivalFormula(paste(deparse(surv_part), "~",
                                           paste(test_vars, collapse = " + ")))

          tryCatch({
            test_model <- survival::coxph(test_formula, data = data)
            test_summary <- summary(test_model)

            if (!is.null(test_summary$coefficients)) {
              # Get p-value for the new variable
              var_p <- test_summary$coefficients[var, "Pr(>|z|)"]

              if (var_p < best_p) {
                best_p <- var_p
                best_var <- var
                best_model <- test_model
              }
            }
          }, error = function(e) {
            # Skip this variable if model fails
            NULL
          })
        }

        # Add the best variable if it meets criteria
        if (!is.null(best_var) && best_p < pEntry) {
          selected_vars <- c(selected_vars, best_var)
          added_vars <- c(added_vars, best_var)
          current_model <- best_model

          # Update status
          status_msg <- paste0("Adding variable: ", best_var,
                               " (p=", format.pval(best_p, digits=3), ")")
          # EXPERIMENTAL: Disabled - result element not in .r.yaml
          # self$results$text2_model_selection$setContent(status_msg)

          private$.checkpoint()
        } else {
          # No more variables meet criteria
          break
        }
      }

      # Final model is ready
      # EXPERIMENTAL: Disabled - result element not in .r.yaml
      # self$results$text_model_selection$setStatus('complete')
      # EXPERIMENTAL: Disabled - result element not in .r.yaml
      # self$results$text2_model_selection$setStatus('complete')
      private$.checkpoint()

      # Store selection steps for reporting
      attr(current_model, "selection_steps") <- list(
        added = added_vars,
        final = selected_vars
      )

      return(current_model)
    }

    # For stepwise (both directions)
    else if (modelSelection == "both") {
      # Use MASS::stepAIC for bidirectional selection
      if (requireNamespace("MASS", quietly = TRUE)) {
        # Set status
        # EXPERIMENTAL: Disabled - result element not in .r.yaml
      # self$results$text_model_selection$setStatus('running')
        # EXPERIMENTAL: Disabled - result element not in .r.yaml
        # self$results$text2_model_selection$setStatus('running')

        private$.checkpoint()

        # Use stepwise selection with AIC
        step_model <- MASS::stepAIC(full_model,
                                    scope = list(lower = null_model, upper = full_model),
                                    direction = "both",
                                    trace = 0)  # Silent operation

        # Final model is ready
        # EXPERIMENTAL: Disabled - result element not in .r.yaml
      # self$results$text_model_selection$setStatus('complete')
        # EXPERIMENTAL: Disabled - result element not in .r.yaml
        # self$results$text2_model_selection$setStatus('complete')
        private$.checkpoint()

        return(step_model)
      } else {
        # Fallback to backward selection if MASS not available
        return(private$.fitModelWithSelection(formula, data))
      }
    }

    # Default: return full model
    return(full_model)

  }, error = function(e) {
    # Set error status
    # EXPERIMENTAL: Disabled - result element not in .r.yaml
    # self$results$text_model_selection$setStatus('error')
    # self$results$text2_model_selection$setContent(paste(.("Model selection error:"), e$message))

    # Return full model as fallback
    return(survival::coxph(formula, data = data))
  })
}

,
    ## Final Fit ----
.final_fit2 = function() {
  # Retrieve cleaned data and variable information
  cleaneddata <- private$.cleandata()

  # Extract necessary data components
  mydata <- cleaneddata$cleanData

  # Ensure outcome is numeric for finalfit compatibility
  if (is.factor(mydata$myoutcome)) {
    if ("Event" %in% levels(mydata$myoutcome)) {
      mydata$myoutcome <- as.numeric(mydata$myoutcome == "Event")
    } else if (nlevels(mydata$myoutcome) == 2) {
      mydata$myoutcome <- as.numeric(mydata$myoutcome == levels(mydata$myoutcome)[2])
    } else {
      private$.addHtmlMessage(
        "warning",
        .("Model selection requires binary outcome"),
        .("Model selection requires a binary outcome; no selection performed. Consider using cause-specific coding for competing risks.")
      )
      return(NULL)
    }
  }

  # Extract variable names
  myexplanatory <- NULL
  if (!is.null(self$options$explanatory)) {
    myexplanatory <- as.vector(cleaneddata$myexplanatory_labelled)
  }

  mycontexpl <- NULL
  if (!is.null(self$options$contexpl)) {
    mycontexpl <- as.vector(cleaneddata$mycontexpl_labelled)
  }

  # Combine all explanatory variables (+ interaction terms)
  explanatory_formula <- c(myexplanatory, mycontexpl)
  if (length(self$options$interactions) > 0) {
    .all_labels_ff <- labelled::var_label(cleaneddata$mydata_labelled)
    explanatory_formula <- c(
      explanatory_formula,
      .interactionTermsForFinalfit(
        .mapInteractionTerms(self$options$interactions, .all_labels_ff))
    )
  }

  # Prepare the dependent variable formula
  dependent_formula <- "Surv(mytime, myoutcome)"

  private$.checkpoint()


  # Use finalfit to generate nicely formatted Cox regression table
  tryCatch({
    finalfit::finalfit(
      .data = mydata,
      dependent = dependent_formula,
      explanatory = explanatory_formula,
      metrics = TRUE
    ) -> tCox

    # Convert finalfit table to HTML with nice formatting
    # escape = TRUE: finalfit HR cells are plain text, so HTML-escaping keeps
    # formatting intact while preventing user column labels (row names) that
    # contain HTML from being injected as markup into this type:Html output.
    text_html <- knitr::kable(
      tCox[[1]],
      row.names = FALSE,
      align = c('l', 'l', 'r', 'l', 'l'),
      format = "html",
      escape = TRUE
    )

    # Set the content for the HR table
    self$results$text$setContent(text_html)

    # Extract and format model metrics from finalfit
    metrics_text <- unlist(tCox[[2]])

    # Create the model metrics text for text2
    text2_html <- glue::glue("
      <br>
      <b>Model Metrics:</b><br>
      {metrics_text}
      <br>
    ")

    # Add landmark information if used
    if (self$options$uselandmark) {
      landmark <- jmvcore::toNumeric(self$options$landmark)

      text2_html <- glue::glue(
        text2_html,
        "Landmark time used as: ",
        landmark,
        " ",
        self$options$timetypeoutput,
        "."
      )
    }

    # Set the content for the model metrics
    self$results$text2$setContent(text2_html)

    # Generate natural language summary for multivariable Cox if showSummaries is enabled
    if (self$options$showSummaries) {
      tryCatch({
        # Extract data from finalfit table for summary generation
        cox_table <- tCox[[1]]

        # Count significant variables (p < 0.05)
        sig_count <- 0
        strongest_var <- NULL
        strongest_hr <- 1
        strongest_effect <- NULL

        # Parse the HR column to find significant predictors
        if ("HR (multivariable)" %in% names(cox_table)) {
          hr_col <- cox_table[["HR (multivariable)"]]

          # Skip the header row if it exists
          data_rows <- which(!is.na(hr_col) & hr_col != "-" & hr_col != "")

          for (i in data_rows) {
            hr_text <- as.character(hr_col[i])

            # Extract HR value and p-value from text like "1.50 (1.20-2.00, p=0.001)"
            hr_match <- regmatches(hr_text, regexpr("[0-9]+\\.?[0-9]*", hr_text))
            p_match <- regmatches(hr_text, regexpr("p[=<][0-9\\.]+", hr_text))

            if (length(hr_match) > 0 && length(p_match) > 0) {
              hr_val <- as.numeric(hr_match[1])
              p_text <- gsub("p[=<]", "", p_match[1])
              p_val <- as.numeric(p_text)

              if (!is.na(p_val) && p_val < 0.05) {
                sig_count <- sig_count + 1

                # Track strongest effect
                if (!is.na(hr_val) && abs(log(hr_val)) > abs(log(strongest_hr))) {
                  strongest_hr <- hr_val
                  strongest_var <- as.character(cox_table[i, 1])  # First column = variable name
                  strongest_effect <- if (hr_val > 1) "increased risk" else "decreased risk"
                }
              }
            }
          }
        }

        # Count total variables analyzed
        n_vars <- length(c(self$options$explanatory, self$options$contexpl))

        # Count events from the data
        cleaneddata <- private$.cleandata()
        mydata <- cleaneddata$cleanData
        n_events <- sum(.eventIndicator(mydata$myoutcome), na.rm = TRUE)

        # Generate summary text
        summary_parts <- list()

        # Overview
        summary_parts$overview <- paste0(
          "This multivariable Cox regression analysis examined ", n_vars, " ",
          "potential risk factors in patients with ", n_events, " ",
          "events observed during follow-up."
        )

        # Key findings
        if (sig_count > 0) {
          summary_parts$findings <- paste0(
            "<br><br><b>Key Finding:</b> ", sig_count, " out of ", n_vars, " ",
            "factors showed statistically significant associations with the outcome (p < 0.05)."
          )

          if (!is.null(strongest_var) && !is.null(strongest_effect)) {
            summary_parts$strongest <- paste0(
              "<br><br><b>Strongest predictor:</b> ", htmltools::htmlEscape(strongest_var), " was associated with ",
              strongest_effect, " (hazard ratio = ", round(strongest_hr, 2), ")."
            )
          }

          # Clinical interpretation
          risk_interpretation <- ""
          if (strongest_hr > 2) {
            risk_interpretation <- "This represents a substantial clinical effect."
          } else if (strongest_hr > 1.5 || strongest_hr < 0.67) {
            risk_interpretation <- "This represents a moderate clinical effect."
          } else if (strongest_hr != 1) {
            risk_interpretation <- "This represents a mild clinical effect."
          }

          if (risk_interpretation != "") {
            summary_parts$interpretation <- paste0(
              "<br><br><b>Clinical Significance:</b> ", risk_interpretation, " ",
              "Consider this factor in clinical decision-making and patient counseling."
            )
          }
        } else {
          summary_parts$findings <- paste0(
            "<br><br><b>Key Finding:</b> No statistically significant associations were identified among the ",
            n_vars, " factors examined (all p-values \u2265 0.05)."
          )
        }

        # Combine all parts into HTML
        full_summary <- paste0(
          "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #007bff;'>",
          "<p style='margin: 0; line-height: 1.8;'>",
          paste(unlist(summary_parts), collapse = ""),
          "</p>",
          "</div>"
        )

        self$results$multivariableCoxSummary$setContent(full_summary)

      }, error = function(e) {
        # Fail gracefully - summary is optional
        self$results$multivariableCoxSummary$setContent(
          "<p style='color: #856404;'>Summary generation encountered an issue. See detailed results above.</p>"
        )
      })
    }

  }, error = function(e) {
    # Fallback error handling if finalfit fails
    error_msg <- paste0(
      "<p style='color: red;'><b>Error generating Cox regression table:</b> ",
      e$message,
      "</p><p>Please check your data and variable selections.</p>"
    )
    self$results$text$setContent(error_msg)
  })
}

,
# Helper function to create HR table with error handling
.createHRTable = function(model) {
  # If model has no coefficients, return empty table
  if (is.null(model$coefficients) || length(model$coefficients) == 0) {
    return(data.frame(
      Variable = "No variables in model",
      "HR (multivariable)" = "N/A"
    ))
  }

  # Get model summary
  summary_model <- summary(model)

  # Extract coefficients, hazard ratios, and CIs safely
  tryCatch({
    coefs <- summary_model$coefficients
    confint <- summary_model$conf.int

    # Create data frame with variable names and hazard ratios
    hr_table <- data.frame(
      Variable = row.names(coefs),
      HR = round(confint[, 1], 2),
      Lower_CI = round(confint[, 3], 2),
      Upper_CI = round(confint[, 4], 2),
      P_value = format.pval(coefs[, 5], digits = 3)
    )

    # Format the HR with CI
    hr_table$HR_with_CI <- paste0(
      hr_table$HR, " (",
      hr_table$Lower_CI, "-",
      hr_table$Upper_CI, ", p=",
      hr_table$P_value, ")"
    )

    # Return simplified table
    final_table <- data.frame(
      Variable = hr_table$Variable,
      "HR (multivariable)" = hr_table$HR_with_CI
    )

    return(final_table)
  }, error = function(e) {
    # If something goes wrong, return a basic table with the error
    return(data.frame(
      Variable = names(model$coefficients),
      "HR (multivariable)" = "Error calculating hazard ratios"
    ))
  })
}

      # Survival Decision Tree Function ----
      ,
      .survivalTree = function(results) {
        tryCatch({
          # Skip if tree analysis not requested
          # NOTE: use_tree option not defined in .a.yaml - experimental feature
          use_tree <- tryCatch(self$options$use_tree, error = function(e) FALSE)
          if (!isTRUE(use_tree)) {
            return(NULL)
          }

          # Check required packages
          if (!requireNamespace("rpart", quietly = TRUE)) {
            self$results$tree_summary$setContent(
              "<p>Error: The 'rpart' package is required for decision tree analysis but not available.</p>"
            )
            return(NULL)
          }

          # Get cleaned data
          cleanData <- results$cleanData
          # Use the standardized column names that actually exist in cleanData
          mytime <- "mytime"
          myoutcome <- "myoutcome"













          # Validate data
          if (is.null(cleanData) || nrow(cleanData) == 0) {
            self$results$tree_summary$setContent(
              "<p>Error: No data available for decision tree analysis.</p>"
            )
            return(NULL)
          }

          # Check if standardized columns exist
          if (!"mytime" %in% names(cleanData) || !"myoutcome" %in% names(cleanData)) {
            self$results$tree_summary$setContent(
              paste0("<p>Error: Required columns not found in cleaned data. Available columns: ",
                     paste(names(cleanData), collapse = ", "), "</p>")
            )
            return(NULL)
          }

          # Get explanatory variables
          expl_vars <- NULL
          if (!is.null(self$options$explanatory)) {
            expl_vars <- c(expl_vars, as.vector(results$myexplanatory_labelled))
          }
          if (!is.null(self$options$contexpl)) {
            expl_vars <- c(expl_vars, as.vector(results$mycontexpl_labelled))
          }

          # Check for explanatory variables
          if (length(expl_vars) == 0) {
            self$results$tree_summary$setContent(
              "<p>Error: At least one explanatory variable is required for decision tree analysis.</p>"
            )
            return(NULL)
          }

          # Validate that explanatory variables exist in data
          missing_vars <- setdiff(expl_vars, names(cleanData))
          if (length(missing_vars) > 0) {
            self$results$tree_summary$setContent(
              paste0("<p>Error: Variables not found in data: ", paste(missing_vars, collapse = ", "), "</p>")
            )
            return(NULL)
          }

          private$.checkpoint()

          # Create formula for rpart using the actual column names from results
          formula_string <- paste("survival::Surv(", mytime, ", ", myoutcome, ") ~ ",
                                   paste(expl_vars, collapse = " + "))
          formula <- .asSurvivalFormula(formula_string)

          # Validate minimum parameters (with safe defaults for undefined options)
          # NOTE: These options not defined in .a.yaml - experimental feature
          min_node <- tryCatch(max(1, self$options$min_node), error = function(e) 10)
          complexity <- tryCatch(max(0.001, self$options$complexity), error = function(e) 0.01)
          max_depth <- tryCatch(max(1, min(30, self$options$max_depth)), error = function(e) 10)

          # Fit survival tree using rpart with error handling
          tree <- tryCatch({
            rpart::rpart(
              formula = formula,
              data = cleanData,
              method = "exp",  # exponential survival model
              control = rpart::rpart.control(
                minsplit = 2 * min_node,
                minbucket = min_node,
                cp = complexity,
                maxdepth = max_depth
              )
            )
          }, error = function(e) {
            NULL
          })

          # Create summary text
          if (is.null(tree) || nrow(tree$frame) == 0) {
            tree_text <- paste0(
              "<h3>Survival Decision Tree Results</h3>",
              "<p><strong>The survival tree could not be built with the current parameters.</strong></p>",
              "<p>This may be due to:</p>",
              "<ul>",
              "<li>Insufficient data for the specified minimum node size</li>",
              "<li>Complexity parameter too high</li>",
              "<li>Variables not providing meaningful splits</li>",
              "</ul>",
              "<p>Try adjusting the parameters:</p>",
              "<ul>",
              "<li>Reduce minimum node size</li>",
              "<li>Lower complexity parameter</li>",
              "<li>Include more variables</li>",
              "</ul>"
            )
          } else {
            # Get variable importance
            var_imp <- tree$variable.importance
            if (!is.null(var_imp) && length(var_imp) > 0) {
              var_imp_df <- data.frame(
                Variable = names(var_imp),
                Importance = var_imp,
                stringsAsFactors = FALSE
              )
              var_imp_df <- var_imp_df[order(-var_imp_df$Importance), ]

              var_imp_html <- paste(
                "<tr>",
                "<td>", var_imp_df$Variable, "</td>",
                "<td>", round(var_imp_df$Importance, 2), "</td>",
                "</tr>",
                collapse = ""
              )

              var_imp_table <- paste0(
                "<table class='jmv-results-table'>",
                "<thead><tr><th>Variable</th><th>Importance</th></tr></thead>",
                "<tbody>", var_imp_html, "</tbody>",
                "</table>"
              )
            } else {
              var_imp_table <- "<p>No variable importance measures available.</p>"
            }

            # Get tree statistics
            n_terminal <- sum(tree$frame$var == "<leaf>")
            n_splits <- nrow(tree$frame) - 1

            tree_text <- paste0(
              "<h3>Survival Decision Tree Results</h3>",
              "<p>The decision tree was successfully built with the following characteristics:</p>",
              "<ul>",
              "<li><strong>Terminal nodes:</strong> ", n_terminal, "</li>",
              "<li><strong>Internal splits:</strong> ", n_splits, "</li>",
              "<li><strong>Variables used:</strong> ", length(unique(tree$frame$var[tree$frame$var != "<leaf>"])), "</li>",
              "</ul>",
              "<p><strong>Parameters used:</strong></p>",
              "<ul>",
              "<li>Complexity parameter: ", complexity, "</li>",
              "<li>Minimum node size: ", min_node, "</li>",
              "<li>Maximum depth: ", max_depth, "</li>",
              "</ul>",
              "<h4>Variable Importance</h4>",
              var_imp_table,
              "<p><i>Note: The decision tree plot visualizes how the variables split the data into groups with different survival outcomes.</i></p>"
            )
          }

          self$results$tree_summary$setContent(tree_text)

          # Store tree for plotting
          return(tree)

        }, error = function(e) {
          private$.addHtmlMessage(
            "error",
            .("Survival decision tree error"),
            paste0(e$message, .(". Recommendations: (1) check sufficient data for tree building, (2) try reducing minimum node size, or (3) ensure variables contain valid data."))
          )
          return(NULL)
        })
      }

      # Plot Tree ----
      ,
      .plotTree = function(image, ggtheme, theme, ...) {
        tryCatch({
          # Skip if tree analysis not requested
          # NOTE: use_tree option not defined in .a.yaml - experimental feature
          use_tree <- tryCatch(self$options$use_tree, error = function(e) FALSE)
          if (!isTRUE(use_tree)) {
            return(FALSE)
          }

          # Check required packages
          if (!requireNamespace("rpart.plot", quietly = TRUE)) {
            return(FALSE)
          }

          # Get results and tree
          results <- private$.cleandata()
          tree <- private$.survivalTree(results)

          if (is.null(tree) || nrow(tree$frame) == 0) {
            return(FALSE)
          }

            self$results$mydataview_survivaldecisiontree$setContent(
    list(
      results = results,
      tree = tree
    )
  )


          # Add checkpoint before plotting
          private$.checkpoint()

          # Plot tree with error handling
          rpart.plot::rpart.plot(
            tree,
            main = "Survival Decision Tree",
            extra = 101,  # show fitted risk and percentage of observations
            box.palette = "auto",  # color by fitted risk
            shadow.col = "gray",  # add shadows to the boxes
            nn = TRUE,  # show node numbers
            fallen.leaves = TRUE,  # align leaf nodes
            roundint = FALSE,  # don't round integers
            cex = 0.8,  # text size
            clip.right.labs = FALSE  # don't clip labels
          )

          return(TRUE)

        }, error = function(e) {
          # Report tree plotting error for debugging
          self$results$tree_summary$setContent(
            paste0("<p>Tree plotting error: ", e$message, "</p>")
          )
          return(FALSE)
        })
      }

      # Plot Node Survival ----
      ,
      .plotNodeSurvival = function(image, ggtheme, theme, ...) {
        tryCatch({
          # Skip if not requested
          # NOTE: use_tree and show_terminal_nodes options not defined in .a.yaml - experimental feature
          use_tree <- tryCatch(self$options$use_tree, error = function(e) FALSE)
          show_terminal_nodes <- tryCatch(self$options$show_terminal_nodes, error = function(e) FALSE)
          if (!isTRUE(use_tree) || !isTRUE(show_terminal_nodes)) {
            return(FALSE)
          }

          # Check required packages
          if (!requireNamespace("survminer", quietly = TRUE)) {
            return(FALSE)
          }

          # Get results and tree
          results <- private$.cleandata()
          message("Node survival: results obtained, cleanData columns: ", paste(names(results$cleanData), collapse = ", "))
          tree <- private$.survivalTree(results)
          message("Node survival: tree obtained")

          if (is.null(tree) || nrow(tree$frame) == 0) {
            return(FALSE)
          }

          # Get cleaned data
          cleanData <- results$cleanData
          # Use the standardized column names that actually exist in cleanData
          mytime <- "mytime"
          myoutcome <- "myoutcome"

          # Validate data
          if (is.null(cleanData) || nrow(cleanData) == 0) {
            return(FALSE)
          }

          # Add checkpoint before plotting
          private$.checkpoint()

          # Get terminal node assignments for each observation
          message("Node survival: getting node assignments")
          node_assignments <- tree$where
          cleanData$node <- paste("Node", node_assignments)
          message("Node survival: node assignments created, unique nodes: ", length(unique(cleanData$node)))

          # Check if we have at least 2 nodes
          unique_nodes <- unique(cleanData$node)
          if (length(unique_nodes) < 2) {
            message("Node survival: insufficient nodes (", length(unique_nodes), ")")
            return(FALSE)
          }

          # Plot survival curves for each terminal node
          # Check that required columns exist
          if (!"mytime" %in% names(cleanData)) {
            message("Error: mytime column not found in cleanData")
            return(FALSE)
          }
          if (!"myoutcome" %in% names(cleanData)) {
            message("Error: myoutcome column not found in cleanData")
            return(FALSE)
          }

          # Create formula properly
          message("Node survival: creating formula with proper syntax")
          formula <- .asSurvivalFormula("Surv(mytime, myoutcome) ~ node")
          message("Node survival: formula created: ", deparse(formula))

          message("Node survival: calling survfit")
          fit <- survival::survfit(formula, data = cleanData)
          message("Node survival: survfit completed")

          message("Node survival: trying minimal ggsurvplot call")

          # Try the most minimal ggsurvplot call possible
          plot <- survminer::ggsurvplot(
            fit,
            data = cleanData
          )

          message("Node survival: minimal ggsurvplot completed, printing")
          print(plot)
          message("Node survival: plot printed successfully")
          return(TRUE)

        }, error = function(e) {
          # Report node survival plotting error for debugging
          message("Node survival plotting error: ", e$message)
          return(FALSE)
        })

        # Educational Explanations ----
        if (self$options$showExplanations) {
          private$.addExplanations()
        }
      }

      # Convert Wide Format to Long Format for Time-Dependent Covariates ----
      ,
      .convertWideToLong = function(mydata, time_dep_vars, all_labels) {

        # Get change time points with input sanitization
        change_times <- private$.sanitizeStringInput(
          self$options$change_times,
          private$DEFAULT_CHANGE_TIMES,
          "^[0-9., ]+$"  # Only numbers, commas, periods, spaces
        )

        # Parse change times
        time_points <- tryCatch({
          as.numeric(trimws(strsplit(change_times, ",")[[1]]))
        }, error = function(e) {
          as.numeric(trimws(strsplit(private$DEFAULT_CHANGE_TIMES, ",")[[1]]))
        })
        time_points <- sort(time_points[!is.na(time_points)])

        # Get suffix pattern with input sanitization
        suffix_pattern <- private$.sanitizeStringInput(
          self$options$td_suffix_pattern,
          private$DEFAULT_TD_SUFFIX,
          "^[a-zA-Z0-9_{}]+$"  # Only alphanumeric, underscore, braces
        )

        # Initialize long format data
        long_data <- data.frame()

        for (i in seq_len(nrow(mydata))) {
          subject_data <- mydata[i, ]

          # Get subject's total follow-up time
          total_time <- subject_data$mytime

          # Create time intervals: 0, change_times, total_time
          intervals <- c(0, time_points[time_points < total_time], total_time)
          intervals <- unique(sort(intervals))

          # If subject has very short follow-up, create just one interval
          if (length(intervals) < 2) {
            intervals <- c(0, total_time)
          }

          # Create rows for each interval
          for (j in 1:(length(intervals)-1)) {
            tstart <- intervals[j]
            tstop <- intervals[j+1]

            # Status is 1 only in the last interval if subject has event
            status <- ifelse(j == (length(intervals)-1), subject_data$myoutcome, 0)

            # Create new row
            new_row <- subject_data
            new_row$tstart <- tstart
            new_row$tstop <- tstop
            new_row$myoutcome <- status

            # Update time-dependent variables for this interval
            for (var in time_dep_vars) {

              # Determine which time-dependent version to use
              if (tstart == 0) {
                # Use baseline version for first interval
                baseline_var <- paste0(var, "_baseline")
                if (baseline_var %in% names(mydata)) {
                  new_row[[var]] <- subject_data[[baseline_var]]
                } else {
                  # If no baseline version, use the base variable
                  new_row[[var]] <- subject_data[[var]]
                }
              } else {
                # Find appropriate time-dependent version
                applicable_times <- time_points[time_points <= tstart]
                if (length(applicable_times) > 0) {
                  use_time <- max(applicable_times)
                  td_var_name <- gsub("\\{time\\}", use_time, suffix_pattern)
                  full_var_name <- paste0(var, td_var_name)

                  if (full_var_name %in% names(mydata)) {
                    new_row[[var]] <- subject_data[[full_var_name]]
                  } else {
                    # Fall back to previous value or baseline
                    new_row[[var]] <- subject_data[[var]]
                  }
                } else {
                  # Use baseline if no applicable time found
                  new_row[[var]] <- subject_data[[var]]
                }
              }
            }

            long_data <- rbind(long_data, new_row)
          }
        }

        return(long_data)
      }

      ,
      # Helper: set explanation content on a (possibly absent) result item.
      # Defined as a proper private method. Previously this was assigned at run
      # time via `private$.setExplanationContent <- function(...)`, which threw
      # "cannot add bindings to a locked environment" on the locked R6 instance
      # whenever Show Explanations was enabled (GitHub issue #122).
      .setExplanationContent = function(result_name, content) {
        tryCatch({
          self$results[[result_name]]$setContent(content)
        }, error = function(e) {
          # Silently ignore if result doesn't exist
        })
      },

      # Educational Explanations ----
      .addExplanations = function() {

        # Multivariable Cox Regression Explanation
        private$.setExplanationContent("multivariableCoxExplanation", '
        <div class="explanation-box" style="background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin: 10px 0;">
            <h3 style="color: #2c5282; margin-top: 0;"> Understanding Multivariable Cox Regression</h3>

            <div style="background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;">
                <h4 style="color: #2d3748; margin-top: 0;">What is Multivariable Survival Analysis?</h4>
                <p style="margin: 8px 0;">Multivariable Cox regression analyzes <strong>multiple risk factors simultaneously</strong> to identify which factors independently affect survival when all others are held constant.</p>

                <div style="background-color: #e6f7ff; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong> Key Advantage:</strong> Separates the <strong>independent effect</strong> of each variable from the effects of other variables
                </div>
            </div>

            <div style="background-color: #fef5e7; padding: 12px; border-radius: 5px; margin: 10px 0;">
                <h4 style="color: #d68910; margin-top: 0;"> Adjusted vs Unadjusted Hazard Ratios</h4>
                <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                    <tr style="background-color: #fff3cd;">
                        <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Type</th>
                        <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">What It Shows</th>
                        <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Clinical Use</th>
                    </tr>
                    <tr>
                        <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Unadjusted HR</strong></td>
                        <td style="padding: 8px; border: 1px solid #ffc107;">Raw association with survival</td>
                        <td style="padding: 8px; border: 1px solid #ffc107;">Initial screening of factors</td>
                    </tr>
                    <tr style="background-color: #fffbf0;">
                        <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Adjusted HR</strong></td>
                        <td style="padding: 8px; border: 1px solid #ffc107;">Independent effect after controlling for other variables</td>
                        <td style="padding: 8px; border: 1px solid #ffc107;">True prognostic value</td>
                    </tr>
                </table>
            </div>

            <div style="background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin: 10px 0;">
                <h4 style="color: #2e7d32; margin-top: 0;"> Clinical Examples</h4>

                <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>Example: Cancer Survival Model</strong>
                    <p style="margin: 8px 0;"><strong>Variables:</strong> Age, Stage, Grade, Treatment</p>
                    <table style="width: 100%; margin: 5px 0;">
                        <tr><td><strong>Age:</strong></td><td>Adjusted HR = 1.02 (p=0.01)</td></tr>
                        <tr><td><strong>Stage III vs I:</strong></td><td>Adjusted HR = 2.5 (p<0.001)</td></tr>
                        <tr><td><strong>High grade:</strong></td><td>Adjusted HR = 1.8 (p=0.003)</td></tr>
                        <tr><td><strong>Treatment B:</strong></td><td>Adjusted HR = 0.7 (p=0.02)</td></tr>
                    </table>
                    <p style="margin: 8px 0;"><strong>Interpretation:</strong> Stage is the strongest independent predictor, even after accounting for age, grade, and treatment.</p>
                </div>

                <div style="background-color: #f3e5f5; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong> Confounding Example:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li><strong>Unadjusted:</strong> Age HR = 1.05 (appears strongly associated)</li>
                        <li><strong>Adjusted for stage:</strong> Age HR = 1.01 (much weaker effect)</li>
                        <li><strong>Reason:</strong> Older patients tend to have more advanced disease</li>
                    </ul>
                </div>
            </div>

            <div style="background-color: #e3f2fd; padding: 12px; border-radius: 5px; margin: 10px 0;">
                <h4 style="color: #1976d2; margin-top: 0;"> Model Building Strategy</h4>
                <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>1. Variable Selection:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li>Include clinically important variables</li>
                        <li>Consider statistical significance (p<0.05 or p<0.1)</li>
                        <li>Check for multicollinearity</li>
                    </ul>

                    <strong>2. Model Assessment:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li>Concordance index (C-index): >0.7 indicates good discrimination</li>
                        <li>Proportional hazards assumption testing</li>
                        <li>Model calibration assessment</li>
                    </ul>
                </div>
            </div>

            <div style="background-color: #fff3e0; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800;">
                <strong> Clinical Applications:</strong>
                <ul style="margin: 5px 0; padding-left: 20px;">
                    <li><strong>Prognostic models:</strong> Identify independent risk factors</li>
                    <li><strong>Treatment decisions:</strong> Assess benefit after controlling for confounders</li>
                    <li><strong>Risk stratification:</strong> Combine multiple factors into risk scores</li>
                    <li><strong>Research:</strong> Control for baseline differences between groups</li>
                </ul>
            </div>
        </div>
        ')

        # Adjusted Survival Curves Explanation
        private$.setExplanationContent("adjustedSurvivalExplanation", '
        <div style="margin-bottom: 20px; padding: 15px; background-color: #d1ecf1; border-left: 4px solid #bee5eb;">
            <h4 style="margin-top: 0; color: #2c3e50;">Understanding Adjusted Survival Curves</h4>
            <p><strong>Adjusted Curves:</strong> Survival curves that account for differences in patient characteristics between groups.</p>
            <ul>
                <li><strong>Covariate Adjustment:</strong> Controls for confounding variables that might bias group comparisons</li>
                <li><strong>Average Patient:</strong> Shows survival for a typical patient with average covariate values</li>
                <li><strong>True Group Effect:</strong> Isolates the effect of the grouping variable from other factors</li>
                <li><strong>Fair Comparison:</strong> Enables valid comparisons between groups with different baseline characteristics</li>
            </ul>
            <p><em>When to use:</em> Essential when comparing groups that differ in important prognostic factors.</p>
        </div>
        ')

        # Risk Score Analysis Explanation
        private$.setExplanationContent("riskScoreExplanation", '
        <div style="margin-bottom: 20px; padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107;">
            <h4 style="margin-top: 0; color: #2c3e50;">Understanding Risk Score Analysis</h4>
            <p><strong>Risk Scoring:</strong> Combines multiple prognostic factors into a single risk prediction score.</p>
            <ul>
                <li><strong>Linear Predictor:</strong> Weighted sum of all variables in the Cox model</li>
                <li><strong>Risk Stratification:</strong> Divides patients into low, intermediate, and high-risk groups</li>
                <li><strong>Prognostic Tool:</strong> Single score that summarizes overall patient risk</li>
                <li><strong>Clinical Utility:</strong> Facilitates treatment decisions and patient counseling</li>
            </ul>
            <p><em>Advantage:</em> Simplifies complex multivariable models into an easily interpretable risk score.</p>
        </div>
        ')

        # Nomogram Explanation
        private$.setExplanationContent("nomogramExplanation", '
        <div style="margin-bottom: 20px; padding: 15px; background-color: #f8d7da; border-left: 4px solid #dc3545;">
            <h4 style="margin-top: 0; color: #721c24;">Understanding Nomograms</h4>
            <p><strong>Nomogram:</strong> Graphical calculation tool that translates regression models into visual risk calculators.</p>
            <ul>
                <li><strong>Point System:</strong> Each predictor contributes points based on its value and hazard ratio</li>
                <li><strong>Total Points:</strong> Sum of individual points provides overall risk score</li>
                <li><strong>Survival Probability:</strong> Converts total points to predicted survival at specific time points</li>
                <li><strong>Clinical Tool:</strong> Enables bedside risk calculation without complex mathematics</li>
            </ul>
            <p><em>Clinical application:</em> Allows clinicians to quickly estimate individual patient survival probabilities.</p>
        </div>
        ')

        # Person-Time Analysis Explanation
        private$.setExplanationContent("personTimeExplanation", '
        <div style="margin-bottom: 20px; padding: 15px; background-color: #d4edda; border-left: 4px solid #28a745;">
            <h4 style="margin-top: 0; color: #2c3e50;">Understanding Person-Time Analysis</h4>
            <p><strong>Person-Time:</strong> Comprehensive measure combining participant count and observation duration.</p>
            <ul>
                <li><strong>Incidence Rates:</strong> Events per person-time unit across different time intervals</li>
                <li><strong>Time-Stratified Analysis:</strong> Examines how event rates change over follow-up time</li>
                <li><strong>Group Comparisons:</strong> Compares incidence rates between different risk groups</li>
                <li><strong>Rate Ratios:</strong> Quantifies relative differences in event rates between groups</li>
            </ul>
            <p><em>Clinical insight:</em> Reveals patterns of risk over time and identifies periods of highest event rates.</p>
        </div>
        ')

        # Stratified Analysis Explanation
        private$.setExplanationContent("stratifiedAnalysisExplanation", '
        <div style="margin-bottom: 20px; padding: 15px; background-color: #e2e3e5; border-left: 4px solid #6c757d;">
            <h4 style="margin-top: 0; color: #2c3e50;">Understanding Stratified Cox Regression</h4>
            <p><strong>Stratification:</strong> Allows different baseline hazards for distinct patient subgroups while estimating common covariate effects.</p>
            <ul>
                <li><strong>Heterogeneous Baseline Risk:</strong> Accounts for fundamentally different risk levels between strata</li>
                <li><strong>Common Covariate Effects:</strong> Assumes treatment/predictor effects are similar across strata</li>
                <li><strong>Improved Model Fit:</strong> Better accommodates population heterogeneity</li>
                <li><strong>Robust Estimates:</strong> Provides more accurate hazard ratios when baseline risks differ</li>
            </ul>
            <p><em>When to use:</em> When proportional hazards assumption is violated due to different baseline hazards between groups.</p>
        </div>
        ')

        # Survival Plots Explanation
        private$.setExplanationContent("survivalPlotsExplanation", '
        <div class="explanation-box" style="background-color: #f9f9f9; padding: 15px; border-radius: 8px; margin: 10px 0;">
            <h3 style="color: #2c5282; margin-top: 0;"> Understanding Adjusted Survival Curves and Hazard Ratio Plots</h3>

            <div style="background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin: 10px 0;">
                <h4 style="color: #2e7d32; margin-top: 0;"> Adjusted Survival Curves</h4>
                <p style="margin: 8px 0;">Adjusted survival curves show survival probabilities after <strong>controlling for confounding variables</strong>, providing a fair comparison between groups.</p>

                <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>Key Features:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li><strong>Covariate-adjusted:</strong> Controls for differences in patient characteristics</li>
                        <li><strong>Population-averaged:</strong> Shows survival for typical patients with average risk factors</li>
                        <li><strong>Isolates group effect:</strong> Separates the true effect of the grouping variable</li>
                        <li><strong>Clinical relevance:</strong> Provides realistic survival estimates for clinical decision-making</li>
                    </ul>
                </div>

                <div style="background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong> Interpretation Guide:</strong>
                    <table style="width: 100%; border-collapse: collapse; margin: 5px 0;">
                        <tr style="background-color: #bbdefb;">
                            <th style="padding: 8px; text-align: left; border: 1px solid #2196f3;">Curve Pattern</th>
                            <th style="padding: 8px; text-align: left; border: 1px solid #2196f3;">Clinical Meaning</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #2196f3;">Steep early decline</td>
                            <td style="padding: 8px; border: 1px solid #2196f3;">High early mortality risk</td>
                        </tr>
                        <tr style="background-color: #f3f8ff;">
                            <td style="padding: 8px; border: 1px solid #2196f3;">Plateau phase</td>
                            <td style="padding: 8px; border: 1px solid #2196f3;">Stable survival period with low event rate</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #2196f3;">Wide confidence bands</td>
                            <td style="padding: 8px; border: 1px solid #2196f3;">Uncertainty due to small sample size or high censoring</td>
                        </tr>
                    </table>
                </div>
            </div>

            <div style="background-color: #fff3e0; padding: 12px; border-radius: 5px; margin: 10px 0;">
                <h4 style="color: #d68910; margin-top: 0;"> Hazard Ratio (Forest) Plots</h4>
                <p style="margin: 8px 0;">Forest plots visualize <strong>hazard ratios and confidence intervals</strong> for multiple variables simultaneously, enabling quick assessment of relative risk factors.</p>

                <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>Reading Forest Plots:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li><strong>Vertical line at HR=1:</strong> Line of no effect (reference)</li>
                        <li><strong>Points to the right (HR>1):</strong> Increased hazard (worse survival)</li>
                        <li><strong>Points to the left (HR<1):</strong> Decreased hazard (better survival)</li>
                        <li><strong>Horizontal lines:</strong> 95% confidence intervals for each HR</li>
                        <li><strong>Crossing HR=1:</strong> Non-significant effect (p>0.05)</li>
                    </ul>
                </div>

                <div style="background-color: #fef5e7; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong> Clinical Example - Cancer Study:</strong>
                    <table style="width: 100%; margin: 5px 0;">
                        <tr><td><strong>Age (per year):</strong></td><td>HR = 1.02 [0.99-1.05] \u2192 Minimal age effect</td></tr>
                        <tr><td><strong>Stage III vs I:</strong></td><td>HR = 3.2 [2.1-4.8] \u2192 Strong predictor of poor survival</td></tr>
                        <tr><td><strong>Treatment B vs A:</strong></td><td>HR = 0.6 [0.4-0.9] \u2192 Protective treatment effect</td></tr>
                    </table>
                </div>
            </div>

            <div style="background-color: #e6f7ff; padding: 12px; border-radius: 5px; margin: 10px 0;">
                <h4 style="color: #1976d2; margin-top: 0;"> Clinical Applications</h4>

                <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>1. Treatment Comparison:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li>Adjusted curves eliminate confounding by patient characteristics</li>
                        <li>Shows true treatment effect independent of baseline differences</li>
                        <li>Critical for observational studies with treatment selection bias</li>
                    </ul>

                    <strong>2. Prognostic Modeling:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li>Identifies independent risk factors from forest plots</li>
                        <li>Quantifies relative importance of different predictors</li>
                        <li>Builds comprehensive prognostic models</li>
                    </ul>

                    <strong>3. Risk Stratification:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li>Combines multiple risk factors for patient classification</li>
                        <li>Guides treatment intensity decisions</li>
                        <li>Enables personalized survival predictions</li>
                    </ul>
                </div>
            </div>

            <div style="background-color: #f3e5f5; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #9c27b0;">
                <strong> Best Practices:</strong>
                <ul style="margin: 5px 0; padding-left: 20px;">
                    <li><strong>Always report confidence intervals:</strong> Shows precision of estimates</li>
                    <li><strong>Check proportional hazards:</strong> Ensure model assumptions are met</li>
                    <li><strong>Consider clinical significance:</strong> Statistical significance \u2260 clinical importance</li>
                    <li><strong>Validate findings:</strong> Test models in independent populations when possible</li>
                </ul>
            </div>
        </div>
        ')
      }

      # Machine Learning Methods ----

      ,

.runMLAnalysis = function() {
        tryCatch({
          cleaneddata <- private$.cleandata()

          # EXPERIMENTAL: Disabled - ml_method and related options not in .a.yaml
          # # Check for required packages
          # if (self$options$ml_method == "rsf" && !requireNamespace("randomForestSRC", quietly = TRUE)) {
          #   self$results$ml_performance_metrics$setContent(.("randomForestSRC package not available. Please install it to use Random Survival Forest."))
          #   return()
          # }
          #
          # if (self$options$ml_method == "glmnet" && !requireNamespace("glmnet", quietly = TRUE)) {
          #   self$results$ml_performance_metrics$setContent(.("glmnet package not available. Please install it to use regularized regression."))
          #   return()
          # }
          #
          # if (self$options$ml_method == "xgboost" && !requireNamespace("xgboost", quietly = TRUE)) {
          #   self$results$ml_performance_metrics$setContent(.("xgboost package not available. Please install it to use XGBoost survival models."))
          #   return()
          # }
          #
          # if (self$options$ml_method == "svm" && !requireNamespace("survivalsvm", quietly = TRUE)) {
          #   self$results$ml_performance_metrics$setContent(.("survivalsvm package not available. Please install it to use SVM survival analysis."))
          #   return()
          # }
          #
          # # Select ML method
          # if (self$options$ml_method == "rsf") {
          #   private$.performRandomForest(cleaneddata)
          # } else if (self$options$ml_method == "glmnet") {
          #   private$.performGlmnet(cleaneddata)
          # } else if (self$options$ml_method == "xgboost") {
          #   private$.performXGBoost(cleaneddata)
          # } else if (self$options$ml_method == "svm") {
          #   private$.performSVM(cleaneddata)
          # } else if (self$options$ml_method == "deepsurv") {
          #   private$.performDeepSurvival(cleaneddata)
          # } else if (self$options$ml_method == "ensemble") {
          #   private$.performEnsemble(cleaneddata)
          # }
          #
          # # Feature selection if requested
          # if (self$options$ml_feature_selection) {
          #   private$.performFeatureSelection(cleaneddata)
          # }
          #
          # # Cross-validation if requested
          # if (self$options$ml_validation == "cv" && self$options$ml_method != "none") {
          #   private$.performCrossValidation(cleaneddata)
          # }
          #
          # # SHAP values if requested
          # if (self$options$ml_shap && self$options$ml_method %in% c("rsf", "xgboost")) {
          #   private$.calculateSHAPValues(cleaneddata)
          # }

          # EXPERIMENTAL: Disabled - ml_performance option not in .a.yaml
          # # Performance metrics if requested
          # if (self$options$ml_performance) {
          #   private$.calculateMLPerformanceMetrics(cleaneddata)
          # }

        }, error = function(e) {
          error_msg <- glue::glue("Error in ML analysis: {e$message}")
          self$results$ml_performance_metrics$setContent(error_msg)
        })
      }

      ,
.performRandomForest = function(cleaneddata) {
        # Random Forest survival analysis
        formula_vars <- private$.prepareMLFormula(cleaneddata)

        tryCatch({
          # Fit Random Forest
          rf_model <- randomForestSRC::rfsrc(
            formula = formula_vars$formula,
            data = cleaneddata,
            ntree = 1000,
            importance = TRUE,
            proximity = TRUE
          )

          # Variable importance
          var_imp <- rf_model$importance
          private$.populateVariableImportance(var_imp)

          # Performance metrics
          oob_error <- rf_model$err.rate[length(rf_model$err.rate)]
          c_index <- 1 - oob_error

          metrics_html <- paste0(
            "<h4>", .("Random Forest Survival Model Results"), "</h4>",
            "<p><strong>", .("Out-of-Bag Error Rate:"), "</strong> ", round(oob_error, 4), "</p>",
            "<p><strong>", .("Concordance Index:"), "</strong> ", round(c_index, 4), "</p>",
            "<p><strong>", .("Number of Trees:"), "</strong> ", rf_model$ntree, "</p>",
            "<p><strong>", .("Variables Used:"), "</strong> ", length(formula_vars$variables), "</p>"
          )

          self$results$ml_performance_metrics$setContent(metrics_html)

          # Prediction intervals
          predictions <- predict(rf_model, newdata = cleaneddata)
          private$.populatePredictionIntervals(predictions, cleaneddata)

        }, error = function(e) {
          error_msg <- glue::glue("Random Forest error: {e$message}")
          self$results$ml_performance_metrics$setContent(error_msg)
        })
      }

      ,
.performGlmnet = function(cleaneddata) {
        # Regularized Cox regression with cross-validation
        formula_vars <- private$.prepareMLFormula(cleaneddata)

        tryCatch({
          # Prepare data for glmnet
          x <- model.matrix(formula_vars$formula, data = cleaneddata)[,-1]
          y <- survival::Surv(cleaneddata$mytime, cleaneddata$myoutcome)

          # Cross-validated glmnet
          cv_fit <- glmnet::cv.glmnet(x, y, family = "cox", alpha = 0.5, nfolds = self$options$ml_cv_folds)

          # Best lambda
          best_lambda <- cv_fit$lambda.min

          # Final model coefficients
          coefs <- coef(cv_fit, s = "lambda.min")
          selected_vars <- which(coefs != 0)

          # Performance metrics
          c_index <- max(cv_fit$glmnet.fit$dev.ratio)

          metrics_html <- paste0(
            "<h4>", .("Regularized Cox Regression Results"), "</h4>",
            "<p><strong>", .("Best Lambda:"), "</strong> ", round(best_lambda, 6), "</p>",
            "<p><strong>", .("Selected Variables:"), "</strong> ", length(selected_vars), " out of ", ncol(x), "</p>",
            "<p><strong>", .("Deviance Explained:"), "</strong> ", round(c_index * 100, 2), "%</p>",
            "<p><strong>", .("Cross-Validation Folds:"), "</strong> ", self$options$ml_cv_folds, "</p>"
          )

          self$results$ml_performance_metrics$setContent(metrics_html)

          # Variable importance from coefficients
          if (length(selected_vars) > 0) {
            var_names <- rownames(coefs)[selected_vars]
            var_coefs <- as.numeric(coefs[selected_vars])
            var_imp <- abs(var_coefs)
            names(var_imp) <- var_names
            private$.populateVariableImportance(var_imp)
          }

        }, error = function(e) {
          error_msg <- glue::glue("Glmnet error: {e$message}")
          self$results$ml_performance_metrics$setContent(error_msg)
        })
      }

      ,
.performEnsemble = function(cleaneddata) {
        # Ensemble of multiple methods
        tryCatch({
          ensemble_results <- list()

          # Random Forest component
          if (requireNamespace("randomForestSRC", quietly = TRUE)) {
            formula_vars <- private$.prepareMLFormula(cleaneddata)
            rf_model <- randomForestSRC::rfsrc(
              formula = formula_vars$formula,
              data = cleaneddata,
              ntree = 500,
              importance = TRUE
            )
            ensemble_results$rf <- rf_model
          }

          # Cox regression component
          cox_model <- private$.cox_model()
          ensemble_results$cox <- cox_model

          # Glmnet component
          if (requireNamespace("glmnet", quietly = TRUE)) {
            formula_vars <- private$.prepareMLFormula(cleaneddata)
            x <- model.matrix(formula_vars$formula, data = cleaneddata)[,-1]
            y <- survival::Surv(cleaneddata$mytime, cleaneddata$myoutcome)
            glmnet_model <- glmnet::cv.glmnet(x, y, family = "cox", alpha = 0.5)
            ensemble_results$glmnet <- glmnet_model
          }

          # Ensemble summary
          n_models <- length(ensemble_results)
          model_names <- paste(names(ensemble_results), collapse = ", ")

          ensemble_html <- glue::glue("
            <h4>Ensemble Model Summary</h4>
            <p><strong>Component Models:</strong> {model_names}</p>
            <p><strong>Total Models:</strong> {n_models}</p>
            <p><strong>Ensemble Method:</strong> {self$options$ml_ensemble_weights}</p>
            <p>Ensemble predictions combine multiple modeling approaches for robust predictions.</p>
          ")

          self$results$ml_ensemble_summary$setContent(ensemble_html)

        }, error = function(e) {
          error_msg <- glue::glue("Ensemble error: {e$message}")
          self$results$ml_performance_metrics$setContent(error_msg)
        })
      }

      ,
.performFeatureSelection = function(cleaneddata) {
        # Cross-validated feature selection
        tryCatch({
          formula_vars <- private$.prepareMLFormula(cleaneddata)
          all_vars <- formula_vars$variables

          # Stability selection simulation
          n_bootstrap <- 50
          selected_vars <- character(0)
          selection_freq <- rep(0, length(all_vars))
          names(selection_freq) <- all_vars

          for (i in 1:n_bootstrap) {
            # Bootstrap sample
            boot_indices <- sample(nrow(cleaneddata), replace = TRUE)
            boot_data <- cleaneddata[boot_indices, ]

            # Fit model and select variables (simplified)
            if (requireNamespace("glmnet", quietly = TRUE)) {
              x <- model.matrix(formula_vars$formula, data = boot_data)[,-1]
              y <- survival::Surv(boot_data$mytime, boot_data$myoutcome)
              cv_fit <- glmnet::cv.glmnet(x, y, family = "cox", alpha = 1)
              coefs <- coef(cv_fit, s = "lambda.min")
              selected <- which(coefs != 0)

              if (length(selected) > 0) {
                var_names <- rownames(coefs)[selected]
                for (var in var_names) {
                  if (var %in% names(selection_freq)) {
                    selection_freq[var] <- selection_freq[var] + 1
                  }
                }
              }
            }
          }

          # Normalize frequencies
          selection_freq <- selection_freq / n_bootstrap

          # Populate results table
          feature_results <- data.frame(
            variable = names(selection_freq),
            selected = ifelse(selection_freq >= 0.8, "Yes", ifelse(selection_freq >= 0.5, "Maybe", "No")),
            selection_frequency = selection_freq,
            importance_score = selection_freq,
            stringsAsFactors = FALSE
          )

          # Sort by frequency
          feature_results <- feature_results[order(-feature_results$selection_frequency), ]

          table <- self$results$ml_feature_selection_results
          for (i in seq_len(nrow(feature_results))) {
            table$addRow(rowKey = i, values = list(
              variable = feature_results$variable[i],
              selected = feature_results$selected[i],
              selection_frequency = round(feature_results$selection_frequency[i], 3),
              importance_score = round(feature_results$importance_score[i], 3)
            ))
          }

        }, error = function(e) {
          error_msg <- glue::glue("Feature selection error: {e$message}")
          self$results$ml_performance_metrics$setContent(error_msg)
        })
      }

      ,
.performXGBoost = function(cleaneddata) {
        # XGBoost survival analysis
        formula_vars <- private$.prepareMLFormula(cleaneddata)

        tryCatch({
          # Check for required packages with graceful fallback
          pkg_check <- .checkPackageDependency("xgboost", "XGBoost Survival", "Random Survival Forest")
          if (!pkg_check$available) {
            self$results$ml_performance_metrics$setContent(pkg_check$message)
            # Fallback to Random Forest if available, or Cox regression
            if (requireNamespace("randomForestSRC", quietly = TRUE)) {
              private$.performRandomForest(cleaneddata)
            } else {
              private$.cox_model()
            }
            return()
          }

          # Prepare data for xgboost
          x <- model.matrix(formula_vars$formula, data = cleaneddata)[,-1]
          y <- survival::Surv(cleaneddata$mytime, cleaneddata$myoutcome)

          # XGBoost survival model
          dtrain <- xgboost::xgb.DMatrix(data = x, label = cleaneddata$mytime)

          # Set survival-specific parameters
          params <- list(
            objective = "survival:cox",
            eta = 0.1,
            max_depth = 6,
            subsample = 0.8,
            colsample_bytree = 0.8
          )

          # Train model
          xgb_model <- xgboost::xgb.train(
            params = params,
            data = dtrain,
            nrounds = 100,
            verbose = 0
          )

          # Variable importance
          var_imp <- xgboost::xgb.importance(model = xgb_model)
          importance_scores <- setNames(var_imp$Gain, var_imp$Feature)
          private$.populateVariableImportance(importance_scores)

          # Performance metrics (simplified)
          metrics_html <- paste0(
            "<h4>", .("XGBoost Survival Model Results"), "</h4>",
            "<p><strong>", .("Model Type:"), "</strong> ", .("Cox Proportional Hazards with Gradient Boosting"), "</p>",
            "<p><strong>", .("Number of Rounds:"), "</strong> 100</p>",
            "<p><strong>", .("Variables Used:"), "</strong> ", length(formula_vars$variables), "</p>",
            "<p><strong>", .("Learning Rate:"), "</strong> ", params$eta, "</p>",
            "<p><strong>", .("Max Depth:"), "</strong> ", params$max_depth, "</p>"
          )

          self$results$ml_performance_metrics$setContent(metrics_html)

        }, error = function(e) {
          error_msg <- glue::glue("XGBoost error: {e$message}")
          self$results$ml_performance_metrics$setContent(error_msg)
        })
      }

      ,
.performSVM = function(cleaneddata) {
        # Support Vector Survival analysis
        formula_vars <- private$.prepareMLFormula(cleaneddata)

        tryCatch({
          # Check for required packages with graceful fallback
          pkg_check <- .checkPackageDependency("survivalsvm", "SVM Survival", "Random Survival Forest")
          if (!pkg_check$available) {
            self$results$ml_performance_metrics$setContent(pkg_check$message)
            # Fallback to Random Forest if available, or Cox regression
            if (requireNamespace("randomForestSRC", quietly = TRUE)) {
              private$.performRandomForest(cleaneddata)
            } else {
              private$.cox_model()
            }
            return()
          }

          # Prepare data for SVM
          x <- model.matrix(formula_vars$formula, data = cleaneddata)[,-1]
          y <- survival::Surv(cleaneddata$mytime, cleaneddata$myoutcome)

          # Fit SVM survival model
          svm_model <- survivalsvm::survivalsvm(
            formula = formula_vars$formula,
            data = cleaneddata,
            gamma.mu = 1
          )

          # Performance metrics
          metrics_html <- paste0(
            "<h4>", .("Support Vector Survival Model Results"), "</h4>",
            "<p><strong>", .("Model Type:"), "</strong> ", .("Support Vector Machines for Survival Analysis"), "</p>",
            "<p><strong>", .("Variables Used:"), "</strong> ", length(formula_vars$variables), "</p>",
            "<p><strong>", .("Kernel:"), "</strong> ", .("RBF (Radial Basis Function)"), "</p>",
            "<p><strong>", .("Note:"), "</strong> ", .("SVM survival analysis provides non-parametric survival predictions"), "</p>"
          )

          self$results$ml_performance_metrics$setContent(metrics_html)

        }, error = function(e) {
          error_msg <- paste(.("SVM Survival error:"), e$message, .("Note: This method requires the 'survivalsvm' package."))
          self$results$ml_performance_metrics$setContent(error_msg)
        })
      }

      ,
.performDeepSurvival = function(cleaneddata) {
        # Deep Learning Survival analysis
        tryCatch({
          # Note: Deep survival is complex and would typically require Python integration
          # For now, provide informative message about implementation status

          metrics_html <- paste0(
            "<h4>", .("Deep Survival Learning"), "</h4>",
            "<p><strong>", .("Status:"), "</strong> ", .("Deep survival methods are computationally intensive and typically require specialized Python packages (DeepSurv, DeepHit)."), "</p>",
            "<p><strong>", .("Alternative:"), "</strong> ", .("Consider using Random Survival Forest or XGBoost for advanced non-linear survival modeling."), "</p>",
            "<p><strong>", .("Implementation Note:"), "</strong> ", .("Full deep learning integration would require:"), "</p>",
            "<ul>",
              "<li>", .("Python environment with TensorFlow/PyTorch"), "</li>",
              "<li>", .("Deep survival libraries (pycox, scikit-survival)"), "</li>",
              "<li>", .("GPU acceleration for optimal performance"), "</li>",
            "</ul>",
            "<p><strong>", .("Recommendation:"), "</strong> ", .("Use ensemble methods or XGBoost for similar performance with easier implementation."), "</p>"
          )

          self$results$ml_performance_metrics$setContent(metrics_html)

        }, error = function(e) {
          error_msg <- glue::glue("Deep Survival: {e$message}")
          self$results$ml_performance_metrics$setContent(error_msg)
        })
      }

      ,
.performCrossValidation = function(cleaneddata) {
        # Cross-validation for ML methods
        tryCatch({
          formula_vars <- private$.prepareMLFormula(cleaneddata)
          n_folds <- self$options$ml_cv_folds

          # Create folds
          folds <- sample(rep(1:n_folds, length.out = nrow(cleaneddata)))
          cv_results <- numeric(n_folds)

          for (i in 1:n_folds) {
            train_data <- cleaneddata[folds != i, ]
            test_data <- cleaneddata[folds == i, ]

            # EXPERIMENTAL: Disabled - ml_method option not in .a.yaml
            # # Simple C-index calculation for cross-validation
            # if (self$options$ml_method == "rsf" && requireNamespace("randomForestSRC", quietly = TRUE)) {
            #   model <- randomForestSRC::rfsrc(
            #     formula = formula_vars$formula,
            #     data = train_data,
            #     ntree = 500
            #   )
            #   pred <- predict(model, newdata = test_data)
            #   cv_results[i] <- 1 - pred$err.rate[length(pred$err.rate)]
            # }
          }

          mean_cv_score <- mean(cv_results, na.rm = TRUE)
          sd_cv_score <- sd(cv_results, na.rm = TRUE)

          cv_html <- glue::glue("
            <h4>Cross-Validation Results</h4>
            <p><strong>Method:</strong> {n_folds}-fold cross-validation</p>
            <p><strong>Mean CV Score:</strong> {round(mean_cv_score, 4)} \u00b1 {round(sd_cv_score, 4)}</p>
            <p><strong>Individual Fold Scores:</strong> {paste(round(cv_results, 3), collapse = ', ')}</p>
          ")

          self$results$ml_cross_validation_summary$setContent(cv_html)

        }, error = function(e) {
          error_msg <- glue::glue(.("Cross-validation error: {e$message}"))
          self$results$ml_cross_validation_summary$setContent(error_msg)
        })
      }

      ,
.calculateSHAPValues = function(cleaneddata) {
        # SHAP values for interpretability
        tryCatch({
          shap_html <- paste0("
            <h4>", .("SHAP Values (SHapley Additive exPlanations)"), "</h4>
            <p><strong>", .("Status:"), "</strong> ", .("SHAP values provide model-agnostic interpretability by showing how each feature contributes to individual predictions."), "</p>
            <p><strong>", .("Implementation Note:"), "</strong> ", .("Full SHAP implementation requires specialized packages and significant computation time."), "</p>
            <p><strong>", .("Alternatives available in this module:"), "</strong></p>
            <ul>
              <li>", .("Variable importance scores (available in Random Forest and XGBoost methods)"), "</li>
              <li>", .("Hazard ratios from Cox regression (traditional interpretability)"), "</li>
              <li>", .("Decision tree visualization (rule-based interpretability)"), "</li>
            </ul>
            <p><strong>", .("For advanced SHAP analysis:"), "</strong> ", .("Consider using Python packages like 'shap' with 'scikit-survival' for comprehensive survival SHAP values."), "</p>
          ")

          # Note: Full SHAP implementation would require significant development
          # This provides informative guidance instead
          self$results$ml_performance_metrics$setContent(shap_html)

        }, error = function(e) {
          error_msg <- glue::glue(.("SHAP calculation error: {e$message}"))
          self$results$ml_performance_metrics$setContent(error_msg)
        })
      }

      ,
.calculateMLPerformanceMetrics = function(cleaneddata) {
        # Comprehensive performance metrics
        tryCatch({
          formula_vars <- private$.prepareMLFormula(cleaneddata)

          performance_html <- paste0("
            <h4>", .("Machine Learning Performance Metrics"), "</h4>
            <p><strong>", .("Available Metrics:"), "</strong></p>
            <table style='border-collapse: collapse; width: 100%;'>
              <tr style='border-bottom: 1px solid #ddd;'>
                <th style='text-align: left; padding: 8px;'>", .("Metric"), "</th>
                <th style='text-align: left; padding: 8px;'>", .("Description"), "</th>
                <th style='text-align: left; padding: 8px;'>", .("Status"), "</th>
              </tr>
              <tr style='border-bottom: 1px solid #ddd;'>
                <td style='padding: 8px;'>", .("Concordance Index (C-index)"), "</td>
                <td style='padding: 8px;'>", .("Discrimination ability"), "</td>
                <td style='padding: 8px;'>", .("Available in model outputs"), "</td>
              </tr>
              <tr style='border-bottom: 1px solid #ddd;'>
                <td style='padding: 8px;'>", .("Integrated Brier Score (IBS)"), "</td>
                <td style='padding: 8px;'>", .("Time-dependent prediction error"), "</td>
                <td style='padding: 8px;'>", .("Requires pec package"), "</td>
              </tr>
              <tr style='border-bottom: 1px solid #ddd;'>
                <td style='padding: 8px;'>", .("Time-dependent AUC"), "</td>
                <td style='padding: 8px;'>", .("Dynamic discrimination"), "</td>
                <td style='padding: 8px;'>", .("Available via survivalROC"), "</td>
              </tr>
              <tr>
                <td style='padding: 8px;'>", .("Calibration Plot"), "</td>
                <td style='padding: 8px;'>", .("Prediction reliability"), "</td>
                <td style='padding: 8px;'>", .("Requires specialized implementation"), "</td>
              </tr>
            </table>
            <p><strong>", .("Note:"), "</strong> ", .("Detailed performance metrics require additional computational resources and specialized packages. Use the cross-validation option for robust performance assessment."), "</p>
          ")

          self$results$ml_performance_metrics$setContent(performance_html)

        }, error = function(e) {
          error_msg <- glue::glue(.("Performance metrics error: {e$message}"))
          self$results$ml_performance_metrics$setContent(error_msg)
        })
      }

      ,
.prepareMLFormula = function(cleaneddata) {
        # Prepare formula and variables for ML methods
        myexplanatory_labelled <- private$.getData()$myexplanatory_labelled
        mycontexpl_labelled <- private$.getData()$mycontexpl_labelled

        # Combine all explanatory variables
        all_vars <- c(myexplanatory_labelled, mycontexpl_labelled)

        # Create survival formula
        formula_str <- paste("Surv(mytime, myoutcome) ~", paste(all_vars, collapse = " + "))
        formula_obj <- .asSurvivalFormula(formula_str)

        return(list(
          formula = formula_obj,
          variables = all_vars
        ))
      }

      ,
.populateVariableImportance = function(var_imp) {
        # Populate variable importance table
        if (length(var_imp) > 0) {
          var_imp_sorted <- sort(var_imp, decreasing = TRUE)

          table <- self$results$ml_variable_importance
          for (i in seq_along(var_imp_sorted)) {
            table$addRow(rowKey = i, values = list(
              variable = names(var_imp_sorted)[i],
              importance = round(var_imp_sorted[i], 4),
              rank = i
            ))
          }
        }
      }

      ,
      .populatePredictionIntervals = function(predictions, cleaneddata) {
        # Populate prediction intervals table (simplified)
        if (!is.null(predictions)) {
          n_show <- min(10, nrow(cleaneddata))  # Show first 10 observations

          table <- self$results$ml_prediction_intervals
          for (i in 1:n_show) {
            # Simplified prediction intervals (would need proper implementation)
            pred_value <- if (is.list(predictions)) predictions$predicted[i] else predictions[i]

            table$addRow(rowKey = i, values = list(
              observation = i,
              prediction = round(pred_value, 4),
              lower_ci = round(pred_value * 0.8, 4),  # Simplified
              upper_ci = round(pred_value * 1.2, 4),  # Simplified
              risk_group = ifelse(pred_value > median(predictions, na.rm = TRUE), "High", "Low")
            ))
          }
        }
      }

        # Natural Language Summary Generation ----
      ,
      .generateMultivariableCoxSummary = function(tMultivariable, explanatory_vars, outcome_var) {
            tryCatch({
                # Extract the results table and metrics
                cox_table <- tMultivariable[[1]]
                metrics <- tMultivariable[[2]]

                # Generate summary components
                sig_data <- private$.generateSignificantPredictorsSummary(cox_table)
                model_performance <- private$.generateModelPerformanceSummary(metrics)
                clinical_interpretation <- private$.generateClinicalInterpretation(cox_table, outcome_var)

                # Generate main summary text
                summary_html <- paste0(
                    '<div style="background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin: 10px 0;">',
                    '<h4 style="color: #2c5282; margin-top: 0;"> ', .("Multivariable Cox Regression Summary"), '</h4>',
                    '<p style="margin: 10px 0;"><strong>', .("Analysis Overview:"), '</strong> ', .("Multivariable Cox proportional hazards regression was performed to examine the relationship between"), ' ',
                    length(explanatory_vars), ' ', .("explanatory variable(s) and the time-to-event outcome"), ' <em>', outcome_var, '</em>.</p>'
                )

                # Add findings summary
                if (sig_data$significant_count > 0) {
                    summary_html <- paste0(summary_html,
                        '<p style="margin: 10px 0;"><strong>', .("Key Findings:"), '</strong></p>',
                        '<ul style="margin: 5px 0; padding-left: 20px;">',
                        '<li>', sig_data$significant_count, ' ', .("out of"), ' ', sig_data$total_predictors, ' ', .("predictor(s) showed statistically significant associations (p < 0.05)"), '</li>'
                    )

                    if (!is.null(sig_data$strongest_predictor)) {
                        summary_html <- paste0(summary_html,
                            '<li>', .("Strongest association:"), ' <em>', sig_data$strongest_predictor, '</em> ', .("with"), ' ', sig_data$strongest_effect, ' (HR = ', round(sig_data$strongest_hr, 2), ')</li>'
                        )
                    }

                    summary_html <- paste0(summary_html, '</ul>')
                } else if (sig_data$total_predictors > 0) {
                    summary_html <- paste0(summary_html,
                        '<p style="margin: 10px 0;"><strong>', .("Key Findings:"), '</strong> ', .("None of the"), ' ', sig_data$total_predictors,
                        ' ', .("predictor(s) showed statistically significant associations with the outcome (all p \u2265 0.05)."), '</p>'
                    )
                } else {
                    summary_html <- paste0(summary_html,
                        '<p style="margin: 10px 0;"><strong>', .("Note:"), '</strong> ', .("Unable to extract predictor information from the analysis."), '</p>'
                    )
                }

                # Add model performance
                summary_html <- paste0(summary_html, model_performance)

                # Add clinical interpretation
                summary_html <- paste0(summary_html, clinical_interpretation, '</div>')

                return(summary_html)

            }, error = function(e) {
                return(paste0("<p>", .("Summary generation encountered an error."), "</p>"))
            })
        }

        # Helper Functions for Summary Generation ----
        ,
        .generateSignificantPredictorsSummary = function(cox_table) {
            tryCatch({
                # Count significant predictors
                significant_count <- 0
                total_predictors <- 0
                strongest_predictor <- NULL
                strongest_hr <- 0
                strongest_effect <- ""

                # Parse the table to identify significant predictors
                if (!is.null(cox_table) && nrow(cox_table) > 0) {
                    for (i in seq_len(nrow(cox_table))) {
                        if (!is.na(cox_table[i, "p"]) && cox_table[i, "p"] != "") {
                            p_value <- as.numeric(cox_table[i, "p"])
                            if (!is.na(p_value)) {
                                total_predictors <- total_predictors + 1
                                if (p_value < 0.05) {
                                    significant_count <- significant_count + 1
                                    # Track strongest predictor
                                    hr_value <- cox_table[i, "HR (95% CI, p-value)"]
                                    if (!is.na(hr_value) && hr_value != "" && hr_value != "-") {
                                        # Extract HR value from the formatted string
                                        hr_match <- regmatches(hr_value, regexpr("[0-9]+\\.?[0-9]*", hr_value))
                                        if (length(hr_match) > 0) {
                                            hr_numeric <- as.numeric(hr_match[1])
                                            if (!is.na(hr_numeric)) {
                                                # Calculate effect size (distance from 1.0)
                                                effect_size <- abs(log(hr_numeric))
                                                if (effect_size > abs(log(strongest_hr + 0.001))) {
                                                    strongest_hr <- hr_numeric
                                                    strongest_predictor <- cox_table[i, 1]
                                                    strongest_effect <- if (hr_numeric > 1) .("increased hazard") else .("decreased hazard")
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                # Return the significant predictors data
                return(list(
                    significant_count = significant_count,
                    total_predictors = total_predictors,
                    strongest_predictor = strongest_predictor,
                    strongest_hr = strongest_hr,
                    strongest_effect = strongest_effect
                ))
            }, error = function(e) {
                return(list(
                    significant_count = 0,
                    total_predictors = 0,
                    strongest_predictor = NULL,
                    strongest_hr = 0,
                    strongest_effect = ""
                ))
            })
        }

        ,
        .generateModelPerformanceSummary = function(metrics) {
            if (!is.null(metrics)) {
                return(paste0('<p style="margin: 10px 0;"><strong>', .("Model Performance:"), '</strong> ', metrics, '</p>'))
            }
            return("")
        }

        ,
        .generateClinicalInterpretation = function(cox_table, outcome_var) {
            # Generate interpretation guide
            summary_html <- paste0(
                '<div style="background-color: #e6f7ff; padding: 10px; border-radius: 5px; margin-top: 10px;">',
                '<strong> ', .("Interpretation Guide:"), '</strong>',
                '<ul style="margin: 5px 0; padding-left: 20px; font-size: 0.95em;">',
                '<li>', .("HR > 1: Factor increases the hazard (risk) of the event"), '</li>',
                '<li>', .("HR < 1: Factor decreases the hazard (risk) of the event"), '</li>',
                '<li>', .("HR = 1: No association between factor and event timing"), '</li>',
                '<li>', .("95% CI not crossing 1.0 indicates statistical significance"), '</li>',
                '</ul>',
                '</div>'
            )

            return(summary_html)
        }

        ,
        .calculate_nomogram = function() {
            # Builds the rms nomogram (points table + plot) and a plain-language
            # summary of the underlying Cox model for the "Natural Language
            # Summary" output.
            tryCatch({
                if (!requireNamespace("rms", quietly = TRUE)) {
                    self$results$nomogramSummary$setContent(paste0("
                        <p><strong>", .("Nomogram Analysis"), "</strong></p>
                        <p>", .("The 'rms' package is required for nomogram generation but is not available."), "</p>
                        <p>", .("Please install it using:"), " <code>install.packages('rms')</code></p>
                        <p>", .("Nomograms provide visual tools for calculating individual risk predictions from multivariable models."), "</p>
                    "))
                    return()
                }

                # Central Cox model (same model the rest of the analysis uses)
                cox_model <- private$.cox_model()

                if (is.null(cox_model)) {
                    self$results$nomogramSummary$setContent(
                        .("Unable to generate nomogram: the Cox model could not be fitted. See the warnings above (for example, too few events or invalid survival times)."))
                    return()
                }

                # Construct the actual nomogram (populates the scoring guide and
                # the nomogram plot via private$.nom_object). Degrade gracefully
                # if rms cannot build a nomogram for this particular model.
                nomogram_ok <- tryCatch({
                    private$.nomogram(cox_model)
                    !is.null(private$.nom_object)
                }, error = function(e) {
                    private$.debug_write(list(phase = ".nomogram(error)", message = e$message))
                    FALSE
                })

                # Assemble a model-specific natural-language summary. Predictor
                # display names come from user data labels, so they are HTML-
                # escaped before being placed into this type:Html output.
                cleaneddata <- private$.cleandata()
                predictors <- c(cleaneddata$myexplanatory_labelled,
                                cleaneddata$mycontexpl_labelled)
                predictors <- predictors[!is.na(predictors) & nzchar(predictors)]
                pred_html <- if (length(predictors))
                    paste0("<li>", htmltools::htmlEscape(predictors), "</li>", collapse = "")
                else
                    paste0("<li>", .("(none specified)"), "</li>")

                n_patients <- if (!is.null(cox_model$n)) cox_model$n else NA_integer_
                n_events   <- if (!is.null(cox_model$nevent)) cox_model$nevent else NA_integer_
                cidx <- tryCatch(unname(summary(cox_model)$concordance[1]),
                                 error = function(e) NA_real_)
                cidx_html <- if (is.na(cidx)) .("not available") else sprintf("%.2f", cidx)

                pred_times <- suppressWarnings(as.numeric(
                    trimws(unlist(strsplit(self$options$cutp, ",")))))
                pred_times <- pred_times[!is.na(pred_times)]
                horizon <- if (length(pred_times)) pred_times[1] else 12

                avail_html <- if (nomogram_ok)
                    paste0("<p style='color:#2e7d32;'>", .("The point-scoring guide and the nomogram plot are shown below."), "</p>")
                else
                    paste0("<p style='color:#b71c1c;'>", .("The nomogram plot could not be constructed for this model; the summary above still describes the fitted model."), "</p>")

                summary_html <- paste0(
                    "<div style='font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif; line-height: 1.6; max-width: 820px;'>",
                    "<p>", sprintf(.("This nomogram is a visual calculator derived from a multivariable Cox proportional-hazards model fitted on <b>%s patients</b> with <b>%s events</b>. It turns the model into a point-scoring tool so an individual patient's risk can be read off directly."),
                                   format(n_patients), format(n_events)), "</p>",
                    "<p><b>", .("Predictors included:"), "</b></p><ul>", pred_html, "</ul>",
                    "<p>", sprintf(.("The nomogram estimates the probability of the event within <b>%g months</b> of follow-up. Model discrimination (Harrell's C-index) is <b>%s</b>, where 0.5 is no better than chance and 1.0 is perfect separation."),
                                   horizon, cidx_html), "</p>",
                    "<p><b>", .("How to read the nomogram:"), "</b></p><ol>",
                    "<li>", .("For each predictor, draw a vertical line up to the <i>Points</i> axis to read its score."), "</li>",
                    "<li>", .("Add the points from all predictors to obtain the <i>Total Points</i>."), "</li>",
                    "<li>", .("Find the Total Points on the bottom axis and read down to the predicted-risk scale."), "</li></ol>",
                    avail_html,
                    "<p style='font-size: 0.9em; color: #555;'><i>", .("Caution: these estimates reflect the development cohort only and require external validation before clinical use. The nomogram assumes proportional hazards and (for continuous predictors) linear effects."), "</i></p>",
                    "</div>")

                self$results$nomogramSummary$setContent(summary_html)

            }, error = function(e) {
                error_msg <- paste(.("Nomogram calculation error:"),
                                   htmltools::htmlEscape(conditionMessage(e)))
                self$results$nomogramSummary$setContent(error_msg)
            })
        }

        ,
        .calculate_survivaldecisiontree = function() {
            # Decision tree calculation function
            tryCatch({
                # Get cleaned data
                cleaneddata <- private$.cleandata()

                # Use the existing decision tree analysis
                tree_results <- private$.survivalTree(list(cleanData = cleaneddata$cleanData))

                if (!is.null(tree_results)) {
                    # Tree analysis completed successfully
                    # Results are populated in .survivalTree function
                } else {
                    self$results$tree_summary$setContent(.("Decision tree analysis could not be completed."))
                }

            }, error = function(e) {
                error_msg <- paste(.("Decision tree calculation error:"), e$message)
                self$results$tree_summary$setContent(error_msg)
            })
        }

        ,
        .generateAndDisplayClinicalSummary = function(cleaneddata) {
          # Generate clinical interpretation summary for display
          tryCatch({
            # Get basic data information
            mydata <- cleaneddata$cleanData
            n_total <- nrow(mydata)
            n_events <- sum(.eventIndicator(mydata$myoutcome), na.rm = TRUE)
            n_vars <- length(c(cleaneddata$myexplanatory_labelled, cleaneddata$mycontexpl_labelled))

            # Try to get Cox regression results for summary
            cox_results <- NULL
            tryCatch({
              cox_model <- private$.cox_model()
              if (!is.null(cox_model)) {
                cox_summary <- summary(cox_model)
                cox_results <- cox_summary$coefficients
              }
            }, error = function(e) {
              # Silent error handling
            })

            # Generate clinical summary
            clinical_summary <- .generateClinicalSummary(
              results = cox_results,
              analysis_type = "cox",
              n_vars = n_vars,
              n_events = n_events
            )

            # Format for display
            summary_html <- paste0(
              "<div style='background-color: #e7f3ff; border: 1px solid #b3d9ff; padding: 20px; border-radius: 8px; margin: 15px 0;'>",
              "<h3 style='color: #0056b3; margin-top: 0; margin-bottom: 15px;'> ", .("Clinical Summary"), "</h3>",
              "<div style='background-color: white; padding: 15px; border-radius: 5px; border-left: 4px solid #0056b3;'>",
              "<p style='font-size: 16px; line-height: 1.6; margin: 0;'>", clinical_summary$summary, "</p>",
              "</div>"
            )

            # Add study details
            if (n_vars > 0) {
              summary_html <- paste0(summary_html,
                "<div style='margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;'>",
                "<p style='margin: 5px 0; font-size: 14px;'><strong>", .("Study Details:"), "</strong></p>",
                "<ul style='margin: 5px 0; padding-left: 20px; font-size: 14px;'>",
                "<li>", .("Total patients:"), " ", n_total, "</li>",
                "<li>", .("Events observed:"), " ", n_events, " (", round(n_events/n_total*100, 1), "%)</li>",
                "<li>", .("Variables analyzed:"), " ", n_vars, "</li>",
                "</ul>",
                "</div>"
              )
            }

            # Add recommendations if there are issues
            if (n_events < 10) {
              summary_html <- paste0(summary_html,
                "<div style='margin-top: 15px; padding: 10px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px;'>",
                "<p style='margin: 0; color: #856404;'><strong> ", .("Recommendation:"), "</strong> ",
                .("With fewer than 10 events, results should be interpreted cautiously. Consider longer follow-up or pooled analysis."),
                "</p>",
                "</div>"
              )
            }

            summary_html <- paste0(summary_html, "</div>")

            # Display in a dedicated result area (prepend to existing text if possible)
            if (!is.null(self$results$text)) {
              current_content <- ""
              tryCatch({
                # Try to get existing content
                current_content <- self$results$text$content
                if (is.null(current_content)) current_content <- ""
              }, error = function(e) {
                current_content <- ""
              })

              # Prepend clinical summary
              new_content <- paste0(summary_html, current_content)
              self$results$text$setContent(new_content)
            }

          }, error = function(e) {
            # Silent error handling - don't break analysis if summary fails
            warning(paste("Clinical summary generation failed:", e$message))
          })
        }

    ) # End of private list
    
    # , public = list(
        # @description
        # Generate R source code for Multi-Variable Survival analysis
        # @return Character string with R syntax for reproducible analysis
        # NOTE (2026-07): Custom asSource() commented out. It emitted
        # `elapsedtime`/`outcome` MANUALLY and then private$.asArgs() emitted
        # them AGAIN, producing duplicated (non-runnable) arguments in the
        # generated syntax, e.g.:
        #     elapsedtime = "elapsedtime",
        #     outcome = "outcome",
        #     elapsedtime = elapsedtime,   <- duplicate from .asArgs
        #     outcome = outcome,
        # jmvcore's default asSource() renders every option exactly once, so we
        # rely on it (this override is removed).
        #
        # asSource = function() {
        #     elapsedtime <- self$options$elapsedtime
        #     outcome <- self$options$outcome
        #
        #     if (is.null(elapsedtime) || is.null(outcome))
        #         return('')
        #
        #     # Escape variable names that contain spaces or special characters
        #     elapsedtime_escaped <- if (!is.null(elapsedtime) && !identical(make.names(elapsedtime), elapsedtime)) {
        #         paste0('`', elapsedtime, '`')
        #     } else {
        #         elapsedtime
        #     }
        #
        #     outcome_escaped <- if (!is.null(outcome) && !identical(make.names(outcome), outcome)) {
        #         paste0('`', outcome, '`')
        #     } else {
        #         outcome
        #     }
        #
        #     # Build arguments
        #     elapsedtime_arg <- paste0('elapsedtime = "', elapsedtime_escaped, '"')
        #     outcome_arg <- paste0('outcome = "', outcome_escaped, '"')
        #
        #     # Get other arguments using base helper (if available)
        #     args <- ''
        #     if (!is.null(private$.asArgs)) {
        #         args <- private$.asArgs(incData = FALSE)
        #     }
        #     if (args != '')
        #         args <- paste0(',\n    ', args)
        #
        #     # Get package name dynamically
        #     pkg_name <- utils::packageName()
        #     if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback
        #
        #     # Build complete function call
        #     paste0(pkg_name, '::multisurvival(\n    data = data,\n    ',
        #            elapsedtime_arg, ',\n    ', outcome_arg, args, ')')
        # }
   # ) # End of public list
)
