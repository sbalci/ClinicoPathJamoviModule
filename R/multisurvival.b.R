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



# Helper function for comprehensive data validation
.validateSurvivalData <- function(data, time_var = "mytime", outcome_var = "myoutcome",
                                  event_level = NULL, multievent = FALSE,
                                  analysistype = "overall",
                                  dod = NULL, dooc = NULL, awd = NULL, awod = NULL) {
  issues <- list()
  warnings <- list()
  event_indicator <- NULL

  # DEBUG: show the incoming time variable class/summary when needed
  if (isTRUE(getOption("multisurvival.debug"))) {
    message("[multisurvival.debug] validate: time_var class = ", paste(class(data[[time_var]]), collapse = "/"))
  }

  # Check for negative survival times
  if (length(time_var) == 1L && !is.na(time_var) && time_var %in% names(data)) {
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
      warnings <- append(
        warnings,
        paste(
          "Zero survival times detected:", zero_times,
          "observations. Verify the time origin and same-time event convention;",
          "do not add an arbitrary constant solely to change these observations."
        )
      )
    }
  }

  # Check outcome coding
  if (length(outcome_var) == 1L && !is.na(outcome_var) && outcome_var %in% names(data)) {
    outcome_vec <- data[[outcome_var]]

    # In multi-event mode the four-level clinical outcome (e.g. DOD / DOOC /
    # AWD / AWOD) is mapped by dod/dooc/awd/awod, not by event_level, and the
    # mapping happens later in .definemyoutcome(). Validating the RAW outcome
    # here with .eventIndicator() rejected exactly the input this mode exists to
    # accept: "Outcome Factor Has Unsupported Levels ... AWD, AWOD, DOD, DOOC",
    # which made the whole Fine-Gray pathway unreachable from its normal
    # clinical input. Use the same shared coder the analysis will use.
    if (isTRUE(multievent)) {
      res <- .defineEventIndicator(outcome_vec, multievent = TRUE,
                                   analysistype = analysistype,
                                   dod = dod, dooc = dooc, awd = awd, awod = awod)
      # A mapping problem is reported by .definemyoutcome() with a far better
      # message than this validator can give, so only the derived indicator is
      # taken here.
      event_indicator <- if (is.null(res$error)) res$status == 1 else NULL

    } else {
      # Build an event indicator safely (handles factors, logicals, numeric)
      event_indicator <- .eventIndicator(outcome_vec, event_level)

      # Only enforce binary check when the underlying values are numeric/logical
      if (is.numeric(outcome_vec) || is.logical(outcome_vec)) {
        unique_outcomes <- unique(outcome_vec[!is.na(outcome_vec)])
        if (!all(unique_outcomes %in% c(0, 1, TRUE, FALSE))) {
          issues <- append(issues, "Outcome should be binary (0/1 or TRUE/FALSE)")
        }
      } else if (is.factor(outcome_vec) && length(levels(outcome_vec)) > 2) {
        warnings <- append(warnings, "Outcome has multiple levels; analysis will treat non-event levels as censored where applicable.")
      }
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
#
# `event_level` is the user's Event Level selection. Passing it matters: this
# helper used to reject any factor whose levels were neither "Event" nor
# numeric-coercible, which killed the whole analysis for an ordinary
# "Alive"/"Dead" outcome -- the exact case the Event Level option exists to
# handle. It is reached from .validateSurvivalData() and ten other call sites,
# so the rejection was not a corner case.
.eventIndicator <- function(outcome_vec, event_level = NULL) {
  if (is.null(outcome_vec)) {
    return(NULL)
  }

  # Factor handling: competing risk encoding uses an "Event" level
  if (is.factor(outcome_vec) || is.character(outcome_vec)) {
    if ("Event" %in% levels(outcome_vec)) {
      return(outcome_vec == "Event")
    }
    # An explicitly selected event level wins over any guessing below.
    if (!is.null(event_level) && length(event_level) > 0 &&
        as.character(event_level) %in% as.character(outcome_vec)) {
      return(as.character(outcome_vec) == as.character(event_level))
    }
    # Try to coerce factor levels to numeric (e.g., "0"/"1")
    suppressWarnings(num_levels <- as.numeric(as.character(outcome_vec)))
    if (!all(is.na(num_levels))) {
      return(num_levels >= 1)
    }
    jmvcore::reject(sprintf(
      "Outcome Factor Has Unsupported Levels: the outcome variable has non-numeric levels that cannot be interpreted as events: %s\n\nTo Fix:\n1. Select which level represents the event using the Event Level option.\n2. Or recode as numeric (0 = censored, 1 = event) or logical (FALSE/TRUE).\n3. For competing risks, use a factor with levels 'Censored', 'Event', 'Competing'.",
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
.generateClinicalSummary <- function(results, analysis_type = "cox", n_vars = 0, n_events = 0,
                                     term_map = NULL) {
  # `term_map` is coxph's $assign: a named list mapping each model TERM to the
  # coefficient indices it owns. Without it this function counted significant
  # coefficient ROWS and printed the total as a count of VARIABLES, so a single
  # three-level factor with two significant contrasts announced "2 out of 1
  # factors showed statistically significant associations" -- more significant
  # factors than factors examined.

  # Extract key statistics based on analysis type
  if (analysis_type %in% c("cox", "finegray") && !is.null(results)) {
    model_name <- if (identical(analysis_type, "finegray"))
      "Fine-Gray subdistribution hazards" else "Cox proportional hazards"
    effect_name <- if (identical(analysis_type, "finegray"))
      "subdistribution hazard ratio" else "hazard ratio"

    # Count significant variables if results is a table/data.frame
    sig_count <- 0
    strongest_var <- NULL
    strongest_hr <- 1
    strongest_effect <- NULL

    tryCatch({
      # `results` is summary(coxph)$coefficients, which is a MATRIX, not a data
      # frame -- so `is.data.frame(results)` was FALSE on every single run and
      # sig_count stayed hard-wired at 0. The Clinical Summary therefore always
      # declared "No statistically significant associations were identified",
      # printed directly above hazard ratios with p < 1e-15. A false negative
      # inside a box labelled "Clinical Summary" is about the worst place for
      # one, so accept a matrix and locate the p column by its actual name
      # (coxph calls it "Pr(>|z|)", or "Pr(>|z|)" with a robust variance).
      res_df <- if (is.matrix(results)) as.data.frame(results, stringsAsFactors = FALSE)
                else results

      p_col <- NULL
      if (is.data.frame(res_df)) {
        cand <- c("p", "Pr(>|z|)", "Pr(>|t|)", "p.value", "pvalue")
        hit  <- intersect(cand, names(res_df))
        if (length(hit) > 0) p_col <- hit[1]
        # Fall back to any column whose name starts with "Pr(".
        if (is.null(p_col)) {
          pr <- grep("^Pr\\(", names(res_df), value = TRUE)
          if (length(pr) > 0) p_col <- pr[1]
        }
      }

      if (is.data.frame(res_df) && !is.null(p_col)) {
        results <- res_df
        p_values <- suppressWarnings(as.numeric(res_df[[p_col]]))
        sig_indices <- which(p_values < 0.05 & !is.na(p_values))

        # Count VARIABLES, not coefficient rows. A term counts once if any of
        # its contrasts is significant; without the map fall back to rows but
        # never report more of them than there are variables.
        if (!is.null(term_map) && length(term_map) > 0) {
          sig_count <- sum(vapply(term_map, function(idx) {
            idx <- idx[idx >= 1 & idx <= length(p_values)]
            length(idx) > 0 && any(p_values[idx] < 0.05, na.rm = TRUE)
          }, logical(1)))
        } else {
          sig_count <- min(length(sig_indices), if (n_vars > 0) n_vars else length(sig_indices))
        }

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
    factor_word <- if (identical(n_vars, 1L) || identical(n_vars, 1)) "factor" else "factors"

    # Analysis overview
    summary_parts$overview <- paste0(
      "This multivariable ", model_name, " analysis examined", " ", n_vars, " ",
      "potential risk ", factor_word, " in", " ", "patients with", " ", n_events, " ",
      "events observed during follow-up."
    )

    # Key findings
    if (sig_count > 0) {
      summary_parts$findings <- paste0(
        "Key Finding:", " ", sig_count, " ", "out of", " ", n_vars, " ",
        factor_word, " showed statistically significant associations with the outcome", " (p < 0.05)."
      )

      if (!is.null(strongest_var) && !is.null(strongest_effect)) {
        summary_parts$strongest <- paste0(
          "Strongest predictor:", " ", strongest_var, " ", "was associated with", " ",
          strongest_effect, " (", effect_name, " = ", round(strongest_hr, 2), ")."
        )
      }
    } else {
      summary_parts$findings <- paste0(
        "No statistically significant associations were identified among the", " ",
        n_vars, " ", factor_word, " examined", " (", "all p-values \u2265 0.05", ")."
      )
    }

    # Clinical interpretation
    if (sig_count > 0 && !is.null(strongest_hr)) {
      summary_parts$interpretation <- paste0(
        "Clinical importance cannot be assigned from the ", effect_name, " magnitude or p-value alone. ",
        "Interpret the estimate with its confidence interval, outcome definition, predictor scale, ",
        "study design, and external evidence. This is an association observed in these data."
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

  paste0(
    "Clinical importance cannot be determined from a hazard ratio alone. ",
    "Interpret its confidence interval, outcome, predictor scale, study design, ",
    "and relevant clinical thresholds."
  )
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

      # Per-run compute caches. .cleandata() is invoked ~25x and .cox_model()
      # ~15x within a single .run(); each .cox_model() call re-fits Cox (and,
      # for competing risks, re-expands the dataset via survival::finegray).
      # These caches ensure each heavy computation runs at most once per run.
      # They are reset at the top of .run() via .resetComputeCaches() so a
      # re-run with changed options recomputes rather than serving stale results.
      .dataCache = NULL,
      # Result of .defineEventIndicator(), kept so .run() can render the
      # recode disclosure without redoing the work.
      .eventRecode = NULL,
      .dataComputed = FALSE,
      .coxCache = NULL,
      .coxComputed = FALSE,

      # R6 locks the private environment, so a field must be DECLARED here before
      # anything may assign to it. These were only ever assigned (in .init() and
      # friends), which threw "cannot add bindings to a locked environment" and
      # aborted the analysis during init - before any result was produced.
      .perf_timers = NULL,
      .nom_object = NULL,
      .validation_warnings = NULL,
      .validation_time = NULL,
      .analysis_times = NULL,


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
              '<div class="%s" style="margin: 10px 0; padding: 10px; border-left: 4px solid %s; background-color: rgba(138, 155, 172, 0.06); color: inherit;"><strong>%s:</strong> %s</div>',
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
          "a small p indicates evidence that the effect of one variable <b>depends on</b> the other ",
          "(effect modification). A treatment-by-biomarker interaction supports a predictive-biomarker claim only in an appropriate treatment-comparison design with pre-specified validation. HR = 1 means no modification.</p>",
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
          "<tr style='background-color: rgba(33, 33, 33, 0.07); color: inherit;'>",
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

          # A term spanning several coefficients needs a JOINT test.
          #
          # Each row above is a 1-df Wald test on one interaction coefficient.
          # For a 3-level x 2-level interaction that is two rows, and neither
          # answers "is the effect modified by this variable at all?" -- the rows
          # can read as borderline evidence (p = 0.077) while the joint 2-df test
          # over the same model is clearly null (p = 0.154). Report the joint
          # test so the overall question is answered explicitly.
          jt <- tryCatch(.interactionJointTests(cox_model), error = function(e) NULL)
          if (!is.null(jt) && nrow(jt) > 0) {
            itbl$setNote("joint", paste0(
              "Joint test of effect modification (all coefficients of the term at once), ",
              "which is the test to read when a term spans more than one coefficient: ",
              paste(sprintf("%s: chi-square = %.2f, df = %d, p = %s",
                            jt$term, jt$chisq, jt$df,
                            format.pval(jt$p, digits = 3, eps = 0.001)),
                    collapse = "; "),
              ". Individual rows are single-coefficient tests and can look more ",
              "extreme than the joint test."))
          }
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
          "<div style='background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffeaa7; border-radius: 5px; padding: 15px; margin: 10px; color: inherit;'>",
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

      .isCompetingRisk = function(state = NULL) {
        # Check if competing risk analysis is active.
        #
        # The STATUS VECTOR decides this, not the options. This used to
        # read `self$options$multievent && analysistype == "compete"`
        # alone, which is blind to the outcomeorganizer hand-off: a
        # recoded Censored/Event/Competing column arrives already 0/1/2
        # with multievent = FALSE -- the user never fills
        # dod/dooc/awd/awod, that is the whole point of the recoded
        # column. The guard was therefore FALSE and the 0/1/2 vector
        # went into an ordinary survival::Surv(), which for a max status
        # of 2 subtracts 1 and NAs anything outside 0/1: Censored became
        # NA (row silently DELETED), Event became censored, and
        # Competing became the event. If this ever reverts to testing
        # the options alone, competing-risk data is analysed backwards
        # again with no warning.
        #
        # `state` is a plot's image$state. jmvcore's .load() restores
        # results from disk without calling .run(), so a renderer can
        # execute where private$.eventRecode is still NULL; the flag
        # then has to come off the serialised state.
        state_data <- if (is.data.frame(state)) state else
          if (is.list(state)) state$cleanData else NULL
        state_outcome <- if (!is.null(state_data)) state_data[["myoutcome"]] else NULL
        state_has_competing <- is.factor(state_outcome) &&
          all(c("Censored", "Event", "Competing") %in% levels(state_outcome))
        state_flag <- if (is.list(state)) isTRUE(state$has_competing) else FALSE

        state_flag ||
            state_has_competing ||
            isTRUE(private$.eventRecode$has_competing) ||
            (isTRUE(self$options$multievent) &&
                 identical(self$options$analysistype, "compete"))
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

      # init ----
      .init = function() {
        # Initialize mutable private fields
        private$.nom_object <- NULL
        private$.perf_timers <- NULL
        private$.validation_warnings <- NULL
        private$.validation_time <- NULL
        private$.analysis_times <- NULL

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

        # The risk-group table is a substantive numeric result, not optional
        # prose; hiding it with showSummaries made the selected risk analysis
        # disappear when users disabled narrative text.
        if (self$options$calculateRiskScore)
            self$results$riskScoreTable$setVisible(TRUE)

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
                self$results$riskScoreSummary$setVisible(TRUE)
            }
        }

        # Handle showExplanations visibility
        if (self$options$showExplanations) {
            # ENHANCEMENT: Add statistical glossary panel for clinical users
            # Provides plain-language definitions of key statistical terms
            self$results$glossaryPanel$setContent(
              "<div style='padding: 15px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #007bff; border-radius: 5px; margin: 10px 0; color: inherit;'>
              <h4 style='color: #0056b3; margin-top: 0;'>Statistical Terms Glossary</h4>
              <dl style='line-height: 1.6;'>
                <dt><b>Hazard Ratio (HR)</b></dt>
                <dd style='margin-bottom: 10px;'>Relative instantaneous event rate under a proportional-hazards model. HR &gt; 1 indicates a higher fitted hazard and HR &lt; 1 a lower fitted hazard. It is not a cumulative-risk ratio or a causal effect by itself.</dd>
                <dt><b>C-index (Concordance Index)</b></dt>
                <dd style='margin-bottom: 10px;'>Measures rank discrimination among comparable patient pairs. 0.5 indicates chance ordering and 1.0 perfect ordering. There are no universal clinical cut-offs; adequacy depends on the intended use, outcome, case mix, and external validation.</dd>
                <dt><b>EPV (Events Per Variable)</b></dt>
                <dd style='margin-bottom: 10px;'>Number of events divided by estimated model coefficients. Ten EPV is a conventional diagnostic rule, not a mathematical minimum or guarantee; lower EPV increases concern about instability, small-sample bias, and optimistic performance.</dd>
                <dt><b>Proportional Hazards (PH) Assumption</b></dt>
                <dd style='margin-bottom: 10px;'>Core assumption of Cox regression that the hazard ratio stays constant over time. Tested using cox.zph; p &gt; 0.05 means no departure was detected, which is not the same as the assumption holding. If violated, consider time-varying effects or stratification.</dd>
                <dt><b>Fine-Gray Model (Competing Risks)</b></dt>
                <dd style='margin-bottom: 10px;'>Extension of Cox regression for competing risks that models subdistribution hazards. Appropriate when interested in cumulative incidence functions. Hazard ratios are not directly comparable to cause-specific Cox models.</dd>
                <dt><b>Censoring</b></dt>
                <dd style='margin-bottom: 10px;'>Observation where the event of interest has not occurred before follow-up ends. Standard analyses assume censoring is non-informative, conditional on the model and study design.</dd>
                <dt><b>Person-Time</b></dt>
                <dd style='margin-bottom: 10px;'>Sum of time each individual is observed (at risk) in the study. Used to calculate incidence rates; accounts for varying follow-up durations across participants.</dd>
              </dl>
              </div>"
            )
            self$results$glossaryPanel$setVisible(TRUE)

            # ENHANCEMENT: Add assumptions checklist panel for clinical safety
            # Lists key assumptions and provides guidance on checking them
            self$results$assumptionsPanel$setContent(
              "<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; border-radius: 5px; margin: 10px 0; color: inherit;'>
              <h4 style='color: #856404; margin-top: 0;'>Cox Model Assumptions and Caveats</h4>
              <p style='line-height: 1.6;'><b>Before interpreting results, verify these assumptions:</b></p>
              <ul style='line-height: 1.6;'>
                <li><b>Proportional Hazards:</b> Hazard ratios remain constant over time. Check using PH diagnostic test (cox.zph). If p &lt; 0.05 for any variable, consider time-varying effects or stratification.</li>
                <li><b>Independent Censoring:</b> Censoring is unrelated to the event risk. Verify through study design (e.g., administrative censoring is typically safe; loss to follow-up may be informative).</li>
                <li><b>Linear Relationships:</b> Continuous predictors have linear effects on log-hazard. Check using martingale residuals or categorize continuous variables.</li>
                <li><b>Adequate Information:</b> Review events per estimated coefficient, confidence intervals, separation, and validation. Ten EPV is a conventional warning threshold, not a hard validity boundary.</li>
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
              "<div style='padding: 15px; background-color: rgba(33, 144, 255, 0.11); border-left: 4px solid #2196F3; border-radius: 5px; margin: 10px 0; color: inherit;'>
              <h4 style='color: #1976D2; margin-top: 0;'>Understanding Multivariable Cox Regression</h4>
              <p style='line-height: 1.6;'>This analysis adjusts for multiple variables simultaneously, providing <b>conditional associations</b> for each term given the others in the fitted model.</p>
              <ul style='line-height: 1.6;'>
                <li><b>Hazard Ratio (HR) > 1:</b> Higher fitted instantaneous event rate</li>
                <li><b>Hazard Ratio (HR) < 1:</b> Lower fitted instantaneous event rate</li>
                <li><b>Hazard Ratio (HR) = 1:</b> No fitted hazard-rate association on that contrast</li>
              </ul>
              <p style='line-height: 1.6;'><i>Example:</i> HR = 2.0 means the hazard is doubled; HR = 0.5 means the hazard is halved compared to the reference group.</p>
              </div>"
            )
            self$results$multivariableCoxExplanation$setVisible(TRUE)

            # Conditional explanations - require both showExplanations AND their specific option
            if (self$options$ac) {
                self$results$adjustedSurvivalExplanation$setContent(
                  "<div style='padding: 15px; background-color: rgba(255, 169, 33, 0.14); border-left: 4px solid #ff9800; border-radius: 5px; margin: 10px 0; color: inherit;'>
                  <h4 style='color: #F57C00; margin-top: 0;'>Understanding Adjusted Survival Curves</h4>
                  <p style='line-height: 1.6;'>Adjusted curves are model-based survival or cumulative-incidence predictions under the selected standardisation.</p>
                  <p style='line-height: 1.6;'><b>Key Points:</b></p>
                  <ul style='line-height: 1.6;'>
                    <li>The standardised option averages predictions over all observed patients; the reference-profile option uses one mean/mode profile</li>
                    <li>Useful for model-based comparisons conditional on the measured covariates included in the model</li>
                    <li>Does not remove bias from unmeasured confounding or model misspecification</li>
                  </ul>
                  </div>"
                )
                self$results$adjustedSurvivalExplanation$setVisible(TRUE)
            }
            if (self$options$calculateRiskScore) {
                self$results$riskScoreExplanation$setContent(
                  "<div style='padding: 15px; background-color: rgba(153, 33, 170, 0.12); border-left: 4px solid #9c27b0; border-radius: 5px; margin: 10px 0; color: inherit;'>
                  <h4 style='color: #7B1FA2; margin-top: 0;'>Understanding Risk Score Analysis</h4>
                  <p style='line-height: 1.6;'>The displayed score is exp(centered linear predictor), a relative Cox risk score rather than an absolute event probability.</p>
                  <p style='line-height: 1.6;'><b>How It Works:</b></p>
                  <ul style='line-height: 1.6;'>
                    <li><b>Calculation:</b> Exponentiated, centered coefficient-weighted linear predictor</li>
                    <li><b>Higher scores</b> indicate a higher fitted hazard relative to the centering reference</li>
                    <li><b>Risk groups</b> are created by dividing patients into quantiles (tertiles, quartiles, etc.)</li>
                  </ul>
                  <p style='line-height: 1.6;'><b>Clinical caution:</b> These are apparent, data-derived groups. Do not use them to allocate treatment or resources without calibration, external validation, and an independently justified clinical decision rule.</p>
                  </div>"
                )
                self$results$riskScoreExplanation$setVisible(TRUE)
            }
            if (self$options$showNomogram) {
                self$results$nomogramExplanation$setContent(
                  "<div style='padding: 15px; background-color: rgba(33, 159, 43, 0.1); border-left: 4px solid #4caf50; border-radius: 5px; margin: 10px 0; color: inherit;'>
                  <h4 style='color: #388E3C; margin-top: 0;'>Understanding Nomograms</h4>
                  <p style='line-height: 1.6;'>A nomogram is a <b>graphical representation</b> of predictions from the fitted regression model.</p>
                  <p style='line-height: 1.6;'><b>How to Use:</b></p>
                  <ol style='line-height: 1.6;'>
                    <li>Find each predictor's value on its scale</li>
                    <li>Draw a line straight up to the <b>Points</b> axis to get points for that variable</li>
                    <li>Add up all points to get the <b>Total Points</b></li>
                    <li>Find the total on the <b>Total Points</b> axis</li>
                    <li>Draw a line down to read the predicted <b>survival probability</b></li>
                  </ol>
                  <p style='line-height: 1.6;'><i>Clinical caution:</i> This nomogram is derived and evaluated in the same data. It is not a point-of-care decision tool without calibration, internal validation, and external validation in the intended population.</p>
                  </div>"
                )
                self$results$nomogramExplanation$setVisible(TRUE)
            }
            if (self$options$person_time) {
                self$results$personTimeExplanation$setContent(
                  "<div style='padding: 15px; background-color: rgba(230, 33, 99, 0.12); border-left: 4px solid #e91e63; border-radius: 5px; margin: 10px 0; color: inherit;'>
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
                  "<div style='padding: 15px; background-color: rgba(255, 203, 33, 0.14); border-left: 4px solid #ffc107; border-radius: 5px; margin: 10px 0; color: inherit;'>
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
                  "<div style='padding: 15px; background-color: rgba(33, 162, 155, 0.14); border-left: 4px solid #009688; border-radius: 5px; margin: 10px 0; color: inherit;'>
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
          jmvcore::reject(.("Data contains no (complete) rows"))
        }

        # Get the data
        mydata <- self$data


        # Check if data has names
        if (is.null(names(mydata))) {
          jmvcore::reject(.("Data must have column names"))
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
        validation_results <- .validateSurvivalData(
          mydata_labelled, mytime, myoutcome,
          event_level  = self$options$outcomeLevel,
          multievent   = self$options$multievent,
          analysistype = self$options$analysistype,
          dod = self$options$dod, dooc = self$options$dooc,
          awd = self$options$awd, awod = self$options$awod)

        # Handle validation issues and warnings
        if (length(validation_results$issues) > 0) {
          issue_message <- paste0(
            "<div style='background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
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
            "<div style='background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffeaa7; padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
            "<h4 style='color: #856404; margin-top: 0;'> ", .("Data Validation Warnings"), "</h4>",
            "<ul style='margin: 5px 0; padding-left: 20px;'>",
            paste(lapply(validation_results$warnings, function(x) paste0("<li>", x, "</li>")), collapse = ""),
            "</ul>",
            "<p><strong>", .("Note:"), "</strong> ", .("Analysis will proceed, but consider these recommendations for optimal results."), "</p>",
            "</div>"
          )
          # Store warning to display later
          private$.validation_warnings <- warning_message
          private$.addHtmlMessage(
            "warning",
            .("Data validation warnings"),
            paste(unlist(validation_results$warnings), collapse = " ")
          )
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
                  jmvcore::reject(jmvcore::format(
                      .("Unsupported time type format: {format}. Supported formats are: {supported}"),
                      format = timetypedata,
                      supported = paste(names(lubridate_functions), collapse = ", ")))
              }
          } else {
              # Mixed types error
              jmvcore::reject(.("Diagnosis date and follow-up date must be in the same format (both numeric or both text)"))
          }


          if (sum(!is.na(mydata[["start"]])) == 0 ||
              sum(!is.na(mydata[["end"]])) == 0)  {
            jmvcore::reject(jmvcore::format(
              .("Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: {format}"),
              format = self$options$timetypedata
            ))
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
          
          error_msg <- jmvcore::format(
              .("Negative Survival Times Detected: {count} observation(s) have negative time values. This typically indicates:\n\u2022 Follow-up date occurs before diagnosis date\n\u2022 Incorrect date variable selection (dates reversed)\n\u2022 Data entry errors in date fields\n\nTo Fix:\n1. Verify 'Diagnosis Date' and 'Follow-up Date' are correctly assigned\n2. Check that diagnosis always precedes follow-up\n3. Review date formats and ensure consistency\n4. Examine observations with negative times for data errors"),
              count = n_negative
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


        # Delegated to the shared coder in survival_utils.R. Note the two
        # behaviour changes here: the numeric check used sum(unique(x)) == 1,
        # which accepted nonsense pairs such as {-1, 2}; and the competing-risk
        # branch pre-filled the vector with "Censored", so patients with unknown
        # vital status silently entered the Fine-Gray model as event-free.
        # .defineEventIndicator() preserves NA in both cases.
        res <- .defineEventIndicator(
            outcome      = mydata[[myoutcome_labelled]],
            outcomeLevel = self$options$outcomeLevel,
            multievent   = self$options$multievent,
            analysistype = self$options$analysistype,
            dod          = self$options$dod,
            dooc         = self$options$dooc,
            awd          = self$options$awd,
            awod         = self$options$awod,
            outcome_name = self$options$outcome
        )

        if (!is.null(res$error))
          jmvcore::reject(res$error, code = "outcome_recode")

        private$.eventRecode <- res

        # Fine-Gray needs the labelled factor; everything else wants 0/1.
        mydata[["myoutcome"]] <- if (!is.null(res$status_factor))
          res$status_factor else res$status

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

        # The stratification variable has to be carried into the analysis frame
        # too. It was omitted here, so the Cox formula asked for strata(<var>) on
        # a column that did not exist in cleanData: the fit failed and every
        # stratified output came back blank, with no error -- while the module's
        # own proportional-hazards note actively recommends stratifying.
        mystratvar_labelled <- labelled_data$mystratvar_labelled

        df_factor <- mydata %>%
          jmvcore::select(unique(
            c(
              "row_names",
              myexplanatory_labelled,
              adjexplanatory_labelled,
              mycontexpl_labelled,
              mystratvar_labelled
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

          ml_time <- 0

          # Optimism-corrected discrimination (bootstrap C-index), if requested
          private$.calculateOptimismCIndex()

          # Generate clinical interpretation summary
          # Honour the option. This ran unconditionally, so unticking
          # "Show summaries" left the Clinical Summary panel on the page.
          if (isTRUE(self$options$showSummaries))
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
            "Recommendations: (1) Check data for missing/invalid values in time and outcome variables, (2) Ensure the time origin and units are correct, (3) Verify outcome coding and the selected event level, (4) Review the number of events relative to model complexity, (5) Ensure explanatory variables have appropriate types and variation, (6) Try fewer variables if the model is unstable, or (7) check influential observations."
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
          jmvcore::reject(.("Data contains no (complete) rows"))
        }

        # Fit central Cox model once for downstream plots
        cox_model <- private$.cox_model()

        if (isTRUE(getOption("multisurvival.debug"))) {
          message("[multisurvival.debug] performSurvivalAnalysis: cox_model fitted = ", !is.null(cox_model))
        }

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

        # Model performance metrics (C-index, IPCW Brier / AUC, IBS via riskRegression)
        if (self$options$show_survmetrics) {
          private$.calculate_survmetrics()
        }

        # Covariate contribution (single-term deletion LRT / AIC)
        if (self$options$compare_models) {
          private$.compare_models()
        }

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

        # Always disclose how the outcome was recoded. A silent recode is a
        # clinical-safety hazard: the reader of a survival curve cannot otherwise
        # see which levels were collapsed into "censored", nor which estimand
        # the probability-scale outputs actually correspond to.
        if (!is.null(private$.eventRecode))
          self$results$eventRecodeInfo$setContent(
              .describeEventIndicator(private$.eventRecode, self$options$outcome))

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

          # Reconcile against the recode disclosure shown just above.
          #
          # That panel counts the OUTCOME column only, so it reports every row
          # whose outcome was readable. The model additionally drops rows with a
          # missing follow-up time or a missing covariate. A report could
          # therefore state "Event level 1: 149" directly above "140 events" with
          # nothing accounting for the 9, which reads as a contradiction.
          recode_note <- ""
          rc <- private$.eventRecode
          if (!is.null(rc)) {
            rc_total <- sum(c(rc$n_event, rc$n_censored, rc$n_missing), na.rm = TRUE)
            dropped <- rc_total - n_obs
            dropped_ev <- if (!is.null(rc$n_event)) rc$n_event - n_events else NA_integer_
            if (!is.na(dropped) && dropped > 0)
              recode_note <- paste0(" ", jmvcore::format(
                .("A further {rows} row(s){events} were excluded from the model because the follow-up time or at least one selected covariate was missing; the outcome recode shown above counts the outcome column alone."),
                rows = dropped,
                events = if (!is.na(dropped_ev) && dropped_ev > 0)
                  paste0(" ", jmvcore::format(.("({count} of them events)"), count = dropped_ev)) else ""
              ))
          }

          private$.addHtmlMessage(
            "info",
            .("Analysis complete"),
            paste0(sprintf(
              .("Analysis completed successfully using %d observations with %d events (%.1f%% event rate) over %.1f %s median follow-up."),
              n_obs, n_events, event_rate, median_followup, time_unit
            ), recode_note)
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

        # With no events the partial likelihood contains no information about
        # covariate effects, so a Cox model cannot be estimated. A small but
        # non-zero event count is not a mathematical prohibition: fit it, but
        # surface a strong warning about instability instead of enforcing the
        # old, unsupported hard threshold of ten events.
        if (n_events == 0) {
          private$.addHtmlMessage(
            "error",
            .("No events observed"),
            .("No events of interest remain after complete-case filtering, so Cox regression cannot estimate covariate effects. Check the selected event level and missing-data exclusions, or use descriptive follow-up summaries.")
          )
          return(NULL)
        }

        # STRONG WARNING: 1-19 events - Results may be unreliable
        if (n_events < 20) {
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

        # Say out loud how a few-valued continuous predictor is being modelled.
        #
        # jmvcore rejects a genuine factor in `contexpl` before .run() is
        # reached ("Argument 'contexpl' requires a numeric variable"), so there
        # is no factor to coerce. The silent path was the reverse one:
        # finalfit auto-factorised any numeric covariate with < 5 distinct
        # values (see cont_cut in .final_fit2), so an ordinal score was fitted
        # as a factor in the main table and as a linear trend everywhere else.
        # That coercion is now off; this notice states the consequence, so the
        # linear-trend assumption is a documented choice and not a silent one.
        if (length(mycontexpl) > 0) {
          .ndist <- vapply(mycontexpl, function(v) {
            x <- mydata[[v]]
            if (is.numeric(x)) length(unique(x[!is.na(x)])) else NA_integer_
          }, integer(1))
          .few <- mycontexpl[!is.na(.ndist) & .ndist < 5L]
          if (length(.few) > 0) {
            .shown <- vapply(.few, function(v) {
              lbl <- all_labels[[v]]
              if (is.null(lbl)) v else as.character(lbl)
            }, character(1))
            private$.addHtmlMessage(
              "info",
              .("Continuous predictor with few distinct values"),
              paste0(
                .("Entered as continuous:"), " ",
                paste(sprintf("%s (%d distinct values)", .shown, .ndist[.few]),
                      collapse = "; "), ". ",
                .("The hazard ratio is the effect per one-unit increase, assuming a constant step in log hazard between consecutive values. If the categories are not equally spaced, move the variable to Explanatory Variables to estimate a separate hazard ratio for each level.")
              )
            )
          }
        }

        # Build formula parts (exclude strata from covariates)
        #
        # The comment above was aspirational: strata variables were left in the
        # predictor list AND passed separately as strata_vars below, so the
        # formula became `... + treatment + strata(treatment)`. coxph then
        # returns a singular NA coefficient for the duplicated term, which in
        # turn inflates the coefficient count used for the events-per-variable
        # check and can trip a spurious "too few events" warning.
        formula_parts <- c(myexplanatory, mycontexpl)
        if (length(mystratvar) > 0)
          formula_parts <- setdiff(formula_parts, mystratvar)

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

        # Check for competing risks analysis.
        #
        # .isCompetingRisk(), not `multievent && analysistype == "compete"`: an
        # outcomeorganizer hand-off arrives already coded 0/1/2 with multievent
        # unset, so the option pair sent it down the STANDARD Cox branch below
        # with a three-level status. .definemyoutcome() hands Fine-Gray the
        # Censored/Event/Competing factor, which is exactly what finegray()
        # needs, so the hand-off belongs in this branch.
        if (private$.isCompetingRisk()) {
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

            # id = keeps a subject identifier on the expanded rows so the fit
            # below can cluster on it.
            mydata$fgid <- seq_len(nrow(mydata))
            # finegray() keeps only the variables named in the formula, so the
            # subject id has to be carried on the right-hand side for it to
            # survive into the expanded data. It is dropped again from the
            # modelling formula below, which is rebuilt from the original.
            fg_data <- survival::finegray(update(coxformula, . ~ . + fgid),
                                          data = mydata, etype = "Event", id = fgid)

            # Update formula to use Fine-Gray variables
            fg_formula <- update(coxformula, survival::Surv(fgstart, fgstop, fgstatus) ~ .)

            # Fit Cox model on expanded data with weights.
            #
            # finegray() splits one subject into several rows, so those rows are
            # not independent. Without clustering, the naive weighted-likelihood
            # variance treats them as independent observations and understates
            # the standard errors -- making the subdistribution CIs too narrow
            # and the p-values too small. survival::finegray's documentation
            # calls for a robust variance on the expanded data.
            cox_model <- survival::coxph(
              fg_formula,
              data = fg_data,
              weights = fgwt,
              cluster = fgid,
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
            is_finegray = private$.isCompetingRisk()
          )
        }

        if (private$.isCompetingRisk()) {
          private$.addHtmlMessage(
            "info",
            .("Competing-risk model"),
            .("Competing-risk mode fits a Fine-Gray subdistribution model; HRs reflect subdistribution hazards and are not directly comparable to cause-specific Cox HRs.")
          )
        }

        # Events-per-variable (EPV) diagnostic. Low EPV increases concern about
        # instability, small-sample bias, and optimistic performance, but 10 EPV
        # is a conventional warning threshold rather than a hard minimum. See
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
              .("Low events-per-variable: this Cox model fits %d coefficient(s) on %d event(s) (EPV = %.1f, below the conventional 10-EPV warning threshold). Hazard-ratio estimates and CIs may be unstable. Consider: (i) reducing covariates; (ii) penalised Cox (lassocox / adaptivelasso); (iii) bootstrap-optimism correction (survivalvalidation)."),
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

        if (!is.finite(total_time) || total_time <= 0) {
          private$.addHtmlMessage(
            "warning",
            .("Person-time unavailable"),
            .("Total observed follow-up is zero, so an incidence rate cannot be calculated. Check the time variable and its units."))
          return(invisible(NULL))
        }

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




        # Exact (Garwood) bounds: a row with almost no accrued person-time
        # genuinely cannot rule out a very high rate. Correct, but it looks
        # like a bug without a footnote saying so. Do not cap it.
        self$results$personTimeTable$setNote(
          "ci",
          .("Exact (Garwood) Poisson 95% CI. Rows with 0 events give a one-sided 97.5% upper bound; intervals with very little accrued person-time yield correspondingly wide bounds."))

        # Clear before repopulating.
        #
        # jmvcore's Table$addRow() appends unconditionally -- it has no
        # duplicate-key check -- so a table that jamovi did not clear between
        # runs ends up holding the previous run's rows AND this run's. That is
        # not hypothetical here: person-time is computed on the complete-case set
        # over ALL selected variables, so adding one continuous covariate with
        # missing values took the overall row from 134 events / 8235.5 person-time
        # to 94 / 6005.47 -- while `contexpl` was absent from this table's
        # clearWith, leaving the old rates on screen. The clearWith list is
        # corrected too, but deleteRows() is what makes the table correct
        # regardless of any future omission from that list. Six other tables in
        # this file already do this.
        self$results$personTimeTable$deleteRows()

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
        interval_tokens <- trimws(unlist(strsplit(self$options$time_intervals, ",")))
        time_intervals <- suppressWarnings(as.numeric(interval_tokens))
        max_followup <- max(mydata[["mytime"]], na.rm = TRUE)
        valid_intervals <- is.finite(time_intervals) & time_intervals > 0 &
                           time_intervals < max_followup
        if (any(!valid_intervals)) {
          private$.addHtmlMessage(
            "warning",
            .("Invalid person-time cutpoints ignored"),
            sprintf(
              .("Person-time cutpoints must be numeric, greater than zero, and below the observed maximum follow-up (%.2f %s). Invalid entries were ignored."),
              max_followup, self$options$timetypeoutput
            )
          )
        }
        time_intervals <- sort(unique(time_intervals[valid_intervals]))

        if (length(time_intervals) > 0) {
          # Create time intervals
          breaks <- c(0, time_intervals, max_followup)

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
                # The final interval's upper bound is the observed maximum
                # follow-up, a raw double, so the label read "60-134.449661066093".
                interval=paste0(.fmtTimeLabel(start_time), "-", .fmtTimeLabel(end_time)),
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
        if (private$.isCompetingRisk()) {
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
            # Refitting needs every variable the model formula names, including
            # the stratification variables. cleanData does not always carry
            # them, so the refit died with a bare "object 'stage' not found"
            # that surfaced to the clinician as a raw R error. Pull anything
            # missing from the labelled frame, matched on row_names.
            .need <- all.vars(stats::formula(cox_model))
            .miss <- setdiff(.need, names(mydata))
            if (length(.miss) > 0) {
              .src <- cleaneddata$mydata_labelled
              if (!is.null(.src) && all(.miss %in% names(.src)) &&
                  !is.null(mydata[["row_names"]]) && !is.null(.src[["row_names"]])) {
                .idx <- match(mydata[["row_names"]], .src[["row_names"]])
                for (.v in .miss) mydata[[.v]] <- .src[[.v]][.idx]
                .miss <- setdiff(.need, names(mydata))
              }
            }
            # riskRegression's Brier/AUC scoring does not reliably support a
            # stratified Cox model: predictCox rebuilds the strata from the
            # prediction data and rejects the result ("New data has a strata not
            # found in the original model") even when the fit and the prediction
            # data are the same rows. Harrell's C above is computed directly
            # from the fit and is unaffected, so it is kept. Say plainly that
            # the time-dependent metrics are unavailable rather than surfacing a
            # raw R error the reader cannot act on.
            if (length(attr(stats::terms(stats::formula(cox_model)), "term.labels")) > 0 &&
                any(grepl("^strata\\(", attr(stats::terms(stats::formula(cox_model)), "term.labels")))) {
              private$.addHtmlMessage(
                "info",
                .("Time-dependent metrics unavailable for a stratified model"),
                .("Harrell's C-index above is reported as usual. The Brier score, integrated Brier score and time-dependent AUC are not computed for stratified Cox models, because each stratum has its own baseline hazard and the scoring routine cannot form a single absolute-risk prediction across strata. To obtain these metrics, re-run without stratification, or fit the stratification variable as an ordinary covariate."))

              # Clear what a previous run left behind. Returning early does not
              # overwrite these, so the old "Performance metric error ... has new
              # levels" text and an empty Brier frame stayed on the page next to
              # the notice explaining that the metrics were not computed.
              try(self$results$survMetricsSummary$setContent(""), silent = TRUE)
              try(self$results$survMetricsSummary$setVisible(FALSE), silent = TRUE)
              try(self$results$survMetricsPlot$setVisible(FALSE), silent = TRUE)
              return(invisible(NULL))
            }

            .refit <- private$.coxRefitForScore(cox_model, mydata)
            if (inherits(.refit, "multisurvival_refit_error")) {
              private$.addHtmlMessage(
                "warning",
                .("Model performance metrics unavailable"),
                sprintf(.("Performance metrics could not be computed because the analysis dataset does not carry: %s. This usually happens when a stratification variable is not retained alongside the model covariates."),
                        paste(.refit$missing, collapse = ", ")))
              return(invisible(NULL))
            }
            cox_local <- .refit$fit
            # Score must see the SAME frame the model was fitted on, including
            # the dropped factor levels.
            mydata    <- .refit$data
            # riskRegression's response parser does not accept a
            # namespace-qualified `survival::Surv(...)` on the left-hand side --
            # it fails with "Cannot assign response type", which was swallowed
            # into a generic notice, leaving only Harrell's C in the table.
            # Its documented interface takes a bare `Surv(...)`, which
            # .asSurvivalFormula() already allow-lists.
            score_formula <- .asSurvivalFormula("Surv(mytime, myoutcome) ~ 1")

            # riskRegression refuses a model carrying NA coefficients, and its
            # message ("One or several parameters ... have no value") does not
            # say why. NA coefficients mean a term is aliased -- a covariate
            # that is constant, or collinear with another, after listwise
            # deletion. Name the offending terms instead of surfacing a generic
            # "performance metric error".
            na_coefs <- names(stats::coef(cox_local))[is.na(stats::coef(cox_local))]
            if (length(na_coefs) > 0) {
              self$results$survMetricsSummary$setContent(paste0(
                "<p><b>Performance metrics unavailable.</b> The model could not estimate a ",
                "coefficient for: <i>", paste(na_coefs, collapse = ", "), "</i>. ",
                "This happens when a covariate is constant, or is collinear with another ",
                "covariate, in the rows remaining after missing values are dropped. ",
                "Brier score, time-dependent AUC and IBS require a fully identified model - ",
                "remove or combine the affected term(s) and re-run.</p>"))
              return(invisible(NULL))
            }

            # Integrate the Brier score over a DENSE grid, not just the handful
            # of timepoints the user typed. IBS is an integral: evaluating it at
            # 3-4 points materially misstates it (on the bundled data a 4-point
            # IBS read 0.144 against 0.166 over a dense grid through the same
            # horizon). Per-timepoint rows below still report only the requested
            # timepoints.
            dense <- unique(sort(c(tps, seq(0, max(tps), length.out = 100))))
            dense <- dense[dense > 0 & dense <= max(tps)]

            # null.model = TRUE gives the Kaplan-Meier (covariate-free) reference.
            # Without it there is nothing to judge a Brier score against except a
            # fixed cut-off, and 0.25 is the non-informative benchmark only when
            # the event probability happens to be 50%.
            sc <- riskRegression::Score(
              list(Cox = cox_local),
              formula   = score_formula,
              data      = mydata,
              times     = dense,
              metrics   = c("brier", "auc"),
              summary   = "ibs",
              se.fit    = FALSE,
              conf.int  = FALSE,
              null.model = TRUE
            )
            br_all <- as.data.frame(sc$Brier$score)
            br     <- br_all[br_all$model == "Cox", , drop = FALSE]
            br_ref <- br_all[br_all$model != "Cox", , drop = FALSE]
            au <- as.data.frame(sc$AUC$score);   au <- au[au$model == "Cox", , drop = FALSE]

            for (t in tps) {
              bval <- br$Brier[br$times == t]
              if (length(bval) == 1 && !is.na(bval)) {
                rk <- rk + 1L
                tbl$addRow(rowKey = rk, values = list(
                  metric   = paste0("Brier score (t = ", t, " ", self$options$timetypeoutput, ")"),
                  value    = bval, ci_lower = NA_real_, ci_upper = NA_real_,
                  # Judged against the Kaplan-Meier reference at the same
                  # timepoint (IPA = 1 - Brier_model / Brier_null), not against a
                  # universal cut-off. IPA <= 0 means the model predicts no better
                  # than ignoring the covariates entirely.
                  interpretation = {
                    rval <- br_ref$Brier[br_ref$times == t]
                    if (length(rval) == 1 && !is.na(rval) && rval > 0) {
                      ipa <- 1 - bval / rval
                      paste0(sprintf("%+.1f%%", ipa * 100),
                             " vs Kaplan-Meier (", sprintf("%.3f", rval), ")",
                             if (ipa <= 0) " - no better than ignoring covariates" else "")
                    } else "no reference available"
                  }
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
                  interpretation = {
                    rref <- utils::tail(br_ref$IBS[!is.na(br_ref$IBS)], 1)
                    if (length(rref) == 1 && rref > 0) {
                      ipa <- 1 - ibs / rref
                      paste0(sprintf("%+.1f%%", ipa * 100),
                             " vs Kaplan-Meier (", sprintf("%.3f", rref), ")",
                             if (ipa <= 0) " - no better than ignoring covariates" else "")
                    } else "no reference available"
                  }
                ))
              }
            }
          }

          if (self$options$showSummaries) {
            self$results$survMetricsSummary$setContent(paste0(
              "<h4>Model Performance Summary</h4>",
              "<p><b>Discrimination</b> (Harrell's C = ", round(c_index, 3), "): the probability that, ",
              "for a random pair of subjects, the one predicted higher-risk experiences the event first. ",
              "0.5 is chance. <b>Time-dependent AUC</b> extends this to each timepoint.</p>",
              "<p><b>Brier score</b> is the inverse-probability-of-censoring-weighted mean squared error ",
              "between predicted survival and observed status at a timepoint (lower is better). It is ",
              "reported against the Kaplan-Meier model fitted to the same data, because a fixed cut-off ",
              "such as 0.25 is the non-informative benchmark only when the event probability is 50%. ",
              "The <b>Integrated Brier Score</b> integrates it over a dense grid across the follow-up.</p>",
              "<p style='background-color: rgba(255, 161, 33, 0.12);border-left:4px solid #e67e22;padding:8px;margin:10px 0; color: inherit;'>",
              "<b>These are apparent (in-sample) estimates.</b> The model was fitted and evaluated on the ",
              "same observations, with no bootstrap correction, cross-validation or held-out set, so every ",
              "value here is optimistic - typically substantially so with many covariates or few events. ",
              "Treat them as an upper bound on the performance this model would show in new patients ",
              "(TRIPOD, Collins et al. 2015).</p>",
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
        if (private$.isCompetingRisk(image$state)) {
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
          # Same limitation as the metrics table: no Brier curve for a
          # stratified model. The table above explains why; returning FALSE
          # here leaves the plot area empty rather than drawing a broken curve.
          .tl <- attr(stats::terms(stats::formula(cox_model)), "term.labels")
          if (length(.tl) > 0 && any(grepl("^strata\\(", .tl))) return(FALSE)

          .refit <- private$.coxRefitForScore(cox_model, mydata)
          if (inherits(.refit, "multisurvival_refit_error")) return(FALSE)
          cox_local <- .refit$fit
          mydata    <- .refit$data
          sc <- riskRegression::Score(
            list(Cox = cox_local),
            # Bare Surv(): riskRegression's response parser rejects a
            # namespace-qualified survival::Surv() with "Cannot assign response
            # type". null.model = TRUE supplies the real reference curve.
            formula = .asSurvivalFormula("Surv(mytime, myoutcome) ~ 1"),
            data = mydata, times = grid, metrics = "brier",
            se.fit = FALSE, conf.int = FALSE, null.model = TRUE
          )
          br_all <- as.data.frame(sc$Brier$score)
          br  <- br_all[br_all$model == "Cox" & !is.na(br_all$Brier), c("times", "Brier")]
          ref <- br_all[br_all$model != "Cox" & !is.na(br_all$Brier), c("times", "Brier")]
          if (nrow(br) == 0) return(FALSE)

          # The old flat line at 0.25 was labelled the "random-prediction
          # reference". That is only true when the event probability is 50%; at
          # any other prevalence it is meaningless, and it was drawn even though
          # null.model was FALSE so no reference had been computed at all. The
          # honest reference is the Kaplan-Meier (covariate-free) Brier curve,
          # which varies with time.
          g <- ggplot2::ggplot(br, ggplot2::aes(x = times, y = Brier)) +
            ggplot2::geom_line(linewidth = 1.1, colour = "#2E8B57")
          if (nrow(ref) > 0)
            g <- g + ggplot2::geom_line(data = ref, linetype = "dashed",
                                        colour = "#B22222", alpha = 0.8)
          g + ggplot2::labs(
              title = "Brier Score Over Time",
              x = paste0("Time (", self$options$timetypeoutput, ")"),
              y = "Brier score (IPCW)",
              caption = paste0("Lower is better. Solid = model; dashed = Kaplan-Meier ",
                               "(no covariates). Apparent, in-sample estimates.")
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
        if (private$.isCompetingRisk()) {
          self$results$modelContributionSummary$setContent(
            "<p>Covariate contribution (single-term deletion) is shown for standard Cox models and is not available for competing-risks / Fine-Gray analyses.</p>"
          )
          return()
        }

        mydata <- private$.cleandata()$cleanData

        tryCatch({
          full_formula <- stats::formula(full)
          full_local  <- survival::coxph(full_formula, data = mydata, x = TRUE, y = TRUE)
          term_labels <- attr(stats::terms(full_local), "term.labels")
          if (length(term_labels) < 2) {
            self$results$modelContributionSummary$setContent(
              "<p>Add at least two covariates to compare their individual contributions.</p>"
            )
            return()
          }

          full_aic <- stats::AIC(full_local)
          full_ll  <- as.numeric(stats::logLik(full_local))

          tbl <- self$results$modelContributionTable
          tbl$deleteRows()
          rk <- 0L

          # Refit each reduced model explicitly rather than calling stats::drop1().
          #
          # drop1() refits by re-evaluating the model's stored call via update(),
          # and it does so in the environment of the model's formula rather than
          # here. That environment does not hold `mydata`, so every refit failed
          # with "object 'mydata' not found" -- swallowed by the tryCatch below,
          # leaving an empty table and a blank summary with no indication that
          # anything had gone wrong. Building each reduced formula and fitting it
          # directly sidesteps the environment question entirely.
          # Respect marginality, as stats::drop1() does: a main effect that
          # appears inside a higher-order term cannot be dropped on its own.
          # Dropping `age` while keeping `sex:age` leaves a model that is not
          # nested in any meaningful way -- it came back with df = 0 and "not
          # estimable" rather than being skipped.
          tf         <- stats::terms(full_local)
          fac        <- attr(tf, "factors")

          # strata()/cluster()/frailty() are not covariates and must never enter
          # a single-term-deletion table. Dropping strata() replaces a set of
          # separate baseline hazards with one pooled baseline, which changes the
          # likelihood enormously while contributing no coefficients -- so the
          # row came out as a huge LRT (660.9) on df = 0 with p = NaN, labelled
          # "not estimable (term is aliased)". It is not aliased; it simply is
          # not the kind of term this test applies to.
          special_terms <- grepl("^(strata|cluster|frailty|tt)\\(", term_labels)
          keep_terms    <- term_labels[!special_terms]
          if (length(keep_terms) == 0) {
            self$results$modelContributionSummary$setContent(
              "<p>No covariates are available for single-term deletion; the model contains only stratification or clustering terms.</p>")
            return()
          }
          # The special terms must still be carried in every reduced formula --
          # a model stratified on treatment stays stratified when `age` is
          # dropped -- so keep them separately rather than discarding them.
          carry_terms <- term_labels[special_terms]
          order_all   <- attr(tf, "order")
          names(order_all) <- term_labels
          term_labels <- keep_terms

          droppable  <- term_labels
          if (!is.null(fac) && ncol(fac) > 0) {
            # Subset by NAME so the marginality check below cannot index a
            # column belonging to a term that is no longer in term_labels.
            fac       <- fac[, colnames(fac) %in% term_labels, drop = FALSE]
            order_of  <- order_all[term_labels]
            droppable <- term_labels[vapply(seq_along(term_labels), function(i) {
              # A term is droppable if no higher-order term contains all of its
              # variables.
              vars_i <- rownames(fac)[fac[, i] > 0]
              !any(vapply(seq_along(term_labels), function(j) {
                if (j == i || order_of[j] <= order_of[i]) return(FALSE)
                all(vars_i %in% rownames(fac)[fac[, j] > 0])
              }, logical(1)))
            }, logical(1))]
          }

          if (length(droppable) == 0) {
            self$results$modelContributionSummary$setContent(
              "<p>No term can be dropped while preserving model hierarchy.</p>")
            return()
          }

          skipped <- setdiff(term_labels, droppable)

          for (term in droppable) {
            # carry_terms keeps strata()/cluster() in every reduced model, so
            # each LRT compares like with like: the same stratification, one
            # covariate fewer. Omitting them would compare a stratified model
            # against an unstratified one and attribute the whole difference in
            # baseline hazard to the dropped covariate.
            reduced_terms <- c(setdiff(term_labels, term), carry_terms)
            rhs <- if (length(reduced_terms)) paste(reduced_terms, collapse = " + ") else "1"
            lhs <- deparse(full_formula[[2]])

            fit_red <- try(survival::coxph(
              .asSurvivalFormula(paste(lhs, "~", rhs)), data = mydata), silent = TRUE)
            if (inherits(fit_red, "try-error")) next

            red_ll  <- as.numeric(stats::logLik(fit_red))
            lrt     <- 2 * (full_ll - red_ll)
            df_diff <- length(stats::coef(full_local)) - length(stats::coef(fit_red))
            pval    <- if (df_diff > 0) stats::pchisq(lrt, df_diff, lower.tail = FALSE) else NA_real_

            rk <- rk + 1L
            tbl$addRow(rowKey = rk, values = list(
              term   = term,
              df     = df_diff,
              aic    = stats::AIC(fit_red),
              lrt    = lrt,
              pvalue = pval,
              interpretation = if (isTRUE(pval < 0.05)) "Significant contribution to fit"
                               else if (is.na(pval)) "Not estimable (term is aliased)"
                               else "No significant contribution"
            ))
          }

          if (self$options$showSummaries) {
            self$results$modelContributionSummary$setContent(paste0(
              "<p>Each row is a likelihood-ratio test comparing the full model against the model with that ",
              "single covariate removed (all others retained). A small p-value indicates the covariate ",
              "significantly improves fit. An <b>AIC if dropped</b> below the full-model AIC (",
              round(full_aic, 1), ") indicates the covariate could be removed without penalising fit.</p>",
              if (length(skipped))
                paste0("<p>Not tested, because each appears inside a higher-order term and ",
                       "dropping it alone would break model hierarchy: <i>",
                       paste(skipped, collapse = ", "), "</i>. Remove the interaction first ",
                       "if you want to test the main effect.</p>") else ""
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

        # Set datadist globally; restore on exit so rms datadist state does not
        # leak into the user's session and affect later rms-based analyses.
        old_datadist <- options(datadist = dd)
        on.exit(options(old_datadist), add = TRUE)

        # Get baseline Cox model (to check for Fine-Gray)
        cox_model <- private$.cox_model()

        # A stratified model has no single nomogram.
        #
        # rms::cph needs rms's own strat(), not survival::strata(), so the fit
        # failed outright -- but even if it fitted, a nomogram converts a total
        # point score into ONE absolute risk, and a stratified model has a
        # different baseline hazard per stratum. There is no single risk scale
        # to print. The previous code silently took the FIRST stratum's baseline
        # survival and labelled it as the model's, which would have given every
        # patient outside that stratum a wrong absolute risk. Refusing is the
        # safe behaviour; the summary panel explains it.
        if (isTRUE(self$options$use_stratify) && length(strata_vars) > 0) {
          private$.addHtmlMessage(
            "info",
            .("Nomogram not available for a stratified model"),
            sprintf(.("The model is stratified by %s. A nomogram maps a total point score onto a single absolute-risk scale, but a stratified model has a separate baseline hazard for each stratum, so no single scale applies to all patients. Remove the stratification, or enter these variables as ordinary covariates, to obtain a nomogram."),
                    paste(strata_vars, collapse = ", ")))

          # Withdraw the whole nomogram section, not just the plot.
          #
          # Returning early leaves every sibling panel showing whatever the
          # previous run put there: an empty plot frame, a "How to read the
          # nomogram" walkthrough, and a scoring guide -- all describing a
          # nomogram that was deliberately not produced. An explanation of how
          # to read something that is not on the page is worse than silence.
          for (nm in c("nomogramHeading", "plot_nomogram", "nomogram_display",
                       "nomogramSummaryHeading", "nomogramSummary",
                       "nomogramExplanation")) {
            it <- try(self$results[[nm]], silent = TRUE)
            if (!inherits(it, "try-error") && !is.null(it)) {
              try(it$setVisible(FALSE), silent = TRUE)
              try(it$setContent(""), silent = TRUE)
            }
          }
          return(FALSE)
        }

        is_finegray <- !is.null(cox_model$weights) && private$.isCompetingRisk()

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
        pred_tokens <- trimws(unlist(strsplit(self$options$cutp, ",")))
        pred_times <- suppressWarnings(as.numeric(pred_tokens))
        max_followup <- max(mydata$mytime, na.rm = TRUE)
        valid_pred <- is.finite(pred_times) & pred_times > 0 & pred_times <= max_followup
        if (any(!valid_pred)) {
          private$.addHtmlMessage(
            "warning",
            .("Invalid nomogram timepoints ignored"),
            sprintf(
              .("Nomogram timepoints must be numeric, greater than zero, and no later than the observed maximum follow-up (%.2f %s). Invalid entries were ignored."),
              max_followup, self$options$timetypeoutput))
        }
        pred_times <- sort(unique(pred_times[valid_pred]))
        if (length(pred_times) == 0) {
          pred_times <- stats::median(mydata$mytime[mydata$mytime > 0], na.rm = TRUE)
          private$.addHtmlMessage(
            "info",
            .("Nomogram timepoint selected from follow-up"),
            sprintf(.("No requested timepoint was estimable; the nomogram uses the median observed follow-up (%.2f %s)."),
                    pred_times, self$options$timetypeoutput))
        }

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
            background-color: rgba(255, 255, 255, 0.06); color: inherit;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            border-radius: 8px;
        }
        .tech-details {
            font-family: "Roboto Mono", monospace;
            background-color: rgba(138, 155, 172, 0.06); color: inherit;
            padding: 15px;
            border-radius: 4px;
            margin: 15px 0;
            color: inherit;
        }
        .instructions {
            background-color: rgba(33, 159, 43, 0.1); color: inherit;
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
            background-color: rgba(138, 155, 172, 0.06); color: inherit;
            border-left: 4px solid #2196f3;
            border-radius: 4px;
        }
        .section-title {
            font-size: 1.2em;
            font-weight: 600;
            color: inherit;
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
            background-color: rgba(255, 169, 33, 0.14); color: inherit;
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
            background-color: rgba(255, 169, 33, 0.14); color: inherit;
            font-weight: 600;
        }
        .notes {
            background-color: rgba(255, 237, 33, 0.11); color: inherit;
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

        is_cr <- private$.isCompetingRisk()
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
      # Refit a Cox model so that riskRegression can score it.
      #
      # riskRegression re-evaluates the model's terms when it predicts, and it
      # does so in the environment of the model's FORMULA. .asSurvivalFormula()
      # leaves that environment pointing at the frame it was built in, which no
      # longer holds the analysis data -- so a model containing strata(stage)
      # failed with a bare "object 'stage' not found" that surfaced to the
      # clinician as a raw R error. Binding the data into the formula's
      # environment makes every later lookup resolve. Same class of defect as
      # the one that used to stop cox.zph re-finding its data.
      .coxRefitForScore = function(cox_model, mydata) {
        fml <- stats::formula(cox_model)
        need <- all.vars(fml)
        miss <- setdiff(need, names(mydata))
        if (length(miss) > 0) return(structure(list(missing = miss),
                                               class = "multisurvival_refit_error"))
        # Drop factor levels that no longer occur.
        #
        # Rows are removed upstream (listwise deletion on the covariates), but
        # the factor columns keep their original level sets. coxph silently
        # ignores an empty stratum while riskRegression's predictCox compares
        # the level sets and rejects the mismatch with "New data has a strata
        # not found in the original model". Dropping the empty levels first
        # makes the fitted model and the prediction data agree.
        for (v in intersect(need, names(mydata)))
          if (is.factor(mydata[[v]])) mydata[[v]] <- droplevels(mydata[[v]])

        environment(fml) <- list2env(as.list(mydata), parent = environment(fml))
        list(fit = survival::coxph(fml, data = mydata, x = TRUE, y = TRUE),
             data = mydata)
      }
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
            structure(list(error = e$message), class = "multisurvival_ph_error")
          }
        )




        # The cox.zph result is rendered as a proper jamovi table rather than a
        # console dump. The Preformatted element is kept for the failure case
        # only, and hidden when the diagnostics computed successfully.
        phTable <- self$results$cox_phTable
        phTable$deleteRows()

        if (inherits(zph, "multisurvival_ph_error")) {
          self$results$cox_ph$setVisible(TRUE)
          self$results$cox_ph$setContent(paste0(
            "Unable to compute proportional hazards diagnostics (cox.zph):\n",
            zph$error
          ))
        } else {
          zph_table <- zph$table
          self$results$cox_ph$setVisible(FALSE)
          self$results$cox_ph$setContent("")

          if (!is.null(zph_table) && nrow(zph_table) > 0) {
            cn <- colnames(zph_table)
            col_chisq <- if ("chisq" %in% cn) "chisq" else cn[1]
            col_df    <- if ("df" %in% cn) "df" else cn[min(2, length(cn))]
            col_p     <- if ("p" %in% cn) "p" else cn[length(cn)]
            terms <- rownames(zph_table)

            for (i in seq_len(nrow(zph_table))) {
              phTable$addRow(rowKey = i, values = list(
                term  = terms[i],
                chisq = unname(zph_table[i, col_chisq]),
                df    = as.integer(round(unname(zph_table[i, col_df]))),
                p     = unname(zph_table[i, col_p])
              ))
            }

            violating <- setdiff(terms[which(zph_table[, col_p] < 0.05)], "GLOBAL")
            if (length(violating) > 0) {
              phTable$setNote("ph", paste0(
                "The proportional hazards assumption appears to be violated for: ",
                paste(violating, collapse = ", "),
                ". Consider using these as stratification variables instead of covariates.\n\n",
                "A non-significant test is <i>no evidence of a violation</i>, which is not the same as confirming the assumption holds."))
            } else {
              phTable$setNote("ph", paste0(
                "A non-significant test is <i>no evidence of a violation</i>, which is not the same as ",
                "confirming proportional hazards holds. GLOBAL is the joint test across all terms."))
            }
          }
        }






        # Always set state so the renderer can show a diagnostic message if needed
        # (returning FALSE from an Image render function yields a blank image in jamovi).
        image8 <- self$results$plot8
        image8$setState(zph)

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

        # Competing risks: the shared model is Fine-Gray, so its estimates are
        # SUBdistribution hazard ratios. Drawing them on an axis labelled
        # "HR, 95% CI" beside a report whose main table is suppressed for exactly
        # this reason (see .final_fit2) would reintroduce the two-estimands-in-one
        # -report problem from a different direction.
        if (private$.isCompetingRisk(plotData)) {
          grid::grid.newpage()
          grid::grid.text(
            paste0("Hazard-ratio forest plot is not shown for competing risks.\n",
                   "The model is Fine-Gray, whose estimates are subdistribution\n",
                   "hazard ratios and must not be read as ordinary hazard ratios."),
            x = 0.05, y = 0.95, just = c("left", "top"),
            gp = grid::gpar(fontsize = 11))
          return(TRUE)
        }

        # Build the descriptive column ourselves so that cont_cut = 0 applies.
        #
        # hr_plot() has no cont_cut argument and does NOT forward its `...` to
        # summary_factorlist -- it calls summary_factorlist(.data, dependent,
        # explanatory, fit_id = TRUE) with finalfit's default cont_cut = 5, which
        # silently splits any numeric predictor having fewer than 5 distinct
        # values into factor levels. The two Cox tables were pinned to
        # cont_cut = 0 for precisely this reason, so leaving the plot on the
        # default let it describe a variable as categorical while both tables
        # described it as continuous. Falls back to hr_plot's own default if
        # this fails, rather than losing the plot entirely.
        hr_factorlist <- tryCatch(
          finalfit::summary_factorlist(mydata, myformula, formula2,
                                       cont_cut = 0, fit_id = TRUE),
          error = function(e) NULL)

        plot <- tryCatch({
          finalfit::hr_plot(
            .data = mydata,
            dependent = myformula,
            explanatory = formula2,
            # Reuse the ALREADY-FITTED model. Left NULL, hr_plot runs
            # `coxphmulti(.data, dependent, explanatory)` internally -- a third
            # fit of the same data, on top of .cox_model() and the finalfit
            # table. That refit is built from raw columns, so it silently loses
            # stratification: a stratified analysis drew its forest plot from an
            # UNSTRATIFIED model, which is the defect that opened this whole
            # review (MS-03), reappearing in the plot layer.
            coxfit = cox_model,
            factorlist = hr_factorlist,
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
        is_finegray <- !is.null(cox_model$weights) && private$.isCompetingRisk()

        if (is_finegray) {
          grid::grid.newpage()
          grid::grid.text(
            paste0("Forest plot is not shown for competing risks.\n",
                   "The fitted effects are Fine-Gray subdistribution hazard ratios;\n",
                   "use the adjusted cumulative-incidence output for probability-scale interpretation."),
            x = 0.05, y = 0.95, just = c("left", "top"),
            gp = grid::gpar(fontsize = 11))
          return(TRUE)
        }
        
        plot3 <- tryCatch({
          survminer::ggforest(model = cox_model, data = mydata)
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

        if (is.null(zph_state)) {
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
          grid::grid.newpage()
          grid::grid.text(
            paste0("Unable to compute PH diagnostics (cox.zph): ", zph_state$error),
            x = 0.05, y = 0.95, just = c("left", "top"),
            gp = grid::gpar(fontsize = 11)
          )
          return(TRUE)
        }

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

        # Kaplan-Meier is not an absolute-risk estimator in the presence of a
        # competing terminal event. Direct users to the Fine-Gray/Aalen-Johansen
        # outputs instead of letting a standard survival plot silently censor
        # competing events.
        if (private$.isCompetingRisk(imageKM$state)) {
          grid::grid.newpage()
          grid::grid.text(
            paste0("Kaplan-Meier is not shown for competing-risk outcomes.\n",
                   "Use the adjusted cumulative-incidence or risk-group cumulative-incidence plot;\n",
                   "those account for competing events."),
            x = 0.05, y = 0.95, just = c("left", "top"),
            gp = grid::gpar(fontsize = 11))
          return(TRUE)
        }

        # Check conditions and show message if not met
        if (length(self$options$explanatory) > 2) {
          text_warning <- "Kaplan-Meier plot accepts one or two categorical explanatory variables.\nYou have selected more than two variables."
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






        if (length(self$options$contexpl) > 0) {
          text_warning <- "Kaplan-Meier plot cannot be created with continuous explanatory variables. Please select only categorical variables."
          grid::grid.newpage()
          grid::grid.text(text_warning, 0.5, 0.5)
          return(TRUE)
        }

        if (length(self$options$explanatory) < 1) {
          text_warning <- "Please select one or two categorical explanatory variables to create the Kaplan-Meier plot."
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
        #
        # In competing-risks mode cox_model is a Fine-Gray fit on the EXPANDED
        # data (finegray() splits one subject into several rows), so predict()
        # returns one value per expanded row -- more than nrow(mydata). Assigning
        # that back aborted the entire analysis with a replacement-length error,
        # for the entirely reasonable combination of competing risks plus a risk
        # score. Recompute the linear predictor on the unexpanded frame instead.
        risk_scores <- predict(cox_model, type = "risk")

        if (length(risk_scores) != nrow(mydata)) {
            risk_scores <- tryCatch(
                predict(cox_model, newdata = mydata, type = "risk"),
                error = function(e) NULL)
        }

        if (is.null(risk_scores) || length(risk_scores) != nrow(mydata)) {
            private$.addHtmlMessage(
                "warning",
                "Risk score unavailable for this model",
                paste0("A per-patient risk score could not be produced for this fit ",
                       "(the competing-risks model is fitted on an expanded dataset with ",
                       "more rows than patients). All other results are unaffected."))
            return(invisible(NULL))
        }

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


        # If no grouping could be formed at any size, `result` is NULL -- and
        # `mydata$risk_group <- NULL` DELETES the column rather than creating an
        # empty one. Everything below then ran against a missing column, and
        # tapply(event_indicator, NULL, ...) aborted the ENTIRE analysis with
        # "arguments must have same length". This is not rare: a single two-level
        # predictor gives the linear predictor only two distinct values, so the
        # quantile break points collapse and every group size fails.
        if (is.null(result)) {
            private$.addHtmlMessage(
                "warning",
                "Risk groups could not be formed",
                paste0("The risk score takes too few distinct values to be split into ",
                       "groups -- this happens when the model contains only categorical ",
                       "predictors with few levels. The risk score itself is unaffected, ",
                       "and all other results below are unchanged. Add a continuous ",
                       "predictor if you want risk groups."))
            return(invisible(NULL))
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
        # Harrell's concordance is not a competing-risks statistic.
        #
        # Under competing risks cox_model is the Fine-Gray fit, whose rows are
        # finegray()-expanded pseudo-observations rather than patients, so
        # concordance() ranks pseudo-rows and reports the result as a patient
        # discrimination index. Report nothing rather than a number that cannot
        # be interpreted; a cause- and horizon-specific measure would be needed.
        .cr_mode <- private$.isCompetingRisk()

        c_index <- if (.cr_mode) NA_real_ else
                     survival::concordance(cox_model)$concordance

        c_index_formatted <- if (is.na(c_index))
          .("not reported for competing risks") else sprintf("%.3f", c_index)

        # Create dynamic group summary text
        group_summary <- character()
        for(i in seq_len(nrow(risk_summary))) {
          group_summary[i] <- glue::glue("{risk_summary$group[i]}: {risk_summary$n_patients[i]} ({base::format(risk_summary$percent[i], digits=1, nsmall=1)}%)")

        }
        group_text <- paste(group_summary, collapse = "<br>")

        metrics_html <- glue::glue(
          "
<br>
<b>Risk Score Model Performance:</b><br>
Harrell's C-index (apparent, in-sample): {c_index_formatted}<br>
<i>{if (is.na(c_index)) 'Harrell&apos;s C assumes a single event type. Under competing risks it would be computed from expanded pseudo-observations rather than patients, so it is not reported here.' else 'This apparent concordance is optimistic; see the C-index validation table for an optimism-corrected estimate.'}</i><br>
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

        score_description <- if (.cr_mode) {
          paste(
            "The displayed score is the Fine-Gray relative subdistribution-hazard score,",
            "exp(centered linear predictor), calculated from all model coefficients.",
            "It is a ranking score, not an absolute event probability; a higher score",
            "indicates a higher fitted subdistribution hazard for the event of interest."
          )
        } else {
          paste(
            "The displayed score is the Cox relative-risk score, exp(centered linear predictor),",
            "calculated from all model coefficients. It is a ranking score, not an absolute",
            "event probability; a higher score indicates a higher fitted hazard."
          )
        }

        message_risk_score_analysis <- glue::glue(
"<b>Risk Scores Were Calculated As Follows:</b><br>
{score_description}<br>
<br>
Patients were then divided at empirical quantile cutpoints into {as.character(length(levels(mydata$risk_group)))} groups based on these scores (ties can make group sizes unequal):
 <br>
- Scores below the {percentile_text}.<br>
<br>
{if (is.na(c_index)) 'Discrimination is not summarised by Harrell\\'s C here: it assumes a single event type, and under competing risks it would rank expanded pseudo-observations rather than patients.' else paste0(\"The apparent Harrell's C-index is \", c_index_formatted, \". It measures rank discrimination in these same data and may be optimistic; 0.5 indicates chance ordering and 1.0 perfect ordering.\")}
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
              "<div style='background-color: rgba(153, 33, 170, 0.12); padding: 15px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #9c27b0; color: inherit;'>",
              "<p style='margin: 0; line-height: 1.8;'>",
              "Risk stratification identified <b>", nrow(risk_summary), " distinct risk groups</b> from the Cox model. ",
              "The <b>", highest_risk_group, "</b> group showed the highest median risk score (",
              sprintf("%.2f", highest_median_score), ") with <b>", highest_events, " events</b> observed, ",
              "while the <b>", lowest_risk_group, "</b> group had <b>", lowest_events, " events</b>.",
              "<br><br>",
              "The risk scores show a <b>", sprintf("%.1f", fold_diff), "-fold difference</b> between highest and lowest risk groups. ",
              if (is.na(c_index))
                "Harrell's C-index is not reported for this competing-risk fit."
              else paste0(
                "The apparent C-index is <b>", sprintf("%.3f", c_index),
                "</b>. It measures rank discrimination in these same data; there are no universal ",
                "clinical cut-offs, and external performance may be lower."),
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

        # Competing risks: plot cumulative incidence, not 1 - Kaplan-Meier.
        #
        # `.eventIndicator()` above maps the 0/1/2 outcome to a binary flag, so
        # a competing event became a CENSORING. Kaplan-Meier then treats those
        # patients as though they remained at risk of the event of interest,
        # which they no longer are, and the resulting curve overstates
        # event-free probability -- the more competing events, the larger the
        # overstatement. survfit() on the multi-state factor gives the
        # Aalen-Johansen cumulative incidence, which accounts for them.
        is_cr <- is.factor(riskData$myoutcome) &&
                 "Competing" %in% levels(riskData$myoutcome)

        if (is_cr) {
          cr_df <- data.frame(time = riskData$mytime,
                              st   = riskData$myoutcome,
                              group = riskData$risk_group)
          cr_df <- cr_df[!is.na(cr_df$time) & !is.na(cr_df$st) & !is.na(cr_df$group), , drop = FALSE]
          cr_df$st <- droplevels(cr_df$st)

          aj <- try(survival::survfit(survival::Surv(time, st) ~ group, data = cr_df),
                    silent = TRUE)
          if (inherits(aj, "try-error")) return(FALSE)

          # pstate carries one column per state; take the event-of-interest one.
          st_names <- colnames(aj$pstate)
          ev_col <- if ("Event" %in% st_names) which(st_names == "Event")[1] else 2L
          grp <- rep(names(aj$strata), aj$strata)
          cif <- data.frame(time  = aj$time,
                            cif   = aj$pstate[, ev_col],
                            group = sub("^group=", "", grp),
                            stringsAsFactors = FALSE)

          p <- ggplot2::ggplot(cif, ggplot2::aes(x = time, y = cif, colour = group)) +
            ggplot2::geom_step(linewidth = 0.8, na.rm = TRUE) +
            ggplot2::scale_y_continuous(limits = c(0, 1)) +
            ggplot2::labs(
              x = paste0("Time (", self$options$timetypeoutput, ")"),
              y = .("Cumulative incidence"),
              colour = .("Risk group"),
              title = .("Cumulative Incidence by Risk Group"),
              subtitle = .("Aalen-Johansen estimator"),
              caption = .("Competing events are accounted for; this is not 1 - Kaplan-Meier.")) +
            ggplot2::theme_bw() +
            ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 8))

          if (!is.null(self$options$byplot) && is.finite(self$options$byplot) &&
              self$options$byplot > 0)
            p <- p + ggplot2::scale_x_continuous(
              breaks = seq(0, max(cif_df$time, na.rm = TRUE), by = self$options$byplot))

          print(p)
          return(TRUE)
        }

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
          surv.median.line = self$options$medianline,




          title = "Survival by Risk Group",
          subtitle = "Groups defined by empirical quantiles of the Cox relative-risk score",
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

        # Varying a column that is absent from the fitted formula produces
        # identical predictions while looking like an adjusted group contrast.
        # Require the requested variable to enter the model either as a regular
        # categorical covariate or as a stratification variable.
        model_vars <- unique(c(cleaneddata$myexplanatory_labelled,
                               cleaneddata$mystratvar_labelled))
        if (!(adj_var %in% model_vars)) {
          private$.addHtmlMessage(
            "warning",
            .("Adjustment variable is not in the model"),
            .("The variable selected for adjusted curves must also be selected under Explanatory Variables or Stratification Variables. No curve was produced because changing a variable absent from the fitted model cannot change its predictions.")
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

        if (private$.isCompetingRisk()) {
          private$.addHtmlMessage(
            "info",
            .("Adjusted curves use Fine-Gray"),
            .("Adjusted curves are based on the Fine-Gray subdistribution model and display cumulative incidence of the event of interest, not cause-specific survival.")
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

        # State the estimand before anything is drawn. Rendered here rather than
        # inside the ac_summary block because it explains the CURVE as much as
        # the tables, and the curve is shown even when the tables are not.
        private$.renderAdjustedEstimandPanel(data, adj_var, self$options$ac_method)

        # Numeric adjusted-survival tables (opt-in via ac_summary): adjusted
        # survival at the cutpoint timepoints, adjusted median survival, and the
        # adjusted Cox hazard-ratio table. Each backend populates its own result
        # slots. The adjusted survival curve itself is drawn by .plot_adj().
        if (self$options$ac_summary) {
          private$.checkpoint()
          # One estimator for both tables (and, in .plot_adj, for the curve).
          # Computed once here because the g-computation branch predicts every
          # patient under every level and is the expensive part of this run.
          curves <- private$.adjustedCurveData(cox_model, data, adj_var,
                                               self$options$ac_method)
          private$.adjustedSurvTable(cleaneddata, cox_model, curves)
          private$.adjustedMedianSurv(cleaneddata, cox_model, curves)
          private$.adjustedCox(cleaneddata, cox_model)
        }

        return(invisible(NULL))
    }

      ,
    ## Shared adjusted-curve estimator ----
    # CR-3. `ac_method` used to be read in exactly ONE place: the `method=`
    # argument of survminer::ggadjustedcurves() in .plot_adj(). The adjusted
    # survival table, the adjusted median table and both narratives ignored it
    # and built their own prediction instead -- survfit() on a single mean/mode
    # covariate profile per level. Switching between "average" and "conditional"
    # therefore redrew the plot while leaving the tables byte-identical, so one
    # report could show a curve and a table answering two different questions.
    # Every adjusted output now comes from this one function; plot and tables
    # cannot disagree by construction.
    #
    # Naming: survminer's option names do not mean what a survival analyst
    # expects. Its "conditional" is the g-computation curve (each patient set to
    # every level in turn, then averaged) and its "average" averages patients
    # only within their OWN observed level. This module's UI has always read
    # "Average" / "Conditional Mean", so the names below follow the usual
    # marginal-versus-conditional distinction instead: `average` = standardised
    # over the observed patients, `conditional` = one curve at the mean/mode
    # covariate profile. The estimand is printed under every table and on the
    # plot, so the reader never has to infer which question was answered.
    #
    # Returns a tidy data frame (time, surv, lower, upper, group) or NULL. NULL
    # means the method was refused and a notice has already been emitted -- it
    # must never be turned into a fallback to another method.
    .adjustedCurveData = function(cox_model, mydata, adj_var, method) {
      if (is.null(cox_model) || is.null(mydata) || is.null(adj_var)) return(NULL)
      if (!(adj_var %in% names(mydata)) || nrow(mydata) == 0) return(NULL)

      # sort(unique(x)) keeps a factor a factor with its original level set, so
      # assigning one element back into the column below cannot drop levels and
      # cannot turn the column into the NA-producing character/factor mismatch
      # that the old table code risked.
      lv <- sort(unique(mydata[[adj_var]]))

      # Standardised (g-computation) curve for one target population: predict
      # every row of `nd` and average the predicted survival across rows. This
      # is the same shape as the Fine-Gray CIF branch of .plot_adj().
      standardised <- function(nd, label) {
        sf <- survival::survfit(cox_model, newdata = nd)
        cm <- private$.survfitCurveMatrix(sf)
        data.frame(time  = c(0, cm$time),
                   surv  = c(1, rowMeans(cm$surv, na.rm = TRUE)),
                   lower = NA_real_,
                   upper = NA_real_,
                   group = label,
                   stringsAsFactors = FALSE)
      }

      out <- tryCatch({
        if (identical(method, "single")) {
          # survminer's "single": the cohort's own expected survival with every
          # patient at their observed covariates; the adjustment variable is not
          # varied, so there is one curve. Identical to survexp(~ 1, ratetable).
          standardised(mydata, .("Overall"))

        } else if (identical(method, "average")) {
          do.call(rbind, lapply(seq_along(lv), function(k) {
            nd <- mydata
            nd[[adj_var]] <- lv[k]
            standardised(nd, as.character(lv[k]))
          }))

        } else if (identical(method, "conditional")) {
          ref <- private$.adjustedReferenceProfile(mydata, adj_var)
          do.call(rbind, lapply(seq_along(lv), function(k) {
            nd <- ref
            nd[[adj_var]] <- lv[k]
            sf <- survival::survfit(cox_model, newdata = nd)
            cm <- private$.survfitCurveMatrix(sf)
            col <- function(x) if (is.null(x)) rep(NA_real_, length(cm$time)) else as.numeric(x[, 1])
            data.frame(time  = c(0, cm$time),
                       surv  = c(1, as.numeric(cm$surv[, 1])),
                       lower = c(1, col(cm$lower)),
                       upper = c(1, col(cm$upper)),
                       group = as.character(lv[k]),
                       stringsAsFactors = FALSE)
          }))

        } else if (identical(method, "marginal")) {
          # Refuse WITHOUT calling survminer.
          #
          # Inverse-probability weighting is survminer's estimator, and its
          # implementation builds a propensity glm by string surgery on the Cox
          # formula. When the adjustment variable is also a model covariate --
          # the usual case here -- the response lands on the right-hand side and
          # the weighted fit is unusable. Calling it anyway did not merely fail:
          # on real data it ran without returning, so the plot renderer never
          # completed and jamovi span on a loading animation forever. The table
          # path had already refused, so the report showed a refusal notice
          # beside a plot that never stopped loading.
          #
          # There is nothing to learn from attempting it, so do not attempt it.
          # Writing our own IPTW estimator is a separate piece of work (weight
          # truncation, multi-level treatment, robust CIs); a subtly wrong one
          # would be worse than none.
          NULL
        } else {
          NULL
        }
      }, error = function(e) e)

      if (inherits(out, "error") || !is.data.frame(out) || nrow(out) == 0) {
        # Refuse explicitly. The old .plot_adj() silently retried "marginal" as
        # "average" and printed the result under the requested name; that kind
        # of silent substitution is how this whole defect stayed invisible.
        detail <- if (identical(method, "marginal"))
          .("The marginal (inverse-probability-weighted) curve is computed by survminer, whose implementation fails whenever the adjustment variable is also a covariate of the Cox model - which is the usual case here.")
        else
          .("The model could not be evaluated under this adjustment method.")
        private$.addHtmlMessage(
          "warning",
          .("Adjusted curves unavailable"),
          paste(jmvcore::format(.("Adjustment method: {method}."), method = method), detail,
                .("No adjusted curve or table is shown; nothing was substituted for it. Choose Average (standardised over the observed patients) or Conditional Mean instead."))
        )
        return(NULL)
      }

      out[order(out$group, out$time), , drop = FALSE]
    }

      ,
    # survfit() returns two different shapes and averaging the wrong one is
    # silent nonsense. Unstratified: $surv is a times-by-subjects matrix on one
    # common grid. Stratified (use_stratify): the per-subject curves are
    # concatenated, each on its own stratum's time grid, with $strata holding the
    # block lengths -- rowMeans() on that would average unrelated time points.
    # Re-evaluating on the union grid gives one matrix in both cases.
    .survfitCurveMatrix = function(sf) {
      as_matrix <- function(x, nr) {
        if (is.null(x)) return(NULL)
        if (is.null(dim(x))) matrix(x, nrow = nr) else x
      }
      if (is.null(sf$strata)) {
        return(list(time  = sf$time,
                    surv  = as_matrix(sf$surv,  length(sf$time)),
                    lower = as_matrix(sf$lower, length(sf$time)),
                    upper = as_matrix(sf$upper, length(sf$time))))
      }
      grid <- sort(unique(sf$time))
      sm <- summary(sf, times = grid, extend = TRUE)
      list(time  = grid,
           surv  = as_matrix(sm$surv,  length(grid)),
           lower = as_matrix(sm$lower, length(grid)),
           upper = as_matrix(sm$upper, length(grid)))
    }

      ,
    # The mean/mode covariate profile the adjusted tables have always used; it is
    # now the `conditional` branch only. Factor levels are preserved on purpose:
    # assigning the bare mode string into a factor column yields NA and survfit()
    # then predicts for a patient who does not exist.
    .adjustedReferenceProfile = function(mydata, adj_var) {
      ref <- mydata[1, , drop = FALSE]
      for (var in names(mydata)) {
        if (var %in% c("mytime", "myoutcome", "row_names", adj_var)) next
        v <- mydata[[var]]
        if (is.numeric(v)) {
          ref[[var]] <- mean(v, na.rm = TRUE)
        } else if (is.factor(v)) {
          ref[[var]] <- factor(names(which.max(table(v))), levels = levels(v))
        }
      }
      ref
    }

      ,
    # Read the step function the plot draws: the estimate in force at time t is
    # the last one at or before t. Tables read the curve through this, so a
    # tabulated number is exactly the height of the plotted curve at that time.
    # Beyond the last observed time nothing is estimable, so the row is dropped
    # rather than carried forward (the plotted curve stops there too).
    .adjustedCurveAt = function(g, t) {
      idx <- which(g$time <= t)
      if (length(idx) == 0 || t > max(g$time, na.rm = TRUE)) return(NULL)
      g[max(idx), , drop = FALSE]
    }

      ,
    # One sentence naming the estimand, attached to every adjusted table. Without
    # it "Adjusted Survival" is ambiguous between a standardised whole-cohort
    # quantity and a single reference patient, which is what let CR-3 hide.
    # Up-front, plain-language statement of the estimand.
    #
    # The methods answer different questions, and until now the only place that
    # said so was a footnote under the tables -- read, if at all, after the
    # reader had already interpreted the curve. Naming the actual variable, its
    # levels and the cohort size makes it concrete rather than generic.
    .renderAdjustedEstimandPanel = function(mydata, adj_var, method) {
      item <- try(self$results$adjustedEstimandPanel, silent = TRUE)
      if (inherits(item, "try-error") || is.null(item)) return(invisible(NULL))

      esc <- function(x) htmltools::htmlEscape(as.character(x))
      var_show <- esc(if (!is.null(self$options$adjexplanatory))
                        self$options$adjexplanatory else adj_var)

      lv <- tryCatch({
        col <- mydata[[adj_var]]
        if (is.null(col)) character(0) else levels(droplevels(as.factor(col)))
      }, error = function(e) character(0))
      n_pat <- tryCatch(nrow(mydata), error = function(e) NA_integer_)

      lv_txt <- if (length(lv) > 0)
        paste0(" (", paste(esc(lv), collapse = ", "), ")") else ""

      body <- switch(
        method,
        "average" = paste0(
          "<p><b>", .("Standardised over cohort (g-computation)"), "</b></p>",
          "<p>", sprintf(.("Every one of the %s patients is set to <i>%s</i> = each level in turn%s, a survival curve is predicted for each patient from the fitted model, and those curves are averaged. The procedure repeats for every level."),
                         esc(n_pat), var_show, lv_txt), "</p>",
          # Do NOT name example covariates here: the text is generic and the
          # model may not contain them. An earlier draft said "the age, grade and
          # treatment mix", which asserted three variables that were absent from
          # the fitted model on the very first dataset it was run against.
          "<p>", .("Because the same patients underlie every curve, the curves differ <i>only</i> by the adjustment variable - the distribution of every other covariate in the model is identical across them. This is what most published papers mean by an adjusted survival curve."), "</p>",
          "<p>", .("Confidence intervals are left blank: a standardised curve has no closed-form interval and would need bootstrapping."), "</p>"),
        "conditional" = paste0(
          "<p><b>", .("At reference covariate profile"), "</b></p>",
          "<p>", sprintf(.("One curve is predicted per level of <i>%s</i>%s for a single reference patient: the mean of every numeric covariate and the most common level of every categorical covariate."),
                         var_show, lv_txt), "</p>",
          "<p>", .("This describes one hypothetical patient, not your cohort. If your covariates are skewed, that reference patient may resemble nobody in the data. Choose \"Standardised over cohort\" if you want a curve that represents the patients you actually have."), "</p>",
          "<p>", .("Confidence intervals are those of that single prediction."), "</p>"),
        "single" = paste0(
          "<p><b>", .("Whole-cohort expected survival"), "</b></p>",
          "<p>", sprintf(.("One curve is produced for the whole cohort, each patient at their own observed covariate values. <i>%s</i> is <b>not</b> varied, so this output does <b>not</b> compare its levels."),
                         var_show), "</p>",
          "<p>", .("Use this to see the model's overall fitted survival. If you selected an adjustment variable expecting to compare groups, choose \"Standardised over cohort\" instead."), "</p>",
          "<p>", .("Confidence intervals require bootstrapping and are left blank."), "</p>"),
        paste0("<p>", .("Model-based adjusted survival."), "</p>")
      )

      caveat <- paste0(
        "<p style='margin-top:10px;'><i>",
        .("These are model-based predictions. They rely on the Cox model being correctly specified and on proportional hazards holding, and they adjust only for variables in the model - not for anything unmeasured."),
        "</i></p>")

      if (private$.isCompetingRisk())
        caveat <- paste0(
          "<p style='margin-top:10px;'><b>", .("Competing risks:"), "</b> ",
          .("the fitted model is Fine-Gray, so the quantity plotted is cumulative incidence of the event of interest, which accounts for competing events. It is not 1 minus Kaplan-Meier."),
          "</p>", caveat)

      item$setContent(paste0(
        "<div style='font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif; ",
        "line-height:1.55; max-width:820px; background: rgba(88, 138, 205, 0.06); color: inherit; border-left:4px solid #0056b3; ",
        "padding:12px 16px;'>", body, caveat, "</div>"))

      invisible(NULL)
    }
    ,
    .adjustedEstimandNote = function(method) {
      base <- switch(
        method,
        "average" = .("Estimand: survival standardised over the observed patients - every patient is set to the stated level in turn and the model-predicted curves are averaged (g-computation). Confidence intervals require bootstrapping and are left blank."),
        "conditional" = .("Estimand: the model-predicted curve for one reference patient - the mean of every numeric covariate and the most common level of every categorical covariate. Confidence intervals are those of that single prediction."),
        "single" = .("Estimand: one curve for the whole cohort with each patient at their own covariate values; the adjustment variable is not varied. Confidence intervals require bootstrapping and are left blank."),
        "marginal" = .("Estimand: survival reweighted by the inverse probability of the observed level (survminer's marginal method). Confidence intervals are not available and are left blank."),
        .("Estimand: model-based adjusted survival.")
      )
      if (private$.isCompetingRisk()) {
        base <- paste(
          base,
          .("The fitted model is Fine-Gray; the displayed probability is cumulative incidence (1 minus subdistribution survival), not cause-specific survival."))
      }
      base
    }



      ,
    ## Adjusted Survival Table ----
    .adjustedSurvTable = function(results, cox_model, curves = NULL) {
      adj_var <- results$adjexplanatory_name
      mydata <- results$cleanData

      # Input validation
      if (is.null(mydata) || is.null(cox_model)) {
        return(NULL)
      }

      method <- self$options$ac_method
      if (is.null(curves)) {
        curves <- private$.adjustedCurveData(cox_model, mydata, adj_var, method)
      }
      if (is.null(curves)) {
        # The method was refused. jamovi does not clear a result item just
        # because the code that fills it was skipped, so wipe both explicitly or
        # last run's numbers stay on screen under the new method's name.
        self$results$adjustedSurvTable$deleteRows()
        self$results$adjustedSurvTableSummary$setContent("")
        return(NULL)
      }

      # Get timepoints
      timepoints <- tryCatch({
        tokens <- trimws(unlist(strsplit(self$options$cutp, ",")))
        pts <- suppressWarnings(as.numeric(tokens))
        if (any(!is.finite(pts) | pts <= 0))
          private$.addHtmlMessage(
            "warning",
            .("Invalid adjusted-curve timepoints ignored"),
            .("Adjusted-curve timepoints must be numeric and greater than zero. Invalid entries were ignored."))
        pts <- sort(unique(pts[is.finite(pts) & pts > 0]))
        if (length(pts) == 0) private$.getDefaultCutpoints() else pts
      }, error = function(e) private$.getDefaultCutpoints())

      # Observed follow-up, used only for the observed counts below.
      obs_time <- mydata[["mytime"]]
      obs_event <- .eventIndicator(mydata[["myoutcome"]])
      if (is.null(obs_event)) obs_event <- rep(NA_real_, nrow(mydata))
      obs_group <- as.character(mydata[[adj_var]])

      is_cr <- private$.isCompetingRisk()
      estimate_label <- if (is_cr) .("adjusted cumulative incidence") else .("adjusted survival")
      pct <- function(x) if (is.na(x)) "" else scales::percent(x, accuracy = 0.1)

      all_results <- list()
      for (grp in unique(curves$group)) {
        g <- curves[curves$group == grp, , drop = FALSE]

        # n.risk / n.event used to be lifted from the model's common risk set,
        # so every group showed the SAME numbers while carrying a group label.
        # These are genuine observed counts: patients whose observed level is
        # this one ("single" has no level, so the whole cohort). They describe
        # the data, not the standardised curve -- the column titles and the
        # table note say so.
        in_grp <- if (identical(method, "single")) rep(TRUE, nrow(mydata)) else obs_group == grp

        for (tp in timepoints) {
          row <- private$.adjustedCurveAt(g, tp)
          if (is.null(row)) next
          all_results[[length(all_results) + 1]] <- list(
            strata = grp,
            time   = tp,
            atrisk = sum(in_grp & obs_time >= tp, na.rm = TRUE),
            events = sum(in_grp & obs_time <= tp & obs_event == 1, na.rm = TRUE),
            # A Fine-Gray fit predicts subdistribution survival. The clinically
            # interpretable probability is its complement, cumulative
            # incidence. CI endpoints reverse under 1 - S.
            surv   = pct(if (is_cr) 1 - row$surv else row$surv),
            lower  = pct(if (is_cr) 1 - row$upper else row$lower),
            upper  = pct(if (is_cr) 1 - row$lower else row$upper)
          )
        }
      }

      # Clear existing rows (jmvcore Table has no setRows(); deleteRows() clears all)
      self$results$adjustedSurvTable$deleteRows()

      if (length(all_results) > 0) {
        for (i in seq_along(all_results)) {
          self$results$adjustedSurvTable$addRow(
            rowKey = i,
            values = all_results[[i]]
          )
        }

        # Generate natural language interpretations
        summaries <- sapply(all_results, function(row) {
          ci <- if (nzchar(row$lower) && nzchar(row$upper))
            glue::glue(" [{row$lower}-{row$upper}, 95% CI]") else ""
          glue::glue(
            "For {row$strata} at {row$time} {self$options$timetypeoutput}, {estimate_label} is {row$surv}{ci}. ",
            "Among the {row$atrisk} patients still under observation at that time, ",
            "{row$events} events had been observed in this group."
          )
        })

        self$results$adjustedSurvTableSummary$setContent(
          paste(c(summaries, private$.adjustedEstimandNote(method)), collapse = "<br><br>"))
      } else {
        self$results$adjustedSurvTableSummary$setContent("")
      }

      self$results$adjustedSurvTable$setNote("estimand", private$.adjustedEstimandNote(method))
      self$results$adjustedSurvTable$setNote(
        "counts",
        .("Observed at risk and observed events are counts in the data, not properties of the adjusted curve."))

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


        plotData <- image_plot_adj$state
        
        if (is.null(plotData)) {
          if (isTRUE(getOption("multisurvival.debug"))) {
            message("[multisurvival.debug] .plot_adj: state is NULL, recomputing...")
          }
          plotData <- private$.cleandata()
          if (is.null(plotData$cleanData)) return(FALSE)
        }

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

        model_vars <- unique(c(plotData$myexplanatory_labelled,
                               plotData$mystratvar_labelled))
        if (!(plotData$adjexplanatory_name %in% model_vars)) {
          grid::grid.newpage()
          grid::grid.text(
            paste0("Adjusted curves require the selected variable to be part of ",
                   "the fitted model. Add it under Explanatory Variables or ",
                   "Stratification Variables."),
            0.5, 0.5)
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
        is_finegray <- !is.null(cox_model$weights) && private$.isCompetingRisk()
        
        # Use correct data for plotting
        plot_data <- mydata

        if (is_finegray) {
          # Use the same estimator object as the numeric tables, then transform
          # Fine-Gray subdistribution survival to cumulative incidence. This
          # also honours average/conditional/single consistently; the previous
          # branch silently drew g-computation for every selected method.
          cif_df <- private$.adjustedCurveData(
            cox_model, mydata, adjexplanatory_name, self$options$ac_method)

          if (is.null(cif_df) || nrow(cif_df) == 0) {
            private$.addHtmlMessage(
              "warning",
              .("Adjusted competing-risks curve unavailable"),
              .("The adjusted cumulative-incidence curve could not be computed from the Fine-Gray model."))
            return(FALSE)
          }

          cif_df$cif <- 1 - cif_df$surv
          cif_df$cif_lower <- 1 - cif_df$upper
          cif_df$cif_upper <- 1 - cif_df$lower

          if (!is.null(self$options$endplot) && is.finite(self$options$endplot))
            cif_df <- cif_df[cif_df$time <= self$options$endplot, , drop = FALSE]

          p <- ggplot2::ggplot(cif_df,
                 ggplot2::aes(x = time, y = cif, colour = group))
          if (isTRUE(self$options$ci95) &&
              any(is.finite(cif_df$cif_lower) & is.finite(cif_df$cif_upper))) {
            p <- p + ggplot2::geom_ribbon(
              ggplot2::aes(ymin = cif_lower, ymax = cif_upper, fill = group),
              colour = NA, alpha = 0.18, show.legend = FALSE)
          }
          p <- p +
            ggplot2::geom_step(linewidth = 0.8, na.rm = TRUE) +
            ggplot2::scale_y_continuous(limits = c(0, 1)) +
            ggplot2::labs(
              x = paste0("Time (", self$options$timetypeoutput, ")"),
              y = .("Cumulative incidence"),
              colour = self$options$adjexplanatory,
              title = jmvcore::format(.("Adjusted Cumulative Incidence for {variable}"),
                                      variable = self$options$adjexplanatory),
              subtitle = private$.adjustedEstimandNote(self$options$ac_method),
              caption = .("Cumulative incidence, not 1 - Kaplan-Meier: competing events are accounted for.")) +
            ggplot2::theme_bw() +
            ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 8))

          print(p)
          return(TRUE)
        }

        method <- self$options$ac_method

        # Draw the SAME object the adjusted tables are built from. This used to
        # call survminer::ggadjustedcurves() directly, which made the plot the
        # only consumer of ac_method in the whole analysis; the tables computed
        # their own curve and never moved when the method changed. It also fell
        # back from "marginal" to "average" on error while keeping the requested
        # name in the title, so a failed run looked like a successful one.
        curves <- private$.adjustedCurveData(cox_model, plot_data,
                                             adjexplanatory_name, method)
        if (is.null(curves)) return(FALSE)   # refused; notice already emitted

        if (!is.null(self$options$endplot) && is.finite(self$options$endplot))
          curves <- curves[curves$time <= self$options$endplot, , drop = FALSE]
        if (nrow(curves) == 0) return(FALSE)

        plot <- ggplot2::ggplot(curves,
                 ggplot2::aes(x = time, y = surv, colour = group)) +
          ggplot2::geom_step(linewidth = 0.8, na.rm = TRUE) +
          ggplot2::scale_y_continuous(limits = c(0, 1)) +
          ggplot2::labs(
            x = paste0("Time (", self$options$timetypeoutput, ")"),
            y = .("Adjusted survival"),
            colour = self$options$adjexplanatory,
            fill = self$options$adjexplanatory,
            title = jmvcore::format(.("Adjusted Survival Curves for {variable}"),
                                    variable = self$options$adjexplanatory),
            subtitle = private$.adjustedEstimandNote(method)) +
          ggplot2::theme_bw() +
          ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8),
                         plot.caption = ggplot2::element_text(hjust = 0, size = 8))

        # Only `conditional` carries a confidence band; the standardised methods
        # return NA limits rather than borrowing someone else's interval.
        if (isTRUE(self$options$ci95) && any(is.finite(curves$lower)))
          plot <- plot + ggplot2::geom_ribbon(
            ggplot2::aes(ymin = lower, ymax = upper, fill = group),
            alpha = 0.15, colour = NA, na.rm = TRUE)

        if (!is.null(self$options$byplot) && is.finite(self$options$byplot) &&
            self$options$byplot > 0)
          plot <- plot + ggplot2::scale_x_continuous(
            breaks = seq(0, max(curves$time, na.rm = TRUE), by = self$options$byplot))




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
    .adjustedMedianSurv = function(results, cox_model, curves = NULL) {
      # cleanData carries the standardized survival columns "mytime"/"myoutcome"
      # (see .definemytime/.definemyoutcome); results$name* hold the *display
      # labels*, which are not column names.
      adj_var <- results$adjexplanatory_name
      mydata <- results$cleanData
      medianTable <- self$results$adjustedMedianTable

      method <- self$options$ac_method
      is_cr <- private$.isCompetingRisk()
      if (is.null(curves)) {
        curves <- private$.adjustedCurveData(cox_model, mydata, adj_var, method)
      }
      medianTable$deleteRows()
      if (is.null(curves)) {
        self$results$adjustedMedianSummary$setContent("")
        return(invisible(NULL))
      }

      event_indicator <- .eventIndicator(mydata[["myoutcome"]])
      if (is.null(event_indicator)) {
        event_indicator <- rep(NA, nrow(mydata))
      }
      obs_group <- as.character(mydata[[adj_var]])

      # The median is read off the SAME curve the plot draws: the first time the
      # adjusted survival is at or below 0.5. The confidence limits come from the
      # confidence band -- the lower band crosses 0.5 first, so it gives the
      # LOWER limit of the median. Deriving them here instead of from a private
      # survfit() is the point of CR-3: the median can no longer describe a
      # different estimand from the curve above it.
      crossing <- function(x, tm) {
        if (is.null(x) || all(is.na(x))) return(NA_real_)
        i <- which(!is.na(x) & x <= 0.5)
        if (length(i) == 0) NA_real_ else tm[min(i)]
      }

      rows <- list()
      for (grp in unique(curves$group)) {
        g <- curves[curves$group == grp, , drop = FALSE]
        in_grp <- if (identical(method, "single")) rep(TRUE, nrow(mydata)) else obs_group == grp
        rows[[length(rows) + 1]] <- list(
          factor   = grp,
          records  = sum(in_grp & !is.na(mydata[["mytime"]])),
          events   = sum(event_indicator[in_grp] == 1, na.rm = TRUE),
          median   = crossing(g$surv,  g$time),
          x0_95lcl = crossing(g$lower, g$time),
          x0_95ucl = crossing(g$upper, g$time)
        )
      }

      for (i in seq_along(rows)) {
        r <- rows[[i]]
        # Omit the CI cells entirely when there is no interval, rather than
        # setting them to NA. R's NA_real_ carries a NaN bit pattern, so a
        # `type: number` column rendered it as the literal string "NaN" -- in a
        # table whose own note says the intervals are "left blank". An unset
        # cell renders blank, which is what the note promises.
        vals <- list(
          factor  = r$factor,
          records = r$records,
          events  = r$events,
          median  = round(r$median, 1)
        )
        if (!is.na(r$x0_95lcl)) vals$x0_95lcl <- round(r$x0_95lcl, 1)
        if (!is.na(r$x0_95ucl)) vals$x0_95ucl <- round(r$x0_95ucl, 1)
        medianTable$addRow(rowKey = i, values = vals)
      }

      # Create natural language summaries
      summaries <- lapply(rows, function(r) {
        ci <- if (is.na(r$x0_95lcl) && is.na(r$x0_95ucl)) "" else
          glue::glue(" [{round(r$x0_95lcl, 1)} - {round(r$x0_95ucl, 1)}, 95% CI]")
        quantity <- if (is_cr) "adjusted median time to the event of interest" else
          "adjusted median survival"
        description <- glue::glue(
          "For {adj_var} = {r$factor}, {quantity} is {round(r$median, 1)}{ci} ",
          self$options$timetypeoutput, "."
        )

        if (is.na(r$median)) {
          description <- paste0(
            description,
            if (is_cr)
              "\nNote: The adjusted cumulative-incidence curve for this group does not reach 1/2 during "
            else
              "\nNote: The adjusted survival curve for this group does not drop below 1/2 during ",
            "the observation period, so this median is undefined."
          )
        }

        return(description)
      })

      # Add general interpretation
      medianSummary <- c(
        unlist(summaries),
        if (is_cr)
          "This median is the time when adjusted cumulative incidence of the event of interest reaches 50%."
        else
          "The median survival time is when adjusted survival reaches 50%.",
        private$.adjustedEstimandNote(method)
      )

      self$results$adjustedMedianSummary$setContent(paste(medianSummary, collapse = "<br><br>"))
      medianTable$setNote("estimand", private$.adjustedEstimandNote(method))
      medianTable$setNote(
        "counts",
        .("Records and observed events are counts in the data, not properties of the adjusted curve."))
      invisible(NULL)
    }


      ,
    # Recover variable / level / reference for each coefficient of a coxph.
    #
    # The rows of summary(coxph)$coefficients are DESIGN-MATRIX columns, not
    # variables: "stageIV" is level IV of `stage` measured against `stage`'s
    # reference level, and janitor::clean_names() has already renamed the column
    # the clinician actually chose. Printing that string raw produced
    # "For stageIV ... 653.4 % increase in hazard for each unit increase in
    # stageIV" -- a per-unit slope claim about a contrast that has no units,
    # attached to a comparison that was never named.
    #
    # The string cannot be split back apart, because a level may repeat the term
    # name ("treatmentTreatment A"). The fitted object carries the answer:
    #   $assign     named list, term -> coefficient indices
    #   $contrasts  named only for terms that got one; ABSENT => continuous
    #               (a logical predictor has an entry here but none in $xlevels)
    #   $xlevels    the levels; [1] is the reference under treatment contrasts
    # coxph is fitted with x/y/model = TRUE, and the Fine-Gray branch is still a
    # coxph, so all three are always present.
    .coefTerms = function(cox_model, display = NULL) {
      nms  <- rownames(summary(cox_model)$coefficients)
      amap <- cox_model$assign
      ctr  <- cox_model$contrasts
      xl   <- cox_model$xlevels

      # $assign carries the term as written in the formula, i.e. BACKTICKED for
      # a non-syntactic name (`my stage`), while $contrasts / $xlevels are keyed
      # on the bare name. Without this the lookup returns NULL and every level
      # silently degrades to the unnamed-contrast branch.
      bare  <- function(t) gsub("^`|`$", "", t)
      shown <- function(t) {
        t <- bare(t)
        if (!is.null(display) && t %in% names(display)) unname(display[[t]]) else t
      }

      term_of <- rep(NA_character_, length(nms))
      pos_in  <- rep(NA_integer_, length(nms))
      if (is.list(amap)) {
        for (tm in names(amap)) {
          idx <- amap[[tm]]
          idx <- idx[idx >= 1L & idx <= length(nms)]
          term_of[idx] <- tm
          pos_in[idx]  <- seq_along(idx)
        }
      }

      lapply(seq_along(nms), function(i) {
        tm <- term_of[i]
        if (is.na(tm))
          return(list(kind = "unknown", var = nms[i]))
        if (grepl(":", tm, fixed = TRUE)) {
          # Name the LEVEL of each crossed component. A 3-level factor crossed
          # with a 2-level one contributes two coefficients that share the same
          # term, so a term-only label prints the same string twice with
          # different hazard ratios beside it. Coefficient names are
          # paste0(term, level) joined by ":", and colons are not permitted in
          # factor levels, so the split is unambiguous; fall back to the bare
          # terms if the pieces do not line up.
          comps  <- strsplit(tm, ":", fixed = TRUE)[[1]]
          pieces <- strsplit(nms[i], ":", fixed = TRUE)[[1]]
          lab <- if (length(pieces) == length(comps)) {
            vapply(seq_along(comps), function(k) {
              lv <- substring(pieces[k], nchar(comps[k]) + 1L)
              if (nzchar(lv)) sprintf("%s: %s", shown(comps[k]), lv) else shown(comps[k])
            }, character(1))
          } else {
            vapply(comps, shown, character(1))
          }
          return(list(kind = "interaction", var = paste(lab, collapse = " \u{00D7} ")))
        }
        u <- bare(tm)
        if (!(u %in% names(ctr)))
          return(list(kind = "continuous", var = shown(tm)))
        lv <- xl[[u]]
        # Gate on treatment contrasts. An ORDERED factor gets contr.poly, whose
        # coefficients are .L/.Q polynomial trends -- still nlevels-1 of them,
        # so a positional level lookup would confidently print the WRONG
        # reference level. Obviously wrong beats subtly wrong; keep the gate.
        if (identical(unname(ctr[[u]]), "contr.treatment") &&
            !is.null(lv) && length(amap[[tm]]) == length(lv) - 1L)
          return(list(kind = "level", var = shown(tm),
                      level = lv[pos_in[i] + 1L], ref = lv[1L]))
        # Coefficient names are paste0(term, suffix), so substring is safe where
        # sub("^term", ...) would treat a term containing regex metacharacters
        # as a pattern.
        if (is.null(lv))   # logical predictor: contrast, but no levels recorded
          return(list(kind = "level", var = shown(tm),
                      level = substring(nms[i], nchar(tm) + 1L), ref = "FALSE"))
        list(kind = "contrast", var = shown(tm),
             suffix = substring(nms[i], nchar(tm) + 1L))
      })
    }
      ,
    ## Adjusted Cox ----
    .adjustedCox = function(results, cox_model) {
      mydata <- results$cleanData
      adj_var <- results$adjexplanatory_name

      # Get Cox model summary
      cox_summary <- summary(cox_model)
      is_cr <- private$.isCompetingRisk()
      effect_name <- if (is_cr) "subdistribution hazard ratio" else "hazard ratio"

      # Create metrics summary
      concordance_line <- if (is_cr) {
        "Concordance is not reported for the Fine-Gray pseudo-row fit; use a cause- and horizon-specific validation measure.<br>"
      } else {
        glue::glue("Concordance: {round(cox_summary$concordance[1], 3)} (SE = {round(cox_summary$concordance[2], 3)})<br>")
      }
      test_lines <- if (is_cr) {
        robust_score <- if (!is.null(cox_summary$robscore))
          glue::glue("Robust score test = {round(cox_summary$robscore[1], 2)}, df = {cox_summary$robscore[2]}, p = {format.pval(cox_summary$robscore[3], digits=3)}<br>")
        else ""
        glue::glue(
          "Robust Wald test = {round(cox_summary$waldtest[1], 2)}, df = {cox_summary$waldtest[2]}, p = {format.pval(cox_summary$waldtest[3], digits=3)}<br>",
          "{robust_score}",
          "Likelihood-ratio and ordinary score tests are omitted because Fine-Gray expansion creates correlated pseudo-rows.<br>")
      } else {
        glue::glue(
          "Likelihood ratio test = {round(cox_summary$logtest[1], 2)}, df = {cox_summary$logtest[2]}, p = {format.pval(cox_summary$logtest[3], digits=3)}<br>",
          "Wald test = {round(cox_summary$waldtest[1], 2)}, df = {cox_summary$waldtest[2]}, p = {format.pval(cox_summary$waldtest[3], digits=3)}<br>",
          "Score test = {round(cox_summary$sctest[1], 2)}, df = {cox_summary$sctest[2]}, p = {format.pval(cox_summary$sctest[3], digits=3)}<br>")
      }
      tCoxtext2 <- glue::glue("
        <br>
        <b>Model Metrics:</b><br>
        {concordance_line}
        {test_lines}
    ")

      if (self$options$uselandmark) {
        landmark <- jmvcore::toNumeric(self$options$landmark)
        tCoxtext2 <- glue::glue(
          tCoxtext2,
          "Landmark time used as: ", landmark, " ", self$options$timetypeoutput, "."
        )
      }

      self$results$adjustedCoxText$setContent(tCoxtext2)

      # Extract hazard ratios and CIs BY COLUMN NAME, never by position.
      #
      # summary(coxph)$coefficients has 5 columns for an ordinary fit
      #   coef | exp(coef) | se(coef) | z | Pr(>|z|)
      # but SIX for a clustered/robust fit (as used for Fine-Gray):
      #   coef | exp(coef) | se(coef) | robust se | z | Pr(>|z|)
      #
      # Indexing positionally therefore silently reported the Z STATISTIC as the
      # p-value whenever clustering was in play -- which is how a "p-value" of
      # -11.5 reaches the screen -- and built the confidence interval from the
      # NAIVE standard error, ignoring the robust one the model was fitted to
      # produce, so the intervals were too narrow as well.
      cf <- cox_summary$coefficients
      cn <- colnames(cf)

      col_coef <- if ("coef" %in% cn) "coef" else 1L
      # Prefer the robust SE when the model carries one.
      col_se   <- if ("robust se" %in% cn) "robust se" else
                  if ("se(coef)" %in% cn) "se(coef)" else 3L
      col_p    <- if ("Pr(>|z|)" %in% cn) "Pr(>|z|)" else
                  if ("Pr(>|t|)" %in% cn) "Pr(>|t|)" else ncol(cf)

      .beta <- cf[, col_coef]
      .se   <- cf[, col_se]
      .p    <- cf[, col_p]

      # qnorm(0.975), not 1.96: finalfit builds the main table's intervals with
      # the exact quantile, and this table is now required to agree with it.
      .z <- stats::qnorm(0.975)
      coef_matrix <- cbind(
        exp(.beta),                       # HR
        exp(.beta - .z * .se),            # Lower CI
        exp(.beta + .z * .se),            # Upper CI
        .p                                # p-value
      )

      # Render variable + level from the fitted object, never the raw
      # coefficient string. mydata_labelled carries janitor's cleaned name ->
      # the clinician's original column name for EVERY column, so strata and
      # interaction components are covered too.
      display <- tryCatch({
        lb <- labelled::var_label(results$mydata_labelled)
        lb <- lb[!vapply(lb, is.null, logical(1))]
        stats::setNames(as.character(unlist(lb)), names(lb))
      }, error = function(e) NULL)
      terms <- private$.coefTerms(cox_model, display)

      # Create Cox table
      coxTable <- self$results$adjustedCoxTable
      coxTable$deleteRows()
      coxTable$setNote(
        "effect",
        if (is_cr)
          .("Effects are Fine-Gray subdistribution hazard ratios for the event of interest; they are not cause-specific hazard ratios or cumulative-risk ratios.")
        else
          .("Effects are Cox proportional-hazards ratios."))

      for (i in seq_len(nrow(coef_matrix))) {
        tt <- terms[[i]]
        coxTable$addRow(
          rowKey = i,
          values = list(
            Variable = switch(
              tt$kind,
              level       = sprintf("%s: %s (vs %s)", tt$var, tt$level, tt$ref),
              continuous  = sprintf("%s (per 1-unit increase)", tt$var),
              contrast    = sprintf("%s (%s contrast)", tt$var, tt$suffix),
              interaction = sprintf("%s (interaction)", tt$var),
              tt$var
            ),
            HR = sprintf("%.2f (%.2f-%.2f)",
                         coef_matrix[i,1], coef_matrix[i,2], coef_matrix[i,3]),
            Pvalue = coef_matrix[i,4]
          )
        )
      }

      # Interpretive summary. adjustedCoxSummary is type: Html, i.e. a raw-HTML
      # sink, and variable names and factor levels come from imported .csv/.omv
      # data -- escape both.
      .esc <- function(x) htmltools::htmlEscape(as.character(x))
      .ci  <- function(i) sprintf("%.2f (%.2f-%.2f, 95%% CI)",
                                  coef_matrix[i,1], coef_matrix[i,2], coef_matrix[i,3])

      coxSummary <- vapply(seq_len(nrow(coef_matrix)), function(i) {
        tt <- terms[[i]]
        v  <- .esc(tt$var)
        switch(
          tt$kind,
          level = sprintf(
            "For %s = %s compared with %s = %s, the adjusted %s is %s, holding the other covariates in the model constant.",
            v, .esc(tt$level), v, .esc(tt$ref), effect_name, .ci(i)),
          continuous = sprintf(
            "For %s, the adjusted %s is %s per 1-unit increase in %s, holding the other covariates in the model constant.",
            v, effect_name, .ci(i), v),
          contrast = sprintf(
            "For the %s %s contrast, the adjusted %s is %s. %s is an ordered factor, so this is a polynomial trend across its levels, not a comparison with one reference level.",
            v, .esc(tt$suffix), effect_name, .ci(i), v),
          interaction = sprintf(
            "For the interaction %s, the adjusted %s is %s. This is a ratio of hazard ratios - how one variable's effect differs across the other - not the effect for any single group.",
            v, effect_name, .ci(i)),
          # Term not resolvable from the model: state the estimate and claim
          # nothing about what a change in it would mean.
          sprintf("For %s, the adjusted %s is %s.", v, effect_name, .ci(i))
        )
      }, character(1))

      # The old closing line quoted "(hr-1)*100 % increase in hazard" per row.
      # It is only defensible for a continuous per-unit slope, it turned HR 7.53
      # into "653.4 % increase", and the HR with its CI already carries the
      # magnitude on the scale the model estimated. Deleted, not reworded.
      coxSummary <- c(
        coxSummary,
        if (is_cr)
          "A subdistribution hazard ratio above 1 corresponds to greater cumulative incidence over follow-up under the Fine-Gray model. It is not a cause-specific event-rate ratio or a cumulative-risk ratio."
        else
          "A hazard ratio above 1 means a higher instantaneous event rate and below 1 a lower one. It is a rate ratio, not a ratio of cumulative risks and not a difference in survival time.",
        "All estimates are mutually adjusted for the other variables in this model. These are associations observed in these data."
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
    ## Final Fit ----
.final_fit2 = function() {

  # Never print a second, different model in competing-risks mode.
  #
  # The central model here is Fine-Gray (subdistribution hazards, fitted on
  # finegray()-expanded data with subject-clustered robust variance). This
  # function, however, collapses "Competing" to 0 below and hands the result to
  # finalfit, which fits an ordinary CAUSE-SPECIFIC Cox model -- a different
  # estimand answering a different question. Both were then displayed in the
  # same panel: the Fine-Gray clinical narrative was prepended onto a
  # cause-specific hazard-ratio table, and the model-metrics line underneath
  # carried the Fine-Gray concordance. On simulated data where a covariate
  # raises only the competing hazard this printed sHR 0.29 as HR 0.90.
  #
  # There is no way to label this out of trouble: two estimands in one table is
  # a reader trap. The Fine-Gray results above are the competing-risks answer.
  if (private$.isCompetingRisk()) {
    private$.addHtmlMessage(
      "info",
      .("Fine-Gray subdistribution model"),
      .("Competing-risks mode reports subdistribution hazard ratios (sHRs). They describe association with the cumulative incidence of the event of interest and are not cause-specific hazard ratios. To obtain cause-specific hazard ratios, set the survival type to cause-specific.")
    )

    # Report the central Fine-Gray fit directly. The previous safety guard
    # correctly suppressed finalfit's second, cause-specific Cox refit, but it
    # also returned without rendering ANY coefficient table unless the user
    # happened to request adjusted-survival summaries. A default competing-risk
    # analysis therefore fitted a model and showed no model estimates.
    fg_model <- private$.cox_model()
    if (is.null(fg_model)) return(invisible(NULL))

    sm <- summary(fg_model)
    cf <- sm$coefficients
    cn <- colnames(cf)
    coef_col <- if ("coef" %in% cn) "coef" else 1L
    se_col <- if ("robust se" %in% cn) "robust se" else
              if ("se(coef)" %in% cn) "se(coef)" else 3L
    p_col <- if ("Pr(>|z|)" %in% cn) "Pr(>|z|)" else ncol(cf)
    z <- stats::qnorm(0.975)

    cleaneddata <- private$.cleandata()
    display <- tryCatch({
      lb <- labelled::var_label(cleaneddata$mydata_labelled)
      lb <- lb[!vapply(lb, is.null, logical(1))]
      stats::setNames(as.character(unlist(lb)), names(lb))
    }, error = function(e) NULL)
    term_info <- private$.coefTerms(fg_model, display)
    term_labels <- vapply(term_info, function(tt) switch(
      tt$kind,
      level = sprintf("%s: %s (vs %s)", tt$var, tt$level, tt$ref),
      continuous = sprintf("%s (per 1-unit increase)", tt$var),
      contrast = sprintf("%s (%s contrast)", tt$var, tt$suffix),
      interaction = sprintf("%s (interaction)", tt$var),
      tt$var
    ), character(1))

    beta <- cf[, coef_col]
    se <- cf[, se_col]
    fg_table <- data.frame(
      Variable = term_labels,
      `sHR (95% CI)` = sprintf("%.2f (%.2f-%.2f)",
        exp(beta), exp(beta - z * se), exp(beta + z * se)),
      `p-value` = format.pval(cf[, p_col], digits = 3, eps = 0.001),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    self$results$text$setContent(knitr::kable(
      fg_table, row.names = FALSE, format = "html", escape = TRUE))
    self$results$text2$setContent(paste0(
      "<p><b>Fine-Gray subdistribution hazards model.</b> ",
      "Confidence intervals and p-values use the subject-clustered robust variance. ",
      "An sHR is not a risk ratio and does not quantify a cause-specific hazard.</p>"))
    return(invisible(NULL))
  }

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

  # Carry stratification into the *displayed* model.
  #
  # This table used to be fitted with no strata term whatever the user chose,
  # while cox.zph and the footer below described a stratified fit. A single
  # report then claimed the model was stratified by, say, treatment and stage
  # and simultaneously printed hazard ratios *for* treatment and stage --
  # impossible, since a stratified variable is absorbed into the baseline
  # hazard and has no coefficient. The give-away was the likelihood ratio df,
  # which counted the strata as covariates.
  strata_ff <- NULL
  if (isTRUE(self$options$use_stratify) && !is.null(self$options$stratvar)) {
    strata_ff <- as.vector(cleaneddata$mystratvar_labelled)
    strata_ff <- strata_ff[!is.na(strata_ff) & nzchar(strata_ff)]
    if (length(strata_ff) == 0) strata_ff <- NULL
    # `explanatory_formula` is deliberately left intact: a stratified variable
    # still has a meaningful UNIVARIABLE hazard ratio, and dropping it from the
    # table altogether would hide information the unstratified report showed.
    # It is excluded from the multivariable fit only, at the call below.
  }

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
    # `explanatory_multi` keeps the strata out of the univariable column: a
    # univariable fit of `Surv(...) ~ strata(v)` has no covariate at all and
    # would come back empty.
    #
    # cont_cut = 0 stops finalfit re-specifying the model behind our back.
    #
    # finalfit::finalfit() runs
    #   cont_distinct = select(contains(explanatory)) %>% summarise_if(is.numeric, n_distinct) %>% keep(~ .x < cont_cut)
    #   .data = mutate_at(.data, cont_distinct, as.factor)
    # with cont_cut = 5 by default, then fits ITS OWN coxph on the mutated
    # frame. Any numeric covariate with fewer than 5 distinct values -- an
    # ordinal score such as performance status 0/1/2 -- was therefore silently
    # promoted to a factor here while .cox_model() kept it numeric. One report
    # then carried two different models: this table showed two rows for
    # performance_status with LR df = 7, the Adjusted Cox table showed one row
    # with df = 6, and because the adjustment sets differed every SHARED
    # coefficient moved too (gradePoor p .129 vs .127). cont_cut = 0 makes
    # keep(~ .x < 0) match nothing, so finalfit fits exactly the specification
    # the user declared -- the same one .cox_model() fits.
    #
    # It also neutralises the substring match in that select(contains(...)):
    # a column merely CONTAINING an explanatory name could be dragged in and
    # factorised. Do not "tidy away" this argument.
    if (is.null(strata_ff)) {
      finalfit::finalfit(
        .data = mydata,
        dependent = dependent_formula,
        explanatory = explanatory_formula,
        cont_cut = 0,
        metrics = TRUE
      ) -> tCox
    } else {
      covars_multi <- explanatory_formula[!explanatory_formula %in% strata_ff]

      # Every selected covariate is also a stratification variable, so the
      # multivariable model has nothing left to estimate. finalfit would fail
      # with a formula parser error shown verbatim to the clinician.
      if (length(covars_multi) == 0) {
        private$.addHtmlMessage(
          "warning",
          .("No covariates left to estimate"),
          .("Every selected explanatory variable is also a stratification variable. A stratified variable is absorbed into the baseline hazard and has no hazard ratio, so no multivariable model can be fitted. Remove at least one variable from the stratification list, or add a covariate that is not stratified.")
        )
        return(invisible(NULL))
      }

      finalfit::finalfit(
        .data = mydata,
        dependent = dependent_formula,
        explanatory = explanatory_formula,
        explanatory_multi = c(covars_multi, paste0("strata(", strata_ff, ")")),
        cont_cut = 0,
        metrics = TRUE
      ) -> tCox
    }

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

    # One C-index per report.
    #
    # finalfit computes its own concordance from its own fit, which differed
    # from the value every other panel shows (Model Performance Metrics, the
    # risk-score summary and the nomogram summary all read
    # summary(.cox_model())$concordance). A single report therefore stated
    # C = 0.591 here and C = 0.572 three panels later, leaving no way to tell
    # which to cite. The .cox_model() fit is the one the rest of the analysis is
    # built on, so its concordance wins and finalfit's is overwritten here.
    conc_model <- tryCatch({
      cm <- private$.cox_model()
      if (is.null(cm)) NULL else summary(cm)$concordance
    }, error = function(e) NULL)

    if (!is.null(conc_model) && length(conc_model) >= 1 && is.finite(conc_model[1])) {
      conc_txt <- if (length(conc_model) >= 2 && is.finite(conc_model[2]))
        sprintf("Concordance = %.3f (SE = %.3f)", conc_model[1], conc_model[2])
      else
        sprintf("Concordance = %.3f", conc_model[1])

      metrics_text <- sub("Concordance = [0-9.]+( \\(SE = [0-9.]+\\))?",
                          conc_txt, metrics_text)
    }

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

        # Significance counting is shared with the clinical summary so the two
        # can no longer report different numbers for the same model. This path
        # counted significant coefficient ROWS but printed the total as a count
        # of VARIABLES, so a model with 3 significant variables across 5 level
        # rows was announced as "5 out of 8 factors".
        .sig <- private$.summariseCoxSignificance(cox_table)
        sig_count       <- .sig$n_sig_vars
        sig_rows        <- .sig$n_sig_rows
        strongest_var   <- .sig$strongest_label
        strongest_hr    <- if (is.na(.sig$strongest_hr)) 1 else .sig$strongest_hr
        strongest_effect <- if (nzchar(.sig$strongest_effect)) .sig$strongest_effect else NULL

        # Count total variables analyzed, as actually carried into the table.
        n_vars <- if (.sig$n_total_vars > 0) .sig$n_total_vars
                  else length(c(self$options$explanatory, self$options$contexpl))

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
            "factors showed statistically significant associations with the outcome (p < 0.05)",
            if (!is.null(sig_rows) && sig_rows > sig_count)
              paste0(", across ", sig_rows, " coefficient levels")
            else "",
            "."
          )

          if (!is.null(strongest_var) && !is.null(strongest_effect)) {
            summary_parts$strongest <- paste0(
              "<br><br><b>Strongest predictor:</b> ", htmltools::htmlEscape(strongest_var), " was associated with ",
              strongest_effect, " (hazard ratio = ", round(strongest_hr, 2), ")."
            )
          }

          summary_parts$interpretation <- paste0(
            "<br><br><b>Clinical interpretation:</b> Clinical importance cannot be assigned from ",
            "the hazard-ratio magnitude or p-value alone. Interpret the estimate with its confidence ",
            "interval, outcome definition, predictor scale, study design, and external evidence. ",
            "This is an association observed in these data."
          )
        } else {
          summary_parts$findings <- paste0(
            "<br><br><b>Key Finding:</b> No statistically significant associations were identified among the ",
            n_vars, " factors examined (all p-values \u2265 0.05)."
          )
        }

        # Combine all parts into HTML
        full_summary <- paste0(
          "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #007bff; color: inherit;'>",
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
        <div class="explanation-box" style="background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;">
            <h3 style="color: #2c5282; margin-top: 0;"> Understanding Multivariable Cox Regression</h3>

            <div style="background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;">
                <h4 style="color: #2d3748; margin-top: 0;">What is Multivariable Survival Analysis?</h4>
                <p style="margin: 8px 0;">Multivariable Cox regression analyzes <strong>multiple variables simultaneously</strong> to estimate each variable&#39;s conditional association with the event hazard, given the others in the fitted model.</p>

                <div style="background-color: rgba(33, 184, 255, 0.11); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                    <strong> Key Advantage:</strong> Reports mutually adjusted associations while making the model specification explicit
                </div>
            </div>

            <div style="background-color: rgba(246, 163, 33, 0.11); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                <h4 style="color: #d68910; margin-top: 0;"> Adjusted vs Unadjusted Hazard Ratios</h4>
                <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                    <tr style="background-color: rgba(255, 202, 33, 0.23); color: inherit;">
                        <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Type</th>
                        <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">What It Shows</th>
                        <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Clinical Use</th>
                    </tr>
                    <tr>
                        <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Unadjusted HR</strong></td>
                        <td style="padding: 8px; border: 1px solid #ffc107;">Raw association with survival</td>
                        <td style="padding: 8px; border: 1px solid #ffc107;">Initial screening of factors</td>
                    </tr>
                    <tr style="background-color: rgba(255, 196, 33, 0.07); color: inherit;">
                        <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Adjusted HR</strong></td>
                        <td style="padding: 8px; border: 1px solid #ffc107;">Conditional association given the other model variables</td>
                        <td style="padding: 8px; border: 1px solid #ffc107;">Model-based prognostic association</td>
                    </tr>
                </table>
            </div>

            <div style="background-color: rgba(33, 159, 43, 0.1); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
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
                    <p style="margin: 8px 0;"><strong>Interpretation:</strong> In this illustrative fitted model, stage has the largest adjusted hazard-ratio magnitude. This does not establish causality or transportability.</p>
                </div>

                <div style="background-color: rgba(153, 33, 170, 0.12); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                    <strong> Confounding Example:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li><strong>Unadjusted:</strong> Age HR = 1.05 (appears strongly associated)</li>
                        <li><strong>Adjusted for stage:</strong> Age HR = 1.01 (much weaker effect)</li>
                        <li><strong>Reason:</strong> Older patients tend to have more advanced disease</li>
                    </ul>
                </div>
            </div>

            <div style="background-color: rgba(33, 152, 239, 0.13); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                <h4 style="color: #1976d2; margin-top: 0;"> Model Building Strategy</h4>
                <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>1. Variable Selection:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li>Include clinically important variables</li>
                        <li>Prefer clinically justified, pre-specified variables; avoid automatic screening by p-value alone</li>
                        <li>Check for multicollinearity</li>
                    </ul>

                    <strong>2. Model Assessment:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li>Assess discrimination with uncertainty and validation; there is no universal clinical C-index threshold</li>
                        <li>Proportional hazards assumption testing</li>
                        <li>Model calibration assessment</li>
                    </ul>
                </div>
            </div>

            <div style="background-color: rgba(255, 169, 33, 0.14); padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800; color: inherit;">
                <strong> Clinical Applications:</strong>
                <ul style="margin: 5px 0; padding-left: 20px;">
                    <li><strong>Prognostic models:</strong> Identify independent risk factors</li>
                    <li><strong>Treatment research:</strong> Estimate adjusted associations; causal treatment benefit requires an appropriate causal design</li>
                    <li><strong>Risk stratification:</strong> Develop candidate risk scores that still require validation</li>
                    <li><strong>Research:</strong> Account for measured model variables, while acknowledging residual and unmeasured confounding</li>
                </ul>
            </div>
        </div>
        ')

        # Adjusted Survival Curves Explanation
        private$.setExplanationContent("adjustedSurvivalExplanation", '
        <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 163, 188, 0.21); border-left: 4px solid #bee5eb; color: inherit;">
            <h4 style="margin-top: 0; color: #2c3e50;">Understanding Adjusted Survival Curves</h4>
            <p><strong>Adjusted Curves:</strong> Model-based survival or cumulative-incidence predictions under the selected covariate standardisation.</p>
            <ul>
                <li><strong>Standardised option:</strong> Sets every observed patient to each group level in turn and averages predictions</li>
                <li><strong>Reference-profile option:</strong> Predicts for one hypothetical mean/mode covariate profile</li>
                <li><strong>Scope:</strong> Adjusts only for measured variables included in a correctly specified model</li>
                <li><strong>Interpretation:</strong> Supports adjusted descriptive comparisons, not automatic causal conclusions</li>
            </ul>
            <p><em>Use with:</em> Proportional-hazards assessment, a justified covariate set, and an explicit statement of the chosen estimand.</p>
        </div>
        ')

        # Risk Score Analysis Explanation
        private$.setExplanationContent("riskScoreExplanation", '
        <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; color: inherit;">
            <h4 style="margin-top: 0; color: #2c3e50;">Understanding Risk Score Analysis</h4>
            <p><strong>Risk Scoring:</strong> Combines model terms into the Cox relative-risk score, exp(centered linear predictor).</p>
            <ul>
                <li><strong>Displayed Score:</strong> Exponentiated centered linear predictor; not an absolute event probability</li>
                <li><strong>Risk Stratification:</strong> Divides patients into low, intermediate, and high-risk groups</li>
                <li><strong>Apparent Performance:</strong> Group separation in the development data is optimistic</li>
                <li><strong>Clinical Utility:</strong> Requires calibration, external validation, and impact assessment</li>
            </ul>
            <p><em>Use:</em> Exploratory model summarisation and validation planning, not direct treatment assignment.</p>
        </div>
        ')

        # Nomogram Explanation
        private$.setExplanationContent("nomogramExplanation", '
        <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; color: inherit;">
            <h4 style="margin-top: 0; color: #721c24;">Understanding Nomograms</h4>
            <p><strong>Nomogram:</strong> Graphical representation of predictions from the fitted Cox model.</p>
            <ul>
                <li><strong>Point System:</strong> Each predictor contributes points based on its value and hazard ratio</li>
                <li><strong>Total Points:</strong> Sum of individual points provides overall risk score</li>
                <li><strong>Survival Probability:</strong> Converts total points to predicted survival at specific time points</li>
                <li><strong>Validation:</strong> Apparent predictions must be calibrated and externally validated before clinical use</li>
            </ul>
            <p><em>Important:</em> This output is not a point-of-care calculator by itself.</p>
        </div>
        ')

        # Person-Time Analysis Explanation
        private$.setExplanationContent("personTimeExplanation", '
        <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 162, 64, 0.19); border-left: 4px solid #28a745; color: inherit;">
            <h4 style="margin-top: 0; color: #2c3e50;">Understanding Person-Time Analysis</h4>
            <p><strong>Person-Time:</strong> Comprehensive measure combining participant count and observation duration.</p>
            <ul>
                <li><strong>Incidence Rates:</strong> Events per person-time unit across different time intervals</li>
                <li><strong>Time-Stratified Analysis:</strong> Examines how event rates change over follow-up time</li>
                <li><strong>Group Comparisons:</strong> Compares incidence rates between different risk groups</li>
                <li><strong>Group Rates:</strong> Reports descriptive rates for selected groups; it does not estimate adjusted rate ratios</li>
            </ul>
            <p><em>Interpretation:</em> Describes how observed event rates vary over follow-up; interval differences may reflect changing risk sets and are not adjusted causal effects.</p>
        </div>
        ')

        # Stratified Analysis Explanation
        private$.setExplanationContent("stratifiedAnalysisExplanation", '
        <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 41, 56, 0.13); border-left: 4px solid #6c757d; color: inherit;">
            <h4 style="margin-top: 0; color: #2c3e50;">Understanding Stratified Cox Regression</h4>
            <p><strong>Stratification:</strong> Allows different baseline hazards for distinct patient subgroups while estimating common covariate effects.</p>
            <ul>
                <li><strong>Heterogeneous Baseline Risk:</strong> Accounts for fundamentally different risk levels between strata</li>
                <li><strong>Common Covariate Effects:</strong> Assumes treatment/predictor effects are similar across strata</li>
                <li><strong>Model Structure:</strong> Accommodates distinct baseline-hazard shapes without estimating a coefficient for the stratifying variable</li>
                <li><strong>Trade-off:</strong> Common covariate effects are still assumed across strata and must be assessed</li>
            </ul>
            <p><em>When to use:</em> When proportional hazards assumption is violated due to different baseline hazards between groups.</p>
        </div>
        ')

        # Survival Plots Explanation
        private$.setExplanationContent("survivalPlotsExplanation", '
        <div class="explanation-box" style="background-color: rgba(155, 155, 155, 0.06); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;">
            <h3 style="color: #2c5282; margin-top: 0;"> Understanding Adjusted Survival Curves and Hazard Ratio Plots</h3>

            <div style="background-color: rgba(33, 159, 43, 0.1); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                <h4 style="color: #2e7d32; margin-top: 0;"> Adjusted Survival Curves</h4>
                <p style="margin: 8px 0;">Adjusted curves show model-based survival or cumulative-incidence predictions under the selected covariate standardisation.</p>

                <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>Key Features:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li><strong>Covariate-adjusted:</strong> Conditions or standardises over measured model variables</li>
                        <li><strong>Estimand-specific:</strong> May average over observed patients or use one reference profile</li>
                        <li><strong>Adjusts for modelled covariates:</strong> does not establish a causal effect</li>
                        <li><strong>Clinical caution:</strong> Requires model checking and validation before decision use</li>
                    </ul>
                </div>

                <div style="background-color: rgba(33, 152, 239, 0.13); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                    <strong> Interpretation Guide:</strong>
                    <table style="width: 100%; border-collapse: collapse; margin: 5px 0;">
                        <tr style="background-color: rgba(33, 147, 242, 0.31); color: inherit;">
                            <th style="padding: 8px; text-align: left; border: 1px solid #2196f3;">Curve Pattern</th>
                            <th style="padding: 8px; text-align: left; border: 1px solid #2196f3;">Clinical Meaning</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #2196f3;">Steep early decline</td>
                            <td style="padding: 8px; border: 1px solid #2196f3;">High early mortality risk</td>
                        </tr>
                        <tr style="background-color: rgba(55, 138, 255, 0.06); color: inherit;">
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

            <div style="background-color: rgba(255, 169, 33, 0.14); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                <h4 style="color: #d68910; margin-top: 0;"> Hazard Ratio (Forest) Plots</h4>
                <p style="margin: 8px 0;">Forest plots visualize <strong>hazard ratios and confidence intervals</strong> for multiple variables simultaneously, enabling quick assessment of relative risk factors.</p>

                <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>Reading Forest Plots:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li><strong>Vertical line at HR=1:</strong> Line of no effect (reference)</li>
                        <li><strong>Points to the right (HR>1):</strong> Increased hazard (worse survival)</li>
                        <li><strong>Points to the left (HR<1):</strong> Decreased hazard (better survival)</li>
                        <li><strong>Horizontal lines:</strong> 95% confidence intervals for each HR</li>
                        <li><strong>Crossing HR=1:</strong> The interval includes the null value; review its width and exact p-value</li>
                    </ul>
                </div>

                <div style="background-color: rgba(246, 163, 33, 0.11); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                    <strong> Clinical Example - Cancer Study:</strong>
                    <table style="width: 100%; margin: 5px 0;">
                        <tr><td><strong>Age (per year):</strong></td><td>HR = 1.02 [0.99-1.05] \u2192 Small, imprecisely estimated association per year</td></tr>
                        <tr><td><strong>Stage III vs I:</strong></td><td>HR = 3.2 [2.1-4.8] \u2192 Higher fitted hazard for Stage III</td></tr>
                        <tr><td><strong>Treatment B vs A:</strong></td><td>HR = 0.6 [0.4-0.9] \u2192 Lower fitted hazard; not automatically a causal treatment effect</td></tr>
                    </table>
                </div>
            </div>

            <div style="background-color: rgba(33, 184, 255, 0.11); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                <h4 style="color: #1976d2; margin-top: 0;"> Clinical Applications</h4>

                <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>1. Treatment Comparison:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li>Adjusted curves account for measured variables included in the model; residual confounding can remain</li>
                        <li>Shows the treatment association after adjusting for modelled covariates; residual confounding may remain</li>
                        <li>Observational treatment comparisons require a justified causal design beyond outcome regression alone</li>
                    </ul>

                    <strong>2. Prognostic Modeling:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li>Displays conditional associations from the fitted model</li>
                        <li>Shows effect scales and uncertainty for model terms</li>
                        <li>Supports development of candidate prognostic models that require validation</li>
                    </ul>

                    <strong>3. Risk Stratification:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li>Combines multiple risk factors for patient classification</li>
                        <li>Requires pre-specified thresholds and impact evaluation before guiding treatment</li>
                        <li>Provides apparent predictions that require calibration and external validation</li>
                    </ul>
                </div>
            </div>

            <div style="background-color: rgba(153, 33, 170, 0.12); padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #9c27b0; color: inherit;">
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
                    '<div style="background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;">',
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

        # Single source of truth for "how many predictors were significant".
        #
        # Two summaries used to answer this question separately and disagree in
        # the same report -- one said "1 out of 8", the other "5 out of 8", for a
        # model with 3 significant variables spread over 5 coefficient rows. The
        # first read a column named "p" that a finalfit table does not have (the
        # p-value is embedded in the HR string); the second counted coefficient
        # ROWS but printed them against a count of VARIABLES. Both now call this.
        .summariseCoxSignificance = function(cox_table) {
            out <- list(n_sig_vars = 0, n_total_vars = 0, n_sig_rows = 0,
                        strongest_label = NULL, strongest_hr = NA_real_,
                        strongest_effect = "")
            if (is.null(cox_table) || !is.data.frame(cox_table) || nrow(cox_table) == 0)
                return(out)

            hr_name <- if ("HR (multivariable)" %in% names(cox_table)) "HR (multivariable)"
                       else if ("HR (univariable)" %in% names(cox_table)) "HR (univariable)"
                       else return(out)

            # finalfit prints the variable name on the FIRST row of each block
            # only, so a level row carries "". Reading column 1 directly is why
            # the strongest predictor rendered as an empty name whenever it was
            # not the reference level -- "stage IV" arrived as "".
            var_col <- as.character(cox_table[[1]])
            var_col[is.na(var_col) | !nzchar(trimws(var_col))] <- NA_character_
            for (k in seq_along(var_col))
                if (is.na(var_col[k]) && k > 1) var_col[k] <- var_col[k - 1]

            lvl_col <- if (ncol(cox_table) >= 2) as.character(cox_table[[2]])
                       else rep("", nrow(cox_table))
            hr_txt <- as.character(cox_table[[hr_name]])

            out$n_total_vars <- length(unique(stats::na.omit(var_col)))
            sig_vars <- character(0)
            best <- 0

            for (i in seq_along(hr_txt)) {
                txt <- hr_txt[i]
                if (is.na(txt) || !nzchar(trimws(txt)) || identical(trimws(txt), "-")) next
                hr_m <- regmatches(trimws(txt), regexpr("^[0-9]+\\.?[0-9]*", trimws(txt)))
                p_m  <- regmatches(txt, regexpr("p[=<][0-9.]+", txt))
                if (length(hr_m) == 0 || length(p_m) == 0) next
                hr_val <- suppressWarnings(as.numeric(hr_m[1]))
                p_val  <- suppressWarnings(as.numeric(sub("p[=<]", "", p_m[1])))
                if (is.na(hr_val) || is.na(p_val) || p_val >= 0.05) next

                out$n_sig_rows <- out$n_sig_rows + 1
                if (!is.na(var_col[i])) sig_vars <- c(sig_vars, var_col[i])

                if (hr_val > 0 && abs(log(hr_val)) > best) {
                    best <- abs(log(hr_val))
                    # var_col[i] is still NA when the very first row carried a
                    # blank label (nothing earlier to forward-fill from), which
                    # would render the literal string "NA (level)".
                    lbl <- if (is.na(var_col[i])) trimws(lvl_col[i]) else var_col[i]
                    if (!is.na(var_col[i]) && !is.na(lvl_col[i]) &&
                        nzchar(trimws(lvl_col[i])) &&
                        !grepl("^Mean|^Median", trimws(lvl_col[i])))
                        lbl <- paste0(lbl, " (", trimws(lvl_col[i]), ")")
                    if (!nzchar(lbl)) next
                    out$strongest_label  <- lbl
                    out$strongest_hr     <- hr_val
                    out$strongest_effect <- if (hr_val > 1) .("increased hazard")
                                            else .("decreased hazard")
                }
            }
            out$n_sig_vars <- length(unique(sig_vars))
            out
        }
        ,
        .generateSignificantPredictorsSummary = function(cox_table) {
            tryCatch({
                # This used to index cox_table[i, "p"] and
                # cox_table[i, "HR (95% CI, p-value)"] -- neither column exists
                # in a finalfit table, so the count was near-meaningless and
                # contradicted the natural-language summary on the same page.
                s <- private$.summariseCoxSignificance(cox_table)

                return(list(
                    significant_count = s$n_sig_vars,
                    total_predictors = s$n_total_vars,
                    strongest_predictor = s$strongest_label,
                    strongest_hr = if (is.na(s$strongest_hr)) 0 else s$strongest_hr,
                    strongest_effect = s$strongest_effect
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
                '<div style="background-color: rgba(33, 184, 255, 0.11); padding: 10px; border-radius: 5px; margin-top: 10px; color: inherit;">',
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
                    FALSE
                })

                # Assemble a model-specific natural-language summary. Predictor
                # display names come from user data labels, so they are HTML-
                # escaped before being placed into this type:Html output.
                cleaneddata <- private$.cleandata()
                predictors <- c(cleaneddata$myexplanatory_labelled,
                                cleaneddata$mycontexpl_labelled)
                predictors <- predictors[!is.na(predictors) & nzchar(predictors)]

                # Stratification variables are NOT predictors of this nomogram.
                # They are absorbed into separate baseline hazards and carry no
                # coefficient, so listing them told the reader to look for point
                # scales the nomogram does not and cannot have.
                strat_nom <- if (isTRUE(self$options$use_stratify))
                    cleaneddata$mystratvar_labelled else NULL
                strat_nom <- strat_nom[!is.na(strat_nom) & nzchar(strat_nom)]
                if (length(strat_nom) > 0)
                    predictors <- setdiff(predictors, strat_nom)

                pred_html <- if (length(predictors))
                    paste0("<li>", htmltools::htmlEscape(predictors), "</li>", collapse = "")
                else
                    paste0("<li>", .("(none specified)"), "</li>")

                strat_html <- if (length(strat_nom) > 0)
                    paste0("<p><b>", .("Stratification variables (not shown on the nomogram):"),
                           "</b> ", htmltools::htmlEscape(paste(strat_nom, collapse = ", ")),
                           ". ", .("These define separate baseline hazards and have no point scale, so the nomogram applies within a stratum."), "</p>")
                else ""

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
                else if (length(strat_nom) > 0)
                    # Deliberately withheld, not a failure -- distinguish the two
                    # so the reader does not go looking for a plot that should
                    # never appear for this model.
                    paste0("<p style='color:#b71c1c;'>", .("No nomogram is drawn for a stratified model: each stratum has its own baseline hazard, so a single point-to-risk scale would be wrong for patients outside whichever stratum it was drawn from."), "</p>")
                else
                    paste0("<p style='color:#b71c1c;'>", .("The nomogram plot could not be constructed for this model; the summary above still describes the fitted model."), "</p>")

                summary_html <- paste0(
                    "<div style='font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif; line-height: 1.6; max-width: 820px;'>",
                    "<p>", sprintf(.("This nomogram is a visual calculator derived from a multivariable Cox proportional-hazards model fitted on <b>%s patients</b> with <b>%s events</b>. It turns the model into a point-scoring tool so an individual patient's risk can be read off directly."),
                                   format(n_patients), format(n_events)), "</p>",
                    "<p><b>", .("Predictors included:"), "</b></p><ul>", pred_html, "</ul>",
                    strat_html,
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
                error_msg <- jmvcore::format(
                    .("Nomogram calculation error: {message}"),
                    message = htmltools::htmlEscape(conditionMessage(e)))
                self$results$nomogramSummary$setContent(error_msg)
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
            # coxph's $assign maps each term to the coefficients it owns, which
            # is what lets the summary count variables rather than contrasts.
            .tmap <- tryCatch({
              a <- cox_model$assign
              if (is.list(a) && length(a) > 0) a else NULL
            }, error = function(e) NULL)

            clinical_summary <- .generateClinicalSummary(
              results = cox_results,
              analysis_type = if (private$.isCompetingRisk()) "finegray" else "cox",
              n_vars = n_vars,
              n_events = n_events,
              term_map = .tmap
            )

            # Format for display
            summary_html <- paste0(
              "<div style='background-color: rgba(33, 144, 255, 0.11); border: 1px solid #b3d9ff; padding: 20px; border-radius: 8px; margin: 15px 0; color: inherit;'>",
              "<h3 style='color: #0056b3; margin-top: 0; margin-bottom: 15px;'> ", .("Clinical Summary"), "</h3>",
              "<div style='background-color: white; padding: 15px; border-radius: 5px; border-left: 4px solid #0056b3;'>",
              "<p style='font-size: 16px; line-height: 1.6; margin: 0;'>", clinical_summary$summary, "</p>",
              "</div>"
            )

            # Add study details
            if (n_vars > 0) {
              summary_html <- paste0(summary_html,
                "<div style='margin-top: 15px; padding: 10px; background-color: rgba(138, 155, 172, 0.06); border-radius: 5px; color: inherit;'>",
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
                "<div style='margin-top: 15px; padding: 10px; background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffeaa7; border-radius: 5px; color: inherit;'>",
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
