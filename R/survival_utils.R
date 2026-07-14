# ============================================================================
# CLINICOPATH SURVIVAL UTILITY FUNCTIONS
# ============================================================================
# Shared helper functions specifically for survival and time-to-event analyses.

#' Build a survival formula safely via jmvcore::asFormula
#'
#' Wraps `jmvcore::asFormula` with the function allow-list extended to cover
#' common survival modelling helpers (`Surv`, `strata`, `cluster`, `frailty`,
#' `tt`, `pspline`, `ns`, `bs`, `I`, `const`, `finegray`). Use this instead of
#' base `stats::as.formula()` in survival / Cox / Fine-Gray paths so the formula
#' goes through jmvcore's allow-listed parser.
#'
#' @param x A character formula string (e.g. `"survival::Surv(t, d) ~ x"`).
#' @return A parsed formula object.
#' @keywords internal
.asSurvivalFormula <- function(x) {
    jmvcore::asFormula(
        x,
        additional_allowed_functions = c(
            "Surv", "strata", "cluster", "frailty", "tt",
            "pspline", "ns", "bs", "I", "const", "finegray"
        )
    )
}

#' Build a survival model formula from variable names
#'
#' Consolidated helper used across the survival-analysis backends (e.g.
#' `multisurvival.b.R`) to assemble a `survival::Surv(...) ~ ...` formula
#' from raw variable names, with safe escaping of non-syntactic names via
#' `.escapeVariableNames()` and safe parsing via `.asSurvivalFormula()`.
#'
#' @param time_var Character. Time variable name (or start time for
#'   `"standard"`/`"interval"` types).
#' @param outcome_var Character. Event/outcome variable name.
#' @param predictors Character vector of predictor (main-effect) variable
#'   names.
#' @param survival_type One of `"standard"`, `"counting"`, `"interval"`.
#' @param start_var Character. Start-time variable name (required when
#'   `survival_type = "counting"`).
#' @param stop_var Character. Stop-time variable name (required when
#'   `survival_type` is `"counting"` or `"interval"`).
#' @param strata_vars Character vector of stratification variable names.
#' @param interaction_terms Character vector of already-escaped, `:`-joined
#'   interaction terms (e.g. `` "`Arm`:`Bio`" ``) appended to the right-hand
#'   side after main effects and before strata.
#' @return A parsed formula object (see `.asSurvivalFormula()`).
#' @keywords internal
.buildSurvivalFormula <- function(time_var, outcome_var, predictors, survival_type = "standard", start_var = NULL, stop_var = NULL, strata_vars = NULL, interaction_terms = NULL) {
  # Escape all variable names for safe formula construction
  escaped_time <- .escapeVariableNames(time_var)
  escaped_outcome <- .escapeVariableNames(outcome_var)
  escaped_predictors <- .escapeVariableNames(predictors)

  # Build left-hand side based on survival type
  lhs <- switch(survival_type,
    "standard" = paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ")"),
    "counting" = {
      if (is.null(start_var) || is.null(stop_var)) {
        jmvcore::reject("Start and stop variables required for counting process format")
      }
      escaped_start <- .escapeVariableNames(start_var)
      escaped_stop <- .escapeVariableNames(stop_var)
      paste0("survival::Surv(", escaped_start, ", ", escaped_stop, ", ", escaped_outcome, ")")
    },
    "interval" = {
      if (is.null(stop_var)) {
        jmvcore::reject("Stop time variable required for interval censoring")
      }
      escaped_stop <- .escapeVariableNames(stop_var)
      paste0("survival::Surv(", escaped_time, ", ", escaped_stop, ", ", escaped_outcome, ")")
    },
    jmvcore::reject("Unknown survival type: ", survival_type)
  )

  # Build right-hand side: main effects + interaction terms (already escaped)
  main_terms <- if (length(escaped_predictors) == 0) character(0) else escaped_predictors
  int_terms  <- if (length(interaction_terms) == 0) character(0) else interaction_terms
  rhs_terms  <- c(main_terms, int_terms)

  if (length(rhs_terms) == 0) {
    rhs <- "1"  # Null model
  } else {
    rhs <- paste(rhs_terms, collapse = " + ")
  }

  # Add stratification if specified (applies whether or not predictors exist)
  if (!is.null(strata_vars) && length(strata_vars) > 0) {
    escaped_strata <- .escapeVariableNames(strata_vars)
    strata_term <- paste0("strata(", paste(escaped_strata, collapse = ", "), ")")
    rhs <- if (identical(rhs, "1")) strata_term else paste(rhs, strata_term, sep = " + ")
  }

  formula_string <- paste0(lhs, " ~ ", rhs)
  return(.asSurvivalFormula(formula_string))
}
