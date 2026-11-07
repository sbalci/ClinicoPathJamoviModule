#' Multivariable-survival interaction helpers
#'
#' Pure, harness-free helpers used by `multisurvivalClass` to build Cox
#' interaction terms and to summarise effect modification. Kept out of the R6
#' class so they can be unit-tested directly (see
#' tests/testthat/test-multisurvival-interactions.R). All depend only on
#' `survival`, `stats`, and `.escapeVariableNames()` (R/utils.R).
#'
#' @noRd

# Map interaction terms from jamovi display labels to real column names.
# `interactions`: list of character vectors (self$options$interactions).
# `all_labels`  : named list (names = real columns, values = display labels).
.mapInteractionTerms <- function(interactions, all_labels) {
  if (is.null(interactions) || length(interactions) == 0)
    return(list())

  # Accept a formula as well as the Terms list. `interactions = ~sex:age` is the
  # documented R interface, but the value can arrive here still carrying the
  # formula's tilde glued to the first variable -- list(c("~sex", "age")) -- which
  # then built the term `~sex`:age, matched no column, and left the interaction
  # and covariate-contribution tables silently empty.
  if (inherits(interactions, "formula"))
    interactions <- jmvcore::decomposeFormula(interactions)

  lapply(interactions, function(term) {
    # Strip a leading tilde and surrounding whitespace, and drop empty pieces.
    term <- trimws(sub("^\\s*~\\s*", "", as.character(term)))
    term <- term[nzchar(term)]
    vapply(term, function(component) {
      real <- names(all_labels)[match(component, all_labels)]
      if (length(real) == 0 || is.na(real)) component else real
    }, character(1), USE.NAMES = FALSE)
  })
}

# Escaped, colon-joined interaction terms for the Cox model formula RHS.
.interactionTermsForFormula <- function(real_terms) {
  if (length(real_terms) == 0) return(character(0))
  vapply(real_terms, function(components) {
    paste(.escapeVariableNames(components), collapse = ":")
  }, character(1), USE.NAMES = FALSE)
}

# Raw colon-joined interaction terms for finalfit `explanatory` vectors
# (main effects are passed to finalfit unescaped, so match that).
.interactionTermsForFinalfit <- function(real_terms) {
  if (length(real_terms) == 0) return(character(0))
  vapply(real_terms, paste, character(1), collapse = ":", USE.NAMES = FALSE)
}

# Describe a single interaction term: focal, moderator, whether it is 2-way,
# and whether the moderator is categorical. Convention: for term A:B the
# moderator is the SECOND variable (B). But subgroup HRs need a categorical
# moderator, so if the second variable is not categorical while the first is,
# swap them (categorical variable becomes the moderator) and set swapped=TRUE.
.interactionModeratorInfo <- function(real_term, data) {
  twoway <- length(real_term) == 2
  if (!twoway) {
    return(list(focal = real_term[1], moderator = NA_character_,
                twoway = FALSE, categorical_moderator = FALSE, swapped = FALSE))
  }
  v1 <- real_term[1]; v2 <- real_term[2]
  is_cat <- function(v) is.factor(data[[v]]) || is.character(data[[v]])
  cat1 <- is_cat(v1); cat2 <- is_cat(v2)
  if (cat2) {                     # second var categorical -> convention
    focal <- v1; moderator <- v2; swapped <- FALSE
  } else if (cat1) {              # only first is categorical -> swap
    focal <- v2; moderator <- v1; swapped <- TRUE
  } else {                        # neither categorical
    focal <- v1; moderator <- v2; swapped <- FALSE
  }
  list(focal = focal, moderator = moderator, twoway = twoway,
       categorical_moderator = is_cat(moderator), swapped = swapped)
}

# Interaction terms of a fitted model, by the formula's own term order. A ":" in a
# coefficient name proves nothing: a factor level can contain one.
.interactionTermNames <- function(cox_model) {
  tt <- stats::terms(cox_model)
  intersect(attr(tt, "term.labels")[attr(tt, "order") > 1L], names(cox_model$assign))
}

# Variables that interact with `term`: the rest of every fitted term containing it.
# `term` is a label of cox_model$assign. Returned without backticks.
.termPartners <- function(cox_model, term) {
  fac <- attr(stats::terms(cox_model), "factors")
  if (length(fac) == 0 || !term %in% colnames(fac)) return(character(0))
  vars_in <- function(tm) rownames(fac)[fac[, tm] > 0]
  own <- vars_in(term)
  others <- unlist(lapply(intersect(colnames(fac), names(cox_model$assign)), function(tm) {
    v <- vars_in(tm)
    if (all(own %in% v)) setdiff(v, own)
  }))
  unique(gsub("^`|`$", "", others))
}

# "age = 0", "grade = G1": the value at which a coefficient holds each partner. A
# factor sits at its reference level and a numeric at 0, whatever its range.
.conditionLabels <- function(partners, cox_model, data, display = NULL) {
  shown <- function(v) if (v %in% names(display)) unname(display[[v]]) else v
  vapply(partners, function(v) {
    lv <- cox_model$xlevels[[v]]
    if (is.null(lv) && (is.factor(data[[v]]) || is.character(data[[v]])))
      lv <- levels(factor(data[[v]]))
    paste(shown(v), "=", if (is.null(lv)) "0" else lv[1])
  }, character(1), USE.NAMES = FALSE)
}

# What each coefficient of an interaction model is conditional on.
#   coef      along coef(): "age = 0, site = X", or "" for a coefficient with no partner
#   terms     named by shown term: the same text for each conditioned term
#   outside   data.frame(var, low, high): numeric partners whose 0 no patient has
#   interaction_labels  interaction coefficient names, as finalfit labels them ("armB:age")
.coefConditions <- function(cox_model, data, display = NULL) {
  shown <- function(tm) {
    parts <- strsplit(tm, ":", fixed = TRUE)[[1]]
    parts <- gsub("^`|`$", "", parts)
    known <- parts %in% names(display)
    if (any(known)) parts[known] <- unname(display[parts[known]])
    paste(parts, collapse = ":")
  }
  asg <- cox_model$assign
  coef_at <- rep("", length(stats::coef(cox_model)))
  terms_at <- character(0)
  partners_all <- character(0)
  for (tm in names(asg)) {
    partners <- .termPartners(cox_model, tm)
    if (length(partners) == 0) next
    at <- paste(.conditionLabels(partners, cox_model, data, display), collapse = ", ")
    coef_at[asg[[tm]]] <- at
    terms_at[shown(tm)] <- at
    partners_all <- union(partners_all, partners)
  }
  numeric_partners <- partners_all[vapply(partners_all, function(v) is.numeric(data[[v]]), logical(1))]
  outside <- data.frame(var = character(0), low = numeric(0), high = numeric(0),
                        stringsAsFactors = FALSE)
  for (v in numeric_partners) {
    rng <- range(data[[v]], na.rm = TRUE)
    if (rng[1] > 0 || rng[2] < 0)
      outside[nrow(outside) + 1L, ] <- list(
        if (v %in% names(display)) unname(display[[v]]) else v, rng[1], rng[2])
  }
  interaction_rows <- unlist(asg[.interactionTermNames(cox_model)], use.names = FALSE)
  list(coef = coef_at, terms = terms_at, outside = outside,
       interaction_labels = gsub("`", "", names(stats::coef(cox_model))[interaction_rows], fixed = TRUE))
}

# One row per interaction coefficient: HR, CI, p.
.interactionTestTable <- function(cox_model, conf_level = 0.95) {
  sm <- summary(cox_model)$coefficients
  if (is.null(sm) || nrow(sm) == 0) return(NULL)
  cn <- rownames(sm)
  int_idx <- sort(unlist(cox_model$assign[.interactionTermNames(cox_model)], use.names = FALSE))
  if (length(int_idx) == 0) return(NULL)
  cis <- suppressWarnings(stats::confint(cox_model, level = conf_level))
  if (is.null(dim(cis))) cis <- matrix(cis, ncol = 2,
                                       dimnames = list(cn, NULL))
  data.frame(
    term     = cn[int_idx],
    hr       = exp(unname(sm[int_idx, "coef"])),
    ci_lower = exp(unname(cis[int_idx, 1])),
    ci_upper = exp(unname(cis[int_idx, 2])),
    p        = unname(sm[int_idx, "Pr(>|z|)"]),
    stringsAsFactors = FALSE
  )
}

# Joint (multi-df) Wald test for each interaction TERM.
#
# .interactionTestTable() emits one row per interaction COEFFICIENT, each with
# its own 1-df Wald p. When the moderator or the focal variable has more than two
# levels, a single term produces several coefficients, and no individual row
# answers "is there effect modification by this variable at all?".
#
# On a 3-level stage x 2-level treatment interaction the two rows came out at
# p = 0.077 and p = 0.726, which reads as a borderline interaction; the joint
# 2-df test over the same model is p = 0.154. Reporting only per-coefficient
# Wald tests for a multi-df term is a well-known way to over-read a subgroup
# effect, so the joint test is reported alongside them.
#
# Wald rather than a likelihood-ratio refit, deliberately:
#   * it needs only the fitted model (b and its covariance), so it cannot hit the
#     refit-environment failure documented in .compare_models(), and
#   * it is valid for the Fine-Gray fit too, where vcov() is the robust
#     covariance and a naive LRT on the weighted partial likelihood is not.
# Agreement is close: on the case above Wald gives chisq = 3.7426, df = 2,
# p = 0.1539 against the LRT's 3.7466 / 2 / 0.1536.
#
# Returns a data.frame(term, chisq, df, p) with one row per interaction term
# that contributes MORE THAN ONE coefficient, or NULL when there is none (a
# 1-df term's joint test is identical to the row already shown).
.interactionJointTests <- function(cox_model) {
  asg <- cox_model$assign
  if (!is.list(asg) || length(asg) == 0) return(NULL)

  b <- stats::coef(cox_model)
  V <- try(stats::vcov(cox_model), silent = TRUE)
  if (inherits(V, "try-error") || is.null(V) || is.null(b)) return(NULL)

  int_terms <- .interactionTermNames(cox_model)
  rows <- list()
  for (tm in int_terms) {
    idx <- asg[[tm]]
    if (any(idx > length(b))) next
    # An empty cross-cell aliases one coefficient; the rest still have a joint test,
    # on fewer df.
    idx <- idx[!is.na(b[idx])]
    # A single-coefficient term adds nothing beyond its own row.
    if (length(idx) < 2) next

    Vs <- V[idx, idx, drop = FALSE]
    chi <- try(as.numeric(t(b[idx]) %*% solve(Vs) %*% b[idx]), silent = TRUE)
    # A singular sub-covariance means the remaining coefficients are still aliased;
    # report nothing rather than a fabricated chi-square.
    if (inherits(chi, "try-error") || !is.finite(chi) || chi < 0) next

    rows[[length(rows) + 1]] <- data.frame(
      term  = tm,
      chisq = chi,
      df    = length(idx),
      p     = stats::pchisq(chi, df = length(idx), lower.tail = FALSE),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# Within-subgroup HRs for a 2-way interaction with a CATEGORICAL moderator.
# Relevels `moderator` to each level, refits the SAME full model, and reads the
# focal main-effect coefficient(s) - i.e. the fully-adjusted focal effect within
# that subgroup. Focal may be categorical (one row per non-reference level) or
# continuous (single "per unit" row). Returns NULL if moderator is not a usable
# factor. Reference focal level yields no row (no contrast).
.computeSubgroupHRs <- function(cox_formula, data, focal, moderator,
                                conf_level = 0.95) {
  modvec <- data[[moderator]]
  if (!is.factor(modvec)) {
    if (is.character(modvec)) modvec <- factor(modvec) else return(NULL)
  }
  # Clean the moderator ONCE: unordered (relevel rejects ordered factors) and
  # dropping declared-but-unobserved levels (relevel rejects a zero-count ref),
  # while preserving the declared factor level ordering.
  # Treatment-contrast subgroup HRs are inherently about observed discrete
  # levels, so coercing an ordered moderator to unordered is correct here.
  modvec <- factor(modvec, ordered = FALSE)
  data[[moderator]] <- modvec
  mod_levels <- levels(modvec)
  if (length(mod_levels) < 2) return(NULL)

  # Reconstruct focal coefficient names exactly (avoids prefix collisions).
  # Escape the focal name the same way the formula builder did, so it matches
  # coxph's backticked coefficient names for non-syntactic names (spaces etc.).
  fvec <- data[[focal]]
  focal_is_cat <- is.factor(fvec) || is.character(fvec)
  if (focal_is_cat) {
    flev <- if (is.factor(fvec)) levels(droplevels(fvec)) else levels(factor(fvec))
    focal_coef_names <- paste0(.escapeVariableNames(focal), flev[-1])  # <var><level>
    focal_effect_lab <- flev[-1]
  } else {
    focal_coef_names <- .escapeVariableNames(focal)
    focal_effect_lab <- focal
  }

  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  rows <- list()
  for (b in mod_levels) {
    d2 <- data
    d2[[moderator]] <- stats::relevel(d2[[moderator]], ref = b)
    # Capture warnings via an environment flag instead of `<<-`.
    warn_env <- new.env(parent = emptyenv())
    warn_env$warned <- FALSE
    at_level <- !is.na(modvec) & modvec == b
    fit <- tryCatch(
      withCallingHandlers(
        survival::coxph(cox_formula, data = d2),
        warning = function(w) { warn_env$warned <- TRUE; invokeRestart("muffleWarning") }
      ),
      error = function(e) NULL
    )
    warned <- warn_env$warned
    if (is.null(fit)) next
    sm <- summary(fit)$coefficients
    for (i in seq_along(focal_coef_names)) {
      nm <- focal_coef_names[i]
      if (!nm %in% rownames(sm)) next
      # This level needs the focal contrast among its own patients. Without it the
      # model aliases another coefficient, and `nm` then carries a different level's
      # effect: a real-looking number with no NA anywhere to give it away.
      estimable <- if (focal_is_cat) {
        all(flev[c(1L, i + 1L)] %in% as.character(fvec[at_level]))
      } else {
        length(unique(stats::na.omit(fvec[at_level]))) > 1L
      }
      est <- if (estimable) sm[nm, "coef"] else NA_real_
      se <- sm[nm, "se(coef)"]
      rows[[length(rows) + 1]] <- data.frame(
        interaction     = paste0(focal, " * ", moderator),
        moderator_level = b,
        focal_effect    = focal_effect_lab[i],
        hr              = exp(est),
        ci_lower        = exp(est - z * se),
        ci_upper        = exp(est + z * se),
        p               = if (estimable) sm[nm, "Pr(>|z|)"] else NA_real_,
        converged       = !warned || !estimable,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}
