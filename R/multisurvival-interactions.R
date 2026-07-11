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
  lapply(interactions, function(term) {
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

# One row per interaction coefficient (name contains ":"): HR, CI, p.
.interactionTestTable <- function(cox_model, conf_level = 0.95) {
  sm <- summary(cox_model)$coefficients
  if (is.null(sm) || nrow(sm) == 0) return(NULL)
  cn <- rownames(sm)
  int_idx <- which(grepl(":", cn))
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
  # dropping declared-but-unobserved levels (relevel rejects a zero-count ref).
  # Treatment-contrast subgroup HRs are inherently about observed discrete
  # levels, so coercing an ordered moderator to unordered is correct here.
  modvec <- droplevels(factor(as.character(modvec)))
  data[[moderator]] <- modvec
  mod_levels <- levels(modvec)
  if (length(mod_levels) < 2) return(NULL)

  # Reconstruct focal coefficient names exactly (avoids prefix collisions).
  # Escape the focal name the same way the formula builder did, so it matches
  # coxph's backticked coefficient names for non-syntactic names (spaces etc.).
  fvec <- data[[focal]]
  focal_is_cat <- is.factor(fvec) || is.character(fvec)
  if (focal_is_cat) {
    flev <- levels(factor(fvec))
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
      est <- sm[nm, "coef"]; se <- sm[nm, "se(coef)"]
      rows[[length(rows) + 1]] <- data.frame(
        interaction     = paste0(focal, " * ", moderator),
        moderator_level = b,
        focal_effect    = focal_effect_lab[i],
        hr              = exp(est),
        ci_lower        = exp(est - z * se),
        ci_upper        = exp(est + z * se),
        p               = sm[nm, "Pr(>|z|)"],
        converged       = !warned,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}
