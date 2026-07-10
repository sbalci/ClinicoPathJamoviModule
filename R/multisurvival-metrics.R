#' Multivariable-survival discrimination-metric helpers
#'
#' Pure, harness-free helpers used by `multisurvivalClass` to compute
#' optimism-corrected model discrimination (Harrell's C-index). Kept out of the
#' R6 class so they can be unit-tested directly (see
#' tests/testthat/test-multisurvival-validation.R). Depend only on `survival`
#' and `stats`.
#'
#' @noRd

# Bootstrap optimism-corrected Harrell's C-index for a fitted Cox model.
#
# Implements Harrell's standard optimism bootstrap (Regression Modeling
# Strategies, 2nd ed., section 5.3):
#   1. apparent C  = C of the final model on the original data.
#   2. for each of B bootstrap resamples:
#        - refit the model on the resample;
#        - C_train = C of that refit on the resample (optimistic);
#        - C_test  = C of that refit evaluated on the ORIGINAL data;
#        - optimism_b = C_train - C_test.
#   3. corrected C = apparent C - mean(optimism_b).
#
# Concordance definition is kept consistent across all three quantities by
# using the fitted model's OWN concordance everywhere:
#   - apparent  = survival::concordance(cox_model)
#   - C_train   = survival::concordance(fit_b)                 (in-sample resample)
#   - C_test    = survival::concordance(fit_b, newdata = data) (out-of-sample)
# concordance(coxph) returns Harrell's C with the correct Cox orientation and
# honours strata() (within-stratum comparable pairs), so no manual
# `Surv ~ lp, reverse = TRUE` is needed. Using the model object for C_test also
# means stratified and unstratified models are corrected on the same footing
# (a manual global-concordance C_test would over-correct stratified models).
#
# `cox_model` : a fitted survival::coxph object (NOT a Fine-Gray/weighted fit;
#               callers must guard competing-risk models out).
# `data`      : the model data frame used to fit `cox_model`; must contain the
#               response (mytime + outcome), every covariate, and any strata
#               referenced by the formula.
# `status`    : numeric/logical event indicator aligned row-for-row with `data`
#               (used only for the minimum-events guard).
# `B`         : number of bootstrap resamples.
# `seed`      : RNG seed; the global RNG state is saved and restored so the
#               analysis remains reproducible without disturbing other draws.
#
# Returns a list(apparent, optimism, corrected, B, n_boot) or NULL when the
# correction cannot be computed reliably (too few valid resamples, degenerate
# model, missing package state).
.multisurvivalOptimismCIndex <- function(cox_model, data, status,
                                         B = 150, seed = 1234) {
  if (is.null(cox_model) || is.null(data) || nrow(data) < 10)
    return(NULL)
  B <- as.integer(B)
  if (is.na(B) || B < 20) B <- 20L

  form <- tryCatch(stats::formula(cox_model), error = function(e) NULL)
  if (is.null(form)) return(NULL)

  status <- as.integer(status)
  status[is.na(status)] <- 0L
  if (sum(status) < 10) return(NULL)

  apparent <- tryCatch(
    survival::concordance(cox_model)$concordance,
    error = function(e) NA_real_)
  if (!is.finite(apparent)) return(NULL)

  # Save and restore the global RNG so the bootstrap is reproducible without
  # perturbing any downstream randomness in the same session.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)

  n <- nrow(data)
  optimism <- rep(NA_real_, B)
  for (b in seq_len(B)) {
    idx <- sample.int(n, n, replace = TRUE)
    boot <- data[idx, , drop = FALSE]
    # model = TRUE makes the refit self-contained: survival::concordance()
    # (esp. for strata() models) otherwise re-evaluates fit_b's stored call
    # `data = boot`, but `boot` is local to this function and invisible there,
    # yielding "object 'boot' not found". Storing the model frame avoids that.
    fit_b <- tryCatch(survival::coxph(form, data = boot, model = TRUE),
                      error = function(e) NULL, warning = function(w) NULL)
    if (is.null(fit_b) || is.null(stats::coef(fit_b)) ||
        any(!is.finite(stats::coef(fit_b))))
      next
    c_train <- tryCatch(survival::concordance(fit_b)$concordance,
                        error = function(e) NA_real_)
    # Evaluate the resample model on the ORIGINAL data using the model's OWN
    # concordance definition. survival::concordance(fit_b, newdata=) respects
    # the fitted model structure -- including strata() (within-stratum pairs)
    # and the Cox orientation -- so C_test is consistent with the apparent /
    # C_train definition and needs no manual Surv ~ lp + reverse=TRUE. (For an
    # unstratified model the two are identical; for a stratified model the
    # manual global-concordance form would over-correct.)
    c_test <- tryCatch(
      survival::concordance(fit_b, newdata = data)$concordance,
      error = function(e) NA_real_)
    if (is.finite(c_train) && is.finite(c_test))
      optimism[b] <- c_train - c_test
  }

  valid <- optimism[is.finite(optimism)]
  # Require at least half of the requested resamples to have produced a usable
  # optimism estimate; otherwise the correction is not trustworthy.
  if (length(valid) < max(10L, floor(B / 2)))
    return(NULL)

  opt <- mean(valid)
  corrected <- apparent - opt
  # C-index is bounded to [0, 1]; clamp to guard against bootstrap noise.
  corrected <- max(0, min(1, corrected))

  list(apparent = apparent, optimism = opt, corrected = corrected,
       B = B, n_boot = length(valid))
}
