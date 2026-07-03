#!/usr/bin/env Rscript
# Validation for multisurvival interaction terms.
# Generates a Treatment x Biomarker predictive-biomarker dataset with a KNOWN
# interaction, then checks that the module's interaction pathway reproduces a
# direct survival::coxph(Surv ~ arm * bio + age) fit, and that the
# relevel-and-refit subgroup HRs match manual releveling.
#
# Sourcing note: this deliberately does NOT use devtools::load_all(".") — the
# installed jmvcore is CRAN 2.7.7, which lacks asFormula(), so
# .buildSurvivalFormula() -> .asSurvivalFormula() -> jmvcore::asFormula would
# fail under load_all() (a .GlobalEnv shim cannot override a namespace-loaded
# function). Instead we source() the two self-contained helper files directly
# (mirrors tests/testthat/test-multisurvival-interactions.R lines 1-50) and
# install the SAME conditional shim, which .buildSurvivalFormula() picks up
# via lexical scoping since it too is sourced into .GlobalEnv. On a real
# jamovi jmvcore (>= 2.7.12, which has asFormula), the shim is inert.
#
# Usage:  Rscript tests/validate_multisurvival_interactions.R
suppressMessages({
  library(survival)
  library(jmvcore)
  library(labelled)
})

# ---- Resolve repo root and source helpers (no devtools::load_all) ---------
.find_root <- function(start) {
  d <- suppressWarnings(normalizePath(start, mustWork = FALSE))
  for (i in seq_len(8)) {
    if (file.exists(file.path(d, "R", "utils.R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  NA_character_
}

.root <- .find_root(getwd())
if (is.na(.root)) .root <- .find_root(file.path(dirname(dirname(getwd()))))
if (is.na(.root)) stop("Could not locate repo root (marker file R/utils.R not found)")

source(file.path(.root, "R", "utils.R"))
source(file.path(.root, "R", "multisurvival-interactions.R"))

# Local-only verification shim (does NOT touch production code): see
# tests/testthat/test-multisurvival-interactions.R lines 34-47 for the full
# rationale. CRAN jmvcore (2.7.7) lacks asFormula(); jamovi's bundled jmvcore
# (>= 2.7.12) has it. `.buildSurvivalFormula()` resolves `.asSurvivalFormula`
# by lexical scoping in .GlobalEnv (both are sourced here), so this shim is
# picked up transparently when running outside the jamovi app.
if (!exists("asFormula", envir = asNamespace("jmvcore"), inherits = FALSE)) {
  assign(".asSurvivalFormula", function(x) stats::as.formula(x), envir = .GlobalEnv)
}

set.seed(20260703)
n <- 800
arm <- factor(sample(c("control", "treatment"), n, TRUE))
bio <- factor(sample(c("neg", "pos"), n, TRUE))
age <- round(rnorm(n, 60, 10), 1)
# True model: treatment helps only biomarker-positive patients (interaction).
lp <- -0.15 * (arm == "treatment") + 0.10 * (bio == "pos") +
      -0.90 * (arm == "treatment" & bio == "pos") + 0.02 * (age - 60)
time <- rexp(n, rate = exp(lp))
status <- rbinom(n, 1, 0.7)
d <- data.frame(mytime = time, myoutcome = status, arm = arm, bio = bio, age = age)
labelled::var_label(d) <- list(mytime = "Time", myoutcome = "Event",
                               arm = "Treatment Arm", bio = "Biomarker", age = "Age")
all_labels <- labelled::var_label(d)

cat("=== multisurvival interaction-pathway validation ===\n")
cat(sprintf("n = %d, events = %d\n\n", n, sum(status)))

# --- 1. Module pathway: map display labels -> real, build formula, fit -----
interactions <- list(c("Treatment Arm", "Biomarker"))   # as jamovi delivers
real_int <- .mapInteractionTerms(interactions, all_labels)
stopifnot(identical(real_int, list(c("arm", "bio"))))
cat("OK: .mapInteractionTerms(list(c('Treatment Arm','Biomarker')), all_labels) == list(c('arm','bio'))\n")

int_cox <- .interactionTermsForFormula(real_int)
f_mod <- .buildSurvivalFormula("mytime", "myoutcome",
                               predictors = c("arm", "bio", "age"),
                               interaction_terms = int_cox)
fit_mod <- survival::coxph(f_mod, data = d)

# --- Ground truth: direct coxph --------------------------------------------
fit_ref <- survival::coxph(Surv(mytime, myoutcome) ~ arm * bio + age, data = d)

stopifnot(all.equal(unname(coef(fit_mod)), unname(coef(fit_ref)), tolerance = 1e-8))
cat("OK: module formula reproduces coxph(Surv ~ arm*bio + age) coefficients exactly\n")

# --- 2. Interaction test table ----------------------------------------------
itab <- .interactionTestTable(fit_mod, conf_level = 0.95)
sm <- summary(fit_ref)$coefficients
int_name <- rownames(sm)[grepl(":", rownames(sm))][1]
stopifnot(abs(itab$hr[grepl(":", itab$term)][1] - exp(sm[int_name, "coef"])) < 1e-8)
cat(sprintf("OK: interaction HR = %.3f (p = %.4f) matches exp(coef) of '%s' from reference fit\n",
            itab$hr[grepl(":", itab$term)][1], itab$p[grepl(":", itab$term)][1], int_name))

# --- 3. Within-subgroup HRs vs manual relevel-refit -------------------------
sub <- .computeSubgroupHRs(f_mod, d, focal = "arm", moderator = "bio")
d_pos <- d; d_pos$bio <- relevel(d_pos$bio, ref = "pos")
hr_pos <- unname(exp(coef(survival::coxph(f_mod, data = d_pos))["armtreatment"]))
got_pos <- sub$hr[sub$moderator_level == "pos"]
stopifnot(abs(got_pos - hr_pos) < 1e-6)
cat(sprintf("OK: subgroup HR (bio=pos) = %.3f matches manual relevel-refit (%.3f)\n", got_pos, hr_pos))

cat("\nALL INTERACTION VALIDATIONS PASSED\n")
