# Run from the repository root: Rscript development-scripts/review_survivalPower_cases.R
# Review evidence, not a passing regression suite: differences are deliberately retained.
source("R/survivalPower.h.R")
source("R/survivalPower_distributions.R")
source("R/survivalPower.b.R")
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else "development-ideas/survivalPower-review-2026-09-13"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
make <- function(...) {
  analysis <- survivalPowerClass$new(
    options = survivalPowerOptions$new(...), data = data.frame()
  )
  analysis$run()
  analysis
}
private_of <- function(analysis) analysis$.__enclos_env__$private
save_json <- function(value, filename) {
  jsonlite::write_json(value, file.path(out, filename), pretty = TRUE,
                       auto_unbox = TRUE, digits = 12)
}

# Independent uniform-entry integration. Defaults: median 12 months, accrual 24,
# additional follow-up 12, annual dropout probability 0.05, equal arm sizes.
event_probability <- function(hr) {
  lambda <- log(2) / 12 * hr
  eta <- -log1p(-0.05) / 12
  integrate(function(entry) {
    lambda / (lambda + eta) * (-expm1(-(lambda + eta) * (36 - entry)))
  }, 0, 24)$value / 24
}
multi <- list()
for (arms in c(3, 10)) {
  for (hr in c(0.1, 0.75, 1.5)) {
    analysis <- make(study_design = "multi_arm", number_of_arms = arms,
                     analysis_type = "power", sample_size_input = 1000,
                     effect_size = hr, multiple_comparisons = "bonferroni")
    design <- private_of(analysis)$.resolved_design()
    multi[[length(multi) + 1L]] <- data.frame(
      arms = arms, hr = hr, reported_whole = design$events,
      integrated_whole = 1000 / arms *
        (event_probability(1) + (arms - 1) * event_probability(hr)),
      reported_comparison = design$comparison_events,
      integrated_comparison = 1000 / arms *
        (event_probability(1) + event_probability(hr))
    )
  }
}
write.csv(do.call(rbind, multi), file.path(out, "multiarm-events.csv"), row.names = FALSE)

# Public effect-size table: event counts must use the solved HR and all actual arms.
multi_effect <- lapply(c(3, 10), function(arms) {
  analysis <- make(study_design = "multi_arm", number_of_arms = arms,
                   analysis_type = "effect_size", sample_size_input = 1000,
                   multiple_comparisons = "bonferroni")
  design <- private_of(analysis)$.resolved_design()
  list(
    arms = arms, solved_hr = design$hr, reported_whole = design$events,
    integrated_whole = 1000 / arms *
      (event_probability(1) + (arms - 1) * event_probability(design$hr)),
    table = analysis$results$effect_size_results$asDF
  )
})
save_json(multi_effect, "multiarm-effect-table.json")

ni_cases <- lapply(c(10, 200), function(n) {
  analysis <- make(test_type = "non_inferiority", analysis_type = "power",
                   sample_size_input = n, effect_size = 1, alpha_level = 0.025,
                   show_interpretation = TRUE, show_summary = TRUE)
  list(n = n, power = private_of(analysis)$primary_numbers$power,
       ni_table = analysis$results$non_inferiority_table$asDF,
       regulatory = analysis$results$regulatory_table$asDF,
       notices = analysis$results$notices$content)
})
save_json(ni_cases, "ni-low-power.json")

ni_start <- lapply(c(1, 1.5), function(hr) {
  analysis <- make(test_type = "non_inferiority", analysis_type = "effect_size",
                   sample_size_input = 1000, effect_size = hr, alpha_level = 0.025)
  list(input_hr = hr, numbers = private_of(analysis)$primary_numbers,
       notices = analysis$results$notices$content)
})
save_json(ni_start, "ni-effect-starting-value.json")

# Under the null, two-sided rejection means any |Z_i| exceeds z, with rho = 0.5
# for equal allocation and equal hazards. Conditional on a common N(0,1) factor,
# the Z_i are independent. One-dimensional quadrature is fast and deterministic;
# avoid high-dimensional Miwa integration, which is very slow for nine contrasts.
family <- list()
withr::with_seed(713, {
  for (arms in c(3, 5, 10)) {
    for (alpha in c(0.05, 0.1)) {
      analysis <- make(study_design = "multi_arm", number_of_arms = arms,
                       analysis_type = "power", sample_size_input = 1000,
                       effect_size = 1, multiple_comparisons = "bonferroni",
                       alpha_level = alpha)
      comparisons <- arms - 1
      alpha_pair <- alpha / comparisons
      z <- qnorm(1 - alpha_pair / 2)
      nonrejection <- integrate(function(u) {
        conditional <- pnorm((z - sqrt(0.5) * u) / sqrt(0.5)) -
          pnorm((-z - sqrt(0.5) * u) / sqrt(0.5))
        dnorm(u) * conditional^comparisons
      }, -Inf, Inf, rel.tol = 1e-10)
      pairwise <- private_of(analysis)$primary_numbers$power
      actual <- private_of(analysis)$.disjunctive_power(
        0, z, comparisons, 0.5
      )
      family[[length(family) + 1L]] <- data.frame(
        arms = arms, alpha = alpha, pairwise = pairwise, reported = actual,
        rectangle_probability = 1 - nonrejection$value,
        integration_error = nonrejection$abs.error,
        displayed = analysis$results$multi_arm_table$asDF$total_study_power[1] / 100
      )
    }
  }
})
write.csv(do.call(rbind, family), file.path(out, "multiarm-two-sided-power.csv"),
          row.names = FALSE)

rng_preserved <- withr::with_seed(345, {
  before <- .Random.seed
  invisible(make(study_design = "multi_arm", number_of_arms = 5))
  identical(before, .Random.seed)
})
benchmarks <- lapply(list(
  list(),
  list(test_type = "cox_regression"),
  list(test_type = "cox_regression", interim_analyses = 5, alpha_spending = "pocock"),
  list(test_type = "cox_regression", interim_analyses = 5,
       alpha_spending = "pocock", sensitivity_analysis = TRUE),
  list(run_simulation_validation = TRUE, simulation_runs = 2000)
), function(options) {
  elapsed <- system.time(analysis <- do.call(make, options))["elapsed"]
  list(options = options, elapsed_seconds = unname(elapsed),
       primary = private_of(analysis)$primary_numbers)
})
save_json(list(benchmarks = benchmarks, multiarm_rng_preserved = rng_preserved),
          "performance.json")
save_json(list(
  R = R.version.string,
  packages = setNames(lapply(c("jmvcore", "gsDesign", "mvtnorm", "survival", "testthat"),
                            function(x) as.character(packageVersion(x))),
                      c("jmvcore", "gsDesign", "mvtnorm", "survival", "testthat"))
), "runtime.json")
cat("Independent review cases complete; discrepancies are recorded, not asserted away.\n")
