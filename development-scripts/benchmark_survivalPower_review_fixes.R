source('R/survivalPower.h.R')
source('R/survivalPower_distributions.R')
source('R/survivalPower.b.R')
current <- survivalPowerClass
old_environment <- new.env(parent = globalenv())
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop('Supply the timestamped backup directory as the first argument.')
backup <- args[[1]]
source(file.path(backup, 'R/survivalPower.b.R.bak'), local = old_environment)
baseline <- old_environment$survivalPowerClass
make <- function(generator, options) {
  analysis <- generator$new(options = do.call(survivalPowerOptions$new, options), data = data.frame())
  analysis$run()
  analysis
}
invisible(make(baseline, list()))
invisible(make(current, list()))
scenarios <- list(default = list(), fixed_cox = list(test_type = 'cox_regression'),
  sequential_cox = list(test_type = 'cox_regression', interim_analyses = 5,
                        alpha_spending = 'pocock'),
  sequential_cox_sensitivity = list(test_type = 'cox_regression', interim_analyses = 5,
                        alpha_spending = 'pocock', sensitivity_analysis = TRUE))
rows <- list()
for (scenario in names(scenarios)) for (iteration in seq_len(3)) {
  for (version in c('baseline','current')) {
    generator <- if (version == 'baseline') baseline else current
    elapsed <- system.time(a <- make(generator, scenarios[[scenario]]))['elapsed']
    primary <- a$.__enclos_env__$private$primary_numbers
    rows[[length(rows) + 1L]] <- data.frame(scenario, iteration, version,
      elapsed = unname(elapsed), n = primary$n, events = primary$events)
  }
  cat(scenario, iteration, 'complete\n')
}
results <- do.call(rbind, rows)
write.csv(results, 'development-ideas/survivalPower-review-fixes-2026-09-13/paired-benchmarks.csv', row.names = FALSE)
print(aggregate(elapsed ~ scenario + version, results, median))
for (scenario in names(scenarios)) {
  x <- results[results$scenario == scenario, , drop = FALSE]
  stopifnot(length(unique(x$n)) == 1L, length(unique(x$events)) == 1L)
}
