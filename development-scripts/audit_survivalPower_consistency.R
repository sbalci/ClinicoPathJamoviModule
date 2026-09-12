# Report-only consistency probes; run from the package root.
source('R/survivalPower.h.R')
source('R/survivalPower_distributions.R')
source('R/survivalPower.b.R')
out <- 'development-ideas/survivalPower-check-full-2026-09-12'
warnings_seen <- list()
run_case <- function(label, ...) {
  a <- survivalPowerClass$new(options=survivalPowerOptions$new(...), data=data.frame())
  w <- character()
  withCallingHandlers(a$run(), warning=function(e) {
    w <<- c(w, conditionMessage(e))
    invokeRestart('muffleWarning')
  })
  warnings_seen[[label]] <<- w
  a
}

cox <- list()
for (hr in c(0.5, 0.75, 1.5)) {
  for (ratio in c(0.5, 1, 2, 5)) {
    a <- run_case(paste('cox', hr, ratio), test_type='cox_regression',
      effect_size=hr, allocation_ratio=ratio, sensitivity_analysis=TRUE)
    p <- a$.__enclos_env__$private
    curve <- a$results$sample_size_plot$state$data
    power_curve <- a$results$power_curve_plot$state$data
    curve_n <- curve$sample_size[abs(curve$hazard_ratio-hr)<1e-8]
    curve_p <- power_curve$power[abs(power_curve$hazard_ratio-hr)<1e-8]
    n <- p$primary_numbers$n
    b <- run_case(paste('cox_power', hr, ratio), test_type='cox_regression',
      effect_size=hr, allocation_ratio=ratio, analysis_type='power', sample_size_input=n)
    upstream <- gsDesign::gsSurvPower(k=1, test.type=1, sided=1, alpha=0.025,
      lambdaC=log(2)/12, hr=hr, eta=-log1p(-0.05)/12, ratio=1/ratio,
      gamma=n/24, R=24, minfup=12, plannedCalendarTime=36)
    cox[[length(cox)+1]] <- data.frame(hr=hr, ratio=ratio, headline_n=n,
      curve_n=curve_n, curve_power=curve_p,
      power_mode=b$.__enclos_env__$private$primary_numbers$power,
      upstream_power=upstream$power, method=upstream$method)
  }
}
write.csv(do.call(rbind, cox), file.path(out, 'cox-consistency.csv'), row.names=FALSE)

sensitivity <- lapply(c('effect_size', 'duration'), function(mode) {
  a <- run_case(mode, analysis_type=mode, sample_size_input=500L, sensitivity_analysis=TRUE)
  list(mode=mode, table=a$results$sensitivity_analysis_table$asDF,
       curve=a$results$sensitivity_plot$state$data,
       notices=a$results$notices$content)
})
jsonlite::write_json(sensitivity, file.path(out, 'sensitivity-modes.json'),
  auto_unbox=TRUE, pretty=TRUE, na='null')

# Compare the local approximation with BOTH boundaries of a symmetric test.
# A comparison with only an upper bound would omit valid opposite-tail rejection.
sequential <- list()
for (spending in c('obrien_fleming', 'pocock')) {
  for (looks in c(1L, 5L)) {
    for (n in c(10L, 200L, 1000L)) {
      a <- run_case(paste(spending, looks, n), analysis_type='power',
        sample_size_input=n, interim_analyses=looks, alpha_spending=spending)
      p <- a$.__enclos_env__$private
      d <- p$.gs_info_design()
      E <- p$primary_numbers$events
      prob <- gsDesign::gsProbability(k=looks+1, theta=abs(log(0.75))*0.5,
        n.I=E*d$timing, a=-d$upper$bound, b=d$upper$bound)
      sequential[[length(sequential)+1]] <- data.frame(spending=spending,
        interims=looks, n=n, local=p$primary_numbers$power,
        both_boundaries=sum(prob$upper$prob[,1])+sum(prob$lower$prob[,1]))
    }
  }
}
write.csv(do.call(rbind, sequential), file.path(out, 'sequential-two-sided.csv'), row.names=FALSE)
jsonlite::write_json(warnings_seen, file.path(out, 'runtime-warnings.json'),
  auto_unbox=TRUE, pretty=TRUE)
cat('Wrote Cox, sensitivity, sequential and warning evidence.\n')
