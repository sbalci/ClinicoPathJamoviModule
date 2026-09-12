# Independent report-only reproductions for the full audit; run from package root.
source('R/survivalPower.h.R')
source('R/survivalPower_distributions.R')
source('R/survivalPower.b.R')
out <- 'development-ideas/survivalPower-check-full-2026-09-12'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
sp_case <- function(...) {
  a <- survivalPowerClass$new(options=survivalPowerOptions$new(...),data=data.frame())
  a$run()
  a
}
record <- function(a) list(numbers=a$.__enclos_env__$private$primary_numbers,
  headline=a$results$power_summary$asDF,notices=a$results$notices$content,
  power=a$results$power_results$asDF, effect=a$results$effect_size_results$asDF,
  duration=a$results$study_duration_results$asDF,regulatory=a$results$regulatory_table$asDF,
  multiarm=a$results$multi_arm_table$asDF,simulation=a$results$simulation_validation_table$asDF,
  timeline=a$results$accrual_timeline_plot$state$data,
  natural_summary=a$results$natural_language_summary$content,
  interpretation=a$results$clinical_interpretation$content)
cases <- list()
cases$duration_500 <- record(sp_case(analysis_type='duration',sample_size_input=500,
  show_summary=TRUE,show_interpretation=TRUE))
a <- sp_case(analysis_type='duration',sample_size_input=1000)
cases$half_events <- record(a)
# Event accumulation during recruitment: integrate only over patients already entered.
calendar_events <- function(t,n=1000,A=24,hr=0.75,median=12,dropout=0.05) {
  lambda <- log(2)/median; eta <- -log1p(-dropout)/12
  sum(vapply(c(lambda,lambda*hr),function(rate) {
    n/2/A * integrate(function(u) rate/(rate+eta)*(-expm1(-(rate+eta)*(t-u))),
                      0,min(t,A))$value
  },0))
}
required <- a$.__enclos_env__$private$primary_numbers$events
cases$half_events$independent_half_time <- uniroot(function(t)calendar_events(t)-required/2,c(0,100))$root
for (hr in c(0.2,0.75)) {
  a <- sp_case(analysis_type='effect_size',effect_size=hr,sample_size_input=200)
  item <- record(a)
  derived_hr <- a$.__enclos_env__$private$primary_numbers$hr_detectable
  item$events_at_solved_hr <- calendar_events(36,n=200,hr=derived_hr)
  cases[[paste0('effect_',hr)]] <- item
}
a <- sp_case(analysis_type='power',sample_size_input=10,control_median_survival=240,
  accrual_period=1,follow_up_period=0,run_simulation_validation=TRUE,simulation_runs=1000,
  sensitivity_analysis=TRUE)
cases$low_events <- record(a)
cases$low_events$convergence <- a$.__enclos_env__$private$simulation_cache$convergence
cases$low_events$failed_or_zero_event_replicates <- sum(a$.__enclos_env__$private$simulation_cache$event_counts==0)
cases$cluster <- record(sp_case(study_design='cluster_randomized',cluster_size=100,
  icc=0.01,effect_size=0.1))
cases$ni_narrative <- record(sp_case(test_type='non_inferiority',effect_size=1,
  alpha_level=0.025,show_interpretation=TRUE,show_summary=TRUE,show_glossary=TRUE))
# Equivalent effect input rejected because the conversion bracket stops at HR 0.2.
cases$direct_hr_015 <- record(sp_case(effect_size=0.15))
delta <- exp(-log(2)*0.15)-0.5
cases$survival_difference_015 <- record(sp_case(effect_size_type='survival_difference',effect_size=delta))
cases$survival_difference_015$equivalent_HR <- 0.15
# Unadjusted m-arm pairwise alpha can be tiny; power_level remains a design input in power mode.
for (target in c(0.5,0.8,0.99)) {
  a <- sp_case(test_type='cox_regression',analysis_type='power',sample_size_input=200,
    interim_analyses=5,alpha_spending='pocock',power_level=target)
  cases[[paste0('cox_requested_power_',target)]] <- record(a)
}
# Compare directional boundary-crossing probabilities against the actual reported design.
seq_results <- list()
for (spending in c('obrien_fleming','pocock')) {
  for (looks in c(1L,5L)) {
    for (n in c(10L,200L,1000L)) {
      a <- sp_case(analysis_type='power',sample_size_input=n,interim_analyses=looks,alpha_spending=spending)
      p <- a$.__enclos_env__$private; d <- p$.gs_info_design()
      E <- p$primary_numbers$events
      prob <- gsDesign::gsProbability(k=looks+1,theta=abs(log(0.75))*0.5,
        n.I=E*d$timing,a=rep(-20,looks+1),b=d$upper$bound)
      seq_results[[length(seq_results)+1]] <- data.frame(spending=spending,interims=looks,n=n,
        local=p$primary_numbers$power,upper_boundary_probability=sum(prob$upper$prob[,1]))
    }
  }
}
write.csv(do.call(rbind,seq_results),file.path(out,'sequential-power-comparison.csv'),row.names=FALSE)
jsonlite::write_json(cases,file.path(out,'reproductions.json'),auto_unbox=TRUE,pretty=TRUE,null='null',digits=10)
cat('Wrote',length(cases),'reproductions.\n')
