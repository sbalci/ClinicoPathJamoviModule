# Report-only survivalPower audit. Run from the package root with Rscript.
root <- normalizePath(if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else '.')
setwd(root)
out <- 'development-ideas/survivalPower-check-full-2026-09-12'
dir.create(out, recursive=TRUE, showWarnings=FALSE)
source('R/survivalPower.h.R')
source('R/survivalPower_distributions.R')
source('R/survivalPower.b.R')
a_spec <- yaml::read_yaml('jamovi/survivalPower.a.yaml')
r_spec <- yaml::read_yaml('jamovi/survivalPower.r.yaml')
run_sp_audit <- function(args=list()) {
  a <- survivalPowerClass$new(options=do.call(survivalPowerOptions$new,args), data=data.frame())
  warnings <- character()
  elapsed <- system.time(error <- tryCatch(withCallingHandlers({a$run(); NULL},
    warning=function(w){warnings <<- c(warnings,conditionMessage(w)); invokeRestart('muffleWarning')}),
    error=function(e)conditionMessage(e)))[['elapsed']]
  outputs <- lapply(r_spec$items,function(item){
    x <- a$results$get(item$name)
    if (item$type=='Table') return(x$asDF)
    if (item$type=='Image') return(if(is.null(x$state))NULL else x$state$data)
    x$content
  })
  names(outputs) <- vapply(r_spec$items,`[[`,'','name')
  list(a=a, outputs=outputs, numbers=a$.__enclos_env__$private$primary_numbers,
       error=error,warnings=warnings,elapsed=elapsed)
}
changed <- list(clinical_preset='cardio_prevention',analysis_type='power',
  test_type='cox_regression',study_design='multi_arm',primary_endpoint='progression_free_survival',
  effect_size_type='median_ratio',effect_size=0.6,alpha_level=0.025,power_level=0.9,
  allocation_ratio=2,sample_size_input=400L,control_median_survival=18,
  survival_distribution='weibull',weibull_shape=1.5,accrual_period=36,follow_up_period=24,
  accrual_pattern='linear_increasing',dropout_rate=0.1,ni_margin=1.5,ni_type='absolute_margin',
  rmst_tau=24,rmst_difference=4,number_of_arms=4L,multiple_comparisons='none',
  interim_analyses=2L,alpha_spending='obrien_fleming',cluster_size=75,icc=0.1,
  sensitivity_analysis=TRUE,run_simulation_validation=TRUE,simulation_runs=1000L,
  simulation_seed=99L,show_interpretation=TRUE,show_summary=TRUE,show_explanations=TRUE,
  show_glossary=TRUE,guided_mode=TRUE)
contexts <- list(sample_size_input=list(analysis_type='power'),
  weibull_shape=list(survival_distribution='weibull'),
  ni_margin=list(test_type='non_inferiority',effect_size=1,alpha_level=0.025),
  ni_type=list(test_type='non_inferiority',effect_size=1,alpha_level=0.025),
  rmst_tau=list(effect_size_type='rmst_difference'),
  rmst_difference=list(effect_size_type='rmst_difference'),
  number_of_arms=list(study_design='multi_arm'),multiple_comparisons=list(study_design='multi_arm'),
  interim_analyses=list(alpha_spending='obrien_fleming'),alpha_spending=list(interim_analyses=2L),
  cluster_size=list(study_design='cluster_randomized'),icc=list(study_design='cluster_randomized'),
  simulation_runs=list(run_simulation_validation=TRUE),
  simulation_seed=list(run_simulation_validation=TRUE,simulation_runs=1000L))
baseline <- run_sp_audit()
rows <- list(); snapshots <- list(default=baseline$outputs)
for (opt in a_spec$options) {
  nm <- opt$name; value <- changed[[nm]]
  stopifnot(!is.null(value))
  new <- run_sp_audit(setNames(list(value),nm))
  affected <- names(Filter(identity,Map(function(a,b)!isTRUE(all.equal(a,b)),baseline$outputs,new$outputs)))
  ctx <- contexts[[nm]]
  contextual <- character()
  ctx_numbers <- ''
  if (!is.null(ctx)) {
    old_ctx <- run_sp_audit(ctx)
    new_ctx <- run_sp_audit(modifyList(ctx,setNames(list(value),nm)))
    contextual <- names(Filter(identity,Map(function(a,b)!isTRUE(all.equal(a,b)),old_ctx$outputs,new_ctx$outputs)))
    ctx_numbers <- paste(capture.output(str(list(old=old_ctx$numbers,new=new_ctx$numbers))),collapse=' ')
    snapshots[[paste0(nm,'_context')]] <- new_ctx$outputs
  }
  rows[[nm]] <- data.frame(option=nm,default=as.character(opt$default),changed=as.character(value),
    changed_outputs=paste(affected,collapse=';'),context=paste(capture.output(dput(ctx)),collapse=' '),
    contextual_outputs=paste(contextual,collapse=';'),context_numbers=ctx_numbers,
    primary_changed=!isTRUE(all.equal(baseline$numbers,new$numbers)),
    error=if(is.null(new$error))'' else new$error,
    notices=paste(new$outputs$notices,collapse=' '),elapsed=new$elapsed)
  snapshots[[nm]] <- new$outputs
  cat(nm,':',length(affected),'default outputs;',length(contextual),'context outputs\n')
}
write.csv(do.call(rbind,rows),file.path(out,'argument-behavior.csv'),row.names=FALSE)
saveRDS(snapshots,file.path(out,'differential-snapshots.rds'))
writeLines(capture.output(sessionInfo()),file.path(out,'session-info.txt'))
write.csv(data.frame(file=c('R/survivalPower.b.R','R/survivalPower.h.R',
  'jamovi/survivalPower.a.yaml','jamovi/survivalPower.r.yaml','jamovi/survivalPower.u.yaml'),
  md5=unname(tools::md5sum(c('R/survivalPower.b.R','R/survivalPower.h.R',
  'jamovi/survivalPower.a.yaml','jamovi/survivalPower.r.yaml','jamovi/survivalPower.u.yaml')))),
  file.path(out,'source-fingerprints.csv'),row.names=FALSE)
