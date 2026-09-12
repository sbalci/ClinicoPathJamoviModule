# Schema, population and rendering evidence for the report-only audit.
source('R/survivalPower.h.R')
source('R/survivalPower_distributions.R')
source('R/survivalPower.b.R')
out <- 'development-ideas/survivalPower-check-full-2026-09-12'
a_spec <- yaml::read_yaml('jamovi/survivalPower.a.yaml')
r_spec <- yaml::read_yaml('jamovi/survivalPower.r.yaml')
src <- readLines('R/survivalPower.b.R')
snapshots <- readRDS(file.path(out, 'differential-snapshots.rds'))
for (mode in c('effect_size', 'duration')) {
  a <- survivalPowerClass$new(options=survivalPowerOptions$new(
    analysis_type=mode, sample_size_input=500L), data=data.frame())
  a$run()
  snapshots[[mode]] <- setNames(lapply(r_spec$items, function(item) {
    x <- a$results$get(item$name)
    if (item$type=='Table') return(x$asDF)
    if (item$type=='Image') return(if (is.null(x$state)) NULL else x$state$data)
    x$content
  }), vapply(r_spec$items, `[[`, '', 'name'))
}
# Setter sites individually inspected, including aliases such as table$setRow().
sites <- c(notices=77, instructions=563, power_summary=651,
  simulation_validation_table=1481, sample_size_results=1598,
  power_results=1641, effect_size_results=1713, study_duration_results=1774,
  assumptions_table=2287, non_inferiority_table=1857, multi_arm_table=1958,
  interim_analysis_table=2002, sensitivity_analysis_table=2125,
  regulatory_table=2722, power_curve_plot=2750, sample_size_plot=2777,
  survival_curves_plot=2804, accrual_timeline_plot=2831, sensitivity_plot=2859,
  clinical_interpretation=2313, natural_language_summary=4376,
  educational_explanations=4420, statistical_glossary=4458, guided_workflow=4491)
anames <- vapply(a_spec$options, `[[`, '', 'name')
data_options <- setdiff(anames, c('show_interpretation','show_summary',
  'show_explanations','show_glossary','guided_mode'))
matrix <- do.call(rbind, lapply(r_spec$items, function(item) {
  method <- if (item$type=='Table') 'setRow' else if (item$type=='Image') 'setState' else 'setContent'
  line <- sites[[item$name]]
  stopifnot(grepl(paste0('$',method,'('), src[line], fixed=TRUE))
  populated <- names(Filter(function(s) {
    x <- s[[item$name]]
    if (is.data.frame(x)) return(nrow(x)>0 && ncol(x)>1)
    !is.null(x) && length(x)>0 && any(nzchar(x))
  }, snapshots))
  data.frame(output=item$name, type=item$type, setter=method, line=line,
    visible=if (is.null(item$visible)) 'default' else as.character(item$visible),
    renderer=if (is.null(item$renderFun)) '' else item$renderFun,
    populated=length(populated)>0, example_scenario=populated[1],
    missing_data_clearWith=paste(setdiff(data_options,item$clearWith),collapse=';'))
}))
stopifnot(nrow(matrix)==24, all(matrix$populated), !any(nzchar(matrix$missing_data_clearWith)))
write.csv(matrix, file.path(out,'output-population.csv'), row.names=FALSE)

refs <- yaml::read_yaml('jamovi/00refs.yaml')$refs
refs_checked <- do.call(rbind,lapply(r_spec$refs,function(nm) {
  entry <- refs[[nm]]
  data.frame(key=nm,exists=!is.null(entry),
    author=if(is.null(entry$author)) '' else entry$author,
    year=if(is.null(entry$year)) '' else entry$year)
}))
stopifnot(all(refs_checked$exists), all(nzchar(refs_checked$author)), all(nzchar(refs_checked$year)))
write.csv(refs_checked, file.path(out,'citation-integrity.csv'), row.names=FALSE)

used <- unique(sub('self$options$', '', unlist(regmatches(src,
  gregexpr('self\\$options\\$[A-Za-z0-9_]+', src))), fixed=TRUE))
imports <- strsplit(read.dcf('DESCRIPTION')[1,'Imports'], ',')[[1]]
imports <- trimws(sub('\\s*\\(.*', '', imports))
packages <- unique(sub('::$','',unlist(regmatches(src,
  gregexpr('[A-Za-z][A-Za-z0-9.]*::',src)))))
static <- list(options_defined=length(anames), defined_but_not_read=setdiff(anames,used),
  reads_without_definition=setdiff(used,anames), namespace_packages=packages,
  packages_missing_imports=setdiff(packages,imports),
  html_entities=unique(unlist(regmatches(src,gregexpr('&[a-zA-Z][a-zA-Z0-9]{1,12};',src)))),
  setVisible_FALSE=grep('setVisible\\(FALSE\\)',src),
  warning_calls=grep('^\\s*warning\\(',src),
  addRow_sites=grep('addRow\\(',src),
  translation_calls=grep('\\.\\(',src),
  renderers=matrix$renderer[nzchar(matrix$renderer)])
jsonlite::write_json(static,file.path(out,'static-integrity.json'),pretty=TRUE,auto_unbox=TRUE)

a <- survivalPowerClass$new(options=survivalPowerOptions$new(sensitivity_analysis=TRUE),data=data.frame())
a$run()
render_results <- list()
grDevices::pdf(file.path(out,'rendered-plots.pdf'),width=8,height=6)
for (item in Filter(function(x)x$type=='Image',r_spec$items)) {
  render <- a$.__enclos_env__$private[[item$renderFun]]
  good <- render(a$results$get(item$name), ggtheme=ggplot2::theme_minimal(), theme=NULL)
  empty <- render(list(state=NULL), ggtheme=ggplot2::theme_minimal(), theme=NULL)
  stopifnot(isTRUE(good), identical(empty, FALSE))
  render_results[[item$name]] <- list(valid_state=good,null_state=empty)
}
grDevices::dev.off()
jsonlite::write_json(render_results,file.path(out,'plot-rendering.json'),pretty=TRUE,auto_unbox=TRUE)
cat('24 outputs populated; six references resolve; rendered all five plots.\n')
