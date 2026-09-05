pkgload::load_all('../jjstatsplot',quiet=TRUE)
out <- list()
d <- data.frame(y=factor(rep(c('Yes','No','Yes','No'),c(2,10,8,6))),g=factor(rep(c('A','B'),c(12,14))))
for(pre in c('diagnostic','treatment','biomarker','riskfactor')) {
 a<-jjstatsplot:::jjbarstatsClass$new(options=jjstatsplot:::jjbarstatsOptions$new(dep='y',group='g',clinicalpreset=pre),data=d)
 a$run(); pr<-a$.__enclos_env__$private
 out[[paste0('bar_preset_',pre)]]<-list(overrides=pr$overrides,effective=lapply(setNames(c('resultssubtitle','pairwisecomparisons','typestatistics','proportiontest'),c('resultssubtitle','pairwisecomparisons','typestatistics','proportiontest')),pr$.option))
}
a<-jjstatsplot:::jjbarstatsClass$new(options=jjstatsplot:::jjbarstatsOptions$new(dep='y',group='g',showexplanations=TRUE,resultssubtitle=TRUE),data=d)
a$run(); pr<-a$.__enclos_env__$private
out$bar_sparse_report<-as.character(a$results$report$content)
out$bar_sparse_summary<-as.character(a$results$summary$content)
out$bar_sparse_exact_subtitle<-paste(deparse(pr$.exactSubtitle(d,'y')),collapse=' ')
# Restore a previously rendered image using the actual jmvcore protobuf path.
for(ch in c('sampleSize','sampleThreshold','seed')) {
 opts<-jjstatsplot:::statsplot2Options$new(dep='y',group='g')
 r<-jjstatsplot:::statsplot2Results$new(opts)
 pb<-list(state=raw(),image=list(path='previous-render.png'))
 fresh<-jjstatsplot:::statsplot2Results$new(opts)
 fresh$plot$fromProtoBuf(pb,oChanges=ch,vChanges=character())
 out[paste0('image_restore_',ch)]<-list(fresh$plot$.__enclos_env__$private$.filePath)
}
a<-jjstatsplot:::jjpiestatsClass$new(options=jjstatsplot:::jjpiestatsOptions$new(dep='y',group='g',resultssubtitle=TRUE),data=d)
a$run(); pr<-a$.__enclos_env__$private
out$pie_sparse_report<-as.character(a$results$report$content)
out$pie_sparse_exact_subtitle<-paste(deparse(pr$.exactSubtitle(d,'y','g')),collapse=' ')
# Methods prose in paired mode.
dp<-data.frame(y=factor(rep(c('Yes','No','Yes','No'),c(10,5,4,11))),g=factor(rep(c('Yes','Yes','No','No'),c(10,5,4,11))))
b<-jjpiestats(dp,dep='y',group='g',paired=TRUE,resultssubtitle=TRUE)
out$pie_paired_report<-as.character(b$report$content)
# Preset uses derived options while the narrative reads raw options.
a<-jjstatsplot:::jjpiestatsClass$new(options=jjstatsplot:::jjpiestatsOptions$new(dep='y',group='g',clinicalpreset='treatment',typestatistics='bayes'),data=d)
a$run(); pr<-a$.__enclos_env__$private
out$pie_preset_mismatch<-list(effective_type=pr$.effectiveOptionsList()$typestatistics,report=as.character(a$results$report$content))
jsonlite::write_json(out,'quality-reports/jjstatsplot-check-2026-09-05/probes.json',pretty=TRUE,auto_unbox=TRUE,null='null')
print(out[c('bar_preset_treatment','image_restore_sampleSize','image_restore_sampleThreshold','image_restore_seed','pie_preset_mismatch')])
