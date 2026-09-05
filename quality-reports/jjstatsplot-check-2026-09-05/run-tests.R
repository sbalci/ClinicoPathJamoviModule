# Run from the ClinicoPathJamoviModule repository root.
# Build a disposable jjstatsplot snapshot and supply umbrella test fixtures.
root <- normalizePath('.')
source_pkg <- normalizePath('../jjstatsplot')
stage <- tempfile('jjstatsplot-check-'); dir.create(stage)
for (d in c('R','jamovi')) file.copy(file.path(source_pkg,d),stage,recursive=TRUE)
file.copy(file.path(source_pkg,c('DESCRIPTION','NAMESPACE')),stage)
dir.create(file.path(stage,'data'))
fixtures <- list.files(file.path(root,'data'), pattern='^(jjbarstats|jjpiestats|statsplot2).*\\.rda$',full.names=TRUE)
file.copy(fixtures,file.path(stage,'data'))
pkgload::load_all(stage,quiet=TRUE)
library(dplyr) # Older edge-case tests call row_number() and tibble() unqualified.
paths <- unlist(lapply(c('jjbarstats','jjpiestats','statsplot2'), function(n)
  file.path(root,'tests/testthat',paste0('test-',n,c('-basic.R','-release-review.R','-edge-cases.R')))))
paths <- c(paths,file.path(root,'tests/testthat/test-statsplot2-smoke.R'),
  file.path(root,'tests/testthat',paste0('test-',c('jjbarstats','jjpiestats'),'-correctness.R')))
tests <- tempfile('jjstatsplot-tests-'); dir.create(tests)
for (p in paths) {
  src <- readLines(p,warn=FALSE)
  src <- gsub("(['\"])ClinicoPath\\1", '"jjstatsplot"',src)
  src <- gsub('ClinicoPath::','jjstatsplot::',src,fixed=TRUE)
  writeLines(src,file.path(tests,basename(p)))
}
res <- testthat::test_dir(tests,reporter='summary',stop_on_failure=FALSE,load_helpers=FALSE)
df <- as.data.frame(res)
print(colSums(df[,c('nb','failed','skipped','error','warning','passed')]))
if (any(df$failed > 0 | df$error)) quit(status=1)
