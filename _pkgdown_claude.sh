RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64" \
Rscript -e 'tryCatch(pkgdown::build_articles(), error = function(e) cat("ERROR:", conditionMessage(e), "\n"))' | claude
