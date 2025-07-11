RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64" \
Rscript -e 'tryCatch(pkgdown::build_articles(), error = function(e) cat("ERROR:", conditionMessage(e), "\n"))' | claude

RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64" \
Rscript -e "
  tryCatch(
    withCallingHandlers(
      pkgdown::build_articles(),
      warning = function(w) {
        cat('WARNING:', conditionMessage(w), '\n')
        invokeRestart('muffleWarning')
      }
    ),
    error = function(e) cat('ERROR:', conditionMessage(e), '\n')
  )" | claude


Rscript -e 'tryCatch(jmvtools::prepare(), error = function(e) cat("ERROR:", conditionMessage(e), "\n"))' | claude

Rscript -e 'tryCatch(devtools::document(), error = function(e) cat("ERROR:", conditionMessage(e), "\n"))' | claude
