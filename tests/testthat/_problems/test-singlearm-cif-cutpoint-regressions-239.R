# Extracted from test-singlearm-cif-cutpoint-regressions.R:239

# prequel ----------------------------------------------------------------------
library(testthat)
.singlearm_src <- function() {
  for (p in c("../../R/singlearm.b.R", "../R/singlearm.b.R", "R/singlearm.b.R"))
    if (file.exists(p)) return(p)
  NULL
}
.singlearm_stub <- function(timetypeoutput = "months") {
  src <- .singlearm_src()
  skip_if(is.null(src), "R/singlearm.b.R not available (installed-package check)")

  e <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(sys.source(src, envir = e)))
  pm <- e$singlearmClass$private_methods

  log <- new.env(parent = emptyenv())
  log$msgs <- character()
  add <- function(kind) function(m) log$msgs <- c(log$msgs, paste0(kind, ": ", m))

  stub <- new.env(parent = globalenv())
  bind <- function(f) { environment(f) <- stub; f }
  stub$self <- list(options = list(timetypeoutput = timetypeoutput))
  stub$private <- list(
    .addInfo    = add("INFO"),
    .addWarning = add("WARNING"),
    .addError   = add("ERROR"),
    .isCompetingRisk = function(...) FALSE,
    .yearInUnits         = bind(pm$.yearInUnits),
    .getDefaultCutpoints = bind(pm$.getDefaultCutpoints),
    .parseNumericList    = bind(pm$.parseNumericList),
    .resolveCutpoints    = bind(pm$.resolveCutpoints),
    .supportedCutpoints  = bind(pm$.supportedCutpoints),
    .ciText              = bind(pm$.ciText)
  )
  stub$log <- log
  stub
}

# test -------------------------------------------------------------------------
s <- .singlearm_stub("years")
expect_equal(s$private$.resolveCutpoints(""), c(1, 3, 5))
expect_length(s$log$msgs, 0)
