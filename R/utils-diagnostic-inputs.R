# Fatal input validation must reach the engine's error handler. Call only after
# validation has refused the analysis, outside catches for optional computations.
.diagRejectInputErrors <- function(notices) {
  errors <- Filter(function(notice) identical(notice$type, "ERROR"), notices)
  if (length(errors)) {
    message <- paste(vapply(errors, function(notice) {
      paste(notice$title, notice$content, sep = ": ")
    }, character(1)), collapse = " ")
    jmvcore::reject(message)
  }
  invisible(NULL)
}
