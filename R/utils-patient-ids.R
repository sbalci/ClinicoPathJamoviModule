# Patient labels used by the timeline renderer and in response-analysis notices.
# Seventeen significant digits distinguish every representable double. Formatting
# with as.character() first would merge distinct numeric IDs near 1e15. Keep this
# mapping independent of the other IDs present in a subset or a later run.
.patientIDLabels <- function(x) {
  if (!is.numeric(x)) return(as.character(x))
  labels <- sprintf("%.17g", x)
  labels[is.na(x)] <- NA_character_
  labels[!is.na(x) & x == 0] <- "0"
  labels
}
