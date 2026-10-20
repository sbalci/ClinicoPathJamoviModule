# Character option values are R literals, including names of selected columns.
# Older jmvcore versions interpolate these values without quoting or escaping.
.sourcifyDiagnosticOption <- function(option) {
  if (option$name == "data" || startsWith(option$name, "results/")) return(NULL)
  value <- option$value
  if (!is.character(value)) return(NULL)
  if (identical(value, option$default)) return("")
  paste0(option$name, " = ", paste(deparse(value, width.cutoff = 100L), collapse = "\n"))
}
