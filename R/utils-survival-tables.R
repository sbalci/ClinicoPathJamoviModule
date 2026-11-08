# Populate a native result grid without interpreting its display strings.
# finalfit combines estimates and intervals in text cells; retain that precision.
.populateSurvivalTable <- function(table, data, formats = NULL) {
  table$deleteRows()
  for (column in table$columns) column$setVisible(FALSE)
  for (key in names(table$notes)) table$setNote(key, NULL, init = FALSE)
  data <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
  ids <- paste0("column", seq_along(data))
  for (i in seq_along(data)) {
    numeric <- is.numeric(data[[i]])
    table$addColumn(name = ids[i], title = names(data)[i],
      type = if (numeric) "number" else "text",
      format = if (!is.null(formats)) formats[i] else if (numeric) "sf:6" else "")
  }
  for (i in seq_len(nrow(data))) {
    values <- lapply(data, function(x) if (is.factor(x)) as.character(x[i]) else x[i])
    table$addRow(rowKey = as.character(i), values = stats::setNames(values, ids))
  }
  invisible(table)
}
