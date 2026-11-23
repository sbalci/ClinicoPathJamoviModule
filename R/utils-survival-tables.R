# Populate a native result grid without interpreting its display strings.
# finalfit combines estimates and intervals in text cells; retain that precision.
# Titles and text cells go through .rocLabel(): the client parses both as HTML, so a level or
# variable named "<LOD" would open a tag and render blank. "p<0.001" is untouched.
.populateSurvivalTable <- function(table, data, formats = NULL) {
  table$deleteRows()
  for (column in table$columns) column$setVisible(FALSE)
  for (key in names(table$notes)) table$setNote(key, NULL, init = FALSE)
  data <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
  ids <- paste0("column", seq_along(data))
  for (i in seq_along(data)) {
    numeric <- is.numeric(data[[i]])
    table$addColumn(name = ids[i], title = .rocLabel(names(data)[i], note = FALSE),
      type = if (numeric) "number" else "text",
      format = if (!is.null(formats)) formats[i] else if (numeric) "sf:6" else "")
  }
  for (i in seq_len(nrow(data))) {
    values <- lapply(data, function(x)
      if (is.factor(x) || is.character(x)) .rocLabel(x[i], note = FALSE) else x[i])
    table$addRow(rowKey = as.character(i), values = stats::setNames(values, ids))
  }
  invisible(table)
}

# library-audit 2026-11-20 jsurvival [HIGH] DONE: a cox.zph result is rows of a Table (term,
# chisq, df, p), never printed console text; returns the terms with p < 0.05
.fillCoxZphTable <- function(table, zph, display = NULL) {
  .deleteTableRows(table)
  tab <- zph$table
  if (is.null(tab) || nrow(tab) == 0) return(invisible(character(0)))
  cn <- colnames(tab)
  col_chisq <- if ("chisq" %in% cn) "chisq" else cn[1]
  col_df <- if ("df" %in% cn) "df" else cn[min(2, length(cn))]
  col_p <- if ("p" %in% cn) "p" else cn[length(cn)]
  terms <- .displayTerm(rownames(tab), display)   # the user's column names, not the cleaned ones
  for (i in seq_len(nrow(tab))) {
    table$addRow(rowKey = i, values = list(
      term = terms[i],
      chisq = unname(tab[i, col_chisq]),
      df = as.integer(round(unname(tab[i, col_df]))),
      p = unname(tab[i, col_p])))
  }
  invisible(setdiff(terms[which(tab[, col_p] < 0.05)], "GLOBAL"))
}

# A model term shown with the user's column names. `display` maps a cleaned name to the name as
# typed (multisurvival's .displayNames()); a part it does not know is kept as it is. Interaction
# parts are mapped one by one and joined with " * ", and a factor coefficient ("<name><level>")
# becomes "<Name>: <level>".
.displayTerm <- function(term, display = NULL) {
  term <- as.character(term)
  if (is.null(display) || length(display) == 0) return(term)
  known <- names(display)
  one <- function(part) {
    bare <- gsub("^`|`$", "", part)
    if (bare %in% known) return(unname(display[[bare]]))
    hit <- known[startsWith(bare, known)]
    if (length(hit) == 0) return(part)
    hit <- hit[which.max(nchar(hit))]
    paste0(unname(display[[hit]]), ": ", substring(bare, nchar(hit) + 1L))
  }
  vapply(term, function(t) paste(vapply(strsplit(t, ":", fixed = TRUE)[[1]], one, character(1)),
                                 collapse = " * "), character(1), USE.NAMES = FALSE)
}
