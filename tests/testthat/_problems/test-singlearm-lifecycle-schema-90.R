# Extracted from test-singlearm-lifecycle-schema.R:90

# prequel ----------------------------------------------------------------------
collect_yaml_names <- function(x) {
  found <- character()
  if (is.list(x)) {
    if (!is.null(x$name) && is.character(x$name))
      found <- c(found, x$name)
    for (value in x)
      found <- c(found, collect_yaml_names(value))
  }
  unique(found)
}
yaml_item <- function(definition, name) {
  matches <- Filter(function(item) identical(item$name, name), definition$items)
  stopifnot(length(matches) == 1L)
  matches[[1]]
}

# test -------------------------------------------------------------------------
d <- data.frame(check.names = FALSE)
d[["follow up`time"]] <- 1:8
