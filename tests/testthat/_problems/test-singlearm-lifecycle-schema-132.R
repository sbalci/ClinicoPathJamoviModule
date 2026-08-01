# Extracted from test-singlearm-lifecycle-schema.R:132

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
d <- data.frame(
    time = 1:6,
    duplicate_time = 11:16,
    status = c(1L, 0L, 1L, 0L, 1L, 0L),
    check.names = FALSE
  )
names(d)[2] <- "time"
result <- singlearm(
    data = d,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL
  )
expect_match(result$errors$content, "duplicated in the data")
