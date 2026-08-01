# Extracted from test-singlearm-lifecycle-schema.R:42

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
namespace <- environment(singlearm)
options_generator <- get("singlearmOptions", envir = namespace)
analysis_generator <- get("singlearmClass", envir = namespace)
d <- data.frame(time = 1:4, status = c(1L, 0L, 1L, 0L))
make_analysis <- function() {
    options <- options_generator$new(
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = NULL,
      dod = NULL,
      dooc = NULL,
      awd = NULL,
      awod = NULL
    )
    analysis_generator$new(options = options, data = d)
  }
first <- make_analysis()
second <- make_analysis()
first_cache <- first$.__enclos_env__$private$.cache
second_cache <- second$.__enclos_env__$private$.cache
expect_false(identical(first_cache, second_cache))
