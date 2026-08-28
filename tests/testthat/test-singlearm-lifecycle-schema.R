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

test_that("each analysis instance owns a private cache", {
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
  first$init()
  second$init()
  first_cache <- first$.__enclos_env__$private$.cache
  second_cache <- second$.__enclos_env__$private$.cache

  expect_false(identical(first_cache, second_cache))
  assign("sentinel", d, envir = first_cache)
  expect_false(exists("sentinel", envir = second_cache, inherits = FALSE))
})

test_that("rerunning an analysis replaces rows and cache content", {
  namespace <- environment(singlearm)
  options_generator <- get("singlearmOptions", envir = namespace)
  analysis_generator <- get("singlearmClass", envir = namespace)
  d <- data.frame(time = 1:8, status = rep(c(1L, 0L), 4))
  options <- options_generator$new(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL,
    person_time = TRUE,
    incidence_rate_unit = "person-months"
  )
  analysis <- analysis_generator$new(options = options, data = d)
  analysis$run()

  first_rows <- analysis$results$personTimeTable$rowCount
  first_cache_keys <- ls(envir = analysis$.__enclos_env__$private$.cache)

  expect_gt(first_rows, 0)
  expect_gt(length(first_cache_keys), 0)

  d_new <- data.frame(time = 1:12, status = rep(c(1L, 0L), 6))
  analysis <- analysis_generator$new(options = options, data = d_new)
  analysis$run()

  second_rows <- analysis$results$personTimeTable$rowCount
  second_cache_keys <- ls(envir = analysis$.__enclos_env__$private$.cache)

  expect_gt(second_rows, 0)
  expect_gt(length(second_cache_keys), 0)
})

test_that("unusual user column names are safe in survival formulas", {
  d <- data.frame(dummy = 1:8, check.names = FALSE)
  d$dummy <- NULL
  d[["follow up`time"]] <- 1:8
  d[["event status`code"]] <- factor(
    rep(c("Dead", "Alive"), 4),
    levels = c("Alive", "Dead")
  )

  result <- singlearm(
    data = d,
    elapsedtime = "follow up`time",
    outcome = "event status`code",
    outcomeLevel = "Dead",
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL
  )

  expect_equal(result$errors$content, "")
  expect_equal(result$medianTable$rowCount, 1)
  expect_equal(result$medianTable$getCell(rowNo = 1, "events")$value, 4)
})

test_that("selecting the same variable for elapsed time and outcome is rejected", {
  d <- data.frame(
    time = 1:6,
    status = c(1L, 0L, 1L, 0L, 1L, 0L)
  )
  result <- singlearm(
    data = d,
    elapsedtime = "time",
    outcome = "time",
    outcomeLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL
  )

  expect_match(result$errors$content, "Elapsed time and outcome must be different variables")
  expect_equal(result$medianTable$rowCount, 0)
})

test_that("unusual user column names also render in the Kaplan-Meier plot", {
  skip_if_not_installed("finalfit")
  namespace <- environment(singlearm)
  options_generator <- get("singlearmOptions", envir = namespace)
  analysis_generator <- get("singlearmClass", envir = namespace)
  d <- data.frame(dummy = 1:12, check.names = FALSE)
  d$dummy <- NULL
  d[["follow up`time"]] <- 1:12
  d[["event status`code"]] <- factor(
    rep(c("Dead", "Alive"), 6),
    levels = c("Alive", "Dead")
  )
  options <- options_generator$new(
    elapsedtime = "follow up`time",
    outcome = "event status`code",
    outcomeLevel = "Dead",
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL,
    sc = TRUE,
    endplot = 12,
    byplot = 2
  )
  analysis <- analysis_generator$new(options = options, data = d)
  analysis$run()

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(analysis$.__enclos_env__$private$.plot(
    analysis$results$plot,
    ggplot2::theme_bw(),
    NULL
  ))
})

test_that("KMunicate renderer honors CI and risk-table options without error", {
  skip_if_not_installed("KMunicate")
  namespace <- environment(singlearm)
  options_generator <- get("singlearmOptions", envir = namespace)
  analysis_generator <- get("singlearmClass", envir = namespace)
  d <- data.frame(
    time = 1:12,
    status = factor(rep(c("Dead", "Alive"), 6), levels = c("Alive", "Dead"))
  )
  options <- options_generator$new(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "Dead",
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL,
    kmunicate = TRUE,
    ci95 = TRUE,
    risktable = TRUE,
    endplot = 12,
    byplot = 2
  )
  analysis <- analysis_generator$new(options = options, data = d)
  analysis$run()

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(analysis$.__enclos_env__$private$.plot6(
    analysis$results$plot6,
    ggplot2::theme_bw(),
    NULL
  ))
})

test_that("singlearm UI exposes every non-data option exactly once", {
  analysis <- yaml::read_yaml(test_path("..", "..", "jamovi", "singlearm.a.yaml"))
  ui <- yaml::read_yaml(test_path("..", "..", "jamovi", "singlearm.u.yaml"))

  option_names <- vapply(analysis$options, `[[`, character(1), "name")
  ui_names <- collect_yaml_names(ui$children)
  expected <- setdiff(option_names, "data")

  expect_setequal(ui_names, expected)
  expect_equal(length(ui_names), length(unique(ui_names)))
})

test_that("plot result dependencies include every option used by its renderer", {
  results <- yaml::read_yaml(test_path("..", "..", "jamovi", "singlearm.r.yaml"))

  expect_true(all(c("ci95", "risktable") %in%
                  yaml_item(results, "plot6")$clearWith))
  expect_true("medianline" %in% yaml_item(results, "plot2")$clearWith)
  expect_true("medianline" %in% yaml_item(results, "plot3")$clearWith)
  expect_true(all(c("analysistype", "dod", "dooc", "awd", "awod") %in%
                  yaml_item(results, "medianTable")$clearWith))
})

test_that("every declared render function exists in the backend", {
  namespace <- environment(singlearm)
  generator <- get("singlearmClass", envir = namespace)
  results <- yaml::read_yaml(test_path("..", "..", "jamovi", "singlearm.r.yaml"))
  renderers <- unique(vapply(
    Filter(function(item) !is.null(item$renderFun), results$items),
    `[[`, character(1), "renderFun"
  ))

  expect_true(all(sub("^\\.", "", renderers) != ""))
  expect_true(all(renderers %in% names(generator$private_methods)))
})
