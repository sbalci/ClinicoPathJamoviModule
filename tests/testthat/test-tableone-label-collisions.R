tableone_collision_rows <- function(content) {
  document <- xml2::read_html(as.character(content))
  rows <- xml2::xml_find_all(document, "//tbody/tr")
  lapply(rows, function(row) trimws(gsub("\u00a0", " ",
    xml2::xml_text(xml2::xml_find_all(row, "./td")), fixed = TRUE)))
}

test_that("janitor preserves real categories when summary labels collide", {
  labels <- c("Total", "Total (all cases)", "NA", "Missing (NA)", "Unknown", NA)
  for (kind in c("factor", "ordered", "character")) {
    value <- rep(labels, each = 4)
    if (kind != "character") value <- factor(value, ordered = kind == "ordered")
    d <- data.frame(g = value)
    before <- d
    result <- tableone(d, "g", sty = "t4", showReportSentence = TRUE)
    rows <- tableone_collision_rows(result$tablestyle4$content)
    names <- vapply(rows, `[`, "", 1)
    expect_length(rows, 7L)
    expect_false(anyDuplicated(names) > 0L)
    for (label in labels[!is.na(labels)]) {
      expect_identical(rows[[match(label, names)]][2:4], c("4", "16.7%", "20.0%"))
    }
    expect_identical(rows[[match("Missing (NA) (1)", names)]][2:4],
                     c("4", "16.7%", "-"))
    expect_identical(rows[[match("Total (all cases) (1)", names)]][2:4],
                     c("24", "100.0%", "100.0%"))
    expect_match(result$reportSentence$content, "24 cases")
    expect_false(grepl("could not be produced", result$todo$content, fixed = TRUE))
    expect_identical(d, before)
  }
})

test_that("unused Total levels and exclusion cannot corrupt frequency totals", {
  d <- data.frame(g = factor(c("A", "B", NA), levels = c("A", "B", "Total")))
  result <- tableone(d, "g", sty = "t4", excl = TRUE, showReportSentence = TRUE)
  rows <- tableone_collision_rows(result$tablestyle4$content)
  expect_length(rows, 3L)
  expect_identical(rows[[3]], c("Total", "2", "100.0%"))
  expect_match(result$reportSentence$content, "2 cases with complete data")
})

test_that("gtsummary distinguishes Unknown categories from missing records", {
  d <- data.frame(g = factor(rep(c("Unknown", "Missing (NA)", "A", NA), each = 5)))
  before <- d
  result <- tableone(d, "g", sty = "t2")
  rows <- tableone_collision_rows(result$tablestyle2$content)
  names <- vapply(rows, `[`, "", 1)
  expect_false(anyDuplicated(names) > 0L)
  expect_identical(rows[[match("Unknown", names)]][2], "5 (33%)")
  expect_identical(rows[[match("Missing (NA)", names)]][2], "5 (33%)")
  expect_identical(rows[[match("Missing (NA) (1)", names)]][2], "5")
  expect_identical(d, before)
})

test_that("arsenal distinguishes N-Miss categories from missing records", {
  d <- data.frame(g = factor(rep(c("N-Miss", "Missing (NA)", "A", NA), each = 5)))
  result <- tableone(d, "g", sty = "t3")
  rows <- tableone_collision_rows(result$tablestyle3$content)
  names <- vapply(rows, `[`, "", 1)
  expect_false(anyDuplicated(names) > 0L)
  expect_identical(rows[[match("N-Miss", names)]][2], "5 (33.3%)")
  expect_identical(rows[[match("Missing (NA)", names)]][2], "5 (33.3%)")
  expect_identical(rows[[match("Missing (NA) (1)", names)]][2], "5")
})

test_that("collision labels use the compiled Turkish translations", {
  locale <- system.file("i18n/tr.json", package = "ClinicoPath")
  skip_if(!nzchar(locale), "Compiled Turkish catalog is required")
  ns <- environment(tableone)
  d <- data.frame(g = factor(rep(c("Total", "NA", "Unknown", "N-Miss", NA), 4)))
  for (style in c("t2", "t3", "t4")) {
    analysis <- get("tableoneClass", ns)$new(data = d,
      options = get("tableoneOptions", ns)$new(vars = "g", sty = style))
    options <- analysis$options
    options$.__enclos_env__$private$.translator <-
      jmvcore:::Translator$new(jsonlite::read_json(locale))
    analysis$run()
    content <- analysis$results[[paste0("tablestyle", substring(style, 2))]]$content
    expect_match(content, "Eksik (NA)", fixed = TRUE)
    if (style == "t4") expect_match(content, "Toplam (t\u00fcm olgular)", fixed = TRUE)
  }
})

test_that("reserved-label repairs survive actual framework save and load", {
  skip_if_not_installed("RProtoBuf")
  ns <- environment(tableone)
  d <- data.frame(g = factor(rep(c("Total", "NA", "Unknown", NA), 10)))
  make <- function(data, style) {
    get("tableoneClass", ns)$new(data = data,
      options = get("tableoneOptions", ns)$new(vars = "g", sty = style,
        showSummary = TRUE, showReportSentence = TRUE), datasetId = "1", analysisId = 1L)
  }
  for (style in c("t1", "t2", "t3", "t4")) {
    path <- tempfile(fileext = ".pb")
    on.exit(unlink(path), add = TRUE)
    original <- make(d, style)
    original$.setStatePathSource(function() path)
    original$run()
    original$.save()
    expect_true(file.exists(path))
    restored <- make(d, style)
    restored$.setStatePathSource(function() path)
    restored$init()
    restored$postInit()
    restored$.load()
    item <- paste0("tablestyle", substring(style, 2))
    expect_identical(as.character(restored$results[[item]]$content),
                     as.character(original$results[[item]]$content))
    # A filtered/edited cohort must replace saved counts, not retain N=40.
    edited <- make(d[1:12, , drop = FALSE], style)
    edited$.setStatePathSource(function() path)
    edited$init()
    edited$postInit()
    edited$.load(vChanges = "g")
    edited$run()
    expect_match(edited$results$reportSentence$content, "12 cases")
    expect_false(grepl("40 cases", edited$results$reportSentence$content))
  }
})
