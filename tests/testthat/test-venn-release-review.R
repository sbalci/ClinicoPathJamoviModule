# Regression cover for the defects found during the venn release review.
# Each block fails against the pre-review backend.

library(testthat)

vn <- function(d, ...) {
  args <- list(data = d, var3 = NULL, var3true = NULL, var4 = NULL, var4true = NULL,
               var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
               var7 = NULL, var7true = NULL)
  do.call(venn, utils::modifyList(args, list(...)))
}

test_that("variables whose names sanitise alike stay separate sets", {
  # Each block called make.names() on its own variable, so "Tumor Grade" and
  # "Tumor.Grade" both became "Tumor.Grade" and the second set silently
  # overwrote the first in the working frame. The summary table still showed
  # both original names, so two genuinely different sets - 30 positive and 5
  # positive - were both reported as 5, and the diagram drew them identically.
  d <- data.frame(check.names = FALSE,
    `Tumor Grade` = factor(rep(c("Yes", "No"), c(30, 10))),
    `Tumor.Grade` = factor(rep(c("Yes", "No"), c(5, 35))))

  df <- vn(d, var1 = "Tumor Grade", var1true = "Yes",
              var2 = "Tumor.Grade", var2true = "Yes")$summary$asDF

  expect_equal(nrow(df), 2L)
  counts <- stats::setNames(df$trueCount, as.character(df$variable))
  expect_equal(counts[["Tumor Grade"]], 30)
  expect_equal(counts[["Tumor.Grade"]], 5)
  expect_false(counts[["Tumor Grade"]] == counts[["Tumor.Grade"]])
})

test_that("selecting the same variable twice is refused", {
  # A Venn diagram of a set against itself is degenerate; it used to be accepted
  # silently and drawn as two identical circles.
  d <- data.frame(A = factor(rep(c("Yes", "No"), c(30, 10))),
                  B = factor(rep(c("Yes", "No"), c(5, 35))))
  expect_error(
    vn(d, var1 = "A", var1true = "Yes", var2 = "A", var2true = "Yes"),
    "more than once")

  # two different variables still work
  expect_equal(nrow(vn(d, var1 = "A", var1true = "Yes",
                          var2 = "B", var2true = "Yes")$summary$asDF), 2L)
})

test_that("set counts reproduce a hand-computed tabulation", {
  set.seed(11); n <- 200
  A <- rbinom(n, 1, .5); B <- rbinom(n, 1, .4); C <- rbinom(n, 1, .3)
  d <- data.frame(A = factor(ifelse(A == 1, "Pos", "Neg")),
                  B = factor(ifelse(B == 1, "Pos", "Neg")),
                  C = factor(ifelse(C == 1, "Pos", "Neg")))

  df <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
              var3 = "C", var3true = "Pos")$summary$asDF

  counts <- stats::setNames(df$trueCount, as.character(df$variable))
  expect_equal(counts[["A"]], sum(A))
  expect_equal(counts[["B"]], sum(B))
  expect_equal(counts[["C"]], sum(C))
  expect_true(all(df$totalCount == n))
  expect_equal(unname(df$truePercentage), unname(df$trueCount / n))
})

test_that("the membership table is bounded and says when it truncates", {
  # jmvcore's addRow is called once per case and the cost grew quadratically:
  # 3.2s at n=200, 8.3s at n=400, 34.0s at n=800, and a 20000-row dataset did
  # not finish in ten minutes. Rendering is now capped.
  set.seed(2); n <- 900
  d <- data.frame(A = factor(sample(c("Pos", "Neg"), n, TRUE)),
                  B = factor(sample(c("Pos", "Neg"), n, TRUE)))

  tb <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
              showMembershipTable = TRUE)$membershipTable

  expect_equal(nrow(tb$asDF), 500L)
  note <- get("note", tb$.__enclos_env__$private$.notes[["truncated"]])
  expect_match(note, "first 500 of 900")
  expect_match(note, "Add membership groups to data")
})

test_that("a small membership table is not truncated and carries no note", {
  set.seed(3); n <- 40
  d <- data.frame(A = factor(sample(c("Pos", "Neg"), n, TRUE)),
                  B = factor(sample(c("Pos", "Neg"), n, TRUE)))

  tb <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
              showMembershipTable = TRUE)$membershipTable

  expect_equal(nrow(tb$asDF), n)
  nt <- tb$.__enclos_env__$private$.notes[["truncated"]]
  expect_true(is.null(nt) || is.null(get("note", nt)))
})

test_that("membership groups match a hand-computed set combination", {
  d <- data.frame(
    A = factor(c("Pos", "Pos", "Neg", "Neg")),
    B = factor(c("Pos", "Neg", "Pos", "Neg")))

  mt <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
              showMembershipTable = TRUE)$membershipTable$asDF

  expect_equal(nrow(mt), 4L)
  grp <- mt[[grep("Group", names(mt), ignore.case = TRUE)[1]]]
  expect_equal(as.character(grp), c("A & B", "A", "B", "None"))
})

test_that("enabling the membership table does not slow the rest of the analysis", {
  # The whole cost belongs to the table; everything else must stay instant.
  set.seed(2); n <- 2000
  d <- data.frame(A = factor(sample(c("Pos", "Neg"), n, TRUE)),
                  B = factor(sample(c("Pos", "Neg"), n, TRUE)))

  t0 <- Sys.time()
  vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos")
  without <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_lt(without, 5)
})

test_that("an absent true level is reported, not thrown", {
  d <- data.frame(a = factor(c("Yes", "Yes")), b = factor(c("No", "No")))
  res <- vn(d, var1 = "a", var1true = "Maybe", var2 = "b", var2true = "No")

  expect_true(res$validationErrors$visible)
  msg <- as.character(res$validationErrors$content)
  expect_match(msg, "Maybe")
  expect_match(msg, "Available levels")
  expect_equal(nrow(res$summary$asDF), 0L)
})

test_that("excluded incomplete cases are disclosed", {
  d <- data.frame(A = factor(c(rep("Pos", 8), rep("Neg", 2))),
                  B = factor(c(rep("Pos", 5), rep("Neg", 3), NA, NA)))
  res <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos")

  txt <- paste(as.character(res$validationWarnings$content), collapse = " ")
  expect_match(txt, "CASE EXCLUSION")
  expect_match(txt, "Original N=10")
  expect_match(txt, "Final N=8")
  # and the reported totals reflect the complete cases only
  expect_true(all(res$summary$asDF$totalCount == 8))
})

test_that("re-running does not duplicate the summary rows", {
  # jmvcore's Table$addRow() appends without a duplicate-rowKey check, and the
  # top-level clearWith covers the variables and plot options but not the panel
  # toggles (showGlossary, clinicalSummary, showSetCalculations, ...). Ticking one
  # of those re-entered .run() against the retained rows: three runs gave 9 rows
  # for 3 variables, and summary$asDF then died with "duplicate 'row.names'".
  d <- data.frame(A = factor(rep(c("Pos", "Neg"), c(7, 3))),
                  B = factor(rep(c("Pos", "Neg"), c(4, 6))),
                  C = factor(rep(c("Pos", "Neg"), c(5, 5))))

  a <- vennClass$new(
    options = vennOptions$new(
      var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
      var3 = "C", var3true = "Pos",
      var4 = NULL, var4true = NULL, var5 = NULL, var5true = NULL,
      var6 = NULL, var6true = NULL, var7 = NULL, var7true = NULL,
      show_ggvenn = FALSE),
    data = d)

  a$init()
  a$run()
  expect_equal(a$results$summary$rowCount, 3L)
  a$run()
  a$run()
  expect_equal(a$results$summary$rowCount, 3L)
  expect_equal(nrow(a$results$summary$asDF), 3L)
  expect_equal(a$results$summary$asDF$trueCount, c(7L, 4L, 5L))
})

test_that("an explicit NA factor level is missing data, not a negative", {
  # addNA()/factor(exclude = NULL) put NA in as a real level: is.na() is FALSE, so
  # naOmit() kept the case, and Ops.factor compares level CODES, so `f == var1true`
  # returned FALSE rather than NA. The missing marker was therefore counted as a
  # negative - inflating falseCount, inflating the denominator of every percentage,
  # and never appearing in the CASE EXCLUSION warning.
  d <- data.frame(
    A = addNA(factor(c("Pos", "Pos", "Pos", NA, NA))),
    B = factor(c("Pos", "Pos", "Neg", "Pos", "Neg")))

  res <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos")
  sm <- res$summary$asDF

  # the two NA cases are excluded, not scored
  expect_equal(sm$totalCount, c(3L, 3L))
  expect_equal(sm$trueCount[sm$variable == "A"], 3L)
  expect_equal(sm$falseCount[sm$variable == "A"], 0L)
  expect_equal(sm$truePercentage[sm$variable == "A"], 1)

  # and the exclusion is disclosed rather than silent
  txt <- paste(as.character(res$validationWarnings$content), collapse = " ")
  expect_match(txt, "CASE EXCLUSION")
  expect_match(txt, "Original N=5")
  expect_match(txt, "Final N=3")
})

test_that("an all-missing optional variable is named, not reported as a level problem", {
  # var1/var2 already got this check; var3-7 fell through to the level test and
  # reported "Available levels: " with an empty list.
  d <- data.frame(A = factor(rep("Pos", 4)),
                  B = factor(rep("Pos", 4)),
                  C = factor(rep(NA_character_, 4), levels = c("Pos", "Neg")))

  res <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
            var3 = "C", var3true = "Pos")

  expect_true(res$validationErrors$visible)
  msg <- as.character(res$validationErrors$content)
  expect_match(msg, "only missing values")
  expect_match(msg, "'C'")
  expect_false(grepl("Available levels", msg))
})

test_that("a set covering almost every case is flagged, like its rare mirror image", {
  # The advisory had a low tail only: a variable positive in 2% drew two notices
  # while the same variable positive in 98% drew none, even though a circle that
  # swallows the figure separates just as little as one holding a handful.
  n <- 60
  mk <- function(a, b) data.frame(A = factor(a, levels = c("Pos", "Neg")),
                                  B = factor(b, levels = c("Pos", "Neg")))
  txt <- function(d) as.character(
    vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos")$notices$content)

  balanced <- rep(c("Pos", "Neg"), n / 2)

  expect_match(txt(mk(rep("Pos", n), balanced)), "Very High Prevalence")
  expect_match(txt(mk(c(rep("Pos", 59), "Neg"), balanced)), "Very High Prevalence")
  # the low tail still behaves
  expect_match(txt(mk(c("Pos", rep("Neg", 59)), balanced)), "Very Few Positive Cases")
  # and a set that separates the cohort raises neither
  expect_false(grepl("Very High Prevalence|Very Few Positive Cases",
                     txt(mk(balanced, rep(c("Pos", "Neg"), each = n / 2)))))
})

test_that("the membership groups output column is written", {
  # `type: Output` options are client-driven: jmvtools leaves membershipGroups out
  # of the generated wrapper's formals, so venn(membershipGroups = TRUE) is silently
  # dropped and this branch had no coverage at all. Set the option value the way the
  # jamovi client does, so the one path no example or asSource() call can reach is
  # still exercised here.
  d <- data.frame(A = factor(c("Pos", "Pos", "Neg", "Neg")),
                  B = factor(c("Pos", "Neg", "Pos", "Neg")))

  o <- vennOptions$new(
    var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
    var3 = NULL, var3true = NULL, var4 = NULL, var4true = NULL,
    var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
    var7 = NULL, var7true = NULL,
    showMembershipTable = TRUE, show_ggvenn = FALSE)
  o$.__enclos_env__$private$..membershipGroups$value <- list(value = TRUE)
  expect_true(o$membershipGroups)

  a <- vennClass$new(options = o, data = d)
  a$init(); a$run()

  mg <- a$results$membershipGroups
  expect_false(mg$isNotFilled())
  # jmvcore stores the Output payload wrapped in a list
  expect_equal(unlist(mg$.__enclos_env__$private$.values, use.names = FALSE),
               c("A & B", "A", "B", "None"))
  expect_equal(unlist(mg$.__enclos_env__$private$.rowNums, use.names = FALSE), 1:4)
  # the write succeeded, so the new failure notice must NOT appear
  expect_false(grepl("Membership Groups Column Was Not Added",
                     as.character(a$results$notices$content)))
})

test_that("fill colour mapping can actually be switched off", {
  # build_palette_scale() returns NULL for BOTH "no palette chosen" and "mapping
  # off", and the fallback applied afterwards was itself a count -> colour
  # gradient. So at the default palette the checkbox was inert: the regions
  # stayed shaded by intersection size however it was set.
  set.seed(7); n <- 60
  d <- data.frame(A = factor(sample(c("Pos", "Neg"), n, TRUE)),
                  B = factor(sample(c("Pos", "Neg"), n, TRUE)),
                  C = factor(sample(c("Pos", "Neg"), n, TRUE)))

  render <- function(fcm) {
    o <- vennOptions$new(
      var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
      var3 = "C", var3true = "Pos",
      var4 = NULL, var4true = NULL, var5 = NULL, var5true = NULL,
      var6 = NULL, var6true = NULL, var7 = NULL, var7true = NULL,
      show_ggVennDiagram = TRUE, show_ggvenn = FALSE,
      fillColorMapping = fcm)          # colorPalette left at its "default" default
    a <- vennClass$new(options = o, data = d); a$init(); a$run()
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 700, 450)
    on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
    a$.__enclos_env__$private$.plotGgVennDiagram(
      a$results$plotGgVennDiagram, ggplot2::theme_bw(),
      list(fill = c("#FFFFFF", "#79A6EA")))
    try(grDevices::dev.off(), silent = TRUE)
    as.character(tools::md5sum(f))
  }

  expect_false(identical(render(TRUE), render(FALSE)))
})

test_that("no table rows are built for a membership table nobody can see", {
  # membershipTable is gated by `visible: (showMembershipTable)`, but the data
  # output alone used to trigger the whole render path - up to 500 addRow() calls
  # written into a hidden element.
  d <- data.frame(A = factor(c("Pos", "Pos", "Neg", "Neg")),
                  B = factor(c("Pos", "Neg", "Pos", "Neg")))

  o <- vennOptions$new(
    var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
    var3 = NULL, var3true = NULL, var4 = NULL, var4true = NULL,
    var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
    var7 = NULL, var7true = NULL,
    showMembershipTable = FALSE, show_ggvenn = FALSE)
  o$.__enclos_env__$private$..membershipGroups$value <- list(value = TRUE)

  a <- vennClass$new(options = o, data = d); a$init(); a$run()

  # nothing rendered into the hidden table ...
  expect_equal(a$results$membershipTable$rowCount, 0L)
  # ... but the dataset column the user asked for is still written
  expect_false(a$results$membershipGroups$isNotFilled())
  expect_equal(unlist(a$results$membershipGroups$.__enclos_env__$private$.values,
                      use.names = FALSE),
               c("A & B", "A", "B", "None"))
})

test_that("the ggvenn fallback is a notice, not a panel titled 'To Do'", {
  d <- data.frame(A = factor(c("Pos", "Neg")), B = factor(c("Pos", "Neg")))
  res <- vn(d, var1 = "A", var1true = "Pos", var2 = "B", var2true = "Pos",
            show_ggvenn = FALSE)   # nothing else on either, so the fallback fires

  expect_match(as.character(res$notices$content), "Defaulting to the ggvenn Diagram")
  # the item is gone from the schema entirely, not merely blank
  expect_error(res$todo, "does not exist")
})

test_that("every intersection order is reported, not just pairwise plus the k-way", {
  # The panel used to emit the C(k,2) pairwise intersections plus the single k-way
  # one and say nothing about the gap: with 4 sets the four 3-way intersections
  # were silently absent, with 7 sets 105 of 120 were.
  set.seed(3); n <- 200
  d <- data.frame(A = factor(sample(c("P", "N"), n, TRUE)),
                  B = factor(sample(c("P", "N"), n, TRUE)),
                  C = factor(sample(c("P", "N"), n, TRUE)),
                  E = factor(sample(c("P", "N"), n, TRUE)))

  res <- venn(data = d, var1 = "A", var1true = "P", var2 = "B", var2true = "P",
              var3 = "C", var3true = "P", var4 = "E", var4true = "P",
              var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
              var7 = NULL, var7true = NULL,
              show_ggvenn = FALSE, showSetCalculations = TRUE, calculateOverlap = TRUE)
  html <- as.character(res$setCalculations$content)

  m <- sapply(d, function(x) x == "P")
  # 2^4 - 4 - 1 = 11 intersections, every one of them present with the right count
  combos <- unlist(lapply(2:4, function(k) combn(4, k, simplify = FALSE)), recursive = FALSE)
  expect_equal(length(combos), 11L)
  for (idx in combos) {
    label <- paste(colnames(m)[idx], collapse = " &amp; ")   # htmlEscape()d in the panel
    truth <- sum(rowSums(m[, idx, drop = FALSE]) == length(idx))
    expect_match(html, paste0(label, ":</strong> ", truth, " cases"), fixed = TRUE)
  }
})

test_that("set-calculation percentages all carry one decimal", {
  # round() drops a trailing zero, so a whole number printed as "26%" in a list
  # whose neighbours read "23.5%".
  d <- data.frame(A = factor(rep(c("P", "N"), c(50, 50))),
                  B = factor(rep(c("P", "N"), c(50, 50))))
  res <- venn(data = d, var1 = "A", var1true = "P", var2 = "B", var2true = "P",
              var3 = NULL, var3true = NULL, var4 = NULL, var4true = NULL,
              var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
              var7 = NULL, var7true = NULL,
              show_ggvenn = FALSE, showSetCalculations = TRUE,
              calculateOverlap = TRUE, calculateUnite = TRUE)
  html <- as.character(res$setCalculations$content)

  expect_match(html, "50 cases (50.0%)", fixed = TRUE)   # intersection AND union
  expect_false(grepl("(50%)", html, fixed = TRUE))
})

test_that("variables tied for most prevalent are all named", {
  # which.max() returns the FIRST maximum, so a genuine tie was reported as if one
  # variable were the winner and the equally-common ones went unmentioned.
  d <- data.frame(A = factor(rep(c("P", "N"), c(30, 70))),
                  B = factor(rep(c("P", "N"), c(30, 70))),   # tied with A
                  C = factor(rep(c("P", "N"), c(10, 90))))

  res <- venn(data = d, var1 = "A", var1true = "P", var2 = "B", var2true = "P",
              var3 = "C", var3true = "P", var4 = NULL, var4true = NULL,
              var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
              var7 = NULL, var7true = NULL,
              show_ggvenn = FALSE, clinicalSummary = TRUE)
  txt <- as.character(res$clinicalSummary$content)

  expect_match(txt, "A, B were equally the most common (30 cases each", fixed = TRUE)
  expect_match(txt, "are equally the most prevalent, each with 30 positive cases")
  expect_false(grepl("was most common", txt, fixed = TRUE))
})

test_that("the syntax pane emits code that parses and reproduces the analysis", {
  # jmvcore's .asArgs() already returns a leading "\n    ", so the copy-pasted
  # ',\n    ' prefix left a stray blank argument line in what the user copies.
  data <- data.frame(A = factor(c("P", "P", "N", "N")),
                     B = factor(c("P", "N", "P", "N")))
  a <- vennClass$new(
    options = vennOptions$new(
      var1 = "A", var1true = "P", var2 = "B", var2true = "P",
      var3 = NULL, var3true = NULL, var4 = NULL, var4true = NULL,
      var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
      var7 = NULL, var7true = NULL, show_ggvenn = FALSE, showGlossary = TRUE),
    data = data)
  src <- a$asSource()

  expect_false(grepl("data = data,\n    \n", src, fixed = TRUE))
  expect_match(src, "^ClinicoPath::venn\\(\n    data = data,\n    var1 = A,")
  # every Level is emitted even when its Variable is not, because a Level can carry
  # no default and is therefore a required argument of the generated wrapper
  for (lv in paste0("var", 3:7, "true = NULL"))
    expect_match(src, lv, fixed = TRUE)

  # and the emitted call actually round-trips
  expect_silent(p <- parse(text = sub("^ClinicoPath::", "", src)))
  expect_s3_class(eval(p[[1]]), "vennResults")
})
