# Tests for crosstable analysis

# Load required library

test_that("crosstable works", {
  skip_if_not_installed('jmvReadWrite')
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test basic functionality with histopathology data
  expect_error(
    crosstable(
      data = histopathology,
      vars = c("Sex", "Grade"),
      group = "Group",
      sty = "nejm"
    ),
    NA
  )
  
  # Test different styles
  styles <- c("arsenal", "finalfit", "gtsummary", "nejm", "lancet", "hmisc")
  
  for (style in styles) {
    test_that(paste("crosstable works with", style, "style"), {
      expect_error(
        crosstable(
          data = histopathology,
          vars = c("Sex", "Grade"),
          group = "Group",
          sty = style
        ),
        NA
      )
    })
  }
  
  # Test with missing value exclusion
  test_that("crosstable works with missing value exclusion", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Sex", "Grade"),
        group = "Group",
        sty = "nejm",
        excl = TRUE
      ),
      NA
    )
  })
  
  # Test with different continuous variable summary methods
  test_that("crosstable works with different continuous methods", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Age", "Sex"),
        group = "Group",
        sty = "finalfit",
        cont = "mean"
      ),
      NA
    )
    
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Age", "Sex"),
        group = "Group",
        sty = "finalfit",
        cont = "median"
      ),
      NA
    )
  })
  
  # Test with different categorical test methods
  test_that("crosstable works with different categorical tests", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Sex", "Grade"),
        group = "Group",
        sty = "finalfit",
        pcat = "chisq"
      ),
      NA
    )
    
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Sex", "Grade"),
        group = "Group",
        sty = "finalfit",
        pcat = "fisher"
      ),
      NA
    )
  })
  
  # Test with single variable
  test_that("crosstable works with single variable", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = "Sex",
        group = "Group",
        sty = "nejm"
      ),
      NA
    )
  })
  
  # Test with multiple variables
  test_that("crosstable works with multiple variables", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Sex", "Grade", "Race"),
        group = "Group",
        sty = "gtsummary"
      ),
      NA
    )
  })
  
  # Test with continuous and categorical variables mixed
  test_that("crosstable works with mixed variable types", {
    expect_error(
      crosstable(
        data = histopathology,
        vars = c("Age", "Sex", "Grade"),
        group = "Group",
        sty = "gtsummary"
      ),
      NA
    )
  })
  
  # Test error conditions
  test_that("crosstable handles errors appropriately", {
    # `vars` and `group` both carry default: NULL, so supplying only one is a
    # SUPPORTED state: .run() shows the Welcome panel rather than throwing.
    # expect_error() here asserted the opposite of the intended behaviour.
    res_vars <- crosstable(data = histopathology, vars = "Sex")
    expect_s3_class(res_vars, "crosstableResults")
    expect_match(as.character(res_vars$todo$content), "Welcome to Cross Table Analysis")

    res_group <- crosstable(data = histopathology, group = "Group")
    expect_match(as.character(res_group$todo$content), "Welcome to Cross Table Analysis")

    # with both supplied, a table is produced (gtsummary is the default style)
    res_both <- crosstable(data = histopathology, vars = "Sex", group = "Group")
    expect_gt(nchar(as.character(res_both$tablestyle3$content)), 0)
  })
  
})


test_that("crosstable SMD balance column computes correctly", {
  skip_if_not_installed("jmvcore")

  set.seed(1)
  df <- data.frame(
    grp  = factor(rep(c("A", "B"), c(200, 180))),
    xcon = c(rnorm(200, 10, 2), rnorm(180, 11, 2.2)),
    xbin = factor(c(rbinom(200, 1, 0.3), rbinom(180, 1, 0.45))),
    xcat = factor(c(sample(c("L1","L2","L3"), 200, TRUE, c(.5,.3,.2)),
                    sample(c("L1","L2","L3"), 180, TRUE, c(.4,.3,.3)))))

  expect_no_error({
    model <- crosstable(
      data = df, vars = c("xcon", "xbin", "xcat"), group = "grp",
      sty = "gtsummary", showSMD = TRUE)
  })
  expect_s3_class(model, "crosstableResults")

  smd <- model$smdTable$asDF
  expect_equal(nrow(smd), 3)
  # continuous SMD ~ -0.49 (magnitude ~0.49)
  expect_equal(round(abs(smd$absSMD[smd$variable == "xcon"]), 1), 0.5)
  # types classified
  expect_equal(smd$vtype[smd$variable == "xcon"], "continuous")
  expect_equal(smd$vtype[smd$variable == "xcat"], "categorical")
  # all |SMD| finite and >= 0
  expect_true(all(smd$absSMD >= 0 & is.finite(smd$absSMD)))
})

test_that("crosstable SMD requires exactly two groups", {
  skip_if_not_installed("jmvcore")
  set.seed(2)
  df <- data.frame(
    grp = factor(sample(c("A", "B", "C"), 150, TRUE)),
    x   = rnorm(150))
  model <- crosstable(data = df, vars = "x", group = "grp",
                      sty = "gtsummary", showSMD = TRUE)
  smd <- model$smdTable$asDF
  expect_true(is.na(smd$smd[1]))
  expect_match(model$smdTable$notes$smd$note, "require exactly two groups")
})

test_that("crosstable SMD uses the same exclusion set as the main table", {
  skip_if_not_installed("jmvcore")

  df <- data.frame(
    grp = factor(rep(c("A", "B"), each = 3)),
    x = c(0, 0, 10, 1, 1, 1),
    auxiliary = c(NA, 1, 1, 1, 1, 1)
  )

  pairwise <- crosstable(
    data = df,
    vars = c("x", "auxiliary"),
    group = "grp",
    sty = "gtsummary",
    showSMD = TRUE,
    excl = FALSE
  )
  complete_case <- crosstable(
    data = df,
    vars = c("x", "auxiliary"),
    group = "grp",
    sty = "gtsummary",
    showSMD = TRUE,
    excl = TRUE
  )

  pairwise_x <- pairwise$smdTable$asDF
  pairwise_x <- pairwise_x$absSMD[pairwise_x$variable == "x"]
  complete_x <- complete_case$smdTable$asDF
  complete_x <- complete_x$absSMD[complete_x$variable == "x"]

  expect_equal(pairwise_x, abs((10 / 3 - 1) / sqrt((100 / 3) / 2)))
  expect_equal(complete_x, abs((5 - 1) / sqrt(50 / 2)))
})

test_that("variable names with spaces, punctuation and Unicode survive the name-cleaning round trip", {
  # .labelData() runs janitor::clean_names() and maps the cleaned names back to
  # the originals through labelled::var_label(). A regression there shows up
  # either as a "could not be matched" rejection or as cleaned names (ki_67,
  # tumor_grade) leaking into the rendered table in place of the user's labels.
  set.seed(1)
  ki67  <- "Ki-67 (%)"
  grade <- "Tumor Grade"
  size  <- "Größe [cm]"
  grp   <- "Sex at birth"
  d <- data.frame(
    rnorm(60, 20, 5),
    factor(sample(c("Low", "High"), 60, TRUE)),
    rnorm(60, 3, 1),
    factor(rep(c("F", "M"), 30)),
    check.names = FALSE
  )
  names(d) <- c(ki67, grade, size, grp)

  item <- c(arsenal = "tablestyle1", finalfit = "tablestyle2",
            gtsummary = "tablestyle3", nejm = "tablestyle4")
  txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

  for (sty in names(item)) {
    # do.call() inlines the evaluated strings: the wrapper's resolveQuo() would
    # otherwise read bare symbols like `grp` as a column literally named "grp".
    res <- do.call(crosstable, list(data = d, vars = c(ki67, grade, size),
                                    group = grp, sty = sty))
    out <- txt(res[[item[[sty]]]]$content)
    expect_false(res$errorNotice$visible, info = sty)
    expect_match(out, "Ki-67", fixed = TRUE, info = sty)
    expect_match(out, grade, fixed = TRUE, info = sty)
    expect_false(grepl("ki_67|tumor_grade|grosse_cm|sex_at_birth", out), info = sty)
  }
})

# --- audit fixes (2026-09-05: /check-function-full crosstable) ----------------
ct_text <- function(x) {
  x <- gsub("<[^>]*>", " ", as.character(x))
  ents <- c("&lt;" = "<", "&gt;" = ">", "&amp;" = "&")
  for (e in names(ents)) x <- gsub(e, ents[[e]], x, fixed = TRUE)
  trimws(gsub("[[:space:]]+", " ", x))
}

test_that("missing-value exclusion reports the rows it removed; missingness is judged before exclusion", {
  set.seed(11)
  d <- data.frame(grp = factor(rep(c("A", "B"), each = 10)), x = c(rnorm(14), rep(NA, 6)))
  # 6 of 20 rows (30%) carry a missing x -> WARNING, top panel
  res <- do.call(crosstable, list(data = d, vars = "x", group = "grp", sty = "arsenal", excl = TRUE))
  expect_true(res$notices$visible)
  expect_match(res$notices$content, "removed 6 of 20 rows")
  # the > 20% missing warning used to be computed AFTER naOmit, so it could never fire
  expect_match(ct_text(res$dataQualityNotice$content), "High missing data in x")
  # a small exclusion is an INFO note below the table, not a warning above it
  d2 <- d; d2$x[15:20] <- 1; d2$x[1] <- NA
  res2 <- do.call(crosstable, list(data = d2, vars = "x", group = "grp", sty = "arsenal", excl = TRUE))
  expect_match(res2$notes$content, "removed 1 of 20 rows")
  expect_false(grepl("removed 1 of 20", res2$notices$content))
})

test_that("low expected counts escalate only when a chi-square will actually be run", {
  # smallest expected count 1.33
  d <- data.frame(g = factor(rep(c("A", "B"), c(4, 8))),
                  c = factor(c("y", "y", "n", "n", "y", "n", "n", "n", "n", "n", "n", "y")))
  chi <- ct_text(crosstable(data = d, vars = "c", group = "g", sty = "arsenal", pcat = "chisq")$dataQualityNotice$content)
  expect_match(chi, "STRONG_WARNING: Low expected counts")
  fis <- ct_text(crosstable(data = d, vars = "c", group = "g", sty = "arsenal", pcat = "fisher")$dataQualityNotice$content)
  expect_false(grepl("Low expected counts", fis))
  gts <- ct_text(crosstable(data = d, vars = "c", group = "g", sty = "gtsummary")$dataQualityNotice$content)
  expect_match(gts, "WARNING: Low expected counts")
  expect_false(grepl("STRONG_WARNING: Low expected counts", gts))
  expect_match(gts, "Fisher's exact test automatically")
  nejm <- ct_text(crosstable(data = d, vars = "c", group = "g", sty = "nejm")$dataQualityNotice$content)
  expect_match(nejm, "STRONG_WARNING: Low expected counts")
})

test_that("INFO notes render below the table, errors above it", {
  data("histopathology", package = "ClinicoPath")
  # Grade is numeric 1-4: the coded-category note is INFO -> bottom `notes` item
  res <- crosstable(data = histopathology, vars = c("Age", "Grade"), group = "Group", sty = "gtsummary")
  expect_true(res$notes$visible)
  expect_match(res$notes$content, "coded categories")
  expect_match(res$notes$content, "gtsummary style")
  expect_false(grepl("coded categories", res$notices$content))
  # a one-level grouping variable is an ERROR -> top `notices` item, nothing below
  one <- data.frame(g = factor(rep("A", 30)), x = rnorm(30))
  err <- crosstable(data = one, vars = "x", group = "g")
  expect_match(err$notices$content, "^ERROR: Only one group")
  expect_false(err$notes$visible)
})

test_that("numeric codes are labelled as such in the SMD table", {
  set.seed(3)
  d <- data.frame(grp = factor(rep(c("A", "B"), each = 40)),
                  grade = sample(1:3, 80, TRUE), age = rnorm(80, 60, 10))
  smd <- crosstable(data = d, vars = c("grade", "age"), group = "grp",
                    sty = "gtsummary", showSMD = TRUE)$smdTable$asDF
  expect_equal(smd$vtype[smd$variable == "grade"], "continuous (numeric codes)")
  expect_equal(smd$vtype[smd$variable == "age"], "continuous")
  expect_false(is.na(smd$smd[smd$variable == "grade"]))
})

test_that("gtsummary is the default style", {
  data("histopathology", package = "ClinicoPath")
  res <- crosstable(data = histopathology, vars = "Sex", group = "Group")
  expect_gt(nchar(as.character(res$tablestyle3$content)), 0)
  expect_false(res$tablestyle4$visible)
})

# --- review-function recommendations (2026-09-05) -----------------------------

test_that("the plain-language summary names the tests used and the variables that differed", {
  d <- data.frame(grp = factor(rep(c("A", "B"), c(20, 20))),
                  cat = factor(c(rep("Yes", 2), rep("No", 18), rep("Yes", 8), rep("No", 12))))
  s <- function(...) ct_text(crosstable(data = d, vars = "cat", group = "grp", showSummary = TRUE, ...)$summary$content)
  # arsenal: uncorrected Pearson 0.028 -> listed; Fisher 0.065 -> not
  expect_match(s(sty = "arsenal", pcat = "chisq"), "cat \\(p = 0.028\\)")
  expect_match(s(sty = "arsenal", pcat = "chisq"), "Pearson's chi-square")
  expect_match(s(sty = "arsenal", pcat = "fisher"), "No variable differed")
  expect_match(s(sty = "arsenal", pcat = "fisher"), "Fisher's exact test")
  # finalfit: Yates-corrected 0.068 -> not listed
  expect_match(s(sty = "finalfit", pcat = "chisq"), "No variable differed")
  # gtsummary: smallest expected count is exactly 5, so no automatic switch: 0.028
  expect_match(s(sty = "gtsummary"), "cat \\(p = 0.028\\)")
  expect_match(s(sty = "gtsummary"), "A n = 20; B n = 20")
  # tangram styles: the recomputed Pearson p must agree with the P= the table prints
  res <- crosstable(data = d, vars = "cat", group = "grp", sty = "nejm", showSummary = TRUE)
  shown <- as.numeric(sub(".*P=([0-9.]+).*", "\\1", ct_text(res$tablestyle4$content)))
  expect_equal(shown, 0.03)
  expect_match(ct_text(res$summary$content), "cat \\(p = 0.028\\)")
  # hidden unless requested
  expect_false(crosstable(data = d, vars = "cat", group = "grp")$summary$visible)
})

test_that("the summary reports exclusions and, under adjustment, q-values", {
  data("histopathology", package = "ClinicoPath")
  res <- crosstable(data = histopathology, vars = c("Age", "Sex", "Grade"), group = "Group",
                    sty = "gtsummary", p_adjust = "BH", excl = TRUE, showSummary = TRUE)
  s <- ct_text(res$summary$content)
  expect_match(s, "excluded for missing values")
  expect_match(s, "Benjamini-Hochberg")
  expect_match(s, "q < 0.05")
  expect_match(s, "Wilcoxon rank-sum")
})

test_that("skewed continuous variables shown as Mean (SD) get a note", {
  set.seed(5)
  d <- data.frame(grp = factor(rep(c("A", "B"), each = 50)),
                  ki = exp(rnorm(100, 2, 1)), age = rnorm(100, 60, 10))
  m <- crosstable(data = d, vars = c("ki", "age"), group = "grp", sty = "arsenal")
  expect_match(m$notes$content, "look markedly skewed")
  listed <- sub(".*skewness coefficient above 1\\): ([^.]*)\\..*", "\\1", m$notes$content)
  expect_equal(trimws(listed), "ki")
  md <- crosstable(data = d, vars = c("ki", "age"), group = "grp", sty = "arsenal", cont = "median")
  expect_false(grepl("skewed", md$notes$content))
})

# --- release review 2026-09-05: inputs that used to crash an engine -----------
ct_main <- c(arsenal = "tablestyle1", finalfit = "tablestyle2", gtsummary = "tablestyle3", nejm = "tablestyle4")

test_that("all-missing and single-valued variables are reported, not surfaced as an engine error", {
  set.seed(9)
  d <- data.frame(grp = factor(rep(c("A", "B"), each = 15)), x = rnorm(30),
                  gone = NA_real_, one = factor("only"))
  for (sty in names(ct_main)) {
    res <- crosstable(data = d, vars = c("x", "gone", "one"), group = "grp", sty = sty)
    expect_false(res$errorNotice$visible, info = sty)
    expect_match(res$notices$content, "no non-missing values", info = sty)
    expect_match(res$notices$content, "single value", info = sty)
    expect_gt(nchar(as.character(res[[ct_main[[sty]]]]$content)), 0)
  }
  # finalfit is the one engine that cannot hold the single-valued row
  only <- crosstable(data = d, vars = "one", group = "grp", sty = "finalfit")
  expect_match(only$notices$content, "Nothing left to tabulate")
  expect_false(only$errorNotice$visible)
  expect_gt(nchar(as.character(crosstable(data = d, vars = "one", group = "grp", sty = "arsenal")$tablestyle1$content)), 0)
})

test_that("an empty level of the grouping variable does not become a column", {
  set.seed(10)
  d <- data.frame(grp = factor(rep(c("A", "B"), each = 20), levels = c("A", "B", "Ghost")),
                  x = rnorm(40), b = factor(rep(c("y", "n"), 20)))
  for (sty in names(ct_main)) {
    t <- ct_text(crosstable(data = d, vars = c("x", "b"), group = "grp", sty = sty)[[ct_main[[sty]]]]$content)
    expect_false(grepl("Ghost", t, fixed = TRUE), info = sty)
  }
})
