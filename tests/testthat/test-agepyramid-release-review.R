# Regression cover for the defects found during the agepyramid release review.
# Each block fails against the pre-review backend.

library(testthat)

ap <- function(d, ...) agepyramid(data = d, age = "age", gender = "gender",
                                  female = "Female", male = "Male", ...)
bands <- function(res) {
  df <- res$pyramidTable$asDF
  df[df$Pop != "Total", , drop = FALSE]
}

test_that("a uniform population produces equal bars", {
  # THE decisive defect. cut(..., right = TRUE) with include.lowest widened the
  # first band: [0,5] holds six single-year ages while (5,10] holds five, so on a
  # uniform population the bottom bar came out 20% too tall and the top bar 20%
  # too short. The bottom bar of an age pyramid is the birth cohort.
  d <- data.frame(age = rep(0:19, each = 2),
                  gender = rep(c("Female", "Male"), 20))
  b <- bands(ap(d, bin_width = 5))

  expect_equal(nrow(b), 4L)
  expect_true(all(b$Female == 5), info = paste(b$Female, collapse = ","))
  expect_true(all(b$Male == 5), info = paste(b$Male, collapse = ","))

  # every band must span the same number of single years
  spans <- vapply(as.character(b$Pop), function(lab) {
    p <- as.numeric(strsplit(lab, "-")[[1]]); p[2] - p[1] + 1
  }, numeric(1))
  expect_true(all(spans == 5), info = paste(spans, collapse = ","))
})

test_that("each preset's boundary age falls in the band the preset names", {
  # "Geriatric (65+)" put a 65-year-old in the band labelled "1-65" - not
  # geriatric at all. "Reproductive (15-50)" put a 15-year-old in "1-15".
  # "Pediatric (<18)" counted an 18-year-old as paediatric.
  cases <- list(
    list(preset = "geriatric",    age = 65, want = "^65-"),
    list(preset = "reproductive", age = 15, want = "^15-"),
    list(preset = "pediatric",    age = 18, want = "^18\\+$")
  )
  for (cs in cases) {
    d <- data.frame(age = rep(cs$age, 2), gender = c("Female", "Male"))
    b <- bands(ap(d, age_groups = cs$preset))
    occupied <- as.character(b$Pop[(b$Female + b$Male) > 0])
    expect_true(any(grepl(cs$want, occupied)),
                info = sprintf("%s: age %d -> %s", cs$preset, cs$age,
                               paste(occupied, collapse = ", ")))
  }

  # an 18-year-old must NOT be counted as paediatric
  d18 <- data.frame(age = c(17, 18), gender = c("Female", "Male"))
  b18 <- bands(ap(d18, age_groups = "pediatric"))
  expect_equal(b18$Male[as.character(b18$Pop) == "18+"], 1)
  expect_equal(b18$Female[as.character(b18$Pop) == "15-17"], 1)
})

test_that("the first band's label includes age zero", {
  # cut()'s include.lowest widened band 1 to [0, 5] while the label read "1-5",
  # so every age-0 infant was displayed under a label that excluded it.
  d <- data.frame(age = c(0, 0, 3, 3), gender = rep(c("Female", "Male"), 2))
  b <- bands(ap(d, bin_width = 5))

  labs <- as.character(b$Pop)
  expect_true(any(grepl("^0-", labs)), info = paste(labs, collapse = ", "))
  expect_false(any(grepl("^1-5$", labs)))
  # and the age-0 rows are actually counted there
  expect_equal(sum(b$Female) + sum(b$Male), 4)
})

test_that("labels describe exactly the ages their band contains", {
  # The invariant the earlier right=TRUE fix protected, kept and checked directly.
  d <- data.frame(age = rep(0:24, each = 2),
                  gender = rep(c("Female", "Male"), 25))
  b <- bands(ap(d, bin_width = 5))

  for (i in seq_len(nrow(b))) {
    lab <- as.character(b$Pop[i])
    if (!grepl("^[0-9]+-[0-9]+$", lab)) next
    p <- as.numeric(strsplit(lab, "-")[[1]])
    expect_equal(b$Female[i], p[2] - p[1] + 1, info = lab)
    expect_equal(b$Male[i], p[2] - p[1] + 1, info = lab)
  }
})

test_that("the open-ended band is named for its lower bound", {
  d <- data.frame(age = c(96, 100), gender = c("Female", "Male"))
  labs <- as.character(bands(ap(d, age_groups = "geriatric"))$Pop)
  expect_true(any(grepl("^95\\+$", labs)))
  expect_false(any(grepl("^96\\+$", labs)))
})

test_that("the welcome panel hides once both variables are chosen", {
  # `visible: (!age || !gender)` never worked: jmvcore only treats a visible
  # string as an expression when it starts with "(" plus a letter, so the leading
  # "!" left an empty "Getting Started" box on screen for every run.
  d <- data.frame(age = c(1, 20, 40, 60), gender = c("F", "M", "F", "M"))
  res <- agepyramid(data = d, age = "age", gender = "gender",
                    female = "F", male = "M")
  expect_false(res$welcome$visible)

  # and it IS shown, with content, when a variable is missing
  res2 <- agepyramid(data = d, gender = "gender", female = "F", male = "M")
  expect_true(res2$welcome$visible)
  expect_gt(nchar(as.character(res2$welcome$content)), 0)
})

test_that("percentages are within-sex column percentages summing to 100", {
  set.seed(4)
  d <- data.frame(age = c(rep(10, 3), rep(30, 5), rep(50, 2)),
                  gender = c(rep("Female", 6), rep("Male", 4)))
  b <- bands(ap(d, bin_width = 20))

  expect_equal(sum(b$Female), 6)
  expect_equal(sum(b$Male), 4)
  # each sex's percentages are of that sex's own total
  expect_equal(sum(b$Female_Pct), 100, tolerance = 0.05)
  expect_equal(sum(b$Male_Pct), 100, tolerance = 0.05)
  for (i in seq_len(nrow(b))) {
    expect_equal(b$Female_Pct[i], 100 * b$Female[i] / 6, tolerance = 0.05)
    expect_equal(b$Male_Pct[i], 100 * b$Male[i] / 4, tolerance = 0.05)
  }
})

test_that("counts reproduce a hand-computed cross-tabulation", {
  set.seed(9)
  age <- sample(0:89, 400, replace = TRUE)
  gender <- sample(c("Female", "Male"), 400, replace = TRUE)
  d <- data.frame(age = age, gender = gender)

  b <- bands(ap(d, bin_width = 10))

  brk <- seq(0, max(age), by = 10)
  if (max(age) > tail(brk, 1)) brk <- c(brk, max(age))
  ref <- table(cut(age, brk, include.lowest = TRUE, right = FALSE), gender)

  expect_equal(sum(b$Female), sum(gender == "Female"))
  expect_equal(sum(b$Male), sum(gender == "Male"))
  expect_equal(sum(b$Female) + sum(b$Male), nrow(d))
  expect_equal(sort(b$Female), sort(as.vector(ref[, "Female"])))
  expect_equal(sort(b$Male), sort(as.vector(ref[, "Male"])))
})

# ---------------------------------------------------------------------------
# WHO/UN standard age groups.
#
# The left-closed binning above IS the WHO/UN convention; these blocks cover the
# standard GROUPINGS added on top of it. The wrapper cannot accept the new
# `age_groups` values until jmvtools::prepare() regenerates the OptionList, so
# the band arithmetic is exercised through the label helper and the declaration
# is checked in the .a.yaml.
# ---------------------------------------------------------------------------

ap_labels <- function(breaks) {
  ns <- asNamespace("ClinicoPath")
  obj <- get("agepyramidClass", ns)$new(
    options = get("agepyramidOptions", ns)$new(
      age = "age", gender = "gender", female = "F", male = "M"),
    data = data.frame(age = 1:10, gender = rep(c("F", "M"), 5)))
  obj$.__enclos_env__$private$.create_age_labels(breaks)
}

test_that("the WHO/UN standard grouping is the published one", {
  # WHO World Standard Population / UN population-pyramid convention:
  # 0-4, 5-9, ... 80-84, 85+  (Ahmad OB et al., WHO GPE Discussion Paper 31, 2001)
  who <- c(seq(0, 85, by = 5), Inf)
  labs <- ap_labels(who)

  expect_equal(length(labs), 18L)
  expect_equal(labs[1], "0-4")
  expect_equal(labs[2], "5-9")
  expect_equal(labs[17], "80-84")
  expect_equal(labs[18], "85+")
  # every closed band spans exactly five single years
  closed <- labs[grepl("^[0-9]+-[0-9]+$", labs)]
  spans <- vapply(closed, function(l) {
    p <- as.numeric(strsplit(l, "-")[[1]]); p[2] - p[1] + 1
  }, numeric(1))
  expect_true(all(spans == 5))
})

test_that("the WHO abridged grouping separates infants", {
  # <1, 1-4, 5-9, ... 85+ - the WHO abridged life-table groups
  who_i <- c(0, 1, seq(5, 85, by = 5), Inf)
  labs <- ap_labels(who_i)

  expect_equal(labs[1], "<1")
  expect_equal(labs[2], "1-4")
  expect_equal(labs[3], "5-9")
  expect_equal(labs[length(labs)], "85+")
})

test_that("WHO bands place boundary ages in the band named for them", {
  who <- c(seq(0, 85, by = 5), Inf)
  who_i <- c(0, 1, seq(5, 85, by = 5), Inf)
  band <- function(a, brk) as.character(
    cut(a, brk, include.lowest = TRUE, right = FALSE, labels = ap_labels(brk)))

  expect_equal(band(0, who), "0-4")
  expect_equal(band(4, who), "0-4")
  expect_equal(band(5, who), "5-9")     # 5 starts its own band, not 0-4
  expect_equal(band(84, who), "80-84")
  expect_equal(band(85, who), "85+")    # the open band starts AT 85
  expect_equal(band(120, who), "85+")

  # abridged: age 0 is an infant, age 1 is not
  expect_equal(band(0, who_i), "<1")
  expect_equal(band(1, who_i), "1-4")
  expect_equal(band(4, who_i), "1-4")
})

test_that("both WHO presets are declared as selectable options", {
  opts <- yaml::read_yaml("../../jamovi/agepyramid.a.yaml")$options
  ag <- Filter(function(o) identical(o$name, "age_groups"), opts)[[1]]
  names_declared <- vapply(ag$options, function(o) o$name, character(1))

  expect_true("who" %in% names_declared)
  expect_true("who_infant" %in% names_declared)
  # the pre-existing presets must survive
  for (nm in c("custom", "pediatric", "reproductive", "geriatric", "lifecourse"))
    expect_true(nm %in% names_declared, info = nm)

  # default unchanged - switching it would move every existing user's bands again
  expect_equal(ag$default, "custom")
})

test_that("the default bin_width already yields WHO-shaped bands", {
  # custom + bin_width 5 gives left-closed five-year bands, so the default
  # behaviour is WHO-conformant for the observed age range; the `who` preset adds
  # the standard's fixed 85+ top band regardless of the data.
  d <- data.frame(age = rep(0:24, each = 2),
                  gender = rep(c("Female", "Male"), 25))
  labs <- as.character(bands(ap(d, bin_width = 5))$Pop)
  expect_true(all(c("0-4", "5-9", "10-14", "15-19") %in% labs))
})

# ---------------------------------------------------------------------------
# User-selectable interval closure.
#
# The left-closed change is a behaviour change for existing users, so the old
# convention is offered rather than removed. Both must label honestly - the
# original age-0 bug is fixed in BOTH, not just the new default.
# ---------------------------------------------------------------------------

ap_priv <- function() {
  ns <- asNamespace("ClinicoPath")
  get("agepyramidClass", ns)$new(
    options = get("agepyramidOptions", ns)$new(
      age = "age", gender = "gender", female = "F", male = "M"),
    data = data.frame(age = 1:10, gender = rep(c("F", "M"), 5)))$.__enclos_env__$private
}

test_that("age_interval is declared, defaulting to the WHO/UN convention", {
  opts <- yaml::read_yaml("../../jamovi/agepyramid.a.yaml")$options
  ai <- Filter(function(o) identical(o$name, "age_interval"), opts)
  expect_length(ai, 1)
  ai <- ai[[1]]

  expect_equal(ai$type, "List")
  expect_equal(vapply(ai$options, function(o) o$name, character(1)), c("left", "right"))
  expect_equal(ai$default, "left")   # WHO/UN standard is the default

  ui <- readLines("../../jamovi/agepyramid.u.yaml")
  expect_true(any(grepl("name: age_interval", ui)))
})

test_that("both closure conventions label exactly what they contain", {
  lab <- ap_priv()$.create_age_labels
  b <- c(0, 5, 10, 15, 20)
  ages <- 0:20                      # covers every band fully

  for (r in c(FALSE, TRUE)) {
    L <- lab(b, right = r, include_lowest = TRUE)
    f <- cut(ages, b, include.lowest = TRUE, right = r, labels = L)
    for (i in seq_along(L)) {
      held <- ages[!is.na(f) & f == L[i]]
      p <- as.numeric(strsplit(L[i], "-")[[1]])
      expect_gt(length(held), 0)
      expect_equal(min(held), p[1], info = sprintf("right=%s %s", r, L[i]))
      expect_equal(max(held), p[2], info = sprintf("right=%s %s", r, L[i]))
    }
  }
})

test_that("age zero is never hidden under either convention", {
  # The original defect: include.lowest widened band 1 to [0,5] while the label
  # read "1-5". Under right-closed the band is still wider, but the label now
  # says so ("0-5") instead of concealing it.
  lab <- ap_priv()$.create_age_labels
  b <- c(0, 5, 10, 15, 20)

  expect_equal(lab(b, right = FALSE, include_lowest = TRUE)[1], "0-4")
  expect_equal(lab(b, right = TRUE,  include_lowest = TRUE)[1], "0-5")
  expect_false(identical(lab(b, right = TRUE, include_lowest = TRUE)[1], "1-5"))
})

test_that("the two conventions differ exactly as documented", {
  lab <- ap_priv()$.create_age_labels
  b <- c(0, 5, 10, 15, 20)

  left_counts  <- as.vector(table(cut(0:19, b, include.lowest = TRUE, right = FALSE)))
  right_counts <- as.vector(table(cut(0:19, b, include.lowest = TRUE, right = TRUE)))

  expect_equal(left_counts,  c(5, 5, 5, 5))   # equal bands - WHO/UN
  expect_equal(right_counts, c(6, 5, 5, 4))   # youngest bar inflated

  expect_equal(lab(b, right = FALSE, include_lowest = TRUE),
               c("0-4", "5-9", "10-14", "15-20"))
  expect_equal(lab(b, right = TRUE, include_lowest = TRUE),
               c("0-5", "6-10", "11-15", "16-20"))
})

test_that("selecting right-closed does not disturb the WHO preset labels", {
  # The WHO groups are defined left-closed; the toggle exists for continuity with
  # older outputs, and choosing it must not silently produce non-standard groups
  # while the preset still claims to be the WHO standard.
  lab <- ap_priv()$.create_age_labels
  who <- c(seq(0, 85, by = 5), Inf)

  L_left <- lab(who, right = FALSE, include_lowest = TRUE)
  expect_equal(L_left[1], "0-4")
  expect_equal(L_left[length(L_left)], "85+")

  # under right-closed the SAME breaks no longer give the WHO groups, and the
  # labels report that honestly rather than pretending otherwise
  L_right <- lab(who, right = TRUE, include_lowest = TRUE)
  expect_equal(L_right[1], "0-5")
  expect_equal(L_right[length(L_right)], "86+")
  expect_false(identical(L_left, L_right))
})

# ---- /check-function pass (2026-09-03) ---------------------------------------

.ap_obj <- function(name) {
  if (exists(name, inherits = TRUE)) return(get(name))
  utils::getFromNamespace(name, "ClinicoPath")
}

test_that("re-running with the table rows still in place does not duplicate them", {
  # A change to plot_title or a colour re-runs .run() WITHOUT clearing the
  # Population Data table (neither is in its clearWith), and addRow() never
  # checks for an existing rowKey, so every such change doubled the table.
  d <- data.frame(age = rep(0:19, each = 2), gender = rep(c("Female", "Male"), 20))
  o <- .ap_obj("agepyramidOptions")$new(age = "age", gender = "gender",
                                        female = NULL, male = NULL, bin_width = 5)
  a <- .ap_obj("agepyramidClass")$new(options = o, data = d)
  priv <- a$.__enclos_env__$private
  priv$.run()
  n1 <- a$results$pyramidTable$rowCount
  priv$.run()
  n2 <- a$results$pyramidTable$rowCount
  expect_equal(n1, 5L)   # four bands + Total
  expect_equal(n2, n1)
})

test_that("a gender variable named 'Age' is read before the working Age column overwrites it", {
  # mydata[["Age"]] <- age_values ran before mydata[[gender]] was read, so a
  # gender column literally called "Age" was replaced by the ages and every
  # row was then dropped as "unrecognised gender".
  d <- data.frame(Yrs = rep(0:19, each = 2), Age = rep(c("Female", "Male"), 20))
  res <- agepyramid(data = d, age = "Yrs", gender = "Age",
                    female = "Female", male = "Male")
  tot <- res$pyramidTable$asDF
  tot <- tot[tot$Pop == "Total", , drop = FALSE]
  expect_equal(tot$Female, 20)
  expect_equal(tot$Male, 20)
})

test_that("unusable ages raise a warning even when few rows are affected", {
  # Negative ages were dropped with no notice unless the total exclusion
  # crossed 20%; a negative age is always a data error.
  d <- data.frame(age = c(-3, -1, rep(0:19, each = 2)), gender = rep(c("Female", "Male"), 21))
  res <- ap(d, bin_width = 5)
  expect_match(res$notices$content, "2 observation\\(s\\) had an age that is negative")
  expect_equal(bands(res)$Female + bands(res)$Male, rep(10, 4))
})

test_that("the top bin-width band is labelled by its width, like the presets", {
  # With ages up to 73 the top band [70, Inf) was labelled "70-73"; the WHO
  # preset labels the same band "70-74". Label by width so the two agree.
  d <- data.frame(age = rep(c(60, 66, 73), each = 2), gender = rep(c("Female", "Male"), 3))
  labs <- as.character(bands(ap(d, bin_width = 5))$Pop)
  expect_true("70-74" %in% labs, info = paste(labs, collapse = ","))
  expect_false("70-73" %in% labs)
  # fractional widths cannot name a whole-year top band: fall back to "lower+"
  labs2 <- as.character(bands(ap(d, bin_width = 2.5))$Pop)
  expect_true(any(grepl("\\+$", labs2)), info = paste(labs2, collapse = ","))
})

test_that("pct_base = 'total' gives gender shares that sum to 100 across both columns", {
  d <- data.frame(age = rep(0:19, each = 3), gender = rep(c("Female", "Male", "Male"), 20))  # 20 F, 40 M
  res <- ap(d, bin_width = 5, pct_base = "total")
  tab <- res$pyramidTable$asDF
  tot <- tab[tab$Pop == "Total", , drop = FALSE]
  b <- bands(res)
  expect_equal(tot$Female_Pct, 33.3)
  expect_equal(tot$Male_Pct, 66.7)
  expect_equal(sum(b$Female_Pct) + sum(b$Male_Pct), 100, tolerance = 0.2)
  expect_match(res$pyramidTable$notes$pct$note, "of all 60 analysed observations")
  # the default keeps each column at 100
  res2 <- ap(d, bin_width = 5)
  tot2 <- res2$pyramidTable$asDF
  tot2 <- tot2[tot2$Pop == "Total", , drop = FALSE]
  expect_equal(c(tot2$Female_Pct, tot2$Male_Pct), c(100, 100))
})

test_that("plot_values = 'percent' carries per-band percentages into both plot states and renders", {
  d <- data.frame(age = rep(0:19, each = 3), gender = rep(c("Female", "Male", "Male"), 20))
  o <- .ap_obj("agepyramidOptions")$new(age = "age", gender = "gender", female = NULL, male = NULL,
                                        pct_base = "total", plot_values = "percent", enableGGCharts = TRUE)
  a <- .ap_obj("agepyramidClass")$new(options = o, data = d)
  priv <- a$.__enclos_env__$private
  priv$.run()
  st <- a$results$plot$state
  expect_true("pct" %in% names(st))
  expect_equal(sum(st$pct), 100)
  expect_equal(sum(st$pct[st$Gender == "Female"]), 100 / 3, tolerance = 1e-6)
  f <- tempfile(fileext = ".png"); grDevices::png(f)
  ok <- priv$.plot(a$results$plot, ggtheme = ggplot2::theme_gray(), theme = list())
  ok2 <- priv$.plotGGCharts(a$results$plotGGCharts, ggtheme = ggplot2::theme_gray(), theme = list())
  grDevices::dev.off()
  expect_true(ok); expect_true(ok2)
  # within-gender base: each side sums to 100
  o2 <- .ap_obj("agepyramidOptions")$new(age = "age", gender = "gender", female = NULL, male = NULL,
                                         plot_values = "percent")
  a2 <- .ap_obj("agepyramidClass")$new(options = o2, data = d)
  a2$.__enclos_env__$private$.run()
  st2 <- a2$results$plot$state
  expect_equal(as.vector(tapply(st2$pct, st2$Gender, sum)), c(100, 100))
})
