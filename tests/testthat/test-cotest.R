test_that("cotest module loads correctly", {
  expect_true(exists("cotestClass"))
  expect_true(is.function(cotest))
})

test_that("cotest works with default parameters", {
  # Test basic functionality with default parameters
  result <- cotest()

  expect_s3_class(result, "cotestResults")

  # Check that results contain expected components
  expect_true("testParamsTable" %in% names(result))
  expect_true("cotestResultsTable" %in% names(result))
  expect_true("explanation" %in% names(result))
  expect_true("dependenceExplanation" %in% names(result))
})

test_that("cotest works with custom parameters", {
  # Test with custom sensitivity and specificity values
  result <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.92,
    test2_sens = 0.78,
    test2_spec = 0.88,
    prevalence = 0.05
  )

  expect_s3_class(result, "cotestResults")
})

test_that("cotest handles independent tests correctly", {
  # Test independent tests scenario
  result <- cotest(
    test1_sens = 0.80,
    test1_spec = 0.90,
    test2_sens = 0.75,
    test2_spec = 0.95,
    prevalence = 0.10,
    indep = TRUE
  )

  expect_s3_class(result, "cotestResults")

  # The dependence panel is now written in BOTH branches and carries no `visible:`
  # expression, so it always describes the model that was actually fitted. Under
  # independence it must SAY so rather than being blank or absent -- the previous
  # conditional-visibility scheme could leave an empty titled box on screen.
  #
  # The self-cancelling `skip()` that used to guard this assertion is gone: it skipped
  # exactly when the panel was wrongly visible, so the test could never fail.
  expect_true(nzchar(result$dependenceInfo$content))
  expect_match(result$dependenceInfo$content, "conditionally independent")
  expect_false(grepl("Realized phi", result$dependenceInfo$content))
})

test_that("cotest handles dependent tests correctly", {
  # Test dependent tests scenario
  result <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.88,
    test2_sens = 0.82,
    test2_spec = 0.92,
    prevalence = 0.05,
    indep = FALSE,
    cond_dep_pos = 0.15,
    cond_dep_neg = 0.10
  )

  expect_s3_class(result, "cotestResults")

  # For dependent tests it SHOULD be visible -- and this direction holds either way,
  # so it is asserted unconditionally.
  expect_true(isTRUE(result$dependenceInfo$visible))
})

# These messages contain parentheses, which are regex metacharacters. Passed as a pattern the
# "(is 1.2)" tail is read as a capture group and never matches the literal "(is 1.2)" in the
# message, so every one of these assertions silently failed to match -- and because testthat
# re-raises a non-matching error, only the first in each block was ever reported. fixed = TRUE
# compares literally.
test_that("cotest validates input parameters", {
  # Test invalid sensitivity (> 1)
  expect_error(
    cotest(test1_sens = 1.2),
    "test1_sens must be between 0.01 and 0.99 (is 1.2)",
    fixed = TRUE
  )

  # Test invalid sensitivity (<= 0)
  expect_error(
    cotest(test1_sens = 0),
    "test1_sens must be between 0.01 and 0.99 (is 0)",
    fixed = TRUE
  )

  # Test invalid specificity (> 1)
  expect_error(
    cotest(test1_spec = 1.1),
    "test1_spec must be between 0.01 and 0.99 (is 1.1)",
    fixed = TRUE
  )

  # Test invalid specificity (<= 0)
  expect_error(
    cotest(test2_spec = -0.1),
    "test2_spec must be between 0.01 and 0.99 (is -0.1)",
    fixed = TRUE
  )

  # Test invalid prevalence (> 1)
  expect_error(
    cotest(prevalence = 1.5),
    "prevalence must be between 0.001 and 0.999 (is 1.5)",
    fixed = TRUE
  )

  # Test invalid prevalence (<= 0)
  expect_error(
    cotest(prevalence = 0),
    "prevalence must be between 0.001 and 0.999 (is 0)",
    fixed = TRUE
  )
})

test_that("cotest validates conditional dependence parameters", {
  # The permitted range became -1 to 1 in 1.0.4: negative conditional dependence describes tests
  # that compensate for each other's errors, and was previously impossible to express.
  expect_error(
    cotest(indep = FALSE, cond_dep_pos = 1.2),
    "cond_dep_pos must be between -1 and 1 (is 1.2)",
    fixed = TRUE
  )

  # -0.1 is now a legitimate value rather than an error
  expect_no_error(cotest(indep = FALSE, cond_dep_neg = -0.1))

  expect_error(
    cotest(indep = FALSE, cond_dep_neg = -1.5),
    "cond_dep_neg must be between -1 and 1 (is -1.5)",
    fixed = TRUE
  )

  # Test valid boundary values
  expect_error(
    {
      result <- cotest(indep = FALSE, cond_dep_pos = 0, cond_dep_neg = 1)
    },
    NA
  ) # Should not error
})

test_that("cotest handles extreme parameter values", {
  # Test with very high sensitivity and specificity
  result_high <- cotest(
    test1_sens = 0.99,
    test1_spec = 0.99,
    test2_sens = 0.98,
    test2_spec = 0.98,
    prevalence = 0.001
  )

  expect_s3_class(result_high, "cotestResults")

  # Test with low sensitivity and specificity
  result_low <- cotest(
    test1_sens = 0.60,
    test1_spec = 0.70,
    test2_sens = 0.65,
    test2_spec = 0.75,
    prevalence = 0.50
  )

  expect_s3_class(result_low, "cotestResults")
})

test_that("cotest handles different prevalence scenarios", {
  # Low prevalence (screening scenario)
  result_low_prev <- cotest(
    test1_sens = 0.90,
    test1_spec = 0.95,
    test2_sens = 0.85,
    test2_spec = 0.93,
    prevalence = 0.01
  )

  expect_s3_class(result_low_prev, "cotestResults")

  # High prevalence (symptomatic patients)
  result_high_prev <- cotest(
    test1_sens = 0.80,
    test1_spec = 0.85,
    test2_sens = 0.75,
    test2_spec = 0.88,
    prevalence = 0.60
  )

  expect_s3_class(result_high_prev, "cotestResults")
})

test_that("cotest calculates likelihood ratios correctly", {
  # Test with known values for manual verification
  result <- cotest(
    test1_sens = 0.80, # PLR = 0.8/0.1 = 8, NLR = 0.2/0.9 = 0.222
    test1_spec = 0.90,
    test2_sens = 0.90, # PLR = 0.9/0.05 = 18, NLR = 0.1/0.95 = 0.105
    test2_spec = 0.95,
    prevalence = 0.10
  )

  expect_s3_class(result, "cotestResults")

  params <- result$testParamsTable$asDF
  expect_equal(params$plr[params$test == "Test 1"], 8, tolerance = 1e-6)
  expect_equal(params$nlr[params$test == "Test 1"], 0.2222222, tolerance = 1e-6)
  expect_equal(params$plr[params$test == "Test 2"], 18, tolerance = 1e-6)
  expect_equal(params$nlr[params$test == "Test 2"], 0.1052632, tolerance = 1e-6)
})

test_that("cotest handles footnotes option", {
  # Test with footnotes enabled
  result_footnotes <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fnote = TRUE
  )

  expect_s3_class(result_footnotes, "cotestResults")

  # Test with footnotes disabled
  result_no_footnotes <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fnote = FALSE
  )

  expect_s3_class(result_no_footnotes, "cotestResults")

  test_params_notes <- result_footnotes$testParamsTable$footnotes
  expect_true(any(grepl("Test 1", test_params_notes)))
  expect_true(any(grepl("Test 2", test_params_notes)))
})

test_that("cotest handles Fagan nomogram option", {
  # Test with Fagan nomogram enabled
  result_fagan <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fagan = TRUE
  )

  expect_s3_class(result_fagan, "cotestResults")
  expect_true("plot1" %in% names(result_fagan))

  # Test with Fagan nomogram disabled
  result_no_fagan <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fagan = FALSE
  )

  expect_s3_class(result_no_fagan, "cotestResults")
})

test_that("cotest calculates post-test probabilities correctly", {
  # Test a scenario where we can manually verify calculations
  # Using simple values for easier verification
  result <- cotest(
    test1_sens = 0.80,
    test1_spec = 0.90,
    test2_sens = 0.70,
    test2_spec = 0.95,
    prevalence = 0.10,
    indep = TRUE
  )

  expect_s3_class(result, "cotestResults")

  results_df <- result$cotestResultsTable$asDF
  t1_only_prob <- results_df$postProb[results_df$scenario == "Test 1 Positive Only"]
  t2_only_prob <- results_df$postProb[results_df$scenario == "Test 2 Positive Only"]
  both_pos_prob <- results_df$postProb[results_df$scenario == "Both Tests Positive"]
  both_neg_prob <- results_df$postProb[results_df$scenario == "Both Tests Negative"]

  # Reference values derived from Bayes' theorem on the joint likelihoods under
  # conditional independence, with sens1 = .80, spec1 = .90, sens2 = .70, spec2 = .95,
  # prevalence = .10:
  #     P(D+ | pattern) = p*P(pattern|D+) / (p*P(pattern|D+) + (1-p)*P(pattern|D-))
  # The previous expectations implied joint likelihood ratios of 2.105 / 3.333 / 120 /
  # 0.0585 where the correct ones are 2.526 / 3.111 / 112 / 0.0702, so they were wrong,
  # not the module.
  bayes <- function(pD, pDbar, p = 0.10) (p * pD) / (p * pD + (1 - p) * pDbar)
  se1 <- 0.80; sp1 <- 0.90; se2 <- 0.70; sp2 <- 0.95

  expect_equal(t1_only_prob,  bayes(se1 * (1 - se2),     (1 - sp1) * sp2),       tolerance = 1e-9)
  expect_equal(t2_only_prob,  bayes((1 - se1) * se2,     sp1 * (1 - sp2)),       tolerance = 1e-9)
  expect_equal(both_pos_prob, bayes(se1 * se2,           (1 - sp1) * (1 - sp2)), tolerance = 1e-9)
  expect_equal(both_neg_prob, bayes((1 - se1) * (1 - se2), sp1 * sp2),           tolerance = 1e-9)

  # and the numeric values those formulas give, spelled out. expect_equal's tolerance is
  # RELATIVE, so a small probability needs the same number of significant digits as a large
  # one -- 0.0077369 is only 5 s.f. and misses a 1e-6 relative check.
  expect_equal(t1_only_prob,  0.219178082, tolerance = 1e-6)
  expect_equal(t2_only_prob,  0.256880734, tolerance = 1e-6)
  expect_equal(both_pos_prob, 0.925619835, tolerance = 1e-6)
  expect_equal(both_neg_prob, 0.007736944, tolerance = 1e-6)
})

test_that("cotest handles conditional dependence calculations", {
  # Test with moderate dependence
  result_moderate <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.88,
    test2_sens = 0.82,
    test2_spec = 0.92,
    prevalence = 0.05,
    indep = FALSE,
    cond_dep_pos = 0.20,
    cond_dep_neg = 0.15
  )

  expect_s3_class(result_moderate, "cotestResults")
  moderate_table <- result_moderate$cotestResultsTable$asDF
  expect_true(all(is.finite(moderate_table$postProb)))
  expect_true(all(moderate_table$postProb > 0))
  expect_true(all(moderate_table$postProb < 1))

  # Test with minimal dependence (close to independence)
  result_minimal <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.88,
    test2_sens = 0.82,
    test2_spec = 0.92,
    prevalence = 0.05,
    indep = FALSE,
    cond_dep_pos = 0.01,
    cond_dep_neg = 0.01
  )

  expect_s3_class(result_minimal, "cotestResults")
  minimal_table <- result_minimal$cotestResultsTable$asDF
  expect_true(all(is.finite(minimal_table$postProb)))
  expect_true(all(minimal_table$postProb > 0))
  expect_true(all(minimal_table$postProb < 1))
})

test_that("cotest comprehensive test with all options", {
  # Test with all options enabled
  result <- cotest(
    test1_sens = 0.88,
    test1_spec = 0.92,
    test2_sens = 0.83,
    test2_spec = 0.89,
    indep = FALSE,
    cond_dep_pos = 0.12,
    cond_dep_neg = 0.08,
    prevalence = 0.07,
    fnote = TRUE,
    fagan = TRUE
  )

  expect_s3_class(result, "cotestResults")

  # Verify all expected components are present
  expect_true("testParamsTable" %in% names(result))
  expect_true("cotestResultsTable" %in% names(result))
  expect_true("dependenceInfo" %in% names(result))
  expect_true("dependenceExplanation" %in% names(result))
  expect_true("explanation" %in% names(result))
  expect_true("plot1" %in% names(result))
})

test_that("cotest handles perfect and poor test scenarios", {
  # Perfect tests scenario
  expect_error(
    {
      result_perfect <- cotest(
        test1_sens = 0.99,
        test1_spec = 0.99,
        test2_sens = 0.99,
        test2_spec = 0.99,
        prevalence = 0.10
      )
    },
    NA
  ) # Should not error

  # Poor tests scenario
  expect_error(
    {
      result_poor <- cotest(
        test1_sens = 0.55,
        test1_spec = 0.60,
        test2_sens = 0.58,
        test2_spec = 0.65,
        prevalence = 0.10
      )
    },
    NA
  ) # Should not error
})

test_that("cotest mathematical consistency checks", {
  # Test that post-test probabilities are logical
  result <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.10,
    indep = TRUE
  )

  expect_s3_class(result, "cotestResults")

  # Mathematical checks:
  # - Both positive should increase probability above prevalence
  # - Both negative should decrease probability below prevalence
  # - Individual positive tests should be between prevalence and both positive
  # These would require accessing the actual calculated values from the results
})

test_that("cotest reproducibility", {
  # Test that identical inputs produce identical outputs
  params <- list(
    test1_sens = 0.82,
    test1_spec = 0.88,
    test2_sens = 0.79,
    test2_spec = 0.91,
    prevalence = 0.12,
    indep = TRUE
  )

  result1 <- do.call(cotest, params)
  result2 <- do.call(cotest, params)

  expect_s3_class(result1, "cotestResults")
  expect_s3_class(result2, "cotestResults")

  # Results should be identical (deterministic calculation)
})

test_that("cotest boundary value testing", {
  # Test at exact boundaries of allowed ranges
  expect_error(
    {
      cotest(
        test1_sens = 0.01,
        test1_spec = 0.01,
        test2_sens = 0.99,
        test2_spec = 0.99,
        prevalence = 0.001
      )
    },
    NA
  )

  expect_error(
    {
      cotest(
        test1_sens = 0.99,
        test1_spec = 0.99,
        test2_sens = 0.01,
        test2_spec = 0.01,
        prevalence = 0.999
      )
    },
    NA
  )
})

test_that("cotest clinical scenario examples", {
  # Scenario 1: COVID-19 screening with antigen + PCR
  covid_screening <- cotest(
    test1_sens = 0.68, # Antigen test
    test1_spec = 0.99,
    test2_sens = 0.95, # PCR test
    test2_spec = 0.99,
    prevalence = 0.05, # Community prevalence
    indep = FALSE, # Tests may be dependent
    cond_dep_pos = 0.10,
    cond_dep_neg = 0.05
  )

  expect_s3_class(covid_screening, "cotestResults")

  # Scenario 2: Cancer screening with imaging + biopsy
  cancer_screening <- cotest(
    test1_sens = 0.88, # Imaging
    test1_spec = 0.92,
    test2_sens = 0.95, # Biopsy
    test2_spec = 0.98,
    prevalence = 0.02, # Cancer prevalence in screening
    indep = FALSE, # Tests likely dependent
    cond_dep_pos = 0.25,
    cond_dep_neg = 0.15,
    fagan = TRUE
  )

  expect_s3_class(cancer_screening, "cotestResults")

  # Scenario 3: Cardiac biomarkers
  cardiac_biomarkers <- cotest(
    test1_sens = 0.92, # Troponin
    test1_spec = 0.89,
    test2_sens = 0.85, # CK-MB
    test2_spec = 0.94,
    prevalence = 0.25, # Emergency department patients
    indep = FALSE, # Biomarkers likely correlated
    cond_dep_pos = 0.30,
    cond_dep_neg = 0.20,
    fnote = TRUE
  )

  expect_s3_class(cardiac_biomarkers, "cotestResults")
})


# ---------------------------------------------------------------------------
# Worked examples: one source of truth
# ---------------------------------------------------------------------------

# The preset numbers exist twice -- in R/cotest.b.R .getPresetValues(), which computes the
# results, and in jamovi/js/cotest.events.js, which writes them into the input boxes the user
# reads. They were independently maintained and drifted apart in 25 of 48 fields; three presets
# even disagreed about conditional independence, so the boxes on screen described one model
# while the results table reported another (tb_xray_sputum: inputs implying 91.5% for "both
# positive" against a printed 45.5%). This test is the only thing keeping them together.

parse_js_presets <- function() {
    js <- readLines("../../jamovi/js/cotest.events.js", warn = FALSE)
    from <- grep("^const PRESET_CONFIGS = \\{", js)
    to   <- from - 1 + grep("^\\};", js[from:length(js)])[1]
    stopifnot(length(from) == 1, !is.na(to))
    js <- js[(from + 1):(to - 1)]

    out <- list(); cur <- NULL
    for (ln in js) {
        if (grepl("^    (\\w+): \\{\\},?\\s*$", ln)) {         # one-line empty block
            cur <- NULL
            next
        }
        if (grepl("^    (\\w+): \\{\\s*$", ln)) {
            cur <- sub("^    (\\w+):.*$", "\\1", ln)
            out[[cur]] <- list()
        } else if (grepl("^    \\},?\\s*$", ln)) {
            cur <- NULL
        } else if (!is.null(cur) && grepl("^\\s+\\w+:", ln)) {
            k <- sub("^\\s*(\\w+):.*$", "\\1", ln)
            v <- trimws(gsub(",\\s*$", "", sub("^[^:]*:", "", ln)))
            out[[cur]][[k]] <- if (v == "true") TRUE
                               else if (v == "false") FALSE
                               else if (grepl("^'.*'$", v)) gsub("^'|'$", "", v)   # string values
                               else as.numeric(v)
        }
    }
    # `custom` carries the schema defaults so that returning to it clears the previous example;
    # it is not a worked example and has no counterpart in .getPresetValues().
    out[names(out) != "custom"]
}

test_that("the JS and R preset tables agree field for field", {
    skip_if_not(file.exists("../../jamovi/js/cotest.events.js"))
    js <- parse_js_presets()

    a <- ClinicoPath:::cotestClass$new(options = ClinicoPath:::cotestOptions$new(),
                                       data = data.frame(x = 1))
    getR <- a$.__enclos_env__$private$.getPresetValues

    # Derive the preset list from the SCHEMA, not from a literal in this file. An earlier
    # version hardcoded both the six names and the eight fields, so it compared JS against
    # the literal rather than against R: adding a 7th preset to .getPresetValues() alone
    # passed, while the GUI would have written nothing into the boxes and the backend would
    # have computed with the new numbers -- the exact divergence this test exists to prevent.
    a_yaml <- readLines("../../jamovi/cotest.a.yaml", warn = FALSE)
    i <- grep("^    - name: preset$", a_yaml)
    j <- i - 1 + grep("^      default:", a_yaml[i:length(a_yaml)])[1]
    presets <- setdiff(sub("^\\s*- name: ", "", grep("^        - name: ", a_yaml[i:j], value = TRUE)),
                       "custom")
    expect_gt(length(presets), 0)

    # and derive the fields from what R actually stores, so a 9th parameter is compared too
    fields <- setdiff(unique(unlist(lapply(presets, function(nm) names(getR(nm))))),
                      c("label", "note"))
    expect_true(all(c("test1_sens", "prevalence", "indep") %in% fields))

    expect_setequal(names(js), presets)

    for (nm in presets) {
        r <- getR(nm)
        expect_false(is.null(r), info = paste(nm, "is offered in the .a.yaml but unknown to R"))
        for (f in fields)
            expect_identical(js[[nm]][[f]], r[[f]],
                             info = paste0(nm, "$", f, ": events.js and .getPresetValues() disagree"))
    }
})

test_that("applyPresetConfig writes every field the backend overrides", {
    # The table can be perfectly in sync and still not reach the boxes: the parity test above
    # parses PRESET_CONFIGS only, so deleting the single line that fills the prevalence control
    # left it green while every preset silently kept the previous prevalence on screen.
    js <- readLines("../../jamovi/js/cotest.events.js", warn = FALSE)
    from <- grep("^const applyPresetConfig", js)
    to   <- from - 1 + grep("^\\};", js[from:length(js)])[1]
    expect_length(from, 1)
    expect_false(is.na(to))
    body <- paste(js[from:to], collapse = "\n")
    for (f in c("test1_sens", "test1_spec", "test2_sens", "test2_spec",
                "prevalence", "indep", "cond_dep_pos", "cond_dep_neg"))
        expect_match(body, paste0("ui\\.", f),
                     info = paste0("applyPresetConfig never writes ", f,
                                   ", so the box will not match what the backend computes"))
})

test_that("the custom preset holds the schema defaults, so switching back clears the example", {
    # Returning to "Custom values" used to leave the worked example's numbers in the unlocked
    # boxes with no disclosure: identical results, empty notices, one click.
    js <- paste(readLines("../../jamovi/js/cotest.events.js", warn = FALSE), collapse = "\n")
    blk <- regmatches(js, regexpr("custom: \\{[^}]*\\}", js, perl = TRUE))
    expect_length(blk, 1)
    expect_match(blk, "test1_sens")

    a_yaml <- readLines("../../jamovi/cotest.a.yaml", warn = FALSE)
    for (nm in c("test1_sens", "test1_spec", "test2_sens", "test2_spec", "prevalence")) {
        i <- grep(paste0("^    - name: ", nm, "$"), a_yaml)
        d <- as.numeric(sub("^      default: ", "", a_yaml[i + 3]))
        got <- as.numeric(sub(".*", "", regmatches(blk,
                    regexpr(paste0(nm, ": [0-9.]+"), blk))))
        got <- as.numeric(sub(paste0(nm, ": "), "", regmatches(blk,
                    regexpr(paste0(nm, ": [0-9.]+"), blk))))
        expect_equal(got, d, info = paste(nm, "custom block does not match the .a.yaml default"))
    }
})

test_that("UI enable expressions use jamovi's binding grammar, not ==", {
    # jamovi's _resolveBindPart terminates an operand only on ':', '&&', '||' or ')'. There is
    # no '==' operator: `preset == 'custom'` parses as an option NAME that does not exist, the
    # binding resolves to null (not FALSE), sourceNames is empty so it never re-evaluates, and
    # every control stays enabled. On a CheckBox it is worse -- the click handler is
    # `if (!getPropertyValue("enable")) preventDefault()`, so !null blocks the click and the
    # box becomes permanently greyed AND unclickable. Official jmv uses the colon form 66
    # times and '==' zero times.
    u <- readLines("../../jamovi/cotest.u.yaml", warn = FALSE)
    exprs <- grep("^\\s*(enable|visible):", u, value = TRUE)
    expect_gt(length(exprs), 0)
    expect_false(any(grepl("==", exprs)),
                 info = paste("jamovi's UI grammar has no '==' operator:",
                              paste(grep("==", exprs, value = TRUE), collapse = " | ")))
    # and every operand must reference a real option
    opts <- names(ClinicoPath:::cotestOptions$new()$.__enclos_env__$private)
    opts <- sub("^\\.\\.", "", grep("^\\.\\.", opts, value = TRUE))
    for (e in exprs) {
        body <- sub("^.*?\\((.*)\\).*$", "\\1", e)
        for (tok in unlist(strsplit(body, "\\s*(&&|\\|\\|)\\s*"))) {
            nm <- sub("^!", "", trimws(sub(":.*$", "", tok)))
            expect_true(nm %in% opts,
                        info = paste0("'", nm, "' in `", trimws(e), "` is not an option name"))
        }
    }
})

test_that("every worked example is labelled as a demonstration, in the UI and in the results", {
    # A demonstration figure quoted as if it were evidence is the failure mode this guards.
    a_yaml <- paste(readLines("../../jamovi/cotest.a.yaml", warn = FALSE), collapse = "\n")
    expect_match(a_yaml, "must not be used for\\s+patient care")
    # every non-custom option title carries the marker
    titles <- regmatches(a_yaml, gregexpr("- name: (hpv_pap|psa_dre|troponin_ecg|mammogram_ultrasound|covid_antigen_pcr|tb_xray_sputum)\\n          title: [^\\n]*", a_yaml, perl = TRUE))[[1]]
    expect_length(titles, 6)
    expect_true(all(grepl("demo only", titles, fixed = TRUE)))

    for (nm in c("hpv_pap", "psa_dre", "troponin_ecg",
                 "mammogram_ultrasound", "covid_antigen_pcr", "tb_xray_sputum")) {
        res <- ClinicoPath::cotest(preset = nm)
        txt <- gsub("<[^>]+>", " ", paste(res$notices$content, collapse = " "))
        expect_match(txt, "Worked example in use",
                     info = paste(nm, "did not disclose that a worked example was in use"))
        expect_match(txt, "not values to use for patient care", info = nm)
    }
})

test_that("a worked example computes what the input boxes show", {
    # events.js writes the preset into the controls; the backend recomputes from its own table.
    # Passing the JS values explicitly with preset = "custom" must reproduce the preset run.
    js <- parse_js_presets()
    for (nm in names(js)) {
        v <- js[[nm]]
        manual <- do.call(ClinicoPath::cotest,
                          c(list(preset = "custom"), v[intersect(names(v),
                            c("test1_sens", "test1_spec", "test2_sens", "test2_spec",
                              "prevalence", "indep", "cond_dep_pos", "cond_dep_neg"))]))
        viapreset <- ClinicoPath::cotest(preset = nm)
        expect_equal(as.data.frame(manual$cotestResultsTable)$postProb,
                     as.data.frame(viapreset$cotestResultsTable)$postProb,
                     tolerance = 1e-12,
                     info = paste(nm, "displayed inputs do not reproduce the reported results"))
    }
})

test_that("every control a worked example overrides is disabled while it is selected", {
    # Locking only sensitivity/specificity left prevalence and the independence checkbox
    # editable but discarded: cotest(preset = "hpv_pap", prevalence = 0.40) and
    # prevalence = 0.001 returned byte-identical tables.
    # Colon form, not `==`: jamovi's binding grammar has no equality operator, so
    # `(preset == 'custom')` resolves to null, disables nothing, and on the indep CheckBox
    # makes the control permanently greyed and unclickable. See the grammar test below.
    u <- readLines("../../jamovi/cotest.u.yaml", warn = FALSE)
    for (nm in c("test1_sens", "test1_spec", "test2_sens", "test2_spec",
                 "prevalence", "indep", "cond_dep_pos", "cond_dep_neg")) {
        i <- grep(paste0("name: ", nm, "$"), u)
        expect_length(i, 1)
        blk <- paste(u[i:min(i + 6, length(u))], collapse = " ")
        expect_match(blk, "enable: \\(preset:custom",
                     info = paste(nm, "is overridden by a worked example but stays editable"))
    }
})

test_that("footnotes do not accumulate over repeated run cycles", {
    # addFootnote() appends with no dedup and neither table declares clearWith, so this is
    # only safe because setRow() -- which clears each cell's footnotes -- runs BEFORE
    # .addFootnotes() on every path. Lock that ordering.
    opts <- ClinicoPath:::cotestOptions$new(fnote = TRUE)
    a <- ClinicoPath:::cotestClass$new(options = opts, data = data.frame(x = 1))
    p <- a$.__enclos_env__$private
    p$.init()
    counts <- vapply(1:4, function(i) {
        p$.run()
        length(a$results$testParamsTable$getCell(rowKey = "test1", col = "sens")$footnotes)
    }, numeric(1))
    expect_equal(counts, rep(1, 4))

    # the headline parallel-rule row must be footnoted too -- it was the one row the loop skipped
    expect_gt(length(a$results$cotestResultsTable$getCell(rowKey = "either_pos",
                                                          col = "postProb")$footnotes), 0)
})

test_that("the Fagan nomogram is titled with the rule it actually plots", {
    # It is built from the parallel-rule likelihood ratios, so its positive arm is the
    # "Either Test Positive" row (42.5% at the defaults), not "Both Tests Positive" (89.1%).
    r_yaml <- paste(readLines("../../jamovi/cotest.r.yaml", warn = FALSE), collapse = "\n")
    expect_match(r_yaml, "title: 'Fagan nomogram - parallel rule")

    res <- ClinicoPath::cotest(fagan = TRUE)
    ct <- as.data.frame(res$cotestResultsTable)
    either <- ct$postProb[ct$scenario == "Either Test Positive (Parallel Rule)"]
    plr <- res$plot1$state$Plr_PositiveRule
    odds <- 0.10 / 0.90 * plr
    expect_equal(unname(odds / (1 + odds)), unname(either), tolerance = 1e-9)
})

test_that("every Collate entry matches a tracked filename exactly (case included)", {
    # cotest sources R/nomogrammer.R for the Fagan plot. git tracked it as `nomogrammer.r`
    # while DESCRIPTION's Collate said `nomogrammer.R`; macOS is case-insensitive so this was
    # invisible locally, but on a case-sensitive filesystem R CMD build fails outright with
    # "files in 'Collate' field missing from 'R'".
    skip_if_not(nzchar(Sys.which("git")))
    root <- normalizePath("../..")
    tracked <- system2("git", c("-C", shQuote(root), "ls-files", "R/"), stdout = TRUE)
    tracked <- sub("^R/", "", tracked[startsWith(tracked, "R/")])
    desc <- paste(readLines(file.path(root, "DESCRIPTION")), collapse = "\n")
    tail_txt <- substring(desc, regexpr("Collate:", desc))   # match and extract on the SAME string
    collate <- gsub("'", "", regmatches(tail_txt, gregexpr("'[^']+'", tail_txt))[[1]])
    skip_if(length(collate) == 0)
    expect_true("nomogrammer.R" %in% tracked)
    expect_true("nomogrammer.R" %in% collate)
    expect_equal(setdiff(collate, tracked), character(0))
})
