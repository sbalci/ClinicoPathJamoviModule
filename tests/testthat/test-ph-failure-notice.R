# Regression test for PH-SILENT-SWALLOW (survivalcont).
#
# The proportional-hazards tryCatch handler used to emit its "test could not be
# performed" notice ONLY when the error message did NOT match "singular" or
# "convergence" -- i.e. it stayed silent in exactly the two cases most worth
# reporting. A user whose Cox model failed to converge therefore saw no PH
# warning anywhere in the output and had every reason to conclude the assumption
# held, when in truth the test had never run. A failed test must never be
# indistinguishable from a passed one.
#
# The handler sits behind an earlier `return()` guard that degenerate data trips
# first, so the failure path cannot be reached through the public wrapper. These
# tests therefore assert on the handler's source and on the notice text.

.sc_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists("survivalcontClass", envir = .cand, inherits = FALSE)) {
            .sc_ns <- .cand
            break
        }
    }
}
skip_if(is.null(.sc_ns), "survivalcont not available in this distribution")

.sc_src <- function() {
    gen <- get("survivalcontClass", envir = .sc_ns)
    src <- paste(vapply(gen$private_methods,
                        function(f) paste(deparse(f), collapse = " "),
                        character(1)), collapse = " ")
    gsub("[[:space:]]+", " ", src)
}

test_that("a non-convergent PH test is reported, not swallowed", {
    src <- .sc_src()

    # The exact defect: the notice gated on the message NOT matching these.
    expect_false(grepl('if (!grepl("singular|convergence", e$message', src, fixed = TRUE))

    # The handler must still recognise those messages -- to TAILOR the wording,
    # not to suppress the notice.
    expect_true(grepl("singular|convergence", src))
    expect_true(grepl("Proportional Hazards Test Could Not Be Performed", src, fixed = TRUE))
})

test_that("the notice states the assumption was untested, not that it holds", {
    src <- .sc_src()

    # The wording must not let absence of a warning read as a pass.
    expect_true(grepl("could NOT be tested", src, fixed = TRUE) ||
                grepl("did not converge", src, fixed = TRUE))
    expect_true(grepl("not evidence that proportional hazards holds", src, fixed = TRUE))

    # It is raised at warning level, not buried as info.
    expect_true(grepl('type = "warning"', src, fixed = TRUE))
})

test_that("cox.zph genuinely errors on a singular fit, which is what the handler catches", {
    # Documents the failure mode the handler exists for: the message contains
    # "singular", which the old guard matched and therefore suppressed.
    set.seed(1); n <- 40
    d <- data.frame(t = runif(n, 1, 50), ev = c(1, rep(0, n - 1)), x = rnorm(n))
    err <- tryCatch({
        survival::cox.zph(survival::coxph(survival::Surv(t, ev) ~ x, d))
        NA_character_
    }, error = function(e) conditionMessage(e))

    skip_if(is.na(err), "this survival version does not error on the degenerate fit")
    expect_match(err, "singular|convergence|subscript")
})
