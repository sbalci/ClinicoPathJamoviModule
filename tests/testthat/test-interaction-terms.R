# Regression tests for interaction-term normalisation.
#
# `interactions = ~sex:age` is the documented R interface, but the value could
# reach the backend with the formula's tilde still glued to the first variable
# -- list(c("~sex", "age")) -- which built the term `~sex`:age, matched no
# column, and left the interaction and covariate-contribution tables silently
# empty. .mapInteractionTerms() now normalises whatever form it receives.

.map_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists(".mapInteractionTerms", envir = .cand, inherits = FALSE)) {
            .map_ns <- .cand
            break
        }
    }
}
if (!is.null(.map_ns))
    .mapInteractionTerms <- get(".mapInteractionTerms", envir = .map_ns)

labs <- list(sex = "sex", age = "age")

test_that("a tilde-prefixed first component is normalised away", {
    expect_equal(.mapInteractionTerms(list(c("~sex", "age")), labs),
                 list(c("sex", "age")))
})

test_that("an already-clean Terms list is unchanged", {
    expect_equal(.mapInteractionTerms(list(c("sex", "age")), labs),
                 list(c("sex", "age")))
})

test_that("a raw formula is accepted", {
    expect_equal(.mapInteractionTerms(~sex:age, labs), list(c("sex", "age")))
})

test_that("empty and NULL inputs stay empty", {
    expect_equal(.mapInteractionTerms(NULL, labs), list())
    expect_equal(.mapInteractionTerms(list(), labs), list())
})

test_that("three-way terms and whitespace are handled", {
    expect_equal(.mapInteractionTerms(list(c("~ sex", " age ", "sex")), labs),
                 list(c("sex", "age", "sex")))
})
