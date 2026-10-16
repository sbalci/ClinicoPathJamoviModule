# ============================================================================
# kappaSize SHARED UTILITIES
# ============================================================================
# Helpers shared by the kappaSize trio: kappaSizeCI, kappaSizeFixedN and
# kappaSizePower. Each of the three carried its own byte-for-byte copy of
# .gofCells(); three copies of a closed form is three chances for one of them to
# drift, so the single implementation lives here and the three backends call it.

# Expected probability of every goodness-of-fit cell at agreement level `rho`.
#
# kappaSize does NOT run its chi-square over the outcome categories -- it runs it
# over AGREEMENT PATTERNS. For a binary outcome and n raters the cells are
# "exactly j raters call the finding present" (j = 0..n); for 3-5 categories they
# are "all n raters choose category j", plus one pooled "any disagreement" cell.
# These are the P0..Pn closures inside body(kappaSize::PowerBinary) /
# Power3Cats / Power4Cats / Power5Cats and their CI*/FixedN* counterparts.
#
# Where each engine evaluates this matters, and it differs per analysis:
#   * Power*  : cells at kappa0, the hypothesised agreement.
#   * FixedN* : the lower bound is found by walking rho down from kappa0 in steps
#               of 0.001 until the chi-square sum crosses qchisq(1 - 2a, 1); the
#               expected counts in the denominator are the cells at rho = kappaL.
#   * CI*     : n is grown until the sum exceeds the critical value at rho =
#               kappaL and (two-sided) rho = kappaU.
# In every case the expected counts that can go sparse are these agreement-pattern
# cells, NOT the outcome marginals that kappaSize's own print/summary checks. The
# middle cells carry a (1 - rho) factor and p^j (1 - p)^(n - j), so they empty out
# long before any marginal does: 6 raters, a 5% finding and N = 316 leaves three
# cells below 0.5 expected while the marginal check (0.05 * 316 = 16) sees nothing
# wrong -- exactly the multi-rater, rare-finding design of a typical pathology
# agreement study. A rho at which any cell is negative is outside the
# common-correlation model altogether.
#
# The multi-category "all agree on j" cell is the Dirichlet-multinomial product
# prod_{i=0}^{n-1} (p_j (1 - rho) + i rho) / ((1 - rho) + i rho), which reduces to
# p_j^2 + rho p_j (1 - p_j) for two raters.
#
# Verified against every kappaSize .CalcIT for raters 2-6: the binomial form to
# 1e-11 across the FixedN* engines, the K = 3..5 polynomials to 1e-15.
#
# @param outcome  number of outcome categories (2 = binary, else 3-5)
# @param raters   number of raters
# @param props    category proportions; props[1] is P(present) when outcome == 2
# @param rho      agreement level at which the cells are evaluated
# @return numeric vector of cell probabilities
kappaSizeGofCells <- function(outcome, raters, props, rho) {
    if (outcome == 2) {
        p <- props[1]
        j <- 0:raters
        choose(raters, j) * p^j * (1 - p)^(raters - j) * (1 - rho) +
            rho * ifelse(j == raters, p, ifelse(j == 0, 1 - p, 0))
    } else {
        i <- seq_len(raters) - 1
        agree <- vapply(props, function(pj)
            prod((pj * (1 - rho) + i * rho) / ((1 - rho) + i * rho)), numeric(1))
        c(1 - sum(agree), agree)
    }
}

# TRUE when `raw` (the proportions text, already space-normalised by the caller) is valid
# proportions once every comma between two digits is read as a decimal point -- i.e. the
# user's real mistake is the decimal separator. EVERY token must parse: the three backends
# used to drop the tokens that did not, so the typo "1.2, 0.8" ("1.2," unreadable, 0.8 valid)
# was told to use the decimal point it already used. A string with no digit-comma-digit is
# never a decimal-comma problem.
kappaSizePropsLookDecimalComma <- function(raw) {
    if (!grepl("[0-9],[0-9]", raw)) return(FALSE)
    tok <- strsplit(gsub("([0-9]),([0-9])", "\\1.\\2", raw), "[,;|[:space:]]+")[[1]]
    v <- suppressWarnings(as.numeric(tok[nzchar(tok)]))
    length(v) > 0 && all(is.finite(v)) && all(v > 0 & v < 1)
}

# Syntax pane for the free-text `props` option. jmvcore's sourcify() wraps a String in quotes
# without escaping it, so a value carried in a shared .omv such as
# `0.2", alpha = {<code>; 0.05}, citype = "two_sided` exported a call that parsed and ran
# <code> when pasted into R. Returns the escaped argument for a non-default scalar value, or
# NULL to mean "let jmvcore's .sourcifyOption() handle it" (defaults stay omitted). Each
# backend's .sourcifyOption() calls this first.
kappaSizeSourcifyOption <- function(option) {
    value <- option$value
    if (!identical(option$name, "props") || !is.character(value) || length(value) != 1L ||
        is.na(value) || identical(value, option$default))
        return(NULL)
    paste0("props = ", encodeString(value, quote = '"'))
}
