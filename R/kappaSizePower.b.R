#' @title Power Approach for the Number of Subjects Required
#' @importFrom R6 R6Class
#' @import jmvcore
#' @return An \code{R6} class generator object for the \code{kappaSizePowerClass} backend; used internally by the jamovi analysis wrapper and not called directly.


kappaSizePowerClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizePowerClass",
    inherit = kappaSizePowerBase,
    private = list(

        # Resolved 2026-07-15 (autofix): input validation + jmvcore::reject error handling;
        #   4 duplicate outcome branches collapsed into one switch-based path;
        #   paste0(sep=) misuse and binary-branch proportion drop fixed.
        # Resolved 2026-08-23 (review): Notes panel, goodness-of-fit sparse-cell rule,
        #   degenerate-design warnings, i18n wrapping of every user-facing string.

        # Split the proportions string into numbers. Same format as the siblings
        # (R/kappaSizeFixedN.b.R:.parsePropTokens): commas, semicolons, pipes or whitespace
        # between values, decimal point only. Text pasted from Word or Excel carries U+00A0
        # (non-breaking space) in place of a space, which [:space:] does NOT match, so it is
        # normalised first; otherwise "0.30<nbsp>0.70" fails as one unreadable token.
        .parseProps = function(raw) {
            raw  <- gsub("\u{00A0}", " ", raw, fixed = TRUE)
            toks <- unlist(strsplit(raw, "[,;|[:space:]]+"), use.names = FALSE)
            toks <- toks[nzchar(trimws(toks))]
            list(raw = raw, values = suppressWarnings(as.numeric(trimws(toks))))
        },

        # Expected probability of every goodness-of-fit cell under kappa0. kappaSize sizes the
        # study with a chi-square over AGREEMENT PATTERNS, not over outcome categories: for a
        # binary outcome and n raters the cells are "exactly j raters call the finding present"
        # (j = 0..n); for 3-5 categories they are "all n raters choose category j" plus one
        # "any disagreement" cell. These are the P0..Pn closures inside
        # body(kappaSize::PowerBinary) / Power3Cats / Power4Cats / Power5Cats. The cells in the
        # middle carry a (1 - kappa0) factor and p^j (1 - p)^(n - j), so they empty out long
        # before any category marginal does -- with 6 raters and a 5% finding, N = 316 leaves
        # three cells below 0.5 expected while the marginal check (0.05 * 316 = 16) sees nothing
        # wrong. That multi-rater, rare-finding design is the typical pathology agreement study.
        # The multi-category "all agree on j" cell is the Dirichlet-multinomial product
        # prod_{i=0}^{n-1} (p_j (1 - kappa0) + i kappa0) / ((1 - kappa0) + i kappa0), which
        # reproduces the engine's polynomials for K = 3..5 and n = 2..6 to 1e-15 and reduces
        # to p_j^2 + kappa0 p_j (1 - p_j) for two raters.
        .gofCells = function(outcome, raters, props, kappa0) {
            if (outcome == 2L) {
                p <- props[1]
                j <- 0:raters
                choose(raters, j) * p^j * (1 - p)^(raters - j) * (1 - kappa0) +
                    kappa0 * ifelse(j == raters, p, ifelse(j == 0, 1 - p, 0))
            } else {
                i <- seq_len(raters) - 1
                agree <- vapply(props, function(pj)
                    prod((pj * (1 - kappa0) + i * kappa0) / ((1 - kappa0) + i * kappa0)),
                    numeric(1))
                c(1 - sum(agree), agree)
            }
        },

        # Preformatted panes do not wrap: a 300-character sentence runs off the right edge of
        # the results panel (the siblings hard-break theirs). Wrap at render time so the
        # translated text wraps too.
        .wrap = function(x, width = 78) paste(strwrap(x, width = width), collapse = "\n"),

        # Build the study-explanation paragraph (plain text; generic across cardinalities).
        .buildExplanation = function(kappa0, kappa1, alpha, power, raters, props) {
            prev <- if (length(props) == 1) {
                jmvcore::format(
                    .("Further suppose that the prevalence of the trait is {p}."),
                    p = props[1])
            } else if (length(props) == 2) {
                # The user never said which category is the "trait"; with "0.80, 0.20" the
                # old sentence claimed the lesion was present in 80% of cases.
                jmvcore::format(
                    .("Further suppose that the two categories occur in {p1} and {p2} of subjects (the required sample size is the same whichever category is called positive)."),
                    p1 = props[1], p2 = props[2])
            } else {
                jmvcore::format(
                    .("Further suppose that the prevalences of the categories are {head} and {last}."),
                    head = paste0(props[-length(props)], collapse = ", "),
                    last = props[length(props)])
            }

            paste0(
                private$.wrap(paste(
                    .("This is a POWER calculation: it returns the number of subjects needed to REJECT kappa0 in favour of kappa1 in a two-sided test at the stated significance level and power."),
                    .("It answers a different question from the confidence-interval approach (kappaSizeCI), which sizes a study to achieve a target interval width, so the two will not agree on a sample size for the same study - pick the one that matches how the result will be reported."))),
                "\n\n",
                private$.wrap(jmvcore::format(
                    .("Researchers would like to determine the required sample size to test the null hypothesis kappa = {kappa0} against the alternative kappa = {kappa1} at a two-sided significance level of {alpha} with power {power}, in a study of interobserver agreement with {raters} raters."),
                    kappa0 = kappa0, kappa1 = kappa1, alpha = alpha, power = power,
                    raters = raters)),
                "\n",
                private$.wrap(prev))
        },

        # Notes panel, same shape as kappaSizeCI / kappaSizeFixedN: warnings first, then the
        # method statement. Rendered via a dedicated Html output rather than jmvcore::Notice
        # objects (those cannot be serialised when inserted dynamically).
        # "1 subjects" reads as a bug in a protocol sentence.
        .subjects = function(n) if (isTRUE(n == 1)) .("subject") else .("subjects"),

        .buildNotices = function(n_required, sparse, outcome, raters, kappa0, kappa1, power) {
            warn_div <- "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #ec971f; background-color: rgba(227, 144, 33, 0.07); color: inherit;'>"
            info_div <- "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #3c8dbc; background-color: rgba(72, 138, 188, 0.06); color: inherit;'>"
            block <- function(div, title, ...)
                paste0(div, "<b>", title, "</b> ", paste(c(...), collapse = " "), "</div>")

            warn <- ""
            has_n <- length(n_required) == 1 && is.finite(n_required)

            # kappa1 below kappa0 is accepted by the engine and returns a number, but it asks a
            # different question: how many subjects to show agreement is WORSE than the null.
            # That is occasionally intended and more often a transposition, and the returned n
            # differs from the one for the mirrored alternative, so say which way round it read.
            if (isTRUE(kappa1 < kappa0)) {
                warn <- paste0(warn, block(warn_div,
                    .("Alternative below the null."),
                    jmvcore::format(
                        .("The alternative kappa ({kappa1}) is BELOW the null ({kappa0}). This sizes a study to demonstrate that agreement is WORSE than the null value, not better. If you meant to detect an improvement, swap the two values - the required sample size is not the same either way."),
                        kappa0 = kappa0, kappa1 = kappa1)))
            }

            # A gap of 0.01 between the two kappas asks for ~100,000 subjects. That is the
            # signature of the other common transposition: the EXPECTED kappa typed as kappa1
            # and a nearby value as kappa0, where kappa0 should be the agreement to rule out.
            # Worded as "difference", not "improvement", because the swap note above may have
            # just told the user that kappa1 below kappa0 is a legitimate design.
            if (abs(kappa1 - kappa0) < 0.05) {
                warn <- paste0(warn, block(warn_div,
                    .("Small kappa difference."),
                    jmvcore::format(
                        .("The null and alternative kappa differ by only {delta}. Check that kappa0 is the agreement you want to rule OUT (not the agreement you expect to observe) and that kappa1 is a clinically meaningful difference from it; very close values need very large studies."),
                        delta = signif(abs(kappa1 - kappa0), 3))))
            }

            # A large n has three drivers -- a rare finding, a small kappa gap, high power --
            # and the kappa gap is often NOT the one (prevalence 1% with the default kappas
            # needs 3,429), so this block names all three rather than blaming one.
            if (has_n && n_required > 2000) {
                warn <- paste0(warn, block(warn_div,
                    .("Very large sample size."),
                    jmvcore::format(
                        .("The design needs {n} subjects; few agreement studies can enrol this many. A rare finding (prevalence far from 0.5), a small difference between kappa0 and kappa1, and a high power target all inflate the sample size - revisit whichever of these is not a firm requirement."),
                        n = n_required)))
            }

            # The option ranges still admit designs the method cannot size: alpha just below
            # power gives "A minimum of 2 subjects", kappa 0.01 vs 0.99 gives 1. The engine
            # converges on these instantly, so they have to be caught by their result.
            if (has_n && n_required < 10) {
                warn <- paste0(warn, block(warn_div,
                    .("Very small sample size."),
                    jmvcore::format(
                        .("The required sample size is only {n} {subjects}. The large-sample approximation behind this method does not hold for such small studies; treat the figure as a lower bound, not a plan."),
                        n = n_required, subjects = private$.subjects(n_required))))
            }

            if (is.finite(power) && power < 0.5) {
                warn <- paste0(warn, block(warn_div,
                    .("Low power."),
                    jmvcore::format(
                        .("The requested power is {power}. A study powered below 50% is more likely to miss the alternative kappa than to detect it; conventional values are 0.80 or 0.90."),
                        power = power)))
            }

            # Sparse goodness-of-fit cells (see .gofCells). kappaSize prints its own version of
            # this once per sparse CATEGORY inside the raw result text; it was reaching the user
            # only as repeated lines there, and it watches the wrong quantity. Skipped when n is
            # below 10: every cell is sparse then, the "Very small sample size" block above
            # already says so, and "enrich the case series" would point at the wrong cause.
            if (isTRUE(sparse) && !(has_n && n_required < 10)) {
                remedy <- if (outcome == 2L) {
                    .("Consider enriching the case series so the rare finding is more common (the calculation assumes the stated prevalence), or planning a larger study.")
                } else {
                    .("Consider collapsing rare categories or planning a larger study.")
                }
                warn <- paste0(warn, block(warn_div,
                    .("Sparse categories."),
                    .("At the required sample size at least one agreement-pattern cell (for example, exactly k of the raters calling the finding present, or all raters agreeing on one category) has an expected count below five. The calculation rests on a large-sample chi-square approximation, so the sample size shown is less dependable here."),
                    remedy))
            }

            info <- block(info_div,
                .("Methodology."),
                .("Power-based sample size for a two-sided test of H0: kappa = kappa0 against H1: kappa = kappa1, for the unweighted (nominal-category) kappa coefficient, using the goodness-of-fit (non-central chi-square) approach implemented in the kappaSize package (Rotondi and Donner)."),
                .("It does not apply to weighted kappa for ordered grades; for an ordinal grading study either treat the grades as nominal categories or use a dedicated weighted-kappa method."),
                .("It answers 'how many subjects do I need to reject kappa0 in favour of kappa1?' - not how precisely kappa will be estimated; for a target interval width use kappaSizeCI instead."),
                .("Note that kappa0 here is the null hypothesis value, whereas in kappaSizeCI and kappaSizeFixedN it is the agreement you anticipate observing."),
                if (has_n)
                    jmvcore::format(
                        .("Required sample size: <b>{n}</b> {subjects}, each rated by all {raters} raters."),
                        n = n_required, subjects = private$.subjects(n_required), raters = raters)
                else "")

            paste0(warn, info)
        },

        .run = function() {

            outcome <- as.integer(self$options$outcome)
            kappa0  <- self$options$kappa0
            kappa1  <- self$options$kappa1
            raters  <- as.integer(self$options$raters)
            alpha   <- self$options$alpha
            power   <- self$options$power

            # Clear any prior content so stale results never survive a rejected run.
            self$results$notices$setContent("")
            self$results$text1$setContent("")
            self$results$text_summary$setContent("")
            self$results$text2$setContent("")

            # --- Parse and validate the proportions ---------------------------
            parsed <- private$.parseProps(self$options$props)
            props  <- parsed$values

            if (length(props) == 0 || anyNA(props))
                jmvcore::reject(
                    .("Proportions must be numbers separated by commas, semicolons or spaces (for example '0.20, 0.80'). One or more entries could not be read as a number. Note that a decimal comma is not recognised - use a point, as in 0.20."),
                    code = "invalid_props")

            if (any(props <= 0) || any(props >= 1)) {
                # "0,30 0,70" splits into 0, 30, 0, 70 and then fails the range check, which
                # tells a user with a European keyboard that their proportions are out of range
                # rather than that the decimal separator is wrong. Detect that case by re-reading
                # the string with the comma as a decimal point and seeing whether it becomes
                # valid; only then is it really a decimal-separator problem.
                as_decimal <- suppressWarnings(as.numeric(trimws(unlist(
                    strsplit(gsub("([0-9]),([0-9])", "\\1.\\2", parsed$raw),
                             "[;|[:space:]]+"), use.names = FALSE))))
                as_decimal <- as_decimal[!is.na(as_decimal)]
                if (length(as_decimal) > 0 && all(as_decimal > 0 & as_decimal < 1))
                    jmvcore::reject(
                        .("Proportions must use a decimal point, not a decimal comma: write 0.30, 0.70 rather than 0,30 0,70."),
                        code = "props_decimal_comma")

                jmvcore::reject(
                    .("Each proportion must be strictly between 0 and 1."),
                    code = "props_range")
            }

            # kappaSize::PowerBinary accepts a single prevalence or two proportions
            # summing to 1; the 3/4/5-category engines require exactly N proportions.
            if (outcome == 2L) {
                if (!(length(props) %in% c(1L, 2L)))
                    jmvcore::reject(
                        .("For a binary outcome enter either one prevalence value or two proportions that sum to 1."),
                        code = "props_count_mismatch")
            } else if (length(props) != outcome) {
                jmvcore::reject(
                    jmvcore::format(
                        .("Enter exactly {k} proportions for {k} outcome levels (received {got})."),
                        k = outcome, got = length(props)),
                    code = "props_count_mismatch")
            }

            if (length(props) >= 2 && abs(sum(props) - 1) >= 0.001)
                jmvcore::reject(
                    jmvcore::format(
                        .("Proportions must sum to 1 (current sum = {sum})."),
                        sum = round(sum(props), 4)),
                    code = "props_sum")

            # --- Validate alpha against power ---------------------------------
            # kappaSize's root finder (kappaSize:::.hichi) never converges when alpha is ABOVE
            # the target power: its first loop keeps quartering the lower bracket towards zero
            # and pchisq(q, df, 0) = 1 - alpha never reaches 1 - power. Verified with
            # alpha = 0.90, power = 0.20, still running after 60 seconds and not interruptible.
            # In jamovi that freezes the analysis with no way to recover. alpha EQUAL to power
            # returns instantly but with N ~ 1e-13, which is just as meaningless, so both are
            # refused before the engine is entered. (Non-finite values never reach here: the
            # generated option classes reject them at construction.)
            if (alpha >= power)
                jmvcore::reject(
                    jmvcore::format(
                        .("The significance level ({alpha}) must be below the power ({power}). A study whose power does not exceed its type I error rate provides no evidence, and the sample size cannot be computed for it. Conventional values are alpha 0.05 and power 0.80."),
                        alpha = alpha, power = power),
                    code = "alpha_ge_power")

            # --- Validate the kappa relationship ------------------------------
            if (isTRUE(kappa0 == kappa1))
                jmvcore::reject(
                    .("kappa0 (null) and kappa1 (alternative) must differ; equal values make the required sample size undefined."),
                    code = "kappa_equal")

            # --- Select the engine for the chosen cardinality -----------------
            # The option list restricts outcome to "2".."5", so no default branch is needed.
            powerFun <- switch(as.character(outcome),
                "2" = kappaSize::PowerBinary,
                "3" = kappaSize::Power3Cats,
                "4" = kappaSize::Power4Cats,
                "5" = kappaSize::Power5Cats)

            # --- Compute (wrap only the engine call; reject cleanly on error) -
            result <- tryCatch(
                powerFun(
                    kappa0 = kappa0,
                    kappa1 = kappa1,
                    props  = props,
                    raters = raters,
                    alpha  = alpha,
                    power  = power),
                error = function(e)
                    jmvcore::reject(
                        jmvcore::format(
                            .("Sample size calculation failed: {error}"),
                            error = conditionMessage(e)),
                        code = "kappasize_error"))

            # --- Populate outputs ---------------------------------------------
            # print() gives the headline sentence ("A minimum of N subjects ..."); summary()
            # repeats it and adds the study parameters. The engine prints its cell-count warning
            # once per sparse category (five times for five levels); keep the first and lift the
            # caveat into the Notes panel, where it is computed on the right cells.
            dedupe <- function(lines)
                lines[!(duplicated(lines) & grepl("expected cell count", lines, fixed = TRUE))]
            result_text <- paste(dedupe(utils::capture.output(print(result))), collapse = "\n")
            self$results$text1$setContent(result_text)

            summary_text <- paste(dedupe(utils::capture.output(summary(result))),
                                  collapse = "\n")
            self$results$text_summary$setContent(summary_text)

            # Put the computed answer in the explanation too, so the sentence a user copies into
            # a protocol carries the number it is explaining.
            n_required <- suppressWarnings(ceiling(as.numeric(result$N)))
            self$results$text2$setContent(paste0(
                private$.buildExplanation(kappa0, kappa1, alpha, power, raters, props),
                if (is.finite(n_required))
                    paste0("\n", jmvcore::format(
                        .("The required sample size is {n} {subjects}."),
                        n = n_required, subjects = private$.subjects(n_required)))
                else ""))

            cells  <- private$.gofCells(outcome, raters, props, kappa0)
            sparse <- is.finite(n_required) && any(cells * as.numeric(result$N) < 5)
            self$results$notices$setContent(
                private$.buildNotices(n_required, sparse = sparse, outcome = outcome,
                                      raters = raters, kappa0 = kappa0, kappa1 = kappa1,
                                      power = power))
        })
)
