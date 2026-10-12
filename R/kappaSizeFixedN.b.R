#' @title Lowest Expected Value for a fixed sample size
#' @importFrom R6 R6Class
#' @import jmvcore
#' @return An \code{R6} class generator object for the \code{kappaSizeFixedNClass} backend; used internally by the jamovi analysis wrapper and not called directly.


kappaSizeFixedNClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizeFixedNClass",
    inherit = kappaSizeFixedNBase,
    private = list(

        # Resolved 2026-08-23 (review): parameter-space check on the returned bound,
        #   Cochran sparse-cell rule on the agreement-pattern cells at kappaL, wrapped
        #   explanation, i18n wrapping of every user-facing string.

        # Split the proportions string into numbers. Same format as the siblings
        # (R/kappaSizePower.b.R:.parseProps): commas, semicolons, pipes or whitespace between
        # values, decimal point only. Text pasted from Word or Excel carries U+00A0, and macOS /
        # French locales emit U+202F (narrow no-break space); neither is in [:space:], so both
        # are normalised first. The normalised string is returned too so that the decimal-comma
        # diagnosis below re-reads the same text.
        .parseProps = function(raw) {
            raw  <- gsub("[\u{00A0}\u{202F}]", " ", raw)
            toks <- unlist(strsplit(raw, "[,;|[:space:]]+"), use.names = FALSE)
            toks <- toks[nzchar(trimws(toks))]
            list(raw = raw, tokens = trimws(toks),
                 values = suppressWarnings(as.numeric(trimws(toks))))
        },

        # The goodness-of-fit cell probabilities now live in R/utils-kappasize.R
        # (kappaSizeGofCells) -- the identical closed form used to be copied into all
        # three kappaSize backends. See that file for the derivation and for which
        # agreement level each engine evaluates the cells at.

        # Preformatted panes do not wrap; wrap at render time so translated text wraps too.
        .wrap = function(x, width = 78) paste(strwrap(x, width = width), collapse = "\n"),

        # One rendering of the bound for every pane. kappaSize prints kappaL with cat(), i.e.
        # at getOption("digits") = 7 significant digits, so anything narrower disagrees with
        # the Analysis result pane on the same screen: signif(, 4) turned the engine's
        # "0.45334" (kappa0 = 0.61234) into "0.4533" in the explanation. Seven digits also
        # absorbs the drift the 0.001 walk accumulates (0.0819999999999997 -> "0.082").
        .fmtBound = function(x)
            base::format(x, digits = 7, scientific = FALSE, trim = TRUE),

        # Expected agreement-pattern counts run from ~1e-06 to a few dozen, and signif() pasted
        # straight into a sentence renders the small end as "2.4e-06" in prose aimed at
        # pathologists -- 64 of 120 swept designs did exactly that here, because this file was
        # the one kappaSize backend with no formatter at all. scientific = FALSE stops the
        # notation; the floor only stops the tail growing unreadably long. Identical to
        # R/kappaSizePower.b.R:.fmtCount and R/kappaSizeCI.b.R:.fmtCount, same floor and same
        # wording, so the three analyses word the same caveat the same way.
        .fmtCount = function(x) {
            if (!isTRUE(is.finite(x))) return(.("unavailable"))
            if (x < 0.0001) return(.("below 0.0001"))
            base::format(signif(x, 2), scientific = FALSE, trim = TRUE)
        },

        # Notes panel, same shape as the two siblings: warnings first, then the method.
        # `sparse_*` carry the Cochran-rule verdict plus the numbers behind it so the notice
        # can say how sparse, not just that it is.
        .buildNotices = function(kappaL_val, sparse_cells = FALSE, outcome = 2,
                                 sparse_min = NA_real_, sparse_below5 = NA_integer_,
                                 sparse_total = NA_integer_) {
            warn_div <- "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #ec971f; background-color: rgba(227, 144, 33, 0.07); color: inherit;'>"
            red_div  <- "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #d9534f; background-color: rgba(222, 55, 55, 0.06); color: inherit;'>"
            info_div <- "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #3c8dbc; background-color: rgba(72, 138, 188, 0.06); color: inherit;'>"
            block <- function(div, title, ...)
                paste0(div, "<b>", title, "</b> ", paste(c(...), collapse = " "), "</div>")

            warn <- ""
            has_bound <- length(kappaL_val) == 1 && is.finite(kappaL_val)

            # A bound at or below zero is the clinically decisive case: the study cannot exclude
            # agreement no better than chance, whatever the point estimate turns out to be.
            # "Use a less extreme prevalence" used to be offered here, but in the low-kappa0 /
            # small-n region that triggers this block balancing the prevalence LOWERS the bound
            # (kappa0 0.2, n 30: prevalence 0.30 -> -0.088, 0.50 -> -0.099, 0.05 -> -0.025), so
            # only the three remedies that move the bound the right way are listed.
            if (has_bound && kappaL_val <= 0) {
                warn <- paste0(warn, block(red_div,
                    .("This sample size cannot demonstrate agreement."),
                    .fmt(
                        .("The expected lower bound is {bound}, at or below zero, so even if the study observes the anticipated kappa it will not be able to rule out agreement no better than chance. Enrol more subjects, add raters, or relax the significance level."),
                        bound = private$.fmtBound(kappaL_val))))
            } else if (has_bound && kappaL_val < 0.40) {
                # A positive bound is not automatically a usable one. Below 0.40 the study
                # excludes only chance-level agreement and everything the module elsewhere
                # labels slight or fair (Landis and Koch 1977 cut-points 0.20/0.40/0.60/0.80,
                # the same scale R/agreement.b.R grades kappa on), so it cannot underwrite a
                # reliability claim. Amber, not red: the design is weak, not broken.
                #
                # The remedy is NOT the same in both halves of this block. The bound is a
                # confidence limit around kappa0 and approaches it from below without ever
                # reaching it (measured on kappaSize::FixedNBinary, kappa0 = 0.30, props
                # 0.30/0.70, 2 raters, alpha 0.05: 0.062 at n = 50, 0.130 at n = 100, 0.224 at
                # n = 500, 0.262 at n = 2000, 0.288 at n = 20000, 0.296 at n = 200000; six
                # raters and alpha 0.20 buy 0.008 more at most, and over 200 random designs
                # the bound never once reached kappa0). So when the anticipated kappa0 is
                # itself under 0.40 NO sample size, rater count or alpha can lift the bound
                # past 0.40, and "enrol more subjects" is advice the reader cannot act on -
                # which is exactly the case that fires most often. Say what actually binds.
                # `<=`, not `<`: the search steps DOWN from kappa0, so kappaL <= kappa0 - 0.001
                # always. At kappa0 exactly 0.40 the bound tops out at 0.398 even at n = 1e6 (the
                # compiled maximum), so no reachable design clears 0.40 and the "enrol more
                # subjects" branch would be just as unfollowable as it is below 0.40.
                remedy <- if (self$options$kappa0 <= 0.40)
                    .("The bound is a confidence limit around the anticipated kappa and approaches it from below without ever reaching it, so while the anticipated kappa is itself 0.40 or below no sample size, number of raters or significance level can lift the bound past 0.40; the anticipated agreement, not the number of subjects, is what binds here.")
                else
                    .("Enrol more subjects, add raters, or relax the significance level.")
                warn <- paste0(warn, block(warn_div,
                    .("This design cannot support a reliability claim."),
                    .fmt(
                        .("The expected lower bound is {bound}. The study excludes agreement below that value but leaves everything above it open, and on the Landis and Koch scale a bound under 0.40 is no better than fair agreement. These subjects can therefore show that agreement exceeds chance, but cannot establish moderate or better reliability even if the raters perform exactly as anticipated."),
                        bound = private$.fmtBound(kappaL_val)),
                    remedy))
            }

            # Sparse goodness-of-fit cells (see kappaSizeGofCells), judged by Cochran's rule: no expected
            # count below 1 and at most one cell in five below 5. A bare "any cell < 5" flagged
            # the default design with four raters on one cell of 1.85 and so fired on nearly
            # every multi-rater study, hiding the cases that matter (six raters: 1.08 and 0.11).
            if (isTRUE(sparse_cells)) {
                remedy <- if (outcome == 2)
                    .("Consider enriching the case series so the rare finding is more common (the calculation assumes the stated prevalence), adding subjects, or relaxing the significance level.")
                else
                    .("Consider collapsing rare categories or enrolling more subjects.")
                warn <- paste0(warn, block(warn_div,
                    .("Sparse categories."),
                    .fmt(
                        .("At this sample size the agreement-pattern cells (for example, exactly k of the raters calling the finding present, or all raters agreeing on one category) are too sparse at the reported lower bound: the smallest expected count is {min} and {below} of {total} cells are below 5. The calculation rests on a large-sample chi-square approximation, so the bound shown is less dependable here."),
                        min = private$.fmtCount(sparse_min), below = sparse_below5,
                        total = sparse_total),
                    remedy))
            }

            info <- block(info_div,
                .("Methodology."),
                .("With the sample size already fixed, this reports the lower bound of the one-sided 100(1 - alpha)% confidence interval that the study can expect to achieve for the intraclass (Fleiss-type) kappa of the common-correlation model used by the kappaSize package (Donner and Eliasziw; Rotondi and Donner); for two raters with equal marginal frequencies this coincides with Cohen's kappa."),
                .("It answers 'given the subjects I have, how little agreement am I still unable to rule out?' - the mirror image of the sample-size question; every kappa below the bound is excluded."),
                .("kappaSize searches downward in steps of 0.001 and reports the first value rejected, so the bound is conservative by at most 0.001 and its third decimal is the search resolution, not estimation precision."),
                .("This is the bound the study reaches if the agreement it observes lands exactly on kappa0. Roughly half of such studies will observe less agreement than anticipated and end with a lower bound below the figure shown, so read it as a planning expectation rather than a guarantee."),
                .("Note that kappa0 here is the agreement you anticipate observing, not a null hypothesis value as it is in kappaSizePower."),
                # The engine prints its own "expected cell count is less than five" line into
                # the Analysis result pane, in almost the same words as the Sparse categories
                # block above but from a DIFFERENT and looser check - kappaSize inspects the
                # category prevalences you typed (for a binary design, only the prevalence of
                # the finding, because FixedNBinary keeps props[1] alone), not the
                # agreement-pattern cells the chi-square divides by. The two can and do
                # disagree, so say which is which rather than leaving the reader to guess
                # whether they are seeing one problem or two.
                #
                # The cross-reference to the Sparse categories block is CONDITIONAL. It used to
                # be part of the sentence above and so shipped in every Notes panel, including
                # the ones with no such block - a dangling "the warning above" pointing at
                # nothing, and a panel that says "Sparse categories" to a user whose design is
                # perfectly dense.
                .("If the Analysis result panel also prints 'At least one expected cell count is less than five', that is a separate and looser check: kappaSize applies it to the outcome category prevalences you entered (for a binary outcome, to the prevalence of the finding alone), not to the agreement-pattern cells the chi-square actually divides by, and those empty out long before any category prevalence does. Seeing one warning without the other is expected."),
                if (isTRUE(sparse_cells))
                    .("The warning above is the check that matters for the bound, because it is the one computed on those agreement-pattern cells."))

            paste0(warn, info)
        },

        # Human-readable list: "0.20 and 0.80" or "0.20, 0.30 and 0.50"
        .formatProps = function(tokens) {
            k <- length(tokens)
            if (k == 0) return("")
            if (k == 1) return(tokens)
            if (k == 2) return(.fmt(.("{a} and {b}"), a = tokens[1], b = tokens[2]))
            .fmt(.("{head} and {last}"),
                            head = paste(tokens[-k], collapse = ", "), last = tokens[k])
        },

        # Validate all inputs; reject with an actionable message on failure. Kept outside any
        # tryCatch so jmvcore::reject surfaces as a proper jamovi error.
        # The generated option classes already enforce the List levels, the Integer type and
        # the Number ranges before .run() is entered, so the range clauses here are BACKSTOPS
        # for an R caller whose option bounds may one day be relaxed - they are not reached
        # from the jamovi GUI.
        .validateInputs = function(outcome, kappa0, parsed, alpha, n) {
            props4 <- parsed$values

            if (!outcome %in% c(2, 3, 4, 5))
                jmvcore::reject(.("Number of outcome levels must be 2, 3, 4, or 5."), code = NULL)

            if (length(props4) == 0 || any(is.na(props4)))
                jmvcore::reject(
                    .("Proportions must be numbers separated by commas, semicolons or spaces (for example '0.20, 0.80'). One or more entries could not be read as a number. Note that a decimal comma is not recognised - use a point, as in 0.20."),
                    code = NULL)

            # "0,30 0,70" splits into 0, 30, 0, 70 and would be reported as the wrong COUNT of
            # proportions, which sends the user looking in the wrong place. Diagnose the decimal
            # separator first, as the two siblings do.
            if (any(props4 >= 1, na.rm = TRUE)) {
                as_decimal <- suppressWarnings(as.numeric(trimws(unlist(strsplit(
                    gsub("([0-9]),([0-9])", "\\1.\\2", parsed$raw),
                    "[;|[:space:]]+")))))
                as_decimal <- as_decimal[!is.na(as_decimal)]
                if (length(as_decimal) > 0 && all(as_decimal > 0 & as_decimal < 1))
                    jmvcore::reject(
                        .("Proportions must use a decimal point, not a decimal comma: write 0.30, 0.70 rather than 0,30 0,70."),
                        code = NULL)
            }

            # A binary outcome may be given as one prevalence or as two proportions summing
            # to 1 - kappaSize::FixedNBinary accepts both (it does props <- props[1] after the
            # sum check); the 3/4/5-category engines require exactly N proportions.
            if (outcome == 2) {
                if (!(length(props4) %in% c(1L, 2L)))
                    jmvcore::reject(
                        .fmt(
                            .("For a binary outcome enter either one prevalence value or two proportions that sum to 1 (received {got})."),
                            got = length(props4)),
                        code = NULL)
            } else if (length(props4) != outcome) {
                jmvcore::reject(
                    .fmt(
                        .("Enter exactly {k} proportions for {k} outcome levels (received {got})."),
                        k = outcome, got = length(props4)),
                    code = NULL)
            }

            if (any(props4 <= 0) || any(props4 >= 1))
                jmvcore::reject(.("Each proportion must be strictly between 0 and 1."), code = NULL)

            # Only meaningful when two or more proportions were given. Uses the engine's own
            # predicate (abs(sum - 1) >= 0.001) so its message is never bypassed at sum = 1.001.
            if (length(props4) >= 2 && abs(sum(props4) - 1) >= 0.001)
                jmvcore::reject(
                    .fmt(
                        .("Proportions must sum to 1 (current sum = {sum})."),
                        sum = round(sum(props4), 4)),
                    code = NULL)

            # Backstop: every kappaSize FixedN* engine stops on n <= 10, and an Inf used to
            # hang its search loop (the test statistic becomes NaN).
            if (!is.finite(n) || n < 11)
                jmvcore::reject(
                    .fmt(
                        .("Sample size (N) must be a whole number of at least 11. The kappaSize method is a large-sample approximation and its engine refuses any study of 10 or fewer subjects (received {n})."),
                        n = n),
                    code = NULL)

            # Backstops for the compiled (0.01, 0.99) and (0.001, 0.20) bounds.
            if (!is.finite(kappa0) || kappa0 <= 0 || kappa0 >= 1)
                jmvcore::reject(.("kappa0 must be strictly between 0 and 1."), code = NULL)

            if (!is.finite(alpha) || alpha < 0.001 || alpha > 0.20)
                jmvcore::reject(
                    .("Significance level (alpha) must be between 0.001 and 0.20."), code = NULL)

            invisible(TRUE)
        },

        .run = function() {

            # Blank the panels first: .validateInputs() rejects before anything is written, so a
            # failed re-run must not leave the PREVIOUS run's numbers on screen under the error.
            self$results$text1$setContent("")
            self$results$text_summary$setContent("")
            self$results$text2$setContent("")
            self$results$notices$setContent("")

            outcome <- as.numeric(self$options$outcome)
            kappa0  <- self$options$kappa0
            raters  <- as.numeric(self$options$raters)
            alpha   <- self$options$alpha
            n       <- self$options$n

            parsed <- private$.parseProps(self$options$props)
            props4 <- parsed$values

            private$.validateInputs(outcome, kappa0, parsed, alpha, n)

            kappa_fn <- switch(
                as.character(outcome),
                "2" = kappaSize::FixedNBinary,
                "3" = kappaSize::FixedN3Cats,
                "4" = kappaSize::FixedN4Cats,
                "5" = kappaSize::FixedN5Cats
            )

            # Convert any kappaSize error into a readable jamovi message. The reject() runs in
            # the error handler (after tryCatch has returned), so it propagates normally.
            # n goes in as an integer: the engine cat()s the value it was given, and a double
            # of 100000 prints as "1e+05" in the Analysis result and Summary panes.
            result <- tryCatch(
                kappa_fn(
                    kappa0 = kappa0,
                    n      = as.integer(n),
                    props  = props4,
                    alpha  = alpha,
                    raters = raters
                ),
                error = function(e)
                    jmvcore::reject(
                        .fmt(
                            .("kappaSize could not compute the expected lower bound: {error}"),
                            error = conditionMessage(e)),
                        code = NULL)
            )

            # kappaSize's search decrements rho from kappa0 by 0.001 with no floor, and an
            # underpowered design walks straight out of the common-correlation model: the
            # probability of "all raters call the finding present" is p^r (1 - rho) + rho p,
            # which turns negative below -p^(r-1) / (1 - p^(r-1)). With prevalence 0.02, three
            # raters, kappa0 0.01, n 100 and alpha 0.2 the engine returns -0.841 (model floor
            # -0.0004) and with prevalence 0.02, two raters, n 11, alpha 0.001 it returns -23.78.
            # Neither is a lower limit for kappa; a "< -1" check catches only the second.
            kappaL_val <- suppressWarnings(as.numeric(result$kappaL))
            has_bound  <- length(kappaL_val) == 1 && is.finite(kappaL_val)
            cells <- if (has_bound) kappaSizeGofCells(outcome, raters, props4, kappaL_val)
                     else NA_real_

            # `any(cells < 0)` alone is BLIND for a 3-5 category outcome. There each
            # "all raters agree on category j" cell is a PRODUCT of `raters` factors
            # (p_j (1 - rho) + i rho) / ((1 - rho) + i rho), i = 0..raters-1, so when the
            # bound runs far enough negative that an EVEN number of numerator factors flips
            # sign the product comes back POSITIVE and the out-of-model design sails through
            # (verified: outcome 5, raters 3, props 0.01/0.02/0.02/0.05/0.90, n 11,
            # kappa0 0.01, alpha 0.001 -> kappaL -0.704, every cell positive and summing to
            # 1; and outcome 3, raters 3, props 0.05/0.15/0.80, n 15, kappa0 0.05,
            # alpha 0.001 -> kappaL -0.079). Test the FACTORS, not the product. For rho >= 0
            # every factor is a sum of non-negative terms, so this can only reject a bound
            # that is already negative (checked over 20,000 random rho >= 0 designs: zero
            # rejections). The binary cells are sums, not products, so they expose the
            # violation directly and need no extra test.
            in_model <- if (!has_bound || outcome == 2) has_bound else {
                i <- seq_len(raters) - 1
                all((1 - kappaL_val) + i * kappaL_val > 0) &&
                    all(outer(props4, i,
                              function(p, k) p * (1 - kappaL_val) + k * kappaL_val) >= 0)
            }

            if (!has_bound || !all(is.finite(cells)) || any(cells < 0) || !in_model)
                jmvcore::reject(
                    .fmt(
                        .("The calculation did not converge to a usable answer: the search returned {bound}, which is below the lowest agreement the model allows for these prevalences (every agreement pattern must keep a non-negative probability). With this combination of sample size, anticipated kappa and category prevalences the large-sample approximation breaks down. Increase N, use a less extreme prevalence, or raise the significance level."),
                        bound = if (has_bound) private$.fmtBound(kappaL_val) else "NA"),
                    code = NULL)

            # --- Populate outputs ---------------------------------------------
            # The engine prints its marginal cell-count warning once per rare category (five
            # times for five levels); keep the first, the Notes panel carries the real check.
            dedupe <- function(lines)
                lines[!(duplicated(lines) & grepl("expected cell count", lines, fixed = TRUE))]
            self$results$text1$setContent(
                paste(dedupe(utils::capture.output(print(result))), collapse = "\n"))
            self$results$text_summary$setContent(
                paste(dedupe(utils::capture.output(summary(result))), collapse = "\n"))

            prev_txt <- if (outcome == 2 && length(parsed$tokens) == 1) {
                .fmt(.("Further suppose that the prevalence of the trait is {p}."),
                                p = parsed$tokens[1])
            } else {
                .fmt(
                    .("Further suppose that the proportions of the outcome categories are {props}."),
                    props = private$.formatProps(parsed$tokens))
            }

            # A lower confidence bound is the smallest kappa still compatible with the
            # anticipated result; everything BELOW it is ruled out. The old sentence ("the lowest
            # value of kappa that the study can expect to rule out") said the opposite.
            text2 <- paste0(
                private$.wrap(paste(
                    .fmt(
                        .("Researchers anticipate an agreement of kappa = {kappa0} and have access to {n} subjects, each rated by {raters} raters."),
                        kappa0 = kappa0, n = base::format(n, scientific = FALSE),
                        raters = raters),
                    prev_txt)),
                "\n",
                private$.wrap(.fmt(
                    .("They would like to know how low the one-sided {conf}% lower confidence bound for kappa can be expected to fall - the smallest agreement the study would still be unable to rule out; every value below it is excluded."),
                    conf = base::format(100 * (1 - alpha), scientific = FALSE))),
                "\n",
                private$.wrap(.fmt(
                    .("The expected lower bound for kappa is {bound}."),
                    bound = private$.fmtBound(kappaL_val))),
                if (kappaL_val <= 0)
                    paste0("\n", private$.wrap(
                        .("A bound at or below zero means this many subjects cannot rule out agreement no better than chance.")))
                else "")
            self$results$text2$setContent(text2)

            # Cochran's rule on the expected counts at the reported bound (see .buildNotices).
            e <- cells * n
            self$results$notices$setContent(private$.buildNotices(
                kappaL_val,
                sparse_cells  = any(e < 1) || mean(e < 5) > 0.2,
                outcome       = outcome,
                sparse_min    = min(e),
                sparse_below5 = sum(e < 5),
                sparse_total  = length(e)))
        })
)
