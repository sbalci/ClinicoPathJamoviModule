#' @title Lowest Expected Kappa for a Fixed Sample Size
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
        # Resolved 2026-10-16 (/fix-function after development-ideas/kappaSizeFixedN-validation-2026-10-16.md):
        #   the bound is solved directly instead of read off kappaSize's 0.001 walk (VAL-01, VAL-04);
        #   except for a binary outcome with two raters it is the lower of the fixed-proportion and
        #   restricted-ML bounds, as kappaSizeCI sizes n (VAL-03); the sparse rule judges the pooled
        #   cells of the 1-df test (VAL-02); small-sample box (VAL-05); refs and model names (VAL-06);
        #   no "relax alpha" sparse remedy (VAL-07).

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

        # The goodness-of-fit cell probabilities live in R/utils-kappasize.R (kappaSizeGofCells),
        # shared by the three kappaSize backends.

        # Preformatted panes do not wrap; wrap at render time so translated text wraps too.
        .wrap = function(x, width = 78) paste(strwrap(x, width = width), collapse = "\n"),

        # One rendering of the bound for every pane. The bound is solved to ~1e-12, so the third
        # decimal is now precision, not kappaSize's search step. A positive bound is rounded DOWN:
        # the figure shown never overstates it, and it sits on the same side of 0.40 as the
        # unrounded value the verdicts test (rounding to nearest showed 0.39996 as "0.400" under
        # an amber "under 0.40"). Within 0.001 of zero two significant digits keep its sign
        # visible ("0.00053", not "0.000", beside a verdict that chance is excluded); a positive one
        # is rounded DOWN there too (0.000197 -> "0.00019", not "0.0002"; IR-02). A negative
        # bound is rounded to nearest, not down: rounding down could print a value below the
        # model's floor (-0.005024728 -> -0.006 against a floor of -0.005025).
        .fmtBound = function(x) {
            if (!isTRUE(is.finite(x))) return("NA")
            if (abs(x) < 0.001) {
                if (x > 0) { m <- 10^(floor(log10(x)) - 1); x <- floor(round(x / m, 6)) * m }
                return(base::format(signif(x, 2), scientific = FALSE, trim = TRUE))
            }
            v <- if (x > 0) floor(round(x * 1000, 6)) / 1000 else round(x, 3)
            formatC(v, format = "f", digits = 3)
        },

        # kappaSize's own figure, for the Summary pane comparison. Its 0.001 walk accumulates
        # floating-point error (0.6 walked 200 steps is 0.3999999999999998, 0.2 walked 200 steps
        # is -1.6e-16), so it is snapped before it is printed or compared.
        .fmtEngine = function(x) base::format(round(x, 10), digits = 7, scientific = FALSE, trim = TRUE),

        # Expected agreement-pattern counts run from ~1e-06 to a few dozen. scientific = FALSE stops
        # "2.4e-06" in prose; the floor stops the tail growing unreadably long; and signif(x, 2)
        # alone printed 4.95-4.99 as "5" beside "the number of cells below 5 is 1 of 3". Identical
        # to R/kappaSizeCI.b.R:.fmtCount and R/kappaSizePower.b.R:.fmtCount.
        .fmtCount = function(x) {
            if (!isTRUE(is.finite(x))) return(.("unavailable"))
            if (x < 0.0001) return(.("below 0.0001"))
            s <- signif(x, 2)
            if (x < 5 && s >= 5) {
                s <- signif(x, 3)                        # 4.97 -> "4.97"
                if (s >= 5) s <- floor(x * 100) / 100    # 4.9996 -> "4.99"
            }
            base::format(s, scientific = FALSE, trim = TRUE)
        },

        # Number of subjects, grouped by thousands. Identical to R/kappaSizeCI.b.R:.fmtN.
        .fmtN = function(n) {
            if (!isTRUE(is.finite(n))) return("NA")
            base::format(round(n), scientific = FALSE, big.mark = ",", trim = TRUE)
        },

        # Every design except a binary outcome rated by two raters: there Donner & Eliasziw's
        # published interval plugs in the observed proportion and keeps its stated coverage
        # (0.953 simulated at the defaults, validation 2026-10-16), and re-estimating is not what
        # anyone is told to do. Same condition as R/kappaSizeCI.b.R:.reestimationApplies.
        .reestimationApplies = function(outcome, raters) {
            !(isTRUE(outcome == 2) && isTRUE(raters == 2))
        },

        # The cells of the 1-df goodness-of-fit test: for a binary outcome with 3+ raters every split
        # call is pooled into one disagreement cell (Altaye, Donner & Klar 2001 p.586); under their
        # eq. (4) the interior cells are proportional, so pooling leaves the chi-square unchanged
        # (max difference 8.9e-15 over 240 designs) but not the sparse-cell judgement (VAL-02).
        .pooledCells = function(outcome, raters, props, rho) {
            e <- kappaSizeGofCells(outcome, raters, props, rho)
            if (outcome == 2 && raters >= 3) c(e[1], sum(e[2:raters]), e[raters + 1]) else e
        },

        # TRUE when rho is inside the common-correlation model for these proportions. Binary cells
        # are sums, so a negative cell shows the violation directly. A 3-5 category "all raters
        # agree on j" cell is a PRODUCT of `raters` factors (p_j (1 - rho) + i rho) /
        # ((1 - rho) + i rho), i = 0..raters-1, so an even number of negative factors comes back
        # positive (outcome 5, raters 3, props .01/.02/.02/.05/.90 at rho -0.704: every cell
        # positive, summing to 1): test the factors, not the product. For rho >= 0 every factor is
        # a sum of non-negative terms, so this only ever rejects a negative rho.
        .inModel = function(outcome, raters, props, rho) {
            cells <- kappaSizeGofCells(outcome, raters, props, rho)
            if (!all(is.finite(cells)) || any(cells < 0)) return(FALSE)
            if (outcome == 2) return(TRUE)
            i <- seq_len(raters) - 1
            all((1 - rho) + i * rho > 0) &&
                all(outer(props, i, function(p, k) p * (1 - rho) + k * rho) >= 0)
        },

        # The lowest rho at which the cells are a valid distribution. Binary: the all-positive cell
        # p^R (1 - rho) + rho p (and its mirror) reaches 0 at -p^(R-1) / (1 - p^(R-1)). 3-5 categories:
        # the binding factor is i = R - 1, p_j (1 - rho) + (R - 1) rho >= 0, and 1 + (R - 2) rho > 0.
        .modelFloor = function(outcome, raters, props) {
            if (outcome == 2) {
                p <- props[1]
                return(max(-p^(raters - 1) / (1 - p^(raters - 1)),
                           -(1 - p)^(raters - 1) / (1 - (1 - p)^(raters - 1))))
            }
            f <- max(-props / (raters - 1 - props))
            if (raters >= 3) f <- max(f, -1 / (raters - 2))
            f
        },

        # The expected lower bound with the proportions held at their anticipated values: the
        # minimal admissible root of N X2(rho) = qchisq(1 - 2 alpha, 1), the observed table being the
        # expected one at kappa0 (Donner & Eliasziw 1992 p.1514; Rotondi & Donner 2012 p.780). X2 falls
        # monotonically to 0 at kappa0 and grows without bound at the model floor, where a cell with
        # positive expected frequency reaches 0, so the root exists and is unique; it is solved, not
        # searched. kappaSize walks down in steps of 0.001 and stops at X2 >= crit + 0.001, which put
        # its figure up to 0.00115 below this root and let it step past the floor altogether
        # (prevalence 0.02, 3 raters, kappa0 0.01, n 100, alpha 0.2: -0.841 against a root of
        # -0.0004). NA when neither offset brackets the root. The root always exists, but for an
        # extremely rare category it lies closer to the floor than either offset (prevalence 1e-10,
        # 2 raters, kappa0 0.01, n 11, alpha 0.001: 1.15e-14 above it; /fix-function 2026-10-17).
        .fixedBound = function(outcome, raters, props, kappa0, n, crit) {
            q <- private$.pooledCells(outcome, raters, props, kappa0)
            f <- function(r) {
                b <- private$.pooledCells(outcome, raters, props, r)
                n * sum((q - b)^2 / b) - crit
            }
            fl <- private$.modelFloor(outcome, raters, props)
            for (eps in c(1e-10, 1e-13)) {
                lo <- fl + eps * max(1, abs(fl))
                v <- f(lo)
                if (is.finite(v) && v > 0 && private$.inModel(outcome, raters, props, lo)) {
                    hi <- kappa0 - 1e-12
                    return(stats::uniroot(f, c(lo, hi), tol = 1e-13)$root)
                }
            }
            NA_real_
        },

        # The same bound when the study's analysis RE-ESTIMATES the category proportions by maximum
        # likelihood at each candidate kappa, from the pooled cells (Altaye, Donner & Eliasziw 2001
        # eqs (8)-(9), p.2483), the Pearson chi-square then taken against the fitted cells. That is the
        # interval that keeps its stated coverage with three or more raters or categories: simulated
        # at R = 2000, one-sided coverage 0.927 / 0.919 for the plug-in against 0.938 / 0.953 when
        # re-estimated (binary, 4 raters, p .1, n 100; RD2012's 3 categories, 3 raters, n 171). The same
        # likelihood as R/kappaSizeCI.b.R:.rmlSlopes, which still fits it with BFGS over the plain simplex
        # (that copy did not reproduce IR-03 in 912 rare-category fits; consolidation is a TODO).
        #
        # The restricted-ML fit at one agreement level: returns a function of rho giving the fitted
        # proportions and the Pearson statistic against the fitted cells (n-free), or NULL when no fit
        # exists. Every fit starts from the anticipated proportions (when the model allows them at rho) and
        # from equal proportions, keeping the better likelihood, so the statistic is a function of rho
        # alone. A warm start from the previous fit made it depend on the order of evaluation instead: one
        # rho gave -2.54 fitted first and +3.83 after a fit 0.01 lower, so uniroot saw no change of sign.
        #
        # The fit is over the proportions the model ALLOWS at rho: every one at least t, where t is 0 for
        # rho >= 0 and otherwise -(R - 1) rho / (1 - rho) for 3-5 categories (the factor
        # p_j (1 - rho) + (R - 1) rho of .inModel) and (-rho / (1 - rho))^(1 / (R - 1)) for a binary
        # outcome (the end cell p^R (1 - rho) + rho p). Writing p = t + (1 - K t) softmax(theta) puts every
        # theta inside the model and the boundary at infinity, where a cell the data fill goes to zero.
        # IR-03: fitting over the plain simplex with a flat 1e10 outside it, BFGS's finite-difference probes
        # crossed the boundary near a rare category's optimum and it stopped short (rho -0.025: -log L
        # 0.75903 against 0.75880 at the optimum), or never started from a warm start that had left the
        # model; 18 of 160 extreme designs gave up although a root existed, and others gave a wrong root.
        # Near that floor the likelihood is also nearly flat in the rare category's share, so BFGS ran onto
        # its iteration cap even inside the model. nlminb (PORT), restarted from its own solution while
        # -log L still falls, reached the multi-start optimum to 1e-12 where BFGS did not. Its "singular
        # convergence" code is what that flat direction produces, so a fit is judged by its result (finite,
        # inside the model), not by the code.
        # Ceiling: when the rare category's expected count is far below 1 (kappa0 near 0 with a 1-2%
        # category), the Pearson statistic against fitted cells that small is ill-conditioned - likelihoods
        # equal to 1e-12 gave statistics 30% apart - so the bound there is approximate whatever the
        # optimiser; the Cochran rule flags exactly those designs as sparse. It need not be monotone there
        # either: it can spike near rho = 0, where the rare category's fitted share goes to 0, and
        # .rmlBound() reports the first crossing below the fixed root, the lower (conservative) reading.
        # .inModel() still checks every fit, so a product with an even number of negative factors cannot
        # pass as one.
        .rmlFitter = function(outcome, raters, props, kappa0) {
            K <- length(props)
            q <- private$.pooledCells(outcome, raters, props, kappa0)
            function(rho) {
                t <- if (rho >= 0) 0
                     else if (outcome == 2) (-rho / (1 - rho))^(1 / (raters - 1))
                     else -(raters - 1) * rho / (1 - rho)
                if (K * t >= 1 || 1 + (raters - 2) * rho <= 0) return(NULL)
                to_props <- function(th) { e <- exp(c(th, 0) - max(c(th, 0))); t + (1 - K * t) * e / sum(e) }
                nll <- function(th) {
                    b <- private$.pooledCells(outcome, raters, to_props(th), rho)
                    if (!all(is.finite(b)) || any(b <= 0)) return(1e10)
                    -sum(q * log(b))
                }
                fit_from <- function(st) {
                    fit <- NULL
                    for (k in 1:20) {
                        f <- tryCatch(
                            stats::nlminb(if (is.null(fit)) st else fit$par, nll,
                                          control = list(rel.tol = 1e-15, x.tol = 1e-12, eval.max = 2000, iter.max = 1000)),
                            error = function(e) NULL)
                        if (is.null(f) || !(f$objective < (if (is.null(fit)) 1e10 else fit$objective - 1e-15))) break
                        fit <- f
                    }
                    fit
                }
                w <- (props - t) / (1 - K * t)
                starts <- c(if (all(w > 0)) list(log(w[-K] / w[K])), list(rep(0, K - 1)))
                fit <- NULL
                for (st in starts) {
                    f <- fit_from(st)
                    if (!is.null(f) && (is.null(fit) || f$objective < fit$objective)) fit <- f
                }
                if (is.null(fit)) return(NULL)
                p <- to_props(fit$par)
                if (!private$.inModel(outcome, raters, p, rho)) return(NULL)
                b <- private$.pooledCells(outcome, raters, p, rho)
                if (any(b <= 0)) return(NULL)
                list(props = p, stat = sum((q - b)^2 / b))
            }
        },

        # `start` is the fixed-proportion root. Returns status "rml" with the bound and the fitted
        # proportions when re-estimation gives the LOWER bound, "fixed" when it does not (its statistic
        # already exceeds the critical value at the fixed root), "failed" when a fit failed.
        .rmlBound = function(outcome, raters, props, kappa0, n, crit, start) {
            fit_at <- private$.rmlFitter(outcome, raters, props, kappa0)
            g <- function(r) { ft <- fit_at(r); if (is.null(ft)) NA_real_ else n * ft$stat - crit }
            failed <- list(status = "failed")

            v <- g(start)
            if (is.na(v)) return(failed)
            if (v >= 0) return(list(status = "fixed"))
            # Step down until the statistic reaches the critical value. A step that lands where no fit
            # exists (below the lowest rho the model allows for any proportions) has no sign, so it is
            # halved towards `hi`, which keeps g(hi) < 0, until a sign is found; only then is it a bracket.
            hi <- start
            step <- 0.01
            repeat {
                lo <- hi - step
                if (lo <= -1) return(failed)
                v <- g(lo)
                if (is.na(v)) {
                    step <- step / 2
                    if (step < 1e-7) return(failed)
                    next
                }
                if (v >= 0) break
                hi <- lo
            }
            root <- tryCatch(
                stats::uniroot(function(r) { x <- g(r); if (is.na(x)) stop("fit failed"); x },
                               c(lo, hi), tol = 1e-9)$root,
                error = function(e) NA_real_)
            if (!is.finite(root)) return(failed)
            ft <- fit_at(root)
            if (is.null(ft)) return(failed)
            list(status = "rml", bound = root, props = ft$props)
        },

        # Notes panel, at the top of the results: the red and amber warnings only ("" when there are none);
        # the method statement is the Methodology panel at the foot (.buildMethodology), so the answer is
        # no longer pushed below some 250 words of method. `sparse_*` carry the Cochran-rule verdict plus
        # the numbers behind it so the notice can say how sparse, not just that it is.
        .buildNotices = function(kappaL_val, sparse_cells = FALSE, outcome = 2,
                                 sparse_min = NA_real_, sparse_below5 = NA_integer_,
                                 sparse_total = NA_integer_, n = NA_real_, kappa0 = NA_real_,
                                 liftable = NA, min_prop = NA_real_) {
            block <- private$.block

            warn <- ""
            has_bound <- length(kappaL_val) == 1 && is.finite(kappaL_val)

            # Fewer than one subject per rater expected in the rarest category (/review-function F1,
            # 2026-10-17). The bound cannot fall far below the model floor, and for a rare category that
            # floor is next to zero (binary, p .02, 6 raters: -3.2e-9), so the `<= 0` test below could not
            # fire however few the subjects: 117 of 180 binary designs with under 2 expected positives
            # (n p 0.22-1.5) showed a positive bound and "agreement exceeds chance". Not worded as
            # impossibility: for 3-5 categories the common categories can still show agreement.
            rare_k <- n * min_prop
            too_few <- has_bound && isTRUE(rare_k < 1)
            rare_args <- if (isTRUE(is.finite(rare_k))) list(
                share = paste0(base::format(signif(100 * min_prop, 3), scientific = FALSE, trim = TRUE), "%"),
                k = base::format(signif(rare_k, 2), scientific = FALSE, trim = TRUE),
                n = private$.fmtN(n))

            # A bound at or below zero is the clinically decisive case: the study cannot exclude
            # agreement no better than chance, whatever the point estimate turns out to be.
            # "Use a less extreme prevalence" used to be offered here, but in the low-kappa0 /
            # small-n region that triggers this block balancing the prevalence LOWERS the bound
            # (kappa0 0.2, n 30: prevalence 0.30 -> -0.088, 0.50 -> -0.099, 0.05 -> -0.025), so
            # only the three remedies that move the bound the right way are listed.
            # The tests run on the SOLVED bound. They used to run on kappaSize's walked float, which
            # lands on 0 as -1.6e-16 and on 0.400 as 0.3999999999999998 for every two-decimal kappa0,
            # and which is the first value rejected, so even an exact 0 meant chance WAS excluded
            # (kappa0 0.2, p 0.5, n 68: walked -1.6e-16 under a red "cannot rule out chance"; solved
            # +0.00053) (VAL-01).
            if (has_bound && kappaL_val <= 0) {
                warn <- paste0(warn, block("red",
                    .("This sample size cannot demonstrate agreement."),
                    .fmt(
                        .("The expected lower bound is {bound}, at or below zero, so even if the study observes the anticipated kappa it will not be able to rule out agreement no better than chance. Enrol more subjects, add raters, or relax the significance level."),
                        bound = private$.fmtBound(kappaL_val))))
            } else if (has_bound && kappaL_val < 0.40 && !too_few) {
                # A positive bound is not automatically a usable one. Below 0.40 the study
                # excludes only chance-level agreement and everything the module elsewhere
                # labels slight or fair (Landis and Koch 1977 cut-points 0.20/0.40/0.60/0.80,
                # the same scale R/agreement.b.R grades kappa on), so it cannot underwrite a
                # reliability claim. Amber, not red: the design is weak, not broken.
                #
                # The remedy is NOT the same throughout this block. The bound is a confidence
                # limit around kappa0 and approaches it from below without ever reaching it, so
                # when the anticipated kappa0 is itself 0.40 or below NO sample size, rater count
                # or alpha can lift the bound past 0.40, and "enrol more subjects" is advice the
                # reader cannot act on. Just above 0.40 it can still be out of reach within the
                # option limits (kappa0 0.4001: 0.399 at n 1e6, 6 raters, alpha 0.20; IR-04), so
                # `liftable` (.liftable(), computed in .run()) decides; NA means not known, and the
                # general remedy stays.
                remedy <- if (isTRUE(kappa0 <= 0.40))
                    .("The bound is a confidence limit around the anticipated kappa and approaches it from below without ever reaching it, so while the anticipated kappa is itself 0.40 or below no sample size, number of raters or significance level can lift the bound past 0.40; the anticipated agreement, not the number of subjects, is what binds here.")
                else if (isFALSE(liftable))
                    .fmt(.("With an anticipated kappa of {kappa0} and these proportions, even 1,000,000 subjects, up to six raters and a significance level of 0.20 (the largest this analysis allows) leave the expected lower bound below 0.40, so changing the sample size, the raters or the significance level cannot support a claim of moderate or better reliability here."),
                         kappa0 = kappa0)
                else
                    .("Enrol more subjects, add raters, or relax the significance level.")
                # With sparse cells the chi-square the bound inverts is not dependable, so neither the
                # exclusion of agreement below the bound nor "exceeds chance" is claimed (F1).
                body <- if (isTRUE(sparse_cells))
                    .("The expected lower bound is {bound}, which on the Landis and Koch scale is no better than fair agreement, so these subjects cannot establish moderate or better reliability even if the raters perform exactly as anticipated. The goodness-of-fit cells are too sparse at this sample size (see Sparse categories) for the bound to be relied on, so it does not show that agreement exceeds chance either.")
                else
                    .("The expected lower bound is {bound}. The study excludes agreement below that value but leaves everything above it open, and on the Landis and Koch scale a bound under 0.40 is no better than fair agreement. These subjects can therefore show that agreement exceeds chance, but cannot establish moderate or better reliability even if the raters perform exactly as anticipated.")
                warn <- paste0(warn, block("amber",
                    .("This design cannot support a reliability claim."),
                    .fmt(body, bound = private$.fmtBound(kappaL_val)),
                    remedy))
            }

            if (too_few) {
                warn <- paste0(warn, block("red",
                    .("Too few expected subjects in the rarest category."),
                    do.call(.fmt, c(list(
                        .("The rarest category is expected in {share} of ratings, so each rater is expected to place only about {k} of the {n} subjects in it, fewer than one. The goodness-of-fit approximation behind the expected lower bound of {bound} does not hold with so few, and with a category this rare the model allows little or no agreement below zero, so the bound stays near or above zero however few the subjects are: it cannot support a dependable conclusion. Enrol more subjects, or enrich the case series so that the rare category is more common.")),
                        rare_args, list(bound = private$.fmtBound(kappaL_val))))))
            }

            # Sparse goodness-of-fit cells (the cells of the test the bound inverts, see .pooledCells),
            # judged by Cochran's rule: no expected count below 1 and at most one cell in five below 5.
            # The binary remedy no longer offers a larger alpha: re-run at alpha 0.20 that removed the
            # sparsity in 2 of 103 sparse designs, where doubling n removed it in 48 (VAL-07).
            if (isTRUE(sparse_cells)) {
                remedy <- if (outcome == 2)
                    .("Consider enriching the case series so the rare finding is more common (the calculation assumes the stated prevalence), or enrolling more subjects.")
                else
                    .("Consider collapsing rare categories or enrolling more subjects.")
                warn <- paste0(warn, block("amber",
                    .("Sparse categories."),
                    .fmt(
                        .("At this sample size the goodness-of-fit cells (all raters agreeing on one category, or the single pooled cell for any disagreement) are too sparse at the reported lower bound: the smallest expected count is {min}, and the number of cells below 5 is {below} of {total}. The calculation rests on a large-sample chi-square approximation, so the bound shown is less dependable here."),
                        min = private$.fmtCount(sparse_min), below = sparse_below5,
                        total = sparse_total),
                    remedy))
            }

            # A category under 5% (a warning heuristic, not a statistical boundary): the msgid of
            # R/kappaSizeCI.b.R's "Rare category." block, so both analyses say it alike (F1). Not repeated
            # under the red block above, which already gives the count.
            if (isTRUE(min_prop < 0.05) && !too_few && !is.null(rare_args)) {
                warn <- paste0(warn, block("amber",
                    .("Rare category."),
                    do.call(.fmt, c(list(
                        .("The rarest category is expected in {share} of ratings, so each rater is expected to place only about {k} of the {n} subjects in it. Kappa depends strongly on prevalence (Byrt, Bishop and Carlin): when a category is this rare, high raw agreement can coexist with a low kappa, the estimate rests on those few subjects, and a kappa measured here cannot be assumed to generalize to a setting where the category is more common. Report the prevalence and the category-specific agreement (the proportion of that category's ratings that another rater matched) beside kappa.")),
                        rare_args))))
            }

            # Donner & Eliasziw 1992 p.1518 and Rotondi & Donner 2012 p.782 document the
            # goodness-of-fit interval's coverage from about 25 subjects; the qualifiers are the
            # sources' own (binary outcomes; ADK 2001 p.587 "provided pi >= 0.2"), as in kappaSizeCI.
            # Simulated at n = 20 (2 raters, p .5, kappa0 .6) the coverage held (0.951), but 0.60 of
            # such studies observed less agreement than anticipated (VAL-05).
            if (isTRUE(is.finite(n)) && n < 25) {
                warn <- paste0(warn, block("amber",
                    .("Small sample."),
                    .("The number of subjects is below 25. For a binary outcome, the goodness-of-fit interval this calculation assumes was found close to its stated coverage from about 25 subjects when the rarer category makes up at least about a fifth of the ratings (Donner and Eliasziw; Altaye, Donner and Klar); for three to five categories no such figure was given, and the expected cell counts are the guide (Rotondi and Donner). Below 25 subjects it has not been shown to hold, so read the bound with caution.")))
            }

            warn
        },

        # One coloured panel: a bold title, then the sentences. Each style keeps its translucent rgba fill
        # and `color: inherit` in one literal so it reads on the dark theme (review guide, theme-safe HTML).
        .block = function(kind, title, ...) {
            div <- switch(kind,
                red   = "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #d9534f; background-color: rgba(222, 55, 55, 0.06); color: inherit;'>",
                amber = "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #ec971f; background-color: rgba(227, 144, 33, 0.07); color: inherit;'>",
                info  = "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #3c8dbc; background-color: rgba(72, 138, 188, 0.06); color: inherit;'>")
            # library-audit 2026-10-23 meddecide [LOW] DONE: the severity is a word in the title, not the border colour alone.
            label <- switch(kind, red = .("Important warning"), amber = .("Warning"), info = .("Note"))
            paste0(div, "<b>", label, ": ", title, "</b> ", paste(c(...), collapse = " "), "</div>")
        },

        # IR-04: TRUE when some design within the option limits (n 1e6, alpha 0.20, the current number of
        # raters up to 6: jamovi/kappaSizeFixedN.a.yaml, pinned by a test) has an expected lower bound of at
        # least 0.40, so "enrol more subjects, add raters, or relax the significance level" can be followed.
        # The bound is the one REPORTED, the lower of the fixed and re-estimated ones, so both must reach
        # 0.40 (outcome 3, .11/.70/.19, kappa0 0.4005, 6 raters: fixed 0.40005, re-estimated 0.39999), and
        # it is not monotone in raters (five equal categories, kappa0 0.17: 0.169613 / 0.169603 / 0.169581
        # at 4 / 5 / 6 raters), so every count is tried. Each statistic falls towards kappa0, so a bound of
        # at least 0.40 is the statistic at 0.40 reaching the critical value: one evaluation, no root.
        # NA when a fit fails: not known.
        .liftable = function(outcome, raters, props, kappa0) {
            n <- 1e6
            crit <- stats::qchisq(1 - 2 * 0.20, 1)
            unknown <- FALSE
            for (r in raters:6) {
                q <- private$.pooledCells(outcome, r, props, kappa0)
                b <- private$.pooledCells(outcome, r, props, 0.40)
                if (n * sum((q - b)^2 / b) < crit) next
                if (!private$.reestimationApplies(outcome, r)) return(TRUE)
                ft <- private$.rmlFitter(outcome, r, props, kappa0)(0.40)
                if (is.null(ft)) unknown <- TRUE
                else if (n * ft$stat >= crit) return(TRUE)
            }
            if (unknown) NA else FALSE
        },

        # Methodology panel, at the foot of the results. `rml` is the status from .rmlBound() ("rml",
        # "fixed", "failed"), or "na" when re-estimation does not apply; `engine_sparse` is TRUE when
        # kappaSize's own summary printed its expected-cell-count line, and `engine_ok` when its search
        # returned a figure inside the model. As in R/kappaSizeCI.b.R, the
        # caveats are always shown and the method detail waits for showExplanations (F2); a sentence
        # about the Summary panel appears only while that panel is shown. The builder defaults show
        # everything; .run() passes the two options.
        .buildMethodology = function(outcome, raters, alpha, rml = "na", sparse_cells = FALSE,
                                     engine_sparse = FALSE, show_details = TRUE, show_summary = TRUE,
                                     engine_ok = TRUE) {
            conf_pct <- if (isTRUE(is.finite(alpha)))
                paste0(base::format(100 * (1 - alpha), scientific = FALSE, trim = TRUE), "%")
            else
                "100(1 - alpha)%"
            reest <- private$.reestimationApplies(outcome, raters)

            caveats <- private$.block("info",
                .("This is a planning expectation, not a guarantee."),
                # The bound conditions on the proportions too: two raters, n 250, observed kappa exactly
                # 0.6 give 0.510 at prevalence .5 and 0.489 at .2 (/fix-function 2026-10-17).
                .("This is the bound the study reaches if the agreement it observes lands exactly on the anticipated kappa and its category proportions match the anticipated ones. Roughly half of such studies will observe less agreement than anticipated and end with a lower bound below the figure shown, so read it as a planning expectation rather than a guarantee."),
                # As kappaSizeCI: ClinicoPath's own agreement analysis gives Cohen's kappa a Wald or
                # Fisher-z interval, not this one (R/agreement.b.R, 2026-10-16).
                .("This bound is for the model-based goodness-of-fit interval; the Wald, Fisher-z or bootstrap intervals that agreement software usually reports for Cohen's or Fleiss' kappa can give a different lower limit for the same data. Report the goodness-of-fit interval, or plan for the interval that will be reported."),
                # VAL-03, as kappaSizeCI sizes n (IR-kappaSizeCI-01): the bound holds whichever way the
                # study is analysed, but only when the restricted fit ran.
                if (reest && identical(rml, "failed"))
                    .("For this design the proportions could not be re-estimated, so the bound holds them at their anticipated values only."),
                .("For any pair of raters this kappa equals Cohen's kappa when the raters share the same category frequencies (no rater bias); when their frequencies differ, it does not."),
                .("Note that the anticipated kappa here is the agreement you expect to observe, not a null hypothesis value as it is in kappaSizePower."),
                # The engine prints its own "expected cell count is less than five" line into the
                # Summary pane from a DIFFERENT and looser check - kappaSize inspects the category
                # proportions typed (for a binary design only props[1]), not the goodness-of-fit
                # cells. Both cross-references are conditional: unconditional, they pointed at nothing
                # in every panel without that line or without the Sparse block.
                if (isTRUE(show_summary) && isTRUE(engine_sparse))
                    .("The Summary panel's line 'At least one expected cell count is less than five' comes from a separate and looser check: kappaSize applies it to the category proportions you entered (for a binary outcome, to the first proportion only), not to the goodness-of-fit cells the chi-square divides by, and those empty out long before any category proportion does. Seeing one warning without the other is expected."),
                if (isTRUE(show_summary) && isTRUE(sparse_cells))
                    .("The Sparse categories warning above is the check that matters for the bound, because it is the one computed on the goodness-of-fit cells."))

            if (!isTRUE(show_details)) return(caveats)

            details <- private$.block("info",
                .("Methodology."),
                .fmt(
                    .("With the sample size already fixed, this reports the expected lower bound of the one-sided {level} goodness-of-fit confidence interval for the intraclass (Fleiss-type) kappa of the common-correlation model, on the models of Donner and Eliasziw (two raters, binary outcome), Altaye, Donner and Klar (binary outcome, more raters) and Altaye, Donner and Eliasziw (three to five categories), in the confidence-interval approach of Rotondi and Donner implemented in the kappaSize package; for two raters with equal marginal frequencies this kappa coincides with Cohen's kappa."),
                    level = conf_pct),
                .("It answers 'given the subjects I have, how little agreement am I still unable to rule out?' - the mirror image of the sample-size question; every kappa below the bound is excluded."),
                if (reest && !identical(rml, "failed"))
                    .("The bound shown is the lower of two: the one obtained holding the category proportions at their anticipated values (the published calculation, and kappaSize's) and the one obtained re-estimating them by maximum likelihood at each value of kappa (the interval that keeps its stated coverage)."),
                # VAL-04: kappaSize's own search (Summary panel) is up to 0.00115 below the root - while it
                # stays inside the model; past the floor it can be any distance (-23.78 against -0.020).
                .("The bound is the lower root of the goodness-of-fit chi-square equation, solved directly; a positive bound is shown rounded down (to three decimals, or to two significant digits below 0.001), so the figure shown never overstates it."),
                if (isTRUE(show_summary) && isTRUE(engine_ok))
                    .("The kappaSize package's own search, reproduced in the Summary panel, moves down in steps of 0.001 and stops once the chi-square exceeds its critical value by 0.001, so it can report a value up to about 0.001 lower."),
                # IR-kappaSizeCI-02 carried over: a binary outcome uses ADK eq. (4); the 3-5 category cells are
                # the Dirichlet-multinomial, which for two categories is the beta-binomial (ADE 2001 p.2483).
                # Fixed-proportion beta-binomial bound vs the bound shown, 180 binary designs (3-6 raters,
                # p .05-.5, kappa0 .4/.6/.8, n 30/100/300, alpha .05; /review-function 2026-10-17): higher in
                # 171, equal in the 9 at p .5 with 3 raters (where the two cell sets coincide), never lower;
                # up to +0.103 (p .05, 6 raters, n 30), median +0.015.
                if (isTRUE(outcome == 2 && raters >= 3))
                    .("With three or more raters, the lower bound for a binary finding also depends on how agreement among more than two raters is modeled. This analysis uses the model of Altaye, Donner and Klar; the Dirichlet-multinomial model used for three to five categories, which for two categories is the beta-binomial, describes the same kappa, and with the proportions held at their anticipated values it never gave a lower bound than the one shown in the designs checked and usually gave a higher one (by up to about 0.10 when the finding is rare and the study small), so the figure shown is the more cautious of the two."),
                # FN-02 (independent review 2026-10-21): NOT a rare-category effect only, as the IR-01 wording
                # said. One-sided 95% plug-in coverage: 0.901-0.9375 with a category near 0.1 (R = 2000), 0.861-0.914
                # in the review's rare designs, and 0.878 with four BALANCED categories and four raters (n 80,
                # R = 1000); 0.939-0.942 with three balanced categories and three raters. At the true kappa the
                # plug-in statistic's mean is 1.37 (K3 R3), 1.50 (K4 R2), 1.88-1.94 (K4 R4) and 2.77 (K5 R6) with
                # every category balanced, against 1.0-1.1 re-estimated: the margins estimate the proportions,
                # so the excess grows with raters and categories and does not shrink with n.
                if (reest)
                    .("When the study is analyzed, estimate the category prevalences by maximum likelihood at each candidate value of kappa. With three or more raters or categories, plugging in the overall proportions of ratings instead can give an interval that covers kappa less often than stated (about 0.86 to 0.94 instead of 0.95 in the designs simulated for this analysis), generally more so with more raters or categories. Balanced categories do not prevent it: with four equally common categories and four raters the plug-in interval covered kappa about 0.88 of the time."))

            paste0(caveats, details)
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
        # The generated option classes check the List levels, the Integer type and the Number
        # ranges in init(), and jamovi's engine stops when init() fails (engine/enginer.cpp checks
        # `errored` before it calls run()), so the range clauses here are BACKSTOPS. They matter
        # for an R caller: jmvcore's run(noThrow = TRUE) still enters .run() after init() recorded
        # the failed check, and raters "7", "60" or "1000" then completed and reported a bound
        # (security audit 2026-10-17). Every option the calculation reads is checked here.
        .validateInputs = function(outcome, kappa0, parsed, alpha, n, raters) {
            props4 <- parsed$values

            if (!outcome %in% c(2, 3, 4, 5))
                jmvcore::reject(.("Number of outcome levels must be 2, 3, 4, or 5."), code = NULL)

            if (!isTRUE(raters %in% 2:6))
                jmvcore::reject(.("Number of raters must be 2, 3, 4, 5, or 6."), code = NULL)

            if (length(props4) == 0 || any(is.na(props4)))
                jmvcore::reject(
                    .("Proportions must be numbers separated by commas, semicolons or spaces (for example '0.20, 0.80'). One or more entries could not be read as a number. Note that a decimal comma is not recognized - use a point, as in 0.20."),
                    code = NULL)

            # "0,30 0,70" splits into 0, 30, 0, 70 and would be reported as the wrong COUNT of
            # proportions, which sends the user looking in the wrong place. Diagnose the decimal
            # separator first, as the two siblings do. Every token must parse
            # (R/utils-kappasize.R): dropping the unreadable ones told the typo "1.2, 0.8" to use
            # the decimal point it already used.
            if (any(props4 >= 1, na.rm = TRUE)) {
                if (kappaSizePropsLookDecimalComma(parsed$raw))
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

            # Backstop: every kappaSize FixedN* engine stops on n <= 10, an Inf used to hang its
            # search loop (the test statistic becomes NaN), and a fractional n was computed as given
            # but printed rounded (82.5: "82 subjects" beside the bound for 82.5).
            if (!isTRUE(is.finite(n) && n >= 11 && n == round(n)))
                jmvcore::reject(
                    .fmt(
                        .("Sample size (N) must be a whole number of at least 11. The kappaSize method is a large-sample approximation and its engine refuses any study of 10 or fewer subjects (received {n})."),
                        n = n),
                    code = NULL)

            # Backstops: kappa0 in (0, 1), the range the model needs (the compiled 0.01-0.99 is the
            # interface's); alpha in its compiled (0.001, 0.20).
            if (!is.finite(kappa0) || kappa0 <= 0 || kappa0 >= 1)
                jmvcore::reject(.("The anticipated kappa must be strictly between 0 and 1."), code = NULL)

            if (!is.finite(alpha) || alpha < 0.001 || alpha > 0.20)
                jmvcore::reject(
                    .("Significance level (alpha) must be between 0.001 and 0.20."), code = NULL)

            invisible(TRUE)
        },

        # Syntax pane: only this analysis's own arguments, string values escaped (see
        # kappaSizeSourcifyOption in R/utils-kappasize.R). jmvcore adds every option name a saved
        # file carries and pastes it into the call raw, so a name spelling
        # `outcome = "2", kappa0 = {<code>; 0.6}, alpha` exported a call that ran <code>.
        .sourcifyOption = function(option) {
            if (!option$name %in% names(formals(kappaSizeFixedN))) return("")
            src <- kappaSizeSourcifyOption(option)
            if (is.null(src)) super$.sourcifyOption(option) else src
        },

        .run = function() {


            outcome <- as.numeric(self$options$outcome)
            kappa0  <- self$options$kappa0
            raters  <- as.numeric(self$options$raters)
            alpha   <- self$options$alpha
            n       <- self$options$n
            # Study Explanation, method detail and the kappaSize printout are optional (F2, as
            # kappaSizeCI); nothing is built for a hidden pane, and the engine runs only for its own.
            show_details <- isTRUE(self$options$showExplanations)
            show_summary <- isTRUE(self$options$showSummary)

            parsed <- private$.parseProps(self$options$props)
            props4 <- parsed$values

            private$.validateInputs(outcome, kappa0, parsed, alpha, n, raters)

            # The proportions the calculation uses: a single binary prevalence expanded to its pair,
            # and every vector renormalised. kappaSize accepts a sum within 0.001 of 1 and uses it
            # verbatim, which can drive the lumped cell 1 - sum(agree) negative; this is a no-op for
            # an input that already sums to one (same rule as R/kappaSizeCI.b.R:.validateProportions).
            props_used <- if (outcome == 2) {
                p <- if (length(props4) == 1) props4 else props4[1] / sum(props4)
                c(p, 1 - p)
            } else props4 / sum(props4)

            crit <- stats::qchisq(1 - 2 * alpha, 1)    # one-sided: DE1992 p.1514, RD2012 p.780

            fixed <- private$.fixedBound(outcome, raters, props_used, kappa0, n, crit)
            if (!isTRUE(is.finite(fixed)))
                jmvcore::reject(
                    .("The expected lower bound could not be computed for this design: it lies too close to the lowest agreement the model allows for these proportions to be resolved numerically, as happens when a category is extremely rare. Use a less extreme prevalence, increase N, or raise the significance level."),
                    code = NULL)

            rml <- if (private$.reestimationApplies(outcome, raters))
                private$.rmlBound(outcome, raters, props_used, kappa0, n, crit, start = fixed)
            else list(status = "na")
            rml_governs <- identical(rml$status, "rml") && rml$bound < fixed
            bound <- if (rml_governs) rml$bound else fixed
            props_at_bound <- if (rml_governs) rml$props else props_used
            # Say so only when the two bounds differ at the precision shown ("would be 0.559 ...
            # gives 0.559" told the reader nothing).
            show_rml <- rml_governs && !identical(private$.fmtBound(fixed), private$.fmtBound(bound))

            # --- Populate outputs ---------------------------------------------
            # library-audit 2026-10-23 meddecide [MEDIUM] DONE: panels are cleared on the success path only; under
            #   reject() the greyed previous result is jamovi's failure presentation (guide section 5).
            self$results$text1$setContent("")
            self$results$text_summary$setContent("")
            self$results$text2$setContent("")
            self$results$notices$setContent("<div></div>")  # Html: the client skips ""
            self$results$methodology$setContent("<div></div>")
            n_txt <- private$.fmtN(n)
            # FN-01 (independent review 2026-10-21): when re-estimation lowers the bound, the default view must
            # say so - the cited method and kappaSize give the fixed figure (up to 0.029 higher), and on rare
            # designs that figure clears 0.40 while the headline, and so the amber verdict, does not.
            self$results$text1$setContent(paste0(.fmt(
                .("A total of {n} subjects can produce an expected one-sided {conf} lower limit for kappa of {bound}."),
                n = n_txt, conf = paste0(base::format(100 * (1 - alpha), scientific = FALSE, trim = TRUE), "%"),
                bound = private$.fmtBound(bound)),
                if (show_rml)
                    paste0("\n", private$.wrap(.fmt(
                        .("Holding the category proportions at their anticipated values, as the published calculation and kappaSize do, gives {fixed}; the figure shown re-estimates them at each value of kappa, as the analysis of the study should, and is the lower of the two."),
                        fixed = private$.fmtBound(fixed))))
                else ""))

            engine_sparse <- FALSE
            engine_ok <- FALSE
            if (show_summary) {
                # kappaSize's own search, for the Summary pane only: the reported bound no longer depends
                # on it. n goes in as an integer: the engine cat()s the value it was given, and a double
                # of 100000 prints as "1e+05".
                kappa_fn <- switch(
                    as.character(outcome),
                    "2" = kappaSize::FixedNBinary,
                    "3" = kappaSize::FixedN3Cats,
                    "4" = kappaSize::FixedN4Cats,
                    "5" = kappaSize::FixedN5Cats
                )
                # kappaSize walks down from kappa0 in steps of 0.001 with no floor and no cap, and for a
                # rare finding its statistic barely moves: prevalence 1e-4 (2 raters, kappa0 0.01,
                # n 11) takes about two million steps, and 1e-8 (some 1e10 steps, over a day) held
                # the analysis and a jamovi engine (security audit 2026-10-17). A normal walk takes
                # well under a second, so five seconds is ample; the time-out arrives as an error and
                # lands in the branch below. Same guard as R/kappaSizeCI.b.R.
                engine <- tryCatch({
                    setTimeLimit(elapsed = 5, transient = TRUE)
                    kappa_fn(kappa0 = kappa0, n = as.integer(n), props = props4, alpha = alpha,
                             raters = raters)
                }, error = function(e) e)
                setTimeLimit(elapsed = Inf, transient = TRUE)

                if (inherits(engine, "error")) {
                    summary_txt <- private$.wrap(.fmt(
                        .("kappaSize's own search could not be run for comparison: {error}"),
                        error = conditionMessage(engine)))
                } else {
                    eng_val <- suppressWarnings(as.numeric(engine$kappaL))
                    if (length(eng_val) == 1 && is.finite(eng_val)) engine$kappaL <- round(eng_val, 10)
                    # The engine prints its marginal cell-count warning once per rare category (five
                    # times for five levels); keep the first, the Notes panel carries the real check.
                    lines <- utils::capture.output(summary(engine))
                    lines <- lines[!(duplicated(lines) & grepl("expected cell count", lines, fixed = TRUE))]
                    engine_sparse <- any(grepl("expected cell count", lines, fixed = TRUE))
                    engine_ok <- length(eng_val) == 1 && is.finite(eng_val) &&
                        private$.inModel(outcome, raters, props_used, round(eng_val, 10))
                    note <- if (!engine_ok)
                        .fmt(.("kappaSize's own search, above, stepped past the lowest agreement the model allows for these proportions, so its figure is not a valid limit. The expected lower bound, solved directly, is {bound}."),
                             bound = private$.fmtBound(bound))
                    else if (show_rml)
                        .fmt(.("kappaSize's own search, above, reports {engine}, holding the category proportions at their anticipated values as the published tables do. The expected lower bound is {bound}: the lower limit when the proportions are re-estimated at each value of kappa, as the analysis of the study should do."),
                             engine = private$.fmtEngine(eng_val), bound = private$.fmtBound(bound))
                    else if (!identical(private$.fmtBound(round(eng_val, 10)), private$.fmtBound(bound)))
                        .fmt(.("kappaSize's own search, above, reports {engine}; the expected lower bound is {bound}. kappaSize moves down in steps of 0.001 and stops only once the chi-square exceeds its critical value by 0.001, so it can report a value slightly lower than the bound."),
                             engine = private$.fmtEngine(eng_val), bound = private$.fmtBound(bound))
                    else NULL
                    summary_txt <- paste(c(lines, if (!is.null(note)) c("", private$.wrap(note))),
                                         collapse = "\n")
                }
                self$results$text_summary$setContent(summary_txt)
            }

            if (show_details) {
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
                            kappa0 = kappa0, n = n_txt, raters = raters),
                        prev_txt)),
                    "\n",
                    private$.wrap(.fmt(
                        .("They would like to know how low the one-sided {conf}% lower confidence bound for kappa can be expected to fall - the smallest agreement the study would still be unable to rule out; every value below it is excluded."),
                        conf = base::format(100 * (1 - alpha), scientific = FALSE))),
                    "\n",
                    private$.wrap(.fmt(
                        .("The expected lower bound for kappa is {bound}."),
                        bound = private$.fmtBound(bound))),
                    if (show_rml)
                        paste0("\n", private$.wrap(.fmt(
                            .("Holding the category proportions at their anticipated values (the published calculation, and kappaSize's) the bound would be {fixed}; re-estimating them at each value of kappa, as the analysis of the study should, gives {bound}, and that is the figure reported."),
                            fixed = private$.fmtBound(fixed), bound = private$.fmtBound(bound))))
                    else "",
                    if (bound <= 0)
                        paste0("\n", private$.wrap(
                            .("A bound at or below zero means this many subjects cannot rule out agreement no better than chance.")))
                    else "")
                self$results$text2$setContent(text2)
            }

            # Cochran's rule on the expected counts of the test's cells at the reported bound, with
            # the proportions that bound was computed with (see .buildNotices).
            e <- n * private$.pooledCells(outcome, raters, props_at_bound, bound)
            sparse <- any(e < 1) || mean(e < 5) > 0.2
            liftable <- if (bound > 0 && bound < 0.40 && kappa0 > 0.40)
                private$.liftable(outcome, raters, props_used, kappa0) else NA
            warn <- private$.buildNotices(
                bound,
                sparse_cells  = sparse,
                outcome       = outcome,
                sparse_min    = min(e),
                sparse_below5 = sum(e < 5),
                sparse_total  = length(e),
                n             = n,
                kappa0        = kappa0,
                liftable      = liftable,
                min_prop      = min(props_used))
            # Never "" after a run: the jamovi client renders an Html element only when its content is
            # non-empty, so "" would leave the previous run's warnings on screen.
            self$results$notices$setContent(if (nzchar(warn)) warn else "<div></div>")
            self$results$methodology$setContent(private$.buildMethodology(
                outcome, raters, alpha, rml = rml$status, sparse_cells = sparse,
                engine_sparse = engine_sparse, show_details = show_details,
                show_summary = show_summary, engine_ok = engine_ok))
        })
)
