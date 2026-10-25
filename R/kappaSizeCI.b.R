#' @title Confidence Interval Approach for the Number of Subjects Required
#' @importFrom R6 R6Class
#' @import jmvcore
#'
#' @description Calculate sample size for interobserver agreement studies from the width of the
#' confidence interval around kappa. The estimand is the intraclass (Fleiss-type) kappa of the
#' common-correlation model of Donner and Eliasziw / Rotondi and Donner; for two raters with
#' equal marginal frequencies it coincides with Cohen's kappa. It supports 2-5 outcome
#' categories and 2-6 raters.
#'
#' @details The function uses the kappaSize package to calculate required sample sizes
#' for kappa coefficient confidence intervals. It supports 2-5 outcome categories
#' and 2-6 raters, with customizable precision requirements and significance levels.
#' Both two-sided and one-sided confidence intervals are supported.
#' @return An \code{R6} class generator object for the \code{kappaSizeCIClass} backend; used internally by the jamovi analysis wrapper and not called directly.

kappaSizeCIClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizeCIClass",
    inherit = kappaSizeCIBase,
    private = list(
        # Input validation methods
        .validateInputs = function() {
            errors <- c()

            is_one_sided <- (self$options$citype == "one_sided")

            # Only the RELATIONSHIPS between the kappa values are checked here. The bounds
            # themselves (kappa/alpha in range, outcome and raters in their option lists) are
            # enforced by OptionNumber/OptionList before .run() is ever entered, with better
            # messages than this method produced -- "alpha must be between 0.001 and 0.2 (is
            # 0.5)" rather than "alpha must be between 0 and 1".
            if (!is_one_sided) {
                if (self$options$kappaL >= self$options$kappaU) {
                    errors <- c(errors, .("kappaL must be less than kappaU"))
                }

                # kappaSize requires kappa0 to lie strictly inside the interval;
                # kappa0 == kappaL or kappa0 == kappaU errors inside the engine.
                if (self$options$kappa0 <= self$options$kappaL || self$options$kappa0 >= self$options$kappaU) {
                    errors <- c(errors, .("kappa0 must be strictly within the confidence interval (kappaL, kappaU)"))
                }
            } else {
                # One-sided: kappaSize requires kappa0 strictly greater than the lower limit.
                if (self$options$kappa0 <= self$options$kappaL) {
                    errors <- c(errors, .("kappa0 must be greater than kappaL"))
                }
            }

            # Validate proportions
            props_validation <- private$.validateProportions()
            if (!is.null(props_validation$error)) {
                errors <- c(errors, props_validation$error)
            }

            return(if (length(errors) > 0) errors else NULL)
        },

        .validateProportions = function() {
            tryCatch({
                # jamovi always sends one string, but the R wrapper can pass NULL, NA or a vector
                # (c(0.2, 0.8)), which reached the user as "argument is of length zero" / "the
                # condition has length > 1". The siblings' .parseProps() already take a vector.
                raw <- self$options$props
                raw <- if (length(raw) == 0 || all(is.na(raw))) "" else paste(raw, collapse = ", ")
                # U+00A0 and U+202F (no-break spaces: Word/Excel paste, and French typography puts
                # U+202F before ";") are not in [:space:]. Same class as kappaSizeFixedN's .parseProps.
                props_str <- trimws(gsub("[\u{00A0}\u{202F}]", " ", raw))
                if (props_str == "") {
                    return(list(error = .("Proportions cannot be empty")))
                }

                # Parse proportions with flexible delimiters. The old class "[,;|\\t]+" was the
                # SET {, ; | \ t}: it matched a literal backslash and the letter "t" but NOT an
                # actual tab, and not a space -- so "0.2, 0.3 0.5" was rejected while
                # "0.2, 0.3; 0.5" was accepted. [[:space:]] covers tab and space properly.
                # Same advice for "20%, 80%" here and "20, 80" below, both read as percentages.
                percent_msg <- .("Proportions must be fractions, not percentages: write 0.20, 0.80 rather than 20, 80.")
                if (grepl("%", props_str, fixed = TRUE)) {
                    return(list(error = percent_msg))
                }

                props_clean <- gsub("[,;|[:space:]]+", ",", props_str)
                props_split <- strsplit(props_clean, ",")[[1]]
                # A leading separator ("; 0.2 0.8") leaves an empty first token, which used to be
                # reported as "must be valid numbers".
                props_split <- props_split[nzchar(props_split)]
                # suppressWarnings: a non-numeric token is reported as "must be valid
                # numbers" below, so do not also surface a coercion warning.
                props_numeric <- suppressWarnings(as.numeric(trimws(props_split)))

                # A space-separated fallback used to sit here. It was dead: the gsub above
                # has already turned every whitespace run into a comma, so props_numeric can
                # only have length 1 when the (already trimws-ed) input contains no separator
                # at all -- and then grepl("\\s+", props_str) is FALSE. Removed rather than
                # left implying a parsing path this regex does not already handle.

                if (any(is.na(props_numeric))) {
                    return(list(error = .("All proportions must be valid numbers")))
                }

                if (any(props_numeric <= 0) || any(props_numeric >= 1)) {
                    # "0,20 0,80" splits into 0, 20, 0, 80 and lands here, telling the user the
                    # proportions are out of range when the real problem is the decimal
                    # separator. Re-read it with the comma as a decimal point; if that yields
                    # valid proportions, say so instead (kappaSizePropsLookDecimalComma, shared
                    # with the two siblings: every token must parse, so "1.2, 0.8" is not told to
                    # use the decimal point it already used).
                    if (kappaSizePropsLookDecimalComma(props_str)) {
                        return(list(error = .("Proportions must use a decimal point, not a decimal comma: write 0.20, 0.80 rather than 0,20 0,80.")))
                    }
                    if (all(props_numeric > 0) && abs(sum(props_numeric) - 100) <= 0.1) {
                        return(list(error = percent_msg))
                    }
                    return(list(error = .("All proportions must be between 0 and 1")))
                }

                expected_length <- as.numeric(self$options$outcome)
                # Binary models accept one prevalence value or two values summing to one
                if (expected_length == 2 && length(props_numeric) == 1) {
                    props_numeric <- c(props_numeric, 1 - props_numeric)
                }

                if (length(props_numeric) != expected_length) {
                    # One msgid with placeholders: the old paste0() spliced English word order
                    # into the message, which no catalog can reorder.
                    error_msg <- .fmt(
                        .("Enter exactly {k} proportions for {k} outcome categories (received {got})."),
                        k = expected_length, got = length(props_numeric))
                    return(list(error = error_msg))
                }

                prop_sum <- sum(props_numeric)
                # Match kappaSize's strict tolerance: proportions must sum to 1
                # within 0.001 (a looser 0.01 lets inputs pass here but reject in the engine).
                if (abs(prop_sum - 1) > 0.001) {
                    error_msg <- .fmt(.("Proportions should sum to 1.0, current sum is {sum}."),
                                      sum = round(prop_sum, 3))
                    return(list(error = error_msg))
                }

                # Renormalise. kappaSize accepts a sum within 0.001 of 1 and then uses the
                # values verbatim, so 0.9997/0.0002/0.0002 (sum 1.0001) drives the lumped
                # goodness-of-fit cell P0 = 1 - sum(agree) NEGATIVE and the engine returns
                # 215,974 where the valid design needs 93,636 -- a 2.3-fold error with no
                # warning. This is a no-op (bit-identical) for any input that already sums to
                # one, so it only touches the inputs that were wrong.
                props_numeric <- props_numeric / sum(props_numeric)

                return(list(props = props_numeric, error = NULL))

            }, error = function(e) {
                return(list(error = .fmt(.("Error parsing proportions: {error}"),
                                         error = conditionMessage(e))))
            })
        },

        # The goodness-of-fit cell probabilities now live in R/utils-kappasize.R
        # (kappaSizeGofCells) -- the identical closed form used to be copied into all
        # three kappaSize backends. See that file for the derivation and for which
        # agreement level each engine evaluates the cells at.

        # Cochran's rule applied to each confidence limit SEPARATELY, reporting the numbers
        # from the limit that is actually thinnest.
        #
        # An earlier version took the element-wise MINIMUM across kappaL and kappaU and counted
        # over that. P_j(rho) moves in opposite directions across cells -- for a binary outcome
        # the interior cells fall with rho while j = 0 and j = raters rise -- so the minimum was
        # assembled from BOTH limits, summed to anything but 1 (measured range 0.098 to 0.968),
        # and turned "k of m cells are below 5" into a union count that no single chi-square
        # ever has. Verified: outcome 2, kappa0 0.60, limits 0.30/0.80, props 0.05/0.95, 2
        # raters -> n = 181 and the notice claimed "2 of 3" where each limit alone has 1 of 3.
        #
        # A limit whose cells are not a valid probability vector is skipped rather than
        # reported. props inside kappaSize's own 0.001 sum tolerance can exceed 1 (for example
        # 0.99945, 0.0003, 0.0003 sums to 1.00005 and both the module validator and CI3Cats
        # accept it), which drives the lumped P0 = 1 - sum(agree) negative; the notice used to
        # print "the smallest expected count is -1".
        .sparseVerdict = function(params, required_n) {
            none <- list(sparse = FALSE, min = NA_real_,
                         below5 = NA_integer_, total = NA_integer_)
            if (!is.finite(required_n)) return(none)

            rhos <- c(params$kappaL, params$kappaU)
            rhos <- rhos[is.finite(rhos)]

            per <- list()
            for (rho in rhos) {
                e <- kappaSizeGofCells(params$outcome, params$raters, params$props, rho) *
                     required_n
                # Binary, 3+ raters: judge the cells of the test the n is for. That test pools every
                # split call into one disagreement cell (Altaye, Donner & Klar 2001 p.586), and under
                # their eq. (4) pooling leaves the chi-square, and so n, unchanged. Judging the
                # 'exactly k of n' cells flagged 19 of 40 designs whose pooled cells are comfortable --
                # n = 976 with pooled counts 882 / 70 / 24 read as "less dependable"
                # (VAL-kappaSizeCI-02, validation 2026-10-15). Same pooling as R/kappaSizePower.b.R.
                if (params$outcome == 2 && params$raters >= 3)
                    e <- c(e[1], sum(e[2:params$raters]), e[params$raters + 1])
                if (!all(is.finite(e)) || any(e < 0)) next   # degenerate here; not assessable
                per[[length(per) + 1L]] <- list(
                    sparse = any(e < 1) || mean(e < 5) > 0.2,
                    min    = min(e),
                    below5 = sum(e < 5),
                    total  = length(e))
            }
            if (length(per) == 0) return(none)

            # Flag if EITHER limit is sparse, but quote a single coherent limit: the sparsest
            # of the ones that fired, so the numbers shown always justify the warning shown.
            flagged <- Filter(function(x) isTRUE(x$sparse), per)
            pool    <- if (length(flagged) > 0) flagged else per
            worst   <- pool[[which.min(vapply(pool, function(x) x$min, numeric(1)))]]
            worst$sparse <- length(flagged) > 0
            worst
        },

        # Expected counts run from ~3e-06 to a few dozen, and signif() pasted straight into a
        # sentence renders the small end as "8.9e-06" in prose aimed at pathologists.
        # scientific = FALSE already prevents that, so the only job left for a floor is to stop
        # the tail growing unreadably long. It used to be 0.01, justified as beating a rounded
        # "0.000" -- but nothing here rounds to fixed decimals, so that justification never
        # applied, and 0.01 is far too high: the default six-rater binary design's smallest cell
        # is 0.013 (see .sparseVerdict below), i.e. right on the old floor, and anything under
        # it -- a pattern effectively never observed -- read as "below 0.01", a figure that also
        # covers a comfortable 0.009. Same floor and same wording as R/kappaSizePower.b.R and
        # R/kappaSizeFixedN.b.R, so the three analyses word the same caveat the same way.
        # signif(x, 2) alone printed 4.95-4.99 as "5" beside "the number of cells below 5 is 1 of 3"
        # (ten binary designs, e.g. 3 raters, p .24, 0.50 [0.30, 0.70]); same guard as kappaSizePower.
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

        # Display form of a category proportion. .validateProportions() renormalises the
        # vector (kappaSize accepts a sum within 0.001 of 1 and then uses it verbatim, which
        # can drive a goodness-of-fit cell negative), so a typed "0.333, 0.333, 0.333" becomes
        # 0.33333333333333331 and jmvcore::format renders a double at 15 significant digits --
        # the Study Explanation read "Expected category proportions of 0.333333333333333 ...".
        # Round for DISPLAY only; params$props keeps full precision for the engine.
        # Element-wise: base::format() on a VECTOR pads every element to a common number of
        # decimals, which would turn a typed 0.1 into "0.10" next to a 0.25.
        # Also every other non-count number in prose: signif() pasted straight in prints a round
        # 0.0001 as "1e-04" ("The limit ... is 1e-04 away from kappa0") and a slope below 1e-04,
        # i.e. any n above about 38,000, as "3.84146e-05" in the Study Explanation.
        # `digits` is passed to format() too: its default of 7 would undo a signif() to more.
        .fmtNum = function(x, digits = 4) {
            vapply(x, function(v) base::format(signif(v, digits), scientific = FALSE, trim = TRUE,
                                               digits = digits),
                   character(1))
        },

        # No memoisation here. Every option a result's content depends on is in its clearWith,
        # so jamovi only re-enters .run() when one of them has actually changed -- a
        # parameter-hash cache could never register a hit.
        .prepareParameters = function() {
            props_result <- private$.validateProportions()
            if (!is.null(props_result$error)) {
                jmvcore::reject(props_result$error)
            }

            is_one_sided <- (self$options$citype == "one_sided")

            params <- list(
                outcome = as.numeric(self$options$outcome),
                citype = self$options$citype,
                kappa0 = self$options$kappa0,
                kappaL = self$options$kappaL,
                kappaU = if (is_one_sided) NA else self$options$kappaU,
                props = props_result$props,
                raters = as.numeric(self$options$raters),
                alpha = self$options$alpha
            )
            # Computed once per run and carried with the parameters (the restricted-ML fit is an
            # optimisation, and five methods read it). params is rebuilt every run, so it cannot go
            # stale; a hand-built params without it is computed on demand.
            params$governing <- private$.governingSlopes(params)
            params
        },

        # kappaSize's stopping rule solved directly instead of searched. Every term of the
        # engine's chi-square is
        #   (n P_j(k0) - n P_j(rho))^2 / (n P_j(rho))  =  n (P_j(k0) - P_j(rho))^2 / P_j(rho),
        # so the statistic is EXACTLY LINEAR in n and the brute-force loop is doing a division.
        # Cross-checked against the engine over 10,080 designs (outcomes 2-5 x raters 2-6 x
        # several proportion vectors each x kappa0 0.30-0.90 x half-width 0.05-0.25 x alpha
        # 0.01-0.20 x one- and two-sided): 0 mismatches, 0 fallbacks. Used only to TRIAGE -- the
        # sample size this module reports still comes from kappaSize itself -- so a design
        # needing millions of subjects is turned away in microseconds WITH its actual number
        # instead of freezing the interface for 20 seconds. Anything it cannot judge returns NA
        # and goes to the engine, where setTimeLimit remains the backstop.
        # Chi-square slope per requested confidence limit, named by that limit. The engine's
        # statistic is n * slope, so the limit with the SMALLER slope is the one that binds --
        # which is NOT always the limit geometrically nearer kappa0. These are the slopes with the
        # proportions held fixed (kappaSize's, so .predictedN() uses them alone); the reported n uses
        # .governingSlopes(), which also allows for re-estimating the proportions.
        .limitSlopes = function(params) {
            at_kappa0 <- kappaSizeGofCells(params$outcome, params$raters, params$props,
                                           params$kappa0)
            valid <- function(x) all(is.finite(x)) && all(x > 0)
            if (!valid(at_kappa0)) return(NULL)

            rhos <- c(params$kappaL, params$kappaU)
            rhos <- rhos[is.finite(rhos)]
            if (length(rhos) == 0) return(NULL)

            out <- vapply(rhos, function(rho) {
                b <- kappaSizeGofCells(params$outcome, params$raters, params$props, rho)
                if (!valid(b)) return(NA_real_)
                # No na.rm: the engine drops NaN terms, but a NaN here means this estimate
                # cannot be trusted, and dropping it would understate the slope and so
                # overstate n.
                sum((at_kappa0 - b)^2 / b)
            }, numeric(1))
            stats::setNames(out, rhos)
        },

        # The same slope when the analysis RE-ESTIMATES the category proportions by maximum likelihood
        # at each candidate kappa, from the pooled goodness-of-fit cells (Altaye, Donner & Eliasziw
        # 2001 eq. (8)-(9); the binary cells pooled as in Altaye, Donner & Klar 2001 p.586). That is
        # the interval that keeps its stated coverage with three or more raters (simulated 0.938-0.951
        # against 0.909-0.932 for the plug-in), and the Notes recommend it. .limitSlopes() holds the
        # proportions at their anticipated values (Rotondi & Donner 2012 p.780, and kappaSize), which
        # on expected data is the plug-in interval's statistic. Re-estimating them gives up the
        # information they share with kappa, so the slope is smaller and the limits fall further out:
        # RD2012's own example (3 raters, .11/.70/.19, [.34, .54]) reaches [.3388, .5453] at its
        # n = 171 and needs 190 (independent review IR-kappaSizeCI-01, 2026-10-15).
        # NULL when any fit fails, so the caller can say the check was not made.
        .rmlSlopes = function(params) {
            K <- length(params$props)
            R <- params$raters
            pooled <- function(p, rho) {
                e <- kappaSizeGofCells(params$outcome, R, p, rho)
                if (params$outcome == 2 && R >= 3) c(e[1], sum(e[2:R]), e[R + 1]) else e
            }
            in_model <- function(p, rho) {
                cells <- kappaSizeGofCells(params$outcome, R, p, rho)
                if (!all(is.finite(cells)) || any(cells < 0)) return(FALSE)
                if (params$outcome == 2) return(TRUE)
                i <- seq_len(R) - 1
                all((1 - rho) + i * rho > 0) &&
                    all(outer(p, i, function(pj, k) pj * (1 - rho) + k * rho) >= 0)
            }
            q <- pooled(params$props, params$kappa0)
            if (!all(is.finite(q)) || any(q <= 0)) return(NULL)

            rhos <- c(params$kappaL, params$kappaU)
            rhos <- rhos[is.finite(rhos)]
            if (length(rhos) == 0) return(NULL)

            out <- vapply(rhos, function(rho) {
                t <- if (rho >= 0) 0
                     else if (params$outcome == 2) (-rho / (1 - rho))^(1 / (R - 1))
                     else -(R - 1) * rho / (1 - rho)
                if (K * t >= 1 || 1 + (R - 2) * rho <= 0) return(NA_real_)
                to_props <- function(th) { e <- exp(c(th, 0) - max(c(th, 0))); t + (1 - K * t) * e / sum(e) }
                nll <- function(th) {
                    b <- pooled(to_props(th), rho)
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
                w <- (params$props - t) / (1 - K * t)
                starts <- c(if (all(w > 0)) list(log(w[-K] / w[K])), list(rep(0, K - 1)))
                fit <- NULL
                for (st in starts) {
                    f <- fit_from(st)
                    if (!is.null(f) && (is.null(fit) || f$objective < fit$objective)) fit <- f
                }
                if (is.null(fit)) return(NA_real_)
                p <- to_props(fit$par)
                if (!in_model(p, rho)) return(NA_real_)
                b <- pooled(p, rho)
                if (!all(is.finite(b)) || any(b <= 0)) return(NA_real_)
                sum((q - b)^2 / b)
            }, numeric(1))
            if (!all(is.finite(out))) return(NULL)
            stats::setNames(out, rhos)
        },

        # The slope that governs n at each limit. It is the smaller of the two analyses', so the
        # reported n reaches the requested limits whether the study holds the proportions fixed
        # (the published calculation) or re-estimates them (the interval the Notes recommend,
        # because the plug-in one under-covers: 0.909-0.916 binary with 4 raters, 0.912-0.913 for
        # RD2012's 3-rater example, 0.928-0.932 for it with 2 raters; Altaye, Donner & Eliasziw 2001
        # p.2483 specify re-estimation for 3-5 categories with any number of raters). The one
        # exception is a binary outcome with two raters: Donner & Eliasziw's published interval
        # plugs in the observed proportion and keeps close to its stated coverage (1992 Table II;
        # 0.945 simulated in the validation), so the fixed-proportion n stands there --
        # re-estimating would raise it (1,624 -> 1,669 for p .2, [.55, .65]) for an analysis no one
        # is told to run. `fixed` is kept for the Study Explanation and the Summary pane, which
        # compare against kappaSize. `rml` is NULL when not applicable or when the restricted fit
        # failed; `rml_failed` tells the two apart so the Notes can say so.
        .governingSlopes = function(params) {
            if (!is.null(params$governing)) return(params$governing)
            fixed <- private$.limitSlopes(params)
            if (is.null(fixed) || !all(is.finite(fixed))) return(NULL)
            applies <- private$.reestimationApplies(params$outcome, params$raters)
            rml <- if (applies) private$.rmlSlopes(params) else NULL
            if (!is.null(rml) && length(rml) != length(fixed)) rml <- NULL
            list(fixed = fixed, rml = rml, rml_failed = applies && is.null(rml),
                 eff = stats::setNames(if (is.null(rml)) fixed else pmin(fixed, rml), names(fixed)))
        },

        # Every design except a binary outcome rated by two raters (see .governingSlopes): the
        # sizing rule and the Notes' advice share this one condition so they cannot drift apart.
        .reestimationApplies = function(outcome, raters) {
            !(isTRUE(outcome == 2) && isTRUE(raters == 2))
        },

        # The confidence limit that actually governs the sample size, and its distance from
        # kappa0. The Study Explanation used to name whichever limit was geometrically nearer;
        # measured over 4,000 random two-sided designs that is the wrong limit about 18% of the
        # time. Worked example: kappa0 = 0.75 with [0.60, 0.85] names the upper limit (0.10
        # away), yet moving kappaU to 0.95 leaves n at 167 while moving kappaL to 0.70 takes it
        # to 1,212 -- the limit being named has no influence at all.
        .bindingLimit = function(params) {
            slopes <- private$.governingSlopes(params)$eff
            # ALL slopes, not any: with one limit degenerate there is nothing to compare the
            # survivor against, so naming it as "the limit that drives the sample size" states
            # as a finding what was never established. .predictedN() already refuses the same
            # case (returns NA); fall back to the geometric-distance wording instead.
            if (is.null(slopes) || !all(is.finite(slopes))) return(NULL)
            rho <- as.numeric(names(slopes)[which.min(slopes)])
            list(rho = rho, distance = abs(params$kappa0 - rho))
        },

        # Two-sided limits use qchisq(1 - alpha, 1); a one-sided lower bound qchisq(1 - 2 alpha, 1)
        # (Rotondi & Donner 2012 p.780: 3.84 and 2.71 at alpha = 0.05).
        .critValue = function(params) {
            if (isTRUE(is.finite(params$kappaU)))
                stats::qchisq(1 - params$alpha, 1)
            else
                stats::qchisq(1 - 2 * params$alpha, 1)
        },

        # The sample size this analysis REPORTS: the smallest n whose expected chi-square reaches
        # the critical value at every requested limit (Rotondi & Donner 2012 p.780). The statistic
        # is n * slope (see .limitSlopes), so that n is ceiling(crit / min(slope)) -- a division.
        # kappaSize's own search never returns fewer than 11 and can return more: it starts at
        # n = 10, increments before testing, and stops only once the statistic exceeds the critical
        # value by 0.001 (the margin Rotondi & Donner's printed tables carry too). That margin adds
        # up to ceiling(0.001 / slope) = about n x 0.001 / crit subjects: 0.026% of n two-sided at
        # alpha 0.05, 0.14% at most (alpha 0.20 one-sided) -- one or two subjects in the hundreds to
        # low thousands, 25 at n = 93,611. Validation 2026-10-15 (VAL-kappaSizeCI-01/03): over
        # RD2012's 265 published cells kappaSize was 1 above this minimum in 23.
        # NA when a cell vector is degenerate (the caller then falls back to the engine), Inf when
        # kappa0 sits on a limit. The slope is the governing one (.governingSlopes): the larger n of
        # the fixed-proportion calculation and the restricted-ML one. which = "fixed" gives the
        # published, fixed-proportion figure alone, for the explanation beside it.
        .minimumN = function(params, which = c("eff", "fixed")) {
            slopes <- private$.governingSlopes(params)[[match.arg(which)]]
            if (is.null(slopes) || !all(is.finite(slopes))) return(NA_real_)
            worst <- min(slopes)
            if (worst <= 0) return(Inf)
            ceiling(private$.critValue(params) / worst)
        },

        # kappaSize's own stopping rule, which is what its loop costs (time budget below). The
        # number REPORTED is .minimumN().
        .predictedN = function(params) {
            chi_crit <- private$.critValue(params)

            per_limit <- private$.limitSlopes(params)
            # NULL or non-finite means a degenerate or invalid cell vector, which this cannot
            # judge -- hand it to the engine and let the wall-clock backstop cover it. That is
            # reachable: props inside kappaSize's own 0.001 sum tolerance used to make the
            # lumped P0 = 1 - sum(agree) negative (now renormalised in .validateProportions).
            if (is.null(per_limit) || !all(is.finite(per_limit))) return(NA_real_)
            worst <- min(per_limit)
            # A zero slope means kappa0 sits exactly on a limit, so no n ever separates them.
            # That is Inf, not "unknown": returning NA here would send an unanswerable design to
            # the engine to hang for the full 20 seconds. .validateInputs already blocks it.
            if (worst <= 0) return(Inf)

            # The engine starts at n = 10 and its loop condition carries an abs(x - 0.001)
            # offset, so the first n it accepts is max(11, ceiling((crit + 0.001) / slope)).
            max(11, ceiling((chi_crit + 0.001) / worst))
        },

        # run_engine = FALSE applies the ceiling refusal below and returns NULL without running
        # kappaSize: its result only feeds the Summary pane (see .run).
        .calculateSampleSize = function(params, run_engine = TRUE) {
            if (!requireNamespace('kappaSize', quietly = TRUE)) {
                jmvcore::reject(.('The kappaSize package is required but not installed. Please install it using install.packages("kappaSize")'))
            }

            kappa_function <- switch(
                as.character(params$outcome),
                "2" = kappaSize::CIBinary,
                "3" = kappaSize::CI3Cats,
                "4" = kappaSize::CI4Cats,
                "5" = kappaSize::CI5Cats,
                stop(.("Unsupported number of outcome categories"))
            )

            # kappaSize searches for n by brute force -- `n <- 10; while (...) n <- n + 1` in
            # interpreted R, with no cap, at 10 to 100 microseconds an iteration depending on the
            # outcome count and rater count (measured below). The required n
            # grows as one over the square of the distance from kappa0 to the binding limit, and
            # every value that makes that distance tiny is typable in the interface, so the
            # search can run for minutes with no way to abort it from jamovi.
            #
            # .predictedN() is that same stopping rule in closed form, so an unsizeable design is
            # refused in microseconds AND told what it would actually cost. "About 3,762,498
            # subjects" tells a clinician to widen the interval; "still running after 20 seconds"
            # does not.
            #
            # The ceiling is set from the SLOWEST engine, not the fastest. Cost per iteration
            # ranges from 9.7 microseconds (binary, 2 raters) to 99 microseconds (5 categories,
            # 6 raters -- measured steady at n = 5,652 / 15,701 / 27,912), an 11-fold spread, so
            # a ceiling calibrated on the binary engine would let the five-category one run four
            # times longer than intended. At 1e-4 s an iteration, 100,000 subjects is about ten
            # seconds of engine time, which the twenty-second budget below covers twice over.
            # Anything above that is a study no one is running: this module already calls
            # n > 1,000 impractical.
            # kappaSize's binary engine keeps only props[1] and evaluates its cell
            # probabilities as polynomials in that value. Those cancel differently for p and
            # 1 - p, so the ORDER the user typed decides whether it converges: props
            # 0.9999/0.0001 with 5 raters ran for 18 seconds and was then refused, while
            # 0.0001/0.9999 -- the same study, the same kappa -- returned n = 88,673 in 1.9 s.
            # Agreement is symmetric under relabelling the two categories (the pattern
            # distribution simply reverses, j -> raters - j), so passing the smaller proportion
            # first cannot change the answer: verified identical n across 35 designs
            # (p 0.6-0.999 x raters 2-6), 0 differences.
            engine_props <- params$props
            if (params$outcome == 2)
                engine_props <- c(min(params$props), max(params$props))

            engine_n_limit <- 100000
            predicted <- private$.predictedN(params)
            # Judged and quoted on the reported n; `predicted` (the engine's own stopping point,
            # with the proportions fixed) only sizes the time budget for the Summary pane's run.
            minimum <- private$.minimumN(params)
            # The limit that actually governs n, not the one that merely looks closest.
            binding <- private$.bindingLimit(params)
            half_width <- if (!is.null(binding))
                binding$distance
            else if (isTRUE(is.finite(params$kappaU)))
                min(params$kappa0 - params$kappaL, params$kappaU - params$kappa0)
            else
                params$kappa0 - params$kappaL

            if (isTRUE(minimum > engine_n_limit)) {
                # Two COMPLETE alternative sentences rather than one sentence with an English
                # fragment spliced in: a catalog cannot reorder or inflect a spliced clause,
                # and "it would need about N subjects" is not a phrase every language builds
                # the same way.
                msg <- if (isTRUE(is.finite(minimum)))
                    .fmt(
                        .("The requested confidence interval is too narrow to size in reasonable time: it would need about {n} subjects. The limit that governs the sample size is {dist} away from kappa0, and the required sample size grows roughly as one over the square of that distance. Widen the interval, use more raters, or accept a lower confidence level. As a guide, halving that distance multiplies the required sample size by about four."),
                        n = private$.fmtN(minimum), dist = private$.fmtNum(half_width, 3))
                else
                    .fmt(
                        .("The requested confidence interval is too narrow to size in reasonable time: no sample size can deliver it. The limit that governs the sample size is {dist} away from kappa0, and the required sample size grows roughly as one over the square of that distance. Widen the interval, use more raters, or accept a lower confidence level. As a guide, halving that distance multiplies the required sample size by about four."),
                        dist = private$.fmtNum(half_width, 3))
                jmvcore::reject(msg, code = NULL)
            }

            if (!run_engine) return(NULL)

            # Wall-clock backstop, sized to what the engine should cost rather than a flat 20
            # seconds. kappaSize evaluates its cell probabilities as expanded polynomials, and at an
            # extreme prevalence with several raters those can cancel to exactly 0 while the true
            # value is merely tiny; it then divides by that zero, maps the Inf to 0, and its
            # while-loop never exits. Historically kappa0 = 0.70 on [0.60, 0.80] with props
            # 0.999/0.001 and six raters never returned. Since the binary props are passed
            # smaller-first (above) that design sizes in 0.8 s (n = 38,375), and a 2026-10-16
            # search of 166 extreme designs (p down to 1e-4, 3-6 raters, 2-5 categories) found no
            # engine failure: each either converged or was refused by the closed-form ceiling first.
            # No current trigger is known. The guard stays because no magnitude threshold predicts
            # the cancellation (props 0.9998 with six raters has a far smaller cell, 7.7e-19 against
            # 2.4e-15, and sized without complaint), so the clock is the only reliable detector.
            #
            # Budget twice the worst-case cost (1e-4 s an iteration, the five-category
            # six-rater engine) rather than a flat twenty seconds, with a five-second floor so a
            # small design still gets a fair run. Paired with the ceiling above this is 2x margin
            # everywhere by construction -- at predicted = engine_n_limit the budget is the full
            # twenty seconds against about ten seconds of real work -- so a legitimate run is
            # never cut short, while a design that can never converge is caught in five seconds
            # instead of twenty.
            time_budget <- if (isTRUE(is.finite(predicted)))
                min(20, max(5, ceiling(predicted * 1e-4 * 2)))
            else
                20

            # Wall-clock, not message text: R TRANSLATES "reached elapsed time limit", so a
            # grepl() for it silently fails under any non-English locale and the user gets the
            # raw translated error instead of the guidance below (verified: LANGUAGE=fr gives
            # "la limite de temps est atteinte"). This module ships a Turkish catalog.
            # jmvcore cannot interrupt inside the vendor's interpreted while-loop, but this
            # lets jamovi flush and mark the analysis as running before up to `time_budget`
            # seconds of uninterruptible engine time. Outside the tryCatch below on purpose:
            # .checkpoint() signals its restart as an ERROR and must not be caught here.
            private$.checkpoint()

            t0 <- Sys.time()
            result <- tryCatch({
                setTimeLimit(elapsed = time_budget, transient = TRUE)
                on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)
                kappa_function(
                    kappa0 = params$kappa0,
                    kappaL = params$kappaL,
                    kappaU = params$kappaU,
                    props = engine_props,
                    alpha = params$alpha,
                    raters = params$raters
                )
            }, error = function(e) {
                msg <- conditionMessage(e)
                timed_out <- as.numeric(difftime(Sys.time(), t0, units = "secs")) >=
                             time_budget * 0.95
                # A finite `minimum` is always <= engine_n_limit here: anything above it was refused
                # before the engine ran. So a time-out on a finite minimum means the design is
                # sizeable and the engine's arithmetic is what failed -- the fix is FEWER raters or a
                # less extreme prevalence, and "use more raters" would push further into the failure.
                if (timed_out && isTRUE(is.finite(minimum))) {
                    # \u2014, not \u{2014}: R resolves both at parse time, but the
                    # catalog stores the msgid verbatim, so a brace form inside .() can
                    # never be looked up and the string stays untranslated forever.
                    jmvcore::reject(
                        .fmt(
                            .("This combination of expected proportions and rater count is too extreme for the kappaSize engine: with {raters} raters and a rarest category of {rarest}, some agreement patterns are so unlikely that the calculation loses them to rounding and never converges. About {n} subjects would be needed. Use fewer raters, or a less extreme expected prevalence \u2014 the same design usually computes with one rater fewer."),
                            raters = params$raters,
                            rarest = private$.fmtNum(min(params$props), 3),
                            n = private$.fmtN(minimum)),
                        code = NULL)
                }
                # Otherwise no diagnosis has been established (a NA minimum means degenerate cells,
                # which the closed form cannot judge), so report the engine's own error. A "too narrow
                # ... still running after N seconds; use more raters" message used to sit here, and the
                # NA case was the only one that ever reached it.
                jmvcore::reject(.("Error in sample size calculation: {}"), code = NULL, msg)
            })

            setTimeLimit(elapsed = Inf, transient = TRUE)
            return(result)
        },

        .generateExplanation = function(params) {
            # Only the VALUE list is built here; the words around it ("Expected category
            # proportions of ...") are one translatable sentence further down. The old form
            # pasted the English noun phrase in front of the list, and "and" is a word too, so
            # the joiner is its own msgid, matching R/kappaSizeFixedN.b.R:.formatProps. (The old
            # form also emitted a stray space before the final comma: "0.2, 0.3 , and 0.5".)
            props_text <- if (params$outcome == 2) {
                .fmt(.("{a} and {b}"), a = private$.fmtNum(params$props[1]),
                     b = private$.fmtNum(params$props[2]))
            } else {
                k <- length(params$props)
                .fmt(.("{head} and {last}"),
                     head = paste(private$.fmtNum(params$props[-k]), collapse = ", "),
                     last = private$.fmtNum(params$props[k]))
            }

            is_one_sided <- (params$citype == "one_sided")

            # Escapes inside a .() msgid are written \u03ba-style (four hex digits, no
            # braces): R resolves both forms at parse time, but the catalog stores the msgid
            # verbatim, so a brace form could never be looked up and would stay untranslated.
            if (is_one_sided) {
                ci_text <- .fmt(.("\u2022 Lower confidence limit (\u03baL): {kappaL}"),
                                kappaL = params$kappaL)
                ci_type_text <- .("One-sided (lower bound only)")
                # Not "ensuring": the limit is an expectation, reached only if the study observes
                # kappa0 (IR-kappaSizeCI-03; the same overclaim was removed from text1).
                objective_text <- .fmt(
                    .("Determine the required sample size to estimate \u03ba\u2080 = {kappa0} so that the expected lower confidence limit reaches {kappaL} in an interobserver agreement study."),
                    kappa0 = params$kappa0, kappaL = params$kappaL)
            } else {
                # Name the limit that GOVERNS n, not the one geometrically nearer kappa0.
                # kappaSize stops when the chi-square clears the critical value at BOTH limits,
                # so the binding limit is the one with the smaller slope. Over 4,000 random
                # two-sided designs the two disagree about 18% of the time, and when they do the
                # geometrically-nearer limit has no influence on n whatsoever.
                binding <- private$.bindingLimit(params)
                near_txt <- if (!is.null(binding))
                    .fmt(
                        .("\u2022 The limit that drives the sample size is {rho}, which is {dist} from \u03ba\u2080 (it is this distance, not the full interval width, that sets n)"),
                        rho = binding$rho, dist = round(binding$distance, 3))
                else
                    .fmt(
                        .("\u2022 Distance from \u03ba\u2080 to the nearer limit: {dist} (this is what drives the sample size, not the full width)"),
                        dist = round(min(params$kappa0 - params$kappaL,
                                         params$kappaU - params$kappa0), 3))
                # The bracketed interval is passed in as a value: a literal " [" inside a .()
                # msgid is read as a trailing msgctxt marker by jmvcore's translator, which
                # strips it and everything after it from the displayed text.
                limits_txt <- paste0("[", params$kappaL, ", ", params$kappaU, "]")
                ci_text <- paste0(
                    .fmt(.("\u2022 Confidence interval: {limits}"), limits = limits_txt),
                    "\n", near_txt
                )
                ci_type_text <- .("Two-sided")
                objective_text <- .fmt(
                    .("Determine the required sample size to estimate \u03ba\u2080 = {kappa0} with confidence limits {limits} in an interobserver agreement study."),
                    kappa0 = params$kappa0, limits = limits_txt)
            }

            # The arithmetic behind the reported n (.minimumN), so a reader can reproduce it: the
            # expected chi-square is n x slope, and n is the critical value over the smallest slope,
            # rounded up. Six significant digits, more when the quotient sits so close to a whole
            # number that the rounded figures would ceil to a different n (2,327.996 displayed at six
            # digits reads 2,328.002, i.e. 2,329, for 5 raters, p .1, 0.4 [0.3, 0.45]).
            gov    <- private$.governingSlopes(params)
            slopes <- gov$eff
            n_min  <- private$.minimumN(params)
            if (!is.null(slopes) && all(is.finite(slopes)) && isTRUE(is.finite(n_min))) {
                crit <- private$.critValue(params)
                dg <- 6
                while (dg < 15 && ceiling(signif(crit, dg) / signif(min(slopes), dg)) != n_min)
                    dg <- dg + 1
                ci_text <- paste0(ci_text, "\n", .fmt(
                    .("\u2022 Required n = \u2308{crit} / {slope}\u2309 = {n}: the chi-square critical value divided by the expected chi-square that each subject contributes at the limit that drives the sample size, rounded up"),
                    crit = private$.fmtNum(crit, dg), slope = private$.fmtNum(min(slopes), dg),
                    n = private$.fmtN(n_min)))
            }
            # When re-estimating the proportions is what sets n, show the published figure beside it
            # so the difference from kappaSize and the RD2012 tables is explained where it appears.
            n_fixed <- private$.minimumN(params, "fixed")
            if (isTRUE(is.finite(n_min) && is.finite(n_fixed) && n_min > n_fixed))
                ci_text <- paste0(ci_text, "\n", .fmt(
                    .("\u2022 Holding the category proportions at their anticipated values (the published calculation, and kappaSize's) would need {fixed}; re-estimating them at each value of \u03ba, as the analysis of the study should, needs {n}, and that is the figure reported"),
                    fixed = private$.fmtN(n_fixed), n = private$.fmtN(n_min)))

            explanation <- paste0(
                .("Sample Size Calculation for Interobserver Agreement Study"), "\n\n",
                # One paragraph, one msgid. The hard line breaks stay inside it because this
                # pane is preformatted and does not wrap; splitting it at them would hand a
                # translator five sentence fragments instead of a paragraph.
                # "requested width" was wrong: n is set by the limit that binds, and moving the
                # other one leaves n unchanged (VAL-kappaSizeCI-04; RD2012 p.782, limits need not be
                # symmetric). "will not agree" overstated a general tendency as a rule.
                .("This is a CONFIDENCE-INTERVAL calculation: it returns the number of subjects\nneeded for the expected confidence limits around kappa to reach the requested\nlimits. It answers a different question from the power approach\n(kappaSizePower), which sizes a study to reject a null value, so the two\ngenerally give different sample sizes for the same study - choose the one that\nmatches how the result will be reported."),
                "\n\n",
                .("Study Design:"), "\n",
                .fmt(.("\u2022 Number of outcome categories: {k}"), k = params$outcome), "\n",
                .fmt(.("\u2022 Number of raters: {raters}"), raters = params$raters), "\n",
                .fmt(.("\u2022 Confidence level: {pct}% (\u03b1 = {alpha})"),
                     pct = base::format(100 * (1 - params$alpha), scientific = FALSE,
                                        trim = TRUE),
                     alpha = params$alpha), "\n",
                .fmt(.("\u2022 CI type: {type}"), type = ci_type_text), "\n\n",
                .("Kappa Parameters:"), "\n",
                .fmt(.("\u2022 Anticipated kappa (\u03ba\u2080): {kappa0}"),
                     kappa0 = params$kappa0), "\n",
                ci_text, "\n\n",
                .("Population Characteristics:"), "\n",
                .fmt(.("\u2022 Expected category proportions of {props}"), props = props_text),
                "\n\n",
                .("Objective:"), "\n",
                objective_text
            )

            return(explanation)
        },

        # `required_n` is the reported minimum (.minimumN), not kappaSize's own n. The design comes
        # from params: the engine only echoed kappa0/kappaL/kappaU/raters, and it now runs only for
        # the Summary pane.
        .formatSampleSizeOutput = function(params, required_n) {
            if (is.na(required_n)) {
                return(.("Required sample size: unavailable"))
            }

            sentence <- private$.buildExampleSentence(
                required_n = required_n,
                kappa0 = params$kappa0,
                kappaL = params$kappaL,
                kappaU = params$kappaU,
                one_sided = isTRUE(params$citype == "one_sided"),
                raters = params$raters
            )

            # F-02 (independent review 2026-10-21): a two-sided n is set by ONE limit, so "within [0.4, 0.8]"
            # alone reads as if both were planned for. Name it. The other limit has the larger slope, so at
            # this n its expected chi-square clears the critical value too: it comes in at or inside its request.
            binding <- if (!isTRUE(params$citype == "one_sided") && isTRUE(is.finite(params$kappaU)))
                private$.bindingLimit(params)
            governs <- if (is.null(binding)) NULL
                else if (abs(binding$rho - params$kappaL) < abs(binding$rho - params$kappaU))
                    .fmt(.("The lower limit ({lower}) drives the sample size; the expected upper limit then falls at or inside {upper}."),
                         lower = params$kappaL, upper = params$kappaU)
                else
                    .fmt(.("The upper limit ({upper}) drives the sample size; the expected lower limit then falls at or inside {lower}."),
                         lower = params$kappaL, upper = params$kappaU)

            return(paste(c(.fmt(.("Required sample size: {n}"), n = private$.fmtN(required_n)),
                           sentence, governs), collapse = "\n"))
        },

        .extractRequiredN = function(result) {
            if (is.null(result) || length(result) == 0) {
                return(NA_real_)
            }

            if (is.list(result)) {
                if ("n" %in% names(result)) {
                    return(as.numeric(ceiling(result$n)))
                }
                # NB: kept numeric so the NA sentinels below stay typed; every RENDER goes
                # through .fmtN(), because a double of 100000 pastes as "1e+05".
                # Defensive fallback: kappaSize uses $n, not $N.
                if ("N" %in% names(result)) {
                    return(as.numeric(ceiling(result$N)))
                }
            }

            # Defensive fallback: kappaSize returns a classed list, not a bare numeric.
            if (is.numeric(result) && length(result) == 1) {
                return(as.numeric(ceiling(result)))
            }

            return(NA_real_)
        },

        # A round sample size pastes as "1e+05" and a large one is unreadable without
        # separators. Every place n reaches the user goes through here.
        #
        # round(), not as.integer(): this used to see only engine output, which the wall-clock
        # guard capped in the hundreds of thousands. .predictedN() reports what an unsizeable
        # design would actually cost, and that runs past the 2,147,483,647 integer ceiling --
        # kappaL = 0.5999999 needs 3.8e13 subjects, where as.integer() returned NA and the
        # refusal read "it would need about NA subjects" with a coercion warning attached.
        .fmtN = function(n) {
            if (!isTRUE(is.finite(n))) return("NA")
            base::format(round(n), scientific = FALSE, big.mark = ",", trim = TRUE)
        },

        .buildExampleSentence = function(required_n, kappa0, kappaL, kappaU, one_sided = FALSE,
                                         raters = NA) {
            if (is.na(required_n)) {
                return(.("The required sample size could not be determined from the provided inputs."))
            }
            n_txt <- private$.fmtN(required_n)

            if (is.na(kappa0) || is.na(kappaL)) {
                return(.fmt(.("At least {n} subjects are needed for the requested confidence interval precision."),
                            n = n_txt))
            }

            # "ensure" was an overclaim: the calculation carries no assurance probability. The
            # limits are those obtained IF the observed kappa lands exactly on kappa0, and about
            # half of such studies observe less. Inverting the engine's own chi-square at the
            # default design (n = 118, kappa0 = 0.60, target [0.40, 0.80]) gives realised limits
            # of [0.400, 0.749] at khat = 0.60 but [0.348, 0.708] at khat = 0.55 -- only 0.54 SD
            # below kappa0, and already short of the promised 0.40 floor.
            # Four COMPLETE sentences instead of one sentence with " rated by N raters"
            # spliced into the middle of it: a spliced English clause cannot be reordered or
            # inflected by a catalog, and the bracketed interval is passed in as a value
            # because a literal " [" inside a msgid is taken for a trailing msgctxt marker.
            has_raters <- !(is.null(raters) || is.na(raters))

            if (one_sided || is.na(kappaU)) {
                if (has_raters)
                    return(.fmt(
                        .("At least {n} subjects rated by {raters} raters are needed for the lower confidence limit for \u03ba\u2080 = {kappa0} to reach {kappaL}, if the observed kappa comes in at {kappa0}."),
                        n = n_txt, raters = raters, kappa0 = kappa0, kappaL = kappaL))
                return(.fmt(
                    .("At least {n} subjects are needed for the lower confidence limit for \u03ba\u2080 = {kappa0} to reach {kappaL}, if the observed kappa comes in at {kappa0}."),
                    n = n_txt, kappa0 = kappa0, kappaL = kappaL))
            }

            limits_txt <- paste0("[", kappaL, ", ", kappaU, "]")
            if (has_raters)
                return(.fmt(
                    .("At least {n} subjects rated by {raters} raters are needed for the confidence limits to fall within {limits}, if the observed kappa comes in at {kappa0}."),
                    n = n_txt, raters = raters, limits = limits_txt, kappa0 = kappa0))
            return(.fmt(
                .("At least {n} subjects are needed for the confidence limits to fall within {limits}, if the observed kappa comes in at {kappa0}."),
                n = n_txt, limits = limits_txt, kappa0 = kappa0))
        },

        # Build methodology (INFO) and large-sample (WARNING) notices as HTML.
        # Rendered via a dedicated Html output rather than jmvcore::Notice objects because
        # Notice content renders as escaped plain text and accepts no newlines, and these
        # panels are multi-line. NOTE (corrected 2026-09-23): Notice serializes fine -- the
        # "attempt to apply non-function" crash once blamed on it was Group$insert() having no
        # bounds check. Use $add(), never insert(999, ...).
        .buildNotices = function(required_n, sparse_cells = FALSE,
                                 sparse_min = NA_real_, sparse_below5 = NA_integer_,
                                 sparse_total = NA_integer_,
                                 alpha = NA_real_, one_sided = FALSE,
                                 outcome = NA_integer_, raters = NA_integer_,
                                 rml_checked = TRUE, props = NULL,
                                 show_details = FALSE, show_summary = FALSE) {
            # The confidence level was previously only ever shown symbolically as
            # "100(1 - alpha)%", so a clinician never saw the number. Both citype values deliver
            # 100(1 - alpha)%: two-sided uses qchisq(1 - alpha, 1) = z(1 - alpha/2)^2 and
            # one-sided uses qchisq(1 - 2 alpha, 1) = z(1 - alpha)^2.
            conf_pct <- if (isTRUE(is.finite(alpha)))
                base::format(100 * (1 - alpha), scientific = FALSE, trim = TRUE)
            else
                .("100(1 - alpha)")
            ci_side_text <- if (isTRUE(one_sided)) .("one-sided lower bound") else .("two-sided")
            # The percent sign travels in the value: "{pct}% c..." inside a msgid reads as a C
            # format directive to gettext tooling.
            conf_lvl <- paste0(conf_pct, "%")

            # HTML tags stay OUTSIDE the msgids: a translator should never have to carry markup,
            # and "Donner and Eliasziw" is spelled out rather than "&amp;" so the msgid has no
            # entity in it (the sibling kappaSizeFixedN words it the same way).
            #
            # Validation 2026-10-15 (development-ideas/kappaSizeCI-validation-2026-10-15.md):
            # - The models: DE 1992 covers two raters only; the binary multi-rater cells are Altaye,
            #   Donner & Klar 2001 eq. (4) and the 3-5 category cells Altaye, Donner & Eliasziw 2001,
            #   both now in refs (VAL-07). "width approach" is gone: n is set by the binding limit,
            #   not the width (VAL-04).
            # - Under the model every PAIR of raters has Cohen's kappa equal to the intraclass kappa
            #   (marginalising the 3- and 6-rater cells gives 0.6 for 0.6); what breaks the
            #   equivalence is rater bias, not the number of raters (VAL-05).
            # - Coverage of the interval the study will report, simulated at the planned n (R = 2000):
            #   with the overall rating proportions plugged in, 0.929 (4 raters, binary) and 0.915
            #   (3 raters, 3 categories, RD2012's own example); with the prevalences re-estimated by
            #   restricted maximum likelihood at each kappa, 0.958 and 0.950. Two raters: 0.945.
            info_div <- "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #3c8dbc; background-color: rgba(72, 138, 188, 0.06); color: inherit;'>"

            # Always shown: what the number does and does not promise, and which kappa it is about.
            # The method detail below is ~300 more words, so it waits for showExplanations
            # (/review-function 2026-10-16: the Notes ran to ~480 words on every run).
            caveats <- paste0(
                info_div,
                # The engine's own Summary pane says n subjects "ensure" the limits are met.
                # There is no assurance probability anywhere in this design: the limits are the
                # ones obtained IF the observed kappa lands exactly on kappa0, and P(khat < kappa0)
                # is about 0.5. Inverting the engine's chi-square at the default design
                # (n = 118, kappa0 = 0.60, target [0.40, 0.80]) gives [0.400, 0.749] at
                # khat = 0.60 but [0.348, 0.708] at khat = 0.55 -- only 0.54 SD below kappa0 and
                # already short of the 0.40 floor.
                # library-audit 2026-10-23 meddecide [LOW] DONE: the severity is a word in the title, not the border colour alone.
                "<b>", .("Note"), ": ", .("This is a planning expectation, not a guarantee."), "</b> ",
                .("The sample size delivers the stated limits only if the study observes the anticipated kappa. Roughly half of such studies observe less and finish with a lower limit short of the target, so plan on a conservative anticipated kappa or enrol above the figure shown."), " ",
                # Agreement software rarely computes the goodness-of-fit interval: ClinicoPath's own
                # agreement analysis gives Cohen's kappa a Wald or Fisher-z interval and pooled Fleiss'
                # kappa none (R/agreement.b.R, 2026-10-16). At the default design a simulated Wald
                # interval at n = 118 averaged [0.412, 0.776] against the planned [0.40, 0.80]
                # (R = 4000, coverage 0.941): close for two raters, not the same interval.
                .("This sample size is planned for the model-based goodness-of-fit interval; it does not establish the requested precision for the Wald, Fisher-z or bootstrap intervals that agreement software usually reports for Cohen's or Fleiss' \u03ba. Report the goodness-of-fit interval, or size the study for the interval that will be reported."), " ",
                if (!isTRUE(rml_checked))
                    paste0(.("For this design the proportions could not be re-estimated, so the sample size holds them at their anticipated values only."), " "),
                .("For any pair of raters this \u03ba equals Cohen's \u03ba when the raters share the same category frequencies (no rater bias); when their frequencies differ, it does not."),
                # Beside the pane it qualifies, so only when that pane is shown.
                if (isTRUE(show_summary))
                    paste0(" ", .("(The Summary panel below reproduces the kappaSize package's own wording, which says the sample size will \u201censure\u201d the limits \u2014 read it with this caveat in mind.)")),
                "</div>")

            info <- if (isTRUE(show_details)) paste0(
                info_div,
                "<b>", .("Methodology."), "</b> ",
                .("The required sample size follows the confidence-interval approach of Rotondi and Donner, on the goodness-of-fit models of Donner and Eliasziw (two raters, binary outcome), Altaye, Donner and Klar (binary outcome, more raters) and Altaye, Donner and Eliasziw (three to five categories), as implemented in the kappaSize package."), " ",
                # IR-kappaSizeCI-01: the n now reaches the limits under BOTH ways of analysing the
                # study, so the sentence says so -- but only when the restricted fit ran; otherwise the
                # caveats above say the proportions were held fixed, and claiming both would contradict it.
                if (private$.reestimationApplies(outcome, raters) && isTRUE(rml_checked))
                    .fmt(
                        .("It is the smallest number of subjects for which the expected limits of the {level} confidence interval ({side}) for the intraclass (Fleiss-type) \u03ba of the common-correlation model reach the requested limits, given the expected category proportions and the number of raters, whether the analysis holds those proportions at their anticipated values (the published calculation, and kappaSize's) or re-estimates them by maximum likelihood at each value of \u03ba (the interval that keeps its stated coverage)."),
                        level = conf_lvl, side = ci_side_text)
                else
                    .fmt(
                        .("It is the smallest number of subjects for which the expected limits of the {level} confidence interval ({side}) for the intraclass (Fleiss-type) \u03ba of the common-correlation model reach the requested limits, given the expected category proportions and the number of raters."),
                        level = conf_lvl, side = ci_side_text),
                " ",
                .("Because the expected goodness-of-fit chi-square grows in proportion to the number of subjects, that number is computed directly: the chi-square critical value divided by the chi-square each subject contributes at the limit that drives the sample size, rounded up (the Study Explanation shows the numbers)."), " ",
                # IR-kappaSizeCI-02: a binary outcome uses ADK eq. (4); the 3-5 category engine is the
                # Dirichlet-multinomial, which for two categories is the beta-binomial (ADE 2001
                # p.2483). Same pairwise kappa, different higher-order agreement, different n: over
                # 120 binary designs (3-6 raters, p .05-.5, kappa0 .4-.8, half-width .1/.2) the
                # beta-binomial needed 0.55 to 1.04 times as many subjects, fewer in 119.
                if (isTRUE(outcome == 2 && raters >= 3))
                    paste0(.("With three or more raters, the sample size for a binary finding also depends on how agreement among more than two raters is modeled. This analysis uses the model of Altaye, Donner and Klar; the Dirichlet-multinomial model used for three to five categories, which for two categories is the beta-binomial, describes the same \u03ba but usually needs fewer subjects (in the designs checked, as few as about half when the finding is rare), so the figure shown is generally the more cautious of the two."), " "),
                # IR-kappaSizeCI-05: "0.92 to 0.93" was narrower than the simulations (0.909, 0.912
                # in the independent review; 0.929, 0.915 in the validation).
                if (private$.reestimationApplies(outcome, raters))
                    paste0(.("When the study is analyzed, estimate the category prevalences by maximum likelihood at each candidate value of \u03ba: with three or more raters, or three or more categories, plugging in the overall proportions of ratings gives an interval that covers \u03ba less often than stated (about 0.91 to 0.93 instead of 0.95 in the designs simulated for this analysis)."), " "),
                "</div>")
            else ""

            warn <- ""

            # Sparse goodness-of-fit cells, judged by Cochran's rule on the cells the engine
            # actually divides by (see kappaSizeGofCells / .sparseVerdict): no expected count below 1
            # and at most one cell in five below 5. This replaces a grep for kappaSize's own
            # "expected cell count is less than five" line, which tests the outcome MARGINALS
            # and for a binary outcome only props[1]. That rule missed 7 of 10 realistic
            # designs measured here -- the default six-rater binary study produced no warning at
            # all. For a binary outcome with 3+ raters the cells judged are the POOLED ones of the
            # published test (all negative / any disagreement / all positive); that six-rater
            # default has n = 42 and a smallest pooled count of 3.4, 1 of 3 below 5, so it still
            # warns -- on the cells that matter (VAL-kappaSizeCI-02).
            if (isTRUE(sparse_cells)) {
                # A binary outcome has nothing to collapse -- offering it as a remedy sends
                # the reader looking for an option that cannot exist. Two complete sentences,
                # not one sentence with a clause spliced in: English word order is not a
                # property a catalog can preserve across languages.
                remedy <- if (isTRUE(outcome > 2))
                    .("Consider collapsing rare categories, using fewer raters, or recruiting more subjects than the figure shown.")
                else
                    .("Consider using fewer raters, or recruiting more subjects than the figure shown.")
                warn <- paste0(warn,
                    "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #ec971f; background-color: rgba(227, 144, 33, 0.07); color: inherit;'>",
                    "<b>", .("Warning"), ": ", .("Sparse categories."), "</b> ",
                    .fmt(
                        # "{below} of {total} cells are below 5" printed "1 of 4 cells are below 5":
                        # worded so the count needs no singular/plural agreement.
                        .("At the computed sample size the goodness-of-fit cells (all raters agreeing on one category, or the single pooled cell for any disagreement) are too sparse at the confidence limit where they are thinnest: the smallest expected count is {min}, and the number of cells below 5 is {below} of {total}. The calculation rests on a large-sample chi-square approximation, so the required n is less dependable here."),
                        min = private$.fmtCount(sparse_min),
                        below = sparse_below5, total = sparse_total), " ",
                    remedy,
                    "</div>"
                )
            }

            # Extreme prevalence (a category below 5%, a warning heuristic rather than a statistical
            # boundary). Nothing else fires here: p = 0.02 with two raters needs n = 855 with healthy
            # cells (7.0 / 20 / 828 at kappaL), so the panels said only "Methodology" while each rater
            # is expected to call about 17 subjects positive. Kappa's dependence on prevalence (Byrt,
            # Bishop & Carlin 1993) is what a reader of that study must be told.
            min_prop <- if (length(props) > 0) min(props) else NA_real_
            if (isTRUE(min_prop < 0.05) && isTRUE(is.finite(required_n))) {
                warn <- paste0(warn,
                    "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #ec971f; background-color: rgba(227, 144, 33, 0.07); color: inherit;'>",
                    "<b>", .("Warning"), ": ", .("Rare category."), "</b> ",
                    .fmt(
                        .("The rarest category is expected in {share} of ratings, so each rater is expected to place only about {k} of the {n} subjects in it. Kappa depends strongly on prevalence (Byrt, Bishop and Carlin): when a category is this rare, high raw agreement can coexist with a low kappa, the estimate rests on those few subjects, and a kappa measured here cannot be assumed to generalize to a setting where the category is more common. Report the prevalence and the category-specific agreement (the proportion of that category's ratings that another rater matched) beside kappa."),
                        share = paste0(private$.fmtNum(100 * min_prop, 3), "%"),
                        k = private$.fmtNum(required_n * min_prop, 2),
                        n = private$.fmtN(required_n)),
                    "</div>")
            }

            # DE 1992 p.1518 and RD2012 p.782 document the goodness-of-fit interval's coverage from
            # about 25 subjects; a smaller n (reachable with a wide interval, and silent under the
            # sparse rule -- e.g. n = 17 with cells 5.1 / 6.8 / 5.1) is outside that range
            # (VAL-kappaSizeCI-06). Simulated at n = 17 the one-sided bound was conservative (0.987).
            # IR-kappaSizeCI-04: the sources qualify that figure -- binary outcomes only (RD2012
            # p.782), "provided pi >= 0.2" (ADK 2001 p.587), not at pi = 0.1 with kappa >= 0.9
            # (DE 1992 p.1515) -- and for 3-5 categories point to the expected cell counts instead.
            if (!is.na(required_n) && required_n < 25) {
                warn <- paste0(warn,
                    "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #ec971f; background-color: rgba(227, 144, 33, 0.07); color: inherit;'>",
                    "<b>", .("Warning"), ": ", .("Small sample."), "</b> ",
                    .("The required sample size is below 25. For a binary outcome, the goodness-of-fit interval this calculation assumes was found close to its stated coverage from about 25 subjects when the rarer category makes up at least about a fifth of the ratings (Donner and Eliasziw; Altaye, Donner and Klar); for three to five categories no such figure was given, and the expected cell counts are the guide (Rotondi and Donner). Below 25 subjects it has not been shown to hold, so consider enrolling at least 25 subjects."),
                    "</div>")
            }

            # NB: paste0(warn, ...), not paste0(...). Writing over `warn` here silently deleted
            # the sparse-cell block whenever both conditions held.
            if (!is.na(required_n) && required_n > 1000) {
                # Adding raters lowers n, but it is also what makes the agreement-pattern
                # cells sparse. Recommending it directly under a Sparse-categories box that
                # has just said "use fewer raters" left the two panels contradicting each
                # other with no way to tell which applied. Two complete sentences rather than
                # one with a trailing clause glued on, so each is translatable on its own.
                advice <- if (isTRUE(sparse_cells))
                    .("Consider a wider confidence interval (lower precision) or revisiting the expected category proportions. More raters would lower this figure, but that is what makes the cells sparse above \u2014 the two cannot both be improved by the rater count, so change the interval or the proportions instead.")
                else
                    .("Consider a wider confidence interval (lower precision), revisiting the expected category proportions, or increasing the number of raters.")
                warn <- paste0(warn,
                    "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #d9534f; background-color: rgba(222, 55, 55, 0.06); color: inherit;'>",
                    "<b>", .("Important warning"), ": ", .("Very large sample size."), "</b> ",
                    .fmt(
                        .("The computed sample size ({n}) is very large and may be impractical for a typical interobserver-agreement study."),
                        n = private$.fmtN(required_n)), " ",
                    advice,
                    "</div>"
                )
            }

            # The answer, between the warnings and the Methodology: this Html is the first result item,
            # so the n used to sit below up to ~410 words of method text, in text1. Same msgid as
            # R/kappaSizePower.b.R (whose Turkish already exists), hence the <b> inside it.
            answer <- if (isTRUE(is.finite(required_n)) && isTRUE(is.finite(raters)))
                paste0(info_div,
                       .fmt(if (isTRUE(raters == 2)) .("Required sample size: <b>{n}</b> {subjects}, each rated by both raters.")
                            else .("Required sample size: <b>{n}</b> {subjects}, each rated by all {raters} raters."),
                            n = private$.fmtN(required_n), subjects = private$.subjects(required_n),
                            raters = raters),
                       "</div>")
            else ""

            return(paste0(warn, answer, caveats, info))
        },

        .subjects = function(n) if (isTRUE(n == 1)) .("subject") else .("subjects"),

        # Syntax pane: props escaped (see kappaSizeSourcifyOption in R/utils-kappasize.R).
        .sourcifyOption = function(option) {
            if (!option$name %in% names(formals(kappaSizeCI))) return("")
            src <- kappaSizeSourcifyOption(option)
            if (is.null(src)) super$.sourcifyOption(option) else src
        },

        .run = function() {
            # Input validation
            validation_errors <- private$.validateInputs()
            if (!is.null(validation_errors)) {
                error_msg <- .fmt(.("Input validation failed: {errors}"),
                                  errors = paste(validation_errors, collapse = "; "))
                jmvcore::reject(error_msg, code='validation_failed')
            }

            # Prepare parameters and run the calculation OUTSIDE the display tryCatch.
            # These call jmvcore::reject() on failure (missing kappaSize package,
            # invalid proportions, or an engine calculation error); letting those
            # conditions propagate makes jamovi render them as real error notices
            # instead of swallowing them into the result body text.
            params <- private$.prepareParameters()

            # text2 and text_summary are visible only when their checkbox is on (r.yaml), and nothing
            # is built for or written to a hidden pane.
            show_details <- isTRUE(self$options$showExplanations)
            show_summary <- isTRUE(self$options$showSummary)

            # The reported n is the closed-form minimum (.minimumN); kappaSize's own n is kept for
            # the Summary pane, which is the package's own output. Its brute-force search ran on
            # every run and was discarded unless that pane was on: 10 s for n = 90,407 (5 categories,
            # 6 raters, [0.5975, 0.6025]). So it runs for that pane, or to stand in when the closed
            # form cannot judge the design (a degenerate cell vector). The ceiling refusal applies
            # either way, so the checkbox never decides whether a design is refused.
            required_n <- private$.minimumN(params)
            raw_result <- private$.calculateSampleSize(
                params, run_engine = show_summary || !isTRUE(is.finite(required_n)))
            engine_n   <- private$.extractRequiredN(raw_result)
            if (!isTRUE(is.finite(required_n))) required_n <- engine_n

            # Build outputs defensively; only unexpected formatting/summary errors
            # are caught here (the reject-throwing steps already ran above).
            tryCatch({
                # Format the result
                formatted_result <- private$.formatSampleSizeOutput(params, required_n)

                if (show_summary) {
                    # Capture summary output from kappaSize
                    # The engine prints its marginal cell-count warning once per rare category (five
                    # times for five levels) and, in one-sided mode, echoes the deliberate
                    # `kappaU = NA` sentinel as a bare "KappaU: NA" that reads to a clinician like a
                    # failed calculation. Both siblings already strip the repeats; this file never
                    # got the fix.
                    summary_lines <- utils::capture.output(summary(raw_result))
                    summary_lines <- summary_lines[
                        !(duplicated(summary_lines) &
                          grepl("expected cell count", summary_lines, fixed = TRUE))]
                    # Assigned by index, not through sub(): sub() parses its REPLACEMENT for
                    # backslash escapes (\1 is a backreference), and the replacement here is a
                    # translated string, so a catalog entry containing a backslash would error or
                    # silently mangle the line. The PATTERN stays the untranslated engine output.
                    if (isTRUE(params$citype == "one_sided")) {
                        ku <- grepl("^KappaU:\\s*NA\\s*$", summary_lines)
                        if (any(ku))
                            summary_lines[ku] <- .("KappaU: not applicable (one-sided interval)")
                    }

                    # kappaSize's binary engine keeps only props[1] and summary() prints THAT as
                    # "Event Proportion:". .calculateSampleSize() hands it the SORTED pair (the
                    # order decides whether the vendor polynomials converge), so a user who typed
                    # a prevalence above 0.5 read the complement of their own design assumption
                    # here while the Study Explanation pane showed it correctly -- two panels of
                    # one analysis stating opposite prevalences. n is identical either way
                    # (agreement is symmetric under relabelling the two categories), so only the
                    # printed line is wrong. Restore the entered value.
                    if (params$outcome == 2) {
                        ep <- grepl("^Event Proportion:", summary_lines)
                        if (any(ep))
                            summary_lines[ep] <- .fmt(.("Event Proportion: {p}"),
                                                      p = private$.fmtNum(params$props[1]))
                    }
                    # The pane prints kappaSize's own "A minimum of N subjects", which can differ from
                    # the reported n. Say why beside the number, rather than leave two panes stating two
                    # sample sizes. Two reasons, checked in this order: re-estimating the proportions
                    # needs more subjects than kappaSize plans for (IR-kappaSizeCI-01), or kappaSize's
                    # search floor and stopping margin put it slightly above the minimum.
                    n_fixed <- private$.minimumN(params, "fixed")
                    if (isTRUE(is.finite(engine_n) && is.finite(n_fixed) && required_n > n_fixed))
                        summary_lines <- c(summary_lines, "", .fmt(
                            .("kappaSize's own search, above, reports {engine} subjects, holding the\ncategory proportions at their anticipated values as the published tables do.\nThe required sample size is {n}: it also reaches the requested limits when\nthe proportions are re-estimated at each value of kappa, as the analysis of\nthe study should do."),
                            engine = private$.fmtN(engine_n), n = private$.fmtN(required_n)))
                    else if (isTRUE(is.finite(engine_n) && engine_n != required_n))
                        summary_lines <- c(summary_lines, "", .fmt(
                            .("kappaSize's own search, above, reports {engine} subjects; the required\nsample size is {n}. kappaSize starts its search at 11 subjects and stops only\nonce the chi-square exceeds its critical value by 0.001, so it can report\nslightly more subjects than the smallest number that reaches the requested limits."),
                            engine = private$.fmtN(engine_n), n = private$.fmtN(required_n)))
                    summary_text <- paste(summary_lines, collapse = "\n")
                }

                # Methodology / large-sample notices. Sparseness is judged on the
                # agreement-pattern cells at the requested confidence limits (Cochran's rule),
                # not on kappaSize's own marginal check -- see kappaSizeGofCells. The engine's own
                # marginal line is left untouched in the Summary pane.
                verdict <- private$.sparseVerdict(params, required_n)
                notices_html <- private$.buildNotices(
                    required_n,
                    alpha         = params$alpha,
                    one_sided     = isTRUE(params$citype == "one_sided"),
                    outcome       = params$outcome,
                    raters        = params$raters,
                    rml_checked   = !isTRUE(params$governing$rml_failed),
                    sparse_cells  = verdict$sparse,
                    sparse_min    = verdict$min,
                    sparse_below5 = verdict$below5,
                    sparse_total  = verdict$total,
                    props         = params$props,
                    show_details  = show_details,
                    show_summary  = show_summary)

                # library-audit 2026-10-23 meddecide [MEDIUM] DONE: panels are cleared on the success path only; under
                #   reject() the greyed previous result is jamovi's failure presentation (guide section 5).
                # Every panel is written, so a panel switched off cannot keep an earlier run's text.
                self$results$text1$setContent(formatted_result)
                self$results$text_summary$setContent(if (show_summary) summary_text else "")
                self$results$text2$setContent(if (show_details) private$.generateExplanation(params) else "")
                self$results$notices$setContent(notices_html)

            }, error = function(e) {
                # This handler used to write the error into text1 and return NORMALLY, so jamovi
                # showed no error notice and the analysis looked like it had succeeded. Its advice
                # was wrong at that point too: validation has passed and the engine has already
                # returned, so kappaSize IS installed and the parameters ARE valid - the fault is
                # downstream formatting. Neither sibling has such a handler; reject so the failure
                # surfaces as a real jamovi error instead of as body text.
                jmvcore::reject(
                    .fmt(.("The sample size was computed but could not be formatted for display: {error}"),
                         error = conditionMessage(e)),
                    code = NULL)
            })
        }
    )
)
