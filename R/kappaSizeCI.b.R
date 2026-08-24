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
                    errors <- c(errors, "kappaL must be less than kappaU")
                }

                # kappaSize requires kappa0 to lie strictly inside the interval;
                # kappa0 == kappaL or kappa0 == kappaU errors inside the engine.
                if (self$options$kappa0 <= self$options$kappaL || self$options$kappa0 >= self$options$kappaU) {
                    errors <- c(errors, "kappa0 must be strictly within the confidence interval (kappaL, kappaU)")
                }
            } else {
                # One-sided: kappaSize requires kappa0 strictly greater than the lower limit.
                if (self$options$kappa0 <= self$options$kappaL) {
                    errors <- c(errors, "kappa0 must be greater than kappaL")
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
                # U+00A0 (non-breaking space, what Word/Excel paste) is not in [:space:].
                props_str <- trimws(gsub("\u{00A0}", " ", self$options$props, fixed = TRUE))
                if (props_str == "") {
                    return(list(error = "Proportions cannot be empty"))
                }

                # Parse proportions with flexible delimiters. The old class "[,;|\\t]+" was the
                # SET {, ; | \ t}: it matched a literal backslash and the letter "t" but NOT an
                # actual tab, and not a space -- so "0.2, 0.3 0.5" was rejected while
                # "0.2, 0.3; 0.5" was accepted. [[:space:]] covers tab and space properly.
                props_clean <- gsub("[,;|[:space:]]+", ",", props_str)
                props_split <- strsplit(props_clean, ",")[[1]]
                # suppressWarnings: the space-separated fallback below is the intended
                # path when this comma parse yields NAs, so do not surface a coercion warning.
                props_numeric <- suppressWarnings(as.numeric(trimws(props_split)))

                # Handle space-separated format
                if (length(props_numeric) == 1 && grepl("\\s+", props_str)) {
                    props_split <- trimws(strsplit(props_str, "\\s+")[[1]])
                    props_numeric <- suppressWarnings(as.numeric(props_split))
                }

                if (any(is.na(props_numeric))) {
                    return(list(error = "All proportions must be valid numbers"))
                }

                if (any(props_numeric <= 0) || any(props_numeric >= 1)) {
                    # "0,20 0,80" splits into 0, 20, 0, 80 and lands here, telling the user the
                    # proportions are out of range when the real problem is the decimal
                    # separator. Re-read it with the comma as a decimal point; if that yields
                    # valid proportions, say so instead.
                    as_decimal <- suppressWarnings(as.numeric(trimws(unlist(strsplit(
                        gsub("([0-9]),([0-9])", "\\1.\\2", props_str), "[;|[:space:]]+")))))
                    as_decimal <- as_decimal[!is.na(as_decimal)]
                    if (length(as_decimal) > 0 && all(as_decimal > 0 & as_decimal < 1)) {
                        return(list(error = paste0(
                            "Proportions must use a decimal point, not a decimal comma: write ",
                            "0.20, 0.80 rather than 0,20 0,80")))
                    }
                    return(list(error = "All proportions must be between 0 and 1"))
                }

                expected_length <- as.numeric(self$options$outcome)
                # Binary models accept one prevalence value or two values summing to one
                if (expected_length == 2 && length(props_numeric) == 1) {
                    props_numeric <- c(props_numeric, 1 - props_numeric)
                }

                if (length(props_numeric) != expected_length) {
                    error_msg <- paste0("Expected ", expected_length, " proportions for ", expected_length, " outcome categories, got ", length(props_numeric))
                    return(list(error = error_msg))
                }

                prop_sum <- sum(props_numeric)
                # Match kappaSize's strict tolerance: proportions must sum to 1
                # within 0.001 (a looser 0.01 lets inputs pass here but reject in the engine).
                if (abs(prop_sum - 1) > 0.001) {
                    error_msg <- paste0("Proportions should sum to 1.0, current sum is ", round(prop_sum, 3))
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
                return(list(error = paste("Error parsing proportions:", e$message)))
            })
        },

        # Expected probability of every goodness-of-fit cell at agreement rho. kappaSize's
        # CI* engines grow n until the chi-square sum over AGREEMENT PATTERNS --
        # (n P_j(kappa0) - n P_j(rho))^2 / (n P_j(rho)) -- exceeds the critical value at
        # rho = kappaL and (two-sided) rho = kappaU. The expected counts in those denominators
        # are where sparseness matters, NOT the outcome marginals that kappaSize's own
        # print/summary check. Identical closed forms to R/kappaSizeFixedN.b.R:.gofCells and
        # R/kappaSizePower.b.R:.gofCells, verified against every engine .CalcIT for raters 2-6.
        .gofCells = function(outcome, raters, props, rho) {
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
        },

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
                e <- private$.gofCells(params$outcome, params$raters, params$props, rho) *
                     required_n
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
        # sentence renders the small end as "8.9e-06" in prose aimed at pathologists. Rounding
        # to fixed decimals instead would print "0.000", which is worse; say "below 0.01".
        .fmtCount = function(x) {
            if (!isTRUE(is.finite(x))) return("unavailable")
            if (x < 0.01) return("below 0.01")
            base::format(signif(x, 2), scientific = FALSE, trim = TRUE)
        },

        # No memoisation here. Every option this analysis has is listed in each result's
        # clearWith, so jamovi only re-enters .run() when one of them has actually changed --
        # a parameter-hash cache could never register a hit.
        .prepareParameters = function() {
            props_result <- private$.validateProportions()
            if (!is.null(props_result$error)) {
                jmvcore::reject(props_result$error)
            }

            is_one_sided <- (self$options$citype == "one_sided")

            list(
                outcome = as.numeric(self$options$outcome),
                citype = self$options$citype,
                kappa0 = self$options$kappa0,
                kappaL = self$options$kappaL,
                kappaU = if (is_one_sided) NA else self$options$kappaU,
                props = props_result$props,
                raters = as.numeric(self$options$raters),
                alpha = self$options$alpha
            )
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
        # which is NOT always the limit geometrically nearer kappa0. Both .predictedN() and the
        # Study Explanation need this, so it lives in one place.
        .limitSlopes = function(params) {
            at_kappa0 <- private$.gofCells(params$outcome, params$raters, params$props,
                                           params$kappa0)
            valid <- function(x) all(is.finite(x)) && all(x > 0)
            if (!valid(at_kappa0)) return(NULL)

            rhos <- c(params$kappaL, params$kappaU)
            rhos <- rhos[is.finite(rhos)]
            if (length(rhos) == 0) return(NULL)

            out <- vapply(rhos, function(rho) {
                b <- private$.gofCells(params$outcome, params$raters, params$props, rho)
                if (!valid(b)) return(NA_real_)
                # No na.rm: the engine drops NaN terms, but a NaN here means this estimate
                # cannot be trusted, and dropping it would understate the slope and so
                # overstate n.
                sum((at_kappa0 - b)^2 / b)
            }, numeric(1))
            stats::setNames(out, rhos)
        },

        # The confidence limit that actually governs the sample size, and its distance from
        # kappa0. The Study Explanation used to name whichever limit was geometrically nearer;
        # measured over 4,000 random two-sided designs that is the wrong limit about 18% of the
        # time. Worked example: kappa0 = 0.75 with [0.60, 0.85] names the upper limit (0.10
        # away), yet moving kappaU to 0.95 leaves n at 167 while moving kappaL to 0.70 takes it
        # to 1,212 -- the limit being named has no influence at all.
        .bindingLimit = function(params) {
            slopes <- private$.limitSlopes(params)
            if (is.null(slopes) || !any(is.finite(slopes))) return(NULL)
            slopes <- slopes[is.finite(slopes)]
            rho <- as.numeric(names(slopes)[which.min(slopes)])
            list(rho = rho, distance = abs(params$kappa0 - rho))
        },

        .predictedN = function(params) {
            chi_crit <- if (isTRUE(is.finite(params$kappaU)))
                stats::qchisq(1 - params$alpha, 1)
            else
                stats::qchisq(1 - 2 * params$alpha, 1)

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

        .calculateSampleSize = function(params) {
            if (!requireNamespace('kappaSize', quietly = TRUE)) {
                jmvcore::reject('The kappaSize package is required but not installed. Please install it using install.packages("kappaSize")')
            }

            kappa_function <- switch(
                as.character(params$outcome),
                "2" = kappaSize::CIBinary,
                "3" = kappaSize::CI3Cats,
                "4" = kappaSize::CI4Cats,
                "5" = kappaSize::CI5Cats,
                stop("Unsupported number of outcome categories")
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
            # The limit that actually governs n, not the one that merely looks closest.
            binding <- private$.bindingLimit(params)
            half_width <- if (!is.null(binding))
                binding$distance
            else if (isTRUE(is.finite(params$kappaU)))
                min(params$kappa0 - params$kappaL, params$kappaU - params$kappa0)
            else
                params$kappa0 - params$kappaL

            if (isTRUE(predicted > engine_n_limit)) {
                cost_text <- if (isTRUE(is.finite(predicted)))
                    paste0("it would need about ", private$.fmtN(predicted), " subjects")
                else
                    "no sample size can deliver it"
                jmvcore::reject(
                    paste0(
                        "The requested confidence interval is too narrow to size in reasonable ",
                        "time: ", cost_text, ". The ",
                        "limit that governs the sample size is ", signif(half_width, 3),
                        " away from kappa0, and ",
                        "the required sample size grows roughly as one over the square of that ",
                        "distance. Widen the interval, use more raters, or accept a lower ",
                        "confidence level. As a guide, halving that distance multiplies the ",
                        "required sample size by about four."),
                    code = NULL)
            }

            # Wall-clock backstop, sized to what the engine should cost rather than a flat 20
            # seconds. It is NOT unreachable: kappaSize evaluates its cell probabilities as
            # expanded polynomials, and at an extreme prevalence with several raters those
            # cancel to exactly 0 while the true value is merely tiny. It then divides by that
            # zero, maps the resulting Inf to 0, and its while-loop can never exit. Measured:
            # kappa0 = 0.70 on [0.60, 0.80] with props 0.999/0.001 and SIX raters never returns,
            # while the same design with five raters is sized in 0.55 s. (At the module defaults,
            # kappa0 = 0.60 on [0.40, 0.80], that same props converges in 0.2 s -- the kappa
            # values are part of the trigger, so quote the whole design.) There is no magnitude
            # threshold that separates the two
            # -- props 0.9998 with six raters has a far smaller cell (7.7e-19 against 2.4e-15)
            # and sizes without complaint -- so the only reliable detector is the clock.
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
                if (timed_out) {
                    # Which failure this is matters, because the two need OPPOSITE advice. If
                    # .predictedN sized the design comfortably and the engine still did not
                    # finish, the interval is not the problem -- the engine's arithmetic is, and
                    # the fix is FEWER raters or a less extreme prevalence. Telling that user to
                    # "use more raters", the correct advice for a genuinely narrow interval,
                    # would send them further into the case that is failing.
                    if (isTRUE(predicted <= engine_n_limit)) {
                        jmvcore::reject(
                            paste0(
                                "This combination of expected proportions and rater count is too ",
                                "extreme for the kappaSize engine: with ", params$raters,
                                " raters and a rarest category of ", signif(min(params$props), 3),
                                ", some agreement patterns are so unlikely that the calculation ",
                                "loses them to rounding and never converges. About ",
                                private$.fmtN(predicted), " subjects would be needed. Use fewer ",
                                "raters, or a less extreme expected prevalence \u{2014} the same ",
                                "design usually computes with one rater fewer."),
                            code = NULL)
                    }
                    jmvcore::reject(
                        paste0(
                            "The requested confidence interval is too narrow to size in reasonable ",
                            "time. The limit that governs the sample size is ",
                            signif(half_width, 3),
                            " away from kappa0, and the required sample size grows roughly as one over ",
                            "the square of that distance \u{2014} the search was still running after ",
                            time_budget, " seconds. Widen the interval, use more raters, or accept ",
                            "a lower confidence level."),
                        code = NULL)
                }
                jmvcore::reject("Error in sample size calculation: {}", code = NULL, msg)
            })

            setTimeLimit(elapsed = Inf, transient = TRUE)
            return(result)
        },

        .generateExplanation = function(params) {
            props_text <- if (params$outcome == 2) {
                paste("proportions of", paste(params$props, collapse = " and "))
            } else {
                prop_list <- paste(params$props[-length(params$props)], collapse = ", ")
                paste("proportions of", prop_list, ", and", params$props[length(params$props)])
            }

            is_one_sided <- (params$citype == "one_sided")

            if (is_one_sided) {
                ci_text <- paste0("\u{2022} Lower confidence limit (\u{03BA}L): ", params$kappaL)
                ci_type_text <- "One-sided (lower bound only)"
                objective_text <- paste0(
                    "Determine the required sample size to estimate \u03ba\u2080 = ", params$kappa0,
                    " ensuring the lower confidence limit is at least ", params$kappaL,
                    " in an interobserver agreement study."
                )
            } else {
                # Name the limit that GOVERNS n, not the one geometrically nearer kappa0.
                # kappaSize stops when the chi-square clears the critical value at BOTH limits,
                # so the binding limit is the one with the smaller slope. Over 4,000 random
                # two-sided designs the two disagree about 18% of the time, and when they do the
                # geometrically-nearer limit has no influence on n whatsoever.
                binding <- private$.bindingLimit(params)
                near_txt <- if (!is.null(binding))
                    paste0(
                        "\u{2022} The limit that drives the sample size is ", binding$rho,
                        ", which is ", round(binding$distance, 3), " from \u03ba\u2080",
                        " (it is this distance, not the full interval width, that sets n)")
                else
                    paste0(
                        "\u{2022} Distance from \u03ba\u2080 to the nearer limit: ",
                        round(min(params$kappa0 - params$kappaL,
                                  params$kappaU - params$kappa0), 3),
                        " (this is what drives the sample size, not the full width)")
                ci_text <- paste0(
                    "\u{2022} Confidence interval: [", params$kappaL, ", ", params$kappaU, "]\n",
                    near_txt
                )
                ci_type_text <- "Two-sided"
                objective_text <- paste0(
                    "Determine the required sample size to estimate \u03ba\u2080 = ", params$kappa0,
                    " with confidence limits [", params$kappaL, ", ", params$kappaU,
                    "] in an interobserver agreement study."
                )
            }

            explanation <- paste0(
                "Sample Size Calculation for Interobserver Agreement Study\n\n",
                "This is a CONFIDENCE-INTERVAL calculation: it returns the number of subjects\n",
                "needed for the interval around kappa to reach the requested width. It answers a\n",
                "different question from the power approach (kappaSizePower), which sizes a study\n",
                "to reject a null value, so the two will not agree on a sample size for the same\n",
                "study - choose the one that matches how the result will be reported.\n\n",
                "Study Design:\n",
                "\u{2022} Number of outcome categories: ", params$outcome, "\n",
                "\u{2022} Number of raters: ", params$raters, "\n",
                "\u{2022} Confidence level: ",
                base::format(100 * (1 - params$alpha), scientific = FALSE, trim = TRUE),
                "% (\u03b1 = ", params$alpha, ")\n",
                "\u{2022} CI type: ", ci_type_text, "\n\n",
                "Kappa Parameters:\n",
                "\u{2022} Anticipated kappa (\u03ba\u2080): ", params$kappa0, "\n",
                ci_text, "\n\n",
                "Population Characteristics:\n",
                "\u{2022} Expected category ", props_text, "\n\n",
                "Objective:\n",
                objective_text
            )

            return(explanation)
        },

        .formatSampleSizeOutput = function(result) {
            if (is.null(result) || length(result) == 0) {
                return("Sample size calculation failed")
            }

            required_n <- private$.extractRequiredN(result)
            if (is.na(required_n)) {
                return("Required sample size: unavailable")
            }

            is_one_sided <- (self$options$citype == "one_sided")

            if (is.list(result)) {
                sentence <- private$.buildExampleSentence(
                    required_n = required_n,
                    kappa0 = result$kappa0,
                    kappaL = result$kappaL,
                    kappaU = result$kappaU,
                    one_sided = is_one_sided,
                    raters = result$raters
                )
            } else {
                # Defensive fallback: current kappaSize versions always return a
                # classed list, so this non-list branch is not reached in practice.
                sentence <- private$.buildExampleSentence(
                    required_n = required_n,
                    kappa0 = NA,
                    kappaL = NA,
                    kappaU = NA,
                    one_sided = is_one_sided
                )
            }

            return(paste0("Required sample size: ", private$.fmtN(required_n), "\n", sentence))
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
                return("The required sample size could not be determined from the provided inputs.")
            }
            n_txt <- private$.fmtN(required_n)

            if (is.na(kappa0) || is.na(kappaL)) {
                return(paste0("At least ", n_txt, " subjects are needed for the requested confidence interval precision."))
            }

            # "ensure" was an overclaim: the calculation carries no assurance probability. The
            # limits are those obtained IF the observed kappa lands exactly on kappa0, and about
            # half of such studies observe less. Inverting the engine's own chi-square at the
            # default design (n = 118, kappa0 = 0.60, target [0.40, 0.80]) gives realised limits
            # of [0.400, 0.749] at khat = 0.60 but [0.348, 0.708] at khat = 0.55 -- only 0.54 SD
            # below kappa0, and already short of the promised 0.40 floor.
            raters_txt <- if (is.null(raters) || is.na(raters)) ""
                          else paste0(" rated by ", raters, " raters")

            if (one_sided || is.na(kappaU)) {
                return(paste0(
                    "At least ", n_txt, " subjects", raters_txt, " are needed for the lower ",
                    "confidence limit for \u03ba\u2080 = ", kappa0, " to reach ", kappaL,
                    ", if the observed kappa comes in at ", kappa0, "."
                ))
            }

            return(paste0(
                "At least ", n_txt, " subjects", raters_txt, " are needed for the confidence ",
                "limits to fall within [", kappaL, ", ", kappaU, "], if the observed kappa comes ",
                "in at ", kappa0, "."
            ))
        },

        # Build methodology (INFO) and large-sample (WARNING) notices as HTML.
        # Rendered via a dedicated Html output rather than jmvcore::Notice objects
        # to avoid the notice serialization / no-newline limitations in jamovi.
        .buildNotices = function(required_n, sparse_cells = FALSE,
                                 sparse_min = NA_real_, sparse_below5 = NA_integer_,
                                 sparse_total = NA_integer_,
                                 alpha = NA_real_, one_sided = FALSE,
                                 outcome = NA_integer_) {
            # The confidence level was previously only ever shown symbolically as
            # "100(1 - alpha)%", so a clinician never saw the number. Both citype values deliver
            # 100(1 - alpha)%: two-sided uses qchisq(1 - alpha, 1) = z(1 - alpha/2)^2 and
            # one-sided uses qchisq(1 - 2 alpha, 1) = z(1 - alpha)^2.
            conf_pct <- if (isTRUE(is.finite(alpha)))
                base::format(100 * (1 - alpha), scientific = FALSE, trim = TRUE) else "100(1 - alpha)"
            ci_side_text <- if (isTRUE(one_sided)) "one-sided lower bound" else "two-sided"

            info <- paste0(
                "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #3c8dbc; background-color: rgba(72, 138, 188, 0.06); color: inherit;'>",
                "<b>Methodology.</b> The required sample size is computed with the confidence-interval ",
                "width approach of the kappaSize package (Donner &amp; Eliasziw; Rotondi &amp; Donner). ",
                "It returns the minimum number of subjects so that the ", conf_pct, "% confidence ",
                "interval (", ci_side_text, ") for the intraclass (Fleiss-type) \u{03BA} of the ",
                "common-correlation model attains the requested precision, given the expected ",
                "category proportions and the number of raters. For two raters with equal marginal ",
                "frequencies this coincides with Cohen's \u{03BA}; with more raters, or unequal ",
                "marginals, it does not. ",
                # The engine's own Summary pane says n subjects "ensure" the limits are met.
                # There is no assurance probability anywhere in this design: the limits are the
                # ones obtained IF the observed kappa lands exactly on kappa0, and P(khat < kappa0)
                # is about 0.5. Inverting the engine's chi-square at the default design
                # (n = 118, kappa0 = 0.60, target [0.40, 0.80]) gives [0.400, 0.749] at
                # khat = 0.60 but [0.348, 0.708] at khat = 0.55 -- only 0.54 SD below kappa0 and
                # already short of the 0.40 floor. The module's own text1 is worded
                # conditionally; this sentence covers the vendor pane, which cannot be.
                "<b>This is a planning expectation, not a guarantee.</b> The sample size delivers ",
                "the stated limits only if the study observes the anticipated kappa. Roughly half ",
                "of such studies observe less and finish with a lower limit short of the target, ",
                "so plan on a conservative anticipated kappa or enrol above the figure shown. ",
                "(The Summary panel below reproduces the kappaSize package's own wording, which ",
                "says the sample size will \u{201C}ensure\u{201D} the limits \u{2014} read it with ",
                "this caveat in mind.)",
                "</div>"
            )

            warn <- ""

            # Sparse goodness-of-fit cells, judged by Cochran's rule on the cells the engine
            # actually divides by (see .gofCells / .sparseVerdict): no expected count below 1
            # and at most one cell in five below 5. This replaces a grep for kappaSize's own
            # "expected cell count is less than five" line, which tests the outcome MARGINALS
            # and for a binary outcome only props[1]. That rule missed 7 of 10 realistic
            # designs measured here -- the default six-rater binary study has 6 of 7 pattern
            # cells below 5 with a minimum of 0.013 and produced no warning at all. (That
            # "6 of 7" is the count the ABANDONED element-wise-minimum rule produced; the
            # per-limit rule now in use reports 5 of 7, which is what the notice prints.)
            if (isTRUE(sparse_cells)) {
                warn <- paste0(warn,
                    "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #ec971f; background-color: rgba(227, 144, 33, 0.07); color: inherit;'>",
                    "<b>Sparse categories.</b> At the computed sample size the agreement-pattern ",
                    "cells (for example, exactly k of the raters calling the finding present, or all ",
                    "raters agreeing on one category) are too sparse at the confidence limit where ",
                    "they are thinnest: the smallest expected count is ",
                    private$.fmtCount(sparse_min), " and ",
                    sparse_below5, " of ", sparse_total, " cells are below 5. The calculation rests ",
                    "on a large-sample chi-square approximation, so the required n is less ",
                    "dependable here. Consider ",
                    # A binary outcome has nothing to collapse -- offering it as a remedy sends
                    # the reader looking for an option that cannot exist.
                    if (isTRUE(outcome > 2)) "collapsing rare categories, " else "",
                    "using fewer raters, or recruiting more subjects than the figure shown.",
                    "</div>"
                )
            }

            # NB: paste0(warn, ...), not paste0(...). Writing over `warn` here silently deleted
            # the sparse-cell block whenever both conditions held.
            if (!is.na(required_n) && required_n > 1000) {
                warn <- paste0(warn,
                    "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #d9534f; background-color: rgba(222, 55, 55, 0.06); color: inherit;'>",
                    "<b>Warning.</b> The computed sample size (", private$.fmtN(required_n), ") is very large and may be ",
                    "impractical for a typical interobserver-agreement study. Consider a wider confidence ",
                    "interval (lower precision) or revisiting the expected category proportions",
                    # Adding raters lowers n, but it is also what makes the agreement-pattern
                    # cells sparse. Recommending it directly under a Sparse-categories box that
                    # has just said "use fewer raters" left the two panels contradicting each
                    # other with no way to tell which applied.
                    if (isTRUE(sparse_cells))
                        paste0(". More raters would lower this figure, but that is what makes ",
                               "the cells sparse above \u{2014} the two cannot both be improved ",
                               "by the rater count, so change the interval or the proportions ",
                               "instead.")
                    else
                        ", or increasing the number of raters.",
                    "</div>"
                )
            }

            return(paste0(warn, info))
        },

        # TODO [meddecide audit 2026-05-14] - see docs/audit/MODULE_AUDIT_REPORT_20260514-1847.md
        #   [i18n] 0 .() wraps; bootstrap jamovi/i18n/ then /prepare-translation kappasizeci

        .run = function() {
            # Input validation
            validation_errors <- private$.validateInputs()
            if (!is.null(validation_errors)) {
                error_msg <- paste("Input validation failed:", paste(validation_errors, collapse = "; "))
                jmvcore::reject(error_msg, code='validation_failed')
            }

            # Prepare parameters and run the calculation OUTSIDE the display tryCatch.
            # These call jmvcore::reject() on failure (missing kappaSize package,
            # invalid proportions, or an engine calculation error); letting those
            # conditions propagate makes jamovi render them as real error notices
            # instead of swallowing them into the result body text.
            params <- private$.prepareParameters()
            raw_result <- private$.calculateSampleSize(params)

            # Build outputs defensively; only unexpected formatting/summary errors
            # are caught here (the reject-throwing steps already ran above).
            tryCatch({
                # Format the result
                formatted_result <- private$.formatSampleSizeOutput(raw_result)

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
                if (isTRUE(params$citype == "one_sided"))
                    summary_lines <- sub("^KappaU:\\s*NA\\s*$",
                                         "KappaU: not applicable (one-sided interval)",
                                         summary_lines)
                summary_text <- paste(summary_lines, collapse = "\n")

                # Generate explanation
                explanation <- private$.generateExplanation(params)

                # Methodology / large-sample notices. Sparseness is judged on the
                # agreement-pattern cells at the requested confidence limits (Cochran's rule),
                # not on kappaSize's own marginal check -- see .gofCells. The engine's own
                # marginal line is left untouched in the Summary pane.
                required_n <- private$.extractRequiredN(raw_result)
                verdict <- private$.sparseVerdict(params, required_n)
                notices_html <- private$.buildNotices(
                    required_n,
                    alpha         = params$alpha,
                    one_sided     = isTRUE(params$citype == "one_sided"),
                    outcome       = params$outcome,
                    sparse_cells  = verdict$sparse,
                    sparse_min    = verdict$min,
                    sparse_below5 = verdict$below5,
                    sparse_total  = verdict$total)

                # Set results
                self$results$text1$setContent(formatted_result)
                self$results$text_summary$setContent(summary_text)
                self$results$text2$setContent(explanation)
                self$results$notices$setContent(notices_html)

            }, error = function(e) {
                # This handler used to write the error into text1 and return NORMALLY, so jamovi
                # showed no error notice and the analysis looked like it had succeeded. Its advice
                # was wrong at that point too: validation has passed and the engine has already
                # returned, so kappaSize IS installed and the parameters ARE valid - the fault is
                # downstream formatting. Neither sibling has such a handler; reject so the failure
                # surfaces as a real jamovi error instead of as body text.
                self$results$text1$setContent("")
                self$results$text_summary$setContent("")
                self$results$text2$setContent("")
                self$results$notices$setContent("")
                jmvcore::reject(
                    paste0("The sample size was computed but could not be formatted for display: ",
                           conditionMessage(e)),
                    code = NULL)
            })
        }
    )
)
