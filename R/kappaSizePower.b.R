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
        # Resolved 2026-10-09 (validation, development-ideas/kappaSizePower-validation-2026-10-09.md):
        #   N recomputed from stable cells + exact lambda (VAL-01/03), plain-digit panes (VAL-04),
        #   1-df test named in Methodology (VAL-02), 0.05 gap boundary (VAL-05), small-N wording
        #   (VAL-06), power-approach sources cited (VAL-07).
        # Resolved 2026-10-11 (full texts of the four method papers): Methodology names the
        #   papers' pooled-cell goodness-of-fit test, the intraclass/no-rater-bias estimand and the
        #   Altaye-Donner-Klar binary model; the 3+-rater warning is split by outcome type; the
        #   binary 3+-rater sparse check uses the pooled cells of that test.
        # Resolved 2026-10-14 (validation third pass, development-ideas/kappaSizePower-validation-2026-10-14.md):
        #   the N for estimated proportions is disclosed when the published N falls short of the stated
        #   power (VAL-09); binary 3+ raters: ML prevalence and the GOF test's model dependence stated
        #   (VAL-10); counts just under 5 no longer print as "5" (VAL-11); gap monotonicity qualified (VAL-12).
        # Resolved 2026-10-15 (independent review, development-ideas/kappaSizePower-independent-review-2026-10-15.md):
        #   binary outcome: the recommended test is simulated at the shown N and a simulated N replaces the local
        #   projection's figures, which missed held-out simulation by up to 0.14 either way; 3-5 categories: a
        #   qualitative note (KSP-IR-01). The Methodology states the approximation in both directions; two raters
        #   get the ML-prevalence sentence (KSP-IR-02).
        # Resolved 2026-10-21 (independent review KSP-21-01): the 3-5 category note fires for every 3+ rater design,
        #   not on the projection, which missed shortfalls of 0.06-0.13; the Methodology says so.
        # Resolved 2026-10-15 (review-function): the bold sample size names the simulated N beside the published one;
        #   every simulated figure comes from the one seed shown; a shortfall whose simulated N exceeds 1e9 is still reported.

        # Split the proportions string into numbers. Same format as the siblings
        # (R/kappaSizeFixedN.b.R:.parsePropTokens): commas, semicolons, pipes or whitespace
        # between values, decimal point only. Text pasted from Word or Excel carries U+00A0
        # (non-breaking space) in place of a space, which [:space:] does NOT match, so it is
        # normalised first; otherwise "0.30<nbsp>0.70" fails as one unreadable token.
        .parseProps = function(raw) {
            raw  <- gsub("[\u{00A0}\u{202F}]", " ", raw)   # as kappaSizeFixedN: U+202F too
            toks <- unlist(strsplit(raw, "[,;|[:space:]]+"), use.names = FALSE)
            toks <- toks[nzchar(trimws(toks))]
            list(raw = raw, values = suppressWarnings(as.numeric(trimws(toks))))
        },

        # The goodness-of-fit cell probabilities now live in R/utils-kappasize.R
        # (kappaSizeGofCells) -- the identical closed form used to be copied into all
        # three kappaSize backends. See that file for the derivation and for which
        # agreement level each engine evaluates the cells at.

        # Preformatted panes do not wrap: a 300-character sentence runs off the right edge of
        # the results panel (the siblings hard-break theirs). Wrap at render time so the
        # translated text wraps too.
        .wrap = function(x, width = 78) paste(strwrap(x, width = width), collapse = "\n"),

        # Expected counts run from ~3e-06 to a few dozen, and signif() pasted straight into a
        # sentence rendered the small end as "1.4e-06" in prose aimed at pathologists (binary,
        # 6 raters, 2% prevalence, kappa0 0.40 vs kappa1 0.90 reaches it). scientific = FALSE
        # already prevents that, so the only job left for a floor is to stop the tail growing
        # unreadably long. It used to be 0.01, which is far too high: the six-rater, 5%-finding
        # design at N = 316 has a cell of 0.00034 -- a pattern that is effectively never
        # observed -- and "below 0.01" hid exactly how empty it is behind a figure that also
        # covers a comfortable 0.009. R/kappaSizeCI.b.R:.fmtCount and
        # R/kappaSizeFixedN.b.R:.fmtCount share the floor and wording, not the guard below.
        # Two significant digits round 4.97 up to "5", printed beside "... are below 5" and
        # "Fewer than five cases" (VAL-11), so a count just under 5 keeps two decimals.
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

        # KSP-IR-01: power of the test the Notes recommend for a binary outcome - the goodness-of-fit
        # chi-square on the pooled cells (all negative / any disagreement / all positive; for two raters
        # these are Donner & Eliasziw's three cells), prevalence estimated by maximum likelihood with
        # kappa held at kappa0, 1 df - in R studies of N subjects generated under kappa1 (Altaye-Donner-
        # Klar mixture). The ML step is a bisection on the score, vectorised over the R studies; the
        # pooled-cell likelihood has one free parameter. Checked against an independent optimize()-based
        # simulation in development-scripts/audit_kappaSizePower.R (block W).
        .simPowerBinary = function(raters, p, kappa0, kappa1, alpha, N, R) {
            cells <- function(q, rho) {
                a0 <- (1 - rho) * (1 - q)^raters + rho * (1 - q)
                an <- (1 - rho) * q^raters + rho * q
                cbind(a0, 1 - a0 - an, an)
            }
            O <- t(stats::rmultinom(R, N, cells(p, kappa1)[1, ]))
            score <- function(q) {
                P  <- cells(q, kappa0)
                d0 <- -(1 - kappa0) * raters * (1 - q)^(raters - 1) - kappa0
                dn <-  (1 - kappa0) * raters * q^(raters - 1) + kappa0
                O[, 1] * d0 / P[, 1] - O[, 2] * (d0 + dn) / P[, 2] + O[, 3] * dn / P[, 3]
            }
            lo <- rep(1e-12, R); hi <- rep(1 - 1e-12, R)
            for (it in seq_len(60)) {
                mid <- (lo + hi) / 2
                up  <- score(mid) > 0
                up[is.na(up)] <- FALSE
                lo[up] <- mid[up]; hi[!up] <- mid[!up]
            }
            E  <- N * cells((lo + hi) / 2, kappa0)
            x2 <- rowSums(ifelse(E > 0, (O - E)^2 / E, ifelse(O > 0, Inf, 0)))
            mean(x2 > stats::qchisq(1 - alpha, 1))
        },

        # N whose simulated power reaches the target, starting from a short n0 with simulated power p0. The
        # 1-df noncentrality grows about linearly in N, so each step rescales N by lambda(aim) /
        # lambda(simulated power) and simulates again. Every step uses the one seed the notes show, so each figure
        # they report is reproducible from it (seed + step made "random seed X" true only of the first). It aims 0.01 above the target and
        # stops within 0.015 of the aim (about 2.4 Monte Carlo SE at R = 4000), so the N it returns reaches the
        # stated power to within Monte Carlo error rather than landing just below it; at most 5 steps.
        .simulatedN = function(raters, p, kappa0, kappa1, alpha, power, n0, p0, R, seed) {
            z   <- stats::qnorm(1 - alpha / 2)
            lam <- function(x) {
                x <- min(x, 1 - 1e-6)
                if (x <= alpha) return(0)
                stats::uniroot(function(l) stats::pnorm(sqrt(l) - z) + stats::pnorm(-sqrt(l) - z) - x,
                               c(0, 1e4), tol = 1e-10)$root
            }
            aim    <- min(power + 0.01, 0.999)
            target <- lam(aim)
            n <- n0; pw <- p0
            for (step in seq_len(5)) {
                l <- lam(pw)
                n <- if (l > 0) ceiling(n * target / l) else n * 4
                if (n > 1e9) return(NULL)
                pw <- withr::with_seed(seed,
                    private$.simPowerBinary(raters, p, kappa0, kappa1, alpha, n, R))
                if (abs(pw - aim) <= 0.015) break
            }
            list(n = n, power = pw)
        },

        # A round sample size pastes as "1e+05" and a large one is unreadable without
        # separators ("The design needs 76724 subjects"). Every place n reaches the user
        # goes through here, as in R/kappaSizeCI.b.R:.fmtN.
        .fmtN = function(n) {
            if (!isTRUE(is.finite(n))) return("NA")
            base::format(round(n), scientific = FALSE, big.mark = ",", trim = TRUE)
        },

        # A power or probability for prose: two decimals, never "0.8" beside "0.75".
        .fmtPower = function(x) base::format(round(x, 2), nsmall = 2),

        # Display form of a category proportion. .run() now renormalises the vector before it
        # reaches the engine, so a typed "0.333, 0.333, 0.333" becomes 0.33333333333333331 and
        # jmvcore::format renders a double at 15 significant digits -- the Study Explanation
        # would read "the prevalences of the categories are 0.333333333333333 ...". Round for
        # DISPLAY only; the engine keeps full precision. Element-wise, because base::format()
        # on a VECTOR pads every element to a common number of decimals and would turn a typed
        # 0.1 into "0.10" next to a 0.25.
        .fmtProp = function(x) {
            vapply(x, function(v) base::format(signif(v, 4), scientific = FALSE, trim = TRUE),
                   character(1))
        },

        # Build the study-explanation paragraph (plain text; generic across cardinalities).
        .buildExplanation = function(kappa0, kappa1, alpha, power, raters, props) {
            props_txt <- private$.fmtProp(props)
            prev <- if (length(props) == 1) {
                .fmt(
                    .("Further suppose that the prevalence of the trait is {p}."),
                    p = props_txt[1])
            } else if (length(props) == 2) {
                # The user never said which category is the "trait"; with "0.80, 0.20" the
                # old sentence claimed the lesion was present in 80% of cases.
                .fmt(
                    .("Further suppose that the two categories occur in {p1} and {p2} of subjects (the required sample size is the same whichever category is called positive)."),
                    p1 = props_txt[1], p2 = props_txt[2])
            } else {
                .fmt(
                    .("Further suppose that the prevalences of the categories are {head} and {last}."),
                    head = paste0(props_txt[-length(props_txt)], collapse = ", "),
                    last = props_txt[length(props_txt)])
            }

            paste0(
                private$.wrap(paste(
                    .("This is a POWER calculation: it returns the number of subjects needed to REJECT kappa0 in favor of kappa1 in a two-sided test at the stated significance level and power."),
                    .("It answers a different question from the confidence-interval approach (kappaSizeCI), which sizes a study so that the expected confidence limits around an anticipated kappa fall at chosen values, so the two will not agree on a sample size for the same study - pick the one that matches how the result will be reported."))),
                "\n\n",
                private$.wrap(.fmt(
                    .("Researchers would like to determine the required sample size to test the null hypothesis kappa = {kappa0} against the alternative kappa = {kappa1} at a two-sided significance level of {alpha} with power {power}, in a study of interobserver agreement with {raters} raters."),
                    kappa0 = kappa0, kappa1 = kappa1, alpha = alpha,
                    power = base::format(power, nsmall = 2), raters = raters)),
                "\n",
                private$.wrap(prev))
        },

        # Notes panel, same shape as kappaSizeCI / kappaSizeFixedN: warnings first, then the
        # method statement. Rendered via a dedicated Html output rather than jmvcore::Notice
        # objects: Notice content is escaped plain text with no line breaks, and these blocks
        # need bold titles and several paragraphs (CLAUDE.md, "Notices: the insert() index").
        # "1 subjects" reads as a bug in a protocol sentence.
        .subjects = function(n) if (isTRUE(n == 1)) .("subject") else .("subjects"),

        .buildNotices = function(n_required, sparse, outcome, raters, kappa0, kappa1, power,
                                 sparse_min = NA_real_, sparse_below5 = NA_integer_,
                                 sparse_total = NA_integer_, marg_min = NA_real_,
                                 marg_sparse = FALSE, est = NULL) {
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
                    .fmt(
                        .("The alternative kappa ({kappa1}) is BELOW the null ({kappa0}). This sizes a study to demonstrate that agreement is WORSE than the null value, not better. If you meant to detect an improvement, swap the two values - the required sample size is not the same either way."),
                        kappa0 = kappa0, kappa1 = kappa1)))
            }

            # A gap of 0.01 between the two kappas asks for ~100,000 subjects. That is the
            # signature of the other common transposition: the EXPECTED kappa typed as kappa1
            # and a nearby value as kappa0, where kappa0 should be the agreement to rule out.
            # Worded as "difference", not "improvement", because the swap note above may have
            # just told the user that kappa1 below kappa0 is a legitimate design.
            # A gap typed as exactly 0.05 computes as 0.0499999... (0.45 - 0.40), which fired this
            # notice at its own threshold for 6 of 10 typed pairs; the tolerance keeps it out.
            if (abs(kappa1 - kappa0) < 0.05 - 1e-9) {
                warn <- paste0(warn, block(warn_div,
                    .("Small kappa difference."),
                    .fmt(
                        .("The null and alternative kappa differ by only {delta}. Check that kappa0 is the agreement you want to rule OUT (not the agreement you expect to observe) and that kappa1 is a clinically meaningful difference from it; very close values need very large studies."),
                        delta = signif(abs(kappa1 - kappa0), 3))))
            }

            # A large n has three drivers -- a rare finding, a small kappa gap, high power --
            # and the kappa gap is often NOT the one (prevalence 1% with the default kappas
            # needs 3,429), so this block names all three rather than blaming one. The gap and
            # the power act on every design; the prevalence usually does (Altaye, Donner &
            # Eliasziw 2001 p.2486), but for 3-5 categories the driver is UNEQUAL proportions, not
            # distance from 0.5 (their Table IV: 0.1/0.4/0.5 needs more subjects than 0.3/0.3/0.4).
            if (has_n && n_required > 2000) {
                warn <- paste0(warn, block(warn_div,
                    .("Very large sample size."),
                    .fmt(
                        .("The design needs {n} subjects; few agreement studies can enrol this many. Moving kappa1 closer to kappa0 from the same side, or raising the power target, always inflates the sample size, and a rare finding usually does too (for a binary outcome, a prevalence far from 0.5; with more categories, very unequal category proportions) - revisit whichever of these is not a firm requirement."),
                        n = private$.fmtN(n_required))))
            }

            # The option ranges still admit designs the method cannot size: alpha just below
            # power gives "A minimum of 2 subjects", kappa 0.01 vs 0.99 gives 1. The engine
            # converges on these instantly, so they have to be caught by their result.
            # No direction is claimed: this used to say "a lower bound", but exact enumeration of
            # every two-rater design below 10 subjects found the figure too HIGH (power 0.95-0.99
            # at N = 9, nominal power first reached at N = 5).
            if (has_n && n_required < 10) {
                warn <- paste0(warn, block(warn_div,
                    .("Very small sample size."),
                    .fmt(
                        .("The required sample size is only {n} {subjects}. The large-sample approximation behind this method does not hold for such small studies, so the figure is only approximate; treat it as a rough guide, not a plan."),
                        n = private$.fmtN(n_required), subjects = private$.subjects(n_required))))
            }

            # VAL-09 / KSP-IR-01: .run() fills est only from 10 subjects up. Binary: the simulated power fell
            # more than 0.03 short; 3-5 categories: three or more raters (KSP-21-01), a loss it cannot size reliably.
            # est$n is NULL when the N that reaches the target is above 1e9 (rmultinom's integer size): the shortfall
            # was still measured, so it is still reported, without a simulated N.
            if (isTRUE(est$type == "sim")) {
                warn <- paste0(warn, block(warn_div,
                    .("Simulated power below the stated power."),
                    .fmt(
                        .("The published formula behind this sample size is a large-sample approximation that treats the expected proportions as known. In {R} simulated studies of {n} subjects analyzed with the recommended goodness-of-fit test (prevalence estimated by maximum likelihood with kappa held at kappa0), the power to reject kappa0 when kappa equals kappa1 was about {sim}, short of the stated {power}."),
                        R = private$.fmtN(est$R), n = private$.fmtN(n_required),
                        # nsmall, not .fmtPower(): "0.80" beside "0.75", but a typed 0.825 stays 0.825
                        sim = private$.fmtPower(est$power), power = base::format(power, nsmall = 2)),
                    if (is.null(est$n))
                        .fmt(.("The sample size that reaches the stated power is too large to simulate (random seed {seed})."),
                             seed = est$seed)
                    else
                        .fmt(.("About {needed} subjects gave a simulated power of about {simN} (random seed {seed})."),
                             needed = private$.fmtN(est$n), simN = private$.fmtPower(est$power_n),
                             seed = est$seed)))
            }
            if (isTRUE(est$type == "cat")) {
                warn <- paste0(warn, block(warn_div,
                    .("Expected proportions treated as known."),
                    .fmt(
                        .("The published formula behind this sample size treats the expected proportions as known and rests on a large-sample approximation. With three or more raters and three to five categories, simulations of the recommended goodness-of-fit test at the published sample size gave a power of about 0.64 to 0.81 for a stated 0.80, below 0.75 in 8 of 12 designs, balanced proportions included, so this design can fall well short of the stated power of {power} at {n} subjects. How far short cannot be computed reliably here, so confirm the sample size by simulating the intended analysis."),
                        n = private$.fmtN(n_required), power = base::format(power, nsmall = 2))))
            }

            if (is.finite(power) && power < 0.5) {
                warn <- paste0(warn, block(warn_div,
                    .("Low power."),
                    .fmt(
                        .("The requested power is {power}. A study powered below 50% is more likely to miss the alternative kappa than to detect it; conventional values are 0.80 or 0.90."),
                        power = base::format(power, nsmall = 2))))
            }

            # Sparse goodness-of-fit cells (see kappaSizeGofCells), judged by Cochran's rule: no expected
            # count below 1 and at most one cell in five below 5. kappaSize prints its own
            # marginal version once per sparse CATEGORY inside the raw result text; it was
            # reaching the user only as repeated lines there, and it watches the wrong quantity.
            # Skipped when n is below 10: every cell is sparse then, the "Very small sample size"
            # block above already says so, and "enrich the case series" would point at the wrong
            # cause.
            too_small <- has_n && n_required < 10
            pattern_shown <- isTRUE(sparse) && !too_small
            # One remedy for both blocks below: the two conditions are different, but what the
            # user can do about either is the same, and printing the identical sentence twice
            # reads like a copy-paste slip. Whichever block comes first carries it.
            remedy <- if (outcome == 2L) {
                .("Consider enriching the case series so the rare finding is more common (the calculation assumes the stated prevalence), or planning a larger study.")
            } else {
                .("Consider collapsing rare categories or planning a larger study.")
            }
            if (pattern_shown) {
                warn <- paste0(warn, block(warn_div,
                    .("Sparse categories."),
                    .fmt(
                        .("At the required sample size the goodness-of-fit cells (all raters agreeing on one category, or the single pooled cell for any disagreement) are too sparse: the smallest expected count is {min} and {below} of {total} cells are below 5. The calculation rests on a large-sample chi-square approximation, so the sample size shown is less dependable here."),
                        min = private$.fmtCount(sparse_min), below = sparse_below5,
                        total = sparse_total),
                    remedy))
            }

            # The engine's own (suppressed, untranslated) caveat watches the outcome MARGINALS,
            # props[j] * N < 5, and the Cochran rule above does NOT cover it. An agreement-pattern
            # probability is never larger than its category's marginal, so one rare category in a
            # 4- or 5-category design puts exactly ONE of the 5-6 cells below 5 -- mean(e < 5) is
            # then 1/6 = 0.17, under the 20% threshold, and the block above stays silent while the
            # engine would have warned. Measured: 5 categories, props 0.02/0.245 x 4, 2 raters,
            # kappa0 0.80 vs kappa1 0.90 -> N = 176, rarest marginal 3.5 (engine warns), smallest
            # agreement cell 2.8 with 1 of 6 below 5 and none below 1 (Cochran silent).
            #
            # This used to be gated on !pattern_shown, which silenced it on every design where
            # the Cochran block fired -- including the designs where BOTH are true, which is
            # most sparse binary studies. The two describe different quantities (how empty the
            # chi-square's cells are, versus how few cases of the rare category the study will
            # even contain) and a reader needs both, so both are allowed to appear. Only the
            # shared remedy is dropped from the second, since it is the same sentence.
            if (isTRUE(marg_sparse) && !too_small) {
                warn <- paste0(warn, block(warn_div,
                    .("Rare outcome category."),
                    .fmt(
                        .("At the required sample size the rarest outcome category is expected in only {min} subjects. Fewer than five cases of a category makes the large-sample chi-square approximation behind this method unreliable, and it leaves the study with too few examples of that category for the raters to disagree about."),
                        min = private$.fmtCount(marg_min)),
                    if (!pattern_shown) remedy))
            }

            # Every sentence below was checked against the full texts (2026-10-11,
            # development-ideas/kappaSizePower_refs_audit.md):
            # - N formula, two-sided, 1-df noncentral chi-square: Donner & Eliasziw 1992 p.1517-1518
            #   (N = 66 example, reproduced); Altaye, Donner & Klar 2001 p.587 (N = 66 and 40,
            #   reproduced); Altaye, Donner & Eliasziw 2001 p.2486 (Table IV, reproduced to within
            #   the paper's nearest-integer rounding; this module rounds up).
            # - Intraclass kappa, no rater bias: Donner & Eliasziw p.1512 and p.1518 ("When there is
            #   rater bias, methods described by Fleiss ... are more appropriate"); ADE p.2482.
            # - The test the N is for pools every disagreement into one cell: ADK p.586 (2 df over
            #   all four 3-rater cells, 1 df after pooling; pooling leaves N unchanged under eq. 4);
            #   ADE p.2482. ADE p.2483 re-estimates the prevalences by ML at kappa0; plugging in the
            #   raw proportions instead rejected a true kappa0 10-22% of the time at a nominal 5%
            #   with 3+ raters (validation VAL-02 simulation), while the binary all-counts chi-square
            #   has raters - 1 df (ADK p.586), size 0.16-0.65 when read on 1 df.
            # - Binary 3+ raters: the cells are ADK's eq. (4) model, lambda_k = (1 - rho) pi^k + rho pi,
            #   i.e. the mixture; ADE's model reduces to the beta-binomial for two categories (p.2483),
            #   which gives 0.89 to 12.5 times the subjects of this N at the same kappa (VAL-08).
            info <- block(info_div,
                .("Methodology."),
                .("Power-based sample size for a two-sided test of H0: kappa = kappa0 against H1: kappa = kappa1, using the goodness-of-fit (non-central chi-square) approach of Donner and Eliasziw (two raters, binary outcome), Altaye, Donner and Klar (binary outcome, more raters) and Altaye, Donner and Eliasziw (three to five categories), as implemented in the kappaSize package (Rotondi and Donner)."),
                .("Kappa here is the unweighted intraclass kappa for nominal categories (Scott's pi for two raters, Fleiss' kappa for more), which assumes that all raters share the same category prevalences; when raters differ systematically (rater bias), other methods are more appropriate."),
                .("The sample size is for the papers' goodness-of-fit chi-square test on one degree of freedom, computed over the cells in which all raters agree on a category plus one pooled cell for any disagreement; a likelihood-ratio test of kappa is an approximate alternative."),
                # KSP-IR-01: simulated power at the published N was 0.74-0.83 with two raters and as low as 0.28
                # with several raters and a rare finding, for a stated 0.80 (independent review 2026-10-15); with
                # 3-5 categories and 3+ raters 0.64-0.74 in 8 of 12 designs, balanced ones too (KSP-21-01).
                .("Like the published formulas, it treats the expected proportions as known and rests on a large-sample approximation, so the power actually achieved can differ from the stated power in either direction: by several percentage points in many designs, and by much more with several raters or a rare finding. For a binary outcome the analysis therefore simulates the recommended test at the sample size shown and, when the simulated power falls more than 0.03 short, gives a simulated sample size; with three to five categories and three or more raters, where the simulated power at the published sample size was below 0.75 for a stated 0.80 in 8 of 12 designs checked, a note asks for the sample size to be confirmed by simulation (both from 10 subjects upwards)."),
                if (isTRUE(outcome == 2L && raters >= 3))
                    .("With three or more raters, do not compute the chi-square over every count of raters calling the finding present (none, one, ..., all): that statistic has more than one degree of freedom and, read on one, rejects a true kappa0 much more often than the stated significance level."),
                # VAL-10: DE 1992 p.1513 and ADK 2001 p.585 plug in the overall proportion of positive
                # ratings; simulated size 0.096-0.164 at a nominal 0.05 with 4-6 raters and a 1-10%
                # finding, 0.046-0.050 with the ML estimate (validation 2026-10-14, N5).
                if (isTRUE(outcome == 2L && raters >= 3))
                    .("Estimate the prevalence for that test by maximum likelihood from the same pooled cells, with kappa held at kappa0: the published procedure plugs in the overall proportion of positive ratings, which makes the test reject a true kappa0 too often with four or more raters and a rare finding."),
                # KSP-IR-02: DE 1992 p.1513 plugs in the overall proportion of positive ratings. Simulated (R = 20000,
                # p .01-.05, kappa0 .2-.6, nominal 0.05): size 0.057-0.066, ML at kappa0 0.043-0.051 at equal power.
                # A second run (release review 2026-10-15, R = 20000, the 9 designs p x kappa0 at the published N
                # for kappa1 = kappa0 + 0.2) gave plug-in 0.055-0.066 and ML 0.046-0.052; the sentence states the union.
                # Rates, not percentages: a literal "%" in a msgid can be read as a C format directive.
                if (isTRUE(outcome == 2L && raters == 2L))
                    .("With two raters, estimate the prevalence for the test by maximum likelihood with kappa held at kappa0 rather than plugging in the overall proportion of positive ratings, as the published procedure does: in simulations of a rare finding (1 to 5 cases in 100) at a 0.05 level, the plug-in test rejected a true kappa0 at a rate of 0.055-0.066 and the maximum-likelihood version at 0.043-0.052."),
                if (isTRUE(outcome >= 3L && raters >= 3))
                    .("With three or more raters, estimate the category prevalences by maximum likelihood with kappa held at kappa0, as Altaye, Donner and Eliasziw do; plugging in the overall proportions of ratings makes the test reject a true kappa0 too often."),
                if (isTRUE(outcome == 2L && raters >= 3))
                    .("For a binary outcome with three or more raters the calculation uses the rater-dependence model of Altaye, Donner and Klar: with probability kappa all raters give one shared call, otherwise each rater calls independently. Other models with the same kappa, such as the beta-binomial (the two-category case of the model of Altaye, Donner and Eliasziw), can need a markedly different number of subjects."),
                # Beta-binomial ratings at the same pairwise kappa: the mixture-model GOF test rejected a
                # true kappa0 in 9-81% of simulated studies, Fleiss' kappa with a jackknife Wald test in
                # 6-7% (validation 2026-10-14, N6).
                if (isTRUE(outcome == 2L && raters >= 3))
                    .("The goodness-of-fit test also tests that model: if the raters follow another model with the same kappa, it can reject a true kappa0 far more often than the stated significance level, whereas Fleiss' kappa with a jackknife or bootstrap interval that resamples subjects is not tied to the model."),
                .("It does not apply to weighted kappa for ordered grades; for an ordinal grading study either treat the grades as nominal categories or use a dedicated weighted-kappa method."),
                .("It answers 'how many subjects do I need to reject kappa0 in favor of kappa1?' - not how precisely kappa will be estimated; to plan the expected confidence limits around an anticipated kappa, use kappaSizeCI instead."),
                .("Note that kappa0 here is the null hypothesis value, whereas in kappaSizeCI and kappaSizeFixedN it is the agreement you anticipate observing."))

            # The bold figure is what gets copied into a protocol, so when the simulation found the published N
            # short it carries the simulated N too, not the published one alone. Its own block, between the
            # warnings and the Methodology: it used to be the last of up to 15 sentences of the method statement.
            # "each rated by all 2 raters" read oddly: two raters get their own whole sentence.
            both <- isTRUE(raters == 2)
            answer <- if (has_n && isTRUE(est$type == "sim") && !is.null(est$n))
                .fmt(
                    if (both) .("Required sample size: <b>{n}</b> {subjects} by the published formula and about <b>{needed}</b> by simulating the recommended test, each rated by both raters.")
                    else .("Required sample size: <b>{n}</b> {subjects} by the published formula and about <b>{needed}</b> by simulating the recommended test, each rated by all {raters} raters."),
                    n = private$.fmtN(n_required), subjects = private$.subjects(n_required),
                    needed = private$.fmtN(est$n), raters = raters)
            else if (has_n)
                .fmt(
                    if (both) .("Required sample size: <b>{n}</b> {subjects}, each rated by both raters.")
                    else .("Required sample size: <b>{n}</b> {subjects}, each rated by all {raters} raters."),
                    n = private$.fmtN(n_required),
                    subjects = private$.subjects(n_required), raters = raters)
            if (!is.null(answer)) answer <- paste0(info_div, answer, "</div>")

            paste0(warn, answer, info)
        },

        # Syntax pane: props escaped (see kappaSizeSourcifyOption in R/utils-kappasize.R).
        .sourcifyOption = function(option) {
            if (!option$name %in% names(formals(kappaSizePower))) return("")
            src <- kappaSizeSourcifyOption(option)
            if (is.null(src)) super$.sourcifyOption(option) else src
        },

        .run = function() {

            outcome <- suppressWarnings(as.numeric(self$options$outcome))
            kappa0  <- self$options$kappa0
            kappa1  <- self$options$kappa1
            raters  <- suppressWarnings(as.numeric(self$options$raters))
            alpha   <- self$options$alpha
            power   <- self$options$power


            if (!isTRUE(outcome %in% c(2, 3, 4, 5)))
                jmvcore::reject(.("Number of outcome levels must be 2, 3, 4, or 5."), code = "invalid_outcome")
            outcome <- as.integer(outcome)

            if (!isTRUE(raters %in% 2:6))
                jmvcore::reject(.("Number of raters must be 2, 3, 4, 5, or 6."), code = "invalid_raters")
            raters <- as.integer(raters)

            if (!is.finite(kappa0) || kappa0 <= 0 || kappa0 >= 1)
                jmvcore::reject(.("kappa0 must be strictly between 0 and 1."), code = "invalid_kappa0")

            if (!is.finite(kappa1) || kappa1 <= 0 || kappa1 >= 1)
                jmvcore::reject(.("kappa1 must be strictly between 0 and 1."), code = "invalid_kappa1")

            if (!is.finite(alpha) || alpha < 0.001 || alpha > 0.20)
                jmvcore::reject(.("Significance level (alpha) must be between 0.001 and 0.20."), code = "invalid_alpha")

            if (!is.finite(power) || power < 0.01 || power > 0.99)
                jmvcore::reject(.("Power must be between 0.01 and 0.99."), code = "invalid_power")

            # --- Parse and validate the proportions ---------------------------
            parsed <- private$.parseProps(self$options$props)
            props  <- parsed$values

            if (length(props) == 0 || anyNA(props))
                jmvcore::reject(
                    .("Proportions must be numbers separated by commas, semicolons or spaces (for example '0.20, 0.80'). One or more entries could not be read as a number. Note that a decimal comma is not recognized - use a point, as in 0.20."),
                    code = "invalid_props")

            if (any(props <= 0) || any(props >= 1)) {
                # "0,30 0,70" splits into 0, 30, 0, 70 and then fails the range check, which
                # tells a user with a European keyboard that their proportions are out of range
                # rather than that the decimal separator is wrong. Detect that case by re-reading
                # the string with the comma as a decimal point and seeing whether it becomes
                # valid; only then is it really a decimal-separator problem. Every token must
                # parse (R/utils-kappasize.R): dropping the unreadable ones told the typo
                # "1.2, 0.8" to use the decimal point it already used.
                if (kappaSizePropsLookDecimalComma(parsed$raw))
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
                    .fmt(
                        .("Enter exactly {k} proportions for {k} outcome levels (received {got})."),
                        k = outcome, got = length(props)),
                    code = "props_count_mismatch")
            }

            if (length(props) >= 2 && abs(sum(props) - 1) >= 0.001)
                jmvcore::reject(
                    .fmt(
                        .("Proportions must sum to 1 (current sum = {sum})."),
                        sum = round(sum(props), 4)),
                    code = "props_sum")

            # Renormalise. kappaSize accepts a sum within 0.001 of 1 and then uses the values
            # VERBATIM, so a 4-decimal rounding artefact such as 0.0003/0.0003/0.9999 (sum
            # 1.0005) drives the lumped goodness-of-fit cell P0 = 1 - sum(agree) negative, the
            # chi-square denominator flips sign and the engine returns a NEGATIVE sample size:
            # Power3Cats(kappa0 = 0.50, kappa1 = 0.40, props = c(0.0003, 0.0003, 0.9999),
            # raters = 2) gave N = -380296.1 where the valid design needs 218,244, and
            # ceiling() of a negative is finite so every pane printed it. Where the result
            # stays positive the same defect mis-sizes silently (0.01/0.01/0.9805 at
            # kappa0 = 0.90: 1,675 verbatim against 1,887 renormalised). This is bit-identical
            # for any input that already sums to one, so it only touches inputs that were
            # wrong. Same fix as R/kappaSizeCI.b.R:.validateProportions().
            if (length(props) >= 2)
                props <- props / sum(props)

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
                    .fmt(
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
                        .fmt(
                            .("Sample size calculation failed: {error}"),
                            error = conditionMessage(e)),
                        code = "kappasize_error"))

            # --- Recompute N from stable cells and an exact noncentrality -----
            # Same formula as kappaSize, N = lambda / sum((P1 - P0)^2 / P0), evaluated where the
            # engine is numerically fragile. PowerBinary expands its cells as polynomials in
            # props[1]: with 5-6 raters and the COMMON category typed first (p >= 0.999) a cell
            # cancels to exactly 0 and N came back as 0 ("0.9999, 0.0001", 5 raters: "0 subjects"
            # where the design needs 181,129) or NaN (an R error from print()). Its lambda is a
            # bisection stopped at a CDF error of 1e-5, so N also carried a relative error up to
            # ~1e-4 and could be a subject off at large N. kappaSizeGofCells() is the binomial /
            # Dirichlet-multinomial form of the same cells (no cancellation; symmetric in p and
            # 1 - p), and lambda is solved to 1e-12. Checked against an independent oracle on 900
            # designs: development-ideas/kappaSizePower-validation-2026-10-09.md (VAL-01, VAL-03).
            P0 <- kappaSizeGofCells(outcome, raters, props, kappa0)
            P1 <- kappaSizeGofCells(outcome, raters, props, kappa1)
            if (any(!is.finite(P0)) || any(!is.finite(P1)) || any(P0 <= 0) || any(P1 <= 0))
                jmvcore::reject(
                    .("The sample size could not be computed: a proportion is too close to 0 for the agreement-pattern probabilities to be evaluated. Check the proportions."),
                    code = "n_not_computable")
            crit <- stats::qchisq(1 - alpha, 1)
            lambda <- stats::uniroot(
                function(l) stats::pchisq(crit, 1, ncp = l) - (1 - power),
                c(0, 100), tol = 1e-12)$root
            result$N <- lambda / sum((P1 - P0)^2 / P0)
            # Only a proportion small enough to underflow a cell (props "1e-200") gets here.
            if (!isTRUE(is.finite(result$N) && result$N > 0))
                jmvcore::reject(
                    .("The sample size could not be computed: a proportion is too close to 0 for the agreement-pattern probabilities to be evaluated. Check the proportions."),
                    code = "n_not_computable")

            # kappaSize's print() and summary() write N with cat(), which prints a round 100000 as
            # "1e+05" (VAL-04); scipen keeps both panes in plain digits.
            op <- options(scipen = 999)
            on.exit(options(op), add = TRUE)

            # --- Populate outputs ---------------------------------------------
            # print() gives the headline sentence ("A minimum of N subjects ..."); summary()
            # repeats it and adds the study parameters. The engine prints its cell-count warning
            # once per sparse category (five times for five levels), in untranslatable English,
            # and computes it as props[i] * N on the outcome MARGINALS -- not the quantity that
            # governs the chi-square. Keep the FIRST of those identical lines and drop the
            # repeats, as R/kappaSizeFixedN.b.R does: deleting them all would silently edit the
            # engine's own output, and a reader comparing the pane against a direct kappaSize
            # call would find a warning missing. The Notes panel restates the caveat once,
            # translated, from the agreement-pattern cells that matter PLUS a marginal check
            # that covers the designs those cells miss (see .buildNotices).
            dedupe <- function(lines)
                lines[!(duplicated(lines) & grepl("expected cell count", lines, fixed = TRUE))]
            result_text <- paste(dedupe(utils::capture.output(print(result))), collapse = "\n")
            # library-audit 2026-10-23 meddecide [MEDIUM] DONE: panels are cleared on the success path only; under
            #   reject() the greyed previous result is jamovi's failure presentation (guide section 5).
            self$results$notices$setContent("<div></div>")  # Html: the client skips ""
            self$results$text1$setContent("")
            self$results$text_summary$setContent("")
            self$results$text2$setContent("")
            self$results$text1$setContent(result_text)

            summary_text <- paste(dedupe(utils::capture.output(summary(result))),
                                  collapse = "\n")
            self$results$text_summary$setContent(summary_text)

            # Put the computed answer in the explanation too, so the sentence a user copies into
            # a protocol carries the number it is explaining.
            n_required <- suppressWarnings(ceiling(as.numeric(result$N)))

            # VAL-09 / KSP-IR-01: the published N is a large-sample approximation that treats the proportions as
            # known. Binary outcome: simulate the recommended test at the shown N and, when it falls more than 0.03
            # short (about 4.7 Monte Carlo SE at R = 4000), find the N that reaches the target by simulation.
            # 3-5 categories: a qualitative warning for every design with three or more raters (KSP-21-01, independent
            # review 2026-10-21). Simulated power of the ML-at-kappa0 test at the published N, stated 0.80, R = 2000:
            # 0.636-0.807 over 12 such designs, below 0.75 in 8, balanced .3/.3/.4 with 6 raters at 0.68; two raters
            # 0.785 and 0.805. The trigger it replaces, a local projection of the noncentrality lost to estimating
            # the proportions (Mitra 1958; ratio < 0.85), fired in 1 of those 8: it read 0.995 for .3/.3/.4, so the
            # shortfall is not that projection's and no ratio threshold separated short from adequate designs.
            # Not computed below 10 subjects (the "Very small sample size" note already says approximate) or above
            # 1e9 (rmultinom's integer size; the "Very large sample size" note covers those).
            est <- NULL
            if (isTRUE(n_required >= 10 && n_required <= 1e9)) {
                if (outcome == 2L) {
                    sim_R <- 4000L; sim_seed <- 20261015L
                    sim_p <- withr::with_seed(sim_seed, private$.simPowerBinary(
                        raters, props[1], kappa0, kappa1, alpha, n_required, sim_R))
                    if (sim_p < power - 0.03) {
                        found <- private$.simulatedN(raters, props[1], kappa0, kappa1, alpha, power,
                                                     n_required, sim_p, sim_R, sim_seed)
                        # found is NULL above 1e9 subjects; est$n and est$power_n are then NULL
                        est <- list(type = "sim", power = sim_p, n = found$n, power_n = found$power,
                                    R = sim_R, seed = sim_seed)
                    }
                } else if (raters >= 3) {
                    est <- list(type = "cat")
                }
            }

            self$results$text2$setContent(paste0(
                private$.buildExplanation(kappa0, kappa1, alpha, power, raters, props),
                if (is.finite(n_required))
                    paste0("\n", private$.wrap(paste(c(
                        .fmt(
                            .("The required sample size is {n} {subjects}."),
                            n = private$.fmtN(n_required),
                            subjects = private$.subjects(n_required)),
                        if (isTRUE(est$type == "sim") && is.null(est$n))
                            .fmt(
                                .("A simulation of the recommended goodness-of-fit test gives a power of about {sim} at this sample size, short of the stated {power}; the sample size that reaches it is too large to simulate (random seed {seed})."),
                                sim = private$.fmtPower(est$power), power = base::format(power, nsmall = 2),
                                seed = est$seed)
                        else if (isTRUE(est$type == "sim"))
                            .fmt(
                                .("A simulation of the recommended goodness-of-fit test gives a power of about {sim} at this sample size, short of the stated {power}; about {needed} subjects reach about {simN} (random seed {seed})."),
                                sim = private$.fmtPower(est$power), power = base::format(power, nsmall = 2),
                                needed = private$.fmtN(est$n), simN = private$.fmtPower(est$power_n),
                                seed = est$seed),
                        if (isTRUE(est$type == "cat"))
                            .fmt(
                                .("With three or more raters and three to five categories, this sample size can fall well short of power {power}; confirm it by simulating the intended analysis."),
                                power = base::format(power, nsmall = 2))),
                        collapse = " ")))
                else ""))

            # Cochran's rule on the expected counts at N: no cell below 1, at most 20% below 5,
            # judged on the cells of the test the N is for. With a binary outcome and 3+ raters
            # that test pools every split call into one disagreement cell (Altaye, Donner & Klar
            # 2001 p.586), so the 'exactly k of n raters' cells are pooled here too; under their
            # eq. (4) model pooling leaves N unchanged. Judging the unpooled cells flagged designs
            # whose pooled cells are comfortable (6 raters, 5% finding: smallest pooled count 6.3,
            # smallest unpooled 0.0003). Two raters and 3-5 categories already use pooled cells.
            # Counts are at the N shown, not the unrounded one: the notes say "at the required sample size",
            # and 0.1 x 25.09 printed as 2.5 beside "26 subjects".
            gof    <- if (outcome == 2L && raters >= 3)
                          c(P0[1], sum(P0[2:raters]), P0[raters + 1]) else P0
            e      <- gof * n_required
            sparse <- is.finite(n_required) && (any(e < 1) || mean(e < 5) > 0.2)

            # Expected count of each OUTCOME category at N -- the quantity the engine's own
            # discarded warning uses. A binary design may have been entered as a single
            # prevalence, so restore the complement before taking the minimum.
            marg     <- if (length(props) == 1) c(props, 1 - props) else props
            marg     <- marg * n_required
            marg_min <- min(marg)
            self$results$notices$setContent(
                private$.buildNotices(n_required, sparse = sparse, outcome = outcome,
                                      raters = raters, kappa0 = kappa0, kappa1 = kappa1,
                                      power = power, sparse_min = min(e),
                                      sparse_below5 = sum(e < 5), sparse_total = length(e),
                                      marg_min = marg_min,
                                      marg_sparse = is.finite(n_required) && marg_min < 5,
                                      est = est))
        })
)
