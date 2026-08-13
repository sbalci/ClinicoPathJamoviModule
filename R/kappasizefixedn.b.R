#' @title Lowest Expected Value for a fixed sample size
#' @importFrom R6 R6Class
#' @import jmvcore
#' @return An \code{R6} class generator object for the \code{kappaSizeFixedNClass} backend; used internally by the jamovi analysis wrapper and not called directly.


kappaSizeFixedNClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizeFixedNClass",
    inherit = kappaSizeFixedNBase,
    private = list(

        # TODO [meddecide audit 2026-05-14] - see docs/audit/MODULE_AUDIT_REPORT_20260514-1847.md
        #   [i18n] 0 .() wraps; bootstrap jamovi/i18n/ then /prepare-translation kappasizefixedn

        # Split the props string into tokens (comma / semicolon / pipe / whitespace separated).
        # The separator set matches kappaSizeCI's so the same string is accepted by every
        # member of the family; a user moving between them should not have to retype it.
        .parsePropTokens = function(props) {
            toks <- unlist(strsplit(props, "[,;|[:space:]]+"), use.names = FALSE)
            toks[nzchar(toks)]
        },

        # Notes panel. kappaSizeCI carries the same two blocks; this analysis had none, so its
        # only statement of method was the reference list and its only caveat was buried in the
        # raw kappaSize summary text.
        .buildNotices = function(kappaL_val, sparse_cells = FALSE) {
            info <- paste0(
                "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #3c8dbc; background:#f4f8fb;'>",
                "<b>Methodology.</b> With the sample size already fixed, this reports the lower bound ",
                "of the one-sided 100(1 &minus; &alpha;)% confidence interval for Cohen's &kappa; that ",
                "the study can expect to achieve, using the large-sample method implemented in the ",
                "kappaSize package (Rotondi &amp; Donner). It answers &quot;given the subjects I have, ",
                "how little agreement am I still unable to rule out?&quot; &mdash; the mirror image of ",
                "the sample-size question. Note that <b>kappa0 here is the agreement you anticipate ",
                "observing</b>, not a null hypothesis value as it is in kappaSizePower.",
                "</div>"
            )

            warn <- ""

            # kappaSize writes "At least one expected cell count is less than five" into its own
            # summary text when a category is rare at this n. That is a real caveat about the
            # asymptotics the method relies on, and it was reaching only the Summary pane.
            if (isTRUE(sparse_cells)) {
                warn <- paste0(warn,
                    "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #ec971f; background:#fdf7ef;'>",
                    "<b>Sparse categories.</b> At this sample size at least one category is expected to ",
                    "contain fewer than five subjects. The calculation rests on a large-sample ",
                    "approximation, so the bound shown is less dependable here. Consider collapsing ",
                    "rare categories or enrolling more subjects.",
                    "</div>"
                )
            }

            # A bound at or below zero is the clinically decisive case: the study cannot exclude
            # agreement no better than chance, whatever the point estimate turns out to be.
            if (length(kappaL_val) == 1 && is.finite(kappaL_val) && kappaL_val <= 0) {
                warn <- paste0(warn,
                    "<div style='margin:6px 0; padding:8px 10px; border-left:3px solid #d9534f; background:#fdf3f3;'>",
                    "<b>This sample size cannot demonstrate agreement.</b> The expected lower bound is ",
                    signif(kappaL_val, 4), ", at or below zero, so even if the study observes the ",
                    "anticipated kappa it will not be able to rule out agreement no better than chance. ",
                    "Enrol more subjects, use a less extreme category distribution, add raters, or ",
                    "relax the significance level.",
                    "</div>"
                )
            }

            paste0(warn, info)
        },

        # Human-readable list: "0.20 and 0.80" or "0.20, 0.30, and 0.50"
        .formatProps = function(tokens) {
            k <- length(tokens)
            if (k == 0) return("")
            if (k == 1) return(tokens)
            if (k == 2) return(paste(tokens, collapse = " and "))
            paste0(paste(tokens[-k], collapse = ", "), ", and ", tokens[k])
        },

        # Validate all inputs; reject with an actionable message on failure.
        # Kept outside any tryCatch so jmvcore::reject surfaces as a proper
        # jamovi error rather than being swallowed into text output.
        .validateInputs = function(outcome, kappa0, props4, alpha, n) {
            if (!outcome %in% c(2, 3, 4, 5))
                jmvcore::reject("Number of outcome levels must be 2, 3, 4, or 5.", code = NULL)

            if (length(props4) == 0 || any(is.na(props4)))
                jmvcore::reject(
                    "Proportions must be numeric values separated by commas (e.g. '0.20, 0.80').",
                    code = NULL)

            # A binary outcome may be given as one prevalence or as two proportions summing
            # to 1 - kappaSize::FixedNBinary accepts both (it does props <- props[1] after the
            # sum check). This used to demand exactly two, so the same "0.30" that works in
            # kappaSizePower and kappaSizeCI was rejected here.
            # "0,30 0,70" splits into 0, 30, 0, 70 and would be reported as the wrong COUNT of
            # proportions, which sends the user looking in the wrong place. Diagnose the decimal
            # separator first, as the two siblings do.
            if (any(props4 >= 1, na.rm = TRUE)) {
                as_decimal <- suppressWarnings(as.numeric(trimws(unlist(strsplit(
                    gsub("([0-9]),([0-9])", "\\1.\\2", self$options$props),
                    "[;|[:space:]]+")))))
                as_decimal <- as_decimal[!is.na(as_decimal)]
                if (length(as_decimal) > 0 && all(as_decimal > 0 & as_decimal < 1))
                    jmvcore::reject(
                        "Proportions must use a decimal point, not a decimal comma: write 0.30, 0.70 rather than 0,30 0,70.",
                        code = NULL)
            }

            if (outcome == 2) {
                if (!(length(props4) %in% c(1L, 2L)))
                    jmvcore::reject(
                        paste0("For a binary outcome enter either one prevalence value or two ",
                               "proportions that sum to 1 (received ", length(props4), ")."),
                        code = NULL)
            } else if (length(props4) != outcome) {
                jmvcore::reject(
                    paste0("Expected ", outcome, " proportions for ", outcome,
                           " outcome levels, but got ", length(props4),
                           ". Please provide one proportion per outcome level."),
                    code = NULL)
            }

            if (any(props4 <= 0) || any(props4 >= 1))
                jmvcore::reject("Each proportion must be between 0 and 1 (exclusive).", code = NULL)

            # Only meaningful when two or more proportions were given: a single binary
            # prevalence is not supposed to sum to 1.
            # Use the engine's own predicate (abs(sum - 1) >= 0.001) rather than all.equal's
            # <= 0.001, so the module's clearer message is not bypassed at exactly sum = 1.001.
            if (length(props4) >= 2 && abs(sum(props4) - 1) >= 0.001)
                jmvcore::reject(
                    paste0("Proportions must sum to 1. Current sum is ", round(sum(props4), 4), "."),
                    code = NULL)

            # !is.finite covers NA, NaN and Inf. Inf used to pass every clause here
            # (is.na(Inf) FALSE, Inf < 2 FALSE, Inf != round(Inf) FALSE) and then hung the
            # engine: its search loop never terminates because the test statistic is NaN.
            # The floor is 11, not 2: every kappaSize FixedN* engine contains
            # `if (n <= 10) stop("Sorry, your study should enroll at least 10 subjects.")`,
            # so n of 2..10 reached the engine only to come back as a vendor error string.
            if (!is.finite(n) || n != round(n))
                jmvcore::reject(
                    "Sample size (N) must be a whole number.", code = NULL)

            if (n < 11)
                jmvcore::reject(
                    paste0("Sample size (N) must be at least 11. The kappaSize method is a ",
                           "large-sample approximation and its engine refuses any study of 10 ",
                           "or fewer subjects (received ", n, ")."),
                    code = NULL)

            if (is.na(kappa0) || kappa0 <= 0 || kappa0 >= 1)
                jmvcore::reject("kappa0 must be between 0 and 1 (exclusive).", code = NULL)

            # Match the compiled option bounds. The old (0,1) test was a false safety net:
            # an R caller passing alpha = 0.5 reached the engine and got an opaque
            # "missing value where TRUE/FALSE needed".
            if (!is.finite(alpha) || alpha < 0.001 || alpha > 0.20)
                jmvcore::reject(
                    "Significance level (alpha) must be between 0.001 and 0.20.", code = NULL)

            invisible(TRUE)
        },

        .run = function() {

            # Blank the panels first: .validateInputs() rejects before anything is written, so a
            # failed re-run used to leave the PREVIOUS run's numbers on screen with a red error
            # above them. Both siblings clear up front.
            self$results$text1$setContent("")
            self$results$text_summary$setContent("")
            self$results$text2$setContent("")
            self$results$notices$setContent("")

            outcome <- as.numeric(self$options$outcome)
            kappa0  <- self$options$kappa0
            props   <- self$options$props
            raters  <- as.numeric(self$options$raters)
            alpha   <- self$options$alpha
            n       <- self$options$n

            props3 <- private$.parsePropTokens(props)
            props4 <- suppressWarnings(as.numeric(props3))

            # Validate before any computation so failures surface as a clean
            # jamovi error instead of an opaque kappaSize crash / silently
            # truncated proportions.
            private$.validateInputs(outcome, kappa0, props4, alpha, n)

            if (!requireNamespace('kappaSize', quietly = TRUE))
                jmvcore::reject(
                    "The 'kappaSize' package is required but not installed. Install it with install.packages('kappaSize').",
                    code = NULL)

            kappa_fn <- switch(
                as.character(outcome),
                "2" = kappaSize::FixedNBinary,
                "3" = kappaSize::FixedN3Cats,
                "4" = kappaSize::FixedN4Cats,
                "5" = kappaSize::FixedN5Cats
            )

            # Convert any kappaSize error into a readable jamovi message. The
            # reject() runs in the error handler (after tryCatch has returned),
            # so it propagates normally rather than being re-caught.
            result <- tryCatch(
                kappa_fn(
                    kappa0 = kappa0,
                    n      = n,
                    props  = props4,
                    alpha  = alpha,
                    raters = raters
                ),
                error = function(e)
                    jmvcore::reject(
                        paste0("kappaSize could not compute the expected lower bound: ",
                               conditionMessage(e)),
                        code = NULL)
            )

            # Cohen's kappa is bounded below by -1, but kappaSize's search decrements rho from
            # kappa0 by 0.001 with no floor, so an underpowered design walks straight past it:
            # kappa0 = 0.01, n = 11, prevalence 0.02, alpha = 0.001 returns kappaL = -23.78.
            # Printing that as "a lower limit for kappa" is meaningless.
            kappaL_val <- suppressWarnings(as.numeric(result$kappaL))
            if (length(kappaL_val) == 1 && is.finite(kappaL_val) && kappaL_val < -1)
                jmvcore::reject(
                    paste0("The calculation did not converge to a usable answer: it returned a ",
                           "lower bound of ", signif(kappaL_val, 4), ", and Cohen's kappa cannot ",
                           "be below -1. With this combination of sample size, anticipated kappa ",
                           "and category prevalences the large-sample approximation breaks down. ",
                           "Increase N, use a less extreme prevalence, or raise the significance ",
                           "level."),
                    code = NULL)

            prev_txt <- if (outcome == 2 && length(props3) == 1) {
                paste0("Further suppose that the prevalence of the trait is ",
                       private$.formatProps(props3), ".")
            } else {
                paste0("Further suppose that the proportions of the outcome categories are ",
                       private$.formatProps(props3), ".")
            }

            # "determine the expected lower bound for kappa0=0.6" reads as though the bound
            # belonged to kappa0. It does not: kappa0 is the agreement the researchers ANTICIPATE
            # observing, and the bound is the worst case still compatible with it at this n.
            text2 <- paste0(
                "Researchers anticipate an agreement of kappa = ", kappa0,
                " and have access to ", base::format(n, scientific = FALSE),
                " subjects rated by ", raters, " raters.\n",
                prev_txt,
                "\nThey would like to know the lowest value of kappa that the study can expect to\n",
                "rule out - the lower bound of the one-sided ",
                format(100 * (1 - alpha), scientific = FALSE), "% confidence interval.",
                if (length(kappaL_val) == 1 && is.finite(kappaL_val))
                    paste0("\nThe expected lower bound for kappa is ", signif(kappaL_val, 4), ".",
                           if (kappaL_val <= 0)
                               paste0(" A bound at or below zero means this many subjects cannot ",
                                      "rule out agreement no better than chance.")
                           else "")
                else "")

            # capture.output(print(.)) rather than the raw object: setContent then stores a plain
            # string, which is what the other two members of the family store and what serialises
            # predictably. Same visible text either way.
            self$results$text1$setContent(
                paste(utils::capture.output(print(result)), collapse = "\n"))
            self$results$text2$setContent(text2)

            summary_text <- paste(utils::capture.output(summary(result)), collapse = "\n")
            self$results$text_summary$setContent(summary_text)

            self$results$notices$setContent(private$.buildNotices(
                kappaL_val,
                sparse_cells = grepl("expected cell count is less than five", summary_text,
                                     fixed = TRUE)))
        })
)
