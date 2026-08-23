#' @title Power Approach for the Number of Subjects Required
#' @importFrom R6 R6Class
#' @import jmvcore
#' @return An \code{R6} class generator object for the \code{kappaSizePowerClass} backend; used internally by the jamovi analysis wrapper and not called directly.


kappaSizePowerClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizePowerClass",
    inherit = kappaSizePowerBase,
    private = list(

        # TODO [meddecide audit 2026-05-14] - remaining tracked items:
        #   [i18n] 0 .() wraps; bootstrap jamovi/i18n/ then /prepare-translation kappasizepower
        # Resolved 2026-07-15 (autofix): input validation + jmvcore::reject error handling;
        #   4 duplicate outcome branches collapsed into one switch-based path;
        #   paste0(sep=) misuse and binary-branch proportion drop fixed.

        # Build the study-explanation sentence (generic across cardinalities).
        .buildExplanation = function(kappa0, kappa1, alpha, power, raters, props) {
            if (length(props) <= 2) {
                prev <- paste0(
                    "Further suppose that the prevalence of the trait is ",
                    props[1], ".")
            } else {
                prev <- paste0(
                    "Further suppose that the prevalences of the categories are ",
                    paste0(props[-length(props)], collapse = ", "),
                    " and ", props[length(props)], ".")
            }

            paste0(
                "This is a POWER calculation: it returns the number of subjects needed to ",
                "REJECT kappa0 in favour of kappa1 at the stated significance and power. ",
                "It answers a different question from the confidence-interval approach ",
                "(kappaSizeCI), which sizes a study to achieve a target interval width, so the ",
                "two will not agree on a sample size for the same study \u{2014} pick the one that ",
                "matches how the result will be reported.\n\n",
                "Researchers would like to determine the required sample size to test\n",
                "kappa0=", kappa0, " vs. kappa1=", kappa1,
                " with alpha=", alpha, " and power=", power, "\n",
                "in a study of interobserver agreement with ", raters, " raters.\n",
                prev)
        },

        .run = function() {

            outcome <- as.integer(self$options$outcome)
            kappa0  <- self$options$kappa0
            kappa1  <- self$options$kappa1
            raters  <- as.integer(self$options$raters)
            alpha   <- self$options$alpha
            power   <- self$options$power

            # Clear any prior content so stale results never survive a rejected run.
            self$results$text1$setContent("")
            self$results$text_summary$setContent("")
            self$results$text2$setContent("")

            # --- Parse and validate the proportions ---------------------------
            # Split on commas, semicolons or whitespace. This used to accept commas only, so
            # "0.30 0.70" -- which the sibling kappaSizeFixedN accepts happily -- was rejected
            # here. Three analyses in one menu taking the same field in three different formats
            # is a trap; this matches kappaSizeFixedN's parser (R/kappasizefixedn.b.R:21).
            toks <- unlist(strsplit(self$options$props, "[,;|[:space:]]+"), use.names = FALSE)
            toks <- toks[nzchar(trimws(toks))]
            props <- suppressWarnings(as.numeric(trimws(toks)))

            if (length(props) == 0 || anyNA(props))
                jmvcore::reject(
                    "Proportions must be numbers separated by commas, semicolons or spaces (for example '0.20, 0.80'). One or more entries could not be read as a number. Note that a decimal comma is not recognised - use a point, as in 0.20.",
                    code = "invalid_props")

            if (any(props <= 0) || any(props >= 1)) {
                # "0,30 0,70" splits into 0, 30, 0, 70 and then fails the range check, which
                # tells a user with a European keyboard that their proportions are out of range
                # rather than that the decimal separator is wrong. Detect that case by re-reading
                # the string with the comma as a decimal point and seeing whether it becomes
                # valid; only then is it really a decimal-separator problem.
                as_decimal <- suppressWarnings(as.numeric(trimws(unlist(
                    strsplit(gsub("([0-9]),([0-9])", "\\1.\\2", self$options$props),
                             "[;[:space:]]+"), use.names = FALSE))))
                as_decimal <- as_decimal[!is.na(as_decimal)]
                if (length(as_decimal) > 0 && all(as_decimal > 0 & as_decimal < 1))
                    jmvcore::reject(
                        "Proportions must use a decimal point, not a decimal comma: write 0.30, 0.70 rather than 0,30 0,70.",
                        code = "props_decimal_comma")

                jmvcore::reject(
                    "Each proportion must be strictly between 0 and 1.",
                    code = "props_range")
            }

            # kappaSize::PowerBinary accepts a single prevalence or two proportions
            # summing to 1; the 3/4/5-category engines require exactly N proportions.
            if (outcome == 2) {
                if (!(length(props) %in% c(1L, 2L)))
                    jmvcore::reject(
                        "For a binary outcome enter either one prevalence value or two proportions that sum to 1.",
                        code = "props_count_mismatch")
            } else if (length(props) != outcome) {
                jmvcore::reject(
                    paste0("Enter exactly ", outcome, " proportions for ", outcome,
                           " outcome levels (received ", length(props), ")."),
                    code = "props_count_mismatch")
            }

            if (length(props) >= 2 && abs(sum(props) - 1) >= 0.001)
                jmvcore::reject(
                    paste0("Proportions must sum to 1 (current sum = ",
                           round(sum(props), 4), ")."),
                    code = "props_sum")

            # --- Validate alpha against power ---------------------------------
            # kappaSize's root finder (kappaSize:::.hichi) does not converge when the
            # significance level is at or above the target power: it loops indefinitely and
            # never returns. Verified with alpha = 0.90, power = 0.20, which was still running
            # after 60 seconds and could not be interrupted, while alpha = 0.05, power = 0.80
            # returns instantly. In jamovi that freezes the analysis with no way to recover, so
            # the combination has to be refused before the engine is entered. It is not a
            # meaningful study design either: power at or below the type I error rate means the
            # test is no better than deciding at random.
            if (!is.finite(alpha) || !is.finite(power))
                jmvcore::reject(
                    "Significance level and power must both be numbers.",
                    code = "alpha_power_nonfinite")

            if (alpha >= power)
                jmvcore::reject(
                    paste0("The significance level (", alpha, ") must be below the power (",
                           power, "). A study whose power does not exceed its type I error rate ",
                           "provides no evidence, and the sample size cannot be computed for it. ",
                           "Conventional values are alpha 0.05 and power 0.80."),
                    code = "alpha_ge_power")

            # --- Validate the kappa relationship ------------------------------
            if (isTRUE(kappa0 == kappa1))
                jmvcore::reject(
                    "kappa0 (null) and kappa1 (alternative) must differ; equal values make the required sample size undefined.",
                    code = "kappa_equal")

            # kappa1 below kappa0 is accepted by the engine and returns a number, but it asks a
            # different question: how many subjects to show agreement is WORSE than the null.
            # That is occasionally intended and more often a transposition, and the returned n
            # differs from the one for the mirrored alternative, so say which way round it read.
            kappa_note <- if (kappa1 < kappa0) {
                paste0(
                    "\nNote: the alternative kappa (", kappa1, ") is BELOW the null (", kappa0,
                    "). This sizes a study to demonstrate that agreement is WORSE than the null ",
                    "value, not better. If you meant to detect an improvement, swap the two ",
                    "values \u{2014} the required sample size is not the same either way.")
            } else {
                ""
            }

            # --- Select the engine for the chosen cardinality -----------------
            powerFun <- switch(as.character(outcome),
                "2" = kappaSize::PowerBinary,
                "3" = kappaSize::Power3Cats,
                "4" = kappaSize::Power4Cats,
                "5" = kappaSize::Power5Cats,
                jmvcore::reject(
                    "Number of outcome levels must be 2, 3, 4, or 5.",
                    code = "invalid_outcome"))

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
                        paste0("Sample size calculation failed: ", conditionMessage(e)),
                        code = "kappasize_error"))

            # --- Populate outputs ---------------------------------------------
            # print() gives the headline sentence ("A minimum of N subjects ..."); summary()
            # repeats it and adds the study parameters. Rendering the object itself into text1
            # dispatched the same print method, so the two panels were near-duplicates and the
            # 5-category case showed the identical "expected cell count" warning ten times.
            result_text <- paste(utils::capture.output(print(result)), collapse = "\n")
            self$results$text1$setContent(result_text)

            summary_text <- paste(utils::capture.output(summary(result)),
                                  collapse = "\n")
            self$results$text_summary$setContent(summary_text)

            # Put the computed answer in the explanation too, so the sentence a user copies into
            # a protocol carries the number it is explaining.
            n_required <- suppressWarnings(ceiling(as.numeric(result$N)))
            self$results$text2$setContent(paste0(
                private$.buildExplanation(kappa0, kappa1, alpha, power, raters, props),
                if (is.finite(n_required))
                    paste0("\nThe required sample size is ", n_required, " subjects.")
                else "",
                kappa_note))
        })
)
