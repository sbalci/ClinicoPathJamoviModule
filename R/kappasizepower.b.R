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
        #   [testing] add tests/testthat/test-kappasizepower.R - verify against
        #     kappaSize::PowerBinary/Power3Cats/Power4Cats/Power5Cats
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
                    "Further suppose that the prevalence of the categories are ",
                    paste0(props[-length(props)], collapse = ", "),
                    ", and ", props[length(props)], ".")
            }

            paste0(
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
            props <- suppressWarnings(as.numeric(trimws(
                unlist(strsplit(self$options$props, ","), use.names = FALSE))))

            if (length(props) == 0 || anyNA(props))
                jmvcore::reject(
                    "Proportions must be numeric values separated by commas (for example '0.20, 0.80'). One or more entries could not be read as a number.",
                    code = "invalid_props")

            if (any(props <= 0) || any(props >= 1))
                jmvcore::reject(
                    "Each proportion must be strictly between 0 and 1.",
                    code = "props_range")

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

            # --- Validate the kappa relationship ------------------------------
            if (isTRUE(kappa0 == kappa1))
                jmvcore::reject(
                    "kappa0 (null) and kappa1 (alternative) must differ; equal values make the required sample size undefined.",
                    code = "kappa_equal")

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
            self$results$text1$setContent(result)

            summary_text <- paste(utils::capture.output(summary(result)),
                                  collapse = "\n")
            self$results$text_summary$setContent(summary_text)

            self$results$text2$setContent(
                private$.buildExplanation(kappa0, kappa1, alpha, power, raters, props))
        })
)
