#' @title Lowest Expected Value for a fixed sample size
#' @importFrom R6 R6Class
#' @import jmvcore
#' @return An \code{R6} class generator object for the \code{kappaSizeFixedNClass} backend; used internally by the jamovi analysis wrapper and not called directly.


kappaSizeFixedNClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizeFixedNClass",
    inherit = kappaSizeFixedNBase,
    private = list(

        # TODO [meddecide audit 2026-05-14] - see docs/audit/MODULE_AUDIT_REPORT_20260514-1847.md
        #   [hygiene/notices] input validation + jmvcore::reject now guard malformed props /
        #     n / alpha before the kappaSize call; consider adding an INFO methodology summary
        #   [i18n] 0 .() wraps; bootstrap jamovi/i18n/ then /prepare-translation kappasizefixedn
        #   [testing] no tests/testthat/test-kappasizefixedn.R - verify against
        #     kappaSize::FixedNBinary/FixedN3Cats/FixedN4Cats/FixedN5Cats

        # Split the props string into tokens (comma / semicolon / whitespace separated)
        .parsePropTokens = function(props) {
            toks <- unlist(strsplit(props, "[,;[:space:]]+"), use.names = FALSE)
            toks[nzchar(toks)]
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
        .validateInputs = function(outcome, kappa0, props4, raters, alpha, n) {
            if (!outcome %in% c(2, 3, 4, 5))
                jmvcore::reject("Number of outcome levels must be 2, 3, 4, or 5.", code = NULL)

            if (length(props4) == 0 || any(is.na(props4)))
                jmvcore::reject(
                    "Proportions must be numeric values separated by commas (e.g. '0.20, 0.80').",
                    code = NULL)

            if (length(props4) != outcome)
                jmvcore::reject(
                    paste0("Expected ", outcome, " proportions for ", outcome,
                           " outcome levels, but got ", length(props4),
                           ". Please provide one proportion per outcome level."),
                    code = NULL)

            if (any(props4 <= 0) || any(props4 >= 1))
                jmvcore::reject("Each proportion must be between 0 and 1 (exclusive).", code = NULL)

            if (!isTRUE(all.equal(sum(props4), 1, tolerance = 1e-3)))
                jmvcore::reject(
                    paste0("Proportions must sum to 1. Current sum is ", round(sum(props4), 4), "."),
                    code = NULL)

            if (is.na(n) || n < 2 || n != round(n))
                jmvcore::reject(
                    "Sample size (N) must be a whole number of at least 2.", code = NULL)

            if (is.na(kappa0) || kappa0 <= 0 || kappa0 >= 1)
                jmvcore::reject("kappa0 must be between 0 and 1 (exclusive).", code = NULL)

            if (is.na(alpha) || alpha <= 0 || alpha >= 1)
                jmvcore::reject(
                    "Significance level (alpha) must be between 0 and 1 (exclusive).", code = NULL)

            invisible(TRUE)
        },

        .run = function() {

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
            private$.validateInputs(outcome, kappa0, props4, raters, alpha, n)

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

            text2 <- paste0(
                "Researchers would like to determine the expected lower bound for\n",
                "kappa0=", kappa0,
                " assuming they have access to ", n, " subjects and ", raters, " raters.\n",
                "Further suppose that the proportions of the outcome categories are ",
                private$.formatProps(props3), ".")

            self$results$text1$setContent(result)
            self$results$text2$setContent(text2)

            # Set summary output from kappaSize
            if (!is.null(result)) {
                summary_text <- paste(utils::capture.output(summary(result)), collapse = "\n")
                self$results$text_summary$setContent(summary_text)
            }


        })
)
