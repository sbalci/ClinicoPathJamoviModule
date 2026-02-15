#' @title Lowest Expected Value for a fixed sample size
#' @importFrom R6 R6Class
#' @import jmvcore


kappaSizeFixedNClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizeFixedNClass",
    inherit = kappaSizeFixedNBase,
    private = list(
        .run = function() {


            outcome <- self$options$outcome
            kappa0 <- self$options$kappa0
            props  <- self$options$props
            raters  <- self$options$raters
            alpha  <- self$options$alpha
            n  <- self$options$n

            props2 <- strsplit(props, ",")
            props3 <- unlist(props2, use.names = FALSE)
            props4 <- as.numeric(props3)

            result <- NULL

            if (outcome == 2) {

                result <-
                kappaSize::FixedNBinary(
                    kappa0=kappa0,
                    n=n,
                    props=props4,
                    alpha=alpha,
                    raters=raters
                    )

                text2 <- paste0(
                    "Researchers would like to determine the expected lower bound for","\n",
                    "kappa0=", kappa0,
                    " assuming they have access to ", n, " subjects and ", raters, " raters.", "\n",
                    "Further suppose that the prevalence of the trait is ",
                    props3,
                    ".",
                    sep = " ")


                self$results$text1$setContent(result)
                self$results$text2$setContent(text2[1])


            }


            if (outcome == 3) {

                result <-
                    kappaSize::FixedN3Cats(
                        kappa0=kappa0,
                        n=n,
                        props=props4,
                        alpha=alpha,
                        raters=raters
                        )


                text2 <- paste0(
                    "Researchers would like to determine the expected lower bound for","\n",
                    "kappa0=", kappa0,
                    " assuming they have access to ", n, " subjects and ", raters, " raters.", "\n",
                "Further suppose that the prevalence of the categories are ",
                props3[1], "," , props3[2], ", and ", props3[3],
                ".",
                sep = " ")


                self$results$text1$setContent(result)
                self$results$text2$setContent(text2[1])


            }


            if (outcome == 4) {

                result <-
                    kappaSize::FixedN4Cats(
                        kappa0=kappa0,
                        n=n,
                        props=props4,
                        alpha=alpha,
                        raters=raters
                        )


                text2 <- paste0(
                    "Researchers would like to determine the expected lower bound for","\n",
                    "kappa0=", kappa0,
                    " assuming they have access to ", n, " subjects and ", raters, " raters.", "\n",
                    "Further suppose that the prevalence of the categories are ",

                    props3[1], "," , props3[2], "," ,props3[3], ", and ", props3[4],
                    ".",
                    sep = " ")

                self$results$text1$setContent(result)
                self$results$text2$setContent(text2[1])


            }


            if (outcome == 5) {

                result <-
                    kappaSize::FixedN5Cats(
                        kappa0=kappa0,
                        n=n,
                        props=props4,
                        alpha=alpha,
                        raters=raters
                        )

                text2 <- paste0(
                    "Researchers would like to determine the expected lower bound for","\n",
                    "kappa0=", kappa0,
                    " assuming they have access to ", n, " subjects and ", raters, " raters.", "\n",
                    "Further suppose that the prevalence of the categories are ",
                    props3[1], "," , props3[2], "," , props3[3], "," , props3[4], ", and ", props3[5],
                    ".",
                    sep = " ")

                self$results$text1$setContent(result)
                self$results$text2$setContent(text2[1])


            }

            # Set summary output from kappaSize
            if (!is.null(result)) {
                summary_text <- paste(utils::capture.output(summary(result)), collapse = "\n")
                self$results$text_summary$setContent(summary_text)
            }


        })
)
