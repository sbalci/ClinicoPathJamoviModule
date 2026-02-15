#' @title Power Approach for the Number of Subjects Required
#' @importFrom R6 R6Class
#' @import jmvcore


kappaSizePowerClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizePowerClass",
    inherit = kappaSizePowerBase,
    private = list(
        .run = function() {

            outcome <- self$options$outcome
            kappa0 <- self$options$kappa0
            kappa1 <- self$options$kappa1
            props  <- self$options$props
            raters  <- self$options$raters
            alpha  <- self$options$alpha
            power  <- self$options$power

            props2 <- strsplit(props, ",")
            props3 <- unlist(props2, use.names = FALSE)
            props4 <- as.numeric(props3)

            result <- NULL

            if (outcome == 2) {

            result <-
            kappaSize::PowerBinary(
                kappa0 = kappa0,
                kappa1 = kappa1,
                props = props4,
                raters = raters,
                alpha = alpha,
                power = power
                )

            text2 <- paste0(
                "Researchers would like to determine the required sample size to test","\n",
                "kappa0=", kappa0,
                " vs.",
                " kappa1=", kappa1,
                " with alpha=",alpha,
                " and power=", power,"\n",
                "in a study of interobserver agreement with ",
                raters, " raters. ","\n",
                "Further suppose that the prevalence of the trait is ",
                props3,
                ".",
                sep = " ")


            self$results$text1$setContent(result)
            self$results$text2$setContent(text2[1])

        }


            if (outcome == 3) {


                result <-
                    kappaSize::Power3Cats(
                        kappa0 = kappa0,
                        kappa1 = kappa1,
                        props = props4,
                        raters = raters,
                        alpha = alpha,
                        power = power
                        )


                text2 <- paste0(
                    "Researchers would like to determine the required sample size to test","\n",
                    "kappa0=", kappa0,
                    " vs.",
                    " kappa1=", kappa1,
                    " with alpha=",alpha,
                    " and power=", power,"\n",
                    "in a study of interobserver agreement with ",
                    raters, " raters. ","\n",
                    "Further suppose that the prevalence of the categories are ",
                    props3[1], "," , props3[2], ", and ", props3[3],
                    ".",
                    sep = " ")

                self$results$text1$setContent(result)
                self$results$text2$setContent(text2[1])


                            }


            if (outcome == 4) {

                result <-
                    kappaSize::Power4Cats(
                        kappa0 = kappa0,
                        kappa1 = kappa1,
                        props = props4,
                        raters = raters,
                        alpha = alpha,
                        power = power
                        )

                text2 <- paste0(
                    "Researchers would like to determine the required sample size to test","\n",
                    "kappa0=", kappa0,
                    " vs.",
                    " kappa1=", kappa1,
                    " with alpha=",alpha,
                    " and power=", power,"\n",
                    "in a study of interobserver agreement with ",
                    raters, " raters. ","\n",
                    "Further suppose that the prevalence of the categories are ",

                    props3[1], "," , props3[2], "," ,props3[3], ", and ", props3[4],
                    ".",
                    sep = " ")

                self$results$text1$setContent(result)
                self$results$text2$setContent(text2[1])

                }


            if (outcome == 5) {

                result <-
                    kappaSize::Power5Cats(
                        kappa0 = kappa0,
                        kappa1 = kappa1,
                        props = props4,
                        raters = raters,
                        alpha = alpha,
                        power = power
                        )

                text2 <- paste0(
                    "Researchers would like to determine the required sample size to test","\n",
                    "kappa0=", kappa0,
                    " vs.",
                    " kappa1=", kappa1,
                    " with alpha=",alpha,
                    " and power=", power, "\n",
                    "in a study of interobserver agreement with ",
                    raters, " raters. ","\n",
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
