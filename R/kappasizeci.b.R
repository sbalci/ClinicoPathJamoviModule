#' @title Confidence Interval Approach for the Number of Subjects Required
#' @importFrom R6 R6Class
#' @import jmvcore


kappaSizeCIClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaSizeCIClass",
    inherit = kappaSizeCIBase,
    private = list(
        .run = function() {


            outcome <- self$options$outcome
            kappa0 <- self$options$kappa0
            kappaL <- self$options$kappaL
            kappaU <- self$options$kappaU
            props  <- self$options$props
            raters  <- self$options$raters
            alpha  <- self$options$alpha

            props2 <- strsplit(props, ",")
            props3 <- unlist(props2, use.names = FALSE)
            props4 <- as.numeric(props3)

            if (outcome == 2) {

                text1 <-
                    kappaSize::CIBinary(
                        kappa0 = kappa0,
                        kappaL = kappaL,
                        kappaU = kappaU,
                        props = props4,
                        alpha = alpha,
                        raters = raters
                    )


                text2 <- paste0(
                    "Researchers would like to determine the required sample size to test","\n",
                    "kappa0=", kappa0,
                    " with precision of [kappaL=", kappaL, " - kappaU=", kappaU, "]",
                    " in a study of interobserver agreement with","\n",
                    raters, " raters. ",
                    "Further suppose that the prevalence of the trait is ",
                    props3,
                    ".",
                    sep = " ")


            self$results$text1$setContent(text1)
            self$results$text2$setContent(text2[1])


            }


            if (outcome == 3) {

                text1 <-
                    kappaSize::CI3Cats(
                        kappa0 = kappa0,
                        kappaL = kappaL,
                        kappaU = kappaU,
                        props = props4,
                        alpha = alpha,
                        raters = raters
                        )


                text2 <- paste0(
                    "Researchers would like to determine the required sample size to test","\n",
                    "kappa0=", kappa0,
                    " with precision of [kappaL=", kappaL, " - kappaU=", kappaU, "]",
                    " in a study of interobserver agreement with","\n",
                    raters, " raters. ",
                    "Further suppose that the prevalence of the categories are ",
                    props3[1], "," , props3[2], ", and ", props3[3],
                    ".",
                    sep = " ")


                self$results$text1$setContent(text1)
                self$results$text2$setContent(text2[1])


            }


            if (outcome == 4) {

                text1 <-
                    kappaSize::CI4Cats(
                        kappa0 = kappa0,
                        kappaL = kappaL,
                        kappaU = kappaU,
                        props = props4,
                        alpha = alpha,
                        raters = raters
                        )


                text2 <- paste0(
                    "Researchers would like to determine the required sample size to test","\n",
                    "kappa0=", kappa0,
                    " with precision of [kappaL=", kappaL, " - kappaU=", kappaU, "]",
                    " in a study of interobserver agreement with","\n",
                    raters, " raters. ",
                    "Further suppose that the prevalence of the categories are ",

                    props3[1], "," , props3[2], "," ,props3[3], ", and ", props3[4],
                    ".",
                    sep = " ")


                    self$results$text1$setContent(text1)
                    self$results$text2$setContent(text2[1])


                }


                    if (outcome == 5) {

                        text1 <-
                            kappaSize::CI5Cats(
                                kappa0 = kappa0,
                                kappaL = kappaL,
                                kappaU = kappaU,
                                props = props4,
                                alpha = alpha,
                                raters = raters
                                )


                        text2 <- paste0(
                            "Researchers would like to determine the required sample size to test","\n",
                            "kappa0=", kappa0,
                            " with precision of [kappaL=", kappaL, " - kappaU=", kappaU, "]",
                            " in a study of interobserver agreement with","\n",
                            raters, " raters. ",
                            "Further suppose that the prevalence of the categories are ",

                            props3[1], "," , props3[2], "," , props3[3], "," , props3[4], ", and ", props3[5],
                            ".",
                            sep = " ")


                        self$results$text1$setContent(text1)
                        self$results$text2$setContent(text2[1])


                    }




        })
)
