#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
# This file is a generated template, your changes will not be overwritten

correlationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "correlationClass",
    inherit = correlationBase,
    private = list(
        .run = function() {



            #########




            # Correlation

            suppressPackageStartupMessages(library(psycho))

            cor <- mydata %>%
            select(t_pdl1, i_pdl1) %>%
            correlation()

            summary(cor) %>%
                knitr::kable(format = "latex") %>%
                kableExtra::kable_styling(latex_options="scale_down")

            plot(cor)




            print(cor)



            cor %>%
                report::to_values()



            ggplot(mydata, aes(x = t_pdl1, y = i_pdl1)) +
                geom_point() +
                geom_smooth(method = lm, size = 1)
            # size sayıya göre olsun











            ###############









            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
