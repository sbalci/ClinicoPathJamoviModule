#' Correlation Analysis
#'


#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %$%
#'


correlationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "correlationClass",
    inherit = correlationBase,
    private = list(
        .run = function() {

            # TODO

            todo <- glue::glue(
                "This Module is still under development
                - Correlation matrix via ggplot
                -
                "
            )

            self$results$todo$setContent(todo)

            ####

            if (length(self$options$vars) < 2)
                return()


            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Correlation


            mydata <- self$data

            formula <- jmvcore::constructFormula(terms = self$options$vars)

            myvars <- jmvcore::decomposeFormula(formula = formula)

            myvars <- unlist(myvars)


            cor1 <- mydata %>%
                select(myvars) %>%
                correlation::correlation(.)

            self$results$text1$setContent(cor1)



            # corx <- mydata %>%
            #     dplyr::select(myvars) %$%
            #     stats::cor.test(method = "spearman", exact = FALSE) %>%
            #     report::report()

            # cor2 <- cor1 %>%
            #     report::report(.)


            # self$results$text2$setContent(cor2)








            # self$results$text3$setContent(cor3)



            # ggplot(mydata, aes(x = PeritumoralTomurcukSayi, y = Yas)) +
            #     geom_point() +
            #     geom_smooth(method = lm, size = 1)
            # mydata %>%
            #     ggstatsplot::ggscatterstats(x = PeritumoralTomurcukSayi, y = Yas, type = "np")

#
#             iris %>%
#                 select(Species, starts_with("Sepal")) %>%
#                 group_by(Species) %>%
#                 correlation::correlation() %>%
#                 filter(r < 0.9)
#
#             correlation::correlation(select(iris, Species, starts_with("Sepal")),
#                                      select(iris, Species, starts_with("Petal")),
#                                      partial=TRUE)
#
#             correlation(iris, bayesian=TRUE)
#
#
#             library(report)
#             iris %>%
#                 select(starts_with("Sepal")) %>%
#                 correlation::correlation(bayesian=TRUE) %>%
#                 report()
#
#
#             report::report(cor.test(iris$Sepal.Length, iris$Petal.Length))
#
#
#             # https://stat.ethz.ch/R-manual/R-patched/library/stats/html/cor.test.html
#
#
#             iris %>%
#                 group_by(Species) %>%
#                 correlation() %>%
#                 report() %>%
#                 to_table()
#
#             iris %>% explore(Sepal.Length, Petal.Length)
#
#             iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
#             iris %>% explore(Sepal.Length, Petal.Length, target = is_versicolor)
#
#
#             dlookr::correlate(carseats)
#             dlookr::correlate(carseats, Sales, CompPrice, Income)
#             dlookr::correlate(carseats, Sales:Income)
#             dlookr::correlate(carseats, -(Sales:Income))
#             carseats %>%
#                 dlookr::correlate(Sales:Income) %>%
#                 dplyr::filter(as.integer(var1) > as.integer(var2))
#             carseats %>%
#                 dplyr::filter(ShelveLoc == "Good") %>%
#                 group_by(Urban, US) %>%
#                 dlookr::correlate(Sales) %>%
#                 dplyr::filter(abs(coef_corr) > 0.5)
#
#
#             dlookr::plot_correlate(carseats)
#             dlookr::plot_correlate(carseats, Sales, Price)
#             carseats %>%
#                 dplyr::filter(ShelveLoc == "Good") %>%
#                 dplyr::group_by(Urban, US) %>%
#                 dlookr::plot_correlate(Sales)
#
#
#             SmartEDA::ExpNumStat(
#                 Carseats,
#                 by = "A",
#                 gp = "Price",
#                 Qnt = seq(0, 1, 0.1),
#                 MesofShape = 1,
#                 Outlier = TRUE,
#                 round = 2
#             )
#
#
#             # https://alastairrushworth.github.io/inspectdf/articles/pkgdown/inspect_cor_exampes.html
#             inspectdf::inspect_cor(storms)
#
#             inspectdf::inspect_cor(storms) %>% inspectdf::show_plot()
#
#             inspectdf::inspect_cor(storms, storms[-c(1:200), ])
#
#             inspectdf::inspect_cor(storms, storms[-c(1:200), ]) %>%
#                 slice(1:20) %>%
#                 inspectdf::show_plot()
#
#

#
#
#             ggplot(mydata, aes(x = tx_zamani_verici_yasi, y = trombosit)) +
#                 geom_point() +
#                 geom_smooth(method = lm, size = 1)
#
#
#
# # https://easystats.github.io/report/articles/interpret_metrics.html
#
#             mydata %>%
#                 select(continiousVariables,
#                        -dateVariables) %>%
#                 visdat::vis_cor()


            ####


            # cor <- self$data %>%
            #     select(myvars) %>%
            #     psycho::correlation(.)


            # cor1 <- summary(cor) %>%
            #     knitr::kable(format = "latex") %>%
            #     kableExtra::kable_styling(latex_options="scale_down")



            # cor2 <- print(cor)

            # cor3 <- cor %>%
            #     report::to_values()


            # Prepare Plot Data

            plotData <- self$data %>%
                dplyr::select(self$options$vars)

            image <- self$results$plot

            image$setState(plotData)


        },
        .plot = function(image, ...) {  # <-- the plot function

            if (length(self$options$vars) < 2)
                return()

            plotData <- image$state

            plot <- plot(plotData)

            print(plot)
            TRUE
        },
.plot2 = function(image, ...) {  # <-- the plot2 function

    if (length(self$options$vars) < 2)
        return()

    plotData <- image$state


    plot2 <- ggplot(plotData) +
        geom_point() +
        geom_smooth(method = lm, size = 1)


    print(plot2)
    TRUE
}

)
)
