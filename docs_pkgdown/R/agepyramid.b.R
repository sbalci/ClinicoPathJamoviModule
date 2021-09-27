#' @title Age Pyramid
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'

# https://stackoverflow.com/questions/14680075/simpler-population-pyramid-in-ggplot2

agepyramidClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "agepyramidClass",
    inherit = agepyramidBase,
    private = list(
        .run = function() {


            # Error Message ----
            if ( (is.null(self$options$age) || is.null(self$options$gender)) )
                return()

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")


            # Read data ----

            mydata <- self$data

            age <- self$options$age

            gender <- self$options$gender

            mydata <- jmvcore::select(mydata, c(age, gender))

            mydata <- jmvcore::naOmit(mydata)

            mydata[["Age"]] <- jmvcore::toNumeric(mydata[[age]])

            mydata[["Gender"]] <- as.factor(mydata[[gender]])

            mydata <- mydata %>%
                dplyr::mutate(
                    Gender2 = dplyr::case_when(
                        Gender == self$options$female ~ "Female",
                        TRUE ~ "Male"
                        ))


            mydata[["Pop"]] <- cut(mydata[["Age"]],
                                 include.lowest = TRUE,
                                 right = TRUE,
                                 breaks = c(
                                     seq(from = 0,
                                         to = max(mydata[["Age"]], na.rm = TRUE),
                                         by = 5
                                     ),
                                     max(mydata[["Age"]], na.rm = TRUE)
                                 ),
                                 ordered_result = TRUE
            )


            plotData <- mydata %>%
                dplyr::select(Gender = Gender2,
                              Pop
                              ) %>%
                dplyr::group_by(Gender, Pop) %>%
                dplyr::count()

            image <- self$results$plot
            image$setState(plotData)



            plotData2 <- plotData %>%
                tidyr::pivot_wider(data = .,
                                   names_from = Gender,
                                   values_from = n) %>%
                dplyr::arrange(dplyr::desc(Pop))



            plotData2 <- as.data.frame(plotData2) %>%
                tibble::rownames_to_column(.data = .) %>%
                dplyr::filter(!is.na(Pop)) %>%
                dplyr::mutate(
                    Pop = as.character(Pop)
                )



            pyramidTable <- self$results$pyramidTable

            data_frame <- plotData2
            for(i in seq_along(data_frame[,1,drop=T])) {
                pyramidTable$addRow(rowKey = i, values = c(data_frame[i,]))
            }



        }

        ,
        .plot = function(image, ggtheme, theme, ...) {


            # Error Message ----
            if ( (is.null(self$options$age) || is.null(self$options$gender)) )
                return()

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")


            # read data ----

            plotData <- image$state

            plot <- ggplot2::ggplot(data = plotData,
                           mapping = ggplot2::aes(
                               x = Pop,
                               y = ifelse(
                                   test = Gender == "Female",
                                   yes = -n,
                                   no = n
                               ),
                               fill = Gender
                           )) +
                ggplot2::geom_col() +
                ggplot2::coord_flip() +
                ggplot2::scale_y_continuous(labels = abs,
                                            limits = max(plotData$n) * c(-1, 1)
                                            ) +
                ggplot2::labs(x = "Age",
                              y = "Population"
                              )


         plot <- plot + ggtheme

         print(plot)
         TRUE

        }


        )
)
