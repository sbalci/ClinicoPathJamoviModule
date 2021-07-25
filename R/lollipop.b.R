
# This file is a generated template, your changes will not be overwritten

lollipopClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "lollipopClass",
    inherit = lollipopBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)



            # https://www.r-graph-gallery.com/lollipop-plot.html


            # plotData <- self$data

            # image <- self$results$plot
            # image$setState(plotData)




        }


        ,
        .plot = function(image, ggtheme, theme, ...) {


            # Error Message ----
            if ( (is.null(self$options$dep) || is.null(self$options$group)) )
                return()

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")


            # read data ----

            # plotData <- image$state


            plotData <- self$data

            group <- self$options$group
            dep <- self$options$dep

            plot <-
            plotData



            # plot <- plot + ggtheme

            print(plot)
            TRUE

        }

        )
)




# # # https://link.springer.com/article/10.1007/s00383-020-04666-4/figures/1
#
# # https://media.springernature.com/full/springer-static/image/art%3A10.1007%2Fs00383-020-04666-4/MediaObjects/383_2020_4666_Fig1_HTML.png
#
#
# library(ggplot2)
# library(dplyr)
# library(hrbrthemes)
# library(readxl)
#
# torsiyon <- read_excel(here::here("data/perinatal torsion hastalar ekim 2019.xls"))
#
# torsiyon <- torsiyon %>%
#     select(recognition = `farkedilme yaşı gün`,
#            admittance = `başvuru yaşı`,
#            operation = `ameliyat yaşı`,
#            basvurugap = `başvuru gap`,
#            ameliyatgap = `ameliyat gap`) %>%
#     arrange( desc(recognition), basvurugap, ameliyatgap)
#
#
# torsiyon <- torsiyon %>%
#     tibble::rownames_to_column(var = "patient")
#
# torsiyon$patient <- as.numeric(torsiyon$patient)
# ## Reordering torsiyon$patient
# torsiyon$patient <- factor(torsiyon$patient, levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32"))
#
#
#
# color_recognition <- rgb(0.3,0.9,0.2,0.4)
# color_admittance <- rgb(0.2,0.2,0.9,0.4)
# color_operation <- rgb(0.9,0.2,0.2,0.4)
#
# mean_recognition <- mean(torsiyon$recognition)
# sd_recognition <- sd(torsiyon$recognition)
#
# mean_admittance <- mean(torsiyon$admittance)
#
# mean_operation <- mean(torsiyon$operation)
#
#
#
# # Plot
# ggplot(torsiyon) +
#     geom_segment(
#         aes(x = patient,
#             xend = patient,
#             y = recognition,
#             yend = admittance
#         ),
#         color = "grey50",
#         size = 0.5,
#         linetype = "dashed"
#     ) +
#     geom_segment(
#         aes(x = patient,
#             xend = patient,
#             y = admittance,
#             yend = operation
#         ),
#         color = "grey50",
#         size = 0.5
#     ) +
#     geom_point(
#         aes(x = patient, y = recognition),
#         color = color_recognition,
#         size = 5
#         # shape = 3
#     ) +
#     geom_point(
#         aes(x = patient, y = admittance),
#         color = color_admittance,
#         size = 3
#     ) +
#     geom_point(
#         aes(x = patient, y = operation),
#         color = color_operation,
#         size = 2
#     ) +
#     coord_flip() +
#     # theme_modern_rc() +
#     # theme_ipsum() +
#     theme_light() +
#     theme(panel.background = element_blank(),
#           panel.grid.major = element_line(colour = "grey90")
#     ) +
#
#     annotate("rect",
#              xmin = 20, xmax = 24, ymin = 25, ymax = 34,
#              color = "grey",
#              fill = "white"
#
#     ) +
#
#
#     geom_point(
#         aes(x = 23, y = 26),
#         color = color_recognition,
#         size = 2
#     ) +
#     annotate("text", label = "Recognition", x = 23, y = 27,
#              color = color_recognition,
#              size = 4,
#              hjust = 0
#     ) +
#     geom_point(
#         aes(x = 22, y = 26),
#         color = color_admittance,
#         size = 2
#     ) +
#     annotate("text", label = "Admittance", x = 22, y = 27,
#              color = color_admittance,
#              size = 4,
#              hjust = 0
#     ) +
#     geom_point(
#         aes(x = 21, y = 26),
#         color = color_operation,
#         size = 2
#     ) +
#     annotate("text", label = "Operation", x = 21, y = 27,
#              color = color_operation,
#              size = 4,
#              hjust = 0
#     ) +
#
#
#     xlab("Patients") +
#     ylab("Day: Recognition -> Admittance -> Operation") +
#     scale_y_continuous(breaks = seq(0,35,2)) +
#
#     geom_hline(yintercept = mean_recognition,
#                color = color_recognition,
#                linetype = "dashed") +
#
#     geom_hline(yintercept = mean_admittance,
#                color = color_admittance,
#                linetype = "dashed") +
#
#     geom_hline(yintercept = mean_operation,
#                color = color_operation,
#                linetype = "dashed")
#
#
#
# # gganimate ----
#
# library(gganimate)
#
#
# ggplot(torsiyon) +
#
#     coord_flip() +
#
#     geom_point(
#         aes(x = patient, y = recognition),
#         color = color_recognition,
#         size = 5
#         # shape = 3
#     ) +
#
#     # transition_states(states = recognition,
#     #                   transition_length = 2,
#     #                   state_length = 1,
#     #                   wrap = FALSE) +
#
#     geom_segment(
#         aes(x = patient,
#             xend = patient,
#             y = recognition,
#             yend = admittance
#         ),
#         color = "grey50",
#         size = 0.5,
#         linetype = "dashed"
#     ) +
#
#     enter_fly(x_loc = patient, y_loc = recognition) +
#     ease_aes() +
#
#     xlab("Patients") +
#
#     ylab("Day: Recognition -> Admittance -> Operation") +
#
#     scale_y_continuous(breaks = seq(0,35,2)) +
#
#
#
#     # geom_point(
#     #     aes(x = patient, y = admittance),
#     #     color = color_admittance,
#     #     size = 3
#     # ) +
#
#     # geom_segment(
#     #     aes(x = patient,
# #         xend = patient,
# #         y = admittance,
# #         yend = operation
# #     ),
# #     color = "grey50",
# #     size = 0.5
# # ) +
# # geom_point(
# #     aes(x = patient, y = recognition),
# #     color = color_recognition,
# #     size = 5
# #     # shape = 3
# # ) +
# # geom_point(
# #     aes(x = patient, y = admittance),
# #     color = color_admittance,
# #     size = 3
# # ) +
# # geom_point(
# #     aes(x = patient, y = operation),
# #     color = color_operation,
# #     size = 2
# # ) +
# # coord_flip() +
# # # theme_modern_rc() +
# # # theme_ipsum() +
# # theme_light() +
# # theme(panel.background = element_blank(),
# #       panel.grid.major = element_line(colour = "grey90")
# # ) +
# #
# # annotate("rect",
# #          xmin = 20, xmax = 24, ymin = 25, ymax = 34,
# #          color = "grey",
# #          fill = "white"
# #
# # ) +
# #
# #
# # geom_point(
# #     aes(x = 23, y = 26),
# #     color = color_recognition,
# #     size = 2
# # ) +
# # annotate("text", label = "Recognition", x = 23, y = 27,
# #          color = color_recognition,
# #          size = 4,
# #          hjust = 0
# # ) +
# # geom_point(
# #     aes(x = 22, y = 26),
# #     color = color_admittance,
# #     size = 2
# # ) +
# # annotate("text", label = "Admittance", x = 22, y = 27,
# #          color = color_admittance,
# #          size = 4,
# #          hjust = 0
# # ) +
# # geom_point(
# #     aes(x = 21, y = 26),
# #     color = color_operation,
# #     size = 2
# # ) +
# # annotate("text", label = "Operation", x = 21, y = 27,
# #          color = color_operation,
# #          size = 4,
# #          hjust = 0
# # ) +
# #
# #
#
# #
# # geom_hline(yintercept = mean_recognition,
# #            color = color_recognition,
# #            linetype = "dashed") +
# #
# # geom_hline(yintercept = mean_admittance,
# #            color = color_admittance,
# #            linetype = "dashed") +
# #
# # geom_hline(yintercept = mean_operation,
# #            color = color_operation,
# #            linetype = "dashed")
#
#
#
#
#
#
#
