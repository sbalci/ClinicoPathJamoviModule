#' @title Venn Diagram
#' @description Generates a Venn Diagram and an Upset diagram from selected categorical variables.
#' This function converts specified variables to logical values based on a chosen "true" level.
#' Two visual outputs are produced: a Venn diagram (via ggvenn) and an Upset plot (via UpSetR).
#' Additionally, a summary table of "true" counts for each variable is provided.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom dplyr inner_join
#'
vennClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "vennClass",
        inherit = vennBase,
        private = list(
            .run = function() {
                # Check if required variables (var1 and var2) are provided.
                if (is.null(self$options$var1) || is.null(self$options$var2)) {
                    # Display a friendly welcome and instruction message.
                    todo <- "
                        <br><strong>Welcome to ClinicoPath Venn Diagram Tool</strong>
                        <br><br>
                        This tool helps you visualize overlaps between categorical variables
                        using Venn and Upset diagrams.
                        <br>
                        <em>Please select at least Variable 1 and Variable 2 to proceed.</em>
                        <hr><br>
                    "
                    self$results$todo$setContent(todo)
                } else {
                    # Clear welcome message once variables are selected.
                    self$results$todo$setContent("")

                    # Ensure data contains complete rows.
                    if (nrow(self$data) == 0)
                        stop("Data contains no (complete) rows")

                    # Read and clean the data.
                    mydata <- jmvcore::naOmit(self$data)

                    # Retrieve variable names and their corresponding "true" level selections.
                    var1 <- self$options$var1
                    var1true <- self$options$var1true
                    var2 <- self$options$var2
                    var2true <- self$options$var2true
                    var3 <- self$options$var3
                    var3true <- self$options$var3true
                    var4 <- self$options$var4
                    var4true <- self$options$var4true

                    # Convert each selected variable to logical values (TRUE if equal to the selected true level).
                    if (!is.null(self$options$var1)) {
                        mydata[[var1]] <- ifelse(mydata[[var1]] == var1true, TRUE, FALSE)
                    }
                    if (!is.null(self$options$var2)) {
                        mydata[[var2]] <- ifelse(mydata[[var2]] == var2true, TRUE, FALSE)
                    }
                    if (!is.null(self$options$var3)) {
                        mydata[[var3]] <- ifelse(mydata[[var3]] == var3true, TRUE, FALSE)
                    }
                    if (!is.null(self$options$var4)) {
                        mydata[[var4]] <- ifelse(mydata[[var4]] == var4true, TRUE, FALSE)
                    }

                    # Prepare data for the Venn diagram.
                    plotData <- list("mydata" = mydata,
                                     "names" = names(mydata))
                    self$results$plot$setState(plotData)

                    # Prepare data for the Upset diagram by converting logical values to integers.
                    mydata2 <- mydata %>%
                        dplyr::mutate(dplyr::across(.cols = dplyr::everything(), ~ as.integer(.)))
                    plotData2 <- list("mydata" = mydata2,
                                      "names" = names(mydata2))
                    self$results$plot2$setState(plotData2)

                    # summaryCounts <- list()
                    # if (!is.null(var1)) summaryCounts[[var1]] <- sum(mydata[[var1]])
                    # if (!is.null(var2)) summaryCounts[[var2]] <- sum(mydata[[var2]])
                    # if (!is.null(var3)) summaryCounts[[var3]] <- sum(mydata[[var3]])
                    # if (!is.null(var4)) summaryCounts[[var4]] <- sum(mydata[[var4]])
                    # # Set the summary result content if the summary output is available.
                    # if (!is.null(self$results$summary))
                    #     self$results$summary$setContent(summaryCounts)
                }
            },

            .plot = function(image, ggtheme, theme, ...) {
                # Validate that the required inputs are available.
                if (is.null(self$options$var1) || is.null(self$options$var2))
                    return()
                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                # Retrieve the prepared data.
                results <- image$state
                mydata2 <- results$mydata
                namescolumn2 <- results$names

                # Generate the Venn Diagram using ggvenn.
                plot <- ggvenn::ggvenn(
                    data = mydata2,
                    columns = namescolumn2
                )

                # Enhance the plot with a title and a refined theme for improved presentation.
                plot <- plot +
                    ggtheme +
                    ggplot2::ggtitle("Venn Diagram of Selected Variables") +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        axis.line.x = ggplot2::element_blank(),
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank(),
                        axis.title.x = ggplot2::element_blank(),
                        axis.line.y = ggplot2::element_blank(),
                        axis.text.y = ggplot2::element_blank(),
                        axis.ticks.y = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank()
                    )

                # Print the Venn Diagram.
                print(plot)
                TRUE
            },

            .plot2 = function(image, ggtheme, theme, ...) {
                # Validate that the required inputs are available.
                if (is.null(self$options$var1) || is.null(self$options$var2))
                    return()
                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                # Retrieve the prepared data.
                results <- image$state
                mydata2 <- results$mydata

                # Generate the Upset Diagram using UpSetR.
                plot2 <- UpSetR::upset(mydata2, order.by = "freq")

                # Print the Upset Diagram.
                print(plot2)
                # NEW FEATURE: Add a title to the Upset Diagram using grid.text.
                grid::grid.text("Upset Diagram of Selected Variables", x = 0.5, y = 0.97,
                                gp = grid::gpar(fontsize = 14, fontface = "bold"))
                TRUE
            }
        )
    )
