#' @title Alluvial Plot
#' @description
#' This function generates an Alluvial Diagram to visualize flows between multiple categorical variables.
#' It supports optional features such as marginal histograms, and custom titles
#'
#' @param data Data frame containing the input variables.
#' @param vars Character vector of variable names from the data used to create the alluvial diagram.
#' @param condensationvar (Optional) A variable name used for creating a condensation plot.
#' @param excl Logical. If TRUE, rows with missing values are excluded.
#' @param marg Logical. If TRUE, marginal histograms are added to the diagram.
#' @param fill Character string specifying the variable for color fill.
#' @param bin A label or vector of labels used for binning. If set to "default", a predefined set is used.
#' @param orient Character. "vert" for vertical (default) or "horr" for horizontal orientation.
#' @param usetitle Logical. If TRUE, a custom title (from `mytitle`) is added.
#' @param mytitle Character. Custom title for the plot.
#'
#' @return An Alluvial Plot is generated and printed.
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom magrittr %>%
alluvialClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "alluvialClass",
    inherit = alluvialBase,
    private = list(
        .run = function() {
            # If no variables are selected, display a welcome message with instructions.
            if (is.null(self$options$vars)) {
                todo <- "
                <br><h2>Welcome to ClinicoPath</h2>
                <p>This tool will help you create Alluvial Diagrams to visualize flows between categorical variables.</p>
                <hr><br>
                "
                self$results$todo$setContent(todo)
            } else {
                # Clear the welcome message once variables are provided.
                self$results$todo$setContent("")
                if (nrow(self$data) == 0)
                    stop("Error: The dataset contains no complete rows. Please check your data.")
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Validate input: Check that variables are provided and data is non-empty.
            if (is.null(self$options$vars))
                return()
            if (nrow(self$data) == 0)
                stop("Error: The dataset contains no complete rows. Please provide valid data.")

            # Prepare Data: Select specified variables from the input data.
            varsName <- self$options$vars
            mydata <- jmvcore::select(self$data, c(varsName))

            # Exclude missing values if the option is selected.
            if (self$options$excl) {
                mydata <- jmvcore::naOmit(mydata)
            }

            # Prepare fill variable for color coding.
            fill <- jmvcore::composeTerm(self$options$fill)

            # Prepare bin labels; use a default set if "default" is chosen.
            bin <- self$options$bin
            if (bin == "default")
                bin <- c("LL", "ML", "M", "MH", "HH")


            # Generate the Alluvial Plot using the easyalluvial package.
            plot <- tryCatch({
                p <- easyalluvial::alluvial_wide(
                    data = mydata,
                    max_variables = 8,
                    fill_by = fill,
                    verbose = TRUE,
                    bin_labels = bin
                )

                p
            }, error = function(e) {
                stop("Plot generation failed: ", e$message)
            })

            # Add marginal histograms if the option is enabled.
            if (self$options$marg) {
                plot <- tryCatch({
                    easyalluvial::add_marginal_histograms(
                        p = plot,
                        data_input = mydata,
                        keep_labels = TRUE,
                        top = TRUE,
                        plot = TRUE
                    )
                }, error = function(e) {
                    warning("Adding marginal histograms failed: ", e$message)
                    plot  # Return the original plot if an error occurs.
                })
            }

            # Adjust plot orientation.
            orient <- self$options$orient
            if (orient != "vert") {
                plot <- plot + ggplot2::coord_flip()
            }

            # Add a custom title if enabled and if marginal plots are not used.
            if (self$options$usetitle) {
                if (self$options$marg) {
                    stop("Error: Custom title cannot be used with marginal plots. Please disable one of these options.")
                }
                mytitle <- self$options$mytitle
                plot <- plot + ggplot2::ggtitle(mytitle) +
                    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"))
            }

            # Enhance overall aesthetics: apply a minimal theme and add axis labels.
            plot <- plot + ggplot2::theme_minimal() +
                ggplot2::labs(x = "Variables", y = "Frequency")

            # Print the final plot.
            print(plot)
            TRUE
        },

        .plot2 = function(image, ggtheme, theme, ...) {
            # Validate input for the condensation plot.
            if (is.null(self$options$condensationvar) || is.null(self$options$vars))
                return()
            if (nrow(self$data) == 0)
                stop("Error: The dataset contains no complete rows.")

            # Prepare data for the condensation plot.
            condvarName <- jmvcore::composeTerm(components = self$options$condensationvar)
            mydata <- self$data

            # Generate the condensation plot using easyalluvial.
            plot2 <- tryCatch({
                easyalluvial::plot_condensation(
                    df = mydata,
                    first = mydata[[condvarName]]  # Corrected from .data to mydata.
                )
            }, error = function(e) {
                stop("Condensation plot generation failed: ", e$message)
            })

            # Enhance aesthetics for the condensation plot.
            plot2 <- plot2 + ggplot2::theme_minimal() +
                ggplot2::labs(title = paste("Condensation Plot for", condvarName))

            # Print the condensation plot.
            print(plot2)
            TRUE
        }
    )
)
