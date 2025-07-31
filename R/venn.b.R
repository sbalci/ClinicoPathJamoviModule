#' @title Venn Diagram
#' @description Generates a Venn Diagram and an Upset diagram from selected categorical variables.
#' This function converts specified variables to logical values based on a chosen "true" level.
#' Two visual outputs are produced: a Venn diagram (via ggvenn) and an Upset plot (via UpSetR or ComplexUpset).
#' Additionally, a summary table of "true" counts for each variable is provided.
#' 
#' ComplexUpset features include advanced styling, statistical annotations, custom sorting,
#' and enhanced theming options for publication-ready figures.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom dplyr inner_join
#' @import ggvenn
#' @import UpSetR
#' @import ComplexUpset
#' @importFrom grid grid.text
#' @importFrom ggplot2 ggtitle theme element_text
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#'
#' @return The function produces a Venn diagram and an Upset diagram.
#' @export vennClass
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic 2-variable Venn diagram
#' data("mtcars")
#' mtcars$vs <- factor(mtcars$vs, levels = c(0, 1), labels = c("V-shaped", "Straight"))
#' mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
#' 
#' # Create Venn diagram showing overlap between V-shaped engines and Manual transmission
#' venn(data = mtcars, var1 = "vs", var1true = "V-shaped", 
#'      var2 = "am", var2true = "Manual")
#' 
#' # Example 2: 3-variable Venn diagram with penguins data
#' library(palmerpenguins)
#' data("penguins")
#' penguins$large_bill <- factor(ifelse(penguins$bill_length_mm > 45, "Large", "Small"))
#' penguins$heavy_weight <- factor(ifelse(penguins$body_mass_g > 4000, "Heavy", "Light"))
#' penguins$adelie_species <- factor(ifelse(penguins$species == "Adelie", "Adelie", "Other"))
#' 
#' venn(data = penguins, 
#'      var1 = "large_bill", var1true = "Large",
#'      var2 = "heavy_weight", var2true = "Heavy", 
#'      var3 = "adelie_species", var3true = "Adelie")
#' 
#' # Example 3: Medical/Clinical example
#' # Create sample clinical data
#' clinical_data <- data.frame(
#'   patient_id = 1:100,
#'   diabetes = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.3, 0.7)),
#'   hypertension = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.4, 0.6)),
#'   obesity = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.25, 0.75))
#' )
#' 
#' # Analyze comorbidity patterns
#' venn(data = clinical_data,
#'      var1 = "diabetes", var1true = "Yes",
#'      var2 = "hypertension", var2true = "Yes",
#'      var3 = "obesity", var3true = "Yes")
#' 
#' # Example 4: Using ComplexUpset for advanced features
#' venn(data = clinical_data,
#'      var1 = "diabetes", var1true = "Yes",
#'      var2 = "hypertension", var2true = "Yes",
#'      var3 = "obesity", var3true = "Yes",
#'      upsetType = "complexUpset",
#'      sortBy = "freq",
#'      minSize = 5,
#'      showAnnotations = TRUE)
#' }
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

                    # Create summary statistics for each variable
                    summaryData <- data.frame(
                        Variable = character(),
                        TrueCount = integer(),
                        FalseCount = integer(),
                        TotalCount = integer(),
                        TruePercentage = numeric(),
                        stringsAsFactors = FALSE
                    )
                    
                    # Process each variable that was selected
                    if (!is.null(var1)) {
                        true_count <- sum(mydata[[var1]], na.rm = TRUE)
                        false_count <- sum(!mydata[[var1]], na.rm = TRUE)
                        total_count <- true_count + false_count
                        summaryData <- rbind(summaryData, data.frame(
                            Variable = var1,
                            TrueCount = true_count,
                            FalseCount = false_count,
                            TotalCount = total_count,
                            TruePercentage = round(true_count / total_count, 4),
                            stringsAsFactors = FALSE
                        ))
                    }
                    
                    if (!is.null(var2)) {
                        true_count <- sum(mydata[[var2]], na.rm = TRUE)
                        false_count <- sum(!mydata[[var2]], na.rm = TRUE)
                        total_count <- true_count + false_count
                        summaryData <- rbind(summaryData, data.frame(
                            Variable = var2,
                            TrueCount = true_count,
                            FalseCount = false_count,
                            TotalCount = total_count,
                            TruePercentage = round(true_count / total_count, 4),
                            stringsAsFactors = FALSE
                        ))
                    }
                    
                    if (!is.null(var3)) {
                        true_count <- sum(mydata[[var3]], na.rm = TRUE)
                        false_count <- sum(!mydata[[var3]], na.rm = TRUE)
                        total_count <- true_count + false_count
                        summaryData <- rbind(summaryData, data.frame(
                            Variable = var3,
                            TrueCount = true_count,
                            FalseCount = false_count,
                            TotalCount = total_count,
                            TruePercentage = round(true_count / total_count, 4),
                            stringsAsFactors = FALSE
                        ))
                    }
                    
                    if (!is.null(var4)) {
                        true_count <- sum(mydata[[var4]], na.rm = TRUE)
                        false_count <- sum(!mydata[[var4]], na.rm = TRUE)
                        total_count <- true_count + false_count
                        summaryData <- rbind(summaryData, data.frame(
                            Variable = var4,
                            TrueCount = true_count,
                            FalseCount = false_count,
                            TotalCount = total_count,
                            TruePercentage = round(true_count / total_count, 4),
                            stringsAsFactors = FALSE
                        ))
                    }
                    
                    # Set the summary results
                    if (!is.null(self$results$summary)) {
                        for (i in seq_len(nrow(summaryData))) {
                            self$results$summary$addRow(rowKey = i, values = list(
                                variable = summaryData$Variable[i],
                                trueCount = summaryData$TrueCount[i],
                                falseCount = summaryData$FalseCount[i],
                                totalCount = summaryData$TotalCount[i],
                                truePercentage = summaryData$TruePercentage[i]
                            ))
                        }
                    }
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
                
                # Get user options
                upsetType <- self$options$upsetType
                sortBy <- self$options$sortBy
                minSize <- self$options$minSize
                showAnnotations <- self$options$showAnnotations

                # Generate the Upset Diagram based on user choice
                if (upsetType == "complexUpset") {
                    # Use ComplexUpset for advanced features
                    
                    # Prepare data for ComplexUpset (convert back to logical from integer)
                    upset_data <- mydata2
                    for (col in names(upset_data)) {
                        upset_data[[col]] <- as.logical(upset_data[[col]])
                    }
                    
                    # Determine sort mode for ComplexUpset
                    sort_mode <- switch(sortBy,
                        "freq" = "descending",
                        "degree" = "ascending", 
                        "none" = FALSE,
                        "descending"  # default
                    )
                    
                    # Create the base ComplexUpset plot
                    plot2 <- ComplexUpset::upset(
                        data = upset_data,
                        intersect = names(upset_data),
                        min_size = minSize,
                        sort_intersections = sort_mode,
                        sort_sets = sort_mode,
                        name = "Intersection Size",
                        width_ratio = 0.1,
                        height_ratio = 0.8,
                        wrap = TRUE,
                        themes = list(
                            'intersections_matrix' = ggplot2::theme(
                                text = ggplot2::element_text(size = 10),
                                axis.text = ggplot2::element_text(size = 8)
                            ),
                            'overall_sizes' = ggplot2::theme(
                                text = ggplot2::element_text(size = 10),
                                axis.text = ggplot2::element_text(size = 8)
                            )
                        )
                    )
                    
                    # Note: ComplexUpset doesn't support annotate_text function
                    # Annotations are built into the plot by default
                    
                    # Add title
                    plot2 <- plot2 + 
                        ggplot2::ggtitle("ComplexUpset Diagram of Selected Variables") +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
                        )
                    
                    # Print the ComplexUpset plot
                    print(plot2)
                    
                } else {
                    # Use classic UpSetR
                    
                    # Determine order.by parameter
                    orderBy <- switch(sortBy,
                        "freq" = "freq",
                        "degree" = "degree",
                        "none" = "freq",  # Default to "freq" instead of NULL to avoid xtfrm error
                        "freq"  # default
                    )
                    
                    # Create UpSetR plot
                    # Note: UpSetR shows intersection counts by default when showAnnotations is enabled
                    plot2 <- UpSetR::upset(
                        mydata2, 
                        order.by = orderBy,
                        cutoff = minSize,
                        text.scale = if(showAnnotations) c(1.3, 1.3, 1, 1, 2, 0.75) else c(1, 1, 1, 1, 1, 1)
                    )
                    
                    # Print the Upset Diagram.
                    print(plot2)
                    
                    # Add a title to the Upset Diagram using grid.text.
                    grid::grid.text("UpSetR Diagram of Selected Variables", x = 0.5, y = 0.97,
                                    gp = grid::gpar(fontsize = 14, fontface = "bold"))
                }
                
                TRUE
            }
        )
    )
