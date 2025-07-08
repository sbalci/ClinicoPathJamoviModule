#' @title Treemap
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2

jjtreemapClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjtreemapClass",
    inherit = jjtreemapBase,
    private = list(
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .treemap_data = NULL,
        
        # Init function to set up the plot size
        .init = function() {
            self$results$plot$setSize(800, 500)
            
            # Pre-prepare data and options for performance
            private$.prepareData()
            private$.prepareOptions()
        },
        
        # Performance optimization methods ----
        
        .prepareData = function() {
            # Create a simple hash of current data to detect changes
            current_hash <- paste(self$options$group, self$options$size, self$options$color, nrow(self$data), collapse = "_")
            
            # Only reprocess if data has changed
            if (is.null(private$.data_hash) || private$.data_hash != current_hash) {
                
                if (!is.null(self$options$group) && !is.null(self$options$size)) {
                    mydata <- self$data
                    
                    # Remove NA values once
                    mydata <- jmvcore::naOmit(mydata)
                    
                    # Ensure size variable is numeric
                    if (!is.null(self$options$size)) {
                        mydata[[self$options$size]] <- as.numeric(mydata[[self$options$size]])
                        # Ensure all values are positive for treemap
                        mydata[[self$options$size]] <- pmax(mydata[[self$options$size]], 0.01)
                    }
                    
                    private$.prepared_data <- mydata
                    private$.data_hash <- current_hash
                } else {
                    private$.prepared_data <- NULL
                }
            }
            
            return(private$.prepared_data)
        },
        
        .prepareOptions = function() {
            # Cache processed options
            if (is.null(private$.prepared_options)) {
                
                # Convert font face option to numeric
                font_face_map <- list(
                    "normal" = 1,
                    "bold" = 2,
                    "italic" = 3,
                    "bolditalic" = 4
                )
                font_face <- font_face_map[[self$options$labelFontFace]]
                
                # Create alignment list based on user options
                align_labels <- list(
                    c(self$options$labelAlignH, self$options$labelAlignV)
                )
                if (!is.null(self$options$color)) {
                    align_labels[[2]] <- c(self$options$labelAlignH, self$options$labelAlignV)
                }
                
                private$.prepared_options <- list(
                    group_var = self$options$group,
                    size_var = self$options$size,
                    color_var = self$options$color,
                    font_face = font_face,
                    align_labels = align_labels,
                    aspectRatio = self$options$aspectRatio,
                    borderWidth = self$options$borderWidth,
                    borderLevel1Width = self$options$borderLevel1Width,
                    borderLevel2Width = self$options$borderLevel2Width,
                    borderLevel1Color = self$options$borderLevel1Color,
                    borderLevel2Color = self$options$borderLevel2Color,
                    labelLevel1Size = self$options$labelLevel1Size,
                    labelLevel2Size = self$options$labelLevel2Size,
                    labelLevel1Color = self$options$labelLevel1Color,
                    labelLevel2Color = self$options$labelLevel2Color,
                    labelBackground = self$options$labelBackground,
                    labelOverlap = self$options$labelOverlap,
                    showLabels = self$options$showLabels,
                    labelSize = self$options$labelSize,
                    title = self$options$title,
                    subtitle = self$options$subtitle,
                    caption = self$options$caption
                )
            }
            
            return(private$.prepared_options)
        },

        # Run function - initial setup and checks
        .run = function() {
            if (is.null(self$options$group) || is.null(self$options$size)) {
                # Show welcome message if required variables aren't provided
                todo <- glue::glue(
                    "<br>Welcome to ClinicoPath Treemap
                    <br><br>
                    This tool will help you create treemap visualizations for hierarchical categorical data.
                    <br><br>
                    Treemaps display hierarchical data as nested rectangles, where:
                    <br>• Rectangle size represents a quantitative value
                    <br>• Rectangle color can represent categories
                    <br>• Nested rectangles show hierarchical relationships
                    <br><br>
                    Please provide at least:
                    <br>• <b>Group Variable</b>: Categories to create rectangles
                    <br>• <b>Size Variable</b>: Numeric values for rectangle areas
                    <br>• <b>Color Variable</b> (optional): Additional categorization
                    <br><br>
                    This function uses the treemap and ggplot2 R packages.
                    <br><hr>"
                )
                self$results$todo$setContent(todo)
                return()
            }

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')
            
            # Prepare data and options
            private$.prepareData()
            private$.prepareOptions()
        },

        # Main plot function
        .plot = function(image, ggtheme, theme, ...) {
            # Check for required variables
            if (is.null(self$options$group) || is.null(self$options$size))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use prepared data and options ----
            mydata <- private$.prepareData()
            opts <- private$.prepareOptions()
            
            if (is.null(mydata)) {
                return()
            }

            # Additional validation
            if (nrow(mydata) == 0) {
                warning_msg <- "<br>No complete data rows available after removing missing values.<br>Please check your data for missing values in the selected variables.<br><hr>"
                self$results$todo$setContent(warning_msg)
                return()
            }

            # Create treemap using optimized data and options
            tryCatch({
                # Create or reuse treemap data
                if (is.null(private$.treemap_data) || private$.data_hash != attr(private$.treemap_data, "hash")) {
                    private$.treemap_data <- treemap::treemap(
                        mydata,
                        index = c(opts$group_var),
                        vSize = opts$size_var,
                        vColor = if (!is.null(opts$color_var)) opts$color_var else opts$group_var,
                        type = "categorical",
                        algorithm = "pivotSize",

                        # Border customization
                        border.col = c(opts$borderLevel1Color, opts$borderLevel2Color),
                        border.lwds = c(opts$borderLevel1Width, opts$borderLevel2Width),

                        # Label customization
                        fontsize.labels = c(opts$labelLevel1Size, opts$labelLevel2Size),
                        fontcolor.labels = c(opts$labelLevel1Color, opts$labelLevel2Color),
                        fontface.labels = rep(opts$font_face, 2),
                        bg.labels = opts$labelBackground,
                        align.labels = opts$align_labels,
                        overlap.labels = opts$labelOverlap,

                        aspRatio = opts$aspectRatio,
                        draw = FALSE
                    )
                    attr(private$.treemap_data, "hash") <- private$.data_hash
                }

                # Convert treemap data to ggplot-friendly format
                data_ggplot <- private$.treemap_data[["tm"]] %>%
                    tibble::as_tibble() %>%
                    dplyr::arrange(desc(vSize)) %>%
                    dplyr::mutate(
                        xmax = x0 + w,
                        ymax = y0 + h,
                        label = sprintf("%s\n(%g)", get(opts$group_var), vSize)
                    )

                # Create the plot
                p <- ggplot(data_ggplot) +
                    geom_rect(
                        aes(
                            xmin = x0,
                            ymin = y0,
                            xmax = xmax,
                            ymax = ymax,
                            fill = vColor
                        ),
                        colour = opts$borderLevel1Color,
                        size = opts$borderWidth
                    )

                # Add labels if requested
                if (opts$showLabels) {
                    p <- p + ggfittext::geom_fit_text(
                        aes(
                            xmin = x0,
                            xmax = xmax,
                            ymin = y0,
                            ymax = ymax,
                            label = label
                        ),
                        min.size = opts$labelSize,
                        place = "centre",
                        grow = TRUE
                    )
                }

                # Add title and labels
                if (opts$title != "" || opts$subtitle != "" || opts$caption != "") {
                    p <- p + labs(
                        title = if (opts$title != "") opts$title else NULL,
                        subtitle = if (opts$subtitle != "") opts$subtitle else NULL,
                        caption = if (opts$caption != "") opts$caption else NULL
                    )
                }

                # Add theme with better customization
                p <- p +
                    theme_void() +
                    theme(
                        legend.position = "none",
                        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
                        plot.subtitle = element_text(size = 12, hjust = 0.5),
                        plot.caption = element_text(size = 9, hjust = 1, face = "italic"),
                        plot.margin = margin(10, 10, 10, 10)
                    )

                # Print plot
                print(p)
                TRUE
                
            }, error = function(e) {
                error_msg <- paste0(
                    "<br>Error creating treemap: ", e$message, 
                    "<br><br>Please check that:",
                    "<br>• Group variable is categorical",
                    "<br>• Size variable contains numeric values",
                    "<br>• All size values are positive (negative values are converted to 0.01)",
                    "<br>• Data has at least one complete row",
                    "<br><hr>"
                )
                self$results$todo$setContent(error_msg)
            })
        }
    )
)
