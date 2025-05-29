#' @title Interrater Reliability Analysis
#' @return Table
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom irr kappa2 kappam.fleiss agree
#' @importFrom dplyr select group_by count
#' @importFrom htmlTable htmlTable
#' @importFrom glue glue
#'
#' @description This function calculates interrater reliability for ordinal or categorical data.
#'
#' @details The function calculates Cohen's kappa for two raters and Fleiss' kappa for three or more raters.
#'
#'


# See
# \url{http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/#ordinal-data-weighted-kappa}


agreementClass <- if (requireNamespace("jmvcore")) R6::R6Class("agreementClass",
    inherit = agreementBase, private = list(



        .run = function() {



        # # Error Message ----
        #
        # if (nrow(self$data) == 0) stop("Data contains no (complete) rows")
        #
        # if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) ) {
        #     # ToDo Message ----
        #     todo <- "
        #         <br>Welcome to ClinicoPath
        #                   <br><br>
        #                   This tool will help you form an Alluvial Plots.
        #                   "
        #     html <- self$results$todo
        #     html$setContent(todo)
        #
        # } else {
        #     todo <- ""
        #     html <- self$results$todo
        #     html$setContent(todo)
        #
        #
        #
        # }









        # Data definition ----


        exct <- self$options$exct
        wght <- self$options$wght

        mydata <- self$data

        formula <- jmvcore::constructFormula(terms = self$options$vars)

        myvars <- jmvcore::decomposeFormula(formula = formula)

        myvars <- unlist(myvars)

        ratings <- mydata %>% dplyr::select(myvars)


        # psych::cohen.kappa ----

        # from https://github.com/kwongwh/Kappa
        # ratings2 <- mydata[c(self$options$vars)]
        #
        # result_cohen <- psych::cohen.kappa(x = ratings2)
        #
        # self$results$result_cohen$setContent(result_cohen)


        # irr.kappa <- kappa2(vars, weight = self$options$weights)
        # if(self$options$weights == "unweighted"){
        #     n = 1
        # } else {
        #     n = 2
        # }
        #
        # table <- self$results$ka
        #
        # table$setRow(rowNo=1, values=list(
        #     weights = self$options$weights,
        #     kappa=irr.kappa$value,
        #     upper_CI=results$confid[1,3],
        #     lower_CI=results$confid[1,1],
        #     p = irr.kappa$p.value
        # ))




        if (is.null(self$options$vars) || length(self$options$vars) < 2) {
            return()
            # No variables ----

            # todo <- glue::glue( 'This function ' )

            # self$results$todo$setContent(todo)

        } else {
            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")


            # 2 & categorical ----

            if (length(self$options$vars) == 2) {
                # todo <- 'Cohen'

                # self$results$todo$setContent(todo)


                xorder <- unlist(lapply(ratings, is.ordered))

                if (wght %in% c("equal", "squared") && !all(xorder == TRUE)) stop("Use ordinal variables when using weight argument")

                if (exct == TRUE) stop("Use exact argument only >=3 variables")


                # irr::kappa2 ----

                result2 <- irr::kappa2(ratings = ratings, weight = wght)

                # self$results$text2$setContent(result2)


                # >=2 & categorical ----


            } else if (length(self$options$vars) >= 2) {
                # todo <- 'kappam.fleiss'

                # self$results$todo$setContent(todo)

                # irr::kappam.fleiss ----

                result2 <- irr::kappam.fleiss(ratings = ratings, exact = exct,
                  detail = TRUE)

                # self$results$text2$setContent(result2)

            }


            # irr::agree ----

            result <- table(ratings)

            self$results$text$setContent(result)



            result3 <- ratings %>%
                dplyr::group_by_all() %>%
                dplyr::count() %>%
                as.data.frame() %>%
                htmlTable::htmlTable()

            self$results$text2$setContent(result3)



            # freqtable <- self$results$freqtable
            #
            # data_frame <- result3
            # for (i in seq_along(data_frame[, 1, drop = T])) {
            #     freqtable$addRow(rowKey = i, values = c(data_frame[i,]))
            # }






            result1 <- irr::agree(ratings)

            # self$results$text1$setContent(result1[["value"]])

            if (result1[["value"]] > 100) {

                result1[["value"]] <- "Please check the data. It seems that observers do not agree on any cases"
            }


            # Table ----

            table2 <- self$results$irrtable

            table2$setRow(
                rowNo = 1,
                values = list(
                    method = result2[["method"]],
                    subjects = result1[["subjects"]],
                    raters = result1[["raters"]],
                    peragree = result1[["value"]],
                    kappa = result2[["value"]],
                    z = result2[["statistic"]],
                    p = result2[["p.value"]]
                    )
                )


            # Prepare heatmap data - with error handling
            if (length(self$options$vars) >= 2) {
                # Check if heatmap option exists and is enabled
                show_heatmap <- FALSE
                tryCatch({
                    show_heatmap <- self$options$heatmap
                }, error = function(e) {
                    show_heatmap <- TRUE  # Default to true if option doesn't exist
                })

                if (show_heatmap) {
                    private$.prepareHeatmapData(ratings)
                }
            }

        }

        # Krippendorff's alpha ----
        if (self$options$kripp) {
            # Convert ratings data frame to matrix
            ratings_matrix <- as.matrix(ratings)

            # Ensure numeric conversion if needed
            if (!is.numeric(ratings_matrix)) {
                # If categorical/factor data, convert to numeric codes
                ratings_matrix <- matrix(
                    as.numeric(factor(ratings_matrix)),
                    nrow = nrow(ratings_matrix),
                    ncol = ncol(ratings_matrix)
                )
            }

            # Add error handling
            tryCatch({
                # Calculate Krippendorff's alpha
                kripp_result <- irr::kripp.alpha(
                    ratings_matrix,
                    method = self$options$krippMethod
                )

                # Initialize values list for table
                values_list <- list(
                    method = paste0("Krippendorff's Alpha (", self$options$krippMethod, ")"),
                    subjects = nrow(ratings_matrix),
                    raters = ncol(ratings_matrix),
                    alpha = kripp_result$value
                )

                # Calculate bootstrap CI if requested
                if (self$options$bootstrap) {
                    set.seed(123) # for reproducibility
                    n_boot <- 1000
                    alpha_boots <- numeric(n_boot)

                    for(i in 1:n_boot) {
                        boot_indices <- sample(1:nrow(ratings_matrix), replace = TRUE)
                        boot_data <- ratings_matrix[boot_indices,]

                        boot_alpha <- try(irr::kripp.alpha(boot_data,
                                                           method = self$options$krippMethod)$value,
                                          silent = TRUE)

                        if(!inherits(boot_alpha, "try-error")) {
                            alpha_boots[i] <- boot_alpha
                        }
                    }

                    # Calculate 95% confidence intervals
                    ci <- quantile(alpha_boots, c(0.025, 0.975), na.rm = TRUE)

                    # Add CI values to list
                    values_list$ci_lower <- ci[1]
                    values_list$ci_upper <- ci[2]
                }

                # Populate results table
                krippTable <- self$results$krippTable
                krippTable$setRow(rowNo = 1, values = values_list)

            }, error = function(e) {
                # Handle any errors that occur during calculation
                errorMessage <- paste("Error calculating Krippendorff's alpha:", e$message)
                warning(errorMessage)

                # Initialize values list for error case
                values_list <- list(
                    method = paste0("Krippendorff's Alpha (", self$options$krippMethod, ")"),
                    subjects = nrow(ratings_matrix),
                    raters = ncol(ratings_matrix),
                    alpha = NA
                )

                if (self$options$bootstrap) {
                    values_list$ci_lower <- NA
                    values_list$ci_upper <- NA
                }

                # Populate table with NA values
                krippTable <- self$results$krippTable
                krippTable$setRow(rowNo = 1, values = values_list)

                # Add error message as footnote
                krippTable$addFootnote(rowNo = 1, col = "alpha", paste0("Error calculating Krippendorff's alpha: ", e$message))
            })
        }
    },







    .prepareHeatmapData = function(ratings) {
        # Calculate pairwise agreement matrix
        rater_names <- names(ratings)
        n_raters <- length(rater_names)
        agreement_matrix <- matrix(0, nrow = n_raters, ncol = n_raters,
                                   dimnames = list(rater_names, rater_names))

        # Check if detailed heatmap option exists
        show_details <- FALSE
        tryCatch({
            show_details <- self$options$heatmapDetails
        }, error = function(e) {
            show_details <- FALSE  # Default to false if option doesn't exist
        })

        # Initialize kappa matrix for detailed view
        kappa_matrix <- NULL
        if (show_details) {
            kappa_matrix <- matrix(NA, nrow = n_raters, ncol = n_raters,
                                   dimnames = list(rater_names, rater_names))
        }

        for (i in 1:n_raters) {
            for (j in 1:n_raters) {
                if (i == j) {
                    agreement_matrix[i,j] <- 100
                    if (show_details && !is.null(kappa_matrix)) {
                        kappa_matrix[i,j] <- 1.0
                    }
                } else {
                    # Calculate percentage agreement
                    matches <- sum(ratings[[i]] == ratings[[j]], na.rm = TRUE)
                    total <- sum(!is.na(ratings[[i]]) & !is.na(ratings[[j]]))
                    agreement_matrix[i,j] <- if(total > 0) (matches/total) * 100 else 0

                    # Calculate Cohen's kappa for pair if detailed view is requested
                    if (show_details && !is.null(kappa_matrix)) {
                        tryCatch({
                            pair_data <- data.frame(
                                rater1 = ratings[[i]],
                                rater2 = ratings[[j]]
                            )
                            pair_data <- pair_data[complete.cases(pair_data), ]
                            if (nrow(pair_data) > 0) {
                                kappa_result <- irr::kappa2(pair_data)
                                kappa_matrix[i,j] <- kappa_result$value
                            }
                        }, error = function(e) {
                            kappa_matrix[i,j] <- NA
                        })
                    }
                }
            }
        }

        # Store for plotting
        image <- self$results$heatmapPlot
        image$setState(list(
            matrix = agreement_matrix,
            kappa_matrix = kappa_matrix,
            raters = rater_names,
            show_details = show_details
        ))
    },

    .heatmapPlot = function(image, ggtheme, ...) {
        state <- image$state
        if (is.null(state)) return(FALSE)

        agreement_matrix <- state$matrix
        kappa_matrix <- state$kappa_matrix
        raters <- state$raters
        show_details <- if(is.null(state$show_details)) FALSE else state$show_details
        n_raters <- length(raters)

        # Convert matrix to long format for ggplot
        melted_matrix <- reshape2::melt(agreement_matrix)
        colnames(melted_matrix) <- c("Rater1", "Rater2", "Agreement")

        # Add kappa values if available
        if (show_details && !is.null(kappa_matrix)) {
            melted_kappa <- reshape2::melt(kappa_matrix)
            colnames(melted_kappa) <- c("Rater1", "Rater2", "Kappa")
            melted_matrix <- merge(melted_matrix, melted_kappa,
                                   by = c("Rater1", "Rater2"))
        }

        # Ensure factor levels are in correct order
        melted_matrix$Rater1 <- factor(melted_matrix$Rater1, levels = raters)
        melted_matrix$Rater2 <- factor(melted_matrix$Rater2, levels = rev(raters))

        # Determine text color based on background
        melted_matrix$text_color <- ifelse(melted_matrix$Agreement > 65, "white", "black")

        # Create label based on whether details are shown
        if (show_details && "Kappa" %in% names(melted_matrix)) {
            melted_matrix$label <- ifelse(
                melted_matrix$Rater1 == melted_matrix$Rater2,
                "100%",
                sprintf("%.1f%%\nκ=%.2f", melted_matrix$Agreement, melted_matrix$Kappa)
            )
        } else {
            melted_matrix$label <- sprintf("%.1f%%", melted_matrix$Agreement)
        }

        # Create improved heatmap
        plot <- ggplot2::ggplot(melted_matrix, ggplot2::aes(x = Rater1, y = Rater2, fill = Agreement)) +
            # Main tiles
            ggplot2::geom_tile(color = "white", size = 1.2) +

            # Enhanced color scale
            ggplot2::scale_fill_gradientn(
                colors = c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#3288bd"),
                values = scales::rescale(c(0, 25, 40, 50, 60, 75, 85, 100)),
                name = "Agreement\n(%)",
                limits = c(0, 100),
                guide = ggplot2::guide_colorbar(
                    barwidth = 1.5,
                    barheight = 8,
                    title.position = "top",
                    title.hjust = 0.5
                )
            ) +

            # Text labels with dynamic color and content
            ggplot2::geom_text(
                ggplot2::aes(label = label, color = text_color),
                size = ifelse(n_raters <= 3, 4, ifelse(n_raters <= 5, 3.2, 2.8)),
                fontface = "bold",
                lineheight = 0.9
            ) +

            # Manual color scale for text
            ggplot2::scale_color_identity() +

            # Labels and title
            ggplot2::labs(
                title = if(show_details) "Interrater Agreement & Kappa Heatmap" else "Interrater Agreement Heatmap",
                subtitle = paste("Based on", nrow(agreement_matrix), "raters"),
                x = "Rater",
                y = "Rater",
                caption = if(show_details)
                    "Values show percentage agreement and Cohen's kappa (κ) for each rater pair" else
                        "Values represent percentage agreement between rater pairs"
            ) +

            # Enhanced theme
            ggplot2::theme_minimal() +
            ggplot2::theme(
                # Plot appearance
                plot.title = ggplot2::element_text(
                    hjust = 0.5,
                    size = 14,
                    face = "bold",
                    margin = ggplot2::margin(b = 5)
                ),
                plot.subtitle = ggplot2::element_text(
                    hjust = 0.5,
                    size = 11,
                    color = "gray40",
                    margin = ggplot2::margin(b = 15)
                ),
                plot.caption = ggplot2::element_text(
                    hjust = 0.5,
                    size = 9,
                    color = "gray50",
                    margin = ggplot2::margin(t = 10)
                ),

                # Axis formatting
                axis.text.x = ggplot2::element_text(
                    angle = 45,
                    hjust = 1,
                    vjust = 1,
                    size = 10,
                    face = "bold"
                ),
                axis.text.y = ggplot2::element_text(
                    size = 10,
                    face = "bold"
                ),
                axis.title = ggplot2::element_text(
                    size = 12,
                    face = "bold"
                ),

                # Remove grid and background
                panel.grid = ggplot2::element_blank(),
                panel.background = ggplot2::element_rect(fill = "white"),
                plot.background = ggplot2::element_rect(fill = "white"),

                # Legend formatting
                legend.title = ggplot2::element_text(size = 10, face = "bold"),
                legend.text = ggplot2::element_text(size = 9),
                legend.position = "right",

                # Margins
                plot.margin = ggplot2::margin(20, 20, 20, 20)
            ) +

            # Ensure square tiles
            ggplot2::coord_fixed(ratio = 1)

        # Add interpretive text annotation
        if (!show_details) {
            max_agreement <- max(melted_matrix$Agreement[melted_matrix$Rater1 != melted_matrix$Rater2])
            min_agreement <- min(melted_matrix$Agreement[melted_matrix$Rater1 != melted_matrix$Rater2])
            avg_agreement <- mean(melted_matrix$Agreement[melted_matrix$Rater1 != melted_matrix$Rater2])

            # Add summary statistics as annotation
            if (n_raters >= 3) {
                plot <- plot +
                    ggplot2::annotate(
                        "text",
                        x = 1,
                        y = 0.5,
                        label = sprintf("Avg: %.1f%%\nRange: %.1f%% - %.1f%%",
                                        avg_agreement, min_agreement, max_agreement),
                        hjust = 0,
                        vjust = 1,
                        size = 3,
                        color = "gray30",
                        fontface = "italic"
                    )
            }
        } else {
            # Add kappa interpretation guide
            plot <- plot +
                ggplot2::annotate(
                    "text",
                    x = 1,
                    y = 0.3,
                    label = "Kappa interpretation:\n<0.20 Poor\n0.21-0.40 Fair\n0.41-0.60 Moderate\n0.61-0.80 Good\n0.81-1.00 Very Good",
                    hjust = 0,
                    vjust = 1,
                    size = 2.5,
                    color = "gray30",
                    fontface = "italic",
                    lineheight = 0.8
                )
        }

        print(plot)
        return(TRUE)
    }






    ))
