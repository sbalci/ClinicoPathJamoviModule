#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom ggalluvial geom_flow geom_stratum
#' @importFrom dplyr group_by summarise mutate n
#' @importFrom tidyr separate_rows
#' @importFrom magrittr %>%
#'
#' @title Alluvial Survival Plot
#' @return Alluvial Survival Plot
#' @description Generates an alluvial plot to visualize patient treatment pathways over time.
#'

alluvialSurvivalClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "alluvialSurvivalClass",
    inherit = alluvialSurvivalBase,
    private = list(
        .init = function() {
            if (is.null(self$options$timeVar)) {
                todo <- "
                    <br>Welcome to Treatment Pathway Visualization
                    <br><br>
                    This tool helps visualize patient treatment pathways over time.
                    <br><br>
                    Required inputs:
                    <br>- Time: Time points for measurements
                    <br>- Disease Stage: Stage at each time point
                    <br>- Treatment: Treatment received
                    <br>- Patient ID: Unique identifier
                    <br>- Survival Status (optional): For survival curves
                    "
                self$results$todo$setContent(todo)
            }
        },

        .run = function() {
            if (is.null(self$options$timeVar) ||
                is.null(self$options$stageVar) ||
                is.null(self$options$treatmentVar) ||
                is.null(self$options$patientId))
                return()

            if (is.null(self$data) || nrow(self$data) == 0)
                stop('Data contains no complete rows')

            # Process data and calculate statistics
            stats <- private$.calculateStats()



            mydataview <- self$results$mydataview
            mydataview$setContent(
                list(
                    head(self$data),
                    stats = stats
                    )
            )



            # Populate summary table
            table <- self$results$summaryTable

            for (i in seq_along(stats)) {
                stat <- stats[[i]]
                table$addRow(rowKey=i, values=list(
                    timePoint = stat$timePoint,
                    totalPatients = stat$totalPatients,
                    stageDistribution = paste(names(stat$stageDistribution),
                                              stat$stageDistribution,
                                              collapse=", "),
                    treatmentDistribution = paste(names(stat$treatmentDistribution),
                                                  stat$treatmentDistribution,
                                                  collapse=", ")
                ))
            }

            # If survival data is available, populate survival stats
            if (!is.null(self$options$survivalVar)) {
                survStats <- private$.calculateSurvivalStats()
                table <- self$results$survivalStats

                for (i in seq_along(survStats)) {
                    stat <- survStats[[i]]
                    table$addRow(rowKey=i, values=list(
                        stage = stat$stage,
                        treatment = stat$treatment,
                        patients = stat$patients,
                        events = stat$events,
                        survivalRate = stat$survivalRate
                    ))
                }
            }

            # Prepare plot data
            image <- self$results$plot
            image$setState(list(
                data = private$.prepareAlluvialData(),
                options = self$options
            ))
        },

        .calculateStats = function() {
            data <- self$data
            timePoints <- sort(unique(data[[self$options$timeVar]]))

            stats <- lapply(timePoints, function(t) {
                currentData <- data[data[[self$options$timeVar]] == t,]
                list(
                    timePoint = t,
                    totalPatients = length(unique(currentData[[self$options$patientId]])),
                    stageDistribution = table(currentData[[self$options$stageVar]]),
                    treatmentDistribution = table(currentData[[self$options$treatmentVar]])
                )
            })

            return(stats)
        },

        .calculateSurvivalStats = function() {
            if (is.null(self$options$survivalVar))
                return(NULL)

            data <- self$data

            # Calculate survival statistics for each stage-treatment combination
            stats <- tapply(data[[self$options$survivalVar]],
                            list(data[[self$options$stageVar]],
                                 data[[self$options$treatmentVar]]),
                            function(x) {
                                list(
                                    patients = length(x),
                                    events = sum(x == 1),
                                    survivalRate = mean(x == 0)
                                )
                            }, simplify = FALSE)

            # Flatten the results
            result <- list()
            for (stage in dimnames(stats)[[1]]) {
                for (treatment in dimnames(stats)[[2]]) {
                    if (!is.null(stats[[stage, treatment]])) {
                        result[[length(result) + 1]] <- c(
                            list(stage = stage, treatment = treatment),
                            stats[[stage, treatment]]
                        )
                    }
                }
            }

            return(result)
        },



        .prepareAlluvialData = function() {
            data <- self$data
            timeVar <- self$options$timeVar
            stageVar <- self$options$stageVar
            treatmentVar <- self$options$treatmentVar
            patientId <- self$options$patientId

            # Calculate nodes for each time point
            timePoints <- sort(unique(data[[timeVar]]))
            nodes <- lapply(timePoints, function(t) {
                currentData <- data[data[[timeVar]] == t,]
                list(
                    time = t,
                    stages = as.data.frame(table(currentData[[stageVar]])),
                    treatments = as.data.frame(table(currentData[[treatmentVar]]))
                )
            })

            # Calculate flows between time points
            flows <- list()
            for (i in 1:(length(timePoints)-1)) {
                t1 <- timePoints[i]
                t2 <- timePoints[i+1]

                # Get data for both time points
                data1 <- data[data[[timeVar]] == t1,]
                data2 <- data[data[[timeVar]] == t2,]

                # Get common patients between the two time points
                commonIds <- intersect(data1[[patientId]], data2[[patientId]])

                # Filter data to only include common patients
                data1 <- data1[data1[[patientId]] %in% commonIds,]
                data2 <- data2[data2[[patientId]] %in% commonIds,]

                # Sort both datasets by patient ID to ensure alignment
                data1 <- data1[order(data1[[patientId]]),]
                data2 <- data2[order(data2[[patientId]]),]

                # Now calculate transitions for stages and treatments
                stageTransitions <- table(data1[[stageVar]], data2[[stageVar]])
                treatmentTransitions <- table(data1[[treatmentVar]], data2[[treatmentVar]])

                flows[[length(flows) + 1]] <- list(
                    from = t1,
                    to = t2,
                    stageTransitions = stageTransitions,
                    treatmentTransitions = treatmentTransitions,
                    patientCount = length(commonIds)
                )
            }

            return(list(
                nodes = nodes,
                flows = flows,
                timePoints = timePoints
            ))
        }

        ,


        .plot = function(image, ggtheme, theme, ...) {
            # Check for valid image state
            if (is.null(image$state))
                return(FALSE)

            # Extract data and options from state
            stateData <- image$state$data
            options <- image$state$options

            # Check if data is valid
            if (is.null(stateData) || is.null(stateData$nodes))
                return(FALSE)

            # Prepare plot data from nodes
            plotData <- data.frame()
            for (node in stateData$nodes) {
                # Process stages
                stages <- node$stages
                if (nrow(stages) > 0) {
                    stageData <- data.frame(
                        time = node$time,
                        group = as.character(stages$Var1),
                        count = stages$Freq,
                        type = "Stage"
                    )
                    plotData <- rbind(plotData, stageData)
                }

                # Process treatments
                treatments <- node$treatments
                if (nrow(treatments) > 0) {
                    treatmentData <- data.frame(
                        time = node$time,
                        group = as.character(treatments$Var1),
                        count = treatments$Freq,
                        type = "Treatment"
                    )
                    plotData <- rbind(plotData, treatmentData)
                }
            }

            # Check if plotData is valid
            if (nrow(plotData) == 0)
                return(FALSE)

            # Define color schemes
            colorSchemes <- list(
                clinical = c(
                    "Resectable" = "#F8D5B5",
                    "BR/LA" = "#E6A17B",
                    "Metastatic" = "#8B4513",
                    "Surgery" = "#9ED682",
                    "Chemotherapy" = "#6BAED6",
                    "Neoadjuvant" = "#FD8D3C",
                    "Follow-up" = "#969696",
                    "Supportive Care" = "#D4B9DA"
                ),
                colorblind = c(
                    "Resectable" = "#009E73",
                    "BR/LA" = "#56B4E9",
                    "Metastatic" = "#E69F00",
                    "Surgery" = "#CC79A7",
                    "Chemotherapy" = "#0072B2",
                    "Neoadjuvant" = "#D55E00",
                    "Follow-up" = "#999999",
                    "Supportive Care" = "#E6AB02"
                )
            )

            # Select color scheme
            colors <- if (options$colorScheme == "colorblind") {
                colorSchemes$colorblind
            } else {
                colorSchemes$clinical
            }

            # Create the plot
            p <- ggplot2::ggplot(
                plotData,
                ggplot2::aes(
                    x = factor(time),
                    y = count,
                    fill = group,
                    stratum = group,
                    alluvium = group
                )
            ) +
                ggalluvial::geom_flow(alpha = 0.7) +
                ggalluvial::geom_stratum() +
                ggplot2::scale_fill_manual(values = colors) +
                ggplot2::labs(
                    x = "Time (months)",
                    y = "Number of Patients",
                    title = "Patient Treatment Pathways",
                    fill = "Category"
                ) +
                ggtheme +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    legend.position = "right"
                )

            # Add right axis if requested
            if (options$showRightAxis) {
                maxPatients <- max(plotData$count)
                p <- p + ggplot2::scale_y_continuous(
                    sec.axis = ggplot2::sec_axis(
                        ~ . / maxPatients * 100,
                        name = "Percentage of Patients",
                        labels = function(x) paste0(round(x), "%")
                    )
                )
            }

            print(p)
            return(TRUE)
        }




           )
)
