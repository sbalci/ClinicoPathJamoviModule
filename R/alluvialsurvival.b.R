#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom ggalluvial geom_flow geom_stratum
#' @importFrom dplyr group_by summarise mutate n arrange
#' @importFrom tidyr separate_rows
#' @importFrom magrittr %>%
#' @importFrom survival survfit Surv
#' @importFrom survminer ggsurvplot
#'
#' @title Alluvial Survival Plot
#' @return Alluvial Survival Plot
#' @description Generates an alluvial plot to visualize patient treatment pathways over time with optional survival analysis.
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
                    <br><br>
                    Data should be in longitudinal format with one row per patient per time point.
                    "
                self$results$todo$setContent(todo)
            }
        },

        .run = function() {
            # Validate required parameters
            if (is.null(self$options$timeVar) ||
                is.null(self$options$stageVar) ||
                is.null(self$options$treatmentVar) ||
                is.null(self$options$patientId))
                return()

            if (is.null(self$data) || nrow(self$data) == 0)
                stop('Data contains no complete rows')

            # Validate data structure
            private$.validateData()

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

            # Prepare main plot data
            image <- self$results$plot
            image$setState(list(
                data = private$.prepareAlluvialData(),
                options = self$options
            ))

            # Prepare survival plot if requested
            if (!is.null(self$options$survivalVar) && self$options$showSurvival) {
                survivalImage <- self$results$survivalPlot
                survivalImage$setState(list(
                    data = private$.prepareSurvivalData(),
                    options = self$options
                ))
            }
        },

        .validateData = function() {
            data <- self$data
            timeVar <- self$options$timeVar
            stageVar <- self$options$stageVar
            treatmentVar <- self$options$treatmentVar
            patientId <- self$options$patientId
            survivalVar <- self$options$survivalVar

            # Check if required columns exist
            if (!timeVar %in% names(data))
                stop(paste("Time variable", timeVar, "not found in data"))
            if (!stageVar %in% names(data))
                stop(paste("Stage variable", stageVar, "not found in data"))
            if (!treatmentVar %in% names(data))
                stop(paste("Treatment variable", treatmentVar, "not found in data"))
            if (!patientId %in% names(data))
                stop(paste("Patient ID variable", patientId, "not found in data"))

            # Check for longitudinal structure
            timePoints <- length(unique(data[[timeVar]]))
            if (timePoints < 2)
                stop("Data must contain at least 2 time points for pathway analysis")

            patients <- length(unique(data[[patientId]]))
            if (patients < 5)
                stop("Data must contain at least 5 patients for meaningful analysis")

            # Check for missing values in key variables
            if (any(is.na(data[[timeVar]])))
                stop("Time variable cannot contain missing values")
            if (any(is.na(data[[patientId]])))
                stop("Patient ID variable cannot contain missing values")

            # Validate survival variable if provided
            if (!is.null(survivalVar)) {
                if (!survivalVar %in% names(data))
                    stop(paste("Survival variable", survivalVar, "not found in data"))
                
                if (!all(data[[survivalVar]] %in% c(0, 1, NA)))
                    stop("Survival variable must be binary (0/1) or missing")
            }

            return(TRUE)
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
                                x <- x[!is.na(x)]  # Remove missing values
                                list(
                                    patients = length(x),
                                    events = sum(x == 1),
                                    survivalRate = if(length(x) > 0) mean(x == 0) else 0
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

                if (length(commonIds) > 0) {
                    # Filter data to only include common patients
                    data1 <- data1[data1[[patientId]] %in% commonIds,]
                    data2 <- data2[data2[[patientId]] %in% commonIds,]

                    # Sort both datasets by patient ID to ensure alignment
                    data1 <- data1[order(data1[[patientId]]),]
                    data2 <- data2[order(data2[[patientId]]),]

                    # Calculate transitions for stages and treatments
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
            }

            return(list(
                nodes = nodes,
                flows = flows,
                timePoints = timePoints
            ))
        },

        .prepareSurvivalData = function() {
            if (is.null(self$options$survivalVar))
                return(NULL)

            data <- self$data
            survivalVar <- self$options$survivalVar
            timeVar <- self$options$timeVar
            stageVar <- self$options$stageVar
            treatmentVar <- self$options$treatmentVar
            patientId <- self$options$patientId

            # Create survival data by stage and treatment
            survData <- data %>%
                dplyr::group_by(!!sym(patientId)) %>%
                dplyr::arrange(!!sym(timeVar)) %>%
                dplyr::summarise(
                    maxTime = max(!!sym(timeVar), na.rm = TRUE),
                    event = max(!!sym(survivalVar), na.rm = TRUE),
                    initialStage = first(!!sym(stageVar)),
                    finalTreatment = last(!!sym(treatmentVar)),
                    .groups = 'drop'
                )

            return(survData)
        },

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

            # Prepare enhanced plot data for alluvial visualization
            plotData <- private$.createAlluvialPlotData(stateData)

            # Check if plotData is valid
            if (nrow(plotData) == 0)
                return(FALSE)

            # Define enhanced color schemes
            colors <- private$.getColorScheme(options$colorScheme)

            # Create the enhanced alluvial plot
            p <- ggplot2::ggplot(
                plotData,
                ggplot2::aes(
                    x = time,
                    stratum = category,
                    alluvium = patient_group,
                    y = count,
                    fill = category
                )
            ) +
                ggalluvial::geom_flow(alpha = 0.6, curve_type = "linear") +
                ggalluvial::geom_stratum(alpha = 0.8, width = 0.3) +
                ggplot2::geom_text(
                    stat = "stratum",
                    ggplot2::aes(label = after_stat(stratum)),
                    size = 3,
                    color = "white",
                    fontface = "bold"
                ) +
                ggplot2::scale_fill_manual(values = colors) +
                ggplot2::labs(
                    x = "Time Points",
                    y = "Number of Patients",
                    title = "Patient Treatment Pathways Over Time",
                    subtitle = "Flow visualization showing stage and treatment transitions",
                    fill = "Category"
                ) +
                ggtheme +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
                    legend.position = "right",
                    legend.title = ggplot2::element_text(face = "bold"),
                    plot.title = ggplot2::element_text(face = "bold", size = 14),
                    plot.subtitle = ggplot2::element_text(size = 11, color = "gray40")
                )

            # Add right axis if requested
            if (options$showRightAxis) {
                maxPatients <- max(plotData$count, na.rm = TRUE)
                if (maxPatients > 0) {
                    p <- p + ggplot2::scale_y_continuous(
                        sec.axis = ggplot2::sec_axis(
                            ~ . / maxPatients * 100,
                            name = "Percentage of Patients (%)",
                            labels = function(x) paste0(round(x), "%")
                        )
                    )
                }
            }

            print(p)
            return(TRUE)
        },

        .plotSurvival = function(image, ggtheme, theme, ...) {
            # Check for valid image state
            if (is.null(image$state) || is.null(image$state$data))
                return(FALSE)

            survData <- image$state$data
            options <- image$state$options

            if (nrow(survData) == 0)
                return(FALSE)

            # Create survival object
            surv_obj <- survival::Surv(time = survData$maxTime, event = survData$event)

            # Create survival fit by initial stage
            fit_stage <- survival::survfit(surv_obj ~ initialStage, data = survData)
            
            # Create enhanced survival plot
            p1 <- survminer::ggsurvplot(
                fit_stage,
                data = survData,
                pval = TRUE,
                conf.int = TRUE,
                risk.table = TRUE,
                risk.table.col = "strata",
                break.time.by = 6,
                ggtheme = ggtheme,
                palette = private$.getColorScheme(options$colorScheme),
                title = "Survival Analysis by Initial Disease Stage",
                xlab = "Time (months)",
                ylab = "Survival Probability",
                legend.title = "Disease Stage",
                legend.labs = levels(as.factor(survData$initialStage))
            )

            # Create survival fit by final treatment
            fit_treatment <- survival::survfit(surv_obj ~ finalTreatment, data = survData)
            
            p2 <- survminer::ggsurvplot(
                fit_treatment,
                data = survData,
                pval = TRUE,
                conf.int = TRUE,
                risk.table = TRUE,
                risk.table.col = "strata",
                break.time.by = 6,
                ggtheme = ggtheme,
                palette = private$.getColorScheme(options$colorScheme),
                title = "Survival Analysis by Final Treatment",
                xlab = "Time (months)",
                ylab = "Survival Probability",
                legend.title = "Treatment",
                legend.labs = levels(as.factor(survData$finalTreatment))
            )

            # Print both plots
            print(p1)
            return(TRUE)
        },

        .createAlluvialPlotData = function(stateData) {
            plotData <- data.frame()
            
            for (i in seq_along(stateData$nodes)) {
                node <- stateData$nodes[[i]]
                
                # Process stages
                stages <- node$stages
                if (nrow(stages) > 0) {
                    stageData <- data.frame(
                        time = factor(node$time),
                        category = paste("Stage:", as.character(stages$Var1)),
                        count = stages$Freq,
                        type = "Stage",
                        patient_group = paste("Stage", as.character(stages$Var1), "T", node$time),
                        stringsAsFactors = FALSE
                    )
                    plotData <- rbind(plotData, stageData)
                }

                # Process treatments
                treatments <- node$treatments
                if (nrow(treatments) > 0) {
                    treatmentData <- data.frame(
                        time = factor(node$time),
                        category = paste("Treatment:", as.character(treatments$Var1)),
                        count = treatments$Freq,
                        type = "Treatment",
                        patient_group = paste("Treatment", as.character(treatments$Var1), "T", node$time),
                        stringsAsFactors = FALSE
                    )
                    plotData <- rbind(plotData, treatmentData)
                }
            }

            return(plotData)
        },

        .getColorScheme = function(scheme) {
            colorSchemes <- list(
                clinical = c(
                    "Stage: Resectable" = "#F8D5B5",
                    "Stage: BR/LA" = "#E6A17B", 
                    "Stage: Metastatic" = "#8B4513",
                    "Treatment: Surgery" = "#9ED682",
                    "Treatment: Chemotherapy" = "#6BAED6",
                    "Treatment: Neoadjuvant" = "#FD8D3C",
                    "Treatment: Follow-up" = "#969696",
                    "Treatment: Supportive Care" = "#D4B9DA",
                    "Treatment: Radiation" = "#FF7F50",
                    "Treatment: Immunotherapy" = "#98FB98"
                ),
                colorblind = c(
                    "Stage: Resectable" = "#009E73",
                    "Stage: BR/LA" = "#56B4E9",
                    "Stage: Metastatic" = "#E69F00", 
                    "Treatment: Surgery" = "#CC79A7",
                    "Treatment: Chemotherapy" = "#0072B2",
                    "Treatment: Neoadjuvant" = "#D55E00",
                    "Treatment: Follow-up" = "#999999",
                    "Treatment: Supportive Care" = "#E6AB02",
                    "Treatment: Radiation" = "#F0E442",
                    "Treatment: Immunotherapy" = "#9986A5"
                )
            )

            return(if (scheme == "colorblind") colorSchemes$colorblind else colorSchemes$clinical)
        }
    )
)