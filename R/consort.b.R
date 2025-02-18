#' @title CONSORT Flowchart
#' @importFrom R6 R6Class
#' @import jmvcore

consortClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "consortClass",
    inherit = consortBase,
    private = list(
        .run = function() {

            # Show welcome message if no inputs yet
            if (self$options$initialN == 0) {
                todo <- "
                <br>Welcome to CONSORT Flowchart Generator
                <br><br>
                This tool helps create CONSORT flow diagrams for clinical trials reporting.
                <br><br>
                Enter the numbers for each stage:
                <br>
                1. Enrollment: Initial participants and exclusions
                <br>
                2. Randomization: Number randomized and allocated to each arm
                <br>
                3. Follow-up: Numbers lost and analyzed in each arm
                <br><br>
                The diagram will update automatically as you enter data.
                <hr>
                "
                self$results$todo$setContent(todo)
                return()
            }

            # Prepare data for summary table
            summaryData <- list(
                stages = c("Assessed for eligibility",
                           "Not eligible",
                           "Randomized",
                           paste(self$options$arm1Label, "allocated"),
                           paste(self$options$arm2Label, "allocated")),
                numbers = c(self$options$initialN,
                            self$options$notEligibleN,
                            self$options$randomizedN,
                            self$options$arm1N,
                            self$options$arm2N)
            )

            # Calculate percentages
            summaryData$percents <- c(
                100,
                round(self$options$notEligibleN / self$options$initialN * 100, 1),
                round(self$options$randomizedN / self$options$initialN * 100, 1),
                round(self$options$arm1N / self$options$randomizedN * 100, 1),
                round(self$options$arm2N / self$options$randomizedN * 100, 1)
            )

            # Add rows to summary table
            for(i in seq_along(summaryData$stages)) {
                self$results$summary$addRow(rowKey=i, values=list(
                    stage = summaryData$stages[i],
                    n = summaryData$numbers[i],
                    percent = summaryData$percents[i]
                ))
            }

            # Generate text summary
            textSummary <- sprintf("
            CONSORT Flow Diagram Summary
            ---------------------------
            Enrollment:
            - Initially assessed: %d
            - Not eligible: %d (%d%%)
            - Reasons not eligible: %s

            Randomization:
            - Total randomized: %d (%d%%)

            Allocation:
            %s arm:
            - Allocated: %d
            - Received intervention: %d
            - Lost to follow-up: %d
            - Analyzed: %d

            %s arm:
            - Allocated: %d
            - Received intervention: %d
            - Lost to follow-up: %d
            - Analyzed: %d

            Additional notes:
            %s
            ",
                                   self$options$initialN,
                                   self$options$notEligibleN,
                                   round(self$options$notEligibleN/self$options$initialN*100),
                                   self$options$notEligibleText,
                                   self$options$randomizedN,
                                   round(self$options$randomizedN/self$options$initialN*100),
                                   self$options$arm1Label,
                                   self$options$arm1N,
                                   self$options$arm1ReceivedN,
                                   self$options$arm1LostN,
                                   self$options$arm1AnalyzedN,
                                   self$options$arm2Label,
                                   self$options$arm2N,
                                   self$options$arm2ReceivedN,
                                   self$options$arm2LostN,
                                   self$options$arm2AnalyzedN,
                                   self$options$excludedText
            )

            self$results$text$setContent(textSummary)

            # Save data for plot
            plotData <- list(
                initial = list(
                    n = self$options$initialN,
                    excluded = self$options$notEligibleN,
                    reasons = self$options$notEligibleText
                ),
                randomized = self$options$randomizedN,
                arms = list(
                    list(
                        label = self$options$arm1Label,
                        allocated = self$options$arm1N,
                        received = self$options$arm1ReceivedN,
                        lost = self$options$arm1LostN,
                        analyzed = self$options$arm1AnalyzedN
                    ),
                    list(
                        label = self$options$arm2Label,
                        allocated = self$options$arm2N,
                        received = self$options$arm2ReceivedN,
                        lost = self$options$arm2LostN,
                        analyzed = self$options$arm2AnalyzedN
                    )
                ),
                excluded_reasons = self$options$excludedText
            )

            image <- self$results$plot
            image$setState(plotData)
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Get data from state
            plotData <- image$state
            if (is.null(plotData)) return()

            # Create nodes list
            nodes <- DiagrammeR::create_node_df(
                n = 8,
                label = c(
                    sprintf("Assessed for eligibility\n(n=%d)", plotData$initial$n),
                    sprintf("Excluded (n=%d)\n%s",
                            plotData$initial$excluded,
                            plotData$initial$reasons),
                    sprintf("Randomized\n(n=%d)", plotData$randomized),
                    sprintf("%s\nAllocated (n=%d)\nReceived (n=%d)",
                            plotData$arms[[1]]$label,
                            plotData$arms[[1]]$allocated,
                            plotData$arms[[1]]$received),
                    sprintf("%s\nAllocated (n=%d)\nReceived (n=%d)",
                            plotData$arms[[2]]$label,
                            plotData$arms[[2]]$allocated,
                            plotData$arms[[2]]$received),
                    sprintf("Lost (n=%d)\n%s",
                            plotData$arms[[1]]$lost,
                            plotData$excluded_reasons),
                    sprintf("Lost (n=%d)\n%s",
                            plotData$arms[[2]]$lost,
                            plotData$excluded_reasons),
                    sprintf("Analyzed\n(n=%d)", plotData$arms[[1]]$analyzed),
                    sprintf("Analyzed\n(n=%d)", plotData$arms[[2]]$analyzed)
                ),
                shape = "rectangle",
                width = 2.5,
                height = 1,
                x = c(2, 4, 2, 1, 3, 1, 3, 1, 3),
                y = c(1, 1, 2, 3, 3, 4, 4, 5, 5)
            )

            # Create edges list
            edges <- DiagrammeR::create_edge_df(
                from = c(1, 1, 3, 3, 4, 5, 6, 7),
                to = c(2, 3, 4, 5, 6, 7, 8, 9)
            )

            # Create and render graph
            graph <- DiagrammeR::create_graph(
                nodes_df = nodes,
                edges_df = edges
            ) %>%
                DiagrammeR::add_global_graph_attrs(
                    attr = "rankdir",
                    value = "TB",
                    attr_type = "graph"
                )

            DiagrammeR::render_graph(graph)
            TRUE
        }
    )
)
