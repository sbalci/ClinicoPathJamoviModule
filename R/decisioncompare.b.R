#' @title Compare Medical Decision Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#'

decisioncompareClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisioncompareClass",
        inherit = decisioncompareBase,
        private = list(
            .run = function() {
                if (length(self$options$gold) == 0)
                    return()

                if (nrow(self$data) == 0)
                    stop("Data contains no (complete) rows")

                # Data definition ----
                mydata <- self$data
                mydata <- jmvcore::naOmit(mydata)

                goldPLevel <- jmvcore::constructFormula(terms = self$options$goldPositive)
                goldPLevel <- jmvcore::decomposeFormula(formula = goldPLevel)
                goldPLevel <- unlist(goldPLevel)

                goldVariable <- jmvcore::constructFormula(terms = self$options$gold)
                goldVariable <- jmvcore::decomposeFormula(formula = goldVariable)
                goldVariable <- unlist(goldVariable)

                mydata[[goldVariable]] <- forcats::as_factor(mydata[[goldVariable]])

                # Create comparison results table
                comparisonTable <- self$results$comparisonTable

                # Store metrics for plotting
                plotData <- list()

                # Get test variables and positive levels
                testVariables <- c(self$options$test1,
                                   self$options$test2,
                                   self$options$test3)
                testVariables <- testVariables[!is.null(testVariables) &
                                                   testVariables != ""]

                testPositives <- list()
                if (!is.null(self$options$test1) &&
                    self$options$test1 != "") {
                    testPositives[[self$options$test1]] <- self$options$test1Positive
                }
                if (!is.null(self$options$test2) &&
                    self$options$test2 != "") {
                    testPositives[[self$options$test2]] <- self$options$test2Positive
                }
                if (!is.null(self$options$test3) &&
                    self$options$test3 != "") {
                    testPositives[[self$options$test3]] <- self$options$test3Positive
                }

                # Process each test
                for (i in seq_along(testVariables)) {
                    testVariable <- testVariables[i]

                    # Skip if no test variable
                    if (is.null(testVariable) ||
                        testVariable == "")
                        next

                    # Get positive level for this test
                    testPLevel <- testPositives[[testVariable]]

                    # Get the appropriate table based on test number
                    cTable <- NULL
                    epirTable <- NULL

                    if (testVariable == self$options$test1) {
                        cTable <- self$results$cTable1
                        epirTable <- self$results$epirTable1
                    } else if (testVariable == self$options$test2) {
                        cTable <- self$results$cTable2
                        epirTable <- self$results$epirTable2
                    } else if (testVariable == self$options$test3) {
                        cTable <- self$results$cTable3
                        epirTable <- self$results$epirTable3
                    }

                    # Process test data
                    mydata[[testVariable]] <- forcats::as_factor(mydata[[testVariable]])

                    # Recode data to positive/negative
                    mydata2 <- mydata
                    mydata2 <- mydata2 %>%
                        dplyr::mutate(
                            testVariable2 = dplyr::case_when(
                                .data[[testVariable]] == testPLevel ~ "Positive",
                                NA ~ NA_character_,
                                TRUE ~ "Negative"
                            )
                        ) %>%
                        dplyr::mutate(
                            goldVariable2 = dplyr::case_when(
                                .data[[goldVariable]] == goldPLevel ~ "Positive",
                                NA ~ NA_character_,
                                TRUE ~ "Negative"
                            )
                        )

                    mydata2 <- mydata2 %>%
                        dplyr::mutate(testVariable2 = forcats::fct_relevel(testVariable2, "Positive")) %>%
                        dplyr::mutate(goldVariable2 = forcats::fct_relevel(goldVariable2, "Positive"))

                    # Create confusion table
                    conf_table <- table(mydata2[["testVariable2"]], mydata2[["goldVariable2"]])

                    # Extract values from confusion matrix
                    TP <- conf_table[1, 1]
                    FP <- conf_table[1, 2]
                    FN <- conf_table[2, 1]
                    TN <- conf_table[2, 2]

                    # Populate contingency table
                    cTable$addRow(
                        rowKey = "Test Positive",
                        values = list(
                            newtest = "Test Positive",
                            GP = TP,
                            GN = FP,
                            Total = TP + FP
                        )
                    )

                    cTable$addRow(
                        rowKey = "Test Negative",
                        values = list(
                            newtest = "Test Negative",
                            GP = FN,
                            GN = TN,
                            Total = FN + TN
                        )
                    )

                    cTable$addRow(
                        rowKey = "Total",
                        values = list(
                            newtest = "Total",
                            GP = TP + FN,
                            GN = FP + TN,
                            Total = TP + FP + FN + TN
                        )
                    )

                    # Calculate metrics
                    TotalPop <- TP + TN + FP + FN
                    DiseaseP <- TP + FN
                    DiseaseN <- TN + FP
                    TestP <- TP + FP
                    TestN <- TN + FN

                    Sens <- TP /
                        DiseaseP
                    Spec <- TN /
                        DiseaseN
                    AccurT <- (TP + TN) /
                        TotalPop
                    PPV <- TP /
                        TestP
                    NPV <- TN /
                        TestN
                    LRP <- Sens / (1 - Spec)
                    LRN <- (1 - Sens) / Spec

                    # Add to comparison table
                    comparisonTable$addRow(
                        rowKey = testVariable,
                        values = list(
                            test = testVariable,
                            Sens = Sens,
                            Spec = Spec,
                            AccurT = AccurT,
                            PPV = PPV,
                            NPV = NPV,
                            LRP = LRP,
                            LRN = LRN
                        )
                    )

                    # Add to plot data
                    plotData[[testVariable]] <- list(
                        test = testVariable,
                        Sens = Sens,
                        Spec = Spec,
                        PPV = PPV,
                        NPV = NPV,
                        LRP = LRP,
                        LRN = LRN
                    )

                    # Calculate confidence intervals if requested
                    if (self$options$ci) {
                        epirresult <- epiR::epi.tests(dat = conf_table)
                        epirresult2 <- summary(epirresult)
                        epirresult2 <- as.data.frame(epirresult2) %>%
                            tibble::rownames_to_column(.data = ., var = 'statsabv')

                        epirresult2$statsnames <-
                            c(
                                "Apparent prevalence",
                                "True prevalence",
                                "Test sensitivity",
                                "Test specificity",
                                "Diagnostic accuracy",
                                "Diagnostic odds ratio",
                                "Number needed to diagnose",
                                "Youden's index",
                                "Positive predictive value",
                                "Negative predictive value",
                                "Likelihood ratio of a positive test",
                                "Likelihood ratio of a negative test",
                                "Proportion of subjects with the outcome ruled out",
                                "Proportion of subjects with the outcome ruled in",
                                "Proportion of false positives",
                                "Proportion of false negative",
                                "False Discovery Rate",
                                "False Omission Rate"
                            )

                        ratiorows <- c(
                            "ap",
                            "tp",
                            "se",
                            "sp",
                            "diag.ac",
                            "pv.pos",
                            "pv.neg",
                            "p.tpdn",
                            "p.tndp",
                            "p.dntp",
                            "p.dptn"
                        )

                        epirresult_ratio <- epirresult2[epirresult2$statistic %in% ratiorows, ]

                        data_frame <- epirresult_ratio
                        for (i in seq_along(data_frame[, 1, drop =
                                                       T])) {
                            epirTable$addRow(rowKey = i,
                                             values = c(data_frame[i, ]))
                        }
                    }
                }

                # Set plot data for visualization
                if (self$options$plot) {
                    image1 <- self$results$plot1
                    image1$setState(plotData)
                }
            },

            .plot1 = function(image1, ggtheme, ...) {
                plotData <- image1$state

                # Prepare data frame for plotting
                df <- data.frame(
                    test = character(),
                    metric = character(),
                    value = numeric(),
                    stringsAsFactors = FALSE
                )

                for (test_name in names(plotData)) {
                    test_data <- plotData[[test_name]]

                    # Add sensitivity
                    df <- rbind(
                        df,
                        data.frame(
                            test = test_name,
                            metric = "Sensitivity",
                            value = test_data$Sens
                        )
                    )

                    # Add specificity
                    df <- rbind(
                        df,
                        data.frame(
                            test = test_name,
                            metric = "Specificity",
                            value = test_data$Spec
                        )
                    )

                    # Add PPV
                    df <- rbind(df,
                                data.frame(
                                    test = test_name,
                                    metric = "PPV",
                                    value = test_data$PPV
                                ))

                    # Add NPV
                    df <- rbind(df,
                                data.frame(
                                    test = test_name,
                                    metric = "NPV",
                                    value = test_data$NPV
                                ))
                }

                # Create plot
                plot <- ggplot2::ggplot(df, ggplot2::aes(x = metric, y = value, fill = test)) +
                    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
                    ggplot2::coord_flip() +
                    ggplot2::scale_y_continuous(labels = scales::percent) +
                    ggplot2::labs(
                        title = "Comparison of Tests",
                        x = "",
                        y = "Value",
                        fill = "Test"
                    ) +
                    ggtheme +
                    ggplot2::theme(legend.position = "bottom")

                print(plot)
                TRUE
            }
        )
    )
