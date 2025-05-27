#' @title Compare Medical Decision Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#'

decisioncompareClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisioncompareClass",
        inherit = decisioncompareBase,
        private = list(

            # init ----
            .init = function() {
                self$results$epirTable1$setVisible(FALSE)
                self$results$epirTable2$setVisible(FALSE)
                self$results$epirTable3$setVisible(FALSE)

                self$results$cTable1$setVisible(FALSE)
                self$results$cTable2$setVisible(FALSE)
                self$results$cTable3$setVisible(FALSE)

            }

            ,
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

                # Handle original data display
                if (self$options$od) {
                    # Create frequency tables for original data
                    freq_table <- table(mydata[[goldVariable]])
                    self$results$text1$setContent(freq_table)

                    # Create a cross-tabulation of all tests with gold standard
                    test_vars <- c(self$options$test1,
                                   self$options$test2,
                                   self$options$test3)
                    test_vars <- test_vars[!is.null(test_vars) &
                                               test_vars != ""]

                    if (length(test_vars) > 0) {
                        html_tables <- ""
                        for (test_var in test_vars) {
                            # Create cross-tabulation
                            cross_tab <- table(mydata[[test_var]], mydata[[goldVariable]])
                            html_table <- knitr::kable(
                                cross_tab,
                                format = "html",
                                caption = paste(
                                    "Cross-tabulation of",
                                    test_var,
                                    "and",
                                    goldVariable
                                )
                            )
                            html_tables <- paste(html_tables, html_table, "<br><br>")
                        }
                        self$results$text2$setContent(html_tables)
                    }
                }

                # Create comparison results table
                comparisonTable <- self$results$comparisonTable

                # Store metrics for plotting
                plotData <- list()

                # Create variable to store test results for statistical comparison
                all_test_results <- list()
                test_metrics <- list()

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

                        if (self$options$ci) {
                        self$results$epirTable1$setVisible(TRUE)
                        }

                        self$results$cTable1$setVisible(TRUE)

                        cTable <- self$results$cTable1
                        epirTable <- self$results$epirTable1

                    } else if (testVariable == self$options$test2) {

                        if (self$options$ci) {
                        self$results$epirTable2$setVisible(TRUE)
                        }

                        self$results$cTable2$setVisible(TRUE)

                        cTable <- self$results$cTable2
                        epirTable <- self$results$epirTable2

                    } else if (testVariable == self$options$test3) {

                        if (self$options$ci) {
                        self$results$epirTable3$setVisible(TRUE)
                        }

                        self$results$cTable3$setVisible(TRUE)

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

                    # Store test results for statistical comparison
                    all_test_results[[testVariable]] <- mydata2[["testVariable2"]]

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

                    # Add footnotes if requested
                    if (self$options$fnote) {
                        cTable$addFootnote(rowKey = "Test Positive",
                                           col = "GP",
                                           "True Positive (TP)")
                        cTable$addFootnote(rowKey = "Test Positive",
                                           col = "GN",
                                           "False Positive (FP)")
                        cTable$addFootnote(rowKey = "Test Negative",
                                           col = "GP",
                                           "False Negative (FN)")
                        cTable$addFootnote(rowKey = "Test Negative",
                                           col = "GN",
                                           "True Negative (TN)")
                    }

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

                    # Store metrics for statistical comparison
                    test_metrics[[testVariable]] <- list(
                        Sens = Sens,
                        Spec = Spec,
                        PPV = PPV,
                        NPV = NPV,
                        conf_table = conf_table
                    )

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

                    # Add footnotes to comparison table if requested
                    if (self$options$fnote) {
                        comparisonTable$addFootnote(
                            rowKey = testVariable,
                            col = "Sens",
                            "Sensitivity = TP/(TP+FN) = Probability of positive test among diseased"
                        )
                        comparisonTable$addFootnote(
                            rowKey = testVariable,
                            col = "Spec",
                            "Specificity = TN/(TN+FP) = Probability of negative test among healthy"
                        )
                        comparisonTable$addFootnote(
                            rowKey = testVariable,
                            col = "PPV",
                            "Positive Predictive Value = TP/(TP+FP) = Probability of disease when test is positive"
                        )
                        comparisonTable$addFootnote(
                            rowKey = testVariable,
                            col = "NPV",
                            "Negative Predictive Value = TN/(TN+FN) = Probability of being healthy when test is negative"
                        )
                    }

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

                # Perform statistical comparisons if requested
                if (self$options$statComp &&
                    length(testVariables) >= 2) {
                    # Get the tables
                    mcnemarTable <- self$results$mcnemarTable
                    diffTable <- self$results$diffTable

                    # Generate all pairwise combinations of tests
                    test_pairs <- combn(testVariables, 2, simplify = FALSE)

                    # For each pair, perform McNemar's test and calculate CIs for differences
                    for (pair in test_pairs) {
                        test1 <- pair[1]
                        test2 <- pair[2]
                        comparison_name <- paste(test1, "vs", test2)

                        # Create contingency table for McNemar's test
                        # We need to count discordant pairs (where test results differ)
                        test1_results <- all_test_results[[test1]]
                        test2_results <- all_test_results[[test2]]

                        # Create a 2x2 contingency table for McNemar's test
                        mcnemar_table <- table(test1_results, test2_results)

                        # Perform McNemar's test
                        mcnemar_result <- stats::mcnemar.test(mcnemar_table)

                        # Add to McNemar's test table
                        mcnemarTable$addRow(
                            rowKey = comparison_name,
                            values = list(
                                comparison = comparison_name,
                                stat = mcnemar_result$statistic,
                                df = mcnemar_result$parameter,
                                p = mcnemar_result$p.value
                            )
                        )

                        # Calculate confidence intervals for differences in metrics
                        metrics <- c("Sens", "Spec", "PPV", "NPV")

                        for (metric in metrics) {
                            # Extract values for each test
                            value1 <- test_metrics[[test1]][[metric]]
                            value2 <- test_metrics[[test2]][[metric]]

                            # Calculate difference
                            diff <- value1 - value2

                            # Get total sample size from confusion tables
                            n1 <- sum(test_metrics[[test1]][["conf_table"]])
                            n2 <- sum(test_metrics[[test2]][["conf_table"]])

                            # Calculate standard error for the difference
                            se_diff <- sqrt((value1 * (1 - value1) / n1) + (value2 * (1 - value2) / n2))

                            # Calculate 95% CI
                            z <- 1.96  # Z-score for 95% CI
                            lower <- diff - z * se_diff
                            upper <- diff + z * se_diff

                            # Add to difference table
                            diffTable$addRow(
                                rowKey = paste(comparison_name, metric, sep = "_"),
                                values = list(
                                    comparison = comparison_name,
                                    metric = metric,
                                    diff = diff,
                                    lower = lower,
                                    upper = upper
                                )
                            )
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
