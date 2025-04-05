#' @title Combine Medical Decision Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#'

decisioncombineClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisioncombineClass",
        inherit = decisioncombineBase,
        private = list(
            .init = function() {
                cTable <- self$results$cTable

                cTable$addRow(rowKey = "Test Positive",
                              values = list(newtest = "Test Positive"))

                cTable$addRow(rowKey = "Test Negative",
                              values = list(newtest = "Test Negative"))

                cTable$addRow(rowKey = "Total", values = list(newtest = "Total"))
            },

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

                # Create recoded data with positives and negatives for each test
                mydata2 <- mydata

                # Add gold standard recode
                mydata2 <- mydata2 %>%
                    dplyr::mutate(
                        goldVariable2 = dplyr::case_when(
                            .data[[goldVariable]] == goldPLevel ~ "Positive",
                            NA ~ NA_character_,
                            TRUE ~ "Negative"
                        )
                    )

                mydata2 <- mydata2 %>%
                    dplyr::mutate(goldVariable2 = forcats::fct_relevel(goldVariable2, "Positive"))

                # Process individual tests if requested
                showIndividual <- self$options$showIndividual

                # First create individual test binary columns
                for (i in seq_along(testVariables)) {
                    testVariable <- testVariables[i]
                    testPLevel <- testPositives[[testVariable]]

                    # Process test data
                    mydata[[testVariable]] <- forcats::as_factor(mydata[[testVariable]])

                    # Add recode for this test
                    binary_col_name <- paste0(testVariable, "_bin")
                    mydata2 <- mydata2 %>%
                        dplyr::mutate(
                            !!binary_col_name := dplyr::case_when(.data[[testVariable]] == testPLevel ~ 1, NA ~ NA_real_, TRUE ~ 0)
                        )

                    # Show individual test results if requested
                    if (showIndividual) {
                        # Recode this test
                        recode_col_name <- paste0(testVariable, "_recode")
                        mydata2 <- mydata2 %>%
                            dplyr::mutate(
                                !!recode_col_name := dplyr::case_when(
                                    .data[[testVariable]] == testPLevel ~ "Positive",
                                    NA ~ NA_character_,
                                    TRUE ~ "Negative"
                                )
                            )

                        mydata2 <- mydata2 %>%
                            dplyr::mutate(!!recode_col_name := forcats::fct_relevel(.data[[recode_col_name]], "Positive"))

                        # Create contingency table
                        conf_table <- table(mydata2[[recode_col_name]], mydata2[["goldVariable2"]])

                        # Extract values
                        TP <- conf_table[1, 1]
                        FP <- conf_table[1, 2]
                        FN <- conf_table[2, 1]
                        TN <- conf_table[2, 2]

                        # Get appropriate individual table
                        indTable <- NULL
                        if (testVariable == self$options$test1) {
                            indTable <- self$results$indTable1
                        } else if (testVariable == self$options$test2) {
                            indTable <- self$results$indTable2
                        } else if (testVariable == self$options$test3) {
                            indTable <- self$results$indTable3
                        }

                        if (!is.null(indTable)) {
                            indTable$addRow(
                                rowKey = "Test Positive",
                                values = list(
                                    newtest = "Test Positive",
                                    GP = TP,
                                    GN = FP,
                                    Total = TP + FP
                                )
                            )

                            indTable$addRow(
                                rowKey = "Test Negative",
                                values = list(
                                    newtest = "Test Negative",
                                    GP = FN,
                                    GN = TN,
                                    Total = FN + TN
                                )
                            )

                            indTable$addRow(
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
                                indTable$addFootnote(rowKey = "Test Positive",
                                                     col = "GP",
                                                     "True Positive (TP)")
                                indTable$addFootnote(rowKey = "Test Positive",
                                                     col = "GN",
                                                     "False Positive (FP)")
                                indTable$addFootnote(rowKey = "Test Negative",
                                                     col = "GP",
                                                     "False Negative (FN)")
                                indTable$addFootnote(rowKey = "Test Negative",
                                                     col = "GN",
                                                     "True Negative (TN)")
                            }
                        }
                    }
                }

                # Combine test results according to the rule
                binary_cols <- paste0(testVariables, "_bin")
                combRule <- self$options$combRule

                if (combRule == "any") {
                    # OR rule: positive if any test is positive
                    mydata2 <- mydata2 %>%
                        dplyr::mutate(combined_result = dplyr::case_when(
                            rowSums(dplyr::across(dplyr::all_of(
                                binary_cols
                            ))) > 0 ~ "Positive",
                            TRUE ~ "Negative"
                        ))
                } else if (combRule == "all") {
                    # AND rule: positive only if all tests are positive
                    mydata2 <- mydata2 %>%
                        dplyr::mutate(
                            combined_result = dplyr::case_when(
                                rowSums(dplyr::across(dplyr::all_of(
                                    binary_cols
                                ))) == length(testVariables) ~ "Positive",
                                TRUE ~ "Negative"
                            )
                        )
                } else if (combRule == "majority") {
                    # Majority rule: positive if more than half of tests are positive
                    mydata2 <- mydata2 %>%
                        dplyr::mutate(
                            combined_result = dplyr::case_when(
                                rowSums(dplyr::across(dplyr::all_of(
                                    binary_cols
                                ))) > length(testVariables) / 2 ~ "Positive",
                                TRUE ~ "Negative"
                            )
                        )
                }

                mydata2 <- mydata2 %>%
                    dplyr::mutate(combined_result = forcats::fct_relevel(combined_result, "Positive"))

                # Create contingency table for combined test
                conf_table <- table(mydata2[["combined_result"]], mydata2[["goldVariable2"]])

                # Extract values
                TP <- conf_table[1, 1]
                FP <- conf_table[1, 2]
                FN <- conf_table[2, 1]
                TN <- conf_table[2, 2]

                # Populate main contingency table
                cTable <- self$results$cTable

                cTable$setRow(
                    rowKey = "Test Positive",
                    values = list(
                        newtest = "Test Positive",
                        GP = TP,
                        GN = FP,
                        Total = TP + FP
                    )
                )

                cTable$setRow(
                    rowKey = "Test Negative",
                    values = list(
                        newtest = "Test Negative",
                        GP = FN,
                        GN = TN,
                        Total = FN + TN
                    )
                )

                cTable$setRow(
                    rowKey = "Total",
                    values = list(
                        newtest = "Total",
                        GP = TP + FN,
                        GN = FP + TN,
                        Total = TP + FP + FN + TN
                    )
                )

                # Add footnotes to main contingency table if requested
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
                TestT <- TP + TN
                TestW <- FP + FN

                Sens <- TP /
                    DiseaseP
                Spec <- TN /
                    DiseaseN
                AccurT <- TestT /
                    TotalPop
                PrevalenceD <- DiseaseP /
                    TotalPop
                PPV <- TP /
                    TestP
                NPV <- TN /
                    TestN

                pp <- self$options$pp
                pprob <- self$options$pprob

                if (pp) {
                    # Known prior probability from population
                    PriorProb <- pprob
                } else {
                    # From ConfusionMatrix
                    PriorProb <- PrevalenceD
                }

                PostTestProbDisease <- (PriorProb * Sens) /
                    ((PriorProb * Sens) + ((1 - PriorProb) * (1 - Spec)))
                PostTestProbHealthy <- ((1 - PriorProb) * Spec) /
                    (((1 - PriorProb) * Spec) + (PriorProb * (1 - Sens)))

                LRP <- Sens / (1 - Spec)
                LRN <- (1 - Sens) / Spec

                # nTable Populate Table ----
                nTable <- self$results$nTable
                nTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "n",
                        TotalPop = TotalPop,
                        DiseaseP = DiseaseP,
                        DiseaseN = DiseaseN,
                        TestP = TestP,
                        TestN = TestN,
                        TestT = TestT,
                        TestW = TestW
                    )
                )

                # ratioTable Populate Table ----
                ratioTable <- self$results$ratioTable
                ratioTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "Ratios",
                        Sens = Sens,
                        Spec = Spec,
                        AccurT = AccurT,
                        PrevalenceD = PriorProb,
                        PPV = PPV,
                        NPV = NPV,
                        PostTestProbDisease = PostTestProbDisease,
                        PostTestProbHealthy = PostTestProbHealthy,
                        LRP = LRP,
                        LRN = LRN
                    )
                )

                # Add footnotes if requested
                if (self$options$fnote) {
                    # nTable footnotes
                    nTable$addFootnote(rowNo = 1,
                                       col = "TotalPop",
                                       "Total Number of Subjects")
                    nTable$addFootnote(rowNo = 1,
                                       col = "DiseaseP",
                                       "Total Number of Subjects with Disease")
                    nTable$addFootnote(rowNo = 1,
                                       col = "DiseaseN",
                                       "Total Number of Healthy Subjects")
                    nTable$addFootnote(rowNo = 1,
                                       col = "TestP",
                                       "Total Number of Positive Tests")
                    nTable$addFootnote(rowNo = 1,
                                       col = "TestN",
                                       "Total Number of Negative Tests")
                    nTable$addFootnote(rowNo = 1,
                                       col = "TestT",
                                       "Total Number of True Test Results")
                    nTable$addFootnote(rowNo = 1,
                                       col = "TestW",
                                       "Total Number of Wrong Test Results")

                    # ratioTable footnotes
                    ratioTable$addFootnote(
                        rowNo = 1,
                        col = "Sens",
                        "Sensitivity (True Positives among Diseased)"
                    )
                    ratioTable$addFootnote(
                        rowNo = 1,
                        col = "Spec",
                        "Specificity (True Negatives among Healthy)"
                    )
                    ratioTable$addFootnote(rowNo = 1,
                                           col = "AccurT",
                                           "Accuracy (True Test Result Ratio)")
                    ratioTable$addFootnote(rowNo = 1,
                                           col = "PrevalenceD",
                                           "Disease Prevalence in this population")
                    ratioTable$addFootnote(
                        rowNo = 1,
                        col = "PPV",
                        "Positive Predictive Value (Probability of having disease after a positive test using this experimental population)"
                    )
                    ratioTable$addFootnote(
                        rowNo = 1,
                        col = "NPV",
                        "Negative Predictive Value (Probability of being healthy after a negative test using this experimental population)"
                    )
                    ratioTable$addFootnote(
                        rowNo = 1,
                        col = "PostTestProbDisease",
                        "Post-test Probability of Having Disease (Probability of having disease after a positive test using known Population Prevalence)"
                    )
                    ratioTable$addFootnote(
                        rowNo = 1,
                        col = "PostTestProbHealthy",
                        "Post-test Probability of Being Healthy (Probability of being healthy after a negative test using known Population Prevalence)"
                    )
                }

                # Calculate confidence intervals if requested
                if (self$options$ci) {
                    # epiR calculations
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

                    numberrows <- c("diag.or", "nndx", "youden", "lr.pos", "lr.neg")

                    epirresult_number <- epirresult2[epirresult2$statistic %in% numberrows, ]
                    epirresult_ratio <- epirresult2[epirresult2$statistic %in% ratiorows, ]

                    # epirTable_ratio
                    epirTable_ratio <- self$results$epirTable_ratio
                    data_frame <- epirresult_ratio
                    for (i in seq_along(data_frame[, 1, drop =
                                                   T])) {
                        epirTable_ratio$addRow(rowKey = i,
                                               values = c(data_frame[i, ]))
                    }

                    # epirTable_number
                    epirTable_number <- self$results$epirTable_number
                    data_frame <- epirresult_number
                    for (i in seq_along(data_frame[, 1, drop =
                                                   T])) {
                        epirTable_number$addRow(rowKey = i,
                                                values = c(data_frame[i, ]))
                    }
                }

                # Set data for Fagan nomogram if requested
                if (self$options$fagan) {
                    plotData1 <- list(
                        "Prevalence" = PriorProb,
                        "Sens" = Sens,
                        "Spec" = Spec,
                        "Plr" = LRP,
                        "Nlr" = LRN
                    )

                    image1 <- self$results$plot1
                    image1$setState(plotData1)
                }
            },

            .plot1 = function(image1, ggtheme, ...) {
                plotData1 <- image1$state

                plot1 <- nomogrammer(
                    Prevalence = plotData1$Prevalence,
                    Sens = plotData1$Sens,
                    Spec = plotData1$Spec,
                    Plr = plotData1$Plr,
                    Nlr = plotData1$Nlr,
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = (14 /
                                     5),
                    Verbose = TRUE
                )

                print(plot1)
                TRUE
            }
        )
    )
