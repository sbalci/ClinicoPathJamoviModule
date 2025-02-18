#' @title Medical Decision Analysis
#' @description Implements comprehensive medical decision analysis including:
#' @details This module provides tools for analyzing diagnostic test performance
#'   with options for various visualization methods and statistical comparisons.
#'   - Sensitivity, specificity and predictive values
#' @section Usage:
#'   1. Provide test and reference standard data
#'   2. Select analysis options
#'   3. View results in tables and plots
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import boot
#' @importFrom stats quantile qnorm
#' @importFrom dplyr %>% mutate case_when
#' @importFrom forcats as_factor fct_relevel
#' @importFrom epiR epi.tests


#  @references
#    - DeLong et al. (1988) for ROC comparison
#   - Hanley & McNeil (1982) for AUC confidence intervals
#    - ROC curve analysis with confidence intervals
#    - Multiple test comparison
#    - Bootstrapped confidence intervals



decisionClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisionClass",
        inherit = decisionBase,
        private = list(
            .state = NULL,

            # Add state field for storing data

            # .cache = new.env(),

            # .get_cached = function(key) {
            #     if (exists(key, envir=private$.cache)) {
            #         return(get(key, envir=private$.cache))
            #     }
            #     return(NULL)
            # },

            # .set_cached = function(key, value) {
            #     assign(key, value, envir=private$.cache)
            # },

            .init = function() {
                cTable <- self$results$cTable

                cTable$addRow(rowKey = "Test Positive",
                              values = list(newtest = "Test Positive"))

                cTable$addRow(rowKey = "Test Negative",
                              values = list(newtest = "Test Negative"))

                cTable$addRow(rowKey = "Total", values = list(newtest = "Total"))




                # cTable2 <- self$results$cTable2
                # cTable2$addRow(rowKey = "Test Positive",
                #               values = list(newtest = "Test Positive"))
                # cTable2$addRow(rowKey = "Test Negative",
                #               values = list(newtest = "Test Negative"))
                # cTable2$addRow(rowKey = "Total", values = list(newtest = "Total"))

            }



            # Helper functions
            # .calculate_basic_metrics = function(TP, FP, TN, FN) {
            #     metrics <- list(
            #         total_pop = TP + TN + FP + FN,
            #         disease_pos = TP + FN,
            #         disease_neg = TN + FP,
            #         test_pos = TP + FP,
            #         test_neg = TN + FN,
            #         test_true = TP + TN,
            #         test_wrong = FP + FN,
            #         sensitivity = TP / (TP + FN),
            #         specificity = TN / (TN + FP),
            #         accuracy = (TP + TN) / (TP + TN + FP + FN),
            #         ppv = TP / (TP + FP),
            #         npv = TN / (TN + FN),
            #         plr = (TP / (TP + FN)) / (1 - (TN / (TN + FP))),
            #         nlr = (1 - (TP / (TP + FN))) / (TN / (TN + FP))
            #     )
            #     return(metrics)
            # },

            # .bootstrap_ci = function(data, index, metrics_func) {
            #     # Resample data
            #     resampled <- data[index,]
            #     # Calculate confusion matrix
            #     conf_mat <- table(resampled$test, resampled$gold)
            #     # Extract values
            #     TP <- conf_mat[2,2]
            #     FP <- conf_mat[2,1]
            #     TN <- conf_mat[1,1]
            #     FN <- conf_mat[1,2]
            #     # Calculate and return metrics
            #     metrics <- metrics_func(TP, FP, TN, FN)
            #     return(unlist(metrics))
            # },

            # .calculate_confidence_intervals = function(data, metrics, conf_level = 0.95) {
            #     # Perform bootstrap
            #     boot_results <- boot::boot(
            #         data = data,
            #         statistic = private$.bootstrap_ci,
            #         R = 2000,
            #         metrics_func = private$.calculate_basic_metrics
            #     )
            #
            #     # Calculate CIs for each metric
            #     ci_results <- list()
            #     metric_names <- names(metrics)
            #     for(i in seq_along(metrics)) {
            #         ci <- boot::boot.ci(boot_results,
            #                             type = "perc",
            #                             index = i,
            #                             conf = conf_level)
            #         ci_results[[metric_names[i]]] <- list(
            #             lower = ci$percent[4],
            #             upper = ci$percent[5]
            #         )
            #     }
            #     return(ci_results)
            # },

            # .create_roc_plot = function(sens, spec, ci = NULL) {
            #     roc_data <- data.frame(
            #         FPR = 1 - spec,
            #         TPR = sens
            #     )
            #
            #     p <- ggplot(roc_data, aes(x = FPR, y = TPR)) +
            #         geom_point(size = 3, color = "blue") +
            #         geom_line(size = 1) +
            #         geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
            #         coord_equal() +
            #         theme_minimal() +
            #         labs(
            #             x = "False Positive Rate (1 - Specificity)",
            #             y = "True Positive Rate (Sensitivity)",
            #             title = "ROC Curve"
            #         )
            #
            #     if (!is.null(ci)) {
            #         # Add confidence bands if provided
            #         p <- p + geom_ribbon(
            #             data = ci,
            #             aes(ymin = lower, ymax = upper),
            #             alpha = 0.2
            #         )
            #     }
            #
            #     return(p)
            # },

            # .compare_tests = function(test1_metrics, test2_metrics, n) {
            #     # Calculate statistical differences between tests
            #     # Using McNemar's test for paired comparisons
            #     mcnemar_stat <- (abs(test1_metrics$sensitivity - test2_metrics$sensitivity))^2 * n
            #     p_value <- 1 - pchisq(mcnemar_stat, df = 1)
            #
            #     return(list(
            #         diff_sens = test1_metrics$sensitivity - test2_metrics$sensitivity,
            #         diff_spec = test1_metrics$specificity - test2_metrics$specificity,
            #         mcnemar_stat = mcnemar_stat,
            #         p_value = p_value
            #     ))
            # }

            # ,
            # .prepare_data = function(data, vars) {
            #     # Handle missing values
            #     data <- jmvcore::naOmit(data)
            #
            #     # Convert variables to factors
            #     data[[vars$test]] <- forcats::as_factor(data[[vars$test]])
            #     data[[vars$gold]] <- forcats::as_factor(data[[vars$gold]])
            #
            #     # Recode variables to binary (Positive/Negative)
            #     data <- data %>%
            #         dplyr::mutate(
            #             test = dplyr::case_when(
            #                 .data[[vars$test]] == vars$test_pos ~ "Positive",
            #                 TRUE ~ "Negative"
            #             ),
            #             gold = dplyr::case_when(
            #                 .data[[vars$gold]] == vars$gold_pos ~ "Positive",
            #                 TRUE ~ "Negative"
            #             )
            #         )
            #
            #     # Convert to factors with proper levels
            #     data$test <- forcats::fct_relevel(data$test, "Positive")
            #     data$gold <- forcats::fct_relevel(data$gold, "Positive")
            #
            #     return(data)
            # },

            # .populate_main_tables = function(conf_matrix, metrics) {
            #     # Populate confusion matrix table
            #     cTable2 <- self$results$cTable2
            #
            #     # conf_matrix <- metrics # table(self$data$test, self$data$gold)
            #
            #     cTable2$setRow(rowKey = "Test Positive", values = list(
            #         newtest = "Test Positive",
            #         GP = conf_matrix[2,2],  # TP
            #         GN = conf_matrix[2,1],  # FP
            #         Total = sum(conf_matrix[2,])
            #     ))
            #
            #     cTable2$setRow(rowKey = "Test Negative", values = list(
            #         newtest = "Test Negative",
            #         GP = conf_matrix[1,2],  # FN
            #         GN = conf_matrix[1,1],  # TN
            #         Total = sum(conf_matrix[1,])
            #     ))
            #
            #     cTable2$setRow(rowKey = "Total", values = list(
            #         newtest = "Total",
            #         GP = sum(conf_matrix[,2]),
            #         GN = sum(conf_matrix[,1]),
            #         Total = sum(conf_matrix)
            #     ))
            #
            #     # Populate metrics table
            #     ratioTable2 <- self$results$ratioTable2
            #
            #     ratioTable2$setRow(rowNo = 1, values = list(
            #         tablename = "Ratios",
            #         Sens = metrics$sensitivity,
            #         Spec = metrics$specificity,
            #         AccurT = metrics$accuracy,
            #         # PrevalenceD = self$options$pp ? self$options$pprob : metrics$disease_pos/metrics$total_pop,
            #         PPV = metrics$ppv,
            #         NPV = metrics$npv,
            #         # PostTestProbDisease = self$calculate_post_test_prob(metrics, positive = TRUE),
            #         # PostTestProbHealthy = self$calculate_post_test_prob(metrics, positive = FALSE),
            #         LRP = metrics$plr,
            #         LRN = metrics$nlr
            #     ))
            #
            #     # Add footnotes if requested
            #     # if (self$options$fnote) {
            #     #     self$add_table_footnotes()
            #     # }
            # },

            # .populate_ci_tables = function(ci_results) {
            #     if (!self$options$ci) return()
            #
            #     # Get epiR results
            #     conf_matrix <- table(self$data$test, self$data$gold)
            #     epir_results <- epiR::epi.tests(conf_matrix)
            #     epir_summary <- summary(epir_results)
            #
            #     # Populate ratio CI table
            #     ratio_table <- self$results$epirTable_ratio
            #     ratio_metrics <- c("se", "sp", "pv.pos", "pv.neg")
            #
            #     for (metric in ratio_metrics) {
            #         if (metric %in% rownames(epir_summary)) {
            #             ratio_table$addRow(rowKey = metric, values = list(
            #                 statsnames = epir_summary[metric, "statistic"],
            #                 est = epir_summary[metric, "est"],
            #                 lower = epir_summary[metric, "lower"],
            #                 upper = epir_summary[metric, "upper"]
            #             ))
            #         }
            #     }
            #
            #     # Populate number CI table
            #     number_table <- self$results$epirTable_number
            #     number_metrics <- c("diag.or", "nndx", "youden")
            #
            #     for (metric in number_metrics) {
            #         if (metric %in% rownames(epir_summary)) {
            #             number_table$addRow(rowKey = metric, values = list(
            #                 statsnames = epir_summary[metric, "statistic"],
            #                 est = epir_summary[metric, "est"],
            #                 lower = epir_summary[metric, "lower"],
            #                 upper = epir_summary[metric, "upper"]
            #             ))
            #         }
            #     }
            # },

            # .populate_comparison_tables = function(comparison_results) {
            #     if (!self$options$compare_tests) return()
            #
            #     comparison_table <- self$results$comparison_table
            #     comparison_table$setRow(rowNo = 1, values = list(
            #         diff_sens = comparison_results$diff_sens,
            #         diff_spec = comparison_results$diff_spec,
            #         mcnemar_stat = comparison_results$mcnemar_stat,
            #         p_value = comparison_results$p_value
            #     ))
            # },

            # calculate_post_test_prob = function(metrics, positive = TRUE) {
            #     prior_prob <- self$options$pp ? self$options$pprob : metrics$disease_pos/metrics$total_pop
            #
            #     if (positive) {
            #         # Calculate positive post-test probability
            #         return((prior_prob * metrics$sensitivity) /
            #                    (prior_prob * metrics$sensitivity + (1 - prior_prob) * (1 - metrics$specificity)))
            #     } else {
            #         # Calculate negative post-test probability
            #         return(((1 - prior_prob) * metrics$specificity) /
            #                    ((1 - prior_prob) * metrics$specificity + prior_prob * (1 - metrics$sensitivity)))
            #     }
            # },

            # add_table_footnotes = function() {
            #     # Add footnotes to ratio table
            #     ratioTable <- self$results$ratioTable
            #
            #     footnotes <- list(
            #         Sens = "Sensitivity (True Positives among Diseased)",
            #         Spec = "Specificity (True Negatives among Healthy)",
            #         AccurT = "Accuracy (True Test Result Ratio)",
            #         PrevalenceD = "Disease Prevalence in this population",
            #         PPV = "Positive Predictive Value (Probability of disease given positive test)",
            #         NPV = "Negative Predictive Value (Probability of no disease given negative test)",
            #         PostTestProbDisease = "Post-test Probability of Disease",
            #         PostTestProbHealthy = "Post-test Probability of Being Healthy",
            #         LRP = "Positive Likelihood Ratio",
            #         LRN = "Negative Likelihood Ratio"
            #     )
            #
            #     for (col in names(footnotes)) {
            #         ratioTable$addFootnote(rowNo = 1, col = col, footnotes[[col]])
            #     }
            # }

            ,
            .run = function() {

                # private$.state <- list()  # Initialize empty state

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

                # Input validation
                # if (is.null(self$data) || nrow(self$data) == 0) {
                #   jmvcore::reject("No data provided for analysis")
                #   return()
                # }
                # if (any(sapply(vars, is.null))) {
                #   jmvcore::reject("Missing required variables")
                #   return()
                # }

                # TODO

                # todo <- glue::glue( 'This Module is still under development - - ' )
                # self$results$todo$setContent(todo)


                if (length(self$options$testPositive) + length(self$options$newtest) +
                    length(self$options$goldPositive) + length(self$options$gold) <
                    4)
                    return()

                if (nrow(self$data) == 0)
                    stop("Data contains no (complete) rows")

                # Input validation
                if (is.null(self$data) || nrow(self$data) == 0) {
                    stop("No data provided for analysis")
                }

                # Validate test and gold standard variables
                if (length(self$options$testPositive) +
                    length(self$options$newtest) +
                    length(self$options$goldPositive) +
                    length(self$options$gold) < 4) {
                    stop("Missing required variables: test and gold standard must be specified")
                }

                # Validate prevalence if specified
                if (self$options$pp &&
                    (self$options$pprob <= 0 || self$options$pprob >= 1)) {
                    stop("Prior probability must be between 0 and 1")
                }

                # Add NA handling for data
                mydata <- self$data
                mydata <- jmvcore::naOmit(mydata)
                if (nrow(mydata) < nrow(self$data)) {
                    warning(sprintf("Removed %d rows with missing values",
                                    nrow(self$data) - nrow(mydata)))
                }


                # Data definition ----
                mydata <- self$data

                mydata <- jmvcore::naOmit(mydata)

                testPLevel <- jmvcore::constructFormula(terms = self$options$testPositive)

                testPLevel <- jmvcore::decomposeFormula(formula = testPLevel)

                testPLevel <- unlist(testPLevel)


                testVariable <- jmvcore::constructFormula(terms = self$options$newtest)

                testVariable <- jmvcore::decomposeFormula(formula = testVariable)

                testVariable <- unlist(testVariable)


                goldPLevel <- jmvcore::constructFormula(terms = self$options$goldPositive)

                goldPLevel <- jmvcore::decomposeFormula(formula = goldPLevel)

                goldPLevel <- unlist(goldPLevel)


                goldVariable <- jmvcore::constructFormula(terms = self$options$gold)

                goldVariable <- jmvcore::decomposeFormula(formula = goldVariable)

                goldVariable <- unlist(goldVariable)

                mydata[[testVariable]] <- forcats::as_factor(mydata[[testVariable]])

                mydata[[goldVariable]] <- forcats::as_factor(mydata[[goldVariable]])

                # Table 1 ----

                results1 <- mydata %>% dplyr::select(.data[[testVariable]], .data[[goldVariable]]) %>%
                    table()

                self$results$text1$setContent(results1)


                result2 <- mydata %>%
                    dplyr::group_by_all() %>%
                    dplyr::count() %>%
                    as.data.frame() %>%
                    htmlTable::htmlTable()

                self$results$text2$setContent(result2)


                # Extract and validate variables
                # vars <- list(
                #   test = self$options$newtest,
                #   gold = self$options$gold,
                #   test_pos = self$options$testPositive,
                #   gold_pos = self$options$goldPositive
                # )


                # Prepare data
                # prepared_data <- private$.prepare_data(self$data, vars)
                # self$results$text3$setContent(prepared_data)



                # Calculate basic metrics
                # conf_matrix <- table(prepared_data$test, prepared_data$gold)

                # self$results$text3$setContent(conf_matrix)

                # metrics <- private$.calculate_basic_metrics(
                #   conf_matrix[2,2], # TP
                #   conf_matrix[2,1], # FP
                #   conf_matrix[1,1], # TN
                #   conf_matrix[1,2]  # FN
                # )

                # self$results$text3$setContent(list(conf_matrix, metrics))

                # Populate main results tables
                # private$.populate_main_tables(conf_matrix, metrics)



                # Calculate confidence intervals if requested
                # if (self$options$ci) {
                #   ci <- private$.calculate_confidence_intervals(data, metrics)
                #
                #   self$results$text3$setContent(ci)
                #
                #   # private$.populate_ci_tables(ci)
                #
                #   }

                # Create ROC plot if requested
                # if (self$options$roc) {
                #   roc_plot <- private$.create_roc_plot(
                #     metrics$sensitivity,
                #     metrics$specificity,
                #     ci
                #   )
                #   self$results$plot_roc$setState(roc_plot)
                # }

                # Compare tests if requested
                # if (self$options$compare_tests && !is.null(self$options$additional_test)) {
                #   test_comparison <- private$.compare_tests(
                #     metrics,
                #     private$.calculate_metrics_for_additional_test(),
                #     nrow(data)
                #   )
                #   private$.populate_comparison_tables(test_comparison)
                # }





                # results2 <- as.data.frame(results1)
                #
                # namesfrom <- names(results2)[2]
                #
                # results2 <- results2 %>%
                #         tidyr::pivot_wider(data = .,
                #                        names_from = namesfrom,
                #                        values_from = Freq)
                #
                #
                # self$results$text2$setContent(results2)



                # Original Table -----

                # origTable <- self$results$origTable


                # xnames <- results2[,1]
                #
                #
                # data_frame <- results2
                # for (i in seq_along(data_frame[,1,drop = T])) {
                #     origTable$addRow(rowKey = i, values = c(data_frame[i,]))
                # }


                # Recode ----

                mydata2 <- mydata

                mydata2 <- mydata2 %>% dplyr::mutate(
                    testVariable2 = dplyr::case_when(
                        .data[[testVariable]] ==
                            self$options$testPositive ~ "Positive",
                        NA ~ NA_character_,
                        TRUE ~
                            "Negative"
                    )
                ) %>%
                    dplyr::mutate(
                        goldVariable2 = dplyr::case_when(
                            .data[[goldVariable]] ==
                                self$options$goldPositive ~ "Positive",
                            NA ~ NA_character_,
                            TRUE ~
                                "Negative"
                        )
                    )

                mydata2 <- mydata2 %>% dplyr::mutate(testVariable2 = forcats::fct_relevel(testVariable2, "Positive")) %>% dplyr::mutate(goldVariable2 = forcats::fct_relevel(goldVariable2, "Positive"))




                # conf_table ----

                conf_table <- table(mydata2[["testVariable2"]], mydata2[["goldVariable2"]])


                # Caret ----
                # results_caret <- caret::confusionMatrix(conf_table, positive = "Positive")


                # self$results$text2$setContent(
                #     list(
                #         conf_table,
                #         results_caret
                #         )
                # )

                TP <- conf_table[1, 1]

                FP <- conf_table[1, 2]

                FN <- conf_table[2, 1]

                TN <- conf_table[2, 2]




                # Cross Table in jamovi style ----

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





                # Self Calculations ----

                # Self Calculation https://cran.r-project.org/web/packages/caret/caret.pdf
                # https://online.stat.psu.edu/stat509/node/150/

                # https://en.wikipedia.org/wiki/Sensitivity_and_specificity

                TotalPop <- TP + TN + FP + FN

                DiseaseP <- TP + FN

                DiseaseN <- TN + FP

                TestP <- TP + FP

                TestN <- TN + FN

                TestT <- TP + TN

                TestW <- FP + FN

                Sens <- TP / DiseaseP

                Spec <- TN / DiseaseN

                AccurT <- TestT / TotalPop

                PrevalenceD <- DiseaseP / TotalPop

                PPV <- TP / TestP

                NPV <- TN / TestN


                pp <- self$options$pp
                pprob <- self$options$pprob

                if (pp) {
                    # Known prior probability from population
                    PriorProb <- pprob
                } else {
                    # From ConfusionMatrix
                    PriorProb <- PrevalenceD
                }


                PostTestProbDisease <- (PriorProb * Sens) / ((PriorProb * Sens) + ((1 -
                                                                                        PriorProb) * (1 - Spec)))



                PostTestProbHealthy <- ((1 - PriorProb) * Spec) / (((1 - PriorProb) *
                                                                        Spec) + (PriorProb * (1 - Sens)))




                LRP <- Sens / (1 - Spec)

                LRN <- (1 - Sens) / Spec



                # Cache computed values
                # private$.state <- list(
                #     sens = Sens,
                #     spec = Spec,
                #     ppv = PPV,
                #     npv = NPV,
                #     tp = TP,
                #     fp = FP,
                #     tn = TN,
                #     fn = FN,
                #     n_pos = DiseaseP,
                #     n_neg = DiseaseN,
                #     prevalence = PriorProb
                # )



                # self$results$nTable2$setContent(
                #     list(
                #         tablename = "",
                #         TotalPop = TotalPop,
                #         DiseaseP = DiseaseP,
                #         DiseaseN = DiseaseN,
                #         TestP = TestP,
                #         TestN = TestN,
                #         TestT = TestT,
                #         TestW = TestW
                #     )
                # )



                # nTable Populate Table ----

                nTable <- self$results$nTable
                nTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "",
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
                        tablename = "",
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

                # nTable footnotes ----

                if (self$options$fnote) {
                    # nTable$addFootnote(rowKey = "1", col = "TotalPop", "Total Population")

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


                }


                # ratioTable footnotes ----


                if (self$options$fnote) {
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
                        "Post-test Probability of Having Disease  (Probability of having disease after a positive test using known Population Prevalence)"
                    )

                    ratioTable$addFootnote(
                        rowNo = 1,
                        col = "PostTestProbHealthy",
                        "Post-test Probability of Being Healthy (Probability of being healthy after a negative test using known Population Prevalence)"
                    )

                    # ratioTable$addFootnote(rowNo = 1, col = "LRP", "")

                    # ratioTable$addFootnote(rowNo = 1, col = "LRN", "")


                }




                # Reorganize Table

                # caretresult[['positive']]
                # caretresult[['table']]
                # caretresult[['overall']]
                # caretresult[['overall']][['Accuracy']]
                # caretresult[['overall']][['Kappa']]
                # caretresult[['overall']][['AccuracyLower']]
                # caretresult[['overall']][['AccuracyUpper']]
                # caretresult[['overall']][['AccuracyNull']]
                # caretresult[['overall']][['AccuracyPValue']]
                # caretresult[['overall']][['McnemarPValue']]
                # caretresult[['byClass']]
                # caretresult[['byClass']][['Sensitivity']]
                # caretresult[['byClass']][['Specificity']]
                # caretresult[['byClass']][['Pos Pred Value']]
                # caretresult[['byClass']][['Neg Pred Value']]
                # caretresult[['byClass']][['Precision']]
                # caretresult[['byClass']][['Recall']] caretresult[['byClass']][['F1']]
                # caretresult[['byClass']][['Prevalence']]
                # caretresult[['byClass']][['Detection Rate']]
                # caretresult[['byClass']][['Detection Prevalence']]
                # caretresult[['byClass']][['Balanced Accuracy']] caretresult[['mode']]
                # caretresult[['dots']]

                # Write Summary

                # 95% CI ----

                ci <- self$options$ci

                if (ci) {
                    # epiR ----


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





                    # epirTable_ratio -----

                    epirTable_ratio <- self$results$epirTable_ratio

                    data_frame <- epirresult_ratio
                    for (i in seq_along(data_frame[, 1, drop = T])) {
                        epirTable_ratio$addRow(rowKey = i,
                                               values = c(data_frame[i, ])) # This code produces a named vector/list, which is what the values argument expects
                    }



                    # epirTable_ratio footnotes ----

                    if (self$options$fnote) {

                        epirTable_ratio$addFootnote(
                            rowNo = 5,
                            col = "statsnames",
                            "Proportion of all tests that give a correct result."
                        )
                    }






                    # epirTable_number ----


                    epirTable_number <- self$results$epirTable_number

                    data_frame <- epirresult_number
                    for (i in seq_along(data_frame[, 1, drop = T])) {
                        epirTable_number$addRow(rowKey = i,
                                                values = c(data_frame[i, ]))
                    }



                    # epirTable_number footnotes ----

                    if (self$options$fnote) {


                        epirTable_number$addFootnote(
                            rowNo = 1,
                            col = "statsnames",
                            "How much more likely will the test make a correct diagnosis than an incorrect diagnosis in patients with the disease."
                        )

                        epirTable_number$addFootnote(
                            rowNo = 2,
                            col = "statsnames",
                            "Number of patients that need to be tested to give one correct positive test."
                        )


                        epirTable_number$addFootnote(
                            rowNo = 3,
                            col = "statsnames",
                            "Youden's index is the difference between the true positive rate and the false positive rate. Youden's index ranges from -1 to +1 with values closer to 1 if both sensitivity and specificity are high (i.e. close to 1)."

                        )

                    }


                }





                # Send Data to Plot ----


                plotData1 <- list(
                    "Prevalence" = PriorProb,
                    "Sens" = Sens,
                    "Spec" = Spec,
                    "Plr" = LRP,
                    "Nlr" = LRN
                )


                # self$results$plotcontent$setContent(plotData1)



                image1 <- self$results$plot1
                image1$setState(plotData1)




                # if (self$options$roc) {
                #     plotData2 <- list(
                #         sens = Sens,
                #         spec = Spec,
                #         tp = TP,
                #         fp = FP,
                #         tn = TN,
                #         fn = FN,
                #         n_pos = DiseaseP,
                #         n_neg = DiseaseN,
                #         thresholds = NULL  # Add thresholds if available
                #     )
                #
                #     image2 <- self$results$plot_roc
                #     image2$setState(plotData2)
                # }



            }


            ,
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
                    LabelSize = (14 / 5),
                    Verbose = TRUE
                )

                print(plot1)
                TRUE

            }



            # matrixdetails <- list(results_caret[["positive"]], results_caret[["table"]],
            #     results_caret[["overall"]], results_caret[["overall"]][["Accuracy"]],
            #     results_caret[["overall"]][["Kappa"]], results_caret[["overall"]][["AccuracyLower"]],
            #     results_caret[["overall"]][["AccuracyUpper"]], results_caret[["overall"]][["AccuracyNull"]],
            #     results_caret[["overall"]][["AccuracyPValue"]], results_caret[["overall"]][["McnemarPValue"]],
            #     results_caret[["byClass"]], results_caret[["byClass"]][["Sensitivity"]],
            #     results_caret[["byClass"]][["Specificity"]], results_caret[["byClass"]][["Pos Pred Value"]],
            #     results_caret[["byClass"]][["Neg Pred Value"]], results_caret[["byClass"]][["Precision"]],
            #     results_caret[["byClass"]][["Recall"]], results_caret[["byClass"]][["F1"]],
            #     results_caret[["byClass"]][["Prevalence"]], results_caret[["byClass"]][["Detection Rate"]],
            #     results_caret[["byClass"]][["Detection Prevalence"]], results_caret[["byClass"]][["Balanced Accuracy"]],
            #     results_caret[["mode"]], results_caret[["dots"]])

            # self$results$text3$setContent(matrixdetails)


            # Individual analysis

            # sens <- caret::sensitivity(conf_table, positive = 'Positive')

            # PPV <- caret::posPredValue(conf_table, positive = 'Positive')

            # summary_caret <- glue::glue('Sensitivity is {sens}.  PPV is {PPV}.')

            # self$results$text4$setContent(summary_caret)


            # bdpv ---- https://cran.r-project.org/web/packages/bdpv/bdpv.pdf



            # epiR ---- https://cran.r-project.org/web/packages/epiR/epiR.pdf


            # dat <- as.table( matrix(c(670,202,74,640), nrow = 2, byrow = TRUE) )

            # colnames(dat) <- c('Dis+','Dis-') rownames(dat) <- c('Test+','Test-')

            # rval <- epiR::epi.tests(dat, conf.level = 0.95)

            # rval <- list( dat, rval, print(rval), summary(rval) )

            # self$results$text5$setContent(rval)



            # Prior Probability

            # lvs <- c('normal', 'abnormal') truth <- factor(rep(lvs, times = c(86,
            # 258)), levels = rev(lvs)) pred <- factor( c( rep(lvs, times = c(54,
            # 32)), rep(lvs, times = c(27, 231))), levels = rev(lvs)) xtab <-
            # table(pred, truth) confusionMatrix(xtab) confusionMatrix(pred, truth)
            # confusionMatrix(xtab, prevalence = 0.25) ## 3 class example
            # confusionMatrix(iris$Species, sample(iris$Species)) newPrior <- c(.05,
            # .8, .15) names(newPrior) <- levels(iris$Species)
            # confusionMatrix(iris$Species, sample(iris$Species))






            # ,
            # .plot_roc = function(image2, ggtheme, ...) {
            #
            #     plotData2 <- image2$state
            #
            #     if (is.null(plotData2)) return(FALSE)
            #
            #     # Calculate confidence intervals
            #     auc <- 0.5 * (plotData2$sens * (1-plotData2$spec)) +
            #         0.5 * (1 * (1-(1-plotData2$spec))) +
            #         0.5 * ((1-plotData2$sens) * plotData2$spec)
            #
            #     ci <- auc_ci(auc, plotData2$n_pos, plotData2$n_neg)
            #
            #     # Create ROC curve points
            #     roc_points <- data.frame(
            #         fpr = c(0, 1-plotData2$spec, 1),
            #         tpr = c(0, plotData2$sens, 1)
            #     )
            #
            #     # Create plot with confidence band
            #     p <- ggplot(roc_points, aes(x=fpr, y=tpr)) +
            #         geom_line(color="blue", size=1) +
            #         geom_point(data=data.frame(
            #             fpr=1-plotData2$spec,
            #             tpr=plotData2$sens),
            #             color="red", size=3) +
            #         geom_abline(slope=1, intercept=0,
            #                     linetype="dashed", color="gray") +
            #         annotate("text", x=0.75, y=0.25,
            #                  label=sprintf("AUC = %.3f (%.3f-%.3f)",
            #                                auc, ci[1], ci[2])) +
            #         labs(x = "False Positive Rate (1 - Specificity)",
            #              y = "True Positive Rate (Sensitivity)",
            #              title = "ROC Curve") +
            #         theme_minimal() +
            #         coord_equal() +
            #         theme(plot.title = element_text(hjust = 0.5))
            #
            #     print(p)
            #     TRUE
            # }



            # ,
            # .plot_comparative_roc = function(image, ggtheme, ...) {
            #     test_data <- image$state
            #     if (is.null(test_data) || length(test_data) < 2) return(FALSE)
            #
            #     n_tests <- length(test_data)
            #
            #     # Create base plot
            #     p <- create_base_roc_plot(test_data)
            #
            #     # Add statistical tests if there are multiple tests
            #     if(n_tests >= 2) {
            #         p <- add_statistical_comparison(p, test_data)
            #     }
            #
            #     print(p)
            #     TRUE
            # }



        )
    )
