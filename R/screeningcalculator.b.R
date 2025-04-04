#' @title Screening Test Calculator
#' @importFrom R6 R6Class
#' @import jmvcore

screeningcalculatorClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "screeningcalculatorClass",
        inherit = screeningcalculatorBase,
        private = list(
            .run = function() {
                # Get input values
                sens <- self$options$sens
                spec <- self$options$spec
                prev <- self$options$prev

                # Add explanatory text about sequential testing
                explanatoryHtml <- '
            <div class="jmv-results-item">
                <h3>How Sequential Testing Works</h3>
                <p>This calculator demonstrates how disease probability changes with sequential tests. <strong>Each subsequent test uses the post-test probability from the previous test as the new pre-test probability (prevalence).</strong> This is known as Bayesian updating.</p>
                <p>For example, if a patient has a positive test result, their probability of disease becomes the Positive Predictive Value (PPV). If they then have a second test, this PPV is used as the new "prevalence" when calculating the next post-test probability.</p>
            </div>
            '
                self$results$explanatoryText$setContent(explanatoryHtml)

                # Add math explanations
                mathHtml <- '
            <div class="jmv-results-item">
                <h3>Math Behind the Calculations</h3>
                <p>These calculations use Bayes\' theorem to update probabilities based on test results:</p>
                <p><strong>Positive Predictive Value (PPV)</strong>: Probability of disease after a positive test<br>
                PPV = (Sensitivity × Prevalence) / [(Sensitivity × Prevalence) + (1-Specificity) × (1-Prevalence)]</p>
                <p><strong>Negative Predictive Value (NPV)</strong>: Probability of being disease-free after a negative test<br>
                NPV = (Specificity × (1-Prevalence)) / [(Specificity × (1-Prevalence)) + (1-Sensitivity) × Prevalence]</p>
                <p><strong>Probability of disease after a negative test</strong> = 1 - NPV</p>
                <p><strong>Sequential Testing</strong>: For each subsequent test, the post-test probability from the previous test becomes the new pre-test probability.</p>
                <p><strong>Likelihood Ratios</strong>:<br>
                Positive LR = Sensitivity / (1-Specificity)<br>
                Negative LR = (1-Sensitivity) / Specificity</p>
            </div>
            '
                self$results$mathText$setContent(mathHtml)

                # Calculate single test metrics
                PPV <- (sens * prev) / ((sens * prev) + ((1 - spec) * (1 - prev)))
                NPV <- (spec * (1 - prev)) / ((spec * (1 - prev)) + ((1 - sens) * prev))
                LRP <- sens / (1 - spec)
                LRN <- (1 - sens) / spec

                # Fill single test table
                singleTestTable <- self$results$singleTestTable
                singleTestTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "Values",
                        Sensitivity = sens,
                        Specificity = spec,
                        Prevalence = prev,
                        PPV = PPV,
                        NPV = NPV,
                        LRP = LRP,
                        LRN = LRN
                    )
                )

                # Add footnotes if requested
                if (self$options$fnote) {
                    singleTestTable$addFootnote(
                        rowNo = 1,
                        col = "PPV",
                        "Probability of disease given a positive test result"
                    )
                    singleTestTable$addFootnote(
                        rowNo = 1,
                        col = "NPV",
                        "Probability of being healthy given a negative test result"
                    )
                    singleTestTable$addFootnote(
                        rowNo = 1,
                        col = "LRP",
                        "How much more likely a positive test is in a diseased vs. non-diseased person"
                    )
                    singleTestTable$addFootnote(
                        rowNo = 1,
                        col = "LRN",
                        "How much more likely a negative test is in a diseased vs. non-diseased person"
                    )
                }

                # Setup data for first Fagan nomogram
                if (self$options$fagan) {
                    plotData1 <- list(
                        "Prevalence" = prev,
                        "Sens" = sens,
                        "Spec" = spec,
                        "Plr" = LRP,
                        "Nlr" = LRN
                    )

                    image1 <- self$results$plot1
                    image1$setState(plotData1)
                }

                # Store key probabilities for sequential tests
                test2Results <- list()
                test3Results <- list()

                # Calculate and display two consecutive tests if requested
                if (self$options$repeat2) {
                    repeatTest2Table <- self$results$repeatTest2Table

                    # Scenario 1: Positive-Positive (+/+)
                    # Use PPV from first test as new prevalence
                    ppPrev <- PPV
                    ppPPV <- (sens * ppPrev) / ((sens * ppPrev) + ((1 - spec) * (1 - ppPrev)))
                    ppExplanation <- paste0(
                        "After two consecutive positive tests, the probability of disease increases from the baseline ",
                        round(prev * 100, 1),
                        "% to ",
                        round(ppPPV * 100, 1),
                        "%."
                    )

                    repeatTest2Table$addRow(
                        rowKey = 1,
                        values = list(
                            testResult = "Positive, then Positive (+/+)",
                            probDisease = ppPPV,
                            explanation = ppExplanation
                        )
                    )

                    # Store for nomogram
                    test2Results[["PP"]] <- list(
                        prev = ppPrev,
                        post = ppPPV,
                        plr = LRP,
                        nlr = LRN
                    )

                    # Scenario 2: Positive-Negative (+/-)
                    pnPrev <- PPV
                    pnNPV <- (spec * (1 - pnPrev)) / ((spec * (1 - pnPrev)) + ((1 - sens) * pnPrev))
                    probDiseasePn <- 1 - pnNPV
                    pnExplanation <- paste0(
                        "A positive test followed by a negative test results in a ",
                        round(probDiseasePn * 100, 1),
                        "% probability of disease."
                    )

                    repeatTest2Table$addRow(
                        rowKey = 2,
                        values = list(
                            testResult = "Positive, then Negative (+/-)",
                            probDisease = probDiseasePn,
                            explanation = pnExplanation
                        )
                    )

                    # Store for nomogram
                    test2Results[["PN"]] <- list(
                        prev = pnPrev,
                        post = probDiseasePn,
                        plr = LRP,
                        nlr = LRN
                    )

                    # Scenario 3: Negative-Positive (-/+)
                    npPrev <- 1 - NPV  # Probability of disease after a negative test
                    npPPV <- (sens * npPrev) / ((sens * npPrev) + ((1 - spec) * (1 - npPrev)))
                    npExplanation <- paste0(
                        "A negative test followed by a positive test results in a ",
                        round(npPPV * 100, 1),
                        "% probability of disease."
                    )

                    repeatTest2Table$addRow(
                        rowKey = 3,
                        values = list(
                            testResult = "Negative, then Positive (-/+)",
                            probDisease = npPPV,
                            explanation = npExplanation
                        )
                    )

                    # Store for nomogram
                    test2Results[["NP"]] <- list(
                        prev = npPrev,
                        post = npPPV,
                        plr = LRP,
                        nlr = LRN
                    )

                    # Scenario 4: Negative-Negative (-/-)
                    nnPrev <- 1 - NPV  # Probability of disease after a negative test
                    nnNPV <- (spec * (1 - nnPrev)) / ((spec * (1 - nnPrev)) + ((1 - sens) * nnPrev))
                    probDiseaseNn <- 1 - nnNPV
                    nnExplanation <- paste0(
                        "After two consecutive negative tests, the probability of disease drops from the baseline ",
                        round(prev * 100, 1),
                        "% to ",
                        round(probDiseaseNn * 100, 1),
                        "%."
                    )

                    repeatTest2Table$addRow(
                        rowKey = 4,
                        values = list(
                            testResult = "Negative, then Negative (-/-)",
                            probDisease = probDiseaseNn,
                            explanation = nnExplanation
                        )
                    )

                    # Store for nomogram
                    test2Results[["NN"]] <- list(
                        prev = nnPrev,
                        post = probDiseaseNn,
                        plr = LRP,
                        nlr = LRN
                    )

                    # Setup data for second test Fagan nomograms
                    if (self$options$fagan) {
                        # For +/+ pathway
                        plotData2PP <- list(
                            "Prevalence" = ppPrev,
                            # PPV from first test
                            "Sens" = sens,
                            "Spec" = spec,
                            "Plr" = LRP,
                            "Nlr" = LRN,
                            "Title" = "After Initial Positive Test (+/+)"
                        )

                        # For -/- pathway
                        plotData2NN <- list(
                            "Prevalence" = nnPrev,
                            # 1-NPV from first test
                            "Sens" = sens,
                            "Spec" = spec,
                            "Plr" = LRP,
                            "Nlr" = LRN,
                            "Title" = "After Initial Negative Test (-/-)"
                        )

                        image2PP <- self$results$plot2PP
                        image2PP$setState(plotData2PP)

                        image2NN <- self$results$plot2NN
                        image2NN$setState(plotData2NN)
                    }
                }

                # Calculate and display three consecutive tests if requested
                if (self$options$repeat3) {
                    repeatTest3Table <- self$results$repeatTest3Table

                    # Scenario 1: Positive-Positive-Positive (+/+/+)
                    # Starting from the result of +/+
                    pppPrev <- ppPPV  # Probability after two positive tests
                    pppPPV <- (sens * pppPrev) / ((sens * pppPrev) + ((1 - spec) * (1 - pppPrev)))
                    pppExplanation <- paste0(
                        "After three consecutive positive tests, the probability of disease is ",
                        round(pppPPV * 100, 1),
                        "% (increased from baseline ",
                        round(prev * 100, 1),
                        "%)."
                    )

                    repeatTest3Table$addRow(
                        rowKey = 1,
                        values = list(
                            testResult = "Positive, Positive, Positive (+/+/+)",
                            probDisease = pppPPV,
                            explanation = pppExplanation
                        )
                    )

                    # Store for nomogram
                    test3Results[["PPP"]] <- list(
                        prev = pppPrev,
                        post = pppPPV,
                        plr = LRP,
                        nlr = LRN
                    )

                    # Scenario 2: Positive-Positive-Negative (+/+/-)
                    ppnPrev <- ppPPV  # Probability after two positive tests
                    ppnNPV <- (spec * (1 - ppnPrev)) / ((spec * (1 - ppnPrev)) + ((1 - sens) * ppnPrev))
                    probDiseasePpn <- 1 - ppnNPV
                    ppnExplanation <- paste0(
                        "After two positive tests followed by a negative test, the probability of disease is ",
                        round(probDiseasePpn * 100, 1),
                        "%."
                    )

                    repeatTest3Table$addRow(
                        rowKey = 2,
                        values = list(
                            testResult = "Positive, Positive, Negative (+/+/-)",
                            probDisease = probDiseasePpn,
                            explanation = ppnExplanation
                        )
                    )

                    # Scenario 3: Positive-Negative-Positive (+/-/+)
                    pnpPrev <- probDiseasePn  # From +/-
                    pnpPPV <- (sens * pnpPrev) / ((sens * pnpPrev) + ((1 - spec) * (1 - pnpPrev)))
                    pnpExplanation <- paste0(
                        "After positive, negative, then positive tests, the probability of disease is ",
                        round(pnpPPV * 100, 1),
                        "%."
                    )

                    repeatTest3Table$addRow(
                        rowKey = 3,
                        values = list(
                            testResult = "Positive, Negative, Positive (+/-/+)",
                            probDisease = pnpPPV,
                            explanation = pnpExplanation
                        )
                    )

                    # Scenario 4: Positive-Negative-Negative (+/-/-)
                    pnnPrev <- probDiseasePn  # From +/-
                    pnnNPV <- (spec * (1 - pnnPrev)) / ((spec * (1 - pnnPrev)) + ((1 - sens) * pnnPrev))
                    probDiseasePnn <- 1 - pnnNPV
                    pnnExplanation <- paste0(
                        "After positive, negative, then negative tests, the probability of disease is ",
                        round(probDiseasePnn * 100, 1),
                        "%."
                    )

                    repeatTest3Table$addRow(
                        rowKey = 4,
                        values = list(
                            testResult = "Positive, Negative, Negative (+/-/-)",
                            probDisease = probDiseasePnn,
                            explanation = pnnExplanation
                        )
                    )

                    # Scenario 5: Negative-Positive-Positive (-/+/+)
                    nppPrev <- npPPV  # From -/+
                    nppPPV <- (sens * nppPrev) / ((sens * nppPrev) + ((1 - spec) * (1 - nppPrev)))
                    nppExplanation <- paste0(
                        "After negative, positive, then positive tests, the probability of disease is ",
                        round(nppPPV * 100, 1),
                        "%."
                    )

                    repeatTest3Table$addRow(
                        rowKey = 5,
                        values = list(
                            testResult = "Negative, Positive, Positive (-/+/+)",
                            probDisease = nppPPV,
                            explanation = nppExplanation
                        )
                    )

                    # Scenario 6: Negative-Positive-Negative (-/+/-)
                    npnPrev <- npPPV  # From -/+
                    npnNPV <- (spec * (1 - npnPrev)) / ((spec * (1 - npnPrev)) + ((1 - sens) * npnPrev))
                    probDiseaseNpn <- 1 - npnNPV
                    npnExplanation <- paste0(
                        "After negative, positive, then negative tests, the probability of disease is ",
                        round(probDiseaseNpn * 100, 1),
                        "%."
                    )

                    repeatTest3Table$addRow(
                        rowKey = 6,
                        values = list(
                            testResult = "Negative, Positive, Negative (-/+/-)",
                            probDisease = probDiseaseNpn,
                            explanation = npnExplanation
                        )
                    )

                    # Scenario 7: Negative-Negative-Positive (-/-/+)
                    nnpPrev <- probDiseaseNn  # From -/-
                    nnpPPV <- (sens * nnpPrev) / ((sens * nnpPrev) + ((1 - spec) * (1 - nnpPrev)))
                    nnpExplanation <- paste0(
                        "After two negative tests followed by a positive test, the probability of disease is ",
                        round(nnpPPV * 100, 1),
                        "%."
                    )

                    repeatTest3Table$addRow(
                        rowKey = 7,
                        values = list(
                            testResult = "Negative, Negative, Positive (-/-/+)",
                            probDisease = nnpPPV,
                            explanation = nnpExplanation
                        )
                    )

                    # Store for nomogram
                    test3Results[["NNP"]] <- list(
                        prev = nnpPrev,
                        post = nnpPPV,
                        plr = LRP,
                        nlr = LRN
                    )

                    # Scenario 8: Negative-Negative-Negative (-/-/-)
                    nnnPrev <- probDiseaseNn  # From -/-
                    nnnNPV <- (spec * (1 - nnnPrev)) / ((spec * (1 - nnnPrev)) + ((1 - sens) * nnnPrev))
                    probDiseaseNnn <- 1 - nnnNPV
                    nnnExplanation <- paste0(
                        "After three consecutive negative tests, the probability of disease is ",
                        round(probDiseaseNnn * 100, 1),
                        "% (decreased from baseline ",
                        round(prev * 100, 1),
                        "%)."
                    )

                    repeatTest3Table$addRow(
                        rowKey = 8,
                        values = list(
                            testResult = "Negative, Negative, Negative (-/-/-)",
                            probDisease = probDiseaseNnn,
                            explanation = nnnExplanation
                        )
                    )

                    # Store for nomogram
                    test3Results[["NNN"]] <- list(
                        prev = nnnPrev,
                        post = probDiseaseNnn,
                        plr = LRP,
                        nlr = LRN
                    )

                    # Setup data for third test Fagan nomograms
                    if (self$options$fagan) {
                        # For +/+/+ pathway (after +/+)
                        plotData3PPP <- list(
                            "Prevalence" = pppPrev,
                            # Result from two positive tests
                            "Sens" = sens,
                            "Spec" = spec,
                            "Plr" = LRP,
                            "Nlr" = LRN,
                            "Title" = "After Two Positive Tests (+/+/+)"
                        )

                        # For -/-/- pathway (after -/-)
                        plotData3NNN <- list(
                            "Prevalence" = nnnPrev,
                            # Result from two negative tests
                            "Sens" = sens,
                            "Spec" = spec,
                            "Plr" = LRP,
                            "Nlr" = LRN,
                            "Title" = "After Two Negative Tests (-/-/-)"
                        )

                        image3PPP <- self$results$plot3PPP
                        image3PPP$setState(plotData3PPP)

                        image3NNN <- self$results$plot3NNN
                        image3NNN$setState(plotData3NNN)
                    }
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

                plot1 <- plot1 + ggplot2::ggtitle("Fagan Nomogram - Single Test")

                print(plot1)
                TRUE
            },

            .plot2PP = function(image2PP, ggtheme, ...) {
                plotData2 <- image2PP$state

                plot2PP <- nomogrammer(
                    Prevalence = plotData2$Prevalence,
                    Sens = plotData2$Sens,
                    Spec = plotData2$Spec,
                    Plr = plotData2$Plr,
                    Nlr = plotData2$Nlr,
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = (14 /
                                     5),
                    Verbose = TRUE
                )

                plot2PP <- plot2PP + ggplot2::ggtitle(paste0("Fagan Nomogram - Second Test ", plotData2$Title))

                print(plot2PP)
                TRUE
            },

            .plot2NN = function(image2NN, ggtheme, ...) {
                plotData2 <- image2NN$state

                plot2NN <- nomogrammer(
                    Prevalence = plotData2$Prevalence,
                    Sens = plotData2$Sens,
                    Spec = plotData2$Spec,
                    Plr = plotData2$Plr,
                    Nlr = plotData2$Nlr,
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = (14 /
                                     5),
                    Verbose = TRUE
                )

                plot2NN <- plot2NN + ggplot2::ggtitle(paste0("Fagan Nomogram - Second Test ", plotData2$Title))

                print(plot2NN)
                TRUE
            },

            .plot3PPP = function(image3PPP, ggtheme, ...) {
                plotData3 <- image3PPP$state

                plot3PPP <- nomogrammer(
                    Prevalence = plotData3$Prevalence,
                    Sens = plotData3$Sens,
                    Spec = plotData3$Spec,
                    Plr = plotData3$Plr,
                    Nlr = plotData3$Nlr,
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = (14 /
                                     5),
                    Verbose = TRUE
                )

                plot3PPP <- plot3PPP + ggplot2::ggtitle(paste0("Fagan Nomogram - Third Test ", plotData3$Title))

                print(plot3PPP)
                TRUE
            },

            .plot3NNN = function(image3NNN, ggtheme, ...) {
                plotData3 <- image3NNN$state

                plot3NNN <- nomogrammer(
                    Prevalence = plotData3$Prevalence,
                    Sens = plotData3$Sens,
                    Spec = plotData3$Spec,
                    Plr = plotData3$Plr,
                    Nlr = plotData3$Nlr,
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = (14 /
                                     5),
                    Verbose = TRUE
                )

                plot3NNN <- plot3NNN + ggplot2::ggtitle(paste0("Fagan Nomogram - Third Test ", plotData3$Title))

                print(plot3NNN)
                TRUE
            }
        )
    )
