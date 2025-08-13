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
                
                # Input validation and error handling
                errors <- c()
                warnings <- c()
                
                # Validate probability ranges
                if (is.na(sens) || sens < 0 || sens > 1) {
                    errors <- c(errors, "Sensitivity must be between 0 and 1 (0% to 100%)")
                }
                if (is.na(spec) || spec < 0 || spec > 1) {
                    errors <- c(errors, "Specificity must be between 0 and 1 (0% to 100%)")
                }
                if (is.na(prev) || prev < 0 || prev > 1) {
                    errors <- c(errors, "Prevalence must be between 0 and 1 (0% to 100%)")
                }
                
                # Clinical plausibility warnings
                if (sens < 0.50) {
                    warnings <- c(warnings, "Sensitivity <50% is unusual for most clinical tests")
                }
                if (spec < 0.50) {
                    warnings <- c(warnings, "Specificity <50% is unusual for most clinical tests")
                }
                if (sens > 0.99) {
                    warnings <- c(warnings, "Sensitivity >99% is rarely achieved in practice")
                }
                if (spec > 0.99) {
                    warnings <- c(warnings, "Specificity >99% is rarely achieved in practice")
                }
                if (prev > 0.90) {
                    warnings <- c(warnings, "Prevalence >90% is unusual except for confirmatory testing")
                }
                if (prev < 0.001) {
                    warnings <- c(warnings, "Very low prevalence (<0.1%) may lead to extremely low PPV")
                }
                
                # Test characteristic combinations that may be problematic
                if ((sens + spec) < 1.0) {
                    warnings <- c(warnings, "Combined sensitivity + specificity <100% suggests poor test performance")
                }
                
                # Show errors if any exist
                if (length(errors) > 0) {
                    errorHtml <- paste0(
                        '<div class="alert alert-danger"><h4>Input Errors:</h4><ul>',
                        paste0('<li>', errors, '</li>', collapse = ''),
                        '</ul><p>Please correct these values to proceed with calculations.</p></div>'
                    )
                    self$results$explanatoryText$setContent(errorHtml)
                    return()
                }
                
                # Show warnings if any exist (but continue with calculation)
                if (length(warnings) > 0) {
                    warningHtml <- paste0(
                        '<div class="alert alert-warning"><h4>Clinical Notes:</h4><ul>',
                        paste0('<li>', warnings, '</li>', collapse = ''),
                        '</ul><p>Calculations will proceed, but please verify these parameters are appropriate for your clinical scenario.</p></div>'
                    )
                    # We'll add this to the explanatory text later
                }

                # Add explanatory text about sequential testing with clinical examples
                explanatoryHtml <- '
            <div class="jmv-results-item">
                <h3>How Sequential Testing Works</h3>
                <p>This calculator demonstrates how disease probability changes with sequential tests. <strong>Each subsequent test uses the post-test probability from the previous test as the new pre-test probability (prevalence).</strong> This is known as Bayesian updating.</p>
                
                <h4>Clinical Examples:</h4>
                <p><strong>COVID-19 Testing:</strong> A rapid antigen test positive in community screening (2% prevalence) might have 26% PPV. If confirmed with RT-PCR, the probability increases to >95%.</p>
                
                <p><strong>Cancer Screening:</strong> Mammography positive in 50-year-old women (0.8% prevalence) has ~7% PPV. Tissue biopsy after positive mammogram increases probability to >90%.</p>
                
                <p><strong>Cardiac Testing:</strong> Abnormal stress test in chest pain patients (25% prevalence) has ~70% PPV. Cardiac catheterization after positive stress test approaches 95% certainty.</p>
                
                <p><strong>HIV Screening:</strong> Positive ELISA in general population (0.1% prevalence) has ~9% PPV. Western blot confirmation increases probability to >99%.</p>
                
                <p><strong>Key Principle:</strong> Sequential testing dramatically improves diagnostic accuracy by updating disease probability with each test result, following Bayes theorem.</p>
            </div>
            '
                
                # Add warnings to explanatory text if they exist
                if (length(warnings) > 0) {
                    explanatoryHtml <- paste0(warningHtml, explanatoryHtml)
                }
                
                self$results$explanatoryText$setContent(explanatoryHtml)

                # Add math explanations with clinical interpretation guidance
                mathHtml <- '
            <div class="jmv-results-item">
                <h3>Math Behind the Calculations</h3>
                <p>These calculations use Bayes\' theorem to update probabilities based on test results:</p>
                
                <p><strong>Positive Predictive Value (PPV)</strong>: Probability of disease after a positive test<br>
                PPV = (Sensitivity × Prevalence) / [(Sensitivity × Prevalence) + (1-Specificity) × (1-Prevalence)]<br>
                <em>Clinical use: Determines confidence in positive results. PPV <10% suggests confirmatory testing needed.</em></p>
                
                <p><strong>Negative Predictive Value (NPV)</strong>: Probability of being disease-free after a negative test<br>
                NPV = (Specificity × (1-Prevalence)) / [(Specificity × (1-Prevalence)) + (1-Sensitivity) × Prevalence]<br>
                <em>Clinical use: Determines confidence in negative results. NPV >95% suggests disease can be ruled out.</em></p>
                
                <p><strong>Likelihood Ratios (LR)</strong>:<br>
                Positive LR = Sensitivity / (1-Specificity) &nbsp;&nbsp;&nbsp; Negative LR = (1-Sensitivity) / Specificity<br>
                <em>Clinical interpretation: LR+ >10 = strong evidence for disease; LR- <0.1 = strong evidence against disease</em></p>
                
                <p><strong>Sequential Testing Formula</strong>: For each subsequent test, the post-test probability from the previous test becomes the new pre-test probability.<br>
                <em>Clinical principle: Each test result updates disease probability, creating a chain of evidence.</em></p>
                
                <h4>Prevalence Effects:</h4>
                <p><em>Low prevalence (screening): Even excellent tests have low PPV due to many false positives</em><br>
                <em>High prevalence (symptomatic): Same tests have high PPV due to enriched disease population</em></p>
                
                <h4>Example Datasets Available:</h4>
                <p><em>Use data(screening_examples) for 15 realistic clinical scenarios across medical specialties</em><br>
                <em>Use data(prevalence_demo) to see how prevalence affects test performance</em><br>
                <em>Use data(common_tests) for reference characteristics of standard medical tests</em></p>
            </div>
            '
                self$results$mathText$setContent(mathHtml)

                # Calculate single test metrics with error handling
                tryCatch({
                    # Check for division by zero conditions
                    if (spec == 1) {
                        # Perfect specificity: positive LR is infinite
                        LRP <- Inf
                    } else {
                        LRP <- sens / (1 - spec)
                    }
                    
                    if (spec == 0) {
                        # Zero specificity: negative LR is infinite
                        LRN <- Inf
                    } else {
                        LRN <- (1 - sens) / spec
                    }
                    
                    # Calculate predictive values with protection against edge cases
                    denom_ppv <- (sens * prev) + ((1 - spec) * (1 - prev))
                    if (denom_ppv == 0) {
                        PPV <- NaN  # Undefined case
                    } else {
                        PPV <- (sens * prev) / denom_ppv
                    }
                    
                    denom_npv <- (spec * (1 - prev)) + ((1 - sens) * prev)
                    if (denom_npv == 0) {
                        NPV <- NaN  # Undefined case
                    } else {
                        NPV <- (spec * (1 - prev)) / denom_npv
                    }
                    
                    # Ensure results are in valid probability ranges
                    PPV <- pmax(0, pmin(1, PPV))
                    NPV <- pmax(0, pmin(1, NPV))
                    
                }, error = function(e) {
                    # If any calculation fails, set to NA and show error
                    PPV <<- NA
                    NPV <<- NA
                    LRP <<- NA
                    LRN <<- NA
                    
                    errorMsg <- paste0(
                        '<div class="alert alert-danger"><h4>Calculation Error:</h4>',
                        '<p>Unable to calculate test metrics with the provided values. Error: ', e$message, '</p>',
                        '<p>Please check your input parameters and try again.</p></div>'
                    )
                    self$results$explanatoryText$setContent(errorMsg)
                    return()
                })

                # Prepare values for table with handling of special cases
                display_PPV <- if (is.infinite(PPV) || is.nan(PPV)) NA else PPV
                display_NPV <- if (is.infinite(NPV) || is.nan(NPV)) NA else NPV
                display_LRP <- if (is.infinite(LRP)) "∞" else if (is.nan(LRP)) NA else LRP
                display_LRN <- if (is.infinite(LRN)) "∞" else if (is.nan(LRN)) NA else LRN
                
                # Fill single test table
                singleTestTable <- self$results$singleTestTable
                singleTestTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "Values",
                        Sensitivity = sens,
                        Specificity = spec,
                        Prevalence = prev,
                        PPV = display_PPV,
                        NPV = display_NPV,
                        LRP = if (is.character(display_LRP)) NA else display_LRP,  # Handle ∞ symbol
                        LRN = if (is.character(display_LRN)) NA else display_LRN   # Handle ∞ symbol
                    )
                )
                
                # Add special footnotes for infinite or undefined values
                if (is.infinite(LRP)) {
                    singleTestTable$addFootnote(
                        rowNo = 1,
                        col = "LRP", 
                        "Infinite (perfect specificity)"
                    )
                }
                if (is.infinite(LRN)) {
                    singleTestTable$addFootnote(
                        rowNo = 1,
                        col = "LRN",
                        "Infinite (zero specificity)"
                    )
                }
                if (is.nan(PPV) || is.nan(NPV)) {
                    singleTestTable$addFootnote(
                        rowNo = 1,
                        col = "PPV",
                        "Undefined due to extreme parameter values"
                    )
                }

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
                
                # Sample size estimation (DiagROC inspired functionality)
                if (self$options$samplesize) {
                    sampleSizeTable <- self$results$sampleSizeTable
                    
                    expected_sens <- self$options$expected_sens
                    expected_spec <- self$options$expected_spec
                    width_sens <- self$options$width_sens
                    width_spec <- self$options$width_spec
                    alpha <- self$options$alpha_level
                    
                    # Calculate z-score for confidence level
                    z_score <- stats::qnorm(1 - alpha/2)
                    
                    # Sample size for sensitivity estimation (Clopper-Pearson)
                    # n = [z^2 * p * (1-p)] / (width/2)^2
                    # where p is expected sensitivity
                    n_sens_diseased <- ceiling((z_score^2 * expected_sens * (1 - expected_sens)) / (width_sens/2)^2)
                    
                    # Sample size for specificity estimation
                    n_spec_healthy <- ceiling((z_score^2 * expected_spec * (1 - expected_spec)) / (width_spec/2)^2)
                    
                    # Total sample size assuming 50% prevalence for balanced design
                    n_total_balanced <- n_sens_diseased + n_spec_healthy
                    
                    # Sample size for different prevalence scenarios
                    prev_scenarios <- c(0.05, 0.10, 0.20, 0.50)
                    
                    sampleSizeTable$addRow(
                        rowKey = 1,
                        values = list(
                            parameter = "Sensitivity Precision",
                            value = n_sens_diseased,
                            assumptions = paste0("Diseased subjects needed (Expected Sens = ", 
                                                round(expected_sens*100, 1), "%, CI width = ", 
                                                round(width_sens*100, 1), "%)")
                        )
                    )
                    
                    sampleSizeTable$addRow(
                        rowKey = 2, 
                        values = list(
                            parameter = "Specificity Precision",
                            value = n_spec_healthy,
                            assumptions = paste0("Healthy subjects needed (Expected Spec = ",
                                                round(expected_spec*100, 1), "%, CI width = ",
                                                round(width_spec*100, 1), "%)")
                        )
                    )
                    
                    # Add prevalence-specific total sample sizes
                    for (i in seq_along(prev_scenarios)) {
                        prev <- prev_scenarios[i]
                        n_total_prev <- ceiling(n_sens_diseased / prev) + ceiling(n_spec_healthy / (1 - prev))
                        
                        sampleSizeTable$addRow(
                            rowKey = 2 + i,
                            values = list(
                                parameter = paste0("Total Sample (Prev = ", round(prev*100, 1), "%)"),
                                value = n_total_prev,
                                assumptions = paste0("Assuming disease prevalence of ", round(prev*100, 1), "% in study population")
                            )
                        )
                    }
                    
                    # Add balanced design recommendation
                    sampleSizeTable$addRow(
                        rowKey = 7,
                        values = list(
                            parameter = "Balanced Design",
                            value = n_total_balanced,
                            assumptions = "Equal numbers of diseased and healthy subjects (50% prevalence)"
                        )
                    )
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
