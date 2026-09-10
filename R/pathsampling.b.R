# This file is a generated template, your changes will not be overwritten

pathsamplingClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "pathsamplingClass",
        inherit = pathsamplingBase,
        public = list(
            initialize = function(options, data = NULL, datasetId = "", analysisId = "", revision = 0) {
                # jmvcore cannot select zero columns from a nonempty provided frame.
                if (is.data.frame(data) && length(options$varsRequired) == 0)
                    data <- data.frame()
                super$initialize(options = options, data = data, datasetId = datasetId,
                    analysisId = analysisId, revision = revision)
            }
        ),
        private = list(
            # HTML Styling Constants (decisionpanel style)
            .styleConstants = list(
                font = "font-family: Arial, sans-serif;",
                lineHeight = "line-height: 1.4;",
                colorPrimary = "color: inherit;",
                colorSecondary = "color: inherit;",
                bgLight = "background: rgba(88, 88, 88, 0.06); color: inherit;",
                bgLighter = "background: rgba(155, 155, 155, 0.06); color: inherit;",
                bgWhite = "background: rgba(255, 255, 255, 0.10); color: inherit;",
                borderPrimary = "border: 2px solid #333;",
                borderSecondary = "border: 1px solid #ccc;",
                borderLeft = "border-left: 4px solid #333;",
                borderWarning = "border-left: 4px solid #ffc107;",
                fontSize13 = "font-size: 13px;",
                fontSize14 = "font-size: 14px;",
                fontSize15 = "font-size: 15px;",
                fontSize16 = "font-size: 16px;",
                fontSize18 = "font-size: 18px;",
                fontWeight700 = "font-weight: 700;",
                padding10 = "padding: 10px;",
                padding15 = "padding: 15px;",
                padding20 = "padding: 20px;",
                margin10 = "margin: 10px 0;",
                margin15 = "margin: 15px 0;",
                margin20 = "margin: 20px 0;",
                colorSuccess = "color: inherit;",
                colorInfo = "color: inherit;"
            ),
            .empiricalHeterogeneity = NULL,
            .totalSamplesData = NULL,
            .firstDetectionData = NULL,
            .pEstimate = NULL,
            .maxSamp = NULL,
            .bootstrapResults = NULL,
            .positiveCassettesData = NULL,
            .maxPositiveSingleData = NULL,

            .fixedTables = c("binomialTable", "recommendTable", "bootstrapTable",
                "obsPredTable", "multifocalProbTable", "stageMigrationTable",
                "distributionPatternTable", "empiricalCumulativeTable", "populationDetectionTable",
                "hypergeometricTable", "hyperRecommendTable", "betaBinomialTable",
                "betaBinomialRecommendTable", "lnrClassification", "ajccNStage", "adequacyByELN"),
            .initTableRows = function() {
                # Establish option-determined table shapes before slow estimation/resampling.
                maxN <- self$options$maxSamples
                nkeys <- paste0("n_", seq_len(maxN))
                confkeys <- paste0("conf_", c(0.8, 0.9, 0.95, 0.99))
                data <- self$data
                present <- function(vars) all(vapply(vars,
                    function(x) !is.null(self$options[[x]]), logical(1)))
                core <- present(c("totalSamples", "firstDetection")) && nrow(data) > 0
                if (core) {
                    t <- private$.numericColumn(data, self$options$totalSamples)
                    f <- private$.numericColumn(data, self$options$firstDetection)
                    core <- any(private$.validCount(t, TRUE) & private$.validCount(f, TRUE) & f <= t)
                }
                add <- function(name, keys, enabled) {
                    if (!enabled) return()
                    table <- self$results$get(name)
                    for (key in keys) table$addRow(rowKey = key, values = list())
                }
                # The binomial estimate can be required by a dependent output with its panel off.
                add("binomialTable", nkeys, core)
                add("recommendTable", confkeys, core)
                add("bootstrapTable", as.list(seq_len(maxN)), core && self$options$showBootstrap)
                add("obsPredTable", paste0("op_", seq_len(maxN)), core && self$options$showObsPred)
                add("multifocalProbTable", as.list(seq_len(maxN)), core && self$options$showMultifocalAnalysis)
                add("stageMigrationTable", paste0("group_", 1:3), core && self$options$showStageMigration && present("positiveCassettes"))
                add("distributionPatternTable", c("predominant_single", "summed_effect", "diffuse"), core && self$options$showDistributionPattern && present(c("totalFoci", "maxPositiveSingle")))
                add("empiricalCumulativeTable", nkeys, core && self$options$showEmpiricalCumulative)
                add("populationDetectionTable", nkeys, core && self$options$showPopulationDetection)
                finite <- present(c("totalPopulation", "successStates")) && nrow(data) > 0
                if (finite) {
                    n <- private$.numericColumn(data, self$options$totalPopulation)
                    k <- private$.numericColumn(data, self$options$successStates)
                    finite <- any(private$.validCount(n, TRUE) & private$.validCount(k) & k <= n)
                }
                add("hypergeometricTable", nkeys, finite && self$options$showHypergeometric)
                add("hyperRecommendTable", confkeys, finite && self$options$showHypergeometric)
                add("betaBinomialTable", nkeys, finite && self$options$showBetaBinomial)
                add("betaBinomialRecommendTable", confkeys, finite && self$options$showBetaBinomial)
                nodes <- present(c("totalLymphNodes", "positiveLymphNodes")) && self$options$showLNAnalysis &&
                    self$options$lnrThreshold1 < self$options$lnrThreshold2 && nrow(data) > 0
                if (nodes) {
                    n <- private$.numericColumn(data, self$options$totalLymphNodes)
                    k <- private$.numericColumn(data, self$options$positiveLymphNodes)
                    nodes <- any(private$.validCount(n, TRUE) & private$.validCount(k) & k <= n)
                }
                add("lnrClassification", paste0("lnr_", 1:4), nodes)
                add("ajccNStage", paste0("stage_", 1:3), nodes)
                add("adequacyByELN", paste0("eln_", 1:5), nodes)
            },
            .deleteRows = function(table) {
                table$deleteRows()
                # Current jmvcore clears cells/keys but can retain display row names.
                # An empty table then fails as.data.frame() after a previous populated run.
                if (table$rowCount == 0 && length(table$names) > 0)
                    table$.__enclos_env__$private$.rowNames <- character()
            },
            .clearTable = function(table) {
                if (!table$name %in% private$.fixedTables) private$.deleteRows(table)
            },
            .putRow = function(table, rowKey, values) {
                # Dynamic/result-dependent rows are created here; fixed rows already exist.
                if (!any(vapply(table$rowKeys, identical, logical(1), rowKey)))
                    table$addRow(rowKey = rowKey, values = values)
                else table$setRow(rowKey = rowKey, values = values)
            },
            .estimateQ = function(first, total, positive = NULL) {
                method <- self$options$estimationMethod
                empirical <- method == "empirical" || (method == "auto" &&
                    !is.null(positive) && self$options$analysisContext %in%
                        c("general", "lymphnode", "omentum"))
                result <- list(q = NA_real_, method = if (empirical) "empirical" else "geometric",
                    cv = NA_real_, rejected = FALSE, n = 0L)
                detected <- !is.na(first)
                if (empirical) {
                    if (is.null(positive)) return(result)
                    keep <- detected & private$.validCount(total, TRUE) &
                        private$.validCount(positive, TRUE) & positive <= total
                    result$n <- sum(keep)
                    if (!result$n) return(result)
                    result$q <- sum(positive[keep]) / sum(total[keep])
                    proportions <- positive[keep] / total[keep]
                    if (length(proportions) >= 3L) {
                        result$cv <- stats::sd(proportions) / mean(proportions)
                        result$rejected <- is.finite(result$cv) && result$cv > 0.5
                    }
                    if (result$rejected) result$q <- NA_real_
                } else {
                    result$n <- sum(detected)
                    if (result$n) result$q <- 1 / mean(first[detected])
                }
                result
            },
            .validCount = function(x, positive = FALSE) {
                is.finite(x) & x >= ifelse(positive, 1, 0) &
                    x <= .Machine$integer.max & x == floor(x)
            },
            .numericColumn = function(data, variable) {
                x <- data[[variable]]
                if (is.factor(x)) x <- as.character(x)
                suppressWarnings(as.numeric(x))
            },
            .groupColumn = function(x) {
                if (inherits(x, "haven_labelled") || !is.null(attr(x, "labels"))) {
                    labels <- attr(x, "labels")
                    values <- as.character(unclass(x))
                    matched <- match(as.numeric(unclass(x)), unname(labels))
                    values[!is.na(matched)] <- names(labels)[matched[!is.na(matched)]]
                    return(factor(values, levels = unique(c(names(labels), values[!is.na(values)]))))
                }
                if (is.factor(x)) droplevels(x) else factor(x, levels = unique(x[!is.na(x)]))
            },
            .sampleCounts = function(data, variable, total) {
                x <- private$.numericColumn(data, variable)
                valid <- private$.validCount(x) & x <= total
                if (any(!valid)) private$.addNotice("WARNING", jmvcore::.("Invalid positive sample counts"),
                    jmvcore::format(jmvcore::.("{v1} cases excluded from positive-sample summaries because counts are missing, invalid or greater than total samples."), v1 = sprintf("%d", sum(!valid))))
                x[!valid] <- NA_real_
                x
            },
            .resetResults = function() {
                private$.bootstrapCache <- NULL
                private$.noticeList <- list()
                private$.empiricalHeterogeneity <- NULL
                private$.positiveCassettesData <- NULL
                private$.bootstrapResults <- NULL
                private$.pEstimate <- NULL
                for (nm in self$results$itemNames) {
                    item <- self$results$get(nm)
                    if (inherits(item, "Table")) private$.deleteRows(item)
                    else if (inherits(item, "Image")) item$setState(NULL)
                    else if (!nm %in% c("welcome", "guidedInstructions", "conciseInstructions"))
                        item$setContent("")
                }
                private$.initTableRows()
            },
            .checkOptionalInputs = function() {
                requirements <- list(
                    showTumorBurden = c("positiveCassettes"),
                    showStageMigration = c("positiveCassettes"),
                    showCorrelation = c("positiveCassettes"),
                    showDistributionPattern = c("totalFoci", "maxPositiveSingle"),
                    showStratifiedAnalysis = c("sampleType"),
                    showSpatialClustering = c("positiveSamplesList"),
                    showHypergeometric = c("totalPopulation", "successStates"),
                    showBetaBinomial = c("totalPopulation", "successStates"),
                    showLNAnalysis = c("totalLymphNodes", "positiveLymphNodes"),
                    showEffectSizes = c("totalLymphNodes"))
                labels <- c(positiveCassettes = jmvcore::.("Positive samples"), totalFoci = jmvcore::.("Total foci"),
                    maxPositiveSingle = jmvcore::.("Maximum foci per slide"), sampleType = jmvcore::.("Sample type"),
                    positiveSamplesList = jmvcore::.("Positive sample positions"), totalPopulation = jmvcore::.("Total population"),
                    successStates = jmvcore::.("Success states"), totalLymphNodes = jmvcore::.("Total lymph nodes"),
                    positiveLymphNodes = jmvcore::.("Positive lymph nodes"))
                for (option in names(requirements)) {
                    if (!self$options[[option]]) next
                    missing <- requirements[[option]][vapply(requirements[[option]],
                        function(x) is.null(self$options[[x]]), logical(1))]
                    if (length(missing)) private$.addNotice("INFO", jmvcore::.("Additional inputs needed"),
                        jmvcore::format(jmvcore::.("Select {inputs} to calculate the requested optional analysis."), inputs = paste(labels[missing], collapse = ", ")))
                }
            },
            .planSamples = function(estimate = NA_real_) {
                if (!self$options$showSampleSizePlanning) return()
                target <- self$options$planningTargetProb
                q <- self$options$planningAssumedQ
                tab <- self$results$sampleSizePlanningTable
                private$.clearTable(tab)
                probabilities <- q
                labels <- jmvcore::.("User-specified probability")
                if (is.finite(estimate) && estimate > 0 && estimate <= 1) {
                    probabilities <- c(probabilities, estimate)
                    labels <- c(labels, jmvcore::.("Observed-positive approximation"))
                }
                for (i in seq_along(probabilities)) {
                    qi <- probabilities[i]
                    n <- if (qi == 1) 1 else ceiling(log1p(-target) / log1p(-qi))
                    private$.putRow(tab, rowKey = if (i == 1) "user_spec" else "observed", values = list(
                        scenario = labels[i], planningTargetProb = target, assumedQ = qi,
                        nSamples = n, achievedProb = -expm1(n * log1p(-qi))))
                }
                self$results$sampleSizePlanningText$setContent(
                    jmvcore::.("<p>Independent-trial planning uses ceiling(log(1-target)/log(1-q)) for at least one detection. A user-specified q requires no data. This calculation is not statistical power, a confidence interval or a validated clinical protocol; an observed-positive q inherits ascertainment limitations.</p>"))
            },
            .summarizeAdequacy = function() {
                if (!self$options$showEffectSizes || is.null(self$options$totalLymphNodes)) return()
                x <- private$.numericColumn(self$data, self$options$totalLymphNodes)
                valid <- private$.validCount(x)
                if (any(!valid)) private$.addNotice("WARNING", jmvcore::.("Invalid examined-node counts"),
                    jmvcore::format(jmvcore::.("{v1} cases excluded from the adequacy summary because examined-node counts are missing or invalid."), v1 = sprintf("%d", sum(!valid))))
                x <- x[valid]
                text <- self$results$effectSizesText
                text$setContent(jmvcore::.("<p>The descriptive proportion meeting the selected examined-node threshold has a 95% Wilson interval. No treatment or protocol effect is estimated. Choose a disease-appropriate threshold; counts alone do not establish adequate staging.</p>"))
                if (!length(x)) return()
                threshold <- self$options$adequacyThreshold
                n <- length(x)
                successes <- sum(x >= threshold)
                phat <- successes / n
                z <- stats::qnorm(0.975)
                denominator <- 1 + z^2 / n
                center <- (phat + z^2 / (2 * n)) / denominator
                half <- z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2)) / denominator
                values <- list(
                    list(measure = jmvcore::.("Cases with valid examined-node counts"), value = as.character(n), interpretation = jmvcore::.("Descriptive denominator")),
                    list(measure = jmvcore::format(jmvcore::.("Cases with at least {v1} examined nodes"), v1 = sprintf("%d", threshold)), value = sprintf("%d/%d (%.1f%%)", successes, n, 100 * phat), interpretation = jmvcore::.("User-selected count threshold")),
                    list(measure = jmvcore::.("95% Wilson confidence interval"), value = sprintf("%.1f%%-%.1f%%", 100 * max(0, center-half), 100 * min(1, center+half)), interpretation = jmvcore::.("Uncertainty in the observed proportion")),
                    list(measure = jmvcore::.("Median examined nodes"), value = format(stats::median(x)), interpretation = jmvcore::.("Observed count")))
                for (i in seq_along(values)) private$.putRow(self$results$effectSizesTable, rowKey = i, values = values[[i]])
            },
            .runFiniteModels = function() {
                data <- self$data
                validCases <- rep(TRUE, nrow(data))
                invalidCases <- NULL
                maxSamp <- self$options$maxSamples
                addRecommendation <- function(...) invisible(NULL)
                # === Hypergeometric Model Analysis ===
                local({
                if (self$options$showHypergeometric && !is.null(self$options$totalPopulation) && !is.null(self$options$successStates)) {
                    # Get hypergeometric parameters

                    totalPopulationData <- jmvcore::toNumeric(data[[self$options$totalPopulation]])
                    successStatesData <- jmvcore::toNumeric(data[[self$options$successStates]])

                    # Handle labelled data
                    if (is.factor(totalPopulationData) || !is.null(attr(totalPopulationData, "labels"))) {
                        totalPopulationData <- as.numeric(as.character(totalPopulationData))
                    }
                    if (is.factor(successStatesData) || !is.null(attr(successStatesData, "labels"))) {
                        successStatesData <- as.numeric(as.character(successStatesData))
                    }

                    # Filter to valid cases
                    totalPopulationData <- totalPopulationData[validCases]
                    successStatesData <- successStatesData[validCases]
                    if (!is.null(invalidCases)) {
                        totalPopulationData <- totalPopulationData[!invalidCases]
                        successStatesData <- successStatesData[!invalidCases]
                    }

                    target <- self$options$targetDetections # Minimum detections desired
                    if (is.null(target) || is.na(target) || target < 1) {
                        target <- 1
                    }

                    # Identify cases with complete hypergeometric inputs
                    hyperValid <- !is.na(totalPopulationData) & !is.na(successStatesData)
                    if (sum(hyperValid) == 0) {
                        hypergeometricText <- self$results$hypergeometricText
                        hypergeometricTable <- self$results$hypergeometricTable
                        private$.clearTable(hypergeometricTable)
                        hyperRecommendTable <- self$results$hyperRecommendTable
                        private$.clearTable(hyperRecommendTable)
                        if (self$options$showHypergeometric) {
                            hypergeometricText$setContent(jmvcore::.("<p>No valid cases with total population and success counts were found for the hypergeometric model.</p>"))
                        }
                        private$.clearTable(hypergeometricTable)
                        private$.clearTable(hyperRecommendTable)
                        return()
                    }

                    N_values <- totalPopulationData[hyperValid]
                    K_values <- successStatesData[hyperValid]

                    hyperNotes <- character()

                    # Enforce integer counts (hypergeometric requires discrete totals)
                    if (any(abs(N_values - round(N_values)) > 1e-6, na.rm = TRUE)) {
                        hyperNotes <- c(hyperNotes, jmvcore::.("Non-integer total population counts excluded"))
                    }
                    if (any(abs(K_values - round(K_values)) > 1e-6, na.rm = TRUE)) {
                        hyperNotes <- c(hyperNotes, jmvcore::.("Non-integer positive counts excluded"))
                    }

                    N_int <- round(N_values)
                    K_int <- round(K_values)

                    # Remove impossible cases
                    invalidHyper <- !is.finite(N_values) | !is.finite(K_values) |
                        N_values != N_int | K_values != K_int |
                        (N_int <= 0) | (K_int < 0) | (K_int > N_int)
                    if (any(invalidHyper, na.rm = TRUE)) {
                        removed <- sum(invalidHyper, na.rm = TRUE)
                        hyperNotes <- c(hyperNotes, jmvcore::format(jmvcore::.("{v1} cases removed (invalid population/success counts)"), v1 = sprintf("%d", removed)))
                        N_int <- N_int[!invalidHyper]
                        K_int <- K_int[!invalidHyper]
                    }

                    if (length(N_int) == 0) {
                        hypergeometricText <- self$results$hypergeometricText
                        hypergeometricTable <- self$results$hypergeometricTable
                        private$.clearTable(hypergeometricTable)
                        hyperRecommendTable <- self$results$hyperRecommendTable
                        private$.clearTable(hyperRecommendTable)
                        if (self$options$showHypergeometric) {
                            hypergeometricText$setContent(jmvcore::.("<p>All cases were removed because population/success counts were invalid for the hypergeometric model.</p>"))
                        }
                        private$.clearTable(hypergeometricTable)
                        private$.clearTable(hyperRecommendTable)
                        return()
                    }

                    positiveCaseIdx <- K_int > 0
                    if (sum(positiveCaseIdx) == 0) {
                        hypergeometricText <- self$results$hypergeometricText
                        hypergeometricTable <- self$results$hypergeometricTable
                        private$.clearTable(hypergeometricTable)
                        hyperRecommendTable <- self$results$hyperRecommendTable
                        private$.clearTable(hyperRecommendTable)
                        note <- jmvcore::format(jmvcore::.("<p>No cases had at least {v1} positive observations, so the hypergeometric model could not be estimated.</p>"), v1 = sprintf("%d", target))
                        if (self$options$showHypergeometric) {
                            hypergeometricText$setContent(note)
                        }
                        private$.clearTable(hypergeometricTable)
                        private$.clearTable(hyperRecommendTable)
                        return()
                    }
                    removedZeroPos <- sum(!positiveCaseIdx)
                    if (removedZeroPos > 0) {
                        hyperNotes <- c(
                            hyperNotes,
                            jmvcore::format(jmvcore::.("{v1} cases excluded (no positive observations for conditional model)"), v1 = sprintf("%d", removedZeroPos))
                        )
                    }
                    N_int <- N_int[positiveCaseIdx]
                    K_int <- K_int[positiveCaseIdx]

                    nHyperCases <- length(N_int)
                    medianN <- stats::median(N_int)
                    medianK <- stats::median(K_int)

                    hypergeometricText <- self$results$hypergeometricText
                    notesHtml <- if (length(hyperNotes) > 0) {
                        jmvcore::format(jmvcore::.("<p><b>Data notes:</b> {v1}.</p>"), v1 = paste(hyperNotes, collapse = "; "))
                    } else {
                        ""
                    }

                    html <- jmvcore::format(jmvcore::.("<p><b>Hypergeometric model</b></p><p>Sampling without replacement is evaluated in {v1} positive cases. Median population size = {v2}; median positive count = {v3}.</p>{v4}<p>The probability of at least {v5} positives is calculated for each case and averaged. Draws are capped at each case's population size; cases with fewer positives than the target contribute zero.</p>"), v1 = sprintf("%d", nHyperCases), v2 = sprintf("%.0f", medianN), v3 = sprintf("%.0f", medianK), v4 = notesHtml, v5 = sprintf("%d", target), v6 = sprintf("%d", target), v7 = sprintf("%d", max(target - 1, 0)))
                    if (self$options$showHypergeometric) {
                        hypergeometricText$setContent(html)
                    }

                    # Pre-compute cumulative probabilities across cases
                    hypergeometricTable <- self$results$hypergeometricTable
                    private$.clearTable(hypergeometricTable)
                    aggregatedProb <- rep(NA_real_, maxSamp)

                    prevProb <- NA_real_
                    for (n in 1:maxSamp) {
                        draws <- pmin(n, N_int)

                        caseProb <- vapply(seq_along(N_int), function(idx) {
                            Ni <- N_int[idx]
                            Ki <- K_int[idx]
                            draw <- draws[idx]

                            if (Ki <= 0 || target > draw) {
                                return(0)
                            }

                            failures <- Ni - Ki
                            if (target > 1) {
                                probLess <- stats::phyper(target - 1, Ki, failures, draw)
                            } else {
                                probLess <- stats::dhyper(0, Ki, failures, draw)
                            }

                            1 - probLess
                        }, numeric(1))

                        if (all(is.na(caseProb))) {
                            cumProb <- NA_real_
                        } else {
                            cumProb <- mean(caseProb, na.rm = TRUE)
                        }

                        aggregatedProb[n] <- cumProb

                        marginal <- if (!is.na(cumProb) && !is.na(prevProb)) cumProb - prevProb else if (!is.na(cumProb) && is.na(prevProb)) cumProb else NA

                        private$.putRow(hypergeometricTable, rowKey = paste0("n_", n), values = list(
                            nSamples = n,
                            cumProb = cumProb,
                            marginalGain = marginal
                        ))

                        if (!is.na(cumProb)) prevProb <- cumProb
                    }

                    hyperDetail <- if (length(hyperNotes) > 0) paste(hyperNotes, collapse = "; ") else ""

                    addRecommendation(
                        method = "Hypergeometric",
                        probVec = aggregatedProb,
                        priority = 3,
                        description = jmvcore::.("Finite population model (sampling without replacement)"),
                        detail = hyperDetail
                    )

                    # Minimum samples for target confidence levels
                    hyperRecommendTable <- self$results$hyperRecommendTable
                    private$.clearTable(hyperRecommendTable)

                    confLevels <- c(0.80, 0.90, 0.95, 0.99)
                    for (i in seq_along(confLevels)) {
                        conf <- confLevels[i]

                        minSamples <- NA
                        expectedYield <- NA

                        idx <- which(!is.na(aggregatedProb) & aggregatedProb >= conf)[1]
                        if (!is.na(idx)) {
                            minSamples <- idx

                            # Expected detections based on case-level probabilities
                            caseExpected <- vapply(seq_along(N_int), function(idxCase) {
                                Ni <- N_int[idxCase]
                                Ki <- K_int[idxCase]
                                draw <- min(minSamples, Ni)

                                if (Ki <= 0 || draw <= 0) {
                                    return(0)
                                }

                                # Expected detections under hypergeometric sampling with replacement adjustment
                                expected <- draw * Ki / Ni

                                # To respect user target, cap expected at target if target specified
                                if (!is.null(target) && !is.na(target)) {
                                    expected <- min(expected, target)
                                }

                                expected
                            }, numeric(1))

                            expectedYield <- mean(caseExpected, na.rm = TRUE)
                        }

                        private$.putRow(hyperRecommendTable, rowKey = paste0("conf_", i), values = list(
                            confidence = conf,
                            minSamples = minSamples,
                            expectedYield = expectedYield
                        ))
                    }
                }

                })
                local({
                # === Beta-Binomial Model Analysis ===
                if (self$options$showBetaBinomial && !is.null(self$options$totalPopulation) && !is.null(self$options$successStates)) {
                    # Get data

                    totalPopulationData <- jmvcore::toNumeric(data[[self$options$totalPopulation]])
                    successStatesData <- jmvcore::toNumeric(data[[self$options$successStates]])

                    # Handle labelled data (parity with the hypergeometric block above)
                    if (is.factor(totalPopulationData) || !is.null(attr(totalPopulationData, "labels"))) {
                        totalPopulationData <- as.numeric(as.character(totalPopulationData))
                    }
                    if (is.factor(successStatesData) || !is.null(attr(successStatesData, "labels"))) {
                        successStatesData <- as.numeric(as.character(successStatesData))
                    }

                    # Filter to valid cases
                    totalPopulationData <- totalPopulationData[validCases]
                    successStatesData <- successStatesData[validCases]
                    if (!is.null(invalidCases)) {
                        totalPopulationData <- totalPopulationData[!invalidCases]
                        successStatesData <- successStatesData[!invalidCases]
                    }

                    betaBinomNotes <- character()
                    betaProceed <- TRUE

                    betaValid <- !is.na(totalPopulationData) & !is.na(successStatesData)
                    if (sum(betaValid) == 0) {
                        betaBinomialText <- self$results$betaBinomialText
                        betaBinomialTable <- self$results$betaBinomialTable
                        private$.clearTable(betaBinomialTable)
                        betaBinomialRecommendTable <- self$results$betaBinomialRecommendTable
                        private$.clearTable(betaBinomialRecommendTable)
                        if (self$options$showBetaBinomial) {
                            betaBinomialText$setContent(jmvcore::.("<p>No valid cases were available for the beta-binomial model (missing total population or success counts).</p>"))
                        }
                        private$.clearTable(betaBinomialTable)
                        private$.clearTable(betaBinomialRecommendTable)
                        betaProceed <- FALSE
                    } else {
                        totalPopulationData <- totalPopulationData[betaValid]
                        successStatesData <- successStatesData[betaValid]

                        invalidBeta <- !private$.validCount(totalPopulationData, positive = TRUE) |
                            !private$.validCount(successStatesData) |
                            (successStatesData > totalPopulationData)
                        if (any(invalidBeta, na.rm = TRUE)) {
                            removed <- sum(invalidBeta, na.rm = TRUE)
                            betaBinomNotes <- c(betaBinomNotes, jmvcore::format(jmvcore::.("{v1} cases removed (invalid population/success counts)"), v1 = sprintf("%d", removed)))
                            totalPopulationData <- totalPopulationData[!invalidBeta]
                            successStatesData <- successStatesData[!invalidBeta]
                        }

                        if (length(totalPopulationData) == 0) {
                            betaBinomialText <- self$results$betaBinomialText
                            betaBinomialTable <- self$results$betaBinomialTable
                            private$.clearTable(betaBinomialTable)
                            betaBinomialRecommendTable <- self$results$betaBinomialRecommendTable
                            private$.clearTable(betaBinomialRecommendTable)
                            if (self$options$showBetaBinomial) {
                                betaBinomialText$setContent(jmvcore::.("<p>All cases were removed after validating total population and success counts.</p>"))
                            }
                            private$.clearTable(betaBinomialTable)
                            private$.clearTable(betaBinomialRecommendTable)
                            betaProceed <- FALSE
                        }
                    }

                    if (betaProceed) {
                        target <- self$options$targetDetections
                        if (is.null(target) || is.na(target) || target < 1) {
                            target <- 1
                        }

                        # Estimate alpha and beta using VGAM for proper N-weighted estimation
                        # Previous unweighted method biased estimates when sample sizes varied
                        alpha <- NA_real_
                        beta <- NA_real_
                        modelRejected <- FALSE

                        # Check minimum requirements
                        if (length(totalPopulationData) < 2) {
                            betaBinomNotes <- c(betaBinomNotes, jmvcore::.("Insufficient cases for parameter estimation (need >=2)"))
                            modelRejected <- TRUE
                        } else if (!requireNamespace("VGAM", quietly = TRUE)) {
                            betaBinomNotes <- c(betaBinomNotes, jmvcore::.("VGAM package required for beta-binomial analysis. Please install: install.packages('VGAM')"))
                            modelRejected <- TRUE
                        } else {
                            # Use VGAM for proper maximum likelihood estimation with varying N
                            betaData <- data.frame(
                                success = successStatesData,
                                fail = totalPopulationData - successStatesData,
                                total = totalPopulationData
                            )

                            # Check for invalid data (negative failures)
                            if (any(betaData$fail < 0, na.rm = TRUE)) {
                                betaBinomNotes <- c(betaBinomNotes, jmvcore::.("Invalid data: success counts exceed total population in some cases"))
                                modelRejected <- TRUE
                            } else {
                                tryCatch(
                                    {
                                        # Fit beta-binomial using VGAM (proper N-weighted MLE)
                                        fitWarnings <- character()
                                        fit <- withCallingHandlers(VGAM::vglm(
                                            cbind(success, fail) ~ 1,
                                            family = VGAM::betabinomial, data = betaData, trace = FALSE
                                        ), warning = function(w) {
                                            fitWarnings <<- c(fitWarnings, conditionMessage(w))
                                            invokeRestart("muffleWarning")
                                        })
                                        if (length(fitWarnings)) {
                                            private$.addNotice("WARNING", jmvcore::.("Beta-binomial fitting diagnostics"),
                                                paste(unique(fitWarnings), collapse = "; "))
                                            if (any(grepl("half-step|converg|inaccurate|iteration limit",
                                                fitWarnings, ignore.case = TRUE))) {
                                                stop(jmvcore::.("Beta-binomial fit did not converge reliably; predictions were withheld."))
                                            }
                                        }

                                        # Extract shape parameters from the VGAM fit.
                                        # NB: VGAM::Coef() (capital C) already returns the
                                        # parameters on their NATURAL scale -- it applies the
                                        # inverse link itself. It is coef() (lowercase) that
                                        # returns linear-predictor coefficients on the logit
                                        # scale. Passing Coef()'s output through
                                        # logitlink(inverse = TRUE) transformed mu and rho a
                                        # second time: a fit with mu = 0.481 was reported as
                                        # plogis(0.481) = 0.618, and because rho lies in [0, 1]
                                        # every rho was squashed into [0.5, 0.731] -- so a
                                        # sample with no overdispersion at all (rho ~ 1e-36)
                                        # was reported as rho = 0.5. alpha and beta are derived
                                        # from both, so the whole beta-binomial fit was wrong.
                                        # unname(): Coef() returns c(mu = , rho = ), and the
                                        # names leak into downstream comparisons.
                                        coefs <- VGAM::Coef(fit)
                                        mu_fit <- unname(coefs[["mu"]])
                                        rho_fit <- unname(coefs[["rho"]])

                                        # Convert to alpha/beta parameterization
                                        # rho = 1/(alpha + beta + 1), so alpha + beta = (1-rho)/rho
                                        # mu = alpha/(alpha + beta)
                                        if (rho_fit > 0 && rho_fit < 1) {
                                            total_shape <- (1 - rho_fit) / rho_fit
                                            alpha <- mu_fit * total_shape
                                            beta <- (1 - mu_fit) * total_shape
                                        } else {
                                            betaBinomNotes <- c(betaBinomNotes, jmvcore::.("VGAM fit produced invalid rho (overdispersion parameter)"))
                                            modelRejected <- TRUE
                                        }

                                        # Validate estimated parameters
                                        if (!modelRejected && (!is.finite(alpha) || !is.finite(beta) || alpha <= 0 || beta <= 0)) {
                                            betaBinomNotes <- c(
                                                betaBinomNotes,
                                                jmvcore::.("Parameter estimation failed (non-finite or negative values). Data may not fit beta-binomial distribution.")
                                            )
                                            modelRejected <- TRUE
                                        } else if (!modelRejected) {
                                            # Check for extremely skewed parameters
                                            if (alpha < 0.01 || beta < 0.01) {
                                                betaBinomNotes <- c(
                                                    betaBinomNotes,
                                                    jmvcore::format(jmvcore::.("CAUTION: Extreme parameter values (\u03b1={v1}, \u03b2={v2}) suggest poor model fit or extreme skew. Results should be interpreted with caution."), v1 = sprintf("%.4f", alpha), v2 = sprintf("%.4f", beta))
                                                )
                                            }

                                            # Check for zero variance (all identical)
                                            if (rho_fit < 0.001) {
                                                betaBinomNotes <- c(
                                                    betaBinomNotes,
                                                    jmvcore::format(jmvcore::.("Very low overdispersion (\u03c1={v1}): Cases have nearly identical detection rates. Simple binomial model may be more appropriate."), v1 = sprintf("%.4f", rho_fit))
                                                )
                                            }

                                            # Add note about N-weighted estimation
                                            betaBinomNotes <- c(
                                                betaBinomNotes,
                                                jmvcore::format(jmvcore::.("Parameters estimated using N-weighted MLE via VGAM (\u03bc={v1}, \u03c1={v2}, \u03b1={v3}, \u03b2={v4})"), v1 = sprintf("%.3f", mu_fit), v2 = sprintf("%.4g", rho_fit), v3 = private$.fmtShape(alpha), v4 = private$.fmtShape(beta))
                                            )
                                        }
                                    },
                                    error = function(e) {
                                        # `<<-`: the handler runs in its own frame, so a plain
                                        # `<-` here would be discarded and the caller would go
                                        # on to report beta-binomial results from a failed fit.
                                        betaBinomNotes <<- c(
                                            betaBinomNotes,
                                            jmvcore::format(jmvcore::.("VGAM fitting failed: {v1}. Data may not fit beta-binomial distribution or may have convergence issues."), v1 = jmvcore::htmlEscape(conditionMessage(e)))
                                        )
                                        modelRejected <<- TRUE
                                    }
                                )
                            }
                        }

                        # If model is rejected, skip beta-binomial analysis
                        if (modelRejected) {
                            betaBinomialText <- self$results$betaBinomialText
                            betaBinomialTable <- self$results$betaBinomialTable
                            private$.clearTable(betaBinomialTable)
                            betaBinomialRecommendTable <- self$results$betaBinomialRecommendTable
                            private$.clearTable(betaBinomialRecommendTable)

                            errorHtml <- jmvcore::format(jmvcore::.("<p><b>Beta-binomial predictions unavailable</b></p><p>{v7}</p><p>Review the fitting diagnostics and data before selecting another model. Resampling does not remove ascertainment bias.</p>"), v1 = private$.styleConstants$font, v2 = private$.styleConstants$bgLight, v3 = private$.styleConstants$borderWarning, v4 = private$.styleConstants$padding15, v5 = private$.styleConstants$fontSize15, v6 = private$.styleConstants$fontSize14, v7 = paste(betaBinomNotes, collapse = " "), v8 = private$.styleConstants$fontSize14, v9 = private$.styleConstants$fontSize14)

                            if (self$options$showBetaBinomial) {
                                betaBinomialText$setContent(errorHtml)
                            }
                            private$.clearTable(betaBinomialTable)
                            private$.clearTable(betaBinomialRecommendTable)
                            betaProceed <- FALSE
                        }

                        # Beta-Binomial Text (only if model is valid)
                        if (!modelRejected) {
                            betaBinomialText <- self$results$betaBinomialText
                            extraText <- if (length(betaBinomNotes) > 0) {
                                jmvcore::format(jmvcore::.("<p><b>Estimation notes:</b> {v1}.</p>"), v1 = paste(betaBinomNotes, collapse = "; "))
                            } else {
                                ""
                            }

                            # NB: a "Zhou J, et al. Beta-binomial model for lymph node yield.
                            # Front Oncol. 2022;12:872527" reference was removed here on
                            # 2026-09-09 -- it did not resolve in PubMed by citation lookup or
                            # title search. The beta-binomial method itself is standard and does
                            # not depend on it; re-add a source only once one is confirmed.
                            html <- jmvcore::format(jmvcore::.("<p><b>Beta-binomial model</b></p><p>This model describes between-case variation in binomial sample positivity. It is not a finite-population correction or an interchangeable alternative to the hypergeometric estimand.</p><p>Estimated alpha = {v1}; beta = {v2}.</p>{v3}"), v1 = private$.fmtShape(alpha), v2 = private$.fmtShape(beta), v3 = extraText)
                            if (self$options$showBetaBinomial) {
                                betaBinomialText$setContent(html)
                            }

                            # Calculate beta-binomial probabilities
                            betaBinomialTable <- self$results$betaBinomialTable
                            private$.clearTable(betaBinomialTable)

                            betaCumProb <- rep(NA_real_, maxSamp)
                            prevProb <- 0

                            # Beta-binomial PMF.
                            # As the overdispersion rho -> 0 the beta-binomial converges to a
                            # binomial with p = alpha/(alpha+beta), and alpha+beta = (1-rho)/rho
                            # blows up. Past about 1e12 the two lbeta() terms cancel
                            # catastrophically: at alpha+beta = 1e17 this returned 2980, and at
                            # 1e18 it returned exactly 1 for every k -- a silent, finite,
                            # positive answer that no downstream guard rejects. Below the
                            # threshold the binomial limit is the numerically correct value
                            # anyway (they agree to ~1e-8 relative at alpha+beta = 1e8).
                            dbetabinom_pmf <- function(k, n, alpha, beta) {
                                if (!is.finite(alpha) || !is.finite(beta) ||
                                    (alpha + beta) > 1e12) {
                                    return(stats::dbinom(k, n, alpha / (alpha + beta)))
                                }
                                exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
                            }

                            for (n in 1:maxSamp) {
                                # P(detect >= target) using beta-binomial distribution
                                # Calculate P(X < target) = sum of P(X=k) for k=0 to target-1
                                prob_less_than_target <- 0
                                for (k in 0:(target - 1)) {
                                    if (k <= n) { # Can't have more successes than samples
                                        prob_less_than_target <- prob_less_than_target + dbetabinom_pmf(k, n, alpha, beta)
                                    }
                                }
                                # P(X >= target) = 1 - P(X < target)
                                cumProb <- 1 - prob_less_than_target

                                marginal <- cumProb - prevProb

                                private$.putRow(betaBinomialTable, rowKey = paste0("n_", n), values = list(
                                    nSamples = n,
                                    cumProb = cumProb,
                                    marginalGain = marginal
                                ))

                                prevProb <- cumProb
                                betaCumProb[n] <- cumProb
                            }

                            # Minimum samples for target confidence levels
                            betaBinomialRecommendTable <- self$results$betaBinomialRecommendTable
                            private$.clearTable(betaBinomialRecommendTable)

                            addRecommendation(
                                method = "Beta-binomial",
                                probVec = betaCumProb,
                                priority = 2,
                                description = jmvcore::.("Beta-binomial mixture of sample positivity probabilities"),
                                detail = if (length(betaBinomNotes) > 0) paste(betaBinomNotes, collapse = "; ") else ""
                            )

                            maxFeasible <- maxSamp

                            confLevels <- c(0.80, 0.90, 0.95, 0.99)
                            for (i in seq_along(confLevels)) {
                                conf <- confLevels[i]

                                minSamples <- NA
                                expectedYield <- NA

                                if (maxFeasible > 0) {
                                    for (n in 1:maxFeasible) {
                                        # Calculate P(X >= target) correctly
                                        prob_less_than_target <- 0
                                        for (k in 0:(target - 1)) {
                                            if (k <= n) {
                                                prob_less_than_target <- prob_less_than_target + dbetabinom_pmf(k, n, alpha, beta)
                                            }
                                        }
                                        cumProb <- 1 - prob_less_than_target

                                        if (cumProb >= conf) {
                                            minSamples <- n
                                            expectedYield <- n * alpha / (alpha + beta)
                                            if (!is.null(target) && !is.na(target)) {
                                                expectedYield <- expectedYield
                                            }
                                            break
                                        }
                                    }
                                }

                                private$.putRow(betaBinomialRecommendTable, rowKey = paste0("conf_", i), values = list(
                                    confidence = conf,
                                    minSamples = minSamples,
                                    expectedYield = expectedYield
                                ))
                            }
                        } # End if (!modelRejected)
                    }
                }

                })
            },
            .runLymphNodes = function() {
                data <- self$data
                validCases <- rep(TRUE, nrow(data))
                invalidCases <- NULL
                # === Lymph Node Ratio and Staging Analysis ===
                if (self$options$showLNAnalysis && !is.null(self$options$totalLymphNodes) && !is.null(self$options$positiveLymphNodes)) {
                    # Get LN variables
                    totalELN <- jmvcore::toNumeric(data[[self$options$totalLymphNodes]])
                    positiveLN <- jmvcore::toNumeric(data[[self$options$positiveLymphNodes]])

                    # Handle labelled data
                    if (is.factor(totalELN) || !is.null(attr(totalELN, "labels"))) {
                        totalELN <- as.numeric(as.character(totalELN))
                    }
                    if (is.factor(positiveLN) || !is.null(attr(positiveLN, "labels"))) {
                        positiveLN <- as.numeric(as.character(positiveLN))
                    }

                    # Filter to valid cases
                    totalELN <- totalELN[validCases]
                    positiveLN <- positiveLN[validCases]
                    if (!is.null(invalidCases)) {
                        totalELN <- totalELN[!invalidCases]
                        positiveLN <- positiveLN[!invalidCases]
                    }

                    keep <- private$.validCount(totalELN, positive = TRUE) &
                        private$.validCount(positiveLN) & positiveLN <= totalELN
                    if (any(!keep)) private$.addNotice("WARNING", jmvcore::.("Invalid lymph-node pairs"),
                        jmvcore::format(jmvcore::.("{v1} cases excluded: counts must be complete nonnegative integers, total nodes must be positive, and positive nodes cannot exceed total nodes."), v1 = sprintf("%d", sum(!keep))))
                    totalELN <- totalELN[keep]
                    positiveLN <- positiveLN[keep]
                    nCases <- length(totalELN)
                    if (nCases == 0) {
                        self$results$lnAnalysisText$setContent(jmvcore::.("No valid lymph-node pairs are available."))
                        return()
                    }
                    if (self$options$lnrThreshold1 >= self$options$lnrThreshold2) {
                        private$.addNotice("ERROR", jmvcore::.("Invalid LNR thresholds"),
                            jmvcore::.("LNR threshold 1 must be smaller than threshold 2. Correct the thresholds to calculate LNR and staging results."))
                        return()
                    }
                    # NB: a "Pu et al. 2021 (Johns Hopkins, n=1,837)" bullet and its reference
                    # were removed here on 2026-09-09. Neither the citation
                    # (Pu / J Natl Compr Canc Netw / 2021;19(9):1029-1036) nor its title resolved
                    # in PubMed; the nearest real Pu N pancreatic lymph-node paper is Adv Ther
                    # 2021;38(8):4258-4270 (PMID 34176089), a different study. It had been the
                    # stated source of minELN=12 -- which Yoon 2025 independently supports and
                    # which therefore still stands -- and of the LNR 0.1/0.3 cut-points, which
                    # are now described in the panel as conventional defaults rather than
                    # attributed to a study.
                    # LN Analysis Text
                    lnAnalysisText <- self$results$lnAnalysisText
                    html <- jmvcore::.("<p><b>Lymph-node counts and ratios</b></p><p>LNR is positive nodes divided by examined nodes. The 0.1 and 0.3 defaults are adjustable descriptive cutoffs, not universal validated thresholds.</p><p>The N0/N1/N2 count grouping shown here uses 0, 1-3 and at least 4 positive nodes, as in pancreatic adenocarcinoma. It is not a general staging algorithm; use disease-specific staging rules.</p><p>Pancreatic sampling references: Tomlinson et al., Arch Surg. 2007;142:767-774; Yoon et al., Ann Surg Oncol. 2025, doi:10.1245/s10434-025-18029-7. Study-specific findings do not determine an individual false-N0 risk.</p>")
                    if (self$options$showLNAnalysis) {
                        lnAnalysisText$setContent(html)
                    }

                    # Calculate LNR
                    LNR <- positiveLN / totalELN

                    # Get user-specified thresholds
                    threshold1 <- self$options$lnrThreshold1
                    threshold2 <- self$options$lnrThreshold2

                    # LNR Classification (Pu2021 thresholds: 0.1 and 0.3)
                    lnrStage <- character(length(LNR))
                    lnrRange <- character(length(LNR))
                    lnrStage[LNR == 0] <- "LNR0"
                    lnrRange[LNR == 0] <- "0.000"
                    lnrStage[LNR > 0 & LNR <= threshold1] <- "LNR1"
                    lnrRange[LNR > 0 & LNR <= threshold1] <- sprintf("0.001-%.3f", threshold1)
                    lnrStage[LNR > threshold1 & LNR <= threshold2] <- "LNR2"
                    lnrRange[LNR > threshold1 & LNR <= threshold2] <- sprintf("%.3f-%.3f", threshold1, threshold2)
                    lnrStage[LNR > threshold2] <- "LNR3"
                    lnrRange[LNR > threshold2] <- sprintf("%.3f-1.000", threshold2)

                    # Populate LNR Classification Table
                    lnrClassification <- self$results$lnrClassification
                    private$.clearTable(lnrClassification)

                    lnrGroups <- c("LNR0 (node-negative)", jmvcore::.("LNR1 (low burden)"), jmvcore::.("LNR2 (moderate burden)"), jmvcore::.("LNR3 (high burden)"))
                    lnrRanges <- c(
                        "0.000", sprintf("0.001-%.3f", threshold1),
                        sprintf("%.3f-%.3f", threshold1, threshold2),
                        sprintf("%.3f-1.000", threshold2)
                    )

                    for (i in 1:4) {
                        cases_in_group <- sum(lnrStage == c("LNR0", "LNR1", "LNR2", "LNR3")[i], na.rm = TRUE)
                        median_eln <- median(totalELN[lnrStage == c("LNR0", "LNR1", "LNR2", "LNR3")[i]], na.rm = TRUE)

                        private$.putRow(lnrClassification, rowKey = paste0("lnr_", i), values = list(
                            lnrStage = lnrGroups[i],
                            lnrRange = lnrRanges[i],
                            cases = cases_in_group,
                            percent = cases_in_group / nCases,
                            medianELN = if (!is.na(median_eln)) median_eln else NA
                        ))
                    }

                    # AJCC N Stage Classification
                    nStage <- character(length(positiveLN))
                    nStage[positiveLN == 0] <- "N0"
                    nStage[positiveLN >= 1 & positiveLN <= 3] <- "N1"
                    nStage[positiveLN >= 4] <- "N2"

                    # Populate AJCC N Stage Table
                    ajccNStage <- self$results$ajccNStage
                    private$.clearTable(ajccNStage)

                    nStageGroups <- c("N0", "N1", "N2")
                    nStageCriteria <- c(jmvcore::.("0 positive LN"), jmvcore::.("1-3 positive LN"), jmvcore::.(">=4 positive LN"))

                    for (i in 1:3) {
                        cases_in_stage <- sum(nStage == nStageGroups[i], na.rm = TRUE)
                        median_eln_stage <- median(totalELN[nStage == nStageGroups[i]], na.rm = TRUE)

                        private$.putRow(ajccNStage, rowKey = paste0("stage_", i), values = list(
                            nStage = nStageGroups[i],
                            criteria = nStageCriteria[i],
                            cases = cases_in_stage,
                            percent = cases_in_stage / nCases,
                            medianELN = if (!is.na(median_eln_stage)) median_eln_stage else NA
                        ))
                    }

                    # Adequacy Assessment by ELN Thresholds (Tomlinson2007 + Pu2021)
                    adequacyByELN <- self$results$adequacyByELN
                    private$.clearTable(adequacyByELN)

                    # Define ELN groups (Tomlinson 2007, Yoon 2025)
                    eln_group <- character(length(totalELN))
                    eln_group[totalELN < 9] <- "<9 ELN"
                    eln_group[totalELN >= 9 & totalELN < 12] <- "9-11 ELN"
                    eln_group[totalELN >= 12 & totalELN < 16] <- "12-15 ELN"
                    eln_group[totalELN >= 16 & totalELN < 22] <- "16-21 ELN"
                    eln_group[totalELN >= 22] <- ">=22 ELN"

                    elnGroups <- c("<9 ELN", "9-11 ELN", "12-15 ELN", "16-21 ELN", ">=22 ELN")
                    comments <- c(
                        jmvcore::.("Observed count below 9"), jmvcore::.("Observed count from 9 to 11"),
                        jmvcore::.("Observed count from 12 to 15"), jmvcore::.("Observed count from 16 to 21"),
                        jmvcore::.("Observed count of at least 22; no individual false-N0 risk estimated")
                    )

                    for (i in 1:5) {
                        cases_in_eln <- sum(eln_group == elnGroups[i], na.rm = TRUE)
                        npositive_in_eln <- sum(positiveLN[eln_group == elnGroups[i]] > 0, na.rm = TRUE)

                        private$.putRow(adequacyByELN, rowKey = paste0("eln_", i), values = list(
                            elnGroup = elnGroups[i],
                            cases = cases_in_eln,
                            percent = cases_in_eln / nCases,
                            nPositive = npositive_in_eln,
                            comment = comments[i]
                        ))
                    }
                }

            },
            # Helper to build combined styles
            .buildStyle = function(...) {
                paste(..., collapse = " ")
            },

            # Beta-binomial shape parameters span an enormous range: alpha + beta is
            # (1 - rho)/rho, so a sample with almost no overdispersion pushes them past
            # 1e27, where "%.3f" prints a 27-digit integer with three meaningless
            # decimals. Small values still read naturally.
            .fmtShape = function(x) {
                if (!is.finite(x)) {
                    return(jmvcore::.("not estimable"))
                }
                if (abs(x) >= 1e6) {
                    return(formatC(x, format = "e", digits = 2))
                }
                formatC(x, format = "f", digits = 3)
            },

            # Notice collection helpers. A single Preformatted (plain-text) output item:
            # avoids BOTH the jmvcore::Notice serialization error from
            # self$results$insert(999, Notice) AND any HTML in notices (project convention:
            # notice content must be plain text). ====
            .noticeList = list(),
            .addNotice = function(type, title, content) {
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
                # Render immediately so early-return validation aborts still display the notice
                private$.renderNotices()
            },
            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    self$results$notices$setContent("")
                    return()
                }

                # Plain text only - notices avoid HTML by project convention; the Preformatted
                # output item renders this literally (no markup, no injection surface).
                priority <- c(ERROR = 1L, STRONG_WARNING = 2L, WARNING = 3L, INFO = 4L)
                notices <- private$.noticeList[order(vapply(private$.noticeList,
                    function(x) priority[[x$type]], integer(1)))]
                blocks <- unique(vapply(notices, function(notice) {
                    prefix <- switch(notice$type,
                        ERROR          = "ERROR: ",
                        STRONG_WARNING = jmvcore::.("STRONG WARNING:"),
                        WARNING        = "WARNING: ",
                        ""
                    )
                    paste0(prefix, notice$title, "\n", notice$content)
                }, character(1)))

                self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
            },
            .init = function() {
                private$.initTableRows()
                # Welcome message when no variables selected
                welcome <- self$results$welcome
                welcomeHtml <- jmvcore::.("<p><b>Pathology sampling analysis</b></p><p>Describe recorded first-detection positions, model-based detection targets and uncertainty. Select total samples and first detection for the core analysis; independent planning, finite-population and node summaries use their own inputs.</p><p>Observed-positive detection is not independently validated disease sensitivity. Read the exclusions and model assumptions before using any result.</p>")
                welcome$setContent(welcomeHtml)

                # === Guided Instructions (Detailed Checklist) ===
                if (self$options$showGuidedInstructions) {
                    guidedInstructions <- self$results$guidedInstructions

                    guidedHtml <- jmvcore::format(jmvcore::.("<p><b>Quick start</b></p><ol><li>Select total samples and first-detection position. Use NA when no lesion was observed; this does not prove disease absence.</li><li>Choose a descriptive detection target, maximum sample count and bootstrap iterations.</li><li>Optional count, sample-type, finite-population and node analyses require their own inputs.</li><li>Check exclusions, denominators and assumptions before interpreting the results. Independent planning and node analyses can run without first-detection data.</li></ol>"), v1 = private$.styleConstants$font, v2 = private$.buildStyle(
                            private$.styleConstants$bgLight,
                            private$.styleConstants$borderLeft,
                            private$.styleConstants$padding15,
                            private$.styleConstants$margin10
                        ), v3 = private$.buildStyle(
                            private$.styleConstants$colorPrimary,
                            private$.styleConstants$fontSize16,
                            "margin: 0 0 10px 0;"
                        ), v4 = private$.buildStyle(
                            "margin: 0;",
                            "padding-left: 20px;",
                            private$.styleConstants$fontSize14,
                            private$.styleConstants$colorPrimary
                        ), v5 = private$.buildStyle(
                            "margin: 5px 0;",
                            "padding-left: 20px;",
                            private$.styleConstants$fontSize14,
                            private$.styleConstants$colorSecondary
                        ), v6 = private$.buildStyle(
                            "margin: 5px 0;",
                            "padding-left: 20px;",
                            private$.styleConstants$fontSize14,
                            private$.styleConstants$colorSecondary
                        ), v7 = private$.buildStyle(
                            "margin: 5px 0;",
                            "padding-left: 20px;",
                            private$.styleConstants$fontSize14,
                            private$.styleConstants$colorSecondary
                        ), v8 = private$.buildStyle(
                            "margin: 5px 0;",
                            "padding-left: 20px;",
                            private$.styleConstants$fontSize14,
                            private$.styleConstants$colorSecondary
                        ), v9 = private$.buildStyle(
                            "background: rgba(255, 202, 33, 0.23); color: inherit;",
                            private$.styleConstants$borderSecondary,
                            private$.styleConstants$padding10,
                            private$.styleConstants$margin10
                        ), v10 = private$.buildStyle(
                            "color: #856404;",
                            private$.styleConstants$fontSize15,
                            "margin: 0 0 8px 0;"
                        ), v11 = private$.buildStyle(
                            "margin: 0;",
                            "padding-left: 20px;",
                            "color: #856404;",
                            private$.styleConstants$fontSize14
                        ))

                    guidedInstructions$setContent(guidedHtml)
                }

                # === Concise Instructions (Brief Overview) ===
                if (self$options$showConciseInstructions) {
                    conciseInstructions <- self$results$conciseInstructions

                    conciseHtml <- jmvcore::format(jmvcore::.("<p><b>About this analysis</b></p><p>This analysis describes first detections among eventually observed-positive cases. Select total samples and first-detection position, leaving the position missing when no lesion was observed. Model-based targets and resampling intervals do not establish clinical sampling adequacy.</p>"), v1 = private$.styleConstants$font, v2 = private$.styleConstants$colorPrimary, v3 = private$.styleConstants$fontSize16, v4 = private$.styleConstants$fontSize14, v5 = private$.styleConstants$colorPrimary, v6 = private$.styleConstants$fontSize14, v7 = private$.styleConstants$colorPrimary, v8 = private$.styleConstants$fontSize14, v9 = private$.styleConstants$colorPrimary, v10 = private$.styleConstants$fontSize14, v11 = private$.styleConstants$colorPrimary)

                    conciseInstructions$setContent(conciseHtml)
                }

                # Visibility of the welcome panel is declarative -- see `visible:` on the
                # `welcome` item in pathsampling.r.yaml. The imperative setVisible() pair that
                # used to live here disagreed with it (&& here vs. || in the schema), so
                # selecting exactly one of the two required variables gave contradictory rules.
                if (is.null(self$options$totalSamples) && is.null(self$options$firstDetection)) {
                    return()
                }
            },
            .run = function() {
                private$.resetResults()
                private$.planSamples()
                private$.runFiniteModels()
                private$.runLymphNodes()
                private$.summarizeAdequacy()
                private$.checkOptionalInputs()


                # Get required variables
                totalSamples <- self$options$totalSamples
                firstDetection <- self$options$firstDetection

                # Get optional enhanced variables
                positiveCount <- self$options$positiveCount
                positiveSamplesList <- self$options$positiveSamplesList
                sampleType <- self$options$sampleType

                # Validate required variables
                if (is.null(totalSamples) || is.null(firstDetection)) {
                    # Variables not selected - show instructions only
                    return()
                }

                # Get options and validate
                targetConf <- self$options$targetConfidence
                maxSamp <- self$options$maxSamples
                nBoot <- self$options$bootstrapIterations
                analysisContext <- if (is.null(self$options$analysisContext)) "general" else self$options$analysisContext

                # addRow() does not reject a duplicate rowKey, so a second .run() on the
                # same instance -- what jamovi does whenever the user toggles an option that
                # is not in this table's clearWith -- appended a second copy of every row and
                # then failed with "non-unique values when setting 'row.names'". dataInfo is
                # fetched at seven points (error paths and the main summary), so it is cleared
                # once here rather than at each fetch.
                private$.clearTable(self$results$dataInfo)

                # Flag to skip Binomial model if assumptions violated
                skipBinomial <- FALSE

                # pForCalc is assigned inside `if (self$options$showBinomialModel)`, but the
                # population-detection section reads it unconditionally. Both options default
                # to FALSE, so ticking "Population-Level Detection Rates" on its own used to
                # abort the whole analysis with "object 'pForCalc' not found".
                pForCalc <- NA_real_

                # Prepare recommendation collector to summarize minimum samples for target confidence
                recommendations <- list()
                addRecommendation <- function(method, probVec, priority, description, detail = NULL, ci = NULL) {
                    # Extend recommendation list with standardized fields and target-driven summary
                    if (length(probVec) == 0 || all(is.na(probVec))) {
                        status <- jmvcore::.("No valid probabilities available")
                        rec <- list(
                            method = method,
                            description = description,
                            minSamples = NA_integer_,
                            achievedProb = NA_real_,
                            bestProb = NA_real_,
                            bestN = NA_integer_,
                            status = status,
                            detail = ifelse(is.null(detail) || detail == "", "", detail),
                            priority = priority,
                            ciLower = ifelse(is.null(ci), NA_real_, ci[1]),
                            ciUpper = ifelse(is.null(ci), NA_real_, ci[2])
                        )
                        recommendations <<- c(recommendations, list(rec))
                        return(invisible())
                    }

                    validIdx <- which(!is.na(probVec))
                    if (length(validIdx) == 0) {
                        status <- jmvcore::.("No valid probabilities available")
                        rec <- list(
                            method = method,
                            description = description,
                            minSamples = NA_integer_,
                            achievedProb = NA_real_,
                            bestProb = NA_real_,
                            bestN = NA_integer_,
                            status = status,
                            detail = ifelse(is.null(detail) || detail == "", "", detail),
                            priority = priority,
                            ciLower = ifelse(is.null(ci), NA_real_, ci[1]),
                            ciUpper = ifelse(is.null(ci), NA_real_, ci[2])
                        )
                        recommendations <<- c(recommendations, list(rec))
                        return(invisible())
                    }

                    targetCandidates <- validIdx[probVec[validIdx] >= targetConf]
                    targetIdx <- if (length(targetCandidates) > 0) targetCandidates[1] else NA_integer_

                    bestInValid <- which.max(probVec[validIdx])
                    bestIdx <- validIdx[bestInValid]
                    bestProb <- probVec[bestIdx]

                    if (!is.na(targetIdx)) {
                        minSamples <- targetIdx
                        achievedProb <- probVec[targetIdx]
                        status <- jmvcore::format(jmvcore::.("Meets target at {v1} samples ({v2}%)"), v1 = sprintf("%d", minSamples), v2 = sprintf("%.1f", achievedProb * 100))
                    } else {
                        minSamples <- NA_integer_
                        achievedProb <- bestProb
                        status <- ifelse(length(validIdx) > 0,
                            jmvcore::format(jmvcore::.("Target not reached; best {v1}% at {v2} samples"), v1 = sprintf("%.1f", bestProb * 100), v2 = sprintf("%d", bestIdx)),
                            jmvcore::.("Target not reached")
                        )
                    }

                    detailParts <- character()
                    if (!is.null(detail) && nzchar(detail)) {
                        detailParts <- c(detailParts, detail)
                    }
                    if (!is.null(ci) && length(ci) == 2 && all(is.finite(ci))) {
                        detailParts <- c(
                            detailParts,
                            sprintf("95%% CI %.1f%%-%.1f%%", ci[1] * 100, ci[2] * 100)
                        )
                    }
                    detailText <- paste(detailParts, collapse = "; ")

                    rec <- list(
                        method = method,
                        description = description,
                        minSamples = minSamples,
                        achievedProb = achievedProb,
                        bestProb = bestProb,
                        bestN = bestIdx,
                        status = status,
                        detail = detailText,
                        priority = priority,
                        ciLower = ifelse(is.null(ci), NA_real_, ci[1]),
                        ciUpper = ifelse(is.null(ci), NA_real_, ci[2])
                    )

                    recommendations <<- c(recommendations, list(rec))
                }

                # Storage for bootstrap-derived summary (used later for clinical summary narrative)
                bootstrapTargetIdx <- NA_integer_
                bootstrapMeansVec <- NULL
                bootstrapCILowerVec <- NULL
                bootstrapCIUpperVec <- NULL

                # Validate target confidence range
                if (targetConf <= 0 || targetConf >= 1) {
                    dataInfo <- self$results$dataInfo
                    private$.putRow(dataInfo, rowKey = "error_conf", values = list(
                        measure = "ERROR",
                        value = jmvcore::.("Target confidence must be between 0 and 1")
                    ))
                    private$.addNotice(
                        "ERROR",
                        jmvcore::.("Invalid target confidence"),
                        jmvcore::format(jmvcore::.("Target confidence must be strictly between 0 and 1; {v1} was supplied. Set it to a value such as 0.95 and re-run."), v1 = format(targetConf, trim = TRUE))
                    )
                    return()
                }

                # Warning for extreme confidence levels
                if (targetConf > 0.99) {
                    private$.addNotice(
                        "WARNING",
                        jmvcore::.("Extreme confidence level"),
                        jmvcore::format(jmvcore::.("A target of {value}% may require an impractical number of samples. This is a descriptive target, not a clinical standard."), value = sprintf("%.1f", targetConf * 100))
                    )
                }

                # Warning for insufficient bootstrap iterations
                if (self$options$showBootstrap && nBoot < 1000) {
                    private$.addNotice(
                        "WARNING",
                        jmvcore::.("Low bootstrap iterations"),
                        jmvcore::format(jmvcore::.("Only {value} bootstrap iterations were requested. The percentile intervals may have noticeable Monte Carlo variation; increase iterations and assess stability."), value = nBoot)
                    )
                }

                # One row mask keeps all core and optional columns aligned.
                data <- self$data
                rawTotalSamplesData <- private$.numericColumn(data, totalSamples)
                rawFirst <- private$.numericColumn(data, firstDetection)
                totalCasesInput <- nrow(data)
                nExcludedMissingTotal <- sum(is.na(rawTotalSamplesData))
                validRows <- private$.validCount(rawTotalSamplesData, positive = TRUE) &
                    (is.na(rawFirst) | (private$.validCount(rawFirst, positive = TRUE) &
                    rawFirst <= rawTotalSamplesData))
                nExcludedInvalidDetection <- sum(!is.na(rawFirst) &
                    (!private$.validCount(rawFirst, positive = TRUE) |
                    rawFirst > rawTotalSamplesData), na.rm = TRUE)
                dataWarnings <- character()
                if (any(!validRows)) {
                    dataWarnings <- jmvcore::format(jmvcore::.("{v1} cases excluded because total samples or first-detection positions were missing or invalid."), v1 = sprintf("%d", sum(!validRows)))
                    private$.addNotice("WARNING", jmvcore::.("Excluded invalid sampling data"), dataWarnings)
                }
                if (!is.null(sampleType)) data[[sampleType]] <- private$.groupColumn(data[[sampleType]])
                data <- data[validRows, , drop = FALSE]
                totalSamplesData <- rawTotalSamplesData[validRows]
                firstDetectionData <- rawFirst[validRows]
                validCases <- rep(TRUE, nrow(data))
                invalidCases <- NULL
                if (nrow(data) == 0) {
                    private$.addNotice("ERROR", jmvcore::.("No analysable cases"),
                        jmvcore::.("Supply at least one positive integer total sample count and a valid first-detection position, or a missing position if no lesion was detected."))
                    return()
                }
                positiveCountData <- if (!is.null(positiveCount))
                    private$.numericColumn(data, positiveCount) else NULL
                positiveSamplesListData <- if (!is.null(positiveSamplesList))
                    as.character(data[[positiveSamplesList]]) else NULL
                sampleTypeData <- if (!is.null(sampleType))
                    private$.groupColumn(data[[sampleType]]) else NULL
                if (!is.null(positiveCountData)) {
                    badCount <- !private$.validCount(positiveCountData) |
                        positiveCountData > totalSamplesData |
                        (!is.na(firstDetectionData) & positiveCountData < 1) |
                        (is.na(firstDetectionData) & positiveCountData > 0)
                    if (any(badCount)) private$.addNotice("WARNING", jmvcore::.("Positive count exclusions"),
                        jmvcore::format(jmvcore::.("{v1} cases have missing, invalid or inconsistent positive counts and are excluded from count-based estimation; their first-detection data remain available."), v1 = sprintf("%d", sum(badCount))))
                    positiveCountData[badCount] <- NA_real_
                }
                if (!is.null(positiveSamplesListData)) {
                    badList <- vapply(seq_along(positiveSamplesListData), function(i) {
                        x <- private$.parseSampleList(positiveSamplesListData[i])
                        if (!length(x)) return(!is.na(firstDetectionData[i]))
                        anyNA(x) || any(x > totalSamplesData[i]) ||
                            is.na(firstDetectionData[i]) || min(x) != firstDetectionData[i] ||
                            (!is.null(positiveCountData) && !is.na(positiveCountData[i]) &&
                                length(x) != positiveCountData[i])
                    }, logical(1))
                    if (any(badList)) private$.addNotice("WARNING", jmvcore::.("Invalid positive sample lists"),
                        jmvcore::format(jmvcore::.("{v1} lists excluded: use unique integer positions within the total count, consistent with first detection and the positive count when supplied."), v1 = sprintf("%d", sum(badList))))
                    positiveSamplesListData[badList] <- NA_character_
                }
                detectedCases <- !is.na(firstDetectionData)
                nDetected <- sum(detectedCases)
                if (nDetected == 0) {
                    private$.addNotice("ERROR", jmvcore::.("No detected lesions"),
                        jmvcore::.("No case records a first detection, so observed-positive detection curves cannot be estimated. Independent planning and lymph-node results remain available when requested."))
                    return()
                }
                private$.addNotice("STRONG_WARNING", jmvcore::.("Observed-positive cases only"),
                    jmvcore::.("Detection curves and recommendations describe cases in which a lesion was eventually observed. Undetected disease and finite sampling windows are not identified by these data; results are not validated population sensitivity or a clinical rule-out guarantee. Geometric estimates are descriptive approximations to the observed first-detection distribution."))
                # Warning 1: Small sample size (detected cases only)
                if (nDetected < 10) {
                    private$.addNotice(
                        "WARNING",
                        jmvcore::.("Small sample size"),
                        jmvcore::format(jmvcore::.("Only {value} cases recorded a detected lesion. Estimates and bootstrap intervals may be unstable; assess the observation design and collect more cases before drawing conclusions."), value = nDetected)
                    )
                }

                # Set seed if requested - save and restore global RNG state so subsequent
                # random draws elsewhere in the user's session are not affected by our seed.
                if (self$options$setSeed) {
                    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
                        get(".Random.seed", envir = .GlobalEnv)
                    } else {
                        NULL
                    }
                    on.exit(
                        {
                            if (!is.null(old_seed)) {
                                assign(".Random.seed", old_seed, envir = .GlobalEnv)
                            } else if (exists(".Random.seed", envir = .GlobalEnv)) {
                                rm(".Random.seed", envir = .GlobalEnv)
                            }
                        },
                        add = TRUE
                    )
                    set.seed(self$options$seedValue)
                }

                # === Data Summary ===
                dataInfo <- self$results$dataInfo

                nCases <- length(totalSamplesData)
                nNoDetection <- nCases - nDetected

                examinedDetected <- sum(firstDetectionData[detectedCases], na.rm = TRUE)
                examinedNondetected <- sum(totalSamplesData[!detectedCases], na.rm = TRUE)
                totalExamined <- examinedDetected + examinedNondetected # Samples actually examined across all cases
                meanSamplesPerCase <- mean(totalSamplesData, na.rm = TRUE)
                medianFirst <- if (nDetected > 0) median(firstDetectionData[detectedCases], na.rm = TRUE) else NA

                dataInfo <- self$results$dataInfo

                totalSubmittedRaw <- sum(rawTotalSamplesData[private$.validCount(rawTotalSamplesData, positive = TRUE)])

                private$.putRow(dataInfo, rowKey = "total_cases", values = list(
                    measure = jmvcore::.("Total cases supplied"),
                    value = as.character(totalCasesInput)
                ))

                private$.putRow(dataInfo, rowKey = "cases_analyzed", values = list(
                    measure = jmvcore::.("Cases analyzed"),
                    value = as.character(nCases)
                ))

                if (nExcludedMissingTotal > 0) {
                    private$.putRow(dataInfo, rowKey = "excluded_missing", values = list(
                        measure = jmvcore::.("Excluded: missing total samples"),
                        value = as.character(nExcludedMissingTotal)
                    ))
                }

                if (nExcludedInvalidDetection > 0) {
                    private$.putRow(dataInfo, rowKey = "excluded_invalid", values = list(
                        measure = jmvcore::.("Excluded: first detection > total"),
                        value = as.character(nExcludedInvalidDetection)
                    ))
                }

                private$.putRow(dataInfo, rowKey = "total_input", values = list(
                    measure = jmvcore::.("Total samples (input)"),
                    value = sprintf("%s (recorded)", format(totalSubmittedRaw, trim = TRUE))
                ))

                private$.putRow(dataInfo, rowKey = "total_analyzed", values = list(
                    measure = jmvcore::.("Total samples analyzed"),
                    value = jmvcore::format(jmvcore::.("{v1} (up to first detection)"), v1 = format(totalExamined, trim = TRUE))
                ))

                private$.putRow(dataInfo, rowKey = "mean_samples", values = list(
                    measure = jmvcore::.("Mean samples per analyzed case"),
                    value = sprintf("%.2f", meanSamplesPerCase)
                ))

                private$.putRow(dataInfo, rowKey = "median_first", values = list(
                    measure = jmvcore::.("Median first detection"),
                    value = if (!is.na(medianFirst)) sprintf("%.0f", medianFirst) else jmvcore::.("No lesions detected")
                ))

                private$.putRow(dataInfo, rowKey = "no_detection", values = list(
                    measure = jmvcore::.("Cases without detected lesion"),
                    value = sprintf("%d", nNoDetection)
                ))

                if (length(dataWarnings) > 0) {
                    private$.putRow(dataInfo, rowKey = "data_notes", values = list(
                        measure = jmvcore::.("Data notes"),
                        value = paste(dataWarnings, collapse = "; ")
                    ))
                }

                # === Binomial Model ===
                pEstimate <- NA_real_
                estimationMethod <- jmvcore::.("Not calculated")

                needEstimate <- self$options$showBinomialModel || self$options$showDetectionCurve ||
                    self$options$showModelFit || self$options$showObsPred ||
                    self$options$showSampleSizePlanning || self$options$showMultifocalAnalysis ||
                    self$options$showPopulationDetection || self$options$showProbabilityExplanation ||
                    self$options$showEmpiricalCumulative || self$options$showStratifiedAnalysis
                if (needEstimate) {
                    estimate <- private$.estimateQ(firstDetectionData, totalSamplesData, positiveCountData)
                    pEstimate <- pForCalc <- estimate$q
                    skipBinomial <- estimate$rejected
                    estimationMethod <- if (estimate$method == "empirical")
                        jmvcore::.("Empirical Proportion (uses all positive samples)") else
                        jmvcore::.("Detected-case geometric approximation")
                    binomialText <- self$results$binomialText
                    diagnostic <- ""
                    if (skipBinomial) {
                        diagnostic <- jmvcore::.("Binomial Model Not Applicable: the case-proportion CV exceeds 0.5. This screening heuristic is not a formal independence test.")
                        private$.addNotice("STRONG_WARNING", jmvcore::.("Binomial predictions withheld"),
                            jmvcore::.("The pooled case-proportion CV exceeds 0.5, so pooled binomial predictions are withheld. Subgroups are assessed separately using the same estimator and screening rule."))
                    } else if (is.finite(estimate$cv) && estimate$cv > 0.3) {
                        diagnostic <- jmvcore::.("MODERATE HETEROGENEITY: case-proportion CV exceeds 0.3. Interpret the pooled approximation with caution.")
                        private$.addNotice("WARNING", jmvcore::.("Variation in case proportions"), diagnostic)
                    } else if (!is.finite(pEstimate)) {
                        diagnostic <- jmvcore::.("Could not estimate detection probability from the available inputs. Empirical estimation requires valid positive-count pairs among eventually detected cases.")
                    }
                    if (self$options$showBinomialModel) {
                        binomialText$setContent(paste0(
                            jmvcore::.("<p><b>Independent-trial approximation among eventually observed-positive cases</b></p>"),
                            jmvcore::format(jmvcore::.("<p>Estimator: {v1}; q = {v2}; eligible cases = {v3}.</p>"), v1 = estimationMethod, v2 = if (is.finite(pEstimate)) sprintf("%.4f", pEstimate) else "NA", v3 = sprintf("%d", estimate$n)),
                            jmvcore::.("<p>Modelled probability at n samples is 1 - (1-q)^n. This assumes independent trials with constant probability and does not correct unequal observation windows, missed disease or spatial dependence.</p>"),
                            sprintf("<p>%s</p>", diagnostic)))
                    }
                    private$.putRow(dataInfo, "binomial_method", list(
                        measure = jmvcore::.("Binomial estimator"), value = estimationMethod))

                    # Calculate detection probabilities for different sample sizes
                    binomialTable <- self$results$binomialTable
                    private$.clearTable(binomialTable)

                    binomProbVec <- rep(NA_real_, maxSamp)
                    prevProb <- 0
                    for (i in 1:maxSamp) {
                        if (is.na(pForCalc)) {
                            cumProb <- NA
                            marginal <- NA
                        } else {
                            cumProb <- 1 - (1 - pForCalc)^i
                            marginal <- cumProb - prevProb
                        }

                        private$.putRow(binomialTable, rowKey = paste0("n_", i), values = list(
                            nSamples = i,
                            cumProb = cumProb,
                            marginalGain = marginal
                        ))
                        if (!is.na(cumProb)) {
                            prevProb <- cumProb
                            binomProbVec[i] <- cumProb
                        }
                    }

                    # Minimum samples for different confidence levels
                    recommendTable <- self$results$recommendTable
                    private$.clearTable(recommendTable)

                    confLevels <- c(0.80, 0.90, 0.95, 0.99)
                    for (i in seq_along(confLevels)) {
                        conf <- confLevels[i]
                        if (is.na(pForCalc) || pForCalc <= 0) {
                            nMin <- NA
                        } else if (pForCalc >= 0.9999) {
                            # For very high probabilities, minimum samples is effectively 1
                            nMin <- 1
                        } else {
                            # Use log1p for better numerical stability
                            # Formula: n = log(1-conf) / log(1-p) = log1p(-conf) / log1p(-p)
                            nMin <- ceiling(log1p(-conf) / log1p(-pForCalc))

                            # Sanity check: if result is negative or extremely large, set to NA
                            if (!is.finite(nMin) || nMin < 1 || nMin > 10000) {
                                nMin <- NA
                            }
                        }

                        private$.putRow(recommendTable, rowKey = paste0("conf_", conf), values = list(
                            confidence = conf,
                            minSamples = nMin
                        ))
                    }

                    addRecommendation(
                        method = "Binomial",
                        probVec = binomProbVec,
                        priority = 4,
                        description = jmvcore::.("Independent detection probability model"),
                        detail = if (!is.na(pEstimate)) sprintf("p = %.4f", pEstimate) else jmvcore::.("Per-sample probability unavailable")
                    )
                }

                # === Heterogeneity Test ===
                if (self$options$showHeterogeneityTest) {
                    heterogeneityText <- self$results$heterogeneityText
                    heterogeneityTable <- self$results$heterogeneityTest
                    private$.clearTable(heterogeneityTable)

                    if (is.null(sampleTypeData)) {
                        heterogeneityText$setContent(jmvcore::.("To test for heterogeneity, please specify a 'Sample Type' variable."))
                    } else {
                        het_results <- private$.testHeterogeneity(firstDetectionData, sampleTypeData)

                        if (!is.na(het_results$statistic)) {
                            private$.putRow(heterogeneityTable, rowKey = "het_test", values = list(
                                test = jmvcore::.("Likelihood Ratio Test"),
                                statistic = het_results$statistic,
                                df = het_results$df,
                                pValue = het_results$pValue,
                                interpretation = het_results$interpretation
                            ))

                            # Add explanatory text
                            html <- jmvcore::format(jmvcore::.("<p><b>Group comparison:</b> {v2}</p><p>The likelihood-ratio test compares pooled and group-specific geometric working models among eventual detections. Unequal observation windows or sparse groups can undermine this approximation; the test does not establish clinical differences.</p>"), v1 = private$.styleConstants$font, v2 = het_results$interpretation)
                            heterogeneityText$setContent(html)
                        } else {
                            heterogeneityText$setContent(het_results$interpretation)
                        }
                    }
                }

                # === Model Fit Assessment ===
                if (self$options$showModelFit) {
                    modelFitText <- self$results$modelFitText
                    modelFitTable <- self$results$modelFitTable
                    private$.clearTable(modelFitTable)

                    if (exists("estimate") && estimate$method != "geometric") {
                        modelFitText$setContent(jmvcore::.("A goodness-of-fit p-value is available only for the detected-case geometric estimator. Empirical-count q is estimated from a different outcome and its test calibration has not been validated. Use the observed-versus-predicted table for descriptive comparison."))
                    } else if (is.na(pEstimate)) {
                        modelFitText$setContent(jmvcore::.("Cannot assess model fit: detection probability (q) could not be estimated."))
                    } else {
                        fit_results <- private$.testModelFit(firstDetectionData, pEstimate)

                        if (!is.na(fit_results$chiSquare)) {
                            private$.putRow(modelFitTable, rowKey = "fit_test", values = list(
                                test = jmvcore::.("Chi-Square Goodness of Fit"),
                                chiSquare = fit_results$chiSquare,
                                df = fit_results$df,
                                pValue = fit_results$pValue,
                                fitQuality = fit_results$fitQuality
                            ))

                            # Add explanatory text
                            html <- jmvcore::format(jmvcore::.("<p><b>Model fit:</b> {v2}</p><p>The approximate chi-square test assesses an untruncated geometric working model among eventual detections. A large p-value does not establish model validity, independent sampling or clinical sensitivity. No p-value is calculated for empirical-count q.</p>"), v1 = private$.styleConstants$font, v2 = fit_results$fitQuality)
                            modelFitText$setContent(html)
                        } else {
                            modelFitText$setContent(fit_results$fitQuality)
                        }
                    }
                }

                # === Observed vs Predicted Table ===
                if (self$options$showObsPred) {
                    obsPredText <- self$results$obsPredText
                    obsPredTable <- self$results$obsPredTable
                    private$.clearTable(obsPredTable)

                    if (is.na(pEstimate)) {
                        private$.deleteRows(obsPredTable)
                        obsPredText$setContent(jmvcore::.("Cannot compare observed vs predicted: detection probability (q) could not be estimated."))
                    } else {
                        op_results <- private$.calculateObsPred(firstDetectionData, pEstimate, maxSamp)

                        if (nrow(op_results) > 0) {
                            for (i in seq_len(nrow(op_results))) {
                                private$.putRow(obsPredTable, rowKey = paste0("op_", i), values = list(
                                    nSamples = op_results$nSamples[i],
                                    observed = op_results$observed[i],
                                    predicted = op_results$predicted[i],
                                    difference = op_results$difference[i],
                                    assessment = op_results$assessment[i]
                                ))
                            }

                            obsPredText$setContent(jmvcore::.("<p>Comparison of observed cumulative detection rates vs. model predictions. Large differences indicate model misfit.</p>"))
                        } else {
                            obsPredText$setContent(jmvcore::.("No positive cases available for comparison."))
                        }
                    }
                }

                private$.planSamples(pEstimate)

                # === Multi-Focal Analysis ===
                if (self$options$showMultifocalAnalysis) {
                    multifocalText <- self$results$multifocalAnalysisText
                    multifocalTable <- self$results$multifocalProbTable
                    private$.clearTable(multifocalTable)

                    q_val <- pEstimate
                    if (!is.finite(q_val)) private$.deleteRows(multifocalTable)

                    for (i in if (is.finite(q_val)) seq_len(maxSamp) else integer()) {
                        # P(X >= k) = 1 - pbinom(k-1, n, p)
                        p_ge_1 <- 1 - pbinom(0, i, q_val)
                        p_ge_2 <- 1 - pbinom(1, i, q_val)
                        p_ge_3 <- 1 - pbinom(2, i, q_val)

                        private$.putRow(multifocalTable, rowKey = i, values = list(
                            nSamples = i,
                            detectOne = p_ge_1,
                            detectTwo = p_ge_2,
                            detectThree = p_ge_3
                        ))
                    }

                    html <- jmvcore::format(jmvcore::.("<p><b>Multiple positive samples</b></p><p>Probabilities use independent trials with q = {v2}. Several positive samples may come from the same focus; these probabilities do not count distinct anatomical lesions.</p>"), v1 = private$.styleConstants$font, v2 = sprintf("%.3f", q_val))
                    multifocalText$setContent(if (is.finite(q_val)) html else jmvcore::.("Positive-sample predictions are unavailable because no applicable per-sample probability could be estimated."))
                }

                if (self$options$autoSelectModel) {
                    self$results$modelSelectionText$setContent(jmvcore::format(jmvcore::.("<p><b>Model applicability guide</b></p><p>Empirical curves describe {v1} eventual detections; the independent-trial approximation is {v2}. Hypergeometric sampling requires known finite counts and no replacement. Beta-binomial models describe between-case variation, not finite-population correction. Different estimands are not automatically ranked.</p>"), v1 = sprintf("%d", nDetected), v2 = if (is.finite(pEstimate)) jmvcore::.("estimated; assess independence and ascertainment before use") else jmvcore::.("unavailable or withheld")))
                }

                # === Auto-Detect Heterogeneity (Warning Only) ===
                if (self$options$autoDetectHeterogeneity && !is.null(sampleTypeData)) {
                    auto_het <- private$.autoDetectHeterogeneity(firstDetectionData, sampleTypeData)

                    if (auto_het$warning) {
                        # Goes to the notices item: the Statistical Interpretation section
                        # later in .run() calls interpretText$setContent() unconditionally,
                        # which would overwrite anything written here.
                        private$.addNotice(
                            "WARNING",
                            jmvcore::.("Heterogeneity detected"),
                            auto_het$message
                        )
                    }
                }

                if (self$options$showProbabilityExplanation) {
                    observed <- vapply(c(3, 5, 10), function(n)
                        mean(firstDetectionData[detectedCases] <= n), numeric(1))
                    self$results$probabilityExplanation$setContent(jmvcore::format(jmvcore::.("<p><b>Two descriptive quantities</b></p><p>Among {v1} eventually detected cases, {v2}% were detected by three samples, {v3}% by five and {v4}% by ten. Multiplying these fractions by the observed-positive fraction ({v5}/{v6}) gives the fraction of all recorded cases detected at that position.</p><p>Neither quantity establishes true disease prevalence or sensitivity. Geometric approximations and bootstrap intervals do not correct missed disease, truncation, censoring or dependence.</p>"), v1 = sprintf("%d", nDetected), v2 = sprintf("%.1f", 100 * observed[1]), v3 = sprintf("%.1f", 100 * observed[2]), v4 = sprintf("%.1f", 100 * observed[3]), v5 = sprintf("%d", nDetected), v6 = sprintf("%d", length(firstDetectionData))))
                }

                # === Bootstrap Analysis ===
                if (self$options$showBootstrap) {
                    # Validation: Check if we have enough positive cases for bootstrap
                    if (nDetected < 3) {
                        bootstrapText <- self$results$bootstrapText
                        errorHtml <- jmvcore::format(jmvcore::.("<p><b>Insufficient data for bootstrap</b></p><p>At least 3 eventually detected cases are required; {v7} are available. Collect more observations before interpreting resampling intervals.</p>"), v1 = private$.styleConstants$font, v2 = private$.styleConstants$bgLight, v3 = private$.styleConstants$borderWarning, v4 = private$.styleConstants$padding15, v5 = private$.styleConstants$fontSize14, v6 = private$.styleConstants$fontSize14, v7 = sprintf("%d", nDetected), v8 = if (nDetected == 1) "" else "s", v9 = private$.styleConstants$fontSize14)
                        bootstrapText$setContent(errorHtml)
                    } else {
                        bootstrapText <- self$results$bootstrapText
                        html <- jmvcore::format(jmvcore::.("<p><b>Bootstrap resampling</b></p><p>{v6} case-resampling iterations estimate the observed-positive detection curve and 95% percentile intervals. Cases are resampled independently; missed disease and unequal sampling windows are not corrected.</p>"), v1 = private$.styleConstants$font, v2 = private$.buildStyle(
                                private$.styleConstants$bgLight,
                                private$.styleConstants$borderLeft,
                                private$.styleConstants$padding15,
                                private$.styleConstants$margin10
                            ), v3 = private$.buildStyle(
                                private$.styleConstants$colorPrimary,
                                private$.styleConstants$fontSize15,
                                "margin: 0 0 10px 0;"
                            ), v4 = private$.buildStyle(
                                private$.styleConstants$fontSize14,
                                private$.styleConstants$colorPrimary,
                                "margin: 0 0 10px 0;"
                            ), v5 = private$.styleConstants$colorPrimary, v6 = sprintf("%d", nBoot), v7 = private$.buildStyle(
                                private$.styleConstants$fontSize14,
                                private$.styleConstants$colorPrimary,
                                "margin: 0 0 10px 0;"
                            ), v8 = private$.buildStyle(
                                private$.styleConstants$fontSize14,
                                private$.styleConstants$colorPrimary,
                                "margin: 0;"
                            ))
                        if (self$options$showBootstrap) {
                            bootstrapText$setContent(html)
                        }

                        private$.addNotice("STRONG_WARNING", jmvcore::.("Bootstrap Selection Bias"),
                            jmvcore::.("Bootstrap resamples only eventually detected cases. Its intervals quantify variation in that conditional distribution; they do not correct missed disease, ascertainment bias or unequal sampling windows."))
                        bootstrapResults <- private$.bootstrapCDF(firstDetectionData, maxSamp, nBoot)

                        # Store for plotting
                        private$.bootstrapResults <- bootstrapResults
                        self$results$sensitivityPlot$setState(list(
                            bootstrapResults = bootstrapResults,
                            maxSamp = maxSamp,
                            targetConfidence = self$options$targetConfidence,
                            bootstrapIterations = self$options$bootstrapIterations
                        ))

                        bootstrapMeans <- bootstrapCILower <- bootstrapCIUpper <- numeric(maxSamp)
                        # Calculate summary statistics from bootstrap results
                        for (j in 1:maxSamp) {
                            bootstrapMeans[j] <- mean(bootstrapResults[, j], na.rm = TRUE)
                            bootstrapCILower[j] <- quantile(bootstrapResults[, j], probs = 0.025, na.rm = TRUE)
                            bootstrapCIUpper[j] <- quantile(bootstrapResults[, j], probs = 0.975, na.rm = TRUE)
                        }

                        # Populate bootstrap table
                        bootstrapTable <- self$results$bootstrapTable

                        # Clear existing rows (if any)
                        private$.clearTable(bootstrapTable)

                        for (i in 1:maxSamp) {
                            meanVal <- bootstrapMeans[i]
                            ciLower <- bootstrapCILower[i]
                            ciUpper <- bootstrapCIUpper[i]

                            # Add row to table
                            private$.putRow(bootstrapTable, rowKey = i, values = list(
                                nSamples = i,
                                meanSens = meanVal,
                                ciLower = ciLower,
                                ciUpper = ciUpper
                            ))
                        }

                        bootstrapMeansVec <- bootstrapMeans
                        bootstrapCILowerVec <- bootstrapCILower
                        bootstrapCIUpperVec <- bootstrapCIUpper

                        targetCandidates <- which(!is.na(bootstrapMeans) & bootstrapMeans >= targetConf)
                        bootstrapTargetIdx <- if (length(targetCandidates) > 0) targetCandidates[1] else NA_integer_

                        ciTarget <- if (!is.na(bootstrapTargetIdx)) {
                            c(bootstrapCILowerVec[bootstrapTargetIdx],
                              bootstrapCIUpperVec[bootstrapTargetIdx])
                        } else NULL

                        addRecommendation(
                            method = "Bootstrap",
                            probVec = bootstrapMeans,
                            priority = 1,
                            description = jmvcore::.("Empirical resampling of cases"),
                            detail = sprintf("%d iterations", nBoot),
                            ci = ciTarget
                        )
                    } # End else block for bootstrap validation
                }

                # Store data for plotting. Private fields alone are not enough: jamovi
                # decides whether to re-render an image from its state, so the visual
                # options each renderer reads must be part of the state too.
                private$.totalSamplesData <- totalSamplesData
                private$.firstDetectionData <- firstDetectionData
                private$.pEstimate <- pEstimate
                private$.maxSamp <- maxSamp

                curveState <- list(
                    firstDetection = firstDetectionData,
                    pEstimate = pEstimate,
                    maxSamp = maxSamp,
                    targetConfidence = self$options$targetConfidence
                )
                self$results$detectionCurve$setState(curveState)
                self$results$empiricalCumulativePlot$setState(curveState)

                # Calculate observed conditional detection probability (observed-positive detection)
                # Only among positive cases, not population-level
                nPositiveCases <- sum(!is.na(firstDetectionData))

                observedProbVec <- sapply(1:maxSamp, function(n) {
                    if (nPositiveCases == 0) {
                        return(0)
                    }
                    # Count positive cases detected by sample n / total positive cases
                    sum(!is.na(firstDetectionData) & firstDetectionData <= n) / nPositiveCases
                })
                addRecommendation(
                    method = "Empirical",
                    probVec = observedProbVec,
                    priority = 5,
                    description = jmvcore::.("Observed cumulative detection in dataset"),
                    detail = jmvcore::format(jmvcore::.("Conditional probability among {v1} positive cases"), v1 = sprintf("%d", nPositiveCases))
                )

                obsPercents <- observedProbVec * 100
                obs3 <- if (length(obsPercents) >= 3) obsPercents[3] else NA_real_
                obsIndices <- seq_len(min(4, length(obsPercents)))
                obsListHtml <- ""
                if (length(obsIndices) > 0) {
                    obsListHtml <- paste(
                        sprintf("<li>%d sample%s: %.1f%%</li>", obsIndices, ifelse(obsIndices == 1, "", "s"), obsPercents[obsIndices]),
                        collapse = ""
                    )
                }

                # ===== Tumor Burden Analysis =====
                # Modern implementation: Analyzes extent of tumor involvement using
                # sample positivity ratio (SPR) and distribution patterns

                if (self$options$showTumorBurden && !is.null(self$options$positiveCassettes)) {
                    private$.checkpoint()

                    # Get positive samples count data
                    positiveCassettesVar <- self$options$positiveCassettes
                    positiveCassettesData <- private$.sampleCounts(data, positiveCassettesVar, totalSamplesData)

                    # Handle factor/labelled data
                    if (is.factor(positiveCassettesData) || !is.null(attr(positiveCassettesData, "labels"))) {
                        positiveCassettesData <- as.numeric(as.character(positiveCassettesData))
                    }

                    # Filter to valid analyzed cases
                    positiveCassettesData <- positiveCassettesData[validCases]
                    if (!is.null(invalidCases)) {
                        positiveCassettesData <- positiveCassettesData[!invalidCases]
                    }

                    # Store for other analyses
                    private$.positiveCassettesData <- positiveCassettesData
                    self$results$correlationPlot$setState(list(
                        totalSamples = totalSamplesData,
                        positiveCassettes = positiveCassettesData
                    ))

                    # === Explanatory Text ===
                    tumorBurdenText <- self$results$tumorBurdenText

                    html <- jmvcore::format(jmvcore::.("<p><b>Sample positivity ratio (SPR)</b></p><p>SPR is the proportion of examined samples containing tumor, calculated from valid count pairs among eventually detected cases. It describes sample positivity, not lesion volume, independent anatomical foci or detection completeness.</p>"), v1 = private$.buildStyle(private$.styleConstants$font), v2 = private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary), v3 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary), v4 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary), v5 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary), v6 = private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorSecondary))

                    if (self$options$showTumorBurden) {
                        tumorBurdenText$setContent(html)
                    }

                    # === Calculate SPR Statistics ===
                    tumorBurdenInfo <- self$results$tumorBurdenInfo
                    private$.clearTable(tumorBurdenInfo)

                    # Filter to positive cases only
                    positive_cases_idx <- !is.na(firstDetectionData) & !is.na(positiveCassettesData)
                    spr_values <- positiveCassettesData[positive_cases_idx] / totalSamplesData[positive_cases_idx]

                    # Summary statistics
                    n_positive_for_burden <- sum(positive_cases_idx)
                    mean_spr <- mean(spr_values, na.rm = TRUE)
                    median_spr <- median(spr_values, na.rm = TRUE)
                    sd_spr <- sd(spr_values, na.rm = TRUE)
                    min_spr <- min(spr_values, na.rm = TRUE)
                    max_spr <- max(spr_values, na.rm = TRUE)

                    # Overall totals
                    total_positive_samples <- sum(positiveCassettesData[positive_cases_idx], na.rm = TRUE)
                    total_samples_positive_cases <- sum(totalSamplesData[positive_cases_idx], na.rm = TRUE)
                    overall_spr <- total_positive_samples / total_samples_positive_cases

                    # Populate statistics table
                    private$.putRow(tumorBurdenInfo, rowKey = "n_cases", values = list(
                        measure = jmvcore::.("Cases analyzed (with tumor)"),
                        value = sprintf("%d", n_positive_for_burden)
                    ))

                    private$.putRow(tumorBurdenInfo, rowKey = "mean_spr", values = list(
                        measure = jmvcore::.("Mean SPR"),
                        value = sprintf("%.3f (SD: %.3f)", mean_spr, sd_spr)
                    ))

                    private$.putRow(tumorBurdenInfo, rowKey = "median_spr", values = list(
                        measure = jmvcore::.("Median SPR"),
                        value = sprintf("%.3f", median_spr)
                    ))

                    private$.putRow(tumorBurdenInfo, rowKey = "range_spr", values = list(
                        measure = jmvcore::.("SPR range"),
                        value = sprintf("%.3f - %.3f", min_spr, max_spr)
                    ))

                    private$.putRow(tumorBurdenInfo, rowKey = "overall_spr", values = list(
                        measure = jmvcore::.("Overall SPR (pooled)"),
                        value = sprintf("%.3f (%s / %s samples)", overall_spr, format(total_positive_samples, trim = TRUE), format(total_samples_positive_cases, trim = TRUE))
                    ))

                    # === Tumor Distribution Pattern Classification ===
                    cassetteDistribution <- self$results$cassetteDistribution
                    private$.clearTable(cassetteDistribution)

                    # Classify based on number of positive samples
                    # Focal: 1 positive, Limited: 2-3 positive, Moderate: 4-6 positive, Extensive: 7+ positive
                    n_focal <- sum(positiveCassettesData[positive_cases_idx] == 1, na.rm = TRUE)
                    n_limited <- sum(positiveCassettesData[positive_cases_idx] >= 2 &
                        positiveCassettesData[positive_cases_idx] <= 3, na.rm = TRUE)
                    n_moderate <- sum(positiveCassettesData[positive_cases_idx] >= 4 &
                        positiveCassettesData[positive_cases_idx] <= 6, na.rm = TRUE)
                    n_extensive <- sum(positiveCassettesData[positive_cases_idx] >= 7, na.rm = TRUE)

                    # Add to table
                    if (n_focal > 0) {
                        private$.putRow(cassetteDistribution, rowKey = "focal", values = list(
                            pattern = jmvcore::.("Focal (1 positive sample)"),
                            count = n_focal,
                            percent = n_focal / n_positive_for_burden
                        ))
                    }

                    if (n_limited > 0) {
                        private$.putRow(cassetteDistribution, rowKey = "limited", values = list(
                            pattern = "Limited (2-3 positive)",
                            count = n_limited,
                            percent = n_limited / n_positive_for_burden
                        ))
                    }

                    if (n_moderate > 0) {
                        private$.putRow(cassetteDistribution, rowKey = "moderate", values = list(
                            pattern = "Moderate (4-6 positive)",
                            count = n_moderate,
                            percent = n_moderate / n_positive_for_burden
                        ))
                    }

                    if (n_extensive > 0) {
                        private$.putRow(cassetteDistribution, rowKey = "extensive", values = list(
                            pattern = "Extensive (7+ positive)",
                            count = n_extensive,
                            percent = n_extensive / n_positive_for_burden
                        ))
                    }

                    # === Additional Insight ===
                    # Calculate correlation between total samples and positive samples
                    if (n_positive_for_burden >= 3) {
                        cor_result <- tryCatch(
                            {
                                suppressWarnings(cor.test(totalSamplesData[positive_cases_idx],
                                    positiveCassettesData[positive_cases_idx],
                                    method = "spearman"
                                ))
                            },
                            error = function(e) NULL
                        )

                        if (!is.null(cor_result) && !is.na(cor_result$estimate)) {
                            private$.putRow(tumorBurdenInfo, rowKey = "correlation", values = list(
                                measure = jmvcore::.("Correlation (samples examined vs positive)"),
                                value = sprintf("\u{03C1} = %.3f (p %s %.3f)", cor_result$estimate, if (cor_result$p.value < 0.001) "<" else "=", if (cor_result$p.value < 0.001) 0.001 else cor_result$p.value)
                            ))
                        }
                    }
                }

                # ===== Stage Migration Analysis =====
                # Analyzes whether examining fewer samples leads to understaging

                if (self$options$showStageMigration && !is.null(self$options$positiveCassettes)) {
                    private$.checkpoint()

                    # Get positive samples data (reuse if already loaded from tumor burden)
                    if (!is.null(private$.positiveCassettesData)) {
                        # Already loaded in tumor burden section
                        positiveCassettesData <- private$.positiveCassettesData
                    } else {
                        # Load it now
                        positiveCassettesVar <- self$options$positiveCassettes
                        positiveCassettesData <- private$.sampleCounts(data, positiveCassettesVar, totalSamplesData)

                        if (is.factor(positiveCassettesData) || !is.null(attr(positiveCassettesData, "labels"))) {
                            positiveCassettesData <- as.numeric(as.character(positiveCassettesData))
                        }

                        positiveCassettesData <- positiveCassettesData[validCases]
                        if (!is.null(invalidCases)) {
                            positiveCassettesData <- positiveCassettesData[!invalidCases]
                        }
                    }

                    # === Explanatory Text ===
                    stageMigrationText <- self$results$stageMigrationText

                    html <- jmvcore::format(jmvcore::.("<p><b>Observed detection by sampling intensity</b></p><p>This cross-sectional comparison describes positivity in cases with different numbers of examined samples. It does not identify individual understaging, establish causality, or estimate the effect of examining more samples.</p>"), v1 = private$.buildStyle(private$.styleConstants$font), v2 = private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary), v3 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary), v4 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary), v5 = private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorSecondary))

                    if (self$options$showStageMigration) {
                        stageMigrationText$setContent(html)
                    }

                    # Analyze detection rates by cassette groups
                    stageMigrationTable <- self$results$stageMigrationTable
                    private$.clearTable(stageMigrationTable)

                    # Define groups based on data quartiles or standard thresholds
                    # Using standard thresholds: <median, >=median
                    medianCassettes <- median(totalSamplesData)

                    # Group 1: Below median
                    group1 <- totalSamplesData < medianCassettes & !is.na(positiveCassettesData)
                    nGroup1 <- sum(group1)
                    nPosGroup1 <- sum(positiveCassettesData[group1] > 0)
                    rateGroup1 <- if (nGroup1 > 0) nPosGroup1 / nGroup1 else 0

                    # Group 2: At or above median
                    group2 <- totalSamplesData >= medianCassettes & !is.na(positiveCassettesData)
                    nGroup2 <- sum(group2)
                    nPosGroup2 <- sum(positiveCassettesData[group2] > 0)
                    rateGroup2 <- if (nGroup2 > 0) nPosGroup2 / nGroup2 else 0

                    private$.putRow(stageMigrationTable, rowKey = "group_1", values = list(
                        cassettes = sprintf("<%s", format(medianCassettes, trim = TRUE)),
                        nCases = nGroup1,
                        nPositive = nPosGroup1,
                        positivityRate = rateGroup1
                    ))
                    private$.putRow(stageMigrationTable, rowKey = "group_2", values = list(
                        cassettes = sprintf(">=%s", format(medianCassettes, trim = TRUE)),
                        nCases = nGroup2,
                        nPositive = nPosGroup2,
                        positivityRate = rateGroup2
                    ))
                    private$.putRow(stageMigrationTable, rowKey = "group_3", values = list(
                        cassettes = jmvcore::.("Absolute difference"),
                        nCases = NA,
                        nPositive = NA,
                        positivityRate = abs(rateGroup2 - rateGroup1)
                    ))
                }

                # === Correlation Analysis ===
                positiveCassettes <- self$options$positiveCassettes
                if (self$options$showCorrelation && !is.null(positiveCassettes)) {
                    positiveCassettesData <- private$.sampleCounts(data, positiveCassettes, totalSamplesData)

                    if (is.factor(positiveCassettesData) || !is.null(attr(positiveCassettesData, "labels"))) {
                        positiveCassettesData <- as.numeric(as.character(positiveCassettesData))
                    }

                    positiveCassettesData <- positiveCassettesData[validCases]
                    if (!is.null(invalidCases)) {
                        positiveCassettesData <- positiveCassettesData[!invalidCases]
                    }

                    self$results$correlationPlot$setState(list(
                        totalSamples = totalSamplesData, positiveCassettes = positiveCassettesData))
                    correlationText <- self$results$correlationText
                    correlationText$setContent(jmvcore::.("<p><b>Examined versus positive samples</b></p><p>Correlation describes the association between examined and positive cassette counts. It does not establish that more sampling causes better detection.</p>"))

                    correlationStats <- self$results$correlationStats
                    private$.clearTable(correlationStats)

                    # A correlation needs variation in BOTH columns. Under a fixed sampling
                    # protocol every case can have the same number of samples examined, and
                    # cor.test() then returns rho = NA and p = NA -- which used to reach
                    # `if (corTest$p.value < 0.05)` and abort the analysis with "missing value
                    # where TRUE/FALSE needed".
                    corPairs <- stats::complete.cases(totalSamplesData, positiveCassettesData)
                    nCorPairs <- sum(corPairs)
                    corTest <- NULL
                    if (nCorPairs >= 3 &&
                        stats::sd(totalSamplesData[corPairs]) > 0 &&
                        stats::sd(positiveCassettesData[corPairs]) > 0) {
                        corTest <- suppressWarnings(stats::cor.test(
                            totalSamplesData[corPairs], positiveCassettesData[corPairs],
                            method = "spearman"
                        ))
                    }

                    if (is.null(corTest) || is.na(corTest$estimate) || is.na(corTest$p.value)) {
                        reason <- if (nCorPairs < 3) {
                            jmvcore::format(jmvcore::.("only {v1} complete pairs are available (at least 3 are needed)"), v1 = sprintf("%d", nCorPairs))
                        } else {
                            jmvcore::.("one of the two variables takes the same value in every case, so there is no variation to correlate")
                        }
                        private$.addNotice(
                            "WARNING",
                            jmvcore::.("Correlation not computed"),
                            jmvcore::format(jmvcore::.("The examined-versus-positive correlation could not be estimated because {v1}."), v1 = reason)
                        )
                        private$.putRow(correlationStats, rowKey = "r_value", values = list(
                            statistic = "Spearman's rho", value = jmvcore::.("Not estimable")
                        ))
                    } else {
                        private$.putRow(correlationStats, rowKey = "r_value", values = list(
                            statistic = "Spearman's rho",
                            value = sprintf("%.3f", corTest$estimate)
                        ))
                        private$.putRow(correlationStats, rowKey = "p_value", values = list(
                            statistic = "p-value",
                            value = sprintf("%.4f", corTest$p.value)
                        ))
                        private$.putRow(correlationStats, rowKey = "n_cases", values = list(
                            statistic = "Interpretation",
                            value = if (corTest$p.value < 0.05) {
                                if (corTest$estimate > 0) jmvcore::.("Significant positive correlation") else jmvcore::.("Significant negative correlation")
                            } else {
                                jmvcore::.("No significant correlation")
                            }
                        ))
                    }
                }

                # === Distribution Pattern Analysis (Single vs Summed) ===
                totalFoci <- self$options$totalFoci
                maxPositiveSingle <- self$options$maxPositiveSingle

                if (self$options$showDistributionPattern && !is.null(totalFoci) && !is.null(maxPositiveSingle)) {
                    positiveCassettesData <- private$.numericColumn(data, totalFoci)
                    maxPositiveSingleData <- private$.numericColumn(data, maxPositiveSingle)
                    keepFoci <- private$.validCount(positiveCassettesData) &
                        private$.validCount(maxPositiveSingleData) &
                        maxPositiveSingleData <= positiveCassettesData &
                        ((positiveCassettesData == 0 & maxPositiveSingleData == 0) |
                         (positiveCassettesData > 0 & maxPositiveSingleData > 0))
                    if (any(!keepFoci)) private$.addNotice("WARNING", jmvcore::.("Invalid foci counts"),
                        jmvcore::format(jmvcore::.("{v1} cases excluded from foci classification: provide complete integer counts, with maximum foci per slide no greater than total foci and consistent zero counts."), v1 = sprintf("%d", sum(!keepFoci))))
                    positiveCassettesData <- positiveCassettesData[keepFoci]
                    maxPositiveSingleData <- maxPositiveSingleData[keepFoci]
                    nFociCases <- sum(keepFoci)
                    # Get threshold
                    threshold <- self$options$distributionThreshold

                    # Distribution Pattern Text
                    distributionPatternText <- self$results$distributionPatternText
                    html <- jmvcore::format(jmvcore::.("<p><b>Single-slide versus summed foci</b></p><p>Cases are classified by whether the threshold of {v1} foci is met on one slide or {v2} foci is met only by summing slides. Counts must represent foci, not positive cassettes. This descriptive classification does not estimate prognosis.</p>"), v1 = sprintf("%d", threshold), v2 = sprintf("%d", threshold))
                    if (self$options$showDistributionPattern) {
                        distributionPatternText$setContent(html)
                    }

                    # Classify cases
                    # Focal: total < threshold
                    # Substantial-single: max on single slide >= threshold
                    # Substantial-summed: total >= threshold BUT max < threshold

                    focal <- positiveCassettesData < threshold
                    substantialSingle <- maxPositiveSingleData >= threshold
                    substantialSummed <- (positiveCassettesData >= threshold) & (maxPositiveSingleData < threshold)

                    nFocal <- sum(focal, na.rm = TRUE)
                    nSubstantialSingle <- sum(substantialSingle, na.rm = TRUE)
                    nSubstantialSummed <- sum(substantialSummed, na.rm = TRUE)

                    # Distribution Pattern Table
                    distributionPatternTable <- self$results$distributionPatternTable
                    private$.clearTable(distributionPatternTable)

                    private$.putRow(distributionPatternTable, rowKey = "predominant_single", values = list(
                        pattern = sprintf("Focal (<%d total)", threshold),
                        count = nFocal,
                        percent = nFocal / ifelse(nFociCases > 0, nFociCases, NA_real_)
                    ))
                    private$.putRow(distributionPatternTable, rowKey = "summed_effect", values = list(
                        pattern = jmvcore::format(jmvcore::.("Substantial on single slide (>={v1} on >=1 slide)"), v1 = sprintf("%d", threshold)),
                        count = nSubstantialSingle,
                        percent = nSubstantialSingle / ifelse(nFociCases > 0, nFociCases, NA_real_)
                    ))
                    private$.putRow(distributionPatternTable, rowKey = "diffuse", values = list(
                        pattern = jmvcore::format(jmvcore::.("Substantial only when summed (>={v1} total, <{v2} max)"), v1 = sprintf("%d", threshold), v2 = sprintf("%d", threshold)),
                        count = nSubstantialSummed,
                        percent = nSubstantialSummed / ifelse(nFociCases > 0, nFociCases, NA_real_)
                    ))

                    # Comparison statistics
                    distributionComparisonTable <- self$results$distributionComparisonTable
                    private$.clearTable(distributionComparisonTable)

                    # Among substantial cases (total >= threshold)
                    substantialCases <- positiveCassettesData >= threshold
                    nSubstantial <- sum(substantialCases, na.rm = TRUE)

                    if (nSubstantial > 0) {
                        pctSingleAmongSubstantial <- sum(substantialSingle, na.rm = TRUE) / nSubstantial * 100
                        pctSummedAmongSubstantial <- sum(substantialSummed, na.rm = TRUE) / nSubstantial * 100

                        private$.putRow(distributionComparisonTable, rowKey = "mean_single", values = list(
                            measure = jmvcore::format(jmvcore::.("Cases with >={v1} foci (substantial)"), v1 = sprintf("%d", threshold)),
                            value = sprintf("%d (%.1f%%)", nSubstantial, nSubstantial / ifelse(nFociCases > 0, nFociCases, NA_real_) * 100)
                        ))
                        private$.putRow(distributionComparisonTable, rowKey = "max_single", values = list(
                            measure = jmvcore::.("- Met on single slide"),
                            value = sprintf("%d (%.1f%% of substantial)", nSubstantialSingle, pctSingleAmongSubstantial)
                        ))
                        private$.putRow(distributionComparisonTable, rowKey = "mean_summed", values = list(
                            measure = jmvcore::.("- Met only by summing"),
                            value = sprintf("%d (%.1f%% of substantial)", nSubstantialSummed, pctSummedAmongSubstantial)
                        ))

                        # Calculate mean max on single slide for each group
                        meanMaxSingle <- mean(maxPositiveSingleData[substantialSingle], na.rm = TRUE)
                        meanMaxSummed <- mean(maxPositiveSingleData[substantialSummed], na.rm = TRUE)

                        private$.putRow(distributionComparisonTable, rowKey = "predominance_ratio", values = list(
                            measure = jmvcore::.("Mean max foci per slide (single group)"),
                            value = sprintf("%.1f", meanMaxSingle)
                        ))
                        private$.putRow(distributionComparisonTable, rowKey = "detection_yield", values = list(
                            measure = jmvcore::.("Mean max foci per slide (summed group)"),
                            value = sprintf("%.1f", meanMaxSummed)
                        ))

                        # Clinical interpretation
                        private$.putRow(distributionComparisonTable, rowKey = "clinical_relevance", values = list(
                            measure = jmvcore::.("Clinical significance"),
                            value = jmvcore::.("This classification is descriptive; no survival effect is estimated from these data.")
                        ))
                    } else {
                        private$.putRow(distributionComparisonTable, rowKey = "mean_single", values = list(
                            measure = jmvcore::.("No substantial cases"),
                            value = jmvcore::format(jmvcore::.("No cases with >={v1} foci"), v1 = sprintf("%d", threshold))
                        ))
                    }

                    # Store for potential future use
                    private$.maxPositiveSingleData <- maxPositiveSingleData
                }

                # ===== PHASE 2: Empirical Cumulative Detection =====
                if (self$options$showEmpiricalCumulative && nDetected > 0) {
                    private$.checkpoint()

                    empiricalCumulativeText <- self$results$empiricalCumulativeText
                    empiricalCumulativeTable <- self$results$empiricalCumulativeTable
                    private$.clearTable(empiricalCumulativeTable)

                    # Calculate empirical cumulative detection with bootstrap CIs
                    boot_results <- private$.bootstrapEmpiricalCumulative(
                        firstDetectionData, totalSamplesData, maxSamp, nBoot
                    )

                    if (!is.null(boot_results)) {
                        # Context-specific explanatory text
                        analysisContext <- self$options$analysisContext

                        contextNote <- ""
                        if (analysisContext == "tumor") {
                            contextNote <- jmvcore::format(jmvcore::.("<p><b>Tumor sampling:</b> Spatial dependence can invalidate independent-trial predictions. Empirical curves describe the observed first detections; resampling does not correct missed disease or unequal observation windows.</p>"), v1 = private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorPrimary), v2 = private$.buildStyle(private$.styleConstants$fontSize12, private$.styleConstants$colorSecondary))
                        } else if (analysisContext == "margin") {
                            contextNote <- jmvcore::format(jmvcore::.("<p><b>Margin sampling:</b> Nearby samples may be dependent. The observed detection curve does not validate margin clearance or a rule-out protocol.</p>"), v1 = private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorPrimary))
                        }

                        # Populate text
                        html <- jmvcore::format(jmvcore::.("<p><b>Empirical cumulative detection</b></p><p>The curve describes {v5} eventually detected cases, with first-detection positions from {v6} to {v7}. Bootstrap intervals quantify case-resampling variation without imposing a geometric distribution.</p>{v8}"), v1 = private$.buildStyle(private$.styleConstants$font), v2 = private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary), v3 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary), v4 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary), v5 = sprintf("%d", nDetected), v6 = sprintf("%.0f", min(firstDetectionData, na.rm = TRUE)), v7 = sprintf("%.0f", max(firstDetectionData, na.rm = TRUE)), v8 = contextNote)

                        if (self$options$showEmpiricalCumulative) {
                            empiricalCumulativeText$setContent(html)
                        }

                        # Populate table
                        prev_cum <- 0
                        for (n in seq_len(nrow(boot_results))) {
                            incremental <- boot_results$mean[n] - prev_cum
                            private$.putRow(empiricalCumulativeTable, rowKey = paste0("n_", n), values = list(
                                nSamples = n,
                                cumDetection = boot_results$mean[n],
                                ciLower = boot_results$lower[n],
                                ciUpper = boot_results$upper[n],
                                incrementalYield = incremental
                            ))
                            prev_cum <- boot_results$mean[n]
                        }
                    }
                }

                # ===== PHASE 2: Incremental Yield Analysis =====
                if (self$options$showIncrementalYield && nDetected > 0) {
                    private$.checkpoint()

                    incrementalYieldText <- self$results$incrementalYieldText
                    incrementalYieldTable <- self$results$incrementalYieldTable
                    private$.clearTable(incrementalYieldTable)

                    # Calculate incremental yield
                    positive_idx <- !is.na(firstDetectionData)
                    positive_first <- firstDetectionData[positive_idx]

                    if (maxSamp > 1) {
                        for (n in seq_len(maxSamp - 1)) {
                            from_n <- sum(positive_first <= n, na.rm = TRUE) / length(positive_first)
                            to_n <- sum(positive_first <= (n + 1), na.rm = TRUE) / length(positive_first)
                            incremental <- to_n - from_n
                            cases_per_100 <- incremental * 100

                            # Descriptive yield rating
                            if (incremental >= 0.10) {
                                rating <- jmvcore::.("Higher observed yield")
                            } else if (incremental >= 0.05) {
                                rating <- jmvcore::.("Moderate observed yield")
                            } else if (incremental >= 0.02) {
                                rating <- jmvcore::.("Diminishing returns")
                            } else {
                                rating <- jmvcore::.("Low yield")
                            }

                            private$.putRow(incrementalYieldTable, rowKey = paste0("n_", n), values = list(
                                fromSamples = n,
                                toSamples = n + 1,
                                incrementalDetection = incremental,
                                casesDetected = cases_per_100,
                                costBenefit = rating
                            ))
                        }
                    }

                    html <- jmvcore::format(jmvcore::.("<p><b>Incremental observed yield</b></p><p>The table shows the added fraction of eventual detections at each sample position. Bands are descriptive: higher (at least 10%), moderate (5% to below 10%), diminishing (2% to below 5%) and low (below 2%). These bands include no costs or utilities and do not define a clinical stopping rule.</p>"), v1 = private$.buildStyle(private$.styleConstants$font), v2 = private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary), v3 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary), v4 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary))

                    if (self$options$showIncrementalYield) {
                        incrementalYieldText$setContent(html)
                    }
                }

                # ===== PHASE 3: Sample Type Stratification =====
                if (self$options$showStratifiedAnalysis && !is.null(sampleTypeData) && nDetected > 0) {
                    private$.checkpoint()

                    stratifiedText <- self$results$stratifiedText
                    prevalenceTable <- self$results$prevalenceTable
                    private$.clearTable(prevalenceTable)
                    stratifiedDetectionTable <- self$results$stratifiedDetectionTable
                    private$.clearTable(stratifiedDetectionTable)

                    # Get unique sample types
                    unique_types <- levels(droplevels(sampleTypeData))

                    if (length(unique_types) > 0) {
                        html <- jmvcore::format(jmvcore::.("<p><b>Sample-type comparisons</b></p><p>Each group retains its recorded counts and observed-positive fraction. The selected estimator and the case-proportion screening rule are applied separately to each group. Groups without an eligible q retain their descriptive counts but receive no model predictions.</p>"), v1 = private$.buildStyle(private$.styleConstants$font), v2 = private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary), v3 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary), v4 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary))

                        if (self$options$showStratifiedAnalysis) {
                            stratifiedText$setContent(html)
                        }

                        # Calculate for each type
                        type_row <- 1
                        for (type in unique_types) {
                            type_idx <- sampleTypeData == type & !is.na(sampleTypeData)
                            type_positive_idx <- type_idx & !is.na(firstDetectionData)

                            n_type_total <- sum(type_idx, na.rm = TRUE)
                            n_type_positive <- sum(type_positive_idx, na.rm = TRUE)
                            prevalence_type <- n_type_positive / n_type_total

                            groupEstimate <- private$.estimateQ(firstDetectionData[type_idx],
                                totalSamplesData[type_idx],
                                if (is.null(positiveCountData)) NULL else positiveCountData[type_idx])
                            q_type <- groupEstimate$q
                            private$.putRow(prevalenceTable, paste0("type_", type_row), list(
                                sampleType = as.character(type), totalCases = n_type_total,
                                positiveCases = n_type_positive, prevalence = prevalence_type,
                                qEstimate = q_type))
                            if (!is.finite(q_type)) {
                                private$.addNotice("WARNING", jmvcore::.("Subgroup estimate unavailable"),
                                    jmvcore::format(jmvcore::.("Sample type {v1}: q and model predictions are unavailable because no eligible detections/count pairs remain or the within-group case-proportion CV exceeds 0.5. Its observed case counts and positive fraction are retained."), v1 = type))
                            } else {
                                for (n in c(3, 5, 7, 10)) {
                                    if (n <= maxSamp) {
                                        conditional_det <- 1 - (1 - q_type)^n
                                        private$.putRow(stratifiedDetectionTable, paste0(type, "_", n), list(
                                            sampleType = as.character(type), nSamples = n,
                                            conditionalDetection = conditional_det,
                                            populationDetection = prevalence_type * conditional_det))
                                    }
                                }
                            }
                            type_row <- type_row + 1

                        }
                    }
                }

                # ===== PHASE 3: Population-Level Detection Rates =====
                if (self$options$showPopulationDetection && nDetected > 0) {
                    private$.checkpoint()

                    populationDetectionText <- self$results$populationDetectionText
                    populationDetectionTable <- self$results$populationDetectionTable
                    private$.clearTable(populationDetectionTable)

                    # Calculate overall prevalence
                    prevalence <- nDetected / length(firstDetectionData)

                    html <- jmvcore::format(jmvcore::.("<p><b>Recorded-case and conditional detection</b></p><p>The observed-positive fraction is {v6}% ({v7}/{v8} cases). Conditional predictions refer to eventual observation, not independently known disease. Multiplying by the recorded positive fraction gives a modelled fraction of recorded cases, not validated population sensitivity.</p>"), v1 = private$.buildStyle(private$.styleConstants$font), v2 = private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary), v3 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary), v4 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary), v5 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary), v6 = sprintf("%.1f", 100 * prevalence), v7 = sprintf("%d", nDetected), v8 = sprintf("%d", length(firstDetectionData)), v9 = private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorSecondary))

                    if (self$options$showPopulationDetection) {
                        populationDetectionText$setContent(html)
                    }

                    # Populate table
                    for (n in 1:maxSamp) {
                        if (!is.na(pForCalc) && pForCalc > 0) {
                            conditional <- 1 - (1 - pForCalc)^n
                            population <- prevalence * conditional

                            private$.putRow(populationDetectionTable, rowKey = paste0("n_", n), values = list(
                                nSamples = n,
                                prevalence = prevalence,
                                conditional = conditional,
                                population = population
                            ))
                        }
                    }
                }

                # ===== PHASE 4: Spatial Clustering Analysis =====
                if (self$options$showSpatialClustering && !is.null(positiveSamplesListData) && nDetected > 0) {
                    private$.checkpoint()

                    spatialClusteringText <- self$results$spatialClusteringText
                    clusteringTable <- self$results$clusteringTable
                    private$.clearTable(clusteringTable)

                    # Parse sample lists and calculate clustering
                    clustering_indices <- numeric(length(positiveSamplesListData))
                    clustering_indices[] <- NA

                    positive_idx <- !is.na(firstDetectionData)
                    for (i in which(positive_idx)) {
                        samples <- private$.parseSampleList(positiveSamplesListData[i])
                        if (length(samples) > 1) {
                            clustering_indices[i] <- private$.calculateClusteringIndex(
                                samples, totalSamplesData[i]
                            )
                        }
                    }

                    # Categorize patterns
                    clustered_idx <- clustering_indices < 0.7 & !is.na(clustering_indices)
                    random_idx <- clustering_indices >= 0.7 & clustering_indices <= 1.3 & !is.na(clustering_indices)
                    dispersed_idx <- clustering_indices > 1.3 & !is.na(clustering_indices)

                    n_clustered <- sum(clustered_idx, na.rm = TRUE)
                    n_random <- sum(random_idx, na.rm = TRUE)
                    n_dispersed <- sum(dispersed_idx, na.rm = TRUE)
                    n_total <- sum(!is.na(clustering_indices))

                    if (n_total > 0) {
                        html <- jmvcore::format(jmvcore::.("<p><b>Sample-position clustering</b></p><p>The index compares mean gaps with their expectation under uniform random placement. Labels below 0.7, from 0.7 to 1.3, and above 1.3 are descriptive bands, not hypothesis tests. Sampling order need not represent anatomical distance; these groups do not establish focality or a sampling protocol.</p>"), v1 = private$.buildStyle(private$.styleConstants$font), v2 = private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary), v3 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary), v4 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary), v5 = private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorSecondary))

                        if (self$options$showSpatialClustering) {
                            spatialClusteringText$setContent(html)
                        }

                        # Populate table
                        private$.putRow(clusteringTable, rowKey = "clustered", values = list(
                            pattern = jmvcore::.("Below reference gap (index < 0.7)"),
                            count = n_clustered,
                            percent = n_clustered / n_total,
                            meanClusterIndex = if (n_clustered > 0) mean(clustering_indices[clustered_idx], na.rm = TRUE) else NA
                        ))
                        private$.putRow(clusteringTable, rowKey = "random", values = list(
                            pattern = jmvcore::.("Near reference gap (index 0.7-1.3)"),
                            count = n_random,
                            percent = n_random / n_total,
                            meanClusterIndex = if (n_random > 0) mean(clustering_indices[random_idx], na.rm = TRUE) else NA
                        ))
                        private$.putRow(clusteringTable, rowKey = "dispersed", values = list(
                            pattern = jmvcore::.("Above reference gap (index > 1.3)"),
                            count = n_dispersed,
                            percent = n_dispersed / n_total,
                            meanClusterIndex = if (n_dispersed > 0) mean(clustering_indices[dispersed_idx], na.rm = TRUE) else NA
                        ))
                    }
                }

                # ===== PHASE 4: Multifocal Detection Analysis =====
                if (self$options$showMultifocalAnalysis && !is.null(positiveSamplesListData) && nDetected > 0) {
                    private$.checkpoint()

                    multifocalText <- self$results$multifocalText
                    multifocalTable <- self$results$multifocalTable
                    private$.clearTable(multifocalTable)

                    # Parse sample lists and estimate foci
                    foci_counts <- integer(length(positiveSamplesListData))
                    foci_counts[] <- NA

                    positive_idx <- !is.na(firstDetectionData)
                    for (i in which(positive_idx)) {
                        samples <- private$.parseSampleList(positiveSamplesListData[i])
                        if (length(samples) > 0) {
                            foci_counts[i] <- private$.estimateFociCount(samples)
                        }
                    }

                    # Categorize by foci count
                    unifocal_idx <- foci_counts == 1 & !is.na(foci_counts)
                    bifocal_idx <- foci_counts == 2 & !is.na(foci_counts)
                    multifocal_idx <- foci_counts >= 3 & !is.na(foci_counts)

                    n_unifocal <- sum(unifocal_idx, na.rm = TRUE)
                    n_bifocal <- sum(bifocal_idx, na.rm = TRUE)
                    n_multifocal <- sum(multifocal_idx, na.rm = TRUE)
                    n_total <- sum(!is.na(foci_counts))

                    if (n_total > 0) {
                        html <- jmvcore::format(jmvcore::.("<p><b>Heuristic foci count</b></p><p>A gap exceeding the user-selected threshold starts a new sample-position group. These groups are a sampling-order heuristic, not independently established anatomical lesions; they do not determine stage or treatment.</p>"), v1 = private$.buildStyle(private$.styleConstants$font), v2 = private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary), v3 = private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary), v4 = private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorSecondary))

                        if (self$options$showMultifocalAnalysis) {
                            multifocalText$setContent(html)
                        }

                        # Populate table
                        if (n_unifocal > 0) {
                            private$.putRow(multifocalTable, rowKey = "single", values = list(
                                fociCount = jmvcore::.("One sample-position group"),
                                cases = n_unifocal,
                                percent = n_unifocal / n_total,
                                meanFirstDetection = mean(firstDetectionData[unifocal_idx], na.rm = TRUE)
                            ))
                        }
                        if (n_bifocal > 0) {
                            private$.putRow(multifocalTable, rowKey = "low_multi", values = list(
                                fociCount = jmvcore::.("Two sample-position groups"),
                                cases = n_bifocal,
                                percent = n_bifocal / n_total,
                                meanFirstDetection = mean(firstDetectionData[bifocal_idx], na.rm = TRUE)
                            ))
                        }
                        if (n_multifocal > 0) {
                            private$.putRow(multifocalTable, rowKey = "high_multi", values = list(
                                fociCount = jmvcore::.("Three or more sample-position groups"),
                                cases = n_multifocal,
                                percent = n_multifocal / n_total,
                                meanFirstDetection = mean(firstDetectionData[multifocal_idx], na.rm = TRUE)
                            ))
                        }
                    }
                }

                # === Clinical Recommendations ===
                recommendationTable <- NULL
                primaryRecommendation <- NULL
                fallbackRecommendation <- NULL

                if (length(recommendations) > 0) {
                    recommendationTable <- do.call(rbind, lapply(recommendations, function(rec) {
                        data.frame(
                            method = rec$method,
                            description = rec$description,
                            minSamples = ifelse(is.null(rec$minSamples), NA, rec$minSamples),
                            achievedProb = rec$achievedProb,
                            bestProb = rec$bestProb,
                            bestN = rec$bestN,
                            status = rec$status,
                            detail = ifelse(is.null(rec$detail), "", rec$detail),
                            priority = rec$priority,
                            ciLower = rec$ciLower,
                            ciUpper = rec$ciUpper,
                            stringsAsFactors = FALSE
                        )
                    }))

                    recommendationTable$priority <- as.numeric(recommendationTable$priority)
                    recommendationTable$minSamples <- as.numeric(recommendationTable$minSamples)
                    recommendationTable$achievedProb <- as.numeric(recommendationTable$achievedProb)
                    recommendationTable$bestProb <- as.numeric(recommendationTable$bestProb)
                    recommendationTable$bestN <- as.numeric(recommendationTable$bestN)
                    recommendationTable$ciLower <- as.numeric(recommendationTable$ciLower)
                    recommendationTable$ciUpper <- as.numeric(recommendationTable$ciUpper)

                    recommendationTable <- recommendationTable[order(recommendationTable$priority, recommendationTable$method), , drop = FALSE]

                    validPrimary <- recommendationTable[!is.na(recommendationTable$minSamples), , drop = FALSE]
                    if (nrow(validPrimary) > 0) {
                        ordPrimary <- order(validPrimary$priority, validPrimary$minSamples)
                        primaryRecommendation <- validPrimary[ordPrimary[1], , drop = FALSE]
                    }

                    fallbackCandidates <- recommendationTable[!is.na(recommendationTable$bestProb), , drop = FALSE]
                    if (nrow(fallbackCandidates) > 0) {
                        ordFallback <- order(-fallbackCandidates$bestProb, fallbackCandidates$priority, fallbackCandidates$method)
                        fallbackRecommendation <- fallbackCandidates[ordFallback[1], , drop = FALSE]
                    }
                }

                recommendText <- self$results$recommendText

                # Get analysis context for tailored recommendations
                analysisContext <- self$options$analysisContext

                if (!is.null(primaryRecommendation) && nrow(primaryRecommendation) == 1) {
                    rec <- primaryRecommendation
                    detailSuffix <- if (!is.na(rec$detail) && nzchar(rec$detail)) sprintf(" (%s)", rec$detail) else ""

                    # Context-specific recommendation text
                    contextHeader <- switch(analysisContext,
                        "tumor" = jmvcore::.("Tumor Sampling Recommendations"),
                        "lymphnode" = jmvcore::.("Lymph Node Dissection Recommendations"),
                        "omentum" = jmvcore::.("Omentum Sampling Recommendations"),
                        "margin" = jmvcore::.("Margin Sampling Recommendations"),
                        jmvcore::.("Clinical Recommendations") # default for general
                    )

                    contextExample <- switch(analysisContext,
                        "tumor" = jmvcore::format(jmvcore::.("<p><b>Example interpretation:</b> The selected descriptive target is reached at {v1} samples under the model assumptions. This is not a validated tumor-sampling minimum.</p>"), v1 = sprintf("%d", rec$minSamples)),
                        "lymphnode" = jmvcore::format(jmvcore::.("<p><b>Example interpretation:</b> The selected descriptive target is reached at {v1} nodes under the model assumptions. This does not establish adequate dissection for an individual patient.</p>"), v1 = sprintf("%d", rec$minSamples)),
                        "" # default: no example
                    )

                    html <- jmvcore::format(jmvcore::.("<h4>{v1}</h4><p>The descriptive {v2}% target is reached at {v3} samples by the {v4} model{v5}, with an estimated observed-positive detection of {v6}% using {v7}.</p><p>This does not establish a patient-level sampling minimum.</p>{v8}"), v1 = contextHeader, v2 = sprintf("%.0f", targetConf * 100), v3 = sprintf("%d", rec$minSamples), v4 = rec$method, v5 = detailSuffix, v6 = sprintf("%.1f", rec$achievedProb * 100), v7 = rec$description, v8 = contextExample)
                    if (nzchar(obsListHtml)) {
                        html <- paste0(
                            html,
                            jmvcore::.("<p><b>Observed cumulative detection:</b></p><ul>"),
                            obsListHtml,
                            "</ul>"
                        )
                    }
                } else if (!is.null(fallbackRecommendation) && nrow(fallbackRecommendation) == 1) {
                    rec <- fallbackRecommendation
                    detailSuffix <- if (!is.na(rec$detail) && nzchar(rec$detail)) sprintf(" (%s)", rec$detail) else ""
                    bestProb <- rec$bestProb * 100
                    bestSamples <- rec$bestN
                    html <- jmvcore::format(jmvcore::.("<p><b>Descriptive target not reached</b></p><p>The {v1}% target was not reached in the evaluated range. The {v2} model{v3} reached {v4}% at {v5} samples. This does not establish clinical sampling adequacy.</p>"), v1 = sprintf("%.0f", targetConf * 100), v2 = rec$method, v3 = detailSuffix, v4 = sprintf("%.1f", bestProb), v5 = sprintf("%d", bestSamples))
                    if (nzchar(obsListHtml)) {
                        html <- paste0(
                            html,
                            jmvcore::.("<p><b>Observed cumulative detection:</b></p><ul>"),
                            obsListHtml,
                            "</ul>"
                        )
                    }
                } else {
                    html <- jmvcore::.("<p>No sufficient information to produce clinical recommendations.</p>")
                }

                if (self$options$showRecommendText) {
                    recommendText$setContent(html)
                }

                keyResults <- self$results$keyResults

                targetLine <- jmvcore::format(jmvcore::.("<p style='{v1} {v2}'><b>Target detection probability:</b> {v3}%</p>"), v1 = private$.styleConstants$fontSize14, v2 = private$.styleConstants$colorPrimary, v3 = sprintf("%.0f", targetConf * 100))

                if (!is.null(primaryRecommendation) && nrow(primaryRecommendation) == 1) {
                    rec <- primaryRecommendation
                    detailSuffix <- if (!is.na(rec$detail) && nzchar(rec$detail)) sprintf(" (%s)", rec$detail) else ""
                    primaryLine <- jmvcore::format(jmvcore::.("<p><b>Descriptive target sample count:</b> {v3} ({v4}), with {v5}% observed-positive detection{v6}.</p>"), v1 = private$.styleConstants$fontSize15, v2 = private$.styleConstants$colorPrimary, v3 = sprintf("%d", rec$minSamples), v4 = rec$method, v5 = sprintf("%.1f", rec$achievedProb * 100), v6 = detailSuffix)
                } else if (!is.null(fallbackRecommendation) && nrow(fallbackRecommendation) == 1) {
                    rec <- fallbackRecommendation
                    detailSuffix <- if (!is.na(rec$detail) && nzchar(rec$detail)) sprintf(" (%s)", rec$detail) else ""
                    primaryLine <- jmvcore::format(jmvcore::.("<p><b>Target not reached:</b> Best modelled value is {v3}% at {v4} samples using {v5}{v6}.</p>"), v1 = private$.styleConstants$fontSize15, v2 = private$.styleConstants$colorPrimary, v3 = sprintf("%.1f", rec$bestProb * 100), v4 = sprintf("%d", rec$bestN), v5 = rec$method, v6 = detailSuffix)
                } else {
                    primaryLine <- jmvcore::format(jmvcore::.("<p>No eligible model could estimate the descriptive sampling target.</p>"), v1 = private$.styleConstants$fontSize15, v2 = private$.styleConstants$colorPrimary)
                }

                methodListHtml <- ""
                if (!is.null(recommendationTable) && nrow(recommendationTable) > 0) {
                    methodItems <- character(nrow(recommendationTable))
                    for (i in seq_len(nrow(recommendationTable))) {
                        rec <- recommendationTable[i, ]
                        detailSuffix <- if (!is.na(rec$detail) && nzchar(rec$detail)) sprintf(" (%s)", rec$detail) else ""
                        methodItems[i] <- sprintf("<li><b>%s:</b> %s%s</li>", rec$method, rec$status, detailSuffix)
                    }
                    methodListHtml <- paste(methodItems, collapse = "")
                }

                comparisonSection <- if (nzchar(methodListHtml)) {
                    jmvcore::format(jmvcore::.("<p><b>Model comparison</b></p><ul>{v5}</ul>"), v1 = private$.styleConstants$fontSize14, v2 = private$.styleConstants$colorPrimary, v3 = private$.styleConstants$fontSize14, v4 = private$.styleConstants$colorPrimary, v5 = methodListHtml)
                } else {
                    ""
                }

                keyResultsHtml <- sprintf("<div style='%s %s %s %s %s'>%s%s%s</div>", private$.styleConstants$font, private$.styleConstants$bgLight, private$.styleConstants$borderPrimary, private$.styleConstants$padding15, private$.styleConstants$margin10, targetLine, primaryLine, comparisonSection)

                if (self$options$showKeyResults) {
                    keyResults$setContent(keyResultsHtml)
                }

                if (self$options$showClinicalSummary) {
                    summary <- jmvcore::format(jmvcore::.("<p>Of {v1} analyzed cases, {v2} recorded a lesion. The analysis describes the position at which these observed lesions were first detected.</p>"), v1 = sprintf("%d", nCases), v2 = sprintf("%d", nDetected))
                    if (!is.na(bootstrapTargetIdx)) {
                        summary <- paste0(summary, jmvcore::format(jmvcore::.("<p>The observed-positive bootstrap mean reaches the {v1}% target at {v2} samples (95% percentile interval at that position: {v3}%-{v4}%).</p>"), v1 = sprintf("%.0f", 100 * targetConf), v2 = sprintf("%d", bootstrapTargetIdx), v3 = sprintf("%.1f", 100 * bootstrapCILowerVec[bootstrapTargetIdx]), v4 = sprintf("%.1f", 100 * bootstrapCIUpperVec[bootstrapTargetIdx])))
                    }
                    summary <- paste0(summary, jmvcore::.("<p>This is a descriptive sampling analysis, not a validated patient-level recommendation. Assess eventual-detection ascertainment, missed disease, unequal observation windows and dependence before applying results to a protocol. Independent probability models and node summaries have separate assumptions and denominators.</p>"))
                    self$results$clinicalSummary$setContent(summary)
                }

                # === Statistical Interpretation ===
                interpretText <- self$results$interpretText

                html <- jmvcore::.("<p><b>Interpretation and limitations</b></p><p>These results describe recorded detections. A missing first-detection position records no observed lesion; this does not establish a confirmed disease-negative case.</p><p>Geometric/binomial models assume independent trials with constant probability. Finite-population models require known counts. Bootstrap intervals describe case-resampling variation and assume independent, representative cases.</p><p>Missed disease, dependence and unequal observation windows can bias interpretation. These results do not establish true sensitivity or a clinical rule-out guarantee.</p>")

                if (self$options$showInterpretText) {
                    interpretText$setContent(html)
                }

                # === Omentum-Specific Analysis ===
                if (self$options$showOmentumAnalysis) {
                    private$.populateOmentumAnalysis()
                }

                # === References ===
                referencesText <- self$results$referencesText

                html <- jmvcore::.("<p><b>Methods and references</b></p><p>Methods: geometric and binomial approximations; hypergeometric sampling without replacement; beta-binomial mixtures; case bootstrap percentile intervals; descriptive node counts with Wilson intervals; sample-position heuristics.</p><p>References: R statistical distribution documentation; VGAM package documentation; Skala and Hagemann, Int J Gynecol Pathol. 2015;34:374-378; Tomlinson et al., Arch Surg. 2007;142:767-774; Yoon et al., doi:10.1245/s10434-025-18029-7.</p><p>Literature citations are contextual; this analysis does not validate a disease-specific sampling protocol.</p>")

                if (self$options$showReferencesText) {
                    referencesText$setContent(html)
                }

                # === Analysis summary ===
                private$.addNotice(
                    "INFO",
                    jmvcore::.("Analysis complete"),
                    jmvcore::format(jmvcore::.("Analysed {v1} of {v2} supplied cases; {v3} recorded a detected lesion. {v4}"), v1 = sprintf("%d", nCases), v2 = sprintf("%d", totalCasesInput), v3 = sprintf("%d", nDetected), v4 = if (identical(estimationMethod, jmvcore::.("Not calculated"))) ""
                            else jmvcore::format(jmvcore::.("Per-sample detection probability estimated by {v1}."), v1 = estimationMethod))
                )
            },
            .detectionCurve = function(image, ggtheme, theme, ...) {
                state <- image$state
                if (is.null(state) || is.null(state$firstDetection)) {
                    return(FALSE)
                }

                firstDetectionData <- state$firstDetection
                pEstimate <- state$pEstimate
                maxSamp <- state$maxSamp
                targetConfidence <- state$targetConfidence
                nCases <- length(firstDetectionData)
                nPositiveCases <- sum(!is.na(firstDetectionData))

                # Calculate observed conditional probability (observed-positive detection)
                # Only among positive cases, not population-level
                nSamples <- 1:maxSamp
                observedProb <- sapply(nSamples, function(n) {
                    if (nPositiveCases == 0) {
                        return(0)
                    }
                    sum(!is.na(firstDetectionData) & firstDetectionData <= n) / nPositiveCases
                })
                if (is.na(pEstimate)) {
                    predictedProb <- rep(NA_real_, length(nSamples))
                } else {
                    predictedProb <- 1 - (1 - pEstimate)^nSamples
                }

                # Create data frame for plotting
                plotData <- data.frame(
                    nSamples = rep(nSamples, 2),
                    Probability = c(observedProb, predictedProb),
                    Type = rep(c("Observed", "Binomial Model"), each = maxSamp)
                )

                # Create plot
                p <- ggplot2::ggplot(plotData, ggplot2::aes(
                    x = nSamples, y = Probability,
                    color = Type, linetype = Type
                )) +
                    ggplot2::geom_line(linewidth = 1.2) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::geom_hline(
                        yintercept = targetConfidence,
                        linetype = "dashed", color = "red", alpha = 0.5
                    ) +
                    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                    ggplot2::scale_x_continuous(breaks = nSamples) +
                    ggplot2::labs(
                        title = jmvcore::.("Diagnostic Yield Curve"),
                        subtitle = jmvcore::format(jmvcore::.("Target observed-positive detection: {v1}% (red line)"), v1 = sprintf("%.0f", targetConfidence * 100)),
                        x = jmvcore::.("Number of Samples"),
                        y = jmvcore::.("Cumulative Detection Probability"),
                        color = "Method",
                        linetype = "Method"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        legend.position = "bottom",
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(size = 11)
                    )

                print(p)
                TRUE
            },
            .sensitivityPlot = function(image, ggtheme, theme, ...) {
                state <- image$state
                if (is.null(state) || is.null(state$bootstrapResults)) {
                    return(FALSE)
                }

                bootstrapResults <- state$bootstrapResults
                maxSamp <- state$maxSamp

                # Calculate statistics
                nSamples <- 1:maxSamp
                meanSens <- colMeans(bootstrapResults)
                ciLower <- apply(bootstrapResults, 2, quantile, 0.025)
                ciUpper <- apply(bootstrapResults, 2, quantile, 0.975)

                plotData <- data.frame(
                    nSamples = nSamples,
                    meanSens = meanSens,
                    ciLower = ciLower,
                    ciUpper = ciUpper
                )

                # Create plot
                p <- ggplot2::ggplot(plotData, ggplot2::aes(x = nSamples, y = meanSens)) +
                    ggplot2::geom_ribbon(ggplot2::aes(ymin = ciLower, ymax = ciUpper),
                        alpha = 0.3, fill = "steelblue"
                    ) +
                    ggplot2::geom_line(color = "steelblue", linewidth = 1.2) +
                    ggplot2::geom_point(color = "steelblue", size = 3) +
                    ggplot2::geom_hline(
                        yintercept = state$targetConfidence,
                        linetype = "dashed", color = "red", alpha = 0.5
                    ) +
                    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                    ggplot2::scale_x_continuous(breaks = nSamples) +
                    ggplot2::labs(
                        title = jmvcore::.("Bootstrap Observed-positive detection Estimates with 95% Confidence Intervals"),
                        subtitle = jmvcore::format(jmvcore::.("Based on {v1} bootstrap iterations"), v1 = sprintf("%d", state$bootstrapIterations)),
                        x = jmvcore::.("Number of Samples"),
                        y = jmvcore::.("Observed-positive detection (Detection Probability)")
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(size = 11)
                    )

                print(p)
                TRUE
            },
            .empiricalCumulativePlot = function(image, ggtheme, theme, ...) {
                state <- image$state
                if (is.null(state) || is.null(state$firstDetection)) {
                    return(FALSE)
                }

                firstDetectionData <- state$firstDetection
                pEstimate <- state$pEstimate
                maxSamp <- state$maxSamp
                nPositiveCases <- sum(!is.na(firstDetectionData))

                if (nPositiveCases == 0) {
                    return(FALSE)
                }

                # Calculate empirical cumulative detection (conditional - positive cases only)
                nSamples <- 1:maxSamp
                empirical_prob <- sapply(nSamples, function(n) {
                    sum(!is.na(firstDetectionData) & firstDetectionData <= n) / nPositiveCases
                })

                # Calculate binomial model for comparison
                if (!is.na(pEstimate) && pEstimate > 0) {
                    model_prob <- 1 - (1 - pEstimate)^nSamples
                } else {
                    model_prob <- rep(NA_real_, length(nSamples))
                }

                # Create data frame for plotting
                plot_data <- data.frame(
                    samples = rep(nSamples, 2),
                    probability = c(empirical_prob, model_prob),
                    method = rep(c("Empirical (Observed)", "Binomial Model"), each = length(nSamples))
                )

                # Remove NAs for model if pEstimate not available
                plot_data <- plot_data[!is.na(plot_data$probability), ]

                # Create plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(
                    x = samples, y = probability,
                    color = method, linetype = method
                )) +
                    ggplot2::geom_line(linewidth = 1.2) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::geom_hline(
                        yintercept = state$targetConfidence,
                        linetype = "dashed", color = "red", alpha = 0.5
                    ) +
                    ggplot2::scale_y_continuous(
                        labels = scales::percent_format(),
                        limits = c(0, 1),
                        breaks = seq(0, 1, 0.2)
                    ) +
                    ggplot2::scale_x_continuous(breaks = nSamples) +
                    ggplot2::scale_color_manual(values = c(
                        "Empirical (Observed)" = "darkorange",
                        "Binomial Model" = "steelblue"
                    )) +
                    ggplot2::scale_linetype_manual(values = c(
                        "Empirical (Observed)" = "solid",
                        "Binomial Model" = "dashed"
                    )) +
                    ggplot2::labs(
                        title = jmvcore::.("Empirical Cumulative Detection vs Binomial Model"),
                        subtitle = jmvcore::format(jmvcore::.("Conditional probability (observed-positive detection) | {v1} positive cases | q = {v2}"), v1 = sprintf("%d", nPositiveCases), v2 = sprintf("%.3f", ifelse(is.na(pEstimate), 0, pEstimate))),
                        x = jmvcore::.("Number of Samples Examined"),
                        y = jmvcore::.("Cumulative Detection Probability"),
                        color = "Method",
                        linetype = "Method"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        legend.position = "bottom",
                        plot.title = ggplot2::element_text(face = "bold", size = 14),
                        plot.subtitle = ggplot2::element_text(size = 11)
                    )

                print(p)
                TRUE
            },
            .correlationPlot = function(image, ggtheme, theme, ...) {
                state <- image$state
                if (is.null(state) || is.null(state$totalSamples) || is.null(state$positiveCassettes)) {
                    return(FALSE)
                }

                totalSamplesData <- state$totalSamples
                positiveCassettesData <- state$positiveCassettes

                # Create data frame
                plotData <- data.frame(
                    total = totalSamplesData,
                    positive = positiveCassettesData
                )

                # Calculate correlation
                # Same zero-variance trap as the correlation table: a constant column gives
                # rho = NA, and the subtitle's "%.3f" would then print "NA".
                corTest <- suppressWarnings(stats::cor.test(
                    totalSamplesData, positiveCassettesData, method = "spearman"
                ))

                # Create scatter plot with regression line
                p <- ggplot2::ggplot(plotData, ggplot2::aes(x = total, y = positive)) +
                    ggplot2::geom_point(alpha = 0.6, size = 3, color = "steelblue") +
                    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "darkred", fill = "pink", alpha = 0.2) +
                    ggplot2::labs(
                        title = jmvcore::.("Correlation: Total Cassettes Examined vs Positive Cassettes"),
                        subtitle = if (is.na(corTest$estimate) || is.na(corTest$p.value)) {
                            jmvcore::.("Spearman's rho not estimable (no variation in one of the two variables)")
                        } else {
                            sprintf("Spearman's rho = %.3f, p = %.4f", corTest$estimate, corTest$p.value)
                        },
                        x = jmvcore::.("Total Cassettes Examined"),
                        y = jmvcore::.("Number of Positive Cassettes")
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(size = 11)
                    )

                print(p)
                TRUE
            },
            .populateOmentumAnalysis = function() {
                self$results$omentumText$setContent(
                    jmvcore::.("<p><b>Omentum literature</b></p><p>Relevant studies include Maglalang and Fadare (2025; doi:10.1093/ajcp/aqaf082), Malpica et al. (2019; ISGyP recommendations), and Skala and Hagemann (2015; Int J Gynecol Pathol. 34:374-378). Review each study's population and design before applying its findings.</p><p>No patient-level minimum, false-negative rate or independent disease status is established here. These observations do not justify reducing sampling below a validated protocol.</p>"))
            },

            # ===== Helper Functions for Enhanced Analyses =====

            # Parse comma-separated sample list
            .parseSampleList = function(sampleListString) {
                if (is.null(sampleListString) || length(sampleListString) == 0 ||
                    is.na(sampleListString) || !nzchar(trimws(as.character(sampleListString))))
                    return(integer())
                tokens <- trimws(strsplit(as.character(sampleListString), ",", fixed = TRUE)[[1]])
                if (!all(grepl("^[0-9]+$", tokens))) return(NA_integer_)
                samples <- suppressWarnings(as.numeric(tokens))
                if (!all(private$.validCount(samples, positive = TRUE)) ||
                    anyDuplicated(samples) || any(samples > .Machine$integer.max)) return(NA_integer_)
                sort(as.integer(samples))
            },

            # Calculate clustering index for a list of positive samples
            .calculateClusteringIndex = function(positiveSamples, totalSamples) {
                if (length(positiveSamples) <= 1) {
                    return(NA_real_)
                }

                sorted_samples <- sort(positiveSamples)
                distances <- diff(sorted_samples)
                mean_distance <- mean(distances)

                # Expected mean gap between consecutive positives when k positions are drawn
                # uniformly at random from 1..N. The k positions cut the sequence into k + 1
                # spacings, each with expectation (N + 1)/(k + 1); the internal gaps that
                # diff() returns share that expectation.
                #
                # The previous denominator was N/k, which is too large and made the index sit
                # BELOW 1 under random placement -- systematically so as k falls. With k = 2
                # positives (an everyday finding) the index averaged 0.67, and the module
                # calls anything below 0.7 "clustered", so two randomly scattered positive
                # blocks were reported as focal disease about half the time.
                k <- length(positiveSamples)
                expected_distance <- (totalSamples + 1) / (k + 1)

                if (!is.finite(expected_distance) || expected_distance <= 0) {
                    return(NA_real_)
                }

                clustering_index <- mean_distance / expected_distance
                return(clustering_index)
            },

            # Estimate number of foci from sample positions
            .estimateFociCount = function(positiveSamples) {
                if (!length(positiveSamples)) return(0L)
                if (anyNA(positiveSamples)) return(NA_integer_)
                if (length(positiveSamples) == 1) return(1L)

                sorted_samples <- sort(positiveSamples)
                distances <- diff(sorted_samples)

                # A gap wider than fociGapThreshold starts a new focus. This was a bare
                # literal 2 with no citation; it is a sampling-interval heuristic, so it
                # belongs under the user's control rather than hard-coded.
                gapThreshold <- self$options$fociGapThreshold
                if (is.null(gapThreshold) || !is.finite(gapThreshold)) {
                    gapThreshold <- 2
                }

                gaps <- sum(distances > gapThreshold)
                foci <- gaps + 1

                return(foci)
            },

            .bootstrapCache = NULL,
            .bootstrapCDF = function(first, maxN, nBoot) {
                first <- first[!is.na(first)]
                if (!length(first)) return(NULL)
                key <- list(first = first, maxN = maxN, nBoot = nBoot)
                if (identical(private$.bootstrapCache$key, key))
                    return(private$.bootstrapCache$values)
                values <- matrix(0, nrow = nBoot, ncol = maxN)
                for (b in seq_len(nBoot)) {
                    if (b %% 100L == 0L) private$.checkpoint()
                    sampled <- first[sample.int(length(first), length(first), replace = TRUE)]
                    counts <- tabulate(pmin(sampled, maxN + 1L), nbins = maxN + 1L)
                    values[b, ] <- cumsum(counts)[seq_len(maxN)] / length(first)
                }
                private$.bootstrapCache <- list(key = key, values = values)
                values
            },

            # Bootstrap empirical cumulative detection
            .bootstrapEmpiricalCumulative = function(firstDetectionData, totalSamplesData,
                                                     maxN, nBoot = 10000) {
                detection_matrix <- private$.bootstrapCDF(firstDetectionData, maxN, nBoot)
                if (is.null(detection_matrix)) return(NULL)

                # Calculate percentiles
                ci_lower <- apply(detection_matrix, 2, quantile, probs = 0.025, na.rm = TRUE)
                ci_upper <- apply(detection_matrix, 2, quantile, probs = 0.975, na.rm = TRUE)
                ci_mean <- apply(detection_matrix, 2, mean, na.rm = TRUE)

                return(data.frame(
                    n = 1:maxN,
                    mean = ci_mean,
                    lower = ci_lower,
                    upper = ci_upper
                ))
            },

            # Private storage for plotting data

            # ==============================================================================
            # 1. HETEROGENEITY TESTING
            # ==============================================================================

            .testHeterogeneity = function(first_detection, groups) {
                # Remove NAs
                valid_idx <- !is.na(first_detection) & !is.na(groups)
                first_detection <- first_detection[valid_idx]
                groups <- groups[valid_idx]

                if (length(first_detection) < 10) {
                    return(list(
                        statistic = NA,
                        df = NA,
                        pValue = NA,
                        interpretation = jmvcore::.("Insufficient data (n < 10)")
                    ))
                }

                if (length(unique(groups)) < 2) return(list(statistic = NA_real_,
                    df = NA_real_, pValue = NA_real_, interpretation = jmvcore::.("At least two sample types are required.")))
                # Calculate pooled q (H0: null model)
                mean_first_pooled <- mean(first_detection, na.rm = TRUE)
                q_pooled <- 1 / mean_first_pooled

                # Log-likelihood for pooled model (geometric distribution)
                ll_null <- sum(dgeom(first_detection - 1, q_pooled, log = TRUE))

                # Calculate group-specific q (H1: alternative model)
                ll_alt <- 0
                unique_groups <- unique(as.character(groups))
                n_groups <- length(unique_groups)

                for (group in unique_groups) {
                    group_data <- first_detection[groups == group]
                    if (length(group_data) > 0) {
                        mean_first_group <- mean(group_data, na.rm = TRUE)
                        q_group <- 1 / mean_first_group
                        ll_alt <- ll_alt + sum(dgeom(group_data - 1, q_group, log = TRUE))
                    }
                }

                # Likelihood ratio statistic
                lr_stat <- 2 * (ll_alt - ll_null)
                df <- n_groups - 1
                p_value <- pchisq(lr_stat, df, lower.tail = FALSE)

                # Interpretation
                if (p_value < 0.001) {
                    interp <- jmvcore::.("Strong evidence of heterogeneity (p < 0.001)")
                } else if (p_value < 0.01) {
                    interp <- jmvcore::.("Significant heterogeneity detected (p < 0.01)")
                } else if (p_value < 0.05) {
                    interp <- jmvcore::.("Moderate heterogeneity detected (p < 0.05)")
                } else {
                    interp <- jmvcore::.("No evidence of heterogeneity at the 0.05 level; this does not establish homogeneity.")
                }

                list(
                    statistic = lr_stat,
                    df = df,
                    pValue = p_value,
                    interpretation = interp
                )
            },

            # ==============================================================================
            # 2. GEOMETRIC CI CALCULATION
            # ==============================================================================


            # ==============================================================================
            # 3. MODEL FIT ASSESSMENT
            # ==============================================================================

            .testModelFit = function(first_detection, q_estimate) {
                # Remove NAs
                first_detection <- first_detection[!is.na(first_detection)]

                if (length(first_detection) < 10) {
                    return(list(
                        chiSquare = NA,
                        df = NA,
                        pValue = NA,
                        fitQuality = jmvcore::.("Insufficient data")
                    ))
                }

                # Bins must PARTITION the whole support of the geometric distribution,
                # otherwise sum(expected) != sum(observed) and the statistic is inflated.
                # The previous version binned only the OBSERVED values, silently discarding
                # the mass above the largest one; on data drawn from exactly this model that
                # rejected the fit in up to 86% of samples (q = 0.10, n = 30) against a
                # nominal 5%. Bins are therefore 1, 2, ..., k plus an explicit ">= k+1" bin
                # carrying P(X > k) = (1 - q)^k, which makes the expected counts sum to n.
                n_total <- length(first_detection)
                k_max <- min(max(first_detection), 1000L)
                observed <- tabulate(pmin(first_detection, k_max + 1L), nbins = k_max + 1L)
                expected <- c(n_total * stats::dgeom(seq_len(k_max) - 1, q_estimate),
                              n_total * (1 - q_estimate)^k_max)

                # Pool from the right until every expected count reaches 5, so the tail mass
                # is folded into the last bin rather than dropped.
                while (length(expected) > 2 && expected[length(expected)] < 5) {
                    m <- length(expected)
                    expected <- c(expected[seq_len(m - 2)], sum(expected[(m - 1):m]))
                    observed <- c(observed[seq_len(m - 2)], sum(observed[(m - 1):m]))
                }
                # Any remaining sparse interior bins are pooled into the tail as well.
                while (length(expected) > 2 && any(expected < 5)) {
                    m <- length(expected)
                    expected <- c(expected[seq_len(m - 2)], sum(expected[(m - 1):m]))
                    observed <- c(observed[seq_len(m - 2)], sum(observed[(m - 1):m]))
                }

                # Chi-square goodness of fit
                chi_sq <- sum((observed - expected)^2 / expected)
                df <- length(observed) - 1 - 1 # -1 for the total, -1 for the estimated q

                if (df < 1) {
                    # Not enough distinct bins to test anything. Reporting a fabricated
                    # df = 1 here produced a fit verdict from a distribution that does
                    # not apply; say so instead.
                    return(list(
                        chiSquare = NA,
                        df = NA,
                        pValue = NA,
                        fitQuality = jmvcore::.("Not estimable (too few distinct detection positions)")
                    ))
                }

                p_value <- stats::pchisq(chi_sq, df, lower.tail = FALSE)

                # Fit quality assessment
                if (p_value >= 0.10) {
                    fit_quality <- jmvcore::.("No detected lack of fit (p >= 0.10); this does not establish model validity")
                } else if (p_value >= 0.05) {
                    fit_quality <- jmvcore::.("No detected lack of fit (p >= 0.05); this does not establish model validity")
                } else if (p_value >= 0.01) {
                    fit_quality <- jmvcore::.("Marginal fit (p < 0.05)")
                } else {
                    fit_quality <- jmvcore::.("Poor fit (p < 0.01)")
                }

                list(
                    chiSquare = chi_sq,
                    df = df,
                    pValue = p_value,
                    fitQuality = fit_quality
                )
            },

            # ==============================================================================
            # 4. OBSERVED VS PREDICTED COMPARISON
            # ==============================================================================

            .calculateObsPred = function(first_detection, q_estimate, max_samples) {
                # Remove NAs
                valid_data <- first_detection[!is.na(first_detection)]
                n_positive <- length(valid_data)

                if (n_positive == 0) {
                    return(data.frame(
                        nSamples = integer(0),
                        observed = numeric(0),
                        predicted = numeric(0),
                        difference = numeric(0),
                        assessment = character(0)
                    ))
                }

                results <- data.frame(
                    nSamples = 1:max_samples,
                    observed = NA_real_,
                    predicted = NA_real_,
                    difference = NA_real_,
                    assessment = NA_character_,
                    stringsAsFactors = FALSE
                )

                for (i in 1:max_samples) {
                    # Observed cumulative detection
                    n_detected_by_i <- sum(valid_data <= i)
                    obs <- n_detected_by_i / n_positive

                    # Predicted (geometric model)
                    pred <- 1 - (1 - q_estimate)^i

                    # Difference
                    diff <- obs - pred

                    # Assessment
                    abs_diff <- abs(diff)
                    if (abs_diff < 0.05) {
                        assess <- jmvcore::.("Excellent fit")
                    } else if (abs_diff < 0.10) {
                        assess <- jmvcore::.("Good fit")
                    } else if (abs_diff < 0.15) {
                        assess <- jmvcore::.("Fair fit")
                    } else {
                        assess <- jmvcore::.("Poor fit")
                    }

                    results[i, ] <- list(i, obs, pred, diff, assess)
                }

                results
            },


            # ==============================================================================
            # 6. AUTO-DETECT HETEROGENEITY (COMPOSITION ANALYSIS)
            # ==============================================================================

            .autoDetectHeterogeneity = function(first_detection, groups) {
                # Remove NAs
                valid_idx <- !is.na(first_detection) & !is.na(groups)
                first_detection <- first_detection[valid_idx]
                groups <- groups[valid_idx]

                if (length(first_detection) < 10) {
                    return(list(
                        warning = FALSE,
                        message = jmvcore::.("Insufficient data for heterogeneity assessment (n < 10)")
                    ))
                }

                # Calculate q for each group
                unique_groups <- unique(as.character(groups))
                group_stats <- list()
                q_values <- numeric(length(unique_groups))

                for (i in seq_along(unique_groups)) {
                    group <- unique_groups[i]
                    group_data <- first_detection[groups == group]
                    n_group <- length(group_data)

                    if (n_group >= 3) {
                        mean_first <- mean(group_data)
                        q_group <- 1 / mean_first
                        q_values[i] <- q_group

                        group_stats[[as.character(group)]] <- list(
                            n = n_group,
                            q = q_group,
                            mean_first = mean_first
                        )
                    } else {
                        q_values[i] <- NA
                    }
                }

                # Remove NA q values
                q_values <- q_values[!is.na(q_values)]

                if (length(q_values) < 2) {
                    return(list(
                        warning = FALSE,
                        message = jmvcore::.("Only one group has sufficient data for q estimation")
                    ))
                }

                # Calculate coefficient of variation
                mean_q <- mean(q_values)
                sd_q <- sd(q_values)
                cv_q <- sd_q / mean_q

                # Warning threshold
                warning_flag <- cv_q > 0.30

                # Create summary message
                if (warning_flag) {
                    severity <- if (cv_q > 0.50) "HIGH" else "MODERATE"
                    message <- jmvcore::format(jmvcore::.("{v1} heterogeneity detected (CV = {v2}). Detection probability varies substantially across groups. Consider stratified analysis."), v1 = severity, v2 = sprintf("%.2f", cv_q))
                } else {
                    message <- jmvcore::format(jmvcore::.("Low observed between-group variation (CV = {v1}); this does not establish independence or justify pooling."), v1 = sprintf("%.2f", cv_q))
                }

                list(
                    warning = warning_flag,
                    cv = cv_q,
                    mean_q = mean_q,
                    sd_q = sd_q,
                    group_stats = group_stats,
                    message = message
                )
            }
        )
    )
}
