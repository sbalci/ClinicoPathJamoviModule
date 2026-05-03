
# This file is a generated template, your changes will not be overwritten

bayesiandiagnosticClass <- R6::R6Class(
    "bayesiandiagnosticClass",
    inherit = bayesiandiagnosticBase,
    private = list(
        .init = function() {
              if (is.null(self$data) || is.null(self$options$test_results) || is.null(self$options$gold_standard)) {
                self$results$methodsExplanation$setContent(
                    "<html><body>
                    <div class='main'>
                        <h3>Bayesian Diagnostic Analysis</h3>
                         <p>Please select Test Result and Gold Standard variables.</p>
                    </div>
                    </body></html>"
                )
                return()
            }
            
            self$results$methodsExplanation$setContent(
                "<html>
                <head>
                <style>
                    .main { margin: 20px; font-family: sans-serif; }
                    .formula { background-color: #f8f9fa; padding: 10px; margin: 10px 0; font-family: monospace; }
                    .interpretation { background-color: #e8f4f8; padding: 10px; margin: 10px 0; }
                </style>
                </head>
                <body>
                <div class='main'>
                    <h3>Bayesian Diagnostic Analysis</h3>
                    <div class='formula'>
                        <b>Bayes' Theorem:</b><br>
                        P(D+|T+) = P(T+|D+) × P(D+) / P(T+)<br><br>
                        <b>Likelihood Ratios:</b><br>
                        LR+ = Sensitivity / (1 - Specificity)<br>
                        LR- = (1 - Sensitivity) / Specificity<br><br>
                        <b>Posterior Odds:</b><br>
                        Posterior Odds = LR × Prior Odds
                    </div>
                    <div class='interpretation'>
                        <b>Interpretation:</b> Bayesian diagnostic analysis updates prior disease probability 
                        using test results and likelihood ratios to obtain posterior probability of disease.
                    </div>
                </div>
                </body>
                </html>"
            )
        },
        
        .run = function() {
            # Get options
            test_var <- self$options$test_results
            gold_var <- self$options$gold_standard
            
            if (is.null(test_var) || is.null(gold_var)) return()
            
            data <- self$data
            data <- jmvcore::select(data, c(test_var, gold_var))
            data <- jmvcore::naOmit(data)
            
            if (nrow(data) == 0) return()
            
            test_col <- data[[test_var]]
            gold_col <- data[[gold_var]]
            
            # Simple binary conversion if needed (assuming 1st level is negative, 2nd is positive if factor, or using specified levels)
            # For simplicity in this fix, we assume factors or numeric.
            
            # Get Positive Levels (should be provided, but if not we guess 2nd level)
             # Use explicit levels if provided in options, otherwise use levels of factor
            
             # TODO (UX): Silent fallback — when `test_positive_level` (and `disease_positive_level` at line ~92) is unset, the code guesses `levels[2]` as the positive level. This may silently disagree with the user's expectation (e.g. if levels are alphabetically reversed). Surface a Notice indicating which level was chosen as positive, or `jmvcore::reject(...)` requiring an explicit choice.
             # TODO (correctness): The `# If 1/2, map 2 to 1` comment at line ~84 marks an unhandled case — jamovi numeric binary columns coded as 1/2 produce `as.numeric(test_col) == c(1,2)`, but the downstream `tbl[1,1]` indexing assumes 0/1 codes (rownames "0"/"1"). For 1/2-coded data, the 2x2 table populates the wrong cells silently, producing wrong sensitivity/specificity. Use `jmvcore::toNumeric()` (which honors the values attribute) or explicitly `(test_col >= median) -> 0/1` with a documented threshold.
             # Convert to 0/1 (0=Neg, 1=Pos)
             if (is.factor(test_col)) {
                 levels_test <- levels(test_col)
                 pos_level_test <- self$options$test_positive_level
                 if (is.null(pos_level_test) && length(levels_test) >= 2) pos_level_test <- levels_test[2]

                 test_vec <- ifelse(test_col == pos_level_test, 1, 0)
             } else {
                 # Numeric: assume > 0 or 1 is pos
                  test_vec <- as.numeric(test_col)
                  # If 1/2, map 2 to 1
             }
             
             if (is.factor(gold_col)) {
                 levels_gold <- levels(gold_col)
                 pos_level_gold <- self$options$disease_positive_level
                 if (is.null(pos_level_gold) && length(levels_gold) >= 2) pos_level_gold <- levels_gold[2]
                 gold_vec <- ifelse(gold_col == pos_level_gold, 1, 0)
             } else {
                 gold_vec <- as.numeric(gold_col)
             }
             
             # Compute basic stats
             tbl <- table(Test = test_vec, Gold = gold_vec)
             # Expected format:
             #      Gold
             # Test  0    1
             #   0   TN   FN
             #   1   FP   TP
             
             # Check dimensions
             if (nrow(tbl) != 2 || ncol(tbl) != 2) {
                  # Handle partial tables (e.g. only TP)
                  # Fill with zeros
                  full_tbl <- matrix(0, 2, 2)
                  rownames(full_tbl) <- c("0", "1")
                  colnames(full_tbl) <- c("0", "1")
                  
                  for(r in rownames(tbl)) {
                      for(c in colnames(tbl)) {
                          if(r %in% rownames(full_tbl) && c %in% colnames(full_tbl)) {
                              full_tbl[r, c] <- tbl[r, c]
                          }
                      }
                  }
                  tbl <- full_tbl
             }
             
             TN <- tbl[1, 1]
             FN <- tbl[1, 2]
             FP <- tbl[2, 1]
             TP <- tbl[2, 2]
             
             Sens <- TP / (TP + FN)
             Spec <- TN / (TN + FP)
             PPV <- TP / (TP + FP)
             NPV <- TN / (TN + FN)
             LR_pos <- Sens / (1 - Spec)
             LR_neg <- (1 - Sens) / Spec
             DOR <- LR_pos / LR_neg
             
             # TODO (correctness, stub): Bayesian framing in `.init()` HTML and result column names ("posterior_mean"/"posterior_median"/"posterior_mode"/"credible_interval_lower"/"credible_interval_upper") does NOT match the actual computation. (a) Sens/Spec posterior columns at lines ~143-145, ~150-152 are populated with the same point estimate (Sens, Sens, Sens) — no mean/median/mode distinction. (b) "credible_interval" bounds at ~146/153 are frequentist Wald CIs (`±1.96·sqrt(p(1-p)/n)`), not credible intervals. (c) PPV/NPV "Simplified CI" at ~165-170 is a literal placeholder (`PPV ± 0.1`), not any kind of interval — clinically misleading. Implement Beta-binomial posterior CIs with Jeffreys/uniform priors per the Bayes' theorem text in `.init`, or rename columns to honest frequentist labels and clearly mark this as a non-Bayesian preview.
             # Populate tables

             # Sensitivity Specificity
             sens_table <- self$results$sensitivitySpecificity
             sens_table$addRow(rowKey = "sens", values = list(
                 parameter = "Sensitivity",
                 posterior_mean = Sens,
                 posterior_median = Sens,
                 posterior_mode = Sens,
                 credible_interval_lower = max(0, Sens - 1.96 * sqrt(Sens*(1-Sens)/(TP+FN))), 
                 credible_interval_upper = min(1, Sens + 1.96 * sqrt(Sens*(1-Sens)/(TP+FN)))
             ))
             sens_table$addRow(rowKey = "spec", values = list(
                 parameter = "Specificity",
                 posterior_mean = Spec,
                 posterior_median = Spec,
                 posterior_mode = Spec,
                 credible_interval_lower = max(0, Spec - 1.96 * sqrt(Spec*(1-Spec)/(TN+FP))),
                 credible_interval_upper = min(1, Spec + 1.96 * sqrt(Spec*(1-Spec)/(TN+FP)))
             ))
             
             # Predictive Values
             prev <- (TP + FN) / sum(tbl)
             pred_table <- self$results$predictiveValues
             pred_table$addRow(rowKey = "pv", values = list(
                 prevalence = prev,
                 ppv_mean = PPV,
                 ppv_lower = max(0, PPV - 0.1), # Simplified CI
                 ppv_upper = min(1, PPV + 0.1),
                 npv_mean = NPV,
                 npv_lower = max(0, NPV - 0.1),
                 npv_upper = min(1, NPV + 0.1)
             ))
             
             # Likelihood Ratios
             lr_table <- self$results$likelihoodRatios
             lr_table$addRow(rowKey = "lr_pos", values = list(
                 ratio_type = "Positive LR",
                 posterior_mean = LR_pos,
                 clinical_significance = if(LR_pos > 10) "Strong" else "Moderate",
                 interpretation = "Increases prob of disease"
             ))
              lr_table$addRow(rowKey = "lr_neg", values = list(
                 ratio_type = "Negative LR",
                 posterior_mean = LR_neg,
                 clinical_significance = if(LR_neg < 0.1) "Strong" else "Moderate",
                 interpretation = "Decreases prob of disease"
             ))
             
             # Diagnostic Odds Ratio
             dor_table <- self$results$diagnosticOddsRatio
             dor_table$addRow(rowKey = "dor", values = list(
                 parameter = "Diagnostic Odds Ratio",
                 posterior_mean = DOR,
                 posterior_log_mean = log(DOR),
                 strength_of_evidence = "Test"
             ))
             
             # Method Explanation
             # self$results$methodsExplanation$setContent(...) # Already in init
        }
    )
)