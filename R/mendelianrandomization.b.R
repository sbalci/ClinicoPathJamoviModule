#' @title Mendelian Randomization
#' @importFrom R6 R6Class
#' @import jmvcore
#' @export

mendelianrandomizationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mendelianrandomizationClass",
    inherit = mendelianrandomizationBase,
    private = list(

        # Data storage
        .mr_input = NULL,
        .mr_results = NULL,
        .selected_snps = NULL,

        #---------------------------------------------
        # INIT
        #---------------------------------------------
        .init = function() {

            # Instructions
            html <- "<h3>Mendelian Randomization Analysis</h3>
            <p>Mendelian Randomization uses genetic variants as instrumental variables
            to infer causal relationships between exposures and outcomes.</p>

            <h4>When to Use MR:</h4>
            <ul>
            <li><b>Observational data:</b> When RCTs are infeasible or unethical</li>
            <li><b>Confounding concerns:</b> Genetic variants randomly assigned at conception</li>
            <li><b>Reverse causation:</b> Genes fixed before disease onset</li>
            <li><b>Drug target validation:</b> Assess causal role before trials</li>
            </ul>

            <h4>Key MR Methods:</h4>
            <ul>
            <li><b>IVW (Inverse Variance Weighted):</b> Standard method, assumes no pleiotropy</li>
            <li><b>MR-Egger:</b> Allows horizontal pleiotropy, tests for bias</li>
            <li><b>Weighted Median:</b> Robust to invalid instruments (<50%)</li>
            <li><b>MR-PRESSO:</b> Detects and corrects outliers</li>
            </ul>

            <h4>Data Requirements:</h4>
            <ol>
            <li>GWAS summary statistics (NOT individual-level data)</li>
            <li>SNP IDs (e.g., rsIDs)</li>
            <li>Effect sizes (beta coefficients)</li>
            <li>Standard errors</li>
            <li>P-values (for instrument selection)</li>
            </ol>

            <h4>MR Assumptions:</h4>
            <ol>
            <li><b>Relevance:</b> Genetic variant associated with exposure</li>
            <li><b>Independence:</b> Variant independent of confounders</li>
            <li><b>Exclusion:</b> Variant affects outcome only through exposure</li>
            </ol>

            <p><b>Note:</b> This module requires the 'MendelianRandomization' package.</p>"

            self$results$instructions$setContent(html)

            # Interpretation guide
            interp_html <- "<h3>Interpretation Guide</h3>

            <h4>Causal Effect Estimate:</h4>
            <ul>
            <li><b>Positive estimate:</b> Exposure increases outcome</li>
            <li><b>Negative estimate:</b> Exposure decreases outcome</li>
            <li><b>p < 0.05:</b> Significant causal effect</li>
            <li><b>CI excludes 0:</b> Robust evidence for causality</li>
            </ul>

            <h4>Method Comparison:</h4>
            <ul>
            <li><b>Concordant estimates:</b> Consistent causal effect across methods</li>
            <li><b>Discordant estimates:</b> May indicate pleiotropy or heterogeneity</li>
            <li><b>IVW vs MR-Egger difference:</b> Suggests horizontal pleiotropy</li>
            </ul>

            <h4>Heterogeneity Assessment:</h4>
            <ul>
            <li><b>Q statistic p < 0.05:</b> Significant heterogeneity</li>
            <li><b>I² > 50%:</b> Substantial heterogeneity</li>
            <li><b>High heterogeneity:</b> Consider random effects IVW or weighted median</li>
            </ul>

            <h4>Pleiotropy Test (MR-Egger):</h4>
            <ul>
            <li><b>Intercept ≠ 0 (p < 0.05):</b> Evidence of horizontal pleiotropy</li>
            <li><b>Intercept ≈ 0:</b> No evidence of pleiotropy (IVW valid)</li>
            </ul>

            <h4>Leave-One-Out Analysis:</h4>
            <ul>
            <li><b>Stable estimates:</b> No single SNP driving results</li>
            <li><b>Unstable estimates:</b> Results sensitive to specific SNPs</li>
            </ul>"

            self$results$interpretation$setContent(interp_html)
        },

        #---------------------------------------------
        # RUN
        #---------------------------------------------
        .run = function() {

            # Check requirements
            if (is.null(self$options$snp_column) || is.null(self$options$beta_exposure) ||
                is.null(self$options$se_exposure) || is.null(self$options$beta_outcome) ||
                is.null(self$options$se_outcome)) {
                return()
            }

            # Check for MendelianRandomization package
            if (!requireNamespace('MendelianRandomization', quietly = TRUE)) {
                error_msg <- "<h3>Package Required</h3>
                <p>The 'MendelianRandomization' package is required but not installed.</p>
                <p><b>Install using:</b> install.packages('MendelianRandomization')</p>"
                self$results$dataInfo$setContent(error_msg)
                return()
            }

            # Run MR analysis
            tryCatch({

                # Prepare data
                private$.prepareData()

                # Select instruments
                private$.selectInstruments()

                # Perform MR analysis
                private$.performMR()

                # Sensitivity analyses
                if (self$options$heterogeneity_test) {
                    private$.testHeterogeneity()
                }

                if (self$options$pleiotropy_test) {
                    private$.testPleiotropy()
                }

                if (self$options$leave_one_out) {
                    private$.leaveOneOut()
                }

            }, error = function(e) {
                error_msg <- paste0("<h3>MR Analysis Error</h3><p>", e$message, "</p>")
                self$results$dataInfo$setContent(error_msg)
            })
        },

        #---------------------------------------------
        # PREPARE DATA
        #---------------------------------------------
        .prepareData = function() {

            # Get data
            data <- self$data

            # Extract columns
            snp <- as.character(data[[self$options$snp_column]])
            beta_exp <- as.numeric(data[[self$options$beta_exposure]])
            se_exp <- as.numeric(data[[self$options$se_exposure]])
            beta_out <- as.numeric(data[[self$options$beta_outcome]])
            se_out <- as.numeric(data[[self$options$se_outcome]])

            # P-values (optional)
            if (!is.null(self$options$pval_exposure)) {
                pval_exp <- as.numeric(data[[self$options$pval_exposure]])
            } else {
                # Calculate from beta/SE if not provided
                pval_exp <- 2 * pnorm(-abs(beta_exp / se_exp))
            }

            if (!is.null(self$options$pval_outcome)) {
                pval_out <- as.numeric(data[[self$options$pval_outcome]])
            } else {
                pval_out <- 2 * pnorm(-abs(beta_out / se_out))
            }

            # Remove missing values
            complete_cases <- !is.na(snp) & !is.na(beta_exp) & !is.na(se_exp) &
                             !is.na(beta_out) & !is.na(se_out)

            if (sum(!complete_cases) > 0) {
                warning(paste("Removed", sum(!complete_cases), "SNPs with missing data"))
            }

            # Create MR input data
            mr_data <- data.frame(
                SNP = snp[complete_cases],
                beta_exp = beta_exp[complete_cases],
                se_exp = se_exp[complete_cases],
                pval_exp = pval_exp[complete_cases],
                beta_out = beta_out[complete_cases],
                se_out = se_out[complete_cases],
                pval_out = pval_out[complete_cases],
                stringsAsFactors = FALSE
            )

            # Store data
            private$.mr_input <- mr_data

            # Display data info
            info <- paste0("<b>Total SNPs in dataset:</b> ", nrow(mr_data))
            self$results$dataInfo$setContent(info)
        },

        #---------------------------------------------
        # SELECT INSTRUMENTS
        #---------------------------------------------
        .selectInstruments = function() {

            mr_data <- private$.mr_input
            pval_threshold <- self$options$pval_threshold %||% 5e-8

            # Filter by p-value threshold
            significant_snps <- mr_data$pval_exp < pval_threshold

            if (sum(significant_snps) == 0) {
                stop(paste("No SNPs meet the p-value threshold of", pval_threshold,
                          ". Consider relaxing the threshold."))
            }

            selected_data <- mr_data[significant_snps, ]

            # Check minimum SNPs
            min_snps <- self$options$min_snps %||% 3
            if (nrow(selected_data) < min_snps) {
                stop(paste("Insufficient SNPs:", nrow(selected_data),
                          "selected, minimum required:", min_snps))
            }

            # Store selected SNPs
            private$.selected_snps <- selected_data

            # Populate SNP table
            table <- self$results$selectedSNPs

            n_display <- min(20, nrow(selected_data))  # Show top 20
            for (i in 1:n_display) {
                table$addRow(rowKey = as.character(i), values = list(
                    snp = selected_data$SNP[i],
                    beta_exp = selected_data$beta_exp[i],
                    se_exp = selected_data$se_exp[i],
                    pval_exp = selected_data$pval_exp[i],
                    beta_out = selected_data$beta_out[i],
                    se_out = selected_data$se_out[i],
                    pval_out = selected_data$pval_out[i]
                ))
            }

            # SNP selection summary
            summary_html <- paste0(
                "<h4>Instrumental Variable Selection</h4>",
                "<p><b>SNPs meeting threshold (p < ", pval_threshold, "):</b> ",
                nrow(selected_data), "</p>",
                "<p><b>SNPs displayed in table:</b> ", n_display, "</p>"
            )

            self$results$snpSelection$setContent(summary_html)
        },

        #---------------------------------------------
        # PERFORM MR ANALYSIS
        #---------------------------------------------
        .performMR = function() {

            selected_data <- private$.selected_snps

            # Create MR input object
            mr_object <- MendelianRandomization::mr_input(
                bx = selected_data$beta_exp,
                bxse = selected_data$se_exp,
                by = selected_data$beta_out,
                byse = selected_data$se_out,
                snps = selected_data$SNP
            )

            # Get method selection
            mr_methods <- self$options$mr_methods %||% "main_three"

            # Perform methods
            results_list <- list()

            # IVW
            if (mr_methods %in% c("all", "ivw", "ivw_egger", "main_three")) {
                ivw_model <- self$options$ivw_model %||% "random"

                if (ivw_model == "fixed") {
                    ivw_result <- MendelianRandomization::mr_ivw(mr_object, model = "fixed")
                } else if (ivw_model == "random") {
                    ivw_result <- MendelianRandomization::mr_ivw(mr_object, model = "random")
                } else {
                    ivw_result <- MendelianRandomization::mr_ivw(mr_object, model = "random")
                }

                results_list$IVW <- ivw_result
            }

            # MR-Egger
            if (mr_methods %in% c("all", "egger", "ivw_egger", "main_three")) {
                egger_result <- MendelianRandomization::mr_egger(mr_object)
                results_list$`MR-Egger` <- egger_result
            }

            # Weighted Median
            if (mr_methods %in% c("all", "weighted_median", "main_three")) {
                wm_result <- MendelianRandomization::mr_median(mr_object, weighting = "weighted")
                results_list$`Weighted Median` <- wm_result
            }

            # Store results
            private$.mr_results <- results_list

            # Format and display results
            private$.formatMRResults(results_list)
        },

        .formatMRResults = function(results_list) {

            table <- self$results$mrResults
            conf_level <- self$options$conf_level %||% 0.95

            for (method_name in names(results_list)) {
                result <- results_list[[method_name]]

                # Extract results
                estimate <- result$Estimate
                se <- result$StdError
                ci_lower <- result$CILower
                ci_upper <- result$CIUpper
                p_value <- result$Pvalue

                # Number of SNPs
                n_snps <- if (!is.null(result$SNPs)) result$SNPs else nrow(private$.selected_snps)

                table$addRow(rowKey = method_name, values = list(
                    method = method_name,
                    n_snps = n_snps,
                    estimate = estimate,
                    se = se,
                    ci_lower = ci_lower,
                    ci_upper = ci_upper,
                    p_value = p_value
                ))
            }
        },

        #---------------------------------------------
        # HETEROGENEITY TEST
        #---------------------------------------------
        .testHeterogeneity = function() {

            results_list <- private$.mr_results

            if (is.null(results_list)) return()

            table <- self$results$heterogeneityTable

            # IVW heterogeneity
            if (!is.null(results_list$IVW)) {
                ivw <- results_list$IVW

                if (!is.null(ivw$Heter.Stat)) {
                    table$addRow(rowKey = "IVW", values = list(
                        method = "IVW",
                        q_statistic = ivw$Heter.Stat[1],
                        q_df = ivw$Heter.Stat[2],
                        q_pval = ivw$Heter.Stat[3],
                        i2 = NA  # Would calculate if data available
                    ))
                }
            }

            # MR-Egger heterogeneity
            if (!is.null(results_list$`MR-Egger`)) {
                egger <- results_list$`MR-Egger`

                if (!is.null(egger$Heter.Stat)) {
                    table$addRow(rowKey = "Egger", values = list(
                        method = "MR-Egger",
                        q_statistic = egger$Heter.Stat[1],
                        q_df = egger$Heter.Stat[2],
                        q_pval = egger$Heter.Stat[3],
                        i2 = NA
                    ))
                }
            }
        },

        #---------------------------------------------
        # PLEIOTROPY TEST
        #---------------------------------------------
        .testPleiotropy = function() {

            results_list <- private$.mr_results

            if (is.null(results_list$`MR-Egger`)) return()

            egger <- results_list$`MR-Egger`
            table <- self$results$pleiotropyTable

            intercept <- egger$Intercept
            se <- egger$StdError.Int
            p_value <- egger$Pvalue.Int

            interpretation <- if (p_value < 0.05) {
                "Significant pleiotropy detected - caution with IVW"
            } else {
                "No significant pleiotropy - IVW assumptions met"
            }

            table$addRow(rowKey = "pleiotropy", values = list(
                intercept = intercept,
                se = se,
                p_value = p_value,
                interpretation = interpretation
            ))
        },

        #---------------------------------------------
        # LEAVE-ONE-OUT ANALYSIS
        #---------------------------------------------
        .leaveOneOut = function() {

            selected_data <- private$.selected_snps
            n_snps <- nrow(selected_data)

            if (n_snps < 4) {
                # Too few SNPs for meaningful leave-one-out
                return()
            }

            table <- self$results$looTable

            # Perform leave-one-out for each SNP
            for (i in 1:min(n_snps, 20)) {  # Limit to 20 for performance

                # Remove one SNP
                loo_data <- selected_data[-i, ]

                # Create MR input
                mr_object <- MendelianRandomization::mr_input(
                    bx = loo_data$beta_exp,
                    bxse = loo_data$se_exp,
                    by = loo_data$beta_out,
                    byse = loo_data$se_out
                )

                # Run IVW
                tryCatch({
                    ivw_result <- MendelianRandomization::mr_ivw(mr_object)

                    table$addRow(rowKey = as.character(i), values = list(
                        snp_removed = selected_data$SNP[i],
                        estimate = ivw_result$Estimate,
                        ci_lower = ivw_result$CILower,
                        ci_upper = ivw_result$CIUpper
                    ))
                }, error = function(e) {
                    # Skip if error
                })
            }
        },

        #---------------------------------------------
        # VISUALIZATION
        #---------------------------------------------
        .plotForest = function(image, ...) {
            # Placeholder for forest plot
            TRUE
        },

        .plotFunnel = function(image, ...) {
            # Placeholder for funnel plot
            TRUE
        },

        .plotScatter = function(image, ...) {
            # Placeholder for scatter plot
            TRUE
        },

        .plotLeaveOneOut = function(image, ...) {
            # Placeholder for leave-one-out plot
            TRUE
        }
    )
)
