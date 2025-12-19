# PATHOLOGYAGREEMENT FIXES - COMPLETE PATCH
# Apply these changes to R/pathologyagreement.b.R

# ============================================================================
# FIX 1: Enhanced Bland-Altman with LoA Confidence Intervals
# ============================================================================
# LOCATION: .performAgreementAnalysis() - Replace lines 362-417

.performAgreementAnalysis = function(method1, method2) {
    # Calculate ICC using user-selected type
    if (requireNamespace('psych', quietly = TRUE)) {
        data_for_icc <- data.frame(Method1 = method1, Method2 = method2)
        icc_result <- psych::ICC(data_for_icc)

        # Select the appropriate ICC based on user choice
        # ICC(3,1) for consistency (relative agreement), ICC(2,1) for absolute agreement
        if (self$options$icc_type == "consistency") {
            icc_index <- 6  # ICC(3,1) - Consistency (fixed raters, relative agreement)
            icc_label <- "ICC(3,1) - Consistency"
            icc_interpretation <- "relative agreement between methods"
        } else { # absolute
            icc_index <- 4  # ICC(2,1) - Absolute Agreement (random raters, absolute values)
            icc_label <- "ICC(2,1) - Absolute Agreement"
            icc_interpretation <- "absolute value concordance between methods"
        }

        icc_value <- icc_result$results$ICC[icc_index]
        icc_lower <- icc_result$results$`lower bound`[icc_index]
        icc_upper <- icc_result$results$`upper bound`[icc_index]

        # STRONG_WARNING: Negative ICC indicates severe reliability issues
        if (!is.na(icc_value) && icc_value < 0) {
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'negativeICC',
                type = jmvcore::NoticeType$STRONG_WARNING
            )
            notice$setContent(sprintf('Negative ICC (%.3f) indicates severe reliability issues, often due to model assumption errors or greater within-subject than between-subject variance.', icc_value))
            self$results$insert(6, notice)  # STRONG_WARNING position
        }

        icc_interp <- ifelse(icc_value >= private$.CLINICAL_CONSTANTS$ICC_EXCELLENT, "Excellent reliability",
                     ifelse(icc_value >= private$.CLINICAL_CONSTANTS$ICC_GOOD, "Good reliability",
                     ifelse(icc_value >= private$.CLINICAL_CONSTANTS$ICC_MODERATE, "Moderate reliability", "Poor reliability")))
    } else {
        icc_value <- icc_lower <- icc_upper <- NA
        icc_interp <- "psych package required"
    }

    # Calculate Concordance Correlation Coefficient (CCC)
    if (requireNamespace('epiR', quietly = TRUE)) {
        ccc_result <- epiR::epi.ccc(method1, method2, ci = "z-transform", conf.level = self$options$conf_level)
        ccc_value <- ccc_result$rho.c$est
        ccc_lower <- ccc_result$rho.c$lower
        ccc_upper <- ccc_result$rho.c$upper

        # McBride 2005 interpretation
        ccc_interp <- ifelse(ccc_value >= 0.99, "Almost perfect",
                     ifelse(ccc_value >= 0.95, "Substantial",
                     ifelse(ccc_value >= 0.90, "Moderate", "Poor")))
    } else {
        ccc_value <- ccc_lower <- ccc_upper <- NA
        ccc_interp <- "epiR package required"
    }

    # Bland-Altman statistics with ENHANCED LoA CIs (Bland & Altman 1999)
    differences <- method1 - method2
    means <- (method1 + method2) / 2  # For proportional bias check
    mean_diff <- mean(differences)
    sd_diff <- sd(differences)
    n_obs <- length(differences)
    se_diff <- sd_diff / sqrt(n_obs)
    ci_factor <- qt(1 - (1 - self$options$conf_level)/2, df = n_obs - 1)

    mean_diff_lower <- mean_diff - ci_factor * se_diff
    mean_diff_upper <- mean_diff + ci_factor * se_diff

    # FIX 1: Limits of Agreement with 95% Confidence Intervals
    loa_lower <- mean_diff - 1.96 * sd_diff
    loa_upper <- mean_diff + 1.96 * sd_diff

    # Calculate CIs for LoA (Bland & Altman 1999)
    # SE(LoA) = SD × sqrt(3/n) for 95% limits
    se_loa <- sd_diff * sqrt(3 / n_obs)
    ci_factor_loa <- qt(1 - (1 - self$options$conf_level)/2, df = n_obs - 1)

    loa_lower_ci_low <- loa_lower - ci_factor_loa * se_loa
    loa_lower_ci_high <- loa_lower + ci_factor_loa * se_loa

    loa_upper_ci_low <- loa_upper - ci_factor_loa * se_loa
    loa_upper_ci_high <- loa_upper + ci_factor_loa * se_loa

    # Bias significance check
    bias_present <- (mean_diff_lower > 0) || (mean_diff_upper < 0)
    bias_msg <- if(bias_present) "Systematic bias present (CI excludes 0)" else "No significant bias"

    # FIX 2: Check for proportional bias (Bland-Altman regression)
    ba_regression <- lm(differences ~ means)
    prop_bias_slope <- coef(ba_regression)[2]
    prop_bias_p <- summary(ba_regression)$coefficients[2, 4]

    # WARNING: Proportional bias detected
    if (prop_bias_p < 0.05) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'proportionalBias',
            type = jmvcore::NoticeType$WARNING
        )
        if (prop_bias_slope > 0) {
            notice$setContent(sprintf('Proportional bias detected (slope=%.3f, p=%.3f). Disagreement increases at higher values. Consider log-transformation or percentage-difference analysis.', prop_bias_slope, prop_bias_p))
        } else {
            notice$setContent(sprintf('Proportional bias detected (slope=%.3f, p=%.3f). Disagreement decreases at higher values. This may indicate range restriction effects.', prop_bias_slope, prop_bias_p))
        }
        self$results$insert(21, notice)  # WARNING position
    }

    # Populate agreement table
    table <- self$results$agreementtable

    table$addRow(rowKey = 1, values = list(
        metric = icc_label,
        value = icc_value,
        ci_lower = icc_lower,
        ci_upper = icc_upper,
        interpretation = icc_interp
    ))

    table$addRow(rowKey = 2, values = list(
        metric = "Concordance Correlation Coefficient",
        value = ccc_value,
        ci_lower = ccc_lower,
        ci_upper = ccc_upper,
        interpretation = ccc_interp
    ))

    table$addRow(rowKey = 3, values = list(
        metric = "Bland-Altman Mean Difference (Bias)",
        value = mean_diff,
        ci_lower = mean_diff_lower,
        ci_upper = mean_diff_upper,
        interpretation = bias_msg
    ))

    table$addRow(rowKey = 4, values = list(
        metric = "Limits of Agreement (Lower)",
        value = loa_lower,
        ci_lower = loa_lower_ci_low,  # NEW: Add CI
        ci_upper = loa_lower_ci_high,  # NEW: Add CI
        interpretation = "95% of differences fall within limits"
    ))

    table$addRow(rowKey = 5, values = list(
        metric = "Limits of Agreement (Upper)",
        value = loa_upper,
        ci_lower = loa_upper_ci_low,  # NEW: Add CI
        ci_upper = loa_upper_ci_high,  # NEW: Add CI
        interpretation = "95% of differences fall within limits"
    ))
},


# ============================================================================
# FIX 3: Add Plain-Language Summary Generation
# ============================================================================
# LOCATION: Insert BEFORE .generateReportSentences() (around line 843)

.generatePlainLanguageSummary = function(method1, method2, n, n_removed = 0) {
    # Calculate key metrics
    spearman_test <- cor.test(method1, method2, method = "spearman")
    spearman_r <- spearman_test$estimate
    spearman_p <- spearman_test$p.value

    # Get ICC
    icc_value <- NA
    icc_lower <- NA
    icc_upper <- NA
    if (requireNamespace('psych', quietly = TRUE)) {
        data_for_icc <- data.frame(Method1 = method1, Method2 = method2)
        icc_result <- psych::ICC(data_for_icc)
        icc_index <- if (self$options$icc_type == "consistency") 6 else 4
        icc_value <- icc_result$results$ICC[icc_index]
        icc_lower <- icc_result$results$`lower bound`[icc_index]
        icc_upper <- icc_result$results$`upper bound`[icc_index]
    }

    # Bland-Altman
    differences <- method1 - method2
    mean_diff <- mean(differences)
    sd_diff <- sd(differences)
    se_diff <- sd_diff / sqrt(n)
    ci_factor <- qt(1 - (1 - self$options$conf_level)/2, df = n - 1)
    mean_diff_lower <- mean_diff - ci_factor * se_diff
    mean_diff_upper <- mean_diff + ci_factor * se_diff
    bias_present <- (mean_diff_lower > 0) || (mean_diff_upper < 0)

    # Interpret ICC
    icc_interp <- if (!is.na(icc_value)) {
        if (icc_value >= 0.90) "excellent"
        else if (icc_value >= 0.75) "good"
        else if (icc_value >= 0.50) "moderate"
        else "limited"
    } else {
        "unknown"
    }

    # Interpret correlation
    cor_strength <- if (abs(spearman_r) >= 0.90) "very strong"
                    else if (abs(spearman_r) >= 0.70) "strong"
                    else if (abs(spearman_r) >= 0.50) "moderate"
                    else "weak"

    # Clinical conclusion
    clinical_conclusion <- if (!is.na(icc_value) && icc_value >= 0.75 && abs(spearman_r) >= 0.70) {
        "These methods show acceptable agreement for interchangeable clinical use"
    } else if (!is.na(icc_value) && icc_value >= 0.50) {
        "These methods show moderate agreement; use caution when substituting one for the other"
    } else {
        "These methods show limited agreement and should not be used interchangeably without further validation"
    }

    # Build summary paragraph
    summary_html <- paste0(
        "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #007bff;'>",
        "<h4 style='margin-top: 0;'>📄 Plain Language Summary</h4>",
        "<div style='font-family: Georgia, serif; line-height: 1.6;'>",
        "<p>",
        sprintf("We compared <strong>%s</strong> versus <strong>%s</strong> using agreement analysis with <strong>%d paired observations</strong>",
                self$options$dep1, self$options$dep2, n),
        if (n_removed > 0) sprintf(" (%d cases excluded due to missing values)", n_removed) else "",
        ". ",

        if (!is.na(icc_value)) {
            sprintf("The intraclass correlation coefficient was <strong>%.3f (95%% CI %.3f-%.3f)</strong>, indicating <strong>%s reliability</strong>. ",
                    icc_value, icc_lower, icc_upper, icc_interp)
        } else {
            ""
        },

        sprintf("Spearman correlation was <strong>%.3f</strong> (p=%s), showing a <strong>%s association</strong>. ",
                spearman_r,
                if (spearman_p < 0.001) "&lt;0.001" else sprintf("=%.3f", spearman_p),
                cor_strength),

        sprintf("Bland-Altman analysis revealed %s systematic bias (mean difference=<strong>%.3f</strong>, 95%% CI %.3f to %.3f). ",
                if (bias_present) "<strong>significant</strong>" else "<strong>no significant</strong>",
                mean_diff, mean_diff_lower, mean_diff_upper),

        "<strong>", clinical_conclusion, ".</strong>",
        "</p>",

        "<p style='font-size: 0.9em; color: #666; margin-top: 10px; border-top: 1px solid #ddd; padding-top: 10px;'>",
        "<em>💡 Tip: Select and copy this text (Ctrl+C/Cmd+C) for use in reports or manuscripts.</em>",
        "</p>",
        "</div>",
        "</div>"
    )

    self$results$summary$setContent(summary_html)
},


# ============================================================================
# FIX 4: Add Educational Explanations Generation
# ============================================================================
# LOCATION: Insert BEFORE .generateReportSentences() (around line 843)

.generateEducationalExplanations = function() {
    explanations_html <- paste0(
        "<div style='background-color: #f0f8ff; padding: 20px; border-radius: 8px; border: 2px solid #4CAF50;'>",
        "<h3 style='margin-top: 0; color: #2c3e50;'>📘 About Agreement Analysis</h3>",

        "<div style='background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;'>",
        "<h4 style='color: #2c3e50;'>What This Analysis Does</h4>",
        "<p>Agreement analysis evaluates whether two measurement methods can be used <strong>interchangeably</strong> ",
        "by quantifying three key aspects:</p>",
        "<ol>",
        "<li><strong>Reliability</strong> (ICC): How consistently methods rank samples</li>",
        "<li><strong>Correlation</strong> (Spearman/Pearson): Strength of linear/monotonic relationship</li>",
        "<li><strong>Bias</strong> (Bland-Altman): Systematic differences between methods</li>",
        "</ol>",
        "</div>",

        "<div style='background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;'>",
        "<h4 style='color: #2c3e50;'>📋 When to Use This Analysis</h4>",
        "<ul>",
        "<li><strong>Platform comparison:</strong> HALO vs Aiforia vs ImageJ for Ki67 scoring</li>",
        "<li><strong>Method validation:</strong> New digital system vs established manual method</li>",
        "<li><strong>Inter-observer agreement:</strong> Pathologist A vs Pathologist B scoring</li>",
        "<li><strong>AI validation:</strong> Algorithm predictions vs expert pathologist diagnoses</li>",
        "<li><strong>Multi-site studies:</strong> Ensure consistent measurements across institutions</li>",
        "</ul>",
        "</div>",

        "<div style='background-color: #fff3cd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
        "<h4 style='color: #856404; margin-top: 0;'>⚠️ Key Assumptions</h4>",
        "<ul style='margin-bottom: 0;'>",
        "<li><strong>Paired measurements:</strong> Same sample measured by both methods</li>",
        "<li><strong>Continuous data:</strong> Numeric or ordered measurements (not categorical)</li>",
        "<li><strong>Sample size:</strong> Minimum n=30 recommended for pathology validation</li>",
        "<li><strong>For Pearson correlation:</strong> Approximately normal distributions (Shapiro-Wilk test performed)</li>",
        "<li><strong>For ICC:</strong> Independence of measurements within same sample</li>",
        "</ul>",
        "</div>",

        "<div style='background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;'>",
        "<h4 style='color: #2c3e50;'>📊 How to Interpret Results</h4>",
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<tr style='background-color: #f8f9fa; border-bottom: 2px solid #dee2e6;'>",
        "<th style='padding: 10px; text-align: left;'>Metric</th>",
        "<th style='padding: 10px; text-align: left;'>Threshold</th>",
        "<th style='padding: 10px; text-align: left;'>Interpretation</th>",
        "</tr>",
        "<tr style='border-bottom: 1px solid #dee2e6;'>",
        "<td style='padding: 10px;'><strong>ICC</strong></td>",
        "<td style='padding: 10px;'>≥ 0.90</td>",
        "<td style='padding: 10px;'>Excellent - methods interchangeable</td>",
        "</tr>",
        "<tr style='border-bottom: 1px solid #dee2e6;'>",
        "<td style='padding: 10px;'></td>",
        "<td style='padding: 10px;'>0.75-0.89</td>",
        "<td style='padding: 10px;'>Good - acceptable for most uses</td>",
        "</tr>",
        "<tr style='border-bottom: 1px solid #dee2e6;'>",
        "<td style='padding: 10px;'></td>",
        "<td style='padding: 10px;'>&lt; 0.75</td>",
        "<td style='padding: 10px;'>Caution - investigate disagreement</td>",
        "</tr>",
        "<tr style='border-bottom: 1px solid #dee2e6;'>",
        "<td style='padding: 10px;'><strong>Correlation</strong></td>",
        "<td style='padding: 10px;'>≥ 0.90</td>",
        "<td style='padding: 10px;'>Very strong association</td>",
        "</tr>",
        "<tr style='border-bottom: 1px solid #dee2e6;'>",
        "<td style='padding: 10px;'><strong>Bland-Altman</strong></td>",
        "<td style='padding: 10px;'>CI includes 0</td>",
        "<td style='padding: 10px;'>No systematic bias</td>",
        "</tr>",
        "<tr>",
        "<td style='padding: 10px;'></td>",
        "<td style='padding: 10px;'>CI excludes 0</td>",
        "<td style='padding: 10px;'>Systematic bias present</td>",
        "</tr>",
        "</table>",
        "</div>",

        "<div style='background-color: #d4edda; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #28a745;'>",
        "<h4 style='color: #155724; margin-top: 0;'>💡 Tips for Clinical Use</h4>",
        "<ul style='margin-bottom: 0;'>",
        "<li><strong>Check Bland-Altman plot:</strong> Look for funnel shape (proportional bias) or outliers</li>",
        "<li><strong>Consider clinical significance:</strong> Small statistical differences may not matter clinically</li>",
        "<li><strong>Report all metrics:</strong> ICC, correlation, and bias together tell the full story</li>",
        "<li><strong>Use presets:</strong> Select appropriate clinical preset (biomarker, AI, multi-site) for tailored validation</li>",
        "</ul>",
        "</div>",

        "<div style='margin-top: 20px; padding-top: 15px; border-top: 2px solid #dee2e6;'>",
        "<p style='font-size: 0.9em; color: #666; margin: 0;'>",
        "<strong>References:</strong><br>",
        "• Bland JM, Altman DG. Statistical methods for assessing agreement between two methods of clinical measurement. Lancet 1986;1:307-10.<br>",
        "• Bland JM, Altman DG. Measuring agreement in method comparison studies. Stat Methods Med Res 1999;8:135-60.<br>",
        "• Koo TK, Li MY. A Guideline of Selecting and Reporting Intraclass Correlation Coefficients for Reliability Research. J Chiropr Med 2016;15:155-63.",
        "</p>",
        "</div>",
        "</div>"
    )

    self$results$explanations$setContent(explanations_html)
},


# ============================================================================
# FIX 5: Update .run() to Call New Methods
# ============================================================================
# LOCATION: Insert after line 259 (in .run(), before existing guides)

# Generate plain-language summary (if enabled)
if (self$options$show_summary) {
    private$.generatePlainLanguageSummary(method1, method2, n, n_removed)
}

# Generate educational explanations (if enabled)
if (self$options$show_explanations) {
    private$.generateEducationalExplanations()
}


# ============================================================================
# END OF PATCH
# ============================================================================
#
# APPLY INSTRUCTIONS:
#
# 1. REPLACE .performAgreementAnalysis() method (lines 305-417) with enhanced version above
# 2. INSERT .generatePlainLanguageSummary() method BEFORE .generateReportSentences() (~line 843)
# 3. INSERT .generateEducationalExplanations() method BEFORE .generateReportSentences() (~line 843)
# 4. INSERT summary/explanations calls in .run() method (after line 259)
#
# After applying patch:
# - Run: Rscript -e "jmvtools::prepare('.')"
# - Test in jamovi with sample data
# - Verify new checkboxes appear in Display Options
# - Verify summary and explanations show when checkboxes enabled
