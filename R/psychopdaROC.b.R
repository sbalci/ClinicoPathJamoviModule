#' @title Comprehensive ROC Analysis with Advanced Features
#'
#' @description
#' Performs sophisticated Receiver Operating Characteristic (ROC) curve analysis
#' with optimal cutpoint determination, multiple comparison methods, and advanced
#' statistical features including IDI/NRI calculations, DeLong test, and
#' comprehensive visualization options.
#'
#' @details
#' This function provides an extensive ROC analysis toolkit that goes beyond
#' basic ROC curve generation. Key features include:
#'
#' **Core ROC Analysis:**
#' - AUC calculation with confidence intervals
#' - Multiple cutpoint optimization methods (12 different approaches)
#' - 16 different optimization metrics (Youden, accuracy, F1, etc.)
#' - Bootstrap confidence intervals
#' - Manual cutpoint specification
#'
#' **Advanced Statistical Methods:**
#' - DeLong test for comparing multiple AUCs
#' - IDI (Integrated Discrimination Index) with bootstrap CI
#' - NRI (Net Reclassification Index) with bootstrap CI
#' - Partial AUC calculations
#' - ROC curve smoothing (multiple methods)
#' - Classifier performance comparison
#'
#' **Visualization Options:**
#' - ROC curves (individual and combined)
#' - Sensitivity/specificity vs threshold plots
#' - Predictive value vs prevalence plots
#' - Precision-recall curves
#' - Dot plots showing class distributions
#' - Additional static ROC visualization
#' - Confidence bands and quantile confidence intervals
#'
#' **Subgroup Analysis:**
#' - Stratified analysis by grouping variables
#' - Cost-benefit optimization with custom cost ratios
#' - Hospital/site comparisons
#'
#' Single-AUC inference defaults to a logit transformation of the DeLong
#' standard error; the legacy untransformed approximation remains selectable.
#' Patient summaries use one prespecified mean or median per patient for all
#' calculations. They estimate patient-summary accuracy, not lesion-level accuracy.
#' Pairwise DeLong output includes raw and Holm-adjusted p-values. IDI/NRI remain
#' exploratory apparent estimates with percentile intervals and no significance
#' test. Within-cohort meta-analysis is unavailable, including legacy overrides.
#'
#' @param data A data frame containing the variables for analysis
#' @param patientID Optional patient identifier; required for patient summaries.
#' @param analysisUnit Independent rows (default), patientMean or patientMedian.
#' @param aucInference Single-AUC inference: logit (default) or legacy delong.
#' @param dependentVars Character vector of test variable names to evaluate.
#'   Multiple variables can be specified for comparison.
#' @param classVar Name of the classification variable (gold standard). Select two
#'   distinct observed levels; other levels and missing outcomes are excluded.
#' @param positiveClass Which level of classVar represents the positive class
#'   (condition present). Required: with NULL nothing is computed; levels are
#'   never inferred from their order.
#' @param negativeClass Which observed level represents the negative class
#'   (condition absent). Required and distinct from positiveClass.
#' @param subGroup Optional grouping variable for stratified analysis.
#' @param method Cutpoint optimization method. Options include:
#' - "maximize_metric": Maximize the specified metric
#' - "minimize_metric": Minimize the specified metric
#' - "oc_youden_kernel": Youden index with kernel smoothing
#' - "oc_manual": Use manually specified cutpoint
#' - "oc_cost_ratio": Optimize based on cost ratio
#' - "oc_equal_sens_spec": Equal sensitivity and specificity
#' - "oc_closest_01": Closest to perfect classifier (0,1)
#' @param metric Optimization metric when using maximize/minimize methods:
#'   "youden", "accuracy", "F1_score", "cohens_kappa", etc.
#' @param direction Classification direction: ">=" (higher values = positive) or
#'   "<=" (lower values = positive)
#' @param specifyCutScore Manual cutpoint value (required when method = "oc_manual")
#' @param delongTest Logical. Perform DeLong test for AUC comparison (requires >=2 variables)
#' @param calculateIDI Logical. Calculate Integrated Discrimination Index (requires >=2 variables)
#' @param calculateNRI Logical. Calculate Net Reclassification Index (requires >=2 variables)
#' @param refVar Reference variable for IDI/NRI calculations
#' @param plotROC Logical. Generate ROC curve plots
#' @param combinePlots Logical. Combine multiple variables in single plot
#' @param sensSpecTable Logical. Generate sensitivity/specificity confusion matrix
#' @param showThresholdTable Logical. Show detailed threshold performance table
#' @param partialAUC Logical. Calculate partial AUC over specified range
#' @param bootstrapCI Logical. Calculate bootstrap confidence intervals
#' @param precisionRecallCurve Logical. Generate precision-recall curves
#' @param compareClassifiers Logical. Generate classifier comparison metrics
#' @param ... Additional parameters for fine-tuning analysis
#'
#' @return A psychopdaROCResults object containing:
#' - `resultsTable`: Detailed results for each threshold
#' - `sensSpecTable`: Confusion matrix at optimal cutpoint
#' - `plotROC`: ROC curve visualization
#' - `delongTest`: DeLong test results (if requested)
#' - `idiTable`: IDI results with confidence intervals (if requested)
#' - `nriTable`: NRI results with confidence intervals (if requested)
#' - Additional plots and tables based on options selected
#'
#' @examples
#' \dontrun{
#' # Load example medical data
#' data(medical_roc_data)
#'
#' # Basic ROC analysis
#' result1 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = "biomarker1",
#'   classVar = "disease_status",
#'   positiveClass = "Disease", negativeClass = "Healthy",
#'   refVar = NULL
#' )
#'
#' # Compare multiple biomarkers with DeLong test
#' result2 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = c("biomarker1", "biomarker2", "biomarker3"),
#'   classVar = "disease_status",
#'   positiveClass = "Disease", negativeClass = "Healthy",
#'   delongTest = TRUE,
#'   combinePlots = TRUE,
#'   refVar = NULL
#' )
#'
#' # Advanced analysis with IDI/NRI
#' result3 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = c("biomarker1", "biomarker2"),
#'   classVar = "disease_status",
#'   positiveClass = "Disease", negativeClass = "Healthy",
#'   calculateIDI = TRUE,
#'   calculateNRI = TRUE,
#'   refVar = "biomarker1",
#'   nriThresholds = "0.3,0.7"
#' )
#'
#' # Cost-benefit optimization
#' result4 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = "biomarker1",
#'   classVar = "disease_status",
#'   positiveClass = "Disease", negativeClass = "Healthy",
#'   method = "oc_cost_ratio",
#'   costratioFP = 2.5,  # False positives cost 2.5x false negatives
#'   refVar = NULL
#' )
#'
#' # Subgroup analysis by hospital
#' result5 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = "biomarker1",
#'   classVar = "disease_status",
#'   positiveClass = "Disease", negativeClass = "Healthy",
#'   subGroup = "hospital",
#'   refVar = NULL
#' )
#'
#' # Comprehensive analysis with all features
#' result6 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = c("biomarker1", "biomarker2"),
#'   classVar = "disease_status",
#'   positiveClass = "Disease", negativeClass = "Healthy",
#'   method = "maximize_metric",
#'   metric = "youden",
#'   plotROC = TRUE,
#'   sensSpecTable = TRUE,
#'   showThresholdTable = TRUE,
#'   delongTest = TRUE,
#'   calculateIDI = TRUE,
#'   partialAUC = TRUE,
#'   bootstrapCI = TRUE,
#'   precisionRecallCurve = TRUE,
#'   compareClassifiers = TRUE,
#'   refVar = NULL
#' )
#'
#' # Financial risk assessment example
#' data(financial_roc_data)
#'
#' financial_result <- psychopdaROC(
#'   data = financial_roc_data,
#'   dependentVars = c("credit_score", "income_debt_ratio", "employment_score"),
#'   classVar = "default_status",
#'   positiveClass = "Default", negativeClass = "No_Default",
#'   direction = "<=", # Lower credit scores indicate higher risk
#'   method = "oc_cost_ratio",
#'   costratioFP = 0.1, # False positives (rejected good clients) cost less
#'   subGroup = "client_type", # DeLong comparisons require an ungrouped analysis
#'   refVar = NULL
#' )
#'
#' # Educational assessment example
#' data(education_roc_data)
#'
#' education_result <- psychopdaROC(
#'   data = education_roc_data,
#'   dependentVars = c("exam_score", "project_score", "peer_score"),
#'   classVar = "pass_status",
#'   positiveClass = "Pass", negativeClass = "Fail",
#'   method = "maximize_metric",
#'   metric = "accuracy",
#'   calculateIDI = TRUE,
#'   refVar = "exam_score",
#'   subGroup = "class_section"
#' )
#'
#' # Manufacturing quality control example
#' data(manufacturing_roc_data)
#'
#' quality_result <- psychopdaROC(
#'   data = manufacturing_roc_data,
#'   dependentVars = c("dimension_score", "surface_score", "strength_score"),
#'   classVar = "quality_status",
#'   positiveClass = "Defect", negativeClass = "Pass",
#'   method = "oc_equal_sens_spec", # Balanced sensitivity/specificity
#'   plotROC = TRUE,
#'   showCriterionPlot = TRUE,
#'   showDotPlot = TRUE,
#'   subGroup = "production_line",
#'   refVar = NULL
#' )
#' }
#'
#' @references
#' Kottas, M., Kuss, O., & Zapf, A. (2014). A modified Wald interval for the
#' area under the ROC curve (AUC) in diagnostic case-control studies.
#' BMC Medical Research Methodology, 14, 26. doi:10.1186/1471-2288-14-26.
#'
#' Pepe, M. S., Janes, H., & Li, C. I. (2014). Net risk reclassification
#' P values: valid or misleading? JNCI, 106(4), dju041. doi:10.1093/jnci/dju041.
#'
#' DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988).
#' Comparing the areas under two or more correlated receiver operating
#' characteristic curves: a nonparametric approach. Biometrics, 44(3), 837-845.
#'
#' Pencina, M. J., D'Agostino, R. B., D'Agostino, R. B., & Vasan, R. S. (2008).
#' Evaluating the added predictive ability of a new marker: from area under the
#' ROC curve to reclassification and beyond. Statistics in Medicine, 27(2), 157-172.
#'
#' Youden, W. J. (1950). Index for rating diagnostic tests. Cancer, 3(1), 32-35.
#'
#' @seealso
#' \code{\link[cutpointr]{cutpointr}} for cutpoint optimization methods
#' \code{\link[pROC]{roc}} for ROC curve analysis
#'
#' @note
#' This function originally developed by Lucas Friesen in the psychoPDA module.
#' Enhanced version with additional features added to the ClinicoPath module.
#'
#' @keywords ROC AUC sensitivity specificity diagnostic biomarker classification
#'
#' @export
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom cutpointr cutpointr
#' @importFrom MASS ginv
# ============================================================================
# MAIN ANALYSIS CLASS
# ============================================================================

psychopdaROCClass <- if (requireNamespace("jmvcore")) {
  R6::R6Class(
    "psychopdaROCClass",
    inherit = psychopdaROCBase,
    private = list(
      # ============================================================================
      # CLASS PRIVATE FIELDS
      # ============================================================================

      ## Storage for ROC data and other analysis results
      .rocDataList = list(), # Store ROC curve data for each variable
      .optimalCriteriaList = list(), # Store optimal cutpoints
      # Conditions the USER needs to know about, collected during .run() and
      # rendered into the runSummary warning box (which is always visible).
      #
      # These were previously raised with bare warning(). jamovi does not surface
      # R warnings as analysis output - they land in the undifferentiated
      # "Analysis Notes" panel alongside package chatter, so a silently degraded
      # feature (pROC missing -> partial AUC/bootstrap CI/smoothing quietly
      # skipped) looked identical to a feature the user had not switched on.
      .userWarnings = character(0),
      .varIdentity = list(),
      .quantileProbs = numeric(0),
      .smoothedRocDataList = list(),
      .prevalenceList = list(), # Store prevalence values
      # The SAMPLE prevalence, kept separate from .prevalenceList because that one is overwritten
      # by the user's prior when "Use prior prevalence" is ticked. The prior is documented as
      # applying to PREDICTIVE VALUES; accuracy is not a predictive value, and letting the prior
      # reach it silently turned the Accuracy column into expected accuracy in the assumed
      # population (0.849 against an empirical 0.767), under an unchanged heading and
      # contradicting this analysis's own fixedSensSpecTable.
      .samplePrevalenceList = list(),
      .aucList = list(), # Store AUC values for clinical interpretation
      .runSummaryHead = NULL, # Fixed part of the Analysis Status box, built in .run(); see .renderRunSummary
      .delongUsedFallback = FALSE, # TRUE when the pROC DeLong path failed and the local fallback ran; the table note reports which
      .modeInstructionsHtml = "", # Guidance text built by .applyClinicalModeSettings() and
      # appended to Procedure Notes by .run(). It is NOT written to `instructions`: .init()
      # overwrites that item and .run() hides it once variables are chosen.
      .CONFIDENCE_LEVEL = 0.95, # Default confidence level for tests

      # ============================================================================
      # INITIALIZATION AND VALIDATION
      # ============================================================================

      # ============================================================================
      # UTILITY METHODS
      # ============================================================================

      # Centralized package dependency checking
      # Total N needed for `target_power` to detect an AUC of `auc` against 0.5.
      #
      # This INVERTS the module's own Hanley-McNeil power function numerically
      # rather than using a closed form. The previous closed form was
      #   ((z_a + z_b) / (2*qnorm(A) - 1))^2 * (1/p + 1/q) * adj^2
      # which is in neither of the two papers the table cites, and failed three
      # ways that were all reproduced numerically:
      #   * internally inconsistent - it returned 141 where its OWN power formula,
      #     printed in the adjacent cell, gives that n only 50.6% power (true 283);
      #   * singular INSIDE the option's allowed range - 2*qnorm(A) - 1 vanishes at
      #     A = 0.6915, i.e. expectedAUCDifference = 0.1915, so the sequence ran
      #     141 -> 651 -> 496,622 -> 752,165,490 -> 281 as the effect GREW;
      #   * dangerously optimistic at small effects - 62 where 1149 are needed.
      # Inverting the power function cannot disagree with the power column, is
      # monotone in the effect size, and has no pole.
      # Power of a two-sided z-test of AUC = 0.5 at total n with positive fraction
      # pos_frac, Hanley-McNeil variance. Shared by .requiredNforPower and the power
      # curve plot, so the figure can no longer disagree with the table.
      .hanleyMcNeilPower = function(auc, n, pos_frac, alpha, adjustment_factor = 1) {
        z_alpha <- stats::qnorm(1 - alpha / 2)
        n_a <- n * pos_frac
        n_n <- n * (1 - pos_frac)
        if (n_a < 2 || n_n < 2) return(0)
        q1 <- auc / (2 - auc)
        q2 <- 2 * auc^2 / (1 + auc)
        v <- (auc * (1 - auc) + (n_a - 1) * (q1 - auc^2) +
              (n_n - 1) * (q2 - auc^2)) / (n_a * n_n)
        se <- sqrt(max(v, 0)) * adjustment_factor
        if (!is.finite(se) || se <= 0) return(1)
        z <- abs(auc - 0.5) / se
        stats::pnorm(z - z_alpha) + stats::pnorm(-z - z_alpha)
      },

      .requiredNforPower = function(auc, alpha, target_power, pos_frac,
                                    adjustment_factor = 1, n_max = 1e7) {
        if (!is.finite(auc) || auc <= 0 || auc >= 1 || auc == 0.5 || !is.finite(pos_frac) ||
            pos_frac <= 0 || pos_frac >= 1) return(NA_integer_)
        # A non-finite or non-positive adjustment means the comparison is
        # degenerate (perfectly correlated markers). Refuse rather than return a
        # number: adj -> 0 makes the SE vanish and yields an absurd n of 6.
        if (!is.finite(adjustment_factor) || adjustment_factor <= 0) return(NA_integer_)
        powr <- function(n) private$.hanleyMcNeilPower(auc, n, pos_frac, alpha, adjustment_factor)
        if (powr(n_max) < target_power) return(NA_integer_)
        lo <- 4; hi <- 16
        while (hi < n_max && powr(hi) < target_power) { lo <- hi; hi <- hi * 2 }
        hi <- min(hi, n_max)
        while (lo + 1 < hi) {                       # smallest n reaching the target
          mid <- floor((lo + hi) / 2)
          if (powr(mid) >= target_power) hi <- mid else lo <- mid
        }
        as.integer(hi)
      },

      .warnUser = function(msg, type = jmvcore::NoticeType$WARNING) {
        if (length(msg) && nzchar(msg)) {
          private$.userWarnings <- unique(c(private$.userWarnings, msg))
          private$.setNotice(paste0("warning", match(msg, private$.userWarnings)), msg, type)
        }
        invisible(NULL)
      },

      # Native notices supplement the detailed HTML panel. Reuse names across reruns.
      .setNotice = function(key, msg, type = jmvcore::NoticeType$INFO) {
        name <- paste0("psychopdaROC_", key)
        notice <- self$results$items[[name]]
        if (is.null(notice)) {
          notice <- jmvcore::Notice$new(self$options, name = name, type = type)
          self$results$add(notice)
        }
        notice$set(type, gsub("[\r\n]+", " ", msg))
        notice$setVisible(TRUE)
        invisible(NULL)
      },

      .resetNotices = function() {
        for (notice in self$results$items) {
          if (inherits(notice, "Notice") && startsWith(notice$name, "psychopdaROC_")) {
            notice$setContent("")
            notice$setVisible(FALSE)
          }
        }
      },

      # Writes the Analysis Status box: the header built in .run() plus every warning
      # collected so far. Producers pass plain text; escaping happens here, once.
      .renderRunSummary = function() {
        head <- private$.runSummaryHead
        if (is.null(head)) return(invisible(NULL))
        warnings <- private$.userWarnings
        box <- ""
        if (length(warnings) > 0) {
          box <- paste0(
            "<div style='background-color: rgba(255, 202, 33, 0.23); color: inherit; padding: 10px; border-radius: 4px; margin-top: 10px;'>",
            "<strong>", .("Warnings:"), "</strong><ul>",
            paste0("<li>", jmvcore::htmlEscape(warnings), "</li>", collapse = ""),
            "</ul></div>")
        }
        self$results$runSummary$setContent(paste0(head, box, "</div>"))
        invisible(NULL)
      },

      .checkPackageDependencies = function(required_packages, feature_name = "this feature") {
        missing_packages <- character(0)

        for (pkg in required_packages) {
          if (!requireNamespace(pkg, quietly = TRUE)) {
            missing_packages <- c(missing_packages, pkg)
          }
        }

        if (length(missing_packages) > 0) {
          pkg_list <- paste(missing_packages, collapse = ", ")
          private$.warnUser(sprintf(
            .("%1$s was skipped because the %2$s package is not installed. Install it and re-run to enable that output."),
            feature_name, pkg_list))
          return(FALSE)
        }
        return(TRUE)
      },

      # Standardized error handling with user-friendly messages
      .throwError = function(error_type, details = "", suggestion = "") {
        error_messages <- list(
          "insufficient_data" = .("Insufficient data for analysis"),
          "missing_positive_class" = .("Specified positive class not found"),
          "invalid_cutpoint" = .("Invalid cutpoint specification"),
          "insufficient_variables" = .("Insufficient variables for comparison"),
          "data_mismatch" = .("Data dimensions do not match"),
          "analysis_failed" = .("Analysis calculation failed")
        )

        base_message <- error_messages[[error_type]] %||% .("An error occurred")
        full_message <- if (nzchar(details)) paste0(base_message, ": ", details) else base_message
        if (suggestion != "") {
          full_message <- paste0(full_message, " \n", .("Suggestion:"), " ", suggestion)
        }

        private$.setNotice("validation", full_message, jmvcore::NoticeType$ERROR)
        jmvcore::reject(full_message)
      },

      # Safe variable name handling for data frame bracket access
      # Returns names unchanged - backtick escaping is NOT needed for data[[name]]
      .escapeVar = function(x) {
        if (is.null(x) || length(x) == 0) {
          return(x)
        }
        as.character(x)
      },

      # The whole class column as 1 (positive) / 0 (negative) / NA (missing), row-aligned with the
      # markers. An explicit NA level (addNA) is missing, not negative - factor `==` maps it to a
      # sentinel and returns FALSE - as in .prepareVarData() and the Outcome coding table.
      .classIndicator = function(data, positiveClass) {
        response <- private$.binaryResponse(data[[private$.escapeVar(self$options$classVar)]], positiveClass)
        as.numeric(response == positiveClass)
      },

      # ============================================================================
      # CLINICAL UTILITY METHODS
      # ============================================================================

      # Apply clinical mode settings
      .applyClinicalModeSettings = function() {
        mode <- self$options$clinicalMode

        # Adjust UI visibility based on clinical mode
        if (mode == "basic") {
          # Show only essential features for clinical users
          # Markup OUTSIDE .(), one complete sentence INSIDE each. A single .()
          # wrapping the whole indented HTML block hands the translator a blob
          # with embedded newlines and tags, which is what the i18n gate forbids.
          private$.modeInstructionsHtml <- paste0(
            "<h3>", .("Basic ROC Analysis"), "</h3>",
            "<p>", .("This simplified interface shows essential diagnostic performance metrics."),
            " ", .("Results include Area Under the Curve (AUC), optimal cutpoints, and performance statistics."), "</p>",
            "<p><strong>", .("AUC interpretation"), ":</strong> ",
            .("0.9 and above is excellent, 0.8 to 0.9 good, 0.7 to 0.8 fair, and below 0.7 poor."),
            " ", .("These labels describe discrimination in this sample only."), "</p>",
            "<p><strong>", .("Cutpoint caveat"), ":</strong> ",
            .("Unless you supplied the cutpoint yourself using the Manual cutpoint method, the optimal cutpoint is searched on these same data, so the sensitivity and specificity reported at it are optimistically biased and will typically be lower in a new sample."),
            " ", .("The bias grows as the sample gets smaller and as more candidate cutpoints are searched."),
            " ", .("The AUC is computed over all cutpoints and is not affected by that search."), "</p>"
          )
        } else if (mode == "advanced") {
          private$.modeInstructionsHtml <- paste0(
            "<h3>", .("Advanced ROC Analysis"), "</h3>",
            "<p>", .("Advanced interface with statistical comparisons, effect sizes, and additional metrics for research applications."), "</p>"
          )
        } else if (mode == "comprehensive") {
          private$.modeInstructionsHtml <- paste0(
            "<h3>", .("Comprehensive Statistical Analysis"), "</h3>",
            "<p>", .("Guidance for statistical comparisons, effect sizes, and bootstrap ROC analysis with prior weighting. Select each optional analysis separately."), "</p>"
          )
        } else {
          private$.modeInstructionsHtml <- ""
        }
        # Deliberately no setContent() here. This used to write `instructions`, which
        # .init() overwrites with the getting-started panel three lines later and .run()
        # then hides as soon as variables are chosen -- so every word of the text above,
        # including the cutpoint-optimism caveat, was computed and thrown away. The caller
        # in .run() appends it to Procedure Notes, which stays visible.
        invisible(private$.modeInstructionsHtml)
      },

      # Clinical interpretation helpers
      .interpretAUC = function(auc) {
        if (is.na(auc) || is.null(auc)) {
          return("Unknown")
        }
        # Six bands, matching the sibling analysis enhancedROC, so the same AUC is not given
        # two different words by two analyses in the same menu. Everything below 0.7 used to
        # collapse into "Poor", which gave a pure-noise marker (AUC 0.50) and an emphatically
        # backwards one (AUC 0.31, p = 5e-07) the same cell text.
        #
        # The bottom band reports WHAT WAS OBSERVED and does not assert that the marker is
        # reversed. Direction here is a fixed user choice, and under the null the AUC is
        # symmetric about 0.5, so about half of all uninformative markers fall below it by
        # chance alone (simulated: 0.492 of 2000 null markers at n = 200). Calling those
        # "reversed" would invite the user to flip Direction - and with it the cutpoint,
        # sensitivity and specificity - on noise. The confidence interval in aucSummaryTable
        # is what settles it, and the below-0.5 note there says so.
        # Band the value as displayed (3 dp): an AUC of exactly 80/100 computes as
        # 0.7999999999999999 and read "Fair" beside a printed 0.800.
        auc <- round(auc, 3)
        if (auc >= 0.9) {
          return(.("Excellent"))
        } else if (auc >= 0.8) {
          return(.("Good"))
        } else if (auc >= 0.7) {
          return(.("Fair"))
        } else if (auc >= 0.6) {
          return(.("Poor"))
        } else if (auc >= 0.5) {
          return(.("Minimal"))
        } else {
          return(.("Below chance"))
        }
      },
      .getClinicalRecommendation = function(auc, var) {
        if (is.na(auc) || is.null(auc)) {
          return(.("Unable to assess"))
        }

        # Descriptive summary only - no fitness-for-care verdict. The AUC is computed over all
        # cutpoints, so the cutpoint search does not bias it; it is simply an in-sample estimate
        # with no internal or external validation behind it.
        sprintf(.("In-sample AUC %.2f, estimated on these same data; not internally or externally validated"), auc)
      },
      .getDetailedInterpretation = function(auc, var) {
        if (is.na(auc) || is.null(auc)) {
          return(.("Analysis could not be completed"))
        }

        interpretation <- sprintf(.("The test '%1$s' has an AUC of %2$.3f"), var, auc)

        a <- round(auc, 3)   # the displayed value, as in .interpretAUC (same table row)
        band <- if (a >= 0.9) {
          .("excellent")
        } else if (a >= 0.8) {
          .("good")
        } else if (a >= 0.7) {
          .("fair")
        } else {
          .("poor")
        }

        # With method = "oc_manual" the cut score comes from the user and nothing is searched,
        # so the cutpoint-selection optimism does not apply. Same carve-out as .noteCutpointOptimism().
        cut_clause <- if (identical(self$options$method, "oc_manual")) {
          .("The sensitivity and specificity quoted at the cut score you supplied are a separate matter: those were not inflated by any cutpoint search, but they still carry ordinary sampling error.")
        } else {
          .("The sensitivity and specificity quoted at the selected cutpoint are a separate matter: those are optimistic, because that cutpoint was searched for on these same data.")
        }

        interpretation <- paste(interpretation, sprintf(.("indicating %1$s discrimination in this sample: the marker ranks a randomly chosen case from the positive class ahead of a randomly chosen case from the negative class about %2$.0f%% of the time (ties count one half), in the classification direction in use. AUC is a ranking property of pairs, computed over all cutpoints; it does not depend on which cutpoint was selected, and it does not describe how reliably any single cutpoint classifies an individual patient, which also depends on the cutpoint and on disease prevalence. This AUC is an in-sample estimate that has not been internally or externally validated. %3$s"), band, auc * 100, cut_clause))

        return(interpretation)
      },

      # Populate clinical interpretation table
      .populateClinicalInterpretation = function() {
        table <- self$results$clinicalInterpretationTable

        if (is.null(self$options$dependentVars) || length(self$options$dependentVars) == 0) {
          return()
        }

        # Iterate the keys .aucList was actually filled with, as the AUC summary table does.
        # In subgroup mode those keys are "<marker> ::: <group>", so looking up the bare
        # dependentVars names returned NA for every one of them and the table rendered with
        # headers and no rows at all.
        vars <- names(private$.aucList)
        if (length(vars) == 0) vars <- self$options$dependentVars

        for (var in vars) {
          # Get AUC from simple results table or calculate if needed
          auc <- private$.getAUCForVariable(var)

          if (!is.na(auc) && !is.null(auc)) {
            # Generate clinical interpretation using helper methods
            performance_level <- private$.interpretAUC(auc)
            clinical_recommendation <- private$.getClinicalRecommendation(auc, var)
            interpretation_text <- private$.getDetailedInterpretation(auc, var)

            table$addRow(rowKey = var, values = list(
              variable = var,
              performance_level = performance_level,
              clinical_recommendation = clinical_recommendation,
              interpretation_text = interpretation_text
            ))
          }
        }
      },

      # Helper to get AUC for a specific variable
      .getAUCForVariable = function(var) {
        # Try to get AUC from stored results
        tryCatch(
          {
            if (!is.null(private$.aucList[[var]])) {
              return(private$.aucList[[var]])
            } else {
              return(NA)
            }
          },
          error = function(e) {
            return(NA)
          }
        )
      },

      # ============================================================================
      # HELPER METHODS FOR OPTIMAL CUTPOINT CALCULATION
      # ============================================================================

      # Calculate cutpoint optimized for cost ratio
      #
      # @param confusionMatrix Matrix with tp, fp, tn, fn values for each threshold
      # @param prevalence The prevalence of the positive class
      # @param costRatio The cost ratio of false positives to false negatives
      # @return List with optimal index, threshold, and score
      .calculateCostRatioOptimal = function(confusionMatrix, prevalence, costRatio) {
        # Initialize variables
        n_thresholds <- length(confusionMatrix$x.sorted)
        scores <- numeric(n_thresholds)

        # Calculate utility score for each threshold
        for (i in 1:n_thresholds) {
          # Extract confusion matrix values
          tp <- confusionMatrix$tp[i]
          fp <- confusionMatrix$fp[i]
          tn <- confusionMatrix$tn[i]
          fn <- confusionMatrix$fn[i]

          # Calculate sensitivity and specificity with safe denominators
          sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else 0
          specificity <- if ((tn + fp) > 0) tn / (tn + fp) else 0

          # Expected cost minimization (Metz 1978, Pauker & Kassirer 1980)
          # Minimize: costRatio * (1-specificity) * (1-prevalence) + (1-sensitivity) * prevalence
          # Equivalent to maximizing the negative of this (net benefit formulation):
          scores[i] <- sensitivity * prevalence -
            costRatio * (1 - specificity) * (1 - prevalence)
        }

        # Find threshold with highest score
        best_idx <- which.max(scores)

        return(list(
          optimal_idx = best_idx,
          optimal_threshold = confusionMatrix$x.sorted[best_idx],
          score = scores[best_idx]
        ))
      },

      # Calculate cutpoint with equal sensitivity and specificity
      #
      # @param confusionMatrix Matrix with tp, fp, tn, fn values for each threshold
      # @return List with optimal index, threshold, and difference
      .calculateEqualSensSpec = function(confusionMatrix) {
        # Initialize variables
        n_thresholds <- length(confusionMatrix$x.sorted)
        differences <- numeric(n_thresholds)

        # Calculate absolute difference between sensitivity and specificity for each threshold
        for (i in 1:n_thresholds) {
          # Extract confusion matrix values
          tp <- confusionMatrix$tp[i]
          fp <- confusionMatrix$fp[i]
          tn <- confusionMatrix$tn[i]
          fn <- confusionMatrix$fn[i]

          # Calculate sensitivity and specificity with safe denominators
          sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else 0
          specificity <- if ((tn + fp) > 0) tn / (tn + fp) else 0

          # Calculate absolute difference
          differences[i] <- abs(sensitivity - specificity)
        }

        # Find threshold with minimal difference
        best_idx <- which.min(differences)

        return(list(
          optimal_idx = best_idx,
          optimal_threshold = confusionMatrix$x.sorted[best_idx],
          difference = differences[best_idx]
        ))
      },

      # Calculate cutpoint closest to (0,1) in ROC space
      #
      # @param confusionMatrix Matrix with tp, fp, tn, fn values for each threshold
      # @return List with optimal index, threshold, and distance
      .calculateClosestToOptimal = function(confusionMatrix) {
        # Initialize variables
        n_thresholds <- length(confusionMatrix$x.sorted)
        distances <- numeric(n_thresholds)

        # Calculate Euclidean distance to (0,1) in ROC space for each threshold
        for (i in 1:n_thresholds) {
          # Extract confusion matrix values
          tp <- confusionMatrix$tp[i]
          fp <- confusionMatrix$fp[i]
          tn <- confusionMatrix$tn[i]
          fn <- confusionMatrix$fn[i]

          # Calculate sensitivity and specificity with safe denominators
          sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else 0
          specificity <- if ((tn + fp) > 0) tn / (tn + fp) else 0

          # Calculate distance to (0,1) point
          # In ROC space, x-axis is 1-specificity, y-axis is sensitivity
          distances[i] <- sqrt((1 - specificity)^2 + (1 - sensitivity)^2)
        }

        # Find threshold with minimal distance
        best_idx <- which.min(distances)

        return(list(
          optimal_idx = best_idx,
          optimal_threshold = confusionMatrix$x.sorted[best_idx],
          distance = distances[best_idx]
        ))
      },

      # ============================================================================
      # FIXED SENSITIVITY/SPECIFICITY ANALYSIS METHODS
      # ============================================================================

      # Map a numeric cutoff to the empirical decision rule, including infinite endpoints.
      .thresholdIndex = function(thresholds, cut, direction = self$options$direction) {
        if (length(cut) != 1L || is.na(cut)) return(integer(0))
        hit <- if (direction == ">=") which(thresholds >= cut) else which(thresholds <= cut)
        if (!length(hit)) return(integer(0))
        if (direction == ">=") hit[which.min(thresholds[hit])] else hit[which.max(thresholds[hit])]
      },

      # Calculate cutpoint for fixed sensitivity or specificity value
      .calculateFixedSensSpec = function(confusionMatrix, target_value, analysis_type, interpolation_method = "linear") {
        n_thresholds <- length(confusionMatrix$x.sorted)

        # Calculate sensitivity and specificity for all thresholds
        sensitivities <- numeric(n_thresholds)
        specificities <- numeric(n_thresholds)

        for (i in 1:n_thresholds) {
          tp <- confusionMatrix$tp[i]
          fp <- confusionMatrix$fp[i]
          tn <- confusionMatrix$tn[i]
          fn <- confusionMatrix$fn[i]

          sensitivities[i] <- if ((tp + fn) > 0) tp / (tp + fn) else 0
          specificities[i] <- if ((tn + fp) > 0) tn / (tn + fp) else 0
        }

        # Find closest point based on analysis type
        if (analysis_type == "sensitivity") {
          target_values <- sensitivities
          target_name <- "sensitivity"
        } else {
          target_values <- specificities
          target_name <- "specificity"
        }

        # Handle interpolation methods
        # best_idx is carried out of this block so the caller can read PPV/NPV/accuracy at
        # the SAME confusion-matrix row the sensitivity/specificity came from. It stays NA
        # only on the genuinely interpolated path, which has no single row.
        best_idx <- NA_integer_

        if (interpolation_method == "nearest") {
          # Find nearest point
          best_idx <- which.min(abs(target_values - target_value))
          cutpoint <- confusionMatrix$x.sorted[best_idx]
          achieved_sens <- sensitivities[best_idx]
          achieved_spec <- specificities[best_idx]
          interpolation_used <- .("Nearest Point")
        } else if (interpolation_method == "stepwise") {
          # Conservative approach - among the cutpoints that MEET the target, take the one
          # that is best on the other metric.
          #
          # WAS: `valid_indices[1]` in both arms of an if/else whose two branches were
          # byte-identical. cutpointr's x.sorted runs DESCENDING from Inf, so sensitivity
          # increases along it while specificity DECREASES - the first qualifying index for
          # a specificity target is therefore always index 1 (specificity 1.000,
          # sensitivity 0.000, cutpoint Inf), i.e. "classify nobody as positive", for every
          # target <= 1. Selecting on the complementary metric is correct whatever the sort
          # order, and gives exactly the same point as `valid_indices[1]` on the sensitivity
          # arm, which was already right.
          valid_indices <- which(target_values >= target_value)

          if (length(valid_indices) > 0) {
            other_values <- if (analysis_type == "sensitivity") specificities else sensitivities
            best_idx <- valid_indices[which.max(other_values[valid_indices])]
          } else {
            # If no point meets target, use the closest
            best_idx <- which.min(abs(target_values - target_value))
          }

          cutpoint <- confusionMatrix$x.sorted[best_idx]
          achieved_sens <- sensitivities[best_idx]
          achieved_spec <- specificities[best_idx]
          interpolation_used <- .("Stepwise (Conservative)")
        } else {
          # Linear interpolation (default)
          if (target_value <= min(target_values)) {
            # Target below minimum, use minimum point
            best_idx <- which.min(target_values)
            cutpoint <- confusionMatrix$x.sorted[best_idx]
            achieved_sens <- sensitivities[best_idx]
            achieved_spec <- specificities[best_idx]
          } else if (target_value >= max(target_values)) {
            # Target above maximum, use maximum point
            best_idx <- which.max(target_values)
            cutpoint <- confusionMatrix$x.sorted[best_idx]
            achieved_sens <- sensitivities[best_idx]
            achieved_spec <- specificities[best_idx]
          } else {
            # Interpolate between two closest points
            below_indices <- which(target_values <= target_value)
            above_indices <- which(target_values >= target_value)

            if (length(below_indices) > 0 && length(above_indices) > 0) {
              below_idx <- below_indices[which.max(target_values[below_indices])]
              above_idx <- above_indices[which.min(target_values[above_indices])]

              # Linear interpolation
              below_val <- target_values[below_idx]
              above_val <- target_values[above_idx]
              denom <- above_val - below_val
              weight <- if (abs(denom) > 1e-12) (target_value - below_val) / denom else 0.5

              endpoints <- confusionMatrix$x.sorted[c(below_idx, above_idx)]
              if (all(is.finite(endpoints))) {
                cutpoint <- (1 - weight) * endpoints[1] + weight * endpoints[2]
              } else {
                # Infinity cannot be interpolated. Use the endpoint meeting the target.
                cutpoint <- confusionMatrix$x.sorted[above_idx]
              }
              best_idx <- private$.thresholdIndex(confusionMatrix$x.sorted, cutpoint)
              achieved_sens <- sensitivities[best_idx]
              achieved_spec <- specificities[best_idx]
            } else {
              # Fallback to nearest
              best_idx <- which.min(abs(target_values - target_value))
              cutpoint <- confusionMatrix$x.sorted[best_idx]
              achieved_sens <- sensitivities[best_idx]
              achieved_spec <- specificities[best_idx]
            }
          }
          interpolation_used <- .("Linear cutoff; empirical rates")
        }

        return(list(
          cutpoint = cutpoint,
          achieved_sensitivity = achieved_sens,
          achieved_specificity = achieved_spec,
          index = best_idx,
          interpolation_method = interpolation_used
        ))
      },

      # Populate fixed sensitivity/specificity results table
      .populateFixedSensSpecTable = function(var, confusionMatrix, positiveClass) {
        table <- self$results$fixedSensSpecTable

        if (!self$options$fixedSensSpecAnalysis) {
          return()
        }

        analysis_type <- self$options$fixedAnalysisType
        target_value <- if (analysis_type == "sensitivity") {
          self$options$fixedSensitivityValue
        } else {
          self$options$fixedSpecificityValue
        }

        interpolation_method <- self$options$fixedInterpolation

        # Calculate fixed sensitivity/specificity cutpoint
        fixed_result <- private$.calculateFixedSensSpec(
          confusionMatrix, target_value, analysis_type, interpolation_method
        )

        # Calculate additional metrics at the determined cutpoint
        cutpoint <- fixed_result$cutpoint
        achieved_sens <- fixed_result$achieved_sensitivity
        achieved_spec <- fixed_result$achieved_specificity

        # Calculate PPV, NPV, accuracy, and Youden's J at the SAME row the
        # sensitivity/specificity above came from.
        # WAS: always re-derived with which.min(abs(x.sorted - cutpoint)). x.sorted[1] is
        # Inf, so abs(Inf - Inf) is NaN, which.min() skips it and silently returned index 2
        # - the PPV/NPV/accuracy in the row then described a DIFFERENT cutpoint from the
        # sensitivity/specificity beside them. Use the index the selection actually used;
        # only the interpolated path has none.
        closest_idx <- fixed_result$index
        if (is.null(closest_idx) || is.na(closest_idx)) {
          distances <- abs(confusionMatrix$x.sorted - cutpoint)
          closest_idx <- if (any(is.finite(distances))) which.min(distances) else NA_integer_
        }

        if (is.na(closest_idx)) {
          tp <- NA_real_; fp <- NA_real_; tn <- NA_real_; fn <- NA_real_
        } else {
          tp <- confusionMatrix$tp[closest_idx]
          fp <- confusionMatrix$fp[closest_idx]
          tn <- confusionMatrix$tn[closest_idx]
          fn <- confusionMatrix$fn[closest_idx]
        }

        ppv <- if (!is.na(tp + fp) && (tp + fp) > 0) tp / (tp + fp) else NA
        npv <- if (!is.na(tn + fn) && (tn + fn) > 0) tn / (tn + fn) else NA
        if (self$options$usePriorPrev) {
          prevalence <- self$options$priorPrev
          ppv <- achieved_sens * prevalence /
            (achieved_sens * prevalence + (1 - achieved_spec) * (1 - prevalence))
          npv <- achieved_spec * (1 - prevalence) /
            (achieved_spec * (1 - prevalence) + (1 - achieved_sens) * prevalence)
        }
        accuracy <- if (!is.na(tp + fp + tn + fn) && (tp + fp + tn + fn) > 0) (tp + tn) / (tp + fp + tn + fn) else NA
        youden <- achieved_sens + achieved_spec - 1

        # Ensure all values are atomic (scalar) for jamovi table
        # Convert vectors to single values, handle NaN/Inf
        cutpoint <- as.numeric(cutpoint)[1]
        achieved_sens <- as.numeric(achieved_sens)[1]
        achieved_spec <- as.numeric(achieved_spec)[1]
        ppv <- as.numeric(ppv)[1]
        npv <- as.numeric(npv)[1]
        accuracy <- as.numeric(accuracy)[1]
        youden <- as.numeric(youden)[1]
        target_value <- as.numeric(target_value)[1]

        # Ensure strings are atomic
        var <- as.character(var)[1]
        analysis_type_str <- paste("Fixed", tools::toTitleCase(analysis_type))
        interpolation_str <- as.character(fixed_result$interpolation_method)[1]
        if (self$options$usePriorPrev) {
          private$.notePredictiveValues(table, self$options$priorPrev)
        } else {
          table$setNote("predictive_values_prevalence",
            .("PPV and NPV use each marker's complete-case sample prevalence. They require recalculation for another prevalence; transport also requires comparable case mix and conditional score distributions."), init = FALSE)
        }
        table$setNote("achieved_rule",
          .("Achieved rates and accuracy classify the observed complete cases at the displayed cutoff. An interpolated cutoff can miss the target; Stepwise meets or exceeds it. Empty predicted groups have undefined predictive values, shown as blank cells."), init = FALSE)

        # Add row to table
        table$addRow(rowKey = var, values = list(
          variable = var,
          analysis_type = analysis_type_str,
          target_value = target_value,
          cutpoint = cutpoint,
          achieved_sensitivity = achieved_sens,
          achieved_specificity = achieved_spec,
          ppv = ppv,
          npv = npv,
          accuracy = accuracy,
          youden = youden,
          interpolation_used = interpolation_str
        ))
      },

      # Generate explanatory content for fixed sensitivity/specificity analysis
      .generateFixedSensSpecExplanation = function() {
        if (!self$options$fixedSensSpecAnalysis || !self$options$showFixedExplanation) {
          return()
        }

        sensitivity <- identical(self$options$fixedAnalysisType, "sensitivity")
        target <- if (sensitivity) self$options$fixedSensitivityValue else
          self$options$fixedSpecificityValue
        overview <- if (sensitivity)
          sprintf(.("Find a cutpoint for target sensitivity %1$.3f (%2$.1f%%), then report the achieved sensitivity and corresponding specificity."), target, 100 * target)
        else sprintf(.("Find a cutpoint for target specificity %1$.3f (%2$.1f%%), then report the achieved specificity and corresponding sensitivity."), target, 100 * target)
        context <- if (sensitivity)
          .("Higher sensitivity reduces missed cases but may increase false positives. A negative result supports exclusion only when the negative likelihood ratio, (1 - sensitivity) / specificity, is sufficiently small for the clinical context.")
        else .("Higher specificity reduces false positives but may increase missed cases. A positive result supports confirmation only when the positive likelihood ratio, sensitivity / (1 - specificity), is sufficiently large for the clinical context.")
        interpolation <- switch(self$options$fixedInterpolation,
          linear = .("Linear interpolation estimates a cutoff between ROC points. Reported rates are recalculated from observations at that cutoff, so a discrete sample may miss the requested target. If interpolation involves infinity, the endpoint meeting the target is used."),
          nearest = .("Nearest point uses the observed cutoff closest to the requested target. It does not interpolate and may fall below or above the target."),
          stepwise = .("Stepwise uses an observed cutoff that meets or exceeds the target while retaining the best corresponding specificity or sensitivity. Ties and discrete values can make the achieved rate exceed the target."))
        explanation <- paste0(
          "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>",
          "<h4>", .("Fixed Sensitivity/Specificity Analysis Guide"), "</h4><p>", overview,
          "</p><p>", context, "</p><h5>", .("Interpolation and achieved performance"),
          "</h5><p>", interpolation, "</p><h5>", .("How to read these results"), "</h5><ul><li>",
          .("The achieved value is the rate in this sample at the selected cutpoint. It is not guaranteed in another sample. Cutpoint selection and performance assessment use the same observations, so these are apparent estimates requiring independent validation."),
          "</li><li>", .("Sensitivity and specificity use the positive and negative groups separately. They can change with case mix. Small class counts make both the cutpoint and the achieved rates unstable."),
          "</li><li>", .("PPV and NPV depend on prevalence. A prior prevalence setting adjusts predictive values, while sensitivity, specificity and accuracy remain empirical sample rates."),
          "</li><li>", .("Youden's J equals sensitivity plus specificity minus one. Use the actual trade-off and clinical consequences when assessing a cutpoint."),
          "</li></ul><p>", .("Validate the cutpoint and its uncertainty in the intended population before clinical use."),
          "</p><h5>", .("Further reading"), "</h5><p>",
          "Youden WJ (1950). Index for rating diagnostic tests. Cancer 3:32-35. ",
          "Zweig MH, Campbell G (1993). Receiver-operating characteristic plots. Clin Chem 39:561-577.",
          "</p></div>"
        )

        # Set the content
        self$results$fixedSensSpecExplanation$setContent(explanation)
      },

      # Keep the explicitly selected classes in pROC's control/case order.
      # Factor conversion excludes missing and unselected outcomes without a sentinel label.
      .binaryResponse = function(class, positiveClass) {
        factor(as.character(class), levels = c(self$options$negativeClass, positiveClass))
      },

      # Calculate partial AUC using pROC package
      .calculatePartialAUC = function(x, class, positiveClass, from, to) {
        # Check package dependencies
        if (!private$.checkPackageDependencies("pROC", "partial AUC calculations")) {
          return(NULL)
        }

        # Create ROC object with direction matching user setting
        pROC_direction <- ifelse(self$options$direction == ">=", "<", ">")
        roc_obj <- pROC::roc(
          response = private$.binaryResponse(class, positiveClass),
          predictor = x,
          direction = pROC_direction,
          quiet = TRUE
        )

        # Calculate partial AUC
        partial_auc <- try(pROC::auc(roc_obj, partial.auc = c(from, to), partial.auc.focus = "specificity"), silent = TRUE)

        # Calculate normalized partial AUC (scaled to 0-1 range)
        partial_auc_norm <- suppressWarnings(try(pROC::auc(roc_obj,
          partial.auc = c(from, to), partial.auc.focus = "specificity",
          partial.auc.correct = TRUE), silent = TRUE))
        if (inherits(partial_auc, "try-error") || !is.finite(as.numeric(partial_auc))) {
          private$.warnUser(.("The raw partial AUC could not be estimated for the requested specificity range; its cell is blank."))
          partial_auc <- NA_real_
        }
        if (inherits(partial_auc_norm, "try-error") || !is.finite(as.numeric(partial_auc_norm))) {
          private$.warnUser(.("The normalized partial AUC is unavailable. The correction is undefined when the partial curve falls below the diagonal; the raw partial AUC is reported separately."))
          partial_auc_norm <- NA_real_
        }

        # Calculate CI if possible
        if (self$options$bootstrapCI) {
          ci <- try(pROC::ci.auc(roc_obj,
            partial.auc = c(from, to), partial.auc.focus = "specificity",
            reuse.auc = FALSE, partial.auc.correct = FALSE,
            method = "bootstrap", boot.n = self$options$bootstrapReps
          ), silent = TRUE)

          if (inherits(ci, "try-error")) {
            ci_lower <- NA
            ci_upper <- NA
          } else {
            ci_lower <- ci[1]
            ci_upper <- ci[3]
          }
        } else {
          ci_lower <- NA
          ci_upper <- NA
        }

        return(list(
          pAUC = as.numeric(partial_auc),
          pAUC_normalized = as.numeric(partial_auc_norm),
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          spec_range = paste0("(", from, " - ", to, ")")
        ))
      },

      # Create smoothed ROC curve using pROC
      .createSmoothedROC = function(x, class, positiveClass, method) {
        # Check package dependencies
        if (!private$.checkPackageDependencies("pROC", "ROC curve smoothing")) {
          return(NULL)
        }

        # Create ROC object with direction matching user setting
        pROC_direction <- ifelse(self$options$direction == ">=", "<", ">")
        roc_obj <- pROC::roc(
          response = private$.binaryResponse(class, positiveClass),
          predictor = x,
          direction = pROC_direction,
          quiet = TRUE
        )

        # Apply smoothing
        if (method != "none") {
          smooth_roc <- try(pROC::smooth(roc_obj, method = method), silent = TRUE)

          if (!inherits(smooth_roc, "try-error")) {
            # Extract coordinates
            coords <- pROC::coords(smooth_roc, x = "all", ret = c("threshold", "specificity", "sensitivity"))

            return(list(
              threshold = coords$threshold,
              specificity = coords$specificity,
              sensitivity = coords$sensitivity,
              auc = pROC::auc(smooth_roc)
            ))
          }
        }

        return(NULL)
      },

      # Calculate bootstrap confidence intervals for metrics
      .calculateBootstrapCI = function(x, class, positiveClass, metrics = c("auc", "threshold", "sensitivity", "specificity")) {
        # Check package dependencies
        if (!private$.checkPackageDependencies("pROC", "bootstrap confidence intervals")) {
          return(NULL)
        }

        # Create ROC object with direction matching user setting
        pROC_direction <- ifelse(self$options$direction == ">=", "<", ">")
        roc_obj <- pROC::roc(
          response = private$.binaryResponse(class, positiveClass),
          predictor = x,
          direction = pROC_direction,
          quiet = TRUE
        )

        results <- list()

        # Calculate CI for AUC
        if ("auc" %in% metrics) {
          auc_ci <- try(pROC::ci.auc(roc_obj, method = "bootstrap", boot.n = self$options$bootstrapReps), silent = TRUE)

          if (!inherits(auc_ci, "try-error")) {
            results$auc <- list(
              estimate = as.numeric(pROC::auc(roc_obj)),
              ci_lower = auc_ci[1],
              ci_upper = auc_ci[3]
            )
          }
        }

        # This table explicitly reports a Youden reference, independently of the main method.
        # Use the same seeded random tie policy for the estimate and each bootstrap replicate.
        params <- intersect(c("threshold", "sensitivity", "specificity"), metrics)
        if (length(params)) {
          optimal <- pROC::coords(roc_obj, x = "best", best.method = "youden",
            ret = c("threshold", "sensitivity", "specificity"))
          chosen <- if (nrow(optimal) == 1L) 1L else sample.int(nrow(optimal), 1L)
          ci <- try(pROC::ci.coords(roc_obj, x = "best", best.method = "youden",
            best.policy = "random", ret = params, boot.n = self$options$bootstrapReps,
            boot.stratified = TRUE), silent = TRUE)
          for (param in params) {
            limits <- if (inherits(ci, "try-error")) c(NA_real_, NA_real_) else
              as.numeric(ci[[param]][1, c(1, 3)])
            results[[param]] <- list(estimate = as.numeric(optimal[[param]][chosen]),
              ci_lower = limits[1], ci_upper = limits[2])
          }
          if (inherits(ci, "try-error")) {
            private$.warnUser(.fmt(
              .("Bootstrap Youden-reference coordinate intervals are unavailable ({msg}). Point estimates are shown with blank interval cells."),
              msg = as.character(ci)))
          }
        }

        return(results)
      },

      # Prepare data for a single variable with optional subgrouping
      .prepareVarData = function(data, var, subGroup) {
        # Escape variable names for safe data frame access
        classVarEscaped <- private$.escapeVar(self$options$classVar)

        if (is.null(subGroup)) {
          # Use [[]] for column access with escaped names
          varEscaped <- private$.escapeVar(var)
          dependentVar <- jmvcore::toNumeric(data[[varEscaped]])
          classVar <- data[[classVarEscaped]]
        } else {
          identity <- private$.varIdentity[[var]]
          varName <- identity$marker
          varNameEscaped <- private$.escapeVar(varName)
          groupName <- identity$group
          dependentVar <- jmvcore::toNumeric(data[[varNameEscaped]][subGroup %in% groupName])
          classVar <- data[[classVarEscaped]][subGroup %in% groupName]
        }

        # Complete cases. An explicit NA level (addNA) is a missing class, not a negative one,
        # and only the levels present here count (the same set .rocOutcomeCoding() sees).
        classVar <- factor(classVar, exclude = NA)
        complete_cases <- !is.na(dependentVar) & !is.na(classVar) &
          classVar %in% c(self$options$negativeClass, self$options$positiveClass)
        dependentVar <- dependentVar[complete_cases]
        classVar <- droplevels(classVar[complete_cases])

        # The positive class was chosen and checked against the whole column in .run(); it is
        # never re-guessed here. A subgroup, or a marker's complete cases, can still lack it.
        positiveClass <- self$options$positiveClass
        if (!positiveClass %in% levels(classVar)) {
          if (is.null(subGroup)) {
            private$.throwError("missing_positive_class",
              sprintf(.("No case of the positive class (%1$s) has a value of %2$s, so %2$s cannot be analysed."), positiveClass, var),
              .("Check the missing values of this test variable, or remove it from the analysis."))
          }
          private$.throwError("missing_positive_class",
            sprintf(.("Subgroup %1$s has no case of the positive class (%2$s) with a value of %3$s, so %3$s cannot be analysed in that subgroup."), groupName, positiveClass, varName),
            .("Remove the subgroup variable, or use one in which every subgroup contains cases of the positive class."))
        }

        negativeClass <- self$options$negativeClass
        classVar <- private$.binaryResponse(classVar, positiveClass)

        # Check minimum sample sizes. One thin stratum rejects the whole analysis, so the message
        # names the marker, the subgroup and the short side (it was English-only and named none).
        pos_count <- sum(classVar == positiveClass)
        neg_count <- sum(classVar != positiveClass)

        if (pos_count < 2 || neg_count < 2) {
          details <- if (is.null(subGroup)) {
            if (pos_count < 2)
              sprintf(.("%1$s has a value for %2$d case(s) of the positive class (%3$s); at least 2 are needed to analyse it."), var, pos_count, positiveClass)
            else
              sprintf(.("%1$s has a value for %2$d case(s) of the negative class (%3$s); at least 2 are needed to analyse it."), var, neg_count, negativeClass)
          } else if (pos_count < 2) {
            sprintf(.("Subgroup %1$s has %2$d case(s) of the positive class (%3$s) with a value of %4$s; at least 2 are needed to analyse %4$s in that subgroup."), groupName, pos_count, positiveClass, varName)
          } else {
            sprintf(.("Subgroup %1$s has %2$d case(s) of the negative class (%3$s) with a value of %4$s; at least 2 are needed to analyse %4$s in that subgroup."), groupName, neg_count, negativeClass, varName)
          }
          private$.throwError("insufficient_data", details,
            if (is.null(subGroup)) .("Check the missing values of this test variable, or remove it from the analysis.")
            else .("Remove the subgroup variable, or use one in which every subgroup has at least 2 positive and 2 negative cases."))
        }

        # Nothing on the main path warned about small event counts. The only small-sample
        # notice in the analysis sits on the DeLong table, and the class-imbalance check
        # fires only outside 10%-90% prevalence, which 6 events in 40 passes - so an AUC of
        # 0.92 estimated from five cases was printed beside "Excellent" and looked exactly
        # like a 500-patient result. Ten per class is the conventional floor for a stable
        # AUC variance (Hanley & McNeil 1982). .warnUser() de-duplicates, so the repeated
        # calls to this method for the same variable produce one message.
        if (pos_count < 10 || neg_count < 10) {
          private$.warnUser(.fmt(
            .("'{var}' was analysed with only {pos} positive and {neg} negative case(s). Below about 10 per class the AUC, its confidence interval and the selected cutpoint are unstable; treat these results as exploratory."),
            var = var, pos = pos_count, neg = neg_count),
            type = jmvcore::NoticeType$STRONG_WARNING)
        }

        list(dependentVar = dependentVar, classVar = classVar)
      },

      # Run cutpointr and return results with confusion matrix
      .runCutpointrMetrics = function(dependentVar, classVar, positiveClass,
                                      method, score, metric, direction,
                                      tol_metric, boot_runs, break_ties,
                                      var_label = .("this marker")) {
        result_success <- FALSE
        result_message <- NULL
        results <- NULL

        result_message <- tryCatch(
          {
            results <- cutpointr::cutpointr(
              x = dependentVar,
              class = classVar,
              subgroup = NULL,
              method = method,
              cutpoint = score,
              metric = metric,
              direction = direction,
              pos_class = positiveClass,
              tol_metric = tol_metric,
              boot_runs = boot_runs,
              break_ties = break_ties,
              na.rm = TRUE
            )
            result_success <- TRUE
            NULL
          },
          error = function(e) e$message
        )

        if (!result_success) {
          # Fallback: manual ROC calculation. Say so -- it ignores the selected method,
          # metric and tolerance, and used to replace them silently.
          private$.warnUser(sprintf(
            .("cutpointr could not analyse %1$s (%2$s). A built-in Youden-index search was used instead, so the selected cutpoint method, metric and tolerance were not applied to that marker."),
            var_label, result_message))
          response <- as.numeric(classVar == positiveClass)
          # Emit the fallback curve with cutpointr's OWN geometry so it is interchangeable
          # with it for every downstream consumer (fixed sensitivity/specificity selection,
          # the plot state builders). Verified against cutpointr: the curve starts at an
          # infinite endpoint where nothing is called positive (sensitivity 0, specificity
          # 1) and then runs so that sensitivity increases and specificity decreases - that
          # is +Inf then DESCENDING values for direction ">=", -Inf then ASCENDING values
          # for "<=". This used to be plain ascending unique values with no endpoint, which
          # truncated the plotted curve at the observed extremes and sent the fixed
          # sensitivity/specificity search to the wrong end of the range.
          uniqueValues <- sort(unique(dependentVar))
          roc_data <- data.frame(
            x.sorted = if (direction == ">=") c(Inf, rev(uniqueValues)) else c(-Inf, uniqueValues),
            direction = ifelse(direction == ">=", ">", "<")
          )
          for (i in seq_len(nrow(roc_data))) {
            threshold <- roc_data$x.sorted[i]
            if (direction == ">=") {
              predicted_pos <- dependentVar >= threshold
            } else {
              predicted_pos <- dependentVar <= threshold
            }

            roc_data$tp[i] <- sum(predicted_pos & response == 1)
            roc_data$fp[i] <- sum(predicted_pos & response == 0)
            roc_data$tn[i] <- sum(!predicted_pos & response == 0)
            roc_data$fn[i] <- sum(!predicted_pos & response == 1)
          }

          sens <- ifelse((roc_data$tp + roc_data$fn) > 0, roc_data$tp / (roc_data$tp + roc_data$fn), 0)
          spec <- ifelse((roc_data$tn + roc_data$fp) > 0, roc_data$tn / (roc_data$tn + roc_data$fp), 0)
          roc_points <- data.frame(specificity = spec, sensitivity = sens)
          roc_points <- roc_points[order(1 - roc_points$specificity), ]
          auc <- 0
          for (i in 2:nrow(roc_points)) {
            x_diff <- (1 - roc_points$specificity[i]) - (1 - roc_points$specificity[i - 1])
            y_avg <- (roc_points$sensitivity[i] + roc_points$sensitivity[i - 1]) / 2
            auc <- auc + x_diff * y_avg
          }
          youdens_j <- sens + spec - 1
          optimal_idx <- which.max(youdens_j)
          results <- list(
            optimal_cutpoint = roc_data$x.sorted[optimal_idx],
            roc_curve = list(roc_data),
            AUC = auc
          )
        }

        confusionMatrix <- results$roc_curve[[1]]
        list(results = results, confusionMatrix = confusionMatrix)
      },

      # Generate tables with metrics for each threshold
      # This was the missing method that was causing errors
      .generateTables = function(var, results, confusionMatrix, resultsToDisplay,
                                 dependentVar, classVar, positiveClass, direction, metric,
                                 classLabels) {
        # Initialize lists to store metrics
        sensList <- numeric()
        specList <- numeric()
        ppvList <- numeric()
        npvList <- numeric()
        youdenList <- numeric()
        metricList <- numeric()

        # Calculate prevalence
        n_pos <- sum(classVar == positiveClass, na.rm = TRUE)
        n_neg <- sum(classVar != positiveClass, na.rm = TRUE)
        prevalence <- if ((n_pos + n_neg) > 0) n_pos / (n_pos + n_neg) else NA_real_

        # Keep the observed value before any prior overwrites it - accuracy needs it.
        private$.samplePrevalenceList[[var]] <- prevalence

        # Override with prior prevalence if requested
        if (self$options$usePriorPrev) {
          prevalence <- self$options$priorPrev
        }

        # Store prevalence for this variable
        private$.prevalenceList[[var]] <- prevalence

        # Process each threshold
        for (i in seq_along(confusionMatrix$x.sorted)) {
          tp <- confusionMatrix$tp[i]
          fp <- confusionMatrix$fp[i]
          tn <- confusionMatrix$tn[i]
          fn <- confusionMatrix$fn[i]

          # Handle missing values by replacing with 0
          tp <- ifelse(is.na(tp), 0, tp)
          fp <- ifelse(is.na(fp), 0, fp)
          tn <- ifelse(is.na(tn), 0, tn)
          fn <- ifelse(is.na(fn), 0, fn)

          # Calculate metrics with safe denominators
          sensitivity <- if (!is.na(tp + fn) && (tp + fn) > 0) tp / (tp + fn) else 0
          specificity <- if (!is.na(tn + fp) && (tn + fp) > 0) tn / (tn + fp) else 0

          # PPV/NPV: use Bayes' theorem when prior prevalence is specified
          if (self$options$usePriorPrev && !is.null(prevalence) && prevalence > 0 && prevalence < 1) {
            ppv <- (sensitivity * prevalence) /
              (sensitivity * prevalence + (1 - specificity) * (1 - prevalence))
            npv <- (specificity * (1 - prevalence)) /
              (specificity * (1 - prevalence) + (1 - sensitivity) * prevalence)
          } else {
            ppv <- if (!is.na(tp + fp) && (tp + fp) > 0) tp / (tp + fp) else NA_real_
            npv <- if (!is.na(tn + fn) && (tn + fn) > 0) tn / (tn + fn) else NA_real_
          }
          youden <- sensitivity + specificity - 1

          # Store metrics
          sensList <- c(sensList, sensitivity)
          specList <- c(specList, specificity)
          ppvList <- c(ppvList, ppv)
          npvList <- c(npvList, npv)
          youdenList <- c(youdenList, youden)

          # Calculate the optimization metric value
          metric_value <- 0
          if (!is.null(metric)) {
            metric_value <- tryCatch(
              {
                metric(tp, fp, tn, fn)
              },
              error = function(e) {
                NA
              }
            )
          }
          metricList <- c(metricList, metric_value)
        }

        # Plots describe the selected cutoff. For all tied cutpoints use the first listed.
        selected_cut <- unlist(results$optimal_cutpoint)[1]
        j_max_idx <- private$.thresholdIndex(confusionMatrix$x.sorted, selected_cut, direction)

        # Store optimal criteria for this variable
        if (length(j_max_idx) > 0 && is.finite(selected_cut)) {
          private$.optimalCriteriaList[[var]] <- list(
            threshold = selected_cut,
            sensitivity = sensList[j_max_idx],
            specificity = specList[j_max_idx],
            ppv = ppvList[j_max_idx],
            npv = npvList[j_max_idx],
            youden = youdenList[j_max_idx]
          )
        } else {
          private$.optimalCriteriaList[[var]] <- NULL
        }

        # Store ROC data for plotting
        private$.rocDataList[[var]] <- data.frame(
          threshold = confusionMatrix$x.sorted,
          sensitivity = sensList,
          specificity = specList,
          ppv = ppvList,
          npv = npvList,
          youden = youdenList,
          metric = metricList,
          selected_cutpoint = rep(selected_cut, length(sensList)),
          stringsAsFactors = FALSE
        )

        # Add raw data as attribute for confidence band calculations
        attr(private$.rocDataList[[var]], "rawData") <- data.frame(
          value = dependentVar,
          class = ifelse(classVar == positiveClass, "Positive", "Negative"),
          stringsAsFactors = FALSE
        )

        # Populate results table
        resultsTable <- self$results$resultsTable$get(key = var)
        private$.noteMetricTolerance(resultsTable)
        private$.noteCutpointOptimism(resultsTable)
        # the bootstrap-metric methods choose the cutpoint over random resamples
        resultsTable$setNote("seed", if (self$options$method %in% c("maximize_boot_metric", "minimize_boot_metric"))
          .fmt(.("Random seed: {seed}"), seed = self$options$seed), init = FALSE)
        # This is the table that actually shows PPV/NPV, and it is visible by
        # default -- so the prevalence caveat belongs here, not only inside the
        # optional Fixed Sensitivity/Specificity panel.
        private$.notePredictiveValues(resultsTable, private$.prevalenceList[[var]])

        # Clear existing rows
        .deleteTableRows(resultsTable)

        # A non-finite cutpoint is not an operating point a clinician can apply to a patient.
        # It appears when no threshold achieves a positive Youden index - typically because the
        # wrong level is the Positive Class, or the Classification Direction is backwards for
        # this marker - and the row then reads "cutpoint Inf, sensitivity 0, PPV 0", which is
        # worse than showing nothing. Say what happened instead of printing it, Positive Class
        # first as in .noteDirection(). NULL removes a note restored from the previous run.
        # When some threshold does have J > 0 (exactly, from the counts: tp*n0 + tn*n1 > n1*n0),
        # it was the method that put the cut at +/-Inf (accuracy, cost or utility in an
        # imbalanced sample, oc_cost_ratio, or a Metric Tolerance wide enough to pull the
        # infinite end into the tied set, whose mean is then +/-Inf), and "nothing beats
        # chance" would be false. That end is where nobody is called positive (cutpointr's
        # +Inf for ">=", -Inf for "<="), so the note can say "classified negative".
        cm <- lapply(confusionMatrix[c("tp", "fp", "tn", "fn")], as.numeric)   # doubles: n1 * n0 can pass the integer limit
        separates <- any(cm$tp * (cm$tn + cm$fp) + cm$tn * (cm$tp + cm$fn) > (cm$tp + cm$fn) * (cm$tn + cm$fp), na.rm = TRUE)
        resultsTable$setNote(
          key = "no_usable_cutpoint",
          note = if (!any(is.finite(resultsToDisplay))) {
            if (separates)
              .("No usable cut-point is shown for this marker: the cut-point chosen by the selected method lies beyond the observed values, so every patient would be classified negative, although some cut-points do separate the groups better than chance in the classification direction currently selected. This happens, for example, when accuracy, a misclassification cost or a total utility is optimised in an imbalanced sample, when at the cost ratio entered the cost-benefit method finds classifying every patient negative the least costly choice, or when Metric Tolerance is wide enough to count that choice as tied with the best cut-point. If a cut-point is needed, lower Metric Tolerance or choose another Cutpoint Method or Optimization Metric. The AUC and its confidence interval in the Area Under the ROC Curve table are unaffected.")
            else
              sprintf(.("No usable cut-point exists for this marker in the classification direction currently selected: no threshold separates the groups better than chance. First check Positive Class: is %1$s the level that means the condition is present? Only if it is, check Classification Direction. The AUC and its confidence interval in the Area Under the ROC Curve table are unaffected."), .rocLabel(positiveClass))
          },
          init = FALSE
        )

        # A row must hold what the rule "x >= cut" (or "<=") gives at its cut-point. The cut-point
        # need not be an observed value (mean or median of tied optima, oc_youden_kernel/normal,
        # oc_manual), and the nearest observed value - the old lookup - can lie on the other side
        # of it: cut-point 9.333 printed the sensitivity of x >= 9 (0.6), not of x >= 9.333 (0.5).
        # Use exact comparisons; a large additive offset must not enlarge a snapping tolerance.
        xs <- confusionMatrix$x.sorted
        snap <- 4 * .Machine$double.eps * max(abs(xs[is.finite(xs)]))
        ruleIdx <- function(cut) private$.thresholdIndex(xs, cut, direction)

        # Tied optima are summarised by Tie Breaking Method, and their mean or median is not one of
        # them and can score lower than each (optima 8, 9 and 11 with J 0.5; mean 9.333 with J 0.4).
        # Keep the user's choice but say so. Above the default tolerance .noteMetricTolerance()
        # already explains the averaging.
        cut1 <- unlist(results$optimal_cutpoint)[1]
        tol <- self$options$tol_metric
        tieNote <- NULL
        if (self$options$method %in% c("maximize_metric", "minimize_metric") &&
            self$options$break_ties %in% c("mean", "median") && isTRUE(is.finite(cut1)) &&
            isTRUE(tol <= 1e-05) && any(is.finite(metricList))) {
          fin <- metricList[is.finite(metricList)]
          best <- if (self$options$method == "maximize_metric") max(fin) else min(fin)
          tied <- xs[which(abs(metricList - best) <= tol)]
          shown <- if (self$options$break_ties == "mean") mean(tied) else stats::median(tied)
          at <- ruleIdx(cut1)
          # the note only when these ARE cutpointr's ties and the cut-point is none of them
          if (length(tied) > 1 && length(at) && isTRUE(abs(shown - cut1) <= snap) && !any(abs(tied - cut1) <= snap))
            tieNote <- sprintf(.("Several cut-points tie for the best %1$s (%2$s): %3$s. The cut-point shown, %4$s, is computed from them and is not one of them; its %1$s is %5$s. Choose 'All tied cutpoints' in Tie Breaking Method to list each one."),
              self$options$metric, signif(best, 3), paste(signif(sort(tied), 4), collapse = ", "),
              signif(cut1, 4), signif(metricList[at], 3))
        }
        resultsTable$setNote("tied_optima", tieNote, init = FALSE)

        # Add rows for each threshold to display
        for (row_index in seq_along(resultsToDisplay)) {
          threshold <- resultsToDisplay[row_index]
          idx <- ruleIdx(threshold)

          if (length(idx) > 0 && is.finite(threshold)) {
            # Add row to results table
            resultsTable$addRow(rowKey = paste0("cutpoint_", row_index), values = list(
              cutpoint = threshold,
              sensitivity = sensList[idx],
              specificity = specList[idx],
              ppv = ppvList[idx],
              npv = npvList[idx],
              youden = youdenList[idx],
              AUC = results$AUC,
              metricValue = metricList[idx]
            ))
          }
        }

        # Populate sensitivity/specificity table if requested
        if (self$options$sensSpecTable) {
          sensSpecTable <- self$results$sensSpecTable$get(key = var)

          # One matrix per reported cut-point, read with the Results rows' rule. 'All tied
          # cutpoints' makes optimal_cutpoint a list column (a Results row per tie), and the
          # arithmetic on its [1] aborted the whole analysis.
          html <- vapply(unlist(results$optimal_cutpoint), function(cut) {
            i <- ruleIdx(cut)
            if (!length(i)) return("")
            private$.formatSensSpecTable(
              Title = paste("Confusion Matrix for", var, "at Selected Cutpoint =", round(cut, 3)),
              TP = as.numeric(confusionMatrix$tp[i]), FP = as.numeric(confusionMatrix$fp[i]),
              TN = as.numeric(confusionMatrix$tn[i]), FN = as.numeric(confusionMatrix$fn[i]),
              labels = classLabels)
          }, character(1))
          sensSpecTable$setContent(paste(html, collapse = "<br>"))
        }

        return(list(
          sensList = sensList,
          specList = specList,
          ppvList = ppvList,
          npvList = npvList,
          youdenList = youdenList,
          metricList = metricList,
          j_max_idx = if (is.finite(selected_cut) && length(j_max_idx)) j_max_idx else NA_integer_
        ))
      },

      # Calculate precision-recall curve
      .calculatePrecisionRecall = function(x, class, positiveClass) {
        pr <- .dvPR(x, class, positiveClass, self$options$direction == ">=")
        if (is.null(pr)) return(list(threshold = numeric(), precision = numeric(),
                                    recall = numeric(), auprc = NA_real_))
        c(as.list(pr$curve), list(auprc = pr$auprc, avg_precision = pr$avg_precision))
      },

      # Calculate comprehensive performance metrics for classifier comparison
      .calculateClassifierMetrics = function(x, class, positiveClass) {
        # Check package dependencies
        if (!private$.checkPackageDependencies("pROC", "classifier comparison")) {
          return(NULL)
        }

        # Create ROC object with direction matching user setting
        pROC_direction <- ifelse(self$options$direction == ">=", "<", ">")
        roc_obj <- pROC::roc(
          response = private$.binaryResponse(class, positiveClass),
          predictor = x,
          direction = pROC_direction,
          quiet = TRUE
        )

        # Calculate AUC
        auc <- pROC::auc(roc_obj)

        # Calculate precision-recall
        pr_results <- private$.calculatePrecisionRecall(x, class, positiveClass)
        auprc <- pr_results$auprc

        # Convert to binary response (1 = positive, 0 = negative)
        response <- as.numeric(class == positiveClass)

        # Find optimal threshold using Youden's index
        coords <- pROC::coords(roc_obj, x = "best", best.method = "youden")
        # One classifier per marker: among tied Youden optima choose greatest sensitivity.
        threshold <- coords$threshold[which.max(coords$sensitivity)]

        # Get predictions using optimal threshold (respect direction)
        predicted_prob <- roc_obj$predictor
        if (pROC_direction == "<") {
          # Higher values = positive
          predicted_class <- ifelse(predicted_prob >= threshold, 1, 0)
        } else {
          # Lower values = positive
          predicted_class <- ifelse(predicted_prob <= threshold, 1, 0)
        }

        # Calculate Brier score - requires calibrated probabilities in [0,1]
        # If raw values are not in [0,1], Brier score is not meaningful
        if (all(predicted_prob >= 0 & predicted_prob <= 1, na.rm = TRUE)) {
          brier_score <- mean((predicted_prob - response)^2)
        } else {
          brier_score <- NA # Brier score not applicable for uncalibrated test values
        }

        # Calculate confusion matrix metrics
        tp <- sum(predicted_class == 1 & response == 1)
        tn <- sum(predicted_class == 0 & response == 0)
        fp <- sum(predicted_class == 1 & response == 0)
        fn <- sum(predicted_class == 0 & response == 1)

        # Calculate F1 score
        precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
        recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
        f1_score <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)

        # Calculate accuracy
        total <- tp + tn + fp + fn
        accuracy <- if (total > 0) (tp + tn) / total else NA

        # Calculate balanced accuracy
        sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else 0
        specificity <- if ((tn + fp) > 0) tn / (tn + fp) else 0
        balanced_accuracy <- (sensitivity + specificity) / 2

        return(list(
          auc = as.numeric(auc),
          auprc = auprc,
          brier = brier_score,
          f1_score = f1_score,
          accuracy = accuracy,
          balanced_accuracy = balanced_accuracy
        ))
      },

      # A generalized inverse must not discard an unestimable nonzero contrast.
      .delongGlobalInference = function(difference, covariance) {
        unavailable <- list(statistic = NA_real_, p = NA_real_)
        if (any(!is.finite(covariance)) || any(!is.finite(difference)))
          return(unavailable)
        decomposition <- eigen((covariance + t(covariance)) / 2, symmetric = TRUE)
        tolerance <- sqrt(.Machine$double.eps) * max(abs(decomposition$values))
        keep <- decomposition$values > tolerance
        if (!any(keep) || any(decomposition$values < -tolerance)) return(unavailable)
        coordinates <- as.numeric(crossprod(decomposition$vectors, difference))
        if (any(abs(coordinates[!keep]) > sqrt(.Machine$double.eps) *
            max(1, abs(difference)))) return(unavailable)
        statistic <- sum(coordinates[keep]^2 / decomposition$values[keep])
        list(statistic = statistic,
          p = stats::pchisq(statistic, df = sum(keep), lower.tail = FALSE))
      },

      .delongPairInference = function(difference, covariance, quantile) {
        variance <- covariance[1, 1] + covariance[2, 2] - 2 * covariance[1, 2]
        tolerance <- 64 * .Machine$double.eps * max(abs(covariance))
        if (!is.finite(variance) || variance <= tolerance)
          return(list(ci = c(NA_real_, NA_real_), z = NA_real_, p = NA_real_))
        se <- sqrt(variance)
        z <- difference / se
        list(ci = difference + c(-1, 1) * quantile * se,
          z = z, p = 2 * stats::pnorm(-abs(z)))
      },

      # Perform DeLong's test for comparing AUCs
      # Moved from outside the class to inside as a private method
      .deLongTest = function(data, classVar, positiveClass, ref = NULL, conf.level = private$.CONFIDENCE_LEVEL) {
        # Validate and prepare inputs
        # Convert factor to character first to handle labels safely
        if (is.factor(classVar)) {
          classVar <- as.character(classVar)
        }

        # positiveClass is the user's choice, never reinterpreted (as a position, or by data
        # order); a class with no rows of it is refused below.

        # Check for NA values in classification variable
        if (any(is.na(classVar))) {
          jmvcore::reject(.("Classification variable contains missing values (NA). Please remove or handle missing values before performing ROC analysis."))
        }

        # Check if positive class exists in the data
        id.pos <- classVar == positiveClass
        if (sum(id.pos, na.rm = TRUE) < 1) {
          jmvcore::reject(.("Wrong level specified for positive class. No observations found."))
        }

        # Check data dimensions
        if (dim(data)[2] < 2) {
          jmvcore::reject(.("Data must contain at least two columns (different measures)."))
        }
        if (dim(data)[1] < 2) {
          jmvcore::reject(.("Data must contain at least two rows (observations)."))
        }

        # Get counts of positive and negative cases
        nn <- sum(!id.pos) # Number of negative cases
        np <- sum(id.pos) # Number of positive cases
        nauc <- ncol(data) # Number of tests

        # Set up comparison matrix based on reference or pairwise
        if (is.null(ref)) {
          # Create matrix for all pairwise comparisons
          L <- matrix(0, nrow = nauc * (nauc - 1) / 2, ncol = nauc)
          newa <- 0
          for (i in 1:(nauc - 1)) {
            newl <- nauc - i
            L[(newa + 1):(newa + newl), i] <- rep(1, newl)
            L[(newa + 1):(newa + newl), ((i + 1):(i + newl))] <-
              diag(-1, nrow = newl, ncol = newl)
            newa <- newa + newl
          }
        } else {
          # Create matrix for comparing all tests against a reference
          if (ref > nauc) {
            jmvcore::reject(paste("Reference ref must be one of the markers (1...", nauc, " in this case)", sep = ""))
          }
          L <- matrix(1, ncol = nauc, nrow = nauc - 1)
          L[, -ref] <- diag(-1, nrow = nauc - 1, ncol = nauc - 1)
        }

        # The Wilcoxon statistic below counts HIGHER values as positive. Read the markers in the
        # user's Classification Direction first, as the main AUC table and .enhancedDelongTest do;
        # otherwise under "<=" every AUC came out as max(AUC, 1 - AUC) and `inverted` named the
        # wrong markers.
        if (identical(self$options$direction, "<=")) data <- -data

        # Split data into positive and negative cases
        markern <- as.matrix(data[!id.pos, ])
        markerp <- as.matrix(data[id.pos, ])

        # Function to compute the Wilcoxon statistic
        WK.STAT <- function(data, y) {
          r <- rank(c(data, y))
          n.data <- length(data)
          n.y <- length(y)
          STATISTIC <- sum(r[seq_along(data)]) - n.data * (n.data + 1) / 2
          STATISTIC
        }

        # Calculate AUC for each test using Wilcoxon statistic
        auc <- vector("numeric", length = nauc)
        for (r in 1:nauc) {
          auc[r] <- WK.STAT(markerp[, r], markern[, r])
        }
        auc <- auc / (nn * np) # Normalize to [0,1]

        # Below-chance AUCs remain below chance: the declared direction defines the estimand.
        inverted_vars <- character(0)

        # Calculate placement values for covariance estimation
        V10 <- matrix(0, nrow = np, ncol = nauc)
        V01 <- matrix(0, nrow = nn, ncol = nauc)

        tmn <- t(markern)
        tmp <- t(markerp)

        # Calculate placement values for each positive case
        for (i in 1:np) {
          V10[i, ] <- rowSums(tmn < tmp[, i]) + 0.5 * rowSums(tmn == tmp[, i])
        }

        # Calculate placement values for each negative case
        for (i in 1:nn) {
          V01[i, ] <- rowSums(tmp > tmn[, i]) + 0.5 * rowSums(tmp == tmn[, i])
        }

        # Normalize placement values
        V10 <- V10 / nn
        V01 <- V01 / np

        # Calculate covariance matrices
        W10 <- cov(V10)
        W01 <- cov(V01)

        # Estimated covariance matrix for AUCs
        S <- W10 / np + W01 / nn

        # Compute variances of AUCs using Hanley & McNeil (1982) formula
        q1 <- auc / (2 - auc)
        q2 <- 2 * auc^2 / (1 + auc)

        # Calculate standard errors and p-values (against null hypothesis AUC = 0.5)
        aucvar <- (auc * (1 - auc) + (np - 1) * (q1 - auc^2) + (nn - 1) * (q2 - auc^2)) / (np * nn)
        zhalf <- ifelse(aucvar > 0, (auc - 0.5) / sqrt(aucvar), NA)
        phalf <- ifelse(!is.na(zhalf), 2 * pnorm(-abs(zhalf)), NA)
        diag_S <- diag(S)
        zdelong <- ifelse(diag_S > 0, (auc - 0.5) / sqrt(diag_S), NA)
        pdelong <- ifelse(!is.na(zdelong), 2 * pnorm(-abs(zdelong)), NA)

        # Global test for difference between AUCs
        aucdiff <- L %*% auc
        global <- private$.delongGlobalInference(aucdiff, L %*% S %*% t(L))
        z <- global$statistic
        p <- global$p

        # Calculate confidence intervals for pairwise differences
        quantil <- qnorm(1 - (1 - conf.level) / 2)
        if (is.null(ref)) {
          # All pairwise comparisons
          cor.auc <- matrix(ncol = 1, nrow = nauc * (nauc - 1) / 2)
          ci <- matrix(ncol = 2, nrow = nauc * (nauc - 1) / 2)
          ctr <- 1
          rows <- vector("character", length = (nauc * (nauc - 1) / 2))
          pair_labels <- character(length(rows))
          pair_z <- numeric(length(rows))
          pairp <- matrix(nrow = nauc * (nauc - 1) / 2, ncol = 1)
          quantil <- qnorm(1 - (1 - conf.level) / 2)

          for (i in 1:(nauc - 1)) {
            for (j in (i + 1):nauc) {
              # Calculate correlation between AUCs
              denom_cor <- S[i, i] * S[j, j]
              cor.auc[ctr] <- if (denom_cor > 0) S[i, j] / sqrt(denom_cor) else NA

              # Calculate confidence interval for the difference
              pair <- private$.delongPairInference(aucdiff[ctr], S[c(i, j), c(i, j)], quantil)
              pairp[ctr] <- pair$p
              pair_z[ctr] <- pair$z
              ci[ctr, ] <- pair$ci
              rows[ctr] <- .psychopdaPairKey(i, j)
              pair_labels[ctr] <- .psychopdaPairLabel(colnames(data)[i], colnames(data)[j])
              ctr <- ctr + 1
            }
          }
        } else {
          # Comparisons against a reference
          cor.auc <- matrix(ncol = 1, nrow = nauc - 1)
          ci <- matrix(ncol = 2, nrow = nauc - 1)
          rows <- vector("character", length = nauc - 1)
          pair_labels <- character(length(rows))
          pair_z <- numeric(length(rows))
          pairp <- matrix(nrow = nauc - 1, ncol = 1)
          comp <- (1:nauc)[-ref]

          for (i in 1:(nauc - 1)) {
            # Calculate correlation between reference and current AUC
            denom_cor_i <- S[ref, ref] * S[comp[i], comp[i]]
            cor.auc[i] <- if (denom_cor_i > 0) S[ref, comp[i]] / sqrt(denom_cor_i) else NA

            # Calculate confidence interval for the difference
            pair <- private$.delongPairInference(aucdiff[i],
              S[c(ref, comp[i]), c(ref, comp[i])], quantil)
            pairp[i] <- pair$p
            pair_z[i] <- pair$z
            ci[i, ] <- pair$ci
            rows[i] <- .psychopdaPairKey(ref, comp[i])
            pair_labels[i] <- .psychopdaPairLabel(colnames(data)[ref], colnames(data)[comp[i]])
          }
        }

        # Format results for return
        newres <- as.data.frame(cbind(aucdiff, ci, pairp, cor.auc))
        names(newres) <- c("AUC Difference", "CI(lower)", "CI(upper)", "P.Value", "Correlation")
        newres$Z <- pair_z
        rownames(newres) <- rows

        row.names(ci) <- row.names(cor.auc) <- row.names(aucdiff) <- row.names(pairp) <- rows
        colnames(ci) <- c(paste0(100 * conf.level, "% CI (lower)"), paste0(100 * conf.level, "% CI (upper)"))

        names(auc) <- colnames(data)
        auc <- as.data.frame(cbind(auc, sqrt(aucvar), phalf, sqrt(diag(S)), pdelong))
        colnames(auc) <- c("AUC", "SD(Hanley)", "P(H0: AUC=0.5)", "SD(DeLong)", "P(H0: AUC=0.5)")

        # Prepare return object
        ERG <- list(
          AUC = auc,
          difference = newres,
          comparison_labels = pair_labels,
          covariance = S,
          global.z = z,
          global.p = p,
          # Retained for compatibility; no score is automatically reflected.
          inverted = inverted_vars
        )
        class(ERG) <- "DeLong"
        return(ERG)
      },

      # ============================================================================
      # INITIALIZATION METHOD
      # ============================================================================

      # Maintenance note: keep feature-specific calculations behind private helpers and add
      # parity coverage before extracting them from this large class. AUC, DeLong, cutpoint,
      # IDI/NRI and optional-plot paths have dedicated release-review tests.

      .showWelcome = function() {
        self$results$instructions$setVisible(TRUE)
        self$results$instructions$setContent(paste0(
          "<div class='jmv-welcome' style='background-color: rgba(37, 99, 235, 0.08); ",
          "border-left: 4px solid #93c5fd; padding: 16px; color: inherit;'>",
          "<h3>", .("Advanced ROC Analysis"), "</h3>",
          "<p>", .("Compare diagnostic markers and select cutpoints for explicitly selected positive and negative classes."), "</p>",
          "<ol><li>", .("Select one or more numeric Test Variables."), "</li>",
          "<li>", .("Select the Class Variable (Gold Standard), then explicitly choose Positive Class and Negative Class."), "</li>",
          "<li>", .("Check Classification Direction. Unselected outcome levels and missing outcomes are excluded and counted."), "</li>",
          "<li>", .("Choose any optional tables, ROC curves, or comparisons; checkboxes start unchecked."), "</li></ol>",
          "<p>", .("Each row must be an independent observation. Repeated measurements from the same patient require a method that accounts for clustering."), "</p>",
          "<p>", .("Cutpoints selected on these data require external validation before clinical use."), "</p>",
          "</div>"))
      },

      # Initialize the analysis
      .init = function() {
        # Visibility is expressed declaratively in jamovi/psychopdaroc.r.yaml
        # (`visible: (showCriterionPlot)`, `visible: (metaAnalysis)`, ...) and is
        # NOT repeated here. An imperative setVisible(FALSE) in .init() overrides
        # that binding for the run, so it has to be undone by a matching
        # setVisible(TRUE) in .run() -- and the two halves drift. They did:
        # criterionPlot, prevalencePlot, dotPlot and precisionRecallPlot were
        # hidden here and never restored, so those four options computed a plot
        # and silently threw it away. The declarative form cannot drift.

        # The clinical use-case preset option was REMOVED: its handler wrote
        # guidance into self$results$instructions from .init(), which is then
        # overwritten, so all four presets were a byte-identical no-op (confirmed
        # by full-output diff). The text it tried to show said itself that it
        # changed no analysis setting, so nothing was lost. Use Method / Metric.
        #
        # .applyClinicalModeSettings() was called HERE and had exactly the same defect:
        # the getting-started setContent() below overwrote it. It is now called from
        # .run(), which appends its text to Procedure Notes.

        # Initialize advanced plot states (enable rendering when data is ready)
        # These plots have renderFun defined in .r.yaml but need setState to activate
        if (length(self$options$dependentVars) > 0) {
          # Effect size plots
          if (self$options$effectSizeAnalysis && length(self$options$dependentVars) > 1) {
            # ONE item, not one per test variable. .plotEffectSize() ignores its item key
            # and rebuilds the same all-pairs bar chart from effectSizeTable every time, so
            # N items drew N identical charts - each headed with a single marker's name
            # while containing comparisons that marker is not part of (a panel titled
            # 'Ki-67' showing a bar for 'p53 vs CD8').
            #
            # clear() first, not an itemKeys guard: .init() re-runs on EVERY option change
            # (Analysis$run() calls init() whenever status != "inited"), addItem() has no
            # duplicate check, and this also sweeps away per-variable items added by an
            # earlier run of this same analysis object.
            self$results$effectSizePlot$clear()
            self$results$effectSizePlot$addItem(key = "all")
            image <- self$results$effectSizePlot$get(key = "all")
            if (!is.null(image)) image$setState(list(ready = FALSE))
          }
        }

        # Set up initial instructions - show by default until variables are provided
        private$.showWelcome()

        # .init() re-runs on EVERY option change (jmvcore's optionsChangedHandler sets
        # status to "none" and Analysis$run() then calls init() again) on the same object,
        # against results that persist - and Table$addRow() has no duplicate check. This
        # scaffold therefore stacked a fresh Fixed/Random pair on every option change,
        # while setRow() only ever reaches the FIRST match, so the later pairs showed stale
        # or blank pooled AUCs. Rebuilding the row set from scratch also drops a "random"
        # row left over from a previous method choice, which .blankMetaAnalysisRows()
        # (keyed on the CURRENT labels) would otherwise never clear.
        .deleteTableRows(self$results$metaAnalysisTable)
        metaLabels <- private$.metaAnalysisRowLabels()
        for (key in names(metaLabels))
          self$results$metaAnalysisTable$addRow(rowKey = key,
            values = list(model_type = unname(metaLabels[[key]])))

        # Enable meta-analysis options only when sufficient variables are available
        private$.updateMetaAnalysisVisibility()
      },

      # Resolve both classes once, before computing anything; show every exclusion explicitly.
      .resolveOutcomeClasses = function(classVar) {
        coding <- .psychopdaClassCoding(classVar, self$options$positiveClass,
          self$options$negativeClass)
        table <- self$results$outcomeCoding
        .deleteTableRows(table)
        for (key in c("unset", "positive", "source")) table$setNote(key, NULL)
        for (i in seq_along(coding$levels)) {
          level <- coding$levels[i]
          role <- if (!identical(coding$status, "selected")) .("Not assigned yet")
            else if (level == coding$positive) .("Positive (condition present)")
            else if (level == coding$negative) .("Negative (condition absent)")
            else .("Excluded: unselected outcome level")
          table$addRow(rowKey = paste0("level_", i), values = list(
            level = .rocLabel(level, note = FALSE), n = coding$counts[i], role = role))
        }
        if (coding$n_missing > 0)
          table$addRow(rowKey = "missing", values = list(level = .("(missing)"),
            n = coding$n_missing, role = .("Excluded: missing outcome")))
        if (identical(coding$status, "unset"))
          table$setNote("unset", .("Choose distinct observed levels in Positive Class and Negative Class. Nothing is computed until both are selected; levels are never chosen from their order."))
        if (identical(coding$status, "selected")) {
          table$setNote("positive", sprintf(.("Positive class: %1$s. Negative class: %2$s. Sensitivity, PPV, LR+, every cutpoint and the AUC describe detection of %1$s relative to %2$s. Other outcome levels are excluded."),
            .rocLabel(coding$positive), .rocLabel(coding$negative)))
          table$setNote("source", .("Chosen in Positive Class and Negative Class."))
        }
        table$setNote("n", .("N counts every row with each outcome value before exclusions. Each test is analysed on its complete cases within the two selected classes."))
        coding
      },

      # State, in one plain sentence, which way the marker was read. Without this a user
      # comparing this analysis against enhancedROC (same menu, same data) sees two different
      # AUCs -- 0.8999 and 0.1001 on an inverted marker -- with nothing on screen explaining
      # that the two simply read the marker in opposite directions.
      .noteDirection = function(table, key = "direction_used") {
        dir <- tryCatch(self$options$direction, error = function(e) NULL)
        if (is.null(dir) || !nzchar(dir)) return(invisible(NULL))
        pos <- .rocLabel(self$options$positiveClass)
        higher <- identical(dir, ">=")
        # Translatable: the msgid must be a string LITERAL, and HIGHER/LOWER plus the
        # opposite direction symbol cannot be spliced through %s (the words carry grammar
        # and the two halves would have to be translated blind), so the two readings are
        # written out as two complete alternative sentences.
        # Positive Class comes first: naming the wrong level reverses the reading too. Flipping
        # Direction re-estimates the cutpoint, sensitivity and specificity (both directions
        # include the cutpoint itself), so they are not simply swapped or complemented.
        # <i>, not <b>: the client's table-note renderer keeps only em, i, sub and sup.
        note <- if (higher) {
          sprintf(.("Reading of the test values: <i>HIGHER values were taken to indicate %1$s</i> (Classification Direction = \"%2$s\"). If that reading looks reversed for this marker, first check Positive Class: is %1$s the level that means the condition is present? Only if it is, switch Classification Direction to \"<=\": the AUC becomes 1 minus the value shown, and the cutpoint, sensitivity and specificity are re-estimated."), pos, dir)
        } else {
          sprintf(.("Reading of the test values: <i>LOWER values were taken to indicate %1$s</i> (Classification Direction = \"%2$s\"). If that reading looks reversed for this marker, first check Positive Class: is %1$s the level that means the condition is present? Only if it is, switch Classification Direction to \">=\": the AUC becomes 1 minus the value shown, and the cutpoint, sensitivity and specificity are re-estimated."), pos, dir)
        }
        tryCatch(table$setNote(key, note), error = function(e) NULL)
      },

      # A non-trivial metric tolerance makes cutpointr treat every cutpoint within that
      # distance of the best metric value as equivalent and average them, so the reported
      # cutpoint is no longer the one that maximises the metric. Say so next to it.
      .noteMetricTolerance = function(table) {
        tol <- tryCatch(self$options$tol_metric, error = function(e) NULL)
        if (is.null(tol) || !is.finite(tol) || tol <= 1e-05) return(invisible(NULL))
        tryCatch(
          table$setNote(
            "metric_tolerance",
            # Was raw English. The msgid is now one literal; the substituted values are
            # numbers and option keys, not grammar, so %s interpolation is safe here.
            sprintf(.("Metric tolerance is %1$s: every cutpoint scoring within %2$s of the best %3$s was treated as equivalent and the reported cutpoint is their %4$s. It is therefore <i>not</i> necessarily the cutpoint that maximises the metric, and its sensitivity and specificity can differ appreciably from the maximum. Set the tolerance to 0 to report the maximising cutpoint itself."),
              base::format(tol), base::format(tol),
              tryCatch(self$options$metric, error = function(e) "metric"),
              tryCatch(self$options$break_ties, error = function(e) "average"))),
          error = function(e) NULL)
      },

      # Degenerate-estimate caveats for the AUC tables.
      #
      # Three cases produced a confident-looking number with nothing to qualify it:
      #   AUC == 1  - DeLong's interval collapses to exactly 1-1 and the variance is
      #               0, so the CI columns showed 1.000 to 1.000 (read as extreme
      #               precision) and p came back blank with no explanation.
      #   tiny n    - the correct min(n_pos, n_neg) < 10 guard existed but was wired
      #               only to the DeLong comparison table, so a single-marker run
      #               never saw it. At the enforced floor (2 per class) the analysis
      #               reported AUC 1.000 and "Excellent".
      #   zero var  - a constant marker gave AUC 0.500 with a 0.500-0.500 CI and
      #               "Sensitivity 1.000" at cutpoint Inf; grep for "zero variance"
      #               over the whole backend returned nothing.
      # PPV and NPV are prevalence-dependent, so they do not transfer to a
      # population with a different disease frequency. The module DID carry that
      # caveat -- but only inside .generateFixedSensSpecExplanation(), which
      # renders solely when the Fixed Sensitivity/Specificity feature is switched
      # on. The main results table, which is where PPV/NPV actually appear and is
      # visible by default, said nothing. At 2% prevalence it reported PPV 0.13
      # (59 false positives for 9 true positives) with no note at all.
      .notePredictiveValues = function(table, prevalence) {
        if (is.null(prevalence) || !is.finite(prevalence)) return(invisible(NULL))
        extreme <- prevalence < 0.10 || prevalence > 0.90
        msg <- sprintf(
          .("PPV and NPV below are computed at the prevalence of this sample (%.1f%%) and do not carry over to a population where the condition is more or less common - sensitivity and specificity also require comparable case mix and conditional score distributions. Recompute them for your own setting's prevalence before quoting them."),
          prevalence * 100)
        if (extreme) {
          msg <- paste(msg, sprintf(
            .("At %1$.1f%% this sample is strongly imbalanced, so the %2$s is especially fragile: a small shift in prevalence moves it a long way."),
            prevalence * 100,
            if (prevalence < 0.10) .("PPV") else .("NPV")))
        }
        tryCatch(table$setNote("predictive_values_prevalence", msg), error = function(e) NULL)
        table$setNote("undefined_predictive_values",
          .("A predictive value is undefined and shown as blank when its conditioning group has no observations or its adjusted denominator is zero."), init = FALSE)
        invisible(NULL)
      },

      .noteDegenerateEstimates = function(table, auc_values, n_pos, n_neg, constant_vars = character(0)) {
        # Recomputed estimates/counts can remove a condition on the same analysis instance.
        for (key in c("perfect_separation", "small_sample", "constant_marker"))
          table$setNote(key, NULL)
        finite_auc <- auc_values[is.finite(auc_values)]

        if (length(finite_auc) > 0 && any(finite_auc >= 0.9995)) {
          tryCatch(table$setNote(
            "perfect_separation",
            .("At least one marker separates the two classes completely in this sample (AUC 1.000). The empirical variance may be zero; the AUC summary withholds the interval and p-value rather than presenting degenerate inference as precision. Complete separation in a sample this size is far more often label leakage, an outcome-derived predictor, or overfitting than a genuinely perfect test. Validate externally before drawing any conclusion.")
          ), error = function(e) NULL)
        }

        min_class <- suppressWarnings(min(n_pos, n_neg, na.rm = TRUE))
        if (is.finite(min_class) && min_class < 10) {
          tryCatch(table$setNote(
            "small_sample",
            sprintf(.("Only %d cases in the smaller outcome class. The AUC, interval and p-value can be unstable in sparse samples. Ten per class is a warning threshold, not a guarantee of reliable inference above it; imbalance and score distributions also affect coverage. Treat these estimates as provisional."), as.integer(min_class))
          ), error = function(e) NULL)
        }

        if (length(constant_vars) > 0) {
          tryCatch(table$setNote(
            "constant_marker",
            sprintf(.("These markers take the same value in every case and carry no discrimination information: %s. Their AUC of 0.500 has zero empirical variance; the AUC summary withholds the interval and p-value. No clinically useful cutpoint can distinguish the classes. Remove them from the analysis."), paste(constant_vars, collapse = ", "))
          ), error = function(e) NULL)
        }
        invisible(NULL)
      },

      # A cutpoint searched on the same data the sensitivity and specificity are then read off
      # gives resubstitution estimates, which are optimistic. Say so beside them rather than only
      # in the instructions panel. With method = "oc_manual" nothing is searched, so that branch
      # gets the opposite (and correct) note.
      .noteCutpointOptimism = function(table) {
        method <- tryCatch(self$options$method, error = function(e) NULL)
        if (identical(method, "oc_manual")) {
          tryCatch(
            table$setNote(
              "cutpoint_optimism",
              .("The cutpoint was supplied by you rather than searched for in these data, so the sensitivity and specificity reported at it are not inflated by cutpoint selection. They remain subject to ordinary sampling error.")),
            error = function(e) NULL)
          return(invisible(NULL))
        }
        # Only the maximize/minimize families actually use the Optimization Metric setting.
        # The Youden-smoothing methods use their own criterion, and oc_cost_ratio /
        # oc_equal_sens_spec / oc_closest_01 have their cutpoint recomputed afterwards
        # without reading it (see the override block in .run()), so naming the metric
        # for those methods would tell the user something untrue.
        method_key <- if (is.null(method)) "" else method
        metric_txt <- if (method_key %in% c("maximize_metric", "minimize_metric",
                                            "maximize_loess_metric", "minimize_loess_metric",
                                            "maximize_boot_metric", "minimize_boot_metric")) {
          metric <- tryCatch(self$options$metric, error = function(e) NULL)
          metric_titles <- c(
            youden = "Youden Index (Sens + Spec - 1)",
            sum_sens_spec = "Sum of sensitivity and specificity",
            accuracy = "Overall accuracy",
            sum_ppv_npv = "Sum of PPV and NPV",
            prod_sens_spec = "Product of sensitivity and specificity",
            prod_ppv_npv = "Product of PPV and NPV",
            cohens_kappa = "Cohen's Kappa",
            abs_d_sens_spec = "Minimize |Sens - Spec|",
            abs_d_ppv_npv = "Minimize |PPV - NPV|",
            F1_score = "F1 Score",
            odds_ratio = "Diagnostic odds ratio",
            risk_ratio = "Risk ratio",
            misclassification_cost = "Misclassification cost",
            total_utility = "Total utility",
            roc01 = "Distance to (0,1) in ROC space",
            p_chisquared = "Chi-squared statistic"
          )
          if (!is.null(metric) && nzchar(metric) && metric %in% names(metric_titles)) {
            sprintf(.("the metric you chose under Optimization Metric (%s)"), metric_titles[[metric]])
          } else {
            .("the metric you chose under Optimization Metric")
          }
        } else if (method_key == "oc_youden_kernel") {
          .("the Youden index (sensitivity + specificity - 1) read off a kernel-smoothed ROC curve; the Optimization Metric setting is not used by this method")
        } else if (method_key == "oc_youden_normal") {
          .("the Youden index (sensitivity + specificity - 1) computed under a normal-distribution model for the two groups; the Optimization Metric setting is not used by this method")
        } else if (method_key == "oc_cost_ratio") {
          .("expected misclassification cost, using the cost ratio you set for false positives against false negatives; the Optimization Metric setting is not used by this method")
        } else if (method_key == "oc_equal_sens_spec") {
          .("the balance between sensitivity and specificity, by taking the cutpoint at which the two come closest to being equal; the Optimization Metric setting is not used by this method")
        } else if (method_key == "oc_closest_01") {
          .("closeness to the top-left corner of the ROC plot, the point of perfect sensitivity and specificity; the Optimization Metric setting is not used by this method")
        } else {
          .("the criterion belonging to the cutpoint method you selected")
        }
        note <- sprintf(.("The cutpoint was chosen to optimise %s on these same data. The sensitivity, specificity and other metrics reported at that cutpoint are therefore optimistically biased and will typically be lower in a new sample; the bias grows as the sample gets smaller and as more candidate cutpoints are searched. Use bootstrap or split-sample validation before quoting these figures."), metric_txt)
        if (isTRUE(self$options$allObserved)) {
          note <- paste(note, .("This table lists every observed test value as a candidate cutpoint, not only the selected one; the bias above applies to whichever row you report as the cutpoint."))
        }
        tryCatch(table$setNote("cutpoint_optimism", note), error = function(e) NULL)
      },

      # Empty every results table before .run() repopulates them. jmvcore's addRow() appends
      # unconditionally, so a table filled that way grows on every re-run; tables filled with
      # setRow() are unaffected but clearing them is harmless because .run() rewrites them.
      # Discovering the tables from self$results rather than hard-coding a list means a table
      # added later is covered without anyone having to remember this method.
      .clearTables = function() {
        clearNotes <- function(item) {
          if (inherits(item, "Table")) {
            for (key in names(item$notes)) item$setNote(key, NULL)
          } else if (inherits(item, "Group") || inherits(item, "Array")) {
            for (child in item$items) clearNotes(child)
          }
        }
        clearNotes(self$results)
        for (nm in names(self$results)) {
          item <- tryCatch(self$results[[nm]], error = function(e) NULL)
          if (!is.null(item) && inherits(item, "Table")) {
            # metaAnalysisTable's rows are scaffolded in .init(); deleting them would drop the
            # scaffold (and make setRow() fail), so only its values are blanked.
            if (identical(nm, "metaAnalysisTable")) {
              private$.blankMetaAnalysisRows()
              next
            }
            tryCatch(.deleteTableRows(item), error = function(e) NULL)
          }
        }
      },

      # library-audit 2026-09-16 meddecide [LOW] DONE (same class): metaAnalysisTable has a fixed row set, so .init()
      # scaffolds the rows and .run() fills them with setRow()
      # The pooled models reported are chosen by the Meta-Analysis Method option alone, so the
      # rows are known before any data is seen.
      .metaAnalysisRowLabels = function() {
        labels <- c(fixed = .("Fixed Effects"), random = .("Random Effects"))
        method <- self$options$metaAnalysisMethod
        labels[c(method %in% c("fixed", "both"), method %in% c("random", "both"))]
      },

      .blankMetaAnalysisRows = function() {
        for (key in names(private$.metaAnalysisRowLabels()))
          self$results$metaAnalysisTable$setRow(rowKey = key, values = list(
            pooled_auc = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
            heterogeneity_i2 = NA_real_, tau_squared = NA_real_,
            cochran_q = NA_real_, q_p_value = NA_real_))
      },

      # 95% limits for an AUC, built on the logit scale with the delta-method SE (se / (a (1 - a)))
      # and back-transformed, so they stay inside 0-1: the raw a +/- 1.96 se gave a pooled upper
      # limit of 1.012. An AUC of 0 or 1, or a zero SE, has no logit; it keeps the raw limits,
      # clamped to 0-1.
      .aucLimits = function(a, se) {
        ok <- is.finite(a) & is.finite(se) & a > 0 & a < 1 & se > 0
        z <- stats::qnorm(.975)
        w <- ifelse(ok, z * se / (a * (1 - a)), NA_real_)
        list(lower = ifelse(ok, stats::plogis(stats::qlogis(a) - w), pmax(0, a - z * se)),
             upper = ifelse(ok, stats::plogis(stats::qlogis(a) + w), pmin(1, a + z * se)))
      },

      # ============================================================================
      # MAIN ANALYSIS METHOD
      # ============================================================================

      # Execute the ROC analysis
      .run = function() {
        # jamovi reuses the R6 instance across runs, so a warning raised once
        # would otherwise persist into every later run of the analysis.
        # -----------------------------------------------------------------------
        # 0. SET SEED FOR REPRODUCIBILITY (preserve and restore global RNG state)
        # -----------------------------------------------------------------------
        if (!is.null(self$options$seed)) withr::local_seed(self$options$seed)

        # -----------------------------------------------------------------------
        # 0a. MANUAL RUN GATE
        # -----------------------------------------------------------------------
        if (isTRUE(self$options$manualRun) && !isTRUE(self$options$run)) {
          self$results$instructions$setContent(
            "<html><body><div class='instructions'>
            <p><b>Manual run mode active.</b></p>
            <p>Adjust options as needed, then click the <b>Run</b> button to compute results.</p>
          </div></body></html>"
          )
          return()
        }

        private$.userWarnings <- character(0)
        private$.runSummaryHead <- NULL
        private$.varIdentity <- list()
        private$.quantileProbs <- numeric(0)
        private$.resetNotices()

        # -----------------------------------------------------------------------
        # 0b. CLEAR TABLES BEFORE REPOPULATING
        # -----------------------------------------------------------------------
        # jamovi re-runs .run() on the SAME analysis object whenever any option changes, and
        # almost every table here is filled with addRow(). Without clearing first they stack
        # duplicates on each re-run: measured 1 -> 2 -> 3 rows per predictor, and the decision
        # curve going 40 -> 80 -> 120. Only thresholdTable and the results table cleared
        # themselves. Doing it once here covers every table and cannot land inside one of the
        # population loops.
        #
        # This MUST sit after the manual-run gate: in manual mode jamovi still calls .run() on
        # every option change, so clearing before the gate would wipe the user's computed tables
        # and then return without recomputing them, which is the opposite of what manual mode is
        # for.
        #
        # runSummary and procedureNotes have no clearWith and are not written in .init(), so
        # jamovi hands back the previous run's text: the 'Choose the positive class' panel after
        # a class was chosen, 'Positive Class: X' after it was unset. Clear both here (after the
        # gate, so manual mode keeps its results). The client keeps what it painted when an Html
        # turns empty, so one still empty when .run() exits is hidden too.
        self$results$runSummary$setContent("")
        self$results$procedureNotes$setContent("")
        on.exit(for (el in list(self$results$runSummary, self$results$procedureNotes))
          if (!isTRUE(nzchar(el$content))) el$setVisible(FALSE), add = TRUE)
        private$.delongUsedFallback <- FALSE
        # Per-run storage is keyed by variable name and read back by the threshold table,
        # the combined criterion plot, the prevalence plot and the forest plot renderer.
        # Without a reset, entries from the previous run survive: a `<var>_smooth` frame
        # (different columns) written by the smoothing step made every later rbind() and
        # addRow() over names(.rocDataList) fail, and dropped variables kept their rows.
        private$.rocDataList <- list()
        private$.smoothedRocDataList <- list()
        private$.optimalCriteriaList <- list()
        private$.prevalenceList <- list()
        private$.samplePrevalenceList <- list()
        private$.aucList <- list()
        private$.clearTables()
        self$results$delongComparisonTable$setNote("degenerate_contrast", NULL)

        # -----------------------------------------------------------------------
        # 1. INSTRUCTIONS AND PRELIMINARY CHECKS
        # -----------------------------------------------------------------------

        # Update meta-analysis UI visibility based on variable count
        private$.updateMetaAnalysisVisibility()

        # Both selectors must name distinct observed levels; never infer a reference group.
        if (!is.null(self$options$classVar)) {
          coding <- private$.resolveOutcomeClasses(self$data[[private$.escapeVar(self$options$classVar)]])
          if (identical(coding$status, "unset")) {
            private$.setNotice("chooseClasses", .("Choose distinct observed levels in Positive Class and Negative Class."))
            self$results$runSummary$setContent(paste0("<p>",
              .("Choose the positive and negative classes. Nothing is computed until both are selected. Other outcome levels and missing outcomes will be excluded."), "</p>"))
            return()
          }
          if (identical(coding$status, "single")) {
            private$.throwError("insufficient_data",
              sprintf(.("The class variable %1$s has fewer than two observed levels; ROC analysis needs cases with and without the condition."), self$options$classVar))
          }
          if (identical(coding$status, "same")) {
            private$.throwError("invalid_class",
              .("Positive Class and Negative Class must be different observed levels."))
          }
          if (coding$status %in% c("invalid_positive", "invalid_negative")) {
            message <- if (identical(coding$status, "invalid_positive"))
              sprintf(.("Positive Class is set to %1$s, but no row of %2$s has that level. Choose an observed level in Positive Class."), coding$positive, self$options$classVar)
            else sprintf(.("Negative Class is set to %1$s, but no row of %2$s has that level. Choose an observed level in Negative Class."), coding$negative, self$options$classVar)
            private$.throwError("invalid_class", message,
              .("After changing the class variable or filtering rows, reselect both outcome levels."))
          }
        }

        # Show instructions if required inputs are not provided
        if (is.null(self$options$classVar) || is.null(self$options$dependentVars) ||
          length(self$options$dependentVars) == 0) {
          private$.showWelcome()
          return()
        } else {
          # Strict Validation
          val <- private$.validateInputs()
          if (!is.null(val)) {
            self$results$runSummary$setContent(paste("<b>Error:</b>", val))
            self$results$instructions$setVisible(FALSE)
            private$.setNotice("validation", val, jmvcore::NoticeType$ERROR)
            jmvcore::reject(val)
          }

          if (isTRUE(self$options$plotROC) && isTRUE(self$options$quantileCIs)) {
            parsed <- .psychopdaProbabilityList(self$options$quantiles)
            private$.quantileProbs <- parsed$values
            if (parsed$kept < parsed$given || !length(parsed$values)) {
              private$.warnUser(.fmt(
                .("Only {kept} of {given} ROC quantiles are valid probabilities from 0 to 1. Enter comma-separated values such as 0.1,0.5,0.9. Error bars use the accepted values; none are drawn if no value is valid."),
                kept = parsed$kept, given = parsed$given))
            }
          }

          # Hide instructions when inputs are provided. This one is genuine
          # option/data-driven visibility (the onboarding panel), not a failure
          # signal, so it stays. The block that used to follow -- restoring
          # procedureNotes, plotROC, resultsTable, thresholdTable, sensSpecTable,
          # delongComparisonTable and delongTest -- only existed to undo the
          # setVisible(FALSE) calls that .init() no longer makes. Each of those
          # elements already carries the equivalent `visible:` expression in
          # jamovi/psychopdaroc.r.yaml.
          self$results$instructions$setVisible(FALSE)

          # Create procedure notes with analysis details
          procedureNotes <- paste0(
            "<html><body><p>", .("Procedure Notes"), "</p><hr>",
            "<p>", .("The ROC analysis used the following specifications."), "</p>",
            "<p>\u{00A0}</p>",
            "<p>", .("Measure Variable(s)"), ": ",
            jmvcore::htmlEscape(paste(unlist(self$options$dependentVars), collapse = ", ")),
            "</p>",
            "<p>", .("Class Variable"), ": ",
            jmvcore::htmlEscape(self$options$classVar),
            "</p>",
            "<p>", .("Positive Class"), ": ",
            jmvcore::htmlEscape(coding$positive),
            "</p><p>", .("Negative Class"), ": ",
            jmvcore::htmlEscape(coding$negative), "</p>"
          )

          # Add subgroup info if used
          if (!is.null(self$options$subGroup) && self$options$subGroup != "") {
            procedureNotes <- paste0(
              procedureNotes,
              "<p>", .("Subgroup Variable"), ": ",
              jmvcore::htmlEscape(self$options$subGroup),
              "</p>"
            )
          }

          # Add enhanced analysis settings with more detail
          procedureNotes <- paste0(
            procedureNotes,
            "<p>\u{00A0}</p>",
            "<p>", .("Method"), ": ",
            self$options$method,
            "</p>",
            "<p>", .("All Observed Cutpoints"), ": ",
            self$options$allObserved,
            "</p>",
            "<p>", .("Metric"), ": ",
            self$options$metric,
            "</p>",
            "<p>", .("Direction (relative to cutpoint)"), ": ",
            self$options$direction,
            "</p>",
            "<p>", .("Tie Breakers"), ": ",
            self$options$break_ties,
            "</p>",
            "<p>", .("Metric Tolerance"), ": ",
            self$options$tol_metric,
            "</p>",
            "<p>\u{00A0}</p>"
          )

          # Add bootstrap info if applicable
          if (self$options$boot_runs > 0) {
            procedureNotes <- paste0(
              procedureNotes,
              "<p>", .("Bootstrap Runs"), ": ",
              self$options$boot_runs,
              "</p>"
            )
          }

          # Guidance for the selected level. This is the only place the Guidance Level
          # option affects optional Procedure Notes -- unlike
          # `instructions`, where this text used to be written and immediately discarded.
          # For the Basic level it carries the cutpoint-optimism caveat; the per-method
          # version of that caveat is also footnoted on the Results table itself by
          # .noteCutpointOptimism(), so it reaches the user at every level.
          if (isTRUE(self$options$showExplanations)) private$.applyClinicalModeSettings()
          else private$.modeInstructionsHtml <- ""
          private$.setNotice("scope",
            .("Exploratory ROC analysis: analysis units must be independent; selected cutpoints need validation. Nominal interval coverage is not guaranteed in small or imbalanced samples."))

          # Close notes
          procedureNotes <- paste0(
            procedureNotes,
            private$.modeInstructionsHtml,
            "<p>", if (identical(self$options$analysisUnit, "rows"))
              .("Each row must represent an independent observation. These confidence intervals, tests and bootstrap resamples do not account for clustering or repeated measurements. Multiple lesions, blocks, slides or visits from the same patient require a prespecified patient-level summary or a method that accounts for clustering; do not treat them as independent patients.")
            else .("Patient summaries are the independent analysis units. Choose the mean or median before inspecting performance; the resulting accuracy describes that patient summary. This does not estimate lesion-level or visit-level accuracy with cluster-adjusted uncertainty."), "</p>",
            "<hr /></body></html>"
          )
          self$results$procedureNotes$setContent(procedureNotes)
        }


        # -----------------------------------------------------------------------
        # 2. SET UP ANALYSIS PARAMETERS
        # -----------------------------------------------------------------------

        # Every analysis receives the same explicit pair. Unselected levels cannot become
        # negatives in an optional path, a subgroup, a prevalence estimate or a resample.
        classCol <- private$.escapeVar(self$options$classVar)
        patient_data <- tryCatch(.psychopdaPatientData(self$data, coding$included,
          self$options$patientID,
          unique(c(self$options$dependentVars, self$options$refVar)), classCol,
          self$options$subGroup, self$options$analysisUnit), error = identity)
        if (inherits(patient_data, "error")) {
          message <- switch(conditionMessage(patient_data),
            patient_id_required = .("Select a Patient ID before using patient-level summaries."),
            patient_id_role = .("Patient ID must be different from the markers, outcome and subgroup variables."),
            patient_id_missing = .("Patient ID is missing for an included observation. Supply an ID for every included row."),
            patient_repeated = .("Repeated Patient IDs require a prespecified patient mean or median. Independent-row inference cannot treat repeated patients as separate observations."),
            patient_outcome = .("A patient has conflicting outcome levels. Patient summaries require one outcome per patient."),
            patient_subgroup = .("A patient has conflicting or partly missing subgroup values. Use one subgroup per patient."),
            conditionMessage(patient_data))
          private$.setNotice("patientData", message, jmvcore::NoticeType$ERROR)
          jmvcore::reject(message)
        }
        data <- patient_data$data
        unit_note <- if (!identical(self$options$analysisUnit, "rows")) .fmt(
          .("Patient-level analysis: {rows} included rows were reduced to {patients} patients using the prespecified {method} of available scores for each marker. All cutpoints, models, intervals and resamples use these patient summaries. This estimates accuracy of the patient summary, not of individual lesions or visits. Patients with no observed score for a marker are excluded for that marker."),
          rows = patient_data$rows, patients = patient_data$patients,
          method = if (identical(self$options$analysisUnit, "patientMedian")) .("median") else .("mean"))
        else .("Independent-row analysis: each analysed row must represent an independent observation.")
        private$.setNotice("analysisUnit", unit_note)
        self$results$outcomeCoding$setNote("analysis_unit", unit_note)
        positiveClass <- coding$positive
        negativeClass <- coding$negative
        data[[classCol]] <- private$.binaryResponse(data[[classCol]], positiveClass)
        classLabels <- c(positive = sprintf(.("Positive (%s)"), positiveClass),
          negative = sprintf(.("Negative (%s)"), negativeClass))
        n_pos <- sum(data[[classCol]] == positiveClass)
        n_total <- nrow(data)
        prevalence <- n_pos / n_total

        if (coding$n_other > 0 || coding$n_missing > 0) {
          private$.setNotice("excludedOutcomes", sprintf(
            .("Outcome selection excluded %1$d row(s) in unselected levels and %2$d row(s) with missing outcomes. All analyses use only %3$s (positive) and %4$s (negative), with marker-specific complete cases."),
            coding$n_other, coding$n_missing, positiveClass, negativeClass))
        }
        if (prevalence < 0.1 || prevalence > 0.9) {
          private$.warnUser(sprintf(
            .("Class imbalance detected (Prevalence: %.1f%%). PPV and NPV depend on prevalence and may not transfer to your clinical population. Consider Precision-Recall curves and validate in the intended setting."),
            prevalence * 100), type = jmvcore::NoticeType$STRONG_WARNING)
        }

        # Subgroup analysis: DeLong + subgroup is rejected by .validateInputs() before this
        # point (an explicit, actionable error), so the old "DeLong test disabled" warning
        # here could never be seen. IDI/NRI are NOT disabled -- they run on the full sample
        # (see the idiTable note) -- and the message used to claim the opposite.
        if (!is.null(self$options$subGroup)) {
          if (self$options$calculateIDI || self$options$calculateNRI) {
            private$.warnUser(.("IDI/NRI are computed on the full sample; the subgroup variable is not applied to them."))
          }
        }

        # Analysis Status box. The header is fixed for this run; the warning box is
        # rendered from private$.userWarnings now AND again when .run() exits (normally
        # or through reject()), so warnings raised by later steps -- package checks,
        # the DeLong fallback, the global test, subgroup handling -- reach the user
        # instead of being collected after the box was already written.
        private$.runSummaryHead <- paste0(
          "<div style='padding: 10px; background-color: rgba(138, 155, 172, 0.06); border: 1px solid #dee2e6; border-radius: 4px; margin-bottom: 15px; color: inherit;'>",
          "<h4>", .("Analysis Status"), "</h4>",
          "<ul>",
          "<li><strong>", .("Seed"), ":</strong> ", self$options$seed, "</li>",
          "<li><strong>", .("Positive Class"), ":</strong> ", jmvcore::htmlEscape(positiveClass), " (", .("Prevalence"), ": ", round(prevalence * 100, 1), "%)</li>",
          "<li><strong>", .("Negative Class"), ":</strong> ", jmvcore::htmlEscape(negativeClass), "</li>",
          # "Analysis Mode" implied the option changed what was computed; it only chooses
          # which guidance paragraph appears in Procedure Notes.
          "<li><strong>", .("Guidance Level"), ":</strong> ", tools::toTitleCase(self$options$clinicalMode), "</li>",
          "</ul>"
        )
        on.exit(private$.renderRunSummary(), add = TRUE)
        private$.renderRunSummary()

        # Set up cutpoint method
        if (self$options$method == "oc_manual") {
          method <- cutpointr::oc_manual
          if (self$options$specifyCutScore == "") {
            private$.throwError(
              "invalid_cutpoint",
              "Cut score is required when using manual cutpoint method.",
              "Enter a numeric value in the 'Manual Cut Score' field"
            )
          } else {
            score <- as.numeric(self$options$specifyCutScore)
          }
        } else {
          # Map method name to actual function
          methodName <- self$options$method
          if (methodName == "maximize_metric") {
            method <- cutpointr::maximize_metric
          } else if (methodName == "minimize_metric") {
            method <- cutpointr::minimize_metric
          } else if (methodName == "maximize_loess_metric") {
            method <- cutpointr::maximize_loess_metric
          } else if (methodName == "minimize_loess_metric") {
            method <- cutpointr::minimize_loess_metric
          } else if (methodName == "maximize_boot_metric") {
            method <- cutpointr::maximize_boot_metric
          } else if (methodName == "minimize_boot_metric") {
            method <- cutpointr::minimize_boot_metric
          } else if (methodName == "oc_youden_kernel") {
            method <- cutpointr::oc_youden_kernel
          } else if (methodName == "oc_youden_normal") {
            method <- cutpointr::oc_youden_normal
          } else if (methodName %in% c("oc_cost_ratio", "oc_equal_sens_spec", "oc_closest_01")) {
            # For custom methods, use maximize_metric as placeholder
            # We'll handle the actual calculation later
            method <- cutpointr::maximize_metric
          } else {
            # Default if method not recognized
            method <- cutpointr::maximize_metric
          }

          score <- NULL
        }

        # Set up tolerance if needed for specific methods.
        #
        # The two boot_metric methods MUST be in this list. cutpointr's boot
        # wrappers pass tol_metric straight through to maximize_metric on each
        # resample, and a NULL there makes cutpointr return optimal_cutpoint =
        # NaN. The addRow loop then computes which.min(abs(x.sorted - NaN)) ==
        # integer(0), its length guard fails, and NO row is ever added -- the
        # Results table came back completely empty for both boot methods, with
        # no error and no notice. They belong here anyway: the file's own list
        # of metric-consuming methods (see .procedureNotes) already includes them.
        if (self$options$method %in% c(
          "maximize_metric",
          "minimize_metric",
          "maximize_loess_metric",
          "minimize_loess_metric",
          "maximize_boot_metric",
          "minimize_boot_metric"
        )) {
          tol_metric <- self$options$tol_metric
        } else {
          tol_metric <- NULL
        }

        # Set up metric function
        metricName <- self$options$metric

        if (metricName == "youden") {
          metric <- cutpointr::youden
        } else if (metricName == "sum_sens_spec") {
          metric <- cutpointr::sum_sens_spec
        } else if (metricName == "accuracy") {
          metric <- cutpointr::accuracy
        } else if (metricName == "sum_ppv_npv") {
          metric <- cutpointr::sum_ppv_npv
        } else if (metricName == "prod_sens_spec") {
          metric <- cutpointr::prod_sens_spec
        } else if (metricName == "prod_ppv_npv") {
          metric <- cutpointr::prod_ppv_npv
        } else if (metricName == "cohens_kappa") {
          metric <- cutpointr::cohens_kappa
        } else if (metricName == "abs_d_sens_spec") {
          metric <- cutpointr::abs_d_sens_spec
        } else if (metricName == "roc01") {
          metric <- cutpointr::roc01
        } else if (metricName == "abs_d_ppv_npv") {
          metric <- cutpointr::abs_d_ppv_npv
        } else if (metricName == "p_chisquared") {
          metric <- function(tp, fp, tn, fn, ...) {
            den <- (tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)
            value <- ifelse(den > 0,
              (tp + fp + tn + fn) * (tp * tn - fp * fn)^2 / den, NA_real_)
            matrix(value, ncol = 1, dimnames = list(NULL, "chisquared"))
          }
        } else if (metricName == "odds_ratio") {
          metric <- cutpointr::odds_ratio
        } else if (metricName == "risk_ratio") {
          metric <- cutpointr::risk_ratio
        } else if (metricName == "misclassification_cost") {
          metric <- cutpointr::misclassification_cost
        } else if (metricName == "total_utility") {
          metric <- cutpointr::total_utility
        } else if (metricName == "F1_score") {
          metric <- cutpointr::F1_score
        } else {
          # Default to Youden's index if not recognized
          metric <- cutpointr::youden
        }

        # Set up break_ties function
        if (self$options$break_ties == "c") {
          break_ties <- c
        } else if (self$options$break_ties == "mean") {
          break_ties <- mean
        } else if (self$options$break_ties == "median") {
          break_ties <- median
        } else {
          break_ties <- mean
        }

        # Get other analysis parameters
        direction <- self$options$direction
        boot_runs <- as.numeric(self$options$boot_runs)

        # Set up for collecting plot data
        plotDataList <- data.frame(
          var = character(),
          cutpoint = numeric(),
          sensitivity = numeric(),
          specificity = numeric(),
          ppv = numeric(),
          npv = numeric(),
          AUC = numeric(),
          youden = numeric(),
          stringsAsFactors = FALSE
        )


        # -----------------------------------------------------------------------
        # 3. PREPARE VARIABLES FOR ANALYSIS
        # -----------------------------------------------------------------------

        # Get dependent variables list
        vars <- self$options$dependentVars

        # Handle subgroups if present
        if (!is.null(self$options$subGroup)) {
          # Escape variable names for safe data frame access
          subGroupEscaped <- private$.escapeVar(self$options$subGroup)
          classVarEscaped <- private$.escapeVar(self$options$classVar)

          # Use [[]] for safe column access with escaped names
          subGroup <- data[[subGroupEscaped]]
          classVar <- data[[classVarEscaped]]
          # NA is not a stratum: unique() kept it, producing a "var ::: NA" level whose
          # `subGroup == "NA"` selection was empty and aborted the whole analysis.
          n_na_subgroup <- sum(is.na(subGroup))
          if (n_na_subgroup > 0) {
            private$.warnUser(sprintf(
              .("%1$d row(s) with a missing value in the subgroup variable (%2$s) were excluded from the subgroup analysis."),
              n_na_subgroup, self$options$subGroup))
          }
          uniqueGroups <- unique(subGroup[!is.na(subGroup)])
          if (length(uniqueGroups) == 0L) {
            private$.throwError("insufficient_data",
              .("The subgroup variable has no observed levels. Remove it or provide nonmissing subgroup values."))
          }
          identities <- expand.grid(marker = vars, group = as.character(uniqueGroups),
            stringsAsFactors = FALSE)
          vars <- make.unique(paste(identities$marker, identities$group, sep = " ::: "))
          private$.varIdentity <- stats::setNames(lapply(seq_len(nrow(identities)),
            function(i) as.list(identities[i, , drop = FALSE])), vars)
        } else {
          subGroup <- NULL
        }

        # Storage for AUCs
        aucList <- list()

        # -----------------------------------------------------------------------
        # 4. PROCESS EACH VARIABLE
        # -----------------------------------------------------------------------

        for (var in vars) {
          # Checkpoint before expensive variable processing
          private$.checkpoint()
          # Add items to results tables if not already present.
          # Each Array is guarded on ITS OWN itemKeys. Nesting them all under the
          # resultsTable guard meant that once a variable was registered there, an Array
          # emptied by its own clearWith (or enabled later by its own checkbox) never got
          # its item back - and Array$get() then reject()s with "No such key or name",
          # killing the whole analysis rather than leaving one panel blank.
          if (!var %in% self$results$sensSpecTable$itemKeys)
            self$results$sensSpecTable$addItem(key = var)
          if (!var %in% self$results$resultsTable$itemKeys)
            self$results$resultsTable$addItem(key = var)

          # Add individual plots if not combining
          if (self$options$combinePlots == FALSE) {
            if (!var %in% self$results$plotROC$itemKeys)
              self$results$plotROC$addItem(key = var)

            # Add additional plot items if enabled
            if (self$options$showCriterionPlot && !var %in% self$results$criterionPlot$itemKeys) {
              self$results$criterionPlot$addItem(key = var)
            }
            if (self$options$showPrevalencePlot && !var %in% self$results$prevalencePlot$itemKeys) {
              self$results$prevalencePlot$addItem(key = var)
            }
            if (self$options$showDotPlot && !var %in% self$results$dotPlot$itemKeys) {
              self$results$dotPlot$addItem(key = var)
            }
          }

          # Extract data for analysis
          prepared <- private$.prepareVarData(data, var, subGroup)
          dependentVar <- prepared$dependentVar
          classVar <- prepared$classVar

          # -----------------------------------------------------------------------
          # 5. RUN ROC ANALYSIS
          # -----------------------------------------------------------------------

          # Checkpoint before expensive ROC analysis
          private$.checkpoint()

          cp_res <- private$.runCutpointrMetrics(
            dependentVar = dependentVar,
            classVar = classVar,
            positiveClass = positiveClass,
            method = method,
            score = score,
            metric = metric,
            direction = direction,
            tol_metric = tol_metric,
            boot_runs = boot_runs,
            break_ties = break_ties,
            var_label = var
          )
          results <- cp_res$results
          confusionMatrix <- cp_res$confusionMatrix

          # -----------------------------------------------------------------------
          # 6. HANDLE CUSTOM CUTPOINT METHODS
          # -----------------------------------------------------------------------

          # Apply custom methods if specified
          if (self$options$method %in% c("oc_cost_ratio", "oc_equal_sens_spec", "oc_closest_01")) {
            # Get the confusion matrix data
            confusionMatrix <- results$roc_curve[[1]]

            # Calculate prevalence for cost-ratio method
            n_pos <- sum(classVar == positiveClass, na.rm = TRUE)
            n_neg <- sum(classVar != positiveClass, na.rm = TRUE)
            prevalence <- n_pos / (n_pos + n_neg)

            # Override with prior prevalence if requested
            if (self$options$usePriorPrev) {
              prevalence <- self$options$priorPrev
            }

            if (self$options$method == "oc_cost_ratio") {
              # Use custom cost ratio optimization
              cost_results <- private$.calculateCostRatioOptimal(
                confusionMatrix,
                prevalence,
                self$options$costratioFP
              )
              # Override the optimal cutpoint
              results$optimal_cutpoint <- confusionMatrix$x.sorted[cost_results$optimal_idx]
            } else if (self$options$method == "oc_equal_sens_spec") {
              # Find cutpoint with equal sensitivity and specificity
              eq_results <- private$.calculateEqualSensSpec(confusionMatrix)
              results$optimal_cutpoint <- confusionMatrix$x.sorted[eq_results$optimal_idx]
            } else if (self$options$method == "oc_closest_01") {
              # Find cutpoint closest to (0,1) point in ROC space
              closest_results <- private$.calculateClosestToOptimal(confusionMatrix)
              results$optimal_cutpoint <- confusionMatrix$x.sorted[closest_results$optimal_idx]
            }
          }

          # -----------------------------------------------------------------------
          # 7. DETERMINE CUTPOINTS TO DISPLAY
          # -----------------------------------------------------------------------

          resultsToDisplay <- if (!self$options$allObserved) unlist(results$optimal_cutpoint) else sort(unique(dependentVar))

          # Generate tables with metrics
          metrics_res <- private$.generateTables(
            var = var,
            results = results,
            confusionMatrix = results$roc_curve[[1]],
            resultsToDisplay = resultsToDisplay,
            dependentVar = dependentVar,
            classVar = classVar,
            positiveClass = positiveClass,
            direction = direction,
            metric = metric,
            classLabels = classLabels
          )
          sensList <- metrics_res$sensList
          specList <- metrics_res$specList
          ppvList <- metrics_res$ppvList
          npvList <- metrics_res$npvList
          youdenList <- metrics_res$youdenList
          metricList <- metrics_res$metricList
          j_max_idx <- metrics_res$j_max_idx
          aucList[[var]] <- results$AUC
          private$.aucList[[var]] <- results$AUC # Store in private field for clinical interpretation

          # -----------------------------------------------------------------------
          # 7.5. FIXED SENSITIVITY/SPECIFICITY ANALYSIS
          # -----------------------------------------------------------------------

          # Populate fixed sensitivity/specificity table if enabled
          if (self$options$fixedSensSpecAnalysis) {
            private$.populateFixedSensSpecTable(var, results$roc_curve[[1]], positiveClass)

            # Generate explanatory content (only once for first variable)
            if (var == vars[1]) {
              private$.generateFixedSensSpecExplanation()
            }

            # The fixed-point output remains a per-marker image in either main-plot mode.
            if (self$options$showFixedROC) {
              if (!var %in% self$results$fixedSensSpecROC$itemKeys) {
                self$results$fixedSensSpecROC$addItem(key = var)
              }
              # Set plot state data for fixed ROC
              fixedImage <- self$results$fixedSensSpecROC$get(key = var)
              # A msgid must be a literal. `.(paste(...))` is evaluated first, so the
              # compiler extracts nothing and the title can never be translated.
              fixedImage$setTitle(.fmt(
                if (identical(self$options$fixedAnalysisType, "sensitivity"))
                  .("Fixed Sensitivity ROC: {var}") else .("Fixed Specificity ROC: {var}"),
                var = var))
              fixedImage$setState(
                data.frame(
                  var = rep(var, length(confusionMatrix$x.sorted)),
                  cutpoint = confusionMatrix$x.sorted,
                  sensitivity = sensList,
                  specificity = specList,
                  stringsAsFactors = FALSE
                )
              )
            }
          }

          # -----------------------------------------------------------------------
          # 8. PREPARE PLOTTING DATA
          # -----------------------------------------------------------------------

          if (self$options$plotROC) {
            # Build this variable's ROC plot data once and always accumulate it into
            # plotDataList so the interactive plot renders in both individual and
            # combined modes (previously plotDataList was only filled when combining).
            varPlotData <- data.frame(
              var = rep(var, length(confusionMatrix$x.sorted)),
              cutpoint = confusionMatrix$x.sorted,
              sensitivity = sensList,
              specificity = specList,
              ppv = ppvList,
              npv = npvList,
              AUC = rep(results$AUC, length(confusionMatrix$x.sorted)),
              youden = youdenList,
              j_max_idx = j_max_idx,
              stringsAsFactors = FALSE
            )
            plotDataList <- rbind(plotDataList, varPlotData)

            if (self$options$combinePlots == FALSE) {
              # Individual plot for this variable
              image <- self$results$plotROC$get(key = var)
              image$setTitle(paste("ROC Curve:", var))
              image$setState(varPlotData)
            }
          }

          # Diagnostic plots have their own controls and do not require ROC curves.
          if (!self$options$combinePlots) {
            # Set states for additional plots if enabled
            if (self$options$showCriterionPlot) {
              criterionImage <- self$results$criterionPlot$get(key = var)
              criterionImage$setTitle(paste("Sensitivity and Specificity vs. Threshold:", var))
              # Drop the "rawData" attribute (one row per patient, attached for the ROC
              # confidence bands and read only in .run()). The criterion renderer never
              # touches it, so leaving it on serialises a full copy of the marker column
              # into the .omv per test variable.
              criterionData <- private$.rocDataList[[var]]
              attr(criterionData, "rawData") <- NULL
              criterionImage$setState(criterionData)
            }

            if (self$options$showPrevalencePlot) {
              prevImage <- self$results$prevalencePlot$get(key = var)
              prevImage$setTitle(paste("Predictive Values vs. Prevalence:", var))
              prevImage$setState(list(
                optimal = private$.optimalCriteriaList[[var]],
                prevalence = private$.prevalenceList[[var]]
              ))
            }

            if (self$options$showDotPlot) {
              # Get raw data for dot plot
              rawData <- data.frame(
                value = dependentVar,
                class = ifelse(classVar == positiveClass, "Positive", "Negative"),
                label = ifelse(classVar == positiveClass, classLabels[["positive"]], classLabels[["negative"]]),
                # unlist: a list column under 'All tied cutpoints'; the renderer draws threshold[1]
                threshold = rep(unlist(results$optimal_cutpoint)[1], length(dependentVar)),
                direction = rep(direction, length(dependentVar)),
                stringsAsFactors = FALSE
              )

              dotImage <- self$results$dotPlot$get(key = var)
              dotImage$setTitle(paste("Dot Plot:", var))
              dotImage$setState(rawData)
            }
          }
        } # End of loop through variables

        # -----------------------------------------------------------------------
        # 9. CREATE COMBINED PLOTS
        # -----------------------------------------------------------------------

        # Set interactive plot state if requested (uses combined data)
        if (isTRUE(self$options$interactiveROC) && self$options$plotROC && nrow(plotDataList) > 0) {
          self$results$interactivePlot$setState(as.data.frame(plotDataList))
        }

        # Create combined plot if requested
        if (self$options$combinePlots) {
          if (self$options$plotROC && nrow(plotDataList) > 0) {
            # Add the combined plot. Array$addItem has no duplicate check, so an unguarded
            # call stacks a fresh (blank, because get() returns the FIRST match) copy on
            # every run.
            if (!1 %in% self$results$plotROC$itemKeys)
              self$results$plotROC$addItem(key = 1)
            image <- self$results$plotROC$get(key = 1)
            image$setTitle(.("ROC Curve: Combined"))
            image$setState(plotDataList)
          }

          # Combined criterion plot if enabled
          if (self$options$showCriterionPlot) {
            # addItem has no duplicate check - guard, or blank copies accumulate.
            if (!1 %in% self$results$criterionPlot$itemKeys)
              self$results$criterionPlot$addItem(key = 1)
            criterionImage <- self$results$criterionPlot$get(key = 1)
            criterionImage$setTitle(.("Sensitivity and Specificity vs. Threshold: Combined"))

            # Prepare combined data for criterion plot
            combinedCriterionData <- data.frame()
            for (var in names(private$.rocDataList)) {
              varData <- private$.rocDataList[[var]]
              # rbind() propagates the attribute (verified), so strip it here too.
              attr(varData, "rawData") <- NULL
              varData$var <- var
              combinedCriterionData <- rbind(combinedCriterionData, varData)
            }
            criterionImage$setState(combinedCriterionData)
          }

          # Dot plots can't be combined meaningfully.
          # Visibility of both items is declarative in the .r.yaml
          # (dotPlotMessage: showDotPlot && combinePlots; dotPlot: showDotPlot &&
          # !combinePlots). The imperative setVisible() calls that used to live here were
          # one-way - nothing ever set dotPlot visible again - so the dot plots stayed
          # hidden after the user left combined mode while the checkbox still read "on".
          if (self$options$showDotPlot) {
            self$results$dotPlotMessage$setContent(
              .("<p>Dot plots aren't available in combined plot mode. Please uncheck 'Combine plots' to view individual dot plots.</p>")
            )
          }

          # Combined prevalence plot if enabled
          if (self$options$showPrevalencePlot) {
            # addItem has no duplicate check - guard, or blank copies accumulate.
            if (!1 %in% self$results$prevalencePlot$itemKeys)
              self$results$prevalencePlot$addItem(key = 1)
            prevImage <- self$results$prevalencePlot$get(key = 1)
            prevImage$setTitle("Predictive Values vs. Prevalence")

            # Only the first test variable is plotted here, so name it in the title rather
            # than labelling the panel "Combined" - the label named a different quantity
            # from the one actually plotted.
            if (length(private$.optimalCriteriaList) > 0 && length(private$.prevalenceList) > 0) {
              firstVar <- names(private$.optimalCriteriaList)[1]
              prevImage$setTitle(paste0("Predictive Values vs. Prevalence: ", firstVar))
              prevImage$setState(list(
                optimal = private$.optimalCriteriaList[[firstVar]],
                prevalence = private$.prevalenceList[[firstVar]]
              ))
            }
          }
        }

        private$.attachRocOverlayState()

        # -----------------------------------------------------------------------
        # 10. PERFORM DELONG'S TEST FOR AUC COMPARISON
        # -----------------------------------------------------------------------

        if (self$options$delongTest) {
          # Checkpoint before expensive DeLong test computation
          private$.checkpoint()
          # Enhanced validation with better error handling
          if (length(self$options$dependentVars) < 2) {
            private$.throwError(
              "insufficient_variables",
              "DeLong's test requires at least 2 test variables.",
              "Select multiple test variables for comparison"
            )
          }

          if (!is.null(self$options$subGroup)) {
            # Unreachable: .validateInputs() already rejects DeLong with a subgroup variable.
            NULL
          } else {
            # Escape variable names for safe data access
            depVarsEscaped <- private$.escapeVar(self$options$dependentVars)
            classVarEscaped <- private$.escapeVar(self$options$classVar)

            # Prepare data frames with escaped column names
            depVarsData <- data.frame(lapply(depVarsEscaped, function(v) jmvcore::toNumeric(data[[v]])))
            names(depVarsData) <- self$options$dependentVars # Use original names for output

            # Prepare classification variable
            classVarData <- as.character(data[[classVarEscaped]])

            # DeLong compares CORRELATED ROC curves, so every marker's AUC has to be
            # estimated on the SAME patients. Filtering on the class variable alone left
            # each marker's own NAs to be dropped later inside pROC::roc(), so the curves
            # were built on different row sets and pROC::cov(..., method = "delong") either
            # errored or paired mismatched observations. Delete listwise across the class
            # variable AND every marker entering the comparison.
            complete_cases <- !is.na(classVarData) & stats::complete.cases(depVarsData)
            n_excluded <- sum(!complete_cases)

            if (n_excluded > 0) {
              # Filter data to complete cases only
              depVarsData_complete <- depVarsData[complete_cases, , drop = FALSE]
              classVarData_complete <- classVarData[complete_cases]
            } else {
              depVarsData_complete <- depVarsData
              classVarData_complete <- classVarData
            }

            # Run enhanced DeLong's test with better error handling
            delongResults <- tryCatch(
              {
                private$.enhancedDelongTest(
                  data = depVarsData_complete,
                  classVar = classVarData_complete,
                  pos_class = positiveClass,
                  ref = NULL,
                  conf.level = 0.95
                )
              },
              error = function(e) {
                # jmvcore::reject() raises a PLAIN simpleError, so this catch-all used to
                # swallow every input-validation failure -- NA in the class variable, a wrong
                # positive class, fewer than two markers -- and then run the fallback on the
                # SAME invalid input while the note below claimed a validated pROC
                # implementation. Validation failures belong to the user, so re-raise them.
                if (identical(e$code, "delong_validation")) stop(e)
                # .checkpoint() signals a restart as an error too; never eat that.
                if (identical(e$code, "restart")) stop(e)
                # Fallback to original implementation if enhanced version fails
                private$.delongUsedFallback <- TRUE
                private$.warnUser(sprintf(
                  .("The enhanced DeLong comparison failed (%s), so a fallback implementation was used; the AUC differences and p-values below come from that fallback."),
                  conditionMessage(e)))
                private$.deLongTest(
                  data = depVarsData_complete,
                  classVar = classVarData_complete,
                  ref = NULL,
                  positiveClass = positiveClass,
                  conf.level = 0.95
                )
              }
            )

            # No setVisible() here. delongTest already carries `visible: (delongTest)` in
            # jamovi/psychopdaroc.r.yaml, and an imperative setVisible(TRUE) overrides that
            # binding with no counterpart to undo it -- so once the panel had been shown it
            # stayed on screen after the user unticked the option. Same defect, and the same
            # declarative fix, as criterionPlot / prevalencePlot / dotPlot /
            # precisionRecallPlot (see the .init() note). `delongTest` is now in both items'
            # clearWith so the stale content goes with it.

            # Add methodology note
            self$results$delongComparisonTable$setNote(
              key = "delong_method",
              note = if (isTRUE(private$.delongUsedFallback)) {
                .("DeLong's test was computed by this module's own fallback implementation, not by pROC, because the pROC path failed; see the warning above. Reference: DeLong ER, DeLong DM, Clarke-Pearson DL (1988). Comparing the areas under two or more correlated ROC curves: a nonparametric approach. Biometrics 44(3):837-845.")
              } else {
                .("DeLong's test uses the validated pROC package implementation. Reference: DeLong ER, DeLong DM, Clarke-Pearson DL (1988). Comparing the areas under two or more correlated ROC curves: a nonparametric approach. Biometrics 44(3):837-845.")
              },
              init = FALSE
            )

            # Add note about excluded cases if any
            if (n_excluded > 0) {
              self$results$delongComparisonTable$setNote(
                key = "missing_cases",
                # Was raw English; wrap the LITERAL, interpolate outside.
                # The filter is listwise across the class variable AND all markers, so the
                # note has to say so - it used to name only the classification variable.
                note = sprintf(.("Note: %d row(s) with a missing value in the classification variable or in any of the compared test variables were excluded from the DeLong test, so all curves are estimated on the same patients."), n_excluded),
                init = FALSE
              )
            }

            # DeLong's test is asymptotic: its variance estimate is a large-sample
            # approximation, and with few cases in either class the p-value and CI are not
            # dependable. Placed here rather than inside either helper so it covers both the
            # pROC path and the fallback. The floor of 10 per class is the conventional
            # minimum for a stable AUC variance (Hanley & McNeil 1982; Obuchowski 2004);
            # below it the note says so rather than silently printing a confident p-value.
            n_delong_pos <- sum(classVarData_complete == positiveClass, na.rm = TRUE)
            n_delong_neg <- sum(classVarData_complete != positiveClass, na.rm = TRUE)
            if (min(n_delong_pos, n_delong_neg) < 10) {
              self$results$delongComparisonTable$setNote(
                key = "delong_small_sample",
                # Was raw English assembled by paste0, which cannot be extracted; one literal now.
                note = sprintf(.("Small sample: %1$d positive and %2$d negative case(s). DeLong's test relies on a large-sample approximation to the variance of the AUC, so with fewer than 10 cases in a class the p-values and confidence intervals below are unreliable and are likely to be too narrow. Treat any difference as provisional and confirm it in a larger sample."),
                  n_delong_pos, n_delong_neg),
                init = FALSE
              )
            }

            delongResults$difference[["P.Holm"]] <- stats::p.adjust(
              delongResults$difference[["P.Value"]], method = "holm",
              n = nrow(delongResults$difference))
            multiplicity_note <- .("Raw and Holm-adjusted pairwise p-values are shown. Holm adjustment controls family-wise error across all displayed pairs under the individual tests' assumptions. The 95% confidence intervals are pointwise and unadjusted; the global test evaluates joint equality of the AUCs.")
            self$results$delongComparisonTable$setNote("multiplicity", multiplicity_note)
            self$results$delongTest$setContent(paste(c(
              private$.printEnhancedDeLong(delongResults), multiplicity_note), collapse = "\n\n"))

            # Format results for the DeLong comparison table
            delongTable <- self$results$delongComparisonTable

            # Extract pairwise comparisons from DeLong test results
            diff_data <- delongResults$difference

            if (is.na(delongResults$global.p) || any(!is.finite(diff_data[, "P.Value"]))) {
              self$results$delongComparisonTable$setNote("degenerate_contrast",
                .("Some DeLong inference is unavailable because a contrast has zero or non-estimable variance. AUC differences remain descriptive point estimates; blank intervals and p-values do not establish equality or a definite difference. This can occur with identical markers or perfect rankings."), init = FALSE)
            }

            for (i in seq_len(nrow(diff_data))) {
              comparison <- rownames(diff_data)[i]
              row_values <- list(
                comparison = delongResults$comparison_labels[i],
                auc_diff = diff_data[i, "AUC Difference"],
                ci_lower = diff_data[i, "CI(lower)"],
                ci_upper = diff_data[i, "CI(upper)"],
                # Use the z the test actually computed (diff / se). Back-computing
                # it from the p-value returned Inf whenever p underflowed to 0.
                z = if ("Z" %in% colnames(diff_data)) diff_data[i, "Z"]
                    else sign(diff_data[i, "AUC Difference"]) *
                         sqrt(qchisq(1 - min(diff_data[i, "P.Value"], 1 - 1e-16), df = 1)),
                p = diff_data[i, "P.Value"],
                p_holm = diff_data[i, "P.Holm"]
              )
              # jmvcore serializes numeric NA as NaN; logical NA is a missing cell.
              row_values <- lapply(row_values, function(value) {
                if (is.numeric(value) && length(value) == 1L && is.na(value)) NA else value
              })
              delongTable$addRow(rowKey = comparison, values = row_values)
            }
          }
        }

        # -----------------------------------------------------------------------
        # 11. CREATE SIMPLIFIED RESULTS TABLES
        # -----------------------------------------------------------------------

        # The duplicate summary table was REMOVED: it was column-for-column
        # identical to aucSummaryTable (variable, auc, ci_lower, ci_upper, p),
        # always visible, populated from the same loop and given the same
        # footnotes -- users saw the same AUC table twice in the default output.
        # Degenerate-estimate caveats use the per-marker complete cases below.
        .degen_const <- tryCatch({
          nm <- names(aucList)
          nm[vapply(nm, function(v) {
            col <- private$.prepareVarData(data, v, subGroup)$dependentVar
            is.numeric(col) && length(unique(col[!is.na(col)])) <= 1
          }, logical(1))]
        }, error = function(e) character(0))

        # CI method per variable (delong / hanley_mcneil), recorded by the AUC summary
        # loop below for the footnote; a second, identical CI pass used to run here.
        ci_methods_used <- list()
        effective_counts <- character(0)
        smallest_class <- Inf

        # Populate clinical interpretation table after simple table is complete
        private$.populateClinicalInterpretation()

        # Populate the AUC summary table
        aucSummaryTable <- self$results$aucSummaryTable
        private$.noteDirection(aucSummaryTable)
        private$.noteDirection(self$results$outcomeCoding, "reading")

        for (var in names(aucList)) {
          # Get AUC value directly from the list
          auc_value <- aucList[[var]]

          # Validation stays outside the third-party error handler. Both the primary
          # calculation and fallback must use these exact marker/outcome pairs.
          prepared <- private$.prepareVarData(data, var, subGroup)
          n_pos <- sum(prepared$classVar == positiveClass)
          n_neg <- sum(prepared$classVar != positiveClass)
          smallest_class <- min(smallest_class, n_pos, n_neg)
          effective_counts <- c(effective_counts, .fmt(
            .("{var}: {positive} positive, {negative} negative complete observations"),
            var = .rocLabel(var), positive = n_pos, negative = n_neg))

          # DeLong asymptotic AUC interval; finite-sample limitations are disclosed below.
          auc_lci <- NA
          auc_uci <- NA
          p_val <- NA

          valid_sample_size <- !is.na(n_pos) && !is.na(n_neg) && n_pos > 0 && n_neg > 0
          valid_auc <- !is.na(auc_value) && is.finite(auc_value) && auc_value >= 0 && auc_value <= 1

          if (valid_sample_size && valid_auc) {
            tryCatch(
              {
                pROC_direction <- ifelse(direction == ">=", "<", ">")
                roc_obj <- pROC::roc(
                  response = prepared$classVar,
                  predictor = prepared$dependentVar,
                  levels = c(
                    levels(as.factor(prepared$classVar))[levels(as.factor(prepared$classVar)) != positiveClass][1],
                    positiveClass
                  ),
                  direction = pROC_direction,
                  ci = FALSE,
                  quiet = TRUE
                )
                auc_se_delong <- sqrt(pROC::var(roc_obj))
                inference <- .psychopdaAUCInference(auc_value, auc_se_delong,
                  self$options$aucInference)
                auc_lci <- inference$ci[1]
                auc_uci <- inference$ci[2]
                p_val <- inference$p
                ci_methods_used[[var]] <- "delong"
              },
              error = function(e) {
                # Fallback to Hanley-McNeil if pROC fails
                q1 <- auc_value / (2 - auc_value)
                q2 <- 2 * auc_value^2 / (1 + auc_value)
                auc_se <- sqrt((auc_value * (1 - auc_value) +
                  (n_pos - 1) * (q1 - auc_value^2) +
                  (n_neg - 1) * (q2 - auc_value^2)) / (n_pos * n_neg))
                if (!is.na(auc_se) && auc_se > 0) {
                  inference <- .psychopdaAUCInference(auc_value, auc_se,
                    self$options$aucInference)
                  auc_lci <<- inference$ci[1]
                  auc_uci <<- inference$ci[2]
                  p_val <<- inference$p
                }
                if (is.null(ci_methods_used[[var]])) {
                  ci_methods_used[[var]] <<- "hanley_mcneil"
                }
              }
            )
          }

          # Add or update row in AUC summary table
          row_values <- list(
            variable = as.character(var),
            auc = auc_value,
            ci_lower = if (is.finite(auc_lci)) auc_lci else NA,
            ci_upper = if (is.finite(auc_uci)) auc_uci else NA,
            p = if (is.finite(p_val)) p_val else NA
          )
          tryCatch(
            aucSummaryTable$setRow(rowKey = var, values = row_values),
            error = function(e) {
              tryCatch(
                aucSummaryTable$addRow(rowKey = var, values = row_values),
                error = function(e2) NULL
              )
            }
          )
        }

        private$.noteDegenerateEstimates(aucSummaryTable, unlist(aucList),
          smallest_class, smallest_class, .degen_const)
        aucSummaryTable$setNote("effective_n", paste(effective_counts, collapse = "; "),
          init = FALSE)
        independence_note <- .("Inference assumes independent observations. These intervals, tests and bootstrap resamples do not account for repeated measurements or clustering within patients; use a prespecified patient-level summary or a method that accounts for clustering.")
        if (!identical(self$options$analysisUnit, "rows")) independence_note <- unit_note
        for (table in list(aucSummaryTable, self$results$delongComparisonTable,
          self$results$partialAUCTable, self$results$bootstrapCITable,
          self$results$nriTable, self$results$idiTable, self$results$bayesianROCTable)) {
          table$setNote("independent_observations", independence_note, init = FALSE)
        }

        # Surface worse-than-chance discrimination (AUC < 0.5) to the user.
        # A native Notice/HTML banner is not available at this point in the flow,
        # so a prominent table note is used (jamovi renders these under the table).
        # Same 3-dp reading as the "Below chance" band, so the note and the band agree.
        poor_disc_vars <- names(aucList)[vapply(
          aucList, function(a) isTRUE(is.finite(a) && round(a, 3) < 0.5), logical(1)
        )]
        if (length(poor_disc_vars) > 0) {
          # One literal msgid; the %s are a variable list, the positive level and two direction
          # symbols. Positive Class is checked first (see .noteDirection). Notes show entities
          # literally, so nothing is htmlEscaped: names go through .rocLabel, and "<=" is not a tag.
          alt_direction <- if (identical(self$options$direction, ">=")) "<=" else ">="
          poor_disc_note <- sprintf(
            .("WARNING: AUC below 0.5 (worse than chance) for: %1$s. First check Positive Class: is %2$s the level that means the condition is present? If it is, the marker either separates the groups in the opposite direction to the one in use or does not separate them at all (an uninformative marker falls below 0.5 about half the time by chance, so check whether the confidence interval excludes 0.5). Classification Direction is currently \"%3$s\"; switching it to \"%4$s\" gives an AUC of 1 minus the value shown and re-estimates the cutpoint, sensitivity and specificity. Change it only if that matches what the marker means clinically."),
            .rocLabel(paste(poor_disc_vars, collapse = ", ")),
            .rocLabel(positiveClass),
            self$options$direction,
            alt_direction
          )
          aucSummaryTable$setNote("auc_below_chance", poor_disc_note)
          private$.setNotice("belowChance",
            .("At least one AUC is below 0.5. Check Positive Class and Classification Direction against the clinical meaning of the marker."),
            jmvcore::NoticeType$WARNING)
        }

        # Add table footnotes explaining the CI method used
        fallback_vars <- names(ci_methods_used)[vapply(ci_methods_used, function(m) identical(m, "hanley_mcneil"), logical(1))]
        delong_vars <- names(ci_methods_used)[vapply(ci_methods_used, function(m) identical(m, "delong"), logical(1))]

        if (length(fallback_vars) > 0) {
          # Was raw English built by paste0 around the variable list; one literal msgid now.
          # %% escapes the literal percent sign for sprintf.
          fallback_note <- sprintf(
            .("DeLong intervals could not be computed for %s. The Hanley-McNeil approximation was used instead. Its distributional assumptions may not hold, and its intervals need not be conservative. Nominal coverage is not guaranteed, especially in modest or imbalanced samples."),
            paste(fallback_vars, collapse = ", ")
          )
          aucSummaryTable$setNote("ci_method_fallback", fallback_note)
          private$.setNotice("aucFallback",
            .("Some DeLong intervals could not be computed. The AUC table identifies the markers using a Hanley-McNeil approximation; interpret those intervals cautiously."),
            jmvcore::NoticeType$STRONG_WARNING)
        } else if (length(delong_vars) > 0) {
          # Was raw English.
          aucSummaryTable$setNote("ci_method", .("AUC 95% confidence intervals and tests use the asymptotic DeLong approximation. Nominal coverage and type-I error are not guaranteed in modest or imbalanced samples, even with ten or more observations per class. Interpret the uncertainty cautiously; a bootstrap interval is not automatically a remedy (Obuchowski and Lieber, 1998)."))
        }
        aucSummaryTable$setNote("inference_scale", if (identical(self$options$aucInference, "logit"))
          .("Single-AUC 95% intervals and two-sided tests against AUC 0.5 use a logit delta transformation of the DeLong standard error (or the explicitly labelled fallback standard error). This improves the audited small-sample coverage but remains an asymptotic approximation. It does not change paired DeLong comparisons. Boundary AUCs or zero variance have unavailable inference.")
        else .("Single-AUC inference uses the legacy untransformed normal approximation. Boundary AUCs or zero variance have unavailable inference. This method undercovered in the audited small, imbalanced sample."))

        aucSummaryTable$setNote("auc_bands",
          .("The minimal/poor/fair/good/excellent AUC bands are descriptive conventions, not validated clinical decision thresholds. Clinical value depends on the intended cutoff, uncertainty, case mix and consequences of errors."), init = FALSE)

        # Name the method behind the pointwise intervals drawn on the ROC plot. They are a
        # different calculation from the AUC interval above, and previously nothing on
        # screen said what they were.
        if (isTRUE(self$options$plotROC) && !isTRUE(self$options$cleanPlot) &&
            (isTRUE(self$options$showConfidenceBands) || length(private$.quantileProbs) > 0L)) {
          aucSummaryTable$setNote(
            "roc_pointwise_ci",
            .("Confidence bands on the ROC plot are pointwise 95% Wilson score intervals for sensitivity; quantile error bars are pointwise 95% Wilson score intervals for both sensitivity and specificity. Each is computed separately at its operating point, so neither is a simultaneous confidence region for the curve as a whole."))
        }

        # -----------------------------------------------------------------------
        # 12. CREATE THRESHOLD TABLE IF REQUESTED
        # -----------------------------------------------------------------------

        if (self$options$showThresholdTable) {
          # Checkpoint before expensive threshold table generation
          private$.checkpoint()
          thresholdTable <- self$results$thresholdTable
          .deleteTableRows(thresholdTable) # Clear previous results

          for (var in names(private$.rocDataList)) {
            rocData <- private$.rocDataList[[var]]
            # Get prevalence for this variable
            prevalence <- private$.prevalenceList[[var]]

            # Select a reasonable number of thresholds to display
            indices <- .psychopdaThresholdIndices(nrow(rocData),
              which.max(rocData$youden), self$options$maxThresholds)

            # Add rows to threshold table
            for (i in indices) {
              # Calculate likelihood ratios
              plr <- rocData$sensitivity[i] / (1 - rocData$specificity[i])
              nlr <- (1 - rocData$sensitivity[i]) / rocData$specificity[i]

              # Avoid division by zero in likelihood ratios
              if (!is.finite(plr)) plr <- NA
              if (!is.finite(nlr)) nlr <- NA

              # Accuracy is empirical: sens*p + spec*(1-p) at the SAMPLE prevalence collapses to
              # (TP+TN)/n. It must not follow the user's prior - that is documented as applying to
              # predictive values, and letting it through inflated the headline accuracy whenever
              # the assumed population was mostly negatives, which reads as the test improving.
              sample_prev <- private$.samplePrevalenceList[[var]]
              if (is.null(sample_prev) || !is.finite(sample_prev)) sample_prev <- prevalence
              accuracy <- (rocData$sensitivity[i] * sample_prev) +
                (rocData$specificity[i] * (1 - sample_prev))

              # The marker identity used to survive only in the invisible rowKey, so with
              # two markers the table was ~40 undifferentiated rows and a cut-off could be
              # transcribed against the wrong antibody.
              thresholdTable$addRow(rowKey = paste0(var, "_", i), values = list(
                variable = var,
                threshold = rocData$threshold[i],
                sensitivity = rocData$sensitivity[i],
                specificity = rocData$specificity[i],
                accuracy = accuracy,
                ppv = rocData$ppv[i],
                npv = rocData$npv[i],
                plr = plr,
                nlr = nlr,
                youden = rocData$youden[i]
              ))
            }
          }
        }


        # -----------------------------------------------------------------------
        # 13. HANDLE ADDITIONAL ANALYSES
        # -----------------------------------------------------------------------

        # Calculate partial AUC if requested
        if (self$options$partialAUC) {
          # Checkpoint before expensive partial AUC calculations
          private$.checkpoint()
          self$results$partialAUCTable$setNote("seed", if (self$options$bootstrapCI) .fmt(.("Random seed: {seed}"), seed = self$options$seed), init = FALSE)
          self$results$partialAUCTable$setNote("interval_scope",
            .("The bootstrap confidence limits refer to the raw partial AUC, not the normalized partial AUC."), init = FALSE)

          # Table visibility is controlled by YAML: visible: (partialAUC)

          for (var in vars) {
            # Get the necessary data
            prepared <- private$.prepareVarData(data, var, subGroup)
            dependentVar <- prepared$dependentVar
            classVar <- prepared$classVar

            # Calculate partial AUC
            pAUC_results <- private$.calculatePartialAUC(
              x = dependentVar,
              class = classVar,
              positiveClass = positiveClass,
              from = self$options$partialAUCfrom,
              to = self$options$partialAUCto
            )

            # Add to table
            if (!is.null(pAUC_results)) {
              self$results$partialAUCTable$addRow(rowKey = var, values = list(
                variable = var,
                pAUC = pAUC_results$pAUC,
                pAUC_normalized = pAUC_results$pAUC_normalized,
                ci_lower = pAUC_results$ci_lower,
                ci_upper = pAUC_results$ci_upper,
                spec_range = pAUC_results$spec_range
              ))
            }
          }
        }

        # Apply ROC curve smoothing if requested
        if (self$options$rocSmoothingMethod != "none") {
          # Checkpoint before expensive ROC smoothing operations
          private$.checkpoint()

          for (var in vars) {
            # Get the necessary data
            prepared <- private$.prepareVarData(data, var, subGroup)
            dependentVar <- prepared$dependentVar
            classVar <- prepared$classVar

            # Create smoothed ROC curve
            smooth_results <- private$.createSmoothedROC(
              x = dependentVar,
              class = classVar,
              positiveClass = positiveClass,
              method = self$options$rocSmoothingMethod
            )

            # Store smoothed ROC curve data for plotting
            if (!is.null(smooth_results)) {
              # Store for plotting
              threshold_val <- if (is.null(smooth_results$threshold) || length(smooth_results$threshold) == 0) NA else smooth_results$threshold
              private$.smoothedRocDataList[[var]] <- data.frame(
                threshold = threshold_val,
                sensitivity = smooth_results$sensitivity,
                specificity = smooth_results$specificity,
                var = var,
                smoothed = TRUE,
                method = self$options$rocSmoothingMethod
              )
            }
          }
        }

        # The smoothed curves exist only now; refresh the ROC image states so they carry them.
        if (self$options$rocSmoothingMethod != "none") private$.attachRocOverlayState()

        # Calculate bootstrap confidence intervals if requested
        if (self$options$bootstrapCI) {
          # Checkpoint before expensive bootstrap calculations
          private$.checkpoint()
          self$results$bootstrapCITable$setNote("seed", .fmt(.("Random seed: {seed}"), seed = self$options$seed), init = FALSE)
          self$results$bootstrapCITable$setNote("estimand",
            .("AUC intervals refer to the full AUC. Threshold, sensitivity and specificity describe a separate Youden reference, not the main table's selected cutoff method. Youden is reselected in each stratified bootstrap sample; tied optima are selected randomly using the stated seed, including for the point estimate. These are percentile intervals, not externally validated performance intervals."), init = FALSE)

          # Table visibility is controlled by YAML: visible: (bootstrapCI)

          for (var in vars) {
            # Get the necessary data
            prepared <- private$.prepareVarData(data, var, subGroup)
            dependentVar <- prepared$dependentVar
            classVar <- prepared$classVar

            # Calculate bootstrap CIs
            bootstrap_results <- private$.calculateBootstrapCI(
              x = dependentVar,
              class = classVar,
              positiveClass = positiveClass
            )

            # Add to table
            if (!is.null(bootstrap_results)) {
              for (param in names(bootstrap_results)) {
                est <- bootstrap_results[[param]]$estimate
                if (is.null(est) || length(est) == 0) est <- NA
                if (length(est) > 1) est <- est[1]

                lower <- bootstrap_results[[param]]$ci_lower
                if (is.null(lower) || length(lower) == 0) lower <- NA
                if (length(lower) > 1) lower <- lower[1]

                upper <- bootstrap_results[[param]]$ci_upper
                if (is.null(upper) || length(upper) == 0) upper <- NA
                if (length(upper) > 1) upper <- upper[1]

                tryCatch(
                  {
                    self$results$bootstrapCITable$addRow(rowKey = paste0(var, "_", param), values = list(
                      variable = var,
                      parameter = param,
                      estimate = est,
                      ci_lower = lower,
                      ci_upper = upper
                    ))
                  },
                  error = function(e) {
                    # A silently dropped row read to the user as "this marker was not
                    # eligible" rather than "this marker errored". Route it through the
                    # Analysis Status box, which is always visible.
                    private$.warnUser(.fmt(
                      .("Bootstrap CI for {param} of '{var}' could not be added to the table ({msg})."),
                      param = param, var = var, msg = conditionMessage(e)))
                  }
                )
              }
            }
          }
        }

        # Calculate precision-recall curves if requested
        if (self$options$precisionRecallCurve) {
          # Checkpoint before expensive precision-recall calculations
          private$.checkpoint()

          # Initialize precision-recall plot array
          if (self$options$combinePlots) {
            # Add a single item for combined plot (addItem has no duplicate check)
            if (!1 %in% self$results$precisionRecallPlot$itemKeys)
              self$results$precisionRecallPlot$addItem(key = 1)
            pr_plot_data <- data.frame()
          }

          for (var in vars) {
            # Get the necessary data
            prepared <- private$.prepareVarData(data, var, subGroup)
            dependentVar <- prepared$dependentVar
            classVar <- prepared$classVar

            # Calculate precision-recall
            pr_results <- private$.calculatePrecisionRecall(
              x = dependentVar,
              class = classVar,
              positiveClass = positiveClass
            )

            if (!is.null(pr_results)) {
              if (self$options$combinePlots) {
                # Add to combined data
                pr_plot_data <- rbind(
                  pr_plot_data,
                  data.frame(
                    threshold = pr_results$threshold,
                    precision = pr_results$precision,
                    recall = pr_results$recall,
                    auprc = rep(pr_results$auprc, length(pr_results$threshold)),
                    var = rep(var, length(pr_results$threshold))
                  )
                )
              } else {
                # Create individual plot (addItem has no duplicate check)
                if (!var %in% self$results$precisionRecallPlot$itemKeys)
                  self$results$precisionRecallPlot$addItem(key = var)
                pr_plot <- self$results$precisionRecallPlot$get(key = var)
                pr_plot$setTitle(paste0("Precision-Recall Curve: ", var))
                pr_plot$setState(
                  data.frame(
                    threshold = pr_results$threshold,
                    precision = pr_results$precision,
                    recall = pr_results$recall,
                    auprc = rep(pr_results$auprc, length(pr_results$threshold))
                  )
                )
              }
            }
          }

          # Set state for combined plot if using
          if (self$options$combinePlots && nrow(pr_plot_data) > 0) {
            pr_plot <- self$results$precisionRecallPlot$get(key = 1)
            pr_plot$setTitle("Precision-Recall Curves: Combined")
            pr_plot$setState(pr_plot_data)
          }
        }

        # Compare classifier performance if requested
        if (self$options$compareClassifiers) {
          self$results$rocComparisonTable$setNote("classifier_rule",
            .("F1 and accuracy use a single Youden reference cutoff; ties choose the greatest sensitivity. This is separate from the main table's cutoff method. Brier score assumes that input values between 0 and 1 are positive-class probabilities; a normalized assay is not necessarily a probability. All performance estimates are apparent, in-sample values."), init = FALSE)
          # Set up comparison table
          # Table visibility is controlled by YAML: visible: (compareClassifiers)

          for (var in vars) {
            # Get the necessary data
            prepared <- private$.prepareVarData(data, var, subGroup)
            dependentVar <- prepared$dependentVar
            classVar <- prepared$classVar

            # Calculate classifier metrics
            metrics <- private$.calculateClassifierMetrics(
              x = dependentVar,
              class = classVar,
              positiveClass = positiveClass
            )

            # Add to table
            if (!is.null(metrics)) {
              self$results$rocComparisonTable$addRow(rowKey = var, values = list(
                variable = var,
                auc = metrics$auc,
                auprc = metrics$auprc,
                brier = metrics$brier,
                f1_score = metrics$f1_score,
                accuracy = metrics$accuracy,
                balanced_accuracy = metrics$balanced_accuracy
              ))
            }
          }
        }

        # -----------------------------------------------------------------------
        # 14. CALCULATE IDI AND NRI IF REQUESTED
        # -----------------------------------------------------------------------

        # Calculate IDI and NRI if requested
        if (self$options$calculateIDI || self$options$calculateNRI) {
          # (enough tests: checked in .validateInputs(), which counts a separate reference test)
          # both are bootstrap estimates
          if (self$options$calculateIDI) self$results$idiTable$setNote("seed", .fmt(.("Random seed: {seed}"), seed = self$options$seed), init = FALSE)
          if (self$options$calculateNRI) self$results$nriTable$setNote("seed", .fmt(.("Random seed: {seed}"), seed = self$options$seed), init = FALSE)
          for (table in list(self$results$idiTable, self$results$nriTable)) {
            table$setNote("descriptive_inference",
              .("IDI/NRI compare apparent risks from separate logistic models fitted and evaluated on these data. Intervals are exploratory percentile bootstrap intervals with both models refitted per resample. No p-value is reported: bootstrap tail fractions are not a validated null test here. These results do not establish incremental clinical value or external calibration."))
          }

          # IDI/NRI does not support subgroup analysis. Set on every run (NULL removes it) with the
          # default init: neither table clears with subGroup, so an init = FALSE copy outlived it.
          sgIdiNri <- if (!is.null(self$options$subGroup) && self$options$subGroup != "")
            .("IDI/NRI calculations do not support subgroup analysis; these results are computed on the full sample, not per subgroup.")
          self$results$idiTable$setNote("subgroup_warning", sgIdiNri)
          self$results$nriTable$setNote("subgroup_warning", sgIdiNri)

          # Get reference variable
          using_default_ref <- FALSE
          refVar_val <- tryCatch(self$options$refVar, error = function(e) NULL)
          if (is.null(refVar_val) || refVar_val == "") {
            refVar <- self$options$dependentVars[1]
            using_default_ref <- TRUE
          } else {
            refVar <- refVar_val
          }

          # Escape variable names for safe data access
          classVarEscaped <- private$.escapeVar(self$options$classVar)

          # Get actual class values and convert to binary
          classVar <- data[[classVarEscaped]]
          actual_binary <- private$.classIndicator(data, positiveClass)

          # Get direction
          direction <- self$options$direction

          # Get bootstrap runs
          boot_runs <- as.integer(self$options$idiNriBootRuns)

          # Calculate IDI if requested
          if (self$options$calculateIDI) {
            # Get reference variable values with escaped name
            refVarEscaped <- private$.escapeVar(refVar)
            ref_values <- jmvcore::toNumeric(data[[refVarEscaped]])

            # For each other variable
            for (var in self$options$dependentVars) {
              if (var != refVar) {
                varEscaped <- private$.escapeVar(var)
                var_values <- jmvcore::toNumeric(data[[varEscaped]])

                # Filter complete cases across all three vectors
                cc <- complete.cases(var_values, ref_values, actual_binary)
                if (sum(cc) < 10) {
                  self$results$idiTable$addRow(rowKey = var, values = list(
                    variable = var, refVar = refVar,
                    idi = NaN, ci_lower = NaN, ci_upper = NaN, p = NaN
                  ))
                  self$results$idiTable$setNote(
                    key = paste0("insufficient_", make.names(var)),
                    note = .fmt(
                      .("IDI for {variable} was not computed because fewer than 10 complete cases were available."),
                      variable = var
                    ),
                    init = FALSE
                  )
                  next
                }

                # Calculate IDI
                idi_warnings <- character(0)
                idi_result <- withCallingHandlers(
                  bootstrapIDI(
                    new_values = var_values[cc],
                    ref_values = ref_values[cc],
                    actual = actual_binary[cc],
                    direction = direction,
                    n_boot = boot_runs
                  ),
                  warning = function(w) {
                    idi_warnings <<- c(idi_warnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                  }
                )

                if (length(idi_warnings) > 0L) {
                  self$results$idiTable$setNote(
                    key = paste0("fit_", make.names(var)),
                    note = .fmt(
                      .("IDI caution for {variable}: {details}"),
                      variable = var,
                      details = paste(unique(idi_warnings), collapse = " ")
                    ),
                    init = FALSE
                  )
                }

                # Add to IDI table
                self$results$idiTable$addRow(rowKey = var, values = list(
                  variable = var,
                  refVar = refVar,
                  idi = idi_result$idi,
                  ci_lower = idi_result$ci_lower,
                  ci_upper = idi_result$ci_upper,
                  p = NA
                ))
              }
            }
          }

          # Calculate NRI if requested
          if (self$options$calculateNRI) {
            # Get reference variable values with escaped name
            refVarEscaped <- private$.escapeVar(refVar)
            ref_values <- jmvcore::toNumeric(data[[refVarEscaped]])

            # Parse thresholds string if provided
            thresholds <- NULL
            if (!is.null(self$options$nriThresholds) && self$options$nriThresholds != "") {
              parsed <- .psychopdaProbabilityList(self$options$nriThresholds, endpoints = FALSE)
              thresholds <- parsed$values
              # A malformed string (semicolons, percentages, a decimal comma) parses to an
              # empty vector, and computeNRI() reads length 0 as a request for CONTINUOUS
              # NRI - so the analysis silently switched method under an unchanged heading.
              # Categorical and continuous NRI routinely differ by a factor of two or more.
              if (parsed$kept < parsed$given) {
                private$.warnUser(.fmt(
                  .("Only {kept} of the {given} NRI risk threshold(s) entered could be read as a number strictly between 0 and 1. Enter them as comma-separated proportions, for example: 0.2, 0.5"),
                  kept = parsed$kept, given = parsed$given))
              }
              if (parsed$duplicates) {
                private$.warnUser(.("Repeated NRI risk category boundaries were removed. The sorted, distinct boundaries used are listed below the NRI table."))
              }
            }

            # State which NRI variant was actually computed, next to the numbers.
            self$results$nriTable$setNote(
              key = "nri_variant",
              note = if (length(thresholds) > 0) {
                .fmt(
                  .("Categorical NRI, using risk category boundaries at {thr}."),
                  thr = paste(thresholds, collapse = ", "))
              } else {
                .("Continuous (category-free) NRI: no usable risk category boundaries were supplied, so reclassification counts changes in predicted risk rather than crossings of category boundaries.")
              },
              init = FALSE
            )
            self$results$nriTable$setNote("numerical_ties",
              .("Predicted risks differing by at most sqrt(machine epsilon), approximately 1.5e-8, are treated as numerical ties in both continuous and categorical NRI. This prevents numerical roundoff from becoming reclassification and is not a threshold for clinical importance."), init = FALSE)

            # For each other variable
            for (var in self$options$dependentVars) {
              if (var != refVar) {
                varEscaped <- private$.escapeVar(var)
                var_values <- jmvcore::toNumeric(data[[varEscaped]])

                # Filter complete cases across all three vectors
                cc <- complete.cases(var_values, ref_values, actual_binary)
                if (sum(cc) < 10) {
                  self$results$nriTable$addRow(rowKey = var, values = list(
                    variable = var, refVar = refVar,
                    nri = NaN, event_nri = NaN, non_event_nri = NaN,
                    ci_lower = NaN, ci_upper = NaN, p = NaN
                  ))
                  self$results$nriTable$setNote(
                    key = paste0("insufficient_", make.names(var)),
                    note = .fmt(
                      .("NRI for {variable} was not computed because fewer than 10 complete cases were available."),
                      variable = var
                    ),
                    init = FALSE
                  )
                  next
                }

                # Calculate NRI
                nri_warnings <- character(0)
                nri_result <- withCallingHandlers(
                  bootstrapNRI(
                    new_values = var_values[cc],
                    ref_values = ref_values[cc],
                    actual = actual_binary[cc],
                    direction = direction,
                    thresholds = thresholds,
                    n_boot = boot_runs
                  ),
                  warning = function(w) {
                    nri_warnings <<- c(nri_warnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                  }
                )

                if (length(nri_warnings) > 0L) {
                  self$results$nriTable$setNote(
                    key = paste0("fit_", make.names(var)),
                    note = .fmt(
                      .("NRI caution for {variable}: {details}"),
                      variable = var,
                      details = paste(unique(nri_warnings), collapse = " ")
                    ),
                    init = FALSE
                  )
                }

                # Add to NRI table
                self$results$nriTable$addRow(rowKey = var, values = list(
                  variable = var,
                  refVar = refVar,
                  nri = nri_result$nri,
                  event_nri = nri_result$event_nri,
                  non_event_nri = nri_result$non_event_nri,
                  ci_lower = nri_result$ci_lower,
                  ci_upper = nri_result$ci_upper,
                  p = NA
                ))
              }
            }
          }

          # Add note if using default reference variable
          if (using_default_ref) {
            note_text <- sprintf(.("Reference test: %1$s, the first test variable, because Reference Test for IDI/NRI is empty. To compare against another test, put it in Reference Test for IDI/NRI."), .rocLabel(refVar))
            if (self$options$calculateIDI) {
              self$results$idiTable$setNote(
                key = "default_ref",
                note = note_text,
                init = FALSE
              )
            }
            if (self$options$calculateNRI) {
              self$results$nriTable$setNote(
                key = "default_ref",
                note = note_text,
                init = FALSE
              )
            }
          }
        }

        # -----------------------------------------------------------------------
        # ADVANCED STATISTICAL ANALYSES
        # -----------------------------------------------------------------------

        # These analyses read the whole class column and each whole marker, so they pool the
        # subgroups that split every ROC table above. Say so on each table; set on every run (NULL
        # removes it) with the default init, as none of these tables clears with subGroup.
        sgNote <- if (!is.null(self$options$subGroup)) .("Computed on the full sample: the subgroup variable is not applied to this table.")
        for (tbl in list(self$results$effectSizeTable, self$results$powerAnalysisTable,
                         self$results$bayesianROCTable, self$results$clinicalUtilityTable,
                         self$results$decisionCurveTable, self$results$metaAnalysisTable))
          tbl$setNote("subgroup_scope", sgNote)

        # Effect size analysis
        if (self$options$effectSizeAnalysis) {
          # Checkpoint before expensive effect size computation
          private$.checkpoint()
          private$.calculateEffectSizes(data, classVar, positiveClass)
        }

        # Power analysis
        if (self$options$powerAnalysis) {
          # Checkpoint before expensive power analysis computation
          private$.checkpoint()
          private$.calculatePowerAnalysis(data, classVar, positiveClass)
        }

        # Bayesian ROC analysis
        if (self$options$bayesianAnalysis) {
          # Checkpoint before expensive Bayesian analysis computation
          private$.checkpoint()
          private$.calculateBayesianROC(data, classVar, positiveClass)
        }

        # Clinical utility analysis (decision curves)
        if (self$options$clinicalUtilityAnalysis) {
          private$.checkpoint()
          private$.calculateClinicalUtility(data, classVar, positiveClass)
        }

        # Meta-analysis
        if (self$options$metaAnalysis) {
          private$.calculateMetaAnalysis()
        }
      },

      # ============================================================================
      # PLOTTING METHODS
      # ============================================================================

      # Plot ROC curves
      # @param image The image object
      # @param ggtheme The ggplot theme to use
      # @param theme Additional theme elements
      # @param ... Additional parameters
      # library-audit 2026-09-16 meddecide [MEDIUM] DONE: every ROC overlay draws from image$state, never
      #   private$.rocDataList - export and .omv reopen call .plotROC() without .run()
      # Wraps each ROC image's curve with the small inputs its overlays need: per-marker class counts
      # (confidence bands, quantile CIs), predictor quantiles at the requested probabilities, and the
      # smoothed curve. Counts and quantiles, not the predictor values, so the .omv holds no raw data.
      .attachRocOverlayState = function() {
        probs <- private$.quantileProbs
        plots <- self$results$plotROC
        for (key in plots$itemKeys) {
          image <- plots$get(key = key)
          st <- image$state
          if (is.null(st)) next
          curve <- if (is.data.frame(st)) st else st$curve
          if (is.null(curve) || nrow(curve) == 0) next
          counts <- NULL
          quantiles <- NULL
          smooth <- NULL
          for (v in unique(as.character(curve$var))) {
            raw <- attr(private$.rocDataList[[v]], "rawData")
            if (!is.null(raw)) {
              counts <- rbind(counts, data.frame(
                var = v, n_pos = sum(raw$class == "Positive", na.rm = TRUE),
                n_neg = sum(raw$class == "Negative", na.rm = TRUE), stringsAsFactors = FALSE))
              if (length(probs) > 0) {
                quantiles <- rbind(quantiles, data.frame(
                  var = v, prob = probs,
                  value = unname(quantile(raw$value, probs = probs, na.rm = TRUE)),
                  stringsAsFactors = FALSE))
              }
            }
            sm <- private$.smoothedRocDataList[[v]]
            if (!is.null(sm)) smooth <- rbind(smooth, as.data.frame(sm))
          }
          image$setState(list(curve = as.data.frame(curve), counts = counts,
                              quantiles = quantiles, smooth = smooth))
        }
      },

      # Wilson score interval for a binomial proportion, vectorised over p.
      # The Wald interval used before (p +- z*sqrt(p(1-p)/n)) has ZERO width at p = 0 and
      # p = 1 - exactly the two endpoints every ROC curve passes through - so the band
      # pinched shut precisely where the uncertainty is largest, and it under-covers badly
      # in small samples. Wilson never collapses, stays inside [0, 1] and is the standard
      # recommendation (Wilson 1927; Agresti & Coull 1998; Brown, Cai & DasGupta 2001).
      .wilsonCI = function(p, n, z = 1.959964) {
        if (!is.finite(n) || n < 1) {
          return(list(lower = rep(NA_real_, length(p)), upper = rep(NA_real_, length(p))))
        }
        centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
        half <- (z / (1 + z^2 / n)) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
        list(lower = pmax(0, centre - half), upper = pmin(1, centre + half))
      },

      .plotROC = function(image, ggtheme, theme = NULL, ...) {
        state <- image$state
        # list(curve, counts, quantiles, smooth) - see .attachRocOverlayState(). A bare data frame is
        # the curve-only state of older saved analyses: it draws without overlays.
        overlay <- if (is.data.frame(state)) list() else state
        plotData <- data.frame(if (is.data.frame(state) || is.null(state$curve)) state else state$curve)

        if (nrow(plotData) == 0) {
          return(FALSE)
        }

        marker_levels <- sort(unique(as.character(plotData$var)))
        marker_colours <- stats::setNames(jmvcore::colorPalette(length(marker_levels),
          if (is.null(theme$palette)) "jmv" else theme$palette, "color"), marker_levels)

        # Determine if we're creating a combined or individual plot
        if (self$options$combinePlots == TRUE && length(unique(plotData$var)) > 1) {
          # Multiple variables in one plot
          plot <- ggplot2::ggplot(
            plotData,
            ggplot2::aes(
              x = 1 - specificity,
              y = sensitivity,
              color = var,
              linetype = var
            )
          ) +
            ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
            ggplot2::geom_line(linewidth = 1) +
            # Follow jamovi's global palette instead of a hardcoded Brewer "Set1", so a
            # document mixing this with other ClinicoPath plots gets one set of series
            # colours. theme is NULL only outside jamovi (tests), hence the fallback.
            ggplot2::scale_color_manual(values = marker_colours) +
            ggplot2::scale_linetype_manual(values = rep(c("solid", "dashed", "dotted", "longdash"),
              length.out = length(unique(plotData$var))
            ))
        } else {
          # Single variable plot
          plot <- ggplot2::ggplot(
            plotData,
            ggplot2::aes(
              x = 1 - specificity,
              y = sensitivity
            )
          ) +
            ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
            ggplot2::geom_line(linewidth = 1, colour = unname(marker_colours[1])) +
            ggplot2::geom_point(size = 0.5, colour = unname(marker_colours[1]),
              alpha = ifelse(self$options$cleanPlot, 0, 0.7))
        }

        # Add common elements
        plot <- plot + ggplot2::scale_fill_manual(values = marker_colours) +
          ggplot2::xlab(.("1 - Specificity (False Positive Rate)")) +
          ggplot2::ylab(.("Sensitivity (True Positive Rate)")) +
          ggplot2::xlim(0, 1) +
          ggplot2::ylim(0, 1)

        # ggtheme is jamovi's global theme and REPLACES every earlier theme(), so
        # theme_minimal() on the clean-plot path discarded it entirely - under a dark
        # jamovi theme a clean-mode ROC plot rendered as a light panel with a black border
        # in an otherwise dark results pane. Apply ggtheme FIRST, then layer the clean-mode
        # tweaks on top of it. The border colour is left to the theme (element_rect with no
        # colour inherits it) rather than forced to black.
        plot <- plot + ggtheme
        if (self$options$cleanPlot) {
          plot <- plot +
            ggplot2::theme(
              panel.grid.minor = ggplot2::element_blank(),
              plot.title = ggplot2::element_blank(),
              plot.subtitle = ggplot2::element_blank(),
              panel.border = ggplot2::element_rect(fill = NA)
            )
        }

        # Legend positioning. This used to live INSIDE the `if (cleanPlot)`
        # branch, so at the default cleanPlot = FALSE every legendPosition value
        # (including "none") changed nothing at all. It applies to both themes.
        if (self$options$combinePlots && length(unique(plotData$var)) > 1) {
          if (self$options$legendPosition == "none") {
            plot <- plot + ggplot2::theme(legend.position = "none")
          } else {
            legend_pos <- self$options$legendPosition
            # Map custom positions to ggplot2 coordinate pairs
            if (legend_pos == "topleft") {
              plot <- plot + ggplot2::theme(
                legend.position = c(0.02, 0.98),
                legend.justification = c(0, 1),
                legend.title = ggplot2::element_blank(),
                legend.key = ggplot2::element_rect(fill = "white"),
                legend.background = ggplot2::element_rect(fill = "white", color = NA)
              )
            } else if (legend_pos == "topright") {
              plot <- plot + ggplot2::theme(
                legend.position = c(0.98, 0.98),
                legend.justification = c(1, 1),
                legend.title = ggplot2::element_blank(),
                legend.key = ggplot2::element_rect(fill = "white"),
                legend.background = ggplot2::element_rect(fill = "white", color = NA)
              )
            } else if (legend_pos == "bottomleft") {
              plot <- plot + ggplot2::theme(
                legend.position = c(0.02, 0.02),
                legend.justification = c(0, 0),
                legend.title = ggplot2::element_blank(),
                legend.key = ggplot2::element_rect(fill = "white"),
                legend.background = ggplot2::element_rect(fill = "white", color = NA)
              )
            } else if (legend_pos == "bottomright") {
              plot <- plot + ggplot2::theme(
                legend.position = c(0.98, 0.02),
                legend.justification = c(1, 0),
                legend.title = ggplot2::element_blank(),
                legend.key = ggplot2::element_rect(fill = "white"),
                legend.background = ggplot2::element_rect(fill = "white", color = NA)
              )
            } else {
              plot <- plot + ggplot2::theme(
                legend.position = legend_pos,
                legend.title = ggplot2::element_blank(),
                legend.key = ggplot2::element_rect(fill = "white"),
                legend.background = ggplot2::element_rect(fill = "white", color = NA)
              )
            }
          }
        }

        # Smoothed curves for the markers on this panel, from the image state
        smoothed_data <- if (is.null(overlay$smooth)) data.frame() else as.data.frame(overlay$smooth)

        if (nrow(smoothed_data) > 0 && self$options$rocSmoothingMethod != "none") {
          if (nrow(smoothed_data) > 0) {
            # Add smoothed curves
            if (self$options$combinePlots && length(unique(smoothed_data$var)) > 1) {
              # For combined plot
              plot <- plot +
                ggplot2::geom_line(
                  data = smoothed_data,
                  ggplot2::aes(
                    x = 1 - specificity,
                    y = sensitivity,
                    color = var
                  ),
                  linetype = "solid",
                  linewidth = 1.5,
                  alpha = 0.8
                )
            } else {
              # For individual plot
              plot <- plot +
                ggplot2::geom_line(
                  data = smoothed_data,
                  ggplot2::aes(
                    x = 1 - specificity,
                    y = sensitivity
                  ),
                  linetype = "solid",
                  linewidth = 1.5,
                  color = unname(marker_colours[1]),
                  alpha = 0.8
                )
            }

            # Add annotation about smoothing
            plot <- plot +
              ggplot2::annotate(
                "text",
                x = 0.25,
                y = 0.1,
                label = paste("Smoothing:", self$options$rocSmoothingMethod),
                hjust = 0,
                alpha = 0.7
              )
          }
        }

        # Add smoothing if requested
        if (self$options$smoothing) {
          plot <- plot + if (length(marker_levels) > 1L) {
            ggplot2::geom_smooth(ggplot2::aes(fill = var), method = "loess",
              formula = y ~ x, se = self$options$displaySE, level = 0.95)
          } else {
            ggplot2::geom_smooth(method = "loess", formula = y ~ x,
              se = self$options$displaySE, level = 0.95,
              colour = unname(marker_colours[1]), fill = unname(marker_colours[1]))
          }
        }

        # Mark optimal points if not clean plot or if specifically requested
        # `!cleanPlot || showOptimalPoint` meant that with the DEFAULT cleanPlot =
        # FALSE the first clause was always TRUE, so unticking "Show optimal point"
        # did nothing at all. The checkbox is the control; honour it.
        if (self$options$showOptimalPoint) {
          if (self$options$combinePlots == TRUE && length(unique(plotData$var)) > 1) {
            # For combined plot, find optimal points for each variable
            optimal_points <- data.frame()
            for (var_name in unique(plotData$var)) {
              var_data <- plotData[plotData$var == var_name, ]
              j_max_idx <- unique(var_data$j_max_idx)
              if (length(j_max_idx) == 1L && !is.na(j_max_idx)) {
                optimal_points <- rbind(optimal_points, var_data[j_max_idx, ])
              }
            }

            if (nrow(optimal_points) > 0) {
              plot <- plot +
                ggplot2::geom_point(
                  data = optimal_points,
                  ggplot2::aes(
                    x = 1 - specificity,
                    y = sensitivity,
                    color = var
                  ),
                  size = 3, shape = 18
                )
            }
          } else {
            # For single variable plot
            if ("j_max_idx" %in% names(plotData)) {
              j_max_idx <- unique(plotData$j_max_idx)
              if (length(j_max_idx) == 1 && !is.na(j_max_idx)) {
                # Add optimal point
                plot <- plot +
                  ggplot2::geom_point(
                    data = plotData[j_max_idx, ],
                    ggplot2::aes(
                      x = 1 - specificity,
                      y = sensitivity
                    ),
                    size = 3, shape = 18, color = "red"
                  )

                # Add annotation if not clean plot
                if (!self$options$cleanPlot) {
                  plot <- plot +
                    ggplot2::annotate(
                      "text",
                      x = 1 - plotData$specificity[j_max_idx] + 0.05,
                      y = plotData$sensitivity[j_max_idx],
                      label = paste("J =", round(plotData$youden[j_max_idx], 3)),
                      hjust = 0
                    )
                }
              }
            }
          }
        }

        # Add AUC annotations if not in clean plot mode
        if (!self$options$cleanPlot) {
          if (self$options$combinePlots && length(unique(plotData$var)) > 1) {
            # Multiple AUC annotations for combined plot
            auc_data <- aggregate(AUC ~ var, data = plotData, FUN = function(x) x[1])
            auc_data$AUC_formatted <- sprintf("AUC = %.3f", auc_data$AUC)
            auc_data$x <- 0.75 # Position for annotations
            auc_data$y <- seq(0.3, 0.1, length.out = nrow(auc_data))

            plot <- plot +
              ggplot2::geom_text(
                data = auc_data,
                ggplot2::aes(x = x, y = y, label = AUC_formatted, color = var),
                hjust = 0, show.legend = FALSE
              )
          } else {
            # Single AUC annotation for individual plot
            if (nrow(plotData) > 0) {
              auc_value <- unique(plotData$AUC)[1]
              plot <- plot + ggplot2::annotate(
                "text",
                x = 0.75,
                y = 0.25,
                label = sprintf("AUC = %.3f", auc_value)
              )
            }
          }
        }

        # Add direct labels if requested
        if (self$options$directLabel && self$options$combinePlots) {
          # Get unique variables and their optimal points
          unique_vars <- unique(plotData$var)
          label_points <- data.frame()

          for (var_name in unique_vars) {
            var_data <- plotData[plotData$var == var_name, ]
            j_max_idx <- unique(var_data$j_max_idx)
            if (length(j_max_idx) == 1L && !is.na(j_max_idx)) {
              label_points <- rbind(label_points, var_data[j_max_idx, ])
            }
          }

          # Add text labels directly to curves
          if (nrow(label_points) > 0) plot <- plot +
            ggplot2::geom_text(
              data = label_points,
              ggplot2::aes(
                x = 1 - specificity,
                y = sensitivity,
                label = var
              ),
              hjust = -0.1,
              vjust = 1.1
            )
        }

        # Add confidence bands if requested
        if (self$options$showConfidenceBands && !self$options$cleanPlot) {
          # Bands are now drawn for EVERY curve on the panel, not only for a
          # single-marker plot. Previously the whole block was skipped whenever
          # combinePlots was on with 2+ markers -- which is the DEFAULT -- so
          # ticking "Confidence bands" changed nothing at all for most users.
          # One ribbon per marker, filled to match that marker's curve colour.
          bands_data <- do.call(rbind, lapply(unique(plotData$var), function(var_name) {
            if (is.null(overlay$counts)) return(NULL)
            cnt <- overlay$counts[overlay$counts$var == var_name, , drop = FALSE]
            vd <- plotData[plotData$var == var_name, , drop = FALSE]
            if (nrow(cnt) == 0 || nrow(vd) == 0) return(NULL)
            n_pos <- cnt$n_pos[1]
            n_neg <- cnt$n_neg[1]
            if (n_pos < 1 || n_neg < 1) return(NULL)
            # Pointwise Wilson score interval on sensitivity (see .wilsonCI). The Wald
            # band this replaces had zero width at sensitivity 0 and 1, i.e. at both ends
            # of every curve, which read as certainty where there is none.
            sens_ci <- private$.wilsonCI(vd$sensitivity, n_pos)
            data.frame(
              x = 1 - vd$specificity,
              ymin = sens_ci$lower,
              ymax = sens_ci$upper,
              var = var_name,
              stringsAsFactors = FALSE
            )
          }))

          if (!is.null(bands_data) && nrow(bands_data) > 0) {
            plot <- plot +
              ggplot2::geom_ribbon(
                data = bands_data,
                ggplot2::aes(x = x, ymin = ymin, ymax = ymax, fill = var),
                alpha = 0.15, inherit.aes = FALSE, show.legend = FALSE
              )
            # Match the curve palette so a band is unambiguously "its" marker's.
            plot <- plot + ggplot2::labs(
              # The method named here must be the method computed above. This said
              # "normal-approximation" after the band was switched to Wilson -- a caption
              # that names the wrong interval is a mislabelled statistic, not a typo.
              caption = paste(strwrap(.("Shaded bands are pointwise 95% Wilson score intervals for sensitivity; they are not a simultaneous confidence region for the whole curve."),
                width = max(25L, floor(image$width / 9))), collapse = "\n")
            ) + ggplot2::theme(plot.caption.position = "plot",
              plot.caption = ggplot2::element_text(hjust = 0))
          }
        }

        # Handle quantile CIs if requested
        if (self$options$quantileCIs && !self$options$cleanPlot) {
          if (!is.null(overlay$quantiles) && nrow(overlay$quantiles) > 0L) {
            # For each variable in the plot
            for (var_name in unique(plotData$var)) {
              var_data <- plotData[plotData$var == var_name, ]

              # Predictor quantiles and class counts for this marker, from the image state
              qs <- if (is.null(overlay$quantiles)) NULL else
                overlay$quantiles[overlay$quantiles$var == var_name, , drop = FALSE]
              cnt <- if (is.null(overlay$counts)) NULL else
                overlay$counts[overlay$counts$var == var_name, , drop = FALSE]
              if (!is.null(qs) && nrow(qs) > 0 && !is.null(cnt) && nrow(cnt) > 0) {
                n_pos <- cnt$n_pos[1]
                n_neg <- cnt$n_neg[1]
                pred_quantiles <- qs$value
                quantiles_vec <- qs$prob

                # Create a data frame for quantile points
                quantile_points <- data.frame()

                # For each quantile, use its classification rule.
                for (q_idx in seq_along(pred_quantiles)) {
                  q <- pred_quantiles[q_idx]
                  q_prob <- quantiles_vec[q_idx]

                  # Exact decision rule, independent of numerical distance to an observed value.
                  idx <- private$.thresholdIndex(var_data$cutpoint, q)

                  if (length(idx) > 0) {
                    # Get coordinates
                    x <- 1 - var_data$specificity[idx]
                    y <- var_data$sensitivity[idx]

                    # Wilson score intervals, not Wald: at a quantile that sits near either
                    # ROC endpoint the Wald bar collapsed to a point (see .wilsonCI).
                    sens_ci <- private$.wilsonCI(y, n_pos)
                    sens_ci_lower <- sens_ci$lower
                    sens_ci_upper <- sens_ci$upper

                    spec <- var_data$specificity[idx]
                    spec_ci <- private$.wilsonCI(spec, n_neg)
                    spec_ci_lower <- spec_ci$lower
                    spec_ci_upper <- spec_ci$upper

                    # Add to data frame
                    quantile_points <- rbind(quantile_points, data.frame(
                      x = x,
                      y = y,
                      ymin = sens_ci_lower,
                      ymax = sens_ci_upper,
                      xmin = 1 - spec_ci_upper,
                      xmax = 1 - spec_ci_lower,
                      var = var_name,
                      q_label = sprintf("q=%.2f", q_prob)
                    ))
                  }
                }

                # Add quantile points and CIs to plot
                if (nrow(quantile_points) > 0) {
                  if (self$options$combinePlots && length(unique(plotData$var)) > 1) {
                    # For combined plot with multiple variables
                    plot <- plot +
                      # Horizontal error bars
                      ggplot2::geom_errorbarh(
                        data = quantile_points,
                        ggplot2::aes(x = x, y = y, xmin = xmin, xmax = xmax, color = var),
                        height = 0.02
                      ) +
                      # Vertical error bars
                      ggplot2::geom_errorbar(
                        data = quantile_points,
                        ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax, color = var),
                        width = 0.02
                      ) +
                      # Points
                      ggplot2::geom_point(
                        data = quantile_points,
                        ggplot2::aes(x = x, y = y, color = var),
                        size = 3, shape = 4
                      ) +
                      # Labels
                      ggplot2::geom_text(
                        data = quantile_points,
                        ggplot2::aes(x = x, y = y, label = q_label, color = var),
                        hjust = -0.2, vjust = -0.5, size = 3
                      )
                  } else {
                    # For single variable plot
                    plot <- plot +
                      # Horizontal error bars
                      ggplot2::geom_errorbarh(
                        data = quantile_points,
                        ggplot2::aes(x = x, y = y, xmin = xmin, xmax = xmax),
                        height = 0.02, color = "blue"
                      ) +
                      # Vertical error bars
                      ggplot2::geom_errorbar(
                        data = quantile_points,
                        ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax),
                        width = 0.02, color = "blue"
                      ) +
                      # Points
                      ggplot2::geom_point(
                        data = quantile_points,
                        ggplot2::aes(x = x, y = y),
                        size = 3, shape = 4, color = "blue"
                      ) +
                      # Labels
                      ggplot2::geom_text(
                        data = quantile_points,
                        ggplot2::aes(x = x, y = y, label = q_label),
                        hjust = -0.2, vjust = -0.5, size = 3, color = "blue"
                      )
                  }
                }
              }
            }
          }
        }

        print(plot)
        return(TRUE)
      },

      # Plot sensitivity/specificity vs criterion
      # @param image The image object
      # @param ggtheme The ggplot theme to use
      # @param theme Additional theme elements
      # @param ... Additional parameters
      .plotCriterion = function(image, ggtheme, theme, ...) {
        plotData <- image$state

        if (is.null(plotData) || (is.data.frame(plotData) && nrow(plotData) == 0)) {
          return(FALSE)
        }

        # Check if this is a combined plot with multiple variables
        if ("var" %in% names(plotData)) {
          # Multiple variables
          plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = threshold, group = var, color = var)) +
            ggplot2::geom_line(ggplot2::aes(y = sensitivity, linetype = "Sensitivity")) +
            ggplot2::geom_line(ggplot2::aes(y = specificity, linetype = "Specificity")) +
            ggplot2::scale_linetype_manual(name = "Metric", values = c("Sensitivity" = "solid", "Specificity" = "dashed")) +
            ggplot2::labs(
              x = .("Threshold"),
              y = .("Value"),
              color = .("Variable"),
              title = .("Sensitivity and Specificity vs. Threshold")
            )
        } else {
          # Single variable - reshape data for better plotting (base R, no tidyr dependency)
          sens_df <- data.frame(
            threshold = plotData$threshold,
            value = plotData$sensitivity,
            metric = "Sensitivity",
            stringsAsFactors = FALSE
          )
          spec_df <- data.frame(
            threshold = plotData$threshold,
            value = plotData$specificity,
            metric = "Specificity",
            stringsAsFactors = FALSE
          )
          plot_data_long <- rbind(sens_df, spec_df)
          plot_data_long$metric <- factor(
            plot_data_long$metric,
            levels = c("Sensitivity", "Specificity")
          )

          # Create plot
          plot <- ggplot2::ggplot(
            plot_data_long,
            ggplot2::aes(x = threshold, y = value, color = metric)
          ) +
            ggplot2::geom_line() +
            ggplot2::labs(
              x = .("Threshold"),
              y = .("Value"),
              color = .("Metric"),
              title = .("Sensitivity and Specificity vs. Threshold")
            )

          # The selected cutoff is part of the saved state; do not reselect by Youden.
          opt_threshold <- unique(plotData$selected_cutpoint)
          if (length(opt_threshold) == 1L && is.finite(opt_threshold)) {

            # Add vertical line at optimal threshold
            plot <- plot + ggplot2::geom_vline(
              xintercept = opt_threshold,
              linetype = "dotted",
              color = "darkgray"
            ) +
              ggplot2::annotate(
                "text",
                x = opt_threshold,
                y = 0.1,
                label = sprintf("Selected: %.3f", opt_threshold),
                hjust = -0.1
              )
          }
        }

        # Apply theme
        plot <- plot + ggtheme

        print(plot)
        return(TRUE)
      },

      # Plot PPV/NPV vs prevalence
      # @param image The image object
      # @param ggtheme The ggplot theme to use
      # @param theme Additional theme elements
      # @param ... Additional parameters
      .plotPrevalence = function(image, ggtheme, theme, ...) {
        state <- image$state

        if (is.null(state)) {
          return(FALSE)
        }

        # Extract data
        optimal <- state$optimal
        prevalence <- state$prevalence

        # Override with prior prevalence if requested
        if (self$options$usePriorPrev) {
          prevalence <- self$options$priorPrev
        }

        if (is.null(optimal) || is.null(prevalence)) {
          return(FALSE)
        }

        # Create prevalence sequence
        prev_seq <- seq(0.01, 0.99, by = 0.01)

        # Calculate PPV and NPV for different prevalence values
        # PPV = (sens * prev) / (sens * prev + (1-spec) * (1-prev))
        ppv_vals <- (optimal$sensitivity * prev_seq) /
          ((optimal$sensitivity * prev_seq) + ((1 - optimal$specificity) * (1 - prev_seq)))

        # NPV = (spec * (1-prev)) / (spec * (1-prev) + (1-sens) * prev)
        npv_vals <- (optimal$specificity * (1 - prev_seq)) /
          ((optimal$specificity * (1 - prev_seq)) + ((1 - optimal$sensitivity) * prev_seq))

        # Create data frame for plotting
        plot_data <- data.frame(
          prevalence = c(prev_seq, prev_seq),
          value = c(ppv_vals, npv_vals),
          metric = factor(
            rep(c("PPV", "NPV"), each = length(prev_seq)),
            levels = c("PPV", "NPV"),
            labels = c("Positive Predictive Value", "Negative Predictive Value")
          )
        )

        # Create plot
        plot <- ggplot2::ggplot(
          plot_data,
          ggplot2::aes(x = prevalence, y = value, color = metric)
        ) +
          ggplot2::geom_line() +
          ggplot2::geom_vline(
            xintercept = prevalence,
            linetype = "dashed",
            color = "darkgray"
          ) +
          ggplot2::labs(
            title = .("Predictive Values vs. Disease Prevalence"),
            # %n$ positional markers: three conversions in one msgid, so a translator has
            # to be able to reorder them.
            subtitle = sprintf(
              .("At optimal threshold = %1$s (sensitivity %2$s%%, specificity %3$s%%)"),
              format(round(optimal$threshold, 3)),
              format(round(optimal$sensitivity * 100, 1)),
              format(round(optimal$specificity * 100, 1))
            ),
            x = .("Disease Prevalence"),
            y = .("Value"),
            color = .("Metric")
          ) +
          ggplot2::annotate(
            "text",
            x = prevalence,
            y = 0.1,
            label = sprintf(.("Sample prevalence: %.2f"), prevalence),
            hjust = -0.1
          ) +
          ggtheme

        print(plot)
        return(TRUE)
      },

      # Plot dot plot showing distribution of test values by class
      # @param image The image object
      # @param ggtheme The ggplot theme to use
      # @param theme Additional theme elements
      # @param ... Additional parameters
      .plotDot = function(image, ggtheme, theme, ...) {
        plotData <- image$state

        if (is.null(plotData) || nrow(plotData) == 0) {
          return(FALSE)
        }

        # x shows the level names, negative class first (older saved states carry only the role).
        lab <- if (is.null(plotData$label)) plotData$class else plotData$label
        plotData$label <- factor(lab, levels = unique(lab[order(plotData$class)]))

        # Create plot
        plot <- ggplot2::ggplot(
          plotData,
          ggplot2::aes(x = label, y = value, color = class)
        ) +
          # Add jittered points
          ggplot2::geom_jitter(
            width = 0.3,
            height = 0,
            alpha = 0.7,
            size = 3
          ) +
          # Add horizontal line at threshold
          ggplot2::geom_hline(
            yintercept = plotData$threshold[1],
            linetype = "dashed",
            color = "darkgray"
          ) +
          # Set color palette
          ggplot2::scale_color_manual(
            values = c("Negative" = "darkblue", "Positive" = "darkred")
          ) +
          # Add labels
          ggplot2::labs(
            title = .("Distribution of Values by Class"),
            x = .("Class"),
            y = .("Value")
          ) +
          # Add annotation with threshold info
          ggplot2::annotate(
            "text",
            x = 1.5,
            y = plotData$threshold[1],
            label = paste0(
              "Threshold: ", round(plotData$threshold[1], 3),
              "\nDirection: ", plotData$direction[1]
            ),
            hjust = 0
          ) +
          # Remove legend
          ggplot2::theme(legend.position = "none") +
          ggtheme

        # Add boxplots if helpful for distribution visualization
        plot <- plot +
          ggplot2::geom_boxplot(
            alpha = 0.3,
            width = 0.5,
            outlier.shape = NA # Hide outliers since we're showing all points
          )

        print(plot)
        return(TRUE)
      },

      # Plot precision-recall curves
      .plotPrecisionRecall = function(image, ggtheme, theme, ...) {
        plotData <- image$state

        if (is.null(plotData) || nrow(plotData) == 0) {
          return(FALSE)
        }

        # Determine if this is a combined or individual plot
        if ("var" %in% names(plotData)) {
          # Combined plot with multiple variables
          plot <- ggplot2::ggplot(
            plotData,
            ggplot2::aes(
              x = recall,
              y = precision,
              color = var,
              linetype = var
            )
          ) +
            ggplot2::geom_line(linewidth = 1) +
            ggplot2::scale_color_brewer(palette = "Set1") +
            ggplot2::scale_linetype_manual(
              values = rep(c("solid", "dashed", "dotted", "longdash"),
                length.out = length(unique(plotData$var))
              )
            )
        } else {
          # Individual plot
          plot <- ggplot2::ggplot(
            plotData,
            ggplot2::aes(
              x = recall,
              y = precision
            )
          ) +
            ggplot2::geom_line(linewidth = 1) +
            ggplot2::geom_point(size = 0.5, alpha = 0.7)
        }

        # Add AUPRC annotations
        if ("auprc" %in% names(plotData)) {
          if ("var" %in% names(plotData)) {
            # For combined plot
            auprc_data <- aggregate(auprc ~ var, data = plotData, FUN = function(x) x[1])
            auprc_data$label <- sprintf("AUPRC = %.3f", auprc_data$auprc)
            auprc_data$x <- 0.25
            auprc_data$y <- seq(0.3, 0.1, length.out = nrow(auprc_data))

            plot <- plot +
              ggplot2::geom_text(
                data = auprc_data,
                ggplot2::aes(x = x, y = y, label = label, color = var),
                hjust = 0,
                show.legend = FALSE
              )
          } else {
            # For individual plot
            plot <- plot +
              ggplot2::annotate(
                "text",
                x = 0.25,
                y = 0.25,
                label = sprintf(.("AUPRC = %.3f"), unique(plotData$auprc)[1])
              )
          }
        }

        # Add labels and theme
        plot <- plot +
          ggplot2::xlab(.("Recall (Sensitivity)")) +
          ggplot2::ylab(.("Precision (Positive Predictive Value)")) +
          ggplot2::xlim(0, 1) +
          ggplot2::ylim(0, 1) +
          ggtheme

        print(plot)
        return(TRUE)
      },

      # Fixed Sensitivity/Specificity ROC plotting function
      .plotFixedSensSpecROC = function(image, ggtheme, theme, ...) {
        plotData <- image$state

        if (!self$options$fixedSensSpecAnalysis || is.null(plotData)) {
          return(FALSE)
        }

        # Get variable name from plot data (stored in state)
        var <- plotData$var[1]
        if (is.null(var) || is.na(var)) {
          return(FALSE)
        }

        # Get fixed analysis parameters
        analysis_type <- self$options$fixedAnalysisType
        target_value <- if (analysis_type == "sensitivity") {
          self$options$fixedSensitivityValue
        } else {
          self$options$fixedSpecificityValue
        }

        # Get the fixed point from the results table
        fixedTable <- self$results$fixedSensSpecTable
        fixed_row <- NULL

        if (length(fixedTable$rowKeys) > 0) {
          # rowKey is the variable name (see .populateFixedSensSpecTable line 791)
          # So we can compare directly without extracting from cells
          if (var %in% fixedTable$rowKeys) {
            # $value: getCell() returns a jmvcore Cell R6 object, not the number.
            # Without it round()/arithmetic below raised "non-numeric argument to
            # mathematical function" every time this plot was drawn.
            fixed_row <- list(
              cutpoint = fixedTable$getCell(rowKey = var, "cutpoint")$value,
              achieved_sensitivity = fixedTable$getCell(rowKey = var, "achieved_sensitivity")$value,
              achieved_specificity = fixedTable$getCell(rowKey = var, "achieved_specificity")$value
            )
          }
        }

        if (is.null(fixed_row)) {
          return(FALSE)
        }

        # Create ROC plot with highlighted fixed point
        plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
          ggplot2::geom_line(color = "steelblue", linewidth = 1) +
          ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
          ggplot2::xlim(0, 1) +
          ggplot2::ylim(0, 1) +
          ggplot2::labs(
            x = .("1 - Specificity (False Positive Rate)"),
            y = .("Sensitivity (True Positive Rate)"),
            # A msgid must be a LITERAL: paste()/toTitleCase() built the title at run time,
            # so nothing was extractable. One literal per branch instead.
            title = if (analysis_type == "sensitivity")
              .("ROC Curve with Fixed Sensitivity Point") else
              .("ROC Curve with Fixed Specificity Point"),
            subtitle = sprintf(
              if (analysis_type == "sensitivity")
                .("Target sensitivity %1$s | achieved %2$s | cutpoint %3$s") else
                .("Target specificity %1$s | achieved %2$s | cutpoint %3$s"),
              format(round(target_value, 3)),
              format(round(if (analysis_type == "sensitivity") fixed_row$achieved_sensitivity else fixed_row$achieved_specificity, 3)),
              format(round(fixed_row$cutpoint, 3))
            )
          ) +
          ggtheme

        # Add the fixed point
        plot <- plot + ggplot2::geom_point(
          x = 1 - fixed_row$achieved_specificity,
          y = fixed_row$achieved_sensitivity,
          color = "red",
          size = 4,
          shape = 16
        )

        # Add annotation for the fixed point
        plot <- plot + ggplot2::annotate(
          "text",
          x = 1 - fixed_row$achieved_specificity + 0.1,
          y = fixed_row$achieved_sensitivity + 0.05,
          label = paste0(
            "Fixed Point\n",
            "Sens: ", round(fixed_row$achieved_sensitivity, 3), "\n",
            "Spec: ", round(fixed_row$achieved_specificity, 3)
          ),
          hjust = 0,
          size = 3,
          color = "red"
        )

        print(plot)
        return(TRUE)
      },

      # Plot interactive ROC (static fallback - jamovi does not support interactive HTML plots)
      .plotInteractiveROC = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }
        plotData <- as.data.frame(image$state)
        if (nrow(plotData) == 0) {
          return(FALSE)
        }

        # One path per marker. Ungrouped, geom_line sorted every marker's points together by x
        # and drew them as a single zigzag line (the state holds all markers in both modes).
        # Within a marker the rows are cutpointr's cumulative order, so TPR never drops.
        n_var <- length(unique(plotData$var))
        line <- if (n_var > 1) ggplot2::geom_line(ggplot2::aes(colour = var), linewidth = 1)
                else ggplot2::geom_line(linewidth = 1, color = "steelblue")
        plot <- ggplot2::ggplot(
          plotData,
          ggplot2::aes(x = 1 - specificity, y = sensitivity, group = var)
        ) +
          ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
          line +
          ggplot2::xlab(.("1 - Specificity")) +
          ggplot2::ylab(.("Sensitivity")) +
          ggplot2::xlim(0, 1) +
          ggplot2::ylim(0, 1) +
          ggplot2::ggtitle(.("ROC Curve (static rendering)")) +
          ggtheme
        if (n_var > 1)   # jamovi's global palette, as in .plotROC
          plot <- plot + ggplot2::scale_color_manual(name = NULL, values = jmvcore::colorPalette(
            n_var, if (is.null(theme$palette)) "jmv" else theme$palette, "color"))

        print(plot)
        return(TRUE)
      },

      # ============================================================================
      # ENHANCED FUNCTIONS FROM LEGACY TESTROC IMPLEMENTATION
      # ============================================================================

      # Enhanced HTML table formatting for sensitivity/specificity results
      .formatSensSpecTable = function(Title, TP, FP, TN, FN, labels) {
        # Escape Title once at function entry - callers (e.g. L1385-1390) pass
        # paste("Confusion Matrix for", var, ...) where `var` is a user column
        # name from OptionVariables. Without this escape, a column literally
        # named `<img src=x onerror=alert(1)>` would render as live HTML in the
        # `<th>` cell at L4240 since sensSpecTable is type:Html in .r.yaml.
        Title <- jmvcore::htmlEscape(Title)
        # The header cells were raw English buried in the HTML literal, so a translated
        # analysis showed translated tables next to an English confusion matrix. Broken out
        # so each is an extractable msgid.
        lblDecision <- .("DECISION BASED ON MEASURE")
        lblCriterion <- .("CRITERION")
        # "Positive (<level>)" / "Negative (<level>)": level names are user data, and this is Html.
        lblNegative <- jmvcore::htmlEscape(labels[["negative"]])
        lblPositive <- jmvcore::htmlEscape(labels[["positive"]])
        res <- paste0(
          "<style type='text/css'>
        .tg  {border-collapse:collapse;border-spacing:0;border-width:1px;border-style:solid;border-color:black;}
        .tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
        .tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
        .tg .tg-s6z2{text-align:center}
        .tg .tg-uys7{border-color:inherit;text-align:center}
        .tg .tg-h0x1{text-align:center}
        </style>
        <table class='tg'>
          <tr>
            <th class='tg-0lax' colspan='4'>",
          Title,
          "</th>
          </tr>
          <tr>
            <td class='tg-s6z2'></td>
            <td class='tg-uys7' colspan='3'>",
          lblDecision,
          "</td>
          </tr>
          <tr>
            <td class='tg-h0x1' rowspan='3'>",
          lblCriterion,
          "</td>
            <td class='tg-h0x1'></td>
            <td class='tg-h0x1'>",
          lblNegative,
          "</td>
            <td class='tg-h0x1'>",
          lblPositive,
          "</td>
          </tr>
          <tr>
            <td class='tg-s6z2'>",
          lblNegative,
          "</td>
            <td class='tg-s6z2'>",
          TN,
          " (TN)</td>
            <td class='tg-s6z2'>",
          FP,
          " (FP)</td>
          </tr>
          <tr>
            <td class='tg-h0x1'>",
          lblPositive,
          "</td>
            <td class='tg-h0x1'>",
          FN,
          " (FN)</td>
            <td class='tg-h0x1'>",
          TP,
          " (TP)</td>
          </tr>
          <tr>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
          </tr>
        </table>"
        )
        return(res)
      },

      # Enhanced DeLong test implementation with better error handling
      # Enhanced DeLong test implementation using pROC for robust covariance calculation
      .enhancedDelongTest = function(data, classVar, pos_class, ref = NULL, conf.level = 0.95) {
        # Check pROC availability
        if (!requireNamespace("pROC", quietly = TRUE)) {
          jmvcore::reject("pROC package is required for DeLong's test", code = "delong_validation")
        }

        # Validate inputs
        if (length(classVar) != nrow(data)) {
          jmvcore::reject("The number of rows in data must match the length of classVar", code = "delong_validation")
        }

        # Check for NA values in classification variable
        if (any(is.na(classVar))) {
          jmvcore::reject("Classification variable contains missing values (NA). Please remove or handle missing values before performing ROC analysis.", code = "delong_validation")
        }

        id.pos <- classVar == pos_class
        if (sum(id.pos, na.rm = TRUE) < 1) {
          jmvcore::reject("Wrong positive class level specified.", code = "delong_validation")
        }
        if (ncol(data) < 2) {
          jmvcore::reject("Data must contain at least two columns.", code = "delong_validation")
        }

        nauc <- ncol(data)
        vars <- colnames(data)

        # Calculate ROC curves using pROC
        rocs <- list()
        auc_values <- numeric(nauc)

        # Use user-specified direction for consistency with main analysis
        pROC_direction <- ifelse(self$options$direction == ">=", "<", ">")
        for (i in 1:nauc) {
          rocs[[i]] <- pROC::roc(
            response = private$.binaryResponse(classVar, pos_class), predictor = data[[i]],
            direction = pROC_direction, quiet = TRUE
          )
          auc_values[i] <- as.numeric(pROC::auc(rocs[[i]]))
        }

        # Compute Covariance Matrix S using pROC
        # S[i,j] = cov(roc[i], roc[j])
        S <- matrix(0, nrow = nauc, ncol = nauc)
        for (i in 1:nauc) {
          for (j in i:nauc) {
            if (i == j) {
              # Variance of a single ROC curve
              S[i, j] <- pROC::var(rocs[[i]], method = "delong")
            } else {
              # Covariance between two ROC curves
              v <- pROC::cov(rocs[[i]], rocs[[j]], method = "delong")
              S[i, j] <- v
              S[j, i] <- v
            }
          }
        }

        # Construct Contrast Matrix L for Global Test
        if (is.null(ref)) {
          # Matrix for all pairwise differences (sequential construction matching original)
          L <- matrix(0, nrow = nauc * (nauc - 1) / 2, ncol = nauc)
          newa <- 0
          for (i in 1:(nauc - 1)) {
            newl <- nauc - i
            L[(newa + 1):(newa + newl), i] <- rep(1, newl)
            L[(newa + 1):(newa + newl), ((i + 1):(i + newl))] <-
              diag(-1, nrow = newl, ncol = newl)
            newa <- newa + newl
          }
        } else {
          # Compare against reference
          if (ref > nauc) jmvcore::reject(paste("Reference ref must be one of the markers (1...", nauc, ")", sep = ""), code = "delong_validation")
          L <- matrix(1, ncol = nauc, nrow = nauc - 1)
          L[, -ref] <- diag(-1, nrow = nauc - 1, ncol = nauc - 1)
        }

        # Calculate Global Test Statistics
        aucdiff <- L %*% auc_values

        # Initialize before tryCatch (error handler creates local scope)
        z_score <- NA
        global_p <- NA

        # Singular contrasts are valid only in their estimable covariance subspace.
        tryCatch(
          {
            inference <- private$.delongGlobalInference(aucdiff, L %*% S %*% t(L))
            z_score <- inference$statistic
            global_p <- inference$p
          },
          error = function(e) {
            private$.warnUser(sprintf(
              .("The global test across all markers could not be computed (%s); its statistic and p-value are left blank."),
              conditionMessage(e)))
            # z_score and global_p remain NA from initialization above
          }
        )

        # Pairwise Comparisons
        quantil <- qnorm(1 - (1 - conf.level) / 2)

        if (is.null(ref)) {
          # All pairwise
          n_pairs <- nauc * (nauc - 1) / 2
          rows <- character(n_pairs)
          pair_labels <- character(length(rows))
          diff_vec <- numeric(n_pairs)
          ci <- matrix(NA, nrow = n_pairs, ncol = 2)
          p_vals <- numeric(n_pairs)
          cors <- numeric(n_pairs)

          z_vals <- numeric(n_pairs)

          ctr <- 1
          for (i in 1:(nauc - 1)) {
            for (j in (i + 1):nauc) {
              # Difference
              diff_val <- auc_values[i] - auc_values[j]
              diff_vec[ctr] <- diff_val

              inference <- private$.delongPairInference(
                diff_val, S[c(i, j), c(i, j), drop = FALSE], quantil)
              ci[ctr, ] <- inference$ci
              p_vals[ctr] <- inference$p
              z_vals[ctr] <- inference$z

              # Correlation
              denom_cor_ij <- S[i, i] * S[j, j]
              cors[ctr] <- if (denom_cor_ij > 0) S[i, j] / sqrt(denom_cor_ij) else NA

              # Label by variable NAME. "1 vs. 2" gave the clinician no way to
              # tell which two markers were being compared.
              rows[ctr] <- .psychopdaPairKey(i, j)
              pair_labels[ctr] <- .psychopdaPairLabel(vars[i], vars[j])
              ctr <- ctr + 1
            }
          }
        } else {
          # Against reference
          n_pairs <- nauc - 1
          rows <- character(n_pairs)
          pair_labels <- character(length(rows))
          diff_vec <- numeric(n_pairs)
          ci <- matrix(NA, nrow = n_pairs, ncol = 2)
          p_vals <- numeric(n_pairs)
          cors <- numeric(n_pairs)

          z_vals <- numeric(n_pairs)

          comp <- (1:nauc)[-ref]
          for (i in 1:n_pairs) {
            idx <- comp[i]

            diff_val <- auc_values[ref] - auc_values[idx]
            diff_vec[i] <- diff_val

            inference <- private$.delongPairInference(
              diff_val, S[c(ref, idx), c(ref, idx), drop = FALSE], quantil)
            ci[i, ] <- inference$ci
            p_vals[i] <- inference$p
            z_vals[i] <- inference$z

            denom_cor_ri <- S[ref, ref] * S[idx, idx]
            cors[i] <- if (denom_cor_ri > 0) S[ref, idx] / sqrt(denom_cor_ri) else NA

            rows[i] <- .psychopdaPairKey(ref, idx)
            pair_labels[i] <- .psychopdaPairLabel(vars[ref], vars[idx])
          }
        }

        # Enhanced results formatting
        newres <- data.frame(
          "AUC Difference" = diff_vec,
          "CI(lower)" = ci[, 1],
          "CI(upper)" = ci[, 2],
          "P.Value" = p_vals,
          "Correlation" = cors,
          "Z" = z_vals,
          check.names = FALSE
        )
        rownames(newres) <- rows
        colnames(newres) <- c("AUC Difference", "CI(lower)", "CI(upper)", "P.Value", "Correlation", "Z")

        # Format AUC dataframe
        # We use pROC's AUC and Variance, but calculate Hanley SD for backward compatibility
        nn <- sum(classVar != pos_class)
        np <- sum(classVar == pos_class)

        q1 <- auc_values / (2 - auc_values)
        q2 <- 2 * auc_values^2 / (1 + auc_values)
        aucvar_hanley <- (auc_values * (1 - auc_values) + (np - 1) * (q1 - auc_values^2) + (nn - 1) * (q2 - auc_values^2)) / (np * nn)

        sd_hanley <- sqrt(pmax(0, aucvar_hanley))
        sd_delong <- sqrt(pmax(0, diag(S)))

        auc_df <- data.frame(
          "AUC" = auc_values,
          "SD(Hanley)" = sd_hanley,
          "P(H0: AUC=0.5)" = ifelse(sd_hanley > 0, 2 * pnorm(-abs(auc_values - 0.5) / sd_hanley), NA),
          "SD(DeLong)" = sd_delong,
          "P(H0: AUC=0.5)" = ifelse(sd_delong > 0, 2 * pnorm(-abs(auc_values - 0.5) / sd_delong), NA),
          check.names = FALSE
        )
        names(auc_df) <- c("AUC", "SD(Hanley)", "P(H0: AUC=0.5)", "SD(DeLong)", "P(H0: AUC=0.5)")
        rownames(auc_df) <- vars   # was 1..n, so the printed output named no marker

        ERG <- list(
          AUC = auc_df,
          difference = newres,
          comparison_labels = pair_labels,
          covariance = S,
          global.z = z_score,
          global.p = global_p
        )
        class(ERG) <- "EnhancedDeLong"
        return(ERG)
      },

      # Enhanced print method for DeLong results
      .printEnhancedDeLong = function(x, digits = max(3, getOption("digits") - 3), ...) {
        output <- character()

        # Format AUC table
        output <- c(output, "Estimated AUC's:")
        auc_formatted <- base::format(round(x$AUC[, c("AUC", "SD(DeLong)"), drop = FALSE],
          digits = digits, ...), nsmall = digits, ...)
        output <- c(output, capture.output(print(auc_formatted, quote = FALSE)))

        # Format pairwise comparisons
        output <- c(output, "", "Pairwise comparisons:")
        diff_formatted <- base::format(round(x$difference, digits = digits, ...), nsmall = digits, ...)
        diff_formatted <- data.frame(Comparison = x$comparison_labels,
          diff_formatted, check.names = FALSE, row.names = NULL)
        output <- c(output, capture.output(print(diff_formatted, quote = FALSE, row.names = FALSE)))

        # Format overall test
        if (!is.na(x$global.p)) {
          output <- c(output, "", paste("Overall test:", "p-value =", format.pval(x$global.p, digits = digits)))
        } else {
          output <- c(output, "", .("Overall test: unavailable (contrast covariance is degenerate or inference could not be computed)."))
        }

        return(paste(output, collapse = "\n"))
      },

      # ============================================================================
      # ADVANCED STATISTICAL ANALYSIS METHODS
      # ============================================================================

      # Calculate effect sizes for ROC comparisons
      .calculateEffectSizes = function(data, classVar, positiveClass) {
        vars <- self$options$dependentVars
        if (length(vars) < 2) {
          self$results$effectSizePlot$clear()
          private$.warnUser(.("Effect size comparisons require at least two test variables. Add another test variable or disable Effect Size Analysis."))
          return()
        }

        # Table visibility is controlled by YAML: visible: (effectSizeAnalysis)

        # Get binary outcomes
        y <- private$.classIndicator(data, positiveClass)

        # The three "difference" columns held d2 - d1 under the heading "Cohen's d", so a
        # reader took the between-marker difference for a single marker's effect size.
        self$results$effectSizeTable$setNote(
          key = "definition",
          note = .("Cohen's d is the standardized difference in a marker's mean between the positive and negative classes (pooled SD), given for each marker of the pair. The three difference columns are second marker minus first marker; Glass' delta uses the negative-class SD and Hedges' g applies the small-sample correction J to each d before differencing. No interval or test accompanies these point estimates. The 0.2/0.5/0.8 size bands are descriptive conventions only; they have not been validated as clinical thresholds for a difference between two marker effects."),
          init = FALSE
        )

        for (i in 1:(length(vars) - 1)) {
          for (j in (i + 1):length(vars)) {
            var1 <- vars[i]
            var2 <- vars[j]

            # Get predictor values
            x1 <- jmvcore::toNumeric(data[[var1]])
            x2 <- jmvcore::toNumeric(data[[var2]])

            # Remove missing values
            complete_cases <- complete.cases(x1, x2, y)
            x1 <- x1[complete_cases]
            x2 <- x2[complete_cases]
            y_complete <- y[complete_cases]

            # Calculate effect sizes (standardized mean difference between
            # positive and negative classes, per variable)
            tryCatch(
              {
                pos1 <- x1[y_complete == 1]
                neg1 <- x1[y_complete == 0]
                pos2 <- x2[y_complete == 1]
                neg2 <- x2[y_complete == 0]

                n_pos <- length(pos1)
                n_neg <- length(neg1)

                # Cohen's d for each variable: (mean_pos - mean_neg) / pooled_sd
                pooled_sd1 <- sqrt(((n_pos - 1) * var(pos1) +
                  (n_neg - 1) * var(neg1)) /
                  (n_pos + n_neg - 2))
                pooled_sd2 <- sqrt(((n_pos - 1) * var(pos2) +
                  (n_neg - 1) * var(neg2)) /
                  (n_pos + n_neg - 2))

                d1 <- if (pooled_sd1 > 0) (mean(pos1) - mean(neg1)) / pooled_sd1 else NA
                d2 <- if (pooled_sd2 > 0) (mean(pos2) - mean(neg2)) / pooled_sd2 else NA

                # Difference in discriminative ability between the two tests
                cohens_d <- if (!is.na(d1) && !is.na(d2)) d2 - d1 else NA

                # Glass's delta: difference using negative-class (control) SD
                sd_neg1 <- sd(neg1)
                sd_neg2 <- sd(neg2)
                glass_delta1 <- if (sd_neg1 > 0) (mean(pos1) - mean(neg1)) / sd_neg1 else NA
                glass_delta2 <- if (sd_neg2 > 0) (mean(pos2) - mean(neg2)) / sd_neg2 else NA
                glass_delta <- glass_delta2 - glass_delta1

                # Hedges' g: bias-corrected Cohen's d, J = 1 - 3/(4*df - 1). Both markers share
                # n_pos/n_neg after complete-case filtering, so J*(d2 - d1) == g2 - g1.
                df <- n_pos + n_neg - 2
                correction <- 1 - 3 / (4 * df - 1)
                hedges_g <- cohens_d * correction

                # Effect magnitude based on absolute difference in d
                abs_d <- abs(cohens_d)
                # Displayed cell: wrap the literals so it is not an English word in an
                # otherwise translated table.
                effect_magnitude <- if (abs_d < 0.2) .("Negligible") else if (abs_d < 0.5) .("Small") else if (abs_d < 0.8) .("Medium") else .("Large")

                # Describe the size of the observed difference only: no interval and no test
                # accompanies it, so "not meaningful" would be accepting the null from a point
                # estimate.
                clinical_importance <- if (abs_d < 0.2) {
                  .("Observed difference in Cohen's d under 0.20")
                } else if (abs_d < 0.5) {
                  .("Observed difference in Cohen's d of 0.20 to 0.50")
                } else if (abs_d < 0.8) {
                  .("Observed difference in Cohen's d of 0.50 to 0.80")
                } else {
                  .("Observed difference in Cohen's d of 0.80 or more")
                }

                self$results$effectSizeTable$addRow(rowKey = .psychopdaPairKey(i, j), values = list(
                  comparison = .psychopdaPairLabel(var1, var2),
                  d_first = d1,
                  d_second = d2,
                  cohens_d = cohens_d,
                  glass_delta = glass_delta,
                  hedges_g = hedges_g,
                  effect_magnitude = effect_magnitude,
                  clinical_importance = clinical_importance
                ))

                # Activate the single all-pairs plot. The renderer reads the whole
                # effectSizeTable, so the state only has to be non-NULL; it used to be
                # written to the item keyed by var1, which meant item 1 was overwritten by
                # each pair in turn and the last variable's item was never written at all.
                if (self$options$effectSizeAnalysis &&
                    "all" %in% self$results$effectSizePlot$itemKeys) {
                  image <- self$results$effectSizePlot$get(key = "all")
                  if (!is.null(image)) image$setState(list(ready = TRUE))
                }
              },
              error = function(e) {
                # Was silent: the comparison simply vanished from the table.
                private$.warnUser(.fmt(
                  .("Effect sizes for '{var1}' vs '{var2}' could not be computed ({msg}), so that comparison is omitted."),
                  var1 = var1, var2 = var2, msg = conditionMessage(e)))
              }
            )
          }
        }
      },

      # Calculate power analysis for ROC studies
      .calculatePowerAnalysis = function(data, classVar, positiveClass) {
        vars <- self$options$dependentVars
        self$results$powerCurvePlot$clear()

        # Table visibility is controlled by YAML: visible: (powerAnalysis)
        # Add method reference
        self$results$powerAnalysisTable$setNote(
          key = "method_reference",
          # Was raw English.
          note = .("Power calculations based on: Obuchowski NA, McClish DK (1997). Sample size determination for diagnostic accuracy studies involving binormal ROC curve indices. Statistics in Medicine 16(13):1529-1542; Hanley JA, McNeil BJ (1982). The meaning and use of the area under a ROC curve. Radiology 143(1):29-36."),
          init = FALSE
        )

        self$results$powerAnalysisTable$setNote("target_definition",
          .("Target |AUC - 0.5| is the absolute distance from chance used in this calculation. Post-hoc power uses the observed AUC; prospective and sample-size calculations use the specified target."))
        self$results$powerAnalysisTable$setNote("approximation",
          .("Power includes both tails of the two-sided normal approximation. Current N and the class ratio use complete marker/outcome pairs. With multiple markers, the correlation adjustment assumes equal AUC variances and is a planning approximation, not a fitted paired ROC power model."), init = FALSE)

        y <- private$.classIndicator(data, positiveClass)

        # Use actual options from .a.yaml
        target_power <- self$options$targetPower # Already a proportion (0-1)
        expected_auc_diff <- self$options$expectedAUCDifference # Use this as effect size
        significance_level <- self$options$significanceLevel
        correlation <- self$options$correlationROCs
        analysis_type <- self$options$powerAnalysisType # post_hoc, prospective, sample_size

        for (var in vars) {
          x <- jmvcore::toNumeric(data[[var]])
          complete_cases <- complete.cases(x, y)
          x_complete <- x[complete_cases]
          y_complete <- y[complete_cases]
          n_pos <- sum(y_complete)
          n_neg <- sum(1 - y_complete)
          n_total <- n_pos + n_neg

          tryCatch(
            {
              # Calculate observed AUC for effect size
              if (requireNamespace("pROC", quietly = TRUE)) {
                roc_obj <- pROC::roc(y_complete, x_complete, levels = c(0, 1), direction = ifelse(self$options$direction == ">=", "<", ">"), quiet = TRUE)
                observed_auc <- as.numeric(pROC::auc(roc_obj))

                # Standard error of AUC (Hanley-McNeil)
                se_auc <- sqrt((observed_auc * (1 - observed_auc) +
                  (sum(y_complete) - 1) * (observed_auc / (2 - observed_auc) - observed_auc^2) +
                  (sum(1 - y_complete) - 1) * (2 * observed_auc^2 / (1 + observed_auc) - observed_auc^2)) /
                  (sum(y_complete) * sum(1 - y_complete)))

                z_alpha <- qnorm(1 - significance_level / 2)
                z_beta <- qnorm(target_power)
                adjustment_factor <- 1 # post-hoc: single AUC against 0.5; other branches reassign

                # Switch logic based on analysis type
                if (analysis_type == "post_hoc") {
                  # Post-hoc power: Calculate power for observed effect
                  z_stat <- if (se_auc > 0) (observed_auc - 0.5) / se_auc else NA
                  observed_power <- if (!is.na(z_stat)) pnorm(abs(z_stat) - z_alpha) + pnorm(-abs(z_stat) - z_alpha) else NA

                  # Preserve the observed direction and variance; null/degenerate targets
                  # have no finite sample-size recommendation under this approximation.
                  target_auc <- observed_auc
                  required_n <- private$.requiredNforPower(
                    auc = target_auc, alpha = significance_level,
                    target_power = target_power, pos_frac = n_pos / n_total,
                    adjustment_factor = 1)

                  power_adequacy <- .("Not informative (post-hoc)")
                  # Observed power computed from the observed effect is a one-to-one transform of
                  # the p-value, so it can never say anything the p-value did not already say.
                  recommendation <- paste(
                    sprintf(.("Observed (post-hoc) power uses the AUC estimated from these same data and adds no independent evidence about discrimination. Use the confidence interval for the observed AUC (%1$.3f), or plan against a prespecified target AUC."), observed_auc),
                    if (is.na(required_n))
                      .("A required sample size is unavailable for this null or degenerate observed AUC.")
                    else sprintf(.("For reference, n=%1$d would be required for %2$.1f%% power against an AUC of %3$.3f."), required_n, 100 * target_power, observed_auc))
                } else if (analysis_type == "prospective") {
                  # Prospective power: Calculate power for expected effect with current N
                  expected_auc <- 0.5 + expected_auc_diff

                  # Adjust for correlation when comparing two tests.
                  #
                  # The `correlation != 0` guard was a bug: it sent r = 0 down the
                  # ELSE branch to adjustment_factor = 1, which is exactly what the
                  # default r = 0.5 already yields (sqrt(2 * 0.5) == 1). So both ends
                  # of the range collapsed to "no adjustment", and r = 0 -- which the
                  # option's own help text calls "independent samples" and which
                  # should give the LARGEST inflation, sqrt(2) = 1.414 -- silently
                  # returned the paired answer instead.
                  adjustment_factor <- if (length(vars) > 1) {
                    sqrt(max(0, 2 * (1 - correlation)))
                  } else {
                    1
                  }
                  # r = 1 (the option's own maximum) means the two AUCs are
                  # perfectly correlated, so their difference has zero variance:
                  # the adjustment collapses to 0, expected_se becomes 0, z becomes
                  # NA, and `if (NA)` threw inside a bare tryCatch(error=...) that
                  # swallowed it -- the table came back with 0 rows and no message.
                  # A note on the table itself rather than .warnUser(): it sits
                  # next to the affected numbers, which is where it is needed.
                  if (!is.finite(adjustment_factor) || adjustment_factor <= 0) {
                    tryCatch(self$results$powerAnalysisTable$setNote(
                      "degenerate_correlation",
                      .("A correlation of 1 gives zero variance for the paired contrast under this equal-variance planning approximation. Power and required sample size are unavailable, so those cells and the power curve are left blank. Specify a correlation below 1 that is justified for the intended study.")
                    ), error = function(e) NULL)
                    adjustment_factor <- NA_real_
                  }

                  expected_se <- sqrt((expected_auc * (1 - expected_auc) +
                    (n_pos - 1) * (expected_auc / (2 - expected_auc) - expected_auc^2) +
                    (n_neg - 1) * (2 * expected_auc^2 / (1 + expected_auc) - expected_auc^2)) /
                    (n_pos * n_neg)) * adjustment_factor

                  # is.finite(), not just > 0: adjustment_factor is NA_real_ for the
                  # degenerate correlation = 1 case above, so expected_se is NA and the bare
                  # `if (expected_se > 0)` raised "missing value where TRUE/FALSE needed".
                  # The enclosing per-variable tryCatch swallowed it, so NO row was added for
                  # ANY variable and the whole table rendered empty under a note that spoke
                  # about a blank cell.
                  z_stat_expected <- if (is.finite(expected_se) && expected_se > 0) (expected_auc - 0.5) / expected_se else NA
                  observed_power <- if (!is.na(z_stat_expected)) pnorm(abs(z_stat_expected) - z_alpha) + pnorm(-abs(z_stat_expected) - z_alpha) else NA

                  required_n <- private$.requiredNforPower(
                    auc = expected_auc, alpha = significance_level,
                    target_power = target_power, pos_frac = n_pos / n_total,
                    adjustment_factor = adjustment_factor)

                  # power_adequacy is recomputed NA-safely below for every branch; the
                  # assignment that used to be here was dead AND crashed on a NA power.
                  recommendation <- if (is.na(observed_power)) {
                    .("Power and required sample size cannot be computed for this configuration.")
                  } else if (observed_power < target_power) {
                    sprintf(.("Need n=%1$d for %2$.1f%% power at an AUC distance from 0.5 of %3$s."), required_n, 100 * target_power, expected_auc_diff)
                  } else {
                    sprintf(.("Current n=%1$d provides %2$.1f%% power at an AUC distance from 0.5 of %3$s."), n_total, 100 * observed_power, expected_auc_diff)
                  }
                } else { # sample_size
                  # Sample size calculation: N needed for target power and expected effect
                  expected_auc <- 0.5 + expected_auc_diff

                  # Correlation adjustment
                  # Same correction as the prospective branch above: r = 0 must
                  # give the largest inflation, not fall through to no adjustment.
                  adjustment_factor <- if (length(vars) > 1) {
                    sqrt(max(0, 2 * (1 - correlation)))
                  } else {
                    1
                  }
                  # r = 1 (the option's own maximum) means the two AUCs are
                  # perfectly correlated, so their difference has zero variance:
                  # the adjustment collapses to 0, expected_se becomes 0, z becomes
                  # NA, and `if (NA)` threw inside a bare tryCatch(error=...) that
                  # swallowed it -- the table came back with 0 rows and no message.
                  # A note on the table itself rather than .warnUser(): it sits
                  # next to the affected numbers, which is where it is needed.
                  if (!is.finite(adjustment_factor) || adjustment_factor <= 0) {
                    tryCatch(self$results$powerAnalysisTable$setNote(
                      "degenerate_correlation",
                      .("A correlation of 1 gives zero variance for the paired contrast under this equal-variance planning approximation. Power and required sample size are unavailable, so those cells and the power curve are left blank. Specify a correlation below 1 that is justified for the intended study.")
                    ), error = function(e) NULL)
                    adjustment_factor <- NA_real_
                  }

                  required_n <- private$.requiredNforPower(
                    auc = expected_auc, alpha = significance_level,
                    target_power = target_power, pos_frac = n_pos / n_total,
                    adjustment_factor = adjustment_factor)

                  # Calculate what power we would have with current N
                  current_se <- sqrt((expected_auc * (1 - expected_auc) +
                    (n_pos - 1) * (expected_auc / (2 - expected_auc) - expected_auc^2) +
                    (n_neg - 1) * (2 * expected_auc^2 / (1 + expected_auc) - expected_auc^2)) /
                    (n_pos * n_neg)) * adjustment_factor
                  # Same NA guard as the prospective branch: current_se is NA when the
                  # correlation adjustment is degenerate, and `if (NA > 0)` threw.
                  z_stat_current <- if (is.finite(current_se) && current_se > 0) (expected_auc - 0.5) / current_se else NA
                  observed_power <- if (!is.na(z_stat_current)) pnorm(abs(z_stat_current) - z_alpha) + pnorm(-abs(z_stat_current) - z_alpha) else NA

                  # power_adequacy is recomputed NA-safely below; the assignment that used to
                  # be here compared n_total against a possibly-NA required_n and threw.
                  recommendation <- if (is.na(required_n)) {
                    .("Power and required sample size cannot be computed for this configuration.")
                  } else {
                    sprintf(.("Need n=%1$d (current n=%2$d) for %3$.1f%% power at an AUC distance from 0.5 of %4$s. Correlation setting: %5$s."),
                      required_n, n_total, 100 * target_power, expected_auc_diff, correlation)
                  }
                }

                power_adequacy <- if (analysis_type == "post_hoc") {
                  .("Not informative (post-hoc)")
                } else if (!is.na(observed_power) && observed_power >= target_power) {
                  .("Adequate")
                } else if (!is.na(observed_power)) {
                  .("Inadequate")
                } else {
                  .("Cannot compute")
                }

                self$results$powerAnalysisTable$addRow(rowKey = var, values = list(
                  variable = var,
                  observed_power = observed_power,
                  required_n = required_n,
                  target_effect_size = if (analysis_type == "post_hoc") abs(observed_auc - 0.5) else expected_auc_diff,
                  power_adequacy = power_adequacy,
                  recommendation = recommendation
                ))

                # Set state for power curve plot
                plot_ready <- is.finite(adjustment_factor) && adjustment_factor > 0 &&
                  is.finite(observed_power) &&
                  if (analysis_type == "post_hoc") observed_auc > 0 && observed_auc < 1 else
                    expected_auc > 0.5 && expected_auc < 1
                if (!plot_ready) {
                  private$.warnUser(.fmt(
                    .("The power curve for '{var}' is unavailable for this degenerate configuration. Use a nondegenerate target AUC and, for paired planning, a correlation below 1."),
                    var = var))
                }
                if (self$options$powerAnalysis) {
                  # addItem has no duplicate check - guard, or blank copies accumulate.
                  if (!var %in% self$results$powerCurvePlot$itemKeys)
                    self$results$powerCurvePlot$addItem(key = var)
                  image <- self$results$powerCurvePlot$get(key = var)
                  if (!is.null(image)) {
                    image$setState(list(
                      ready = plot_ready,
                      observed_power = observed_power,
                      target_power = target_power,
                      significance_level = significance_level,
                      n_total = n_total,
                      pos_frac = n_pos / n_total,
                      adjustment_factor = adjustment_factor,
                      # the AUC the table's power/required N refer to
                      auc_target = if (analysis_type == "post_hoc") observed_auc else 0.5 + expected_auc_diff,
                      required_n = required_n
                    ))
                  }
                }
              }
            },
            error = function(e) {
              # Was silent: the marker dropped out of this table while still appearing in
              # the AUC table above it, with no reason given.
              private$.warnUser(.fmt(
                .("Power analysis for '{var}' failed ({msg}), so it has no row in the Power Analysis table."),
                var = var, msg = conditionMessage(e)))
            }
          )
        }
      },

      # Calculate Bootstrap ROC analysis with prior weighting
      # NOTE: This is NOT Bayesian inference and no MCMC runs - it is bootstrap resampling
      # shrunk towards a prior AUC, and the "evidence ratio" is a resample tail count.
      .calculateBayesianROC = function(data, classVar, positiveClass) {
        vars <- self$options$dependentVars

        # Table visibility is controlled by YAML: visible: (bayesianAnalysis)
        # Add methodology clarification
        self$results$bayesianROCTable$setNote(
          key = "methodology_note",
          # Was raw English, and it stopped short of the most misleading column. The
          # "Bootstrap Evidence Ratio" is a tail count over resamples, not a Bayes factor -
          # no marginal likelihood is ever computed - so say that here rather than let the
          # 10/3/1 banding be read as Jeffreys' evidence scale.
          note = .("This analysis resamples the data by the bootstrap and shrinks the result towards the prior AUC. It does not run MCMC and it does not produce a Bayesian posterior: the interval shown is a bootstrap percentile interval, not a credible interval. The Bootstrap Evidence Ratio is a resampling frequency - the number of bootstrap resamples whose AUC exceeded 0.5, divided by one plus the number that did not - and is not a Bayes factor, because no marginal likelihood is computed. Do not read it against Bayes factor evidence thresholds. For genuine Bayesian inference use dedicated software such as Stan or JAGS."),
          init = FALSE
        )
        self$results$bayesianROCTable$setNote("seed", .fmt(.("Random seed: {seed}"), seed = self$options$seed), init = FALSE)
        # What the interval brackets. The prior weighting is applied to the resampled AUCs
        # themselves (w * AUC* + (1 - w) * prior), and a percentile interval of an affine
        # transform is that transform of the percentile interval - so the width is the AUC
        # bootstrap width multiplied by w = n / (n + precision). Labelled "95% CI" it looked
        # like an unusually precise confidence interval for the AUC; it is neither that nor a
        # credible interval, it is an interval for the weighted quantity in the column beside it.
        self$results$bayesianROCTable$setNote(
          key = "interval_scope",
          note = .("The interval brackets the Weighted Bootstrap AUC in the column beside it, not the AUC itself. The prior is treated as a fixed, known number, so the weighting multiplies the estimate and the interval width alike by n / (n + prior precision): with a prior precision of 20 and 100 complete cases the interval is 83% as wide as the bootstrap interval for the AUC, and it narrows further as you raise the precision. Read the AUC Summary table for an interval that refers to the AUC itself."),
          init = FALSE
        )

        y <- private$.classIndicator(data, positiveClass)

        # Use options from .a.yaml
        prior_auc <- self$options$priorAUC # Prior belief about AUC (0.5-1.0)
        prior_precision <- self$options$priorPrecision # Precision of prior (1-100)
        # Renamed from mcmc_samples: no MCMC runs anywhere in this method, these are
        # ordinary bootstrap resamples and the old name advertised an algorithm that is
        # not here.
        n_boot <- 2000 # Fixed number of bootstrap resamples

        for (var in vars) {
          x <- jmvcore::toNumeric(data[[var]])
          complete_cases <- complete.cases(x, y)
          x_complete <- x[complete_cases]
          y_complete <- y[complete_cases]

          tryCatch(
            {
              if (requireNamespace("pROC", quietly = TRUE)) {
                # Bootstrap resampling shrunk towards the prior AUC; not Bayesian inference.

                roc_obj <- pROC::roc(y_complete, x_complete, levels = c(0, 1), direction = ifelse(self$options$direction == ">=", "<", ">"), quiet = TRUE)
                observed_auc <- as.numeric(pROC::auc(roc_obj))

                # Simulate posterior distribution using bootstrap
                bootstrap_aucs <- numeric(n_boot)
                n <- length(y_complete)

                # Bootstrap simulation with prior weighting
                # (prior_auc and prior_precision are used in the weighted average below)
                for (i in 1:n_boot) {
                  # Bootstrap sample
                  sample_idx <- sample(n, n, replace = TRUE)
                  boot_x <- x_complete[sample_idx]
                  boot_y <- y_complete[sample_idx]

                  # Calculate AUC for bootstrap sample
                  if (length(unique(boot_y)) > 1) {
                    boot_roc <- pROC::roc(boot_y, boot_x, levels = c(0, 1), direction = ifelse(self$options$direction == ">=", "<", ">"), quiet = TRUE)
                    bootstrap_aucs[i] <- as.numeric(pROC::auc(boot_roc))
                  } else {
                    # NA, not 0.5. A resample holding a single outcome class has NO defined
                    # AUC; substituting 0.5 put a fabricated point mass at exactly chance
                    # into the displayed estimate, its interval and the evidence ratio, and
                    # it did so hardest in the small/imbalanced samples where those numbers
                    # are already least trustworthy. Dropped below, with the count disclosed.
                    bootstrap_aucs[i] <- NA_real_
                  }
                }
                n_degenerate <- sum(is.na(bootstrap_aucs))
                bootstrap_aucs <- bootstrap_aucs[!is.na(bootstrap_aucs)]
                n_kept <- length(bootstrap_aucs)
                # NOT return()/next: this sits inside tryCatch() inside `for (var in vars)`.
                # return() abandons every remaining variable and next silently skips them all
                # (both verified). The reporting block is gated instead.
                if (n_kept < 50) {
                  private$.warnUser(.fmt(
                    .("Only {kept} of {total} bootstrap resamples for '{var}' contained both outcome classes, which is too few for a stable interval, so no bootstrap AUC summary is reported for it."),
                    kept = n_kept, total = n_boot, var = var))
                } else {
                if (n_degenerate > 0) {
                  private$.warnUser(.fmt(
                    .("{n} of {total} bootstrap resamples for '{var}' contained only one outcome class and were dropped; the bootstrap AUC summary uses the remaining {kept}."),
                    n = n_degenerate, total = n_boot, kept = n_kept, var = var))
                }

                # Apply Bayesian updating: weighted average of prior and data
                # Weight data vs prior based on sample size relative to prior precision
                data_weight <- n / (n + prior_precision)
                prior_weight <- prior_precision / (n + prior_precision)
                posterior_mean <- data_weight * mean(bootstrap_aucs) + prior_weight * prior_auc

                # Posterior credible interval via weighted bootstrap samples
                posterior_samples <- data_weight * bootstrap_aucs + prior_weight * prior_auc
                credible_lower <- quantile(posterior_samples, 0.025)
                credible_upper <- quantile(posterior_samples, 0.975)

                # Tail count over the retained resamples, NOT a Bayes factor: nothing here
                # integrates a likelihood over a prior. Renamed from bayes_factor to stop the
                # code itself asserting otherwise (the displayed column is already titled
                # "Bootstrap Evidence Ratio"; the r.yaml column key stays bayes_factor because
                # the generated header defines it).
                null_samples <- sum(bootstrap_aucs <= 0.5)
                # n_kept, not n_boot: dropped degenerate resamples are not evidence.
                evidence_ratio <- (n_kept - null_samples) / (null_samples + 1)

                # Describe what the ratio counts instead of grading it as Bayesian evidence.
                # The bands are the same cut points as before (10 / 3 / 1), which correspond
                # to roughly 91%, 75% and 50% of resamples above 0.5.
                evidence_strength <- if (evidence_ratio > 10) {
                  .("AUC above 0.5 in >91% of resamples")
                } else if (evidence_ratio > 3) {
                  .("AUC above 0.5 in >75% of resamples")
                } else if (evidence_ratio > 1) {
                  .("AUC above 0.5 in >50% of resamples")
                } else {
                  .("AUC above 0.5 in no more than 50% of resamples")
                }

                # Report the actual weight; precision alone does not determine influence.
                prior_influence <- sprintf(.('Prior weight %.1f%%'), 100 * prior_weight)

                self$results$bayesianROCTable$addRow(rowKey = var, values = list(
                  variable = var,
                  posterior_auc_mean = posterior_mean,
                  credible_lower = credible_lower,
                  credible_upper = credible_upper,
                  bayes_factor = evidence_ratio, # column key is fixed by the generated header
                  evidence_strength = evidence_strength,
                  prior_influence = prior_influence
                ))

                # Set state for Bayesian trace plot
                if (self$options$bayesianAnalysis) {
                  if (!var %in% self$results$bayesianTracePlot$itemKeys) {
                    self$results$bayesianTracePlot$addItem(key = var)
                  }
                  image <- self$results$bayesianTracePlot$get(key = var)
                  if (!is.null(image)) {
                    image$setState(list(
                      ready = TRUE,
                      bootstrap_aucs = bootstrap_aucs,
                      prior_auc = prior_auc,
                      prior_precision = prior_precision,
                      n_boot = n_kept
                    ))
                  }
                }
                }
              }
            },
            error = function(e) {
              # Was silent: the marker dropped out of this table while still appearing in
              # the AUC table above it, with no reason given.
              private$.warnUser(.fmt(
                .("Bayesian ROC analysis for '{var}' failed ({msg}), so it has no row in the Bayesian table."),
                var = var, msg = conditionMessage(e)))
            }
          )
        }
      },

      # Calculate clinical utility analysis (decision curves)
      .calculateClinicalUtility = function(data, classVar, positiveClass) {
        vars <- self$options$dependentVars
        # A failed fit on a rerun must not leave the preceding model's curve visible.
        self$results$decisionCurvePlot$clear()

        # Table visibility controlled by YAML: visible: (clinicalUtilityAnalysis)

        y <- private$.classIndicator(data, positiveClass)
        prevalence <- mean(y, na.rm = TRUE)

        # Use actual options from .a.yaml
        harm_benefit_ratio <- self$options$harmBenefitRatio # Default: 0.25
        intervention_cost <- self$options$interventionCost # Default: false

        grid <- .psychopdaThresholdGrid(self$options$treatmentThreshold)
        threshold_range <- grid$values
        if (grid$fallback) {
          msg <- .("The Treatment Threshold Range could not be read as \"min,max,step\" with 0 < min <= max < 1 and a positive step. The default range 0.05 to 0.50 in steps of 0.05 was used instead.")
          self$results$decisionCurveTable$setNote("range_fallback", msg, init = FALSE)
          private$.warnUser(msg)
        }
        if (grid$capped) {
          msg <- .("The requested treatment threshold grid exceeds 100 points. Tables and plots use 100 equally spaced thresholds spanning the requested minimum and maximum.")
          self$results$decisionCurveTable$setNote("range_capped", msg, init = FALSE)
          private$.warnUser(msg)
        }

        # Both tables report Vickers & Elkin net benefit on a logistic risk fitted to these same
        # rows. Say so: the risk model is internally fitted, so these are apparent estimates.
        self$results$clinicalUtilityTable$setNote(
          key = "definition",
          note = .("Net benefit (Vickers & Elkin 2006) at the mid-point of the treatment threshold range: patients are treated when their predicted risk reaches the threshold probability. The risk model is fitted to these same data, so this is an apparent, uncorrected estimate."),
          init = FALSE
        )
        self$results$decisionCurveTable$setNote(
          key = "definition",
          note = .("Net benefit (Vickers & Elkin 2006) across the treatment threshold range. Compare each row against Treat All and Treat None: a strategy is only worth using when it beats both. The risk model is fitted to these same data, so these are apparent, uncorrected estimates."),
          init = FALSE
        )

        # Use midpoint of threshold range as default for single analysis
        threshold_prob <- median(threshold_range)

        for (var in vars) {
          x <- jmvcore::toNumeric(data[[var]])
          complete_cases <- complete.cases(x, y)
          x_complete <- x[complete_cases]
          y_complete <- y[complete_cases]

          tryCatch(
            {
              if (requireNamespace("pROC", quietly = TRUE)) {
                roc_obj <- pROC::roc(y_complete, x_complete, levels = c(0, 1), direction = ifelse(self$options$direction == ">=", "<", ">"), quiet = TRUE)

                # Net benefit, Vickers & Elkin (Med Decis Making 2006;26:565-74).
                #
                # This used to freeze sensitivity and specificity at the YOUDEN cutoff and vary
                # only the odds weight. That is a different estimand: a decision curve requires
                # the classification itself to move with the threshold - treat when predicted
                # risk reaches pt - so the old number was "net benefit of one fixed rule, priced
                # at pt", systematically below the true value whenever pt sat away from the
                # Youden point (0.2152 against a hand-computed 0.2793 at pt = 0.10). It also
                # disagreed with this analysis's own decisionCurveTable, which already used the
                # correct rule, by about 29%.
                # Center and scale before fitting: a large measurement offset can make
                # glm drop the raw predictor as collinear with its intercept. This is
                # a change of coordinates, so the fitted risks must remain unchanged.
                x_center <- mean(x_complete)
                x_spread <- max(abs(x_complete - x_center))
                if (!is.finite(x_center) || !is.finite(x_spread))
                  stop(.("The marker could not be scaled to finite values."))
                if (x_spread == 0) x_spread <- 1
                x_scaled <- (x_complete - x_center) / x_spread
                fit_warnings <- character(0)
                nb_fit <- withCallingHandlers(
                  stats::glm(y_complete ~ x_scaled, family = stats::binomial()),
                  warning = function(w) {
                    fit_warnings <<- c(fit_warnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                  })
                nb_risk <- as.numeric(stats::fitted(nb_fit))
                if (length(nb_risk) != length(y_complete) || any(!is.finite(nb_risk)))
                  stop(.("The fitted risk probabilities are unavailable."))
                if (!isTRUE(nb_fit$converged))
                  fit_warnings <- c(fit_warnings, .("The logistic model did not converge."))
                if (length(fit_warnings)) {
                  fit_note <- .fmt(
                    .("Decision-curve risk fitting for '{var}' reported: {details}. The apparent net benefit may be unreliable; check separation and model convergence before interpreting it."),
                    var = var, details = paste(unique(fit_warnings), collapse = "; "))
                  private$.warnUser(fit_note, type = jmvcore::NoticeType$STRONG_WARNING)
                  for (table in list(self$results$clinicalUtilityTable,
                                     self$results$decisionCurveTable))
                    table$setNote(paste0("fit_", var), fit_note)
                }
                n_v <- length(y_complete)
                prevalence_v <- mean(y_complete)

                # A one-predictor logistic fit is invariant to the sign the user declared:
                # glm(y ~ x) and glm(y ~ -x) give identical fitted probabilities. So the
                # calibrated risk always runs in the DATA-OPTIMAL direction. When that contradicts
                # the declared Classification Direction, the net benefit below describes the
                # opposite decision rule to the one configured - and always the more favourable
                # one - so say so rather than printing a bare verdict.
                fitted_slope <- tryCatch(stats::coef(nb_fit)[[2]], error = function(e) NA_real_)
                declared_up <- identical(self$options$direction, ">=")
                if (is.finite(fitted_slope) && fitted_slope != 0 &&
                    ((fitted_slope > 0) != declared_up)) {
                  self$results$clinicalUtilityTable$setNote(
                    key = paste0("dir_", var),
                    note = sprintf(
                      .("%s: net benefit is computed on the direction the data support, which is the opposite of the Classification Direction you selected. Under the direction you selected this marker performs worse than shown here."),
                      var),
                    init = FALSE
                  )
                }

                treated <- nb_risk >= threshold_prob
                tp_rate <- sum(treated & y_complete == 1) / n_v
                fp_rate <- sum(treated & y_complete == 0) / n_v

                net_benefit <- tp_rate - fp_rate * (threshold_prob / (1 - threshold_prob))

                # Treat all strategy
                treat_all_benefit <- prevalence_v - (1 - prevalence_v) * (threshold_prob / (1 - threshold_prob))

                # Treat none strategy
                treat_none_benefit <- 0

                # Standardized net benefit (guard against zero prevalence)
                standardized_net_benefit <- if (prevalence_v > 1e-10) net_benefit / prevalence_v else NA

                # Report what the net-benefit comparison shows at this one threshold, not a
                # verdict on whether the marker should be used in patient care: the cutoff was
                # found by Youden on these same data and no interval is attached.
                clinical_utility <- if (net_benefit > max(treat_all_benefit, treat_none_benefit)) {
                  .("Net benefit exceeds both treat-all and treat-none at this threshold")
                } else if (net_benefit > treat_none_benefit) {
                  .("Net benefit exceeds treat-none but not treat-all at this threshold")
                } else {
                  .("Net benefit does not exceed treat-none at this threshold")
                }

                self$results$clinicalUtilityTable$addRow(rowKey = var, values = list(
                  variable = var,
                  threshold_probability = threshold_prob,
                  net_benefit = net_benefit,
                  treat_all_benefit = treat_all_benefit,
                  treat_none_benefit = treat_none_benefit,
                  standardized_net_benefit = standardized_net_benefit,
                  clinical_utility = clinical_utility
                ))

                # Add detailed threshold analysis for decision curve.
                # Clamp away from 0 and 1 to avoid extreme t/(1-t) values.
                #
                # NOTE the name: this grid used to be called `threshold_range`, which SHADOWED
                # the user's Treatment Threshold Range parsed above. The rename fixed the
                # shadowing but not the wiring - the grid stayed a hardcoded
                # seq(0.01, 0.90, length.out = 20), so the decision curve was byte-identical
                # whatever range the user typed. Drive it from the option.
                detail_thresholds <- threshold_range
                n_thresholds <- length(detail_thresholds)

                # Same calibrated-risk rule the decision-curve PLOT uses, so the
                # table and the figure can no longer disagree. The previous rule
                # cut at quantile(x, 1 - t) -- "treat the top t fraction" -- which
                # makes a HIGHER treatment threshold treat MORE patients, the exact
                # opposite of what a threshold probability means. Net benefit was
                # off by up to 4.69 against the dcurves reference.
                # One fit drives the summary, threshold table and saved plot state.
                # Fitting warnings have already been surfaced as notices and table notes.
                dca_tbl_risk <- nb_risk
                n_dca_tbl <- length(y_complete)

                for (t in detail_thresholds) {
                  if (!is.null(dca_tbl_risk) && length(dca_tbl_risk) == n_dca_tbl) {
                    treat_t <- dca_tbl_risk >= t
                    tp_rate_t <- sum(treat_t & y_complete == 1) / n_dca_tbl
                    fp_rate_t <- sum(treat_t & y_complete == 0) / n_dca_tbl
                    net_benefit_t <- tp_rate_t - fp_rate_t * (t / (1 - t))

                    # "Interventions avoided" is the standard net reduction in
                    # interventions relative to treating everyone, per 100 patients:
                    #   (NB_model - NB_treat_all) / (t/(1-t)) * 100
                    # It used to be round(fp_rate * n), i.e. the count of FALSE
                    # POSITIVES -- unnecessary interventions PERFORMED, the exact
                    # opposite of the column's name.
                    # Use the complete-case prevalence, matching the rows the model curve is
                    # computed on. `prevalence` is the all-rows value, so with any missingness
                    # in the marker the benchmark and the curve sat on different denominators.
                    prev_dca <- mean(y_complete)
                    treat_all_nb_t <- prev_dca - (1 - prev_dca) * (t / (1 - t))
                    odds_t <- t / (1 - t)
                    interventions_avoided <- if (odds_t > 0)
                      round((net_benefit_t - treat_all_nb_t) / odds_t * 100) else NA_integer_
                    cases_detected <- round(tp_rate_t * n_dca_tbl)

                    # A strategy is worth using when it beats BOTH default
                    # strategies: treat everyone and treat nobody. The previous test
                    # (net_benefit > t * prevalence) appears in no DCA literature and
                    # disagreed with this standard rule on half the rows, every time
                    # calling the marker useless where it was in fact the best option.
                    clinical_value <- if (net_benefit_t > max(0, treat_all_nb_t)) {
                      .("Beneficial")
                    } else {
                      .("Not beneficial")
                    }
                    # A net benefit means nothing without the two default strategies it has to
                    # beat. There is no column for them in this table, so name them in the cell.
                    clinical_value <- sprintf(
                      .("%1$s (treat all: %2$.4f, treat none: 0)"), clinical_value, treat_all_nb_t)

                    self$results$decisionCurveTable$addRow(
                      rowKey = paste0(var, "_", t),
                      values = list(
                        variable = var,
                        threshold = t,
                        net_benefit = net_benefit_t,
                        interventions_avoided = interventions_avoided,
                        cases_detected = cases_detected,
                        clinical_value = clinical_value
                      )
                    )
                  }
                }

                # Set state for decision curve plot
                if (self$options$clinicalUtilityAnalysis) {
                  # addItem has no duplicate check - guard, or blank copies accumulate.
                  if (!var %in% self$results$decisionCurvePlot$itemKeys)
                    self$results$decisionCurvePlot$addItem(key = var)
                  image <- self$results$decisionCurvePlot$get(key = var)
                  if (!is.null(image)) {
                    # Decision curve analysis thresholds a PREDICTED RISK at the
                    # threshold probability t. Passing the ROC curve instead forced
                    # the renderer to guess an operating point, and the guess it
                    # made (specificity = 1 - t) ran BACKWARDS: a higher treatment
                    # threshold must treat FEWER patients, so specificity has to
                    # RISE with t, but 1 - t falls. Measured cor(t, spec) = -1.000
                    # exactly, against +0.96 for the correct rule; the plotted net
                    # benefit was wrong by up to 0.181 and changed SIGN at t = 0.50.
                    # Calibrate the marker to a probability once, here, and let the
                    # renderer threshold it directly.
                    # Reuse the table's fit (dca_tbl_risk): a separate fit here caught
                    # warnings as NULL, so a perfectly separating marker got a table but
                    # an empty plot; and `prevalence` was the all-rows value while the
                    # curve is computed on complete cases, so Treat All sat on a
                    # different denominator whenever the marker had missing values.
                    image$setState(list(
                      ready = TRUE,
                      threshold_range = threshold_range,
                      harm_benefit_ratio = harm_benefit_ratio,
                      prevalence = mean(y_complete),
                      intervention_cost = intervention_cost,
                      risk = dca_tbl_risk,
                      outcome = as.integer(y_complete),
                      sensitivities = roc_obj$sensitivities,
                      specificities = roc_obj$specificities
                    ))
                  }
                }
              }
            },
            error = function(e) {
              # Was silent: the marker dropped out of this table while still appearing in
              # the AUC table above it, with no reason given.
              private$.warnUser(.fmt(
                .("Clinical utility analysis for '{var}' failed ({msg}), so it has no row in the Clinical Utility table."),
                var = var, msg = conditionMessage(e)))
            }
          )
        }
      },

      # Calculate meta-analysis across multiple predictors
      .calculateMetaAnalysis = function() {
        # This analysis has marker columns from one cohort, not independent studies.
        # A legacy override cannot supply the missing study-level sampling design.
        self$results$metaAnalysisTable$setVisible(FALSE)
        self$results$metaAnalysisForestPlot$clear()
        self$results$metaAnalysisForestPlot$setVisible(FALSE)
        message <- .("Meta-analysis is unavailable: markers from the same cohort are not independent studies. No pooled AUC, interval, heterogeneity statistic or forest plot is computed. Use paired DeLong comparisons for these markers, or a study-level meta-analysis with independent cohorts.")
        if (isTRUE(self$options$overrideMetaAnalysisWarning)) message <- paste(message,
          .("The saved legacy override is ignored because it cannot correct dependence between markers."))
        self$results$metaAnalysisWarning$setContent(paste0("<p>", message, "</p>"))
        self$results$metaAnalysisWarning$setVisible(TRUE)
        private$.warnUser(message, type = jmvcore::NoticeType$STRONG_WARNING)
      },

      # Enhanced input validation inspired by old testroc implementation
      .validateInputs = function() {
        if (isTRUE(self$options$partialAUC) &&
            self$options$partialAUCfrom >= self$options$partialAUCto) {
          return(.("Partial AUC requires the specificity From value to be smaller than the To value. Choose a range with positive width."))
        }
        # Enhanced validation for manual cutpoint method
        if (self$options$method == "oc_manual") {
          if (is.null(self$options$specifyCutScore) || self$options$specifyCutScore == "") {
            return(.("Please specify a cut score for using the manual cutpoint method."))
          }

          # Validate that cut score is numeric and finite
          cut_score <- suppressWarnings(as.numeric(self$options$specifyCutScore))
          if (is.na(cut_score) || !is.finite(cut_score)) {
            return(.("Cut score must be a valid finite number."))
          }
        }

        # Enhanced validation for DeLong test
        if (self$options$delongTest && length(self$options$dependentVars) < 2) {
          return(.("Please specify at least two dependent variables to use DeLong's test."))
        }

        # Enhanced validation for IDI/NRI. Reference Test for IDI/NRI may be any numeric column, not
        # only a test variable (jmvcore rejects a non-numeric one before .run(): permitted: numeric);
        # a reference that is also a test variable is not compared with itself, so count distinct tests.
        if ((self$options$calculateIDI || self$options$calculateNRI) &&
          length(union(self$options$dependentVars, self$options$refVar)) < 2) {
          return(.("IDI/NRI need two tests: put at least two in Test Variables, or one there and a different test in Reference Test for IDI/NRI."))
        }

        # The class variable is the gold standard; as the reference, every test would be compared with the outcome itself.
        if ((self$options$calculateIDI || self$options$calculateNRI) &&
          isTRUE(self$options$refVar %in% self$options$classVar)) {
          return(.("Reference Test for IDI/NRI holds the class variable, the gold standard itself. Put a test to compare against there, or leave it empty to use the first test variable."))
        }

        # Enhanced validation for subgroup analysis with DeLong
        if (self$options$delongTest && !is.null(self$options$subGroup)) {
          return(.("DeLong's test does not currently support the group variable. Please remove grouping or disable DeLong test."))
        }

        return(NULL) # No errors found
      },

      # ============================================================================
      # ADVANCED PLOTTING METHODS
      # ============================================================================

      # Plot effect size visualization
      .plotEffectSize = function(image, ggtheme, theme, ...) {
        if (is.null(image$state) || length(self$options$dependentVars) < 2) {
          return(FALSE)
        }

        # Create effect size comparison plot
        if (private$.checkPackageDependencies("ggplot2", "effect size visualization")) {
          # Prepare data for plotting from the effectSizeTable
          plot_data <- data.frame(
            comparison = character(0),
            cohens_d = numeric(0),
            magnitude = character(0)
          )

          # Extract data from results table
          vars <- self$options$dependentVars
          for (i in 1:(length(vars) - 1)) {
            for (j in (i + 1):length(vars)) {
              row_key <- .psychopdaPairKey(i, j)
              if (length(self$results$effectSizeTable$rowKeys) > 0 &&
                row_key %in% self$results$effectSizeTable$rowKeys) {
                row_data <- self$results$effectSizeTable$getRow(rowKey = row_key)
                comp_val <- if (inherits(row_data$comparison, "R6")) row_data$comparison$value else row_data$comparison
                d_val <- if (inherits(row_data$cohens_d, "R6")) row_data$cohens_d$value else row_data$cohens_d
                mag_val <- if (inherits(row_data$effect_magnitude, "R6")) row_data$effect_magnitude$value else row_data$effect_magnitude
                plot_data <- rbind(plot_data, data.frame(
                  comparison = as.character(comp_val),
                  cohens_d = as.numeric(d_val),
                  magnitude = as.character(mag_val),
                  stringsAsFactors = FALSE
                ))
              }
            }
          }

          if (nrow(plot_data) > 0) {
            # Preserve table order and give complete marker names room along the vertical axis.
            plot_data$comparison <- factor(plot_data$comparison,
              levels = rev(plot_data$comparison))
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = comparison, y = cohens_d, fill = magnitude)) +
              ggplot2::geom_col(alpha = 0.7) +
              ggplot2::coord_flip() +
              ggplot2::geom_hline(
                yintercept = c(-0.8, -0.5, -0.2, 0.2, 0.5, 0.8),
                linetype = "dashed", alpha = 0.5
              ) +
              ggplot2::labs(
                title = .("Difference in Cohen's d between markers"),
                x = .("Comparison (first vs second marker)"),
                y = .("Cohen's d, second minus first"),
                fill = .("Size of difference")
              ) +
              ggtheme +
              ggplot2::theme(legend.position = "bottom")

            print(p)
            return(TRUE)
          }
        }
        return(FALSE)
      },

      # Plot power analysis curves
      .plotPowerCurves = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }

        if (private$.checkPackageDependencies("ggplot2", "power analysis visualization")) {
          state <- image$state
          # Same Hanley-McNeil power the table reports, at this sample's positive fraction
          # and the chosen alpha. The previous curve used a crude SE (sqrt(0.25/(n/2))) on
          # an "effect size" scale unrelated to the table, so the two disagreed.
          pos_frac <- state$pos_frac
          alpha <- state$significance_level
          adj <- state$adjustment_factor
          if (is.null(pos_frac) || !is.finite(pos_frac) || pos_frac <= 0 || pos_frac >= 1 ||
              is.null(alpha) || !is.finite(alpha)) {
            return(FALSE)
          }
          if (!isTRUE(state$ready) || is.null(adj) || !is.finite(adj) || adj <= 0)
            return(FALSE)
          target_power <- state$target_power
          n_total <- state$n_total
          auc_target <- state$auc_target
          required_n <- state$required_n

          n_max <- max(500, 2 * n_total, if (is.finite(required_n)) 1.2 * required_n else 0)
          sample_sizes <- unique(round(seq(20, n_max, length.out = 60)))
          auc_grid <- c(0.60, 0.65, 0.70, 0.75, 0.80, 0.90)
          if (is.finite(auc_target) && auc_target > 0 && auc_target < 1 && auc_target != 0.5) {
            auc_grid <- sort(unique(c(auc_grid, auc_target)))
          }
          power_data <- expand.grid(auc = auc_grid, n = sample_sizes)
          power_data$power <- mapply(function(a, n) private$.hanleyMcNeilPower(a, n, pos_frac, alpha, adj),
                                     power_data$auc, power_data$n)
          power_data$curve <- ifelse(is.finite(auc_target) & power_data$auc == auc_target,
                                     sprintf("%.3f (target)", power_data$auc), sprintf("%.2f", power_data$auc))

          p <- ggplot2::ggplot(power_data, ggplot2::aes(x = n, y = power, color = curve)) +
            ggplot2::geom_line(linewidth = 1) +
            ggplot2::geom_hline(yintercept = target_power, linetype = "dashed", color = "red") +
            ggplot2::geom_vline(xintercept = n_total, linetype = "dotted") +
            ggplot2::labs(
              title = .("Power to detect AUC different from 0.5 (Hanley-McNeil)"),
              subtitle = sprintf(.("alpha = %1$s, positive fraction = %2$.1f%%, dotted line = current n = %3$d%4$s"),
                                 format(alpha), 100 * pos_frac, as.integer(n_total),
                                 if (adj != 1) paste0(", ", sprintf(.("correlation adjustment = %.2f"), adj)) else ""),
              x = .("Total sample size"),
              y = .("Statistical power"),
              color = .("AUC")
            ) +
            ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
            # Was theme_minimal(), which ignored the ggtheme argument jamovi passes in.
            ggtheme

          print(p)
          return(TRUE)
        }
        return(FALSE)
      },

      # Plot bootstrap AUC trace from actual bootstrap samples
      .plotBayesianTrace = function(image, ggtheme, theme, ...) {
        state <- image$state
        if (is.null(state) || !isTRUE(state$ready)) {
          return(FALSE)
        }
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
          return(FALSE)
        }

        bootstrap_aucs <- state$bootstrap_aucs
        if (is.null(bootstrap_aucs) || length(bootstrap_aucs) == 0) {
          return(FALSE)
        }

        n_samples <- length(bootstrap_aucs)
        trace_df <- data.frame(
          iteration = seq_len(n_samples),
          auc = bootstrap_aucs
        )

        p <- ggplot2::ggplot(trace_df, ggplot2::aes(x = iteration, y = auc)) +
          ggplot2::geom_line(alpha = 0.5, color = "steelblue") +
          ggplot2::geom_hline(
            yintercept = mean(bootstrap_aucs),
            linetype = "dashed", color = "darkred", linewidth = 0.8
          ) +
          ggplot2::geom_hline(
            yintercept = quantile(bootstrap_aucs, c(0.025, 0.975)),
            linetype = "dotted", color = "grey40"
          ) +
          ggplot2::labs(
            title = .("Bootstrap AUC Samples"),
            subtitle = sprintf(
              # NOT "... CI [%2$.3f, %3$.3f]": jmvcore's parseContext() treats a
              # TRAILING " [...]" as msgctxt and STRIPS it, so the msgid became
              # "Mean = %1$.3f, 95%% CI" and the interval bounds vanished from the
              # subtitle. Parentheses carry no such meaning.
              .("Mean = %1$.3f, 95%% CI (%2$.3f to %3$.3f)"),
              mean(bootstrap_aucs),
              quantile(bootstrap_aucs, 0.025),
              quantile(bootstrap_aucs, 0.975)
            ),
            x = .("Bootstrap Iteration"),
            y = .("AUC")
          ) +
          # Was theme_minimal(); ggtheme (jamovi's global theme) replaces it, and the
          # legend tweak must come AFTER ggtheme or ggtheme would wipe it out.
          ggtheme +
          ggplot2::theme(legend.position = "none")

        print(p)
        return(TRUE)
      },

      # Plot decision curve analysis using data-driven net benefit (Vickers 2006)
      .plotDecisionCurve = function(image, ggtheme, theme, ...) {
        state <- image$state
        if (is.null(state) || !isTRUE(state$ready)) {
          return(FALSE)
        }
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
          return(FALSE)
        }

        prevalence <- state$prevalence
        if (is.null(prevalence) || is.na(prevalence)) {
          return(FALSE)
        }

        # Read ROC data from state (set by .calculateClinicalUtility)
        roc_sens <- state$sensitivities
        roc_spec <- state$specificities
        if (is.null(roc_sens) || is.null(roc_spec)) {
          return(FALSE)
        }

        # Threshold grid comes from the user's Treatment Threshold Range. The plot
        # used to hardcode seq(0.01, 0.99, 0.01) and ignore the option entirely,
        # so the curve never matched the range the clinical-utility TABLE used.
        threshold_probs <- state$threshold_range
        if (!length(threshold_probs) || !all(is.finite(threshold_probs)) ||
            any(threshold_probs <= 0 | threshold_probs >= 1)) return(FALSE)

        dc_data <- data.frame()

        # Threshold the CALIBRATED RISK at t and count the resulting classifications.
        # This is the Vickers & Elkin definition; the previous ROC-walking rule was
        # monotonically backwards (see the note where the state is built).
        risk <- state$risk
        outcome <- state$outcome
        use_risk <- !is.null(risk) && !is.null(outcome) &&
          length(risk) == length(outcome) && length(risk) > 0 && all(is.finite(risk))
        if (!use_risk) return(FALSE)
        n_total_dca <- length(outcome)

        for (t in threshold_probs) {
          treat <- risk >= t
          tp_rate <- sum(treat & outcome == 1) / n_total_dca
          fp_rate <- sum(treat & outcome == 0) / n_total_dca

          # Net benefit = TP/N - FP/N * (pt / (1 - pt))
          nb <- tp_rate - fp_rate * (t / (1 - t))

          # Treat All: NB = prevalence - (1-prevalence) * pt/(1-pt)
          treat_all_nb <- prevalence - (1 - prevalence) * (t / (1 - t))

          dc_data <- rbind(dc_data, data.frame(
            threshold = t, net_benefit = nb,
            strategy = "Model", type = "Model"
          ))
          dc_data <- rbind(dc_data, data.frame(
            threshold = t, net_benefit = treat_all_nb,
            strategy = "Treat All", type = "Reference"
          ))
          dc_data <- rbind(dc_data, data.frame(
            threshold = t, net_benefit = 0,
            strategy = "Treat None", type = "Reference"
          ))
        }

        # --- Intervention-cost view -------------------------------------------
        # "Intervention cost analysis" now selects the standard second DCA
        # readout: net reduction in interventions per 100 patients, relative to
        # treating everyone (Vickers & Elkin 2006). It was previously written
        # into the plot state and then ignored by this renderer, so ticking it
        # produced a byte-identical image.
        #   reduction per 100 = (NB_model - NB_treat_all) / (pt / (1 - pt)) * 100
        show_reduction <- isTRUE(state$intervention_cost)
        if (show_reduction) {
          nb_model <- dc_data$net_benefit[dc_data$strategy == "Model"]
          nb_all <- dc_data$net_benefit[dc_data$strategy == "Treat All"]
          pt <- dc_data$threshold[dc_data$strategy == "Model"]
          odds <- pt / (1 - pt)
          reduction <- ifelse(odds > 0, (nb_model - nb_all) / odds * 100, NA_real_)
          dc_data <- data.frame(
            threshold = pt, net_benefit = reduction,
            strategy = "Model", type = "Model", stringsAsFactors = FALSE
          )
          # On this scale "treat all" is zero by construction - it is the baseline
          # the reduction is measured against, so it is the reference line.
          dc_data <- rbind(dc_data, data.frame(
            threshold = pt, net_benefit = 0,
            strategy = "Treat All (reference)", type = "Reference", stringsAsFactors = FALSE
          ))
        }

        y_lab <- if (show_reduction) {
          .("Net reduction in interventions per 100 patients")
        } else {
          .("Net Benefit")
        }
        plot_title <- if (show_reduction) {
          .("Decision Curve Analysis - interventions avoided")
        } else {
          .("Decision Curve Analysis")
        }

        finite_nb <- dc_data$net_benefit[is.finite(dc_data$net_benefit)]
        y_hi <- if (length(finite_nb)) max(finite_nb) else 1
        y_lo <- if (show_reduction && length(finite_nb)) min(0, min(finite_nb)) else -0.05

        p <- ggplot2::ggplot(dc_data, ggplot2::aes(
          x = threshold, y = net_benefit,
          color = strategy, linetype = type
        )) +
          (if (length(threshold_probs) == 1L) ggplot2::geom_point(size = 2.5) else
            ggplot2::geom_line(linewidth = 1)) +
          ggplot2::coord_cartesian(ylim = c(y_lo, y_hi * 1.1 + 1e-8)) +
          ggplot2::labs(
            title = plot_title,
            x = .("Threshold Probability"),
            y = y_lab,
            color = .("Strategy"),
            linetype = .("Type")
          ) +
          # Was theme_minimal(), so jamovi's global theme (ggtheme) was never applied.
          ggtheme

        # --- Harm-to-benefit reference ----------------------------------------
        # A harm:benefit ratio IS a threshold probability: pt = h / (1 + h). Mark
        # it, so the clinician can read the curve at the trade-off they declared
        # instead of guessing which x-value corresponds to it. This option was
        # also carried in the state and never read.
        hbr <- state$harm_benefit_ratio
        if (!is.null(hbr) && is.finite(hbr) && hbr > 0) {
          pt_star <- hbr / (1 + hbr)
          if (pt_star > min(threshold_probs) && pt_star < max(threshold_probs)) {
            p <- p +
              ggplot2::geom_vline(xintercept = pt_star, linetype = "dotted",
                                  colour = "grey30", linewidth = 0.6) +
              ggplot2::annotate(
                "text", x = pt_star, y = y_hi * 1.05 + 1e-8, hjust = -0.05, size = 3,
                colour = "grey30",
                label = sprintf(.("harm:benefit %1$.2f (threshold %2$.0f%%)"), hbr, pt_star * 100)
              )
          }
        }

        print(p)
        return(TRUE)
      },

      # ============================================================================
      # META-ANALYSIS FOREST PLOT GENERATION
      # ============================================================================

      # Generate forest plot for meta-analysis results
      .generateMetaAnalysisForestPlot = function(aucs, se_aucs, vars, combined_result) {
        tryCatch(
          {
            # Create data frame for forest plot (limits as in the table, so none falls past the
            # renderer's 0-1 scale and is silently dropped)
            lim <- private$.aucLimits(aucs, se_aucs)
            plot_data <- data.frame(
              study = vars,
              auc = aucs,
              se = se_aucs,
              ci_lower = lim$lower,
              ci_upper = lim$upper,
              stringsAsFactors = FALSE
            )

            # Add combined result
            if (!is.null(combined_result)) {
              combined_row <- data.frame(
                study = "Combined",
                auc = combined_result$auc,
                se = combined_result$se,
                ci_lower = combined_result$ci_lower,
                ci_upper = combined_result$ci_upper,
                stringsAsFactors = FALSE
              )
              plot_data <- rbind(plot_data, combined_row)
            }

            # library-audit 2026-09-16 meddecide [HIGH] DONE: the state is the renderer's only input;
            #   export and .omv reopen redraw without .run(), so a private$ copy was NULL there
            # jmvcore::Array$addItem has NO duplicate check, and this item's clearWith does
            # not list metaAnalysisMethod / forestPlot / direction, so toggling one of those
            # re-ran this block and stacked a second identical forest plot. Same guard as
            # fixedSensSpecROC above.
            if (!"forestPlot" %in% self$results$metaAnalysisForestPlot$itemKeys) {
              self$results$metaAnalysisForestPlot$addItem(key = "forestPlot")
            }
            image <- self$results$metaAnalysisForestPlot$get(key = "forestPlot")
            image$setState(list(data = plot_data))
          },
          error = function(e) {
            # Say why the plot is missing; hiding it left the user with a silent gap.
            self$results$metaAnalysisWarning$setContent(paste0(
              "<p>", jmvcore::htmlEscape(.fmt(
                .("The meta-analysis forest plot could not be built: {msg}"),
                msg = conditionMessage(e))), "</p>"))
            self$results$metaAnalysisWarning$setVisible(TRUE)
          }
        )
      },

      # Forest plot render function (called by jamovi)
      .plotMetaAnalysisForest = function(image, ggtheme, theme, ...) {
        plot_data <- image$state$data
        if (is.null(plot_data)) {
          return(FALSE)
        }
        plot_data <- as.data.frame(plot_data)

        # Create forest plot
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = auc, y = study)) +
          ggplot2::geom_point(size = 3) +
          ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
          ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.7) +
          ggplot2::labs(
            title = .("Meta-Analysis Forest Plot"),
            subtitle = .("AUC values with 95% confidence intervals"),
            x = .("Area Under the Curve (AUC)"),
            y = .("Test Variable")
          ) +
          # Was theme_minimal(); ggtheme is jamovi's global theme and replaces it. The
          # centring/grid tweaks below must follow ggtheme or they are discarded.
          ggtheme +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5),
            panel.grid.minor = ggplot2::element_blank()
          ) +
          ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))

        # Add text annotations for AUC values
        p <- p + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", auc)),
          vjust = -0.5, size = 3
        )

        print(p)
        return(TRUE)
      },

      # ============================================================================
      # META-ANALYSIS UI CONTROL METHOD
      # ============================================================================

      # Update meta-analysis visibility based on number of variables
      .updateMetaAnalysisVisibility = function() {
        enabled <- isTRUE(self$options$metaAnalysis)
        self$results$metaAnalysisWarning$setVisible(enabled)
        self$results$metaAnalysisTable$setVisible(FALSE)
        self$results$metaAnalysisForestPlot$setVisible(FALSE)
        self$results$metaAnalysisForestPlot$clear()
        if (!enabled) self$results$metaAnalysisWarning$setContent("")
        enabled
      }
    ), # End of private list
    public = list(
      #' @description Initialize safely when an R caller has not selected any variables.
      #' @param noThrow Capture initialization errors in the analysis error state when TRUE.
      init = function(noThrow = FALSE) {
        # jmvcore::select() cannot assign nonempty row names to zero selected columns.
        # Let the framework initialize an empty view, then retain the caller's data
        # for the welcome state and subsequent variable selection on this instance.
        if (is.data.frame(private$.data) && length(self$options$varsRequired) == 0L) {
          original <- private$.data
          private$.data <- data.frame()
          on.exit(private$.data <- original, add = TRUE)
        }
        super$init(noThrow = noThrow)
      },

      #' @description
      #' Generate R source code for psychopdaROC analysis
      #' @return Character string with R syntax for reproducible analysis
      asSource = function() {
        dependentVars <- self$options$dependentVars
        if (is.null(dependentVars) || length(dependentVars) == 0) {
          return("")
        }

        # Get arguments using base helper (if available)
        args <- ""
        if (!is.null(private$.asArgs)) {
          args <- private$.asArgs(incData = FALSE)
        }
        if (args != "") {
          args <- paste0(",\n    ", args)
        }

        # Get package name dynamically
        pkg_name <- utils::packageName()
        if (is.null(pkg_name)) pkg_name <- "ClinicoPath" # fallback

        # Build complete function call
        paste0(pkg_name, "::psychopdaROC(\n    data = data", args, ")")
      }
    ) # End of public list
  )
}
