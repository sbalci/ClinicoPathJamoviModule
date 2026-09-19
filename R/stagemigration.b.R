#' @title Advanced TNM Stage Migration Analysis
#'
#' @description
#' State-of-the-art analysis for validating TNM staging system improvements using
#' comprehensive statistical methods. This analysis provides pathologists with robust
#' tools to evaluate whether a new staging system provides superior prognostic
#' discrimination compared to existing systems.
#'
#' @details
#' This comprehensive staging validation analysis includes:
#'
#' \strong{Core Migration Analysis:}
#' \itemize{
#'   \item Migration matrices with detailed statistics
#'   \item Stage distribution comparisons
#'   \item Will Rogers phenomenon detection
#'   \item Upstaging and downstaging quantification
#' }
#'
#' \strong{Advanced Discrimination Metrics:}
#' \itemize{
#'   \item Harrell's C-index with confidence intervals
#'   \item Net Reclassification Improvement (NRI)
#'   \item Integrated Discrimination Improvement (IDI)
#'   \item Time-dependent ROC analysis
#'   \item Likelihood ratio tests for nested models
#' }
#'
#' \strong{Clinical Utility Assessment:}
#' \itemize{
#'   \item Decision Curve Analysis (DCA)
#'   \item Net benefit calculations
#'   \item Clinical significance thresholds
#'   \item Cancer-type specific interpretations
#' }
#'
#' \strong{Validation Framework:}
#' \itemize{
#'   \item Bootstrap validation with optimism correction
#'   \item Cross-validation options
#'   \item Stability assessment
#'   \item Internal validation metrics
#' }
#'
#' \strong{Advanced Visualizations:}
#' \itemize{
#'   \item Migration heatmaps with flow statistics
#'   \item Time-dependent ROC curves
#'   \item Calibration plots
#'   \item Decision curves
#'   \item Forest plots with confidence intervals
#' }
#'
#' \strong{PHASE 1 ENHANCEMENTS - Evidence-Based Assessment Framework:}
#' \itemize{
#'   \item \strong{Will Rogers Evidence Assessment:} Multi-criteria evaluation framework
#'   \item \strong{Migration Pattern Analysis:} Advanced flow statistics and retention rates
#'   \item \strong{Survival Pattern Validation:} Upstaged patient survival similarity analysis
#'   \item \strong{Biological Consistency Checks:} Risk factor profile assessments
#'   \item \strong{Landmark Analysis Integration:} Time-based cutoff discrimination analysis
#'   \item \strong{Clinical Decision Support:} Evidence-based implementation recommendations
#'   \item \strong{Traffic Light Assessment:} PASS/BORDERLINE/CONCERN/FAIL evidence grading
#'   \item \strong{Enhanced Heatmap Analytics:} Major flow identification and net migration analysis
#' }
#'
#' @section Clinical Applications:
#' \itemize{
#'   \item TNM staging system validation (7th to 8th edition transitions)
#'   \item AJCC staging improvements
#'   \item Institution-specific staging modifications
#'   \item Multi-institutional staging harmonization
#'   \item Biomarker-enhanced staging systems
#' }
#'
#' @section Statistical Methods:
#' The analysis implements state-of-the-art methods for staging validation:
#' \itemize{
#'   \item \strong{NRI:} Quantifies net improvement in risk classification
#'   \item \strong{IDI:} Measures integrated discrimination improvement
#'   \item \strong{C-index:} Harrell's concordance with bootstrap confidence intervals
#'   \item \strong{DCA:} Clinical utility across decision thresholds
#'   \item \strong{Bootstrap:} Internal validation with bias correction
#' }
#'
#' @section Clinical Decision Framework:
#' Results include comprehensive guidance for staging system adoption:
#' \itemize{
#'   \item Statistical significance vs. clinical importance
#'   \item Effect size interpretation (small, medium, large improvements)
#'   \item Sample size adequacy assessment
#'   \item Recommendation confidence levels
#'   \item Implementation considerations
#' }
#'
#' @section Data Requirements:
#' \itemize{
#'   \item \strong{Sample Size:} Minimum 30 patients (100+ recommended)
#'   \item \strong{Follow-up:} Adequate survival time for meaningful analysis
#'   \item \strong{Staging:} Both old and new staging variables with 2+ levels
#'   \item \strong{Events:} Binary event indicator (0/1) or factor with specified level
#'   \item \strong{Data Quality:} Complete case analysis (missing values removed)
#' }
#'
#' @section Troubleshooting:
#' \itemize{
#'   \item \strong{"TRUE/FALSE error":} Check for missing values in staging or survival variables
#'   \item \strong{"Not atomic error":} Disable individual tables to isolate problematic components
#'   \item \strong{Model fitting errors:} Ensure adequate sample size and event rate (5-95%)
#'   \item \strong{Stage level errors:} Verify staging variables have multiple distinct levels
#' }
#'
#' @examples
#' \dontrun{
#' # Basic staging comparison
#' stagemigration(
#'     data = cancer_data,
#'     oldStage = "old_stage",
#'     newStage = "new_stage",
#'     survivalTime = "survival_months",
#'     event = "outcome",
#'     eventLevel = "DEAD",
#'     analysisType = "basic"
#' )
#'
#' # Comprehensive analysis with all options
#' stagemigration(
#'     data = lung_cancer_cohort,
#'     oldStage = "tnm7_stage",
#'     newStage = "tnm8_stage",
#'     survivalTime = "os_months",
#'     event = "death",
#'     eventLevel = "dead",
#'     analysisType = "comprehensive",
#'     calculateNRI = TRUE,
#'     performBootstrap = TRUE,
#'     bootstrapReps = 1000
#' )
#'
#' # PHASE 1 ENHANCED: Evidence-based Will Rogers assessment
#' stagemigration(
#'     data = pancreatic_cohort,
#'     oldStage = "T_AJCC8",
#'     newStage = "T_modified",
#'     survivalTime = "overall_survival_months",
#'     event = "death_status",
#'     eventLevel = "Dead",
#'     analysisType = "publication",
#'     advancedMigrationAnalysis = TRUE,
#'     showMigrationHeatmap = TRUE,
#'     cancerType = "other",
#'     showExplanations = TRUE
#' )
#'
#' # Phase 1 Enhanced with landmark analysis for lung cancer
#' stagemigration(
#'     data = lung_staging_data,
#'     oldStage = "stage_7th_edition",
#'     newStage = "stage_8th_edition",
#'     survivalTime = "survival_months",
#'     event = "vital_status",
#'     eventLevel = "deceased",
#'     analysisType = "comprehensive",
#'     advancedMigrationAnalysis = TRUE,
#'     cancerType = "lung", # Uses lung-specific landmark times: 3,6,12,24 months
#'     showWillRogersVisualization = TRUE,
#'     showMigrationSurvivalComparison = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link[survival]{concordance}} for C-index calculations,
#' \code{\link[survminer]{ggsurvplot}} for survival visualizations
#'
#' @keywords TNM staging, stage migration, staging validation, survival analysis
#' @concept staging systems
#' @concept prognostic models
#' @concept cancer staging
#' @concept pathology
#'
#' @return A comprehensive staging validation analysis with statistical comparisons,
#'         clinical interpretation, and advanced visualizations
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom survival Surv survfit coxph concordance survdiff
#' @importFrom survminer ggsurvplot
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_rug labs theme_minimal
#' @importFrom ggplot2 coord_fixed scale_linetype_manual
#' @importFrom dplyr mutate group_by summarize
#' @importFrom stats chisq.test fisher.test AIC BIC
#' @importFrom boot boot boot.ci
#' @importFrom pROC roc ci.auc
#' @importFrom timeROC timeROC
#' @importFrom dcurves dca
#' @importFrom mgcv gam s
#' @importFrom rms val.prob calibrate rcs
#' @importFrom Hmisc rcorr.cens

# ============================================================================
# TODO (security): C1 RCE remediation - DEDICATED SESSION REQUIRED (Step 2)
# ----------------------------------------------------------------------------
# WHAT: ~90 dynamic formula builders interpolate RAW user column names
#   (self$options$oldStage / newStage / survivalTime, and covariate names) into
#   as.formula(paste("Surv(", time_var, ",", event_var, ") ~", old_stage, ...))
#   which reach coxph()/survreg(). R's model.frame EVALUATES the formula RHS, so a
#   crafted column name (e.g. a column literally named
#   `stop(paste(collapse=', ', system('ls', intern=TRUE)))`) = arbitrary code
#   execution on the (shared cloud) jamovi worker. This is the highest-severity
#   finding in the module.
#
# DONE (this pass):
#   * Step 1 - central anchor in .validateData(): the returned analysis data now
#     carries fixed internal-safe column aliases  stage_old / stage_new /
#     time_internal  (alongside the pre-existing event_binary). Additive + guarded,
#     so no behavior change. THIS IS A PREREQUISITE, NOT THE FIX.
#   * Step 2 - migrated 1 of ~25 methods: .performTimeROCAnalysis (2 formula sites)
#     now uses  as.formula("Surv(time_internal, event_binary) ~ stage_old/stage_new").
#
# REMAINING (~88 formula sites across ~24 methods) - the dedicated session:
#   1. Enumerate the sites:
#        grep -nE 'as\.formula\(\s*paste' R/stagemigration.b.R
#      (the live ones interpolate time_var/time_col/survival_time + old_stage/
#      new_stage/old_col/new_col; many also use  factor(", old_col, ")  and
#      "+ covariate_formula").
#   2. For EACH site's enclosing method, verify BEFORE swapping:
#        (a) the method receives the .validateData() output (so the aliases exist
#            in its `data`); methods called with a re-validated copy or self$data
#            need the aliases added at their own prep point first;
#        (b) whether the method DISPLAYS Cox coefficient NAMES (coef()/summary(cox)/
#            rownames → tables). If yes, the RHS swap changes labels from
#            <oldStageColName><level> to "stage_old<level>" - keep the RAW option
#            name for the displayed label and only swap the FORMULA RHS.
#   3. Swap the formula string to the internal literals:
#        Surv(time_internal, event_binary) ~ stage_old   (and ~ stage_new, etc.)
#      Leave time_var/old_stage variables intact for data[[...]] access + display.
#   4. COVARIATES: covariate_formula (e.g. ~L13080 baseline/old_plus/new_plus
#      models) is built from raw covariate column names - wrap those with
#      jmvcore::composeTerms(), or add internal covariate aliases in .validateData
#      and reference them.
#   5. The 4 non-paste  as.formula(varname)  sites + the trend/quartile formulas
#      (~ stage_numeric / ~ survival_quartile, internal computed columns) are
#      already safe - confirm and skip.
#   6. After each method: Rscript -e 'parse("R/stagemigration.b.R")'  + spot-run.
#   Also: 1 na.omit (→ jmvcore::naOmit), 23 stop() (triage user-facing →
#   jmvcore::reject), 164 as.numeric (sample for as.numeric(factor) → toNumeric)
#   are lower-priority jamovify items deferred to the same session.
#
# TODO (security): D HTML/XSS - SECOND systematic surface (same dedicated session).
#   ~51 setContent + ~40 setNote sinks, 111 e$message interpolations, 0 htmlEscape file-wide.
#   Confirmed: ~L5519 setNote("models_compared", paste(... old_label ... new_label ...)) where
#   old_label/new_label are STAGE LABELS from the data (~L5447-5464); also e$message → error panels
#   (error_msg <- paste("Error in multi-state analysis:", e$message) ~L23221/L23812) → setContent.
#   A crafted stage factor label / column name / error string injects (setNote IS an HTML sink).
#   REMEDIATION: add a private helper .escHtml(x) = htmltools::htmlEscape(x), then per-sink:
#   (1) enumerate  grep -nE 'setContent|setNote' R/stagemigration.b.R  + the e$message paste sites;
#   (2) SKIP the literal *_explanation_html / methodology setContents (no interpolation);
#   (3) wrap every interpolated stage label / column name / factor value / e$message with .escHtml().
#   Table cells (addRow/setRow) are auto-escaped - only setContent/setNote HTML needs wrapping.
# ============================================================================

# Private methods after .run() live in R/stagemigration-part1.R ... part5.R; this class inherits
# them through the chain stagemigrationBase -> Part1 -> ... -> Part5 -> stagemigrationClass.
stagemigrationClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "stagemigrationClass",
        inherit = stagemigrationPart5,
        private = list(
            # ---- User-facing notices ----------------------------------------------------
            # Rendered into one Html item. jmvcore::Notice + results$insert() fails protobuf
            # serialization (Notice objects carry function references; see CLAUDE.md), so the
            # house pattern from R/waterfall.b.R is used instead. Notices accumulate during
            # .run() and are rendered by on.exit(), so they survive return() and stop().
            .noticeList = list(),

            .addNotice = function(type, title, content) {
                content <- paste(content, collapse = " ")
                content <- trimws(gsub("\\s{2,}", " ", gsub("[\r\n]+", " ", content)))
                # De-duplicate: helpers such as .safeExecute() run inside bootstrap and
                # per-stage loops, and one failure mode should yield one notice, not N.
                for (x in private$.noticeList) {
                    if (identical(x$type, type) && identical(x$title, title) && identical(x$content, content)) {
                        return(invisible(NULL))
                    }
                }
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type, title = title, content = content)
                invisible(NULL)
            },

            # p-value text for clinician-facing sentences: "p < 0.0001" / "p = 0.0123".
            # base::format.pval() returns "<1e-04", which read as "p = <1e-04" in running text.
            .pText = function(p) {
                if (length(p) != 1 || is.na(p) || !is.finite(p)) return("p not estimable")
                if (p < 1e-4) return("p < 0.0001")
                paste0("p = ", base::format(signif(p, 3), scientific = FALSE))
            },

            # timeROC returns one column per entry of `times` and prepends t = 0 when a single time is
            # requested, so the slot for time t must be looked up, never taken as [1].
            # Direction of stage migration per patient: +1 upstaged, -1 downstaged, 0 same label, NA when
            # undefined. Direction needs ONE ordered scale containing every observed label of both systems:
            # the original system's factor levels, else the new system's, else the sorted labels (numeric
            # when all labels are numbers). When neither scale holds all labels (e.g. a new edition adds
            # IIA/IIB), where a new-only label sits relative to the old ones cannot be read from the data,
            # so direction is NA. The former union-of-levels ranking put every new-only label above every
            # old label (II -> IIA and IV -> IIB both "upstaged"; disjoint label sets: everyone upstaged).
            .stageDirection = function(old, new) {
                order_labels <- function(x) {
                    if (is.factor(x)) return(levels(x))
                    u <- unique(as.character(x))
                    u <- u[!is.na(u)]
                    num <- suppressWarnings(as.numeric(u))
                    if (length(u) > 0 && !anyNA(num)) u[order(num)] else sort(u)
                }
                old_chr <- as.character(old)
                new_chr <- as.character(new)
                seen <- unique(c(old_chr, new_chr))
                seen <- seen[!is.na(seen)]
                scale <- NULL
                for (cand in list(order_labels(old), order_labels(new))) {
                    if (all(seen %in% cand)) {
                        scale <- cand
                        break
                    }
                }
                both <- !is.na(old_chr) & !is.na(new_chr)
                direction <- rep(NA_integer_, length(old_chr))
                if (!is.null(scale)) {
                    direction[both] <- as.integer(sign(match(new_chr[both], scale) - match(old_chr[both], scale)))
                } else {
                    direction[both & old_chr == new_chr] <- 0L
                }
                list(
                    direction = direction,
                    comparable = !is.null(scale),
                    scale = scale,
                    only_old = setdiff(unique(old_chr[!is.na(old_chr)]), new_chr),
                    only_new = setdiff(unique(new_chr[!is.na(new_chr)]), old_chr)
                )
            },
            # timeROC calls Surv() unqualified and neither imports nor depends on survival, so outside an
            # R session with survival attached (jamovi) every call errored and ROC silently fell back
            # to pROC. Attach survival for the duration of the call only.
            .timeROC = function(...) {
                withr::with_package("survival", timeROC::timeROC(...), quietly = TRUE)
            },
            .timeROCIndex = function(roc, t) {
                k <- which(abs(roc$times - t) < 1e-8)
                if (length(k) > 0) k[length(k)] else length(roc$times)
            },
            .seedValue = function() {
                s <- tryCatch(self$options$seed, error = function(e) NULL)
                if (is.null(s) || length(s) != 1 || is.na(s)) 42L else as.integer(s)
            },
            .renderNotices = function() {
                tryCatch(
                    {
                        item <- self$results$notices
                        if (is.null(item)) return(invisible(NULL))
                        if (length(private$.noticeList) == 0) {
                            item$setContent("")
                            return(invisible(NULL))
                        }
                        rank <- c(ERROR = 1, STRONG_WARNING = 2, WARNING = 3, INFO = 4)
                        ord <- order(vapply(private$.noticeList, function(x) {
                            r <- unname(rank[x$type]); if (is.na(r)) 5 else r
                        }, numeric(1)))
                        styles <- list(
                            ERROR = list(color = "#dc2626", bg = "rgba(220, 38, 38, 0.10)", border = "#fca5a5"),
                            STRONG_WARNING = list(color = "#ea580c", bg = "rgba(234, 88, 12, 0.10)", border = "#fdba74"),
                            WARNING = list(color = "#ca8a04", bg = "rgba(202, 138, 4, 0.12)", border = "#fde047"),
                            INFO = list(color = "#2563eb", bg = "rgba(37, 99, 235, 0.08)", border = "#93c5fd")
                        )
                        html <- "<div style='margin: 10px 0;'>"
                        for (x in private$.noticeList[ord]) {
                            st <- styles[[x$type]] %||% styles$INFO
                            html <- paste0(html,
                                "<div style='background-color: ", st$bg, "; border-left: 4px solid ", st$border,
                                "; padding: 12px; margin: 8px 0; border-radius: 4px; color: inherit;'>",
                                "<strong style='color: ", st$color, ";'>", jmvcore::htmlEscape(x$title), "</strong><br>",
                                "<span style='color: inherit;'>", jmvcore::htmlEscape(x$content), "</span></div>")
                        }
                        item$setContent(paste0(html, "</div>"))
                    },
                    error = function(e) invisible(NULL)
                )
            },

            # TODO: Architecture - This file is 29,000+ lines. Consider splitting into:
            #   R/stagemigration-helpers-rf.R - Random Forest methods (~800 lines)
            #   R/stagemigration-helpers-calibration.R - Calibration methods (~600 lines)
            #   R/stagemigration-helpers-competing.R - Competing risks methods (~500 lines)
            #   R/stagemigration-helpers-advanced.R - Win Ratio, Frailty, SHAP, etc.
            #   Use source() or define additional R6 methods in separate files.
            #   Priority: LOW (functional as-is, but maintainability concern).

            # Escape variable names for safe handling
            # survdiff degrees of freedom: survival:::print.survdiff uses the number of
            # groups with EXPECTED > 0, not length(n). $n is table(groups) and keeps factor
            # levels that survive subsetting with zero events, which inflates df and makes
            # the p-value too large (hiding real stage separation). lower.tail = FALSE also
            # preserves precision instead of flooring 1 - pchisq() at 0.
            .survdiffP = function(sd_obj) {
                if (is.null(sd_obj) || is.null(sd_obj$chisq)) return(NA_real_)
                etmp <- if (is.matrix(sd_obj$exp)) rowSums(sd_obj$exp) else sd_obj$exp
                df <- sum(etmp > 0) - 1
                if (!is.finite(df) || df < 1) return(NA_real_)
                stats::pchisq(sd_obj$chisq, df = df, lower.tail = FALSE)
            },

            .escapeVar = function(x) {
                # Handle variables with spaces/special characters
                gsub("[^A-Za-z0-9_]+", "_", make.names(x))
            },

            # TODO: Variable Name Safety - .escapeVar is defined but never used.
            # 92 formula constructions use raw paste("Surv(", var, ")") without escaping.
            # Variables with spaces/special chars will break. Priority: MEDIUM.
            # Fix: Replace all paste-based Surv() formula construction with:
            #   safe_var <- private$.escapeVar(var_name)
            #   fml <- as.formula(paste0("Surv(", safe_var, ", ", event_var, ") ~ ", stage_var))
            # Affects ~92 locations. Consider a .buildSurvFormula() helper.

            # Return lower and upper quantile probabilities from confidenceLevel
            .ciProbs = function() {
                cl <- self$options$confidenceLevel
                alpha <- 1 - cl
                c(alpha / 2, 1 - alpha / 2)
            },

            # Two-sided normal critical value at the user's confidenceLevel.
            # Every interval in this analysis previously hardcoded 1.96, so a user who
            # selected 90% or 99% still got a 95% interval -- next to a label that said
            # otherwise.
            # Absolute risk at t from a fitted Cox model: 1 - S0(t)^exp(lp).
            #
            # plogis(lp) / exp(lp)/(1+exp(lp)) was previously used as a stand-in for risk.
            # A Cox linear predictor is a CENTRED log-hazard-ratio, not a logit, so plogis()
            # is an arbitrary monotone rescaling onto [0,1] with no survival-probability
            # meaning -- in an average-risk cohort every subject maps to about 0.5, which
            # compresses the discrimination slope and makes IDI-type statistics built on it
            # numerically meaningless rather than merely biased.
            .coxRisk = function(model, time_point, newdata = NULL) {
                tryCatch(
                    {
                        sf <- survival::survfit(model)
                        idx <- findInterval(time_point, sf$time)
                        s0 <- if (idx == 0) 1 else sf$surv[idx]
                        lp <- if (is.null(newdata)) {
                            stats::predict(model, type = "lp")
                        } else {
                            stats::predict(model, newdata = newdata, type = "lp")
                        }
                        1 - s0^exp(lp)
                    },
                    error = function(e) rep(NA_real_, if (is.null(newdata)) model$n else nrow(newdata))
                )
            },

            # Evaluation horizon for IDI-style statistics: the first user-supplied NRI
            # time point that lies inside observed follow-up, else the median follow-up.
            # Cumulative incidence of the primary and competing events at `horizon`,
            # estimated by Aalen-Johansen via cmprsk::cuminc.
            #
            # Previously these were crude proportions n_primary / n_total: no time axis, no
            # censoring adjustment, biased downward by censoring, and not comparable across
            # stages with different follow-up -- the exact bias the CIF exists to remove.
            # (1 - KM would be biased the other way, upward, in the presence of competing
            # events; neither is acceptable.)
            # Restricted mean survival time for one group, with the Uno/Tian variance.
            # Verified to match survRM2::rmst2 to 4 decimal places.
            #
            # Replaces a hand-rolled version that (a) integrated the KM STEP function with the
            # trapezoidal rule, (b) had no variance, SE, CI or p-value anywhere, and (c) when a
            # group's follow-up ended before tau, appended a flat segment asserting zero hazard
            # over a period with no data and integrated it -- inflating RMST by >20% in a
            # 60-patient check (14.78 reported vs 11.99 true).
            # Event status at a fixed horizon, with censoring handled honestly.
            #
            # Returns 1 (event by t), 0 (event-free through t) or NA (censored BEFORE t, so
            # status at t is unknown). The previous
            #     ifelse(time <= t & event == 1, 1, 0)
            # sent early dropouts to 0 alongside genuine survivors, asserting that anyone lost
            # to follow-up at month 3 was event-free at month 60. In a typical oncology cohort
            # that is 20-40% of the "non-event" group, and the bias is directional: it inflates
            # the non-event component of NRI toward whichever staging system down-classifies
            # more. Excluding them is the complete-case estimator that the sibling helper
            # stagemigration_calculateNRI already uses; a full IPCW estimator (Uno 2013) would
            # additionally recover their information.
            .eventStatusAtTime = function(time, event, time_point) {
                time <- suppressWarnings(as.numeric(time))
                event <- suppressWarnings(as.numeric(event))
                out <- rep(NA_real_, length(time))
                out[!is.na(time) & !is.na(event) & time <= time_point & event == 1] <- 1
                out[!is.na(time) & time > time_point] <- 0
                out
            },

            .rmstOneGroup = function(time, status, tau) {
                out <- list(rmst = NA_real_, se = NA_real_, max_time = NA_real_)
                tryCatch(
                    {
                        ok <- is.finite(time) & is.finite(status)
                        time <- time[ok]; status <- status[ok]
                        if (length(time) < 3) return(out)

                        out$max_time <- max(time, na.rm = TRUE)
                        # No extrapolation: tau beyond the last observation is not estimable.
                        if (!is.finite(tau) || tau <= 0 || tau > out$max_time) return(out)

                        fit <- survival::survfit(survival::Surv(time, status) ~ 1)
                        k <- fit$time <= tau
                        tt <- fit$time[k]; ss <- fit$surv[k]
                        dd <- fit$n.event[k]; nn <- fit$n.risk[k]

                        starts <- c(0, tt)
                        heights <- c(1, ss)
                        ends <- c(tt, tau)
                        areas <- (ends - starts) * heights

                        out$rmst <- sum(areas)

                        # A_j = area under S from event time j to tau
                        A <- rev(cumsum(rev(areas)))[-1]
                        keep <- dd > 0 & nn > dd
                        if (any(keep)) {
                            v <- sum((A[keep])^2 * dd[keep] / (nn[keep] * (nn[keep] - dd[keep])))
                            out$se <- if (is.finite(v) && v >= 0) sqrt(v) else NA_real_
                        }
                        out
                    },
                    error = function(e) out
                )
            },

            # Gray's (1988) k-sample test comparing cumulative incidence functions of the
            # primary event across stages. cmprsk::cuminc computes it directly; it is the
            # competing-risks analogue of the log-rank test and does not assume a model.
            .grayTest = function(data, stage_var, time_var) {
                out <- list(statistic = NA_real_, p_value = NA_real_, df = NA_integer_)
                tryCatch(
                    {
                        ft <- as.numeric(data[[time_var]])
                        fs <- ifelse(data$primary_event == 1, 1L,
                              ifelse(data$competing_event == 1, 2L, 0L))
                        grp <- as.character(data[[stage_var]])
                        ok <- is.finite(ft) & !is.na(fs) & !is.na(grp)
                        ft <- ft[ok]; fs <- fs[ok]; grp <- grp[ok]
                        if (length(unique(grp)) < 2 || !any(fs == 1)) return(out)

                        ci <- cmprsk::cuminc(ftime = ft, fstatus = fs, group = grp, cencode = 0)
                        tst <- ci$Tests
                        if (is.null(tst)) return(out)
                        # row "1" is the primary event
                        rn <- rownames(tst)
                        idx <- if (!is.null(rn) && "1" %in% rn) which(rn == "1")[1] else 1
                        out$statistic <- unname(tst[idx, "stat"])
                        out$p_value <- unname(tst[idx, "pv"])
                        out$df <- as.integer(unname(tst[idx, "df"]))
                        out
                    },
                    error = function(e) out
                )
            },

            .stageCIF = function(stage_data, time_var, horizon) {
                out <- list(primary = NA_real_, competing = NA_real_,
                            se_primary = NA_real_, se_competing = NA_real_)
                tryCatch(
                    {
                        ft <- as.numeric(stage_data[[time_var]])
                        fs <- ifelse(stage_data$primary_event == 1, 1L,
                              ifelse(stage_data$competing_event == 1, 2L, 0L))
                        ok <- is.finite(ft) & !is.na(fs)
                        ft <- ft[ok]; fs <- fs[ok]
                        if (length(ft) < 5 || !any(fs > 0)) return(out)

                        ci <- cmprsk::cuminc(ftime = ft, fstatus = fs, cencode = 0)
                        tp <- cmprsk::timepoints(ci, times = horizon)
                        rn <- rownames(tp$est)

                        pick <- function(cause) {
                            hit <- grep(paste0(" ", cause, "$"), rn)
                            if (length(hit) == 0) return(c(NA_real_, NA_real_))
                            c(tp$est[hit[1], 1], tp$var[hit[1], 1])
                        }
                        pv <- pick(1); cv <- pick(2)
                        out$primary <- pv[1]
                        out$se_primary <- if (is.na(pv[2])) NA_real_ else sqrt(pv[2])
                        out$competing <- cv[1]
                        out$se_competing <- if (is.na(cv[2])) NA_real_ else sqrt(cv[2])
                        out
                    },
                    error = function(e) out
                )
            },

            .idiTimePoint = function(data) {
                tv <- self$options$survivalTime
                times <- suppressWarnings(as.numeric(data[[tv]]))
                maxt <- suppressWarnings(max(times, na.rm = TRUE))
                tp_str <- self$options$nriTimePoints
                cand <- if (!is.null(tp_str) && nzchar(tp_str)) {
                    suppressWarnings(as.numeric(unlist(strsplit(tp_str, "\\s*,\\s*"))))
                } else {
                    numeric(0)
                }
                cand <- cand[is.finite(cand) & cand > 0 & cand <= maxt]
                if (length(cand)) cand[1] else stats::median(times, na.rm = TRUE)
            },

            # Isolate one metric's failure from the other eight.
            #
            # .calculateEnhancedReclassificationMetrics wraps nine independent estimators in a
            # SINGLE tryCatch, so one bad metric collapsed the whole table into one "Error" row
            # and silently suppressed the eight that computed fine.
            # Log-rank p comparing two patient subsets on the analysis time/event columns.
            # Used to put a TEST behind the Will Rogers verdict, which previously rested on
            # three bare point-estimate median comparisons with no inference at all.
            .logrankTwoGroups = function(g1, g2, time_col, event_col) {
                tryCatch(
                    {
                        if (nrow(g1) < 3 || nrow(g2) < 3) return(NA_real_)

                        # Prefer the validated event_binary column. Falling through to
                        # as.numeric() on a raw factor event variable would return level
                        # INDICES (1, 2), which Surv() reads as 1 = censored / 2 = event.
                        ecol <- if ("event_binary" %in% names(g1) && "event_binary" %in% names(g2)) {
                            "event_binary"
                        } else {
                            event_col
                        }
                        coerce_event <- function(x) {
                            if (is.factor(x)) {
                                lev <- self$options$eventLevel
                                if (!is.null(lev) && nzchar(lev)) {
                                    as.numeric(as.character(x) == lev)
                                } else {
                                    suppressWarnings(as.numeric(as.character(x)))
                                }
                            } else {
                                as.numeric(x)
                            }
                        }

                        wd <- rbind(
                            data.frame(.t = as.numeric(g1[[time_col]]),
                                       .e = coerce_event(g1[[ecol]]), .g = "A"),
                            data.frame(.t = as.numeric(g2[[time_col]]),
                                       .e = coerce_event(g2[[ecol]]), .g = "B")
                        )
                        wd <- wd[stats::complete.cases(wd), , drop = FALSE]
                        if (length(unique(wd$.g)) < 2) return(NA_real_)
                        sd_obj <- survival::survdiff(survival::Surv(.t, .e) ~ .g, data = wd)
                        private$.survdiffP(sd_obj)
                    },
                    error = function(e) NA_real_
                )
            },

            .safeMetric = function(expr) {
                tryCatch(expr, error = function(e) NULL, warning = function(w) suppressWarnings(expr))
            },

            .zCrit = function() {
                cl <- self$options$confidenceLevel
                if (is.null(cl) || !is.finite(cl) || cl <= 0 || cl >= 1) cl <- 0.95
                stats::qnorm(1 - (1 - cl) / 2)
            },
            .init = function() {
                # If core variables are not selected, show a welcome message
                if (is.null(self$options$oldStage) || is.null(self$options$newStage) ||
                    is.null(self$options$survivalTime) || is.null(self$options$event)) {
                    self$results$welcomeMessage$setContent(private$.generateWelcomeMessage())
                }

                # Set dynamic plot sizes based on plot type
                if (self$options$showSurvivalCurves) {
                    plot_type <- self$options$survivalPlotType

                    # Adjust size based on plot type and options
                    if (plot_type == "separate") {
                        # Vertical stacking needs more height
                        height <- if (!is.null(self$options$showRiskTables) && self$options$showRiskTables) 1200 else 1000
                        self$results$survivalCurves$setSize(900, height)
                    } else if (plot_type == "sidebyside") {
                        # Horizontal layout needs more width
                        height <- if (!is.null(self$options$showRiskTables) && self$options$showRiskTables) 700 else 600
                        self$results$survivalCurves$setSize(1200, height)
                    } else if (plot_type == "overlay") {
                        # Standard size for single overlay plot
                        self$results$survivalCurves$setSize(900, 700)
                    }
                }
            },

            # Helper function to safely convert values to atomic types
            .safeAtomic = function(value, type = "numeric", default = NA) {
                # Safely convert a value to atomic type with fallback
                tryCatch(
                    {
                        if (is.null(value) || length(value) == 0) {
                            return(default)
                        }

                        # Convert based on type
                        if (type == "numeric") {
                            result <- as.numeric(value)[1]
                            return(if (is.finite(result)) result else default)
                        } else if (type == "integer") {
                            result <- as.integer(value)[1]
                            return(if (is.finite(result)) result else as.integer(default))
                        } else if (type == "character") {
                            result <- as.character(value)[1]
                            return(if (is.na(result)) as.character(default) else result)
                        } else if (type == "logical") {
                            result <- as.logical(value)[1]
                            return(if (is.na(result)) as.logical(default) else result)
                        } else {
                            return(default)
                        }
                    },
                    error = function(e) {
                        return(default)
                    }
                )
            },

            # Standardized error handling wrapper
            .safeExecute = function(expr,
                                    errorReturn = NULL,
                                    errorMessage = "Operation failed",
                                    warningMessage = NULL,
                                    silent = FALSE) {
                # Standardized error handling for consistent user experience
                # expr: Expression to execute
                # errorReturn: Value to return on error (default NULL)
                # errorMessage: User-friendly error message
                # warningMessage: Optional warning to show on error
                # silent: If TRUE, suppress error messages

                result <- tryCatch(
                    {
                        expr
                    },
                    error = function(e) {
                        if (!silent) {
                            # Log detailed error for debugging

                            # Show user-friendly warning if specified
                            # This parameter exists to reach the user; warning() only surfaced it
                            # in jamovi's undifferentiated Analysis Notes panel.
                            if (!is.null(warningMessage)) {
                                private$.addNotice("WARNING", .("Analysis step could not be completed"), warningMessage)
                            }
                        }
                        return(errorReturn)
                    },
                    warning = function(w) {
                        # Capture warnings but let execution continue
                        if (!silent) {
                        }
                        # Re-evaluate the expression suppressing the warning
                        suppressWarnings(expr)
                    }
                )

                return(result)
            },

            # Helper function to get bootstrap repetitions consistently
            .getBootstrapReps = function(maxReps = NULL) {
                # Get bootstrap repetitions from options with optional maximum limit
                # maxReps: Optional maximum number of repetitions for efficiency

                baseReps <- self$options$bootstrapReps

                # Validate base repetitions
                if (is.null(baseReps) || !is.numeric(baseReps) || baseReps < 1) {
                    baseReps <- 1000
                }

                # Apply maximum limit if specified
                if (!is.null(maxReps) && is.numeric(maxReps) && maxReps > 0) {
                    return(min(baseReps, maxReps))
                }

                return(baseReps)
            },

            # TODO: i18n - All user-visible strings are hardcoded English.
            # When internationalization is implemented:
            #   1. Wrap all setContent/setNote/addRow text strings with .()
            #   2. Create jamovi/i18n/en.po and jamovi/i18n/tr.po catalogs
            #   3. Use .("text with {placeholder}") + .fmt() for dynamic messages
            #   4. Turkish medical terms: C-indeksi, Tehlike Orani, Guven Araligi
            #   Priority: LOW (depends on module-wide i18n effort).

            .setExplanationContent = function(resultName, htmlContent) {
                # Centralized explanation content management
                # Only set content if showExplanations is enabled to optimize memory usage
                if (isTRUE(self$options$showExplanations)) {
                    self$results[[resultName]]$setContent(htmlContent)
                }
            },

            # NEW MODULAR HELPER FUNCTIONS FOR ENHANCED FUNCTIONALITY

            .generateGuidedModeProgress = function(current_step = 1, total_steps = 5) {
                # Generate guided mode progress indicator
                if (!self$options$enableGuidedMode) {
                    return(NULL)
                }

                steps <- c(
                    "1. Variable Selection",
                    "2. Analysis Configuration",
                    "3. Statistical Validation",
                    "4. Clinical Interpretation",
                    "5. Results Review"
                )

                progress_html <- paste0(
                    '<div style="background: linear-gradient(135deg, rgba(55, 95, 155, 0.05) 0%, rgba(33, 77, 148, 0.27) 100%); color: inherit; padding: 20px; border-radius: 10px; margin: 10px 0;">',
                    '<h3 style="color: inherit; margin-top: 0;">Analysis Progress</h3>',
                    '<div style="display: flex; justify-content: space-between; align-items: center; margin: 15px 0;">'
                )

                for (i in seq_along(steps)) {
                    status_color <- if (i <= current_step) "#27ae60" else if (i == current_step + 1) "#f39c12" else "#bdc3c7"
                    status_icon <- if (i < current_step) "[DONE]" else if (i == current_step) "[CURRENT]" else "[PENDING]"

                    progress_html <- paste0(
                        progress_html,
                        '<div style="text-align: center; flex: 1;">',
                        '<div style="width: 40px; height: 40px; border-radius: 50%; background: ', status_color, '; color: white; display: flex; align-items: center; justify-content: center; margin: 0 auto 10px; font-size: 18px;">',
                        status_icon,
                        "</div>",
                        '<div style="font-size: 12px; color: inherit; max-width: 100px; margin: 0 auto;">', steps[i], "</div>",
                        "</div>"
                    )

                    if (i < length(steps)) {
                        progress_html <- paste0(
                            progress_html,
                            '<div style="flex: 0 0 30px; height: 2px; background: ', if (i < current_step) "#27ae60" else "#bdc3c7", '; margin: 20px 0;"></div>'
                        )
                    }
                }

                progress_html <- paste0(progress_html, "</div></div>")
                return(progress_html)
            },
            .generateCopyReadyReport = function(results) {
                # Generate copy-ready clinical summary for reports and manuscripts
                if (!self$options$generateCopyReadyReport || is.null(results)) {
                    return(NULL)
                }

                lang <- self$options$preferredLanguage %||% "en"

                # Localized text templates
                text_templates <- list(
                    en = list(
                        title = "Clinical Summary: TNM Stage Migration Analysis",
                        methods_header = "Methods",
                        results_header = "Key Findings",
                        interpretation_header = "Clinical Interpretation",
                        recommendation_header = "Recommendation",
                        patients_analyzed = "patients were analyzed",
                        migration_rate = "migration rate",
                        statistical_significance = "statistical significance",
                        clinical_significance = "clinical significance"
                    ),
                    tr = list(
                        title = "Klinik \u{00D6}zet: TNM Evre Migrasyonu Analizi",
                        methods_header = "Y\u{00F6}ntem",
                        results_header = "Temel Bulgular",
                        interpretation_header = "Klinik De\u{011F}erlendirme",
                        recommendation_header = "\u{00D6}neri",
                        patients_analyzed = "hasta analiz edildi",
                        migration_rate = "migrasyon oran\u{0131}",
                        statistical_significance = "istatistiksel anlaml\u{0131}l\u{0131}k",
                        clinical_significance = "klinik anlaml\u{0131}l\u{0131}k"
                    )
                )

                t <- text_templates[[lang]] %||% text_templates[["en"]] # Fallback to English

                # Extract key statistics for copy-ready text
                total_patients <- if (!is.null(results$migration_overview)) {
                    results$migration_overview$total_patients %||% "N/A"
                } else {
                    "N/A"
                }

                migration_rate <- if (!is.null(results$migration_summary)) {
                    paste0(results$migration_summary$migration_percentage %||% "N/A", "%")
                } else {
                    "N/A"
                }

                # Generate copy-ready paragraphs
                copy_ready_html <- paste0(
                    '<div style="background-color: rgba(138, 155, 172, 0.06); border-left: 5px solid #007bff; padding: 20px; margin: 20px 0; border-radius: 0 10px 10px 0; color: inherit;">',
                    '<h3 style="color: #007bff; margin-top: 0;"> ', t$title, "</h3>",
                    '<div style="background: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 8px; margin: 15px 0;">',
                    '<h4 style="color: #28a745; margin-top: 0;">', t$methods_header, "</h4>",
                    '<p style="line-height: 1.6; color: inherit;">',
                    "We performed a comprehensive TNM stage migration analysis comparing the original and revised staging systems. ",
                    "Statistical validation included migration matrix analysis, concordance assessment, and bootstrap validation. ",
                    # Name only the advanced methods that were actually enabled for this run.
                    # (This used to key off clinicalPreset, which configured nothing, so the
                    # sentence could claim analyses that never ran.)
                    local({
                        adv <- c(
                            if (isTRUE(self$options$performROCAnalysis)) "time-dependent ROC analysis",
                            if (isTRUE(self$options$performDCA)) "decision curve analysis",
                            if (isTRUE(self$options$advancedMigrationAnalysis)) "Will Rogers phenomenon evaluation"
                        )
                        if (length(adv) == 0) "" else paste0("Advanced methods included ", paste(adv, collapse = ", "), ". ")
                    }),
                    "</p>",
                    "</div>",
                    '<div style="background: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 8px; margin: 15px 0;">',
                    '<h4 style="color: #28a745; margin-top: 0;">', t$results_header, "</h4>",
                    '<p style="line-height: 1.6; color: inherit;">',
                    "A total of <strong>", total_patients, " ", t$patients_analyzed, "</strong>. ",
                    "The overall ", t$migration_rate, " was <strong>", migration_rate, "</strong>. ",
                    if (!is.null(results$statistical_comparison)) {
                        paste0(
                            "C-index improved from ", results$statistical_comparison$c_index_old %||% "N/A",
                            " to ", results$statistical_comparison$c_index_new %||% "N/A", ". "
                        )
                    } else {
                        ""
                    },
                    "</p>",
                    "</div>",
                    '<div style="background: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 8px; margin: 15px 0;">',
                    '<h4 style="color: #28a745; margin-top: 0;">', t$interpretation_header, "</h4>",
                    '<p style="line-height: 1.6; color: inherit;">',
                    "The revised staging system demonstrated ",
                    if (!is.null(results$clinical_interpretation)) "improved prognostic discrimination " else "staging validation ",
                    "compared to the original system. ",
                    "These findings support the clinical implementation of the revised staging criteria.",
                    "</p>",
                    "</div>",
                    '<div style="background: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 8px; margin: 15px 0;">',
                    '<h4 style="color: #dc3545; margin-top: 0;">', t$recommendation_header, "</h4>",
                    '<p style="line-height: 1.6; color: inherit; font-weight: 600;">',
                    "Based on the statistical validation and clinical assessment, we recommend ",
                    if (!is.null(results$clinical_interpretation) && grepl("recommend.*implementation", results$clinical_interpretation$recommendation %||% "", ignore.case = TRUE)) {
                        "implementation of the revised staging system in clinical practice."
                    } else {
                        "careful consideration of the revised staging system with additional validation if needed."
                    },
                    "</p>",
                    "</div>",
                    '<div style="text-align: center; margin-top: 20px; padding: 10px; background-color: rgba(33, 152, 239, 0.13); border-radius: 5px; color: inherit;">',
                    '<small style="color: inherit;">',
                    " <strong>Usage Note:</strong> This summary can be copied directly into clinical reports or adapted for manuscript preparation. ",
                    "Statistical details and complete methodology are available in the detailed analysis results above.",
                    "</small>",
                    "</div>",
                    "</div>"
                )

                return(copy_ready_html)
            },
            .optimizeMemoryUsage = function(data) {
                # Optimize memory usage for large datasets
                if (!self$options$optimizeForLargeDatasets) {
                    return(data)
                }

                # Check if dataset qualifies for optimization
                if (nrow(data) < 10000) {
                    return(data)
                }

                # Apply memory optimizations
                optimized_data <- data

                # Convert character columns to factors where appropriate
                char_cols <- sapply(optimized_data, is.character)
                for (col in names(char_cols)[char_cols]) {
                    if (length(unique(optimized_data[[col]])) < nrow(optimized_data) * 0.1) {
                        optimized_data[[col]] <- as.factor(optimized_data[[col]])
                    }
                }

                # Optimize numeric precision where possible
                numeric_cols <- sapply(optimized_data, is.numeric)
                for (col in names(numeric_cols)[numeric_cols]) {
                    # Check if values are integers (with safety checks)
                    if (length(optimized_data[[col]]) > 0 &&
                        all(is.finite(optimized_data[[col]]), na.rm = TRUE) &&
                        all(optimized_data[[col]] == round(optimized_data[[col]]), na.rm = TRUE)) {
                        max_val <- max(abs(optimized_data[[col]]), na.rm = TRUE)
                        if (is.finite(max_val) && max_val < 2^15) {
                            optimized_data[[col]] <- as.integer(optimized_data[[col]])
                        }
                    }
                }

                return(optimized_data)
            },
            .showProgressIndicator = function(message, step = 1, total = 5) {
                # Show progress indicators for long-running analyses
                if (!(self$options$enableProgressIndicators %||% TRUE)) {
                    return(NULL)
                }

                # Safely handle progress calculation
                if (!is.numeric(step) || !is.numeric(total) || total <= 0) {
                    return(NULL)
                }


                # Update guided mode progress if enabled
                if (self$options$enableGuidedMode %||% FALSE) {
                    tryCatch(
                        {
                            progress_html <- private$.generateGuidedModeProgress(step, total)
                            if (!is.null(progress_html)) {
                                self$results$guidedModeProgress$setContent(progress_html)
                            }
                        },
                        error = function(e) {
                            # Silently handle guided mode errors to not break analysis
                        }
                    )
                }

                # Progress message with better formatting
                if (!is.null(message) && nchar(message) > 0) {
                }
            },
            .validateVisibilityLogic = function() {
                # Validate consistency between visibility conditions and actual content generation
                # This function ensures that results are only generated when they will be visible

                visibility_issues <- list()

                # Check for common visibility conflicts
                visibility_rules <- list(
                    # Tables that should be visible when their primary option is enabled
                    "homogeneityTests" = "performHomogeneityTests",
                    "trendTests" = "performTrendTests",
                    "nriResults" = "calculateNRI",
                    "idiResults" = "calculateIDI",
                    "bootstrapResults" = "performBootstrap",
                    "rocAnalysis" = "performROCAnalysis",
                    "dcaResults" = "performDCA",
                    "calibrationAnalysis" = "performCalibration",
                    "pseudoR2Results" = "calculatePseudoR2",
                    "likelihoodTests" = "performLikelihoodTests",
                    "clinicalInterpretation" = "showClinicalInterpretation",
                    "executiveSummary" = "generateExecutiveSummary",
                    "statisticalSummary" = "showStatisticalSummary",
                    "effectSizes" = "includeEffectSizes",
                    "monotonicityCheck" = "advancedMigrationAnalysis",
                    "willRogersAnalysis" = "advancedMigrationAnalysis",
                    "stageSpecificCIndex" = "advancedMigrationAnalysis",
                    "enhancedPseudoR2" = "advancedMigrationAnalysis",

                    # Multifactorial analysis results
                    "multifactorialResults" = "enableMultifactorialAnalysis",
                    "adjustedCIndexComparison" = "enableMultifactorialAnalysis",
                    "nestedModelTests" = "enableMultifactorialAnalysis",
                    "stepwiseResults" = "enableMultifactorialAnalysis",
                    "interactionTests" = "enableMultifactorialAnalysis",
                    "stratifiedAnalysis" = "enableMultifactorialAnalysis",

                    # Visualization elements
                    "migrationHeatmap" = "showMigrationHeatmap",
                    "rocComparisonPlot" = "showROCComparison",
                    "forestPlot" = "showForestPlot",
                    "calibrationPlots" = "showCalibrationPlots",
                    "decisionCurves" = "showDecisionCurves",
                    "survivalCurves" = "showSurvivalCurves"
                )

                # Check each visibility rule
                for (result_name in names(visibility_rules)) {
                    option_name <- visibility_rules[[result_name]]

                    # Check if option is enabled
                    if (!is.null(self$options[[option_name]]) &&
                        isTRUE(self$options[[option_name]])) {
                        # This result should be generated - no issue
                        next
                    }

                    # Check if result exists despite option being disabled
                    if (exists(result_name, envir = self$results)) {
                        visibility_issues[[result_name]] <- paste(
                            "Result", result_name, "may be generated despite option",
                            option_name, "being disabled"
                        )
                    }
                }

                # Return any visibility issues found
                return(visibility_issues)
            },

            # Option dependency validation system
            .validateOptionDependencies = function() {
                # Validate that option dependencies are properly satisfied
                # Returns list with validation results and warnings

                issues <- list()
                warnings <- list()

                # Define dependency rules
                dependencies <- list(
                    # DCA depends on Cox models being fittable (requires basic survival data)
                    "performDCA" = list(
                        requires = c("oldStage", "newStage", "survivalTime", "event"),
                        analysis_type = "comprehensive",
                        message = "Decision Curve Analysis requires Cox models to be fitted first"
                    ),

                    # NRI depends on survival analysis capability
                    "calculateNRI" = list(
                        requires = c("oldStage", "newStage", "survivalTime", "event"),
                        analysis_type = "standard",
                        message = "Net Reclassification Improvement requires survival analysis"
                    ),

                    # IDI depends on Cox models and discrimination analysis
                    "calculateIDI" = list(
                        requires = c("oldStage", "newStage", "survivalTime", "event"),
                        analysis_type = "standard",
                        message = "Integrated Discrimination Improvement requires Cox models"
                    ),

                    # ROC Analysis depends on Cox models
                    "performROCAnalysis" = list(
                        requires = c("oldStage", "newStage", "survivalTime", "event"),
                        analysis_type = "standard",
                        message = "Time-dependent ROC Analysis requires Cox models"
                    ),

                    # Calibration depends on Cox models being fitted
                    "performCalibration" = list(
                        requires = c("oldStage", "newStage", "survivalTime", "event"),
                        analysis_type = "any",
                        message = "Calibration Analysis requires Cox models to be fitted"
                    ),

                    # Bootstrap validation depends on basic analysis capability
                    "performBootstrap" = list(
                        requires = c("oldStage", "newStage", "survivalTime", "event"),
                        analysis_type = "comprehensive",
                        message = "Bootstrap validation requires basic survival analysis"
                    ),

                    # Homogeneity tests need staging variables
                    "performHomogeneityTests" = list(
                        requires = c("oldStage", "newStage", "survivalTime", "event"),
                        analysis_type = "any",
                        message = "Homogeneity tests require staging and survival variables"
                    ),

                    # Pseudo R-squared depends on Cox models
                    "calculatePseudoR2" = list(
                        requires = c("oldStage", "newStage", "survivalTime", "event"),
                        analysis_type = "any",
                        message = "Pseudo R-squared calculation requires Cox models"
                    )
                )

                # Check each dependency
                for (option_name in names(dependencies)) {
                    option_enabled <- self$options[[option_name]]

                    if (!is.null(option_enabled) && isTRUE(option_enabled)) {
                        dep_rule <- dependencies[[option_name]]

                        # Check required options
                        missing_reqs <- character(0)
                        for (req in dep_rule$requires) {
                            if (is.null(self$options[[req]]) ||
                                (is.character(self$options[[req]]) && self$options[[req]] == "")) {
                                missing_reqs <- c(missing_reqs, req)
                            }
                        }

                        if (length(missing_reqs) > 0) {
                            issues[[option_name]] <- list(
                                option = option_name,
                                missing = missing_reqs,
                                message = paste(dep_rule$message, "- Missing:", paste(missing_reqs, collapse = ", "))
                            )
                        }

                        # Check analysis type requirements
                        if (dep_rule$analysis_type != "any") {
                            current_type <- self$options$analysisType

                            if (dep_rule$analysis_type == "standard" &&
                                !current_type %in% c("standard", "comprehensive", "publication")) {
                                warnings[[option_name]] <- list(
                                    option = option_name,
                                    message = paste(
                                        option_name, "is enabled but requires 'standard' or higher analysis type.",
                                        "Current type:", current_type
                                    )
                                )
                            } else if (dep_rule$analysis_type == "comprehensive" &&
                                !current_type %in% c("comprehensive", "publication")) {
                                warnings[[option_name]] <- list(
                                    option = option_name,
                                    message = paste(
                                        option_name, "is enabled but requires 'comprehensive' or 'publication' analysis type.",
                                        "Current type:", current_type
                                    )
                                )
                            }
                        }
                    }
                }

                # Additional logical dependency checks

                # Bootstrap-dependent options
                bootstrap_dependent <- c("calculateNRI", "calculateIDI")
                for (option_name in bootstrap_dependent) {
                    if (!is.null(self$options[[option_name]]) && isTRUE(self$options[[option_name]])) {
                        if (is.null(self$options$performBootstrap) || !isTRUE(self$options$performBootstrap)) {
                            warnings[[paste0(option_name, "_bootstrap")]] <- list(
                                option = option_name,
                                message = paste(
                                    option_name, "is enabled but bootstrap validation is disabled.",
                                    "Consider enabling 'performBootstrap' for confidence intervals."
                                )
                            )
                        }
                    }
                }

                # Multifactorial analysis dependencies
                if (!is.null(self$options$enableMultifactorialAnalysis) &&
                    isTRUE(self$options$enableMultifactorialAnalysis)) {
                    has_covariates <- (!is.null(self$options$continuousCovariates) &&
                        length(self$options$continuousCovariates) > 0) ||
                        (!is.null(self$options$categoricalCovariates) &&
                            length(self$options$categoricalCovariates) > 0)

                    if (!has_covariates) {
                        warnings[["multifactorial_no_covariates"]] <- list(
                            option = "enableMultifactorialAnalysis",
                            message = "Multifactorial analysis is enabled but no covariates are specified."
                        )
                    }
                }

                return(list(
                    issues = issues,
                    warnings = warnings,
                    has_issues = length(issues) > 0,
                    has_warnings = length(warnings) > 0
                ))
            },
            .validateData = function() {
                # Comprehensive data validation using utility functions

                # Prepare options object with all necessary fields
                validation_options <- self$options

                # Add any additional variables needed for this analysis
                additional_vars <- character(0)

                # Include covariates if multifactorial analysis is enabled
                if (isTRUE(self$options$enableMultifactorialAnalysis)) {
                    continuous_vars <- self$options$continuousCovariates
                    categorical_vars <- self$options$categoricalCovariates
                    covariate_vars <- c(continuous_vars, categorical_vars)

                    # Remove any NULL or empty values
                    covariate_vars <- covariate_vars[!is.null(covariate_vars) & covariate_vars != ""]

                    if (length(covariate_vars) > 0) {
                        additional_vars <- c(additional_vars, covariate_vars)
                    }
                }

                # Also include institution variable if specified for cross-validation
                if (!is.null(self$options$institutionVariable) && self$options$institutionVariable != "") {
                    additional_vars <- c(additional_vars, self$options$institutionVariable)
                }

                # Call utility validation function with checkpoint callback
                validation_result <- stagemigration_validateData(
                    data = self$data,
                    options = validation_options,
                    additional_vars = additional_vars,
                    checkpoint_callback = private$.checkpoint
                )

                # Handle validation errors
                if (!validation_result$valid) {
                    # Create formatted error HTML
                    error_html <- "<div style='color: inherit; padding: 15px; background-color: rgba(255, 33, 67, 0.09); border-left: 4px solid #d32f2f;'>"
                    error_html <- paste0(error_html, "<h3 style='margin-top: 0;'>Validation Errors:</h3><ul>")

                    for (error in validation_result$errors) {
                        error_html <- paste0(error_html, "<li>", error, "</li>")
                    }

                    # Stop execution cleanly
                    jmvcore::reject(paste("Data validation failed:", paste(validation_result$errors, collapse = "; ")))
                }

                # Surface validation findings as graded notices.
                #
                # These were emitted with warning(), which lands in jamovi's undifferentiated
                # "Analysis Notes" panel, so a POOR sample-size finding looked like routine
                # package chatter -- and the recommendations were never read at all.
                adequacy <- validation_result$metadata$sample_adequacy
                if (!is.null(adequacy)) {
                    sev <- switch(adequacy$level %||% "",
                        CRITICAL = "ERROR",
                        POOR = "STRONG_WARNING",
                        MARGINAL = "WARNING",
                        "INFO")
                    parts <- c(adequacy$messages, adequacy$recommendations)
                    parts <- parts[nzchar(parts)]
                    if (length(parts) > 0) {
                        private$.addNotice(sev, .("Sample size adequacy"), paste(parts, collapse = " \u{2022} "))
                    }
                }
                other_warnings <- setdiff(validation_result$warnings, adequacy$messages)
                other_warnings <- other_warnings[nzchar(other_warnings)]
                if (length(other_warnings) > 0) {
                    private$.addNotice("WARNING", .("Data validation"), paste(other_warnings, collapse = " \u{2022} "))
                }
                # Informational messages ("Event level used", "Removed N incomplete cases") are not
                # warnings. Merging them into the WARNING above put a yellow banner on every clean run
                # whose only content was the event level.
                info_msgs <- setdiff(validation_result$notices, adequacy$recommendations)
                info_msgs <- info_msgs[nzchar(info_msgs)]
                if (length(info_msgs) > 0) {
                    private$.addNotice("INFO", .("Data preparation"), paste(info_msgs, collapse = " \u{2022} "))
                }

                # Return validated data with event_binary column
                # C1 RCE hardening (Step-1 anchor): add fixed internal-safe column aliases so downstream
                # Surv() formula builders can reference literal names (stage_old / stage_new / time_internal /
                # event_binary) instead of raw self$options$* column names. Additive + guarded → no behavior
                # change for existing code; event_binary is already present. (Step 2: migrate the ~90
                # as.formula(paste(...)) sites to use these literals - tracked TODO below.)
                vdata <- validation_result$data
                if (!is.null(self$options$oldStage) && self$options$oldStage %in% names(vdata)) {
                    vdata[["stage_old"]] <- vdata[[self$options$oldStage]]
                }
                if (!is.null(self$options$newStage) && self$options$newStage %in% names(vdata)) {
                    vdata[["stage_new"]] <- vdata[[self$options$newStage]]
                }
                if (!is.null(self$options$survivalTime) && self$options$survivalTime %in% names(vdata)) {
                    vdata[["time_internal"]] <- vdata[[self$options$survivalTime]]
                }
                return(vdata)
            },
            .calculateBasicMigration = function(data) {
                # Comprehensive migration analysis
                old_stage <- self$options$oldStage
                new_stage <- self$options$newStage

                # Create cross-tabulation
                migration_table <- table(
                    Old = data[[old_stage]],
                    New = data[[new_stage]]
                )

                # Calculate migration statistics
                total_patients <- sum(migration_table)
                # Unchanged = same label under both systems. sum(diag()) was positional, so it was wrong
                # whenever the two factors list their levels in a different order.
                unchanged <- sum(as.character(data[[old_stage]]) == as.character(data[[new_stage]]), na.rm = TRUE)
                migrated <- total_patients - unchanged
                migration_rate <- migrated / total_patients

                # Calculate stage-wise migration
                stage_migration <- list()
                for (i in seq_len(nrow(migration_table))) {
                    stage_name <- rownames(migration_table)[i]
                    stage_total <- sum(migration_table[i, ])
                    # Check if this stage exists in new staging
                    if (stage_name %in% colnames(migration_table)) {
                        stage_unchanged <- migration_table[i, stage_name]
                    } else {
                        stage_unchanged <- 0
                    }
                    stage_migrated <- stage_total - stage_unchanged

                    stage_migration[[stage_name]] <- list(
                        total = stage_total,
                        unchanged = stage_unchanged,
                        migrated = stage_migrated,
                        migration_rate = if (stage_total > 0) stage_migrated / stage_total else 0,
                        destinations = migration_table[i, migration_table[i, ] > 0]
                    )
                }

                # Direction of migration on one ordered scale (see .stageDirection)
                dirn <- private$.stageDirection(data[[old_stage]], data[[new_stage]])
                if (dirn$comparable) {
                    upstaging <- sum(dirn$direction == 1L, na.rm = TRUE)
                    downstaging <- sum(dirn$direction == -1L, na.rm = TRUE)
                } else {
                    upstaging <- NA_integer_
                    downstaging <- NA_integer_
                    private$.addNotice("WARNING", .("Direction of migration not determined"), jmvcore::format(
                        .("The two staging systems use different stage labels (original only: {onlyOld}; new only: {onlyNew}), so whether a change is up or down cannot be read from the data. Upstaging and downstaging are not reported; every patient whose label changed counts as migrated. Recode both variables to one shared, ordered set of labels to obtain them."),
                        onlyOld = if (length(dirn$only_old)) paste(dirn$only_old, collapse = ", ") else "none",
                        onlyNew = if (length(dirn$only_new)) paste(dirn$only_new, collapse = ", ") else "none"
                    ))
                }

                # Statistical tests with proper error handling
                chi_test <- NULL
                fisher_test <- NULL

                # Chi-square test
                tryCatch(
                    {
                        # Sparse migration tables routinely have expected counts below 5, and chisq.test() then warns.
                        # The enclosing tryCatch catches only errors, so that warning reached jamovi's Analysis Notes panel.
                        chi_test <- withCallingHandlers(
                            chisq.test(migration_table),
                            warning = function(w) {
                                if (grepl("Chi-squared approximation may be incorrect", conditionMessage(w), fixed = TRUE)) {
                                    private$.addNotice("WARNING", .("Chi-square approximation"),
                                        .("Some expected cell counts in the migration table are below 5, so the chi-square p-value may be inaccurate. Fisher's exact test is reported alongside it for cohorts of up to 1,000 patients."))
                                    invokeRestart("muffleWarning")
                                }
                            })
                    },
                    error = function(e) {
                        private$.addNotice("WARNING", .("Chi-square test not computed"), conditionMessage(e))
                    }
                )

                # Fisher's exact test (only for smaller tables)
                # min_cell_count >= 1 was an inverted gate: a stage-migration table is sparse
                # by construction (most off-diagonal cells are 0), which is precisely WHY one
                # reaches for Fisher over chi-square. fisher.test handles zero cells fine, so
                # the old condition meant Fisher essentially never ran, exactly when needed.
                if (total_patients <= 1000) {
                    tryCatch(
                        {
                            fisher_test <- fisher.test(migration_table, simulate.p.value = TRUE)
                        },
                        error = function(e) {
                            private$.addNotice("WARNING", .("Fisher's exact test not computed"), conditionMessage(e))
                        }
                    )
                }

                return(list(
                    migration_table = migration_table,
                    total_patients = total_patients,
                    unchanged = unchanged,
                    migrated = migrated,
                    migration_rate = migration_rate,
                    upstaging = upstaging,
                    downstaging = downstaging,
                    upstaging_rate = upstaging / total_patients,
                    downstaging_rate = downstaging / total_patients,
                    stage_migration = stage_migration,
                    chi_test = chi_test,
                    fisher_test = fisher_test
                ))
            },
            .calculateAdvancedMetrics = function(data) {
                # Delegated to stagemigration-metrics.R
                return(stagemigration_calculateAdvancedMetrics(data, self$options, function() private$.checkpoint()))
            },
            .compareBootstrapCIndex = function(data, old_stage, new_stage, time_var, event_var, n_boot = 200) {
                # Delegated to stagemigration-metrics.R
                return(stagemigration_compareBootstrapCIndex(data, old_stage, new_stage, time_var, event_var, n_boot, function() private$.checkpoint(), options = self$options))
            },
            .calculateNRI = function(data, time_points = NULL) {
                # Delegated to stagemigration-metrics.R
                return(stagemigration_calculateNRI(data, self$options, time_points, function() private$.checkpoint()))
            },
            .extractSurvivalProbabilities = function(fit, data, time_point, stage_var) {
                # Per-patient S(t) from a stratified Kaplan-Meier fit.
                #
                # Three defects fixed here:
                #  1. approx() defaults to method = "linear", but the KM estimator is a
                #     right-continuous STEP function. Linear interpolation returns a value
                #     strictly below the true S(t) everywhere between event times, biasing
                #     every patient's survival probability downward. Use method = "constant".
                #  2. The fallback stratum match used grepl(fixed = TRUE), a plain substring
                #     test, so stage "1" matched "stage=1", "stage=1A" AND "stage=10" and the
                #     first hit in level order silently won. For AJCC labels (I, IA, II, III)
                #     that misassigns survival probabilities to whole stage groups. Exact
                #     match only; unmatched patients get NA rather than a wrong stratum.
                #  3. time_point exactly equal to the first event time returned 1.0, but S(t)
                #     has already dropped there.
                probs <- rep(NA_real_, nrow(data))
                strata_names <- names(fit$strata)
                has_strata <- !is.null(strata_names) && length(strata_names) > 0

                step_surv <- function(times, surv, t) {
                    if (length(times) == 0) return(NA_real_)
                    if (t < min(times)) return(1.0)
                    if (t >= max(times)) return(surv[length(surv)])
                    stats::approx(times, surv, t, method = "constant", f = 0)$y
                }

                if (!has_strata) {
                    val <- step_surv(fit$time, fit$surv, time_point)
                    return(rep(val, nrow(data)))
                }

                ends <- cumsum(fit$strata)
                starts <- c(1, utils::head(ends, -1) + 1)

                # cache one value per stratum instead of recomputing per patient
                cache <- vapply(seq_along(strata_names), function(k) {
                    step_surv(fit$time[starts[k]:ends[k]], fit$surv[starts[k]:ends[k]], time_point)
                }, numeric(1))
                names(cache) <- strata_names

                targets <- paste0(stage_var, "=", as.character(data[[stage_var]]))
                hit <- match(targets, strata_names)
                probs[!is.na(hit)] <- cache[hit[!is.na(hit)]]

                return(probs)
            },
            .calculateIDI = function(data) {
                # Delegated to stagemigration-metrics.R
                return(stagemigration_calculateIDI(data, self$options, function() private$.checkpoint()))
            },
            .calculateLinearTrendTest = function(data, old_stage, new_stage, time_var, event_var) {
                # Linear Trend Chi-square test for ordinal staging trends
                # Tests if there's a linear trend in survival across ordered stages

                tryCatch(
                    {
                        # Prepare survival object
                        surv_obj <- survival::Surv(data[[time_var]], data[[event_var]])

                        # Function to calculate linear trend test for a staging system
                        .calculateTrendForStage <- function(stage_col) {
                            # Get unique stages and sort them
                            stages <- sort(unique(data[[stage_col]]))
                            n_stages <- length(stages)

                            if (n_stages < 3) {
                                return(list(
                                    stat = NA, p_value = NA, df = NA,
                                    interpretation = "At least 3 stages required for trend test"
                                ))
                            }

                            # Create ordered numeric scores for stages (1, 2, 3, ...)
                            stage_scores <- match(data[[stage_col]], stages)

                            # Fit Cox model with stage as continuous variable (for trend)
                            trend_formula <- as.formula(paste("surv_obj ~ stage_scores"))
                            trend_data <- data.frame(surv_obj = surv_obj, stage_scores = stage_scores)

                            # Remove rows with missing stage scores
                            trend_data <- trend_data[!is.na(trend_data$stage_scores), ]

                            if (nrow(trend_data) < 10) {
                                return(list(
                                    stat = NA, p_value = NA, df = 1,
                                    interpretation = "Insufficient data for trend test"
                                ))
                            }

                            # Fit trend model
                            trend_cox <- survival::coxph(trend_formula, data = trend_data)

                            # Extract Wald chi-square statistic for linear trend
                            trend_summary <- summary(trend_cox)
                            wald_stat <- trend_summary$waldtest["test"]
                            wald_p <- trend_summary$waldtest["pvalue"]

                            # Interpretation
                            interpretation <- if (is.na(wald_p)) {
                                "Unable to calculate trend test"
                            } else if (wald_p < 0.001) {
                                "Highly significant linear trend (p < 0.001)"
                            } else if (wald_p < 0.01) {
                                "Significant linear trend (p < 0.01)"
                            } else if (wald_p < 0.05) {
                                "Statistically significant linear trend (p < 0.05)"
                            } else if (wald_p < 0.10) {
                                "Marginal evidence of linear trend (p < 0.10)"
                            } else {
                                "No significant linear trend detected"
                            }

                            # Add direction information
                            if (!is.na(wald_p) && wald_p < 0.05) {
                                coef_value <- coef(trend_cox)[1]
                                direction <- if (coef_value > 0) {
                                    " (increasing hazard with higher stages)"
                                } else {
                                    " (decreasing hazard with higher stages)"
                                }
                                interpretation <- paste0(interpretation, direction)
                            }

                            return(list(
                                stat = as.numeric(wald_stat),
                                p_value = as.numeric(wald_p),
                                df = 1,
                                n_stages = n_stages,
                                coefficient = if (exists("coef_value")) coef_value else coef(trend_cox)[1],
                                interpretation = interpretation
                            ))
                        }

                        # Calculate trend tests for both staging systems
                        old_trend <- .calculateTrendForStage(old_stage)
                        new_trend <- .calculateTrendForStage(new_stage)

                        # Overall comparison
                        comparison <- if (!is.na(old_trend$p_value) && !is.na(new_trend$p_value)) {
                            old_sig <- old_trend$p_value < 0.05
                            new_sig <- new_trend$p_value < 0.05

                            if (old_sig && new_sig) {
                                "Both staging systems show significant linear trends"
                            } else if (!old_sig && new_sig) {
                                "New staging system shows better linear trend"
                            } else if (old_sig && !new_sig) {
                                "Original staging system shows better linear trend"
                            } else {
                                "Neither staging system shows significant linear trend"
                            }
                        } else {
                            "Unable to compare linear trends"
                        }

                        return(list(
                            old_trend = old_trend,
                            new_trend = new_trend,
                            comparison = comparison
                        ))
                    },
                    error = function(e) {
                        return(list(
                            old_trend = list(
                                stat = NA, p_value = NA, df = NA,
                                interpretation = paste("Error:", e$message)
                            ),
                            new_trend = list(
                                stat = NA, p_value = NA, df = NA,
                                interpretation = paste("Error:", e$message)
                            ),
                            comparison = "Linear trend test failed"
                        ))
                    }
                )
            },
            .performTimeROCAnalysis = function(data, force = FALSE) {
                # Time-dependent ROC analysis
                if (!force && !self$options$performROCAnalysis) {
                    return(NULL)
                }

                # Check dependencies
                if (is.null(self$options$oldStage) || is.null(self$options$newStage) ||
                    is.null(self$options$survivalTime) || is.null(self$options$event)) {
                    return(list(error = "Missing required variables for ROC Analysis"))
                }

                # Parse time points
                time_points_str <- self$options$rocTimePoints
                time_points <- as.numeric(unlist(strsplit(time_points_str, "\\s*,\\s*")))
                time_points <- time_points[!is.na(time_points)]

                if (length(time_points) == 0) {
                    time_points <- c(12, 24, 36, 60) # Default time points
                }

                time_var <- self$options$survivalTime
                event_var <- "event_binary"

                # TODO (security): C1 RCE - Step 2 IN PROGRESS (~88 of ~90 sites remain). old_stage/new_stage/
                #   time_var are RAW self$options$oldStage/newStage/survivalTime column names pasted into
                #   as.formula(paste(...)) → coxph/survreg (model.frame evaluates the RHS = arbitrary code
                #   execution via a crafted column name). Step 1 (DONE, .validateData) exposes fixed internal-
                #   safe aliases stage_old / stage_new / time_internal / event_binary in the analysis data.
                #   DONE so far: .performTimeROCAnalysis (the 2 formulas just below). REMAINING: migrate the
                #   other ~88 as.formula(paste("Surv(", time_var, ...) ~", old_stage)) sites to those literals,
                #   e.g. as.formula("Surv(time_internal, event_binary) ~ stage_old"). PER-SITE CHECKLIST:
                #   (1) confirm the method receives the .validateData() output (aliases present), (2) confirm it
                #   does NOT display Cox COEFFICIENT NAMES (would regress oldStageName→stage_old in tables) - 
                #   if it does, keep raw name for the displayed label and only swap the formula RHS, (3) keep
                #   the RAW option name for any DISPLAY use. Covariate RHS (covariate_formula, ~L13080) also
                #   needs composeTerms / internal aliasing.
                # Fit Cox models
                # C1 fix (Step 2): internal-safe literals (stage_old/stage_new/time_internal/event_binary
                # added by .validateData) - closes the RCE; same model (these are copies of the user cols).
                old_formula <- as.formula("Surv(time_internal, event_binary) ~ stage_old")
                new_formula <- as.formula("Surv(time_internal, event_binary) ~ stage_new")

                old_cox <- coxph(old_formula, data = data)
                new_cox <- coxph(new_formula, data = data)

                # Get risk scores
                old_risk <- predict(old_cox, type = "risk")
                new_risk <- predict(new_cox, type = "risk")

                roc_results <- list()

                # Calculate time-dependent ROC for each time point
                if (requireNamespace("timeROC", quietly = TRUE)) {
                    for (t in time_points) {
                        # Checkpoint before each time point ROC calculation
                        private$.checkpoint()

                        # Skip time points that are beyond the data range
                        max_time <- max(data[[time_var]], na.rm = TRUE)
                        if (t > max_time) {
                            next
                        }

                        # TimeROC analysis for old staging
                        old_roc <- try(
                            {
                                private$.timeROC(
                                    T = data[[time_var]],
                                    delta = data[[event_var]],
                                    marker = old_risk,
                                    cause = 1,
                                    times = t,
                                    iid = TRUE
                                )
                            },
                            silent = TRUE
                        )

                        # TimeROC analysis for new staging
                        new_roc <- try(
                            {
                                private$.timeROC(
                                    T = data[[time_var]],
                                    delta = data[[event_var]],
                                    marker = new_risk,
                                    cause = 1,
                                    times = t,
                                    iid = TRUE
                                )
                            },
                            silent = TRUE
                        )

                        if (!inherits(old_roc, "try-error") && !inherits(new_roc, "try-error")) {
                            # timeROC prepends t = 0 when one time is requested (times = c(0, t)), so
                            # AUC[1] was the NA t = 0 slot: this branch never ran and every ROC silently
                            # fell back to pROC on a censoring-truncated sample.
                            k_old <- private$.timeROCIndex(old_roc, t)
                            k_new <- private$.timeROCIndex(new_roc, t)
                            old_auc <- old_roc$AUC[k_old]
                            new_auc <- new_roc$AUC[k_new]

                            # Check if AUC values are valid (not NA)
                            if (!is.na(old_auc) && !is.na(new_auc)) {
                                # Safely calculate confidence intervals
                                old_sd <- private$.safeAtomic(old_roc$inference$vect_sd_1[k_old], "numeric", NA)
                                new_sd <- private$.safeAtomic(new_roc$inference$vect_sd_1[k_new], "numeric", NA)

                                # timeROC's inference$vect_sd_1 is already a standard error
                                # (see timeROC:::confint.ipcwsurvivalROC); sqrt() inflated the
                                # half-width by ~7x and pushed CIs outside [0, 1].
                                zc <- stats::qnorm(private$.ciProbs()[2])

                                old_ci <- if (!is.na(old_sd) && old_sd >= 0) {
                                    c(old_auc - zc * old_sd, old_auc + zc * old_sd)
                                } else {
                                    c(NA, NA)
                                }

                                new_ci <- if (!is.na(new_sd) && new_sd >= 0) {
                                    c(new_auc - zc * new_sd, new_auc + zc * new_sd)
                                } else {
                                    c(NA, NA)
                                }

                                roc_results[[paste0("t", t)]] <- list(
                                    time_point = t,
                                    old_auc = old_auc,
                                    new_auc = new_auc,
                                    auc_improvement = new_auc - old_auc,
                                    old_ci = old_ci,
                                    new_ci = new_ci,
                                    # Plots read FP[, 1] / TP[, 1]: keep only the requested time's column,
                                    # plus the full object for the paired comparison.
                                    old_roc = list(FP = old_roc$FP[, k_old, drop = FALSE], TP = old_roc$TP[, k_old, drop = FALSE], troc = old_roc, k = k_old),
                                    new_roc = list(FP = new_roc$FP[, k_new, drop = FALSE], TP = new_roc$TP[, k_new, drop = FALSE], troc = new_roc, k = k_new)
                                )
                            } else {
                                # If timeROC returned NA, use pROC fallback
                                old_roc <- NULL
                                new_roc <- NULL
                            }
                        }

                        # If timeROC failed or returned NA, try time-specific pROC fallback
                        if (is.null(old_roc) || is.null(new_roc) || inherits(old_roc, "try-error") || inherits(new_roc, "try-error") ||
                            (exists("old_auc") && exists("new_auc") && (is.na(old_auc) || is.na(new_auc)))) {
                            # Try alternative approach using pROC with time-specific events
                            if (requireNamespace("pROC", quietly = TRUE)) {
                                # Create time-specific event indicator for this time point
                                event_at_time <- ifelse(data[[time_var]] <= t & data[[event_var]] == 1, 1, 0)
                                # Only include patients who either had event by time t or were followed past time t
                                include_patients <- (data[[time_var]] <= t & data[[event_var]] == 1) | (data[[time_var]] > t)

                                if (sum(include_patients) > 10 && sum(event_at_time[include_patients]) > 5) {
                                    # Use time-specific ROC with event status at time t
                                    old_roc_simple <- try(
                                        {
                                            pROC::roc(event_at_time[include_patients], old_risk[include_patients], quiet = TRUE)
                                        },
                                        silent = TRUE
                                    )

                                    new_roc_simple <- try(
                                        {
                                            pROC::roc(event_at_time[include_patients], new_risk[include_patients], quiet = TRUE)
                                        },
                                        silent = TRUE
                                    )

                                    if (!inherits(old_roc_simple, "try-error") && !inherits(new_roc_simple, "try-error")) {
                                        old_auc <- as.numeric(old_roc_simple$auc)
                                        new_auc <- as.numeric(new_roc_simple$auc)

                                        # Calculate confidence intervals using pROC
                                        old_ci <- try(
                                            {
                                                ci_result <- pROC::ci.auc(old_roc_simple, quiet = TRUE)
                                                c(ci_result[1], ci_result[3])
                                            },
                                            silent = TRUE
                                        )
                                        if (inherits(old_ci, "try-error")) old_ci <- c(NA, NA)

                                        new_ci <- try(
                                            {
                                                ci_result <- pROC::ci.auc(new_roc_simple, quiet = TRUE)
                                                c(ci_result[1], ci_result[3])
                                            },
                                            silent = TRUE
                                        )
                                        if (inherits(new_ci, "try-error")) new_ci <- c(NA, NA)

                                        # Create ROC curve data
                                        old_roc_obj <- list(
                                            FP = matrix(1 - old_roc_simple$specificities, ncol = 1),
                                            TP = matrix(old_roc_simple$sensitivities, ncol = 1),
                                            roc_simple = old_roc_simple
                                        )

                                        new_roc_obj <- list(
                                            FP = matrix(1 - new_roc_simple$specificities, ncol = 1),
                                            TP = matrix(new_roc_simple$sensitivities, ncol = 1),
                                            roc_simple = new_roc_simple
                                        )


                                        roc_results[[paste0("t", t)]] <- list(
                                            time_point = t,
                                            old_auc = old_auc,
                                            new_auc = new_auc,
                                            auc_improvement = new_auc - old_auc,
                                            old_ci = old_ci,
                                            new_ci = new_ci,
                                            old_roc = old_roc_obj,
                                            new_roc = new_roc_obj
                                        )
                                    } else {
                                        private$.addNotice("INFO", .("Time-dependent ROC"), jmvcore::format(.("The AUC at {t} months could not be estimated."), t = t))
                                    }
                                } else {
                                    private$.addNotice("INFO", .("Time-dependent ROC"), jmvcore::format(.("The AUC at {t} months was not estimated: only {includePatients} patients and {eventAtTime} events are informative at that time."), t = t, includePatients = sprintf("%d", sum(include_patients)), eventAtTime = sprintf("%d", sum(event_at_time[include_patients]))))
                                }
                            }
                        }
                    }
                } else {
                    # timeROC is a hard dependency (DESCRIPTION Imports), so this branch should
                    # be unreachable. It previously fell back to pROC::roc() on the OVERALL event
                    # indicator -- ignoring both the time point and censoring -- inside a loop
                    # over time_points, so the "time-dependent AUC" was one identical number at
                    # 12, 24, 36 and 60 months, reported alongside hardcoded +/-0.05 intervals
                    # labelled as confidence intervals. Refusing is the only honest option.
                    roc_results$unavailable <- list(
                        reason = "timeROC is required for time-dependent ROC analysis but is not installed"
                    )
                }

                return(roc_results)
            },
            .performDCA = function(data) {
                # Decision Curve Analysis
                if (!self$options$performDCA) {
                    return(NULL)
                }

                # Check dependencies
                if (is.null(self$options$oldStage) || is.null(self$options$newStage) ||
                    is.null(self$options$survivalTime) || is.null(self$options$event)) {
                    return(list(error = "Missing required variables for DCA"))
                }

                old_stage <- self$options$oldStage
                new_stage <- self$options$newStage
                time_var <- self$options$survivalTime
                event_var <- "event_binary"

                # Fit Cox models with consistent error handling (internal aliases prevent syntax errors with spaces)
                old_formula <- stats::as.formula("survival::Surv(time_internal, event_binary) ~ stage_old")
                new_formula <- stats::as.formula("survival::Surv(time_internal, event_binary) ~ stage_new")

                old_cox <- private$.safeExecute(
                    {
                        coxph(old_formula, data = data)
                    },
                    errorReturn = NULL,
                    errorMessage = "Failed to fit Cox model for original staging in DCA",
                    warningMessage = "Decision Curve Analysis failed for original staging system"
                )

                new_cox <- private$.safeExecute(
                    {
                        coxph(new_formula, data = data)
                    },
                    errorReturn = NULL,
                    errorMessage = "Failed to fit Cox model for new staging in DCA",
                    warningMessage = "Decision Curve Analysis failed for new staging system"
                )

                if (is.null(old_cox) || is.null(new_cox)) {
                    return(list(error = "Failed to fit Cox models for DCA"))
                }

                # Evaluation horizon. Was hardcoded at 60 months while
                # .calculateDecisionCurveAnalysis derived its horizon from nriTimePoints, so the
                # two DCA outputs could silently disagree about the time point they described.
                time_horizon <- private$.idiTimePoint(data)

                # Calculate baseline survival
                baseline_surv_old <- survfit(old_cox)
                baseline_surv_new <- survfit(new_cox)

                # Extract baseline survival at time horizon
                baseline_prob_old <- private$.extractBaselineSurvival(baseline_surv_old, time_horizon)
                baseline_prob_new <- private$.extractBaselineSurvival(baseline_surv_new, time_horizon)

                # Calculate individual risk predictions
                old_lp <- predict(old_cox, type = "lp")
                new_lp <- predict(new_cox, type = "lp")

                old_risk <- 1 - (baseline_prob_old^exp(old_lp))
                new_risk <- 1 - (baseline_prob_new^exp(new_lp))

                dca_results <- list()

                if (requireNamespace("dcurves", quietly = TRUE)) {
                    # Survival DCA (Vickers' KM net benefit), not binary DCA.
                    #
                    # Previously the outcome was ifelse(time <= horizon & event == 1, 1, 0) and
                    # dca() was called in its BINARY form, so every patient censored before the
                    # horizon was counted as a false positive if flagged high-risk. On a 5-year
                    # horizon in a typical pathology cohort that systematically depresses net
                    # benefit for both systems, and differentially between them.
                    #
                    # dcurves::dca() accepts a Surv() left-hand side with `time=`, which applies
                    # the Kaplan-Meier net-benefit formula and handles censoring correctly.
                    dca_data <- data.frame(
                        .dca_time = as.numeric(data[[time_var]]),
                        .dca_event = as.numeric(data[[event_var]]),
                        old_risk = old_risk,
                        new_risk = new_risk
                    )
                    dca_data <- dca_data[stats::complete.cases(dca_data), , drop = FALSE]

                    dca_result <- private$.safeExecute(
                        {
                            dcurves::dca(
                                formula = survival::Surv(.dca_time, .dca_event) ~ old_risk + new_risk,
                                data = dca_data,
                                time = time_horizon,
                                thresholds = seq(0.01, 0.99, by = 0.01)
                            )
                        },
                        errorReturn = NULL,
                        errorMessage = "Failed to perform Decision Curve Analysis",
                        warningMessage = "Decision Curve Analysis could not be completed. Please check your data."
                    )

                    if (!is.null(dca_result)) {
                        dca_results$dca_result <- dca_result
                        dca_results$time_horizon <- time_horizon
                    } else {
                        dca_results$error <- "DCA calculation failed"
                    }
                }

                return(dca_results)
            },
            .extractBaselineSurvival = function(surv_fit, time_point) {
                # Extract baseline survival probability at specific time point
                if (time_point <= min(surv_fit$time)) {
                    return(1.0)
                } else if (time_point >= max(surv_fit$time)) {
                    return(min(surv_fit$surv))
                } else {
                    return(approx(surv_fit$time, surv_fit$surv, time_point)$y)
                }
            },
            .performLegacyBootstrapValidation = function(data, bootstrapReps = NULL) {
                # Bootstrap validation with optimism correction
                if (!self$options$performBootstrap) {
                    return(NULL)
                }

                # Check dependencies
                if (is.null(self$options$oldStage) || is.null(self$options$newStage) ||
                    is.null(self$options$survivalTime) || is.null(self$options$event)) {
                    return(list(error = "Missing required variables for bootstrap validation"))
                }

                # Get bootstrap repetitions using standardized helper
                if (is.null(bootstrapReps)) {
                    bootstrapReps <- private$.getBootstrapReps()
                }

                old_stage <- self$options$oldStage
                new_stage <- self$options$newStage
                time_var <- self$options$survivalTime
                event_var <- "event_binary"

                # Bootstrap function for validation
                bootstrap_function <- function(data, indices) {
                    boot_data <- data[indices, ]

                    # Fit models on bootstrap sample (internal aliases prevent syntax errors with spaces)
                    old_formula <- stats::as.formula("survival::Surv(time_internal, event_binary) ~ stage_old")
                    new_formula <- stats::as.formula("survival::Surv(time_internal, event_binary) ~ stage_new")

                    old_cox_boot <- private$.safeExecute(
                        {
                            coxph(old_formula, data = boot_data)
                        },
                        errorReturn = NULL,
                        errorMessage = "Bootstrap: Failed to fit old Cox model",
                        silent = TRUE
                    )

                    new_cox_boot <- private$.safeExecute(
                        {
                            coxph(new_formula, data = boot_data)
                        },
                        errorReturn = NULL,
                        errorMessage = "Bootstrap: Failed to fit new Cox model",
                        silent = TRUE
                    )

                    if (is.null(old_cox_boot) || is.null(new_cox_boot)) {
                        return(c(NA, NA, NA))
                    }

                    # Helper to safely get concordance using standardized error handling
                    safe_concordance <- function(model, newdata = NULL) {
                        return(private$.safeExecute(
                            {
                                concordance(model, newdata = newdata)$concordance
                            },
                            errorReturn = NA,
                            errorMessage = "Failed to calculate concordance",
                            silent = TRUE
                        ))
                    }

                    # Calculate all four C-indices safely
                    old_c_boot <- safe_concordance(old_cox_boot)
                    new_c_boot <- safe_concordance(new_cox_boot)
                    old_c_orig <- safe_concordance(old_cox_boot, newdata = data)
                    new_c_orig <- safe_concordance(new_cox_boot, newdata = data)

                    # Calculate optimism only if all values are valid
                    optimism <- NA
                    if (!is.na(old_c_boot) && !is.na(new_c_boot) && !is.na(old_c_orig) && !is.na(new_c_orig)) {
                        optimism <- (new_c_boot - old_c_boot) - (new_c_orig - old_c_orig)
                    }

                    return(c(old_c_boot, new_c_boot, optimism))
                }

                # Perform bootstrap
                if (requireNamespace("boot", quietly = TRUE)) {
                    boot_results <- boot::boot(
                        data = data,
                        statistic = bootstrap_function,
                        R = bootstrapReps
                    )

                    # Calculate optimism-corrected estimates
                    apparent_improvement <- boot_results$t0[2] - boot_results$t0[1]
                    mean_optimism <- mean(boot_results$t[, 3], na.rm = TRUE)
                    optimism_corrected_improvement <- apparent_improvement - mean_optimism

                    # Percentile CI for the C-index DIFFERENCE.
                    #
                    # boot.ci(index = c(2, 1)) does NOT mean "column 2 minus column 1": index is
                    # c(<statistic position>, <variance position>), so that call returned the CI
                    # of the new system's C-index alone, with column 1 misread as a variance,
                    # and it was handed back to the caller as improvement_ci. The difference has
                    # to be formed first and its own quantiles taken.
                    boot_diffs <- boot_results$t[, 2] - boot_results$t[, 1]
                    boot_diffs <- boot_diffs[is.finite(boot_diffs)]

                    improvement_ci <- if (length(boot_diffs) >= 20) {
                        unname(stats::quantile(boot_diffs, private$.ciProbs(), na.rm = TRUE))
                    } else {
                        NULL
                    }

                    return(list(
                        boot_results = boot_results,
                        apparent_improvement = apparent_improvement,
                        mean_optimism = mean_optimism,
                        optimism_corrected_improvement = optimism_corrected_improvement,
                        improvement_ci = improvement_ci,
                        bootstrapReps = bootstrapReps
                    ))
                }

                return(NULL)
            },
            .bootstrapConcordance = function(data, old_formula, new_formula) {
                # Bootstrap confidence intervals for C-index
                bootstrap_c <- function(data, indices) {
                    boot_data <- data[indices, ]

                    old_cox <- try(coxph(old_formula, data = boot_data), silent = TRUE)
                    new_cox <- try(coxph(new_formula, data = boot_data), silent = TRUE)

                    if (inherits(old_cox, "try-error") || inherits(new_cox, "try-error")) {
                        return(c(NA, NA))
                    }

                    old_c <- concordance(old_cox)$concordance
                    new_c <- concordance(new_cox)$concordance

                    return(c(old_c, new_c))
                }

                if (requireNamespace("boot", quietly = TRUE)) {
                    boot_results <- boot::boot(
                        data = data,
                        statistic = bootstrap_c,
                        R = private$.getBootstrapReps(500) # Limit for efficiency
                    )

                    # Calculate confidence intervals with standardized error handling
                    old_ci <- private$.safeExecute(
                        {
                            boot::boot.ci(boot_results, type = "perc", index = 1)
                        },
                        errorReturn = NULL,
                        errorMessage = "Failed to calculate bootstrap CI for original staging",
                        silent = TRUE
                    )

                    new_ci <- private$.safeExecute(
                        {
                            boot::boot.ci(boot_results, type = "perc", index = 2)
                        },
                        errorReturn = NULL,
                        errorMessage = "Failed to calculate bootstrap CI for new staging",
                        silent = TRUE
                    )

                    return(list(
                        boot_results = boot_results,
                        old_ci = old_ci,
                        new_ci = new_ci
                    ))
                }

                return(NULL)
            },
            .bootstrapIDI = function(data, old_formula, new_formula) {
                # Bootstrap confidence intervals for IDI
                bootstrap_idi <- function(data, indices) {
                    boot_data <- data[indices, ]

                    old_cox <- try(coxph(old_formula, data = boot_data), silent = TRUE)
                    new_cox <- try(coxph(new_formula, data = boot_data), silent = TRUE)

                    if (inherits(old_cox, "try-error") || inherits(new_cox, "try-error")) {
                        return(NA)
                    }

                    # Calculate IDI on bootstrap sample

                    idi_t <- private$.idiTimePoint(boot_data)
                    old_prob <- private$.coxRisk(old_cox, idi_t)
                    new_prob <- private$.coxRisk(new_cox, idi_t)

                    events <- boot_data[["event_binary"]]

                    old_disc_slope <- mean(old_prob[events == 1], na.rm = TRUE) - mean(old_prob[events == 0], na.rm = TRUE)
                    new_disc_slope <- mean(new_prob[events == 1], na.rm = TRUE) - mean(new_prob[events == 0], na.rm = TRUE)

                    return(new_disc_slope - old_disc_slope)
                }

                if (requireNamespace("boot", quietly = TRUE)) {
                    boot_results <- boot::boot(
                        data = data,
                        statistic = bootstrap_idi,
                        R = private$.getBootstrapReps(500) # Limit for efficiency
                    )

                    idi_ci <- private$.safeExecute(
                        {
                            boot::boot.ci(boot_results, type = "perc")
                        },
                        errorReturn = NULL,
                        errorMessage = "Failed to calculate bootstrap CI for IDI",
                        silent = TRUE
                    )

                    return(list(
                        boot_results = boot_results,
                        idi_ci = idi_ci
                    ))
                }

                return(NULL)
            },
            .calculatePseudoR2 = function(old_cox, new_cox, data) {
                # Calculate various pseudo R-squared measures with robust error handling
                # For Cox models, we need to fit a proper null model for comparison
                # IMPORTANT: In multifactorial analysis, the null model should include covariates!


                tryCatch(
                    {
                        # Extract fitted model log-likelihoods
                        if (is.null(old_cox$loglik) || length(old_cox$loglik) < 2) {
                            private$.addNotice("WARNING", .("Pseudo R-squared not computed"), .("A Cox model did not return a usable log-likelihood, so pseudo R-squared values are shown as missing."))
                            return(list(
                                nagelkerke_old = NA, nagelkerke_new = NA, nagelkerke_improvement = NA,
                                mcfadden_old = NA, mcfadden_new = NA, mcfadden_improvement = NA,
                                cox_snell_old = NA, cox_snell_new = NA, cox_snell_improvement = NA,
                                adj_mcfadden_old = NA, adj_mcfadden_new = NA, adj_mcfadden_improvement = NA
                            ))
                        }

                        if (is.null(new_cox$loglik) || length(new_cox$loglik) < 2) {
                            private$.addNotice("WARNING", .("Pseudo R-squared not computed"), .("A Cox model did not return a usable log-likelihood, so pseudo R-squared values are shown as missing."))
                            return(list(
                                nagelkerke_old = NA, nagelkerke_new = NA, nagelkerke_improvement = NA,
                                mcfadden_old = NA, mcfadden_new = NA, mcfadden_improvement = NA,
                                cox_snell_old = NA, cox_snell_new = NA, cox_snell_improvement = NA,
                                adj_mcfadden_old = NA, adj_mcfadden_new = NA, adj_mcfadden_improvement = NA
                            ))
                        }

                        # Extract the final (fitted) log-likelihoods from each model
                        ll_fitted_old <- old_cox$loglik[2] # Final log-likelihood of old model
                        ll_fitted_new <- new_cox$loglik[2] # Final log-likelihood of new model

                        # Determine the appropriate null model based on analysis type
                        time_var <- self$options$survivalTime
                        event_var <- "event_binary"
                        survival_obj <- survival::Surv(data[[time_var]], data[[event_var]])

                        # Check if multifactorial analysis is enabled
                        if (self$options$enableMultifactorialAnalysis) {
                            # MULTIFACTORIAL ANALYSIS: Null model should include covariates
                            # This measures the incremental value of staging beyond the covariates
                            continuous_vars <- self$options$continuousCovariates
                            categorical_vars <- self$options$categoricalCovariates
                            all_covariates <- c(continuous_vars, categorical_vars)

                            if (length(all_covariates) > 0) {
                                # Build covariate-only null model
                                covariate_formula <- paste(all_covariates, collapse = " + ")
                                null_formula <- as.formula(paste("survival_obj ~", covariate_formula))

                                null_model <- tryCatch(
                                    {
                                        survival::coxph(null_formula, data = data)
                                    },
                                    error = function(e) {
                                        NULL
                                    }
                                )

                                if (!is.null(null_model) && !is.null(null_model$loglik) &&
                                    length(null_model$loglik) >= 2 && is.finite(null_model$loglik[2])) {
                                    ll_null <- null_model$loglik[2]
                                } else {
                                    # Fallback: use initial log-likelihood
                                    ll_null <- old_cox$loglik[1]
                                }
                            } else {
                                # No covariates specified - use intercept-only model
                                ll_null <- old_cox$loglik[1]
                            }
                        } else {
                            # UNIVARIATE ANALYSIS: Use intercept-only null model
                            null_model <- tryCatch(
                                {
                                    survival::coxph(survival_obj ~ 1, data = data)
                                },
                                error = function(e) {
                                    # Intercept-only Cox models often fail - this is normal
                                    NULL
                                }
                            )

                            # Extract null log-likelihood using the most robust approach
                            if (!is.null(null_model) && !is.null(null_model$loglik) &&
                                length(null_model$loglik) >= 2 && is.finite(null_model$loglik[2])) {
                                # Use the proper null model log-likelihood if available
                                ll_null <- null_model$loglik[2]
                            } else {
                                # Standard approach: use the initial log-likelihood from fitted models
                                # This represents the log-likelihood before any covariates are added
                                ll_null <- old_cox$loglik[1]

                                # Verify both models have the same initial log-likelihood (they should)
                                if (abs(old_cox$loglik[1] - new_cox$loglik[1]) > 1e-6) {
                                    private$.addNotice("WARNING", .("Model log-likelihood"), .("Old and new Cox models have different initial log-likelihoods, suggesting different baseline data."))
                                }

                            }
                        }

                        # Validate that the null log-likelihood makes sense
                        if (ll_null > ll_fitted_old || ll_null > ll_fitted_new) {
                            private$.addNotice("WARNING", .("Model log-likelihood"), .("Null model log-likelihood is greater than fitted model log-likelihood."))
                        }

                        # Debug log-likelihood values with more detail

                        # Check for valid log-likelihoods
                        if (is.na(ll_null) || is.na(ll_fitted_old) || is.na(ll_fitted_new)) {
                            private$.addNotice("WARNING", .("Pseudo R-squared not computed"), .("A Cox model did not return a usable log-likelihood, so pseudo R-squared values are shown as missing."))
                            return(list(
                                nagelkerke_old = NA, nagelkerke_new = NA, nagelkerke_improvement = NA,
                                mcfadden_old = NA, mcfadden_new = NA, mcfadden_improvement = NA,
                                cox_snell_old = NA, cox_snell_new = NA, cox_snell_improvement = NA,
                                adj_mcfadden_old = NA, adj_mcfadden_new = NA, adj_mcfadden_improvement = NA
                            ))
                        }

                        # Additional checks for finite values
                        if (!is.finite(ll_null) || !is.finite(ll_fitted_old) || !is.finite(ll_fitted_new)) {
                            private$.addNotice("WARNING", .("Pseudo R-squared not computed"), .("A Cox model did not return a usable log-likelihood, so pseudo R-squared values are shown as missing."))
                            return(list(
                                nagelkerke_old = NA, nagelkerke_new = NA, nagelkerke_improvement = NA,
                                mcfadden_old = NA, mcfadden_new = NA, mcfadden_improvement = NA,
                                cox_snell_old = NA, cox_snell_new = NA, cox_snell_improvement = NA,
                                adj_mcfadden_old = NA, adj_mcfadden_new = NA, adj_mcfadden_improvement = NA
                            ))
                        }

                        # Number of parameters
                        p_old <- length(coef(old_cox))
                        p_new <- length(coef(new_cox))
                        n <- nrow(data)

                        # Helper function for safe division
                        safe_divide <- function(numerator, denominator) {
                            if (is.na(denominator) || denominator == 0) {
                                return(NA)
                            }
                            return(numerator / denominator)
                        }

                        # Helper function for safe exponential calculations
                        safe_exp <- function(x) {
                            if (is.na(x) || !is.finite(x)) {
                                return(NA)
                            }
                            result <- exp(x)
                            if (!is.finite(result)) {
                                return(NA)
                            }
                            return(result)
                        }

                        # McFadden R-squared (using correct log-likelihood values)
                        mcfadden_old <- if (ll_null != 0) {
                            1 - safe_divide(ll_fitted_old, ll_null)
                        } else {
                            NA
                        }

                        mcfadden_new <- if (ll_null != 0) {
                            1 - safe_divide(ll_fitted_new, ll_null)
                        } else {
                            NA
                        }

                        # Cox-Snell R-squared
                        cox_snell_old <- if (n > 0) {
                            exp_term <- safe_exp((ll_null - ll_fitted_old) * 2 / n)
                            if (is.na(exp_term)) NA else 1 - exp_term
                        } else {
                            NA
                        }

                        cox_snell_new <- if (n > 0) {
                            exp_term <- safe_exp((ll_null - ll_fitted_new) * 2 / n)
                            if (is.na(exp_term)) NA else 1 - exp_term
                        } else {
                            NA
                        }

                        # Nagelkerke R-squared (normalized Cox-Snell)
                        nagelkerke_old <- if (!is.na(cox_snell_old) && n > 0) {
                            max_exp <- safe_exp(ll_null * 2 / n)
                            if (is.na(max_exp)) {
                                NA
                            } else {
                                denominator <- 1 - max_exp
                                if (denominator == 0) NA else safe_divide(cox_snell_old, denominator)
                            }
                        } else {
                            NA
                        }

                        nagelkerke_new <- if (!is.na(cox_snell_new) && n > 0) {
                            max_exp <- safe_exp(ll_null * 2 / n)
                            if (is.na(max_exp)) {
                                NA
                            } else {
                                denominator <- 1 - max_exp
                                if (denominator == 0) NA else safe_divide(cox_snell_new, denominator)
                            }
                        } else {
                            NA
                        }

                        # Adjusted McFadden R-squared (penalized)
                        adj_mcfadden_old <- if (ll_null != 0) {
                            1 - safe_divide((ll_fitted_old - p_old), ll_null)
                        } else {
                            NA
                        }

                        adj_mcfadden_new <- if (ll_null != 0) {
                            1 - safe_divide((ll_fitted_new - p_new), ll_null)
                        } else {
                            NA
                        }

                        # Calculate improvements
                        nagelkerke_improvement <- if (!is.na(nagelkerke_old) && !is.na(nagelkerke_new)) {
                            nagelkerke_new - nagelkerke_old
                        } else {
                            NA
                        }

                        mcfadden_improvement <- if (!is.na(mcfadden_old) && !is.na(mcfadden_new)) {
                            mcfadden_new - mcfadden_old
                        } else {
                            NA
                        }

                        cox_snell_improvement <- if (!is.na(cox_snell_old) && !is.na(cox_snell_new)) {
                            cox_snell_new - cox_snell_old
                        } else {
                            NA
                        }

                        adj_mcfadden_improvement <- if (!is.na(adj_mcfadden_old) && !is.na(adj_mcfadden_new)) {
                            adj_mcfadden_new - adj_mcfadden_old
                        } else {
                            NA
                        }

                        # 5. Royston & Sauerbrei R-squared (explained variation approach)
                        royston_old <- tryCatch(
                            {
                                private$.calculateRoystonR2(old_cox)
                            },
                            error = function(e) {
                                NA
                            }
                        )

                        royston_new <- tryCatch(
                            {
                                private$.calculateRoystonR2(new_cox)
                            },
                            error = function(e) {
                                NA
                            }
                        )

                        royston_improvement <- if (!is.na(royston_old) && !is.na(royston_new)) {
                            royston_new - royston_old
                        } else {
                            NA
                        }


                        result <- list(
                            nagelkerke_old = nagelkerke_old,
                            nagelkerke_new = nagelkerke_new,
                            nagelkerke_improvement = nagelkerke_improvement,
                            mcfadden_old = mcfadden_old,
                            mcfadden_new = mcfadden_new,
                            mcfadden_improvement = mcfadden_improvement,
                            cox_snell_old = cox_snell_old,
                            cox_snell_new = cox_snell_new,
                            cox_snell_improvement = cox_snell_improvement,
                            adj_mcfadden_old = adj_mcfadden_old,
                            adj_mcfadden_new = adj_mcfadden_new,
                            adj_mcfadden_improvement = adj_mcfadden_improvement,
                            royston_old = royston_old,
                            royston_new = royston_new,
                            royston_improvement = royston_improvement
                        )


                        return(result)
                    },
                    error = function(e) {
                        # If anything fails, return NA values
                        return(list(
                            nagelkerke_old = NA, nagelkerke_new = NA, nagelkerke_improvement = NA,
                            mcfadden_old = NA, mcfadden_new = NA, mcfadden_improvement = NA,
                            cox_snell_old = NA, cox_snell_new = NA, cox_snell_improvement = NA,
                            adj_mcfadden_old = NA, adj_mcfadden_new = NA, adj_mcfadden_improvement = NA,
                            royston_old = NA, royston_new = NA, royston_improvement = NA
                        ))
                    }
                )
            },
            .performHomogeneityTests = function(data) {
                # Test homogeneity within stages and trend across stages
                # This function is also needed for trend tests, so run if either option is enabled
                if (!self$options$performHomogeneityTests && !self$options$performTrendTests) {
                    return(NULL)
                }


                old_stage <- self$options$oldStage
                new_stage <- self$options$newStage
                time_var <- self$options$survivalTime
                event_var <- "event_binary"

                homogeneity_results <- list()

                # Test for old staging system
                old_formula <- stats::as.formula("survival::Surv(time_internal, event_binary) ~ stage_old")
                old_survdiff <- survival::survdiff(old_formula, data = data)

                # Overall test
                old_overall_p <- private$.survdiffP(old_survdiff)

                # Trend test (if stages are ordinal)
                old_trend_test <- NULL
                if (self$options$performTrendTests) {
                    old_trend_test <- private$.calculateTrendTest(data, old_stage, time_var, event_var)
                }

                # Within-stage homogeneity tests
                old_within_stage <- private$.calculateWithinStageHomogeneity(data, old_stage, time_var, event_var, new_stage)

                # Jonckheere-Terpstra trend test
                old_jt_test <- private$.calculateJonckheereTerpstraTest(data, old_stage, time_var, event_var)

                # Separation test
                old_separation <- private$.calculateSeparationTest(data, old_stage, time_var, event_var)

                homogeneity_results$old_staging <- list(
                    overall_test = old_survdiff,
                    overall_p = old_overall_p,
                    trend_test = old_trend_test,
                    within_stage_homogeneity = old_within_stage,
                    jonckheere_terpstra = old_jt_test,
                    separation_test = old_separation
                )

                # Test for new staging system
                new_formula <- stats::as.formula("survival::Surv(time_internal, event_binary) ~ stage_new")
                new_survdiff <- survival::survdiff(new_formula, data = data)

                new_overall_p <- private$.survdiffP(new_survdiff)

                new_trend_test <- NULL
                if (self$options$performTrendTests) {
                    new_trend_test <- private$.calculateTrendTest(data, new_stage, time_var, event_var)
                }

                # Within-stage homogeneity tests
                new_within_stage <- private$.calculateWithinStageHomogeneity(data, new_stage, time_var, event_var, old_stage)

                # Jonckheere-Terpstra trend test
                new_jt_test <- private$.calculateJonckheereTerpstraTest(data, new_stage, time_var, event_var)

                # Separation test
                new_separation <- private$.calculateSeparationTest(data, new_stage, time_var, event_var)

                homogeneity_results$new_staging <- list(
                    overall_test = new_survdiff,
                    overall_p = new_overall_p,
                    trend_test = new_trend_test,
                    within_stage_homogeneity = new_within_stage,
                    jonckheere_terpstra = new_jt_test,
                    separation_test = new_separation
                )

                return(homogeneity_results)
            },
            .calculateTrendTest = function(data, stage_var, time_var, event_var) {
                # Calculate trend test for ordinal stages

                # Equally-spaced scores from the ordered factor levels.
                #
                # The previous version stripped non-digits from the labels, which produced
                # wrong spacing on real TNM label sets:
                #   T1, T2, T3, T4a, T4b   -> 1, 2, 3, 4, 4   (T4a and T4b collapsed)
                #   Stage 1, Stage 2, Stage 10 -> 1, 2, 10    (one stage dominates the slope)
                # The score enters coxph as a metric covariate, so both distortions bias the
                # trend coefficient. Level order is the clinically meaningful ordering and is
                # what the sibling helper (stagemigration_calculateLinearTrendTest) already uses.
                stage_factor <- as.factor(data[[stage_var]])
                data$stage_numeric <- as.integer(stage_factor)

                # Fit Cox model with stage as continuous variable for trend test
                trend_formula <- stats::as.formula("survival::Surv(time_internal, event_binary) ~ stage_numeric")
                trend_cox <- try(survival::coxph(trend_formula, data = data), silent = TRUE)

                if (!inherits(trend_cox, "try-error")) {
                    trend_p <- summary(trend_cox)$coefficients[1, "Pr(>|z|)"]
                    trend_coef <- summary(trend_cox)$coefficients[1, "coef"]
                    trend_se <- summary(trend_cox)$coefficients[1, "se(coef)"]
                    trend_z <- summary(trend_cox)$coefficients[1, "z"]

                    return(list(
                        trend_p = trend_p,
                        trend_coef = trend_coef,
                        trend_se = trend_se,
                        trend_z = trend_z,
                        trend_cox = trend_cox
                    ))
                }

                return(NULL)
            },
            .calculateWithinStageHomogeneity = function(data, stage_var, time_var, event_var, split_var = NULL) {
                # Within-stage homogeneity: is there prognostic variation hiding INSIDE a
                # stage that the other staging system exposes?
                #
                # The previous implementation split each stage into quartiles OF THE OUTCOME
                # and log-rank tested those quartiles against each other. That asks whether
                # patients with short survival times have short survival times, so it rejects
                # on every dataset -- verified at chi-square 277 (p < 2e-16) on data drawn
                # from a single exponential, i.e. homogeneous by construction. Every stage was
                # therefore reported "Heterogeneous" regardless of the data.
                #
                # The valid test splits each stage of one system by the OTHER system's stage
                # assignment, which is an external, outcome-independent partition.
                tryCatch(
                    {
                        stage_levels <- levels(as.factor(data[[stage_var]]))
                        within_stage_results <- list()

                        if (is.null(split_var) || !(split_var %in% names(data))) {
                            for (stage in stage_levels) {
                                within_stage_results[[stage]] <- list(
                                    stage = stage,
                                    test_type = "Within-Stage Homogeneity",
                                    statistic = NA,
                                    p_value = NA,
                                    note = "No comparison staging system available"
                                )
                            }
                            return(within_stage_results)
                        }

                        for (stage in stage_levels) {
                            stage_data <- data[as.character(data[[stage_var]]) == stage, , drop = FALSE]

                            if (nrow(stage_data) < 10) {
                                within_stage_results[[stage]] <- list(
                                    stage = stage,
                                    test_type = "Within-Stage Homogeneity",
                                    statistic = NA,
                                    p_value = NA,
                                    note = "Insufficient patients"
                                )
                                next
                            }

                            stage_data[["within_split"]] <- droplevels(
                                as.factor(as.character(stage_data[[split_var]])))
                            n_sub <- nlevels(stage_data[["within_split"]])

                            if (n_sub < 2) {
                                within_stage_results[[stage]] <- list(
                                    stage = stage,
                                    test_type = "Within-Stage Homogeneity",
                                    statistic = NA,
                                    p_value = NA,
                                    note = "No subgroups in comparison system"
                                )
                                next
                            }

                            homog_formula <- stats::as.formula(
                                "survival::Surv(time_internal, event_binary) ~ within_split")
                            stage_data[["time_internal"]] <- stage_data[[time_var]]
                            stage_data[["event_binary"]] <- stage_data[[event_var]]

                            homog_test <- survival::survdiff(homog_formula, data = stage_data)
                            p_value <- private$.survdiffP(homog_test)

                            within_stage_results[[stage]] <- list(
                                stage = stage,
                                test_type = "Within-Stage Homogeneity",
                                statistic = homog_test$chisq,
                                p_value = p_value,
                                note = if (is.na(p_value)) {
                                    "Not estimable"
                                } else if (p_value > 0.05) {
                                    "Homogeneous"
                                } else {
                                    "Heterogeneous"
                                }
                            )
                        }

                        return(within_stage_results)
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            },
            .calculateJonckheereTerpstraTest = function(data, stage_var, time_var, event_var) {
                # Log-rank test for trend across ordered stages (Tarone 1975).
                #
                # This replaces clinfun::jonckheere.test, which was previously run on the raw
                # follow-up times with `event_var` accepted but never used -- so every censored
                # observation entered the rank statistic as a completed survival time. In a
                # stage-migration cohort with heavy censoring that is a trend test on time under
                # observation (i.e. on accrual date), not on survival. The old call also
                # hardcoded alternative = "decreasing", halving the p-value one-sidedly and
                # silently returning non-significance whenever stages were coded best-to-worst.
                #
                # The log-rank trend statistic uses survdiff's observed/expected counts and its
                # full covariance matrix, so censoring is handled correctly and the test is
                # two-sided on 1 df:
                #     Z = sum(s_i * (O_i - E_i)),  Var(Z) = s' V s,  X2 = Z^2 / Var(Z)
                tryCatch(
                    {
                        wd <- data
                        wd[["trend_time"]] <- as.numeric(wd[[time_var]])
                        wd[["trend_event"]] <- as.numeric(wd[[event_var]])
                        wd[["trend_group"]] <- droplevels(as.factor(wd[[stage_var]]))

                        keep <- stats::complete.cases(
                            wd[["trend_time"]], wd[["trend_event"]], wd[["trend_group"]])
                        wd <- wd[keep, , drop = FALSE]

                        k <- nlevels(wd[["trend_group"]])
                        if (k < 2 || nrow(wd) < 3) {
                            return(list(
                                test_type = "Log-rank trend",
                                statistic = NA,
                                p_value = NA,
                                note = if (k < 2) "Insufficient stage groups" else "Insufficient data points"
                            ))
                        }

                        sd_obj <- survival::survdiff(
                            survival::Surv(trend_time, trend_event) ~ trend_group,
                            data = wd)

                        obs <- as.numeric(sd_obj$obs)
                        exp <- if (is.matrix(sd_obj$exp)) rowSums(sd_obj$exp) else as.numeric(sd_obj$exp)
                        V <- sd_obj$var

                        # equally spaced scores in level order (clinical stage ordering)
                        scores <- seq_len(k)
                        scores <- scores - mean(scores)

                        z_num <- sum(scores * (obs - exp))
                        z_var <- as.numeric(t(scores) %*% V %*% scores)

                        if (!is.finite(z_var) || z_var <= 0) {
                            return(list(
                                test_type = "Log-rank trend",
                                statistic = NA,
                                p_value = NA,
                                note = "Trend variance not estimable"
                            ))
                        }

                        chi2 <- z_num^2 / z_var
                        p_val <- stats::pchisq(chi2, df = 1, lower.tail = FALSE)

                        list(
                            test_type = "Log-rank trend",
                            statistic = chi2,
                            p_value = p_val,
                            note = "Two-sided trend test across ordered stages, censoring-aware"
                        )
                    },
                    error = function(e) {
                        # No substitute test. A different statistic must never be returned
                        # under this name -- the previous fallback reported a Spearman
                        # correlation over k stage medians as "Jonckheere-Terpstra".
                        list(
                            test_type = "Log-rank trend",
                            statistic = NA,
                            p_value = NA,
                            note = "Trend test could not be computed"
                        )
                    }
                )
            },
            .calculateSimpleJTTest = function(data, stage_var, time_var, event_var) {
                # Simplified non-parametric trend test as fallback

                tryCatch(
                    {
                        # Prepare data
                        stage_factor <- as.factor(data[[stage_var]])
                        survival_times <- data[[time_var]]

                        # Remove missing values
                        complete_cases <- complete.cases(stage_factor, survival_times)
                        stage_clean <- stage_factor[complete_cases]
                        survival_clean <- survival_times[complete_cases]

                        if (length(unique(stage_clean)) < 2) {
                            return(list(
                                test_type = "Jonckheere-Terpstra",
                                statistic = NA,
                                p_value = NA,
                                note = "Insufficient stage groups"
                            ))
                        }

                        if (length(survival_clean) < 3) {
                            return(list(
                                test_type = "Jonckheere-Terpstra",
                                statistic = NA,
                                p_value = NA,
                                note = "Insufficient data points"
                            ))
                        }

                        # Calculate median survival for each stage
                        stage_medians <- tapply(survival_clean, stage_clean, median, na.rm = TRUE)
                        stage_counts <- table(stage_clean)

                        # Remove stages with insufficient data
                        valid_stages <- stage_counts >= 2
                        if (sum(valid_stages) < 2) {
                            return(list(
                                test_type = "Jonckheere-Terpstra",
                                statistic = NA,
                                p_value = NA,
                                note = "Insufficient valid stages"
                            ))
                        }

                        stage_medians_clean <- stage_medians[valid_stages]

                        # Convert stage names to numeric order
                        stage_order <- seq_along(stage_medians_clean)

                        # Check if we have enough stages and finite values
                        if (length(stage_medians_clean) < 2 || any(!is.finite(stage_medians_clean))) {
                            return(list(
                                test_type = "Jonckheere-Terpstra",
                                statistic = NA,
                                p_value = NA,
                                note = "Non-finite median values or insufficient stages"
                            ))
                        }

                        # Calculate Spearman correlation between stage order and median survival
                        # For TNM staging, we expect decreasing survival with higher stages
                        cor_result <- cor.test(stage_order, stage_medians_clean, method = "spearman", exact = FALSE)

                        # Extract correlation coefficient and p-value
                        rho <- as.numeric(cor_result$estimate)
                        p_value <- as.numeric(cor_result$p.value)

                        # Convert correlation to Z-score approximation for test statistic
                        n_stages <- length(stage_medians_clean)
                        if (n_stages > 3) {
                            z_score <- rho * sqrt(n_stages - 1)
                            test_statistic <- abs(z_score)
                        } else {
                            test_statistic <- abs(rho)
                        }

                        return(list(
                            test_type = "Jonckheere-Terpstra",
                            statistic = if (is.finite(test_statistic)) test_statistic else NA,
                            p_value = if (is.finite(p_value)) p_value else NA,
                            note = "Simplified trend test (Spearman correlation)"
                        ))
                    },
                    error = function(e) {
                        return(list(
                            test_type = "Jonckheere-Terpstra",
                            statistic = NA,
                            p_value = NA,
                            note = paste("Error in fallback:", e$message)
                        ))
                    }
                )
            },
            .calculateSeparationTest = function(data, stage_var, time_var, event_var) {
                # Calculate separation index between stages

                tryCatch(
                    {
                        stage_levels <- levels(as.factor(data[[stage_var]]))

                        if (length(stage_levels) < 2) {
                            return(list(
                                test_type = "Separation Test",
                                statistic = NA,
                                p_value = NA,
                                note = "Need at least 2 stages"
                            ))
                        }

                        # Calculate median survival for each stage
                        stage_medians <- numeric(length(stage_levels))
                        stage_ranges <- numeric(length(stage_levels))

                        for (i in seq_along(stage_levels)) {
                            stage_data <- data[data[[stage_var]] == stage_levels[i], ]

                            if (nrow(stage_data) > 0) {
                                survival_times <- stage_data[[time_var]][!is.na(stage_data[[time_var]])]

                                if (length(survival_times) > 0) {
                                    stage_medians[i] <- median(survival_times)
                                    stage_ranges[i] <- IQR(survival_times)
                                } else {
                                    stage_medians[i] <- NA
                                    stage_ranges[i] <- NA
                                }
                            } else {
                                stage_medians[i] <- NA
                                stage_ranges[i] <- NA
                            }
                        }

                        # Calculate separation index
                        # Separation = (range of medians) / (mean of IQRs)
                        median_range <- max(stage_medians, na.rm = TRUE) - min(stage_medians, na.rm = TRUE)
                        mean_iqr <- mean(stage_ranges, na.rm = TRUE)

                        separation_index <- if (mean_iqr > 0) {
                            median_range / mean_iqr
                        } else {
                            NA
                        }

                        # Simple test: higher separation index = better separation
                        # No p-value is reported for the separation index. The previous
                        # exp(-separation_index) transform was a deterministic function of the
                        # point estimate: no null distribution, no dependence on N, and it
                        # crossed 0.05 by construction at separation_index = 3.
                        p_value <- NA_real_

                        return(list(
                            test_type = "Separation Test",
                            statistic = separation_index,
                            p_value = p_value,
                            note = ifelse(is.na(separation_index), "Unable to calculate",
                                ifelse(separation_index > 1, "Good separation", "Poor separation")
                            )
                        ))
                    },
                    error = function(e) {
                        return(list(
                            test_type = "Separation Test",
                            statistic = NA,
                            p_value = NA,
                            note = paste("Error:", e$message)
                        ))
                    }
                )
            },
            .generateClinicalInterpretation = function(all_results) {
                # Generate comprehensive clinical interpretation
                if (!self$options$showClinicalInterpretation) {
                    return(NULL)
                }

                # Extract key metrics
                basic_results <- all_results$basic_migration
                advanced_results <- all_results$advanced_metrics
                nri_results <- all_results$nri_analysis

                # Clinical significance thresholds
                c_threshold <- self$options$clinicalSignificanceThreshold
                nri_threshold <- self$options$nriClinicalThreshold

                interpretation <- list()

                # Overall assessment
                interpretation$overall_assessment <- private$.assessOverallImprovement(
                    basic_results, advanced_results, nri_results, c_threshold, nri_threshold
                )

                # Statistical significance vs clinical importance
                interpretation$significance_assessment <- private$.assessSignificance(
                    advanced_results, c_threshold
                )

                # Sample size adequacy
                old_stage_name <- self$options$oldStage
                new_stage_name <- self$options$newStage

                interpretation$sample_adequacy <- private$.assessSampleAdequacy(
                    basic_results$total_patients, length(unique(c(
                        levels(as.factor(self$data[[old_stage_name]])),
                        levels(as.factor(self$data[[new_stage_name]]))
                    )))
                )

                # Recommendation
                interpretation$recommendation <- private$.generateRecommendation(
                    all_results, c_threshold, nri_threshold
                )

                # Cancer-type specific guidance
                if (self$options$cancerType != "general") {
                    interpretation$cancer_specific <- private$.getCancerSpecificGuidance(
                        self$options$cancerType, all_results
                    )
                }

                return(interpretation)
            },
            .assessOverallImprovement = function(basic_results, advanced_results, nri_results, c_threshold, nri_threshold) {
                # Assess overall improvement magnitude

                assessment <- list()

                # C-index improvement assessment
                c_improvement <- advanced_results$c_improvement
                c_improvement_pct <- advanced_results$c_improvement_pct

                if (abs(c_improvement) < c_threshold) {
                    assessment$c_index_magnitude <- "negligible"
                } else if (abs(c_improvement) < 2 * c_threshold) {
                    assessment$c_index_magnitude <- "small"
                } else if (abs(c_improvement) < 4 * c_threshold) {
                    assessment$c_index_magnitude <- "moderate"
                } else {
                    assessment$c_index_magnitude <- "large"
                }

                assessment$c_improvement <- c_improvement
                assessment$c_improvement_pct <- c_improvement_pct

                # NRI assessment
                if (!is.null(nri_results) && length(nri_results) > 0) {
                    # Use first time point for overall assessment
                    first_nri <- nri_results[[1]]
                    nri_overall <- first_nri$nri_overall

                    if (abs(nri_overall) < nri_threshold / 2) {
                        assessment$nri_magnitude <- "negligible"
                    } else if (abs(nri_overall) < nri_threshold) {
                        assessment$nri_magnitude <- "small"
                    } else if (abs(nri_overall) < 2 * nri_threshold) {
                        assessment$nri_magnitude <- "moderate"
                    } else {
                        assessment$nri_magnitude <- "large"
                    }

                    assessment$nri_overall <- nri_overall
                }

                # Migration assessment
                migration_rate <- basic_results$migration_rate
                if (migration_rate < 0.05) {
                    assessment$migration_magnitude <- "minimal"
                } else if (migration_rate < 0.15) {
                    assessment$migration_magnitude <- "low"
                } else if (migration_rate < 0.30) {
                    assessment$migration_magnitude <- "moderate"
                } else {
                    assessment$migration_magnitude <- "high"
                }

                assessment$migration_rate <- migration_rate

                return(assessment)
            },
            .assessSignificance = function(advanced_results, c_threshold) {
                # Assess statistical vs clinical significance
                assessment <- list()

                # --- Statistical Significance ---
                lr_p <- NA
                # Check if lr_test result exists and is valid
                tryCatch(
                    {
                        if (!is.null(advanced_results$lr_test)) {
                            # Check if it's a data frame and has the required structure
                            if (is.data.frame(advanced_results$lr_test) && nrow(advanced_results$lr_test) > 1) {
                                lr_p <- advanced_results$lr_test[2, "Pr(>Chi)"]
                            } else if (is.list(advanced_results$lr_test) && !is.null(advanced_results$lr_test$p_value)) {
                                # Handle case where lr_test is a list structure
                                lr_p <- advanced_results$lr_test$p_value
                            }
                        }
                    },
                    error = function(e) {
                        lr_p <<- NA
                    }
                )
                assessment$lr_p_value <- lr_p

                # This is the robust way to check for a single, valid p-value
                # It avoids the `&&` operator's problematic behavior with empty vectors
                stat_sig <- FALSE # Default to FALSE
                if (length(lr_p) == 1) {
                    if (!is.na(lr_p)) {
                        stat_sig <- lr_p < 0.05
                    }
                }
                assessment$statistically_significant <- stat_sig

                # --- Clinical Significance ---
                c_improvement <- tryCatch(
                    {
                        if (!is.null(advanced_results$c_improvement)) {
                            advanced_results$c_improvement
                        } else {
                            NA
                        }
                    },
                    error = function(e) {
                        NA
                    }
                )

                assessment$c_improvement <- c_improvement
                assessment$c_threshold <- c_threshold

                # Check for NA and NULL before comparison
                # TODO (correctness): direction-blind verdict. abs() makes a WORSE new system
                # "clinically significant", and lr_p (line ~3151) is "new staging adds to the
                # original", which is significant for a worse system carrying any extra factor.
                # Repro (2026-09-19, n=3000): C old 0.728 vs new 0.696 -> "RECOMMEND ADOPTION",
                # Confidence: High. Require c_improvement >= c_threshold (and the paired C-index
                # CI lower bound > 0); lr_p < 0.05 also ignores the confidenceLevel option.
                assessment$clinically_significant <- tryCatch(
                    {
                        if (!is.null(c_improvement) && length(c_improvement) == 1 && !is.na(c_improvement)) {
                            abs(c_improvement) >= c_threshold
                        } else {
                            FALSE
                        }
                    },
                    error = function(e) {
                        FALSE
                    }
                )

                # --- Combined Assessment ---
                # This block is now safe because the inputs are guaranteed to be TRUE or FALSE
                if (assessment$statistically_significant && assessment$clinically_significant) {
                    assessment$combined_significance <- "Both statistically and clinically significant"
                    assessment$recommendation_strength <- "Strong"
                } else if (assessment$statistically_significant && !assessment$clinically_significant) {
                    assessment$combined_significance <- "Statistically significant but not clinically meaningful"
                    assessment$recommendation_strength <- "Weak"
                } else if (!assessment$statistically_significant && assessment$clinically_significant) {
                    assessment$combined_significance <- "Clinically meaningful but not statistically significant"
                    assessment$recommendation_strength <- "Moderate"
                } else {
                    assessment$combined_significance <- "Neither statistically nor clinically significant"
                    assessment$recommendation_strength <- "None"
                }

                return(assessment)
            },
            .assessSampleAdequacy = function(n_patients, n_stages) {
                # Assess if sample size is adequate for staging validation

                assessment <- list()
                assessment$total_patients <- n_patients
                assessment$n_stages <- n_stages

                # Rule of thumb: at least 10 events per stage, 50 patients per stage
                min_per_stage <- 50
                recommended_total <- n_stages * min_per_stage

                assessment$recommended_minimum <- recommended_total
                assessment$adequacy_ratio <- n_patients / recommended_total

                if (n_patients < recommended_total / 2) {
                    assessment$adequacy <- "severely_inadequate"
                    assessment$adequacy_description <- "Sample size is severely inadequate for reliable staging validation"
                } else if (n_patients < recommended_total) {
                    assessment$adequacy <- "inadequate"
                    assessment$adequacy_description <- "Sample size is below recommended minimum for staging validation"
                } else if (n_patients < 2 * recommended_total) {
                    assessment$adequacy <- "adequate"
                    assessment$adequacy_description <- "Sample size is adequate for staging validation"
                } else {
                    assessment$adequacy <- "excellent"
                    assessment$adequacy_description <- "Sample size is excellent for robust staging validation"
                }

                # Power considerations
                if (n_patients >= 500) {
                    assessment$power_assessment <- "Excellent power to detect meaningful differences"
                } else if (n_patients >= 200) {
                    assessment$power_assessment <- "Good power to detect moderate to large differences"
                } else if (n_patients >= 100) {
                    assessment$power_assessment <- "Limited power; may miss small but clinically important differences"
                } else {
                    assessment$power_assessment <- "Poor power; results should be interpreted cautiously"
                }

                return(assessment)
            },
            .generateRecommendation = function(all_results, c_threshold, nri_threshold) {
                # Generate evidence-based recommendation

                basic_results <- all_results$basic_migration
                advanced_results <- all_results$advanced_metrics
                significance_assessment <- private$.assessSignificance(advanced_results, c_threshold)

                recommendation <- list()

                # Primary recommendation
                if (significance_assessment$recommendation_strength == "Strong") {
                    recommendation$primary <- "RECOMMEND ADOPTION"
                    recommendation$confidence <- "High"
                    recommendation$rationale <- "New staging system shows both statistically significant and clinically meaningful improvement in prognostic discrimination."
                } else if (significance_assessment$recommendation_strength == "Moderate") {
                    recommendation$primary <- "CONSIDER ADOPTION"
                    recommendation$confidence <- "Moderate"
                    recommendation$rationale <- "New staging system shows clinically meaningful improvement. Consider larger validation study to confirm statistical significance."
                } else if (significance_assessment$recommendation_strength == "Weak") {
                    recommendation$primary <- "INSUFFICIENT EVIDENCE"
                    recommendation$confidence <- "Low"
                    recommendation$rationale <- "While statistically significant, the improvement is too small to be clinically meaningful."
                } else {
                    # TODO (correctness): a non-significant result is reported as "DO NOT ADOPT",
                    # Confidence: High (repro: n=120, p=0.849). This branch also catches lr_p and
                    # c_improvement both NA, i.e. the tests failed. Absence of evidence: report
                    # "INCONCLUSIVE" with low confidence, and a separate row when the tests did not run.
                    recommendation$primary <- "DO NOT ADOPT"
                    recommendation$confidence <- "High"
                    recommendation$rationale <- "New staging system does not provide meaningful improvement over existing system."
                }

                # Additional considerations
                recommendation$considerations <- list()

                # Migration rate consideration
                if (basic_results$migration_rate > 0.3) {
                    recommendation$considerations$high_migration <-
                        "High migration rate may cause confusion during transition period. Plan for careful communication and training."
                }

                # Sample size consideration
                if (basic_results$total_patients < 200) {
                    recommendation$considerations$sample_size <-
                        "Small sample size limits confidence in results. Consider validation in larger cohort before implementation."
                }

                # Bootstrap validation consideration
                if (!is.null(all_results$validation_results)) {
                    optimism <- all_results$validation_results$mean_optimism
                    if (optimism > 0.01) {
                        recommendation$considerations$optimism <-
                            "Bootstrap validation suggests some optimism in apparent improvement. Adjusted estimate should be considered."
                    }
                }

                # Will Rogers phenomenon
                if (!is.null(all_results$will_rogers) && length(all_results$will_rogers) > 0) {
                    recommendation$considerations$will_rogers <-
                        "Will Rogers phenomenon detected. Ensure that migration benefits are genuine prognostic improvements."
                }

                return(recommendation)
            },
            .getCancerSpecificGuidance = function(cancer_type, all_results) {
                # Cancer-type specific interpretation guidance

                guidance <- list()

                switch(cancer_type,
                    "lung" = {
                        guidance$specific_considerations <- c(
                            "Lung cancer staging frequently updated due to rapid advances in molecular characterization",
                            "Consider impact on stage distribution for clinical trial eligibility",
                            "TNM 8th edition introduced significant changes for T descriptors",
                            "Histology-specific considerations may apply (adenocarcinoma vs. squamous)"
                        )
                        guidance$recommended_thresholds <- list(
                            c_index = 0.02,
                            nri = 0.15
                        )
                    },
                    "breast" = {
                        guidance$specific_considerations <- c(
                            "Breast cancer staging increasingly incorporates biomarker information",
                            "Consider hormone receptor and HER2 status in staging validation",
                            "Genomic assays may provide additional prognostic information",
                            "Long-term follow-up essential due to late recurrences"
                        )
                        guidance$recommended_thresholds <- list(
                            c_index = 0.025,
                            nri = 0.20
                        )
                    },
                    "colorectal" = {
                        guidance$specific_considerations <- c(
                            "Microsatellite instability status affects prognosis significantly",
                            "Location-specific differences (colon vs. rectal) should be considered",
                            "Nodal staging particularly important for treatment decisions",
                            "Consider peritoneal disease patterns in advanced stages"
                        )
                        guidance$recommended_thresholds <- list(
                            c_index = 0.02,
                            nri = 0.18
                        )
                    },
                    "prostate" = {
                        guidance$specific_considerations <- c(
                            "Gleason score integration crucial for staging validation",
                            "PSA levels provide additional prognostic information",
                            "Long natural history requires extended follow-up",
                            "Grade Group classification may affect staging interpretation"
                        )
                        guidance$recommended_thresholds <- list(
                            c_index = 0.03,
                            nri = 0.25
                        )
                    },
                    {
                        guidance$specific_considerations <- c(
                            "Consider tumor biology and natural history",
                            "Evaluate impact on treatment decision algorithms",
                            "Assess feasibility of implementation in routine practice",
                            "Consider inter-observer variability in staging assessment"
                        )
                        guidance$recommended_thresholds <- list(
                            c_index = 0.02,
                            nri = 0.20
                        )
                        guidance$is_generic <- TRUE
                    }
                )

                return(guidance)
            },
            .run = function() {
                # Main analysis execution

                # Notices are rendered on EVERY exit path (completion, the welcome-state
                # return(), and stop()), so a validation finding is never silently dropped.
                private$.noticeList <- list()
                on.exit(private$.renderNotices(), add = TRUE)
                # One seed for every resampling step (bootstrap, cross-validation, SHAP subsampling),
                # so the same data and options reproduce the same numbers.
                set.seed(private$.seedValue())

                # Check if core variables are selected
                if (is.null(self$options$oldStage) || self$options$oldStage == "" ||
                    is.null(self$options$newStage) || self$options$newStage == "" ||
                    is.null(self$options$survivalTime) || self$options$survivalTime == "" ||
                    is.null(self$options$event) || self$options$event == "") {
                    # Empty state, not an error: the welcome message explains what to select.
                    # (An "ERROR:" footnote on an empty table was alarming on first open.)
                    # Show welcome message and exit
                    welcome_html <- private$.generateWelcomeMessage()
                    self$results$welcomeMessage$setContent(welcome_html)
                    return()
                }

                # Validate option dependencies
                dep_validation <- private$.validateOptionDependencies()

                # Handle critical dependency issues
                if (dep_validation$has_issues) {
                    error_messages <- sapply(dep_validation$issues, function(issue) issue$message)
                    stop(paste("Option dependency errors:", paste(error_messages, collapse = "; ")))
                }

                # Show warnings for dependency issues
                if (dep_validation$has_warnings) {
                    dep_msgs <- vapply(dep_validation$warnings, function(w) as.character(w$message %||% ""), character(1))
                    dep_msgs <- dep_msgs[nzchar(dep_msgs)]
                    if (length(dep_msgs) > 0) {
                        private$.addNotice("WARNING", .("Option settings"), paste(dep_msgs, collapse = " \u{2022} "))
                    }
                }

                # ENHANCED FUNCTIONALITY - Apply clinical preset and guided mode
                private$.showProgressIndicator("Initializing analysis configuration", 1, 5)


                # Validate and prepare data
                private$.showProgressIndicator("Validating and preparing data", 2, 5)
                data <- private$.validateData()

                # Clinical safety validation: Check event count for survival analysis adequacy
                total_events <- sum(data[["event_binary"]], na.rm = TRUE)
                total_n <- nrow(data)

                # Event-count adequacy is reported once, by .validateData() via
                # stagemigration_checkSampleSize(), which is analysisType-aware (50 events for
                # standard, 100 for comprehensive) and also checks events-per-variable and
                # event-rate extremes. The fixed <10/<20/<50 footnotes that used to sit here
                # duplicated that finding, disagreed with it under "comprehensive", and lived
                # on migrationOverview -- so switching that table off hid the warning.

                # Apply memory optimization for large datasets
                data <- private$.optimizeMemoryUsage(data)

                # Perform analyses based on selected scope
                private$.showProgressIndicator("Running statistical analysis", 3, 5)
                all_results <- list()
                analysisType <- self$options$analysisType

                # Basic migration analysis (always performed)
                all_results$basic_migration <- private$.calculateBasicMigration(data)

                # Advanced metrics
                all_results$advanced_metrics <- private$.calculateAdvancedMetrics(data)

                # Proportional-hazards check on the primary Cox models (Schoenfeld global test).
                # The only PH test previously lived inside advancedMigrationAnalysis, behind a silent
                # tryCatch, so the default analysis reported Cox-based C-index, NRI and IDI without ever
                # checking the assumption they rest on. This reuses the models fitted just above.
                am <- all_results$advanced_metrics
                if (!is.null(am) && !is.null(am$old_cox) && !is.null(am$new_cox)) {
                    ph <- Filter(Negate(is.null), list(
                        private$.performSchoenfeld(am$old_cox, "Original staging"),
                        private$.performSchoenfeld(am$new_cox, "New staging")))
                    ph <- Filter(function(x) length(x$p_value) == 1 && is.finite(x$p_value) && x$p_value <= 0.05, ph)
                    if (length(ph) > 0) {
                        worst <- min(vapply(ph, function(x) x$p_value, numeric(1)))
                        detail <- vapply(ph, function(x) sprintf("%s: chi-square = %.2f (df = %g), %s",
                            x$variable, x$chi_square, x$df, private$.pText(x$p_value)), character(1))
                        private$.addNotice(
                            if (worst <= 0.01) "STRONG_WARNING" else "WARNING",
                            .("Proportional hazards assumption"),
                            paste0("The Schoenfeld global test suggests non-proportional hazards (", paste(detail, collapse = "; "),
                                   "). Hazard ratios, C-index, NRI and IDI from these Cox models assume proportional hazards; consider a stratified or time-varying Cox model, or RMST, which does not require the assumption."))

                        ph_tbl_note <- .("Note: Proportional hazards assumption may be violated (Schoenfeld p \u{2264} 0.05). Hazard ratios and concordance measures reflect average effects over follow-up; evaluate Time-Dependent ROC or Landmark Analysis.")
                        if (!is.null(self$results$concordanceComparison)) {
                            tryCatch(self$results$concordanceComparison$setNote("ph_violation", ph_tbl_note), error = function(e) NULL)
                        }
                        if (!is.null(self$results$statisticalComparison)) {
                            tryCatch(self$results$statisticalComparison$setNote("ph_violation", ph_tbl_note), error = function(e) NULL)
                        }
                    }
                }

                # Cross-validation (independent of other analysis options)
                if (self$options$performCrossValidation) {
                    tryCatch(
                        {
                            private$.performCrossValidation(data, all_results)
                        },
                        error = function(e) {}
                    )

                    # Add explanatory text for cross-validation
                    if (isTRUE(self$options$showExplanations)) {
                        cv_folds <- if (is.null(self$options$cvFolds)) 5 else self$options$cvFolds
                        institution_col <- self$options$institutionVariable
                        is_multi_institutional <- !is.null(institution_col) && institution_col != ""

                        if (is_multi_institutional) {
                            cv_explanation_html <- paste0(
                                '<div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #1565c0; color: inherit;">
                            <h4 style="margin-top: 0; color: inherit;">Understanding Multi-Institutional Validation Results</h4>
                            <p style="margin-bottom: 10px;">Multi-institutional validation provides the strongest evidence for staging system generalizability by testing across different medical centers:</p>

                            <div style="margin-bottom: 15px;">
                                <h5 style="color: #1976d2; margin-bottom: 8px;">Internal-External Validation Methodology:</h5>
                                <ul style="margin-left: 20px;">
                                    <li><strong>Data Splitting:</strong> Each institution serves as an independent test set</li>
                                    <li><strong>Train/Test Cycle:</strong> Train on all other institutions, test on target institution</li>
                                    <li><strong>Center Effects:</strong> Accounts for institutional variations in patient populations</li>
                                    <li><strong>External Validity:</strong> Each test represents true external validation</li>
                                    <li><strong>Heterogeneity Assessment:</strong> Performance variability indicates generalizability</li>
                                </ul>
                            </div>

                            <div style="margin-bottom: 15px;">
                                <h5 style="color: #388e3c; margin-bottom: 8px;">Multi-Institutional vs K-Fold Validation:</h5>
                                <ul style="margin-left: 20px;">
                                    <li><strong>Multi-Institutional:</strong> Tests across different healthcare systems and populations</li>
                                    <li><strong>K-Fold:</strong> Tests random data splits within same population</li>
                                    <li><strong>Clinical Relevance:</strong> Multi-institutional better reflects real-world implementation</li>
                                    <li><strong>Geographic Diversity:</strong> Different centers may have different patient characteristics</li>
                                </ul>
                            </div>'
                            )
                        } else {
                            cv_explanation_html <- paste0(
                                '<div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #1565c0; color: inherit;">
                            <h4 style="margin-top: 0; color: inherit;">Understanding Cross-Validation Results</h4>
                            <p style="margin-bottom: 10px;">Cross-validation assesses model generalizability by testing performance on independent data splits:</p>

                            <div style="margin-bottom: 15px;">
                                <h5 style="color: #1976d2; margin-bottom: 8px;">K-Fold Methodology:</h5>
                                <ul style="margin-left: 20px;">
                                    <li><strong>Data Splitting:</strong> Dataset divided into ', cv_folds, " equal parts (folds)</li>
                                    <li><strong>Train/Test Cycle:</strong> Train on ", (cv_folds - 1), " folds, test on 1 remaining fold</li>
                                    <li><strong>Repeated Process:</strong> Each fold serves as test set exactly once</li>
                                    <li><strong>Performance Aggregation:</strong> Results averaged across all folds</li>
                                    <li><strong>Statistical Testing:</strong> Paired t-test across fold improvements</li>
                                </ul>
                            </div>"
                            )
                        }

                        # Complete the HTML structure
                        cv_explanation_html <- paste0(cv_explanation_html, "</div>")

                        # Clean up HTML entities
                        cv_explanation_html <- gsub("< 0.05", "&lt; 0.05", cv_explanation_html)
                        cv_explanation_html <- gsub("> 0.02", "&gt; 0.02", cv_explanation_html)

                        self$results$crossValidationExplanation$setContent(cv_explanation_html)
                    }
                }

                # Optional advanced analyses based on analysis type
                isStandard <- analysisType %in% c("standard", "comprehensive", "publication")
                isComprehensive <- analysisType %in% c("comprehensive", "publication")

                # Check for analysis type mismatches and inform users
                if (!isStandard && (self$options$calculateNRI || self$options$calculateIDI || self$options$performROCAnalysis)) {
                    private$.addNotice("INFO", .("Analysis type"), jmvcore::format(.("NRI, IDI and time-dependent ROC analysis need the 'standard' analysis type or higher. The current type is '{analysisType}', so these outputs were not computed."), analysisType = analysisType))
                }

                if (!isComprehensive && (self$options$performDCA || self$options$performBootstrap)) {
                    private$.addNotice("INFO", .("Analysis type"), jmvcore::format(.("Decision curve analysis and bootstrap validation need the 'comprehensive' or 'publication' analysis type. The current type is '{analysisType}', so these outputs were not computed."), analysisType = analysisType))
                }

                # NRI analysis (requires standard+ analysis type and Cox models)
                if (isStandard && self$options$calculateNRI) {
                    nri_result <- private$.calculateNRI(data)
                    if (!is.null(nri_result) && !is.null(nri_result$error)) {
                        private$.addNotice("WARNING", .("NRI analysis failed"), nri_result$error)
                    } else {
                        all_results$nri_analysis <- nri_result
                    }
                }

                # IDI analysis (requires standard+ analysis type and Cox models)
                if (isStandard && self$options$calculateIDI) {
                    idi_result <- private$.calculateIDI(data)
                    if (!is.null(idi_result) && !is.null(idi_result$error)) {
                        private$.addNotice("WARNING", .("IDI analysis failed"), idi_result$error)
                    } else {
                        all_results$idi_analysis <- idi_result
                    }
                }

                # ROC analysis (requires standard+ analysis type and Cox models)
                if (isStandard && self$options$performROCAnalysis) {
                    roc_result <- private$.performTimeROCAnalysis(data)
                    if (!is.null(roc_result) && !is.null(roc_result$error)) {
                        private$.addNotice("WARNING", .("ROC analysis failed"), roc_result$error)
                    } else {
                        all_results$roc_analysis <- roc_result
                    }
                }

                # Calibration analysis (any analysis type, requires Cox models)
                if (self$options$performCalibration) {
                    if (!is.null(all_results$advanced_metrics) &&
                        !is.null(all_results$advanced_metrics$old_cox) &&
                        !is.null(all_results$advanced_metrics$new_cox)) {
                        all_results$calibration_analysis <- private$.performCalibrationAnalysis(data, all_results$advanced_metrics)
                    } else {
                        private$.addNotice("WARNING", .("Calibration not computed"), .("The Cox models needed for calibration could not be fitted."))
                    }
                }

                # DCA analysis (requires comprehensive+ analysis type and Cox models)
                if (isComprehensive && self$options$performDCA) {
                    dca_result <- private$.performDCA(data)
                    if (!is.null(dca_result) && !is.null(dca_result$error)) {
                        private$.addNotice("WARNING", .("Decision curve analysis failed"), dca_result$error)
                    } else {
                        all_results$dca_analysis <- dca_result
                    }
                }

                # Bootstrap validation (requires comprehensive+ analysis type)
                if (isComprehensive && self$options$performBootstrap) {
                    bootstrap_result <- private$.performLegacyBootstrapValidation(data)
                    if (!is.null(bootstrap_result) && !is.null(bootstrap_result$error)) {
                        private$.addNotice("WARNING", .("Bootstrap validation failed"), bootstrap_result$error)
                    } else {
                        all_results$validation_results <- bootstrap_result
                    }
                }

                # Calculate homogeneity tests if requested OR if trend tests are enabled
                # This ensures users can get homogeneity tests even with basic/standard analysis
                # Trend tests require the same underlying calculations as homogeneity tests
                if (self$options$performHomogeneityTests || self$options$performTrendTests) {
                    # Debug message removed
                    all_results$homogeneity_tests <- private$.performHomogeneityTests(data)
                }

                # Will Rogers analysis (handled by existing analysis functions)

                # Multifactorial analysis (requires covariates available in data)
                if (self$options$enableMultifactorialAnalysis) {
                    continuous_vars <- self$options$continuousCovariates
                    categorical_vars <- self$options$categoricalCovariates

                    # Check if covariates exist in the validated data
                    available_continuous <- if (length(continuous_vars) > 0) {
                        intersect(continuous_vars, names(data))
                    } else {
                        character(0)
                    }

                    available_categorical <- if (length(categorical_vars) > 0) {
                        intersect(categorical_vars, names(data))
                    } else {
                        character(0)
                    }

                    if (length(available_continuous) > 0 || length(available_categorical) > 0) {
                        multifactorial_result <- private$.performMultifactorialAnalysis(data)
                        if (!is.null(multifactorial_result) && !is.null(multifactorial_result$error)) {
                            private$.addNotice("WARNING", .("Multifactorial analysis failed"), multifactorial_result$error)
                        } else {
                            all_results$multifactorial_analysis <- multifactorial_result
                        }
                    } else {
                        private$.addNotice("WARNING", .("Multifactorial analysis not run"), .("No covariates were found in the data."))
                    }
                } else if (self$options$performInteractionTests) {
                    # Create interaction tests even when multifactorial analysis is disabled
                    all_results$multifactorial_analysis <- private$.performInteractionTestsOnly(data)
                }

                # Generate clinical interpretation
                if (self$options$showClinicalInterpretation) {
                    all_results$clinical_interpretation <- private$.generateClinicalInterpretation(all_results)
                }

                # Populate results tables and plots
                private$.showProgressIndicator("Populating results", 4, 5)
                private$.populateResults(all_results, data)

                # ENHANCED FUNCTIONALITY - Generate copy-ready clinical report
                if (self$options$generateCopyReadyReport) {
                    private$.showProgressIndicator("Generating copy-ready clinical summary", 5, 5)
                    copy_ready_report <- private$.generateCopyReadyReport(all_results)
                    if (!is.null(copy_ready_report)) {
                        self$results$copyReadyReport$setContent(copy_ready_report)
                    }
                }

                # Final guided mode completion
                if (self$options$enableGuidedMode) {
                    completion_html <- private$.generateGuidedModeProgress(5, 5)
                    self$results$guidedModeProgress$setContent(completion_html)
                }

                private$.addNotice(
                    "INFO", .("Analysis complete"),
                    jmvcore::format(.("Stage migration analysis completed for {totalN} patients with {totalEvents} events. Review the statistical comparisons and clinical interpretation below."), totalN = sprintf("%d", as.integer(total_n)), totalEvents = sprintf("%d", as.integer(total_events))))
            }
        )
    )
}
