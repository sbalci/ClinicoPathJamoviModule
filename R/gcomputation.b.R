#' @title G-computation (Parametric G-formula)
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats glm gaussian binomial predict as.formula quantile
#' @export

gcomputationClass <- R6::R6Class(
    "gcomputationClass",
    inherit = gcomputationBase,
    private = list(

        .init = function() {
            todo <- glue::glue(
                "<h3>G-computation (Parametric G-formula)</h3>
                <p>Estimates the marginal causal effect of a binary treatment by
                standardization. An outcome model is fitted on treatment and covariates;
                the model then predicts every subject's outcome <i>as if treated</i> and
                <i>as if untreated</i>, and these predictions are averaged over the
                covariate distribution to give E[Y<sup>1</sup>] and E[Y<sup>0</sup>].</p>
                <p><b>Provide:</b> an <b>outcome</b>, a binary <b>treatment</b>, and the
                <b>covariates</b> (measured confounders) to adjust for. The average
                treatment effect is reported as a difference (and risk ratio for binary
                outcomes) with a percentile bootstrap confidence interval.</p>"
            )
            self$results$todo$setContent(todo)
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$outcome) || is.null(opt$treatment) ||
                is.null(opt$covariates) || length(opt$covariates) == 0)
                return()

            prep <- private$.prepareData()
            if (is.null(prep)) return()

            est <- private$.gcompute(prep$df, prep$binary)
            if (is.null(est)) return()

            ci <- private$.bootstrap(prep$df, prep$binary)

            private$.populateMain(est, ci, prep$binary)
            if (opt$showCounterfactual)
                private$.populateCounterfactual(est)
            if (opt$showPlot)
                self$results$plot$setState(list(est = est, binary = prep$binary))
            if (opt$showSummary)
                private$.populateSummary(est, ci, prep$binary)
            if (opt$showExplanation)
                private$.populateExplanation()
        },

        .prepareData = function() {
            opt <- self$options
            binary <- opt$outcomeType == "binary"

            y <- self$data[[opt$outcome]]
            if (binary) {
                if (is.factor(y) || is.character(y)) {
                    ev <- if (!is.null(opt$outcomeEvent) && opt$outcomeEvent != "")
                              opt$outcomeEvent else levels(as.factor(y))[nlevels(as.factor(y))]
                    y <- as.integer(as.character(y) == ev)
                } else {
                    y <- as.integer(jmvcore::toNumeric(y) > 0)
                }
            } else {
                y <- jmvcore::toNumeric(y)
            }

            a <- self$data[[opt$treatment]]
            if (is.factor(a) || is.character(a)) {
                tl <- if (!is.null(opt$treatmentLevel) && opt$treatmentLevel != "")
                          opt$treatmentLevel else levels(as.factor(a))[nlevels(as.factor(a))]
                a <- as.integer(as.character(a) == tl)
            } else {
                a <- as.integer(jmvcore::toNumeric(a) > 0)
            }

            df <- data.frame(.y = y, .a = a)
            for (cv in opt$covariates) {
                col <- self$data[[cv]]
                if (is.character(col)) col <- as.factor(col)
                df[[cv]] <- col
            }
            df <- df[stats::complete.cases(df), , drop = FALSE]
            if (nrow(df) < 20 || length(unique(df$.a)) != 2) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>Need at least 20 complete cases with both
                    treatment groups present.</p>")
                return(NULL)
            }
            list(df = df, binary = binary)
        },

        .formula = function() {
            opt <- self$options
            cv <- opt$covariates
            if (opt$interactions)
                rhs <- paste0(".a * (", paste(cv, collapse = " + "), ")")
            else
                rhs <- paste0(".a + ", paste(cv, collapse = " + "))
            stats::as.formula(paste0(".y ~ ", rhs))
        },

        .gcompute = function(df, binary) {
            fam <- if (binary) stats::binomial() else stats::gaussian()
            m <- tryCatch(stats::glm(private$.formula(), data = df, family = fam),
                          error = function(e) NULL)
            if (is.null(m)) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>The outcome model failed to fit.</p>")
                return(NULL)
            }
            d1 <- df; d1$.a <- 1L
            d0 <- df; d0$.a <- 0L
            ey1 <- mean(stats::predict(m, newdata = d1, type = "response"))
            ey0 <- mean(stats::predict(m, newdata = d0, type = "response"))
            list(ey1 = ey1, ey0 = ey0, diff = ey1 - ey0,
                 ratio = if (binary) ey1 / ey0 else NA_real_)
        },

        .bootstrap = function(df, binary) {
            B <- self$options$bootstrap_n
            fam <- if (binary) stats::binomial() else stats::gaussian()
            fml <- private$.formula()
            diffs <- numeric(0); ratios <- numeric(0)
            for (b in seq_len(B)) {
                idx <- sample(nrow(df), replace = TRUE)
                dfi <- df[idx, , drop = FALSE]
                m <- tryCatch(suppressWarnings(stats::glm(fml, data = dfi, family = fam)),
                              error = function(e) NULL)
                if (is.null(m)) next
                d1 <- dfi; d1$.a <- 1L; d0 <- dfi; d0$.a <- 0L
                e1 <- mean(stats::predict(m, newdata = d1, type = "response"))
                e0 <- mean(stats::predict(m, newdata = d0, type = "response"))
                diffs <- c(diffs, e1 - e0)
                if (binary) ratios <- c(ratios, e1 / e0)
            }
            conf <- self$options$conf_level
            a <- (1 - conf) / 2
            qd <- if (length(diffs) >= 10)
                stats::quantile(diffs, c(a, 1 - a), names = FALSE) else c(NA, NA)
            qr <- if (binary && length(ratios) >= 10)
                stats::quantile(ratios, c(a, 1 - a), names = FALSE) else c(NA, NA)
            list(diff = qd, ratio = qr)
        },

        .populateMain = function(est, ci, binary) {
            tab <- self$results$mainTable
            tab$addRow(rowKey = "diff", values = list(
                measure = if (binary) "Risk difference (ATE)" else "Mean difference (ATE)",
                estimate = est$diff, ci_lower = ci$diff[1], ci_upper = ci$diff[2]))
            if (binary)
                tab$addRow(rowKey = "ratio", values = list(
                    measure = "Risk ratio", estimate = est$ratio,
                    ci_lower = ci$ratio[1], ci_upper = ci$ratio[2]))
            tab$setNote("diff", sprintf(
                "Marginal effect standardized over the covariate distribution; %d-replicate percentile bootstrap CI.",
                self$options$bootstrap_n))
        },

        .populateCounterfactual = function(est) {
            tab <- self$results$counterfactualTable
            tab$addRow(rowKey = "y1", values = list(
                scenario = "E[Y | all treated]", mean = est$ey1))
            tab$addRow(rowKey = "y0", values = list(
                scenario = "E[Y | all untreated]", mean = est$ey0))
        },

        .populateSummary = function(est, ci, binary) {
            ciTxt <- if (!is.na(ci$diff[1]))
                sprintf(" (95%% CI %.3f to %.3f)", ci$diff[1], ci$diff[2]) else ""
            extra <- if (binary && !is.na(est$ratio))
                sprintf(" The standardized risk ratio is %.2f.", est$ratio) else ""
            html <- glue::glue(
                "<p>If the whole population were treated, the standardized mean outcome
                would be <b>{sprintf('%.3f', est$ey1)}</b>; if untreated,
                <b>{sprintf('%.3f', est$ey0)}</b>. The average treatment effect is
                <b>{sprintf('%.3f', est$diff)}</b>{ciTxt}.{extra} This standardization
                adjusts for the measured covariates.</p>")
            self$results$summary$setContent(html)
        },

        .populateExplanation = function() {
            self$results$explanation$setContent(
                "<h4>The parametric g-formula</h4>
                <p>G-computation estimates a marginal causal effect by standardization
                (Robins, 1986; Hern&aacute;n &amp; Robins, 2020). An outcome regression is
                fitted conditional on treatment A and covariates L. Using the fitted model,
                each subject's outcome is predicted twice - once setting A = 1 and once
                setting A = 0 - and the predictions are averaged over the observed
                covariate distribution to estimate the counterfactual means
                E[Y<sup>1</sup>] and E[Y<sup>0</sup>].</p>
                <p>The average treatment effect is their difference (a risk difference for
                binary outcomes, with a risk ratio also reported). Confidence intervals use
                a nonparametric percentile bootstrap that resamples subjects and refits the
                model each replicate.</p>
                <p>Validity requires no unmeasured confounding given L, correct model
                specification, and positivity (both treatment levels possible at every
                covariate pattern).</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            est <- st$est
            df <- data.frame(
                scenario = factor(c("All untreated", "All treated"),
                                  levels = c("All untreated", "All treated")),
                mean = c(est$ey0, est$ey1))
            p <- ggplot2::ggplot(df, ggplot2::aes(x = scenario, y = mean,
                                                  fill = scenario)) +
                ggplot2::geom_col(width = 0.6) +
                ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", mean)),
                                   vjust = -0.4, size = 3.5) +
                ggplot2::scale_fill_manual(values = c(`All untreated` = "#bdbdbd",
                                                      `All treated` = "#2c7fb8"),
                                           guide = "none") +
                ggplot2::labs(x = NULL, y = "Standardized mean outcome",
                              title = sprintf("Average treatment effect = %.3f", est$diff)) +
                ggtheme
            print(p)
            TRUE
        }
    )
)
