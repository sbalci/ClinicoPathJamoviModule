#' @title E-value for Unmeasured Confounding
#' @importFrom R6 R6Class
#' @import jmvcore
#' @export

evalueClass <- R6::R6Class(
    "evalueClass",
    inherit = evalueBase,
    private = list(

        .init = function() {
            todo <- glue::glue(
                "<h3>E-value for Unmeasured Confounding</h3>
                <p>The <b>E-value</b> is the minimum strength of association (on the risk-
                ratio scale) that an unmeasured confounder would need to have with
                <i>both</i> the exposure and the outcome - beyond the measured covariates -
                to fully explain away an observed association (VanderWeele &amp; Ding,
                2017). Larger E-values indicate results more robust to unmeasured
                confounding.</p>
                <p><b>Enter:</b> the effect measure, the point estimate, and (optionally)
                its confidence limits. Odds and hazard ratios are converted to an
                approximate risk-ratio scale first. An E-value is reported for the point
                estimate and for the confidence limit closest to the null.</p>"
            )
            self$results$todo$setContent(todo)
        },

        .run = function() {
            opt <- self$options
            est <- opt$estimate
            if (is.null(est) || is.na(est)) return()

            rr <- private$.toRR(est, opt$effectType, opt$rare, opt$trueValue)
            if (is.null(rr)) return()

            ev_point <- private$.evalue(rr)

            # CI limit closest to the null (1 on RR scale)
            ev_ci <- NA_real_; rr_ci <- NA_real_; ci_used <- NA_real_
            haveCI <- !(opt$ci_lower == 0 && opt$ci_upper == 0) &&
                      !is.na(opt$ci_lower) && !is.na(opt$ci_upper)
            if (haveCI) {
                lo <- private$.toRR(opt$ci_lower, opt$effectType, opt$rare, opt$trueValue)
                hi <- private$.toRR(opt$ci_upper, opt$effectType, opt$rare, opt$trueValue)
                # if the CI crosses the null, E-value for CI is 1 (already compatible with null)
                if (lo <= 1 && hi >= 1) {
                    ev_ci <- 1; rr_ci <- 1; ci_used <- 1
                } else if (rr > 1) {
                    rr_ci <- lo; ci_used <- opt$ci_lower; ev_ci <- private$.evalue(lo)
                } else {
                    rr_ci <- hi; ci_used <- opt$ci_upper; ev_ci <- private$.evalue(hi)
                }
            }

            private$.populateMain(rr, ev_point, rr_ci, ev_ci, haveCI, ci_used)
            if (opt$showPlot)
                self$results$plot$setState(list(rr = rr, ev = ev_point))
            if (opt$showSummary)
                private$.populateSummary(rr, ev_point, ev_ci, haveCI)
            if (opt$showExplanation)
                private$.populateExplanation()
        },

        # ---- convert effect measure to risk-ratio scale --------------------
        .toRR = function(x, type, rare, trueValue) {
            if (is.na(x)) return(NULL)
            if (type %in% c("RR", "OR", "HR") && x <= 0) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>Ratio measures must be greater than zero.</p>")
                return(NULL)
            }
            # express relative to the true (null) value for ratio measures
            if (type %in% c("RR", "OR", "HR") && trueValue > 0 && trueValue != 1)
                x <- x / trueValue

            rr <- switch(type,
                RR = x,
                # Ding & VanderWeele OR->RR: sqrt(OR) if common, OR if rare
                OR = if (rare) x else sqrt(x),
                # HR->RR (VanderWeele 2017)
                HR = if (rare) x else (1 - 0.5^sqrt(x)) / (1 - 0.5^sqrt(1 / x)),
                # SMD (Cohen's d) -> approximate RR (Chinn 2000: OR = exp(1.81*d)), common-outcome
                SMD = {
                    or <- exp(1.81 * x)
                    sqrt(or)
                })
            rr
        },

        .evalue = function(rr) {
            if (is.na(rr)) return(NA_real_)
            # map protective effects to > 1 scale
            if (rr < 1) rr <- 1 / rr
            if (rr <= 1) return(1)
            rr + sqrt(rr * (rr - 1))
        },

        .populateMain = function(rr, ev_point, rr_ci, ev_ci, haveCI, ci_used) {
            tab <- self$results$mainTable
            tab$addRow(rowKey = "point", values = list(
                quantity = "Point estimate", rr_scale = rr, evalue = ev_point))
            if (haveCI) {
                lbl <- if (!is.na(ci_used) && ci_used == 1)
                    "Confidence interval (crosses null)"
                else
                    "CI limit closest to null"
                tab$addRow(rowKey = "ci", values = list(
                    quantity = lbl, rr_scale = rr_ci, evalue = ev_ci))
            }
            tab$setNote("point",
                "E-value: minimum confounder association (risk-ratio scale) with both
                exposure and outcome needed to explain away the estimate.")
        },

        .populateSummary = function(rr, ev_point, ev_ci, haveCI) {
            ciTxt <- if (haveCI && !is.na(ev_ci)) {
                if (ev_ci == 1)
                    " The confidence interval already includes the null, so no unmeasured
                     confounding is required to be compatible with no effect (E-value = 1)."
                else
                    glue::glue(" To shift the confidence limit to the null, a confounder
                     associated with both exposure and outcome by a risk ratio of
                     <b>{sprintf('%.2f', ev_ci)}</b> each would be required.")
            } else ""
            html <- glue::glue(
                "<p>On the risk-ratio scale the estimate is {sprintf('%.2f', rr)}. An
                unmeasured confounder would need to be associated with both the exposure
                and the outcome by a risk ratio of at least <b>{sprintf('%.2f', ev_point)}</b>
                each (beyond the measured covariates) to fully explain away the observed
                association.{ciTxt}</p>")
            self$results$summary$setContent(html)
        },

        .populateExplanation = function() {
            self$results$explanation$setContent(
                "<h4>E-value methodology</h4>
                <p>For a risk ratio RR \u{2265} 1, the E-value is
                <i>RR + sqrt(RR \u{00D7} (RR \u{2212} 1))</i>; for RR &lt; 1 the formula is
                applied to 1/RR (VanderWeele &amp; Ding, 2017). It is the smallest value
                such that a confounder associated with both exposure and outcome by that
                risk ratio (on both) could reduce the observed association to the null.</p>
                <p>Odds ratios and hazard ratios are first converted to an approximate risk
                ratio: for a common outcome OR is converted as sqrt(OR); hazard ratios use
                VanderWeele's approximation; a rare outcome allows OR and HR to approximate
                RR directly. Standardized mean differences are mapped via
                OR = exp(1.81 \u{00D7} d) (Chinn, 2000).</p>
                <p>An E-value near 1 indicates the association could be explained by weak
                confounding; a large E-value indicates that only a strong unmeasured
                confounder could account for it.</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            rr <- st$rr; ev <- st$ev
            if (is.na(ev) || ev <= 1) return(FALSE)
            rrX <- if (rr < 1) 1 / rr else rr
            # bounding curve: for confounder-outcome association RR_UD (>= point),
            # required exposure-confounder association RR_EU to explain away.
            # RR_EU = (RR_UD * (RRobs - 1) ... ) inverse of the joint bound:
            # RRobs >= (RR_EU*RR_UD)/(RR_EU+RR_UD-1)  => solve for RR_EU given RR_UD
            g <- seq(rrX, rrX * 4, length.out = 200)
            # RR_EU as a function of RR_UD on the bounding curve
            eu <- (rrX * (g - 1)) / (g - rrX)
            eu[g <= rrX] <- NA
            df <- data.frame(RR_UD = g, RR_EU = eu)
            df <- df[is.finite(df$RR_EU) & df$RR_EU > 0, ]
            p <- ggplot2::ggplot(df, ggplot2::aes(x = RR_UD, y = RR_EU)) +
                ggplot2::geom_line(colour = "#2c7fb8", linewidth = 1) +
                ggplot2::geom_point(data = data.frame(x = ev, y = ev),
                                    ggplot2::aes(x = x, y = y),
                                    colour = "#de2d26", size = 3) +
                ggplot2::annotate("text", x = ev, y = ev,
                                  label = sprintf("E-value = %.2f", ev),
                                  hjust = -0.1, vjust = -0.5, colour = "#de2d26",
                                  size = 3.5) +
                ggplot2::labs(
                    x = "Confounder-outcome risk ratio",
                    y = "Exposure-confounder risk ratio",
                    title = "Confounder associations that would explain away the estimate") +
                ggtheme
            print(p)
            TRUE
        }
    )
)
