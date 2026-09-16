# Competing-risks helper used only by survival.b.R.

#' Message shown when an output is unavailable in competing-risks mode
#'
#' `survival::Surv()` does not reject a 0/1/2 status vector. It emits only a
#' warning and remaps 1 to censored, 2 to event and 0 to `NA`, which jamovi
#' never surfaces. Any output that cannot handle competing risks must therefore
#' be blocked explicitly and say why, rather than silently rendering inverted
#' results.
#'
#' @param feature Display name of the output being blocked.
#' @return A character string.
#' @keywords internal
.competingRiskUnavailable <- function(feature, self = NULL) {
    if (is.null(self)) {
        paste0(feature, " is not available for competing-risks analysis. It assumes a ",
               "single event type, and the competing-risk outcome is coded 0/1/2. Use the ",
               "cumulative incidence output instead, or set survival type to Overall or ",
               "Cause Specific.")
    } else {
        jmvcore::format(.("{feature} is not available for competing-risks analysis. It assumes a single event type, and the competing-risk outcome is coded 0/1/2. Use the cumulative incidence output instead, or set survival type to Overall or Cause Specific."), feature = feature)
    }
}
