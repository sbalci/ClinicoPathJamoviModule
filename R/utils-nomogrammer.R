#' @title Fagan Nomogram for Diagnostic Test Analysis
#' @description Creates Fagan nomograms for Bayesian analysis in diagnostic testing.
#'   A Fagan nomogram is a graphical tool used to estimate post-test probabilities
#'   from pre-test probabilities and likelihood ratios. This function supports
#'   input via sensitivity/specificity or directly via likelihood ratios.
#'
#' @param Prevalence Prior probability (prevalence) as a number between 0 and 1.
#'   This represents the probability of disease before the test is performed.
#' @param Sens Model sensitivity as a number between 0 and 1. The probability
#'   that the test is positive when the disease is present. Optional if Plr/Nlr provided.
#' @param Spec Model specificity as a number between 0 and 1. The probability
#'   that the test is negative when the disease is absent. Optional if Plr/Nlr provided.
#' @param Plr Positive likelihood ratio (calculated from Sens and Spec if not provided).
#'   Must be >= 1. If provided along with Nlr, takes precedence over Sens/Spec.
#' @param Nlr Negative likelihood ratio (calculated from Sens and Spec if not provided).
#'   Must be between 0 and 1. If provided along with Plr, takes precedence over Sens/Spec.
#' @param Detail Logical. If TRUE, overlays key statistics (prevalence, likelihood ratios,
#'   posterior probabilities) onto the plot.
#' @param NullLine Logical. If TRUE, adds a reference line from prior probability
#'   through LR = 1 to illustrate an uninformative test.
#' @param LabelSize Numeric. Controls the size of text labels on the plot.
#'   Default is 14/5 ~= 2.8.
#' @param Verbose Logical. If TRUE, prints diagnostic metrics to the console.
#' @param Theme A ggplot2 theme object used as the base theme, for example jamovi's global
#'   \code{ggtheme}. The nomogram's own structural theme (hidden x axis, no grid, no legend)
#'   is layered on top of it, so the layout is preserved. Defaults to \code{theme_bw()}.
#' @param Title Character. Plot title. Callers inside a jamovi analysis pass a translated
#'   string; this function is file-level, so it cannot translate one itself.
#' @param PriorLabel Character. Name of the left (prior probability) axis.
#' @param PosteriorLabel Character. Name of the right (posterior probability) axis.
#' @param DetailLabels Named character vector with the words used in the \code{Detail} text:
#'   \code{prevalence}, \code{plr}, \code{nlr}, \code{post_pos}, \code{post_neg}. Callers inside a
#'   jamovi analysis pass translated strings (this function cannot translate them itself).
#'
#' @details
#' The Fagan nomogram visually represents Bayes' theorem for diagnostic testing:
#' 
#' \deqn{Post-test odds = Pre-test odds x Likelihood ratio}
#' 
#' The function accepts either:
#' \itemize{
#'   \item Sensitivity and Specificity (traditional approach)
#'   \item Positive and Negative Likelihood Ratios (direct approach)
#' }
#' 
#' If both are provided, sensitivity/specificity take precedence and a warning is issued.
#' 
#' Mathematical relationships:
#' \itemize{
#'   \item PLR = Sensitivity / (1 - Specificity)
#'   \item NLR = (1 - Sensitivity) / Specificity
#'   \item When calculating from LRs: Specificity = (PLR - 1) / (PLR - NLR)
#'   \item When calculating from LRs: Sensitivity = PLR x (1 - Specificity)
#'   \item Post-test probability (+) = (Prevalence x PLR) / ((Prevalence x PLR) + (1 - Prevalence))
#'   \item Post-test probability (-) = (Prevalence x NLR) / ((Prevalence x NLR) + (1 - Prevalence))
#' }
#'
#' @return A ggplot2 object containing the Fagan nomogram. The plot shows:
#' \itemize{
#'   \item Left axis: Prior probability (prevalence) as percentages
#'   \item Middle axis: Likelihood ratios
#'   \item Right axis: Posterior probability as percentages
#'   \item Red line: Positive test pathway
#'   \item Blue line: Negative test pathway
#' }
#'
#' @references
#' Fagan TJ. Letter: Nomogram for Bayes theorem. N Engl J Med. 1975;293(5):257.
#' 
#' Based on Perl web-implementation: https://araw.mede.uic.edu/cgi-bin/testcalc.pl
#' Authors: A.M. Chekroud & A. Schwartz, December 2016
#'
#' @seealso \code{\link{decision}}, \code{\link{decisioncalculator}} for related diagnostic test functions
#'
#' @importFrom ggplot2 aes annotate element_blank element_text geom_line geom_vline
#' @importFrom ggplot2 ggplot ggtitle margin rel scale_color_manual
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous sec_axis theme theme_bw
#' @importFrom stats line
#' @export
#'
#' @examples
#' # Example 1: Using sensitivity and specificity
#' nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.8)
#'
#' # Example 2: Using likelihood ratios directly
#' nomogrammer(Prevalence = 0.3, Plr = 4.5, Nlr = 0.125)
#'
#' # Example 3: With detailed annotations and null line
#' nomogrammer(Prevalence = 0.1, Sens = 0.95, Spec = 0.85, 
#'             Detail = TRUE, NullLine = TRUE, Verbose = TRUE)
#'
#' # Example 4: Low prevalence scenario (screening test)
#' nomogrammer(Prevalence = 0.01, Sens = 0.99, Spec = 0.95, Detail = TRUE)
#'
#' @author ClinicoPath Development Team
#' @note This function is used internally by the \code{decision} analysis in
#'   the ClinicoPath jamovi module for generating Fagan nomograms.

nomogrammer <- function(Prevalence,
                        Sens = NULL,
                        Spec = NULL,
                        Plr = NULL,
                        Nlr = NULL,
                        Detail = FALSE,
                        NullLine = FALSE,
                        LabelSize = (14/5),
                        Verbose = FALSE,
                        Theme = NULL,
                        Title = "Fagan Nomogram",
                        PriorLabel = "Prior\nProb.\n(%)",
                        PosteriorLabel = "Posterior\nProb.\n(%)",
                        DetailLabels = c(prevalence = "Prevalence", plr = "PLR", nlr = "NLR",
                                         post_pos = "Post(+)", post_neg = "Post(-)")) {

    ######################################
    ########## Helper Functions ##########
    ######################################

    # Helper functions (defined locally to avoid namespace pollution)
    odds <- function(p) {
        # Convert probability to odds
        if (any(p <= 0 | p >= 1)) {
            stop("Probability must be between 0 and 1 (exclusive)")
        }
        return(p / (1 - p))
    }

    logodds <- function(p) {
        # Convert probability to log-odds (base 10)
        if (any(p <= 0 | p >= 1)) {
            stop("Probability must be between 0 and 1 (exclusive)")
        }
        return(log10(p / (1 - p)))
    }

    logodds_to_p <- function(lo) {
        # Convert log-odds back to probability
        o <- 10^lo
        return(o / (1 + o))
    }

    p2percent <- function(p) {
        # Convert a probability to a percentage string.
        #
        # This used to be scales::percent(signif(p, 3)), which for a single value picks
        # integer accuracy and therefore renders EVERY probability below 0.5% as "0%".
        # On a rule-out nomogram that reads as "disease excluded": cotest(preset = "hpv_pap")
        # has a true both-negative post-test probability of 0.193%, the results table showed
        # 0.19%, and the plot's own annotation said "Post(-) = 0%".
        #
        # At or above 1% the output is byte-identical to the old integer form (verified over
        # 209,905 scalar values), so ordinary readings are unchanged. Below 1% the output DOES
        # change for every caller, and that is the point: decisioncalculator and
        # screeningcalculator routinely pass sub-1% prevalences and post-test probabilities, and
        # both were printing them as "0%". For example
        #   decisioncalculator(TP=20, FP=5, TN=99000, FN=975): prevalence "1%"   -> "0.995%"
        #   screeningcalculator(sens=.95, spec=.92, prev=.001): prevalence "0%"  -> "0.100%"
        #                                                        Post(-)    "0%"  -> "0.005%"
        # Those two analyses are therefore affected, for the better; they are not "unchanged".
        if (length(p) != 1L || !is.finite(p) || p == 0)
            return(scales::percent(signif(p, digits = 3), accuracy = 1))
        # Upper tail, the mirror of the lower one below: at integer accuracy every
        # probability from 99.5% up printed as "100%" - certainty - while the widened
        # panel now draws the line visibly short of 100% (PPV 99.69% read "Post(+) = 100%").
        if (p >= 0.995 && p < 1) {
            if (1 - p < 1e-5) return(">99.999%")
            return(scales::percent(p, accuracy = if (1 - p < 0.001) 1e-3 else 0.1))
        }
        if (abs(p) >= 0.01)                       # 1% to below 99.5%: byte-identical to the old output
            return(scales::percent(signif(p, digits = 3), accuracy = 1))
        if (abs(p) < 1e-5)                        # would still render as all zeros
            return(if (p > 0) "<0.001%" else ">-0.001%")
        # Three decimal places. An earlier version wrote
        #   max(1e-3, 10^(floor(log10(abs(p) * 100)) - 2))
        # which looks adaptive but is not: inside this branch p < 0.01, so the second term is
        # always <= 1e-3 and max() always returned exactly 1e-3.
        scales::percent(p, accuracy = 1e-3)
    }

    ######################################
    ########## Input Validation ##########
    ######################################

    # Prevalence validation
    if (missing(Prevalence)) {
        stop("Prevalence is required. Please provide a value between 0 and 1.")
    }
    if (!is.numeric(Prevalence) || length(Prevalence) != 1) {
        stop("Prevalence must be a single numeric value.")
    }
    if (Prevalence <= 0 || Prevalence >= 1) {
        stop("Prevalence must be between 0 and 1 (exclusive). Did you provide a percentage instead of a probability?")
    }

    # Check what inputs were provided
    sensspec_provided <- !missing(Sens) && !missing(Spec)
    plrnlr_provided <- !missing(Plr) && !missing(Nlr)

    # Validate sensitivity and specificity if provided
    if (sensspec_provided) {
        if (!is.numeric(Sens) || !is.numeric(Spec)) {
            stop("Sensitivity and Specificity must be numeric.")
        }
        if (length(Sens) != 1 || length(Spec) != 1) {
            stop("Sensitivity and Specificity must be single values.")
        }
        if (Sens <= 0 || Sens >= 1) {
            stop("Sensitivity must be between 0 and 1 (exclusive). Did you provide a percentage?")
        }
        if (Spec <= 0 || Spec >= 1) {
            stop("Specificity must be between 0 and 1 (exclusive). Did you provide a percentage?")
        }
    }

    # Validate likelihood ratios if provided
    if (plrnlr_provided) {
        if (!is.numeric(Plr) || !is.numeric(Nlr)) {
            stop("Positive and Negative likelihood ratios must be numeric.")
        }
        if (length(Plr) != 1 || length(Nlr) != 1) {
            stop("Likelihood ratios must be single values.")
        }
        if (Plr < 1) {
            stop("Positive likelihood ratio should be >= 1 for an informative test.")
        }
        if (Nlr < 0 || Nlr > 1) {
            stop("Negative likelihood ratio must be between 0 and 1.")
        }
        if (abs(Plr - Nlr) < .Machine$double.eps) {
            stop("PLR and NLR cannot be equal - this indicates an uninformative test.")
        }
    }

    # Check that at least one pair of inputs was provided
    if (!sensspec_provided && !plrnlr_provided) {
        stop("Either (Sens, Spec) or (Plr, Nlr) must be provided.")
    }

    # Warn if both were provided (sens/spec takes precedence)
    if (sensspec_provided && plrnlr_provided) {
        warning("Both sensitivity/specificity and likelihood ratios provided. Using sensitivity/specificity values.")
    }

    ######################################
    ########## Calculations     ##########
    ######################################

    if (sensspec_provided) {
        # Use sensitivity and specificity
        prior_prob <- Prevalence
        prior_odds <- odds(prior_prob)
        sensitivity <- Sens
        specificity <- Spec
        PLR <- sensitivity / (1 - specificity)
        NLR <- (1 - sensitivity) / specificity
        
        # Validate calculated likelihood ratios
        if (PLR < 1) {
            warning("Calculated PLR < 1, indicating poor test performance.")
        }
        if (NLR > 1) {
            warning("Calculated NLR > 1, indicating poor test performance.")
        }
        
    } else {
        # Use likelihood ratios and back-calculate sensitivity/specificity
        prior_prob <- Prevalence
        prior_odds <- odds(prior_prob)
        PLR <- Plr
        NLR <- Nlr
        
        # Calculate sensitivity and specificity from likelihood ratios
        # Using algebraic relationships:
        # PLR = Sens/(1-Spec) and NLR = (1-Sens)/Spec
        # Solving: Spec = (PLR-1)/(PLR-NLR) and Sens = PLR*(1-Spec)
        specificity <- (PLR - 1) / (PLR - NLR)
        sensitivity <- PLR * (1 - specificity)

        # Validate back-calculated sens/spec
        if (sensitivity < 0 || sensitivity > 1 || specificity < 0 || specificity > 1) {
            warning("Back-calculated sensitivity or specificity is outside [0,1] range. Please check your likelihood ratios.")
        }
    }

    # Calculate posterior probabilities
    post_odds_pos <- prior_odds * PLR
    post_odds_neg <- prior_odds * NLR
    post_prob_pos <- post_odds_pos / (1 + post_odds_pos)
    post_prob_neg <- post_odds_neg / (1 + post_odds_neg)

    ######################################
    ########## Plotting Setup   ##########
    ######################################

    # Set plotting theme. A caller may supply a base theme (jamovi's global ggtheme); the
    # structural tweaks below are applied AFTER it, because a complete theme replaces whatever
    # came before it and would otherwise bring back the x axis, the grid and the legend.
    # jamovi hands a renderer `ggtheme`, which is NOT a theme object: jmvcore returns a
    # LIST whose first element is the theme and whose remaining elements (when present) are
    # discrete colour/fill scales. `inherits(Theme, "theme")` is therefore FALSE for every
    # built-in jamovi theme, so the previous guard always fell through to theme_bw() and the
    # nomogram stayed a white rectangle on the dark pane - the exact defect it claimed to fix.
    # Take only the theme element: the palette scales would collide with this figure's own
    # scale_colour_manual() for the positive/negative rules.
    base_theme <- if (inherits(Theme, "theme")) {
        Theme
    } else if (is.list(Theme) && length(Theme) > 0 && inherits(Theme[[1]], "theme")) {
        Theme[[1]]
    } else {
        theme_bw()
    }
    theme_nomogram <- base_theme +
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.title.y.right = element_text(angle = 0, vjust = 0.5),
            axis.line = element_blank(),
            panel.grid = element_blank(),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = rel(1.2)),
            plot.margin = margin(20, 20, 20, 20)
        )

    # Define probability ticks (as percentages)
    ticks_prob <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 30,
                    40, 50, 60, 70, 80, 90, 95, 99)
    
    # Convert percentages to probabilities and then to odds/log-odds
    ticks_odds <- odds(ticks_prob / 100)
    ticks_logodds <- logodds(ticks_prob / 100)

    # Define likelihood ratio ticks
    ticks_lrs <- sort(c(10^(-3:3), 2 * (10^(-3:2)), 5 * (10^(-3:2))))
    ticks_log_lrs <- log10(ticks_lrs)

    # Set x-coordinates for plot elements
    left <- 0
    right <- 1
    middle <- 0.5

    # Create data frame with the four key points (start/end of pos/neg lines)
    df <- data.frame(
        x = c(left, right, left, right),
        y = c(prior_prob, post_prob_pos, prior_prob, post_prob_neg),
        line = c("pos", "pos", "neg", "neg")
    )

    # Calculate scaling factors for proper display
    adj_min <- range(ticks_logodds)[1]
    adj_max <- range(ticks_logodds)[2]
    adj_diff <- adj_max - adj_min
    scale_factor <- abs(adj_min) - adj_diff / 2

    # Convert probabilities to log-odds for plotting
    # Left (prior) points use the expression the left-axis breaks use, -(logodds(p) +
    # scale_factor); logodds(1 - p) - scale_factor is the same number up to rounding, and
    # that rounding put a prior at the panel edge a few ulp outside its own tick.
    df$lo_y <- ifelse(df$x == left,
                      -(logodds(df$y) + scale_factor),
                      logodds(df$y))

    # Calculate axis scaling
    rescale <- range(ticks_logodds) + abs(adj_min) - adj_diff / 2
    rescale_x_breaks <- ticks_logodds + abs(adj_min) - adj_diff / 2

    # The panel used to be exactly `rescale` (+/- 2.498 log10-odds), passed as
    # scale_y_continuous(limits =). A scale limit CENSORS out-of-range data to NA, so a
    # posterior below 0.317% or above 99.68% - a strong rule-out test at 5% prevalence,
    # or any prior at the 0.1% minimum - silently deleted that pathway's line while the
    # Detail text still printed its value. Now the panel widens to contain every endpoint
    # (and the right end of the dashed LR = 1 line, which sits at the prior), up to
    # +/- 5 log10-odds (about 0.001% / 99.999%); beyond that coord_cartesian() CLIPS the
    # line at the frame instead of deleting it. Unbounded widening crushed the 1-99% scale
    # into a sliver and printed the tick labels on top of one another.
    # `scale_factor` and the tick set above are untouched, so a nomogram whose endpoints
    # already fitted keeps the same frame, lines and ticks.
    y_cap <- 5
    ends <- c(df$lo_y[is.finite(df$lo_y)], logodds(prior_prob))
    ylim <- range(c(rescale, pmax(pmin(ends, y_cap), -y_cap)))
    # A widened side gets a hair of slack so a tick sitting exactly on the new edge
    # (the prior's own tick, say) is not dropped by rounding.
    ylim <- ylim + c(-1, 1) * 1e-9 * (abs(ylim - rescale) > 1e-12)
    # Extra labelled ticks, drawn only OUTSIDE the old frame: inside it they would add a
    # "99.5" to every nomogram that never needed widening.
    ticks_prob_axis <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05,
                         ticks_prob, 99.5, 99.8, 99.9, 99.95, 99.98, 99.99)
    ticks_logodds_axis <- logodds(ticks_prob_axis / 100)
    left_breaks <- -(ticks_logodds_axis + scale_factor)
    outside <- function(y) y < rescale[1] - 1e-12 | y > rescale[2] + 1e-12
    base_tick <- ticks_prob_axis %in% ticks_prob
    keep_left <- base_tick | outside(left_breaks)
    keep_right <- base_tick | outside(ticks_logodds_axis)

    # Detail text (the numbers the figure encodes): a subtitle, not text inside the panel,
    # where a strong rule-in line ran straight through it once the panel could widen.
    detailed_annotation <- if (Detail) paste(
        paste0(DetailLabels[["prevalence"]], " = ", p2percent(prior_prob)),
        paste(DetailLabels[["plr"]], "=", signif(PLR, 3), ",", DetailLabels[["nlr"]], "=", signif(NLR, 3)),
        paste(DetailLabels[["post_pos"]], "=", p2percent(post_prob_pos),
              ",", DetailLabels[["post_neg"]], "=", p2percent(post_prob_neg)),
        sep = "\n"
    ) else NULL

    ######################################
    ########## Create Plot       ##########
    ######################################

    p <- ggplot(df) +
        geom_line(aes(x = x, y = lo_y, color = line), linewidth = 1) +
        geom_vline(xintercept = middle, color = "black", linewidth = 0.5) +
        annotate(
            geom = "text",
            x = rep(middle + 0.075, length(ticks_log_lrs)),
            y = (ticks_log_lrs - scale_factor) / 2,
            label = ticks_lrs,
            size = rel(LabelSize)
        ) +
        annotate(
            geom = "point",
            x = rep(middle, length(ticks_log_lrs)),
            y = (ticks_log_lrs - scale_factor) / 2,
            size = 1
        ) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(
            expand = c(0, 0),
            breaks = left_breaks[keep_left],
            labels = ticks_prob_axis[keep_left],
            name = PriorLabel,
            sec.axis = sec_axis(
                transform = ~ .,
                name = PosteriorLabel,
                labels = ticks_prob_axis[keep_right],
                breaks = ticks_logodds_axis[keep_right]
            )
        ) +
        ggplot2::coord_cartesian(ylim = ylim, expand = FALSE) +
        scale_color_manual(values = c("pos" = "red", "neg" = "blue")) +
        ggtitle(Title, subtitle = detailed_annotation) +
        theme_nomogram +
        theme(plot.subtitle = element_text(hjust = 0.5, size = rel(0.85)))

    ######################################
    ########## Optional Features ##########
    ######################################

    # Add null line (LR = 1) if requested
    if (NullLine) {
        uninformative <- data.frame(
            x = c(left, right),
            lo_y = c(
                -(logodds(prior_prob) + scale_factor),
                logodds(prior_prob)
            )
        )

        p <- p + geom_line(
            aes(x = x, y = lo_y),
            data = uninformative,
            color = "gray",
            linetype = "dashed",
            inherit.aes = FALSE
        )
    }

    # The Detail text is the plot subtitle (set above).

    # Print verbose output if requested
    if (Verbose) {
        cat("\n=== Fagan Nomogram Results ===\n")
        cat("Prevalence =", p2percent(prior_prob), "\n")
        cat("Sensitivity =", p2percent(sensitivity), "\n")
        cat("Specificity =", p2percent(specificity), "\n")
        cat("Positive LR =", signif(PLR, 3), "\n")
        cat("Negative LR =", signif(NLR, 3), "\n")
        cat("Post-test probability (positive test) =", p2percent(post_prob_pos), "\n")
        cat("Post-test probability (negative test) =", p2percent(post_prob_neg), "\n")
        cat("===============================\n")
    }

    return(p)
}
