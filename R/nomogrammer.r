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
#'   Default is 14/5 ≈ 2.8.
#' @param Verbose Logical. If TRUE, prints diagnostic metrics to the console.
#'
#' @details
#' The Fagan nomogram visually represents Bayes' theorem for diagnostic testing:
#' 
#' \deqn{Post-test odds = Pre-test odds × Likelihood ratio}
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
#'   \item When calculating from LRs: Sensitivity = PLR × (1 - Specificity)
#'   \item Post-test probability (+) = (Prevalence × PLR) / ((Prevalence × PLR) + (1 - Prevalence))
#'   \item Post-test probability (-) = (Prevalence × NLR) / ((Prevalence × NLR) + (1 - Prevalence))
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
#' @import ggplot2
#' @import scales
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
                        Verbose = FALSE) {

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
        # Convert numeric probability to formatted percentage string
        # e.g., 0.6346111 -> "63.5%"
        scales::percent(signif(p, digits = 3))
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

    # Set plotting theme
    theme_nomogram <- theme_bw() +
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
    midright <- 0.75

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
    df$lo_y <- ifelse(df$x == left, 
                      logodds(1 - df$y) - scale_factor, 
                      logodds(df$y))

    # Calculate axis scaling
    rescale <- range(ticks_logodds) + abs(adj_min) - adj_diff / 2
    rescale_x_breaks <- ticks_logodds + abs(adj_min) - adj_diff / 2

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
            limits = rescale,
            breaks = -rescale_x_breaks,
            labels = ticks_prob,
            name = "Prior\nProb.\n(%)",
            sec.axis = sec_axis(
                transform = ~ .,
                name = "Posterior\nProb.\n(%)",
                labels = ticks_prob,
                breaks = ticks_logodds
            )
        ) +
        scale_color_manual(values = c("pos" = "red", "neg" = "blue")) +
        ggtitle("Fagan Nomogram") +
        theme_nomogram

    ######################################
    ########## Optional Features ##########
    ######################################

    # Add null line (LR = 1) if requested
    if (NullLine) {
        uninformative <- data.frame(
            x = c(left, right),
            lo_y = c(
                logodds(1 - prior_prob) - scale_factor,
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

    # Add detailed annotations if requested
    if (Detail) {
        detailed_annotation <- paste(
            paste0("Prevalence = ", p2percent(prior_prob)),
            paste("PLR =", signif(PLR, 3), ", NLR =", signif(NLR, 3)),
            paste("Post(+) =", p2percent(post_prob_pos),
                  ", Post(-) =", p2percent(post_prob_neg)),
            sep = "\n"
        )

        p <- p + annotate(
            geom = "text",
            x = midright,
            y = max(rescale) * 0.8,
            label = detailed_annotation,
            size = rel(LabelSize),
            hjust = 0,
            vjust = 1
        )
    }

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