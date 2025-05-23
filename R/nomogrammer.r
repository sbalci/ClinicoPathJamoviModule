#' @title Fagan Nomogram Function
#' @description Create the Fagan nomogram for Bayesian analysis in diagnostic testing
#' @param Prevalence Prior probability / prevalence as a number between 0 and 1
#' @param Sens Model sensitivity as a number between 0 and 1 (optional if Plr/Nlr provided)
#' @param Spec Model specificity as a number between 0 and 1 (optional if Plr/Nlr provided)
#' @param Plr Positive likelihood ratio (calculated from Sens and Spec if not provided)
#' @param Nlr Negative likelihood ratio (calculated from Sens and Spec if not provided)
#' @param Detail If TRUE, will overlay key statistics onto the plot
#' @param NullLine If TRUE, will add a line from prior prob through LR = 1
#' @param LabelSize Tweak this number to change the label sizes
#' @param Verbose Print out relevant metrics in the console
#' @return A ggplot object with the Fagan nomogram
#' @import ggplot2
#' @import scales
#' @importFrom stats line
#' @export
#'
#' @examples
#' # Using sensitivity and specificity
#' nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.8)
#'
#' # Using likelihood ratios
#' nomogrammer(Prevalence = 0.3, Plr = 4.5, Nlr = 0.125)

nomogrammer <- function(Prevalence,
                        Sens = NULL,
                        Spec = NULL,
                        Plr = NULL,
                        Nlr = NULL,
                        Detail = FALSE,
                        NullLine = FALSE,
                        LabelSize = (14/5),
                        Verbose = FALSE){

    ## Function inputs:
    # Prevalence (prior probability) as a number between 0 and 1
    # Either
    # Sens & Spec
    # model sensitivity and specificity as a number between 0 and 1
    # Or
    # Likelihood ratios
    # Positive and Negative LRs (numeric)

    ## Function options:
    # Detail: If true, will overlay key statistics onto the plot
    # NullLine: If true, will add a line from prior prob through LR = 1
    # LabelSize: Tweak this number to change the label sizes
    # Verbose: Print out relevant metrics in the console

    ## Function returns:
    # ggplot object

    ######################################
    ########## Helper Functions ##########
    ######################################

    ## Helper functions
    ##   (defined inside nomogrammer, so remain local only & won't clutter user env)
    odds         <- function(p){
        # Function converts probability into odds
        o <- p/(1-p)
        return(o)
    }

    logodds      <- function(p){
        # Function returns logodds for a probability
        lo <- log10(p/(1-p))
        return(lo)
    }

    logodds_to_p <- function(lo){
        # Function goes from logodds back to a probability
        o <- 10^lo
        p <- o/(1+o)
        return(p)
    }

    p2percent <- function(p){
        # Function turns numeric probability into string percentage
        # e.g. 0.6346111 -> 63.5%
        scales::percent(signif(p, digits = 3))
    }

    ######################################
    ########## Input Validation ##########
    ######################################

    ## Prevalence validation
    # needs to exist
    if(missing(Prevalence)){
        stop("Prevalence is missing")
    }
    # needs to be numeric
    if(!is.numeric(Prevalence)){
        stop("Prevalence should be numeric")
    }
    # needs to be a prob not a percent
    if((Prevalence > 1) | (Prevalence <= 0)){
        stop("Prevalence should be a probability between 0 and 1 (did you give a %?)")
    }

    # Did user give sens & spec?
    if(missing(Sens) | missing(Spec)){
        sensspec <- FALSE
    } else{
        sensspec <- TRUE
    }

    # if yes, make sure they are numbers
    if(sensspec == TRUE){
        if(!is.numeric(Sens)){stop("Sensitivity should be numeric")}
        if(!is.numeric(Spec)){stop("Specificity should be numeric")}
        # numbers that are probabilities not percentages
        if((Sens > 1) | (Sens <= 0)){stop("Sensitivity should be a probability between 0 and 1 (did you give a %?)")}
        if((Spec > 1) | (Spec <= 0)){stop("Specificity should be a probability between 0 and 1 (did you give a %?)")}
    }

    # Did user give PLR & NLR?
    if(missing(Plr) | missing(Nlr)){
        plrnlr <- FALSE
    } else{
        plrnlr <- TRUE
    }

    # if yes, make sure they are numbers
    if(plrnlr == TRUE){
        if(!is.numeric(Plr)){stop("PLR should be numeric")}
        if(!is.numeric(Nlr)){stop("NLR should be numeric")}
        # numbers that vaguely make sense
        if(Plr < 1){stop("PLR shouldn't be less than 1")}
        if(Nlr < 0){stop("NLR shouldn't be below zero")}
        if(Nlr > 1){stop("NLR shouldn't be more than 1")}
    }

    # Did they give a valid sensspec and plrnlr? If yes, ignore the LRs and tell them
    if((sensspec == TRUE) && (plrnlr == TRUE) ){
        warning("You provided sens/spec as well as likelihood ratios-- I ignored the LRs!")
    }

    ######################################
    ########## Calculations     ##########
    ######################################

    ## If sens/spec provided, we calculate posterior probabilities & odds using sens & spec
    ##  otherwise, if plr and nlr provided, we calculate posteriors using them
    ##  if neither exist, then return an error
    if(sensspec == TRUE){
        prior_prob  <- Prevalence
        prior_odds  <- odds(prior_prob)
        sensitivity <- Sens
        specificity <- Spec
        PLR <- sensitivity/(1-specificity)
        NLR <- (1-sensitivity)/specificity
        post_odds_pos  <- prior_odds * PLR
        post_odds_neg  <- prior_odds * NLR
        post_prob_pos  <- post_odds_pos/(1+post_odds_pos)
        post_prob_neg  <- post_odds_neg/(1+post_odds_neg)
    } else if(plrnlr == TRUE){
        prior_prob  <- Prevalence
        prior_odds  <- odds(prior_prob)
        PLR <- Plr
        NLR <- Nlr
        # Calculate sensitivity and specificity from likelihood ratios
        # Using the relationships: PLR = Sens/(1-Spec) and NLR = (1-Sens)/Spec
        # Solving: Sens = (PLR*(1-NLR))/(PLR-NLR) and Spec = (1-PLR)/(NLR-PLR)
        if(PLR == NLR) {
            stop("PLR and NLR cannot be equal - this would indicate an uninformative test")
        }
        sensitivity <- (PLR*(1-NLR))/(PLR-NLR)
        specificity <- (1-NLR)/(PLR-NLR)

        # Validate calculated sens/spec are reasonable
        if(sensitivity < 0 | sensitivity > 1 | specificity < 0 | specificity > 1){
            warning("Calculated sensitivity or specificity is outside [0,1] range. Check your likelihood ratios.")
        }

        post_odds_pos  <- prior_odds * PLR
        post_odds_neg  <- prior_odds * NLR
        post_prob_pos  <- post_odds_pos/(1+post_odds_pos)
        post_prob_neg  <- post_odds_neg/(1+post_odds_neg)
    } else{
        stop("Couldn't find sens & spec, or positive & negative likelihood ratios")
    }

    ######################################
    ########## Plotting Setup   ##########
    ######################################

    ## Set common theme preferences up front
    theme_set(theme_bw() +
                  theme(axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_text(angle = 0, vjust = 0.5),
                        axis.title.y.right = element_text(angle = 0, vjust = 0.5),
                        axis.line = element_blank(),
                        panel.grid = element_blank(),
                        legend.position = "none",
                        plot.title = element_text(hjust = 0.5, size = rel(1.2))
                  )
    )

    ## Setting up the points of interest along the y-axes

    # Select probabilities of interest (nb as percentages)
    ticks_prob    <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 30,
                       40, 50, 60, 70, 80, 90, 95, 99)
    # Convert % to odds
    ticks_odds    <- odds(ticks_prob/100)
    # Convert % to logodds
    ticks_logodds <- logodds(ticks_prob/100)

    # Select the likelihood ratios of interest (for the middle y-axis)
    ticks_lrs     <- sort(c(10^(-3:3), 2*(10^(-3:2)), 5*(10^(-3:2))))
    # Log10 them since plot is in logodds space
    ticks_log_lrs <- log10(ticks_lrs)

    ## Fixing particular x-coordinates
    left     <- 0
    right    <- 1
    middle   <- 0.5
    midright <- 0.75

    ## Lay out the four key plot points
    ##  (the start and finish of the positive and negative lines)

    # Initially these are expressed as probabilities
    df <- data.frame(x=c(left, right, left, right),
                     y=c(prior_prob, post_prob_pos, prior_prob, post_prob_neg),
                     line = c("pos", "pos", "neg", "neg"))

    adj_min      <- range(ticks_logodds)[1]
    adj_max      <- range(ticks_logodds)[2]
    adj_diff     <- adj_max - adj_min
    scale_factor <- abs(adj_min) - adj_diff/2

    # Convert probabilities to logodds for plotting
    df$lo_y  <- ifelse(df$x==left,logodds(1-df$y)-scale_factor,logodds(df$y))

    rescale   <- range(ticks_logodds) + abs(adj_min) - adj_diff/2
    rescale_x_breaks  <- ticks_logodds + abs(adj_min) - adj_diff/2

    ######################################
    ########## Create Plot       ##########
    ######################################

    p <- ggplot(df) +
        geom_line(aes(x = x, y = lo_y, color = line), size = 1) +
        geom_vline(xintercept = middle, color = "black", size = 0.5) +
        annotate(geom = "text",
                 x = rep(middle+.075, length(ticks_log_lrs)),
                 y = (ticks_log_lrs-scale_factor)/2,
                 label = ticks_lrs,
                 size = rel(LabelSize)) +
        annotate(geom="point",
                 x = rep(middle, length(ticks_log_lrs)),
                 y = (ticks_log_lrs-scale_factor)/2,
                 size = 1) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0),
                           limits = rescale,
                           breaks = -rescale_x_breaks,
                           labels = ticks_prob,
                           name = "Prior\nProb.\n(%)",
                           sec.axis = sec_axis(trans = ~.,
                                               name = "Posterior\nProb.\n(%)",
                                               labels = ticks_prob,
                                               breaks = ticks_logodds)) +
        scale_color_manual(values = c("pos" = "red", "neg" = "blue")) +
        ggtitle("Fagan Nomogram") +
        theme(plot.margin = margin(20, 20, 20, 20))

    ## Optional overlay text: prevalence, PLR/NLR, and posterior probabilities
    detailedAnnotation <- paste(
        paste0("Prevalence = ", p2percent(prior_prob)),
        paste("PLR =", signif(PLR, 3),", NLR =", signif(NLR, 3)),
        paste("Post(+) =", p2percent(post_prob_pos),
              ", Post(-) =", p2percent(post_prob_neg)),
        sep = "\n")

    ######################################
    ########## Optional Features ##########
    ######################################

    ## Do we add the null line i.e. LR = 1, illustrating an uninformative model
    if(NullLine == TRUE){
        ## If yes, first calculate the start and end points
        uninformative <- data.frame(
            x = c(left,right),
            lo_y = c( (logodds(1-prior_prob) - scale_factor) , logodds(prior_prob))
        )

        p <- p + geom_line(aes(x = x, y = lo_y), data = uninformative,
                           color = "gray",
                           lty = 2,
                           inherit.aes = FALSE)
    }

    ## Do we add the detailed stats to the top right?
    if(Detail == TRUE){
        p <- p + annotate(geom = "text",
                          x = midright,
                          y = max(rescale) * 0.8,
                          label = detailedAnnotation,
                          size = rel(LabelSize),
                          hjust = 0,
                          vjust = 1)
    }

    ## Print verbose output if requested
    if(Verbose == TRUE){
        writeLines(
            text = c(
                paste0("Prevalence = ", p2percent(prior_prob)),
                paste("PLR =", signif(PLR, 3)),
                paste("NLR =", signif(NLR, 3)),
                paste("Posterior probability (positive test) =", p2percent(post_prob_pos)),
                paste("Posterior probability (negative test) =", p2percent(post_prob_neg)),
                paste("Sensitivity =", p2percent(sensitivity)),
                paste("Specificity =", p2percent(specificity))
            )
        )
    }

    return(p)
}
