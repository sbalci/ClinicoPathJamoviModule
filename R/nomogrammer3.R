#' @title Fagan Nomogram Function
#' @description Create the Fagan nomogram for Bayesian analysis in diagnostic testing
#' @param Prevalence Prior probability / prevalence
#' @param Sens Sensitivity
#' @param Spec Specificity
#' @param Plr Positive likelihood ratio (calculated from Sens and Spec if not provided)
#' @param Nlr Negative likelihood ratio (calculated from Sens and Spec if not provided)
#' @param Detail Whether to show detailed statistics on the plot
#' @param NullLine Whether to show the null effect line
#' @param LabelSize Size of text labels
#' @param Verbose Whether to print additional information
#' @return A plot object with the Fagan nomogram
#' @importFrom graphics plot segments text par axis mtext
#'
#' @export
nomogrammer <- function(Prevalence = 0.4,
                        Sens = 0.9,
                        Spec = 0.8,
                        Plr = NULL,
                        Nlr = NULL,
                        Detail = TRUE,
                        NullLine = TRUE,
                        LabelSize = 2.8,
                        Verbose = FALSE) {

    # Calculate LR if not provided
    if (is.null(Plr)) {
        Plr <- Sens / (1 - Spec)
    }

    if (is.null(Nlr)) {
        Nlr <- (1 - Sens) / Spec
    }

    # Calculate post-test probabilities
    PosttestProbP <- (Prevalence * Plr) / ((Prevalence * Plr) + (1 - Prevalence))
    PosttestProbN <- (Prevalence * Nlr) / ((Prevalence * Nlr) + (1 - Prevalence))

    # Transformation functions for plotting
    PreToY <- function(Pre) {
        100 * Pre / (1 - Pre)
    }

    YToPre <- function(Y) {
        Y / (Y + 100)
    }

    # Print info if verbose
    if (Verbose) {
        cat("Prevalence:", Prevalence, "\n")
        cat("Sensitivity:", Sens, "\n")
        cat("Specificity:", Spec, "\n")
        cat("Positive LR:", Plr, "\n")
        cat("Negative LR:", Nlr, "\n")
        cat("Post-test Probability (Positive test):", PosttestProbP, "\n")
        cat("Post-test Probability (Negative test):", PosttestProbN, "\n")
    }

    # Set up plot
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    par(mar = c(2, 4, 2, 4))

    # Create empty plot
    plot(NULL, xlim = c(0, 2), ylim = c(0.1, 1000),
         log = "y", axes = FALSE,
         xlab = "", ylab = "",
         main = "Fagan Nomogram")

    # Draw the three vertical lines
    segments(0, 0.1, 0, 1000, lwd = 2)
    segments(1, 0.1, 1, 1000, lwd = 2)
    segments(2, 0.1, 2, 1000, lwd = 2)

    # Add axis labels
    axis(2, at = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000),
         labels = c("0.1", "0.2", "0.5", "1", "2", "5", "10", "20", "50", "100", "200", "500", "1000"),
         las = 1, cex.axis = 0.8)

    mtext("Pre-test Probability (%)", side = 1, at = 0, line = 0.5, cex = 0.8)
    mtext("Likelihood Ratio", side = 1, at = 1, line = 0.5, cex = 0.8)
    mtext("Post-test Probability (%)", side = 1, at = 2, line = 0.5, cex = 0.8)

    # Pre-test probability ticks and labels
    pre_ticks <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5,
                   0.6, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99, 0.995)
    pre_labels <- c("0.1", "0.2", "0.5", "1", "2", "5", "10", "20", "30", "40", "50",
                    "60", "70", "80", "90", "95", "98", "99", "99.5")

    segments(0, PreToY(pre_ticks), -0.05, PreToY(pre_ticks))
    text(-0.1, PreToY(pre_ticks), pre_labels, cex = 0.6, adj = 1)

    # LR ticks and labels
    lr_ticks <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
    lr_labels <- c("0.001", "0.002", "0.005", "0.01", "0.02", "0.05", "0.1", "0.2", "0.5", "1", "2", "5", "10", "20", "50", "100", "200", "500", "1000")

    segments(1, lr_ticks, 1.05, lr_ticks)
    text(1.1, lr_ticks, lr_labels, cex = 0.6, adj = 0)

    # Post-test probability ticks and labels
    post_ticks <- pre_ticks
    post_labels <- pre_labels

    segments(2, PreToY(post_ticks), 2.05, PreToY(post_ticks))
    text(2.1, PreToY(post_ticks), post_labels, cex = 0.6, adj = 0)

    # Draw the positive and negative LR lines
    segments(0, PreToY(Prevalence), 1, Plr, col = "red", lwd = 2)
    segments(1, Plr, 2, PreToY(PosttestProbP), col = "red", lwd = 2)

    segments(0, PreToY(Prevalence), 1, Nlr, col = "blue", lwd = 2)
    segments(1, Nlr, 2, PreToY(PosttestProbN), col = "blue", lwd = 2)

    # Draw null line if requested
    if (NullLine) {
        segments(0, PreToY(0.001), 2, PreToY(0.001), col = "gray", lty = 2)
        segments(0, PreToY(0.999), 2, PreToY(0.999), col = "gray", lty = 2)
        segments(0, PreToY(0.5), 2, PreToY(0.5), col = "gray", lty = 2)
    }

    # Add detailed information if requested
    if (Detail) {
        # Mark the prevalence and post-test probabilities
        points(0, PreToY(Prevalence), pch = 19, col = "black")
        text(0, PreToY(Prevalence), paste0(" ", round(Prevalence * 100, 1), "%"),
             adj = c(0, 0.5), cex = LabelSize/2.8)

        points(2, PreToY(PosttestProbP), pch = 19, col = "red")
        text(2, PreToY(PosttestProbP), paste0(" ", round(PosttestProbP * 100, 1), "%"),
             adj = c(0, 0.5), cex = LabelSize/2.8)

        points(2, PreToY(PosttestProbN), pch = 19, col = "blue")
        text(2, PreToY(PosttestProbN), paste0(" ", round(PosttestProbN * 100, 1), "%"),
             adj = c(0, 0.5), cex = LabelSize/2.8)

        # Mark the LRs
        points(1, Plr, pch = 19, col = "red")
        text(1, Plr, paste0(" LR+ = ", round(Plr, 2)),
             adj = c(0, 0.5), cex = LabelSize/2.8)

        points(1, Nlr, pch = 19, col = "blue")
        text(1, Nlr, paste0(" LR- = ", round(Nlr, 2)),
             adj = c(0, 0.5), cex = LabelSize/2.8)

        # Add legend
        legend("topright",
               legend = c("Positive Test", "Negative Test"),
               col = c("red", "blue"),
               lwd = 2,
               cex = 0.8)
    }

    # Return the plot
    return(invisible(NULL))
}
