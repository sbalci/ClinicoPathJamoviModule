#' Create HTML Progress Bar for Tree Analysis
#'
#' @description Creates an SVG-based progress bar for displaying analysis progress
#' @param current Current step number
#' @param total Total number of steps
#' @param message Progress message to display
#' @param width Width of the progress bar (default: 520)
#' @param height Height of the progress bar (default: 50)
#' @return HTML string containing the progress bar
treeProgressBar <- function(current, total = 100, message = '', width = 520, height = 50) {
    # Ensure current is within bounds
    current <- max(0, min(current, total))
    
    # Calculate progress percentage
    progress_width <- (width - 20) * current / total + 10
    
    # Create outer border points
    outer_points <- paste(
        "10,10", "10,40", paste0(width - 10, ",40"), paste0(width - 10, ",10"), "10,10",
        sep = " "
    )
    
    # Create inner progress bar points  
    inner_points <- paste(
        "10,10", "10,40", paste0(progress_width, ",40"), paste0(progress_width, ",10"), "10,10",
        sep = " "
    )
    
    # Create HTML output
    html_output <- paste0(
        '<div style="margin: 10px auto; text-align: center;">',
        '<svg width="', width, '" height="', height, '" style="background-color: transparent; margin: auto; padding: 0;" xmlns="http://www.w3.org/2000/svg">',
        '<polyline points="', outer_points, '" fill="#FFFFFF" stroke="#000000" fill-opacity="1" stroke-width="1" stroke-opacity="1" />',
        '<polyline points="', inner_points, '" fill="#4CAF50" stroke="#4CAF50" fill-opacity="0.8" stroke-width="1" stroke-opacity="1" />',
        '</svg>',
        '</div>',
        '<div style="text-align: center; font-size: 14px; color: #333; margin-top: 5px;">',
        message, ' (', round(current/total * 100, 1), '%)',
        '</div>'
    )
    
    return(html_output)
}

#' Create Tree Analysis Progress Steps
#'
#' @description Defines the progress steps for tree analysis
#' @param algorithm Algorithm being used ("rpart", "fftrees", etc.)
#' @param validation Whether cross-validation is enabled
#' @param hyperparameter_tuning Whether hyperparameter tuning is enabled
#' @param model_comparison Whether model comparison is enabled
#' @return List containing step definitions
getTreeAnalysisSteps <- function(algorithm = "rpart", validation = FALSE, hyperparameter_tuning = FALSE, model_comparison = FALSE) {
    
    base_steps <- list(
        list(step = 1, message = "Initializing analysis", weight = 1),
        list(step = 2, message = "Validating input data", weight = 1),
        list(step = 3, message = "Preparing training data", weight = 2)
    )
    
    # Algorithm-specific steps
    if (algorithm == "fftrees") {
        algo_steps <- list(
            list(step = 4, message = "Building Fast-and-Frugal Trees", weight = 4),
            list(step = 5, message = "Evaluating FFTrees performance", weight = 2)
        )
    } else {
        algo_steps <- list(
            list(step = 4, message = "Training decision tree model", weight = 3),
            list(step = 5, message = "Generating tree visualization", weight = 2)
        )
    }
    
    additional_steps <- list()
    
    # Add optional steps based on settings
    if (validation) {
        additional_steps <- c(additional_steps, list(
            list(step = length(base_steps) + length(algo_steps) + 1, message = "Performing cross-validation", weight = 3)
        ))
    }
    
    if (hyperparameter_tuning) {
        additional_steps <- c(additional_steps, list(
            list(step = length(base_steps) + length(algo_steps) + length(additional_steps) + 1, message = "Optimizing hyperparameters", weight = 4)
        ))
    }
    
    if (model_comparison) {
        additional_steps <- c(additional_steps, list(
            list(step = length(base_steps) + length(algo_steps) + length(additional_steps) + 1, message = "Comparing multiple models", weight = 3)
        ))
    }
    
    # Final steps
    final_steps <- list(
        list(step = 999, message = "Generating results tables", weight = 2),
        list(step = 1000, message = "Finalizing analysis", weight = 1)
    )
    
    all_steps <- c(base_steps, algo_steps, additional_steps, final_steps)
    
    # Recalculate step numbers and create cumulative weights
    total_weight <- sum(sapply(all_steps, function(x) x$weight))
    cumulative_weight <- 0
    
    for (i in seq_along(all_steps)) {
        all_steps[[i]]$step_num <- i
        cumulative_weight <- cumulative_weight + all_steps[[i]]$weight
        all_steps[[i]]$progress_percent <- (cumulative_weight / total_weight) * 100
    }
    
    return(all_steps)
}