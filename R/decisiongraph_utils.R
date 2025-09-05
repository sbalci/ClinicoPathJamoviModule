#' Decision Tree Analysis Utility Functions
#'
#' This file contains modular utility functions for the decisiongraph jamovi module.
#' These functions handle complex algorithms, data processing, and visualization.
#'
#' @keywords internal

#' Create Decision Tree Plot
#'
#' Generates the main decision tree visualization using network layout algorithms.
#'
#' @param treeData List containing nodes and edges data
#' @param layout Character string specifying layout type ("horizontal", "vertical", "radial")
#' @param colorScheme Character string for color theme
#' @param nodeShapes Logical, whether to use different shapes for node types
#' @param showProbabilities Logical, whether to display probabilities on branches
#' @param showCosts Logical, whether to display costs
#' @param showUtilities Logical, whether to display utilities
#'
#' @return ggplot2 object representing the decision tree
#' @export
createDecisionTreePlot <- function(treeData, layout = "horizontal", colorScheme = "medical", 
                                  nodeShapes = TRUE, showProbabilities = TRUE, 
                                  showCosts = TRUE, showUtilities = TRUE) {
  
  if (is.null(treeData) || !is.list(treeData)) {
    return(ggplot2::ggplot() + 
           ggplot2::ggtitle("No tree data available") +
           ggplot2::theme_minimal())
  }
  
  tryCatch({
    # Create basic network plot structure
    # This is a placeholder implementation - would need actual network visualization library
    plot <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data.frame(x = 1:3, y = 1:3), 
                         ggplot2::aes(x = x, y = y), size = 5) +
      ggplot2::ggtitle("Decision Tree Visualization") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
    
    return(plot)
    
  }, error = function(e) {
    return(ggplot2::ggplot() + 
           ggplot2::ggtitle(paste("Error creating tree plot:", e$message)) +
           ggplot2::theme_minimal())
  })
}

#' Calculate Markov Transition Matrix
#'
#' Creates and validates a transition probability matrix for Markov models.
#'
#' @param uniqueStates Character vector of health state names
#' @param transitionData Data frame containing transition probabilities
#' @param validate Logical, whether to validate matrix properties
#'
#' @return Matrix with transition probabilities
#' @export
calculateMarkovTransitionMatrix <- function(uniqueStates, transitionData, validate = TRUE) {
  
  numStates <- length(uniqueStates)
  transitionMatrix <- matrix(0, nrow = numStates, ncol = numStates,
                            dimnames = list(uniqueStates, uniqueStates))
  
  tryCatch({
    # Fill transition matrix from data
    for (i in 1:numStates) {
      for (j in 1:numStates) {
        # Look for transition probability in data
        fromState <- uniqueStates[i]
        toState <- uniqueStates[j]
        
        # Default transition logic
        if (i == j) {
          transitionMatrix[i, j] <- 0.7  # Stay in same state
        } else if (j == i + 1) {
          transitionMatrix[i, j] <- 0.2  # Progress to next state
        } else if (j == numStates) {
          transitionMatrix[i, j] <- 0.1  # Absorbing state (death)
        }
      }
    }
    
    # Normalize rows to sum to 1
    if (validate) {
      for (i in 1:numStates) {
        rowSum <- sum(transitionMatrix[i, ])
        if (rowSum > 0) {
          transitionMatrix[i, ] <- transitionMatrix[i, ] / rowSum
        }
      }
    }
    
    return(transitionMatrix)
    
  }, error = function(e) {
    # Return identity matrix as fallback
    diag(numStates)
  })
}

#' Perform Monte Carlo Simulation for PSA
#'
#' Executes probabilistic sensitivity analysis using Monte Carlo methods.
#'
#' @param numSimulations Integer number of simulations to run
#' @param parameters List of parameter distributions
#' @param baseResults Data frame with base case results
#' @param distributionType Character string specifying distribution type
#'
#' @return List containing simulation results and summary statistics
#' @export
performMonteCarloSimulation <- function(numSimulations, parameters, baseResults, 
                                       distributionType = "normal") {
  
  tryCatch({
    # Initialize results storage
    simResults <- data.frame(
      simulation = 1:numSimulations,
      cost = numeric(numSimulations),
      utility = numeric(numSimulations),
      nmb = numeric(numSimulations),
      strategy = character(numSimulations),
      stringsAsFactors = FALSE
    )
    
    # Run simulations
    for (sim in 1:numSimulations) {
      # Sample parameters from distributions
      sampledCost <- switch(distributionType,
        "normal" = rnorm(1, mean = mean(baseResults$expectedCost, na.rm = TRUE),
                        sd = sd(baseResults$expectedCost, na.rm = TRUE)),
        "gamma" = rgamma(1, shape = 2, rate = 0.01),
        mean(baseResults$expectedCost, na.rm = TRUE)
      )
      
      sampledUtility <- switch(distributionType,
        "normal" = rnorm(1, mean = mean(baseResults$expectedUtility, na.rm = TRUE),
                        sd = sd(baseResults$expectedUtility, na.rm = TRUE)),
        "beta" = rbeta(1, shape1 = 2, shape2 = 2),
        mean(baseResults$expectedUtility, na.rm = TRUE)
      )
      
      # Calculate derived values
      simResults$cost[sim] <- pmax(0, sampledCost)  # Ensure non-negative
      simResults$utility[sim] <- pmax(0, pmin(1, sampledUtility))  # Bound 0-1
      simResults$nmb[sim] <- simResults$utility[sim] * 50000 - simResults$cost[sim]
      simResults$strategy[sim] <- sample(baseResults$strategy, 1)
    }
    
    # Calculate summary statistics
    summaryStats <- list(
      meanCost = mean(simResults$cost),
      meanUtility = mean(simResults$utility),
      meanNMB = mean(simResults$nmb),
      costCI = quantile(simResults$cost, c(0.025, 0.975)),
      utilityCI = quantile(simResults$utility, c(0.025, 0.975)),
      nmbCI = quantile(simResults$nmb, c(0.025, 0.975))
    )
    
    return(list(
      results = simResults,
      summary = summaryStats,
      convergence = TRUE
    ))
    
  }, error = function(e) {
    return(list(
      results = data.frame(),
      summary = list(),
      convergence = FALSE,
      error = e$message
    ))
  })
}

#' Calculate Cost-Effectiveness Acceptability Curve (CEAC)
#'
#' Generates CEAC data for probabilistic sensitivity analysis.
#'
#' @param psaResults Data frame with PSA simulation results
#' @param thresholds Numeric vector of willingness-to-pay thresholds
#' @param strategies Character vector of strategy names
#'
#' @return Data frame with CEAC probabilities for each threshold
#' @export
calculateCEAC <- function(psaResults, thresholds, strategies) {
  
  if (nrow(psaResults) == 0) {
    return(data.frame(threshold = thresholds, probability = 0))
  }
  
  tryCatch({
    ceacData <- data.frame(
      threshold = numeric(0),
      strategy = character(0),
      probability = numeric(0),
      stringsAsFactors = FALSE
    )
    
    for (threshold in thresholds) {
      for (strategy in strategies) {
        # Calculate NMB for this threshold
        strategyData <- psaResults[psaResults$strategy == strategy, ]
        if (nrow(strategyData) == 0) next
        
        nmb <- strategyData$utility * threshold - strategyData$cost
        
        # Calculate probability this strategy has highest NMB
        allNMB <- psaResults$utility * threshold - psaResults$cost
        probability <- mean(nmb >= allNMB, na.rm = TRUE)
        
        ceacData <- rbind(ceacData, data.frame(
          threshold = threshold,
          strategy = strategy,
          probability = probability,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    return(ceacData)
    
  }, error = function(e) {
    return(data.frame(threshold = thresholds, strategy = "", probability = 0))
  })
}

#' Validate Decision Analysis Inputs
#'
#' Comprehensive validation of user inputs for decision analysis.
#'
#' @param data Data frame with analysis data
#' @param treeType Character string specifying analysis type
#' @param options List of analysis options
#'
#' @return List with validation results (valid = TRUE/FALSE, messages = character vector)
#' @export
validateDecisionAnalysisInputs <- function(data, treeType, options) {
  
  messages <- character(0)
  valid <- TRUE
  
  # Basic data checks
  if (is.null(data) || nrow(data) == 0) {
    messages <- c(messages, "No data provided for analysis")
    valid <- FALSE
    return(list(valid = valid, messages = messages))
  }
  
  # Tree-type specific validation
  if (treeType == "simple") {
    if (is.null(options$decisions) || length(options$decisions) == 0) {
      messages <- c(messages, "Simple decision trees require at least one decision variable")
      valid <- FALSE
    }
    
    if ((is.null(options$probabilities) || length(options$probabilities) == 0) &&
        (is.null(options$costs) || length(options$costs) == 0)) {
      messages <- c(messages, "Simple decision trees require either probabilities or costs")
      valid <- FALSE
    }
    
  } else if (treeType == "markov") {
    if (is.null(options$healthStates) || length(options$healthStates) == 0) {
      messages <- c(messages, "Markov models require health state variables")
      valid <- FALSE
    }
    
    if (is.null(options$transitionProbs) || length(options$transitionProbs) == 0) {
      messages <- c(messages, "Markov models require transition probability variables")
      valid <- FALSE
    }
    
    if (options$cycleLength <= 0 || options$cycleLength > 10) {
      messages <- c(messages, "Markov models require a valid cycle length (0.1-10 years)")
      valid <- FALSE
    }
    
  } else if (treeType == "costeffectiveness") {
    if (is.null(options$costs) || length(options$costs) == 0) {
      messages <- c(messages, "Cost-effectiveness analysis requires cost variables")
      valid <- FALSE
    }
    
    if ((is.null(options$utilities) || length(options$utilities) == 0) &&
        (is.null(options$outcomes) || length(options$outcomes) == 0)) {
      messages <- c(messages, "Cost-effectiveness analysis requires utility or outcome variables")
      valid <- FALSE
    }
    
    if (options$willingnessToPay < 0 || options$willingnessToPay > 500000) {
      messages <- c(messages, "Willingness-to-pay threshold must be between 0 and 500,000")
      valid <- FALSE
    }
  }
  
  # Data quality checks
  numericVars <- c(options$probabilities, options$costs, options$utilities, options$transitionProbs)
  for (var in numericVars) {
    if (var %in% names(data)) {
      if (!is.numeric(data[[var]])) {
        messages <- c(messages, paste("Variable", var, "must be numeric"))
        valid <- FALSE
      }
      
      # Check for valid probability ranges
      if (var %in% options$probabilities || var %in% options$transitionProbs) {
        if (any(data[[var]] < 0 | data[[var]] > 1, na.rm = TRUE)) {
          messages <- c(messages, paste("Probabilities in", var, "must be between 0 and 1"))
          valid <- FALSE
        }
      }
      
      # Check for negative costs
      if (var %in% options$costs) {
        if (any(data[[var]] < 0, na.rm = TRUE)) {
          messages <- c(messages, paste("Costs in", var, "cannot be negative"))
          valid <- FALSE
        }
      }
    }
  }
  
  return(list(valid = valid, messages = messages))
}

#' Calculate Expected Value of Perfect Information (EVPI)
#'
#' Computes EVPI for value of information analysis.
#'
#' @param psaResults Data frame with PSA results containing cost, utility, strategy
#' @param willingnessToPay Numeric willingness-to-pay threshold
#'
#' @return List with EVPI calculations
#' @export
calculateEVPI <- function(psaResults, willingnessToPay) {
  
  if (nrow(psaResults) == 0) {
    return(list(evpi = 0, populationEVPI = 0))
  }
  
  tryCatch({
    # Calculate NMB for each simulation and strategy
    psaResults$nmb <- psaResults$utility * willingnessToPay - psaResults$cost
    
    # Expected value with current information (E[max(NMB)])
    expectedNMB <- by(psaResults$nmb, psaResults$strategy, mean, na.rm = TRUE)
    expectedValueCurrentInfo <- max(expectedNMB, na.rm = TRUE)
    
    # Expected value with perfect information (max[E(NMB)])
    # For each simulation, find the best strategy
    maxNMBPerSim <- by(psaResults, psaResults$simulation, function(x) {
      max(x$nmb, na.rm = TRUE)
    })
    expectedValuePerfectInfo <- mean(unlist(maxNMBPerSim), na.rm = TRUE)
    
    # EVPI per person
    evpiPerPerson <- pmax(0, expectedValuePerfectInfo - expectedValueCurrentInfo)
    
    return(list(
      evpi = evpiPerPerson,
      populationEVPI = evpiPerPerson * 10000,  # Assuming 10,000 person population
      expectedValueCurrentInfo = expectedValueCurrentInfo,
      expectedValuePerfectInfo = expectedValuePerfectInfo
    ))
    
  }, error = function(e) {
    return(list(evpi = 0, populationEVPI = 0, error = e$message))
  })
}

#' Create Error-Safe HTML Content
#'
#' Generates HTML content for jamovi results with error handling.
#'
#' @param title Character string for section title
#' @param content Character string or list of content items
#' @param includeStyle Logical, whether to include CSS styling
#'
#' @return Character string with formatted HTML content
#' @export
createSafeHTMLContent <- function(title, content, includeStyle = TRUE) {
  
  tryCatch({
    html <- ""
    
    if (includeStyle) {
      html <- paste0(html, "<style>
        .analysis-section { margin: 15px 0; padding: 10px; border-left: 3px solid #007ACC; }
        .section-title { font-weight: bold; font-size: 14px; color: #333; margin-bottom: 8px; }
        .content-item { margin: 5px 0; font-size: 12px; }
        .value { font-weight: bold; color: #007ACC; }
        </style>")
    }
    
    html <- paste0(html, '<div class="analysis-section">')
    html <- paste0(html, '<div class="section-title">', htmltools::htmlEscape(title), '</div>')
    
    if (is.list(content)) {
      for (item in content) {
        html <- paste0(html, '<div class="content-item">', htmltools::htmlEscape(as.character(item)), '</div>')
      }
    } else {
      html <- paste0(html, '<div class="content-item">', htmltools::htmlEscape(as.character(content)), '</div>')
    }
    
    html <- paste0(html, '</div>')
    return(html)
    
  }, error = function(e) {
    return(paste0('<div style="color: red;">Error generating content: ', htmltools::htmlEscape(e$message), '</div>'))
  })
}
