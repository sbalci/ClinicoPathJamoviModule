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

  if (is.null(treeData) || is.null(treeData$nodes) || is.null(treeData$edges)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  # Create graph object
  nodes <- treeData$nodes
  edges <- treeData$edges
  
  if (nrow(nodes) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())
  
  # Ensure column names match for igraph
  # edges needs "from", "to"
  # nodes needs "name" as first column or specific mapping
  
  # igraph expects first column of nodes to be ID matching from/to in edges
  g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  
  # Determine layout
  graph_layout <- switch(layout,
                        "horizontal" = "dendrogram",
                        "vertical" = "dendrogram",
                        "radial" = "dendrogram",
                        "dendrogram") # Default
               
  circular <- layout == "radial"
  
  # Create plot
  # layout "dendrogram" usually puts root at top (vertical) or left (horizontal)
  # But ggraph dendrogram is top-down by default. Coord_flip for horizontal?
  
  p <- ggraph::ggraph(g, layout = "dendrogram", circular = circular) + 
    ggraph::geom_edge_diagonal(ggplot2::aes(label = if(showProbabilities) label else NA),
                              arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm")),
                              end_cap = ggraph::circle(3, "mm")) +
    ggraph::geom_node_point(ggplot2::aes(color = type, shape = if(nodeShapes) type else NULL), size = 5) +
    ggraph::geom_node_label(ggplot2::aes(label = label), repel = TRUE) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom")
    
  if (layout == "horizontal" && !circular) {
    p <- p + ggplot2::coord_flip() + ggplot2::scale_y_reverse()
  }
  
  return(p)
}

#' Perform Monte Carlo Simulation for PSA (Deprecated)
#'
#' This function is deprecated. PSA is now handled internally by decisiongraphClass.
#'
#' @param numSimulations Integer
#' @param parameters List
#' @param baseResults Data frame
#' @param distributionType Character string
#'
#' @return NULL
#' @export
performMonteCarloSimulation <- function(numSimulations, parameters, baseResults,
                                       distributionType = "normal") {
  
  warning("performMonteCarloSimulation in utils is deprecated. Use decisiongraphClass internal methods.")
  return(NULL)
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

    # Get unique simulation IDs (if available)
    if (!"simulation" %in% names(psaResults)) {
      psaResults$simulation <- 1:nrow(psaResults)
    }

    for (threshold in thresholds) {
      # CRITICAL FIX: Calculate NMB for ALL strategies at this threshold
      # Then find which strategy has MAX NMB for EACH simulation

      # Create NMB matrix: rows = simulations, columns = strategies
      unique_sims <- unique(psaResults$simulation)
      nmb_matrix <- matrix(NA, nrow = length(unique_sims), ncol = length(strategies))
      colnames(nmb_matrix) <- strategies

      for (i in seq_along(strategies)) {
        strategy <- strategies[i]
        strategyData <- psaResults[psaResults$strategy == strategy, ]

        if (nrow(strategyData) > 0) {
          # Calculate NMB for this strategy at this threshold
          nmb_matrix[, i] <- strategyData$utility * threshold - strategyData$cost
        }
      }

      # CORRECT CEAC FORMULA:
      # For each simulation (row), find which strategy has highest NMB
      optimal_strategy_indices <- apply(nmb_matrix, 1, function(row) {
        if (all(is.na(row))) return(NA)
        which.max(row)
      })

      # CEAC = proportion of iterations where EACH strategy is optimal
      for (i in seq_along(strategies)) {
        probability <- mean(optimal_strategy_indices == i, na.rm = TRUE)

        ceacData <- rbind(ceacData, data.frame(
          threshold = threshold,
          strategy = strategies[i],
          probability = probability,
          stringsAsFactors = FALSE
        ))
      }
    }

    return(ceacData)

  }, error = function(e) {
    warning(paste("CEAC calculation failed:", e$message))
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
