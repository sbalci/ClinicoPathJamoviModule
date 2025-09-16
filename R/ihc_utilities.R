# ============================================================================
# IHC ANALYSIS UTILITY FUNCTIONS
# ============================================================================
# This file contains shared utility functions used by IHC analysis functions
# Functions are specific to immunohistochemistry analysis workflows

#' Validate IHC Data Requirements
#'
#' @param data The dataset
#' @param markers Vector of marker column names
#' @return List with validation results (valid = TRUE/FALSE, message = character)
#' @export
validateIHCData <- function(data, markers) {
    if (is.null(data) || nrow(data) < 3) {
        return(list(
            valid = FALSE,
            message = "At least 3 samples are required for IHC analysis"
        ))
    }

    if (length(markers) < 2) {
        return(list(
            valid = FALSE,
            message = "At least 2 IHC markers are required for clustering analysis"
        ))
    }

    # Check if marker columns exist
    missing_markers <- markers[!markers %in% names(data)]
    if (length(missing_markers) > 0) {
        return(list(
            valid = FALSE,
            message = paste("Missing marker columns:", paste(missing_markers, collapse = ", "))
        ))
    }

    return(list(valid = TRUE, message = NULL))
}

#' Convert IHC Marker Data to Numeric Matrix
#'
#' @param data The dataset
#' @param markers Vector of marker column names
#' @param id_variable Optional ID variable name for row names
#' @return Numeric matrix with markers as columns
#' @export
convertIHCToNumeric <- function(data, markers, id_variable = NULL) {
    ihc_matrix <- matrix(0, nrow = nrow(data), ncol = length(markers))
    colnames(ihc_matrix) <- markers

    for (i in seq_along(markers)) {
        marker_data <- data[[markers[i]]]

        if (is.factor(marker_data) || is.character(marker_data)) {
            # Convert categorical to numeric
            levels <- sort(unique(marker_data[!is.na(marker_data)]))
            numeric_values <- as.numeric(factor(marker_data, levels = levels)) - 1
        } else {
            numeric_values <- as.numeric(marker_data)
        }

        ihc_matrix[, i] <- numeric_values
    }

    # Add row names if ID variable is provided
    if (!is.null(id_variable) && id_variable != "" && id_variable %in% names(data)) {
        rownames(ihc_matrix) <- as.character(data[[id_variable]])
    }

    return(ihc_matrix)
}

#' Calculate Distance Matrix for IHC Data
#'
#' @param data Numeric matrix or data frame
#' @param method Distance method: "gower", "jaccard", or "euclidean"
#' @return Distance matrix
#' @export
calculateIHCDistance <- function(data, method = "gower") {
    if (!requireNamespace("cluster", quietly = TRUE)) {
        stop("Package 'cluster' is required for distance calculations")
    }

    if (method == "gower") {
        dist_matrix <- cluster::daisy(data, metric = "gower")
    } else if (method == "jaccard") {
        # Binary jaccard for IHC data
        binary_data <- data > 0
        dist_matrix <- dist(binary_data, method = "binary")
    } else {
        dist_matrix <- dist(data, method = "euclidean")
    }

    return(dist_matrix)
}

#' Calculate H-Score from Intensity and Proportion
#'
#' @param intensity Staining intensity (typically 0-3)
#' @param proportion Proportion of positive cells (0-100)
#' @return H-Score value
#' @export
calculateHScore <- function(intensity, proportion) {
    # H-Score = (% of cells with intensity 1 × 1) + (% intensity 2 × 2) + (% intensity 3 × 3)
    # Simplified calculation when we have overall intensity and proportion
    hscore <- intensity * proportion / 100 * 100
    return(round(hscore))
}

#' Get Professional IHC Visualization Theme
#'
#' @return ggplot2 theme object
#' @export
getIHCTheme <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        return(NULL)
    }

    ggplot2::theme_minimal() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 11),
        axis.text = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(size = 11),
        legend.text = ggplot2::element_text(size = 10),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "gray80", fill = NA, size = 0.5)
    )
}

#' Get Professional Color Palette for IHC Visualizations
#'
#' @param n Number of colors needed
#' @return Vector of color codes
#' @export
getIHCColorPalette <- function(n = 8) {
    # Professional color palette for IHC visualizations
    colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D",
               "#6A994E", "#BC4B51", "#5D576B", "#F4D35E")

    if (n <= length(colors)) {
        return(colors[1:n])
    } else {
        # Use color ramp for more colors
        return(grDevices::colorRampPalette(colors)(n))
    }
}

#' Generate IHC Data Requirements HTML
#'
#' @param specific_requirements Optional additional requirements text
#' @return HTML string with requirements
#' @export
showIHCDataRequirements <- function(specific_requirements = NULL) {
    base_html <- "
    <h3>IHC Analysis Requirements</h3>
    <p><b>Minimum Data Requirements:</b></p>
    <ul>
        <li>At least 3 samples (cases)</li>
        <li>At least 2 IHC markers</li>
        <li>Marker data should be categorical (0/1/2/3) or continuous (H-scores)</li>
    </ul>"

    if (!is.null(specific_requirements)) {
        base_html <- paste0(base_html, specific_requirements)
    }

    return(base_html)
}

#' Calculate Silhouette Analysis for Clustering Quality
#'
#' @param clusters Vector of cluster assignments
#' @param dist_matrix Distance matrix
#' @return List with silhouette results and interpretation
#' @export
calculateIHCSilhouette <- function(clusters, dist_matrix) {
    if (!requireNamespace("cluster", quietly = TRUE)) {
        return(NULL)
    }

    if (length(unique(clusters)) < 2 || length(unique(clusters)) >= length(clusters)) {
        return(NULL)
    }

    sil <- cluster::silhouette(clusters, dist_matrix)
    avg_width <- mean(sil[, 3])

    return(list(
        silhouette = sil,
        avg_width = avg_width,
        interpretation = if (avg_width > 0.7) "Strong structure"
                        else if (avg_width > 0.5) "Reasonable structure"
                        else if (avg_width > 0.25) "Weak structure"
                        else "No substantial structure"
    ))
}

#' Format P-values Consistently for IHC Reports
#'
#' @param p P-value
#' @return Formatted p-value string
#' @export
formatIHCPValue <- function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.001) return("< 0.001")
    if (p < 0.01) return(sprintf("%.3f", p))
    if (p < 0.05) return(sprintf("%.3f", p))
    return(sprintf("%.3f", p))
}

#' Get Significance Stars for P-values
#'
#' @param p P-value
#' @return Significance stars string
#' @export
getIHCSignificanceStars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01) return("**")
    if (p < 0.05) return("*")
    return("ns")
}

#' Convert IHC Marker Data Types Safely
#'
#' @param marker_data Vector of marker values
#' @return Numeric vector
#' @export
convertIHCMarkerToNumeric <- function(marker_data) {
    if (is.factor(marker_data) || is.character(marker_data)) {
        # Convert categorical to numeric
        levels <- sort(unique(marker_data[!is.na(marker_data)]))
        numeric_values <- as.numeric(factor(marker_data, levels = levels)) - 1
    } else {
        numeric_values <- as.numeric(marker_data)
    }
    return(numeric_values)
}

#' Validate IHC Clustering Parameters
#'
#' @param data Data matrix
#' @param method Clustering method
#' @param k Number of clusters
#' @return List with validation results
#' @export
validateIHCClustering <- function(data, method, k = NULL) {
    if (nrow(data) < 3) {
        return(list(valid = FALSE, message = "At least 3 samples required for clustering"))
    }

    if (ncol(data) < 2) {
        return(list(valid = FALSE, message = "At least 2 markers required for clustering"))
    }

    if (!is.null(k) && (k < 2 || k >= nrow(data))) {
        return(list(valid = FALSE, message = "Number of clusters must be between 2 and n-1"))
    }

    # Check for valid method
    valid_methods <- c("hierarchical", "kmeans", "pam")
    if (!method %in% valid_methods) {
        return(list(valid = FALSE, message = paste("Method must be one of:", paste(valid_methods, collapse = ", "))))
    }

    return(list(valid = TRUE, message = NULL))
}

#' Calculate IHC Marker Summary Statistics
#'
#' @param marker_data Numeric vector of marker values
#' @param marker_name Name of the marker
#' @return List with summary statistics
#' @export
calculateIHCMarkerSummary <- function(marker_data, marker_name) {
    clean_data <- marker_data[!is.na(marker_data)]

    if (length(clean_data) == 0) {
        return(list(
            marker = marker_name,
            n = 0,
            mean = NA,
            median = NA,
            sd = NA,
            min = NA,
            max = NA,
            positive_rate = NA
        ))
    }

    # Calculate positivity rate (> 0)
    positive_count <- sum(clean_data > 0)
    positive_rate <- (positive_count / length(clean_data)) * 100

    return(list(
        marker = marker_name,
        n = length(clean_data),
        mean = mean(clean_data),
        median = median(clean_data),
        sd = sd(clean_data),
        min = min(clean_data),
        max = max(clean_data),
        positive_rate = positive_rate
    ))
}