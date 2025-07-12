#' @title Find Retracted Papers from DOI
#' @description This module checks DOIs against retraction databases to identify 
#' retracted publications. It supports OpenRetractions database and can optionally 
#' retrieve PubMed IDs for valid DOIs. The function includes robust error handling, 
#' DOI format validation, and rate limiting for API calls.
#' @return A results object containing retraction status, details, and optional PubMed IDs
#' @examples 
#' \donttest{
#' # Example 1: Basic retraction check
#' data <- data.frame(
#'   doi = c("10.1126/science.aac4716", "10.1038/nature12373", "10.1016/j.cell.2014.09.045")
#' )
#' result <- retracted(data = data, doi = "doi")
#' 
#' # Example 2: Include PubMed IDs
#' result_with_pmid <- retracted(
#'   data = data, 
#'   doi = "doi",
#'   pmid = TRUE
#' )
#' 
#' # Example 3: Using different database
#' result_rw <- retracted(
#'   data = data,
#'   doi = "doi", 
#'   database = "rw"
#' )
#' }
#' @importFrom utils URLencode
#' @importFrom httr GET status_code content timeout
#' @importFrom rcrossref id_converter

# Helper function to validate DOI format
.validate_doi <- function(doi) {
    if (is.na(doi) || doi == "") return(FALSE)
    
    # Clean DOI first
    clean_doi <- gsub("^(https?://)?doi.org/", "", doi)
    clean_doi <- gsub("^doi:", "", clean_doi)
    
    # Basic DOI format validation (prefix/suffix pattern)
    doi_pattern <- "^10\\.[0-9]{4,}/[-._;()/:a-zA-Z0-9]+$"
    return(grepl(doi_pattern, clean_doi))
}

# Helper function to check retractions with improved error handling
.check_retractions <- function(dois, database = "or") {
    # Validate input
    if (length(dois) == 0) {
        stop("No DOIs provided for checking")
    }
    
    # Check dependencies
    if (!requireNamespace("httr", quietly = TRUE)) {
        stop("Package 'httr' is required for API calls. Please install it.")
    }
    
    # Initialize results
    results <- data.frame(
        doi = dois,
        status = "Not checked",
        retracted = FALSE,
        error_message = "",
        stringsAsFactors = FALSE
    )
    
    # Validate DOI formats
    for (i in seq_along(dois)) {
        if (!.validate_doi(dois[i])) {
            results$status[i] <- "Invalid DOI format"
            results$error_message[i] <- "DOI format validation failed"
            next
        }
        
        doi <- dois[i]
        if (is.na(doi) || doi == "") {
            results$status[i] <- "Empty DOI"
            next
        }
        
        # Clean DOI
        clean_doi <- gsub("^(https?://)?doi.org/", "", doi)
        clean_doi <- gsub("^doi:", "", clean_doi)
        
        # Select API endpoint
        if (database == "or") {
            url <- paste0("https://api.openretractions.com/doi/", utils::URLencode(clean_doi))
        } else {
            # For RetractionWatch, use OpenRetractions as fallback for now
            # In future versions, could add RetractionWatch API if available
            url <- paste0("https://api.openretractions.com/doi/", utils::URLencode(clean_doi))
        }
        
        # Add rate limiting - wait between API calls
        if (i > 1) Sys.sleep(0.1)  # 100ms delay
        
        tryCatch({
            response <- httr::GET(url, httr::timeout(10))
            
            if (httr::status_code(response) == 200) {
                content <- httr::content(response, as = "parsed")
                
                if (!is.null(content$retracted) && isTRUE(content$retracted)) {
                    results$status[i] <- "Retracted"
                    results$retracted[i] <- TRUE
                } else {
                    results$status[i] <- "Not retracted"
                }
            } else if (httr::status_code(response) == 404) {
                results$status[i] <- "DOI not found"
                results$error_message[i] <- "DOI not found in database"
            } else {
                results$status[i] <- "API error"
                results$error_message[i] <- paste("HTTP", httr::status_code(response))
            }
        }, error = function(e) {
            results$status[i] <<- "API call failed"
            results$error_message[i] <<- paste("Error:", e$message)
        })
    }
    
    return(results)
}

retractedClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "retractedClass",
    inherit = retractedBase,
    private = list(
        .run = function() {

            # Show enhanced instructions if no DOI selected
            if (is.null(self$options$doi)) {
                todo <- "
                <div style='font-family: Arial, sans-serif; color: #2c3e50;'>
                  <h2>Check DOIs for Retractions</h2>
                  <p>This tool checks DOIs against retraction databases to identify retracted publications.</p>
                  <p><strong>Instructions:</strong></p>
                  <ul>
                    <li>Select a variable containing DOI strings</li>
                    <li>Choose retraction database (OpenRetractions recommended)</li>
                    <li>Optionally add PubMed IDs to results</li>
                  </ul>
                  <p><strong>Supported DOI formats:</strong></p>
                  <ul>
                    <li>10.1000/journal.example.1234567</li>
                    <li>doi:10.1000/journal.example.1234567</li>
                    <li>https://doi.org/10.1000/journal.example.1234567</li>
                  </ul>
                  <hr>
                </div>"
                self$results$todo$setContent(todo)
                return()
            }

            # Get data
            data <- self$data
            dois <- data[[self$options$doi]]
            
            # Check for empty data
            if (length(dois) == 0)
                stop('No DOIs found in selected variable')

            # Remove NAs
            dois <- dois[!is.na(dois)]
            
            # Convert database option
            db <- self$options$database
            
            # Check retractions using OpenRetractions API
            results <- tryCatch({
                .check_retractions(dois, db)
            }, error = function(e) {
                stop('Error checking retractions: ', e$message)
            })

            # Get PMIDs if requested
            if (self$options$pmid) {
                # Check if rcrossref is available
                if (!requireNamespace("rcrossref", quietly = TRUE)) {
                    # Show results without PMIDs and warn about missing package
                    for (i in seq_along(results$doi)) {
                        self$results$summary$addRow(rowKey=i, values=list(
                            doi = results$doi[i],
                            status = results$status[i],
                            pmid = ""
                        ))
                    }
                    self$results$pmids$setContent("PMID lookup requires 'rcrossref' package. Please install it.")
                } else {
                    tryCatch({
                        # Only try to get PMIDs for successfully checked DOIs
                        valid_dois <- results$doi[results$status %in% c("Retracted", "Not retracted")]
                        
                        if (length(valid_dois) > 0) {
                            ids <- rcrossref::id_converter(valid_dois, type = "doi")
                            pmids <- if (!is.null(ids$records)) ids$records$pmid else character(0)
                        } else {
                            pmids <- character(0)
                        }
                        
                        # Add to results table
                        for (i in seq_along(results$doi)) {
                            # Find corresponding PMID
                            pmid_val <- ""
                            if (results$status[i] %in% c("Retracted", "Not retracted")) {
                                doi_idx <- which(valid_dois == results$doi[i])
                                if (length(doi_idx) > 0 && doi_idx[1] <= length(pmids) && !is.na(pmids[doi_idx[1]])) {
                                    pmid_val <- pmids[doi_idx[1]]
                                }
                            }
                            
                            self$results$summary$addRow(rowKey=i, values=list(
                                doi = results$doi[i],
                                status = results$status[i],
                                pmid = pmid_val
                            ))
                        }
                        
                        # Set PMIDs output
                        if (!is.null(self$options$resids) && length(pmids) > 0) {
                            self$results$resids$setValues(as.factor(pmids[!is.na(pmids)]))
                        }
                        
                        # Show PMID summary
                        found_pmids <- sum(!is.na(pmids) & pmids != "")
                        self$results$pmids$setContent(
                            paste("Found", found_pmids, "PubMed IDs out of", length(valid_dois), "valid DOIs")
                        )
                    }, error = function(e) {
                        # If PMID lookup fails, show results without PMIDs
                        for (i in seq_along(results$doi)) {
                            self$results$summary$addRow(rowKey=i, values=list(
                                doi = results$doi[i],
                                status = results$status[i],
                                pmid = ""
                            ))
                        }
                        self$results$pmids$setContent(paste("PMID lookup failed:", e$message))
                    })
                }
            } else {
                # Results without PMIDs
                for (i in seq_along(results$doi)) {
                    self$results$summary$addRow(rowKey=i, values=list(
                        doi = results$doi[i],
                        status = results$status[i]
                    ))
                }
            }

            # Show details for retracted papers
            if (any(results$retracted)) {
                details <- results[results$retracted, c("doi", "status")]
                details_html <- paste0(
                    "<table class='table table-striped'>",
                    "<thead><tr><th>DOI</th><th>Status</th></tr></thead>",
                    "<tbody>",
                    paste(apply(details, 1, function(row) {
                        paste0("<tr><td>", row[1], "</td><td>", row[2], "</td></tr>")
                    }), collapse = ""),
                    "</tbody></table>"
                )
                self$results$details$setContent(details_html)
            }
        }
    )
)