# Helper function to check retractions
.check_retractions <- function(dois, database = "or") {
    # Initialize results
    results <- data.frame(
        doi = dois,
        status = "Not retracted",
        retracted = FALSE,
        stringsAsFactors = FALSE
    )
    
    # Check each DOI
    for (i in seq_along(dois)) {
        doi <- dois[i]
        if (is.na(doi) || doi == "") next
        
        # Clean DOI
        doi <- gsub("^(https?://)?doi.org/", "", doi)
        doi <- gsub("^doi:", "", doi)
        
        # Check against OpenRetractions API
        if (database == "or") {
            url <- paste0("https://api.openretractions.com/doi/", doi)
        } else {
            # For other databases, use OpenRetractions as fallback
            url <- paste0("https://api.openretractions.com/doi/", doi)
        }
        
        tryCatch({
            response <- httr::GET(url)
            if (httr::status_code(response) == 200) {
                content <- httr::content(response, as = "parsed")
                if (!is.null(content$retracted) && content$retracted) {
                    results$status[i] <- "Retracted"
                    results$retracted[i] <- TRUE
                }
            }
        }, error = function(e) {
            # Continue if API call fails
        })
    }
    
    return(results)
}

retractedClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "retractedClass",
    inherit = retractedBase,
    private = list(
        .run = function() {

            # Show instructions if no DOI selected
            if (is.null(self$options$doi)) {
                todo <- "
                <br>Check DOIs for Retractions
                <br><br>
                Select a variable containing DOIs to check against retraction databases.
                <br><br>
                Results will show:
                - Retraction status
                - PubMed IDs (optional)
                - Details for retracted papers
                <hr>
                "
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
                tryCatch({
                    ids <- rcrossref::id_converter(results$doi, type = "doi")
                    pmids <- ids$records$pmid
                    
                    # Add to results table
                    for (i in seq_along(results$doi)) {
                        pmid_val <- if (i <= length(pmids) && !is.na(pmids[i])) pmids[i] else ""
                        self$results$summary$addRow(rowKey=i, values=list(
                            doi = results$doi[i],
                            status = results$status[i],
                            pmid = pmid_val
                        ))
                    }
                    
                    # Set PMIDs output
                    if (!is.null(self$options$resids) && length(pmids) > 0) {
                        self$results$resids$setValues(as.factor(pmids))
                    }
                    
                    # Show PMID summary
                    self$results$pmids$setContent(
                        paste("Found", sum(!is.na(pmids)), "PubMed IDs")
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
                    self$results$pmids$setContent("PMID lookup failed")
                })
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