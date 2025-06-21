#' @title Eurostat Map
#' @description Creates choropleth maps using Eurostat data.
#' Supports both downloading data from Eurostat API and using local data.
#' Maps can be static or interactive with customizable styling.
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import eurostat
#' @import tmap
#' @import sf
#' @import dplyr
#' @import giscoR

eurostatmapClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "eurostatmapClass",
    inherit = eurostatmapBase,
    private = list(
        .init = function() {
            # Initialize tmap mode
            tmap::tmap_mode("plot")
        },
        
        .run = function() {
            # Get data based on user choice
            if (self$options$use_local_data) {
                if (is.null(self$options$indicator) || nrow(self$data) == 0) {
                    self$results$.setStatus("Please import your Eurostat data and select an indicator variable")
                    return()
                }
                
                # Check if geo column exists
                if (!"geo" %in% names(self$data)) {
                    self$results$.setError("Your data must contain a 'geo' column with NUTS geographic codes (e.g., 'DE', 'FR', 'ES'). Please check your data format.")
                    return()
                }
                
                plotData <- self$data
                indicator_var <- self$options$indicator
                
                # Provide feedback about local data
                self$results$.setStatus(paste("Using local data with", nrow(plotData), "observations"))
                
            } else {
                # Download from Eurostat
                dataset_id <- self$options$dataset_id
                if (is.null(dataset_id) || dataset_id == "") {
                    self$results$.setStatus("Please enter a Eurostat dataset ID (e.g., 'demo_r_gind3' for population density)")
                    return()
                }
                
                tryCatch({
                    # Check for cached data if caching is enabled
                    cache_key <- paste(dataset_id, self$options$year, sep = "_")
                    cached_data <- NULL
                    
                    if (self$options$cache_data) {
                        # Simple session-based caching
                        if (!exists(".eurostat_cache", envir = .GlobalEnv)) {
                            assign(".eurostat_cache", list(), envir = .GlobalEnv)
                        }
                        cache_env <- get(".eurostat_cache", envir = .GlobalEnv)
                        if (cache_key %in% names(cache_env)) {
                            cached_data <- cache_env[[cache_key]]
                            message("Using cached Eurostat data")
                        }
                    }
                    
                    if (is.null(cached_data)) {
                        # Download data with progress indication
                        self$results$.setStatus("Downloading data from Eurostat...")
                        
                        # Set timeout for download (default is 60 seconds)
                        options(timeout = 120)
                        
                        start_time <- Sys.time()
                        plotData <- eurostat::get_eurostat(
                            id = dataset_id, 
                            time_format = "num", 
                            cache = self$options$cache_data,
                            update_cache = FALSE
                        )
                        end_time <- Sys.time()
                        
                        download_time <- round(as.numeric(end_time - start_time, units = "secs"), 1)
                        message(paste("Downloaded", nrow(plotData), "rows in", download_time, "seconds"))
                        
                        # Filter by year if specified
                        if (!is.null(self$options$year)) {
                            # Filter by TIME_PERIOD column (the actual column name in eurostat data)
                            plotData <- plotData %>%
                                dplyr::filter(TIME_PERIOD == self$options$year)
                        }
                        
                        # Cache the filtered data if caching is enabled
                        if (self$options$cache_data && nrow(plotData) > 0) {
                            cache_env <- get(".eurostat_cache", envir = .GlobalEnv)
                            cache_env[[cache_key]] <- plotData
                            assign(".eurostat_cache", cache_env, envir = .GlobalEnv)
                            message("Data cached for future use")
                        }
                    } else {
                        plotData <- cached_data
                        message("Using cached data")
                    }
                    
                    # Use 'values' as the indicator variable for Eurostat data
                    indicator_var <- "values"
                    
                }, error = function(e) {
                    error_msg <- paste("Error downloading Eurostat data:", e$message)
                    
                    # Provide helpful error messages for common issues
                    if (grepl("timeout", e$message, ignore.case = TRUE)) {
                        error_msg <- paste(error_msg, "\n\nSuggestion: The dataset may be large. Try enabling caching or use a smaller dataset.")
                    } else if (grepl("not found", e$message, ignore.case = TRUE)) {
                        error_msg <- paste(error_msg, "\n\nSuggestion: Check if the dataset ID is correct. Use eurostat::get_eurostat_toc() to find valid IDs.")
                    } else if (grepl("network", e$message, ignore.case = TRUE)) {
                        error_msg <- paste(error_msg, "\n\nSuggestion: Check your internet connection.")
                    }
                    
                    self$results$.setError(error_msg)
                    return()
                })
            }
            
            # Populate dataset info table
            info_table <- self$results$info
            if (self$options$use_local_data) {
                info_table$setRow(rowNo = 1, values = list(
                    dataset = "Local Data",
                    title = "User Provided Data",
                    observations = nrow(plotData),
                    last_update = format(Sys.Date(), "%Y-%m-%d")
                ))
            } else {
                info_table$setRow(rowNo = 1, values = list(
                    dataset = dataset_id,
                    title = "Eurostat Dataset",
                    observations = nrow(plotData),
                    last_update = format(Sys.Date(), "%Y-%m-%d")
                ))
            }
            
            # Populate summary statistics
            if (!is.null(plotData) && indicator_var %in% names(plotData)) {
                values <- plotData[[indicator_var]]
                values <- values[!is.na(values)]
                
                if (length(values) > 0) {
                    summary_table <- self$results$summary
                    summary_table$setRow(rowNo = 1, values = list(statistic = "Min", value = min(values)))
                    summary_table$setRow(rowNo = 2, values = list(statistic = "Max", value = max(values)))
                    summary_table$setRow(rowNo = 3, values = list(statistic = "Mean", value = mean(values)))
                    summary_table$setRow(rowNo = 4, values = list(statistic = "Median", value = median(values)))
                    summary_table$setRow(rowNo = 5, values = list(statistic = "Std Dev", value = sd(values)))
                    summary_table$setRow(rowNo = 6, values = list(statistic = "Count", value = length(values)))
                }
            }
            
            # Add downloaded data to output table if requested
            if (!self$options$use_local_data && self$options$add_to_data && !is.null(plotData)) {
                downloaded_table <- self$results$downloaded_data
                
                # Clear existing columns and add new ones based on data
                downloaded_table$deleteColumns()
                
                # Add columns dynamically based on the downloaded data
                for (col_name in names(plotData)) {
                    col_type <- if (is.numeric(plotData[[col_name]])) "number" else "text"
                    downloaded_table$addColumn(name = col_name, title = col_name, type = col_type)
                }
                
                # Add rows
                for (i in seq_len(min(nrow(plotData), 1000))) {  # Limit to 1000 rows for performance
                    row_values <- as.list(plotData[i, ])
                    names(row_values) <- names(plotData)
                    downloaded_table$addRow(rowKey = i, values = row_values)
                }
                
                # Add a note about data availability
                if (nrow(plotData) > 1000) {
                    message("Downloaded data table shows first 1000 rows. Complete data used for mapping.")
                }
            }
            
            # Store data for plotting
            image <- self$results$plot
            image$setState(list(
                data = plotData,
                indicator = indicator_var
            ))
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state$data) || is.null(state$indicator)) {
                return()
            }
            
            plotData <- state$data
            indicator_var <- state$indicator
            
            # Set tmap mode based on user selection
            if (self$options$map_type == "interactive") {
                tmap::tmap_mode("view")
            } else {
                tmap::tmap_mode("plot")
            }
            
            tryCatch({
                # Get geographic level
                geo_level <- switch(self$options$geo_level,
                    "nuts0" = 0,
                    "nuts1" = 1, 
                    "nuts2" = 2,
                    "nuts3" = 3,
                    2  # default to NUTS2
                )
                
                # Download geospatial data
                geo_data <- eurostat::get_eurostat_geospatial(
                    output_class = "sf",
                    resolution = "60",
                    nuts_level = geo_level,
                    year = self$options$year %||% 2021
                )
                
                # Merge data with geospatial
                if (self$options$use_local_data) {
                    # For local data, assume there's a 'geo' column
                    if ("geo" %in% names(plotData)) {
                        map_data <- geo_data %>%
                            dplyr::left_join(plotData, by = c("geo" = "geo"))
                    } else {
                        stop("Local data must contain a 'geo' column with geographic codes")
                    }
                } else {
                    map_data <- geo_data %>%
                        dplyr::left_join(plotData, by = "geo")
                }
                
                # Create map
                color_palette <- switch(self$options$color_palette,
                    "viridis" = "viridis",
                    "plasma" = "plasma", 
                    "blues" = "Blues",
                    "reds" = "Reds",
                    "greens" = "Greens",
                    "viridis"  # default
                )
                
                # Classification method
                classification <- switch(self$options$classification_method,
                    "quantile" = "quantile",
                    "equal" = "equal",
                    "jenks" = "jenks",
                    "pretty" = "pretty",
                    "quantile"  # default
                )
                
                map <- tmap::tm_shape(map_data) +
                    tmap::tm_fill(
                        col = indicator_var,
                        style = classification,
                        n = self$options$n_classes %||% 5,
                        palette = color_palette,
                        title = self$options$map_title %||% "Eurostat Map"
                    ) +
                    tmap::tm_borders(alpha = 0.5) +
                    tmap::tm_layout(
                        title = self$options$map_title %||% "Eurostat Map",
                        title.size = 1.2,
                        legend.outside = TRUE
                    )
                
                print(map)
                return(TRUE)
                
            }, error = function(e) {
                self$results$.setError(paste("Error creating map:", e$message))
                return(FALSE)
            })
        }
    )
)