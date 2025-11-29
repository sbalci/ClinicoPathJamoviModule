
#' @noRd
jrecodeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jrecodeClass",
    inherit = jrecodeBase,
    public = list(
        init = function(noThrow = FALSE) {
            # Try the default init; if the selected variable is missing (dataset changed),
            # clear dep so we can show instructions instead of throwing an error.
            result <- try(super$init(noThrow = noThrow), silent = TRUE)
            if (inherits(result, "try-error")) {
                err <- attr(result, "condition")
                msg <- if (!is.null(err)) conditionMessage(err) else as.character(result)
                if (!is.null(msg) && grepl("not present in the dataset", msg, fixed = TRUE)) {
                    opt <- self$options$option("dep")
                    if (!is.null(opt))
                        opt$value <- NULL
                    private$.status <- "none"
                    return(super$init(noThrow = noThrow))
                }
                stop(err)
            }
            invisible(result)
        }
    ),
    private = list(
        .toCharPreserveNA = function(x) {
            out <- as.character(x)
            out[is.na(x)] <- NA_character_
            out
        },

        .escapeQuotes = function(x) {
            gsub("'", "\\\\'", x, fixed = TRUE)
        },

        .init = function() {
            # Show instructions when no variable selected
            if (is.null(self$options$dep)) {
                self$results$instructions$setContent(paste0(
                    "<div style='padding: 15px;'>",
                    "<h3>Interactive Recoding Tool</h3>",
                    "<p>This tool helps you recode categorical variables easily, similar to <code>questionr::irec</code>.</p>",
                    "<h4>How to Use:</h4>",
                    "<ol>",
                    "<li><b>Select a variable</b> - Choose a categorical variable to recode</li>",
                    "<li><b>View levels</b> - See all available levels and their frequencies</li>",
                    "<li><b>Define rules</b> - Write recoding rules (one per line):<br>",
                    "<code>oldvalue -&gt; newvalue</code><br>",
                    "Example:<br>",
                    "<code>setosa -&gt; S<br>versicolor -&gt; VC<br>virginica -&gt; VG</code></li>",
                    "<li><b>Preview</b> - Check the recoding in the comparison table</li>",
                    "<li><b>Create variable</b> - Optionally generate code to create a new variable</li>",
                    "</ol>",
                    "<p><b>Note:</b> This tool generates code you can apply via jamovi's Compute or an R console.</p>",
                    "</div>"
                ))
                self$results$instructions$setVisible(TRUE)
                self$results$levels_table$setVisible(FALSE)
                self$results$code_output$setVisible(FALSE)
                self$results$comparison$setVisible(FALSE)
                self$results$notices$setVisible(FALSE)
                return()
            }

            # Configure visibility based on options
            self$results$instructions$setVisible(FALSE)
            self$results$levels_table$setVisible(self$options$show_levels)
            self$results$code_output$setVisible(self$options$show_code)
            self$results$comparison$setVisible(self$options$show_table)

            # Populate levels table
            if (self$options$show_levels) {
                private$.populateLevelsTable()
            }
        },

        .run = function() {
            dep <- self$options$dep
            if (is.null(dep)) return()

            # Get variable data
            var <- self$data[[dep]]

            # Pre-fill recode rules with available levels if empty
            private$.prefillRulesFromLevels(var)

            # Parse recoding rules
            rules_result <- private$.parseRules(self$options$recode_rules, var)

            # Show validation notices if any
            if (length(rules_result$notices) > 0) {
                notices_html <- paste0(
                    "<div style='padding: 10px;'>",
                    paste(rules_result$notices, collapse = "<br>"),
                    "</div>"
                )
                self$results$notices$setContent(notices_html)
                self$results$notices$setVisible(TRUE)
            } else {
                self$results$notices$setVisible(FALSE)
            }

            # If rules are invalid, show error and stop
            if (!rules_result$valid) {
                if (self$options$show_code) {
                    self$results$code_output$setContent(paste0(
                        "# Error: Invalid recoding rules\n",
                        "# ", paste(rules_result$notices, collapse = "\n# ")
                    ))
                }
                return()
            }

            # Perform recoding
            recoded_var <- private$.applyRecode(var, rules_result$rules_map, self$options$else_level)

            # Generate R code
            if (self$options$show_code) {
                code <- private$.generateCode(dep, rules_result$rules_map, self$options$else_level)
                self$results$code_output$setContent(code)
            }

            # Update comparison table
            if (self$options$show_table) {
                private$.updateComparisonTable(var, recoded_var)
            }

            # Create new variable if requested
            if (self$options$recoded_output) {
                private$.createNewVariable(recoded_var, dep)
            }
        },

        .prefillRulesFromLevels = function(var) {
            current_rules <- self$options$recode_rules
            if (!is.null(current_rules) && trimws(current_rules) != "")
                return()

            # Derive available levels from factor or unique values
            available_levels <- levels(var)
            if (is.null(available_levels)) {
                available_levels <- unique(var)
            }
            available_levels <- private$.toCharPreserveNA(available_levels)
            available_levels <- available_levels[!is.na(available_levels)]

            if (length(available_levels) == 0)
                return()

            template <- paste(paste0(available_levels, " -> ", available_levels),
                              collapse = "\n")

            option <- self$options$option("recode_rules")
            if (!is.null(option)) {
                option$value <- template
            }
        },

        .populateLevelsTable = function() {
            dep <- self$options$dep
            if (is.null(dep)) return()

            var <- self$data[[dep]]

            # Get frequency table
            freq_table <- table(var, useNA = "ifany")
            total <- length(var)

            # Clear existing rows
            self$results$levels_table$deleteRows()

            # Add rows for each level
            for (i in seq_along(freq_table)) {
                level_name <- names(freq_table)[i]
                if (is.na(level_name)) level_name <- "<NA>"

                count <- as.integer(freq_table[i])
                percent <- count / total

                self$results$levels_table$addRow(rowKey = i, values = list(
                    level = level_name,
                    count = count,
                    percent = percent
                ))
            }
        },

        .parseRules = function(rules_str, var) {
            # Initialize result
            result <- list(
                valid = FALSE,
                rules_map = list(),
                notices = character()
            )

            # Clean up input
            rules_str <- trimws(rules_str)

            # If empty, return invalid
            if (rules_str == "") {
                result$notices <- c(result$notices,
                    "<span style='color: orange;'>⚠ No recoding rules defined</span>")
                return(result)
            }

            # Split into lines
            lines <- strsplit(rules_str, "\n")[[1]]
            lines <- trimws(lines)
            lines <- lines[lines != ""]  # Remove empty lines

            if (length(lines) == 0) {
                result$notices <- c(result$notices,
                    "<span style='color: orange;'>⚠ No valid rules found</span>")
                return(result)
            }

            # Get available levels
            available_levels <- levels(var)
            if (is.null(available_levels)) {
                available_levels <- as.character(unique(var))
            }

            # Parse each line
            rules_map <- list()
            invalid_lines <- character()
            unknown_levels <- character()

            for (line in lines) {
                # Try to parse with different separators (colon is primary)
                parts <- strsplit(line, ":", fixed = TRUE)[[1]]

                if (length(parts) != 2) {
                    # Try semicolon
                    parts <- strsplit(line, ";", fixed = TRUE)[[1]]
                    if (length(parts) != 2) {
                        # Try arrow formats for backward compatibility
                        parts <- strsplit(line, "->", fixed = TRUE)[[1]]
                        if (length(parts) != 2) {
                            parts <- strsplit(line, "=>", fixed = TRUE)[[1]]
                            if (length(parts) != 2) {
                                parts <- strsplit(line, "=", fixed = TRUE)[[1]]
                            }
                        }
                    }
                }

                if (length(parts) == 2) {
                    old_value <- trimws(parts[1])
                    new_value <- trimws(parts[2])

                    # Remove surrounding quotes if present
                    old_value <- gsub("^['\"]|['\"]$", "", old_value)
                    new_value <- gsub("^['\"]|['\"]$", "", new_value)

                    # Validate old value exists
                    if (!(old_value %in% available_levels)) {
                        unknown_levels <- c(unknown_levels, old_value)
                    }

                    # Store mapping
                    rules_map[[old_value]] <- new_value
                } else {
                    invalid_lines <- c(invalid_lines, line)
                }
            }

            # Check for issues
            has_errors <- FALSE

            if (length(invalid_lines) > 0) {
                result$notices <- c(result$notices,
                    paste0("<span style='color: red;'>✗ Invalid format in ", length(invalid_lines),
                           " line(s). Expected: oldvalue -> newvalue</span>"))
                has_errors <- TRUE
            }

            if (length(unknown_levels) > 0) {
                result$notices <- c(result$notices,
                    paste0("<span style='color: orange;'>⚠ Unknown levels: ",
                           paste(unknown_levels, collapse = ", "), "</span>"))
            }

            if (length(rules_map) == 0) {
                result$notices <- c(result$notices,
                    "<span style='color: red;'>✗ No valid recoding rules found</span>")
                has_errors <- TRUE
            }

            # Return result
            if (!has_errors && length(rules_map) > 0) {
                result$valid <- TRUE
                result$rules_map <- rules_map
                result$notices <- c(result$notices,
                    paste0("<span style='color: green;'>✓ ", length(rules_map),
                           " recoding rule(s) parsed successfully</span>"))
            }

            return(result)
        },

        .applyRecode = function(var, rules_map, else_level) {
            # Convert to character for easier handling
            var_char <- private$.toCharPreserveNA(var)

            # Create result vector (copy original)
            result <- var_char

            # Apply recoding rules
            for (old_val in names(rules_map)) {
                result[var_char == old_val] <- rules_map[[old_val]]
            }

            # Handle unmatched values
            matched <- !is.na(var_char) & var_char %in% names(rules_map)
            unmatched <- !is.na(var_char) & !matched

            if (else_level == "na") {
                result[unmatched] <- NA_character_
            } else if (else_level == "other") {
                result[unmatched] <- "Other"
            }
            # else_level == "copy": already done (result starts as copy of original)

            return(result)
        },

        .generateCode = function(var_name, rules_map, else_level) {
            # Determine new variable name
            new_var <- self$options$new_var_name
            if (is.null(new_var) || trimws(new_var) == "") {
                new_var <- paste0(var_name, "_recoded")
            }

            # Build dplyr::recode() arguments
            recode_args <- character()
            for (old_val in names(rules_map)) {
                new_val <- rules_map[[old_val]]
                safe_old <- private$.escapeQuotes(old_val)
                safe_new <- private$.escapeQuotes(new_val)
                # Properly quote both old and new values
                recode_args <- c(recode_args,
                    paste0("  '", safe_old, "' = '", safe_new, "'"))
            }

            # Add default argument
            default_arg <- ""
            if (else_level == "na") {
                default_arg <- ",\n  .default = NA_character_"
            } else if (else_level == "other") {
                default_arg <- ",\n  .default = 'Other'"
            }

            # Generate code
            code <- paste0(
                "# Recoding variable: ", var_name, "\n",
                "# Replace `data` with your dataset name if needed\n",
                "library(dplyr)\n\n",
                "data$", new_var, " <- dplyr::recode(\n",
                "  data$", var_name, ",\n",
                paste(recode_args, collapse = ",\n"),
                default_arg, "\n",
                ")\n\n",
                "# Alternatively, using base R:\n",
                "# data$", new_var, " <- data$", var_name, "\n"
            )

            # Add base R version
            for (old_val in names(rules_map)) {
                new_val <- rules_map[[old_val]]
                safe_old <- private$.escapeQuotes(old_val)
                safe_new <- private$.escapeQuotes(new_val)
                code <- paste0(code,
                    "# data$", new_var, "[data$", var_name, " == '", safe_old,
                    "'] <- '", safe_new, "'\n")
            }

            if (else_level == "na") {
                code <- paste0(code,
                    "# data$", new_var, "[!(data$", var_name, " %in% c(",
                    paste(paste0("'", names(rules_map), "'"), collapse = ", "),
                    "))] <- NA\n")
            } else if (else_level == "other") {
                code <- paste0(code,
                    "# data$", new_var, "[!(data$", var_name, " %in% c(",
                    paste(paste0("'", names(rules_map), "'"), collapse = ", "),
                    "))] <- 'Other'\n")
            }

            return(code)
        },

        .updateComparisonTable = function(original, recoded) {
            # Clear existing rows
            self$results$comparison$deleteRows()

            # Create frequency table of combinations
            df <- data.frame(
                original = private$.toCharPreserveNA(original),
                recoded = private$.toCharPreserveNA(recoded),
                stringsAsFactors = FALSE
            )

            # Count combinations
            freq <- df %>%
                group_by(original, recoded) %>%
                summarise(count = n(), .groups = "drop") %>%
                arrange(original)

            total <- nrow(df)

            # Add rows to table
            for (i in seq_len(nrow(freq))) {
                self$results$comparison$addRow(rowKey = i, values = list(
                    original = freq$original[i],
                    recoded = freq$recoded[i],
                    count = freq$count[i],
                    percent = freq$count[i] / total
                ))
            }
        },

        .createNewVariable = function(recoded_var, source_var) {
            # Determine new variable name
            new_var_name <- self$options$new_var_name
            if (is.null(new_var_name) || trimws(new_var_name) == "") {
                new_var_name <- paste0(source_var, "_recoded")
            }
            new_var_name <- trimws(new_var_name)

            # Validate variable name
            if (!grepl("^[a-zA-Z][a-zA-Z0-9_]*$", new_var_name)) {
                notice <- paste0(
                    "<span style='color: red;'>✗ Invalid variable name: '", new_var_name,
                    "'. Must start with a letter and contain only letters, numbers, and underscores.</span>"
                )
                current_notices <- self$results$notices$content
                if (is.null(current_notices) || current_notices == "") {
                    self$results$notices$setContent(paste0("<div style='padding: 10px;'>", notice, "</div>"))
                } else {
                    self$results$notices$setContent(paste0(current_notices, "<br>", notice))
                }
                self$results$notices$setVisible(TRUE)
                return()
            }

            # Use jamovi's transform/computed variable system
            # Create a new column in the dataset
            # Note: This requires the dataset to be modifiable
            # In jamovi, this is typically done through the UI's transform feature
            # For now, we'll create the code that users can apply

            # Show a notice about how to create the variable
            notice <- paste0(
                "<div style='padding: 10px; background-color: #e7f3ff; border-left: 4px solid #2196F3;'>",
                "<b>To create the new variable '", new_var_name, "':</b><br>",
                "1. Copy the generated R code below<br>",
                "2. Use Data → Compute to create a new variable<br>",
                "3. Or run the code in an R console with your data<br>",
                "<br><i>Note: Direct variable creation in jamovi requires additional setup. ",
                "This analysis generates the code for you to apply.</i>",
                "</div>"
            )

            current_notices <- self$results$notices$content
            if (is.null(current_notices) || current_notices == "") {
                self$results$notices$setContent(notice)
            } else {
                self$results$notices$setContent(paste0(current_notices, "<br>", notice))
            }
            self$results$notices$setVisible(TRUE)
        }
    )
)
