#' @title Table One
#'
#' @description This function creates a “Table One” – a descriptive summary table commonly used in clinicopathological research.
#' It offers several output styles using different packages (tableone, gtsummary, arsenal, and janitor).
#'
#' @return A formatted table according to the chosen style.
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric select naOmit constructFormula
#' @importFrom tableone CreateTableOne
#' @importFrom gtsummary tbl_summary as_kable_extra
#' @importFrom arsenal tableby
#' @importFrom janitor tabyl adorn_totals adorn_pct_formatting
#' @importFrom dplyr rename
#' @importFrom kableExtra kable kable_styling
#' @importFrom rlang sym
#' @importFrom stats as.formula
#' @importFrom grDevices rgb
#'
#' @export tableoneClass
#'
tableoneClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "tableoneClass",
    inherit = tableoneBase,
    private = list(
        .run = function() {
            # Check that the input data has at least one complete row.
            if (nrow(self$data) == 0) {
                stop("Error: The input data contains no (complete) rows. Please provide a valid dataset.")
            }

            # If no variables are selected, show a welcome/instructions message.
            if (is.null(self$options$vars)) {
                todo_message <- "
                <br><strong>Welcome to the ClinicoPath Table One Generator</strong>
                <br><br>
                <strong>Instructions:</strong>
                <ul>
                    <li>Select the <em>Variables</em> to include in the Table One. (Numeric, Ordinal, or Categorical)</li>
                    <li>Choose a <em>Table Style</em> for the output format.</li>
                    <li>If needed, check the option to <em>Exclude Missing Values</em> (NA). (Exclusion may remove entire cases.)</li>
                </ul>
                <br>
                Please ensure you cite the packages and jamovi as referenced below.
                "
                self$results$todo$setContent(todo_message)
                return(invisible(NULL))  # Stop further processing until variables are selected.
            } else {
                # Clear the instructions message once variables are selected.
                self$results$todo$setContent("")
            }

            # Prepare the data using user-selected variables.
            selected_vars <- self$options$vars  # Improved variable naming.
            data <- jmvcore::select(self$data, selected_vars)

            # Optionally exclude rows with missing values.
            if (isTRUE(self$options$excl)) {
                data <- jmvcore::naOmit(data)
            }

            # Retrieve the table style selected by the user.
            table_style <- self$options$sty

            # Generate the table based on the chosen style.
            if (table_style == "t1") {
                # --- Using tableone package ---
                mytable <- tryCatch({
                    tableone::CreateTableOne(data = data)
                }, error = function(e) {
                    stop("Error in tableone package: ", e$message)
                })
                self$results$tablestyle1$setContent(mytable)

            } else if (table_style == "t2") {
                # --- Using gtsummary package ---
                mytable <- tryCatch({
                    tbl <- gtsummary::tbl_summary(data = data)
                    gtsummary::as_kable_extra(tbl)
                }, error = function(e) {
                    stop("Error in gtsummary package: ", e$message)
                })
                self$results$tablestyle2$setContent(mytable)

            } else if (table_style == "t3") {
                # --- Using arsenal package ---
                formula_str <- jmvcore::constructFormula(terms = selected_vars)
                formula_obj <- as.formula(paste('~', formula_str))
                mytable <- tryCatch({
                    tab <- arsenal::tableby(formula = formula_obj,
                                            data = data,
                                            total = TRUE,
                                            digits = 1,
                                            digits.count = 0,
                                            digits.pct = 1)
                    tab_summary <- summary(tab, text = "html")
                    kableExtra::kable(tab_summary, format = "html", digits = 1, escape = FALSE)
                }, error = function(e) {
                    stop("Error in arsenal package: ", e$message)
                })
                self$results$tablestyle3$setContent(mytable)

            } else if (table_style == "t4") {
                # --- Using janitor package for frequency tables with improved spacing & styling ---
                table_list <- lapply(selected_vars, function(var) {
                    freq_table <- tryCatch({
                        table <- janitor::tabyl(data, !!rlang::sym(var))
                        table <- janitor::adorn_totals(table, "row")
                        table <- janitor::adorn_pct_formatting(table)

                        # Rename columns for consistency and clarity
                        # (If you don't have dplyr, you can use base R: colnames(table)[2:4] <- c("N", "Percent", "Valid Percent"))
                        table <- dplyr::rename(
                            table,
                            "N"            = n,
                            "Percent"      = percent,
                            "Valid Percent" = valid_percent
                        )
                        table
                    }, error = function(e) {
                        stop("Error processing variable '", var, "' with janitor: ", e$message)
                    })

                    # Add a header for clarity for each variable's table, plus a top margin.
                    header <- paste0("<h4 style='margin-top:20px;'>Frequency Table for '", var, "'</h4>")

                    # Convert to an HTML table with columns centered from the second column onward:
                    # The first column (variable level) is left-aligned, and columns 2-4 are centered.
                    styled_table <- kableExtra::kable(
                        freq_table,
                        format = "html",
                        digits = 1,
                        escape = FALSE,
                        align = c("l", "c", "c", "c")  # left, center, center, center
                    ) %>%
                        kableExtra::kable_styling(
                            bootstrap_options = c("striped", "hover"),
                            full_width = FALSE,
                            font_size = 14,
                            position = "center"
                        )

                    # Combine the header and the table with spacing (hr).
                    paste0(header, styled_table, "<br><hr style='margin:20px 0;'>")
                })

                # Join all the tables together
                mytable <- paste(table_list, collapse = "")
                self$results$tablestyle4$setContent(mytable)
            } else {
                stop("Error: Invalid table style selected. Please choose a valid style.")
            }
        } # End of .run function.
    ) # End of private list.
) # End of R6Class definition.
