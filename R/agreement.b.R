#' @title Interrater Reliability Analysis
#' @return Table
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#' @importFrom irr kappa2 kappam.fleiss agree
#' @importFrom dplyr select group_by count
#' @importFrom htmlTable htmlTable
#' @importFrom glue glue
#'
#' @description This function calculates interrater reliability for ordinal or categorical data.
#'
#' @details The function calculates Cohen's kappa for two raters and Fleiss' kappa for three or more raters.
#'
#'


# See
# \url{http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/#ordinal-data-weighted-kappa}


agreementClass <- if (requireNamespace("jmvcore")) R6::R6Class("agreementClass",
    inherit = agreementBase, private = list(.run = function() {



        # # Error Message ----
        #
        # if (nrow(self$data) == 0) stop("Data contains no (complete) rows")
        #
        # if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) ) {
        #     # ToDo Message ----
        #     todo <- "
        #         <br>Welcome to ClinicoPath
        #                   <br><br>
        #                   This tool will help you form an Alluvial Plots.
        #                   "
        #     html <- self$results$todo
        #     html$setContent(todo)
        #
        # } else {
        #     todo <- ""
        #     html <- self$results$todo
        #     html$setContent(todo)
        #
        #
        #
        # }









        # Data definition ----


        exct <- self$options$exct
        wght <- self$options$wght

        mydata <- self$data

        formula <- jmvcore::constructFormula(terms = self$options$vars)

        myvars <- jmvcore::decomposeFormula(formula = formula)

        myvars <- unlist(myvars)

        ratings <- mydata %>% dplyr::select(myvars)


        # psych::cohen.kappa ----

        # from https://github.com/kwongwh/Kappa
        # ratings2 <- mydata[c(self$options$vars)]
        #
        # result_cohen <- psych::cohen.kappa(x = ratings2)
        #
        # self$results$result_cohen$setContent(result_cohen)


        # irr.kappa <- kappa2(vars, weight = self$options$weights)
        # if(self$options$weights == "unweighted"){
        #     n = 1
        # } else {
        #     n = 2
        # }
        #
        # table <- self$results$ka
        #
        # table$setRow(rowNo=1, values=list(
        #     weights = self$options$weights,
        #     kappa=irr.kappa$value,
        #     upper_CI=results$confid[1,3],
        #     lower_CI=results$confid[1,1],
        #     p = irr.kappa$p.value
        # ))




        if (is.null(self$options$vars) || length(self$options$vars) < 2) {
            # No variables ----

            # todo <- glue::glue( 'This function ' )

            # self$results$todo$setContent(todo)

        } else {
            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")


            # 2 & categorical ----

            if (length(self$options$vars) == 2) {
                # todo <- 'Cohen'

                # self$results$todo$setContent(todo)


                xorder <- unlist(lapply(ratings, is.ordered))

                if (wght %in% c("equal", "squared") && !all(xorder == TRUE)) stop("Use ordinal variables when using weight argument")

                if (exct == TRUE) stop("Use exact argument only >=3 variables")


                # irr::kappa2 ----

                result2 <- irr::kappa2(ratings = ratings, weight = wght)

                # self$results$text2$setContent(result2)


                # >=2 & categorical ----


            } else if (length(self$options$vars) >= 2) {
                # todo <- 'kappam.fleiss'

                # self$results$todo$setContent(todo)

                # irr::kappam.fleiss ----

                result2 <- irr::kappam.fleiss(ratings = ratings, exact = exct,
                  detail = TRUE)

                # self$results$text2$setContent(result2)

            }


            # irr::agree ----

            result <- table(ratings)

            self$results$text$setContent(result)



            result3 <- ratings %>%
                dplyr::group_by_all() %>%
                dplyr::count() %>%
                as.data.frame() %>%
                htmlTable::htmlTable()

            self$results$text2$setContent(result3)



            # freqtable <- self$results$freqtable
            #
            # data_frame <- result3
            # for (i in seq_along(data_frame[, 1, drop = T])) {
            #     freqtable$addRow(rowKey = i, values = c(data_frame[i,]))
            # }






            result1 <- irr::agree(ratings)

            # self$results$text1$setContent(result1[["value"]])

            if (result1[["value"]] > 100) {

                result1[["value"]] <- "Please check the data. It seems that observers do not agree on any cases"
            }


            # Table ----

            table2 <- self$results$irrtable



            table2$setRow(rowNo = 1, values = list(method = result2[["method"]],
                subjects = result1[["subjects"]], raters = result1[["raters"]],
                peragree = result1[["value"]], kappa = result2[["value"]],
                z = result2[["statistic"]], p = result2[["p.value"]]))

        }








        if (self$options$kripp) {
            # Convert ratings data frame to matrix
            ratings_matrix <- as.matrix(ratings)

            # Ensure numeric conversion if needed
            if (!is.numeric(ratings_matrix)) {
                # If categorical/factor data, convert to numeric codes
                ratings_matrix <- matrix(
                    as.numeric(factor(ratings_matrix)),
                    nrow = nrow(ratings_matrix),
                    ncol = ncol(ratings_matrix)
                )
            }

            # Add error handling
            tryCatch({
                # Calculate Krippendorff's alpha
                kripp_result <- irr::kripp.alpha(
                    ratings_matrix,
                    method = self$options$krippMethod
                )

                # Initialize values list for table
                values_list <- list(
                    method = paste0("Krippendorff's Alpha (", self$options$krippMethod, ")"),
                    subjects = nrow(ratings_matrix),
                    raters = ncol(ratings_matrix),
                    alpha = kripp_result$value
                )

                # Calculate bootstrap CI if requested
                if (self$options$bootstrap) {
                    set.seed(123) # for reproducibility
                    n_boot <- 1000
                    alpha_boots <- numeric(n_boot)

                    for(i in 1:n_boot) {
                        boot_indices <- sample(1:nrow(ratings_matrix), replace = TRUE)
                        boot_data <- ratings_matrix[boot_indices,]

                        boot_alpha <- try(irr::kripp.alpha(boot_data,
                                                           method = self$options$krippMethod)$value,
                                          silent = TRUE)

                        if(!inherits(boot_alpha, "try-error")) {
                            alpha_boots[i] <- boot_alpha
                        }
                    }

                    # Calculate 95% confidence intervals
                    ci <- quantile(alpha_boots, c(0.025, 0.975), na.rm = TRUE)

                    # Add CI values to list
                    values_list$ci_lower <- ci[1]
                    values_list$ci_upper <- ci[2]
                }

                # Populate results table
                krippTable <- self$results$krippTable
                krippTable$setRow(rowNo = 1, values = values_list)

            }, error = function(e) {
                # Handle any errors that occur during calculation
                errorMessage <- paste("Error calculating Krippendorff's alpha:", e$message)
                warning(errorMessage)

                # Initialize values list for error case
                values_list <- list(
                    method = paste0("Krippendorff's Alpha (", self$options$krippMethod, ")"),
                    subjects = nrow(ratings_matrix),
                    raters = ncol(ratings_matrix),
                    alpha = NA
                )

                if (self$options$bootstrap) {
                    values_list$ci_lower <- NA
                    values_list$ci_upper <- NA
                }

                # Populate table with NA values
                krippTable <- self$results$krippTable
                krippTable$setRow(rowNo = 1, values = values_list)

                # Add error message as footnote
                krippTable$addFootnote(rowNo = 1, col = "alpha", paste0("Error calculating Krippendorff's alpha: ", e$message))
            })
        }








    }))
