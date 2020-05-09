#' Summary of Continuous Variables
#'


#'
#' 
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom purrr map
#'

summarydataClass <- if (requireNamespace("jmvcore")) R6::R6Class("summarydataClass", 
    inherit = summarydataBase, private = list(.run = function() {
        
        if (length(self$options$vars) == 0) {
            todo <- "
                <br>Welcome to ClinicoPath
                          <br><br>
                          This tool will help you to write descriptive statistics for numeric variables.
                          <br><br>
                          Please cite the packages and jamovi using references below.
                          "
            
            html <- self$results$todo
            html$setContent(todo)
            return()
            
        } else {
            todo <- ""
            html <- self$results$todo
            html$setContent(todo)
            
            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")
            
            
            mydata <- self$data
            
            myvars <- jmvcore::constructFormula(terms = self$options$vars)
            
            myvars <- jmvcore::decomposeFormula(formula = myvars)
            
            myvars <- unlist(myvars)
            
            # mysummary function
            mysummary <- function(myvar) {
                
                mean_x <- round(mean(jmvcore::toNumeric(mydata[[myvar]]), 
                  na.rm = TRUE), digits = 1)
                
                sd_x <- round(sd(x = jmvcore::toNumeric(mydata[[myvar]]), 
                  na.rm = TRUE), digits = 1)
                
                median_x <- round(median(jmvcore::toNumeric(mydata[[myvar]]), 
                  na.rm = TRUE), digits = 1)
                
                min_x <- round(min(jmvcore::toNumeric(mydata[[myvar]]), na.rm = TRUE), 
                  digits = 1)
                
                max_x <- round(max(jmvcore::toNumeric(mydata[[myvar]]), na.rm = TRUE), 
                  digits = 1)
                
                print(paste0("Mean of ", myvar, " is: ", mean_x, " ± ", sd_x, 
                  ". (Median: ", median_x, " [Min: ", min_x, " - ", "Max: ", 
                  max_x, "])", collapse = " "))
            }
            
            results <- purrr::map(.x = myvars, .f = mysummary)
            
            results <- unlist(results)
            
            self$results$text$setContent(results)
            
        }
        
        
    }))
