#' Interrater Reliability Analysis
#' @return Table
#' @export
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom irr kappa2
#' @importFrom irr kappam.fleiss
#' @importFrom irr agree
#'


# See
# \url{http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/#ordinal-data-weighted-kappa}


agreementClass <- if (requireNamespace("jmvcore")) R6::R6Class("agreementClass", 
    inherit = agreementBase, private = list(.run = function() {
        # Data definition ----
        
        
        exct <- self$options$exct
        wght <- self$options$wght
        
        mydata <- self$data
        
        formula <- jmvcore::constructFormula(terms = self$options$vars)
        
        myvars <- jmvcore::decomposeFormula(formula = formula)
        
        myvars <- unlist(myvars)
        
        ratings <- mydata %>% dplyr::select(myvars)
        
        
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
                
                
                result2 <- irr::kappa2(ratings = ratings, weight = wght)
                
                # self$results$text2$setContent(result2)
                
                
                # >=2 & categorical ----
                
                
            } else if (length(self$options$vars) >= 2) {
                # todo <- 'kappam.fleiss'
                
                # self$results$todo$setContent(todo)
                
                result2 <- irr::kappam.fleiss(ratings = ratings, exact = exct, 
                  detail = TRUE)
                
                # self$results$text2$setContent(result2)
                
            }
            
            
            
            result <- table(ratings)
            
            self$results$text$setContent(result)
            
            
            result1 <- irr::agree(ratings)
            
            # self$results$text1$setContent(result1)
            
            
            table2 <- self$results$irrtable
            table2$setRow(rowNo = 1, values = list(method = result2[["method"]], 
                subjects = result1[["subjects"]], raters = result1[["raters"]], 
                peragree = result1[["value"]], kappa = result2[["value"]], 
                z = result2[["statistic"]], p = result2[["p.value"]]))
            
        }
        
        
    }))
