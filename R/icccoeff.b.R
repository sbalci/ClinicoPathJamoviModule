#' Interclass Correlation Coefficient
#'
#' Also see \url{http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/#ordinal-data-weighted-kappa}


#'
#' 
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'



icccoeffClass <- if (requireNamespace("jmvcore")) R6::R6Class("icccoeffClass", 
    inherit = icccoeffBase, private = list(.run = function() {
        
        
        
        # 
        
    }))
