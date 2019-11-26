
# This file is a generated template, your changes will not be overwritten

crosstableClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "crosstableClass",
    inherit = crosstableBase,
    private = list(
        .run = function() {

            
            if (length(self$options$vars) == 0)
                return()
            
            formulaR <- jmvcore::constructFormula(terms = self$options$vars)
            
            formulaL <- jmvcore::constructFormula(terms = self$options$group)
            
            formula <- paste(formulaL, '~', formulaR)
            formula <- as.formula(formula)
            
            table1 <- arsenal::tableby(formula, self$data)
            
            results <- summary(table1)
            
            self$results$text$setContent(results)
            
            
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
