
# This file is a generated template, your changes will not be overwritten

tableoneClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "tableoneClass",
    inherit = tableoneBase,
    private = list(
        .run = function() {

            
            if (length(self$options$vars) == 0)
                return()
            
            formula <- jmvcore::constructFormula(terms = self$options$vars)
            formula <- paste('~', formula)
            formula <- as.formula(formula)
            
            table1 <- arsenal::tableby(formula, self$data)
            
            results <- summary(table1)
            
            self$results$text$setContent(results)
        
            
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
