
# This file is a generated template, your changes will not be overwritten

survivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "survivalClass",
    inherit = survivalBase,
    private = list(
        .run = function() {

            
            
            
            if (length(self$options$vars) == 0)
                return()
            
            formula <- jmvcore::constructFormula(terms = self$options$vars)
            
            
            explanatoryUni <- "LVI"
            dependentUni <- "Surv(OverallTime, Outcome)"
            
            mydata %>%
                finalfit::finalfit(dependentUni, explanatoryUni) -> tUni
            
            knitr::kable(tUni, row.names=FALSE, align=c('l', 'l', 'r', 'r', 'r', 'r'))
            
            
            tUni_df <- tibble::as_tibble(tUni, .name_repair = "minimal") %>% 
                janitor::clean_names() 
            
            tUni_df_descr <- paste0("When ",
                                    tUni_df$dependent_surv_overall_time_outcome[1],
                                    " is ",
                                    tUni_df$x[2],
                                    ", there is ",
                                    tUni_df$hr_univariable[2],
                                    " times risk than ",
                                    "when ",
                                    tUni_df$dependent_surv_overall_time_outcome[1],
                                    " is ",
                                    tUni_df$x[1],
                                    "."
            )
            
            
            
            
            
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
