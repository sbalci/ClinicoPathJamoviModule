
# This file is a generated template, your changes will not be overwritten

finalfitClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "finalfitClass",
    inherit = finalfitBase,
    private = list(
        .run = function() {

            if (length(self$options$explanatory) + length(self$options$outcome) + length(self$options$overalltime) < 3)
                return()
            
            mydata <- self$data
            
            explanatory <- self$options$explanatory
            
            explanatory <- self$data[[explanatory]]
            
            myoveralltime <- self$options$overalltime
            
            myoveralltime <- jmvcore::toNumeric(self$data[[myoveralltime]])
            
            myoutcome <- self$options$outcome
            
            myoutcome <- self$data[[myoutcome]]
            
            
            
            
            
            finalfit::finalfit(.data = mydata,
                               dependent = Surv(myoveralltime, myoutcome),
                               explanatory = explanatory) -> tUni
            
            results1 <- tUni
            
            # results1 <- knitr::kable(tUni, row.names=FALSE, align=c('l', 'l', 'r', 'r', 'r', 'r'))
            # 
            # 
            # 
            # tUni_df <- tibble::as_tibble(tUni, .name_repair = "minimal") %>% 
            #     janitor::clean_names() 
            # 
            # tUni_df_descr <- paste0("When ",
            #                         tUni_df$dependent_surv_overall_time_outcome[1],
            #                         " is ",
            #                         tUni_df$x[2],
            #                         ", there is ",
            #                         tUni_df$hr_univariable[2],
            #                         " times risk than ",
            #                         "when ",
            #                         tUni_df$dependent_surv_overall_time_outcome[1],
            #                         " is ",
            #                         tUni_df$x[1],
            #                         "."
            # )
            
            
            # results2 <- names(km_fit_median_df)
            
            
            self$results$text1$setContent(results1)
            
            # self$results$text2$setContent(results2)
            
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
