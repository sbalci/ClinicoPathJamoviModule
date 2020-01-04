#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric

# This file is a generated template, your changes will not be overwritten
survivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "survivalClass",
    inherit = survivalBase,
    private = list(
        .run = function() {
            
            if (length(self$options$factor) + length(self$options$outcome) + length(self$options$overalltime) < 3)
                return()

                        
            mydata <- self$data
            
            myoveralltime <- self$options$overalltime
            
            myoveralltime <- jmvcore::toNumeric(self$data[[myoveralltime]])
            
            myfactor <- self$options$factor
            
            myfactor <- self$data[[myfactor]]
            
            myoutcome <- self$options$outcome
          
            myoutcome <- self$data[[myoutcome]]
            
            km_fit <- survival::survfit(survival::Surv(myoveralltime, myoutcome) ~ myfactor, data = mydata)
            
            
            km_fit_median_df <- summary(km_fit)[["table"]]
            
            # km_fit_median_df <- km_fit_median_df$table
            
            km_fit_median_df <- 
                as.data.frame(km_fit_median_df) %>%
                tibble::rownames_to_column()
            
            
            
            # km_fit <- survival::survfit(formula = survival::Surv(myoveralltime, myoutcome) ~ myfactor, data = mydata)
            # 
            # # s <- survival::survfit(formula=survival::Surv(elapsed, event) ~ group, data=data)
            # 
            # km_fit_Table <- summary(km_fit)$table
            # 
            # # sTable <- summary(s)$table
            # 
            # st <- self$results$summary
            # 
            # for (i in seq_len(nrow(km_fit))) {
            #     if (nrow(km_fit) == 1)
            #         g <- km_fit_Table
            #     else
            #         g <- km_fit_Table[i,]
            #     nevents <- sum(g['events'])
            #     n <- g['n.max']
            #     ncensor <- n - nevents
            #     median <- g['median']
            #     mean <- g['*rmean']
            #     prop <- nevents / n
            #     
            #     # st$setRow(rowNo=i, list(
            #     #     censored=ncensor,
            #     #     events=nevents,
            #     #     n=n,
            #     #     prop=nevents/n,
            #     #     median=median,
            #     #     mean=mean))
            # }
            # 
            # st$setStatus('complete')
            # 
            # self$results$summary2$setContent(st)
            # 
            # self$results$summary$setContent(summary)
            
            

            # km_fit_median_definition <-

            # km_fit_median_df %>%
            #     dplyr::mutate(
            #         description =
            #             glue::glue(
            #                 "When {m1}, {m2}, {m3}, {m4}, median survival is "
            #                 # {m8} [{m9} - {m10}, 95% CI] months.
            #             )
            #     ) %>%
            #     dplyr::select(description) %>%
            #     dplyr::pull()
                        
            
            # sTable <- summary(km_fit)$table
            # st <- self$results$summary
            # 
            # for (i in seq_len(nrow(km_fit))) {
            #     if (nrow(km_fit) == 1)
            #         g <- sTable
            #     else
            #         g <- sTable[i,]
            #     nevents <- sum(g['events'])
            #     n <- g['n.max']
            #     ncensor <- n - nevents
            #     median <- g['median']
            #     mean <- g['*rmean']
            #     prop <- nevents / n
            #     
            #     st$setRow(rowNo=i, list(
            #         censored=ncensor,
            #         events=nevents,
            #         n=n,
            #         prop=nevents/n,
            #         median=median,
            #         mean=mean))
            # }
            # 
            # st$setStatus('complete')
            # 
            # 
            # results1 <- st
            
            
            
            results1 <- surv_summary(km_fit, data = mydata)
            
            results2 <- summary(km_fit)$table
            
            results3 <- km_fit_median_df
            
            results4 <- km_fit_median_df$table
            
            results5 <- capture.output(km_fit)
            
            self$results$text1$setContent(results1)
            
            self$results$text2$setContent(results2)

            self$results$text3$setContent(results3)

            self$results$text4$setContent(results4)
            
            self$results$text5$setContent(results5)
            
            
            
            # formulaL <- jmvcore::format('({a} , {b})', a = myoveralltime, b = formulaL2)
            # formulaL <- paste("(", formulaL1, "," ,formulaL2, ")")

            # formulaR <- jmvcore::constructFormula(terms = self$options$factor)
            # formulaR <- jmvcore::constructFormula(terms = "myfactor")
            
            # formula <- jmvcore::composeFormula(lht = formulaL, rht = formulaR)
            # formula <- paste(formulaL, '~', formulaR, ', data = self$data')
            
            # formula <- as.formula(formula)
            
            
            # km_fit <- survival::survfit(survival::Surv(formula))
            
            
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        }
        
        # ,
        # 
        # 
        # .plot=function(image, theme, ggtheme, ...){
        #     
        #     
        # 
        #     plot(km_fit)
        #     
        #     
        #     
        #     
        # }
        
        
        
        
        
        
        )
)
