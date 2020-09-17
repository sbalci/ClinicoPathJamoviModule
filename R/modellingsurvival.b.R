modellingSurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "modellingSurvivalClass",
    inherit = modellingSurvivalBase,
    private = list(


    )
)



# # from https://github.com/AlbertoAlvarezIglesias2019/SimpleSurvival
#
#
# modellingSurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
#     "modellingSurvivalClass",
#     inherit = modellingSurvivalBase,
#     private = list(
#
#       .init = function() {
#
#         image <- self$results$plot1
#         image$setVisible(visible=FALSE)
#
#         image <- self$results$plot2
#         image$setVisible(visible=FALSE)
#
#         image <- self$results$plot3
#         image$setVisible(visible=FALSE)
#
#         image <- self$results$plot4
#         image$setVisible(visible=FALSE)
#
#
#         groups <- private$.groups()
#
#         summary <- self$results$modsurvTable2
#         for (group in groups)
#           summary$addRow(rowKey=group, list(term=group))
#
#       },
#       .groups = function() {
#
#         times <- self$options$times
#         status <- self$options$status
#         deps <- self$options$deps
#         tmpDat <- self$data
#         tmpDat[,status] <- as.numeric(as.character(tmpDat[,status]))
#
#         if (is.null(times) || is.null(status) || is.null(deps))
#           return()
#
#         temp <- jmvcore::constructFormula(terms = deps)
#         form <- paste("Surv(",times,",",status,")~",temp,sep="")
#         form <- as.formula(form)
#         fit <- model.matrix(form,tmpDat)
#         modterms <- names(as.data.frame(fit))
#         wher <- modterms%in%"(Intercept)"
#         modterms[!wher]
#       },
#       .run = function() {
#
#         image <- self$results$plot1
#         image$setVisible(visible=FALSE)
#
#         image <- self$results$plot2
#         image$setVisible(visible=FALSE)
#
#         image <- self$results$plot3
#         image$setVisible(visible=FALSE)
#
#         image <- self$results$plot4
#         image$setVisible(visible=FALSE)
#
#
#         # `self$data` contains the data
#         # `self$options` contains the options
#         # `self$results` contains the results object (to populate)
#
#         times <- self$options$times
#         status <- self$options$status
#         deps <- self$options$deps
#         strat <- self$options$strata
#         plotvar <- self$options$plotvar
#         tmpDat <- self$data
#         tmpDat[,status] <- as.numeric(as.character(tmpDat[,status]))
#
#
#         if (is.null(times) || is.null(status) || is.null(deps))
#           return()
#         #if (!class(tmpDat[,strat])=="character" & !class(tmpDat[,strat])=="factor" ) return()
#         temp <- jmvcore::constructFormula(terms = deps)
#         form <- paste("Surv(",times,",",status,")~",temp,sep="")
#         if (!is.null(strat)) form <- paste("Surv(",times,",",status,")~",temp,"+strata(",strat,")",sep="")
#         form <- as.formula(form)
#         fit <- survival::coxph(form, data = tmpDat,model=TRUE,ties="breslow")
#         modterms <- row.names(summary(fit)$coefficients)
#
#         table1 <- self$results$modsurvTable1
#         table1$setRow(rowNo=1, values=list(
#           test=summary(fit)$logtest[1],
#           df = summary(fit)$logtest[2],
#           pvalue = summary(fit)$logtest[3]))
#
#         table2 <- self$results$modsurvTable2
#         for (i in 1:length(modterms) ) {
#           table2$setRow(rowNo=i, values=list(
#             beta=summary(fit)$coefficients[i,1],
#             sebeta=summary(fit)$coefficients[i,3],
#             z=summary(fit)$coefficients[i,4],
#             pvalue=summary(fit)$coefficients[i,5],
#             hr=summary(fit)$conf.int[i,1],
#             hrlower=summary(fit)$conf.int[i,3],
#             hrupper=summary(fit)$conf.int[i,4]))}
#
#         #*********************************
#         #*********************************
#         #****
#         #**** Calculates the state for the
#         #**** plot
#         #****
#         #*********************************
#         #*********************************
#
#         library(tidyverse)
#         library(broom)
#         library(party)
#
#
#
#         ################################################
#         ### calculate the new data.frame for prediction
#         ################################################
#         if (!is.null(plotvar)) {
#
#           wher <- deps %in% plotvar
#           if (!any(wher)) return()
#
#           image <- self$results$plot1
#           image$setVisible(visible=TRUE)
#
#           if (length(deps)>1) {
#             datmean <- tmpDat[,deps[!wher], drop=FALSE]
#             temp <- lapply(datmean,function(xx) {
#               if (class(xx)[1]=="integer"|class(xx)[1]=="numeric") {
#                 out <- median(xx,na.rm=TRUE)
#               } else {
#                 uniqv <- unique(xx)
#                 out <- uniqv[which.max(table(xx))]
#               }
#               out
#             })
#             datmean <- as.data.frame(temp)
#
#             xx <- tmpDat[,plotvar]
#             if (class(xx)[1]=="integer"|class(xx)[1]=="numeric") {
#               out <- as.numeric(quantile(xx,probs = c(0.25,0.5,0.75)))
#             } else {
#               out <- as.character(unique(xx))
#             }
#             newdat <- cbind(out,datmean,stringsAsFactors=FALSE)
#             wher <- names(newdat) %in% "out"
#             names(newdat)[wher] <- plotvar
#           }
#
#           if (length(deps)==1) {
#             xx <- tmpDat[,plotvar]
#             if (class(xx)[1]=="integer"|class(xx)[1]=="numeric") {
#               out <- as.numeric(quantile(xx,probs = c(0.25,0.5,0.75)))
#             } else {
#               out <- as.character(unique(xx))
#             }
#             newdat <- data.frame(out=out)
#             wher <- names(newdat) %in% "out"
#             names(newdat)[wher] <- plotvar
#
#           }
#
#
#           ################################
#           ### Calculate predictions using
#           ### survfit
#           ################################
#           tmpPred <-  survival::survfit(fit,newdata=newdat)
#
#
#           ####################################
#           ### tidy those to obtain CI as well
#           ####################################
#           tmpPred <- broom::tidy(tmpPred)
#
#           ####################################
#           ### check if there is strata or not
#           ####################################
#           if (!any(str_detect(names(tmpPred),"strata"))) {
#             tmpPred$strata <- FALSE
#           }
#
#           ###############################
#           ### Reshape to the long format
#           ###############################
#           ddd1 <- tmpPred %>%
#             select(time,n.censor,strata,contains("estimate")) %>%
#             pivot_longer(contains("estimate"),names_to = "var",values_to = "surv") %>%
#             mutate(var = str_remove(var,"estimate."))
#           ddd2 <- tmpPred %>%
#             select(time,n.censor,strata,contains("conf.low")) %>%
#             pivot_longer(contains("conf.low"),names_to = "var",values_to = "lower") %>%
#             mutate(var = str_remove(var,"conf.low."))
#           ddd3 <- tmpPred %>%
#             select(time,n.censor,strata,contains("conf.high")) %>%
#             pivot_longer(contains("conf.high"),names_to = "var",values_to = "upper") %>%
#             mutate(var = str_remove(var,"conf.high."))
#           tmpPred <- merge(ddd1,ddd2)
#           tmpPred <- merge(tmpPred,ddd3)
#           tmpPred <- tmpPred %>%
#             arrange(strata,time)
#
#           ############################
#           ### Tidy up strata Variable
#           ############################
#           if (class(tmpPred$strata)=="logical") tmpPred <- tmpPred %>% select(-strata)
#           if (!class(tmpPred$strata)=="logical") {
#             wher <- names(tmpPred) %in% "strata"
#             names(tmpPred)[wher] <- "strataVariable"
#           }
#
#
#           ################################
#           ### Fix levels ploting variable
#           ################################
#           newdat1 <- newdat %>%
#             dplyr::mutate(var = as.character(1:n()))
#           wher <- names(newdat1) %in% c(plotvar,"var")
#           newdat1 <- newdat1[,wher]
#
#
#           #############################
#           ### Tidy up ploting variable
#           #############################
#           tmpPred <- merge(tmpPred,newdat1)
#           tmpPred <- tmpPred %>%
#             dplyr::select(-var) %>%
#             dplyr::arrange(time)
#           wher <- names(tmpPred) %in% plotvar
#           names(tmpPred)[wher] <- "strata"
#
#           if (!is.null(strat)) {
#             tmpPred$strataVariable <- factor(tmpPred$strataVariable)
#             levels(tmpPred$strataVariable) <- paste(strat," = ",levels(tmpPred$strataVariable),sep="")
#           }
#
#           i_1_State <- tmpPred
#           image <- self$results$plot1
#           image$setState(i_1_State)
#
#
#         }
#
#         #*********************************
#         #*********************************
#         #****
#         #**** Calculates the state for the
#         #**** plot2 PH assumption
#         #****
#         #*********************************
#         #*********************************
#
#         if (self$options$diagnosticsyn) {
#           image <- self$results$plot2
#           image$setVisible(visible=TRUE)
#           test.ph <- cox.zph(fit)
#           i_2_State <- test.ph
#           image$setState(i_2_State)
#         }
#
#
#
#         #*********************************
#         #*********************************
#         #****
#         #**** Calculates the state for
#         #**** plot3  Survival Tree
#         #****
#         #*********************************
#         #*********************************
#
#         if (self$options$survivaltreeyn) {
#           image <- self$results$plot3
#           image$setVisible(visible=TRUE)
#           i_3_State <- tmpDat
#           image$setState(i_3_State)
#         }
#
#
#         #*********************************
#         #*********************************
#         #****
#         #**** Calculates the state for
#         #**** plot4  forest plot
#         #****
#         #*********************************
#         #*********************************
#
#         if (self$options$forestyn) {
#           image <- self$results$plot4
#           image$setVisible(visible=TRUE)
#           i_4_State <- tmpDat
#           image$setState(i_4_State)
#         }
#
#
#
#       },
#       .plot1=function(image, ...) {
#
#         times <- self$options$times
#         status <- self$options$status
#         deps <- self$options$deps
#         plotvar <- self$options$plotvar
#         strat <- self$options$strata
#
#         if (is.null(times) || is.null(status) || is.null(deps)|| is.null(plotvar))
#           return()
#
#         wher <- deps %in% plotvar
#         if (!any(wher)) return()
#
#
#         plotData <- as.data.frame(image$state)
#         conf.int <- self$options$ciyn
#         xlab <- ifelse(self$options$timeunits=="None","Time",paste("Time (",self$options$timeunits,")",sep=""))
#         p <- survminer::ggsurvplot_df(plotData,
#                                       xlab=xlab,
#                                       ylab = "Survival Probabilities",
#                                       conf.int = conf.int,
#                                       legend.title=plotvar)
#
#         if (!is.null(strat)) {
#           plotData$strataVariable <- factor(plotData$strataVariable)
#           levels(plotData$strataVariable) <- paste(strat," = ",levels(plotData$strataVariable),sep="")
#           p <- p + facet_wrap(vars(strataVariable) )
#
#         }
#
#         print(p)
#         TRUE
#       },
#       .plot2=function(image, ...) {
#
#         times <- self$options$times
#         status <- self$options$status
#         deps <- self$options$deps
#
#         if (is.null(times) || is.null(status) || is.null(deps))
#           return()
#
#         plotData <- image$state
#         plot <- ggcoxzph(plotData)
#         print(plot)
#         TRUE
#       },
#       .plot3=function(image, ...) {
#
#         times <- self$options$times
#         status <- self$options$status
#         deps <- self$options$deps
#
#         if (is.null(times) || is.null(status) || is.null(deps))
#           return()
#
#         temp <- jmvcore::constructFormula(terms = deps)
#         form <- paste("Surv(",times,",",status,")~",temp,sep="")
#         form <- as.formula(form)
#
#         plotData <- image$state
#         fitctree <- party::ctree(form, data = plotData)
#
#
#         plot <- plot(fitctree)
#         print(plot)
#         TRUE
#       },
#       .plot4=function(image, ...) {
#
#         times <- self$options$times
#         status <- self$options$status
#         deps <- self$options$deps
#         strat <- self$options$strata
#
#         if (is.null(times) || is.null(status) || is.null(deps))
#           return()
#
#         if (!is.null(strat)) {
#           jmvcore::reject("Forest plot not available with stratification variable", code='')
#           return()
#         }
#
#
#         temp <- jmvcore::constructFormula(terms = deps)
#         form <- paste("Surv(",times,",",status,")~",temp,sep="")
#         if (!is.null(strat)) form <- paste("Surv(",times,",",status,")~",temp,"+strata(",strat,")",sep="")
#         form <- as.formula(form)
#
#         plotData <- image$state
#
#         fitcox <- survival::coxph(form, data = plotData,model=TRUE,ties="breslow")
#         plot <- survminer::ggforest(fitcox,data=plotData,fontsize=1)
#         print(plot)
#         TRUE
#       }
#
#       )
# )
