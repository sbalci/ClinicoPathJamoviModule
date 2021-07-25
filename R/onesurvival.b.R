# from https://github.com/AlbertoAlvarezIglesias2019/SimpleSurvival


oneSurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "oneSurvivalClass",
    inherit = oneSurvivalBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            times <- self$options$times
            status <- self$options$status
            tmpDat <- self$data

            if (is.null(times) || is.null(status))
              return()



            form <- paste("survival::Surv(",times,",",status,") ~ 1")
            form <- as.formula(form)

            tmpDat[,status] <- as.numeric(as.character(tmpDat[,status]))

            fit <- survival::survfit(form, data = tmpDat)
            temp <- as.list(summary(fit)$table)
            #$results$text$setContent(fit)

            table1 <- self$results$onesurvTable1
            table1$setRow(rowNo=1, values=list(
              n=temp$records,
              nevents = temp$events,
              median=temp$median,
              cilb=temp$`0.95LCL`,
              ciub=temp$`0.95UCL`
            ))

            image <- self$results$onesurvPlot1
            image$setState(fit)

          },
          .plot=function(image, ...) {

            times <- self$options$times
            status <- self$options$status
            tmpDat <- self$data

            if (is.null(times) || is.null(status))
              return()

            plotData <- image$state
            conf.int <- self$options$ciyn
            xlab <- ifelse(self$options$timeunits=="None","Time",paste("Time (",self$options$timeunits,")",sep=""))
            plot <- ggfortify:::autoplot.survfit(plotData,
                                                 xlab=xlab,
                                                 ylab = "Survival Probabilities",
                                                 conf.int = conf.int,
                                                 yScale = "frac")
            print(plot)
            TRUE
          }

        )
)
