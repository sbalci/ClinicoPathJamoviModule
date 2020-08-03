# from https://github.com/FredHasselman/casnet-jmvMAC
#
# This file is a generated template, your changes will not be overwritten

tsTSTClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "tsTSTClass",
    inherit = tsTSTBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

          if(is.null(self$options$y1)){return(FALSE)}

          y1   <- self$options$y1
          data <- self$data

          #siglevel <- jmvcore::toNumeric(self$options$siglevel)

          # convert to appropriate data types
          data[[y1]] <- jmvcore::toNumeric(data[[y1]])
          yNA   <- is.na(data[[y1]])

          data <- na.omit(data)

          if(self$options$jb){
            results <- tseries::jarque.bera.test(x =data[[y1]])
            results$data.name <- paste(y1)
            jbOut <- self$results$jbOut
            jbOut$setVisible(visible=TRUE)
            self$results$jbOut$setContent(results)
          }

          if(self$options$bTRND){
            results <- randtests::bartels.rank.test(data[[y1]], alternative = "left.sided")
            results$data.name <- paste(y1)
            bTRNDout <- self$results$bTRNDout
            bTRNDout$setVisible(visible=TRUE)
            self$results$bTRNDout$setContent(results)
          }

          if(self$options$bVON){
            results <-  randtests::bartels.rank.test(data[[y1]], alternative = "two.sided")
            results$data.name <- paste(y1)
            bVONout <- self$results$bVONout
            bVONout$setVisible(visible=TRUE)
            self$results$bVONout$setContent(results)
          }

          if(self$options$bSOSC){
            results <- randtests::bartels.rank.test(data[[y1]], alternative = "right.sided")
            results$data.name <- paste(y1)
            bSOSCout <- self$results$bSOSCout
            bSOSCout$setVisible(visible=TRUE)
            self$results$bSOSCout$setContent(results)
          }

          if(self$options$adf){
            results <- tseries::adf.test(data[[y1]], alternative = c("stationary"),k = trunc((length(data[[y1]])-1)^(1/3)))
            results$data.name <- paste(y1)
            adfOut <- self$results$adfOut
            adfOut$setVisible(visible=TRUE)
            self$results$adfOut$setContent(results)
          }

          if(self$options$kpssLVL){
            results <- tseries::kpss.test(x =data[[y1]],null = "Level")
            results$data.name <- paste(y1)
            kpssLVLout <- self$results$kpssLVLout
            kpssLVLout$setVisible(visible=TRUE)
            self$results$kpssLVLout$setContent(results)
          }

          if(self$options$kpssTRND){
            results <- tseries::kpss.test(x =data[[y1]],null = "Trend")
            results$data.name <- paste(y1)
            kpssTRNDout <- self$results$kpssTRNDout
            kpssTRNDout$setVisible(visible=TRUE)
            self$results$kpssTRNDout$setContent(results)
          }

          if(self$options$csu){
            results <- randtests::cox.stuart.test(data[[y1]], alternative = "right.sided")
            results$data.name <- paste(y1)
            csuOut <- self$results$csuOut
            csuOut$setVisible(visible=TRUE)
            self$results$csuOut$setContent(results)
          }

          if(self$options$csd){
            results <-  randtests::cox.stuart.test(data[[y1]], alternative = "left.sided")
            results$data.name <- paste(y1)
            csdOut <- self$results$csdOut
            csdOut$setVisible(visible=TRUE)
            self$results$csdOut$setContent(results)
          }

          if(self$options$csdu){
            results <-  randtests::cox.stuart.test(data[[y1]], alternative = "two.sided")
            results$data.name <- paste(y1)
            csduOut <- self$results$csduOut
            csduOut$setVisible(visible=TRUE)
            self$results$csduOut$setContent(results)
          }

#           ## Test for **AR**, **ARCH** or an optimal **ARIMA** process
#
#           # KEENAN 1-DEGREE TEST OF NONLINEARITY
#           # H0: time series follows some AR process
#           # H1: time series cannot be considered some AR process
#           TSA::Keenan.test(na.exclude(df.ts$mood_down))
#
#           # MCLEOD-LI TEST FOR CONDITIONAL HETEROSCEDASTICITY
#           # H0: time series follows some AR process
#           # H1: time series cannot be considered some ARCH process
#           TSA::McLeod.Li.test(y = df.ts$mood_down, plot = TRUE, omit.initial = TRUE)
#
#           # MCLEOD-LI TEST FOR CONDITIONAL HETEROSCEDASTICITY
#           # H0: time series follows some AR process
#           # H1: time series cannot be considered some ARiMA process
#           TSA::McLeod.Li.test(object = forecast::auto.arima(df.ts$mood_down), plot = TRUE, omit.initial = TRUE)
#

          #if(self$options$)
          #results <- self$data, var.equal=self$options$varEq)

          #self$results$text$setContent(results)

        })
)
