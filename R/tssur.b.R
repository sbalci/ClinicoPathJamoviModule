# from https://github.com/FredHasselman/casnet-jmvMAC


tsSURClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "tsSURClass",
  inherit = tsSURBase,
  private = list(
    .run = function() {

      if(is.null(self$options$y1)){return(FALSE)}


      # `self$data` contains the data
      # `self$options` contains the options
      # `self$results` contains the results object (to populate)

      dv1  <- self$options$y1
      data <- self$data

      # convert to appropriate data types
      data[[dv1]] <- jmvcore::toNumeric(data[[dv1]])
      y1NA        <- is.na(data[[dv1]])
      data        <- na.omit(data)

      tsData <- data.frame(t  = seq_along(data[[dv1]]),
                           y1 = as.numeric(data[[dv1]]))

      tsImage <- self$results$tsplot
      tsImage$setState(tsData)

      surImage <- self$results$surplot
      surImage$setState(tsData)

      saveState <- self$options$Save

    },

    .tsplot=function(tsImage, ...) {

      if(is.null(self$options$y1)){return(FALSE)}

      plotData <- tsImage$state
      plotD <- data.frame(t  = plotData$t,
                          y1 = plotData$y1,
                          label = rep(self$options$y1,length(plotData$y1)), stringsAsFactors = FALSE)


      tspl <- ggplot2::ggplot(plotD, ggplot2::aes(x=t,y=y1)) +
        ggplot2::geom_line() +
        ggplot2::facet_grid(label ~ ., scales = "free") +
        ggplot2::scale_x_continuous("Time", expand=c(0,0)) +
        ggplot2::scale_y_continuous("", expand=c(0,0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(strip.text = ggplot2::element_text(face="bold"))

      print(tspl)
      TRUE
    },

    .surplot=function(surImage, ...) {

      if(is.null(self$options$y1)){
        surImage$setVisible(visible=FALSE)
        return(FALSE)
      }
      if(is.null(self$options$surmethod)|self$options$surmethod=="choose"){
        surImage$setVisible(visible=FALSE)
        return(FALSE)
      }
      if(is.null(self$options$numSUR)|self$options$numSUR<=0){
        surImage$setVisible(visible=FALSE)
        return(FALSE)
      }

      saved <- FALSE

      saveIt <- self$options$Save

      plotData <- surImage$state
      plotD <- data.frame(t  = plotData$t,
                          y1 = plotData$y1, stringsAsFactors = FALSE)

      if(self$options$surmethod=="randshuffle"){
        fft <- FALSE
        amplitude <- FALSE
        label <- "rand"
      }
      if(self$options$surmethod=="blockshuffle"){
        fft <- FALSE
        amplitude <- FALSE
        label <- "block"
      }
      if(self$options$surmethod=="randphase"){
        fft <- TRUE
        amplitude <- FALSE
        label <- "FFT"
      }
      if(self$options$surmethod=="randphase"){
        fft <- TRUE
        amplitude <- TRUE
        label <- "AAFT"
      }

      plotSurData <- data.frame(tseries::surrogate(plotD$y1, ns =  self$options$numSUR, fft = fft, amplitude = amplitude))
      plotSurData <- data.frame(plotD,plotSurData)
      nums <- paste(1:(NCOL(plotSurData)-NCOL(plotD)))
      nums[as.numeric(nums)<10] <- paste0("0",1:9)
      colnames(plotSurData) <- c("t",paste0(self$options$y1,".ori"), paste0(self$options$y1,".",label,nums))

      plotSurData.long <- tidyr::gather(plotSurData, key = series, value=value, -t)
      plotSurData.long <- plotSurData.long[!plotSurData.long$series%in%paste0(self$options$y1,".ori"),]

      surpl <- ggplot2::ggplot(plotSurData.long, ggplot2::aes(x=t,y=value)) +
        ggplot2::geom_line() +
        ggplot2::facet_grid(series ~ ., scales = "free") +
        ggplot2::scale_x_continuous("Time", expand=c(0,0)) +
        ggplot2::scale_y_continuous("", expand=c(0,0)) +
        ggplot2::theme_bw()
      # ggplot2::theme(strip.text = ggplot2::element_text(face="bold"))

      if(!saved&saveIt){
        if(dir.exists(normalizePath(self$options$savePath))){
         filename <- file.path(self$options$savePath,paste0("casnet_surrogates_",format(Sys.time(), "%d_%b_%Y_%H_%M_%S"),".csv"))
         write.csv(x = plotSurData, file = filename)
         saved <- TRUE
         self$results$savemessage$setContent(filename)
        } else {
          self$results$savemessage$setContent(paste0("The path '",self$options$savePath,"' does not exist!"))
        }
      }

      # Descriptives ----
      # table <- self$results$savedata
      #
      # table$setRow(rowNo=saveNum,
      #              values=list(
      #                saveNum = Nsaves,
      #                var   = self$options$y1,
      #                surMethod  = self$options$numSUR,
      #                file = paste0("~/casnet_surrogates_",format(Sys.time(), "%d_%b_%Y_%H_%M_%S"),".csv")
      #              ))

      print(surpl)
      TRUE
    }
  )
)
