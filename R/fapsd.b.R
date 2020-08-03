# from https://github.com/FredHasselman/casnet-jmvMAC


faPSDClass <- if (requireNamespace("jmvcore")) {
  R6::R6Class(
    "faPSDClass",
    inherit = faPSDBase,
    private = list(
      .run = function() {

        if(is.null(self$options$y1)){
          return(FALSE)
        } else {

          y1   <- self$options$y1
          data <- self$data

          # convert to appropriate data types
          data[[y1]] <- jmvcore::toNumeric(data[[y1]])
          data <- na.omit(data)


          #self$options$maxScale <- (floor(log2(NROW(data[[y1]])/2)))
          if(self$options$standardise=="meanSD"){
            standardise <- "mean.sd"
          }
          if(self$options$standardise=="medianMAD"){
            standardise <- "median.mad"
          }
          if(self$options$standardise=="none"){
            standardise <- "none"
          }

          if(self$options$nfft=="user"){
            NFFT <- self$options$userNFFT
          } else {
            NFFT <- NA
          }

          if(self$options$removeTrend=="polydet"){
            polyOrder <- self$options$polydet_order
            detrend <- TRUE
          } else {
            polyOrder <- NA
            detrend <- FALSE
          }

          results <- fd_psd(y  = data[[y1]],
                            fs = self$options$fs,
                            nfft = NFFT,
                            standardise = standardise,
                            detrend = detrend,
                            polyOrder= polyOrder,
                            doPlot = FALSE,
                            returnPlot = TRUE,
                            returnPLAW = TRUE,
                            returnInfo = FALSE,
                            silent = TRUE,
                            noTitle = TRUE,
                            tsName = y1)

          if(detrend){
            detr <- "Yes"
          } else {
            detr <- "No"
          }

          # Descriptives ----
          tableTS <- self$results$tblTS

          tableTS$setRow(rowNo=1,
                         values=list(
                           var = y1,
                           N   = NROW(na.omit(data[[y1]])),
                           na  = sum(is.na(data[[y1]])),
                           median = stats::median(data[[y1]],na.rm = TRUE),
                           mad = stats::mad(data[[y1]],na.rm = TRUE),
                           mean = mean(data[[y1]],na.rm = TRUE),
                           sd = stats::sd(data[[y1]],na.rm = TRUE),
                           standardise = standardise,
                           detrending = paste(detr,"(order = ",polyOrder,")")))

          tableTS$setRow(rowNo=2,
                         values=list(
                           var = paste(y1,"adjusted"),
                           N   = NROW(na.omit(results[[2]]$y)),
                           na  = sum(is.na(data[[y1]])),
                           median = stats::median(results[[2]]$y,na.rm = TRUE),
                           mad = stats::mad(results[[2]]$y,na.rm = TRUE),
                           mean = mean(results[[2]]$y,na.rm = TRUE),
                           sd = stats::sd(results[[2]]$y,na.rm = TRUE),
                           standardise = "",
                           detrending = ""))

          # PSD ----
          tablePSD <- self$results$tblPSD


          tablePSD$setRow(rowNo=1,
                          values=list(
                            method = results[[2]]$method,
                            antiper = ifelse(results[[2]]$antiper,"Yes","No"),
                            slope = results[[2]]$sap,
                            intercept = results[[2]]$int,
                            H =  as.numeric(results[[2]]$H),
                            FD = results[[2]]$FD))

          tablePSD$setRow(rowNo=2,
                          values=list(
                            method = results[[3]]$method,
                            antiper = ifelse(results[[3]]$antiper,"Yes","No"),
                            slope = results[[3]]$sap,
                            intercept = results[[3]]$int,
                            H = as.numeric(results[[3]]$H),
                            FD = results[[3]]$FD))

          #self$results$DFAout$setContent(results)

          tsImage <- self$results$tsplot
          tsImage$setState(results$plots$g1)

          psdImage <- self$results$psdplot
          psdImage$setState(results$plots$g2)
        }
      },

      .tsplot=function(tsImage, ...) {

        if(is.null(self$options$y1)){return(FALSE)}

        g1 <- tsImage$state
        print(g1)
        TRUE
      },

      .psdplot=function(psdImage, ...) {

        if(is.null(self$options$y1)){return(FALSE)}

        g2 <- psdImage$state
        print(g2)
        TRUE
      }
    )
  )
}
