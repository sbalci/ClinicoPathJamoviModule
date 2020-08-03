# from https://github.com/FredHasselman/casnet-jmvMAC

# @export
tsACFClass <- if (requireNamespace("jmvcore")) {
  R6::R6Class(
    "tsACFClass",
    inherit = tsACFBase,
    private = list(
      .run = function() {

        Xcor <- TRUE
        data <- self$data

        if(is.null(self$options$dv1)){
          return(FALSE)
        } else {
          dv1  <- self$options$dv1
          # convert to appropriate data types
          data[[dv1]] <- jmvcore::toNumeric(data[[dv1]])
          y1NA        <- is.na(data[[dv1]])
        }
        if(is.null(self$options$dv2)){
          Xcor <- FALSE
        } else {
          dv2 <- self$options$dv2
          data[[dv2]] <- jmvcore::toNumeric(data[[dv2]])
          y2NA        <- is.na(data[[dv2]])
        }
        data  <- na.omit(data)

        if(Xcor){
          tsData <- data.frame(t  = seq_along(data[[dv1]]),
                               y1 = as.numeric(data[[dv1]]),
                               y2 = as.numeric(data[[dv2]]))
        } else {
          tsData <- data.frame(t  = seq_along(data[[dv1]]),
                               y1 = as.numeric(data[[dv1]]))
        }

        tsImage <- self$results$tsplot
        tsImage$setState(tsData)


        # Descriptives ----
        table <- self$results$tsdesc

        y1Descriptives <- summary(data[[dv1]])

        table$setRow(rowNo=1,
                     values=list(
                       var = dv1,
                       N   = NROW(na.omit(data[[dv1]])),
                       na  = sum(y1NA),
                       minimum = y1Descriptives[[1]],
                       q1 = y1Descriptives[[2]],
                       q3 = y1Descriptives[[5]],
                       maximum = y1Descriptives[[6]],
                       median = y1Descriptives[[3]],
                       mad = stats::mad(data[[dv1]],na.rm = TRUE),
                       mean = y1Descriptives[[4]],
                       sd = stats::sd(data[[dv1]],na.rm = TRUE)
                     ))

        if(Xcor){
          y2Descriptives <- summary(data[[dv2]])
          table$setRow(rowNo=2,
                       values=list(
                         var = dv2,
                         N   = NROW(na.omit(data[[dv2]])),
                         na  = sum(y2NA),
                         minimum = y2Descriptives[[1]],
                         q1 = y2Descriptives[[2]],
                         q3 = y2Descriptives[[5]],
                         maximum = y2Descriptives[[6]],
                         median = y2Descriptives[[3]],
                         mad = stats::mad(data[[dv2]],na.rm = TRUE),
                         mean = y2Descriptives[[4]],
                         sd = stats::sd(data[[dv2]],na.rm = TRUE)
                       ))
        }


        if(Xcor){

          # No XCorrelation data
          XcfImage <- self$results$Xcfplot
          XcfImage$setVisible(visible=TRUE)

          # Return data ----
          y01 <- tsData$y1
          y1  <- c(rep(NA,self$options$rtLAG1), y01[1:(NROW(y01)-self$options$rtLAG1)])
          y02 <- tsData$y2
          y2  <- c(rep(NA,self$options$rtLAG2), y02[1:(NROW(y02)-self$options$rtLAG2)])

          rtData  <- data.frame(y0 = c(y01, y02),
                                y1 = c(y1, y2),
                                label = c(rep(paste(self$options$dv1,"| n = ",self$options$rtLAG1),length(y1)),
                                          rep(paste(self$options$dv2,"| n = ",self$options$rtLAG2),length(y2))),
                                stringsAsFactors = FALSE)

          # Phase diagram
          # y1 <- tsData$y1
          # y2 <- tsData$y2
          phData  <- data.frame(y1 = y01,
                                y2 = y02, stringsAsFactors = FALSE)

          # Phase diagram
          phImage <- self$results$phdiagram
          phImage$setVisible(visible=TRUE)
          phImage$setState(phData)

        } else {

          # No XCorrelation data
          XcfImage <- self$results$Xcfplot
          XcfImage$setVisible(visible=FALSE)

          # Return data
          y0 <- tsData$y1
          y1 <- c(rep(NA,self$options$rtLAG1), y0[1:(NROW(y0)-self$options$rtLAG1)])
          rtData  <- data.frame(y0 = y0,
                                y1 = y1,
                                label = rep(self$options$dv1,length(y1)), stringsAsFactors = FALSE)

          # Phase diagram
          phImage <- self$results$phdiagram
          phImage$setVisible(visible=FALSE)

        }

        # XCorrelation data ----
        XcfImage <- self$results$Xcfplot
        XcfImage$setState(rbind(tsData))

        # Correlation plot ----
        cfImage <- self$results$cfplot
        cfImage$setState(tsData)

        # Return plot
        rtImage <- self$results$rtplot
        rtImage$setState(rtData)

      },

      .tsplot=function(tsImage, ...) {

        Xcor <- TRUE
        if(is.null(self$options$dv1)){return(FALSE)}
        if(is.null(self$options$dv2)){Xcor <- FALSE}

        plotData <- tsImage$state

        if(Xcor){
          plotD <- data.frame(t  = c(plotData$t,plotData$t),
                              y1 = c(plotData$y1,plotData$y2),
                              label = c(rep(self$options$dv1,length(plotData$y1)),
                                        rep(self$options$dv2,length(plotData$y2))), stringsAsFactors = FALSE)
        } else {
          plotD <- data.frame(t  = plotData$t,
                              y1 = plotData$y1,
                              label = rep(self$options$dv1,length(plotData$y1)), stringsAsFactors = FALSE)
        }


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

      .cfplot=function(cfImage, ...) {

        Xcor <- TRUE
        if(is.null(self$options$dv1)){return(FALSE)}
        if(is.null(self$options$dv2)){Xcor <- FALSE}

        plotCF <- cfImage$state

        # Correlation functions ----

        if(self$options$hypothesis%in%"both"){
          sides <- 2
        } else {
          sides <- 1
        }

        alt      <- self$options$hypothesis
        siglevel <- jmvcore::toNumeric(self$options$siglevel)

        df.acf1  <- stats::acf(x = plotCF$y1, plot=FALSE, lag.max = jmvcore::toNumeric(self$options$maxLAG))
        df.pacf1 <- stats::pacf(x = plotCF$y1, plot=FALSE, lag.max = jmvcore::toNumeric(self$options$maxLAG))
        dfN1 <- plyr::laply(0:self$options$maxLAG, function(l){length(plotCF$y1)-l})

        corfunACF1  <- plyr::ldply(seq_along(df.acf1$acf), function(cc){pacf_fisherZ(r=df.acf1$acf[cc],n=dfN1[cc],lag=df.acf1$lag[cc], siglevel = siglevel, sides = sides, alt=alt, type="Auto-Correlation Function", label=paste0(self$options$dv1))})
        corfunPACF1 <- plyr::ldply(seq_along(df.pacf1$acf), function(cc){pacf_fisherZ(r=df.pacf1$acf[cc],n=(dfN1[cc]-1),lag=df.pacf1$lag[cc], siglevel = siglevel, sides = sides, alt=alt, type="Partial Auto-Correlation Function", label=paste0(self$options$dv1))})


        if(Xcor){
          df.acf2  <- stats::acf(x = plotCF$y2, plot=FALSE, lag.max = jmvcore::toNumeric(self$options$maxLAG))
          df.pacf2 <- stats::pacf(x = plotCF$y2, plot=FALSE, lag.max = jmvcore::toNumeric(self$options$maxLAG))

          dfN2 <- plyr::laply(0:self$options$maxLAG, function(l){length(plotCF$y2)-l})

          corfunACF2  <- plyr::ldply(seq_along(df.acf2$acf), function(cc){pacf_fisherZ(r=df.acf2$acf[cc],n=dfN2[cc],lag=df.acf2$lag[cc], siglevel = siglevel, sides = sides, alt=alt, type="Auto-Correlation Function", label=paste0(self$options$dv2))})
          corfunPACF2 <- plyr::ldply(seq_along(df.pacf2$acf), function(cc){pacf_fisherZ(r=df.pacf2$acf[cc],n=(dfN2[cc]-1),lag=df.pacf2$lag[cc], siglevel = siglevel, sides = sides, alt=alt, type="Partial Auto-Correlation Function", label=paste0(self$options$dv2))})

          # ACF/PACF
          cfData     <- as.data.frame(rbind(corfunACF1,corfunACF2,corfunPACF1,corfunPACF2)) #,corfunXCF)
        } else {
          # ACF/PACF
          cfData     <- rbind(corfunACF1,corfunPACF1)
        }

        minLAG <- 0 #min(plotData$lag, na.rm = TRUE)
        maxLAG <- self$options$maxLAG #max(plotData$lag[1])
        N      <- max(cfData$n)[1]
        siglevel <- cfData$alpha[1]
        breaks <- seq(minLAG,maxLAG, by = round(maxLAG/10))

        groupColours <-  scales::brewer_pal(palette="RdBu")(11)
        cols <- c("yes"=groupColours[9],"no"=groupColours[3])

        if(self$options$doTest){

          if(self$options$hypothesis=="both"){
            GuideLabels <- list("yes"= expression(rho != 0),
                                "no" = expression(rho == 0))
          }
          if(self$options$hypothesis=="greater"){
            GuideLabels <- list("yes"= expression(rho > 0),
                                "no" = expression(rho <= 0))
          }
          if(self$options$hypothesis=="less"){
            GuideLabels <- list("yes"= expression(rho < 0),
                                "no" = expression(rho >= 0))
          }

          pl<-ggplot2::ggplot(cfData,ggplot2::aes(x=lag, y=r)) +
            ggplot2::facet_grid(label~type, scales = "free") +
            ggplot2::geom_hline(yintercept = 0, colour="grey",size=1) +
            ggplot2::geom_line(data = data.frame(x=c(0,cfData$lag[1]),y=c(1,cfData$r[1])),
                               ggplot2::aes(x=x,y=y),colour="grey50") +
            ggplot2::geom_point(x=0,y=1,colour=groupColours[10],fill=groupColours[9],size=2,pch=21) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin=ciL,ymax=ciU),fill="grey70",colour="grey50",alpha=.3) +
            ggplot2::geom_path(colour="grey50") +
            ggplot2::geom_point(ggplot2::aes(fill = sig, colour=sig), pch=21)  +
            ggplot2::scale_fill_manual(bquote(p < .(siglevel)),values = cols, labels = GuideLabels) +
            ggplot2::scale_colour_manual(bquote(p < .(siglevel)),values = cols, labels =  GuideLabels) +
            ggplot2::scale_y_continuous("",limits = c(-1,1), expand=c(0.01,0.01)) +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(),
                           panel.grid.minor.x = ggplot2::element_blank())
        } else {

          pl<-ggplot2::ggplot(cfData,ggplot2::aes(x=lag, y=r)) +
            ggplot2::facet_wrap("type",ncol = 2, scales = "free") +
            ggplot2::geom_hline(yintercept = 0, colour="grey",size=1) +
            ggplot2::geom_line(data = data.frame(x=c(0,cfData$lag[1]),y=c(1,cfData$r[1])),
                               ggplot2::aes(x=x,y=y),colour="grey50") +
            ggplot2::geom_point(x=0,y=1,colour=groupColours[10],fill=groupColours[9],size=2,pch=21) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin=ciL,ymax=ciU),fill="grey70",colour="grey50",alpha=.3) +
            ggplot2::geom_path(colour="grey50") +
            ggplot2::geom_point(pch=21,colour=groupColours[10],fill=groupColours[9]) + #, cex=pcex) +
            ggplot2::scale_y_continuous("",limits = c(-1,1), expand=c(0.01,0.01)) +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(),
                           panel.grid.minor.x = ggplot2::element_blank())
        }
        print(pl)
        TRUE
      },


      .Xcfplot=function(XcfImage, ...) {

        Xcor <- TRUE
        if(is.null(self$options$dv1)){return(FALSE)}
        if(is.null(self$options$dv2)){Xcor <- FALSE}

        plotXCF <- XcfImage$state

        # Correlation functions ----

        if(self$options$hypothesis%in%"both"){
          sides <- 2
        } else {
          sides <- 1
        }

        alt      <- self$options$hypothesis
        siglevel <- jmvcore::toNumeric(self$options$siglevel)

        df.ccf   <- stats::ccf(x = plotXCF$y1, y = plotXCF$y2, plot = FALSE, lag.max = jmvcore::toNumeric(self$options$maxLAG))

        dfN1 <- plyr::laply(-self$options$maxLAG:self$options$maxLAG, function(l){length(plotXCF$y1)-l})
        dfN2 <- plyr::laply(-self$options$maxLAG:self$options$maxLAG, function(l){length(plotXCF$y2)-l})

        corfunXCF <- plyr::ldply(seq_along(df.ccf$acf), function(cc){pacf_fisherZ(r=df.ccf$acf[cc],n=(dfN1[cc]+dfN2[cc]-1),lag=df.ccf$lag[cc], siglevel = siglevel, sides = sides, alt=alt, type="Cross-Correlation Function", label=paste(self$options$dv1,"X",self$options$dv2))})

        minLAG <- -self$options$maxLAG
        maxLAG <- self$options$maxLAG
        N      <- max(plotXCF$n)[1]
        siglevel <- plotXCF$alpha[1]
        breaks <- seq(minLAG,maxLAG, by = round(maxLAG/10))

        groupColours <-  scales::brewer_pal(palette="RdBu")(11)
        cols <- c("yes"=groupColours[9],"no"=groupColours[3])

        if(self$options$doTest){

          if(self$options$hypothesis=="both"){
            GuideLabels <- list("yes"= expression(rho != 0),
                                "no" = expression(rho == 0))
          }
          if(self$options$hypothesis=="greater"){
            GuideLabels <- list("yes"= expression(rho > 0),
                                "no" = expression(rho <= 0))
          }
          if(self$options$hypothesis=="less"){
            GuideLabels <- list("yes"= expression(rho < 0),
                                "no" = expression(rho >= 0))
          }

          xpl<-ggplot2::ggplot(corfunXCF,ggplot2::aes(x=lag, y=r)) +
            ggplot2::facet_grid(label~type, scales = "free") +
            ggplot2::geom_hline(yintercept = 0, colour="grey",size=1) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin=ciL,ymax=ciU),fill="grey70",colour="grey50",alpha=.3) +
            ggplot2::geom_path(colour="grey50") +
            ggplot2::geom_point(ggplot2::aes(fill = sig, colour=sig), pch=21)  +
            ggplot2::scale_fill_manual(bquote(p < .(siglevel)),values = cols, labels = GuideLabels) +
            ggplot2::scale_colour_manual(bquote(p < .(siglevel)),values = cols, labels =  GuideLabels) +
            ggplot2::scale_y_continuous("",limits = c(-1,1), expand=c(0.01,0.01)) +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(),
                           panel.grid.minor.x = ggplot2::element_blank())
        } else {

          xpl<-ggplot2::ggplot(corfunXCF,ggplot2::aes(x=lag, y=r)) +
            ggplot2::facet_wrap("type",ncol = 2, scales = "free") +
            ggplot2::geom_hline(yintercept = 0, colour="grey",size=1) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin=ciL,ymax=ciU),fill="grey70",colour="grey50",alpha=.3) +
            ggplot2::geom_path(colour="grey50") +
            ggplot2::geom_point(pch=21,colour=groupColours[10],fill=groupColours[9]) + #, cex=pcex) +
            ggplot2::scale_y_continuous("",limits = c(-1,1), expand=c(0.01,0.01)) +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(),
                           panel.grid.minor.x = ggplot2::element_blank())
        }
        print(xpl)
        TRUE
      },

      .rtplot=function(rtImage, ...) {

        Xcor <- TRUE
        if(is.null(self$options$dv1)){return(FALSE)}
        if(is.null(self$options$dv2)){Xcor <- FALSE}

        plotRT <- rtImage$state

        if(Xcor){

          rtpl <- ggplot2::ggplot(plotRT, ggplot2::aes(x=y1, y=y0)) +
            ggplot2::geom_point() +
            ggplot2::facet_wrap(~ label, scales = "free") +
            ggplot2::theme_bw() +
            #ggplot2::coord_equal() +
            ggplot2::ylab("Y(t)") +
            ggplot2::xlab("Y(t+n)")

        } else {

          rtpl <- ggplot2::ggplot(plotRT, ggplot2::aes(x=y1, y=y0)) +
            ggplot2::geom_point() +
            ggplot2::facet_wrap(~ label) + #, scales = "free", space = "free") +
            ggplot2::theme_bw() +
            ggplot2::coord_equal() +
            ggplot2::ylab("Y(t)") +
            ggplot2::xlab(paste0("Y(t+",self$options$rtLAG1,")"))

        }

        print(rtpl)
        TRUE
      },

      .phdiagram=function(phImage, ...) {

        Xcor <- TRUE
        if(is.null(self$options$dv1)){return(FALSE)}
        if(is.null(self$options$dv2)){Xcor <- FALSE}

        plot2D <- phImage$state

        if(Xcor){

          phpl <- ggplot2::ggplot(plot2D, ggplot2::aes(x=y1, y=y2)) +
            ggplot2::geom_path(colour="grey80",alpha=.4) +
            ggplot2::geom_point() +
            ggplot2::theme_bw() +
            ggplot2::coord_equal() +
            ggplot2::ylab(self$options$dv1) +
            ggplot2::xlab(self$options$dv2) +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                           panel.grid.minor = ggplot2::element_blank())

        }

        print(phpl)
        TRUE
      }
    )
  )
}
