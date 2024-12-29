# from https://github.com/FredHasselman/casnet-jmvMAC


tsENTClass <- if (requireNamespace("jmvcore")) {
  R6::R6Class(
    "tsENTClass",
    inherit = tsENTBase,
    private = list(
      .run = function() {

        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)

        if(is.null(self$options$y1)){return(FALSE)}

        y1  <- self$options$y1
        data <- self$data

        # convert to appropriate data types
        data[[y1]] <- jmvcore::toNumeric(data[[y1]])
        y1NA        <- is.na(data[[y1]])
        data        <- na.omit(data)


        table <- self$results$tsdesc
        tbEntropy <- summary(data[[y1]])
        SD <- stats::sd(data[[y1]],na.rm = TRUE)

        VAR  <- stats::acf(data[[y1]], lag.max = 1, type = 'covariance', plot=FALSE)
        # RR formula
        RelR   <- 2*(1-VAR$acf[2] / VAR$acf[1])
        localVar <- VAR$acf[2]
        globalVar <- VAR$acf[1]

        SampEn <- pracma::sample_entropy(data[[y1]], edim = self$options$Ent_edim, r = self$options$Ent_r*SD)
        ApprEn <- pracma::approx_entropy(data[[y1]], edim = self$options$Ent_edim, r = self$options$Ent_r*SD)

        table$setRow(rowNo=1,
                     values=list(
                       var = y1,
                       N   = NROW(na.omit(data[[y1]])),
                       na  = sum(y1NA),
                       edim = self$options$Ent_edim,
                       factor = self$options$Ent_r,
                       SD = SD,
                       radius = self$options$Ent_r*SD,
                       sampEnt = SampEn,
                       apprEnt = ApprEn,
                       locVar = localVar,
                       globVar = globalVar,
                       RR = RelR))




      }
    )
  )
}
