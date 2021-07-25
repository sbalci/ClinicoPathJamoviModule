# from https://github.com/ClinicoPath/PUBH5018-jamovi


checkdataClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "checkdataClass",
    inherit = checkdataBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            variable <- self$options$dep

            if (is.null(variable)) return()

            variable <- self$data[,variable]
            variable <- data.frame(variable=variable,nms = 1:length(variable))
            n <- 5


#            if(class(variable)!= "variable") stop("variable must be numeric")
#            else {
              variable <- variable[which(!is.na(variable$variable)),]
              variable <- variable[order(variable$variable),]
              x <- data.frame(head(variable$variable,n),  head(variable$nms,n)
                              ,tail(variable$variable,n), tail(variable$nms,n))
              names(x) <- c("Min values", "Min row number", "Max values", "Max row number")
#            }

            minTable <- self$results$minvals
            for (i in 1:n){
              minTable$setRow(rowNo=i, values=list(
                minRowNumber=x[i,2]
                ,minvalues=x[i,1]))
            }

            maxTable <- self$results$maxvals
            for (i in 1:n){
              maxTable$setRow(rowNo=i, values=list(
                maxRowNumber=x[i,4]
                ,maxvalues=x[i,3]))
            }

            variable <- self$options$dep
            variable <- self$data[,variable]
            if (sum(is.na(variable))==0) {
              self$results$missingvals$setContent("No missing values")
            } else {
              message <- paste(sum(is.na(variable))
                               , "missing values in rows: "
                               , paste(which(is.na(variable))
                                       , collapse=", "))
              self$results$missingvals$setContent(message)
            }
        })
)
