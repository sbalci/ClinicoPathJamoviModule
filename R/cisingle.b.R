# from https://github.com/ClinicoPath/PUBH5018-jamovi


ciSingleClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ciSingleClass",
    inherit = ciSingleBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            splitBy <- self$options$splitBy
            data <- self$data
            splitByTrue <- !is.null(splitBy)
            if (splitByTrue) data[[splitBy]] <- as.factor(data[[splitBy]])

            for (dep in self$options$deps) {
              # Do T-test
              x <- data[,dep]
              ciLevel <- self$options$ciWidth/100
              ttest <- t.test(x, conf.level = ciLevel)

              # Display confidence level
              disp_tx <- paste0("Confidence level = ",ciLevel*100, "%")
              self$results$conflevel$setContent(disp_tx)

              # Populate table of results
              table <- self$results$citable
              table$setRow(rowKey=dep, values=list(
                var   = dep
                ,mean = ttest$estimate
                ,lb   = ttest$conf.int[1]
                ,ub   = ttest$conf.int[2]
              ))

              if (splitByTrue) {
                for (i in levels(data[[splitBy]])){
                  # Do T-test
                  x2 <- x[data[[splitBy]]==i]
                  if (length(x2[which(!is.na(x2))]) > 1) {
                    ttest <- t.test(x2, conf.level = ciLevel)
                  } else {
                    ttest <- NULL
                    ttest$estimate <- "Not enough data"
                    ttest$conf.int <- c("","")
                  }

                  table$addRow(rowKey=paste0(dep,i), values=list(
                    var   = paste0(dep," - ", i)
                    ,mean = ttest$estimate
                    ,lb   = ttest$conf.int[1]
                    ,ub   = ttest$conf.int[2]
                  ))
                }
              }

            }

        })
)
