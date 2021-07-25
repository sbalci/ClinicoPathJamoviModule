# from https://github.com/AlbertoAlvarezIglesias2019/SimpleSurvival

comparingSurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "comparingSurvivalClass",
    inherit = comparingSurvivalBase,
    private = list(

      .init = function() {

        groups <- private$.groups()

        summary <- self$results$compsurvTable1
        for (group in groups)
          summary$addRow(rowKey=group, list(group=group))

        summary <- self$results$compsurvTable3
        for (group in groups)
          summary$addRow(rowKey=group, list(group=group))
      },
      .groups = function() {
        if (is.null(self$options$groups))
          group <- NULL
        else
          group <- self$data[[self$options$groups]]

        groups <- levels(group)
        if (length(groups) == 0)
          groups = ''

        groups
      },
      .run = function() {

        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)

        times <- self$options$times
        status <- self$options$status
        groups <- self$options$groups
        tmpDat <- self$data

        if (is.null(times) || is.null(status) || is.null(groups))
          return()



        form <- paste("survival::Surv(",times,",",status,") ~ ",groups)
        form <- as.formula(form)


        tmpDat[,status] <- as.numeric(as.character(tmpDat[,status]))

        fit <- survival::survdiff(form, data = tmpDat)

        #$results$text$setContent(fit)



        table1 <- self$results$compsurvTable1
        for (i in 1:length(unique(tmpDat[, groups ])) ) {
          table1$setRow(rowNo=i, values=list(
            n=fit$n[i],
            obs=fit$obs[i],
            exp=fit$exp[i],
            ovse= (fit$obs[i] - fit$exp[i])^2/fit$exp[i],
            ovsev=(fit$obs[i] - fit$exp[i])^2/fit$var[i,i]))
        }

        table2 <- self$results$compsurvTable2
        table2$setRow(rowNo=1, values=list(
          var="Log-Rank",
          chisqr=fit$chisq,
          df=length(fit$n)-1,
          p=pchisq(fit$chisq, length(fit$n)-1, lower.tail = FALSE)))

        fit1 <- survival::survfit(form, data = tmpDat)
        temp <- as.data.frame(summary(fit1)$table)


        table3 <- self$results$compsurvTable3
        for (i in 1:length(unique(tmpDat[, groups ])) ) {
          table3$setRow(rowNo=i, values=list(
            median=temp$median[i],
            cilb=temp$`0.95LCL`[i],
            ciub=temp$`0.95UCL`[i]
          ))
          }



        image <- self$results$plot
        image$setState(fit1)

        image2 <- self$results$plot2
        image2$setVisible(visible=FALSE)

        if (self$options$loglogyn) {
          image2$setState(fit1)
          image2$setVisible(visible=TRUE)
        } else image2$setVisible(visible=FALSE)

      },
      .plot=function(image, ...) {

        times <- self$options$times
        status <- self$options$status
        groups <- self$options$groups
        tmpDat <- self$data

        if (is.null(times) || is.null(status) || is.null(groups))
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
      },
      .plot2=function(image2, ...) {

        times <- self$options$times
        status <- self$options$status
        groups <- self$options$groups
        tmpDat <- self$data

        if (is.null(times) || is.null(status) || is.null(groups))
          return()

        plotData <- image2$state
        conf.int <- self$options$ciyn
        xlab <- ifelse(self$options$timeunits=="None","Time",paste("Time (",self$options$timeunits,")",sep=""))
        plot <- ggfortify:::autoplot.survfit(plotData,
                                             fun = function(x) log(-log(x)),
                                             xlab=xlab,
                                             ylab = "Cumulative Hazard (log scale)",
                                             conf.int = conf.int)
        print(plot)
        TRUE
      }



        )
)
