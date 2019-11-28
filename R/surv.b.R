

#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
survClass <- R6::R6Class(
    "survClass",
    inherit = survBase,
    private = list(
        .init = function() {
            
            groups <- private$.groups()
            
            summary <- self$results$summary
            for (group in groups)
                summary$addRow(rowKey=group, list(group=group))
            
            tests <- self$results$tests
            if (length(groups) >= 2) {
                comparisons <- combn(groups, 2)
                for (i in seq_len(ncol(comparisons))) {
                    key = comparisons[,i]
                    tests$addRow(rowKey=key)
                }
            }
            else {
                tests$setVisible(FALSE)
            }
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
            
            eventVarName <- self$options$event
            elapsedVarName <- self$options$elapsed
            
            if (is.null(eventVarName) || is.null(elapsedVarName))
                return()
            
            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')
            
            elapsed <- toNumeric(self$data[[elapsedVarName]])
            event   <- ifelse(self$data[[eventVarName]] == self$options$eventLevel, 1, 0)
            
            group <- 1
            if ( ! is.null(self$options$groups))
                group <- as.factor(self$data[[self$options$groups]])
            
            data <- data.frame(elapsed=elapsed, event=event, group=group)
            data <- na.omit(data)
            
            if (nrow(data) == 0)
                stop('Data contains no (complete) rows')
            
            s <- survival::survfit(formula=survival::Surv(elapsed, event) ~ group, data=data)
            sTable <- summary(s)$table
            st <- self$results$summary
            
            for (i in seq_len(nrow(s))) {
                if (nrow(s) == 1)
                    g <- sTable
                else
                    g <- sTable[i,]
                nevents <- sum(g['events'])
                n <- g['n.max']
                ncensor <- n - nevents
                median <- g['median']
                mean <- g['*rmean']
                prop <- nevents / n
                
                st$setRow(rowNo=i, list(
                    censored=ncensor,
                    events=nevents,
                    n=n,
                    prop=nevents/n,
                    median=median,
                    mean=mean))
            }
            
            st$setStatus('complete')
            
            self$results$sc$setState(s)
            self$results$hf$setState(s)
            self$results$chf$setState(s)
            
            tt <- self$results$tests
            if (tt$isNotFilled() && length(self$options$tests) > 0) {
            
                groups <- private$.groups()
                
                if (length(groups) >= 2) {
                    groupsData <- list()
                    for (group in groups) {
                        ss <- (data$group == group)
                        groupsData[[group]] <- subset(data, subset=ss, select=c('elapsed', 'event'))
                    }
                }
                
                for (pair in tt$rowKeys) {
                    
                    x <- groupsData[[pair[1]]]
                    y <- groupsData[[pair[2]]]
                    
                    for (i in seq_along(self$options$tests)) {
                        test <- self$options$tests[i]
                        
                        if (tt$isFilled(rowKey=pair, col=paste0('nu[', test, ']')))
                            next()
                        
                        private$.checkpoint()
                        
                        result <- EnvStats::twoSampleLinearRankTestCensored(
                            test = test,
                            x = x$elapsed,
                            x.censored = ! x$event,
                            y = y$elapsed,
                            y.censored = ! y$event,
                            censoring.side = 'right')
                        
                        row <- list()
                        
                        row[[paste0('z[', test, ']')]] <- result$statistic['z']
                        row[[paste0('nu[', test, ']')]] <- result$statistic['nu']
                        row[[paste0('nuse[', test, ']')]] <- sqrt(result$statistic['var.nu'])
                        row[[paste0('p[', test, ']')]] <- result$p.value
                        
                        tt$setRow(rowKey=pair, values=row)
                    }
                }
            }
        },
        .plot=function(image, theme, ggtheme, ...) {
            
            state <- image$state
            if (is.null(state))
                return(FALSE)
            
            if (identical(image, self$results$sc)) {
                ylab <- 'Survival'
                fun <- NULL
                cens <- self$options$cens
                ci <- self$options$ci
                ylim <- c(0, 1)
            } else if (identical(image, self$results$hf)) {
                
                return(FALSE)
                
                ylab <- 'Hazard Function'
                fun <- 'event'
                cens <- FALSE
                ci <- FALSE
                ylim <- c(0, 1)
            } else {
                ylab <- 'Cumulative Hazard'
                fun <- 'cumhaz'
                cens <- FALSE
                ci <- FALSE
                ylim <- NULL
            }
            
            plot <- ggfortify:::autoplot.survfit(
                object=state,
                fun=fun,
                xlab='Elapsed',
                ylab=ylab,
                ylim=ylim,
                surv.size = 1,
                censor.size = 8,
                censor.alpha = 0.8,
                conf.int=ci,
                censor=cens)
            
            plot <- plot + ggtheme
            
            print(plot)
            
            return(TRUE)
        })
)
