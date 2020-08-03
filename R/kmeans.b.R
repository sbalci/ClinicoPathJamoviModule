# from https://github.com/stmueller/MTUcluster

kmeansClass <- R6::R6Class(
    "kmeansClass",
    inherit = kmeansBase,
    private = list(


        .init = function(){


            ##initialize the centroids table:
            tab2 <- self$results$centroids
            vars <- self$options$vars
            vars <- factor(vars,levels=vars)#reset the order in the order specified
            nVars <- length(vars)
            k <- self$options$k

            tab2$addColumn(name="cluster",title="Cluster ID",type='integer')

            for(i in 1:nVars)
            {
                var <- vars[[i]]
                tab2$addColumn(name = paste0(var),
                               index = i,
                               type= 'number',
                               format='zto',visible=TRUE)

            }


            values <- list(cluster=1)

            for(i in 1:nVars)
            {
                values[[paste0(vars[[i]])]]  <- '\u2014'
            }

            ##add dummy values to table:
            for(j in 1:k)
            {
               values[["cluster"]]<-j
               tab2$setRow(rowNo=j,values)
            }
              print("done initializing")
            },


        .run = function() {
            print('.runnig"')
            text <- "started"

            if(!is.null(self$options$vars))
            {

                dat2 <- jmvcore::select(self$data,self$options$vars)

                ##standardize/normalize if necessary.
                if(self$options$stand)
                {
                    for(var in 1:ncol(dat2))
                        {
                         tmp <- dat2[,var]
                         dat2[,var] <- (tmp - mean(tmp,na.rm=T))/sd(tmp,na.rm=T)

                        }
                }

                if(dim(dat2)[2]>0)
                {
                    model <- stats::kmeans(dat2,
                                           centers=self$options$k,
                                           nstart=self$options$nstart,
                                           algorithm=self$options$algo,
                                           )

                    tab <- self$results$clustering
                    tab$deleteRows()



                    clusters <- table(model$cluster)
                    rowno <- 1
                    for(i in 1:dim(clusters))
                    {
                        tab$addRow(rowKey=paste("Cluster",i),
                                   values=list(cluster=i,count=clusters[i]))
                    }
                    ##the centroids table:
                    tab2 <- self$results$centroids
                    vars <- self$options$vars
                    vars <- factor(vars,levels=vars)

                    nVars <- length(vars)
                    k <- self$options$k

                    for(i in 1:k)
                    {
                       values <- unlist(list(cluster=i,model$centers[i,]))
                       tab2$setRow(rowNo=i,values)

                    }


                 text <- print(model)


                plotData <- data.frame(
                    cluster=as.factor(rep(1:k,nVars)),
                    var=rep(vars,each=k),
                    centers=as.vector(model$centers))

                image <- self$results$plot
                image$setState(plotData)

                } else{
                    image <- self$results$plot
                    image$setState(NULL)
                    text <- "No results"


                }



                # `self$options` contains the options
                # `self$results` contains the results object (to populate)

                # `self$data` contains the data
                # `self$options` contains the options
                # `self$results` contains the results object (to populate)
            } else {
                text <- cat(text, "Error in clustering analysis")
                image <- self$results$plot
                image$setState(NULL)

            }

            textResults <- self$results$text
            textResults$content <-  text
            footer <- "Notes on K-means clustering
Results classify each row into a class, based on a pre-specified
fixed number of classes, in an unsupervised fashion.  Each class is
defined by its center on each feature/dimension, and each case is
classified into its nearest class."

            self$results$footer$content <- footer

            },
        .plot = function(image, ...)
          {
               plotData <- image$state
               if(!is.null(plotData))
               {
               plot <- ggplot(plotData,aes(x=centers,y=var,group=cluster,colour=cluster)) +
                       geom_path(size=1.2) + geom_point(size=4) + labs(xlab="Mean value",ylab="Feature")
               print(plot)
               }
               TRUE

        }

        )


)
