# from https://github.com/FredHasselman/casnet-jmvMAC


recNetClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "recNetClass",
    inherit = recNetBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

          if (is.null(self$options$y1)){
            return(FALSE)
          }

            y1 <- self$options$y1

            data <- self$data

            if(!jmvcore::canBeNumeric(data[[y1]])){
                data[[y1]] <- as.character(data[[y1]])
            } else {
                data[[y1]] <- jmvcore::toNumeric(data[[y1]])
            }

            Nna <- sum(is.na(data[[y1]]))
            data <- na.omit(data)
            standardise <- "none"

            if(is.factor(data[[y1]])|is.character(data[[y1]])){
                if(is.factor(data[[y1]])&is.ordered(data[[y1]])){
                    y1.vlevel <- "Ordered categorical"
                } else {
                    y1.vlevel <- "Unordered categorical"
                }
            }
            if(is.numeric(data[[y1]])){
                y1.vlevel <- typeof(data[[y1]])
            }

            v1_discretised <- v2_discretised <- "no"
            if(!y1.vlevel%in%c("Ordered categorical","Unordered categorical","integer")){
                v1_discretised <- "yes"
            }

            if(self$options$standardise=="none"){
                standardise <- "none"
            }

            y1vec <- data[[y1]]

            if(is.numeric(y1vec)){

                y1.vlevel <- ifelse(all(is.wholenumber(y1vec)),"Discrete","Continuous")

                if(self$options$standardise=="meanSD"){
                    standardise <- "mean.sd"
                }
                if(self$options$standardise=="medianMAD"){
                    standardise <- "median.mad"
                }

                if(self$options$standardise=="unitScale"){
                    y1vec <- elascer(y1vec)
                    standardise <- "unit scale"
                    y1.vlevel <- "Continuous"
                }

                if(self$options$standardise=="symbolicScale"){
                    y1vec <- ts_symbolic(y1vec)
                    y1vec <- y1vec[!is.na(y1vec)]

                    standardise <- "symbolic"
                    y1.vlevel <- "Ordered categorical"
                }

                if(any(standardise%in%c("mean.sd","median.mad"))){
                    y1vec <- ts_standardise(y1vec, type = standardise,  adjustN = FALSE)
                    y1.vlevel <- "Continuous"
                }
            }

            v1 <- as.numeric_discrete(y1vec, sortUnique = TRUE)

            TStable <- self$results$tblTS

            # Time Series table ----
            TStable$setRow(rowNo=1,
                           values=list(
                               var = y1,
                               vlevel = y1.vlevel,
                               N   = NROW(v1),
                               na  = Nna,
                               uni_obs = length(unique(v1)),
                               transformed = standardise)
            )

            tsData <- data.frame(t  = seq_along(v1),
                                 y1       = v1,
                                 y1_lab  = names(v1),
                                 stringsAsFactors = FALSE
            )
            if(!is.numeric(y1vec)|standardise=="symbolicScale"){
                #codingFrame <- data.frame(value = cod(ing, label = names(coding))
                self$results$warnings$setContent(dplyr::as_tibble(tsData))
            }
            # tsImage <- self$results$tsplot
            # tsImage$setState(tsData)

            emLag <- self$options$emLag
            emDim <- self$options$emDim
            if(emDim<=0){emDim<-1}

            weighted   <- TRUE
            weightedBy <- "rt"
            switch(self$options$edgeWeight,
                none = weighted <- FALSE,
                rt = weightedBy <- "rt",
                rf = weightedBy <- "rf",
                si = weightedBy <- "si",
            )

            emRad <- NULL
            if(self$options$fixed%in%"RAD"){
                emRad <- self$options$fixRAD
                #if(emRad<=0){emRad<-emRad+.Machine$double.eps}
            }
            if(self$options$fixed%in%"RR"){
                emRad <- crqa_radius(y1 = tsData$y1, emLag = emLag, emDim = emDim, targetValue = self$options$fixRR)$Radius
            }
            if(self$options$fixed%in%"NO"){
                RN <- rn(y1 = tsData$y1, emDim = emDim, emLag = emLag,
                         method = as.character(self$options$norm),
                         directed = FALSE, weighted = weighted, weightedBy = weightedBy)
            } else {
                RN <- rn(y1 = tsData$y1, emDim = emDim, emLag = emLag, emRad = emRad,
                         method = as.character(self$options$norm),
                         directed = FALSE, weighted = weighted, weightedBy = weightedBy)
            }

            rpImage <- self$results$rpplot
            rpImage$setState(RN)

            if(!is.null(emRad)){

                if(self$options$DLmax<=0){
                    DLmax <- length(Matrix::diag(RN))-1
                }
                if(self$options$VLmax<=0){
                    VLmax <- length(Matrix::diag(RN))-1
                }
                if(self$options$HLmax<=0){
                    HLmax <- length(Matrix::diag(RN))-1
                }

                crqa_all <- crqa_rp(RM = RN, emRad = emRad,
                                    DLmin = self$options$DLmin, DLmax = DLmax,
                                    VLmin = self$options$VLmin, VLmax = VLmax,
                                    HLmin = self$options$HLmin, HLmax = HLmax,
                                    theiler = self$options$theiler,
                                    matrices = TRUE)

                crqa_out <- crqa_all$crqaMeasures

                # RPtable <- self$results$tblRP
                #
                # RPtable$setRow(rowNo=1,
                #                values = list(
                #                    emRad = emRad,
                #                    RP = crqa_out$RP_N,
                #                    RN = rp_size(RM, AUTO=TRUE),
                #                    RR = crqa_out$RR,
                #                    SING = crqa_out$SING_N,
                #                    DIV = crqa_out$DIV_dl,
                #                    REP = crqa_out$REP_av,
                #                    ANI = crqa_out$ANI)
            }

          rnetImage <- self$results$rnetplot
          rnetImage$setState(RN)


        },

          .rpplot=function(rpImage, ...) {

            ready <- TRUE
            rm <- rpImage$state

            if(is.null(self$options$y1)){
              ready <- FALSE
            }

            if(ready){

              rm <- rpImage$state

              # RadiusRRbar <- FALSE
              # if(self$options$fixed%in%"NO"){
              #   RadiusRRbar <- TRUE
              # }

              #   if(plyr::is.discrete(df$y1)){

              rppl <- rn_plot(rm,
                              plotDimensions = TRUE,
                              xlab = self$options$y1,
                              ylab = self$options$y1,
                              radiusValue = attributes(rm)$emRad)

              print(rppl)
              TRUE
            }
          },

          .rnetplot=function(rnetImage, ...) {

            ready <- TRUE
            rnet <- rnetImage$state

            if(is.null(self$options$y1)){
              ready <- FALSE
            }

            if(ready){

              rnet <- rnetImage$state

              #rnet[rnet<=quantile(rnet[upper.tri(rnet)],self$options$Qtile)] <- 0

              mode <- "undirected"

              # if(self$options$Gdirect){
              #   mode <- "directed"
              # }


              tvec <- 1:length(Matrix::diag(rnet))
              w <- max(plyr::laply(paste0(tvec), nchar), na.rm = TRUE)
              colnames(rnet) <- formatC(tvec, width=w, flag="0")
              rownames(rnet) <- formatC(tvec, width=w, flag="0")

              switch (self$options$nsize,
                degree = nsize   <- "degree",
                hubscore = nsize <- "hubscore",
                strength = nsize <- "strength",
                fixed = nsize <- .3
              )

              # switch (self$options$nsize,
              #         degree = lsize <- "asnodesize",
              #         hubscore = lsize <- "asnodesize",
              #         fixed = lsize <- .6
              # )


              eweight  <- "weight"
              weighted <- TRUE
              if(self$options$edgeWeight=="none"){
                  weighted <- NULL
                  eweight  <- 1
              }


              g <- igraph::graph_from_adjacency_matrix(rnet, mode = mode, weighted = weighted, diag = FALSE)

              if(self$options$PruneDegree>0){
                g <- igraph::delete.vertices(g, which(igraph::degree(g) < self$options$PruneDegree))
              }
              if(self$options$PruneWeight>0){
                g <- igraph::delete.edges(g, which(igraph::E(g)$weight < self$options$PruneWeight))
              }

              g <- plotNET_prep(g, nodesize = nsize, edgeweight = eweight)

              self$options$Glayout

              switch(self$options$Glayout,
                     ar = layout <- "Archimedean",
                     be = layout <- "Bernoulli",
                     fe = layout <- "Fermat",
                     eu = layout <- "Euler")

              if(self$options$LayoutA==0){
                  switch(self$options$Glayout,
                         ar = layoutA <- 1,
                         be = layoutA <- 1,
                         fe = layoutA <- 1,
                         eu = layoutA <- 1)
              } else {
                  layoutA = 1
              }
              if(self$options$LayoutB==0){
                  switch(self$options$Glayout,
                         ar = layoutB <- 1,
                         be = layoutB <- 0.1,
                         fe = layoutB <- 1,
                         eu = layoutB <- .5)
              } else {
                  LayoutB <- NULL
              }

              if(!self$options$Glabels){
                  Glabels <- NULL
              } else {
                  Glabels <- TRUE
              }


              gn <- make_spiral_graph(g = g,
                                      type = layout,
                                      arcs = self$options$Narcs,
                                      a = layoutA,
                                      b = layoutB,
                                      markTimeBy = Glabels,
                                      doPlot = FALSE)


              # if(self$options$Glabels){
              #   vlabs <- igraph::V(g)$name
              # } else {
              #   vlabs <- ""
              # }

              # if(self$options$Glayout%in%"cl"){
              #   membership <- cut(tvec,breaks = 10,labels = FALSE)
              #   names(membership) <- igraph::V(g)$name
              #   igraph::E(g)$weight <- plotNET_groupWeight(g, membership, weigth.within = 5, weight.between = 10, preserve.weight.within = TRUE,preserve.weight.between = TRUE)
              # }
              #
              # g <- plotNET_prep(g, labels = vlabs, nodesize = nsize, edgeweight = eweight,labelsize = lsize, doPlot = FALSE)
              # switch(self$options$Glayout,
              #        ni = l <- igraph::layout_nicely(g),
              #        st = l <- igraph::layout_as_star(g),
              #        ci = l <- igraph::layout_in_circle(g),
              #        tr = l <- igraph::layout_as_tree(g),
              #        fr = l <- igraph::layout_with_fr(g),
              #        md = l <- igraph::layout_with_mds(g),
              #        cl = l <- igraph::layout_with_fr(g, weights=igraph::E(g)$weight)
              # )


              plot(gn)
              #print(g)
              TRUE
            }
        }
    )
)
