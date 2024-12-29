# from https://github.com/FredHasselman/casnet-jmvMAC


ssgBIClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "ssgBIClass",
  inherit = ssgBIBase,
  private = list(
    .run = function() {

      if (is.null(self$options$y1)|is.null(self$options$y2)){
        return(FALSE)
      }

      y1_ready <- FALSE
      y2_ready <- FALSE

      y1 <- self$options$y1
      y2 <- self$options$y2


      # Time variable ----
      t <- self$options$time

      data <- self$data

      if(!is.null(t)){
        if(!jmvcore::canBeNumeric(data[[t]])){
          t   <- "Time (generated)"
          tNA <- 0
        } else {
          data[[t]] <- jmvcore::toNumeric(data[[t]])
          tNA <- is.na(data[[t]])
        }
      } else {
        t   <- "Time (generated)"
        tNA <- 0
      }


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
      if(is.factor(data[[y2]])|is.character(data[[y2]])){
        if(is.factor(data[[y2]])&is.ordered(data[[y2]])){
          y2.vlevel <- "Ordered categorical"
        } else {
          y2.vlevel <- "Unordered categorical"
        }
      }
      if(is.numeric(data[[y2]])){
        y2.vlevel <- typeof(data[[y2]])
      }

      v1_discretised <- v2_discretised <- "no"
      if(!y1.vlevel%in%c("Ordered categorical","Unordered categorical","integer")){
        v1_discretised <- "yes"
      }
      if(!y2.vlevel%in%c("Ordered categorical","Unordered categorical","integer")){
        v2_discretised <- "yes"
      }

      Nna1 <- sum(is.na(data[[y1]]))
      Nna2 <- sum(is.na(data[[y2]]))

      idNA1 <- !is.na(data[[y1]])
      idNA2 <- !is.na(data[[y2]])

      data <- data[idNA1&idNA2,]

      if(!jmvcore::canBeNumeric(data[[y1]])){
        y1vec <- as.character(data[[y1]])
      } else {
        y1vec <- data[[y1]]
      }
      if(!jmvcore::canBeNumeric(data[[y2]])){
        y2vec <- as.character(data[[y2]])
      } else {
        y2vec <- data[[y2]]
      }

      data <- na.omit(data)

      v12 <- as.numeric_discrete(c(y1vec,y2vec), sortUnique = TRUE)
      v1 <- v12[1:length(y1vec)]
      v2 <- v12[(length(y1vec)+1):length(v12)]

      # v1 <- as.numeric_discrete(data[[y1]], sortUnique = TRUE)
      # v2 <- as.numeric_discrete(data[[y2]], sortUnique = TRUE)

      # Variable 1 ----
     # v1 <- as.numeric_discrete(data[[y1]], sortUnique = TRUE)

      # Get labels in data (obs, if any)
       v1_labs_obs    <- unique(names(v1))
       v1_Nlabs_obs   <- length(v1_labs_obs)
       v1_Nstates_usr <- self$options$v1_Nstates
       v1_Nlabs_usr   <- length(self$options$v1_labels)
       #y1_ready       <- TRUE

       if(v1_Nlabs_usr>0){
         v1_labs        <- self$options$v1_labels
         v1_labs_usr    <- strsplit(x = v1_labs, split = "[,]")[[1]]
         v1_Nlabs_usr   <- length(v1_labs_usr)
       } else {
         v1_labs_usr <- ""
         v1_Nlabs_usr <- 0
       }


       # Variable 2 ----
      # v2 <- as.numeric_discrete(data[[y2]],sortUnique = TRUE)

       # Get labels in data (obs, if any)
       v2_labs_obs    <- unique(names(v2))
       v2_Nlabs_obs   <- length(v2_labs_obs)
       v2_Nstates_usr <- self$options$v2_Nstates
       v2_Nlabs_usr   <- length(self$options$v2_labels)
       #y2_ready <- TRUE

       if(v2_Nlabs_usr>0){
         v2_labs        <- self$options$v2_labels
         v2_labs_usr    <- strsplit(x = v2_labs, split = "[,]")[[1]]
         v2_Nlabs_usr <- length(v2_labs_usr)
       } else {
         v2_labs_usr <- ""
         v2_Nlabs_usr <- 0
       }

       warningMessage1a <- paste("Variable",y1,"contains",v1_Nlabs_obs,"unique states")
       warningMessage1b <- paste("- if more states were possible, please specify missing categories!")
       warningMessage2a <- paste("Variable",y2,"contains",v2_Nlabs_obs,"unique states")
       warningMessage2b <- paste("- if more states were possible, please specify missing categories!")


       # # v2_Nstates_usr <- self$options$v2_Nstates
       # if(v1_Nstates_usr>v1_Nlabs_obs){
       #   if(v1_Nstates_usr==(v1_Nlabs_obs+v1_Nlabs_usr)){
       #     v1_labs_usr  <- c(v1_labs_usr,v1_labs_obs)
       #     v1_Nlabs_usr <- length(v1_labs_usr)
       #   }}
       #
       # if(v2_Nstates_usr>v2_Nlabs_obs){
       #   if(v2_Nstates_usr==(v2_Nlabs_obs+v2_Nlabs_usr)){
       #     v2_labs_usr  <- c(v2_labs_usr,v2_labs_obs)
       #     v2_Nlabs_usr <- length(v2_labs_usr)
       #   }}


      # if(v1_Nlabs_usr>0&v1_Nstates_usr>0){
      #   if(v1_Nlabs_obs!=v1_Nstates_usr){
      #     jmvcore::reject(jmvcore::format('Number of user defined states and number of user defined state labels mismatch for {}',y1))
      #   }}
      #
      # if(v2_Nlabs_usr>0&v2_Nstates_usr>0){
      #   if(v2_Nlabs_obs!=v2_Nstates_usr){
      #     jmvcore::reject(jmvcore::format('Number of user defined states and number of user defined state labels mismatch for {}',y2))
      #   }}
      #
      #
      # NLABS_V1 <- v1_Nlabs_obs + v1_Nlabs_obs
      # if(self$options$v1_Nstates>0){
      #   if(v1_Nstates_usr<v1_Nstates_obs){
      #     jmvcore::reject(jmvcore::format('Number of user defined states and number of observed states mismatch for {}',y1))
      #   }
      #   if(v1_Nstates_usr>v1_Nstates_obs){
      #     warningMessage1b <- paste("-", v1_Nstates_usr, "were expected, please specify labels for",(v1_Nstates_usr-v1_Nstates_obs),"missing states!")
      #   }
      #   if(NLABS_V1==v1_Nlabs_obs){
      #     warningMessage1b <- paste("-", v1_Nstates_usr, "were expected,",v1_Nlabs_obs,"were provided.")
      #     LABS_v1 <- c(v1_labs_obs, v2_labs_usr)
      #   }

        y1_ready <- TRUE

      # }
      #
      # NLABS_V2 <- v2_Nlabs_obs + v2_Nlabs_obs
      # if(self$options$v2_Nstates>0){
      #   if(v2_Nstates_usr<v2_Nstates_obs){
      #     jmvcore::reject(jmvcore::format('Number of user defined states and number of observed states mismatch for {}',y2))
      #   }
      #   if(v2_Nstates_usr>v2_Nstates_obs){
      #     warningMessage2b <- paste("-", v2_Nstates_usr, "were expected, please specify labels for",(v2_Nstates_usr-v2_Nstates_obs),"missing states!")
      #   }
      #   if(NLABS_V2==v2_Nstates_usr){
      #     warningMessage2b <- paste("-", v2_Nstates_usr, "were expected,",v2_Nlabs_obs,"were provided.")
      #     LABS_v2 <- c(v2_labs_obs, v2_labels_usr)
      #   }
         y2_ready <- TRUE
      # }
      #

       self$results$warnings$setContent(paste(paste(warningMessage1a,warningMessage1b),
                                              paste(warningMessage2a,warningMessage2b),sep="\n"))

      if(all(y1_ready,y2_ready)){

        COORD_v1 <- unique(sort(v1))
        COORD_v2 <- unique(sort(v2))

        LABS_v1 <- unique(names(sort(v1)))
        LABS_v2 <- unique(names(sort(v2)))

        NLABS_V1 <- length(LABS_v1)
        NLABS_V2 <- length(LABS_v2)

        # SSG
        ssg   <- matrix(data = 0, nrow = NLABS_V1,ncol= NLABS_V2, dimnames = list(LABS_v1,LABS_v2))
        dlist <- list(LABS_v1,LABS_v2)
        names(dlist) <- c(y1,y2)

        ssg_freq       <- as.data.frame(arrayInd(which(ssg==0),.dim = dim(ssg), .dimnames = dlist,useNames = TRUE))
        ssg_freq$combi <- interaction(ssg_freq[[1]],ssg_freq[[2]])

        # Expected Grid ----
        ssg      <- data.frame(expand.grid(y1.labs=LABS_v1,y2.labs=LABS_v2),expand.grid(y1=COORD_v1,y2=COORD_v2))
        colnames(ssg) <- c(y1,y2,paste0("COORD.",y1),paste0("COORD.",y2))
        ssg$cell  <- paste0(ssg[,1],".",ssg[,2])
        ssg$loc   <- paste0(ssg[,3],".",ssg[,4])
        ssg$value <- NA

        for(i in unique(ssg[,3])){
          ssg$value[ssg[,3]==i] <-  (i:(i+max(ssg[,4])-1)%%2)[seq_along(ssg$value[ssg[,3]==i])]
        }
        ssg$value <- factor(ssg$value)

        # for(i in seq_along(ssg$cell)){
        #   ssg$value[ssg$cell%in%ssg$cell[i]] <- i%%2
        # }
        # ssg$value <- factor(ssg$value)

        # Handle Trajectories and Waves ----
        SSG_cell_obs   <- paste0(v1,".",v2)
        #SSG_labs_obs   <- paste0(names(v1),".",names(v2))

        #SSG_cell_obs   <- factor(paste0(v1,".",v2))
        SSG_cell_unobs <- ssg$cell[!ssg$loc%in%SSG_cell_obs]

        if(is.null(self$options$trajectories)){
          traj_ID <- c("A")
          trajectories <- "One"
        } else {
          trajectories <- self$options$trajectories
          data[[trajectories]] <- as.factor(data[[trajectories]])
          traj_ID      <- unique(data[[trajectories]])
        }
        if(is.null(self$options$waves)){
          wave_ID <- c("0")
          waves <- "None"
        } else {
          waves    <- self$options$waves
          data[[waves]] <- as.factor(data[[waves]])
          wave_ID  <- unique(data[[waves]])
        }

        SSG_traj <- list()
        cnt <- 0
        for(traj in traj_ID){

          if(is.null(self$options$trajectories)){
            trajs <- rep(TRUE,NROW(data))
          } else {
            trajs <- data[[trajectories]]%in%traj
          }
          for(wav in wave_ID){
            if(is.null(self$options$waves)){
              wavs <- rep(TRUE,NROW(data))
            } else {
              wavs <- data[[waves]]%in%wav
            }

            cnt <- cnt+1

            cell_obs   <- paste0(v1[trajs&wavs],".",v2[trajs&wavs])
            labs_obs   <- paste0(names(v1[trajs&wavs]),".",names(v2[trajs&wavs]))
            cell_unobs <- ssg$cell[!ssg$cell%in%cell_obs]
            labs_unobs <- paste0(ssg[[y1]][!ssg$cell%in%cell_obs],".",ssg[[y2]][!ssg$cell%in%cell_obs])

            if(t%in%"Time (generated)"){
              tvec <- seq_along(v1[trajs&wavs])
            } else {
              tvec <- as.numeric(data[[t]][trajs&wavs])
            }

            durations  <- casnetjmv::ts_duration(labs_obs, timeVec = tvec)
            durations2 <- casnetjmv::ts_duration(cell_obs, timeVec = tvec)
            durations$y.value <- durations2$y.name
            rm(durations2)
            durations$duration.time <- abs(durations$duration.time)
            durations$Trajectory <- traj
            durations$Wave  <- wav
            durations$value <- NA
            durations$value <- factor(plyr::laply(durations$y.name,function(tt){as.numeric(paste(ssg$value[ssg$cell%in%tt]))}))

            durations$y1      <- plyr::laply(strsplit(paste(durations$y.value),"[.]"),function(n1){n1[1]})
            durations$y2      <- plyr::laply(strsplit(paste(durations$y.value),"[.]"),function(n2){n2[2]})
            durations$x.from  <- durations$y1 #plyr::laply(durations$y.name,function(t){as.numeric(ssg[,3][ssg$cell%in%t])})
            durations$y.from  <- durations$y2   #plyr::laply(durations$y.name,function(t){as.numeric(ssg[,4][ssg$cell%in%t])})
            durations$y1      <- plyr::laply(strsplit(paste(durations$y.value),"[.]"),function(n1){n1[1]})
            durations$y2      <- plyr::laply(strsplit(paste(durations$y.value),"[.]"),function(n2){n2[2]})
            durations$y1_labs <- plyr::laply(strsplit(paste(durations$y.name),"[.]"),function(n1){n1[1]})
            durations$y2_labs <- plyr::laply(strsplit(paste(durations$y.name),"[.]"),function(n2){n2[2]})
            durations$trajectory.id <- 1:NROW(durations)

            # durations <- ts_duration(labs_obs, timeVec = tvec)
            # durations$duration.time <- abs(durations$duration.time)
            # durations$Trajectory = traj
            # durations$Wave = wav
            # durations$value   <- NA
            # durations$value   <- factor(plyr::llply(durations$y.name,function(tt){as.numeric(paste(ssg$value[ssg$cell%in%as.character(tt)]))}))
            # durations$x.from  <- plyr::laply(durations$y.name,function(tt){as.numeric(ssg[,3][ssg$cell%in%tt])})
            # durations$y.from  <- plyr::laply(durations$y.name,function(tt){as.numeric(ssg[,4][ssg$cell%in%tt])})
            # durations$y1      <- plyr::laply(strsplit(paste(cell_obs),"[.]"),function(n1){n1[1]})
            # durations$y2      <- plyr::laply(strsplit(paste(cell_obs),"[.]"),function(n1){n1[2]})
            # durations$y1_labs <- plyr::laply(strsplit(paste(labs_obs),"[.]"),function(n1){n1[1]})
            # durations$y2_labs <- plyr::laply(strsplit(paste(labs_obs),"[.]"),function(n1){n1[2]})
            # # durations$y1 <- plyr::laply(strsplit(paste(durations$y.name),"[.]"),function(n1){n1[1]}) #, labels = LABS_v1)
            # # durations$y2 <- plyr::laply(strsplit(paste(durations$y.name),"[.]"),function(n1){n1[2]}) #, labels = LABS_v2)
            # durations$trajectory.id <- 1:NROW(durations)

            # durations$y1_labs <- plyr::laply(durations$y1, function(l){unique(names(v1)[v1==l])})
            # durations$y2_labs <- plyr::laply(durations$y2, function(l){unique(names(v2)[v2==l])})

            # if(any(is.na(durations$value))){
            #
            # }

            ssg_obs <- durations %>% dplyr::group_by(y.name) %>%
              dplyr::summarise(Trajectory = traj,
                               Wave       = wav,
                               Nvisits  = n(),
                               Nreturns = Nvisits-1,
                               MNduration = mean(duration.time), na.rm=TRUE,
                               SDduration = stats::sd(duration.time, na.rm=TRUE),
                               Nevents = sum(duration.samples, na.rm=TRUE),
                               MNevent = sum(duration.time, na.rm=TRUE)/sum(duration.samples, na.rm=TRUE),
                               SDevent = stats::sd(duration.samples/duration.samples, na.rm=TRUE),
                               MNreturn = 0,
                               SDreturn =0)

            SSG_traj[[cnt]] <- list(cell_obs = cell_obs,
                                    cell_unobs = cell_unobs,
                                    durations = durations,
                                    ssg_obs = ssg_obs)

          } # traj
        } # wav

        names(SSG_traj) <- levels(interaction(traj_ID,wave_ID))

        dur_data <- plyr::ldply(SSG_traj, function(ssgl){data.frame(ssgl$durations, stringsAsFactors = FALSE)})
        dur_data$Trajectory <- factor(dur_data$Trajectory)
        dur_data$Wave <- factor(dur_data$Wave)

        obs_data   <- plyr::ldply(SSG_traj, function(ssgl){data.frame(ssgl$cell_obs, stringsAsFactors = FALSE)})
        unobs_data <- plyr::ldply(SSG_traj, function(ssgl){data.frame(ssgl$cell_unobs, stringsAsFactors = FALSE)})

        checkerboardObs <- sort(as.character(unique(dur_data$y.name)))
        tmp <- ssg[!ssg$cell%in%checkerboardObs,]
        if(NROW(tmp)>=1){
         dur_data <- dplyr::add_row(dur_data,.id = "unobs", y1 = tmp[[y1]], y2 = tmp[[y2]], y.name = tmp$cell, value = NA)
        }

        dur_data$y1 <- factor(dur_data$y1,ordered = TRUE)
        dur_data$y2 <- factor(dur_data$y2,ordered = TRUE)

        TStable <- self$results$tblTS

        # Time Series table ----
        TStable$setRow(rowNo=1,
                       values=list(
                         var = y1,
                         vlevel = y1.vlevel,
                         N   = NROW(na.omit(data[[y1]])),
                         na  = Nna1,
                         uni_obs = v1_Nlabs_obs,
                         uni_exp = v1_Nlabs_obs,
                         discretised = "")
        )

        TStable$setRow(rowNo=2,
                       values=list(
                         var = y2,
                         vlevel = y2.vlevel,
                         N   = NROW(na.omit(data[[y2]])),
                         na  = Nna2,
                         uni_obs = v2_Nlabs_obs,
                         uni_exp = v2_Nlabs_obs,
                         discretised = "")
        )

        # Grid table ----
        SSG_obs  <- plyr::ldply(SSG_traj, function(ssgl){ssgl$ssg_obs})
        SSG_obs$Trajectory <- factor(SSG_obs$Trajectory)
        SSG_obs$Wave <- factor(SSG_obs$Wave)
        traj_max <- plyr::ldply(SSG_traj, function(ssgl){max(ssgl$durations$t.end, na.rm = TRUE)})

        SSGtable <- self$results$tblSSG
        SSGtable$setRow(rowNo=1,
                        values=list(
                          Ntraj = length(unique(SSG_obs$Trajectory)),
                          Nwave = length(unique(SSG_obs$Wave)),
                          MNtrajDuration = mean(traj_max$V1, na.rm = TRUE),
                          SDtrajDuration = stats::sd(traj_max$V1, na.rm = TRUE),
                          uni_NstatesSSG = length(unique(SSG_cell_obs)),
                          uni_expSSG     = NROW(ssg$cell),
                          uni_unobsSSG   = length(unique(SSG_cell_unobs)),
                          Nreturns = sum(SSG_obs$Nreturns[SSG_obs$Nreturns>=self$options$MinReturns], na.rm = TRUE))
        )

        # Trajectories table ----

        traj_data <- plyr::ldply(SSG_traj, function(ssgl){ssgl$durations})
        # traj_data$Trajectory <- factor(traj_data$Trajectory)
        # traj_data$Wave <- factor(traj_data$Wave)
        traj_data <- traj_data %>% dplyr::group_by(Trajectory,Wave) %>%
          dplyr::summarise(TRAJduration = max(t.end, na.rm = TRUE),
                           TRAJvisited = length(unique(y)),
                           NeventsTRAJ = sum(duration.samples, na.rm = TRUE),
                           MNeventTRAJ = mean(duration.time, na.rm = TRUE),
                           SDeventTRAJ = stats::sd(duration.time, na.rm = TRUE)
          )

        traj_data_state <- SSG_obs %>% dplyr::group_by(Trajectory,Wave) %>%
          dplyr::summarise(TRAJvisited    = sum(Nvisits, na.rm = TRUE),
                           MNdurationTRAJ = mean(MNduration, na.rm = TRUE),
                           SDdurationTRAJ = stats::sd(MNduration, na.rm = TRUE)
                           )

        SSGtableTrajectories <- self$results$tblSSGtrajectories
        for (j in seq_along(traj_data$Trajectory)){
          SSGtableTrajectories$addRow(j,
                                      values=list(
                                        TRAJid = traj_data$Trajectory[j],
                                        TRAJwave = traj_data$Wave[j],
                                        TRAJduration = traj_data$TRAJduration[j],
                                        TRAJvisited = traj_data_state$TRAJvisited[j],
                                        MNdurationTRAJ = traj_data_state$MNdurationTRAJ[j],
                                        SDdurationTRAJ = traj_data_state$SDdurationTRAJ[j],
                                        NeventsTRAJ = traj_data$NeventsTRAJ[j],
                                        MNeventTRAJ = traj_data$MNeventTRAJ[j],
                                        SDeventTRAJ = traj_data$SDeventTRAJ[j],
                                        MNreturnTRAJ = 0,
                                        SDreturnTRAJ = 0
                                      ))
        }

        # States table ----
        SSGtableStates <- self$results$tblSSGstates

        for (s in seq_along(SSG_obs$y.name)){
          SSGtableStates$addRow(s,
                                values=list(
                                  TRAJid     = paste(SSG_obs$Trajectory[s]),
                                  WAVid      = paste(SSG_obs$Wave[s]),
                                  stateName  = paste(SSG_obs$y.name[s]),
                                  Nvisits    = SSG_obs$Nvisits[s],
                                  Nreturns   = SSG_obs$Nreturns[s],
                                  MNduration = SSG_obs$MNduration[s],
                                  SDduration = SSG_obs$SDduration[s],
                                  Nevents    = SSG_obs$Nevents[s],
                                  MNevent    = SSG_obs$MNevent[s],
                                  SDevent    = SSG_obs$SDevent[s],
                                  MNreturn   = SSG_obs$MNreturn[s],
                                  SDreturn   = SSG_obs$MNduration[s]
                                ))
        }

        SSG_attr  <- plyr::ldply(SSG_traj, function(ssgl){ssgl$durations})
        SSG_attr$Trajectory <- factor(SSG_attr$Trajectory)
        SSG_attr$Wave       <- factor(SSG_attr$Wave)
        SSG_attr$duration.time[is.na(SSG_attr$duration.time)] <- 0

        if(is.null(self$options$trajectories)){
          traj_vec <- 1
        } else {
          traj_vec <-data[[self$options$trajectories]]
        }

        if(is.null(self$options$waves)){
          wav_vec <- 1
        } else {
          wav_vec <- data[[self$options$waves]]
        }

        if(is.null(self$options$time)){
          tvec <- seq_along(v1)
        } else {
          tvec <- data[[self$options$time]]
        }

        tsData <- data.frame(t  = tvec,
                             y1 = v1,
                             y2 = v2,
                             y1_labels = names(v1),
                             y2_labels = names(v2),
                             combi = paste0(v1,".",v2),#SSG_cell_obs,
                             Trajectory = traj_vec,
                             Waves = wav_vec)

        if(self$options$doWinnowing){
            screeCut <- self$options$screeCut
            attractors <- ssg_winnowing(durations = SSG_attr,screeCut = screeCut)

            ssg_attr <- attractors$attractors %>%
              dplyr::group_by(y.name) %>%
              dplyr::summarise(Trajectory = traj,
                               Wave       = wav,
                               Nvisits = n(),
                               Nreturns = Nvisits-1,
                               MNduration = mean(duration.time),
                               SDduration = stats::sd(duration.time),
                               Nevents = sum(duration.samples),
                               MNevent = sum(duration.time)/sum(duration.samples),
                               SDevent = stats::sd(duration.samples/duration.samples),
                               MNreturn = 0,
                               SDreturn =0)

            # Attractors table ----
            SSGtableAttractors <- self$results$tblSSGattractors
            SSGtableAttractors$setVisible(visible=TRUE)

            for (s in seq_along(ssg_attr$y.name)){
              SSGtableAttractors$addRow(s,
                                        values=list(
                                          stateName = paste(ssg_attr$y.name[s]),
                                          Nvisits = ssg_attr$Nvisits[s],
                                          Nreturns = ssg_attr$Nreturns[s],
                                          MNduration = ssg_attr$MNduration[s],
                                          SDduration = ssg_attr$SDduration[s],
                                          Nevents = ssg_attr$Nevents[s],
                                          MNevent = ssg_attr$MNevent[s],
                                          SDevent = ssg_attr$SDevent[s],
                                          MNreturn = ssg_attr$MNreturn[s],
                                          SDreturn = ssg_attr$MNduration[s]
                                        ))
            }
        } else {
          SSGtableAttractors <- self$results$tblSSGattractors
          SSGtableAttractors$setVisible(visible=FALSE)
        }

        tsImage <- self$results$tsplot
        tsImage$setState(tsData)

        dur_data <- plyr::ldply(SSG_traj, function(ssgl){ssgl$durations})
        dur_data$Trajectory <- factor(dur_data$Trajectory)
        dur_data$Wave <- factor(dur_data$Wave)

        ssgImage <- self$results$SSGplot
        ssgImage$setState(dur_data)
      } else {
        return(FALSE)
      }
    },

    .tsplot=function(tsImage, ...) {

      ready <- TRUE
      dfl <- tsImage$state

      if(is.null(self$options$y1)|is.null(self$options$y2)){
        ready <- FALSE
      }
      if(!is.data.frame(dfl)){
        ready <- FALSE
      }

      if(ready){

        dfl <- tsImage$state

        df <- tidyr::gather(dfl[-c(4,5,6)], key = label, value = y1, -c("t","Trajectory","Waves"))

        df$y1         <- factor(df$y1)
        #df$combi      <- factor(df$combi)
        df$Trajectory <- factor(df$Trajectory)
        df$Waves      <- factor(df$Waves)

        pal <- scales::col_factor(palette="Blues", domain=NULL)(levels(df$y1))

        tVec <- unique(sort(dfl$t))
        breaks <- seq(1,length(tVec), length.out = 4)
        labels <- signif(tVec[breaks],2)

        tspl <- ggplot2::ggplot(df, ggplot2::aes(x=t, y=y1, fill=y1)) +
          ggplot2::geom_point(pch=22,colour="black") +
          ggplot2::facet_grid(label ~Trajectory:Waves, scales = "free") +
          ggplot2::scale_x_continuous("Time", breaks = labels, labels = labels) +
          ggplot2::scale_y_discrete("",labels = levels(df$y1)) +
          ggplot2::scale_fill_manual("Category",values = pal) +
          ggplot2::theme_bw() +
          ggplot2::theme(strip.text = ggplot2::element_text(face="bold"),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         axis.text.x = element_text(angle = 90, hjust = 0,  vjust = 0.5))

        print(tspl)
        TRUE
      }
    },

    .SSGplot=function(ssgImage, ...) {

      ready   <- TRUE
      durData <- ssgImage$state

      if(is.null(self$options$y1)|is.null(self$options$y2)){
        ready <- FALSE
      }
      if(!is.data.frame(durData)){
        ready <- FALSE
      }

      if(ready){

        pd <- ggplot2::position_jitterdodge(jitter.height=.2,jitter.width =.2, seed=12345)

        tmp <- na.omit(durData)
        tmp$unique.ID <- 1:NROW(tmp)

        g1 <- ggplot2::ggplot(tmp, ggplot2::aes(x=y1,y=y2,group=unique.ID)) +
          ggplot2::geom_raster(ggplot2::aes(fill=value),show.legend = FALSE, alpha =.5) +
          ggplot2:: geom_point(ggplot2::aes(size=duration.samples), alpha=.5, show.legend = FALSE, position = pd)

        # build the plot for rendering
        bar <- ggplot2::ggplot_build(g1)
        durData$x.fromRND <- NA
        durData$y.fromRND <- NA

        durData$x.fromRND[bar$data[[2]]$group] <- bar$data[[2]][['x']]
        durData$y.fromRND[bar$data[[2]]$group] <- bar$data[[2]][['y']]

        trajectories          <- durData
        trajectories$x.from   <- as.numeric(trajectories$x.from)
        trajectories$y.from   <- as.numeric(trajectories$x.from)

        trajectories$x.to     <- as.numeric(c(durData$x.from[seq_along(durData$x.from)[-1]],NA))
        trajectories$y.to     <- as.numeric(c(durData$y.from[seq_along(durData$y.from)[-1]],NA))
        trajectories$x.toRND  <- as.numeric(c(durData$x.fromRND[seq_along(durData$x.fromRND)[-1]],NA))
        trajectories$y.toRND  <- as.numeric(c(durData$y.fromRND[seq_along(durData$y.fromRND)[-1]],NA))
        trajectories          <- trajectories[1:(NROW(trajectories)-1),]
        trajectories$size     <- factor(cut(trajectories$duration.samples, 4, include.lowest = TRUE, ordered_result = TRUE, labels = FALSE, right = TRUE))

        # first <- trajectories[trajectories$trajectory.id==1,]
        # last  <- trajectories[(NROW(trajectories)),]

        first <- trajectories %>% filter(trajectories$trajectory.id==1)
        if(NROW(first)==1){
          last  <- first
        } else {
          last  <- trajectories[which(trajectories$trajectory.id==max(trajectories$trajectory.id, na.rm = TRUE)),]
        }
        if(identical(first,last)){
          last$x.from <- first$x.to
          last$y.from <- first$y.to
          last$x.fromRND <- first$x.toRND
          last$y.fromRND <- first$y.toRND
          last$x.to <- as.numeric(last$x.to)+.05
          last$y.to <- as.numeric(last$y.to)+.05
          last$x.toRND <- last$x.to
          last$y.toRND <- last$y.to
        }

        mark <- plyr::ldply(list(First=data.frame(first), Last = data.frame(last)))

        #mark$.id <- relevel(factor(mark$.id),ref=first[1])
        #mark$size <- mark$size*1.2

        COORD_v1 <- as.numeric(unique(sort(na.omit(durData$y1))))
        COORD_v2 <- as.numeric(unique(sort(na.omit(durData$y2))))

        LABS_v1 <- unlist(plyr::llply(COORD_v1, function(l) unique(na.omit(durData$y1_labs[durData$y1==l]))))
        LABS_v2 <- unlist(plyr::llply(COORD_v2, function(l) unique(na.omit(durData$y2_labs[durData$y2==l]))))

        Iv1 <- ts_trimfill(x=COORD_v1,y=as.numeric_discrete(LABS_v1),action = "trim.cut")
        Iv2 <- ts_trimfill(x=COORD_v2,y=as.numeric_discrete(LABS_v2),action = "trim.cut")

        COORD_v1 <- Iv1[[1]]
        LABS_v1 <-  names(Iv1[[2]])

        COORD_v2 <- Iv2[[1]]
        LABS_v2 <-  names(Iv2[[2]])

        flist <- c("0" = "grey80","1" = "grey50", "NA" = "white")
        clist <- c("Last" = "red3","First" = "steelblue4", "NA" = "orange")
        slist <- c("1"=6, "2" = 9, "3"= 12, "4" = 15)

        if(length(unique(trajectories$Trajectory:trajectories$Wave))>1){
          if(length(unique(na.omit(trajectories$Trajectory)))==1){
            Ncol <- c(0,1)
          } else {
            if(length(unique(na.omit(trajectories$Trajectory)))==2){
             Ncol <- c(0,.5,1)
            } else {
             Ncol <- seq(0,1,length.out = length(unique(na.omit(trajectories$Trajectory)))+1)
            }
            }
          facPal <- scales::col_factor(palette="Set1", domain=NULL)(Ncol)
          pal <- scales::gradient_n_pal(colours = facPal)(seq(0,1, length.out = (length(unique(na.omit(trajectories$Trajectory:trajectories$Wave)))+1)))
        } else {
          pal <- "#AAB2A9"
        }

        ssgr <- ggplot2::ggplot(durData, ggplot2::aes(x=y1,
                                                      y=y2,
                                                      group=y.name)) +
          ggplot2::geom_raster(ggplot2::aes(fill=value),
                               alpha =.5,
                               show.legend = FALSE) +
          ggplot2::geom_point(data = trajectories,
                              ggplot2::aes(x = x.fromRND,
                                           y = y.fromRND,
                                           size= size),
                              fill="steelblue",
                              colour="steelblue4",
                              alpha=.5,
                              pch = 21,
                              show.legend = FALSE) +
          ggplot2::geom_point(data = mark,
                              ggplot2::aes(x = x.fromRND,
                                           y = y.fromRND,
                                           size= size),
                              fill="steelblue1",
                              colour="steelblue4",
                              alpha=.8,
                              pch = 21,
                              show.legend = FALSE) +
          ggplot2:: geom_curve(data=trajectories, ggplot2::aes(x = x.fromRND,
                                                               y = y.fromRND,
                                                               xend = x.toRND,
                                                               yend = y.toRND,
                                                               colour = Trajectory:Wave),
                               lineend = "butt", curvature = 0.2, angle = 120, ncp=10,
                               arrow= grid::arrow(angle = 20, length = grid::unit(0.02, units = "npc"), type = "closed"),
                               alpha = .8, size=1, show.legend = FALSE,arrow.fill = NULL) +
          ggplot2::geom_curve(data = mark, ggplot2::aes(x = x.fromRND,
                                                        y = y.fromRND,
                                                        xend = x.toRND,
                                                        yend = y.toRND,
                                                        colour = Trajectory:Wave),
                              lineend = "butt",
                              curvature = 0.2,
                              angle = 120,
                              ncp=10,
                              arrow= grid::arrow(angle = 20, length = grid::unit(0.03, units = "npc"), type = "closed"),
                              alpha=1, size=1, arrow.fill = NULL) +
          ggplot2::guides(colour = "legend",size="none",fill="none") +
          ggplot2::scale_color_manual("Trajectory", values = pal, na.translate = FALSE) +
          ggplot2::scale_fill_manual(values = flist) +
          ggplot2::scale_size_manual(breaks = 1:4,values = slist) +
          ggplot2::scale_x_discrete(paste(self$options$y1),breaks = COORD_v1, labels = LABS_v1) +
          ggplot2::scale_y_discrete(paste(self$options$y2),breaks = COORD_v2, labels = LABS_v2) +
          ggplot2::coord_fixed(ratio = length(LABS_v2)/length(LABS_v1)) +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_line(colour = "grey70"),
                         axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

        plot(ssgr)
        TRUE
      }
    }
  )
)
