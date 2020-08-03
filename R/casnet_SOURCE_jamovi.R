# # from https://github.com/FredHasselman/casnet-jmvMAC
#
# # @import invctr
# # @import ggplot2
#
#
# # (C)RQA ------------------------
#
# # @name crqa_cl_main
# #
# # @title Command line crp
# #
# #
# # @param y1 y1
# # @param y2 y1
# # @param emDim y1
# # @param emLag y1
# # @param emRad y1
# # @param DLmin y1
# # @param VLmin y1
# # @param theiler y1
# # @param win y1
# # @param step y1
# # @param JRP y1
# # @param distNorm y1
# # @param returnMeasures y1
# # @param returnRPvector y1
# # @param returnLineDist y1
# # @param returnDistance y1
# # @param path_to_rp y1
# # @param saveOut y1
# # @param path_out y1
# # @param file_ID y1
# # @param silent y1
# # @param targetValue y1
# # @param ...
# #
# # @keywords internal
# #
# # @export
# #
# crqa_cl_main <- function(data,
#                          emDim  = 1,
#                          emLag  = 1,
#                          emRad  = NA,
#                          DLmin = 2,
#                          VLmin = 2,
#                          theiler = 0,
#                          win     = min(length(y1),ifelse(is.null(y2),(length(y1)+1), length(y2)), na.rm = TRUE),
#                          step    = step,
#                          JRP     = FALSE,
#                          distNorm       = c("EUCLIDEAN", "MAX", "MIN", "OP")[[1]],
#                          returnMeasures = TRUE,
#                          returnRPvector = FALSE,
#                          returnLineDist = FALSE,
#                          plot_recmat = c("noplot","rp","distmat")[[1]],
#                          path_to_rp = getOption("casnet.path_to_rp"),
#                          saveOut    = FALSE,
#                          path_out   = NULL,
#                          file_ID    = NULL,
#                          silent     = TRUE,
#                          targetValue  = .05,
#                          useParallel = FALSE,
#                          ...){
#
#
#   if(useParallel){
#     #data <- unnest(data)
#     returnRPvector = FALSE
#     returnLineDist = FALSE
#   }
#   y1     <- data[,1]
#   y2     <- data[,2]
#
#   fixedRR <- FALSE
#   if(is.na(emRad)){fixedRR=TRUE}
#
#   file_ID <- file_ID%00%0
#
#   RQAmeasures <- list(
#     RR      = 'Recurrence rate',
#     DET     = 'Determinism',
#     DET_RR  = 'Ratio DET/RR',
#     LAM     = 'Laminarity',
#     LAM_DET = 'Ratio LAM/DET',
#     L_max   = 'maximal diagonal line length',
#     L_mean  = 'mean diagonal line length',
#     L_entr  = 'Entropy of diagonal line length distribution',
#     DIV     = 'Divergence (1/L_max)',
#     V_max   = 'maximal vertical line length',
#     TT      = 'Trapping time',
#     V_entr  = 'Entropy of vertical line length distribution',
#     T1      = 'Recurrence times 1st type',
#     T2      = 'Recurrence times 2nd type',
#     W_max   = 'Max interval',
#     W_mean  = 'Mean of interval',
#     W_entr  = 'Entropy interval distribution',
#     W_prob  = 'Prob.of intervals',
#     F_min   = 'F min'
#   )
#
#   if(plot_recmat%in%"distmat"){-1 * emRad}
#
#   tmpd  <- tempdir()
#   tmpf1 <- tempfile(tmpdir = tmpd, fileext = ".dat")
#   utils::write.table(as.data.frame(y1), tmpf1, col.names = FALSE, row.names = FALSE, sep = "\t")
#
#   fileSep <- ifelse(any(path_out%in%"/"),"/","\\")
#
#   plotOUT     <- normalizePath(file.path(path_out,paste0("RQAplot_",     file_ID, ".txt"),fsep = fileSep), mustWork = FALSE)
#   measOUT     <- normalizePath(file.path(path_out,paste0("RQAmeasures_", file_ID, ".txt"),fsep = fileSep), mustWork = FALSE)
#   histOUTdiag <- normalizePath(file.path(path_out,paste0("RQAhist_diag_",file_ID, ".txt"),fsep = fileSep), mustWork = FALSE)
#   histOUThori <- normalizePath(file.path(path_out,paste0("RQAhist_hori_",file_ID, ".txt"),fsep = fileSep), mustWork = FALSE)
#
#   if(any(is.null(y2))|any(is.na(y2%00%NA))){
#
#     if(is.na(emRad)){
#       if(!is.na(targetValue)){
#         emRad <- crqa_radius(y1 = y1, emDim = emDim, emLag = emLag, targetValue = targetValue, radiusOnFail = "percentile", tol = .2, silent = silent)
#         if(emRad$Converged){
#           emRad <- emRad$Radius
#         } else {
#           emRad <-  emRad <- stats::sd(c(y1,y2),na.rm = T)
#         }
#       }
#     }
#     opts <- paste("-i", shQuote(tmpf1),
#                   "-r", shQuote(plotOUT),
#                   "-o", shQuote(measOUT),
#                   "-p", shQuote(histOUTdiag),
#                   "-q", shQuote(histOUThori),
#                   "-m", emDim,
#                   "-t", emLag,
#                   "-e", emRad,
#                   "-l", DLmin,
#                   "-v", VLmin,
#                   "-w", theiler,
#                   "-n", shQuote(distNorm),
#                   ifelse(silent,"-s",""))
#   } else {
#     if(is.na(emRad)){
#       if(!is.na(targetValue)){
#         emRad <- crqa_radius(y1 = y1, y2 = y2, emDim = emDim, emLag = emLag, targetValue = targetValue, tol = .2, radiusOnFail = "percentile", silent = silent)
#         if(emRad$Converged){
#           emRad <- emRad$Radius
#         } else {
#           emRad <- stats::sd(c(y1,y2),na.rm = T)
#         }
#       }
#     }
#     tmpf2 <- tempfile(tmpdir = tmpd, fileext = ".dat")
#     utils::write.table(as.data.frame(y2), tmpf2, col.names = FALSE, row.names = FALSE, sep = "\t")
#     opts <- paste("-i", shQuote(tmpf1),
#                   "-j", shQuote(tmpf2),
#                   "-r", shQuote(plotOUT),
#                   "-o", shQuote(measOUT),
#                   "-p", shQuote(histOUTdiag),
#                   "-q", shQuote(histOUThori),
#                   "-m", emDim,
#                   "-t", emLag,
#                   "-e", emRad,
#                   "-l", DLmin,
#                   "-v", VLmin,
#                   "-w", theiler,
#                   "-n", shQuote(distNorm),
#                   ifelse(silent,"-s",""))
#   }
#
#   #closeAllConnections()
#
#   ## RCMD
#   callr::rcmd(cmd = paste0(getOption("casnet.rp_prefix"),getOption("casnet.rp_command")), cmdargs = opts, wd = normalizePath(path.expand(path_to_rp), mustWork = FALSE), show = silent)
#
#   measures     <- try_CATCH(utils::read.delim(normalizePath(gsub("[']+","",measOUT)),header=TRUE))
#   rpMAT        <- try_CATCH(utils::read.delim(normalizePath(gsub("[']+","",plotOUT)),header=TRUE))
#   disthistDiag <- try_CATCH(utils::read.delim(normalizePath(gsub("[']+","",histOUTdiag)), header=FALSE, sep = " "))
#   disthistHori <- try_CATCH(utils::read.delim(normalizePath(gsub("[']+","",histOUThori)), header=FALSE, sep = " "))
#
#   if(all(is.null(measures$warning),is.data.frame(measures$value))){
#     measures <- measures$value
#   } else {
#     measures <- rbind.data.frame(rep(NA,length(RQAmeasures)))
#   }
#   colnames(measures) <- gsub("(#RR)|(X.RR)","RR",colnames(measures))
#   measures <- cbind.data.frame(measures, emDim=emDim, emLag=emLag, emRad=emRad, DLmin=DLmin, VLmin=VLmin, distNorm =distNorm)
#
#   if(all(is.null(rpMAT$warning),is.data.frame(rpMAT$value))){
#     rpMAT <- rpMAT$value
#   } else {
#     rpMAT <- data.frame(y1=NA,y2=NA,dist=NA)
#   }
#
#   if(any(is.null(disthistDiag$warning),is.data.frame(grepl("Error",paste(disthistDiag$value))))){
#     disthistDiag <- disthistDiag$value
#   } else {
#     disthistDiag <- data.frame(line.length=NA,freq=NA)
#   }
#
#   if(any(is.null(disthistHori$warning),is.data.frame(grepl("Error",paste(disthistHori$value))))){
#     disthistHori <- disthistHori$value
#   } else {
#     disthistHori <- data.frame(line.length=NA,freq=NA)
#   }
#
#   if(!silent){cat(paste0("[ID ",file_ID,"] Analysis completed... "))}
#   if(!saveOut){
#     file.remove(measOUT)
#     file.remove(plotOUT)
#     file.remove(histOUTdiag)
#     file.remove(histOUThori)
#     if(!silent){cat("temporary files removed ...\n")}
#   } else {
#     if(!silent){
#       cat("files saved ...\n")
#       cat(measOUT,"\n",plotOUT,"\n",histOUTdiag,"\n",histOUThori,"\n")
#     }
#   }
#
#   if(useParallel){
#     return(tibble::as_tibble(measures))
#   } else{
#     return(list(measures = measures,
#                 rpMAT    = rpMAT,
#                 diag_disthist = disthistDiag,
#                 hori_disthist = disthistHori))
#   }
# }
#
#
# # Fast (C)RQA (command line crp)
# #
# # This function will run the \href{http://tocsy.pik-potsdam.de/commandline-rp.php}{commandline Recurrence Plots} executable provided by Norbert Marwan.
# #
# # @param y1 Time series 1
# # @param y2 Time series 2 for Cross Recurrence Analysis (default = \code{NULL})
# # @param emDim Embedding dimensions (default = \code{1})
# # @param emLag Embedding lag (default = \code{1})
# # @param emRad Radius on distance matrix (default = \code{1})
# # @param DLmin Minimum length of diagonal structure to be considered a line (default = \code{2})
# # @param VLmin Minimum length of vertical structure to be considered a line (default = \code{2})
# # @param theiler Theiler window (default = \code{0})
# # @param win Window to calculate the (C)RQA (default = minimum of length of \code{y1} or \code{y2})
# # @param step Stepsize for sliding windows (default = size of \code{win}, so no sliding window)
# # @param JRP Wether to calculate a Joint Recurrence Plot (default = \code{FALSE})
# # @param distNorm One of "EUCLIDEAN" (default), \code{"MAX", "MIN"}, or \code{"OP"} for an Order Pattern recurrence matrix
# # @param standardise Standardise data: \code{"none"} (default), \code{"mean.sd"}, or \code{"median.mad"}
# # @param returnMeasures Return the (C)RQA measures? (default = \code{TRUE})
# # @param returnRPvector Return the recurrent points in a dataframe? (default = \code{FALSE})
# # @param returnLineDist Return the distribution of diagonal and horizontal line length distances (default = \code{FALSE})
# # @param plot_recmat Produce a plot of the recurrence matrix by calling \code{\link{rp_plot}}, values can be \code{"rp"} (the thresholded recurrence matrix),\code{"distmat"} (the unthresholded recurrence matrix) or \code{"noplot"} (default = \code{"noplot"})
# # @param path_to_rp Path to the command line executable (default = path set during installation, use \code{getOption("casnet.path_to_rp")} to see)
# # @param saveOut Save the output to files? If \code{TRUE} and \code{path_out = NA}, the current working directory will be used (default = \code{FALSE})
# # @param path_out Path to save output if \code{saveOut = TRUE} (default = \code{NULL})
# # @param file_ID A file ID which will be a prefix to to the filename if \code{saveOut = TRUE} (default = \code{NULL}, an integer will be added tot the file name to ensure unique files)
# # @param silent Do not display any messages (default = \code{TRUE})
# # @param surrogateTest Perform surrogate tests. If \code{TRUE}, will run surrogate tests using default settings for a two-sided test of \eqn{H_0: The data generating process is a rescaled linear Gaussian process} at \eqn{\alpha = .05} (arguments \code{ns = 39, fft = TRUE, amplitude = TRUE})
# # @param targetValue A value passed to \code{crqa_radius(...,type="fixed", targetMeasure="RR")} if \code{is.na(emRad)==TRUE}. This is useful for windowed analysis, it will estimate a new radius for each window.
# # @param useParallel Speed up calculations by using the parallel processing options provided by `parallel` to assign a seperate process/core for each window in windowed (C)RQA analysis using \code{\link[purrr]{map2}} to assign data and \code{\link[parallel]{detectCores}} with  \code{logical = TRUE} to decide on the available cores (default = \code{FALSE})
# # @param ... Additional parameters (currently not used)
# #
# # @details The \code{rp} executable is installed when the function is called for the first time and is renamed to \code{rp}, from a platform specific filename downloaded from \url{http://tocsy.pik-potsdam.de/commandline-rp.php} or extracted from an archive located in the directory: \code{...\\casnet\\commandline_rp\\}.
# # The file is copied to the directory: \code{...\\casnet\\exec\\}
# # The latter location is stored as an option and can be read by calling \code{getOption("casnet.path_to_rp")}.
# #
# # @section Troubleshooting:
# # Some notes on resolving errors with \code{rp}.The script will first try to download the correct executable, if that fails it will try to extract the file from a .zip archive in \code{...\\casnet\\commandline_rp\\crp_cl.zip}. If that fails, the copy will have failed. It should be relatively easy to get \code{crqa_cl()} working using custom settings:
# #
# # \itemize{
# # \item \emph{Copy failed} - Every time the function \code{crqa_cl()} is called it will check whether a log file \code{rp_instal_log.txt} is present in the \code{...\\casnet\\exec\\} directory. If you delete the log file, and call the function, another copy of the executable will be attempted.
# # \item \emph{Copy still fails and/or no permission to copy} - You can copy the approrpiate executable to any directory you have access to, be sure to rename it to \code{rp} (\code{rp.exe} on Windows OS). Then, either pass the path to \code{rp} as the argument \code{path_to_rp} in the \code{crqa_cl} function call, or, as a more permanent solution, set the \code{path_to_rp} option by calling \code{options(casnet.path_to_rp="YOUR_PATH")}. If you cannot acces the directory \code{...\\casnet\\commandline_rp\\}, download the appropriate executable from the \href{http://tocsy.pik-potsdam.de/commandline-rp.php}{commandline Recurrence Plots} page and copy to a directory you have access to. Then follow the instruction to set \code{path_to_rp}.
# # \item \emph{Error in execution of \code{rp}} - This can have a variety of causes, the \code{rp} executable is called using \code{\link[callr]{rcmd}} and makes use of the \code{\link{normalizePath}} function with argument \code{mustWork = FALSE}. Problems caused by specific OS, machine, or, locale problems (e.g. the \code{winslash} can be reported as an \href{https://github.com/FredHasselman/casnet/issues}{issue on Github}). One execution error that occurs when the OS is not recognised properly can be resolved by chekcing \code{getOption("casnet.rp_prefix")}. On Windows OS this should return an empty character vector, on Linux or macOS it should return \code{"./"}. You can manually set the correct prefix by calling \code{options(casnet.rp_prefix="CORRECT OS PREFIX")} and fill in the prefix that is correct for your OS
# # }
# #
# # @return A list object containing 1-3 elements, depending on arguments requesting output.
# #
# # \enumerate{
# # \item \code{rqa_measures} - A list of the (C)RQA measures returned if \code{returnMeasures = TRUE}:
# # \itemize{
# #  \item RR = 'Recurrence rate'
# #  \item DET = 'Determinism'
# #  \item DET_RR = 'Ratio DET/RR'
# #  \item LAM = 'Laminarity'
# #  \item LAM_DET = 'Ratio LAM/DET'
# #  \item L_max = 'maximal diagonal line length'
# #  \item L_mean = 'mean diagonal line length'
# #  \item L_entr = 'Entropy of diagonal line length distribution'
# #  \item DIV =  'Divergence (1/L_max)'
# #  \item V_max = 'maximal vertical line length'
# #  \item TT = 'Trapping time'
# #  \item V_entr = 'Entropy of vertical line length distribution'
# #  \item T1 = 'Recurrence times 1st type'
# #  \item T2 = 'Recurrence times 2nd type'
# #  \item W_max = 'Max interval length'
# #  \item W_mean = 'Mean of interval lengths'
# #  \item W_entr = 'Entropy of interval length distribution'
# #  \item W_prob = 'Probability of interval'
# #  \item F_min = 'F min'
# # }
# # \item \code{rqa_rpvector} - The radius thresholded distance matrix (recurrence matrix), which can be visualised as a recurrence plot by calling \code{\link{rp_plot}}. If a sliding window analysis is conducted this will be a list of matrices and could potentially grow too large to handle. It is recommended you save the output to disk by setting \code{saveOut = TRUE}.
# # \item \code{rqa_diagdist} - The distribution of diagonal line lengths
# # }
# #
# # @note The platform specific \code{rp} command line executables were created by Norbert Marwan and obtained under a Creative Commons License from the website of the Potsdam Institute for Climate Impact Research at \url{http://tocsy.pik-potsdam.de/}.
# #
# # The full copyright statement on the website is as follows:
# #
# # © 2004-2017 SOME RIGHTS RESERVED
# #
# # University of Potsdam, Interdisciplinary Center for Dynamics of Complex Systems, Germany
# #
# # Potsdam Institute for Climate Impact Research, Transdisciplinary Concepts and Methods, Germany
# #
# # This work is licensed under a \href{https://creativecommons.org/licenses/by-nc-nd/2.0/de/}{Creative Commons Attribution-NonCommercial-NoDerivs 2.0 Germany License}.
# #
# # More information about recurrence analysis can be found on the \href{http://www.recurrence-plot.tk}{Recurrence Plot} website.
# #
# # @family Recurrence Quantification Analysis
#
# # @export
# #
# crqa_cl <- function(y1,
#                     y2    = NULL,
#                     emDim  = 1,
#                     emLag  = 1,
#                     emRad  = NA,
#                     DLmin =  2,
#                     VLmin =  2,
#                     theiler = 0,
#                     win     = min(length(y1),ifelse(is.null(y2),(length(y1)+1), length(y2)), na.rm = TRUE),
#                     step    = win,
#                     JRP     = FALSE,
#                     distNorm       = c("EUCLIDEAN", "MAX", "MIN", "OP")[[1]],
#                     standardise   = c("none","mean.sd","median.mad")[1],
#                     returnMeasures = TRUE,
#                     returnRPvector = FALSE,
#                     returnLineDist = FALSE,
#                     plot_recmat = c("noplot","rp","distmat")[[1]],
#                     path_to_rp = getOption("casnet.path_to_rp"),
#                     saveOut    = FALSE,
#                     path_out   = NULL,
#                     file_ID    = NULL,
#                     silent     = TRUE,
#                     surrogateTest = FALSE,
#                     targetValue  = .05,
#                     useParallel   = FALSE,
#                     ...){
#
#   if(!file.exists(normalizePath(file.path(getOption("casnet.path_to_rp"),"/rp_install_log.txt"), mustWork = FALSE))){
#     set_command_line_rp()
#   }
#
#   cat("\n~~~o~~o~~casnet~~o~~o~~~\n")
#
#   dotArgs <- list(...)
#   if(is.na(dotArgs%00%NA)){dotArgs<-list(nothing=NA)}
#
#   if(!is.null(y2)){
#     y2 <- zoo::as.zoo(y2)
#     N1 <- NROW(y1)
#     N2 <- NROW(y2)
#     if(N1>N2){
#       y2[N2:(N1+(N1-N2))] <- 0
#     }
#     if(N1<N2){
#       y1[N1:(N2+(N2-N1))] <- 0
#     }
#     df <- cbind.data.frame(y1=unclass(y1),y2=unclass(y2))
#     df <- df[stats::complete.cases(df),]
#   } else {
#     if(any(is.na(y1))){
#       y1 <- y1[!is.na(y1)]
#       if(!silent){message("Removed NAs from timeseries y1 before (C)RQA")}
#     }
#     df <- cbind.data.frame(y1=unclass(y1),y2=NA)
#   }
#
#   # Begin input checks
#   if(is.null(path_out)){
#     if(saveOut){
#       path_out <- getwd()
#     } else {
#       path_out <- tempdir()
#     }
#   } else {
#     path_out <- normalizePath(path_out, mustWork = TRUE)
#   }
#
#   if(is.null(y2)){
#     cat("\nPerforming auto-RQA\n")
#     if(theiler<1){theiler=1}
#   } else {
#     cat("\nPerforming cross-RQA\n")
#   }
#
#   if(surrogateTest){
#     surrogateSeries <- tseries::surrogate(y1,ns=39,fft = TRUE, amplitude = TRUE)
#   }
#
#   windowedAnalysis <- FALSE
#   if(win==NROW(df)|step==(NROW(df))){
#     win <- step <- NROW(df)
#     wIndex <- seq(1,NROW(df))
#     if(is.na(emRad)){cat(paste("Radius will be calulated to yield targetValue =",targetValue,"RR...\n"))}
#   } else {
#     windowedAnalysis = TRUE
#     wIndex <- seq(1,NROW(df)-win, by = step)
#     cat(paste("\nCalculating recurrence measures in a window of size",win,"taking steps of",step,"data points. \n"))
#     if(is.na(emRad)){cat(paste("Radius will be re-calculated to yield targetValue =",targetValue,"RR for each window...\n\n"))}
#   }
#
#
#   if(windowedAnalysis){
#
#     # Adjust time series lengths
#     if((dplyr::last(wIndex)+win-NROW(df))>0){
#
#       yy1 <- ts_trimfill(x=seq(1,dplyr::last(wIndex)+win),y=df[,1])
#       yy2 <- rep(NA,length(yy1))
#       if(!all(is.na(df[,2]))){
#         yy2 <- ts_trimfill(x=seq(1,dplyr::last(wIndex)+win),y=df[,2])
#       }
#       df <- cbind.data.frame(y1=yy1,y2=yy2)
#       if(!silent){message("\nAdded 0s to vector to make window and stepsize combination fit to time series length\n\n ")}
#     } else {
#       if(!silent){message("\nWindow and stepsize combination do not fit to the full length of the time series!\n\n ")}
#     }
#
#     if(silent){cat("\nsst! [it's a silent-ish mode]\n\n")}
#
#     wIndices <- plyr::llply(wIndex, function(w){seq(w,w+(win))})
#     names(wIndices) <- paste0("window: ",seq_along(wIndices)," | start: ",wIndex," | stop: ",wIndex+win)
#
#   } else {
#
#     wIndices <- list(wIndex)
#     names(wIndices) <- paste0("window: 1 | start: ",wIndex[1]," | stop: ",wIndex[NROW(df)])
#
#   } # If windowed
#
#   #cl <- parallel::makeCluster(mc.cores)
#   #parallel::clusterExport(cl = cl, c("crqa_cl_main","wIndices","df","y1","y2","emDim","emLag","emRad","DLmin","VLmin","theiler", "win","step","JRP","distNorm","returnMeasures","returnRPvector","returnLineDist","plot_recmat","path_to_rp","saveOut","path_out","file_ID","silent","..."))
#
#   # Create the data, for all windows,need this for parallel, but also speeds up processing in general.
#   dfList <- plyr::llply(wIndices, function(ind){cbind(y1 = df[ind,1],y2 = df[ind,2])})
#
#   if(useParallel){
#
#     if(names(dotArgs)%in%"logical"){logical <- logical} else {logical <- TRUE}
#
#     cat("\n...using parallel processing...\n")
#
#     # Only return measures
#     returnRPvector <- FALSE
#     returnLineDist <- FALSE
#
#     # How many cores is optimal?
#     cores_available <- parallel::detectCores(logical = logical)-1
#     if(cores_available>1){
#       if(length(wIndices)%%2==0){odd_windows <- FALSE} else {odd_windows <- TRUE}
#       if(cores_available%%2==0){odd_cores <- FALSE} else {odd_cores <- TRUE}
#       if(odd_windows&!odd_cores){cores_available<-cores_available-1}
#       if(!odd_windows&odd_cores){cores_available<-cores_available-1}
#     } else {
#       cores_available <- 1
#     }
#
#     cl       <- parallel::makeCluster(cores_available)
#
#     parallel::clusterEvalQ(cl, library(callr))
#     parallel::clusterEvalQ(cl,library(utils))
#     parallel::clusterEvalQ(cl,library(plyr))
#     parallel::clusterEvalQ(cl,library(tidyverse))
#     parallel::clusterEvalQ(cl,library(pROC))
#     parallel::clusterEvalQ(cl,library(Matrix))
#     parallel::clusterEvalQ(cl,library(casnet))
#
#     # parallel::clusterExport(cl, varlist = c("data","emDim","emLag","emRad","DLmin","VLmin","theiler","win","step","JRP","distNorm","returnMeasures","returnRPvector","returnLineDist","plot_recmat","path_to_rp", "saveOut","path_out","file_ID","silent","targetValue", "useParallel"))
#
#     # cluster_library(c("devtools","utils","plyr","dplyr","tidyr","Matrix","pROC")) %>%
#     # cluster_assign_value("crqa_cl_main", crqa_cl_main) %>%
#     # cluster_assign_value("crqa_radius", crqa_radius) %>%
#     # cluster_assign_value("emDim", emDim)  %>%
#     # cluster_assign_value("emLag", emLag)  %>%
#     # cluster_assign_value("emRad", emRad)  %>%
#     # cluster_assign_value("DLmin", DLmin)  %>%
#     # cluster_assign_value("VLmin", VLmin)  %>%
#     # cluster_assign_value("theiler", theiler)  %>%
#     # cluster_assign_value("JRP", JRP)  %>%
#     # cluster_assign_value("distNorm", distNorm)  %>%
#     # cluster_assign_value("returnMeasures", returnMeasures)  %>%
#     # cluster_assign_value("returnRPvector", returnRPvector)  %>%
#     # cluster_assign_value("returnLineDist", returnLineDist)  %>%
#     # cluster_assign_value("plot_recmat", plot_recmat)  %>%
#     # cluster_assign_value("path_to_rp", path_to_rp)  %>%
#     # cluster_assign_value("saveOut", saveOut)  %>%
#     # cluster_assign_value("path_out", path_out)  %>%
#     # cluster_assign_value("file_ID", file_ID)  %>%
#     # cluster_assign_value("silent", silent)  %>%
#     # cluster_assign_value("targetValue", targetValue) %>%
#     # cluster_assign_value("useParallel", useParallel)
#
#     #  parallel::clusterExport(cl = cl, c("crqa_cl_main","wIndices","df","y1","y2","emDim","emLag","emRad","DLmin","VLmin","theiler", "win","step","JRP","distNorm","returnMeasures","returnRPvector","returnLineDist","plot_recmat","path_to_rp","saveOut","path_out","file_ID","silent","..."))
#
#
#     start <- proc.time()
#     wList <- parallel::parLapply(cl,dfList,function(df){crqa_cl_main(data = df,
#                                                                      emDim          = emDim,
#                                                                      emLag          = emLag,
#                                                                      emRad          = emRad,
#                                                                      DLmin          = DLmin,
#                                                                      VLmin          = VLmin,
#                                                                      theiler        = theiler,
#                                                                      JRP            = JRP,
#                                                                      distNorm       = distNorm,
#                                                                      returnMeasures = returnMeasures,
#                                                                      returnRPvector = returnRPvector,
#                                                                      returnLineDist = returnLineDist,
#                                                                      plot_recmat    = plot_recmat,
#                                                                      path_to_rp     = path_to_rp,
#                                                                      saveOut        = saveOut,
#                                                                      path_out       = path_out,
#                                                                      file_ID        = file_ID,
#                                                                      silent         = silent,
#                                                                      targetValue    = targetValue,
#                                                                      useParallel    = useParallel)})
#
#     parallel::stopCluster(cl)
#     time_elapsed_parallel <- proc.time() - start # End clock
#
#     cat("\nCompleted in:\n")
#     print(time_elapsed_parallel)
#
#   } else {
#
#     cat("\n...using sequential processing...\n")
#
#     start <- proc.time()
#
#     wList <- plyr::llply(dfList, function(data){
#       crqa_cl_main(
#         data           = data,
#         emDim          = emDim,
#         emLag          = emLag,
#         emRad          = emRad,
#         DLmin          = DLmin,
#         VLmin          = VLmin,
#         theiler        = theiler,
#         JRP            = JRP,
#         distNorm       = distNorm,
#         returnMeasures = returnMeasures,
#         returnRPvector = returnRPvector,
#         returnLineDist = returnLineDist,
#         plot_recmat    = plot_recmat,
#         path_to_rp     = path_to_rp,
#         saveOut        = saveOut,
#         path_out       = path_out,
#         file_ID        = file_ID,
#         silent         = silent,
#         targetValue    = targetValue)},
#       .progress = plyr::progress_text(char = "o~o"))
#
#     time_elapsed_parallel <- proc.time() - start # End clock
#
#     cat(paste("\nCompleted in:\n"))
#     print(time_elapsed_parallel)
#
#
#   } # useParallel
#   rm(dfList)
#
#   rqa_measures <- rqa_rpvector <- rqa_diagdist <- rqa_horidist <- NA
#   if(useParallel){
#     if(is.list(wList)){
#       rqa_measures <-  plyr::ldply(wList)
#     } else {
#       rqa_measures <-  tidyr::unnest(wList)
#     }
#   } else {
#     rqa_measures <-plyr::ldply(wList, function(l) l$measures) # %>% dplyr::mutate(win = win, step = step, index = attr(wlist, "index"))
#     rqa_rpvector <-plyr::ldply(wList, function(l) l$rpMAT) # %>% dplyr::mutate(win = win, step = step, index = attr(wlist, "index"))
#     rqa_diagdist <-plyr::ldply(wList, function(l) l$diag_disthist) # %>% dplyr::mutate(win = win, step = step, index = attr(wlist, "index"))
#     rqa_horidist <-plyr::ldply(wList, function(l) l$hori_disthist) # %>% dplyr::mutate(win = win, step = step, index = attr(wlist, "index"))
#   }
#
#   doPlot <- which(plot_recmat%in%c("noplot","rp","distmat"))
#
#   if(doPlot>1){
#     if(doPlot==2){
#       plotList <- plyr::llply(wIndices, function(ind) rp_plot(di2bi(rp(df[ind,1],df[ind,2], emDim = emDim, emLag = emLag),emRad = emRad)))
#     }
#     if(doPlot==3){plotList <- plyr::llply(wIndices, function(ind) rp_plot(rp(df[ind,1],df[ind,2], emDim = emDim, emLag = emLag)))}
#     multi_PLOT(plotList)
#   }
#
#   justData <- FALSE
#   if(win!=NROW(df)){
#     if(!silent){message("Windowed (c)rqa: Returning only (c)rqa measures, use saveOut = TRUE, to get the distribution files")}
#     justData <- TRUE
#   }
#
#   if(returnMeasures&!returnRPvector&!returnLineDist){
#     justData<- TRUE
#   }
#
#   out <- list(rqa_measures = rqa_measures,
#               rqa_rpvector = rqa_rpvector,
#               rqa_diagdist = rqa_diagdist,
#               rqa_horidist = rqa_horidist)
#   if(saveOut){saveRDS(out,paste0(path_out,"CRQA_out",file_ID,".rds"))}
#
#   cat("\n~~~o~~o~~casnet~~o~~o~~~\n")
#
#   if(justData){
#     return(rqa_measures)
#   } else {
#     return(out[c(returnMeasures,returnRPvector,returnLineDist,returnLineDist)])
#   }
# }
#
#
# # Find optimal (C)RQA parameters
# #
# # A wrapper for various algorithms used to find optimal embedding delay, number of embedding dimensions and radius.
# #
# # @param y A numeric vector or time series
# # @param maxDim Maximum number of embedding dimensions (default = \code{10})
# # @param maxLag Maximum embedding lag to consider. Defaults to \code{floor(length(y)/(maxDim+1))}.
# # @param emLag Optimal embedding lag (delay), e.g., provided by optimising algorithm. Leave empty to estimate (default = \code{NULL})
# # @param lagMethods A range of embedding lags to consider when calling \code{\link[nonlinearTseries]{timeLag}} with \code{technique="ami"}, valid options are c("first.e.decay", "first.zero", "first.minimum")
# # @param nnSizes  Points whose distance is \code{nnSize} times further apart than the estimated size of the attractor will be declared false neighbours. See the argument \code{atol} in \code{\link[fractal]{FNN}} (default = \code{c(2,5)})
# # @param nnRadius If the ratio of the distance between two points in successive dimensions is larger than \code{nnRadius}, the points are declared false neighbours. See the argument \code{rtol} in \code{\link[fractal]{FNN}} (default = \code{c(5,10)})
# # @param nnThres  Threshold for selecting optimal parameter in percentage points (default = \code{10})
# # @param theiler Theiler window on distance matrix (default = \code{0})
# # @param diagPlot Plot the results
# # @param estimateDimensions Get optimal embedding dimension
# # @param estimateLags Get optimal embedding lags
# # @param silent Silent-ish mode
# # @param ... Other parameters passed to \code{\link[nonlinearTseries]{timeLag}}
# #
# # @return A list object containing the optimal values and iteration history.
# #
# # @details A number of functions are called to determie optimal parameters for delay embedding a time series:
# #
# # \itemize{
# # \item{Embedding lag (\eqn{\tau}, \code{emLag}): The default is to call \code{\link[casnet]{est_emLag}}, which is a wrapper around \code{\link[nonlinearTseries]{timeLag}} with \code{technique="ami"} to get lags based on the mutual information function.}
# # \item{Embedding dimension (\code{m}, \code{emDim}): The default is to call \code{\link[casnet]{est_emDim}}, which is a wrapper around \code{\link[fractal]{FNN}}}
# # }
# #
# # @family Recurrence Quantification Analysis
# #
# # @export
# #
# crqa_parameters <- function(y,
#                             maxDim   = 10,
#                             maxLag   = floor(length(y)/(maxDim+1)),
#                             lagMethods = "first.minimum",
#                             emLag     = NULL,
#                             nnSizes  = c(2,5,10,15),
#                             nnRadius = 5,
#                             nnThres  = 10,
#                             theiler  = 0,
#                             estimateLags = TRUE,
#                             estimateDimensions = TRUE,
#                             diagPlot = TRUE,
#                             silent   = TRUE,
#                             ...){
#
#   if(!is.null(dim(y))){stop("y must be a 1D numeric vector!")}
#
#   if(length(nnRadius)!=1){stop("nnRadius must have 1 numeric value")}
#   if(length(nnSizes)!=4){stop("nnSizes must have 4 numeric values")}
#   y <- y[!is.na(y)]
#   #y <- ts_standardise(y, adjustN = FALSE)
#
#   emDims  <-  1:maxDim
#
#   if(is.null(emLag)){
#     if(estimateDimensions){
#       mi <- mif(data.frame(y),lags = 1:maxLag) #est_emLag(y,selection.methods = lagMethods, maxLag = maxLag)
#       emLag$first.minimum <- which(ts_symbolic(data.frame(mi))[,2]%in%"trough")[1]
#       emLag$global.minimum <- min(mi,na.rm = TRUE)
#       emLag$max.lag <- maxLag
#     } else {
#       emLags <- cbind.data.frame(selection.methods = "Not estimated", optimal.lag = NA)
#     }
#   } else {
#     emLags <- cbind.data.frame(selection.methods = "User", optimal.lag = emLag)
#   }
#
#   # (fn.out <- tseriesChaos::false.nearest(lx, m=10, d=17, t=0, eps=sd(lx)/10, rt=20))
#   # plot(fn.out[1,],type="b")
#
#   if(estimateDimensions){
#     lagList <- list()
#     cnt = 0
#     for(N in seq_along(nnSizes)){
#       for(R in seq_along(nnRadius)){
#         for(L in seq_along(emLags$selection.method)){
#
#           if(!is.na(emLags$optimal.lag[L])){
#             cnt = cnt+1
#
#             Nn.max <- Nn.mean <- Nn.sd <- Nn.min <- numeric(maxDim)
#             for(D in seq_along(emDims)){
#               RM <- rp(y,y, emDim = emDims[[D]], emLag = emLags$optimal.lag[L])
#               RM <- bandReplace(RM,-theiler,theiler,0,silent = silent)
#               Nn.min[D] <- min(RM, na.rm = TRUE)
#               Nn.max[D] <- max(RM, na.rm = TRUE)
#               Nn.sd[D]   <- stats::sd(RM, na.rm = TRUE)
#               Nn.mean[D] <- mean(RM, na.rm = TRUE)
#               rm(RM)
#             }
#
#             #surrDims <- nonlinearTseries::buildTakens(time.series =  as.numeric(y), embedding.dim =  emDims[[D]], time.lag = emLags$optimal.lag[L])
#
#             # (fn.out <- false.nearest(rnorm(1024), m=6, d=1, t=1, rt=3))
#             # plot(fn.out)
#
#             fnnSeries <- fractal::FNN(x = as.numeric(y),
#                                       dimension = maxDim,
#                                       tlag = emLags$optimal.lag[L],
#                                       rtol = nnRadius[R],
#                                       atol = nnSizes[N],
#                                       olag = 1
#             )
#           }
#
#           #allN <- nonlinearTseries::findAllNeighbours(surrDims, radius = nnSizes[N]*sd(y))
#           #Nn <- sum(plyr::laply(allN, length), na.rm = TRUE)
#           #   if(D==1){Nn.max <- Nn}
#           lagList[[cnt]] <- data.frame(Nn.pct = as.numeric(fnnSeries[1,]),
#                                        Nsize = nnSizes[N],
#                                        Nradius = nnRadius[R],
#                                        emLag.method = emLags$selection.method[[L]],
#                                        emLag = emLags$optimal.lag[L],
#                                        emDim = emDims,
#                                        Nn.mean = Nn.mean,
#                                        Nn.sd  = Nn.sd,
#                                        Nn.min = Nn.min,
#                                        Nn.max = Nn.max)
#         } # L
#       } # R
#     } # N
#
#
#     df        <- plyr::ldply(lagList)
#     # df$Nn.pct <- df$Nn/df$Nn.max
#
#     opt <-plyr::ldply(unique(df$emLag), function(n){
#       id <- which((df$Nn.pct<=nnThres)&(df$emLag==n)&(!(df$emLag.method%in%"maximum.lag")))
#       if(length(id)>0){
#         idmin <- id[df$emDim[id]==min(df$emDim[id], na.rm = TRUE)]
#         if(length(idmin)>0){
#           return(df[idmin,])
#         }
#       } else {
#         return(df[which.min(df$Nn.pct[(df$emLag==n)&(!(df$emLag.method%in%"maximum.lag"))]),])
#       }
#     }
#     )
#
#     opt <- opt[opt$emDim==min(opt$emDim),][1,]
#
#     opDim <- opt$emDim
#
#   } else { # if estimateDim
#     opDim <- NA
#   }
#
#
#   #opDim <- min(df$emDim[df$Nn.pct<nnThres], na.rm = TRUE)
#   # opLag <- tau(y,
#   #              selection.methods = ami.method,
#   #              maxLag =maxLag)$opLag[1]
#   opLag <- df$emLag[which.min(min(df$emDim[df$Nn.pct<nnThres], na.rm = TRUE))]
#   #opRad = NULL
#
#   df$emLag <- factor(df$emLag)
#   df$Nns   <- interaction(df$Nsize,df$Nradius)
#   df$Nns.f  <-  factor(df$Nns, levels=levels(df$Nns), labels = paste0("size=",nnSizes," | radius=",nnRadius))
#
#   if(diagPlot){
#
#     dfs <- data.frame(startAt= c(.5, graphics::hist(emDims,plot=FALSE)$mids),
#                       stopAt = c(graphics::hist(emDims,plot=FALSE)$mids,max(emDims)+.5),
#                       f=factor(seq_along(c(.5, graphics::hist(emDims,plot=FALSE)$mids))%%2))
#
#     tmi <-  nonlinearTseries::mutualInformation(y, lag.max = maxLag, n.partitions = , do.plot = FALSE)
#
#     dfMI <- data.frame(emDelay = tmi$time.lag[-1],
#                        ami     = tmi$mutual.information[-1])
#
#
#     #  RColorBrewer::brewer.pal(n = length(unique(df$emLag) name = "Set2")
#
#     # use: alpha()
#
#     Ncol <- length(emLags$selection.method[!is.na(emLags$optimal.lag)])
#     myPal <- RColorBrewer::brewer.pal(Ncol,"Set2")
#     myPalLag <- myPal
#     names(myPalLag) <- emLags$selection.method[!is.na(emLags$optimal.lag)]
#     myPalNn <- myPal
#     names(myPalNn) <- emLags$optimal.lag[!is.na(emLags$optimal.lag)]
#
#     gNdims <- ggplot2::ggplot(df, ggplot2::aes_(y = ~Nn.pct, x = ~emDim, colour = ~emLag)) +
#       geom_rect( ggplot2::aes_(xmin = ~startAt, xmax = ~stopAt, fill = ~f), ymin = -Inf, ymax = Inf, data = dfs, inherit.aes = FALSE) +
#       scale_fill_manual(values = alpha(c("grey", "white"),.2), guide=FALSE) +
#       geom_hline(yintercept = nnThres, linetype = 2, colour = "grey60") +
#       geom_hline(yintercept = c(0,100),   colour = "grey60") +
#       geom_hline(yintercept = 50, colour = "grey90") +
#       geom_line(position  = position_dodge(.4)) +
#       geom_point(position = position_dodge(.4)) +
#       annotate("text",x=maxDim/3,y=nnThres, label="threshold", size = .8) +
#       xlab("Embedding Dimension") + ylab("Nearest neigbours (% of max.)") +
#       facet_wrap(~Nns.f, ncol=2) +
#       scale_x_continuous(breaks=emDims) +
#       scale_y_continuous(breaks = c(nnThres,50,100)) +
#       scale_color_manual("Lag", values = myPalNn ) +
#       theme_minimal() +
#       theme(strip.background = element_rect(colour = "grey90", fill = "grey90"),
#             strip.text.x = element_text(colour = "black", face = "bold"),
#             panel.spacing = ggplot2::unit(1, "lines"),
#             legend.background = element_rect(colour = "grey90",fill = "grey90"),
#             legend.title = element_text(face = "bold"),
#             legend.key = element_rect(colour = "grey90", fill = "grey90"),
#             panel.grid.minor.x = element_blank(),
#             panel.grid.major.y = element_blank(),
#             panel.grid.minor.y = element_blank()
#       )
#
#     gDelay <- ggplot2::ggplot(dfMI, ggplot2::aes_(y = ~ami, x = ~emDelay)) +
#       geom_line() +
#       geom_vline(data = emLags,  ggplot2::aes_(colour=factor(emLags$selection.method),
#                                                xintercept = ~optimal.lag), alpha = .3) +
#       geom_point(data = emLags,  ggplot2::aes_(x = ~optimal.lag, y = ~ami, colour = factor(emLags$selection.method)), size = 2) +
#       xlab("Embedding Lag") +
#       ylab("Average Mututal Information") +
#       scale_color_manual("Lag", values = myPalLag) +
#       theme_bw() +
#       theme(legend.position = c(.95, .95),
#             legend.justification = c("right", "top"),
#             legend.box.just = "right",
#             legend.margin = margin(6, 6, 6, 6),
#             legend.background = element_rect(colour = "grey90",fill = "grey90"),
#             legend.title = element_text(face = "bold"),
#             legend.key = element_rect(colour = "grey90", fill = "grey90"),
#             #panel.grid.major.x = element_blank(),
#             panel.grid.minor.x = element_blank(),
#             panel.grid.major.y = element_blank(),
#             panel.grid.minor.y = element_blank()
#
#       )
#
#     g <- gridExtra::grid.arrange(gDelay, gNdims, ncol=1, nrow=2)
#     grid::grid.newpage()
#     grid::grid.draw(g)
#
#   }
#
#   return(list(optimLag  = opLag,
#               optimDim  = opDim,
#               #optimRad  = opRad,
#               optimRow  = opt,
#               optimData = df,
#               diagPlot = invisible(g))[TRUE,TRUE,TRUE,TRUE,diagPlot]
#   )
# }
#
#
# # Find fixed or optimal radius
# #
# # @param RM Unthresholded Recurrence Matrix
# # @param y1  A numeric vector or time series
# # @param y2  A numeric vector or time series
# # @param emLag Delay to use for embedding
# # @param emDim Number of embedding dimensions
# # @param type Either \code{"fixed"} (default) or \code{"optimal"}, \code{"fixed"} will search for a radius that is close to the value for the \code{targetMeasure} in \code{targetValue}, \code{"optimal"} will optimise the radius for the \code{targetMeasure}, \code{targetValue} is ignored.
# # @param startRadius If \code{type = "fixed"} this is the starting value for the radius (default = percentile of unique distances in RM given by \code{targetValue}). If \code{type = "optimal"} this will be a range of radius values (in normalised SD units) that will be considered (default = \code{seq(0,2,by=.01)})
# # @param eachRadius If \code{type = "optimal"} this is the number of signal and noise series that will be generated for each level in \code{startRadius} (default = \code{1})
# # @param targetMeasure If \code{type = "optimal"}, it must be a character vector indicating which recurrence measure to optimise the radius for, options are "RR" (default), "DET", "LAM", "T1", and "all". The option \code{targetMeasure = "all"} will report all the optimal values obtained from one realisation of \code{startRadius * eachRadius} signal and noise series.
# # @param targetValue When argument \code{type} is set to "fixed", the value represents the target value for the measure in \code{targetMeasure} (default = \code{RR = .05}).
# # @param tol Tolerance for achieving \code{targetValue} for \code{targetMeasure} (default = \code{0.1})
# # @param maxIter If \code{type = "fixed"}: Maximum number of iterations to reach targetValue.
# # @param theiler Size of theiler window (default \code{0})
# # @param histIter Return iteration history? (default = \code{FALSE})
# # @param noiseLevel Noise level to construct the \code{signal + noiseLevel *} \eqn{N(\mu=0,\sigma=1)} (default = \code{0.75})
# # @param noiseType Type
# # @param plotROC Generates an ROC plot if \code{type = "optimal"}
# # @param standardise Standardise
# # @param radiusOnFail Radius to return when search fails \code{"tiny" = 0 + ,Machine.double.eps}, this will likely cause a matrix full of zeros. \code{"huge" = 1 + max. distance in RM}, which will give a matrix full of ones, \code{"percentile" = quantile(RM, prob = targetValue) of distances greater than 0}.
# # @param silent Silent-ish
# #
# # @family Recurrence Quantification Analysis
# #
# # @return A dataframe listing settings ussed to search for the radius, the radius found given the settings and the recurrence rate produced by the radius (either 1 row or the entire iteration history)
# # @export
# #
# crqa_radius <- function(RM = NULL,
#                         y1 = NULL,
#                         y2 = NULL,
#                         emLag = 1,
#                         emDim = 1,
#                         type           = c("fixed","optimal")[1],
#                         startRadius    = NULL,
#                         eachRadius     = 1,
#                         targetMeasure  = c("RR","DET","LAM","T1","all")[1],
#                         targetValue    = 0.05,
#                         tol            = 0.1,
#                         maxIter        = 100,
#                         theiler        = -1,
#                         histIter       = FALSE,
#                         noiseLevel     = 0.75,
#                         noiseType      = c("normal","uniform")[1],
#                         plotROC        = FALSE,
#                         standardise  = c("mean.sd","median.mad","none")[3],
#                         radiusOnFail   = c("tiny","huge","percentile")[1],
#                         silent         = FALSE){
#
#   optimOK <- FALSE
#   if(is.null(RM)&!is.null(y1)){
#     optimOK <- TRUE
#     RM <- rp(y1=y1, y2=y2,emDim=emDim, emLag=emLag)
#   }
#
#   # check auto-recurrence
#   RM   <- rp_checkfix(RM, checkAUTO = TRUE)
#   AUTO <- attr(RM,"AUTO")
#
#   if(AUTO){
#     if(!silent){cat(paste0("\nAuto-recurrence: Setting diagonal to (1 + max. distance) for analyses\n"))}
#     if(theiler < 0) theiler <- 0
#   }
#
#   if(is.null(startRadius)){
#     if(type=="fixed"){
#       startRadius <- as.numeric(stats::quantile(unique(as.vector(RM[lower.tri(RM)])),probs = ifelse(targetValue>=1,.05,targetValue)))
#     } else {
#       startRadius <- seq(0,1.5,by=0.001)
#     }
#   }
#
#   recmatsize <- rp_size(RM,AUTO,theiler)
#
#   if(theiler>=0){
#     RM <- bandReplace(RM,-theiler,theiler,1+max(RM),silent = silent)
#   }
#
#   if(type%in%"fixed"){
#
#     if(tol%][%c(0,1)){stop("Argument tol must be between 0 and 1.")}
#
#     tryRadius <- startRadius
#     Measure   <- 0
#     iter      <- 0
#     Converged <- FALSE
#     seqIter   <- 1:maxIter
#
#     iterList <- data.frame(iter        = seqIter,
#                            Measure     = Measure,
#                            Radius      = tryRadius,
#                            targetValue = targetValue,
#                            tollo       = targetValue*(1-tol),
#                            tolhi       = targetValue*(tol+1),
#                            startRadius = startRadius,
#                            rp.size = recmatsize,
#                            AUTO        = AUTO,
#                            Converged   = Converged, check.names = FALSE)
#
#     exitIter <- FALSE
#     if(!silent){cat(paste("\nSearching for a radius that will yield",targetValue, "for", targetMeasure,"\n"))}
#
#     # p <- dplyr::progress_estimated(maxIter)
#     while(!exitIter){
#
#
#       iter <- iter+1
#       #p$tick()$print()
#
#       RMs <- di2bi(RM,emRad = tryRadius, convMat = TRUE)
#       RT  <- Matrix::nnzero(RMs)
#       rp.size <- length(RMs)
#       Measure <- RT/rp.size
#
#       # crpOut <- crqa_rp(RM = RMs, emRad = tryRadius, AUTO=AUTO)
#       #Measure  <-  crpOut[[targetMeasure]]
#       #crpOut <- data.frame(RR = RR, RT = RT, size = length(RMs))
#       #Measure <- RR
#
#       iterList[iter,] <-    cbind.data.frame(iter        = iter,
#                                              Measure     = Measure,
#                                              Radius      = tryRadius,
#                                              targetValue = targetValue,
#                                              tollo       = targetValue*(1-tol),
#                                              tolhi       = targetValue*(tol+1),
#                                              startRadius = startRadius,
#                                              rp.size = recmatsize,
#                                              AUTO        = AUTO,
#                                              Converged   = Converged)
#
#       if(any(Measure%[]%c(targetValue*(1-tol),targetValue*(tol+1)),(iter>=maxIter))){
#         if(Measure%[]%c(targetValue*(1-tol),targetValue*(tol+1))){
#           Converged <- TRUE
#           if(!silent){message("\nConverged! Found an appropriate radius...")}
#         }
#         iterList$Converged[iter] <- Converged
#         exitIter <- TRUE
#       }
#
#       if(round(Measure,digits = 2)>round(targetValue,digits = 2)){
#         tryRadius <- tryRadius*(min(0.8,tol*2))
#       } else {
#         tryRadius <- tryRadius*(min(1.8,1+(tol*2)))
#       }
#     } # While ....
#
#     if(iter>=maxIter){
#       warning("Max. iterations reached!")
#     }
#     if(Measure %][% c(targetValue*(1-tol),targetValue*(tol+1))){
#       iterList$Radius[iter] <- dplyr::case_when(
#         radiusOnFail%in%"tiny" ~ 0 + .Machine$double.eps,
#         radiusOnFail%in%"huge" ~ 1 + max(RM),
#         radiusOnFail%in%"percentile" ~ as.numeric(stats::quantile(unique(as.vector(Matrix::tril(RM,-1))),probs = ifelse(targetValue>=1,.05,targetValue)))
#       )
#       warning(paste0("\nTarget not found, try increasing tolerance, max. iterations, or, change value of startRadius.\nreturning radius: ",iterList$Radius[iter]))
#     }
#
#     ifelse(histIter,id<-c(1:iter),id<-iter)
#     return(iterList[id,])
#
#   } # if "fixed"
#
#   if(type%in%"optimal"){
#
#     if(optimOK){
#
#       if(!silent){cat(paste0("\nNormalisation set to: ",standardise,"!!\n"))}
#
#       startRadius <- rep(startRadius, each = eachRadius)
#
#       dfREC  <-  plyr::ldply(startRadius, function(r){roc_noise(y = y1,
#                                                                 emDim = emDim,
#                                                                 emLag = emLag,
#                                                                 emRad = r,
#                                                                 noiseLevel = noiseLevel,
#                                                                 standardise = standardise,
#                                                                 noiseType = noiseType)},
#                              .progress = plyr::progress_text(char = "o~o"))
#
#
#       dfREC      <-  dplyr::arrange(dfREC,dfREC$response,dfREC$emRad)
#       caseID    <- dfREC$response%in%"signal+noise"
#       controlID <- dfREC$response%in%"noise"
#
#       rocREC <- list(RR  = pROC::roc(cases=dfREC$RR[caseID], controls=dfREC$RR[controlID]),
#                      DET = pROC::roc(cases=dfREC$DET[caseID], controls=dfREC$DET[controlID]),
#                      LAM = pROC::roc(cases=dfREC$LAM[caseID], controls=dfREC$LAM[controlID]),
#                      T1  = pROC::roc(cases=dfREC$T1[caseID], controls=dfREC$T1[controlID]))
#
#
#       optimal.radius <- list(RR  = dfREC$emRad[(dfREC$RR>=min(pROC::coords(rocREC$RR,
#                                                                            "b",best.method="c",
#                                                                            ret="t")))][1],
#                              DET = dfREC$emRad[(dfREC$DET  >=min(pROC::coords(rocREC$DET,
#                                                                               "b",best.method="c",
#                                                                               ret="t")))][1],
#                              LAM = dfREC$emRad[(dfREC$LAM  >=min(pROC::coords(rocREC$LAM,
#                                                                               "b",best.method="c",
#                                                                               ret="t")))][1],
#                              T1  = dfREC$emRad[(dfREC$T1   >=min(pROC::coords(rocREC$T1,
#                                                                               "b",best.method="c",
#                                                                               ret="t")))][1]
#       )
#
#       optimal.value <- list(RR  = crqa_cl(y1, emRad = optimal.radius$RR, emDim=emDim, emLag=emLag)[["RR"]],
#                             DET = crqa_cl(y1, emRad = optimal.radius$DET, emDim=emDim, emLag=emLag)[["DET"]],
#                             LAM = crqa_cl(y1, emRad = optimal.radius$LAM, emDim=emDim, emLag=emLag)[["LAM"]],
#                             T1  = crqa_cl(y1, emRad = optimal.radius$T1, emDim=emDim, emLag=emLag)[["T1"]]
#       )
#
#       if(targetMeasure=="all"){
#
#         if(plotROC){
#
#           op <- graphics::par(pty="s", mfrow=c(2,2))
#
#           pROC::plot.roc(rocREC$RR,
#                          print.thres="best",
#                          print.thres.best.method="closest.topleft",
#                          print.thres.cex=.6,
#                          print.auc=TRUE,
#                          print.auc.x=.9,
#                          print.auc.y=.1,
#                          auc.polygon=TRUE,
#                          main = paste("RR =",round(optimal.value$RR,3),"- Radius =",optimal.radius$RR))
#
#           pROC::plot.roc(rocREC$DET,
#                          print.thres="best",
#                          print.thres.best.method="closest.topleft",
#                          print.thres.cex=.6,
#                          print.auc=TRUE,
#                          print.auc.x=.9,
#                          print.auc.y=.1,
#                          auc.polygon=TRUE,
#                          main = paste("DET =",round(optimal.value$DET,3),"- Radius =",optimal.radius$DET))
#
#           pROC::plot.roc(rocREC$LAM,
#                          print.thres="best",
#                          print.thres.best.method="closest.topleft",
#                          print.thres.cex=.6,
#                          print.auc=TRUE,
#                          print.auc.x=.9,
#                          print.auc.y=.1,
#                          auc.polygon=TRUE,
#                          main = paste("LAM =",round(optimal.value$LAM,3),"- Radius =",optimal.radius$LAM))
#
#           pROC::plot.roc(rocREC$T1,
#                          print.thres="best",
#                          print.thres.best.method="closest.topleft",
#                          print.thres.cex=.6,
#                          print.auc=TRUE,
#                          print.auc.x=.9,
#                          print.auc.y=.1,
#                          auc.polygon=TRUE,
#                          main = paste("T1 =",round(optimal.value$T1,3),"- Radius =",optimal.radius$T1))
#
#           #    plot(precision ~ recall, t(pROC::coords(rocREC, "all", ret = c("recall", "precision"))), type="l")
#           #  graphics::text(x=-5,y=0,labels=paste("Optimal radius for", targetMeasure,"=",optimal.value,"is:",optimal.radius))
#           graphics::par(op)
#         }
#
#         return(data.frame(measure = c("RR","DET","LAM","T1"),
#                           optimal = plyr::ldply(optimal.value)[-1],
#                           radius  = plyr::ldply(optimal.radius)[-1]
#         )
#         )
#
#       } else {
#
#         #  rocREC <- dplyr::case_when(
#         #    targetMeasure == "RR"  ~ list(rocREC=pROC::roc(controls=dfREC$RR[dfREC$response%in%"signal+noise"],
#         #                                                   cases=dfREC$RR[dfREC$response=="noise"])),
#         #    targetMeasure == "DET" ~ list(rocREC=pROC::roc(controls=dfREC$DET[dfREC$response%in%"signal+noise"],
#         #                                                   cases=dfREC$DET[dfREC$response=="noise"])),
#         #    targetMeasure == "LAM" ~ list(rocREC=pROC::roc(controls=dfREC$LAM[dfREC$response%in%"signal+noise"],
#         #                                                   cases=dfREC$LAM[dfREC$response=="noise"])),
#         #    targetMeasure == "T1"  ~ list(rocREC=pROC::roc(controls=dfREC$T1[dfREC$response%in%"signal+noise"],
#         #                                                   cases=dfREC$T1[dfREC$response=="noise"]))
#         #  )
#
#
#         optimal.radius <- optimal.radius[[targetMeasure]]
#         optimal.value <- optimal.value[[targetMeasure]]
#         rocREC        <- rocREC[[targetMeasure]]
#
#         if(plotROC){
#           op<-graphics::par(pty="s")
#           pROC::plot.roc(rocREC,
#                          print.thres="best",
#                          print.thres.best.method="closest.topleft",
#                          print.thres.cex=.6,
#                          print.auc=TRUE,
#                          print.auc.x=.9,
#                          print.auc.y=.1,
#                          auc.polygon=TRUE,
#                          main = paste(targetMeasure,"=",round(optimal.value,3),"- Radius =",optimal.radius))
#
#           #    plot(precision ~ recall, t(pROC::coords(rocREC, "all", ret = c("recall", "precision"))), type="l")
#           #  graphics::text(x=-5,y=0,labels=paste("Optimal radius for", targetMeasure,"=",optimal.value,"is:",optimal.radius))
#           graphics::par(op)
#         }
#
#       }
#
#       return(cbind.data.frame(measure=targetMeasure,
#                               optimal.value=optimal.value[[1]]%00%NA,
#                               optimal.radius=optimal.radius%00%NA))
#
#     } else {
#
#       stop("Need time series vector(s) to perform optimal Radius search.")
#
#     }
#   } # if "optimal"
# }
#
#
# # roc_noise
# #
# # @param y y
# # @param emRad radius
# # @param emDim embedding Dims
# # @param emLag embedding Lag
# # @param noiseLevel noise Level
# # @param standardise Standardise y? Choose from "mean.sd","median.mad","none".
# # @param noiseType Use a Normal distribution of uniform distribution for noiselevels
# #
# # @return data frame for ROC
# # @export
# #
# # @keywords internal
# roc_noise <- function(y, emRad, emDim=1, emLag=1, noiseLevel=.75, standardise = c("mean.sd","median.mad","none")[3], noiseType = c("normal","uniform")[1]){
#   y <- dplyr::case_when(
#     standardise == "mean.sd"   ~ ts_standardise(y, type="mean.sd"),
#     standardise == "median.sd" ~ ts_standardise(y, type="median.mad"),
#     standardise == "none"      ~ y
#   )
#
#   yn <- dplyr::case_when(
#     noiseType == "normal"  ~ stats::rnorm(NROW(y), mean=round(mean(y, na.rm = TRUE),3), sd=stats::sd(y,na.rm = TRUE)),
#     noiseType == "uniform" ~ sign(rnorm(1))*stats::runif(NROW(y), min=floor(min(y, na.rm = TRUE)), max = ceiling(max(y,na.rm = TRUE)))
#   )
#
#   noise_out   <- crqa_cl(yn, emRad = emRad, emDim=emDim, emLag=emLag)
#   measure_out <- crqa_cl((y  + noiseLevel * yn), emRad = emRad, emDim=emDim, emLag=emLag)
#
#   return(cbind.data.frame(radius   = emRad,
#                           response = c("signal+noise","noise"),
#                           rbind(measure_out,noise_out)))
# }
#
#
#
# # Get (C)RQA measures from a Recurrence Matrix
# #
# # Use `crqa_rp`
# #
# # @param RM A binary recurrence matrix
# # @param emRad Threshold for distance value that counts as a recurrence
# # @param DLmin Minimal diagonal line length
# # @param VLmin Minimal vertical line length
# # @param HLmin Minimal horizontal line length
# # @param DLmax Maximal diagonal line length
# # @param VLmax Maximal vertical line length
# # @param HLmax Maximal horizontal line length
# # @param AUTO Is this an AUTO RQA?
# # @param theiler theiler
# # @param chromatic Chromatic RQA?
# # @param matrices Return Matrices?
# # @param doHalf Analyse half of the matrix?
# # @param Nboot How many bootstraps?
# # @param CL Confidence Limit for bootstrap results
# # @param doParallel Use parallel?
# #
# #
# # @family Recurrence Quantification Analysis
# #
# # @export
# #
# # @keywords internal
# #
# crqa_rp_measures <- function(RM,
#                              emRad = NULL,
#                              DLmin = 2,
#                              VLmin = 2,
#                              HLmin = 2,
#                              DLmax = length(Matrix::diag(RM)),
#                              VLmax = length(Matrix::diag(RM)),
#                              HLmax = length(Matrix::diag(RM)),
#                              AUTO      = NULL,
#                              theiler = NULL,
#                              chromatic = FALSE,
#                              matrices  = FALSE,
#                              doHalf    = FALSE,
#                              Nboot     = NULL,
#                              CL        = .95,
#                              doParallel = FALSE){
#
#
#   if(is.null(Nboot)){Nboot = 1}
#
#   NRows <- NROW(RM)
#   NCols <- NCOL(RM)
#   mc.cores <- parallel::detectCores()-1
#   if(Nboot<mc.cores) mc.cores <- Nboot
#
#
#   if(doParallel){
#     tstart <- Sys.time()
#     cl <- parallel::makeCluster(mc.cores)
#     out    <- parallel::mclapply(1, function(i){
#       #crqa_rp_prep(matrix(RM[ceiling(NCols*NRows*stats::runif(NCols*NRows))], ncol=NCols, nrow = NRows),
#       crqa_rp_prep(RP = RM,
#                    emRad= emRad,
#                    DLmin = DLmin,
#                    VLmin = VLmin,
#                    HLmin = HLmin,
#                    DLmax = DLmax,
#                    VLmax = VLmax,
#                    HLmax = HLmax,
#                    AUTO  = AUTO,
#                    chromatic = chromatic,
#                    matrices  = matrices,
#                    doHalf    = doHalf)
#     },
#     mc.cores = mc.cores
#     )
#     parallel::stopCluster(cl)
#     tend  <- Sys.time()
#   } else {
#     out <- crqa_rp_prep(RP = RM,
#                         emRad= emRad,
#                         DLmin = DLmin,
#                         VLmin = VLmin,
#                         HLmin = HLmin,
#                         DLmax = DLmax,
#                         VLmax = VLmax,
#                         HLmax = HLmax,
#                         AUTO  = AUTO,
#                         chromatic = chromatic,
#                         matrices  = matrices,
#                         doHalf    = doHalf)
#   }
#
#
#   dfori <- tidyr::gather(as.data.frame(out), key = "measure", value = "value")
#
#   if(Nboot>1){
#
#
#     cat(paste0("Bootstrapping Recurrence Matrix... ",Nboot," iterations...\n"))
#     cat(paste0("Estimated duration: ", round((difftime(tend,tstart, units = "mins")*Nboot)/max((round(mc.cores/2)-1),1), digits=1)," min.\n"))
#
#     tstart <-Sys.time()
#     cl <- parallel::makeCluster(mc.cores)
#     bootOut <-  parallel::mclapply(1:Nboot, function(i){
#       replicate <- as.data.frame(crqa_rp_prep(matrix(RM[ceiling(NCols*NRows*stats::runif(NCols*NRows))],
#                                                      ncol=NCols, nrow = NRows),
#                                               emRad= emRad,
#                                               DLmin = DLmin,
#                                               VLmin = VLmin,
#                                               HLmin = HLmin,
#                                               DLmax = DLmax,
#                                               VLmax = VLmax,
#                                               HLmax = HLmax,
#                                               AUTO  = AUTO,
#                                               chromatic = chromatic,
#                                               matrices  = matrices,
#                                               doHalf    = doHalf))
#       replicate$replicate = i
#       return(replicate)
#     },
#     mc.cores = mc.cores
#     )
#     parallel::stopCluster(cl)
#     tend <- Sys.time()
#     cat(paste0("Actual duration: ", round(difftime(tend,tstart, units = "mins"), digits=1)," min.\n"))
#
#     dfrepl <- plyr::ldply(bootOut)
#     dfrepl <- tidyr::gather(dfrepl, key = "measure", value = "value", -replicate)
#
#     if(length(CL)==1){
#       ci.lo <- (1-CL)/2
#       ci.hi <- CL + ci.lo
#     } else {
#       ci.lo <- CL[1]
#       ci.hi <- CL[2]
#     }
#
#     rqout <-  dfrepl %>%
#       dplyr::group_by(dfrepl$measure) %>%
#       dplyr::summarise(
#         val          = NA,
#         ci.lower     = stats::quantile(dfrepl$value%00%NA, ci.lo, na.rm = TRUE),
#         ci.upper     = stats::quantile(dfrepl$value%00%NA, ci.hi, na.rm = TRUE),
#         BOOTmean     = mean(dfrepl$value%00%NA, na.rm = TRUE),
#         BOOTsd       = stats::sd(dfrepl$value%00%NA, na.rm = TRUE),
#         BOOTse       = stats::sd(dfrepl$value%00%NA, na.rm = TRUE)/sqrt(Nboot),
#         BOOTvar      = stats::var(dfrepl$value%00%NA, na.rm = TRUE),
#         BOOTmedian   = stats::median(dfrepl$value%00%NA, na.rm = TRUE),
#         BOOTmad      = stats::mad(dfrepl$value%00%NA, na.rm = TRUE),
#         BOOTn        = Nboot
#       )
#
#     for(m in 1:nrow(rqout)){
#       rqout$val[rqout$measure%in%dfori$measure[m]] <- dfori$value[m]
#
#     }
#   } else {
#     rqout <- dfori %>% tidyr::spread(dfori$measure,dfori$value)
#   }
#   return(rqout)
# }
#
#
# # Get bootsrapped (C)RQA measures based on a recurrence matrix
# #
# # A zoo of measures based on singular recurrent points, diagonal, vertical and horizontal line structures will be caluclated.
# #
# # @param RM A distance matrix, or a matrix of zeroes and ones (you must set \code{emRad = NULL})
# # @param emRad Threshold for distance value that counts as a recurrence
# # @param DLmin Minimal diagonal line length (default = \code{2})
# # @param VLmin Minimal vertical line length (default = \code{2})
# # @param HLmin Minimal horizontal line length (default = \code{2})
# # @param DLmax Maximal diagonal line length (default = length of diagonal -1)
# # @param VLmax Maximal vertical line length (default = length of diagonal -1)
# # @param HLmax Maximal horizontal line length (default = length of diagonal -1)
# # @param AUTO Auto-recurrence? (default = \code{FALSE})
# # @param theiler = Use a theiler window around the line of identity / synchronisation to remove high auto-correlation at short time-lags (default = \code{0})
# # @param chromatic Force chromatic RQA? (default = \code{FALSE})
# # @param matrices Return matrices? (default = \code{FALSE})
# # @param doHalf Analyse half of the matrix? (default = \code{FALSE})
# # @param Nboot How many bootstrap replications? (default = \code{NULL})
# # @param CL Confidence limit for bootstrap results (default = \code{.95})
# # @param targetValue A value passed to \code{crqa_radius(...,type="fixed", targetMeasure="RR", tol = .2)} if \code{is.na(emRad)==TRUE}, it will estimate a radius (default = \code{.05}).
# # @param doParallel Speed up calculations by using the parallel processing options provided by `parallel` to assign a seperate process/core for each window in windowed (C)RQA analysis using \code{\link[purrr]{map2}} to assign data and \code{\link[parallel]{detectCores}} with  \code{logical = TRUE} to decide on the available cores (default = \code{FALSE})
# # @param silent Do not display any messages (default = \code{TRUE})
#
# # @return A list object containing (C)RQA measures (and matrices if requested)
# #
# # @export
# #
# # @family Recurrence Quantification Analysis
# #
# #
# crqa_rp <- function(RM,
#                     emRad = NA,
#                     DLmin = 2,
#                     VLmin = 2,
#                     HLmin = 2,
#                     DLmax = length(Matrix::diag(RM))-1,
#                     VLmax = length(Matrix::diag(RM))-1,
#                     HLmax = length(Matrix::diag(RM))-1,
#                     AUTO      = NULL,
#                     theiler   = NULL,
#                     chromatic = FALSE,
#                     matrices  = FALSE,
#                     doHalf    = FALSE,
#                     Nboot     = NULL,
#                     CL        = .95,
#                     targetValue = .05,
#                     doParallel = FALSE,
#                     silent     = TRUE){
#
#
#   # Input should be a distance matrix, or a matrix of zeroes and ones with emRad = NULL, output is a list
#   # Fred Hasselman - August 2013
#
#   #require(parallel)
#
#   # check auto-recurrence and make sure Matrix has sparse triplet representation
#   RM <- rp_checkfix(RM, checkAUTO = TRUE, fixAUTO = TRUE, checkTSPARSE = TRUE, fixTSPARSE = TRUE)
#
#   if(is.null(AUTO)){
#     AUTO <- attr(RM,"AUTO")
#   }
#
#   if(is.na(emRad)){
#     if(!is.null(attributes(RM)$emRad)){
#       emRad <- attributes(RM)$emRad
#     } else {
#       # Check for attributes
#       if(is.na(targetValue)){
#         targetValue <- .05
#       }
#       if(!is.null(attributes(RM)$emDim)){
#         emDim <- attributes(RM)$emDim
#       } else {
#         emdim <- 1
#       }
#       if(!is.null(attributes(RM)$emLag)){
#         emLag <- attributes(RM)$emLag
#       } else {
#         emLag <- 1
#       }
#
#       emRad <- crqa_radius(RM = RM,
#                            emDim = emDim, emLag = emLag, targetValue = targetValue, tol = .2, radiusOnFail = "percentile", silent = silent)
#       if(emRad$Converged){
#         emRad <- emRad$Radius
#       } else {
#         emRad <- stats::sd(RM,na.rm = TRUE)
#       }
#     }
#   }
#
#   #uval <- unique(as.vector(RM))
#   if(!all(as.vector(RM)==0|as.vector(RM)==1)){
#     if(!is.null(emRad)){
#       RM <- di2bi(RM,emRad)
#     } else{
#       if(!chromatic){
#         stop("Expecting a binary (0,1) matrix.\nUse 'crqa_radius()', or set 'chromatic = TRUE'")
#       } else {
#         stop("Chromatic RQA not implemented yet.")
#       }
#     }
#     # } else {
#     #
#   }
#   #rm(RM)
#
#   out <- crqa_rp_calc(RM,
#                       emRad = emRad,
#                       DLmin = DLmin,
#                       VLmin = VLmin,
#                       HLmin = HLmin,
#                       DLmax = DLmax,
#                       VLmax = VLmax,
#                       HLmax = HLmax,
#                       AUTO  = AUTO,
#                       chromatic = chromatic,
#                       matrices  = matrices)
#
#   #
#   #   if(is.null(Nboot)){Nboot = 1}
#   #
#   #   out <- crqa_rp_measures(RM,
#   #                            emRad= emRad,
#   #                            DLmin = DLmin,
#   #                            VLmin = VLmin,
#   #                            HLmin = HLmin,
#   #                            DLmax = DLmax,
#   #                            VLmax = VLmax,
#   #                            HLmax = HLmax,
#   #                            AUTO  = AUTO,
#   #                            chromatic = chromatic,
#   #                            matrices  = matrices,
#   #                            doHalf = doHalf,
#   #                            Nboot  = Nboot,
#   #                            CL     = CL)
#   #
#   #   dfori <- dplyr::gather(out, key = measure, value = value)
#   #
#   #   col.ind <- dplyr::tbl_df(index(RM))
#   #   row.ind <- dplyr::tbl_df(sample(index(RM),size=nrow(RM)))
#   #
#   #   if(Nboot>1){cat(paste0("Bootstrapping Recurrence Matrix... ",Nboot," iterations.\n"))
#   #     bootout <- col.ind  %>%
#   #       bootstrap(Nboot) %>%
#   #       do(crqa_rp_measures(RM[row.ind,unlist(.)],
#   #                            emRad= emRad,
#   #                            DLmin = DLmin,
#   #                            VLmin = VLmin,
#   #                            HLmin = HLmin,
#   #                            DLmax = DLmax,
#   #                            VLmax = VLmax,
#   #                            HLmax = HLmax,
#   #                            AUTO  = AUTO,
#   #                            chromatic = chromatic,
#   #                            matrices  = matrices,
#   #                            doHalf = doHalf))
#   #
#   #     dfrepl <- gather(bootout, key = measure, value = value, -replicate)
#   #
#   #     if(length(CL)==1){
#   #       ci.lo <- (1-CL)/2
#   #       ci.hi <- CL + ci.lo
#   #     } else {
#   #       ci.lo <- CL[1]
#   #       ci.hi <- CL[2]
#   #     }
#   #
#   #     rqout <-  dfrepl %>% dplyr::group_by(measure) %>%
#   #       summarize(
#   #         val     = NA,
#   #         ci.low  = stats::quantile(value, ci.lo, na.rm = TRUE),
#   #         ci.high = stats::quantile(value, ci.hi, na.rm = TRUE),
#   #         mean    = mean(value, na.rm = TRUE),
#   #         sd      = stats::sd(value, na.rm = TRUE),
#   #         var     = stats::var(value, na.rm = TRUE),
#   #         N       = n(),
#   #         se      = stats::sd(value, na.rm = TRUE)/sqrt(n()),
#   #         median  = mean(value, na.rm = TRUE),
#   #         mad     = stats::mad(value, na.rm = TRUE)
#   #       )
#   #
#   #     for(m in 1:nrow(rqout)){
#   #       rqout$val[rqout$measure%in%dfori$measure[m]] <- dfori$value[m]
#   #     }
#   #   } else {
#   #     rqout <- dfori
#   #   }
#
#   return(out)
# }
#
#
#
# # Diagonal Recurrence Profile
# #
# # @param RM A binary recurrence matrix
# # @param diagWin Window around the line of synchrony
# # @param xname xlab
# # @param yname ylab
# # @param DLmin DLmin
# # @param VLmin VLmin
# # @param HLmin HLmin
# # @param DLmax DLmax
# # @param VLmax VLmax
# # @param HLmax HLmax
# # @param AUTO ARTO
# # @param chromatic chromatic
# # @param matrices matrices
# #
# # @return
# # @export
# #
# # @examples
# crqa_diagPofile <- function(RM,
#                             diagWin = NULL,
#                             xname = "",
#                             yname = "",
#                             DLmin = 2,
#                             VLmin = 2,
#                             HLmin = 2,
#                             DLmax = length(Matrix::diag(RM))-1,
#                             VLmax = length(Matrix::diag(RM))-1,
#                             HLmax = length(Matrix::diag(RM))-1,
#                             AUTO      = NULL,
#                             chromatic = FALSE,
#                             matrices  = FALSE){
#
#
#   if(!all(as.vector(RM)==0|as.vector(RM)==1)){
#     stop("Expecting a binary (0,1) matrix.")
#   }
#
#   # check auto-recurrence and make sure Matrix has sparse triplet representation
#   RM <- rp_checkfix(RM, checkAUTO = TRUE, fixAUTO = TRUE, checkTSPARSE = TRUE, fixTSPARSE = TRUE)
#
#   if(is.null(AUTO)){
#     AUTO <- attr(RM,"AUTO")
#   }
#
#   if(is.numeric(diagWin)){
#     if(diagWin>0){
#       diagWin <- seq(-diagWin,diagWin)
#     } else {
#       diagWin <- seq(-1*NCOL(RM),NCOL(RM))
#     }
#   } else {
#     stop("Diagonal window must be 1 positive integer!!")
#   }
#
#   #rp_lineDist(RM,d = diagWin, matrices = TRUE)
#   B <- rp_nzdiags(RM,removeNZ = FALSE)
#   diagID <- 1:NCOL(B)
#   names(diagID) <- colnames(B)
#   if(length(diagWin)<NCOL(B)){
#     cID <- which(colnames(B)%in%diagWin)
#     B <- B[,cID]
#     diagID <- seq_along(cID)
#     names(diagID) <- colnames(B)
#   }
#
#   dp <- plyr::ldply(diagID, function(i){
#     N <- NROW(B)-abs(as.numeric(colnames(B)[i]))
#     Nh1 <-
#     p <- sum(B[,i]==1, na.rm = TRUE)/N
#     data.frame(RR = p,
#                N = N,
#                SEd = sqrt((p*(1-p))/N),
#                SEh = sqrt((p*(1-p))/N))
#   }, .id = "Diagonal")
#
#   dp$group  <- 1
#   dp$labels <- paste(dp$Diagonal)
#   dp$labels[dp$Diagonal==0] <- ifelse(AUTO,"LOI","LOS")
#   dp$labels.f <- factor(dp$labels,levels = dp$labels,ordered = TRUE)
#
#   if(length(levels(dp$labels.f))>21){
#     ext <- max(min(abs(dp$Diagonal),na.rm = TRUE),abs(max(dp$Diagonal,na.rm = TRUE)))
#     breaks <- c(seq(-ext,-1,length.out = 10),0,seq(1,ext,length.out = 10))
#   } else {
#     breaks <- dp$labels
#   }
#
#   x1<-(which.min(as.numeric(paste(dp$Diagonal))))
#   x2<-(which.max(as.numeric(paste(dp$Diagonal))))
#
#   B
#   B_long <- B %>% tibble::as_tibble() %>% tidyr::gather(key = "diagonal", value = "rec")
#
#   dpdense <- density(dp$RR,n = 21)
#   round(seq(1,21,length.out=512))
#
#    ggplot2::ggplot(data.frame(x=dpdense$x,y=dpdense$y), aes(x=x,y=y)) +
#      ggplot2::geom_line() +
#      ggplot2::scale_x_continuous(breaks = dpdense$x, labels = breaks) +
#      theme_bw()
#
#
#
#
#    # scale_fill_manual(values=c("0"="white","1"="black"))
#
#  g <- ggplot2::ggplot(dp, ggplot2::aes_(x=~labels.f,y=~RR, group = ~group)) +
#    ggplot2::geom_path(alpha=.7) +
#    ggplot2::geom_vline(xintercept = which(dp$labels.f%in%c("LOS","LOI")), size=1, colour = "grey50") +
#     ggplot2::geom_point(pch=21,fill="grey50",size=3) +
#     ggplot2::scale_y_continuous("Recurrence Rate",limits = c(0,1)) +
#     ggplot2::scale_x_discrete("Diagonals in recurrence Matrix", breaks = breaks) +
#    ggplot2::geom_label(x=x1,y=1,label=paste0("Recurrences due to\n ",xname),hjust="left") +
#    ggplot2::geom_label(x=x2,y=1,label=paste0("Recurrences due to\n ",yname),hjust="right") +
#     ggplot2::theme_bw()
#
#  return(invisible(g))
#
# }
#
#
#
#
# # Estimate embedding lag (tau)
# #
# # A wrapper for nonlinearTseries::timemLag
# #
# # @param y Time series or numeric vector
# # @param selection.methods Selecting an optimal embedding lag (default: Return "first.e.decay", "first.zero", "first.minimum", "first.value", where value is 1/exp(1))
# # @param maxLag Maximal lag to consider (default: 1/4 of timeseries length)
# # @param ... Additional parameters
# #
# # @return The ami function with requested minima
# #
# # @export
# #
# # @family Estimate CRQA parameters
# #
# est_emLag <- function(y,
#                       selection.methods = "first.minimum",
#                       maxLag = length(y)/4,
#                       ...){
#
#   y   <- y[!is.na(y)]
#   tmi <- nonlinearTseries::mutualInformation(y,
#                                              lag.max = maxLag,
#                                              #n.partitions = floor((max(y)-min(y))/(2*stats::IQR(y)*length(y)^(-1/3))),
#                                              do.plot = FALSE
#   )
#
#   lags <- numeric(length=length(selection.methods))
#   cnt <- 0
#   for(sm in selection.methods){
#     cnt <- cnt + 1
#     lag <-try_CATCH(nonlinearTseries::timeLag(y,
#                                               technique = "ami",
#                                               selection.method = sm,
#                                               lag.max = maxLag,
#                                               do.plot = FALSE,
#                                               #n.partitions = floor((max(y)-min(y))/(2*stats::IQR(y)*length(y)^(-1/3)))
#     ))
#     if(any(grepl("Error",lag$value))){lags[cnt] <- NA} else {lags[cnt] <- lag$value}
#   }
#
#   #id.peaks <- find_peaks(tmi$mutual.information, m = 3, wells = TRUE)
#   id.min   <- tmi$time.lag[which.min(tmi$mutual.information)]
#   #as.numeric(tmi$mutual.information[id.peaks[id.min]])
#
#   out <-   cbind.data.frame(selection.method = c(selection.methods,
#                                                  "global.minimum",
#                                                  "maximum.lag"),
#                             optimal.lag = c(lags, id.min, maxLag)
#   )
#
#   #tmi$mutual.information[tmi$time.lag%in%out$optimal.lag]
#   out$ami <- sapply(out$optimal.lag, function(r){
#     if(is.na(r)){
#       NA
#     } else {
#       tmi$mutual.information[r]
#     }
#   })
#
#   #tout <- summarise(dplyr::group_by(out, opLag, ami), selection.method = paste0(unique(selection.method), collapse="|")
#
#   return(out)
# }
#
#
# # Estimate number of embedding dimensions
# #
# # @param y Time series or numeric vector
# # @param delay Embedding lag
# # @param maxDim Maximum number of embedding dimensions
# # @param threshold See \code{\link[nonlinearTseries]{estimateEmbeddingDim}}
# # @param max.relative.change See \code{\link[nonlinearTseries]{estimateEmbeddingDim}}
# # @param doPlot Plot
# # @param ... Other arguments (not in use)
# #
# # @description A wrapper for nonlinearTseries::estimateEmbeddingDim
# #
# # @return Embedding dimensions
# # @export
# #
# est_emDim <- function(y, delay = est_emLag(y), maxDim = 15, threshold = .95, max.relative.change = .1, doPlot = FALSE, ...){
#   cbind.data.frame(EmbeddingLag   = delay,
#                    EmbeddingDim   = nonlinearTseries::estimateEmbeddingDim(y,
#                                                                            time.lag  = delay,
#                                                                            threshold = threshold,
#                                                                            max.relative.change = max.relative.change,
#                                                                            max.embedding.dim = maxDim,
#                                                                            do.plot = doPlot)
#   )
# }
#
# # rp_nzdiags
# #
# # @description Get all nonzero diagonals of a binary matrix, or, diagonals specified as a vector by argument \code{d}.
# #
# # @param RM A binary (0,1) matrix.
# # @param d An optional vector of diagonals to extract.
# # @param returnVectorList Return list
# # @param returnNZtriplets Return a dataframe with coordinates of only nonzero elements in diagonals (default = \code{FALSE})
# # @param removeNZ Remove nonzero diagonals if \code{TRUE}. If \code{FALSE} returns the full diagonals matrix. Use e.g. to plot diagonal recurrence profiles (default = \code{TRUE})
# # @param silent Silent-ish mode
# #
# # @author Fred Hasselman
# #
# # @return A matrix object with nonzero diagonals as columns and/or a dataframe with coordinates of nonzero diagonal elements
# #
# # @export
# #
# # @family Distance matrix operations
# #
# rp_nzdiags <- function(RM=NULL, d=NULL, returnVectorList=TRUE, returnNZtriplets=FALSE, removeNZ = TRUE,silent = TRUE){
#   # Loosely based on MATLAB function spdiags() by Rob Schreiber - Copyright 1984-2014 The MathWorks, Inc.
#   #require(Matrix)
#
#   if(grepl("matrix",class(RM),ignore.case = TRUE)){
#
#     if(all(RM>0)){warning("All matrix elements are nonzero.")}
#
#     s  <- Sys.time()
#
#     nzdiagsM <- methods::as(RM, "dgTMatrix")
#     nzdiags  <- data.frame(row   = nzdiagsM@i,
#                            col   = nzdiagsM@j,
#                            value = nzdiagsM@x,
#                            ndiag = (nzdiagsM@j)-(nzdiagsM@i))
#     nzdiags <- dplyr::arrange(nzdiags,nzdiags$ndiag)
#
#  if(removeNZ){
#     if(!is.null(d)){
#       nd <- unique(nzdiags$ndiag)
#       # Get diagonals which have nonzero elements
#       d <- nd[nd%in%sort(as.vector(d))]
#     } else {
#       d <- unique(nzdiags$ndiag)
#     }
#    } else {
#       d <-  c(-1*rev(1:(NCOL(nzdiagsM)-1)),0,1:(NCOL(nzdiagsM)-1))
#     }
#
#     indMat <- col(RM)-row(RM)
#     indMatV <- as.vector(indMat)
#
#     #cat("\nStart extraction of nonzero diagonals\n")
#     m  <- NROW(RM)
#     n  <- NCOL(RM)
#     p  <- length(d)
#     if(is.logical(RM)){
#       B <- matrix(FALSE,nrow = min(c(m,n), na.rm = TRUE), ncol = p)
#     } else {
#       B <- matrix(0, nrow = min(c(m,n), na.rm = TRUE), ncol = p)
#     }
#     colnames(B) <- paste(d)
#
#     for(i in seq_along(d)){
#       B[(nzdiags$row[nzdiags$ndiag==d[i]])+1,i] <- nzdiags$value[nzdiags$ndiag==d[i]]
#     }
#
#     zID <- which(Matrix::colSums(Matrix::as.matrix(B))==0)
#     if(length(zID)>0){
#       if(removeNZ){
#       B  <- B[,-zID]
#       nzdiags <- nzdiags[nzdiags$ndiag%in%zID,]
#       }
#     }
#     #dl <- plyr::llply(seq_along(toList),function(dd){RM[indMat==toList[[dd]]]}, .progress = plyr::progress_text(char = "o~o"))
#     e  <- Sys.time()
#
#     if(!silent){cat(paste0("\nTime: ", round(difftime(e,s, units = "mins"), digits=1)," min.\n"))}
#
#     if(returnNZtriplets){
#       return(list(B = B, nzdiags = nzdiags))
#     } else {
#       return(B)
#     }
#   } else {
#     stop("Input to rp_nzdiags is not a matrix-like object.")
#   }
# }
#
# #
# # rp_nzdiags_matlab <- function(RP,d=NULL){
# #   # Loosely based on MATLAB function spdiags() by Rob Schreiber - Copyright 1984-2014 The MathWorks, Inc.
# #   #require(Matrix)
# #
# #   if(grepl("matrix",class(RP),ignore.case = TRUE)){
# #
# #     A <- RP
# #
# #     if(all(A>0)){warning("All matrix elements are nonzero.")}
# #
# #     # create an indicator for all diagonals in the matrix
# #     ind   <- col(A)-row(A)
# #
# #     # Split the matrix!
# #     spd <- split(A, ind)
# #
# #     if(is.null(d)){
# #
# #       # Get diagonals which have nonzero elements
# #       keepID <-plyr::ldply(spd, function(di) any(di>0))
# #       nzdiag <- spd[keepID$V1]
# #       # Indices of nonzero diagonals
# #       d      <- as.numeric(keepID$.id[keepID$V1])
# #
# #     } else {
# #
# #       # Diagonals are specified
# #       d <- sort(as.vector(d))
# #       keepID <- names(spd)%in%d
# #       nzdiag <- spd[keepID]
# #     }
# #
# #     # Deal with uneven rows and cols
# #     m  <- NROW(A)
# #     n  <- NCOL(A)
# #     p  <- length(d)
# #     if(is.logical(A)){
# #       B <- matrix(FALSE,nrow = min(c(m,n), na.rm = TRUE), ncol = p)
# #     } else {
# #       B <- matrix(0, nrow = min(c(m,n), na.rm = TRUE), ncol = p)
# #     }
# #
# #     for(i in seq_along(d)){
# #       B[zoo::index(nzdiag[[i]]),i] <- nzdiag[[i]]
# #     }
# #     colnames(B) <- paste(d)
# #
# #     return(B)
# #   }
# # }
# #
# #
# # rp_nzdiags_chroma <- function(RP, d=NULL){
# #   # Loosely based on MATLAB function spdiags() by Rob Schreiber - Copyright 1984-2014 The MathWorks, Inc.
# #   #require(Matrix)
# #
# #   if(grepl("matrix",class(RP),ignore.case = TRUE)){
# #
# #     A <- RP
# #
# #     if(all(A>0)){warning("All matrix elements are nonzero.")}
# #
# #     # create an indicator for all diagonals in the matrix
# #     ind   <- col(A)-row(A)
# #
# #     # Split the matrix!
# #     spd <- split(A, ind)
# #
# #     if(is.null(d)){
# #
# #       # Get diagonals which have nonzero elements
# #       keepID <-plyr::ldply(spd, function(di) any(di>0))
# #       nzdiag <- spd[keepID$V1]
# #       # Indices of nonzero diagonals
# #       d      <- as.numeric(keepID$.id[keepID$V1])
# #
# #     } else {
# #
# #       # Diagonals are specified
# #       d <- sort(as.vector(d))
# #       keepID <-  names(spd)%in%d
# #       nzdiag <- spd[keepID]
# #     }
# #
# #     # Deal with uneven rows and cols
# #     m  <- NROW(A)
# #     n  <- NCOL(A)
# #     p  <- length(d)
# #     if(is.logical(A)){
# #       B <- matrix(FALSE,nrow = min(c(m,n), na.rm = TRUE), ncol = p)
# #     } else {
# #       B <- matrix(0, nrow = min(c(m,n), na.rm = TRUE), ncol = p)
# #     }
# #
# #     for(i in seq_along(d)){
# #       B[zoo::index(nzdiag[[i]]),i] <- nzdiag[[i]]
# #     }
# #     colnames(B) <- paste(d)
# #
# #     return(B)
# #   }
# # }
#
#
#
#
# # Line length distributions
# #
# # @param RM A thresholded recurrence matrix (binary: 0 - 1)
# # @param DLmin Minimal diagonal line length (default = \code{2})
# # @param VLmin Minimal vertical line length (default = \code{2})
# # @param HLmin Minimal horizontal line length (default = \code{2})
# # @param DLmax Maximal diagonal line length (default = length of diagonal -1)
# # @param VLmax Maximal vertical line length (default = length of diagonal -1)
# # @param HLmax Maximal horizontal line length (default = length of diagonal -1)
# # @param d Vector of diagonals to be extracted from matrix \code{RM} before line length distributions are calculated. A one element vector will be interpreted as a windowsize, e.g., \code{d = 50} will extract the diagonal band \code{-50:50}. A two element vector will be interpreted as a band, e.g. \code{d = c(-50,100)} will extract diagonals \code{-50:100}. If \code{length(d) > 2}, the numbers will be interpreted to refer to individual diagonals, \code{d = c(-50,50,100)} will extract diagonals \code{-50,50,100}.
# # @param theiler Size of the theiler window, e.g. \code{theiler = 1} removes diagonal bands -1,0,1 from the matrix. If \code{length(d)} is \code{NULL}, 1 or 2, the theiler window is applied before diagonals are extracted. The theiler window is ignored if \code{length(d)>2}, or if it is larger than the matrix or band indicated by parameter \code{d}.
# # @param invert Relevant for Recurrence Time analysis: Return the distribution of 0 valued segments in nonzero diagonals/verticals/horizontals. This indicates the time between subsequent line structures.
# # @param AUTO Is this an AUTO RQA?
# # @param chromatic Chromatic RQA?
# # @param matrices Return the matrices ?
# # @param doHalf Analyse half of the matrix?
# #
# # @description Extract lengths of diagonal, vertical and horizontal line segments from a recurrence matrix.
# #
# # @details Based on the Matlab function \code{linedists} by Stefan Schinkel, Copyright (C) 2009 Stefan Schinkel, University of Potsdam, http://www.agnld.uni-potsdam.de
# #
# # References:
# # S. Schinkel, N. Marwan, O. Dimigen & J. Kurths (2009):
# # "Confidence Bounds of recurrence-based complexity measures
# # Physics Letters A,  373(26), pp. 2245-2250
# #
# # Copyright (C) 2009 Stefan Schinkel, University of Potsdam
# # \url{http://www.agnld.uni-potsdam.de}
# #
# # @author Fred Hasselman
# # @return A list object with distributions of line lengths. If \code{matrices = TRUE} datafr are returned whose columns represent the nonzero diagonals, verticals, or, horizontals.
# #
# # @export
# #
# # @family Distance matrix operations
# #
# rp_lineDist <- function(RM,
#                         DLmin = 2,
#                         VLmin = 2,
#                         HLmin = 2,
#                         DLmax = length(Matrix::diag(RM))-1,
#                         VLmax = length(Matrix::diag(RM))-1,
#                         HLmax = length(Matrix::diag(RM))-1,
#                         d         = NULL,
#                         theiler   = NULL,
#                         invert    = FALSE,
#                         AUTO      = NULL,
#                         chromatic = FALSE,
#                         matrices  = FALSE,
#                         doHalf    = FALSE){
#
#   # For boot()
#   # RP <- RP[indices,]
#
#   if(invert){RM <- Matrix::Matrix(1-RM)}
#
#   if(!all(as.vector(RM)==0|as.vector(RM)==1)){stop("Matrix should be a binary (0,1) matrix!!")}
#
#   if(!is.null(d)){
#     if(length(d)==1){d <- -d:d}
#     if(length(d)==2){d <-  d[1]:d[2]}
#   }
#   if(!is.null(theiler)){
#     if(length(d)<length(-theiler:theiler)){warning("Ignoring theiler window...")}
#     RM <- bandReplace(RM,-theiler,theiler,0)
#   }
#
#   RP <- matrix(0,ncol=NCOL(RM),nrow = NROW(rm))
#   if(Matrix::isSymmetric(unname(RM))){
#     if(all(Matrix::diag(RM)==1)){
#       RP <- bandReplace(RM,0,0,0)
#     }
#   } else {
#     RP <- RM
#   }
#
#
#   B <- rp_nzdiags(RP)
#   if(length(B)==0){B<-as.matrix(0)}
#   nzV <- which(colSums(Matrix::as.matrix(RP))>0)
#   V <- Matrix::as.matrix(Matrix::as.matrix(RP)[,nzV])
#   colnames(V) <- nzV
#
#   RP <- matrix(0,ncol=NCOL(RM),nrow = NROW(rm))
#   if(Matrix::isSymmetric(unname(RM))){
#     if(all(Matrix::diag(RM)==1)){
#       RP <- bandReplace(Matrix::t(RM),0,0,0)
#     }
#   } else {
#     RP <- Matrix::t(RM)
#   }
#
#   nzH <- which(colSums(Matrix::as.matrix(RP))>0)
#   H <- Matrix::as.matrix(Matrix::as.matrix(RP)[,nzH])
#   colnames(H) <- nzH
#   rm(RP)
#
#   # Get diagonal lines & pad with zeros
#   diagonals   <- rbind.data.frame(rep(0,dim(B)[2]),
#                                   B,
#                                   rep(0,dim(B)[2])
#                                   )
#
#   # get nonzero vertical Lines & pad with zeros
#   verticals <- rbind.data.frame(rep(0,dim(V)[2]),
#                                 V,
#                                 rep(0,dim(V)[2])
#                                 )
#   colnames(verticals) <- paste(1:ncol(verticals))
#
#   # get nonzero horizontal Lines & pad with zeros
#   horizontals <- rbind.data.frame(rep(0,dim(H)[2]),
#                                   H,
#                                   rep(0,dim(H)[2])
#                                   )
#   colnames(horizontals) <- paste(1:ncol(horizontals))
#
#   # Get indices of line lengths
#   diagonals.ind   <- tidyr::gather(diagonals,   key = "diagonal",   value = "segment")
#   verticals.ind   <- tidyr::gather(verticals,   key = "vertical",   value = "segment")
#   horizontals.ind <- tidyr::gather(horizontals, key = "horizontal", value = "segment")
#
#   D <- diagonals.ind$segment
#   names(D) <- paste0(diagonals.ind$diagonal,ifelse(invert,"DT","D"))
#   V <- verticals.ind$segment
#   names(V) <- paste0(verticals.ind$vertical,ifelse(invert,"VT","V"))
#   H <- horizontals.ind$segment
#   names(H) <- paste0(horizontals.ind$horizontal,ifelse(invert,"HT","H"))
#
#   # Get consecutive nonzero segments from indices, their difference is the segment length
#   # We added a row of 0s so we'll get sequences of -1, 1, -1
#   diagonals.dist   <- sort(which(diff(D)==-1)-which(diff(D)==1))
#   verticals.dist   <- sort(which(diff(V)==-1)-which(diff(V)==1))
#   horizontals.dist <- sort(which(diff(H)==-1)-which(diff(H)==1))
#
#   diagonals.dist   <- diagonals.dist[diagonals.dist%[]%c(DLmin,DLmax)]
#   verticals.dist   <- verticals.dist[verticals.dist%[]%c(VLmin,VLmax)]
#   horizontals.dist <- horizontals.dist[horizontals.dist%[]%c(HLmin,HLmax)]
#
#   if(length(diagonals.dist)==0){diagonals.dist <- NA}
#   if(length(verticals.dist)==0){verticals.dist <- NA}
#   if(length(horizontals.dist)==0){horizontals.dist <- NA}
#
#   return(list(diagonals.dist   = diagonals.dist,
#               verticals.dist   = verticals.dist,
#               horizontals.dist = horizontals.dist,
#               diagonals.mat = diagonals[-c(1,NROW(diagonals)),],
#               verticals.mat = verticals[-c(1,NROW(verticals)),],
#               horizontals.mat = horizontals[-c(1,NROW(horizontals)),])[c(TRUE,TRUE,TRUE,matrices,matrices,matrices)]
#   )
# }
#
#
# # Calculate Hamming distance
# #
# # @param X A matrix (of coordinates)
# # @param Y A matrix (of coordinates)
# # @param embedded Do X and/or Y represent surrogate dimensions of an embedded time series?
# #
# # @return A hamming-distance matrix of X, or X and Y. Useful for ordered and unordered categorical data.
# # @export
# #
# # @family Distance matrix operations
# # @author Fred Hasselman
# #
# dist_hamming <- function(X, Y=NULL, embedded=TRUE) {
#   if ( missing(Y) ) {
#     if(embedded){
#       # X and Y represent delay-embeddings of a timeseries
#       if(which.max(dim(X))==1){X <- t(X)}
#     }
#     uniqs <- unique(as.vector(X))
#     U <- X == uniqs[1]
#     H <- t(U) %*% U
#     for ( uniq in uniqs[-1] ) {
#       U <- X == uniq
#       H <- H + t(U) %*% U
#     }
#   } else {
#     if(embedded){
#       # X and Y represent delay-embeddings of a timeseries
#       if(which.max(dim(X))==1){X <- t(X)}
#       if(which.max(dim(Y))==1){Y <- t(Y)}
#     }
#     uniqs <- union(X, Y)
#     H <- t(X == uniqs[1]) %*% (Y == uniqs[1])
#     for ( uniq in uniqs[-1] ) {
#       H <- H + t(X == uniq) %*% (Y == uniq)
#     }
#   }
#   NROW(X) - H
# }
#
#
# # Replace matrix diagonals
# #
# # Sets a band of matrix diagonals to any given value
# #
# # @param mat A Matrix
# # @param lower Lower diagonal to be included in the band (should be \eqn{\le 0})
# # @param upper Upper diagonal to be included in the band (should be \eqn{\ge 0})
# # @param value A single value to replace all values in the selected band (default = \code{NA})
# # @param silent Operate in silence, only (some) warnings will be shown (default = \code{TRUE})
# #
# # @return A matrix in which the values in the selected diagonals have been replaced
# #
# # @export
# #
# # @family Distance matrix operations
# #
# # @author Fred Hasselman
# #
# #
# # @examples
# # # Create a 10 by 10 matrix
# # library(Matrix)
# # m <- Matrix(rnorm(10),10,10)
# #
# # bandReplace(m,-1,1,0)   # Replace diagonal and adjacent bands with 0 (Theiler window of 1)
# bandReplace <- function(mat, lower, upper, value = NA, silent=TRUE){
#   if(lower>0){lower=-1*lower
#   warning("lower > 0 ...\n using: -1*lower")
#   }
#   if(upper<0){upper=abs(upper)
#   warning("upper > 0 ...\n using: abs(upper)")
#   }
#   if(all(lower==0,upper==0)){
#     #diag(mat) <- value
#     if(!silent){message(paste0("lower and upper are both 0 (no band, just diagonal)\n using: diag(mat) <- ",round(value,4),"..."))}
#   }
#
#   delta <- col(mat)-row(mat)
#   mat[delta >= lower & delta <= upper] <- value
#
#   return(mat)
# }
#
#
# # Create a Distance Matrix
# #
# # @param y1 A numeric vector or time series
# # @param y2 A numeric vector or time series for cross recurrence
# # @param emDim The embedding dimensions
# # @param emLag The embedding lag
# # @param emRad The threshold (emRad) to apply to the distance matrix to create a binary matrix
# # @param to.ts Should \code{y1} and \code{y2} be converted to time series objects?
# # @param order.by If \code{to.ts = TRUE}, pass a vector of the same length as \code{y1} and \code{y2}. It will be used as the time index, if \code{NA} the vector indices will be used to represent time.
# # @param to.sparse Should sparse matrices be used?
# # @param method Distance measure to use. Any option that is valid for argument \code{method} of \code{\link[proxy]{dist}}. Type \code{proxy::pr_DB$get_entries()} to se a list of all the options. Common methods are: "Euclidean", "Manhattan", "Minkowski", "Chebysev" (or the same but shorter: "L2","L1","Lp" and "max" distance) (default = \code{"Euclidean"})
# # @param weighted Creates a matrix with \code{0} for distances greater than \code{emRad} and the distance magnitude for distances that fall within the radius. An atribute \code{weighted = TRUE} will be added to the matrix object (default = \code{FALSE})
# # @param doPlot Plot the matrix by calling \code{\link{rp_plot}} with defult settings
# # @param silent Silent-ish mode
# # @param ... Any paramters to pass to \code{\link{rp_plot}} if \code{doPlot = TRUE}
# #
# # @return A (Coss-) Recurrence matrix
# # @export
# #
# # @family Distance matrix operations
# #
# rp <- function(y1, y2 = NULL,
#                emDim = 1,
#                emLag = 1,
#                emRad = NULL,
#                to.ts = NULL,
#                order.by = NULL,
#                to.sparse = FALSE,
#                method = "Euclidean",
#                weighted = FALSE,
#                doPlot = FALSE,
#                silent = TRUE,
#                ...){
#
#   if(is.null(y2)){
#     y2 <- y1
#     attributes(y2) <- attributes(y1)
#   }
#
#   if(!is.data.frame(y1)){
#     id <- deparse(substitute(y1))
#     y1 <- as.data.frame(y1)
#     if(length(id)==NCOL(y1)){
#       colnames(y1) <- id
#     }
#   }
#
#   if(!is.data.frame(y2)){
#     id <- deparse(substitute(y2))
#     y2 <- as.data.frame(y2)
#     if(length(id)==NCOL(y2)){
#       colnames(y2) <- id
#     }
#   }
#
#   et1 <- ts_embed(y1, emDim, emLag, silent = silent)
#   et2 <- ts_embed(y2, emDim, emLag, silent = silent)
#
#   # dist_method <- try_CATCH(proxy::pr_DB$get_entry(method))
#   # if("error"%in%class(dist_method$value)){
#   #   stop("Unknown distance metric!\nUse proxy::pr_DB$get_entries() to see a list of valid options.")
#   #  } else {
#     # dmat <- proxy::dist(x=et2, y=et1,
#     #                     method = tolower(method))
#     dmat <- flexclust::dist2(x=et2, y=et1, method = tolower(method))
#   #}
#
#   # Remove proxy class
#   #dmat <- unclass(as.matrix(dmat))
#   dmat<- as.matrix(dmat)
#
#   # Check a time index was requested
#   if(!is.null(to.ts)){
#     if(is.null(order.by)){
#       order.by <- lubridate::as_datetime(1:NROW(dmat), origin = lubridate::ymd_hms(Sys.time()))
#     }
#     dmat <-  switch(to.ts,
#                     "xts" =  xts::xts(dmat, order.by = lubridate::as_datetime(order.by)),
#                     "zoo" =  zoo::zoo(dmat, order.by = lubridate::as_datetime(order.by))
#     )
#   }
#   # Add names if ordered by time vector
#   if(!is.null(order.by)){
#     colnames(dmat) <- paste(order.by)
#     rownames(dmat) <- paste(order.by)
#   }
#
#   if(to.sparse){
#     if(!is.null(emRad)){
#       if(weighted){
#         dmat <- di2we(dmat, emRad = emRad, convMat = TRUE)
#       } else {
#         dmat <- di2bi(dmat, emRad = emRad, convMat = TRUE)
#       }
#     }
#     attributes(dmat)$emDims1  <- et1
#     attributes(dmat)$emDims2  <- et2
#     attributes(dmat)$emDims1.name <- colnames(y1)
#     attributes(dmat)$emDims2.name <- colnames(y2)
#     #dmat <- rp_checkfix(dmat, checkAUTO=TRUE, fixAUTO=TRUE)
#   } else {
#     if(!is.null(emRad)){
#       if(weighted){
#         dmat <- di2we(dmat, emRad = emRad, convMat = FALSE)
#       } else {
#         dmat <- di2bi(dmat, emRad = emRad, convMat = FALSE)
#       }
#     }
#     attr(dmat,"emDims1") <- et1
#     attr(dmat,"emDims2") <- et2
#     attr(dmat,"emDims1.name") <- colnames(y1)
#     attr(dmat,"emDims2.name") <- colnames(y2)
#     attr(dmat,"weighted") <- weighted
#   }
#
#   dmat <- rp_checkfix(dmat, checkAUTO = TRUE, fixAUTO = TRUE)
#
#   if(doPlot){
#     dotArgs <- list(...)
#
#     nameOK  <- names(dotArgs)%in%methods::formalArgs(rp_plot)
#     # Plot with defaults
#     if(!all(dotArgs)){
#       dotArgs    <- formals(rp_plot)
#       nameOk <- rep(TRUE,length(dotArgs))
#     }
#
#     dotArgs$RM <- dmat
#     do.call(rp_plot, dotArgs[nameOk])
#   }
#   return(dmat)
# }
#
#
# # Copy Matrix Attributes
# #
# # Simple attribute copy used in `casnet` to convert between `matrix` and `Matrix` classes and back.
# #
# # @param source Source matrix
# # @param target Target matrix
# # @param source_remove Remove these attribute fields from the source before copying.
# #
# # @return The target matrix with attributes copied deom the source matrix.
# # @export
# #
# rp_copy_attributes <- function(source, target, source_remove = c("names", "row.names", "class","dim", "dimnames","x")){
#   attrList <- attributes(source)
#   attrList[source_remove] <- NULL
#   attributes(target) <- c(attributes(target), attrList)
#   return(target)
# }
#
# # Check a Recurrence Matrix
# #
# # @param RM RM
# # @param checkS4 checkS4
# # @param checkAUTO checkAUTO
# # @param checkSPARSE checkSPARSE
# # @param fixS4 fixS4
# # @param fixAUTO fixAUTO
# # @param fixSPARSE fixSPARSE
# #
# # @return A checked and/or fixed matrix
# # @export
# # @keywords internal
# #
# rp_checkfix <- function(RM, checkS4 = TRUE, checkAUTO = TRUE, checkSPARSE = FALSE, checkTSPARSE = FALSE, fixS4 = FALSE, fixAUTO = TRUE, fixSPARSE = FALSE, fixTSPARSE = FALSE){
#
#   dummy <- Matrix::Matrix(matrix(c(0,0)))
#   dummy <- rp_copy_attributes(source = RM, target =dummy)
#
#   # Always check S4
#   if(!checkS4){checkS4 <- TRUE}
#
#   yesS4      <- FALSE
#   yesAUTO    <- FALSE
#   yesSPARSE  <- FALSE
#   yesTSPARSE <- FALSE
#
#   if(checkS4){
#     yesS4 <- isS4(RM)
#   }
#   if(!yesS4&fixS4){
#     RM <- Matrix::Matrix(RM)
#   }
#
#   # check auto-recurrence
#   if(checkAUTO){
#     if(yesS4){
#       yesAUTO <- Matrix::isSymmetric(RM)
#     } else {
#       yesAUTO <- identical(as.vector(RM[lower.tri(RM)]),as.vector(t(RM)[lower.tri(RM)]))
#     }
#   }
#   if(fixAUTO){
#     attributes(RM)$AUTO    <- yesAUTO
#     attributes(dummy)$AUTO <- yesAUTO
#   }
#
#   if(checkTSPARSE){
#     if(class(RM)%in%names(methods::getClass("TsparseMatrix")@subclasses)){
#       yesTSPARSE <- TRUE
#     }
#   }
#   if(fixTSPARSE&!yesTSPARSE){
#     if(!yesS4){
#       RM <- Matrix::Matrix(RM,sparse = TRUE)
#     }
#     Mtype <- gsub("CMatrix","TMatrix",class(RM))
#     eval(parse(text=paste0("RM <- as(RM,'",Mtype,"')")))
#   }
#
#
#   RM <- rp_copy_attributes(source = dummy, target = RM, source_remove = c("Dimnames", "i", "class","Dim", "p","x","factors"))
#
#   return(RM)
# }
#
#
#
# # Plot (thresholded) distance matrix as a recurrence plot
# #
# # @param RM A distance matrix or recurrence matrix
# # @param plotDimensions Should the state vectors be plotted if they are available as attributes of RM (default = `TRUE`)
# # @param plotMeasures Print common (C)RQA measures in the plot if the matrix is binary
# # @param plotRadiusRRbar The `Radius-RR-bar` is a colour-bar guide plotted with an unthresholded distance matrix indicating a number of `RR` values one would get if a certain distance threshold were chosen (`default = TRUE`)
# # @param drawGrid Draw a grid on the recurrence plot (`default = FALSE`)
# # @param markEpochsLOI Pass a factor whose levels indicate different epochs or phases in the time series and use the line of identity to represent the levels by different colours (`default = NULL`)
# # @param Chromatic If `TRUE` and there are more than two discrete values in `RM`, give recurrent points a distinct colour. If `RM` was returned by `crqa_rp(..., chromatic = TRUE)`, the recurrence plot will colour-code recurrent points according to the category values in `attributes(RM)$chromaticRP` (`default = FALSE`)
# # @param radiusValue If `plotMeasures = TRUE` and RM is an unthresholded matrix, this value will be used to calculate recurrence measures. If `plotMeasures = TRUE` and RM is already a binary recurence matrix, pass the radius that was used as a threshold to create the matrix for display purposes. If `plotMeasures = TRUE` and `radiusValue = NA`, function `est_radius()` will be called with default settings (find a radius that yields .05 recurrence rate). If `plotMeasures = FALSE` this setting will be ignored.
# # @param title A title for the plot
# # @param xlabel An x-axis label
# # @param ylabel An y-axis label
# # @param plotSurrogate Should a 2-panel comparison plot based on surrogate time series be added? If `RM` has attributes `y1` and `y2` containing the time series data (i.e. it was created by a call to [rp()]), the following options are available: "RS" (random shuffle), "RP" (randomised phases), "AAFT" (amplitude adjusted fourier transform). If no timeseries data is included, the columns will be shuffled.  NOTE: This is not a surrogate test, just 1 surrogate is created from `y1`.
# # @param returnOnlyObject Return the ggplot object only, do not draw the plot (default = `TRUE`)
# #
# # @return A nice plot of the recurrence matrix.
# # @export
# #
# # @family Distance matrix operations (recurrence plot)
# #
# rp_plot_jmv <- function(RM,
#                     plotDimensions = FALSE,
#                     plotMeasures   = FALSE,
#                     plotRadiusRRbar = TRUE,
#                     drawGrid = FALSE,
#                     markEpochsLOI = NULL,
#                     Chromatic = NULL,
#                     radiusValue = NA,
#                     title = "", xlabel = "", ylabel="",
#                     plotSurrogate = NA,
#                     returnOnlyObject = FALSE){
#
#   useGtable <- TRUE
#
#   # # check patchwork
#   # if(!length(find.package("patchwork",quiet = TRUE))>0){
#   #   warning("Package patchwork is not installed...\n1. Install Xcode from App Store (MacOS) or rwintools.exe from CRAN (Windows) \n2. Install patchwork: devtools::install_github('thomasp85/patchwork')\n3. Install casnet: devtools::install_github('FredHasselman/casnet')\n....Using gtable instead, with limited options\n")
#   #   useGtable=TRUE
#   # }
#
#   colvec <- c("#FFFFFF","#000000")
#   names(colvec) <- c("0","1")
#
#   # check auto-recurrence and make sure Matrix has sparse triplet representation
#   RM   <- rp_checkfix(RM, checkAUTO = TRUE)
#   AUTO <- attr(RM,"AUTO")
#
#   # prepare data
#   if(attr(RM,"package")%00%""%in%"Matrix"){
#     RM     <- rp_checkfix(RM, checkTSPARSE = TRUE, fixTSPARSE = TRUE)
#     meltRP <- data.frame(Var1 = (RM@i+1), Var2 = (RM@j+1), value = as.numeric(RM@x))
#   } else {
#     meltRP <- reshape2::melt(as.matrix(RM))
#   }
#
#   hasNA <- FALSE
#   if(any(is.na(meltRP$value))){
#     hasNA <- TRUE
#     meltRP$value[is.na(meltRP$value)] <- max(meltRP$value, na.rm = TRUE) + 1
#   }
#
#   # check unthresholded
#   showL <- FALSE
#   if(!all(as.vector(meltRP$value[!is.na(meltRP$value)])==0|as.vector(meltRP$value[!is.na(meltRP$value)])==1)|attr(RM,"weighted")){
#
#     unthresholded <- TRUE
#
#     if(attr(RM,"weighted")){
#       plotRadiusRRbar <- FALSE
#     }
#
#     if(!is.null(markEpochsLOI)){
#       warning("Can't show epochs on an unthresholded Recurrence Plot!")
#     }
#
#   } else {
#
#     unthresholded <- FALSE
#
#     # Check epochs ----
#     if(!is.null(markEpochsLOI)){
#       if(is.factor(markEpochsLOI)&length(markEpochsLOI)==max(c(NROW(RM),NCOL(RM)))){
#         start <- max(meltRP$value, na.rm = TRUE) + 1
#         #cpal <- paletteer::paletteer_d(package = "rcartocolor",palette = "Safe", n = nlevels(markEpochsLOI))
#         cpal <- getColours(Ncols = nlevels(markEpochsLOI))
#         #cpal <- paletteer::paletteer_d(package = "ggthemes",palette = "tableau_colorblind10",n = nlevels(markEpochsLOI),direction = 1)
#
#         if(hasNA){
#           colvec <- c("#FFFFFF","#000000","#FF0000", cpal)
#           names(colvec) <- c("0","1","NA",levels(markEpochsLOI))
#         } else {
#           colvec <- c("#FFFFFF","#000000", cpal) #viridis::viridis_pal()(nlevels(markEpochsLOI)))
#           names(colvec) <- c("0","1",levels(markEpochsLOI))
#         }
#         N <- max(c(NROW(RM),NCOL(RM)))
#         for(i in 1:N){
#           j <- i
#           meltRP$value[meltRP$Var1==i&meltRP$Var2==j] <- start + as.numeric_character(markEpochsLOI)[i]
#         }
#         meltRP$value <- factor(meltRP$value, levels = sort(unique(meltRP$value)), labels = names(colvec))
#         showL <- TRUE
#       } else {
#         warning("Variable passed to 'markEpochsLOI' is not a factor or doesn't have correct length.")
#       }
#     } else {
#       colvec <- c("#FFFFFF","#000000")
#       names(colvec) <- c("0","1")
#       meltRP$value <- factor(meltRP$value, levels = c(0,1), labels = c("0","1"))
#     }
#   }
#
#   if(is.na(radiusValue)){
#     if(!is.null(attr(RM,"emRad"))|!is.na(attr(RM,"emRad"))){
#       radiusValue <- attr(RM,"emRad")
#     } else {
#       if(unthresholded|plotMeasures){
#         radiusValue <- est_radius(RM,silent = TRUE)$Radius
#         attr(RM,"emRad") <- radiusValue
#       }
#     }
#   }
#
#   # Get CRQA measures
#   if(plotMeasures){
#     # if(is.na(radiusValue)){
#     #   if(!is.null(attributes(RM)$emRad)){
#     #     radiusValue <- attr(RM,"emRad")
#     #   } else {
#     #     radiusValue <- est_radius(RM,silent = TRUE)$Radius
#     #   }
#     # }
#     if(unthresholded){
#       rpOUT   <- crqa_rp(RM, emRad = radiusValue, AUTO = AUTO)
#     } else {
#       rpOUT   <- crqa_rp(RM, AUTO = AUTO)
#     }
#   }
#
#   #meltRP$value <- log(meltRP$value+.Machine$double.eps)
#   # main plot ----
#   gRP <-  ggplot2::ggplot(ggplot2::aes_(x=~Var1, y=~Var2, fill = ~value), data= meltRP) +
#     ggplot2::geom_raster(hjust = 0, vjust=0, show.legend = showL) +
#     ggplot2::geom_abline(slope = 1,colour = "grey50", size = 1)
#
#   ## unthresholded ----
#   if(unthresholded){
#
#     # #barValue <- 0.05
#     # if(is.na(radiusValue)){
#     #   barValue <- mean(meltRP$value, na.rm = TRUE)
#     #   } else {
#     #   barValue <- est_radius(RM, targetValue = radiusValue, silent = TRUE, maxIter = 100, radiusOnFail = "percentile")$Radius
#     # }
#
#     if(!is.na(radiusValue)){
#       barValue <- radiusValue
#     } else {
#       barValue <- mean(meltRP$value, na.rm = TRUE)
#     }
#
#     if(attr(RM,"weighted")){
#
#       gRP <- gRP + ggplot2::scale_fill_gradient(low      = "white",
#                                                 high     = "red3",
#                                                 na.value = scales::muted("slategray4"),
#                                                 space    = "Lab",
#                                                 name     = "")
#
#
#     } else {
#
#       gRP <- gRP + ggplot2::scale_fill_gradient2(low      = "red3",
#                                                  high     = "steelblue",
#                                                  mid      = "white",
#                                                  na.value = scales::muted("slategray4"),
#                                                  midpoint = barValue*1.1, #mean(meltRP$value, na.rm = TRUE),
#                                                  limit    = c(min(meltRP$value, na.rm = TRUE),max(meltRP$value, na.rm = TRUE)),
#                                                  space    = "Lab",
#                                                  name     = "")
#
#     }
#
#     ## RadiusRRbar ----
#     if(plotRadiusRRbar){
#       # Create a custom legend ---
#       distrange  <- round(seq(0,max(RM,na.rm = TRUE),length.out=7),2)
#       resol      <- sort(unique(round(as.vector(RM),2)))
#       if(length(resol)<7){
#         resol <- distrange
#       }
#       if(length(resol)>100){
#         resol <- round(seq(0,max(RM,na.rm = TRUE),length.out=100),2)
#       }
#       resol <- resol %>% tibble::as_tibble() %>% dplyr::mutate(y= seq(exp(0),exp(1),length.out=NROW(resol)), x=0.5)
#       #resol <- resol[-1,]
#
#       distrange <- plyr::ldply(c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5), function(t){
#         suppressWarnings(est_radius(RM,targetValue = t,silent = TRUE, maxIter = 100, radiusOnFail = "percentile"))
#       })
#       #ldply(distrange[2:6],function(d) cbind(epsilon=d,RR=crqa_rp(RM = RM, emRad = d)$RR))
#
#
#       RecScale <- data.frame(RR=distrange$Measure,epsilon=distrange$Radius)
#       RecScale <- RecScale %>%
#         dplyr::add_row(epsilon=mean(c(0,distrange$Radius[1])),RR=mean(c(0,distrange$Measure[1])),.before = 1) %>%
#         dplyr::add_row(epsilon=max(RM),RR=1)
#
#
#       resol$y <- elascer(x = resol$y,lo = min(log(RecScale$RR),na.rm = TRUE), hi = max(log(RecScale$RR),na.rm = TRUE))
#       #resol$value <- log(resol$value)
#       resol <- resol[-1,]
#
#
#       if(!is.na(radiusValue)){
#         barValue <- round(RecScale$RR[which(round(RecScale$epsilon,4)>=radiusValue)[1]],4)
#         barValue <- resol$value[which(resol$y>=log(barValue))[1]]
#       } else {
#         barValue <- mean(meltRP$value, na.rm = TRUE)
#       }
#
#       gDist <-  ggplot2::ggplot(resol,ggplot2::aes_(x=~x,y=~y,fill=~value)) +
#         ggplot2::geom_tile(show.legend = FALSE) +
#         ggplot2::scale_y_continuous(name = "Recurrence Rate", breaks = log(RecScale$RR), labels = paste(round(RecScale$RR,3)), sec.axis = dup_axis(name=expression(paste("recurrence threshold",~ epsilon)), labels = paste(round(RecScale$epsilon,2)))) +
#         ggplot2::scale_fill_gradient2(low      = "red3",
#                                       high     = "steelblue",
#                                       mid      = "white",
#                                       na.value = scales::muted("slategray4"),
#                                       midpoint =  barValue * 1.1,#mean(meltRP$value, na.rm = TRUE),
#                                       #limit    = c(min(meltRP$value, na.rm = TRUE),max(meltRP$value, na.rm = TRUE)),
#                                       space    = "Lab",
#                                       name     = "") +
#         ggplot2::coord_equal(1, expand = FALSE) +
#         ggplot2::theme_bw() +
#         ggplot2::theme(panel.background = element_blank(),
#                        panel.grid.major  = element_blank(),
#                        panel.grid.minor  = element_blank(),
#                        legend.background = element_blank(),
#                        legend.key = element_blank(),
#                        panel.border = element_blank(),
#                        axis.text.y  =  element_text(size = 8),
#                        # axis.text.y.left  =  element_text(size = 8),
#                        # axis.text.y.right =  element_text(size = 8),
#                        axis.text.x  = element_blank(),
#                        axis.ticks.x = element_blank(),
#                        axis.title.x = element_blank(),
#                        axis.title.y = element_text(hjust = 0, size = 10),
#                        # axis.title.y.left = element_text(hjust = 0, size = 10),
#                        # axis.title.y.right = element_text(hjust = 0, size = 10),
#                        plot.margin = margin(0,5,0,5, unit = "pt"))
#     }
#
#   } else { # unthresholded
#
#
#
#   }
#
#   rptheme <- ggplot2::theme_bw() + ggplot2::theme(panel.background = element_blank(),
#                                                   panel.grid.minor  = element_blank(),
#                                                   panel.border = element_rect("grey50",fill=NA),
#                                                   legend.key = element_rect(colour = "grey90"),
#                                                   axis.ticks = element_blank(),
#                                                   axis.text = element_blank(),
#                                                   plot.margin = margin(0,0,0,0))
#
#   if(drawGrid){
#     rptheme <- rptheme +  theme(panel.grid.major  = element_line("grey70",size = .1),
#                                 panel.ontop = TRUE)
#   } else {
#     rptheme <- rptheme +  theme(panel.grid.major  = element_blank(),
#                                 panel.ontop = FALSE)
#   }
#
#   if(showL){
#     rptheme <- rptheme +  theme(legend.position = c(1.1,1),
#                                 legend.direction = "vertical",
#                                 legend.background = element_rect(fill="grey90"),
#                                 legend.justification = "top")
#   }
#
#   if(plotDimensions){
#     rptheme <- rptheme + theme(axis.title.y =element_blank(),
#                                axis.title.x =element_blank())
#   }
#
#
#   if(!is.null(markEpochsLOI)){
#     if(!unthresholded){
#       gRP <- gRP +  ggplot2::scale_fill_manual(name  = "Key:",
#                                                values = colvec,
#                                                na.translate = TRUE ,
#                                                na.value = scales::muted("slategray4"),
#                                                guide = "legend",
#                                                limits = levels(meltRP$value))
#     }
#     # theme(panel.ontop = TRUE,
#     #       legend.position = "top",
#     #       legend.background = element_rect(colour =  "grey50"))
#     #geom_line(data = Edata, aes_(x=~x,y=~y,colour = ~value), size = 2, show.legend = TRUE) +
#
#   } else {
#     if(!unthresholded){
#       gRP <- gRP +  ggplot2::scale_fill_manual(name  = "", breaks = c(0,1),
#                                                values = colvec,
#                                                na.translate = TRUE ,
#                                                na.value = scales::muted("slategray4"),
#                                                guide = "none")
#     }
#   }
#
#   # if(!is.null(markEpochsGrid)){
#   #   if(is.list(markEpochsGrid)&length(markEpochsGrid)==2){
#   #
#   #     markEpochsGrid[[1]] <- factor(markEpochsGrid[[1]])
#   #     markEpochsGrid[[2]] <- factor(markEpochsGrid[[2]])
#   #
#   #     cpalV <- paletteer::paletteer_d(package = "rcartocolor",palette = "Safe", n = nlevels(markEpochsGrid[[1]]))
#   #     names(cpalV) <- levels(markEpochsGrid[[1]])
#   #
#   #     dataV <- data.frame(t= as.numeric_character(markEpochsGrid[[1]]), xb=markEpochsGrid[[1]],col=NA)
#   #     for(c in unique(dataV$t)){
#   #       dataV$col[dataV$t%in%c] <- cpalV[c]
#   #     }
#   #
#   #     cpalH <- paletteer::paletteer_d(package = "rcartocolor",palette = "Vivid", n = nlevels(markEpochsGrid[[2]]))
#   #     names(cpalH) <- levels(markEpochsGrid[[2]])
#   #
#   #     dataH <- data.frame(t= as.numeric_character(markEpochsGrid[[2]]), yb=markEpochsGrid[[2]],col=NA)
#   #     for(c in unique(dataV$t)){
#   #       dataH$col[dataH$t%in%c] <- cpalH[c]
#   #     }
#   #
#   #       gRP <- gRP +
#   #         #geom_vline(data = dataV, aes_(xintercept = diff(c((max(~t, na.rm = TRUE)+1),~xb)!=0)), colour = dataV$col) +
#   #         #geom_hline(data = dataH, aes_(yintercept = diff(c((max(~t, na.rm = TRUE)+1),~yb)!=0)), colour = dataH$col) +
#   #         ggplot2::geom_vline(data = dataV, aes(xintercept = diff(c((max(t)+1),t))!=0), colour = dataV$col) +
#   #         ggplot2::geom_hline(data = dataH, aes(yintercept = diff(c((max(t)+1),t))!=0), colour = dataH$col) +
#   #         ggplot2::theme(panel.ontop = TRUE)
#   #
#   #     } else {
#   #       warning("Variable passed to 'markEpochsGrid' is not a list, and/or is not of length 2.")
#   #     }
#   #   }
#
#   if(plyr::is.discrete(meltRP$Var1)){
#     gRP <- gRP + ggplot2::scale_x_discrete(breaks=meltRP$Var1,expand = c(0,0))
#   } else {
#     gRP <- gRP + ggplot2::scale_x_continuous(breaks=meltRP$Var1,expand = c(0,0))
#   }
#   if(plyr::is.discrete(meltRP$Var2)){
#     gRP <- gRP + ggplot2::scale_y_discrete(breaks=meltRP$Var2,expand = c(0,0))
#   } else {
#     gRP <- gRP + ggplot2::scale_y_continuous(breaks=meltRP$Var2,expand = c(0,0))
#   }
#   gRP <- gRP + rptheme + ggplot2::coord_equal(dim(RM)[1]/dim(RM)[2]) #coord_fixed(expand = FALSE)
#
#   gy1 <- gg_plotHolder()
#   gy2 <- gg_plotHolder()
#
#   xdims <- ""
#   ydims <- ""
#
#   if(!is.null(attr(RM,"emDims1.name"))|nchar(xlabel)>0){
#     xdims <- ifelse(nchar(xlabel)>0, xlabel, attr(RM,"emDims1.name"))
#   }
#   if(!is.null(attr(RM,"emDims2.name"))|nchar(ylabel)>0){
#     ydims <- ifelse(nchar(ylabel)>0, ylabel, attr(RM,"emDims2.name"))
#     if(AUTO){
#       ydims <- xdims
#     }
#   }
#
#   if(plotDimensions){
#
#     if(!is.null(attr(RM,"emDims1"))){
#
#       gRP <- gRP + ylab("") + xlab("")
#
#       y1 <- data.frame(t1=attr(RM,"emDims1"))
#       y2 <- data.frame(t2=attr(RM,"emDims2"))
#
#       # Y1
#
#       colnames(y1) <- paste0("X",1:NCOL(y1))
#       y1$tm  <- 1:NROW(y1)
#       y1$tmna <- 0
#       y1$tmna[is.na(y1[,1])] <- y1$tm[is.na(y1[,1])]
#       y1 <- tidyr::gather(y1,key="Dimension",value = "Value", -c("tm","tmna"))
#       y1$Value <-  elascer(y1$Value)
#
#       # Y2
#       colnames(y2) <- paste0("Y",1:NCOL(y2))
#       y2$tm <- 1:NROW(y2)
#       y2$tmna <- 0
#       y2$tmna[is.na(y2[,1])] <- y2$tm[is.na(y2[,1])]
#       y2 <- tidyr::gather(y2,key="Dimension",value = "Value", -c("tm","tmna"))
#       y2$Value <-  elascer(y2$Value)
#
#
#     } else {
#       if(unthresholded){
#         y1 <- data.frame(Value=diag(RM),x=1:length(diag(RM)), Dimension = rep("LOS",length(diag(RM))))
#         y2 <- data.frame(Value=diag(RM),x=1:length(diag(RM)), Dimension = rep("LOS",length(diag(RM))))
#         xdims <- paste("LOS",xdims)
#         ydims <- paste("LOS",ydims)
#       } else {
#         plotDimensions <- FALSE
#       }
#     }
#   } else {
#     gRP <- gRP + ylab(ydims) + xlab(xdims)
#   }
#
#   if(plotDimensions){
#
#     gy1 <- ggplot2::ggplot(y1, ggplot2::aes_(y=~Value, x= ~tm,  group= ~Dimension)) +
#       ggplot2::geom_line(aes_(colour=~Dimension), show.legend = FALSE) +
#       ggplot2::xlab(xdims) + ggplot2::ylab(" ") +
#       ggplot2::geom_vline(ggplot2::aes_(xintercept = ~tmna), colour = scales::muted("slategray4"),alpha=.1, size=.5) +
#       ggplot2::scale_color_grey() +
#       ggplot2::scale_x_continuous(expand = c(0,0)) +
#       ggplot2::scale_y_continuous(expand = c(0,0)) +
#       ggplot2::theme(panel.background = element_blank(),
#                      panel.grid.major  = element_blank(),
#                      panel.grid.minor  = element_blank(),
#                      legend.background = element_blank(),
#                      legend.key = element_blank(),
#                      panel.border = element_blank(),
#                      axis.text = element_blank(),
#                      axis.line = element_blank(),
#                      axis.ticks = element_blank(),
#                      axis.title.x =element_text(colour = "black",angle = 0, vjust = +3),
#                      axis.title.y =element_blank(),
#                      plot.margin = margin(0,0,0,0, unit = "pt")) +
#       ggplot2::coord_cartesian(expand = FALSE)  # +  coord_fixed(1/10)
#
#     gy2 <- ggplot2::ggplot(y2, ggplot2::aes_(y=~Value, x=~tm, group=~Dimension)) +
#       ggplot2::geom_line(ggplot2::aes_(colour=~Dimension), show.legend = FALSE) +
#       ggplot2::ylab(" ") + ggplot2::xlab(ydims) +
#       ggplot2::geom_vline(ggplot2::aes_(xintercept = ~tmna), colour = scales::muted("slategray4"),alpha=.1, size=.5) +
#       ggplot2::scale_color_grey() +
#       ggplot2::scale_x_continuous(expand = c(0,0)) +
#       ggplot2::theme(panel.background = element_blank(),
#                      panel.grid.major  = element_blank(),
#                      panel.grid.minor  = element_blank(),
#                      legend.background = element_blank(),
#                      legend.key = element_blank(),
#                      panel.border = element_blank(),
#                      axis.text = element_blank(),
#                      axis.line = element_blank(),
#                      axis.ticks = element_blank(),
#                      axis.title.x =element_blank(),
#                      axis.title.y =element_text(colour = "black",angle = 90, vjust = -2),
#                      plot.margin = margin(0,0,0,0, unit = "pt")) +
#       ggplot2::coord_flip(expand = FALSE) +
#       ggplot2::scale_y_reverse(expand = c(0,0))
#
#   } # plotdimensions
#
#
#   if(plotMeasures){
#
#     rpOUT    <- round(rpOUT,3)
#     if(is.na(rpOUT$emRad)){
#       rpOUT$Radius <- round(radiusValue,3)
#     } else {
#       rpOUT$Radius <- rpOUT$emRad
#     }
#
#
#     rpOUTdat <- rpOUT %>%
#       dplyr::select(dplyr::one_of(c("Radius","RP_N","RR","DET","MEAN_dl","ENT_dl","LAM_vl","TT_vl","ENT_vl"))) %>%
#       tidyr::gather(key="measure",value="value") %>%
#       dplyr::mutate(x=rep(0,9),y=9:1)
#
#     #rpOUTdat <- cbind(rpOUTdat,rpOUTdat)
#     rpOUTdat$label <-  paste0(rpOUTdat$measure,":\n",rpOUTdat$value)
#
#     gA <-ggplot2::ggplot(rpOUTdat,ggplot2::aes_(x=~x,y=~y)) +
#       ggplot2::geom_text(ggplot2::aes_(label=~label), family="mono", hjust="left", vjust="center", size=3, parse = FALSE) +
#       #scale_x_continuous(limits = c(0,.3)) +
#       ggplot2::theme_void() +
#       ggplot2::theme(plot.margin = margin(0,5,0,5, unit = "pt"))
#
#     #geom="text", label = paste("Radius:",rpOUT$Radius,"\nRec points:",rpOUT$RT,"\nRR",rpOUT$RR,"\nDET:",rpOUT$DET,"\nMEAN_dl:",rpOUT$MEAN_dl,"\nENT_dl:",rpOUT$ENT_dl,"\nLAM_vl:",rpOUT$LAM_vl, "\nTT_vl:",rpOUT$TT_vl,"\nENTR_vl:",rpOUT$ENT_vl)) + theme_minimal() + theme(text = element_text(family = "mono"))
#     # ,"\nLAM_hl:",rpOUT$LAM_vl, "| TT_hl:",rpOUT$TT_vl,"| ENTR_hl:",rpOUT$ENT_hl))
#   }
#
#   if(useGtable){
#
#     #gRP <- gRP #+ theme(panel.background = element_rect(colour="white"))
#
#     g <- ggplot2::ggplotGrob(gRP)
#
#     if(plotDimensions){
#       gry2<-ggplot2::ggplotGrob(gy2)
#       gry1<-ggplot2::ggplotGrob(gy1)
#     }
#
#     if(unthresholded&plotRadiusRRbar){
#       grDist <- ggplot2::ggplotGrob(gDist)
#     }
#
#     if(plotMeasures){
#       grA <- ggplot2::ggplotGrob(gA)
#     }
#
#
#     if(plotDimensions&!plotMeasures&unthresholded&plotRadiusRRbar){
#       mat <- matrix(list(gry2, grid::nullGrob(),g, gry1, grDist, grid::nullGrob()),nrow = 2)
#       gt  <- gtable::gtable_matrix("di_rp_dim", mat, widths = unit(c(.25, 1,.5), "null"), heights =  unit(c(1,.25), "null"),respect = TRUE)
#     }
#
#     if(plotDimensions&!plotMeasures&unthresholded&!plotRadiusRRbar){
#       mat <- matrix(list(gry2, grid::nullGrob(),g, gry1),nrow = 2)
#       gt  <- gtable::gtable_matrix("di_rp_dim", mat, widths = unit(c(.25, 1), "null"), heights =  unit(c(1,.25), "null"),respect = TRUE)
#     }
#
#     if(plotDimensions&!plotMeasures&!unthresholded){
#       mat <- matrix(list(gry2, grid::nullGrob(),g, gry1),nrow = 2)
#       gt  <- gtable::gtable_matrix("bi_rp_dim", mat, widths = unit(c(.25, 1), "null"), heights =  unit(c(1, .25), "null"),respect = TRUE)
#     }
#
#     if(plotDimensions&plotMeasures&unthresholded&plotRadiusRRbar){
#       mat <- matrix(list(grA, grid::nullGrob(), gry2, grid::nullGrob(),g, gry1, grDist, grid::nullGrob()),nrow = 2)
#       gt  <- gtable::gtable_matrix("di_rp_dim_meas", mat, widths = unit(c(.35,.25, 1,.5), "null"), heights =  unit(c(1,.25), "null"),respect = TRUE)
#     }
#
#     if(plotDimensions&plotMeasures&unthresholded&!plotRadiusRRbar){
#       mat <- matrix(list(grA, grid::nullGrob(), gry2, grid::nullGrob(),g, gry1),nrow = 2)
#       gt  <- gtable::gtable_matrix("di_rp_dim_meas", mat, widths = unit(c(.35,.25, 1), "null"), heights =  unit(c(1,.25), "null"),respect = TRUE)
#     }
#
#     if(plotDimensions&plotMeasures&!unthresholded){
#       mat <- matrix(list(grA, grid::nullGrob(), gry2, grid::nullGrob(),g, gry1),nrow = 2)
#       gt  <- gtable::gtable_matrix("bi_rp_dim_meas", mat, widths = unit(c(.35,.25, 1), "null"), heights =  unit(c(1,.25), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&plotMeasures&unthresholded&plotRadiusRRbar){
#       mat <- matrix(list(grA, g, grDist),nrow = 1)
#       gt  <- gtable::gtable_matrix("di_rp_meas", mat, widths = unit(c(.35, 1,.5), "null"), heights =  unit(c(1), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&plotMeasures&unthresholded&!plotRadiusRRbar){
#       mat <- matrix(list(grA, g),nrow = 1)
#       gt  <- gtable::gtable_matrix("di_rp_meas", mat, widths = unit(c(.35, 1), "null"), heights =  unit(c(1), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&plotMeasures&!unthresholded){
#       mat <- matrix(list(grA, g),nrow = 1)
#       gt  <- gtable::gtable_matrix("bi_rp_meas", mat, widths = unit(c(.35, 1), "null"), heights =  unit(c(1), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&!plotMeasures&unthresholded&plotRadiusRRbar){
#       mat <- matrix(list(g, grDist),nrow = 1)
#       gt  <- gtable::gtable_matrix("di_rp", mat, widths = unit(c(1,.5), "null"), heights =  unit(c(1), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&!plotMeasures&unthresholded&!plotRadiusRRbar){
#       mat <- matrix(list(g),nrow = 1)
#       gt  <- gtable::gtable_matrix("di_rp", mat, widths = unit(c(1), "null"), heights =  unit(c(1), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&!plotMeasures&!unthresholded){
#       mat <- matrix(list(g),nrow = 1)
#       gt  <- gtable::gtable_matrix("bi_rp", mat, widths = unit(c(1), "null"), heights =  unit(c(1), "null"),respect = TRUE)
#     }
#
#     # gindex <- subset(g$layout, name == "layout")
#     # g <- gtable::gtable_add_cols(g, grid::unit(.5, "grobwidth", data = g),0)
#     # g <- gtable::gtable_add_grob(g, ggplot2::ggplotGrob(gA), t=gindex$t, l=1, b=gindex$b, r=gindex$l)
#     #rm(gindex)
#     #}
#
#     if(nchar(title)>0){
#       grT <- ggplot2::ggplot(data.frame(x=1,y=1)) +
#         ggplot2::geom_text(ggplot2::aes_(x=~x,y=~y), label=title) +
#         theme_void() +
#         theme(plot.margin = margin(0,0,0,0, unit = "pt"))
#       gt  <- gtable::gtable_add_rows(x = gt, heights =  unit(c(.1),"null"), pos=0)
#       l <- sum(c(plotDimensions,plotMeasures))+1
#       gt  <- gtable::gtable_add_grob(x = gt, grobs = ggplot2::ggplotGrob(grT), name = "Title", t=1, l=l)
#     }
#
#     #  gt <- gtable::gtable_add_col_space(gt,)
#
#     g <- gtable::gtable_add_padding(gt, unit(5, "pt"))
#
#   }
#
#
#   # else {
#   #
#   #   if(plotDimensions){
#   #
#   #     if(unthresholded){
#   #       g <- (gy2 + gRP + gDist + gg_plotHolder() + gy1 + gg_plotHolder() +
#   #               patchwork::plot_layout(nrow = 2, ncol = 3, widths = c(1,10,1), heights = c(10,1)) + patchwork::plot_annotation(title = title, caption = ifelse(AUTO,"Auto-recurrence plot","Cross-recurrence plot")))
#   #
#   #     } else {
#   #
#   #       if(plotMeasures){
#   #         g <- (gy2 + gRP + gA + gg_plotHolder() + gy1 + gg_plotHolder() +
#   #                 patchwork::plot_layout(nrow = 2, ncol = 3, widths = c(1,9,2), heights = c(10,1)) + patchwork::plot_annotation(title = title, caption = ifelse(AUTO,"Auto-recurrence plot","Cross-recurrence plot")))
#   #       } else {
#   #         g <- (gy2 + gRP + gg_plotHolder() + gg_plotHolder() + gy1 + gg_plotHolder() +
#   #                 patchwork::plot_layout(nrow = 2, ncol = 3, widths = c(1,9,2), heights = c(10,1)) + patchwork::plot_annotation(title = title, caption = ifelse(AUTO,"Auto-recurrence plot","Cross-recurrence plot")))
#   #       }
#   #     }
#   #
#   #   } else {
#   #     g <- gRP
#   #   }
#   # } # use gtable
#
#   if(!returnOnlyObject){
#     if(useGtable){
#       grid::grid.newpage()
#       grid::grid.draw(g)
#     } else {
#       # graphics::plot.new()
#       graphics::plot(g)
#     }
#   }
#   return(invisible(g))
# }
#
#
# # Plot (thresholded) distance matrix
# #
# # @param RM A distance matrix or recurrence matrix
# # @param plotDimensions Should the state vectors be plotted if they are available as attributes of RM (default = \code{TRUE})
# # @param plotMeasures Print common (C)RQA measures in the plot if the matrix is binary
# # @param plotRadiusRRbar The \code{Radius-RR-bar} is a colour-bar guide plotted with an unthresholded distance matrix indicating a number of \code{RR} values one would get if a certain distance threshold were chosen (\code{default = TRUE})
# # @param markEpochsLOI Pass a factor whose levels indicate different epochs or phases in the time series and use the line of identity to represent the levels by different colours (\code{default = NULL})
# # @param markEpochsGrid Pass a list 2 numeric vectors, \code{markEpochsGrid[[1]]} should have length equal to NROW(RM) and \code{markEpochsGrid[[2]]} should have length equal to NCOL(RM). The values in the vectors represent different epochs associated with each time stamp, the change index will become 'breaks' on the \code{y} and \code{x} axis respectively (\code{default = NULL})
# # @param radiusValue If \code{plotMeasures = TRUE} and RM is an unthresholded matrix, this value will be used to calculate recurrence measures. If \code{plotMeasures = TRUE} and RM is already a binary recurence matrix, pass the radius that was used as a threshold to create the matrix for display purposes. If \code{plotMeasures = TRUE} and \code{radiusValue = NA}, function \code{crqa_radius()} will be called with default settings (find a radius that yields .05 recurrence rate). If \code{plotMeasures = FALSE} this setting will be ignored.
# # @param title A title for the plot
# # @param xlab An x-axis label
# # @param ylab An y-axis label
# # @param plotSurrogate Should a 2-panel comparison plot based on surrogate time series be added? If \code{RM} has attributes \code{y1} and \code{y2} containing the time series data (i.e. it was created by a call to \code{\link{rp}}), the following options are available: "RS" (random shuffle), "RP" (randomised phases), "AAFT" (amplitude adjusted fourier transform). If no timeseries data is included, the columns will be shuffled.  NOTE: This is not a surrogate test, just 1 surrogate is created from \code{y1}.
# # @param weighted If this is a weighted matrix, treat it as an unthresholded matrix (default = \code{FALSE})
# # @param returnOnlyObject Return the ggplot object only, do not draw the plot (default = \code{TRUE})
# # @param useGtable Use package \code{\link{gtable}}. If this results in errors (e.g. viewport settings), set set to FALSE to use package \code{patchwork}. This package is in development, see the warning for instructions on how to install it.
# #
# # @return A nice plot of the recurrence matrix.
# # @export
# #
# # @family Distance matrix operations
# #
# rp_plot <- function(RM, plotDimensions= FALSE, plotMeasures = FALSE, plotRadiusRRbar = TRUE, markEpochsLOI = NULL, markEpochsGrid = NULL, radiusValue = NA, title = "", xlab = "", ylab="", plotSurrogate = NA, weighted = FALSE, returnOnlyObject = FALSE, useGtable = TRUE){
#
#   # # check patchwork
#   # if(!length(find.package("patchwork",quiet = TRUE))>0){
#   #   warning("Package patchwork is not installed...\n1. Install Xcode from App Store (MacOS) or rwintools.exe from CRAN (Windows) \n2. Install patchwork: devtools::install_github('thomasp85/patchwork')\n3. Install casnet: devtools::install_github('FredHasselman/casnet')\n....Using gtable instead, with limited options\n")
#   #   useGtable=TRUE
#   # }
#
#
#   # check auto-recurrence and make sure Matrix has sparse triplet representation
#   if(is.null(attr(RM,"AUTO"))){
#     RM   <- rp_checkfix(RM, checkAUTO = TRUE)
#   }
#
#   AUTO <- attr(RM,"AUTO")
#
#   if(is.null(attr(RM,"weighted"))){
#     attr(RM,"weighted") <- weighted
#   }
#
#   # prepare data
#   if(attr(RM,"package")%00%""%in%"Matrix"){
#     RM     <- rp_checkfix(RM, checkTSPARSE = TRUE, fixTSPARSE = TRUE)
#     meltRP <- data.frame(Var1 = (RM@i+1), Var2 = (RM@j+1), value = as.numeric(RM@x))
#   } else {
#     meltRP <- reshape2::melt(as.matrix(RM))
#   }
#
#   if(any(is.na(meltRP$value))){
#     meltRP$value[is.na(meltRP$value)] <- max(meltRP$value, na.rm = TRUE) + 1
#   }
#
#   # check unthresholded
#   if(!all(as.vector(meltRP$value[!is.na(meltRP$value)])==0|as.vector(meltRP$value[!is.na(meltRP$value)])==1)|weighted){
#     unthresholded <- TRUE
#   } else {
#     unthresholded <- FALSE
#     if(!is.null(attr(RM,"emRad"))){
#       radiusValue <- attr(RM,"emRad")
#     }
#     meltRP$value <- factor(meltRP$value, levels = c(0,1), labels = c("0","1"))
#   }
#
#   # Get CRQA measures
#   if(plotMeasures){
#     if(is.na(radiusValue)){
#       if(!is.null(attributes(RM)$emRad)){
#         radiusValue <- attr(RM,"emRad")
#       } else {
#         radiusValue <- crqa_radius(RM,silent = TRUE)$Radius
#       }
#     }
#     if(unthresholded){
#       rpOUT   <- crqa_rp(RM, emRad = radiusValue, AUTO = AUTO)
#     } else {
#       rpOUT   <- crqa_rp(RM, AUTO = AUTO)
#     }
#   }
#
#   # main plot
#   gRP <-  ggplot2::ggplot(ggplot2::aes_(x=~Var2, y=~Var1, fill = ~value), data= meltRP) +
#     ggplot2::geom_raster(hjust = 0, vjust=0,show.legend = FALSE) +
#     ggplot2::geom_abline(slope = 1,colour = "grey50", size = 1)
#   #ggtitle(label=title, subtitle = ifelse(AUTO,"Auto-recurrence plot","Cross-recurrence plot")) +
#
#
#   if(unthresholded){
#     gRP <- gRP + ggplot2::scale_fill_gradient2(low      = "red3",
#                                       high     = "steelblue",
#                                       mid      = "white",
#                                       na.value = scales::muted("slategray4"),
#                                       midpoint = mean(meltRP$value, na.rm = TRUE),
#                                       limit    = c(min(meltRP$value, na.rm = TRUE),max(meltRP$value, na.rm = TRUE)),
#                                       space    = "Lab",
#                                       name     = "")
#
#     rptheme <-     ggplot2::theme(panel.grid.major  = ggplot2::element_blank(),
#                          panel.grid.minor  = ggplot2::element_blank(),
#                          #panel.border = element_rect(colour = "grey50"),
#                          axis.ticks = ggplot2::element_blank(),
#                          axis.text = ggplot2::element_blank(),
#                          # axis.title.x = element_blank(),
#                          # axis.title.y = element_blank(),
#                          legend.position = "top",
#                          plot.margin = ggplot2::margin(0,0,0,0))
#
#     if(plotRadiusRRbar){
#       # Create a custom legend ---
#       distrange  <- round(seq(0,max(RM,na.rm = TRUE),length.out=7),2)
#       resol      <- sort(unique(round(as.vector(RM),2)))
#       if(length(resol)<7){
#         resol <- distrange
#       }
#       if(length(resol)>100){
#         resol <- round(seq(0,max(RM,na.rm = TRUE),length.out=100),2)
#       }
#       resol <- resol %>% tibble::as_tibble() %>% dplyr::mutate(y= seq(exp(0),exp(1),length.out=NROW(resol)), x=0.5)
#       #resol <- resol[-1,]
#
#       distrange <- plyr::ldply(c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5), function(t){
#         suppressWarnings(crqa_radius(RM,targetValue = t,silent = TRUE, maxIter = 100, radiusOnFail = "percentile"))
#       })
#       #ldply(distrange[2:6],function(d) cbind(epsilon=d,RR=crqa_rp(RM = RM, emRad = d)$RR))
#
#       RecScale <- data.frame(RR=distrange$Measure,epsilon=distrange$Radius)
#       RecScale <- RecScale %>%
#         dplyr::add_row(epsilon=mean(c(0,distrange$Radius[1])),RR=mean(c(0,distrange$Measure[1])),.before = 1) %>%
#         dplyr::add_row(epsilon=max(RM),RR=1)
#
#       resol$y <- elascer(x = resol$y,lo = min(log(RecScale$RR),na.rm = TRUE), hi = max(log(RecScale$RR),na.rm = TRUE))
#       #resol$value <- log(resol$value)
#       resol <- resol[-1,]
#
#       breaks <- RecScale$RR
#       if(any(round(diff(breaks))==0)){
#         id <- which(round(diff(breaks))==0)
#         breaks[id] <- (breaks[id] + mean(diff(breaks[id]),na.rm=TRUE))
#       }
#       breaks <- log(RecScale$RR)
#
#       form <- as.formula("~y ~ ~x")
#       gDist <-  ggplot2::ggplot(resol,ggplot2::aes_(x=~x,y=~y,fill=~value)) +
#         ggplot2::geom_tile(show.legend = FALSE) +
#         ggplot2::scale_y_continuous(name = "",
#                                     position = "right",
#                                     breaks = breaks,
#                                     labels = paste(round(RecScale$epsilon,3)," = ",round(RecScale$RR,2))
#                                     #sec.axis = ggplot2::dup_axis(trans = ~y ~ ~x, name="recurrence hreshold", labels = paste(round(RecScale$epsilon,2)))
#                                     ) + #expression(paste("recurrence threshold",~ epsilon))
#         ggplot2::scale_fill_gradient2(low      = "red3",
#                              high     = "steelblue",
#                              mid      = "white",
#                              na.value = scales::muted("slategray4"),
#                              midpoint = mean(resol$value, na.rm = TRUE),
#                              #limit    = c(min(meltRP$value, na.rm = TRUE),max(meltRP$value, na.rm = TRUE)),
#                              space    = "Lab",
#                              name     = "") +
#         ggplot2::coord_equal(1, expand = FALSE) +
#         ggplot2::ggtitle(label = "",subtitle = "Radius = Recurrence Rate") +
#         ggplot2::theme_bw() +
#         ggplot2::theme(panel.background = ggplot2::element_blank(),
#               panel.grid.major  = ggplot2::element_blank(),
#               panel.grid.minor  = ggplot2::element_blank(),
#               legend.background = ggplot2::element_blank(),
#               legend.key = ggplot2::element_blank(),
#               panel.border = ggplot2::element_blank(),
#               axis.text.y  =  ggplot2::element_text(size = 8),
#               # axis.text.y.left  =  element_text(size = 8),
#               # axis.text.y.right =  element_text(size = 8),
#               axis.text.x  = ggplot2::element_blank(),
#               axis.ticks.x = ggplot2::element_blank(),
#               axis.title.x = ggplot2::element_blank(),
#               axis.title.y = ggplot2::element_text(hjust = 0, size = 10),
#               # axis.title.y.left = element_text(hjust = 0, size = 10),
#               # axis.title.y.right = element_text(hjust = 0, size = 10),
#               plot.margin = ggplot2::margin(0,5,0,5, unit = "pt"))
#     }
#
#   } else { # unthresholded
#
#     gRP <- gRP +  ggplot2::scale_fill_manual(name  = "", breaks = c(0,1),
#                                     values = c("0"="white","1"="black"),
#                                     na.translate = TRUE , na.value = scales::muted("slategray4"), guide = "none")
#
#     rptheme <-  ggplot2::theme(
#       panel.background = ggplot2::element_blank(),
#       panel.border = ggplot2::element_rect("grey50",fill=NA),
#       panel.grid.major  = ggplot2::element_blank(),
#       #panel.grid.minor  = element_blank(),
#       legend.background = ggplot2::element_blank(),
#       legend.key = ggplot2::element_blank(),
#       axis.ticks = ggplot2::element_blank(),
#       axis.text = ggplot2::element_blank(),
#       legend.position = "top",
#       # axis.title.x =element_blank(),
#       # axis.title.y =element_blank(),
#       plot.margin = ggplot2::margin(0,0,0,0))
#   }
#
#
#   # Main plot
#
#   if(!is.null(markEpochsLOI)){
#     if(is.factor(markEpochsLOI)&length(markEpochsLOI)==max(c(NROW(RM),NCOL(RM)))){
#       #EcolI <- grey.colors(n=levels(markEpochsLOI))
#       gRP <- gRP + ggplot2::geom_abline(data = data.frame(EcolI = markEpochsLOI), ggplot2::aes_(colour = ~EcolI), slope = 1, size = 2, show.legend = TRUE)
#     } else {
#       warning("Variable passed to 'markEpochsLOI' is not a factor or doesn't have correct length.")
#     }
#   }
#
#   if(!is.null(markEpochsGrid)){
#     if(is.list(markEpochsGrid)&length(markEpochsGrid)==2){
#       gRP <- gRP +
#         ggplot2::geom_vline(data = data.frame(xb=markEpochsGrid[[1]]), ggplot2::aes_(xintercept = diff(c((max(~xb, na.rm = TRUE)+1),~xb)!=0))) +
#         ggplot2::geom_hline(data = data.frame(yb=markEpochsGrid[[2]]), ggplot2::aes_(yintercept = diff(c((max(~yb, na.rm = TRUE)+1),~yb)!=0)))
#     } else {
#       warning("Variable passed to 'markEpochsGrid' is not a list, and/or is not of length 2.")
#     }
#   }
#
#   if(plyr::is.discrete(meltRP$Var1)){
#     gRP <- gRP + ggplot2::scale_x_discrete(breaks=meltRP$Var1,expand = c(0,0))
#   } else {
#     gRP <- gRP + ggplot2::scale_x_continuous(breaks=meltRP$Var1,expand = c(0,0))
#   }
#   if(plyr::is.discrete(meltRP$Var2)){
#     gRP <- gRP + ggplot2::scale_y_discrete(breaks=meltRP$Var2,expand = c(0,0))
#   } else {
#     gRP <- gRP + ggplot2::scale_y_continuous(breaks=meltRP$Var2,expand = c(0,0))
#   }
#   gRP <- gRP + rptheme + ggplot2::coord_equal(dim(RM)[1]/dim(RM)[2]) #coord_fixed(expand = FALSE)
#
#   gy1 <- gg_plotHolder()
#   gy2 <- gg_plotHolder()
#
#   xdims <- ""
#   ydims <- ""
#
#   if(!is.null(attr(RM,"emDims1.name"))){
#     xdims <- ifelse(nchar(xlab)>0,xlab,attr(RM,"emDims1.name"))
#     if(AUTO){
#       ydims <- xdims
#     } else {
#       ydims <- ifelse(nchar(ylab)>0,ylab,attr(RM,"emDims2.name"))
#     }
#   }
#
#   if(nchar(xlab)>0){
#     xdims <- xlab
#   }
#   if(nchar(ylab)>0){
#     ydims <- ylab
#   }
#
#   gRP <- gRP + ggplot2::ylab(ydims) + ggplot2::xlab(xdims)
#
#   if(plotDimensions){
#
#     if(!is.null(attr(RM,"emDims1"))){
#
#       gRP <- gRP + ggplot2::ylab("") + ggplot2::xlab("")
#
#       y1 <- data.frame(t1=attr(RM,"emDims1"))
#       y2 <- data.frame(t2=attr(RM,"emDims2"))
#
#       # Y1
#
#       colnames(y1) <- paste0("X",1:NCOL(y1))
#       y1$tm  <- 1:NROW(y1)
#       y1$tmna <- 0
#       y1$tmna[is.na(y1[,1])] <- y1$tm[is.na(y1[,1])]
#       y1 <- tidyr::gather(y1,key="Dimension",value = "Value", -c("tm","tmna"))
#       y1$Value <-  elascer(y1$Value)
#
#       # Y2
#       colnames(y2) <- paste0("Y",1:NCOL(y2))
#       y2$tm <- 1:NROW(y2)
#       y2$tmna <- 0
#       y2$tmna[is.na(y2[,1])] <- y2$tm[is.na(y2[,1])]
#       y2 <- tidyr::gather(y2,key="Dimension",value = "Value", -c("tm","tmna"))
#       y2$Value <-  elascer(y2$Value)
#
#
#     } else {
#       if(unthresholded){
#         y1 <- data.frame(Value=diag(RM),x=1:length(diag(RM)), Dimension = rep("LOS",length(diag(RM))))
#         y2 <- data.frame(Value=diag(RM),x=1:length(diag(RM)), Dimension = rep("LOS",length(diag(RM))))
#         xdims <- paste("LOS",xdims)
#         ydims <- paste("LOS",ydims)
#       } else {
#         plotDimensions <- FALSE
#       }
#     }
#   }
#
#   if(plotDimensions){
#
#     gy1 <- ggplot2::ggplot(y1, ggplot2::aes_(y=~Value, x= ~tm,  group= ~Dimension)) +
#       ggplot2::geom_line(ggplot2::aes_(colour=~Dimension), show.legend = FALSE) +
#       ggplot2::xlab(xdims) + ggplot2::ylab("") +
#       ggplot2::geom_vline(ggplot2::aes_(xintercept = ~tmna), colour = scales::muted("slategray4"),alpha=.1, size=.5) +
#       ggplot2::scale_color_grey() +
#       ggplot2::scale_x_continuous(expand = c(0,0)) +
#       ggplot2::scale_y_continuous(expand = c(0,0)) +
#       ggplot2::theme(panel.background = ggplot2::element_blank(),
#             panel.grid.major  = ggplot2::element_blank(),
#             panel.grid.minor  = ggplot2::element_blank(),
#             legend.background = ggplot2::element_blank(),
#             legend.key = ggplot2::element_blank(),
#             panel.border = ggplot2::element_blank(),
#             axis.text = ggplot2::element_blank(),
#             axis.ticks = ggplot2::element_blank(),
#             axis.title.x = ggplot2::element_text(colour = "black",angle = 0, vjust = +3),
#             axis.title.y = ggplot2::element_blank(),
#             plot.margin = ggplot2::margin(0,0,0,0, unit = "pt")) +
#       ggplot2::coord_cartesian(expand = FALSE)  # +  coord_fixed(1/10)
#
#     gy2 <- ggplot2::ggplot(y2, ggplot2::aes_(y=~Value, x=~tm, group=~Dimension)) +
#       ggplot2::geom_line(ggplot2::aes_(colour=~Dimension), show.legend = FALSE) +
#       ggplot2::ylab("") + ggplot2::xlab(ydims) +
#       ggplot2::geom_vline(ggplot2::aes_(xintercept = ~tmna), colour = scales::muted("slategray4"),alpha=.1, size=.5) +
#       ggplot2::scale_color_grey() +
#       ggplot2::scale_x_continuous(expand = c(0,0)) +
#       ggplot2::theme(panel.background = ggplot2::element_blank(),
#             panel.grid.major  = ggplot2::element_blank(),
#             panel.grid.minor  = ggplot2::element_blank(),
#             legend.background = ggplot2::element_blank(),
#             legend.key = ggplot2::element_blank(),
#             panel.border = ggplot2::element_blank(),
#             axis.text = ggplot2::element_blank(),
#             axis.ticks = ggplot2::element_blank(),
#             axis.title.x =ggplot2::element_blank(),
#             axis.title.y =ggplot2::element_text(colour = "black",angle = 90, vjust = -2),
#             plot.margin = ggplot2::margin(0,0,0,0, unit = "pt")) +
#       ggplot2::coord_flip(expand = FALSE) +
#       ggplot2::scale_y_reverse(expand = c(0,0))
#
#   } # plotdimensions
#
#
#   if(plotMeasures){
#
#     rpOUT    <- round(rpOUT,3)
#     if(is.na(rpOUT$emRad)){
#       rpOUT$emRad <- round(radiusValue,3)
#     }
#
#     rpOUTdat <- rpOUT %>%
#       dplyr::select(dplyr::one_of(c("Radius","RP_N","RR","DET","MEAN_dl","ENT_dl","LAM_vl","TT_vl","ENT_vl"))) %>%
#       tidyr::gather(key="measure",value="value") %>%
#       dplyr::mutate(x=rep(0,9),y=9:1)
#
#     rpOUTdat <- cbind(rpOUTdat,rpOUTdat)
#     rpOUTdat$label <-  paste0(rpOUTdat$measure,":\n",rpOUTdat$value)
#
#     gA <-ggplot2::ggplot(rpOUTdat,ggplot2::aes_(x=~x,y=~y)) +
#       ggplot2::geom_text(ggplot2::aes_(label=~label), family="mono", hjust="left", vjust="center", size=3, parse = FALSE) +
#       #scale_x_continuous(limits = c(0,.3)) +
#       ggplot2::theme_void() +
#       ggplot2::theme(plot.margin = ggplot2::margin(0,5,0,5, unit = "pt"))
#
#     #geom="text", label = paste("Radius:",rpOUT$Radius,"\nRec points:",rpOUT$RT,"\nRR",rpOUT$RR,"\nDET:",rpOUT$DET,"\nMEAN_dl:",rpOUT$MEAN_dl,"\nENT_dl:",rpOUT$ENT_dl,"\nLAM_vl:",rpOUT$LAM_vl, "\nTT_vl:",rpOUT$TT_vl,"\nENTR_vl:",rpOUT$ENT_vl)) + theme_minimal() + theme(text = element_text(family = "mono"))
#     # ,"\nLAM_hl:",rpOUT$LAM_vl, "| TT_hl:",rpOUT$TT_vl,"| ENTR_hl:",rpOUT$ENT_hl))
#   }
#
#   if(useGtable){
#
#     gRP <- gRP + ggplot2::theme(panel.background = ggplot2::element_rect(colour="white"))
#
#     g <- ggplot2::ggplotGrob(gRP)
#
#     if(plotDimensions){
#       gry2<-ggplot2::ggplotGrob(gy2)
#       gry1<-ggplot2::ggplotGrob(gy1)
#     }
#
#     if(unthresholded&plotRadiusRRbar){
#       grDist <- ggplot2::ggplotGrob(gDist)
#     }
#
#     if(plotMeasures){
#       grA <- ggplot2::ggplotGrob(gA)
#     }
#
#     if(plotDimensions&!plotMeasures&unthresholded&plotRadiusRRbar){
#       mat <- matrix(list(gry2, grid::nullGrob(),g, gry1, grDist, grid::nullGrob()),nrow = 2)
#       gt  <- gtable::gtable_matrix("di_rp_dim", mat, widths = ggplot2::unit(c(.25, 1,.5), "null"), heights =  ggplot2::unit(c(1,.25), "null"),respect = TRUE)
#     }
#
#     if(plotDimensions&!plotMeasures&unthresholded&!plotRadiusRRbar){
#       mat <- matrix(list(gry2, grid::nullGrob(),g, gry1),nrow = 2)
#       gt  <- gtable::gtable_matrix("di_rp_dim", mat, widths = ggplot2::unit(c(.25, 1), "null"), heights =  ggplot2::unit(c(1,.25), "null"),respect = TRUE)
#     }
#
#     if(plotDimensions&!plotMeasures&!unthresholded){
#       mat <- matrix(list(gry2, grid::nullGrob(),g, gry1),nrow = 2)
#       gt  <- gtable::gtable_matrix("bi_rp_dim", mat, widths = ggplot2::unit(c(.25, 1), "null"), heights =  ggplot2::unit(c(1, .25), "null"),respect = TRUE)
#     }
#
#     if(plotDimensions&plotMeasures&unthresholded&plotRadiusRRbar){
#       mat <- matrix(list(grA, grid::nullGrob(), gry2, grid::nullGrob(),g, gry1, grDist, grid::nullGrob()),nrow = 2)
#       gt<- gtable::gtable_matrix("di_rp_dim_meas", mat, widths = ggplot2::unit(c(.35,.25, 1,.5), "null"), heights =  ggplot2::unit(c(1,.25), "null"),respect = TRUE)
#     }
#
#     if(plotDimensions&plotMeasures&unthresholded&!plotRadiusRRbar){
#       mat <- matrix(list(grA, grid::nullGrob(), gry2, grid::nullGrob(),g, gry1),nrow = 2)
#       gt<- gtable::gtable_matrix("di_rp_dim_meas", mat, widths = ggplot2::unit(c(.35,.25, 1), "null"), heights =  ggplot2::unit(c(1,.25), "null"),respect = TRUE)
#     }
#
#     if(plotDimensions&plotMeasures&!unthresholded){
#       mat <- matrix(list(grA, grid::nullGrob(), gry2, grid::nullGrob(),g, gry1),nrow = 2)
#       gt<- gtable::gtable_matrix("bi_rp_dim_meas", mat, widths = ggplot2::unit(c(.35,.25, 1), "null"), heights =  ggplot2::unit(c(1,.25), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&plotMeasures&unthresholded&plotRadiusRRbar){
#       mat <- matrix(list(grA, g, grDist),nrow = 1)
#       gt<- gtable::gtable_matrix("di_rp_meas", mat, widths = ggplot2::unit(c(.35, 1,.5), "null"), heights =  ggplot2::unit(c(1), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&plotMeasures&unthresholded&!plotRadiusRRbar){
#       mat <- matrix(list(grA, g),nrow = 1)
#       gt<- gtable::gtable_matrix("di_rp_meas", mat, widths = ggplot2::unit(c(.35, 1), "null"), heights =  ggplot2::unit(c(1), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&plotMeasures&!unthresholded){
#       mat <- matrix(list(grA, g),nrow = 1)
#       gt<- gtable::gtable_matrix("bi_rp_meas", mat, widths = ggplot2::unit(c(.35, 1), "null"), heights =  ggplot2::unit(c(1), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&!plotMeasures&unthresholded&plotRadiusRRbar){
#       mat <- matrix(list(g, grDist),nrow = 1)
#       gt<- gtable::gtable_matrix("di_rp", mat, widths = ggplot2::unit(c(1,.5), "null"), heights =  ggplot2::unit(c(1), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&!plotMeasures&unthresholded&!plotRadiusRRbar){
#       mat <- matrix(list(g),nrow = 1)
#       gt<- gtable::gtable_matrix("di_rp", mat, widths = ggplot2::unit(c(1), "null"), heights =  ggplot2::unit(c(1), "null"),respect = TRUE)
#     }
#
#     if(!plotDimensions&!plotMeasures&!unthresholded){
#       mat <- matrix(list(g),nrow = 1)
#       gt<- gtable::gtable_matrix("bi_rp", mat, widths = ggplot2::unit(c(1), "null"), heights =  ggplot2::unit(c(1), "null"),respect = TRUE)
#     }
#
#     # gindex <- subset(g$layout, name == "layout")
#     # g <- gtable::gtable_add_cols(g, grid::unit(.5, "grobwidth", data = g),0)
#     # g <- gtable::gtable_add_grob(g, ggplot2::ggplotGrob(gA), t=gindex$t, l=1, b=gindex$b, r=gindex$l)
#     #rm(gindex)
#     #}
#
#     #  gt <- gtable::gtable_add_col_space(gt,)
#
#     g <- gtable::gtable_add_padding(gt, ggplot2::unit(5, "pt"))
#
#   } else {
#
#     if(plotDimensions){
#
#       if(unthresholded){
#         g <- (gy2 + gRP + gDist + gg_plotHolder() + gy1 + gg_plotHolder() +
#                 patchwork::plot_layout(nrow = 2, ncol = 3, widths = c(1,10,1), heights = c(10,1)) + patchwork::plot_annotation(title = title, caption = ifelse(AUTO,"Auto-recurrence plot","Cross-recurrence plot")))
#
#       } else {
#
#         if(plotMeasures){
#           g <- (gy2 + gRP + gA + gg_plotHolder() + gy1 + gg_plotHolder() +
#                   patchwork::plot_layout(nrow = 2, ncol = 3, widths = c(1,9,2), heights = c(10,1)) + patchwork::plot_annotation(title = title, caption = ifelse(AUTO,"Auto-recurrence plot","Cross-recurrence plot")))
#         } else {
#           g <- (gy2 + gRP + gg_plotHolder() + gg_plotHolder() + gy1 + gg_plotHolder() +
#                   patchwork::plot_layout(nrow = 2, ncol = 3, widths = c(1,9,2), heights = c(10,1)) + patchwork::plot_annotation(title = title, caption = ifelse(AUTO,"Auto-recurrence plot","Cross-recurrence plot")))
#         }
#       }
#
#     } else {
#       g <- gRP
#     }
#   } # use gtable
#
#
#   if(!returnOnlyObject){
#     if(useGtable){
#       grid::grid.newpage()
#       grid::grid.draw(g)
#     } else {
#       graphics::plot.new()
#       graphics::plot(g)
#     }
#   }
#   return(invisible(g))
# }
#
#
# # rp_size
# #
# # @param mat A Matrix object
# # @param AUTO Is the Matrix an Auto Recurrence Matrix? If so, the length of the diagonal will be subtracted from the matrix size, pass \code{FALSE} to prevent this behaviour. If \code{NULL} (default) \code{AUTO} will take on the value of \code{isSymmetric(mat)}.
# # @param theiler Should a Theiler window be applied?
# #
# # @return Matrix size for computation of recurrence measures.
# # @export
# #
# # @family Distance matrix operations
# #
# # @examples
# # # Create a 10 by 10 matrix
# # library(Matrix)
# # m <- Matrix(rnorm(10),10,10)
# #
# # rp_size(m,TRUE,0)   # Subtract diagonal
# # rp_size(m,FALSE,0)  # Do not subtract diagonal
# # rp_size(m,NULL,0)   # Matrix is symmetrical, AUTO is set to TRUE
# # rp_size(m,NULL,1)   # Subtract a Theiler window of 1 around and including the diagonal
# rp_size <- function(mat, AUTO=NULL, theiler = NULL){
#   if(is.null(AUTO)){
#     AUTO <- Matrix::isSymmetric(unname(mat))
#   }
#   if(is.null(theiler)){
#     if(!is.null(attributes(mat)$theiler)){
#       theiler <- attr(mat,"theiler")
#     } else {
#       theiler <- 0
#     }
#   }
#   return(cumprod(dim(mat))[2] - ifelse((AUTO&theiler==0),length(Matrix::diag(mat)),
#                                        ifelse(theiler>0,Matrix::nnzero(Matrix::band(mat,-theiler,theiler)),0)))
# }
#
#
# # Empty results vector
# #
# # @return an empty crqa_rp
# # @keywords internal
# # @export
# #
# crqa_rp_empty <- function(){
#   data.frame(
#     emRad   = NA,
#     RP_N     = NA,
#     RR       = NA,
#     DET      = NA,
#     MEAN_dl  = NA,
#     MAX_dl   = NA,
#     ENT_dl   = NA,
#     ENTrel_dl= NA,
#     REP_av  = NA,
#     CoV_dl   = NA,
#     DIV_dl   = NA,
#     SING_dl  = NA,
#     N_dl     = NA,
#     ANI      = NA,
#     LAM_vl   = NA,
#     TT_vl    = NA,
#     MAX_vl   = NA,
#     ENT_vl   = NA,
#     ENTrel_vl= NA,
#     CoV_vl   = NA,
#     REP_vl   = NA,
#     DIV_vl   = NA,
#     SING_vl  = NA,
#     N_vl     = NA,
#     LAM_hl   = NA,
#     TT_hl    = NA,
#     MAX_hl   = NA,
#     ENT_hl   = NA,
#     ENTrel_hl= NA,
#     CoV_hl   = NA,
#     REP_hl   = NA,
#     DIV_hl   = NA,
#     SING_hl  = NA,
#     N_hl     = NA)
# }
#
# # crqa_rp_calc
# #
# # @param RM RM
# # @param emRad r
# # @param DLmin d
# # @param VLmin v
# # @param HLmin h
# # @param DLmax dd
# # @param VLmax vv
# # @param HLmax hh
# # @param AUTO a
# # @param chromatic c
# # @param matrices m
# #
# # @return crqa measures
# # @export
# # @keywords internal
# #
# crqa_rp_calc <- function(RM,
#                          emRad = NULL,
#                          DLmin = 2,
#                          VLmin = 2,
#                          HLmin = 2,
#                          DLmax = length(Matrix::diag(RM))-1,
#                          VLmax = length(Matrix::diag(RM))-1,
#                          HLmax = length(Matrix::diag(RM))-1,
#                          theiler = NULL,
#                          AUTO      = NULL,
#                          chromatic = FALSE,
#                          matrices  = FALSE){
#
#   recmatsize <- rp_size(RM, AUTO=AUTO)
#
#   if(!is.null(attributes(RM)$emRad)){
#     emRad <- attributes(RM)$emRad
#   } else {
#     emRad <- NA
#   }
#
#   if(!all(as.vector(RM)==0|as.vector(RM)==1)){
#     if(!chromatic){
#       stop("Need a thresholded recurrence matrix")
#     } else {
#       stop("Chromatic not yet implemented")
#     }
#   }
#
#   #Total nr. recurrent points
#   RP_N <- Matrix::nnzero(RM, na.counted = FALSE)
#
#   minDiag <- 0
#   if(Matrix::isSymmetric(unname(RM))){
#     if(all(Matrix::diag(RM)==1)){
#       minDiag <- length(Matrix::diag(RM))
#     }
#   }
#
#   RP_N <- RP_N-minDiag
#
#   #Proportion recurrence / Recurrence Rate
#   RR <- RP_N/recmatsize
#
#   if(length(RR)==0){RR<-0}
#
#   if(RR==1){
#     warning("Everything is recurring!\nReturning empty vector")
#     return(crqa_rp_empty())
#   }
#
#   #Get line segments
#   lineSegments <- rp_lineDist(RM,
#                               DLmin = DLmin, DLmax = DLmax,
#                               VLmin = VLmin, VLmax = VLmax,
#                               HLmin = HLmin, HLmax = HLmax,
#                               theiler = theiler, AUTO = AUTO)
#
#   dlines <- lineSegments$diagonals.dist
#   vlines <- lineSegments$verticals.dist
#   hlines <- lineSegments$horizontals.dist
#
#   #Frequency tables of line lengths
#   freq_dl <- table(dlines)
#   freq_vl <- table(vlines)
#   freq_hl <- table(hlines)
#
#   freqvec_dl <- as.numeric(names(freq_dl))
#   freqvec_vl <- as.numeric(names(freq_vl))
#   freqvec_hl <- as.numeric(names(freq_hl))
#
#   # Number of lines
#   N_dl <- sum(freq_dl, na.rm = TRUE)
#   N_vl <- sum(freq_vl, na.rm = TRUE)
#   N_hl <- sum(freq_hl, na.rm = TRUE)
#
#   #Number of recurrent points on diagonal, vertical and horizontal lines
#   N_dlp <- sum(freqvec_dl*freq_dl, na.rm = TRUE)
#   N_vlp <- sum(freqvec_vl*freq_vl, na.rm = TRUE)
#   N_hlp <- sum(freqvec_hl*freq_hl, na.rm = TRUE)
#
#   #Determinism / Horizontal and Vertical Laminarity
#   DET    <- N_dlp/RP_N
#   LAM_vl <- N_vlp/RP_N
#   LAM_hl <- N_hlp/RP_N
#
#   #anisotropy ratio
#   ANI    <- ((N_vlp-N_hlp)/N_dlp)%00%NA
#
#   # Singularities
#   SING <- rp_lineDist(RM,
#                         DLmin = 1, DLmax = DLmax,
#                         VLmin = 1, VLmax = VLmax,
#                         HLmin = 1, HLmax = HLmax,
#                         theiler = theiler, AUTO = AUTO)
#
#   # table(SING_N$verticals.dist)
#   # table(SING_N$horizontals.dist)
#   SING_N  <-  table(SING$diagonals.dist)[1]
#   SING_rate <- SING_N / RP_N
#
#   #Array of probabilities that a certain line length will occur (all >1)
#   P_dl <- freq_dl/N_dl
#   P_vl <- freq_vl/N_vl
#   P_hl <- freq_hl/N_hl
#
#   #Entropy of line length distributions
#   ENT_dl <- -1 * sum(P_dl * log(P_dl))
#   ENT_vl <- -1 * sum(P_vl * log(P_vl))
#   ENT_hl <- -1 * sum(P_hl * log(P_hl))
#
#   #Relative Entropy (Entropy / Max entropy)
#   ENTrel_dl = ENT_dl/(-1 * log(1/DLmax))
#   ENTrel_vl = ENT_vl/(-1 * log(1/VLmax))
#   ENTrel_hl = ENT_hl/(-1 * log(1/HLmax))
#
#   #Meanline
#   MEAN_dl = mean(dlines, na.rm = TRUE)
#   MEAN_vl = mean(vlines, na.rm = TRUE)
#   MEAN_hl = mean(hlines, na.rm = TRUE)
#
#   #Maxline
#   MAX_dl = max(freqvec_dl, na.rm = TRUE)
#   MAX_vl = max(freqvec_vl, na.rm = TRUE)
#   MAX_hl = max(freqvec_hl, na.rm = TRUE)
#
#   # REPetetiveness
#   REP_av <- ((N_hlp/N_dlp) + (N_vlp/N_dlp))/2
#   REP_hl  <-  N_hlp/N_dlp
#   REP_vl  <-  N_vlp/N_dlp
#
#   #Coefficient of determination
#   CoV_dl = stats::sd(dlines)/mean(dlines)
#   CoV_vl = stats::sd(vlines)/mean(vlines)
#   CoV_hl = stats::sd(hlines)/mean(hlines)
#
#   #Divergence
#   DIV_dl = 1/MAX_dl
#   DIV_vl = 1/MAX_vl
#   DIV_hl = 1/MAX_hl
#
#   #Output
#   out <- data.frame(
#     emRad     = emRad,
#     RP_N      = RP_N,
#     RR        = RR,
#     SING_N    = SING_N,
#     SING_rate = SING_rate,
#     DIV_dl    = DIV_dl,
#     REP_av    = REP_av,
#     ANI       = ANI,
#     N_dl      = N_dl,
#     N_dlp     = N_dlp,
#     DET       = DET,
#     MEAN_dl   = MEAN_dl,
#     MAX_dl    = MAX_dl,
#     ENT_dl    = ENT_dl,
#     ENTrel_dl = ENTrel_dl,
#     CoV_dl    = CoV_dl,
#     N_vl      = N_vl,
#     N_vlp     = N_vlp,
#     LAM_vl    = LAM_vl,
#     TT_vl     = MEAN_vl,
#     MAX_vl    = MAX_vl,
#     ENT_vl    = ENT_vl,
#     ENTrel_vl = ENTrel_vl,
#     CoV_vl    = CoV_vl,
#     REP_vl    = REP_vl,
# #    DIV_vl   = DIV_vl,
#     N_hlp     = N_hlp,
#     N_hl      = N_hl,
#     LAM_hl    = LAM_hl,
#     TT_hl     = MEAN_hl,
#     MAX_hl    = MAX_hl,
#     ENT_hl    = ENT_hl,
#     ENTrel_hl = ENTrel_hl,
#     CoV_hl    = CoV_hl,
#     REP_hl    = REP_hl)
# #    DIV_hl   = DIV_hl)
#
#   if(matrices){
#     return(list(
#       crqaMeasures = out,
#       crqaMatrices = list(RM     = RM,
#                           dlines = dlines,
#                           vlines = vlines,
#                           hlines = hlines,
#                           freq_dl = freq_dl,
#                           freq_vl = freq_vl,
#                           freq_hl = freq_hl))
#     )
#   } else {
#     return(out)
#   }
# }
#
#
# # Prepare matrix
# #
# # @param RP Recurrence plot
# # @param emRad Radiuc
# # @param DLmin Minimal diagonal line length
# # @param VLmin Minimal vertical line length
# # @param HLmin Minimal horizontal line length
# # @param DLmax Maximal diagonal line length
# # @param VLmax Maximal vertical line length
# # @param HLmax Maximal horizontal line length
# # @param AUTO Is this an AUTO RQA?
# # @param chromatic Chromatic RQA?
# # @param matrices Return Matrices?
# # @param doHalf Analyse half of the matrix?
# #
# # @return A prepped matrix
# # @keywords internal
# #
# # @export
# #
# crqa_rp_prep <- function(RP,
#                          emRad = NULL,
#                          DLmin = 2,
#                          VLmin = 2,
#                          HLmin = 2,
#                          DLmax = length(Matrix::diag(RP)),
#                          VLmax = length(Matrix::diag(RP)),
#                          HLmax = length(Matrix::diag(RP)),
#                          AUTO      = FALSE,
#                          chromatic = FALSE,
#                          matrices  = FALSE,
#                          doHalf    = FALSE){
#
#   out<-crqa_rp_calc(RP,
#                     emRad = emRad,
#                     DLmin = DLmin,
#                     VLmin = VLmin,
#                     HLmin = HLmin,
#                     DLmax = DLmax,
#                     VLmax = VLmax,
#                     HLmax = HLmax,
#                     AUTO  = AUTO,
#                     chromatic = chromatic,
#                     matrices  = matrices)
#
#   if(doHalf){
#     if(!AUTO){
#       outLo <- crqa_rp_calc(Matrix::tril(RP,-1),
#                             emRad = emRad,
#                             DLmin = DLmin,
#                             VLmin = VLmin,
#                             HLmin = HLmin,
#                             DLmax = DLmax,
#                             VLmax = VLmax,
#                             HLmax = HLmax,
#                             AUTO  = AUTO,
#                             chromatic = chromatic,
#                             matrices  = matrices)
#
#       outUp <- crqa_rp_calc(Matrix::triu(RP,-1),
#                             emRad= emRad,
#                             DLmin = DLmin,
#                             VLmin = VLmin,
#                             HLmin = HLmin,
#                             DLmax = DLmax,
#                             VLmax = VLmax,
#                             HLmax = HLmax,
#                             AUTO  = AUTO,
#                             chromatic = chromatic,
#                             matrices  = matrices)
#       out <- cbind.data.frame(full  = out,
#                               lower = outLo,
#                               upper = outUp)
#     } else {
#       warning("doHalf = TRUE and AUTO = TRUE. Results would be the same for lower and upper triangle!")
#       out<- cbind.data.frame(full  = out,
#                              lower = crqa_rp_empty(),
#                              upper = crqa_rp_empty())
#     }
#   }
#   return(out)
# }
#
#
#
#
# # plotRM.crqa <- function(RM,
# #                         useLattice = FALSE,
# #                         plotVars   = NULL,
# #                         plotTime   = NULL,
# #                         plotMatrix = TRUE){
# #   require(scales)
# #
# #   # RP dimensions
# #   nr <- dim(RM)[1]
# #   nc <- dim(RM)[2]
# #
# #   # Do some checks
# #   plotVarsOK <- FALSE
# #   plotTimeOK <- FALSE
# #
# #   if(any(class(RM)=="xts"|class(RM)=="zoo")){
# #     if(!is.null(attr(RM,"emDims1"))){
# #       plotVarsOK <- TRUE
# #       plotTimeOK <- TRUE
# #       plotVars   <- list(attributes(RM)$emDims1,attributes(RM)$emDims2)
# #       matNAmes   <- c(attr(RM,"emDims1.name"),attr(RM,"emDims1.name"))
# #       AUTO       <- attr(RM,"AUTO")
# #       RM         <- unclass(RM)
# #     }
# #   } else {
# #
# #    AUTO <-  ifelse(isSymmetric(RM),TRUE,FALSE)
# #
# #     if(is.list(plotVars)){
# #       if(all(lengths(plotVars)==c(nr,nc))){
# #         yr <- plotVars[[1]]
# #         yc <- plotVars[[2]]
# #         plotVarsOK <- TRUE
# #         if(any(ncol(yr)>10,ncol(yc)>10)){
# #           yr <- yr[,1:10]
# #           yc <- yc[,1:10]
# #           warning("Detected more than 10 embedding dims, will plot 1-10.")}
# #       } else {warning("Expecting: length(plotVars[[1]]) == dims(RM)[1] & length(plotVars[[2]]) == dims(RM)[2]\nFound: nrows = ",paste(lengths(plotVars),collapse ="; cols = "))}
# #     } else {warning("plotVars not a list.\n Not plotting dimensions.")}
# #
# #     # Plot Time?
# #     if(!is.null(plotTime)){
# #       if(is.POSIXct(as_datetime(dimnames(RM)[[2]]))){
# #         plotTimeOK <- TRUE
# #       }
# #     }
# #   }
# #
# #   #   if(!is.list(plotTime)){
# #   #     if(nc!=nr){
# #   #       warning("Unequal rows and columns, expecting a list with 2 time variables.")
# #   #     } else {
# #   #       if(NROW(plotTime==nr)){
# #   #         plotTimeRow <- plotTimeCol <- plotTime
# #   #         plotTimeOK <- TRUE
# #   #       } else {
# #   #         warning("Time variable length should equal nrows(RM) & ncols(RM).")
# #   #       } # if rows are not equal to length
# #   #     } #if row and columns equal
# #   #   } else {
# #   #     if(all(lengths(plotTime)==c(nr,nc))){
# #   #       plotTimeOK <- TRUE
# #   #       plotTimeRow <- plotTime[[1]]
# #   #       plotTimeCol <- plotTime[[2]]
# #   #     } else {
# #   #       warning("Expecting: length(plotTime[[1]]) == dims(RM)[1] & length(plotTime[[2]]) == dims(RM)[2]\nFound: nrows = ",paste(lengths(plotTime),collapse ="; cols = "))
# #   #     }
# #   #   }
# #   #   if(plotTimeOK){
# #   #     if(is.POSIXt(plotTimeRow)&is.POSIXt(plotTimeCol)){
# #   #       plyr::amv_dimnames(RM) <-list(plotTimeRow, plotTimeCol)
# #   #     } else {
# #   #       warning("plotTime is not a POSIXct or POSIXlt object.")
# #   #     }
# #   #   }
# #   # }
# #
# #   # AUTO or CROSS?
# #   if(AUTO){
# #     RM   <- Matrix::t(RM)
# #   }
# #
# #   if(nc*nr>10^6){
# #     message(paste("\nLarge RM with",nc*nr,"elements... \n >> This could take some time to plot!\n"))
# #   }
# #
# #   if(useLattice){
# #     require(lattice)
# #     require(latticeExtra)
# #
# #     ifelse(AUTO,
# #            Titles <- list(main="Auto Recurrence Plot", X = expression(Y[t]), Y = expression(Y[t])),
# #            Titles <- list(main="Cross Recurrence Plot", X = expression(X[t]), Y = expression(Y[t]))
# #     )
# #
# #     #binned <- ftable(as.vector(RM))
# #     dimnames(RM) <- list(NULL,NULL)
# #
# #     #Thresholded or Distance matrix?
# #     ifelse((all(as.vector(RM)==0|as.vector(RM)==1)),
# #            distPallette <- grDevices::colorRampPalette(c("white", "black"))(2),
# #            distPallette <- grDevices::colorRampPalette(colors = c("red3", "snow", "steelblue"),
# #                                             space = "Lab",
# #                                             interpolate = "spline")
# #     )
# #
# #     lp <- levelplot(as.matrix(RM),
# #                     colorkey = !AUTO,
# #                     region = TRUE,
# #                     col.regions = distPallette,
# #                     useRaster = TRUE,
# #                     aspect = nc/nr,
# #                     main = Titles$main,
# #                     xlab = Titles$X,
# #                     ylab = Titles$Y)
# #
# #
# #     if(plotVarsOK){
# #       lp
# #     }
# #
# #     if(plotMatrix){
# #       lp
# #       #grid.arrange(lp, txt, ncol=2, nrow=1, widths=c(4, 1), heights=c(4))
# #     }
# #
# #     return(lp)
# #
# #   } else {
# #
# #     require(gridExtra)
# #     require(ggplot2)
# #     require(reshape2)
# #
# #     meltRP  <- reshape2::melt((RM))
# #
# #     if(!all(dplyr::between(meltRP$value[!is.na(meltRP$value)],0,1))){
# #       meltRP$value <- scales::rescale(meltRP$value)
# #     }
# #
# #   # #  if(plotTimeOK){
# #   #     #if( (year(last(paste0(meltRP$Var1)))- year(meltRP$Var1[1]))>=1){
# #   #       meltRP$Var1 <- parse_date_time(as.character(meltRP$Var1), c("ymd", "ymd HM", "ymd HMS"))
# #   #       meltRP$Var2 <- parse_date_time(as.character(meltRP$Var2), c("ymd", "ymd HM", "ymd HMS"))
# #   #     #}
# #   #  # } else {
# #   #     meltRP$Var1 <- index(meltRP$value)
# #   #     meltRP$Var2 <- index(meltRP$value)
# #   #   #}
# #
# #   ##  colnames(meltRP)[1:2] <- matNAmes
# #
# #     grp <- ggplot(data = meltRP, aes_(x = meltRP[,2],
# #                                      y = meltRP[,1],
# #                                      fill = value)) + geom_raster()
# #
# #     if(all(as.vector(RM)==0|as.vector(RM)==1)){
# #       grp <- grp + scale_fill_grey(name="Manhattan")
# #     } else {
# #       grp <- grp +  scale_fill_gradient2(low      = "red3",
# #                                          high     = "steelblue",
# #                                          mid      = "white",
# #                                          na.value = scales::muted("slategray4"),
# #                                          midpoint = .5,
# #                                          limit    = c(0,1),
# #                                          space    = "Lab",
# #                                          name     = "Manhattan")
# #     }
# #
# #     if(AUTO){
# #       grp <- grp + labs(title = "Auto-Recurrence Plot\n")
# #     } else {
# #       grp <- grp + labs("Cross-Recurrence Plot\n")
# #     }
# #
# #     # if(plotTime){
# #     grp <- grp +
# #       scale_x_discrete(limits = c(first(meltRP$Var1),last(meltRP$Var2))) + #breaks = meltRP$Var2[seq(1,nrow(meltRP),round(nrow(meltRP)/5))]) +
# #       scale_y_discrete(limits = c(first(meltRP$Var1),last(meltRP$Var2))) #breaks = meltRP$Var1[seq(1,nrow(meltRP),round(nrow(meltRP)/5))])
# #
# #     grp <- grp + theme_bw() +  coord_fixed()
# #
# #       # theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
# #       #       axis.text.y = element_text(size = 8)) +
# #       #
# #
# #     if(plotVarsOK){
# #       grp
# #     }
# #
# #
# #     if(plotMatrix){
# #       grp
# #       #grid.arrange(lp, txt, ncol=2, nrow=1, widths=c(4, 1), heights=c(4))
# #     }
# #
# #     return(grp)
# #   }
# #
# #   # } else {
# #   #   warning("\nInput is not list output from function crqa().\n")
# #   # }
# #
# # }
#
#
#
# # Networks ----
#
# # Create a Recurrence Network Matrix
# #
# # This function serves as a wrapper for function `rp()`, it will add some attributes to the matrix related to network representation. These attributes will be used to decide which network type to generate (e.g. undirected, directed, weighted, etc.)
# #
# # @inheritParams rp
# # @param directed Should the matrix be considered to represent a directed network? (default = `FALSE`)
# # @param weighted Should the matrix be considered to represent a weighted network? (default = `FALSE`)
# # @param weightedBy After setting values smaller than `emRad` to `0`, what should the recurrent values represent? The default is to use the state space similarity (distance/proximity) values as weights (`"si"`). Other option are `"rt"` for *recurrence time* and `"rf"` for *recurrence time frequency*, Because vertices represent time points in \eqn{\epsilon}-thresholded recurrence networks, a difference of two vertex-indices represents duration. If an edge `e1` connects `v1` and `v10` then the *recurrence time* will be the difference of the vertex indices, `9`, and the *recurrence time frequency* will be `1/9`.
# # @param rescaleWeights If set to `TRUE` and `weighted = TRUE`, all weight values will be rescaled to `[0,1]`, where `0` means no recurrence relation and `1` the maximum weight value.
# # @param fs Sample frequency: A numeric value interpreted as the `number of observed samples per unit of time`. If the weights represent recurrence times (`"rt"`), they will be divided by the value in `fs`. If the weights represent recurrence time frequencies (`"rf"`), they will be multiplied by the value of `fs` (default = `NA`)
# # @param includeDiagonal Should the diagonal of the matrix be included when creating the network (default = `FALSE`)
# # @param returnGraph Return an [igraph::igraph()] object (default = `FALSE`)
# # @param ... Any paramters to pass to [rn_plot()] if `doPlot = TRUE`
# #
# # @return A (Coss-) Recurrence matrix that can be interpreted as an adjacency (or incidence) matrix.
# #
# # @export
# #
# # @family Distance matrix operations (recurrence network)
# #
# #
# rn <- function(y1, y2 = NULL,
#                emDim = 1,
#                emLag = 1,
#                emRad = NULL,
#                directed = FALSE,
#                weighted = FALSE,
#                weightedBy = c("si","rt","rf")[1],
#                rescaleWeights = FALSE,
#                fs = NA,
#                includeDiagonal = FALSE,
#                to.ts = NULL,
#                order.by = NULL,
#                to.sparse = FALSE,
#                method = "Euclidean",
#                targetValue = .05,
#                returnGraph = FALSE,
#                doPlot = FALSE,
#                silent = TRUE,
#                ...){
#
#   dmat <- rp(y1 = y1, y2 = y2,
#              emDim = emDim, emLag = emLag, emRad = emRad,
#              to.ts = to.ts, order.by = order.by, to.sparse = to.sparse,
#              weighted = weighted,targetValue = targetValue,
#              method = method, doPlot = FALSE, silent = silent)
#
#
#   if(to.sparse){
#     attributes(dmat)$directed <- directed
#     attributes(dmat)$includeDiagonal <- includeDiagonal
#     #attributes(dmat)$weighted <- weighted
#   } else {
#     attr(dmat,"directed") <- directed
#     attr(dmat,"includeDiagonal") <- includeDiagonal
#     #attr(dmat,"weighted") <- weighted
#   }
#
#   if(attr(dmat,"AUTO")){
#     mode <- "upper"
#   } else {
#     mode <- "directed"
#   }
#
#   if(weighted){
#     if(!weightedBy%in%c("si","rt","rf")){stop("Invalid string in argument weightedBy!")}
#     if(weightedBy%in%c("rt","rf")){
#       grW <- igraph::graph_from_adjacency_matrix(dmat, weighted = TRUE, mode = mode, diag = TRUE) #includeDiagonal)
#       edgeFrame         <- igraph::as_data_frame(grW,"edges")
#       edgeFrame$rectime <- edgeFrame$to-edgeFrame$from
#       if(weightedBy=="rf"){
#         edgeFrame$rectime <- 1/(edgeFrame$rectime+1) #.Machine$double.eps)
#       }
#       if(is.numeric(fs)){
#         if(weightedBy=="rf"){edgeFrame$rectime <- edgeFrame$rectime * fs}
#         if(weightedBy=="rt"){edgeFrame$rectime <- edgeFrame$rectime / fs}
#       }
#       igraph::E(grW)$weight <- edgeFrame$rectime
#       # for(r in 1:NROW(edgeFrame)){
#       #   dmat[edgeFrame$from[r],edgeFrame$to[r]] <- 1/edgeFrame$rectime[r]
#       #   if(attr(dmat,"AUTO")){
#       #     dmat[edgeFrame$to[r],edgeFrame$from[r]] <- 1/edgeFrame$rectime[r]
#       #     }
#       #   }
#       tmp <- igraph::as_adjacency_matrix(grW, type = "both", sparse = to.sparse, attr = "weight")
#       #tmp <- bandReplace(tmp,0,0,0)
#       dmat <- rp_copy_attributes(source = dmat, target = tmp)
#       rm(tmp)
#     }
#
#     if(rescaleWeights==TRUE){
#       dmat <- dmat/max(dmat, na.rm = TRUE)
#     }
#   }
#
#   if(doPlot){
#
#     if(doPlot){
#       dotArgs  <- formals(rn_plot)
#       nameOk   <- rep(TRUE,length(dotArgs))
#       if(...length()>0){
#         dotArgs <- list(...)
#         nameOK  <- names(dotArgs)%in%methods::formalArgs(rn_plot)
#         # Plot with defaults
#         if(!all(nameOK)){
#           dotArgs    <- formals(rn_plot)
#           nameOk <- rep(TRUE,length(dotArgs))
#         }
#       }
#       dotArgs$RM <- dmat
#       do.call(rn_plot, dotArgs[nameOk])
#     }
#
#     # dotArgs <- list(...)
#     #
#     # if(is.null(dotArgs)){
#     #   dotArgs<- formals(rn_plot)
#     #   }
#     #
#     # nameOk  <- names(dotArgs)%in%methods::formalArgs(rn_plot)
#     # # Plot with defaults
#     # if(!all(nameOk)){
#     #   dotArgs <- formals(rn_plot)
#     #   nameOk  <- rep(TRUE,length(dotArgs))
#     # }
#     #
#     # dotArgs$RN <- dmat
#     #
#     # do.call(rn_plot, dotArgs[nameOk])
#   }
#
#   if(returnGraph){
#     return(list(RN = dmat,
#                 g  = igraph::graph_from_adjacency_matrix(dmat, weighted = weighted, mode = mode, diag = includeDiagonal))
#     )
#   } else {
#     return(dmat)
#   }
# }
#
#
# # Plot (thresholded) distance matrix as a network
# #
# # @param RN A distance matrix or recurrence matrix
# # @inheritParams rp_plot
# #
# # @return A nice plot of the recurrence network
# # @export
# #
# # @family Distance matrix operations (recurrence network)
# #
# rn_plot <- function(RN,
#                     plotDimensions = FALSE,
#                     plotMeasures   = FALSE,
#                     drawGrid = FALSE,
#                     markEpochsLOI = NULL,
#                     Chromatic = NULL,
#                     radiusValue = NA,
#                     title = "", xlab = "", ylab="",
#                     plotSurrogate = NA,
#                     returnOnlyObject = FALSE){
#
#   # JAMOVI VERSION
#   rp_plot(RM = RN,
#           plotDimensions  = plotDimensions,
#           plotMeasures    = plotMeasures,
#           markEpochsLOI = markEpochsLOI,
#           radiusValue = radiusValue,
#           title = title, xlab = xlab, ylab = ylab,
#           plotSurrogate = plotSurrogate,
#           returnOnlyObject = returnOnlyObject)
#
# }
#
#
# # Recurrence Time Spectrum
# #
# # Get the recurrence time distribution from a recurrence network.
# #
# # @param RN A thresholded recurrence matrix generated by function `rn()`
# # @param fitRange If `NULL` the entire range will be used for log-log slope. If a 2-element vector of integers, this will represent the range of recurrence times to use for fitting the log=log slope (e.g. `c(1,50)` would fit the first 50 recurrence times).
# # @param fs Sample rate (default = `1`)
# # @param doPlot Should a plot of the recurrence time spectrum be produced?
# # @param returnPlot Return ggplot2 object (default = `FALSE`)
# # @param returnPLAW Return the power law data (default = `FALSE`)
# # @param returnInfo Return all the data used in SDA (default = `FALSE`)
# # @param silent Silent-ish mode
# # @param noTitle Do not generate a title (only the subtitle)
# # @param tsName Name of y added as a subtitle to the plot
# #
# # @return A vector of frequencies of recurrence times and a plot (if requested)
# #
# # @export
# #
# # @family Distance matrix operations (recurrence network)
# #
# rn_recSpec <- function(RN,
#                        fitRange = NULL,
#                        fs = 1,
#                        doPlot = TRUE,
#                        returnPlot = FALSE,
#                        returnPLAW = FALSE,
#                        returnInfo = FALSE,
#                        silent = TRUE,
#                        noTitle = FALSE,
#                        tsName="y"){
#
#   if(is.null(attributes(RN)$weighted)){
#     stop("Wrong RN format: Create a thresholded recurrence matrix using function rn()")
#   }
#
#   diagonal <- attributes(RN)$includeDiagonal
#   weighted <- NULL
#   if(attributes(RN)$weighted){weighted <- TRUE}
#
#   g1 <- igraph::graph_from_adjacency_matrix(RN, mode="upper", diag = diagonal, weighted = weighted)
#
#   edgeFrame <- igraph::as_data_frame(g1,"edges")
#   edgeFrame$rectime <- edgeFrame$to-edgeFrame$from
#
#   #d <- density(edgeFrame$rectime, kernel = "cosine")
#   #plot(d)
#   #d <- table(edgeFrame$rectime)
#
#   RT <- edgeFrame$rectime
#   f1 = seq(0,1, by = 1/(1000*fs))
#   P1 = graphics::hist(x = 1/(RT+1), breaks = f1)
#
#   # f = seq(1,2*max(RT))
#   # P = graphics::hist(RT, f)
#   # f2 = fs/(P$mids+.5)
#
#
#   yl <- c(smooth(P1$counts)+1)
#   xl <- c(fs*P1$mids)
#   #lreg <- stats::lm(log2(yl[yl>0] ~ xl[yl>0])
#   #pred <- predict(object = lreg)
#
#   # d <- graphics::hist(edgeFrame$rectime,breaks = (0:NROW(RN))+0.5, plot = FALSE)$count
#   # names(d) <- paste(1:NROW(RN))
#   # d <- d[d>0]
#
#   ddata <- data.frame(size = xl, bulk = as.numeric(yl))
#   ddata <- dplyr::arrange(ddata,size)
#   rownames(ddata) <- paste(1:NROW(ddata))
#
#   if(is.null(fitRange)){
#     nr <- 1:round(NROW(ddata)/4)
#   } else {
#     if(length(fitRange)==2&is.numeric(fitRange)){
#       if(fitRange[1]<0){fitRange[1] <- 1}
#       if(fitRange[2]>NROW(ddata)){fitRange[2] <- NROW(ddata)}
#       nr <- fitRange[1]:fitRange[2]
#     } else {
#       stop("Wrong fitRange format: Either NULL or a 2-integer vector.")
#     }
#   }
#
#   nr <- which(nr%in%rownames(ddata))
#
#   lmfit1  <- stats::lm(log(ddata$bulk) ~ log(ddata$size))
#   alpha1 <- stats::coef(lmfit1)[2]
#
#   lmfit2  <- stats::lm(log(ddata$bulk[nr]) ~ log(ddata$size[nr]))
#   alpha2 <- stats::coef(lmfit2)[2]
#
#   outList <- list(
#     PLAW  =  ddata,
#     fullRange = list(sap = alpha1,
#                      # H = 1+stats::coef(lmfit1)[2] + Hadj,
#                      # FD = sa2fd_sda(stats::coef(lmfit1)[2]),
#                      fitlm1 = lmfit1,
#                      method = paste0("Full range (n = ",length(ddata$size),")\nSlope = ",round(stats::coef(lmfit1)[2],2))), #" | FD = ",round(sa2fd_sda(stats::coef(lmfit1)[2]),2))),
#     fitRange  = list(sap = stats::coef(lmfit2)[2],
#                      # H = 1+stats::coef(lmfit2)[2] + Hadj,
#                      # FD = sa2fd_sda(stats::coef(lmfit2)[2]),
#                      fitlm2 = lmfit2,
#                      method = paste0("Fit range (n = ",length(ddata$size[nr]),")\nSlope = ",round(stats::coef(lmfit2)[2],2))), # | FD = ",round(sa2fd_sda(stats::coef(lmfit2)[2]),2))),
#     info = list(edgelist=edgeFrame,fitdata=ddata),
#     plot = NA,
#     analysis = list(
#       name = "Recurrence Time Spectrum",
#       logBaseFit = "log2",
#       logBasePlot = 2))
#
#   if(doPlot|returnPlot){
#     if(noTitle){
#       title <- ""
#     } else {
#       title <- "log-log regression (Recurrence Time Spectrum)"
#     }
#     g <- plotFD_loglog(fd.OUT = outList, title = title, subtitle = tsName, logBase = "2", xlabel = "Frequency", ylabel = "Power")
#     if(doPlot){
#       grid::grid.newpage()
#       grid::grid.draw(g)
#     }
#     if(returnPlot){
#       outList$plot <- invisible(g)
#     }
#   }
#
#   if(returnInfo){returnPLAW<-TRUE}
#
#   return(outList[c(returnPLAW,TRUE,TRUE,returnInfo,returnPlot)])
#
#
#   #if(doPlot){
#   #
#   #   breaks_x <- scales::log_breaks(n=abs(diff(range(round(log2(ddata$x)))+c(-1,1))),base=2)(ddata$x)
#   #   labels_x <- eval(quote(scales::trans_format("log2", scales::math_format(2^.x,format=scales::number_format(accuracy = .1)))(breaks_x)))
#   #
#   #   breaks_y <- scales::log_breaks(n=abs(diff(range(round(log2(ddata$y)))+c(-1,1))),base=2)(ddata$y)
#   #   labels_y <- eval(quote(scales::trans_format("log2", scales::math_format(2^.x,format=scales::number_format(accuracy = .1)))(breaks_y)))
#   #
#   #   g <- ggplot2::ggplot(ddata, ggplot2::aes_(x=~x_l,y=~y_l)) +
#   #     scale_x_continuous(breaks = log2(breaks_x),
#   #                        labels = labels_x,  expand = c(0,0)) + # limits = lims_x) +
#   #     scale_y_continuous(breaks = log2(breaks_y),
#   #                        labels = labels_y, expand = c(0,0)) + # limits = lims_y) +
#   #     geom_point() +
#   #     geom_line(ggplot2::aes_(x=~x_p,y=~y_p), colour = "red3") +
#   #     ylab("Recurrence Times (log2)")+xlab("Frequency of Occurrence (log2)") +
#   #     #geom_label(aes(x=ddata$x_l[25],y=ddata$y_l[25],label=round(alpha,2))) +
#   #     annotation_logticks() +
#   #     theme_bw()
#
#   #   grid::grid.newpage()
#   #   grid::grid.draw(g)
#   # }
#   #
#   # if(returnOnlyObject){
#   #   return(invisible(g))
#   # }
#   #
#   #   if(returnPowerLaw){
#   #     return(ddata)
#   #   }
#
# }
#
#
# # Recurrence Time Scaleogram
# #
# # Display a recurrence network in a space representing Time (x-axis) x Scale (y-axis). The scale axis will be determined by the latency between the occurence of a value in the (embedded) time series vector and its recurrences in the future (i.e. only the upper triangle of the recurrence matrix will be displayed, excluding the diagonal).
# #
# # @param RN A thresholded recurrence matrix generated by function `rn()`
# # @param returnOnlyObject Return the `ggplot` / `ggraph` object only, do not draw the plot (default = `FALSE`)
# #
# # @return A `ggraph` graph object
# # @export
# #
# # @family Distance matrix operations (recurrence network)
# #
# rn_scaleoGram <- function(RN, returnOnlyObject = FALSE){
#
#   if(is.null(attributes(RN)$emRad)){
#     stop("Wrong RN format: Create a thresholded recurrence matrix using function rn()")
#   }
#
#   g1 <- igraph::graph_from_adjacency_matrix(RN, mode="upper", diag = FALSE, weighted = NULL)
#
#   edgeFrame <- igraph::as_data_frame(g1,"edges")
#   edgeFrame$rectime <- edgeFrame$to-edgeFrame$from
#   edgeFrame$rectime_bin <- cut(x = edgeFrame$rectime, ordered_result = TRUE, breaks = round(seq.int(1, max(edgeFrame$rectime), length.out = 11)), dig.lab = 3, include.lowest = TRUE, labels = FALSE)
#
#   #edgeFrame$rectime_bin <- as.numeric_character(edgeFrame$rectime_bin)
#   # table(edgeFrame$rectime_bin)
#   # range(edgeFrame$rectime[edgeFrame$rectime_bin==1])
#   #
#   # edgeFrame$vcol <- "#000000"
#   # edgeFrame <- arrange(edgeFrame,edgeFrame$from,edgeFrame$rectime)
#   #edgeFrame$scale.vertex <- interaction(edgeFrame$rectime,edgeFrame$from)
#
#   timepoints <- data.frame(name = as.numeric(igraph::V(g1)), degree =  igraph::degree(g1))
#   vertexFrame <- dplyr::left_join(x = expand.grid(name=timepoints$name, rt_scale = c(0,sort(unique(edgeFrame$rectime)))), y = timepoints, by = "name")
#
#   #Change Names
#   vertexFrame$name <- paste0(vertexFrame$name,".",vertexFrame$rt_scale)
#   edgeFrame$from <- paste0(edgeFrame$from,".0")
#   edgeFrame$to   <- paste0(edgeFrame$to,".",edgeFrame$rectime)
#
#   colvec <- paletteer::paletteer_c(package = "scico",palette = "vik",n = length(unique(edgeFrame$rectime)))
#   names(colvec) <- sort(unique(edgeFrame$rectime))
#   edgeFrame$colour <- NA
#   for(c in names(colvec)){
#     edgeFrame$colour[edgeFrame$rectime%in%c] <- colvec[names(colvec)%in%c]
#   }
#
#   g2 <- igraph::graph_from_data_frame(edgeFrame, directed = FALSE, vertices = vertexFrame)
#   igraph::E(g2)$color <- edgeFrame$colour
#
#   scaleogram <- ggraph::create_layout(g2, layout = "linear")
#   scaleogram$x <- as.numeric(plyr::laply(as.character(scaleogram$name),function(s) strsplit(s,"[.]")[[1]][1]))
#   scaleogram$y <- as.numeric(plyr::laply(as.character(scaleogram$name),function(s) strsplit(s,"[.]")[[1]][2]))
#   #scaleogram$rt_scale <- factor(scaleogram$y)
#   #attributes(scaleogram)
#
#   y1 <- data.frame(t1=attr(RN,"emDims1"))
#
#   # Y1
#   colnames(y1) <- paste0("X",1:NCOL(y1))
#   y1$tm  <- 1:NROW(y1)
#   y1$tmna <- 0
#   y1$tmna[is.na(y1[,1])] <- y1$tm[is.na(y1[,1])]
#   y1 <- tidyr::gather(y1,key="Dimension",value = "Value", -c("tm","tmna"))
#   y1$Value <-  elascer(y1$Value,lo = -round(max(igraph::E(g2)$rectime)/8),hi = 0)
#
#   g <- ggraph::ggraph(scaleogram) +
#     #geom_node_point(colour = "grey60", size=.1, alpha = .3) +
#     ggraph::geom_edge_diagonal(aes_(edge_colour = ~rectime, group = ~rectime), edge_alpha=.1,lineend = "round", linejoin = "round") +
#     ggplot2::geom_line(data = y1, ggplot2::aes_(y=~Value, x= ~tm, colour = ~Dimension, group = ~Dimension), show.legend = FALSE, size = .1) +
#     ggplot2::scale_color_grey() +
#     ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
#     ggraph::scale_edge_color_gradient2("Recurrence Time", low = paste(colvec[1]), mid = paste(colvec[round(length(colvec)/2)]), high = paste(colvec[length(colvec)]), midpoint = round(length(colvec)/2)) +
#     ggplot2::scale_x_continuous("Time", expand = c(0,0),
#                                 breaks = round(seq.int(1,max(igraph::V(g1)),length.out = 10)),
#                                 limits = c(0,max(as.numeric(igraph::V(g1))))) +
#     ggplot2::scale_y_continuous("Recurrence Time", expand = c(0,0),
#                                 breaks = c(-round(max(igraph::E(g2)$rectime)/8),sort(unique(igraph::E(g2)$rectime))[round(seq.int(1,length(unique(igraph::E(g2)$rectime)), length.out = 10))]),
#                                 labels = c("", paste(sort(unique(igraph::E(g2)$rectime))[round(seq.int(1,length(unique(igraph::E(g2)$rectime)), length.out = 10))])),
#                                 limits = c(-round(max(igraph::E(g2)$rectime)/8),max(igraph::E(g2)$rectime))) +
#     ggplot2::theme_bw() +
#     ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank())
#
#   if(!returnOnlyObject){
#     #grid::grid.newpage()
#     grid::grid.draw(g)
#   }
#   return(invisible(g))
# }
#
#
# # Mutual Information Function
# #
# #  Calculate the lagged mutual information fucntion within (auto-mif) or between (cross-mif) time series, or, conditional on another time series (conditional-cross-mif). Alternatively, calculate the total information of a multivariate dataset for different lags.
# #
# # @param y A `Nx1` matrix for auto-mif, a `Nx2` matrix or data frame for cross-mif, a `Nx3` matrix or data frame for mif between col 1 and 2 conditional on col 3; or a `NxM` matrix or data frame for the multi-information function. Mutual information for each lag will be calculated using functions in package [infotheo::infotheo()] for `lags` lagged versions of the time series.
# # @param lags The lags to evaluate mutual information.
# # @param nbins The number of bins passed to [infotheo::discretize()] if y is a matrix or [casnet::ts_discrete()]
# # @param doPlot Produce a plot of the symbolic time series by calling [plotRED_mif()] (default = `FALSE`)
# # @param surTest If `TRUE`, a surrogate will be conducted using simple surrogates. The surrogates will be created from the transition probabilities of the discretised time series, i.e. the probability of observing bin `j` when the current value is in bin `j`. The number of surrogates needed will be computed based on the value of the `alpha` parameter, conceived as a one-sided test: `mi > 0`.
# # @param alpha The alpha level for the surrogate test (default = `0.05`)
# #
# # @return The auto- or cross-mi function
# # @export
# #
# # @family Redundancy measures (mutual information)
# #
# # @examples
# #
# # # Lags to evaluate mututal information
# # lags <- -10:30
# #
# # # Auto-mutual information
# # y1 <- sin(seq(0, 100, by = 1/8)*pi)
# #
# # (mif(data.frame(y1),lags = lags))
# #
# # # Cross-mututal information, y2 is a lagged version y1
# # y2 <- y1[10:801]
# #
# # y <- data.frame(ts_trimfill(y1, y2, action = "trim.cut"))
# # (mif(y,lags = lags))
# #
# # # Conditional mutual information, add some noise to y2 and add it as a 3rd column
# # y$s <- y2+rnorm(NROW(y2))
# # (mif(y,lags = lags))
# #
# # # Multi-information, the information of the entire multivariate series at each lag
# # y$y3 <- cumsum(rnorm(NROW(y)))
# # (mif(y,lags = lags))
# #
# #
# mif <- function(y, lags=-10:10, nbins = ceiling(2*NROW(y)^(1/3)), doPlot = FALSE, surTest = FALSE, alpha = 0.05){
#
#   if(is.null(dim(y))){
#     y <- as.matrix(y,ncol=1)
#   }
#
#   cnt <- 0
#   N <- NROW(y)
#   mif_out <- numeric(length=length(lags))
#   for(i in lags) {
#     cnt <- cnt + 1
#     # shift y2 to left for neg lags
#     if(i < 0) {
#
#       ID2 <- -i:N
#       ID1 <- 1:(N-abs(i)+1)
#     }
#     if(i == 0) {
#       ID2 <- c(1:N)
#       ID1 <- c(1:N)
#     }
#     # shift y2 to right for pos lags
#     if(i > 0) {
#       ID2 <- 1:(N-i+1)
#       ID1 <- i:N
#     }
#     mif_out[cnt] <- mi_mat(y, ID1, ID2, discreteBins = nbins)
#   }
#
#   names(mif_out) <- paste(lags)
#   if(NCOL(y)==1){miType <- "I(X;X)"}
#   if(NCOL(y)==2){miType <- "I(X;Y)"}
#   if(NCOL(y)==3){miType <- "I(X;Y|Z)"}
#   if(NCOL(y)> 3){miType <- "I(X;Y;Z;...;N)"}
#   attr(mif_out,"miType") <- miType
#
#   if(doPlot){
#     plotRED_mif(mif.OUT = mif_out, lags = lags, nbins = nbins)
#   }
#
#   return(mif_out)
# }
#
#
# # Mutual Information variations
# #
# # @param y A matrix with time series in columns
# # @param ID1 ids
# # @param ID2 ids
# # @param discreteBins Number of bins to use when discretizing the time series
# #
# # @return mi in nats
# # @export
# #
# # @family Redundancy measures (mutual information)
# #
# mi_mat <- function(y, ID1, ID2, discreteBins = ceiling(2*NROW(ID1)^(1/3))){
#   Nc <- NCOL(y)
#   if(!is.null(dim(y))){
#     if(Nc == 1){out <- infotheo::mutinformation(X = infotheo::discretize(y[ID1,1], nbins = discreteBins),
#                                                 Y = infotheo::discretize(y[ID2,1], nbins = discreteBins))}
#     if(Nc == 2){out <- infotheo::mutinformation(X = infotheo::discretize(y[ID1,1], nbins = discreteBins), #length(seq(-.5,(NROW(ID1)-.5)))),
#                                                 Y = infotheo::discretize(y[ID2,2], nbins = discreteBins))} #length(seq(-.5,(NROW(ID2)-.5)))))}
#     if(Nc == 3){out <- infotheo::condinformation(X = infotheo::discretize(y[ID1,1], nbins = discreteBins),
#                                                  Y = infotheo::discretize(y[ID2,2], nbins = discreteBins),
#                                                  S = infotheo::discretize(y[ID1,3], nbins = discreteBins))}
#     if(Nc >  3){out <- infotheo::multiinformation(X = infotheo::discretize(y[ID1,], nbins = discreteBins))}
#     return(out)
#   } else {
#     warning("Input should be a matrix or data frame.")
#   }
# }
#
#
# mi_ts <- function(y1,y2=NULL, nbins=NA){
#
#   if(is.null(y2)){y2 <- y1}
#
#   # y1 <-  ts_checkfix(y1,fixNumericVector = TRUE)
#   # y2 <-  ts_checkfix(y2,fixNumericVector = TRUE)
#
#   equal <- data.frame(ts_trimfill(y1,y2,action = "trim.cut"))
#   idNA1 <- is.na(equal[,1])
#   idNA2 <- is.na(equal[,2])
#
#   equal <- equal[!apply(apply(equal,1,is.na),2,any),]
#
#   if(is.na(nbins)){ nbins <- ceiling(2*NROW(equal)^(1/3))}
#
#   equal[,1]  <- ts_discrete(equal[,1], nbins = nbins)
#   equal[,2]  <- ts_discrete(equal[,2], nbins = nbins)
#
#   ## init entropies
#   H_s <- H_u <- H_su <- 0
#
#   ## get ts length
#   TT <- NROW(equal)
#   bb <- max(equal)
#   ## get ts length
#
#   for(i in 1:bb) {
#     ## calculate entropy for 1st series
#     p_s <- sum(equal[,1] == i)/TT
#     if(p_s != 0) { H_s <- H_s - p_s*log(p_s, 2) }
#     ## calculate entropy for 2nd series
#     p_u <- sum(equal[,2] == i)/TT
#     if(p_u != 0) { H_u <- H_u - p_u*log(p_u, 2) }
#     for(j in 1:bb) {
#       ## calculate joint entropy
#       p_su <- sum(equal[,1]==i & equal[,2]==j)/TT
#       if(p_su != 0) { H_su <- H_su - p_su*log(p_su, 2) }
#     } ## end j loop
#   } ## end i loop
#   ## calc mutual info
#   return(MI <- H_s + H_u - H_su)
#   #  if(!normal) { return(MI) } else { return(MI/sqrt(H_s*H_u)) }
# }
#
#
# # Inter-layer mutual information
# #
# # @param g0 An igraph object representing a layer in a multiplex graph
# # @param g1 An igraph object representing a layer in a multiplex graph
# # @param probTable Option to return the table with marginal and joint degree distribution probabilities (default = `TRUE`)
# #
# # @return The inter-layer mutual information between `g1` and `g2`. If `probTable=TRUE`, a list object with two fields, the inter-layer mutual information and the table with marginal and joint degree distributions
# # @export
# #
# # @note If the networks are weighted the strength distribution will be used instead of the the degree distribution.
# #
# # @family Redundancy measures (mutual information)
# # @family Multiplex Recurrence Networks
# #
# mi_interlayer <- function(g0,g1, probTable=FALSE){
#
#   if(igraph::is.weighted(g0)&igraph::is.weighted(g1)){
#     s0  <- strength(g0)
#     s0m <- max(s0, na.rm = TRUE)
#     d0  <- hist(s0,seq(0,(ceiling(s0m)+1)), include.lowest = TRUE, plot = FALSE)$density
#     s1  <- strength(g1)
#     s1m <- max(s1, na.rm = TRUE)
#     d1  <- hist(s1,seq(0,(ceiling(s1m)+1)), include.lowest = TRUE, plot = FALSE)$density
#     equal <- data.frame(ts_trimfill(d0,d1))
#     p01 <- graphics::hist(x = igraph::strength(g0),breaks = seq(-.5,((NROW(equal))-.5)),plot=FALSE)$counts
#     p10 <- graphics::hist(x = igraph::strength(g1),breaks = seq(-.5,((NROW(equal))-.5)),plot=FALSE)$counts
#   } else {
#     d0    <- igraph::degree_distribution(g0)
#     d1    <- igraph::degree_distribution(g1)
#     equal <- data.frame(ts_trimfill(d0,d1))
#     p01 <- graphics::hist(x = igraph::degree(g0),breaks = seq(-.5,((NROW(equal))-.5)),plot=FALSE)$counts
#     p10 <- graphics::hist(x = igraph::degree(g1),breaks = seq(-.5,((NROW(equal))-.5)),plot=FALSE)$counts
#   }
#
#   equal$joint <-  (p01+p10) / sum(p01,p10,na.rm = TRUE)
#   equal$degree <- seq_along(equal[,1])-1
#
#   # imiAB <- sum(equal$joint * log(equal$joint/(equal[,1]*equal[,2]+.Machine$double.eps))%00%0, na.rm = TRUE)
#   imiAB <- infotheo::mutinformation(p01,p10)
#   if(probTable){
#     attributes(imiAB) <- list(miType = "inter-layer mutual information", probTable = equal)
#   } else {
#     attributes(imiAB) <- list(miType = "inter-layer mutual information")
#   }
#   return(imiAB)
# }
#
#
# # @title Distance to binary matrix
# #
# # @description Distance matrix to binary matrix based on threshold value
# #
# # @param distmat Distance matrix
# # @param emRad The radius or threshold value
# # @param theiler = Use a theiler window around the line of identity / synchronisation to remove high auto-correlation at short time-lags (default = `0`)
# # @param convMat Should the matrix be converted from a `distmat` obkect of class [Matrix::Matrix()] to [base::matrix()] (or vice versa)
# #
# # @return A (sparse) matrix with only 0s and 1s
# #
# # @export
# #
# # @family Distance matrix operations (recurrence plot)
# # @family Distance matrix operations (recurrence network)
# #
# di2bi <- function(distmat, emRad, theiler = 0, convMat = FALSE){
#
#   matPack <- FALSE
#   # if already Matrix do not convert to matrix
#   if(grepl("Matrix",class(distmat))){
#     matPack <- TRUE
#     convMat <- TRUE
#   }
#
#   # RP <- matrix(0,dim(distmat)[1],dim(distmat)[2])
#   # RP[as.matrix(distmat <= emRad)] <- 1
#   if(emRad==0){emRad <- .Machine$double.eps}
#   # Always use sparse representation for conversion to save memory load
#   ij  <- Matrix::which(distmat <= emRad, arr.ind=TRUE)
#
#   if(NROW(ij)>0){
#
#     xij <- data.frame(y =  sapply(seq_along(ij[,1]),function(r){distmat[ij[[r,1]],ij[[r,2]]]}), ij)
#     suppressWarnings(RP <- Matrix::sparseMatrix(x=rep(1,length(xij$y)),i=xij$row,j=xij$col, dims = dim(distmat)))
#
#     # Simple check
#     if(!all(as.vector(RP)==0|as.vector(RP)==1)){warning("Matrix did not convert to a binary (0,1) matrix!!")}
#
#   } else {
#
#     RP <- matrix(0,dim(distmat)[1],dim(distmat)[2])
#   }
#
#   if(convMat&matPack){RP <- Matrix::as.matrix(RP)}
#
#   suppressMessages(RP <- rp_copy_attributes(source = distmat,  target = RP))
#   attributes(RP)$emRad <- emRad
#
#   return(RP)
# }
#
#
# # Distance 2 weighted matrix
# #
# # Distance matrix to weighted matrix based on threshold value
# #
# # @param distmat Distance matrix
# # @param emRad The radius or threshold value
# # @param convMat convMat Should the matrix be converted from a `distmat` obkect of class [Matrix::Matrix()] to [base::matrix()] (or vice versa)
# #
# # @return A matrix with 0s and values < threshold distance value
# #
# # @export
# #
# # @family Distance matrix operations (recurrence plot)
# # @family Distance matrix operations (recurrence network)
# #
# di2we <- function(distmat, emRad, convMat = FALSE){
#
#   matPack <- FALSE
#   if(grepl("Matrix",class(distmat))){
#     matPack <- TRUE
#     convMat <- TRUE
#   }
#
#   # RP <- NetComp::matrix_threshold(distmat,threshold = emRad, minval = 1, maxval = 0)
#   if(emRad==0) emRad <- .Machine$double.eps
#   # RP <- distmat #matrix(0,dim(distmat)[1],dim(distmat)[2])
#   # RP[distmat <= emRad] <- 0
#
#   ij  <- Matrix::which(distmat <= emRad, arr.ind=TRUE)
#
#   if(NROW(ij)>0){
#
#     # Always use sparse representation for conversion to save memory load
#     #xij <- data.frame(y =  sapply(which(distmat > emRad, arr.ind=TRUE)[,1],function(r){distmat[ij[[r,1]],ij[[r,2]]]}), which(distmat > emRad, arr.ind=TRUE))
#     xij <- data.frame(y =  sapply(seq_along(ij[,1]),function(r){distmat[ij[[r,1]],ij[[r,2]]]}), ij)
#
#     suppressWarnings(RP <- Matrix::sparseMatrix(x=xij$y,i=xij$row,j=xij$col, dims = dim(distmat)))
#
#
#     #  if(!all(as.vector(RP)==0|as.vector(RP)==1)){warning("Matrix did not convert to a binary (0,1) matrix!!")}
#
#
#   } else {
#
#     RP <- matrix(0,dim(distmat)[1],dim(distmat)[2])
#   }
#
#   if(convMat&matPack){RP <- Matrix::as.matrix(RP)}
#
#   RP <- rp_copy_attributes(source = distmat,  target = RP)
#   attributes(RP)$emRad <- emRad
#
#   return(RP)
# }
#
#
#
# # Complex Networks ---
#
#
# SWtest0 <- function(g){
#   Nreps <- 10;
#   histr  <- vector("integer",Nreps)
#   target<- round(mean(igraph::degree(g)))
#   now   <- target/2
#   for(i in 1:Nreps){
#     gt      <- igraph::watts.strogatz.game(dim=1, size=length(igraph::degree(g)), nei=now, 0)
#     histr[i] <- round(mean(igraph::degree(gt)))
#     ifelse(histr[i] %in% histr,break,{
#       ifelse(histr[i]>target,{now<-now-1},{
#         ifelse(histr[i]<target,{now<-now+1},{
#           break})
#       })
#     })
#   }
#   return(gt)
# }
#
#
# # SWtestV <- function(g,N){
# #  return(list(cp=igraph::transitivity(g,type="global"),cpR=igraph::transitivity(igraph::rewire(g,mode=c("simple"),niter=N),type="global"),lp=igraph::average.path.length(g), lpR=igraph::average.path.length(igraph::rewire(g,mode=c("simple"),niter=N))))
# # }
#
# # Small World test
# #
# # @param g An igraph object
# # @param p p
# # @param N N
# #
# # @export
# #
# SWtestE <- function(g,p=1,N=20){
#   values <- matrix(nrow=N,ncol=6,dimnames=list(c(1:N),c("cp","cpR","cp0","lp","lpR","lp0")))
#
#   for(n in 1:N) {
#     gt<-SWtest0(g)
#     values[n,] <- c(igraph::transitivity(g,type="localaverage"),igraph::transitivity(igraph::rewire(g,igraph::each_edge(p=p)),type="localaverage"),igraph::transitivity(gt,type="localaverage"),igraph::average.path.length(g),igraph::average.path.length(igraph::rewire(g,igraph::each_edge(p=p))),igraph::average.path.length(gt))}
#   values[n,values[n,]==0] <- NA #values[n,values[n,]==0]+1e-8}
#
#   values   <- cbind(values,(values[,1]/values[,2])/(values[,4]/values[,5]),(values[,1]/values[,3]),(values[,4]/values[,6]),((values[,1]/values[,3])/values[,2])/((values[,4]/values[,6])/values[,5]))
#   valuesSD <- data.frame(matrix(apply(values[,1:10],2,FUN = stats::sd,na.rm=TRUE),nrow=1,ncol=10,dimnames=list(c(1),c("cp","cpR","cp0","lp","lpR","lp0","SWI","cp:cp0","lp:lp0","SWIn"))))
#   valuesAV <- data.frame(matrix(colMeans(values[,1:10],na.rm=T),nrow=1,ncol=10,dimnames=list(c(1),c("cp","cpR","cp0","lp","lpR","lp0","SWI","cp:cp0","lp:lp0","SWIn"))))
#   return(list(valuesAV=valuesAV,valuesSD=valuesSD,valuesSE=valuesSD/sqrt(N)))
# }
#
#
#
# # Layout a graph on a spiral
# #
# # @param g An igraph object. If (`rev = FALSE`) the vertex with the lowest index will be placed in the centre of the spiral, the highest index will be most outer vertex,
# # @param type Spiral type, one of `"Archimedean"`,`"Bernoulli"`,`"Fermat"`, or, `"Euler"` (default = `"Archimedean"`)
# # @param arcs The number of arcs (half circles/ovals) that make up the spiral (default = `10`)
# # @param a Parameter controlling the distance between spiral arms, however, the effect will vary for different spiral types (default = `0.5`)
# # @param b Parameter controlling where the spiral originates. A value of 1 will generally place the origin in the center. The default `NULL` will choose a value based on the different spiral types (default = `NULL`)
# # @param rev If `TRUE` the vertex with the highest index will be placed in the centre of the spiral (default = `FALSE`)
# #
# # @return An igraph layout
# #
# # @export
# #
# # @examples
# #
# #library(igraph)
# #
# # g  <- igraph::sample_gnp(100, 1/100)
# #
# # # Equiangular spiral: Any line from the origin cuts at the same angle.
# # plot(g, layout = layout_as_spiral(g, type = "Bernoulli", arcs = 5))
# #
# # # The arms of Fermat's spiral diverge quadratically.
# # plot(g, layout = layout_as_spiral(g, type = "Fermat", arcs = 5))
# #
# # # Equidistance of intersection points along a line through the origin.
# # plot(g, layout = layout_as_spiral(g, type = "Archimedean", arcs = 5))
# #
# layout_as_spiral <- function(g,
#                              type = c("Archimedean","Bernoulli","Fermat","Euler"),
#                              arcs = 6,
#                              a = 1,
#                              b = NULL,
#                              rev= FALSE){
#   if(length(which(type%in%c("Archimedean","Bernoulli","Fermat","Euler")))==0){
#     stop("Type must be one of: Archimedean, Bernoulli, Fermat, Euler")
#   }
#
#   N <- igraph::vcount(g)
#   if(length(unique(igraph::V(g)$size))>1){
#     res <- round(max(igraph::V(g)$size)*N)
#   } else {
#     res <- N*2
#   }
#
#   theta <- seq(0,arcs*pi,length.out = res)
#
#   if(type=="Archimedean"){
#     if(is.null(b)){
#       b <- 1
#     }
#     r  <- a + b*theta
#     df <- matrix(cbind(r*cos(theta),r*sin(theta)),ncol = 2)
#   }
#   if(type=="Bernoulli"){
#     if(is.null(b)){
#       b <- 0.1
#     }
#     r  <- a + exp(b*theta)
#     df <- matrix(cbind(r*cos(theta),r*sin(theta)),ncol = 2)
#   }
#   if(type=="Fermat"){
#     if(is.null(b)){
#       b <- 1
#     }
#     r  <- a + b*(theta*theta)
#     df <- matrix(cbind(r*cos(theta),r*sin(theta)),ncol = 2)
#   }
#   if(type=="Euler"){
#     if(is.null(b)){
#       b = .5
#     }
#     res1  <- ceiling(res/2)
#     res2  <- floor(res/2)
#     theta <- seq(0,pi/2,length.out = res1)
#     df1   <- data.frame(t=theta,x=NA,y=NA)
#     df1$x[1] <-0
#     df1$y[1] <-0
#
#     dt <- arcs/res1
#
#     for(i in 2:res1){
#       df1$x[i] <- df1$x[i-1] + cos(df1$t[i-1]^(b+1)/(b+1)) * dt #*df1$t[i-1]) * dt
#       df1$y[i] <- df1$y[i-1] + sin(df1$t[i-1]^(b+1)/(b+1)) * dt #*df1$t[i-1]) * dt
#       df1$t[i] <- df1$t[i-1]+dt
#     }
#     df <- matrix(cbind(c(-rev(df1$x[1:res2]),df1$x),c(-rev(df1$y[1:res2]),df1$y)),ncol = 2)
#   }
#
#   df     <- df[seq(1, res, length.out = N),]
#   df[,1] <- elascer(df[,1],lo = 0.1, hi = 0.9,boundaryPrecision = NA)
#   df[,2] <- elascer(df[,2],lo = 0.1, hi = 0.9,boundaryPrecision = NA)
#
#   if(rev){
#     df[,1] <- rev(df[,1])
#     df[,2] <- rev(df[,2])
#   }
#   return(df)
# }
#
# # Make Spiral Graph
# #
# # Turn an [igraph] object into a spiral graph returning a [ggplot2] object.
# #
# # @note To keep the igraph object, use the layout function [layout_as_spiral(g)] when plotting the graph.
# #
# # @inheritParams layout_as_spiral
# # @param markTimeBy Include a vector that indicates time. The time will be displayed on the plot. Pass `TRUE` to generate auto labels (experimental)
# # @param curvature The `curvature` parameter for edges see [geom_curve()] (default = `-0.7`)
# # @param angle The `angle` parameter for edges see [geom_curve()] (default = `90`)
# # @param title A title for the plot
# # @param subtitle A subtitle for the plot
# # @param showEpochLegend Should a legend be shown for the epoch colours? (default = `TRUE`)
# # @param markEpochsBy A vector of length `vcount(g)` indicating epochs or groups (default = `NULL`)
# # @param epochColours A vector of length `vcount(g)` with colour codes (default = `NULL`)
# # @param epochLabel A title for the epoch legend (default = `"Epoch"`)
# # @param showSizeLegend Should a legend be shown for the size of the nodes? (default = `FALSE`)
# # @param sizeLabel Use to indicate if `V(g)$size` represents some measure, e.g. [igraph::degree()], or, [igraph::hubscore()] (default = `"Size"`)
# # @param scaleVertexSize Scale the size of the vertices by setting a range for [ggplot2::scale_size()]. This will not affect the numbers on the size legend (default = `c(1,6)`)
# # @param vertexBorderColour Draw a border around the vertices. Pass `NULL` to use the same colour as the fill colour (default = `"black"`)
# # @param edgeColourLabel Use to indicate if `E(g)$color` represents color coding based on some property. (default = `"Weight"`)
# # @param scaleEdgeSize Scale the size of the edges by a constant: `E(g)$width * scaleEdgeSize` (default = `1/5`)
# # @param showEdgeColourLegend Should a legend be shown for the colour of the edges? (default = `FALSE`)
# # @param edgeColourByEpoch Should edges that connect to the same epoch be assigned the epoch colour? This will ignore edge colour info in `E(g)$color`. (default = `TRUE`)
# # @param defaultEdgeColour Colour of edges that do not connect to the same epoch (default = `"grey70"`)
# # @param doPlot Produce a plot? (default = `TRUE`)
# #
# # @return A ggplot object.
# #
# # @export
# #
# # @examples
# #
# # library(igraph)
# #
# # g  <- igraph::sample_gnp(200, 1/20)
# # V(g)$size <- degree(g)
# # make_spiral_graph(g, markTimeBy = TRUE, showSizeLegend = TRUE, sizeLabel = "Node degree")
# #
# make_spiral_graph <- function(g,
#                               type = "Archimedean",
#                               arcs = 6,
#                               a = 1,
#                               b = NULL,
#                               rev= FALSE,
#                               curvature = -0.6,
#                               angle = 90,
#                               markTimeBy = NULL,
#                               alphaV = 1,
#                               alphaE = .6,
#                               title = "",
#                               subtitle = "",
#                               showEpochLegend = TRUE,
#                               markEpochsBy = NULL,
#                               epochColours = NULL,
#                               epochLabel = "Epoch",
#                               showSizeLegend = FALSE,
#                               sizeLabel = "Size",
#                               scaleVertexSize = c(1,6),
#                               vertexBorderColour = "black",
#                               scaleEdgeSize = 1/5,
#                               edgeColourLabel = "Weight",
#                               showEdgeColourLegend = FALSE,
#                               edgeColourByEpoch = TRUE,
#                               defaultEdgeColour = "grey70",
#                               doPlot = TRUE){
#
#   g$layout <- layout_as_spiral(g, type = type, arcs = arcs, a = a, b = b, rev = rev)
#
# #
# #   tbreaks <- c(1,igraph::vcount(g))
# #   tlabels <- paste0(tbreaks)
#   if(is.null(markTimeBy)){
#     if(!is.null(markEpochsBy)){
#       grIDs <- ts_changeindex(markEpochsBy)
#       tbreaks <- unique(sort(c(grIDs$xmax,grIDs$xmax)))
#       tlabels <- ceiling(tbreaks)
#       markTimeBy <- TRUE
#     } else {
#       x <- 1:igraph::vcount(g)
#       v <- seq(1,igraph::vcount(g),by=igraph::vcount(g)/arcs)
#       tbreaks <- c(1,which(diff(findInterval(x, v))!=0),igraph::vcount(g))
#       #tbreaks <- which(diff(c(g$layout[1,1],g$layout[,2],g$layout[1,2])>=g$layout[1,2])!=0)
#       if(max(tbreaks)!=igraph::vcount(g)){
#         tbreaks[which.max(tbreaks)]<-igraph::vcount(g)
#         tlabels <- paste(tbreaks)
#       }
#       if(min(tbreaks)>1){
#         tbreaks<- c(1,tbreaks)
#         tlabels <- paste(tbreaks)
#       }
#     }
#   } else {
#     if(is.numeric(markTimeBy)){
#       if(all(markTimeBy%in%1:igraph::vcount(g))){
#         tbreaks <- unique(markTimeBy)
#         if(!is.null(names(markTimeBy))){
#           tlabels <- names(unique(markTimeBy))
#         } else {
#           tlabels <- paste(tbreaks)
#         }
#       }
#     }
#     if(markTimeBy){
#       if(type == "Euler"){
#         tbreaks <- which(diff(as.numeric(cut(g$layout[,2], breaks = arcs,include.lowest = TRUE)))!=0)
#         tbreaks[1] <- 1
#         tbreaks[which.max(tbreaks)] <- igraph::vcount(g)
#       } else {
#         tbreaks <- which(diff(c(g$layout[1,1],g$layout[,2],g$layout[1,2])>=g$layout[1,2])!=0)
#       }
#       if(max(tbreaks)>igraph::vcount(g)){tbreaks[which.max(tbreaks)]<-igraph::vcount(g)}
#       if(max(tbreaks)<igraph::vcount(g)){tbreaks<- c(tbreaks,igraph::vcount(g))}
#       if(min(tbreaks)>1){tbreaks<- c(1,tbreaks)}
#       tlabels <- paste(tbreaks)
#     }
#   }
#
#   if(max(tbreaks)!=igraph::vcount(g)){
#     tbreaks[which.max(tbreaks)]<-igraph::vcount(g)
#     tlabels <- paste(tbreaks)
#   }
#
#   if(min(tbreaks)>1){
#     tbreaks<- c(1,tbreaks)
#     tlabels <- paste(tbreaks)
#   }
#
#   if(length(which(diff(tbreaks)==1))>0){
#     tbreaks <- tbreaks[-(which(diff(tbreaks)==1)+1)]
#     tlabels <- paste(tbreaks)
#   }
#
#
#   if(is.null(markEpochsBy)){
#     markEpochsBy <- character(igraph::vcount(g))
#     for(i in 1:(length(tbreaks)-1)){
#       markEpochsBy[tbreaks[i]:tbreaks[i+1]] <- rep(paste0(tbreaks[i],"-",tbreaks[i+1]),length(tbreaks[i]:tbreaks[i+1]))
#     }
#     if(!is.null(epochColours)){
#       if(length(unique(markEpochsBy))>length(unique(epochColours))){
#         warning("Number of unique epochs is unequal to number of unique colours!\nUsing default colour scheme.")
#         epochColours <- NULL
#       }
#     }
#   }
#
#   g <- plotNET_groupColour(g,
#                            groups = markEpochsBy,
#                            colourV = TRUE,
#                            colourE = edgeColourByEpoch,
#                            # alphaV = alphaV,
#                            # aplhaE = alphaE,
#                            groupColours = epochColours,
#                            defaultEdgeColour = defaultEdgeColour,
#                            doPlot = FALSE)
#
#   size <- 1
#   if(!is.null(igraph::V(g)$size)){
#     size <- igraph::V(g)$size
#   }
#   gNodes        <- as.data.frame(g$layout)
#   gNodes$ID     <- as.numeric(igraph::V(g))
#   gNodes$colour <- igraph::V(g)$colour
#   gNodes$labels <- factor(igraph::V(g)$groupnum, levels = unique(igraph::V(g)$groupnum), labels = unique(igraph::V(g)$group))
#   gNodes$size   <- size
#   gNodes$alpha  <- igraph::V(g)$alpha
#
#   width <- 1
#   if(!is.null(igraph::E(g)$width)){
#     width <- igraph::E(g)$width
#   }
#   gEdges        <- igraph::get.data.frame(g)
#   gEdges$from.x <- gNodes$V1[match(gEdges$from, gNodes$ID)]
#   gEdges$from.y <- gNodes$V2[match(gEdges$from, gNodes$ID)]
#   gEdges$to.x   <- gNodes$V1[match(gEdges$to, gNodes$ID)]
#   gEdges$to.y   <- gNodes$V2[match(gEdges$to, gNodes$ID)]
#   gEdges$width  <- width
#   gEdges$colorVar <- as.numeric_discrete(gEdges$color)
#
#   if(is.null(vertexBorderColour))(
#     vBc <- gNodes$colour
#   ) else {
#     if(length(vertexBorderColour)==1|length(vertexBorderColour)==NROW(gNodes)){
#       vBc <- vertexBorderColour
#     } else {
#       warning("Invalid value(s) for vertexBorderColour, using default.")
#       vertexBorderColour <- "black"
#     }
#   }
#
#   # Fix same coords
#   sameID <- which(gEdges$from.x==gEdges$to.x)
#   if(length(sameID)>0){
#     gNodes$V2[gEdges$from[sameID]] <- gNodes$V2[gEdges$from[sameID]]+mean(diff(gNodes$V2))/2
#     gEdges$to.x[sameID] <- gEdges$to.x[sameID]+mean(diff(gEdges$to.x))/2
#     gEdges$to.y[sameID] <- gEdges$to.y[sameID]+mean(diff(gEdges$to.y))/2
#   }
#
#
#   gg <- ggplot(gNodes,aes(x=V1,y=V2)) +
#     geom_curve(data=gEdges, aes(x = from.x,
#                                 xend = to.x,
#                                 y = from.y,
#                                 yend = to.y,
#                                 colour = colorVar),
#                curvature = curvature,
#                angle = angle,
#                size = gEdges$width * scaleEdgeSize,
#                alpha = alphaE) +
#     geom_point(aes(fill = labels, size = size), pch=21, colour = vBc, alpha = alphaV) +
#     ggtitle(label = title, subtitle = subtitle) +
#     scale_fill_manual(epochLabel, values = unique(gNodes$colour)) +
#     scale_size(sizeLabel, range = scaleVertexSize) +
#     scale_color_gradientn(edgeColourLabel, colours = unique(gEdges$color))
#
#   if(showEpochLegend){
#     gg <- gg + guides(fill = guide_legend(title.position = "top",
#                                           byrow = TRUE,
#                                           override.aes = list(size=5, order = 0)))
#   } else {
#     gg <- gg + guides(fill = "none")
#   }
#
#   if(showSizeLegend){
#     gg <- gg + guides(size = guide_legend(title.position = "top",
#                                           byrow = TRUE,
#                                           override.aes = list(legend.key.size = unit(1.2,"lines"), order = 1)))
#   } else {
#     gg <- gg + guides(size = "none")
#   }
#
#   if(showEdgeColourLegend){
#     gg <- gg + guides(colour = guide_legend(title.position = "top",
#                                             byrow = TRUE,
#                                             override.aes = list(size=5, order = 3)))
#   } else {
#     gg <- gg + guides(colour = "none")
#   }
#
#   if(!is.null(markTimeBy)){
#     gg <- gg + annotate("label", x=gNodes$V1[tbreaks], y=gNodes$V2[tbreaks], label = tlabels)
#   }
#
#   gg <- gg +
#     coord_fixed() +
#     theme_void() +
#     theme(legend.title = element_text(face="bold"),
#           legend.position =  "top",
#           legend.margin = margin(t = 0,r = 1,l = 1,0))
#
#   if(doPlot){
#     print(gg)
#   }
#
#   return(invisible(gg))
# }
#
#
# # Spiral Graph with Epoch Focus
# #
# # Turn an [igraph] object into a spiral graph returning a [ggplot2] object.
# #
# # @note To keep the igraph object, use the layout function [layout_as_spiral(g)] when plotting the graph.
# #
# # @inheritParams make_spiral_graph
# #
# # @return A ggplot object.
# #
# # @export
# #
# # @examples
# #
# # library(igraph)
# # g  <- sample_gnp(200, 1/20)
# # V(g)$size <- degree(g)
# # make_spiral_graph(g, markTimeBy = TRUE, showSizeLegend = TRUE, sizeLabel = "Node degree")
# #
# make_spiral_focus <- function(g,
#                               arcs = 6,
#                               a = 1,
#                               b = NULL,
#                               rev= FALSE,
#                               curvature = -0.6,
#                               angle = 90,
#                               markTimeBy = NULL,
#                               alphaV = 1,
#                               alphaE = .6,
#                               title = "",
#                               subtitle = "",
#                               showEpochLegend = TRUE,
#                               markEpochsBy = NULL,
#                               epochColours = NULL,
#                               epochLabel = "Epoch",
#                               showSizeLegend = FALSE,
#                               sizeLabel = "Size",
#                               scaleVertexSize = c(1,6),
#                               vertexBorderColour = "black",
#                               scaleEdgeSize = 1/5,
#                               defaultEdgeColour = "grey70",
#                               doPlot = TRUE){
#
#   type   <- "Archimedean"
#   g_left <- g_right <- g
#
#   theta <- seq(0,arcs*pi,length.out = res)
#
#   if(type=="Archimedean"){
#     if(is.null(b)){
#       b <- 1
#     }
#     r  <- a + b*theta
#     g_right$layout <- matrix(cbind(r*cos(theta),r*sin(theta)),ncol = 2)
#     g_left$layout  <- matrix(cbind(r*cos(theta),r*sin(theta)),ncol = 2)
#     g_left
#
#   }
#
#   # g_right$layout <- layout_as_spiral(g, type = type, arcs = arcs, a = a, b = b, rev = rev)
#   # g_left$layout  <- -1*g_right$layout
#
#   g <- g_left + g_right
#
#   plot(g_left)
#   # g$layout <- layout_as_spiral(g, type = type, arcs = arcs, a = a, b = b, rev = rev)
#
#   if(is.null(markTimeBy)){
#     if(!is.null(markEpochsBy)){
#       grIDs <- ts_changeindex(markEpochsBy)
#       tbreaks <- unique(sort(c(grIDs$xmax,grIDs$xmax)))
#       tlabels <- ceiling(tbreaks)
#     } else {
#       x <- 1:igraph::vcount(g)
#       v <- seq(1,igraph::vcount(g),by=igraph::vcount(g)/arcs)
#       tbreaks <- c(1,which(diff(findInterval(x, v))!=0),igraph::vcount(g))
#       #tbreaks <- which(diff(c(g$layout[1,1],g$layout[,2],g$layout[1,2])>=g$layout[1,2])!=0)
#       if(max(tbreaks)!=igraph::vcount(g)){
#         tbreaks[which.max(tbreaks)]<-igraph::vcount(g)
#         tlabels <- paste(tbreaks)
#       }
#       if(min(tbreaks)>1){
#         tbreaks<- c(1,tbreaks)
#         tlabels <- paste(tbreaks)
#       }
#     }
#   } else {
#     if(is.numeric(markTimeBy)){
#       if(all(markTimeBy%in%1:igraph::vcount(g))){
#         tbreaks <- unique(markTimeBy)
#         if(!is.null(names(markTimeBy))){
#           tlabels <- names(unique(markTimeBy))
#         } else {
#           tlabels <- paste(tbreaks)
#         }
#       }
#     }
#     if(markTimeBy){
#       tbreaks <- which(diff(c(g$layout[1,1],g$layout[,2],g$layout[1,2])>=g$layout[1,2])!=0)
#       if(max(tbreaks)>igraph::vcount(g)){tbreaks[which.max(tbreaks)]<-igraph::vcount(g)}
#       if(min(tbreaks)>1){tbreaks<- c(1,tbreaks)}
#       tlabels <- paste(tbreaks)
#     }
#   }
#
#   if(max(tbreaks)!=igraph::vcount(g)){
#     tbreaks[which.max(tbreaks)]<-igraph::vcount(g)
#     tlabels <- paste(tbreaks)
#   }
#   if(min(tbreaks)>1){
#     tbreaks<- c(1,tbreaks)
#     tlabels <- paste(tbreaks)
#   }
#
#   if(is.null(markEpochsBy)){
#     markEpochsBy <- character(igraph::vcount(g))
#     for(i in 1:(length(tbreaks)-1)){
#       markEpochsBy[tbreaks[i]:tbreaks[i+1]] <- rep(paste0(tbreaks[i],"-",tbreaks[i+1]),length(tbreaks[i]:tbreaks[i+1]))
#     }
#     if(!is.null(epochColours)){
#       if(length(unique(markEpochsBy))>length(unique(epochColours))){
#         warning("Number of unique epochs is unequal to number of unique colours!\nUsing default colour scheme.")
#         epochColours <- NULL
#       }
#     }
#   }
#
#   g <- plotNET_groupColour(g,
#                            groups = markEpochsBy,
#                            colourV = TRUE,
#                            colourE = TRUE,
#                            groupColours = epochColours,
#                            defaultEdgeColour = defaultEdgeColour,
#                            doPlot = FALSE)
#
#   size <- 1
#   if(!is.null(igraph::V(g)$size)){
#     size <- igraph::V(g)$size
#   }
#   gNodes        <- as.data.frame(g$layout)
#   gNodes$ID     <- as.numeric(igraph::V(g))
#   gNodes$colour <- igraph::V(g)$colour
#   gNodes$labels <- factor(igraph::V(g)$groupnum, levels = unique(igraph::V(g)$groupnum), labels = unique(igraph::V(g)$group))
#   gNodes$size   <- size
#   gNodes$alpha  <- igraph::V(g)$alpha
#
#   width <- 1
#   if(!is.null(igraph::E(g)$width)){
#     width <- igraph::E(g)$width
#   }
#   gEdges        <- igraph::get.data.frame(g)
#   gEdges$from.x <- gNodes$V1[match(gEdges$from, gNodes$ID)]
#   gEdges$from.y <- gNodes$V2[match(gEdges$from, gNodes$ID)]
#   gEdges$to.x   <- gNodes$V1[match(gEdges$to, gNodes$ID)]
#   gEdges$to.y   <- gNodes$V2[match(gEdges$to, gNodes$ID)]
#   gEdges$width  <- width
#
#   if(is.null(vertexBorderColour))(
#     vBc <- gNodes$colour
#   ) else {
#     if(length(vertexBorderColour)==1|length(vertexBorderColour)==NROW(gNodes)){
#       vBc <- vertexBorderColour
#     } else {
#       warning("Invalid value(s) for vertexBorderColour, using default.")
#       vertexBorderColour <- "black"
#     }
#   }
#
#   # Fix same coords
#   sameID <- which(gEdges$from.x==gEdges$to.x)
#   if(length(sameID)>0){
#     gNodes$V2[gEdges$from[sameID]] <- gNodes$V2[gEdges$from[sameID]]+mean(diff(gNodes$V2))/2
#     gEdges$to.x[sameID] <- gEdges$to.x[sameID]+mean(diff(gEdges$to.x))/2
#     gEdges$to.y[sameID] <- gEdges$to.y[sameID]+mean(diff(gEdges$to.y))/2
#   }
#
#
#   gg <- ggplot(gNodes,aes(x=V1,y=V2)) +
#     geom_curve(data=gEdges, aes(x = from.x, xend = to.x, y = from.y, yend = to.y),
#                curvature = curvature,
#                angle = angle,
#                size = gEdges$width * scaleEdgeSize,
#                colour= gEdges$color,
#                alpha = alphaE) +
#     geom_point(aes(fill = labels, size = size), pch=21, colour = vBc, alpha = alphaV) +
#     ggtitle(label = title, subtitle = subtitle) +
#     scale_fill_manual(epochLabel, values = unique(gNodes$colour)) +
#     scale_size(sizeLabel, range = scaleVertexSize)
#
#   if(showEpochLegend){
#     gg <- gg + guides(fill = guide_legend(title.position = "top",
#                                           byrow = TRUE,
#                                           override.aes = list(size=5, order = 0)))
#   } else {
#     gg <- gg + guides(fill = "none")
#   }
#
#   if(showSizeLegend){
#     gg <- gg + guides(size = guide_legend(title.position = "top",
#                                           byrow = TRUE,
#                                           override.aes = list(legend.key.size = unit(1.2,"lines"), order = 1)))
#   } else {
#     gg <- gg + guides(size = "none")
#   }
#
#   if(!is.null(markTimeBy)){
#     gg <- gg + annotate("label", x=gNodes$V1[tbreaks], y=gNodes$V2[tbreaks], label = tlabels)
#   }
#
#   gg <- gg +
#     coord_fixed() +
#     theme_void() +
#     theme(legend.title = element_text(face="bold"),
#           legend.position =  "top",
#           legend.margin = margin(t = 0,r = 1,l = 1,0))
#
#   if(doPlot){
#     print(gg)
#   }
#
#   return(invisible(gg))
# }
#
#
#
# # State Space Grids ------
#
#
# # Import GrdWare files
# #
# # @param gwf_name Name of the GridWare project file. A directory named \code{../gwf_name_trjs} must be present at the location of the project file.
# # @param returnOnlyData Just return the data, do not return a list object with data, variable info and preferences.
# # @param saveLongFormat Save the long format trajectory data as a \code{.csv} file in the same location as \code{gwf_name}
# #
# # @return A data frame containing State Space Grid trajectories, or a list object with additional info.
# # @export
# #
# ssg_gwf2long <- function(gwf_name, delta_t = 0.01,returnOnlyData = TRUE, saveLongFormat = FALSE){
#   gwf_lines <- readr::read_lines(gwf_name)
#
#   var_list_b <- which(grepl("<Config>", gwf_lines, fixed = TRUE))+1
#   var_list_e <- which(grepl("MinReturns", gwf_lines, fixed = TRUE))-1
#
#   Nvars   <- length(gwf_lines[var_list_b:var_list_e])
#   MaxCols <-  max(plyr::laply(as.list(gwf_lines[var_list_b:var_list_e]), function(li){length(strsplit(li,"\t")[[1]])}))
#
#   var_list <- as.list(gwf_lines[var_list_b:var_list_e])
#   varinfo <-   plyr::ldply(var_list, function(li){
#     tmp <- strsplit(li,"\t")[[1]]
#     dat<- tibble::as_tibble(data.frame(var.name = tmp[3],
#                                        var.role = tmp[1],
#                                        var.type = tmp[2]))
#     if(dat$var.type%in%"integer"|dat$var.role%in%"state"){
#       dat$min <- tmp[4]
#       dat$max <- tmp[5]
#     } else {
#       dat$min <- NA
#       dat$max <- NA
#     }
#     if(dat$var.type%in%"categorical"){
#       Bvals <- 4
#     } else {
#       Bvals <- 6
#     }
#     Evals <- length(tmp)-Bvals
#     Nvals <- length(tmp[Bvals:(Bvals+Evals)])
#     dat2 <- tibble::as_tibble(matrix(NA,ncol=MaxCols-NCOL(dat), dimnames = list(NULL,paste0("value",1:(MaxCols-NCOL(dat))))))
#     if(length(tmp)>3){
#       dat2[1,1:Nvals] <- tmp[Bvals:(Bvals+Evals)]
#     }
#     return(cbind.data.frame(dat,dat2))
#   })
#   varinfo <- tibble::as_tibble(varinfo)
#
#   conf_list_b <- which(grepl("MinReturns", gwf_lines, fixed = TRUE))
#   conf_list_e <- which(grepl("</Config>", gwf_lines, fixed = TRUE))-1
#   conf_list <-  readr::read_tsv(paste0(c("setting\tvalue",gwf_lines[conf_list_b:conf_list_e]),collapse = "\n"))
#
#   traj_list_b <- which(grepl("<Trajectories>", gwf_lines, fixed = TRUE))+1
#   traj_list_e <- which(grepl("</Trajectories>", gwf_lines, fixed = TRUE))-1
#   traj_list <- readr::read_tsv(paste0(gwf_lines[traj_list_b:traj_list_e],collapse = "\n"))
#
#   state_vars <- sum(varinfo$var.role=="state")+1
#   dirname <- paste0(gsub(".gwf","",gwf_name,fixed = TRUE),"_trjs")
#   if(dir.exists(dirname)){
#     trj_data <- dir(dirname, pattern = "([.]trj)$", full.names =TRUE, include.dirs = FALSE)
#     names(trj_data) <-  dir(dirname, pattern = "([.]trj)$", full.names =FALSE, include.dirs = FALSE)
#
#     suppressMessages(suppressWarnings(data_long <- plyr::ldply(trj_data, function(p){
#       tmp <- readr::read_tsv(p)
#       for(l in 1:(NROW(tmp)-1)){
#         return(data.frame(time = seq(from = tmp$Onset[l],to = (tmp$Onset[l+1]-delta_t), by = delta_t), tmp[l,-1]))
#       }
#       }, .id = "Filename"))
#       )
#     suppressMessages(suppressWarnings(data_long <- dplyr::left_join(x = data_long,traj_list,by="Filename")))
#
#
#     attr(data_long,"Preferences") <-  conf_list
#     attr(data_long,"Variable definitions") <-  varinfo
#   }
#
#   if(returnOnlyData){
#     return(data_long)
#   } else {
#     return(list(variable_config = varinfo,
#                 settings_config = conf_list,
#                 trajectories_list  = traj_list,
#                 data_long          = data_long))
#   }
# }
#
#
# # ssg_winnowing
# #
# # @param durations durations frame
# # @param screeCut cutoff
# # @param timeLimit Stop the procedure after this time limit isn seconds (default = `120`)
# #
# # @return attractor frame
# # @export
# #
# ssg_winnowing <- function(durations, screeCut, timeLimit = 120){
#
#   durations$duration.time[is.na(durations$duration.time)] <- 0
#   winnowing <- durations %>% dplyr::filter(duration.time>0)
#   Ncells <- NROW(winnowing)
#   winnowingList <- scree <- heterogeneity <- list()
#   removed <- 0
#   run <- 1
#   exp <- sum(winnowing$duration.time, na.rm = TRUE)/NROW(winnowing)
#   #obs <- winnowing$duration.time-exp
#   heterogeneity[[run]] <- sum((winnowing$duration.time-exp)^2 / exp, na.rm = TRUE) / NROW(winnowing)
#   scree[[run]] <- 1
#   removed <- Ncells - NROW(winnowing)
#   winnowingList[[run]] <- winnowing
#
#   while(removed!=(Ncells-1)){
#     run <- run +1
#
#     winnowing <- winnowing %>% dplyr::filter(duration.time>min(winnowing$duration.time, na.rm = TRUE))
#     removed <- Ncells - NROW(winnowing)
#     exp <- sum(winnowing$duration.time, na.rm = TRUE)/NROW(winnowing)
#
#     heterogeneity[[run]] <- sum((winnowing$duration.time-exp)^2 / exp, na.rm = TRUE) / NROW(winnowing)
#     scree[[run]]         <-  (heterogeneity[[run-1]]-heterogeneity[[run]])/heterogeneity[[run-1]]
#     winnowingList[[run]] <- winnowing
#   }
#
#   useRun <- which(unlist(scree)<.5)[1]
#
#   return(list(
#     useRun = which(unlist(scree)<screeCut)[1],
#     attractors = winnowingList[[useRun]],
#     scree = scree,
#     heterogeneity = heterogeneity
#   )
#   )
# }
#
#
# # Factor labels
# #
# # @param observed_Ncat obsN
# # @param observed_labels obsL
# # @param expected_Ncat expN
# # @param expected_labels expL
# # @param varname varname
# #
# # @return character vector
# # @export
# #
# factor_obs_exp <- function(observed_Ncat, observed_labels, expected_Ncat=0, expected_labels="", varname = ""){
#
#   LABS <- ""
#   warningMessage <- ""
#   if(expected_Ncat>0){
#
#     if(expected_Ncat==observed_Ncat){
#       if(!identical(sort(observed_labels),sort(expected_labels))){
#         #  warningMessage <- paste0("User defined and observed state labels are different. User defined labels will be used.")
#         newN <- sum(expected_labels%in%observed_labels, na.rm = TRUE)
#         if(newN < observed_Ncat){
#           warningMessage <- paste("Different user defined state labels from those observed. New user defined labels will be added.")
#           LABS  <- expected_labels[!expected_labels%in%observed_labels]
#           LABS0 <- observed_labels[observed_labels%in%expected_labels]
#           origin <- "expected"
#           code <-  paste0("c('",paste0(LABS0, collapse="','"),"','",paste0(LABS, collapse="','"),"')")
#         }
#       } else {
#         LABS <- observed_labels
#         origin <- "observed"
#         code <- paste0("factor(",varname,", levels = c('",paste0(LABS, collapse="','"),"'))")
#       }
#     }
#
#     if(expected_Ncat<observed_Ncat){
#       warningMessage <- paste("Fewer user defined state labels than observed. Observed labels will be used.")
#       LABS <- observed_labels
#       origin <- "observed"
#       code <- paste0("factor(",varname,", levels = c('",paste0(LABS, collapse="','"),"'))")
#     }
#     if(expected_Ncat>observed_Ncat){
#       newN <- sum(expected_labels%in%observed_labels, na.rm = TRUE)
#       if(newN <observed_Ncat){
#         warningMessage <- paste("More user defined state labels than observed. New user defined labels will be added.")
#         LABS  <- expected_labels[!expected_labels%in%observed_labels]
#         LABS0 <- observed_labels[observed_labels%in%expected_labels]
#         origin <- "expected"
#         code <-  paste0("c('",paste0(LABS0, collapse="','"),"','",paste0(LABS, collapse="','"),"')")
#       } else {
#         warningMessage <- paste("More user defined state labels than observed. User defined labels will be used.")
#         LABS <- expected_labels
#         origin <- "expected"
#         code <- paste0("factor(",varname,", levels = c('",paste0(LABS, collapse="','"),"'))")
#       }
#     }
#   } else {
#     LABS <- observed_labels
#     origin <- "observed"
#     code <- paste0("factor(",varname,", levels = c('",paste0(LABS, collapse="','"),"'))")
#   }
#   attr(LABS,"varname") <- varname
#   attr(LABS,"warningMessage") <- warningMessage
#   attr(LABS,"origin") <- origin
#   attr(LABS,"code") <- code
#   return(LABS)
# }
#
# # FD estimators ----------------------------------------------
#
#
# FDrel <- function(g){
#   d<-igraph::degree(g,mode="all")
#   nbreaks <- round(length(igraph::V(g))/2)-1
#   y<-graphics::hist(d,breaks=nbreaks,plot=FALSE)$density
#   y<-y[y>0]
#   return(FD <- -sum(y*log2(y))/-(log2(1/length(y))))
# }
#
#
# # Informed Dimension estimate from Spectral Slope (aplha)
# #
# # @description Conversion formula: From periodogram based self-affinity parameter estimate (\code{sa}) to an informed estimate of the (fractal) dimension (FD).
# # @param sa Self-Affinity parameter estimate based on PSD slope (e.g., \code{\link{fd_psd}}))
# # @param ... Other arguments
# #
# # @return An informed estimate of the Fractal Dimension, see Hasselman(2013) for details.
# # @export
# #
# # @details The spectral slope will be converted to a dimension estimate using:
# #
# # \deqn{D_{PSD}\approx\frac{3}{2}+\frac{14}{33}*\tanh\left(Slope * \ln(1+\sqrt{2})\right)}
# #
# # @author Fred Hasselman
# # @references Hasselman, F. (2013). When the blind curve is finite: dimension estimation and model inference based on empirical waveforms. Frontiers in Physiology, 4, 75. \url{http://doi.org/10.3389/fphys.2013.00075}
# #
# #
# sa2fd_psd <- function(sa, ...){return(round(3/2 + ((14/33)*tanh(sa*log(1+sqrt(2)))), digits = 2))}
#
#
# # Informed Dimension estimate from DFA slope (H)
# #
# # @description Conversion formula: Detrended Fluctuation Analysis (DFA) estimate of the Hurst exponent (a self-affinity parameter \code{sa}) to an informed estimate of the (fractal) dimension (FD).
# #
# # @param sa Self-Afinity parameter estimate based on DFA slope (e.g., \code{\link{fd_sda}})).
# # @param ... Other arguments
# #
# # @return An informed estimate of the Fractal Dimension, see Hasselman(2013) for details.
# #
# # @export
# #
# # @details The DFA slope (H) will be converted to a dimension estimate using:
# #
# # \deqn{D_{DFA}\approx 2-(\tanh(\log(3)*sa)) }{D_{DFA} ≈ 2-(tanh(log(3)*sa)) }
# #
# #
# # @author Fred Hasselman
# # @references Hasselman, F. (2013). When the blind curve is finite: dimension estimation and model inference based on empirical waveforms. Frontiers in Physiology, 4, 75. \url{http://doi.org/10.3389/fphys.2013.00075}
# #
# sa2fd_dfa <- function(sa, ...){return(round(2-(tanh(log(3)*sa)), digits = 2))}
#
#
# # Informed Dimension estimate from SDA slope.
# #
# # @description Conversion formula: Standardised Dispersion Analysis (SDA) estimate of self-affinity parameter (\code{SA}) to an informed estimate of the fractal dimension (FD).
# #
# # @param sa Self-afinity parameter estimate based on SDA slope (e.g., \code{\link{fd_sda}})).
# # @param ... Other arguments
# #
# # @details
# #
# #
# # Note that for some signals different PSD slope values project to a single SDA slope. That is, SDA cannot distinguish dplyr::between all variaties of power-law scaling in the frequency domain.
# #
# # @return An informed estimate of the Fractal Dimension, see Hasselman(2013) for details.
# # @export
# #
# # @author Fred Hasselman
# # @references Hasselman, F. (2013). When the blind curve is finite: dimension estimation and model inference based on empirical waveforms. Frontiers in Physiology, 4, 75. \url{http://doi.org/10.3389/fphys.2013.00075}
# #
# #
# sa2fd_sda <- function(sa, ...){return(1-sa)}
#
#
#
# # Relative Roughness ----
#
#
# # Relative Roughness
# #
# # Relative Rougness is a ratio of local variance (autocovariance at lag-1) to global variance (autocovariance at lag-0) that can be used to classify different 'noises'.
# #
# #\deqn{RR = 2 * \left[1 − \frac{\gamma(y)}{Var(y)}\right]}{RR = 2 * [ 1 − autoCov-1(y) / Var(y) ]}
# #
# # @param y A numeric vector.
# #
# # @return The Relative Roughness of y, the values of local and global variance are returned as attributes
# # @export
# #
# # @family Fluctuation Analyses
# #
# # @references
# # \itemize{
# # \item{Marmelat, V., Torre, K., & Delignieres, D. (2012). Relative roughness: an index for testing the suitability of the monofractal model. \emph{Frontiers in Physiology, 3}, 208.}}
# #
# #
# fd_RR <- function(y){
#   # lag.max = n gives autocovariance of lags 0 ... n,
#   VAR  <- stats::acf(y, lag.max = 1, type = 'covariance', plot=FALSE)
#   # RR formula
#   RelR   <- 2*(1-VAR$acf[2] / VAR$acf[1])
#   # Add some attributes to the output
#   attributes(RelR) <- list(localAutoCoVariance = VAR$acf[2], globalAutoCoVariance = VAR$acf[1])
#   return(RelR)
# }
#
#
# # @title Power Spectral Density Slope (PSD).
#
# # @description Estimate Alpha, Hurst Exponent and Fractal Dimension through log-log slope.
# #
# # @param y    A numeric vector or time series object.
# # @param fs Sample rate (default = \code{NULL})
# # @param nfft Number of frequencies to estimate (default = Nyquist)
# # @param standardise standardise the series (default = \code{TRUE}).
# # @param detrend    Subtract linear trend from the series (default = \code{TRUE}).
# # @param polyOrder Order of polynomial for detrending (default = \code{1}).
# # @param doPlot    Return the log-log spectrum with linear fit (default = \code{TRUE}).
# # @param returnPlot Return the plot objects (default = \code{FALSE}).
# # @param returnPLAW Return the power law data (default = \code{FALSE})
# # @param returnInfo Return all the data used in DFA (default = \code{FALSE})
# # @param silent Run in silent-ish mode (default = \code{TRUE)})
# # @param noTitle No title on plots.
# # @param tsName Name of y.
# #
# # @author Fred Hasselman
# #
# # @references Hasselman, F. (2013). When the blind curve is finite: dimension estimation and model inference based on empirical waveforms. Frontiers in Physiology, 4, 75. \url{http://doi.org/10.3389/fphys.2013.00075}
# #
# # @return A list object containing:
# # \itemize{
# # \item A data matrix \code{PLAW} with columns \code{freq.norm}, \code{size} and \code{bulk}.
# # \item Estimate of scaling exponent \code{alpha} based on a fit over the lowest 25\% frequencies (\code{low25}), or using the HD estimate \code{HD}.
# # \item Estimate of the the Fractal Dimension (\code{FD}) using conversion formula's reported in Hasselman(2013).
# # \item Information output by various functions.
# # }
# #
# # @family Fluctuation Analyses
# #
# # @export
# #
# # @details Calls function \code{\link[sapa]{SDF}} to estimate the scaling exponent of a timeseries based on the periodogram frequency spectrum. After detrending and normalizing the signal (if requested), \code{SDF} is called using a Tukey window (\code{raised cosine \link[sapa]{taper}}).
# #
# # A line is fitted on the periodogram in log-log coordinates. Two fit-ranges are used: The 25\% lowest frequencies and the Hurvich-Deo estimate (\code{\link[fractal]{HDEst}}).
# #
# fd_psd <- function(y, fs = NULL, nfft = NA, standardise = c("none","mean.sd","median.mad")[2], detrend = TRUE, polyOrder = 1, doPlot = TRUE, returnPlot = TRUE, returnPLAW = FALSE, returnInfo = FALSE, silent = TRUE, noTitle = FALSE, tsName = ""){
#
#   y_ori <- y
#
#   if(!stats::is.ts(y)){
#     if(is.null(fs)){fs <- 1}
#     y <- stats::ts(y, frequency = fs)
#     if(!silent){cat("\n\nfd.psd:\tSample rate was set to 1.\n\n")}
#   }
#
#   N <- NROW(y)
#
#   # Simple linear detrending.
#   if(detrend){
#     y <- stats::ts(ts_detrend(as.vector(y),polyOrder = polyOrder), frequency = fs)
#   }
#
#   # standardise using N instead of N-1.
#   # Standardise by N
#   if(any(standardise%in%c("mean.sd","median.mad"))){
#     y <- stats::ts(ts_standardise(as.vector(y), type = standardise,  adjustN = FALSE), frequency = fs)
#   }
#
#   # Number of frequencies estimated cannot be set! (defaults to Nyquist)
#   # Use Tukey window: cosine taper with r = 0.5
#
#   # fast = TRUE ensures padding with zeros to optimize FFT to highly composite number.
#   # However, we just pad to nextPow2, except if length already is a power of 2.
#   #npad <- 1+(stats::nextn(N,factors=2)-N)/N
#   if(is.na(nfft)|nfft<=2|nfft>N){
#     nfft <- stats::nextn(N,factors = 2)
#     if(nfft>N){
#       y <- stats::ts(ts_trimfill(x=1:nfft, y=as.vector(y),action = "fill")$y, frequency = fs)
#     }
#   }
#
#   # if(N==npad) npad = 0
#   # psd  <- stats::spec.pgram(y, fast = FALSE, demean=FALSE, detrend=FALSE, plot=FALSE, pad=npad, taper=0.5)
#
#   Tukey <- sapa::taper(type="raised cosine", flatness = 0.5, n.sample = NROW(y))
#   psd   <- sapa::SDF(y, taper. = Tukey)
#
#   powspec <- cbind.data.frame(freq.norm = attr(psd, "frequency")[-1], size = attr(psd, "frequency")[-1]*stats::frequency(y), bulk = as.matrix(psd)[-1])
#
#   powspec$size.log <- log(powspec$size)
#   powspec$bulk.log <- log(powspec$bulk)
#
#   # First check the global slope for anti-persistent noise (GT +0.20)
#   # If so, fit the line starting from the highest frequency
#   nr     <- length(powspec[,1])
#   lsfit  <- stats::lm(powspec$bulk.log[1:nr] ~ powspec$size.log[1:nr])
#   glob   <- stats::coef(lsfit)[2]
#
#   # General guideline: fit over 25% frequencies
#   # If signal is continuous (sampled) consider Wijnants et al. (2013) log-log fitting procedure
#   nr <- fractal::HDEst(NFT = length(powspec$bulk[1:nr]), sdf = as.vector(powspec$bulk[1:nr]))
#
#   exp1 <- fractal::hurstSpec(y, sdf.method="direct", freq.max = 0.25, taper.=Tukey )
#   if(nr>=length(powspec$freq.norm)){nr <- length(powspec$freq.norm)-1}
#   exp2 <- fractal::hurstSpec(y, sdf.method="direct", freq.max = powspec$freq.norm[nr], taper.=Tukey)
#
#   ifelse((glob > 0.2), {
#     lmfit1 <- stats::lm(rev(powspec$bulk.log[powspec$size<=0.25]) ~ rev(powspec$size.log[powspec$size<=0.25]))
#     lmfit2 <- stats::lm(rev(powspec$bulk.log[1:nr]) ~ rev(powspec$size.log[1:nr]))
#   },{
#     lmfit1 <- stats::lm(powspec$bulk.log[powspec$size<=0.25] ~ powspec$size.log[powspec$size<=0.25])
#     lmfit2 <- stats::lm(powspec$bulk.log[1:nr] ~ powspec$size.log[1:nr])
#   })
#
#   outList <- list(
#     PLAW  = powspec,
#     low25 = list(y = y, antiper = (glob > 0.2), nfft = sum(powspec$size<=0.25), int = stats::coef(lmfit1)[1], sap = stats::coef(lmfit1)[2], H = exp1, FD = sa2fd_psd(stats::coef(lmfit1)[2]), fitlm1 = lmfit1, method = paste0("lowest 25% (n = ",sum(powspec$size<=0.25),")")),
#     HD    = list(y = y, antiper = (glob > 0.2), nfft = nr, int = stats::coef(lmfit2)[1], sap = stats::coef(lmfit2)[2], H = exp2, FD = sa2fd_psd(stats::coef(lmfit2)[2]), fitlm2 = lmfit2, method = paste0("Hurvich-Deo estimate (n = ",nr,")")),
#     info  = psd)
#
#   if(noTitle){title <- ""} else {title <- "Time series"}
#
#   tsData <- data.frame(y=c(as.numeric(y_ori),as.numeric(y)),
#                        x=c(time(y_ori),time(y)),
#                        label = c(rep(tsName,NROW(y_ori)),rep(paste(tsName," adj."),NROW(y))),
#                        stringsAsFactors = FALSE)
#
#
#   g1 <- ggplot2::ggplot(tsData,ggplot2::aes_(x=~x,y=~y)) +
#     ggplot2::geom_line() +
#     ggplot2::facet_grid(label ~ ., scales = "free") +
#     ggplot2::scale_x_continuous("Time", expand=c(0,0)) +
#     ggplot2::scale_y_continuous("", expand=c(0,0)) +
#     ggplot2::ggtitle(label = title,
#                      subtitle = paste0("Standardisation: ",standardise, " | Detrended: ",ifelse(detrend,paste("Yes (order = ",polyOrder),"No"))) +
#     ggplot2::theme_bw() +
#     ggplot2::theme(strip.text = ggplot2::element_text(face="bold"))
#
#
#   if(noTitle){title <- ""} else {title <- "log-log plot"}
#
#   g2 <- plotFA_loglog(outList,
#                       title = title,
#                       subtitle = paste0("lowest 25%: Slope = ",round(outList$low25$sap,digits=2)," | Hasselman's informed FD = ",round(outList$low25$FD,digits = 2),"\nHurvich-Deo: Slope = ",round(outList$HD$sap,digits = 2)," | Hasselman's informed FD = ",round(outList$HD$FD,digits = 2)),
#                       xlabel = "Frequencies",
#                       ylabel = "Power",
#                       logBase = "e")
#
#   if(doPlot){
#     multi_PLOT(g1,g2)
#   }
#
#   if(returnPlot){
#     outList$plots <- list(g1=g1, g2=g2)
#   }
#
#
#   # old<- ifultools::splitplot(2,1,1)
#   # graphics::plot(y,ylab = "Y", main = paste0('Lowest 25%    sap: ', round(stats::coef(lmfit1)[2],digits=2), ' | H:', round(exp1,digits=2), ' | FD:',round(sa2fd_psd(stats::coef(lmfit1)[2]),digits=2),'\nHurvic-Deo    sap: ', round(stats::coef(lmfit2)[2],digits=2), ' | H:', round(exp2,digits=2), ' | FD:',round(sa2fd_psd(stats::coef(lmfit2)[2]),digits=2)))
#   # ifultools::splitplot(2,1,2)
#   # graphics::plot(log(powspec$bulk) ~ log(powspec$size), xlab="log(Frequency)", ylab = "log(Power)")
#   # graphics::lines(log(powspec$size[powspec$size<=0.25]), stats::predict(lmfit1),lwd=3,col="darkred")
#   # graphics::lines(log(powspec$size[1:nr]), stats::predict(lmfit2),lwd=3,col="darkblue")
#   # graphics::legend("bottomleft",c(paste0("lowest 25% (n = ",sum(powspec$size<=0.25),")"), paste0("Hurvic-Deo estimate (n = ",nr,")")), lwd=c(3,3),col=c("darkred","darkblue"), cex = .8)
#   # graphics::par(old)
#   #}
#
#   return(outList[c(returnPLAW,TRUE,TRUE,returnInfo,returnPlot)])
# }
#
#
# # fd_sda
# #
# # @title Standardised Dispersion Analysis (SDA).
# #
# # @param y    A numeric vector or time series object.
# # @param fs Sample rate (default = NULL)
# # @param standardise standardise the series (default = "mean.sd")
# # @param detrend Subtract linear trend from the series (default = FALSE)
# # @param polyOrder Order of detrending polynomial
# # @param adjustSumOrder  Adjust the time series (summation or differencing), based on the global scaling exponent, see e.g. <https://www.frontiersin.org/files/Articles/23948/fphys-03-00141-r2/image_m/fphys-03-00141-t001.jpg>{Ihlen (2012)} (default = `FALSE`)
# # @param scaleMax   Maximum scale to use
# # @param scaleMin   Minimium scale to use
# # @param scaleResolution  The scales at which the standardised fluctuations are calculated as: `(scaleMax-scaleMin)/scaleResolution`
# # @param scaleS If not `NA`, it should be a numeric vector listing the scales on which to evaluate the fluctuations. Arguments `scaleMax, scaleMin, scaleResolution` will be ignored.
# # @param overlap Turn SDA into a sliding window analysis. A number in `[0 ... 1]` representing the amount of 'bin overlap'. If `length(y) = 1024` and overlap is `.5`, a scale of `4` will be considered a sliding window of size `4` with stepsize `floor(.5 * 4) = 2` (default = `0`)
# # @param minData Minimum number of data points in a bin needed to calculate standardised dispersion
# # @param doPlot   Output the log-log scale versus fluctuation plot with linear fit by calling function `plotFD_loglog()` (default = `TRUE`)
# # @param returnPlot Return ggplot2 object (default = `FALSE`)
# # @param returnPLAW Return the power law data (default = `FALSE`)
# # @param returnInfo Return all the data used in SDA (default = `FALSE`)
# # @param silent Silent-ish mode
# # @param noTitle Do not generate a title (only the subtitle)
# # @param tsName Name of y added as a subtitle to the plot
# #
# # @author Fred Hasselman
# # @references Hasselman, F. (2013). When the blind curve is finite: dimension estimation and model inference based on empirical waveforms. Frontiers in Physiology, 4, 75. <http://doi.org/10.3389/fphys.2013.00075>
# #
# # @return A list object containing:
# # \itemize{
# # \item A data matrix `PLAW` with columns `freq.norm`, `size` and `bulk`.
# # \item Estimate of scaling exponent `sap` based on a fit over the standard range (`fullRange`), or on a user defined range `fitRange`.
# # \item Estimate of the the Fractal Dimension (`FD`) using conversion formula's reported in Hasselman(2013).
# # \item Information output by various functions.
# # }
# #
# # @export
# #
# # @family Fluctuation Analyses
# #
# fd_sda <- function(y,
#                    fs = NULL,
#                    standardise = c("mean.sd","median.mad")[1],
#                    detrend = FALSE,
#                    polyOrder=1,
#                    adjustSumOrder = FALSE,
#                    scaleMin = 2,
#                    scaleMax = floor(log2(NROW(y)/2)),
#                    scaleResolution = 30,
#                    scaleS = NA,
#                    overlap = 0,
#                    minData = 4,
#                    doPlot = FALSE,
#                    returnPlot = FALSE,
#                    returnPLAW = FALSE,
#                    returnInfo = FALSE,
#                    silent = FALSE,
#                    noTitle = FALSE,
#                    tsName="y"){
#
#
#   if(!stats::is.ts(y)){
#     if(is.null(fs)){fs <- 1}
#     y <- stats::ts(y, frequency = fs)
#     if(!silent){cat("\n\nfd_sda:\tSample rate was set to 1.\n\n")}
#   }
#
#   y_ori <- y
#
#   N             <- length(y)
#   # Simple linear detrending.
#   if(detrend){y <- ts_detrend(y,polyOrder = polyOrder)} # y <- stats::ts(pracma::detrend(as.vector(y), tt = 'linear'), frequency = fs)
#   # standardise using N instead of N-1.
#   if(is.na(scaleS)){
#     scaleS <- unique(round(2^(seq(scaleMin, scaleMax, by=((scaleMax-scaleMin)/scaleResolution)))))
#   }
#
#   if(max(scaleS)>(NROW(y)/2)){
#     scaleS <- scaleS[scaleS<=(NROW(y)/2)]
#   }
#
#   if(!all(is.numeric(scaleS),length(scaleS)>0,scaleS%[]%c(2,(NROW(y)/2)))){
#     message("Something wrong with vector passed to scaleS.... \nUsing default: (scaleMax-scaleMin)/scaleResolution")
#   }
#
#   # Standardise by N
#   if(any(standardise%in%c("mean.sd","median.mad"))){
#     y <- ts_standardise(y, type = standardise,  adjustN = FALSE)
#   }
#
#   if(adjustSumOrder){
#     y       <- ts_sumorder(y, scaleS = scaleS, polyOrder = polyOrder, minData = minData)
#     Hadj    <- attr(y,"Hadj")
#     Hglobal <- attr(y,"Hglobal.excl")
#   } else {
#     Hadj    <- 0
#     Hglobal <- NA
#   }
#
#   out <- fractal::dispersion(y, front = FALSE)
#
#   fitRange <- which(lengths(lapply(out$scale, function(s){ts_slice(y,s)}))>=minData)
#
#   lmfit1        <- stats::lm(log(out$sd) ~ log(out$scale))
#   lmfit2        <- stats::lm(log(out$sd[fitRange]) ~ log(out$scale[fitRange]))
#
#   outList <- list(
#     PLAW  =  cbind.data.frame(freq.norm = stats::frequency(y)/out$scale,
#                               size = out$scale,
#                               bulk = out$sd),
#     fullRange = list(sap = stats::coef(lmfit1)[2],
#                      H = 1+stats::coef(lmfit1)[2] + Hadj,
#                      FD = sa2fd_sda(stats::coef(lmfit1)[2]),
#                      fitlm1 = lmfit1,
#                      method = paste0("Full range (n = ",length(out$scale),")\nSlope = ",round(stats::coef(lmfit1)[2],2)," | FD = ",round(sa2fd_sda(stats::coef(lmfit1)[2]),2))),
#     fitRange  = list(sap = stats::coef(lmfit2)[2],
#                      H = 1+stats::coef(lmfit2)[2] + Hadj,
#                      FD = sa2fd_sda(stats::coef(lmfit2)[2]),
#                      fitlm2 = lmfit2,
#                      method = paste0("Fit range (n = ",length(out$scale[fitRange]),")\nSlope = ",round(stats::coef(lmfit2)[2],2)," | FD = ",round(sa2fd_sda(stats::coef(lmfit2)[2]),2))),
#     info = out,
#     plot = NA,
#     analysis = list(
#       name = "Standardised Dispersion Analysis",
#       logBaseFit = "log",
#       logBasePlot = "e")
#   )
#
#   # if(doPlot|returnPlot){
#   #   if(noTitle){
#   #     title <- ""
#   #   } else {
#   #     title <- "log-log regression (SDA)"
#   #   }
#   #
#   #   if(doPlot){
#   #     g <- plotFD_loglog(fd.OUT = outList, title = title, subtitle = tsName, logBase = "e", ylabel = "Standardised Dispersion")
#   #     if(returnPlot){
#   #       outList$plot <- g
#   #     }
#   #   }
#   # }
#
#   if(noTitle){title <- ""} else {title <- "Time series"}
#
#   tsData <- data.frame(y=c(as.numeric(y_ori),as.numeric(y)),
#                        x=c(time(y),time(y)),
#                        label = c(rep(tsName,NROW(y_ori)),rep(paste(tsName," adj."),NROW(y))),
#                        stringsAsFactors = FALSE)
#
#
#   g1 <- ggplot2::ggplot(tsData,ggplot2::aes_(x=~x,y=~y)) +
#     ggplot2::geom_line() +
#     ggplot2::facet_grid(label ~ ., scales = "free") +
#     ggplot2::scale_x_continuous("Time", expand=c(0,0)) +
#     ggplot2::scale_y_continuous("", expand=c(0,0)) +
#     ggplot2::ggtitle(label = title, subtitle = paste0("Standardisation: ",standardise, " | Summation order",ifelse(is.na(Hglobal),"",paste0(" (H = ",round(Hglobal,digits = 2),")"))," adjustment: ", Hadj," | Detrending order: ",polyOrder)) +
#     ggplot2::theme_bw() +
#     ggplot2::theme(strip.text = ggplot2::element_text(face="bold"))
#
#
#   if(noTitle){title <- ""} else {title <- "log-log plot"}
#
#   g2 <- plotFA_loglog(outList, title = title, subtitle = paste0("Full range: Slope = ",round(outList$fullRange$sap,digits=2)," | H = ",round(outList$fullRange$H,digits = 2), " | Hasselman's informed FD = ",round(outList$fullRange$FD,digits = 2),"\nExcl large: Slope = ",round(outList$fitRange$sap,digits = 2)," | H = ",round(outList$fitRange$H,digits = 2), " | Hasselman's informed FD = ",round(outList$fitRange$FD,digits = 2)) ,logBase = "2")
#
#   if(doPlot){
#     multi_PLOT(g1,g2)
#   }
#
#   if(returnPlot){
#     outList$plots <- list(g1=g1, g2=g2)
#   }
#
#
#   return(outList[c(returnPLAW,TRUE,TRUE,returnInfo,returnPlot)])
# }
#
# #   if(returnInfo){returnPLAW<-TRUE}
# #
# #   if(!silent){
# #     cat("\n~~~o~~o~~casnet~~o~~o~~~\n")
# #     cat(paste("\n",outList$analysis$name,"\n\n",outList$fullRange$method,"\n\n",outList$fitRange$method))
# #     cat("\n\n~~~o~~o~~casnet~~o~~o~~~\n")
# #   }
# #
# #   return(invisible(outList[c(returnPLAW,TRUE,TRUE,returnInfo,returnPlot, TRUE)]))
# #
# # }
#
#
# # fd_dfa
# #
# # @title Detrended Fluctuation Analysis (DFA)
# #
# # @param y    A numeric vector or time series object.
# # @param fs   Sample rate
# # @param removeTrend Method to use for detrending, see \code{\link[fractal]{DFA}} (default = "poly")
# # @param polyOrder Order of polynomial trend to remove if \code{removeTrend = "poly"}
# # @param standardise Standardise by the series using \code{\link[casnet]{ts_standardise}} with \code{adjustN = FALSE} (default = "mean.sd")
# # @param adjustSumOrder  Adjust the time series (summation or differencing), based on the global scaling exponent, see e.g. [Ihlen (2012)](https://www.frontiersin.org/files/Articles/23948/fphys-03-00141-r2/image_m/fphys-03-00141-t001.jpg) (default = \code{TRUE})
# # @param scaleMax   Maximum scale to use
# # @param scaleMin   Minimium scale to use
# # @param scaleResolution  The scales at which detrended fluctuation will be evaluated will are calculatd as: \code{(scaleMax-scaleMin)/scaleResolution}
# # @param scaleS If not \code{NA}, it should be a numeric vector listing the scales on which to evaluate the detrended fluctuations. Arguments \code{scaleMax, scaleMin, scaleResolution} will be ignored.
# # @param overlap Turn DFA into a sliding window analysis. A number in \code{[0 ... 1]} representing the amount of 'bin overlap'. If \code{length(y) = 1024} and overlap is \code{.5}, a scale of \code{4} will be considered a sliding window of size \code{4} with stepsize \code{floor(.5 * 4) = 2}. The detrended fluctuation in   For scale \code{128} this will be  (default = \code{0})
# # @param minData Minimum number of data points in a bin needed to calculate detrended fluctuation
# # @param doPlot   Return the log-log scale versus fluctuation plot with linear fit (default = \code{TRUE}).
# # @param returnPLAW Return the power law data (default = \code{FALSE})
# # @param returnInfo Return all the data used in DFA (default = \code{FALSE})
# # @param silent Silent-ish mode
# # @param noTitle Do not generate a title (only the subtitle)
# # @param ... Other arguments
# #
# #
# # @return Estimate of Hurst exponent (slope of \code{log(bin)} vs. \code{log(RMSE))} and an FD estimate based on Hasselman(2013)
# # A list object containing:
# # \itemize{
# # \item A data matrix \code{PLAW} with columns \code{freq.norm}, \code{size} and \code{bulk}.
# # \item Estimate of scaling exponent \code{sap} based on a fit over the standard range (\code{fullRange}), or on a user defined range \code{fitRange}.
# # \item Estimate of the the Fractal Dimension (\code{FD}) using conversion formula's reported in Hasselman(2013).
# # \item Information output by various functions.
# # }
# #
# # @export
# #
# # @author Fred Hasselman
# # @references Hasselman, F. (2013). When the blind curve is finite: dimension estimation and model inference based on empirical waveforms. Frontiers in Physiology, 4, 75. \url{http://doi.org/10.3389/fphys.2013.00075}
# #
# # @family Fluctuation Analyses
# #
# #
# fd_dfa <- function(y,
#                    fs = NULL,
#                    removeTrend = c("poly","adaptive","bridge")[1],
#                    polyOrder=1,
#                    standardise = c("none","mean.sd","median.mad")[2],
#                    adjustSumOrder = TRUE,
#                    scaleMin = 2,
#                    scaleMax = floor(log2(NROW(y)/2)),
#                    scaleResolution = 30,
#                    scaleS = NA, overlap = 0, minData = 4, doPlot = TRUE, returnPlot = FALSE, returnPLAW = FALSE, returnInfo = FALSE, silent = TRUE, noTitle = FALSE, tsName="y", ...){
#
#   y_ori <- y
#
#   if(!stats::is.ts(y)){
#     if(is.null(fs)){fs <- 1}
#     y <- stats::ts(y, frequency = fs)
#     cat("\n\nfd.dfa:\tSample rate was set to 1.\n\n")
#   }
#
#   if(is.na(scaleS)){
#     scaleS <- unique(round(2^(seq(scaleMin, scaleMax, by=((scaleMax-scaleMin)/scaleResolution)))))
#   }
#
#   if(max(scaleS)>(NROW(y)/2)){
#     scaleS <- scaleS[scaleS<=(NROW(y)/2)]
#   }
#
#   if(!all(is.numeric(scaleS),length(scaleS)>0,scaleS%[]%c(2,(NROW(y)/2)))){
#     message("Something wrong with vector passed to scaleS.... \nUsing default: (scaleMax-scaleMin)/scaleResolution")
#   }
#
#   # Standardise by N
#   if(any(standardise%in%c("mean.sd","median.mad"))){
#     y <- ts_standardise(y, type = standardise,  adjustN = FALSE)
#   }
#
#
#   if(adjustSumOrder){
#     y       <- ts_sumorder(y_ori, scaleS = scaleS, polyOrder = polyOrder, minData = minData)
#     Hadj    <- attr(y,"Hadj")
#     Hglobal <- attr(y,"Hglobal.excl")
#   } else {
#     Hadj    <- 0
#     Hglobal <- NA
#   }
#
#   # Integrate the series
#   if(standardise%in%"none"){
#     y <- ts_integrate(ts_center(y))
#   } else {
#     y <- ts_integrate(y)
#   }
#
#   TSm    <- as.matrix(cbind(t=1:NROW(y),y=y))
#   DFAout <- monoH(TSm = TSm, scaleS = scaleS, polyOrder = polyOrder, returnPLAW = TRUE, returnSegments = TRUE)
#
#   fitRange <- which(lapply(DFAout$segments,NROW)>=minData)
#
#   lmfit1        <- stats::lm(DFAout$PLAW$bulk.log2 ~ DFAout$PLAW$size.log2, na.action=stats::na.omit)
#   H1  <- lmfit1$coefficients[2] + Hadj
#   lmfit2        <- stats::lm(DFAout$PLAW$bulk.log2[fitRange] ~ DFAout$PLAW$size.log2[fitRange], na.action=stats::na.omit)
#   H2  <- lmfit2$coefficients[2] + Hadj
#
#   outList <- list(
#     PLAW  =  DFAout$PLAW,
#     fullRange = list(y = y,
#                      sap = lmfit1$coefficients[2],
#                      Hadj = Hadj,
#                      H = H1,
#                      FD = sa2fd_dfa(lmfit1$coefficients[2]),
#                      fitlm1 = lmfit1,
#                      method = paste0("Full range (n = ",NROW(DFAout$PLAW$size),")")),
#     fitRange  = list(y = y,
#                      sap = lmfit2$coefficients[2],
#                      H = H2,
#                      Hadj = Hadj,
#                      FD = sa2fd_dfa(lmfit2$coefficients[2]),
#                      fitlm2 = lmfit2,
#                      method = paste0("Exclude large bin sizes (n = ",NROW(fitRange),")")),
#     info = list(fullRange=lmfit1,fitRange=lmfit2,segments=DFAout$segments))
#
#   if(noTitle){title <- ""} else {title <- "Time series"}
#
#   tsData <- data.frame(y=c(as.numeric(y_ori),as.numeric(y)),
#                        x=c(time(y),time(y)),
#                        label = c(rep(tsName,NROW(y_ori)),rep(paste(tsName," profile"),NROW(y))),
#                        stringsAsFactors = FALSE)
#
#
#   g1 <- ggplot2::ggplot(tsData,ggplot2::aes_(x=~x,y=~y)) +
#     ggplot2::geom_line() +
#     ggplot2::facet_grid(label ~ ., scales = "free") +
#     ggplot2::scale_x_continuous("Time", expand=c(0,0)) +
#     ggplot2::scale_y_continuous("", expand=c(0,0)) +
#     ggplot2::ggtitle(label = title, subtitle = paste0("Standardisation: ",standardise, " | Summation order",ifelse(is.na(Hglobal),"",paste0(" (H = ",round(Hglobal,digits = 2),")"))," adjustment: ", Hadj," | Detrending method: ",removeTrend)) +
#     ggplot2::theme_bw() +
#     ggplot2::theme(strip.text = ggplot2::element_text(face="bold"))
#
#
#   if(noTitle){title <- ""} else {title <- "log-log plot"}
#
#   g2 <- plotFA_loglog(outList, title = title, subtitle = paste0("Full range: Slope = ",round(outList$fullRange$sap,digits=2)," | H = ",round(outList$fullRange$H,digits = 2), " | Hasselman's informed FD = ",round(outList$fullRange$FD,digits = 2),"\nExcl large: Slope = ",round(outList$fitRange$sap,digits = 2)," | H = ",round(outList$fitRange$H,digits = 2), " | Hasselman's informed FD = ",round(outList$fitRange$FD,digits = 2)) ,logBase = "2")
#
#   if(doPlot){
#     multi_PLOT(g1,g2)
#   }
#
#   if(returnPlot){
#     outList$plots <- list(g1=g1, g2=g2)
#   }
#
#
#   return(outList[c(returnPLAW,TRUE,TRUE,returnInfo,returnPlot)])
# }
#
#
# # Calculate FD using Sevcik's method
# #
# # @param y A time series or numeric vector
# # @param detrend Subtract linear trend from the series (default = \code{TRUE}).
# # @param adjustSumOrder Adjust the time series (summation or differencing), based on the global scaling exponent, see e.g. [Ihlen (2012)](https://www.frontiersin.org/files/Articles/23948/fphys-03-00141-r2/image_m/fphys-03-00141-t001.jpg) (default = \code{TRUE})
# # @param doPlot   Return the log-log scale versus fluctuation plot with linear fit (default = \code{TRUE}).
# # @param returnPLAW Return the FD series (default = \code{FALSE})
# # @param returnInfo Return all the data used in DFA (default = \code{FALSE})
# # @param silent Silent-ish mode
# # @param ... Other arguments
# #
# # @author Fred Hasselman
# #
# # @return An FD estimate
# #
# # @export
# #
# # @references Sevcik, C. (1998). A procedure to Estimate the Fractal Dimension of Waveforms. Paper available at http://arxiv.org/pdf/1003.5266.pdf
# #
# fd_sev <- function(y, detrend = FALSE, adjustSumOrder = FALSE, doPlot = TRUE, returnPLAW = FALSE, returnInfo = FALSE, silent = TRUE, ...){
#
#
#
#   Hadj<-0
#   if(detrend){y <- ts_detrend(y)}
#   if(adjustSumOrder){
#     y <- ts_sumorder(y)
#     Hadj <- attr(y,"Hadj")
#   }
#
#   N <- NROW(y)
#   D.FD <- D.sd <- numeric(N)
#   D.L <- list()
#
#   L<-0
#   for(n in 1:N){
#     if(n>1){L <- cumsum(sqrt(diff(elascer(y[1:n]))^2 + (1/(n-1)^2)))}
#     D.FD[n] <- 1 + ((log(dplyr::last(L)+.Machine$double.eps) - log(2)) / log((2*(n-1))+.Machine$double.eps) )
#     D.sd[n] <- stats::var(L, na.rm = TRUE) / (dplyr::last(L)^2 * log((2*(n-1)^2)+.Machine$double.eps))
#
#     D.L[[n]] <- L
#
#   }
#
#   D.n <- lengths(D.sd)
#   D.SE <- D.sd / sqrt(D.n)
#   D.SE[is.na(D.SE)] <- 0
#
#
#   D.sd2 = ts_sd(D.FD,type = "unadjusted")
#   lengths(D.L)
#   if(doPlot){
#
#     # dfU <- data.frame(time=seq(0,1,length.out = N),y=elascer(y))
#     # ggU <- ggplot(dfU,aes_(x=time,y=y)) + geom_line() + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + coord_equal() + theme_bw() + theme(panel.grid = element_blank())
#     #
#     # dfOri <- data.frame(time=time(ts(y)),y=y)
#     # ggOri <- ggplot(dfOri,aes_(x=time,y=y)) + geom_line() + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + theme_bw() + theme(panel.grid = element_blank())
#     #
#     # dfFD <- data.frame(time=time(ts(y)),FD=D.FD,ci_lo=D.FD-(1.96*D.SE),ci_hi=D.FD+(1.96*D.SE))
#     # ggFD <- ggplot(dfFD,aes_(x=time,y=FD)) +  geom_ribbon(aes_(ymin=ci_lo, ymax=ci_hi),colour="grey70", fill = "grey70")+ geom_line() +  scale_x_continuous(expand = c(0,0)) + scale_y_continuous("Fractal Dimension",breaks = c(0.8,1,1.1,1.2,1.5,1.8),expand = c(0,0), limits = c(.8,2)) + theme_bw() + theme(panel.grid.major.x  = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())
#
#     graphics::plot.new()
#     old <- ifultools::splitplot(2,1,1)
#     #graphics::plot(y,ylab = "Y", main = paste0('FD: ', round(dplyr::last(D.FD), digits=2)))
#     graphics::plot(elascer(y)~ seq(0,1,length.out = N), ylab = "", xlab="", main = paste0('FD: ', round(dplyr::last(D.FD), digits=2)),xlim=c(0,1),ylim=c(0,1),pty="s", type="l")
#     ifultools::splitplot(2,1,2)
#     graphics::plot(D.FD ~ seq(0,1,length.out = N), xlab="Normalised time", ylab = "", type="l")
#     #graphics::legend("topleft",c(paste0("Full (n = ",length(DFAout$PLAW$scaleS),")"), paste0("Range (n = ",length(DFAout$PLAW$scaleS[fitRange]),")")), lwd=c(3,3),col=c("darkred","darkblue"), cex = .8)
#     graphics::par(old)
#
#   }
#
#   return(list(PLAW = D.FD,
#               fullRange  = list(sap = NA, H = NA, FD = dplyr::last(D.FD), fitlm1 = NA),
#               fitRange   = list(sap = NA, H = NA, FD = dplyr::last(D.FD), fitlm2 = NA),
#               info = list(FDseries=data.frame(y=y,FD=D.FD,sd=D.sd,sd2=D.sd2),L=D.L))[returnPLAW,TRUE,TRUE,returnInfo])
# }
#
#
# # Allan Variance Analysis
# #
# # @param y A numeric vector or time series object
# # @param fs Sample frequency in Hz
# # @param doPlot Produce a plot of the Allan variance?
# # @param useSD Use the standarddeviation instead of variance?
# #
# # @return A dataframe with for each scale \code{tau}, the Allan variance (or Allan factor for poisson processes), Alan standard deviation and error due to bin size
# # @export
# #
# fd_allan <- function(y, fs = stats::tsp(stats::hasTsp(y))[3], doPlot = FALSE, useSD=FALSE){
#
#   N <- NROW(y)
#   tau <- 1/fs
#   n <- ceiling((N - 1)/2)
#   p <- floor(log10(n)/log10(2))
#   av <- rep(0, p + 1)
#   time <- rep(0, p + 1)
#   error <- rep(0, p + 1)
#   for (i in 0:(p)) {
#     omega <- rep(0, floor(N/(2^i)))
#     TT <- (2^i) * tau
#     l <- 1
#     k <- 1
#     while (k <= floor(N/(2^i))) {
#       omega[k] <- sum(y[l:(l + ((2^i) - 1))])/(2^i)
#       l <- l + (2^i)
#       k <- k + 1
#     }
#     sumvalue <- 0
#     for (k in 1:(length(omega) - 1)) {
#       sumvalue <- sumvalue + (omega[k + 1] - omega[k])^2
#     }
#     av[i + 1] <- sumvalue/(2 * (length(omega) - 1))
#     time[i + 1] <- TT
#     error[i + 1] <- 1/sqrt(2 * ((N/(2^i)) - 1))
#   }
#
#   df <- data.frame(Tcluster = time, AT = av, ATsd= sqrt(av), error=error)
#
#   if(doPlot){
#     if(useSD){
#
#       if((sum(log10(df$ATsd)<0)/NROW(df))>.5){
#         lims <- c(min(df$ATsd),max(df$ATsd)^2)
#       }
#
#       g <-  ggplot2::ggplot(df,ggplot2::aes_(x=~Tcluster,y=~ATsd)) +
#         ggplot2::geom_path() +
#         ggplot2::geom_pointrange(ggplot2::aes_(ymin=~ATsd-(df$ATsd*df$error),ymax=~ATsd+(df$ATsd*df$error))) +
#         ggplot2::scale_y_continuous(trans = scales::log10_trans(),
#                            breaks = scales::trans_breaks("log10", function(x) 10^x),
#                            labels = scales::trans_format("log10", scales::math_format())
#         ) +
#         ggplot2::scale_x_continuous(trans = scales::log10_trans(),
#                            breaks = scales::trans_breaks("log10", function(x) 10^x),
#                            labels = scales::trans_format("log10", scales::math_format())) +
#         ggplot2::xlab("Cluster Times (T)") +
#         ggplot2::ylab("Allan Deviation") +
#         ggplot2::coord_equal() +
#         ggplot2::theme_bw()
#
#     } else {
#       g <-  ggplot2::ggplot(df,ggplot2::aes_(x=~Tcluster,y=~AT)) +
#         ggplot2::geom_path() +
#         ggplot2::geom_pointrange(ggplot2::aes_(ymin=~AT-(df$AT*df$error),ymax=~AT+(df$AT*df$error))) +
#         ggplot2::scale_y_continuous(trans = scales::log10_trans(),
#                            breaks = scales::trans_breaks("log10", function(x) 10^x),
#                            labels = scales::trans_format("log10", scales::math_format())) +
#         ggplot2::scale_x_continuous(trans = scales::log10_trans(),
#                            breaks = scales::trans_breaks("log10", function(x) 10^x),
#                            labels = scales::trans_format("log10", scales::math_format())) +
#         ggplot2::xlab("Cluster Times (T)") +
#         ggplot2::ylab("Allan Variance / Factor")  +
#         ggplot2::theme_bw()
#     }
#     graphics::plot(g)
#   }
#   return(df)
# }
#
# # Multi-Fractal DFA -----------------------------------------------------------------------------------------------
#
# # Multi-fractal Detrended Fluctuation Analysis
# #
# # @param signal    An input signal.
# # @param qq    A vector containing a range of values for the order of fluctuation \code{q}.
# # @param mins    Minimum scale to consider.
# # @param maxs    Maximum scale to consider.
# # @param ressc rescc
# # @param m m
# #
# # @return output
# # @export
# #
# # @family Fluctuation Analyses
# #
# MFDFA <- function(signal,qq=c(-10,-5:5,10),mins=6,maxs=12,ressc=30,m=1){
#
#   #   reload <- FALSE
#   #   if("signal" %in% .packages()){
#   #     warning("signal:poly is loaded and stats:poly is needed... will unload package:signal, compute slope, and reload...")
#   #     reload <- TRUE
#   #     detach("package:signal", unload=TRUE)
#   #   }
#   scale     <- round(2^(seq(mins,maxs,by=((maxs-mins)/ressc))))
#   segv      <- numeric(length(scale))
#   RMS_scale <- vector("list",length(scale))
#   qRMS      <- vector("list",length(qq))
#   Fq        <- vector("list",length(qq))
#   qRegLine  <- vector("list",length(qq))
#   Hq        <- numeric(length(qq))
#
#   y        <- cumsum(signal-mean(signal))
#   TSm      <- as.matrix(cbind(t=1:length(Y),y=Y))
#   Hglobal  <- monoH(TSm,scale)
#
#   Hadj <- 0
#   if((Hglobal>1.2)&(Hglobal<1.8)){
#     Y <- diff(signal)
#     Hadj=1}
#   if(Hglobal>1.8){
#     Y <- diff(diff(signal))
#     Hadj <- 2}
#   if(Hglobal<0.2){
#     Y <- cumsum(signal-mean(signal))
#     Hadj <- -1}
#   if(Hadj!=0){TSm  <- as.matrix(cbind(t=1:length(Y),y=cumsum(Y-mean(Y))))}
#
#   for(ns in seq_along(scale)){
#     RMS_scale[[ns]] <-plyr::ldply(ts_slice(TSm,scale[ns]),function(sv){return(sqrt(mean(ts_detrend(sv[,2]))^2))})
#     for(nq in seq_along(qq)){
#       qRMS[[nq]][1:length(RMS_scale[[ns]]$V1)] <- RMS_scale[[ns]]$V1^qq[nq]
#       Fq[[nq]][ns] <- mean(qRMS[[nq]][1:length(RMS_scale[[ns]]$V1)])^(1/qq[nq])
#       if(is.infinite(log2(Fq[[nq]][ns]))){Fq[[nq]][ns]<-NA}
#     }
#     Fq[[which(qq==0)]][ns] <- exp(0.5*mean(log(RMS_scale[[ns]]^2)))
#     if(is.infinite(log2(Fq[[which(qq==0)]][ns]))){Fq[[which(qq==0)]][ns]<-NA}
#   }
#
#   fmin<-1
#   fmax<-which(scale==max(scale))
#   #for(nq in seq_along(qq)){Hq[nq] <- stats::lm(log2(Fq[[nq]])~log2(scale))$coefficients[2]}
#   Hq <-plyr::ldply(Fq,function(Fqs){stats::lm(log2(Fqs[fmin:fmax])~log2(scale[fmin:fmax]),na.action=stats::na.omit)$coefficients[2]})
#
#   tq <- (Hq[,1]*qq)-1
#   hq <- diff(tq)/diff(qq)
#   Dq <- (qq[1:(length(qq)-1)]*hq) - (tq[1:(length(qq)-1)])
#
#   #if(reload==TRUE){library(signal,verbose=FALSE,quietly=TRUE)}
#
#   return(list(q=qq,Hq=Hq,tq=tq,hq=hq,Dq=Dq,Hglobal=Hglobal,Hadj=Hadj))
# }
#
#
# # mono Hurst
# #
# # @param TSm TS matrix with 2 columns \code{t} (1st) and \code{y} (second)
# # @param scaleS scales to evaluate
# # @param polyOrder If numeric: order to use for polynomial detrendiing, if "adaptive" will use the best fitting polynomial.
# #
# # @export
# # @keywords internal
# #
# monoH <- function(TSm, scaleS, polyOrder = 1, returnPLAW = FALSE, returnSegments = FALSE, removeRMSbelow = .Machine$double.eps){
#
#   dfaRMS_scale <- vector("list",length(scaleS))
#
#   if(max(scaleS)>NROW(TSm)/2){
#     scaleS <- scaleS[scaleS<=NROW(TSm)/2]
#   }
#   F2 <- numeric(length(scaleS))
#
#   for(ns in seq_along(scaleS)){
#     dfaRMS <- plyr::ldply(ts_slice(TSm[,2],scaleS[ns]), function(sv){return(sqrt(mean(ts_detrend(sv,polyOrder=polyOrder)^2,na.rm = TRUE)))})
#     dfaRMS$V1[dfaRMS$V1 <= removeRMSbelow^2] <- NA
#     dfaRMS_scale[[ns]] <- dfaRMS$V1
#     F2[ns] <- mean(dfaRMS_scale[[ns]]^2,na.rm = TRUE)^(1/2)
#     if(is.infinite(log2(F2[ns]))){F2[ns] <- NA}
#     rm(dfaRMS)
#   }
#
#   PLAW <- data.frame(size=scaleS%00%NA, bulk = F2%00%NA,
#                      size.log2=log2(scaleS)%00%NA, bulk.log2 = log2(F2)%00%NA)
#   H <- stats::lm(bulk.log2~size.log2, data = PLAW, na.action=stats::na.omit)$coefficients[2]
#   if(any(returnPLAW,returnSegments)){
#     return(list(H = H,
#                 PLAW = PLAW,
#                 segments = dfaRMS_scale)[c(TRUE,returnPLAW,returnSegments)])
#   } else {
#     return(H)
#   }
# }
#
#
# #
# # set.seed(100)
# # z <- dispersion(rnorm(1024))
# # graphics::plot(log(z$scale),log(z$sd))
# # #
#
# # trace(ts_detrend,edit=T)
# # seq(1,length(X),by=4096)
# #
# # z<-ts_slice(TSm,scale[1])
# # z[[1]][,2]
# #
# # Hglobal <-
# #
# # segments <- plyr::laply(scale,function(s) floor(length(X)/s))
# # IDv <- llply(segments,slice.index,)
# # segv <- function(X,segments){
# #
# #
# #
# #   seq((((v-1)*scale[ns])+1),(v*scale[ns]),length=scale[ns])
# #
# # }
# # for(ns in seq_along(scale)){
# #   segv[ns] <- floor(length(X)/scale[ns])
# #   for(v in 1:segv[ns]){
# #     Index <- seq((((v-1)*scale[ns])+1),(v*scale[ns]),length=scale[ns])
# #     Cslope<- polyfit(Index,X[Index],m)
# #     fit   <- polyval(Cslope,Index)
# #     RMS_scale[[ns]][v] <- sqrt(mean((X[Index]-fit)^2))
# #     rm(Cslope, fit, Index)
# #   }
# #   for(nq in seq_along(qq)){
# #     qRMS[[nq]][1:segv[ns]] <- RMS_scale[[ns]]^qq[nq]
# #     Fq[[nq]][ns] <- mean(qRMS[[nq]][1:segv[ns]])^(1/qq[nq])
# #   }
# #   Fq[[which(qq==0)]][ns] <- exp(0.5*mean(log(RMS_scale[[ns]]^2)))
# # }
# #
# # for(nq in seq_along(qq)){
# #   Cslope <- polyfit(log2(scale),log2(Fq[[nq]]),1)
# #   Hq[nq] <- Cslope[1]
# #   qRegLine[[nq]] <- polyval(Cslope,log2(scale))
# #   rm(Cslope)
# # }
# #
# # tq <- (Hq*qq)-1
# # hq <- diff(tq)/diff(qq)
# # Dq <- (qq[1:(length(qq)-1)]*hq) - (tq[1:(length(qq)-1)])
# #
# # graphics::plot(hq,Dq,type="l")
# #
# #
# # qq<-c(-10,-5,seq(-2,2,.1),5,10)
#
#
# # PLOTS -------------------------------------------------------------------
# #
# #
# # gg_theme
# #
# # @param type      One of \code{"clean"}, or \code{"noax"}
# #
# # @details Will generate a \code{"clean"} ggplot theme, or a theme without any axes (\code{"noax"}).
# #
# # Some scientific journals explicitly request the Arial font should be used in figures. This can be achieved by using \code{.afm} font format (see, e.g. http://www.pure-mac.com/font.html).
# #
# # @return A theme for \code{ggplot2}.
# # @export
# #
# # @examples
# # library(ggplot2)
# # g <- ggplot(data.frame(x = rnorm(n = 100), y = rnorm(n = 100)), aes(x = x, y = y)) + geom_point()
# # g + gg_theme()
# # g + gg_theme("noax")
# gg_theme <- function(type=c("clean","noax")){
#
#   if(length(type)>1){type <- type[1]}
#
#   switch(type,
#          clean = theme_bw(base_size = 16, base_family="sans") +
#            theme(axis.text.x     = element_text(size = 14),
#                  axis.title.y    = element_text(vjust = +1.5),
#                  panel.grid.major  = element_blank(),
#                  panel.grid.minor  = element_blank(),
#                  legend.background = element_blank(),
#                  legend.key = element_blank(),
#                  panel.border = element_blank(),
#                  panel.background = element_blank(),
#                  axis.line  = element_line(colour = "black")),
#          noax = theme(line = element_blank(),
#                       text  = element_blank(),
#                       title = element_blank(),
#                       plot.background = element_blank(),
#                       panel.border = element_blank(),
#                       panel.background = element_blank())
#   )
# }
#
# # gg_plotHolder
# #
# # @return A blank \code{ggplot2} object that can be used in concordance with \code{grid.arrange}.
# # @export
# #
# # @examples
# # # Create a plot with marginal distributions.
# # library(ggplot2)
# # library(scales)
# #
# # df <- data.frame(x = rnorm(n = 100),
# #                  y = rnorm(n = 100),
# #                  group = factor(sample(x=c(0,1),
# #                  size = 100, replace = TRUE)))
# #
# # scatterP <- ggplot(df, aes(x = x, y =y, colour = group)) +
# #                    geom_point() +
# #                    gg_theme()
# #
# # xDense <- ggplot(df, aes(x = x, fill = group)) +
# #                  geom_density(aes(y= ..count..),trim=FALSE, alpha=.5) +
# #                  gg_theme("noax") +
# #                  theme(legend.position = "none")
# #
# # yDense <- ggplot(df, aes(x = y, fill = group)) +
# #                  geom_density(aes(y= ..count..),trim=FALSE, alpha=.5) +
# #                  coord_flip() +
# #                  gg_theme("noax") +
# #                  theme(legend.position = "none")
# #
# # library(gridExtra)
# # grid.arrange(xDense,
# #              gg_plotHolder(),
# #              scatterP,
# #              yDense,
# #              ncol=2, nrow=2,
# #              widths=c(4, 1.4),
# #              heights=c(1.4, 4))
# gg_plotHolder <- function(){
#   return(ggplot2::ggplot() +
#            ggplot2::geom_blank(ggplot2::aes_(1,1)) +
#            ggplot2::theme(line = ggplot2::element_blank(),
#                  text  = ggplot2::element_blank(),
#                  title = ggplot2::element_blank(),
#                  plot.background = ggplot2::element_blank(),
#                  panel.border = ggplot2::element_blank(),
#                  panel.background = ggplot2::element_blank())
#   )
# }
#
#
#
# # Set Edge weights by group
# #
# #  Use a layout which takes a `weights`
# #
# # @param g  An igraph object whose edges (`get.edgelist(g)`) will be re-weighted according to the `membership` argument.
# # @param groups A named numeric vector with `length(V(g))` integers representing each group, or, a named character vector describing each group. If `names(groups)==NULL` then the names of the vector will be set as `names(groups) == V(g)$name`. If `V(g)$name==NULL`, the names of the vector will be set by the Vertex index
# # @param weigth.within The weight within a group (`default = 100`)
# # @param weight.between The weight within a group (`default = 1`)
# # @param preserve.weight.within If `E(g)$weights` is not `NULL`, try to preserve edge weigths within a group
# # @param preserve.weight.between If `E(g)$weights` is not `NULL`, try to preserve edge weigths between a groups
# # @param doPlot Plot the igraph object
# # @param returnOnlyWeights Do not return the graph, just the weights. If `FALSE` this will return the graph object, otherwis it returns `E(g)$weights`
# #
# # @return A numeric vector with `length(get.edgelist(g))` edge weights that will cluster groups defined in `membership` if a layout is used that can handle edge weights as a parameter (see examples).
# #
# # @export
# #
# # @family tools for plotting networks
# #
# # @examples
# # # Make a star graph and let the odd numbers cluster together
# # library(igraph)
# # g <-make_full_graph(10, directed=FALSE)
# # E(g)$width <- 3
# # V(g)$name <- paste(1:10)
# # membership <- rep(c(1,2),5)
# # names(membership) <- V(g)$name
# # E(g)$weight <- plotNET_groupWeight(g,membership,1000,10)
# # g$layout=layout.fruchterman.reingold(g,weights=E(g)$weight)
# # plot(g)
# #
# # # Make 3 groups by changing the 'membership' vector
# # membership[3:6] <- 3
# # names(membership) <- V(g)$name
# # E(g)$weight <- plotNET_groupWeight(g,membership,1000,10)
# # g$layout=layout.fruchterman.reingold(g,weights=E(g)$weight)
# # plot(g)
# #
# # # Use plotNET_groupColour for Vertex and Edge group colours
# # g <- plotNET_groupColour(g, membership, colourE=TRUE)
# # plot(g)
# #
# plotNET_groupWeight <- function(g, groups, weigth.within=100, weight.between=1, preserve.weight.within=FALSE, preserve.weight.between=FALSE, doPlot = FALSE, returnOnlyWeights = TRUE){
#   edgl <- igraph::get.edgelist(g)
#
#   for(r in seq_along(edgl[,1])){
#     row <- edgl[r,]
#     if(as.numeric(groups[which(names(groups)==row[1])])==as.numeric(groups[which(names(groups)==row[2])])){
#       igraph::E(g)$weight[r] <- weigth.within + ifelse(preserve.weight.within, igraph::E(g)$weight[r]%00%0, 0)
#     } else {
#       igraph::E(g)$weight[r] <- weight.between + ifelse(preserve.weight.between, igraph::E(g)$weight[r]%00%0, 0)
#     }
#   }
#   if(doPlot){
#     # graphics::plot.new()
#     graphics::plot(g)
#   }
#   if(returnOnlyWeights){
#     return(igraph::E(g)$weight)
#   } else {
#     return(invisible(g))
#   }
# }
#
#
# # Plot Network Based on RQA
# #
# # @param g An igraph object
# # @param labels Vertex labels
# # @param nodesize Set nodesizes by `degree(g, normalised = TRUE)` (default), `hubscore(g)$vector`, or, `strength(g)`, `eccentricity(g)`, `coreness(g)`. If a numeric value is passed all vertex sizes will be set to that value.
# # @param labelsize Set labelsize: "asnodesize" sets the `cex` for the labels to coincide with nodesize (with min of .4 and max of 1.1). A single numeric value sets the `cex` of all labels to that value. A numeric vector of length two, `c(min,max)` wil scale the node sizes to `min` and `max` which
# # @param edgeweight Set size of edges to `"E(g)$weight"` by passing "weight". If a single numeric value is provided all edges will be set to that value.
# # @param removeZeroDegree Remove vertices with `degree(g) == 0` (default = `TRUE`)
# # @param removeSelfLoops Calls `simplify(g)` (default = `TRUE`)
# # @param doPlot Plot the igraph object.
# #
# # @return an igraph object
# # @export
# #
# # @family tools for plotting networks
# #
# plotNET_prep <- function(g,
#                          labels     = NA,
#                          nodesize   = c("degree","hubscore","strength","eccentricity","coreness")[1],
#                          labelsize  = "asnodesize",
#                          edgeweight = "weight",
#                          removeZeroDegree = TRUE,
#                          removeSelfLoops  = TRUE,
#                          doPlot     = TRUE){
#
#   if(removeSelfLoops){
#     g <- igraph::simplify(g)
#   }
#   if(removeZeroDegree){
#     g <- igraph::delete.vertices(g,igraph::degree(g)==0)
#   }
#
#   rev <- NA
#   if(is.character(nodesize)){
#     switch(nodesize,
#            # degree   = rev <- elascer(log1p(igraph::degree(g,normalized = TRUE))),
#            # hubscore = rev <- elascer(log1p(igraph::hub_score(g)$vector))
#            # strength = rev <- elascer(log1p(igraph::strength(g)$vector)))
#            degree   = rev <- igraph::degree(g),
#            hubscore = rev <- igraph::hub_score(g)$vector,
#            strength = rev <- igraph::strength(g),
#            eccentricity = rev <- igraph::eccentricity(g),
#            coreness = rev <- igraph::coreness(g),
#     )
#   } else {
#     rev <- rep(as.numeric(nodesize),length(igraph::V(g)))
#   }
#
#   # set colors and sizes for vertices
#   #rev<-elascer(log1p(igraph::V(g)$degree))
#
#   igraph::V(g)$size        <- rev
#
#   rev <- rev/max(rev, na.rm = TRUE)
#   rev[rev<=0.2]<-0.2
#   rev[rev>=0.9]<-0.9
#   igraph::V(g)$rev <- rev
#
#   igraph::V(g)$color       <- grDevices::rgb(igraph::V(g)$rev, 1-igraph::V(g)$rev,  0, 1)
#
#   # set vertex labels and their colors and sizes
#   if(all(is.na(labels))){
#     igraph::V(g)$label       <- ""
#   } else {
#     igraph::V(g)$label       <- labels
#
#     if(labelsize == "asnodesize"){igraph::V(g)$label.cex <- elascer(igraph::V(g)$size,lo = .4, hi = 1.1)}
#     if(is.numeric(labelsize)&length(labelsize)==1){igraph::V(g)$label.cex <- labelsize}
#     if(is.numeric(labelsize)&length(labelsize)==2){igraph::V(g)$label.cex <-  elascer(igraph::V(g)$size, lo = labelsize[1], hi = labelsize[2])}
#
#     igraph::V(g)$label.color <- "black"
#
#     igraph::V(g)$label.family = "Helvetica"
#   }
#
#   if(igraph::ecount(g)>0){
#     if(edgeweight%in%"weight"){
#       if(igraph::is_weighted(g)){
#         igraph::E(g)$width <- elascer(igraph::E(g)$weight,lo = .8, hi = 5)
#       }
#     } else {
#       if(is.numeric(edgeweight)){
#         igraph::E(g)$width <- as.numeric(edgeweight)
#       } else {
#         igraph::E(g)$width <- 1
#       }
#     }
#     igraph::E(g)$color <- grDevices::rgb(0.5, 0.5, 0.5, 1)
#   }
#
#   if(doPlot){
#     # graphics::plot.new()
#     graphics::plot(g)
#   }
#
#   return(invisible(g))
# }
#
#
# # Example of Strogatz-Watts small-world network
# #
# # A wrapper around [igraph::sample_smallworld()] with `dim=1`
# #
# # @param n Size of the lattice (integer)
# # @param k Neighbourhood size (integer)
# # @param p Rewiring probability (between `0` and `1`)
# # @param doPlot PLot the igraph object
# #
# # @return A Strogatz-Watts small-world igraph object
# #
# # @export
# #
# # @family tools for plotting networks
# #
# # @seealso [igraph::sample_smallworld()]
# #
# plotNET_SW <- function(n=100,k=5,p=0.05, doPlot = TRUE){
#
#   g <- igraph::sample_smallworld(1, n, k, p)
#   g <- plotNET_prep(g,doPlot = FALSE)
#
#   # igraph::V(g)$degree <- igraph::degree(g)
#   #
#   # # set colors and sizes for vertices
#   # rev<-elascer(log1p(igraph::V(g)$degree))
#   # rev[rev<=0.2]<-0.2
#   # rev[rev>=0.9]<-0.9
#   # igraph::V(g)$rev <- rev$x
#   #
#   # igraph::V(g)$color       <- grDevices::rgb(igraph::V(g)$rev, 1-igraph::V(g)$rev,  0, 1)
#   # igraph::V(g)$size        <- 25*igraph::V(g)$rev
#   #
#   # # set vertex labels and their colors and sizes
#   # igraph::V(g)$label       <- ""
#   #
#   # igraph::E(g)$color <- grDevices::rgb(0.5, 0.5, 0.5, 1)
#
#   if(doPlot){
#     # graphics::plot.new()
#     graphics::plot(g)
#   }
#   return(invisible(g))
# }
#
# # Example of Barabasi scale-free network
# #
# # A wrapper around [igraph::sample_pa()]
# #
# # @param n Number of vertices
# # @param pwr Power of preferential attachment
# # @param out.dist Degree distribution
# # @param doPlot Plot the igraph object
# #
# # @return A Barabasi scale-free igraph object
# # @export
# #
# # @family tools for plotting networks
# #
# # @seealso [igraph::sample_pa()]
# #
# plotNET_BA <- function(n=100, pwr=1, out.dist=NULL, doPlot = TRUE){
#
#   g <- igraph::sample_pa(n, power = pwr, out.dist=out.dist, directed=FALSE)
#   g <- plotNET_prep(g,doPlot = FALSE)
#
#
#   # igraph::V(g)$degree <- igraph::degree(g)
#
#   # # set colors and sizes for vertices
#   # rev<-elascer(log1p(igraph::V(g)$degree))
#   # rev[rev<=0.2] <- 0.2
#   # rev[rev>=0.9] <- 0.9
#   # igraph::V(g)$rev <- rev$x
#   #
#   # igraph::V(g)$color    <- grDevices::rgb(igraph::V(g)$rev, 1-igraph::V(g)$rev,  0, 1)
#   # igraph::V(g)$size     <- 25*igraph::V(g)$rev
#   # # igraph::V(g)$frame.color <- grDevices::rgb(.5, .5,  0, .4)
#   #
#   # # set vertex labels and their colors and sizes
#   # igraph::V(g)$label <- ""
#   # igraph::E(g)$width <- 1
#   # igraph::E(g)$color <- grDevices::rgb(0.5, 0.5, 0.5, 1)
#
#   if(doPlot){
#     # graphics::plot.new()
#     graphics::plot(g)
#   }
#   return(invisible(g))
# }
#
#
# # Vertex and Edge Group Colours
# #
# # Identify Vertex and/or Edge groups by colour.
# #
# # @param g An igraph object
# # @param groups A named numeric vector with `length(V(g))` integers representing each group, or, a named character vector describing each group. If `names(groups)==NULL` then the names of the vector will be set as `names(groups) == V(g)$name`. If `V(g)$name==NULL`, the names of the vector will be set by the Vertex index
# # @param colourV Colour Vertices based on `groups` (default = `TRUE`)
# # @param alphaV Set transparency for Vertices (default = `1`)
# # @param colourE Colour Edges based on `groups`. Edges connecting to vertices of the same group will be coloured as the group (default = `FALSE`)
# # @param alphaE Set transparency for Edges. A single numeric, or a vector of length `ecount(g)` (default = `0.8`)
# # @param groupColours A list of length `groups` with valid colour codes
# # @param defaultEdgeColour Default edge colour
# # @param doPlot Plot the igraph object
# #
# # @return An igraph object with vertices and/or edges coloured by groups listed in `groups`
# #
# # @export
# #
# # @family tools for plotting networks
# #
# plotNET_groupColour <- function(g, groups, colourV=TRUE, alphaV=1, colourE=FALSE, alphaE=.8, groupColours=NULL, defaultEdgeColour = "grey70", doPlot = TRUE){
#
#   unicolours <- unique(groupColours)
#   unigroups  <- unique(groups)
#
#   if(length(groups)==igraph::gorder(g)){
#     if(is.null(names(groups))){
#       if(is.character(groups)){
#         names(groups) <- groups
#       } else {
#         names(groups) <- paste0(1:igraph::gorder(g))
#       }
#     }
#   } else {
#     if(length(unique(groups))>length(unique(groupColours))){
#       stop("length(groups) must be equal to number of Vertices: gorder(g)")
#     }
#   }
#
#   if(is.null(groupColours)){
#     if(length(unigroups)<=12){
#       groupColours <-  scales::brewer_pal(palette="Paired")(length(unigroups))
#     } else {
#       groupColours <- scales::gradient_n_pal(scales::brewer_pal(palette="Paired")(12))(seq(0, 1, length.out = length(unigroups)))
#     }
#     unicolours <- groupColours
#   } else {
#     if(length(groups)==length(groupColours)){
#       if(length(unique(groupColours))<=length(unigroups)){
#         unicolours <- unique(groupColours)
#       } else {
#         stop("Number of groups does not match number of colours!")
#       }
#     } else {
#       if(length(unique(groups))>length(unique(groupColours))){
#         stop("Length of groups vector does not match length of groupColour vector!")
#       }
#     }
#   }
#
#   # Add alpha to edges
#   if(all(alphaE%[]%c(0,1))){
#     if(length(alphaE)==1|length(alphaE)==igraph::ecount(g)){
#       igraph::E(g)$alpha <- alphaE
#     } else {
#       stop("Length of vector alphaE is not equal to 1 or ecount(g)")
#     }
#   } else {
#     stop("All alphaE must be in [0,1]")
#   }
#
#   if(all(alphaV%[]%c(0,1))){
#     if(length(alphaV)==1|length(alphaV)==igraph::vcount(g)){
#       igraph::V(g)$alpha <- alphaV
#     } else {
#       stop("Length of vector alphaV is not equal to 1 or vcount(g)")
#     }
#   } else {
#     stop("All alphaV must be in [0,1]")
#   }
#
#   if(colourE){
#     #init
#     igraph::E(g)$color  <- defaultEdgeColour
#   }
#
#   if(length(igraph::E(g)$color)>0){
#     igraph::E(g)$colour <- igraph::E(g)$color
#   }
#
#
#   for(c in seq_along(unigroups)){
#     Vid <- groups==unigroups[c]
#     if(sum(Vid)>0){
#
#       igraph::V(g)[Vid]$group      <- unigroups[c]
#       igraph::V(g)[Vid]$groupnum   <- c
#
#       if(colourV){
#         igraph::V(g)[Vid]$color  <- add_alpha(unicolours[c], alpha = igraph::V(g)[Vid]$alpha)
#         igraph::V(g)[Vid]$colour <- igraph::V(g)[Vid]$color
#       }
#
#       # if(alphaV){
#       #   igraph::V(g)[Vid]$color <- add_alpha(igraph::V(g)[Vid]$color, alpha = igraph::V(g)[Vid]$alpha)
#       #   igraph::V(g)[Vid]$colour <- igraph::V(g)[Vid]$color
#       # }
#
#       # Get ids for the edges that connect this group
#       Eid <- which(igraph::E(g)%in%igraph::E(g)[igraph::V(g)[Vid]%--%igraph::V(g)[Vid]])
#
#
#       if(length(Eid)>0){
#
#         igraph::E(g)[Eid]$group             <- unigroups[c]
#         igraph::E(g)[Eid]$groupnum          <- c
#
#         if(colourE){
#           igraph::E(g)[Eid]$color   <- add_alpha(unicolours[c], alpha = igraph::E(g)[Eid]$alpha)
#           igraph::E(g)[Eid]$colour  <- igraph::E(g)[Eid]$color
#         }
#         else {
#           #  # Add a default colour and alphac
#           igraph::E(g)[Eid]$color  <- add_alpha(igraph::E(g)[Eid]$color, alpha = igraph::E(g)[Eid]$alpha)
#           #  igraph::E(g)[Eid]$colour <- igraph::E(g)[Eid]$color
#         }
#         # if(alphaE){
#         #   igraph::E(g)[Eid]$color <- add_alpha(groupColours[c], alpha = igraph::E(g)[Eid]$alpha)
#         # }
#
#       } # edge IDs > 0
#     } # group IDs > 0
#   } # group loop
#
#   if(doPlot){
#     # graphics::plot.new()
#     graphics::plot(g)
#   }
#   return(invisible(g))
# }
#
#
# plotFA_loglog <- function(fd.OUT,title="log-log regresion",subtitle="",xlabel="Bin size",ylabel="Fluctuation",logBase=c("e","2","10")[1]){
#
#   if(logBase=="e"){
#     logFormat <- "log"
#     logBaseNum <- exp(1)
#   } else {
#     logFormat <- paste0("log",logBase)
#     logBaseNum <- as.numeric(logBase)
#   }
#
#   logSlopes <- data.frame(x = c(fd.OUT$PLAW$size[1:NROW(fd.OUT[[2]]$fitlm1$fitted.values)],
#                                 fd.OUT$PLAW$size[1:NROW(fd.OUT[[3]]$fitlm2$fitted.values)]),
#                           y = c(fd.OUT$PLAW$bulk[1:NROW(fd.OUT[[2]]$fitlm1$fitted.values)],
#                                 fd.OUT$PLAW$bulk[1:NROW(fd.OUT[[3]]$fitlm2$fitted.values)]),
#                           Method = c(rep(fd.OUT[[2]]$method,NROW(fd.OUT[[2]]$fitlm1$fitted.values)),
#                                      rep(fd.OUT[[3]]$method,NROW(fd.OUT[[3]]$fitlm2$fitted.values))))
#
#   g <- ggplot2::ggplot(data.frame(fd.OUT$PLAW), ggplot2::aes_(x=~size,y=~bulk), na.rm=TRUE) +
#     ggplot2::geom_point() +
#     ggplot2::geom_smooth(data = logSlopes,  ggplot2::aes_(x=~x,y=~y, colour = ~Method, fill = ~Method), method="lm", alpha = .2) +
#     ggplot2::ggtitle(label = title, subtitle = subtitle)
#
#   if(logBase=="e"){
#     g <- g +
#       ggplot2::scale_x_continuous(name = paste0(xlabel," (",logFormat,")"),
#                                   breaks = scales::trans_breaks(logFormat, function(x) logBaseNum^x),
#                                   labels = scales::trans_format(logFormat,scales::math_format(expr = plain(e)^.x)),
#                                   trans = scales::log_trans(base = logBaseNum)) +
#       ggplot2::scale_y_continuous(name = paste0(ylabel," (",logFormat,")"),
#                                   breaks = scales::trans_breaks(logFormat, function(x) logBaseNum^x),
#                                   labels = scales::trans_format(logFormat,scales::math_format(expr = plain(e)^.x)),
#                                   trans = scales::log_trans(base = logBaseNum))
#   }
#
#   if(logBase=="2"){
#     g <- g +
#       ggplot2::scale_x_continuous(name = paste0(xlabel," (",logFormat,")"),
#                                   breaks = scales::trans_breaks(logFormat, function(x) logBaseNum^x),
#                                   labels = scales::trans_format(logFormat, scales::math_format(expr = 2^.x)),
#                                   trans = scales::log_trans(base = logBaseNum)) +
#       ggplot2::scale_y_continuous(name = paste0(ylabel," (",logFormat,")"),
#                                   breaks = scales::trans_breaks(logFormat, function(x) logBaseNum^x),
#                                   labels = scales::trans_format(logFormat,scales::math_format(expr = 2^.x)),
#                                   trans = scales::log_trans(base = logBaseNum))
#   }
#
#   if(logBase=="10"){
#     g <- g +
#       ggplot2::scale_x_continuous(name = paste0(xlabel," (",logFormat,")"),
#                                   breaks = scales::trans_breaks(logFormat, function(x) logBaseNum^x),
#                                   labels = scales::trans_format(logFormat, scales::math_format(expr = 10^.x)),
#                                   trans = scales::log_trans(base = logBaseNum)) +
#       ggplot2::scale_y_continuous(name = paste0(ylabel," (",logFormat,")"),
#                                   breaks = scales::trans_breaks(logFormat, function(x) logBaseNum^x),
#                                   labels = scales::trans_format(logFormat,scales::math_format(expr = 10^.x)),
#                                   trans = scales::log_trans(base = logBaseNum))
#   }
#
#   g <- g +
#     ggplot2::annotation_logticks() +
#     ggplot2::theme_bw() +
#     ggplot2::theme(panel.grid.minor =  ggplot2::element_blank())
#
#   return(g)
# }
#
#
#
# # Plot output from fluctuation analyses based on log-log regression
# #
# # @param fd.OUT Output from one of the `fd_` functions that use log-log regression to get scaling exponents.
# # @param title Plot title
# # @param subtitle Plot subtitle
# # @param xlabel x label
# # @param ylabel y label
# # @param logBase base of the log used
# #
# # @return A ggplot object
# #
# # @export
# #
# plotFD_loglog <- function(fd.OUT, title="", subtitle="", xlabel="Bin size", ylabel="Fluctuation", logBase=NA){
#
#   if(!all(c("PLAW","fullRange","fitRange")%in%names(fd.OUT))){
#     stop("Object fd.OUT should have 3 fields: PLAW, fullRange and fitRange")
#   }
#
#   if(nchar(title)==0){
#     title <- fd.OUT$analysis$name
#   }
#   if(is.na(logBase)){
#     logBase <- fd.OUT$analysis$logBasePlot
#   }
#
#   if(logBase%in%"e"){
#     logFormat <- "log"
#     logBaseNum <- exp(1)
#     yAcc <- .1
#     xAcc <- 1
#   }
#
#   if(logBase%in%"2"){
#     logFormat <- paste0("log",logBase)
#     logBaseNum <- as.numeric(logBase)
#     yAcc <- .1
#     xAcc <- 1
#   }
#
#   if(logBase%in%"10"){
#     logFormat <- paste0("log",logBase)
#     logBaseNum <- as.numeric(logBase)
#     yAcc <- .1
#     xAcc <- .01
#   }
#
#
#   logLabels <- function (expr, format = force)
#   {
#     quoted <- substitute(expr)
#     subs <- function(x) {
#       do.call("substitute", list(quoted, list(.x = as.name(x))))
#     }
#     function(x) {
#       x <- format(x)
#       lapply(x, subs)
#     }
#   }
#
#   if(fd.OUT$analysis$name%in%"2D boxcount of 1D curve"){
#
#     logSlopes <- data.frame(x = c(fd.OUT$PLAW$size[1:NROW(fd.OUT[[2]]$fitlm1$fitted.values)],
#                                   fd.OUT$ullRange$fitRange,fd.OUT$fitRange$fitRange),
#                             y = c(fd.OUT$PLAW$bulk[1:NROW(fd.OUT[[2]]$fitlm1$fitted.values)],
#                                   fd.OUT$PLAW$bulk[1:NROW(fd.OUT[[3]]$fitlm2$fitted.values)]),
#                             Method = c(rep(fd.OUT[[2]]$method,NROW(fd.OUT[[2]]$fitlm1$fitted.values)),
#                                        rep(fd.OUT[[3]]$method,NROW(fd.OUT[[3]]$fitlm2$fitted.values))))
#   } else {
#
#     logSlopes <- data.frame(x = c(fd.OUT$PLAW$size[1:NROW(fd.OUT[[2]]$fitlm1$fitted.values)],
#                                   fd.OUT$PLAW$size[1:NROW(fd.OUT[[3]]$fitlm2$fitted.values)]),
#                             y = c(fd.OUT$PLAW$bulk[1:NROW(fd.OUT[[2]]$fitlm1$fitted.values)],
#                                   fd.OUT$PLAW$bulk[1:NROW(fd.OUT[[3]]$fitlm2$fitted.values)]),
#                             Method = c(rep(fd.OUT[[2]]$method,NROW(fd.OUT[[2]]$fitlm1$fitted.values)),
#                                        rep(fd.OUT[[3]]$method,NROW(fd.OUT[[3]]$fitlm2$fitted.values))))
#   }
#
#   g <- ggplot2::ggplot(data.frame(fd.OUT$PLAW), ggplot2::aes_(x=~size,y=~bulk), na.rm=TRUE) +
#     ggplot2::geom_point() +
#     ggplot2::ggtitle(label = title, subtitle = subtitle)
#
#   # if(logBase=="e"){
#   # evalT<-
#   #   paste0('g <- g + ggplot2::geom_smooth(data = logSlopes,  ggplot2::aes_(x=~x,y=~y, colour = ~Method, fill = ~Method), method="lm", alpha = .2) + ggplot2::scale_x_continuous(name = "',paste0(xlabel," (",logFormat,")"),'", breaks = scales::trans_breaks(',logFormat,', function(x) ',logBaseNum,'^x), labels =  scales::trans_format(',logFormat,',scales::math_format(e^.x)), trans = scales::log_trans(base = ',logBaseNum,')) +
#   #     ggplot2::scale_y_continuous(name = "',paste0(ylabel," (",logFormat,")"),'", breaks = scales::trans_breaks(',logFormat,', function(x) ',logBaseNum,'^x), labels =  scales::trans_format(',logFormat,',scales::math_format(e^.x)), trans = scales::log_trans(base = ',logBaseNum,')) + ggplot2::annotation_logticks()')
#   #
#   # eval(parse(text = evalT))
#   # }
#   #
#   # if(logBase=="2"){
#   #   evalT<-
#   #     paste0('g <- g + ggplot2::geom_smooth(data = logSlopes,  ggplot2::aes_(x=~x,y=~y, colour = ~Method, fill = ~Method), method="lm", alpha = .2) + ggplot2::scale_x_continuous(name = "',paste0(xlabel," (",logFormat,")"),'", breaks = scales::trans_breaks(',logFormat,', function(x) ',logBaseNum,'^x), labels =  scales::trans_format(',logFormat,',scales::math_format(2^.x)), trans = scales::log_trans(base = ',logBaseNum,')) +
#   #            ggplot2::scale_y_continuous(name = "',paste0(ylabel," (",logFormat,")"),'", breaks = scales::trans_breaks(',logFormat,', function(x) ',logBaseNum,'^x), labels =  scales::trans_format(',logFormat,',scales::math_format(2^.x)), trans = scales::log_trans(base = ',logBaseNum,')) + ggplot2::annotation_logticks()')
#   #
#   #   eval(parse(text = evalT))
#   # }
#   #
#
#   breaksX <- unique(fd.OUT$PLAW$size[1:NROW(fd.OUT[[2]]$fitlm1$fitted.values)])
#   breaksY <- unique(fd.OUT$PLAW$bulk[1:NROW(fd.OUT[[2]]$fitlm1$fitted.values)])
#
#   if(length(breaksX)>10){breaksX <- breaksX[seq.int(1,length(breaksX),length.out = 10)]}
#   if(length(breaksY)>10){breaksY <- breaksY[seq.int(1,length(breaksY),length.out = 10)]}
#
#   if(logBase!="no"){
#     g <- g +
#       ggplot2::geom_smooth(data = logSlopes,  ggplot2::aes_(x=~x,y=~y, colour = ~Method, fill = ~Method), method="lm", alpha = .2) +
#       ggplot2::scale_x_continuous(name = paste0(xlabel," (",logFormat,")"),
#                                   breaks = breaksX,
#                                   labels = scales::number_format(accuracy = xAcc),
#                                   trans = scales::log_trans(base = logBaseNum)) +
#       ggplot2::scale_y_continuous(name = paste0(ylabel," (",logFormat,")"),
#                                   breaks = breaksY,
#                                   labels = scales::number_format(accuracy = yAcc),
#                                   trans = scales::log_trans(base = logBaseNum))
#   } else {
#
#     # if(logBase=="no"){
#
#     g <- g +
#       ggplot2::geom_vline(data = data.frame(x = c(NROW(fd.OUT[[2]]$fitlm1$fitted.values),NROW(fd.OUT[[3]]$fitlm2$fitted.values)),Method = c(fd.OUT[[2]]$method,fd.OUT[[3]]$method)),  ggplot2::aes_(xintercept=~x, colour = ~Method)) +
#       ggplot2::scale_x_continuous(name = xlabel, breaks = breaksX) +
#       ggplot2::scale_y_continuous(name = ylabel, breaks = breaksY)
#   }
#
#   g <- g +
#     ggplot2::scale_color_manual(values = c("red3","steelblue")) +
#     ggplot2::scale_fill_manual(values = c("red3","steelblue")) +
#     ggplot2::theme_bw() +
#     ggplot2::theme(panel.grid.minor =  ggplot2::element_blank(),
#                    legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 5,b = 5, unit = "pt"), vjust = .5),
#                    plot.margin = ggplot2::margin(t = 5,b = 5, r = 5,l = 5, unit = "pt"))
#
#
#   # graphics::plot.new()
#   graphics::plot(g)
#
#   return(invisible(g))
# }
#
#
# # Surrogate Test
# #
# # @param surrogateValues Vector of measures based on surrogate time series
# # @param observedValue The measure obtained from the observed value
# # @param sides Is this a 1 or 2-sided test (default = \code{1})
# # @param binWidth The size of the histogram bins. The default is to look for the max. number of digits and set the width to \code{1/10^(Ndigits-1)}. If integers are detectec width will be set to 1.
# # @param measureName Label for x-axis
# # @param title A title for the plot
# # @param doPlot Plot a histogram of the distribution (default = \code{TRUE})
# # @param returnOnlyPvalue Do not return the graph, just the point p-value (default = \code{FALSE})
# #
# # alpha Significance threshold for the test. This value is currently calculated from the data as \eqn{\frac{1}{rank}*Nsides}, setting it will not have an effect.
# #
# # @return A point p-value for the observed value, and/or a histogram of the distribution (\code{ggplot2} object).
# # @export
# #
# plotSUR_hist <- function(surrogateValues,
#                          observedValue,
#                          sides = c("two.sided","greater","less")[1],
#                          binWidth = NULL,
#                          measureName = "",
#                          title="",
#                          doPlot = TRUE,
#                          returnOnlyPvalue = FALSE){
#
#   if(any(sides%in%c("two.sided","greater","less"))){
#     nsides <- dplyr::case_when(sides == "two.sided" ~ 2,
#                                sides %in% c("greater","less") ~ 1)
#   } else {
#     stop("Use one of: 'two.sided', 'greater' or 'less' for argument 'sides'")
#   }
#
#   vec            <- sort(c(surrogateValues, as.vector(observedValue)))
#
#   if(is.null(binWidth)){
#     if(all(is.wholenumber(vec))){
#       binWidth<-1
#     } else {
#       binWidth<-1/10^(max(nchar(gsub("(\\d[.])+","",signif(vec))),na.rm = TRUE))
#       if(binWidth<mean(diff(vec))){
#         binWidth<-round(mean(diff(vec)),3)
#       }
#     }
#   }
#
#   alpha <- nsides/length(vec)
#
#   rank_dist_bin_lab  <- ggplot2::cut_width(vec, width=binWidth, center = 0, closed = "left")
#   if(length(levels(rank_dist_bin_lab))>10*length(vec)){warning("Too many levels? Change binWidth!")}
#   rank_dist_bin      <- ggplot2::cut_interval(vec, n=length(levels(rank_dist_bin_lab)), labels=FALSE)
#   #ggplot2::cut_interval(rank_dist_bin,n = length(unique(rank_dist_bin)),labels=F)
#   low <- seq(vec[1],vec[length(vec)],length.out = length(vec))[floor(length(vec)/2)]
#   hi  <- seq(vec[1],vec[length(vec)],length.out = length(vec))[ceiling(length(vec)/2)]
#   rank_dist_mid     <- mean(low,hi)
#   rank_dist_bin_mid <- rank_dist_bin[which(vec>=rank_dist_mid)[1]]
#   rank_dist         <- rank(vec,ties.method = "max")
#   rank_obs         <- unique(rank(vec,ties.method = "max")[which(vec==observedValue)])
#
#   rank_obs_p <- NULL
#
#   if(sides%in%"less"){
#     rank_obs_p     <- (1/((length(vec)+1)-rank_obs)*nsides)%00%NA #*table(rank(rev(vec),ties.method = "max"))[rank_obs]
#     #rank_obs_p     <- table(rank(rev(vec),ties.method = "max"))[rank_obs]
#     #rank_obs       <- unique(rank(vec,ties.method = "min")[which(vec==observedValue)])
#     dist_lines     <- c(min(rank_dist_bin),rank_dist_bin_mid)
#   }
#
#   if(sides%in%"two.sided"){
#     if(observedValue<rank_dist_mid){
#       rank_obs_p     <- (1/((length(vec)+1)-rank_obs)*nsides)%00%NA
#     } else {
#       rank_obs_p     <- ((1/rank_obs)*nsides)%00%NA
#     }
#     dist_lines    <- c(min(rank_dist_bin),rank_dist_bin_mid, max(rank_dist_bin))
#   }
#
#   if(sides%in%"greater"){
#     rank_obs_p     <- ((1/rank_obs)*nsides)%00%NA #*table(rank(rev(vec),ties.method = "max"))[rank_obs]
#     dist_lines     <- c(rank_dist_bin_mid, max(rank_dist_bin))
#   }
#
#   if(!is.null(rank_obs)){
#
#     rank_dist_prob <- 1/rank_dist
#     df.dist <- data.frame(value      = vec,
#                           rank_dist       = rank_dist,
#                           rank_dist_prob  = rank_dist_prob,
#                           rank_dist_bin   = rank_dist_bin,
#                           rank_dist_bin_label = rank_dist_bin_lab
#     )
#
#     ornd   <- round(observedValue,3)
#     prnd   <- round(rank_obs_p,3)
#     pLabel <- list(bquote(plain(P)(X==.(ornd)) ==.(prnd) ~ ~ alpha == .(alpha)))
#     obs    <- cbind.data.frame(obs = "obs",
#                                x = unique(df.dist$rank_dist_bin[rank_obs]),
#                                y = 0)
#
#     breaks <- seq(1,max(df.dist$rank_dist_bin),by=5)
#     labels <- levels(df.dist$rank_dist_bin_label)[breaks]
#     # if(length(breaks)>=length(vec)/2){
#     #   breaks <- breaks[round(seq(1,max(breaks),length.out=max(breaks)/2))]
#     #   labels <- paste(signif(unique(vec)[breaks],3))
#     #   if(!all(diff(diff(breaks))!=0)){
#     #     breaks <- seq(1,max(df.dist$rank_dist_bin),by=2)
#     #     if(max(df.dist$rank_dist_bin)!=max(breaks)){
#     #     breaks <- c(breaks, max(breaks)+min(diff(breaks)))
#     #     labels <- c(paste(signif(na.exclude(unique(vec)[breaks],3))),paste(signif(max(na.exclude(unique(vec)[breaks],3))+min(diff(na.exclude(unique(vec)[breaks]))),3)))
#     #     }
#     #   }
#     # }
#
#
#     g <- ggplot2:: ggplot(df.dist,ggplot2::aes_(x=~rank_dist_bin)) +
#       geom_histogram(breaks= seq(1,(max(df.dist$rank_dist_bin)+1))-.5,colour="white") +
#       geom_vline(xintercept = dist_lines, colour="steelblue") +
#       geom_point(data=obs, ggplot2::aes_(x=~x, y=~y,colour=~obs), size=5) +
#       scale_x_continuous(name = measureName, breaks = breaks, labels = labels) +
#       scale_color_manual("Observed",values="red", breaks="obs", labels = pLabel) +
#       ggtitle(label = title, subtitle = paste0(nsides,"-sided test with ",length(surrogateValues)," surrogate values. The observed value has (max) rank ",rank_obs,".")) +
#       theme_bw() +
#       theme(legend.position = c(1,.90),
#             legend.justification = "right",
#             legend.background = element_rect(colour = "black"),
#             panel.grid.minor.x = element_blank(),
#             panel.grid.major.y = element_blank(),
#             panel.grid.minor.y = element_blank(),
#             axis.text.x = element_text(angle=90, vjust = 0.5))
#
#     if(doPlot){
#       grid::grid.newpage()
#       grid::grid.draw(g)
#     }
#
#     attr(rank_obs_p,"sides")         <- sides
#     attr(rank_obs_p,"rank_observed") <- rank_obs
#     attr(rank_obs_p,"N_values")      <- length(vec)
#     attr(rank_obs_p,"alpha")         <- alpha
#
#     if(returnOnlyPvalue){
#       return(rank_obs_p)
#     }
#     return(invisible(list(pvalue=rank_obs_p,surrogates_plot=g,plot_data=df.dist)))
#   } else {
#     stop("Could not determine a rank order probability. Did you provide a correct argument for sides?")
#   }
# }
#
#
#
# # Add transparency to a colour
# #
# # @param col A colour name, hexadecimal string or positive integer \code{i}, such that palette()[i]
# # @param alpha Alpha transparency value
# #
# # @return An rgb colour with transparency
# # @export
# #
# add_alpha <- function(col, alpha=1){
#   if(missing(col)){stop("Please provide a vector of colours.")}
#   apply(sapply(col, grDevices::col2rgb)/255, 2, function(x){grDevices::rgb(x[1], x[2], x[3], alpha=alpha)})
# }
#
# # FisherZ for PACF
# #
# # @param r r
# # @param n n
# # @param lag lag
# # @param siglevel siglevel
# # @param sides sides
# # @param alt alt
# # @param type type
# #
# # @return dataframe
# # @export
# #
# #
# pacf_fisherZ <-function(r, n, lag, siglevel=.05, sides=2, alt = "both", type="",label=""){
#
#   z <- atanh(r)/sqrt(1/(n-3))
#
#   if(alt=="both"){
#     conf.low   <- tanh(atanh(r) - (qnorm(1-(siglevel/sides)) * sqrt((1/(n-3)))))
#     conf.high  <- tanh(atanh(r) + (qnorm(1-(siglevel/sides)) * sqrt((1/(n-3)))))
#     p <- (1-pnorm(abs(z)))
#   }
#   if(alt=="greater"){
#     conf.low   <- tanh(atanh(r) - (qnorm(1-(siglevel/sides)) * sqrt((1/(n-3)))))
#     conf.high  <- tanh(atanh(r)) #+ (qnorm(1-(siglevel/sides)) * sqrt((1/(n-3)))))
#     p <- (1-pnorm(z))
#   }
#   if(alt=="less"){
#     conf.low   <- tanh(atanh(r)) #- (qnorm(1-(siglevel/sides)) * sqrt((1/(n-3)))))
#     conf.high  <- tanh(atanh(r) + (qnorm(1-(siglevel/sides)) * sqrt((1/(n-3)))))
#     p <- pnorm(z)
#   }
#
#   #print(rbind(lag,p,r,n))
#
#   if(p<(siglevel/sides)){
#     sig <-"yes"
#   } else {
#     sig <-"no"
#   }
#
#   return(data.frame(r = r,
#                     ciL = conf.low,
#                     ciU = conf.high,
#                     fZ = z,
#                     p = p,
#                     alpha = siglevel,
#                     sig = sig,
#                     lag = lag,
#                     n = n,
#                     alt = alt,
#                     type = type,
#                     label = label,
#                     stringsAsFactors = FALSE))
# }
#
# # Estimate the transition matrix for computing the critical threshold values on
# # mutual information.
# #
# # \code{mutual_info} is a helper function for estimating the transition matrix
# # used in creating resampled vectors for the (1 - alpha)\% critical threshold
# # value on the mutual info.
# #
# # @param x A \code{vector} of values.
# # @param n_bins The number of bins for the entropy calculation.
# #
# # @return A \code{list} with the following components:
# # \describe{
# # \item{\code{xn}}{An [n x 2] matrix of the original and discretized vectors.}
# # \item{\code{MM}}{Transition probability matrix from bin-i to bin-j.}
# # }
# #
# # @author Mark Scheuerell (https://mdscheuerell.github.io/muti/)
# #
# # @importFrom stats runif
# # @export
# # @keywords internal
# #
# transM <- function(x,n_bins) {
#   ## helper function for estimating transition matrix used in
#   ## creating resampled ts for the CI on mutual info
#   ## replace NA with runif()
#   x[is.na(x)] <- runif(length(x[is.na(x)]),min(x,na.rm=TRUE),max(x,na.rm=TRUE))
#   ## length of ts
#   tt <- length(x)
#   ## get bins via slightly extended range
#   bins <- seq(min(x)-0.001,max(x),length.out=n_bins+1)
#   ## discretize ts
#   hin <- vector("numeric",tt)
#   for(b in 1:n_bins) {
#     hin[x > bins[b] & x <= bins[b+1]] <- b
#   }
#   ## matrix of raw-x & discrete-x
#   xn <- cbind(x,hin)
#   ## transition matrix from bin-i to bin-j
#   MM <- matrix(0,n_bins,n_bins)
#   for(i in 1:n_bins) {
#     for(j in 1:n_bins) {
#       MM[i,j] <- sum(hin[-tt]==i & hin[-1]==j)
#     }
#     if(sum(MM[i,])>0) { MM[i,] <- MM[i,]/sum(MM[i,]) }
#     else { MM[i,] <- 1/n_bins }
#   }
#   return(list(xn=xn,MM=MM))
# } ## end function
#
# # Create new vector based on resampling of the original data.
# #
# # \code{newZ} creates new vector based on a transition matrix. It is a helper
# #   function for \code{muti}.
# #
# # @param tM The output from \code{transM}; a \code{list} with elements \code{xn} and \code{MM}.
# # @param n_bins The number of bins to use; passed from \code{muti}.
# #
# # @return A vector of resampled values.
# #
# # @author Mark Scheuerell (https://mdscheuerell.github.io/muti/)
# #
# # @importFrom stats rmultinom
# # @export
# # @keywords internal
# #
# newZ <- function(tM, n_bins) {
#   ## helper function for creating new ts based on resampling
#   ## the original data
#   ## number of bins
#   # n_bins <- dim(tM$MM)[1]
#   ## length of ts
#   tt <- dim(tM$xn)[1]
#   ## random start index
#   tin <- sample(tt,1)
#   ## init zz
#   zz <- matrix(NA,tt,2)
#   ## get first sample
#   zz[1,] <- tM$xn[tin,]
#   ## loop over remaining samples
#   for(t in 2:tt) {
#     ## random transition bin
#     zz[t,2] <- seq(n_bins)[rmultinom(1,1,tM$MM[zz[t-1,2],])==1]
#     ## possible set of real values
#     pset <- tM$xn[tM$xn[,2] %in% zz[t,2],1]
#     if(length(pset)==0) { pset <- NA }
#     ## get next sample
#     zz[t,1] <- pset[sample(length(pset),1)]
#   }
#   return(zz[,1])
# } ## end function
#
# # Plot ACF and PACF
# #
# # @param y A time series or numeric vector
# # @param Lmax Maximum number of lags
# # @param alpha Significance level
# # @param doPlot Plot output
# # @param returnCorFun Return the data
# #
# # @return Either an invisible ggplot2 object r a list containing the plot and the data
# #
# # @family Plot redundancy functions
# #
# # @export
# #
# plotRED_acf <- function(y, Lmax = max(round(NROW(y)/4),10),alpha=.05 ,doPlot = TRUE, returnCorFun = TRUE){
#
#   siglevel <- alpha
#   df.acf <- stats::acf(y,plot=FALSE, lag.max = Lmax)
#   df.pacf <- stats::pacf(y,plot=FALSE, lag.max = Lmax)
#
#   dfN <- c(NROW(y), plyr::laply(1:Lmax, function(l) NROW(ts_embed(y,2,l))+1))
#
#   corfunACF  <- plyr::ldply(seq_along(df.acf$acf), function(cc){pacf_fisherZ(r=df.acf$acf[cc],n=dfN[cc],lag=df.acf$lag[cc],type="acf")})
#   corfunPACF <- plyr::ldply(seq_along(df.pacf$acf), function(cc){pacf_fisherZ(r=df.pacf$acf[cc],n=dfN[cc],lag=df.pacf$lag[cc],type="pacf")})
#   corfun     <- rbind(corfunACF,corfunPACF)
#
#   groupColours <-  scales::brewer_pal(palette="RdBu")(11)
#   cols <- c("yes"=groupColours[9],"no"=groupColours[3])
#
#   g <- ggplot(corfun,ggplot2::aes_(x=~lag,y=~r)) +
#     geom_hline(yintercept = 0, colour="grey",size=1) +
#     geom_line(data = data.frame(x=c(0,corfun$lag[1]),y=c(1,corfun$r[1])),aes_(x=~x,y=~y),colour="grey50") +
#     geom_point(x=0,y=1,colour=groupColours[10],fill=groupColours[9],size=2,pch=21) +
#     geom_ribbon(ggplot2::aes_(ymin=~ciL,ymax=~ciU),fill="grey70",colour="grey50") +
#     geom_path(colour="grey50") +
#     geom_point(ggplot2::aes_(fill = ~sig, colour=~sig),pch=21, cex=(1 + .01*(NROW(y)/Lmax))) +
#     facet_grid(type ~.) +
#     scale_fill_manual(bquote(p < .(siglevel)),values = cols,
#                       labels =  list("yes"= expression(rho != 0),
#                                      "no" = expression(rho == 0))) +
#     scale_colour_manual(bquote(p < .(siglevel)),values = cols,
#                         labels =  list("yes"= expression(rho != 0),
#                                        "no" = expression(rho == 0))) +
#     scale_x_continuous(limits = c(0,Lmax),expand = c(0.01,0), breaks = seq(0,Lmax,by = round(Lmax/10))) +
#     scale_y_continuous(limits = c(-1,1)) +
#     theme_bw() + theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank())
#
#   if(doPlot){
#     graphics::plot.new()
#     graphics::plot(g)
#   }
#
#   if(returnCorFun){
#     return(list(corfun=corfun,
#                 plot=invisible(g)))
#   } else {
#     return(invisible(g))
#   }
# }
#
# # Plot various MI functions
# #
# # @param y A \code{Nx1} matrix for auto-mif, a \code{Nx2} matrix or data frame for cross-mif, a \code{Nx3} matrix or data frame for mif between col 1 and 2 conditional on col 3; or a \code{NxM} matrix or data frame for the multi-information function.
# # @param lags Maximum number of lags
# # @param nbins The number of bins passed to \code{\link[infotheo]{discretize}} if y is a matrix or \code{\link[casnet]{ts_discrete}}
# # @param alpha Significance level
# # @param doPlot Plot output
# # @param returnMIFun Return the data
# #
# # @return Either an invisible ggplot2 object r a list containing the plot and the data
# #
# # @family Plot redundancy functions
# #
# # @export
# #
# plotRED_mif <- function(y, lags = 0:max(round(NROW(y)/4),10), nbins = ceiling(2*NROW(y)^(1/3)), alpha=.05 ,doPlot = TRUE, returnMIFun = TRUE){
#
#   siglevel <- alpha
#
#   mifunMIF  <- mif(y = y, lags = lags, nbins = nbins)
#   #ldply(seq_along(df.acf$acf), function(cc){pacf_fisherZ(r=df.acf$acf[cc],n=dfN[cc],lag=df.acf$lag[cc],type="acf")})
#   mifunPMIF <- mif(y = cbind(y,y[,1],y[,1]), lags = lags, nbins = nbins)
#   #ldply(seq_along(df.pacf$acf), function(cc){pacf_fisherZ(r=df.pacf$acf[cc],n=dfN[cc],lag=df.pacf$lag[cc],type="pacf")})
#   mifun     <- rbind(mifunMIF,mifunPMIF)
#
#   groupColours <-  scales::brewer_pal(palette="RdBu")(11)
#   cols <- c("yes"=groupColours[9],"no"=groupColours[3])
#
#   g <- ggplot(mifun,ggplot2::aes_(x=~lag,y=~r)) +
#     geom_hline(yintercept = 0, colour="grey",size=1) +
#     geom_line(data = data.frame(x=c(0,mifun$lag[1]),y=c(1,mifun$r[1])),ggplot2::aes_(x=~x,y=~y),colour="grey50") +
#     geom_point(x=0,y=1,colour=groupColours[10],fill=groupColours[9],size=2,pch=21) +
#     geom_ribbon(ggplot2::aes_(ymin=~ciL,ymax=~ciU),fill="grey70",colour="grey50") +
#     geom_path(colour="grey50") +
#     geom_point(ggplot2::aes_(fill = ~sig, colour=~sig),pch=21, cex=(1 + .01*(NROW(~y)/~nbins))) +
#     facet_grid(type ~.) +
#     scale_fill_manual(bquote(p < .(siglevel)),values = cols,
#                       labels =  list("yes"= expression(rho != 0),
#                                      "no" = expression(rho == 0))) +
#     scale_colour_manual(bquote(p < .(siglevel)),values = cols,
#                         labels =  list("yes"= expression(rho != 0),
#                                        "no" = expression(rho == 0))) +
#     scale_x_continuous(limits = c(0,nbins),expand = c(0.01,0), breaks = seq(0,nbins,by = round(nbins/10))) +
#     scale_y_continuous(limits = c(-1,1)) +
#     theme_bw() + theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank())
#
#   if(doPlot){
#     graphics::plot.new()
#     graphics::plot(g)
#   }
#
#   if(returnMIFun){
#     return(list(mifun=mifun,
#                 plot=invisible(g)))
#   } else {
#     return(invisible(g))
#   }
# }
#
#
# # Complex Networks# graph2svg <- function(TDM,pname){
# #
# #   # Create weighted Term-Term matrix
# #   tTM <- as.matrix(TDM)
# #   TTM <- tTM %*% t(tTM)
# #   TTM <- log1p(TTM)
# #
# #   g <- graph.adjacency(TTM,weighted=T,mode="undirected",diag=F)
# #   g <- simplify(g)
# #
# #   # Remove vertices that were used in the search query
# #   Vrem <- which(igraph::V(g)$name %in% c("~dev~","~dys~","~sld~","development","children","dyslexia"))
# #   g <- (g - igraph::V(g)$name[Vrem])
# #
# #   # Set colors and sizes for vertices
# #   igraph::V(g)$degree <- igraph::degree(g)
# #   rev         <- scaleRange(log1p(igraph::V(g)$degree))
# #   rev[rev<=0.3]<-0.3
# #
# #   igraph::V(g)$color       <- grDevices::rgb(scaleRange(igraph::V(g)$degree), 1-scaleRange(igraph::V(g)$degree),  0, rev)
# #   igraph::V(g)$size        <- 10*scaleRange(igraph::V(g)$degree)
# #   igraph::V(g)$frame.color <- NA
# #
# #   # set vertex labels and their colors and sizes
# #   igraph::V(g)$label       <- igraph::V(g)$name
# #   igraph::V(g)$label.color <- grDevices::rgb(0, 0, 0, rev)
# #   igraph::V(g)$label.cex   <- scaleRange(igraph::V(g)$degree)+.1
# #
# #   # set edge width and color
# #   rew <- scaleRange(igraph::E(g)$weight)
# #   rew[rew<=0.3]<-0.3
# #
# #   igraph::E(g)$width <- 2*scaleRange(igraph::E(g)$weight)
# #   igraph::E(g)$color <- grDevices::rgb(.5, .5, 0, rew)
# #   set.seed(958)
# #
# #   svg(paste(pname,sep=""),width=8,height=8)
# #   graphics::plot(g, layout=layout.fruchterman.reingold(g))
# #   dev.off()
# #
# #   return(g)
# # }
# #
# # # Plot vertex neighbourhood
# # hoodGraph2svg <- function(TDM,Vname,pname){
# #
# #   # Create weighted Term-Term matrix
# #   tTM <- as.matrix(TDM)
# #   TTM <- tTM %*% t(tTM)
# #   TTM <- log1p(TTM)
# #
# #   ig <- graph.adjacency(TTM,weighted=T,mode="undirected",diag=F)
# #   ig <- simplify(ig)
# #
# #   # Remove vertices that were used in the search query
# #   Vrem <- which(igraph::V(ig)$name %in% c("~dev~","~dys~","~sld~","development","children","dyslexia"))
# #   ig <- (ig - igraph::V(ig)$name[Vrem])
# #
# #   # This is a deletion specific for the Neighbourhood graphs
# #   Vrem <- which(igraph::V(ig)$name %in% c("~rdsp~","~imp~","~som~","~bod~","~mlt~"))
# #   ig   <- ig - igraph::V(ig)$name[Vrem]
# #
# #   idx <- which(igraph::V(ig)$name==Vname)
# #   sg  <- graph.neighborhood(ig, order = 1, nodes=igraph::V(ig)[idx], mode = 'all')[[1]]
# #
# #   # set colors and sizes for vertices
# #   igraph::V(sg)$igraph::degree <- igraph::degree(sg)
# #
# #   rev<-scaleRange(log1p(igraph::V(sg)$igraph::degree))
# #   rev[rev<=0.3]<-0.3
# #
# #   igraph::V(sg)$color <- grDevices::rgb(scaleRange(igraph::V(sg)$igraph::degree), 1-scaleRange(log1p(igraph::V(sg)$igraph::degree*igraph::V(sg)$igraph::degree)),  0, rev)
# #
# #   igraph::V(sg)$size        <- 35*scaleRange(igraph::V(sg)$igraph::degree)
# #   igraph::V(sg)$frame.color <- NA
# #
# #   # set vertex labels and their colors and sizes
# #   igraph::V(sg)$label       <- igraph::V(sg)$name
# #   igraph::V(sg)$label.color <- grDevices::rgb(0, 0, 0, rev)
# #   igraph::V(sg)$label.cex   <- scaleRange(igraph::V(sg)$igraph::degree)
# #
# #   # set edge width and color
# #   rew<-scaleRange(igraph::E(sg)$weight)
# #   rew[rew<=0.3]<-0.3
# #
# #   igraph::E(sg)$width <- 6*scaleRange(igraph::E(sg)$weight)
# #   igraph::E(sg)$color <- grDevices::rgb(.5, .5, 0, rew)
# #
# #   idV <- which(igraph::V(sg)$name==Vname)
# #   idE <- incident(sg,igraph::V(sg)[[idV]])
# #   igraph::E(sg)$color[idE] <- grDevices::rgb(0, 0, 1 ,0.8)
# #
# #   set.seed(958)
# #
# #   idx <- which(igraph::V(sg)$name==Vname)
# #   svg(paste(pname,sep=""),width=8,height=8)
# #   graphics::plot(sg,layout=layout.star(sg,center=igraph::V(sg)[idx]))
# #   dev.off()
# #
# #   return(sg)
# # }
#
#
# #
# # # PSDslope
# # #
# # # @param y    A time series object, or a vector that can be converted to a time series object.
# # # @param fs    Sample frequency (defults to 1).
# # # @param nfft    Number of frequencies to estimate (defaults to next power of 2)
# # # @param fitRange    Vector of length 2 with range of frequencies to perform log-log fit.
# # # @param doPlot    Plot the log-log spectrum and slope.
# # #
# # # @return
# # # @export
# # #
# # # @examples
# # #
# # PSDslope <- function(y  = stats::ts(rnorm(n = 1024), frequency = 1),
# #                      fs = stats::frequency(y),
# #                      nfft = 2^(nextpow2(length(y)/2)),
# #                      fitRange = c(1,round(.1*nfft)),
# #                      doPlot = FALSE){
# #   require(oce)
# #   require(signal)
# #   if(!stats::is.ts(y)){stats::ts(y, frequency = fs)}
# #
# #   win <- signal::hamming(n=nfft)
# #
# #   perioGram <- oce::pwelch(x = y, window = win, fs = stats::frequency(y), nfft = nfft, doPlot = FALSE)
# #   spec <- data.frame(Frequency = perioGram$freq, Power = perioGram$spec)
# #   spec[1,1:2] <- NA
# #   fit <- stats::lm(log10(spec$Power[fitRange[1]:fitRange[2]])~log10(spec$Power[fitRange[1]:fitRange[2]]))
# #   return(list(spec = spec,
# #               slope = fit)
# #   )
# # }
#
#
#
# # Toy models ----------
#
# # Examples of dynamical growth models (maps)
# #
# # Autocatlytic Growth: Iterating differential equations (maps)
# #
# # @param Y0    Initial value.
# # @param r    Growth rate parameter.
# # @param k    Carrying capacity.
# # @param N    Length of the time series.
# # @param type    One of: "driving" (default), "damping", "logistic", "vanGeert1991".
# #
# # @return A timeseries object of length N.
# # @export
# #
# # @author Fred Hasselman
# #
# # @family autocatalytic growth functions
# #
# # @examples
# # # The logistic map in the chaotic regime
# # growth_ac(Y0 = 0.01, r = 4, type = "logistic")
# growth_ac <- function(Y0 = 0.01, r = 1, k = 1, N = 100, type = c("driving", "damping", "logistic", "vanGeert")[1]){
#   # Create a vector Y of length N, which has value Y0 at Y[1]
#   if(N>1){
#     Y <- as.numeric(c(Y0, rep(NA,N-2)))
#     # Conditional on the value of type ...
#     switch(type,
#            # Iterate N steps of the difference function with values passed for Y0, k and r.
#            driving  = sapply(seq_along(Y), function(t) Y[[t+1]] <<- r * Y[t] ),
#            damping  = k + sapply(seq_along(Y), function(t) Y[[t+1]] <<- - r * Y[t]^2 / k),
#            logistic = sapply(seq_along(Y), function(t) Y[[t+1]] <<- r * Y[t] * ((k - Y[t]) / k)),
#            vanGeert = sapply(seq_along(Y), function(t) Y[[t+1]] <<- Y[t] * (1 + r - r * Y[t] / k))
#     )}
#   return(stats::ts(Y))
# }
#
# # Examples of conditional dynamical growth models (maps)
# #
# # Conditional Autocatlytic Growth: Iterating differential equations (maps)
# #
# # @param Y0 Initial value
# # @param r Growth rate parameter
# # @param k Carrying capacity
# # @param cond Conditional rules passed as a data.frame of the form: cbind.data.frame(Y = ..., par = ..., val = ...)
# # @param N Length of the time series
# #
# # @export
# #
# # @author Fred Hasselman
# #
# # @family autocatalytic growth functions
# #
# # @examples
# # # Plot with the default settings
# # library(lattice)
# # xyplot(growth_ac_cond())
# #
# # # The function can take a set of conditional rules
# # # and apply them sequentially during the iterations.
# # # The conditional rules are passed as a `data.frame`
# #
# # (cond <- cbind.data.frame(Y = c(0.2, 0.6), par = c("r", "r"), val = c(0.5, 0.1)))
# # xyplot(growth_ac_cond(cond=cond))
# #
# # # Combine a change of `r` and a change of `k`
# #
# # (cond <- cbind.data.frame(Y = c(0.2, 1.99), par = c("r", "k"), val = c(0.5, 3)))
# # xyplot(growth_ac_cond(cond=cond))
# #
# # # A fantasy growth process
# #
# # cond <- cbind.data.frame(Y = c(0.1, 1.99, 1.999, 2.5, 2.9),
# # par = c("r", "k", "r", "r","k"),
# # val = c(0.3, 3, 0.9, 0.1, 1.3))
# #
# # xyplot(growth_ac_cond(cond=cond))
# growth_ac_cond <- function(Y0 = 0.01, r = 0.1, k = 2, cond = cbind.data.frame(Y = 0.2, par = "r", val = 2), N = 100){
#   # Create a vector Y of length N, which has value Y0 at Y[1]
#   Y <- c(Y0, rep(NA, N-1))
#   # Iterate N steps of the difference equation with values passed for Y0, k and r.
#   cnt <- 1
#   for(t in seq_along(Y)){
#     # Check if the current value of Y is greater than the threshold for the current conditional rule in cond
#     if(Y[t] > cond$Y[cnt]){
#       # If the threshold is surpassed, change the parameter settings by evaluating: cond$par = cond$val
#       eval(parse(text = paste(cond$par[cnt], "=", cond$val[cnt])))
#       # Update the counter if there is another conditional rule in cond
#       if(cnt < nrow(cond)){cnt <- cnt + 1}
#     }
#     # Van Geert growth model
#     Y[[t+1]] <- Y[t] * (1 + r - r * Y[t] / k)
#   }
#   return(stats::ts(Y))
# }
#
#
# # HELPERS ----
#
# # Find change indices
# #
# # @param y An indicator variable representing different levels of a variable or factor
# # @param returnRectdata Return a dataframe suitable for shading a \code{ggplot2} graph with \code{\link[ggplot2]{geom_rect}}
# # @param groupVar Pass a value (length 1) or variable (length of y) that can be used as a variable to join the indices by if \code{returnRectdata = TRUE}
# # @param labelVar If \code{y} is not a character vector, provide a vector of labels equal to \code{length(y)}
# # @param discretize If \code{y} is a continuous variable, setting \code{discretize = TRUE} will partition the values of \code{y} into \code{nbins} number of bins, each value of \code{y} will be replaced by its bin number.
# # @param nbins Number of bins to use to change a continuous \code{y} (if \code{discretize = TRUE}) into a variable with \code{nbins} levels
# #
# # @return Either a vector with the indices of change in \code{y}, or, a data frame with variables \code{xmin,xmax,ymin,ymax,label}
# #
# # @export
# #
# ts_changeindex <- function(y, returnRectdata=TRUE, groupVar = NULL, labelVar = NULL, discretize=FALSE, nbins = 5){
#
#   if(!is.character(y)){
#     y <- ts_checkfix(y,checkNumericVector = TRUE, fixNumericVector = TRUE, checkWholeNumbers = TRUE, fixWholeNumbers = TRUE)
#   }
#
#   if(length(unique(y))>10){
#     warning("More than 10 epochs detected!")
#   }
#
#   xmax <- c(which(diff(y)!=0),NROW(y))
#   xmin <- c(1,xmax)[1:NROW(xmax)]
#   group <- rep(1,length(xmin))
#
#   if(!is.null(groupVar)){
#     if(length(groupVar)==1){
#       group <- groupVar
#     } else {
#       if(length(groupVar)==length(y)){
#         group <- groupVar[xmax]
#       } else {
#         warning("Group values incorrect, using: groupVar = 1")
#       }
#     }
#   }
#
#   if(!is.null(labelVar)){
#     label <- labelVar[xmax]
#   } else {
#     label <- y[xmax]
#   }
#
#   if(returnRectdata){
#     return(data.frame(group = group, xmin = xmin, xmax = xmax, ymin = -Inf, ymax = +Inf, label = label))
#   } else {
#     return(c(1,xmin))
#   }
# }
#
#
#
# # Delay embedding of a time series
# #
# # Create a state vector based on an embedding lag and a number of embedding dimanesions.
# #
# # @param y Time series
# # @param emDim Embedding dimension
# # @param emLag Embedding lag
# # @param returnOnlyIndices Return only the index of y for each surrogate dimension, not the values (default = `FALSE`)
# # @param silent Silent-ish mode
# #
# # @return The lag embedded time series
# # @family Time series operations
# #
# # @author Fred Hasselman
# #
# # @export
# #
# # @family Time series operations
# #
# ts_embed <- function (y, emDim, emLag, returnOnlyIndices = FALSE, silent = TRUE){
#
#   id <- ifelse(is.null(colnames(y)),ifelse(is.null(names(y)),deparse(substitute(y)),names(y)[1]),colnames(y)[1])
#   y.ori <- y
#   N  <- NROW(y)
#
#   if((emDim-1) * emLag > N){stop(paste0("Time series length (N = ",N,") is too short to embed in ",emDim," dimensions with delay ",emLag))}
#
#   if(!is.null(dim(y))){
#     y <- y_data <- as.numeric(y[,1])
#     if(!silent){cat("\ntaking first column...\n")}
#   } else {
#     y <- y_data <- as.numeric(y)
#   }
#
#   if(any(stats::is.ts(y), zoo::is.zoo(y), xts::is.xts(y))){
#     y <- stats::time(y)
#     emTime <- lubridate::as_datetime(y[emLag+1])- lubridate::as_datetime(y[1])
#   } else {
#     y <- zoo::index(y)
#     emTime <- emLag
#   }
#
#
#
#   if(emDim > 1){
#     lag.id    <- seq(1, (emDim*emLag), emLag)
#     maxN      <- N - max(lag.id)
#     emY       <- matrix(nrow = maxN, ncol = emDim)
#
#     for(tau in seq_along(lag.id)){
#       emY[,tau] = y[lag.id[tau]:(N-(rev(lag.id)[tau]))]
#     }
#     colnames(emY) <- paste0("tau.",0:(emDim-1))
#
#   } else {
#     emY <-  matrix(nrow = N, ncol = 1, byrow = TRUE, dimnames = list(NULL,"tau.0"))
#     emY <-  y
#   }
#
#   # Alternative: rollapply(y, list(-d * seq(0, k-1)), c)
#
#
#   attr(emY, "embedding.dims") <- emDim
#   attr(emY, "embedding.lag")  <- emLag
#   attr(emY, "embedding.time") <- emTime
#   attr(emY, "variable.y")     <- id
#
#   if(returnOnlyIndices){
#     attr(emY, "data.y") <- y_data
#     return(as.matrix(emY))
#   } else {
#     if(emDim>1){
#       for(c in 1:NCOL(emY)){
#         emY[,c] <-  y_data[emY[,c]]
#       }
#     } else{
#       emY <- as.matrix(y_data)
#     }
#     return(emY)
#   }
# }
#
#
# # Discrete representation
# #
# #  Return a discrete representation of \code{y} by binning the observed values and returning the transfer probabilities.
# #
# # @param y Numeric vector or time series to be discretised.
# # @param nbins Number of bins to use for calculating transfer probabilities (default = \code{ceiling(2*length(y)^(1/3))})
# # @param keepNA If \code{TRUE}, any \code{NA} values will first be removed and later re-inserted into the discretised time series.
# #
# # @return A discretised version of \code{y}
# #
# # @export
# #
# ts_discrete <- function(y, nbins=ceiling(2*NROW(y)^(1/3)), keepNA = TRUE){
#
#   idNA <- is.na(y)
#   y <- y[!idNA]
#
#   # Make a binvector
#   binvec <- seq(min(y)-0.001, max(y),length.out=nbins+1)
#   # Apply the bins
#   bins <- vector("numeric",NROW(y))
#   for(b in 1:nbins) {
#     bins[y%(]%c(binvec[b],binvec[b+1])] <- b
#   }
#
#   if(keepNA){
#     y <- rep(NA,length(idNA))
#     y[!idNA] <- bins
#   } else {
#     y <- bins
#   }
#
#   return(y)
# }
#
#
# # Time series to Duration series
# #
# # @param y A time series, numeric vector, or categorical variable.
# # @param timeVec A vector, same length as \code{y} containing timestamps, dates, or, sample indices.
# # @param fs Optional sampling frequency if timeVec represents sample indices. An extra column \code{duration.fs} will be added which represents \code{1/fs * duration in samples}
# # @param tolerance A number \code{tol} indicating a range \code{[y-tol,y+tol]} to consider the same value. Useful when \code{y} is continuous (\code{default = 0})
# #
# # @return
# # @export
# #
# # @examples
# ts_durationJMV <- function(y, timeVec = stats::time(y),fs = stats::frequency(y), tolerance = 0){
#   tID <- seq_along(y)[-1]
#
#   same <- list()
#   same[[1]] <- data.frame(y=y[1],
#                           ind.start = 1,
#                           ind.end   = 1,
#                           t.start = timeVec[1],
#                           t.end = timeVec[1],
#                           duration.time = 0,
#                           duration.samples = 1,
#                           duration.fs = fs,
#                           keep = TRUE)
#
#   for(i in tID){
#
#     samesame <- FALSE
#     yval <- y[i]
#     if(is.numeric(yval)){
#       if(yval%[]%c((y[i-1]-tolerance),(y[i-1]+tolerance))){
#         samesame <- TRUE
#       }
#     }
#     if(is.character(yval)|is.factor(yval)){
#       if(yval==y[i-1]){
#         yval <- y[i]
#         samesame <- TRUE
#       }
#     }
#
#     if(samesame){
#       same[[i]] <- data.frame(y=yval, ind.start = same[[i-1]]$ind.start, ind.end = i, t.start = same[[i-1]]$t.start, t.end = timeVec[i], duration.time = 0, duration.samples = (same[[i-1]]$duration.samples+1), duration.fs = fs, keep = TRUE)
#       same[[i-1]]$keep <- FALSE
#     } else {
#       same[[i]] <- data.frame(y=y[i], ind.start = i, ind.end = i, t.start = timeVec[i], t.end = timeVec[i],duration.time = 0, duration.samples = 1,  duration.fs = fs, keep = TRUE)
#     }
#   }
#   same.out <- plyr::ldply(same)
#   same.out <- same.out[same.out$keep,1:8]
#   if(!is.null(fs)){same.out$duration.fs = (1/fs)*same.out$duration.samples}
#   timeVec[NROW(timeVec+1)] <- timeVec[NROW(timeVec)]
#   same.out$duration.time = timeVec[same.out$ind.end+1] - timeVec[same.out$ind.start] #laply(seq_along(same.out$ind.start) function(i){(same.out$t.end[same.out$ind.end] - same.out$t.start[same.out$ind.start])}
#   row.names(same.out) <- paste(seq_along(same.out$t.start))
#   same.out$trajectory.id <- seq_along(same.out$t.start)
#   return(same.out)
# }
#
#
#
# # Time series to Duration series
# #
# # @param y A time series, numeric vector, or categorical variable.
# # @param timeVec A vector, same length as `y` containing timestamps, or, sample indices.
# # @param fs Optional sampling frequency if timeVec represents sample indices. An extra column `duration.fs` will be added which represents `1/fs * duration in samples`
# # @param tolerance A number `tol` indicating a range `[y-tol,y+tol]` to consider the same value. Useful when `y` is continuous (`default = 0`)
# #
# # @return A data frame
# # @export
# #
# # @family Time series operations
# #
# # @examples
# # library(invctr)
# # # Create data with events and their timecodes
# # coder <- data.frame(beh=c("stare","stare","coffee","type","type","stare"),t=c(0,5,10,15,20,25))
# #
# # ts_duration(y = coder$beh, timeVec = coder$t)
# #
# ts_duration <- function(y, timeVec = stats::time(y), fs = stats::frequency(y), tolerance = 0){
#
#
#   if(plyr::is.discrete(y)){
#     y.n <- as.numeric_discrete(y,sortUnique = TRUE)
#   } else {
#     y.n <- as.numeric(y)
#     names(y.n) <- paste(y.n)
#     if(all(is.na(y.n))){
#       stop("Conversion to numeric failed for all elements of y.")
#     }
#   }
#
#   y <- y.n
#   tID <- seq_along(y)[-1]
#   y[NROW(y)+1]             <- max(y, na.rm = TRUE) + tolerance + 1
#   timeVec[NROW(timeVec)+1] <- max(timeVec, na.rm = TRUE)
#
#   same <- list()
#   same[[1]] <- data.frame(y = y[1],
#                           y.name  = names(y)[1],
#                           ind.start = 1,
#                           ind.end   = 1,
#                           t.start = timeVec[1],
#                           t.end   = timeVec[1],
#                           duration.time    = 0,
#                           duration.samples = 1,
#                           duration.fs      = fs,
#                           keep = TRUE)
#
#   for(i in tID){
#     # Same as previous?
#     if(y[i]%[]%c((y[i-1]-tolerance),(y[i-1]+tolerance))){
#       same[[i]] <- data.frame(y = y[i],
#                               y.name  = names(y[i]),
#                               ind.start = same[[i-1]]$ind.start,
#                               ind.end   = i,
#                               t.start = same[[i-1]]$t.start,
#                               t.end   = timeVec[i],
#                               duration.time    = 0,
#                               duration.samples = (same[[i-1]]$duration.samples+1),
#                               duration.fs      = fs,
#                               keep = TRUE)
#       same[[i-1]]$keep <- FALSE
#     } else {
#       same[[i-1]]$t.end <- timeVec[i]
#       # Same as upcoming?
#       if((y[i]%[]%c((y[i+1]-tolerance),(y[i+1]+tolerance)))){
#         same[[i]] <- data.frame(y = y[i],
#                                 y.name  = names(y[i]),
#                                 ind.start = i,
#                                 ind.end   = (i+1),
#                                 t.start = timeVec[i],
#                                 t.end   = timeVec[i+1],
#                                 duration.time    = 0,
#                                 duration.samples = 1,
#                                 duration.fs = fs,
#                                 keep = FALSE)
#       }
#
#       if(i == max(tID)){
#         same[[i]] <- data.frame(y = y[i],
#                                 y.name  = names(y[i]),
#                                 ind.start = i,
#                                 ind.end   = i,
#                                 t.start = timeVec[i],
#                                 t.end   = timeVec[i],
#                                 duration.time    = 0,
#                                 duration.samples = 1,
#                                 duration.fs = fs,
#                                 keep = TRUE)
#       } else {
#
#         same[[i]] <- data.frame(y = y[i],
#                                 y.name  = names(y[i]),
#                                 ind.start = i,
#                                 ind.end   = (i+1),
#                                 t.start = timeVec[i],
#                                 t.end   = timeVec[i+1],
#                                 duration.time    = 0,
#                                 duration.samples = 1,
#                                 duration.fs = fs,
#                                 keep = TRUE)
#       }
#     }
#   }
#   same.out <- plyr::ldply(same)
#   same.out <- same.out[same.out$keep,1:8]
#   if(!is.null(fs)){same.out$duration.fs = (1/fs)*same.out$duration.samples}
#   same.out$duration.time = (same.out$t.end - same.out$t.start)
#   row.names(same.out) <- paste(seq_along(same.out$t.start))
#   return(same.out)
# }
#
#
#
# # Symbolic representation
# #
# #  Return a discrete representation of \code{y} by binning the observed values and returning the transfer probabilities.
# #
# # @param y Numeric vector or time series to be discretised.
# # @param keepNA If \code{TRUE}, any \code{NA} values will first be removed and later re-inserted into the discretised time series.
# # @param doPlot Create a plot of the symbolized series.
# # @param orderedFactor Create an ordered factor?
# #
# # @return A discretised version of \code{y}
# #
# # @export
# #
# ts_symbolic <- function(y, keepNA = TRUE, orderedFactor = TRUE, doPlot = FALSE){
#
#   cl <- class(y)
#   cnames <- colnames(y)
#   if(is.null(cnames)){
#     cnames = paste0("Y",1:NCOL(y))
#   }
#
#   idNA <- plyr::colwise(.fun = is.na)(as.data.frame(y))
#   y <- as.data.frame(y)[stats::complete.cases(!idNA),]
#
#   sym <- symbolize(xy = y)
#
#   if(keepNA){
#     sym_num <- matrix(NA,nrow=NROW(idNA),ncol = NCOL(idNA))
#     for(c in 1:NCOL(y)){
#       sym_num[!idNA[,c],c] <- sym[,c]
#     }
#   } else {
#     sym_num <- sym
#   }
#
#   eval(parse(text=paste0("sym_num <- as.",cl,"(sym_num)")))
#
#   sym_label <- sym_num
#
#   if(!is.null(ncol(sym_num))){
#     for(c in 1:NCOL(y)){
#       sym_label[,c] <-  dplyr::case_when(
#         sym_num[,c]==1 ~ "trough",
#         sym_num[,c]==2 ~ "decrease",
#         sym_num[,c]==3 ~ "same",
#         sym_num[,c]==4 ~ "increase",
#         sym_num[,c]==5 ~ "peak",
#         is.na(sym_num[,c]) ~ NA_character_)
#     }
#   } else {
#     sym_label <-  dplyr::case_when(
#       sym_num==1 ~ "trough",
#       sym_num==2 ~ "decrease",
#       sym_num==3 ~ "same",
#       sym_num==4 ~ "increase",
#       sym_num==5 ~ "peak",
#       is.na(sym_num) ~ NA_character_)
#   }
#
#   if(class(sym_num)%in%"matrix"){
#     out <- sym_num
#     colnames(sym_num)  <- paste0(cnames,"_sym_num")
#     anames <- paste0(cnames,"_sym_label")
#     for(c in 1:NCOL(out)){
#       attr(out,anames[c]) <- factor(sym_label[,c], ordered = orderedFactor, exclude = c(NA,"NA"))
#     }
#   } else {
#     if(!is.null(ncol(sym_num))){
#       for(c in 1:NCOL(sym_label)){
#         sym_label[,c] <- factor(sym_label[,c], ordered = orderedFactor, exclude = c(NA,"NA"))
#       }
#       colnames(sym_num) <- paste0(cnames,"_sym_num")
#       colnames(sym_label) <- paste0(cnames,"_sym_label")
#       out <- cbind(sym_num,sym_label) #,stringsAsFactors = FALSE)
#     } else {
#       out <- factor(sym_label, ordered = orderedFactor, exclude = c(NA,"NA"))
#       attr(out,"sym_numeric") <- sym_num
#     }
#   }
#
#
#   if(doPlot){
#
#     shp <- c("trough" = 25, "decrease" = 21, "same"=23,"increase" = 22, "peak" = 24,"missing" = 8)
#     col <- c("trough"="darkkhaki","decrease"="orange","same"="grey60","increase"="steelblue","peak"="olivedrab","missing"="red")
#     labs <- c("0" = "missing","1"="trough","2"="decrease","3"="same","4"="increase","5"="peak")
#
#     if(!is.null(ncol(sym_num))){
#       out$time <- 1:NROW(out)
#       df_p1 <- out %>% dplyr::as_tibble() %>% dplyr::select(dplyr::ends_with("_sym_label"),"time") %>% tidyr::gather(key="lab_var", value="sym_label", -"time")
#       df_p1$sym_label[is.na(df_p1$sym_label)] <- "missing"
#       df_p2 <- out %>% dplyr::as_tibble() %>% dplyr::select(dplyr::ends_with("_sym_num")) %>% tidyr::gather(key="num_var", value="sym_num")
#       df_p2[is.na(df_p2)] <- 0
#       df_plot <- cbind(df_p1,df_p2)
#       df_plot$num_var <- gsub("_sym_num","",df_plot$num_var)
#     } else {
#       df_plot <- data.frame(time = 1:NROW(out), sym_num= attr(out,"sym_numeric"), sym_label = out)
#       df_plot$sym_num[is.na(df_plot$sym_num)] <- 0
#       df_plot$sym_label[is.na(df_plot$sym_num)] <- "missing"
#     }
#
#
#     g <- ggplot(df_plot,ggplot2::aes_(x=~time,y=~sym_num)) +
#       geom_line(color="grey80") +
#       geom_point(ggplot2::aes_(shape=~sym_label, color=~sym_label, fill=~sym_label),size=2)
#
#     if(!is.null(df_plot$num_var)){
#       if(length(unique(df_plot$num_var))>1){
#         g <- g + facet_grid(num_var~.)
#       }
#     }
#
#     g <- g +
#       scale_x_continuous("Time") +
#       scale_shape_manual("Symbolic value", values = shp) +
#       scale_color_manual("Symbolic value", values = col) +
#       scale_fill_manual("Symbolic value", values = col) +
#       scale_y_continuous("",labels = labs) +
#       theme_bw() + theme(panel.grid = element_blank())
#
#     graphics::plot(g)
#
#     return(list(data=out,plot=invisible(g)))
#   } else {
#     return(out)
#   }
#
#
#
# }
#
# # Convert numeric vectors to symbolic vectors.
# #
# # \code{symbolize} converts numeric vectors to symbolic vectors. It is a helper
# #   function for \code{muti}.
# #
# # @param xy An n x 2 \code{matrix} or \code{data.frame} containing the two
# #   vectors of interest.
# #
# # @return An (n-2) x 2 \code{matrix} of integer symbols that indicate whether
# #   the i-th value, based on the i-1 and i+1 values, is a "trough" (=1),
# #   "decrease" (=2), "same" (=3), "increase" (=4), or "peak" (=5).
# #
# # @author Mark Scheuerell (https://mdscheuerell.github.io/muti/)
# #
# symbolize <- function(xy) {
#   ## of interest and converts them from numeric to symbolic.
#   ## check input for errors
#   if(is.null(dim(xy))) {
#     xy <-  as.matrix(xy)
#   }
#   ## get ts length
#   TT <- dim(xy)[1]
#   ## init su matrix
#   su <- matrix(NA, TT, NCOL(xy))
#   ## convert to symbols
#   ## loop over 2 vars
#   for(i in 1:NCOL(xy)) {
#     for(t in 2:(TT-1)) {
#       ## if xy NA, also assign NA to su
#       if(any(is.na(xy[(t-1):(t+1),i]))) { su[t,i] <- NA }
#       ## else get correct symbol
#       else {
#         if(xy[t,i] == xy[t-1,i] | xy[t,i] == xy[t+1,i]) {
#           ## same
#           su[t,i] <- 3
#         }
#         if(xy[t,i] > xy[t-1,i]) {
#           ## peak
#           if(xy[t,i] > xy[t+1,i]) { su[t,i] <- 5 }
#           ## increase
#           else { su[t,i] <- 4 }
#         }
#         if(xy[t,i] < xy[t-1,i]) {
#           ## trough
#           if(xy[t,i] < xy[t+1,i]) { su[t,i] <- 1 }
#           ## decrease
#           else { su[t,i] <- 2 }
#         }
#       } ## end else
#     } ## end t loop
#   } ## end i loop
#   ## return su matrix
#   return(su)
# }
#
# ts_resample <- function(y, nbins = ceiling(2*length(y)^(1/3)), keepNA = TRUE){
#
#   bins <- ts_discrete(y, nbins)
#   transmat <- matrix(0,nbins,nbins)
#   for(i in 1:nbins) {
#     for(j in 1:nbins) {
#       transmat[i,j] <- sum(bins[-NROW(y)]==i & bins[-1]==j)
#     }
#     if(sum(transmat[i,])>0) { transmat[i,] <- transmat[i,]/sum(transmat[i,]) }
#     else { transmat[i,] <- 1/nbins }
#   }
#
# }
#
# # Derivative of time series
# #
# # Iteratively differenced series up to \code{order}. The same length as the original series is recovered by calculating the mean of two vectors for each iteration: One with a duplicated first value and one with a duplicated last value.
# #
# # @param y A timeseries object or numeric vector or a matrix in which columns are variables and rows are numeric values observed over time.
# # @param order How many times should the difference iteration be applied? (default = \code{1})
# # @param addColumns Should the derivative(s) be added to the input vector/matrix as columns? (default = \code{TRUE})
# # @param keepDerivatives If \code{TRUE} and \code{order > 1}, all derivatives from \code{1:order} will be returned as a matrix )default = \code{FALSE})
# # @param maskEdges Mask the values at the edges of the derivatives by any numeric type that is not \code{NULL} (default = \code{NULL})
# # @param silent Silent-ish mode
# #
# # @return Depending on the setting of \code{addColumns} and the object type passed as \code{y}, a vector of equal length as \code{y} iteratively differenced by \code{order} times; a matrix with derivatives, or a matrix with original(s) and derivative(s).
# #
# # @note The values at the edges of the derivatives represent endpoint averages and should be excluded from any subsequent analyses. Set argument \code{maskEdges} to a value of your choice.
# #
# # @export
# #
# # @examples
# #
# # # Get an intereting numeric vector from package DescTools
# # y <- DescTools::Fibonacci(1:26)
# #
# # # Return the first order derivative as a vector
# # ts_diff(y=y,addColumns=FALSE)
# #
# # # Return original and derivative as a matrix
# # plot(ts_diff(y=y, addColumns=TRUE))
# #
# # # Works on multivariate data objects with mixed variable types
# # df <- data.frame(x=letters, y=1:26, z=y)
# #
# # # Returns only derivatives of the numeric colunmns
# # ts_diff(y=df,addColumns=FALSE)
# #
# # # Returns original data with derivatives of the numeric columns
# # ts_diff(y = df, order=4, addColumns=TRUE)
# #
# # # Plot logistic S-curve and derivatives 1 to 3
# # plot(ts(ts_diff(stats::plogis(seq(-5,5,.1)),
# # order=3, keepDerivatives = TRUE)),  main="ts_derivative")
# #
# ts_diff <- function(y, order= 1, addColumns = TRUE, keepDerivatives = FALSE, maskEdges = NULL, silent = TRUE){
#
#   N <- NCOL(y)
#   if(NROW(y)<N){
#     warning(paste0("Assuming variables in columns and observations in rows!\nDo you really have ",N," columns with time series of length ",NROW(y),"?"))
#   }
#
#   if(!is.null(maskEdges)){
#     if(is.numeric(maskEdges)|is.na(maskEdges)){
#       if(length(maskEdges)==1){
#         maskEdges <- as.numeric(maskEdges)
#       } else {
#         maskEdges = NULL
#       }
#     } else {
#       maskEdges = NULL
#     }
#   }
#
#   Nnum <- plyr::laply(y,is.numeric)
#   if(length(Nnum)<1){stop("Need at least one numeric vector!")}
#
#   if(N==1){
#     Nnum <- TRUE
#     dy <- as.matrix(y)
#   } else {
#     if(is.null(dimnames(y))){
#       dimnames(y) <- list(dimnames(y)[[1]], gsub("[[:punct:]]","", paste0(deparse(substitute(y)),1:NCOL(y))))
#     }
#     dy <- as.matrix(y[,(1:N)[Nnum]])
#   }
#   if(is.null(dimnames(dy)[[2]])){
#     dynames <- paste0("y",1:NCOL(dy))
#   } else {
#     dynames <- paste(dimnames(dy)[[2]])
#   }
#   dimnames(dy) <- list(dimnames(dy)[[1]],paste0(dynames,"_d",order))
#
#   if((N-NCOL(dy))>=0){
#     if(!silent){cat(paste0("\nCalulating derivative for ",NCOL(dy)," time series.\n"))}
#   }
#
#   keepDiffs <- list() # matrix(NA, nrow = NROW(dy),ncol = length(1:order)*NCOL(dy))
#   maskWhich <- list()
#   # Repeat the difference operation
#   for(o in 1:order){
#     dif <- diff(dy)
#     dy  <- cbind((rbind(dif[1,], dif)+rbind(dif,dif[NROW(dif),]))/2)
#     if(!is.null(maskEdges)){
#       maskWhich[[o]] <- c(1:o,(NROW(dy)-o):NROW(dy))
#     }
#     if(keepDerivatives){
#       keepDiffs[[o]] <- dy
#     }
#   }
#
#   if(keepDerivatives){
#     dy <- as.data.frame(keepDiffs)
#     colnames(dy) <- paste0(dynames,"_d",1:order)
#   }
#
#   if(!is.null(maskEdges)){
#     for(c in seq_along(maskWhich)){
#       dy[maskWhich[[c]],c] <- maskEdges
#     }
#   }
#
#   if(addColumns){
#     if(N==1){y<-as.matrix(y)}
#     out <- cbind(y,dy)
#     #class(out) <- class(y)
#     return(out)
#   } else {
#     if(N==1){
#       return(as.vector(dy))
#     } else {
#       return(dy)
#     }
#   }
# }
#
#
# # Adjust time series by summation order
# #
# # Many fluctuation analyses assume a time series' Hurst exponent is within the range of \code{0.2 - 1.2}. If this is not the case it is sensible to make adjustments to the time series, as well as the resutling Hurst exponent.
# #
# # @param y A time series of numeric vector
# # @param scaleS The scales to consider for \code{DFA1}
# # @param polyOrder Order of polynomial for detrending in DFA (default = \code{1})
# #
# # @return The input vector, possibly adjusted based on \code{H} with an attribute \code{"Hadj"} containing an integer by which a Hurst exponent calculated from the series should be adjusted.
# #
# # @details Following recommendations by [Ihlen (2012)](https://www.frontiersin.org/files/Articles/23948/fphys-03-00141-r2/image_m/fphys-03-00141-t001.jpg), a global Hurst exponent is estimated using DFA and \code{y} is adjusted accordingly:
# # \itemize{
# # \item{\code{1.2 < H < 1.8} first derivative of y, atribute \code{Hadj = 1}}
# # \item{\code{H > 1.8} second derivative of y, atribute \code{Hadj = 2}}
# # \item{\code{H < 0.2} y is centered and integrated, atribute \code{Hadj = -1}}
# # \item{\code{0.2 <= H <= 1.2 } y is unaltered, atribute \code{Hadj = 0}}
# # }
# #
# # @references Ihlen, E. A. F. E. (2012). Introduction to multifractal detrended fluctuation analysis in Matlab. Frontiers in physiology, 3, 141.
# #
# # @export
# #
# ts_sumorder <- function(y, scaleS = NULL, polyOrder = 1, minData = minData){
#
#   if(is.null(scaleS)){
#     scaleS <- unique(round(2^(seq(2, floor(log2(NROW(y)/2)), by=((floor(log2(NROW(y)/2))-2)/30)))))
#   }
#
#   # Check global H
#   TSm      <- as.matrix(cbind(t=1:NROW(y),y=ts_integrate(ts_center(y))))
#   Hglobal  <- monoH(TSm = TSm, scaleS = scaleS, polyOrder = polyOrder, returnPLAW = TRUE, returnSegments = TRUE)
#   rm(TSm)
#
#   fitRange <- which(lapply(Hglobal$segments,NROW)>=minData)
#
#   lmfit1        <- stats::lm(Hglobal$PLAW$bulk.log2 ~ Hglobal$PLAW$size.log2, na.action=stats::na.omit)
#   H1  <- lmfit1$coefficients[2]
#   lmfit2        <- stats::lm(Hglobal$PLAW$bulk.log2[fitRange] ~ Hglobal$PLAW$size.log2[fitRange], na.action=stats::na.omit)
#   H2  <- lmfit2$coefficients[2]
#
#
#   # Adjust TS by global H
#   Hadj <- 0
#   if(H2%(]%c(1.2,1.8)){
#     y <- ts_diff(y,addColumns = FALSE)
#     Hadj=1
#   }
#   if(H2>1.8){
#     y <- ts_diff(ts_diff(y, addColumns = FALSE), addColumns = FALSE)
#     Hadj <- 2
#   }
#   if(H2%[]%c(0.2,1.2)){
#     Hadj <- 0
#   }
#   if(H2 < 0.2){
#     y <- ts_integrate(ts_center(y))
#     Hadj <- -1
#   }
#
#   attr(y,"Hglobal.full") <- H1
#   attr(y,"Hglobal.excl") <- H2
#   attr(y,"Hadj")         <- Hadj
#
#   return(y)
# }
#
# # Check and/or Fix a vector
# #
# # @param y A time series object or numeric vector
# # @param checkNumericVector is 1D numeric vector?
# # @param checkTimeVector has time vector?
# # @param checkWholeNumbers contains only wholenumbers?
# # @param checkPow2 length is power of 2?
# # @param checkScale checkScale
# # @param checkSummationOrder checkSummationOrder
# # @param checkNonStationarity checkNonStationarity
# # @param checkNonHomogeneity checkNonHomogeneity
# # @param fixNumericVector return a 1D numeric vector (WARNING: Data frames and Matrices with NCOL > 1 wil be converted to long form)
# # @param fixWholeNumbers fixWholeNumber
# # @param fixTimeVector fixTimeVector
# # @param fixPow2 foxPow2
# # @param fixNA fixNA
# # @param fixScale fixScale
# # @param fixSummationOrder fixSummationOrder
# # @param fixNonStationarity fixNonStationarity
# # @param fixNonHomogeneity fixNonHomogeneity
# #
# # @return A 'check' report and/or a 'fixed' vector y.
# # @export
# #
# ts_checkfix <- function(y, checkNumericVector = TRUE, checkWholeNumbers = FALSE, checkTimeVector = FALSE, checkPow2 = FALSE, checkScale = FALSE, checkSummationOrder = FALSE, checkNonStationarity = FALSE, checkNonHomogeneity = FALSE, fixNumericVector = FALSE, fixWholeNumbers = FALSE, fixTimeVector = FALSE, fixPow2 = FALSE, fixNA = TRUE, fixScale = FALSE, fixSummationOrder = FALSE, fixNonStationarity = FALSE, fixNonHomogeneity = FALSE){
#
#   outtext <- list()
#   pre <- "\ny"
#   post <- "\n"
#   i <- 0
#
#   whichClass <- class(y)
#   yesNumeric <- FALSE
#   yesWholeNumbers <- FALSE
#   yesTime    <- FALSE
#   yesPow2    <- FALSE
#   yesScaled  <- FALSE
#   yesNonStationary <- FALSE
#
#   # Numeric
#   tmptxt <- ""
#   txt <- "NUMERIC"
#   if(checkNumericVector){
#     if(is.numeric(y)){
#       txt <- c(txt,"is numeric")
#       yesNumeric <- TRUE
#     } else {
#       tmptxt <- "is NOT numeric"
#     }
#   }
#
#   if(fixNumericVector&!yesNumeric){
#     if(is.null(dim(y))){
#       suppressWarnings(y <- as.numeric(unlist(y)))
#     } else {
#       if(dim(y)[[2]]==1){suppressWarnings(y <- as.numeric(y[,1]))
#       } else {
#         if(dim(y)[[2]]>1){suppressWarnings(y <- y %>%  tidyr::gather(key="colname",value="value") %>%  as.numeric(.$value))
#         }
#       }
#     }
#     tmptxt <- paste(tmptxt,"... FIXED by as.numeric(y)")
#     yesNumeric <- TRUE
#   }
#   txt <- c(txt, tmptxt)
#   outtext[[i%++%1]] <- paste(pre,txt,post)
#   rm(txt,tmptxt)
#
#
#   # Wholenumber
#   tmptxt <- ""
#   txt <- "WHOLENUMBER"
#   if(all(is.wholenumber(y))){
#     txt <- c(txt,"only contains whole numbers")
#     yesWholeNumbers <- TRUE
#   } else {
#     tmptxt <- "does NOT only contain whole numbers"
#   }
#   if(fixWholeNumbers&!yesWholeNumbers){
#     suppressWarnings(y <- round(y))
#     tmptxt <- paste(tmptxt,"... FIXED by round(y)")
#     yesWholeNumbers <- TRUE
#   }
#   txt <- c(txt, tmptxt)
#   outtext[[i%++%1]] <- paste(pre,txt,post)
#   rm(txt,tmptxt)
#
#   # TimeVector
#   tmptxt <- ""
#   txt <- "TIMEVECTOR"
#   if(checkTimeVector){
#     if(grepl("(ts|mts|xts|zoo)",whichClass)){
#       txt <-c(txt,"has a time vector:",stats::tsp(y))
#       yesTime <- TRUE
#     } else {
#       tmptxt <- "does not have a time vector"
#     }
#   }
#
#   if(is.list(fixTimeVector&!yesTime)){
#     if(all(names(fixTimeVector)%in%names(formals(stats::ts)))){
#       fixTimeVector$data <- NULL
#       etxt <- paste0("stats::ts(data = y, ",paste0(names(fixTimeVector)," = ",fixTimeVector,collapse = ", "),")")
#       y <- eval(parse(text = paste(etxt)))
#       tmptxt <- paste(tmptxt,"... FIXED by",etxt)
#       yesTime <- TRUE
#       rm(etxt)
#     } else {
#       y <- stats::ts(y)
#       tmptxt <- paste(tmptxt,"... FIXED by ts(y)")
#       yesTime <- TRUE
#     }
#   }
#   txt <- c(txt, tmptxt)
#   outtext[[i%++%1]] <- paste(pre,txt,post)
#   rm(txt,tmptxt)
#
#   # Stationarity
#   tmptxt <- ""
#   txt<-"NONSTATIONARY"
#   if(checkNonStationarity){
#     tst1 <- suppressWarnings(tseries::kpss.test(y,null = "Trend"))$p.value
#     tst2 <- 1-suppressWarnings(tseries::adf.test(y, alternative = "stationary"))$p.value
#     tst3 <- 1-suppressWarnings(tseries::pp.test(y, alternative = "stationary"))$p.value
#     result <- sum(c(tst1,tst2,tst3)<.05, na.rm = TRUE)
#     if(result>1){
#       txt <- c(txt,paste0(result,"is NOT stationary (",result,"/3 tests conclude y is stationary)"))
#       yesNonStationary <- TRUE
#     } else {
#       txt <- c(txt,paste0(result,"is stationary (",result,"/3 tests conclude y is not stationary)"))
#     }
#   }
#   if(fixNonStationarity&!yesNonStationary){
#     y <- ts_detrend(y)
#   }
#   txt <- c(txt, tmptxt)
#   outtext[[i%++%1]] <- paste(pre,txt,post)
#   rm(txt,tmptxt)
#
#   return(y)
#
# }
#
# # Trim or Fill Vectors
# #
# # Trim the largest vector by cutting it, or filling it with `NA`.
# # Fill the shortest vector with padding.
# #
# # @param x A numeric vector
# # @param y A numeric vector
# # @param action Use \code{"fill"} to fill the shortest vector with \code{padding} (default); \code{"trim.cut"} to trim the longest vector to the length of the shortest; \code{"trim.NA"} to fill the longest vector with \code{NA}. This is a shortcut for running \code{action = "trim.cut"} with \code{padding=NA}, which can be useful if one wants to match the shortest series, but preserve the original length of largest vector.
# # @param type Should trimming or filling take place at the \code{"end"} (default), or \code{"front"} of the vector? The option \code{"center"} will try to distribute trimming by \code{NA} or filling by \code{padding} evenly across the front and end of the vector.
# # @param padding A value to use for padding (default = \code{0})
# # @param silent Run silent-ish
# #
# # @return A list with two vectors of equal length.
# #
# # @seealso il_mi
# #
# # @family Time series operations
# #
# # @author Fred Hasselman
# #
# # @export
# #
# #
# ts_trimfill <- function(x,y,action=c("fill","trim.cut","trim.NA")[1],type=c("end","center","front")[1],padding=0,silent=TRUE){
#
#   if(all(is.numeric(x),is.numeric(y))){
#
#     l        <- lengths(list(x=x,y=y))
#     oriMin   <- names(l)[which.min(l)]
#     oriMax   <- names(l)[which.max(l)]
#     ldiff    <- abs(diff(l))
#
#     if(ldiff>0){
#
#       if(any(action%in%c("trim.NA","fill"))){
#         if(action=="fill"){
#           if(!silent){message(paste("Padded shortest input",names(which.min(l)),"with", ldiff,"times",padding))}
#         } else {
#           padding <- NA
#           if(!silent){message(paste("Trimmed longest input",names(which.max(l)),"with",ldiff,"times",padding))}
#         } # if "filll
#         if(type=="end"){
#           out <- list(list(x,y)[[which.max(l)]], c(list(x,y)[[which.min(l)]],rep(padding,ldiff)))
#         }
#         if(type=="front"){
#           out <- list(list(x,y)[[which.max(l)]], c(rep(padding,ldiff),list(x,y)[[which.min(l)]]))
#         }
#         if(type=="centered"){
#           front<- floor(ldiff/2)
#           end  <- ceiling(ldiff/2)
#           out  <- list(list(x,y)[[which.max(l)]], c(rep(padding,front),list(x,y)[[which.min(l)]],rep(padding,end)) )
#         }
#       }
#
#       if(action=="trim.cut"){
#         if(!silent){message(paste("Trimmed longest input",names(which.max(l)),"by",ldiff))}
#         if(type=="end"){
#           out <- list(list(x,y)[[which.max(l)]][1:(NROW(list(x,y)[[which.max(l)]])-ldiff)], list(x,y)[[which.min(l)]])
#         }
#         if(type=="front"){
#           out <- list(list(x,y)[[which.max(l)]][ldiff:NROW(list(x,y)[[which.max(l)]])], list(x,y)[[which.min(l)]])
#         }
#         if(type=="centered"){
#           front<-floor(ldiff/2)
#           end  <-ceiling(ldiff/2)
#           out <- list(list(x,y)[[which.max(l)]][front:(NROW(list(x,y)[[which.max(l)]])-end)], list(x,y)[[which.min(l)]])
#         }
#       } # if "trim.cut"
#
#     } else { # if ldiff > 0
#       if(!silent){message("Vectors have equal length.")}
#       out <- list(x=x,y=y)
#     }
#
#     names(out) <- c(oriMax,oriMin)
#     return(out[order(c(oriMax,oriMin))])
#
#   } else { # is.numeric
#     stop("Please use 2 numeric vectors!")
#   }
# }
#
# # Get sliding window indices
# #
# # @param y A time series or numeric vector
# # @param win Size of the window to slide across \code{y}
# # @param step Size of steps between windows. Can be larger than \code{win}, but is ignored if \code{overlap} is not {NA}.
# # @param overlap A value between \code{[0 .. 1]}. If overlap is not \code{NA} (default), the value of \code{step} is ignored and set to \code{floor(overlap*win)}. This produces indices in which the size of \code{step} is always smaller than \code{win}, e.g. for fluctuation analyses that use binning procedures to represent time scales.
# # @param adjustY If not \code{NA}, or, \code{FALSE} a list object with fields that match one or more arguments of \link[casnet]{ts_trimfill} (except for \code{x,y}), e.g. \code{list(action="trim.NA",type="end",padding=NA,silent=TRUE)}. See \code{Return value} below for details.
# #
# # @return If \code{adjustY = FALSE}, or, a list object with fields that represent arguments of \link[casnet]{ts_trimfill}, then the (adjusted) vector \code{y} is returned with an attribute \code{"windower"}. This is a list object with fields that contain the indices for each window that fits on \code{y}, given \code{win}, \code{step} or \code{overlap} and the settings of \code{adjustY}. If \code{adjustY = NA}, only the list object is returned.
# # @export
# #
# ts_windower <- function(y, win=length(y), step=round(win/2), overlap=NA, adjustY=NA){
#
#   adjustOK <- FALSE
#   if(is.list(adjustY)){
#     if(any(names(adjustY)%in%names(formals(ts_trimfill)))){
#       adjustY[!names(adjustY)%in%names(formals(ts_trimfill))[-c(1,2)]] <- NULL
#       adjustY <- adjustY[!lengths(adjustY)==0]
#       if(is.null(adjustY)){
#         stop("No valid arguments of ts_trimfill passed to adjustY!")
#       } else {
#         adjustY<- plyr::llply(adjustY, function(e) if(is.character(e)){shQuote(e)} else {e})
#         adjustOK <- TRUE
#       }
#     }
#   }
#
#   if(!is.na(overlap)){step <- floor(overlap*win)}
#
#   wIndices <- list()
#   wIndex   <- seq(1,(NROW(y)-win),step)
#   iDiff    <- (dplyr::last(wIndex) + win-1)-NROW(y)
#
#   wIndices <- plyr::llply(wIndex,function(i){i:min(i+win-1,length(y))})
#
#   if(adjustOK){
#     if(iDiff<0){
#       if(adjustY$action%in%"fill"){
#         wIndices[[length(wIndices)]] <- c(wIndices[[length(wIndices)]],seq(max(wIndices[[length(wIndices)]]),max(wIndices[[length(wIndices)]])+abs(iDiff)))
#       }
#     }
#   }
#   names(wIndex) <- paste("stepsize",floor(step*win),"| window",1:length(wIndex))
#   return(wIndices)
# }
#
#
# # Detrend a time series
# #
# # @param y A time series ot numeric vector
# # @param polyOrder order Order of polynomial trend to remove
# #
# # @return Residuals after detrending polynomial of order \code{order}
# #
# # @export
# #
# # @family Time series operations
# #
# # @author Fred Hasselman
# #
# #
# ts_detrend <- function(y, polyOrder=1){
#   detR <- stats::lm(y~stats::poly(1:length(y), degree=polyOrder))$residuals
#   return(detR)
# }
#
#
# # Detect levels in time series
# #
# #  Use recursive partitioning function (\link[rpart]{rpart} from \code{rpart} to perform a 'classification' of relatively stable levels in a timeseries.
# #
# # @param y A time series of numeric vector
# # @param minDataSplit A factor indicating how many datapoints should be in a segment before it we analyze whether it contains a level change (default = \code{12})
# # @param minLevelDuration Minimum durtion (length) of a level (default = \code{round(minLevelChange/3)})
# # @param changeSensitivity A factor indicating a criterion of change that must occur before declaring a new level.
# # @param maxLevels Maximum number of levels in one series (default = \code{30})
# # @param method The partitioning method to use, see the manual pages of \link[rpart]{rpart} for details.
# #
# # @return A list object with fields \code{tree} and \code{pred}
# # @export
# #
# # @family Time series operations
# #
# # @author Fred Hasselman
# #
# ts_levels <- function(y, minDataSplit=12, minLevelDuration=round(minDataSplit/3), changeSensitivity = 0.01, maxLevels=30, method=c("anova","poisson","class","exp")[1]){
#   x <- seq_along(y)
#   dfs  <- data.frame(x=x, y=y)
#   tree <- rpart::rpart(y ~ x,
#                        method=method,
#                        control = list(
#                          minsplit=minDataSplit,
#                          minbucket = minLevelDuration,
#                          maxdepth = maxLevels,
#                          cp = changeSensitivity),
#                        data=dfs)
#
#   dfs$p <- stats::predict(tree, data.frame(x=x))
#
#   return(list(tree  = tree,
#               pred  = dfs))
# }
#
#
# # Find Peaks or Wells
# #
# # @param y A time series or numeric vector
# # @param window Window in whcih to look for peaks or wells
# # @param includeWells Find wells?
# # @param minPeakDist Minimum distance between peaks or wells
# # @param minPeakHeight Minimum height / depth for a peak / well
# #
# # @family Time series operations
# #
# # @author Fred Hasselman
# #
# # @return Index with peak or well coordinates
# # @export
# #
# ts_peaks <- function (y,
#                       window        = 3,
#                       includeWells  = FALSE,
#                       minPeakDist   = round(window/2),
#                       minPeakHeight = .2*diff(range(y, na.rm = TRUE))){
#
#   fp <- function(y,window){
#     shape <- diff(sign(diff(y, na.pad = FALSE)))
#     pksl <- sapply(which(shape < 0), FUN = function(i){
#       z <- i - window + 1
#       z <- ifelse(z > 0, z, 1)
#       w <- i + window + 1
#       w <- ifelse(w < length(y), w, length(y))
#       if(all(y[c(z : i, (i + 2) : w)] <= y[i + 1])){
#         return(i + 1)
#       } else {
#         return(numeric(0))
#       }
#     })
#     return(unlist(pksl))
#   }
#
#   pks <- fp(y,window)
#   if(includeWells){
#     wls <- fp(-1*y,window)
#     pks <- sort(c(pks,wls))
#   }
#
#   distOK    <- diff(c(NA,pks))>=minPeakDist
#   distOK[1] <- TRUE
#
#   heightOK <- rep(FALSE,length(pks))
#   for(p in seq_along(pks)){
#     if(abs(y[pks[p]] - mean(y[c(max(1,(pks[p]-window)):(pks[p]-1),(pks[p]+1):min((pks[p]+window),length(y)))])) >= minPeakHeight){
#       heightOK[p] <- TRUE
#     }
#   }
#   return(pks[heightOK&distOK])
# }
#
#
#
# # Center a vector
# #
# # @param numvec A numeric vector
# # @param na.rm Set the \code{na.rm} field
# # @param type Center on the \code{"mean"} (default) or the \code{"median"} of the vector.
# #
# # @return A mean or median centered vector
# # @export
# #
# # @family Time series operations
# #
# # @author Fred Hasselman
# #
# ts_center <- function(numvec, na.rm=TRUE, type = c("mean","median")[1]){
#   if(!is.numeric(numvec)){
#     stop("Vector must be numeric!")
#   } else {
#     switch(type,
#            mean   = return(numvec -   mean(numvec, na.rm=na.rm)),
#            median = return(numvec - stats::median(numvec, na.rm=na.rm))
#     )
#   }
# }
#
#
# # Standardise a vector
# #
# #
# # @param y A time series or numeric vector
# # @param na.rm Set the \code{na.rm} field
# # @param type Center on the \code{"mean"} and divide by \code{sd} (default), or center on \code{"median"} and divide by \code{mad}
# # @param adjustN Use the population SD estimator (\code{N-1}), or \code{N} (default = \code{TRUE})
# #
# # @return A standardised vector
# #
# # @export
# #
# # @family Time series operations
# #
# # @author Fred Hasselman
# #
# ts_standardise <- function(y, na.rm=TRUE, type = c("mean.sd","median.mad")[1], adjustN = TRUE){
#   if(!is.numeric(y)){
#     stop("Vector must be numeric!")
#   } else {
#     N <- NROW(y)
#     if(adjustN){
#       SDtype <- "Bessel"
#     } else {
#       SDtype <- "unadjusted"
#     }
#     switch(type,
#            mean.sd    = return((y - mean(y, na.rm = na.rm)) / ts_sd(y,na.rm = na.rm, type = SDtype)),
#            median.mad = return((y - stats::median(y, na.rm=na.rm)) / stats::mad(y, na.rm = na.rm))
#     )
#   }
# }
#
# # Standard Deviation estimates
# #
# # Calculates the population estimate of the standard deviation, or the unadjusted standard deviation.
# #
# # @param y Time series or numeric vector
# # @param na.rm Remove missing values
# # @param type Apply Bessel's correction (divide by N-1) or return unadjusted SD (divide by N)
# # @param silent Silent-ish mode (default = \code{TRUE})
# #
# # @return Standard deviation of \code{y}
# # @export
# #
# ts_sd <- function(y, na.rm=TRUE, type = c("Bessel","unadjusted")[1], silent=TRUE){
#
#   if(!is.numeric(y)){
#     stop("Vector must be numeric!")
#   }
#
#   if(na.rm){
#     y<-y[stats::complete.cases(y)]
#   }
#
#   N <- NROW(y)
#   Y_mean     <- mean(y)
#   Y_sd       <- stats::sd(y)
#   Bessel     <- sqrt((sum((y - Y_mean)^2)/(N-1)))
#   unadjusted <- sqrt((sum((y - Y_mean)^2)/N))
#
#   if(type=="Bessel"){
#     if(all.equal(Y_sd,Bessel)){
#       if(!silent){
#         cat(paste0("\nDifference adjusted-unadjusted SD:",Bessel-unadjusted,"\n"))
#       }
#     } else {
#       cat(paste0("\nWARNING: Computation of SD based on N = ",N," and Mean = ",Y_mean," differs from function sd(y)!!!\n"))
#       cat(paste0("Difference Bessel-sd(y):",Bessel-Y_sd,"\n"))
#     }
#     out <- Bessel
#   }
#
#   if(type=="unadjusted"){
#     if(all.equal(unadjusted,(Y_sd/sqrt(N/(N-1))))){
#       if(!silent){
#         cat(paste0("\nDifference Unadjusted-Adjusted:",unadjusted-Bessel,"\n"))
#       }
#     } else {
#       cat(paste0("\nWARNING: Computation of SD based on N = ",N," and Mean = ",Y_mean," differs from function sd(y)!!!\n"))
#       cat(paste0("Difference unadjusted-(Y_sd/sqrt(N/(N-1))):",unadjusted-(Y_sd/sqrt(N/(N-1))),"\n"))
#     }
#     out <- unadjusted
#   }
#   return(out)
# }
#
# # Slice columns of a matrix in epochs
# #
# # @param y A matrix with timeseries as columns
# # @param epochSz Epoch size
# #
# # @return A list with epochs
# #
# # @export
# #
# # @family Time series operations
# #
# # @author Fred Hasselman
# #
# #
# ts_slice<-function(y,epochSz=4){
#   if(!is.matrix(y)){yy <- as.matrix(y)} else {yy<-y}
#   N<-dim(yy)
#   wIndex <- plyr::llply(seq(1,N[1],epochSz),function(i) yy[i:min(i+epochSz-1,N[1]),1:N[2]])
#   delID <- which(lengths(wIndex)!=epochSz)%00%NA
#   for(del in delID){
#     if(!is.na(delID)){wIndex <- wIndex[-del]}
#   }
#   names(wIndex) <- paste("epoch",1:length(wIndex),"| size",lengths(wIndex))
#   return(wIndex)
# }
#
#
#
# # Create a timeseries profile
# #
# # @param y A 1D timeseries
# #
# # @return The profile
# # @export
# #
# #
# #
# # @examples
# # y <- runif(1000,-3,3)
# # plot(ts(y))
# # y_i <- ts_integrate(y)
# # plot(ts(y_i))
# #
# ts_integrate <-function(y){
#   #require(zoo)
#   if(!all(is.numeric(y),is.null(dim(y)))){
#     warning("Need a 1D numeric vector! Trying to convert...")
#     y <- as.numeric(as.vector(y))
#   }
#   idOK <- !is.na(y)
#   t    <- zoo::index(y)
#   t[!idOK] <- NA
#   yy  <- c(y[idOK][1],y[idOK])
#   tt  <- c(t[idOK][1],t[idOK])
#   yc  <- y
#   yc[t[idOK]] <- cumsum(yy[-length(yy)]*diff(tt))
#   # recover initial diff value
#   yc <- y[idOK][1] + yc
#   return(yc)
# }
#
#
# ts_permtest_diff <- function(y1, y2, Nperms = 99, sim = "geom", l = 3, alpha = .05, returnBootObject = FALSE){
#
#   tsSame <- function(y){
#     y
#   }
#
#   #dat <- ts(data.frame(y1 = y1, y2=y2))
#
#   ts.boot1 <- boot::tsboot(ts(y1), statistic = tsSame, R = Nperms, sim = sim, l = l)
#   ts.boot2 <- boot::tsboot(ts(y2), statistic = tsSame, R = Nperms, sim = sim, l = l)
#
#   ts.boot <- ts.boot1
#   ts.boot$t0 <- y1-y2
#   ts.boot$t <- ts.boot1$t-ts.boot2$t
#
#   out <- list()
#   for(t in 1:NROW(y1)){
#     if(ts.boot$t0[t] < 0){
#       Ds <- sum(ts.boot$t[,t] <= ts.boot$t0[t], na.rm = TRUE) # how many <= to the observed diff <= 0?
#     } else {
#       Ds <- sum(ts.boot$t[,t] >= ts.boot$t0[t], na.rm = TRUE) # how many >= to the observed diff > 0?
#     }
#     SD <- sd(c(ts.boot$t[,t],ts.boot$t0[t]), na.rm = TRUE)
#     out[[t]] <- data.frame(time = t,
#                            Ori  = ts.boot$t0[t],
#                            Dsum = Ds,
#                            p = Ds / (Nperms + 1),
#                            alpha = alpha,
#                            sig = NA,
#                            Nperms  = Nperms,
#                            Mean = mean(ts.boot$t[,t],na.rm = TRUE),
#                            SD = SD,
#                            SE = SD/sqrt(NROW(y1)+1)
#     )
#   }
#
#   df_sig     <- ldply(out)
#   df_sig$sig <- df_sig$p < alpha
#
#   if(returnBootObject){
#     return(list(df_sig = df_sig, bootOut = ts.boot))
#   } else {
#     return(df_sig = df_sig)
#   }
#
# }
#
#
#
#
# # Help lme4 get a better convergence
# # nlopt <- function(par, fn, lower, upper, control) {
# #   # Add to call: control = lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE
# #   .nloptr <<- res <- nloptr(par, fn, lb = lower, ub = upper,
# #                             opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
# #                                         maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
# #   list(par = res$solution,
# #        fval = res$objective,
# #        conv = if (res$status > 0) 0 else res$status,
# #        message = res$message
# #   )
# # }
#
# # Convert decimal point
# c2p <- function(text,N=1){
#   if(!is.character(text)){text<-as.character(text)}
#   if(sum(grepl("[,]",text))>=N){text <- gsub(",",".",text)}
#   return(text)
# }
#
# # Count missing values in x
# nmissing <- function(x){
#   sum(is.na(x))
# }
#
#
# # Wrapper for filtfilt
# #
# # @param TS A time series
# # @param f A filter
# #
# # @return A filtered signal
# # @export
# # @keywords internal
# fltrIT <- function(TS,f){
#   return(signal::filtfilt(f=f,x=TS))
# }
#
#
#
# # doChecks <- function(y,standardise=FALSE,center=FALSE,){
# #   nextPow2 <- nextn(y,factors = 2)
# #   prevPow2 <- nextn(y,factors = 2)-1
# # }
#
#
#
#
# # Elastic Scaler - A Flexible Rescale Function
# #
# # @description The 'elastic scaler'will rescale numeric vectors (1D, or columns in a matrix or data.frame) to a user defined minimum and maximum, either based on the extrema in the data, or, a minimum and maximum defined by the user.
# #
# # @param x   Input vector or data frame.
# # @param mn  Minimum value of original, defaults to `min(x, na.rm = TRUE)` if set to `NA`.
# # @param mx  Maximum value of original, defaults to `max(x, na.rm = TRUE)` if set to `NA`.
# # @param lo  Minimum value to rescale to, defaults to `0`.
# # @param hi  Maximum value to rescale to, defaults to `1`.
# # @param groupwise If `x` is a data frame with `2+` columns, `mn = NA` and/or `mx = NA` and `groupwise = TRUE`, scaling will occur
# # @param keepNA Keep `NA` values?
# # @param boundaryPrecision If set to `NA` the precision of the input will be the same as the precision of the output. This can cause problems when detecting values that lie just outside of, or, exactly on boundaries given by `lo` and `hi`, e.g. after saving the data using a default precision. Setting `boundaryPrecision` to an integer value will ensure that the boundaries of the new scale are given by `round(..., digits = boundaryPrecision)`. Alternatively one could just round all the output after rescaling to a desired precision (default = `NA`)
# # @param tol The tolerance for deciding wether a value is on the boundary `lo` or `hi` (default = `.Machine$double.eps^0.5)`)
# #
# # @details Three uses:
# # \enumerate{
# # \item elascer(x)             - Scale x to data range: min(x.out)==0;      max(x.out)==1
# # \item elascer(x,mn,mx)       - Scale x to arg. range: min(x.out)==mn==0;  max(x.out)==mx==1
# # \item elascer(x,mn,mx,lo,hi) - Scale x to arg. range: min(x.out)==mn==lo; max(x.out)==mx==hi
# # }
# #
# # @return scaled inout
# # @export
# #
# # @examples
# # # Works on numeric objects
# # somenumbers <- cbind(c(-5,100,sqrt(2)),c(exp(1),0,-pi))
# #
# # # Using the defaults:
# # # 1. mn and mx are derived globally (groupWise = FALSE)
# # # 2. values rescaled to hi and lo are integers, 0 and 1 respectively
# # elascer(somenumbers)
# #
# # # If the data contain values < mn they will return as < lo
# # elascer(somenumbers,mn=-100)
# # # If the data contain values > mx they will return > hi
# # elascer(somenumbers,mx=99)
# #
# # # Effect of setting groupWise
# # elascer(somenumbers,lo=-1,hi=1)
# # elascer(somenumbers,lo=-1,hi=1, groupwise = TRUE)
# #
# # elascer(somenumbers,mn=-10,mx=100,lo=-1,hi=4)
# # elascer(somenumbers,mn= NA,mx=100,lo=-1,hi=4, groupwise = TRUE)
# #
# # # Effect of setting boundaryPrecision
# # x <- rbind(1/3, 1/7)
# #
# # re1 <- elascer(x, lo = 0, hi = 1/13, boundaryPrecision = NA)
# # max(re1)==0.07692308 # FALSE
# # max(re1)==1/13       # TRUE
# #
# # re2 <- elascer(x, lo = 0, hi = 1/13, boundaryPrecision = 8)
# # max(re2)==0.07692308 # TRUE
# # max(re2)==1/13       # FALSE
# #
# elascer <- function(x,mn=NA,mx=NA,lo=0,hi=1,groupwise = FALSE, keepNA = TRUE, boundaryPrecision = NA, tol= .Machine$double.eps^0.5){
#
#   doGroupwise <- FALSE
#   mnNA <- FALSE
#   mxNA <- FALSE
#   UNLIST      <- FALSE
#   if(length(dim(x))<2){UNLIST <- TRUE}
#   if(any(is.na(mn),is.na(mx))){
#     if(groupwise&NCOL(x)>1){doGroupwise <- TRUE}
#     if(is.na(mn)){
#       mnNA <- TRUE
#       mn <- min(x,na.rm=TRUE)
#     }
#     if(is.na(mx)){
#       mxNA <- TRUE
#       mx <- max(x,na.rm=TRUE)
#     }
#   }
#   x <- as.data.frame(x)
#   isNA <- plyr::colwise(is.na)(x)
#   u <- x
#   for(i in 1:NCOL(x)){
#     if(doGroupwise){
#       if(mnNA){mn<-min(x[,i],na.rm=TRUE)}
#       if(mxNA){mx<-max(x[,i],na.rm=TRUE)}
#     }
#     if(mn>mx){warning("Minimum (mn) >= maximum (mx).")}
#     if(lo>hi){warning("Lowest scale value (lo) >= highest scale value (hi).")}
#     if(mn==mx){
#       u[,i]<-rep(mx,length(x[,i]))
#     } else {
#       u[,i]<-(((x[,i]-mn)*(hi-lo))/((mx-mn))+lo)
#       if(!keepNA){
#         id<-stats::complete.cases(u[,i])
#         u[!id,i]<-mn
#       }
#     }
#     if(!is.na(boundaryPrecision)){
#       # Make sure the end points of the scale are the same as passed in the argument
#       idLo <- u[,i]%[]%c(lo-tol,lo+tol)
#       u[idLo,i] <- round(u[idLo,i], digits = boundaryPrecision)
#       idHi <- u[,i]%[]%c(hi-tol,hi+tol)
#       u[idHi,i] <- round(u[idHi,i], digits = boundaryPrecision)
#     }
#   }
#   if(UNLIST){
#     u <- as.numeric(u[,1])
#   }
#   return(u)
# }
#
# # Rmd2htmlWP <- function(infile, outfile, sup = T) {
# #   require(markdown)
# #   require(knitr)
# #   mdOpt <- markdownHTMLOptions(default = T)
# #   mdOpt <- mdOpt[mdOpt != "mathjax"]
# #   mdExt <- markdownExtensions()
# #   mdExt <- mdExt[mdExt != "latex_math"]
# #   if (sup == T) {
# #     mdExt <- mdExt[mdExt != "superscript"]
# #   }
# #   knit2html(input = infile, output = outfile, options = c(mdOpt), extensions = c(mdExt))
# # }
#
#
# # [copied from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ ]
# #
# # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# # - cols:   Number of columns in layout
# # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# #
# # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# # then plot 1 will go in the upper left, 2 will go in the upper right, and
# # 3 will go all the way across the bottom.
# #
# multi_PLOT <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#
#   numPlots = length(plots)
#
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#
#   if (numPlots==1) {
#     print(plots[[1]])
#
#   } else {
#     # Set up the page
#     grid::grid.newpage()
#     grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
#
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#
#       print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
#                                             layout.pos.col = matchidx$col))
#     }
#   }
# }
#
# # TRY ... CATCH
# #
# # @details
# #  In longer simulations, aka computer experiments,
# #  you may want to
# #  1) catch all errors and warnings (and continue)
# #  2) store the error or warning messages
# #
# #  Here's a solution  (see \R-help mailing list, Dec 9, 2010):
# #
# # Catch *and* save both errors and warnings, and in the case of
# # a warning, also keep the computed result.
# #
# # @title tryCatch both warnings (with value) and errors
# # @param expr an \R expression to evaluate
# # @return a list with 'value' and 'warning', where value' may be an error caught.
# # @author Martin Maechler; Copyright (C) 2010-2012  The R Core Team
# # @export
# # @keywords internal
# try_CATCH <- function(expr){
#   W <- NULL
#   w.handler <- function(w){ # warning handler
#     W <<- w
#     invokeRestart("muffleWarning")
#   }
#   list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
#                                    warning = w.handler),
#        warning = W)
# }
#
#
# # Numeric factor to numeric vector
# #
# # Converts a factor with numeric levels to a numeric vector, using the values of the levels.
# #
# # @param x A factor based on numeric values.
# # @param sortUnique Should the unique character values be sorted? (default = `FALSE`)
# # @param keepNA Keep NA values (`TRUE`), or remove them (default = `FALSE`)
# #
# # @return A numeric vector with factor levels as names.
# # @export
# #
# # @examples
# #
# # f <- factor(round(runif(10,0,9)))
# # as.numeric_factor(f)
# #
# # # Add NAs
# # f <- factor(c(round(runif(9,0,9)),NA))
# # as.numeric_factor(f)
# # as.numeric_factor(f, keepNA = TRUE)
# #
# #
# as.numeric_factor <- function(x, keepNA = FALSE, sortUnique = FALSE){
#   idNA <- is.na(x)
#   if(!is.factor(x)){stop("Not a factor, use: `as.numeric_character()`")}
#   out <- try(as.numeric(levels(x))[x])
#   if(sum(is.na(out))>sum(idNA)){
#     stop("Factor levels are not numeric!")
#   }
#   if(!keepNA){
#     out <- out[!is.na(out)]
#   }
#   names(out) <- as.character(out)
#   return(out)
# }
#
#
#
# # Character vector to named numeric vector
# #
# # Converts a character vector to a named numeric vector, with the character elements as names.
# #
# # @param x A character vector
# # @param sortUnique Should the unique character values be sorted? (default = `FALSE`)
# # @param keepNA Keep NA values (`TRUE`), or remove them (default = `FALSE`)
# #
# # @return A named numeric vector
# # @export
# #
# # @examples
# #
# # f <- letters
# # as.numeric_character(f)
# #
# as.numeric_character <- function(x, sortUnique = FALSE, keepNA = FALSE){
#   IDna <- is.na(x)
#   xx <- as.character(x[!IDna])
#   labels.char <- unique(as.character(xx))
#   if(sortUnique){labels.char <- sort(labels.char)}
#   labels.num <- seq_along(labels.char)
#   names(x) <- x
#   xx <- x
#   plyr::laply(labels.num, function(num){
#     xx[xx%in%labels.char[num]]<<-num}
#   )
#   xx<-as.numeric(xx)
#   names(xx) <- x
#   return(xx)
# }
#
#
#
# # Discrete (factor or character) to numeric vector
# #
# # Converts a factor with numeric levels, or, a character vector with numeric values to a numeric vector using [as.numeric_factor], or, [as.numeric_character] respectively. If an unnamed numeric vector is passed, it will be returned as a named numeric vector.
# #
# # @param x A factor with levels that are numeric, or, a character vector representing numbers.
# # @param keepNA Keep NA values (`TRUE`), or remove them (default = `FALSE`)
# # @param sortUnique Should the unique character/factor level values be sorted? (default = `FALSE`)
# #
# # @return A numeric vector with factor levels / numeric character values as names.
# # @export
# #
# # @examples
# #
# # # Continuous
# # i <- runif(10,0,9)
# # as.numeric_discrete(i)
# #
# # # Integer
# # as.numeric_discrete(round(i))
# #
# # # Factor with NAs
# # f <- factor(c(round(runif(9,0,9)),NA))
# # as.numeric_discrete(f)
# # as.numeric_discrete(f, keepNA = FALSE)
# #
# # # Character vector
# # c <- c("Thank","you", "for", "the flowers")
# # as.numeric_discrete(c)
# # as.numeric_discrete(c, sortUnique = TRUE)
# #
# # c <- c("Thank","you", "for", "the", "flowers")
# # as.numeric_discrete(c)
# # as.numeric_discrete(c, sortUnique = TRUE)
# #
# #
# as.numeric_discrete <- function(x, keepNA = FALSE, sortUnique = FALSE){
#
#   if(plyr::is.discrete(x)){
#     if(is.factor(x)){
#       if(suppressWarnings(all(is.na(as.numeric(levels(x)))))){
#         x <- as.character(x)
#       } else {
#         y <- as.numeric_factor(x, keepNA = keepNA, sortUnique = sortUnique)
#       }
#     }
#     if(is.character(x)){
#       y <- as.numeric_character(x, keepNA = keepNA, sortUnique = sortUnique)
#     }
#   } else {
#     if(is.numeric(x)){
#       if(!all(is.wholenumber(x))){
#         y <- ts_discrete(x)
#         if(is.null(names(y))){
#           names(y) <- paste(signif(x,4))
#         }
#       } else { # wholenumber
#         y <- x
#         if(is.null(names(y))){
#           names(y) <- paste(x)
#         }
#       }
#     } else { # numeric
#       stop("Variable is not a factor, character vector, or, unnamed numeric vector.")
#     }
#   } # discrete
#   return(y)
# }
#
# #
# # # Discrete (factor or character) to numeric vector
# # #
# # # Converts a factor with numeric levels, or, a character vector with numeric values to a named numeric vector (using [as.numeric_factor], or, [as.numeric_character]). If the character values or factor levels are non-numeric (or mixed), a named numeric vector will be returned, values represent an unordered categorical variable (nominal), the character values are used as names. If an unnamed numeric vector is passed, it will be returned as a named numeric vector, with the values copied as names. If a continuous numeric vector is passed, a named numeric vector of bins will be returned (using [ts_discrete]), with original vaslues as names.
# # #
# # # @param x A factor, a character, or, numeric vector
# # # @param keepNA Keep NA values (`TRUE`), or remove them (default = `FALSE`)
# # # @param sortUnique In case of character values/character factor levels, should the unique values be sorted before they are assigned a number? (default = `FALSE`)
# # # @param nbins Number of bins to use if the vector is continuous. See [ts_discrete] (default = `ceiling(2*NROW(x)^(1/3))`)
# # #
# # # @return A numeric vector with factor levels / numeric character values as names.
# # # @export
# # #
# # # @examples
# # #
# # # f <- factor(round(runif(10,0,9)))
# # # as.numeric_factor(f)
# # #
# # # # Add NAs
# # # f <- factor(c(round(runif(9,0,9)),NA))
# # # as.numeric_factor(f)
# # # as.numeric_factor(f, keepNA = TRUE)
# # #
# # #
# # #
# # as.numeric_discrete <- function(x, keepNA = FALSE, sortUnique = FALSE, nbins = ceiling(2*NROW(x)^(1/3))){
# #
# #   NAs <- sum(is.na(x%00%NA))
# #   if(!keepNA&NAs>0){
# #     x <- x[!is.na(x)]
# #   }
# #
# #   if(plyr::is.discrete(x)){
# #     x[which(sapply(as.character(x), nchar)==0)] <- NA_character_
# #     if(is.factor(x)){
# #       if(suppressWarnings(sum(is.na(as.numeric(levels(x))))==NAs)){
# #         y <- as.numeric_factor(x, keepNA = keepNA)
# #        #all(is.na(as.numeric(levels(x)))))){
# #       } else {
# #         x <- as.character(x)
# #       }
# #       }
# #     if(is.character(x)){
# #       if(suppressWarnings(sum(is.na(as.numeric(x)))==NAs)){
# #         x <- as.numeric(x)
# #       } else {
# #         if(!sortUnique){
# #           warning("sortUnique was set to TRUE. Vector contains a mix of letters and numbers.")
# #           sortUnique <- TRUE
# #         }
# #     }
# #       y <- as.numeric_character(x, keepNA = keepNA, sortUnique = sortUnique)
# #     }
# #   }
# #
# #   if(is.numeric(x)){
# #     if(all(invctr::is.wholenumber(x))){
# #       y <- x #as.factor(factor(x, ordered = sortUnique))
# #     } else {
# #       y <- ts_discrete(x, keepNA = keepNA, nbins = nbins)
# #     }
# #     if(is.null(names(y))){
# #       names(y) <- paste(signif(x,3))
# #     }
# #   }
# #
# #   return(y)
# # }
# #
#
#
# # Repeat Copies of a Matrix
# #
# # @param X A matrix
# # @param m Multiply dim(X)[1] m times
# # @param n Multiply dim(X)[2] n times
# #
# # @return A repeated matrix
# # @export
# #
# repmat <- function(X,m,n){
#   #  REPMAT R equivalent of repmat (matlab)
#   #  FORMAT
#   #  DESC
#   #  description not available.
#
#   if (is.matrix(X))
#   {
#     mx = dim(X)[1]
#     nx = dim(X)[2]
#     out <- matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
#   }
#   else if (is.vector(X)) {
#     mx = 1
#     nx = length(X)
#     out <- matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
#   }
#   else if (length(X) == 1)
#   {
#     out <- matrix(X,m, n)
#   }
#   return (out)
# }
