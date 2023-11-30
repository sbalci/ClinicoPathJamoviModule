














# This file is a generated template, your changes will not be overwritten

TestROCClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "TestROCClass",
    inherit = TestROCBase,
    private = list(
      .init = function() {
        # if (is.null(self$options$classVar) ||
        #   is.null(self$options$dependentVars)) {
        #     self$results$resultsTable$setVisible(visible = FALSE)
        # }
      },
      .run = function() {
        if (is.null(self$options$classVar) ||
            is.null(self$options$dependentVars)) {
          self$results$instructions$setContent(
            "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p><b>This analysis is still in development: The format of results may change in future releases. Please report any errors or requests <a href='https://github.com/lucasjfriesen/jamoviPsychoPDA/issues' target = '_blank'>here</a></b></p>
            <p>Welcome to PsychoPDA's Test ROC analysis To get started:</p>
            <ol>
            <li>Place the responses in the 'Dependent Variable' slot<br /><br /></li>
            <li>Place the classification in the 'Class Variable' slot<br /><br /></li>
            <li>[<em>Optional</em>] Place a grouping variable in the 'Grouping Variable' slot<br /><br /></li>
            </ol>
            <p>If you encounter any errors, or have questions, please see the <a href='https://lucasjfriesen.github.io/jamoviPsychoPDA_docs/measureDiagnostics_testROC_gettingStarted.html' target = '_blank'>documentation.</a></p>
            <p>If this software is used in conducting published research, please do provide a citation using the information at the bottom of the analysis.</p>
            </div>
            </body>
            </html>"
          )
          return()
        } else {
          self$results$instructions$setVisible(visible = FALSE)
          procedureNotes <- paste0(
            "<html>
            <body>
            <p>Procedure Notes</p>
            <hr>",
            "<p> The TestROC optimal cutpoint analysis has been completed using the following specifications: ",
            "<p>&nbsp;</p>",
            "<p> Measure Variable(s): ",
            paste(unlist(self$options$dependentVars), collapse = ", "),
            "</p>",
            "<p> Class Variable: ",
            self$options$classVar,
            "</p>"
          )
          if (self$options$positiveClass == "") {
            procedureNotes <- paste0(
              procedureNotes,
              "<p> Positive Class: ", self$data[,self$options$classVar][1], "</p>")
          } else {
            procedureNotes <- paste0(procedureNotes,
                                     "<p> Positive Class: ",
                                     self$options$positiveClass,
                                     "</p>")
          }
          
          # Was there subgrouping?
          if (!is.null(self$options$subGroup)) {
            procedureNotes <- paste0(procedureNotes,
                                     "<p> Sub-Group Variable: ",
                                     self$options$subGroup,
                                     "</p>")
          }
          procedureNotes <- paste0(
            procedureNotes,
            "<p>&nbsp;</p>",
            "<p> Method: ",
            self$options$method,
            "</p>",
            "<p> All Observed Cutpoints: ",
            self$options$allObserved,
            "</p>",
            "<p> Metric: ",
            self$options$metric,
            "</p>",
            "<p> Direction (relative to cutpoint): ",
            self$options$direction,
            "</p>",
            "<p> Tie Breakers: ",
            self$options$break_ties,
            "</p>",
            "<p>  Metric Tolerance: ",
            self$options$tol_metric,
            "</p>",
            "<p>&nbsp;</p>"
          )
          # If bootstrapping happened
          if (self$options$boot_runs != 0) {
            procedureNotes <- paste0(procedureNotes,
                                     "<p> Bootstrap Runs: ",
                                     self$options$boot_runs,
                                     "</p>")
          }
          # Close the notes
          procedureNotes <- paste0(
            procedureNotes,
            "<hr />
            <p>For more information on how calculations are performed and interpretting results, please see the <a href='https://lucasjfriesen.github.io/jamoviPsychoPDA_docs/measureDiagnostics_testROC_gettingStarted.html' target = '_blank'>documentation</a></p>
            </body>
            </html>"
          )
          self$results$procedureNotes$setContent(procedureNotes)
        }
        
        # Var handling ----
        data = self$data
        
        if (self$options$method == "oc_manual") {
          method = "cutpointr::oc_manual"
          if (self$options$specifyCutScore == "") {
            stop("Please specify a cut score for using this method.")
          } else {
            score = as.numeric(self$options$specifyCutScore)
          }
        } else {
          method = paste0("cutpointr::", self$options$method)
          score = NULL
        }
        
        if (method %in% c(
          "cutpointr::maximize_metric",
          "cutpointr::minimize_metric",
          "cutpointr::maximize_loess_metric",
          "cutpointr::minimize_loess_metric",
          "cutpointr::maximize_spline_metric",
          "cutpointr::minimize_spline_metric"
        )) {
          tol_metric = self$options$tol_metric
        } else {
          tol_metric = NULL
        }
        
        method = eval(parse(text = method))
        
        metric = paste0("cutpointr::", self$options$metric)
        
        metric = eval(parse(text = metric))
        
        direction = self$options$direction
        boot_runs = self$options$boot_runs
        # use_midpoints = self$options$use_midpoint
        break_ties = self$options$break_ties
        break_ties = eval(parse(text = break_ties))

        boot_runs = self$options$boot_runs
        plotDataList <- data.frame(
                var = list(),
                cutpoint = list(),
                sensitivity = list(),
                specificity = list(),
                ppv = list(),
                npv = list(),
                AUC = list(),
                youden = list(),
                stringsAsFactors = FALSE)
        # Data ----
        
        vars <- self$options$dependentVars
        
        if (!is.null(self$options$subGroup)) {
          subGroup = data[, self$options$subGroup]
          classVar = data[, self$options$classVar]
          uniqueGroups <- unique(subGroup)
          vars <-
            apply(expand.grid(vars, uniqueGroups), 1, paste, collapse = "_")
        } else {
          subGroup = NULL
        }
        
        aucList = list()
        
        for (var in vars) {
          if (!var %in% self$results$resultsTable$itemKeys) {
            self$results$sensSpecTable$addItem(key = var)
            self$results$resultsTable$addItem(key = var)
            if (self$options$combinePlots == FALSE) {
              self$results$plotROC$addItem(key = var)
            }
          }
          
          if (is.null(subGroup)) {
            dependentVar = as.numeric(data[, var])
            classVar = data[, self$options$classVar]
          } else {
            dependentVar = as.numeric(data[subGroup == strsplit(var, split = "_")[[1]][2],
                                           names(data) == strsplit(var, split = "_")[[1]][1]])
            classVar = data[subGroup == strsplit(var, split = "_")[[1]][2],
                                       self$options$classVar]
          }
          
          if (self$options$positiveClass == ""){
            # self$results$debug$setContent(classVar[1])
            pos_class <- classVar[1]
          } else {
            pos_class <- self$options$positiveClass
          }
          # Caclulations ----
          results = cutpointr::cutpointr(
            x = dependentVar,
            class = classVar,
            subgroup = NULL,
            method = method,
            cutpoint = score,
            metric = metric,
            direction = direction,
            pos_class = pos_class,
            # use_midpoints = use_midpoints,
            tol_metric = tol_metric,
            boot_runs = boot_runs,
            break_ties = break_ties,
            na.rm = TRUE
          )
          
          # self$results$debug$setContent(results)
          
          if (!self$options$allObserved) {
            resultsToDisplay <- sort(unlist(results$optimal_cutpoint))
          } else {
            resultsToDisplay <- sort(unlist(unique(dependentVar)))
          }
          # Confusion matrix ----
          
          confusionMatrix <-
            confusionMatrixForTable <- results$roc_curve[[1]]
          
          if (!self$options$allObserved) {
            confusionMatrixForTable = confusionMatrixForTable[confusionMatrixForTable$x.sorted %in% resultsToDisplay,]
          }
          if (self$options$sensSpecTable) {
            self$results$sensSpecTable$setVisible(TRUE)
            sensSpecRes <-
              print.sensSpecTable(
                Title = paste0(
                  "Scale: ",
                  var,
                  " | Score: ",
                  confusionMatrixForTable$x.sorted
                ),
                TP = confusionMatrixForTable$tp,
                FP = confusionMatrixForTable$fp,
                TN = confusionMatrixForTable$tn,
                FN = confusionMatrixForTable$fn
              )
            sensTable <- self$results$sensSpecTable$get(key = var)
            sensTable$setContent(sensSpecRes)
            sensTable$setVisible(TRUE)
          }
          
          # Results columns ----
          
          sensList <-
            (
              cutpointr::sensitivity(
                tp = confusionMatrix$tp,
                fp = confusionMatrix$fp,
                tn = confusionMatrix$tn,
                fn = confusionMatrix$fn
              ) * 100
            )
          
          specList <-
            (
              cutpointr::specificity(
                tp = confusionMatrix$tp,
                fp = confusionMatrix$fp,
                tn = confusionMatrix$tn,
                fn = confusionMatrix$fn
              ) * 100
            )
          
          ppvList <- (
            cutpointr::ppv(
              tp = confusionMatrix$tp,
              fp = confusionMatrix$fp,
              tn = confusionMatrix$tn,
              fn = confusionMatrix$fn
            ) * 100
          )
          
          npvList <- (
            cutpointr::npv(
              tp = confusionMatrix$tp,
              fp = confusionMatrix$fp,
              tn = confusionMatrix$tn,
              fn = confusionMatrix$fn
            ) * 100
          )
          
          youdenList <-
            cutpointr::youden(
              tp = confusionMatrix$tp,
              fp = confusionMatrix$fp,
              tn = confusionMatrix$tn,
              fn = confusionMatrix$fn
            )
          
          metricList <-
            metric(
              tp = confusionMatrix$tp,
              fp = confusionMatrix$fp,
              tn = confusionMatrix$tn,
              fn = confusionMatrix$fn
            )
          
          resultsToReturn <- data.frame(
            #scaleName = rep(var, times = length(sensList)),
            cutpoint = as.character(confusionMatrix$x.sorted),
            sensitivity = formatter(sensList),
            specificity = formatter(specList),
            ppv = formatter(ppvList),
            npv = formatter(npvList),
            AUC = results$AUC,
            youden = youdenList,
            metricValue = unname(metricList),
            stringsAsFactors = FALSE # FUCK
          )
          
          aucList[[var]] = results$AUC
          
          # State Savers ----
          # Results table ----
          resultState <- self$results$resultsTable$state
          if (!is.null(resultState)) {
            # ... populate the table from the state
          } else {
            # ... create the table and the state
            table <- self$results$resultsTable$get(key = var)
            for (row in resultsToDisplay) {
              table$setTitle(paste0("Scale: ", var))
              table$addRow(rowKey = row, value = resultsToReturn[resultsToReturn$cutpoint == row,])
            }
            self$results$resultsTable$setState(resultState)
          }
          
          # Plotting Data ----
          if (self$options$plotROC == TRUE &
              self$options$combinePlots == FALSE) {
            image <- self$results$plotROC$get(key = var)
            image$setTitle(paste0("ROC Curve: ", var))
            image$setState(
              data.frame(
                var = var,
                cutpoint = confusionMatrix$x.sorted,
                sensitivity = sensList,
                specificity = specList,
                ppv = ppvList,
                npv = npvList,
                AUC = results$AUC,
                youden = youdenList,
                stringsAsFactors = FALSE # FUCK
              )
            )
          } else {
            plotDataList <- rbind(
              plotDataList,
              data.frame(
                var = var,
                cutpoint = confusionMatrix$x.sorted,
                sensitivity = sensList,
                specificity = specList,
                ppv = ppvList,
                npv = npvList,
                AUC = results$AUC,
                youden = youdenList,
                stringsAsFactors = FALSE
              )
            )}}
        
          if (self$options$plotROC == TRUE &
              self$options$combinePlots == TRUE){
            self$results$plotROC$addItem(key = 1)
            image <- self$results$plotROC$get(key = 1)
            image$setTitle(paste0("ROC Curve: Combined"))
            
            image$setState(plotDataList)
            # 
          }
          
          # DeLong Test ----
          # self$results$debug$setContent(classVar)
          if (self$options$delongTest == TRUE) {
            if (length(self$options$dependentVars) < 2){
              stop("Please specify at least two dependent variables to use DeLong's test.")
            }
            if (!is.null(self$options$subGroup)) {
              stop(
                "DeLong's test does not currently support the grouping variable. If you would like to contribute/provide guidance, please use the contact information provided in the documentation."
              )
            } else {
              delongResults <- deLong.test(
                data = data.frame(lapply(data[, vars], as.numeric)),
                classVar = as.character(classVar),
                ref = NULL,
                pos_class = pos_class,
                conf.level = 0.95
              )
            }
            self$results$delongTest$setVisible(visible = TRUE)
            self$results$delongTest$setContent(paste0(capture.output(print.DeLong(
              delongResults
            ))))
          }
          
          
        },
        .plotROC = function(image, ggtheme, theme, ...) {

          plotData <- data.frame(image$state)
          
          if (self$options$combinePlots == TRUE) {
            plot <-
              ggplot2::ggplot(plotData,
                              ggplot2::aes(
                                x = 1 - specificity,
                                y = sensitivity,
                                colour = var
                              ))
          } else {
            plot <-
              ggplot2::ggplot(plotData, ggplot2::aes(x = 1 - specificity, y = sensitivity))
          }
          # self$results$debug$setContent(plotData)
          
          plot <- plot +
            ggplot2::geom_point() +
            ggplot2::geom_line() +
            ggplot2::xlab("1 - Specificity") +
            ggplot2::ylab("Sensitivity") +
            ggtheme + ggplot2::theme(plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
          if (self$options$smoothing) {
            if (self$options$displaySE) {
              plot = plot +
                ggplot2::geom_smooth(se = TRUE)
            } else {
              plot = plot +
                ggplot2::geom_smooth(se = FALSE)
            }
          }
          print(plot)
          TRUE
        }
          )
          )
