#' @title Variable Tree
#'
#' @importFrom R6 R6Class
#' @import jmvcore

vartreeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "vartreeClass",
    inherit = vartreeBase,
    private = list(


        # # init ----
        #
        # .init = function() {
        #
        #     self$results$text1$setSize(600, 900)
        #
        # }
        # ,


        # labelData ----

        .labelData = function() {


            # Prepare data for analysis
            mydata <- self$data

            ## Get rownames to data
            mydata$rownames <- rownames(mydata)

            ## Correct variable names and labels
            # Get original variable names
            original_names <- names(mydata)

            # Save original names as a named vector where the names are the original names,
            # and the values are the labels you want to set, which are also the original names.
            labels <- setNames(original_names, original_names)

            # Clean variable names
            mydata <- mydata %>% janitor::clean_names()

            # Now apply the labels to the cleaned names.
            # Since the variable names have been cleaned, you must match the labels to the cleaned names.
            # The labels vector should have names that are the cleaned names and values that are the original names.
            corrected_labels <-
                setNames(original_names, names(mydata))

            # Apply the corrected labels
            mydata <- labelled::set_variable_labels(.data = mydata,
                                                    .labels = corrected_labels)

            # Retrieve all variable labels
            all_labels <- labelled::var_label(mydata)

            # # Retrieve the variable name from the label
            # # Tek değişken için
            # dependent_variable_name_from_label <-
            #     names(all_labels)[all_labels == self$options$outcome]
            #
            # # Retrieve the variable names vector from the label vector
            # # Birden fazla değişkenler için
            # labels <- self$options$explanatory
            #
            # explanatory_variable_names <-
            #     names(all_labels)[match(labels, all_labels)]


            myvars <-  self$options$vars
            myvars <-
                names(all_labels)[match(myvars, all_labels)]


            percvar <-
                names(all_labels)[all_labels == self$options$percvar]

            summaryvar <-
                names(all_labels)[all_labels == self$options$summaryvar]

            follow <-
                names(all_labels)[all_labels == self$options$follow]

            prunebelow <-
                names(all_labels)[all_labels == self$options$prunebelow]



            return(list(
                "mydata" = mydata
                , "myvars" = myvars
                , "percvar" = percvar
                , "summaryvar" = summaryvar
                , "follow" = follow
                , "prunebelow" = prunebelow

                ))


        }
        ,



        .run = function() {


            if ( is.null(self$options$vars) ) {
                # ToDo Message ----
                todo <- "
                <br>Welcome to ClinicoPath Descriptives Module
                          <br><br>
                          This tool will help you form a Variable Tree.
                          "
                html <- self$results$todo
                html$setContent(todo)
                return()

            } else {
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)

            }

            # Error Message ----

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

            # Read Labelled Data ----

            cleaneddata <- private$.labelData()

            mydata <- cleaneddata$mydata
            myvars <- cleaneddata$myvars
            percvar <- cleaneddata$percvar
            summaryvar <- cleaneddata$summaryvar

            # self$results$r_cleaneddata$setContent(cleaneddata)


            # formulaDependent <- jmvcore::constructFormula(terms = dependent_variable_name_from_label)

            # formulaExplanatory <- jmvcore::composeTerms(listOfComponents = explanatory_variable_names)



            # Read Labelled Variables ----


            # myvars <-  self$options$vars
            # percvar <- self$options$percvar
            # summaryvar <- self$options$summaryvar



            # Read Arguments ----

            horizontal <- self$options$horizontal
            sline <- self$options$sline
            mytitle <- self$options$mytitle


            # Default Arguments ----

            # prunesmaller ----
            xprunesmaller <- NULL
            useprunesmaller <- self$options$useprunesmaller
            if (useprunesmaller) {
                xprunesmaller <- self$options$prunesmaller
            }


            xsplitspaces  <-  TRUE
            xprune <- list()
            xprunebelow <- list()
            xkeep <- list()
            xfollow <- list()
            xlabelnode <- list()
            xtlabelnode <- NULL
            xlabelvar <- NULL
            xvarminwidth <- NULL
            xvarminheight <- NULL
            xvarlabelloc <- NULL
            xfillcolor <- "white"
            xfillcolor <- NULL
            xfillnodes <- TRUE
            xNAfillcolor <- "white"
            xrootfillcolor <- "#EFF3FF"
            xpalette <- NULL
            xgradient <- TRUE
            xrevgradient <- FALSE
            xsinglecolor <- 2
            xcolorvarlabels <- TRUE
            xtitle <- ""
            xsameline <- FALSE
            xcheck.is.na <- FALSE

            xptable <- FALSE
            xshowroot <- TRUE
            xtext <- list()
            xttext <- list()
            xplain <- FALSE
            xsqueeze <- 1
            xshowvarinnode <- FALSE
            xshowvarnames <- TRUE
            xshowpct <- TRUE
            xshowlpct <- TRUE
            xshowcount <- TRUE
            xshowlegend <- FALSE
            xvarnamepointsize <- 18
            xHTMLtext <- FALSE
            xdigits <- 0
            xcdigits <- 1
            xsplitwidth <- 20
            xlsplitwidth <- 15
            # vsplitwidth in 5.0.0
            xgetscript <- FALSE
            xnodesep <- 0.5
            xranksep <- 0.5
            xmargin <- 0.2
            xhoriz <- TRUE
            xsummary <- ""
            xrunsummary <- NULL
            xretain <- NULL
            xgraphattr <- ""
            xnodeattr <- ""
            xedgeattr <- ""
            xcolor <- c("blue", "forestgreen", "red", "orange", "pink")
            xcolornodes <- FALSE
            xmincount <- 1
            xmaxcount <- NULL
            xshowempty <- FALSE
            xrounded <- TRUE
            xnodefunc <- NULL
            xnodeargs <- NULL
            xchoicechecklist <- TRUE
            xarrowhead <- "normal"
            xfolder <- NULL
            xpngknit <- TRUE
            xas.if.knit <- FALSE
            xmaxNodes <- 1000
            xparent <- 1
            xlast <- 1
            xroot <- TRUE


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}

            # Prepare Data ----

            mydata <- jmvcore::select(df = mydata, columnNames = c(myvars, percvar, summaryvar))

            # Prepare Formula ----

            # formula <- jmvcore::constructFormula(terms = self$options$vars)

            # myvars1 <- jmvcore::decomposeFormula(formula = formula)

            # myvars1 <- unlist(myvars1)

            # myvars1 <- paste0(myvars1, collapse = " ")

            myvars1 <- myvars


                # myvars2 <- self$options$vars
                # myvars2 <- unlist(myvars2)
                #
                # myvars2 <- paste0(myvars2, collapse = " ")

            # Percentage Variable ----
            if ( !is.null(self$options$percvar) ) {
                # percvar <- self$options$percvar
                xsummary <- paste0(percvar,"=", self$options$percvarLevel
                                   #, "\n%pct%"
                                   )

                # summary=c("Score \nScore: mean (SD) %meanx% (%SD%)","Pre \nPre: range %range%"))



            }


            # Continuous Variable for Summaries ----

            if ( !is.null(self$options$summaryvar) ) {
                # summaryvar <- self$options$summaryvar

                summarylocation <- self$options$summarylocation

                if (summarylocation == "leafonly") {
                    summarylocation1 <- "%leafonly%"
                } else if (summarylocation == "allnodes") {
                    summarylocation1 <- "%allnodes%"
                }

                xsummary <- paste0(
                    summaryvar," \n\n",
                    summaryvar, "\n",
                    "mean=%mean%", "\n",
                    "SD=%SD%", "\n",
                    # "Range=%range%", "\n",
                    # "mv=%mv%",
                    summarylocation1, "\n"
                    )
            }



            # Prune Below ----

            if ( !is.null(self$options$prunebelow) ) {

                # prunebelow <- self$options$prunebelow

                prunebelow <- cleaneddata$prunebelow


                prunebelow <- jmvcore::composeTerm(prunebelow)

                pruneLevel1 <- self$options$pruneLevel1
                pruneLevel1 <- jmvcore::composeTerm(pruneLevel1)

                pruneLevel2 <- self$options$pruneLevel2
                pruneLevel2 <- jmvcore::composeTerm(pruneLevel2)

                xprunebelow <-  paste0("list(", prunebelow,"=c('", pruneLevel1, "','", pruneLevel2,"'))")

                }


            # Follow Below ----

            if ( !is.null(self$options$follow) ) {

                follow <- cleaneddata$follow

                # follow <- self$options$follow
                follow <- jmvcore::composeTerm(follow)

                followLevel1 <- self$options$followLevel1
                followLevel1 <- jmvcore::composeTerm(followLevel1)

                followLevel2 <- self$options$followLevel2
                followLevel2 <- jmvcore::composeTerm(followLevel2)

                xfollow <-  paste0("list(", follow,"=c('", followLevel1, "','", followLevel2,"'))")

            }


            # run vtree function ----

            results <- vtree::vtree(
                z = mydata,
                vars = myvars1,
                sameline = sline,
                title = mytitle,
                horiz = horizontal,
                showvarnames = self$options$varnames,
                showlegend = self$options$legend,
                showpct = self$options$pct,
                splitspaces = xsplitspaces,
                # prune = list(),
                prunebelow = eval(parse(text = xprunebelow)),
                # keep = list(),
                follow = eval(parse(text = xfollow)),
                prunesmaller = xprunesmaller,
                # labelnode = list(),
                # tlabelnode = NULL,
                # labelvar = NULL,
                # varminwidth = NULL,
                # varminheight = NULL,
                # varlabelloc = NULL,
                # fillcolor = "white",
                # fillcolor = NULL,
                # fillnodes = TRUE,
                # NAfillcolor = "white",
                # rootfillcolor = "#EFF3FF",
                # palette = NULL,
                # gradient = TRUE,
                # revgradient = FALSE,
                # singlecolor = 2,
                # colorvarlabels = TRUE,
                # title = "",
                # sameline = FALSE,
                # Venn = self$options$venntable,
                # check.is.na = FALSE,
                seq = self$options$sequence,
                pattern = self$options$pattern,
                ptable = self$options$ptable,
                # showroot = TRUE,
                # text = list(),
                # ttext = list(),
                # plain = FALSE,
                # squeeze = 1,
                # showvarinnode = FALSE,
                shownodelabels = self$options$nodelabel,
                # showvarnames = TRUE,
                # showpct = TRUE,
                # showlpct = TRUE,
                showcount = self$options$showcount,
                # showlegend = FALSE,
                varnamepointsize = 18,
                # HTMLtext = FALSE,
                # digits = 0,
                # cdigits = 1,
                # splitwidth = 20,
                # lsplitwidth = 15,
                # getscript = FALSE,
                # nodesep = 0.5,
                # ranksep = 0.5,
                # margin = 0.2,
                vp = self$options$vp,
                # horiz = TRUE,
                summary = xsummary,
                # runsummary = NULL,
                # retain = NULL,
                # imagewidth = self$options$width,
                # imageheight = self$options$height,
                # graphattr = "",
                # nodeattr = "",
                # edgeattr = "",
                # color = c("blue", "forestgreen", "red", "orange", "pink"),
                # colornodes = FALSE,
                # mincount = 1,
                # maxcount,
                # showempty = FALSE,
                # rounded = TRUE,
                # nodefunc = NULL,
                # nodeargs = NULL,
                # choicechecklist = TRUE,
                # arrowhead = "normal",
                # folder,
                # pngknit = TRUE,
                # as.if.knit = FALSE,
                # maxNodes = 1000,
                # parent = 1,
                # last = 1,
                root = xroot
            )


            # export as svg ----
            # results1 <- DiagrammeRsvg::export_svg(gv = results)
            # self$results$text1$setContent(print(results1))




            # export as svg ----
            results1 <- DiagrammeRsvg::export_svg(gv = results)
            self$results$text1$setContent(print(results1))

            results1 <- base::sub('width=\"[[:digit:]pt\"]+',
                                  ifelse(horizontal==TRUE, 'width=400pt ', 'width=1000pt '),
                                  results1)
            # results1 <- base::sub('scale[([:digit:] [:digit:])]+',
            #                       'scale(1, 1)',
            #                       results1)

            results1 <- paste0('<html><head><style>
                               #myDIV {width: 610px; height: 850px; overflow: auto;}
                               </style></head><body><div id="myDIV">',
                               results1,
                               '</div></script></body></html>')



            self$results$text1$setContent(results1)








            # ptable ----
            if (self$options$ptable) {
                self$results$text2$setContent(results)
            }


            # # venntable ----
            # if (self$options$ptable && self$options$venntable) {
            #     results2 <- print(vtree::VennTable(results), quote = FALSE)
            #     self$results$text3$setContent(results2)
            # }


        }

        )
)




WebPage <- R6::R6Class("WebPage",
                   public = list(
                       name = character(0),
                       head = c("<!DOCTYPE html>","<html>","<head>"),
                       body = "<body>",
                       style = '<style type="text/css">',
                       add_style = function(identifier, content){
                           content <- purrr::imap_chr(content, ~ glue::glue("{.y} : {.x};")) %>%
                               unname() %>%
                               paste(collapse = " ")
                           glued <- glue::glue("%identifier% { %content% }",
                                               .open = "%", .close = "%")
                           self$style <- c(self$style, glued)
                       },
                       initialize = function(name){
                           self$name <- name
                       },
                       add_tag = function(tag, content){
                           glued <- glue::glue("<{tag}>{content}</{tag}>")
                           self$body <- c(self$body, glued)
                       },
                       save = function(path){
                           write(private$concat(self$head, self$style, self$body),
                                 glue::glue("{file.path(path, self$name)}.html"))
                       },
                       view = function(){
                           htmltools::html_print(private$concat(self$head, self$style, self$body))
                       },
                       print = function(){
                           cat(private$concat(self$head, self$style, self$body), sep = "\n")
                       }
                   ),
                   private = list(
                       concat = function(head, style, body){
                           c(head, style, "</style>", body,"</body>","</html>")
                       }
                   )
)
