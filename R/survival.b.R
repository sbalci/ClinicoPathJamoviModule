#' Survival Analysis
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import finalfit
#' @import survival
#' @import survminer
#'

survivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "survivalClass",
    inherit = survivalBase,
    private = list(
        .run = function() {


            # If no variable selected Initial Message ----

            if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$overalltime) ) {

                todo <- glue::glue("
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you calculate median survivals and 1,3,5-yr survivals for a given fisk factor.
                <br><br>
                Explanatory variable should be categorical (ordinal or nominal).
                <br><br>
                Outcome variable should be coded binary (0 or 1).
                <br><br>
                If patient is dead or event (recurrence) occured it is 1.
                <br><br>
                If censored (patient is alive or free of disease) at the last visit it is 0.
                <br><br>
                Survival should be numeric, continuous, and in months.
                <br><br>
                This function uses survival, survminer, and finalfit packages. Please cite jamovi and the packages as given below.
                                   "
                )

                html <- self$results$todo
                html$setContent(todo)
                return()

            } else {

                # Empty message when all variables selected

                todo <- ""
                html <- self$results$todo
                html$setContent(todo)


                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')




                # Check if outcome variable is suitable or stop ----


                myoutcome2 <- self$options$outcome

                myoutcome2 <- self$data[[myoutcome2]]

                myoutcome2 <- na.omit(myoutcome2)



                # if ( !is.numeric(myoutcome2) || any(myoutcome2 != 0 & myoutcome2 != 1))
                if (any(myoutcome2 != 0 & myoutcome2 != 1))
                    stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')




                # self$results$deneme$setContent(head(mydata))
                #
                # self$results$deneme2$setContent(head(mydata))



                # Read Data ----

                # mydata <- self$data

                uoveralltime <- self$options$overalltime

                uoveralltime <- jmvcore::toNumeric(self$data[[uoveralltime]])

                uthefactor <- self$options$explanatory

                uthefactor <- self$data[[uthefactor]]

                uoutcome <- self$options$outcome

                uoutcome <- jmvcore::toNumeric(self$data[[uoutcome]])



                # # myoutcomelevel <- self$options$outcomeLevel
                # myoutcome <- ifelse(self$data[[myoutcome]] == self$options$outcomeLevel, 1, 0)



                mydata <- data.frame(myoveralltime = uoveralltime,
                                     thefactor = uthefactor,
                                     myoutcome = uoutcome)

                mydata <- na.omit(mydata)


                # # Run code for analysis ----
                # self$results$deneme3$setContent(mydata[[myoutcome]])
                # self$results$deneme4$setContent(mydata)


                # results 1, Median Survival Table ----

                km_fit <- survival::survfit(survival::Surv(myoveralltime, myoutcome) ~ thefactor, data = mydata)

                km_fit_median_df <- summary(km_fit)
                results1html <- as.data.frame(km_fit_median_df$table) %>%
                    janitor::clean_names(dat = ., case = "snake") %>%
                    tibble::rownames_to_column(.data = ., var = self$options$explanatory)

                results1html[,1] <- gsub(pattern = "thefactor=",
                                         replacement = "",
                                         x = results1html[,1])

                # results 1 html, Median Survival Table Html Type ----

                results1html <- knitr::kable(results1html,
                                             row.names = FALSE,
                                             align = c('l', rep('r', 9)),
                                             format = "html",
                                             digits = 1)

                # results 2 median survival summary ----

                km_fit_median_df <- summary(km_fit)
                km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>%
                    janitor::clean_names(dat = ., case = "snake") %>%
                    tibble::rownames_to_column(.data = ., var = self$options$explanatory)

                km_fit_median_df %>%
                    dplyr::mutate(
                        description =
                            glue::glue(
                                "When ", self$options$explanatory, "{.data[[self$options$explanatory]]}, median survival is {median} [{x0_95lcl} - {x0_95ucl}, 95% CI] months."
                            )
                    ) %>%
                    dplyr::mutate(
                        description = gsub(pattern = "thefactor=", replacement = " is ", x = description)
                    ) %>%
                    dplyr::select(description) %>%
                    dplyr::pull() -> km_fit_median_definition

                results2 <- km_fit_median_definition

                # results 3 univariate cox regression ----


                # myoveralltime,
                # thefactor
                # myoutcome


                names(mydata) <- c(self$options$overalltime,
                                   self$options$explanatory,
                                   self$options$outcome)


                formula2 <- jmvcore::constructFormula(terms =
                                                          # "thefactor"
                                                          self$options$explanatory
                                                      )

                formula2 <- jmvcore::composeTerm(formula2)

                formulaL <- jmvcore::constructFormula(terms =
                                                          # "myoveralltime"
                                                          self$options$overalltime
                                                          )

                formulaR <- jmvcore::constructFormula(terms =
                                                          # "myoutcome"
                                                          self$options$outcome
                                                      )

                myformula <- paste("Surv(", formulaL, ",", formulaR, ")")

                finalfit::finalfit(.data = mydata,
                                   dependent = myformula,
                                   explanatory = formula2) -> tUni

                # results3 <- tUni

                # results 4  univariate survival html ----

                results4 <- knitr::kable(tUni[, 1:4],
                                         row.names = FALSE,
                                         align = c('l', 'l', 'r', 'r', 'r', 'r'),
                                         format = "html")

                # results 5 univariate survival explanation ----

                tUni_df <- tibble::as_tibble(tUni, .name_repair = "minimal") %>%
                    janitor::clean_names(dat = ., case = "snake")

                n_level <- dim(tUni_df)[1]

                tUni_df_descr <- function(n) {
                    paste0(
                        "When ",
                        self$options$explanatory,
                        # tUni_df$dependent_surv_overall_time_outcome[1],
                        " is ",
                        tUni_df$x[n + 1],
                        ", there is ",
                        tUni_df$hr_univariable[n + 1],
                        " times risk than ",
                        "when ",
                        self$options$explanatory,
                        # tUni_df$dependent_surv_overall_time_outcome[1],
                        " is ",
                        tUni_df$x[1],
                        "."
                    )
                }

                results5 <- purrr::map(.x = c(2:n_level-1), .f = tUni_df_descr)

                results5 <- unlist(results5)


                # results 6 1,3,5-yr survival ----

                utimes <- self$options$cutp

                utimes <- strsplit(utimes, ",")
                utimes <- purrr::reduce(utimes, as.vector)
                utimes <- as.numeric(utimes)

                # as.numeric(strsplit(utimes, ',')[[1]])

                if (length(utimes) == 0) {
                utimes <- c(12,36,60)
                }

                # self$results$deneme$setContent(utimes)

                # utimes <- c(12,36,60)
                km_fit_summary <- summary(km_fit, times = utimes
                                              # c(12,36,60)
                                          )

                km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", "surv", "std.err", "lower", "upper")])


                km_fit_df[,1] <- gsub(pattern = "thefactor=",
                                      replacement = paste0(self$options$explanatory, " "),
                                      x = km_fit_df[,1])

                km_fit_df_html <- knitr::kable(km_fit_df,
                                               row.names = FALSE,
                                               align = c('l', rep('r', 7)),
                                               format = "html",
                                               digits = 2)


                results6 <- km_fit_df_html



                # results 7 1,3,5-yr survival summary ----

                km_fit_df %>%
                    dplyr::mutate(
                        description =
                            glue::glue(
                                "When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]."
                            )
                    ) %>%
                    dplyr::select(description) %>%
                    dplyr::pull() -> km_fit_definition

                results7 <- km_fit_definition



                     # results 8 pairwise comparison ----

                if(n_level < 3) {

                    results8 <- "No pairwise comparison when explanatory variable has < 3 levels"
                    results9 <- ""

                } else {

                    formula_p <- paste0('survival::Surv(', formulaL, ',', formulaR, ') ~ ', formula2)
                    formula_p <- as.formula(formula_p)
                    results8 <-
                        survminer::pairwise_survdiff(
                            formula = formula_p,
                            data = mydata,
                            p.adjust.method = "BH")





                mypairwise2 <- as.data.frame(results8[["p.value"]]) %>%
                                tibble::rownames_to_column()

                            mypairwise2 %>%
                                tidyr::pivot_longer(cols = -rowname) %>%
                                dplyr::filter(complete.cases(.)) %>%
                                dplyr::mutate(description =
                                                  glue::glue(
                                                      "The comparison between ", self$options$explanatory, " {rowname} and ", self$options$explanatory," {name} has a p-value of {round(value, 2)}."
                                                  )
                                ) %>%
                                dplyr::select(description) %>%
                                dplyr::pull() -> mypairwisedescription

                            mypairwisedescription <- unlist(mypairwisedescription)

                            mypairwisedescription <- paste0(
                                "In the pairwise comparison of ", self$options$explanatory, ":\n",
                                mypairwisedescription, "\n")

                            results9 <- mypairwisedescription

                }



                # Results ----
                ## self$results$text1$setContent(results1)
                self$results$text1html$setContent(results1html)
                self$results$text2$setContent(results2)
                ## self$results$text3$setContent(results3)
                self$results$text4$setContent(results4)
                self$results$text5$setContent(results5)
                self$results$text6$setContent(results6)
                self$results$text7$setContent(results7)
                self$results$text8$setContent(results8)
                self$results$text9$setContent(results9)



# Prepare Data For Plot ----

                # plotData <- mydata
                # image <- self$results$plot
                # image$setState(plotData)

            }
},

.plot=function(image, ...) {  # <-- the plot function ----

    # plotData <- image$state

    if (nrow(self$data) == 0)
        stop('Data contains no (complete) rows')

    if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$overalltime) )
        return()

    sc <- self$options$sc

    if(!sc)
        return()



    uoveralltime <- self$options$overalltime

    uoveralltime <- jmvcore::toNumeric(self$data[[uoveralltime]])

    uthefactor <- self$options$explanatory

    uthefactor <- self$data[[uthefactor]]

    uoutcome <- self$options$outcome

    uoutcome <- jmvcore::toNumeric(self$data[[uoutcome]])




    mydata <- data.frame(myoveralltime = uoveralltime,
                         thefactor = uthefactor,
                         myoutcome = uoutcome)

    mydata <- na.omit(mydata)

    names(mydata) <- c(self$options$overalltime,
                       self$options$explanatory,
                       self$options$outcome)


    formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)

    formula2 <- jmvcore::composeTerm(formula2)

    formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

    formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

    myformula <- paste("survival::Surv(", formulaL, ",", formulaR, ")")

    plot <- mydata %>%
        finalfit::surv_plot(.data = .,
                            dependent = myformula,
                            explanatory = formula2,
                            xlab = 'Time (months)',
                            pval = TRUE,
                            legend = 'none',
                            break.time.by = 12,
                            xlim = c(0,60),
                            title = paste0("Survival curves for ", self$options$explanatory),
                            subtitle = "Based on Kaplan-Meier estimates"
        )


    print(plot)
    TRUE



}

        )
)
