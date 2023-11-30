#' @title Competing Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'


competingsurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "competingsurvivalClass",
    inherit = competingsurvivalBase,
    private = list(
        .run = function() {

            # https://finalfit.org/articles/survival.html#death-status


            # If no variable selected Initial Message ----

            if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$overalltime) ) {

                todo <- glue::glue("
<br>This Module is still under development<br><hr><br>
<br>
The explanation below is adopted from <a href = 'https://finalfit.org/articles/survival.html#death-status'>finalfit website documentation</a>.
<br><br>
<b>Outcome</b> is the the status at the time of study:<br>
Dead of Disease   : Patients had died from disease.<br>
Dead of Other     : Patients had died from other causes.<br>
Alive w Disease   : Patients are alive and still have disease (at the last known time).<br>
Alive w/o Disease : PAtients are alive and free of disease (at the last known time).<br>
<br>
<b>Select Analysis Type</b>
<br>
<b>Overall survival:</b><br>
Considering all-cause mortality.<br>
Does not care whether patient is died of disease or a traffic accident.<br>
(Alive) <=> (Dead of Disease & Dead of Other Causes)<br>
<br>
<b>Cause-specific (Disease-specific) survival:</b><br>
Considering disease-specific mortality.<br>
(Alive & Dead of Other Causes) <=> (Dead of Disease)<br>
<br>
<b>Competing risks:</b><br>
Alive <=> Dead of Disease accounting for Dead of Other Causes.<br>
<br>
<hr>
<br>
This function uses survival, survminer, and finalfit packages. Please cite jamovi and the packages as given below.
<br>
<br>
<hr>
<br>
                                   "
                )

                html <- self$results$todo
                html$setContent(todo)
                return()

            } else {



                dod <- self$options$dod

                dooc <- self$options$dooc

                awd <- self$options$awd

                awod <- self$options$awod



                # interim results ----


                results <- list("Dead of disease" = dod,
                                "Dead of other causes" = dooc,
                                "Alive with Disease" = awd,
                                "Alive wo Disease" = awod)


                self$results$text1$setContent(results)



                # select analysis type ----

                analysistype <- self$options$analysistype


                # Overall survival ----


                if ( analysistype == "overall" ) {


                todo <- glue::glue("
                <br>
                <b>Overall Survival</b> <br>
                This tool will help you calculate median survivals and 1,3,5-yr survivals for a given fisk factor.<br>
                <br>
                Explanatory variable should be categorical (ordinal or nominal).<br>
                Outcome variable should be coded binary (0 or 1).<br>
                If patient is dead or event (recurrence) occured it is 1.<br>
                If censored (patient is alive or free of disease) at the last visit it is 0.<br>
                Survival should be numeric, continuous, and in months.<br>
                ")
                html <- self$results$todo
                html$setContent(todo)


                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')


                # dod + dooc vs awd + awod ----


                # status is the the patients status at the end of the study.
                # 1 indicates that they had died from melanoma;
                # 2 indicates that they were still alive and;
                # 3 indicates that they had died from causes unrelated to their melanoma.


                # status_os = ifelse(status == 2, 0, # "still alive"
                #                    1), # "died of melanoma" or "died of other causes"





                # library(survival)
                #
                # survival_object = melanoma %$%
                #     Surv(time, status_os)
                #
                # # Explore:
                # head(survival_object) # + marks censoring, in this case "Alive"
                # #> [1]  10   30   35+  99  185  204
                #
                # # Expressing time in years
                # survival_object = melanoma %$%
                #     Surv(time/365, status_os)




                # # Overall survival in whole cohort
                # my_survfit = survfit(survival_object ~ 1, data = melanoma)
                # my_survfit # 205 patients, 71 events
                # #> Call: survfit(formula = survival_object ~ 1, data = melanoma)
                # #>
                # #>       n  events  median 0.95LCL 0.95UCL
                # #>  205.00   71.00      NA    9.15      NA



                # summary(my_survfit, times = c(0, 1, 2, 3, 4, 5))
                # #> Call: survfit(formula = survival_object ~ 1, data = melanoma)
                # #>
                # #>  time n.risk n.event survival std.err lower 95% CI upper 95% CI
                # #>     0    205       0    1.000  0.0000        1.000        1.000
                # #>     1    193      11    0.946  0.0158        0.916        0.978
                # #>     2    183      10    0.897  0.0213        0.856        0.940
                # #>     3    167      16    0.819  0.0270        0.767        0.873
                # #>     4    160       7    0.784  0.0288        0.730        0.843
                # #>     5    122      10    0.732  0.0313        0.673        0.796
                # # 5 year overall survival is 73%


                # dependent_os = "Surv(time/365, status_os)"
                # explanatory = c("ulcer")
                #
                # melanoma %>%
                #     surv_plot(dependent_os, explanatory, pval = TRUE)
                # #> Warning: Vectorized input to `element_text()` is not officially supported.
                # #> Results may be unexpected or may change in future versions of ggplot2.


                # dependent_os = "Surv(time, status_os)"
                # explanatory = c("age", "sex", "thickness", "ulcer")

                # melanoma %>%
                #     finalfit(dependent_os, explanatory)




                }



                # Diease-specific survival ----



                if ( analysistype == "cause" ) {


                todo <- glue::glue("
                                   <br>
                                   <b>Cause-specific (Diease-specific) survival</b>
                                   <br>
                                   <hr>
                                   <br>
                                   ")
                html <- self$results$todo
                html$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')


                # dod vs awd + awod + dooc  ----





                # status is the the patients status at the end of the study.
                # 1 indicates that they had died from melanoma;
                # 2 indicates that they were still alive and;
                # 3 indicates that they had died from causes unrelated to their melanoma.



                # status_dss = case_when(
                #     status == 2 ~ 0, # "still alive"
                #     status == 1 ~ 1, # "died of melanoma"
                #     TRUE ~  0),     # "died of other causes is censored"


                # dependent_dss = "Surv(time, status_dss)"
                # explanatory = c("age", "sex", "thickness", "ulcer")


                # melanoma %>%
                #     finalfit(dependent_dss, explanatory)

                }



                # Competing risks ----

                if ( analysistype == "compete" ) {




                todo <- glue::glue("
                                       <br>
                                       <b>Competing risks</b>
                                       <br>
                                       <hr>
                                       <br>
                                       ")
                html <- self$results$todo
                html$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')


                # status is the the patients status at the end of the study.
                # 1 indicates that they had died from melanoma;
                # 2 indicates that they were still alive and;
                # 3 indicates that they had died from causes unrelated to their melanoma.



                # status_crr = case_when(
                #     status == 2 ~ 0, # "still alive"
                #     status == 1 ~ 1, # "died of melanoma"
                #     TRUE ~ 2),       # "died of other causes"



                # dependent_crr = "Surv(time, status_crr)"
                # explanatory = c("age", "sex", "thickness", "ulcer")


                # melanoma %>%
                #     finalfit(dependent_crr, explanatory)





                # Competing risks regression ----

                # Competing-risks regression is an alternative to CPH regression. It can be useful if the outcome of interest may not be able to occur simply because something else (like death) has happened first. For instance, in our example it is obviously not possible for a patient to die from melanoma if they have died from another disease first. By simply looking at cause-specific mortality (deaths from melanoma) and considering other deaths as censored, bias may result in estimates of the influence of predictors.

                # The approach by Fine and Gray is one option for dealing with this. It is implemented in the package cmprsk. The crr() syntax differs from survival::coxph() but finalfit brings these together.

                # It uses the finalfit::ff_merge() function, which can join any number of models together.


                # explanatory = c("age", "sex", "thickness", "ulcer")
                # dependent_dss = "Surv(time, status_dss)"
                # dependent_crr = "Surv(time, status_crr)"

                # melanoma %>%
                #
                #     # Summary table
                #     summary_factorlist(dependent_dss, explanatory, column = TRUE, fit_id = TRUE) %>%
                #
                #     # CPH univariable
                #     ff_merge(
                #         melanoma %>%
                #             coxphmulti(dependent_dss, explanatory) %>%
                #             fit2df(estimate_suffix = " (DSS CPH univariable)")
                #     ) %>%
                #
                #     # CPH multivariable
                #     ff_merge(
                #         melanoma %>%
                #             coxphmulti(dependent_dss, explanatory) %>%
                #             fit2df(estimate_suffix = " (DSS CPH multivariable)")
                #     ) %>%
                #
                #     # Fine and Gray competing risks regression
                #     ff_merge(
                #         melanoma %>%
                #             crrmulti(dependent_crr, explanatory) %>%
                #             fit2df(estimate_suffix = " (competing risks multivariable)")
                #     ) %>%
                #
                #
                #     select(-fit_id, -index) %>%
                #     dependent_label(melanoma, "Survival") %>%
                #     mykable()
                # #> Dependent variable is a survival object
























                }










}

        })
)
