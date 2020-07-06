#' @title Multivariate Survival Analysis
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'

multisurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "multisurvivalClass",
    inherit = multisurvivalBase,
    private = list(
        .run = function() {


            # # Error Message ----
            #
            # if (nrow(self$data) == 0) stop("Data contains no (complete) rows")
            #
            # if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) ) {
            #     # ToDo Message ----
            #     todo <- "
            #         <br>Welcome to ClinicoPath
            #                   <br><br>
            #                   This tool will help you form an Alluvial Plots.
            #                   "
            #     html <- self$results$todo
            #     html$setContent(todo)
            #
            # } else {
            #     todo <- ""
            #     html <- self$results$todo
            #     html$setContent(todo)
            #
            #
            #
            # }


            # If no variable selected Initial Message ----

            if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$overalltime) )
            {

                # TODO ----

                todo <- glue::glue("
                    <br>Welcome to ClinicoPath
                    <br><br>
                        This tool will help you perform a multivariate survival analysis.
                    <br><br>
                        Explanatory variables can be categorical (ordinal or nominal) or continuous.
                    <br><br>
                        Outcome variable should be coded binary (0 or 1):
                    <br><br>
                        If patient is dead or event (recurrence) occured it is 1.
                    <br><br>
                        If censored (patient is alive or free of disease) at the last visit it is 0.
                    <br><br>
                        Survival should be numeric, continuous, and in months.
                    <br><br>
                        This function uses finalfit, survival, survminer and ggstatsplot packages. Please cite jamovi and the packages as given below.
                    <br><br>
                    ")
                # https://finalfit.org/articles/all_tables_examples.html#cox-proportional-hazards-model-survival-time-to-event


                html <- self$results$todo
                html$setContent(todo)
                return()

            } else {

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')



            # Check if outcome variable is suitable or stop ----

            myoutcome2 <- self$options$outcome
            myoutcome2 <- self$data[[myoutcome2]]
            myoutcome2 <- na.omit(myoutcome2)

            if (class(myoutcome2) == "factor")
                stop("Please use a continuous variable for outcome.")

            if (any(myoutcome2 != 0 & myoutcome2 != 1))
                stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')


            # prepare data ----

            mydata <- self$data

            # prepare formula ----

            formula2 <- as.vector(self$options$explanatory)

            formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaL <- jmvcore::toNumeric(formulaL)

            # formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            formulaR <- jmvcore::toNumeric(formulaR)


            myformula <- paste("Surv(", formulaL, ",", formulaR, ")")

            # results1 <- list(
            #     formula2,
            #     formulaL,
            #     formulaR,
            #     myformula
            # )
            #
            # self$results$text$setContent(results1)

            # finalfit multivariate table ----

            finalfit::finalfit(.data = mydata,
                               dependent = myformula,
                               explanatory = formula2,

                               # metrics = TRUE
                               ) -> tMultivariate

            results1 <- knitr::kable(tMultivariate,
                                     row.names = FALSE,
                                     align = c('l', 'l', 'r', 'r', 'r', 'r'),
                                     format = "html")

            self$results$text$setContent(results1)


            # Reduced model ----
            # If you are using a backwards selection approach or similar, a reduced model can be directly specified and compared. The full model can be kept or dropped.

            # explanatory_multi = c("age", "thickness", "ulcer")
            # melanoma %>%
            #     finalfit(dependent_os, explanatory, explanatory_multi, keep_models = TRUE) %>%
            #     mykable()


            # Testing for proportional hazards ----
            # An assumption of CPH regression is that the hazard (think risk) associated with a particular variable does not change over time. For example, is the magnitude of the increase in risk of death associated with tumour ulceration the same in the early post-operative period as it is in later years?
            #
            #     The cox.zph() function from the survival package allows us to test this assumption for each variable. The plot of scaled Schoenfeld residuals should be a horizontal line. The included hypothesis test identifies whether the gradient differs from zero for each variable. No variable significantly differs from zero at the 5% significance level.

            # explanatory = c("age", "sex", "thickness", "ulcer", "year")
            # melanoma %>%
            #     coxphmulti(dependent_os, explanatory) %>%
            #     cox.zph() %>%
            #     {zph_result <<- .} %>%
            #     plot(var=5)

            # zph_result
            # #>               rho  chisq      p
            # #> age        0.1633 2.4544 0.1172
            # #> sexMale   -0.0781 0.4473 0.5036
            # #> thickness -0.1493 1.3492 0.2454
            # #> ulcerYes  -0.2044 2.8256 0.0928
            # #> year       0.0195 0.0284 0.8663
            # #> GLOBAL         NA 8.4695 0.1322


            # Stratified models ----
            # One approach to dealing with a violation of the proportional hazards assumption is to stratify by that variable. Including a strata() term will result in a separate baseline hazard function being fit for each level in the stratification variable. It will be no longer possible to make direct inference on the effect associated with that variable.
            #
            # This can be incorporated directly into the explanatory variable list.

            # explanatory= c("age", "sex", "ulcer", "thickness", "strata(year)")
            # melanoma %>%
            #     finalfit(dependent_os, explanatory) %>%
            #     mykable()




            }

        },

        .plot = function(image, ...) {  # <-- the plot function ----

            # plotData <- image$state

            if (is.null(self$options$explanatory) || (length(self$options$outcome) + length(self$options$overalltime) < 2))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Check if outcome variable is suitable or stop ----
            myoutcome2 <- self$options$outcome
            myoutcome2 <- self$data[[myoutcome2]]
            myoutcome2 <- na.omit(myoutcome2)

            if (class(myoutcome2) == "factor")
                stop("Please use a continuous variable for outcome.")


            if (any(myoutcome2 != 0 & myoutcome2 != 1))
                stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')

            mydata <- self$data

            formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)

            formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaL <- jmvcore::toNumeric(formulaL)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            formulaR <- jmvcore::toNumeric(formulaR)


            myformula <- paste("survival::Surv(", formulaL, ",", formulaR, ")")


            # https://finalfit.org/reference/hr_plot.html

            plot <-
                finalfit::hr_plot(
                    .data = mydata,
                    dependent = myformula,
                    explanatory = formula2,
                    dependent_label = "Survival",
                    table_text_size = 4,
                    title_text_size = 14,
                    plot_opts = list(ggplot2::xlab("HR, 95% CI"),
                                     ggplot2::theme(
                                         axis.title =
                                             ggplot2::element_text(size = 12)
                                               )))

            print(plot)
            TRUE

        },

            # .plot2 = function(image, ...) {  # <-- the plot function ----
            #
            # # plotData <- image$state
            #
            #     if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$overalltime) )
            #         return()
            #
            # if (nrow(self$data) == 0)
            #     stop('Data contains no (complete) rows')
            #
            # # Check if outcome variable is suitable or stop ----
            # myoutcome2 <- self$options$outcome
            # myoutcome2 <- self$data[[myoutcome2]]
            # myoutcome2 <- na.omit(myoutcome2)
            #
            # if (class(myoutcome2) == "factor")
            #     stop("Please use a continuous variable for outcome.")
            #
            # if (any(myoutcome2 != 0 & myoutcome2 != 1))
            #     stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')
            #
            # # https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html#cox-proportional-hazards-regression-model-coxph
            # # fit a stratified model
            #
            # mydata <- self$data
            #
            # formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)
            #
            # formulaL <- jmvcore::toNumeric(formulaL)
            #
            # formulaR <- jmvcore::constructFormula(terms = self$options$outcome)
            #
            # formulaR <- jmvcore::toNumeric(formulaR)
            #
            # formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)
            #
            # formula3 <- paste("survival::Surv(", formulaL, ",", formulaR, ") ~ ", formula2)
            #
            # formula3 <- as.formula(formula3)
            #
            # mod <-
            #     survival::coxph(
            #         formula = formula3,
            #         data = mydata
            #     )
            #
            # # plot
            # plot2 <- ggstatsplot::ggcoefstats(
            #     x = mod,
            #     exponentiate = TRUE,
            #     title = "Cox proportional hazards regression model"
            # )
            #
            # print(plot2)
            # TRUE
            #
            # },

        .plot3 = function(image, ...) {  # <-- the plot function ----

            # plotData <- image$state

            if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$overalltime) )
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Check if outcome variable is suitable or stop ----
            myoutcome2 <- self$options$outcome
            myoutcome2 <- self$data[[myoutcome2]]
            myoutcome2 <- na.omit(myoutcome2)

            if (class(myoutcome2) == "factor")
                stop("Please use a continuous variable for outcome.")

            if (any(myoutcome2 != 0 & myoutcome2 != 1))
                stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')



            mydata <- self$data

            formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaL <- jmvcore::toNumeric(formulaL)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            formulaR <- jmvcore::toNumeric(formulaR)

            formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)

            formula3 <- paste("survival::Surv(", formulaL, ",", formulaR, ") ~ ", formula2)

            formula3 <- as.formula(formula3)

            mod <-
                survival::coxph(
                    formula = formula3,
                    data = mydata
                )

            # plot

        # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf


        # The function ggforest() from the survminer package creates a forest plot for a Cox regression model fit. Hazard ratio estimates along with confiden- ce intervals and p-values are plotter for each variable.

        # lung$age <- ifelse(lung$age > 70, ">70","<= 70")
        # fit <- coxph( Surv(time, status) ~ sex + ph.ecog + age, data = lung)
        # ggforest(fit)

            plot3 <- survminer::ggforest(model = mod,
                                         data = mydata)

            print(plot3)
            TRUE

        }


,
.plot4 = function(image, ...) {  # <-- the plot function ----

    # plotData <- image$state

    if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$overalltime) || is.null(self$options$adjexplanatory))
        return()

    if (nrow(self$data) == 0)
        stop('Data contains no (complete) rows')

    # Check if outcome variable is suitable or stop ----
    myoutcome2 <- self$options$outcome
    myoutcome2 <- self$data[[myoutcome2]]
    myoutcome2 <- na.omit(myoutcome2)

    if (class(myoutcome2) == "factor")
        stop("Please use a continuous variable for outcome.")

    if (any(myoutcome2 != 0 & myoutcome2 != 1))
        stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')



    mydata <- self$data

    formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

    formulaL <- jmvcore::toNumeric(formulaL)

    formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

    formulaR <- jmvcore::toNumeric(formulaR)

    formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)

    formula3 <- paste("survival::Surv(", formulaL, ",", formulaR, ") ~ ", formula2)

    formula3 <- as.formula(formula3)

    mod <-
        survival::coxph(
            formula = formula3,
            data = mydata
        )

    # plot

    # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf


    # The function ggadjustedcurves() from the survminer package plots Adjusted Survival Curves for Cox Proportional Hazards Model. Adjusted Survival Curves show how a selected factor influences survival estimated from a Cox model.
    # Note that these curves differ from Kaplan Meier estimates since they present expected ssurvival based on given Cox model.

    # lung$sex <- ifelse(lung$sex == 1, "Male", "Female")

    # fit <- coxph(Surv(time, status) ~ sex + ph.ecog + age +
    #                  strata(sex), data = lung)
    # ggcoxadjustedcurves(fit, data=lung)


    # Note that it is not necessary to include the grouping factor in the Cox model. Survival curves are estimated from Cox model for each group defined by the factor independently.

    # lung$age3 <- cut(lung$age,
    #                  c(35,55,65,85))


    # ggcoxadjustedcurves(fit, data=lung,
    #                     variable=”lage3”)


    adjexplanatory <- self$options$adjexplanatory

    adjexplanatory <- jmvcore::composeTerm(components = adjexplanatory)



    plot4 <- survminer::ggadjustedcurves(fit = mod,
                                         data = mydata,
                                         variable = adjexplanatory
                                         # method = ,
                                         # fun =

                                         )



    print(plot4)
    TRUE

}




        )
)
