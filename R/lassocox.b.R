#' @title Lasso-Cox Regression
#' @importFrom R6 R6Class
#' @import jmvcore
#'

lassocoxClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lassocoxClass",
    inherit = lassocoxBase,
    private = list(
        .init = function() {

            # Initialize with welcome message if no variables selected
            if (is.null(self$options$elapsedtime) ||
                is.null(self$options$outcome) ||
                is.null(self$options$explanatory)) {

                todo <- "
            <br>Welcome to Lasso-Cox Regression
            <br><br>
            This analysis performs variable selection in survival analysis using the Lasso penalty.
            <br><br>
            Required inputs:
            <br>- Time Elapsed: Survival/follow-up time
            <br>- Outcome: Binary outcome (event/censored)
            <br>- Explanatory Variables: Potential predictors for selection
            <br><br>
            The analysis will:
            <br>- Select important variables
            <br>- Estimate their effects
            <br>- Create visualizations
            <br>- Calculate risk scores
        "
                html <- self$results$todo
                html$setContent(todo)

                # Hide results
                self$results$modelSummary$setVisible(FALSE)
                self$results$coefficients$setVisible(FALSE)
                self$results$performance$setVisible(FALSE)
                self$results$cv_plot$setVisible(FALSE)
                self$results$coef_plot$setVisible(FALSE)
                self$results$survival_plot$setVisible(FALSE)
            }




            if (is.null(self$data) || nrow(self$data) == 0)
                return()

            if (is.null(self$options$elapsedtime) ||
                is.null(self$options$outcome) ||
                is.null(self$options$explanatory))
                return()
        }

        # ,
        # .run = function() {
        #     if (is.null(self$data) || nrow(self$data) == 0)
        #         return()
        #
        #     if (is.null(self$options$elapsedtime) ||
        #         is.null(self$options$outcome) ||
        #         is.null(self$options$explanatory))
        #         return()
        #
        #     # Prepare data
        #     data <- private$.cleanData()
        #     if (is.null(data))
        #         return()
        #
        #     # Fit model
        #     results <- private$.fitModel(data)
        #     if (is.null(results))
        #         return()
        #
        #     # Populate tables
        #     private$.populateModelSummary(results)
        #     private$.populateCoefficients(results)
        #     private$.populatePerformance(results)
        #
        #     # Save plots for rendering
        #     private$.savePlotData(results)
        # },
        #
        # .cleanData = function() {
        #
        #     # Get variables
        #     data <- self$data
        #
        #     # Validate time variable
        #     time <- jmvcore::toNumeric(data[[self$options$elapsedtime]])
        #     if (any(is.na(time)))
        #         stop("Time variable contains missing values")
        #     if (any(time < 0))
        #         stop("Time variable contains negative values")
        #
        #     # Validate outcome variable
        #     outcome <- data[[self$options$outcome]]
        #     if (length(unique(na.omit(outcome))) != 2)
        #         stop("Outcome variable must be binary")
        #     status <- as.numeric(outcome == self$options$outcomeLevel)
        #
        #     # Validate predictors
        #     predictors <- data[self$options$explanatory]
        #     if (ncol(predictors) < 2)
        #         stop("At least 2 explanatory variables required")
        #
        #     # Check for complete cases
        #     complete <- complete.cases(time, status, predictors)
        #     if (sum(complete) < 10)
        #         stop("Too few complete cases for analysis")
        #
        #     # Create design matrix
        #     X <- try({
        #         model.matrix(~ ., data=predictors)[,-1]
        #     })
        #
        #     if (inherits(X, "try-error"))
        #         stop("Error creating design matrix. Check variable coding.")
        #
        #     # Standardize if requested
        #     if (self$options$standardize) {
        #         X <- scale(X)
        #     }
        #
        #     # Return cleaned data
        #     return(list(
        #         time = time[complete],
        #         status = status[complete],
        #         X = X[complete,],
        #         n = sum(complete),
        #         n_events = sum(status[complete]),
        #         variable_names = colnames(X)
        #     ))
        #
        # },
        #
        # .fitModel = function(data) {
        #     # Required packages
        #     if (!requireNamespace("glmnet", quietly=TRUE))
        #         stop("Package 'glmnet' needed for Lasso-Cox regression")
        #
        #     if (!requireNamespace("survival", quietly=TRUE))
        #         stop("Package 'survival' needed for Lasso-Cox regression")
        #
        #     # Create survival object
        #     y <- survival::Surv(data$time, data$status)
        #
        #     # Fit cross-validated model
        #     set.seed(1234)
        #     cv_fit <- glmnet::cv.glmnet(
        #         x = data$X,
        #         y = y,
        #         family = "cox",
        #         nfolds = self$options$nfolds
        #     )
        #
        #     # Get optimal lambda
        #     lambda <- if (self$options$lambda == "lambda.min")
        #         cv_fit$lambda.min
        #     else
        #         cv_fit$lambda.1se
        #
        #     # Fit final model
        #     final_model <- glmnet::glmnet(
        #         x = data$X,
        #         y = y,
        #         family = "cox",
        #         lambda = lambda
        #     )
        #
        #     # Get coefficients
        #     coef_matrix <- coef(final_model)
        #     selected_vars <- which(coef_matrix != 0)
        #     coef_values <- coef_matrix[selected_vars]
        #
        #     # Calculate risk scores
        #     risk_scores <- predict(final_model, newx = data$X, type = "link")
        #
        #     # Calculate C-index
        #     cindex <- survival::concordance(y ~ risk_scores)$concordance
        #
        #     return(list(
        #         cv_fit = cv_fit,
        #         final_model = final_model,
        #         coef_matrix = coef_matrix,
        #         selected_vars = selected_vars,
        #         coef_values = coef_values,
        #         risk_scores = risk_scores,
        #         cindex = cindex,
        #         data = data
        #     ))
        # },
        #
        # .populateModelSummary = function(results) {
        #     table <- self$results$modelSummary
        #
        #     # Add rows
        #     table$addRow(rowKey=1, values=list(
        #         statistic="Selected Variables",
        #         value=length(results$selected_vars)
        #     ))
        #
        #     table$addRow(rowKey=2, values=list(
        #         statistic="Lambda",
        #         value=results$final_model$lambda
        #     ))
        # },
        #
        # .populateCoefficients = function(results) {
        #     table <- self$results$coefficients
        #
        #     # Add coefficient rows
        #     for (i in seq_along(results$selected_vars)) {
        #         table$addRow(rowKey=i, values=list(
        #             variable=rownames(results$coef_matrix)[results$selected_vars[i]],
        #             coefficient=results$coef_values[i]
        #         ))
        #     }
        # },
        #
        # .populatePerformance = function(results) {
        #     table <- self$results$performance
        #
        #     table$addRow(rowKey=1, values=list(
        #         metric="C-index",
        #         value=results$cindex
        #     ))
        # },
        #
        # .cvPlot = function(image, ggtheme, theme, ...) {
        #     if (!self$options$cv_plot)
        #         return()
        #
        #     results <- image$state
        #     if (is.null(results))
        #         return()
        #
        #     plot <- plot(results$cv_fit)
        #     print(plot)
        #     TRUE
        # },
        #
        # .coefPlot = function(image, ggtheme, theme, ...) {
        #     if (!self$options$coef_plot)
        #         return()
        #
        #     results <- image$state
        #     if (is.null(results))
        #         return()
        #
        #     plot <- plot(results$final_model)
        #     print(plot)
        #     TRUE
        # },
        #
        # .survivalPlot = function(image, ggtheme, theme, ...) {
        #     if (!self$options$survival_plot)
        #         return()
        #
        #     results <- image$state
        #     if (is.null(results))
        #         return()
        #
        #     # Create risk groups
        #     risk_groups <- cut(results$risk_scores,
        #                        breaks=c(-Inf, median(results$risk_scores), Inf),
        #                        labels=c("Low Risk", "High Risk"))
        #
        #     # Fit survival curves
        #     fit <- survival::survfit(
        #         Surv(results$data$time, results$data$status) ~ risk_groups
        #     )
        #
        #     # Create plot
        #     plot <- survminer::ggsurvplot(
        #         fit,
        #         data = data.frame(
        #             time = results$data$time,
        #             status = results$data$status,
        #             risk_groups = risk_groups
        #         ),
        #         risk.table = TRUE,
        #         pval = TRUE
        #     )
        #
        #     print(plot)
        #     TRUE
        # },
        #
        # .savePlotData = function(results) {
        #     if (self$options$cv_plot)
        #         self$results$cv_plot$setState(results)
        #
        #     if (self$options$coef_plot)
        #         self$results$coef_plot$setState(results)
        #
        #     if (self$options$survival_plot)
        #         self$results$survival_plot$setState(results)
        #
        #     # Add risk scores to dataset if requested
        #     if (self$options$riskScore && is.null(self$results$riskScore))
        #         self$results$riskScore$setValues(results$risk_scores)
        # }





        )
)
