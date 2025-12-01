
# Verification script comparing timedependentdca with dcurves package

# Mock jmvcore environment
jmvcore <- new.env()
library(survival)
library(dcurves)
library(dplyr)

# Mock Option classes
Option <- R6::R6Class("Option", public = list(name=NULL, value=NULL, initialize=function(n,v){self$name<-n; self$value<-v}))
OptionNumber <- R6::R6Class("OptionNumber", inherit = Option, public = list(initialize=function(n,v,d,min,max){super$initialize(n,v)}))
OptionBool <- R6::R6Class("OptionBool", inherit = Option, public = list(initialize=function(n,v,d){super$initialize(n,v)}))
OptionList <- R6::R6Class("OptionList", inherit = Option, public = list(initialize=function(n,v,o,d){super$initialize(n,v)}))

jmvcore$Option <- Option
jmvcore$OptionNumber <- OptionNumber
jmvcore$OptionBool <- OptionBool
jmvcore$OptionList <- OptionList

# Mock Result classes
ResultElement <- R6::R6Class("ResultElement", public = list(name=NULL, visible=TRUE, initialize=function(o,n,t,v=T){self$name<-n}))
Table <- R6::R6Class("Table", inherit = ResultElement, public = list(
    rows = list(),
    columns = list(),
    initialize = function(options, name, title, rows, columns, ...) {
        super$initialize(options, name, title, ...)
        self$columns <- columns
    },
    addRow = function(rowKey, values) {
        self$rows[[length(self$rows) + 1]] <- list(key=rowKey, values=values)
    }
))
Html <- R6::R6Class("Html", inherit = ResultElement, public = list(setContent=function(c){}))
Image <- R6::R6Class("Image", inherit = ResultElement, public = list(initialize=function(o,n,t,w,h,r){}))
Group <- R6::R6Class("Group", inherit = ResultElement, public = list(
    items = NULL,
    initialize = function(options, name, title, ...) {
        super$initialize(options, name, title, ...)
        self$items <- list()
    },
    add = function(item) {
        if (!is.null(item$name)) {
            self$items[[item$name]] <- item
        }
    }
))

jmvcore$Table <- Table
jmvcore$Html <- Html
jmvcore$Image <- Image
jmvcore$Group <- Group
jmvcore$Analysis <- R6::R6Class("Analysis", public = list(
    options=NULL, results=NULL, data=NULL,
    initialize=function(options, results, data, ...){self$options<-options; self$results<-results; self$data<-data},
    init=function(){private$.init()},
    run=function(){private$.run()}
))
jmvcore$Options <- R6::R6Class("Options", public = list(initialize=function(...){}))

# Define Results Class
timedependentdcaResults <- R6::R6Class(
    "timedependentdcaResults",
    inherit = jmvcore$Group,
    active = list(
        instructionsText = function() self$items[["instructionsText"]],
        interpretationText = function() self$items[["interpretationText"]],
        netBenefitTable = function() self$items[["netBenefitTable"]],
        summaryTable = function() self$items[["summaryTable"]],
        interventionsTable = function() self$items[["interventionsTable"]],
        netBenefitPlot = function() self$items[["netBenefitPlot"]],
        interventionsPlot = function() self$items[["interventionsPlot"]]),
    public=list(
        initialize=function(options) {
            super$initialize(options=options, name="", title="Time-Dependent Decision Curve Analysis")
            self$add(jmvcore$Html$new(options, "instructionsText", "Instructions"))
            self$add(jmvcore$Html$new(options, "interpretationText", "Interpretation Guide"))
            self$add(jmvcore$Table$new(options, "netBenefitTable", "Net Benefit Analysis", 0, list()))
            self$add(jmvcore$Table$new(options, "summaryTable", "Summary Metrics", 0, list()))
            self$add(jmvcore$Table$new(options, "interventionsTable", "Interventions Avoided", 0, list()))
            self$add(jmvcore$Image$new(options, "netBenefitPlot", "Net Benefit Curves", 600, 400, ".netBenefitPlot"))
            self$add(jmvcore$Image$new(options, "interventionsPlot", "Interventions Avoided", 600, 400, ".interventionsPlot"))
        }
    )
)

timedependentdcaBase <- R6::R6Class(
    "timedependentdcaBase",
    inherit = jmvcore$Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                options = options,
                results = timedependentdcaResults$new(options=options),
                data = data)
        }))

# Source the implementation
source("R/timedependentdca.b.R")

# 1. Generate Data
set.seed(123)
n <- 1000
predictor <- rnorm(n)
lambda <- 0.1 * exp(0.5 * predictor)
time <- rexp(n, rate = lambda)
censor <- rexp(n, rate = 0.1)
obs_time <- pmin(time, censor)
event <- as.numeric(time <= censor)

test_data <- data.frame(
    time = obs_time,
    event = event,
    predictor = predictor
)

t_eval <- 5.0

# 2. Prepare for dcurves
# Fit Cox model manually to generate risks
cox_fit <- coxph(Surv(time, event) ~ predictor, data = test_data)
# Predict risk at t_eval
surv_probs <- summary(survfit(cox_fit, newdata = test_data), times = t_eval)$surv
if(is.matrix(surv_probs)) {
    test_data$risk <- 1 - surv_probs[1, ]
} else {
    test_data$risk <- 1 - surv_probs
}

# Verify risk range
# print(range(test_data$risk))

# Run dcurves with pre-calculated risk
print(paste("Evaluating at time t =", t_eval))

dca_gold <- dcurves::dca(
    Surv(time, event) ~ risk,
    data = test_data,
    time = t_eval,
    thresholds = seq(0.01, 0.99, by = 0.01)
)

gold_nb <- dca_gold$dca %>% 
    filter(variable == "risk") %>%
    select(threshold, net_benefit)

# 3. Run Our Module
# Use "cox" estimate so it replicates the fitting process
timedependentdcaOptions <- R6::R6Class(
    "timedependentdcaOptions",
    inherit = jmvcore$Options,
    public = list(
        time = "time",
        event = "event",
        predictor = "predictor",
        time_points = as.character(t_eval),
        threshold_range_min = 0.01,
        threshold_range_max = 0.99,
        threshold_steps = 99,
        estimate_survival = "cox",
        reference_strategy = "treat_all",
        smoothing = FALSE,
        plot_by_timepoint = FALSE,
        random_seed = 123,
        initialize = function(...) {}
    )
)

options_obj <- timedependentdcaOptions$new()
module_analysis <- timedependentdcaClass$new(
    options = options_obj,
    data = test_data
)
module_analysis$init()
module_analysis$run()

# Extract Results
module_rows <- module_analysis$results$netBenefitTable$rows
results_df <- data.frame()
for (row in module_rows) {
    vals <- row$values
    if (vals$time_point == t_eval) {
        results_df <- rbind(results_df, data.frame(
            threshold = vals$threshold,
            net_benefit_module = vals$net_benefit
        ))
    }
}

# Join with Gold
comparison <- merge(results_df, gold_nb, by="threshold")
comparison$diff <- comparison$net_benefit_module - comparison$net_benefit

print("Comparison Summary (Module - Gold):")
print(summary(comparison$diff))

max_diff <- max(abs(comparison$diff), na.rm = TRUE)
print(paste("Max Absolute Difference:", max_diff))

if (max_diff < 0.01) {
    print("SUCCESS: Results match within tolerance.")
} else {
    print("FAILURE: Significant discrepancy found.")
    print(head(comparison[order(-abs(comparison$diff)), ]))
}
