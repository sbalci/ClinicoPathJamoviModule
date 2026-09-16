#' Demonstration Scenarios for Sequential Testing Analysis
#'
#' Parameter sets used to demonstrate the [sequentialtests()] analysis. Each row is one
#' worked scenario: the sensitivity, specificity and (where present) unit cost of a screening
#' test and a confirmatory test, the disease prevalence to assume, and the testing strategy to
#' apply. They are inputs to the calculator, not patient-level data — `sequentialtests()` takes
#' no variables.
#'
#' @section These numbers are for demonstration only:
#'
#' **The figures in these datasets are illustrative. They are not clinically accurate and must
#' not be used to design a testing protocol or to advise on a patient.**
#'
#' They are rounded, approximate values chosen to make the behaviour of each strategy easy to
#' see — how confirming positives trades sensitivity for specificity, how retesting negatives
#' does the reverse, and how prevalence drives predictive value. Specifically:
#'
#' - They are **not taken from any particular published study**, and carry no citation,
#'   confidence interval, or population definition.
#' - Real test performance varies substantially with assay, manufacturer, specimen type,
#'   operator, disease stage, and time since exposure. A single sensitivity figure hides all
#'   of that.
#' - The prevalences are illustrative settings, not the prevalence in your population, which
#'   is the input that moves predictive values most.
#' - The costs are round numbers in unspecified units, not any real tariff or reimbursement
#'   rate.
#' - Test names such as `"RT-PCR"` or `"Mammography"` label the scenario. They do not assert
#'   that the accompanying numbers describe that test as actually performed anywhere.
#'
#' Before drawing any clinical conclusion, replace every value with an estimate from your own
#' setting, or from a source you have read and judged applicable to your population. The same
#' warning applies to the Teaching Example control inside the analysis, which loads equivalent
#' illustrative values. These examples are not clinical guidance or recommended pathways.
#'
#' @name sequentialtests_datasets
#' @aliases sequentialtests_cancer sequentialtests_cost_comparison sequentialtests_covid sequentialtests_emergency sequentialtests_extreme sequentialtests_infectious sequentialtests_preset_examples sequentialtests_prevalence_sensitivity sequentialtests_reference sequentialtests_strategy_comparison sequentialtests_teaching
#'
#' @format
#' \describe{
#'   \item{sequentialtests_cancer}{Cancer screening scenarios. 6 rows, 12 columns.}
#'   \item{sequentialtests_cost_comparison}{Scenarios contrasting protocol cost. 9 rows, 12 columns.}
#'   \item{sequentialtests_covid}{Respiratory-virus screening across community, hospital and
#'     outbreak prevalences. 12 rows, 12 columns.}
#'   \item{sequentialtests_emergency}{Emergency-department rule-out scenarios. 6 rows, 12 columns.}
#'   \item{sequentialtests_extreme}{Boundary cases (near-perfect and near-useless tests, very
#'     low and very high prevalence) for exercising edge behaviour. 8 rows, 8 columns.}
#'   \item{sequentialtests_infectious}{Infectious-disease screening scenarios. 6 rows, 12 columns.}
#'   \item{sequentialtests_preset_examples}{One row per Teaching Example offered by the analysis.
#'     7 rows, 12 columns.}
#'   \item{sequentialtests_prevalence_sensitivity}{One test pair held fixed while prevalence is
#'     varied, to show how predictive values move. 9 rows, 9 columns.}
#'   \item{sequentialtests_reference}{Assorted reference test pairs. 10 rows, 12 columns.}
#'   \item{sequentialtests_strategy_comparison}{The same test pairs under all three strategies,
#'     for side-by-side comparison. 15 rows, 11 columns.}
#'   \item{sequentialtests_teaching}{Simple round-numbered scenarios for teaching. 8 rows, 7 columns.}
#' }
#'
#' @section Columns:
#' \describe{
#'   \item{scenario}{Label for the clinical setting being illustrated.}
#'   \item{strategy}{Which strategy to apply: `"serial_positive"` (confirm the positives),
#'     `"serial_negative"` (retest the negatives), or `"parallel"` (test everyone with both).}
#'   \item{test1_name, test2_name}{Labels for the screening and confirmatory tests.}
#'   \item{test1_sens, test1_spec, test2_sens, test2_spec}{Assumed accuracy, as proportions.}
#'   \item{test1_cost, test2_cost}{Assumed unit cost, in unspecified units. Present in most
#'     but not all of these datasets.}
#'   \item{prevalence}{Assumed disease prevalence in the population tested, as a proportion.}
#' }
#'
#' @details
#' Note that `"serial_negative"` and `"parallel"` are the same rule — a subject is positive if
#' either test is positive — and so give identical sensitivity, specificity, PPV and NPV. They
#' differ only in how many second tests are performed, which is what the cost columns are for.
#'
#' @seealso [sequentialtests()]
#'
#' @examples
#' # Run one scenario through the analysis
#' data(sequentialtests_covid)
#' row <- sequentialtests_covid[1, ]
#' sequentialtests(
#'     test1_name = row$test1_name, test1_sens = row$test1_sens, test1_spec = row$test1_spec,
#'     test2_name = row$test2_name, test2_sens = row$test2_sens, test2_spec = row$test2_spec,
#'     prevalence = row$prevalence, strategy = row$strategy
#' )
#'
#' # Serial-negative and parallel testing are the same rule, so they agree exactly
#' data(sequentialtests_strategy_comparison)
#' head(sequentialtests_strategy_comparison)
NULL

#' @rdname sequentialtests_datasets
"sequentialtests_cancer"

#' @rdname sequentialtests_datasets
"sequentialtests_cost_comparison"

#' @rdname sequentialtests_datasets
"sequentialtests_covid"

#' @rdname sequentialtests_datasets
"sequentialtests_emergency"

#' @rdname sequentialtests_datasets
"sequentialtests_extreme"

#' @rdname sequentialtests_datasets
"sequentialtests_infectious"

#' @rdname sequentialtests_datasets
"sequentialtests_preset_examples"

#' @rdname sequentialtests_datasets
"sequentialtests_prevalence_sensitivity"

#' @rdname sequentialtests_datasets
"sequentialtests_reference"

#' @rdname sequentialtests_datasets
"sequentialtests_strategy_comparison"

#' @rdname sequentialtests_datasets
"sequentialtests_teaching"
