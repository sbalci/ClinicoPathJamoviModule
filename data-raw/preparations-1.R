# Prepare data for analysis
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

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

# Retrieve the variable name from the label
dependent_variable_name_from_label <-
    names(all_labels)[all_labels == self$options$outcome]

# Retrieve the variable names vector from the label vector
labels <- self$options$explanatory

explanatory_variable_names <-
    names(all_labels)[match(labels, all_labels)]


formulaDependent <- jmvcore::constructFormula(terms = dependent_variable_name_from_label)

formulaExplanatory <- jmvcore::composeTerms(listOfComponents = explanatory_variable_names)

.mytimetodata = function() {
    mycalculatedtime <- private$.definemytime()

    if (self$options$calculatedtime &&
        self$results$calculatedtime$isNotFilled()) {
        self$results$calculatedtime$setValues(mycalculatedtime)
    }

}


.myoutcometodata = function() {
    mydefinedoutcome <- private$.definemyoutcome()

    if (self$options$outcomeredifened &&
        self$results$outcomeredifened$isNotFilled()) {
        self$results$outcomeredifened$setValues(mydefinedoutcome)
    }

}
