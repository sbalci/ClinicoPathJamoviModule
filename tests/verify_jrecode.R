
library(jmvcore)
library(R6)
library(dplyr)

# Source the implementation files
source("R/jrecode.h.R")
source("R/jrecode.b.R")

# Helper to run the analysis
run_jrecode <- function(data, dep, ...) {
    options <- jrecodeOptions$new(dep = dep, ...)
    analysis <- jrecodeClass$new(options = options, data = data)
    analysis$run()
    return(analysis$results)
}

# Test Data
data(iris)

print("--- Test 1: Factor Recoding (Species) with Output ---")
# Recode setosa to 'Setosa', versicolor to 'Versicolor', virginica to 'Virginica'
# Also test grouping: setosa='Group1', versicolor='Group2', virginica='Group2'
results1 <- run_jrecode(
    data = iris,
    dep = "Species",
    recode_rules = "'setosa'='Group1', 'versicolor'='Group2', 'virginica'='Group2'",
    show_code = TRUE,
    show_table = TRUE,
    recoded_output = TRUE
)

print("Code Output:")
print(results1$code_output$content)
print("Table Output:")
print(results1$comparison$asDF)

print("Output Data Status:")
if (results1$recoded_data$isFilled()) {
    print("Recoded data output is filled.")
    # Note: In a real Jamovi session, this would create a new variable.
    # Here we just check if the object is populated.
} else {
    print("Recoded data output is NOT filled.")
}
