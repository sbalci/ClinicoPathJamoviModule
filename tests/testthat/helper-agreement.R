# Helpers for the agreement test files.

# Run the analysis class directly. `outputs` names jamovi Output options
# (consensusVar, loaOutput) to switch on. Passing them to agreementOptions$new() or
# to the agreement() wrapper is silently DROPPED - OptionOutput$value reads
# isTRUE(private$.value$value) - so a test that passes consensusVar = TRUE that way
# is testing the OFF state. Same shape as run_survivalcont_jamovi().
agreement_run <- function(data, ..., outputs = character()) {
    ns <- asNamespace("ClinicoPath")
    options <- ns$agreementOptions$new(...)
    for (nm in outputs) {
        opt <- options$option(nm)   # an R6 reference, so this sets it on `options`
        opt$value <- list(value = TRUE, synced = FALSE)
    }
    analysis <- ns$agreementClass$new(options = options, data = data)
    suppressWarnings(suppressMessages(analysis$run()))
    analysis
}

# The note TEXTS on a jmvcore Table, keyed by note key.
agreement_notes <- function(table) {
    vapply(table$notes, function(n) n$note, character(1))
}

# The names of every declared column, hidden ones included (asDF drops hidden
# columns, so a "column is gone" check against asDF passes vacuously).
agreement_column_names <- function(table) {
    vapply(table$columns, function(col) col$name, character(1))
}

# An error that looks like jmvcore's .checkpoint() restart signal.
agreement_restart_error <- function() {
    structure(class = c("error", "condition"),
              list(message = "restart", call = NULL, code = "restart"))
}
