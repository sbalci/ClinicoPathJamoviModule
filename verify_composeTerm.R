library(jmvcore)

# Mock jmvcore::composeTerm
if (!exists("composeTerm", where = asNamespace("jmvcore"))) {
    # If we can't load the real one, mock it to behave like we expect (add backticks)
    assign("composeTerm", function(x) {
        if (grepl(" ", x)) return(paste0("`", x, "`"))
        return(x)
    }, envir = asNamespace("jmvcore"))
}

df <- data.frame("A" = 1:3, "B C" = 4:6, check.names = FALSE)
vars <- c("A", "B C")

# Simulate the current problematic logic
var_list_composed <- sapply(vars, function(v) jmvcore::composeTerm(v), USE.NAMES=FALSE)
print(var_list_composed)

# Try subsetting
tryCatch({
    print(head(df[var_list_composed]))
    cat("Subsetting with composeTerm WORKED (unexpectedly if backticks added)\n")
}, error = function(e) {
    cat("Subsetting with composeTerm FAILED: ", e$message, "\n")
})

# Simulate correct logic
var_list_raw <- vars
tryCatch({
    print(head(df[var_list_raw]))
    cat("Subsetting with raw names WORKED\n")
}, error = function(e) {
    cat("Subsetting with raw names FAILED: ", e$message, "\n")
})


