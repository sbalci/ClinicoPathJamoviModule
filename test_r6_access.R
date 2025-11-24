
library(R6)

MyClass <- R6::R6Class("MyClass",
    public = list(
        initialize = function() {},
        val = 1
    ),
    active = list(
        activeVal = function() 2
    )
)

obj <- MyClass$new()
print(paste("Direct access:", obj$activeVal))
tryCatch({
    print(paste("Bracket access:", obj[["activeVal"]]))
}, error = function(e) {
    print(paste("Bracket access failed:", e$message))
})
