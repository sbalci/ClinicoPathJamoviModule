
library(R6)

Parent <- R6::R6Class("Parent",
    private = list(
        .items = list(a=1)
    )
)

Child <- R6::R6Class("Child",
    inherit = Parent,
    active = list(
        items = function() private$.items
    )
)

c <- Child$new()
print(c$items)
