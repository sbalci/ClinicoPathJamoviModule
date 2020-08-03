# from https://github.com/lago1970/jmvexamples
#
Example05Class <- R6::R6Class(
    "Example05Class",
    inherit = Example05Base,
    private = list(
        .run = function() {

            if (length(self$options$deps) == 0 ||
                is.null(self$options$group))
                    return()

            ttestTable <- self$results$ttest

            for (rowKey in ttestTable$rowKeys) {

                data <- data.frame(
                    x = self$data[[self$options$group]],
                    y = jmvcore::toNumeric(self$data[[rowKey]]))

                ttest <- t.test(y ~ x, data, var.equal=self$options$equVar)
                ttestTable$setRow(rowKey=rowKey, values=list(
                    stat=unname(ttest$statistic),
                    df=unname(ttest$parameter),
                    p=unname(ttest$p.value)
                ))
            }

        })
)
