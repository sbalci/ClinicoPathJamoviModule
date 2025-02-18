# jjstreamgraph.b.R

#' @title StreamGraphs
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%

jjstreamgraphClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjstreamgraphClass",
    inherit = jjstreamgraphBase,
    private = list(
        .run = function() {

            # Initial Message ----
            if (is.null(self$options$timeVar) ||
                is.null(self$options$valueVar) ||
                is.null(self$options$groupVar)) {

                return()
            }

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Get the data and variable names
            data <- self$data
            timeVar <- self$options$timeVar
            valueVar <- self$options$valueVar
            groupVar <- self$options$groupVar

            # Convert to numeric if needed
            data[[timeVar]] <- jmvcore::toNumeric(data[[timeVar]])
            data[[valueVar]] <- jmvcore::toNumeric(data[[valueVar]])

            # Remove NAs
            data <- jmvcore::naOmit(data)

            # Create streamgraph using correct variable references
            plot <- streamgraph::streamgraph(
                data = data,
                key = groupVar,
                value = valueVar,
                date = timeVar,
                offset = self$options$offset,
                interpolate = self$options$interpolate,
                width = self$options$width,
                height = self$options$height
            )

            # Add color palette
            plot <- streamgraph::sg_fill_brewer(plot, self$options$palette)

            # Convert to HTML
            html <- plot


            # Send to output
            self$results$StreamGraph$setContent(html)
        }
    )
)
