# from https://github.com/raviselker/ppv


ppvClass <- R6::R6Class(
    "ppvClass",
    inherit = ppvBase,
    private = list(
        .init = function() {

            private$.initConfusionTable()

        },
        .run = function() {

            results <- private$.compute()
            private$.populateConfusionTable(results)
            private$.populatePpv(results)
            private$.prepareDotPlot(results)

        },

        #### Compute results ----
        .compute = function() {

            percTrue <- self$options$percTrue
            alpha <- self$options$alpha
            power <- self$options$power
            percHack <- self$options$percHack

            suppressWarnings({

                c <- 500		# number of studies in plots
                nrow <- 25
                ncol <- c/nrow
                df <- expand.grid(x = c(nrow:1), y = c(1:ncol))

                # compute prestudy odds of true relationships true/false
                R <- percTrue / (100 - percTrue)
                #if (R==0) R <- 0.001
                if (is.infinite(R)) R <- 100000

                # beta Fehler
                beta <- 1 - power
                # bias
                u <- percHack/100		# Ioannidis calls it "u"

                # Hits
                hit <- (c*power*R + u*c*beta*R) / (R + 1)
                falseAlarm <- (c*alpha + u*c*(1 - alpha)) / (R + 1)
                miss <- (1-u)*c*beta*R / (R + 1)
                trueRejection <- (1-u)*c*(1-alpha) / (R + 1)

                # positive predictive value
                ppv <- (power * R + u*beta*R) / (R + alpha - beta*R + u - u*alpha + u*beta*R)
                fdr <- 1-ppv

                cells <- c('hit'=hit, 'falseAlarm'=falseAlarm, 'miss'=miss, 'trueRejection'=trueRejection)
                dec <- numeric(4)

                for (i in seq_along(cells)) {
                    if ( ! private$.isInteger(cells[i])) {
                        dec[i] <- cells[i] - trunc(cells[i])
                        cells[i] <- trunc(cells[i])
                    }
                }

                if (sum(dec > 0) > 0) {
                    left <- c - sum(cells)
                    decCells <- which(dec > 0)
                    probCells <- dec[decCells] / sum(dec[decCells])

                    plusOne <- private$.sample(decCells, left, replace = TRUE, prob = probCells)
                    for (i in plusOne) {
                        cells[i] <- cells[i] + 1
                    }
                }

                type <- c(rep("True positive", cells[1]), rep("False positive", cells[2]),
                          rep("True negative", cells[4]), rep("False negative", cells[3]))

                # combine types
                df$type <- factor(type, levels=c("True positive", "False positive", "True negative", "False negative"))

            }) # suppressWarnings

            return(list('hit'=hit/c, 'falseAlarm'=falseAlarm/c, 'miss'=miss/c, 'trueRejection'=trueRejection/c,
                        ppv=ppv, fdr=fdr, df=df))
        },

        #### Init  <- s ----
        .initConfusionTable = function() {

            table <- self$results$confusion

            table$addFormat(col='true[neg]', rowNo=1, jmvcore::Cell.NEGATIVE)
            table$addFormat(col='false[pos]', rowNo=1, jmvcore::Cell.NEGATIVE)
        },

        #### Populate tables ----
        .populateConfusionTable = function(results) {

            table <- self$results$confusion

            row <- list()
            row[['true[pos]']] <- results$hit * 100
            row[['false[pos]']] <- results$falseAlarm * 100
            row[['total[pos]']] <- (results$hit + results$falseAlarm) * 100
            row[['true[neg]']] <- results$miss * 100
            row[['false[neg]']] <- results$trueRejection * 100
            row[['total[neg]']] <- (results$miss + results$trueRejection) * 100
            row[['true[total]']] <- (results$hit + results$miss) * 100
            row[['false[total]']] <- (results$falseAlarm + results$trueRejection) * 100
            row[['total[total]']] <- (results$falseAlarm + results$trueRejection + results$hit + results$miss) * 100

            table$setRow(rowNo=1, values=row)

        },

        #### Populate html ----
        .populatePpv = function(results) {

            html <- self$results$ppv

            ppv <- paste0("<p><b>Positive Predictive Value (PPV)</b>: ", round(results$ppv*100,2), "% of claimed findings are true, ",
                          round(results$hit*100,2)," / (", round(results$hit*100,2), " + ", round(results$falseAlarm*100,2), ")</p>")
            fdr <- paste0("<p><b>False Discovery Rate (FDR)</b>: ", round(results$fdr*100,2), "% of claimed findings are false, ",
                          round(results$falseAlarm*100,2)," / (", round(results$hit*100,2), " + ", round(results$falseAlarm*100,2), ")</p>")

            html$content <- paste0(ppv, fdr)

            # stylesheet <- "p {margine:0px;}"
            # html$stylesheets <- stylesheet

        },

        #### Plot functions ----
        .prepareDotPlot = function(results) {

            image <- self$results$dotPlot

            image$setState(results$df)
        },
        .dotPlot = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            themeSpec <- ggplot2::theme(
                legend.position = 'top',
                legend.background = ggplot2::element_rect("transparent"),
                legend.key = ggplot2::element_blank(),
                legend.title = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank(),
                axis.title.x = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank(),
                axis.title.y = ggplot2::element_blank())

            p <- ggplot2::ggplot(data=image$state, ggplot2::aes(x=y, y=x, color=type, shape=type)) +
                ggplot2::geom_point(size=3) +
                # ggplot2::scale_colour_brewer(palette = "Set1") +
                ggplot2::scale_color_manual(values=c("#4DAF4A", "#E41A1C", "#4DAF4A", "#E41A1C"), drop = FALSE) +
                ggplot2::scale_shape_manual(values=c(15, 16, 2, 5), drop = FALSE) +
                ggtheme + themeSpec

            print(p)

            TRUE
        },

        #### Helper functions ----
        .isInteger = function(x) {

            if ( ! is.numeric(x))
                return(FALSE)

            if (round(x) == x)
                return(TRUE)
            else
                return(FALSE)
        },
        .sample = function(x, size, ...) {
            if (length(x) == 1)
                return(rep(x, size))
            else
                return(sample(x, size, ...))
        }),

    public=list(
        asSource=function() {

            paste0("This module does not support syntax mode yet.")

        })
)
