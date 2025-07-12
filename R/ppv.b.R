#' @title Positive Predictive Value Calculator
#' 
#' @description
#' This module calculates the Positive Predictive Value (PPV) and False Discovery Rate (FDR)
#' for research findings based on the framework described by Ioannidis (2005). It helps
#' researchers understand the probability that their claimed findings are actually true
#' given various study characteristics.
#' 
#' @details
#' The calculation is based on Bayes' theorem and considers:
#' - Prior probability of true relationships (percentage of a priori true hypotheses)
#' - Type I error rate (alpha level)
#' - Statistical power (1 - beta)
#' - Proportion of p-hacked or biased studies
#' 
#' PPV = (Power × R + u × β × R) / (R + α - β × R + u - u × α + u × β × R)
#' where R is the pre-study odds of true relationships (percTrue/(100-percTrue))
#' and u is the bias factor (percHack/100)
#' 
#' @references
#' Ioannidis, J. P. (2005). Why most published research findings are false. 
#' PLoS medicine, 2(8), e124.
#' 
#' Adapted from https://github.com/raviselker/ppv
#' 
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual scale_shape_manual theme element_blank element_rect

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

            # Input validation
            if (percTrue == 0) {
                jmvcore::reject("Percentage of true hypotheses cannot be 0%")
            }
            if (percTrue == 100 && percHack > 0) {
                jmvcore::reject("Cannot have p-hacking when all hypotheses are true (100%)")
            }

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

            # Calculate actual numbers from percentages
            truePositives <- round(results$hit * 100, 2)
            falsePositives <- round(results$falseAlarm * 100, 2)
            totalPositives <- truePositives + falsePositives
            
            ppv_percentage <- round(results$ppv * 100, 2)
            fdr_percentage <- round(results$fdr * 100, 2)

            # Create comprehensive interpretation
            interpretation <- ""
            if (ppv_percentage < 50) {
                interpretation <- "Most claimed findings are likely to be false positives."
            } else if (ppv_percentage < 75) {
                interpretation <- "A substantial proportion of claimed findings may be false."
            } else {
                interpretation <- "Most claimed findings are likely to be true."
            }

            # Build HTML content with enhanced formatting
            content <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='margin-top: 0;'>Results Summary</h4>",
                "<p><b>Positive Predictive Value (PPV)</b>: ", ppv_percentage, "%</p>",
                "<p style='margin-left: 20px; color: #666;'>",
                "Out of ", round(totalPositives, 1), " positive findings, ",
                round(truePositives, 1), " are expected to be true.",
                "</p>",
                "<p><b>False Discovery Rate (FDR)</b>: ", fdr_percentage, "%</p>",
                "<p style='margin-left: 20px; color: #666;'>",
                "Out of ", round(totalPositives, 1), " positive findings, ",
                round(falsePositives, 1), " are expected to be false.",
                "</p>",
                "</div>",
                
                "<div style='background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='margin-top: 0;'>Interpretation</h4>",
                "<p>", interpretation, "</p>",
                "</div>",
                
                "<div style='background-color: #fff3cd; padding: 15px; border-radius: 5px;'>",
                "<h4 style='margin-top: 0;'>Study Parameters Used</h4>",
                "<ul style='margin: 5px 0;'>",
                "<li>Prior probability of true hypotheses: ", self$options$percTrue, "%</li>",
                "<li>Significance level (α): ", self$options$alpha, "</li>",
                "<li>Statistical power: ", self$options$power, "</li>",
                "<li>Percentage of p-hacked studies: ", self$options$percHack, "%</li>",
                "</ul>",
                "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
                "Based on the framework by Ioannidis (2005): Why most published research findings are false.",
                "</p>",
                "</div>"
            )

            html$content <- content

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
        #' @description
        #' Read dataset for PPV analysis
        #' @return NULL as this analysis doesn't require data
        readDataset=function() {
            # This analysis doesn't require data
            return(NULL)
        },
        
        #' @description
        #' Generate R source code for PPV analysis
        #' @return Character string indicating syntax mode is not supported
        asSource=function() {

            paste0("This module does not support syntax mode yet.")

        })
)
