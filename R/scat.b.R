
scatClass <- R6::R6Class(
    "scatClass",
    inherit = scatBase,
    private = list(
        .init = function() {
            
            image <- self$results$scat
            
            size <- private$.plotSize()
            image$setSize(size$width, size$height)
            
        },
        .run = function() {
            
            x <- self$options$x
            y <- self$options$y
            g <- self$options$group
            
            if ( ! is.null(x) && ! is.null(y)) {
                
                xCol <- jmvcore::toNumeric(self$data[[x]])
                yCol <- jmvcore::toNumeric(self$data[[y]])
                gCol <- if (is.null(g)) rep("var", length(xCol)) else factor(self$data[[g]])
                
                data <- data.frame(x=xCol, y=yCol, g=gCol)
                
                data <- jmvcore::naOmit(data)
                
                image <- self$results$scat
                image$setState(data)
                
            }
        },
        .scat = function(image, ggtheme, theme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            data <- image$state
            marg <- self$options$marg
            line <- self$options$line
            method <- if (line == 'linear') 'lm' else 'auto'
            
            base::suppressMessages({
                base::suppressWarnings({
                    
                    p <- ggplot2::ggplot(data, ggplot2::aes(x=x, y=y, color=g, fill=g)) + 
                        ggplot2::geom_point(alpha=.8, size=2.5) + ggtheme +
                        ggplot2::labs(x=self$options$x, y=self$options$y, fill=self$options$group, color=self$options$group)
                    
                    if (line != 'none')
                        p <- p + ggplot2::geom_smooth(method = method, se = self$options$se)
                    
        
                    colors <- NULL
                    if (is.null(self$options$group)) {
                        colors <- list(ggplot2::scale_color_manual(values=theme$color[1]),
                                       ggplot2::scale_fill_manual(values=theme$fill[2]), 
                                       ggplot2::scale_shape_manual(values=21))
                        p <- p + ggplot2::theme(legend.position = 'none') + colors
                    }
                    
                    if (marg == 'dens') {
                        xdens <- cowplot::axis_canvas(p, axis='x') +
                            ggridges::geom_ridgeline(data=data, ggplot2::aes(x, y=0, height=..density.., fill=g),
                                                  stat='density', alpha=0.5, size=.2, trim=FALSE) + ggtheme + colors 
                        
                        ydens <- cowplot::axis_canvas(p, axis='y') +
                            ggridges::geom_vridgeline(data=data, ggplot2::aes(x=0, y=y, width=..density.., fill=g),
                                                   stat='ydensity', alpha=0.5, size=.2, trim=FALSE) + ggtheme + colors
                        
                        p <- cowplot::insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position="top")
                        p <- cowplot::insert_yaxis_grob(p, ydens, grid::unit(.2, "null"), position="right")
                        
                        p <- cowplot::ggdraw(p)    
                    
                    } else if (marg == 'box') {
                        
                        themeBox <- ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                                   panel.grid.minor = ggplot2::element_blank(),
                                                   strip.background = ggplot2::element_rect(fill='transparent', color=NA),
                                                   panel.background=ggplot2::element_rect(fill='transparent', color=NA))
                        
                        xdens <- ggplot2::ggplot() +
                            ggplot2::geom_boxplot(data=data, ggplot2::aes(x=g, y=x, fill=g, color=g), position=ggplot2::position_dodge(0.8),
                                                  width=0.5, alpha=0.9, notch=TRUE) + ggtheme + themeBox + colors +
                            ggplot2::coord_flip()
                        
                        ydens <- ggplot2::ggplot() +
                            ggplot2::geom_boxplot(data=data, ggplot2::aes(x=g, y=y, fill=g, color=g), position=ggplot2::position_dodge(0.8),
                                                  width=0.5, alpha=0.9, notch=TRUE) + ggtheme + themeBox + colors
                        
                        nLevels <- length(levels(data$g))
                        
                        p <- cowplot::insert_xaxis_grob(p, xdens, grid::unit(.05 * nLevels, "null"), position="top")
                        p <- cowplot::insert_yaxis_grob(p, ydens, grid::unit(.05 * nLevels, "null"), position="right")
                        
                        p <- cowplot::ggdraw(p)
                    }
            
                    return(p)    
                })
            })
        },
        .plotSize = function() {
            
            g <- self$options$group
            
            marg <- 0
            nLevels <- 1
            legend <- 0
            
            x <- 826.5 / 700
            
            title <- 47 / x
            ticks <- 35 / x
            xaxis <- 516 / x
            yaxis <- 450 / x
            
            if ( ! is.null(g)) {
                
                levels <- levels(self$data[[g]])
                nLevels <- length(levels)
                nCharLevels <- max(nchar(levels))
                nCharName <- as.numeric(nchar(g))
                
                preLegend <- 18 / x
                legendTitle <- (10 * nCharName) / x
                preIcon <- 7 / x
                icon <- 10 / x
                postIcon <- 9 / x
                legendLevels <- (7.5 * nCharLevels) / x
                
                legend <- max(preLegend + preIcon + icon + postIcon + legendLevels, preLegend + legendTitle)
                
            }
            
            marg <- 0
            if (self$options$marg == 'box') {
                box <- 26 / x
                marg <- nLevels * box
            } else if (self$options$marg == 'dens') {
                marg <- 26 / x * 4
            }

            post <- 28.5 / x
            
            width <- title + ticks + xaxis + marg + legend + post
            height <- title + ticks + yaxis + marg + post
            
            return(list(width=width, height=height))
            
        })
)
