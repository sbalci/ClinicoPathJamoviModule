#' @importFrom magrittr %>%
manyttestsISClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "manyttestsISClass",
    inherit = manyttestsISBase,
    private = list(
        ### Member variables ----
        .groups = NULL,
        .comparisons = NULL,
        
        #### Init + run functions ----
        .init = function() {
            
            private$.initTable()
            
        },
        .run = function() {
            
            if (is.null(self$options$dep) || length(self$options$groups) == 0)
                return()
            
            results <- private$.compute()
            
            private$.populateTable(results)
        },
        
        #### Compute results ----
        .compute = function() {
            
            dep <- self$options$dep
            vars <- self$options$groups
            
            levels <- self$groups %>%
                tidyr::unite(group, vars, sep = '...') %>%
                .[['group']]
            
            data <- self$data %>%
                tibble::as_tibble() %>%
                dplyr::select(c(dep, vars)) %>%
                tidyr::unite(group, vars, sep = '...') %>%
                dplyr::mutate(group = factor(group, levels))
            
            results <- private$.ttests(data)
            
            return(results)
        },
        
        #### Init table ----
        .initTable = function() {
            
            table <- self$results$tests
            groupNames <- self$options$groups
            corMethod <- self$options$corMethod
            
            for (i in 1:2) {
                for (name in groupNames) {
                    table$addColumn(
                        name=paste0(name, i), 
                        title=name, type='text', 
                        superTitle=paste('Group', i),
                        combineBelow=i==1)
                }

                table$addColumn(
                    name=paste0('n', i), 
                    title='N', 
                    type='number', 
                    superTitle=paste('Group', i),
                    visible="(n)")
            }
            
            ciTitle <- paste0(self$options$ciWidth, '% Confidence Interval')
            ciTitleES <- paste0(self$options$ciWidthES, '% Confidence Interval')
            
            table$addColumn(name='t', title='t', type='number')
            table$addColumn(name='df', title='df', type='number')
            
            cor <- NULL
            sub <- ''
            if (corMethod == 'holm') {
                cor <- 'Holm'
                sub <- '<sub>holm</sub>'
            } else if (corMethod == 'hochberg') {
                cor <- 'Hochberg'
                sub <- '<sub>hoch</sub>'
            } else if (corMethod == 'hommel') {
                cor <- 'Hommel'
                sub <- '<sub>homm</sub>'
            } else if (corMethod == 'bonferroni') {
                cor <- 'Bonferroni'
                sub <- '<sub>bonf</sub>'
            } else if (corMethod == 'BH') {
                cor <- 'Benjamini & Hochberg'
                sub <- '<sub>BH</sub>'
            } else if (corMethod == 'BY') {
                cor <- 'Benjamini & Yekutieli'
                sub <- '<sub>BY</sub>'
            }
            
            table$addColumn(name='p', title=jmvcore::format('p{}', sub), type='number', format='zto,pvalue')
                
            if (! is.null(cor))
                table$setNote('pcor', jmvcore::format('{} corrected p-value', cor))
            
            table$addColumn(name='md', title='Mean difference', type='number', visible="(meanDiff)")
            table$addColumn(name='cil', title='Lower', type='number', visible="(meanDiff && ci)", superTitle=ciTitle)
            table$addColumn(name='ciu', title='Upper', type='number', visible="(meanDiff && ci)", superTitle=ciTitle)
            table$addColumn(name='es', title='Cohen\'s d', type='number', visible="(effectSize)")
            table$addColumn(name='ciles', title='Lower', type='number', visible="(effectSize && ciES)", superTitle=ciTitleES)
            table$addColumn(name='ciues', title='Upper', type='number', visible="(effectSize && ciES)", superTitle=ciTitleES)
            
            hypothesis <- self$options$hypothesis
            flag <- self$options$flag
            if (hypothesis == 'oneGreater') {
                
                table$setNote("hyp", "H\u2090 Group 1 > Group 2")
                if (flag)
                    table$setNote('flag', '* p < .05, ** p < .01, *** p < .001, one-tailed')
                
            } else if (hypothesis == 'twoGreater') {
                
                table$setNote("hyp", "H\u2090 Group 1 < Group 2")
                if (flag)
                    table$setNote('flag', '* p < .05, ** p < .01, *** p < .001, one-tailed')
                
            } else {
                
                table$setNote("hyp", NULL)
                if (flag)
                    table$setNote('flag', '* p < .05, ** p < .01, *** p < .001')
                
            }
            
            groups <- self$groups
            
            if (is.null(groups))
                return()
            
            levels <- groups %>%
                tidyr::unite(group, groupNames, sep = '...') %>%
                .[['group']]
            nGroups <- nrow(groups)
            
            comp <- tibble::tibble(group1=character(), group2=character())
            for (i in 1:(nGroups-1)) {
                for (j in (i+1):nGroups) {
                    
                    row <- list()
                    for (name in groupNames) {
                        row[[paste0(name, 1)]] <- as.character(groups[i,name])
                        row[[paste0(name, 2)]] <- as.character(groups[j,name])
                    }
                    table$addRow(rowKey=paste0(i,j), row)
                    
                    comp <- comp %>%
                        tibble::add_row(group1=levels[i], group2=levels[j])
                }
            }
            private$.comparisons <- comp
        },
        
        #### Populate table ----
        .populateTable = function(results) {
            
            table <- self$results$tests
            flag <- self$options$flag

            for (i in 1:nrow(results)) {
                row <- as.list(results[i,])
                table$setRow(rowNo=i, values=row)
                
                if (flag && ! is.nan(row$p)) {
                    if (row$p < .001)
                        table$addSymbol(rowNo=i, col='p', '***')
                    else if (row$p < .01)
                        table$addSymbol(rowNo=i, col='p', '**')
                    else if (row$p < .05)
                        table$addSymbol(rowNo=i, col='p', '*')
                }
            }
        },
        
        #### Helper functions ----
        .ttests = function(data) {
            
            comp <- private$.comparisons
            dep <- self$options$dep
            vars <- self$options$groups
            confInt <- self$options$ciWidth / 100
            confIntES <- self$options$ciWidthES / 100
            
            if (self$options$hypothesis == 'oneGreater')
                Ha <- "greater"
            else if (self$options$hypothesis == 'twoGreater')
                Ha <- "less"
            else
                Ha <- "two.sided"
            
            ts <- tibble::tibble(group1=character(), group2=character(),
                                 n1=integer(), n2=integer(),
                                 t=numeric(), df=numeric(), p=numeric(), 
                                 md=numeric(), cil=numeric(), ciu=numeric(),
                                 es=numeric(), ciles=numeric(), ciues=numeric())            
            
            for (i in 1:nrow(comp)) {
                
                group1 <- data %>% 
                    dplyr::filter(group==!!as.character(comp[i,1])) %>%
                    .[[dep]]
                
                group2 <- data %>% 
                    dplyr::filter(group==!!as.character(comp[i,2])) %>%
                    .[[dep]]
                
                n1 <- length(group1)
                n2 <- length(group2)
                
                if (n1 < 1 || n2 < 1 || n1 + n2 <= 2) {
                    
                    ts <- ts %>% tibble::add_row(
                        group1=as.character(comp[i,1]), 
                        group2=as.character(comp[i,2]),
                        n1=!!n1, n2=!!n2, t=NaN, df=NaN, p=NaN, 
                        md=NaN, cil=NaN, ciu=NaN,
                        es=NaN, ciles=NaN, ciues=NaN)
                    
                } else {
                    
                    es <- effsize::cohen.d(group1, group2, conf.level=confIntES)

                    test <- t.test(group1, group2, var.equal = TRUE, 
                                   alternative=Ha, conf.level=confInt)
                    
                    ts <- ts %>% tibble::add_row(
                        group1=as.character(comp[i,1]), 
                        group2=as.character(comp[i,2]),
                        n1=!!n1, n2=!!n2, 
                        t=test$statistic, df=test$parameter, p=test$p.value,
                        md=test$estimate[1]-test$estimate[2],
                        cil=test$conf.int[1], ciu=test$conf.int[2],
                        es=es$estimate,
                        ciles=!!es$conf.int[1], ciues=!!es$conf.int[2])
                }
            }
            
            if (self$options$corMethod != 'none') {
                ts <- ts %>%
                    dplyr::mutate(p = stats::p.adjust(p, method = self$options$corMethod))
            }

            # ts <- ts %>% 
            #     tidyr::separate(group1, paste(vars, 1, sep='_'), sep="\\.{3}") %>%
            #     tidyr::separate(group2, paste(vars, 2, sep='_'), sep="\\.{3}")
            
            ts <- ts %>% dplyr::select(-c(group1, group2))
            
            return(ts)
        }
    ),
    active = list(
        ### Active binding for groups ----
        groups = function(value) {
            if (missing(value)) {

                if (is.null(private$.groups)) {
                    
                    groups <- self$options$groups
                    
                    if (length(groups) == 0)
                        return(NULL)
                    
                    levels <- list()
                    for (group in groups)
                        levels[[group]] <- levels(self$data[[group]])
                    
                    private$.groups <- rev(expand.grid(rev(levels)))
                    
                }
                    
                return(private$.groups)
            }
        }
    )
)
