

# This file is a generated template, your changes will not be overwritten

pairchi2Class <- if (requireNamespace('jmvcore'))
    R6::R6Class("pairchi2Class",
                inherit = pairchi2Base,
                private = list(
                    .run = function() {
                        # prepare data ----

                        data <- self$data

                        row <- self$options$row
                        col <- self$options$col

                        row <- jmvcore::composeTerm(components = row)
                        col <- jmvcore::composeTerm(components = col)


                        data <- jmvcore::select(df = data,
                                                columnNames = c(row, col))


                        # conftable ----

                        conftable <- table(data[[row]], data[[col]],
                                            dnn = list(row, col))


                        self$results$conftable$setContent(conftable)


                        # chisq.test ----


                        chi2test <- try(chisq.test(conftable, correct = FALSE))

                        # chi2test
                        # chi2test[["statistic"]]
                        # chi2test[["statistic"]][["X-squared"]]
                        # chi2test[["parameter"]]
                        # chi2test[["parameter"]][["df"]]
                        # chi2test[["p.value"]]
                        # chi2test[["method"]]
                        # chi2test[["data.name"]]
                        # chi2test[["observed"]]
                        # chi2test[["expected"]]
                        # chi2test[["residuals"]]
                        # chi2test[["stdres"]]


                        self$results$chi2test$setContent(chi2test)



                        # pairwise function ------

                        # RVAideMemoire::chisq.multcomp()
                        # RVAideMemoire::fisher.multcomp()

                        # rmngb::pairwise.chisq.test(x, ...)
                        # rmngb::pairwise.fisher.test(x, ...)


                        # pairwise function 1 ------


                        pairwiseRVAideMemoire <- RVAideMemoire::chisq.multcomp(conftable
                                                                               # , p.method = "fdr"
                                                                               )

                        p_pairwiseRVAideMemoire <- pairwiseRVAideMemoire[["p.value"]]

                        # tab_pairwiseRVAideMemoire <-
                        #     as.data.frame(pairwiseRVAideMemoire) %>%
                        #     tibble::rownames_to_column()

                        # tab_pairwiseRVAideMemoire <-
                        #     tab_pairwiseRVAideMemoire %>%
                        #     tidyr::pivot_longer(cols = -rowname) %>%
                        #     dplyr::filter(complete.cases(.))

                        pairwise1 <- list(
                            "RVAideMemoire" = pairwiseRVAideMemoire,
                            "p-value" = p_pairwiseRVAideMemoire
                            # ,
                            # "table" = tab_pairwiseRVAideMemoire
                        )


                        self$results$pairwise1$setContent(pairwise1)


                        # pairwise function 2 ------


                        pairwisermngb <- rmngb::pairwise.chisq.test.table(conftable)

                        p_pairwisermngb <- pairwisermngb[["p.value"]]



                        # tab_pairwisermngb <-
                        #     as.data.frame(pairwisermngb) %>%
                        #     tibble::rownames_to_column()


                        # tab_pairwisermngb <-
                        #     tab_pairwisermngb %>%
                        #     tidyr::pivot_longer(cols = -rowname) %>%
                        #     dplyr::filter(complete.cases(.))

                        pairwise2 <- list("rmngb" = pairwisermngb,
                                          "p-value" = p_pairwisermngb
                                          # ,
                                          # "table" = tab_pairwisermngb
                                          )

                        self$results$pairwise2$setContent(pairwise2)


                        # pairwise function 3 ------

                        pairwise3 <- chisq.posthoc.test::chisq.posthoc.test(conftable)

                        self$results$pairwise3$setContent(pairwise3)


                    }

                ))
