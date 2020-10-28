#' @title Find Retracted Papers from DOI
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


retractedClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "retractedClass",
    inherit = retractedBase,
    private = list(
        .run = function() {


            if (is.null(self$options$doi))
                return()

            mydata <- self$data

            doi <- self$options$doi

            doi_list <- mydata[[doi]]

            doi_list <- as.vector(doi_list)

            # self$results$text$setContent(
            #     list(
            #         doi_list,
            #         typeof(doi_list),
            #         class(doi_list)
            #         )
            # )

            # retracted_list <- list() #create an empty list
            #
            # for (i in 1:length(doi_list)) {
            #
            #     retracted_list[[i]] <-
            #         retractcheck::retractcheck(
            #         dois = doi_list[i],
            #         database = "or",
            #         # database = "rw",
            #         return = "all"
            #         # return = "unique"
            #     )
            # }
            # retracted_list <- do.call("rbind",retracted_list)

            # retracted_list <-
            #     retractcheck::retractcheck(
            #         dois = doi_list,
            #         database = "or",
            #         # database = "rw",
            #         return = "all"
            #         # return = "unique"
            #     )
            #
            # table_retracted_list <- kableExtra::kable(
            #     retracted_list,
            #     format = "html"
            #     )



            # self$results$text2$setContent(table_retracted_list)



            # info_list <-
            # RefManageR::GetBibEntryWithDOI(doi = doi_list)
            #
            #
            # table_info_list <- kableExtra::kable(
            #     info_list,
            #     format = "html"
            # )



            # self$results$text3$setContent(table_info_list)



            ids <- rcrossref::id_converter(x = doi_list, type = "doi")

            table_ids <- ids[["records"]]

            table_table_ids <- kableExtra::kable(
                table_ids,
                format = "html"
            )


            self$results$text4$setContent(table_table_ids)

            pmids1 <- ids[["records"]][["pmid"]]


            pmids1 <- as.vector(pmids1)

            self$results$text5$setContent(pmids1)



            if (! is.null(self$options$resids)) {

                pmids2 <- ids[["records"]][["pmid"]]

                pmids2 <- as.vector(pmids2)
                pmids2 <- as.factor(pmids2)
            }

            if ( ! is.null(self$options$resids)) {
                self$results$resids$setValues(pmids2)

            }

            self$results$resids2$setValues(pmids2)






        })
)
