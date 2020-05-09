#' Tables via gtsummary
#'
#' @return
#' @export
#'
#' @import gtsummary

gtsummaryClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "gtsummaryClass",
    inherit = gtsummaryBase,
    private = list(
        .run = function() {


            # gtsummary

            # myvars <- jmvcore::constructFormula(terms = self$options$vars)
            # myvars <- jmvcore::decomposeFormula(formula = myvars)
            # myvars <- unlist(myvars)
            # mytableone2 <- self$data %>%
            #     dplyr::select(myvars)
            # mytableone2 <- gtsummary::tbl_summary(mytableone2)
            # self$results$text2$setContent(mytableone2)


            # trial <- gtsummary::trial

            gtsum1 <-
                gtsummary::tbl_summary(data = iris)

            # gtsum1 <- trial[c("trt", "age", "grade")] %>%
            #    gtsummary::tbl_summary(data = ., by = trt, missing = "no") %>%
            #    gtsummary::modify_header(stat_by =
            #         gt::md("**{level}** N =  {n} ({style_percent(p)}%)")) %>%
            #     gtsummary::add_n() %>%
            #     gtsummary::bold_labels() %>%
            #     gtsummary::as_gt() %>%
            #     gt::tab_spanner(columns = gt::starts_with("stat_"),
            #                     gt::md("**Chemotherapy Treatment**"))


            gtsum1 <- gtsummary::as_kable_extra(gtsum1)

            # gtsum1 <- gtsummary::as_gt(gtsum1)

            # gtsum1 <- gtsummary::as_flextable(gtsum1)

            # gtsum1 <- gtsummary::as_kable(gtsum1)


            # gtsum1 <- kableExtra::kable(gtsum1, format = "html")

            self$results$gtsum1$setContent(gtsum1)

            # self$results$gtsum1$setContent(gtsum1)


            # self$results$gtsum1pre$setContent(gtsum1)





            # gtsum1html <- knitr::kable(x = gtsum1, format = "html")
            #
            # self$results$gtsum1html$setContent(gtsum1html)

            # TRUE

        })
)
