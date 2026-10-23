# A transposed table needs CI row labels; column supertitles render over empty space.
agr_ci_heading_env <- function() {
    root <- if (file.exists("R/agreement.b.R")) "." else "../.."
    e <- new.env(parent = asNamespace("jmvcore"))
    for (x in parse(file.path(root, "R/utils.R"))) {
        if (is.call(x) && identical(x[[1]], as.name("<-")) &&
            identical(x[[2]], as.name(".fmt"))) eval(x, e)
    }
    sys.source(file.path(root, "R/agreement.h.R"), e)
    sys.source(file.path(root, "R/agreement.b.R"), e)
    e
}

test_that("transposed headline CI rows carry the selected confidence level", {
    e <- agr_ci_heading_env()
    for (level in c(.95, .90, .80)) {
        o <- e$agreementOptions$new(confLevel = level)
        a <- e$agreementClass$new(options = o, data = data.frame())
        a$init()
        for (bound in c("lower", "upper")) {
            col <- a$results$irrtable$getColumn(paste0("ci_", bound))
            expect_identical(col$title, sprintf("%s%% CI %s", round(level * 100),
                if (bound == "lower") "Lower" else "Upper"))
            expect_true(is.null(col$superTitle) || identical(col$superTitle, ""))
        }
    }
})
