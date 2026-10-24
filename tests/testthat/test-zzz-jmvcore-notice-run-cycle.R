# A jmvcore::Notice inserted in .run() does NOT pile up across run cycles: every cycle builds a
# fresh results tree, and Group$fromProtoBuf() restores only items that tree already has. What
# DOES pile up is appending to a declared Html item's content, which is restored each cycle.
# Our 2026-09-22 OncoPath response claimed the opposite without measuring it; the reviewer
# disputed it on 2026-10-23. Engine approximation: new analysis object per cycle + fromProtoBuf
# of the previous cycle's protobuf. Guide section 13.

test_that("run-time Notice does not accumulate across cycles; appended Html content does", {
  skip_if_not_installed("RProtoBuf")
  ns <- asNamespace("jmvcore")
  if (is.null(get0("RProtoBuf_new", envir = ns))) {
    testthat::local_mocked_bindings(RProtoBuf_new = RProtoBuf::new,
      RProtoBuf_serialize = RProtoBuf::serialize, RProtoBuf_read = RProtoBuf::read,
      .package = "jmvcore")
  }
  getFromNamespace("initProtoBuf", "jmvcore")()

  Res <- R6::R6Class("Res", inherit = jmvcore::Group, public = list(
    initialize = function(options) {
      super$initialize(options = options, name = "", title = "T")
      self$add(jmvcore::Html$new(options, name = "html"))
    }))
  A <- R6::R6Class("A", inherit = jmvcore::Analysis,
    public = list(mode = "notice", initialize = function(options) {
      super$initialize(package = "x", name = "x", version = c(1, 0, 0), options = options,
                       results = Res$new(options = options), data = NULL)
    }),
    private = list(.run = function() {
      if (self$mode == "notice") {
        n <- jmvcore::Notice$new(self$options, name = "n1", type = jmvcore::NoticeType$WARNING)
        n$setContent("hello")
        self$results$insert(1, n)
      } else {
        h <- self$results$get("html")
        h$setContent(paste0(h$content, "<p>hello</p>"))
      }
    }))

  cycles <- function(mode) {
    pb <- NULL
    for (i in 1:3) {
      a <- A$new(jmvcore::Options$new())
      a$mode <- mode
      a$.setReadDatasetHeaderSource(function(...) data.frame())
      a$.setReadDatasetSource(function(...) data.frame())
      a$init()
      if (!is.null(pb)) a$results$fromProtoBuf(pb, list(), list())
      a$.__enclos_env__$private$.run()
      pb <- a$results$asProtoBuf()
    }
    a$results
  }

  r <- cycles("notice")
  expect_identical(names(r$items), c("n1", "html"))
  r <- cycles("html")
  expect_identical(lengths(regmatches(r$get("html")$content, gregexpr("hello", r$get("html")$content))), 3L)
})
