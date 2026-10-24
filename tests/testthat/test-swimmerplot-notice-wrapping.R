test_that("long swimmer notices wrap and escape user text", {
  analysis <- ClinicoPath:::swimmerplotClass$new(
    options = ClinicoPath:::swimmerplotOptions$new(), data = data.frame()
  )
  private <- analysis$.__enclos_env__$private
  user_text <- paste0('<img src=x onerror=alert(1)>', strrep("longname", 100))
  private$.addNotice("WARNING", "Unreadable status", user_text)

  notice <- analysis$results$notices
  expect_s3_class(notice, "Html")
  # the shared renderer (R/utils-notices.R, library-audit 2026-10-23 OncoPath [LOW]) keeps newlines
  # and breaks long unbroken text
  expect_match(notice$content, "white-space: pre-line", fixed = TRUE)
  expect_match(notice$content, "overflow-wrap: anywhere", fixed = TRUE)
  expect_false(grepl("<img", notice$content, fixed = TRUE))
  expect_match(notice$content, "&lt;img", fixed = TRUE)
  expect_match(notice$content, strrep("longname", 100), fixed = TRUE)

  private$.noticeList <- list()
  private$.renderNotices()
  expect_identical(notice$content, "<div></div>")
})
