test_that("categorical unanimity counts cases rather than simplified table elements", {
  a <- rep(c("N", "P"), 20)
  b <- rep(c("P", "N"), 20)
  for (encode in list(identity, factor, function(x) as.integer(x == "P"))) {
    for (third in c(FALSE, TRUE)) {
      d <- data.frame(a = encode(a), b = encode(b))
      if (third) d$c <- encode(a)
      r <- suppressWarnings(agreement(data = d, vars = names(d)))$irrtable
      expect_equal(r$asDF$peragree[1], 0)
      if (!third) expect_equal(r$asDF$kappa[1], -1)
      notes <- paste(vapply(r$notes, function(n) n$note, ""), collapse = " ")
      expect_false(grepl("Unexpected agreement", notes, fixed = TRUE))
    }
  }
  d <- data.frame(a = c("N", "P", "P", NA),
                  b = c("N", "N", "P", "P"),
                  c = c("N", "P", "P", "P"))
  r <- suppressWarnings(agreement(data = d, vars = names(d)))$irrtable
  expect_equal(r$asDF$peragree[1], 200 / 3)
  d <- data.frame(a = c("N", "P", NA), b = c(NA, "N", "P"),
                  c = c("N", NA, "P"))
  r <- suppressWarnings(agreement(data = d, vars = names(d)))$irrtable
  expect_true(is.na(r$asDF$peragree[1]))
  expect_match(paste(vapply(r$notes, function(n) n$note, ""), collapse = " "),
               "no case was", fixed = TRUE)
})
