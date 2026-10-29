# Regression test for .footnoteMissingCells() (R/utils.R), used by survival, singlearm,
# survivalcont and multisurvival.
#
# jmvcore sends a numeric NA cell as a double and jamovi prints "NaN"; only "" is blank.
# A not-reached median is ordinary in clinical data, so every such cell has to be blanked
# and explained.

test_that(".footnoteMissingCells() blanks NA cells, footnotes them once, leaves values alone", {
    tbl <- jmvcore::Table$new(jmvcore::Options$new(), name = "t", rows = 0)
    tbl$addColumn(name = "label", type = "text")
    tbl$addColumn(name = "median", type = "number")
    tbl$addColumn(name = "lcl", type = "number")
    tbl$addColumn(name = "ucl", type = "number")
    row <- list(label = "A", median = NA_real_, lcl = 4.2, ucl = NA)
    tbl$addRow(rowKey = "A", values = row)

    .footnoteMissingCells(tbl, "A", row, "Not reached.")

    expect_identical(tbl$getCell(rowKey = "A", col = "median")$value, "")
    expect_identical(tbl$getCell(rowKey = "A", col = "ucl")$value, "")
    expect_equal(tbl$getCell(rowKey = "A", col = "lcl")$value, 4.2)
    expect_identical(tbl$getCell(rowKey = "A", col = "label")$value, "A")
    expect_identical(tbl$getCell(rowKey = "A", col = "median")$footnotes, "Not reached.")
    expect_length(tbl$getCell(rowKey = "A", col = "lcl")$footnotes, 0)
})
