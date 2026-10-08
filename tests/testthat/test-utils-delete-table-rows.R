# Regression test for .deleteTableRows() (R/utils.R), used by agreement and pathsampling.
#
# jmvcore's Table$deleteRows() leaves the private row names behind, so a table emptied
# after holding rows fails $asDF with "invalid 'row.names' length". The helper resets
# them through a private field. If a jmvcore release renames that field, the write
# lands nowhere and this test is what notices.

test_that(".deleteTableRows() leaves an emptied table consistent and reusable", {
    tbl <- jmvcore::Table$new(jmvcore::Options$new(), name = "t", rows = 0)
    tbl$addColumn(name = "a", type = "text")
    tbl$addColumn(name = "b", type = "number")
    tbl$addRow("r1", list(a = "x", b = 1))
    tbl$addRow("r2", list(a = "y", b = 2))

    .deleteTableRows(tbl)

    expect_equal(tbl$rowCount, 0)
    expect_length(tbl$rowKeys, 0)
    expect_length(tbl$names, 0)
    expect_equal(nrow(tbl$asDF), 0)

    # Refilling after the reset behaves like a fresh table.
    tbl$addRow("r3", list(a = "z", b = 3))
    expect_equal(tbl$asDF$a, "z")
    expect_length(tbl$names, 1)
})

test_that("no analysis calls Table$deleteRows() directly", {
    # 269 direct calls in 70 files were moved to .deleteTableRows() on 2026-10-06. Parsed,
    # not grepped, so the comments that explain the bug do not count as calls.
    r_dir <- file.path(normalizePath("../..", mustWork = FALSE), "R")
    skip_if_not(dir.exists(r_dir), "package R/ sources not available")
    files <- setdiff(list.files(r_dir, pattern = "\\.R$", full.names = TRUE),
                     file.path(r_dir, "utils.R"))
    calls <- unlist(lapply(files, function(f) {
        pd <- utils::getParseData(parse(f, keep.source = TRUE))
        hit <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "deleteRows", ]
        if (nrow(hit)) paste0(basename(f), ":", hit$line1)
    }))
    expect(length(calls) == 0,
           paste("use .deleteTableRows(table) instead of table$deleteRows():",
                 paste(calls, collapse = ", ")))
})
