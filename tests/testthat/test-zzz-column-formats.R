# A table column's `format:` is a COMMA-separated list of exact tokens. Both sides of jamovi
# parse it the same way and both do exact membership on the split result:
#
#   jmvcore, Column$initialize:  private$.format <- strsplit(format, ",", fixed = TRUE)[[1]]
#                                zto <- ("zto" %in% private$.format)
#   jamovi client:               let w = []; if (I !== "") w = I.split(",");
#                                ... w.includes("zto") ... w.includes("pvalue") ...
#
# So `format: zto:4` is ONE token spelled "zto:4". `"zto" %in% "zto:4"` is FALSE, the leading-zero
# and no-scientific-notation behaviour is lost, and NOTHING reports it -- no compile error, no
# warning, no visible difference in the .r.yaml. The column just renders unformatted.
#
# 519 columns in this module were written that way before anyone noticed, 447 of them in the
# `zto:N` family, because vignettes/jamovi_tables_guide.md documented the tokens and never named
# the separator. See review guide section 21 and the tables guide, "The token grammar".
#
# Shipped analyses must be clean. Unshipped (menuGroup suffixed D/P/T) are promotion debt and are
# reported, not failed -- `python3 tools/release_gate.py` tracks the count.

ROOT <- normalizePath("../..")

VALID <- c("zto", "pvalue", "pc", "log10")
valid_token <- function(t) t %in% VALID || grepl("^(dp|sf|pc):[0-9]+$", t)

parse_format <- function(x) trimws(strsplit(x, ",", fixed = TRUE)[[1]])

ryamls <- list.files(file.path(ROOT, "jamovi"), pattern = "\\.r\\.yaml$", full.names = TRUE)
skip_if(length(ryamls) == 0, "no jamovi/*.r.yaml in this tree")

# menuGroup suffix D (draft) / P (pending) / T (JamoviTest) means the analysis never reaches a user
is_shipped <- function(analysis) {
    a <- file.path(ROOT, "jamovi", paste0(analysis, ".a.yaml"))
    if (!file.exists(a)) return(FALSE)
    g <- grep("^menuGroup:", readLines(a, warn = FALSE), value = TRUE)
    if (!length(g)) return(FALSE)
    !grepl("[DPT]$", trimws(sub("^menuGroup:", "", g[1])))
}

bad <- list()
for (p in ryamls) {
    analysis <- sub("\\.r\\.yaml$", "", basename(p))
    lines <- readLines(p, warn = FALSE)
    hits <- grep("^\\s*format:\\s*\\S", lines)
    for (i in hits) {
        raw <- sub("^\\s*format:\\s*", "", lines[i])
        raw <- gsub("^['\"]|['\"]\\s*$", "", trimws(sub("#.*$", "", raw)))
        if (!nzchar(raw)) next
        toks <- parse_format(raw)
        if (all(vapply(toks, valid_token, logical(1)))) next
        bad[[length(bad) + 1L]] <- list(
            analysis = analysis, shipped = is_shipped(analysis),
            where = sprintf("%s:%d", basename(p), i), format = raw)
    }
}

test_that("the token grammar is what jamovi actually implements", {
    # the exact cases that cost us 519 columns
    expect_false("zto" %in% parse_format("zto:4"))
    expect_false("zto" %in% parse_format("zto3"))
    expect_false("zto" %in% parse_format("zto;pvalue"))
    expect_false("pvalue" %in% parse_format("zto;pvalue"))
    expect_false("pvalue" %in% parse_format("zto,p:.3"))
    # and the forms that work
    expect_true(all(c("zto", "pvalue") %in% parse_format("zto,pvalue")))
    expect_true(all(vapply(parse_format("zto,dp:4"), valid_token, logical(1))))
    expect_true(valid_token("sf:3"))
    expect_false(valid_token("currency"))
    expect_false(valid_token("proportion"))
})

test_that("no shipped column declares a format jamovi cannot parse", {
    shipped <- Filter(function(b) b$shipped, bad)
    if (length(shipped))
        cat("\n", paste(vapply(shipped, function(b)
            sprintf("  %s  format: %s", b$where, b$format), character(1)), collapse = "\n"), "\n")
    expect_equal(length(shipped), 0L)
})

test_that("promotion debt in unshipped analyses is reported", {
    unshipped <- Filter(function(b) !b$shipped, bad)
    # Not a failure: these analyses do not reach a user. They WOULD fail the moment their
    # menuGroup loses its D/P/T suffix, which is what review guide section 24 is about.
    cat(sprintf("\n  promotion debt: %d malformed format(s) in unshipped analyses\n",
                length(unshipped)))
    expect_true(TRUE)
})
