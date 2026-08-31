# Extracted from test-timeinterval-audit-fixes.R:39

# prequel ----------------------------------------------------------------------
library(testthat)
library(ClinicoPath)
strip <- function(x) gsub("[[:space:]]+", " ", gsub("<[^<>]*>", " ", paste(as.character(x), collapse = " ")))
xl <- function(from, n) as.numeric(as.Date(from) - as.Date("1899-12-30")) + seq_len(n) - 1
clean_df <- function(n = 40) {
    set.seed(2)
    s <- as.Date("2016-01-01") + sample(0:200, n, TRUE)
    data.frame(s = format(s, "%Y-%m-%d"),
               e = format(s + sample(60:1200, n, TRUE), "%Y-%m-%d"),
               stringsAsFactors = FALSE)
}

# test -------------------------------------------------------------------------
df <- data.frame(start = xl("2016-01-01", 60), end = xl("2016-01-01", 60) + 366)
for (fmt in c("auto", "ymd", "mdy")) {
        res <- timeinterval(data = df, dx_date = "start", fu_date = "end",
                            time_format = fmt, output_unit = "months")
        expect_true(grepl("day-count numbers", res$messages$content, fixed = TRUE),
                    info = paste("format:", fmt))
        # the fabricated person-time must never be produced
        expect_false(grepl("person-months", res$summary$content, fixed = TRUE),
                     info = paste("format:", fmt))
    }
