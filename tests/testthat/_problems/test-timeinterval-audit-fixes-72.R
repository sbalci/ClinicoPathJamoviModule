# Extracted from test-timeinterval-audit-fixes.R:72

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
d1 <- as.Date("2020-01-01") + 0:29
d2 <- d1 + 366
e1 <- as.Date("1995-11-01") + 0:29
e2 <- e1 + 366
cases <- list(
        # 8-digit YYYYMMDD
        list(d = data.frame(start = as.numeric(format(d1, "%Y%m%d")),
                            end   = as.numeric(format(d2, "%Y%m%d"))), fmt = "auto"),
        # 6-digit YYMMDD
        list(d = data.frame(start = as.numeric(format(d1, "%y%m%d")),
                            end   = as.numeric(format(d2, "%y%m%d"))), fmt = "auto"),
        # 6-digit MMDDYY with mdy selected: ymd() rejects these, so the old
        # oracle-based guard falsely condemned them
        list(d = data.frame(start = as.numeric(format(e1, "%m%d%y")),
                            end   = as.numeric(format(e2, "%m%d%y"))), fmt = "mdy"),
        # 6-digit DDMMYY with dmy selected: same failure
        list(d = data.frame(start = as.numeric(format(e1, "%d%m%y")),
                            end   = as.numeric(format(e2, "%d%m%y"))), fmt = "dmy"))
for (cs in cases) {
        res <- timeinterval(data = cs$d, dx_date = "start", fu_date = "end",
                            time_format = cs$fmt, output_unit = "months")
        expect_false(grepl("cannot be read as dates unambiguously",
                           res$messages$content, fixed = TRUE), info = cs$fmt)
    }
