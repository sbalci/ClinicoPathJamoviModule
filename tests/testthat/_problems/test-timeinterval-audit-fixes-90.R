# Extracted from test-timeinterval-audit-fixes.R:90

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
good <- format(as.Date("2016-01-01") + 0:29, "%Y-%m-%d")
end  <- format(as.Date("2017-01-01") + 0:29, "%Y-%m-%d")
for (col in list(replace(good, 30, "99999"), factor(replace(good, 30, "99999")))) {
        d <- data.frame(s = col, e = end, stringsAsFactors = FALSE)
        res <- timeinterval(data = d, dx_date = "s", fu_date = "e",
                            time_format = "ymd", output_unit = "months")
        expect_false(grepl("cannot be read as dates unambiguously",
                           res$messages$content, fixed = TRUE))
        expect_match(as.character(res$summary$content), "person-months", fixed = TRUE)
    }
