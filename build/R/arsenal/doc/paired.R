## ----echo = FALSE---------------------------------------------------------------------------------
options(width = 100)

## ---- load-data-----------------------------------------------------------------------------------
library(arsenal)
dat <- data.frame(
  tp = paste0("Time Point ", c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)),
  id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 6),
  Cat = c("A", "A", "A", "B", "B", "B", "B", "A", NA, "B"),
  Fac = factor(c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A")),
  Num = c(1, 2, 3, 4, 4, 3, 3, 4, 0, NA),
  Ord = ordered(c("I", "II", "II", "III", "III", "III", "I", "III", "II", "I")),
  Lgl = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
  Dat = as.Date("2018-05-01") + c(1, 1, 2, 2, 3, 4, 5, 6, 3, 4),
  stringsAsFactors = FALSE
)

## ----results = 'asis'-----------------------------------------------------------------------------
p <- paired(tp ~ Cat + Fac + Num + Ord + Lgl + Dat, data = dat, id = id, signed.rank.exact = FALSE)
summary(p)

## ----results = 'asis'-----------------------------------------------------------------------------
p <- paired(tp ~ Cat + Fac + Num + Ord + Lgl + Dat, data = dat, id = id,
            signed.rank.exact = FALSE, na.action = na.paired("fill"))
summary(p)

## -------------------------------------------------------------------------------------------------
args(paired.control)

## -------------------------------------------------------------------------------------------------
args(arsenal:::summary.tableby)

