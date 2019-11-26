## ----include = FALSE--------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(eval = TRUE, message = FALSE, results = 'asis', comment='')
options(width = 120)

## ----results = 'asis'-------------------------------------------------------------------------------------------------
library(arsenal)

## ---------------------------------------------------------------------------------------------------------------------
df1 <- data.frame(id = paste0("person", 1:3),
                  a = c("a", "b", "c"),
                  b = c(1, 3, 4),
                  c = c("f", "e", "d"),
                  row.names = paste0("rn", 1:3),
                  stringsAsFactors = FALSE)
df2 <- data.frame(id = paste0("person", 3:1),
                  a = c("c", "b", "a"),
                  b = c(1, 3, 4),
                  d = paste0("rn", 1:3),
                  row.names = paste0("rn", c(1,3,2)),
                  stringsAsFactors = FALSE)

## ----results='markup'-------------------------------------------------------------------------------------------------
comparedf(df1, df2)

## ---------------------------------------------------------------------------------------------------------------------
summary(comparedf(df1, df2))

## ---------------------------------------------------------------------------------------------------------------------
summary(comparedf(df1, df2, by = "id"))

## ---------------------------------------------------------------------------------------------------------------------
data(mockstudy)
mockstudy2 <- muck_up_mockstudy()

## ---------------------------------------------------------------------------------------------------------------------
summary(comparedf(mockstudy, mockstudy2, by = "case"))

## ----eval = FALSE-----------------------------------------------------------------------------------------------------
#  summary(comparedf(mockstudy, mockstudy2, by = "case", control = comparedf.control(tol.vars = "case")))

## ---------------------------------------------------------------------------------------------------------------------
summary(comparedf(mockstudy, mockstudy2, by = "case", tol.vars = "case"))

## ---------------------------------------------------------------------------------------------------------------------
summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case") # dots=underscores=spaces, ignore case
))

## ---------------------------------------------------------------------------------------------------------------------
summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c(arm = "Arm", fu.stat = "fu stat", fu.time = "fu_time")
))

## ---------------------------------------------------------------------------------------------------------------------
summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE            # compare integers and numerics
))

## ---------------------------------------------------------------------------------------------------------------------
summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE,           # compare integers and numerics
                tol.num.val = 10             # allow absolute differences <= 10
))

## ---------------------------------------------------------------------------------------------------------------------
summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE,           # compare integers and numerics
                tol.num.val = 10,            # allow absolute differences <= 10
                tol.factor = "labels"        # match only factor labels
))

## ---------------------------------------------------------------------------------------------------------------------
summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE,           # compare integers and numerics
                tol.num.val = 10,            # allow absolute differences <= 10
                tol.factor = "labels",       # match only factor labels
                factor.as.char = TRUE        # compare factors and characters
))

## ---------------------------------------------------------------------------------------------------------------------
summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE,           # compare integers and numerics
                tol.num.val = 10,            # allow absolute differences <= 10
                tol.factor = "labels",       # match only factor labels
                factor.as.char = TRUE,       # compare factors and characters
                tol.char = "case"            # ignore case in character vectors
))

## ----results='markup'-------------------------------------------------------------------------------------------------
tol.NA

## ----results = 'markup'-----------------------------------------------------------------------------------------------
my.tol <- function(x, y, tol)
{
  tol.NA(x, y, x > y)
}

date.df1 <- data.frame(dt = as.Date(c("2017-09-07", "2017-08-08", "2017-07-09", NA)))
date.df2 <- data.frame(dt = as.Date(c("2017-10-01", "2017-08-08", "2017-07-10", "2017-01-01")))
n.diffs(comparedf(date.df1, date.df2)) # default finds any differences
n.diffs(comparedf(date.df1, date.df2, tol.date = my.tol)) # our function identifies only the NA as different...
n.diffs(comparedf(date.df2, date.df1, tol.date = my.tol)) # ... until we change the argument order


## ---------------------------------------------------------------------------------------------------------------------
tol.minus9 <- function(x, y, tol)
{
  idx1 <- is.na(x) & !is.na(y) & y == -9
  idx2 <- tol.num.absolute(x, y, tol) # find other absolute differences
  return(!idx1 & idx2)
}

summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE,           # compare integers and numerics
                tol.num.val = 10,            # allow absolute differences <= 10
                tol.factor = "labels",       # match only factor labels
                factor.as.char = TRUE,       # compare factors and characters
                tol.char = "case",           # ignore case in character vectors
                tol.num = tol.minus9         # ignore NA -> -9 changes
))

## ----results = 'markup'-----------------------------------------------------------------------------------------------
cmp <- comparedf(mockstudy, mockstudy2, by = "case", tol.vars = c("._ ", "case"), int.as.num = TRUE)
n.diffs(cmp)
head(diffs(cmp))

## ----results = 'markup'-----------------------------------------------------------------------------------------------
diffs(cmp, by.var = TRUE)

## ----results = 'markup'-----------------------------------------------------------------------------------------------
diffs(cmp, vars = c("ps", "ast"), by.var = TRUE)
diffs(cmp, vars = c("ps", "ast"))

## ---------------------------------------------------------------------------------------------------------------------
obj <- comparedf(mockstudy, mockstudy2, by = "case")

## ----results='markup'-------------------------------------------------------------------------------------------------
print(obj$frame.summary)

## ----results='markup'-------------------------------------------------------------------------------------------------
print(obj$vars.summary)

