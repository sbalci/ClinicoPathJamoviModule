## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy.opts=list(width.cutoff=80), tidy=TRUE, comment=NA)
options(width=80, max.print=1000)

## ----message = FALSE----------------------------------------------------------
require(arsenal)

## ----loading.data-------------------------------------------------------------
# load the data
data(mockstudy)

# retain NAs when creating the table using the useNA argument
tab.ex <- table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA="ifany")

## ----console.output-----------------------------------------------------------
example1 <- freqlist(tab.ex)

str(example1)

# view the data frame portion of freqlist output
head(as.data.frame(example1)) ## or use as.data.frame(example1)

## ---- results = 'asis'--------------------------------------------------------
summary(example1)

## ---- results = 'asis'--------------------------------------------------------
summary(example1, title="Basic freqlist output")

## -----------------------------------------------------------------------------
head(as.data.frame(summary(example1)))

## ----results='asis'-----------------------------------------------------------
### this works in R >= 3.4.0
# summary(freqlist(~ arm + sex + mdquality.s, data = mockstudy, addNA = TRUE))

### This one is backwards-compatible
summary(freqlist(~ arm + sex + addNA(mdquality.s), data = mockstudy))

## ----results='asis'-----------------------------------------------------------
summary(freqlist(~ arm + sex + includeNA(mdquality.s, "Missing"), data = mockstudy))

## ----results='asis'-----------------------------------------------------------
mockstudy$weights <- c(10000, rep(1, nrow(mockstudy) - 1))
summary(freqlist(weights ~ arm + sex + addNA(mdquality.s), data = mockstudy))

## ----results='asis'-----------------------------------------------------------
mockstudy$weights2 <- c(rep(1, nrow(mockstudy) - 1), 10000)
summary(freqlist(list(weights, weights2) ~ arm + sex + addNA(mdquality.s), data = mockstudy))

## ----labelTranslations, results = 'asis'--------------------------------------
example2 <- freqlist(tab.ex, labelTranslations = c(arm = "Treatment Arm", sex = "Gender", mdquality.s = "LASA QOL"),
                      digits.pct = 1, digits.count = 1)
summary(example2)

## ----sparse, results = 'asis'-------------------------------------------------
summary(freqlist(~ race + sex + arm, data = mockstudy, sparse = TRUE, digits.pct=1))

## ----na.options, results = 'asis'---------------------------------------------
summary(freqlist(tab.ex, na.options="include"))
summary(freqlist(tab.ex, na.options="showexclude"))
summary(freqlist(tab.ex, na.options="remove"))

## ----freq.counts, results='asis'----------------------------------------------
example3 <- freqlist(tab.ex, strata = c("arm","sex"))
summary(example3)

#using the single = TRUE argument will collapse results into a single table for printing
summary(example3, single = TRUE)


## -----------------------------------------------------------------------------
head(summary(sort(example1, decreasing = TRUE), dupLabels = TRUE))

## ----changelabs, results = 'asis'---------------------------------------------
labs <- c(arm = "Arm", sex = "Sex", mdquality.s = "QOL", freqPercent = "%")
labels(example1) <- labs
summary(example1)

## ---- results = 'asis'--------------------------------------------------------
summary(example1, labelTranslations = labs)

## ----results='asis'-----------------------------------------------------------
require(xtable)

# set up custom function for xtable text
italic <- function(x) paste0('<i>', x, '</i>')

xftbl <- xtable(as.data.frame(summary(example1)), 
  caption = "xtable formatted output of freqlist data frame", align="|r|r|r|r|c|c|c|r|")

# change the column names
names(xftbl)[1:3] <- c("Arm", "Gender", "LASA QOL")

print(xftbl, sanitize.colnames.function = italic, include.rownames = FALSE, type = "html", comment = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  summary(freqlist(~ sex + age, data = mockstudy), title="(\\#tab:mytableby) Caption here")

## -----------------------------------------------------------------------------
# base table default removes NAs
tab.d1 <- base::table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA="ifany")
tab.d1

## -----------------------------------------------------------------------------
# without specifying addNA
tab.d2 <- xtabs(formula = ~ arm + sex + mdquality.s, data = mockstudy)
tab.d2

# now with addNA
tab.d3 <- xtabs(~ arm + sex + addNA(mdquality.s), data = mockstudy)
tab.d3


## -----------------------------------------------------------------------------
# providing variables separately (as vectors) drops column names
table(mockstudy$arm, mockstudy$sex, mockstudy$mdquality.s)

## -----------------------------------------------------------------------------
# add the column name labels back using dnn option in base::table
table(mockstudy$arm, mockstudy$sex, mockstudy$mdquality.s, dnn=c("Arm", "Sex", "QOL"))

## -----------------------------------------------------------------------------
table(Arm = mockstudy$arm, Sex = mockstudy$sex, QOL = mockstudy$mdquality.s)

