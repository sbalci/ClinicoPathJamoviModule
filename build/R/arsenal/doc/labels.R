## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(message = FALSE, results = 'asis')

## ------------------------------------------------------------------------
library(arsenal)
data(mockstudy)
library(magrittr)

# for 'freqlist' examples
tab.ex <- table(mockstudy[c("arm", "sex", "mdquality.s")], useNA="ifany")

## ------------------------------------------------------------------------
summary(freqlist(tab.ex),
        labelTranslations = c(arm = "Treatment Arm", sex = "Gender", mdquality.s = "LASA QOL"))
summary(tableby(arm ~ sex + age, data = mockstudy),
        labelTranslations = c(sex = "SEX", age = "Age, yrs"))
summary(modelsum(bmi ~ age, adjust = ~sex, data = mockstudy),
        labelTranslations = list(sexFemale = "Female", age = "Age, yrs"))

## ------------------------------------------------------------------------
# the non-pipe version; somewhat clunky
tmp <- freqlist(tab.ex)
labels(tmp) <- c(arm = "Treatment Arm", sex = "Gender", mdquality.s = "LASA QOL")
summary(tmp)

# piped--much cleaner
mockstudy %>% 
  tableby(arm ~ sex + age, data = .) %>% 
  set_labels(c(sex = "SEX", age = "Age, yrs")) %>% 
  summary()

mockstudy %>% 
  modelsum(bmi ~ age, adjust = ~ sex, data = .) %>% 
  set_labels(list(sexFemale = "Female", age = "Age, yrs")) %>% 
  summary()

## ------------------------------------------------------------------------
mockstudy.lab <- keep.labels(mockstudy)
class(mockstudy$age)
class(mockstudy.lab$age)

## ------------------------------------------------------------------------
class(loosen.labels(mockstudy.lab)$age)

## ------------------------------------------------------------------------
attr(mockstudy.lab$sex, "label") <- "Sex"
labels(mockstudy.lab$age) <- "Age, yrs"

## ------------------------------------------------------------------------
labels(mockstudy.lab) <- list(sex = "Sex", age = "Age, yrs")
summary(tableby(arm ~ sex + age, data = mockstudy.lab))

## ------------------------------------------------------------------------
mockstudy %>% 
  set_labels(list(sex = "SEX", age = "Age, yrs")) %>% 
  modelsum(bmi ~ age, adjust = ~ sex, data = .) %>% 
  summary()

## ----results='markdown'--------------------------------------------------
labels(mockstudy.lab)

