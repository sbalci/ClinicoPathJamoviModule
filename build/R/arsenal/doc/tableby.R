## ----echo = FALSE---------------------------------------------------------------------------------
options(width = 100)
ge330 <- getRversion() >= "3.3.0"

## ---- load-data-----------------------------------------------------------------------------------
library(arsenal)
require(knitr)
require(survival)
data(mockstudy) ##load data
dim(mockstudy)  ##look at how many subjects and variables are in the dataset 
# help(mockstudy) ##learn more about the dataset and variables
str(mockstudy) ##quick look at the data

## ---- simple1-------------------------------------------------------------------------------------
tab1 <- tableby(arm ~ sex + age, data=mockstudy)

## ---- simple-text---------------------------------------------------------------------------------
summary(tab1, text=TRUE)

## ---- simple-markdown, results='asis'-------------------------------------------------------------
summary(tab1)

## -------------------------------------------------------------------------------------------------
as.data.frame(tab1)

## -------------------------------------------------------------------------------------------------
## base R frequency example
tmp <- table(Gender=mockstudy$sex, "Study Arm"=mockstudy$arm)
tmp

# Note: The continuity correction is applied by default in R (not used in %table)
chisq.test(tmp)

## base R numeric summary example
tapply(mockstudy$age, mockstudy$arm, summary)
summary(aov(age ~ arm, data=mockstudy))


## ---- check-labels--------------------------------------------------------------------------------
## Look at one variable's label
attr(mockstudy$age,'label')

## See all the variables with a label
unlist(lapply(mockstudy,'attr','label'))
# Can also use labels(mockstudy)

## ---- add-label, results='asis'-------------------------------------------------------------------
attr(mockstudy$sex,'label')  <- 'Gender'

tab1 <- tableby(arm ~ sex + age, data=mockstudy)
summary(tab1)

## ---- results = 'asis'----------------------------------------------------------------------------
labels(mockstudy)  <- c(age = 'Age, yrs', sex = "Gender")

tab1 <- tableby(arm ~ sex + age, data=mockstudy)
summary(tab1)

## ---- results='asis'------------------------------------------------------------------------------
mylabels <- list(sex = "SEX", age = "Age, yrs")
summary(tab1, labelTranslations = mylabels)

## ---- assignlabels--------------------------------------------------------------------------------
labels(tab1)
labels(tab1) <- c(arm="Treatment Assignment", age="Baseline Age (yrs)")
labels(tab1)

## ---- results='asis'------------------------------------------------------------------------------
summary(tab1)

## ---- results='asis'------------------------------------------------------------------------------
mycontrols  <- tableby.control(test=FALSE, total=FALSE,
                               numeric.test="kwt", cat.test="chisq",
                               numeric.stats=c("N", "median", "q1q3"),
                               cat.stats=c("countpct"),
                               stats.labels=list(N='Count', median='Median', q1q3='Q1,Q3'))
tab2 <- tableby(arm ~ sex + age, data=mockstudy, control=mycontrols)
summary(tab2)

## ---- results='asis'------------------------------------------------------------------------------
tab3 <- tableby(arm ~ sex + age, data=mockstudy, test=FALSE, total=FALSE, 
                numeric.stats=c("median","q1q3"), numeric.test="kwt")
summary(tab3)

## ---- testformula---------------------------------------------------------------------------------
tab.test <- tableby(arm ~ kwt(age) + anova(bmi) + notest(ast), data=mockstudy)
tests(tab.test)

## ---- results='asis'------------------------------------------------------------------------------
summary(tab.test)

## ---- testsAndStats, results='asis'---------------------------------------------------------------
tab.test <- tableby(arm ~ kwt(ast, "Nmiss2","median") + anova(age, "N","mean") +
                    notest(bmi, "Nmiss","median"), data=mockstudy)
summary(tab.test)

## ---- simfe, results='asis'-----------------------------------------------------------------------
set.seed(100)
tab.catsim <- tableby(arm ~ sex + race, cat.test="fe", simulate.p.value=TRUE, B=500, data=mockstudy)
tests(tab.catsim)

## ---- chisqcorrect, results='asis'----------------------------------------------------------------
cat.correct <- tableby(arm ~ sex + race, cat.test="chisq", subset = !grepl("^F", arm), data=mockstudy)
tests(cat.correct)
cat.nocorrect <- tableby(arm ~ sex + race, cat.test="chisq", subset = !grepl("^F", arm),
     chisq.correct=FALSE, data=mockstudy)
tests(cat.nocorrect)

## ---- nobyvar, results='asis'---------------------------------------------------------------------
tab.noby <- tableby(~ bmi + sex + age, data=mockstudy)
summary(tab.noby)

## ---- results="asis"------------------------------------------------------------------------------
summary(tab.test, pfootnote=TRUE)

## -------------------------------------------------------------------------------------------------
mockstudy$age.ordnew <- ordered(c("a",NA,as.character(mockstudy$age.ord[-(1:2)])))
table(mockstudy$age.ord, mockstudy$sex)
table(mockstudy$age.ordnew, mockstudy$sex)
class(mockstudy$age.ord)

## ---- results="asis", eval=requireNamespace("coin", quietly = TRUE)-------------------------------
summary(tableby(sex ~ age.ordnew, data = mockstudy), pfootnote = TRUE)
summary(tableby(sex ~ age.ord, data = mockstudy), pfootnote = TRUE)

## ---- eval=ge330----------------------------------------------------------------------------------
survfit(Surv(fu.time, fu.stat)~sex, data=mockstudy)
survdiff(Surv(fu.time, fu.stat)~sex, data=mockstudy)

## ---- results='asis'------------------------------------------------------------------------------
summary(tableby(sex ~ Surv(fu.time, fu.stat), data=mockstudy))

## ---- eval=ge330----------------------------------------------------------------------------------
summary(survfit(Surv(fu.time/365.25, fu.stat)~sex, data=mockstudy), times=1:5)

## ---- results='asis', eval=ge330------------------------------------------------------------------
summary(tableby(sex ~ Surv(fu.time/365.25, fu.stat), data=mockstudy, times=1:5, surv.stats=c("NeventsSurv","NriskSurv")))

## ---- results='asis'------------------------------------------------------------------------------
set.seed(100)
N <- nrow(mockstudy)
mockstudy$dtentry <- mdy.Date(month=sample(1:12,N,replace=T), day=sample(1:29,N,replace=T), 
                              year=sample(2005:2009,N,replace=T))
summary(tableby(sex ~ dtentry, data=mockstudy))

## ---- results='asis'------------------------------------------------------------------------------
## create a vector specifying the variable names
myvars <- names(mockstudy)

## select the 8th through the last variables
## paste them together, separated by the + sign
RHS <- paste(myvars[8:10], collapse="+")
RHS

## create a formula using the as.formula function
as.formula(paste('arm ~ ', RHS))

## use the formula in the tableby function
summary(tableby(as.formula(paste('arm ~', RHS)), data=mockstudy))

## ---- results='asis'------------------------------------------------------------------------------
## The formulize function does the paste and as.formula steps
tmp <- formulize('arm',myvars[8:10])
tmp

## More complex formulas could also be written using formulize
tmp2 <- formulize('arm',c('ps','hgb^2','bmi'))

## use the formula in the tableby function
summary(tableby(tmp, data=mockstudy))

## -------------------------------------------------------------------------------------------------
newdata <- subset(mockstudy, subset=age>50 & arm=='F: FOLFOX', select = c(sex,ps:bmi))
dim(mockstudy)
table(mockstudy$arm)
dim(newdata)
names(newdata)

## ---- results='asis'------------------------------------------------------------------------------
summary(tableby(sex ~ ., data=newdata))

## ---- results='asis'------------------------------------------------------------------------------
summary(tableby(sex ~ ps + hgb + bmi, subset=age>50 & arm=="F: FOLFOX", data=mockstudy))

## -------------------------------------------------------------------------------------------------
## create a variable combining the levels of mdquality.s and sex
with(mockstudy, table(interaction(mdquality.s,sex)))

## ---- results='asis'------------------------------------------------------------------------------
summary(tableby(arm ~ interaction(mdquality.s,sex), data=mockstudy))

## ---- results='asis'------------------------------------------------------------------------------
## create a new grouping variable with combined levels of arm and sex
summary(tableby(interaction(mdquality.s, sex) ~  age + bmi, data=mockstudy, subset=arm=="F: FOLFOX"))

## ---- maketrans, results='asis'-------------------------------------------------------------------
trans <- tableby(arm ~ I(age/10) + log(bmi) + factor(mdquality.s, levels=0:1, labels=c('N','Y')),
                 data=mockstudy)
summary(trans)

## ---- assignlabels2-------------------------------------------------------------------------------
labels(trans)
labels(trans)[2:4] <- c('Age per 10 yrs', 'log(BMI)', 'MD Quality')
labels(trans)

## ---- transsummary, results='asis'----------------------------------------------------------------
summary(trans)

## ---- results='asis'------------------------------------------------------------------------------
class(mockstudy$mdquality.s)
summary(tableby(arm~mdquality.s, data=mockstudy))

## ---- results='asis'------------------------------------------------------------------------------
summary(tableby(arm ~ chisq(mdquality.s, "Nmiss","countpct"), data=mockstudy))

## ---- results='asis'------------------------------------------------------------------------------
mytab <- tableby(arm ~ sex + alk.phos + age, data=mockstudy)
mytab2 <- mytab[c('age','sex','alk.phos')]
summary(mytab2)
summary(mytab[c('age','sex')], digits = 2)
summary(mytab[c(3,1)], digits = 3)
summary(sort(mytab, decreasing = TRUE))
summary(mytab[mytab < 0.5])
head(mytab, 1) # can also use tail()

## ---- results="asis"------------------------------------------------------------------------------
## demographics
tab1 <- tableby(arm ~ sex + age, data=mockstudy,
                control=tableby.control(numeric.stats=c("Nmiss","meansd"), total=FALSE))
## lab data
tab2 <- tableby(arm ~ hgb + alk.phos, data=mockstudy,
                control=tableby.control(numeric.stats=c("Nmiss","median","q1q3"),
                                        numeric.test="kwt", total=FALSE))
tab12 <- merge(tab1, tab2)
class(tab12)
summary(tab12)

## ---- results='asis'------------------------------------------------------------------------------
t1 <- tableby(arm ~ sex + age, data=mockstudy)
summary(t1, title='Demographics')

## -------------------------------------------------------------------------------------------------
## look at how many missing values there are for each variable
apply(is.na(mockstudy),2,sum)

## ---- results='asis'------------------------------------------------------------------------------
## Show how many subjects have each variable (non-missing)
summary(tableby(sex ~ ast + age, data=mockstudy,
                control=tableby.control(numeric.stats=c("N","median"), total=FALSE)))

## Always list the number of missing values
summary(tableby(sex ~ ast + age, data=mockstudy,
                control=tableby.control(numeric.stats=c("Nmiss2","median"), total=FALSE)))

## Only show the missing values if there are some (default)
summary(tableby(sex ~ ast + age, data=mockstudy, 
                control=tableby.control(numeric.stats=c("Nmiss","mean"),total=FALSE)))

## Don't show N at all
summary(tableby(sex ~ ast + age, data=mockstudy, 
                control=tableby.control(numeric.stats=c("mean"),total=FALSE)))

## ---- results = 'asis'----------------------------------------------------------------------------
mockstudy$ps.cat <- factor(mockstudy$ps)
attr(mockstudy$ps.cat, "label") <- "ps"
summary(tableby(sex ~ includeNA(ps.cat), data = mockstudy, cat.stats = "countpct"))

## ---- results='asis'------------------------------------------------------------------------------
summary(tableby(arm ~ sex + age + fu.time, data=mockstudy), digits=4, digits.p=2, digits.pct=1)

## ----results='asis'-------------------------------------------------------------------------------
summary(tableby(arm ~ chisq(sex, digits.pct=1) + anova(age, digits=4) +
                  anova(fu.time, digits = 1), data=mockstudy))

## ---- results='asis'------------------------------------------------------------------------------
myfunc <- function(x, weights=rep(1,length(x)), ...){
  mean(x, trim=.1, ...)
}

summary(tableby(sex ~ hgb, data=mockstudy, 
                control=tableby.control(numeric.stats=c("Nmiss","myfunc"), numeric.test="kwt",
                    stats.labels=list(Nmiss='Missing values', myfunc="Trimmed Mean, 10%"))))


## -------------------------------------------------------------------------------------------------
##create fake group that is not balanced by age/sex 
set.seed(200)
mockstudy$fake_arm <- ifelse(mockstudy$age>60 & mockstudy$sex=='Female',sample(c('A','B'),replace=T, prob=c(.2,.8)),
                            sample(c('A','B'),replace=T, prob=c(.8,.4)))

mockstudy$agegp <- cut(mockstudy$age, breaks=c(18,50,60,70,90), right=FALSE)

## create weights based on agegp and sex distribution
tab1 <- with(mockstudy,table(agegp, sex))
tab2 <- with(mockstudy, table(agegp, sex, fake_arm))
tab2
gpwts <- rep(tab1, length(unique(mockstudy$fake_arm)))/tab2
gpwts[gpwts>50] <- 30

## apply weights to subjects
index <- with(mockstudy, cbind(as.numeric(agegp), as.numeric(sex), as.numeric(as.factor(fake_arm)))) 
mockstudy$wts <- gpwts[index]

## show weights by treatment arm group
tapply(mockstudy$wts,mockstudy$fake_arm, summary)

## ---- results='asis', eval=ge330------------------------------------------------------------------
orig <- tableby(fake_arm ~ age + sex + Surv(fu.time/365, fu.stat), data=mockstudy, test=FALSE)
summary(orig, title='No Case Weights used')
tab1 <- tableby(fake_arm ~ age + sex + Surv(fu.time/365, fu.stat), data=mockstudy, weights=wts)
summary(tab1, title='Case Weights used')

## ---- results='asis', eval=ge330------------------------------------------------------------------
mypval <- data.frame(
  byvar = "fake_arm",
  variable = c('age','sex','Surv(fu.time/365, fu.stat)'), 
  adj.pvalue = c(.953,.811,.01), 
  method = c('Age/Sex adjusted model results')
)
tab2 <- modpval.tableby(tab1, mypval, use.pname=TRUE)
summary(tab2, title='Case Weights used, p-values added', pfootnote=TRUE)

## ---- results='asis'------------------------------------------------------------------------------
table2 <- tableby(arm~sex + factor(mdquality.s), data=mockstudy, cat.simplify=TRUE)
summary(table2, labelTranslations=c(sex="Female", "factor(mdquality.s)"="MD Quality"))

## ----results='asis'-------------------------------------------------------------------------------
summary(tableby(arm ~ age + ast, data = mockstudy,
                numeric.simplify=TRUE, numeric.stats=c("Nmiss", "meansd")))

## ----results='asis'-------------------------------------------------------------------------------
summary(tableby(arm ~ anova(age, "meansd", numeric.simplify=TRUE) +
                  chisq(sex, cat.simplify=TRUE), data = mockstudy))

## -------------------------------------------------------------------------------------------------
tab1 <- tableby(arm~sex+age, data=mockstudy)
as.data.frame(tab1)

# write.csv(tmp, '/my/path/here/mymodel.csv')

## ----eval = FALSE---------------------------------------------------------------------------------
#  ## write to an HTML document
#  tab1 <- tableby(arm ~ sex + age, data=mockstudy)
#  write2html(tab1, "~/trash.html")
#  
#  ## write to a Word document
#  write2word(tab1, "~/trash.doc", title="My table in Word")

## ----eval=FALSE-----------------------------------------------------------------------------------
#  # A standalone shiny app
#  library(shiny)
#  library(arsenal)
#  data(mockstudy)
#  
#  shinyApp(
#    ui = fluidPage(tableOutput("table")),
#    server = function(input, output) {
#      output$table <- renderTable({
#        as.data.frame(summary(tableby(sex ~ age, data = mockstudy), text = "html"))
#      }, sanitize.text.function = function(x) x)
#    }
#  )

## ----eval=FALSE-----------------------------------------------------------------------------------
#  summary(tableby(sex ~ age, data = mockstudy), title="(\\#tab:mytableby) Caption here")

## ----results='asis'-------------------------------------------------------------------------------
tab <- summary(tableby(sex ~ age + fu.time + bmi + mdquality.s, data = mockstudy))
tab
padjust(tab, method = "bonferroni")

## ----results='asis'-------------------------------------------------------------------------------
summary(tableby(list(sex, mdquality.s, ps) ~ age + bmi, data = mockstudy))

## ----results='asis'-------------------------------------------------------------------------------
summary(tableby(list(sex, mdquality.s, ps) ~ age + bmi, data = mockstudy), term.name = TRUE)

## ----results='asis'-------------------------------------------------------------------------------
summary(tableby(list(sex, ps) ~ age + bmi, strata = arm, data = mockstudy))

## -------------------------------------------------------------------------------------------------
args(tableby.control)

## -------------------------------------------------------------------------------------------------
args(arsenal:::summary.tableby)

