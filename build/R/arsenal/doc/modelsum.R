## ---- echo=FALSE, message=FALSE, results='hide', warning=FALSE-----------
require(knitr)
require(broom)
require(gam)
require(MASS)
require(pROC)
require(rpart)
 
opts_chunk$set(comment = NA, echo=TRUE, prompt=TRUE, collapse=TRUE)


## ---- load-data----------------------------------------------------------
require(arsenal)
data(mockstudy) # load data
dim(mockstudy)  # look at how many subjects and variables are in the dataset 
# help(mockstudy) # learn more about the dataset and variables
str(mockstudy) # quick look at the data

## ----simple1-------------------------------------------------------------
tab1 <- modelsum(bmi ~ sex + age, data=mockstudy)

## ----simple-text---------------------------------------------------------
summary(tab1, text=TRUE)

## ----simple-markdown, results='asis'-------------------------------------
summary(tab1)

## ------------------------------------------------------------------------
as.data.frame(tab1)

## ----adjust, results="asis"----------------------------------------------
tab2 <- modelsum(alk.phos ~ arm + ps + hgb, adjust= ~age + sex, data=mockstudy)
summary(tab2)

## ------------------------------------------------------------------------
fit <- lm(alk.phos ~ arm + age + sex, data=mockstudy)
summary(fit)
plot(fit)

## ------------------------------------------------------------------------
require(MASS)
boxcox(fit)

## ------------------------------------------------------------------------
fit2 <- lm(log(alk.phos) ~ arm + age + sex, data=mockstudy)
summary(fit2)
plot(fit2)

## ------------------------------------------------------------------------
require(gam)
fit3 <- lm(log(alk.phos) ~ arm + ns(age, df=2) + sex, data=mockstudy)

# test whether there is a difference between models 
stats::anova(fit2,fit3)

# look at functional form of age
termplot(fit3, term=2, se=T, rug=T)

## ------------------------------------------------------------------------
tmp <- tidy(fit3) # coefficients, p-values
class(tmp)
tmp

glance(fit3)

## ---- results="asis"-----------------------------------------------------
ms.logy <- modelsum(log(alk.phos) ~ arm + ps + hgb, data=mockstudy, adjust= ~age + sex, 
                    family=gaussian,  
                    gaussian.stats=c("estimate","CI.lower.estimate","CI.upper.estimate","p.value"))
summary(ms.logy)

## ------------------------------------------------------------------------
boxplot(age ~ mdquality.s, data=mockstudy, ylab=attr(mockstudy$age,'label'), xlab='mdquality.s')

fit <- glm(mdquality.s ~ age + sex, data=mockstudy, family=binomial)
summary(fit)

# create Odd's ratio w/ confidence intervals
tmp <- data.frame(summary(fit)$coef)
tmp

tmp$OR <- round(exp(tmp[,1]),2)
tmp$lower.CI <- round(exp(tmp[,1] - 1.96* tmp[,2]),2)
tmp$upper.CI <- round(exp(tmp[,1] + 1.96* tmp[,2]),2)
names(tmp)[4] <- 'P-value'

kable(tmp[,c('OR','lower.CI','upper.CI','P-value')])

# Assess the predictive ability of the model

# code using the pROC package
require(pROC)
pred <- predict(fit, type='response')
tmp <- pROC::roc(mockstudy$mdquality.s[!is.na(mockstudy$mdquality.s)]~ pred, plot=TRUE, percent=TRUE)
tmp$auc


## ------------------------------------------------------------------------
tidy(fit, exp=T, conf.int=T) # coefficients, p-values, conf.intervals

glance(fit) # model summary statistics

## ---- results="asis"-----------------------------------------------------
summary(modelsum(mdquality.s ~ age + bmi, data=mockstudy, adjust=~sex, family=binomial))

fitall <- modelsum(mdquality.s ~ age, data=mockstudy, family=binomial,
                   binomial.stats=c("Nmiss2","OR","p.value"))
summary(fitall)

## ----survival------------------------------------------------------------
require(survival)

# multivariable model with all 3 terms
fit  <- coxph(Surv(fu.time, fu.stat) ~ age + sex + arm, data=mockstudy)
summary(fit)

# check proportional hazards assumption
fit.z <- cox.zph(fit)
fit.z
plot(fit.z[1], resid=FALSE) # makes for a cleaner picture in this case
abline(h=coef(fit)[1], col='red')

# check functional form for age using pspline (penalized spline)
# results are returned for the linear and non-linear components
fit2 <- coxph(Surv(fu.time, fu.stat) ~ pspline(age) + sex + arm, data=mockstudy)
fit2

# plot smoothed age to visualize why significant
termplot(fit2, se=T, terms=1)
abline(h=0)

# The c-statistic comes out in the summary of the fit
summary(fit2)$concordance

# It can also be calculated using the survConcordance function
survConcordance(Surv(fu.time, fu.stat) ~ predict(fit2), data=mockstudy)

## ------------------------------------------------------------------------
tidy(fit) # coefficients, p-values

glance(fit) # model summary statistics

## ----results="asis"------------------------------------------------------
##Note: You must use quotes when specifying family="survival" 
##      family=survival will not work
summary(modelsum(Surv(fu.time, fu.stat) ~ arm, 
                 adjust=~age + sex, data=mockstudy, family="survival"))

##Note: the pspline term is not working yet
#summary(modelsum(Surv(fu.time, fu.stat) ~ arm, 
#                adjust=~pspline(age) + sex, data=mockstudy, family='survival'))

## ----poisson-------------------------------------------------------------
require(rpart) ##just to get access to solder dataset
data(solder)
hist(solder$skips)

fit <- glm(skips ~ Opening + Solder + Mask , data=solder, family=poisson)
stats::anova(fit, test='Chi')
summary(fit)

## ------------------------------------------------------------------------
1-pchisq(fit$deviance, fit$df.residual)

## ------------------------------------------------------------------------
fit2 <- glm(skips ~ Opening + Solder + Mask, data=solder, family=quasipoisson)
summary(fit2)

## ------------------------------------------------------------------------
tidy(fit) # coefficients, p-values

glance(fit) # model summary statistics

## ----results='asis'------------------------------------------------------
summary(modelsum(skips~Opening + Solder + Mask, data=solder, family="quasipoisson"))
summary(modelsum(skips~Opening + Solder + Mask, data=solder, family="poisson"))

## ------------------------------------------------------------------------
# add .01 to the follow-up time (.01*1 day) in order to keep everyone in the analysis
fit <- glm(fu.stat ~ offset(log(fu.time+.01)) + age + sex + arm, data=mockstudy, family=poisson)
summary(fit)
1-pchisq(fit$deviance, fit$df.residual)

coef(coxph(Surv(fu.time,fu.stat) ~ age + sex + arm, data=mockstudy))
coef(fit)[-1]

# results from the Poisson model can then be described as risk ratios (similar to the hazard ratio)
exp(coef(fit)[-1])

# As before, we can model the dispersion which alters the standard error
fit2 <- glm(fu.stat ~ offset(log(fu.time+.01)) + age + sex + arm, 
            data=mockstudy, family=quasipoisson)
summary(fit2)

## ------------------------------------------------------------------------
tidy(fit) ##coefficients, p-values

glance(fit) ##model summary statistics

## ----results="asis", eval=TRUE-------------------------------------------
summary(modelsum(fu.stat ~ age, adjust=~offset(log(fu.time+.01))+ sex + arm, 
                 data=mockstudy, family=poisson))
                 

## ---- results='asis'-----------------------------------------------------
mycontrols  <- modelsum.control(gaussian.stats=c("estimate","std.error","adj.r.squared","Nmiss"),
                                show.adjust=FALSE, show.intercept=FALSE)                            
tab2 <- modelsum(bmi ~ age, adjust=~sex, data=mockstudy, control=mycontrols)
summary(tab2)

## ---- results='asis'-----------------------------------------------------
tab3 <- modelsum(bmi ~  age, adjust=~sex, data=mockstudy,
                 gaussian.stats=c("estimate","std.error","adj.r.squared","Nmiss"), 
                 show.intercept=FALSE, show.adjust=FALSE)
summary(tab3)

## ----check-labels--------------------------------------------------------
## Look at one variable's label
attr(mockstudy$age,'label')

## See all the variables with a label
unlist(lapply(mockstudy,'attr','label'))

## or
cbind(sapply(mockstudy,attr,'label'))

## ----add-label, results='asis'-------------------------------------------
attr(mockstudy$age,'label')  <- 'Age, yrs'

tab1 <- modelsum(bmi ~  age, adjust=~sex, data=mockstudy)
summary(tab1)

## ---- results = 'asis'---------------------------------------------------
labels(mockstudy)  <- c(age = 'Age, yrs')

tab1 <- modelsum(bmi ~  age, adjust=~sex, data=mockstudy)
summary(tab1)

## ---- results='asis'-----------------------------------------------------
mylabels <- list(sexFemale = "Female", age ="Age, yrs")
summary(tab1, labelTranslations = mylabels)

## ---- eval=TRUE----------------------------------------------------------
labels(tab1)
labels(tab1) <- c(sexFemale="Female", age="Baseline Age (yrs)")
labels(tab1)

## ---- results='asis'-----------------------------------------------------
summary(tab1)

## ---- results='asis'-----------------------------------------------------
summary(modelsum(age~mdquality.s+sex, data=mockstudy), show.intercept=FALSE)

## ---- results='asis'-----------------------------------------------------
summary(modelsum(mdquality.s ~ age + bmi, data=mockstudy, adjust=~sex, family=binomial),
        show.adjust=FALSE)  

## ---- results='asis'-----------------------------------------------------
# create a vector specifying the variable names
myvars <- names(mockstudy)

# select the 8th through the 12th
# paste them together, separated by the + sign
RHS <- paste(myvars[8:12], collapse="+")
RHS

# create a formula using the as.formula function
as.formula(paste('mdquality.s ~ ', RHS))

# use the formula in the modelsum function
summary(modelsum(as.formula(paste('mdquality.s ~', RHS)), family=binomial, data=mockstudy))

## ---- results='asis'-----------------------------------------------------
## The formulize function does the paste and as.formula steps
tmp <- formulize('mdquality.s',myvars[8:10])
tmp

## More complex formulas could also be written using formulize
tmp2 <- formulize('mdquality.s',c('ps','hgb','sqrt(bmi)'))

## use the formula in the modelsum function
summary(modelsum(tmp, data=mockstudy, family=binomial))

## ------------------------------------------------------------------------
newdata <- subset(mockstudy, subset=age>50 & arm=='F: FOLFOX', select = c(age,sex, bmi:alk.phos))
dim(mockstudy)
table(mockstudy$arm)
dim(newdata)
names(newdata)

## ---- results='asis'-----------------------------------------------------
summary(modelsum(alk.phos ~ ., data=newdata))

## ---- results='asis', eval=TRUE------------------------------------------
summary(modelsum(log(alk.phos) ~ sex + ps + bmi, subset=age>50 & arm=="F: FOLFOX", data=mockstudy))
summary(modelsum(alk.phos ~ ps + bmi, adjust=~sex, subset = age>50 & bmi<24, data=mockstudy))
summary(modelsum(alk.phos ~ ps + bmi, adjust=~sex, subset=1:30, data=mockstudy))

## ------------------------------------------------------------------------
## create a variable combining the levels of mdquality.s and sex
with(mockstudy, table(interaction(mdquality.s,sex)))

## ---- results='asis'-----------------------------------------------------
summary(modelsum(age ~ interaction(mdquality.s,sex), data=mockstudy))

## ---- results='asis'-----------------------------------------------------
summary(modelsum(arm=="F: FOLFOX" ~ I(age/10) + log(bmi) + mdquality.s,
                 data=mockstudy, family=binomial))

## ---- results='asis'-----------------------------------------------------
mytab <- modelsum(bmi ~ sex + alk.phos + age, data=mockstudy)
mytab2 <- mytab[c('age','sex','alk.phos')]
summary(mytab2)
summary(mytab[c('age','sex')])
summary(mytab[c(3,1)])

## ---- results="asis"-----------------------------------------------------
## demographics
tab1 <- modelsum(bmi ~ sex + age, data=mockstudy)
## lab data
tab2 <- modelsum(mdquality.s ~ hgb + alk.phos, data=mockstudy, family=binomial)
                
tab12 <- merge(tab1, tab2, all = TRUE)
class(tab12)
summary(tab12)

## ---- results='asis'-----------------------------------------------------
t1 <- modelsum(bmi ~ sex + age, data=mockstudy)
summary(t1, title='Demographics')

## ------------------------------------------------------------------------
## look at how many missing values there are for each variable
apply(is.na(mockstudy),2,sum)

## ---- results='asis'-----------------------------------------------------
## Show how many subjects have each variable (non-missing)
summary(modelsum(bmi ~ ast + age, data=mockstudy,
                control=modelsum.control(gaussian.stats=c("N","estimate"))))

## Always list the number of missing values
summary(modelsum(bmi ~ ast + age, data=mockstudy,
                control=modelsum.control(gaussian.stats=c("Nmiss2","estimate"))))

## Only show the missing values if there are some (default)
summary(modelsum(bmi ~ ast + age, data=mockstudy, 
                control=modelsum.control(gaussian.stats=c("Nmiss","estimate"))))

## Don't show N at all
summary(modelsum(bmi ~ ast + age, data=mockstudy, 
                control=modelsum.control(gaussian.stats=c("estimate"))))

## ---- results='asis'-----------------------------------------------------
summary(modelsum(bmi ~ sex + age + fu.time, data=mockstudy), digits=4, digits.test=2)

## ------------------------------------------------------------------------
mockstudy$agegp <- cut(mockstudy$age, breaks=c(18,50,60,70,90), right=FALSE)

## create weights based on agegp and sex distribution
tab1 <- with(mockstudy,table(agegp, sex))
tab1
tab2 <- with(mockstudy, table(agegp, sex, arm))
gpwts <- rep(tab1, length(unique(mockstudy$arm)))/tab2

## apply weights to subjects
index <- with(mockstudy, cbind(as.numeric(agegp), as.numeric(sex), as.numeric(as.factor(arm)))) 
mockstudy$wts <- gpwts[index]

## show weights by treatment arm group
tapply(mockstudy$wts,mockstudy$arm, summary)

## ----results='asis'------------------------------------------------------
mockstudy$newvarA <- as.numeric(mockstudy$arm=='A: IFL')
tab1 <- modelsum(newvarA ~ ast + bmi + hgb, data=mockstudy, subset=(arm !='G: IROX'), 
                 family=binomial)
summary(tab1, title='No Case Weights used')

suppressWarnings({
tab2 <- modelsum(newvarA ~ ast + bmi + hgb, data=mockstudy, subset=(arm !='G: IROX'), 
                 weights=wts, family=binomial)
summary(tab2, title='Case Weights used')
})

## ------------------------------------------------------------------------
summary(tab2, text=T)
tmp <- as.data.frame(tab2)
tmp
# write.csv(tmp, '/my/path/here/mymodel.csv')

## ----eval = FALSE--------------------------------------------------------
#  ## write to an HTML document
#  write2html(tab2, "~/ibm/trash.html")
#  
#  ## write to a Word document
#  write2word(tab2, "~/ibm/trash.doc", title="My table in Word")

## ----eval=FALSE----------------------------------------------------------
#  # A standalone shiny app
#  library(shiny)
#  library(arsenal)
#  data(mockstudy)
#  
#  shinyApp(
#    ui = fluidPage(tableOutput("table")),
#    server = function(input, output) {
#      output$table <- renderTable({
#        as.data.frame(summary(modelsum(age ~ sex, data = mockstudy), text = "html"))
#      }, sanitize.text.function = function(x) x)
#    }
#  )

## ----eval=FALSE----------------------------------------------------------
#  summary(modelsum(age ~ sex, data = mockstudy), title="(\\#tab:mytableby) Caption here")

## ----results='asis'------------------------------------------------------
summary(modelsum(list(age, hgb) ~ bmi + sex, adjust = ~ arm, data = mockstudy))

## ----results='asis'------------------------------------------------------
summary(modelsum(list(age, hgb) ~ bmi + sex, adjust = ~ arm, data = mockstudy), term.name = TRUE)

## ----results='asis'------------------------------------------------------
summary(modelsum(list(age, hgb) ~ bmi + sex, strata = arm, data = mockstudy))

## ------------------------------------------------------------------------
multi.adjust <- modelsum(list(age, bmi) ~ fu.time + ast, adjust = list(Unadjusted = ~ 1, "Adjusted for Arm" = ~ arm), data = mockstudy)
summary(multi.adjust, adjustment.names = TRUE)
summary(multi.adjust, adjustment.names = TRUE, show.intercept = FALSE, show.adjust = FALSE)

## ------------------------------------------------------------------------
args(modelsum.control)

## ------------------------------------------------------------------------
args(arsenal:::summary.modelsum)

