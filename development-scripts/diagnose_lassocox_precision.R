library(jmvcore); library(R6); library(glmnet); library(survival)
. <- jmvcore::.
source('R/utils.R'); source('R/utils-formula.R'); source('R/lassocox.h.R'); source('R/lassocox.b.R')
set.seed(20261101); n <- 90L; x <- rnorm(n)
d <- data.frame(time=rexp(n,exp(.8*x))+.01,status=factor(rep(c('Dead','Dead','Alive'),30),levels=c('Alive','Dead')), x=x,grade=ordered(rep(c('G1','G2','G3'),30)))
for (std in c(TRUE,FALSE)) {
 a <- lassocoxClass$new(options=lassocoxOptions$new(elapsedtime='time',outcome='status',outcomeLevel='Dead',censorLevel='Alive',explanatory=c('x','grade'),lambda='lambda.min',nfolds=5,random_seed=101,standardize=std),data=d)
 pr <- a$.__enclos_env__$private
 clean <- suppressWarnings(pr$.cleanData()); fit <- suppressWarnings(pr$.fitModel(clean))
 y <- Surv(clean$time,clean$status)
 refined <- glmnet(clean$X,y,family='cox',cox.ties='breslow',standardize=std,lambda=fit$cv_fit$lambda,thresh=1e-12)
 b <- fit$coef_matrix[,1]; br <- as.numeric(coef(refined,s=fit$lambda_optimal))
 ll <- function(beta) {
   eta <- as.numeric(clean$X %*% beta)
   sum(vapply(sort(unique(clean$time[clean$status==1])),function(t) {
     at <- clean$time==t & clean$status==1
     sum(eta[at])-sum(at)*log(sum(exp(eta[clean$time>=t])))
   },0.0))
 }
 scale <- if(std) sqrt(colMeans(sweep(clean$X,2,colMeans(clean$X))^2)) else rep(1,length(b))
 obj <- function(beta) -ll(beta)/n + fit$lambda_optimal*sum(abs(beta)*scale)
 cat('standardize=',std,' lambda=',fit$lambda_optimal,' max_beta_diff=',max(abs(b-br)), ' objective_gap=',obj(b)-obj(br),'\n',sep='')
 if(std) {
   nc <- ncvreg::ncvsurv(clean$X,y,penalty='lasso',lambda=fit$lambda_optimal*c(1.1,1,.9),eps=1e-10,max.iter=1000000)
   bn <- as.numeric(coef(nc,lambda=fit$lambda_optimal))
   cat('ncvreg max_beta_diff from default=',max(abs(b-bn)),' refined=',max(abs(br-bn)),'\n')
 }
}
