## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(rstanarm)
library(bayestestR)
library(ggplot2)
library(see)
library(emmeans)
library(lme4)
library(BayesFactor)

options(knitr.kable.NA = '')
opts_chunk$set(echo = TRUE)
opts_chunk$set(comment = ">")
knitr::opts_chunk$set(dpi=300)
theme_set(see::theme_modern())
options(digits = 2)
set.seed(5)

## ----deathsticks_fig, echo=FALSE, fig.cap="Bayesian analysis of the Students' (1908) Sleep data set.", fig.align='center', out.width="80%"----
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/deathsticks.jpg")

## ----sleep_boxplot, echo=FALSE, message=FALSE, warning=FALSE-------------
ggplot(sleep, aes(x = group, y = extra, fill= group)) +
  geom_boxplot() +
  theme_classic()

## ---- echo=FALSE---------------------------------------------------------
null <- c(-.5,.5)
xrange <- c(-12,12)
ggplot() + aes(x = 0, y = 0) + 
  stat_function(aes(fill = "Null"),
                fun = dnorm, args = list(sd = 2.5),
                xlim = null, geom = "area") + 
  stat_function(aes(fill = "Alternative"),
                fun = dnorm, args = list(sd = 2.5),
                xlim = c(xrange[1],null[1]), geom = "area") + 
  stat_function(aes(fill = "Alternative"),
                fun = dnorm, args = list(sd = 2.5),
                xlim = c(null[2],xrange[2]), geom = "area") + 
  stat_function(fun = dnorm, args = list(sd = 2.5),
                xlim = xrange, size = 1) +
  scale_fill_flat(name = "") + 
  labs(x = "Drug effect", y = "Density") + 
  theme_modern() + 
  theme(legend.position = c(0.2, 0.8))

pnull <- diff(pnorm(null, sd = 2.5))
prior_odds <- (1 - pnull) / pnull

## ----rstanarm_disp, eval=FALSE, message=FALSE, warning=FALSE-------------
#  library(rstanarm)
#  model <- stan_glm(extra ~ group, data = sleep)

## ----rstanarm_fit, echo=FALSE, message=FALSE, warning=FALSE--------------
model <- stan_glm(extra ~ group, data = sleep, refresh = 0)
model_prior <- bayestestR:::.update_to_priors.stanreg(model)
posterior <- insight::get_parameters(model)$group2
prior <- insight::get_parameters(model_prior)$group2

f_post <- logspline::logspline(posterior)

dpost <- function(q){
  logspline::dlogspline(q,f_post)
}

xrange <- c(-12,12)
ggplot() + aes(x = 0, y = 0) + 
  stat_function(aes(fill = "Null"),
                fun = dpost,
                xlim = null, geom = "area") + 
  stat_function(aes(fill = "Alternative"),
                fun = dpost,
                xlim = c(xrange[1],null[1]), geom = "area") + 
  stat_function(aes(fill = "Alternative"),
                fun = dpost,
                xlim = c(null[2],xrange[2]), geom = "area") + 
  stat_function(fun = dpost,
                xlim = xrange, size = 1) +
  scale_fill_flat(name = "") + 
  geom_vline(xintercept = point_estimate(posterior)$Median, size = 1, linetype = "dashed") + 
  labs(x = "Drug effect", y = "Density") + 
  theme_modern() + 
  theme(legend.position = c(0.2, 0.8))

My_first_BF <- bayesfactor_parameters(model, null = c(-.5,.5))


BF <- My_first_BF$BF[2]
post_odds <- prior_odds * BF

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  My_first_BF <- bayesfactor_parameters(model, null = c(-.5,.5))
#  My_first_BF

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
print(My_first_BF)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  plot(My_first_BF)

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
plot(bayesfactor_parameters(
  data.frame(group2 = posterior),
  data.frame(group2 = prior),
  null = c(-.5,.5))) + 
  theme(legend.position = c(0.2, 0.8))

## ---- message=FALSE, warning=FALSE---------------------------------------
My_second_BF <- bayesfactor_parameters(model, null = 0)
My_second_BF

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  plot(My_second_BF)

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
plot(bayesfactor_parameters(
  data.frame(group2 = posterior),
  data.frame(group2 = prior),
  null = 0)) + 
  theme(legend.position = c(0.2, 0.8))

## ----prior_n_post_plot_one_sided, echo=FALSE, message=FALSE, warning=FALSE----

# Using "see"
bfsd <- bayesfactor_parameters(
  data.frame(group2 = posterior),
  data.frame(group2 = prior),
  direction = ">"
)

plot(bfsd) +
  theme(legend.position = c(0.8,0.8))


## ----savagedickey_one_sided, message=FALSE, warning=FALSE----------------
test_group2_right <- bayesfactor_parameters(model, direction = ">")
test_group2_right

## ------------------------------------------------------------------------
library(emmeans)
group_diff <- pairs(emmeans(model, ~ group))
group_diff

# pass the original model via prior
bayesfactor_parameters(group_diff, prior = model)

## ----brms_disp, eval=FALSE, message=FALSE, warning=FALSE-----------------
#  library(brms)
#  
#  m0 <- brm(Sepal.Length ~ 1, # intercept only model
#            data = iris, save_all_pars = TRUE)
#  m1 <- brm(Sepal.Length ~ Petal.Length,
#            data = iris, save_all_pars = TRUE)
#  m2 <- brm(Sepal.Length ~ Species,
#            data = iris, save_all_pars = TRUE)
#  m3 <- brm(Sepal.Length ~ Species + Petal.Length,
#            data = iris, save_all_pars = TRUE)
#  m4 <- brm(Sepal.Length ~ Species * Petal.Length,
#            data = iris, save_all_pars = TRUE)

## ----brms_models_disp, eval=FALSE----------------------------------------
#  library(bayestestR)
#  comparison <- bayesfactor_models(m1, m2, m3, m4, denominator = m0)
#  comparison

## ----brms_models_print, echo=FALSE, message=FALSE, warning=FALSE---------
# dput(comparison)
comparison <- structure(
  list(
    Model = c(
      "Petal.Length",
      "Species",
      "Species + Petal.Length",
      "Species * Petal.Length",
      "1"
    ),
    BF = c(3.44736e+44, 5.628679e+29, 7.121386e+55, 9.149948e+55, 1)
  ),
  class = c("bayesfactor_models", "see_bayesfactor_models", "data.frame"),
  row.names = c(NA, -5L),
  denominator = 5L,
  BF_method = "marginal likelihoods (bridgesampling)"
)
comparison

## ----update_models1, message=FALSE, warning=FALSE------------------------
update(comparison, reference = 3)

## ----update_models2, message=FALSE, warning=FALSE------------------------
update(comparison, reference = 2)

## ----lme4_models, message=FALSE, warning=FALSE---------------------------
library(lme4)

m0 <- lmer(Sepal.Length ~ (1 | Species), data = iris)
m1 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
m2 <- lmer(Sepal.Length ~ Petal.Length + (Petal.Length | Species), data = iris)
m3 <- lmer(Sepal.Length ~ Petal.Length + Petal.Width + (Petal.Length | Species), data = iris)
m4 <- lmer(Sepal.Length ~ Petal.Length * Petal.Width + (Petal.Length | Species), data = iris)

bayesfactor_models(m1, m2, m3, m4, denominator = m0)

## ----inclusion_brms, message=FALSE, warning=FALSE, eval=TRUE-------------
bayesfactor_inclusion(comparison)

## ----inclusion_brms2, message=FALSE, warning=FALSE, eval=TRUE------------
bayesfactor_inclusion(comparison, match_models = TRUE)

## ----JASP_all, message=FALSE, warning=FALSE, eval=TRUE-------------------
library(BayesFactor)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
BF_ToothGrowth <- anovaBF(len ~ dose*supp, ToothGrowth)

bayesfactor_inclusion(BF_ToothGrowth)

## ----JASP_all_fig, echo=FALSE, message=FALSE, warning=FALSE--------------
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/JASP1.PNG")

## ----JASP_matched, message=FALSE, warning=FALSE, eval=TRUE---------------
bayesfactor_inclusion(BF_ToothGrowth, match_models = TRUE)

## ----JASP_matched_fig, echo=FALSE, message=FALSE, warning=FALSE----------
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/JASP2.PNG")

## ----JASP_Nuisance, message=FALSE, warning=FALSE, eval=TRUE--------------
BF_ToothGrowth_against_dose <- BF_ToothGrowth[3:4]/BF_ToothGrowth[2] # OR: 
# update(bayesfactor_models(BF_ToothGrowth),
#        subset = c(4, 5),
#        reference = 3)
BF_ToothGrowth_against_dose


bayesfactor_inclusion(BF_ToothGrowth_against_dose)

## ----JASP_Nuisance_fig, echo=FALSE, message=FALSE, warning=FALSE---------
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/JASP3.PNG")

## ----plot_iris, echo=FALSE, message=FALSE, warning=FALSE-----------------
ggplot(iris, aes(Petal.Length, Sepal.Length, color = Species)) + 
  geom_point() + 
  scale_color_flat() + 
  theme(legend.position = c(0.2, 0.8))

## ---- message=FALSE, warning=FALSE, eval = FALSE-------------------------
#  iris_model <- stan_glm(Sepal.Length ~ Species + Petal.Length,
#                         data = iris)

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
iris_model <- stan_glm(Sepal.Length ~ Species + Petal.Length,
                       data = iris, refresh = 0)

model_prior <- bayestestR:::.update_to_priors.stanreg(iris_model)
priors <- insight::get_parameters(model_prior)
priors$`(Intercept)` <- NULL

ggplot(stack(priors), aes(values, fill = ind)) + 
  geom_density(color = NA) + 
  geom_vline(xintercept = 0, size = 1, linetype = "dashed") + 
  facet_grid(~ind) + 
  scale_fill_flat() + 
  theme(legend.position = "none")

# describe_posterior(iris_model)

## ------------------------------------------------------------------------
botanist_hypotheses <- c(
  "Petal.Length > 0",
  "(Speciesversicolor > 0) & (Speciesvirginica > 0)"
)

## ------------------------------------------------------------------------
botanist_BFs <- bayesfactor_restricted(iris_model, hypothesis = botanist_hypotheses)
botanist_BFs

## ---- eval=FALSE---------------------------------------------------------
#  library(bayestestR)
#  library(rstanarm)
#  library(emmeans)
#  library(ggplot2)
#  
#  contrasts(iris$Species) <- contr.sum
#  
#  fit_sum <- stan_glm(Sepal.Length ~ Species, data = iris,
#                      prior = normal(0,0.1), # just to drive the point home
#                      family = gaussian())
#  c_sum <- pairs(emmeans(fit_sum, ~ Species))
#  c_sum

## ---- warning=FALSE, echo=FALSE------------------------------------------
contrasts(iris$Species) <- contr.sum
set.seed(5)
fit_sum <- stan_glm(Sepal.Length ~ Species, data = iris,
                    # just to drive the point home, we'll use ultra-narrow priors
                    # (probably should not be used)
                    prior = normal(0,0.1),
                    family = gaussian(),
                    refresh = 0)
c_sum <- pairs(emmeans(fit_sum, ~ Species))
c_sum

## ---- message=FALSE, echo=FALSE------------------------------------------
plot(bayesfactor_parameters(c_sum, fit_sum)) +
  coord_cartesian(xlim = c(-2,1))

## ------------------------------------------------------------------------
contrasts(iris$Species) <- contr.bayes

## ---- eval=FALSE---------------------------------------------------------
#  options(contrasts = c('contr.bayes', 'contr.poly'))

## ---- eval=FALSE---------------------------------------------------------
#  fit_bayes <- stan_glm(Sepal.Length ~ Species, data = iris,
#                        prior = normal(0,0.1),
#                        family = gaussian())
#  c_bayes <- pairs(emmeans(fit_bayes, ~ Species))
#  c_bayes

## ---- warning=FALSE, echo=FALSE------------------------------------------
set.seed(5)
fit_bayes <- stan_glm(Sepal.Length ~ Species, data = iris,
                      prior = normal(0,0.1), # just to drive the point home
                      family = gaussian(),
                      refresh = 0)
c_bayes <- pairs(emmeans(fit_bayes, ~ Species))
c_bayes

## ---- message=FALSE, echo=FALSE------------------------------------------
plot(bayesfactor_parameters(c_bayes, fit_bayes)) +
  coord_cartesian(xlim = c(-2,1))

## ------------------------------------------------------------------------
hyp <- c(
  # comparing 2 levels
  "setosa > versicolor",
  "setosa > virginica",
  "versicolor > virginica",
  
  # comparing 3 (or more) levels
  "setosa    < virginica  & virginica  < versicolor",
  "virginica < setosa     & setosa     < versicolor",
  "setosa    < versicolor & versicolor < virginica"
)

## ------------------------------------------------------------------------
em_sum <- emmeans(fit_sum, ~Species)
em_sum # the posterior marginal means
  
bayesfactor_restricted(em_sum, fit_sum, hypothesis = hyp)

## ------------------------------------------------------------------------
em_bayes <- emmeans(fit_bayes, ~Species)
em_bayes # the posterior marginal means
  
bayesfactor_restricted(em_bayes, fit_bayes, hypothesis = hyp)

