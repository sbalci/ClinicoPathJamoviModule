## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)

set.seed(333)

## ----warning=FALSE, message=FALSE----------------------------------------
library(bayestestR)
library(dplyr)
library(ggplot2)

# Generate a normal distribution
posterior <- distribution_normal(1000)

# Compute HDI and ETI
ci_hdi <- ci(posterior, method = "HDI")
ci_eti <- ci(posterior, method = "ETI")

# Plot the distribution and add the limits of the two CIs
posterior %>% 
  estimate_density(extend=TRUE) %>% 
  ggplot(aes(x=x, y=y)) +
  geom_area(fill="orange") +
  theme_classic() +
  # HDI in blue
  geom_vline(xintercept=ci_hdi$CI_low, color="royalblue", size=3) +
  geom_vline(xintercept=ci_hdi$CI_high, color="royalblue", size=3) +
  # Quantile in red
  geom_vline(xintercept=ci_eti$CI_low, color="red", size=1) +
  geom_vline(xintercept=ci_eti$CI_high, color="red", size=1)

## ----warning=FALSE, message=FALSE----------------------------------------
library(bayestestR)
library(dplyr)
library(ggplot2)

# Generate a beta distribution
posterior <- distribution_beta(1000, 6, 2)

# Compute HDI and Quantile CI
ci_hdi <- ci(posterior, method = "HDI")
ci_eti <- ci(posterior, method = "ETI")

# Plot the distribution and add the limits of the two CIs
posterior %>% 
  estimate_density(extend=TRUE) %>% 
  ggplot(aes(x=x, y=y)) +
  geom_area(fill="orange") +
  theme_classic() +
  # HDI in blue
  geom_vline(xintercept=ci_hdi$CI_low, color="royalblue", size=3) +
  geom_vline(xintercept=ci_hdi$CI_high, color="royalblue", size=3) +
  # Quantile in red
  geom_vline(xintercept=ci_eti$CI_low, color="red", size=1) +
  geom_vline(xintercept=ci_eti$CI_high, color="red", size=1)

