# \donttest{
# setup
set.seed(123)
library(ggstatsplot)

# two groups (*t*-test)


# more than two groups (anova)
library(WRS2)

ggwithinstats(
  data = WineTasting,
  x = Wine,
  y = Taste,
  type = "r",
  outlier.tagging = TRUE,
  outlier.label = Taster
)

# }
