# Test script for pairwise comparisons in crosstable
library(ClinicoPath)

# Create test data with 3 groups
set.seed(123)
test_data <- data.frame(
  group = factor(rep(c("LumU", "LumP", "Basal"), each = 40)),
  marker = factor(sample(c("Positive", "Negative"), 120, replace = TRUE)),
  stage = factor(sample(c("II", "IIIA", "IIIB"), 120, replace = TRUE))
)

# Test without pairwise comparisons
result1 <- ClinicoPath::crosstable(
  data = test_data,
  vars = "marker",
  group = "group",
  sty = "nejm",
  pcat = "chisq",
  pairwise = FALSE
)

# Test with pairwise comparisons and no adjustment
result2 <- ClinicoPath::crosstable(
  data = test_data,
  vars = "marker",
  group = "group",
  sty = "nejm",
  pcat = "chisq",
  pairwise = TRUE,
  p_adjust = "none"
)

# Test with pairwise comparisons and Bonferroni adjustment
result3 <- ClinicoPath::crosstable(
  data = test_data,
  vars = "marker",
  group = "group",
  sty = "nejm",
  pcat = "chisq",
  pairwise = TRUE,
  p_adjust = "bonferroni"
)

# Test with Fisher's exact test
result4 <- ClinicoPath::crosstable(
  data = test_data,
  vars = "marker",
  group = "group",
  sty = "nejm",
  pcat = "fisher",
  pairwise = TRUE,
  p_adjust = "BH"
)

cat("Tests completed successfully!\n")
cat("Check the pairwiseTable output in the results for pairwise comparisons.\n")