
# Verification Script for hullplot logic

# Mocking the logic from hullplot.b.R to test without running the full Jamovi module

library(dplyr)

# 1. Test Separation Quality Logic (New Normalized Metric)
test_separation <- function(n_groups, dist_val, avg_sd) {
  if (n_groups < 2) {
    return("single cohort")
  }
  
  discrim_index <- if (is.na(avg_sd) || avg_sd == 0) 0 else dist_val / avg_sd
  
  if (is.na(discrim_index)) {
    return("unable to calculate")
  } else if (discrim_index > 3) {
    return("well-separated")
  } else if (discrim_index > 1.5) {
    return("moderately separated")
  } else {
    return("overlapping")
  }
}

# Scenario A: Well separated (High distance, Low SD)
# Dist = 14, SD = 1 -> Index = 14 -> Well separated
print(paste("Scenario A (High Dist, Low SD):", test_separation(2, 14, 1)))

# Scenario B: Overlapping (High distance, High SD)
# Dist = 14, SD = 10 -> Index = 1.4 -> Overlapping
print(paste("Scenario B (High Dist, High SD):", test_separation(2, 14, 10)))

# Scenario C: Moderately Separated
# Dist = 4, SD = 2 -> Index = 2 -> Moderately separated
print(paste("Scenario C (Med Dist, Med SD):", test_separation(2, 4, 2)))

# Scenario C: Single Group
print(paste("Single Group Verdict:", test_separation(1, NA)))


# 2. Test Small Group Handling (Fallback Logic)
# The code uses chull. Let's see what chull does with < 3 points.

test_chull <- function(n) {
  df <- data.frame(x = rnorm(n), y = rnorm(n))
  tryCatch({
    idx <- chull(df$x, df$y)
    return(paste("n=", n, ": Success, hull points=", length(idx)))
  }, error = function(e) {
    return(paste("n=", n, ": Error -", e$message))
  })
}

print(test_chull(1))
print(test_chull(2))
print(test_chull(3))

# 3. Test Outlier Logic (IQR)
# The code uses univariate IQR.
test_outliers <- function(vals) {
  q1 <- quantile(vals, 0.25)
  q3 <- quantile(vals, 0.75)
  iqr <- q3 - q1
  outliers <- which(vals < (q1 - 1.5 * iqr) | vals > (q3 + 1.5 * iqr))
  return(length(outliers))
}

# Normal data
print(paste("Outliers in normal(100):", test_outliers(rnorm(100))))
# Data with obvious outlier
vals_out <- c(rnorm(10), 100)
print(paste("Outliers with extreme value:", test_outliers(vals_out)))
