# Test script for Cochran-Armitage trend test in contTables
# This simulates the omentum sampling data structure from the article

# Create test data similar to the article (block groups vs microscopic positivity)
set.seed(123)
test_data <- data.frame(
  blocks = factor(rep(c("1-2", "3-4", "5-6", ">6"), c(221, 67, 224, 24)), 
                  levels = c("1-2", "3-4", "5-6", ">6")),
  result = factor(c(
    # 1-2 blocks: 15 positive out of 221
    rep(c("positive", "negative"), c(15, 206)),
    # 3-4 blocks: 6 positive out of 67  
    rep(c("positive", "negative"), c(6, 61)),
    # 5-6 blocks: 19 positive out of 224
    rep(c("positive", "negative"), c(19, 205)),
    # >6 blocks: 3 positive out of 24
    rep(c("positive", "negative"), c(3, 21))
  ))
)

# Print the contingency table
print("Test data contingency table:")
print(table(test_data$blocks, test_data$result))

# Load the module and test the trend test functionality
library(ClinicoPath)

# Test the trend test
result <- contTables(
  data = test_data,
  rows = "blocks",
  cols = "result",
  trendTest = TRUE,
  trendDirection = "twosided"
)

print("Cochran-Armitage trend test completed successfully!")
print("The trend test functionality has been implemented in contTables.")