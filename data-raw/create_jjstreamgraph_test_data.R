# Create test data for jjstreamgraph function
# This script generates various time series datasets to test stream graph functionality

library(usethis)

# Basic stream graph test data
set.seed(123)
jjstreamgraph_basic <- data.frame(
  time = rep(1:12, 4),  # 12 months
  value = c(
    cumsum(rnorm(12, 2, 1)) + 10,   # Product A
    cumsum(rnorm(12, 1.5, 0.8)) + 8,  # Product B
    cumsum(rnorm(12, 1, 0.6)) + 6,    # Product C
    cumsum(rnorm(12, 0.8, 0.4)) + 4   # Product D
  ),
  group = factor(rep(c("Product_A", "Product_B", "Product_C", "Product_D"), each = 12)),
  month = rep(month.abb, 4)
)

# Ensure all values are positive for stream graphs
jjstreamgraph_basic$value <- pmax(jjstreamgraph_basic$value, 0.1)

# Financial time series data
set.seed(456)
dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month")
n_months <- length(dates)

jjstreamgraph_financial <- data.frame(
  date = rep(dates, 5),
  revenue = c(
    # Technology sector with growth trend
    cumsum(rnorm(n_months, 5, 2)) + 100 + 0.5 * seq_len(n_months),
    # Healthcare sector with steady growth
    cumsum(rnorm(n_months, 3, 1.5)) + 80 + 0.3 * seq_len(n_months),
    # Energy sector with volatility
    cumsum(rnorm(n_months, 2, 3)) + 60 + sin(seq_len(n_months) * pi / 6) * 10,
    # Consumer goods with seasonal pattern
    cumsum(rnorm(n_months, 2.5, 1)) + 70 + 5 * sin(seq_len(n_months) * 2 * pi / 12),
    # Financial services
    cumsum(rnorm(n_months, 2, 1.8)) + 90 + 0.2 * seq_len(n_months)
  ),
  sector = factor(rep(c("Technology", "Healthcare", "Energy", "Consumer_Goods", "Financial"), each = n_months)),
  year = rep(format(dates, "%Y"), 5),
  quarter = rep(paste0("Q", ceiling(as.numeric(format(dates, "%m")) / 3)), 5)
)

# Ensure positive values
jjstreamgraph_financial$revenue <- pmax(jjstreamgraph_financial$revenue, 1)

# Web analytics test data
set.seed(789)
days <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
jjstreamgraph_web_analytics <- data.frame(
  date = rep(days, 6),
  visitors = c(
    # Organic search with weekly pattern
    500 + 100 * sin(seq_along(days) * 2 * pi / 7) + cumsum(rnorm(length(days), 0, 20)),
    # Direct traffic
    300 + 50 * sin(seq_along(days) * 2 * pi / 7) + cumsum(rnorm(length(days), 0, 15)),
    # Social media with viral spikes
    200 + cumsum(rnorm(length(days), 0, 25)) + ifelse(days %in% sample(days, 20), 500, 0),
    # Email campaigns (weekly spikes)
    100 + ifelse(weekdays(days) == "Tuesday", 200, 0) + cumsum(rnorm(length(days), 0, 10)),
    # Paid advertising
    150 + cumsum(rnorm(length(days), 0.2, 12)),
    # Referral traffic
    80 + cumsum(rnorm(length(days), 0, 8))
  ),
  traffic_source = factor(rep(c("Organic_Search", "Direct", "Social_Media", "Email", "Paid_Ads", "Referral"), each = length(days))),
  day_of_week = rep(weekdays(days), 6),
  month = rep(format(days, "%B"), 6)
)

# Ensure positive values
jjstreamgraph_web_analytics$visitors <- pmax(jjstreamgraph_web_analytics$visitors, 1)

# Sales by region data
set.seed(1001)
quarters <- rep(1:20, 4)  # 5 years of quarterly data
jjstreamgraph_sales <- data.frame(
  quarter = quarters,
  sales = c(
    # North America with steady growth
    cumsum(rnorm(20, 8, 3)) + 200 + 2 * quarters[1:20],
    # Europe with seasonal variation
    cumsum(rnorm(20, 6, 2.5)) + 150 + 10 * sin(quarters[1:20] * pi / 2),
    # Asia Pacific with rapid growth
    cumsum(rnorm(20, 10, 4)) + 100 + 5 * quarters[1:20],
    # Latin America with moderate growth
    cumsum(rnorm(20, 4, 2)) + 80 + 1.5 * quarters[1:20]
  ),
  region = factor(rep(c("North_America", "Europe", "Asia_Pacific", "Latin_America"), each = 20)),
  year = rep(ceiling(quarters[1:20] / 4) + 2019, 4),
  quarter_label = rep(paste0("Q", ((quarters[1:20] - 1) %% 4) + 1), 4)
)

# Ensure positive values
jjstreamgraph_sales$sales <- pmax(jjstreamgraph_sales$sales, 1)

# Clinical research enrollment data
set.seed(2002)
weeks <- 1:52
jjstreamgraph_clinical <- data.frame(
  week = rep(weeks, 5),
  patients = c(
    # Site 1 - early enrollment
    c(rep(0, 5), cumsum(rpois(47, 3))),
    # Site 2 - mid-study start
    c(rep(0, 12), cumsum(rpois(40, 4))),
    # Site 3 - late start but high enrollment
    c(rep(0, 20), cumsum(rpois(32, 6))),
    # Site 4 - consistent enrollment
    c(rep(0, 8), cumsum(rpois(44, 2))),
    # Site 5 - irregular enrollment
    cumsum(c(rpois(26, 1), rep(0, 10), rpois(16, 5)))
  ),
  site = factor(rep(paste0("Site_", 1:5), each = 52)),
  month = rep(ceiling(weeks / 4.33), 5),
  enrollment_phase = rep(ifelse(weeks <= 16, "Early", ifelse(weeks <= 36, "Middle", "Late")), 5)
)

# Performance test data (larger dataset)
set.seed(3003)
days_large <- 1:365
jjstreamgraph_performance <- data.frame(
  day = rep(days_large, 8),
  metric = c(
    cumsum(rnorm(365, 1, 5)) + 100,  # Metric A
    cumsum(rnorm(365, 2, 4)) + 80,   # Metric B
    cumsum(rnorm(365, 1.5, 6)) + 90, # Metric C
    cumsum(rnorm(365, 0.8, 3)) + 70, # Metric D
    cumsum(rnorm(365, 1.2, 4)) + 85, # Metric E
    cumsum(rnorm(365, 0.9, 3.5)) + 75, # Metric F
    cumsum(rnorm(365, 1.1, 4.5)) + 95, # Metric G
    cumsum(rnorm(365, 0.7, 2.8)) + 65  # Metric H
  ),
  category = factor(rep(paste0("Category_", LETTERS[1:8]), each = 365)),
  week = rep(ceiling(days_large / 7), 8),
  month = rep(ceiling(days_large / 30.4), 8)
)

# Ensure positive values
jjstreamgraph_performance$metric <- pmax(jjstreamgraph_performance$metric, 0.1)

# Edge cases test data (with missing values, outliers, etc.)
set.seed(4004)
time_points <- 1:24
jjstreamgraph_edge_cases <- data.frame(
  time = rep(time_points, 3),
  value = c(
    # Series with missing values
    c(cumsum(rnorm(20, 1, 0.5)) + 10, rep(NA, 4)),
    # Series with outliers
    c(cumsum(rnorm(22, 1, 0.5)) + 8, 100, 120),  # Two outliers
    # Series with zeros and negatives (will be adjusted)
    c(cumsum(rnorm(24, 0, 1)))
  ),
  group = factor(rep(c("Normal", "Outliers", "Irregular"), each = 24)),
  season = rep(rep(c("Spring", "Summer", "Fall", "Winter"), each = 6), 3)
)

# Adjust negative values to small positive values for stream graphs
jjstreamgraph_edge_cases$value[jjstreamgraph_edge_cases$value <= 0] <- 0.1

# Save all datasets
usethis::use_data(jjstreamgraph_basic, overwrite = TRUE)
usethis::use_data(jjstreamgraph_financial, overwrite = TRUE)
usethis::use_data(jjstreamgraph_web_analytics, overwrite = TRUE)
usethis::use_data(jjstreamgraph_sales, overwrite = TRUE)
usethis::use_data(jjstreamgraph_clinical, overwrite = TRUE)
usethis::use_data(jjstreamgraph_performance, overwrite = TRUE)
usethis::use_data(jjstreamgraph_edge_cases, overwrite = TRUE)

# Print summary information
cat("Test datasets created for jjstreamgraph:\n")
cat("1. jjstreamgraph_basic: Basic monthly product data (n=48)\n")
cat("2. jjstreamgraph_financial: Financial sector revenue over 4 years (n=240)\n")
cat("3. jjstreamgraph_web_analytics: Daily web traffic for 1 year (n=2190)\n")
cat("4. jjstreamgraph_sales: Quarterly sales by region over 5 years (n=80)\n")
cat("5. jjstreamgraph_clinical: Weekly patient enrollment across 5 sites (n=260)\n")
cat("6. jjstreamgraph_performance: Daily metrics for performance testing (n=2920)\n")
cat("7. jjstreamgraph_edge_cases: Edge cases with missing values and outliers (n=72)\n")