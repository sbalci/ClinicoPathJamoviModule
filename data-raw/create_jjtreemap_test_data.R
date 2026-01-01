# Create test data for jjtreemap function
# This script generates various datasets to test treemap visualization functionality

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(usethis)

# Basic treemap test data
set.seed(123)
jjtreemap_basic <- data.frame(
  category = factor(c("Electronics", "Clothing", "Food", "Books", "Sports", "Home")),
  value = c(450, 320, 280, 150, 200, 350),
  region = factor(c("North", "South", "East", "West", "Central", "North")),
  growth = c(12.5, -5.2, 8.3, 22.1, -2.7, 15.8)
)

# Market share data
set.seed(456)
jjtreemap_market <- data.frame(
  company = factor(c(
    "TechCorp", "DataSoft", "CloudNet", "AIWorks", "SecureIT",
    "WebDev", "AppMaker", "CodeBase", "DevOps", "NetSol"
  )),
  market_share = c(23.5, 18.2, 15.7, 12.3, 9.8, 7.5, 5.2, 3.8, 2.5, 1.5),
  sector = factor(c(
    "Software", "Software", "Cloud", "AI/ML", "Security",
    "Web", "Mobile", "Development", "Infrastructure", "Networking"
  )),
  revenue_millions = c(1250, 980, 850, 650, 520, 400, 280, 200, 130, 80),
  employees = c(5000, 4200, 3500, 2800, 2100, 1600, 1100, 800, 500, 300)
)

# Product portfolio data
set.seed(789)
jjtreemap_products <- data.frame(
  product_line = factor(c(
    "Smartphones", "Laptops", "Tablets", "Headphones", "Smartwatches",
    "Cameras", "Speakers", "Monitors", "Keyboards", "Mice",
    "Routers", "Printers", "Storage", "Accessories", "Software"
  )),
  revenue = c(
    4500, 3800, 2200, 1800, 1500,
    1200, 980, 850, 650, 450,
    380, 320, 280, 220, 180
  ),
  category = factor(c(
    "Mobile", "Computing", "Mobile", "Audio", "Wearables",
    "Imaging", "Audio", "Computing", "Computing", "Computing",
    "Networking", "Office", "Storage", "Accessories", "Services"
  )),
  units_sold = c(
    15000, 8500, 9200, 25000, 12000,
    3500, 8800, 4200, 18000, 35000,
    7500, 2800, 6500, 45000, 5000
  ),
  profit_margin = c(
    22.5, 18.3, 25.7, 35.2, 28.9,
    15.8, 30.5, 20.2, 40.5, 45.2,
    25.8, 12.5, 22.3, 55.8, 65.2
  )
)

# Sales by region and category (hierarchical)
set.seed(1001)
regions <- c("North America", "Europe", "Asia Pacific", "Latin America", "Middle East")
categories <- c("Electronics", "Fashion", "Home", "Sports", "Books")
jjtreemap_hierarchical <- expand.grid(
  region = factor(regions),
  category = factor(categories),
  stringsAsFactors = TRUE
)
jjtreemap_hierarchical$sales <- round(runif(nrow(jjtreemap_hierarchical), 50, 500), 2)
jjtreemap_hierarchical$growth_rate <- round(runif(nrow(jjtreemap_hierarchical), -10, 25), 1)
jjtreemap_hierarchical$market_penetration <- round(runif(nrow(jjtreemap_hierarchical), 5, 85), 1)

# Department budget allocation
set.seed(2002)
jjtreemap_budget <- data.frame(
  department = factor(c(
    "R&D", "Marketing", "Sales", "Operations", "HR",
    "IT", "Finance", "Legal", "Customer_Support", "Admin",
    "Facilities", "Security", "Training", "Quality", "Procurement"
  )),
  budget_millions = c(
    45.2, 32.5, 28.7, 38.9, 12.3,
    22.5, 15.8, 8.7, 18.5, 6.2,
    9.8, 5.5, 7.2, 11.5, 13.8
  ),
  division = factor(c(
    "Innovation", "Growth", "Growth", "Core", "Support",
    "Infrastructure", "Support", "Compliance", "Service", "Support",
    "Infrastructure", "Infrastructure", "Development", "Core", "Core"
  )),
  headcount = c(
    250, 180, 320, 450, 85,
    120, 95, 45, 280, 50,
    65, 35, 40, 95, 70
  ),
  efficiency_score = round(runif(15, 65, 95), 1)
)

# Website traffic sources
set.seed(3003)
jjtreemap_web_traffic <- data.frame(
  source = factor(c(
    "Organic_Search", "Direct", "Social_Media", "Email",
    "Paid_Search", "Display_Ads", "Referral", "Video",
    "Affiliate", "Other"
  )),
  visitors = c(
    125000, 98000, 67000, 45000,
    38000, 28000, 22000, 18000,
    12000, 8000
  ),
  channel = factor(c(
    "Search", "Direct", "Social", "Email",
    "Paid", "Paid", "Referral", "Content",
    "Partner", "Other"
  )),
  conversion_rate = c(
    3.2, 4.5, 1.8, 5.2,
    2.8, 1.2, 3.5, 2.2,
    4.1, 1.5
  ),
  avg_session_duration = c(
    185, 220, 125, 245,
    165, 95, 195, 285,
    175, 105
  )
)

# Performance test data (larger dataset)
set.seed(4004)
jjtreemap_performance <- data.frame(
  item = factor(paste0("Item_", sprintf("%03d", 1:100))),
  value = round(rexp(100, rate = 0.02) * 100, 2),
  group = factor(rep(paste0("Group_", LETTERS[1:10]), each = 10)),
  subgroup = factor(rep(paste0("SubGroup_", 1:20), each = 5)),
  metric1 = round(runif(100, 10, 100), 1),
  metric2 = round(runif(100, 0, 50), 2)
)

# Edge cases test data (with missing values, small values, etc.)
set.seed(5005)
jjtreemap_edge_cases <- data.frame(
  category = factor(c(
    "Very_Large", "Large", "Medium", "Small", "Very_Small",
    "Tiny", NA, "Zero_Value", "Negative_Handled", "Normal"
  )),
  value = c(
    10000, 1000, 100, 10, 1,
    0.1, 50, 0, -10, 150  # Negative will be handled
  ),
  type = factor(c(
    "Extreme", "High", "Normal", "Low", "Minimal",
    "Minimal", "Missing", "Zero", "Invalid", "Normal"
  )),
  secondary_value = c(
    500, 200, 100, 50, 25,
    5, NA, 0, 20, 80
  )
)

# Fix negative and zero values for treemap
jjtreemap_edge_cases$value[jjtreemap_edge_cases$value <= 0] <- 0.01

# Clinical research data (study sites and enrollment)
set.seed(6006)
jjtreemap_clinical <- data.frame(
  site = factor(paste0("Site_", sprintf("%02d", 1:20))),
  patients_enrolled = c(
    125, 98, 87, 76, 72, 68, 65, 58, 52, 48,
    45, 42, 38, 35, 32, 28, 25, 22, 18, 15
  ),
  region = factor(c(
    rep("North_America", 5), rep("Europe", 5),
    rep("Asia_Pacific", 5), rep("Latin_America", 5)
  )),
  site_type = factor(c(
    rep(c("Academic", "Community", "Private"), length.out = 20)
  )),
  enrollment_rate = round(runif(20, 0.5, 0.95), 2) * 100,
  retention_rate = round(runif(20, 0.7, 0.98), 2) * 100
)

# Save all datasets
use_data_multi_format(jjtreemap_basic, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjtreemap_market, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjtreemap_products, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjtreemap_hierarchical, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjtreemap_budget, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjtreemap_web_traffic, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjtreemap_performance, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjtreemap_edge_cases, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(jjtreemap_clinical, overwrite = TRUE, save_csv = TRUE)

# Print summary information
cat("Test datasets created for jjtreemap:\n")
cat("1. jjtreemap_basic: Basic categories with values (n=6)\n")
cat("2. jjtreemap_market: Market share data for companies (n=10)\n")
cat("3. jjtreemap_products: Product portfolio with multiple metrics (n=15)\n")
cat("4. jjtreemap_hierarchical: Hierarchical region x category data (n=25)\n")
cat("5. jjtreemap_budget: Department budget allocation (n=15)\n")
cat("6. jjtreemap_web_traffic: Website traffic sources (n=10)\n")
cat("7. jjtreemap_performance: Large dataset for performance testing (n=100)\n")
cat("8. jjtreemap_edge_cases: Edge cases with extreme values (n=10)\n")
cat("9. jjtreemap_clinical: Clinical research site enrollment (n=20)\n")
