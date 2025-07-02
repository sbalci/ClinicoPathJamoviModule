#' Example Datasets for Benford's Law Analysis
#'
#' These datasets provide realistic examples for demonstrating Benford's Law analysis
#' across different domains including finance, fraud detection, natural phenomena, and scientific data.
#'
#' @name benford_examples
#' @docType data
#' @format Data frames with various structures for different Benford analysis scenarios
#' @author ClinicoPath package team

# Set seed for reproducible data generation
set.seed(20241202)

# Helper functions for realistic data generation
generate_power_law <- function(n, alpha = 2, xmin = 1) {
  # Generate power law distributed numbers (natural phenomena)
  u <- runif(n)
  xmin * (1 - u)^(-1 / (alpha - 1))
}

generate_lognormal_clusters <- function(n, means, sds, proportions) {
  # Generate clustered lognormal data (typical in manipulated datasets)
  cluster_assignments <- sample(length(means), n, replace = TRUE, prob = proportions)
  sapply(1:n, function(i) {
    cluster <- cluster_assignments[i]
    rlnorm(1, meanlog = means[cluster], sdlog = sds[cluster])
  })
}

# 1. Financial Data - Should Follow Benford's Law
#' @rdname benford_examples
#' @description \code{financial_data}: Realistic financial data that naturally follows Benford's Law,
#' including transaction amounts, revenues, expenses, and account balances from different business sectors.
financial_data <- data.frame(
  # Core identifiers
  transaction_id = 1:1000,
  account_id = sample(1001:9999, 1000, replace = TRUE),
  
  # Business sectors with different financial profiles
  sector = factor(sample(c("Retail", "Manufacturing", "Services", "Technology", "Healthcare"), 
                        1000, replace = TRUE, prob = c(0.25, 0.20, 0.20, 0.20, 0.15))),
  
  # Transaction amounts (naturally distributed)
  transaction_amounts = round(c(
    rlnorm(300, meanlog = 6, sdlog = 1.5),     # Small transactions ($50-$2K)
    rlnorm(400, meanlog = 8, sdlog = 1.2),     # Medium transactions ($1K-$50K)
    rlnorm(200, meanlog = 10, sdlog = 1.0),    # Large transactions ($10K-$500K)
    rlnorm(100, meanlog = 12, sdlog = 0.8)     # Very large transactions ($100K+)
  ), 2),
  
  # Monthly revenues by company
  monthly_revenues = round(c(
    rlnorm(250, meanlog = 11, sdlog = 1.5),    # Small companies
    rlnorm(400, meanlog = 13, sdlog = 1.2),    # Medium companies  
    rlnorm(250, meanlog = 15, sdlog = 1.0),    # Large companies
    rlnorm(100, meanlog = 17, sdlog = 0.8)     # Enterprise companies
  )),
  
  # Expense amounts
  expense_amounts = round(c(
    rlnorm(400, meanlog = 7, sdlog = 1.8),     # Operating expenses
    rlnorm(300, meanlog = 9, sdlog = 1.4),     # Capital expenses
    rlnorm(200, meanlog = 8.5, sdlog = 1.6),   # Research & development
    rlnorm(100, meanlog = 11, sdlog = 1.2)     # Major investments
  ), 2),
  
  # Account balances (end of period)
  account_balances = round(abs(c(
    rnorm(200, 5000, 2000),                    # Small accounts
    rlnorm(400, meanlog = 9, sdlog = 1.5),     # Medium accounts
    rlnorm(300, meanlog = 11, sdlog = 1.2),    # Large accounts
    rlnorm(100, meanlog = 13, sdlog = 1.0)     # Premium accounts
  )), 2),
  
  # Stock prices (in cents to get integers)
  stock_prices_cents = round(c(
    rlnorm(300, meanlog = 8, sdlog = 1.5),     # Penny stocks to mid-cap
    rlnorm(400, meanlog = 10, sdlog = 1.2),    # Standard stocks
    rlnorm(200, meanlog = 12, sdlog = 1.0),    # High-value stocks
    rlnorm(100, meanlog = 14, sdlog = 0.8)     # Premium stocks
  )),
  
  # Transaction dates
  transaction_date = sample(seq(as.Date("2023-01-01"), as.Date("2024-01-01"), by = "day"), 
                           1000, replace = TRUE),
  
  # Payment methods
  payment_method = factor(sample(c("Credit Card", "Bank Transfer", "Check", "Cash", "Digital Wallet"), 
                                1000, replace = TRUE, prob = c(0.4, 0.3, 0.15, 0.1, 0.05)))
)

# 2. Fraudulent/Manipulated Data - Violates Benford's Law
#' @rdname benford_examples  
#' @description \code{fraudulent_data}: Data showing signs of manipulation and fraud,
#' with unusual digit patterns, excessive rounding, and systematic biases that violate Benford's Law.
fraudulent_data <- data.frame(
  # Case identifiers
  case_id = 1:600,
  investigation_type = factor(sample(c("Expense Fraud", "Revenue Manipulation", "Procurement Fraud", 
                                     "Payroll Fraud", "Asset Misappropriation"), 
                                   600, replace = TRUE)),
  
  # Suspicious expense claims (too many round numbers)
  suspicious_expenses = c(
    # Pattern 1: Too many round hundreds
    rep(c(100, 200, 300, 400, 500, 600, 700, 800, 900), each = 20),
    # Pattern 2: Avoiding amounts near audit thresholds
    sample(c(999, 1999, 4999, 9999), 90, replace = TRUE),
    # Pattern 3: Psychological clustering
    sample(c(750, 1250, 1750, 2250, 2750), 100, replace = TRUE),
    # Pattern 4: Copy-paste errors (repeated amounts)
    rep(c(1247.83, 2156.77, 987.45), each = 30),
    # Pattern 5: Fabricated "random" amounts (avoiding certain digits)
    sample(c(1234, 1357, 2468, 3579, 4681), 140, replace = TRUE)
  ),
  
  # Manipulated revenue figures
  manipulated_revenues = c(
    # Systematic rounding to meet targets
    rep(seq(50000, 500000, 50000), each = 25),
    # Clustering just above thresholds
    rep(c(1001000, 2001000, 5001000), each = 40),
    # Fabricated growth patterns
    rep(c(1100000, 1210000, 1331000, 1464100), each = 30),
    # Copy-paste manipulations
    rep(c(3456789, 4567890, 5678901), each = 33),
    # Additional pattern to reach 600
    sample(c(9876543, 8765432, 7654321), 11, replace = TRUE)
  ),
  
  # Procurement fraud patterns
  procurement_amounts = c(
    # Just under authorization limits
    rep(c(4999, 9999, 24999, 49999, 99999), each = 30),
    # Splitting invoices (same vendor, similar amounts)
    rep(c(2500, 2501, 2502, 2503, 2504), each = 25),
    # Round number preferences
    rep(c(1000, 2000, 3000, 5000, 10000), each = 30),
    # Fabricated invoices with "random" amounts
    sample(c(8765, 7654, 6543, 5432, 4321), 175, replace = TRUE)
  ),
  
  # Payroll manipulation
  payroll_amounts = c(
    # Excessive overtime patterns
    rep(c(5000, 5500, 6000, 6500, 7000), each = 44),
    # Ghost employee salaries
    rep(c(3500, 4200, 4800, 5200), each = 35),
    # Bonus manipulation
    rep(c(10000, 15000, 20000, 25000), each = 35),
    # Rounded salary adjustments
    rep(c(500, 1000, 1500, 2000, 2500), each = 20)
  ),
  
  # Red flags indicators
  red_flags_score = sample(1:10, 600, replace = TRUE, prob = c(0.05, 0.1, 0.15, 0.2, 0.2, 0.15, 0.1, 0.05, 0.02, 0.01)),
  
  # Investigation outcomes
  confirmed_fraud = factor(sample(c("Confirmed", "Suspected", "Cleared"), 
                                600, replace = TRUE, prob = c(0.3, 0.4, 0.3)))
)

# 3. Natural Phenomena Data - Should Follow Benford's Law
#' @rdname benford_examples
#' @description \code{natural_phenomena_data}: Data from natural processes and phenomena
#' that naturally follow Benford's Law, including populations, physical measurements, and environmental data.
natural_phenomena_data <- data.frame(
  # Observation identifiers
  observation_id = 1:800,
  phenomenon_type = factor(sample(c("Population", "Geographic", "Physical", "Biological", "Astronomical"), 
                                 800, replace = TRUE)),
  
  # City populations (classic Benford's Law example)
  city_populations = round(c(
    # Small cities and towns
    10^(runif(200, 3.5, 5.0)),
    # Medium cities
    10^(runif(300, 4.5, 6.0)),  
    # Large cities
    10^(runif(200, 5.5, 7.0)),
    # Megacities  
    10^(runif(100, 6.5, 8.0))
  )),
  
  # River lengths (in meters)
  river_lengths = round(c(
    # Streams and small rivers
    generate_power_law(200, alpha = 2.5, xmin = 1000),
    # Medium rivers
    generate_power_law(300, alpha = 2.2, xmin = 10000),
    # Large rivers
    generate_power_law(200, alpha = 2.0, xmin = 100000),
    # Major river systems
    generate_power_law(100, alpha = 1.8, xmin = 500000)
  )),
  
  # Mountain heights (in meters)
  mountain_heights = round(c(
    # Hills and small mountains
    rlnorm(200, meanlog = 6.5, sdlog = 0.8),
    # Medium mountains
    rlnorm(300, meanlog = 7.5, sdlog = 0.6),
    # High mountains
    rlnorm(200, meanlog = 8.2, sdlog = 0.5),
    # Extreme peaks
    rlnorm(100, meanlog = 8.8, sdlog = 0.4)
  )),
  
  # Lake areas (in square kilometers)
  lake_areas = round(c(
    # Small lakes and ponds
    generate_power_law(300, alpha = 2.8, xmin = 1),
    # Medium lakes
    generate_power_law(300, alpha = 2.3, xmin = 10),
    # Large lakes
    generate_power_law(150, alpha = 2.0, xmin = 100),
    # Great lakes
    generate_power_law(50, alpha = 1.7, xmin = 1000)
  ), 2),
  
  # Earthquake magnitudes (×1000 for integer analysis)
  earthquake_magnitudes_scaled = round(c(
    # Minor earthquakes
    10^(runif(300, 3.0, 4.0)),
    # Light earthquakes  
    10^(runif(250, 3.5, 4.5)),
    # Moderate earthquakes
    10^(runif(150, 4.0, 5.0)),
    # Strong to major earthquakes
    10^(runif(100, 4.5, 6.0))
  )),
  
  # Physical constants and measurements (scaled for integers)
  physical_measurements = round(c(
    # Molecular weights
    rlnorm(200, meanlog = 4, sdlog = 1.5),
    # Distances in space (astronomical units × 1000)
    rlnorm(200, meanlog = 8, sdlog = 2.0),
    # Energy measurements (joules × 10^6)
    rlnorm(200, meanlog = 6, sdlog = 1.8),
    # Time intervals (seconds × 1000)
    rlnorm(200, meanlog = 5, sdlog = 2.2)
  )),
  
  # Data collection metadata
  measurement_date = sample(seq(as.Date("2020-01-01"), as.Date("2024-01-01"), by = "day"), 
                           800, replace = TRUE),
  data_source = factor(sample(c("NASA", "USGS", "NOAA", "Census Bureau", "Scientific Journal"), 
                             800, replace = TRUE)),
  measurement_precision = factor(sample(c("High", "Medium", "Low"), 800, replace = TRUE, prob = c(0.4, 0.4, 0.2)))
)

# 4. Scientific Research Data - Mixed Benford Compliance
#' @rdname benford_examples
#' @description \code{scientific_data}: Research data from various scientific disciplines
#' with varying degrees of Benford's Law compliance, useful for testing analysis robustness.
scientific_data <- data.frame(
  # Study identifiers
  study_id = 1:500,
  discipline = factor(sample(c("Physics", "Chemistry", "Biology", "Medicine", "Environmental Science"), 
                           500, replace = TRUE)),
  
  # Research measurements
  experimental_values = c(
    # High-precision physics measurements
    round(rlnorm(100, meanlog = 5, sdlog = 2.0), 4) * 10000,
    # Chemical concentrations (ppb)
    round(rlnorm(100, meanlog = 7, sdlog = 1.5)),
    # Biological counts
    round(generate_power_law(100, alpha = 2.2, xmin = 10)),
    # Medical measurements
    round(rlnorm(100, meanlog = 6, sdlog = 1.8), 2) * 100,
    # Environmental data
    round(rlnorm(100, meanlog = 8, sdlog = 1.6))
  ),
  
  # Sample sizes (should follow power law)
  sample_sizes = round(c(
    generate_power_law(150, alpha = 2.5, xmin = 10),    # Small studies
    generate_power_law(200, alpha = 2.2, xmin = 50),    # Medium studies
    generate_power_law(100, alpha = 2.0, xmin = 200),   # Large studies
    generate_power_law(50, alpha = 1.8, xmin = 1000)    # Meta-analyses
  )),
  
  # Research funding amounts
  funding_amounts = round(c(
    rlnorm(125, meanlog = 10, sdlog = 1.5),   # Small grants
    rlnorm(200, meanlog = 12, sdlog = 1.2),   # Standard grants
    rlnorm(125, meanlog = 14, sdlog = 1.0),   # Large grants
    rlnorm(50, meanlog = 16, sdlog = 0.8)     # Major initiatives
  )),
  
  # Publication metrics
  citation_counts = round(c(
    generate_power_law(200, alpha = 2.8, xmin = 1),     # New papers
    generate_power_law(200, alpha = 2.3, xmin = 5),     # Established papers
    generate_power_law(75, alpha = 2.0, xmin = 20),     # Popular papers
    generate_power_law(25, alpha = 1.7, xmin = 100)     # Highly cited papers
  )),
  
  # Laboratory equipment costs
  equipment_costs = round(c(
    rlnorm(150, meanlog = 8, sdlog = 1.5),    # Basic equipment
    rlnorm(200, meanlog = 10, sdlog = 1.2),   # Standard equipment
    rlnorm(100, meanlog = 12, sdlog = 1.0),   # Advanced equipment
    rlnorm(50, meanlog = 14, sdlog = 0.8)     # Cutting-edge equipment
  )),
  
  # Study metadata
  publication_year = sample(2018:2024, 500, replace = TRUE),
  peer_reviewed = factor(sample(c("Yes", "No"), 500, replace = TRUE, prob = c(0.85, 0.15))),
  data_quality = factor(sample(c("Excellent", "Good", "Fair", "Poor"), 
                              500, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)))
)

# 5. Economic and Market Data
#' @rdname benford_examples
#' @description \code{economic_data}: Economic indicators and market data
#' that naturally exhibit Benford's Law properties, including GDP, trade volumes, and market capitalizations.
economic_data <- data.frame(
  # Entity identifiers
  entity_id = 1:400,
  entity_type = factor(sample(c("Country", "Company", "Market", "Commodity", "Currency"), 
                             400, replace = TRUE)),
  
  # GDP and economic indicators (in millions USD)
  gdp_millions = round(c(
    rlnorm(100, meanlog = 12, sdlog = 2.0),   # Small economies
    rlnorm(150, meanlog = 15, sdlog = 1.5),   # Medium economies
    rlnorm(100, meanlog = 17, sdlog = 1.2),   # Large economies
    rlnorm(50, meanlog = 19, sdlog = 1.0)     # Major economies
  )),
  
  # Market capitalizations (in thousands USD)
  market_cap_thousands = round(c(
    rlnorm(100, meanlog = 10, sdlog = 2.0),   # Small cap
    rlnorm(150, meanlog = 13, sdlog = 1.5),   # Mid cap
    rlnorm(100, meanlog = 15, sdlog = 1.2),   # Large cap
    rlnorm(50, meanlog = 17, sdlog = 1.0)     # Mega cap
  )),
  
  # Trade volumes (in units)
  trade_volumes = round(c(
    generate_power_law(120, alpha = 2.5, xmin = 1000),
    generate_power_law(120, alpha = 2.2, xmin = 10000),
    generate_power_law(80, alpha = 2.0, xmin = 100000),
    generate_power_law(80, alpha = 1.8, xmin = 1000000)
  )),
  
  # Population and demographic data
  population_figures = round(c(
    10^(runif(100, 4, 6)),    # Cities
    10^(runif(150, 5, 7)),    # Metropolitan areas
    10^(runif(100, 6, 8)),    # States/provinces
    10^(runif(50, 7, 9))      # Countries
  )),
  
  # Economic indicators
  unemployment_rate_scaled = round(runif(400, 10, 250)),  # ×10 to avoid decimals
  inflation_rate_scaled = round(runif(400, 5, 150)),      # ×10 to avoid decimals
  
  # Time period
  measurement_period = factor(sample(c("Q1 2023", "Q2 2023", "Q3 2023", "Q4 2023", "Q1 2024"), 
                                   400, replace = TRUE)),
  data_source = factor(sample(c("World Bank", "IMF", "OECD", "National Statistics", "Bloomberg"), 
                             400, replace = TRUE))
)

# Export all datasets
usethis::use_data(financial_data, overwrite = TRUE)
usethis::use_data(fraudulent_data, overwrite = TRUE)
usethis::use_data(natural_phenomena_data, overwrite = TRUE)
usethis::use_data(scientific_data, overwrite = TRUE)
usethis::use_data(economic_data, overwrite = TRUE)

# Generate comprehensive summary
cat("✅ Created 5 comprehensive datasets for Benford's Law analysis:\n\n")

cat("💰 financial_data (n=1000):\n")
cat("   - Transaction amounts, revenues, expenses, account balances\n")
cat("   - Naturally follows Benford's Law from realistic business data\n")
cat("   - Multiple sectors: Retail, Manufacturing, Services, Technology, Healthcare\n")
cat("   - Perfect for demonstrating proper Benford's Law compliance\n\n")

cat("🚨 fraudulent_data (n=600):\n")
cat("   - Manipulated expenses, revenues, procurement, payroll data\n")
cat("   - Violates Benford's Law through systematic manipulation\n")
cat("   - Shows fraud patterns: round numbers, threshold avoidance, clustering\n")
cat("   - Ideal for fraud detection training and validation\n\n")

cat("🌍 natural_phenomena_data (n=800):\n")
cat("   - City populations, river lengths, mountain heights, lake areas\n")
cat("   - Earthquake data, physical measurements from natural processes\n")
cat("   - Classic examples of natural Benford's Law compliance\n")
cat("   - Demonstrates why Benford's Law appears in nature\n\n")

cat("🔬 scientific_data (n=500):\n")
cat("   - Research measurements, sample sizes, funding amounts, citations\n")
cat("   - Mixed Benford compliance from various scientific disciplines\n")
cat("   - Equipment costs and publication metrics\n")
cat("   - Tests robustness of Benford analysis methods\n\n")

cat("📈 economic_data (n=400):\n")
cat("   - GDP figures, market capitalizations, trade volumes\n")
cat("   - Population data and economic indicators\n")
cat("   - Real-world economic data patterns\n")
cat("   - Shows Benford's Law in macroeconomic contexts\n\n")

cat("💡 Usage examples:\n")
cat("   benford(data = financial_data, var = 'transaction_amounts')\n")
cat("   benford(data = fraudulent_data, var = 'suspicious_expenses')\n")
cat("   benford(data = natural_phenomena_data, var = 'city_populations')\n")
cat("   benford(data = scientific_data, var = 'sample_sizes')\n")
cat("   benford(data = economic_data, var = 'gdp_millions')\n\n")

cat("🎯 Key features:\n")
cat("   - Realistic data distributions from actual phenomena\n")
cat("   - Clear contrast between compliant and non-compliant data\n")
cat("   - Multiple domains for comprehensive testing\n")
cat("   - Sufficient sample sizes for reliable Benford analysis\n")
cat("   - Metadata for enhanced analysis and interpretation\n")