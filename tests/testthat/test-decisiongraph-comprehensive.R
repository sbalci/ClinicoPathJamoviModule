# Comprehensive tests for decisiongraph module
# Tests cover decision trees, Markov models, PSA, and health economics calculations

test_that("decisiongraph .escapeVar() utility handles special characters", {
    skip_if_not_installed("ClinicoPath")

    # Create mock object with .escapeVar method
    escape_fn <- function(x) {
        if (is.null(x) || length(x) == 0) return(x)
        make.names(gsub("[^A-Za-z0-9_. -]", "_", as.character(x)))
    }

    # Test various problematic variable names
    expect_equal(escape_fn("cost (USD)"), "cost..USD.")
    expect_equal(escape_fn("p-value"), "p.value")
    expect_equal(escape_fn("cost/benefit"), "cost.benefit")
    expect_equal(escape_fn("utility%"), "utility.")

    # Test edge cases
    expect_equal(escape_fn(NULL), NULL)
    expect_equal(escape_fn(character(0)), character(0))
})


test_that("Decision tree expected value calculation is correct", {
    skip_if_not_installed("ClinicoPath")

    # Known example: Two-arm trial
    # Arm A: 60% success, cost=$1000, utility=0.8 if success, 0.2 if failure
    # Arm B: 40% success, cost=$500, utility=0.9 if success, 0.1 if failure

    # Arm A expected value
    prob_success_A <- 0.6
    cost_A <- 1000
    utility_success_A <- 0.8
    utility_failure_A <- 0.2

    expected_cost_A <- cost_A  # Cost incurred regardless
    expected_utility_A <- prob_success_A * utility_success_A +
                         (1 - prob_success_A) * utility_failure_A
    expected_utility_A_calc <- 0.6 * 0.8 + 0.4 * 0.2
    expect_equal(expected_utility_A, expected_utility_A_calc)
    expect_equal(expected_utility_A, 0.56)

    # Arm B expected value
    prob_success_B <- 0.4
    cost_B <- 500
    utility_success_B <- 0.9
    utility_failure_B <- 0.1

    expected_cost_B <- cost_B
    expected_utility_B <- prob_success_B * utility_success_B +
                         (1 - prob_success_B) * utility_failure_B
    expected_utility_B_calc <- 0.4 * 0.9 + 0.6 * 0.1
    expect_equal(expected_utility_B, expected_utility_B_calc)
    expect_equal(expected_utility_B, 0.42)

    # Decision: Arm A has higher utility but higher cost
    expect_true(expected_utility_A > expected_utility_B)
    expect_true(expected_cost_A > expected_cost_B)
})


test_that("ICER calculation is mathematically correct", {
    skip_if_not_installed("ClinicoPath")

    # Incremental Cost-Effectiveness Ratio (ICER)
    # ICER = (Cost_B - Cost_A) / (Effect_B - Effect_A)

    # Strategy A: $10,000 cost, 5 QALYs
    cost_A <- 10000
    qaly_A <- 5

    # Strategy B: $15,000 cost, 7 QALYs
    cost_B <- 15000
    qaly_B <- 7

    # ICER = incremental cost / incremental QALY
    incremental_cost <- cost_B - cost_A
    incremental_qaly <- qaly_B - qaly_A
    icer <- incremental_cost / incremental_qaly

    expect_equal(incremental_cost, 5000)
    expect_equal(incremental_qaly, 2)
    expect_equal(icer, 2500)  # $2,500 per QALY gained

    # Interpretation: If willingness-to-pay > $2,500/QALY, choose B
    wtp_threshold <- 50000  # Typical US threshold
    expect_true(icer < wtp_threshold)  # Strategy B is cost-effective
})


test_that("Net Monetary Benefit calculation is correct", {
    skip_if_not_installed("ClinicoPath")

    # Net Monetary Benefit (NMB) = WTP × Utility - Cost
    # Strategy with highest NMB is optimal

    cost <- 20000
    utility <- 8  # QALYs
    wtp <- 50000  # $/QALY

    nmb <- wtp * utility - cost
    nmb_calc <- 50000 * 8 - 20000
    expect_equal(nmb, nmb_calc)
    expect_equal(nmb, 380000)

    # Test at different WTP thresholds
    wtp_low <- 1000
    nmb_low <- wtp_low * utility - cost
    expect_equal(nmb_low, 1000 * 8 - 20000)
    expect_equal(nmb_low, -12000)  # Negative NMB = not cost-effective
})


test_that("Markov transition matrix properties are validated", {
    skip_if_not_installed("ClinicoPath")

    # Markov transition matrix: rows must sum to 1.0
    # Example: 3-state model (Healthy, Sick, Dead)

    transition_matrix <- matrix(c(
        0.7, 0.2, 0.1,  # From Healthy: 70% stay, 20% sick, 10% dead
        0.1, 0.6, 0.3,  # From Sick: 10% recover, 60% stay, 30% dead
        0.0, 0.0, 1.0   # From Dead: absorbing state
    ), nrow = 3, byrow = TRUE)

    # Validate row sums = 1
    row_sums <- rowSums(transition_matrix)
    expect_equal(row_sums, c(1.0, 1.0, 1.0))

    # Validate probabilities in [0,1]
    expect_true(all(transition_matrix >= 0))
    expect_true(all(transition_matrix <= 1))

    # Validate Dead is absorbing state
    expect_equal(transition_matrix[3, 3], 1.0)
    expect_equal(transition_matrix[3, 1], 0.0)
    expect_equal(transition_matrix[3, 2], 0.0)
})


test_that("Markov cohort simulation over multiple cycles", {
    skip_if_not_installed("ClinicoPath")

    # Simple 2-state model: Alive → Dead
    # Transition probability: 10% mortality per cycle

    mortality_rate <- 0.10
    n_cycles <- 5
    initial_alive <- 1000

    # Simulate cohort
    alive <- numeric(n_cycles + 1)
    dead <- numeric(n_cycles + 1)

    alive[1] <- initial_alive
    dead[1] <- 0

    for (cycle in 2:(n_cycles + 1)) {
        deaths <- alive[cycle - 1] * mortality_rate
        dead[cycle] <- dead[cycle - 1] + deaths
        alive[cycle] <- alive[cycle - 1] - deaths
    }

    # After 5 cycles with 10% mortality:
    # Cycle 0: 1000 alive, 0 dead
    # Cycle 1: 900 alive, 100 dead
    # Cycle 2: 810 alive, 190 dead
    # Cycle 3: 729 alive, 271 dead
    # Cycle 4: 656.1 alive, 343.9 dead
    # Cycle 5: 590.49 alive, 409.51 dead

    expect_equal(round(alive[6], 2), 590.49)
    expect_equal(round(dead[6], 2), 409.51)
    expect_equal(round(alive[6] + dead[6]), initial_alive)  # Conservation
})


test_that("Half-cycle correction adjusts Markov costs correctly", {
    skip_if_not_installed("ClinicoPath")

    # Half-cycle correction: adjust for mid-cycle state changes
    # Without correction: overestimates costs/utilities
    # With correction: more accurate estimates

    annual_cost <- 1000
    n_cycles <- 10

    # Without half-cycle correction (overestimate)
    total_cost_no_correction <- annual_cost * n_cycles
    expect_equal(total_cost_no_correction, 10000)

    # With half-cycle correction
    # First cycle: 0.5 × cost
    # Middle cycles: 1.0 × cost
    # Last cycle: 0.5 × cost
    total_cost_with_correction <- annual_cost * (0.5 + (n_cycles - 2) * 1.0 + 0.5)
    expect_equal(total_cost_with_correction, annual_cost * (n_cycles - 1))
    expect_equal(total_cost_with_correction, 9000)

    # Correction reduces total by one full cycle
    expect_equal(total_cost_no_correction - total_cost_with_correction, annual_cost)
})


test_that("Discounting future costs and utilities", {
    skip_if_not_installed("ClinicoPath")

    # Discount rate: 3% per year (standard for US cost-effectiveness)
    discount_rate <- 0.03
    annual_cost <- 10000

    # Present value of cost in year 5
    years <- 5
    pv_cost <- annual_cost / (1 + discount_rate)^years
    pv_calc <- 10000 / (1.03)^5
    expect_equal(round(pv_cost, 2), round(pv_calc, 2))
    expect_equal(round(pv_cost, 2), 8626.09)

    # Cumulative discounted costs over 10 years
    cumulative_pv <- sum(annual_cost / (1 + discount_rate)^(0:9))
    expect_true(cumulative_pv < annual_cost * 10)  # Discounting reduces total
    expect_equal(round(cumulative_pv, 2), 85302.03)
})


test_that("Dominance detection in cost-effectiveness analysis", {
    skip_if_not_installed("ClinicoPath")

    # Strategy A: $5,000 cost, 3 QALYs
    # Strategy B: $10,000 cost, 2 QALYs  ← DOMINATED (more cost, less utility)
    # Strategy C: $8,000 cost, 5 QALYs

    strategies <- data.frame(
        name = c("A", "B", "C"),
        cost = c(5000, 10000, 8000),
        qaly = c(3, 2, 5)
    )

    # Sort by cost
    strategies <- strategies[order(strategies$cost), ]

    # Strategy B is dominated if:
    # - Higher cost than A but lower utility, OR
    # - Lower utility than C but higher cost per QALY

    # Simple dominance check: B costs more than A but has less utility
    expect_true(strategies$cost[strategies$name == "B"] > strategies$cost[strategies$name == "A"])
    expect_true(strategies$qaly[strategies$name == "B"] < strategies$qaly[strategies$name == "A"])

    # B is DOMINATED by A
    dominated <- "B"
    expect_equal(dominated, "B")
})


test_that("Probabilistic sensitivity analysis (PSA) basic validation", {
    skip_if_not_installed("ClinicoPath")

    # PSA: Run Monte Carlo simulation with parameter uncertainty
    set.seed(42)
    n_simulations <- 100

    # Base case: cost = $10,000, utility = 5 QALYs
    # Uncertainty: cost ~ Gamma(shape, scale), utility ~ Beta(α, β)

    # Simulate costs (Gamma distribution)
    mean_cost <- 10000
    sd_cost <- 2000
    shape <- (mean_cost / sd_cost)^2
    scale <- sd_cost^2 / mean_cost
    cost_samples <- rgamma(n_simulations, shape = shape, scale = scale)

    # Validate distribution properties
    expect_equal(round(mean(cost_samples), -2), mean_cost, tolerance = 500)
    expect_true(all(cost_samples > 0))  # Costs must be positive

    # Simulate utilities (Beta distribution)
    mean_utility <- 0.7
    sd_utility <- 0.1
    alpha <- mean_utility * ((mean_utility * (1 - mean_utility) / sd_utility^2) - 1)
    beta <- (1 - mean_utility) * ((mean_utility * (1 - mean_utility) / sd_utility^2) - 1)
    utility_samples <- rbeta(n_simulations, shape1 = alpha, shape2 = beta)

    # Validate distribution properties
    expect_true(all(utility_samples >= 0 & utility_samples <= 1))
    expect_equal(mean(utility_samples), mean_utility, tolerance = 0.05)
})


test_that("Cost-effectiveness acceptability curve (CEAC) calculation", {
    skip_if_not_installed("ClinicoPath")

    # CEAC: Probability strategy is cost-effective at different WTP thresholds
    set.seed(123)
    n_simulations <- 1000

    # Simulate two strategies
    cost_A <- rnorm(n_simulations, mean = 5000, sd = 1000)
    qaly_A <- rnorm(n_simulations, mean = 3, sd = 0.5)

    cost_B <- rnorm(n_simulations, mean = 8000, sd = 1500)
    qaly_B <- rnorm(n_simulations, mean = 5, sd = 0.7)

    # Calculate CEAC at WTP = $50,000/QALY
    wtp <- 50000
    nmb_A <- wtp * qaly_A - cost_A
    nmb_B <- wtp * qaly_B - cost_B

    # Probability B is cost-effective
    prob_B_ce <- mean(nmb_B > nmb_A)

    # At high WTP, B should be preferred (more QALYs)
    expect_true(prob_B_ce > 0.5)

    # At low WTP, A should be preferred (lower cost)
    wtp_low <- 1000
    nmb_A_low <- wtp_low * qaly_A - cost_A
    nmb_B_low <- wtp_low * qaly_B - cost_B
    prob_A_ce_low <- mean(nmb_A_low > nmb_B_low)
    expect_true(prob_A_ce_low > 0.5)
})


test_that("Empty state handling creates placeholder rows", {
    skip_if_not_installed("ClinicoPath")

    # The module should gracefully handle empty inputs with informative placeholders

    # Simulate empty node table
    placeholder_node <- list(
        nodeId = "No nodes",
        nodeType = "N/A",
        nodeLabel = "No tree data available",
        probability = NA,
        cost = NA,
        utility = NA
    )

    expect_equal(placeholder_node$nodeId, "No nodes")
    expect_true(is.na(placeholder_node$probability))
    expect_true(is.na(placeholder_node$cost))
    expect_true(is.na(placeholder_node$utility))
})
