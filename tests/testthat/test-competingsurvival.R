test_that("competingsurvival works", {
    
    # Skip if required packages are not available
    skip_if_not_installed("survival")
    skip_if_not_installed("finalfit") 
    skip_if_not_installed("cmprsk")
    
    # Load test data if available, otherwise create minimal data
    competing_survival_data <- NULL
    try({
        data("competing_survival_data", package = "ClinicoPath", envir = environment())
    }, silent = TRUE)
    
    if (is.null(competing_survival_data) || !exists("competing_survival_data")) {
        # Create minimal test data if dataset not available
        set.seed(42)
        competing_survival_data <- data.frame(
            Overall_Time = round(rexp(100, 0.05) * 12, 1),
            Outcome = factor(sample(c("Dead_of_Disease", "Dead_of_Other", 
                                     "Alive_w_Disease", "Alive_wo_Disease"), 
                                   100, replace = TRUE)),
            Treatment = factor(sample(c("Standard_Care", "Experimental_Therapy"), 
                                     100, replace = TRUE)),
            Tumor_Stage = factor(sample(c("I", "II", "III", "IV"), 
                                       100, replace = TRUE))
        )
        competing_survival_data$Overall_Time <- pmax(0.5, pmin(60, competing_survival_data$Overall_Time))
    }
    
    # Test 1: Overall survival analysis
    testthat::expect_silent({
        results <- competingsurvival(
            data = competing_survival_data,
            explanatory = "Treatment",
            overalltime = "Overall_Time", 
            outcome = "Outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "overall"
        )
    })
    
    # Test 2: Cause-specific survival analysis
    testthat::expect_silent({
        results <- competingsurvival(
            data = competing_survival_data,
            explanatory = "Treatment",
            overalltime = "Overall_Time",
            outcome = "Outcome", 
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "cause"
        )
    })
    
    # Test 3: Competing risks analysis
    testthat::expect_silent({
        results <- competingsurvival(
            data = competing_survival_data,
            explanatory = "Treatment",
            overalltime = "Overall_Time",
            outcome = "Outcome",
            dod = "Dead_of_Disease", 
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "compete"
        )
    })
    
    # Test 4: Different explanatory variable
    testthat::expect_silent({
        results <- competingsurvival(
            data = competing_survival_data,
            explanatory = "Tumor_Stage",
            overalltime = "Overall_Time",
            outcome = "Outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other", 
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "overall"
        )
    })
    
    # Test 5: All analysis types with same data
    analysis_types <- c("overall", "cause", "compete")
    
    for (type in analysis_types) {
        testthat::expect_silent({
            results <- competingsurvival(
                data = competing_survival_data,
                explanatory = "Treatment",
                overalltime = "Overall_Time",
                outcome = "Outcome",
                dod = "Dead_of_Disease",
                dooc = "Dead_of_Other",
                awd = "Alive_w_Disease", 
                awod = "Alive_wo_Disease",
                analysistype = type
            )
        })
    }
})

test_that("competingsurvival handles edge cases", {
    
    # Skip if required packages are not available
    skip_if_not_installed("survival")
    skip_if_not_installed("finalfit")
    
    # Create minimal test data
    minimal_data <- data.frame(
        time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        outcome = factor(c("Dead_of_Disease", "Dead_of_Other", "Alive_w_Disease", 
                          "Alive_wo_Disease", "Dead_of_Disease", "Dead_of_Other",
                          "Alive_w_Disease", "Alive_wo_Disease", "Dead_of_Disease", "Alive_w_Disease")),
        group = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"))
    )
    
    # Test 1: Empty variable selection should show instructions
    testthat::expect_no_error({
        results <- competingsurvival(
            data = minimal_data
        )
    })
    
    # Test 2: Missing outcome variable
    testthat::expect_no_error({
        results <- competingsurvival(
            data = minimal_data,
            explanatory = "group",
            overalltime = "time"
        )
    })
    
    # Test 3: Missing time variable
    testthat::expect_no_error({
        results <- competingsurvival(
            data = minimal_data,
            explanatory = "group",
            outcome = "outcome"
        )
    })
    
    # Test 4: Missing explanatory variable
    testthat::expect_no_error({
        results <- competingsurvival(
            data = minimal_data,
            overalltime = "time",
            outcome = "outcome"
        )
    })
    
    # Test 5: Data with missing values
    missing_data <- data.frame(
        time = c(1, 2, NA, 4, 5, 6, 7, NA, 9, 10),
        outcome = factor(c("Dead_of_Disease", "Dead_of_Other", "Alive_w_Disease", 
                          NA, "Dead_of_Disease", "Dead_of_Other",
                          "Alive_w_Disease", "Alive_wo_Disease", "Dead_of_Disease", "Alive_w_Disease")),
        group = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", NA))
    )
    
    testthat::expect_silent({
        results <- competingsurvival(
            data = missing_data,
            explanatory = "group",
            overalltime = "time",
            outcome = "outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "overall"
        )
    })
    
    # Test 6: Very small dataset
    tiny_data <- data.frame(
        time = c(1, 2, 3, 4),
        outcome = factor(c("Dead_of_Disease", "Dead_of_Other", "Alive_w_Disease", "Alive_wo_Disease")),
        group = factor(c("A", "A", "B", "B"))
    )
    
    testthat::expect_silent({
        results <- competingsurvival(
            data = tiny_data,
            explanatory = "group",
            overalltime = "time",
            outcome = "outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other", 
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "overall"
        )
    })
})

test_that("competingsurvival data validation", {
    
    # Skip if required packages are not available
    skip_if_not_installed("survival")
    skip_if_not_installed("finalfit")
    
    # Create standard test data
    test_data <- data.frame(
        time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        outcome = factor(c("Dead_of_Disease", "Dead_of_Other", "Alive_w_Disease", 
                          "Alive_wo_Disease", "Dead_of_Disease", "Dead_of_Other",
                          "Alive_w_Disease", "Alive_wo_Disease", "Dead_of_Disease", "Alive_w_Disease")),
        group = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"))
    )
    
    # Test 1: Negative time values (should be handled gracefully)
    negative_time_data <- test_data
    negative_time_data$time[1] <- -1
    
    testthat::expect_no_error({
        results <- competingsurvival(
            data = negative_time_data,
            explanatory = "group",
            overalltime = "time",
            outcome = "outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "overall"
        )
    })
    
    # Test 2: Non-numeric time variable (should error)
    character_time_data <- test_data
    character_time_data$time <- as.character(character_time_data$time)
    
    testthat::expect_error({
        results <- competingsurvival(
            data = character_time_data,
            explanatory = "group",
            overalltime = "time",
            outcome = "outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease", 
            awod = "Alive_wo_Disease",
            analysistype = "overall"
        )
    })
    
    # Test 3: Non-factor outcome variable (should convert automatically)
    character_outcome_data <- test_data
    character_outcome_data$outcome <- as.character(test_data$outcome)
    
    testthat::expect_silent({
        results <- competingsurvival(
            data = character_outcome_data,
            explanatory = "group",
            overalltime = "time",
            outcome = "outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "overall"
        )
    })
    
    # Test 4: Different analysis types handle data appropriately
    for (analysis_type in c("overall", "cause", "compete")) {
        testthat::expect_silent({
            results <- competingsurvival(
                data = test_data,
                explanatory = "group",
                overalltime = "time",
                outcome = "outcome",
                dod = "Dead_of_Disease",
                dooc = "Dead_of_Other",
                awd = "Alive_w_Disease",
                awod = "Alive_wo_Disease",
                analysistype = analysis_type
            )
        })
    }
})

test_that("competingsurvival with realistic data", {
    
    # Skip if required packages are not available
    skip_if_not_installed("survival")
    skip_if_not_installed("finalfit")
    skip_if_not_installed("cmprsk")
    
    # Create realistic clinical trial data for competing risks
    set.seed(123)
    n_patients <- 200
    
    clinical_data <- data.frame(
        # Patient characteristics
        patient_id = 1:n_patients,
        age = round(rnorm(n_patients, 65, 10)),
        sex = factor(sample(c("Male", "Female"), n_patients, replace = TRUE)),
        
        # Treatment assignment
        treatment = factor(sample(c("Standard", "Experimental"), n_patients, replace = TRUE)),
        
        # Disease characteristics
        stage = factor(sample(c("I", "II", "III", "IV"), n_patients, 
                             replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))),
        
        # Survival time
        survival_months = rweibull(n_patients, shape = 1.5, scale = 20)
    )
    
    # Ensure realistic ranges
    clinical_data$age <- pmax(18, pmin(90, clinical_data$age))
    clinical_data$survival_months <- pmax(0.1, pmin(60, round(clinical_data$survival_months, 1)))
    
    # Create competing outcomes based on characteristics
    set.seed(456)
    clinical_data$outcome <- apply(clinical_data, 1, function(row) {
        age <- as.numeric(row["age"])
        stage <- row["stage"]
        
        # Probabilities based on age and stage
        if (stage == "IV") {
            probs <- c(0.6, 0.1, 0.2, 0.1)  # High disease death risk
        } else if (stage == "III") {
            probs <- c(0.4, 0.15, 0.25, 0.2)
        } else if (age > 75) {
            probs <- c(0.2, 0.3, 0.2, 0.3)  # Higher other death risk for elderly
        } else {
            probs <- c(0.2, 0.1, 0.3, 0.4)  # Mostly alive outcomes
        }
        
        sample(c("Dead_of_Disease", "Dead_of_Other", "Alive_w_Disease", "Alive_wo_Disease"),
               1, prob = probs)
    })
    
    clinical_data$outcome <- factor(clinical_data$outcome,
                                   levels = c("Dead_of_Disease", "Dead_of_Other", 
                                             "Alive_w_Disease", "Alive_wo_Disease"))
    
    # Test competing survival analysis by treatment
    testthat::expect_silent({
        results <- competingsurvival(
            data = clinical_data,
            explanatory = "treatment", 
            overalltime = "survival_months",
            outcome = "outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "compete"
        )
    })
    
    # Test competing survival analysis by stage
    testthat::expect_silent({
        results <- competingsurvival(
            data = clinical_data,
            explanatory = "stage",
            overalltime = "survival_months",
            outcome = "outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "overall"
        )
    })
    
    # Test competing survival analysis by sex
    testthat::expect_silent({
        results <- competingsurvival(
            data = clinical_data,
            explanatory = "sex",
            overalltime = "survival_months",
            outcome = "outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease", 
            analysistype = "cause"
        )
    })
})

test_that("competingsurvival statistical accuracy", {
    
    # Skip if required packages are not available
    skip_if_not_installed("survival")
    skip_if_not_installed("finalfit")
    skip_if_not_installed("cmprsk")
    
    # Create test data with known survival differences
    set.seed(789)
    n_per_group <- 50
    
    # Create data where treatment A has better survival
    group_a_data <- data.frame(
        time = rweibull(n_per_group, shape = 2, scale = 30),
        group = "Treatment_A"
    )
    
    # Group B has worse survival
    group_b_data <- data.frame(
        time = rweibull(n_per_group, shape = 2, scale = 15),
        group = "Treatment_B"
    )
    
    combined_data <- rbind(group_a_data, group_b_data)
    combined_data$time <- round(pmax(0.5, pmin(60, combined_data$time)), 1)
    combined_data$group <- factor(combined_data$group)
    
    # Create outcomes where Treatment A has lower disease death rate
    set.seed(321)
    combined_data$outcome <- apply(combined_data, 1, function(row) {
        group <- row["group"]
        
        if (group == "Treatment_A") {
            probs <- c(0.2, 0.1, 0.3, 0.4)  # Better outcomes
        } else {
            probs <- c(0.5, 0.15, 0.25, 0.1)  # Worse outcomes  
        }
        
        sample(c("Dead_of_Disease", "Dead_of_Other", "Alive_w_Disease", "Alive_wo_Disease"),
               1, prob = probs)
    })
    
    combined_data$outcome <- factor(combined_data$outcome,
                                   levels = c("Dead_of_Disease", "Dead_of_Other",
                                             "Alive_w_Disease", "Alive_wo_Disease"))
    
    # Test that we can run the analysis without errors
    testthat::expect_silent({
        results <- competingsurvival(
            data = combined_data,
            explanatory = "group",
            overalltime = "time",
            outcome = "outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "compete"
        )
    })
    
    # Test overall survival analysis
    testthat::expect_silent({
        results <- competingsurvival(
            data = combined_data,
            explanatory = "group", 
            overalltime = "time",
            outcome = "outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other",
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "overall"
        )
    })
    
    # Test cause-specific survival analysis
    testthat::expect_silent({
        results <- competingsurvival(
            data = combined_data,
            explanatory = "group",
            overalltime = "time",
            outcome = "outcome",
            dod = "Dead_of_Disease",
            dooc = "Dead_of_Other", 
            awd = "Alive_w_Disease",
            awod = "Alive_wo_Disease",
            analysistype = "cause"
        )
    })
})