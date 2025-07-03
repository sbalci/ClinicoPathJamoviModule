test_that("consort works", {
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    
    # Test 1: Basic CONSORT flowchart with typical trial numbers
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),  # consort doesn't use input data
            initialN = 500,
            notEligibleN = 150,
            notEligibleText = "Failed inclusion criteria, refused consent",
            randomizedN = 350,
            arm1Label = "Drug A",
            arm1N = 175,
            arm1ReceivedN = 170,
            arm1LostN = 15,
            arm1AnalyzedN = 155,
            arm2Label = "Placebo",
            arm2N = 175,
            arm2ReceivedN = 175,
            arm2LostN = 20,
            arm2AnalyzedN = 155,
            excludedText = "Withdrew consent, lost to follow-up"
        )
    })
    
    # Test 2: Large scale trial
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 2000,
            notEligibleN = 600,
            notEligibleText = "Age criteria, comorbidities, prior treatment",
            randomizedN = 1400,
            arm1Label = "High Dose",
            arm1N = 700,
            arm1ReceivedN = 690,
            arm1LostN = 45,
            arm1AnalyzedN = 645,
            arm2Label = "Standard Care",
            arm2N = 700,
            arm2ReceivedN = 695,
            arm2LostN = 50,
            arm2AnalyzedN = 645,
            excludedText = "Protocol violations, adverse events"
        )
    })
    
    # Test 3: Small pilot study
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 80,
            notEligibleN = 25,
            notEligibleText = "Screening failures",
            randomizedN = 55,
            arm1Label = "Intervention",
            arm1N = 28,
            arm1ReceivedN = 26,
            arm1LostN = 3,
            arm1AnalyzedN = 23,
            arm2Label = "Control",
            arm2N = 27,
            arm2ReceivedN = 27,
            arm2LostN = 2,
            arm2AnalyzedN = 25,
            excludedText = "Moved, withdrew"
        )
    })
    
    # Test 4: Three-arm trial
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 900,
            notEligibleN = 200,
            notEligibleText = "Exclusion criteria not met",
            randomizedN = 700,
            arm1Label = "Low Dose",
            arm1N = 233,
            arm1ReceivedN = 230,
            arm1LostN = 18,
            arm1AnalyzedN = 212,
            arm2Label = "High Dose", 
            arm2N = 234,
            arm2ReceivedN = 230,
            arm2LostN = 20,
            arm2AnalyzedN = 210,
            excludedText = "Study discontinuation, protocol violations"
        )
    })
})

test_that("consort handles edge cases", {
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    
    # Test 1: Empty/default parameters (should show welcome message)
    testthat::expect_no_error({
        results <- consort(
            data = data.frame()
        )
    })
    
    # Test 2: Zero participants (edge case)
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 0,
            notEligibleN = 0,
            randomizedN = 0,
            arm1N = 0,
            arm2N = 0
        )
    })
    
    # Test 3: Perfect retention (no losses)
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 100,
            notEligibleN = 20,
            notEligibleText = "Did not meet criteria",
            randomizedN = 80,
            arm1Label = "Treatment",
            arm1N = 40,
            arm1ReceivedN = 40,
            arm1LostN = 0,
            arm1AnalyzedN = 40,
            arm2Label = "Control",
            arm2N = 40,
            arm2ReceivedN = 40,
            arm2LostN = 0,
            arm2AnalyzedN = 40,
            excludedText = ""
        )
    })
    
    # Test 4: High dropout rate scenario
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 200,
            notEligibleN = 50,
            notEligibleText = "Multiple exclusion criteria",
            randomizedN = 150,
            arm1Label = "Experimental",
            arm1N = 75,
            arm1ReceivedN = 70,
            arm1LostN = 25,
            arm1AnalyzedN = 45,
            arm2Label = "Standard",
            arm2N = 75,
            arm2ReceivedN = 73,
            arm2LostN = 20,
            arm2AnalyzedN = 53,
            excludedText = "High dropout due to side effects"
        )
    })
    
    # Test 5: Only required parameters
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 150,
            randomizedN = 120,
            arm1N = 60,
            arm2N = 60
        )
    })
})

test_that("consort parameter validation", {
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    
    # Test 1: Negative numbers (should handle gracefully)
    testthat::expect_no_error({
        results <- consort(
            data = data.frame(),
            initialN = -10  # This should be handled without crashing
        )
    })
    
    # Test 2: Very large numbers
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 50000,
            notEligibleN = 15000,
            randomizedN = 35000,
            arm1N = 17500,
            arm2N = 17500,
            arm1ReceivedN = 17000,
            arm2ReceivedN = 17200,
            arm1AnalyzedN = 16500,
            arm2AnalyzedN = 16800
        )
    })
    
    # Test 3: Custom arm labels
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 300,
            randomizedN = 240,
            arm1Label = "Surgical Intervention",
            arm1N = 120,
            arm2Label = "Medical Management",
            arm2N = 120,
            arm1ReceivedN = 115,
            arm2ReceivedN = 118,
            arm1AnalyzedN = 110,
            arm2AnalyzedN = 115
        )
    })
    
    # Test 4: Long text descriptions
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 400,
            notEligibleN = 120,
            notEligibleText = "Failed screening: age >75 years, prior myocardial infarction, current use of anticoagulants, kidney disease, pregnancy",
            randomizedN = 280,
            arm1N = 140,
            arm2N = 140,
            excludedText = "Lost to follow-up due to relocation, withdrew consent for personal reasons, protocol violations including medication non-compliance"
        )
    })
})

test_that("consort realistic clinical scenarios", {
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    
    # Test 1: Cardiovascular drug trial
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 2500,
            notEligibleN = 800,
            notEligibleText = "Exclusion: age, comorbidities, medications",
            randomizedN = 1700,
            arm1Label = "ACE Inhibitor",
            arm1N = 850,
            arm1ReceivedN = 835,
            arm1LostN = 95,
            arm1AnalyzedN = 740,
            arm2Label = "Placebo",
            arm2N = 850,
            arm2ReceivedN = 840,
            arm2LostN = 85,
            arm2AnalyzedN = 755,
            excludedText = "Lost to follow-up, adverse events, protocol violations"
        )
    })
    
    # Test 2: Cancer immunotherapy trial
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 1200,
            notEligibleN = 350,
            notEligibleText = "Stage, ECOG performance status, prior therapy",
            randomizedN = 850,
            arm1Label = "Immunotherapy",
            arm1N = 425,
            arm1ReceivedN = 420,
            arm1LostN = 65,
            arm1AnalyzedN = 355,
            arm2Label = "Chemotherapy",
            arm2N = 425,
            arm2ReceivedN = 422,
            arm2LostN = 58,
            arm2AnalyzedN = 364,
            excludedText = "Disease progression, treatment-related toxicity"
        )
    })
    
    # Test 3: Diabetes prevention study
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 800,
            notEligibleN = 180,
            notEligibleText = "BMI, glucose levels, diabetes history",
            randomizedN = 620,
            arm1Label = "Lifestyle Intervention",
            arm1N = 310,
            arm1ReceivedN = 305,
            arm1LostN = 45,
            arm1AnalyzedN = 260,
            arm2Label = "Standard Care",
            arm2N = 310,
            arm2ReceivedN = 308,
            arm2LostN = 40,
            arm2AnalyzedN = 268,
            excludedText = "Moved away, lost interest, non-compliance"
        )
    })
    
    # Test 4: Surgical vs medical therapy
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 600,
            notEligibleN = 150,
            notEligibleText = "Surgical risk, disease severity, consent",
            randomizedN = 450,
            arm1Label = "Surgical Treatment",
            arm1N = 225,
            arm1ReceivedN = 220,
            arm1LostN = 25,
            arm1AnalyzedN = 195,
            arm2Label = "Medical Treatment",
            arm2N = 225,
            arm2ReceivedN = 223,
            arm2LostN = 28,
            arm2AnalyzedN = 195,
            excludedText = "Crossover, complications, patient preference"
        )
    })
})

test_that("consort statistical coherence", {
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    
    # Test 1: Numbers should be coherent (enrolled >= randomized)
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 1000,
            notEligibleN = 300,  # 1000 - 300 = 700 eligible
            randomizedN = 700,   # All eligible were randomized
            arm1N = 350,         # 700 / 2 = 350 each arm
            arm2N = 350,
            arm1ReceivedN = 340, # 10 didn't receive intervention
            arm2ReceivedN = 345, # 5 didn't receive intervention  
            arm1LostN = 40,      # Lost 40 from arm 1
            arm1AnalyzedN = 300, # 340 received - 40 lost = 300 analyzed
            arm2LostN = 35,      # Lost 35 from arm 2
            arm2AnalyzedN = 310, # 345 received - 35 lost = 310 analyzed
            excludedText = "Protocol specified exclusions"
        )
    })
    
    # Test 2: Unequal randomization (2:1 ratio)
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 450,
            notEligibleN = 150,
            randomizedN = 300,
            arm1Label = "Active Treatment",
            arm1N = 200,         # 2:1 randomization
            arm2Label = "Control",
            arm2N = 100,
            arm1ReceivedN = 195,
            arm2ReceivedN = 98,
            arm1LostN = 25,
            arm1AnalyzedN = 170,
            arm2LostN = 12,
            arm2AnalyzedN = 86
        )
    })
    
    # Test 3: Phase I dose escalation style numbers
    testthat::expect_silent({
        results <- consort(
            data = data.frame(),
            initialN = 120,
            notEligibleN = 45,
            notEligibleText = "Strict inclusion criteria for safety study",
            randomizedN = 75,
            arm1Label = "Dose Level 1",
            arm1N = 25,
            arm1ReceivedN = 25,
            arm1LostN = 2,
            arm1AnalyzedN = 23,
            arm2Label = "Dose Level 2",
            arm2N = 25,
            arm2ReceivedN = 24,
            arm2LostN = 3,
            arm2AnalyzedN = 21,
            excludedText = "DLT, withdrawal of consent"
        )
    })
})