test_that("flowchart works with basic data", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    skip_if_not_installed("dplyr")
    
    # Test 1: Basic clinical trial flow
    testthat::expect_silent({
        # Load test data
        data("clinical_trial_flow")
        
        results <- flowchart(
            data = clinical_trial_flow,
            nodes = c("step1", "step2", "step3", "step4", "step5"),
            counts = c("count1", "count2", "count3", "count4", "count5"),
            direction = "TB",
            nodeWidth = 2.5,
            nodeHeight = 1,
            fontSize = 10,
            showPercentages = TRUE,
            showExclusions = TRUE,
            nodeColor = "blue",
            includeTitle = TRUE
        )
    })
    
    # Test 2: Cancer pathway flow with different parameters
    testthat::expect_silent({
        # Load test data
        data("cancer_pathway_flow")
        
        results <- flowchart(
            data = cancer_pathway_flow,
            nodes = c("node1", "node2", "node3", "node4", "node5", "node6"),
            counts = c("n1", "n2", "n3", "n4", "n5", "n6"),
            direction = "LR",
            nodeWidth = 3,
            nodeHeight = 1.5,
            fontSize = 12,
            showPercentages = FALSE,
            showExclusions = FALSE,
            nodeColor = "green",
            includeTitle = FALSE
        )
    })
    
    # Test 3: Biomarker discovery flow with mixed variable names
    testthat::expect_silent({
        # Load test data
        data("biomarker_discovery_flow")
        
        results <- flowchart(
            data = biomarker_discovery_flow,
            nodes = rep("stage_label", 6),
            counts = rep("participant_count", 6),
            direction = "TB",
            nodeColor = "gray"
        )
    })
})

test_that("flowchart handles edge cases", {
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    
    # Test 1: Empty/default parameters (should show welcome message)
    testthat::expect_no_error({
        results <- flowchart(
            data = data.frame(),
            nodes = NULL,
            counts = NULL
        )
    })
    
    # Test 2: Minimal flow (2 steps only)
    testthat::expect_silent({
        # Load test data
        data("minimal_flow")
        
        results <- flowchart(
            data = minimal_flow,
            nodes = c("node_a", "node_b"),
            counts = c("count_a", "count_b"),
            direction = "TB",
            nodeColor = "blue"
        )
    })
    
    # Test 3: Single node (edge case)
    testthat::expect_silent({
        single_node_data <- data.frame(
            single_step = "Only Step",
            single_count = 100
        )
        
        results <- flowchart(
            data = single_node_data,
            nodes = "single_step",
            counts = "single_count"
        )
    })
    
    # Test 4: Large numbers
    testthat::expect_silent({
        # Load test data
        data("epidemiology_study_flow")
        
        results <- flowchart(
            data = epidemiology_study_flow,
            nodes = rep("cohort_stage", 7),
            counts = rep("cohort_size", 7),
            direction = "TB",
            showPercentages = TRUE
        )
    })
})

test_that("flowchart parameter variations", {
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    
    # Load test data for all parameter tests
    data("clinical_trial_flow")
    
    # Test 1: Different directions
    testthat::expect_silent({
        # Top to Bottom
        results_tb <- flowchart(
            data = clinical_trial_flow,
            nodes = c("step1", "step2", "step3"),
            counts = c("count1", "count2", "count3"),
            direction = "TB"
        )
        
        # Left to Right
        results_lr <- flowchart(
            data = clinical_trial_flow,
            nodes = c("step1", "step2", "step3"),
            counts = c("count1", "count2", "count3"),
            direction = "LR"
        )
    })
    
    # Test 2: Different color schemes
    testthat::expect_silent({
        # Blue scheme
        results_blue <- flowchart(
            data = clinical_trial_flow,
            nodes = c("step1", "step2"),
            counts = c("count1", "count2"),
            nodeColor = "blue"
        )
        
        # Gray scheme
        results_gray <- flowchart(
            data = clinical_trial_flow,
            nodes = c("step1", "step2"),
            counts = c("count1", "count2"),
            nodeColor = "gray"
        )
        
        # Green scheme
        results_green <- flowchart(
            data = clinical_trial_flow,
            nodes = c("step1", "step2"),
            counts = c("count1", "count2"),
            nodeColor = "green"
        )
    })
    
    # Test 3: Different node sizes
    testthat::expect_silent({
        # Small nodes
        results_small <- flowchart(
            data = clinical_trial_flow,
            nodes = c("step1", "step2"),
            counts = c("count1", "count2"),
            nodeWidth = 1.5,
            nodeHeight = 0.8,
            fontSize = 8
        )
        
        # Large nodes
        results_large <- flowchart(
            data = clinical_trial_flow,
            nodes = c("step1", "step2"),
            counts = c("count1", "count2"),
            nodeWidth = 4,
            nodeHeight = 2,
            fontSize = 14
        )
    })
    
    # Test 4: Toggle options
    testthat::expect_silent({
        # All options off
        results_minimal <- flowchart(
            data = clinical_trial_flow,
            nodes = c("step1", "step2", "step3"),
            counts = c("count1", "count2", "count3"),
            showPercentages = FALSE,
            showExclusions = FALSE,
            includeTitle = FALSE
        )
        
        # All options on
        results_full <- flowchart(
            data = clinical_trial_flow,
            nodes = c("step1", "step2", "step3"),
            counts = c("count1", "count2", "count3"),
            showPercentages = TRUE,
            showExclusions = TRUE,
            includeTitle = TRUE
        )
    })
})

test_that("flowchart data validation", {
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    
    # Test 1: Mismatched node and count variables
    testthat::expect_error({
        data("clinical_trial_flow")
        
        flowchart(
            data = clinical_trial_flow,
            nodes = c("step1", "step2", "step3"),  # 3 variables
            counts = c("count1", "count2")         # 2 variables - mismatch
        )
    })
    
    # Test 2: Empty data frame
    testthat::expect_error({
        empty_data <- data.frame()
        
        flowchart(
            data = empty_data,
            nodes = "step1",
            counts = "count1"
        )
    })
    
    # Test 3: Non-existent variables
    testthat::expect_error({
        data("clinical_trial_flow")
        
        flowchart(
            data = clinical_trial_flow,
            nodes = c("nonexistent_node"),
            counts = c("nonexistent_count")
        )
    })
    
    # Test 4: Zero participants (should handle gracefully)
    testthat::expect_silent({
        zero_data <- data.frame(
            step_name = "Zero Step",
            step_count = 0
        )
        
        results <- flowchart(
            data = zero_data,
            nodes = "step_name",
            counts = "step_count"
        )
    })
    
    # Test 5: Negative numbers (should handle gracefully)
    testthat::expect_silent({
        negative_data <- data.frame(
            step_name = "Negative Step",
            step_count = -10
        )
        
        results <- flowchart(
            data = negative_data,
            nodes = "step_name",
            counts = "step_count"
        )
    })
})

test_that("flowchart realistic clinical scenarios", {
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    
    # Test 1: Multi-arm clinical trial
    testthat::expect_silent({
        data("multiarm_trial_flow")
        
        results <- flowchart(
            data = multiarm_trial_flow,
            nodes = c("phase1", "phase2", "phase3", "phase4", "phase5", "phase6", "phase7"),
            counts = c("phase1_n", "phase2_n", "phase3_n", "phase4_n", "phase5_n", "phase6_n", "phase7_n"),
            direction = "TB",
            showPercentages = TRUE,
            includeTitle = TRUE
        )
    })
    
    # Test 2: Diagnostic test validation
    testthat::expect_silent({
        data("diagnostic_validation_flow")
        
        results <- flowchart(
            data = diagnostic_validation_flow,
            nodes = rep("step_description", 6),
            counts = rep("case_numbers", 6),
            direction = "LR",
            nodeColor = "green",
            showExclusions = TRUE
        )
    })
    
    # Test 3: Laboratory workflow
    testthat::expect_silent({
        data("lab_workflow_flow")
        
        results <- flowchart(
            data = lab_workflow_flow,
            nodes = rep("process_step", 6),
            counts = rep("sample_count", 6),
            direction = "TB",
            nodeColor = "gray",
            fontSize = 11
        )
    })
    
    # Test 4: Research pipeline
    testthat::expect_silent({
        data("research_pipeline_flow")
        
        results <- flowchart(
            data = research_pipeline_flow,
            nodes = rep("pipeline_step", 9),
            counts = rep("milestone_count", 9),
            direction = "TB",
            nodeWidth = 3.5,
            showPercentages = TRUE,
            includeTitle = TRUE
        )
    })
    
    # Test 5: Patient journey
    testthat::expect_silent({
        data("patient_journey_flow")
        
        results <- flowchart(
            data = patient_journey_flow,
            nodes = rep("journey_point", 6),
            counts = rep("patient_numbers", 6),
            direction = "LR",
            nodeColor = "blue",
            showExclusions = TRUE,
            includeTitle = FALSE
        )
    })
})

test_that("flowchart comprehensive dataset", {
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    
    # Test comprehensive dataset with different flow types
    testthat::expect_silent({
        data("flowchart_comprehensive_data")
        
        # Test each flow type in the comprehensive dataset
        for(flow_type in unique(flowchart_comprehensive_data$flow_type)) {
            subset_data <- flowchart_comprehensive_data[flowchart_comprehensive_data$flow_type == flow_type, ]
            
            results <- flowchart(
                data = subset_data,
                nodes = c("step1", "step2", "step3", "step4", "step5"),
                counts = c("count1", "count2", "count3", "count4", "count5"),
                direction = "TB",
                showPercentages = TRUE,
                includeTitle = TRUE
            )
        }
    })
})

test_that("flowchart statistical coherence", {
    
    # Skip if required packages are not available
    skip_if_not_installed("DiagrammeR")
    
    # Test 1: Decreasing flow (normal pattern)
    testthat::expect_silent({
        decreasing_flow <- data.frame(
            stage = c("Initial", "Middle", "Final"),
            n = c(1000, 800, 600)  # Logical decreasing pattern
        )
        
        results <- flowchart(
            data = decreasing_flow,
            nodes = rep("stage", 3),
            counts = rep("n", 3),
            showPercentages = TRUE
        )
    })
    
    # Test 2: Perfect retention (no losses)
    testthat::expect_silent({
        perfect_retention <- data.frame(
            stage = c("Start", "Middle", "End"),
            n = c(500, 500, 500)  # No losses
        )
        
        results <- flowchart(
            data = perfect_retention,
            nodes = rep("stage", 3),
            counts = rep("n", 3),
            showPercentages = TRUE,
            showExclusions = TRUE
        )
    })
    
    # Test 3: High dropout scenario
    testthat::expect_silent({
        high_dropout <- data.frame(
            stage = c("Enrolled", "Started", "Completed"),
            n = c(1000, 600, 300)  # 70% overall dropout
        )
        
        results <- flowchart(
            data = high_dropout,
            nodes = rep("stage", 3),
            counts = rep("n", 3),
            showPercentages = TRUE,
            showExclusions = TRUE,
            includeTitle = TRUE
        )
    })
})
