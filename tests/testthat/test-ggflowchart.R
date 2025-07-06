test_that("ggflowchart works with basic data", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggflowchart")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("dplyr")
    
    # Test 1: Basic process flow
    testthat::expect_silent({
        # Load test data
        data("simple_process_flow")
        
        results <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            group_var = "process_type",
            node_fill_palette = "clinical_blue",
            plot_title = "Basic Process Flow",
            show_interpretation = TRUE
        )
    })
    
    # Test 2: Clinical decision flow without grouping
    testthat::expect_silent({
        # Load test data
        data("clinical_decision_flow")
        
        results <- ggflowchart(
            data = clinical_decision_flow,
            from_var = "from_step",
            to_var = "to_step",
            group_var = NULL,
            node_fill_palette = "modern_gray",
            plot_title = "Clinical Decision Tree",
            show_interpretation = FALSE
        )
    })
    
    # Test 3: Research pipeline with viridis palette
    testthat::expect_silent({
        # Load test data
        data("research_pipeline_flow")
        
        results <- ggflowchart(
            data = research_pipeline_flow,
            from_var = "pipeline_from",
            to_var = "pipeline_to",
            group_var = "phase",
            node_fill_palette = "viridis",
            plot_title = "Research Pipeline",
            show_interpretation = TRUE
        )
    })
})

test_that("ggflowchart handles edge cases", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggflowchart")
    
    # Test 1: Empty/default parameters (should show welcome message)
    testthat::expect_no_error({
        results <- ggflowchart(
            data = data.frame(),
            from_var = NULL,
            to_var = NULL
        )
    })
    
    # Test 2: Minimal flow (2 nodes only)
    testthat::expect_silent({
        # Load test data
        data("minimal_ggflow")
        
        results <- ggflowchart(
            data = minimal_ggflow,
            from_var = "start_node",
            to_var = "end_node",
            group_var = "simple_type",
            node_fill_palette = "pastel"
        )
    })
    
    # Test 3: Single edge case
    testthat::expect_silent({
        single_edge_data <- data.frame(
            from = "A",
            to = "B",
            category = "Test"
        )
        
        results <- ggflowchart(
            data = single_edge_data,
            from_var = "from",
            to_var = "to",
            group_var = "category",
            node_fill_palette = "set1"
        )
    })
    
    # Test 4: Multi-group workflow
    testthat::expect_silent({
        # Load test data
        data("multi_group_workflow")
        
        results <- ggflowchart(
            data = multi_group_workflow,
            from_var = "workflow_from",
            to_var = "workflow_to",
            group_var = "group_category",
            node_fill_palette = "clinical_blue",
            show_interpretation = TRUE
        )
    })
})

test_that("ggflowchart parameter variations", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggflowchart")
    
    # Load test data for all parameter tests
    data("simple_process_flow")
    
    # Test 1: Different color palettes
    testthat::expect_silent({
        # Clinical blue
        results_blue <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            group_var = "process_type",
            node_fill_palette = "clinical_blue"
        )
        
        # Modern gray
        results_gray <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            group_var = "process_type",
            node_fill_palette = "modern_gray"
        )
        
        # Viridis
        results_viridis <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            group_var = "process_type",
            node_fill_palette = "viridis"
        )
        
        # Set1
        results_set1 <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            group_var = "process_type",
            node_fill_palette = "set1"
        )
        
        # Pastel
        results_pastel <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            group_var = "process_type",
            node_fill_palette = "pastel"
        )
    })
    
    # Test 2: Different titles
    testthat::expect_silent({
        # Custom title
        results_custom <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            plot_title = "Custom Flowchart Title"
        )
        
        # Empty title
        results_empty <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            plot_title = ""
        )
    })
    
    # Test 3: Interpretation guide toggle
    testthat::expect_silent({
        # With interpretation
        results_with <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            show_interpretation = TRUE
        )
        
        # Without interpretation
        results_without <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            show_interpretation = FALSE
        )
    })
    
    # Test 4: With and without grouping
    testthat::expect_silent({
        # With grouping
        results_grouped <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            group_var = "process_type"
        )
        
        # Without grouping
        results_ungrouped <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node",
            group_var = NULL
        )
    })
})

test_that("ggflowchart data validation", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggflowchart")
    
    # Test 1: Missing required variables
    testthat::expect_no_error({
        data("simple_process_flow")
        
        # Missing from_var should show welcome message
        results <- ggflowchart(
            data = simple_process_flow,
            from_var = NULL,
            to_var = "to_node"
        )
        
        # Missing to_var should show welcome message
        results <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = NULL
        )
    })
    
    # Test 2: Empty data frame
    testthat::expect_error({
        empty_data <- data.frame()
        
        ggflowchart(
            data = empty_data,
            from_var = "from",
            to_var = "to"
        )
    })
    
    # Test 3: Non-existent variables
    testthat::expect_error({
        data("simple_process_flow")
        
        ggflowchart(
            data = simple_process_flow,
            from_var = "nonexistent_from",
            to_var = "nonexistent_to"
        )
    })
    
    # Test 4: Data with missing values
    testthat::expect_silent({
        missing_data <- data.frame(
            from_node = c("A", "B", NA, "D"),
            to_node = c("B", "C", "D", NA),
            category = c("Type1", "Type1", "Type2", "Type2")
        )
        
        # Should handle missing values by removing incomplete cases
        results <- ggflowchart(
            data = missing_data,
            from_var = "from_node",
            to_var = "to_node",
            group_var = "category"
        )
    })
    
    # Test 5: All missing data
    testthat::expect_error({
        all_missing_data <- data.frame(
            from_node = c(NA, NA, NA),
            to_node = c(NA, NA, NA)
        )
        
        ggflowchart(
            data = all_missing_data,
            from_var = "from_node",
            to_var = "to_node"
        )
    })
})

test_that("ggflowchart realistic scenarios", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggflowchart")
    
    # Test 1: Laboratory workflow
    testthat::expect_silent({
        data("lab_workflow_flow")
        
        results <- ggflowchart(
            data = lab_workflow_flow,
            from_var = "lab_from",
            to_var = "lab_to",
            group_var = "workflow_type",
            node_fill_palette = "modern_gray",
            plot_title = "Laboratory Processing Workflow"
        )
    })
    
    # Test 2: Data analysis pipeline
    testthat::expect_silent({
        data("data_analysis_pipeline")
        
        results <- ggflowchart(
            data = data_analysis_pipeline,
            from_var = "analysis_from",
            to_var = "analysis_to",
            group_var = "analysis_phase",
            node_fill_palette = "viridis",
            plot_title = "Data Analysis Pipeline"
        )
    })
    
    # Test 3: Patient care pathway
    testthat::expect_silent({
        data("patient_care_pathway")
        
        results <- ggflowchart(
            data = patient_care_pathway,
            from_var = "care_from",
            to_var = "care_to",
            group_var = "care_category",
            node_fill_palette = "clinical_blue",
            plot_title = "Patient Care Pathway"
        )
    })
    
    # Test 4: Software development flow
    testthat::expect_silent({
        data("software_dev_flow")
        
        results <- ggflowchart(
            data = software_dev_flow,
            from_var = "dev_from",
            to_var = "dev_to",
            group_var = "dev_stage",
            node_fill_palette = "set1",
            plot_title = "Software Development Lifecycle"
        )
    })
    
    # Test 5: Complex decision tree
    testthat::expect_silent({
        data("complex_decision_tree")
        
        results <- ggflowchart(
            data = complex_decision_tree,
            from_var = "decision_from",
            to_var = "decision_to",
            group_var = "decision_level",
            node_fill_palette = "pastel",
            plot_title = "Complex Decision Tree"
        )
    })
    
    # Test 6: Manufacturing process
    testthat::expect_silent({
        data("manufacturing_process")
        
        results <- ggflowchart(
            data = manufacturing_process,
            from_var = "process_from",
            to_var = "process_to",
            group_var = "manufacturing_type",
            node_fill_palette = "clinical_blue",
            plot_title = "Manufacturing Process Flow"
        )
    })
})

test_that("ggflowchart comprehensive dataset", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggflowchart")
    
    # Test comprehensive dataset with different workflow types
    testthat::expect_silent({
        data("ggflowchart_comprehensive_data")
        
        # Test each workflow type in the comprehensive dataset
        for(workflow_type in unique(ggflowchart_comprehensive_data$workflow_type)) {
            subset_data <- ggflowchart_comprehensive_data[
                ggflowchart_comprehensive_data$workflow_type == workflow_type, 
            ]
            
            results <- ggflowchart(
                data = subset_data,
                from_var = "from_var",
                to_var = "to_var",
                group_var = "group_var",
                node_fill_palette = "clinical_blue",
                plot_title = paste("Flowchart:", workflow_type),
                show_interpretation = TRUE
            )
        }
    })
})

test_that("ggflowchart package dependency handling", {
    
    # Test 1: Should handle missing ggflowchart package gracefully
    # Note: This test assumes ggflowchart package might not be available
    testthat::expect_no_error({
        # Create a scenario where package might not be available
        # The function should show appropriate error message
        data("simple_process_flow")
        
        # This should either work (if package available) or show error message
        results <- ggflowchart(
            data = simple_process_flow,
            from_var = "from_node",
            to_var = "to_node"
        )
    })
})

test_that("ggflowchart color palette functionality", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggflowchart")
    skip_if_not_installed("RColorBrewer")
    skip_if_not_installed("viridis")
    
    data("simple_process_flow")
    
    # Test each color palette
    palettes <- c("clinical_blue", "modern_gray", "viridis", "set1", "pastel")
    
    for(palette in palettes) {
        testthat::expect_silent({
            results <- ggflowchart(
                data = simple_process_flow,
                from_var = "from_node",
                to_var = "to_node",
                group_var = "process_type",
                node_fill_palette = palette,
                plot_title = paste("Test", palette, "palette")
            )
        })
    }
})

test_that("ggflowchart statistical coherence", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggflowchart")
    
    # Test 1: Linear flow (sequential process)
    testthat::expect_silent({
        linear_flow <- data.frame(
            from_step = c("Step 1", "Step 2", "Step 3", "Step 4"),
            to_step = c("Step 2", "Step 3", "Step 4", "Step 5"),
            process = c("Linear", "Linear", "Linear", "Linear")
        )
        
        results <- ggflowchart(
            data = linear_flow,
            from_var = "from_step",
            to_var = "to_step",
            group_var = "process"
        )
    })
    
    # Test 2: Branching flow (decision points)
    testthat::expect_silent({
        branching_flow <- data.frame(
            from_node = c("Start", "Start", "Decision", "Decision"),
            to_node = c("Decision", "Process A", "End A", "End B"),
            branch_type = c("Initial", "Process", "Outcome", "Outcome")
        )
        
        results <- ggflowchart(
            data = branching_flow,
            from_var = "from_node",
            to_var = "to_node",
            group_var = "branch_type"
        )
    })
    
    # Test 3: Circular flow (with cycles)
    testthat::expect_silent({
        circular_flow <- data.frame(
            from_state = c("A", "B", "C", "C"),
            to_state = c("B", "C", "A", "D"),
            flow_type = c("Forward", "Forward", "Cycle", "Exit")
        )
        
        results <- ggflowchart(
            data = circular_flow,
            from_var = "from_state",
            to_var = "to_state",
            group_var = "flow_type"
        )
    })
})