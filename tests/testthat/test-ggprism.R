test_that("ggprism works with basic data", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    
    # Skip if required packages are not available
    skip_if_not_installed("ggprism")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("dplyr")
    
    # Test 1: Basic clinical data with violin plot
    testthat::expect_silent({
        # Load test data
        data("clinical_prism_data")
        
        results <- ggprism(
            data = clinical_prism_data,
            x_var = "treatment_group",
            y_var = "biomarker_level",
            group_var = "patient_type",
            plot_type = "violin",
            prism_theme = "default",
            prism_palette = "floral",
            show_statistics = TRUE,
            show_summary = TRUE
        )
    })
    
    # Test 2: Dose response data with scatter plot
    testthat::expect_silent({
        # Load test data
        data("dose_response_prism_data")
        
        results <- ggprism(
            data = dose_response_prism_data,
            x_var = "dose_concentration",
            y_var = "response_percentage",
            group_var = "compound_type",
            plot_type = "scatter",
            prism_theme = "publication",
            prism_palette = "office",
            show_points = TRUE,
            point_size = 2.0
        )
    })
    
    # Test 3: Time course data with line plot
    testthat::expect_silent({
        # Load test data
        data("time_course_prism_data")
        
        results <- ggprism(
            data = time_course_prism_data,
            x_var = "time_point",
            y_var = "expression_level",
            group_var = "treatment_condition",
            plot_type = "line",
            prism_theme = "white",
            prism_palette = "viridis",
            show_statistics = FALSE
        )
    })
})

test_that("ggprism handles different plot types", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggprism")
    
    # Load test data for all plot type tests
    data("clinical_prism_data")
    
    # Test 1: Violin plot
    testthat::expect_silent({
        results_violin <- ggprism(
            data = clinical_prism_data,
            x_var = "treatment_group",
            y_var = "biomarker_level",
            group_var = "patient_type",
            plot_type = "violin",
            prism_palette = "candy_bright"
        )
    })
    
    # Test 2: Box plot
    testthat::expect_silent({
        results_box <- ggprism(
            data = clinical_prism_data,
            x_var = "treatment_group",
            y_var = "biomarker_level",
            group_var = "patient_type",
            plot_type = "boxplot",
            prism_palette = "pastels"
        )
    })
    
    # Test 3: Scatter plot
    testthat::expect_silent({
        results_scatter <- ggprism(
            data = clinical_prism_data,
            x_var = "treatment_group",
            y_var = "biomarker_level",
            group_var = "patient_type",
            plot_type = "scatter",
            prism_palette = "blueprint"
        )
    })
    
    # Test 4: Column plot
    testthat::expect_silent({
        results_column <- ggprism(
            data = clinical_prism_data,
            x_var = "treatment_group",
            y_var = "biomarker_level",
            group_var = "patient_type",
            plot_type = "column",
            prism_palette = "neon",
            error_bars = "se"
        )
    })
    
    # Test 5: Line plot
    testthat::expect_silent({
        results_line <- ggprism(
            data = clinical_prism_data,
            x_var = "treatment_group",
            y_var = "biomarker_level",
            group_var = "patient_type",
            plot_type = "line",
            prism_palette = "flames"
        )
    })
})

test_that("ggprism handles different themes and palettes", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggprism")
    
    data("biomarker_expression_data")
    
    # Test different themes
    themes <- c("default", "white", "minimal", "publication")
    
    for(theme_name in themes) {
        testthat::expect_silent({
            results <- ggprism(
                data = biomarker_expression_data,
                x_var = "gene_target",
                y_var = "fold_change",
                group_var = "experiment_type",
                plot_type = "violin",
                prism_theme = theme_name,
                prism_palette = "floral"
            )
        })
    }
    
    # Test different color palettes
    palettes <- c("floral", "candy_bright", "office", "pastels", "colorblind_safe",
                  "blueprint", "neon", "flames", "ocean", "spring", "viridis", "pearl")
    
    for(palette_name in palettes) {
        testthat::expect_silent({
            results <- ggprism(
                data = biomarker_expression_data,
                x_var = "gene_target",
                y_var = "fold_change",
                group_var = "experiment_type",
                plot_type = "boxplot",
                prism_theme = "default",
                prism_palette = palette_name
            )
        })
    }
})

test_that("ggprism statistical features work correctly", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggprism")
    
    # Test statistical comparisons
    data("statistical_comparison_data")
    
    # Test automatic statistical method selection
    testthat::expect_silent({
        results_auto <- ggprism(
            data = statistical_comparison_data,
            x_var = "disease_stage",
            y_var = "survival_months",
            group_var = "tumor_grade",
            plot_type = "violin",
            show_statistics = TRUE,
            stats_method = "auto",
            pvalue_format = "exact"
        )
    })
    
    # Test specific statistical methods
    stat_methods <- c("ttest", "wilcoxon", "anova", "kruskal")
    
    for(method in stat_methods) {
        testthat::expect_silent({
            results <- ggprism(
                data = statistical_comparison_data,
                x_var = "disease_stage",
                y_var = "survival_months",
                group_var = "tumor_grade",
                plot_type = "boxplot",
                show_statistics = TRUE,
                stats_method = method
            )
        })
    }
    
    # Test different p-value formats
    pvalue_formats <- c("exact", "scientific", "stars", "symbols")
    
    for(format in pvalue_formats) {
        testthat::expect_silent({
            results <- ggprism(
                data = statistical_comparison_data,
                x_var = "disease_stage",
                y_var = "survival_months",
                group_var = "tumor_grade",
                show_statistics = TRUE,
                pvalue_format = format
            )
        })
    }
})

test_that("ggprism handles edge cases and error conditions", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggprism")
    
    # Test 1: Empty/default parameters (should show welcome message)
    testthat::expect_no_error({
        results <- ggprism(
            data = data.frame(),
            x_var = NULL,
            y_var = NULL
        )
    })
    
    # Test 2: Minimal data
    testthat::expect_silent({
        data("minimal_prism_data")
        
        results <- ggprism(
            data = minimal_prism_data,
            x_var = "simple_x",
            y_var = "simple_y",
            group_var = "simple_group",
            plot_type = "scatter"
        )
    })
    
    # Test 3: Single group data
    testthat::expect_silent({
        single_group_data <- data.frame(
            category = rep("Single", 10),
            value = rnorm(10, mean = 20, sd = 3),
            stringsAsFactors = FALSE
        )
        
        results <- ggprism(
            data = single_group_data,
            x_var = "category",
            y_var = "value",
            group_var = NULL,
            plot_type = "violin",
            show_statistics = FALSE
        )
    })
    
    # Test 4: Data with missing values
    testthat::expect_silent({
        missing_data <- data.frame(
            x_vals = c("A", "B", "C", NA, "A"),
            y_vals = c(10, 15, NA, 20, 12),
            groups = c("G1", "G1", "G2", "G2", NA),
            stringsAsFactors = FALSE
        )
        
        # Should handle missing values by removing incomplete cases
        results <- ggprism(
            data = missing_data,
            x_var = "x_vals",
            y_var = "y_vals",
            group_var = "groups",
            plot_type = "boxplot"
        )
    })
    
    # Test 5: All missing data should error
    testthat::expect_error({
        all_missing_data <- data.frame(
            x_vals = c(NA, NA, NA),
            y_vals = c(NA, NA, NA)
        )
        
        ggprism(
            data = all_missing_data,
            x_var = "x_vals",
            y_var = "y_vals"
        )
    })
})

test_that("ggprism advanced features work correctly", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggprism")
    
    data("publication_prism_data")
    
    # Test 1: Publication ready mode
    testthat::expect_silent({
        results_pub <- ggprism(
            data = publication_prism_data,
            x_var = "experimental_group",
            y_var = "cell_viability_percent",
            group_var = "experiment_replicate",
            plot_type = "violin",
            publication_ready = TRUE,
            export_dpi = 300,
            prism_palette = "colorblind_safe"
        )
    })
    
    # Test 2: Preview mode
    testthat::expect_silent({
        results_preview <- ggprism(
            data = publication_prism_data,
            x_var = "experimental_group",
            y_var = "cell_viability_percent",
            plot_type = "boxplot",
            preview_mode = TRUE,
            prism_palette = "ocean"
        )
    })
    
    # Test 3: Faceting
    testthat::expect_silent({
        results_facet <- ggprism(
            data = publication_prism_data,
            x_var = "experimental_group",
            y_var = "cell_viability_percent",
            group_var = "experiment_replicate",
            facet_var = "culture_conditions",
            plot_type = "violin",
            prism_palette = "spring"
        )
    })
    
    # Test 4: Different error bar types
    error_types <- c("none", "se", "sd", "ci")
    
    for(error_type in error_types) {
        testthat::expect_silent({
            results <- ggprism(
                data = publication_prism_data,
                x_var = "experimental_group",
                y_var = "cell_viability_percent",
                plot_type = "column",
                error_bars = error_type
            )
        })
    }
    
    # Test 5: Advanced guides
    guide_types <- c("standard", "minor", "offset", "offset_minor", "bracket")
    
    for(guide_type in guide_types) {
        testthat::expect_silent({
            results <- ggprism(
                data = publication_prism_data,
                x_var = "experimental_group",
                y_var = "cell_viability_percent",
                plot_type = "boxplot",
                prism_guides = guide_type
            )
        })
    }
})

test_that("ggprism accessibility and shape palettes", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggprism")
    
    # Test colorblind safe data
    data("colorblind_safe_data")
    
    # Test colorblind safe palette
    testthat::expect_silent({
        results_cb <- ggprism(
            data = colorblind_safe_data,
            x_var = "category",
            y_var = "measurement_value",
            group_var = "accessibility_group",
            plot_type = "scatter",
            prism_palette = "colorblind_safe",
            show_points = TRUE
        )
    })
    
    # Test different shape palettes for scatter plots
    shape_palettes <- c("default", "prism", "filled", "open")
    
    for(shape_palette in shape_palettes) {
        testthat::expect_silent({
            results <- ggprism(
                data = colorblind_safe_data,
                x_var = "category",
                y_var = "measurement_value",
                group_var = "accessibility_group",
                plot_type = "scatter",
                prism_shape_palette = shape_palette,
                point_size = 2.5,
                point_alpha = 0.8
            )
        })
    }
})

test_that("ggprism parameter customization", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggprism")
    
    data("clinical_prism_data")
    
    # Test custom labels and titles
    testthat::expect_silent({
        results_custom <- ggprism(
            data = clinical_prism_data,
            x_var = "treatment_group",
            y_var = "biomarker_level",
            group_var = "patient_type",
            plot_type = "violin",
            plot_title = "Custom Biomarker Analysis",
            x_label = "Treatment Groups",
            y_label = "Biomarker Concentration (ng/mL)",
            legend_position = "bottom",
            base_size = 14
        )
    })
    
    # Test different legend positions
    legend_positions <- c("right", "left", "top", "bottom", "none")
    
    for(position in legend_positions) {
        testthat::expect_silent({
            results <- ggprism(
                data = clinical_prism_data,
                x_var = "treatment_group",
                y_var = "biomarker_level",
                group_var = "patient_type",
                plot_type = "boxplot",
                legend_position = position
            )
        })
    }
    
    # Test point customization
    testthat::expect_silent({
        results_points <- ggprism(
            data = clinical_prism_data,
            x_var = "treatment_group",
            y_var = "biomarker_level",
            plot_type = "violin",
            show_points = TRUE,
            point_size = 3.0,
            point_alpha = 0.4,
            jitter_width = 0.3
        )
    })
    
    # Test violin width customization
    testthat::expect_silent({
        results_violin <- ggprism(
            data = clinical_prism_data,
            x_var = "treatment_group",
            y_var = "biomarker_level",
            plot_type = "violin",
            violin_width = 1.5
        )
    })
})

test_that("ggprism performance with large datasets", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggprism")
    
    # Test with large dataset
    data("large_prism_dataset")
    
    testthat::expect_silent({
        results_large <- ggprism(
            data = large_prism_dataset,
            x_var = "treatment",
            y_var = "outcome_score",
            group_var = "patient_subgroup",
            facet_var = "study_center",
            plot_type = "violin",
            show_statistics = FALSE,  # Disable for performance
            show_summary = TRUE,
            prism_palette = "viridis"
        )
    })
})

test_that("ggprism comprehensive dataset testing", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggprism")
    
    # Test comprehensive dataset with different workflow types
    data("comprehensive_prism_data")
    
    # Test each dataset type in the comprehensive dataset
    for(dataset_type in unique(comprehensive_prism_data$dataset_type)) {
        testthat::expect_silent({
            subset_data <- comprehensive_prism_data[
                comprehensive_prism_data$dataset_type == dataset_type, 
            ]
            
            results <- ggprism(
                data = subset_data,
                x_var = "x_variable",
                y_var = "y_variable",
                group_var = "group_variable",
                facet_var = "facet_variable",
                plot_type = "boxplot",
                prism_palette = "floral",
                plot_title = paste("ggprism Test:", dataset_type),
                show_summary = TRUE,
                show_statistics = TRUE
            )
        })
    }
})

test_that("ggprism package dependency handling", {
    
    # Test 1: Should handle missing ggprism package gracefully
    # Note: This test assumes ggprism package might not be available
    testthat::expect_no_error({
        # Create a scenario where package might not be available
        # The function should show appropriate error message
        data("clinical_prism_data")
        
        # This should either work (if package available) or show error message
        results <- ggprism(
            data = clinical_prism_data,
            x_var = "treatment_group",
            y_var = "biomarker_level"
        )
    })
})

test_that("ggprism real-world scenarios", {
    
    # Skip if required packages are not available
    skip_if_not_installed("ggprism")
    
    # Test 1: Pharmacokinetics analysis
    testthat::expect_silent({
        data("pharmacokinetics_data")
        
        results_pk <- ggprism(
            data = pharmacokinetics_data,
            x_var = "time_hours",
            y_var = "concentration_ng_ml",
            group_var = "formulation",
            plot_type = "line",
            prism_theme = "publication",
            prism_palette = "office",
            plot_title = "Pharmacokinetic Profile",
            x_label = "Time (hours)",
            y_label = "Concentration (ng/mL)"
        )
    })
    
    # Test 2: Biomarker expression analysis
    testthat::expect_silent({
        data("biomarker_expression_data")
        
        results_expr <- ggprism(
            data = biomarker_expression_data,
            x_var = "gene_target",
            y_var = "fold_change",
            group_var = "experiment_type",
            facet_var = "sample_condition",
            plot_type = "violin",
            prism_theme = "white",
            prism_palette = "viridis",
            show_statistics = TRUE,
            stats_method = "auto"
        )
    })
    
    # Test 3: Dose-response analysis
    testthat::expect_silent({
        data("dose_response_prism_data")
        
        results_dr <- ggprism(
            data = dose_response_prism_data,
            x_var = "dose_concentration",
            y_var = "response_percentage",
            group_var = "compound_type",
            plot_type = "scatter",
            prism_theme = "minimal",
            prism_palette = "flames",
            show_points = TRUE,
            point_size = 2.0
        )
    })
})
