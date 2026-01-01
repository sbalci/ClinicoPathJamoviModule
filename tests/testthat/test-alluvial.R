test_that("alluvial class exists and can be instantiated", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    
    # Test that alluvial class exists
    expect_true(exists("alluvialClass"))
    
    # Test that we can create an instance
    if (exists("alluvialClass")) {
        expect_true(inherits(alluvialClass, "R6ClassGenerator"))
    }
})

test_that("alluvial function basic structure", {
    
    # Create sample data for testing
    test_data <- data.frame(
        Sex = sample(c("Male", "Female"), 100, replace = TRUE),
        Grade = sample(c("Low", "Moderate", "High"), 100, replace = TRUE),
        Status = sample(c("Positive", "Negative"), 100, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    # Test that alluvial function is available
    expect_true(exists("alluvial"))
    
    # Test basic function call
    if (exists("alluvial")) {
        result <- alluvial(
            data = test_data,
            vars = c("Sex", "Grade", "Status")
        )
        
        # Check that result is an alluvial analysis object
        expect_s3_class(result, "alluvialClass")
        expect_true(inherits(result, "alluvialBase"))
    }
})

test_that("alluvial parameter validation", {
    
    # Create sample data
    test_data <- data.frame(
        Var1 = sample(c("A", "B"), 50, replace = TRUE),
        Var2 = sample(c("X", "Y", "Z"), 50, replace = TRUE),
        Var3 = sample(c("P", "Q"), 50, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvial")) {
        
        # Test with different fill options
        fill_options <- c("first_variable", "last_variable", "all_flows", "values")
        
        for (fill_opt in fill_options) {
            result <- alluvial(
                data = test_data,
                vars = c("Var1", "Var2", "Var3"),
                fill = fill_opt
            )
            expect_s3_class(result, "alluvialClass")
        }
        
        # Test with different bin options
        bin_options <- c("default", "mean", "median", "min_max", "cuts")
        
        for (bin_opt in bin_options) {
            result <- alluvial(
                data = test_data,
                vars = c("Var1", "Var2", "Var3"),
                bin = bin_opt
            )
            expect_s3_class(result, "alluvialClass")
        }
        
        # Test with different orientation options
        orient_options <- c("vert", "horr")
        
        for (orient_opt in orient_options) {
            result <- alluvial(
                data = test_data,
                vars = c("Var1", "Var2", "Var3"),
                orient = orient_opt
            )
            expect_s3_class(result, "alluvialClass")
        }
    }
})

test_that("alluvial marginal plots parameter", {
    
    # Create sample data
    test_data <- data.frame(
        A = sample(c("High", "Low"), 30, replace = TRUE),
        B = sample(c("Yes", "No"), 30, replace = TRUE),
        C = sample(c("Red", "Blue"), 30, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvial")) {
        # Test with marginal plots enabled (requires vertical orientation)
        result <- alluvial(
            data = test_data,
            vars = c("A", "B", "C"),
            marg = TRUE,
            orient = "vert"
        )
        
        expect_s3_class(result, "alluvialClass")
        expect_true(result$options$marg)
        expect_equal(result$options$orient, "vert")
    }
})

test_that("alluvial custom title parameter", {
    
    # Create sample data
    test_data <- data.frame(
        X = sample(c("Type1", "Type2"), 25, replace = TRUE),
        Y = sample(c("Class A", "Class B"), 25, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvial")) {
        # Test with custom title (cannot be used with marginal plots)
        custom_title <- "Test Flow Analysis"
        result <- alluvial(
            data = test_data,
            vars = c("X", "Y"),
            usetitle = TRUE,
            mytitle = custom_title,
            marg = FALSE
        )
        
        expect_s3_class(result, "alluvialClass")
        expect_true(result$options$usetitle)
        expect_equal(result$options$mytitle, custom_title)
    }
})

test_that("alluvial condensation variable parameter", {
    
    # Create sample data
    test_data <- data.frame(
        Grade = sample(c("I", "II", "III"), 40, replace = TRUE),
        Stage = sample(c("Early", "Late"), 40, replace = TRUE),
        Response = sample(c("Good", "Poor"), 40, replace = TRUE),
        Gender = sample(c("M", "F"), 40, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvial")) {
        # Test with condensation variable
        result <- alluvial(
            data = test_data,
            vars = c("Grade", "Stage", "Response"),
            condensationvar = "Gender"
        )
        
        expect_s3_class(result, "alluvialClass")
        expect_equal(result$options$condensationvar, "Gender")
        
        # Check that second plot method exists
        expect_true(exists(".plot2", envir = result$.__enclos_env__$private))
    }
})

test_that("alluvial missing value exclusion parameter", {
    
    # Create sample data with some missing values
    test_data <- data.frame(
        Status1 = sample(c("Active", "Inactive", NA), 35, replace = TRUE),
        Status2 = sample(c("True", "False"), 35, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvial")) {
        # Test with missing value exclusion
        result <- alluvial(
            data = test_data,
            vars = c("Status1", "Status2"),
            excl = TRUE
        )
        
        expect_s3_class(result, "alluvialClass")
        expect_true(result$options$excl)
    }
})

test_that("alluvial comprehensive parameter combination", {
    
    # Create comprehensive test data
    test_data <- data.frame(
        Feature1 = sample(c("Alpha", "Beta", "Gamma"), 50, replace = TRUE),
        Feature2 = sample(c("North", "South"), 50, replace = TRUE),
        Feature3 = sample(c("Up", "Down", "Stable"), 50, replace = TRUE),
        Feature4 = sample(c("Present", "Absent"), 50, replace = TRUE),
        Condition = sample(c("Met", "Unmet"), 50, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvial")) {
        # Test comprehensive parameter combination
        result <- alluvial(
            data = test_data,
            vars = c("Feature1", "Feature2", "Feature3", "Feature4"),
            condensationvar = "Condition",
            excl = TRUE,
            marg = FALSE,
            fill = "all_flows",
            bin = "median",
            orient = "horr",
            usetitle = TRUE,
            mytitle = "Comprehensive Test Analysis"
        )
        
        expect_s3_class(result, "alluvialClass")
        expect_equal(result$options$condensationvar, "Condition")
        expect_true(result$options$excl)
        expect_false(result$options$marg)
        expect_equal(result$options$fill, "all_flows")
        expect_equal(result$options$bin, "median")
        expect_equal(result$options$orient, "horr")
        expect_true(result$options$usetitle)
        expect_equal(result$options$mytitle, "Comprehensive Test Analysis")
    }
})

test_that("alluvial required methods exist", {
    
    # Simple test data
    test_data <- data.frame(
        A = c("X", "Y", "X", "Y"),
        B = c("1", "2", "1", "2"),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvial")) {
        result <- alluvial(
            data = test_data,
            vars = c("A", "B")
        )
        
        # Check that required methods exist
        expect_true(exists(".plot", envir = result$.__enclos_env__$private))
        expect_true(exists(".run", envir = result$.__enclos_env__$private))
        
        # Check for plot2 method when condensation variable is used
        result_with_condensation <- alluvial(
            data = test_data,
            vars = c("A"),
            condensationvar = "B"
        )
        expect_true(exists(".plot2", envir = result_with_condensation$.__enclos_env__$private))
    }
})
