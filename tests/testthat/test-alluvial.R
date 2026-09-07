# The generated Options/Results classes and the backend R6 class are internal to the
# ClinicoPath namespace: they are visible under devtools::load_all(export_all = TRUE)
# but NOT on the search path of an installed build, which is what jamovi ships. Bind
# them explicitly so this file tests the artifact that actually ships.
alluvialClass <- getFromNamespace("alluvialClass", "ClinicoPath")
alluvialResults <- getFromNamespace("alluvialResults", "ClinicoPath")

test_that("alluvial class exists and can be instantiated", {
    # Test that alluvial class exists
    expect_true(exists("alluvialClass", envir = asNamespace("ClinicoPath")))
    
    # Test that we can create an instance
    if (exists("alluvialClass", envir = asNamespace("ClinicoPath"))) {
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
        
        # Exported jamovi functions return the result tree, not the analysis R6.
        expect_s3_class(result, "alluvialResults")
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
            expect_s3_class(result, "alluvialResults")
        }
        
        # Test with different orientation options
        orient_options <- c("vert", "horr")
        
        for (orient_opt in orient_options) {
            result <- alluvial(
                data = test_data,
                vars = c("Var1", "Var2", "Var3"),
                orient = orient_opt
            )
            expect_s3_class(result, "alluvialResults")
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
        
        expect_s3_class(result, "alluvialResults")
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
        
        expect_s3_class(result, "alluvialResults")
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
        
        expect_s3_class(result, "alluvialResults")
        expect_equal(result$options$condensationvar, "Gender")
        
        # Check that second plot method exists
        expect_true(is.function(alluvialClass$private_methods$.plot2))
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
        
        expect_s3_class(result, "alluvialResults")
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
            orient = "horr",
            usetitle = TRUE,
            mytitle = "Comprehensive Test Analysis"
        )
        
        expect_s3_class(result, "alluvialResults")
        expect_equal(result$options$condensationvar, "Condition")
        expect_true(result$options$excl)
        expect_false(result$options$marg)
        expect_equal(result$options$fill, "all_flows")
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
        expect_true(is.function(alluvialClass$private_methods$.plot))
        expect_true(is.function(alluvialClass$private_methods$.run))
        
        # Check for plot2 method when condensation variable is used
        result_with_condensation <- alluvial(
            data = test_data,
            vars = c("A"),
            condensationvar = "B"
        )
        expect_true(is.function(alluvialClass$private_methods$.plot2))
    }
})

test_that("alluvial caches prepared data and honors missing-value exclusion", {
    data <- data.frame(
        axis_a = factor(c("x", "y", NA, "x")),
        axis_b = factor(c("m", NA, "n", "m")),
        flow_group = factor(c("u", "v", "u", "v"))
    )

    keep_missing <- alluvial(
        data = data,
        vars = c("axis_a", "axis_b"),
        engine = "ggalluvial",
        fillGgalluvial = "flow_group",
        excl = FALSE
    )
    exclude_missing <- alluvial(
        data = data,
        vars = c("axis_a", "axis_b"),
        engine = "ggalluvial",
        fillGgalluvial = "flow_group",
        excl = TRUE
    )

    keep_state <- keep_missing$plot$state
    exclude_state <- exclude_missing$plot$state

    expect_equal(nrow(keep_state$data), 4)
    expect_equal(nrow(exclude_state$data), 2)
    expect_equal(keep_state$fill_var, "flow_group")
    expect_true("flow_group" %in% names(keep_state$data))
    expect_true("(Missing)" %in% levels(keep_state$data$axis_a))
    expect_true("(Missing)" %in% levels(keep_state$data$axis_b))

    render_body <- paste(
        deparse(body(alluvialClass$private_methods$.plot)),
        collapse = "\n"
    )
    expect_false(grepl("self\\$data", render_body))
})

test_that("weighted alluvial aggregation preserves a separate fill variable", {
    data <- data.frame(
        axis_a = factor(c("x", "x", "y", "y")),
        axis_b = factor(c("m", "m", "n", "n")),
        flow_group = factor(c("u", "v", "u", "v")),
        count = c(1, 2, 3, 4)
    )

    result <- alluvial(
        data = data,
        vars = c("axis_a", "axis_b"),
        engine = "ggalluvial",
        fillGgalluvial = "flow_group",
        weight = "count"
    )

    state <- result$plot$state
    expect_equal(nrow(state$data), 4)
    expect_equal(sum(state$data$count), sum(data$count))
    expect_equal(state$fill_var, "flow_group")
})

test_that("weighted alluvial rejects unusable or reused weights", {
    all_missing <- data.frame(
        axis_a = factor(c("x", "y", "x")),
        axis_b = factor(c("m", "n", "m")),
        count = c(NA_real_, NA_real_, NA_real_)
    )
    missing_result <- alluvial(
        data = all_missing,
        vars = c("axis_a", "axis_b"),
        engine = "ggalluvial",
        weight = "count"
    )

    expect_null(missing_result$plot$state)
    expect_match(missing_result$notices$content, "No Valid Weights")

    reused <- data.frame(
        axis_a = c(1, 2, 1),
        axis_b = factor(c("m", "n", "m"))
    )
    reused_result <- alluvial(
        data = reused,
        vars = c("axis_a", "axis_b"),
        engine = "ggalluvial",
        weight = "axis_a"
    )

    expect_null(reused_result$plot$state)
    expect_match(reused_result$notices$content, "Weight Variable Reused")
})

test_that("reversed flow direction renders", {
    data <- data.frame(
        value = 1:10,
        group = factor(rep(c("x", "y"), 5))
    )

    reversed <- alluvial(
        data = data,
        vars = c("value", "group"),
        flowDirection = "right_left"
    )

    expect_error(suppressWarnings(reversed$plot$.render()), NA)
})
