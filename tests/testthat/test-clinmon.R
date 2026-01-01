test_that("clinmon works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    
    # Skip if clintools is not available
    skip_if_not_installed("clintools")
    
    # Create simple test data
    set.seed(42)
    n_points <- 300  # 5 minutes at 1 Hz
    time_seq <- seq(0, 299, by = 1)
    
    # Generate simple physiological signals
    test_data <- data.frame(
        time = time_seq,
        abp = 100 + 20 * sin(2 * pi * 1.2 * time_seq / 60) + rnorm(n_points, 0, 2),
        icp = 10 + 2 * sin(2 * pi * 0.25 * time_seq / 60) + rnorm(n_points, 0, 1),
        mcav = 60 + 10 * sin(2 * pi * 1.2 * time_seq / 60) + rnorm(n_points, 0, 2),
        hr = 75 + 5 * sin(2 * pi * 0.1 * time_seq / 60) + rnorm(n_points, 0, 1)
    )
    
    # Ensure realistic ranges
    test_data$abp <- pmax(60, pmin(180, test_data$abp))
    test_data$icp <- pmax(0, pmin(40, test_data$icp))
    test_data$mcav <- pmax(20, pmin(100, test_data$mcav))
    test_data$hr <- pmax(50, pmin(120, test_data$hr))
    
    # Calculate CPP
    test_data$cpp <- test_data$abp - test_data$icp
    
    # Test 1: Basic functionality with ABP and ICP
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            icp = "icp",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch",
            show_summary = TRUE,
            show_detailed = FALSE
        )
    })
    
    # Test 2: With MCAv for flow indices
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            mcav = "mcav",
            freq = 1,
            blocksize = 5,
            epochsize = 10,
            output_level = "epoch",
            show_summary = TRUE,
            show_detailed = TRUE
        )
    })
    
    # Test 3: Using CPP instead of ICP
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            cpp = "cpp",
            mcav = "mcav",
            freq = 1,
            blocksize = 3,
            epochsize = 20,
            output_level = "epoch"
        )
    })
    
    # Test 4: With heart rate for cardiac output estimation
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            hr = "hr",
            freq = 1,
            blocksize = 5,
            epochsize = 10,
            output_level = "epoch"
        )
    })
    
    # Test 5: All variables together
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            icp = "icp",
            mcav = "mcav",
            hr = "hr",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch",
            show_summary = TRUE,
            show_detailed = TRUE
        )
    })
    
    # Test 6: Different output levels
    output_levels <- c("period", "epoch", "block")
    
    for (level in output_levels) {
        testthat::expect_silent({
            results <- ClinicoPath::clinmon(
                data = test_data,
                time_var = "time",
                abp = "abp",
                icp = "icp",
                freq = 1,
                blocksize = 5,
                epochsize = 5,
                output_level = level,
                show_summary = FALSE,
                show_detailed = FALSE
            )
        })
    }
    
    # Test 7: Fast processing option
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            mcav = "mcav",
            freq = 1,
            fast_processing = TRUE,
            blocksize = 5,
            epochsize = 10,
            output_level = "epoch"
        )
    })
    
    # Test 8: Overlapping blocks
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            mcav = "mcav",
            freq = 1,
            overlapping = TRUE,
            blocksize = 5,
            epochsize = 8,
            output_level = "epoch"
        )
    })
    
    # Test 9: Different minimum thresholds
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            icp = "icp",
            freq = 1,
            blocksize = 5,
            epochsize = 10,
            blockmin = 0.8,
            epochmin = 0.7,
            output_level = "epoch"
        )
    })
    
    # Test 10: High frequency simulation
    hf_data <- data.frame(
        time = seq(0, 59.99, by = 0.01),  # 1 minute at 100 Hz
        abp = 100 + 25 * sin(2 * pi * 1.2 * seq(0, 59.99, by = 0.01) / 60),
        mcav = 60 + 10 * sin(2 * pi * 1.2 * seq(0, 59.99, by = 0.01) / 60)
    )
    
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = hf_data,
            time_var = "time",
            abp = "abp",
            mcav = "mcav",
            freq = 100,
            blocksize = 3,
            epochsize = 5,
            output_level = "epoch"
        )
    })
})

test_that("clinmon handles edge cases", {
    
    # Skip if clintools is not available
    skip_if_not_installed("clintools")
    
    # Create minimal test data
    minimal_data <- data.frame(
        time = 0:19,  # 20 seconds
        abp = rep(100, 20),
        icp = rep(10, 20)
    )
    
    # Test 1: Empty variable selection should show instructions
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = minimal_data
        )
    })
    
    # Test 2: Only time variable selected
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = minimal_data,
            time_var = "time"
        )
    })
    
    # Test 3: Minimal data with small blocks
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = minimal_data,
            time_var = "time",
            abp = "abp",
            icp = "icp",
            freq = 1,
            blocksize = 2,
            epochsize = 3,
            output_level = "block"
        )
    })
    
    # Test 4: Data with missing values
    missing_data <- data.frame(
        time = 0:29,
        abp = c(rep(100, 15), rep(NA, 5), rep(110, 10)),
        mcav = c(rep(60, 10), rep(NA, 10), rep(65, 10))
    )
    
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = missing_data,
            time_var = "time",
            abp = "abp",
            mcav = "mcav",
            freq = 1,
            blocksize = 5,
            epochsize = 3,
            output_level = "epoch"
        )
    })
    
    # Test 5: Very short time series
    short_data <- data.frame(
        time = 0:9,  # 10 seconds only
        abp = 100 + rnorm(10, 0, 5),
        mcav = 60 + rnorm(10, 0, 3)
    )
    
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = short_data,
            time_var = "time",
            abp = "abp",
            mcav = "mcav",
            freq = 1,
            blocksize = 2,
            epochsize = 2,
            output_level = "block"
        )
    })
})

test_that("clinmon parameter validation", {
    
    # Skip if clintools is not available
    skip_if_not_installed("clintools")
    
    # Create standard test data
    test_data <- data.frame(
        time = 0:99,  # 100 seconds
        abp = 100 + 20 * sin(2 * pi * (0:99) / 60),
        icp = 10 + 2 * sin(2 * pi * (0:99) / 60),
        mcav = 60 + 10 * sin(2 * pi * (0:99) / 60)
    )
    
    # Test different frequencies
    frequencies <- c(1, 10, 100, 1000)
    
    for (freq in frequencies) {
        testthat::expect_silent({
            results <- ClinicoPath::clinmon(
                data = test_data,
                time_var = "time",
                abp = "abp",
                freq = freq,
                blocksize = 3,
                epochsize = 5,
                output_level = "epoch"
            )
        })
    }
    
    # Test different block sizes
    block_sizes <- c(1, 3, 5, 10, 30)
    
    for (size in block_sizes) {
        testthat::expect_silent({
            results <- ClinicoPath::clinmon(
                data = test_data,
                time_var = "time",
                abp = "abp",
                freq = 1,
                blocksize = size,
                epochsize = 5,
                output_level = "epoch"
            )
        })
    }
    
    # Test different epoch sizes
    epoch_sizes <- c(2, 5, 10, 20, 50)
    
    for (size in epoch_sizes) {
        testthat::expect_silent({
            results <- ClinicoPath::clinmon(
                data = test_data,
                time_var = "time",
                abp = "abp",
                freq = 1,
                blocksize = 3,
                epochsize = size,
                output_level = "epoch"
            )
        })
    }
    
    # Test different minimum thresholds
    min_thresholds <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    
    for (thresh in min_thresholds) {
        testthat::expect_silent({
            results <- ClinicoPath::clinmon(
                data = test_data,
                time_var = "time",
                abp = "abp",
                freq = 1,
                blocksize = 5,
                epochsize = 5,
                blockmin = thresh,
                epochmin = thresh,
                output_level = "epoch"
            )
        })
    }
})

test_that("clinmon output options", {
    
    # Skip if clintools is not available
    skip_if_not_installed("clintools")
    
    # Create test data
    test_data <- data.frame(
        time = 0:199,  # 200 seconds
        abp = 100 + 20 * sin(2 * pi * (0:199) / 60) + rnorm(200, 0, 2),
        icp = 10 + 2 * sin(2 * pi * (0:199) / 60) + rnorm(200, 0, 1),
        mcav = 60 + 10 * sin(2 * pi * (0:199) / 60) + rnorm(200, 0, 2)
    )
    
    # Test summary only
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            icp = "icp",
            mcav = "mcav",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch",
            show_summary = TRUE,
            show_detailed = FALSE
        )
    })
    
    # Test detailed only
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            icp = "icp",
            mcav = "mcav",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch",
            show_summary = FALSE,
            show_detailed = TRUE
        )
    })
    
    # Test both outputs
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            icp = "icp",
            mcav = "mcav",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch",
            show_summary = TRUE,
            show_detailed = TRUE
        )
    })
    
    # Test neither output (should just run analysis)
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            icp = "icp",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch",
            show_summary = FALSE,
            show_detailed = FALSE
        )
    })
})

test_that("clinmon physiological variable combinations", {
    
    # Skip if clintools is not available
    skip_if_not_installed("clintools")
    
    # Create comprehensive test data
    test_data <- data.frame(
        time = 0:299,  # 5 minutes
        abp = 100 + 20 * sin(2 * pi * (0:299) / 60),
        icp = 10 + 2 * sin(2 * pi * (0:299) / 60),
        mcav = 60 + 10 * sin(2 * pi * (0:299) / 60),
        hr = 75 + 5 * sin(2 * pi * (0:299) / 60)
    )
    test_data$cpp <- test_data$abp - test_data$icp
    
    # Test ABP only
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch"
        )
    })
    
    # Test MCAv only
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            mcav = "mcav",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch"
        )
    })
    
    # Test ICP only
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            icp = "icp",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch"
        )
    })
    
    # Test ABP + ICP (for PRx)
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            icp = "icp",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch"
        )
    })
    
    # Test ABP + MCAv (for flow indices)
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            mcav = "mcav",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch"
        )
    })
    
    # Test CPP + MCAv (alternative to ABP + MCAv)
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            cpp = "cpp",
            mcav = "mcav",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch"
        )
    })
    
    # Test ABP + HR (for cardiac output)
    testthat::expect_silent({
        results <- ClinicoPath::clinmon(
            data = test_data,
            time_var = "time",
            abp = "abp",
            hr = "hr",
            freq = 1,
            blocksize = 10,
            epochsize = 5,
            output_level = "epoch"
        )
    })
})
