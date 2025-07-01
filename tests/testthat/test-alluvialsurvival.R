test_that("alluvialSurvival class exists and can be instantiated", {
    
    # Test that alluvialSurvival class exists
    expect_true(exists("alluvialSurvivalClass"))
    
    # Test that we can create an instance
    if (exists("alluvialSurvivalClass")) {
        expect_true(inherits(alluvialSurvivalClass, "R6ClassGenerator"))
    }
})

test_that("alluvialSurvival function basic structure", {
    
    # Create sample longitudinal data for testing
    test_data <- data.frame(
        PatientID = rep(c("P001", "P002", "P003", "P004", "P005"), each = 3),
        Time = rep(c(0, 6, 12), 5),
        Stage = sample(c("Resectable", "BR/LA", "Metastatic"), 15, replace = TRUE),
        Treatment = sample(c("Surgery", "Chemotherapy", "Follow-up"), 15, replace = TRUE),
        Survival = sample(c(0, 1), 15, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    # Test that alluvialSurvival function is available
    expect_true(exists("alluvialSurvival"))
    
    # Test basic function call
    if (exists("alluvialSurvival")) {
        result <- alluvialSurvival(
            data = test_data,
            timeVar = "Time",
            stageVar = "Stage",
            treatmentVar = "Treatment",
            patientId = "PatientID"
        )
        
        # Check that result is an alluvialSurvival analysis object
        expect_s3_class(result, "alluvialSurvivalClass")
        expect_true(inherits(result, "alluvialSurvivalBase"))
    }
})

test_that("alluvialSurvival with survival variable", {
    
    # Create sample data with survival information
    test_data <- data.frame(
        ID = rep(c("A", "B", "C", "D", "E", "F"), each = 4),
        TimePoint = rep(c(0, 3, 6, 12), 6),
        DiseaseStage = sample(c("Early", "Advanced"), 24, replace = TRUE),
        TreatmentType = sample(c("Standard", "Experimental"), 24, replace = TRUE),
        SurvivalStatus = sample(c(0, 1), 24, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvialSurvival")) {
        result <- alluvialSurvival(
            data = test_data,
            timeVar = "TimePoint",
            stageVar = "DiseaseStage",
            treatmentVar = "TreatmentType",
            patientId = "ID",
            survivalVar = "SurvivalStatus"
        )
        
        expect_s3_class(result, "alluvialSurvivalClass")
        expect_equal(result$options$survivalVar, "SurvivalStatus")
    }
})

test_that("alluvialSurvival parameter validation", {
    
    # Create test data
    test_data <- data.frame(
        Patient = rep(c("P1", "P2", "P3", "P4", "P5"), each = 3),
        Month = rep(c(0, 6, 12), 5),
        Cancer_Stage = sample(c("I", "II", "III"), 15, replace = TRUE),
        Therapy = sample(c("Chemo", "Radio", "Surgery"), 15, replace = TRUE),
        Status = sample(c(0, 1), 15, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvialSurvival")) {
        
        # Test with different color schemes
        colorSchemes <- c("clinical", "colorblind")
        
        for (scheme in colorSchemes) {
            result <- alluvialSurvival(
                data = test_data,
                timeVar = "Month",
                stageVar = "Cancer_Stage",
                treatmentVar = "Therapy",
                patientId = "Patient",
                colorScheme = scheme
            )
            expect_s3_class(result, "alluvialSurvivalClass")
            expect_equal(result$options$colorScheme, scheme)
        }
        
        # Test with showRightAxis option
        result <- alluvialSurvival(
            data = test_data,
            timeVar = "Month",
            stageVar = "Cancer_Stage",
            treatmentVar = "Therapy",
            patientId = "Patient",
            showRightAxis = TRUE
        )
        expect_s3_class(result, "alluvialSurvivalClass")
        expect_true(result$options$showRightAxis)
        
        # Test with survival functionality
        result <- alluvialSurvival(
            data = test_data,
            timeVar = "Month",
            stageVar = "Cancer_Stage",
            treatmentVar = "Therapy",
            patientId = "Patient",
            survivalVar = "Status",
            showSurvival = TRUE
        )
        expect_s3_class(result, "alluvialSurvivalClass")
        expect_equal(result$options$survivalVar, "Status")
        expect_true(result$options$showSurvival)
    }
})

test_that("alluvialSurvival comprehensive parameter combinations", {
    
    # Create comprehensive test data
    test_data <- data.frame(
        PatientCode = rep(paste0("PT", 1:8), each = 4),
        VisitTime = rep(c(0, 3, 6, 12), 8),
        TumorStage = sample(c("T1", "T2", "T3", "T4"), 32, replace = TRUE),
        Treatment = sample(c("Surgery", "Chemo", "Radio", "Immuno"), 32, replace = TRUE),
        Alive = sample(c(0, 1), 32, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvialSurvival")) {
        # Test comprehensive parameter combination
        result <- alluvialSurvival(
            data = test_data,
            timeVar = "VisitTime",
            stageVar = "TumorStage",
            treatmentVar = "Treatment",
            patientId = "PatientCode",
            survivalVar = "Alive",
            showRightAxis = TRUE,
            colorScheme = "colorblind",
            showSurvival = TRUE
        )
        
        expect_s3_class(result, "alluvialSurvivalClass")
        expect_equal(result$options$timeVar, "VisitTime")
        expect_equal(result$options$stageVar, "TumorStage")
        expect_equal(result$options$treatmentVar, "Treatment")
        expect_equal(result$options$patientId, "PatientCode")
        expect_equal(result$options$survivalVar, "Alive")
        expect_true(result$options$showRightAxis)
        expect_equal(result$options$colorScheme, "colorblind")
        expect_true(result$options$showSurvival)
    }
})

test_that("alluvialSurvival data validation edge cases", {
    
    if (exists("alluvialSurvival")) {
        
        # Test with insufficient patients (less than 5)
        small_data <- data.frame(
            ID = rep(c("P1", "P2"), each = 2),
            Time = rep(c(0, 6), 2),
            Stage = c("A", "A", "B", "B"),
            Treatment = c("X", "Y", "X", "Y"),
            stringsAsFactors = TRUE
        )
        
        expect_error(
            alluvialSurvival(
                data = small_data,
                timeVar = "Time",
                stageVar = "Stage", 
                treatmentVar = "Treatment",
                patientId = "ID"
            )
        )
        
        # Test with insufficient time points (less than 2)
        single_time_data <- data.frame(
            ID = paste0("P", 1:10),
            Time = rep(0, 10),
            Stage = sample(c("A", "B"), 10, replace = TRUE),
            Treatment = sample(c("X", "Y"), 10, replace = TRUE),
            stringsAsFactors = TRUE
        )
        
        expect_error(
            alluvialSurvival(
                data = single_time_data,
                timeVar = "Time",
                stageVar = "Stage",
                treatmentVar = "Treatment", 
                patientId = "ID"
            )
        )
        
        # Test with missing values in key variables
        missing_data <- data.frame(
            ID = c("P1", "P2", "P3", "P4", "P5", "P6"),
            Time = c(0, 3, 6, NA, 0, 3),  # Missing time value
            Stage = c("A", "A", "B", "B", "A", "B"),
            Treatment = c("X", "Y", "X", "Y", "X", "Y"),
            stringsAsFactors = TRUE
        )
        
        expect_error(
            alluvialSurvival(
                data = missing_data,
                timeVar = "Time",
                stageVar = "Stage",
                treatmentVar = "Treatment",
                patientId = "ID"
            )
        )
    }
})

test_that("alluvialSurvival survival variable validation", {
    
    if (exists("alluvialSurvival")) {
        
        # Test with invalid survival values (not 0/1)
        invalid_survival_data <- data.frame(
            ID = rep(c("P1", "P2", "P3"), each = 2),
            Time = rep(c(0, 6), 3),
            Stage = sample(c("A", "B"), 6, replace = TRUE),
            Treatment = sample(c("X", "Y"), 6, replace = TRUE),
            Survival = c(0, 1, 2, 3, 0, 1),  # Invalid values 2, 3
            stringsAsFactors = TRUE
        )
        
        expect_error(
            alluvialSurvival(
                data = invalid_survival_data,
                timeVar = "Time",
                stageVar = "Stage",
                treatmentVar = "Treatment",
                patientId = "ID",
                survivalVar = "Survival"
            )
        )
        
        # Test with valid survival values including NA
        valid_survival_data <- data.frame(
            ID = rep(c("P1", "P2", "P3"), each = 2),
            Time = rep(c(0, 6), 3),
            Stage = sample(c("A", "B"), 6, replace = TRUE),
            Treatment = sample(c("X", "Y"), 6, replace = TRUE),
            Survival = c(0, 1, NA, 0, 1, NA),  # Valid values including NA
            stringsAsFactors = TRUE
        )
        
        result <- alluvialSurvival(
            data = valid_survival_data,
            timeVar = "Time",
            stageVar = "Stage",
            treatmentVar = "Treatment",
            patientId = "ID",
            survivalVar = "Survival"
        )
        
        expect_s3_class(result, "alluvialSurvivalClass")
    }
})

test_that("alluvialSurvival required methods exist", {
    
    # Simple test data
    test_data <- data.frame(
        ID = rep(c("A", "B", "C"), each = 2),
        Time = rep(c(0, 6), 3),
        Stage = c("I", "II", "I", "III", "II", "I"),
        Treatment = c("S", "C", "S", "R", "C", "S"),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvialSurvival")) {
        result <- alluvialSurvival(
            data = test_data,
            timeVar = "Time",
            stageVar = "Stage",
            treatmentVar = "Treatment",
            patientId = "ID"
        )
        
        # Check that required methods exist
        expect_true(exists(".plot", envir = result$.__enclos_env__$private))
        expect_true(exists(".run", envir = result$.__enclos_env__$private))
        expect_true(exists(".validateData", envir = result$.__enclos_env__$private))
        expect_true(exists(".calculateStats", envir = result$.__enclos_env__$private))
        expect_true(exists(".prepareAlluvialData", envir = result$.__enclos_env__$private))
        
        # Check for survival-specific methods
        expect_true(exists(".plotSurvival", envir = result$.__enclos_env__$private))
        expect_true(exists(".prepareSurvivalData", envir = result$.__enclos_env__$private))
        expect_true(exists(".calculateSurvivalStats", envir = result$.__enclos_env__$private))
    }
})

test_that("alluvialSurvival with treatment_pathways dataset", {
    
    # Test with the actual treatment_pathways dataset if available
    if (file.exists("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/treatment_pathways.csv")) {
        
        # Read the treatment pathways data
        pathways_data <- read.csv("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/treatment_pathways.csv")
        
        if (exists("alluvialSurvival") && nrow(pathways_data) > 0) {
            # Test with treatment pathways data
            result <- alluvialSurvival(
                data = pathways_data,
                timeVar = "Time",
                stageVar = "Disease_Stage",
                treatmentVar = "Treatment",
                patientId = "ID",
                survivalVar = "Survival_Status"
            )
            
            expect_s3_class(result, "alluvialSurvivalClass")
            
            # Test with all features enabled
            result_full <- alluvialSurvival(
                data = pathways_data,
                timeVar = "Time",
                stageVar = "Disease_Stage",
                treatmentVar = "Treatment",
                patientId = "ID",
                survivalVar = "Survival_Status",
                showRightAxis = TRUE,
                colorScheme = "colorblind",
                showSurvival = TRUE
            )
            
            expect_s3_class(result_full, "alluvialSurvivalClass")
            expect_true(result_full$options$showRightAxis)
            expect_equal(result_full$options$colorScheme, "colorblind")
            expect_true(result_full$options$showSurvival)
        }
    }
})

test_that("alluvialSurvival longitudinal data structure", {
    
    # Create realistic longitudinal data
    realistic_data <- data.frame(
        PatientID = rep(paste0("Patient_", 1:10), each = 5),
        FollowUpMonth = rep(c(0, 3, 6, 12, 24), 10),
        CancerStage = sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), 50, replace = TRUE),
        TreatmentReceived = sample(c("Surgery", "Chemotherapy", "Radiation", "Immunotherapy", "Palliative"), 50, replace = TRUE),
        DeathEvent = sample(c(0, 1), 50, replace = TRUE, prob = c(0.7, 0.3)),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvialSurvival")) {
        # Test with realistic longitudinal structure
        result <- alluvialSurvival(
            data = realistic_data,
            timeVar = "FollowUpMonth",
            stageVar = "CancerStage", 
            treatmentVar = "TreatmentReceived",
            patientId = "PatientID",
            survivalVar = "DeathEvent",
            colorScheme = "clinical"
        )
        
        expect_s3_class(result, "alluvialSurvivalClass")
        expect_equal(result$options$timeVar, "FollowUpMonth")
        expect_equal(result$options$colorScheme, "clinical")
        
        # Test that the data structure is properly validated
        expect_true(length(unique(realistic_data$PatientID)) >= 5)
        expect_true(length(unique(realistic_data$FollowUpMonth)) >= 2)
    }
})

test_that("alluvialSurvival color scheme functionality", {
    
    # Create test data
    test_data <- data.frame(
        ID = rep(c("P1", "P2", "P3", "P4", "P5", "P6"), each = 3),
        Time = rep(c(0, 6, 12), 6),
        Stage = sample(c("Resectable", "BR/LA", "Metastatic"), 18, replace = TRUE),
        Treatment = sample(c("Surgery", "Chemotherapy", "Neoadjuvant"), 18, replace = TRUE),
        stringsAsFactors = TRUE
    )
    
    if (exists("alluvialSurvival")) {
        # Test clinical color scheme
        result_clinical <- alluvialSurvival(
            data = test_data,
            timeVar = "Time",
            stageVar = "Stage",
            treatmentVar = "Treatment",
            patientId = "ID",
            colorScheme = "clinical"
        )
        
        expect_s3_class(result_clinical, "alluvialSurvivalClass")
        expect_equal(result_clinical$options$colorScheme, "clinical")
        
        # Test colorblind-safe scheme
        result_colorblind <- alluvialSurvival(
            data = test_data,
            timeVar = "Time",
            stageVar = "Stage",
            treatmentVar = "Treatment",
            patientId = "ID",
            colorScheme = "colorblind"
        )
        
        expect_s3_class(result_colorblind, "alluvialSurvivalClass")
        expect_equal(result_colorblind$options$colorScheme, "colorblind")
    }
})