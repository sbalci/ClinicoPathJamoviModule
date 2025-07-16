# Load the necessary libraries
library(jmv)
library(ClinicoPath)

# Load the test data
load("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/stage_migration_test_data.rda")

# Run the stage migration analysis with a comprehensive set of options
results <- ClinicoPath::stagemigration(
    data = stage_migration_test_data,
    oldStage = "OldStage",
    newStage = "NewStage",
    survivalTime = "SurvivalTime",
    event = "Event",
    eventLevel = "1",
    analysisType = "comprehensive",
    calculateNRI = TRUE,
    nriTimePoints = "12, 24, 36",
    calculateIDI = TRUE,
    performROCAnalysis = TRUE,
    rocTimePoints = "12, 24, 36",
    performDCA = TRUE,
    performCalibration = TRUE,
    performBootstrap = TRUE,
    bootstrapReps = 100,
    performHomogeneityTests = TRUE,
    performTrendTests = TRUE,
    performLikelihoodTests = TRUE,
    calculatePseudoR2 = TRUE,
    showMigrationOverview = TRUE,
    showMigrationSummary = TRUE,
    showStageDistribution = TRUE,
    showMigrationMatrix = TRUE,
    showStatisticalComparison = TRUE,
    showConcordanceComparison = TRUE,
    showMigrationHeatmap = TRUE,
    showROCComparison = TRUE,
    showCalibrationPlots = TRUE,
    showDecisionCurves = TRUE,
    showForestPlot = TRUE,
    showWillRogersAnalysis = TRUE,
    showSurvivalCurves = TRUE,
    survivalPlotType = "sidebyside",
    showConfidenceIntervals = TRUE,
    showRiskTables = TRUE,
    plotTimeRange = "auto",
    showClinicalInterpretation = TRUE,
    showStatisticalSummary = TRUE,
    showMethodologyNotes = TRUE,
    includeEffectSizes = TRUE,
    generateExecutiveSummary = TRUE,
    cancerType = "lung",
    useOptimismCorrection = TRUE,
    enableMultifactorialAnalysis = TRUE,
    continuousCovariates = c("Age"),
    categoricalCovariates = c("Gender"),
    multifactorialComparisonType = "comprehensive",
    baselineModel = "covariates_only",
    performInteractionTests = TRUE,
    stratifiedAnalysis = TRUE,
    showMultifactorialTables = TRUE,
    showAdjustedCIndexComparison = TRUE,
    showNestedModelTests = TRUE,
    showStepwiseResults = TRUE,
    showExplanations = TRUE
)

# Print the results to check for errors
print(results)