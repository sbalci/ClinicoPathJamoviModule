#' Oncology Datasets from OncoDataSets Package
#'
#' @description
#' A collection of 55 oncology datasets imported from the OncoDataSets package 
#' for use in ClinicoPath analyses. These datasets cover various cancer types,
#' study designs, and analysis scenarios commonly encountered in clinical
#' pathology and oncology research.
#'
#' @details
#' The datasets are organized into several categories:
#' 
#' **Survival Analysis (5 datasets):**
#' - `Melanoma_df`: Melanoma patient survival with tumor characteristics
#' - `LeukemiaSurvival_df`: Leukemia survival with treatment information  
#' - `ProstateSurvival_df`: Prostate cancer survival by grade, stage, age
#' - `NCCTGLungCancer_df`: NCCTG lung cancer trial data
#' - `OvarianCancer_df`: Ovarian cancer trial survival data
#' 
#' **Diagnostic/Decision Analysis (3 datasets):**
#' - `PSAProstateCancer_df`: PSA levels and prostate cancer outcomes
#' - `CA19PancreaticCancer_df`: CA19-9 diagnostic accuracy studies
#' - `LungNodulesDetected_df`: Lung nodule characteristics and malignancy
#' 
#' **Descriptive/Comparative Analysis (6 datasets):**
#' - `BreastCancerWI_df`: Wisconsin Breast Cancer diagnostic features
#' - `ChildCancer_df`: Childhood cancer epidemiological data
#' - `BladderCancer_df`: Bladder cancer patient characteristics
#' - `SmokingLungCancer_df`: Smoking status and lung cancer relationship
#' - `BrainCancerCases_df`: Brain cancer case characteristics
#' - `BrainCancerGeo_df`: Brain cancer geographic distribution
#' 
#' **Biomarker Analysis (5 datasets):**
#' - `BRCA1BreastCancer_df`: BRCA1 mutations in breast cancer
#' - `BRCA2BreastCancer_df`: BRCA2 mutations in breast cancer
#' - `BRCA1OvarianCancer_df`: BRCA1 mutations in ovarian cancer
#' - `BRCA2OvarianCancer_df`: BRCA2 mutations in ovarian cancer
#' - `CASP8BreastCancer_df`: CASP8 gene variants in breast cancer
#' 
#' **Additional categories include:** Treatment Outcomes, Epidemiological,
#' Molecular/Genomic, Experimental, Case-Control Studies, Risk Factors,
#' Clinical Outcomes, and Specialized Studies.
#'
#' @usage
#' # Load a specific dataset
#' data("Melanoma_df")
#' 
#' # View all available oncology datasets
#' data("oncology_datasets_summary")
#' View(oncology_datasets_summary)
#' 
#' # Example analyses:
#' # Survival analysis with Melanoma data
#' data("Melanoma_df")
#' # Use in jsurvival module
#' 
#' # ROC analysis with PSA data  
#' data("PSAProstateCancer_df")
#' # Use in meddecide module
#' 
#' # Descriptive statistics with Breast Cancer data
#' data("BreastCancerWI_df") 
#' # Use in ClinicoPathDescriptives module
#'
#' @references
#' Caceres Rossi, R. (2024). OncoDataSets: A Rich Collection of Data Focused on Cancer Research.
#' R package version 0.1.0. https://CRAN.R-project.org/package=OncoDataSets
#'
#' @seealso
#' \itemize{
#'   \item{\href{https://www.serdarbalci.com/ClinicoPathDescriptives/}{ClinicoPathDescriptives documentation}}
#'   \item{\href{https://www.serdarbalci.com/jsurvival/}{jsurvival documentation}}
#'   \item{\href{https://www.serdarbalci.com/meddecide/}{meddecide documentation}}  
#'   \item{\href{https://www.serdarbalci.com/jjstatsplot/}{jjstatsplot documentation}}
#' }
#'
#' @examples
#' \dontrun{
#' # Load and explore melanoma survival data
#' data("Melanoma_df")
#' str(Melanoma_df)
#' 
#' # Create survival object
#' library(survival)
#' surv_obj <- Surv(Melanoma_df$time, Melanoma_df$status == 1)
#' 
#' # Load PSA data for ROC analysis
#' data("PSAProstateCancer_df")
#' # Create binary outcome
#' PSAProstateCancer_df$high_grade <- ifelse(PSAProstateCancer_df$gleason >= 7, 1, 0)
#' 
#' # Load breast cancer data for descriptive analysis
#' data("BreastCancerWI_df")
#' table(BreastCancerWI_df$diagnosis)
#' }
#'
#' @name oncology-datasets
#' @docType data
#' @keywords datasets oncology cancer survival diagnostic
NULL

#' Oncology Datasets Summary
#'
#' @description
#' A summary data frame containing information about all 55 oncology datasets
#' available in the ClinicoPath package, imported from OncoDataSets.
#'
#' @format A data frame with 55 rows and 3 columns:
#' \describe{
#'   \item{dataset_name}{Character. Name of the dataset}
#'   \item{category}{Character. Analysis category (e.g., "Survival Analysis", "Diagnostic/Decision Analysis")}
#'   \item{primary_use}{Character. Primary analytical use case}
#' }
#'
#' @usage
#' data("oncology_datasets_summary")
#'
#' @examples
#' \dontrun{
#' # Load the summary
#' data("oncology_datasets_summary")
#' 
#' # View all datasets by category
#' table(oncology_datasets_summary$category)
#' 
#' # Find survival analysis datasets
#' survival_datasets <- subset(oncology_datasets_summary, 
#'                            category == "Survival Analysis")
#' print(survival_datasets)
#' 
#' # Find datasets for ROC analysis
#' roc_datasets <- subset(oncology_datasets_summary, 
#'                       grepl("ROC|diagnostic", primary_use, ignore.case = TRUE))
#' print(roc_datasets)
#' }
#'
#' @name oncology_datasets_summary
#' @docType data
#' @keywords datasets summary oncology
NULL