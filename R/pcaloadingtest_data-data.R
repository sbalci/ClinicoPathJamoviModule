#' Example Dataset for PCA Loading Significance Test
#'
#' A simulated dataset designed to demonstrate permutation-based significance
#' testing for PCA loadings using the permV method (Linting et al., 2011).
#' The dataset contains variables with known correlation structure to illustrate
#' how the loading significance test identifies meaningful vs. noise variables.
#'
#' @format A data frame with 100 rows and 10 variables:
#' \describe{
#'   \item{var1}{Numeric variable, primary loading on PC1 (mean=50, sd=10)}
#'   \item{var2}{Numeric variable, high correlation with var1 (r=0.8), loads on PC1}
#'   \item{var3}{Numeric variable, moderate correlation with var1 (r=0.7), loads on PC1}
#'   \item{var4}{Numeric variable, primary loading on PC2 (mean=100, sd=15)}
#'   \item{var5}{Numeric variable, high correlation with var4 (r=0.75), loads on PC2}
#'   \item{var6}{Numeric variable, primary loading on PC3 (mean=75, sd=12)}
#'   \item{var7}{Numeric variable, moderate correlation with var6 (r=0.6), loads on PC3}
#'   \item{noise}{Noise variable with no structure, should show no significant loadings}
#'   \item{Body Mass Index}{Variable with spaces to test escapeVariableNames (mean=25, sd=3)}
#'   \item{Blood Pressure}{Variable with spaces to test escapeVariableNames (mean=120, sd=15)}
#' }
#'
#' @details
#' This dataset is specifically designed to test the PCA loading significance
#' functionality:
#'
#' **Correlation Structure:**
#' - **PC1 cluster:** var1, var2, var3 are highly intercorrelated (r > 0.8)
#' - **PC2 cluster:** var4, var5 are highly correlated (r = 0.91)
#' - **PC3 cluster:** var6, var7 are moderately correlated (r = 0.65)
#' - **Noise:** The 'noise' variable is independent and should not show
#'   significant loadings on any component
#'
#' **Special Features:**
#' - Contains ~5% missing values in var1 to test NA handling
#' - Includes variables with spaces in names ("Body Mass Index", "Blood Pressure")
#'   to test the escapeVariableNames functionality
#'
#' **Expected Results:**
#' When analyzed with default settings (center=TRUE, scale=TRUE, ncomp=3):
#' - var1, var2, var3 should have significant loadings on PC1
#' - var4, var5 should have significant loadings on PC2
#' - var6, var7 should have significant loadings on PC3
#' - 'noise' should have no significant loadings (after FDR adjustment)
#'
#' @source Simulated data with known correlation structure
#'
#' @references
#' Linting M, van Os BJ, Meulman JJ. (2011). Statistical Significance of the
#' Contribution of Variables to the PCA solution: An Alternative Permutation
#' Strategy. Psychometrika, 76(3):440-460.
#'
#' @examples
#' # Load the data
#' data("pcaloadingtest_data")
#'
#' # Check structure
#' str(pcaloadingtest_data)
#'
#' # View correlation matrix
#' cor(pcaloadingtest_data[, 1:8], use = "pairwise.complete.obs")
#'
#' \dontrun{
#' # Run PCA loading significance test
#' pcaloadingtest(
#'   data = pcaloadingtest_data,
#'   vars = c("var1", "var2", "var3", "var4", "var5", "var6", "var7", "noise"),
#'   ncomp = 3,
#'   nperm = 1000,
#'   center = TRUE,
#'   scale = TRUE,
#'   conflevel = 0.95,
#'   adjustmethod = "BH"
#' )
#' }
#'
#' @keywords datasets
"pcaloadingtest_data"
