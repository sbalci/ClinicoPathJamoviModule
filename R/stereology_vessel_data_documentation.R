#' Example Vessel Density Data for Stereology Analysis
#'
#' @description
#' Simulated dataset of vessel density measurements using stereology methods
#' in tissue samples from patients with different diagnoses. This dataset
#' demonstrates quantitative histopathology using systematic grid-based sampling.
#'
#' @format A data frame with 50 observations and 13 variables:
#' \describe{
#'   \item{image_id}{Character. Unique image identifier}
#'   \item{diagnosis}{Factor. Diagnosis group (Normal, Benign, Malignant)}
#'   \item{tissue_type}{Character. Type of structure quantified (Blood Vessels)}
#'   \item{intersections}{Integer. Number of grid intersections with vessels}
#'   \item{total_points}{Integer. Total number of grid points examined (100)}
#'   \item{boundary_intersections}{Integer. Grid line intersections with vessel boundaries}
#'   \item{object_count}{Integer. Number of discrete vessels counted}
#'   \item{reference_area}{Numeric. Total reference area examined (μm²)}
#'   \item{grid_spacing}{Numeric. Distance between grid lines (μm)}
#'   \item{section_thickness}{Numeric. Histological section thickness (μm)}
#'   \item{Aa}{Numeric. Calculated area density (fraction)}
#'   \item{Vv}{Numeric. Calculated volume density (fraction)}
#' }
#'
#' @details
#' **Study Design:**
#' - 50 histological images from 3 diagnostic groups
#' - Systematic 10×10 grid overlay (100 points per image)
#' - Grid spacing: 10 μm
#' - Reference area: 1000×1000 μm (1 mm²)
#' - Section thickness: 5 μm (standard histology)
#'
#' **Clinical Context:**
#' Vessel density is a critical parameter in tumor pathology. Malignant tumors
#' often show increased angiogenesis (new blood vessel formation) to support
#' rapid growth. Stereology provides unbiased quantification of vessel density
#' from 2D histological sections without requiring 3D reconstruction.
#'
#' **Vessel Density by Diagnosis:**
#' - **Normal tissue**: Low vessel density (Aa ~ 0.05-0.10)
#' - **Benign tumors**: Moderate vessel density (Aa ~ 0.15-0.25)
#' - **Malignant tumors**: High vessel density (Aa ~ 0.30-0.50) due to angiogenesis
#'
#' **Stereological Parameters:**
#'
#' 1. **Area Density (Aa)**
#'    - Formula: Aa = intersections / total_points
#'    - Meaning: Fraction of tissue area occupied by vessels
#'    - Use: Primary measure of vessel density
#'
#' 2. **Volume Density (Vv)**
#'    - Formula: Vv = Aa (for isotropic random sections)
#'    - Meaning: Fraction of tissue volume occupied by vessels
#'    - Use: 3D estimate from 2D sections
#'
#' 3. **Boundary Density (Ba)**
#'    - Formula: Ba = (2 × boundary_intersections) / line_length
#'    - Meaning: Vessel boundary length per unit area
#'    - Use: Measure of vessel complexity
#'
#' 4. **Numerical Density (Na)**
#'    - Formula: Na = object_count / reference_area
#'    - Meaning: Number of vessels per unit area
#'    - Use: Vessel count density
#'
#' 5. **Surface Density (Sv)**
#'    - Formula: Sv = 2 × Ba
#'    - Meaning: Vessel surface area per unit volume
#'    - Use: Exchange surface quantification
#'
#' **Example Analysis:**
#' \preformatted{
#' # Load data
#' data(stereology_vessel_data, package = "ClinicoPath")
#'
#' # Basic stereology analysis
#' stereology(
#'   data = stereology_vessel_data,
#'   intersections = 'intersections',
#'   totalPoints = 'total_points',
#'   referenceArea = 'reference_area',
#'   gridSpacing = 'grid_spacing',
#'   calculateAa = TRUE,
#'   calculateVv = TRUE,
#'   showConfidenceIntervals = TRUE
#' )
#'
#' # Group comparison by diagnosis
#' stereology(
#'   data = stereology_vessel_data,
#'   intersections = 'intersections',
#'   totalPoints = 'total_points',
#'   referenceArea = 'reference_area',
#'   gridSpacing = 'grid_spacing',
#'   groupVar = 'diagnosis',
#'   showGroupComparison = TRUE
#' )
#'
#' # Advanced analysis with all parameters
#' stereology(
#'   data = stereology_vessel_data,
#'   intersections = 'intersections',
#'   totalPoints = 'total_points',
#'   boundaryIntersections = 'boundary_intersections',
#'   objectCount = 'object_count',
#'   referenceArea = 'reference_area',
#'   gridSpacing = 'grid_spacing',
#'   tissueType = 'vessels',
#'   calculateAa = TRUE,
#'   calculateVv = TRUE,
#'   calculateBa = TRUE,
#'   calculateNa = TRUE,
#'   calculateSv = TRUE,
#'   showConfidenceIntervals = TRUE,
#'   bootstrapIterations = 1000
#' )
#' }
#'
#' **Why Stereology is Needed:**
#' - Provides unbiased estimates of 3D parameters from 2D sections
#' - No need for complex 3D reconstruction
#' - Well-established statistical properties
#' - Reproducible across laboratories
#' - Applicable to any histological structure
#'
#' **Expected Results:**
#' - Malignant tumors show significantly higher vessel density (Aa ~ 0.39)
#' - Benign tumors have intermediate density (Aa ~ 0.20)
#' - Normal tissue has lowest density (Aa ~ 0.07)
#' - Statistical tests should show significant differences between groups
#'
#' **Quality Control:**
#' - Grid spacing appropriate for vessel size (10 μm)
#' - Adequate number of points per image (100 points)
#' - Multiple images per diagnosis (16-17 per group)
#' - Systematic random sampling approach
#'
#' @source Simulated data based on stereology principles from:
#' Kayser et al. (2009) Diagnostic Pathology 4:6.
#' Realistic vessel density values based on published tumor angiogenesis studies.
#'
#' @examples
#' \donttest{
#' # Load data
#' data(stereology_vessel_data, package = "ClinicoPath")
#'
#' # Summary by diagnosis
#' aggregate(Aa ~ diagnosis, data = stereology_vessel_data,
#'           FUN = function(x) c(mean = mean(x), sd = sd(x)))
#'
#' # Visualize vessel density distribution
#' boxplot(Aa ~ diagnosis, data = stereology_vessel_data,
#'         main = "Vessel Area Density by Diagnosis",
#'         xlab = "Diagnosis", ylab = "Area Density (Aa)",
#'         col = c("lightblue", "lightgreen", "lightcoral"))
#'
#' # Test for differences
#' summary(aov(Aa ~ diagnosis, data = stereology_vessel_data))
#' }
#'
#' @seealso
#' - [stereology()] for stereology analysis function
#' - Kayser et al. (2009) for theoretical framework
#' - Gundersen & Jensen (1985, 1986) for stereology methodology
#'
#' @keywords datasets
"stereology_vessel_data"
