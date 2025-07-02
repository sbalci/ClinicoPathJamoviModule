\name{chisqposttest_test_data}
\alias{chisqposttest_test_data}
\docType{data}

\title{
Test Dataset for Chi-Square Post-Hoc Analysis
}

\description{
A comprehensive test dataset specifically designed for testing the chisqposttest function. Contains multiple categorical variables with known associations of different strengths, edge cases, and missing data patterns.
}

\usage{data("chisqposttest_test_data")}

\format{
A data frame with 300 observations and 14 variables:
\describe{
  \item{PatientID}{Patient identifier (1-300)}
  \item{Treatment}{Treatment group: "Standard", "Experimental"}
  \item{Response}{Treatment response: "No Response", "Response" (strongly associated with Treatment)}
  \item{Sex}{Patient sex: "Male", "Female" (balanced)}
  \item{TumorGrade}{Tumor grade: "Grade 1", "Grade 2", "Grade 3"}
  \item{TumorStage}{Tumor stage: "Stage I", "Stage II", "Stage III" (moderately associated with TumorGrade)}
  \item{Institution}{Hospital: "Hospital A", "Hospital B", "Hospital C", "Hospital D"}
  \item{QualityScore}{Quality rating: "High", "Low" (weakly associated with Institution)}
  \item{RandomVar1}{Random variable: "Group A", "Group B", "Group C" (no associations)}
  \item{RandomVar2}{Random variable: "Type X", "Type Y" (no associations)}
  \item{RareCategory}{Frequency category: "Common", "Uncommon", "Rare" (unbalanced)}
  \item{BinaryOutcome}{Binary outcome: "Negative", "Positive" (associated with RareCategory)}
  \item{AgeGroup}{Age category: "Young", "Middle", "Elderly"}
  \item{BiomarkerStatus}{Biomarker status: "Negative", "Positive" (moderately associated with AgeGroup)}
}
}

\details{
This dataset contains several types of associations designed to test different aspects of chi-square post-hoc analysis:

\strong{Strong Associations:}
\itemize{
  \item Treatment -> Response: Clear treatment effect with odds ratio ~5
}

\strong{Moderate Associations:}
\itemize{
  \item TumorGrade -> TumorStage: Higher grades associated with advanced stages
  \item AgeGroup -> BiomarkerStatus: Age-related biomarker expression pattern
}

\strong{Weak Associations:}
\itemize{
  \item Institution -> QualityScore: Institutional quality differences
  \item RareCategory -> BinaryOutcome: Effect in rare category with small cell counts
}

\strong{No Associations:}
\itemize{
  \item RandomVar1 ⊥ RandomVar2: Independent random variables for null hypothesis testing
}

The dataset includes approximately 5\% missing data in Treatment, Sex, and TumorGrade variables to test missing data handling options.
}

\source{
Simulated data created for testing purposes. Associations are based on realistic clinical scenarios but data is artificially generated.
}

\examples{
# Load the dataset
data(chisqposttest_test_data)

# Examine structure
str(chisqposttest_test_data)

# Example 1: Strong association (should be highly significant)
chisqposttest(
  data = chisqposttest_test_data,
  rows = "Treatment",
  cols = "Response",
  posthoc = "bonferroni"
)

# Example 2: Moderate association (should be significant with post-hoc differences)
chisqposttest(
  data = chisqposttest_test_data,
  rows = "TumorGrade", 
  cols = "TumorStage",
  posthoc = "fdr"
)

# Example 3: No association (should be non-significant)
chisqposttest(
  data = chisqposttest_test_data,
  rows = "RandomVar1",
  cols = "RandomVar2",
  posthoc = "bonferroni"
)

# Example 4: Edge case with rare categories
chisqposttest(
  data = chisqposttest_test_data,
  rows = "RareCategory",
  cols = "BinaryOutcome",
  posthoc = "fdr"
)

# Example 5: Missing data handling
chisqposttest(
  data = chisqposttest_test_data,
  rows = "Treatment",
  cols = "Sex",
  excl = TRUE  # Exclude missing values
)
}

\seealso{
\code{\link{chisqposttest}}, \code{\link{histopathology}}
}

\keyword{datasets}