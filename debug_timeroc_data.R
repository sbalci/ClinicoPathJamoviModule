
load("data/timeroc_cancer_biomarker.rda")
print("Structure of timeroc_cancer_biomarker:")
str(timeroc_cancer_biomarker)
print("Class of follow_up_months:")
print(class(timeroc_cancer_biomarker$follow_up_months))
print("Head of follow_up_months:")
print(head(timeroc_cancer_biomarker$follow_up_months))

if (requireNamespace("jmvcore", quietly=TRUE)) {
    print("jmvcore is available.")
} else {
    print("jmvcore is NOT available.")
}
