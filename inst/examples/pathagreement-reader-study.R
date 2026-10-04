# Entirely synthetic readings, not reconstructed data from the thyroid article.
# Reproducible long-format fixed-reader study with patient clustering.
set.seed(70149)
reader_study <- expand.grid(case_id = sprintf("C%03d", 1:60),
  reader = c("Reader A", "Reader B", "Reader C"),
  modality = c("Microscopy", "Assisted"), stringsAsFactors = FALSE)
case_number <- match(reader_study$case_id, unique(reader_study$case_id))
truth <- rep(c("II", "III", "IV", "VI"), 15)
reader_study$consensus_binary <- factor(ifelse(truth[case_number] == "II", "Negative", "Positive"),
                                      levels = c("Negative", "Positive", "Excluded I"))
reader_study$patient_id <- paste0("P", ceiling(case_number / 2))
labels <- truth[case_number]
error <- runif(length(labels)) < ifelse(reader_study$modality == "Assisted", .12, .25)
labels[error] <- sample(c("II", "III", "IV", "VI"), sum(error), replace = TRUE)
labels[runif(length(labels)) < .07] <- "I"
reader_study$rating <- ordered(labels, c("I", "II", "III", "IV", "V", "VI"))
# V is deliberately declared but unused; it stays in the transition matrix.
result <- ClinicoPath::pathagreement(reader_study,
  readerWorkflow = "ordinal_cutoff", readerFormat = "long", caseID = "case_id",
  readerID = "reader", modality = "modality", rating = "rating",
  baselineModality = "Microscopy", comparisonModality = "Assisted",
  referenceStandard = "consensus_binary", referencePositive = "Positive",
  referenceNegative = "Negative", referenceType = "consensus",
  referenceDescription = "Synthetic expert cytology consensus",
  targetDefinition = "Bethesda III to VI versus II",
  ordinalCutoff = "III", readerNondiagnostic = TRUE, readerNondiagnosticLevel = "I",
  readerAllObservedAgreement = TRUE, readerModalityComparison = TRUE,
  modalityPopulation = "all_observed", readerCluster = "patient_id",
  readerBootstrap = TRUE, bootstrapSamples = 1000, seed = 42,
  readerTransitions = TRUE, readerCode = TRUE)
print(result)

# Article microscopy aggregate count fixture: arithmetic only.
# These counts contain no reader/case pairing and cannot identify a paired CI.
article_counts <- c(a = 108, b = 13, c = 63, d = 67, nd_positive = 9, nd_negative = 25)
with(as.list(article_counts), c(
  determinate_PPA = a / (a + c), determinate_NPA = d / (b + d),
  observed_positive_agreement = a / (a + c + nd_positive),
  observed_negative_agreement = d / (b + d + nd_negative),
  positive_call_reference_agreement = a / (a + b),
  symmetric_positive_agreement = 2 * a / (2 * a + b + c)))
