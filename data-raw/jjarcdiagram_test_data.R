# ═══════════════════════════════════════════════════════════
# Test Data Generation: jjarcdiagram
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic network test data for the jjarcdiagram jamovi function
#
# Generated: 2026-01-04
# Seed: 42
# Scenarios:
#   1. Gene regulatory network (gene-gene interactions)
#   2. Patient similarity network (patient-patient connections)
#   3. Disease co-occurrence network (disease-disease associations)
#   4. Protein interaction network (protein-protein binding)
#   5. Treatment pathway network (sequential interventions)
#
# Function purpose: Arc diagram visualization for network analysis
# Data format: Edge list (source, target, weight, group)

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══════════════════════════════════════════════════════════
# 1. GENE REGULATORY NETWORK
# ═══════════════════════════════════════════════════════════
# Genes regulating other genes in cancer pathways

genes <- c(
  "TP53", "BRCA1", "BRCA2", "EGFR", "MYC", "KRAS", "PTEN", "APC",
  "RB1", "CDKN2A", "PIK3CA", "BRAF", "ATM", "CDK4", "NRAS", "ERBB2"
)

gene_pathways <- c(
  "TP53" = "Tumor Suppressor",
  "BRCA1" = "DNA Repair",
  "BRCA2" = "DNA Repair",
  "EGFR" = "Growth Factor",
  "MYC" = "Oncogene",
  "KRAS" = "Oncogene",
  "PTEN" = "Tumor Suppressor",
  "APC" = "Tumor Suppressor",
  "RB1" = "Cell Cycle",
  "CDKN2A" = "Cell Cycle",
  "PIK3CA" = "Growth Factor",
  "BRAF" = "Oncogene",
  "ATM" = "DNA Repair",
  "CDK4" = "Cell Cycle",
  "NRAS" = "Oncogene",
  "ERBB2" = "Growth Factor"
)

# Create gene-gene regulatory relationships
gene_network_edges <- tibble(
  regulator = c(
    "TP53", "TP53", "TP53", "TP53", "TP53",
    "BRCA1", "BRCA1", "BRCA2", "BRCA2",
    "EGFR", "EGFR", "EGFR",
    "MYC", "MYC", "MYC",
    "KRAS", "KRAS", "KRAS",
    "PTEN", "PTEN", "APC",
    "RB1", "RB1", "CDKN2A",
    "PIK3CA", "PIK3CA",
    "BRAF", "ATM", "CDK4"
  ),
  target = c(
    "CDKN2A", "BAX", "MDM2", "GADD45", "P21",
    "ATM", "CHEK2", "RAD51", "PALB2",
    "PIK3CA", "KRAS", "BRAF",
    "CDK4", "CCND1", "E2F1",
    "PIK3CA", "BRAF", "MAPK",
    "AKT", "PIK3CA", "CTNNB1",
    "CDK4", "E2F1", "CDK4",
    "AKT", "MTOR",
    "MEK", "TP53", "RB1"
  ),
  regulation_score = c(
    0.92, 0.88, 0.85, 0.78, 0.95,
    0.87, 0.82, 0.90, 0.85,
    0.91, 0.89, 0.86,
    0.84, 0.88, 0.79,
    0.93, 0.90, 0.87,
    0.85, 0.91, 0.83,
    0.89, 0.86, 0.88,
    0.94, 0.90,
    0.92, 0.87, 0.85
  )
)

# Add pathway information
gene_network_edges$source_pathway <- gene_pathways[gene_network_edges$regulator]
gene_network_edges$target_pathway <- gene_pathways[gene_network_edges$target]

jjarcdiagram_gene_network <- gene_network_edges %>%
  transmute(
    source = regulator,
    target = target,
    weight = regulation_score,
    pathway = source_pathway,
    edge_type = "Regulatory"
  )

# ═══════════════════════════════════════════════════════════
# 2. PATIENT SIMILARITY NETWORK
# ═══════════════════════════════════════════════════════════
# Patients connected by clinical feature similarity

n_patients <- 20
patient_ids <- paste0("P", sprintf("%03d", 1:n_patients))
patient_subtypes <- sample(
  c("Luminal A", "Luminal B", "HER2+", "Triple Negative"),
  n_patients,
  replace = TRUE,
  prob = c(0.4, 0.3, 0.2, 0.1)
)

# Generate patient-patient similarity edges
patient_edges <- tibble()
for (i in 1:(n_patients - 1)) {
  for (j in (i + 1):n_patients) {
    # Higher similarity if same subtype
    same_subtype <- patient_subtypes[i] == patient_subtypes[j]
    base_similarity <- runif(1, 0.3, 0.7)

    if (same_subtype) {
      similarity <- min(1, base_similarity + runif(1, 0.2, 0.4))
    } else {
      similarity <- base_similarity
    }

    # Only keep edges with similarity > 0.6
    if (similarity > 0.6) {
      patient_edges <- bind_rows(
        patient_edges,
        tibble(
          patient1 = patient_ids[i],
          patient2 = patient_ids[j],
          similarity = similarity,
          subtype1 = patient_subtypes[i]
        )
      )
    }
  }
}

jjarcdiagram_patient_network <- patient_edges %>%
  transmute(
    source = patient1,
    target = patient2,
    weight = similarity,
    subtype = subtype1
  )

# ═══════════════════════════════════════════════════════════
# 3. DISEASE CO-OCCURRENCE NETWORK
# ═══════════════════════════════════════════════════════════
# Diseases that frequently occur together

diseases <- c(
  "Diabetes", "Hypertension", "Obesity", "CAD", "CKD",
  "COPD", "Asthma", "Depression", "Osteoporosis", "Arthritis"
)

disease_categories <- c(
  "Diabetes" = "Metabolic",
  "Hypertension" = "Cardiovascular",
  "Obesity" = "Metabolic",
  "CAD" = "Cardiovascular",
  "CKD" = "Renal",
  "COPD" = "Respiratory",
  "Asthma" = "Respiratory",
  "Depression" = "Mental Health",
  "Osteoporosis" = "Musculoskeletal",
  "Arthritis" = "Musculoskeletal"
)

comorbidity_edges <- tibble(
  disease1 = c(
    "Diabetes", "Diabetes", "Diabetes", "Diabetes",
    "Hypertension", "Hypertension", "Hypertension",
    "Obesity", "Obesity", "Obesity",
    "CAD", "CAD", "CKD",
    "COPD", "Depression",
    "Arthritis"
  ),
  disease2 = c(
    "Hypertension", "Obesity", "CAD", "CKD",
    "CAD", "CKD", "Obesity",
    "CAD", "Arthritis", "Depression",
    "CKD", "COPD", "Hypertension",
    "Asthma", "Arthritis",
    "Osteoporosis"
  ),
  cooccurrence = c(
    156, 142, 98, 87,
    134, 76, 145,
    121, 89, 67,
    112, 54, 92,
    78, 45,
    67
  )
)

jjarcdiagram_disease_network <- comorbidity_edges %>%
  transmute(
    source = disease1,
    target = disease2,
    weight = cooccurrence,
    category = disease_categories[disease1]
  )

# ═══════════════════════════════════════════════════════════
# 4. PROTEIN INTERACTION NETWORK
# ═══════════════════════════════════════════════════════════
# Protein-protein physical interactions

proteins <- c(
  "P53", "MDM2", "P21", "CDK2", "CYCLIN_E",
  "RB", "E2F1", "PCNA", "CDC6", "ORC1"
)

protein_functions <- c(
  "P53" = "Tumor Suppressor",
  "MDM2" = "E3 Ligase",
  "P21" = "CDK Inhibitor",
  "CDK2" = "Cell Cycle Kinase",
  "CYCLIN_E" = "Cell Cycle Regulator",
  "RB" = "Tumor Suppressor",
  "E2F1" = "Transcription Factor",
  "PCNA" = "DNA Replication",
  "CDC6" = "DNA Replication",
  "ORC1" = "DNA Replication"
)

protein_interactions <- tibble(
  protein_a = c(
    "P53", "P53", "P53",
    "MDM2", "MDM2",
    "P21", "P21",
    "CDK2", "CDK2",
    "CYCLIN_E", "CYCLIN_E",
    "RB", "RB",
    "E2F1", "PCNA"
  ),
  protein_b = c(
    "MDM2", "P21", "DNA",
    "E3UB", "P53",
    "CDK2", "PCNA",
    "CYCLIN_E", "P21",
    "CDK2", "RB",
    "E2F1", "CDK2",
    "PCNA", "CDC6"
  ),
  binding_affinity = c(
    0.95, 0.88, 0.92,
    0.89, 0.95,
    0.91, 0.84,
    0.93, 0.91,
    0.93, 0.87,
    0.90, 0.85,
    0.86, 0.88
  )
)

jjarcdiagram_protein_network <- protein_interactions %>%
  transmute(
    source = protein_a,
    target = protein_b,
    weight = binding_affinity,
    function_group = protein_functions[protein_a]
  )

# ═══════════════════════════════════════════════════════════
# 5. TREATMENT PATHWAY NETWORK
# ═══════════════════════════════════════════════════════════
# Sequential treatment pathways

treatments <- c(
  "Surgery", "Chemotherapy", "Radiation", "Immunotherapy",
  "Targeted_Therapy", "Hormone_Therapy", "Surveillance"
)

treatment_types <- c(
  "Surgery" = "Surgical",
  "Chemotherapy" = "Systemic",
  "Radiation" = "Local",
  "Immunotherapy" = "Systemic",
  "Targeted_Therapy" = "Systemic",
  "Hormone_Therapy" = "Systemic",
  "Surveillance" = "Monitoring"
)

treatment_pathways <- tibble(
  from_treatment = c(
    "Surgery", "Surgery", "Surgery",
    "Chemotherapy", "Chemotherapy", "Chemotherapy",
    "Radiation", "Radiation",
    "Immunotherapy", "Immunotherapy",
    "Targeted_Therapy", "Targeted_Therapy",
    "Hormone_Therapy"
  ),
  to_treatment = c(
    "Chemotherapy", "Radiation", "Surveillance",
    "Radiation", "Immunotherapy", "Surveillance",
    "Immunotherapy", "Surveillance",
    "Targeted_Therapy", "Surveillance",
    "Surveillance", "Hormone_Therapy",
    "Surveillance"
  ),
  transition_prob = c(
    0.65, 0.45, 0.25,
    0.55, 0.35, 0.40,
    0.30, 0.50,
    0.25, 0.45,
    0.60, 0.20,
    0.75
  )
)

jjarcdiagram_treatment_network <- treatment_pathways %>%
  transmute(
    source = from_treatment,
    target = to_treatment,
    weight = transition_prob,
    treatment_type = treatment_types[from_treatment]
  )

# ═══════════════════════════════════════════════════════════
# 6. COMPREHENSIVE TEST DATASET
# ═══════════════════════════════════════════════════════════
# Combines multiple scenarios for comprehensive testing

jjarcdiagram_test <- bind_rows(
  jjarcdiagram_gene_network %>% mutate(network_type = "Gene Regulatory"),
  jjarcdiagram_patient_network %>%
    rename(group = subtype) %>%
    mutate(network_type = "Patient Similarity"),
  jjarcdiagram_disease_network %>%
    rename(group = category) %>%
    mutate(network_type = "Disease Comorbidity")
) %>%
  mutate(
    # Standardize column names
    source = as.character(source),
    target = as.character(target),
    # Convert to factors
    source_fct = factor(source),
    target_fct = factor(target),
    network_type = factor(network_type)
  )

# Create group column (use pathway for genes, subtype for patients, category for diseases)
jjarcdiagram_test$group <- coalesce(
  jjarcdiagram_test$pathway,
  jjarcdiagram_test$group
)
jjarcdiagram_test$group <- factor(jjarcdiagram_test$group)

# Remove temporary columns
jjarcdiagram_test <- jjarcdiagram_test %>%
  select(source, target, weight, group, network_type, edge_type)

# ═══════════════════════════════════════════════════════════
# Save in Multiple Formats
# ═══════════════════════════════════════════════════════════

# 1. RDA format (native R)
save(jjarcdiagram_test, file = here::here("data", "jjarcdiagram_test.rda"))
save(jjarcdiagram_gene_network, file = here::here("data", "jjarcdiagram_gene_network.rda"))
save(jjarcdiagram_patient_network, file = here::here("data", "jjarcdiagram_patient_network.rda"))
save(jjarcdiagram_disease_network, file = here::here("data", "jjarcdiagram_disease_network.rda"))
save(jjarcdiagram_protein_network, file = here::here("data", "jjarcdiagram_protein_network.rda"))
save(jjarcdiagram_treatment_network, file = here::here("data", "jjarcdiagram_treatment_network.rda"))

# 2. CSV format
write.csv(jjarcdiagram_test,
          file = here::here("data", "jjarcdiagram_test.csv"),
          row.names = FALSE)
write.csv(jjarcdiagram_gene_network,
          file = here::here("data", "jjarcdiagram_gene_network.csv"),
          row.names = FALSE)
write.csv(jjarcdiagram_patient_network,
          file = here::here("data", "jjarcdiagram_patient_network.csv"),
          row.names = FALSE)

# 3. Excel format
writexl::write_xlsx(
  list(
    comprehensive = jjarcdiagram_test,
    gene_network = jjarcdiagram_gene_network,
    patient_network = jjarcdiagram_patient_network,
    disease_network = jjarcdiagram_disease_network,
    protein_network = jjarcdiagram_protein_network,
    treatment_network = jjarcdiagram_treatment_network
  ),
  path = here::here("data", "jjarcdiagram_test.xlsx")
)

# 4. Jamovi format (OMV)
jmvReadWrite::write_omv(jjarcdiagram_test,
                        here::here("data", "jjarcdiagram_test.omv"))

# ═══════════════════════════════════════════════════════════
# Generate Data Summary Report
# ═══════════════════════════════════════════════════════════

summary_text <- paste0("
═══════════════════════════════════════════════════════════
JJARCDIAGRAM TEST DATA SUMMARY
═══════════════════════════════════════════════════════════

Dataset: jjarcdiagram_test (Comprehensive Network Data)
Generated: ", Sys.Date(), "
Seed: 42

DIMENSIONS
----------
Total edges: ", nrow(jjarcdiagram_test), "
Network types: ", length(unique(jjarcdiagram_test$network_type)), "
Unique nodes: ", length(unique(c(jjarcdiagram_test$source, jjarcdiagram_test$target))), "
Edge groups: ", length(unique(jjarcdiagram_test$group)), "

VARIABLE DESCRIPTIONS
---------------------

Core Network Variables (required):
  • source [character/factor]: Starting node in each relationship (FROM node)
  • target [character/factor]: Ending node in each relationship (TO node)

Optional Variables:
  • weight [numeric, 0-1]: Connection strength, similarity, or importance
  • group [factor]: Node category for color-coding (pathway, subtype, function)
  • network_type [factor]: Type of network (Gene/Patient/Disease)
  • edge_type [character]: Relationship type descriptor

INCLUDED NETWORK DATASETS
--------------------------

1. Gene Regulatory Network (jjarcdiagram_gene_network)
   • ", nrow(jjarcdiagram_gene_network), " regulatory relationships
   • ", length(unique(c(jjarcdiagram_gene_network$source, jjarcdiagram_gene_network$target))), " genes
   • ", length(unique(jjarcdiagram_gene_network$pathway)), " pathways
   • Use for: Gene regulation analysis, pathway visualization

2. Patient Similarity Network (jjarcdiagram_patient_network)
   • ", nrow(jjarcdiagram_patient_network), " patient connections
   • ", length(unique(c(jjarcdiagram_patient_network$source, jjarcdiagram_patient_network$target))), " patients
   • ", length(unique(jjarcdiagram_patient_network$subtype)), " cancer subtypes
   • Use for: Clinical clustering, subtype analysis

3. Disease Co-occurrence Network (jjarcdiagram_disease_network)
   • ", nrow(jjarcdiagram_disease_network), " disease pairs
   • ", length(unique(c(jjarcdiagram_disease_network$source, jjarcdiagram_disease_network$target))), " diseases
   • ", length(unique(jjarcdiagram_disease_network$category)), " disease categories
   • Use for: Comorbidity analysis, epidemiology

4. Protein Interaction Network (jjarcdiagram_protein_network)
   • ", nrow(jjarcdiagram_protein_network), " protein interactions
   • ", length(unique(c(jjarcdiagram_protein_network$source, jjarcdiagram_protein_network$target))), " proteins
   • Binding affinity scores (0-1)
   • Use for: Protein complex analysis, molecular interactions

5. Treatment Pathway Network (jjarcdiagram_treatment_network)
   • ", nrow(jjarcdiagram_treatment_network), " treatment transitions
   • ", length(unique(c(jjarcdiagram_treatment_network$source, jjarcdiagram_treatment_network$target))), " treatment types
   • Transition probabilities (0-1)
   • Use for: Clinical pathway analysis, treatment sequencing

RECOMMENDED USAGE SCENARIOS
---------------------------

1. Basic Arc Diagram (Unweighted):
   - source: source
   - target: target
   - No weight specified

2. Weighted Arc Diagram:
   - source: source
   - target: target
   - weight: weight
   - arcWidth: weight

3. Grouped/Colored Network:
   - source: source
   - target: target
   - group: group
   - colorByGroup: TRUE

4. Comprehensive Network Analysis:
   - source: source
   - target: target
   - weight: weight
   - group: group
   - showNodes: TRUE
   - nodeSize: degree
   - sortNodes: group
   - showStats: TRUE

NETWORK CHARACTERISTICS
-----------------------

Gene Network:
  • Directed regulatory relationships
  • Weights = regulation scores (0.7-0.95)
  • Groups = biological pathways
  • Realistic cancer gene interactions

Patient Network:
  • Undirected similarity connections
  • Weights = similarity scores (>0.6 threshold)
  • Groups = cancer subtypes
  • Sparse network (high similarity only)

Disease Network:
  • Undirected comorbidity associations
  • Weights = co-occurrence counts (45-156)
  • Groups = disease categories
  • Common chronic disease pairs

EXAMPLE R CODE
--------------

# Load comprehensive test data
data(jjarcdiagram_test, package = 'ClinicoPath')

# Basic arc diagram
jjarcdiagram(
  data = jjarcdiagram_test,
  source = 'source',
  target = 'target'
)

# Weighted arc diagram with groups
jjarcdiagram(
  data = subset(jjarcdiagram_test, network_type == 'Gene Regulatory'),
  source = 'source',
  target = 'target',
  weight = 'weight',
  group = 'group',
  arcWidth = 'weight',
  colorByGroup = TRUE,
  showNodes = TRUE,
  nodeSize = 'degree',
  sortNodes = 'group'
)

# Load specific network type
data(jjarcdiagram_gene_network)
jjarcdiagram(
  data = jjarcdiagram_gene_network,
  source = 'source',
  target = 'target',
  weight = 'weight',
  group = 'pathway',
  directed = TRUE,
  showStats = TRUE
)

# Patient similarity network
data(jjarcdiagram_patient_network)
jjarcdiagram(
  data = jjarcdiagram_patient_network,
  source = 'source',
  target = 'target',
  weight = 'weight',
  group = 'subtype',
  colorByGroup = TRUE,
  sortNodes = 'group'
)

# Disease comorbidity network
data(jjarcdiagram_disease_network)
jjarcdiagram(
  data = jjarcdiagram_disease_network,
  source = 'source',
  target = 'target',
  weight = 'weight',
  group = 'category',
  arcWidth = 'weight',
  showStats = TRUE
)

FILES GENERATED
---------------
  ✓ data/jjarcdiagram_test.rda               (Comprehensive)
  ✓ data/jjarcdiagram_gene_network.rda       (Gene regulation)
  ✓ data/jjarcdiagram_patient_network.rda    (Patient similarity)
  ✓ data/jjarcdiagram_disease_network.rda    (Disease comorbidity)
  ✓ data/jjarcdiagram_protein_network.rda    (Protein interactions)
  ✓ data/jjarcdiagram_treatment_network.rda  (Treatment pathways)
  ✓ data/jjarcdiagram_test.csv               (CSV format)
  ✓ data/jjarcdiagram_test.xlsx              (Excel with multiple sheets)
  ✓ data/jjarcdiagram_test.omv               (Jamovi format)

═══════════════════════════════════════════════════════════
")

cat(summary_text)

# Save summary to file
writeLines(summary_text, here::here("JJARCDIAGRAM_TEST_DATA_SUMMARY.md"))

cat("\n✓ All test data files generated successfully!\n")
cat("✓ Summary saved to: JJARCDIAGRAM_TEST_DATA_SUMMARY.md\n\n")
