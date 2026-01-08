# ═══════════════════════════════════════════════════════════
# Arc Diagram Examples
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates usage of the jjarcdiagram function
# for network visualization with various clinical and biological scenarios.

\dontrun{

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Arc Diagram (Unweighted Network)
# ═══════════════════════════════════════════════════════════

# Load test data
data(jjarcdiagram_test, package = "ClinicoPath")

# Simple arc diagram with just connections
jjarcdiagram(
  data = jjarcdiagram_test,
  source = "source",
  target = "target"
)

# ═══════════════════════════════════════════════════════════
# Example 2: Gene Regulatory Network
# ═══════════════════════════════════════════════════════════

# Load gene network data
data(jjarcdiagram_gene_network)

# Visualize gene regulation with pathways
jjarcdiagram(
  data = jjarcdiagram_gene_network,
  source = "source",
  target = "target",
  weight = "weight",
  group = "pathway",
  directed = TRUE,
  arcWidth = "weight",
  colorByGroup = TRUE,
  showNodes = TRUE,
  nodeSize = "degree",
  sortNodes = "group",
  showStats = TRUE,
  plotTitle = "Cancer Gene Regulatory Network"
)

# ═══════════════════════════════════════════════════════════
# Example 3: Patient Similarity Network
# ═══════════════════════════════════════════════════════════

# Load patient network data
data(jjarcdiagram_patient_network)

# Visualize patient clustering by cancer subtype
jjarcdiagram(
  data = jjarcdiagram_patient_network,
  source = "source",
  target = "target",
  weight = "weight",
  group = "subtype",
  directed = FALSE,
  arcWidth = "weight",
  colorByGroup = TRUE,
  showNodes = TRUE,
  nodeSize = "degree",
  sortNodes = "group",
  arcColorMode = "gradient",
  plotTitle = "Patient Similarity Network (>60% similarity)"
)

# ═══════════════════════════════════════════════════════════
# Example 4: Disease Comorbidity Network
# ═══════════════════════════════════════════════════════════

# Load disease network data
data(jjarcdiagram_disease_network)

# Visualize disease co-occurrence patterns
jjarcdiagram(
  data = jjarcdiagram_disease_network,
  source = "source",
  target = "target",
  weight = "weight",
  group = "category",
  directed = FALSE,
  arcWidth = "weight",
  colorByGroup = TRUE,
  showNodes = TRUE,
  nodeSize = "degree",
  sortNodes = "group",
  showStats = TRUE,
  showLegend = TRUE,
  plotTitle = "Chronic Disease Comorbidity Network"
)

# ═══════════════════════════════════════════════════════════
# Example 5: Protein Interaction Network
# ═══════════════════════════════════════════════════════════

# Load protein interaction data
data(jjarcdiagram_protein_network)

# Visualize protein-protein interactions
jjarcdiagram(
  data = jjarcdiagram_protein_network,
  source = "source",
  target = "target",
  weight = "weight",
  group = "function_group",
  directed = FALSE,
  arcWidth = "weight",
  colorByGroup = TRUE,
  showNodes = TRUE,
  nodeSize = "degree",
  sortNodes = "degree",
  sortDecreasing = TRUE,
  plotTitle = "Cell Cycle Protein Interaction Network"
)

# ═══════════════════════════════════════════════════════════
# Example 6: Treatment Pathway Network
# ═══════════════════════════════════════════════════════════

# Load treatment pathway data
data(jjarcdiagram_treatment_network)

# Visualize treatment sequences and transitions
jjarcdiagram(
  data = jjarcdiagram_treatment_network,
  source = "source",
  target = "target",
  weight = "weight",
  group = "treatment_type",
  directed = TRUE,
  arcWidth = "weight",
  colorByGroup = TRUE,
  showNodes = TRUE,
  nodeSize = "fixed",
  nodeSizeValue = 3,
  sortNodes = "group",
  showStats = TRUE,
  plotTitle = "Cancer Treatment Pathway Network"
)

# ═══════════════════════════════════════════════════════════
# Example 7: Horizontal Layout
# ═══════════════════════════════════════════════════════════

# Use horizontal orientation for better readability
jjarcdiagram(
  data = jjarcdiagram_gene_network,
  source = "source",
  target = "target",
  weight = "weight",
  group = "pathway",
  horizontal = TRUE,
  showNodes = TRUE,
  nodeSize = "degree",
  sortNodes = "name",
  plotTitle = "Gene Network (Horizontal Layout)"
)

# ═══════════════════════════════════════════════════════════
# Example 8: Analysis Presets
# ═══════════════════════════════════════════════════════════

# Using gene interaction preset
jjarcdiagram(
  data = jjarcdiagram_gene_network,
  source = "source",
  target = "target",
  weight = "weight",
  group = "pathway",
  analysisPreset = "gene_interaction",
  showStats = TRUE
)

# Using patient network preset
jjarcdiagram(
  data = jjarcdiagram_patient_network,
  source = "source",
  target = "target",
  weight = "weight",
  group = "subtype",
  analysisPreset = "patient_network",
  showStats = TRUE
)

# Using comorbidity network preset
jjarcdiagram(
  data = jjarcdiagram_disease_network,
  source = "source",
  target = "target",
  weight = "weight",
  group = "category",
  analysisPreset = "comorbidity_network",
  showStats = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 9: Custom Visualization Options
# ═══════════════════════════════════════════════════════════

# Fine-tuned visual appearance
jjarcdiagram(
  data = jjarcdiagram_disease_network,
  source = "source",
  target = "target",
  weight = "weight",
  group = "category",
  arcWidth = "weight",
  arcWidthValue = 1.5,
  arcTransparency = 0.3,
  colorByGroup = TRUE,
  showNodes = TRUE,
  nodeSize = "degree",
  nodeSizeValue = 2.5,
  labelSize = 1.0,
  sortNodes = "degree",
  sortDecreasing = TRUE,
  showLegend = TRUE,
  plotTitle = "Custom Styled Comorbidity Network"
)

# ═══════════════════════════════════════════════════════════
# Example 10: Network Statistics and Interpretation
# ═══════════════════════════════════════════════════════════

# Comprehensive network analysis with all outputs
jjarcdiagram(
  data = jjarcdiagram_gene_network,
  source = "source",
  target = "target",
  weight = "weight",
  group = "pathway",
  directed = TRUE,
  weightMode = "strength",
  aggregateEdges = TRUE,
  arcWidth = "weight",
  colorByGroup = TRUE,
  showNodes = TRUE,
  nodeSize = "degree",
  sortNodes = "degree",
  sortDecreasing = TRUE,
  showStats = TRUE,
  showSummary = TRUE,
  showAssumptions = TRUE,
  showGlossary = TRUE,
  plotTitle = "Comprehensive Gene Regulatory Network Analysis"
)

# ═══════════════════════════════════════════════════════════
# Example 11: Filtering and Subsetting Networks
# ═══════════════════════════════════════════════════════════

# Filter to show only strong connections
strong_connections <- subset(
  jjarcdiagram_test,
  network_type == "Gene Regulatory" & weight > 0.85
)

jjarcdiagram(
  data = strong_connections,
  source = "source",
  target = "target",
  weight = "weight",
  group = "group",
  arcWidth = "weight",
  colorByGroup = TRUE,
  showNodes = TRUE,
  plotTitle = "High-Confidence Gene Interactions (>0.85)"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Different Arc Coloring Modes
# ═══════════════════════════════════════════════════════════

# Color by source node
jjarcdiagram(
  data = jjarcdiagram_gene_network,
  source = "source",
  target = "target",
  group = "pathway",
  colorByGroup = TRUE,
  arcColorMode = "source",
  showNodes = TRUE,
  plotTitle = "Colored by Source Pathway"
)

# Color by target node
jjarcdiagram(
  data = jjarcdiagram_gene_network,
  source = "source",
  target = "target",
  group = "pathway",
  colorByGroup = TRUE,
  arcColorMode = "target",
  showNodes = TRUE,
  plotTitle = "Colored by Target Pathway"
)

# Gradient coloring (source to target)
jjarcdiagram(
  data = jjarcdiagram_gene_network,
  source = "source",
  target = "target",
  group = "pathway",
  colorByGroup = TRUE,
  arcColorMode = "gradient",
  showNodes = TRUE,
  plotTitle = "Gradient Coloring (Source→Target)"
)

}
