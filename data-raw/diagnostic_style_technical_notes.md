# Diagnostic Style Clustering - Technical Implementation Notes

## Overview

This document describes the technical implementation of the diagnostic style clustering feature based on Usubutun et al. (2012) methodology for identifying pathologist "diagnostic schools."

## Core Algorithm

### 1. Distance Matrix Calculation

Three distance metrics are implemented:

#### Percentage Agreement (Default)
```r
distance = 1 - (agreement_count / total_cases)
```

#### Correlation Distance  
```r
rater_i_numeric <- as.numeric(as.factor(rater_i_diagnoses))
rater_j_numeric <- as.numeric(as.factor(rater_j_diagnoses))
correlation <- cor(rater_i_numeric, rater_j_numeric, use = "complete.obs")
distance <- 1 - abs(correlation)
```

#### Euclidean Distance
```r
rater_i_numeric <- as.numeric(as.factor(rater_i_diagnoses))
rater_j_numeric <- as.numeric(as.factor(rater_j_diagnoses))
distance <- sqrt(sum((rater_i_numeric - rater_j_numeric)^2)) / n_cases
```

### 2. Hierarchical Clustering

Uses R's `hclust()` function with three linkage methods:

- **Ward's Linkage**: `method = "ward.D2"` (default, minimizes within-cluster variance)
- **Complete Linkage**: `method = "complete"` (tight clusters)
- **Average Linkage**: `method = "average"` (balanced approach)

### 3. Cluster Assignment

```r
style_groups <- cutree(hclust_result, k = n_groups)
```

## Key Functions

### `.performDiagnosticStyleAnalysis()`
Main coordination function that:
1. Calculates distance matrix
2. Performs hierarchical clustering  
3. Assigns style groups
4. Populates results tables
5. Stores results for visualization

### `.calculateRaterDistanceMatrix()`
Creates symmetric distance matrix between all rater pairs using selected metric.

### `.calculateWithinGroupAgreement()`
Computes average agreement between a rater and others in same style group.

### `.generateStyleSummary()`
Creates group-level statistics and identifies predominant characteristics.

### `.identifyDiscordantCases()`
Finds cases where different style groups systematically disagree.

## Data Structures

### Private Fields
```r
.style_clustering_results <- list(
  hclust = hclust_object,
  groups = style_group_assignments,
  distance_matrix = distance_matrix
)
```

### Results Tables

#### Diagnostic Style Table
- Individual rater assignments
- Within-group agreement scores
- Rater characteristics

#### Style Summary Table  
- Group-level statistics
- Predominant characteristics
- Average within-group agreement

#### Discordant Cases Table
- Cases with high inter-style disagreement
- Group-specific diagnoses
- Discord scores

## Visualization Functions

### `.diagnosticStyleDendrogram()`
Creates hierarchical clustering dendrogram using `ggdendro` package.

### `.diagnosticStyleHeatmap()`
Generates style-grouped heatmap showing diagnostic category usage patterns.

## Statistical Considerations

### Minimum Requirements
- **Minimum raters**: 6-8 for meaningful clustering
- **Minimum cases**: 30-50 for stable patterns
- **Case diversity**: Include range of diagnostic difficulties

### Distance Metric Selection
- **Percentage Agreement**: Best for categorical diagnoses
- **Correlation**: Better for ordinal scales
- **Euclidean**: Appropriate for quantitative measures

### Group Number Determination
- **2 groups**: Conservative vs. Aggressive
- **3 groups**: Conservative, Moderate, Aggressive (Usubutun standard)
- **4+ groups**: Subspecialty-specific patterns

### Validation Metrics
- **Within-group agreement**: Should be >70% for meaningful groups
- **Silhouette analysis**: Could be added for cluster validation
- **Characteristic correlation**: Groups should correlate with known factors

## Performance Considerations

### Computational Complexity
- Distance matrix: O(n²) where n = number of raters
- Hierarchical clustering: O(n³) worst case
- Generally fast for typical pathology studies (n < 50)

### Memory Usage
- Distance matrix storage: n² elements
- Clustering results: Linear in number of raters
- Minimal memory requirements for typical use cases

## Integration with jamovi Framework

### YAML Configuration
Options defined in `agreement.a.yaml`:
- `diagnosticStyleAnalysis`: Boolean toggle
- `styleClusterMethod`: Linkage method selection
- `styleDistanceMetric`: Distance metric choice
- `numberOfStyleGroups`: Target cluster count
- Various characteristic variable selectors

### R6 Class Integration
Inherits from `agreementBase` auto-generated class, follows jamovi module patterns.

### Output Tables
Uses jamovi table framework for consistent formatting and display.

## Error Handling

### Input Validation
- Minimum number of raters check
- Complete cases requirement
- Factor variable validation

### Clustering Failures
- Handles degenerate clustering cases
- Provides informative error messages
- Graceful degradation when characteristics missing

### Missing Data
- Complete case analysis only
- Clear reporting of excluded cases
- Warnings for high missing data rates

## Extension Points

### Additional Distance Metrics
Easy to add new metrics in `.calculateRaterDistanceMatrix()`:
```r
} else if (distance_metric == "custom") {
  distance <- custom_distance_function(rater_i, rater_j)
}
```

### Alternative Clustering Methods
Could extend to include:
- k-means clustering
- Model-based clustering
- Density-based clustering

### Enhanced Validation
Potential additions:
- Bootstrap cluster stability
- Cross-validation approaches
- Permutation testing

## Testing Strategy

### Unit Tests
- Distance metric calculations
- Clustering assignment accuracy
- Edge case handling

### Integration Tests  
- Full workflow with test datasets
- Output table validation
- Visualization generation

### Validation Data
- Synthetic datasets with known group structure
- Real pathology data with documented styles
- Edge cases (high agreement, outliers, etc.)

## Performance Benchmarks

Typical performance on standard hardware:

| Raters | Cases | Distance Calc | Clustering | Total Time |
|--------|-------|---------------|------------|------------|
| 10 | 50 | <0.1s | <0.1s | <0.5s |
| 20 | 100 | <0.5s | <0.2s | <1.0s |
| 50 | 200 | <2.0s | <1.0s | <5.0s |

## Future Enhancements

### Planned Features
1. **Consensus trajectory analysis**: Track how individual raters move between groups over time
2. **Case difficulty weighting**: Weight discordant cases by known difficulty
3. **Multi-institutional analysis**: Compare diagnostic styles across institutions
4. **Longitudinal tracking**: Monitor style evolution in training programs

### Research Extensions
1. **Outcome correlation**: Link diagnostic styles to patient outcomes
2. **Cost analysis**: Evaluate economic impact of diagnostic style variation
3. **Machine learning integration**: Use style patterns to train diagnostic AI
4. **Genomic correlation**: Link styles to molecular diagnostic accuracy

## Literature Implementation Fidelity

The implementation closely follows the methodological framework from multiple clustering studies:

### Primary Reference - Usubutun et al. (2012)
**Usubutun A, Mutter GL, Saglam A, Dolgun A, Ozkan EA, Ince T, Akyol A, Bulbul HD, Calay Z, Eren F, Gumurdulu D, Haberal AN, Ilvan S, Karaveli S, Koyuncuoglu M, Muezzinoglu B, Muftuoglu KH, Ozdemir N, Ozen O, Baykara S, Pestereli E, Ulukus EC, Zekioglu O.** (2012). Reproducibility of endometrial intraepithelial neoplasia diagnosis is good, but influenced by the diagnostic style of pathologists. *Modern Pathology*, 25(6), 877-884. doi: 10.1038/modpathol.2011.220. PMID: 22301705.

Implementation features:
1. **Distance metric**: Percentage agreement as primary measure
2. **Clustering method**: Ward's linkage for compact groups
3. **Group identification**: 3 groups as empirically optimal
4. **Validation approach**: Correlation with rater characteristics
5. **Case analysis**: Identification of style-distinguishing cases

### Related IHC Clustering Methods
The methodology also incorporates techniques from complementary clustering studies:

**Sterlacci W, Fiegl M, Juskevicius D, Tzankov A.** (2020). Cluster Analysis According to Immunohistochemistry is a Robust Tool for Non-Small Cell Lung Cancer and Reveals a Distinct, Immune Signature-defined Subgroup. *Applied Immunohistochemistry & Molecular Morphology*, 28(4), 274-283. PMID: 31058655.

**Olsen SH, Thomas DG, Lucas DR.** (2006). Cluster analysis of immunohistochemical profiles in synovial sarcoma, malignant peripheral nerve sheath tumor, and Ewing sarcoma. *Modern Pathology*, 19(5), 659-668. PMID: 16528378.

**Matsuoka T, Mitomi H, Fukui N, Kanazawa H, Saito T, Hayashi T, Yao T.** (2011). Cluster analysis of claudin-1 and -4, E-cadherin, and β-catenin expression in colorectal cancers. *Journal of Surgical Oncology*, 103(7), 674-686. PMID: 21360533.

**Carvalho JC, Wasco MJ, Kunju LP, Thomas DG, Shah RB.** (2011). Cluster analysis of immunohistochemical profiles delineates CK7, vimentin, S100A1 and C-kit (CD117) as an optimal panel in the differential diagnosis of renal oncocytoma from its mimics. *Histopathology*, 58(2), 169-179. PMID: 21323945.

## Code Quality Standards

### Documentation
- Comprehensive roxygen2 documentation
- Inline comments for complex algorithms
- User-facing help text in jamovi interface

### Testing
- Unit tests for all core functions
- Integration tests with multiple datasets
- Edge case validation

### Performance
- Efficient algorithms for typical use cases
- Memory-conscious data structures
- Progress indicators for long-running analyses

### Maintainability
- Modular function design
- Clear separation of concerns
- Consistent coding style