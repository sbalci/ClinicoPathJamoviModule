# Agreement Clustering Feature Specification

**ClinicoPath jamovi Module - agreement function**
**Implementation Date:** 2025-09-30
**Reference:** Usubutun et al. (2012). *Modern Pathology*, 25, 877-884.

---

## Executive Summary

This specification defines the implementation of hierarchical clustering for rater agreement analysis, enabling identification of "diagnostic style groups" among raters and characterization of difficult/discordant cases.

**Key Innovation:** Move beyond simple agreement statistics (kappa) to understand **patterns** of disagreement and rater behavioral clusters.

---

## Background: Usubutun et al. (2012) Study

### Study Design:
- **Sample:** 62 endometrial biopsies
- **Raters:** 20 pathologists with varying experience
- **Categories:** 3-class diagnosis (Benign, EIN, Cancer)
- **Reference:** Expert consensus diagnosis

### Key Finding:
Pathologists demonstrated **three distinct diagnostic styles**:
- **Green style (n=4):** Conservative, under-diagnosed EIN
- **Yellow style (n=11):** Moderate, majority pattern
- **Red style (n=5):** Aligned with expert reference, sensitive to EIN

### Methodology:
1. Create diagnosis matrix: Cases × Raters
2. Hierarchical clustering using:
   - **Distance:** Percentage agreement (100% - % matching)
   - **Linkage:** Ward's method (minimize within-cluster variance)
3. Visualize as heatmap with dendrogram
4. Identify style groups from dendrogram
5. Characterize groups by rater demographics

---

## Feature Requirements

### 1. Rater Clustering

**Purpose:** Identify groups of raters with similar diagnostic patterns

**Input Data:**
- Cases × Raters matrix
- Each cell = diagnosis category (factor)
- Optional: Expert/reference diagnosis

**Algorithm:**
```r
# 1. Calculate pairwise rater agreement
agreement_matrix <- matrix(nrow = n_raters, ncol = n_raters)
for (i in 1:n_raters) {
    for (j in 1:n_raters) {
        agreement_matrix[i, j] <- mean(rater_i_diagnoses == rater_j_diagnoses)
    }
}

# 2. Convert to distance
distance_matrix <- 1 - agreement_matrix

# 3. Hierarchical clustering
hc <- hclust(as.dist(distance_matrix), method = "ward.D2")

# 4. Cut tree to identify style groups
style_groups <- cutree(hc, k = n_groups)
```

**Output:**
- Dendrogram showing rater relationships
- Style group assignments for each rater
- Within-group homogeneity metrics
- Between-group differences

---

### 2. Case Clustering

**Purpose:** Identify difficult/discordant cases that distinguish rater styles

**Algorithm:**
```r
# 1. Calculate case disagreement score
for (case in 1:n_cases) {
    # Entropy-based disagreement
    diagnosis_proportions <- table(diagnoses_for_case) / n_raters
    entropy <- -sum(diagnosis_proportions * log(diagnosis_proportions))

    # OR simple: proportion of majority diagnosis
    disagreement <- 1 - max(diagnosis_proportions)
}

# 2. Hierarchical clustering of cases
# Based on which raters agreed/disagreed on each case
case_distance <- dist(t(diagnosis_matrix), method = "binary")
case_hc <- hclust(case_distance, method = "ward.D2")
```

**Output:**
- Dendrogram showing case relationships
- Disagreement score for each case
- List of "discordant cases" (high disagreement)
- Cases that distinguish style groups

---

### 3. Heatmap Visualization

**Purpose:** Visual representation of diagnosis patterns

**Design:**
```
         Rater 1  Rater 2  ...  Rater 20
         |--------|--------| ... |--------|
         | Green  | Yellow |     | Red    |  <- Style groups (dendrogram)
         |--------|--------| ... |--------|
Case 1   |  Blue  |  Blue  |     |  Blue  |  <- Benign
Case 2   |  Green |  Green |     | Green  |  <- EIN
...
Case 38  |  Blue  | Green  |     | Green  |  <- Discordant!
Case 62  |  Gold  |  Gold  |     |  Gold  |  <- Cancer
```

**Features:**
- Color legend: Categories mapped to colors
- Row dendrogram: Case clustering
- Column dendrogram: Rater clustering
- Highlight discordant cases (arrows/markers)
- Optional: Add expert reference as pseudo-rater column

**Implementation:**
- Use `ComplexHeatmap` package
- Or custom `ggplot2` + `dendextend`

---

### 4. Diagnostic Style Group Analysis

**Purpose:** Characterize each style group's diagnostic tendencies

**Metrics per Style Group:**

1. **Overall Agreement:**
   - Mean kappa with reference
   - Mean pairwise agreement within group
   - % exact concordance with reference

2. **Diagnostic Distribution:**
   ```
   Group    % Benign    % EIN    % Cancer
   Green       45%       40%       15%      <- Conservative
   Yellow      44%       43%       13%      <- Moderate
   Red         40%       47%       13%      <- Sensitive to EIN
   ```

3. **Sensitivity/Specificity:**
   - If reference diagnosis available
   - Calculate for each diagnostic category
   - Compare across groups

4. **Discordant Case Profile:**
   - List cases where this group differs from others
   - Characterize features (e.g., polyp, poor prep, borderline)

**Output Table:**
```
Diagnostic Style Group Characterization

Group   N Raters   Mean κ   Sens (EIN)   Spec (EIN)   Tendency
Green      4        0.65       0.72          0.88      Conservative
Yellow    11        0.72       0.81          0.85      Moderate
Red        5        0.78       0.89          0.82      Sensitive
```

---

### 5. Rater Characteristic Associations

**Purpose:** Test if style groups associate with rater demographics

**Rater Metadata (Optional Variables):**
- Years of experience (continuous)
- Practice type (categorical: specialist/generalist)
- Training institution (categorical)
- Current institution (categorical)
- Diagnostic system used (categorical: EIN/WHO/other)
- Case volume (continuous: cases/month)

**Statistical Tests:**

1. **Categorical Characteristics:**
   ```r
   # Chi-square or Fisher's exact
   chisq.test(table(style_group, practice_type))
   fisher.test(table(style_group, diagnostic_system))
   ```

2. **Continuous Characteristics:**
   ```r
   # Kruskal-Wallis
   kruskal.test(years_experience ~ style_group)

   # Spearman correlation with kappa
   cor.test(years_experience, kappa_with_reference, method = "spearman")
   ```

**Output Table:**
```
Association Between Style Groups and Rater Characteristics

Characteristic        Test              Statistic    P-value    Conclusion
Years of experience   Kruskal-Wallis    χ²=1.24     0.435      No association
Practice type         Fisher's exact    —           0.228      No association
Training institution  Chi-square        χ²=8.45     0.236      No association
Diagnostic system     Fisher's exact    —           0.376      No association
```

**Key Finding from Paper:**
Style group membership was **NOT** associated with experience, practice type, or institution. This suggests diagnostic style is **personal** rather than learned/institutional.

---

## Implementation Specifications

### A. New Options in `agreement.a.yaml`

```yaml
# Clustering Options
- name: performClustering
  title: 'Perform Rater Clustering Analysis'
  type: Bool
  default: false
  description: 'Identify diagnostic style groups using hierarchical clustering (Usubutun 2012)'

- name: clusteringMethod
  title: 'Clustering Linkage Method'
  type: List
  options:
      - title: "Ward's method (minimize variance)"
        name: ward
      - title: 'Complete linkage'
        name: complete
      - title: 'Average linkage'
        name: average
  default: ward
  description: 'Hierarchical clustering linkage method'

- name: nStyleGroups
  title: 'Number of Style Groups'
  type: Integer
  min: 2
  max: 10
  default: 3
  description: 'Number of diagnostic style groups to identify'

- name: autoSelectGroups
  title: 'Automatically Select Number of Groups'
  type: Bool
  default: false
  description: 'Use elbow method or silhouette to determine optimal k'

- name: referenceRater
  title: 'Reference/Expert Rater (optional)'
  type: Variable
  suggested:
      - nominal
      - ordinal
  permitted:
      - factor
  description: 'Expert consensus or reference standard diagnosis for comparison'
  default: NULL

# Rater Characteristics (optional)
- name: raterExperience
  title: 'Rater Experience (years)'
  type: Variable
  suggested:
      - continuous
  permitted:
      - numeric
  description: 'Years of experience for each rater (continuous)'
  default: NULL

- name: raterSpecialty
  title: 'Rater Specialty/Practice Type'
  type: Variable
  suggested:
      - nominal
  permitted:
      - factor
  description: 'Specialist vs generalist, or subspecialty'
  default: NULL

- name: raterInstitution
  title: 'Rater Institution'
  type: Variable
  suggested:
      - nominal
  permitted:
      - factor
  description: 'Training or current practice institution'
  default: NULL

- name: raterVolume
  title: 'Rater Case Volume'
  type: Variable
  suggested:
      - continuous
  permitted:
      - numeric
  description: 'Number of cases seen per month'
  default: NULL

# Visualization
- name: showHeatmap
  title: 'Show Clustering Heatmap'
  type: Bool
  default: true
  description: 'Display Cases × Raters heatmap with dendrograms'

- name: heatmapColors
  title: 'Heatmap Color Palette'
  type: List
  options:
      - name: default
        title: 'Default (blue-green-gold)'
      - name: viridis
        title: 'Viridis (colorblind-safe)'
      - name: custom
        title: 'Custom per category'
  default: default

- name: identifyDiscordant
  title: 'Identify Discordant Cases'
  type: Bool
  default: true
  description: 'Highlight cases with high disagreement'

- name: discordantThreshold
  title: 'Discordant Case Threshold'
  type: Number
  min: 0.2
  max: 0.8
  default: 0.5
  description: 'Disagreement proportion threshold for flagging cases'
```

---

### B. New Result Tables in `agreement.r.yaml`

```yaml
# Style Group Summary
- name: styleGroupSummary
  title: Diagnostic Style Groups
  type: Table
  visible: (performClustering)
  columns:
      - name: style_group
        title: 'Style Group'
        type: text
      - name: n_raters
        title: 'N Raters'
        type: integer
      - name: mean_kappa
        title: 'Mean κ (vs Reference)'
        type: number
        format: zto
      - name: within_agreement
        title: 'Within-Group Agreement'
        type: number
        format: pc
      - name: diagnostic_tendency
        title: 'Diagnostic Tendency'
        type: text

# Style Group Profiles
- name: styleGroupProfiles
  title: Diagnostic Distribution by Style Group
  type: Table
  visible: (performClustering)
  columns:
      - name: style_group
        title: 'Style Group'
        type: text
      - name: category
        title: 'Diagnosis Category'
        type: text
      - name: proportion
        title: 'Proportion'
        type: number
        format: pc
      - name: vs_reference
        title: 'vs Reference'
        type: text

# Discordant Cases
- name: discordantCases
  title: Discordant Cases
  type: Table
  visible: (performClustering && identifyDiscordant)
  columns:
      - name: case_id
        title: 'Case'
        type: text
      - name: disagreement
        title: 'Disagreement Score'
        type: number
        format: zto
      - name: entropy
        title: 'Entropy'
        type: number
        format: zto
      - name: majority_diagnosis
        title: 'Majority Diagnosis'
        type: text
      - name: minority_diagnoses
        title: 'Minority Diagnoses'
        type: text
      - name: distinguishes
        title: 'Distinguishes Groups'
        type: text

# Rater Characteristic Associations
- name: characteristicAssociations
  title: Style Group Associations with Rater Characteristics
  type: Table
  visible: (performClustering)
  columns:
      - name: characteristic
        title: 'Characteristic'
        type: text
      - name: test
        title: 'Test'
        type: text
      - name: statistic
        title: 'Statistic'
        type: number
        format: zto
      - name: p_value
        title: 'P-value'
        type: number
        format: zto,pvalue
      - name: conclusion
        title: 'Conclusion'
        type: text

# Clustering Heatmap
- name: clusteringHeatmap
  title: Rater Clustering Heatmap
  type: Image
  visible: (performClustering && showHeatmap)
  description: 'Cases × Raters heatmap with hierarchical dendrograms'
  width: 800
  height: 600
  renderFun: '.plotClusteringHeatmap'
```

---

### C. Backend Functions in `agreement.b.R`

**Helper Functions:**

```r
# 1. Calculate rater-rater agreement matrix
.calculateRaterAgreement = function(diagnosis_matrix) {
    n_raters <- ncol(diagnosis_matrix)
    agreement <- matrix(nrow = n_raters, ncol = n_raters)

    for (i in 1:n_raters) {
        for (j in 1:n_raters) {
            agreement[i,j] <- mean(diagnosis_matrix[,i] == diagnosis_matrix[,j], na.rm = TRUE)
        }
    }

    return(agreement)
}

# 2. Hierarchical clustering of raters
.clusterRaters = function(agreement_matrix, method = "ward.D2") {
    distance_matrix <- 1 - agreement_matrix
    hc <- hclust(as.dist(distance_matrix), method = method)
    return(hc)
}

# 3. Identify style groups
.identifyStyleGroups = function(hc, k) {
    style_groups <- cutree(hc, k = k)
    return(style_groups)
}

# 4. Calculate case disagreement
.calculateCaseDisagreement = function(diagnosis_matrix) {
    n_cases <- nrow(diagnosis_matrix)
    disagreement <- numeric(n_cases)
    entropy <- numeric(n_cases)

    for (i in 1:n_cases) {
        diagnoses <- diagnosis_matrix[i,]
        diagnoses <- diagnoses[!is.na(diagnoses)]

        # Proportion agreeing with majority
        props <- table(diagnoses) / length(diagnoses)
        disagreement[i] <- 1 - max(props)

        # Entropy
        props_nonzero <- props[props > 0]
        entropy[i] <- -sum(props_nonzero * log(props_nonzero))
    }

    return(list(disagreement = disagreement, entropy = entropy))
}

# 5. Characterize style groups
.characterizeStyleGroups = function(diagnosis_matrix, style_groups, reference = NULL) {
    unique_groups <- unique(style_groups)
    results <- list()

    for (grp in unique_groups) {
        raters_in_group <- which(style_groups == grp)
        group_diagnoses <- diagnosis_matrix[, raters_in_group, drop = FALSE]

        # Diagnostic distribution
        all_diagnoses <- as.vector(group_diagnoses)
        all_diagnoses <- all_diagnoses[!is.na(all_diagnoses)]
        diag_dist <- table(all_diagnoses) / length(all_diagnoses)

        # Agreement with reference
        if (!is.null(reference)) {
            kappas <- sapply(raters_in_group, function(r) {
                irr::kappa2(cbind(reference, diagnosis_matrix[,r]))$value
            })
            mean_kappa <- mean(kappas, na.rm = TRUE)
        } else {
            mean_kappa <- NA
        }

        # Within-group agreement
        if (length(raters_in_group) > 1) {
            group_agreement <- .calculateRaterAgreement(group_diagnoses)
            within_agreement <- mean(group_agreement[upper.tri(group_agreement)])
        } else {
            within_agreement <- NA
        }

        results[[as.character(grp)]] <- list(
            style_group = grp,
            n_raters = length(raters_in_group),
            mean_kappa = mean_kappa,
            within_agreement = within_agreement,
            diagnostic_distribution = diag_dist
        )
    }

    return(results)
}

# 6. Test associations with rater characteristics
.testCharacteristicAssociations = function(style_groups, characteristics) {
    results <- list()

    for (char_name in names(characteristics)) {
        char_data <- characteristics[[char_name]]

        if (is.numeric(char_data)) {
            # Kruskal-Wallis test
            test_result <- kruskal.test(char_data ~ style_groups)
            results[[char_name]] <- list(
                characteristic = char_name,
                test = "Kruskal-Wallis",
                statistic = test_result$statistic,
                p_value = test_result$p.value
            )
        } else {
            # Fisher's exact or Chi-square
            contingency <- table(style_groups, char_data)
            if (min(contingency) < 5) {
                test_result <- fisher.test(contingency, simulate.p.value = TRUE)
                test_name <- "Fisher's exact"
            } else {
                test_result <- chisq.test(contingency)
                test_name <- "Chi-square"
            }
            results[[char_name]] <- list(
                characteristic = char_name,
                test = test_name,
                statistic = test_result$statistic,
                p_value = test_result$p.value
            )
        }
    }

    return(results)
}

# 7. Plot clustering heatmap
.plotClusteringHeatmap = function(diagnosis_matrix, rater_hc, case_hc,
                                   style_groups, discordant_cases = NULL) {
    # Use ComplexHeatmap or custom ggplot2
    # Color map diagnoses
    # Add dendrograms
    # Mark discordant cases
    # Annotate style groups
}
```

---

## Usage Examples

### Example 1: Basic Clustering

```r
# 20 pathologists × 62 endometrial biopsies
# 3 diagnostic categories: Benign, EIN, Cancer

agreement(
    data = ein_data,
    cases = case_id,
    raters = pathologist,
    ratings = diagnosis,

    # Enable clustering
    performClustering = TRUE,
    nStyleGroups = 3,
    clusteringMethod = "ward",

    # Reference
    referenceRater = expert_consensus,

    # Visualization
    showHeatmap = TRUE,
    identifyDiscordant = TRUE
)
```

**Output:**
- Style Group Summary: 3 groups identified
- Heatmap showing diagnostic patterns
- List of discordant cases
- Style group characterization

---

### Example 2: With Rater Characteristics

```r
agreement(
    data = ein_data,
    cases = case_id,
    raters = pathologist,
    ratings = diagnosis,

    # Clustering
    performClustering = TRUE,
    autoSelectGroups = TRUE,  # Let algorithm choose k

    # Rater metadata
    raterExperience = years_experience,
    raterSpecialty = practice_type,
    raterInstitution = institution,
    raterVolume = cases_per_month,

    # Reference
    referenceRater = expert_consensus
)
```

**Output:**
- Optimal number of style groups determined
- Test associations between groups and experience/specialty/institution
- Report: "Style group membership not associated with experience (p=0.435)"

---

## Expected Benefits

1. **Clinical Insight:**
   - Understand patterns of diagnostic disagreement
   - Identify "difficult cases" requiring discussion
   - Recognize personal diagnostic biases

2. **Quality Improvement:**
   - Characterize institutional diagnostic patterns
   - Target education to specific style groups
   - Improve inter-institutional standardization

3. **Research Applications:**
   - Analyze diagnostic reproducibility beyond kappa
   - Study factors affecting diagnostic style
   - Validate new diagnostic criteria

4. **Educational Use:**
   - Show trainees different diagnostic approaches
   - Demonstrate how experience affects style
   - Use discordant cases for teaching

---

## Implementation Timeline

**Phase 1:** Core clustering functionality
- Rater clustering
- Style group identification
- Basic heatmap

**Phase 2:** Advanced characterization
- Case disagreement analysis
- Discordant case identification
- Style group profiling

**Phase 3:** Rater characteristics
- Association testing
- Comprehensive reporting
- Enhanced visualizations

---

## References

1. Usubutun A, et al. (2012). Reproducibility of endometrial intraepithelial neoplasia diagnosis is good, but influenced by the diagnostic style of pathologists. *Modern Pathology*, 25, 877-884.

2. Ward JH. (1963). Hierarchical grouping to optimize an objective function. *J Am Stat Assoc*, 58(301), 236-244.

3. Landis JR, Koch GG. (1977). The measurement of observer agreement for categorical data. *Biometrics*, 33(1), 159-174.

---

**Document Version:** 1.0
**Date:** 2025-09-30
**Status:** Design Specification - Ready for Implementation
