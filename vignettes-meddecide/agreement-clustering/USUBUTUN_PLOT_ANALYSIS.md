# Detailed Analysis: Usubutun et al. (2012) Figure 1 Heatmap

## Visual Structure

### **Overall Layout**
- **Main component**: Hierarchical clustering heatmap with dual dendrograms
- **Dimensions**: 62 cases (rows) × 20 pathologists (columns)
- **Orientation**: Portrait, with dendrograms on top (pathologists) and left (cases)

---

## Component 1: Top Dendrogram (Pathologist Clustering)

### **Structure**
- **Type**: Hierarchical dendrogram
- **Data**: 20 pathologists
- **Clustering method**: Ward's linkage
- **Distance metric**: Percentage matching (1 - proportion agreement)

### **Visual Features**
- **Height**: Proportional to dissimilarity
- **Branch colors** (terminal branches only):
  - **Green**: 4 pathologists (leftmost)
  - **Yellow**: 11 pathologists (middle, majority)
  - **Red**: 5 pathologists (rightmost)
- **Branch structure**: Clear 3-cluster separation visible at cut height

### **Key Observations**
- Three major clusters are well-separated
- Green cluster is most distinct (furthest left branching)
- Red cluster shows tight internal agreement
- Yellow cluster (majority) has moderate internal variability

### **Pathologist Order** (left to right as labeled):
```
GREEN   | YELLOW                                | RED
T S R N | H J D K B I C G O P Q                | M E F A L
```

---

## Component 2: Left Dendrogram (Case Clustering)

### **Structure**
- **Type**: Hierarchical dendrogram
- **Data**: 62 cases
- **Clustering method**: Ward's linkage (same as pathologists)
- **Distance metric**: Similarity of diagnostic patterns across 20 pathologists

### **Visual Features**
- **Width**: Proportional to dissimilarity
- **Groups cases by**: How similarly they were diagnosed across all raters
- **No color coding**: Standard black dendrogram

### **Key Observations**
- Cases at top (28-36): Mostly cancer/EIN - high agreement (mostly gold/green)
- Cases in middle: Mixed EIN/Benign - moderate disagreement
- Cases at bottom (1-10, 41-62): Mostly benign - high agreement (mostly blue)
- **Discordant cases cluster together**: Cases with similar disagreement patterns group

---

## Component 3: Heatmap Matrix

### **Dimensions**
- **Rows**: 62 cases (vertical axis)
- **Columns**: 20 pathologists (horizontal axis)
- **Each cell**: One diagnosis by one pathologist for one case

### **Color Scheme**
| Color | Diagnosis | RGB Approximate | Clinical Meaning |
|-------|-----------|-----------------|------------------|
| **Gold/Yellow** | Cancer (Adenocarcinoma) | #FFC107 / #FFD700 | Malignant - requires surgical staging |
| **Green** | EIN | #4CAF50 / #66BB6A | Premalignant - requires treatment (hysterectomy/hormones) |
| **Blue** | Benign non-EIN | #2196F3 / #1976D2 | Benign - surveillance only |
| **White/Light gray** | No Data | #FFFFFF / #F5F5F5 | Inadequate specimen (rare) |

### **Patterns Visible**
1. **Horizontal bands** (by case):
   - **Top rows (28-36)**: Predominantly gold/green = cancer/EIN cases with high agreement
   - **Middle rows**: Mixed colors = cases with diagnostic disagreement
   - **Bottom rows**: Predominantly blue = benign cases with high agreement

2. **Vertical bands** (by pathologist):
   - **Green group (T,S,R,N)**: More blue than average = conservative style
   - **Yellow group**: Balanced mix of colors = representative of consensus
   - **Red group (M,E,F,A,L)**: More green than average = sensitive to EIN

3. **"Hotspots" of disagreement**:
   - Regions where adjacent pathologists gave different diagnoses
   - Visible as color changes within a row
   - These are the discordant cases

---

## Component 4: Case Labels (Left Side)

### **All Cases** (top to bottom):
```
28, 29, 30, 20, 34, 36, 33, 35, 39, 40, 32, 31, 34, 24, 08,
Case 16 ←
Case 38 ←
09, 18, 37,
Case 14 ←
Case 26 ←
Case 11 ←
21, 07, 27, 50, 22, 60, 05, 04, 23, 57, 56, 58, 53, 59, 17,
10, 54, 46, 41, 47, 55, 62, 52, 49, 03, 02, 48, 12, 45, 43,
42, 13, 01
```

### **Highlighted Cases** (marked with ←):
**Left arrows** (Green group distinguishers):
- Case 16
- Case 38
- Case 14
- Case 26
- Case 11

**Right arrows** (Red group distinguishers):
- Case 61
- Case 44
- Case 6
- Case 25
- Case 51

### **Purpose of Labels**:
- Identify specific cases for follow-up analysis
- Highlight cases that distinguish style groups
- Enable readers to locate discordant cases in the matrix

---

## Component 5: Pathologist Labels (Bottom)

### **Label Structure**:
```
Pathologist:  T S R N H J D K B I C G O P Q M E F A L
Style Group:  [GREEN] [-------- YELLOW --------] [RED]
```

### **Visual Formatting**:
- Single-letter pathologist codes
- Grouped by style with bracket notation
- Style group labels: GREEN, YELLOW, RED
- Color-coded brackets matching dendrogram branch colors

---

## Component 6: Legend (Bottom Right)

### **Diagnosis Key**:
```
■ Cancer (Gold/Yellow square)
■ EIN (Green square)
■ Benign, non-EIN (Blue square)
□ No Data (White/outline square)
```

### **Legend Position**: Bottom right corner
### **Font**: Bold for color labels, regular for text

---

## Component 7: Arrow Annotations

### **Left Arrows** (pointing to rows):
- **Cases 11, 26, 14, 38, 16**
- **Pattern**: Green group (T,S,R,N) diagnoses these as **Benign**
- **Others**: Majority diagnose as **EIN**
- **Clinical feature**: Often involve tubal differentiation, polyps, focal EIN

### **Right Arrows** (pointing to rows):
- **Cases 51, 6, 25, 44, 61**
- **Pattern**: Red group (M,E,F,A,L) diagnoses these as **EIN**
- **Others**: Majority diagnose as **Benign**
- **Clinical feature**: Often involve small size, technical artifacts, altered differentiation

---

## Technical Specifications for Replication

### **R Implementation Requirements**

1. **Data Structure**:
```r
# Matrix: 62 rows (cases) × 20 columns (pathologists)
# Values: "Benign", "EIN", "Adenocarcinoma"
# Column names: Pathologist IDs
# Row names: Case numbers
```

2. **Clustering**:
```r
# Distance matrix calculation
distance_matrix <- 1 - agreement_matrix

# Hierarchical clustering
hclust_pathologists <- hclust(dist(t(diagnosis_matrix)), method = "ward.D2")
hclust_cases <- hclust(dist(diagnosis_matrix), method = "ward.D2")

# Cut tree to get 3 groups
style_groups <- cutree(hclust_pathologists, k = 3)
```

3. **Color Mapping**:
```r
diagnosis_colors <- c(
  "Benign" = "#1976D2",          # Blue
  "EIN" = "#4CAF50",              # Green
  "Adenocarcinoma" = "#FFC107"    # Gold
)
```

4. **Heatmap Generation** (using pheatmap or ComplexHeatmap):
```r
library(pheatmap)

# Convert diagnoses to numeric for coloring
diagnosis_numeric <- matrix(
  as.numeric(factor(diagnosis_matrix,
    levels = c("Benign", "EIN", "Adenocarcinoma"))),
  nrow = nrow(diagnosis_matrix)
)

# Create color palette
color_breaks <- c(0.5, 1.5, 2.5, 3.5)
colors <- c("#1976D2", "#4CAF50", "#FFC107")

# Generate heatmap
pheatmap(
  diagnosis_numeric,
  cluster_rows = hclust_cases,
  cluster_cols = hclust_pathologists,
  color = colorRampPalette(colors)(100),
  breaks = seq(0.5, 3.5, length.out = 101),
  cutree_cols = 3,
  cutree_row = FALSE,
  annotation_col = data.frame(
    Style = factor(style_groups, labels = c("Green", "Yellow", "Red"))
  ),
  annotation_colors = list(
    Style = c("Green" = "#66BB6A", "Yellow" = "#FFEB3B", "Red" = "#EF5350")
  ),
  main = "Diagnostic style groups of 20 independent reviewers",
  fontsize = 8,
  angle_col = 0
)
```

---

## Key Features Our Implementation Must Replicate

### ✅ **Already Implemented**:
1. Percentage agreement distance metric
2. Ward's linkage clustering
3. Automatic k selection (silhouette method)
4. Manual k specification
5. Cluster quality metrics (silhouette scores)
6. Discordant case identification
7. Style group summary tables
8. Rater characteristic associations

### 🎨 **Visualization Requirements**:
1. **Dual dendrograms**: Top (pathologists) + Left (cases)
2. **Color-coded heatmap**: Blue-Green-Gold scheme
3. **Style group annotation**: Colored branch labels
4. **Case highlighting**: Arrows/labels for discordant cases
5. **Legend**: Clear diagnosis key
6. **Professional formatting**: Publication-quality output

### 📊 **Statistical Output**:
1. Style group membership table
2. Within-group vs between-group agreement
3. Discordant case list with patterns
4. Characteristic associations (non-significant in original)

---

## Expected Results with Synthetic Data

When we analyze the synthetic EIN dataset with our clustering implementation:

1. **3 style groups** should emerge (Green, Yellow, Red)
2. **Green group**: 4 pathologists, conservative pattern
3. **Yellow group**: ~11 pathologists, balanced pattern
4. **Red group**: 5 pathologists, EIN-sensitive pattern
5. **Discordant cases**: 10 cases identified (5 per style extreme)
6. **No association**: Style group independent of experience/institution
7. **Heatmap pattern**: Should show horizontal banding (case difficulty) and vertical grouping (style)

---

## Clinical Interpretation Guide

### **What the Plot Reveals**:

1. **Diagnostic styles exist**: Pathologists cluster into consistent behavioral patterns
2. **Styles are stable**: Not random disagreement but systematic differences
3. **Confounders drive disagreement**: Specific case features (polyp, differentiation, quality) polarize groups
4. **Training doesn't eliminate style**: Experience/institution don't predict group membership

### **Implications**:
- **Quality assurance**: Awareness of personal diagnostic style
- **Education focus**: Emphasize response to confounders, not just criteria
- **Panel review**: Mix of styles may improve accuracy
- **Research design**: Account for style in reproducibility studies

---

## Software Implementation

Our `agreement` function clustering features should produce this exact analysis via:

```
jamovi → Analyses → ClinicoPath → Agreement
  ├─ Variables: Select 20 pathologist columns
  ├─ Clustering Analysis
  │   ├─ ☑ Perform Rater Clustering Analysis
  │   ├─ Method: Ward's method
  │   ├─ Number of groups: 3 (or automatic)
  │   ├─ ☑ Identify High-Disagreement Cases
  │   ├─ ☑ Show Clustering Heatmap
  │   └─ Color scheme: Diagnostic categories
  └─ Rater Characteristics (optional)
      ├─ Experience: years_experience
      ├─ Specialty: specialty
      └─ Institution: institution
```

**Output**:
1. Style Groups Summary table
2. Diagnostic Patterns by Style Group table
3. High-Disagreement Cases table
4. Characteristic Associations table (p > 0.05 expected)
5. **Hierarchical Clustering Heatmap** (Figure 1 replica)
6. Dendrogram plot
7. Silhouette plot
