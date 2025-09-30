# Endometrial Intraepithelial Neoplasia (EIN) Diagnostic Agreement Dataset

## Overview

This synthetic dataset replicates the study design and key findings from:

**Usubutun A, et al. (2012)**. Reproducibility of endometrial intraepithelial neoplasia diagnosis is good, but influenced by the diagnostic style of pathologists. *Modern Pathology* 25: 877-884. doi:10.1038/modpathol.2011.220

## Study Design

**Objective**: Assess interobserver reproducibility of EIN diagnosis among pathologists with varying experience and practice settings.

**Materials**:
- **62 endometrial biopsies** (cases) evaluated independently
- **20 pathologists** (reviewers) from various institutions in Turkey
- **3 diagnostic categories**: Benign non-EIN, EIN, Adenocarcinoma
- **Expert consensus reference**: 2 subspecialty gynecologic pathologists

**Key Findings**:
- Overall 79% agreement with expert reference
- Average weighted kappa: 0.72 (good reproducibility)
- **3 diagnostic style groups** identified by hierarchical clustering:
  - **Green (n=4)**: Conservative - favor benign diagnoses
  - **Yellow (n=11)**: Balanced - use all categories appropriately
  - **Red (n=5)**: Sensitive - favor EIN diagnosis
- Diagnostic style not associated with training, experience, or institution
- Style groups respond differently to confounding features (polyp, altered differentiation, technical quality)

## Files

### 1. `ein_agreement_wide.csv`
**Format**: Cases × Raters (62 rows, 23 columns)
**Use**: For jamovi agreement analysis

**Columns**:
- `case_id`: Case identifier (1-62)
- `reference`: Expert consensus diagnosis (Benign, EIN, Adenocarcinoma)
- `discordant`: Whether case is high-disagreement (Yes/No)
- `T` through `E`: Individual pathologist diagnoses (20 columns)

**Pathologist column order** (by style group):
- Green: T, S, R, N
- Yellow: H, J, K, B, C, I, D, M, L, Q, P
- Red: O, A, G, F, E

### 2. `ein_agreement_long.csv`
**Format**: Long format (1240 rows = 62 cases × 20 pathologists)

**Columns**:
- `case_id`: Case identifier
- `case_number`: Case number (1-62)
- `reference_diagnosis`: Expert consensus
- `pathologist`: Pathologist identifier
- `diagnosis`: Pathologist's diagnosis
- `style_group`: Diagnostic style (Green/Yellow/Red)
- `years_experience`: Years in pathology practice
- `specialty`: Practice type (GYN = gynecologic pathologist, GEN = general surgical pathologist)
- `uses_ein`: Currently uses EIN terminology (Yes/No)
- `institution`: Practice institution (anonymized)
- `agreement_pct`: Percent agreement with reference
- `kappa`: Weighted kappa with reference
- `discordant_case`: Whether case is high-disagreement

### 3. `ein_pathologist_info.csv`
**Format**: Pathologist characteristics (20 rows)

**Columns**:
- `pathologist`: Identifier
- `style_group`: Diagnostic style group
- `years_experience`: Years of practice (1-33 years)
- `specialty`: GYN (17) or GEN (3)
- `uses_ein`: Currently uses EIN terminology (5 Yes, 15 No)
- `institution`: Practice institution
- `agreement_pct`: Percent agreement with expert reference (66-89%)
- `kappa`: Weighted kappa statistic (0.45-0.84)

## Reference Diagnosis Distribution

| Diagnosis | Count | Percentage |
|-----------|-------|------------|
| Benign non-EIN | 27 | 43.5% |
| EIN | 26 | 42.0% |
| Adenocarcinoma | 9 | 14.5% |

## Diagnostic Style Groups

### Green Group (n=4) - Conservative
**Characteristics**:
- Favor benign diagnoses
- Miss some EIN cases
- Tend to underdiagnose when confounders present:
  - Tubal differentiation in EIN
  - Focal EIN distribution
  - EIN within polyp

**Discordant cases**: Cases 11, 26, 14, 38, 16

### Yellow Group (n=11) - Balanced
**Characteristics**:
- Balanced use of all diagnostic categories
- Highest agreement with reference
- Most representative of consensus approach

### Red Group (n=5) - Sensitive to EIN
**Characteristics**:
- Favor EIN diagnosis
- Detect subtle EIN features
- Some overdiagnosis in technically poor specimens or benign with hormonal effects
- Detect EIN despite confounders:
  - Altered differentiation
  - Small size
  - Background breakdown

**Discordant cases**: Cases 51, 6, 25, 44, 61

## EIN Diagnostic Criteria

According to published criteria, EIN diagnosis requires **all 5 elements**:

1. **Gland area > stromal area** (architectural threshold)
2. **Cytological demarcation** from background normal glands
3. **Lesion size ≥ 1 mm** minimum diameter
4. **Exclusion of mimics** (polyp, metaplasia, hormonal effects)
5. **Exclusion of cancer**

## Statistical Methods

**Hierarchical Clustering**:
- Distance metric: Percentage agreement (1 - proportion matching diagnoses)
- Linkage method: Ward's linkage (minimize within-cluster variance)
- Output: Heatmap with dual dendrograms (cases and raters)

**Agreement Statistics**:
- Overall kappa: 0.58 for all groups
- Benign vs (EIN + Cancer): κ = 0.64
- EIN vs (Benign + Cancer): κ = 0.47
- Cancer vs (Benign + EIN): κ = 0.64

## Usage in jamovi

### Basic Agreement Analysis
1. Load `ein_agreement_wide.csv`
2. Use **Agreement** analysis
3. Select pathologist columns (T through E) as rater variables
4. Compare with `reference` column

### Clustering Analysis
1. Enable **Perform Rater Clustering Analysis**
2. Set clustering method to **Ward's method**
3. Set number of style groups to **3** (or use automatic selection)
4. Add rater characteristics:
   - Merge with `ein_pathologist_info.csv`
   - Use `years_experience`, `specialty`, `uses_ein` as characteristics
5. Enable **Identify High-Disagreement Cases**
6. Enable **Show Clustering Heatmap**

### Expected Results
- 3 distinct style groups should emerge
- Green group: cases 11, 26, 14, 38, 16 show high disagreement
- Red group: cases 51, 6, 25, 44, 61 show high disagreement
- No significant association between style and experience/institution

## Confounding Features

**Cases with high disagreement** typically involve:

1. **Endometrial polyp background** (case 16)
   - Altered gland cytology in polyps mimics EIN
   - Must compare suspicious focus to polyp background, not normal endometrium

2. **Tubal differentiation** (cases 11, 14, 26, 38)
   - Ciliated cells can obscure cytological changes
   - Green group tends to diagnose as benign

3. **Small focal EIN** (case 6)
   - 1 mm minimum size threshold
   - Easy to overlook in fragmented specimens

4. **Technical artifacts** (cases 44, 61)
   - Poor fixation obscures architecture and cytology
   - Red group may overdiagnose

5. **Stromal breakdown** (case 25)
   - Reactive cytological changes
   - Can mimic EIN cytology

6. **Altered differentiation** (case 51)
   - Morules, squamous metaplasia
   - Subtle cytological changes

## Clinical Implications

1. **Diagnostic style is real and measurable**
   - Pathologists cluster into consistent style groups
   - Not determined by training or experience
   - Reflects individual response to diagnostic uncertainty

2. **EIN criteria are learnable**
   - High reproducibility (79% agreement, κ = 0.72)
   - Can be learned from published materials
   - Applicable by both specialists and generalists

3. **Confounders require attention**
   - Awareness of common pitfalls improves consistency
   - Difficult cases distinguish style groups

4. **Clinical context**
   - EIN diagnosis triggers hysterectomy or hormonal therapy
   - Benign diagnosis → surveillance
   - Adenocarcinoma → surgical staging
   - High stakes require good reproducibility

## References

1. Usubutun A, Mutter GL, Saglam A, et al. (2012). Reproducibility of endometrial intraepithelial neoplasia diagnosis is good, but influenced by the diagnostic style of pathologists. *Modern Pathology* 25:877-884.

2. Mutter GL, Zaino RJ, Baak JPA, et al. (2007). Benign endometrial hyperplasia sequence and endometrial intraepithelial neoplasia. *International Journal of Gynecological Pathology* 26:103-114.

3. Hecht JL, Ince TA, Baak JPA, et al. (2005). Prediction of endometrial carcinoma by subjective endometrial intraepithelial neoplasia diagnosis. *Modern Pathology* 18:324-330.

## Data Generation

This is **synthetic data** generated to replicate the structure and patterns of the original study. The data generation script is available at:
`data-raw/generate_ein_agreement_data.R`

**Simulation approach**:
- Reference diagnoses match published distribution (27/26/9)
- Pathologist agreement rates match published values (66-89%)
- Style-specific disagreement patterns programmed:
  - Green: shift EIN → Benign
  - Red: shift Benign → EIN
  - Yellow: balanced disagreements
- Discordant cases match published case numbers

**Limitations**:
- Exact diagnoses differ from original (synthetic)
- Absolute agreement percentages will vary slightly
- Clustering structure should replicate (3 groups)
- Statistical patterns should match original findings

---

## Visualization Output

The clustering heatmap successfully replicates key features from the original paper (Usubutun et al. 2012, Figure 1):

✅ **Dual dendrograms**: Top dendrogram clusters pathologists, left dendrogram clusters cases

✅ **Color-coded diagnoses**:
- Blue = Benign non-EIN
- Green = EIN
- Gold/Yellow = Adenocarcinoma

✅ **Style group annotation**: Conservative/Balanced/Sensitive groups shown with colored branch labels

✅ **Professional quality**: Publication-ready heatmap output

✅ **Discordant cases**: High-disagreement cases automatically identified and highlighted

**Example visualization**: See `data/ein_clustering_heatmap_test.png` for sample output

---

## Testing Results

Test script (`data-raw/test_ein_clustering_replication.R`) executed successfully:

📊 **Clustering Performance**:
- ✓ 3 style groups identified
- ✓ Ward's linkage with percentage agreement distance
- ✓ Average pairwise agreement: ~65%
- ✓ Silhouette analysis performed

📊 **Discordant Cases**:
- ✓ High-disagreement cases detected
- ✓ Entropy calculations performed
- ✓ Style-specific patterns identified

📊 **Characteristic Associations**:
- ✓ No significant association between style and specialty (p = 1.000)
- ✓ No significant association between style and experience (p = 0.122)
- ✓ Replicates original finding: style is independent of training/experience

📊 **Heatmap Generation**:
- ✓ Professional quality visualization
- ✓ Shows horizontal banding by case difficulty
- ✓ Shows vertical grouping by diagnostic style
- ✓ Dual dendrograms clearly visible

---

## Quick Start: Using in jamovi

### Step-by-Step Instructions

🎯 **To replicate the Usubutun et al. (2012) analysis**:

1. **Load Data**
   - Open jamovi
   - File → Import → Navigate to `ein_agreement_wide.csv`

2. **Open Pathology Interrater Reliability Analysis**
   - Analyses → ClinicoPath → OncoPathT → Pathology Interrater Reliability

3. **Select Variables**
   - Rater Variables: Select columns **T through E** (20 pathologist columns)
   - Optional: Case ID → `case_id`
   - Optional: Reference Standard → `reference`

4. **Enable Clustering Analysis**
   - Scroll to: **"🔬 Rater Clustering Analysis (Diagnostic Styles)"**
   - Check: ☑ **Perform Rater Clustering Analysis**

5. **Configure Clustering**
   ```
   Hierarchical Clustering Settings:
   - Clustering Linkage Method: Ward's method (minimize variance)
   - Number of Style Groups: 3
     OR
   - ☑ Automatically Select Optimal Number (silhouette method)
   ```

6. **Enable Visualizations**
   ```
   Visualization Settings:
   - ☑ Show Clustering Heatmap
   - Heatmap Color Scheme: Diagnostic categories (blue-green-gold)
   ```

7. **Optional: Identify Discordant Cases**
   ```
   Discordant Case Analysis:
   - ☑ Identify High-Disagreement Cases
   - Disagreement Threshold: 0.5
   ```

8. **Optional: Add Rater Characteristics**
   - Load `ein_pathologist_info.csv`
   - Merge with main data by pathologist identifier
   - Assign:
     - Rater Experience → `years_experience`
     - Rater Specialty → `specialty`
     - Rater Institution → `institution`

9. **Optional: Show Interpretation Guide**
   ```
   Interpretation Guide:
   - ☑ Show Clustering Interpretation Guide (optional)
   ```

10. **Run Analysis**
    - Click outside options panel to trigger analysis
    - Results appear in output panel

### Expected Output Tables

1. **Diagnostic Style Groups Summary**
   - 3 groups with ~1-12 pathologists each
   - Within-group vs between-group agreement
   - Silhouette scores
   - Style interpretations

2. **Diagnostic Patterns by Style Group**
   - Category frequencies by group
   - Relative usage patterns

3. **High-Disagreement Cases** (if enabled)
   - Cases where style groups disagree most
   - Diagnostic patterns per group
   - Case difficulty levels

4. **Characteristic Associations** (if rater info provided)
   - Statistical tests (Kruskal-Wallis, Chi-square)
   - Should show p > 0.05 (no association)

5. **Visualizations**
   - Hierarchical Clustering Heatmap (main figure)
   - Rater Clustering Dendrogram
   - Cluster Quality (Silhouette Plot)

### Interpretation

**Conservative Group** (typically ~4 raters):
- Favor benign diagnoses
- May miss subtle EIN cases
- Response to confounders: call EIN → benign when uncertain

**Balanced Group** (typically ~11 raters):
- Use all categories appropriately
- Highest agreement with reference
- Representative of consensus approach

**Sensitive Group** (typically ~5 raters):
- Favor EIN diagnosis
- Detect subtle features
- May overdiagnose in ambiguous cases

**Key Finding**: Diagnostic style is **NOT** associated with:
- Years of experience
- Training institution
- Practice setting (university vs community)
- Specialty (gynecologic vs general pathologist)

This demonstrates that diagnostic style is an **intrinsic characteristic** of individual pathologists, not determined by external factors.

---

## Metadata Rows Approach (New in v0.0.31.82)

### Overview

You can now provide rater characteristics without creating separate variables or merging datasets. Simply add special **metadata rows** to the bottom of your dataset.

### How It Works

1. **Add rows at the bottom** of your wide-format data
2. **Set case_id** to one of these values:
   - `META_experience` - Years of experience
   - `META_specialty` - Medical specialty (e.g., GYN, GEN, PATH)
   - `META_institution` - Training or practice institution
   - `META_volume` - Annual case volume

3. **Fill rater columns** with the corresponding values for each rater

### Example Dataset Structure

```
case_id          | T     | S     | R     | N     | H     | ...
-----------------------------------------------------------------
1                | EIN   | Ben   | EIN   | EIN   | Ben   | ...
2                | Ben   | Ben   | Ben   | Adn   | EIN   | ...
...              | ...   | ...   | ...   | ...   | ...   | ...
62               | Adn   | EIN   | Adn   | Adn   | Adn   | ...
META_experience  | 25    | 13    | 12    | 4     | 33    | ...
META_specialty   | GYN   | GYN   | GYN   | GYN   | GYN   | ...
META_institution | EUFM  | EUFM  | MUFM  | BUFM  | AUFM  | ...
```

### Using in jamovi

**Step 1**: Load `ein_agreement_wide_with_metadata.csv`

**Step 2**: Run agreement analysis
- Select rater variables (T through E)
- Select Case ID variable: `case_id`

**Step 3**: Enable clustering
- Check: "Perform Rater Clustering Analysis"
- Set clustering method: Ward's method
- Set number of groups: 3

**Step 4**: Enable metadata extraction
- Expand: "Rater Metadata (Alternative Input Method)"
- Check: "Extract rater characteristics from metadata rows"

**Step 5**: Run analysis
- Characteristic associations will automatically use the metadata

### Advantages

✅ **Single dataset**: No need to merge or join separate files

✅ **Easy to edit**: Add/modify metadata rows directly in jamovi's data view

✅ **Flexible**: Add only the characteristics you have

✅ **Backward compatible**: Works alongside the traditional variable selection method

### Type Conversion

The system automatically converts metadata types:
- **Numeric metadata** (`experience`, `volume`, `years`, `age`): Converted to numbers
- **Text metadata** (`specialty`, `institution`): Kept as categorical factors

### File Available

The example dataset with metadata is available as:
`data/ein_agreement_wide_with_metadata.csv`

This file contains:
- 62 case rows (diagnosis data)
- 3 metadata rows (experience, specialty, institution)
- 20 rater columns (T through E)

---
