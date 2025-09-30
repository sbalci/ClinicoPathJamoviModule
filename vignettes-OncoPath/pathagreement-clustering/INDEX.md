# Agreement Clustering Analysis - Documentation Index

## 📚 Overview

This folder contains comprehensive documentation for the **Hierarchical Clustering Analysis** feature in the Pathology Agreement function, which identifies diagnostic style groups among raters.

**Based on**: Usubutun A, et al. (2012). *Modern Pathology* 25:877-884

---

## 📖 Documentation Files

### For Users

1. **[QUICK_START_GUIDE.md](QUICK_START_GUIDE.md)** ⭐ **START HERE**
   - 5-step quick start for jamovi
   - Usage examples
   - Interpretation guide
   - Troubleshooting tips
   - Clinical applications
   - **Audience**: All users (beginners to advanced)
   - **Length**: 7.5 KB

2. **[EIN_AGREEMENT_README.md](EIN_AGREEMENT_README.md)**
   - Synthetic dataset documentation
   - Study design details
   - Data format specifications
   - Usage in jamovi (step-by-step)
   - Expected results
   - **Audience**: Users analyzing agreement data
   - **Length**: 13 KB

3. **[USUBUTUN_PLOT_ANALYSIS.md](USUBUTUN_PLOT_ANALYSIS.md)**
   - Detailed analysis of original study Figure 1
   - Visual component breakdown
   - Interpretation guide
   - Replication specifications
   - **Audience**: Users wanting to understand the heatmap
   - **Length**: 10 KB

### For Developers

4. **[IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md)**
   - Complete implementation overview
   - Feature checklist
   - Code organization
   - Testing results
   - Technical specifications
   - **Audience**: Developers and maintainers
   - **Length**: 16 KB

5. **[AGREEMENT_CLUSTERING_SPECIFICATION.md](AGREEMENT_CLUSTERING_SPECIFICATION.md)**
   - Algorithm specifications
   - Mathematical formulas
   - Pseudocode
   - Statistical methods
   - Implementation details
   - **Audience**: Developers implementing similar features
   - **Length**: 20 KB

6. **[PHASE_1_IMPLEMENTATION_SUMMARY.md](PHASE_1_IMPLEMENTATION_SUMMARY.md)**
   - Phase 1 features: Jaccard distance, Complete linkage, Bonferroni
   - IHC cluster analysis features
   - Technical implementation details
   - **Audience**: Developers
   - **Length**: 12 KB

7. **[PHASE_2_IMPLEMENTATION_SUMMARY.md](PHASE_2_IMPLEMENTATION_SUMMARY.md)**
   - Phase 2 features: Reproducibility testing, Supervised clustering
   - Validation methods
   - **Audience**: Developers
   - **Length**: 16 KB

### Example Output

8. **[ein_clustering_heatmap_test.png](ein_clustering_heatmap_test.png)**
   - Example heatmap visualization
   - Shows dual dendrograms
   - Color-coded diagnoses
   - Style group annotation
   - **Audience**: Visual reference for all users

---

## 🎯 Quick Navigation

### I want to...

**...use the clustering feature in jamovi**
→ Start with [QUICK_START_GUIDE.md](QUICK_START_GUIDE.md)

**...understand my data requirements**
→ Read [EIN_AGREEMENT_README.md](EIN_AGREEMENT_README.md) (Data Structure section)

**...interpret the heatmap**
→ Read [USUBUTUN_PLOT_ANALYSIS.md](USUBUTUN_PLOT_ANALYSIS.md)

**...understand what the tables mean**
→ Read [QUICK_START_GUIDE.md](QUICK_START_GUIDE.md) (Interpreting Results section)

**...know what analyses are available**
→ Read [IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md) (Features Implemented section)

**...implement a similar feature**
→ Read [AGREEMENT_CLUSTERING_SPECIFICATION.md](AGREEMENT_CLUSTERING_SPECIFICATION.md)

**...troubleshoot an issue**
→ Read [QUICK_START_GUIDE.md](QUICK_START_GUIDE.md) (Tips and Common Issues sections)

**...cite this work**
→ All files include reference section; primary citation is Usubutun et al. (2012)

---

## 📊 What is Agreement Clustering?

### Purpose
Identify **diagnostic style groups** among raters (pathologists, observers, judges) to understand systematic patterns in how they interpret diagnostic criteria.

### Method
**Hierarchical clustering** using:
- **Distance metric**: 1 - percentage agreement
- **Linkage method**: Ward's linkage (minimize within-cluster variance)
- **Optimal k selection**: Silhouette method

### Output
1. **Style Group Summary**: Within vs between-group agreement
2. **Diagnostic Patterns**: Category usage by group
3. **Discordant Cases**: Cases that distinguish groups
4. **Characteristic Associations**: Tests for style predictors
5. **Heatmap**: Dual dendrograms with color-coded diagnoses

### Clinical Insight
Reveals whether raters cluster into distinct "styles" (conservative, balanced, sensitive) and whether these styles relate to training, experience, or other characteristics.

---

## 🔬 Key Findings

From the original Usubutun et al. (2012) study:

1. **Diagnostic styles exist**: Pathologists cluster into 3 distinct groups
2. **Styles are stable**: Consistent patterns, not random disagreement
3. **Styles are intrinsic**: NOT determined by training, experience, or institution
4. **Confounders drive disagreement**: Specific case features (polyp, differentiation, technical quality) polarize groups
5. **Reproducibility is good**: Overall kappa ~ 0.6-0.7 despite style differences

---

## 📦 Dataset Files

The documentation references synthetic datasets available in `/data/`:

- `ein_agreement_wide.csv` - 62 cases × 20 pathologists (for jamovi)
- `ein_agreement_wide_with_metadata.csv` - **NEW:** Wide format with metadata rows for rater characteristics
- `ein_agreement_long.csv` - Long format with 1,240 rows
- `ein_pathologist_info.csv` - Rater characteristics

Data generation scripts are in `/data-raw/`:
- `generate_ein_agreement_data.R` - Creates synthetic data
- `test_ein_clustering_replication.R` - Verification script

---

## 🛠️ Technical Stack

**Backend**: R6 class system
- 15 private functions
- 830 lines of new code
- Full integration with existing agreement analysis

**Frontend**: jamovi YAML configuration
- 14 new options
- 10 new result tables/plots
- Organized CollapseBox UI

**Visualization**: pheatmap / ggplot2
- Dual dendrograms
- Color-coded heatmaps
- Publication quality

**Statistics**: Base R / cluster package
- Hierarchical clustering
- Silhouette analysis
- Kruskal-Wallis, Chi-square, Fisher's exact
- Cohen's kappa

---

## 📄 Citation

If you use this feature in research, please cite:

**Primary Method**:
Usubutun A, Mutter GL, Saglam A, et al. (2012). Reproducibility of endometrial intraepithelial neoplasia diagnosis is good, but influenced by the diagnostic style of pathologists. *Modern Pathology* 25:877-884. doi:10.1038/modpathol.2011.220

**Clustering Method**:
Ward JH Jr. (1963). Hierarchical grouping to optimize an objective function. *Journal of the American Statistical Association* 58:236-244.

**Quality Metric**:
Rousseeuw PJ. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. *Journal of Computational and Applied Mathematics* 20:53-65.

**Software**:
ClinicoPath jamovi module, Pathology Agreement function with Hierarchical Clustering Analysis (Version 0.0.31.81)

---

## 🆘 Support

**Questions about usage?**
→ Check [QUICK_START_GUIDE.md](QUICK_START_GUIDE.md) FAQ section

**Questions about implementation?**
→ Check [IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md) technical details

**Questions about the algorithm?**
→ Check [AGREEMENT_CLUSTERING_SPECIFICATION.md](AGREEMENT_CLUSTERING_SPECIFICATION.md) specifications

**Questions about the dataset?**
→ Check [EIN_AGREEMENT_README.md](EIN_AGREEMENT_README.md) data structure

**Bug reports or feature requests?**
→ Open an issue on GitHub: https://github.com/sbalci/ClinicoPathJamoviModule

---

## ✅ Quick Checklist

Before using the feature:
- [ ] Data is in wide format (cases × raters)
- [ ] At least 3 raters
- [ ] At least 20 cases
- [ ] Consistent category labels
- [ ] Missing data <50% per case

Analysis settings:
- [ ] Clustering method: Ward's recommended
- [ ] Number of groups: 3 or automatic
- [ ] Heatmap enabled
- [ ] Discordant cases enabled (optional)
- [ ] Rater characteristics loaded (optional)

Interpretation:
- [ ] Style groups identified
- [ ] Silhouette scores >0.5 (good separation)
- [ ] Within vs between agreement examined
- [ ] Discordant cases reviewed
- [ ] Clinical implications considered

---

## 📅 Last Updated

**Date**: September 30, 2025
**Version**: 0.0.31.81
**Status**: ✅ Production ready

All documentation is current and reflects the implemented features in the ClinicoPath jamovi module.
