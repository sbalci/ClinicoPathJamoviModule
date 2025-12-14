# ClinicoPath Tutorial Series 📚

**Welcome to the ClinicoPath Tutorial Series!** These step-by-step guides teach you how to perform sophisticated clinical and pathological analyses using ClinicoPath for jamovi.

**Target Audience:** Clinicians, pathologists, and researchers with **no programming experience required**.

---

## Tutorial Overview

| # | Tutorial | Time | Difficulty | Topics |
|---|----------|------|------------|--------|
| 1 | [Getting Started](01-getting-started.qmd) | 30 min | ⭐ Beginner | Installation, navigation, first analysis |
| 2 | [Table One for Clinical Trials](02-table-one-clinical-trial.qmd) | 45 min | ⭐⭐ Intermediate | Baseline characteristics, group comparisons, effect sizes |
| 3 | [Survival Analysis in Oncology](03-survival-analysis-cancer.qmd) | 60 min | ⭐⭐ Intermediate | Kaplan-Meier, log-rank, Cox regression |
| 4 | [ROC Analysis for Diagnostic Tests](04-roc-diagnostic-test.qmd) | 45 min | ⭐⭐ Intermediate | Sensitivity, specificity, AUC, cutpoints |
| 5 | [Decision Curve Analysis](05-decision-curve-analysis.qmd) | 50 min | ⭐⭐⭐ Advanced | Clinical utility, net benefit, threshold selection |
| 6 | [Creating Reproducible Reports](06-reproducible-reports.qmd) | 40 min | ⭐⭐ Intermediate | Quarto integration, automation, manuscripts |

**Total Time:** ~4.5 hours (can be completed over multiple sessions)

---

## Learning Path

### Path 1: Clinical Trials Researcher
```
Tutorial 1 → Tutorial 2 → Tutorial 3 → Tutorial 6
```
Master descriptive statistics, group comparisons, survival analysis, and automated reporting.

### Path 2: Diagnostic Pathologist
```
Tutorial 1 → Tutorial 4 → Tutorial 5
```
Focus on diagnostic test evaluation, ROC curves, and clinical decision-making.

### Path 3: Comprehensive Clinical Researcher
```
Tutorial 1 → 2 → 3 → 4 → 5 → 6 (complete series)
```
Master all aspects of clinicopathological research analysis.

---

## What You'll Learn

### Tutorial 1: Getting Started ⭐
**Goal:** Install ClinicoPath and run your first analysis

**Topics:**
- Installing jamovi and ClinicoPath modules
- Navigating the ClinicoPath interface
- Importing clinical data (CSV, Excel, SPSS)
- Creating a basic Table One
- Interpreting results
- Exporting publication-ready tables

**Dataset:** `clinical_trial_data` (n=200)

**Key Skills:**
✅ jamovi installation
✅ Data import
✅ Table One generation
✅ Result interpretation

---

### Tutorial 2: Table One for Clinical Trials ⭐⭐
**Goal:** Create publication-quality baseline characteristics tables with statistical comparisons

**Topics:**
- Stratified analysis by treatment group
- Automatic statistical test selection (t-test, chi-square, Fisher's exact)
- Effect sizes (Cohen's d, Cramér's V)
- Multiple testing corrections (Bonferroni, Holm, FDR)
- Missing data handling and reporting
- NEJM, Lancet, and gtsummary table styles

**Dataset:** `clinical_trial_data` with group comparisons

**Key Skills:**
✅ Stratified baseline tables
✅ Statistical tests for group comparisons
✅ Effect size calculation and interpretation
✅ Manuscript-ready Methods and Results sections

---

### Tutorial 3: Survival Analysis in Oncology ⭐⭐
**Goal:** Perform comprehensive survival analysis for cancer research

**Topics:**
- Kaplan-Meier survival curves with confidence intervals
- Log-rank, Wilcoxon, and Tarone-Ware tests
- Median survival time calculation
- 1-year, 3-year, and 5-year survival rates
- Cox proportional hazards regression (univariate and multivariate)
- Hazard ratios with forest plots
- Proportional hazards assumption testing (Schoenfeld residuals)
- C-index for model discrimination

**Dataset:** `basic_survival_data` (breast cancer, n=200)

**Key Skills:**
✅ Kaplan-Meier analysis
✅ Log-rank tests
✅ Cox regression
✅ Hazard ratio interpretation
✅ Forest plots for publication

---

### Tutorial 4: ROC Analysis for Diagnostic Tests ⭐⭐
**Goal:** Evaluate diagnostic test performance using ROC curves

**Topics:**
- Receiver Operating Characteristic (ROC) curves
- Area Under the Curve (AUC) with 95% CI
- Sensitivity and specificity at multiple cutpoints
- Positive and negative predictive values (PPV, NPV)
- Optimal cutpoint determination (Youden index, cost-benefit)
- Comparing multiple biomarkers
- Time-dependent ROC for survival outcomes
- Grey zone analysis

**Dataset:** `diagnostic_biomarker_data` (HER2 testing, n=150)

**Key Skills:**
✅ ROC curve generation
✅ AUC interpretation
✅ Cutpoint optimization
✅ Comparing diagnostic tests
✅ Clinical threshold selection

---

### Tutorial 5: Decision Curve Analysis ⭐⭐⭐
**Goal:** Assess clinical utility and net benefit of prediction models

**Topics:**
- Decision curve analysis (DCA)
- Net benefit calculation
- Clinical utility vs. statistical significance
- Threshold probability selection
- Comparing prediction models
- Interventions avoided calculation
- Time-dependent DCA for survival
- Integration with ROC analysis

**Dataset:** `risk_prediction_model_data` (breast cancer recurrence, n=300)

**Key Skills:**
✅ Decision curve generation
✅ Net benefit interpretation
✅ Clinical threshold selection
✅ Model comparison
✅ Communicating clinical utility

---

### Tutorial 6: Creating Reproducible Reports ⭐⭐
**Goal:** Automate analyses and generate reproducible manuscripts

**Topics:**
- Using ClinicoPath programmatically in R
- Quarto document integration
- Automated Table One, survival curves, ROC plots
- Batch processing multiple datasets
- Version control with Git
- Reproducible workflow best practices
- Manuscript templates for common study designs

**Dataset:** Multiple example datasets

**Key Skills:**
✅ R scripting with ClinicoPath
✅ Quarto report generation
✅ Workflow automation
✅ Reproducible research practices

---

## Prerequisites

### Software Requirements
- **jamovi** ≥ 2.5 (free download: [jamovi.org](https://www.jamovi.org))
- **ClinicoPath** modules (install via jamovi library)
- **Computer:** Windows 10+, macOS 10.14+, or Linux Ubuntu 18.04+
- **RAM:** 4 GB minimum, 8 GB recommended
- **Disk Space:** 1 GB for jamovi + ClinicoPath + example data

### Knowledge Requirements
- **Tutorial 1:** None! Complete beginners welcome.
- **Tutorials 2-6:** Completion of Tutorial 1 recommended.
- **No programming experience required** for any tutorial.

### Optional for Tutorial 6 (Reproducible Reports)
- **R** ≥ 4.1 (for programmatic use)
- **RStudio** (recommended IDE)
- **Quarto** (for report generation)

---

## Example Datasets

All tutorials use realistic clinical datasets included with ClinicoPath:

| Dataset | N | Description | Used in Tutorials |
|---------|---|-------------|-------------------|
| `clinical_trial_data` | 200 | Randomized breast cancer trial | 1, 2 |
| `basic_survival_data` | 200 | Breast cancer survival cohort | 3 |
| `diagnostic_biomarker_data` | 150 | HER2 testing validation | 4 |
| `risk_prediction_model_data` | 300 | Recurrence prediction | 5 |
| `lung_agreement_data` | 100 | Pathologist agreement study | -- |
| `her2_breast_cancer_data` | 150 | HER2-low classification | -- |

**All datasets are de-identified** and free to use for learning and teaching.

---

## Tutorial Format

Each tutorial includes:

✅ **Learning objectives** - What you'll master
✅ **Clinical scenario** - Realistic research question
✅ **Step-by-step walkthrough** - Click-by-click instructions with screenshots
✅ **Result interpretation** - How to read and understand outputs
✅ **Common mistakes** - Pitfalls to avoid
✅ **Manuscript reporting** - Methods and Results text templates
✅ **Practice exercises** - Apply skills to new datasets
✅ **Summary** - Key takeaways and next steps

---

## How to Use These Tutorials

### For Self-Study
1. Start with **Tutorial 1** (essential for all users)
2. Follow your **learning path** based on research focus
3. Complete **practice exercises** at the end of each tutorial
4. Apply skills to **your own data**

### For Classroom Teaching
- Each tutorial = one 90-minute lab session
- Students work through tutorials at their own pace
- Instructor provides guidance on practice exercises
- Final project: Analyze real dataset using learned skills

### For Research Teams
- Assign tutorials as onboarding for new team members
- Use as reference for specific analyses
- Standardize analytical approaches across team
- Share custom tutorials for lab-specific workflows

---

## Additional Resources

### Documentation
- **ClinicoPath Website:** [www.serdarbalci.com/ClinicoPathJamoviModule](https://www.serdarbalci.com/ClinicoPathJamoviModule/)
- **Development Guides:** [vignettes/](../vignettes/) (for developers)
- **Function Reference:** [man/](../man/) (R documentation)

### Community
- **GitHub Issues:** [Report bugs or request features](https://github.com/sbalci/ClinicoPathJamoviModule/issues/)
- **jamovi Forum:** [Ask questions](https://forum.jamovi.org/)
- **Email Support:** serdarbalci@serdarbalci.com

### Scientific Skills Integration
These tutorials align with the [scientific-skills framework](https://github.com/anthropics/claude-code) for:
- **peer-review:** Critical evaluation of statistical methods
- **statistical-analysis:** Rigorous hypothesis testing
- **clinical-decision-support:** Evidence-based medicine
- **scientific-writing:** Manuscript preparation

---

## Citation

If you use these tutorials in your teaching or research, please cite:

```
Balci, S. (2025). ClinicoPath Tutorial Series: Step-by-Step Guides for
Clinicopathological Research. ClinicoPath Jamovi Module.
https://www.serdarbalci.com/ClinicoPathJamoviModule/tutorials/
```

**BibTeX:**
```bibtex
@Misc{clinicopath-tutorials2025,
  title = {ClinicoPath Tutorial Series: Step-by-Step Guides for Clinicopathological Research},
  author = {Serdar Balci},
  year = {2025},
  url = {https://www.serdarbalci.com/ClinicoPathJamoviModule/tutorials/},
}
```

---

## Contributing

Have suggestions for improving tutorials or ideas for new ones?

1. **Open an issue:** [GitHub Issues](https://github.com/sbalci/ClinicoPathJamoviModule/issues/)
2. **Submit corrections:** Fork → Edit → Pull Request
3. **Share use cases:** Email examples of how you used tutorials
4. **Request topics:** What analyses do you need tutorials for?

---

## License

**Tutorials:** CC-BY-4.0 (Creative Commons Attribution 4.0 International)
**Software (ClinicoPath):** GPL-2

You are free to:
- ✅ Share tutorials with students and colleagues
- ✅ Adapt tutorials for your courses
- ✅ Translate tutorials into other languages
- ✅ Use tutorial examples in your research

**Attribution:** Please cite ClinicoPath and provide a link to these tutorials.

---

## Acknowledgments

These tutorials were developed using:
- **jamovi framework:** The jamovi project (2025)
- **Clinical examples:** Based on published research and simulated data
- **Statistical methods:** Standard clinicopathological research practices
- **Peer review:** Aligned with [scientific peer-review standards](SCIENTIFIC_REVIEW_2025.md)

**Special thanks** to the jamovi community and ClinicoPath users who provided feedback on early drafts.

---

## Tutorial Status

| Tutorial | Status | Last Updated |
|----------|--------|--------------|
| Tutorial 1: Getting Started | ✅ Complete | Dec 13, 2025 |
| Tutorial 2: Table One | ✅ Complete | Dec 13, 2025 |
| Tutorial 3: Survival Analysis | ✅ Complete | Dec 13, 2025 |
| Tutorial 4: ROC Analysis | 🚧 In Progress | -- |
| Tutorial 5: Decision Curve Analysis | 🚧 In Progress | -- |
| Tutorial 6: Reproducible Reports | 🚧 In Progress | -- |

---

**Ready to start?** Begin with [Tutorial 1: Getting Started →](01-getting-started.qmd)

**Questions?** Check the [FAQ](#faq) or [contact us](#contact).

---

## FAQ {#faq}

**Q: Do I need to complete all tutorials?**
A: No! Choose the learning path that matches your research focus. Tutorial 1 is recommended for everyone.

**Q: Can I skip Tutorial 1 if I already use jamovi?**
A: We recommend scanning Tutorial 1 to learn ClinicoPath-specific features and navigation.

**Q: How long does each tutorial take?**
A: 30-60 minutes per tutorial, but you can work at your own pace. Tutorials are designed to be completed in one sitting or split across multiple sessions.

**Q: Do tutorials work with my own data?**
A: Yes! After completing tutorials with example data, apply the same methods to your datasets. Ensure your data follows the format guidelines in Tutorial 1.

**Q: Can I use tutorials for teaching?**
A: Absolutely! Tutorials are licensed CC-BY-4.0 and designed for classroom use. Adapt as needed for your courses.

**Q: Are video tutorials available?**
A: Not yet, but we're working on video supplements. Written tutorials with screenshots provide comprehensive guidance.

**Q: What if I get stuck?**
A: Each tutorial has a Troubleshooting section. For additional help, visit the [jamovi forum](https://forum.jamovi.org/) or [email us](mailto:serdarbalci@serdarbalci.com).

---

## Contact {#contact}

**Tutorial Author:** Serdar Balci, MD, PhD
**Email:** serdarbalci@serdarbalci.com
**Website:** [www.serdarbalci.com](https://www.serdarbalci.com)
**GitHub:** [sbalci/ClinicoPathJamoviModule](https://github.com/sbalci/ClinicoPathJamoviModule)

---

**Last Updated:** December 13, 2025
**Tutorial Series Version:** 1.0
**Compatible with:** ClinicoPath ≥ 0.0.32, jamovi ≥ 2.5
