I am generating hundreds of example data to be used as examples with hashtag#jamovi 
I wanted to share them and used Anthropic claude slash command to prepare explanatory texts for each jamovi function in hashtag#ClinicoPath module and corresponding dataset. 
Lets see how it goes: 

How do you compare Ki-67 expression across tumor grades? Or hemoglobin levels between treatment arms?

 • Flexible Statistical Framework: Choose from parametric (t-test/ANOVA), non-parametric (Mann-Whitney/Kruskal-Wallis), robust (trimmed means), or Bayesian approaches based on your data distribution
 • Automatic Pairwise Comparisons: Get post-hoc comparisons with built-in multiple testing corrections (Bonferroni, Holm, Hochberg, BH, FDR)
 • Effect Sizes with CIs: Not just p-values—calculate eta-squared, omega-squared, or Cohen's d with confidence intervals for meaningful interpretation
 • Publication-Ready Output: Generate plots with journal-specific color palettes (Journal of Clinical Oncology, Nature, AAAS, Lancet, JAMA, NEJM) and colorblind-safe options
 • Stratified Analysis: Split comparisons by additional variables—compare treatment responses across different tumor stages simultaneously

 • Biomarker Validation: Compare IHC expression scores (Ki-67, ER, PR, HER2) across tumor grades or molecular subtypes
 • Treatment Response: Evaluate changes in tumor markers (CEA, CA19-9, PSA) between treatment groups with statistical rigor
 • Diagnostic Categories: Visualize differences in continuous pathology metrics across benign, borderline, and malignant cases
 • Lab Value Analysis: Compare hemoglobin, albumin, or inflammatory markers between patient cohorts for Table 1 presentations

This function combines the power of hashtag#ggstatsplot with jamovi's point-and-click interface, giving you publication-quality plots in seconds. Whether you're comparing tumor proliferation indices across grades, validating new biomarkers, or preparing manuscript figures—jjbetweenstats handles the statistics and aesthetics automatically.

hashtag#Pathology hashtag#DigitalPathology hashtag#ClinicalResearch hashtag#Biostatistics hashtag#Oncology hashtag#Jamovi hashtag#OpenScience hashtag#BiomarkerResearch hashtag#IHC

EXAMPLE DATASETS
 https://lnkd.in/dkcEnaGt


---


"Every new IHC is sensitive and specific until the next USCAP."

This is the joke we use when evaluating new antibodies. We see promising antibodies, then watch the decline of sensitivity and specificity while reading the abstracts. 

I recall a study (should be last year ESP) using text mining to extract positive/negative case counts from PubMed abstracts and summarising the results. There are also commercial websites listing positive and negative numbers to guide pathologists. 

Here we should be aware of simple pooling. We cannot simply sum positive and negative cases across studies to calculate sensitivity and specificity—even if we ignore lab conditions and antibody clones.

Why Simple Pooling Fails: 
- Sensitivity and specificity are negatively correlated across studies (threshold effect). Studies reporting higher sensitivity often show lower specificity due to different cut-points. Simple pooling ignores this correlation and produces biased estimates.
- Study-level heterogeneity varies dramatically. A 20-case single-institution study shouldn't contribute equally to a 500-case multi-center validation. Simple pooling treats them identically.
- Publication bias systematically inflates accuracy. Studies with disappointing results (sensitivity <70%) rarely get published. Simple pooling amplifies this bias.
- Zero-cell studies (100% sensitivity or specificity) break simple proportion calculations and indicate selective reporting or inadequate validation.

The Proper Approach would be Bivariate Diagnostic Meta-Analysis. Before adopting new diagnostic tests in pathology practice, we need methodologically rigorous evidence synthesis. 

In jamovi OncoPath module there is a function for that: diagnosticmeta
To use it download jamovi or use jamovi cloud and install the OncoPath module from the library. 

Example datasets to explore:

  📁 diagnosticmeta_test_large.omv - Large-scale meta-analysis (20+ studies)
  See how heterogeneity increases with more studies and why simple pooling fails
  https://raw.githubusercontent.com/sbalci/ClinicoPathJamoviModule/master/data-raw/non-rda/diagnosticmeta_test_large.omv

  📁 diagnosticmeta_test_zeros.omv - Studies with perfect accuracy
  See how zero-cell corrections prevent computational errors
  https://raw.githubusercontent.com/sbalci/ClinicoPathJamoviModule/master/data-raw/non-rda/diagnosticmeta_test_zeros.omv

  📁 diagnosticmeta_test.omv - General diagnostic meta-analysis
  Complete workflow from data entry to publication-ready outputs
  https://raw.githubusercontent.com/sbalci/ClinicoPathJamoviModule/master/data-raw/non-rda/diagnosticmeta_test.omv



#Pathology #DigitalPathology #ClinicalResearch #Biostatistics #Oncology #Jamovi #OpenScience #AIinPathology #EvidenceBasedMedicine #IHC #DiagnosticAccuracy

