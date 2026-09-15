# Candidate stranded results — elements hidden with an option-bound `visible:` and never restored

Generated 2026-08-20. This is the CLASS the psychopdaroc HIGH belonged to
(jamovi-library-audit/2026-08-17 meddecide.md): `.init()` hides an element whose
`.r.yaml` already binds visibility to an option, and nothing ever restores it, so
the option computes output that is silently discarded.

**Not all of these are bugs.** Some hide legitimately inside a conditional branch
and show an explanation in a different, visible element — psychopdaroc's `dotPlot`
in combined-plot mode is exactly that, and is correct. Telling the two apart needs
control-flow reading, which is why this is a review list and not a failing test.

Verify each by ticking the bound option in jamovi and checking the output appears.

```
STRANDED (hidden, option-bound in .r.yaml, never restored): 52 

agreement.b.R :: contingencyTable  (visible: (sft))
agreement.b.R :: ratingCombinationsTable  (visible: (sft))
clinmon.b.R :: detailed_results  (visible: (show_detailed))
clinmon.b.R :: summary_stats  (visible: (show_summary))
coefplot.b.R :: coefficient_plot  (visible: (show_coefficient_plot))
coefplot.b.R :: model_summary  (visible: (show_model_summary))
coefplot.b.R :: coefficient_table  (visible: (show_coefficient_table))
desctools.b.R :: effect_size_results  (visible: (effect_size_analysis && show_effect_sizes))
desctools.b.R :: goodness_fit_results  (visible: (goodness_of_fit && show_goodness_tests))
desctools.b.R :: categorical_results  (visible: (categorical_tests && show_categorical_tests))
diagnosticmeta.b.R :: summary  (visible: (show_analysis_summary))
diagnosticmeta.b.R :: interpretation  (visible: (show_interpretation))
enhancedtables.b.R :: enhanced_table  (visible: (vars))
enhancedtables.b.R :: summary_stats  (visible: (table_type:summary))
enhancedtables.b.R :: group_comparison  (visible: (include_pvalues))
enhancedtables.b.R :: export_table  (visible: (export_format))
enhancedtables.b.R :: interpretation  (visible: (show_interpretation))
grafify.b.R :: summary_stats  (visible: (show_summary_stats))
grafify.b.R :: statistical_analysis  (visible: (add_statistics))
grafify.b.R :: posthoc_results  (visible: (posthoc_comparisons && add_statistics))
grafify.b.R :: diagnostic_plots  (visible: (show_model_diagnostics && add_statistics))
grafify.b.R :: qqplot  (visible: (show_model_diagnostics && add_statistics))
grafify.b.R :: export_info  (visible: (export_data))
hematologicindices.b.R :: gpsTable  (visible: (showIndicesTable))
jrecode.b.R :: levels_table  (visible: (show_levels))
jrecode.b.R :: code_output  (visible: (show_code))
jrecode.b.R :: comparison  (visible: (show_table))
lassocox.b.R :: cv_plot  (visible: (cv_plot))
lassocox.b.R :: coef_plot  (visible: (coef_plot))
lassocox.b.R :: survival_plot  (visible: (survival_plot))
multisurvival.b.R :: plot  (visible: (hr && sty:t1))
multisurvival.b.R :: plot3  (visible: (hr && sty:t3))
multisurvival.b.R :: plotKM  (visible: (km))
multisurvival.b.R :: plot_adj  (visible: (ac))
multisurvival.b.R :: plot_nomogram  (visible: (showNomogram))
multisurvival.b.R :: plot8  (visible: (ph_cox))
multisurvival.b.R :: survMetricsSummary  (visible: (show_survmetrics && showSummaries))
multisurvival.b.R :: survMetricsPlot  (visible: (show_survmetrics && survmetrics_show_plots))
pathagreement.b.R :: crosstabTable  (visible: (sft))
psychopdaroc.b.R :: dotPlot  (visible: (showDotPlot))
rpasurvival.b.R :: summary  (visible: (showSummary))
rpasurvival.b.R :: interpretation  (visible: (showInterpretation))
rpasurvival.b.R :: report  (visible: (showReport))
rpasurvival.b.R :: treeplot  (visible: (treeplot))
rpasurvival.b.R :: riskgrouptable  (visible: (riskgrouptable))
rpasurvival.b.R :: kmplot  (visible: (kmplot))
rpasurvival.b.R :: logranktest  (visible: (kmplot))
rpasurvival.b.R :: cptable  (visible: (cptable))
rpasurvival.b.R :: varimp  (visible: (variableimportance))
rpasurvival.b.R :: coxmodel  (visible: (riskgrouptable))
singlearm.b.R :: plot_cif  (visible: (sc && multievent && analysistype:compete))
waterfall.b.R :: todo  (visible: (enableGuidedMode:FALSE)) 
```
