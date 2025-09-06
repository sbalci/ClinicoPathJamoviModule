## check articles

source .venv/bin/activate

.claude/completions/review_article_stats_save.sh "aqaf082" \
  "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/temp3/aqaf082.pdf"

.claude/completions/review_article_stats_save.sh "Thyroid-CNN" \
  "/path/paper.pdf" "/path/supplement.html" "/path/notes.md"

.claude/completions/review_article_stats_save.sh "Example-URL" \
  "<https://example.com/article.html>"

> pdftotext

> /review-article-stats '/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/temp/untitled folder/Multi-modal convolutional neural network-based thyroid cytology classification and diagnosis - ScienceDirect.md'
'/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/temp/untitled folder/Multi-modal convolutional neural network-based thyroid cytology classification and diagnosis - ScienceDirect.html'
'/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/temp/untitled folder/1-s2.0-S0046817725001558-main.pdf'
'/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/temp/untitled folder/1-s2.0-S0046817725001558-main.md'

claude --no-mcp --no-tools "/review-article-stats Deep-Learning-Based-Prediction" \
  "/Users/.../Deep-Learning-Based-Prediction.md" \
  "/Users/.../Deep-Learning-Based-Prediction.html" \
  "/Users/.../Deep-Learning-Based-Prediction.pdf" \
  "/Users/.../Deep-Learning-Based-Prediction.txt"

## check and update each function

echo "/document-function " | claude
claude "/document-function "

echo "/check-function FUNC_NAME" | claude
echo "/checkpoint FUNC_NAME" | claude
echo "/prepare-translation FUNC_NAME" | claude
echo "/review-function FUNC_NAME" | claude
echo "/fix-function FUNC_NAME" | claude
echo "/document-function FUNC_NAME" | claude

> fix issues and implement recommendations. favor functionality over explanations and guidence parts.

> implement escapeVariableNames logic from modelbuilder to FUNC_NAME

> implement welcome/introductory message styling of decisionpanel.

***

## meddecideT

- enhancedROC - Enhanced ROC Analysis with Youden Index Optimization




echo "/review-function enhancedROC" | claude
echo "/fix-function enhancedROC" | claude
echo "/document-function enhancedROC" | claude

- psychopdaROC - ROC Analysis
echo "/check-function psychopdaROC" | claude
echo "/checkpoint psychopdaROC" | claude
echo "/prepare-translation psychopdaROC" | claude
echo "/review-function psychopdaROC" | claude
echo "/fix-function psychopdaROC" | claude
echo "/document-function psychopdaROC" | claude

- agreement - Interrater Reliability
echo "/check-function agreement" | claude
echo "/checkpoint agreement" | claude
echo "/prepare-translation agreement" | claude
echo "/review-function agreement" | claude
echo "/fix-function agreement" | claude
echo "/document-function agreement" | claude

- decision - Medical Decision
echo "/check-function decision" | claude
echo "/checkpoint decision" | claude
echo "/prepare-translation decision" | claude
echo "/review-function decision" | claude
echo "/fix-function decision" | claude
echo "/document-function decision" | claude

- decisioncombine - Combine Medical Decision Tests
echo "/check-function decisioncombine" | claude
echo "/checkpoint decisioncombine" | claude
echo "/prepare-translation decisioncombine" | claude
echo "/review-function decisioncombine" | claude
echo "/fix-function decisioncombine" | claude
echo "/document-function decisioncombine" | claude

- modalitycomparison - Modality Comparison Analysis
echo "/check-function modalitycomparison" | claude
echo "/checkpoint modalitycomparison" | claude
echo "/prepare-translation modalitycomparison" | claude
echo "/review-function modalitycomparison" | claude
echo "/fix-function modalitycomparison" | claude
echo "/document-function modalitycomparison" | claude

- decisioncompare - Compare Medical Decision Tests
echo "/check-function decisioncompare" | claude
echo "/checkpoint decisioncompare" | claude
echo "/prepare-translation decisioncompare" | claude
echo "/review-function decisioncompare" | claude
echo "/fix-function decisioncompare" | claude
echo "/document-function decisioncompare" | claude

/check-module-meddecide

## SurvivalT (5 functions)

implement escapeVariableNames logic from modelbuilder to oddsratio

- oddsratio - Odds Ratio Table and Plot
echo "/check-function oddsratio" | claude
echo "/checkpoint oddsratio" | claude
echo "/prepare-translation oddsratio" | claude
echo "/review-function oddsratio" | claude
echo "/fix-function oddsratio" | claude
echo "/document-function oddsratio" | claude

- singlearm - Single Arm Survival
echo "/check-function singlearm" | claude
echo "/checkpoint singlearm" | claude
echo "/prepare-translation singlearm" | claude
echo "/review-function singlearm" | claude
echo "/fix-function singlearm" | claude
echo "/document-function singlearm" | claude

implement escapeVariableNames logic from modelbuilder to survival

- survival - Survival Analysis
echo "/check-function survival" | claude
echo "/checkpoint survival" | claude
echo "/prepare-translation survival" | claude
echo "/review-function survival" | claude
echo "/fix-function survival" | claude
echo "/document-function survival" | claude

implement escapeVariableNames logic from modelbuilder to multisurvival

- multisurvival - Multivariable Survival Analysis
echo "/check-function multisurvival" | claude
echo "/checkpoint multisurvival" | claude
echo "/prepare-translation multisurvival" | claude
echo "/review-function multisurvival" | claude
echo "/fix-function multisurvival" | claude
echo "/document-function multisurvival" | claude

implement escapeVariableNames logic from modelbuilder to survivalcont

- survivalcont - Survival Analysis for Continuous Variable
echo "/check-function survivalcont" | claude
echo "/checkpoint survivalcont" | claude
echo "/prepare-translation survivalcont" | claude
echo "/review-function survivalcont" | claude
echo "/fix-function survivalcont" | claude
echo "/document-function survivalcont" | claude

/check-module-jsurvival

## ExplorationT (10 functions)

- reportcat - Summary of Categorical Variables
echo "/check-function reportcat" | claude
echo "/checkpoint reportcat" | claude
echo "/prepare-translation reportcat" | claude
echo "/review-function reportcat" | claude
echo "/fix-function reportcat" | claude
echo "/document-function reportcat" | claude

- summarydata - Summary of Continuous Variables
echo "/check-function summarydata" | claude
echo "/checkpoint summarydata" | claude
echo "/prepare-translation summarydata" | claude
echo "/review-function summarydata" | claude
echo "/fix-function summarydata" | claude
echo "/document-function summarydata" | claude

- venn - Venn Diagram
echo "/check-function venn" | claude
echo "/checkpoint venn" | claude
echo "/prepare-translation venn" | claude
echo "/review-function venn" | claude
echo "/fix-function venn" | claude
echo "/document-function venn" | claude

implement escapeVariableNames logic from modelbuilder to crosstable

- crosstable - Cross Tables
echo "/check-function crosstable" | claude
echo "/checkpoint crosstable" | claude
echo "/prepare-translation crosstable" | claude
echo "/review-function crosstable" | claude
echo "/fix-function crosstable" | claude
echo "/document-function crosstable" | claude

- tableone - Table One
echo "/check-function tableone" | claude
echo "/checkpoint tableone" | claude
echo "/prepare-translation tableone" | claude
echo "/review-function tableone" | claude
echo "/fix-function tableone" | claude
echo "/document-function tableone" | claude

implement escapeVariableNames logic from modelbuilder to vartree

- vartree - Variable Tree
echo "/check-function vartree" | claude
echo "/checkpoint vartree" | claude
echo "/prepare-translation vartree" | claude
echo "/review-function vartree" | claude
echo "/fix-function vartree" | claude
echo "/document-function vartree" | claude

- chisqposttest - Chi-Square Post-Hoc Tests
echo "/check-function chisqposttest" | claude
echo "/checkpoint chisqposttest" | claude
echo "/prepare-translation chisqposttest" | claude
echo "/review-function chisqposttest" | claude
echo "/fix-function chisqposttest" | claude
echo "/document-function chisqposttest" | claude

- benford - Benford Analysis
echo "/check-function benford" | claude
echo "/checkpoint benford" | claude
echo "/prepare-translation benford" | claude
echo "/review-function benford" | claude
echo "/fix-function benford" | claude
echo "/document-function benford" | claude

add jjsankeyfier features as additional options and plots to alluvial function
add riverplot features as additional options and plots to alluvial function

- alluvial - Alluvial Diagrams
echo "/check-function alluvial" | claude
echo "/checkpoint alluvial" | claude
echo "/prepare-translation alluvial" | claude
echo "/review-function alluvial" | claude
echo "/fix-function alluvial" | claude
echo "/document-function alluvial" | claude

/check-module-ClinicoPathDescriptives

## OncoPathologyT (26 functions)


- waterfall - Treatment Response Analysis
echo "/check-function waterfall" | claude
echo "/checkpoint waterfall" | claude
echo "/prepare-translation waterfall" | claude
echo "/review-function waterfall" | claude
echo "/fix-function waterfall" | claude
echo "/document-function waterfall" | claude


- swimmerplot - Swimmer Plot
echo "/check-function swimmerplot" | claude
echo "/checkpoint swimmerplot" | claude
echo "/prepare-translation swimmerplot" | claude
echo "/review-function swimmerplot" | claude
echo "/fix-function swimmerplot" | claude
echo "/document-function swimmerplot" | claude

- stagemigration - Advanced TNM Stage Migration Analysis
echo "/check-function stagemigration" | claude
echo "/checkpoint stagemigration" | claude
echo "/prepare-translation stagemigration" | claude
echo "/review-function stagemigration" | claude
echo "/fix-function stagemigration" | claude
echo "/document-function stagemigration" | claude



- ggoncoplot - Genomic Landscape Visualization
echo "/check-function ggoncoplot" | claude
echo "/checkpoint ggoncoplot" | claude
echo "/prepare-translation ggoncoplot" | claude
echo "/review-function ggoncoplot" | claude
echo "/fix-function ggoncoplot" | claude
echo "/document-function ggoncoplot" | claude

- flexparametricadv - Advanced Flexible Parametric Survival Models
echo "/check-function flexparametricadv" | claude
echo "/checkpoint flexparametricadv" | claude
echo "/prepare-translation flexparametricadv" | claude
echo "/review-function flexparametricadv" | claude
echo "/fix-function flexparametricadv" | claude
echo "/document-function flexparametricadv" | claude

- haralicktexture - Haralick Texture Analysis
echo "/check-function haralicktexture" | claude
echo "/checkpoint haralicktexture" | claude
echo "/prepare-translation haralicktexture" | claude
echo "/review-function haralicktexture" | claude
echo "/fix-function haralicktexture" | claude
echo "/document-function haralicktexture" | claude

- ihcstats - IHC Expression Analysis
echo "/check-function ihcstats" | claude
echo "/checkpoint ihcstats" | claude
echo "/prepare-translation ihcstats" | claude
echo "/review-function ihcstats" | claude
echo "/fix-function ihcstats" | claude
echo "/document-function ihcstats" | claude



- spatialanalysis - Spatial Statistics from Coordinates
echo "/check-function spatialanalysis" | claude
echo "/checkpoint spatialanalysis" | claude
echo "/prepare-translation spatialanalysis" | claude
echo "/review-function spatialanalysis" | claude
echo "/fix-function spatialanalysis" | claude
echo "/document-function spatialanalysis" | claude

- hierarchicalpathology - Hierarchical Mixed-Effects Models
echo "/check-function hierarchicalpathology" | claude
echo "/checkpoint hierarchicalpathology" | claude
echo "/prepare-translation hierarchicalpathology" | claude
echo "/review-function hierarchicalpathology" | claude
echo "/fix-function hierarchicalpathology" | claude
echo "/document-function hierarchicalpathology" | claude

- consort - CONSORT Flowchart
echo "/check-function consort" | claude
echo "/checkpoint consort" | claude
echo "/prepare-translation consort" | claude
echo "/review-function consort" | claude
echo "/fix-function consort" | claude
echo "/document-function consort" | claude

- clinmon - Clinical Hemodynamic Monitoring
echo "/check-function clinmon" | claude
echo "/checkpoint clinmon" | claude
echo "/prepare-translation clinmon" | claude
echo "/review-function clinmon" | claude
echo "/fix-function clinmon" | claude
echo "/document-function clinmon" | claude

- pathologyagreement - Pathology Agreement Analysis
echo "/check-function pathologyagreement" | claude
echo "/checkpoint pathologyagreement" | claude
echo "/prepare-translation pathologyagreement" | claude
echo "/review-function pathologyagreement" | claude
echo "/fix-function pathologyagreement" | claude
echo "/document-function pathologyagreement" | claude

- digitalvalidation - Digital Pathology Validation
echo "/check-function digitalvalidation" | claude
echo "/checkpoint digitalvalidation" | claude
echo "/prepare-translation digitalvalidation" | claude
echo "/review-function digitalvalidation" | claude
echo "/fix-function digitalvalidation" | claude
echo "/document-function digitalvalidation" | claude

- mlpathology - Classification Performance Metrics for Digital Pathology
echo "/check-function mlpathology" | claude
echo "/checkpoint mlpathology" | claude
echo "/prepare-translation mlpathology" | claude
echo "/review-function mlpathology" | claude
echo "/fix-function mlpathology" | claude
echo "/document-function mlpathology" | claude

- ihccluster - IHC Clustering Analysis
echo "/check-function ihccluster" | claude
echo "/checkpoint ihccluster" | claude
echo "/prepare-translation ihccluster" | claude
echo "/review-function ihccluster" | claude
echo "/fix-function ihccluster" | claude
echo "/document-function ihccluster" | claude

- biopsysimulation - Biopsy Simulation Analysis
echo "/check-function biopsysimulation" | claude
echo "/checkpoint biopsysimulation" | claude
echo "/prepare-translation biopsysimulation" | claude
echo "/review-function biopsysimulation" | claude
echo "/fix-function biopsysimulation" | claude
echo "/document-function biopsysimulation" | claude

- ihcscoring - IHC Scoring Standardization
echo "/check-function ihcscoring" | claude
echo "/checkpoint ihcscoring" | claude
echo "/prepare-translation ihcscoring" | claude
echo "/review-function ihcscoring" | claude
echo "/fix-function ihcscoring" | claude
echo "/document-function ihcscoring" | claude

- diagnosticmeta - Diagnostic Test Meta-Analysis for Pathology
echo "/check-function diagnosticmeta" | claude
echo "/checkpoint diagnosticmeta" | claude
echo "/prepare-translation diagnosticmeta" | claude
echo "/review-function diagnosticmeta" | claude
echo "/fix-function diagnosticmeta" | claude
echo "/document-function diagnosticmeta" | claude

- biomarkerresponse - Biomarker Response Association
echo "/check-function biomarkerresponse" | claude
echo "/checkpoint biomarkerresponse" | claude
echo "/prepare-translation biomarkerresponse" | claude
echo "/review-function biomarkerresponse" | claude
echo "/fix-function biomarkerresponse" | claude
echo "/document-function biomarkerresponse" | claude

- qualitycontrol - Laboratory Quality Control Statistics
echo "/check-function qualitycontrol" | claude
echo "/checkpoint qualitycontrol" | claude
echo "/prepare-translation qualitycontrol" | claude
echo "/review-function qualitycontrol" | claude
echo "/fix-function qualitycontrol" | claude
echo "/document-function qualitycontrol" | claude

- metaanalysis - Meta-Analysis & Evidence Synthesis
echo "/check-function metaanalysis" | claude
echo "/checkpoint metaanalysis" | claude
echo "/prepare-translation metaanalysis" | claude
echo "/review-function metaanalysis" | claude
echo "/fix-function metaanalysis" | claude
echo "/document-function metaanalysis" | claude

- flowchart - Study Flowchart
echo "/check-function flowchart" | claude
echo "/checkpoint flowchart" | claude
echo "/prepare-translation flowchart" | claude
echo "/review-function flowchart" | claude
echo "/fix-function flowchart" | claude
echo "/document-function flowchart" | claude


- multiplexanalysis - Multiplex Immunofluorescence Analysis
echo "/check-function multiplexanalysis" | claude
echo "/checkpoint multiplexanalysis" | claude
echo "/prepare-translation multiplexanalysis" | claude
echo "/review-function multiplexanalysis" | claude
echo "/fix-function multiplexanalysis" | claude
echo "/document-function multiplexanalysis" | claude

- condsurvival - Conditional Survival Analysis
echo "/check-function condsurvival" | claude
echo "/checkpoint condsurvival" | claude
echo "/prepare-translation condsurvival" | claude
echo "/review-function condsurvival" | claude
echo "/fix-function condsurvival" | claude
echo "/document-function condsurvival" | claude




- pathologycomposition - Pathology Composition Analysis
echo "/check-function pathologycomposition" | claude
echo "/checkpoint pathologycomposition" | claude
echo "/prepare-translation pathologycomposition" | claude
echo "/review-function pathologycomposition" | claude
echo "/fix-function pathologycomposition" | claude
echo "/document-function pathologycomposition" | claude

- timeupdatesurvival - Time-Updated Survival Estimates
echo "/check-function timeupdatesurvival" | claude
echo "/checkpoint timeupdatesurvival" | claude
echo "/prepare-translation timeupdatesurvival" | claude
echo "/review-function timeupdatesurvival" | claude
echo "/fix-function timeupdatesurvival" | claude
echo "/document-function timeupdatesurvival" | claude

/check-module-OncoPathology
