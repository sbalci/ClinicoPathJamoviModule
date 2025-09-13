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

> how does FUNC_NAME handle varibale with empty spaces and characters in them.
is it necessary to implement escapeVariableNames logic from modelbuilder to FUNC_NAME.
In tables and plots I see the modified names that is why I am asking
can we apply labelled logic as in oddsratio

> implement welcome/introductory message styling of decisionpanel.

! Rscript -e "jmvtools::prepare()"
! Rscript -e "devtools::document()"

run jmvtools::prepare() to see if there are any errors

run devtools::document() to see if there are any errors

***

- stagemigration - Advanced TNM Stage Migration Analysis

/check-function stagemigration

/review-function stagemigration
/fix-function stagemigration


- ggoncoplot - Genomic Landscape Visualization
/check-function ggoncoplot

/review-function ggoncoplot
/fix-function ggoncoplot


- flexparametricadv - Advanced Flexible Parametric Survival Models
/check-function flexparametricadv

/review-function flexparametricadv
/fix-function flexparametricadv


- haralicktexture - Haralick Texture Analysis
/check-function haralicktexture

/review-function haralicktexture
/fix-function haralicktexture


- ihcstats - IHC Expression Analysis
/check-function ihcstats

/review-function ihcstats
/fix-function ihcstats


- spatialanalysis - Spatial Statistics from Coordinates
/check-function spatialanalysis

/review-function spatialanalysis
/fix-function spatialanalysis


- hierarchicalpathology - Hierarchical Mixed-Effects Models
/check-function hierarchicalpathology

/review-function hierarchicalpathology
/fix-function hierarchicalpathology


- consort - CONSORT Flowchart
/check-function consort

/review-function consort
/fix-function consort


- flowchart - Study Flowchart
/check-function flowchart

/review-function flowchart
/fix-function flowchart


- clinmon - Clinical Hemodynamic Monitoring
/check-function clinmon

/review-function clinmon
/fix-function clinmon


- pathologyagreement - Pathology Agreement Analysis
/check-function pathologyagreement

/review-function pathologyagreement
/fix-function pathologyagreement


- digitalvalidation - Digital Pathology Validation
/check-function digitalvalidation

/review-function digitalvalidation
/fix-function digitalvalidation


- mlpathology - Classification Performance Metrics for Digital Pathology
/check-function mlpathology

/review-function mlpathology
/fix-function mlpathology


- ihccluster - IHC Clustering Analysis
/check-function ihccluster

/review-function ihccluster
/fix-function ihccluster


- biopsysimulation - Biopsy Simulation Analysis
/check-function biopsysimulation

/review-function biopsysimulation
/fix-function biopsysimulation


- ihcscoring - IHC Scoring Standardization
/check-function ihcscoring

/review-function ihcscoring
/fix-function ihcscoring


- diagnosticmeta - Diagnostic Test Meta-Analysis for Pathology
/check-function diagnosticmeta

/review-function diagnosticmeta
/fix-function diagnosticmeta


- biomarkerresponse - Biomarker Response Association
/check-function biomarkerresponse

/review-function biomarkerresponse
/fix-function biomarkerresponse


- qualitycontrol - Laboratory Quality Control Statistics
/check-function qualitycontrol

/review-function qualitycontrol
/fix-function qualitycontrol


- metaanalysis - Meta-Analysis & Evidence Synthesis
/check-function metaanalysis

/review-function metaanalysis
/fix-function metaanalysis




- multiplexanalysis - Multiplex Immunofluorescence Analysis
/check-function multiplexanalysis

/review-function multiplexanalysis
/fix-function multiplexanalysis


- condsurvival - Conditional Survival Analysis
/check-function condsurvival

/review-function condsurvival
/fix-function condsurvival


- pathologycomposition - Pathology Composition Analysis
/check-function pathologycomposition

/review-function pathologycomposition
/fix-function pathologycomposition


- timeupdatesurvival - Time-Updated Survival Estimates
/check-function timeupdatesurvival

/review-function timeupdatesurvival
/fix-function timeupdatesurvival
