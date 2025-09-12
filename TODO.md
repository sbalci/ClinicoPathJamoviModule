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



- alluvial - Alluvial Diagrams
echo "/check-function alluvial" | claude
add jjsankeyfier features as additional options and plots to alluvial function
add riverplot features as additional options and plots to alluvial function
echo "/prepare-translation alluvial" | claude
echo "/review-function alluvial" | claude
echo "/fix-function alluvial" | claude


- chisqposttest - Chi-Square Post-Hoc Tests
echo "/check-function chisqposttest" | claude
echo "/prepare-translation chisqposttest" | claude
echo "/review-function chisqposttest" | claude
echo "/fix-function chisqposttest" | claude


- waterfall - Treatment Response Analysis
echo "/check-function waterfall" | claude

echo "/prepare-translation waterfall" | claude
echo "/review-function waterfall" | claude
echo "/fix-function waterfall" | claude


- swimmerplot - Swimmer Plot
echo "/check-function swimmerplot" | claude

echo "/prepare-translation swimmerplot" | claude
echo "/review-function swimmerplot" | claude
echo "/fix-function swimmerplot" | claude


- stagemigration - Advanced TNM Stage Migration Analysis
echo "/check-function stagemigration" | claude

echo "/prepare-translation stagemigration" | claude
echo "/review-function stagemigration" | claude
echo "/fix-function stagemigration" | claude


- ggoncoplot - Genomic Landscape Visualization
echo "/check-function ggoncoplot" | claude

echo "/prepare-translation ggoncoplot" | claude
echo "/review-function ggoncoplot" | claude
echo "/fix-function ggoncoplot" | claude


- flexparametricadv - Advanced Flexible Parametric Survival Models
echo "/check-function flexparametricadv" | claude

echo "/prepare-translation flexparametricadv" | claude
echo "/review-function flexparametricadv" | claude
echo "/fix-function flexparametricadv" | claude


- haralicktexture - Haralick Texture Analysis
echo "/check-function haralicktexture" | claude

echo "/prepare-translation haralicktexture" | claude
echo "/review-function haralicktexture" | claude
echo "/fix-function haralicktexture" | claude


- ihcstats - IHC Expression Analysis
echo "/check-function ihcstats" | claude

echo "/prepare-translation ihcstats" | claude
echo "/review-function ihcstats" | claude
echo "/fix-function ihcstats" | claude


- spatialanalysis - Spatial Statistics from Coordinates
echo "/check-function spatialanalysis" | claude

echo "/prepare-translation spatialanalysis" | claude
echo "/review-function spatialanalysis" | claude
echo "/fix-function spatialanalysis" | claude


- hierarchicalpathology - Hierarchical Mixed-Effects Models
echo "/check-function hierarchicalpathology" | claude

echo "/prepare-translation hierarchicalpathology" | claude
echo "/review-function hierarchicalpathology" | claude
echo "/fix-function hierarchicalpathology" | claude


- consort - CONSORT Flowchart
echo "/check-function consort" | claude

echo "/prepare-translation consort" | claude
echo "/review-function consort" | claude
echo "/fix-function consort" | claude


- flowchart - Study Flowchart
echo "/check-function flowchart" | claude

echo "/prepare-translation flowchart" | claude
echo "/review-function flowchart" | claude
echo "/fix-function flowchart" | claude


- clinmon - Clinical Hemodynamic Monitoring
echo "/check-function clinmon" | claude

echo "/prepare-translation clinmon" | claude
echo "/review-function clinmon" | claude
echo "/fix-function clinmon" | claude


- pathologyagreement - Pathology Agreement Analysis
echo "/check-function pathologyagreement" | claude

echo "/prepare-translation pathologyagreement" | claude
echo "/review-function pathologyagreement" | claude
echo "/fix-function pathologyagreement" | claude


- digitalvalidation - Digital Pathology Validation
echo "/check-function digitalvalidation" | claude

echo "/prepare-translation digitalvalidation" | claude
echo "/review-function digitalvalidation" | claude
echo "/fix-function digitalvalidation" | claude


- mlpathology - Classification Performance Metrics for Digital Pathology
echo "/check-function mlpathology" | claude

echo "/prepare-translation mlpathology" | claude
echo "/review-function mlpathology" | claude
echo "/fix-function mlpathology" | claude


- ihccluster - IHC Clustering Analysis
echo "/check-function ihccluster" | claude

echo "/prepare-translation ihccluster" | claude
echo "/review-function ihccluster" | claude
echo "/fix-function ihccluster" | claude


- biopsysimulation - Biopsy Simulation Analysis
echo "/check-function biopsysimulation" | claude

echo "/prepare-translation biopsysimulation" | claude
echo "/review-function biopsysimulation" | claude
echo "/fix-function biopsysimulation" | claude


- ihcscoring - IHC Scoring Standardization
echo "/check-function ihcscoring" | claude

echo "/prepare-translation ihcscoring" | claude
echo "/review-function ihcscoring" | claude
echo "/fix-function ihcscoring" | claude


- diagnosticmeta - Diagnostic Test Meta-Analysis for Pathology
echo "/check-function diagnosticmeta" | claude

echo "/prepare-translation diagnosticmeta" | claude
echo "/review-function diagnosticmeta" | claude
echo "/fix-function diagnosticmeta" | claude


- biomarkerresponse - Biomarker Response Association
echo "/check-function biomarkerresponse" | claude

echo "/prepare-translation biomarkerresponse" | claude
echo "/review-function biomarkerresponse" | claude
echo "/fix-function biomarkerresponse" | claude


- qualitycontrol - Laboratory Quality Control Statistics
echo "/check-function qualitycontrol" | claude

echo "/prepare-translation qualitycontrol" | claude
echo "/review-function qualitycontrol" | claude
echo "/fix-function qualitycontrol" | claude


- metaanalysis - Meta-Analysis & Evidence Synthesis
echo "/check-function metaanalysis" | claude

echo "/prepare-translation metaanalysis" | claude
echo "/review-function metaanalysis" | claude
echo "/fix-function metaanalysis" | claude




- multiplexanalysis - Multiplex Immunofluorescence Analysis
echo "/check-function multiplexanalysis" | claude

echo "/prepare-translation multiplexanalysis" | claude
echo "/review-function multiplexanalysis" | claude
echo "/fix-function multiplexanalysis" | claude


- condsurvival - Conditional Survival Analysis
echo "/check-function condsurvival" | claude

echo "/prepare-translation condsurvival" | claude
echo "/review-function condsurvival" | claude
echo "/fix-function condsurvival" | claude


- pathologycomposition - Pathology Composition Analysis
echo "/check-function pathologycomposition" | claude

echo "/prepare-translation pathologycomposition" | claude
echo "/review-function pathologycomposition" | claude
echo "/fix-function pathologycomposition" | claude


- timeupdatesurvival - Time-Updated Survival Estimates
echo "/check-function timeupdatesurvival" | claude

echo "/prepare-translation timeupdatesurvival" | claude
echo "/review-function timeupdatesurvival" | claude
echo "/fix-function timeupdatesurvival" | claude


