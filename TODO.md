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

prepare a realistic data to test the features in detail 
move csv files under data folder.
move data generation files under data-raw folder.
move documantation files under vignettes folder.


***

- ihcstats - IHC Expression Analysis
/check-function ihcstats
/review-function ihcstats
/fix-function ihcstats

- ihccluster - IHC Clustering Analysis
/check-function ihccluster
/review-function ihccluster
/fix-function ihccluster

- ihcscoring - IHC Scoring Standardization
/check-function ihcscoring
/review-function ihcscoring
/fix-function ihcscoring

- multiplexanalysis - Multiplex Immunofluorescence Analysis
/check-function multiplexanalysis
/review-function multiplexanalysis
/fix-function multiplexanalysis


***

- flowchart - Study Flowchart

/check-function flowchart

check upstream flowchart and ggflowchart packages to get the relevant features, applicable arguments, and outputs. 

/review-function flowchart
/fix-function flowchart


- consort - CONSORT Flowchart
/check-function consort

read this documentation and improve implementation:
https://cran.r-project.org/web/packages/consort/vignettes/consort_diagram.html


read this documentation and improve implementation:
https://www.riinu.me/2024/02/consort/

read this documentation and improve implementation:
https://hbiostat.org/rflow/doverview.html

implement ggconsort package features
https://github.com/tgerke/ggconsort
https://tgerke.github.io/ggconsort/



/review-function consort
/fix-function consort

prepare a realistic data to test consort


***

- pathologyagreement - Pathology Agreement Analysis
/check-function pathologyagreement
/review-function pathologyagreement
/fix-function pathologyagreement


***

- biopsysimulation - Biopsy Simulation Analysis
/check-function biopsysimulation
/review-function biopsysimulation
/fix-function biopsysimulation

***

- diagnosticmeta - Diagnostic Test Meta-Analysis for Pathology
/check-function diagnosticmeta
/review-function diagnosticmeta
/fix-function diagnosticmeta

- metaanalysis - Meta-Analysis & Evidence Synthesis
/check-function metaanalysis
/review-function metaanalysis
/fix-function metaanalysis

