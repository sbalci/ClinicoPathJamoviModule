lassointroClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "lassointroClass",
    inherit = lassointroBase,
    private = list(

        .init = function() {
            self$results$overview$setContent(private$.overviewHtml())
            self$results$decisionGuide$setContent(private$.decisionGuideHtml())
            self$results$clinicalScenarios$setContent(private$.clinicalScenariosHtml())
            self$results$assumptions$setContent(private$.assumptionsHtml())
            self$results$glossary$setContent(private$.glossaryHtml())
        },

        .run = function() {
            # No computation needed
        },

        # =====================================================================
        # HTML Content Generators
        # =====================================================================

        .cssBlock = function() {
            '<style>
            .lg { max-width: 780px; line-height: 1.6; }
            .lg table { width: 100%; border-collapse: collapse; margin-bottom: 16px; }
            .lg th { padding: 8px; text-align: left; border-bottom: 2px solid #333; }
            .lg th.center { text-align: center; }
            .lg td { padding: 8px; border-bottom: 1px solid #ccc; }
            .lg td.center { text-align: center; }
            .lg td.vtop, .lg th.vtop { vertical-align: top; width: 28%; }
            .lg .callout { background: #f8f4e8; border-left: 4px solid #c9a825; padding: 12px 16px; margin: 16px 0; }
            </style>'
        },

        .overviewHtml = function() {
            paste0(private$.cssBlock(),
'<div class="lg">

<h2>Penalized Cox Regression Guide</h2>
<p>Choosing the right method for variable selection in survival analysis.</p>

<p><strong>Who is this for?</strong> Clinicians, pathologists, radiologists, and oncologists
who have survival data with many potential predictors and need to identify which variables
truly matter for patient outcomes.</p>

<div class="callout">
<strong>When NOT to use penalized regression:</strong>
If you have fewer than ~15 candidate predictors and an events-per-variable ratio above 10
(e.g., 150 events with 12 variables), standard multivariable Cox regression is usually better.
It gives you p-values, confidence intervals, and unbiased hazard ratios without penalization.
Use the Survival &rarr; Multivariable Survival menu instead.
</div>

<h3>Available Methods (Increasing Complexity)</h3>

<table>
<thead>
  <tr>
    <th>Method</th>
    <th>Best For</th>
    <th>Complexity</th>
    <th>Menu Location</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td><strong>LASSO Cox</strong></td>
    <td>First-line variable selection. Automatically drops irrelevant predictors. Start here if unsure.</td>
    <td>Basic</td>
    <td>Penalized Cox &rarr; LASSO Cox</td>
  </tr>
  <tr>
    <td><strong>Adaptive LASSO Cox</strong></td>
    <td>More accurate selection when you need statistical consistency.
      Uses data-driven weights to penalize less important variables more heavily.</td>
    <td>Moderate</td>
    <td>Penalized Cox &rarr; Adaptive LASSO Cox</td>
  </tr>
  <tr>
    <td><strong>Elastic Net Cox</strong></td>
    <td>When predictors are moderately correlated.
      Keeps groups of correlated variables together instead of picking just one arbitrarily.</td>
    <td>Moderate</td>
    <td>Penalized Cox &rarr; Elastic Net Cox</td>
  </tr>
  <tr>
    <td><strong>Group LASSO Cox</strong></td>
    <td>When variables naturally form groups (e.g., grouped by genes, pathways, or categories). Drops or keeps whole groups at a time.</td>
    <td>Advanced</td>
    <td>Penalized Cox (Drafts) &rarr; Group LASSO Cox</td>
  </tr>
  <tr>
    <td><strong>Sparse Group LASSO</strong></td>
    <td>Similar to Group LASSO, but can drop individual variables within a selected group. Highly flexible for clustered features.</td>
    <td>Advanced</td>
    <td>Penalized Cox (Drafts) &rarr; Sparse Group LASSO</td>
  </tr>
  <tr>
    <td><strong>SCAD/MCP Cox</strong></td>
    <td>When you need unbiased coefficient estimates. Standard LASSO shrinks large effects
      toward zero; SCAD and MCP do not.</td>
    <td>Advanced</td>
    <td>Penalized Cox &rarr; SCAD/MCP Cox</td>
  </tr>
  <tr>
    <td><strong>PCA Cox</strong></td>
    <td>Reduces variables into linearly uncorrelated primary components. Excellent for severe multicollinearity.</td>
    <td>Moderate</td>
    <td>Dimension Reduction Cox &rarr; PCA Cox</td>
  </tr>
  <tr>
    <td><strong>PLS Cox</strong></td>
    <td>When you have far more variables than patients (e.g., genomic data, radiomic features).
      Creates outcome-aware composite scores rather than just variance-based components.</td>
    <td>Advanced</td>
    <td>Dimension Reduction Cox &rarr; PLS Cox</td>
  </tr>
</tbody>
</table>

<p><strong>Rule of thumb:</strong> Start with LASSO Cox. If your predictors are highly correlated,
switch to Elastic Net Cox. If the variables form known biological groupings, use Group LASSO. If you have more variables than patients and just want composite dimensions, use PLS or PCA.
Check the Decision Guide section for a step-by-step flowchart.</p>

</div>')
        },

        .decisionGuideHtml = function() {
            paste0(private$.cssBlock(),
'<div class="lg">

<h3>Decision Flowchart: Which Method Should I Use?</h3>

<p><strong>Step 1: How many predictors vs. patients?</strong></p>
<ul>
  <li>Predictors &lt; Patients (p &lt; n) &rarr; Go to Step 2</li>
  <li>Predictors &ge; Patients (p &ge; n) &rarr; Use dimension reduction (<strong>PCA Cox</strong> or <strong>PLS Cox</strong>) or <strong>Elastic Net Cox</strong></li>
</ul>

<p><strong>Step 2: Are your predictors naturally grouped?</strong></p>
<ul>
  <li>Yes (e.g., biological pathways or categorical group clusters) &rarr; Use <strong>Group LASSO</strong> or <strong>Sparse Group LASSO</strong></li>
  <li>No &rarr; Go to Step 3</li>
</ul>

<p><strong>Step 3: Are your predictors strongly correlated?</strong></p>
<ul>
  <li>No or mild correlation (r &lt; 0.7) &rarr; Go to Step 4</li>
  <li>Moderate/Strong correlation (r &ge; 0.7) &rarr; Use <strong>Elastic Net Cox</strong> to select correlated groups, or <strong>PCA Cox</strong> to shrink to orthogonal components.</li>
</ul>

<p><strong>Step 4: What is your primary variable selection goal?</strong></p>
<ul>
  <li>Identify which variables matter roughly (screening) &rarr; Use <strong>LASSO Cox</strong></li>
  <li>Accurate variable selection with mathematical consistency (Oracle) &rarr; Use <strong>Adaptive LASSO Cox</strong></li>
  <li>Unbiased hazard ratio estimates without shrinkage bias &rarr; Use <strong>SCAD/MCP Cox</strong></li>
</ul>

<p><strong>Step 5: Event checks (The EPV Rule)</strong></p>
<ul>
  <li>&lt; 10 events &rarr; Penalized regression is likely unreliable. Use Kaplan-Meier or univariate Cox.</li>
  <li>10&ndash;50 events &rarr; Use LASSO or Elastic Net with caution. Let the module\'s Suitability Assessment guide you.</li>
  <li>&gt; 50 events &rarr; Any method above is statistically appropriate.</li>
</ul>

<h3>Quick Comparison Table</h3>

<table>
<thead>
  <tr>
    <th>Feature</th>
    <th class="center">LASSO</th>
    <th class="center">Adaptive</th>
    <th class="center">Elastic Net</th>
    <th class="center">Grouped</th>
    <th class="center">SCAD/MCP</th>
    <th class="center">PCA/PLS</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>Drops variables</td>
    <td class="center">Yes</td>
    <td class="center">Yes</td>
    <td class="center">Yes</td>
    <td class="center">Yes</td>
    <td class="center">Yes</td>
    <td class="center">No</td>
  </tr>
  <tr>
    <td>Handles heavy collinearity</td>
    <td class="center">No</td>
    <td class="center">No</td>
    <td class="center">Yes</td>
    <td class="center">By Group</td>
    <td class="center">No</td>
    <td class="center">Yes</td>
  </tr>
  <tr>
    <td>Unbiased coefficient estimates</td>
    <td class="center">No</td>
    <td class="center">Partial</td>
    <td class="center">No</td>
    <td class="center">No</td>
    <td class="center">Yes</td>
    <td class="center">N/A</td>
  </tr>
  <tr>
    <td>High-dimensional (p &gt; n)</td>
    <td class="center">Yes</td>
    <td class="center">Yes</td>
    <td class="center">Yes</td>
    <td class="center">Yes</td>
    <td class="center">Yes</td>
    <td class="center">Yes</td>
  </tr>
  <tr>
    <td>Oracle consistency</td>
    <td class="center">No</td>
    <td class="center">Yes</td>
    <td class="center">No</td>
    <td class="center">No</td>
    <td class="center">Yes</td>
    <td class="center">No</td>
  </tr>
  <tr>
    <td>Suggested minimum events</td>
    <td class="center">~40</td>
    <td class="center">~60</td>
    <td class="center">~40</td>
    <td class="center">~50</td>
    <td class="center">~60</td>
    <td class="center">~20</td>
  </tr>
</tbody>
</table>

</div>')
        },

        .clinicalScenariosHtml = function() {
            paste0(private$.cssBlock(),
'<div class="lg">

<h3>Clinical Scenarios</h3>

<h4>Pathologist: Building a Prognostic Model from Histopathological Features</h4>
<p><strong>Scenario:</strong> You have 200 breast cancer patients with 20 histopathological variables
(grade, tumor size, LVI, PNI, ER/PR/HER2/Ki-67, etc.) and want to identify predictors of overall survival.</p>
<p><strong>Recommended: LASSO Cox</strong></p>
<p>You have more patients than variables, and you want to screen which features matter. LASSO automatically drops redundant variables. If LASSO drops one of two highly correlated features (e.g., size vs. T-stage), you can switch to Elastic Net if you strictly want to keep them both.</p>
<hr>

<h4>Pathologist: Clustered Genes or Multi-Panel IHCs</h4>
<p><strong>Scenario:</strong> You ran an IHC panel containing marker categories (Immune Markers: CD3, CD8, PDL1; Proliferation: Ki-67, PHH3; Stromal: SMA, Col1). You want the model to keep or drop whole groups representing biological pathways.</p>
<p><strong>Recommended: Group LASSO Cox</strong></p>
<p>Group LASSO will shrink categories together. Instead of dropping CD3 but keeping CD8, it will see if the entire "Immune" panel adds prognostic value, and drop the entire "Stromal" panel if it does not.</p>
<hr>

<h4>Radiologist: Radiomic Feature Selection from CT/MRI</h4>
<p><strong>Scenario:</strong> You extracted 500 radiomic features from CT scans of 120 lung cancer patients and want a predictive signature.</p>
<p><strong>Recommended: Elastic Net Cox, PCA Cox, or PLS Cox</strong></p>
<p>With p = 500 &gt;&gt; n = 120, standard LASSO can be unstable and arbitrarily drop collinear textures. Elastic Net handles collinearity. Alternatively, PCA or PLS Cox create composite radiomic "signatures" (components) representing the overall textural variance instead of throwing away features.</p>
<hr>

<h4>Oncologist: Identifying Biomarkers from a Gene Panel</h4>
<p><strong>Scenario:</strong> You have expression data for 50 genes in 300 colorectal cancer patients, and want accurate hazard ratios to build a predictive nomogram.</p>
<p><strong>Recommended: Adaptive LASSO Cox, then SCAD/MCP Cox</strong></p>
<p>With plenty of events and variables, you can use Adaptive LASSO for consistent variable selection (Oracle property). For nomogram construction, SCAD/MCP prevents standard LASSO\'s "shrinkage bias", giving you unbiased hazard ratio estimates for the important genes.</p>
<hr>

<h4>Clinician: Multivariable Prognostic Model with Standard Clinical Variables</h4>
<p><strong>Scenario:</strong> You have 150 gastric cancer patients with 12 standard clinical variables (age, stage, BMI, LVI) and want to build a simple prognostic scoring system.</p>
<p><strong>Recommended: Standard Multivariable Cox (or Data Suitability Guide)</strong></p>
<p>If you run LASSO Cox and the Data Suitability Report tells you there are "Adequate Events (EPV &gt; 10)", you often do not need LASSO at all. Standard "Multivariable Survival" will give you valid p-values and confidence intervals without penalizing the parameters.</p>

</div>')
        },

        .assumptionsHtml = function() {
            paste0(private$.cssBlock(),
'<div class="lg">

<h3>Key Assumptions (All Penalized Cox Methods)</h3>

<table>
  <tr>
    <td class="vtop"><strong>Proportional hazards</strong></td>
    <td>The hazard ratio between groups must remain constant over time.
      Violation is common with immunotherapy data (delayed treatment effects) or
      when comparing different tumor subtypes.
      Check with Schoenfeld residuals in standard Cox regression first.</td>
  </tr>
  <tr>
    <td class="vtop"><strong>Sufficient events</strong></td>
    <td>The events-per-variable (EPV) ratio should be at least 2 for LASSO
      (vs. 10&ndash;20 for standard Cox). With fewer events, results become unreliable
      regardless of method. The LASSO Cox suitability assessment reports EPV automatically.</td>
  </tr>
  <tr>
    <td class="vtop"><strong>Non-informative censoring</strong></td>
    <td>Patients lost to follow-up should not differ systematically from those
      who remain. If sicker patients drop out earlier, results will be biased.</td>
  </tr>
  <tr>
    <td class="vtop"><strong>Linear predictor effects</strong></td>
    <td>All penalized Cox methods assume linear relationships between
      continuous predictors and log-hazard. Non-linear effects (e.g., J-shaped
      BMI-mortality curve) need to be pre-specified using categorization or
      spline terms.</td>
  </tr>
</table>

<h3>Common Pitfalls</h3>

<p><strong>1. Interpreting LASSO coefficients as hazard ratios.</strong>
LASSO shrinks coefficients toward zero, so exp(coefficient) underestimates
the true hazard ratio. For publishable HRs, refit a standard Cox model with only
the LASSO-selected variables, or use SCAD/MCP Cox which does not shrink large effects.</p>

<p><strong>2. Using LASSO when standard Cox is sufficient.</strong>
If you have 200 patients, 40 events, and 5 well-established clinical variables,
standard multivariable Cox regression is better. It gives you p-values, confidence
intervals, and straightforward hazard ratios. LASSO adds complexity without benefit
when the events-per-variable ratio is already adequate (&ge; 10).</p>

<p><strong>3. Not validating the model.</strong>
LASSO uses internal cross-validation to choose lambda, but this does not
validate the model itself. Always report the optimism-corrected C-index
(available in ClinicoPath Survival Analysis with bootstrap validation) or use an
external validation cohort before drawing clinical conclusions.</p>

<p><strong>4. Ignoring multicollinearity warnings.</strong>
When predictors are correlated (e.g., tumor size and T-stage), standard LASSO
arbitrarily picks one and drops the other. The selected variable may change between
bootstrap samples. If the suitability report flags multicollinearity, switch to
Elastic Net Cox or remove redundant variables.</p>

<p><strong>5. Overfitting with small samples.</strong>
Even with penalization, LASSO can overfit when events are scarce. A C-index of 0.85
in a dataset with 30 events is almost certainly overfitted. Look at the gap between
apparent and cross-validated performance in the model output.</p>

</div>')
        },

        .glossaryHtml = function() {
            paste0(private$.cssBlock(),
'<div class="lg">

<h3>Glossary of Key Terms</h3>

<table>
  <tr>
    <th class="vtop">Term</th>
    <th>Plain Language Explanation</th>
  </tr>
  <tr>
    <td><strong>LASSO</strong></td>
    <td>Least Absolute Shrinkage and Selection Operator. Simultaneously
      selects important variables and shrinks less important ones to exactly zero
      (effectively removing them).</td>
  </tr>
  <tr>
    <td><strong>Penalization / Regularization</strong></td>
    <td>Adding a &ldquo;cost&rdquo; for including variables in the model. Prevents
      overfitting when you have many candidate predictors.</td>
  </tr>
  <tr>
    <td><strong>Lambda (&lambda;)</strong></td>
    <td>Controls how aggressively variables are removed. Higher lambda = fewer variables
      kept. Chosen automatically by cross-validation.</td>
  </tr>
  <tr>
    <td><strong>Group / Sparse Group Penalty</strong></td>
    <td>Instead of evaluating variables alone, evaluates them by a factor group ID or clustering logic, preserving biological/categorical relationships collectively.</td>
  </tr>
  <tr>
    <td><strong>C-index (Concordance Index)</strong></td>
    <td>The probability that a patient with higher predicted risk dies sooner than a randomly paired patient with lower predicted risk. In all these modules, it is calculating an "in-sample" internal C-index which may suffer optimism bias.</td>
  </tr>
  <tr>
    <td><strong>Events-per-variable (EPV)</strong></td>
    <td>Number of outcome events divided by the number of candidate predictors. Included in the built-in Data Suitability checks across the penalized Cox modules.</td>
  </tr>
  <tr>
    <td><strong>Shrinkage bias</strong></td>
    <td>LASSO pushes coefficients toward zero, making hazard ratios artificially closer to 1.0.
      SCAD and MCP penalties avoid this specifically for large coefficients.</td>
  </tr>
  <tr>
    <td><strong>PCA / PLS</strong></td>
    <td>Instead of selecting variables, dimensionality reduction creates composite scores
      (components). Principal Components (PCA) explains the data\'s variance, while Partial Least Squares (PLS) components explain variance <i>directed by survival outcomes</i>.</td>
  </tr>
</table>

</div>')
        }

    )
)
