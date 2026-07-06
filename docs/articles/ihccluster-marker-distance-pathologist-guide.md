# IHC Marker Clustering for Pathologists: Distance Metrics Guide

## IHC Marker Clustering for Pathologists: Distance Metrics Guide

**A Practical Guide to Understanding How Markers Cluster Together**

------------------------------------------------------------------------

### Why Cluster IHC Markers?

As pathologists, we use IHC panels daily to make differential diagnoses.
But have you ever wondered:

- **Which markers provide redundant information?** (Is adding CK20
  really helpful if you already have CDX2?)
- **Which markers tend to co-express?** (Do ER+ tumors always show PR
  positivity?)
- **What’s the minimal panel for a diagnosis?** (Can I drop 2-3 markers
  without losing diagnostic power?)
- **Are there unexpected marker associations?** (Hidden patterns in your
  dataset)

**Marker clustering answers these questions** by grouping IHC markers
based on their expression patterns across your cases.

------------------------------------------------------------------------

### The Basics: Patient Clustering vs. Marker Clustering

#### Patient Clustering (What You Already Know)

- **Question:** “Which patients have similar IHC profiles?”
- **Example:** Clustering breast cancer cases by ER/PR/HER2/Ki67
- **Output:** Luminal A, Luminal B, HER2+, Triple-negative groups

#### Marker Clustering (What’s New) ⭐

- **Question:** “Which IHC markers behave similarly?”
- **Example:** Do CK7 and TTF1 tend to be positive in the same cases?
- **Output:** Marker groups showing co-expression or redundancy

------------------------------------------------------------------------

### The 9 Distance Metrics: A Pathologist’s Guide

#### Quick Selection Flowchart

    START HERE
    ↓
    What type of IHC data do you have?
    ↓
    ├─ Binary (Positive/Negative only)?
    │  ├─ Many negatives? → Use JACCARD
    │  └─ Balanced +/-? → Use CHI-SQUARED ⭐
    │
    ├─ Ordinal (0/1+/2+/3+ intensity)?
    │  └─ → Use CHI-SQUARED ⭐ or HAMMING
    │
    ├─ Continuous (H-scores, % positive)?
    │  ├─ Normally distributed? → Use EUCLIDEAN ⭐
    │  └─ Have outliers? → Use MANHATTAN
    │
    └─ Mixed (Binary + Continuous)?
       ├─ Want automatic handling? → Use MIXED ⭐
       └─ Expect non-linear patterns? → Use MUTUAL INFORMATION

------------------------------------------------------------------------

### Detailed Clinical Scenarios

#### Scenario 1: Lung Adenocarcinoma vs. Squamous Cell Carcinoma

**Your Panel:** TTF1, Napsin A, p40, CK5/6 (all binary: pos/neg)

##### 👉 Recommended: **Chi-squared Distance**

**Why this works:** - Chi-squared tests if two markers are statistically
independent - Perfect for binary/categorical IHC data - Provides
statistical significance (p-values) - Well-established in pathology
literature

**Expected Results:**

    Marker Group 1: TTF1 + Napsin A (adenocarcinoma markers)
    ├─ Strong association (p < 0.001)
    ├─ Cramér's V = 0.72 (strong effect)
    └─ Interpretation: These markers are redundant

    Marker Group 2: p40 + CK5/6 (squamous markers)
    ├─ Strong association (p < 0.001)
    ├─ Cramér's V = 0.68 (strong effect)
    └─ Interpretation: These markers are redundant

    Distance between groups: High
    └─ Interpretation: Groups are mutually exclusive (good!)

**Clinical Implications:** - ✅ **Keep:** One marker from each group
(e.g., TTF1 + p40) - ❌ **Consider dropping:** Napsin A if TTF1 is
positive (provides redundant info) - 💰 **Cost savings:** Reduced
antibody usage without losing diagnostic accuracy

------------------------------------------------------------------------

#### Scenario 2: Breast Cancer Immunoprofile

**Your Panel:** - Binary: ER (pos/neg), PR (pos/neg), HER2 (pos/neg) -
Continuous: Ki67 (% positive, 0-100%)

##### 👉 Recommended: **Mixed Distance**

**Why this works:** - Automatically handles mixed data types - Uses
chi-squared for ER-PR-HER2 pairs - Uses correlation for Ki67
relationships - No manual method selection needed

**Expected Results:**

    Marker Group 1: ER + PR
    ├─ Chi-squared test: p < 0.001
    ├─ Co-expression rate: 85%
    └─ Interpretation: Strong positive association

    ER-Ki67 Relationship:
    ├─ Eta-squared (ANOVA): 0.42
    ├─ ER+ tumors: Ki67 mean = 15%
    └─ ER- tumors: Ki67 mean = 45%
        Interpretation: ER- tumors have higher proliferation

    HER2: Independent marker
    ├─ Low association with ER/PR (p > 0.05)
    └─ Interpretation: Provides unique diagnostic information

**Clinical Implications:**

    Molecular Subtype Prediction:
    ┌─────────────────────────────────────────┐
    │ Luminal A:   ER+/PR+, HER2-, Ki67 <20%  │  ← ER-PR-Ki67 cluster
    │ Luminal B:   ER+/PR+, HER2-, Ki67 >=20%  │  ← ER-PR-Ki67 cluster
    │ HER2-enriched: HER2+ (regardless of ER) │  ← HER2 independent
    │ Triple-negative: ER-/PR-/HER2-          │  ← All separate
    └─────────────────────────────────────────┘

**Actionable Insights:** - ER and PR tend to co-express (85%
concordance) - When ER is negative, PR rarely adds new information -
Ki67 shows inverse relationship with ER status - HER2 provides
independent prognostic information

------------------------------------------------------------------------

#### Scenario 3: Gastrointestinal Tumor Panel

**Your Panel:** CK7, CK20, CDX2, SATB2 (all binary) **Question:** Is
CDX2 necessary if I already have CK20 and SATB2?

##### 👉 Recommended: **Jaccard Distance**

**Why this works:** - GI tumors often have many negative markers (sparse
data) - Jaccard focuses on co-positivity, ignores double-negatives -
Clinically relevant: we care about co-expression of positive markers -
Simple interpretation for binary data

**Example Output:**

    Jaccard Similarity Matrix:
               CK7   CK20  CDX2  SATB2
    CK7       1.00  0.15  0.12  0.08
    CK20      0.15  1.00  0.68  0.45
    CDX2      0.12  0.68  1.00  0.52
    SATB2     0.08  0.45  0.52  1.00

**Converting to Distance:**

    Jaccard Distance = 1 - Jaccard Similarity

               CK7   CK20  CDX2  SATB2
    CK7       0.00  0.85  0.88  0.92  ← CK7 is distant from others
    CK20      0.85  0.00  0.32  0.55  ← CK20-CDX2 are close (0.32)
    CDX2      0.88  0.32  0.00  0.48  ← CDX2-SATB2 are close (0.48)
    SATB2     0.92  0.55  0.48  0.00

**Dendrogram Interpretation:**

             CK7 ─────────────────────┐
                                       ├──── Distinct group
             CK20 ──┐                  │
                    ├────┐             │
             CDX2 ──┘    ├─────────────┘
                         │
             SATB2 ──────┘

    Legend:
    ├─ Short branch = similar markers (redundant)
    └─ Long branch = distinct markers (keep separate)

**Clinical Decision:**

    Marker Group 1 (Lower GI): CK20, CDX2, SATB2
    ├─ CK20-CDX2 distance = 0.32 (very similar!)
    ├─ CDX2-SATB2 distance = 0.48 (moderately similar)
    └─ Recommendation: Could use CDX2 OR CK20 (not both)

    Marker Group 2 (Upper GI): CK7
    ├─ Distant from all others (0.85-0.92)
    └─ Recommendation: Must keep (provides unique info)

    Optimized Panel Suggestion:
    ✅ Keep: CK7 + CDX2 + SATB2
    ❌ Consider dropping: CK20 (redundant with CDX2 in colon tumors)

**Real Case Examples:**

    Case 1: Colon Adenocarcinoma
    CK7[-], CK20[+], CDX2[+], SATB2[+]
    └─ CK20 and CDX2 both positive (redundant in this case)

    Case 2: Cholangiocarcinoma
    CK7[+], CK20[+], CDX2[-], SATB2[-]
    └─ CK7 provides diagnostic value (different from CK20)

    Case 3: Pancreatic Adenocarcinoma
    CK7[+], CK20[+], CDX2[+], SATB2[-]
    └─ Mixed pattern: need multiple markers

------------------------------------------------------------------------

#### Scenario 4: Melanoma Marker Panel

**Your Panel:** S100, SOX10, Melan-A, HMB45 (all intensity: 0/1+/2+/3+)
**Question:** Which markers provide similar information?

##### 👉 Recommended: **Hamming Distance**

**Why this works:** - Counts how often markers disagree in intensity -
Intuitive for pathologists (we think in terms of concordance) - Simple
calculation: % of cases where markers differ - Works well with intensity
scoring

**Example Calculation:**

| Case | S100 | SOX10 | Melan-A | HMB45 | S100=SOX10? | S100=Melan-A? |
|------|------|-------|---------|-------|-------------|---------------|
| 1    | 3+   | 3+    | 2+      | 1+    | ✅ Yes      | ❌ No         |
| 2    | 3+   | 3+    | 3+      | 2+    | ✅ Yes      | ✅ Yes        |
| 3    | 2+   | 3+    | 1+      | 0     | ❌ No       | ❌ No         |
| 4    | 3+   | 2+    | 2+      | 0     | ❌ No       | ❌ No         |
| 5    | 0    | 0     | 0       | 0     | ✅ Yes      | ✅ Yes        |

**Hamming Distance:**

    S100 vs SOX10:   2/5 disagree = 0.40 distance (60% concordance)
    S100 vs Melan-A: 3/5 disagree = 0.60 distance (40% concordance)
    S100 vs HMB45:   4/5 disagree = 0.80 distance (20% concordance)

    Interpretation:
    - S100 and SOX10 are most similar (lowest distance)
    - HMB45 behaves differently (highest distance)

**Clinical Pattern:**

    Nuclear Markers:    S100 ─┬─ High concordance
                        SOX10─┘  (distance = 0.40)

    Cytoplasmic Markers: Melan-A ─┬─ Moderate concordance
                          HMB45 ───┘  (distance varies)

    Distance between groups: High
    └─ Nuclear vs cytoplasmic markers show different patterns

**Practical Recommendations:**

    For melanoma confirmation:
    1. Start with: S100 (sensitive) + HMB45 (specific)
    2. If equivocal: Add SOX10 (nuclear backup)
    3. Reserve Melan-A for: Cytoplasmic confirmation

    Panel Rationalization:
    ✅ Keep: S100 + HMB45 (different sensitivity/specificity profiles)
    ⚠️ Optional: SOX10 (adds value if S100 equivocal)
    ⚠️ Optional: Melan-A (different localization, useful for epithelioid)

------------------------------------------------------------------------

#### Scenario 5: Lymphoma Immunoprofile

**Your Panel:** - Binary: CD20, CD3, CD5, CD10, BCL6 - Continuous: Ki67
(% positive)

**Specific Question:** Do germinal center markers (CD10, BCL6) always
co-express?

##### 👉 Recommended: **Chi-squared Distance** + Statistical Testing

**Example Results:**

    ═══════════════════════════════════════════════════════
    MARKER-MARKER ASSOCIATION TESTS
    ═══════════════════════════════════════════════════════

    CD10 vs BCL6:
    ├─ Chi-squared = 42.3, p < 0.001
    ├─ Cramér's V = 0.68 (strong association)
    ├─ Concordance table:
    │
    │            BCL6-  BCL6+
    │   CD10-    45     8      ← 85% concordance when both negative
    │   CD10+    5      32     ← 86% concordance when CD10+
    │
    └─ Interpretation: Strong co-expression (germinal center phenotype)

    CD5 vs CD10:
    ├─ Chi-squared = 38.7, p < 0.001
    ├─ Cramér's V = -0.72 (strong negative association)
    ├─ Concordance table:
    │
    │            CD10-  CD10+
    │   CD5-     12     35     ← CD5- tumors often CD10+
    │   CD5+     41     2      ← CD5+ tumors rarely CD10+
    │
    └─ Interpretation: Mutually exclusive (mantle vs. GC origin)

    CD20 vs CD3:
    ├─ Chi-squared = 87.5, p < 0.001
    ├─ Perfect separation (B-cell vs T-cell)
    └─ Interpretation: Lineage markers (must keep both)

**Dendrogram Interpretation:**

    Germinal Center Group:
        CD10 ──┬── Very close (distance = 0.32)
        BCL6 ──┘    Co-expression in DLBCL-GCB, FL

    B-cell Lineage:
        CD20 ─────── Separate from others

    Mantle/Non-GC:
        CD5 ──────── Opposite of CD10/BCL6

    T-cell Lineage:
        CD3 ──────── Completely separate

**Clinical Algorithm:**

    Lymphoma Workup:
    ┌─────────────────────────────────────────────────┐
    │ Step 1: Lineage (Required)                      │
    │   → CD20 (B-cell) vs CD3 (T-cell)              │
    │   → Must keep both (mutually exclusive)         │
    └─────────────────────────────────────────────────┘
               ↓
    ┌─────────────────────────────────────────────────┐
    │ Step 2: B-cell Subclassification                │
    │   → CD10 + BCL6 = Germinal center origin       │
    │   → CD5 = Mantle/Non-GC origin                 │
    │   → Pattern: CD10/BCL6 cluster vs CD5 separate │
    └─────────────────────────────────────────────────┘
               ↓
    ┌─────────────────────────────────────────────────┐
    │ Step 3: Proliferation                           │
    │   → Ki67 (continuous variable)                  │
    │   → Independent of other markers                │
    └─────────────────────────────────────────────────┘

    Optimized Panel:
    ✅ Must keep: CD20, CD3 (lineage)
    ✅ Must keep: CD10 OR BCL6 (one from GC cluster)
    ✅ Must keep: CD5 (non-GC)
    ✅ Must keep: Ki67 (proliferation)

    Could eliminate: One of CD10/BCL6 (redundant in most cases)

------------------------------------------------------------------------

#### Scenario 6: Neuroendocrine Tumor Grading

**Your Panel:** - Binary: Chromogranin, Synaptophysin, CD56 -
Continuous: Ki67 % (for grading G1/G2/G3) - Ordinal: p53 intensity
(0/1+/2+/3+)

**Question:** Can I predict Ki67 level from other markers?

##### 👉 Recommended: **Mutual Information Distance**

**Why this works:** - Captures non-linear relationships - Works with
mixed data types - Detects patterns like “Ki67 high ONLY when p53 is
strong positive” - Information-theoretic approach (model-free)

**Example Results:**

    ═══════════════════════════════════════════════════════
    MUTUAL INFORMATION ANALYSIS
    ═══════════════════════════════════════════════════════

    Marker Pair: p53 (ordinal) vs Ki67 (continuous)
    ├─ Mutual Information = 0.62 bits
    ├─ Normalized MI = 0.58 (moderate information sharing)
    ├─
    ├─ Breakdown by p53 intensity:
    │   p53 0/1+  : Ki67 mean = 3.2%  (σ = 2.1)  ← G1 tumors
    │   p53 2+    : Ki67 mean = 12.5% (σ = 5.3)  ← G2 tumors
    │   p53 3+    : Ki67 mean = 48.7% (σ = 18.2) ← G3 tumors
    │
    └─ Interpretation: Strong non-linear relationship
        (High p53 predicts high Ki67, but not vice versa)

    Marker Pair: Chromogranin vs Ki67
    ├─ Mutual Information = 0.12 bits
    ├─ Normalized MI = 0.08 (weak information sharing)
    └─ Interpretation: Independent markers
        (Chromogranin doesn't predict proliferation)

    Marker Pair: Chromogranin vs Synaptophysin
    ├─ Mutual Information = 0.78 bits
    ├─ Normalized MI = 0.85 (strong information sharing)
    ├─ Co-expression: 92% of cases
    └─ Interpretation: Redundant markers
        (One is sufficient for NET diagnosis)

**Clinical Decision Tree:**

    Neuroendocrine Tumor Confirmation:
        ├─ Chromogranin [+] ──┐
        └─ Synaptophysin [+] ─┴─→ High MI (redundant)
             │
             → Recommendation: Use ONE for diagnosis

    Grading (Non-linear pattern detected):
        ├─ Ki67 < 3%  + p53 0/1+ → Grade 1 (Low-grade)
        ├─ Ki67 3-20% + p53 2+   → Grade 2 (Intermediate)
        └─ Ki67 > 20% + p53 3+   → Grade 3 (High-grade)
             │
             → Pattern: p53 intensity increases WITH Ki67
             → MI detects this non-linear association
             → Linear correlation would miss this pattern!

**Comparison: MI vs. Correlation:**

    Using Standard Correlation (Linear):
    p53 vs Ki67 correlation = 0.42
    └─ Would conclude: "Moderate positive correlation"
       (Misses the threshold effects!)

    Using Mutual Information (Non-linear):
    p53 vs Ki67 normalized MI = 0.58
    └─ Detects: "Strong information sharing with thresholds"
       (Captures the G1/G2/G3 transition points!)

    Clinical Impact:
    ✅ MI correctly identifies: p53 3+ predicts high Ki67
    ❌ Correlation misses: The threshold nature of grading

------------------------------------------------------------------------

#### Scenario 7: Prostate Cancer Markers

**Your Panel:** - PSA (H-score: 0-300) - NKX3.1 (H-score: 0-300) - ERG
(H-score: 0-300) - PTEN (% loss: 0-100%)

**Question:** Do ERG+ cases have different marker patterns?

##### 👉 Recommended: **Euclidean Distance** (continuous data)

**Why this works:** - All markers are continuous (H-scores,
percentages) - Euclidean distance is the standard for continuous data -
Captures magnitude differences (important for H-scores) - Auto-scaled to
handle different ranges

**Example Analysis:**

    ═══════════════════════════════════════════════════════
    EUCLIDEAN DISTANCE MATRIX (scaled)
    ═══════════════════════════════════════════════════════

                PSA    NKX3.1  ERG    PTEN
    PSA         0.00   0.45    1.28   0.89
    NKX3.1      0.45   0.00    1.35   0.92
    ERG         1.28   1.35    0.00   0.58
    PTEN        0.89   0.92    0.58   0.00

    Interpretation:
    ├─ PSA-NKX3.1: Close (0.45) → Often co-expressed
    ├─ ERG-PTEN: Moderate (0.58) → Some association
    └─ ERG vs PSA/NKX3.1: Far (1.28-1.35) → Independent

**Dendrogram Shows:**

    Prostate Lineage Markers:
        PSA ───┬─── Distance = 0.45 (high concordance)
        NKX3.1─┘     Both are prostate-specific

    Molecular Subtype Markers:
        ERG ───┬─── Distance = 0.58
        PTEN──┘     TMPRSS2-ERG fusion-related

    Groups are distant from each other (1.28-1.35)
    └─ Lineage markers independent from molecular markers

**Clinical Patterns (Revealed by Clustering):**

    Pattern 1: Prostatic Adenocarcinoma (ERG-)
    ┌────────────────────────────────────────┐
    │ PSA:    High H-score (mean = 245)     │  ← Lineage markers
    │ NKX3.1: High H-score (mean = 238)     │     cluster together
    │ ERG:    Negative (H-score = 0)        │  ← Molecular markers
    │ PTEN:   Intact (loss = 5%)            │     separate cluster
    └────────────────────────────────────────┘

    Pattern 2: Prostatic Adenocarcinoma (ERG+)
    ┌────────────────────────────────────────┐
    │ PSA:    High H-score (mean = 232)     │  ← Lineage markers
    │ NKX3.1: High H-score (mean = 215)     │     still positive
    │ ERG:    Positive (H-score = 180)      │  ← ERG fusion present
    │ PTEN:   Loss (loss = 65%)             │     Often with PTEN loss
    └────────────────────────────────────────┘
                        ↑
            ERG-PTEN co-occurrence (moderate distance = 0.58)

    Pattern 3: High-grade Prostate Cancer
    ┌────────────────────────────────────────┐
    │ PSA:    Low H-score (mean = 85)       │  ← Lineage marker loss
    │ NKX3.1: Low H-score (mean = 92)       │     (dedifferentiation)
    │ ERG:    Variable                       │
    │ PTEN:   Loss (loss = 78%)             │  ← High-grade feature
    └────────────────────────────────────────┘

**Actionable Insights:**

    1. Diagnostic Panel (Metastatic site):
       ✅ PSA + NKX3.1 = Redundant (distance = 0.45)
       → Use ONE for prostate lineage

    2. Prognostic Panel:
       ✅ ERG + PTEN = Complementary (distance = 0.58)
       → ERG: Fusion-positive subtype
       → PTEN: Aggressive disease marker
       → Keep both (moderate association but not redundant)

    3. Dedifferentiation Detection:
       → If PSA + NKX3.1 both low → High-grade features
       → Euclidean distance captures MAGNITUDE loss

------------------------------------------------------------------------

#### Scenario 8: Renal Cell Carcinoma Panel

**Your Panel:** - PAX8, RCC, CD10, CK7, Vimentin - All binary
(pos/neg) - **Many negative stains expected** (sparse data)

##### 👉 Recommended: **Jaccard Distance** (focus on co-positivity)

**Why Jaccard over Chi-squared for RCC:**

    Consider a 100-case dataset:

    Using Chi-squared:
    ├─ Includes double-negatives in calculation
    ├─ PAX8[-]/CK7[-] cases (90 cases) → counted as "agreement"
    └─ Can inflate association when many negatives

    Using Jaccard:
    ├─ Ignores double-negatives
    ├─ Focuses on: PAX8[+]/CK7[+] co-positivity
    └─ Clinically relevant: we care about positive co-expression

**Example Results:**

    ═══════════════════════════════════════════════════════
    JACCARD ANALYSIS (Co-positivity Focus)
    ═══════════════════════════════════════════════════════

    Clear Cell RCC Pattern:
    PAX8 vs RCC marker:
    ├─ Both positive: 68/100 cases
    ├─ Either positive: 72/100 cases
    ├─ Jaccard Index = 68/72 = 0.94 (very high!)
    └─ Interpretation: Nearly always co-express in ccRCC

    PAX8 vs CD10:
    ├─ Both positive: 65/100 cases
    ├─ Either positive: 70/100 cases
    ├─ Jaccard Index = 65/70 = 0.93
    └─ Interpretation: Strong co-expression in ccRCC

    PAX8 vs CK7:
    ├─ Both positive: 8/100 cases
    ├─ Either positive: 78/100 cases
    ├─ Jaccard Index = 8/78 = 0.10 (very low!)
    └─ Interpretation: Rarely co-express (mutually exclusive)

    Vimentin:
    ├─ Positive in most RCCs (non-specific)
    ├─ Jaccard with others: 0.60-0.70
    └─ Interpretation: Sensitive but not specific

**Dendrogram Interpretation:**

    Clear Cell RCC Cluster:
        PAX8 ──┬── Jaccard distance = 0.06 (nearly identical)
        RCC ───┤
               ├── All cluster tightly
        CD10 ──┘   (co-positive in ccRCC)

    Papillary RCC Pattern:
        CK7 ──────── Separate (low Jaccard with ccRCC markers)

    Non-specific:
        Vimentin ─── Moderate distance from all

**Clinical Decision Algorithm:**

    RCC Subtype Differentiation:
    ┌────────────────────────────────────────────────┐
    │ Clear Cell RCC:                                 │
    │   PAX8[+], RCC[+], CD10[+], CK7[-], Vim[+]    │
    │                                                 │
    │   Marker Cluster: PAX8-RCC-CD10                │
    │   └─ High Jaccard (0.93-0.94) = Redundant!     │
    │                                                 │
    │   Minimal Panel: PAX8 + CK7                    │
    │   └─ PAX8[+]/CK7[-] → Likely ccRCC            │
    └────────────────────────────────────────────────┘

    ┌────────────────────────────────────────────────┐
    │ Papillary RCC:                                  │
    │   PAX8[+], RCC[+/-], CD10[-/+], CK7[+], Vim[+]│
    │                                                 │
    │   Key Discriminator: CK7                       │
    │   └─ Low Jaccard with ccRCC markers (0.10)     │
    │                                                 │
    │   Minimal Panel: PAX8 + CK7                    │
    │   └─ PAX8[+]/CK7[+] → Likely pRCC             │
    └────────────────────────────────────────────────┘

    Cost Optimization:
    ✅ Keep: PAX8 (renal lineage), CK7 (subtype discriminator)
    ❌ Drop: RCC marker (redundant with PAX8, Jaccard = 0.94)
    ❌ Drop: CD10 (redundant with PAX8, Jaccard = 0.93)
    ⚠️ Optional: Vimentin (non-specific, moderate Jaccard)

    Potential Savings:
    ├─ 2-3 antibodies per case
    ├─ Maintains diagnostic accuracy
    └─ Based on co-expression analysis

------------------------------------------------------------------------

### Distance Metric Properties: Quick Reference for Pathologists

#### When You Have SPARSE Binary Data (Many Negatives)

**Problem:**

    Example: Rare marker panel (BCL2, BCL6, MYC in lymphoma)

             BCL2-  BCL2+
    BCL6-    85     5      ← 85 double-negative cases
    BCL6+    3      7      ← Only 7 co-positive cases

    Chi-squared: "Strong association" (p < 0.001)
    └─ Driven by 85 double-negative cases!

**Solution: Use Jaccard**

    Jaccard Index = 7 / (7+5+3) = 7/15 = 0.47
    └─ Focuses on the 15 cases with ANY positivity
    └─ Clinically meaningful: 47% co-positivity rate

------------------------------------------------------------------------

#### When You Have OUTLIERS in Continuous Data

**Problem:**

    Ki67 % Distribution:
    Cases 1-98: Range 0-30% (typical)
    Case 99:    Ki67 = 95% (outlier)
    Case 100:   Ki67 = 2%

    Euclidean Distance (Case 99 vs Case 100):
    └─ Heavily penalizes this outlier (squared differences)
    └─ May distort entire clustering

**Solution: Use Manhattan**

    Manhattan Distance:
    └─ Uses absolute differences (not squared)
    └─ More robust to outliers
    └─ Better represents typical case relationships

------------------------------------------------------------------------

#### When Markers Have DIFFERENT SCALES

**Problem:**

    Marker A: Ki67 % (range 0-100)
    Marker B: p53 H-score (range 0-300)

    Without scaling:
    └─ p53 dominates distance calculation (larger numbers)

**Solution: Automatic Scaling**

    Both Euclidean and Manhattan:
    ✅ Automatically z-score normalize
    ✅ Each marker: mean = 0, SD = 1
    ✅ Fair comparison regardless of original scale

------------------------------------------------------------------------

#### When Relationship is NON-LINEAR

**Problem:**

    Linear Correlation Misses This Pattern:

    Ki67 vs Grade:
    Grade 1 (90 cases): Ki67 = 2% (SD = 1%)
    Grade 2 (8 cases):  Ki67 = 12% (SD = 3%)
    Grade 3 (2 cases):  Ki67 = 55% (SD = 15%)

    Pearson Correlation = 0.35 (weak!)
    └─ Linear assumption fails (step-wise relationship)

**Solution: Use Mutual Information**

    Mutual Information = 0.68 (strong!)
    └─ Captures the grade thresholds
    └─ No linearity assumption
    └─ Information-theoretic approach

------------------------------------------------------------------------

### Interpreting the Dendrogram: A Step-by-Step Guide

#### Anatomy of a Marker Dendrogram

                                     ┌─ Marker A
                            ┌────────┤
                     ┌──────┤        └─ Marker B
            ┌────────┤      │
            │        │      └─────────── Marker C
    ────────┤        │
            │        └────────────────── Marker D
            │
            └───────────────────────────  Marker E

            └────┴────┴────┴────┴────┘
             0   0.2  0.4  0.6  0.8  1.0
                  Distance (Y-axis)

#### Reading the Dendrogram (Left to Right)

1.  **Marker Names (X-axis bottom)**
    - Individual IHC markers
2.  **Height (Y-axis)**
    - Distance at which markers join
    - Lower = more similar
    - Higher = more different
3.  **Branches**
    - Markers joined by short vertical lines are similar
    - Long branches = markers are distinct
4.  **Groupings**
    - Markers clustering together share expression patterns

#### Clinical Example: GI Tumor Panel

    Real Dendrogram from Your Data:

    Height (Distance)
    │
    1.0│
       │                              ┌─ CK7
    0.8│                     ┌────────┤
       │                     │        └─ MUC6
    0.6│            ┌────────┤
       │            │        └─────────── MUC5AC
    0.4│    ┌───────┤
       │    │       └────────────────────  CEA
    0.2│────┤
       │    │       ┌──────────────────── CDX2
       │    └───────┤
    0.0│            └──────────────────── MUC2
       └────┴───────┴───────┴───────┴────
          CK7  MUC6  MUC5AC CEA  CDX2 MUC2

**Interpretation:**

**Group 1 (Height 0.2-0.4): Upper GI Markers**

    CK7 ─┬─ Gastric phenotype
    MUC6─┤  Distance = 0.2 (very similar)
         │
    MUC5AC (joins at 0.4)
    CEA (joins at 0.4)

    Clinical Meaning:
    ├─ CK7 + MUC6 almost always together in gastric tumors
    ├─ MUC5AC joins this group (still gastric)
    └─ CEA is related but less specific

    Recommendation:
    ✅ Keep CK7 (lineage marker)
    ❌ Could drop MUC6 (redundant with CK7, distance 0.2)
    ⚠️ Keep MUC5AC IF differentiating gastric vs pancreatobiliary

**Group 2 (Height 0.0-0.2): Lower GI Markers**

    CDX2─┬─ Intestinal phenotype
    MUC2─┘  Distance = 0.1 (nearly identical)

    Clinical Meaning:
    ├─ CDX2 and MUC2 co-express in colorectal adenocarcinoma
    └─ Very low distance = highly redundant

    Recommendation:
    ✅ Keep CDX2 (more sensitive and specific)
    ❌ Drop MUC2 (redundant, distance 0.1)

**Between Groups (Height 0.6-0.8):**

    Upper GI Group ←→ Lower GI Group
    Distance = 0.7 (very different)

    Clinical Meaning:
    ├─ Gastric vs intestinal phenotypes are distinct
    └─ Appropriate separation for differential diagnosis

    Recommendation:
    ✅ Panel structure is good
    ✅ Clear separation between phenotypes

------------------------------------------------------------------------

#### Red Flags in Your Dendrogram

##### ⚠️ Red Flag 1: No Separation Between Groups

    Bad Pattern:
    All markers cluster at distance 0.1-0.2
    └─ Everything is redundant!
    └─ Panel not optimized

    Solution:
    └─ Review marker selection
    └─ Add markers targeting different pathways/lineages

##### ⚠️ Red Flag 2: Unexpected Groupings

    Unexpected Pattern:
    TTF1 clusters with p40 (distance 0.15)

    Wait, what?!
    ├─ TTF1 = adenocarcinoma marker
    └─ p40 = squamous marker
        └─ Should be DISTANT!

    Possible Issues:
    1. Data entry error (swapped columns?)
    2. Mixed tumor population
    3. Unusual cohort (both markers negative in most cases)

    Action:
    └─ Review raw data before interpreting

##### ⚠️ Red Flag 3: Single Marker Far from All Others

    Isolated Pattern:
    Marker X at distance > 0.9 from everything

    Possible Explanations:
    1. ✅ Unique diagnostic marker (good!)
    2. ⚠️ Technical failure (all negative?)
    3. ⚠️ Wrong tissue type?

    Action:
    └─ Check that marker actually worked
    └─ Review positive/negative rates

------------------------------------------------------------------------

### Statistical Tests: Understanding the P-values

#### Chi-squared Test Output

    ═══════════════════════════════════════════════════════
    MARKER-MARKER ASSOCIATION: ER vs PR
    ═══════════════════════════════════════════════════════

    Contingency Table:
              PR-   PR+   Total
    ER-       45    12    57
    ER+       8     85    93
    Total     53    97    150

    Chi-squared statistic: χ² = 68.4
    Degrees of freedom: df = 1
    P-value: p < 0.001 ***

    Cramér's V: 0.675
    Effect size interpretation: Strong association

    Result: Statistically significant association
    Conclusion: ER and PR are NOT independent

**What This Means for Pathologists:**

    Interpretation:
    ├─ p < 0.001: Extremely unlikely due to chance
    ├─ Cramér's V = 0.675: Strong effect size
    ├─ Clinical: 85/93 (91%) of ER+ cases are also PR+
    └─ Decision: Markers are redundant in most cases

    When to keep both:
    ├─ Some ER+/PR- cases exist (8/93 = 9%)
    ├─ Prognostic significance (PR loss = worse prognosis)
    └─ Recommendation: Keep both for breast cancer

#### Cramér’s V Effect Size Guidelines

    For 2x2 Tables (binary markers):

    Cramér's V     Interpretation            Clinical Meaning
    ─────────────────────────────────────────────────────────
    0.00-0.10    Negligible association   Markers are independent
    0.10-0.30    Weak association         Slight relationship
    0.30-0.50    Moderate association     Notable relationship
    0.50-1.00    Strong association       Markers often co-express
    1.00           Perfect association      Always co-express

    Example:
    ├─ TTF1 vs Napsin A: V = 0.72 → Strong (redundant)
    ├─ ER vs PR:         V = 0.68 → Strong (related but keep both)
    └─ CK7 vs CK20:      V = 0.15 → Weak (independent, keep both)

#### P-value Interpretation (Conservative for Pathology)

    P-value     Interpretation             Action
    ──────────────────────────────────────────────────────
    p < 0.001   Very strong evidence       Confident association
                (99.9% confident)

    p < 0.01    Strong evidence            Likely association
                (99% confident)

    p < 0.05    Moderate evidence          Consider association
                (95% confident)            Requires clinical context

    p >= 0.05    Insufficient evidence      Assume independent
                Consider markers           Don't eliminate based
                independent                on clustering alone

**Pathology-Specific Caveat:**

    ⚠️ Statistical significance ≠ Clinical significance

    Example:
    ├─ Large dataset (n=1000): p = 0.001 but Cramér's V = 0.12
    └─ Statistically significant but clinically weak association
        → Don't eliminate markers based on p-value alone!

    Always check BOTH:
    ✅ P-value (statistical significance)
    ✅ Effect size (clinical relevance)

------------------------------------------------------------------------

### Common Mistakes to Avoid

#### Mistake 1: Dropping Markers Based Only on Clustering

    ❌ WRONG Approach:
    "CK7 and CK20 cluster together, so I'll drop CK20"

    ✅ CORRECT Approach:
    "CK7 and CK20 cluster together. Let me check:
     1. Do they truly co-express or are both negative?
     2. What's the Jaccard index for co-positivity?
     3. Do they mark different tumor types?
     4. Clinical: CK7+/CK20- (upper GI) vs CK7-/CK20+ (lower GI)
        → Actually mutually exclusive! Keep both!"

**Reality Check:**

    Clustering Result: "CK7 and CK20 are similar (distance 0.3)"

    Reason for clustering:
    ├─ Scenario A: Often BOTH negative in non-epithelial tumors
    │   └─ NOT redundant for epithelial tumor subtyping
    │
    └─ Scenario B: Both positive in transitional cell carcinoma
        └─ Might be redundant in specific tumor types

    Action:
    └─ Review the PATTERN, not just the distance!

#### Mistake 2: Using Wrong Distance for Data Type

    ❌ Euclidean distance for binary markers:
    ├─ Treats 0/1 as continuous numbers
    └─ Inappropriate (violates assumptions)

    ✅ Use Chi-squared or Jaccard for binary data

    ❌ Chi-squared for H-scores:
    ├─ Requires discretization (loses information)
    └─ Euclidean is better for continuous

    ✅ Use Euclidean or Manhattan for H-scores

#### Mistake 3: Ignoring Sample Size

    Small Dataset (n=20 cases):
    ═══════════════════════════════════════════
    CD10 vs BCL6: χ² = 8.5, p = 0.004
    Cramér's V = 0.65 (strong association!)

    Wait! Only 20 cases?
    ├─ Chi-squared may be unreliable (sparse cells)
    ├─ V = 0.65 could be unstable
    └─ Need Fisher's exact test for small samples

    Recommendation:
    ⚠️ n < 30: Be cautious with interpretation
    ⚠️ n < 50: Consider increasing sample size
    ✅ n > 100: Clustering results more reliable

#### Mistake 4: Not Considering Clinical Context

    Clustering Result:
    "p53 and Ki67 are independent (distance 0.8)"

    Statistical Interpretation:
    └─ Markers don't correlate in your dataset

    But Clinical Reality:
    ├─ p53 mutation → often drives high proliferation
    ├─ Biological link exists
    └─ Your cohort may not show this (selection bias)

    Action:
    ✅ Use clustering to INFORM decisions
    ❌ Don't ignore established biology

------------------------------------------------------------------------

### Step-by-Step Workflow for Panel Optimization

#### Phase 1: Data Preparation

    ✓ 1. Collect IHC data from your cases
         ├─ Minimum: 50-100 cases (more is better)
         └─ Include diverse diagnoses if differential panel

    ✓ 2. Organize data:
         ├─ Binary: Code as positive/negative or 0/1
         ├─ Ordinal: Keep intensity (0/1+/2+/3+) as is
         └─ Continuous: H-scores (0-300), % positive (0-100)

    ✓ 3. Check for missing data:
         ├─ Acceptable: < 10% missing per marker
         └─ If > 20% missing: Consider excluding that marker

#### Phase 2: Select Distance Metric

    ✓ 4. Choose metric based on data type:

         My markers are:
         ├─ All binary? → Chi-squared ⭐
         ├─ Binary with many negatives? → Jaccard
         ├─ All continuous? → Euclidean ⭐
         ├─ Mixed types? → Mixed ⭐ or Mutual Information
         └─ Ordinal (0/1+/2+/3+)? → Chi-squared or Hamming

#### Phase 3: Run Analysis

    ✓ 5. Perform marker clustering:
         ├─ Enable: "Perform Marker-Level Clustering"
         ├─ Set: Distance metric (from step 4)
         ├─ Enable: "Test Marker Associations"
         └─ Enable: "Auto-detect Marker Groups"

    ✓ 6. Review outputs:
         ├─ Dendrogram: Visual inspection
         ├─ Association table: P-values and effect sizes
         ├─ Clustering tree: Merge sequence
         └─ Marker groups: Identified clusters

#### Phase 4: Interpret Results

    ✓ 7. Identify redundant markers:

         Look for:
         ├─ Distance < 0.3: Very similar (consider dropping one)
         ├─ P < 0.001 AND Cramér's V > 0.6: Strong association
         └─ Jaccard > 0.8: High co-expression (redundant)

    ✓ 8. Identify complementary markers:

         Look for:
         ├─ Distance > 0.7: Different information (keep both)
         ├─ P > 0.05: Independent (keep both)
         └─ Mutually exclusive patterns (keep both)

#### Phase 5: Clinical Validation

    ✓ 9. Don't drop markers solely based on statistics!

         Clinical validation checklist:
         ├─ Does literature support this redundancy?
         ├─ Do these markers mark different subtypes?
         ├─ Would dropping this change diagnoses?
         ├─ Is this for prognostic vs diagnostic use?
         └─ Cost savings worth potential information loss?

    ✓ 10. Test optimized panel:
         ├─ Pilot with 20-30 new cases
         ├─ Compare diagnoses: Full panel vs optimized
         └─ If concordance > 95%, consider adopting

#### Phase 6: Implementation

    ✓ 11. Document your findings:
         ├─ Create internal protocol
         ├─ Share with pathology colleagues
         └─ Update ordering guidelines

    ✓ 12. Monitor performance:
         ├─ Track any discordant cases
         ├─ Re-evaluate annually
         └─ Adjust panel as needed

------------------------------------------------------------------------

### Real-World Panel Optimization Examples

#### Example 1: Lung Cancer Panel Reduction

**Original Panel (6 markers):** - TTF1, Napsin A, p40, CK5/6, p63, CK7

**Clustering Results:**

    Group 1 (Adenocarcinoma):
    TTF1 ─┬── Distance 0.18 (very redundant)
    Napsin─┘

    Group 2 (Squamous):
    p40 ──┬── Distance 0.15 (redundant)
    p63 ──┤
          └─ CK5/6 (distance 0.22, slightly different)

    Separate:
    CK7 (distance > 0.9 from all, non-specific)

**Clinical Decision:**

    Optimized Panel (3 markers):
    ✅ TTF1 (adenocarcinoma)
    ✅ p40 (squamous)
    ✅ CK7 (non-specific epithelial)

    Eliminated:
    ❌ Napsin A (redundant with TTF1)
    ❌ p63 (redundant with p40)
    ❌ CK5/6 (redundant with p40/p63)

    Validation (n=100 prospective cases):
    ├─ Concordance with full panel: 98/100 (98%)
    ├─ 2 discordant cases: Both adenosquamous (rare)
    └─ Decision: Acceptable discordance rate

    Cost Savings:
    ├─ $180 per case (3 antibodies @ $60 each)
    ├─ 500 cases/year
    └─ Annual savings: $90,000

#### Example 2: Breast Cancer Biomarker Rationalization

**Original Panel (5 markers):** - ER, PR, HER2, Ki67, p53

**Clustering Results:**

    Group 1 (Hormonal):
    ER ─┬── Distance 0.25 (moderate redundancy)
    PR ─┘    Co-positive in 85% of cases

    Independent:
    HER2 (distance 0.82 from ER/PR)
    Ki67 (distance 0.75 from all)
    p53 (distance 0.68 from all)

    ER vs Ki67: Inverse relationship (high ER → low Ki67)
    └─ Mutual Information = 0.52 (moderate)

**Clinical Decision:**

    Keep All Markers:
    ✅ ER (diagnostic and predictive)
    ✅ PR (prognostic despite ER redundancy)
    ✅ HER2 (independent, therapeutic target)
    ✅ Ki67 (proliferation, relates to ER but independent)
    ✅ p53 (independent prognostic marker)

    Rationale for keeping "redundant" PR:
    ├─ ER+/PR- cases (15%) have worse prognosis
    ├─ PR loss indicates incomplete hormone signaling
    ├─ Clinical utility outweighs redundancy
    └─ CAP guidelines require both

    Result:
    └─ No panel reduction, but clustering confirmed
        current guidelines are evidence-based

#### Example 3: GI Panel Simplification

**Original Panel (8 markers):** - CK7, CK20, CDX2, SATB2, MUC2, MUC5AC,
MUC6, CEA

**Clustering Results:**

    Lower GI Cluster:
    CDX2 ──┬── Distance 0.12 (highly redundant)
    SATB2 ─┤    Mutual Information = 0.81
           │
    CK20 ──┘    Distance 0.25 (moderately redundant)
    MUC2 ────── Distance 0.30 (related)

    Upper GI Cluster:
    CK7 ───┬── Distance 0.35
    MUC5AC─┤
           └─ MUC6 (distance 0.28)

    Non-specific:
    CEA (distance 0.55-0.65 from all groups)

**Clinical Decision:**

    Optimized Panel (4 markers):
    ✅ CK7 (upper GI lineage)
    ✅ CDX2 (intestinal differentiation)
    ✅ MUC5AC (gastric-type mucin)
    ✅ CEA (moderate specificity, useful for metastatic workup)

    Eliminated:
    ❌ CK20 (redundant with CDX2 for colon, distance 0.25)
    ❌ SATB2 (redundant with CDX2, distance 0.12)
    ❌ MUC2 (adds little beyond CDX2, distance 0.30)
    ❌ MUC6 (redundant with CK7 for gastric, distance 0.28)

    Performance (n=150 GI tumors):
    ├─ Esophageal: CK7+/CDX2-/MUC5AC+ → 45/45 correct
    ├─ Gastric:    CK7+/CDX2-/MUC5AC+ → 42/45 correct (93%)
    ├─ Pancreatic: CK7+/CDX2+/MUC5AC+ → 28/30 correct (93%)
    ├─ Colon:      CK7-/CDX2+/MUC5AC- → 29/30 correct (97%)
    └─ Overall:    144/150 correct (96% concordance)

    Cost Savings:
    ├─ $240 per case (4 antibodies eliminated)
    ├─ 300 GI cases/year
    └─ Annual savings: $72,000

------------------------------------------------------------------------

### Summary: Your Distance Metric Toolkit

#### Quick Decision Matrix

| Your Data | First Choice | Alternative | When to Switch |
|----|----|----|----|
| **Binary IHC (pos/neg)** | Chi-squared ⭐ | Jaccard | If many double-negatives |
| **Ordinal (0/1+/2+/3+)** | Chi-squared ⭐ | Hamming | If want simple mismatch count |
| **Continuous (H-scores)** | Euclidean ⭐ | Manhattan | If outliers present |
| **% Positive (0-100)** | Euclidean ⭐ | Correlation | If want pattern similarity |
| **Mixed Binary + Continuous** | Mixed ⭐ | Mutual Info | If non-linear relationships |
| **Sparse Binary (rare +)** | Jaccard ⭐ | Cramér’s V | If want normalized measure |
| **Need Statistical Tests** | Chi-squared ⭐ | Cramér’s V | If different table sizes |
| **Non-linear Relationships** | Mutual Info ⭐ | Mixed | If all same data type |

#### Key Takeaways for Pathologists

1.  **Clustering reveals co-expression**, not causation
    - ER and PR cluster because hormone-driven tumors express both
    - Doesn’t mean one causes the other
2.  **Distance \< 0.3 = Consider eliminating one marker**
    - But check clinical literature first!
    - Some “redundant” markers have prognostic value
3.  **Jaccard for sparse data, Euclidean for continuous**
    - Most common scenarios covered by these two
4.  **Always validate optimized panels**
    - Pilot with new cases before full adoption
    - Accept \>=95% concordance with original panel
5.  **Cost savings = clinical utility**
    - Eliminating 2-3 antibodies per case adds up
    - But never sacrifice diagnostic accuracy

------------------------------------------------------------------------

### Glossary for Pathologists

**Distance**: How different two markers are (0 = identical, 1 =
completely different)

**Similarity**: How alike two markers are (opposite of distance)

**Cramér’s V**: Normalized effect size for categorical associations (0-1
scale)

**Jaccard Index**: Co-positivity rate (ignores double-negatives)

**Mutual Information**: Information-theoretic measure (captures
non-linear patterns)

**Dendrogram**: Tree diagram showing how markers cluster hierarchically

**H-score**: Histological score (0-300) combining intensity and %
positive

**Ordinal Data**: Ordered categories (0/1+/2+/3+) but distances between
levels not equal

**Binary Data**: Two categories only (positive/negative, 0/1)

**Continuous Data**: Measured on a scale (H-scores, percentages)

------------------------------------------------------------------------

### Further Reading for Pathologists

#### Foundational Papers

1.  Greenacre M (2017). *Correspondence Analysis in Practice*. 3rd
    ed. Chapman & Hall/CRC.
    - Chapter 9: Clustering in contingency tables
2.  Olsen LR et al. (2006). “Diagnostic and prognostic value of
    immunohistochemistry…” *Modern Pathology* 19:1238-1251.
    - Systematic approach to IHC panel optimization
3.  Sterlacci W et al. (2019). “Immunohistochemistry clustering…”
    *Virchows Arch* 474:687-696.
    - Clustering methodology in diagnostic pathology

#### Statistical References

4.  Agresti A (2013). *Categorical Data Analysis*. 3rd ed. Wiley.
    - Chapter 2: Describing contingency tables (Cramér’s V, etc.)
5.  Deza MM, Deza E (2009). *Encyclopedia of Distances*. Springer.
    - Comprehensive reference for all distance metrics

#### Online Resources

6.  CAP Guidelines: [www.cap.org](https://www.cap.org)
    - Updated IHC interpretation guidelines
7.  Human Protein Atlas:
    [www.proteinatlas.org](https://www.proteinatlas.org)
    - IHC expression patterns across tissues

------------------------------------------------------------------------

**Document Version**: 1.0 **Last Updated**: 2025-01-26 **For
Questions**: Contact your bioinformatics/biostatistics support team

------------------------------------------------------------------------

*This guide is designed for practicing pathologists. For statistical
details, consult the technical documentation in
`MARKER_CLUSTERING_DISTANCES.md`*
