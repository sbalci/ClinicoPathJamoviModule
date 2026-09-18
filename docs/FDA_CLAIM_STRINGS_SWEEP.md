# FDA claim-string sweep (2026-09-17)

Deliverable §0.6 of the FDA-referenced diagnostic-performance-goals plan. **Report only — no source files were
changed by this sweep.** It exists because shipping a careful "not FDA software" provenance block in three
analyses while unsourced FDA claims stand elsewhere is the inconsistency a regulatory reader notices first.

Search: `grep -rnoiE 'FDA.{0,30}(approv|clear(ed|ance)|complian|recommend|standard|submission|qualif|validat)'`
over `R/*.b.R` and `jamovi/*.{a,u,r}.yaml`. 21 sites in 11 files.

## The distinction that matters

A string naming FDA is only a problem when it makes a claim **about this module**, or asserts a **specific
regulatory fact without a source**. Factual advice about external things ("check the approved indications") is
fine. Explicit *negative* statements are not just fine, they are the model.

---

## Category A — KEEP. Exemplary negative statements; copy these.

| Site | Text |
|---|---|
| `R/waterfall.b.R:1830` | *"This function is NOT validated for regulatory submissions, clinical trial endpoints, or companion diagnostic development. CRITICAL DEFICIENCIES: ..."* |
| `R/checkdata.b.R:2352` | *"Not suitable for regulatory submissions: ... not a validated quality metric for FDA/EMA submissions"* |

Both put the limitation inside the same string as the claim. That is the structural rule §0.1 adopts.

## Category B — KEEP. Factual advice about external artefacts.

| Site | Text | Note |
|---|---|---|
| `R/jjoncoplot.b.R:1274, 1331` | *"Review FDA-approved drugs and companion diagnostics"* | Advice to the user about external products |
| `R/treatmentoptim.b.R:551` | *"FDA Approved Indications: Verify appropriate patient population"* | Same |
| `R/biomarkerresponse.b.R:489` | links `fda.gov/drugs/cder-biomarker-qualification...` | A real URL to a real programme |

---

## Category C — FIX. Unsourced assertions of specific regulatory facts.

These state numeric or clinical thresholds *as FDA decisions* with no citation. Each is independently checkable,
and at least one appears to be wrong.

### C1 — `ihcscoring`: **FIXED** (2026-09-18). The FDA label was wrong, and a scale confusion sat underneath it.

> **Resolved.** On the user's direction — they confirmed the scale finding and added that these functions are
> **calculation-only and must not recommend any clinical feature or decision**. What changed:
> the 1% and 10% cutoffs now apply to `proportion` (percentage scale) instead of `hscore`; the H-score is
> reported as its own row against the user's `binary_cutpoint`; HER2 is reported over `intensity` with the 3+
> row that was previously computed and never displayed; all five false FDA-approval strings are gone; clinical
> verdicts ("Hormone receptor positive", "May benefit from endocrine therapy", "Equivocal - requires FISH",
> "High proliferative activity", immunotherapy-eligibility text) are replaced by descriptions of what was
> counted; the columns "Clinical Significance"/"Clinical Context" are now "What was counted"/"Scale and source";
> and the UI states the three scales rather than recommending cutpoints. A table note records that the scales
> do not transfer. Two regression tests were added and `tests/testthat/test-ihcscoring.R` went from
> 8 failures / 3 passes to **0 failures / 21 passes**. The diagnosis below is retained as the record.

**Ships to no user.** `menuGroup: OncoPathD` — a draft, umbrella-only. An earlier draft of this report called
this the highest priority and said a pathologist "could classify a genuinely ER-positive case as negative."
That overstated it: no user can reach this analysis today. Corrected below.

**Verified against the authoritative guidelines** (via PubMed):

- **ASCO/CAP 2020 update** — Allison KH et al., *J Clin Oncol* 2020;38(12):1346-1366,
  doi:10.1200/JCO.19.02309, PMID 31928404: *"Breast cancer samples with 1% to 100% of tumor nuclei positive
  should be interpreted as ER positive... A sample is considered ER negative if < 1% or 0% of tumor cell nuclei
  are immunoreactive."* 1-10% is the new **ER Low Positive** category. *"Similar principles apply to PgR
  testing."* The panel also states IHC is the standard *"and no other assays are recommended for this purpose."*
- **ASCO/CAP 2010** — Hammond ME et al., *J Clin Oncol* 2010;28(16):2784-2795,
  doi:10.1200/JCO.2009.25.6529, PMID 20404251: *"ER and PgR assays be considered positive if there are at least
  1% positive tumor nuclei."*

So the standard is **≥ 1% of positive tumour nuclei**. An H-score (0-300, intensity × percentage) is a
different scale, and no FDA-approved ER/PR positivity threshold of "H-score ≥ 100" is apparent.

| Site | Text |
|---|---|
| `jamovi/ihcscoring.u.yaml:95` | `label: "• ER/PR: H-score >=100 (FDA approved)"` |
| `jamovi/ihcscoring.u.yaml:21` | `title: "ER/PR (Breast Cancer) - H-score >=100 recommended"` |
| `R/ihcscoring.b.R:982` | `clinical_context = "FDA approved threshold for hormone receptor positivity"` |
| `R/ihcscoring.b.R:2187` | `clinical_context = "FDA-approved cutoff: H-score >=100 for positive classification"` |
| `R/ihcscoring.b.R:2374, 2377` | *"ER/PR scoring typically uses FDA-approved cutoff of 100 - consider standard threshold"* |

**The deeper defect: percentage thresholds applied to an H-score variable, then labelled as percentages.**
`R/ihcscoring.b.R:922-939` (ER/PR):

```r
positive_rate <- sum(hscore >= 1, na.rm = TRUE) / length(hscore) * 100
bio_table$addRow(rowKey = 1, values = list(
    parameter = "Positive Rate (>=1% cells)",
    reference_range = ">=1% for clinical positivity"))
weak_positive <- sum(hscore >= 1 & hscore < 10, na.rm = TRUE) / length(hscore) * 100
    parameter = "Weak Positive Rate (1-10%)",
```

The variable is `hscore`, but the cutoffs 1 and 10 are the ASCO/CAP **percentage** thresholds, and the row
labels tell the user they are percentages (">=1% cells", "1-10%"). H-score and percent-positive are not
interchangeable: an H-score of 1 could be 1% of nuclei at intensity 1 (ASCO/CAP positive) **or** 0.33% at
intensity 3 (ASCO/CAP negative). The same file simultaneously treats `hscore >= 100` as the positivity cutoff
(`:2187`, `:944`, and the UI label), so the analysis carries two mutually inconsistent definitions of ER/PR
positive — `>= 1` and `>= 100` on the same scale.

`R/ihcscoring.b.R:942-944` (HER2) repeats the pattern: `hscore < 1`, `1-100`, `>= 100` are mapped to HER2
0/1+, 2+, 3+, with `reference_range = "<30% membrane staining"` attached. HER2 is scored by membrane staining
completeness and intensity, not by H-score bands, and a "<30%" range does not describe an H-score band.

**Action.** Three separate changes, and **a pathologist must confirm the replacement text** — neither this
sweep nor any agent should invent a clinical threshold:
1. Remove "(FDA approved)" / "FDA-approved cutoff" from all five sites. The claim is unsupported.
2. Decide, per biomarker, whether the analysis scores on **percent-positive nuclei** or on **H-score**, and make
   the variable, the cutoff and the label agree. If percent-positive: cite ASCO/CAP 2020 and use ≥ 1% with the
   1-10% ER Low Positive band. If H-score: drop the percentage labels and state the H-score convention with its
   own citation.
3. Fix the HER2 block separately; H-score bands are not HER2 0/1+/2+/3+.

One of these strings is already in `catalog.pot`, so the fix needs an i18n pass. Because the analysis is
`OncoPathD` and reaches no user, this is **not urgent — but it must not be promoted in this state.**

### C2 — `pathologyagreement`: an invented FDA number, and a units error

| Site | Text |
|---|---|
| `R/pathologyagreement.b.R:24` | `BOOTSTRAP_RECOMMENDED = 2000 # FDA guidance for high-stakes validation` |
| `R/pathologyagreement.b.R:256` | *"Bootstrap replicates (n=%d) below FDA-recommended threshold (n=2000) for high-stakes validation studies. Consider increasing for regulatory submissions."* |
| `jamovi/pathologyagreement.a.yaml:87` | *"Example: FDA biomarker qualification typically uses 2000+ samples."* |
| `R/pathologyagreement.b.R:1268` | *"Guidelines based on: Landis & Koch (1977), Cicchetti (1994), Koo & Li (2016), FDA Biomarker Qualification Guidance"* |

Three problems:
1. **FDA recommends no bootstrap-replicate count.** 2000 is a reasonable statistical default; attributing it to
   FDA guidance is fabrication.
2. **A units error.** The code uses 2000 as *bootstrap replicates*; the `.a.yaml` describes it as 2000+
   ***samples*** (subjects). Those are unrelated quantities, and the description will mislead anyone sizing a
   study from it.
3. `:1268` attributes interpretation thresholds to an FDA guidance with **no document number, no year, and no
   matching `refs:` entry** (that analysis cites only `ClinicoPathJamoviModule`, `psych`, `epiR`).

**This ships in production** (`pathologyagreement` is not a `D`/`P` draft). Highest-severity *compliance-wording*
item.

**Action:** drop the FDA attribution from all four; keep 2000 as a documented statistical default; fix the
replicates-vs-samples wording; if a biomarker-qualification reference is wanted, cite the actual document.

### C3 — `biomarkerresponse.b.R:712`

*"FDA biomarker guidance recommends n>=50 per group for clinical validation studies."* Same class as C2 — a
specific number attributed to unnamed guidance. Either cite the document and its section, or restate as a
general statistical rule of thumb.

---

## Category D — FIX. Claims about this module's regulatory status.

### D1 — `R/digitalvalidation.b.R` (and its `.a.yaml`). The reason §0.1 exists.

| Site | Text |
|---|---|
| `:38-47` | *"This workflow implements CAP/CLSI guidelines for method comparison studies and FDA guidance for AI/ML-based medical devices in pathology."* |
| `:40` | *"FDA/CE-IVD algorithm validation for clinical deployment"* |
| `:415-421` | *"VALIDATION PASSED: ... Suitable for clinical implementation."* |
| `:431` | `"<li> Sample size adequate for FDA/CE submission (n>=40)</li>"` |
| `:457-458` | *"This validation follows CAP/CLSI EP09 guidelines and FDA guidance for AI/ML-based medical device validation."* |
| `jamovi/digitalvalidation.a.yaml:62-77` | `acceptance_criteria` with options `fda_strict` / `fda_standard` / `clsi_ep09`, default `fda_standard` |

Four distinct defects:
1. A **conformance claim** to documents never named, versioned, or cited.
2. A **fitness-for-use verdict** ("Suitable for clinical implementation") from hard-coded thresholds.
3. A **regulatory-sufficiency claim** (n>=40).
4. The FDA-labelled `acceptance_criteria` option is **dead code** — `grep` finds it, and
   `custom_correlation_threshold` / `custom_icc_threshold`, referenced **zero** times in the backend. A user
   selects "FDA Strict" and nothing changes; the hard-coded `r >= 0.95` / `r >= 0.90` apply regardless.

Its `refs:` cites `ClinicoPathJamoviModule`, `gridExtra`, `psych`, `epiR` — **no standards document at all.**

`menuGroup: OncoPathD`, so it ships to no user today. That is the only reason this is not urgent. It should be
fixed or the FDA framing removed before it is ever promoted.

---

## Suggested order — revised after verification

Urgency tracks **what users can reach**, which reorders this from the first draft:

1. **C2** (`pathologyagreement`) — the only item in **production**. The "2000+ samples" units error is cheap and
   safe to fix on its own; dropping the invented FDA attribution is a one-line edit per site.
2. ~~**C1** (`ihcscoring`)~~ — **done 2026-09-18.** Thresholds moved onto the correct scales, FDA claims and
   clinical verdicts removed, regression tests added. No longer a promotion blocker on these grounds.
3. **D1** (`digitalvalidation`) — worst FDA framing, also ships nowhere. Fix or strip before promotion.
4. **C3** (`biomarkerresponse`) — single string, low risk.

Categories A and B need no action.
