# ClinicoPath — Clinical-Needs Review: Improvements & New-Function Candidates

**Date:** 2026-07-11 · **Module version:** 0.0.47 · **Catalog size:** 379 analyses
**Audience lens:** practising pathologists, oncologists, and general clinical researchers.

This review is deliberately *different* from `analysis-gap-review-2026.md`, which was
methodology-driven (which statistical methods are missing). Here the question is
**clinical**: considering the analyses these three specialties actually run day-to-day,
where would the module gain the most bedside/bench utility — by improving what exists, and
by adding what is genuinely missing?

## How this was scoped

The catalog was re-enumerated from `jamovi/*.a.yaml` (not from memory) and bucketed by
menu family. Release maturity (per the module author's convention) is:

| Suffix | Meaning | Count |
|--------|---------|------:|
| *(none)* | **Released** (production) | 58 |
| `T` | **Testing** — nearly ready to release | 57 |
| `D` / `DT` | **Draft** / experimental | 264 |

The four production modules are **jsurvival**, **meddecide** (incl. Power), **ClinicoPathDescriptives**,
and **jjstatsplot**. The `OncoPath*` family and most `D` items are experiments/drafts.

**Headline finding.** This is an exceptionally complete catalog. The core clinical
workflows — survival (Kaplan–Meier, Cox, competing risks, multistate, cure, RMST,
person-time), diagnostic accuracy (ROC in ~a dozen variants, DCA, likelihood ratios,
agreement with weighted kappa / Krippendorff / Gwet AC), IHC scoring (Allred, CPS,
multi-cutpoint, H-score), RECIST/iRECIST waterfall + spider, staging validation, Table 1,
and meta-analysis — are all present, most in more than one implementation. Consequently the
highest-value opportunities are **(a) consolidating and deepening a handful of
high-traffic released functions** and **(b) a short list of specialty-specific analyses
that are truly absent** and are standard in daily reporting. Verified-absent items are
flagged **[confirmed gap]**; the rest are enhancements to existing files.

Tiering uses two axes — **clinical value** (how often the specialty needs it, how much it
changes a report or decision) and **effort** (new dependency? new engine? or an added
option/output on an existing backend?).

---

# Part A — Improvements to existing functions

## A1 · Pathology

**A1.1 — `ihcscoring`: add formal cutpoint optimization + reproducibility output.**
*Value: high · Effort: low–medium.* The function already supports Allred, CPS, binary and
multi-cutoff scoring. What a diagnostic report needs next is a **data-driven optimal
cutpoint** (Youden / maxstat, with a bootstrap-validated confidence band) tied to an
outcome, and an **inter-observer reproducibility panel** (ICC / weighted kappa across
readers) rendered inline rather than as a separate `agreement` run. Both engines already
exist elsewhere in the module (`optimalcutpoint`, `agreement`) — the improvement is to
surface them within the IHC workflow so scoring, cutpoint, and reproducibility appear in
one output.

**A1.2 — `agreement`: category-collapse and per-category agreement.**
*Value: medium · Effort: low.* Weighted kappa, Krippendorff, and Gwet AC are all present.
The practical addition is **per-category (one-vs-rest) agreement** and an optional
**level-collapse** control (e.g. G1+G2 vs G3), which is how grading-concordance studies are
actually reported. This is an options + output-table change, no new engine.

**A1.3 — `pathologyagreement` / `methodcomparison`: add bias-vs-magnitude regression.**
*Value: medium · Effort: low.* Bland–Altman, Passing–Bablok and Deming are covered. Adding
an explicit **proportional-bias test** and a **percentage (relative) difference plot** for
assays spanning several orders of magnitude (e.g. Ki-67 %, digital-image quantitation) rounds
out method-comparison for pathology.

## A2 · Oncology

**A2.1 — `waterfall` / `waterfallrecist`: add time-to-response and duration-of-response.**
*Value: high · Effort: medium.* The waterfall/spider tools compute ORR, DCR, best response
with confidence intervals — but **not time-to-response (TTR) or duration-of-response (DoR)**,
which are required efficacy endpoints in every response-evaluable trial. Since the input
already carries a time variable for the spider plot, TTR/DoR can be derived from the same
data and reported alongside ORR/DCR.

**A2.2 — `survival` (jsurvival): add a subgroup / forest-panel option.**
*Value: high · Effort: medium.* The released `survival` has person-time, pairwise
comparisons, landmark, and risk tables. The most-requested missing output is a
**subgroup hazard-ratio forest panel** (HR per level of a stratifier with interaction
p-value). `subgroupforest` and `groupedforest` exist as separate drafts; wiring a
subgroup option into the released survival function is higher clinical value than a
standalone.

**A2.3 — `stagemigration`: expose the internal Win Ratio and NRI as first-class output.**
*Value: medium · Effort: low.* `stagemigration` already contains a Win Ratio helper and
reclassification logic internally. Now that a standalone `winratio` exists, cross-link them
and surface the staging-specific reclassification (Will Rogers quantification) as a labelled
table.

## A3 · General clinical

**A3.1 — `crosstable`: add a standardized mean difference (SMD) column.**
*Value: medium · Effort: low.* The grouped "Table 1" workflow is already covered — the
released `crosstable` produces a by-group table with automatic test selection (chi-square /
Fisher / t-test / ANOVA) and multiple-testing correction, so `tableone` does **not** need a
grouping option. The one piece still missing for matched/weighted cohort work is a
**standardized mean difference per row** — the standard covariate-balance diagnostic, which
does not depend on sample size the way a p-value does. This is an added output column on the
existing `crosstable` backend, no new engine.

**A3.2 — `decisioncurve` / `bayesdca`: add paired net-benefit difference with CI.**
*Value: medium · Effort: low.* Decision-curve analysis is well covered. Adding an explicit
**Δ net-benefit between two models across the threshold range, with a bootstrap CI**, is the
output clinicians actually compare, rather than reading two overlaid curves by eye.

**A3.3 — `agreement` / diagnostic tools: reference-interval-aware flags.**
*Value: medium · Effort: low.* `referenceintervals` exists; linking it so diagnostic and
agreement analyses can flag values against an established reference range would reduce
copy-between-analyses friction.

---

# Part B — New-function candidates (confirmed gaps)

Each was verified absent by searching titles, descriptions, and option names across all 379
`.a.yaml` files (with corrected regex alternation).

## Tier 1 — high clinical value, low–medium effort

**B1 — Residual Cancer Burden / tumor-regression-grade calculator** **[confirmed gap]**
*Audience: pathology + oncology · Effort: low (deterministic formula).*
No analysis computes **RCB** (the standardized post-neoadjuvant breast index: tumour-bed
dimensions, cellularity, in-situ fraction, node count and largest metastasis → RCB index and
class 0/I/II/III) or a generic **tumor regression grade** (Miller–Payne, Mandard, AJCC TRG,
Ryan). These are entered into synoptic reports daily. The math is a closed-form calculator —
a clean, self-contained backend with no new dependency — and would be one of the highest
value-per-effort additions for a pathology audience.

**B2 — Lymph node ratio / nodal yield analysis** **[confirmed gap]**
*Audience: pathology + oncology · Effort: low.*
No analysis computes **lymph node ratio** (positive/examined) or evaluates **nodal yield
adequacy** against a threshold, despite LNR being an established prognostic factor across
colorectal, gastric, breast, and head-and-neck cancers. A function taking positive-node and
examined-node counts, computing LNR with optimal-cutpoint stratification and a survival link,
fills a recurring need. Reuses the existing cutpoint and survival engines.

**B3 — Inflammatory / prognostic composite indices (NLR, PLR, PNI, GPS)** **[confirmed gap]**
*Audience: oncology + general clinical · Effort: low.*
No analysis derives the widely-reported **neutrophil-to-lymphocyte ratio, platelet-to-lymphocyte
ratio, prognostic nutritional index, or Glasgow prognostic score** from routine bloods, nor
dichotomizes them at literature or data-driven cutpoints with a survival/outcome link. A
single "hematologic prognostic indices" function computing these from component variables
would be heavily used and is arithmetically simple.

## Tier 2 — high value, higher effort

**B4 — ctDNA / MRD longitudinal dynamics** **[confirmed gap]**
*Audience: oncology · Effort: medium.*
Nothing addresses **circulating-tumour-DNA / minimal-residual-disease** kinetics —
clearance vs persistence, lead-time to radiographic progression, or MRD status as a
time-dependent predictor. Given the module's strong time-dependent-covariate and
joint-modelling drafts, a focused ctDNA-dynamics analysis (log-VAF trajectory, clearance
classification, landmark by MRD status) is a natural, high-visibility oncology addition.

**B5 — Tumor budding / spatial hotspot quantification** **[confirmed gap]**
*Audience: pathology · Effort: medium.*
No analysis implements the **ITBCC tumor-budding count-to-grade workflow** (buds per
0.785 mm² hotspot → Bd1/2/3) or a general **hotspot-selection** rule for a
count-per-area biomarker. With `haralicktexture` and `functionalsampling` already present,
a budding/hotspot quantifier extends the digital-pathology surface toward a routine reporting
element.

## Tier 3 — valuable, more specialized

**B6 — Synoptic / structured-report completeness auditor** **[confirmed gap]**
*Audience: pathology (lab QA) · Effort: medium.*
(The term "structured reporting" appears incidentally in `clinicalnomograms` and
`survivalmodelvalidation` as output-format descriptions; neither audits report completeness.)
No analysis audits **CAP/ICCR synoptic-report completeness** — the proportion of required
data elements present per report, by subspecialty or reporting pathologist, over time. This
is a laboratory-quality metric increasingly required for accreditation. It pairs naturally
with the just-added `interruptedtimeseries` for tracking completeness after a protocol change.

**B7 — Concordance / clonality across multifocal samples** **[confirmed gap]**
*Audience: molecular pathology · Effort: medium.*
Beyond `pathsampling`, there is no analysis assessing **biomarker or mutation concordance
across multiple foci / paired primary-metastasis samples** (e.g. per-marker concordance
rate, discordance pattern, simple clonality summary). Relevant as multi-region and
primary/metastasis testing becomes routine.

---

# Prioritized summary

| # | Candidate | Type | Audience | Value | Effort |
|---|-----------|------|----------|:-----:|:------:|
| A2.1 | Waterfall: TTR + duration-of-response | improve | Onc | High | Med |
| A1.1 | ihcscoring: optimal cutpoint + reproducibility panel | improve | Path | High | Low–Med |
| A2.2 | survival: subgroup HR forest option | improve | Onc | High | Med |
| B1 | RCB / tumor-regression-grade calculator | **new** | Path/Onc | High | Low |
| B2 | Lymph node ratio / nodal yield | **new** | Path/Onc | High | Low |
| B3 | Inflammatory prognostic indices (NLR/PLR/PNI/GPS) | **new** | Onc/Clin | High | Low |
| A1.2 | agreement: per-category + level-collapse | improve | Path | Med | Low |
| A2.3 | stagemigration: expose Win Ratio / NRI | improve | Onc | Med | Low |
| A3.1 | crosstable: SMD balance-diagnostic column | improve | Clin | Med | Low |
| A3.2 | decisioncurve: Δ net-benefit with CI | improve | Clin | Med | Low |
| B4 | ctDNA / MRD dynamics | **new** | Onc | High | Med |
| B5 | Tumor budding / hotspot quantification | **new** | Path | Med | Med |
| B6 | Synoptic-report completeness auditor | **new** | Path/QA | Med | Med |
| B7 | Multifocal concordance / clonality | **new** | Mol path | Med | Med |
| A1.3 | methodcomparison: proportional-bias + %-diff | improve | Path | Med | Low |
| A3.3 | reference-interval-aware flags | improve | Clin | Med | Low |

## Recommended first wave (best value-per-effort)

1. **B1 — RCB / tumor-regression-grade calculator** (closed-form, no dependency, used daily in path reports)
2. **B2 — Lymph node ratio** (simple, established prognostic factor, reuses cutpoint+survival engines)
3. **B3 — Inflammatory prognostic indices** (arithmetic, high oncology demand)
4. **A2.1 — waterfall TTR/DoR** (completes the RECIST efficacy endpoint set)

These four are all either dependency-free or reuse engines already in the module, and each
maps to an analysis one of the three specialties runs routinely.

---

*Method note.* Coverage was verified against source files, not recall. Items marked
**[confirmed gap]** have no analysis implementing them; where a search term produced
incidental matches (e.g. the phrase "structured reporting" for B6, or `pathsampling` for
B7), those hits were inspected and are noted in the item text as not covering the workflow.
Improvement items cite the specific released/testing function they extend. Any candidate
should still be checked against the newest draft files before implementation, since the
`D`/`T` layer is large and evolving.
