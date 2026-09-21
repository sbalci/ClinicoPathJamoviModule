# Reference Audit Report — IHC Heterogeneity Analysis

**Audited:** 2026-09-21
**Source:** the References panel of the `ihcheterogeneity` jamovi analysis
**Rendered from:** `R/ihcheterogeneity.b.R:2377-2385` (hard-coded HTML `<li>` strings)
**Cross-checked against:** `jamovi/00refs.yaml` (9 cited keys), CrossRef REST API, NCBI E-utilities

## Executive Summary

- Total references in the panel: **8**
- **Level 1 (Existence):** 8 verified, 0 not found
- **Level 2 (Metadata):** 8 correct, 0 with errors *(1 apparent discrepancy resolved as a convention difference, see below)*
- **Level 3 (Topical relevance):** 8 confirmed, 0 questionable
- **Level 4 (Contextual accuracy):** 8 correct, 0 problematic
- **Retractions:** none

**No action required.** Every reference exists, every bibliographic field is correct, and every
citation is used for a claim the cited work actually supports.

## Critical Issues

None.

## The one apparent discrepancy, resolved

**Bland & Altman 1986 — volume `1` vs CrossRef's `327`.**

CrossRef returns `volume: 327` for DOI `10.1016/S0140-6736(86)90837-8`. The panel says `1(8476)`.
Both are right, in different conventions:

- The Lancet numbered volumes **i** and **ii** within each year in 1986. PubMed (PMID 2868172)
  records it as `Lancet. 1986 Feb 8;1(8476):307-10` — identical to the panel.
- Elsevier later applied **continuous** volume numbering across the journal's history, which makes
  1986 vol. i into vol. 327. That is the number CrossRef serves.

The panel follows the biomedical (PubMed/Vancouver) convention, which is the correct choice for a
pathology audience. **Not an error.** The `.bib` records the printed volume with a `note` naming
the continuous number, so either style can be produced downstream.

A second CrossRef artifact: it records the first author's family name as `Martin Bland`, given
`J`. PubMed gives `Bland JM`. The author is J. Martin Bland; the panel is correct.

## Detailed audit by reference

Each entry was matched on first author, year, journal, volume, issue and page range.

### [1] Koo TK, Li MY. J Chiropr Med 2016;15(2):155-163
- **L1 Existence:** EXISTS — DOI `10.1016/j.jcm.2016.02.012`
- **L2 Metadata:** METADATA_CORRECT — all fields match
- **L3 Topical:** TOPIC_CONFIRMED — the paper is a guideline for selecting and reporting ICCs
- **L4 Contextual:** CITATION_CORRECT — cited for ICC selection/reporting; the backend computes
  ICC(A,1) and ICC(C,1) and reports them with confidence intervals (`.iccFromMatrix`, ~line 434)

### [2] McGraw KO, Wong SP. Psychol Methods 1996;1(1):30-46
- **L1:** EXISTS — DOI `10.1037/1082-989X.1.1.30`
- **L2:** METADATA_CORRECT
- **L3:** TOPIC_CONFIRMED — the source of the ICC(A,1)/ICC(C,1) notation and inference
- **L4:** CITATION_CORRECT — the backend comment at line 430 names exactly these two forms
  ("ICC(A,1) (absolute agreement, two-way random) and ICC(C,1)"), which is McGraw & Wong's scheme

### [3] Bland JM, Altman DG. Lancet 1986;1(8476):307-310
- **L1:** EXISTS — PMID 2868172, DOI `10.1016/S0140-6736(86)90837-8`
- **L2:** METADATA_CORRECT (see the volume-convention note above)
- **L3:** TOPIC_CONFIRMED — the founding paper on limits of agreement
- **L4:** CITATION_CORRECT — cited as the agreement-methods basis of the analysis

### [4] Bland JM, Altman DG. Stat Methods Med Res 1999;8(2):135-160
- **L1:** EXISTS — PMID 10501650, DOI `10.1177/096228029900800204`
- **L2:** METADATA_CORRECT
- **L3:** TOPIC_CONFIRMED — method-comparison studies, incl. differences that vary with magnitude
- **L4:** CITATION_CORRECT — cited in two table notes (lines 280, 282) for judging a difference at
  the 5th and 95th percentiles of the level when the slope is significant at `0.05/m`. This is an
  application of the paper's magnitude-dependent-agreement treatment rather than a verbatim
  procedure from it; the notes name the paper alongside the rule, which is the honest framing.

### [5] Bonett DG, Wright TA. Psychometrika 2000;65(1):23-28
- **L1:** EXISTS — DOI `10.1007/BF02294183`
- **L2:** METADATA_CORRECT
- **L3:** TOPIC_CONFIRMED — sample size and variance for Pearson, Kendall and Spearman correlations
- **L4:** CITATION_CORRECT — the backend (line 192) uses "the Fisher-z 95% CI for a Spearman
  correlation with the variance of Bonett & Wright", which is precisely this paper's contribution
- **Rendering:** conditional — shown only when a reference measurement is present

### [6] Schuirmann DJ. J Pharmacokinet Biopharm 1987;15(6):657-680
- **L1:** EXISTS — DOI `10.1007/BF01068419`
- **L2:** METADATA_CORRECT
- **L3:** TOPIC_CONFIRMED — the two one-sided tests (TOST) procedure for equivalence
- **L4:** CITATION_CORRECT — the analysis judges equivalence by asking whether a confidence
  interval lies inside a materiality margin. That is the TOST/CI duality, for which Schuirmann is
  the standard citation. There is no literal `tost()` call, and none is needed.

### [7] MacKinnon JG, White H. J Econometrics 1985;29(3):305-325
- **L1:** EXISTS — DOI `10.1016/0304-4076(85)90158-7`
- **L2:** METADATA_CORRECT
- **L3:** TOPIC_CONFIRMED — introduces the HC1/HC2/**HC3** heteroskedasticity-consistent estimators
- **L4:** CITATION_CORRECT — verified in the code, not just the comment: line 163 computes
  `e3 <- e / (1 - h)`, the HC3 leverage correction, and the variance is formed from `e3^2`
- **Rendering:** conditional — shown only when a proportional-bias row was computed

### [8] Bonett DG. Stat Med 2002;21(9):1331-1335
- **L1:** EXISTS — DOI `10.1002/sim.1108`
- **L2:** METADATA_CORRECT
- **L3:** TOPIC_CONFIRMED — sample size for estimating ICCs with desired precision
- **L4:** CITATION_CORRECT — cited for the sample-size planning output
- **Rendering:** conditional — shown only when sample-size planning ran

## Metadata corrections table

| Ref | Field | In panel | Database | Verdict |
|---|---|---|---|---|
| [3] | Volume | `1` | CrossRef `327` | **Panel correct** — PubMed also says `1`; 327 is Elsevier's retroactive continuous numbering |
| [3] | First author | `Bland JM` | CrossRef `Martin Bland J` | **Panel correct** — CrossRef name-parsing artifact; PubMed says `Bland JM` |
| [3],[4] | Pages | `307-310`, `135-160` | PubMed `307-10`, `135-60` | **Both valid** — panel uses expanded ranges consistently |

No corrections needed.

## Observations (not defects)

1. **Conditional citation is done well.** Three of the eight are emitted only when the method that
   needs them actually ran (`BonettWright2000` with a reference measurement, `MacKinnonWhite1985`
   with a proportional-bias row, `Bonett2002` with sample-size planning). Citing only what was used
   is better than the usual fixed block, and it means the panel is an accurate record of the run.

2. **The panel is hard-coded free text, independent of `00refs.yaml`.** The nine keys in
   `jamovi/00refs.yaml` carry author/year/title/journal but **no volume, issue or pages**; those
   appear only in the `<li>` strings at `R/ihcheterogeneity.b.R:2377-2385`. Two independent copies
   of the same bibliography can drift, and only the `00refs.yaml` side is covered by the release
   gate's citation checks. They agree today — this audit confirms it — but nothing enforces that.
   A cheap guard would be a test asserting that every author/year in the rendered block matches the
   corresponding `00refs.yaml` entry.

3. **`ClinicoPathJamoviModule` (the software self-citation)** is the ninth cited key in
   `00refs.yaml` and does not appear in this panel. That is expected — jamovi renders it in its own
   references section — and is not a discrepancy.

4. **`pathologyagreement` carries a near-duplicate of the Koo & Li line** in a different format
   (`R/pathologyagreement.b.R:1131`). Not an error, but the two modules format the same reference
   differently.
