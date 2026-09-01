# `timeinterval` — release status (2026-08-31)

Both release blockers are **closed**. This file records what they were, how they were fixed,
and — importantly — the residual risk that is *not* detectable, so nobody re-derives it.

---

## C1 (was BLOCKER) — rows typed in the opposite day/month order — FIXED, with a measured residue

A registry column typed `DD/MM/YYYY` in which some rows were entered `MM/DD/YYYY`. Before the
fix, on a 20-row fixture: person-time **+40.9%**, quality panel "Overall Quality: Good", the
only banner a green "Analysis completed", and a copy-ready manuscript sentence carrying the
number. Every guard missed it: the bad rows parse *successfully*, stay positive, sit inside
2 x the 99th percentile, and are far under the 50-year backstop.

### What the risk actually is — measured, not assumed

200 simulated cohorts with 15% of rows typed in the other order, classified by **what the user
actually sees**:

| Outcome | Count |
|---|---|
| Rejected outright (negative intervals) | 26 (13%) |
| Already warned — missing-data banner | 165 (82%) |
| **Truly silent, person-time wrong >5%** | **9 (4.5%)**, worst 13.2% |

The 82% is the key fact: a row typed in the other order whose day exceeds 12 **cannot be parsed
at all** and becomes NA, which the existing missing-data warning already catches. Only rows
where *both* dates happen to have day <= 12 slip through — one or two in fifty.

### What was done

1. **A group-level test.** Day-of-month has nothing to do with length of follow-up, so in a clean
   column the order-ambiguous rows are a random subset of the rest and the two groups'
   intervals are exchangeable. A rank-sum test at alpha = 0.001 plus a required effect size
   (median ratio outside 0.67–1.5) flags a material difference. **0 false positives in 400 clean
   cohorts**; catches the fixture above.
2. **An honest exposure disclosure**, because the 4.5% residue is one or two rows and no test
   can separate that from chance. The summary panel (always visible) now carries one small-print
   line — *"Day/month ambiguity: 7 of 60 rows could have been typed in either order and cannot be
   verified here (11% of the person-time above)"* — and the Data Quality panel carries the full
   explanation. Stated as a property of the data, not an accusation.
3. **The Clinical Summary is withheld** whenever the mixed-order warning fires. A copy-ready
   sentence is the worst destination for a suspect number: it travels out of the results window
   and the warning does not follow it.

### Why not just warn on ambiguity

Because it cries wolf, and a banner that fires on correct data is worse than none. In a clean
`dd/mm` column roughly a sixth of rows are order-ambiguous — often *more* than in a contaminated
one. A bare ambiguity percentage cannot separate the two cases, which is why the test is on the
interval distribution and the raw exposure is disclosed rather than alarmed.

### Residual risk — accept knowingly

~4.5% of mixed-order contamination remains undetectable by any means available here, worst
observed person-time error 13%. The exposure line is now always on screen for such data, so the
user can see that unverifiable rows exist even when nothing fires. **Nothing further is
achievable from the data alone** — the information needed to distinguish "5 March" from
"3 May" is destroyed at data entry.

---

## M2 (was BLOCKER) — two-digit-year columns read as `ymd` — FIXED

The detector broke ties by the order of its candidate list, and `ymd` was first. For any
`DD/MM/YY` column with years up to 2031, `ymd` always parses (yy <= 31 reads as a day, mm <= 12
as a month, dd as a 2-digit year), so it always tied and always won — and was always wrong.
Measured: mean follow-up **3775 days against a true 493** (7.7x), and the refusal message's own
advice ("tick Remove negative intervals") is what produced it.

**Fix:** ties are broken by which candidate places the dates in the **narrowest range of years**.
A clinical cohort spans a few years; reading a day-of-month as a year scatters them across
thirty. Measured on the same data: `ymd` span 25 years, `dmy` span 0. Genuine ISO text never
reaches the branch (`dmy` cannot parse `2020-03-05` at all), and a true `dmy`/`mdy` ambiguity has
an identical year component either way, so the span is equal and the original order still
decides — that case belongs to C1's check instead.

Same cohort after the fix: mean 493, total 14790 person-days — exact.

---

## Also fixed in the release pass

- The glossary, `.a.yaml` and `man/timeinterval.Rd` claimed landmark analysis selects "6-month
  survivors". It selects on **length of follow-up** — there is no event indicator — which the
  analysis's own landmark warning already said, so the results window contradicted itself.
- The always-visible person-time panel claimed to account for censoring. It does not.
- "Serves as the denominator for incidence rates" was printed unconditionally, including at zero
  person-time (divide by zero) and beside a warning declaring the rows unreliable.
- The format-ambiguity note was computed and then discarded on every rejected run.
- The written-back column is landmark-rebased and did not say so; its title now names the landmark.
- All 26 notice banners are now translatable (`.()` / `.fmt()`), including the four severity
  labels. Rendered English text verified byte-identical across 23 scenarios.
- A landmark of 0 with the box ticked said nothing; the negative-interval refusal did not mention
  the timestamped-start-vs-midnight-end case that laboratory systems produce; "1 participants".
- The Timezone help said it affected parsing only — it also shifts durations by an hour across a
  daylight-saving transition.

## Still open (not blockers)

- The **summary panel's** own strings (`extremeSkipReason`, the landmark exclusion breakdown, the
  missing-values clause) are not translatable — only the notice banners and `reject()` messages
  are. A follow-up if the module is localised properly.
- `jsurvival` ships `timeinterval` 1.0.7 with **no spreadsheet-serial guard** — the pre-existing
  hazard where an Excel day-count column reports 792 months of follow-up. `menuGroup: SurvivalT`
  dev-routes the umbrella copy, so `_updateModules.R` does not propagate any of this work.
  Promoting to `Survival` is a deliberate decision, not a side effect of a fix.
