# jjstatsplot module quality dashboard

Date: 2026-09-05. Profile: **standard**, batch mode. Source: ClinicoPathJamoviModule.

**All three currently production-routed analyses compile and pass the focused tests, but each has a confirmed integration or reporting defect.** This check made no analysis-source changes.

| Analysis | Four-file completeness | YAML/R parse and compiler schemas | Focused assertions passed | High findings | Medium findings | Assessment |
|---|---:|---|---:|---:|---:|---|
| jjbarstats | 4/4 | Pass | 135 | 1 | 1 | Changes needed |
| jjpiestats | 4/4 | Pass | 178 | 1 | 2 | Changes needed |
| statsplot2 | 4/4 | Pass | 82 | 1 | 0 | Changes needed |

No critical severity findings or structural schema errors were identified. There are **five distinct analysis findings**, one of which affects two analyses, plus one module packaging finding. A standard check does not establish statistical or release readiness for every supported configuration.

## Scope and synchronization

Runtime discovery matched exactly `menuGroup: JJStatsPlot` or `menuGroup: Power #jjstatsplot` in umbrella analysis definitions. It found `jjbarstats`, `jjpiestats`, and `statsplot2`. Test/development menu suffixes were excluded as required by the playbook.

All 12 hand-written source files are byte-identical between umbrella and sibling repositories. All three generated headers differ only in the expected package assignments (`ClinicoPath` versus `jjstatsplot`). There is no stale implementation/header drift for these three analyses. Hashes and counts are in [inventory.json](inventory.json).

**M1 — High: the sibling manifest still exposes 16 analyses routed to testing in the umbrella.** The sibling `jamovi/0000.yaml` and analysis definitions expose 19 production analyses. Sixteen of those currently have `JJStatsPlotT` in the umbrella: jjhistostats, jjridges, jwaffle, jjscatterstats, jjcorrmat, jjbetweenstats, jjwithinstats, jjdotplotstats, jjsegmentedtotalbar, raincloud, advancedraincloud, lollipop, jjarcdiagram, linechart, hullplot, and jjdotchart.

The shipping configuration therefore disagrees with current umbrella routing. This report covers the three production selections, not the statistical behavior of all 19 entries in the sibling. Before the next build, reconcile sibling generation/removal of stale analyses with umbrella routing, then verify the resulting manifest. The installed/distributed `.jmo` was not inspected.

## Confirmed findings

### F1 — High: sampling changes can retain an old statsplot2 image

Sources: [statsplot2.r.yaml:51](../../jamovi/statsplot2.r.yaml#L51), [statsplot2.b.R:1046](../../R/statsplot2.b.R#L1046).

`sampleThreshold` and `sampleSize` affect which observations are analyzed, but neither appears in any output's `clearWith`. The render method also never updates image state to force a refresh. The plot can therefore retain results calculated from the previous sample when either control changes.

A focused probe exercised the installed `jmvcore::Image$fromProtoBuf()` method using a serialized-image-shaped record with path `previous-render.png`. Restoring after a `sampleSize` or `sampleThreshold` change retained that path; changing `seed`, which is in `clearWith`, correctly cleared it. This confirms the invalidation defect through jmvcore's actual restoration method; it is not a full interactive GUI rerun.

**Fix:** include both sampling controls in the invalidation lists for the plot and dependent text outputs. Verify a saved analysis rerun with a different retained sample size and threshold.

### F2 — High: copy-ready method descriptions disagree with the displayed analysis

Sources: [jjbarstats.b.R:379](../../R/jjbarstats.b.R#L379), [jjbarstats.b.R:756](../../R/jjbarstats.b.R#L756), [jjpiestats.b.R:464](../../R/jjpiestats.b.R#L464), [jjpiestats.b.R:131](../../R/jjpiestats.b.R#L131).

For the sparse 2×2 fixture with cell counts 10, 6, 2, and 8, both analyses generate a Fisher subtitle (`p = 0.051`) while their report templates still name chi-square; jjbarstats's summary also names chi-square. The text is generated from the requested test type without incorporating the exact-test substitution.

Additional jjpiestats paths have the same root problem:

- With `paired = TRUE`, its report still names Pearson's chi-square instead of the paired test.
- With `clinicalpreset = "treatment"` and `typestatistics = "bayes"`, the effective plot options become parametric while the report says Bayesian. Narrative generation reads raw options; plots use derived preset options.

The existing plot-rendering tests pass, but the jjpiestats release test explicitly expects Pearson wording for the sparse fixture, so the test suite currently protects part of this inconsistency.

**Fix:** derive plot subtitles, summaries, assumptions, and Methods text from a shared resolved analysis decision, including pairing, preset overrides, and exact-test substitution. For multiple outcomes or split panels, describe each actual analysis. Update the contradictory test expectation.

### F3 — Medium: jjbarstats clinical presets do not apply their advertised settings

Source: [jjbarstats.b.R:290](../../R/jjbarstats.b.R#L290).

Every preset assignment is conditional on an option being `NULL`, but the affected options all have non-null generated defaults. Selecting diagnostic, treatment, biomarker, or risk-factor presets leaves `overrides` empty. The probes confirmed all four retain `resultssubtitle = FALSE`, `pairwisecomparisons = FALSE`, `typestatistics = "parametric"`, and `proportiontest = FALSE`.

The presets change contextual guidance but do not perform the automatic statistical configuration promised by the UI and option description.

**Fix:** define how presets interact with manual settings, then apply those rules to the effective options. Test the resulting option values for each preset.

### F4 — Medium: jjpiestats's messages option has no effective behavior

Sources: [jjpiestats.b.R:124](../../R/jjpiestats.b.R#L124), [jjpiestats.b.R:169](../../R/jjpiestats.b.R#L169).

`messages` is declared, shown in the UI, and copied to the effective-options list, but is never forwarded to the plotting functions. Its only other branch deduplicates a local warning vector; the output statements in that branch are commented out. Current run-time notices are emitted independently of this option. A literal-reference scanner would miss this dead control.

**Fix:** define and implement which optional messages it controls, or remove the unsupported control while preserving essential validation notices.

### F5 — Medium: jjpiestats promises automatic Results text that is never populated

Source: [jjpiestats.b.R:496](../../R/jjpiestats.b.R#L496).

The visible Copy-Ready Report includes `[Results will be automatically filled when analysis is complete]` after successful analysis. The only report setter writes this template; no later calculation replaces the placeholder. The element is technically populated, but its promised output is missing.

**Fix:** populate Results from the resolved statistical result, or explicitly label the text as a manual-entry template without promising automatic completion.

## Validation and limits

- Read the check-module playbook, repository guidance, patterns guide, and sibling guidance.
- Verified all 12 core files and three generated headers. All nine YAML documents and six R files parsed successfully.
- Checked integration of 62 declared options, including the three Data options, with UI controls/backend access. No undeclared UI controls or missing non-Data controls were found. Dynamic `.option()` and `.optionOr()` access was accounted for.
- Traced all 25 declared results: 17 text/HTML items and eight images. All have setters or implemented render methods. F5 is a semantic output gap, not a missing setter.
- Reviewed empty/unselected inputs, missing data, category variation, count validation, and error paths. The focused suite includes basic, edge-case, correctness, smoke, and existing release regression tests. Some tests assert only successful result construction; passing them does not establish that every plot or warning is correct.
- **395 assertions passed across 256 test blocks in 12 files; zero final failures, errors, warnings, or skips.** [Final test table](tests-final.csv).
- The first sibling test attempt lacked the umbrella fixtures. Tests were therefore rerun against a disposable copy of the identical sibling source with 17 umbrella `.rda` fixtures. Package-qualified test references were adapted to `jjstatsplot`. A too-broad initial adaptation changed a welcome-text expectation; that harness mistake was corrected. Three test setup errors came from unqualified `row_number()`/`tibble()` calls; loading dplyr resolved them. The two affected files passed on retry. Initial logs remain available for transparency.
- Direct `jmvtools::prepare()` could not obtain a version from the installed jamovi executable: it initially returned Node's `v18.15.0`; with `ELECTRON_RUN_AS_NODE` unset it returned no usable version. The installed app's Info.plist reports **28.2.0.0**. The same compiler was run in the isolated copy with its supported `--assume-app-version 28.2.0` argument. **Schema compilation completed with exit 0**, including all 19 sibling definitions and metadata. This establishes compiler/schema success, not native app launch or end-to-end GUI operation. [Compiler log](compiler-schema.log).
- Runtime: R 4.6.0, jmvtools 28.3, jmvcore 2.7.38, ggstatsplot 1.0.0, ggplot2 4.0.3, testthat 3.3.2, yaml 2.3.12.
- No full R CMD check, `.jmo` build, interactive GUI test, citation/dependency release audit, or exhaustive statistical validation was performed. Release-profile gates remain unchecked.

## Next actions

1. Repair F1 and F2; both can present results inconsistent with the user's current settings or report text.
2. Reconcile the sibling's 19-entry manifest with the three current production routes before building.
3. Repair the presets and incomplete jjpiestats controls/report output, then add focused regression coverage for these confirmed gaps.
4. Run the release profile and verify the built module in jamovi.

Reproductions: [reproduce-findings.R](reproduce-findings.R), [probe output](probes.json), [test runner](run-tests.R). Run scripts from the umbrella repository root. Only this report directory was created by this check; source files and routing were not edited.
