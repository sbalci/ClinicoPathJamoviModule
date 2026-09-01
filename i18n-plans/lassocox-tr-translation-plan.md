# LASSO Cox Turkish localization — completed implementation

Date: 2026-08-31. Analysis version: 0.0.5. Scope: current LASSO messages only.

## Files and changes

All required files were present: `R/lassocox.b.R`, the three LASSO YAML schemas,
`jamovi/i18n/en.po`, `catalog.pot`, and `tr.po`. The generated header was regenerated
by the jamovi compiler; it was not edited directly. Existing unrelated translations
and pre-existing changes were preserved.

The backend now constructs educational HTML from translated complete sentences,
escaping text before embedding it. Plot effect labels, prominent stability notices,
constant-removal provenance, comparison failures, and the risk-table title are
translatable. YAML controls use sentence-case noun labels. No option keys, internal
column names, user-provided variable names, or machine identifiers were translated.

Representative applied change:

```r
# Previously: long literal-English HTML.
self$results$lassoExplanation$setContent(private$.explanation(
    .("Understanding LASSO Cox regression"),
    c(.("The displayed C-index is apparent development performance. Validate the entire modeling process before considering clinical use."))))
```

## Translation inventory and glossary

All **320 current extracted LASSO messages** have nonempty Turkish translations.
The full review table is [lassocox-tr-messages.tsv](lassocox-tr-messages.tsv); the
canonical runtime source is `jamovi/i18n/tr.po`. Existing nonempty translations were
preserved. Obsolete module-wide catalog entries were not removed.

| English | Turkish / rule |
|---|---|
| Predictor | Yordayıcı |
| Penalized coefficient | Cezalı katsayı |
| Higher / lower fitted hazard | Daha yüksek / düşük tahmini hazard |
| Model stability warnings | Model kararlılığı uyarıları |
| Number at risk | Risk altındaki sayı |
| Hazard ratio | Hazard oranı; not an event-probability ratio |
| lambda.min, lambda.1se, glmnet, Breslow | Preserve literal identifiers |

Use complete, direct sentences and Turkish decimal conventions only where the
framework formats numbers. Preserve named braces and printf placeholders exactly.
Do not describe development-sample performance as validated prediction or causal
effects.

## QA results

- Official jmvtools extraction applied to an isolated LASSO package; scoped entries
  merged into repository catalogs without rewriting other analyses.
- All 320 named/printf placeholder sets agree between source and translation.
- GNU `msgfmt --check --check-format` passes.
- The official jamovi compiler's `createTranslationJSON` compiled English and
  Turkish catalogs, installed only into the temporary test library.
- Runtime Turkish tests exercise educational HTML, suitability, tables, and effect
  labels. All four plot renderers were inspected in English and Turkish at their
  declared dimensions.
- The real jamovi client options panel loaded with no errors. This checks UI wiring;
  it is not a complete manual desktop locale-switch or `.omv` round trip.

## Weblate / GitHub handoff

No remote service or repository settings were changed. The local implementation
needs no Weblate setup to run. If translation maintenance moves to Weblate, point
the existing project/component at `jamovi/i18n/*.po`, use the project template
`catalog.pot`, retain English as the source language, and review the normal GitHub
pull request before merging. Confirm repository ownership and credentials through
the project's normal release process; no tokens belong in these files.
