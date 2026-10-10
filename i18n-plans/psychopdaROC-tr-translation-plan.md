# psychopdaROC Turkish translation preparation

Analysis 1.0.88; verified 2026-10-10. The source imports `jmvcore`, making its `.`
translation helper available. All option, result and UI YAML strings are extracted
by the jamovi compiler. Newly changed backend messages use complete `.()` templates,
with positional or named substitution tokens preserved.

The current isolated extraction contains **626 target entries**, all with nonempty
Turkish translations and none fuzzy. This includes all **313 marked backend messages**.
The earlier procedure, fixed-sensitivity/specificity guidance and power-recommendation
fragments were rewritten as complete translatable sentences. User-supplied marker and
outcome names remain literal labels. The new class controls, exclusions and power target
are included in the catalog.

Catalog updates retain every existing nonempty translation and every unrelated entry.
The first pass added 139 extracted entries to each of `catalog.pot`, `en.po`, and
`tr.po`, filling 59 Turkish entries. A second pass filled 200 existing empty target
entries. Historical unused messages remain in the umbrella catalog; the coverage count
uses current compiler references, not obsolete strings. Both `sprintf` and named
placeholders were checked, including literal percentage signs and italic markup.

Executed checks:

```sh
msgfmt -c -o /dev/null jamovi/i18n/tr.po
Rscript --vanilla -e 'jmvtools::i18nUpdate(pkg="/private/tmp/psychopdaROC-classes-20261010")'
Rscript --vanilla tools/i18n_rebuild_json.R /private/tmp/psychopdaROC-classes-20261010
```

The generated Turkish JSON contains all 626 current target entries. The isolated
native test module also builds with the completed catalogs. The umbrella repository's
`inst/i18n` has no checked-in runtime JSON; the build generates it from the catalogs.
No application language preference was changed. A native Turkish layout/terminology
review remains a localization acceptance check; catalog coverage is not that review.

Evidence: [catalog coverage](../development-ideas/psychopdaROC-explicit-classes-fixes-2026-10-10/i18n-final-summary.json),
[catalog changes](../development-ideas/psychopdaROC-explicit-classes-fixes-2026-10-10/catalog-update-summary.json),
and [translation mappings](../development-ideas/psychopdaROC-explicit-classes-fixes-2026-10-10/completed-translations.json).

Future changes should keep a complete message inside `.()`, regenerate with the compiler,
merge only target changes into the umbrella catalogs, check placeholders, and rebuild
runtime JSON before installing the intended distribution.
