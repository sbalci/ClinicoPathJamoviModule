# `sequentialtests` Turkish Translation Plan

## Scope

The four analysis files are present:

- `jamovi/sequentialtests.a.yaml`
- `jamovi/sequentialtests.r.yaml`
- `jamovi/sequentialtests.u.yaml`
- `R/sequentialtests.b.R`

YAML labels are extracted automatically. Backend notices, table labels, explanatory HTML,
formula text, and plot labels are marked with `.()`; dynamic sentences use named placeholders.
The source explicitly describes named scenarios as teaching examples, not clinical guidance or
validated diagnostic pathways.

## Catalog status

`en.po` and `tr.po` were regenerated with `jmvtools::i18nUpdate()`. `catalog.pot` was synchronized
from the English catalog and retains `Language: c`. New Turkish entries intentionally remain
untranslated until reviewed by a Turkish-speaking clinical/statistical translator; runtime will
fall back to the reviewed English source.

## Priority terminology

| English | Suggested Turkish |
|---|---|
| Teaching Example | Eğitim Amaçlı Örnek |
| Not Clinical Guidance | Klinik Rehber Değildir |
| Sensitivity | Duyarlılık |
| Specificity | Özgüllük |
| Positive Predictive Value (PPV) | Pozitif Prediktif Değer (PPD) |
| Negative Predictive Value (NPV) | Negatif Prediktif Değer (NPD) |
| Conditional independence | Koşullu bağımsızlık |
| Positive conditional dependence | Pozitif koşullu bağımlılık |
| Disease prevalence | Hastalık prevalansı |
| False positive | Yanlış pozitif |
| False negative | Yanlış negatif |

## Translator requirements

1. Preserve `%` conversion fields and `{name}` placeholders exactly.
2. Preserve HTML tags and mathematical notation while translating the surrounding prose.
3. Use formal, non-prescriptive clinical language.
4. Keep every teaching-example disclaimer explicit; do not translate examples as recommended
   pathways, clinical presets, or clinical algorithms.
5. Review the long explanatory HTML entries in rendered jamovi output, not only in a PO editor.

## QA

- Run `msgfmt --check --check-format` for `en.po`, `tr.po`, and `catalog.pot`.
- Build with `jmvtools::prepare()`.
- Switch jamovi to Turkish and inspect notices, tables, all optional explanatory panels, and plots.
- Confirm long text wraps correctly and no placeholder is shown literally or replaced by an
  ellipsis.
