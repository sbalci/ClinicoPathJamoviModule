# `decisioncalculator` Turkish localization plan

## Scope and files

All four analysis files are present:

- `jamovi/decisioncalculator.a.yaml`
- `jamovi/decisioncalculator.u.yaml`
- `jamovi/decisioncalculator.r.yaml`
- `R/decisioncalculator.b.R`

The backend's user-visible notices, footnotes, dynamic table text, summaries,
assumptions, glossary, and nomogram explanation are marked with `jmvcore::.`.
YAML text is extracted automatically. `NAMESPACE` is generated from the explicit
`@importFrom jmvcore .` roxygen tag.

The catalogs were refreshed with `jmvtools::i18nUpdate()`:

- `jamovi/i18n/en.po`
- `jamovi/i18n/tr.po`
- `jamovi/i18n/catalog.pot` (`Language: c`)

## Priority Turkish terminology

| English source | Preferred Turkish |
|---|---|
| Sensitivity | Duyarlılık |
| Specificity | Özgüllük |
| Positive predictive value | Pozitif öngörü değeri |
| Negative predictive value | Negatif öngörü değeri |
| Likelihood ratio | Olabilirlik oranı |
| Confidence interval | Güven aralığı |
| Reference standard | Referans standart |
| Youden's index | Youden indeksi |
| Diagnostic odds ratio | Tanısal odds oranı |
| Pre-test probability | Test öncesi olasılık |
| Post-test probability | Test sonrası olasılık |

Priority new messages:

| English | Suggested Turkish |
|---|---|
| Partial confidence intervals with population prevalence | Toplum prevalansı ile kısmi güven aralıkları |
| Weighted or fractional counts | Ağırlıklı veya kesirli sayımlar |
| Fractional frequencies are used for point estimates only. | Kesirli frekanslar yalnızca nokta tahminlerinde kullanılır. |
| Illustrative point estimates; not clinical guidance | Açıklayıcı nokta tahminleri; klinik rehber değildir |
| Reference standards can be imperfect. | Referans standartlar kusurlu olabilir. |
| Presets and scenarios are illustrative examples, not clinical guides. | Ön ayarlar ve senaryolar açıklayıcı örneklerdir; klinik rehber değildir. |

## Translation rules

- Preserve `%s`, `%g`, and `%.1f` placeholders exactly.
- Translate complete sentences rather than fragments.
- Keep abbreviations TP, FP, TN, FN, PPV, NPV, LR, MCC, and DOR when they are
  shown beside their expanded Turkish terms.
- Preserve HTML tags and attributes in translated HTML blocks; translate only
  their visible prose.
- Use an educational, descriptive tone and retain every statement that examples
  are not clinical guidance.

## QA status

- [x] Backend strings marked for extraction.
- [x] YAML strings available to automatic extraction.
- [x] English and Turkish PO catalogs refreshed.
- [x] POT template refreshed with the `c` language header.
- [x] PO syntax validated with `msgfmt` during release verification.
- [ ] Complete human review of all Turkish `msgstr` values in Weblate.
- [ ] In-app review for truncation and HTML rendering under the Turkish locale.

## Weblate handoff

Publish `catalog.pot` in the module's translation repository, add the Weblate bot
as a collaborator, configure the hosted Weblate GitHub webhook, and request that
the jamovi team connect the project. After translation, merge the updated
`tr.po`, run `msgfmt --check`, rebuild the module, and inspect the analysis in the
Turkish locale.
