# Table One — Turkish localization

Updated 2026-08-31 during implementation of the review recommendations.

## Implemented scope

All four authoritative files exist: `R/tableone.b.R` and
`jamovi/tableone.{a,r,u}.yaml`. Backend educational prose, summaries, warnings,
errors and owned labels now use `.()`; YAML labels are extracted automatically.
The existing generated NAMESPACE already imports `jmvcore::.`. The backend
roxygen declaration now also records that import explicitly.

Dynamic prose uses complete messages and named placeholders. `.formatText()`
substitutes only template tokens, preserving literal braces/backslashes in user
values. Copy-ready sentences use complete singular/plural alternatives, not
translated word fragments. HTML escaping remains at the presentation boundary.

The official compiler extracted 144 Table One messages. All have nonempty Turkish
translations in `jamovi/i18n/tr.po`; matching entries were added to `en.po` and
`catalog.pot`. Five existing translations were preserved. Unrelated catalog
entries were checked for semantic identity. No global catalog pruning occurred.

The follow-up release review adds three complete messages for reserved-category
collisions, bringing the scoped inventory to 147. `Missing (NA)` is translated
as `Eksik (NA)` and `Total (all cases)` as `Toplam (tüm olgular)`. A complete
About sentence explains collision handling. Catalog additions are scoped; the
official locale compiler and real Turkish analysis outputs validate them.

The full English/Turkish inventory is [tableone-tr-messages.tsv](tableone-tr-messages.tsv).
Examples of the added translations:

| English | Turkish |
| --- | --- |
| Analysis Summary | Analiz Özeti |
| Valid Percent | Geçerli Yüzde |
| Example interpretation | Yorumlama örneği |
| No cases left | Olgu kalmadı |
| Unused factor levels are not displayed. | Kullanılmayan faktör düzeyleri gösterilmez. |

## Terminology

- Case: olgu; row: satır. Neither implies a verified unique patient.
- Missing value: eksik değer; complete-case analysis: tam olgu analizi.
- Mean (SD): ortalama (SS); median: medyan; Q1/Q3 retained as quartile notation.
- Confidence interval: güven aralığı; p-value: p-değeri.
- Nominal/Ordinal, TRUE, NA, Unknown and N-Miss remain recognizable when they
  identify interface settings, literal data values or upstream-engine labels.

## Validation and release follow-up

- Catalogs parse with the compiler's gettext parser.
- Named placeholders match between English and Turkish for every scoped message.
- The compiler's locale builder generated the Turkish runtime JSON used by
  installed-analysis tests. Tests exercise translated summaries, counts, About,
  frequency headings and variable names containing braces/HTML-like text.
- Full desktop Turkish rendering, light/dark inspection and clipboard checks
  remain release tasks. Upstream package-generated table labels and raw engine
  error details may remain English; complete localization of those packages is
  not claimed. These translations should receive a native-speaker editorial pass.

## Maintenance commands

For a future module-wide update in a clean worktree:

```r
jmvtools::i18nUpdate("en")
jmvtools::i18nUpdate("tr")
```

During this scoped repair, extraction and runtime compilation were done in an
isolated module and only Table One entries were merged back. This avoids changing
unrelated translations. Catalog language headers remain `en`, `tr`, and `c`.

If Weblate publishing is desired, create a dedicated localization repository,
add the catalog and license, invite its bot, configure the hosted Weblate hook,
and coordinate registration with the jamovi team. No repository, webhook, bot
invitation or external publication was created by this repair.
