# Internationalization (i18n) Translation Plan: alluvial → Turkish (TR)

Prepared 2026-09-04 during the post-review pass (analysis version 1.0.9).

## 0) Files

- `jamovi/alluvial.a.yaml`, `jamovi/alluvial.u.yaml`, `jamovi/alluvial.r.yaml` (auto-extracted)
- `R/alluvial.b.R` (124 `.()` wraps, 120 unique msgids)

## 1) NAMESPACE

`import(jmvcore)` present; `.()` resolves. No change.

## 2) What was wrapped

- Notice prefixes and all 23 notice titles and bodies. Multi-sentence bodies are one
  `.()` per sentence joined with `"\n"`, so each sentence translates on its own.
- The "How to read this diagram" notice: every line is one complete sentence with
  `{}` placeholders (`{n}`, `{total}`, `{weight}`, `{label}`, `{path}`, `{pct}`); the
  "Ignored by the ... engine" line reuses the option titles as msgids so the YAML
  translations apply.
- The welcome panel: HTML structure outside `.()`, each phrase wrapped and escaped.
- The condensation panel HTML, the flow-table notes, and both in-image fallback
  messages (`.wrapText()` re-wraps the translated sentences at 62 columns).

Rules followed: camelCase placeholders only (`jmvcore::format()` ignores underscores);
no placeholder named `s`/`st`; no `.()` string ends in `]`; no leading/trailing space.

## 3) Catalog update (done)

```r
jmvtools::i18nUpdate("en"); jmvtools::i18nUpdate("tr")
```

```bash
msgfmt -c jamovi/i18n/tr.po         # 0 errors
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot   # header set to "Language: c"
```

Long msgids are wrapped over several `"..."` lines in the .po; the fill script joins
the pieces before matching (see `fill_generic.py` in the session scratchpad).

## 4) Turkish translations (filled in `jamovi/i18n/tr.po`)

All 203 alluvial msgids (120 backend + 92 YAML, 4 of them already translated module-wide) carry a Turkish msgstr. Key terms:

| English | Turkish |
| --- | --- |
| Alluvial diagram | Alüvyal diyagram |
| stratum / strata | katman / katmanlar |
| ribbon width | şerit genişliği |
| path (through the variables) | yol |
| case(s) | olgu |
| weight variable; weight total | ağırlık değişkeni; ağırlık toplamı |
| fill variable | dolgu değişkeni |
| condensation variable / plot | yoğunlaştırma değişkeni / grafiği |
| marginal plots | kenar grafikleri |
| plot engine | grafik motoru |
| flow direction; flow table | akış yönü; akış tablosu |
| node labels; counts on nodes | düğüm etiketleri; düğümlerde sayılar |
| Missing-value exclusion (NA) | Eksik değer dışlama (NA) |
| Why this matters / What to do next | Neden önemli / Ne yapmalı |

Engine and package names (Easy Alluvial, GG Alluvial, ggalluvial, Viridis, Plasma,
Set3, Pastel1, Dark2) are left as proper nouns. Percent signs precede the number
(`%{pct}`), as in Turkish typography.

## 5) QA

- [x] All user-visible backend strings wrapped (heuristic grep clean)
- [x] Placeholder parity checked by the fill script
- [x] `msgfmt -c` passes for en.po, tr.po, catalog.pot
- [x] No `msgctxt` generated for alluvial strings
- [x] Both alluvial test files pass under `devtools::load_all()` after wrapping
- [ ] Open in jamovi with language set to Turkish and read every panel

## 6) Weblate

`catalog.pot` refreshed; push to the module's `-i18n` repository as usual.
