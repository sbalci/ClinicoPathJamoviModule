# Internationalization (i18n) Translation Plan: agepyramid → Turkish (TR)

Updated 2026-09-03 after the `/prepare-translation` pass. This replaces an earlier
plan that wrongly reported the backend as already wrapped.

## 0) Files

- `jamovi/agepyramid.a.yaml`, `jamovi/agepyramid.u.yaml`, `jamovi/agepyramid.r.yaml`
  (strings auto-extracted)
- `R/agepyramid.b.R` (111 `.()` wraps, 110 unique msgids)

## 1) NAMESPACE

`import(jmvcore)` is present, so `.()` resolves. No change needed.

## 2) What was wrapped in `R/agepyramid.b.R`

- Notice prefixes (`ERROR`, `WARNING`, `NOTE`) and all 19 notice titles and bodies.
  Bodies are single complete sentences with `{}` placeholders filled through
  `jmvcore::format()`; the two notices that used to splice "female"/"male" into one
  sentence are now two complete alternatives.
- The four `reject()` messages.
- The welcome panel: HTML structure stays outside `.()`, each phrase is wrapped and
  HTML-escaped on the way in.
- The Data Summary panel labels and the "Single-gender ({level})" cohort line.
- Table note (both percentage bases), plot axis titles, legend labels, default titles,
  and the ggcharts fallback text (one `.()` per sentence, wrapped with `strwrap`).
- Preset names used inside the right-closure note.

Placeholder rules followed: camelCase names only (`{maxAge}`, `{nBands}`) because
`jmvcore::format()` ignores underscored placeholders; no placeholder named `s`/`st`
(partial-matches the `str` formal); no `.()` string ends in `]` (treated as msgctxt).

## 3) Catalog update (done)

```r
jmvtools::i18nUpdate("en"); jmvtools::i18nUpdate("tr")
```

```bash
msgfmt -c jamovi/i18n/tr.po      # 1724 translated, 0 errors
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot   # header set to "Language: c"
```

Note: `jmvtools::i18nUpdate()` wraps long msgids over several `"..."` lines. A fill
script must join the pieces before matching on msgid text.

## 4) Turkish translations (filled in `jamovi/i18n/tr.po`)

All 158 agepyramid msgids (backend + YAML) carry a Turkish msgstr. Key terms:

| English | Turkish |
| --- | --- |
| Age Pyramid | Yaş Piramidi |
| Female / Male | Kadın / Erkek |
| Female level / Male level | Kadın düzeyi / Erkek düzeyi |
| Age group preset | Hazır yaş grubu |
| Age band boundaries; left-closed / right-closed | Yaş aralığı sınırları; sol-kapalı / sağ-kapalı |
| Bin width (years) | Aralık genişliği (yıl) |
| Custom age breaks | Özel yaş sınırları |
| Percentage base: within each gender / of all observations | Yüzde tabanı: her cinsiyet içinde / tüm gözlemlere göre |
| Bar values: counts / percentages | Çubuk değerleri: sayılar / yüzdeler |
| WHO/UN standard; WHO abridged | DSÖ/BM standart; DSÖ kısaltılmış |
| Pediatric / Reproductive / Geriatric / Life course | Pediatrik / Üreme çağı / Geriatrik / Yaşam evreleri |
| Population Data; Population Count | Nüfus Verisi; Kişi Sayısı |
| Data Summary; Excluded | Veri Özeti; Dışarıda bırakılan |
| observation(s) | gözlem |
| band | aralık |

Style: plain clinical Turkish, second person formal ("kontrol edin"), percentages
written `%{pct}` (Turkish places the sign before the number), decimal comma in fixed
text (14,6 yaş), placeholders untouched.

## 5) QA

- [x] All user-visible backend strings wrapped (grep for unwrapped quoted phrases is clean)
- [x] No `.()` with leading/trailing space, `\n`, or a trailing `]`
- [x] Placeholders identical between msgid and msgstr (checked by the fill script)
- [x] `msgfmt -c` passes for en.po, tr.po, catalog.pot
- [x] No `msgctxt` generated for agepyramid strings
- [x] Rendered outputs (sourced backend) show filled placeholders, no literal braces
- [ ] Open in jamovi with language set to Turkish and read every panel

## 6) Weblate

The module-level `catalog.pot` is refreshed; push it to the `<module>-i18n` repo per
the module's existing Weblate setup.
