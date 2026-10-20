# Agreement Turkish localization plan — 2026-10-20

The backend and the three YAML schemas exist. All 1,535 extracted agreement guide templates now participate in jamovi translation lookup, with markup supplied as named format values. Existing translations were preserved; 48 previously empty guide headings now have Turkish translations. Of these guide templates, 117 have a Turkish msgstr and 1418 remain untranslated. This is extraction readiness plus partial localization, not a completed Turkish edition.

## Remaining translation work

The complete machine-readable queue is [agreement-tr-missing.csv](../quality-reports/meddecide-audit-fixes-2026-10-20/agreement-tr-missing.csv). Translate whole sentences/cells; keep every `{tagN}` token exactly once and preserve numeric, percentage and printf values. Inline tags and entities are inserted after translation. Source whitespace and the original English HTML were independently checked: 269/269 converted literals are unchanged under identity translation.

| English | Turkish wording used |
|---|---|
| What is Gwet's AC Coefficient? | Gwet AC katsayısı nedir? |
| What is Bland-Altman Analysis? | Bland-Altman analizi nedir? |
| What is ICC (Intraclass Correlation Coefficient)? | Sınıf içi korelasyon katsayısı (ICC) nedir? |
| What are Specific Agreement Indices? | Özgül uyum indeksleri nedir? |

## Terminology and QA

Use “gözlemciler arası uyum”, “gözlemci içi güvenirlik”, “duyarlılık”, “özgüllük” and “Güven Aralığı (GA)”. Preserve established method names and distinguish limits-of-agreement coverage from the confidence level of an interval. Do not translate programmatic identifiers, variable names or supplied level labels.

Official `jmvtools::i18nUpdate()` ran on the isolated module. Only generated entries/references belonging to the affected analyses were merged into the umbrella catalogs, preserving unrelated entries and existing msgstr values. English/Turkish `msgfmt -c` checks pass. A runtime regression supplies a Turkish translator and verifies both translated text and preserved HTML. It does not certify every Turkish paragraph or desktop language switching.

After filling the queue, run `msgfmt -c -o /dev/null jamovi/i18n/tr.po`, regenerate a test-module candidate, and inspect several long guides in Turkish in jamovi. Community hosting/contact and language review remain separate maintainer operations; no message was sent.
