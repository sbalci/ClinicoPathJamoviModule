# jamovi docs sync - 2026-10

Run date 2026-10-08 (first dated report; the vendoring of `tuts0110`, `tuts0111` and `submit.md` was also done today, upstream commit `07352e9b`). Method: live pages on dev.jamovi.org and jamovi.org/releases via a summarising fetch tool. The GitHub API/commit list was **not reachable** from the scheduled session (403 / robots), so "commits since last run" could not be listed; the page list below stands in for the docs tree. Because the fetch tool summarises, new pages were vendored as `condensed_*.md` (labelled), not verbatim.

## Upstream state

- jamovi releases: 28.4 (2026-09-30) adds .docx/.odt export and **module support for opening external files**; 28.7 (2026-10-03) adds **module support for exporting files**; 28.5/28.6/28.8 are fixes. No developer-docs page for either file feature was found (module-definition page lists only name, title, version, jms, authors, description, category).
- dev.jamovi.org now has a restructured site (tutorials `tuts0101`-`tuts0112` re-numbered, plus `0200`-`0204`, `0301`-`0304`) and option pages Terms, Group, Action, Output, File; result pages Image, Group, Array, Preformatted, Text, Html, Notice, Output.
- API Updates page newest entries: Image Sizing (2.7.16, 2025-12-31), Action system (2.7.12, 2025-11-11).

## Changes relevant to us

| # | Finding | Our docs said | Action taken |
|---|---------|---------------|--------------|
| 1 | `setSize2(widthM, heightM, widthB, heightB)`, jamovi 2.7.16 | not documented | added to plots guide; guarded use needs no minApp raise |
| 2 | `weightsSupport` header (`auto`/`full`/`integerOnly`/`none`), `attr(data,'jmv-weights')` | not documented; 0 analyses set it | added to a.yaml guide |
| 3 | `api/notice` documents `type: Notice` in `.r.yaml` + `setType`/`NoticeType` | "does not compile" (verified on 28.3.1) | contradiction noted in notices guide; no change to rule |
| 4 | 28.4 open-external-files, 28.7 export-files (release notes only) | absent | listed as "do not use, undocumented" in patterns guide |
| 5 | Action option: only `action: open` supported; `openExternal` still unmentioned | actions guide already lists `openExternal` as unreleased | none |
| 6 | Plot themes page: use `ggtheme` + `theme$fill`/`theme$color`; never apply `theme_bw()` manually | review guide §23 (palette) consistent | none |
| 7 | Image-state tutorial: store minimum data, never `self$data`/models in state, use `requiresData: true` for raw points | patterns/plots guides already say so | none |
| 8 | Additional notes: prefer `pkg::fn` over NAMESPACE imports (lazy loading); `Remotes:` sparingly | guides agree (we also need `@importFrom` for bare symbols, see CLAUDE.md) | none |

Removed/deprecated upstream: none seen. Old snapshot pages `tuts0102`-`tuts0107` remain stale vs the re-numbered site (the old `tuts0107` is already marked superseded).

## Files changed (all text, no commit)

`vignettes/jamovi_plots_guide.md`, `jamovi_a_yaml_guide.md`, `jamovi_notices_guide.md`, `jamovi_module_patterns_guide.md`, `README_GUIDES.md`; new `condensed_*.md` x4 in the snapshot; this report. The identical copies under `.claude/skills/audit-module/guides/` were refreshed for plots, patterns, notices and README_GUIDES; `jamovi_a_yaml_guide.md` there already differed from `vignettes/` before this run and was left alone. No skill/command descriptions changed, so `build_playbook_index.py` was not run.

## Recommendations / decisions for you (nothing done)

1. Re-test `type: Notice` with the newest jmvtools; upstream docs may be ahead of the compiler (minimal recipe in the notices guide). If it compiles, `minApp` implications must be checked first.
2. Consider `weightsSupport: none` or `integerOnly` for analyses where weighted counts would mislead (e.g. survival, agreement); today everything is `auto`. Needs per-analysis review (code change).
3. Consider `setSize2` for faceted plots (code change, guarded, no minApp change).
4. Watch for dev docs on 28.4/28.7 file open/export; potentially useful for an export of tables/reports, but undocumented, so do not use yet.
5. Replace the `condensed_*.md` files with verbatim copies from `jamovi/dev.jamovi.org` (needs GitHub access in the scheduled session), and vendor the remaining new pages (`option-terms/group/output`, `api/image`, `tuts0200`-`0204`, `0301`, `0302`, `0304`, `0112`).
6. `minApp` in `jamovi/0000.yaml` is 2.7.27 and was not touched.
