# jamovi docs sync - 2026-11

Run date 2026-11-09 (second dated report; previous: `jamovi_docs_sync_2026-10.md`). Unattended run; reading taken: compare upstream `jamovi/dev.jamovi.org` against the last vendoring (commit `07352e9`, 2026-10-07) and the repo guides, fix text only.

Method: this time GitHub **was** reachable from the computer shell (the cloud session still gets 403). `git clone https://github.com/jamovi/dev.jamovi.org` (HEAD `7145eb8`, 2026-10-13, verbatim sources), `jamovi/jmvtools` (HEAD `Version 28.17`, 2026-11-05) and jamovi.org/releases. `jamovi/jmvcore` is not a separate repo (it lives in `jamovi/jamovi`, not cloned; no CRAN/standalone check done). The `condensed_*` notes from last month are therefore replaced by verbatim copies.

## Upstream changes since last run (dev.jamovi.org, 07352e9..7145eb8, 9 files)

| Date | Commit | Change |
|------|--------|--------|
| 2026-10-10 | `3c45f04` | New page `api/svg` (results element `Svg`, minApp 28.3.0) and rows in results-definition / results-elements |
| 2026-10-10 | `5b5e012` | `api/image`: new `mode` (`raster`/`vector`) property and `setMode()`; "older versions ignore `mode`, so no `minApp` needed" |
| 2026-10-10 | `90bef96` | `api/option-action`: third action `export` (jamovi 28.7+); per-action version list; button-title advice (use **Open** / **Export**) |
| 2026-10-10 | `e7b3f81` | `resources/misc/updates`: entries May 2026 (28 series, R 4.6.0, CRAN snapshot 2026-05-11), June 2026 (new guides), Sep/Oct 2026 |
| 2026-10-10 | `30c6e7b` | `tuts0111`: updates are now submitted again through the `/submit` form (no more email/Slack) |
| 2026-10-13 | `7145eb8` | `api/text`: **minApp bumped from 28.3.0 to 28.11.0** |
| 2026-09-30 | `bde820a` (before last run, missed then) | `api/option-file`: requirement bumped to **28.4** (our vendored copy still said 28.3) |
| 2026-09-30 | `7d5653f` (before last run) | `openExternal` action documented (28.4) |

Not changed: no page removed or renamed since 07352e9. jamovi releases (jamovi.org/releases): 28.9 (10-10), 28.11 (10-12, Markdown copy, clipboard), 28.12, 28.13, 28.14 (10-21, CA certificates, a colour analysis option), 28.17 (11-02, latest); no new option/result types and no `minApp` news in the release notes. jmvtools 28.17 / jamovi-compiler 0.3.5 schemas: `Action.action` enum is now `open | openExternal | export`; `Svg` in the results schema; no `Notice` result type (so our empirical rule that `type: Notice` does not compile still holds; the upstream `api/notice` page is still ahead of the compiler - not re-run by me, no jmvtools install/compile was done).

## Contradictions with our guides (all fixed in text)

| # | Our guides said | Upstream now says | Fix |
|---|-----------------|-------------------|-----|
| 1 | `File` option and `Text` result both need `minApp: 28.3.0` (about 70 lines across guides, commands, skills) | `File` 28.4.0; `Text` 28.11.0 | Per-line corrections (File to 28.4.0, Text to 28.11.0, mixed lines list both); central table in patterns guide "Version Gating: minApp" |
| 2 | Action: only `open`; `openExternal` "unreleased, undocumented, do not use" | `open` 2.7.12, `openExternal` 28.4, `export` 28.7, all documented | actions guide "Supported Action Types" rewritten; patterns guide "Exists Upstream" list and "Later Releases" table updated |
| 3 | `Svg` "exists upstream but undocumented, do not use" | documented, minApp 28.3.0, interactive, poor for per-observation shapes | r.yaml guide, library review guide, patterns guide updated; still "do not adopt without a use case" |
| 4 | `mode: vector`: older-jamovi behaviour unverified | docs: older jamovi ignores `mode`, no `minApp` needed; `setMode()` exists | plots guide note; our static-in-`.r.yaml` advice unchanged |
| 5 | Snapshot `api_option-file.md` said 28.3 | 28.4 | replaced by verbatim upstream |

## Files changed (text only, nothing committed)

- Guides: `vignettes/jamovi_{a_yaml,actions,b_R,i18n,js,library_review,module_patterns,notices,plots,r_yaml,tables,u_yaml}_guide.md` and `README_GUIDES.md`; identical edits applied to the copies in `.claude/skills/audit-module/guides/` (including `jamovi_a_yaml_guide.md`, which already differed before; edits there were applied by exact-string replacement only).
- Skills/commands (minApp thresholds only, no description changes): `.claude/commands/{check-function,check-function-full,check-module,create-function,fix-notices,review-function}.md`, their copies under `.claude/skills/audit-module/bundled/`, `.claude/skills/{audit-module,release-review-function,library-audit,security-audit-function}/...`.
- Snapshot `development-documentations-dev.jamovi.org-master/`: new/refreshed verbatim pages `api_{svg,option-action,image,text,notice,option-file,option-terms,option-group,option-output,array,group,html,preformatted,output}.md`, `misc_updates.md`, `tuts0111`, `tuts0112`, `tuts0200`, `tuts0201a`, `tuts0202a`, `tuts0202b`, `tuts0204`, `tuts0301`-`tuts0304` (each with a provenance comment). The four `condensed_*.md` files were moved to `_superseded/`. The old Rmd tutorials `tuts0102`-`0108` remain stale vs. the re-numbered upstream (`tuts0102`-`0109` now differ in topic); not rewritten.
- The number-of-lines-per-file corrections were made by a script keyed on whether a line mentions File, Text or both; **spot-check a few mixed lines** (e.g. `.claude/commands/review-function.md` line 38 reads awkwardly: "Text ... once the module has `minApp: 28.4.0` (File) / `28.11.0` (Text)").
- Section heading anchors (`#file-jamovi-283`, `#text-jamovi-283`) were deliberately kept; the headings still say "28.3+" but the body text under them now says 28.4 / 28.11. `.claude/` commands/skills descriptions did not change, so `build_playbook_index.py` was **not** run.

## Recommendations / decisions for you (not done)

1. **Stale code and instructions (not md guides, so untouched):** `tools/release_gate.py` `check_min_app()` still uses 28.3.0 for both File and Text and so under-reports for Text (a module at `minApp` 28.3-28.10 with a `Text` result passes the gate but is documented as unsupported). Also `CLAUDE.md` line ~464 ("File and Text need `minApp: 28.3.0`"). Suggest: File 28.4.0, Text 28.11.0, and drop the `mode: vector` WARN or keep it as conservative.
2. **Is the Text 28.11 bump real?** It is odd that a 28.3 feature now needs 28.11 (jamovi 28.11 notes mention only Markdown copy / clipboard). Worth a one-line check with the jamovi team or a test on 28.3-28.10 before relying on either number. If you already shipped `Text` at 28.3.0 anywhere, re-test there. Current `jamovi/0000.yaml` has `minApp: 2.7.27` (untouched; the file already had uncommitted changes before this run). A crude grep found no `type: File`/`Svg` and no `openExternal`/`export` action in `jamovi/*.yaml`; confirm with `tools/release_gate.py`.
3. `export` Action (28.7) could provide "save table/report as CSV/docx" buttons; needs `minApp: 28.7.0` (module-wide) - product decision.
4. `Svg` is only useful for interactive diagrams (e.g. decision-tree / Markov graphs in `decisiongraph`); not adopting.
5. Re-test `type: Notice` with jmvtools 28.17 (open item from last month; compiler schema still lacks it).
6. Resubmission procedure changed (module submission form at dev.jamovi.org/submit instead of email/Slack): update any release checklist outside this repo.
7. The working tree has many uncommitted modifications unrelated to this run (`R/*.b.R`, `jamovi/0000.yaml`, ...), so review `git diff -- vignettes .claude development-documentations-dev.jamovi.org-master` separately.
