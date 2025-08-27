# Repository Guidelines

## Project Structure & Module Organization
- Core R code: `R/` (`*.b.R` analysis implementations, `*.h.R` generated bases).
- Jamovi specs: `jamovi/*.a.yaml`, `jamovi/*.u.yaml`, `jamovi/*.r.yaml` plus `0000.yaml` references.
- Tests: `tests/testthat/test-*.R` with unit/integration coverage for analyses and helpers.
- Docs: `man/` (roxygen2), `vignettes/` (articles), `docs/` (pkgdown site).
- Package metadata: `DESCRIPTION`, `NAMESPACE`; CI: `.github/workflows/`.

## Build, Test, and Development Commands
- Document: `devtools::document()` — regenerate `man/` from roxygen2.
- Test: `devtools::test()` — run testthat suite in `tests/testthat`.
- Check: `devtools::check()` — run R CMD check (as CI does).
- Site: `pkgdown::build_site()` — rebuild documentation site.
- Optional fast check: `rcmdcheck::rcmdcheck(args = c("--no-manual"))`.

## Coding Style & Naming Conventions
- R style: tidyverse-friendly; 2-space indent; limit lines to ~100 chars.
- Names: lower_snake_case for functions/objects; analysis files `name.b.R` and `name.h.R`.
- R6 analyses: access options via `self$options$...` and results via `self$results$...`.
- YAML: option names in `.a.yaml` must match R6; results in `.r.yaml` referenced exactly in code.

## Testing Guidelines
- Framework: testthat; place files as `tests/testthat/test-<feature>.R`.
- Cover edge cases: small n, single-level factors, missing values, deterministic seeds.
- Plots: assert class/layers; tables: snapshot or value checks where stable.
- Run locally with `devtools::test()`; ensure `devtools::check()` passes before PR.

## Commit & Pull Request Guidelines
- Commits: imperative mood, concise scope (e.g., “Add decision curve helpers”). Avoid “WIP”.
- PRs: clear description, linked issues (`Fixes #123`), screenshots for UI/plots, note breaking changes.
- Update tests/docs when changing behavior; keep changes focused.

## Security & Configuration Tips
- Do not commit PHI/PII. Use synthetic or example datasets.
- Large data goes in `data/` as compressed `.rda`; prefer small reproducible fixtures in tests.
- CI runs on multiple OS/R versions; reproduce locally with `devtools::check()` before pushing.

## Architecture Overview
- Analyses are R6 classes in `R/*.b.R` inheriting from generated `*.h.R`.
- Lifecycle: `.init()` prepares results/tables; `.run()` validates inputs, computes, and populates results.
- Plots: private `.plotX(image, ggtheme)` functions; store inputs in `image$state` and `print(p)`.

## Jamovi Dev Notes
- Options: defined in `.a.yaml`; access exactly as `self$options$name`.
- UI: `.u.yaml` maps controls to options; keep names consistent.
- Results: `.r.yaml` objects referenced via `self$results$...`; use `clearWith` and `visible` wisely.
- Common pattern: preprocess in helpers (e.g., `.prepareData()`, `.buildTableRows()`), then fill results with `$setContent()`, `$addRow()`, `$setRow()`.
