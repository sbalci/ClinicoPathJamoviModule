# Repository Guidelines

## Project Structure & Module Organization
- `R/` holds hand-written analyses (`*.b.R`) and generated bases (`*.h.R`); helper utilities live alongside.
- `jamovi/*.a|u|r.yaml` define options, UI, and results; `jamovi/0000.yaml` anchors shared references.
- `tests/testthat/` contains unit and integration coverage; mirror feature names as `test-<feature>.R`.
- Documentation lives in `man/`, `vignettes/`, and the pkgdown site under `docs/`; metadata is in `DESCRIPTION` and `NAMESPACE`.
- Reusable datasets and fixtures belong in `data/` (compressed `.rda`) or `data-raw/` for generation scripts.

## Build, Test, and Development Commands
- `Rscript -e "devtools::document()"` regenerates roxygen documentation before commits touching `R/`.
- `Rscript -e "devtools::test()"` runs the full testthat suite; prefer focused files via `testthat::test_file()`.
- `Rscript -e "devtools::check()"` mirrors CI; use before release or structural changes.
- `Rscript -e "pkgdown::build_site()"` refreshes the docs site; run after editing `vignettes/` or roxygen examples.
- Fast iterations: `Rscript -e "rcmdcheck::rcmdcheck(args = c('--no-manual'))"` catches failures without rebuilding the manual.

## Coding Style & Naming Conventions
- Follow tidyverse style: 2-space indents, `<-` for assignment, snake_case objects, and ≤100-character lines.
- Name analysis pairs as `analysis.b.R` and `analysis.h.R`; align option names with YAML definitions verbatim.
- Use roxygen2 tags above exported functions; prefer `dplyr` pipelines over deeply nested loops when practical.
- Keep comments concise and functional; avoid describing obvious assignments.

## Testing Guidelines
- Write tests in `tests/testthat/test-<feature>.R`; organize helpers in `helper-*.R`.
- Cover edge cases (single-level factors, missing values, seed-controlled simulations) and ensure deterministic snapshots.
- Validate tables with `expect_equal()` or snapshot tests; assert plot objects via `expect_s3_class()` and layer checks.

## Commit & Pull Request Guidelines
- Commits use imperative verbs (`Add`, `Fix`, `Refactor`) and focus on a single concern; avoid "WIP" labels.
- PRs require a summary, linked issues (`Fixes #NNN`), and screenshots for UI/plot updates; call out breaking changes explicitly.
- Update documentation and tests in the same PR when behaviour shifts; note follow-up tasks in `TODO.md` if deferred.

## Security & Configuration Tips
- Never commit PHI/PII; anonymize inputs and rely on illustrative datasets in `inst/extdata/`.
- Respect CI parity: run `devtools::check()` on macOS or Linux when possible to match GitHub Actions expectations.
