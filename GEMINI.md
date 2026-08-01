# Jamovi development playbooks — read these first

Task-specific procedures for this module (create, review, fix, document, translate, or
release-check a single jamovi analysis) live in [`.claude/commands/`](.claude/commands/) as plain
markdown with YAML frontmatter. Nothing in them needs a Claude-specific runtime — any agent can
read and follow one. `ls .claude/commands/` is the list; each file's frontmatter `description`
says when it applies.

**Before starting jamovi work, open the playbook matching the task and read the whole file.**
The frontmatter is only a routing hint; the body is the procedure. Translate Claude-Code
vocabulary as you go: `$ARGUMENTS` means the target the user named (usually one analysis, e.g.
`singlearm`); "the Skill tool", "invoke the skill", and `/some-command` all just mean *read the
corresponding file and follow it*.

Also read [`CLAUDE.md`](CLAUDE.md) and [`AGENTS.md`](AGENTS.md). Despite the name, `CLAUDE.md`
is repository-wide guidance — architecture, the generated-file policy, and a long list of
hard-won pitfalls — and it applies regardless of which agent you are.

Some machines carry extra local-only playbooks under `.claude/skills/`, deliberately not
committed. If `.claude/PLAYBOOKS.md` is present it indexes everything available on this machine;
regenerate it with `python3 .claude/scripts/build_playbook_index.py`. Its absence is normal — the
commands above are the committed baseline.

---

## Project overview

This is an R package for clinicopathological research. It has a large number of dependencies, suggesting a wide range of statistical and visualization capabilities. The package seems to be well-documented, with a `VignetteBuilder` and multiple URLs for documentation and bug reports.

This project is a jamovi module for clinicopathological research. It provides a wide range of statistical analyses and visualizations, including:

*   **Descriptive Statistics:** `summarydata`, `tableone`, `gtsummary`
*   **Agreement and Reliability:** `agreement`, `icccoeff`, `kappasizeci`
*   **Survival Analysis:** `survival`, `comparingsurvival`, `multisurvival`, `onesurvival`
*   **Diagnostic Tests:** `roc`, `decisioncurve`, `screeningcalculator`
*   **Data Visualization:** `advancedbarplot`, `raincloud`, `waterfall`, `swimmerplot`
*   **Data Quality and Preprocessing:** `checkdata`, `missingdata`, `datecorrection`, `ihcheterogeneity`

The project is well-structured and appears to be actively maintained. The use of `jamovi` suggests a focus on providing a user-friendly interface for these complex analyses.

## Development Structure and Process

This project follows a standard R package structure and incorporates best practices for development and documentation.

*   **Core Structure:** The project is organized as a standard R package with `R/` for source code, `man/` for documentation, `tests/` for unit tests, and `vignettes/` for long-form guides and examples.
*   **Jamovi Integration:** As a jamovi module, the `jamovi/` directory contains the YAML definitions for the user interface components that appear in the jamovi software. These files control the options and layout for each analysis.
*   **Dependency Management:** Dependencies are explicitly declared in the `DESCRIPTION` file, which includes a large number of packages from CRAN and some from GitHub via the `Remotes` field.
*   **Documentation:**
    *   In-code documentation is written using `roxygen2` conventions.
    *   The project website is built using `pkgdown`.
    *   Vignettes are created using `knitr` and `quarto`.
*   **Testing:**
    *   Unit tests are located in the `tests/` directory, likely using the `testthat` framework.
    *   Code coverage is tracked using Codecov, configured via `codecov.yml`.
*   **Data:**
    *   The `data/` directory stores the compressed `.rda` files used by the package examples and tests.
    *   The `data-raw/` directory (inferred standard practice) and scripts like `create_test_data.R` are used to process raw data into the final `.rda` format.
*   **Continuous Integration:** GitHub Actions are used for CI/CD, with workflows defined in the `.github/workflows/` directory. This likely automates tasks like testing, checking the package, and deploying the documentation website.