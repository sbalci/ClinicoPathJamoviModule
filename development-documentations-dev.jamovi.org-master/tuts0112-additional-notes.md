<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/learn/tutorial/tuts0112-additional-notes.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: "Additional Notes"
type: article
description: "Advanced tips on dependency management, performance optimization, and R namespace best practices."
---

To wrap up this tutorial series, here are some important technical details regarding dependency management and performance optimization in jamovi modules.

## 1. Automated Dependency Management

When you run `jmvtools::install()`, jamovi doesn't just build your R code—it also identifies, downloads, and bundles every dependency listed in your `DESCRIPTION` file.

### Why bundle dependencies?
*   **Zero-Install for Users:** Users don't need to have R or any specific R packages installed on their system to use your module.
*   **Reproducibility:** Your analysis will produce the same results today as it will five years from now, regardless of changes to CRAN.

## 2. CRAN Snapshots

When jamovi builds modules for the library, it installs R package dependencies from a **fixed CRAN snapshot** rather than from live CRAN. This snapshot is taken at the start of each jamovi series (e.g. 2.6, 2.7) and remains frozen for the lifetime of that series.

### Why use a snapshot?
This protects users from breaking changes in third-party R packages. If a package author releases an update that changes behaviour or removes a function, modules built against the snapshot continue to work exactly as intended.

### The trade-off
Because the snapshot is frozen, the packages bundled with your module may lag slightly behind the latest versions available on CRAN. The snapshot is moved forward each time a new jamovi series is released — typically every 6–12 months.

### Using a newer package version
If you genuinely need a package version that is newer than the current snapshot, you can add a `Remotes:` field to your `DESCRIPTION` file pointing to a specific GitHub release or commit:

```text
Remotes: wviechtb/metafor#9b1c7081b6b451108670fea480e83d6cb49387e8
```

> [!WARNING]
> Use `Remotes:` sparingly and with care. Pinning a dependency to a version outside the snapshot can conflict with the same package used by other modules, potentially breaking analyses that users rely on.

## 3. Optimizing Performance with Namespaces

While it's common in standard R packages to use `import()` in the `NAMESPACE` file, we recommend using the `::` operator instead (e.g., `stats::t.test`).

### Benefits of this approach:
1.  **Lazy Loading:** Dependencies are only loaded into memory at the exact moment they are needed.
2.  **Reduced RAM:** Only the necessary parts of a package are loaded, keeping jamovi's memory footprint small.

## 4. Understanding Your Tools

| Tool | Primary Role | Used For... |
| :--- | :--- | :--- |
| `jmvtools` | **jamovi Bridge** | Creating modules, adding analyses, and installing into the jamovi application. |
| `devtools` | **R Development** | Standard R package tasks like testing (`test()`) and installing into your R console. |

**Next Step:** Your module is now ready for the world! Explore the **[Module Showcase](/showcase)** to see what others have built.
