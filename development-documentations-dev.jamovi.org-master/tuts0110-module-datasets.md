---
layout: ../layouts/BaseLayout.astro
title: "Module Datasets"
type: article
description: "Learn how to bundle example datasets within your jamovi module to make them available in the jamovi data library."
---

<!-- Vendored from https://github.com/jamovi/dev.jamovi.org/blob/07352e9b/src/content/docs/learn/tutorial/tuts0110-module-datasets.md
     (fetched 2026-10-08; published at https://dev.jamovi.org/tutorial/tuts0110-module-datasets). Newer than the rest of this snapshot. -->

When distributing a jamovi module, it is often helpful to include example datasets. These datasets can be used for tutorials, documentation, or to let users try out your analyses immediately without needing to find or format their own data.

By registering your datasets in your module, they automatically become available to users in jamovi's **Data Library**.

---

## 1. Directory Structure

To include datasets in your module, create a `data/` directory at the root of your module folder (at the same level as your `R/` and `jamovi/` directories) and place your dataset files inside it.

jamovi supports the following two formats for module datasets:
*   **`.omv`** (jamovi's native file format — recommended as it preserves variable types, descriptions, and analysis state)
*   **`.csv`** (Comma-separated values)

A module with example data has a structure like this:

```text
MyModule/
├── DESCRIPTION
├── NAMESPACE
├── R/
├── jamovi/
│   ├── 0000.yaml
│   ├── myanalysis.a.yaml
│   └── myanalysis.r.yaml
└── data/                    <-- Create this folder
    ├── clean_data.omv
    └── experimental_data.csv
```

---

## 2. Registering Datasets in `0000.yaml`

Once your dataset files are in the `data/` folder, you must define them under a `datasets` section in your `jamovi/0000.yaml` file.

Open your `jamovi/0000.yaml` and add a `datasets:` block at the top level:

```yaml
datasets:                    # <-- Add this block
  - name: clean_data
    path: clean_data.omv
    description: Pre-processed example dataset for ANOVA
    tags:
      - ANOVA
      - Tutorial
  - name: experimental_data
    path: experimental_data.csv
    description: Raw experimental results including covariates
    tags:
      - ANCOVA
      - Regression
```

### Dataset Parameters:

| Parameter | Required | Description |
| :--- | :--- | :--- |
| `name` | Yes | A unique identifier for the dataset. |
| `path` | Yes | The filename of the dataset inside the `data/` directory. |
| `description` | Yes | A short description explaining what the dataset contains. |
| `tags` | No | A list of tags to group and filter the dataset in the library. |

---

## 3. Data Size and Privacy Constraints

> [!WARNING]
> When bundling datasets, you must adhere to the following rules:
> *   **Keep Datasets Small:** Example datasets should be as lightweight as possible. They should exist only for demonstration and educational purposes.
> *   **No Sensitive Information:** These datasets are public and downloaded by anyone who installs your module. Ensure they contain absolutely no sensitive, confidential, or personally identifiable information (PII).

---

## 4. Building and Verifying

To compile the datasets into your module:

1.  **Rebuild the module:** Run `jmvtools::install()` in your R console (or use your IDE build tool). This packages the datasets into the compiled `.jmo` file.
2.  **Open jamovi:** Open the jamovi application.
3.  **Access the Data Library:** Click on the main menu icon (three horizontal bars in the top-left corner), select **Open**, and choose **Data Library**.
4.  **Find your module:** Your module's name will appear under the Data Library sections. Selecting it will display your registered datasets with their descriptions and tags. Click on a dataset to load it directly.

**Next Step:** Once your datasets are bundled, learn how to share your module in **[Distributing Modules](/tutorial/tuts0111-distributing-modules)**.
