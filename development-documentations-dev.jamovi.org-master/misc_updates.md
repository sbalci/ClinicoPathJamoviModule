<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/resources/misc/updates.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: API Updates
type: article
description: "Stay informed about the latest changes, ecosystem improvements, and API updates for jamovi developers."
---

Welcome to the jamovi developer updates page. Here we track important changes to the jamovi API and ecosystem improvements.

## October 2026

### 💾 Export Action (03-10-2026)
The 28.7 version of jamovi adds the **`export`** action to the [Action](/api/option-action) option. Like `openExternal`, it hands a file your analysis produced to the user, but in the desktop app it asks the user where to save the file rather than opening it.

---

## September 2026

### 📂 Open External Action (30-09-2026)
The 28.4 version of jamovi adds the **`openExternal`** action to the [Action](/api/option-action) option. It hands a file your analysis produced (a report, a spreadsheet, etc.) to the user: in the desktop app, it opens in the application associated with its file type, and in a browser, it's downloaded.

### 📎 File Option (30-09-2026)
The 28.4 version of jamovi adds the [**File**](/api/option-file) option, which lets the user select one or more files for your analysis to read. The files are stored in the user's `.omv` file, so they're still there when it's re-opened later.

### 🎨 Svg Results Element (18-09-2026)
The 28.3 version of jamovi adds the [**Svg**](/api/svg) results element. It combines the interactivity of an Html element with the user experience of an image: your JavaScript can make the graphic respond to the user, and users can copy and export it just like a plot.

### 📝 Text Results Element (18-09-2026)
The 28.3 version of jamovi adds the [**Text**](/api/text) results element, for wrapped, paragraph-style text with basic inline formatting. Prefer it to Html for narrative or explanatory text.

### 📐 Vector Images (18-09-2026)
The 28.3 version of jamovi lets [Image](/api/image#mode) elements be rendered as SVG, by adding `mode: vector`. Older versions of jamovi ignore it, and render the image as raster.

---

## June 2026

### 📚 New Guides (10-06-2026)
We've added several new guides to the documentation:
- [Plot Modules](/tutorial/tuts0304-plot-modules): when a module belongs in the Plots tab, and how to put it there.
- [Module Datasets](/tutorial/tuts0110-module-datasets): shipping example datasets with your module.
- [Output Variables](/tutorial/tuts0202a-output-variables): writing values from your analysis back to the spreadsheet.
- [Weighted Data](/tutorial/tuts0202b-weighted-data): supporting jamovi's weights in your analyses.

---

## May 2026

### 🚀 jamovi 28 Series (15-05-2026)
The 28 series of jamovi is out (jamovi's version numbering jumps from 2.7 to 28). This updates the **R version to 4.6.0**, and moves the [CRAN snapshot](/tutorial/tuts0112-additional-notes) forward to 2026-05-11.

**Update dev tools:**
You'll need to update your `jmvtools` to build against the 28 series:

```r
install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
```

---

## December 2025

### 🖼️ Image Sizing (31-12-2025)
The 2.7.16 version of jamovi introduces **image sizing** support for R-based modules. This allow developers to specify or constrain the dimensions of plot outputs more precisely.

---

## November 2025

### ⚡ New Analysis Action System (11-11-2025)
The 2.7.12 version of jamovi introduces the new **analysis action system**, providing a more robust way to handle user interactions and dynamic result updates.

---

## March 2024

### 🚀 jamovi 2.5 Series (08-03-2024)
The 2.5 series of jamovi is out. This updates the **R version to 4.3.2**, and moves the snapshot forward to 2024-01-09.

**Key Highlights:**
- **ARM / Apple Silicon Support**: Improved support for computers using ARM CPUs such as Chromebooks and the new "Apple Silicon" Macs. 
- **Architecture Specifics**: Note that modules are (typically) not portable between operating systems or architectures, so separate modules will need to be built for each.

**Update dev tools:**
You’ll need to update your `jmvtools` to build against the 2.5 series:

```r
install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
```

---

## Old News & Historical Changes

### Advanced UI Customisation (08-07-2019)
We’ve refined the advanced UI customization in jamovi 1.0.4 and newer. This is not backwards compatible with older 0.x versions.

### Analysis State (09-06-2017)
We’ve added a new document to our tutorial series describing how jamovi analyses can use **state**. State is used with longer running analyses, and allows the analysis to re-use results that were calculated previously, leading to much faster performance.

### jamovi 0.7.3 Dev Tools (20-04-2017)
Significant improvements to the development workflow:
- **Dependency Resolution**: Isolated system libraries from `jmvtools` for more reliable builds.
- **UI Definition (`.u.yaml`)**: The `.u.yaml` and `.a.yaml` files now work together. Labels and properties are now pulled directly from the analysis definition, reducing redundancy.
- **Compiler Modes**: Introduced `aggressive` (default) and `tame` modes for UI generation.

### Dev Mode & Debugging (02-04-2017)
jamovi 0.7.2.7 adds **dev mode**, providing a stack trace when an analysis errors. Read more in [Debugging an Analysis](/tutorial/tuts0104-input-checks).
