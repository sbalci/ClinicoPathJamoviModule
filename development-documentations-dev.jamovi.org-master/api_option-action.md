<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/option-action.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Action Option
description: "Documentation for the Action option, allowing analyses to trigger UI events like opening new datasets or opening files in their respective programs."
type: article
---

Actions allow an analysis to trigger specific events, such as opening a new dataset in a new jamovi window, or opening a file the analysis produced (a report, a spreadsheet, etc.) in another application.

Each action requires a minimum version of jamovi (after 2.7, jamovi's version numbering jumped to 28):

- `open`: jamovi 2.7.12 or newer
- `openExternal`: jamovi 28.4 or newer
- `export`: jamovi 28.7 or newer

If you use `openExternal` or `export`, declare this in your module's `0000.yaml` using `minApp`, so jamovi prevents installation on older versions that don't support it:

```yaml
minApp: 28.4.0  # or 28.7.0, if you use export
```

## Description

In the jamovi UI, an `Action` option is represented as a **Button**. When clicked, it triggers the specified action. Three actions are supported:

| Action | What happens |
|--------|--------------|
| `open` | Opens a dataset the analysis produced in a new jamovi window. |
| `openExternal` | Hands a file the analysis produced to the user. In the desktop app, the file is opened with whichever application the operating system associates with its extension (e.g. a `.docx` opens in Word). In a browser, the file is downloaded. |
| `export` | Saves a file the analysis produced. In the desktop app, the user is asked where to save it. In a browser, the file is downloaded. |

> [!TIP]
> Take care with the button's `title`. In a browser, `openExternal` and `export` both download the file, so a title like "Open in Word" promises something that won't happen there. Use **Open** for `openExternal`, and **Export** for `export`.

## YAML Properties

| Property | Type | Description |
|----------|------|-------------|
| `name` | string | The unique name of the option. |
| `type` | string | Must be `Action`. |
| `title` | string | The label displayed on the button. |
| `action` | string | The type of action to perform: `open`, `openExternal` or `export`. |

## R Implementation

In R, an `Action` option is represented as a `logical` value. It is `TRUE` when the user has clicked the button, and `FALSE` otherwise.

Checking `self$options$name` tells you the button was clicked, but does not execute the action. To execute it, retrieve the Option object and call its `$perform()` method, passing a function. This function receives an `action` object, and returns a `list` describing the result. What the list must contain depends on the action.

If the function throws an error, the error message is shown to the user as a notification.

## Examples

### Opening a dataset (`open`)

#### 1. Define in YAML

```yaml
- name: open
  title: Open
  type: Action
  action: open
```

#### 2. Implementation in R

Return the data frame as `data`, and an optional `title` for the new window:

```r
if (self$options$open) {
    option <- self$options$option('open')

    if (is.null(option$perform))
        return()

    option$perform(function(action) {
        list(
            data  = ToothGrowth,
            title = 'Results from Action'
        )
    })
}
```

### Opening a file in another application (`openExternal`)

#### 1. Define in YAML

```yaml
- name: export
  title: Open
  type: Action
  action: openExternal
```

#### 2. Implementation in R

Write the file to `action$params$fullPath` (a temporary path jamovi provides for you), and return the `filename` the user should see:

```r
if (self$options$export) {
    option <- self$options$option('export')

    if (is.null(option$perform))
        return()

    option$perform(function(action) {
        writeReport(action$params$fullPath)  # your own function
        list(
            filename = 'report.docx'
        )
    })
}
```

The result list takes the following properties:

| Property | Type | Description |
|----------|------|-------------|
| `filename` | string | **Required.** The name the user sees, e.g. as the title in Word or the name of the downloaded file. Its extension tells the operating system what kind of file it is, so make sure it's correct. |
| `path` | string | (optional) The location of the file, if you wrote it somewhere other than `action$params$fullPath`. The file is moved from here, so don't point it at a file you want to keep. |
| `title` | string | (optional) Defaults to `filename`. |

#### Things to be aware of

- **Supported file types:** For security, the desktop app only opens common document, data, and image formats directly: `pdf`, `docx`, `doc`, `odt`, `rtf`, `txt`, `md`, `xlsx`, `xls`, `ods`, `csv`, `tsv`, `json`, `xml`, `sav`, `omv`, `pptx`, `ppt`, `odp`, `png`, `jpg`, `jpeg`, `gif`, `svg`, `tif`, `tiff`, `bmp`, `webp`, and `zip`. Any other file is shown in the file manager (Finder, Explorer, etc.) instead, and the user can decide what to do with it. The same happens if no application is associated with the extension.

### Saving a file (`export`)

The `export` action works exactly like `openExternal`, except for what happens to the file at the end. In the desktop app, jamovi shows a save dialog (starting in the user's Documents folder), so the user chooses where the file goes, and is told once it's saved. In a browser, the file is downloaded, as with `openExternal`.

Use `export` when the user wants to keep the file, rather than look at it straight away.

#### 1. Define in YAML

```yaml
- name: saveCsv
  title: Export
  type: Action
  action: export
```

#### 2. Implementation in R

Write the file to `action$params$fullPath`, and return the `filename` the user should see. The result list takes the same properties as for [`openExternal`](#opening-a-file-in-another-application-openexternal):

```r
if (self$options$saveCsv) {
    option <- self$options$option('saveCsv')

    if (is.null(option$perform))
        return()

    option$perform(function(action) {
        write.csv(myResults, action$params$fullPath, row.names=FALSE)
        list(
            filename = 'results.csv'
        )
    })
}
```

Here `myResults` stands for a data frame your analysis has calculated. Your R code writes the file first; the save dialog appears afterwards.

#### Things to be aware of

- **The user can cancel:** If the user cancels the save dialog, nothing happens and no error is shown.
- **The filename suggests the name and type:** The `filename` is suggested as the name in the save dialog, and its extension is used to filter the dialog to that type of file.
- **Any file type can be exported:** Because the file is saved, not opened, `export` doesn't have the list of supported file types that `openExternal` has. This makes it the better choice for formats `openExternal` would only reveal in the file manager.
