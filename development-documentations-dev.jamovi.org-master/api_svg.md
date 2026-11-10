<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/svg.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Svg
description: "Reference for the Svg results element, which combines the interactivity of Html with the user experience of an Image."
type: article
---

The `Svg` element combines the interactivity of an [Html](/api/html) element with the user experience of an [Image](/api/image). Like `Html`, your analysis provides the content (SVG markup), along with JavaScript that can draw it and respond to the user. Like an `Image`, the result is a graphic the user can copy and export.

Use `Svg` for graphics the user interacts with in the results, such as highlighting, tooltips, or expanding parts of a diagram. For plots drawn with `ggplot2` or base R graphics, use [Image](/api/image) instead (an `Image` can also be rendered as an SVG, with [`mode: vector`](/api/image#mode)).

As with `Html`, there's no `renderFun`: you simply call `setContent()` from `.run()`.

Available in jamovi 28.3 and newer. Declare this in your module's `0000.yaml` using `minApp`, so jamovi prevents installation on older versions that don't support it:

```yaml
minApp: 28.3.0
```

## How it works

You write an `Svg` element as you would an `Html` element, but jamovi presents it as an image:

- **It's copied and exported like an image.** The user can copy it, or export it as an `.svg` or `.png` file, just like a plot. What's copied or exported is the SVG as it's currently displayed, including anything your scripts drew or changed in response to the user.
- **It still displays without your module.** When the user saves their file, jamovi also stores a snapshot of the finished SVG. If the file is later opened by someone who doesn't have your module installed, your scripts aren't available, so they see this snapshot instead.
- **It's sized by its markup.** There's no `width`, `height` or `setSize()`. The graphic takes the size given by the `width` and `height` attributes of your `<svg>`, and is scaled down if it's wider than the results panel.

> [!WARNING]
> Keep an eye on the size of your SVG. Every shape is part of the content, which is stored with the user's file and drawn by the results view. Plots with one shape per observation, such as scatterplots, are a poor fit: with 10 million observations, that's 10 million shapes. For these, a raster [Image](/api/image) is the better choice.

Because jamovi needs to know which part of the content is "the image", it uses the first top-level `<svg>` in the content. If your content contains more than one, add the class `jmv-results-svg-content` to the one that should be copied and exported.


## YAML Properties

| Property | Type | Description |
|----------|------|-------------|
| `name` | string | The unique name of the element. |
| `type` | string | Must be `Svg`. |
| `title` | string | The title displayed above the graphic. |
| `visible` | boolean or string | Whether the element is visible. |
| `clearWith` | array | The options that, when changed, clear the element's content. |
| `refs` | array or string | References to cite for this element. |
| `content` | string | (optional) Initial markup for the element. |

## Methods

### setStatus(status)

sets the element's status, should be one of `'complete'`, `'error'`, `'inited'`, `'running'`.

### setVisible(visible=TRUE)

overrides the element's default visibility.

### setTitle(title)

sets the element's title.

### setError(message)

sets the element's status to 'error', and assigns the error message.

### setState(object)

sets the state object on the element.

### setContent(content)

sets the markup of the element. `content` should be a string, typically containing an `<svg>` element. Setting new content causes the graphic to be drawn again.

### setScripts(scripts)

sets the JavaScript files to run when the content is displayed. `scripts` is a character vector of paths, relative to your module's `inst/` folder (e.g. a file at `inst/js/chart.js` is given as `'js/chart.js'`). Scripts run in the order they're listed, and are loaded *before* the content, so a script that works with the content should wait for the `DOMContentLoaded` event.

## Examples

### 1. Define the Svg element in YAML

An `Svg` element is defined in the `.r.yaml` file:

```yaml
- name: chart
  title: Group Means
  type: Svg
  clearWith:
    - dep
    - group
```

### 2. Set the Content in R

The simplest approach is to build the SVG markup directly in R. This example draws one bar per group:

```r
.run = function() {

    dep   <- self$options$dep
    group <- self$options$group

    if (is.null(dep) || is.null(group))
        return()

    data  <- self$data
    means <- tapply(
        jmvcore::toNumeric(data[[dep]]),
        data[[group]],
        mean,
        na.rm=TRUE)

    width  <- 60 * length(means)
    height <- 200
    scale  <- (height - 20) / max(means)

    bars <- sprintf(
        '<rect class="bar" x="%d" y="%f" width="40" height="%f" />',
        seq_along(means) * 60 - 50,
        height - means * scale,
        means * scale)

    svg <- sprintf(
        '<svg width="%d" height="%d">
            <style>.bar { fill: #3e6da9; }</style>
            %s
        </svg>',
        width, height,
        paste(bars, collapse=''))

    self$results$chart$setContent(svg)
}
```

Styles placed inside the `<svg>` (as above) are kept when the user copies or exports the graphic.

### 3. Adding interactivity with JavaScript

Scripts let the graphic respond to the user. Place the script in your module's `inst/` folder, for example `inst/js/chart.js`. If your script uses a library such as D3, include the library's file in `inst/` as well, and list it first, e.g. `setScripts(c('js/d3.min.js', 'js/chart.js'))`.

Continuing the example above, add a `<style>` rule for highlighted bars, and set the script after the content:

```r
    svg <- sprintf(
        '<svg width="%d" height="%d">
            <style>
                .bar { fill: #3e6da9; }
                .bar.highlighted { fill: #e07a1f; }
            </style>
            %s
        </svg>',
        width, height,
        paste(bars, collapse=''))

    chart <- self$results$chart
    chart$setContent(svg)
    chart$setScripts('js/chart.js')
```

The script highlights a bar when the user clicks it:

```js
// inst/js/chart.js
// scripts load before the content, so wait until the page is ready
document.addEventListener('DOMContentLoaded', () => {
    for (const bar of document.querySelectorAll('.bar')) {
        bar.addEventListener('click', () => {
            bar.classList.toggle('highlighted');
        });
    }
});
```

If the user copies or exports the graphic, the highlighted bars are highlighted in the copy as well. Scripts can also draw the graphic entirely, for example from data you put in the content as `data-` attributes.

> [!IMPORTANT]
> Your script runs each time the content is displayed (except when the snapshot is shown, as described above). Everything it draws must come from the content, because the script can't communicate with your analysis.
