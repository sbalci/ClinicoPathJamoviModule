<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/image.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Image
description: "Technical reference for the jamovi Image results element, used for plots, charts, and graphical output."
type: article
---

The `Image` element is used for displaying graphical output, such as plots and charts, within jamovi results.

The values of properties can be accessed using the `$` operator. For example, to retrieve the title of an image, one can go:

```r
image$title
```

The methods of an image object are called using the `$` operator as well. For example:

```r
image$setVisible(FALSE)
```

## Properties

### name

a string specifying the name of the image.

### title

a string specifying the title of the image.

### visible

`TRUE` or `FALSE`, whether the image is visible or not.

### status

a string, one of `'complete'`, `'error'`, `'inited'`, `'running'`.

### state

the state object associated with the image.

### width

an integer specifying the width of the image in pixels.

### height

an integer specifying the height of the image in pixels.

### mode

either `'raster'` (the default) or `'vector'`. A `'vector'` image is rendered as an SVG rather than a PNG, so it stays sharp when zoomed or exported. As with the [Svg](/api/svg) element, keep an eye on file size: plots with one point per observation (such as scatterplots of large datasets) are better left as raster.

Older versions of jamovi ignore `mode`, and render the image as raster, so using it doesn't require a `minApp`.

### renderFun

a string specifying the name of the private method in the analysis class used for rendering the image.

### requiresData

`TRUE` or `FALSE`, whether the image requires data to be rendered. If `TRUE`, the image will only be rendered if data is available.

## Methods

### setStatus(status)

sets the image's status, should be one of `'complete'`, `'error'`, `'inited'`, `'running'`.

### setVisible(visible=TRUE)

overrides the image's default visibility.

### setTitle(title)

sets the image's title.

### setError(message)

sets the image's status to 'error', and assigns the error message.

### setState(object)

sets the state object on the image.

### setSize(width, height)

sets the width and height of the image.

### setMode(mode)

sets the image's mode, either `'raster'` or `'vector'`.

## Examples

### 1. Define the Image in YAML

First, you must define the image in your `.r.yaml` file and specify the `renderFun`.

```yaml
- name: plot
  title: My Awesome Plot
  type: Image
  width: 400
  height: 300
  renderFun: .plot
```

To render the image as an SVG, add `mode: vector`.

### 2. Implementation in R

In jamovi, images are typically rendered by defining a **private method** in your analysis class. In R6, private methods are functions defined in the `private` list of your class and their names usually start with a dot (e.g., `.plot`).

#### Prepare the Image
Inside the `.run()` function of your `.b.R` file, you can prepare the image. While you can access `self$data` directly in the rendering function, it is recommended to use `setState()` to store the specific data or objects needed for the plot. This allows jamovi to cache the image and avoid re-rendering it unnecessarily.

```r
.run = function() {
    image <- self$results$plot
    
    # Store data in state for caching and rendering
    image$setState(self$data)
}
```

#### The Rendering Function
The rendering function is where the actual plotting happens. It must return `TRUE` if successful.

```r
.plot = function(image, ...) {
    if (is.null(image$state))
        return(FALSE)
        
    plot_data <- image$state
    p <- ggplot(plot_data, aes(x=x, y=y)) + geom_point()
    print(p)
    
    TRUE
}
```

> [!IMPORTANT]
> The `renderFun` must be a private method and it must **print** the plot object (e.g., using `print(p)`) to the active device.
