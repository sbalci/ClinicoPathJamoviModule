<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/learn/tutorial/tuts0302-plot-themes.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: "Plot Themes"
type: article
description: "Learn how jamovi automatically applies themes to your plots for a native look and feel."
---

Consistency is a core value of the jamovi experience. When users change their global theme settings in jamovi, they expect all plots across all modules to update accordingly. To achieve this, jamovi automatically passes theme information to your plotting function.

## The `.plot` Signature

> [!IMPORTANT]
> **Where do these objects come from?**
> jamovi **automatically passes** `ggtheme` and `theme` to your `.plot` function. You don't need to define them yourself; they are provided by the jamovi rendering system to ensure your plot matches the user's settings.

Instead of manually applying a theme like `theme_bw()` or `jmvcore::theme_joy()`, you should use the `ggtheme` and `theme` arguments provided by jamovi.

The correct signature for your rendering function is:

```r
.plot = function(image, ggtheme, theme, ...) {
    # ... plotting logic
}
```

## Using `ggtheme`

The `ggtheme` argument is a pre-configured `ggplot2` theme object. It contains all the structural styling (fonts, gridlines, background colors) that matches the user's current jamovi settings.

To apply it, simply add it to your `ggplot` object:

```r
.plot = function(image, ggtheme, theme, ...) {
    plotData <- image$state
    
    p <- ggplot(plotData, aes(x=x, y=y)) +
        geom_point() +
        ggtheme # Apply the jamovi-provided theme
    
    # Return the ggplot2 object
    return(p)
}
```

### Why use `ggtheme`?

1.  **Native Look and Feel:** Your plots will automatically match the jamovi UI, whether the user is using the default theme, a dark theme, or a high-contrast theme.
2.  **Export Quality:** `ggtheme` is optimized for high-quality exports to PDF and Word, ensuring fonts and line weights are appropriate for publication.
3.  **User Preference:** It respects the user's global font size and family settings.

## Using `theme`

While `ggtheme` handles the overall look, the `theme` argument is a list containing specific color and fill properties. This is useful when you want your `geoms` (like bars or points) to use the official jamovi color palette.

Common properties include:
- `theme$fill`: A vector of fill colors.
- `theme$color`: A vector of stroke/line colors.

### Example: Using a Single Color

If you have a simple plot with only one group, you can access a single color from the palette:

```r
p <- ggplot(plotData, aes(x=x, y=y)) +
    geom_point(color=theme$color[1]) + # Use the first color from the jamovi palette
    ggtheme
```

### Example: Using a Full Palette

```r
.plot = function(image, ggtheme, theme, ...) {
    plotData <- image$state
    
    p <- ggplot(plotData, aes(x=group, y=value, fill=group)) +
        geom_bar(stat="identity") +
        scale_fill_manual(values=theme$fill) + # Use jamovi palette
        ggtheme
    
    # Return the ggplot2 object
    return(p)
}
```

## Best Practices

### 1. Always add `ggtheme` last
To ensure that `ggtheme` can override any default `ggplot2` settings, it's generally best to add it towards the end of your plot construction.

### 2. Layering Customizations
If you need to add specific theme tweaks (like rotating axis labels), add them *after* `ggtheme`:

```r
p <- p + 
    ggtheme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Authoritative Examples
For real-world examples of how these parameters are used in production, you can explore the source code of the official jamovi modules:
- [jmv](https://github.com/jamovi/jmv): The core jamovi analysis library.
- [jmvplots](https://github.com/jamovi/jmvplots): A dedicated repository for jamovi plotting functions.

**Next Step:** Ensure your plots look great at any size with **[Responsive Image Sizing](/tutorial/tuts0303-responsive-image-sizing)**.
