<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/learn/tutorial/tuts0301-image-state-performance.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: "Image State Performance"
type: article
description: "Optimize your jamovi module by avoiding the \"State\" anti-pattern and using data-driven rendering correctly."
---

In jamovi, the **State/Render** model is designed for speed. However, a common mistake can lead to sluggish performance and bloated file sizes.

## The Mental Model: Data-Driven vs. State-Driven

Choosing between data-driven and state-driven rendering is the most important architectural decision you'll make for your plots.

### State-Driven (The Default)
You calculate everything in `.run()`, store it in `image$state`, and `.plot()` simply paints it.
- **Pros:** Extremely fast rendering; no data access needed during plot resize.
- **Cons:** Can bloat the `.omv` file if the state is too large.

### Data-Driven (The Advanced Path)
Your `.plot()` function accesses the raw dataset directly.
- **Pros:** No state bloat; essential for plots requiring thousands of raw points.
- **Cons:** Slightly slower rendering as data may be re-read; requires `requiresData: true`.

---

## The "State" Common Mistake

The most common mistake occurs when a developer tries to be "data-driven" but uses the "state" mechanism to achieve it by storing the entire raw dataset in the image state:

```r
# ❌ COMMON MISTAKE
.run = function() {
    image <- self$results$plot
    image$setState(self$data) # BAD: Storing the whole dataset in the state!
}
```

### Why this is bad

1.  **Memory Bloat:** If your dataset has 100,000 rows and you have 5 plots, you are storing 500,000 rows of data in memory.
2.  **Slow Saves:** jamovi saves the state of every results element into the `.omv` file. Storing raw data in the state makes files massive and saving/loading painfully slow.
3.  **Redundancy:** The raw data is already available to the analysis. Storing it again in the state is unnecessary.

---

## Solution A: Summary Statistics (State-Driven)

For most plots, the `.run()` function should do the "heavy lifting" of statistical calculation. The `.plot()` function should only handle the "painting" of pixels.

**Always store only the minimum data required to draw the plot.**

Instead of the whole dataset, calculate the means and confidence intervals in `.run()`:

```r
# ✅ BEST PRACTICE (STATE-DRIVEN)
.run = function() {
    dep <- self$options$dep
    group <- self$options$group
    
    # Calculate just what you need
    plotData <- aggregate(
        as.formula(paste(dep, "~", group)), 
        data = self$data, 
        FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)))
    )
    
    # Store only the summary table (e.g., 5-10 rows)
    image <- self$results$plot
    image$setState(plotData)
}
```

---

## Solution B: Use `requiresData` (Data-Driven)

If you *truly* need the raw dataset to render your plot (e.g., a scatter plot or density plot), do **not** use `setState`. Instead, use the `requiresData` property in your `.r.yaml` file.

### 1. Update your Results YAML (.r.yaml)

Set `requiresData: true` to grant your `.plot` function access to the dataset.

```yaml
    - name: scatterPlot
      type: Image
      renderFun: .plot
      requiresData: true  # This is the key!
```

### 2. Implementation in R

Now your `.plot` function can access `self$data` directly without it being stored in the analysis state.

```r
# ✅ BEST PRACTICE (DATA-DRIVEN)
.plot = function(image, ggtheme, theme, ...) {
    # Access self$data directly
    plotData <- self$data 
    
    p <- ggplot(plotData, aes(x=x, y=y)) + 
         geom_point() + 
         ggtheme
    
    # Return the ggplot2 object directly
    return(p)
}
```

---

## Performance Checklist

- [ ] **Is my state a data frame?** Data frames are generally more efficient for `ggplot2` than complex lists.
- [ ] **Did I filter NAs?** Don't pass missing values to the state or use them in plots if they aren't needed.
- [ ] **Is the row count minimal?** If your plot shows 10 groups, your state should ideally have 10 rows.
- [ ] **Are there large objects?** Avoid storing model objects (like `lm` or `lmer` outputs) in the state unless absolutely necessary (and check their size with `length(serialize(object), connection=NULL)`). Extract the coefficients or predictions instead.

**Next Step:** Learn how to make your plots match the jamovi look and feel in **[Plot Themes](/tutorial/tuts0302-plot-themes)**.
