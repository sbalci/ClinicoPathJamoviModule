# A Comprehensive Guide to Creating and Populating Plots in jamovi

This document provides a comprehensive guide to creating and populating plots in jamovi module development. Plots are a crucial component of data analysis, and this guide will walk you through the process of adding them to your jamovi modules.

## 1. Introduction: The Process of Creating Plots

Similar to tables, creating plots in jamovi is a process that involves both the `.r.yaml` and `.b.R` files:

1.  **`.r.yaml` file:** This is where you **define the plot object**. You specify the name of the plot, its title, dimensions, and, most importantly, the R function that will be responsible for rendering it.
2.  **`.b.R` file:** This is where you **write the R code to generate the plot**. This typically involves using a plotting library like `ggplot2` to create the visualization and then passing the necessary data to the plot rendering function.

## 2. Step 1: Defining the Plot in `.r.yaml`

The first step is to define the plot in your analysis's `.r.yaml` file. A plot is defined as an item of type `Image`. Let's look at an example from the `survival.r.yaml` file:

```yaml
- name: plot
  title: '`Survival Plot - ${explanatory}`'
  type: Image
  width:  600
  height: 450
  renderFun: .plot
  visible: (sc)
  requiresData: true
```

Here's a breakdown of the key properties:

*   `name`: A unique name for the plot (e.g., `plot`).
*   `title`: The title of the plot. You can use `${...}` to make the title dynamic.
*   `type`: Must be `Image`.
*   `width` and `height`: The dimensions of the plot in pixels.
*   `renderFun`: The name of the R function in your `.b.R` file that will generate the plot. By convention, these functions are often prefixed with a dot (e.g., `.plot`).
*   `visible`: A condition that determines whether the plot is visible. In this example, the plot is only visible if the `sc` (survival curve) option is checked.
*   `requiresData`: Whether the plot requires data to be rendered. This is almost always `true` for plots.

## 3. Step 2: Passing Data to the Plot Function

You don't call the `renderFun` directly. Instead, jamovi calls it for you when the plot needs to be rendered. This means you need a way to pass data from your main analysis function (`.run`) to your plot function. This is done using the `image$state` object.

In your `.run` function, after you have prepared the data for plotting, you set the state of the plot object:

```R
# In the .run function of survival.b.R

# ... (code to prepare plotData) ...

image <- self$results$plot
image$setState(plotData)
```

*   `self$results$plot`: Gets a reference to the plot object named `plot`.
*   `image$setState(plotData)`: Sets the state of the plot object. `plotData` can be any R object, but it's typically a list or a data frame containing the data and any other information the plot function will need.

## 4. Step 3: Writing the Plot Function in `.b.R`

The final step is to write the R function that will actually generate the plot. This function must have a specific signature:

```R
.plot = function(image, ggtheme, theme, ...) {
    # ... (plotting code) ...
}
```

*   `image`: The plot object itself. You'll use this to retrieve the state you set earlier.
*   `ggtheme` and `theme`: These are provided by jamovi and contain the current jamovi theme settings. You should apply these to your plot to ensure it matches the overall look and feel of jamovi.

Here's a simplified example of the `.plot` function from `survival.b.R`:

```R
.plot = function(image, ggtheme, theme, ...) {
    # Get the data from the image state
    results <- image$state

    if (is.null(results)) {
        return()
    }

    # ... (extract variables from results) ...

    plotData <- results$cleanData

    # ... (code to create the ggplot object `plot`) ...

    plot <- plotData %>%
        finalfit::surv_plot(
            .data = .,
            dependent = myformula,
            explanatory = myfactor,
            # ... (other plotting options) ...
        )

    # The final plot object is printed to the output device
    print(plot)
    TRUE
}
```

### Key Points:

*   **Retrieve the State:** The first thing you do in your plot function is retrieve the data from the `image$state`.
*   **Create the Plot:** Use a plotting library like `ggplot2` to create your plot. The `advancedbarplot.b.R` file is an excellent example of using `ggplot2` to create a variety of plots.
*   **Apply the Theme:** Although not explicitly shown in the simplified example above, you should add the `ggtheme` to your ggplot object (`plot <- plot + ggtheme`) to ensure consistent styling.
*   **Print the Plot:** The last step is to `print()` the plot object. This is what actually renders the plot in the jamovi results.

## 5. A Complete Example: `advancedbarplot`

The `advancedbarplot` analysis is a great example of a plot-heavy analysis.

**`.r.yaml`:**

```yaml
- name: main_plot
  title: "Advanced Bar Chart"
  type: Image
  width: 800
  height: 600
  renderFun: .plot_main
  visible: true
```

**`.b.R` (`.run` function):**

```R
# ... (process data into private$.processed_data) ...

# Set plot state for rendering
self$results$main_plot$setState(private$.processed_data)
```

**`.b.R` (`.plot_main` function):**

```R
.plot_main = function(image, ggtheme, theme, ...) {
    data <- image$state
    if (is.null(data)) return()

    # ... (code to create the plot based on the selected approach) ...

    print(plot)
    TRUE
}
```

This example shows the clear separation of concerns: the `.r.yaml` file defines the plot, the `.run` function prepares the data and sets the state, and the `.plot_main` function uses that state to render the plot.

## 6. Best Practices

*   **Use `image$state`:** This is the standard and recommended way to pass data to your plot functions.
*   **Apply jamovi Themes:** Always apply the `ggtheme` to your `ggplot2` plots to ensure a consistent look and feel.
*   **Handle Errors:** Use `tryCatch()` in your plot functions to gracefully handle any errors that might occur during plot generation.
*   **Keep Plotting Logic Separate:** Keep your plotting logic in dedicated functions (like `.plot`) rather than cluttering up your main `.run` function.
*   **Refer to the Documentation:** The official jamovi tutorial on [adding plots](https://dev.jamovi.org/tuts0106-adding-plots.html) is a great starting point.

By following these guidelines, you can create rich, informative, and visually appealing plots that will greatly enhance your jamovi modules.
