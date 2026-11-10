<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/learn/tutorial/tuts0200-analysis-lifecycle.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: "The Analysis Lifecycle"
type: article
description: "Master the two-stage execution model of jamovi: .init() and .run()."
---

In jamovi, an analysis isn't just a single function call. It's a **lifecycle** designed to provide a fluid, "instant" user experience. To achieve this, jamovi separates the creation of the results structure from the actual calculation.

## The Two-Stage Execution

Every jamovi analysis (inheriting from `jmvcore::Analysis`) has two primary entry points in its **R6 class**: `.init()` and `.run()`.

> [!NOTE]
> **What is R6?** Don't let the name intimidate you! R6 is simply the framework jamovi uses to organize your analysis code into a single, clean structure. If you've used functions in R, you're already 90% of the way there.

### 1. The `.init()` Phase
The `.init()` function is responsible for **UI Responsiveness**. Its job is to create the "skeleton" of your results as quickly as possible.

- **When it runs:** Immediately whenever a user changes an option in the UI.
- **What it does:** It defines the number of rows, columns, and titles of tables based on the current options.
- **The Golden Rule:** **Never perform heavy calculations in `.init()`.** It must be near-instantaneous. If `.init()` is slow, the jamovi UI will feel "laggy" to the user.

### 2. The `.run()` Phase
The `.run()` function is where the **Heavy Lifting** happens.

- **When it runs:** After `.init()` has completed, and only when the data is ready.
- **What it does:** It performs the actual statistical calculations (e.g., running a linear model, computing p-values) and "pours" those values into the skeleton created by `.init()`.

---

## Why this separation is vital

Imagine you are running a complex analysis on a dataset with 100,000 rows. 

1.  **Without separation:** You change an option. The UI freezes for 5 seconds while R calculates. Finally, the table appears. The user feels disconnected.
2.  **With jamovi's Lifecycle:** You change an option. **Instantly**, a table skeleton appears with the correct headers and empty rows. A loading spinner might appear. 2 seconds later, the numbers pop into the cells. 

This "instant feedback" is what makes jamovi feel like a modern application rather than a traditional script-based environment.

## Example: A Dynamic Table Skeleton

Suppose your analysis allows users to select multiple dependent variables. You want the table to show one row for each variable.

```r
ttestClass <- R6::R6Class("ttestClass",
    inherit = ttestBase,
    private = list(
        .init = function() {
            # Get the list of dependent variables from options
            deps <- self$options$deps
            
            # Access the table defined in .r.yaml
            table <- self$results$myTable
            
            # Add a row for each variable immediately
            for (dep in deps) {
                table$addRow(rowKey=dep, values=list(
                    var=dep
                ))
            }
        },
        .run = function() {
            # Perform the actual t-test for each variable
            # and populate the existing rows
            # ...
        }
    )
)
```

By defining the rows in `.init()`, the user sees the table grow or shrink the moment they add or remove variables from the list, even before the t-test has finished running.

---

## Summary of Differences

| Feature | `.init()` | `.run()` |
| :--- | :--- | :--- |
| **Primary Goal** | UI Responsiveness | Accuracy & Calculation |
| **Speed** | Must be near-instant | Can take time |
| **Data Access** | Limited (metadata only) | Full access to data |
| **Results** | Creates the "Skeleton" | Fills the "Content" |

**Next Step:** Now that you understand the lifecycle, let's learn how to create **[Dynamic Tables](/tutorial/tuts0201-dynamic-tables)**.
