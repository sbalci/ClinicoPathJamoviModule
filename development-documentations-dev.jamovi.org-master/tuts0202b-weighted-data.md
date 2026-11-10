<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/learn/tutorial/tuts0202b-weighted-data.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Weighted Data
type: article
description: "Understand how jamovi handles weighted datasets in your analysis, why self$data may have more rows than the spreadsheet, and how to choose the right weightsSupport mode."
---

In the [Handling Data tutorial](/tutorial/tuts0202-handling-data) you learned how the dataset reaches your R code through `self$data`. There is one situation that can quietly break the assumptions you build on top of that data frame: a **weighted dataset**. This tutorial explains what weighting means in jamovi, why it changes the shape of `self$data`, and how to declare the behaviour your analysis actually wants.

## What a Weighted Dataset Means

A jamovi user can designate one column as a **weights variable**. Each value in that column says how much the corresponding row counts. A row with a weight of `5` is not one observation — it stands in for five identical observations.

Most often these are whole numbers — a count of how many times that row was observed (so-called **frequency weights**, common when data arrives pre-summarised, such as a table of category combinations with counts). But jamovi also allows **non-integer weights** — for example a `2.7` — which a few statistical methods use directly rather than as a simple count. Keep that distinction in mind: it is the reason the modes below exist, and why some of them care whether a weight is a whole number.

This matters to your analysis code because the row you see in the spreadsheet is no longer a single data point. If you naively count rows or compute a mean without accounting for the weights, your results will be wrong. jamovi gives you a header key — `weightsSupport` — to declare how your analysis wants to deal with this. The mode you choose changes what `self$data` looks like by the time it reaches your `.run()` method.

## The Default `auto` Gotcha

Most analyses run in `auto` mode — it is the default when you do not set `weightsSupport`. In this mode jamovi does the weighting work for you: before your code runs, it **physically replicates rows** in `self$data` according to their integer weights. A row weighted `5` becomes five identical rows. Your analysis then operates on this expanded data frame as if every row were a plain, unweighted observation — which is exactly what you want for most statistics, because the maths "just works."

The surprise is in the row count. Consider a spreadsheet with 4 rows and a weights variable summing to 100:

```r
.run = function() {
    # Spreadsheet shows 4 rows.
    # But under auto mode, self$data has already been expanded:
    nrow(self$data)   # 100, not 4
}
```

This trips up nearly everyone the first time. If you print `nrow(self$data)`, hard-code an expected sample size, or build a loop around the spreadsheet's row count, your logic silently goes wrong on weighted data.

> [!IMPORTANT]
> **`nrow(self$data)` is not the spreadsheet row count under `auto`**
>
> In `auto` mode jamovi replicates rows according to their integer weights, so `nrow(self$data)` equals the *sum of the weights*, not the number of rows the user sees. Non-integer weights are rounded to the nearest integer before replication. Never assume `self$data` has one row per spreadsheet row.

For most analyses that is the whole story: you write **no** weighting code at all, you simply avoid treating `nrow(self$data)` as the user's row count. The remaining modes are for the cases where letting jamovi replicate rows would be wrong.

## Choosing a Mode

`weightsSupport` is a top-level key in the `.a.yaml` header, alongside `name`, `title`, and `jas`:

```yaml
---
name:  TTestIS
title: Independent Samples T-Test
jas: '1.2'
weightsSupport: full
```

There are four values you can set, depending on how your analysis should treat the weights:

- **`auto`** — Let jamovi expand the data for you: it replicates rows and your code treats them as ordinary observations. It needs no extra R code, and it is the default when you omit `weightsSupport`. *Useful for:* descriptive statistics, a frequency table, or a chi-square test run on pre-summarised counts, where a replicated row is genuinely equivalent to a real one.
- **`full`** — Your method accepts a weights argument *as-is*, including fractional weights, so row replication would be incorrect. jamovi leaves the data unexpanded and hands you the weights to pass on yourself (see below). *Useful for:* a weighted linear regression, where the weights are survey or precision weights and are often non-integer.
- **`integerOnly`** — Like `full` (unexpanded data, you read the weights yourself), but only whole-number **frequency** weights are meaningful; jamovi rounds and warns on any fractional weight. *Useful for:* binomial or ordinal logistic regression, where each weight is a count of identical cases.
- **`none`** — Your analysis cannot incorporate weights at all, so jamovi runs it unweighted and shows the user a prominent warning. *Useful for:* an analysis whose main job is to write a column back to the spreadsheet — there is no sensible way to weight that. (It is also the automatic default for such analyses.)

Choosing between `full` and `integerOnly` comes down to one question: is a *fractional* weight meaningful for your method? `full` if yes, `integerOnly` if only whole counts make sense.

## Handling Weights Yourself

If you choose `full` (or `integerOnly`), jamovi does **not** expand the data — instead it hands you the weights as an attribute on `self$data`. Two accessors are available:

- `attr(self$data, 'jmv-weights')` — a numeric vector of weights, one per row, or `NULL` if the dataset is unweighted.
- `attr(self$data, 'jmv-weights-name')` — the name of the weighting variable.

Always guard for the unweighted case, since the attribute is `NULL` when no weights variable is set. Many R model functions accept `weights = NULL` and treat it as unweighted, which keeps the guard simple:

```r
.run = function() {
    data <- self$data
    w    <- attr(data, 'jmv-weights')   # NULL if unweighted

    # lm accepts NULL weights, so no special-casing needed
    model <- lm(y ~ x, data = data, weights = w)

    # ... populate results from the weighted model ...
}
```

## The Automatic Notice

You do **not** need to tell the user their data is weighted — jamovi does it for you. Whenever a weighted dataset is in play, jamovi automatically prepends a [Notice](/api/notice) to your results, with the severity matching the situation:

- an **informational** note when the weights are applied normally (naming the weighting variable);
- a **warning** when non-integer weights had to be rounded (`auto` and `integerOnly`);
- a **strong warning** when the analysis ignores weights and ran unweighted (`none`).

You never write this notice yourself. In the rare case you need to replace or suppress it, define your own result element named `.weights` and jamovi will leave it alone.

**Next Step:** Now that your analysis handles weighted data correctly, let's learn how to write values back to the spreadsheet with [Output Variables](/tutorial/tuts0202a-output-variables).
