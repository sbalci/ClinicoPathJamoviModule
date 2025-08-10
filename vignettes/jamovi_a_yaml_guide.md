# A Comprehensive Guide to Writing `.a.yaml` Files for jamovi Development

This is a comprehensive guide to writing `.a.yaml` files for developing jamovi modules. These files are the backbone of a jamovi analysis, defining the user interface and the data requirements for your R functions.

## 1. Introduction: The Role of `.a.yaml`

The `.a.yaml` file is a YAML file that specifies the analysis definition for a jamovi module. It defines the user interface (UI) that your analysis will have inside jamovi, including the options, input fields, and buttons that the user will interact with. It also specifies the data requirements for the analysis, such as the types of variables that can be used.

Each analysis in a jamovi module has a corresponding `.a.yaml` file. This file is paired with an `.r.yaml` file (which defines the results) and a `.u.yaml` file (which defines the user interface layout).

## 2. Core Components of an `.a.yaml` File

Let's start by looking at the top-level keys in a typical `.a.yaml` file. We'll use `gtsummary.a.yaml` as an example.

```yaml
---
name:  gtsummary
title: Publication-Ready Tables with gtsummary
menuGroup: ExplorationD
menuSubgroup: Enhanced Tables
menuSubtitle: Summary Tables
version: '0.0.3'
jas: '1.2'

options:
    - name: data
      type: Data
# ... more options
```

*   `name`: This is the unique identifier for the analysis. It should be a single word with no spaces. This name is used to reference the analysis in other parts of the module.
*   `title`: This is the human-readable title of the analysis that will appear in the jamovi menus.
*   `menuGroup`: This specifies the main menu group under which the analysis will appear in jamovi.
*   `menuSubgroup`: This specifies a subgroup within the main menu group.
*   `menuSubtitle`: An optional subtitle for the analysis in the menu.
*   `version`: The version of the analysis. It's good practice to increment this when you make changes.
*   `jas`: The jamovi analysis specification version. `'1.2'` is a common value.
*   `options`: This is the most important part of the `.a.yaml` file. It's a list of all the UI elements (options) that will be available for the analysis. We'll dive into this in detail below.

## 3. The `options` Section: Defining the User Interface

The `options` section is a list of dictionaries, where each dictionary defines a single UI element. Let's break down the common keys for an option:

*   `name`: A unique name for the option within the analysis. This name will be used to access the option's value in your R code.
*   `title`: The label that will be displayed next to the UI element in jamovi.
*   `type`: The type of UI element to create. This is a crucial key, and we'll explore the different types below.
*   `description`: A description of the option. This can have two sub-keys:
    *   `main`: A general description that appears as a tooltip in jamovi.
    *   `R`: A description of how the option is used in the corresponding R code.
*   `default`: The default value for the option.
*   `suggested`: A list of suggested variable types for `Variable` and `Variables` options. Common values are `[continuous, ordinal, nominal]`.
*   `permitted`: A list of permitted variable types. Common values are `[numeric, factor]`.

### Common Option Types

Here are some of the most common option types you'll use in your `.a.yaml` files:

#### `Data`

This option type is used to specify the dataset for the analysis. There is usually only one `Data` option per analysis.

```yaml
- name: data
  type: Data
```

#### `Variable`

This option type allows the user to select a single variable from the dataset.

```yaml
- name: byvar
  title: Grouping Variable (By)
  type: Variable
  suggested: [ ordinal, nominal ]
  permitted: [ factor ]
  default: NULL
```

#### `Variables`

This option type allows the user to select multiple variables from the dataset.

```yaml
- name: vars
  title: Variables for Table
  type: Variables
  suggested: [ continuous, ordinal, nominal ]
  permitted: [ numeric, factor ]
```

#### `List`

This option type creates a dropdown list of choices.

```yaml
- name: tableType
  title: Table Type
  type: List
  options:
    - title: Summary Table (tbl_summary)
      name: summary
    - title: Cross Table (tbl_cross)
      name: cross
    - title: Regression Table (tbl_regression)
      name: regression
    - title: Survival Table (tbl_survfit)
      name: survival
  default: summary
```

Each item in the `options` list for a `List` type has a `title` (what the user sees) and a `name` (the value that gets passed to your R code).

#### `NMXList`

This is a "non-mutual exclusive list", which is essentially a list of checkboxes. The user can select multiple options.

```yaml
- name: statistics
  title: Statistics to Include
  type: NMXList
  options:
    - name: mean_sd
      title: Mean (SD)
    - name: median_iqr
      title: Median (IQR)
    - name: range
      title: Range (Min, Max)
    - name: n_percent
      title: N (%)
    - name: missing
      title: Missing values
  default: [ mean_sd, median_iqr, n_percent ]
```

#### `Bool`

A simple checkbox that returns `true` or `false`.

```yaml
- name: addPValue
  title: Add P-values
  type: Bool
  default: false
```

#### `String`

A text input field for short strings.

```yaml
- name: tableTitle
  title: Table Title
  type: String
  default: ""
```

#### `Number`

A numeric input field. You can specify `min` and `max` values.

```yaml
- name: pValueThreshold
  title: P-value Threshold for Bolding
  type: Number
  default: 0.05
  min: 0.001
  max: 0.1
```

#### `Integer`

Similar to `Number`, but for integer values.

```yaml
- name: digitsOverall
  title: Digits for Overall Statistics
  type: Integer
  default: 1
  min: 0
  max: 5
```

#### `Level`

This option type allows the user to select a specific level from a `Variable`. The `variable` key is used to link this option to the `Variable` option it depends on.

```yaml
- name: outcomeLevel
  title: Event Level
  type: Level
  variable: (outcome)
```

#### `Output`

This option type is used to create a new column in the dataset.

```yaml
- name: calculatedtime
  title: Add Calculated Time to Data
  type: Output
```

## 4. A Complete Example: `advancedbarplot.a.yaml`

Let's look at the `advancedbarplot.a.yaml` file to see how these components come together in a real-world example. This file defines a complex analysis with many options for creating different types of bar charts.

```yaml
---
name: advancedbarplot
title: "Advanced Bar Charts - 5 Ways"
menuGroup: JJStatsPlotD
menuSubgroup: ClinicoPath Advanced Plots
menuSubtitle: "Professional Bar Charts with Multiple Approaches"
version: '0.0.3'
jas: '1.2'

description:
    main: |
        Advanced bar chart visualization module implementing 5 different approaches
        for creating professional bar charts. Choose from ggplot2 basics, polished
        presentations, statistical annotations, interactive plots, and publication-ready
        designs. Each approach optimized for different use cases in clinical research.
    R:
        dontrun: true
        usage: |
            # Example usage - 5 different bar chart approaches:
            # 1. Basic ggplot2 approach
            # 2. Polished presentation style
            # 3. Statistical annotation style
            # 4. Interactive plotly style
            # 5. Publication-ready style

options:
    - name: data
      type: Data
      description:
        R: The data as a data frame.
        jamovi: The dataset for advanced bar chart visualization.

    - name: x_var
      title: "X Variable (Categories)"
      type: Variable
      suggested: [nominal, ordinal]
      permitted: [factor]
      description:
        R: Categorical variable for x-axis categories.
        jamovi: Select categorical variable for the x-axis.

    - name: y_var
      title: "Y Variable (Values)"
      type: Variable
      suggested: [continuous]
      permitted: [numeric]
      description:
        R: Numeric variable for bar heights.
        jamovi: Select numeric variable for bar heights.

    - name: chart_approach
      title: "Bar Chart Approach"
      type: List
      options:
        - title: "1. Basic ggplot2"
          name: basic
        - title: "2. Polished Presentation"
          name: polished
        - title: "3. Statistical Annotations"
          name: statistical
        - title: "4. Interactive Plotly"
          name: interactive
        - title: "5. Publication Ready"
          name: publication
        - title: "6. BBC News Style"
          name: bbc_style
        - title: "7. GraphPad Prism Style"
          name: prism_style
      default: polished
      description:
        R: Choose the bar chart approach and styling.
        jamovi: Select one of 7 professional bar chart approaches including BBC News and GraphPad Prism styles.

# ... many more options
```

This example demonstrates the use of various option types, including `Data`, `Variable`, and `List`. It also shows how to provide detailed descriptions for both the jamovi UI and the R code.

## 5. Advanced Concepts

### Conditional Visibility

You can make options appear or disappear based on the values of other options. This is not explicitly defined in the `.a.yaml` file but is handled in the `.u.yaml` file, which controls the layout. However, it's important to design your `.a.yaml` file with this in mind. For example, the `stat_method` option in `advancedbarplot.a.yaml` is likely only visible when `add_statistics` is checked.

### Dynamic Content

The content of some options, like the levels in a `Level` option, is dynamic and depends on the selected variable. This is handled automatically by jamovi based on the `variable` key.

## 6. Best Practices

*   **Be Descriptive:** Use the `title` and `description` fields to make your analysis easy to understand for users.
*   **Use `suggested` and `permitted`:** These keys help guide the user to select the correct variable types and prevent errors.
*   **Provide Sensible Defaults:** Set default values for your options so that the analysis can be run with minimal configuration.
*   **Keep it Organized:** Structure your `.a.yaml` file logically. Group related options together.
*   **Follow Conventions:** Look at the `.a.yaml` files from other jamovi modules (like the ones in this project) to learn the common conventions.

By following these guidelines and using the examples from this project, you can create powerful and user-friendly analyses for jamovi. For more in-depth information, you can always refer to the official [jamovi developer documentation](https://dev.jamovi.org/api_analysis-definition.html).
