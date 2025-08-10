# A Comprehensive Guide to Writing `.u.yaml` Files for jamovi Development

This document provides a comprehensive guide to writing `.u.yaml` files for jamovi module development. These files are responsible for defining the layout and organization of the user interface (UI) for your analysis.

## 1. Introduction: The Role of `.u.yaml`

The `.u.yaml` file is a YAML file that specifies the user interface definition for a jamovi analysis. It arranges the UI elements defined in the `.a.yaml` file into a structured and user-friendly layout. While the `.a.yaml` file defines *what* UI elements exist, the `.u.yaml` file defines *where* and *how* they are displayed.

## 2. Core Components of a `.u.yaml` File

A `.u.yaml` file is essentially a tree of UI components. The root of the tree is a list of top-level UI elements. Let's look at the basic structure of `survival.u.yaml`:

```yaml
title: Survival Analysis
name: survival
jus: '3.0'
stage: 0
compilerMode: tame
children:
  - type: VariableSupplier
    # ...
  - type: CollapseBox
    # ...
```

*   `title`: The title of the analysis UI.
*   `name`: The name of the analysis, which must match the name in the `.a.yaml` and `.r.yaml` files.
*   `jus`: The jamovi UI specification version. `'3.0'` is a common value.
*   `stage`: The developmental stage of the analysis (e.g., `0` for development).
*   `compilerMode`: Can be `tame` or `aggressive`. `tame` is generally recommended.
*   `children`: A list of the top-level UI components that make up the analysis UI.

## 3. Common UI Components

The `children` list contains a variety of UI components that you can use to build your interface. Here are some of the most common ones:

### `VariableSupplier`

This is a container for the variable selection lists. It typically appears at the top of the UI.

```yaml
- type: VariableSupplier
  persistentItems: false
  stretchFactor: 1
  children:
    - type: TargetLayoutBox
      label: Time Elapsed
      children:
        - type: VariablesListBox
          name: elapsedtime
          maxItemCount: 1
          isTarget: true
```

*   `TargetLayoutBox`: A box that contains a `VariablesListBox` and a label.
*   `VariablesListBox`: The list of variables that the user can drag and drop into. The `name` here must match the name of a `Variable` or `Variables` option in your `.a.yaml` file.

### `CollapseBox`

A collapsible box that can be used to group related options. This is essential for keeping complex UIs organized.

```yaml
- type: CollapseBox
  label: Advanced Elapsed Time Options
  collapsed: true
  children:
    # ... other UI components
```

*   `label`: The text that appears on the header of the `CollapseBox`.
*   `collapsed`: Whether the box is collapsed by default.

### `LayoutBox`

A flexible container for arranging other UI elements. It can be used to create vertical or horizontal layouts.

```yaml
- type: LayoutBox
  margin: large
  children:
    - type: ComboBox
      name: chart_approach
```

*   `margin`: The amount of space around the `LayoutBox`.

### Referencing `.a.yaml` Options

Most of the UI components in a `.u.yaml` file are simply references to the options you defined in your `.a.yaml` file. You reference an option by its `name`. The `type` of the component in the `.u.yaml` file should correspond to the `type` of the option in the `.a.yaml` file.

Here are some common examples:

*   `ComboBox` for `List` options.
*   `CheckBox` for `Bool` options.
*   `TextBox` for `String`, `Number`, and `Integer` options.
*   `VariablesListBox` for `Variable` and `Variables` options.
*   `LevelSelector` for `Level` options.

## 4. Layout and Arrangement

You can create sophisticated layouts by nesting `LayoutBox` components. While not explicitly shown in these examples, you can use `VerticalStack` and `HorizontalStack` within a `LayoutBox` to arrange elements. However, the default behavior of a `LayoutBox` is to stack its children vertically.

## 5. Conditional Visibility

One of the most powerful features of the `.u.yaml` file is the ability to create a dynamic UI where options appear and disappear based on the user's selections. This is achieved using the `enable` property.

```yaml
- type: ComboBox
  name: stat_method
  enable: add_statistics
```

In this example, the `stat_method` `ComboBox` will only be enabled (i.e., clickable) if the `add_statistics` `CheckBox` is checked. The `enable` property takes a condition that is evaluated in real-time.

You can also use more complex conditions:

```yaml
- type: LevelSelector
  name: outcomeLevel
  enable: (outcome && !multievent)
```

Here, the `outcomeLevel` `LevelSelector` is only enabled if the `outcome` variable is selected *and* the `multievent` checkbox is *not* checked.

## 6. A Complete Example: `advancedbarplot.u.yaml`

The `advancedbarplot.u.yaml` file provides a great example of a well-structured and dynamic UI.

```yaml
title: Advanced Bar Charts - 5 Ways
name: advancedbarplot
jus: '3.0'
stage: 0
compilerMode: tame
children:
  - type: VariableSupplier
    # ...
  - type: Label
    label: Chart Approach & Style
  - type: LayoutBox
    margin: large
    children:
      - type: ComboBox
        name: chart_approach
  - type: CollapseBox
    label: Data & Statistics
    collapsed: false
    children:
      - type: LayoutBox
        margin: normal
        children:
          - type: ComboBox
            name: stat_method
            enable: add_statistics
      # ...
```

This example demonstrates:

*   **Clear Organization:** The UI is organized into logical sections using `CollapseBox` elements.
*   **Dynamic UI:** The `stat_method` `ComboBox` is conditionally enabled based on the `add_statistics` `CheckBox`.
*   **Variety of Components:** The UI uses a variety of components, including `VariableSupplier`, `Label`, `LayoutBox`, `ComboBox`, and `CollapseBox`.

## 7. Best Practices

*   **Organize with `CollapseBox`:** For any analysis with more than a few options, use `CollapseBox` to group related settings. This makes the UI much less overwhelming for the user.
*   **Use `enable` for Dynamic UIs:** Use the `enable` property to create a responsive and intuitive UI that only shows the user the options that are relevant to their current selections.
*   **Keep it Clean:** Use `LayoutBox` and margins to create a visually appealing and easy-to-read layout.
*   **Refer to the Documentation:** The official jamovi documentation on [UI definition](https://dev.jamovi.org/api_ui-definition.html), [basic design](https://dev.jamovi.org/ui-basic-design.html), [advanced design](https://dev.jamovi.org/ui-advanced-design.html), and [advanced customization](https://dev.jamovi.org/ui-advanced-customisation.html) are essential resources.

By following these guidelines and learning from the examples in this project, you can design effective and user-friendly interfaces for your jamovi analyses.
