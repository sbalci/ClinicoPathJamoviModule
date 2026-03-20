---
name: document-function
description: Generate documentation for a jamovi function: developer docs, feature map, testing checklist, and executable vignette
interactive: true
args:
  function_name:
    description: Name of the jamovi function to document (base name without extension)
    required: true
    autocomplete: functions
  include_mermaid:
    description: Include Mermaid diagrams (flowchart + sequence + dependency graphs)
    required: false
    default: true
  include_examples:
    description: Include usage examples and sample option payloads
    required: false
    default: true
  depth:
    description: Verbosity level (brief|standard|deep)
    required: false
    default: standard
  update_existing:
    description: Whether to update existing documentation non-destructively
    required: false
    default: true
  docs:
    description: "Which documents to generate: all|developer|feature|testing|vignette (comma-separated)"
    required: false
    default: all
usage: /document-function <function_name> [--include_mermaid=true] [--include_examples=true] [--depth=standard] [--update_existing=true] [--docs=all]
output_file: vignettes/$ARGUMENTS-documentation.md
---

# Jamovi Function Documentation Suite Generator

**Consult:** `vignettes/jamovi_module_patterns_guide.md` for architecture patterns and `vignettes/jamovi_*_guide.md` files for file-specific details.

You are an expert jamovi module developer, biostatistician, and technical writer. Generate a **comprehensive documentation suite** for the jamovi function **`$ARGUMENTS`** by analyzing these files:

- `jamovi/$ARGUMENTS.a.yaml` — **Analysis definition** (options/arguments)
- `R/$ARGUMENTS.b.R` — **Backend implementation** (how options are used, where results are produced)
- `jamovi/$ARGUMENTS.r.yaml` — **Results definition** (tables, images, HTML outputs, columns)
- `jamovi/$ARGUMENTS.u.yaml` — **User interface** (controls, bindings to options)

If a file is missing, note it explicitly and proceed with partial documentation.

---

## Documentation Suite Overview

This command generates **four** complementary documents (controlled by `--docs`):

| Document | File Pattern | Purpose | Audience |
|----------|-------------|---------|----------|
| **Developer Documentation** | `vignettes/$ARGUMENTS-documentation.md` | UI → Options → Backend → Results mapping with Mermaid diagrams | Developers |
| **Feature Mapping** | `vignettes/${ARGUMENTS}_documentation.md` | Concise feature-to-code table (YAML → UI → Results → R function) | Developers & QA |
| **Testing Checklist** | `vignettes/testing_$ARGUMENTS.md` | Test scenarios with datasets, variables, options, and coverage matrix | QA & Testers |
| **Comprehensive Vignette** | `vignettes/{module}-$ARGUMENTS-comprehensive.Rmd` | Executable R Markdown with all feature demonstrations | Users & Developers |

**Module prefix mapping** (based on `menuGroup` in `.a.yaml`):
- `SurvivalT` → `jsurvival`
- `Descriptives` / `ClinicoPath` → `clinicopath-descriptives`
- `DecisionTools` / `meddecide` → `meddecide`
- `JJStatsPlot` → `jjstatsplot`
- Otherwise → derive from `menuGroup` in lowercase

When `--docs=all` (default), generate all four. Otherwise, generate only the specified documents (comma-separated, e.g., `--docs=testing,vignette`).

---

## Goals

1. Explain **how the interface works**: which `.u.yaml` control **binds** to which `.a.yaml` option, including labels, defaults, constraints, and visibility conditions.
2. Describe **how changing an `.a.yaml` option** affects logic in `.b.R` (where it is accessed via `self$options$...`, how it gates computations, and affects results).
3. Map **how results flow** from `.b.R` into `.r.yaml` outputs (tables/images/HTML), including column schemas and visibility rules.
4. Produce **diagrams** (Mermaid) to visualize UI→Options→Backend→Results and execution sequences.
5. Create a **testing checklist** with concrete test scenarios, datasets, and complete option coverage.
6. Create an **executable vignette** demonstrating every feature with real function calls.
7. Output cohesive **Markdown/Rmd documents** suitable for a developer handbook, QA process, and user guide.

---

## What to Analyze

- In **`.u.yaml`**, list each control: its type (checkbox, combo, text, number, variable selector, etc.), label, and **which `.a.yaml` option** it sets. Include any enable/disable/visibility conditions.
- In **`.a.yaml`**, list options: names, types, defaults, allowed values/constraints, and **downstream effects**.
- In **`.b.R`**, find `self$options$<name>` references. For each:
  - Describe **where it's used**, conditional branches, and functions called.
  - Identify **result population calls** such as `self$results$<out>$setXxx(...)`, `setContent`, `setNotes`, `setRow`, `setVisible`, etc.
- In **`.r.yaml`**, enumerate outputs (tables/images/html): ids, titles, descriptions, visibility conditions, **column schemas** for tables, and any footnotes/notes.
- In **`data/`** and **`data-raw/`**, identify available test datasets for this function.

---

## Update Mode (Existing Doc Awareness)

**Non-Destructive Update Policy:** When updating an existing doc, do **not remove** correct, author-written content. Prefer **repairing** broken pieces and **appending** missing mappings/diagrams. Only replace text that is demonstrably outdated or incorrect.

**How to use a prior doc (if provided in the message or as an attachment):**

1. Treat existing files as the baseline.
2. Preserve narrative sections and headings; re-sync only the affected subsections.
3. Fix broken Mermaid blocks (validate minimal syntax) and keep node ids/titles stable.
4. Append a **Changelog** (see section 1a) summarizing edits.
5. If the prior doc is not provided, proceed with a fresh doc and note that the baseline was unavailable.

---

# DOCUMENT 1: Developer Documentation

**File:** `vignettes/$ARGUMENTS-documentation.md`

Produce a single **Markdown** document with the following sections (**always keep this order and headings**):

### 1. Overview

- **Function**: `$ARGUMENTS`
- **Files**:
  - `jamovi/$ARGUMENTS.u.yaml` — UI
  - `jamovi/$ARGUMENTS.a.yaml` — Options
  - `R/$ARGUMENTS.b.R` — Backend
  - `jamovi/$ARGUMENTS.r.yaml` — Results
- **Summary** (2–4 sentences): What the analysis does and typical inputs/outputs.

### 1a. Changelog (if updating an existing doc)

- Date: <YYYY-MM-DD>
- Summary: 1–3 bullets on what changed
- Changes:
  - Options: added/removed/renamed; defaults updated
  - Backend: logic changes, new result population calls
  - Results schema: table/column id changes, visibility tweaks
  - Diagrams: fixes/updates

### 2. UI Controls → Options Map

For each UI control in `.u.yaml`, show a row with:

- **UI Control**: id, type, label
- **Binds to Option**: `.a.yaml` option name
- **Defaults & Constraints**: default, min/max, enum values
- **Visibility/Enable Rules** (if any)

Provide as a Markdown table.

### 3. Options Reference (.a.yaml)

List each option with:

- **Name** / **Type** / **Default**
- **Description** (from `.a.yaml` if present; otherwise infer)
- **Downstream Effects** (how it's used in `.b.R`)

### 4. Backend Usage (.b.R)

For each `self$options$<option>`:

- **Code Locations**: function names or nearest comment headers
- **Logic Summary**: what branches or calculations depend on it
- **Result Population**: which `self$results$...` objects it influences

Include minimal code excerpts (short snippets) when helpful.

### 5. Results Definition (.r.yaml)

- **Outputs**: id, type (Table/Image/Html), title
- **Visibility**: conditions
- **Schema** (for tables): columns, keys, types, notes
- **Population Entry Points**: where `.b.R` writes into these outputs

### 6. Data Flow Diagram (UI → Options → Backend → Results)

*When updating, reuse node ids/titles where possible to minimize diff churn across versions.*

If `--include_mermaid=true`, add a Mermaid **flowchart** that connects:
`[UI control] --> (a.yaml option) --> {b.R logic block} --> [[r.yaml output]]`

Use meaningful node labels (ids and titles).

```mermaid
flowchart TD
  subgraph UI[.u.yaml]
    U1[control_id: label]
    %% Add more controls...
  end

  subgraph A[.a.yaml Options]
    A1[option_name: type=..., default=...]
    %% Add more options...
  end

  subgraph B[R/$ARGUMENTS.b.R]
    B1{{logic_block_or_fn}}
    %% Add more logic nodes...
  end

  subgraph RY[.r.yaml Outputs]
    R1[[table_id: title]]
    %% Add more outputs...
  end

  U1 --> A1
  A1 --> B1
  B1 --> R1
```

### 7. Execution Sequence (User Action → Results)

If `--include_mermaid=true`, provide **multiple focused small diagrams** covering different aspects:

#### User Input Flow

```mermaid
flowchart LR
  A[User Input] --> B[Option Updates] --> C[Backend Processing]
```

#### Decision Logic

```mermaid
flowchart TD
  A[Check Prerequisites] -->|Valid| B[Process Data]
  A -->|Invalid| C[Show Instructions/Error]
  B --> D[Generate Results]
```

#### Result Processing

```mermaid
flowchart TD
  A[Backend Logic] --> B[Update Results]
  B --> C[Apply Visibility Rules]
  C --> D[Display to User]
```

**Step-by-step execution flow:**

1. **User interacts with UI controls** → UI updates `.a.yaml` options
2. **Backend validation** → Check prerequisites, handle empty/invalid cases
3. **Data processing** → Apply filters/transformations based on options
4. **Analysis execution** → Run main logic using selected methods/packages
5. **Results population** → Update result objects via `self$results$...setContent()`
6. **Display application** → Show visible results per `.r.yaml` rules

```mermaid
graph TD
  Opt1[Option A] --> R1[Result Table]
  Opt2[Option B] --> R1
  Opt2 --> R2[Image Output]
```

Optional: Dependency graph of options → outputs

### 8. Change Impact Guide

For each key option:

- **If changed**: what recalculates, which outputs may differ, performance implications
- **Common pitfalls**: invalid combinations, NA handling, variable requirements
- **Recommended defaults**: why

### 9. Example Usage

If `--include_examples=true`, add:

- **Example dataset requirements** (vars/levels)
- **Example option payload** (YAML or JSON)
- **Expected outputs** (short description)

### 10. Appendix (Schemas & Snippets)

- Tables with full column schemas
- Short, targeted code snippets that show `self$options$...` and `self$results$...` bindings

---

# DOCUMENTS 2-4: Feature Mapping, Testing Checklist, Comprehensive Vignette

For full templates and structure specifications for Documents 2-4, read `.claude/references/document-function-templates.md`.

**Output files:**
- **Document 2** (Feature Mapping) → `vignettes/${ARGUMENTS}_documentation.md`
- **Document 3** (Testing Checklist) → `vignettes/testing_$ARGUMENTS.md`
- **Document 4** (Comprehensive Vignette) → `vignettes/{module}-$ARGUMENTS-comprehensive.Rmd`

Follow existing examples: `agreement_documentation.md`, `testing_agreement.md`, `meddecide-enhancedroc-comprehensive.Rmd`.

---

## Style & Constraints

- Respect the `--depth` argument:
  - `brief`: tight summaries, omit code excerpts, fewer test scenarios
  - `standard`: balanced detail (default)
  - `deep`: include more excerpts, all option branches, exhaustive test matrix
- Prefer concise prose with bullet lists and tables.
- When quoting code, use very short, relevant snippets.
- If schemas are inconsistent, flag clearly and suggest fixes.
- If visibility rules or defaults are implicit, infer and mark as `(inferred)`.

---

## File IO & Safety

- Read all 4 jamovi files (`.a.yaml`, `.b.R`, `.r.yaml`, `.u.yaml`) to extract accurate information.
- Also scan `data/` and `data-raw/` for available test datasets matching the function name.
- If a file is missing or unparsable, print a short Warning and continue.
- Never fabricate options/outputs: clearly label any inference.

---

## Final Deliverable

Generate the requested documents (based on `--docs` flag):

1. **Developer Documentation** → `vignettes/$ARGUMENTS-documentation.md`
2. **Feature Mapping** → `vignettes/${ARGUMENTS}_documentation.md`
3. **Testing Checklist** → `vignettes/testing_$ARGUMENTS.md`
4. **Comprehensive Vignette** → `vignettes/{module}-$ARGUMENTS-comprehensive.Rmd`

If previous versions exist and were provided, include a **Changelog** section in each document summarizing differences.

When `--docs=all` (default), generate all four documents. Report which files were created/updated at the end.

---

## Related Commands

- `/review-function` -- Code review with clinical readiness assessment
- `/generate-test-data` -- Create test datasets referenced by documentation
