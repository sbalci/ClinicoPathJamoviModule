---
layout: ../layouts/BaseLayout.astro
title: Text
description: "Reference for the Text results element, used for displaying wrapped, Markdown-formatted text output in jamovi analyses."
type: article
---

<!-- Vendored from https://github.com/jamovi/dev.jamovi.org/blob/17737bbc/src/content/docs/reference/api/text.md
     (fetched 2026-09-19; published at https://dev.jamovi.org). Newer than the rest of this snapshot. -->

The `Text` element is used for displaying plain, word-wrapped text, laid out as paragraphs. Unlike [Preformatted](/api/preformatted), which preserves whitespace and renders in a monospaced font, `Text` reflows its content like ordinary prose — making it suited to narrative summaries, interpretive statements, or other explanatory output that isn't tabular.

Content is authored as Markdown, and split into paragraphs on a blank line (i.e. two consecutive newline characters, `\n\n`), with each paragraph word-wrapped to fit the results panel. The following Markdown formatting is supported:

| Formatting | Markdown |
|---|---|
| Bold | `**bold**` |
| Italic | `*italic*` |
| Strikethrough | `~~strikethrough~~` |
| Links | `[text](url)` |
| Lists | `- item` / `1. item` |
| Subscript / superscript | `<sub>`, `<sup>` tags (no Markdown syntax exists for these) |

Other Markdown constructs — headings, blockquotes, code blocks, tables, images — aren't supported. They're stripped down to plain paragraphs rather than rendered with their own semantics, so don't rely on them for structure.

Because `*` and `_` are Markdown syntax, take care with significance markers and other stats notation that use asterisks (e.g. `5.2*`, `p < .001**`) — an odd number of unescaped asterisks can trigger italics, and an even number can trigger bold, swallowing everything up to the next one. Escape each asterisk with a backslash (`\*`) to render it literally:

```r
text$setContent("Effect size: 5.2\\*\\* (p < .001)")
```

> [!WARNING]
> HTML character entities (e.g. `&mdash;`, `&nbsp;`, `&#8212;`) are deliberately **not** decoded, even though the content is parsed as Markdown — they render literally (e.g. as `&mdash;`) rather than as the character they represent. Use the actual character (e.g. —) in your content instead. If you're writing that character directly into an `.R` file and intend to submit your module to CRAN, use a Unicode escape (e.g. `"\u2014"`) rather than a literal non-ASCII character in your source, since CRAN checks flag those.

Available in jamovi 28.3 and newer. Declare this in your module's `0000.yaml` using `minApp`, so jamovi prevents installation on older versions that don't support it:

```yaml
minApp: 28.3.0
```

## YAML Properties

| Property | Type | Description |
|----------|------|-------------|
| `name` | string | The unique name of the element. |
| `type` | string | Must be `Text`. |
| `title` | string | (optional) The title displayed in the results panel. |
| `content` | string | (optional) The initial text content. |
| `visible` | boolean or string | (optional) Whether the element is visible by default. Defaults to `true`. |
| `clearWith` | array | (optional) A list of option names that clear this element when changed. Defaults to `*` (all options). |
| `refs` | array or string | (optional) References to cite alongside this element. |

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

### setContent(value)

sets the text content of the element. `value` should be a string; if it isn't a character vector, it's coerced with `capture.output()` first.

## Examples

### 1. Define the Text element in YAML

A `Text` element is defined in the `.r.yaml` file:

```yaml
- name: summary
  title: Summary
  type: Text
```

### 2. Set Content in R

In the `.run()` function, set the element's content as a plain string:

```r
.run = function() {
    # Access a Text element named 'summary'
    text <- self$results$summary

    n <- nrow(self$data)

    text$setContent(paste0(
        "The sample consisted of ", n, " observations. ",
        "No significant outliers were detected."
    ))
}
```

Blank lines separate paragraphs:

```r
text$setContent(
    "The first paragraph.\n\nThe second paragraph."
)
```

### 3. Markdown Formatting

```r
text$setContent(paste0(
    "The effect was **statistically significant** ",
    "(see [the appendix](https://example.com/appendix) for details):\n\n",
    "- Factor A: *p* < .001\n",
    "- Factor B: *p* = .045\n"
))
```
