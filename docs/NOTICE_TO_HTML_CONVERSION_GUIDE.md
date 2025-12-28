# Notice to HTML Conversion Guide

## Problem

Using `insert(999, notice)` with `jmvcore::Notice` objects causes serialization errors in jamovi because Notice objects contain function references that cannot be serialized by jamovi's protobuf system.

**Error symptoms:**
- "Serialization error" messages
- Notices not appearing in results
- Analysis failures when moving notices to end (position 999)

## Solution

Convert Notice objects to HTML output items that are rendered as formatted HTML instead of being inserted as Notice objects.

## Conversion Steps

### Step 1: Add HTML Output Item to .r.yaml File

Add an HTML item to the results definition in `jamovi/{function}.r.yaml`:

```yaml
# Notice outputs converted to HTML to avoid serialization errors
- name: notices
  title: Important Information
  type: Html
  clearWith:
     - {relevant_option_1}
     - {relevant_option_2}
     # Add all options that should trigger notice recalculation
```

### Step 2: Add Helper Methods to .b.R File

Add these helper methods after existing utility functions in `R/{function}.b.R`:

```r
# Initialize notice collection list
.noticeList = list(),

# Add a notice to the collection
.addNotice = function(type, title, content) {
  private$.noticeList[[length(private$.noticeList) + 1]] <- list(
    type = type,
    title = title,
    content = content
  )
},

# Render collected notices as HTML
.renderNotices = function() {
  if (length(private$.noticeList) == 0) {
    return()
  }

  # Map notice types to colors and icons
  typeStyles <- list(
    ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5", icon = "⛔"),
    STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74", icon = "⚠️"),
    WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047", icon = "⚡"),
    INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd", icon = "ℹ️")
  )

  html <- "<div style='margin: 10px 0;'>"

  for (notice in private$.noticeList) {
    style <- typeStyles[[notice$type]] %||% typeStyles$INFO

    html <- paste0(html,
      "<div style='background-color: ", style$bgcolor, "; ",
      "border-left: 4px solid ", style$border, "; ",
      "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
      "<strong style='color: ", style$color, ";'>",
      style$icon, " ", private$.safeHtmlOutput(notice$title), "</strong><br>",
      "<span style='color: #374151;'>", private$.safeHtmlOutput(notice$content), "</span>",
      "</div>"
    )
  }

  html <- paste0(html, "</div>")

  self$results$notices$setContent(html)
},
```

**Note:** If `.safeHtmlOutput()` doesn't exist, add it:

```r
# HTML sanitization for security
.safeHtmlOutput = function(text) {
  if (is.null(text) || length(text) == 0) return("")
  text <- as.character(text)
  # Sanitize potentially dangerous characters
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub("\"", "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&#x27;", text, fixed = TRUE)
  text <- gsub("/", "&#x2F;", text, fixed = TRUE)
  return(text)
},
```

### Step 3: Replace Notice Creation and Insertion

**Old pattern:**
```r
regulatory_blocker <- jmvcore::Notice$new(
  options = self$options,
  name = "regulatoryUseError",
  type = jmvcore::NoticeType$ERROR
)
regulatory_blocker$setContent("REGULATORY USE PROHIBITED: This function is NOT validated...")
self$results$insert(999, regulatory_blocker)
```

**New pattern:**
```r
private$.addNotice(
  type = "ERROR",
  title = "REGULATORY USE PROHIBITED",
  content = "This function is NOT validated for regulatory submissions..."
)
```

**Notice type mapping:**
- `jmvcore::NoticeType$ERROR` → `"ERROR"`
- `jmvcore::NoticeType$STRONG_WARNING` → `"STRONG_WARNING"`
- `jmvcore::NoticeType$WARNING` → `"WARNING"`
- `jmvcore::NoticeType$INFO` → `"INFO"`

### Step 4: Add Render Call to .run() Method

At the end of the `.run()` method, add:

```r
.run = function() {
  # ... existing code ...

  # Last step: Render all collected notices as HTML
  private$.renderNotices()
},
```

## Example: Complete Conversion

See `R/waterfall.b.R` for a complete working example of this pattern.

**Key files:**
- `jamovi/waterfall.r.yaml` (lines 343-352) - HTML output definition
- `R/waterfall.b.R` (lines 95-140) - Helper methods
- `R/waterfall.b.R` (line 2230) - Render call in .run()
- `R/waterfall.b.R` (throughout) - Multiple .addNotice() calls

## Files Requiring Conversion

As of 2025-12-28, there are **322 active insert(999,) calls** across **40 .b.R files** that need conversion:

To find files needing conversion:
```bash
for file in R/*.b.R; do
  count=$(grep -c "insert(999," "$file" 2>/dev/null)
  if [ "$count" -gt 0 ]; then
    echo "$file: $count calls"
  fi
done | sort -t: -k2 -rn
```

## Priority Files for Conversion

Convert files when:
1. User encounters serialization errors
2. Function is frequently used
3. Function has many notices

High-priority candidates:
- `R/decision.b.R` (30 calls)
- `R/jjcoefstats.b.R` (25 calls)
- `R/decisioncurve.b.R` (20 calls)
- `R/decisioncombine.b.R` (19 calls)
- `R/decisioncompare.b.R` (19 calls)

## Testing After Conversion

1. Run `jmvtools::prepare()` to regenerate .h.R files
2. Install module in jamovi
3. Test the analysis with sample data
4. Verify notices appear correctly formatted
5. Verify no serialization errors

## Benefits

✅ **No serialization errors** - HTML strings serialize safely
✅ **Better formatting** - Consistent visual styling with colors and icons
✅ **Better UX** - All notices grouped in one "Important Information" section
✅ **Maintainable** - Centralized notice rendering logic
✅ **Safe** - HTML sanitization prevents XSS vulnerabilities

## Notes

- This conversion is **backward compatible** - it doesn't change the jamovi interface
- Notices still display the same information, just formatted as HTML
- The notice list is automatically cleared between runs (in .noticeList initialization)
- HTML sanitization (`.safeHtmlOutput()`) prevents security issues from user-provided data in notice messages
