---
title: "Integrating the Jamovi Tutorial Builder"
output: md_document
---

## Overview

Embed the tutorial builder interface in jamovi without relying on external HTML files.

## Steps

1. **Render the helper**
   - Call `jamovi_tutorial_builder_static_app_html(layout = "wide")` (or `"compact"`) from your analysis code.
   - Assign the returned string to the relevant `Html` result via `setContent()`.

2. **Ship the JavaScript**
   - Ensure `jamovi/js/jamovi_tutorial_builder.js` (or equivalent) contains the event handlers for the builder UI.
   - Update the module configuration (`jamovi/0000.yaml` or the analysis `.a.yaml`) so jamovi loads the script with the analysis.

3. **Keep markup lean**
   - The helper already omits `<html>/<head>` tags and inlines the necessary CSS, so the jamovi results frame stays responsive.
   - Use `layout = "compact"` when the results column feels cramped.

4. **Verify in jamovi**
   - Reload the module, run the analysis, and interact with the builder: adding steps, drawing hotspots, exporting files.
   - Confirm the UI collapses gracefully on narrow widths and buttons remain accessible.

5. **Final checks**
   - Document the helper usage for future contributors.
   - Before release, run `devtools::document()` and `devtools::check()` so the helper is properly exported and validated.

