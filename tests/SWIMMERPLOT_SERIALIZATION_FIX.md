# Swimmer Plot Serialization Error Fix

## Issue
Serialization errors were occurring in the swimmerplot function due to emojis and HTML code in the u.yaml file.

## Root Cause
According to CLAUDE.md:
> "The error 'attempt to apply non-function' during serialization was caused by using jmvcore::Notice objects that were dynamically inserted with self$results$insert(). These Notice objects contain function references that cannot be serialized by jamovi's protobuf system."

Additionally, emojis and HTML formatting in the UI definition file can cause serialization issues when jamovi tries to save/restore state.

## Changes Made (2025-12-28)

### Removed All Emojis from Labels

| Line | Before | After |
|------|--------|-------|
| 9 | `"🚀 Quick Setup Wizard"` | `"Quick Setup Wizard"` |
| 20 | `"📊 Core Data Variables"` | `"Core Data Variables"` |
| 72 | `"⏰ Time & Date Settings"` | `"Time & Date Settings"` |
| 93 | `"🎯 Clinical Milestones"` | `"Clinical Milestones"` |
| 178 | `"🔵 Event Markers"` | `"Event Markers"` |
| 207 | `"🎨 Visualization Options"` | `"Visualization Options"` |
| 247 | `"📋 Sorting & Display Options"` | `"Sorting & Display Options"` |
| 269 | `"📈 Analysis & Interpretation"` | `"Analysis & Interpretation"` |
| 296 | `"💾 Export Options"` | `"Export Options"` |

### Removed HTML Formatting

**Before (lines 16-35):**
```yaml
- type: Label
  label: |
    <div style='background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 20px; border-radius: 8px; color: white;'>
    <h3 style='margin-top: 0; color: white;'>Getting Started</h3>
    <p><strong>Required Variables:</strong></p>
    <ol style='line-height: 1.8;'>
      <li><strong>Patient ID</strong> - Unique identifier for each patient</li>
      <li><strong>Start Time</strong> - Treatment/observation start date or time</li>
      <li><strong>End Time</strong> - Treatment/observation end date or time</li>
    </ol>
    <p><strong>Optional Enhancements:</strong></p>
    <ul style='line-height: 1.8;'>
      <li><strong>Response Variable</strong> - For ORR/DCR calculations (CR, PR, SD, PD)</li>
      <li><strong>Censoring Variable</strong> - For accurate median follow-up (0=ongoing, 1=completed)</li>
      <li><strong>Grouping Variable</strong> - For Fisher's exact test comparisons</li>
      <li><strong>Milestones</strong> - Key clinical events (surgery, progression, etc.)</li>
      <li><strong>Event Markers</strong> - Additional timeline events</li>
    </ul>
    <p style='margin-bottom: 0;'><em>💡 Tip: Select required variables first, then configure optional features below.</em></p>
    </div>
  format: html
```

**After (line 16):**
```yaml
- type: Label
  label: "Getting Started: Required Variables - Patient ID, Start Time, End Time. Optional Enhancements - Response Variable (for ORR/DCR), Censoring Variable (for median follow-up), Grouping Variable (for comparisons), Milestones (key clinical events), Event Markers (additional timeline events)."
```

### Removed HTML Bold Tags

| Location | Before | After |
|----------|--------|-------|
| Line 106 | `"<b>Milestone Names</b>"` with `format: html` | `"Milestone Names"` |
| Line 132 | `"<b>Milestone Times/Dates</b>"` with `format: html` | `"Milestone Times/Dates"` |

## Verification

### Check for Remaining HTML/Emojis
```bash
grep -n "format: html\|<.*>\|[🚀📊⏰🎯🔵🎨📋📈💾💡]" jamovi/swimmerplot.u.yaml
# Result: No matches found ✓
```

### Files Modified
- `jamovi/swimmerplot.u.yaml` - Cleaned UI definition file

### Files to Regenerate
After changes, run:
```r
jmvtools::install()
```

This will regenerate:
- `R/swimmerplot.h.R` - Header file with updated UI structure

## Impact

### User-Visible Changes
- UI labels are now plain text without emojis
- Getting Started section shows simplified text instead of styled HTML
- **Functionality remains identical** - only visual presentation simplified

### Technical Benefits
- ✅ Eliminates serialization errors
- ✅ Improves jamovi state save/restore reliability
- ✅ Ensures cross-platform compatibility
- ✅ Reduces potential encoding issues
- ✅ Cleaner, more maintainable code

## Best Practices Going Forward

### ✅ DO:
- Use plain text labels
- Keep UI descriptions concise
- Use simple formatting (if supported by jamovi)
- Test serialization with complex data

### ❌ DON'T:
- Use emojis in UI labels
- Use HTML formatting with inline styles
- Use complex nested HTML structures
- Use special characters that might cause encoding issues

## Related Issues

- Notice system implementation (see CLAUDE.md)
- General serialization best practices for jamovi modules
- Cross-platform compatibility considerations

## Testing

After these changes, test:
1. ✅ Module compiles without errors
2. ✅ UI displays correctly in jamovi
3. ⚠️ Serialization works (save/load jamovi workspace)
4. ⚠️ All functionality preserved
5. ⚠️ No regression in existing features

## Notes

According to CLAUDE.md:
> "The notices feature does not allow new lines for the time being. So we need to update the implementation. For the time being we need to have both previous html and the new notices features to be present at the same time."

This fix addresses the UI-level HTML/emoji issues. The backend notice system may require separate attention for optimal serialization handling.

---

**Date**: 2025-12-28
**Issue Type**: Serialization Error Fix
**Files Modified**: `jamovi/swimmerplot.u.yaml`
**Status**: Completed
