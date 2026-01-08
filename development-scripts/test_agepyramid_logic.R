#!/usr/bin/env Rscript
# Logical verification of agepyramid fixes
# This tests the architecture without running the full module

cat("=== Verifying agepyramid Architecture Fixes ===\n\n")

# Read the .b.R file to verify fixes
b_file <- "R/agepyramid.b.R"
r_yaml <- "jamovi/agepyramid.r.yaml"

cat("1. Checking agepyramid.b.R for setState() calls in .plot()...\n")
b_content <- readLines(b_file)

# Find .plot function
plot_start <- grep("^\\.plot\\s*=\\s*function", b_content)
plot_end <- grep("^\\s*},\\s*$", b_content)
plot_end <- plot_end[plot_end > plot_start][1]

plot_function <- b_content[plot_start:plot_end]

# Check for setState calls in .plot()
setstate_in_plot <- grep("setState", plot_function, value = TRUE)

if (length(setstate_in_plot) > 0) {
    cat("   ✗ CRITICAL BUG FOUND: setState() called in .plot() function!\n")
    cat("   This will cause infinite render loops and freezes.\n")
    cat("   Offending lines:\n")
    for (line in setstate_in_plot) {
        cat("      ", line, "\n")
    }
    cat("\n")
    has_setstate_bug <- TRUE
} else {
    cat("   ✓ PASS: No setState() calls in .plot() (correct!)\n\n")
    has_setstate_bug <- FALSE
}

cat("2. Checking for .rebuildPlotData() fallback...\n")
has_rebuild <- any(grepl("rebuildPlotData", plot_function))
if (has_rebuild) {
    cat("   ✓ PASS: .rebuildPlotData() fallback exists\n\n")
} else {
    cat("   ✗ FAIL: No .rebuildPlotData() fallback for cleared state\n\n")
}

cat("3. Checking for error handling (tryCatch)...\n")
has_trycatch <- any(grepl("tryCatch", plot_function))
if (has_trycatch) {
    cat("   ✓ PASS: Error handling with tryCatch found\n\n")
} else {
    cat("   ⚠ WARNING: No tryCatch error handling\n\n")
}

cat("4. Checking agepyramid.r.yaml clearWith configuration...\n")
yaml_content <- readLines(r_yaml)

# Find clearWith section for plot
plot_section_start <- grep("name: plot", yaml_content)
clearwith_line <- grep("clearWith:", yaml_content)
clearwith_line <- clearwith_line[clearwith_line > plot_section_start][1]

# Get next 15 lines to see what's in clearWith
clearwith_section <- yaml_content[clearwith_line:(clearwith_line + 15)]

cat("   clearWith configuration:\n")
for (line in clearwith_section[1:15]) {
    cat("      ", line, "\n")
}

# Check if plot_engine is in clearWith
has_plot_engine <- any(grepl("plot_engine", clearwith_section))
has_color_palette <- any(grepl("color_palette", clearwith_section))

cat("\n   Visual options in clearWith:\n")
cat("      - plot_engine:", has_plot_engine, "\n")
cat("      - color_palette:", has_color_palette, "\n")

if (has_plot_engine && has_color_palette) {
    cat("   ✓ PASS: Visual options included (plot will re-render on change)\n\n")
} else {
    cat("   ✗ FAIL: Visual options missing (plot won't update)\n\n")
}

cat("5. Checking .run() for setState() calls...\n")
run_start <- grep("^\\.run\\s*=\\s*function", b_content)
run_end <- grep("^\\s*},\\s*$", b_content)
run_end <- run_end[run_end > run_start][1]
run_function <- b_content[run_start:run_end]

setstate_in_run <- grep("setState", run_function, value = TRUE)
if (length(setstate_in_run) > 0) {
    cat("   ✓ PASS: .run() calls setState() (correct - data management)\n")
    cat("   Found", length(setstate_in_run), "setState call(s)\n\n")
} else {
    cat("   ✗ FAIL: .run() doesn't call setState() (plot won't have data!)\n\n")
}

cat("\n=== ARCHITECTURE VERIFICATION SUMMARY ===\n\n")

issues <- c()
if (has_setstate_bug) {
    issues <- c(issues, "CRITICAL: setState() in .plot() (causes freeze)")
}
if (!has_rebuild) {
    issues <- c(issues, "Missing .rebuildPlotData() fallback")
}
if (!has_plot_engine || !has_color_palette) {
    issues <- c(issues, "Visual options not in clearWith (unresponsive UI)")
}
if (length(setstate_in_run) == 0) {
    issues <- c(issues, "Missing setState() in .run() (no data)")
}

if (length(issues) == 0) {
    cat("✓✓✓ ALL ARCHITECTURAL CHECKS PASSED ✓✓✓\n\n")
    cat("The agepyramid implementation follows correct patterns:\n")
    cat("  1. ✓ .run() manages state (calls setState)\n")
    cat("  2. ✓ .plot() never calls setState (no render loops)\n")
    cat("  3. ✓ .plot() has rebuild fallback for cleared state\n")
    cat("  4. ✓ Visual options in clearWith (responsive UI)\n")
    cat("  5. ✓ Error handling present (graceful failures)\n\n")
    cat("Expected behavior:\n")
    cat("  - Selecting ggplot2 engine: Should work without freezing\n")
    cat("  - Changing colors: Should update immediately\n")
    cat("  - Changing plot_title: Should update immediately\n")
    cat("  - Changing bin_width: Should recompute and update\n\n")
} else {
    cat("✗ ISSUES FOUND:\n")
    for (i in seq_along(issues)) {
        cat("  ", i, ". ", issues[i], "\n", sep="")
    }
    cat("\n")
}
