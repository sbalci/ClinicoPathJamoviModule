# Static and guidance HTML builders for the swimmerplot analysis.
#
# Extracted from swimmerplot.b.R for navigability: these functions build
# fixed educational/guidance panels and touch no analysis state. Each takes
# `self` so that the jmvcore `.()` translation helper can resolve the analysis
# context from the calling frame (calling `.()` from a helper WITHOUT self in
# scope throws "object 'self' not found" - the issue-122 class).
#
# All user-visible text is wrapped in `.()` with the HTML tags OUTSIDE the
# msgids, so translators never see markup.

#' @noRd
swimmerplot_instructions_html <- function(self) {
    paste0(
        "<div style='background-color: rgba(33, 181, 248, 0.14); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>",
        "<h3 style='color: #0277bd; margin-top: 0;'>", .("Swimmer Plot Analysis"), "</h3>",
        "<p>", .("Create comprehensive swimmer plots for visualizing patient timelines, treatments, and clinical events using the advanced ggswim package."), "</p>",

        "<div style='margin: 15px 0;'>",
        "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>", .("Required Variables:"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
        "<li><strong>", .("Patient ID:"), "</strong> ", .("Unique identifier for each patient"), "</li>",
        "<li><strong>", .("Start Time:"), "</strong> ", .("Treatment/observation start time"), "</li>",
        "<li><strong>", .("End Time:"), "</strong> ", .("Treatment/observation end time"), "</li>",
        "</ul>",
        "</div>",

        "<div style='margin: 15px 0;'>",
        "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>", .("Enhanced Features:"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
        "<li><strong>", .("Complete ggswim Integration:"), "</strong> ", .("Professional clinical visualization"), "</li>",
        "<li><strong>", .("Milestone Support:"), "</strong> ", .("Track key clinical events (surgery, progression, etc.)"), "</li>",
        "<li><strong>", .("Event Markers:"), "</strong> ", .("Show specific events along patient timelines"), "</li>",
        "<li><strong>", .("Person-time Analysis:"), "</strong> ", .("Epidemiological metrics and follow-up analysis"), "</li>",
        "<li><strong>", .("Clinical Interpretation:"), "</strong> ", .("Automated insights for research"), "</li>",
        "<li><strong>", .("Enhanced Data Validation:"), "</strong> ", .("Robust error handling and type conversion"), "</li>",
        "</ul>",
        "</div>",

        "<div style='margin: 15px 0;'>",
        "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>", .("Visualization Options:"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
        "<li><strong>", .("Swim Lanes:"), "</strong> ", .("Horizontal patient timelines with response coloring"), "</li>",
        "<li><strong>", .("Event Markers:"), "</strong> ", .("Custom glyphs for clinical events"), "</li>",
        "<li><strong>", .("Status Arrows:"), "</strong> ", .("Ongoing treatment indicators"), "</li>",
        "<li><strong>", .("Reference Lines:"), "</strong> ", .("Protocol times, median values, custom timepoints"), "</li>",
        "<li><strong>", .("Clinical Themes:"), "</strong> ", .("Professional styling for publications"), "</li>",
        "</ul>",
        "</div>",

        "<div style='margin: 15px 0;'>",
        "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>", .("Data Formats Supported:"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
        "<li><strong>", .("Raw Numeric:"), "</strong> ", .("Days, weeks, months from treatment start"), "</li>",
        "<li><strong>", .("Date/Time:"), "</strong> ", .("Actual calendar dates with multiple format support"), "</li>",
        "<li><strong>", .("Relative vs Absolute:"), "</strong> ", .("Timeline display options"), "</li>",
        "<li><strong>", .("Multiple Time Units:"), "</strong> ", .("Days, weeks, months, years"), "</li>",
        "</ul>",
        "</div>",

        "<div style='background-color: rgba(255, 169, 33, 0.14); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
        "<p style='margin: 0; color: #f57c00;'><strong>", .("Clinical Research Applications:"), "</strong> ",
        .("Ideal for oncology trials, treatment response visualization, progression tracking, and regulatory submissions."), "</p>",
        "</div>",

        "<div style='background-color: rgba(153, 33, 170, 0.12); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
        "<p style='margin: 0; color: #7b1fa2;'><strong>", .("Enhanced Features:"), "</strong> ",
        .("Complete ggswim package integration with swim lanes, event markers, status arrows, and professional clinical themes for maximum flexibility and publication-ready output."), "</p>",
        "</div>",

        "</div>"
    )
}

#' @noRd
swimmerplot_glossary_html <- function(self) {
    paste0(
        "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 15px 0; font-family: system-ui, -apple-system, sans-serif; color: inherit;'>",
        "<h3 style='color: #007bff; margin-top: 0;'>", .("Clinical Glossary"), "</h3>",

        "<div style='margin: 10px 0;'>",
        "<h4 style='color: #0056b3; margin: 10px 0 5px 0;'>", .("Response Categories"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px;'>",
        "<li><strong>", .("CR (Complete Response):"), "</strong> ", .("Complete disappearance of all target lesions"), "</li>",
        "<li><strong>", .("PR (Partial Response):"), "</strong> ", .("\u{2265}30% decrease in sum of target lesion diameters"), "</li>",
        "<li><strong>", .("SD (Stable Disease):"), "</strong> ", .("Neither sufficient shrinkage for PR nor sufficient increase for PD"), "</li>",
        "<li><strong>", .("PD (Progressive Disease):"), "</strong> ", .("\u{2265}20% increase in sum of target lesion diameters"), "</li>",
        "</ul>",
        "</div>",

        "<div style='margin: 10px 0;'>",
        "<h4 style='color: #0056b3; margin: 10px 0 5px 0;'>", .("Clinical Metrics"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px;'>",
        "<li><strong>", .("ORR (Objective Response Rate):"), "</strong> ", .("Proportion of patients with CR or PR"), "</li>",
        "<li><strong>", .("DCR (Disease Control Rate):"), "</strong> ", .("Proportion of patients with CR, PR, or SD"), "</li>",
        "<li><strong>", .("Person-Time:"), "</strong> ", .("Total observation time across all patients in the study"), "</li>",
        "<li><strong>", .("Median Follow-up:"), "</strong> ", .("Calculated using reverse Kaplan-Meier method when censoring data is provided (gold standard). Otherwise uses simple median."), "</li>",
        "<li><strong>", .("Follow-up Density:"), "</strong> ", .("Descriptive measure of patient concentration per unit of observation time (not an event incidence rate)"), "</li>",
        "</ul>",
        "</div>",

        "<div style='margin: 10px 0;'>",
        "<h4 style='color: #0056b3; margin: 10px 0 5px 0;'>", .("Statistical Terms"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px;'>",
        "<li><strong>", .("95% CI (Confidence Interval):"), "</strong> ", .("Range of values compatible with the observed data; over repeated studies, 95% of such intervals contain the true population parameter. For response rates, exact binomial CIs are used for accuracy with small sample sizes."), "</li>",
        "<li><strong>", .("Fisher's Exact Test:"), "</strong> ", .("Statistical test for comparing categorical outcomes (like response rates) between groups. Used to determine if response rates differ significantly between patient groups. Does not require large sample sizes and is valid for small cell counts."), "</li>",
        "<li><strong>", .("Odds Ratio (OR):"), "</strong> ", .("Measure of association between group membership and outcome. OR > 1 indicates higher odds of response in the comparison group; OR < 1 indicates lower odds. Example: OR = 2.5 means the comparison group has 2.5 times the odds of responding."), "</li>",
        "<li><strong>", .("P-value:"), "</strong> ", .("Probability of observing results as extreme or more extreme than observed, assuming no true difference exists. Convention: p < 0.05 indicates statistical significance, but clinical significance should also be considered alongside statistical significance."), "</li>",
        "</ul>",
        "</div>",

        "<div style='margin: 10px 0;'>",
        "<h4 style='color: #0056b3; margin: 10px 0 5px 0;'>", .("Timeline Elements"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px;'>",
        "<li><strong>", .("Swim Lanes:"), "</strong> ", .("Horizontal bars representing individual patient treatment courses"), "</li>",
        "<li><strong>", .("Milestones:"), "</strong> ", .("Key clinical events (surgery, assessment, progression)"), "</li>",
        "<li><strong>", .("Event Markers:"), "</strong> ", .("Specific events occurring during treatment"), "</li>",
        "<li><strong>", .("Status Arrows:"), "</strong> ", .("Indicate ongoing treatment at data cutoff"), "</li>",
        "</ul>",
        "</div>",

        "</div>"
    )
}

#' @noRd
swimmerplot_about_html <- function(self) {
    paste0(
        "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 20px; border-left: 4px solid #ffc107; border-radius: 8px; margin: 15px 0; font-family: system-ui, -apple-system, sans-serif; color: inherit;'>",
        "<h3 style='color: #856404; margin-top: 0;'>", .("About Swimmer Plot Analysis"), "</h3>",

        "<div style='margin: 15px 0;'>",
        "<h4 style='color: #856404; margin: 10px 0 5px 0;'>", .("What is a Swimmer Plot?"), "</h4>",
        "<p style='margin: 5px 0; line-height: 1.6;'>",
        .("Swimmer plots are timeline visualizations that display individual patient treatment courses, clinical events, and outcomes in a single comprehensive graph. Each horizontal 'swim lane' represents one patient's journey through treatment."),
        "</p>",
        "</div>",

        "<div style='margin: 15px 0;'>",
        "<h4 style='color: #856404; margin: 10px 0 5px 0;'>", .("When to Use Swimmer Plots"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
        "<li>", .("Clinical trial data visualization and regulatory submissions"), "</li>",
        "<li>", .("Treatment response assessment and duration analysis"), "</li>",
        "<li>", .("Patient outcome tracking in longitudinal studies"), "</li>",
        "<li>", .("Safety event monitoring and adverse event reporting"), "</li>",
        "<li>", .("Milestone-based clinical pathway analysis"), "</li>",
        "</ul>",
        "</div>",

        "<div style='margin: 15px 0;'>",
        "<h4 style='color: #856404; margin: 10px 0 5px 0;'>", .("Required Data"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
        "<li><strong>", .("Patient ID:"), "</strong> ", .("Unique identifier for each patient"), "</li>",
        "<li><strong>", .("Start Time:"), "</strong> ", .("Treatment or observation start date/time"), "</li>",
        "<li><strong>", .("End Time:"), "</strong> ", .("Treatment or observation end date/time"), "</li>",
        "<li><strong>", .("Response Variable (optional):"), "</strong> ", .("Treatment response categories"), "</li>",
        "<li><strong>", .("Milestone Events (optional):"), "</strong> ", .("Key clinical events with dates"), "</li>",
        "</ul>",
        "</div>",

        "<div style='margin: 15px 0;'>",
        "<h4 style='color: #856404; margin: 10px 0 5px 0;'>", .("Key Assumptions"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
        "<li>", .("Each row represents one patient episode or treatment course"), "</li>",
        "<li>", .("Time variables are either numeric (days/months) or valid date formats"), "</li>",
        "<li>", .("End times should be greater than or equal to start times"), "</li>",
        "<li>", .("Missing data is handled appropriately (excluded from calculations)"), "</li>",
        "<li>", .("Response categories follow standard clinical criteria (RECIST, etc.)"), "</li>",
        "</ul>",
        "</div>",

        "<div style='margin: 15px 0;'>",
        "<h4 style='color: #856404; margin: 10px 0 5px 0;'>", .("Output Interpretation"), "</h4>",
        "<p style='margin: 5px 0; line-height: 1.6;'>",
        .("The swimmer plot displays individual patient timelines with optional color coding for response categories. Milestone markers show key events, and summary statistics provide overall study metrics including person-time analysis and response rates."),
        "</p>",
        "</div>",

        "<div style='margin: 15px 0;'>",
        "<h4 style='color: #856404; margin: 10px 0 5px 0;'>", .("Important Considerations for Regulatory Documentation"), "</h4>",
        "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
        "<li><strong>", .("Event Markers:"), "</strong> ", .("Drawn with print-safe geometric symbols that render correctly in PDF/Word exports and regulatory documents."), "</li>",
        "<li><strong>", .("Censoring Variable:"), "</strong> ", .("Provide an explicit censoring/status variable for accurate ongoing treatment arrows and reverse Kaplan-Meier median follow-up calculation."), "</li>",
        "<li><strong>", .("Response Categories:"), "</strong> ", .("Use standard abbreviations (CR, PR, SD, PD) for consistency, though case-insensitive matching is supported."), "</li>",
        "</ul>",
        "</div>",

        "</div>"
    )
}

#' @noRd
swimmerplot_mismatch_guidance_html <- function(self, safe_examples) {
    example_codes <- if (length(safe_examples) > 0) {
        paste0("<code style='background-color: rgba(33, 33, 33, 0.1); padding: 2px 6px; border-radius: 3px; font-family: monospace; color: inherit;'>",
               paste(safe_examples,
                     collapse = "</code>, <code style='background-color: rgba(33, 33, 33, 0.1); padding: 2px 6px; border-radius: 3px; font-family: monospace; color: inherit;'>"),
               "</code>")
    } else {
        .("numeric values")
    }

    paste0(
        "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",

        "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid #d63384; padding: 20px; margin-bottom: 20px; color: inherit;'>",
        "<h2 style='margin: 0 0 10px 0; font-size: 20px; color: #d63384;'>", .("Data Type Mismatch"), "</h2>",
        "<p style='margin: 0; font-size: 14px; color: inherit;'>",
        .("You selected Date/Time input type, but your data contains numeric values."),
        "</p>",
        "<p style='margin: 10px 0 0 0; font-size: 14px; color: inherit;'>",
        .("Examples:"), " ", example_codes,
        "</p>",
        "</div>",

        "<div style='background-color: rgba(155, 155, 155, 0.06); border-left: 4px solid #d63384; padding: 15px; margin-bottom: 20px; color: inherit;'>",
        "<h3 style='margin: 0 0 10px 0; color: inherit; font-size: 16px;'>", .("Required Action"), "</h3>",
        "<ol style='margin: 0; padding-left: 20px; font-size: 14px; line-height: 1.6;'>",
        "<li>", .("Go to the 'Time & Date Settings' section (click to expand)"), "</li>",
        "<li>", .("Change 'Time Input Type' from 'Date/Time' to 'Raw Values'"), "</li>",
        "<li>", .("Select the appropriate 'Time Unit' (Days, Weeks, Months, or Years)"), "</li>",
        "<li>", .("Choose your preferred 'Time Display' mode"), "</li>",
        "<li>", .("Re-run the analysis"), "</li>",
        "</ol>",
        "</div>",

        "<div style='background-color: rgba(155, 155, 155, 0.06); border: 1px solid #ccc; padding: 15px; color: inherit;'>",
        "<h4 style='margin: 0 0 10px 0; font-size: 15px; color: inherit;'>", .("Data Type Guide"), "</h4>",
        "<p style='margin: 0; font-size: 14px; color: inherit;'>",
        "<strong>", .("Use Date/Time for:"), "</strong> 2023-01-15, 15/01/2023, 2023-01-15 14:30:00<br>",
        "<strong>", .("Use Raw Values for:"), "</strong> 0, 30, 90.5, 365 ", .("(numeric days/months/years)"),
        "</p>",
        "</div>",

        "</div>"
    )
}

#' @noRd
swimmerplot_date_guidance_html <- function(self, safe_format, safe_examples) {
    example_codes <- paste0(
        "<code style='background-color: rgba(33, 33, 33, 0.1); padding: 2px 6px; border-radius: 3px; font-family: monospace; color: inherit;'>",
        paste(safe_examples,
              collapse = "</code>, <code style='background-color: rgba(33, 33, 33, 0.1); padding: 2px 6px; border-radius: 3px; font-family: monospace; color: inherit;'>"),
        "</code>")

    paste0(
        "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",

        "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid #333; padding: 20px; margin-bottom: 20px; color: inherit;'>",
        "<h2 style='margin: 0 0 10px 0; font-size: 20px; color: inherit;'>", .("Date Format Detected"), "</h2>",
        "<p style='margin: 0; font-size: 14px; color: inherit;'>",
        .("Found date format:"), " <strong style='color: inherit;'>", safe_format, "</strong> ",
        .("in your time variables"),
        "</p>",
        "<p style='margin: 10px 0 0 0; font-size: 14px; color: inherit;'>",
        .("Examples:"), " ", example_codes,
        "</p>",
        "</div>",

        "<div style='background-color: rgba(155, 155, 155, 0.06); border-left: 4px solid #333; padding: 15px; margin-bottom: 20px; color: inherit;'>",
        "<h3 style='margin: 0 0 10px 0; color: inherit; font-size: 16px;'>", .("Required Action"), "</h3>",
        "<ol style='margin: 0; padding-left: 20px; font-size: 14px; line-height: 1.6;'>",
        "<li>", .("Go to the 'Time & Date Settings' section (click to expand)"), "</li>",
        "<li>", .("Change 'Time Input Type' from 'Raw Values' to 'Date/Time'"), "</li>",
        "<li>", .("Select 'Date Format':"), " <span style='background-color: rgba(33, 33, 33, 0.1); padding: 2px 6px; border-radius: 3px; color: inherit;'>",
        safe_format, "</span></li>",
        "<li>", .("Choose your preferred 'Time Display' mode (Relative or Absolute)"), "</li>",
        "<li>", .("The analysis will be re-run with your settings."), "</li>",
        "</ol>",
        "</div>",

        "<div style='background-color: rgba(155, 155, 155, 0.06); border: 1px solid #ccc; padding: 15px; color: inherit;'>",
        "<h4 style='margin: 0 0 10px 0; font-size: 15px; color: inherit;'>", .("Important Note"), "</h4>",
        "<p style='margin: 0; font-size: 14px; color: inherit;'>",
        .("Configuring the date settings properly ensures accurate timeline calculations and gives you full control over how dates are displayed in your swimmer plot."),
        "</p>",
        "</div>",

        "</div>"
    )
}
