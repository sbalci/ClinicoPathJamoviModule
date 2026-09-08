// Clinical presets for the Advanced Ridge Plot.
//
// WHY THIS FILE EXISTS
// The backend used to apply presets purely through `private$overrides`, an R-side map the
// jamovi client cannot see. `.u.yaml` `enable:` expressions are evaluated CLIENT-side
// against the raw option values, so selecting "Biomarker Distribution" left
// `show_stats` reading FALSE in the client while the analysis ran nonparametric tests with
// Cliff's delta and FDR correction: Statistical test / P-value adjustment / Effect size
// type stayed greyed out, and the user could neither see nor change the settings behind
// their own p-values. Writing the real option values here keeps the panel honest.
//
// KEEPING THE TABLE IN SYNC
// PRESETS below mirrors `private$.PRESETS` in R/jjridges.b.R exactly. That duplication is
// deliberate (the client cannot call into R), and it is guarded:
// tests/testthat/test-jjridges-residual.R parses BOTH files and fails on any drift.
//
// Presets are applied on CHANGE only. Selecting "Custom" afterwards leaves the values in
// place so the user can keep a preset's settings and adjust one of them.

const PRESETS = {
    biomarker_distribution: {
        plot_type: "density_ridges",
        add_boxplot: true,
        add_quantiles: true,
        quantiles: "0.25, 0.5, 0.75",
        theme_style: "theme_pubr",
        color_palette: "clinical_colorblind",
        show_stats: true,
        test_type: "nonparametric",
        effsize_type: "cliff_delta",
        p_adjust_method: "fdr"
    },
    treatment_response: {
        plot_type: "violin_ridges",
        show_stats: true,
        test_type: "nonparametric",
        effsize_type: "cliff_delta",
        theme_style: "theme_pubr",
        color_palette: "clinical_colorblind",
        add_boxplot: true,
        p_adjust_method: "bonferroni"
    },
    age_by_stage: {
        plot_type: "density_ridges",
        add_mean: true,
        add_median: true,
        theme_style: "theme_pubr",
        color_palette: "viridis",
        show_stats: true,
        test_type: "parametric",
        effsize_type: "d"
    },
    tumor_size_comparison: {
        plot_type: "density_ridges",
        add_boxplot: true,
        add_quantiles: true,
        quantiles: "0.25, 0.5, 0.75",
        theme_style: "theme_pubr",
        color_palette: "clinical_colorblind",
        show_stats: true,
        test_type: "nonparametric",
        effsize_type: "hodges_lehmann",
        p_adjust_method: "holm"
    },
    lab_values_by_group: {
        plot_type: "density_ridges",
        add_boxplot: true,
        theme_style: "theme_pubr",
        color_palette: "clinical_colorblind",
        show_stats: true,
        test_type: "robust",
        effsize_type: "g",
        p_adjust_method: "fdr"
    },
    survival_time_distribution: {
        plot_type: "density_ridges",
        add_median: true,
        add_quantiles: true,
        quantiles: "0.25, 0.5, 0.75",
        theme_style: "theme_pubr",
        color_palette: "Set2",
        show_stats: true,
        test_type: "nonparametric",
        effsize_type: "hodges_lehmann",
        p_adjust_method: "holm"
    }
};

// Look the control up defensively. A name that does not resolve returns undefined, and
// calling .setValue() on it throws a TypeError that aborts the whole handler - which in
// the sibling jjhistostats module silently skipped every control after the first bad one.
const setOpt = function(ui, name, value) {
    const c = Object.prototype.hasOwnProperty.call(ui, name) ? ui[name] : null;
    if (c && typeof c.setValue === "function")
        c.setValue(value);
};

const events = {
    onChange_clinicalPreset: function(ui) {
        const preset = ui.clinicalPreset.value();
        const spec = Object.prototype.hasOwnProperty.call(PRESETS, preset)
            ? PRESETS[preset] : null;
        if (spec === null)     // "custom": keep whatever the user currently has
            return;

        for (const name of Object.keys(spec))
            setOpt(ui, name, spec[name]);
    }
};

module.exports = events;
