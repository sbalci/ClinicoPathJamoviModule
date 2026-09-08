// Behavioural check for jamovi/js/jjridges.events.js.
// Run: node development-scripts/verify_jjridges_events.js
// Exits non-zero on failure; tests/testthat/test-jjridges-residual.R shells out to it
// when node is available.
const path = require("path");
const events = require(path.join(__dirname, "..", "jamovi", "js", "jjridges.events.js"));

let failures = 0;
const check = (label, ok) => {
    if (!ok) { failures++; console.log("FAIL: " + label); }
    else console.log("ok  : " + label);
};

// Minimal stand-in for jamovi's ui object: every control records what it was set to.
const makeUi = function (preset, omit) {
    const names = ["plot_type", "add_boxplot", "add_quantiles", "quantiles", "theme_style",
                   "color_palette", "show_stats", "test_type", "effsize_type",
                   "p_adjust_method", "add_mean", "add_median"];
    const ui = { written: {} };
    ui.clinicalPreset = { value: () => preset };
    for (const n of names) {
        if (n === omit) continue;
        ui[n] = { setValue: (v) => { ui.written[n] = v; } };
    }
    return ui;
};

// 1. a preset writes exactly its own settings
let ui = makeUi("biomarker_distribution");
events.onChange_clinicalPreset(ui);
check("biomarker_distribution sets show_stats true", ui.written.show_stats === true);
check("biomarker_distribution sets nonparametric",   ui.written.test_type === "nonparametric");
check("biomarker_distribution sets cliff_delta",     ui.written.effsize_type === "cliff_delta");
check("biomarker_distribution sets fdr",             ui.written.p_adjust_method === "fdr");
check("biomarker_distribution writes 10 controls",   Object.keys(ui.written).length === 10);
check("biomarker_distribution leaves add_mean alone", !("add_mean" in ui.written));

// 2. violin preset differs
ui = makeUi("treatment_response");
events.onChange_clinicalPreset(ui);
check("treatment_response sets violin_ridges", ui.written.plot_type === "violin_ridges");
check("treatment_response sets bonferroni",    ui.written.p_adjust_method === "bonferroni");

// 3. "custom" must not touch anything (the user keeps their settings)
ui = makeUi("custom");
events.onChange_clinicalPreset(ui);
check("custom writes nothing", Object.keys(ui.written).length === 0);

// 4. an unknown preset must not throw
ui = makeUi("not_a_preset");
try { events.onChange_clinicalPreset(ui); check("unknown preset is a no-op", Object.keys(ui.written).length === 0); }
catch (e) { check("unknown preset is a no-op (threw: " + e.message + ")", false); }

// 5. a control missing from the panel must not abort the rest of the handler.
//    This is the exact failure the sibling jjhistostats module shipped with: one
//    unresolved name threw a TypeError and every control after it was silently skipped.
ui = makeUi("biomarker_distribution", "theme_style");
try {
    events.onChange_clinicalPreset(ui);
    check("missing control does not abort the handler", ui.written.p_adjust_method === "fdr");
    check("missing control is simply not written", !("theme_style" in ui.written));
} catch (e) {
    check("missing control does not abort the handler (threw: " + e.message + ")", false);
}

console.log(failures === 0 ? "\nALL EVENTS CHECKS PASSED" : "\n" + failures + " FAILURE(S)");
process.exit(failures === 0 ? 0 : 1);
