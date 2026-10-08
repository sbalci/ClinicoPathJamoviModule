// cotest.events.js - worked-example presets for the co-testing analysis
//
// No input clamping. The numeric boxes used to clamp an out-of-range entry to the nearest
// bound, so "80" typed for an 80% sensitivity silently became 0.99 and the analysis ran on
// it. The jamovi client does not enforce an option's min/max, but jmvcore does: left alone,
// the same entry stops the analysis with "test1_sens must be between 0.01 and 0.99 (is 80)".

const setControlValue = (control, value) => {
    if (!control || typeof control.setValue !== 'function')
        return;

    if (value === undefined || value === null)
        return;

    control.setValue(value);
};

// GENERATED FROM R/cotest.b.R .getPresetValues() -- keep in sync.
//
// These are the same numbers the backend computes with. They used to be a second,
// independently maintained copy, and 25 of 48 fields had drifted apart: the boxes on
// screen described one model while the results table reported another (for three
// presets they even disagreed about conditional independence). Editing this table
// without editing .getPresetValues() will fail the parity test in
// tests/testthat/test-cotest.R -- "the JS and R preset tables agree field for field".
//
// Every value is a round ILLUSTRATIVE figure for demonstrating the calculation. None of
// them is a pooled literature estimate and none should be used for patient care.
const PRESET_CONFIGS = {
    custom: {
        test1_sens: 0.80,
        test1_spec: 0.90,
        test2_sens: 0.75,
        test2_spec: 0.95,
        prevalence: 0.10,
        indep: false,
        cond_dep_pos: 0.05,
        cond_dep_neg: 0.05
    },
    hpv_pap: {
        test1_name: 'HPV',
        test2_name: 'Pap cytology',
        test1_sens: 0.95,
        test1_spec: 0.85,
        test2_sens: 0.70,
        test2_spec: 0.95,
        prevalence: 0.05,
        indep: false,
        cond_dep_pos: 0.15,
        cond_dep_neg: 0.10
    },
    psa_dre: {
        test1_name: 'PSA',
        test2_name: 'Rectal examination',
        test1_sens: 0.80,
        test1_spec: 0.70,
        test2_sens: 0.50,
        test2_spec: 0.85,
        prevalence: 0.15,
        indep: true
    },
    troponin_ecg: {
        test1_name: 'Troponin',
        test2_name: 'ECG',
        test1_sens: 0.90,
        test1_spec: 0.95,
        test2_sens: 0.70,
        test2_spec: 0.90,
        prevalence: 0.20,
        indep: false,
        cond_dep_pos: 0.20,
        cond_dep_neg: 0.05
    },
    mammogram_ultrasound: {
        test1_name: 'Mammography',
        test2_name: 'Ultrasound',
        test1_sens: 0.85,
        test1_spec: 0.90,
        test2_sens: 0.80,
        test2_spec: 0.85,
        prevalence: 0.08,
        indep: false,
        cond_dep_pos: 0.25,
        cond_dep_neg: 0.15
    },
    covid_antigen_pcr: {
        test1_name: 'Rapid antigen',
        test2_name: 'PCR',
        test1_sens: 0.70,
        test1_spec: 0.95,
        test2_sens: 0.95,
        test2_spec: 0.99,
        prevalence: 0.10,
        indep: false,
        cond_dep_pos: 0.30,
        cond_dep_neg: 0.10
    },
    tb_xray_sputum: {
        test1_name: 'Chest radiograph',
        test2_name: 'Sputum microscopy',
        test1_sens: 0.75,
        test1_spec: 0.80,
        test2_sens: 0.85,
        test2_spec: 0.98,
        prevalence: 0.12,
        indep: false,
        cond_dep_pos: 0.20,
        cond_dep_neg: 0.08
    }
};

const applyPresetConfig = (ui, presetKey) => {
    const config = PRESET_CONFIGS[presetKey];

    if (!config)
        return;

    setControlValue(ui.test1_name, config.test1_name === undefined ? '' : config.test1_name);
    setControlValue(ui.test2_name, config.test2_name === undefined ? '' : config.test2_name);
    setControlValue(ui.test1_sens, config.test1_sens);
    setControlValue(ui.test1_spec, config.test1_spec);
    setControlValue(ui.test2_sens, config.test2_sens);
    setControlValue(ui.test2_spec, config.test2_spec);
    setControlValue(ui.prevalence, config.prevalence);

    if (config.indep !== undefined)
        setControlValue(ui.indep, config.indep);

    if (config.cond_dep_pos !== undefined)
        setControlValue(ui.cond_dep_pos, config.cond_dep_pos);

    if (config.cond_dep_neg !== undefined)
        setControlValue(ui.cond_dep_neg, config.cond_dep_neg);
};

const events = {
    onChange_preset(ui) {
        const presetControl = ui.preset;

        if (!presetControl || typeof presetControl.value !== 'function')
            return;

        const preset = presetControl.value();

        // Switching back to "Custom values" used to return early, leaving the worked example's
        // numbers in the now-unlocked boxes with no disclosure anywhere: the results were
        // byte-identical to the preset run but r$notices$content was empty. One click laundered
        // a demonstration figure into an apparently user-entered one. Reset to the .a.yaml
        // defaults instead, so "custom" always means values the user actually chose.
        if (preset === 'custom') {
            applyPresetConfig(ui, 'custom');
            return;
        }

        applyPresetConfig(ui, preset);
    }
};

module.exports = events;
