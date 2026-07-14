// cotest.events.js - Clinical preset system for co-testing analysis
// JavaScript events for intelligent defaults and dynamic UI updates

const clampNumeric = (value, min, max) => {
    if (!Number.isFinite(value))
        return value;
    return Math.min(max, Math.max(min, value));
};

const getNumericValue = (control, fallback) => {
    if (!control || typeof control.value !== 'function')
        return fallback;

    const raw = control.value();
    const numeric = (typeof raw === 'number') ? raw : parseFloat(raw);

    if (Number.isFinite(numeric))
        return numeric;

    return fallback;
};

const setControlValue = (control, value) => {
    if (!control || typeof control.setValue !== 'function')
        return;

    if (value === undefined || value === null)
        return;

    control.setValue(value);
};

const PRESET_CONFIGS = {
    custom: {},
    hpv_pap: {
        test1_sens: 0.95,
        test1_spec: 0.85,
        test2_sens: 0.70,
        test2_spec: 0.95,
        prevalence: 0.05,
        indep: false,
        cond_dep_pos: 0.15,
        cond_dep_neg: 0.10,
        guidance: 'HPV + Pap co-testing for cervical cancer screening. HPV has high sensitivity, Pap has high specificity. Tests show moderate dependence as they examine the same tissue.'
    },
    psa_dre: {
        test1_sens: 0.80,
        test1_spec: 0.70,
        test2_sens: 0.50,
        test2_spec: 0.85,
        prevalence: 0.15,
        indep: true,
        guidance: 'PSA + Digital Rectal Exam for prostate cancer screening. PSA is biochemical, DRE is physical - relatively independent tests with complementary strengths.'
    },
    troponin_ecg: {
        test1_sens: 0.90,
        test1_spec: 0.95,
        test2_sens: 0.70,
        test2_spec: 0.90,
        prevalence: 0.20,
        indep: false,
        cond_dep_pos: 0.20,
        cond_dep_neg: 0.05,
        guidance: 'Troponin + ECG for myocardial infarction diagnosis. Troponin is highly specific biochemical marker, ECG shows electrical changes. Moderate dependence as both reflect severity of cardiac damage.'
    },
    mammogram_ultrasound: {
        test1_sens: 0.85,
        test1_spec: 0.90,
        test2_sens: 0.80,
        test2_spec: 0.85,
        prevalence: 0.08,
        indep: false,
        cond_dep_pos: 0.25,
        cond_dep_neg: 0.15,
        guidance: 'Mammography + Ultrasound for breast cancer screening. Both are imaging modalities of the same tissue, showing significant dependence especially in dense breast tissue.'
    },
    covid_antigen_pcr: {
        test1_sens: 0.70,
        test1_spec: 0.95,
        test2_sens: 0.95,
        test2_spec: 0.99,
        prevalence: 0.10,
        indep: false,
        cond_dep_pos: 0.30,
        cond_dep_neg: 0.10,
        guidance: 'Rapid Antigen + PCR for COVID-19 diagnosis. Both tests detect SARS-CoV-2 but via different mechanisms. High dependence as both affected by viral load and sampling quality.'
    },
    tb_xray_sputum: {
        test1_sens: 0.75,
        test1_spec: 0.80,
        test2_sens: 0.85,
        test2_spec: 0.98,
        prevalence: 0.12,
        indep: false,
        cond_dep_pos: 0.20,
        cond_dep_neg: 0.08,
        guidance: 'Chest X-ray + Sputum microscopy for tuberculosis screening. X-ray shows structural changes, sputum shows organisms. Moderate dependence as advanced disease affects both tests.'
    }
};

const applyPresetConfig = (ui, presetKey) => {
    const config = PRESET_CONFIGS[presetKey];

    if (!config)
        return;

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

// Clamp an input control to the [min, max] range enforced by cotest.a.yaml.
const clampControl = (ui, paramName, min, max) => {
    const control = ui[paramName];

    if (!control)
        return;

    const value = getNumericValue(control, NaN);
    const adjusted = clampNumeric(value, min, max);

    if (Number.isFinite(adjusted) && adjusted !== value)
        setControlValue(control, adjusted);
};

const events = {
    onChange_preset(ui) {
        const presetControl = ui.preset;

        if (!presetControl || typeof presetControl.value !== 'function')
            return;

        const preset = presetControl.value();

        console.log('Co-testing preset changed to:', preset);

        if (preset === 'custom')
            return;

        applyPresetConfig(ui, preset);
    },

    onChange_test1_sens(ui) {
        clampControl(ui, 'test1_sens', 0.01, 0.99);
    },

    onChange_test1_spec(ui) {
        clampControl(ui, 'test1_spec', 0.01, 0.99);
    },

    onChange_test2_sens(ui) {
        clampControl(ui, 'test2_sens', 0.01, 0.99);
    },

    onChange_test2_spec(ui) {
        clampControl(ui, 'test2_spec', 0.01, 0.99);
    },

    onChange_prevalence(ui) {
        clampControl(ui, 'prevalence', 0.001, 0.999);
    },

    onChange_indep() {
        // Independence toggle has no client-side side effects; the R backend
        // selects the dependent/independent calculation branch at run time.
    },

    onChange_cond_dep_pos(ui) {
        clampControl(ui, 'cond_dep_pos', 0, 1);
    },

    onChange_cond_dep_neg(ui) {
        clampControl(ui, 'cond_dep_neg', 0, 1);
    }
};

module.exports = events;
