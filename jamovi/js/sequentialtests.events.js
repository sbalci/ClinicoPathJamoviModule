// sequentialtests.events.js - teaching-example defaults and input clamping

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

const SEQUENTIAL_PRESET_CONFIGS = {
    custom: {},
    covid_screening_confirmation: {
        test1_name: 'Rapid Antigen Test',
        test1_sens: 0.75,
        test1_spec: 0.95,
        test2_name: 'RT-PCR',
        test2_sens: 0.95,
        test2_spec: 0.99,
        prevalence: 0.08,
        strategy: 'serial_positive'
    },
    breast_cancer_screening: {
        test1_name: 'Mammography',
        test1_sens: 0.85,
        test1_spec: 0.90,
        test2_name: 'Tissue Biopsy',
        test2_sens: 0.98,
        test2_spec: 0.99,
        prevalence: 0.06,
        strategy: 'serial_positive'
    },
    mi_emergency_parallel: {
        test1_name: 'Troponin',
        test1_sens: 0.90,
        test1_spec: 0.95,
        test2_name: 'ECG',
        test2_sens: 0.70,
        test2_spec: 0.90,
        prevalence: 0.20,
        strategy: 'parallel'
    },
    tb_screening_confirmation: {
        test1_name: 'Chest X-ray',
        test1_sens: 0.75,
        test1_spec: 0.80,
        test2_name: 'Sputum Culture',
        test2_sens: 0.85,
        test2_spec: 0.98,
        prevalence: 0.12,
        strategy: 'serial_positive'
    },
    prostate_screening_exclusion: {
        test1_name: 'PSA Test',
        test1_sens: 0.80,
        test1_spec: 0.70,
        test2_name: 'MRI',
        test2_sens: 0.90,
        test2_spec: 0.85,
        prevalence: 0.15,
        strategy: 'serial_negative'
    },
    hiv_screening_confirmation: {
        test1_name: 'HIV Ag/Ab Assay',
        test1_sens: 0.98,
        test1_spec: 0.95,
        test2_name: 'Differentiation Assay',
        test2_sens: 0.99,
        test2_spec: 0.99,
        prevalence: 0.02,
        strategy: 'serial_positive'
    },
    stroke_emergency_parallel: {
        test1_name: 'Clinical Assessment',
        test1_sens: 0.85,
        test1_spec: 0.75,
        test2_name: 'CT Scan',
        test2_sens: 0.95,
        test2_spec: 0.98,
        prevalence: 0.25,
        strategy: 'parallel'
    }
};

const applySequentialPresetConfig = (ui, presetKey) => {
    const config = SEQUENTIAL_PRESET_CONFIGS[presetKey];

    if (!config)
        return;

    setControlValue(ui.test1_name, config.test1_name);
    setControlValue(ui.test1_sens, config.test1_sens);
    setControlValue(ui.test1_spec, config.test1_spec);
    setControlValue(ui.test2_name, config.test2_name);
    setControlValue(ui.test2_sens, config.test2_sens);
    setControlValue(ui.test2_spec, config.test2_spec);
    setControlValue(ui.prevalence, config.prevalence);

    if (config.strategy !== undefined)
        setControlValue(ui.strategy, config.strategy);
};

const events = {
    onChange_preset(ui) {
        const presetControl = ui.preset;

        if (!presetControl || typeof presetControl.value !== 'function')
            return;

        const preset = presetControl.value();

        if (preset === 'custom')
            return;

        applySequentialPresetConfig(ui, preset);
    },

    onChange_test1_sens(ui) {
        const control = ui.test1_sens;
        if (!control) return;

        const value = getNumericValue(control, NaN);
        const adjusted = clampNumeric(value, 0.01, 0.99);

        if (Number.isFinite(adjusted) && adjusted !== value)
            setControlValue(control, adjusted);
    },

    onChange_test1_spec(ui) {
        const control = ui.test1_spec;
        if (!control) return;

        const value = getNumericValue(control, NaN);
        const adjusted = clampNumeric(value, 0.01, 0.99);

        if (Number.isFinite(adjusted) && adjusted !== value)
            setControlValue(control, adjusted);
    },

    onChange_test2_sens(ui) {
        const control = ui.test2_sens;
        if (!control) return;

        const value = getNumericValue(control, NaN);
        const adjusted = clampNumeric(value, 0.01, 0.99);

        if (Number.isFinite(adjusted) && adjusted !== value)
            setControlValue(control, adjusted);
    },

    onChange_test2_spec(ui) {
        const control = ui.test2_spec;
        if (!control) return;

        const value = getNumericValue(control, NaN);
        const adjusted = clampNumeric(value, 0.01, 0.99);

        if (Number.isFinite(adjusted) && adjusted !== value)
            setControlValue(control, adjusted);
    },

    onChange_prevalence(ui) {
        const control = ui.prevalence;

        if (!control)
            return;

        let prevalence = getNumericValue(control, NaN);

        if (Number.isFinite(prevalence) && prevalence < 0.001) {
            prevalence = 0.001;
            setControlValue(control, prevalence);
        } else if (Number.isFinite(prevalence) && prevalence > 0.999) {
            prevalence = 0.999;
            setControlValue(control, prevalence);
        }
    }
};

module.exports = events;
