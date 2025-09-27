// sequentialtests.events.js - Clinical preset system for sequential testing analysis
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

const updateClinicalGuidance = (ui) => {
    // Collect all guidance messages
    const preset = ui.preset ? ui.preset.value() : 'custom';
    const test1Name = ui.test1_name ? ui.test1_name.value() : 'Test 1';
    const test2Name = ui.test2_name ? ui.test2_name.value() : 'Test 2';
    const strategy = ui.strategy ? ui.strategy.value() : 'serial_positive';

    const test1Sens = getNumericValue(ui.test1_sens, 0.80);
    const test1Spec = getNumericValue(ui.test1_spec, 0.90);
    const test2Sens = getNumericValue(ui.test2_sens, 0.75);
    const test2Spec = getNumericValue(ui.test2_spec, 0.95);
    const prevalence = getNumericValue(ui.prevalence, 0.10);

    let guidanceHtml = '<h4>Clinical Guidance</h4>';

    // Preset guidance
    if (preset !== 'custom') {
        const config = SEQUENTIAL_PRESET_CONFIGS[preset];
        if (config && config.guidance) {
            guidanceHtml += `<div class="alert alert-info">
                <h5><span class="glyphicon glyphicon-info-sign"></span> Selected Scenario</h5>
                <p>${config.guidance}</p>
            </div>`;
        }
    }

    // Test performance guidance
    guidanceHtml += '<div class="row">';
    guidanceHtml += '<div class="col-md-6">';
    guidanceHtml += `<h5>${test1Name} Performance</h5>`;
    guidanceHtml += '<ul>';
    guidanceHtml += `<li>${getTestPerformanceGuidance(test1Sens, 'sensitivity', test1Name)}</li>`;
    guidanceHtml += `<li>${getTestPerformanceGuidance(test1Spec, 'specificity', test1Name)}</li>`;
    guidanceHtml += '</ul>';
    guidanceHtml += '</div>';

    guidanceHtml += '<div class="col-md-6">';
    guidanceHtml += `<h5>${test2Name} Performance</h5>`;
    guidanceHtml += '<ul>';
    guidanceHtml += `<li>${getTestPerformanceGuidance(test2Sens, 'sensitivity', test2Name)}</li>`;
    guidanceHtml += `<li>${getTestPerformanceGuidance(test2Spec, 'specificity', test2Name)}</li>`;
    guidanceHtml += '</ul>';
    guidanceHtml += '</div>';
    guidanceHtml += '</div>';

    // Strategy guidance
    let strategyGuidance = '';
    if (strategy === 'serial_positive') {
        strategyGuidance = '✅ Serial positive strategy - Tests positives from first test. Maximizes specificity, useful for confirmation.';
        if (test2Spec <= test1Spec) {
            strategyGuidance += ' | ⚠️ For confirmation, second test should have higher specificity than first';
        }
    } else if (strategy === 'serial_negative') {
        strategyGuidance = 'ℹ️ Serial negative strategy - Tests negatives from first test. Maximizes sensitivity, useful for exclusion.';
        if (test2Sens <= test1Sens) {
            strategyGuidance += ' | ⚠️ For exclusion, second test should have higher sensitivity than first';
        }
    } else if (strategy === 'parallel') {
        strategyGuidance = 'ℹ️ Parallel strategy - Tests everyone with both tests. Maximizes sensitivity, useful for rapid diagnosis.';
        if (Math.abs(test1Sens - test2Sens) < 0.05 && Math.abs(test1Spec - test2Spec) < 0.05) {
            strategyGuidance += ' | ℹ️ Parallel testing works best with complementary test characteristics';
        }
    }

    guidanceHtml += `<div class="alert alert-primary">
        <h5><span class="glyphicon glyphicon-cog"></span> Testing Strategy</h5>
        <p>${strategyGuidance}</p>
    </div>`;

    // Prevalence guidance
    let prevalenceGuidance = '';
    if (prevalence < 0.01) {
        prevalenceGuidance = 'ℹ️ Very rare disease - consider cost-effectiveness of sequential testing';
    } else if (prevalence < 0.05) {
        prevalenceGuidance = 'ℹ️ Rare disease - screening strategy should be carefully designed';
    } else if (prevalence < 0.20) {
        prevalenceGuidance = '✅ Typical screening population prevalence';
    } else if (prevalence < 0.50) {
        prevalenceGuidance = 'ℹ️ High-risk population - consider diagnostic rather than screening approach';
    } else {
        prevalenceGuidance = '⚠️ Very high prevalence - may indicate diagnostic rather than screening setting';
    }

    // Calculate estimated metrics for prevalence guidance
    const metrics = calculateSequentialMetrics(ui);
    if (Number.isFinite(metrics.ppv) && Number.isFinite(metrics.npv)) {
        prevalenceGuidance += ` | Est. PPV≈${formatProbability(metrics.ppv)}, NPV≈${formatProbability(metrics.npv)}`;
    }

    guidanceHtml += `<div class="alert alert-warning">
        <h5><span class="glyphicon glyphicon-stats"></span> Disease Prevalence</h5>
        <p>${prevalenceGuidance}</p>
    </div>`;

    // Set the clinical guidance HTML
    const guidanceControl = ui.clinical_guidance;
    if (guidanceControl && typeof guidanceControl.setContent === 'function') {
        guidanceControl.setContent(guidanceHtml);
    }
};

const getTestPerformanceGuidance = (value, type, testName) => {
    if (!Number.isFinite(value)) {
        return `⚠️ ${testName} ${type} is not a number`;
    }

    if (type === 'sensitivity') {
        if (value < 0.60) return `⚠️ Low sensitivity (${formatProbability(value)}) - many cases will be missed`;
        else if (value < 0.80) return `ℹ️ Moderate sensitivity (${formatProbability(value)}) - acceptable for screening`;
        else if (value < 0.95) return `✅ Good sensitivity (${formatProbability(value)}) - suitable for screening`;
        else return `✅ Excellent sensitivity (${formatProbability(value)}) - ideal for screening`;
    } else {
        if (value < 0.70) return `⚠️ Low specificity (${formatProbability(value)}) - many false positives expected`;
        else if (value < 0.90) return `ℹ️ Moderate specificity (${formatProbability(value)}) - acceptable for initial screening`;
        else if (value < 0.98) return `✅ Good specificity (${formatProbability(value)}) - low false positive rate`;
        else return `✅ Excellent specificity (${formatProbability(value)}) - very low false positive rate`;
    }
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
        strategy: 'serial_positive',
        guidance: 'COVID-19 screening with rapid antigen followed by RT-PCR confirmation. Rapid test screens efficiently, PCR confirms positives with high accuracy.'
    },
    breast_cancer_screening: {
        test1_name: 'Mammography',
        test1_sens: 0.85,
        test1_spec: 0.90,
        test2_name: 'Tissue Biopsy',
        test2_sens: 0.98,
        test2_spec: 0.99,
        prevalence: 0.06,
        strategy: 'serial_positive',
        guidance: 'Breast cancer screening with mammography followed by tissue biopsy for confirmation. Mammography screens efficiently, biopsy provides definitive diagnosis.'
    },
    mi_emergency_parallel: {
        test1_name: 'Troponin',
        test1_sens: 0.90,
        test1_spec: 0.95,
        test2_name: 'ECG',
        test2_sens: 0.70,
        test2_spec: 0.90,
        prevalence: 0.20,
        strategy: 'parallel',
        guidance: 'Emergency MI diagnosis using parallel troponin and ECG. Both tests run simultaneously to maximize sensitivity and speed diagnosis.'
    },
    tb_screening_confirmation: {
        test1_name: 'Chest X-ray',
        test1_sens: 0.75,
        test1_spec: 0.80,
        test2_name: 'Sputum Culture',
        test2_sens: 0.85,
        test2_spec: 0.98,
        prevalence: 0.12,
        strategy: 'serial_positive',
        guidance: 'TB screening with chest X-ray followed by sputum culture confirmation. X-ray screens efficiently, culture confirms with high specificity.'
    },
    prostate_screening_exclusion: {
        test1_name: 'PSA Test',
        test1_sens: 0.80,
        test1_spec: 0.70,
        test2_name: 'MRI',
        test2_sens: 0.90,
        test2_spec: 0.85,
        prevalence: 0.15,
        strategy: 'serial_negative',
        guidance: 'Prostate cancer screening with PSA followed by MRI for PSA negatives. Ensures high sensitivity by testing PSA negatives with additional imaging.'
    },
    hiv_screening_confirmation: {
        test1_name: 'ELISA',
        test1_sens: 0.98,
        test1_spec: 0.95,
        test2_name: 'Western Blot',
        test2_sens: 0.99,
        test2_spec: 0.99,
        prevalence: 0.02,
        strategy: 'serial_positive',
        guidance: 'HIV screening with ELISA followed by Western Blot confirmation. ELISA screens with high sensitivity, Western Blot confirms with excellent specificity.'
    },
    stroke_emergency_parallel: {
        test1_name: 'Clinical Assessment',
        test1_sens: 0.85,
        test1_spec: 0.75,
        test2_name: 'CT Scan',
        test2_sens: 0.95,
        test2_spec: 0.98,
        prevalence: 0.25,
        strategy: 'parallel',
        guidance: 'Emergency stroke diagnosis using parallel clinical assessment and CT scan. Both performed simultaneously for rapid, comprehensive evaluation.'
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

const formatProbability = (value) => {
    if (!Number.isFinite(value))
        return 'n/a';

    return `${(value * 100).toFixed(1)}%`;
};


const calculateSequentialMetrics = (ui) => {
    const sens1 = getNumericValue(ui.test1_sens, 0.80);
    const spec1 = getNumericValue(ui.test1_spec, 0.90);
    const sens2 = getNumericValue(ui.test2_sens, 0.75);
    const spec2 = getNumericValue(ui.test2_spec, 0.95);
    const prevalence = getNumericValue(ui.prevalence, 0.10);
    const strategy = ui.strategy && typeof ui.strategy.value === 'function' ? ui.strategy.value() : 'serial_positive';

    let combinedSens, combinedSpec;

    if (strategy === 'serial_positive') {
        combinedSens = sens1 * sens2;
        combinedSpec = spec1 + (1 - spec1) * spec2;
    } else if (strategy === 'serial_negative') {
        combinedSens = sens1 + (1 - sens1) * sens2;
        combinedSpec = spec1 * spec2;
    } else if (strategy === 'parallel') {
        combinedSens = sens1 + sens2 - (sens1 * sens2);
        combinedSpec = spec1 * spec2;
    } else {
        return { ppv: NaN, npv: NaN };
    }

    const preTestOdds = prevalence / (1 - prevalence);
    const plr = combinedSens / (1 - combinedSpec);
    const nlr = (1 - combinedSens) / combinedSpec;
    const postTestOddsPos = preTestOdds * plr;
    const postTestOddsNeg = preTestOdds * nlr;
    const ppv = postTestOddsPos / (1 + postTestOddsPos);
    const npv = 1 - (postTestOddsNeg / (1 + postTestOddsNeg));

    return { ppv, npv, combinedSens, combinedSpec };
};

const events = {
    onChange_preset(ui) {
        const presetControl = ui.preset;

        if (!presetControl || typeof presetControl.value !== 'function')
            return;

        const preset = presetControl.value();

        console.log('Sequential testing preset changed to:', preset);

        if (preset === 'custom') {
            updateClinicalGuidance(ui);
            return;
        }

        applySequentialPresetConfig(ui, preset);
        updateClinicalGuidance(ui);
    },

    onChange_test1_name(ui) {
        updateClinicalGuidance(ui);
    },

    onChange_test2_name(ui) {
        updateClinicalGuidance(ui);
    },

    onChange_test1_sens(ui) {
        const control = ui.test1_sens;
        if (!control) return;

        const value = getNumericValue(control, NaN);
        const adjusted = clampNumeric(value, 0.01, 0.99);

        if (Number.isFinite(adjusted) && adjusted !== value)
            setControlValue(control, adjusted);

        updateClinicalGuidance(ui);
    },

    onChange_test1_spec(ui) {
        const control = ui.test1_spec;
        if (!control) return;

        const value = getNumericValue(control, NaN);
        const adjusted = clampNumeric(value, 0.01, 0.99);

        if (Number.isFinite(adjusted) && adjusted !== value)
            setControlValue(control, adjusted);

        updateClinicalGuidance(ui);
    },

    onChange_test2_sens(ui) {
        const control = ui.test2_sens;
        if (!control) return;

        const value = getNumericValue(control, NaN);
        const adjusted = clampNumeric(value, 0.01, 0.99);

        if (Number.isFinite(adjusted) && adjusted !== value)
            setControlValue(control, adjusted);

        updateClinicalGuidance(ui);
    },

    onChange_test2_spec(ui) {
        const control = ui.test2_spec;
        if (!control) return;

        const value = getNumericValue(control, NaN);
        const adjusted = clampNumeric(value, 0.01, 0.99);

        if (Number.isFinite(adjusted) && adjusted !== value)
            setControlValue(control, adjusted);

        updateClinicalGuidance(ui);
    },

    onChange_strategy(ui) {
        updateClinicalGuidance(ui);
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

        updateClinicalGuidance(ui);
    }
};

module.exports = events;