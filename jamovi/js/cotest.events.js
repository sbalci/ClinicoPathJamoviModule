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

const updateGuidanceField = (ui, fieldName, message, logPrefix) => {
    if (message === undefined || message === null)
        return;

    const guidanceControl = ui[fieldName];

    if (guidanceControl && typeof guidanceControl.setValue === 'function')
        guidanceControl.setValue(message);
    else if (logPrefix)
        console.log(logPrefix, message);
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

    updateGuidanceField(ui, 'preset_guidance', config.guidance, 'Preset guidance:');
};

const formatProbability = (value) => {
    if (!Number.isFinite(value))
        return 'n/a';

    return `${(value * 100).toFixed(1)}%`;
};

const validateSensitivity = (ui, paramName, testName) => {
    const control = ui[paramName];

    if (!control)
        return;

    const value = getNumericValue(control, NaN);
    const adjusted = clampNumeric(value, 0.01, 0.99);

    if (Number.isFinite(adjusted) && adjusted !== value)
        setControlValue(control, adjusted);

    let feedback = '';

    if (!Number.isFinite(adjusted))
        feedback = `⚠️ ${testName} sensitivity is not a number`;
    else if (adjusted < 0.60)
        feedback = `⚠️ ${testName} has low sensitivity - many cases will be missed`;
    else if (adjusted < 0.80)
        feedback = `ℹ️ ${testName} has moderate sensitivity - acceptable for screening`;
    else if (adjusted < 0.95)
        feedback = `✅ ${testName} has good sensitivity - suitable for screening`;
    else
        feedback = `✅ ${testName} has excellent sensitivity - ideal for screening`;

    updateGuidanceField(ui, `${paramName}_guidance`, feedback, `${testName} sensitivity guidance:`);
};

const validateSpecificity = (ui, paramName, testName) => {
    const control = ui[paramName];

    if (!control)
        return;

    const value = getNumericValue(control, NaN);
    const adjusted = clampNumeric(value, 0.01, 0.99);

    if (Number.isFinite(adjusted) && adjusted !== value)
        setControlValue(control, adjusted);

    let feedback = '';

    if (!Number.isFinite(adjusted))
        feedback = `⚠️ ${testName} specificity is not a number`;
    else if (adjusted < 0.70)
        feedback = `⚠️ ${testName} has low specificity - many false positives expected`;
    else if (adjusted < 0.90)
        feedback = `ℹ️ ${testName} has moderate specificity - acceptable for initial screening`;
    else if (adjusted < 0.98)
        feedback = `✅ ${testName} has good specificity - low false positive rate`;
    else
        feedback = `✅ ${testName} has excellent specificity - very low false positive rate`;

    updateGuidanceField(ui, `${paramName}_guidance`, feedback, `${testName} specificity guidance:`);
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
        validateSensitivity(ui, 'test1_sens', 'Test 1');
    },

    onChange_test1_spec(ui) {
        validateSpecificity(ui, 'test1_spec', 'Test 1');
    },

    onChange_test2_sens(ui) {
        validateSensitivity(ui, 'test2_sens', 'Test 2');
    },

    onChange_test2_spec(ui) {
        validateSpecificity(ui, 'test2_spec', 'Test 2');
    },

    onChange_prevalence(ui) {
        const control = ui.prevalence;

        if (!control)
            return;

        let prevalence = getNumericValue(control, NaN);
        let feedback = '';

        if (Number.isFinite(prevalence) && prevalence < 0.001) {
            prevalence = 0.001;
            setControlValue(control, prevalence);
            feedback = '⚠️ Minimum prevalence set to 0.1%';
        } else if (Number.isFinite(prevalence) && prevalence > 0.999) {
            prevalence = 0.999;
            setControlValue(control, prevalence);
            feedback = '⚠️ Maximum prevalence set to 99.9%';
        }

        if (!Number.isFinite(prevalence))
            return;

        if (feedback === '') {
            if (prevalence < 0.01)
                feedback = 'ℹ️ Very rare disease - co-testing may not be cost-effective';
            else if (prevalence < 0.05)
                feedback = 'ℹ️ Rare disease - consider screening criteria carefully';
            else if (prevalence < 0.20)
                feedback = '✅ Typical screening population prevalence';
            else if (prevalence < 0.50)
                feedback = 'ℹ️ High-risk population - consider diagnostic threshold';
            else
                feedback = '⚠️ Very high prevalence - may indicate diagnostic rather than screening setting';
        }

        const sens1 = getNumericValue(ui.test1_sens, 0.80);
        const spec1 = getNumericValue(ui.test1_spec, 0.90);
        const sens2 = getNumericValue(ui.test2_sens, 0.75);
        const spec2 = getNumericValue(ui.test2_spec, 0.95);

        const denomPLR = (1 - spec1) * (1 - spec2);
        const denomNLR = spec1 * spec2;

        const combinedPLR = denomPLR > 0 ? (sens1 * sens2) / denomPLR : Infinity;
        const combinedNLR = denomNLR > 0 ? ((1 - sens1) * (1 - sens2)) / denomNLR : Infinity;

        const preTestOdds = prevalence / (1 - prevalence);
        const postTestOddsPos = preTestOdds * combinedPLR;
        const postTestOddsNeg = preTestOdds * combinedNLR;
        const ppv = postTestOddsPos / (1 + postTestOddsPos);
        const npv = 1 - (postTestOddsNeg / (1 + postTestOddsNeg));

        feedback += ` | Est. PPV≈${formatProbability(ppv)}, NPV≈${formatProbability(npv)}`;

        updateGuidanceField(ui, 'prevalence_guidance', feedback, 'Prevalence guidance:');
    },

    onChange_indep(ui) {
        const control = ui.indep;

        if (!control || typeof control.value !== 'function')
            return;

        const isIndep = Boolean(control.value());
        const guidance = isIndep
            ? '✅ Tests assumed independent - simpler calculation, may underestimate joint performance'
            : 'ℹ️ Tests assumed dependent - more realistic but requires dependence parameters';

        updateGuidanceField(ui, 'indep_guidance', guidance, 'Independence guidance:');
    },

    onChange_cond_dep_pos(ui) {
        const control = ui.cond_dep_pos;

        if (!control)
            return;

        let dependence = getNumericValue(control, NaN);

        if (Number.isFinite(dependence)) {
            const clamped = clampNumeric(dependence, 0, 1);

            if (clamped !== dependence) {
                dependence = clamped;
                setControlValue(control, dependence);
            }
        }

        let guidance = '';

        if (!Number.isFinite(dependence))
            guidance = '⚠️ Dependence value is not a number';
        else if (dependence < 0.05)
            guidance = '✅ Weak dependence - tests nearly independent in diseased';
        else if (dependence < 0.15)
            guidance = 'ℹ️ Moderate dependence - tests somewhat correlated in diseased';
        else if (dependence < 0.30)
            guidance = '⚠️ Strong dependence - tests highly correlated in diseased';
        else
            guidance = '⚠️ Very strong dependence - tests almost measure same thing';

        updateGuidanceField(ui, 'cond_dep_pos_guidance', guidance, 'Positive dependence guidance:');
    },

    onChange_cond_dep_neg(ui) {
        const control = ui.cond_dep_neg;

        if (!control)
            return;

        let dependence = getNumericValue(control, NaN);

        if (Number.isFinite(dependence)) {
            const clamped = clampNumeric(dependence, 0, 1);

            if (clamped !== dependence) {
                dependence = clamped;
                setControlValue(control, dependence);
            }
        }

        let guidance = '';

        if (!Number.isFinite(dependence))
            guidance = '⚠️ Dependence value is not a number';
        else if (dependence < 0.05)
            guidance = '✅ Weak dependence - tests nearly independent in non-diseased';
        else if (dependence < 0.15)
            guidance = 'ℹ️ Moderate dependence - tests somewhat correlated in non-diseased';
        else if (dependence < 0.30)
            guidance = '⚠️ Strong dependence - tests highly correlated in non-diseased';
        else
            guidance = '⚠️ Very strong dependence - tests almost measure same thing';

        updateGuidanceField(ui, 'cond_dep_neg_guidance', guidance, 'Negative dependence guidance:');
    }
};

module.exports = events;
