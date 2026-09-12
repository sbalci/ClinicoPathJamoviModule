'use strict';

// survivalPower.events.js - worked-example designs for the Clinical Study Preset
// dropdown. Bound from jamovi/survivalPower.u.yaml
// (clinical_preset ComboBox -> events: change: ./survivalPower.events::onChange_clinicalPreset).
//
// The backend cannot stand in for this: self$options is read-only during .run(),
// which is why R/survivalPower.b.R's .apply_clinical_preset() is a stub that
// defers here.
//
// Each preset is a COMPLETE design: every field the calculation reads is set, so
// the numbers on screen always describe the study the results table reports. A
// partial preset leaves whatever the user last typed in the untouched boxes and
// silently blends two designs -- effect_size_type is the sharpest example, since
// leaving it on "Median Survival Ratio" reinterprets an HR of 0.75 as an HR of
// 1.33 (see .get_effect_hr in R/survivalPower.b.R).
//
// The values are round ILLUSTRATIVE figures for demonstrating the calculation.
// None is a pooled literature estimate and none should be used to size a real
// trial without substituting your own assumptions.

// Written so a control that does not exist (renamed or removed from the .a.yaml)
// is skipped rather than throwing and abandoning the rest of the preset.
const setControlValue = (control, value) => {
    if (!control || typeof control.setValue !== 'function')
        return;
    if (value === undefined || value === null)
        return;
    control.setValue(value);
};

// Keys must be option names from jamovi/survivalPower.a.yaml and values must sit
// inside the min/max (or List levels) declared there; both are checked by
// tests/testthat/test-survivalPower-events.R, which also runs every preset
// through the analysis to confirm none of them trips a validation error.
//
// Note on allocation_ratio: the option is Control:Experimental (see
// .allocation_props in R/survivalPower.b.R), so 2.0 means two controls per
// experimental subject.
const PRESET_CONFIGS = {

    // The .a.yaml defaults. Selecting "Custom Design" restores these rather than
    // leaving a worked example's numbers in boxes now labelled "custom", where
    // they would read as figures the user chose.
    custom: {
        analysis_type: 'sample_size',
        survival_distribution: 'exponential',
        accrual_pattern: 'uniform',
        interim_analyses: 0,
        alpha_spending: 'none',
        test_type: 'log_rank',
        study_design: 'two_arm_parallel',
        primary_endpoint: 'overall_survival',
        effect_size_type: 'hazard_ratio',
        effect_size: 0.75,
        alpha_level: 0.05,
        power_level: 0.80,
        allocation_ratio: 1.0,
        control_median_survival: 12.0,
        accrual_period: 24.0,
        follow_up_period: 12.0,
        dropout_rate: 0.05,
        ni_margin: 1.25,
        ni_type: 'relative_margin'
    },

    // Phase III oncology trial: 25% risk reduction, 1:1 randomisation,
    // 2 years accrual and 1 year follow-up.
    oncology_phase3: {
        analysis_type: 'sample_size',
        survival_distribution: 'exponential',
        accrual_pattern: 'uniform',
        interim_analyses: 0,
        alpha_spending: 'none',
        test_type: 'log_rank',
        study_design: 'two_arm_parallel',
        primary_endpoint: 'overall_survival',
        effect_size_type: 'hazard_ratio',
        effect_size: 0.75,
        alpha_level: 0.05,
        power_level: 0.80,
        allocation_ratio: 1.0,
        control_median_survival: 12.0,
        accrual_period: 24.0,
        follow_up_period: 12.0,
        dropout_rate: 0.05
    },

    // Cardiovascular prevention: 15% risk reduction in a low-risk cohort, so
    // 5-year median survival, 90% power, and better compliance (3% dropout).
    cardio_prevention: {
        analysis_type: 'sample_size',
        survival_distribution: 'exponential',
        accrual_pattern: 'uniform',
        interim_analyses: 0,
        alpha_spending: 'none',
        test_type: 'log_rank',
        study_design: 'two_arm_parallel',
        primary_endpoint: 'overall_survival',
        effect_size_type: 'hazard_ratio',
        effect_size: 0.85,
        alpha_level: 0.05,
        power_level: 0.90,
        allocation_ratio: 1.0,
        control_median_survival: 60.0,
        accrual_period: 36.0,
        follow_up_period: 24.0,
        dropout_rate: 0.03
    },

    // Biomarker / companion diagnostic study: 33% risk reduction on
    // progression-free survival, randomised 2:1 control:experimental. The
    // unequal allocation costs efficiency, which the analysis reports as a notice.
    biomarker_study: {
        analysis_type: 'sample_size',
        survival_distribution: 'exponential',
        accrual_pattern: 'uniform',
        interim_analyses: 0,
        alpha_spending: 'none',
        test_type: 'cox_regression',
        study_design: 'two_arm_parallel',
        primary_endpoint: 'progression_free_survival',
        effect_size_type: 'hazard_ratio',
        effect_size: 0.67,
        alpha_level: 0.05,
        power_level: 0.80,
        allocation_ratio: 2.0,
        control_median_survival: 8.0,
        accrual_period: 18.0,
        follow_up_period: 12.0,
        dropout_rate: 0.10
    },

    // Non-inferiority against a 25% hazard-ratio margin, assuming true
    // equivalence. Alpha is 0.025 because the non-inferiority test is applied
    // one-sided. The assumed HR must stay strictly below the margin or no sample
    // size can demonstrate non-inferiority, which .validate_inputs() refuses
    // rather than answering.
    non_inferiority: {
        analysis_type: 'sample_size',
        survival_distribution: 'exponential',
        accrual_pattern: 'uniform',
        interim_analyses: 0,
        alpha_spending: 'none',
        test_type: 'non_inferiority',
        study_design: 'two_arm_parallel',
        primary_endpoint: 'overall_survival',
        effect_size_type: 'hazard_ratio',
        effect_size: 1.0,
        alpha_level: 0.025,
        power_level: 0.80,
        allocation_ratio: 1.0,
        control_median_survival: 15.0,
        accrual_period: 30.0,
        follow_up_period: 18.0,
        dropout_rate: 0.05,
        ni_margin: 1.25,
        ni_type: 'relative_margin'
    },

    // Pilot / feasibility study: a relaxed alpha and power deliberately trade
    // type I and type II error for a sample size a single centre can recruit,
    // with the higher dropout a pilot population tends to show.
    pilot_study: {
        analysis_type: 'sample_size',
        survival_distribution: 'exponential',
        accrual_pattern: 'uniform',
        interim_analyses: 0,
        alpha_spending: 'none',
        test_type: 'log_rank',
        study_design: 'two_arm_parallel',
        primary_endpoint: 'progression_free_survival',
        effect_size_type: 'hazard_ratio',
        effect_size: 0.70,
        alpha_level: 0.10,
        power_level: 0.70,
        allocation_ratio: 1.0,
        control_median_survival: 10.0,
        accrual_period: 12.0,
        follow_up_period: 6.0,
        dropout_rate: 0.15
    }
};

const events = {

    // jamovi converts snake_case option names to camelCase for handler names.
    onChange_clinicalPreset: function(ui) {
        const control = ui.clinical_preset;
        if (!control || typeof control.value !== 'function')
            return;

        const config = PRESET_CONFIGS[String(control.value())];
        if (config === undefined)
            return;

        for (const name of Object.keys(config))
            setControlValue(ui[name], config[name]);

        // Don't reset the preset selection here -- it would re-enter onChange.
    }

};

module.exports = events;
