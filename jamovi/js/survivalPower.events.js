'use strict';

const events = {

    // Event handler for clinical preset selection
    // Note: jamovi converts snake_case option names to camelCase for event handler names
    onChange_clinicalPreset: function(ui) {
        // Get the clinical preset value
        var preset = ui.clinical_preset.value();

        if (preset === "oncology_phase3") {
            // Phase III Oncology Trial preset
            ui.effect_size.setValue(0.75);           // 25% risk reduction
            ui.alpha_level.setValue(0.05);           // Standard significance
            ui.power_level.setValue(0.80);           // 80% power
            ui.allocation_ratio.setValue(1.0);       // 1:1 randomization
            ui.control_median_survival.setValue(12.0); // 12 months median
            ui.accrual_period.setValue(24.0);        // 2 years accrual
            ui.follow_up_period.setValue(12.0);      // 1 year follow-up
            ui.test_type.setValue("log_rank");       // Log-rank test
            ui.primary_endpoint.setValue("overall_survival");
            ui.dropout_rate.setValue(0.05);          // 5% annual dropout

        } else if (preset === "cardio_prevention") {
            // Cardiovascular Prevention Study preset
            ui.effect_size.setValue(0.85);           // 15% risk reduction
            ui.alpha_level.setValue(0.05);           // Standard significance
            ui.power_level.setValue(0.90);           // 90% power (higher for prevention)
            ui.allocation_ratio.setValue(1.0);       // 1:1 randomization
            ui.control_median_survival.setValue(60.0); // 5 years median (prevention)
            ui.accrual_period.setValue(36.0);        // 3 years accrual
            ui.follow_up_period.setValue(24.0);      // 2 years follow-up
            ui.test_type.setValue("log_rank");       // Log-rank test
            ui.primary_endpoint.setValue("overall_survival");
            ui.dropout_rate.setValue(0.03);          // 3% annual dropout (better compliance)

        } else if (preset === "biomarker_study") {
            // Biomarker/Companion Diagnostic Study preset
            ui.effect_size.setValue(0.67);           // 33% risk reduction
            ui.alpha_level.setValue(0.05);           // Standard significance
            ui.power_level.setValue(0.80);           // 80% power
            ui.allocation_ratio.setValue(2.0);       // 2:1 favoring biomarker+ group
            ui.control_median_survival.setValue(8.0); // 8 months median
            ui.accrual_period.setValue(18.0);        // 1.5 years accrual
            ui.follow_up_period.setValue(12.0);      // 1 year follow-up
            ui.test_type.setValue("cox_regression");  // Cox regression for biomarker
            ui.primary_endpoint.setValue("progression_free_survival");
            ui.dropout_rate.setValue(0.10);          // 10% annual dropout

        } else if (preset === "non_inferiority") {
            // Non-inferiority Trial preset
            ui.effect_size.setValue(1.0);            // Null hypothesis (no difference)
            ui.alpha_level.setValue(0.025);          // One-sided test
            ui.power_level.setValue(0.80);           // 80% power
            ui.allocation_ratio.setValue(1.0);       // 1:1 randomization
            ui.control_median_survival.setValue(15.0); // 15 months median
            ui.accrual_period.setValue(30.0);        // 2.5 years accrual
            ui.follow_up_period.setValue(18.0);      // 1.5 years follow-up
            ui.test_type.setValue("non_inferiority"); // Non-inferiority test
            ui.primary_endpoint.setValue("overall_survival");
            ui.ni_margin.setValue(1.25);             // 25% margin
            ui.dropout_rate.setValue(0.05);          // 5% annual dropout

        } else if (preset === "pilot_study") {
            // Pilot/Feasibility Study preset
            ui.effect_size.setValue(0.70);           // Moderate effect
            ui.alpha_level.setValue(0.10);           // Relaxed significance
            ui.power_level.setValue(0.70);           // Lower power acceptable
            ui.allocation_ratio.setValue(1.0);       // 1:1 randomization
            ui.control_median_survival.setValue(10.0); // 10 months median
            ui.accrual_period.setValue(12.0);        // 1 year accrual
            ui.follow_up_period.setValue(6.0);       // 6 months follow-up
            ui.test_type.setValue("log_rank");       // Log-rank test
            ui.primary_endpoint.setValue("progression_free_survival");
            ui.dropout_rate.setValue(0.15);          // 15% annual dropout (pilot)
        }

        // Preset applied - values have been updated
        // Note: Don't reset preset selection to avoid recursive onChange calls

        // Preset applied - UI will automatically recalculate
    }
};
module.exports = events;
