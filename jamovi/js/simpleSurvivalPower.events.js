
const events = {

    onChange_clinicalPreset: function(ui) {
        // When clinical preset changes, update related parameters
        let preset = this.getOption('clinical_preset');

        // Map preset to appropriate default values
        const presets = {
            'phase3_oncology': {
                effect_size: 0.75,
                power_level: 0.80,
                alpha_level: 0.05,
                control_median_survival: 12,
                accrual_period: 24,
                follow_up_period: 12
            },
            'phase2_single_arm': {
                effect_size: 0.70,
                power_level: 0.80,
                alpha_level: 0.10,
                control_median_survival: 6,
                accrual_period: 12,
                follow_up_period: 6
            },
            'cardiovascular': {
                effect_size: 0.85,
                power_level: 0.90,
                alpha_level: 0.05,
                control_median_survival: 60,
                accrual_period: 36,
                follow_up_period: 24
            }
        };

        if (preset !== 'custom' && presets[preset]) {
            let values = presets[preset];
            for (let key in values) {
                this.setOption(key, values[key]);
            }
        }
    }

};

module.exports = events;
