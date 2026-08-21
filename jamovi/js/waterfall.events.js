const events = {
    // Input type change with smart defaults
    onChange_inputType: function(ui) {
        let inputType = ui.inputType.value();

        if (inputType === "raw") {
            // Raw measurements - ensure time variable guidance is clear
            // Spider plot becomes more meaningful with raw data
            ui.showSpiderPlot.setValue(true);
        } else {
            // Percentage data - waterfall plot is primary focus
            ui.showWaterfallPlot.setValue(true);
        }

        this.applyInputTypeDefaults(ui, inputType);
    },

    // Color scheme intelligence when changing color-by option
    onChange_colorBy: function(ui) {
        let colorBy = ui.colorBy.value();

        if (colorBy === "group") {
            ui.colorScheme.setValue("colorful");
            ui.spiderColorBy.setValue("group");
            ui.spiderColorScheme.setValue("colorful");
        } else if (colorBy === "recist") {
            ui.colorScheme.setValue("recist");
            ui.spiderColorBy.setValue("response");
            ui.spiderColorScheme.setValue("classic");
        }
    },

    // Group variable selection with intelligent coloring
    onChange_groupVar: function(ui) {
        let groupVar = ui.groupVar.value();

        // Auto-enable group-based coloring when group variable is selected
        if (groupVar) {
            ui.colorBy.setValue("group");
            ui.colorScheme.setValue("colorful");
            ui.spiderColorBy.setValue("group");
            ui.spiderColorScheme.setValue("colorful");
        }
    },

    // Time variable with spider plot optimization
    onChange_timeVar: function(ui) {
        let timeVar = ui.timeVar.value();

        if (timeVar) {
            // Enable spider plot when time variable is available
            ui.showSpiderPlot.setValue(true);
        }
    },

    // Validation for minimum response threshold
    onChange_minResponseForLabel: function(ui) {
        let threshold = ui.minResponseForLabel.value();

        if (threshold < 0) {
            ui.minResponseForLabel.setValue(0);
        } else if (threshold > 100) {
            ui.minResponseForLabel.setValue(100);
        }
    },

    // Bar transparency validation
    onChange_barAlpha: function(ui) {
        let alpha = ui.barAlpha.value();

        if (alpha < 0) {
            ui.barAlpha.setValue(0);
        } else if (alpha > 1) {
            ui.barAlpha.setValue(1);
        }
    },

    // Bar width validation
    onChange_barWidth: function(ui) {
        let width = ui.barWidth.value();

        if (width < 0.1) {
            ui.barWidth.setValue(0.1);
        } else if (width > 1.0) {
            ui.barWidth.setValue(1.0);
        }
    },

    // Helper functions
    applyInputTypeDefaults: function(ui, inputType) {
        // Apply smart defaults based on the input type
        if (inputType === "raw") {
            ui.showThresholds.setValue(true);
            ui.showSpiderPlot.setValue(true);
        } else {
            ui.showWaterfallPlot.setValue(true);
            ui.showThresholds.setValue(true);
        }
    }
};

module.exports = events;