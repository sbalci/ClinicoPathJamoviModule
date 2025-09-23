const events = {
    // Clinical preset system for waterfall analysis with intelligent parameter configuration
    onChange_clinicalPreset: function(ui) {
        let preset = ui.clinicalPreset.value();

        switch(preset) {
            case "phase2":
                // Phase II Efficacy Study: Focus on ORR/DCR with clear thresholds
                ui.showThresholds.setValue(true);
                ui.labelOutliers.setValue(true);
                ui.showConfidenceIntervals.setValue(true);
                ui.generateCopyReadyReport.setValue(true);
                ui.showClinicalSignificance.setValue(true);
                ui.colorScheme.setValue("recist");
                ui.colorBy.setValue("recist");
                ui.showMedian.setValue(false);
                ui.showCI.setValue(false);  // Use enhanced metrics instead
                ui.minResponseForLabel.setValue(40);  // Label significant responses
                ui.barAlpha.setValue(1.0);
                ui.barWidth.setValue(0.7);
                break;

            case "phase1_2":
                // Phase I/II Dose Escalation: Detailed annotations for safety and efficacy
                ui.showThresholds.setValue(true);
                ui.labelOutliers.setValue(true);
                ui.showMedian.setValue(true);
                ui.showCI.setValue(true);
                ui.showConfidenceIntervals.setValue(true);
                ui.generateCopyReadyReport.setValue(false);  // More exploratory
                ui.showClinicalSignificance.setValue(false);  // Too early for significance
                ui.colorScheme.setValue("jamovi");
                ui.colorBy.setValue("recist");
                ui.minResponseForLabel.setValue(30);  // Lower threshold for detailed analysis
                ui.barAlpha.setValue(0.8);  // Slightly transparent for overlays
                ui.barWidth.setValue(0.7);
                break;

            case "biomarker":
                // Biomarker Correlation Study: Group-based analysis with statistical comparisons
                ui.showThresholds.setValue(true);
                ui.labelOutliers.setValue(false);  // Groups more important than outliers
                ui.showMedian.setValue(true);
                ui.showCI.setValue(true);
                ui.showConfidenceIntervals.setValue(true);
                ui.generateCopyReadyReport.setValue(true);
                ui.showClinicalSignificance.setValue(true);
                ui.colorBy.setValue("group");  // Key: Color by patient groups
                ui.colorScheme.setValue("colorful");  // Better group distinction
                ui.spiderColorBy.setValue("group");  // Also apply to spider plots
                ui.spiderColorScheme.setValue("colorful");
                ui.minResponseForLabel.setValue(50);
                ui.barAlpha.setValue(0.9);
                ui.barWidth.setValue(0.8);
                break;

            case "publication":
                // Publication Ready: Clean, professional appearance for manuscripts
                ui.showThresholds.setValue(true);
                ui.labelOutliers.setValue(false);  // Clean appearance
                ui.showMedian.setValue(false);     // Minimize visual clutter
                ui.showCI.setValue(false);         // Use enhanced metrics instead
                ui.showConfidenceIntervals.setValue(true);  // Professional reporting
                ui.generateCopyReadyReport.setValue(true);  // Essential for publications
                ui.showClinicalSignificance.setValue(true);
                ui.colorScheme.setValue("recist");  // Standard clinical colors
                ui.colorBy.setValue("recist");
                ui.spiderColorScheme.setValue("classic");  // Professional spider colors
                ui.minResponseForLabel.setValue(60);  // Only very significant responses
                ui.barAlpha.setValue(1.0);         // Solid colors
                ui.barWidth.setValue(0.8);         // Better visibility
                break;

            case "custom":
                // Custom: No automatic changes - user maintains full control
                break;
        }

        // Update dependent options based on preset
        this.updatePresetGuidance(ui, preset);
    },

    // Input type change with smart defaults
    onChange_inputType: function(ui) {
        let inputType = ui.inputType.value();
        let preset = ui.clinicalPreset.value();

        if (inputType === "raw") {
            // Raw measurements - ensure time variable guidance is clear
            // Spider plot becomes more meaningful with raw data
            ui.showSpiderPlot.setValue(true);
        } else {
            // Percentage data - waterfall plot is primary focus
            ui.showWaterfallPlot.setValue(true);
        }

        // Don't override preset settings unless using custom
        if (preset === "custom") {
            this.applyInputTypeDefaults(ui, inputType);
        }
    },

    // Color scheme intelligence when changing color-by option
    onChange_colorBy: function(ui) {
        let colorBy = ui.colorBy.value();
        let preset = ui.clinicalPreset.value();

        // Only auto-adjust if using custom preset
        if (preset === "custom") {
            if (colorBy === "group") {
                ui.colorScheme.setValue("colorful");
                ui.spiderColorBy.setValue("group");
                ui.spiderColorScheme.setValue("colorful");
            } else if (colorBy === "recist") {
                ui.colorScheme.setValue("recist");
                ui.spiderColorBy.setValue("response");
                ui.spiderColorScheme.setValue("classic");
            }
        }
    },

    // Group variable selection with intelligent coloring
    onChange_groupVar: function(ui) {
        let groupVar = ui.groupVar.value();
        let preset = ui.clinicalPreset.value();

        // Auto-enable group-based coloring when group variable is selected
        if (groupVar && preset === "custom") {
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

    // Guided mode toggle with preset interaction
    onChange_enableGuidedMode: function(ui) {
        let guided = ui.enableGuidedMode.value();

        // When enabling guided mode, suggest appropriate preset for new users
        if (guided) {
            let preset = ui.clinicalPreset.value();
            if (preset === "custom") {
                // Suggest phase2 as good starting point for guided analysis
                ui.clinicalPreset.setValue("phase2");
            }
        }
    },

    // Helper functions
    updatePresetGuidance: function(ui, preset) {
        // Update any preset-specific guidance or validation
        // This could be expanded to show contextual help

        switch(preset) {
            case "phase2":
                // Could show Phase II specific guidance
                break;
            case "phase1_2":
                // Could show Phase I/II specific guidance
                break;
            case "biomarker":
                // Could show biomarker analysis guidance
                break;
            case "publication":
                // Could show publication guidelines
                break;
        }
    },

    applyInputTypeDefaults: function(ui, inputType) {
        // Apply smart defaults based on input type (only for custom preset)
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