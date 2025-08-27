const events = {
    // Clinical preset system with intelligent parameter configuration for histograms
    onChange_clinicalPreset: function(ui) {
        let preset = ui.clinicalPreset.value();
        
        switch(preset) {
            case "lab_values":
                // Optimal settings for lab values analysis
                ui.typestatistics.setValue("parametric");
                ui.centralityline.setValue(true);
                ui.centralitytype.setValue("parametric");
                ui.resultssubtitle.setValue(true);
                ui.showInterpretation.setValue(true);
                ui.test_value.setValue(0);
                ui.conf_level.setValue(0.95);
                ui.bf_message.setValue(true);
                ui.binfill.setValue("#87CEEB");  // Light blue for lab values
                ui.bincolor.setValue("#4682B4");  // Steel blue border
                ui.binalpha.setValue(0.7);
                ui.centralitylinecolor.setValue("#1E90FF");  // Dodger blue
                break;
                
            case "biomarkers":
                // Settings for biomarker distribution analysis
                ui.typestatistics.setValue("nonparametric");
                ui.centralityline.setValue(true);
                ui.centralitytype.setValue("nonparametric");
                ui.resultssubtitle.setValue(true);
                ui.showInterpretation.setValue(true);
                ui.test_value.setValue(0);
                ui.conf_level.setValue(0.95);
                ui.bf_message.setValue(false);
                ui.binfill.setValue("#98FB98");  // Pale green for biomarkers
                ui.bincolor.setValue("#228B22");  // Forest green border
                ui.binalpha.setValue(0.75);
                ui.centralitylinecolor.setValue("#32CD32");  // Lime green
                break;
                
            case "patient_chars":
                // Settings for patient characteristics (age, BMI, etc.)
                ui.typestatistics.setValue("parametric");
                ui.centralityline.setValue(true);
                ui.centralitytype.setValue("parametric");
                ui.resultssubtitle.setValue(true);
                ui.showInterpretation.setValue(false);  // Less interpretation needed for basic demographics
                ui.test_value.setValue(0);
                ui.conf_level.setValue(0.95);
                ui.bf_message.setValue(false);
                ui.binfill.setValue("#FFB6C1");  // Light pink for demographics
                ui.bincolor.setValue("#DC143C");  // Crimson border
                ui.binalpha.setValue(0.6);
                ui.centralitylinecolor.setValue("#FF69B4");  // Hot pink
                break;
                
            case "pathology_scores":
                // Settings for pathological scores (ordinal data)
                ui.typestatistics.setValue("nonparametric");
                ui.centralityline.setValue(true);
                ui.centralitytype.setValue("nonparametric");
                ui.resultssubtitle.setValue(true);
                ui.showInterpretation.setValue(true);
                ui.test_value.setValue(0);
                ui.conf_level.setValue(0.95);
                ui.bf_message.setValue(false);
                ui.changebinwidth.setValue(true);
                ui.binwidth.setValue(1.0);  // Unit bins for discrete scores
                ui.binfill.setValue("#DDA0DD");  // Plum for pathology
                ui.bincolor.setValue("#8B008B");  // Dark magenta border
                ui.binalpha.setValue(0.8);
                ui.centralitylinecolor.setValue("#9932CC");  // Dark orchid
                break;
        }
        
        // Update guidance based on preset selection
        this.updatePresetGuidance(ui, preset);
    },

    // Statistical type change with automatic parameter adjustment
    onChange_typestatistics: function(ui) {
        let statType = ui.typestatistics.value();
        
        switch(statType) {
            case "parametric":
                ui.centralitytype.setValue("parametric");
                ui.bf_message.setValue(true);
                break;
            case "nonparametric":
                ui.centralitytype.setValue("nonparametric");
                ui.bf_message.setValue(false);
                break;
            case "robust":
                ui.centralitytype.setValue("robust");
                ui.bf_message.setValue(false);
                break;
            case "bayes":
                ui.centralitytype.setValue("bayes");
                ui.bf_message.setValue(true);
                // Show performance warning for Bayesian analysis
                this.showBayesianPerformanceWarning(ui);
                break;
        }
        
        this.updateStatisticalGuidance(ui, statType);
    },

    // Bin width validation with clinical recommendations
    onChange_binwidth: function(ui) {
        let binwidth = ui.binwidth.value();
        let guidance = "";
        
        if (binwidth <= 0) {
            ui.binwidth.setValue(0.1);
            binwidth = 0.1;
            guidance = "⚠️ Bin width must be positive";
        } else if (binwidth < 0.1) {
            guidance = "⚠️ Very small bins - may create noisy histogram";
        } else if (binwidth > 10) {
            guidance = "⚠️ Large bins may hide important distribution features";
        } else {
            guidance = "✅ Appropriate bin width for data visualization";
        }
        
        try {
            ui.binwidth_guidance.setValue(guidance);
        } catch(e) {
            // Fallback if dynamic guidance not supported
        }
    },

    // Confidence level validation
    onChange_conf_level: function(ui) {
        let conf = ui.conf_level.value();
        
        if (conf < 0.8) {
            ui.conf_level.setValue(0.8);
        } else if (conf > 0.99) {
            ui.conf_level.setValue(0.99);
        }
        
        // Standard confidence levels
        if (conf === 0.95) {
            // Most common choice
        } else if (conf === 0.99) {
            // Conservative choice
        } else if (conf === 0.90) {
            // Liberal choice
        }
    },

    // Test value validation for clinical relevance
    onChange_test_value: function(ui) {
        let testValue = ui.test_value.value();
        let guidance = "";
        
        if (testValue === 0) {
            guidance = "ℹ️ Testing against zero (no effect)";
        } else {
            guidance = `ℹ️ Testing against clinically meaningful value: ${testValue}`;
        }
        
        try {
            ui.test_value_guidance.setValue(guidance);
        } catch(e) {}
    },

    // Digits validation for appropriate precision
    onChange_digits: function(ui) {
        let digits = ui.digits.value();
        
        if (digits < 0) {
            ui.digits.setValue(0);
        } else if (digits > 5) {
            ui.digits.setValue(5);
        }
        
        let guidance = "";
        if (digits <= 1) {
            guidance = "ℹ️ Low precision - appropriate for large values";
        } else if (digits >= 4) {
            guidance = "ℹ️ High precision - appropriate for small values";
        } else {
            guidance = "✅ Standard precision for most clinical data";
        }
        
        try {
            ui.digits_guidance.setValue(guidance);
        } catch(e) {}
    },

    // Plot dimensions validation
    onChange_plotwidth: function(ui) {
        let width = ui.plotwidth.value();
        
        if (width < 300) {
            ui.plotwidth.setValue(300);
        } else if (width > 1200) {
            ui.plotwidth.setValue(1200);
        }
    },

    onChange_plotheight: function(ui) {
        let height = ui.plotheight.value();
        
        if (height < 300) {
            ui.plotheight.setValue(300);
        } else if (height > 800) {
            ui.plotheight.setValue(800);
        }
    },

    // Centrality line color coordination with bin colors
    onChange_binfill: function(ui) {
        let fillColor = ui.binfill.value();
        
        // Auto-suggest complementary centrality line color
        let lineColor = this.getComplementaryColor(fillColor);
        if (lineColor) {
            ui.centralitylinecolor.setValue(lineColor);
        }
    },

    // Alpha validation for visibility
    onChange_binalpha: function(ui) {
        let alpha = ui.binalpha.value();
        
        if (alpha < 0) {
            ui.binalpha.setValue(0);
        } else if (alpha > 1) {
            ui.binalpha.setValue(1);
        }
        
        let guidance = "";
        if (alpha < 0.3) {
            guidance = "⚠️ Very transparent - may be hard to see";
        } else if (alpha > 0.9) {
            guidance = "ℹ️ Nearly opaque - may hide overlapping features";
        } else {
            guidance = "✅ Good transparency for visualization";
        }
        
        try {
            ui.alpha_guidance.setValue(guidance);
        } catch(e) {}
    },

    // Centrality line width validation
    onChange_centralitylinewidth: function(ui) {
        let width = ui.centralitylinewidth.value();
        
        if (width < 0.1) {
            ui.centralitylinewidth.setValue(0.1);
        } else if (width > 5) {
            ui.centralitylinewidth.setValue(5);
        }
    },

    // Helper functions
    updatePresetGuidance: function(ui, preset) {
        let guidance = "";
        
        switch(preset) {
            case "lab_values":
                guidance = "✅ Configured for laboratory values: Parametric tests, normal distribution assumptions";
                break;
            case "biomarkers":
                guidance = "✅ Configured for biomarkers: Nonparametric tests, robust to outliers";
                break;
            case "patient_chars":
                guidance = "✅ Configured for demographics: Standard parametric approach";
                break;
            case "pathology_scores":
                guidance = "✅ Configured for ordinal scores: Nonparametric tests, unit bins";
                break;
            case "custom":
                guidance = "ℹ️ Custom configuration - adjust parameters manually";
                break;
        }
        
        try {
            ui.preset_guidance.setValue(guidance);
        } catch(e) {}
    },

    updateStatisticalGuidance: function(ui, statType) {
        let guidance = "";
        
        switch(statType) {
            case "parametric":
                guidance = "ℹ️ Assumes normal distribution. Best for: lab values, measurements";
                break;
            case "nonparametric":
                guidance = "ℹ️ No distribution assumptions. Best for: scores, skewed data";
                break;
            case "robust":
                guidance = "ℹ️ Resistant to outliers. Best for: data with extreme values";
                break;
            case "bayes":
                guidance = "⚠️ Bayesian inference with uncertainty quantification. <strong>WARNING: Very slow computation!</strong>";
                break;
        }
        
        try {
            ui.statistical_guidance.setValue(guidance);
        } catch(e) {}
    },

    getComplementaryColor: function(color) {
        // Simple color mapping for better visualization
        const colorMap = {
            "#87CEEB": "#1E90FF",  // Light blue -> Dodger blue
            "#98FB98": "#32CD32",  // Pale green -> Lime green
            "#FFB6C1": "#FF69B4",  // Light pink -> Hot pink
            "#DDA0DD": "#9932CC",  // Plum -> Dark orchid
            "skyblue": "#1E90FF",
            "lightgreen": "#32CD32",
            "lightpink": "#FF69B4",
            "plum": "#9932CC"
        };
        
        return colorMap[color] || null;
    },

    showBayesianPerformanceWarning: function(ui) {
        let warning = "⚠️ <strong>PERFORMANCE WARNING:</strong><br>" +
                     "Bayesian analysis can take 30-60 seconds or more depending on data size.<br>" +
                     "Consider using 'parametric' or 'nonparametric' for faster results.<br><br>" +
                     "💡 <strong>Speed Tips:</strong><br>" +
                     "• Use smaller datasets (&lt;1000 rows) when possible<br>" +
                     "• Consider sampling your data first<br>" +
                     "• Parametric tests often provide similar insights with instant results";
        
        try {
            // Try to show warning in a guidance field
            ui.bayesian_warning.setValue(warning);
        } catch(e) {
            // Fallback - could show in console or other UI element
            console.warn("Bayesian analysis selected - expect slow performance");
        }
    }
};

module.exports = events;