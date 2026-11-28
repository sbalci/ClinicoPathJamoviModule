// library-audit 2026-09-16 jjstatsplot [LOW] DONE: camelCase option names (testValue, confLevel, bfMessage)
// are addressable directly via standard ui.<name> properties without bracket workarounds.
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
                ui.testValue.setValue(0);
                ui.confLevel.setValue(0.95);
                ui.bfMessage.setValue(true);
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
                ui.testValue.setValue(0);
                ui.confLevel.setValue(0.95);
                ui.bfMessage.setValue(false);
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
                ui.testValue.setValue(0);
                ui.confLevel.setValue(0.95);
                ui.bfMessage.setValue(false);
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
                ui.testValue.setValue(0);
                ui.confLevel.setValue(0.95);
                ui.bfMessage.setValue(false);
                ui.changebinwidth.setValue(true);
                ui.binwidth.setValue(1.0);  // Unit bins for discrete scores
                ui.binfill.setValue("#DDA0DD");  // Plum for pathology
                ui.bincolor.setValue("#8B008B");  // Dark magenta border
                ui.binalpha.setValue(0.8);
                ui.centralitylinecolor.setValue("#9932CC");  // Dark orchid
                break;
        }
    },

    // Statistical type change with automatic parameter adjustment
    onChange_typestatistics: function(ui) {
        let statType = ui.typestatistics.value();
        
        switch(statType) {
            case "parametric":
                ui.centralitytype.setValue("parametric");
                break;
            case "nonparametric":
                ui.centralitytype.setValue("nonparametric");
                ui.bfMessage.setValue(false);
                break;
            case "robust":
                ui.centralitytype.setValue("robust");
                ui.bfMessage.setValue(false);
                break;
            case "bayes":
                ui.centralitytype.setValue("bayes");
                ui.bfMessage.setValue(true);
                break;
        }
    },

    // Bin width validation with clinical recommendations
    onChange_binwidth: function(ui) {
        // Only the clamp survives. The guidance strings this used to compute were written
        // to `binwidth_guidance`, a control that exists in neither .u.yaml nor .a.yaml, so
        // the setValue always threw into an empty catch and the user never saw any of it.
        if (ui.binwidth.value() <= 0)
            ui.binwidth.setValue(0.1);
    },

    // Digits validation for appropriate precision
    onChange_digits: function(ui) {
        let digits = ui.digits.value();
        
        if (digits < 0) {
            ui.digits.setValue(0);
        } else if (digits > 5) {
            ui.digits.setValue(5);
        }
        
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
        
        // Auto-suggest complementary centrality line color. `events.`, not `this.`: jamovi
        // runs a handler with `this` bound to its View, which has no getComplementaryColor.
        let lineColor = events.getComplementaryColor(fillColor);
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
    }
};

module.exports = events;
