// Controls whose name contains a dot (test.value, conf.level, bf.message) are keyed by
// their LITERAL name in jamovi's control registry (this._ctrlOptions[e.name], matched with
// an exact string compare), so they are only reachable with bracket notation. The previous
// `ui.test_value` / `ui.conf_level` / `ui.bf_message` read an undefined property and threw
// a TypeError, and because those calls were not wrapped in try/catch the handler aborted
// there - so selecting a clinical preset set the first five controls and silently skipped
// every colour below, and onChange_typestatistics never reached its guidance call.
// Look the control up defensively so one unresolved name can never abort a whole handler.
const ctrl = function(ui, name) {
    return Object.prototype.hasOwnProperty.call(ui, name) ? ui[name] : null;
};
const setOpt = function(ui, name, value) {
    const c = ctrl(ui, name);
    if (c) c.setValue(value);
};
const getOpt = function(ui, name, fallback) {
    const c = ctrl(ui, name);
    return c ? c.value() : fallback;
};

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
                setOpt(ui, 'test.value', 0);
                setOpt(ui, 'conf.level', 0.95);
                setOpt(ui, 'bf.message', true);
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
                setOpt(ui, 'test.value', 0);
                setOpt(ui, 'conf.level', 0.95);
                setOpt(ui, 'bf.message', false);
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
                setOpt(ui, 'test.value', 0);
                setOpt(ui, 'conf.level', 0.95);
                setOpt(ui, 'bf.message', false);
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
                setOpt(ui, 'test.value', 0);
                setOpt(ui, 'conf.level', 0.95);
                setOpt(ui, 'bf.message', false);
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
                setOpt(ui, 'bf.message', true);
                break;
            case "nonparametric":
                ui.centralitytype.setValue("nonparametric");
                setOpt(ui, 'bf.message', false);
                break;
            case "robust":
                ui.centralitytype.setValue("robust");
                setOpt(ui, 'bf.message', false);
                break;
            case "bayes":
                ui.centralitytype.setValue("bayes");
                setOpt(ui, 'bf.message', true);
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

    // Confidence level validation
    onChange_conf_level: function(ui) {
        let conf = getOpt(ui, 'conf.level', null);
        
        if (conf < 0.8) {
            setOpt(ui, 'conf.level', 0.8);
        } else if (conf > 0.99) {
            setOpt(ui, 'conf.level', 0.99);
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