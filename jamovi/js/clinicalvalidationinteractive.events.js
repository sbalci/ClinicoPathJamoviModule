const events = {
    // Clinical preset system with intelligent parameter configuration
    onChange_clinicalPreset: function(ui) {
        let preset = ui.clinical_preset.value();
        
        switch(preset) {
            case "diagnostic_biomarker":
                // Optimal settings for biomarker validation
                ui.clinical_context.setValue("diagnosis");
                ui.validation_method.setValue("bootstrap");
                ui.bootstrap_samples.setValue(2000);
                ui.cost_matrix.setValue("diagnosis");
                ui.fn_fp_cost_ratio.setValue(3.0);
                ui.min_sensitivity.setValue(0.85);
                ui.min_specificity.setValue(0.80);
                ui.min_ppv.setValue(0.75);
                ui.min_npv.setValue(0.90);
                ui.population_prevalence.setValue(15);
                ui.prevalence_adjustment.setValue(true);
                ui.confidence_level.setValue(0.95);
                break;
                
            case "cancer_screening":
                // Conservative screening parameters
                ui.clinical_context.setValue("screening");
                ui.validation_method.setValue("cross_validation");
                ui.cv_folds.setValue(10);
                ui.cv_repeats.setValue(5);
                ui.cost_matrix.setValue("screening");
                ui.fn_fp_cost_ratio.setValue(5.0);
                ui.min_sensitivity.setValue(0.90);
                ui.min_specificity.setValue(0.75);
                ui.min_ppv.setValue(0.20);
                ui.min_npv.setValue(0.95);
                ui.population_prevalence.setValue(5);
                ui.prevalence_adjustment.setValue(true);
                break;
                
            case "cardiovascular_risk":
                // Risk stratification settings
                ui.clinical_context.setValue("risk");
                ui.validation_method.setValue("repeated_cv");
                ui.cv_folds.setValue(5);
                ui.cv_repeats.setValue(10);
                ui.cost_matrix.setValue("equal");
                ui.fn_fp_cost_ratio.setValue(2.0);
                ui.min_sensitivity.setValue(0.75);
                ui.min_specificity.setValue(0.75);
                ui.population_prevalence.setValue(25);
                break;
                
            case "infectious_disease":
                // Infectious disease diagnosis
                ui.clinical_context.setValue("diagnosis");
                ui.validation_method.setValue("bootstrap");
                ui.bootstrap_samples.setValue(1500);
                ui.cost_matrix.setValue("screening");
                ui.fn_fp_cost_ratio.setValue(4.0);
                ui.min_sensitivity.setValue(0.95);
                ui.min_specificity.setValue(0.70);
                ui.population_prevalence.setValue(8);
                break;
                
            case "surgical_risk":
                // Surgical risk assessment
                ui.clinical_context.setValue("risk");
                ui.validation_method.setValue("cross_validation");
                ui.cv_folds.setValue(8);
                ui.cost_matrix.setValue("custom");
                ui.fn_fp_cost_ratio.setValue(3.5);
                ui.min_sensitivity.setValue(0.80);
                ui.min_specificity.setValue(0.85);
                break;
                
            case "treatment_response":
                // Treatment response prediction
                ui.clinical_context.setValue("treatment");
                ui.validation_method.setValue("bootstrap");
                ui.bootstrap_samples.setValue(1200);
                ui.cost_matrix.setValue("equal");
                ui.fn_fp_cost_ratio.setValue(1.5);
                ui.min_sensitivity.setValue(0.70);
                ui.min_specificity.setValue(0.70);
                break;
                
            case "histological_classification":
                // Pathological classification
                ui.clinical_context.setValue("histological");
                ui.validation_method.setValue("cross_validation");
                ui.cv_folds.setValue(10);
                ui.cost_matrix.setValue("diagnosis");
                ui.fn_fp_cost_ratio.setValue(2.5);
                ui.min_sensitivity.setValue(0.85);
                ui.min_specificity.setValue(0.90);
                break;
        }
        
        // Update validation parameters based on method
        this.updateValidationGuidance(ui);
        // Update clinical guidance
        this.updateClinicalGuidance(ui);
    },

    // Model type change with intelligent defaults
    onChange_modelType: function(ui) {
        let modelType = ui.model_type.value();
        
        switch(modelType) {
            case "logistic":
                ui.validation_method.setValue("bootstrap");
                ui.bootstrap_samples.setValue(1000);
                break;
            case "cox":
                ui.validation_method.setValue("cross_validation");
                ui.cv_folds.setValue(10);
                break;
            case "random_forest":
                ui.validation_method.setValue("cross_validation");
                ui.cv_folds.setValue(5);
                ui.cv_repeats.setValue(3);
                break;
            case "svm":
                ui.validation_method.setValue("repeated_cv");
                ui.cv_folds.setValue(5);
                ui.cv_repeats.setValue(5);
                break;
            case "lda":
                ui.validation_method.setValue("bootstrap");
                ui.bootstrap_samples.setValue(800);
                break;
        }
        
        this.updateValidationGuidance(ui);
    },

    // Validation method change with parameter optimization
    onChange_validationMethod: function(ui) {
        let method = ui.validation_method.value();
        
        // Auto-adjust parameters based on method
        switch(method) {
            case "bootstrap":
                ui.bootstrap_samples.setValue(1000);
                break;
            case "cross_validation":
                ui.cv_folds.setValue(10);
                ui.cv_repeats.setValue(1);
                break;
            case "repeated_cv":
                ui.cv_folds.setValue(5);
                ui.cv_repeats.setValue(5);
                break;
            case "holdout":
                ui.holdout_proportion.setValue(0.25);
                break;
            case "time_split":
                ui.holdout_proportion.setValue(0.30);
                break;
        }
        
        this.updateValidationGuidance(ui);
    },

    // Bootstrap samples validation with performance warnings
    onChange_bootstrapSamples: function(ui) {
        let samples = ui.bootstrap_samples.value();
        let guidance = "";
        
        if (samples < 500) {
            guidance = "⚠️ Too few samples - may be unreliable";
            ui.bootstrap_samples.setValue(500);
        } else if (samples < 1000) {
            guidance = "⚠️ Consider increasing to ≥1000 for stable estimates";
        } else if (samples > 3000) {
            guidance = "ℹ️ High computational cost - consider reducing";
        } else {
            guidance = "✅ Good balance of accuracy and efficiency";
        }
        
        // Update guidance label (if UI supports dynamic labels)
        try {
            ui.bootstrap_guidance.setValue(guidance);
        } catch(e) {
            // Fallback if dynamic labels not supported
        }
    },

    // Cross-validation folds with sample size validation
    onChange_cvFolds: function(ui) {
        let folds = ui.cv_folds.value();
        let guidance = "";
        
        if (folds < 5) {
            guidance = "⚠️ Too few folds - high bias";
            ui.cv_folds.setValue(5);
        } else if (folds > 15) {
            guidance = "⚠️ Too many folds - high variance";
            ui.cv_folds.setValue(10);
        } else if (folds === 10) {
            guidance = "✅ Standard 10-fold CV";
        } else {
            guidance = "ℹ️ Custom fold count";
        }
        
        try {
            ui.cv_guidance.setValue(guidance);
        } catch(e) {}
    },

    // CV repeats optimization
    onChange_cvRepeats: function(ui) {
        let repeats = ui.cv_repeats.value();
        
        if (repeats < 3) {
            ui.cv_repeats.setValue(3);
        } else if (repeats > 10) {
            ui.cv_repeats.setValue(10);
        }
    },

    // Hold-out proportion validation
    onChange_holdoutProportion: function(ui) {
        let proportion = ui.holdout_proportion.value();
        let guidance = "";
        
        if (proportion < 0.15) {
            guidance = "⚠️ Test set too small";
            ui.holdout_proportion.setValue(0.20);
        } else if (proportion > 0.35) {
            guidance = "⚠️ Training set too small";
            ui.holdout_proportion.setValue(0.30);
        } else {
            guidance = "✅ Appropriate train/test split";
        }
        
        try {
            ui.holdout_guidance.setValue(guidance);
        } catch(e) {}
    },

    // Clinical context with parameter adjustment
    onChange_clinicalContext: function(ui) {
        let context = ui.clinical_context.value();
        
        switch(context) {
            case "screening":
                ui.cost_matrix.setValue("screening");
                ui.fn_fp_cost_ratio.setValue(4.0);
                ui.min_sensitivity.setValue(0.90);
                ui.min_specificity.setValue(0.75);
                break;
            case "diagnosis":
                ui.cost_matrix.setValue("diagnosis");
                ui.fn_fp_cost_ratio.setValue(2.5);
                ui.min_sensitivity.setValue(0.85);
                ui.min_specificity.setValue(0.85);
                break;
            case "prognosis":
                ui.cost_matrix.setValue("equal");
                ui.fn_fp_cost_ratio.setValue(2.0);
                ui.min_sensitivity.setValue(0.75);
                ui.min_specificity.setValue(0.75);
                break;
        }
        
        this.updateClinicalGuidance(ui);
    },

    // Prevalence adjustment with PPV/NPV updates
    onChange_prevalenceAdjustment: function(ui) {
        let adjust = ui.prevalence_adjustment.value();
        
        if (adjust) {
            this.updatePrevalenceImpact(ui);
        }
    },

    // Real-time prevalence impact calculation
    onChange_prevalence: function(ui) {
        let prev = ui.population_prevalence.value();
        let impact = "";
        
        // Validate prevalence range
        if (prev < 1) {
            ui.population_prevalence.setValue(1);
            prev = 1;
        } else if (prev > 50) {
            ui.population_prevalence.setValue(50);
            prev = 50;
        }
        
        // Calculate PPV/NPV impact at different performance levels
        let sens = 0.85; // Example sensitivity
        let spec = 0.85; // Example specificity
        
        let ppv = (sens * prev) / (sens * prev + (1 - spec) * (100 - prev));
        let npv = (spec * (100 - prev)) / (spec * (100 - prev) + (1 - sens) * prev);
        
        ppv = (ppv * 100).toFixed(1);
        npv = (npv * 100).toFixed(1);
        
        if (prev < 5) {
            impact = `Very low prevalence: PPV≈${ppv}%, NPV≈${npv}%`;
        } else if (prev < 15) {
            impact = `Low prevalence: PPV≈${ppv}%, NPV≈${npv}%`;
        } else if (prev < 30) {
            impact = `Moderate prevalence: PPV≈${ppv}%, NPV≈${npv}%`;
        } else {
            impact = `High prevalence: PPV≈${ppv}%, NPV≈${npv}%`;
        }
        
        try {
            ui.prevalence_impact.setValue(impact);
        } catch(e) {}
    },

    // Cost matrix with clinical interpretation
    onChange_costMatrix: function(ui) {
        let matrix = ui.cost_matrix.value();
        
        switch(matrix) {
            case "screening":
                ui.fn_fp_cost_ratio.setValue(4.0);
                break;
            case "diagnosis":
                ui.fn_fp_cost_ratio.setValue(2.0);
                break;
            case "equal":
                ui.fn_fp_cost_ratio.setValue(1.0);
                break;
        }
        
        this.updateCostInterpretation(ui);
    },

    // Cost ratio with clinical meaning
    onChange_costRatio: function(ui) {
        let ratio = ui.fn_fp_cost_ratio.value();
        let interpretation = "";
        
        if (ratio < 0.5) {
            ui.fn_fp_cost_ratio.setValue(0.5);
            ratio = 0.5;
        } else if (ratio > 10) {
            ui.fn_fp_cost_ratio.setValue(10);
            ratio = 10;
        }
        
        if (ratio < 1) {
            interpretation = "FP more costly than FN";
        } else if (ratio === 1) {
            interpretation = "Equal FN and FP costs";
        } else if (ratio < 3) {
            interpretation = "Moderate FN cost preference";
        } else if (ratio < 5) {
            interpretation = "Strong FN cost preference (screening)";
        } else {
            interpretation = "Very strong FN cost preference";
        }
        
        try {
            ui.cost_interpretation.setValue(interpretation);
        } catch(e) {}
    },

    // Performance threshold validation with warnings
    onChange_minSensitivity: function(ui) {
        let sens = ui.min_sensitivity.value();
        let warning = "";
        
        if (sens < 0.5 || sens > 0.99) {
            sens = Math.max(0.5, Math.min(0.99, sens));
            ui.min_sensitivity.setValue(sens);
        }
        
        if (sens < 0.7) {
            warning = "⚠️ Very low sensitivity requirement";
        } else if (sens > 0.95) {
            warning = "⚠️ Very high sensitivity - may compromise specificity";
        }
        
        this.checkPerformanceConsistency(ui);
        try {
            ui.sensitivity_warning.setValue(warning);
        } catch(e) {}
    },

    onChange_minSpecificity: function(ui) {
        let spec = ui.min_specificity.value();
        let warning = "";
        
        if (spec < 0.5 || spec > 0.99) {
            spec = Math.max(0.5, Math.min(0.99, spec));
            ui.min_specificity.setValue(spec);
        }
        
        if (spec < 0.7) {
            warning = "⚠️ Very low specificity requirement";
        } else if (spec > 0.95) {
            warning = "⚠️ Very high specificity - may compromise sensitivity";
        }
        
        this.checkPerformanceConsistency(ui);
        try {
            ui.specificity_warning.setValue(warning);
        } catch(e) {}
    },

    onChange_minPPV: function(ui) {
        let ppv = ui.min_ppv.value();
        let warning = "";
        
        if (ppv < 0.1 || ppv > 0.99) {
            ppv = Math.max(0.1, Math.min(0.99, ppv));
            ui.min_ppv.setValue(ppv);
        }
        
        let prevalence = ui.population_prevalence.value() / 100;
        if (ppv > 0.8 && prevalence < 0.1) {
            warning = "⚠️ High PPV difficult with low prevalence";
        }
        
        try {
            ui.ppv_warning.setValue(warning);
        } catch(e) {}
    },

    onChange_minNPV: function(ui) {
        let npv = ui.min_npv.value();
        let warning = "";
        
        if (npv < 0.5 || npv > 0.99) {
            npv = Math.max(0.5, Math.min(0.99, npv));
            ui.min_npv.setValue(npv);
        }
        
        let prevalence = ui.population_prevalence.value() / 100;
        if (npv > 0.95 && prevalence > 0.3) {
            warning = "⚠️ High NPV difficult with high prevalence";
        }
        
        try {
            ui.npv_warning.setValue(warning);
        } catch(e) {}
    },

    // Auto-optimization toggle
    onChange_autoOptimize: function(ui) {
        let optimize = ui.auto_optimize_threshold.value();
        
        if (optimize) {
            ui.optimization_metric.setValue("youden");
            ui.show_threshold_optimization.setValue(true);
        }
    },

    // Optimization metric selection
    onChange_optimizationMetric: function(ui) {
        let metric = ui.optimization_metric.value();
        
        switch(metric) {
            case "youden":
                // Balanced approach
                break;
            case "utility":
                // Cost-based optimization
                ui.show_clinical_guidance.setValue(true);
                break;
            case "f1":
                // Precision-recall balance
                break;
            case "mcc":
                // Matthews correlation
                break;
        }
    },

    // Confidence level validation
    onChange_confidenceLevel: function(ui) {
        let conf = ui.confidence_level.value();
        
        if (conf < 0.8 || conf > 0.99) {
            conf = Math.max(0.8, Math.min(0.99, conf));
            ui.confidence_level.setValue(conf);
        }
        
        if (conf < 0.9) {
            // Lower confidence = wider intervals
        } else if (conf > 0.95) {
            // Higher confidence = wider intervals
        }
    },

    // Helper functions
    updateValidationGuidance: function(ui) {
        let method = ui.validation_method.value();
        // Update method-specific guidance
    },

    updateClinicalGuidance: function(ui) {
        let context = ui.clinical_context.value();
        // Update context-specific clinical guidance
    },

    updatePrevalenceImpact: function(ui) {
        // Calculate and display prevalence impact on PPV/NPV
        this.onChange_prevalence(ui);
    },

    updateCostInterpretation: function(ui) {
        // Update cost ratio interpretation
        this.onChange_costRatio(ui);
    },

    checkPerformanceConsistency: function(ui) {
        let sens = ui.min_sensitivity.value();
        let spec = ui.min_specificity.value();
        
        // Check if requirements are achievable
        if (sens + spec > 1.8) {
            // Very demanding requirements - may be difficult to achieve
        }
    }
};

module.exports = events;