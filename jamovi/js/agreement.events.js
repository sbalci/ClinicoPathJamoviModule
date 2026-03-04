const events = {

    // When confidence level changes, sync Bland-Altman confidence level if user hasn't customized it
    onChange_confLevel: function(ui) {
        let confLevel = ui.confLevel.value();
        // Keep Bland-Altman in sync unless user has set a different value
        let baLevel = ui.baConfidenceLevel.value();
        if (Math.abs(baLevel - 0.95) < 0.001 || Math.abs(baLevel - confLevel) < 0.001) {
            ui.baConfidenceLevel.setValue(confLevel);
        }
    },

    // Validate confidence level range
    onChange_baConfidenceLevel: function(ui) {
        let val = ui.baConfidenceLevel.value();
        if (val < 0.50) {
            ui.baConfidenceLevel.setValue(0.50);
        } else if (val > 0.99) {
            ui.baConfidenceLevel.setValue(0.99);
        }
    },

    // Validate cluster count
    onChange_nClusters: function(ui) {
        let val = ui.nClusters.value();
        if (val < 2) {
            ui.nClusters.setValue(2);
        } else if (val > 10) {
            ui.nClusters.setValue(10);
        }
    },

    // Validate case cluster count
    onChange_nCaseClusters: function(ui) {
        let val = ui.nCaseClusters.value();
        if (val < 2) {
            ui.nCaseClusters.setValue(2);
        } else if (val > 20) {
            ui.nCaseClusters.setValue(20);
        }
    },

    // Validate minimum subgroup cases
    onChange_subgroupMinCases: function(ui) {
        let val = ui.subgroupMinCases.value();
        if (val < 5) {
            ui.subgroupMinCases.setValue(5);
        } else if (val > 100) {
            ui.subgroupMinCases.setValue(100);
        }
    }
};

module.exports = events;
