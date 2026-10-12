const events = {

    // Validate confidence level range
    onChange_baConfidenceLevel: function(ui) {
        let val = ui.baConfidenceLevel.value();
        if (val < 0.50) {
            ui.baConfidenceLevel.setValue(0.50);
        } else if (val > 0.99) {
            ui.baConfidenceLevel.setValue(0.99);
        }
    }
};

module.exports = events;
