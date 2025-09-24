module.exports = {

    outcome_changed: function(ui, event) {
        // Handle outcome variable changes
        let outcome = ui.outcome.value();
        if (outcome) {
            // Populate outcome levels if available
            let levels = ui.outcome.levels();
            if (levels.length > 0) {
                ui.outcomeLevel.setValue(levels[levels.length - 1]);
            }
        }
    },

    data_format_changed: function(ui, event) {
        // Handle data format changes
        let format = ui.data_format.value();
        if (format === 'counting') {
            ui.use_start_stop.setValue(true);
        }
    },

    use_start_stop_changed: function(ui, event) {
        // Handle start-stop time option changes
        let use_start_stop = ui.use_start_stop.value();
        if (use_start_stop) {
            // Enable start and stop time variable selection
        }
    }

};