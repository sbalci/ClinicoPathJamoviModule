const events = {

    // A preset used to flip these options invisibly in the backend, so the
    // checkboxes disagreed with what ran. Set the controls instead; "custom"
    // leaves the user's own choices alone.
    onChange_clinicalpreset: function(ui) {
        let preset = ui.clinicalpreset.value();
        if (preset === "custom")
            return;
        ui.resultssubtitle.setValue(true);
        if (preset === "riskfactor")
            ui.proportiontest.setValue(true);
    }
};

module.exports = events;
