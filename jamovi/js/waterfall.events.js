// Smart defaults, bound from waterfall.u.yaml via `events: change:` on each
// control. Range clamping for minResponseForLabel / barAlpha / barWidth is NOT
// done here - the a.yaml min/max already enforce it in the UI.

const events = {
    // Raw per-visit data makes the spider plot meaningful; percentage data
    // centres on the waterfall.
    onChange_inputType: function(ui) {
        if (ui.inputType.value() === "raw")
            ui.showSpiderPlot.setValue(true);
        else
            ui.showWaterfallPlot.setValue(true);
    },

    // Keep waterfall and spider coloring consistent with the chosen mode.
    onChange_colorBy: function(ui) {
        let colorBy = ui.colorBy.value();

        // The colour SCHEME is the user's choice: switching to group colouring
        // used to replace a chosen colour-blind or jamovi palette with rainbow.
        if (colorBy === "group") {
            ui.spiderColorBy.setValue("group");
        } else if (colorBy === "recist") {
            ui.spiderColorBy.setValue("response");
        }
    },

    // Selecting a group variable implies group-based coloring.
    onChange_groupVar: function(ui) {
        if (ui.groupVar.value()) {
            ui.colorBy.setValue("group");
            ui.spiderColorBy.setValue("group");
        }
    }
};

module.exports = events;
