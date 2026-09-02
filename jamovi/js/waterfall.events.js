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

        if (colorBy === "group") {
            ui.colorScheme.setValue("colorful");
            ui.spiderColorBy.setValue("group");
            ui.spiderColorScheme.setValue("colorful");
        } else if (colorBy === "recist") {
            ui.colorScheme.setValue("recist");
            ui.spiderColorBy.setValue("response");
            ui.spiderColorScheme.setValue("classic");
        }
    },

    // Selecting a group variable implies group-based coloring.
    onChange_groupVar: function(ui) {
        if (ui.groupVar.value()) {
            ui.colorBy.setValue("group");
            ui.colorScheme.setValue("colorful");
            ui.spiderColorBy.setValue("group");
            ui.spiderColorScheme.setValue("colorful");
        }
    }
};

module.exports = events;
