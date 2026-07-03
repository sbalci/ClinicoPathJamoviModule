// Populates the interaction model-builder's term pool from the union of the
// `explanatory` and `contexpl` variable boxes, and prunes interaction terms
// that reference a variable no longer selected. Main effects are NOT added
// here — they come from the `explanatory`/`contexpl` options directly.
// Reference: jmvbaseR anova.events.js.

const events = {
    update: function(ui) {
        syncInteractionPool(ui, this);
    },

    onChange_predictors: function(ui) {
        syncInteractionPool(ui, this);
    },

    onUpdate_interactionSupplier: function(ui) {
        let vars = collectPredictors(ui, this);
        ui.interactionSupplier.setValue(
            this.valuesToItems(vars, FormatDef.variable));
    }
};

let collectPredictors = function(ui, context) {
    let a = context.cloneArray(ui.explanatory.value(), []);
    let b = context.cloneArray(ui.contexpl.value(), []);
    return a.concat(b);
};

let syncInteractionPool = function(ui, context) {
    let vars = collectPredictors(ui, context);

    ui.interactionSupplier.setValue(
        context.valuesToItems(vars, FormatDef.variable));

    // Prune interaction terms that reference removed variables.
    let varsDiff = context.findChanges("predictorList", vars, true,
                                       FormatDef.variable);
    let termsList = context.cloneArray(ui.interactions.value(), []);
    let changed = false;

    for (let i = 0; i < varsDiff.removed.length; i++) {
        for (let j = 0; j < termsList.length; j++) {
            if (FormatDef.term.contains(termsList[j], varsDiff.removed[i])) {
                termsList.splice(j, 1);
                changed = true;
                j -= 1;
            }
        }
    }

    if (changed)
        ui.interactions.setValue(termsList);
};

module.exports = events;
