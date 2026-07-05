// Interaction model-builder events for multisurvival.
//
// Populates the interaction term pool ("Available Predictors") from the union of
// the `explanatory` and `contexpl` variable boxes, so the user can cross them
// into interaction terms. Main effects are NOT added here - they come from the
// `explanatory`/`contexpl` options directly.
//
// This uses the standard jamovi model-builder events API (`this.cloneArray`,
// `this.valuesToItems`, `this.findChanges`, `FormatDef`). Those helpers live on
// the handler `this` context under jus 2.0 (as used by jmvbaseR and gamlj); they
// are NOT available under jus 3.0, which is why multisurvival.u.yaml is jus 2.0.
//
// Reference: jmvbaseR anova.events.js and gamlj gamlj.events.js.

const events = {
    update: function(ui) {
        calcInteractionPool(ui, this);
    },

    onChange_predictors: function(ui) {
        calcInteractionPool(ui, this);
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

let calcInteractionPool = function(ui, context) {
    let vars = collectPredictors(ui, context);

    ui.interactionSupplier.setValue(
        context.valuesToItems(vars, FormatDef.variable));

    // Prune interaction terms that reference a variable no longer selected.
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
