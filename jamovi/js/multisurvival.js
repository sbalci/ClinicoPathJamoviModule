module.exports = {

    view_updated: function(ui) {
        this.calcModelTerms(ui);
        this.filterModelTerms(ui);
    },

    factors_changed: function(ui) {
        this.calcModelTerms(ui);
    },

    modelTerms_changed: function(ui) {
        this.filterModelTerms(ui);
    },

    modelSupplier_updated: function(ui) {
        let variableList = utils.clone(ui.factors.value(), []);
        ui.modelSupplier.setValue(utils.valuesToItems(variableList, FormatDef.variable));
    },


    filterModelTerms: function(ui) {
        var termsList = utils.clone(ui.modelTerms.value(), []);

        //Remove common terms
        var termsDiff = this.findChanges("currentList", termsList, true, FormatDef.term);
        var changed = false;
        if (termsDiff.removed.length > 0 && termsList !== null) {
            var itemsRemoved = false;
            for (var i = 0; i < termsDiff.removed.length; i++) {
                var item = termsDiff.removed[i];
                for (var j = 0; j < termsList.length; j++) {
                    if (FormatDef.term.contains(termsList[j], item)) {
                        termsList.splice(j, 1);
                        j -= 1;
                        itemsRemoved = true;
                    }
                }
            }

            if (itemsRemoved)
                changed = true;
        }
        /////////////////////

        //Sort terms
        if (utils.sortArraysByLength(termsList))
            changed = true;
        ////////////

        if (changed)
            ui.modelTerms.setValue(termsList);

    },



    updateModelLabels: function(list, blockName) {
        list.applyToItems(0, (item, index) => {
            item.controls[0].setPropertyValue("label", blockName + " " + (index + 1) );
        });
    },

    calcModelTerms: function(ui) {
        var variableList = utils.clone(ui.factors.value(), []);

        ui.modelSupplier.setValue(utils.valuesToItems(variableList, FormatDef.variable));

        this.calcMarginalMeansSupplier(ui);

        var varsDiff = this.findChanges("variableList", variableList, true, FormatDef.variable);
        var termsList = utils.clone(ui.modelTerms.value(), []);

        var termsChanged = false;
        for (var i = 0; i < varsDiff.removed.length; i++) {
            for (var j = 0; j < termsList.length; j++) {
                if (FormatDef.term.contains(termsList[j], varsDiff.removed[i])) {
                    termsList.splice(j, 1);
                    termsChanged = true;
                    j -= 1;
                }
            }
        }

        termsList = utils.getCombinations(varsDiff.added, termsList);
        termsChanged = termsChanged || varsDiff.added.length > 0;

        if (termsChanged)
            ui.modelTerms.setValue(termsList);
    }
};
