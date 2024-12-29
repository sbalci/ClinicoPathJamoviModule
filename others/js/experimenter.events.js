const settings = {
    DECISION_TREE: `minsplit = 20, cp = 0.01, maxcompete = 4, maxsurrogate = 5, maxdepth = 30, usesurrogate = 2, surrogatestyle = 0, xval = 10`,
    KNN: `k = 7, distance = 2, kernel = optimal, scale = TRUE`,
    RANDOM_FOREST: `holdout = FALSE, min.node.size = 1, num.random.splits = 1, num.trees = 500, sample.fraction = 1, splitrule = gini`,
    LOGISTIC_REGRESSION: ``,
    NAIVE_BAYES: `laplace = 0, threshold = 0.01, eps = 0`,
    DEFAULT: `Settings for chosen algorithm in following format: setting1 = 2, setting2 = 3... setingN = 4`
};

const events = {
    update: function (ui) {
        console.log('update was called');
        setSettingsPlaceHolder(ui);
    },

    onChange_classifiersList: function (ui) {
        let lastElementIndex = ui.classifiersToUse.getControls().length - 1;
        let settings = document.getElementById('settings').value;
        let classifierName = ui.classifiersToUse.value()[lastElementIndex];

        ui.classifiersToUse.getControls()[lastElementIndex].$label.removeClass('silky-list-item-value');
        ui.classifiersToUse.getControls()[lastElementIndex].setValue(`${classifierName} (${settings})`);
        ui.classifiersToUse._localData.pop();

        document.getElementById('settings').value = "";
    },

    onUpdate_classifiersSupplier: function (ui) {
        let availableClassifiers = ["Decision tree", "Random forest", "KNN", 'Logistic regression', 'Naive Bayes'];

        ui.classifiers.setValue(this.base.valuesToItems(availableClassifiers, FormatDef.variable));
    },
};

    var setSettingsPlaceHolder = function (ui) {

        var textarea = document.createElement("TEXTAREA");
        textarea.cols = 50;
        textarea.rows = 3;
        textarea.id = "settings";
        textarea.style = "margin-left: 10px;";

        ui.classifierSettings.$input = ui.classifierSettings.$input.replaceWith(textarea);

        ui.classifiers.value().forEach(classifier => classifier.$el.click(function () {
            switch (classifier.value.raw) {
                case "Decision tree":
                    textarea.placeholder = settings.DECISION_TREE;
                    break;
                case "KNN":
                    textarea.placeholder = settings.KNN;
                    break;
                case "Naive bayes":
                    textarea.placeholder = settings.NAIVE_BAYES;
                    break;
                case "Logistic regression":
                    textarea.placeholder = settings.LOGISTIC_REGRESSION;
                    break;
                case "Random forest":
                    textarea.placeholder = settings.RANDOM_FOREST;
                    break;
                default:
                    textarea.placeholder = settings.DEFAULT;
                    break;
            }
        }));
    };

module.exports = events;