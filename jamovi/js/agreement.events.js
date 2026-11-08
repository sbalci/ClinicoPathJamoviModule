// Read-only guidance for the category shared across the selected raters.
const revisions = new WeakMap();
const maxShown = 30;

const setHint = (hint, text) => {
    hint.textContent = text;
    hint.dispatchEvent(new Event('contentchanged', { bubbles: true }));
};

const hintElement = ui => {
    const control = ui.specificCategoryHint;
    return control && control.$el ? control.$el[0] : null;
};

const refresh = (ui, context) => {
    const hint = hintElement(ui);
    if (!hint)
        return Promise.resolve();

    const revision = (revisions.get(hint) || 0) + 1;
    revisions.set(hint, revision);
    if (!ui.specificAgreement.value()) {
        setHint(hint, _('Enable specific agreement to see the available categories.'));
        return Promise.resolve();
    }

    const raters = ui.vars.value() || [];
    if (raters.length === 0) {
        setHint(hint, _('Select rater variables to see the available categories.'));
        return Promise.resolve();
    }

    setHint(hint, _('Loading categories...'));
    const requests = raters.map(columnName => Promise.resolve().then(() =>
        context.requestData('column', {
            columnName: columnName,
            properties: ['levels', 'measureType']
        })));

    return Promise.all(requests).then(columns => {
        if (revisions.get(hint) !== revision)
            return;

        const categories = [];
        const seen = new Set();
        for (const column of columns) {
            if (!column.columnFound || column.measureType === 'continuous')
                continue;
            for (const level of column.levels || []) {
                if (level.treatAsMissing || level.filtered || level.label == null)
                    continue;
                const label = String(level.label);
                if (label !== '' && !seen.has(label)) {
                    seen.add(label);
                    categories.push(label);
                }
            }
        }

        if (categories.length === 0) {
            setHint(hint, _('No categories available. Select nominal or ordinal rater variables.'));
            return;
        }

        const labels = categories.slice(0, maxShown).join('; ');
        const message = categories.length <= maxShown
            ? _('Available categories: {categories}')
            : _('Available categories (first 30 of {total}): {categories}');
        // Text only: category labels are user data, never HTML or replacement patterns.
        setHint(hint, message.replace('{total}', () => String(categories.length))
            .replace('{categories}', () => labels));
    }).catch(() => {
        if (revisions.get(hint) === revision)
            setHint(hint, _('Categories could not be loaded. Check the levels in the selected rater variables.'));
    });
};

module.exports = {
    createCategoryHint: function(ui) {
        const hint = hintElement(ui);
        hint.style.whiteSpace = 'normal';
        hint.style.overflowWrap = 'anywhere';
        hint.style.maxWidth = '30em';
        hint.style.fontSize = '0.9em';
        hint.setAttribute('role', 'status');
        hint.setAttribute('aria-live', 'polite');
        return refresh(ui, this);
    },

    refreshCategoryHint: function(ui) {
        return refresh(ui, this);
    },

    onRemoteDataChanged: function(ui, event) {
        if (event.dataType === 'columns')
            return refresh(ui, this);
    }
};
