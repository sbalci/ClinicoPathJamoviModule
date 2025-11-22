// jrecode.events.js - UI helpers for interactive recoding

const buildIdentityRules = (levels) => {
    if (!Array.isArray(levels) || levels.length === 0)
        return '';

    const sanitized = levels
        .map(level => (level === null || level === undefined) ? '' : `${level}`.trim())
        .filter(level => level !== '');

    if (sanitized.length === 0)
        return '';

    return sanitized.map(level => `${level} -> ${level}`).join('\n');
};

const events = {
    // When variable changes, pre-fill recode rules with identity mappings
    onChange_dep: function(ui) {
        const rulesControl = ui.recode_rules;
        const depControl = ui.dep;

        if (!rulesControl || !depControl || typeof rulesControl.value !== 'function')
            return;

        const currentValue = rulesControl.value();
        if (currentValue && currentValue.trim() !== '')
            return;  // leave user input untouched

        const levels = (typeof depControl.levels === 'function') ? depControl.levels() : [];
        const template = buildIdentityRules(levels);

        if (template && typeof rulesControl.setValue === 'function') {
            rulesControl.setValue(template);
        }
    }
};

module.exports = events;
