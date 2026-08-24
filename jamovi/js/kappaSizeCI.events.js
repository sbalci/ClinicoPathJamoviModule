// UI events for kappaSizeCI. Bound from jamovi/kappaSizeCI.u.yaml
// (outcome ComboBox -> events: change: ./kappaSizeCI.events::onChange_outcome).
//
// The proportions field must hold exactly one value per outcome level, so changing the
// number of levels always invalidates what is typed there. Replace it with a template of
// the right length (each summing to 1) rather than leaving the user with a count error.
const events = {

    onChange_outcome: function(ui) {
        const outcome = String(ui.outcome.value());
        const templates = {
            '2': '0.20, 0.80',
            '3': '0.20, 0.60, 0.20',
            '4': '0.20, 0.40, 0.20, 0.20',
            '5': '0.10, 0.30, 0.20, 0.20, 0.20'
        };
        const template = templates[outcome];
        if (template === undefined)
            return;

        // Only replace what the new level count has actually invalidated. Overwriting
        // unconditionally destroys hand-entered proportions every time this runs -- including
        // whenever jamovi fires `change` while binding options, which would silently reset a
        // saved .omv back to the template. A binary outcome accepts one value or two, matching
        // .validateProportions() in the backend.
        const current = String(ui.props.value() || '');
        const count = current.split(/[,;|\s]+/).filter(s => s.length > 0).length;
        const needed = outcome === '2' ? [1, 2] : [Number(outcome)];
        if (!needed.includes(count))
            ui.props.setValue(template);
    }

};

module.exports = events;
