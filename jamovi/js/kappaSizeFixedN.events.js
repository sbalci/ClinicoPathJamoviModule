// UI events for kappaSizeFixedN. Bound from jamovi/kappaSizeFixedN.u.yaml
// (outcome ComboBox -> events: change: ./kappaSizeFixedN.events::onChange_outcome).
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
        if (templates[outcome] !== undefined)
            ui.props.setValue(templates[outcome]);
    }

};

module.exports = events;
