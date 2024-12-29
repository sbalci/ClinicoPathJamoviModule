    module.exports = {

        outcome_changed: function(ui, event) {

let outcome = ui.outcome.value();

if (outcome == 2)
  ui.props.setValue('0.20 , 0.80');

if (outcome == 3)
  ui.props.setValue('0.20 , 0.60, 0.20');

  if (outcome == 4)
  ui.props.setValue('0.20 , 0.40, 0.20, 0.20');

  if (outcome == 5)
  ui.props.setValue('0.10 , 0.30, 0.20, 0.20, 0.20');

        }

    };
