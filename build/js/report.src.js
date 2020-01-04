
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"vars","title":"Variables","type":"Variables"},{"name":"med","title":"Median","type":"Bool","default":false},{"name":"cent","title":"Centrality","type":"Bool","default":true},{"name":"disp","title":"Dispersion","type":"Bool","default":true},{"name":"ran","title":"Range","type":"Bool","default":true},{"name":"distr","title":"Distribution","type":"Bool","default":false},{"name":"lev","title":"Levels Percentage","type":"Bool","default":false},{"name":"n_ch","title":"Character Numbers","type":"Integer","default":3,"min":0,"max":5},{"name":"mis","title":"Missing","type":"Bool","default":false},{"name":"gli","title":"Glimpse Data","type":"Bool","default":true}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Report General Features",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Variables",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "vars",
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "med"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "cent"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "disp"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "ran"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "distr"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "lev"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "n_ch",
					format: FormatDef.number
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "mis"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "gli"
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
