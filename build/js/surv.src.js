
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"event","title":"Event","type":"Variable","suggested":["nominal"],"permitted":["factor"]},{"name":"eventLevel","title":"Event level","type":"Level","variable":"(event)"},{"name":"elapsed","title":"Time Elapsed","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"groups","title":"Group","type":"Variable","suggested":["nominal"],"permitted":["factor"]},{"name":"tests","title":"Tests","type":"NMXList","options":[{"name":"logrank","title":"Log-rank"},{"name":"gehan","title":"Gehan"},{"name":"tarone-ware","title":"Tarone-Ware"},{"name":"peto-peto","title":"Peto-Peto"}]},{"name":"sc","title":"Survival curve","type":"Bool","default":true},{"name":"hf","title":"Hazard function","type":"Bool","default":false,"hidden":true},{"name":"chf","title":"Cumulative hazard function","type":"Bool","default":false},{"name":"ci","title":"Confidence intervals","type":"Bool","default":false},{"name":"cens","title":"Censored events","type":"Bool","default":false}];

const view = function() {
    
    

    View.extend({
        jus: "2.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "survcopied",
    jus: "2.0",
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
					label: "Event",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "event",
							maxItemCount: 1,
							isTarget: true
						},
						{
							type: DefaultControls.LevelSelector,
							typeName: 'LevelSelector',
							name: "eventLevel"
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Time Elapsed",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "elapsed",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Groups",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "groups",
							maxItemCount: 1,
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.Label,
			typeName: 'Label',
			name: "tests",
			label: "Tests",
			controls: [
				{
					name: "tests_logrank",
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					optionName: "tests",
					optionPart: "logrank"
				},
				{
					name: "tests_gehan",
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					optionName: "tests",
					optionPart: "gehan",
					enable: "(groups)"
				},
				{
					name: "tests_tarone-ware",
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					optionName: "tests",
					optionPart: "tarone-ware",
					enable: "(groups)"
				},
				{
					name: "tests_peto-peto",
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					optionName: "tests",
					optionPart: "peto-peto",
					enable: "(groups)"
				}
			]
		},
		{
			type: DefaultControls.Label,
			typeName: 'Label',
			label: "Plots",
			margin: "large",
			cell: {"row":1,"column":1},
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "small",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "sc",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "ci"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "cens"
								}
							]
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "chf"
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
