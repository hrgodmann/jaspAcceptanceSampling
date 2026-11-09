//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program. If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP.Controls
import "./common" as Common
import JASP

Form
{
	columns: 1
	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList"; id: allVariablesList }
		AssignedVariablesList	{ name: "measurement"; title: qsTr("Measurement"); id: measurement; singleVariable: true; allowedColumns: ["scale"] }
	}

	Group
	{
		enabled: measurement.count != 1
		Text { text: qsTr("Specify sample statistics directly (used if dataset is not available)") }
		CheckBox { name: "sampleStatistics"; id: sampleStatistics; checked: (measurement.count != 1); enabled: false; visible: false }
		IntegerField { name: "sampleSize"; label: qsTr("Sample size (n)"); defaultValue: 24; min: 1 }
		DoubleField { name: "sampleMean"; label: qsTr("Sample mean"); defaultValue: 1.5; decimals: 6 }
		DoubleField { name: "sampleSd"; label: qsTr("Sample standard deviation"); defaultValue: 1; min: 0; inclusive: JASP.None; decimals: 6 }
	}

	// Todo: Label for k is a hacky solution to align it with the sample values. Adjust the width in code.
	DoubleField { name: "kValue"; label: qsTr("k                        "); defaultValue: 1.309; min: 0; negativeValues: false; inclusive: JASP.None; decimals: 6 }
	
	Group
	{
		title: qsTr("Specification Limits")
		columns: 2
		CheckBox { name: "lowerSpecificationLimit"; label: qsTr("Lower specification limit (LSL)"); id: lowerSpecificationLimit; checked: false }
		DoubleField{ name: "lowerSpecificationLimitValue"; label: ""; id: lowerSpecificationLimitValue; defaultValue: 0; enabled: lowerSpecificationLimit.checked; negativeValues: true; max: upperSpecificationLimitValue.value; inclusive: JASP.MaxOnly; decimals: 6 }
		CheckBox { name: "upperSpecificationLimit"; label: qsTr("Upper specification limit (USL)"); id: upperSpecificationLimit; checked: false }
		DoubleField { name: "upperSpecificationLimitValue"; label: ""; id: upperSpecificationLimitValue; defaultValue: 1; enabled: upperSpecificationLimit.checked; negativeValues: true; min: lowerSpecificationLimitValue.value; inclusive: JASP.MinOnly; decimals: 6 }
	}

	Group
	{
		columns: 2
		CheckBox { name: "historicalSdKnown"; label: qsTr("Standard deviation (historical)"); id: historicalSdKnown; checked: false }
		DoubleField { name: "historicalSd"; label: ""; enabled: historicalSdKnown.checked; defaultValue: 1; min: 0; negativeValues: false; inclusive: JASP.None; decimals: 6 }
	}

	Group
	{
		title: qsTr("Quality Constraints")
		enabled: lowerSpecificationLimit.checked && upperSpecificationLimit.checked && historicalSdKnown.checked
		columns: 2
		Text { text: qsTr("Acceptable quality level (AQL)") }
		DoubleField{ name: "acceptableQualityLevel"; label: ""; negativeValues: false; defaultValue: 0.05; min: 0; max: 1; inclusive: JASP.None; decimals: 6 }
		Text { text: qsTr("Rejectable quality level (RQL / LTPD)") }
		DoubleField { name: "rejectableQualityLevel"; label: ""; negativeValues: false; defaultValue: 0.15; min: 0; max: 1; inclusive: JASP.None; decimals: 6 }
	}
}
