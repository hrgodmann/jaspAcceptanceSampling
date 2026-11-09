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

Group
{
	title: qsTr("Proportion Non-Conforming Items")
	property string prefix: ""
	Group
	{
		columns: 2
		Text { text: qsTr("From") }
		DoubleField{ name: (prefix === "" ? "proportionNonconformingFrom" : prefix + "ProportionNonconformingFrom"); label: ""; negativeValues: false; defaultValue: 0; min: 0; max: 1; decimals: 6 }
		Text { text: qsTr("To") }
		DoubleField{ name: (prefix === "" ? "proportionNonconformingTo" : prefix + "ProportionNonconformingTo"); label: ""; negativeValues: false; defaultValue: 0.15; min: 0; max: 1; decimals: 6 }
		Text { text: qsTr("Step size") }
		DoubleField{ name: (prefix === "" ? "proportionNonconformingStep" : prefix + "ProportionNonconformingStep"); label: ""; negativeValues: false; defaultValue: 0.01; min: 0; max: 1; decimals: 6 }
	}
}
