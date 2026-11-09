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
import JASP

Group
{
	title: qsTr("Quality Constraints")
	property string prefix: ""
	property bool includeLimits: true
	Group
	{
		columns: 2
		Text { text: qsTr("Acceptable quality level (AQL)") }
		DoubleField{ name: (prefix === "" ? "acceptableQualityLevel" : prefix + "AcceptableQualityLevel"); label: ""; negativeValues: false; defaultValue: 0.05; min: 0; max: 1; inclusive: includeLimits ? JASP.MinMax : JASP.None; decimals: 6 }
		Text { text: qsTr("Rejectable quality level (RQL / LTPD)") }
		DoubleField { name: (prefix === "" ? "rejectableQualityLevel" : prefix + "RejectableQualityLevel"); label: ""; negativeValues: false; defaultValue: 0.15; min: 0; max: 1; inclusive: includeLimits ? JASP.MinMax : JASP.None; decimals: 6 }
	}

	Group
	{
		columns: 2
		Text { text: qsTr("Producer's risk (\u03B1)") }
		DoubleField{ name: (prefix === "" ? "producerRisk" : prefix + "ProducerRisk"); label: ""; negativeValues: false; defaultValue: 0.05; min: 0; max: 1; inclusive: includeLimits ? JASP.MinMax : JASP.None; decimals: 6 }
		Text { text: qsTr("Consumer's risk (\u03B2)") }
		DoubleField { name: (prefix === "" ? "consumerRisk" : prefix + "ConsumerRisk"); label: ""; negativeValues: false; defaultValue: 0.10; min: 0; max: 1; inclusive: includeLimits ? JASP.MinMax : JASP.None; decimals: 6 }
	}
}
