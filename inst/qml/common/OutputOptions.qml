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
import "./" as Common

Group
{
	title: qsTr("Output Options")
	property string prefix: ""
	CheckBox { name: (prefix === "" ? "summaryTable" : prefix + "SummaryTable"); label: qsTr("Summary table") }
	CheckBox { name: (prefix === "" ? "operatingCharacteristicCurve" : prefix + "OperatingCharacteristicCurve"); label: qsTr("OC curve") }
	CheckBox { name: (prefix === "" ? "averageOutgoingQualityCurve" : prefix + "AverageOutgoingQualityCurve"); label: qsTr("AOQ curve (plan with rectification)") }
	CheckBox { name: (prefix === "" ? "averageTotalInspectionCurve" : prefix + "AverageTotalInspectionCurve"); label: qsTr("ATI curve (plan with rectification)") }
	CheckBox { name: (prefix === "" ? "averageSampleNumberCurve" : prefix + "AverageSampleNumberCurve"); label: qsTr("ASN curve"); enabled: prefix === "multiplePlan" ? true : false; visible: prefix === "multiplePlan" ? true : false }
}
