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
import JASP.Controls

Group
{
	IntegerField { name: "singlePlanLotSize"; label: qsTr("Lot size (N)"); defaultValue: 1000; min: 1 }
	IntegerField { name: "singlePlanSampleSize"; label: qsTr("Sample size (n)"); defaultValue: 100; min: 1; }
	IntegerField { name: "singlePlanAcceptanceNumber"; label: qsTr("Acceptance number (c)"); id: singlePlanAcceptanceNumber; defaultValue: 4 }
	IntegerField { name: "singlePlanRejectionNumber"; label: qsTr("Rejection number (r)"); id: singlePlanRejectionNumber; value: parseInt(singlePlanAcceptanceNumber.value) + 1; enabled: false }
}
