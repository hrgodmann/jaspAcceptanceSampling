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

Form
{
	Section
	{
		title: qsTr("Single Sampling Plan")
		columns: 1

		Common.PlanSingle {}

		Common.Distribution
		{
			prefix: "singlePlan"
		}

		Group
		{
			CheckBox { name: "singlePlanAssessment"; label: qsTr("Assess attribute plan"); id: assessSingle }
			Common.RiskPoints
			{
				prefix: "singlePlanAssessment"
				enabled: assessSingle.checked
			}
		}

		Common.ProbDefect
		{
			prefix: "singlePlan"
		}

		Common.OutputOptions
		{
			prefix: "singlePlan"
		}
	}

	Section
	{
		title: qsTr("Multiple Sampling Plan")
		columns: 1

		Common.PlanMultiple {}

		Common.Distribution
		{
			prefix: "multiplePlan"
		}

		Group
		{
			CheckBox { name: "multiplePlanAssessment"; label: qsTr("Assess sampling plan"); id: assessMult }
			Common.RiskPoints
			{
				prefix: "multiplePlanAssessment"
				enabled: assessMult.checked
			}
		}

		Common.ProbDefect
		{
			prefix: "multiplePlan"
		}
		
		Common.OutputOptions
		{
			prefix: "multiplePlan"
		}
	}
}
