import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form {
    VariablesForm {
        AvailableVariablesList { name: "allVariables" }

        AssignedVariablesList {
            name: "age"
            title: qsTr("Continuous Explanatory Variable (e.g., Age)")
            singleVariable: true
            allowedColumns: ["scale", "ordinal"]
        }

        AssignedVariablesList {
            name: "stratVars"
            title: qsTr("Stratification Variables (Raking)")
            allowedColumns: ["nominal", "ordinal"]
        }
    }

    Section {
        title: qsTr("Norm Groups (getGroups)")

        IntegerField { name: "nGroups"; label: qsTr("Number of norm groups (0 = auto)"); defaultValue: 0; min: 0; max: 50 }
        CheckBox { name: "equidistant"; label: qsTr("Equidistant intervals (instead of equal sizes)") }
    }

    Section {
        title: qsTr("Raking Weights (computeWeights)")

        Group {
            title: qsTr("Target Population Marginals")
            Layout.columnSpan: 2

            TextField {
                name: "margins"
                label: qsTr("Proportions (var: lvl=p, ...)")
                defaultValue: ""
                fieldWidth: 280
            }
            Label {
                text: qsTr("Example: sex: 1=0.52, 2=0.48; migration: 0=0.70, 1=0.30")
            }
            CheckBox { name: "standardized"; label: qsTr("Standardize raking weights (min weight = 1)"); checked: true }
        }
    }
}