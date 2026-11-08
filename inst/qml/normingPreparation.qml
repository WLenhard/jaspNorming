import QtQuick 2.12
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form {
    VariablesForm {
        AvailableVariablesList { name: "allVariables" }

        AssignedVariablesList {
            name: "age"
            title: qsTr("Continuous Explanatory Variable (e.g., Age)")
            singleVariable: true
            allowedColumns: ["scale"]
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
        CheckBox { name: "equidistant"; label: qsTr("Equidistant intervals (instead of equal sizes)"); defaultValue: false }
    }

    Section {
        title: qsTr("Raking Weights (computeWeights)")

        TextField {
            name: "margins"
            label: qsTr("Population proportions (e.g., sex: 1=0.52, 2=0.48; migration: 0=0.70, 1=0.30)")
            defaultValue: ""
        }
        CheckBox { name: "standardized"; label: qsTr("Standardize raking weights (min weight = 1)"); defaultValue: true }
    }
}