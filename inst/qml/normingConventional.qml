import QtQuick 2.12
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form {
    VariablesForm {
        AvailableVariablesList { name: "allVariables" }

        AssignedVariablesList {
            name: "raw"
            title: qsTr("Raw score variable")
            singleVariable: true
            allowedColumns: ["scale"]
        }

        AssignedVariablesList {
            name: "weights"
            title: qsTr("Weights (optional)")
            singleVariable: true
            allowedColumns: ["scale"]
        }
    }

    DropDown {
        name: "scale"
        label: qsTr("Norm scale")
        values: ["T", "IQ", "z", "Wechsler", "PISA"]
        titles: [
            qsTr("T (m = 50, sd = 10)"),
            qsTr("IQ (m = 100, sd = 15)"),
            qsTr("z (m = 0, sd = 1)"),
            qsTr("Wechsler (m = 10, sd = 3)"),
            qsTr("PISA (m = 500, sd = 100)")
        ]
        defaultValue: "T"
    }

    CheckBox { name: "descend"; label: qsTr("Invert ranking order (descending)"); defaultValue: false }

    Section {
        title: qsTr("Regression Smoothing")

        CheckBox { name: "k"; label: qsTr("Smooth with polynomial regression"); defaultValue: true }
        IntegerField { name: "kPower"; label: qsTr("Degree of location (k)"); defaultValue: 5; min: 1; max: 6; enabled: k.checked }

        RadioButtonGroup {
            name: "selectionType"
            enabled: k.checked
            RadioButton { value: "automaticSelection"; label: qsTr("Automatic term selection"); checked: true }
            RadioButton { value: "manualSelection"; label: qsTr("Manual number of terms") }
        }
        IntegerField {
            name: "terms"
            label: qsTr("Number of terms")
            defaultValue: 4
            min: 1
            max: 6
            enabled: k.checked && selectionType.value === "manualSelection"
        }
    }

    Section {
        title: qsTr("Output")

        DropDown {
            name: "plotting"
            label: qsTr("Result plot")
            values: ["Norm Score", "Percentile"]
            defaultValue: "Norm Score"
        }

        CheckBox { name: "model"; label: qsTr("Model fit and coefficients"); defaultValue: true; enabled: k.checked }

        Group {
            title: qsTr("Norm Score Table")

            DoubleField { name: "range"; label: qsTr("Norm table range (± SD)"); defaultValue: 3.0; min: 0.5; max: 6.0 }
            DoubleField { name: "minRaw"; label: qsTr("Minimum raw score"); defaultValue: 0; enabled: k.checked }
            DoubleField { name: "maxRaw"; label: qsTr("Maximum raw score (0 = auto)"); defaultValue: 0; enabled: k.checked }
            DoubleField { name: "stepping"; label: qsTr("Step size"); defaultValue: 1.0; min: 0.01; enabled: k.checked }
        }

        Group {
            title: qsTr("Confidence Intervals")

            CheckBox { name: "includeCI"; label: qsTr("Include confidence intervals"); defaultValue: false }
            DoubleField { name: "ciLevel"; label: qsTr("Confidence level"); defaultValue: 0.90; min: 0.50; max: 0.999; enabled: includeCI.checked }
            DoubleField { name: "reliability"; label: qsTr("Reliability coefficient"); defaultValue: 0.90; min: 0.01; max: 0.999; enabled: includeCI.checked }
        }
    }
}