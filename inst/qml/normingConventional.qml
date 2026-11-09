import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

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
        indexDefaultValue: 0
        values: [
            { label: qsTr("T (m = 50, sd = 10)"),       value: "T" },
            { label: qsTr("IQ (m = 100, sd = 15)"),     value: "IQ" },
            { label: qsTr("z (m = 0, sd = 1)"),         value: "z" },
            { label: qsTr("Wechsler (m = 10, sd = 3)"), value: "Wechsler" },
            { label: qsTr("PISA (m = 500, sd = 100)"),  value: "PISA" }
        ]
    }

    CheckBox { name: "descend"; label: qsTr("Invert ranking order (descending)") }

    Section {
        title: qsTr("Regression Smoothing")

        CheckBox { name: "k"; id: kSmooth; label: qsTr("Smooth with polynomial regression"); checked: true }
        IntegerField { name: "kPower"; label: qsTr("Degree of location (k)"); defaultValue: 5; min: 1; max: 6; enabled: kSmooth.checked }

        RadioButtonGroup {
            name: "selectionType"
            id: selectionType
            enabled: kSmooth.checked
            RadioButton { value: "automaticSelection"; label: qsTr("Automatic term selection"); checked: true }
            RadioButton { value: "manualSelection";    label: qsTr("Manual number of terms") }
        }

        IntegerField {
            name: "terms"
            label: qsTr("Number of terms")
            defaultValue: 4
            min: 1
            max: 6
            enabled: kSmooth.checked && selectionType.value === "manualSelection"
        }
    }

    Section {
        title: qsTr("Output")

        DropDown {
            name: "plotting"
            label: qsTr("Result plot")
            indexDefaultValue: 0
            values: [
                { label: qsTr("Norm score"), value: "Norm Score" },
                { label: qsTr("Percentile"), value: "Percentile" }
            ]
        }

        CheckBox { name: "model"; label: qsTr("Model fit and coefficients"); checked: true; enabled: kSmooth.checked }

        Group {
            title: qsTr("Norm Score Table")

            DoubleField { name: "range"; label: qsTr("Norm table range (± SD)"); defaultValue: 3.0; min: 0.5; max: 6.0 }
            DoubleField { name: "minRaw"; label: qsTr("Minimum raw score"); defaultValue: 0; enabled: kSmooth.checked }
            DoubleField { name: "maxRaw"; label: qsTr("Maximum raw score (0 = auto)"); defaultValue: 0; enabled: kSmooth.checked }
            DoubleField { name: "stepping"; label: qsTr("Step size"); defaultValue: 1.0; min: 0.01; enabled: kSmooth.checked }
        }

        Group {
            title: qsTr("Confidence Intervals")

            CheckBox { name: "includeCI"; id: includeCI; label: qsTr("Include confidence intervals") }
            DoubleField { name: "ciLevel"; label: qsTr("Confidence level"); defaultValue: 0.90; min: 0.50; max: 0.999; enabled: includeCI.checked }
            DoubleField { name: "reliability"; label: qsTr("Reliability coefficient"); defaultValue: 0.90; min: 0.01; max: 0.999; enabled: includeCI.checked }
        }
    }
}
