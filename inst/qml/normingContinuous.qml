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
            name: "group"
            title: qsTr("Explanatory / grouping variable (e.g., age)")
            singleVariable: true
            allowedColumns: ["scale", "ordinal"]
        }

        AssignedVariablesList {
            name: "weights"
            title: qsTr("Weights (optional)")
            singleVariable: true
            allowedColumns: ["scale"]
        }
    }

    DropDown {
        name: "method"
        id: method
        label: qsTr("Modeling method")
        indexDefaultValue: 0
        values: [
            { label: qsTr("Distribution-Free (Taylor Polynomials)"),          value: "taylor" },
            { label: qsTr("Beta-Binomial (Item Counts / IRT)"),               value: "betabinomial" },
            { label: qsTr("Sinh-Arcsinh (SHASH — Continuous)"),               value: "shash" },
            { label: qsTr("Conway-Maxwell-Poisson (CMP — Speeded / Counts)"), value: "cmp" }
        ]
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

    Section {
        title: qsTr("Model Parameters")

        // --- Taylor Polynomials ---
        Group {
            title: qsTr("Taylor Polynomial Options")
            Layout.columnSpan: 2
            visible: method.value === "taylor"

            CheckBox { name: "descend"; label: qsTr("Invert ranking order (descending)") }
            IntegerField { name: "nGroups"; label: qsTr("Norm groups for continuous age (0 = auto)"); defaultValue: 0; min: 0; max: 50 }
            CheckBox { name: "equidistant"; label: qsTr("Equidistant group intervals") }
            IntegerField { name: "k"; id: kField; label: qsTr("Degree of location (k)"); defaultValue: 5; min: 1; max: 6 }
            IntegerField { name: "t"; id: tField; label: qsTr("Degree of explanatory variable (t)"); defaultValue: 3; min: 1; max: 6 }

            RadioButtonGroup {
                name: "selectionType"
                id: selectionType
                RadioButton { value: "automaticSelection"; label: qsTr("Automatic term selection"); checked: true }
                RadioButton { value: "manualSelection";    label: qsTr("Manual number of terms") }
            }

            IntegerField {
                name: "terms"
                label: qsTr("Number of terms")
                defaultValue: 4
                min: 1
                max: Math.max(1, kField.value * tField.value + kField.value + tField.value)
                enabled: selectionType.value === "manualSelection"
            }
        }

        // --- Beta-Binomial ---
        Group {
            title: qsTr("Beta-Binomial Options")
            Layout.columnSpan: 2
            visible: method.value === "betabinomial"

            IntegerField { name: "alpha"; label: qsTr("Degree of α (alpha)"); defaultValue: 3; min: 0; max: 5 }
            IntegerField { name: "beta"; label: qsTr("Degree of β (beta)"); defaultValue: 3; min: 0; max: 5 }
            IntegerField { name: "itemnumber"; label: qsTr("Number of items in test (0 = max observed)"); defaultValue: 0; min: 0 }
        }

        // --- SHASH ---
        Group {
            title: qsTr("Sinh-Arcsinh (SHASH) Options")
            Layout.columnSpan: 2
            visible: method.value === "shash"

            IntegerField { name: "muDegree"; label: qsTr("Degree of μ (location)"); defaultValue: 3; min: 0; max: 5 }
            IntegerField { name: "sigmaDegree"; label: qsTr("Degree of σ (scale)"); defaultValue: 2; min: 0; max: 5 }
            IntegerField { name: "epsilonDegree"; label: qsTr("Degree of ε (skewness)"); defaultValue: 2; min: 0; max: 5 }
            IntegerField { name: "deltaDegree"; label: qsTr("Degree of δ (tail weight)"); defaultValue: 1; min: 0; max: 5; enabled: !fixDelta.checked }
            CheckBox { name: "fixDelta"; id: fixDelta; label: qsTr("Fix tail weight δ to constant") }
            DoubleField { name: "deltaValue"; label: qsTr("Fixed δ value"); defaultValue: 1.0; min: 0.1; max: 10.0; enabled: fixDelta.checked }
        }

        // --- CMP ---
        Group {
            title: qsTr("Conway-Maxwell-Poisson (CMP) Options")
            Layout.columnSpan: 2
            visible: method.value === "cmp"

            IntegerField { name: "cmpMuDegree"; label: qsTr("Degree of μ (location)"); defaultValue: 3; min: 1; max: 5 }
            IntegerField { name: "cmpNuDegree"; label: qsTr("Degree of ν (dispersion)"); defaultValue: 2; min: 0; max: 5; enabled: !cmpFixNu.checked }
            CheckBox { name: "cmpFixNu"; id: cmpFixNu; label: qsTr("Fix dispersion ν (e.g., 1 = Poisson)") }
            DoubleField { name: "cmpNuValue"; label: qsTr("Fixed ν value"); defaultValue: 1.0; min: 0.05; max: 20.0; enabled: cmpFixNu.checked }
            IntegerField { name: "cmpMaxScore"; label: qsTr("Maximum score ceiling (0 = untruncated)"); defaultValue: 0; min: 0 }
        }
    }

    Section {
        title: qsTr("Output & Norm Table")

        CheckBox { name: "model"; label: qsTr("Model fit and parameter estimates"); checked: true }
        CheckBox { name: "plot";  label: qsTr("Percentile curves plot"); checked: true }

        Group {
            title: qsTr("Norm Table Specification")

            TextField {
                name: "normAge"
                label: qsTr("Value for norm table (e.g., age)")
                defaultValue: ""
                fieldWidth: 100
            }
            DoubleField { name: "range"; label: qsTr("Norm score range (± SD)"); defaultValue: 3.0; min: 0.5; max: 6.0 }
            DoubleField { name: "minRaw"; label: qsTr("Minimum raw score"); defaultValue: 0; enabled: method.value !== "betabinomial" }
            DoubleField { name: "maxRaw"; label: qsTr("Maximum raw score (0 = auto)"); defaultValue: 0; enabled: method.value !== "betabinomial" }
            DoubleField { name: "stepping"; label: qsTr("Step size"); defaultValue: 1.0; min: 0.01; enabled: method.value !== "betabinomial" }
        }

        Group {
            title: qsTr("Confidence Intervals")

            CheckBox { name: "includeCI"; id: includeCI; label: qsTr("Include confidence intervals") }
            DoubleField { name: "ciLevel"; label: qsTr("Confidence level"); defaultValue: 0.90; min: 0.50; max: 0.999; enabled: includeCI.checked }
            DoubleField { name: "reliability"; label: qsTr("Reliability coefficient"); defaultValue: 0.90; min: 0.01; max: 0.999; enabled: includeCI.checked }
        }
    }
}