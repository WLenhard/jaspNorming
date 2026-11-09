import QtQuick
import JASP.Module

Description {
    name:         "jaspNorming"
    title:        qsTr("Norming")
    description:  qsTr("Continuous and Conventional Norming using cNORM")
    version:      "3.7.0"
    author:       "Wolfgang Lenhard & Alexandra Lenhard"
    maintainer:   "Wolfgang Lenhard <wolfgang.lenhard@uni-wuerzburg.de>"
    website:      "https://www.psychometrica.de/cNorm_jamovi_en.html"
    license:      "GPL (>= 3)"
    icon:         "norming.svg"
    requiresData: true

    GroupTitle { title: qsTr("Continuous Norming") }

    Analysis {
        title: qsTr("Continuous Norming")
        menu:  qsTr("Continuous Norming")
        func:  "normingContinuous"
        qml:   "normingContinuous.qml"
    }

    GroupTitle { title: qsTr("Conventional Norming") }

    Analysis {
        title: qsTr("Single Group Norming")
        menu:  qsTr("Single Group Norming")
        func:  "normingConventional"
        qml:   "normingConventional.qml"
    }

    GroupTitle { title: qsTr("Data Preparation") }

    Analysis {
        title: qsTr("Data Preparation (Age Groups & Raking)")
        menu:  qsTr("Data Preparation")
        func:  "normingPreparation"
        qml:   "normingPreparation.qml"
    }
}