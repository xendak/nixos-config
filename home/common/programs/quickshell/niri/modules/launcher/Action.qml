import QtQuick

QtObject {
    required property string name
    required property string desc
    required property string icon

    function onClicked(list: AppList): void {
        // This function can be overridden by instances
    }
}
