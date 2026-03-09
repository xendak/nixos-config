#!/bin/bash

MAC="40:9F:38:A4:61:46"

powered() {
    echo "show" | bluetoothctl | grep "Powered" | cut -d " " -f 2
}

connected() {
    echo "info ${MAC}" | bluetoothctl | grep "Connected" | cut -d " " -f 2
}

while true; do
    sleep 1
    if [ $(powered) = yes ] && [ $(connected) = no ]; then
        echo "connect ${MAC}" | bluetoothctl
        sleep 5
    elif [ $(powered) = yes ] && [ $(connected) = yes ]; then
        break
    fi
done

echo "Bluetooth connected and started? hopefuly"
