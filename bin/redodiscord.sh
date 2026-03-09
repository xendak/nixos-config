#!/bin/sh

if pgrep -x "discordcanary" > /dev/null
then
    echo "killing discord process"
    pkill -9 discordcanary
else
    echo "Process was not running"
fi

cat ~/.config/discordcanary/settings.json > ~/.config/discordcanary/t.json
rm -rf ~/.config/discordcanary/settings.json
discordcanary &
wait
mv ~/.config/discordcanary/t.json ~/.config/discordcanary/settings.json
echo "done"
