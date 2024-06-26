#! /bin/bash

CONFIG="$HOME/tmp/config.kv"
SAVE='./out'
TEMPLATE="$HOME/Flake/home/common/programs/kvantum/patcher/template"
HELP=0

while [ True ]; do
if [ "$1" = "--config" -o "$1" = "-c" ]; then
    CONFIG=$2
    shift 2
elif [ "$1" = "--save" -o "$1" = "-s" ]; then
    SAVE=$2
    shift 2
elif [ "$1" = "--template" -o "$1" = "-t" ]; then
    TEMPLATE=$2
    shift 2
elif [ "$1" = "--help" -o "$1" = "-h" ]; then
    HELP=1
    shift 1
else
    break
fi
done

#ARG=( "${@}" )

#for i in ${ARG[@]}; do
#    echo $i
#done

if [[ $HELP = 1 ]]; then
    echo '
Usage:
    patch.sh [options]

    -h, --help
        show this help message

    -c path, --config=path (default="./example.patchconfig")
        path to patchconfig file

    -s path, --save=path (default="./patchedKvantum")
        path where to save the patched Kvantum theme folder

    -t path, --template=path (default=".")
        path to the template Kvantum theme folder

Patchconfig file syntax:
    # Colors in hex format. Should not be empty
    background=#2e3440
    foreground=#eceff4
    accent=#5e81ac
    negative=#bf616a
    button=#4c566a
    inactive=#d8dee9
    link=#81a1c1
    visitedlink=#8fbcbb

    save_folder=path/to/folder/to/save

    # Details. "name" should not be empty
    name=general-name-of-color-scheme
    author="Name of author to put in kvconfig"
    comment="This comment will be put in kvconfig"
    '
    exit 0 
fi

get_base_name() {
  echo "$(basename "$(cd  "$1" && pwd)")"
}

templateName="template"

if [[ -f "$CONFIG" ]]; then
    . $CONFIG
else
    echo error: $CONFIG is not a file
    exit 1
fi

if [[ -f "$TEMPLATE/$templateName.info" &&  -f "$TEMPLATE/$templateName.svg" && -f "$TEMPLATE/$templateName.kvconfig" ]]; then
    . $TEMPLATE/$templateName.info
else
    echo error: $TEMPLATE is not a valid template folder
    exit 2
fi

if ! [[ $background =~ ^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$ &&
        $foreground =~ ^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$ &&
        $accent =~ ^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$ &&
        $negative =~ ^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$ &&
        $button =~ ^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$ &&
        $inactive =~ ^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$ &&
        $link =~ ^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$ &&
        $visitedlink =~ ^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$ &&
        -n "$name" &&
        -n "$save_folder"
    ]] ; then
    echo error: $CONFIG is not a valid patchconfig. To get the syntax of a valid patchconfig, see the help message \(patch.sh -h\)
    exit 3
fi

mkdir -p $save_folder/$finalName/

hex_to_dec() {
    printf "%d" "0x$1"
}

# Function to convert decimal to hex
dec_to_hex() {
    printf "%02X" "$1"
}

# Function to adjust color component
adjust_color() {
    local color=$1
    local adjustBy=$2
    if (( color <= $adjustBy )); then
        (( color -= $adjustBy ))
    else
        (( color += $adjustBy ))
    fi
    echo $color
}

# Function to process the color
process_color() {
    local color=$1
    local adjustBy=$2

    local r=$(hex_to_dec "${color:1:2}")
    local g=$(hex_to_dec "${color:3:2}")
    local b=$(hex_to_dec "${color:5:2}")

    r=$(adjust_color $r $adjustBy)
    g=$(adjust_color $g $adjustBy)
    b=$(adjust_color $b $adjustBy)

    printf "#%s%s%s" $(dec_to_hex $r) $(dec_to_hex $g) $(dec_to_hex $b)
}

# Get initial color from the first argument
initialColor=$bg1

# Get adjust_by value from the second argument
adjustBy=$bgalt

# Process the initial color
newColor=$(process_color $bg1 $bgalt)
newButton=$(process_color $button 8)


sed "
    s/#000000/$background/
    s/#101010/$bg1/
    s/__bgalt__/$newColor/
    s/#ffffff/$foreground/
    s/#0000ff/$accent/
    s/#ff0000/$negative/
    s/#444444/$newButton/
    s/#888888/$inactive/
    " $TEMPLATE/$templateName.svg > $save_folder/$finalName/$finalName.svg

sed "
    s/author_name/$author/
    s/comment_line/$comment/
    s/#101010/$bg1/
    s/__bgalt__/$newColor/
    s/#000000/$background/
    s/#ffffff/$foreground/
    s/#0000ff/$accent/
    s/#ff0000/$negative/
    s/#444444/$newButton/
    s/#888888/$inactive/
    s/#8888ff/$link/
    s/#4444ff/$visitedlink/
    " $TEMPLATE/$templateName.kvconfig > $save_folder/$finalName/$finalName.kvconfig

echo success: Patched Kvantum theme saved in $save_folder/$finalName/
