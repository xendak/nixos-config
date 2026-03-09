#!/bin/bash

# Check if correct number of arguments are provided
if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <input> <order> <colors>"
    exit 1
fi

order=$2 #"./order.txt"
colors=$3 #"./colors.txt"
input=$1
output="changed.$(echo "$1" | sed 's/.*\.//')"

# Read order mappings into an associative array
declare -A order_map
while IFS='=' read -r key value; do
    key=$(echo "$key" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    value=$(echo "$value" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    order_map[$key]=$value
done < "$order"

# Read colors mappings into an associative array
declare -A colors_map
while IFS='=' read -r key value; do
    key=$(echo "$key" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    value=$(echo "$value" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    colors_map[$key]=$value
done < "$colors"

# Process the input real file and replace expressions
while IFS= read -r line; do
    for key in "${!order_map[@]}"; do
        color_key="${order_map[$key]}"
        color=${colors_map[$color_key]}
        line=${line//$key/$color}
    done
    echo "$line"
done < "$input" > "$output"

echo "Output file generated: $output"

