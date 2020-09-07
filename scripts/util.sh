#!/usr/bin/env bash

red="\x1b[31m"
green="\x1b[32m"
yellow="\x1b[33m"
magenta="\x1b[35m"
cyan="\x1b[36m"
reset="\x1b[0m"

function print_header {
    echo -e "${magenta}# ${@}${reset}"
}

function color_coverage {
    while IFS= read line; do
        decolored_line="$(echo -e $line | sed -e "s/\x1B\[[0-9;]*[JKmsu]//g")"
        echo -e $line | grep --silent '[[:digit:]]%' &&
            echo -e "${yellow}${decolored_line}${reset}" ||
            echo -e "$line"
    done
}
