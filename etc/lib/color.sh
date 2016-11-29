#!/bin/bash

color256()
{
    # To make fg256() and bg256() be convenient,
    # when only one argument given, return it directly.
    if [ "$#" -eq 1 ]; then
        echo -n $1
        return
    fi

    local red=$1;
    local green=$2;
    local blue=$3;

    echo -n $[$red * 36 + $green * 6 + $blue + 16]
}

fg256()
{
    echo -n $'\e[38;5;'$(color256 "$@")"m"
}

bg256()
{
    echo -n $'\e[48;5;'$(color256 "$@")"m"
}

hashed_color() {
    local md5=$(
        (which md5sum || which md5 || \
                echo ""
        ) | tail -n 1)

    if [ -z "${md5}" ]; then
        return
    fi

    local bgval_hex=$(echo $1|${md5}|cut -c1-2)
    # `bgval` should be in range of 17-231
    # as 0-15 represent system color and 232-255 is grayscale pallete.

    # Restict range: 0-255 => 0-215.
    # This value is used later to determine foreground color.
    local bgval_clipped=$(printf "%d" "215*0x${bgval_hex}/255.0")

    # Bias range: 0-216 => 16-231
    local bgval=$(printf "%d" "${bgval_clipped}+16")

    # (color code), (value in bgval_clipped) => (foreground color)
    #
    # 16-33 (0-17) => white
    # 34-51 (18-35) => black
    # 52-69 (36-53) => white
    # ...
    # 196-213 (180-197) => white
    # 214-231 (198-215) => black
    #
    # Hence the foreground color can be represented as follows:
    #
    # ```
    # is_fg_black = (bgval_clipped / 18) % 2 ? true : false
    # ```
    #
    # See http://askubuntu.com/a/821163

    local is_fg_black=$(printf "%d" "(${bgval_clipped}/18)%2")
    if [ "${is_fg_black}" -eq 1 ]; then
        local fgval=232 # system color INDEPENDENT white.
    else
        local fgval=255 # system color INDEPENDENT black.
    fi

    echo $(fg256 ${fgval})$(bg256 ${bgval})
}

hostcolor() {
    hashed_color `hostname`
}

