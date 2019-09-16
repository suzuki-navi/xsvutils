#!/bin/bash

if [ "$1" = "--complete" ]; then
    # $2 is "bash" or "zsh"
    shift
    $MULANG_SOURCE_DIR/main-jvm completion "$@"
else
    if [ -t 1 ]; then
        if which tput >/dev/null 2>&1; then
            export TERMINAL_LINES=$(tput lines 2>/dev/null);
            export TERMINAL_COLS=$(tput cols 2>/dev/null);
        fi
        $MULANG_SOURCE_DIR/main-jvm parser "$@" | less -SRX
    else
        $MULANG_SOURCE_DIR/main-jvm parser "$@"
    fi
fi

