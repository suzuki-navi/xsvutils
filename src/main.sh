#!/bin/bash

if [ "$1" = "--complete" ]; then
    # $2 is "bash" or "zsh"
    shift
    $MULANG_SOURCE_DIR/main-jvm completion "$@"
else
    if [ -t 0 ]; then
        export XSVUTILS_INPUT_TTY=1
    else
        export XSVUTILS_INPUT_TTY=
    fi
    if [ -t 1 ]; then
        export XSVUTILS_OUTPUT_TTY=1
        if which tput >/dev/null 2>&1; then
            export XSVUTILS_TERMINAL_LINES=$(tput lines 2>/dev/null);
            export XSVUTILS_TERMINAL_COLS=$(tput cols 2>/dev/null);
        fi
        $MULANG_SOURCE_DIR/main-jvm parser "$@" | less -SRX
    else
        export XSVUTILS_OUTPUT_TTY=
        $MULANG_SOURCE_DIR/main-jvm parser "$@"
    fi
fi

