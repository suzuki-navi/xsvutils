#!/bin/bash

#use MULANG_SOFT_WORKING_DIR

if [ -t 1 ] && which tput >/dev/null 2>&1; then
    export TERMINAL_LINES=$(tput lines 2>/dev/null);
    export TERMINAL_COLS=$(tput cols 2>/dev/null);
fi

if [ -t 1 ]; then
    $MULANG_SOURCE_DIR/main-scala "$@" | less -SRX
else
    $MULANG_SOURCE_DIR/main-scala "$@"
fi

