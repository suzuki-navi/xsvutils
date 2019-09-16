#!/bin/bash

if [ "$1" = "--complete" ]; then
    # $2 is bash or zsh
    shift
    $MULANG_SOURCE_DIR/main-jvm completion "$@"
else
    $MULANG_SOURCE_DIR/main-jvm parser "$@"
fi

