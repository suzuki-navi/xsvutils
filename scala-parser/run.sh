#!/bin/bash
set -e

cd "$(dirname "$0")"
mkdir -p var/class_file
make -s var/class_file/Main.class
bash ./var/anylang.sh --jdk=11 --scala=2.13.0 scala -cp var/class_file Main "$@"
