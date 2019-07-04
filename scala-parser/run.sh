#!/bin/bash
set -e

cd "$(dirname "$0")"
mkdir -p var/class_file
make -s var/class_file/Main.class
scala -cp var/class_file Main "$@"
