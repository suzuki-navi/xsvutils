#!/bin/bash
set -e

cd "$(dirname "$0")"
make
./scala-parser main "$@"

