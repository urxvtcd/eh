#!/usr/bin/env bash

source scripts/util.sh

print_header "BUILD"

unbuffer stack --no-terminal build --coverage --haddock 2>&1  | color_coverage
