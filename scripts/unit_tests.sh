#!/usr/bin/env bash

source scripts/util.sh

print_header "UNIT TESTS"

unbuffer stack --no-terminal test --coverage | color_coverage
