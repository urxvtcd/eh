#!/usr/bin/env bash

set -Eeuo pipefail

source ./scripts/util.sh

print_header "E2E TESTS"

function wsed {
    # wsed wraps sed's substitute into a command. Wins:
    #  - less repetition,
    #  - skipping quote escaping game,
    #  - easy vertical alignment.

    sed "/${1}/ s/${2}/${3}/"
}

# run tests and make output easier on the eyes
runtest 2>&1 \
    | wsed "^send: "           '.*\("[^"]*"\).*'         "\nsending ${cyan}\1 ${reset}" \
    | wsed '^expect: does ""'  '^.*pattern \(.*\)? .*\r' "trying to match ${cyan}\1${reset}" \
    | wsed '^expect: .* no\r'  '^[^"]*\("[^"]*"\).*'     "${yellow}\1 no${reset}" \
    | wsed '^expect: .* yes\r' '^[^"]*\("[^"]*"\).*'     "${green}\1 yes${reset}" \
    | wsed '^FAIL: '           '.*'                      "${red}&${reset}" \
    | sed -e '/^expect: set / d' \
    | sed -e '/^\r/           d'
