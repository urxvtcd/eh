#!/usr/bin/env sh

runtest 2>&1 |
    sed \
        -e '/^expect: does "" (spawn_id/ s|^.*match glob pattern \(.*\)? [yesno]*\r|trying to match \x1b[33m\1\x1b[0m|' \
        -e '/^expect: does "[^"].* no\r/ s|^[^"]*\("[^"]*"\).*|\x1b[31m\1 no\x1b[0m|' \
        -e '/^expect: does "[^"].* yes\r/ s|^[^"]*\("[^"]*"\).*|\x1b[32m\1 yes\x1b[0m|' \
        -e '/^expect: set / d' \
        -e '/^send: sending / s|.*|\n&|' \
        -e '/^\r/ d'
