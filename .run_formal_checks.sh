#!/usr/bin/env bash

# SPDX-License-Identifier: Apache-2.0

set -e

if [ $# -ne 1 ]; then
    echo "There must be exactly one argument!"
    exit -1
fi

DUT=$1

# Run formal check only for PRs
if [ $GITHUB_WORKFLOW = "pull_request" ]; then
    # See https://docs.github.com/en/actions/reference/environment-variables
    # for info about these variables
    # Travis does a shallow clone, checkout PR target so that we have it
    # THen return to previous branch so HEAD points to the commit we're testing
    git remote set-branches origin $GITHUB_BASE_REF && git fetch
    git checkout $GITHUB_BASE_REF
    git checkout -
    # Skip if '[skip formal checks]' shows up in any of the commit messages in the PR
    if git log --format=%B --no-merges $GITHUB_BASE_REF..HEAD | grep '\[skip formal checks\]'; then
        echo "Commit message says to skip formal checks"
        exit 0
    else
        cp regress/$DUT.fir $DUT.fir
        ./scripts/formal_equiv.sh HEAD $GITHUB_BASE_REF $DUT
    fi
else
    echo "Not a pull request, no formal check"
    exit 0
fi
