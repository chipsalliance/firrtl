set -e
# Run formal check only for PRs
if [ $TRAVIS_PULL_REQUEST = "false" ]; then
    echo "Not a pull request, no formal check"
    exit 0
elif [[ $TRAVIS_COMMIT_MESSAGE == *"[skip formal checks]"* ]]; then
    echo "Commit message says to skip formal checks"
    exit 0
else
    # $TRAVIS_BRANCH is branch targeted by PR
    bash ./scripts/formal_equiv.sh HEAD $TRAVIS_BRANCH
fi
