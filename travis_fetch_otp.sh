#!/usr/bin/env bash
#
# We expect TRAVIS_OS_NAME and OTP_RELEASE to be set before this is invoked.
# Afterwards, you should source $HOME/otp/$TRAVIS_OTP_RELEASE/activate.

set -eu -o pipefail

osx() { [[ "$TRAVIS_OS_NAME" == "osx" ]]; }
linux() { [[ "$TRAVIS_OS_NAME" == "linux" ]]; }
if osx; then brew update && brew install gcc5; fi

if [ -e "$HOME/otp/$TRAVIS_OTP_RELEASE/activate" ]; then exit 0; fi

mkdir -p ~/otp
cd ~/otp

if linux; then
    wget https://s3.amazonaws.com/travis-otp-releases/binaries/$(lsb_release -is | tr "A-Z" "a-z")/$(lsb_release -rs)/$(uname -m)/erlang-${OTP_RELEASE}-nonroot.tar.bz2
    tar xjf erlang-${TRAVIS_OTP_RELEASE}-nonroot.tar.bz2
fi

if osx; then
    curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
    chmod a+x kerl
    ./kerl build $TRAVIS_OTP_RELEASE $TRAVIS_OTP_RELEASE
    ./kerl install $TRAVIS_OTP_RELEASE ~/otp/$TRAVIS_OTP_RELEASE
fi
