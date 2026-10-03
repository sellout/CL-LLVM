#!/bin/sh

set -eu

cd "$(dirname "$0")"

autoreconf -fi
./configure
make
