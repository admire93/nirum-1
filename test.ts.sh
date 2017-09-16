#!/usr/bin/env bash -e
PREFIX=./test/typescript

npm --prefix="$PREFIX" install
stack exec -- nirum -o test/typescript/nirum_fixture -t typescript test/nirum_fixture
npm --prefix="$PREFIX" test
