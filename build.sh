#!/usr/bin/env bash
set -e

GIT=$(command -v git)
STACK=$(command -v stack)
MAKE=$(command -v make)

if [ ! -x "$GIT" ]; then
        echo "git: required but not found in path"
        exit 1
fi

if [ ! -x "$STACK" ]; then
        echo "stack: required not found in path"
        echo "Information about Haskell Stack at http://haskellstack.org"
        exit 1
fi

if [ ! -x "$MAKE" ]; then
        echo "make: required not found in path"
        exit 1
fi

$GIT submodule update --init
$STACK setup
$STACK build
(cd galua-c; $MAKE)
