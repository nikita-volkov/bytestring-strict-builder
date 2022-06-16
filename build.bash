#!/bin/bash
set -eo pipefail

function format {
  ormolu --mode inplace -ce \
  $(find . -name "*.hs" \
    -not -path "./.git/*" \
    -not -path "./*.stack-work/*" \
    -not -path "./samples/*" \
    -not -path "./sketches/*" \
    -not -path "./output/*" \
    -not -path "./ideas/*" \
    -not -path "./refs/*" \
    -not -path "./temp/*")
}

function build_and_test {
  stack build \
  --fast --test \
  --ghc-options "-j +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns"
}

function build_and_run_test_suite {
  stack build \
  --fast --test \
  --ghc-options "-j +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns" \
  :$1
}

function build_and_test_by_pattern {
  stack build \
  --fast --test \
  --ghc-options "-j +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns" \
  --ta "-p \"$1\"" $2
}

function build {
  stack build \
  --ghc-options "-j +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns" \
  --fast
}

function build_haddock {
  stack haddock \
  --ghc-options "-j +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns" \
  --fast
}

function build_failing_on_incomplete_patterns {
  stack build \
  --ghc-options "-j +RTS -A128m -n2m -RTS -Werror=incomplete-patterns" \
  --fast
}

function install {
  stack \
  --work-dir ".install.stack-work" \
  install \
  --ghc-options "-j +RTS -A128m -n2m -RTS"
}

function install_forking {
  mkdir -p .build-logs
  install > .build-logs/install.stdout 2> .build-logs/install.stderr &
}

function build_haddock_forking {
  mkdir -p .build-logs
  build_haddock > .build-logs/build_haddock.stdout 2> .build-logs/build_haddock.stderr &
}

format

build_and_test
