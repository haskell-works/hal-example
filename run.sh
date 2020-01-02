#!/bin/bash -e

export AWS_LAMBDA_RUNTIME_API=2

cabal v2-run exe:hal-example
