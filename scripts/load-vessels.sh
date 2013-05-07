#!/usr/bin/env bash
# @(#) Driver script for the LoadVessels Smocs example program
#

# execute from the smocs-examples directory
cd $(dirname $0)/..

typeset -r BASE_DIR="$(cd ..; pwd)"

test -d smocs-examples/target/scala-2.10/classes || {
	echo "the project must be built before running ${0}" >&2
	exit 1
	}

sbt "project smocs-examples" \
	"run com.tubros.constraints.examples.cargo.LoadVessels $*"
