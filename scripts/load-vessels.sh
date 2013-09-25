#!/usr/bin/env bash
# @(#) Driver script for the LoadVessels Smocs example program
#

# execute from the smocs-examples directory
cd $(dirname $0)/..

typeset -r BASE_DIR="$PWD"

test -d smocs-examples/target/scala-2.10/classes || {
	echo "the project must be built before running ${0}" >&2
	exit 1
	}

export JAVA_OPTS="-Xmx2048M -XX:MaxPermSize=512M -Xss2M -XX:ReservedCodeCacheSize=64m -XX:+UseCodeCacheFlushing -XX:+UseConcMarkSweepGC -Duser.timezone=GMT"

sbt "project smocs-examples" start-script && \
	./smocs-examples/target/start \
	com.tubros.constraints.examples.cargo.LoadVessels \
	$*
