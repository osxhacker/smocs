#!/usr/bin/env bash
# @(#) Driver script for the Sudoku Smocs example program
#

# execute from the smocs-examples directory
cd $(dirname $0)/..

typeset -r BASE_DIR="$PWD"

test -d smocs-examples/target/scala-2.10/classes || {
	echo "the project must be built before running ${0}" >&2
	exit 1
	}

if [[ -z "${PROFILE_SMOCS:-}" ]]
then
	PROFILE_SMOCS=""
elif [[ "${PROFILE_SMOCS:-}" == "visualvm" ]]
then
	PROFILE_SMOCS="-Dcom.sun.management.jmxremote=true -Dcom.sun.management.jmxremote.port=20000 -Dcom.sun.management.jmxremote.ssl=false -Dcom.sun.management.jmxremote.authenticate=false"
else
	PROFILE_SMOCS="-Xprof"
fi

export JAVA_OPTS="-Xmx2048M -XX:MaxPermSize=512M -Xss2M -XX:ReservedCodeCacheSize=64m -XX:+UseCodeCacheFlushing -XX:+UseConcMarkSweepGC -Duser.timezone=GMT"

sbt "project smocs-examples" start-script && \
	env JAVA_OPTS="$JAVA_OPTS $PROFILE_SMOCS" \
	bash ./smocs-examples/target/start \
	com.tubros.constraints.examples.sudoku.SolvePuzzle \
	$*

