#!/bin/bash
lein compile
JVM_OPTS="-Xms50m -Xmx50m -agentlib:hprof=cpu=samples,depth=8" lein trampoline run -m name.stadig.test.deque
