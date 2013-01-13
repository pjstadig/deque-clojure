#!/bin/bash
lein compile
java -Xms50m -Xmx50m -agentlib:hprof=cpu=samples,depth=8 -cp target/classes:/home/paul/.m2/repository/org/clojure/clojure/1.4.0/clojure-1.4.0.jar name.stadig.deque
