#!/bin/bash

# cd into the directory containing this script
cd "$(dirname "$0")"

# uncomment and complete the following calls to javac and java
# compiled class files should be placed in the out/ directory

javac src/Main.java -cp src -d out
java -cp out Main
