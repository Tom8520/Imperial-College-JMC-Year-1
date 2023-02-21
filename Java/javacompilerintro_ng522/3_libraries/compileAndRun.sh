#!/bin/bash

# cd into the directory containing this script
cd "$(dirname "$0")"

# uncomment and complete the following calls to javac and java
# required code libraries can be found in the lib/ directory
# compiled class files should be placed in the out/ directory

# javac ...
# java ...

javac src/Main.java -cp lib/utils.jar -d out
java -cp out:lib/utils.jar Main
