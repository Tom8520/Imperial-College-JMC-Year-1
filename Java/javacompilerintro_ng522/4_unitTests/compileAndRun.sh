#!/bin/bash

# cd into the directory containing this script
cd "$(dirname "$0")"

# uncomment and complete the following calls to javac and java
# libraries for unit tests can be found in the testLib/ directory
# compiled class files should be placed in the testOut/ directory
# for the java call you only need to fill in the classpath correctly

javac test/SquareTest.java -cp lib/junit4.jar:src -d out
javac src/Square.java -d out
java -cp lib/hamcrest-core-1.3.jar:lib/junit4.jar:out org.junit.runner.JUnitCore SquareTest
