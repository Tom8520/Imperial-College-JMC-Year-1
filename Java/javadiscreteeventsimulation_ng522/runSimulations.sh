rm -rf out
mkdir out
javac -g -d out -classpath out -sourcepath src src/**/*.java

echo "Print3 Simulation"
echo "-----------------"
java -ea -cp out print3.Print3

echo
echo "Ticks Simulation"
echo "----------------"
java -ea -cp out ticks.Ticks 10

echo
echo "Single Server Queue Simulation"
echo "------------------------------"
java -ea -cp out ssq.SingleServerQueue 1987281099 4.0
