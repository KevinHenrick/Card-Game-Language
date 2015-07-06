# Author: Hebo Yang
# Usage: test.bash <srcfile>
#        srcfile: the source file to be tested.
#        If no srcfile is specified, it will run all test cases.

#!/bin/bash

cgl="../_build/cgl"
testpath="./parser_test/"

#Run All test if no source file specified
if [ $# -lt 1 ]
then
	cd ../_build/;
	make;
	echo "Running all tests..."

else
	cd ../_build/;
	make;
	file=$1
	cd ../test_cgl;
	echo "Testing $file"
    $cgl -j $testpath$file
	javac Main.java CGLList.java Card.java Player.java
    filename=`expr $file : '\(^.*\)'[.]`
    java Main > $filename.result
	output=$filename.out
    diff -b $filename.result $testpath$output
	rm -rf *.java *.class $testpath/*.result *.result
	cd ../_build/;
	make clean
	echo "Cleaned intermediate files."
	exit
fi

if [ ! -e "$cgl" ]
then
	echo "CGL compiler not found!"
	exit
fi

cd ../test_cgl;
echo "Parser tests"
for file in $testpath*.cgl
do
	echo ""
	echo "Testing $file"
    $cgl -j $file
	javac Main.java CGLList.java Card.java Player.java
    filename=`expr $file : '\(^.*\)'[.]`
    java Main > $filename.result
	output=$filename.out
    diff -b $filename.result $output
	
done

rm -rf *.java *.class $testpath/*.result *.result
cd ../_build/;
make clean
echo "Cleaned intermediate files."
