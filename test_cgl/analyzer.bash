# Author: Hebo Yang
# Usage: test.bash <testfile>
#        testfile: the test file to be tested.
#        If no srcfile is specified, it will run all test cases.

#!/bin/bash

cgl="../_build/cgl"
testpath="./analyzer_test/"

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
    $cgl -s $testpath$file
	echo "Semantic Tests done."
	cd ../_build/;
	make clean
	cd ../test_cgl/;
	rm -rf *.java *.class
	echo "Cleaned intermediate files."
	exit
fi

if [ ! -e "$cgl" ]
then
	echo "CGL compiler not found!"
	exit
fi

cd ../test_cgl;
echo "Semantic tests"
for file in $testpath*.cgl
do
	echo
	echo "Testing $file"
    $cgl -s $file
done

echo "Semantic Tests done."
cd ../_build/;
make clean
cd ../test_cgl/;
rm -rf *.java *.class
echo "Cleaned intermediate files."

