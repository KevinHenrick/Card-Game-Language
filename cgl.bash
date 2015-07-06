# Author: Hebo Yang
# Usage: cgl.bash <somegame.cgl>
#        somegame.cgl: the game written in cgl
#        If no file is specified, it will just run make.
#!/bin/bash
cgl="./_build/cgl"
gamepath="./games/"

if [ $# -lt 1 ]
then
	echo "No file input, just run Makefile in _build"
	cd _build;
	make
	echo "Make Succeed!"

elif [ "$1" = "clean" ]
then
	cd ./_build
    rm -rf *.cmo *.cmi *.java *.class cgl parser.mli parser.ml scanner.ml
    exit

else
	file=$1
	cd _build;
	make;
	cd ../;
	$cgl -j $gamepath$file
	javac Main.java CGLList.java Card.java Player.java
	cd _build;
	make clean;
	cd ../;
	clear
	java Main
	rm -rf *.java *.class
	echo "Finished Game, cleaned up intermediate files"
fi





