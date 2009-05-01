#!/bin/sh
#$Id$

OS=`uname` 
if [ $OS == 'Darwin' ]; then
	echo "Running on a MAC"
	JAVA="/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Home/bin/java"
	if [ ! -f $JAVA ]; then
		echo "Can't find a suitable JRE, need version 1.6.0" 
		exit 1
	fi
else
	echo "Running on other Unix"
	JAVA=java
fi
$JAVA -cp "bin/scala-swing.jar:bin/scala-library.jar:bin/miglayout-3.6.2-swing.jar:bin/vcc.jar" vcc.Main
