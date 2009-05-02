#!/bin/sh
#$Id$

OS=`uname` 
if [ $OS = 'Darwin' ]; then
	echo "Running on a MAC"
	ARGS="-Dvcc.view.lpanel.width=460 -Dvcc.view.efp.max=2"
	JAVA="/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Home/bin/java"
	if [ ! -f $JAVA ]; then
		echo "Can't find a suitable JRE, need version 1.6.0" 
		exit 1
	fi
else
	echo "Running on other Unix"
	JAVA=java
fi
CLASSPATH="bin/scala-swing.jar:bin/scala-library.jar:bin/miglayout-3.6.2-swing.jar:bin/vcc.jar"
$JAVA -cp $CLASSPATH $ARGS vcc.Main
