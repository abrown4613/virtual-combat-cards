#!/bin/sh

if [ $# -ne 1 ]; then 
	echo "Usage:\n\t$0 <version-string>" 
	exit 1
fi

VERSION=$1
INSTALLZIP="target/vcc-$VERSION-release-full.zip" 
if [ ! -f $INSTALLZIP  ]; then
    echo "Must run mvn package and generate $INSTALLZIP before running this command"
    exit 1;
fi

PLISTFILE="../vcc-main/target/classes/external/Info.plist"
if [ ! -f "$PLISTFILE" ]; then
	echo "Cant find updated Info.plist at $PLISTFILE" 
	exit 1;
fi

# Targes
BUILDIN="target/MacImage-$VERSION"
APPBUNDLE="$BUILDIN/Virtual Combat Cards.app"
JAVATGT=$APPBUNDLE/Contents/Resources/Java
IMAGE="target/MacBundle-$VERSION.dmg"

# Remove prior build
rm -f $IMAGE
rm -fr "$BUILDIN"
if [ -d $BUILDIN ]; then
	echo "Failed to clean $BUILDIN"
	exit 1
fi

# Copy bundle and unzip
mkdir -p "$JAVATGT"
unzip -d "$JAVATGT" $INSTALLZIP
(cd "$JAVATGT" && unzip install.zip && rm install.zip)
cp "$PLISTFILE" "$APPBUNDLE/Contents"
cp src/main/macos/files/PkgInfo "$APPBUNDLE/Contents"
cp src/main/macos/Resources/* "$APPBUNDLE/Contents/Resources" 
cp src/main/macos/files/DS_Store $BUILDIN/.DS_Store
cp src/main/macos/files/App_Link "$BUILDIN/Install Here"
# Copy stub and resources
mkdir "$APPBUNDLE/Contents/MacOS"
cp /System/Library/Frameworks/JavaVM.framework/Versions/Current/Resources/MacOS/JavaApplicationStub "$APPBUNDLE/Contents/MacOS/JavaApplicationStub"
chmod 755 "$APPBUNDLE/Contents/MacOS/JavaApplicationStub"
/Developer/Tools/SetFile -a B "$APPBUNDLE"

#Make image
hdiutil create -volname "VCC Installer" -srcfolder $BUILDIN $IMAGE
hdiutil internet-enable -yes $IMAGE
