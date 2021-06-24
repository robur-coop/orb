#!/bin/sh -e

# only execute anything if either
# - running under orb with package = orb
# - not running under opam at all
if [ "$ORB_BUILDING_PACKAGE" != "orb" -a "$OPAM_PACKAGE_NAME" != "" ]; then
    exit 0;
fi

basedir=$(realpath "$(dirname "$0")"/../..)
bdir=$basedir/_build/install/default/bin
tmpd=$basedir/_build/stage
rootdir=$tmpd/rootdir
bindir=$rootdir/usr/bin
debiandir=$rootdir/DEBIAN

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$bindir" "$debiandir"

# stage app binaries
install $bdir/orb $bindir/orb
install $basedir/opam.sh $bindir/opam

# install debian metadata
install $basedir/packaging/debian/control $debiandir/control
install $basedir/packaging/debian/changelog $debiandir/changelog
install $basedir/packaging/debian/copyright $debiandir/copyright

dpkg-deb --build $rootdir $basedir/orb.deb
echo 'bin: [ "orb.deb" ]' > $basedir/orb.install
