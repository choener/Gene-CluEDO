#!/usr/bin/env bash

# prepare release files
P=`pwd`
B=`basename ${P}`
D="build/$B"
rm -fr build
mkdir -p $D
# copy executable(s)
cp `find dist-newstyle -executable -type f -name GeneCluEDO` $D
# copy data
cp -r data $D
# prepare tar.gz
cd build
tar czf $B.tgz $B

