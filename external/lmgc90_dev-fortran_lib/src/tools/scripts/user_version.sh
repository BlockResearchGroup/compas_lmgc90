#!/bin/bash
set -e -x

if [ $# -ne 1 ]; then
  echo 'Usage: $0 tag' >&2 
  exit 1
fi

ROOT=$PWD

#export as a simple directory not versionned
git checkout-index -a -f --prefix=$ROOT/lmgc90_user_$1/

#compile (to get the sphinx doc)
LMGC=$ROOT/lmgc90_user_$1

cd $LMGC
rm -rf $LMGC/src/obsolete
rm -rf $LMGC/src/Sandbox/Astro
rm -rf $LMGC/src/Sandbox/BindingPeligriff
rm -rf $LMGC/src/Sandbox/BindingProjection
rm -rf $LMGC/src/Sandbox/Cellule
rm -rf $LMGC/src/Sandbox/Ddm
rm -rf $LMGC/src/Sandbox/GGC
rm -rf $LMGC/src/Sandbox/Post

mkdir $LMGC/build
cd $LMGC/build
cmake $LMGC -DWITH_HDF5=ON -DBUILD_STANDALONE=ON
make -j4 > make.log 2>&1
make docs > make_docs.log 2>&1

cp -r docs $ROOT/public
mv docs $LMGC/
rm -rf *

cd $ROOT

# tarring the empty version for *nix
zip -q -r lmgc90_user_$1.zip lmgc90_user_$1

