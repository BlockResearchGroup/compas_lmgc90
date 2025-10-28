#!/bin/bash
set -e -x

## Author : Jonathan Lambrecht
## Date : May 19th 2015
## Use : Must set the three first variables and move the script before running it !

###############################################
###############################################
##                                           ##
## cross compile for windows using mingw-w64 ##
## on Ubuntu 18.04                           ##
##                                           ##
###############################################
###############################################

################
# requirements #
################

# mingw-w64
# gfortran-mingw-w64
# g++-mingw-w64
# gendef (mingw-w64-tools)
# wine64
# 7z (p7zip-full package on debian)
# cmake
# wget
# swig
# make
# tar
# sed
# unzip
# zip
#

# check https://https://git-xen.lmgc.univ-montp2.fr/lmgc90/lmgc90_dev/wikis/user_version
# to help with using/modifying the script
#sudo apt install mingw-w64 gfortran-mingw-w64 g++-mingw-w64 mingw-w64-tools p7zip-full

#############
# variables #
#############

#ARCH=i686
ARCH=x86_64

ROOT=$PWD
# TO SET
LMGCSRC=$1

# variables that work for Ubuntu 18.04.2 LTS
# storing path to fortran/math/g++ libraries and so on
MINGWLIBDIR=/usr/lib/gcc/$ARCH-w64-mingw32/7.3-win32
STDCPPLIB=$MINGWLIBDIR/libstdc++-6.dll
if [ $ARCH == x86_64 ]; then
  GCCLIB=$MINGWLIBDIR/libgcc_s_seh-1.dll
else
  GCCLIB=$MINGWLIBDIR/libgcc_s_sjlj-1.dll
fi
GFORTRANLIB=$MINGWLIBDIR/libgfortran-4.dll
#QUADMATHLIB=$MINGWLIBDIR/libquadmath-0.dll

DLLTOOL=$ARCH-w64-mingw32-dlltool
#LIBWINPTHREAD=/usr/$ARCH-w64-mingw32/lib/libwinpthread-1.dll

#cmake executable path in case of recent version needed
CMAKE=/usr/bin/cmake
#CMAKE=/home/mozul/WORK/SOFTS/deps/install/bin/cmake

mkdir -p $ROOT
mkdir -p $ROOT/lib
mkdir -p $ROOT/include
mkdir -p $ROOT/sources
mkdir -p $ROOT/builds
mkdir -p $ROOT/install

########################
# cmake toolchain file #
########################

echo "SET(CMAKE_SYSTEM_NAME Windows)
SET(CMAKE_C_COMPILER $ARCH-w64-mingw32-gcc)
SET(CMAKE_CXX_COMPILER $ARCH-w64-mingw32-g++)
SET(CMAKE_RC_COMPILER $ARCH-w64-mingw32-windres)
SET(CMAKE_Fortran_COMPILER $ARCH-w64-mingw32-gfortran)
SET(CMAKE_FIND_ROOT_PATH $ROOT)
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_CROSSCOMPILING_EMULATOR wine64)" > $ROOT/sources/cmake-mingw64-$ARCH

############
# openblas #
############

#OPENBLAS_VERSION=0.3.5
# problem with openblas at time of writing
# the most recent version do not provide
# binaries only source code which must be
# compiled for a TARGET architecture of cpu (HASWELL, SANDYBRIDGE, ...)
# so we are back to this one
OPENBLAS_VERSION=0.2.19
OPENBLAS_NAME=OpenBLAS-v$OPENBLAS_VERSION-Win64-int32


#getting the sources
mkdir -p $ROOT/sources/openblas
cd $ROOT/sources/openblas
if [ ! -e $OPENBLAS_NAME.zip ]; then
  wget -nv https://sourceforge.net/projects/openblas/files/v$OPENBLAS_VERSION/$OPENBLAS_NAME.zip
fi

unzip -o $OPENBLAS_NAME.zip
mv $OPENBLAS_NAME/lib/libopenblas.a $ROOT/lib


#############################
# python + numpy (anaconda) #
#############################

ANACONDA_VERSION=2018.12
ANACONDA_PATH=https://repo.continuum.io/archive
ANACONDA_NAME=Anaconda3-$ANACONDA_VERSION-Windows-$ARCH.exe

# can get this by doing a 7z l ANACONDA_NAME | grep python
ANACONDA_PYTHON=python-3.7.1-h8c8aaf0_6.tar.bz2
#Python Version No Dot
PY_VER=3.7
PY_VER_ND=37

# can get this by doing a 7z l ANACONDA_NAME | grep numpy
NP_VER=1.15.4
ANACONDA_NUMPY=numpy-base-1.15.4-py37hc3f5095_0.tar.bz2

mkdir -p $ROOT/sources/anaconda
cd $ROOT/sources/anaconda
if [ ! -e $ANACONDA_NAME ]; then
  wget -nv $ANACONDA_PATH/$ANACONDA_NAME
fi

if [ ! -e $ANACONDA_PYTHON ]; then
  7z -y e $ANACONDA_NAME pkgs/$ANACONDA_PYTHON
fi

tar xjf $ANACONDA_PYTHON python$PY_VER_ND.dll include
gendef python$PY_VER_ND.dll
$DLLTOOL -dpython$PY_VER_ND.def -l $ROOT/lib/libpython$PY_VER_ND.a -D python$PY_VER_ND.dll
rm -rf $ROOT/include/python$PY_VER
mv include $ROOT/include/python$PY_VER

if [ ! -e $ANACONDA_NUMPY ]; then
  7z -y e $ANACONDA_NAME pkgs/$ANACONDA_NUMPY
fi 

rm -rf $ROOT/include/numpy
tar xjf $ANACONDA_NUMPY Lib/site-packages/numpy/core/include/numpy
mv Lib/site-packages/numpy/core/include/numpy $ROOT/include/

#########
# metis #
#########

#doe not work yet
#METIS_PATH=http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis
#METIS_VERSION=5.1.0
METIS_PATH=http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/OLD
METIS_VERSION=4.0.3
METIS_NAME=metis-$METIS_VERSION
METIS_FILE=$METIS_NAME.tar.gz
mkdir -p $ROOT/sources/metis
cd $ROOT/sources/metis
if [ ! -e $METIS_FILE ]; then
  wget -nv $METIS_PATH/$METIS_FILE
fi
tar xf $METIS_FILE
if [ $METIS_VERSION == 5.1.0 ]; then
  cd $METIS_NAME/build
  # this the cmake line to use with version 5 and later
  $CMAKE .. -DCMAKE_TOOLCHAIN_FILE=$ROOT/sources/cmake-mingw64-$ARCH -DCMAKE_INSTALL_PREFIX=$ROOT/install -DSHARED=0 -DGKLIB_PATH=../GKlib
  #-DOPENMP=ON ... not yet
  make install
else
  # this is for version 4.0.3 and older...
  cd $METIS_NAME
  make -C Lib -j4 CC=$ARCH-w64-mingw32-gcc AR="$ARCH-w64-mingw32-ar r" RANLIB=$ARCH-w64-mingw32-ranlib COPTIONS="-D__VC__"
  cp libmetis.a $ROOT/lib
  cp Lib/metis.h $ROOT/include
fi

##########
#  HDF5  #
##########


HDF5_VERSION_MAJOR=1.10
HDF5_VERSION=1.10.4
HDF5_NAME=hdf5-$HDF5_VERSION
HDF5_FILE=$HDF5_NAME.tar.bz2
HDF5_PATH=https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-$HDF5_VERSION_MAJOR/$HDF5_NAME/src
mkdir -p $ROOT/sources/hdf5
cd $ROOT/sources/hdf5
if [ ! -e $HDF5_FILE ]; then
  wget -nv $HDF5_PATH/$HDF5_FILE
fi
tar xjf $HDF5_FILE
#need to patch sources first (src/H5win32defs.h)
sed -i -e "s/, \(__VA_ARGS__\)/, ##\1/" $HDF5_NAME/src/H5win32defs.h

cd $ROOT
mkdir -p $ROOT/builds/hdf5
cd $ROOT/builds/hdf5
$CMAKE ../../sources/hdf5/$HDF5_NAME -DCMAKE_TOOLCHAIN_FILE=$ROOT/sources/cmake-mingw64-$ARCH -DCMAKE_INSTALL_PREFIX=$ROOT/install -DHDF5_BUILD_FORTRAN=ON -DBUILD_TESTING=OFF -DHDF5_BUILD_TOOLS=OFF > $ROOT/hdf5_cmake.log 2> $ROOT/hdf5_cmake.err
##need to do some patching in build here
find . -name "build.make" -exec sed -i -e 's/wine64\//wine64 \//' {} \;
find . -name "build.make" -exec sed -i -e 's/\(^.* && \)\(.*\.exe$\)/\1 wine64 \2/' {} \;
make VERBOSE=1 > $ROOT/hdf5_make.log 2> $ROOT/hdf5_make.err
make install > $ROOT/hdf5_install.log 2> $ROOT/hdf5_install.err


##########
# lmgc90 #
##########

mkdir -p $ROOT/builds/lmgc90
cd $ROOT/builds/lmgc90

# specify some flags for c++ build
# the hypot thing is probably due to an error
# of automatic config of python header due to
# the cross-compiling environment
CXXFLAGS="-I$ROOT/include -D_hypot=hypot"

#cmake $LMGCSRC -DCMAKE_TOOLCHAIN_FILE=$ROOT/sources/cmake-mingw64-$ARCH -DBLAS_LIBRARIES=$ROOT/lib/libopenblas.a -DLAPACK_LIBRARIES=$ROOT/lib/libopenblas.a -DNUMPY_INCLUDE_DIR=$ROOT/include -DCMAKE_INSTALL_PREFIX=$ROOT/install -DCMAKE_CXX_FLAGS="$CXXFLAGS" -DCONTRIBS_Fortran_FLAGS=-Dmetis4 -DPYTHON_INCLUDE_DIR=$ROOT/include/python2.7 -DPYTHON_LIBRARY=$ROOT/lib/libpython27.a -DWITH_DOCSTRING=0 -DSWIG_DIR=/usr/share/swig-2.0 -DSTDC++_LIB="$STDCPPLIB" -DBUILD_STANDALONE=True

#cmake option to cross compile
CMAKE_OPT="-DCMAKE_TOOLCHAIN_FILE=$ROOT/sources/cmake-mingw64-$ARCH -DCROSS_COMPILING=ON -DCMAKE_INSTALL_PREFIX=$ROOT/install"

#cmake option to specify python path/library/version
CMAKE_OPT="$CMAKE_OPT -DPYTHON_INCLUDE_DIR=$ROOT/include/python$PY_VER -DPYTHON_LIBRARIES=$ROOT/lib/libpython$PY_VER_ND.a -DPYTHON_VERSION=$PY_VER"

#cmake option to specify numpy path/version
CMAKE_OPT="$CMAKE_OPT -DNUMPY_INCLUDE_DIR=$ROOT/include/numpy -DNUMPY_VERSION=$NP_VER"

#cmake option to specify OpenBLAS library ; current version needs fortran libraries to remove
# undefined reference to `_gfortran_concat_string' error
CMAKE_OPT="$CMAKE_OPT -DLAPACK_LIBRARIES=$ROOT/lib/libopenblas.a;$GFORTRANLIB"

#cmake option to use metis in mumps build
CMAKE_OPT="$CMAKE_OPT -DENABLE_METIS=ON -DMetis_LIBRARIES=$ROOT/lib/libmetis.a"

#cmake option to use HDF5... all this is and libraries on command line
#are just to avoid using the macros...
CMAKE_OPT="$CMAKE_OPT -DWITH_HDF5=ON -DHDF5_INCLUDE_DIRS=$ROOT/install/include/shared -DHDF5_VERSION=$HDF5_VERSION"

#cmake option to tune it a little
CMAKE_OPT="$CMAKE_OPT -DNO_DOXYGEN=1 -DNO_TEST=1 -DWITH_OPENMP=OFF"
#" -DBUILD_STANDALONE=True"

#configure
$CMAKE $LMGCSRC $CMAKE_OPT -DCMAKE_CXX_FLAGS="$CXXFLAGS" -DHDF5_LIBRARIES="$ROOT/install/lib/liblibhdf5.a;$ROOT/install/lib/liblibhdf5_fortran.a;$ROOT/install/lib/liblibhdf5_f90cstub.a;$ROOT/install/lib/liblibhdf5.a"
#build
make  install

#install
cd $ROOT
cp -r install/lib/python$PY_VER/site-packages/pylmgc90/ .
cp -r install/lib/libdmumps.dll install/lib/libmumps_common.dll install/lib/libpord.dll install/lib/libmpiseq.dll pylmgc90/chipy
cp install/lib/libmatlib.dll pylmgc90/chipy
cp $STDCPPLIB   pylmgc90/chipy
cp $GCCLIB      pylmgc90/chipy
cp $GFORTRANLIB pylmgc90/chipy
#cp $MINGWLIBDIR/libquadmath-0.dll $LIBWINPTHREAD pylmgc90/chipy
#cp $LIBWINPTHREAD pylmgc90/chipy
#cp $ROOT/install/bin/lmgc90.exe pylmgc90/chipy
zip -r pylmgc90-$ARCH-py$PY_VER_ND.zip pylmgc90
