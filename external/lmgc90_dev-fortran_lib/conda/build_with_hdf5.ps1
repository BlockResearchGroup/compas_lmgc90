# Download and build a static HDF5 version
# Then use it to build LMGC90
# version 1.0
# Author : Rémy Mozul
$ErrorActionPreference = "Inquire"

# creation and installation should already be made...
#conda create -n lmgc90 python
conda activate lmgc90
##conda install numpy scipy pandas matplotlib h5py
##conda install cmake git swig libpython
##conda install m2w64-gcc-fortran m2w64-make m2w64-openblas
##conda install -c conda-forge vtk
#
cd $PSScriptRoot
cd ..

# in lmgc90_dev create a HDF5 directory in which to download/build/install hdf5-1.12.0
if (-Not (Test-Path -Path ./HDF5) ) {
    mkdir HDF5
}
cd HDF5

if (-Not (Test-Path -Path ./hdf5-1_12_0.tar.gz) ) {
    wget https://github.com/HDFGroup/hdf5/archive/hdf5-1_12_0.tar.gz -OutFile hdf5-1_12_0.tar.gz
}
if (-Not (Test-Path -Path ./hdf5-1_12_0) ) {
    tar -xzf hdf5-1_12_0.tar.gz
}
if (-Not (Test-Path -Path ./build) ) {
    mkdir build
    cd build
    cmake ../hdf5-hdf5-1_12_0 -G "MinGW Makefiles" -DCMAKE_INSTALL_PREFIX='../install' -DHDF5_BUILD_FORTRAN=ON
} else {
    cd build
}

mingw32-make -j4
mingw32-make install

# now building lmgc90 with previous HDF5
cd ../../build
cmake ..  -G "MinGW Makefiles" -DWITH_HDF5=ON -DHDF5_ROOT="../HDF5/install" -DVENV_PATH="$env:CONDA_PREFIX" -DBUILD_rTree_BINDING=OFF
mingw32-make -j4
mingw32-make install
& ../finalize_conda_env.bat

Read-Host Prompt "press any key to close window..."
