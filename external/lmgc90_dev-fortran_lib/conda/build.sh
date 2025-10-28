#!/usr/bin/env bash

if [[ ! -d "./build" ]];then
mkdir ./build
fi

cd build

cmake \
    -DCMAKE_PREFIX_PATH=${PREFIX} \
    -DCMAKE_INSTALL_PREFIX=${PREFIX} \
    -DVENV_PATH=${PREFIX} \
    -DCONDA_BUILD=${CONDA_BUILD} \
    ..

make install
