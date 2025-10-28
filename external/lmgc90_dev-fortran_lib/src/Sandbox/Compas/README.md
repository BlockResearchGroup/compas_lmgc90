# Binding for Compas

Compas-DEM is a python framework providing DEM tools
dedicated to the study of historic and modern masonry structures.

https://blockresearchgroup.github.io/compas_dem/latest/index.html

This seciton aims at providing a convenient binding of LMGC90 core
library inside the Compas-DEM framework.

## Issues:

At the time of writing, the main issue of LMGC90 is the difficulty
to build, with the python API, on any operating system in a convenient way.
Especially, the dependency to swig and HDF5 is, at the time of writing,
troublesome.

Compas-DEM is designed to be simply installed in a conda environment
(https://docs.conda.io/en/latest/). And LMGC90 is not designed to
build/install in a conda environment whatever the operating system.

And, instead of swig, the tool used to bind c/c++ libraries to python
is the nanobind project (https://github.com/wjakob/nanobind).

## Rough, temporary, roadmap:

To go toward an integrated and portable build of a LMGC90 binding
inside the Compas-DEM, the different steps decided during the meeting
of September the 2nd (2025) is:
* to have a minimal build of lmgc90 for rigid 3d polyhedron only
* as a C or Frotran library or even executable
* use files as input/output of this programe.


## Current work:

#### Regarding Operating System

This has been tested on:
* Ubuntu 24.04 using virtualenvwrapper
* MacOSx using miniconda

For miniconda, here is the list of installed packages:
```shell
conda install -c conda-forge cmake gfortran clang clangxx
```
Which worked with gfortran 15 and clang 21... To maintain !


#### First steps: generate/check data for LMGC90

The basic files used to workon on this binding, just to start are:
* `gen_dem_vault.py` a python script using both `compas_dem` and `pylmgc90` python module to generate
  input data of LMGC90 (i.e a `DATBOX` directory)
* `command.py` a python script using `pylmgc90` python module to compute the case, generated
  the standard output of LMGC90 (i.e. `OUTBOX` and/or `DISPLAY` directory)

Is added, a single `post.py` python script, which also uses the `pylmgc90` python module
to generate the content of the `DISPLAY` directory from the `DATBOX` and `OUTBOX` one.

So, to use this, a 'classic' installation of LMGC90 with python binding (generated with swig)
and the `vtk` python module is needed.

To access install the `compas_dem` module, following the instruction of
[its website](https://blockresearchgroup.github.io/compas_dem/latest/installation.html)
in the same virtual environment than the one with LMGC90 installed, simply run
```
python -m pip install compas
```

To generate the test case then:
```shell
python gen_dem_vault.py
```

Furthermore, to work LMGC90 needs that some directories exist.
To no be bothered later, create them with:
```shell
mkdirlmgc
```
This command shoudl be available if you `make install`ed the
LMGC90 project in your python virtual envrionment.

#### C library:

A first and transitional step to get rid of swig is to re-activate an old feature of LMGC90, which
is to build a C library from the current `ISO_C_BINDING` wrapper of the Fortran core.

To generate such library and manage installation path without compromising the system
or user profile, a python like virtual environment is recommended. Update the `$VIRTUAL_ENV`
variable in the following instruction with the `$CONDA_PREFIX` or any other relevant variable.

Build lmgc90 in a specific build directory:
```shell
cmake [path_to_source] -DBUILD_ChiPy=OFF -DBUILD_PRE=OFF -DBUILD_POST=OFF -DNO_DOXYGEN=ON -DWITH_DOCSTRING=OFF  -DWITH_HDF5=OFF -DMATLIB_VERSION=none -DSPARSE_LIBRARY=none -DWITH_OPENMP=ON -DBUILD_C_LIB=ON -DCMAKE_INSTALL_PREFIX=$VIRTUAL_ENV
make install
```

The `install` part is important because it put the `lmgc90.so` library in the desired
path AND it also install all header files !

Then the `command.cpp` file, which is the C++ main program mimicking the behaviour
of the `command.py` file... except that it cannot generate the paraview files.
And if input simulation parameters are changed, the executable must be re-build.

To build an executable, assuming `g++` will be the same compiler than the one
used to build the LMGC90 C shared library:
```shell
g++ command.cpp -o crun -I$(VIRTUAL_ENV)/include -L$(VIRTUAL_ENV)/lib -llmgc90
```

Since this is still testing and the final building process of this binding
is not decided, no effort is made regarding the packaging of these libraries/program.
So the path to the library must be set:
```shell
export LD_LIBRARY=$VIRTUAL_ENV
```

Then by simply using the executable `crun` the LMGC90 computation will be run.
And, as explained in previous section, in a python virtual environment with
pylmgc90 available, it is possible to generate the vtk file to check the results.

TODO:
* check that this work on Windows and MacOS, especially regarding the mix of C/Fortran compilers
* maybe rename the `BUILD_C_LIB` in the cmake command as `BUILD_CXX_LIB`... 


#### Fortran library:

The previous section allows to 'simply' generate an executable allowing to run an LMGC90
computation with a minimal build. The main problem is that this 'C' (in fact C++) API
was designed with the swig use in mind and is not very convenient for the current need.

So the aim is to first regenerate a Fortran library of only the core.

This is done by running, in a new build directory:
```
cmake [path_to_source] -DBUILD_ChiPy=OFF -DBUILD_PRE=OFF -DBUILD_POST=OFF -DNO_DOXYGEN=ON -DWITH_DOCSTRING=OFF -DWITH_HDF5=OFF -DMATLIB_VERSION=none -DSPARSE_LIBRARY=none -DWITH_OPENMP=ON -DBUILD_Fortran_LIB=ON
make
```

This will generate the Fortran library in the `lib` directory of the current build directory. All the `.mod` files
(which are kind of headers files, but generated and depedending on the compiler used) are located in the
`modules` subdirectory.

In this case, it is not intended to use the 'install' target. The philosophy is to use the `lmgc90.cfg` file
generated automatically which provide the compilers used and the paths (using the absolute path of the
build directory) to the library and modules.

To make things easier, a simple `Makefile` which first include this config file allow to generate
an exectuable from the `command.f90` program. This is the translation of the `command.cpp` file but
using directly the core functions of LMGC90.
```
cp [path_to_build]/lmgc90.cfg .
make F
```

Make sure you deactivate your python environment and clean up your `LD_LIBRARY_PATH` environment
variable to now use the content of the `lmgc90.cfg` file to ensure that you link/use the correct library.

Then by running the `frun`  executable, the LMGC90 computation will be run. As in the previous case,
no display are generated.

As a side note, the `Makefile` can also generate the `crun` executable of the previous section, as long as
the `CXX` value is provided (either because it is the same as the one provided in the `lmgc90.cfg` file
or if it is set/updated) with the `make C` command.


#### Toward a convenient binding

A specific binding, still using the `ISO_C_BINDING` is now provided:
* the `wrap_lmgc90_compas.f90` is the implementation of the binding
* the `wrap_lmgc90_compas.h` is the header file describing the available C API

To do a quick check, a `comgc90.c` (which is the combination of COMpas and lMGC90)
main program in C is provided. It calls the function to read and compute, then
it shows how to get an array of C struct describing all the interactions.
The array must be allocated/freed on the C side and be of the right size
so that the copy in this C space is done properly.

This library/program can be tested with:
```shell
make wrap
./comgc90
```

To further test, only a C shared library may be build instead of an executable.
In can then be imported in a python interpretor and used (using the `ctypes` module):
```shell
make wrap_lib
python test.py
```

In my opinion there are now two steps to take:
1. Generate a python binding compatible with compas allowing to:
  * set all input parameters
  * run the computation
  * get the interactions as a structured numpy array
2. Make it so that everything needed by compas is installable within it environment:
  * the library
  * the wrapper
  * the pre-processor of LMGC90, but only the pure python part
  * and make sure this works on any operating system

Many functions/variables/files may be renamed. Just state your preferences...

