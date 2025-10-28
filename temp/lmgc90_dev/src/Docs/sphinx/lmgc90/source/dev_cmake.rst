
About CMake programming
=======================

.. *TODO:* some formatting to do, because this is some rambling/mumbling that will need
.. some effort to make it usable by other people besides the author.

CMake is the chosen tool to manage/prepare the compilation and
detection of dependencies of LMGC90. Alternatives to cmake exist, like
**waf**, the development team is open to relevant propositions on this subject.

Whatever the future holds, the desired behavior of the system inquiry is independent
of the selected tool, as well as the management of the compilation flags, especially when some
dependencies are build as contributions by LMGC90.

On dependencies of LMGC90
-------------------------

**Generalities:**

There are several small libraries on which LMGC90 depends, these libraries
are included within the code and compiled at the same time than LMGC90 itself.
These libraries are:

* ANN
* rTree
* robustPredicates

There are two external libraries which must be found on the system:

* LAPACK
* Python

There are some optional libraries, which may be ignored, looked for on the system or compiled
within LMGC90:

* MUMPs
* MatLib
* Demmefi
* SiconosNumerics

Finally there are some external libraries that can be used, and in the case
of coupling with other software/libraries:

* Robotran
* Xper


**Concerns:**

The concerns coming with these dependencies are :

* if a library is compiled, what compilation options are desired ?
* if a library is looked for, where to look for it ?

When building the LMGC90 project, several optimization levels can be used to compile.
These choices are available only for LMGC90 itself and not for the contribution libraries
because it is not intended to debug or profile others' libraries.

When using a library that may be compiled within LMGC90, the desired behavior is:

+--------------------------------------------------------+
| If the user explicitly ask for the library to be build:|
+----+---------------------------------------------------+
|    | Compile it                                        |
+----+---------------------------------------------------+
| Else:                                                  |
+----+---------------------------------------------------+
|    | Look for the library:                             |
+----+----+----------------------------------------------+
|    |    | First in an input path (if provided)         |
+----+----+----------------------------------------------+
|    |    | Then in the default path                     |
+----+----+----------------------------------------------+
|    | If the library is not found:                      |
+----+----+----------------------------------------------+
|    |    | Compile it                                   |
+----+----+----------------------------------------------+


It is critical that, in this order:

* If the user demands the build of the library it is not looked for but really
  build and linked against.
* If the user do not want to use an optional library, it is not even looked for.
* If the user specify a library path, it is used to look for the library.
* If the library is not found in the default path, to compile it if available.


Implementation with CMake
-------------------------

In this section, some details are given on how the above desired mechanism
has been implemented and in which files or directories to look for information.
The next section will provide, for each dependency, the list of options that
are used or needed depending on the desired behavior.


**Options management:**

In the main `CMakeLists.txt` several options allow to decide if to build
a dependency or where to look for it. Sometimes these options are modified or
set within a macro defined in `lmgc90_dev/cmake/modules`.

Furthermore, before going through any subdirectory and starting to define compilation
targets a list of compilation options is defined for several LMGC90 and its contribution.
The choice of compilation options is defined in:

* `get_lmgc90_options` function of `compilerTuning.cmake` for LMGC90
* `get_mumps_options` function of `contribs_compilerTuning.cmake` for MUMPs
* `get_contribs_options` function of `contribs_compilerTuning.cmake` for rTree, ANN and robustPredicates

The results of all these functions are some variables which are cached in order to
check within the `CMakeCache.txt` file what were the option used.

For MatLib and Siconos the developers of these software implemented themselves the CMake.
In this case the compilation options are decided automatically and LMGC90 tries to 
mess with them as less as possible.


**Common dependencies:**

When building MUMPs and/or Matlib, it is best to ensure that the same LAPACK library
than the one detected by LMGC90 is used.

When building Siconos, it is best to ensure that the same LAPACK and MUMPs libraries
than the ones detected or compiled by LMGC90 are used.

This is done within the `ExternalProject` CMake commands in the corresponding `contribs` directory.

**Dependencies definitions:**

The definition of the dependencies is explicitly made in the `Bindings` directory. In this directory
some target library are defined in a fixe name, used in `Core`. But within the bindings, depending
on the CMake variable defined by the user or the library found on the system, the way to define
the libraries to link against may differ. The typical example is the difference between using the
MatLib in version *v3* or in the latest version.

Dependencies list:
------------------


Python
~~~~~~

Python is needed to build the `chipy` module which is the main interface of
the software. It also depends on the `numpy` package.

The Python interpretor itself is used to look for the library and its headers.
If the Python interpreter changes between two build, then the library and
header paths are updated.

If the user specify explicitly the library and its path between two build,
even if the interpretor changed, the data provided by the user will be used
in the first place. The two variables to set are:

* `PYTHON_LIBRARY`
* `PYTHON_INCLUDE_DIR`


LAPACK
~~~~~~

LAPACK library is detected thanks to the native CMake inquiry function.
The resulting libraries are stored in the variable `LAPACK_LIBRARIES`.

The function works usually very well and detect particular implementation
(MKL, Accelerate, etc). To force the location, on the system, of particular
implementation the `BLA_VENDOR` variable is used.

If LAPACK is not installed in a default system path, juste provide the
desired library in the `LAPACK_LIBRARIES` variable.


MUMPs
~~~~~

MUMPs is an optional library allowing to do sparse system resolution
(http://mumps.enseeiht.fr/index.php?page=home). 

Bindings with MUMPs library are activated only if the `SPARSE_LIBRARY`
variable equals `mumps`.

Because MUMPs is not threadsafe by default a specific version patched
to be threadsafe is used and compiled.

If the `WITH_MPI` variable is set to `True` , a parallel build of MUMPs
is made.

But if the user want to use a specific build of MUMPs either the absolute
path to the library and associated include dir must be specified:
* `MUMPS_LIBRARY`
* `MUMPS_INCLUDE_DIR`

or the path to the directories in which to look for the *dmumps* library
by setting the following variables:
* `MUMPS_LIBRARY_DIRECTORY`
* `METIS_LIBRARY_DIRECTORY`
* `MUMPS_INCLUDE_DIRECTORY`

If the library is not found it will be build, except if the
`BUILD_MUMPS` variable is set to `False`.

In MUMPs case, it is **never** looked for in the default path.
Setting the `WITH_OPENMP` variable to `True` while using an
external build of mumps will stop the configuration step with
an error.

MatLib
~~~~~~

The MatLib is dedicated to the definition of material behavior laws.
It is developed by Laurent Stainier at the Ecole Centrale de Nantes.
There are several options to use the library by setting the `MATLIB_VERSION`:

* `none`: to not use the library
* `v3`: to use the version 3 which include some materials from Dominique Ambard
* `v4`: to use the version 4 but not the latest
* `default`: to use the latest included version (default value).

To look for the library in a specific path, the user can set the
`MATLIB_PATH` variable. If the library is not found it will be build,
except if the `BUILD_MATLIB` variable is set to `False`.


Demmefi
~~~~~~~

Check: (https://git-xen.lmgc.univ-montp2.fr/demmefi/lmgc90).
It is a small Fortran library implementing some of CASTEM material
behaviour laws.
By default the `WITH_DEMMEFI` options is set to `False`.

The `demmefi.mod` file and the `libdemmefi.so` must be found.
They will be looked for in the `VENV_PATH` if provided
or in the system path. It is also possible to specify the build path
of the demmefi procject to avoid to install it somewhere with
the `DEMMEFI_PATH` variable.


Siconos
~~~~~~~

SiconosNumerics library is dedicated to non-smooth  solvers
(http://siconos.gforge.inria.fr/). To activate the use of the
library the option `WITH_SICONOS_NUMERICS` must be set to `True`.
If the `SiconosNumerics_LIBRARY_PATH` is set the library will
be looked for there before the default path.

In this case you have to make sure that the siconos library found
use the same mumps as LMGC90. Thus having the `BUILD_MUMPS` variable
set to `True` and trying to use an external *SiconosNumerics* library
will stop the configuration step with an error.

To force the build of the library, the option `BUILD_SICONOS_NUMERICS`
must be set to `True`.  This option is set by default if the *SiconosNumerics*
library as not been found previously. In this case, the path to the *siconos*
directory with the source must be specified with the variable `SICONOS_SOURCE_DIR`.


External FEM
~~~~~~~~~~~~

To use an external Finite Element Modeling library, there are currently two possible
cases to choose among thanks to the `EXT_FEM_VERSION`:

* `none`: no external library
* `Xper`: library developed by the IRSN
* `tense_dd`: library developed by Dr Postek in Warsaw.

Every needed files are looked in the `EXT_FEM_PATH` variable.

For further extensions what is in fact needed:

* the library itself stored in `EXT_FEM_LIBRARY`
* an interface module in Fortran90 in `EXT_FEM_F90_MODULE`
* a wrap module in Fortran using *iso_c_binding* in `EXT_FEM_WRAP_SRC`
* a wrap header in C in `EXT_FEM_WRAP_HEADER`

External MBS
~~~~~~~~~~~~

To use an external Multi-Body System library, there are currently two possible
cases to choose among thanks to the `EXT_MBS_VERSION`:

* `none`: no external library
* `Robotran`: library developed by the UCL
* `FiberModel`: library developed by the IRT Jules Vernes.

These two libraries provide, when compiled, a configuration file
that CMake can read with every information needed and even do some
consistent check between compilers. This file is provided thanks
to the `EXT_MBS_FILE` variable.

The main variable provided by this file is the list of libraries
to link against: `MBS_EXTERNAL_LINK_LIBRARIES`

About git information
---------------------

CMake look for the git executable and cache a variable storing the name of the branch
compiled. If the branch changed and make is called without a call to cmake the build
is stopped. If any CMakeLists.txt file change, the configured step is called and
the compilation will take place (only a warning is send).

Git is also used to get the commit number and store it in pylmgc90.__git_revision__
 
