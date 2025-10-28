# LMGC90 project: developper version #

# Preamble

This file is intended to user of the development version
which requires a nominative access to our GitLab server.

For the generic user version please read the [README_FOR_USER](README_FOR_USER.md) file

# Downloading

To get the project please check [here](https://git-xen.lmgc.univ-montp2.fr/lmgc90/lmgc90_dev/wikis/home)

# Contact us

If you'd like to discuss a bad behavior during a simulation, to propose a non 
regression test, to propose a new feature or any thing relative to the 
development of LMGC90, please contact us via this mailing list:

lmgc90-dev@groupes.renater.fr

To stay aware, you can subscribe to this mailing list by asking to:

 - Dubois Frédéric : frederic.dubois@umontpellier.fr
 - Rémy Mozul : remy.mozul@umontpellier.fr

# Organization

In *lmgc90_dev* directory is organized in several
subdirectories:

 * *src* : the sources of LMGC90 software
 * *examples* : the user examples working with the sources
 * *manuals* : a list of manuals/documentation for the software in pdf format

The software is originally designed for [Linux or MacOS](#linversion).

For Windows user, please read this before anything else: [Windows Version section](#winversion)

# <a name="linversion"> Linux and MacOS version </a>

# Versionning

People contributing to the project can view up-to-date information
[here](https://git-xen.lmgc.univ-montp2.fr/lmgc90/lmgc90_dev/tree/master)

The versionning tool used is git. To have more information on the workflow used
and how to use git check [there](https://git-xen.lmgc.univ-montp2.fr/lmgc90/lmgc90_dev/wikis/LMGC90_dev)

With MacOS and Linux the installation and use of LMGC90 software is through
the use of the terminal. You must know how to open a terminal in a specific
directory and move through your directory tree.

## Update

To update to very last version from a terminal in current directory:

```shell
git pull origin dev
```

To get as specific version, for example *2019.rc1* 
in a new branch :

```shell
git fetch origin
git checkout -b 2019.rc1 tags/origin 2019.rc1
```

Then to switch between branches:

```shell
git checkout dev
git checkout 2019.rc1
```

If you want to discard any modification you made
to reset your current branch to one of the server
side (use this carefully), for example `dev` branch, use:

```shell
git checkout dev
git reset --hard origin/dev
```

## Compilation

These instructions are most likely obsolete. Check the wiki of the user
version to get up to date instructions.

In case you want to build an old version, the most helpfull you will get
is to check the `ci_script/Dockerfile_*` files to find the dependencies.

What is left of this section are general directions to help setting up
a more comfortable environment.

### Use of virtual environment (optional)

For MacOs, this use of the simple virtual environment is not
fully fonctionnal, please read the [virtualenvwrapper](#virtualenvwrapper) section.

To have more information on virtual environments,
read [here](https://docs.python.org/3/library/venv.html)
The first step is to choose a directory in which
to create the environment. In this explanation
a build directory inside the source directory of
LMGC90 is chosen, but any directory of your convenience
will do:
```shell
cd lmgc90_dev
mkdir build
cd build
python -m venv venv
```

The environment created will not use the python packages
installed in default system path. To use them, it is needed
to either use the option `--system-site-packages` when creating
it or to modify the option `include-system-site-packages`
in the `pyvenv.cfg` file.

Then, once the environment is created, it needs to be
activated in the current terminal:
```shell
. venv/bin/activate
```

Then in this environment you need to install at the very
least *numpy* to be able to build LMGC90:
```shell
pip install numpy
```

To be able to use function `pre.rigidPolyhedron()`
over large polyhedron, it is recommended to install *scipy*
```shell
pip install scipy
```

To be able to generate visualization files for paraview *vtk*
is needed
```shell
pip install vtk
```

Finally to read the tutorial  
```shell
pip install jupyter
```

If you are not willing to use virtual environment, you are
free to look for the corresponding package in your distribution
or use *pip* as administrator to install these modules
system-wide.

### <a name="virtualenvwrapper"> Use of virtualenvwrapper (optional)

Install virtualenvwrapper with your package manager, on Linux:
```bash
sudo apt install virtualenvwrapper
```
On MacOs:
```bash
sudo port install  py38-virtualenvwrapper
sudo port select --set virtualenv virtualenv38
```
And follow the instructions regarding the environment variables.

Then to create a new environment named `lmgc90`, run the following:
```bash
mkvirtualenv lmgc90
```
This will create an isolated environment for your use and activate it.
You can deactivate it to return to a normal use or reactivate it later
with:
```
deactivate
workon lmgc90
```
To install package only, for you active environment run:
```
pip install jupyter numpy scipy matplotlib vtk
```



## Building

You have several commands to run. If you are not familiar with
the terminal and compilation in general please read the output
of each command carefully. And if an error occurs do not blindly
run the next commands, but try to correct it first.

If you want to understand what you are doing and know what are
the possible tweaks in configuration, jump to the next paragraph...
For the really impatient, if you followed the previous section
using *venv* try to run:
```shell
cd lmgc90_dev/build
. venv/bin/activate
cmake .. -DVENV_PATH=$PWD/venv
make
make install
```
If conda has benn used, run the previous commands replacing:
`-DVENV_PATH=$PWD/venv` by `-DVENV_PATH=$CONDA_PREFIX`.
And if virtualenvwrapper has been used, then replace:
`-DVENV_PATH=$PWD/venv` by `-DVENV_PATH=$VIRTUAL_ENV`.

If you do not use virtual environment, it is recommended
to explicitely provide the path to python executable to
use, for example, on ubuntu 18 using the default `/usr/bin/python3`,
then just run:
```shell
cd lmgc90_dev/build
cmake .. -DPYTHON_Executable=/usr/bin/python3
make
make install
```


CMake is used to generate the makefile and out of source build is advised.
So you need to know the source tree path and decide on a build path then.
Then some additional variables can be feed to cmake to help it to do the
configuration. The general way of using cmake is:
```shell
mkdir build_path
cd build_path
cmake source_path -Dvariable=value
make
```

Instead of cmake, one can use *ccmake* to change variable values on
the command line, or *cmake-gui* to use graphical interface.

If you followed the previous section the build path has been chosen as
a *build* subdirectory of the *lmgc90_dev* one. When building, the
virtual environment must be active, and given to in the ```VENV_PATH``` variable
```shell
cd lmgc90_dev/build
. venv/bin/activate
cmake .. -DVENV_PATH=$PWD/venv
make
```

If you are only interested in rigid computations, some external libraries
can be disabled. Before the ```make``` command, run:

```shell
cmake . -DMATLIB_VERSION=none -DMUMPS_VERSION=none
```

To be able to write HDF5 file (assuming you installed the dependencies), 
you have to compile with:
```shell
cmake . -DWITH_HDF5=True
```
Beware, on the some cluster (in fact depending on the mounting
of the directory in which to write the HDF5 file), you may need
to set the following environment variable:
```shell
export HDF5_USE_FILE_LOCKING=FALSE
```

If you want to build the documentation run :

```shell
make docs
```

It will build the sphinx documentation in the ```docs``` directory where
you should open the ```index.html``` file.
The doxygen documentation of the the core of the software would be
in ```src/Docs/html/index.html```.


## Installing

If you use virtual environment and provided the ```VENV_PATH``` variable,
then you can install LMGC90 in your virtual environment using the command:
```shell
cd lmgc90_dev/build
make install
```

Otherwise you need to provide an installation path with
```shell
cmake . -DCMAKE_INSTALL_PREFIX=your_install_path -DNO_INSTALL=FALSE
make install
```

Please note that by default cmake will want to install in a path requiring
administrator rights. Our policy is to not mess with default system paths.
So instead we advise to use a environment variable to add to python the path
to your build directory.

In general adding the following lines to your *.bashrc* (Linux) or *.profile* (MacOS)
file does the trick. Of course you have to replace *mybuildpath* by the path to
your own building directory. Basically it is what returns the command *pwd* when
ran in the same directory you ran the commands *cmake* and *make*.

```shell
if [ -z ${PYTHONPATH} ]; then
  export PYTHONPATH=mybuildpath
else
  export PYTHONPATH=${PYTHONPATH}:mybuildpath
fi
```

## Getting started

There are several examples in the directory *examples*
sorted by the type of simulation.

A good entry point is the *Tutorials* directory where you
can find some **ipython notebooks** trying to introduce
step by step the use of the software. 

To read the notebooks, open a terminal, activate your
virtual environment (if needed) and run jupyter:

```shell
cd lmgc90_dev
. build/venv/bin/activate
cd examples
jupyter notebook
```

Python being the interface language to the LMGC90 software,
[this introduction](https://www.python.org/about/gettingstarted/)
is recommended for those unfamiliar with the language or programming
in general.


## Non regression

To the developpers, no need to insist on the importance of
non regression tests. To store away the results of tests,
one must set the cmake variable ```SAVE_REGRESSION_BASE```
to the path where the file tree of examples/tests will be saved.

Then, after build and in the build directory run:

```shell
ctest -C save_reg
```

Once the reference results are stored, by setting the
cmake variable ```NON_REGRESSION_BASE``` to its path,
the non regression test can be run with:

```shell
ctest -C non_reg
```

** note on the use of `ctest`

To run a subset of tests it is possible to select
only tests of a label by appending on the ctest line
(currently the only label available is `quick`):
```shell
ctest -C non_reg -L quick
```

It is also possible to select tests with regular expressions
on their names, for example to run only the examples starting
with `RIGID_2D`, one must run:
```shell
ctest -C non_reg -L quick -R RIGID_2D
```

## Running set of examples

Again using Ctest, it is possible to run the different
examples which do not have reference results...

To do so use :
```shell
ctest -C just_run -L all
```

The examples are grouped and tagged depending on
the execution time:
* *quick* less than 60s
* *correct* less than 180s
* *long* less than 800s
* *slow* the leftovers

To run only the *quick* and *correct* ones, use:
```shell
ctest -C just_run -L "quick|correct"
```

The `-R` can still be used in combination...

To list all the available test of a config:
```shell
ctest -C just_run -N
```

And to run a test by number, for example from 8 to 13:
```shell
ctest -C just_run -I 8,13
```

# <a name=winversion> Windows version </a>

There are two possibilities to use LMGC90 on Windows:

* to build on a linux subsytem
* to build in conda environment

### Installation on a linux subsystem
The first one is the recommended option for those who want to use
the developper version of the software since it provides an
environment in which the user can build LMGC90 from the sources.

To this end, the bash program for windows must be installed
by following : [https://docs.microsoft.com/en-us/windows/wsl/install-win10]

To be able to run grapical applications (especially the notebooks or a python
text editor like geany), one must also install a third party tools following
this: [https://solarianprogrammer.com/2017/04/16/windows-susbsystem-for-linux-xfce-4/]

Then start the 'bash' program and follow the Linux/Ubuntu instruction for installing.

### Installation in conda environment

Conda and Anaconda (further noted ana/conda) use environment extensively.
This mechanics rely strongly one environment variable (yes, even in windows).
Thus as of time of test, the absolute path to your home directory on Windows
MUST NOT CONTAIN ANY SPACE. Otherwise, you will not be able to create new
environment and even so, LMGC90 will not be able to compile properly.

Download and install miniconda at [conda 64 bits](https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe)
or download and install [Anaconda Python Distribution](https://www.anaconda.com/products/individual)

Then open an ana/conda prompt (use search bar of windows and type ana/conda).

>  To be able to run conda commands from powershell instead of the ana/conda
>prompt only, you need to run once:
>```cmd
>conda init powershell
>```
>After that, a powershell console can be opened and conda can be used there.

To install all dependencies for lmgc90, first create an environment, activate
it and install the needed packages. This can be done with:
```cmd
conda create -n lmgc90 python
conda activate lmgc90
conda install numpy scipy matplotlib h5py pandas
conda install cmake git swig libpython
conda install m2w64-gcc-fortran m2w64-make m2w64-openblas
conda install -c conda-forge vtk #with conda-forge for python3.8 at time of writing
```

In this case, the `lmgc90` environment name is chosen, but it could be
anything depending on your preferences.

Then the simplest way to build and install lmgc90 in the conda environment
with the same name, is to run the script `conda/build_with_hdf5.ps1`.
Since it will try to download HDF5 sources, you need to be connected to
the world wide web before running this script (by doing a right clic and
'execute with powershell'). If that is not the case, read next paragraph.

If you change the name of the environment you have to modify the script
to activate that environment, or if a simple computation is enough you can
run in the current envrionment:
```cmd
cd build
cmake -DVENV_PATH=%CONDA_PREFIX% -G "MinGW Makefiles" ..
mingw32-make install
..\finalize_conda_env.bat
```

You can then try your installation with the command `python -c "import pylmgc90"` which may return warnings but no error.

## Getting started

There are several examples in the directory *examples*
sorted by the type of simulation.

A good entry point is the *Tutorials* directory where you
can find some **ipython notebooks** trying to introduce
step by step the use of the software. 

To read the notebooks, run Anaconda Navigator,
activate your environment, and run Jupyter. To
read simple script, spyder can be used to get
a matlab-like interface.

Python being the interface language to the LMGC90 software,
[this introduction](https://www.python.org/about/gettingstarted/)
is recommended for those unfamiliar with the language or programming
in general.

# Dependency specification

By default and if nothing is specified:

* a matlib version of 2021 is build from the sources shipped with lmgc90
* a thread-safe version of mumps-4.9.2 is build from the sources shipped with lmgc90
* siconos is not used

### Matlib

The default option is equivalent to run, from the build directory:
```shell
cmake . -DMATLIB_VERSION=default
```
To deactivate matlib, the *default* value must be replaced by *none*. An older
version of the matlib can be build by using the value *v4*.
If you want to use an existing version of the matlib library already installed
on your system, you must specify its path with
```shell
cmake . -DBUILD_MATLIB=OFF -DMATLIB_PATH=path_of_the_directory_with_matlib_inside
```

### Demmefi

The demmefi project usable by LMGC90 is [here](https://git-xen.lmgc.univ-montp2.fr/demmefi/lmgc90).
Installation instruction are provided in its own
[README.md file](https://git-xen.lmgc.univ-montp2.fr/demmefi/lmgc90/blob/master/README.md).

As a summary, the default option is equivalent to run, from the build directory:
```shell
cmake . -DWITH_DEMMEFI=OFF
```
If the library is installed in a default path (including a virtual environnment), the config is:
```shell
cmake . -DWITH_DEMMEFI=ON
```
otherwise the config must be:
```shell
cmake . -DWITH_DEMMEFI=ON  -DDEMMEFI_PATH=[DEMMEFI_BUILD_PATH]
```

### Sparse Library

The default option is equivalent to run, from the build directory:
```shell
cmake . -DSPARSE_LIBRARY=mumps
```
To deactivate mumps, the *mumps* value must be replaced by *none*. UMFPack
library can be used instead by using the *umfpack* value (not much tested).

By default, the mumps library is build because it is currently the only
way to have a thread-safe mumps library compatible with an openmp build.

However it is possible to specify the path to mumps library build on your
system by providing:
```shell
cmake . -DMUMPS_LIBRARY_DIRECTORY=path_to_mumps_root
```
in which case a *dmumps* library will be looked for in the directory
and *lib* subdirectory and the *dmumps_struc.h* file will be looked
for in the directory and *include* subdirectory.

Otherwise a library name can be specified with the associated
include path with:
```shell
cmake . -DMUMPS_LIBRARY=path_of_libdmumps_seq.so -DMUMPS_INCLUDE_DIR=path_of_include_directory
```

### Siconos Numerics

By default Siconos is not used. It requires to use mumps sparse library
to be used. The sources of siconos can be obtained by running:
```shell
git clone https://github.com/siconos/siconos.git
```

Then there are two possibilities:

* ask of LMGC90 to build siconos using its mumps (build or provided)
* build siconos using a mumps and build lmgc90 providing the siconos AND mumps library to use

For the first one simply run from your build directory:
```shell
cmake . -DWITH_SICONOS_NUMERICS=ON -DSICONOS_SOURCE_DIR=path_of_siconos_directory
```

For the second one, provided that you already set the right mumps library, run
```shell
cmake . -DWITH_SICONOS_NUMERICS=ON -DSiconosNumerics_LIBRARY_PATH=path_of_directory_with_siconos_numerics_library
```

To have more details on the siconos library, check its official website
[here](https://nonsmooth.gricad-pages.univ-grenoble-alpes.fr/siconos/index.html)

