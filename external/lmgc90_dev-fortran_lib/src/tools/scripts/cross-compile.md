Generate a Windows library
==========================

Everything is done by the script stored in `lmgc90_dev/src/tools/script/cross_compile.sh`.

The difficulty is to compile/link everything with the *mingw* compiler and to be consistent with what is already present in Anaconda. In John's original script `openBLAS` was downloaded and compiled, we keep doing that since I (@mozul) couldn't find a way to get the blas/lapack from the Anaconda.

On the whole the script should be stable for a version specific version of Anaconda, but if there is adjustment to to do for a next version, here is a reminder on how the script has been build.

Use of the script:
------------------

* Move the script where the build must be done (out of source)
* Adapt the different variables at the beginning of the script (check next section) and check that the pre-requisites are fulfilled
* Run the script with the source path to lmgc90 as the first argument

The script will create several directories:

* _sources_ where the different packages will be downloaded with `wget`
* _lib_ where the different external libraries will be temporarily stored
* _include_ where the different external libraries header files will be temporarily stored
* _builds_ where lmgc90 will be build
* _install_ the path where lmgc90 do the installing process

Then the script will zip the final *pylmgc90* python package in the base directory.

Prerequisite:
-------------

- mingw-w64
- gfortran-mingw-w64
- g++-mingw-w64
- gendef (mingw-w64-tools)
- 7z (p7zip-full package on debian)
- wine (for HDF5 build)
- cmake
- wget
- swig
- make
- tar
- sed
- unzip
- zip

In ubuntu 18.4, for the five first items, it was enough to run:
```shell
sudo apt install mingw-w64 gfortran-mingw-w64 g++-mingw-w64 mingw-w64-tools p7zip-full
```

To build HDF5 library `wine64` emulator is needed:
```shell
sudo apt install wine32 wine64
```

If there is an error installing `wine32`, it may be needed to run:
```shell
dpkg --add-architecture i386
apt-get update
```

And to run the executable which will be generated, the path to
the `.dll` of the different compilers used must be added to the
`PATH` environment variable. To this end the registry must be
tampered with thanks to:
```shell
wine64 regedit
```

If there is an X server running this will open a graphic interface
in which you will have to find the `PATH` field in
`HKEY_LOCAL_MACHINE/System/CurrentControlSet/Control/Session Manager/Environment`
registry. The the field must be edited to append `/usr/lib/gcc/x86_64-w64-mingw32/7.3-win32`
in it (use ';' as a separator).

If there is no X server, then you can use the following one liner:
```shell
sed -i -e 's/\("PATH"=str(2):".*\)"/\1;\/usr\/lib\/gcc\/x86_64-w64-mingw32\/7.3-win32"/g' ~/.wine/system.reg
```


Variables of the script:
------------------------

* The main variable is the target arch : `x86_64` or `i686`
* Then path to sources of *lmgc90_dev* directory in `LMGCSRC` variable (which is first argument of the script)
* After that the paths to the *gcc*, *g++* and *gfortran* libraries for *mingw*

Finally other variables are defined later, a small set for each dependency:

* Openblas:

    * `OPENBLAS_VERSION` desired version of openblas
    * `OPENBLAS_NAME` name of file (omitting .zip extension) to download on sourcforge

* Anaconda:

    * `ANACONDA_VERSION` currently 2018.12
    * `ANACONDA_PATH` download path https://repo.continuum.io/archive
    * `ANACONDA_NAME` name of file to download (currently build from `ARCH` and `ANACONDA_VERSION` variables)
    * `ANACONDA_PYTHON` name of the file to extract python, can get this by doing a
      ```shell
      7z l ANACONDA_NAME | grep python
      ```
    * `PY_VER` version of python (currently 3.7)
    * `PY_VER_ND` same as above but with No Dot
    * `ANACONDA_NUMPY` name of the file to extract numpy, can get this by doing a
      ```shell
       7z l ANACONDA_NAME | grep numpy
       ```
     * `NP_VER` full version of numpy (currently 1.15.4)

* Metis :

    * `METIS_PATH` download path http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/OLD
    * `METIS_VERSION` currently 4.0.3
    * `METIS_FILE` name of file to download 
    * `METIS_NAME` name of directory extracted

* HDF5 :

    * `HDF5_VERSION_MAJOR`  currently 1.10 (cannot have less for LMGC90)
    * `HDF5_VERSION` currently 1.10.4 (cannot have less to hope to cross compile)
    * `HDF5_FILE` name of file to download
    * `HDF5_NAME` name of directory extracted
    * `HDF5_PATH` download path https://support.hdfgroup.org/ftp/HDF5/releases/
       and something depending on previous variables

How does the script work:
-------------------------

* Download of sources and binaries using _wget_ depending on the input variables:

    * OpenBlas
    * Anaconda
    * Metis
    * HDF5

* Build or extraction of some libraries/includes from the binaries and put them in `$ROOT/include` and `$ROOT/lib` directories.
* Concerning python, the _7z_ utility of linux is used to list/extract component of the `.exe` package :
  ```shell
  $> 7z l Anaconda3-2018.12-Windows-x86_64.exe | grep -i python3
  2018-12-20 05:43:32 .....          605          605  pkgs/python-3.7.1-h8c8aaf0_6/info/repodata_record.json
  2018-12-20 05:40:12 .....     18603792     18603792  pkgs/python-3.7.1-h8c8aaf0_6.tar.bz2
  $> 7z e Anaconda3-2018.12-Windows-x86_64.exe pkgs/python-3.7.1-h8c8aaf0_6.tar.bz2
  ```
* Then a _toolchain file_ is made to provide to *CMake* the path to the different compiler and prepare the cross-compiling environment.
* Finally where usually *CMake* find a lot of paths by himself, most must be provided on the command line.
* The `make` and `make install` steps are necessary and install `pylmgc90` module in the `install` directory.
* All the external dynamic libraries must be copied in `pylmgc90/chipy` directory.


Concerning the build of HDF5 library:
-------------------------------------

At the time of writing, cross-compiling is not well supported by HDF5 because
at configure step, many program must be executed on the target plateform. To
this end `wine64` must be installed and assigned to the variable `CMAKE_CROSSCOMPILING_EMULATOR` in the toolchain file.

Furthermore on ubuntu18, many thing did not work for the 1.10.4 release. Thus the sources
must be patched :
```shell
sed -i -e "s/, \(__VA_ARGS__\)/, ##\1/" $HDF5_NAME/src/H5win32defs.h
```

And the build directory had to be patched because there were either missing
spaces between `wine64` and some `.exe` program, or the `wine64` call was missing
altogether:
```shell
find . -name "build.make" -exec sed -i -e 's/wine64\/home/wine64 \/home/' {} \;
find . -name "build.make" -exec sed -i -e 's/\(^.* && \)\(.*\.exe$\)/\1 wine64 \2/' {} \;
```

Then all include/libraries/variables  needed to build must be provided by hand to
LMGC90 to shunt the cmake macro which _did not work_ (no further inquiry has been
done on why).


If the binary is not imported on Windows:
-----------------------------------------

On the Windows machine the dependencies can be checked using an external tool : [dependencies](https://github.com/lucasg/Dependencies)


