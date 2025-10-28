
# How can rm use guix

## Set up

To startup with `guix` from scratch, I want to fix
the version of guix I will try to use from now on:

```shell
guix describe > lmgc90-channels.scm
```

This should contains the main GUIX channels
and two others recommended by the ISDM (managing
the IO cluster... and the whole point of this
is to manage to use guix to build/run LMGC90
on this super-computer).

First trick: the description is just that,
a text description. But the channel file
must be a Guile file.
So taking the example from the
[official doc on channels](https://guix.gnu.org/manual/en/guix.html#Using-a-Custom-Guix-Channel)
the channel file is modified to fit the following structure
```text
(list (channel
        (name 'name1)
        (url "where to git clone")
        (branch "the correct branch name")
        (commit "sha1")
      )
        (name 'name2)
        (url "clone another one")
        (branch "the correct branch name")
        (commit "sha1...not sha2")
      )
)
```

Or you can read the manual and do that automatically:
```shell
guix describe -f channels > lmgc90-channels.scm
```

The channel file can be tested with:
```shell
guix time-machine --channels=lmgc90-channels.scm -- shell --pure
```
This can take a while depending on where/when the command
is run. And it could be confusing, but when running the previous command,
the current shell is inherited with some of the environment variables
cleaned up. The result of the `env` command may be used to check the
difference.

To make sure that the commands run will use only the content of
the environment provided by guix, then a clean terminal must be
installed and used within the environment, which is made with:
```shell
guix time-machine --channels=lmgc90-channels.scm -- shell --pure bash
```
But then, only `bash` is installed... not coreutils and any other
usefull packages.

Anyhow, now any GUIX command which aim at finding a library/software to
build LMGC90, must be run using this:
```shell
guix time-machine --channels=lmgc90-channels.scm` --
```
Otherwise, you may not find the library at the specified version...

Finally, to modify the PS1 when in a guix environment, the following
block can be adapted and added to the profile file of the desired
shell (usually `~/.bashrc`):
```shell
## to show guix profile in environement
if [ -n "$GUIX_ENVIRONMENT" ]; then
        export PS1="\[\033[01;96m\]\u@\h\[\033[00m\]:\[\033[01;95m\][$GUIX_ENVIRONMENT]\[\033[00m\]:\[\033[01;34m\]\w\[\033[01;31m\]$(parse_git_branch)\[\033[00m\]\$ "
fi
```

And this confirms that, even if a new and clean bash is asked for... the
profile file is still loaded !


## Search for packages:

Now the depedencies of LMGC90 must be found. So each dependencies:
* cmake
* gcc, g++, gfortran
* swig
* python
* numpy
* openblas (with openmp support)

Can be looked for using:
```shell
guix time-machine --channels=lmgc90-channels.scm` -- package --search=cmake
```

Which gives something like:
```shell
name: cmake
version: 4.0.3
outputs:
+ doc: documentation
+ out: everything else
systems: x86_64-linux
dependencies: bzip2@1.0.8 cppdap@1.58.0-a curl@8.6.0 expat@2.7.1 file@5.46 jsoncpp@1.9.6 libarchive@3.7.7 libuv@1.44.2 ncurses@6.2.20210619 python-sphinx@6.2.1
+ rhash@1.4.3 texinfo@6.8 zlib@1.3.1
location: gnu/packages/cmake.scm:378:2
homepage: https://cmake.org/
license: Modified BSD, Expat, Public Domain
synopsis: Cross-platform build system
description: CMake is a family of tools designed to build, test and package software.  CMake is used to control the software compilation process using simple
+ platform and compiler independent configuration files.  CMake generates native makefiles and workspaces that can be used in the compiler environment of your
+ choice.
relevance: 41
```

Question : in outputs, there is `doc` and `out`... is there a way to
choose a subset of everything. Because the doc is not really needed
in this context... Or should the `cmake-minimal` be inquired/tested ?

The package could be installed when starting the shell with something
like:
```shell
guix time-machine --channels=lmgc90-channels.scm` -- shell --pure bash cmake
```

But with the list of package to install increasing, it is more convenient
to store this list in a manifest file. In this case `lmgc90-manifest.scm`,
which has the following structure:
```
;; Manifest to build a LMGC90-friendly environment

(specifications->manifest
  (list
    "cmake@4.0.3"
  )
)
```

Which can be used like this:
```shell
guix time-machine --channels=lmgc90-channels.scm` -- shell --pure --manifest=lmgc90-manifest.scm
```

## Working in the environment:

As specified before, to run a interactive shell in which to test and/or
build some software, the following command can be used:
```shell
guix time-machine --channels=lmgc90-channels.scm` -- shell --pure --manifest=lmgc90-manifest.scm
```
But not any of the convenenient tools to work within a terminal are present.
So the list of installed packaged can be initialized witht the following:
* `coreutils@9.1`
* `bash@5.2.37`
* `sed@4.9`
* `grep@3.11`

Question: after adding the `gfortran-toolchaine@14.3.0`, the following
message appeared:
```shell
guix shell: warning: ambiguous package specification `gfortran-toolchain@14.3.0'
guix shell: warning: choosing gfortran-toolchain@14.3.0 from gnu/packages/commencement.scm:3766:2
The following derivation will be built:
  /gnu/store/jbad24xwsfdd5rm0mlp4zsbiq020gzh2-profile.drv
```
How to properly get rid of this ?


## Building LMGC90:

By iterating between `guix search` and amending the manifest file and
testing the build of LMGC90 in a `guix shell` dedicated to it, there
are now the channels and manifest file allowing to build LMGC90.

First question: the package `python-scipy@1.12.0` and `python-hdf5@3.13.0`
need to use `python-numpy@1.26.4`. So it was needed to downgrad the
version of numpy. How, probably using 'guix tranformation' is it possible
to update numpy and tell the other package depending on it to use
the more recent one ?

On the first try on `IO` cluster, there is an error when building
`openblas-openmp`, this is probably not linked to `guix` but linked
to build `openBLAS` with `openMP` on this specific `AMD` architecture.
How to inquire/solve this problem ? Should the packager get contacted
or reproduce the build environment and inquire on the error to transforme
the packaging recipe ?

Furthermore, there is a path conflict with the default user profile.
`gcc-toolchain` was installed in the default profile of `guix` (which
was version 15). This created a conflict a build time because the
headers of g++ 15 where found before those of g++14. So the environment
was not as clean as expected... Maybe trying to create dedicated
profile would solve this.

## Defining as a package

... todo !

