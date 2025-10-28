# Using GUIX

[GUIX](https://guix.gnu.org/en/) is meant to help
with the management of package and provide fully
reproductible environments.

Here are some instructions on how to build/use LMGC90
with GUIX.

### Sanity checks

This guide assumes that the `guix` command is already
available (maybe on a cluster or installed on your
computer).

First, make sure that there is no conflict between GUIX
and your default shell by running:

```shell
guix shell --pure --check
```

### Choose build or use

This explanation will provide you two way to use guix
for LMGC90:
1. a shell environment allowing to build LMGC90
2. a way to install LMGC90 as guix package for direct use

The first solution will also allow you to use LMGC90
as a regular package installed on your system provided
that you either install it a python virtualen environment
or provide the corresponding `PYTHONPATH`.

The second is more direct and easier to use, but provide
only a user version.

In any case, if the shell is missing some other tools
use during your computation, it cannot be done once
the shell is started. It  must be done beforehand by
amending the input file used (`manifest.scm` or `lmgc90-package.scm`).

In this tutorial, to avoid any update, all `guix` command
will use the `time-machine` subcommand. Have a look into
GUIX if you want to understand why.

### Build from source

GUIX can start a shell in which some packages are installed
(and does it in a smart way that it is not build each time).
It can be used in same way that an python virtual environment,
the difference being that by defautl, the environment is not
stored in your home directory.

Anyway by running the following command (which may be slow
the first time):
```bash
guix time-machine --channels=channels.scm -- shell --pure --manifest=manifest.scm bash
```
you will be in a bash shell with all pre-requisites installed
to build LMGC90. Follow the regular procedure and you will
have your own version of LMGC90.

Then later you can rerun the previous `guix time-machine [...]` command
and then, provided that your set your `PYTHONPATH` you will be able
to run some computations.

### Install as package

---

** Warning **
On `io` cluster, tests must be deactivated for
the installation to finished properly

---

GUIX can start a shell in which LMGC90 is installed
with all its needed dependencies. To do that and
run a `command.py` script, you only need to do:
```shell
guix time-machine --channels=channels.scm -- shell --pure --file=lmgc90-package.scm -- python command.py
```

In this particular case, only the minimum to install/run LMGC90 is
provided in this environment. If, for any reason, you want to have
a full fonctionnal shell, the you must modify the last line of the
`lmgc90-package.scm` to install, not only LMGC90, but also all the
other package you may need (like `bash` and `coreutils` if you want
a functionnal shell... and any text editor you might need).

### Environment variable

Sometimes some environment variable must be preserve within the
started shell. To do that the `--preserve` option can be used.
For example within a slurm script it may be needed to do
something like:
```shell
guix time-machine --channels=channels.scm \
                  -- shell --pure --file=lmgc90-package.scm \
                           --preserve=^SLURM     \
                           --preserve=^OMP       \
                           --preserve=PYTHONPATH
                  -- python command.py
```
Such command will keep the value of the environment variables
`PYTHONPATH` and all witht the name starting with `SLURM` or `OMP`.


### Guix profile

In the presented use of GUIX a clean environment is started each
time with everything installed inside it. Because of its design,
it should be quite fast after the first time to start such environment.

Despite this efficiency, it is sometimes desirable to create a
enviromment similar to a python virtual environment so that LMGC90
package can be installed and some other useful tools used on a regular
basis.

To do that the `--profile` option must be provided when installing.
The value is a directory which will hold the consecutive installation
made (so its parent must exist):
```shell
mkdir -p $HOME/.guix-extra-profiles
GUIX_LMGC90_PROFILE=$HOME/.guix-extra-profiles/lmgc90
guix time-machine --channels=channels.scm -- package --profile=$GUIX_LMGC90_PROFILE \
                                                     --install-from-file=lmgc90-pacakge.scm
```

Then it can be used more directly by:
```shell
guix shell --pure --profile=$GUIX_LMGC90_PROFILE -- python command.py
```

If something must be added:
```shell
guix time-machine --channels=channels.scm -- package -i coreutils bash vim
```

Then in the next call it will be usable:
```shell
guix shell --pure --profile=$GUIX_LMGC90_PROFILE bash
```

---
** Note **

If you are using profile, depending on your personnal preferences,
it may be desirable to change the command prompt format. To do this
you can add to your shell configuration file (usually `$HOME/.bashrc`):
```shell
export GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
export GUIX_LMGC90_PROFILE=$GUIX_EXTRA_PROFILES/lmgc90
if [ -n "$GUIX_ENVIRONMENT" ]; then
        export PS1="\[\033[01;96m\]\u@\h\[\033[00m\]:\[\033[01;95m\][$GUIX_ENVIRONMENT]\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ "
fi
```
To adapt to your preference [with some help](https://misc.flogisoft.com/bash/tip_colors_and_formating)


---
