# compas_lmgc90

Python bindings for LMGC90 discrete element solver.

## Installation

### Linux

```bash
sudo apt update
sudo apt install -y gfortran libopenblas-dev liblapack-dev cmake
```

### Mac

```bash
brew install gcc
brew install openblas
```

### Windows

Windows is not currently supported. The package requires Fortran compilers and POSIX-style build tools. OneAPI can offer that.

### Package itself

```bash
git clone https://github.com/BlockResearchGroup/compas_lmgc90.git
cd compas_lmgc90
sudo apt-get update
sudo apt-get install -y gfortran cmake libopenblas-dev liblapack-dev
conda remove -n lmgc90 --all -y
conda create -n lmgc90 python=3.12 -y
conda install -n lmgc90 -y -c conda-forge libstdcxx-ng=14
conda activate lmgc90
pip install -r requirements-dev.txt
pip install --no-build-isolation -ve.
python temp/contacts.py
```

**Note:** The `libstdcxx-ng=14` installation is required because the module is compiled with GCC 13.3+ and needs GLIBCXX_3.4.32.


## Run Example

```bash
python temp/dem_of_an_arch.py
```

## Troubleshooting

### ImportError: libmatlib.so not found

Make sure you installed the package with `pip install -e .` in the correct conda environment. The LMGC90 libraries are bundled during installation.

### GLIBCXX version error

Install the required libstdc++:
```bash
conda install -n lmgc90 -y -c conda-forge libstdcxx-ng=14
```
