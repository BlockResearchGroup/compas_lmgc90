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

Does not work.

```bash

```

### Package itself


```bash
conda remove -n lmgc90 --all -y
conda create -n lmgc90 python=3.12 -y
conda activate lmgc90
pip install numpy compas compas_viewer nanobind
git clone https://github.com/petrasvestartas/compas_lmgc90.git
cd compas_lmgc90
pip install -e .
```


## Run Example

```bash
python test_nanobind_exact.py
```

### Mac


