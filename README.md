# compas_lmgc90

Python bindings for LMGC90 discrete element solver.

## Installation

### Conda

With conda alreay installed, create a conda environment from the yaml file
```bash
conda env create -f environment.yml
```
Then activate it and install the module inside it:
```bash
conda activate compas_lmgc90-dev
python -m pip install .
```


### Linux

```bash
# Install dependencies
sudo apt update
sudo apt install -y gfortran libopenblas-dev liblapack-dev cmake

# Clone and install
git clone https://github.com/BlockResearchGroup/compas_lmgc90.git
cd compas_lmgc90
conda create -n lmgc90 python=3.12 -y
conda activate lmgc90
conda install -c conda-forge libstdcxx-ng=14
pip install -r requirements-dev.txt
pip install --no-build-isolation -ve .
```

**Note:** The `libstdcxx-ng=14` is required because the module is compiled with GCC 13.3+ and needs GLIBCXX_3.4.32.

### macOS

```bash
# Install dependencies
brew install gcc openblas cmake

# Clone and install
git clone https://github.com/BlockResearchGroup/compas_lmgc90.git
cd compas_lmgc90
conda create -n lmgc90 python=3.12 -y
conda activate lmgc90
pip install -r requirements-dev.txt
pip install --no-build-isolation -ve .
```

**Note:** CMake will auto-detect Homebrew GCC/gfortran. If detection fails, set compilers manually:
```bash
export FC=$(brew --prefix gcc)/bin/gfortran-14
export CC=$(brew --prefix gcc)/bin/gcc-14  
export CXX=$(brew --prefix gcc)/bin/g++-14
```

### Windows

Windows is not currently supported. The package requires Fortran compilers and POSIX-style build tools.

## Usage

```python
from compas_dem.models import BlockModel
from compas_dem.templates import ArchTemplate
from compas_lmgc90.solver import Solver

# Create model
template = ArchTemplate(rise=3, span=10, thickness=0.5, depth=0.5, n=20)
model = BlockModel.from_boxes(template.blocks())

# Run simulation
solver = Solver()
solver.geometry_from_model(model)
solver.set_supports(z_threshold=0.4)
solver.contact_law("IQS_CLB", 0.35)  # required as soon as blocks touch
solver.preprocess()
solver.run(nb_steps=100)
solver.finalize()
```

Re-running this in the same Python process (Rhino's ScriptEditor, Jupyter) is
supported; `finalize()` is optional, the next `Solver()` resets LMGC90's state.
LMGC90 holds exactly one simulation per process, so only the most recently
created `Solver` is usable — an older one raises `RuntimeError` instead of
crashing. Keep results (`solver.trimeshes`, `solver.get_contacts()`) around,
not the solver itself.

## Examples

```bash
python temp/dem_of_an_arch.py
python temp/dem_of_a_wall.py
python temp/dem_of_a_dome.py
python temp/contacts.py
```

## Troubleshooting

### ImportError: libmatlib.so not found

Make sure you installed the package with `pip install -e .` in the correct conda environment. The LMGC90 libraries are bundled during installation.

### GLIBCXX version error (Linux)

Install the required libstdc++:
```bash
conda install -c conda-forge libstdcxx-ng=14
```

### macOS: Compiler not found

If CMake can't find gfortran, manually set the compilers before building:
```bash
export FC=$(brew --prefix gcc)/bin/gfortran-14
export CC=$(brew --prefix gcc)/bin/gcc-14
export CXX=$(brew --prefix gcc)/bin/g++-14
pip install --no-build-isolation -ve .
```

### macOS: Compiler not found

If CMake can't find gfortran, manually set the compilers before building:
```bash
export FC=$(brew --prefix gcc)/bin/gfortran-14
export CC=$(brew --prefix gcc)/bin/gcc-14
export CXX=$(brew --prefix gcc)/bin/g++-14
pip install --no-build-isolation -ve .
```

## Contributing

### Testing a Pull Request

```bash
# Fetch the PR branch (replace PR_NUMBER with the actual number)
git fetch upstream pull/PR_NUMBER/head:pr-branch-name
git checkout pr-branch-name

# Build and test
pip install --no-build-isolation -ve .
python temp/contacts.py
```

### Pushing changes to a PR

```bash
# Add the PR author's fork as a remote
git remote add author https://github.com/AUTHOR/compas_lmgc90.git

# Push your changes to their branch
git push author branch-name
```

**Note:** This requires the PR author to have enabled "Allow edits from maintainers" on their PR.
