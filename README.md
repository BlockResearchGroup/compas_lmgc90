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
solver = Solver(model)
solver.set_supports(z_threshold=0.4)
solver.preprocess()
solver.run(nb_steps=100)
solver.finalize()
```

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

## Releasing

Releases are pushed to PyPI automatically by `.github/workflows/release.yml`
when a `v*` tag is pushed. There is no manual upload step.

### Cutting a release

From a clean `main` branch:

```bash
git checkout main
git pull --ff-only origin main

# 1. Bump version + create commit + create v0.X.Y tag.
#    Reads [tool.bumpversion] from pyproject.toml; bumps
#    src/compas_lmgc90/__init__.py and rewrites the "Unreleased"
#    heading in CHANGELOG.md to "[0.X.Y] YYYY-MM-DD".
bump-my-version bump patch --verbose

# 2. Re-add a fresh "## Unreleased" template above the just-frozen
#    version block so the next PR has somewhere to land.
python -m invoke prepare-changelog

# 3. Push the commits, then push the tag. The TAG PUSH is what
#    triggers release.yml — without the second push, nothing happens.
git push origin main
git push origin v0.X.Y
```

### What happens on tag push

`release.yml` triggers on `push` of any `v*` tag and runs four jobs:

| Job | Runner | Output |
|---|---|---|
| `create_release` | ubuntu-latest | GitHub Release for the tag |
| `build_wheels` (matrix) | ubuntu-latest, windows-latest, macos-14 | Per-platform wheels via `cibuildwheel` (cp39–cp313) |
| `build_sdist` | ubuntu-latest | Source distribution |
| `publish` | ubuntu-latest | Uploads all wheels + sdist to PyPI |

The `publish` job has `needs: [build_sdist, build_wheels]`, so it only
runs after every wheel platform succeeds. Authentication is via PyPI
**Trusted Publisher** (OIDC) — no API token, no manual step. The
trusted publisher is already configured for this repo on PyPI.

Total runtime: ~25–30 minutes.

### Required GitHub secret

`LMGC90_GIT_URL` — the (private) GitLab URL with embedded credentials
that CMake's `FetchContent` uses to clone LMGC90 source during the
build. Set in `Settings → Secrets and variables → Actions`. Wired into
all three workflows (`build.yml`, `release.yml`, `docs.yml`).

### Verifying the release

After `release.yml` finishes, confirm the artifacts on PyPI:

```bash
curl -s https://pypi.org/pypi/compas_lmgc90/json | python -c \
  "import json,sys; d=json.load(sys.stdin); v=d['info']['version']; \
   print(v, len(d['releases'][v]), 'artifacts'); \
   [print(' ', a['filename']) for a in d['releases'][v]]"
```

Expected: 5 manylinux + 5 windows + 5 macos arm64 wheels (cp39–cp313)
+ 1 sdist = **16 artifacts**.
