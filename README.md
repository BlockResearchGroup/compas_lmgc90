# compas_lmgc90

Binding for LMGC90. 

Prerequisites: [Anaconda](https://www.anaconda.com/download)

## Development Installation

```bash
conda create -n lmgc90 -c conda-forge python=3.10
conda activate lmgc90
git clone https://github.com/BlockResearchGroup/compas_lmgc90.git
cd compas_lmgc90
pip install -e ".[dev]"
```

## Run

```bash
cd ~/brg/code_fortran/compas_lmgc90/ && clear && source /opt/intel/oneapi/setvars.sh --force && pip install -e . --force-reinstall --no-deps && cd src/lmgc90_dev/src/Sandbox/Compas && python /home/pv/brg/code_fortran/compas_lmgc90/temp/visualize_with_orientation_iterative.py
```

## Documentation

For further "getting started" instructions, a tutorial, examples, and an API reference,
please check out the online documentation here: [compas_lmgc90 docs](https://BlockResearchGroup.github.io/compas_lmgc90)

## Issue Tracker

If you find a bug or if you have a problem with running the code, please file an issue on the [Issue Tracker](https://github.com/BlockResearchGroup/compas_lmgc90/issues).
