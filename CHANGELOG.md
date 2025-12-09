# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Added

- `Solver` class in `compas_lmgc90.solver` for high-level COMPAS integration
- Official LMGC90 Fortran wrapper (`wrap_lmgc90_compas.f90`) with C-compatible API
- `LMGC90Solver` C++ class exposing full solver lifecycle via nanobind
- Initial state capture from LMGC90 after `close_before_computing()` for proper transformation tracking
- Contact visualization via `Solver.get_contacts()` returning force lines, normals, and polygons
- Intel MKL libraries bundled in `external/ubuntu/intel_libs/`
- New examples: `dem_of_an_arch.py`, `dem_of_a_wall.py`, `dem_of_a_dome.py`, `dem_of_a_barrel_vault.py`, `dem_of_a_cross_vault.py`, `contacts.py`
- Data files: `crossvault.obj`, `wall.obj`

### Changed

- Build system now uses `FetchContent` to clone LMGC90 from upstream repository
- Simplified CMakeLists.txt: removed Eigen dependency, links directly to `lmgc90_F_lib`
- C++ bindings rewritten from single `solve()` function to full `LMGC90Solver` class
- Python API now object-oriented: `Solver(model)` with `preprocess()`, `run()`, `finalize()` methods

### Removed

- Local Fortran sources (`fortran_calc.f90`, `lmgc90_solver.f90`)
- Old C++ wrappers (`fortran_wrapper.cpp`, `lmgc90_solver_interface.h`)
- Eigen header dependency

