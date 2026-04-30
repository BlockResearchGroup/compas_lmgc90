# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## Unreleased

### Added

- Windows wheel support via cibuildwheel on `windows-latest`.
- `cmake/BootstrapWindowsToolchain.cmake`: self-bootstrapping CMake module
  that, before `project()`, downloads pinned WinLibs MinGW-w64 (UCRT, gcc
  15.2.0) and OpenBLAS 0.3.33 binaries into a project-local cache and sets
  `CMAKE_{C,CXX,Fortran}_COMPILER` plus `BLAS/LAPACK_LIBRARIES`. Lets end
  users on a fresh Windows box run `pip install compas_lmgc90` (or
  `pip install .` from source) without first installing MSYS2, vcpkg, or
  Intel oneAPI. Cache lives at `%LOCALAPPDATA%\compas_lmgc90\toolchain` by
  default; override via `COMPAS_LMGC90_TOOLCHAIN_CACHE`.
- `[tool.cibuildwheel.windows]`: `before-build` installs delvewheel and
  `repair-wheel-command` vendors `libopenblas.dll` plus the dynamic
  toolchain runtimes (libgcc_s, libstdc++, libgfortran, libquadmath,
  libwinpthread) into the wheel from the bootstrapped toolchain cache.
- `workflow_dispatch` trigger and `windows_support` push trigger on the
  build workflow so Windows iteration doesn't require a PR.
- `.gitignore`: `.claude/` session state.

### Changed

- `cmake_minimum_required` bumped from 3.15 to 3.24 (needed for
  `file(ARCHIVE_EXTRACT)` in the Windows toolchain bootstrap).
- `INSTALL_RPATH` is no longer set on Windows targets — Windows resolves
  DLLs via the loading module's directory, so co-installation in
  `compas_lmgc90/` is sufficient.
- `build_wheels` job no longer gates on the Linux conda `Build` job; it
  runs immediately on push so Windows iterations don't wait on the
  orthogonal Linux smoke test.

### Iteration log (Windows bring-up)

- *forward-slashes-fix*: cibuildwheel evaluates `environment` table values
  as bash, so `'C:\toolchain-cache'` collapsed to `'C:<TAB>oolchain-cache'`
  (`\t` → tab) and the zip extraction failed. Switched the cache path and
  the delvewheel `--add-path` argument to forward slashes.
- *static-link-revert*: tried `-static-libgcc -static-libstdc++
  -static-libgfortran` to keep the wheel slim, but on MinGW each contrib
  DLL ended up statically embedding `libgcc_eh.a` AND re-exporting its
  symbols via auto-export. The next link step that pulled two such DLLs
  in failed with `multiple definition of '_Unwind_Resume'`. Switched to
  dynamic toolchain linking; delvewheel bundles the runtime DLLs.
- *export-all-symbols-removed*: dropped `WINDOWS_EXPORT_ALL_SYMBOLS=ON`
  on the SHARED targets — MinGW already auto-exports every global, and
  CMake's `nm`-driven .def generation can be more restrictive.
- *lmgc90-install-off*: cp312 wheel built (20 MB) but `delvewheel repair`
  failed: `Unable to find library: libwrap_lmgc90_compas.dll`. Cause:
  LMGC90's own install rules (active when `ChiPy`/`PRE` are off) used
  `GNUInstallDirs` to dump `bin/`, `lib/`, `include/`, `modules/` at the
  wheel root, alongside our `compas_lmgc90/` package. delvewheel's
  per-package DLL search couldn't find the wrap DLL through the polluted
  layout. Setting `LMGC90_INSTALL=OFF` keeps only our explicit
  `install(TARGETS ...) → compas_lmgc90/` rules.
- *delvewheel-dropped*: with `LMGC90_INSTALL=OFF` the wheel was cleaner
  (16.7 MB) but delvewheel still failed with the same error — its DLL
  search apparently doesn't include subpackage directories where
  `libwrap_lmgc90_compas.dll` actually lives. Switched to manual DLL
  bundling, then realized that whole shape was wrong: shipping 11 DLLs
  with sibling-loading via `os.add_dll_directory` is a workaround for
  Python 3.8+'s hardened DLL search, not a proper architecture.
- *full-static-link*: switched to `LMGC90_BUILD_SHARED_LIBS=OFF` (every
  LMGC90 contrib + lmgc90_F_lib become static archives), made
  `wrap_lmgc90_compas` a STATIC archive, and statically linked the MinGW
  runtimes (`libgcc`, `libstdc++`, `libgfortran`, `libquadmath`,
  `libwinpthread`) into the final `_lmgc90.pyd`. The wheel now ships
  just `_lmgc90.pyd` + `libopenblas.dll`, with the BLAS DLL found via
  Python's default `LOAD_WITH_ALTERED_SEARCH_PATH` for direct imports —
  no `os.add_dll_directory`, no delvewheel, no transitive DLL chains.
  The earlier static-link attempt failed with duplicate `_Unwind_Resume`
  because contribs were also SHARED and each re-exported `libgcc_eh.a`
  symbols; with contribs static, that single final link sees libgcc
  exactly once.
- *wheel-exclude*: scikit-build-core `wheel.exclude` (Windows-only) strips
  any stray `bin/`, `lib/`, `include/`, `modules/`, `share/` from the
  wheel root that LMGC90's vendored Clipper2 installs via
  `GNUInstallDirs`.
- ✅ *green*: cp312 + cp313 Windows wheels build, install, and import
  successfully on `windows-latest` runners (run #25165016826). Wheels are
  ~15 MB each, containing exactly `_lmgc90.pyd` + `libopenblas.dll` +
  the `compas_lmgc90` Python sources. Total Windows wheel build time:
  ~12 min for both Python versions including the ~700 MB WinLibs +
  40 MB OpenBLAS download. Test command also imports `_lmgc90` to
  guard against regressions where a wheel installs but doesn't load.
- *secret-extraction*: removed the hardcoded GitLab personal access token
  from `CMakeLists.txt`. The LMGC90 source URL is now read from the
  `LMGC90_GIT_URL` env var (or `-DLMGC90_GIT_URL=...` on the command
  line). The CI workflow injects it from `secrets.LMGC90_GIT_URL` and
  cibuildwheel's `environment-pass` forwards it into the manylinux
  Docker container. **Action required from the user:** rotate the
  leaked token at git-xen.lmgc.univ-montp2.fr and add the new full URL
  (with credentials) as a GitHub Actions repository secret named
  `LMGC90_GIT_URL`.
- *test-skip-on-dep-mismatch*: `tests/test_placeholder.py` now skips
  cleanly via `pytest.skip(allow_module_level=True)` when `compas_dem`
  fails to import (it transitively pulls in
  `compas_cgal.meshing.project_mesh_on_mesh`, which has been
  renamed/removed in newer `compas_cgal`). The Linux conda Build job
  was failing solely because of this import-time error.

### Removed


## [0.1.6] 2026-04-27

### Added

### Changed

### Removed


## [0.1.5] 2026-04-27

### Added

### Changed

- exclude "external", "venv", "build", "OUTBOX", "temp" due to PYPI limits.

### Removed

## [0.1.4] 2026-04-27

### Added

- Release

### Changed

### Removed

## [0.1.2] 2026-04-27

### Added

- Release

### Changed

### Removed


## [0.1.1] 2026-04-27

### Added

- New example: `dem_of_a_carbcomn_vault.py`
- New temp example: `dem_of_an_arch_force.py` demonstrating applied force on an arch

### Changed

- Fix some warning in `pyproject.toml`
- Fix examples where a contact law was missing
- Refactored `solver.py` code style (formatting, spacing)
- `get_post_processing_data` now uses the stored last result instead of triggering a new solver step

### Removed

