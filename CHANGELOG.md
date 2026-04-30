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
  renamed/removed in newer `compas_cgal`).
- *strip-url-whitespace*: GitHub-secret pastes routinely capture a
  trailing newline. CMake now `string(STRIP ...)` and regex-strips
  trailing whitespace from `LMGC90_GIT_URL` before handing it to
  `FetchContent`. Without this guard, git surfaces
  `fatal: credential url cannot be parsed: ***` /
  `warning: url contains a newline in its path component: ***` and
  every CI job dies during LMGC90 clone.
- *conda-removed-from-ci*: the `Build (ubuntu-latest)` smoke-test job
  used `conda-incubator/setup-miniconda@v3` + `libstdcxx-ng` +
  `compas-dev/compas-actions.build@v4` to test an editable conda-based
  install. That path is orthogonal to what ships on PyPI and adds
  several minutes per run. Dropped entirely; the cibuildwheel
  manylinux + windows wheel jobs already exercise the actual wheel
  artifact (build → install → import smoke test).
- *false-green-import-failure*: the cp312 wheel built and "passed" its
  test-command, but `dumpbin -dependents _lmgc90.pyd` showed lingering
  imports on `libgfortran-5.dll` and `libwinpthread-1.dll` despite the
  `-Wl,-Bstatic -lgfortran -lquadmath -lwinpthread -Wl,-Bdynamic` block.
  Root cause: CMake auto-injects `gfortran`, `quadmath`, `winpthread`
  into the link line via `CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES` (and
  the C/CXX equivalents) using their bare names. ld resolves bare
  `-lgfortran` to `libgfortran.dll.a` (the import lib for the DLL)
  during its single-pass scan, *before* our trailing `-Bstatic` block
  takes effect — so the DLL ends up in the import table even when we
  asked for a static link. Fix: `list(REMOVE_ITEM
  CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES gfortran quadmath winpthread)`
  immediately after `project()`, then feed the static archives back in
  by absolute path on the `_lmgc90` target (`libgfortran.a`,
  `libquadmath.a`, `libwinpthread.a` from the bootstrapped WinLibs
  cache). Bypassing `-l<name>` resolution entirely guarantees the
  static archive is used. Also unmasked the cibuildwheel
  test-command's `import _lmgc90` so a future DLL-load regression
  fails CI loudly instead of being hidden behind `|| echo`.
- *transitive-mingw-runtime*: bundling libgfortran/libquadmath/
  libwinpthread alongside `_lmgc90.pyd` was not enough — `import
  _lmgc90` still failed with `DLL load failed while importing
  _lmgc90: The specified module could not be found`. The .pyd
  itself loads (Python's `LOAD_WITH_ALTERED_SEARCH_PATH` finds the
  3 sibling DLLs), but `libgfortran-5.dll` has its own transitive
  imports on `libgcc_s_seh-1.dll` (and `libquadmath-0.dll` does
  too). Our `-static-libgcc` only embeds libgcc into our `.pyd`,
  not into the prebuilt `libgfortran-5.dll` shipped by WinLibs.
  When the loader pulls in libgfortran-5.dll, it then needs to
  resolve `libgcc_s_seh-1.dll` via the same altered-search rules
  ("the directory containing the DLL"), which on Python 3.8+ is
  the only non-system path searched. So we bundle libgcc_s_seh
  too, plus a defensive set of every MinGW runtime that LMGC90
  contribs might transitively pull (libstdc++, libssp, libgomp,
  libatomic) — the bundled set is harmless when unused.
- *gnu-ld-single-pass*: even with absolute-path static archives, the
  link still failed with thousands of `undefined reference to
  _gfortran_st_close` from `liblmgc90-core.a(mod_*.f90.obj)`. gnu ld
  is single-pass: by the time it visits liblmgc90-core.a (a static
  archive of thousands of Fortran .obj files), it has already
  processed libgfortran.a earlier in the link line and refuses to
  look back. Wrapping every archive in `--start-group/--end-group`
  would work but requires enumerating every transitive contrib
  LMGC90 propagates (no stable list); CMake's `LINK_GROUP` genex
  doesn't reach into INTERFACE_LINK_LIBRARIES of dependencies.
  Switched to bundling all three runtime DLLs (libgfortran-5.dll,
  libquadmath-0.dll, libwinpthread-1.dll) alongside _lmgc90.pyd.
  They're direct deps of the .pyd (or transitive deps of
  libgfortran-5.dll, which the loader resolves from the same
  directory), so Windows' LOAD_WITH_ALTERED_SEARCH_PATH picks them
  up without an os.add_dll_directory shim — same pattern that
  already works for libopenblas.dll. The wheel ships _lmgc90.pyd +
  4 sibling DLLs; libgcc/libstdc++ remain statically embedded via
  the gcc/g++ driver flags.
- *winpthread-dll-only*: switching to absolute-path static archives
  hit `ninja: error: 'libwinpthread.a' missing` because WinLibs r7
  ships winpthread as DLL-only (no `libwinpthread.a` in `mingw64/lib`).
  Made the static-vs-dynamic decision per-runtime: probe each candidate
  (`gfortran`, `quadmath`, `winpthread`) for a `lib<name>.a` in the
  bootstrapped WinLibs lib dir; if present, strip the bare name from
  `CMAKE_<LANG>_IMPLICIT_LINK_LIBRARIES` and feed the static archive
  back in by absolute path on `_lmgc90`; if absent, leave the dynamic
  linkage in place and `install(FILES lib<name>-*.dll)` alongside
  `_lmgc90.pyd`. Windows resolves direct-dep DLLs of a `.pyd` from the
  module's directory via `LOAD_WITH_ALTERED_SEARCH_PATH`, so the
  bundled DLLs load without `os.add_dll_directory` or any package-init
  shim — same shape as `libopenblas.dll`.
- *rhino8-cp39*: dropped `STABLE_ABI` from `nanobind_add_module` and
  removed `cp39-*`, `cp310-*`, `cp311-*` from the cibuildwheel `skip`
  list. nanobind's stable ABI is gated on Python ≥ 3.12, but Rhino 8
  ships CPython 3.9.10 / 3.9.12 — without per-Python wheels, Rhino
  users would fall through to an sdist build that needs the WinLibs +
  OpenBLAS bootstrap to run on their machine. cibuildwheel now emits
  cp39, cp310, cp311, cp312, cp313 Windows wheels.

- *libgomp-glob-tightened*: the `libgomp-*.dll` glob accidentally
  pulled in `libgomp-plugin-nvptx-1.dll` (libgomp's CUDA/NVPTX
  offload plugin) — harmless on CPU code but useless and a future
  load-failure risk if the runner ever has CUDA libs on PATH.
  Switched all runtime globs to use a `[0-9]*` digit suffix so they
  match `lib<name>-1.dll` etc. but skip `lib<name>-plugin-*.dll`.
- *release-windows*: extended `release.yml`'s matrix to include
  `windows-latest` and pass `LMGC90_GIT_URL` so tag-triggered
  releases produce both Windows and manylinux wheels for
  PyPI publish.

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

