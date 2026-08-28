# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## Unreleased

### Added

### Changed

### Removed

## [0.1.11] 2026-08-28

### Added

- `tests/test_reinit.py::test_reinit_solve_with_contacts`,
  `::test_reinit_solve_with_contacts_implicit_finalize`,
  `::test_reinit_solve_varying_model_size` and
  `::test_reinit_rebound_global_without_finalize`: complete solves of
  touching unit cubes repeated in one process — three identical runs with
  an explicit `finalize()`, three where the destructor finalizes instead,
  four runs of small -> large -> small -> large models, and three
  re-executions of `solver = Solver(...)` in one persistent namespace (the
  Rhino ScriptEditor / notebook form, where the new solver is initialised
  before the old one is released).
  Built from raw vertices/faces via `geometry_from_v_f`, so they need no
  `compas_dem` and run on every Linux/macOS/Windows wheel and CPython
  version of the cibuildwheel matrix. The pre-existing `lowlevel` reinit
  tests never add geometry, so contact detection never runs and they could
  not see the crashes fixed below.
- `tests/test_reinit.py::test_superseded_solver_raises_instead_of_crashing`
  plus `test_run_before_preprocess_raises`, `test_preprocess_twice_raises`,
  `test_preprocess_without_contact_law_raises` and
  `test_overlong_contact_law_name_raises`: misuse must raise a Python
  exception, never end in a Fortran `STOP`.

### Changed

- `Solver.preprocess()` now raises `RuntimeError` when no contact law was
  set, when it is called a second time on the same solver (including a
  retry after a failure half-way through), and `Solver.run()` raises when
  `preprocess()` has not run. Each of these used to reach LMGC90, which
  reports the misuse with a Fortran `STOP 1` — i.e. the whole host process
  (Rhino) exits with no Python traceback. Note that `contact_law()` is now
  mandatory before `preprocess()`; every example already called it, and the
  README usage snippet now does too.
- `LMGC90Solver.add_one_tact_behav` validates the nickname (exactly 5
  characters) and the law name (at most 30 characters) and builds a
  blank-padded 30-byte buffer, instead of `strncpy`-ing `law.size()` bytes
  into a 30-byte stack array (stack overflow for any longer name).
  `set_one_polyr` validates the material name the same way.
- `tests/test_placeholder.py` no longer shrinks the arch blocks by 10 %
  (which left zero contacts), sets a contact law, and asserts that contacts
  were detected.
- `[tool.cibuildwheel] test-command` now runs the whole `tests/` directory
  against every built wheel instead of two hand-picked test ids, so every
  test — including new ones — runs on all three OSes and all CPython
  versions. The `compas_dem`-based tests keep skipping themselves if
  `compas_dem` cannot be imported in the slim test env.
- `tests/test_reinit.py::test_reinit_full_solver_cycle` (and the arch in
  it) no longer shrinks the blocks by 0.9: that opened gaps wider than the
  alert distance, produced zero contacts, skipped detection entirely, and
  so passed on the 0.1.10 wheel that crashed on every second real solve.
  It now mirrors the reported flow (`is_support` on the end blocks,
  `set_supports_from_model()`) and asserts `nb_contacts > 0`.

### Fixed

- The second solve in one process crashed the interpreter with SIGSEGV
  inside `STO_compute_contact_PRPRx` — in Rhino the whole application, in
  plain CPython the script. STO detection keeps a procedure-local `save`d
  first-call flag (`is_first_time_f2f` in the reporter's reading of
  `mod_PRPRx.f90`); LMGC90's `clean_memory_PRPRx` resets the `wcp` and
  `f2f4all` detection methods but not STO, while still freeing the
  `visavis` arrays that flag guards, so the second run skipped
  re-detection and dereferenced freed memory. `reset_all_state()` now
  calls `sto_compute_contact_PRPRx(.true.)` before `clean_memory_PRPRx`,
  on both the `finalize()` path and the defensive reset in `initialize()`.
  Verified on the shipped 0.1.10 Linux wheel: the same reset applied
  between runs turns a guaranteed crash on run 2 into 100 consecutive
  clean runs of a 100-block model. Thanks to the user who reported it with
  a root-cause analysis. The upstream fix belongs in LMGC90's
  `clean_memory_PRPRx`; the wrapper-side reset stays regardless.
- `reset_all_state()` also calls `set_time_step(0.d0, .true.)`, so the
  `overall` module takes the same first-call branch on every run (it kept
  a `save`d first-call flag too; no functional effect for this wrapper,
  which never uses adaptive time stepping).
- A second, independent way to crash the second run: `solver = Solver(...)`
  re-executed in a persistent namespace (Rhino's ScriptEditor, a notebook
  cell) with no `finalize()`. Python constructs and initialises the NEW
  solver first and drops the OLD one afterwards, and the old C++ destructor
  then called `lmgc90_finalize()`, wiping the new solver's freshly
  initialised state; the next native call died with `STOP 1` in
  `POLYR::read_bodies`. `LMGC90Solver` now tracks which instance owns the
  process-global LMGC90 state (`g_owner`, set by `initialize()`): a
  superseded instance releases nothing on `finalize()`/destruction, and
  every one of its native-facing methods raises `RuntimeError("... was
  superseded by a newer one ...")` instead of driving somebody else's
  simulation. `get_initial_state()`/`compute_one_step()` also verify that
  LMGC90's body count still matches the one registered by this instance,
  rather than returning uninitialised coordinates when it does not.
  Covered by `test_reinit_rebound_global_without_finalize` and
  `test_superseded_solver_raises_instead_of_crashing`.

### Removed

## [0.1.10] 2026-08-18

### Fixed

- `CMakeLists.txt` reads the LMGC90 source URL from `LMGC90_GIT_URL` again.
  It had been renamed to `LMGC90_18_08_2026` (the name of a GitLab personal
  access token), while all five workflow files still export the environment
  variable as `LMGC90_GIT_URL`. CMake therefore looked up a variable nothing
  sets, and every CI build failed at the `FATAL_ERROR`.
- Typo in that same `FATAL_ERROR` message ("Neither ... no" -> "nor").

### Fixed

- macOS wheels: Homebrew GCC is pinned to `gcc@15` instead of the unversioned
  `gcc` formula, which tracks the newest major release. After Homebrew moved
  past the version used by the last green build (2026-05-04), linking
  `_lmgc90` on arm64 failed with undefined `nanobind::detail::func_create<...>`
  template instantiations from `lmgc90.cpp.o`. The detection now probes for a
  real `gfortran` binary and falls back to unversioned `gcc` if the pin is
  unavailable.

### Changed

- LMGC90 is now pinned to commit `aa4687c2` (tip of `fix_compas`, 2026-07-22)
  instead of the `compas_dev` tag (`bcae5bc2`, 2026-04-17). A commit SHA cannot
  be repointed the way a tag can, and this is the revision the wheel was
  verified against.
- `.gitignore` now excludes LMGC90's simulation output directories
  (`OUTBOX/`, `DISPLAY/`, `POSTPRO/`). Running the examples in `temp/`
  writes over 10,000 result files next to the script that was run.

## [0.1.9] 2026-05-04

### Added

- cibuildwheel macOS `repair-wheel-command` now includes a guard that fails
  the build if any OpenMP runtime (`libomp`, `libgomp`, `libiomp`) ends up
  inside the wheel. Regression guard for the issue fixed below.

### Changed

- macOS BLAS/LAPACK now resolves to Apple's Accelerate framework via
  LMGC90's built-in `BLA_VENDOR=Apple`, instead of Homebrew OpenBLAS.
  Fixes `OMP: Error #15: Initializing libomp.dylib, but found libomp.dylib
  already initialized` when `compas_lmgc90` is imported into a conda env
  that already has `libomp.dylib` loaded (NumPy/SciPy/MKL). Homebrew's
  `libopenblasp` was a threaded build linked against Homebrew's
  `libomp.dylib`, which `delocate` then bundled into the wheel — colliding
  with the conda runtime's copy. Apple Accelerate has no OpenMP runtime
  dependency, so nothing libomp-related is bundled anymore.

### Removed

- Homebrew OpenBLAS detection block from `CMakeLists.txt` (was forcing
  `BLAS_LIBRARIES`/`LAPACK_LIBRARIES` to the keg path before LMGC90's
  `BLA_VENDOR=Apple` could take effect).
- `openblas` from the macOS `before-all` Homebrew install — only `gcc`
  (for `gfortran-<major>`) is needed now.


## [0.1.8] 2026-04-30

### Added

- macOS arm64 wheels via cibuildwheel on `macos-14`. CMakeLists.txt's
  Apple block now auto-discovers Homebrew GCC (for `gfortran-<major>`)
  *and* Homebrew OpenBLAS via `brew --prefix`, mirroring the Windows
  bootstrap pattern. cibuildwheel's `before-all` installs both Homebrew
  formulas; the default `delocate` repair tool bundles libgfortran,
  libgcc_s, libstdc++, libquadmath, and libopenblas into the wheel
  automatically.
- `tests/test_reinit.py`: regression test for the "Rhino crashes the
  second time you run the script" bug. Exercises both
  `LMGC90Solver().initialize().finalize()` cycles in a loop (low-level,
  no `compas_dem` needed — runs in cibuildwheel's slim test env) and
  the full `Solver(model)` lifecycle (gated on `compas_dem`).
- `Solver.__enter__` / `Solver.__exit__`: context-manager support, so
  `with Solver(model) as solver: solver.run(...)` finalizes
  deterministically at scope exit even on exceptions.
- `Solver.__del__`: GC-time finalize backstop. The C++ destructor
  already finalizes when the bound `LMGC90Solver` is collected, but
  this lets early Python-level finalize keep up with re-instantiation
  patterns (Rhino, Jupyter, long-lived services).
- `docs/examples/dem_of_an_arch_rhino.py`: Rhino 8 ScriptEditor example
  using the `# r:` directive to install `compas_lmgc90` on demand.

### Changed

- Removed the `[[tool.scikit-build.overrides]]` darwin block that
  hardcoded `clang/clang++/gfortran`. It shadowed the existing
  CMakeLists.txt auto-detection and mixed Apple's libc++ (from clang++)
  with Homebrew gcc's libgfortran/libgcc_s — linkable but a runtime ABI
  minefield. Pure Homebrew gcc end-to-end is cleaner and matches the
  Linux + Windows stories (gfortran from the same toolchain that
  compiles the wrapper).
- `[tool.cibuildwheel.macos]`: dropped `skip = "*"`; set `archs =
  ["arm64"]`, `before-all = "brew install gcc openblas"`, and
  `MACOSX_DEPLOYMENT_TARGET = "14.0"` (matches the pinned `macos-14`
  runner; bottle deployment-target propagates from runner to wheel).
  Covers Apple Silicon Macs running macOS 14 (Sonoma) or later;
  x86_64 / Intel Mac wheels would need a separate `macos-13` matrix
  entry — to be added if Intel users come asking.
- `build.yml` and `release.yml` matrices: enabled the previously
  commented-out macOS entry (pinned to `macos-14`), wiring
  `LMGC90_GIT_URL` through the same way as the manylinux + windows
  jobs.
- `wrap_lmgc90_compas.f90`: extended the wrapper's `finalize()` so it
  cleans every LMGC90 module the wrapper touches, not just five of
  them. Previously cleaned: `PRPRx`, `POLYR`, `RBDY3`, `tact_behav`,
  `bulk_behav`. Now also cleans `nlgs_3D` (solver `this` ledger and
  scratch arrays — the actual culprit for second-run crashes, since
  its `this` array holds adjacency to PRPRx contacts that *were*
  cleaned, leaving dangling references), `models` (`modelz`/`ppset`
  arrays from the materials chain), `postpro_3D` (file unit handles
  and energy accumulators — no-op when postpro isn't started), and
  `overall` (entity registry, NSTEP, time, every global config flag).
  Cleanup order is consumers-first / providers-last: PRPRx → nlgs_3D
  → POLYR → RBDY3 → tact_behav/bulk_behav → models → postpro_3D →
  overall.
- `wrap_lmgc90_compas.f90:initialize()`: now calls the same cleanup
  helper as its first action. Defense-in-depth — even when a previous
  Solver instance's `__del__` ran late (or not at all, on interpreter
  shutdown), the next `initialize()` starts on a truly clean slate.
  This is what makes `Solver(...)` re-instantiation safe in long-lived
  processes like Rhino's ScriptEditor without the user having to call
  `finalize()` themselves.
- `Solver.finalize()`: now idempotent. Safe to call multiple times,
  safe to call on a partially-constructed instance.
- `Solver.__init__`: only creates `./OUTBOX/` when `debug=True`. The
  Fortran wrapper only writes diagnostics there inside `if(debug)`
  blocks, so the previous unconditional `Path("./OUTBOX").mkdir(...)`
  was creating an empty directory for nothing — and under Rhino's
  ScriptEditor, where the process cwd is unpredictable (often
  somewhere users can't write), it could fail outright.
- `[tool.cibuildwheel] test-command`: extended from a single-line
  import smoke test to a list that also runs the new reinit tests
  (`test_reinit_lowlevel` + `test_reinit_lowlevel_implicit_finalize`)
  on every wheel. A regressed wrapper that crashes on second-init
  fails the wheel build, not just at user runtime.

### Removed


## [0.1.7] 2026-04-30

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

