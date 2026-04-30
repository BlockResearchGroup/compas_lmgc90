import os
import sys

# On Windows, register this package directory so that the bundled DLLs
# (libwrap_lmgc90_compas.dll, liblmgc90-core.dll, libopenblas.dll, the
# MinGW runtime DLLs, …) are findable for transitive loads from _lmgc90.pyd.
# Python 3.8+ already adds the .pyd's own directory to the search path for
# its direct imports, but transitive dependencies use the standard search
# order which would otherwise miss this directory.
if sys.platform == "win32" and hasattr(os, "add_dll_directory"):
    os.add_dll_directory(os.path.dirname(os.path.abspath(__file__)))

__version__ = "0.1.6"
__all__ = ["__version__", "Solver"]
