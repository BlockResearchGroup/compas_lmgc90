__version__ = "0.1.0"

from .parse_bodies import parse_bodies_dat, create_mesh_from_body

# Import C++ extension modules
try:
    from . import _lmgc90
except ImportError:
    _lmgc90 = None

try:
    from . import _lmgc90_iterative
except ImportError:
    _lmgc90_iterative = None

__all__ = ["__version__", "parse_bodies_dat", "create_mesh_from_body", "_lmgc90", "_lmgc90_iterative"]
