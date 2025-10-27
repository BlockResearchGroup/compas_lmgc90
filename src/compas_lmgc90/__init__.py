__version__ = "0.1.0"

# Import C++ extension module first
try:
    from . import _lmgc90
except ImportError as e:
    import warnings
    warnings.warn(f"Could not import _lmgc90 module: {e}")
    _lmgc90 = None

# Import Python utilities
try:
    from .parse_bodies import parse_bodies_dat, create_mesh_from_body
    __all__ = ["__version__", "parse_bodies_dat", "create_mesh_from_body", "_lmgc90"]
except ImportError:
    __all__ = ["__version__", "_lmgc90"]
