from compas_lmgc90 import _primitives  # The actual C++ module


def add(a, b):
    """Add two numbers together.

    Parameters
    ----------
    a : float
        The first number.
    b : float
        The second number.

    Returns
    -------
    float
        The sum of the two numbers.

    Examples
    --------
    >>> add(2.0, 3.0)
    5.0
    """
    return _primitives.add(a, b)
