
.. py:currentmodule:: pylmgc90

.. _gmsh:

Use of GMSH python module
=========================

`gmsh <http://gmsh.info>`_ is a powerfull tool
to create, handle and work with mesh.

Here are added some tools using the Python API
of gmsh to provide some specific use seldom used
during pre-processing, computation or post-processing.

To use these features, the *gmsh* module must be installed
in your python environment. The recommendation is to simply
use: ::

  pip install --upgrade gmsh

In the terminal to install. Once this is done, the features
can be call by importing in your Python script: ::

  from pylmgc90 import gmshutils


.. _gmsh_pre:

Pre-processing tools
--------------------

The first possibility is to create a geometry from a :py:class:`pre.mesh` object
with the :py:func:`gmshutils.getMeshAsGModel` function.
Currently only elements of type *S2xxx*, *T3xxx* and *Q4xxx* are read.

Then physical volumes can be added to model with the: :py:func:`gmshutils.addVolumesToGeo`.

Finally the geometry can be meshed and saved with a simple call to: :py:func:`gmshutils.meshAndSave`.


Docstrings
----------

.. toctree::
   :maxdepth: 1

   gmsh_classes

