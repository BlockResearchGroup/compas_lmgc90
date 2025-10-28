
.. automodule:: pylmgc90.pre
.. py:currentmodule:: pylmgc90.pre

pre module's docstrings
=======================

Basic functions
---------------

Model
"""""

.. autoclass:: model
  :members:
     
.. autoclass:: models
  :members:

Material
""""""""

.. autoclass:: material
  :members:

.. autoclass:: materials
  :members:


Avatar
""""""

.. autoclass:: avatar
  :members:

.. autoclass:: avatars
  :members:


Node
""""

.. autoclass:: node
  :members:

.. autoclass:: nodes
  :members:

Bulk
""""

.. autoclass:: bulk 
  :members:

.. autoclass:: bulks
  :members:

Contactor
"""""""""

.. autoclass:: contactor
  :members:

.. autoclass:: contactors
  :members:

Interaction
"""""""""""

.. autoclass:: tact_behav
  :members:

.. autoclass:: tact_behavs
  :members:

Visibility table
""""""""""""""""
.. autoclass:: see_table
  :members:

.. autoclass:: see_tables
  :members:


Granular functions
------------------

Granulometry generation
"""""""""""""""""""""""

.. autofunction:: pylmgc90.pre.granulo_Random

.. autofunction:: pylmgc90.pre.granulo_Uniform

.. autofunction:: pylmgc90.pre.granulo_TwoSizesNumber

.. autofunction:: pylmgc90.pre.granulo_TwoSizesVolume

.. autofunction:: pylmgc90.pre.granulo_ReadFromFile

Deposit 
"""""""

.. autofunction:: pylmgc90.pre.depositInBox2D

.. autofunction:: pylmgc90.pre.depositInDisk2D

.. autofunction:: pylmgc90.pre.depositInCouette2D

.. autofunction:: pylmgc90.pre.depositInDrum2D

.. autofunction:: pylmgc90.pre.depositInBox3D

.. autofunction:: pylmgc90.pre.depositInCylinder3D

.. autofunction:: pylmgc90.pre.depositInSphere3D

.. autofunction:: pylmgc90.pre.squareLattice2D

.. autofunction:: pylmgc90.pre.triangularLattice2D

.. autofunction:: pylmgc90.pre.cubicLattice3D

Particle generation
"""""""""""""""""""

.. autofunction:: pylmgc90.pre.rigidDisk

.. autofunction:: pylmgc90.pre.rigidCluster

.. autofunction:: pylmgc90.pre.rigidDiscreteDisk

.. autofunction:: pylmgc90.pre.rigidJonc

.. autofunction:: pylmgc90.pre.rigidPolygon

.. autofunction:: rigidOvoidPolygon

.. autofunction:: pylmgc90.pre.deformableParticle2D

.. autofunction:: pylmgc90.pre.rigidSphere

.. autofunction:: pylmgc90.pre.rigidPlan

.. autofunction:: pylmgc90.pre.rigidCylinder

.. autofunction:: pylmgc90.pre.rigidPolyhedron

Wall generation
"""""""""""""""

.. autofunction:: pylmgc90.pre.roughWall

.. autofunction:: pylmgc90.pre.fineWall

.. autofunction:: pylmgc90.pre.smoothWall

.. autofunction:: pylmgc90.pre.granuloRoughWall

.. autofunction:: pylmgc90.pre.roughWall3D

.. autofunction:: pylmgc90.pre.granuloRoughWall3D

Masonry
-------

Bricks
""""""

.. autoclass:: pylmgc90.pre.brick2D
  :members:

.. autoclass:: pylmgc90.pre.brick3D
  :members:

Walls
"""""

.. autoclass:: pylmgc90.pre.paneresse_simple
  :members:

.. autoclass:: pylmgc90.pre.paneresse_double
  :members:

Mesh manipulation
-----------------

mesh creation
"""""""""""""

.. autofunction:: pylmgc90.pre.readMesh

.. autofunction:: pylmgc90.pre.buildMesh2D

.. autofunction:: pylmgc90.pre.buildMeshH8

.. autofunction:: pylmgc90.pre.buildMeshH20

mesh class
""""""""""

.. autoclass:: pylmgc90.pre.mesh
  :members:

mesh manipulation
"""""""""""""""""

.. autofunction:: pylmgc90.pre.extractFreeSurface

.. autofunction:: pylmgc90.pre.reorientSurfacicElements

mesh to avatar(s)
"""""""""""""""""

.. autofunction:: pylmgc90.pre.buildMeshedAvatar

.. autofunction:: pylmgc90.pre.rigidFromMesh2D

.. autofunction:: pylmgc90.pre.rigidsFromMesh2D

.. autofunction:: pylmgc90.pre.rigidsFromMesh3D

.. autofunction:: pylmgc90.pre.explodeMeshedAvatar2D

.. autofunction:: pylmgc90.pre.crackMeshedAvatar2D

.. autofunction:: pylmgc90.pre.explodeMeshedAvatar3D

.. autofunction:: pylmgc90.pre.volumicMeshToRigid3D

.. autofunction:: pylmgc90.pre.surfacicMeshToRigid3D

.. autofunction:: pylmgc90.pre.surfacicMeshesToRigid3D


Miscellaneous
-------------

Display
"""""""
.. autofunction:: pylmgc90.pre.visuAvatars

Evolution
"""""""""
.. autofunction:: pylmgc90.pre.writeEvolution 


Extrusion
"""""""""

.. autofunction:: pylmgc90.pre.extrudeRigid

.. autofunction:: pylmgc90.pre.extrudeRigids

Inputs/Outputs
"""""""""""""""

.. autofunction:: pylmgc90.pre.writeDatbox
.. autofunction:: pylmgc90.pre.readDatbox
.. autofunction:: pylmgc90.pre.readState

