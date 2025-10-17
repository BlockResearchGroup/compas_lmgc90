
.. automodule:: pre_lmgc

.. automodule:: chipy

2 Briques
=========

Simulation of 3 3D rigid bricks generated from a mesh on a fixed base. 
The only load is the gravity

Generation:
-----------

 Use the functions:

 * :py:func:`pre_lmgc.readMesh`
 * :py:func:`pre_lmgc.mesh.separateMeshes`
 * :py:func:`pre_lmgc.volumicMeshToRigid3D`

Simulation: 
-----------

Strategy:

 * NSCD Linear
 * NLGS + Exchange Local Global + Diagonal Resolution

Bulks:

 * RBDY3

Contactors:

 * POLYR
 * PLANx

Interaction:

 * PRPRx
 * PRPLx

Contact Laws:

 * IQS_CLB
 * IQS_CLB_g0

Special keywords:

 * :py:func:`chipy.PRPRx_ShrinkPolyrFaces`
 * :py:func:`chipy.PRPRx_UseCpF2fExplicitDetection`
 * :py:func:`chipy.PRPRx_LowSizeArrayPolyr`

