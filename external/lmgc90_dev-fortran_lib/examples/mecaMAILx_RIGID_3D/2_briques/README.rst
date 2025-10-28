
.. automodule:: pre_lmgc

.. automodule:: chipy

2 Briques
=========

Simulation of 2 3D bricks on a fixed base. 
The base and one brick are POLYR the last brick is meshed.
The only load is the gravity

Generation:
-----------

 Use the functions:

 * :py:func:`pre_lmgc.readMesh`
 * :py:func:`pre_lmgc.mesh.separateMeshes`
 * :py:func:`pre_lmgc.volumicMeshToRigid3D`
 * :py:func:`pre_lmgc.buildMeshAvatar`

 Add contactors of type:

 * CSpx3

 Add postpro commands:

 * NEW MECAx SETS
 * Dep EVOLUTION
 * Fint EVOLUTION
 * BODY TRACKING
 * TORQUE EVOLUTION

Simulation: 
-----------

Strategy:

 * NSCD Linear
 * NLGS + Stored Delassus Loops

Bulks:

 * RBDY3
 * mecaMAILx + MatLib

Contactors:

 * POLYR
 * CSxxx 

Interaction:

 * PRPRx
 * CSPRx

Contact Laws:

 * IQS_CLB_g0
 * GAP_SGR_CLB_g0

Special keywords:

 * :py:func:`chipy.PRPRx_ShrinkPolyrFaces`
 * :py:func:`chipy.PRPRx_UseCpF2fExplicitDetection`
 * :py:func:`chipy.PRPRx_LowSizeArrayPolyr`

