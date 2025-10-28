
.. automodule:: pre_lmgc

.. automodule:: chipy

cubes
=====

Simulation of 2 deformable cubes on a rigid foundation.
The base is a polyhedron. Precondensation is used.
The only load is the gravity.

Generation:
-----------

 Use the functions:

 * :py:func:`pre_lmgc.readMesh`
 * :py:func:`pre_lmgc.buildMeshAvatar`
 * :py:func:`pre_lmgc.rigidPolyhedron`

 Add contactors of type:

 * ASpx3
 * CSpx3

Simulation: 
-----------

Strategy:

 * NSCD Linear
 * NLGS + Exchange_Local_Global

Bulks:

 * mecaMAILx (TE4xx) + MatLib

Contactors:

 * POLYR
 * CSxxx 
 * ASpxx 

Interaction:

 * CSAsp
 * CSPRx

Contact Laws:

 * GAP_SGR_CLB

Special keywords:

 * :py:func:`chipy.mecaMAILx_WithRenumbering`
 * :py:func:`chipy.CSxxx_PushPreconNodes`
 * :py:func:`chipy.ASpxx_PushPreconNodes`
 * :py:func:`chipy.mecaMAILx_ComputePreconW`

