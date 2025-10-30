
.. automodule:: pre_lmgc

.. automodule:: chipy

arche_plein_ceintre_3D
======================

Simulation of an arch made of 3D rigid bricks.
The only load is the gravity

Generation:
-----------

 Use the functions:

 * :py:func:`pre_lmgc.rigidPolyhedron`
 * :py:func:`pre_lmgc.rigidPLAN`

Simulation: 
-----------

Strategy:

 * NSCD Linear
 * NLGS + Exchange Local Global

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
 * :py:func:`chipy.nlgs_3D_DiagonalResolution`

