
.. automodule:: pre_lmgc

.. automodule:: chipy

arche_plein_ceintre_2D
======================

Simulation of an arch made of rigid bricks in 2D
The only load is the gravity

Generation:
-----------

 Use the functions:

 * :py:func:`pre_lmgc.rigidPolygon`
 * :py:func:`pre_lmgc.rigidJONC`

Simulation: 
-----------

Strategy:

 * NSCD Linear
 * NLGS + Stored Delassus Loops

Bulks:

 * RBDY2

Contactors:

 * POLYG
 * JONCx 

Interaction:

 * PLPLx
 * PLJCx

Contact Laws:

 * IQS_CLB
 * IQS_CLB_g0

Special keywords:

