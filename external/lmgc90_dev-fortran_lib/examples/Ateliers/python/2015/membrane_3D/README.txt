Aim :

* Use python interface to get access to data within python
  and use them to drive the computation.
* Add an external field to the paraview visualization.

The example used in this training session will be
the confinement of a 3D heap of spheres in a cylinder
by a constant external pressure.

But in LMGC90, only impulses can be added to a body.
Thus the principle will be to split the cylinder along
its lengths in several layers.

The particles on the external crown of each layer
will be looked for and applied a force such that
the sum of each force (in norm) will equals to
the total force of the pressure on the surface of
the layer cylinder.

To check what has been done some visualization fields
will be added:

* on each contactor the layer it belongs to
* on each rigid, if a pressure has been added

A first computation will be made to pack the
sample using the confinment pressure.

A second simulation, starting from the previous
one, will be made but with the upper and lower
walls moving in order to compress the sample.


This training session is organized as follows :

 - First : generate the sample
   $> python TP_sample.py

 - Second : modify the TP1.py script
   following the instructions at TODO comments.
   Test the script once finished.

 - Third : modfiy the TP2.py and then TP3.py
   scripts. The equilibrium state of the sample
   should be available.

 - Fourth : modify TP_sample.py and TP4.py to
   modify the sample, but not the equilibrium
   computation.

 - Fifth : modify TP_compression.py to use
   the previous results in the compression
   computation.


