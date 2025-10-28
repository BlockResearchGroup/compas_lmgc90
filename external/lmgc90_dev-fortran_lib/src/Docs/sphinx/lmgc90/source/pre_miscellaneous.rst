.. py:currentmodule:: pylmgc90.pre


Miscellaneous
=============

Here are gathered some functions which do not really fit in any previous categories, but
remain usefull.

Displaying avatars
------------------

Using :py:func:`visuAvatars` it is possible to see a preview of the
sample stored in an avatars object.



Building an evolution file
--------------------------

In order to apply non trivial loads it mays be necessary to generate
an *evolution* file. It uses :py:func:`writeEvolution` function. 

**Example:** ::

 t0=0.5
 t1 =1.
 f =100.

 def imposedForce(t):
    # 0 until t0
    if t <= t0:
       return 0.
    # linear growing between [t0, t1]
    elif t > t0 and t <= t1:
       return -f*(t-t0)/(t1-t0)
    # constant value afterward
    else:
       return -f

 pre.writeEvolution(f=imposedForce, instants=numpy.linspace(0., 2*t1, 1000) ,path='DATBOX/', name='force.dat')


.. _extrusions:

Extrusion
---------

It is possible to extrude existing 2D rigid avatars in 3D. There are two functions, one to extrude
only one avatar: :py:func:`extrudeRigid` and another one to extrude a whole container of avatars:
:py:func:`extrudeRigids`. The rule of contactors' extrusion is:

* Polygons (POLYG) become polyhedra (POLYR)
* Disks (DISKx) become spheres (SPHER) or cylinders (CYLND), at choice
* Hollow diskx (xKSID) become hollow cylinders (DNLYC)
* JONCx become plans (PLANx)

