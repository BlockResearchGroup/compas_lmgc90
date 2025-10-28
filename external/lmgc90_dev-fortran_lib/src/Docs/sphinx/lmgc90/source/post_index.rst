
.. py:currentmodule:: pylmgc90

LMGC90 post-processor documentation
===================================

As of late, most information provided
by the historic postprocessing routines
available with :py:func:`chipy.WritePostproFiles`
(and associated functions) are also
available with the python accessors
(or with the HDF5 file).

In this way there are some dedicated postprocessing
function written purely in Python and provided
in the `post` submodule.

Currently there is only one functionality
which allow to generate some paraview files
for the **central kernel** of masonry
structures [ref necessary].


Data generation
---------------

To use this the following excerpt is to be included
in a relevant command script ::

  from pylmgc90 import post

  post.OpenCentralKernelFiles()

  #
  # simulation part ...
  #
  
  # ... calls a simulation time loop

    chipy.WriteDisplayFiles(freq_display)
    time   = chipy.TimeEvolution_GetTime()
    f2f    = chipy.PRPRx_GetF2f2Inters()
    inters = chipy.getInteractions()
    post.WriteCentralKernelFiles(time, f2f, inters)

Visualization
-------------

This will generate some **ck_*.vtp** files in the
**DISPLAY** directory. The content of these files
must be sorted (i.e. threshold in paraview) on
their type to extract relevant data:
 * type = 0 : the pressure center point with the reaction
 * type = 1 : the face to face structured defined as a polygon, each vertex being an point of application of a force.
   The equivalent normal stress can be displayed in it.
 * type = 2 : the central kernel polygon, a status can be display in it (1 if the pressure point is outside it, 0 if inside).


Accessing data
--------------

It is possible to access the raw data of
the central kernel structure allowing to
write those display files  with the function
:py:func:`post.central_kernel.get` .


