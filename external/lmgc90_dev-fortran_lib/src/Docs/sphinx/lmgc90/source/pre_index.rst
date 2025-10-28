
LMGC90 pre-processor documentation
==================================

*pylmgc90.pre* is a **python** module dedicated to the generation of LMGC90's input files. This module provides
basic functionnalities allowing to define body, material, model, visibility table and contact laws
in a self-content way. Furthermore it provides a framework to design ones own pre-processing functions
according to ones needs if they are not provided yet.

The high level pre-processing functions are categorized in three domains: granular, masonry and mesh manipulation.
Of course any functionnality filed under any category can be used or combined in any other context.

First section is a simple example of use with some explanation on how
the preprocessor has been build.
Granular section is dedicated on how to generate collection of
particles with a given granulometry and deposit container. The second
part is dedicated to 2D/3D masonry. The third part details how to read
meshes from file and manipulate them. Miscellaneous part gathers
information on utilities available in so many different cases that
they do not belong to any category.


Fundamentals:
-------------

.. toctree::
   :maxdepth: 1

   pre_philosophie
   pre_material
   pre_model
   pre_interaction
   pre_visibility
   pre_post

Dedicated tools:
----------------

.. toctree::
   :maxdepth: 1

   pre_granular
   pre_masonry
   pre_mesh

Miscellaneous:
--------------

.. toctree::
   :maxdepth: 1

   pre_miscellaneous
   pre_classes

