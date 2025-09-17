.. py:currentmodule:: pylmgc90.chipy.macro

		      
Displaying and postprocessing results of LMGC90
===============================================

Displaying
----------

LMGC90 offers *functions* to generates **vtk** files. Such files can be displayed by paraview or visit. 
Check the jupyter notebook tutorial on basic visualization in the Tutorials to have a short overview
on how to use paraview with these data.

Generating **vtk** files is driven by some functions of the
**pylmgc90.chipy** module ::

  # open some vtk files (pvd) and initialize some internal variables
  chipy.OpenDisplayFiles()

  # Time loop
  for k in range(nb_steps):
  
    # ...

    # writes the vtk files
    chipy.WriteDisplayFiles()

  # close files
  chipy.CloseDisplayFiles()

See :py:func:`OpenDisplayFiles` for function optional arguments.
In case of restart in an existing folder the value of
argument *restart* will give ne number of the first file to write.  

See :py:func:`WriteDisplayFiles` for function optional arguments.

Files are written in **DISPLAY** folder.  You will obtain sequence of vtmb
(multi-block) files and pvd (xml) files which relates files to physical time.

In the **lmgc90_xx.vtmb** files created (where **xx** is a file number)
The following blocks are created :

- **mecafe** : *MAILx* with *MECAx* model, the list of the nodal fields is:

  * `Visible`: is body visible (integer)
  * `Disp`   : displacement (double vector)
  * `Reac`   : contact reaction (double vector)
  * `Velocy` : velocity (double vector)
  * `Ids`    : body number (integer) (Fortran numbering)
  * `Node Id`: node number (integer) (Fortran numbering)
  * `E`      : strain (double tensor)
  * `Evol`   : volumic strain (double)
  * `S`      : stress (double tensor)
  * `Svm`    : von Mises stress (double)
  * `Fdyn`   : inertial force (double vector)
  * `Fext`   : external force (double vector)
  * `Fint`   : internal force (double vector)
  * `Res`    : residual (double vector)

- **therfe** : *MAILx* with *THERx* model, the list of nodal fields is:

  * `Ids`        : body number (integer) (Fortran numbering)
  * `Node Id`    : node number (integer) (Fortran numbering)
  * `Temperature`: temperature (double)
  * `Grad`       : gradient of temperature (double vector)
  * `Flux`       : flux (double)

- **porofe** : contains values attached to *MAILx* with *POROx* model

  * `Visible`      : is body visible (integer)
  * `Disp`         : displacement (double vector)
  * `Velocity`     : velocity (double vector)
  * `Pressure`     : pressure (double)
  * `Ids`          : body number (integer) (Fortran numbering)
  * `Node Id`      : node number (integer) (Fortran numbering)
  * `Almani Strain`: strain (double tensor)
  * `Cauchy Stress`: stress (double tensor)
  * `Grad P`       : gradient of pressure (double vector)
  * `Darcy Flux`   : fluid flux (double vector)
  * `Fext`         : external force (double vector)
  * `Fint`         : internal force (double vector)
  * `Jacobien`     : (integer)

- **rigids** : *RBDY2* or *RBDY3* models, the list of nodal fields is:

  * `Visible` : is body visible (integer)
  * `Disp`    : displacement (double vector)
  * `Rot Z`   : angular rotation Z (double) (2D only)
  * `alpha`   : first axis of inertia frame (double vector) (3D only)
  * `beta`    : second axis of inertia frame (double vector) (3D only)
  * `gamma`   : third axis of inertia frame (double vector) (3D only)
  * `Velocy`  : velocity (double vector)
  * `Spin Z`  : angular velocity around Z(double) (2D only)
  * `Spin`    : angular velocity (double vector) (3D only)
  * `Reac`    : contact reaction (double vector)
  * `Torque Z`: contact reaction moment around Z(double) (2D only)
  * `Torque`  : contact reaction moment (double vector) (3D only)
  * `Ids`     : body number (integer) (Fortran numbering)
  * `Material`: material id (integer)
  * `Fext`    : external force (double vector)
  * `Mext Z`  : external torque around Z (double) (2D only)
  * `Mext`    : external torque (double vector) (3D only)

- **tacts**  : contactors , the list of element fields is:

  * `Visible` : is body visible (integer)
  * `Disp`    : displacement (double vector)
  * `Rot Z`   : angular rotation Z (double) (2D only)
  * `Velocy`  : velocity (double vector)
  * `Spin Z`  : angular velocity around Z(double) (2D only)
  * `Spin`    : angular velocity (double vector) (3D only)
  * `Reac`    : contact reaction (double vector)
  * `Torque Z`: contact moment around Z (double) (2D only)
  * `Torque`  : contact moment (double vector) (3D only)
  * `Ids`     : body number (integer) (Fortran numbering)
  * `Material`: material id (integer)
  * `Shape`   : contactor id (integer)

- **ptc** : contacts points, the list of element fields is:

  * `T`     : first tangent vector (double vector)
  * `N`     : normal vector (double vector)
  * `S`     : second tangent vector (double vector) 3D only
  * `R`     : reaction (in global frame) (double vector)
  * `inter` : type id of interaction (integer)
  * `icdan` : id of interaction within `inter` numbering (integer) (Fortran numbering)
  * `cdbdy` : type of candidate body (integer)
  * `icdbdy`: id of candidate body (integer) (Fortran numbering)
  * `anbdy` : type of antagonist body (integer)
  * `ianbdy`: id of antagonist body (integer) (Fortran numbering)
  * `behav` : id of interaction law (integer) (Fortran numbering)
  * `status`: status of the contact (integer)
  * `gap`   : gap of the contact (double)
  * `rlt`   : local reaction, first tangent (double)
  * `rln`   : local reaction, normal (double)
  * `rls`   : local reaction, second tangent (double) 3D only
  * `vlt`   : local velocity, first tangent (double)
  * `vln`   : local velocity, normal (double)
  * `vls`   : local velocity, second tangent (double) 3D only
  * `id`    : id of interaction (integer) (Pyhon numbering)

It is possible to add some fields to the files. For example ::

 # create an array to store a nodal field for each mesh   
 status=[]
 # here the nodal field is generated by a built-in function
 status.append(mecaMAILx_GetDofStatus(1))

 # time loop
 for k in range(nb_steps):
   
   # ...

   # add a nodel field to mecafe files 
   chipy.WriteDisplayFiles(freq=freq_display,DrvDof=('mecafe','node',status) )


Some paraview macros are available in the **src/addons/paraview** directory
allowing to extract block in the pipeline browser and do some usual settings.

Postprocessing
--------------

LMGC90 offers some builtin *functions* to analyze the results.

Generating these files is driven by some functions of the
**pylmgc90.chipy** module ::

  # open some postpro files and initialize some internal variables
  chipy.OpenPostproFiles() 

  # Time loop
  
    # writes the vtk files
    chipy.WritePostproFiles()

  # close files
  chipy.ClosePostproFiles()

The *OpenPostproFiles()* function has an optional argument:
*restart=0*. In case of restart in an existing folder the value of
argument *restart* will give ne number of the first file to write. 

Files are created in **POSTPRO** folder.
The postpro commands are declared during the pre-processing phase see
:ref:`pre_post-label` . 
The content of the files is described in *manuals/LMGC90_Postpro.pdf*. 
