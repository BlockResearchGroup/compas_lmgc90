.. py:currentmodule:: pylmgc90.chipy.lmgc90

Input and Output files in LMGC90
================================

There are four types of files involved with LMGC90 software :

* The input files stored within the *DATBOX* directory,
* The outputs stored either in a single output HDF5 file
  or text files stored in the *OUTBOX* directory (historical files).  
  If the HDF5 library has not been used during compilation the handling of output uses the historical files.
  This :ref:`section <old-file-management>` details the changes to bring to the next explanation.  
* The paraview visualization files stored within the *DISPLAY* directory.
* The post-processing files stored within the *POSTPRO* directory.

Usually these three directories are created next to the command and/or generation
python script(s). But it can be usefull to have these directories somewhere
else in the filetree, thus the *working directory* can be changed in the
command script thanks to the command: :py:func:`overall_SetWorkingDirectory`.

Input files
-----------

The files inside the *DATBOX* directory are usually automatically
generated with LMGC90's preprocessor and contains:

* **\*.DAT** files : which describe the simulation,
* **\*.INI** files : which describe the state at
  the initial time step of the simulation.

They are ASCII files in text format and their names cannot be changed.
All the files within this directory come has a bundle and must not be
changed **by hand** under the risk of breaking the consistency of the
database when reading them again.

All these files are read in one go with ::

  chipy.ReadDatbox(deformable=True)

which in fact read all these **\*.DAT** files using the functions ::

  chipy.ReadBehaviours() # read BULK_BEHAV.DAT and TACT_BEHAV.DAT
  chipy.ReadModels()     # read MODELS.DAT (only if meshed bodies)
  chipy.ReadBodies()     # read BODIES.DAT
  chipy.ReadIniDof()     # read DOF.INI (necessary to avoid errors with polyr.vtu)

  chipy.LoadBehaviours() # load read data into database
  chipy.LoadModels()     # load read data into database

  chipy.ReadDrivenDof()  # read DRV_DOF.DAT

  chipy.LoadTactors()    # load read data into database

then the initial state is read from the **\*.INI** files using::

  chipy.ReadIni()


Output files
------------

The output file is an HDF5 portable binary file, which name is
chosen by the user in the computation script. It stores for
each desired time step the state of the degrees of freedom,
the Gauss point values if there exist meshed bodies in the simulation
and the interactions informations.

To set the file in which to save the output, before the time loop,
there must be a call to the function ::

  chipy.InitHDF5( 'lmgc90.h5' )

This allows to set any file in which to save the database, the name
of the file provided is relative to the working directory (i.e. where
the *DATBOX* directory is).

For the moment this file is not self sufficient since it goes as a pair
with the *DATBOX* directory which has been read beforehand.

Inside the time loop, there must be a call to ::

  chipy.WriteOut( freq )

which will write the state of the database inside it
(*Vloc_Rloc*, *DOF*  and *GPV* in a single group every
`freq` time step). Each time the function
writes inside the file, an internal index is increased and used
to reference the state.

To know what is inside the HDF5 file, the *h5dump* command can be used
to generate an ASCII text file. Otherwise the *h5py* python module can
be used to generate a dictionnary-like object allowing to get the
stored values. For example to get the number of records in the file one can do ::

  import h5py

  hf = h5py.File('lmgc90.h5')
  print( int( hf['Simulation/nb_record'][()] ) )
  ...
  hf.close()


To get some information on what is available inside the file,
explore the content of the **Help** group.


Display files
-------------

Display files management has already been described :doc:`here </vizpost_index>`.


Restart 
-------

In some cases, it is wanted to put the database state at a 
particular time step; for example when wanting to restart a computation
which as been interrupted or using the final state of a previous simulation
which has prepared your sample (a packing for example) before changing
the boundary conditions for example.

Once the  **DATBOX** directory has been read, an initial state can be read
from the **lmgc90.h5** to overwrite the one read from the **\*.INI** files.
This is done simply by providing the output index to use and the name of the
file in which to read the new state.

Thus if you want to read the i-th record of your **lmgc90.h5** HDF5 file you must
use ::

  chipy.ReadIni( i, 'lmgc90.h5' )

It must be pointed out here that contrary to the call to **chipy.InitHDF5** which define the filename relatively to the working directory, when reading you must provide the path relatively to the python script.

Then, when wanting to write the output, you can either provide a new file name
to create a second one, or append into an existing file by providing the name
of an existing one.

When appending to a file, all records after the current time step will be deleted.
If no initial state is read, everything is deleted.

It must be emphasized that when reading from the **DATBOX** directory,
everything must be read from it especially the **\*.INI** files. And only
once everything is read (after the **LoadTactor** command is run - see below), can the new state be overwritten
by reading a new state.

A posteriori visualization
--------------------------

Since any time step saved can be read again, it is very simple to
generate the visualisation files a posteriori.

One needs to read the DATBOX, initialize the writing of the display
files, then looping on the number of records read the HDF5 file and
write the display files ::

  import h5py

  h5_file = 'lmgc90.h5'
  hf = h5py.File(h5_file)
  nb_record = int( hf['Simulation/nb_record'][()] )
  dim = int( hf['Simulation/dimension'][()] )
  hf.close()

  from pylmgc90 import chipy

  chipy.Initialize()
  chipy.checkDirectories()
  
  chipy.Initialize()
  
  chipy.SetDimension(dim)

  chipy.ReadDatbox()
  
  chipy.OpenDisplayFiles()
  
  chipy.ComputeMass()

  for k in range(1,nb_record+1,1):

    chipy.ReadIni(k,h5_file)
    chipy.ComputeFext()
    chipy.ComputeRnod()
    chipy.WriteDisplayFiles(freq_display)

  chipy.CloseDisplayFiles()
  chipy.Finalize()


From input file to pre
----------------------

It is sometimes desired to read the **DATBOX** directory
to obtain the *avatar* objects and to load a particular
state from the output files. It is possible to do so
with the `pre.readDatbox` and `pre.readState` functions::

  from pylmgc90 import pre

  dim = 2
  nstep = 4
  mats, mods, bodies, tacts, sees, inters = pre.readDatbox(dim, 'OUTBOX')
  pre.readState(bodies, 'OUTBOX', nstep)

For technical reasons, when reading output state from an HDF5 file, not only the *avatar*
container must be provided in input, but also the *tacts*
container if you want to recover the interactions in a numpy array (*inters*) ::

  inters = pre.readState(bodies, 'OUTBOX', nstep, 'lmgc90.h5', tacts)



Smart restart
-------------

*Notice: the following instructions were written before
the `pre.readDatbox` and `pre.readState` functions were
available. Thus the example files were modified to use
these functions instead, but the following instructions
were kept as is because they show an interesting way
to use python and pickle module.*

By combining some python modules allowing to save very
efficiently the database from the pre-processing and
by using the :py:func:`overall_SetWorkingDirectory` and
the :py:func:`chipy.ReadIni`, it is possible to do a
first computation, then change the boundary condition
and run a second computation using the final state of
the previous computation as the initial state of the
new one.

In the directory **examples/Tutorial/advanced/smart_restart**, you
will find all the scripts used to run the example described
in this section. The **2Ddeposit.py** script is an
example of pre-processing doing a periodic deposit of a
2D sample of disk in a channel. Since the computation must
manage the periodicity, the walls are clusters of disks.

The main changes compared to a standard generation script
is that the **DATBOX** directory is writen within a **Press**
directory. The second one is that since there will be a
second computation but with some slight modifications to
the **DATBOX** directory the `pickle` Python module is used
to save some data from the pre-processing script in a file::

  import pickle
  
  with open('pre_data.pickle','wb') as f:
    pickle.dump( (dim, bodies, [down,up], mats, tacts, svs, post), f, pickle.HIGHEST_PROTOCOL)


Then you can run the **comp_press.py** file which
will run the compression of the sample in the **Press**
directory thanks to the command::

  chipy.overall_SetWorkingDirectory('Press')

The rest is a classic command script.

Once this first computation is done the **Press2Shear.py**
script will read the **pre_data.pickle** file generated during
the pre-processing step, change the boundary condition from
a vertical force to a horizontal velocity on the upper wall,
change the friction coefficient value and then add
a thermo-rigid model to the grains. Finally the corresponding
**DATBOX** directory is written in the **Shear** directory.
Since the pre-processing of the thermo-rigid model is currently
not very well supported, you need to have the scripts:
 * **MP_mat.py**
 * **MP_mod.py**
Before being able to use the `Press2Shear.py` script.

The last step is to run this shear computation. To this
end use the **comp_shear.py** script which will
use **Shear** as the working directory thanks to the
command::

 chipy.overall_SetWorkingDirectory('Shear')

Then the last step of the **Press** computation
is used as an initial step of the current computation.
First, one has to look for the last written record, a generic
way to do this independtly of using HDF5 or not is::

  if os.path.isfile( os.path.join('Press',h5_file) ):
    import h5py
    hf = h5py.File('Press/lmgc90.h5', 'r')
    reading_step= int( hf['Simulation/nb_record'][()] )
    hf.close()
  else:
    reading_step      = len(fnmatch.filter(os.listdir('../Press/OUTBOX/'), 'DOF.OUT.*'))
    
Then to read the following record thanks to the following block::

  if os.path.isfile( os.path.join('Press',h5_file) ):
    chipy.ReadIni(reading_step,os.path.join('../Press',h5_file))
  else:
    chipy.ReadIni(reading_step)


.. _old-file-management:

Old file management
-------------------

In fact to use old **\*.OUT** files of *OUTBOX* directory,
just never give a filename in the input of the `chipy.ReadIni`
and `chipy.WriteOut` functions, and they will automatically read
from, or write to, the correct ASCII files.

