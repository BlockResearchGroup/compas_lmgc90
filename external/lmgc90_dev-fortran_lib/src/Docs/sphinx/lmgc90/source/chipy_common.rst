
Common part of a script
=======================

Here is a typical script using *chipy* functions.

Almost every things may be driven by initializing parameters of this
script. 

Specific tunings can be performed setting some additional options.

::

  # importing chipy module
  from pylmgc90 import chipy

  # Initializing 
  chipy.Initialize()  

  # checking/creating mandatory subfolders
  chipy.checkDirectories()
  
  # logMes  
  # chipy.utilities_DisableLogMes()

  #
  # defining some variables
  #   
  
  # space dimension
  dim = 2 

  # modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
  mhyp = 1

  # time evolution parameters
  dt = 1e-3
  nb_steps = 100

  # theta integrator parameter
  theta = 0.5

  # deformable True or False
  deformable = False

  # interaction parameters
  Rloc_tol = 5.e-2

  # nlgs parameters
  tol = 1e-4
  relax = 1.0  
  norm = 'Quad '
  gs_it1 = 50 
  gs_it2 = 10
  solver_type='Stored_Delassus_Loops         '

  # write parameter
  freq_write   = 10

  # display parameters
  freq_display = 10

  # 
  # read and load
  #

  # Set space dimension
  chipy.SetDimension(dim,mhyp)
  # 
  chipy.utilities_logMes('INIT TIME STEPPING')
  chipy.TimeEvolution_SetTimeStep(dt)
  chipy.Integrator_InitTheta(theta)
  #
  chipy.utilities_logMes('READ DATBOX')
  chipy.ReadDatbox(deformable)

  #
  # open display & postpro
  #

  chipy.utilities_logMes('DISPLAY & WRITE') 
  chipy.OpenDisplayFiles()
  chipy.OpenPostproFiles()

  # if HDF5 is available
  chipy.InitHDF5('lmgc90.h5')

  #
  # simulation part ...
  #

  # ... calls a simulation time loop

  #
  # close display & postpro
  #
  chipy.CloseDisplayFiles()
  chipy.ClosePostproFiles()

  # this is the end
  chipy.Finalize()

