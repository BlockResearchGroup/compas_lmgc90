# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
#chipy.utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim = 2

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters

# small ts ; default tolerance 1e-4 
dt = 1e-5
nb_steps = 800000

# large ts 
#dt = 1e-4
#nb_steps = 80000
#PTPT2_SetTolerance(1e-3)

# theta integrator parameter
theta = 0.5

# deformable  yes=1, no=0
deformable = 0

# interaction parameters
Rloc_tol = 5.e-2

# nlgs parameters
tol = 1e-6
relax = 1.0
norm = 'Maxm '
gs_it1 = 500
gs_it2 = 10
stype='Stored_Delassus_Loops         '

# write parameter
freq_write   = 1000

# display parameters
freq_display = 1000

chipy.PT2Dx_SetDisplayRadius(0.02)

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
chipy.utilities_logMes('READ BEHAVIOURS')
chipy.ReadBehaviours()
if deformable: chipy.ReadModels()
#
chipy.utilities_logMes('READ BODIES')
chipy.ReadBodies()
#
chipy.utilities_logMes('LOAD BEHAVIOURS')
chipy.LoadBehaviours()
if deformable: chipy.LoadModels()
#
#
chipy.utilities_logMes('READ DRIVEN DOF')
chipy.ReadDrivenDof()
#
chipy.utilities_logMes('LOAD TACTORS')
chipy.LoadTactors()
#
chipy.utilities_logMes('READ INI')
chipy.ReadIni()
#
# paranoid writes
#
chipy.utilities_logMes('WRITE BODIES')
chipy.WriteBodies()
chipy.utilities_logMes('WRITE BEHAVIOURS')
chipy.WriteBehaviours()
chipy.utilities_logMes('WRITE DRIVEN DOF')
chipy.WriteDrivenDof()

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.InitHDF5('lmgc90.h5')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

#
# simulation part ...
#

# ... calls a simulation time loop
# since constant compute elementary mass once
chipy.ComputeMass()

#ofile = open('./R.txt','w')

for k in range(nb_steps):
  #
  chipy.IncrementStep()

  chipy.ComputeFext()
  chipy.ComputeBulk()
  chipy.ComputeFreeVelocity()

  chipy.SelectProxTactors()

  chipy.RecupRloc(Rloc_tol)

  chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
  chipy.UpdateTactBehav()

  chipy.StockRloc()

  chipy.ComputeDof()

  chipy.UpdateStep()

  chipy.WriteOut(freq_write)

  chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

  chipy.checkInteractiveCommand()

  #nb_PTPT2 = PTPT2_GetNbPTPT2()
  #for icdan in range(1,nb_PTPT2+1):
  #  print PTPT2_GetAntagonistTactId(icdan),PTPT2_GetCandidatTactId(icdan)
  #sys.exit()
  
  #tps=TimeEvolution_GetTime()
  #all= inter_handler_2D_getAll( PTPT2_ID )
  #icdan=1
  #for coorx,coory,nx,ny,fn,ft,g in all:
  #  icdtac = inter_handler_2D_GetCandidatTactId( PTPT2_ID )  
  #  iantac = inter_handler_2D_GetAntagonistTactId( PTPT2_ID )
  #  if (iantac == 5 and icdtac==6) or (iantac==6 and icdtac==5):           
  #    ofile.write('%12.5e %12.5e %12.5e %12.5e\n' % (tps,math.sqrt(fn**2+ft**2),fn,ft))
  #  icdan+=1

  
#ofile.close()  
#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
