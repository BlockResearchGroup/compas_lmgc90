import numpy as np
import sys

# sys.path.insert(0,'/home/ferrante/WORK/CODES/LMGC90/lmgc90_dev/build_dem_v5')
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
dim = 3

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt = 1.e0 
nb_steps = 202

t_final = nb_steps*dt
dt_min = dt
dt_max = dt

NR_max_iter = 300
NR_adapt = 9999999
NR_tol = 1.e-6

forced=True

# theta integrator parameter
theta = 1.

# deformable  yes=1, no=0
deformable = 1

# interaction parameters
Rloc_tol = 8.e-2

# nlgs parameters
tol = 1e-4
relax = 1.0
norm = 'Quad '
gs_it1 = 50
gs_it2 = 10
solver_type='Stored_Delassus_Loops         '

# write parameter
freq_write   = 1

# display parameters
freq_display = 1
ref_radius = 2.5e-2

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
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

# if HDF5 is available
chipy.InitHDF5('lmgc90.h5')

#
# simulation part ...
#

# ... calls a simulation time loop
# Newton loop parameters:
chipy.NewtonRaphson_SetFinalTime(t_final)
chipy.NewtonRaphson_SetMinTimeStep(dt_min)
chipy.NewtonRaphson_SetMaxTimeStep(dt_max)
chipy.NewtonRaphson_SetMaxIter(NR_max_iter)
chipy.NewtonRaphson_SetIncPatience(NR_adapt)

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

chipy.utilities_logMes('COMPUTE BULK')
chipy.ComputeBulk()

# since constant ; compute iteration matrix once
chipy.utilities_logMes('ASSEMB KT')
chipy.mecaMAILx_AssembKT()


idiv={'PPAS': 0,'CDP' : 1,'PC'  : 2,'PT'  : 3,'WPL0': 4,'ERGF': 5,'DTPP': 6,'DCPP': 7,'DPEQ': 8,'DCOM': 9,\
      'DTRA':10,'RTHC':11,'RTHT':12,'RTHE':13,'TMAX':14}

f=open('gpi.txt','w')
g=open('stress.txt','w')
h=open('strain.txt','w')

mats=[]
mats.append(chipy.mecaMAILx_GetMaterials(1))


while chipy.TimeEvolution_GetTime() < t_final :
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()

  # Newton loop
  chipy.NewtonRaphson_Initialize(NR_tol)
  is_converged = 1
  k=0
  #looping until something changes in CheckConvergence
  while is_converged == 1 :
    k+=1
    chipy.utilities_logMes('COMPUTE BULK')
    chipy.ComputeBulk()

    chipy.utilities_logMes('ASSEMB RHS/KT')
    chipy.AssembleMechanicalRHS()
    # chipy.AssembleMechanicalLHS()

    chipy.utilities_logMes('COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()
    #
    chipy.utilities_logMes('SELECT PROX TACTORS')
    chipy.SelectProxTactors()
    #
    ### Signorini Coulomb
    chipy.RecupRloc()
    chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
    chipy.StockRloc()
    ###
    chipy.utilities_logMes('COMPUTE DOF')
    chipy.ComputeDof()
    #
    if k > 1:
      NR_norm = chipy.mecaMAILx_ComputeResidueNorm()
      is_converged = chipy.NewtonRaphson_CheckConvergence(NR_norm)

      if forced and k == NR_max_iter:
        chipy.utilities_logMes('Warning: forced convergence')        
        is_converged=0

  ### end while NR

  chipy.utilities_logMes('COMPUTE TIME STEP')
  #istate = 1 => redo step
  #istate = 2 => stop

  istate = chipy.NewtonRaphson_ComputeTimeStep()

  if not istate == 1 :

    chipy.utilities_logMes('UPDATE TACT BEHAV')
    chipy.UpdateTactBehav()
    chipy.StockRloc()

    chipy.utilities_logMes('UPDATE DOF')
    chipy.UpdateStep()
    #
    ### write results ###
    #
    chipy.WriteOut(freq_write)

    if chipy.TimeEvolution_GetStep() % freq_display == 0:
      IV=chipy.mecaMAILx_GetInternalVariables(1)
      chipy.WriteDisplayFiles(freq=1,\
                              DTRA=('mecafe','node',[IV[:,idiv['DTRA']]]),\
                              DCOM=('mecafe','node',[IV[:,idiv['DCOM']]]))
    chipy.WritePostproFiles()


    # first body,element,gp
    gpi=chipy.mecaMAILx_GetGpInternals(1,2,1)
    xx=np.append([chipy.TimeEvolution_GetTime()],gpi,0)
    np.savetxt(f,xx[np.newaxis])
    
    stress=chipy.mecaMAILx_GetGpStress(1,2,1)
    stress=np.append([chipy.TimeEvolution_GetTime()],stress,0)
    np.savetxt(g,stress[np.newaxis])
    
    strain=chipy.mecaMAILx_GetGpStrain(1,2,1)
    strain=np.append([chipy.TimeEvolution_GetTime()],strain,0)
    np.savetxt(h,strain[np.newaxis])
    
    chipy.checkInteractiveCommand()

    if istate == 2 :
      # istate => Stop
      break

### end while time loop ###

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

f.close()
g.close()
h.close()

# this is the end
chipy.Finalize()
