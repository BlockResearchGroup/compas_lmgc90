import numpy as np

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
dim = 3

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0

# time evolution parameters
dt = 1.e-6

t_final = 1000*dt
dt_min = dt
dt_max = dt

NR_max_iter = 50
NR_adapt = 9999999
NR_tol = 1.e-2

forced=True

nb_steps=int(t_final/dt)


# theta integrator parameter
theta = 0.5

# deformable  yes=1, no=0
deformable = 1

# interaction parameters
Rloc_tol = 5.e-2

chipy.CSASp_SkipAutoContact()
chipy.CSASp_Trim()

# nlgs parameters
tol = 1e-4
relax = 1.0
norm = 'Quad '
gs_it1 = 50
gs_it2 = 5
solver_type='Stored_Delassus_Loops         '

# write parameter
freq_write   = 1 

# display parameters
freq_display = 1

#
# read and load
#

# Newton loop parameters:
chipy.NewtonRaphson_SetFinalTime(t_final)
chipy.NewtonRaphson_SetMinTimeStep(dt_min)
chipy.NewtonRaphson_SetMaxTimeStep(dt_max)
chipy.NewtonRaphson_SetMaxIter(NR_max_iter)
chipy.NewtonRaphson_SetIncPatience(NR_adapt)


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
chipy.utilities_logMes('READ INI DOF')
chipy.ReadIniDof()
#
if deformable:
  chipy.utilities_logMes('READ INI GPV')
  chipy.ReadIniGPV()
#
chipy.utilities_logMes('READ DRIVEN DOF')
chipy.ReadDrivenDof()
#
chipy.utilities_logMes('LOAD TACTORS')
chipy.LoadTactors()
#
chipy.utilities_logMes('READ INI Vloc Rloc')
chipy.ReadIniVlocRloc()

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

idiv={'PPAS': 0,'CDP' : 1,'PC'  : 2,'PT'  : 3,'WPL0': 4,'ERGF': 5,'DTPP': 6,'DCPP': 7,'DPEQ': 8,'DCOM': 9,\
      'DTRA':10,'RTHC':11,'RTHT':12,'RTHE':13,'TMAX':14}

f=open('gpi.txt','w')
s=open('pstrain-pstress.txt','w')

mats=[]
status=[]
nb=chipy.mecaMAILx_GetNbMecaMAILx()
for i in range(nb):
  mats.append(chipy.mecaMAILx_GetMaterials(i+1))
  status.append(chipy.mecaMAILx_GetDofStatus(i+1))


####
# Dictionnary definition to use when writing display files
# Since there are only 'CLALp' contacts, the dictionnary if very simple
beta_values = { chipy.CSASp_ID : None }

# Getting the index of the contact law 'czm__' defined in the generation part
# The index is shift with '+1' because Python starts indexing from 0 and LMGC90 from 1
czm_law_index = []
for i in range( chipy.tact_behav_GetNbTactBehav() ):
  law_type, law_name, law_params = chipy.tact_behav_GetTactBehav(i+1)
  #print(law_type,law_name,law_params,i+1)
  if law_type == 'EXPO_CZM':
    czm_law_index.append(i+1)

# Get the index of internal parameters corresponding to 'beta' field
# First get all the internal parameters name in a single string
beta_index = {}
TPS_index={}
for idl in czm_law_index:
  #print(idl)
  internal_names = chipy.tact_behav_GetInternalComment(idl)
  # Replace the one long string by a list of strings
  internal_names = internal_names.split()
  # Find the index in list matching with 'beta' and shifting index for LMGC90
  beta_index[idl] = internal_names.index('beta')
  TPS_index[idl] = internal_names.index('TPSini')  

#
# simulation part ...
#

# ... calls a simulation time loop
# since constant compute elementary mass matrices once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

chipy.utilities_logMes('COMPUTE BULK')
chipy.ComputeBulk()

# since constant ; compute iteration matrix once
chipy.utilities_logMes('ASSEMB KT')
chipy.mecaMAILx_AssembKT()

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

    nstep=chipy.TimeEvolution_GetStep()

    if  nstep % freq_display == 0:
      
      DPEQ=[]
      DCOM=[]
      DTRA=[]
      for im in range(1,chipy.mecaMAILx_GetNbMecaMAILx()+1):
        IV=chipy.mecaMAILx_GetInternalVariables(im)
        DPEQ.append(IV[:,idiv['DPEQ']])
        DCOM.append(IV[:,idiv['DCOM']])
        DTRA.append(IV[:,idiv['DTRA']])        

      # on travaille dans verlet
      if chipy.inter_handler_3D_getNb( chipy.CSASp_ID ) > 0:
    
        internals = chipy.inter_handler_3D_getAllInternal( chipy.CSASp_ID )
        TactLawNbs= chipy.inter_handler_3D_getAllTactLawNb(chipy.CSASp_ID )
        beta_values[chipy.CSASp_ID] = np.zeros(chipy.inter_handler_3D_getNb( chipy.CSASp_ID))
      
        # let's put non zero values
        ofile = open('./OUTBOX/beta'+str(nstep)+'.txt','w')
        for i in range( chipy.inter_handler_3D_getNb( chipy.CSASp_ID ) ):
          ilaw = int(TactLawNbs[i])
          if ilaw in czm_law_index:
            beta_values[chipy.CSASp_ID][i] = internals[i,beta_index[ilaw]]
            ofile.write('%12.5e  %12.5e\n' % ((beta_values[chipy.CSASp_ID][i]) , TactLawNbs[i]))
          else:
            beta_values[chipy.CSASp_ID] = None
            
        chipy.WriteDisplayFiles(freq=1,\
                                beta=('inter', beta_values),\
                                DrvDof=('mecafe','node',status),\
                                Materials=('mecafe','element',mats),\
                                DPEQ=('mecafe','node',DPEQ),\
                                DCOM=('mecafe','node',DCOM),\
                                DTRA=('mecafe','node',DTRA))
        
      else:
        chipy.WriteDisplayFiles(freq=1,ref_radius=ref_radius,\
                                DrvDof=('mecafe','node',status),\
                                Materials=('mecafe','element',mats),\
                                DPEQ=('mecafe','node',DPEQ),\
                                DCOM=('mecafe','node',DCOM),\
                                DTRA=('mecafe','node',DTRA))
    
    chipy.WritePostproFiles()


    # first body,element,gp
    # gpi=chipy.mecaMAILx_GetGpInternals(3,1,1)
    # xx=np.append([chipy.TimeEvolution_GetTime()],gpi,0)
    # np.savetxt(f,xx[np.newaxis])

    # strain=chipy.mecaMAILx_GetGpPrincipalField(3,1,1,1)
    # # for i in range(3):
    # #   print(i)
    # #   print(strain[i])
    # #   print(strain[3+3*i:3+3*(i+1)])
    # yy=np.append([chipy.TimeEvolution_GetTime()],strain[0:3],0)
    
    # stress=chipy.mecaMAILx_GetGpPrincipalField(3,1,1,2)
    # # for i in range(3):
    # #   print(i)
    # #   print(stress[i])
    # #   print(stress[3+3*i:3+3*(i+1)])
    # yy=np.append(yy,stress[0:3],0)
    # np.savetxt(s,yy[np.newaxis])
    
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
s.close()

# this is the end
chipy.Finalize()


