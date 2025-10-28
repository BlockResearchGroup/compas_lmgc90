
import numpy as np

from pylmgc90 import chipy
####
# info gestion du temps
dt = 1.e-3
theta = 0.50
nstep = 2000

max_it_fp=1

# bavardage de certaines fonctions
echo = 0
#chipy.utilities_DisableLogMes()

# info generation fichier visu
freq_display = 250
freq_write = 1000

# info contact

#       123456789012345678901234567890
stype = 'Stored_Delassus_Loops         '
#stype = 'Exchange_Local_Global         '
quad = 'QM/16'
tol = 0.1666e-3
relax = 1.
gs_it1 = 500
gs_it2 = 10

chipy.Initialize()

chipy.nlgs_3D_SetWithQuickScramble()
chipy.nlgs_3D_DiagonalResolution()

###
chipy.checkDirectories()
###
chipy.SetDimension(3,0)

chipy.utilities_logMes( 'INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### lecture du modele ###
chipy.utilities_logMes( 'READ BODIES')
chipy.ReadDatbox(deformable=True)

chipy.mecaMAILx_SetCoroAllBodies()

### definition des parametres du calcul ### 

### post3D ##
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

### parameters setting ###

chipy.utilities_logMes( 'COMPUTE MASS')
chipy.ComputeMass()
chipy.mecaMAILx_BuildRigidBodies()

chipy.utilities_logMes( 'COMPUTE STIFFNESS')

chipy.mecaMAILx_ComputeBulk()
chipy.mecaMAILx_ComputeRayleighDamping(0.,0.0001)

####
#chipy.mecaMAILx_SetPreconAllBodies()
#chipy.CSxxx_PushPreconNodes()
#chipy.mecaMAILx_ComputePreconW()

chipy.mecaMAILx_AssembKT()

for k in range(1,nstep+1,1):

   chipy.utilities_logMes( 'INCREMENT STEP')
   chipy.IncrementStep()

   chipy.utilities_logMes( 'COMPUTE Fext')
   chipy.ComputeFext()

   chipy.ComputeBulk()

   chipy.ComputeFreeVelocity()

   for ifp in range(1,max_it_fp+1):

     chipy.utilities_logMes('----')
     chipy.utilities_logMes('ite fp: '+str(ifp))
    
     chipy.utilities_logMes( 'COMPUTE Fint')
     chipy.mecaMAILx_ComputeBulk()

     chipy.utilities_logMes( 'ASSEMBLAGE')
     chipy.mecaMAILx_AssembRHS()

     chipy.utilities_logMes( 'COMPUTE Free Vlocy')
     chipy.mecaMAILx_ComputeFreeVelocity()

     #
     chipy.utilities_logMes( 'SELECT PROX TACTORS')
     chipy.SelectProxTactors()
     #
     chipy.RecupRloc()

     chipy.utilities_logMes( 'RESOLUTION' )
     #chipy.utilities_logMes(stype,quad,tol, relax, gs_it1, gs_it2)
     chipy.nlgs_3D_ExSolver(stype,quad, tol, relax, gs_it1, gs_it2)

     chipy.StockRloc()

     chipy.utilities_logMes( 'COMPUTE DOF, FIELDS, etc.')
     chipy.mecaMAILx_ComputeDof()


   chipy.UpdateTactBehav()
   chipy.StockRloc()

   chipy.ComputeDof()
   #
   chipy.utilities_logMes( 'UPDATE DOF, FIELDS')
   chipy.UpdateStep()
   #
   chipy.utilities_logMes( 'WRITE LAST DOF')
   chipy.WriteOutDof(freq_write)

   ### post3D ###
   chipy.WriteDisplayFiles(freq_display)
   chipy.WritePostproFiles()

### postpro ###
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()
