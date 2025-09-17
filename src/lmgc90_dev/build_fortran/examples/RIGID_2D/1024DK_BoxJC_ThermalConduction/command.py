import numpy as np

from pylmgc90 import chipy

chipy.checkDirectories()

## definition des parametres du calcul ### 

dt       = 2e-4
theta    = 0.5
nb_steps = 10000

freq_write   = 100
freq_display = 100

stype = 'Stored_Delassus_Loops        '
quad  = 'Quad '
tol   = 1.666e-5
relax = 1.
it1   = 33
it2   = 101

# on indique qu'on travaille en deformation planes
chipy.Initialize()
chipy.SetDimension(2)

chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

# lecture des corps
chipy.utilities_logMes('READ BODIES')
chipy.ReadBodies()

# lecture des parametres materiaux
chipy.utilities_logMes('READ BEHAVIOURS')
chipy.ReadBehaviours()

chipy.LoadTactors()
chipy.LoadBehaviours()

# serveral model to load
# so cannot call macro function
chipy.mp_solver_ReadMpBehaviour()
chipy.RBDY2_MP_LoadBehaviours(0.,'therm')
# to manage surface energy
#chipy.RBDY2_MP_LoadBehaviours(0.,'sener')

chipy.utilities_logMes('READ INI')
chipy.ReadIni()

chipy.utilities_logMes('READ DRV DOF')
chipy.ReadDrivenDof()

### ecriture paranoiaque du modele ###
chipy.utilities_logMes('WRITE BODIES')
chipy.WriteBodies()

# ecriture des parametres materiaux
chipy.utilities_logMes('WRITE BEHAVIOURS')
chipy.WriteBehaviours()
chipy.WriteMpBehaviours()

chipy.ComputeMass()

##################
#
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()
#
nbdiskx = chipy.DISKx_GetNbDISKx()
nbjoncx = chipy.JONCx_GetNbJONCx()
#
Td =  np.zeros([nbdiskx])
Tj =  np.zeros([nbjoncx])
#
disk2rbdy2 = chipy.DISKx_GetPtrDISKx2BDYTY()
jonc2rbdy2 = chipy.DISKx_GetPtrDISKx2BDYTY()

T_dict = {'DISKx':Td,
          'JONCx':Tj,
         }

##################

# boucle en temps
for k in range(1,nb_steps+1,1):
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.IncrementStep()
   #
   chipy.ComputeFext()
   chipy.ComputeBulk()
   chipy.ComputeFreeVelocity()

   chipy.SelectProxTactors()

   chipy.RecupRloc()
   chipy.ExSolver(stype,quad,tol,relax,it1,it2)
   chipy.StockRloc()

   chipy.utilities_logMes('THERMAL RESOLUTION')
   chipy.mp_solver_RecupTemperature()
   chipy.mp_solver_SolveThermoProblem()

   # if surface energy is defined
   #chipy.utilities_logMes('PHYSICO-CHEMISTRY UPDATING')
   #chipy.RBDY2_UpdateWSvsT()

   chipy.ComputeDof()
   #
   chipy.WriteOut(freq_write)
   #
   chipy.WriteOutMpValues(freq_write)
   #
   chipy.WritePostproFiles()

   if (k % freq_display == 0 ):
     for itacty in range(1,nbdiskx+1,1):
        iddiskx = disk2rbdy2[itacty-1]
        idR2 = int(iddiskx[0])
        idTy = int(iddiskx[1])
        Td[itacty-1] = chipy.RBDY2_GetThermalValue(idR2,idTy)
      
     for itacty in range(1,nbjoncx+1,1):
        idjoncx = jonc2rbdy2[itacty-1]
        idR2 = int(idjoncx[0])
        idTy = int(idjoncx[1])
        Tj[itacty-1] = chipy.RBDY2_GetThermalValue(idR2,idTy)
       
     chipy.WriteDisplayFiles(freq=1, T=('tacts', T_dict))

   chipy.UpdateStep()
   ### gestion des writeout ###

chipy.ClosePostproFiles()
chipy.CloseDisplayFiles()
#
chipy.Finalize()
