import numpy as np

from pylmgc90 import chipy

chipy.Initialize()

chipy.checkDirectories()

chipy.utilities_DisableLogMes()

# OMEGA = 6rpm
# 1 round = 10s 
# Time to simulate 10s

Tfinal = 3.

dt       = 6.0e-4
theta    = 0.5

nb_steps = int(Tfinal/dt)

print( ' @ Number of time steps: ',nb_steps)

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_detect = 1
freq_display = nb_steps//30
print( ' @ display each ',freq_display,' steps')
freq_write   = nb_steps//30

#         123456789012345678901234567890
stype  = 'Stored_Delassus_Loops         '
norm   = 'Quad '
tol    = 0.1666e-3
relax  = 1.0
gs_it1 = 50
gs_it2 = 200

chipy.SetDimension(2,1)

chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

chipy.utilities_logMes('READ BODIES')
chipy.ReadBodies()

chipy.utilities_logMes('READ BEHAVIOURS')
chipy.ReadBehaviours()

chipy.LoadTactors()
chipy.LoadBehaviours()
# THERMO RIGID
chipy.ReadMpBehaviours(disper=0.,model='therm')

chipy.ReadDrivenDof()

chipy.utilities_logMes('READ INI Vloc Rloc')
chipy.ReadIni()

chipy.utilities_logMes('WRITE BODIES')
chipy.WriteBodies()

chipy.utilities_logMes('WRITE BEHAVIOURS')
chipy.WriteBehaviours()

chipy.utilities_logMes('WRITE DRIVEN DOF')
chipy.WriteDrivenDof()

nbdiskx     = chipy.DISKx_GetNbDISKx()
diskx2rbdy2 = chipy.DISKx_GetPtrDISKx2BDYTY()
nbR2        = chipy.RBDY2_GetNbRBDY2()

Th_DISKx = np.zeros([nbdiskx])
Th_xKSID = np.zeros([1])
T_dict = {'DISKx':Th_DISKx,
          'xKSID':Th_xKSID,
         }

chipy.OpenPostproFiles()
chipy.OpenDisplayFiles()
chipy.WriteDisplayFiles(1, T=('tacts',T_dict) )

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

for k in range(1, nb_steps + 1, 1):
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.IncrementStep()
   #
   chipy.utilities_logMes('COMPUTE Fext')
   chipy.ComputeFext()
   #
   chipy.utilities_logMes('COMPUTE Fint')
   chipy.ComputeBulk()
   #
   chipy.utilities_logMes('COMPUTE Free Vlocy')
   chipy.ComputeFreeVelocity()
   #
   chipy.utilities_logMes('SELECT PROX TACTORS')
   chipy.SelectProxTactors(freq_detect)
   #
   chipy.RecupRloc()
   chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
   chipy.StockRloc()
   #
   # THERMO RIGID
   #
   chipy.mp_solver_RecupTemperature()
   chipy.mp_solver_SolveThermoProblem()
   #
   chipy.utilities_logMes('COMPUTE DOF')
   chipy.ComputeDof()
   #
   chipy.utilities_logMes('UPDATE DOF')
   chipy.UpdateStep()
   #
   chipy.utilities_logMes('WRITE OUT DOF')
   chipy.WriteOut(freq_write)
   #
   # THERMO RIGID
   #
   chipy.WriteOutMpValues(freq_write)
   #
   chipy.WritePostproFiles()
   #
   if (k % freq_display == 0 ):
      #
      for itacty in range(1,nbdiskx+1,1):
         iddiskx = diskx2rbdy2[itacty-1]
         idR2 = int(iddiskx[0])
         idTy = int(iddiskx[1])
         # THERMO RIGID
         Th_DISKx[itacty-1] = chipy.RBDY2_GetThermalValue(idR2,idTy)
      #
      chipy.WriteDisplayFiles(1, T=('tacts',T_dict) )
#
chipy.utilities_logMes('WRITE LAST DOF')
chipy.WriteLastDof()
#
chipy.utilities_logMes('WRITE LAST Vloc Rloc')
chipy.WriteLastVlocRloc()
#
chipy.ClosePostproFiles()
chipy.CloseDisplayFiles()
#
chipy.Finalize()
