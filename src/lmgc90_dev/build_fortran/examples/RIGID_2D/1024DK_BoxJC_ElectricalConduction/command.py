import numpy as np

from pylmgc90 import chipy

chipy.checkDirectories()

chipy.Initialize()
chipy.SetDimension(2)

### definition des parametres du calcul ### 

dt       = 2.e-4
nb_steps = 4000
theta    = 0.5

freq_write   = 100
freq_display = 40

norm   = 'Stored_Delassus_Loops         '
quad   = 'Quad '
tol    = 0.1666e-4
relax  = 1.0
gs_it1 = 33
gs_it2 = 101

chipy.nlgs_SetWithQuickScramble()

chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### lecture du modele ###

chipy.utilities_logMes('READ BEHAVIOURS')
chipy.ReadBehaviours()

### model reading ###
chipy.utilities_logMes('READ BODIES')
chipy.ReadBodies()

chipy.LoadTactors()
chipy.LoadBehaviours()

chipy.ReadMpBehaviours(0.,'elect')

chipy.utilities_logMes('READ INI')
chipy.ReadIni()

chipy.utilities_logMes('READ DRIVEN DOF')
chipy.ReadDrivenDof()

### ecriture paranoiaque du modele ###
chipy.utilities_logMes('WRITE BODIES')
chipy.WriteBodies()

chipy.utilities_logMes('WRITE BEHAVIOURS')
chipy.WriteBehaviours()
chipy.WriteMpBehaviours()

chipy.utilities_logMes('WRITE DRIVEN DOF')
chipy.WriteDrivenDof()

### postpro ###
chipy.OpenPostproFiles()
chipy.OpenDisplayFiles()

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()


nbdiskx = chipy.DISKx_GetNbDISKx()
nbjoncx = chipy.JONCx_GetNbJONCx()

EPot_d =  np.zeros([nbdiskx])
EPot_j =  np.zeros([nbjoncx])

disk2rbdy2 = chipy.DISKx_GetPtrDISKx2BDYTY()
jonc2rbdy2 = chipy.JONCx_GetPtrJONCx2BDYTY()

EPot_dict = {'DISKx':EPot_d,
             'JONCx':EPot_j,
            }

for k in range(1,nb_steps+1,1):
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.IncrementStep()

   chipy.utilities_logMes('COMPUTE Fext')
   chipy.ComputeFext()

   chipy.utilities_logMes('COMPUTE Fint')
   chipy.ComputeBulk()
   
   chipy.utilities_logMes('COMPUTE Free Vlocy')
   chipy.ComputeFreeVelocity()
   #
   chipy.utilities_logMes('SELECT PROX TACTORS')
   chipy.SelectProxTactors()
   #
   chipy.utilities_logMes('EX NLGS SOLVER')
   #
   chipy.RecupRloc()
   chipy.ExSolver(norm, quad, tol, relax, gs_it1, gs_it2)
   chipy.StockRloc()
   #
   chipy.utilities_logMes('EX NL ELECTRICAL SOLVER')
   #
   chipy.mp_solver_SolveElectro1G()
   #
   chipy.utilities_logMes('COMPUTE DOF')
   chipy.ComputeDof()
   #
   chipy.utilities_logMes('WRITE OUT')
   chipy.WriteOut(freq_write)
   chipy.WriteOutMpValues(freq_write)
   #
   for itacty in range(1,nbdiskx+1,1):
      iddiskx = disk2rbdy2[itacty-1]
      idR2 = int(iddiskx[0])
      idTy = int(iddiskx[1])
      EPot_d[itacty-1] = chipy.RBDY2_GetElectricalPotential(idR2)

   for itacty in range(1,nbjoncx+1,1):
      idjoncx = jonc2rbdy2[itacty-1]
      idR2 = int(idjoncx[0])
      idTy = int(idjoncx[1])
      EPot_j[itacty-1] = chipy.RBDY2_GetElectricalPotential(idR2)

      
   inters  = chipy.getInteractions()
   il_dkdk  = (inters['inter']==b'DKDKx').nonzero()[0]
   il_dkjc  = (inters['inter']==b'DKJCx').nonzero()[0]

   UI_inte =  np.zeros([inters.size,2])

   for i_dkdk, i_inter in enumerate(il_dkdk):
      UIC = chipy.mp_solver_GetBrancheValues('DKDKx',i_dkdk+1)
      UI_inte[i_inter,:] = UIC[:2]

   for i_dkjc, i_inter in enumerate(il_dkjc):
      UIC = chipy.mp_solver_GetBrancheValues('DKJCx',i_dkjc+1)
      UI_inte[i_inter,:] = UIC[:2]

   ### viz ###
   chipy.WriteDisplayFiles(freq=freq_display,
                           Epot=('tacts', EPot_dict) ,
                           Ue  =('ptc', UI_inte[:,0]),
                           Ie  =('ptc', UI_inte[:,1]),
                          )
   ### postpro ###
   chipy.WritePostproFiles()
   #
   chipy.utilities_logMes('UPDATE DOF')
   chipy.UpdateStep()

   #
### postpro ###
chipy.ClosePostproFiles()
chipy.CloseDisplayFiles()
chipy.Finalize()
