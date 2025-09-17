from pylmgc90 import chipy


chipy.Initialize()

chipy.checkDirectories()

chipy.utilities_DisableLogMes()

####
# info gestion du temps
dt       = 1.0e-7
theta    = 0.5
nb_steps = 50

# info gestion du contact
stype = 'Stored_Delassus_Loops         '
ntype = 'QM/16'
tol = 1.e-5
relax = 1.0
gs_it1 = 50
gs_it2 = 10 

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 1
freq_write   = 1

# on indique qu'on travaille en deformation planes
chipy.SetDimension(2,1)

### definition des parametres du calcul ### 

# choix du pas de temps
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)

# initialisation de l'integrateur en temps :
# en thermique, on utilise Cranck-Nicholson et pas la theta-methode
# utilisee en meca
chipy.Integrator_InitTheta(theta)
chipy.Integrator_InitCrankNickolson(theta)


chipy.therMAILx_SparseStorage()
chipy.mecaMAILx_SparseStorage()

#
chipy.ReadDatbox()
chipy.registerInterInternals('beta')
chipy.addRegistersToDisplay(True)
#

chipy.gts_Initialize()

chipy.ComputeMass()
chipy.ComputeBulk()
chipy.AssembleMechanicalLHS()

chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

for k in range(1, nb_steps + 1, 1):
   #
   chipy.IncrementStep()

   display_T = []
   for a in range(chipy.therMAILx_GetNbTherMAILx()):
     T=chipy.therMAILx_GetBodyVector('T____',a+1)
     T.shape = [T.size]
     TB=chipy.therMAILx_GetBodyVector('Tbeg_',a+1)
     TB.shape = [TB.size]
     chipy.mecaMAILx_SetScalarFieldByNode(a+1,1,theta*T+(1.-theta)*TB)
     display_T.append(theta*T+(1.-theta)*TB)

   # mechanical prediction part
   chipy.utilities_logMes('MECA COMPUTE')
   chipy.mecaMAILx_ComputeFext()
   chipy.mecaMAILx_ComputeBulk()
   chipy.AssembleMechanicalRHS()
   chipy.mecaMAILx_ComputeFreeVelocity() 
   # mechanical interaction part
   chipy.SelectProxTactors()
   chipy.RecupRloc()
   chipy.ExSolver(stype, ntype, tol, relax, gs_it1, gs_it2)
   chipy.UpdateTactBehav()

   if k == 1:
     inters = chipy.getInteractions(this=True, human=False)
     inters_law3 = inters[ inters['behav'] == 3 ]
     chipy.setInternalArray('beta', inters_law3, 0.)

   chipy.StockRloc()

   # mechanical correction part
   chipy.mecaMAILx_ComputeDof()
   chipy.mecaMAILx_ComputeField()

   # thermal  part
   chipy.utilities_logMes('COMPUTE EXTERNAL FLUX')
   chipy.therMAILx_ComputeExternalFlux()

   chipy.utilities_logMes('COMPUTE CAPACITY')
   chipy.therMAILx_ComputeCapacity()
   
   chipy.utilities_logMes('COMPUTE CONDUCTIVITY')
   chipy.therMAILx_ComputeConductivity()
   
   chipy.utilities_logMes('COMPUTE INTERNAL FLUX')
   chipy.therMAILx_ComputeInternalFlux()

   #Resolution avec le global solveur   
   chipy.utilities_logMes('prep')
   chipy.gts_PrepSystem()
   chipy.utilities_logMes('assemble lhs')
   chipy.gts_AssembleLHS()
   chipy.utilities_logMes('assemble rhs')
   chipy.gts_AssembleRHS()
   chipy.utilities_logMes('solve')
   chipy.gts_Solve()

   # resolution, calcul  de la temperature
   chipy.utilities_logMes('COMPUTE THERM FIELDS')
   chipy.therMAILx_ComputeThermFields()
   

   # mise a jour des valeurs aux noeuds
   chipy.utilities_logMes('UPDATE')
   chipy.UpdateStep()

   chipy.WriteOut(freq_write)
   # Pour voir les valeurs des internals()
   # print 'internals', internals[:,4]
   internals = chipy.inter_handler_2D_getAllInternal( chipy.CLALp_ID )

   chipy.WriteDisplayFiles(freq_display, Temp=('mecafe','node',display_T) )
   chipy.WritePostproFiles()

chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()
chipy.Finalize()
chipy.gts_Finalize()
# this is the end
