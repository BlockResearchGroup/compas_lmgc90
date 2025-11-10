
from pylmgc90 import chipy

chipy.checkDirectories()

chipy.utilities_DisableLogMes()

# Time discretization:
dt = 1.E-5
#t_final = 0.1E0
t_final = 1.E-03
chipy.TimeEvolution_SetTimeStep(dt)
chipy.NewtonRaphson_SetFinalTime(t_final)
chipy.NewtonRaphson_SetMinTimeStep(dt)
chipy.NewtonRaphson_SetMaxTimeStep(dt)
NR_tol = 1.e-3

# Newton loop parameters:
chipy.NewtonRaphson_SetMaxIter(20)
chipy.NewtonRaphson_SetIncPatience(999999)

# Initialize theta integrator
chipy.Integrator_InitTheta(0.5E0)

# info generation fichier visu
freq_display = 10

# info generation fichier de sortie
freq_write = 10

# info contact

#         123456789012345678901234567890
stype  = 'Stored_Delassus_Loops         '
ntype  ='Quad '
tol    = 0.1666E-04
relax  = 1.E0
gs_it1 = 101
gs_it2 = 201

# 2D PSTRAIN
chipy.SetDimension(2,1)

chipy.nlgs_SetWithQuickScramble()

### model reading ###
chipy.ReadDatbox()

### post2D ##
chipy.OpenDisplayFiles()
chipy.WriteDisplayFiles(1)

### postpro ###

chipy.CLxxx_SetNbNodesByCLxxx(1)

chipy.CLALp_SetNonSymmetricDetection()

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

while chipy.TimeEvolution_GetTime() < t_final :
   #
   #chipy.utilities_logMes('INCREMENT STEP')
   chipy.IncrementStep()

   #chipy.utilities_logMes('DISPLAY TIMES')
   chipy.TimeEvolution_DisplayStep()

   #chipy.utilities_logMes('COMPUTE Fext')
   chipy.ComputeFext()

   #chipy.utilities_logMes('COMPUTE Free Vlocy')
   chipy.ComputeFreeVelocity()

   chipy.NewtonRaphson_Initialize(NR_tol)
   k=0
   is_converged = 1
   # Newton loop
   while is_converged == 1 : #looping until something changes in CheckNlConvergence
            
      k+=1
      #chipy.utilities_logMes('COMPUTE (gd) NL BULK')
      #chipy.utilities_logMes('COMPUTE Fint')
      chipy.ComputeBulk()

      #chipy.utilities_logMes('ASSEMB NL RHS/KT')
      chipy.AssembleMechanicalRHS()
      chipy.AssembleMechanicalLHS()

      #chipy.utilities_logMes('COMPUTE Free Vlocy')
      chipy.ComputeFreeVelocity()
      #
      #chipy.utilities_logMes('SELECT PROX TACTORS')
      chipy.SelectProxTactors()
      #
      ### Signorini Coulomb
      chipy.RecupRloc()
      chipy.ExSolver(stype, ntype, tol, relax, gs_it1, gs_it2)
      chipy.StockRloc()
      ###

      #chipy.utilities_logMes('COMPUTE DOF')
      chipy.ComputeDof()
      #
      if k > 1:
        norm = chipy.mecaMAILx_ComputeResidueNorm()
        is_converged = chipy.NewtonRaphson_CheckConvergence(norm) 

      if is_converged == 0 or is_converged == 2 :
        break

   ### end while

   #chipy.utilities_logMes('COMPUTE DOF')
   chipy.ComputeDof()

   #chipy.utilities_logMes('COMPUTE TIME STEP')
   istate = chipy.NewtonRaphson_ComputeTimeStep()
   if istate == 1 :
     # istate => Redo Step
     continue
   elif istate == 2 :
     # istate => Stop
     break

   #chipy.utilities_logMes('UPDATE TACT BEHAV')
   chipy.nlgs_UpdateTactBehav()
   chipy.StockRloc()

   #chipy.utilities_logMes('UPDATE DOF')
   chipy.UpdateStep()
   #
   ### write results ###
   #chipy.utilities_logMes('WRITE LAST DOF')
   chipy.WriteLastDof()
   #
   #chipy.utilities_logMes('WRITE LAST Vloc Rloc')
   chipy.WriteLastVlocRloc()
   #
   #chipy.utilities_logMes('WRITE LAST GPV')
   chipy.WriteLastGPV()

   ### post2D ###
   chipy.WriteOut(freq_write)
   chipy.WriteOutRnod()
   #
   chipy.WriteDisplayFiles(freq_display)
   ### postpro ###
   #chipy.WritePostproFiles()

### end of time loop ###

### write results ###
chipy.utilities_logMes('WRITE LAST DOF')
chipy.WriteLastDof()
#
chipy.utilities_logMes('WRITE LAST Vloc Rloc')
chipy.WriteLastVlocRloc()
#
chipy.utilities_logMes('WRITE LAST GPV')
chipy.WriteLastGPV()

chipy.CloseDisplayFiles()
### postpro ###
#chipy.ClosePostproFiles()

chipy.Finalize()
