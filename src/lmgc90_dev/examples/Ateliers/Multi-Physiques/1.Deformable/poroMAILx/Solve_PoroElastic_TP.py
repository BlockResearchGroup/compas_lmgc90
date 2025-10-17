import chipy
import os
import numpy as np

# Time discretization:
dt = 10.0
theta = 1.0
T_final = 1000.0

chipy.TimeEvolution_SetTimeStep(dt)
chipy.NewtonRaphson_SetFinalTime(T_final)
chipy.NewtonRaphson_SetMinTimeStep(dt)
chipy.NewtonRaphson_SetMaxTimeStep(dt)
chipy.NewtonRaphson_SetMaxIter(20)
chipy.NewtonRaphson_SetIncPatience(999999)

# Initialize theta integrator pour toutes les physisques
chipy.Integrator_InitTheta(theta)

# Declaration d'un probleme 2D(2) AXI(3),PLANE STRAIN(1),PLANE STRESS(2)
# Declaration d'un probleme 3D(3)
chipy.SetDimension(2,3)
# Verification des repertoires necessaires
chipy.checkDirectories()

# READ and WRITE BODIES
chipy.MAILx_ReadBodies()
chipy.overall_WriteBodies()
chipy.MAILx_WriteBodies()

# READ and WRITE MODELS
chipy.models_ReadModels()
chipy.models_WriteModels()

# READ and WRITE BEHAVIOURS
chipy.bulk_behav_ReadBehaviours()
chipy.bulk_behav_WriteBehaviours()

# INIT MODELS
chipy.models_InitModels()
chipy.ExternalModels_InitModels()

# Chargement du modele poroMAILx
chipy.poroMAILx_LoadModels()
chipy.poroMAILx_LoadBehaviours()
chipy.poroMAILx_PushProperties()
chipy.poroMAILx_WithRenumbering()
chipy.models_StoreProperties()
chipy.ExternalModels_CheckProperties()

# READ and WRITE Initial condition
chipy.TimeEvolution_ReadIniDof()
chipy.poroMAILx_ReadIniDof()

chipy.TimeEvolution_ReadIniGPV()
chipy.poroMAILx_ReadIniGPV()

# READ and WRITE DRVDOF
chipy.poroMAILx_ReadDrivenDof()
chipy.overall_WriteDrivenDof()
chipy.poroMAILx_WriteDrivenDof()

# Ouverture des fichiers resultats
chipy.OpenDisplayFiles()

while 1:
	# Initialisation d'un nouveau pas de temps
	chipy.TimeEvolution_IncrementStep()
	chipy.poroMAILx_IncrementStep()
	chipy.TimeEvolution_DisplayStep()
	
	# Boucle de Newton
	chipy.NewtonRaphson_Initialize(1.0e-10)
	convergence = 1
	iter = 0
	while convergence ==1:
		# Calcul des matrices elementaires
		chipy.poroMAILx_ComputeFext()
		chipy.poroMAILx_ComputeMass()
		chipy.poroMAILx_ComputeDamping()
		chipy.poroMAILx_ComputeBulk()
				
		# Assemblage du systeme
		chipy.poroMAILx_AssembRHS()
		chipy.poroMAILx_AssembKT()
		
		# Resolution du systeme linearise
		chipy.poroMAILx_ComputeFreeVelocity()
		chipy.poroMAILx_ComputeDof()
		
		# Verification de la convergence si NL
		if iter > 1 :
			norm = chipy.poroMAILx_ComputeResidueNorm()
			convergence  = chipy.NewtonRaphson_CheckConvergence(norm)
		iter += 1
	
	### istate = 1 redo, istate = 2 stop 
	istate = chipy.NewtonRaphson_ComputeTimeStep()
	if istate == 1 :
		
		# Actualisation aux noeuds et aux points de Gauss
		chipy.poroMAILx_UpdateDof()
		chipy.poroMAILx_UpdateBulk()
		chipy.TimeEvolution_UpdateStep()
		
		# Ecriture du fichier resultats
		chipy.WriteDisplayFiles()
		
		if istate == 2 :
			break
			
# Fermeture des fichiers resultats
chipy.CloseDisplayFiles()
