from pylmgc90 import chipy
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

chipy.ReadDatbox()

# Ouverture des fichiers resultats
chipy.OpenDisplayFiles(True)

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
	if not istate == 1 :
		
		# Actualisation aux noeuds et aux points de Gauss
		chipy.TimeEvolution_UpdateStep()
		chipy.poroMAILx_UpdateDof()
		chipy.poroMAILx_UpdateBulk()
				
		# Ecriture du fichier resultats
		chipy.WriteDisplayFiles()
		
		if istate == 2 :
			break
			
# Fermeture des fichiers resultats
chipy.CloseDisplayFiles()
