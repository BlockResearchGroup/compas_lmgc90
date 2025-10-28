import pylmgc90.chipy as chipy
import os
import numpy as np

# make directories
chipy.checkDirectories()

# Time discretization:
dt = 10.0
theta = 1.0
n_steps = 1

chipy.TimeEvolution_SetTimeStep(dt)
chipy.NewtonRaphson_SetFinalTime(dt*n_steps)
chipy.NewtonRaphson_SetMinTimeStep(dt)
chipy.NewtonRaphson_SetMaxTimeStep(dt)

# Initialize theta integrator pour toutes les physisques
chipy.Integrator_InitTheta(theta)

# Use of MUMPs not availalbe for scaling problem
# Young modulus must be > 1.e-2 for the system
# being correctly inverted by the library.
#chipy.poroMAILx_SparseStorage()

# Declaration d'un probleme 2D AXY
chipy.SetDimension(2,3)
chipy.OpenDisplayFiles()

# READ and WRITE BODIES
chipy.ReadDatbox()

# Definition des noeuds fluides pour actualisation
chipy.poroMAILx_LoadALE(1)

n = 0
while 1:
	# Initialisation d'un nouveau pas de temps
	chipy.IncrementStep()

	# Boucle de Newton
	chipy.NewtonRaphson_Initialize(1.0e-8)
	convergence = 1
	iter = 0
	while convergence ==1:
		# Calcul des matrices elementaires
		chipy.poroMAILx_ComputeBulk()
		chipy.poroMAILx_ComputeDamping()
		
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
	
	### istate = 0 ok, istate = 1 redo, istate = 2 ok and stop
	# ------------------------------------------------------
	istate = chipy.NewtonRaphson_ComputeTimeStep()
	if not istate == 1: 
		
		# Actualisation aux noeuds et aux points de Gauss
		# ------------------------------------------------------
		chipy.TimeEvolution_UpdateStep()
		chipy.poroMAILx_UpdateDof()
		chipy.poroMAILx_UpdateBulk()

		# Ecriture pour restart
		# ------------------------------------------------------
		chipy.TimeEvolution_WriteLastGPV()
		chipy.MAILx_WriteLastGPV()
		chipy.TimeEvolution_WriteLastDof()
		chipy.poroMAILx_WriteLastDof()
		
		# Creation des fichier vtk
		# ------------------------------------------------------
		chipy.WriteDisplayFiles()
		
		if istate == 2 :
			break
			
# Fermeture des fichiers de visualisation
# ------------------------------------------------------
chipy.CloseDisplayFiles()
chipy.Finalize()
