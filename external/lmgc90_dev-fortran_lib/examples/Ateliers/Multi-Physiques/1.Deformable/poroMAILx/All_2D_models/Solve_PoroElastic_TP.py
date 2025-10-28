import chipy
from Class_Mesh import gmsh
import os
import numpy as np

# Lecture du maillage

mesh = gmsh(path = os.getcwd(), file_in = 'Mesh.msh', file_out = 'Not_defined')
mesh.ReadNodes()
mesh.ReadElement()
mesh.Qua4 = mesh.Qua8[:,0:6]
mesh.new_Qua4 = mesh.new_Qua8[:,0:6]

# Time discretization:
dt = 10.0
theta = 1.0

chipy.TimeEvolution_SetTimeStep(dt)
chipy.NewtonRaphson_SetFinalTime(dt)
chipy.NewtonRaphson_SetMinTimeStep(dt)
chipy.NewtonRaphson_SetMaxTimeStep(dt)
chipy.NewtonRaphson_SetMaxIter(20)
chipy.NewtonRaphson_SetIncPatience(999999)

# Initialize theta integrator pour toutes les physisques
chipy.Integrator_InitTheta(theta)

# Declaration d'un probleme 2D(2) AXI(3),PLANE STRAIN(1),PLANE STRESS(2)
# Declaration d'un probleme 3D(3)
chipy.overall_DIME(2,3)
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

# Recuperation du nombre de noeuds du corps
nb_node = chipy.poroMAILx_GetNbNodes(1)
n = 0
while n < 100:
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
	
	# Actualisation aux noeuds et aux points de Gauss
	chipy.poroMAILx_UpdateDof()
	chipy.poroMAILx_UpdateBulk()
	chipy.TimeEvolution_UpdateStep()
	
	# Recuperation des informations vectorielles
	U = chipy.poroMAILx_GetBodyVector('X____',1,nb_node*2)
	U1 = U.reshape((2,nb_node),order = 'F').T
	F = chipy.poroMAILx_GetBodyVector('Fint_',1,nb_node*2)
	F1 = F.reshape((2,nb_node),order = 'F').T
	# Recuperation des informations scalaires
	P0 = chipy.poroMAILx_GetBodyVector('Pbeg_',1,nb_node)
	P = chipy.poroMAILx_GetBodyVector('P____',1,nb_node)
	P1 = np.zeros((P.shape[0],1),float)
	P1[:,0] = P 

	# Ecriture du fichier resultats
	Solution = np.concatenate((np.concatenate((U1[:nb_node],F1[:nb_node]),axis = 1),P1),axis = 1)	                   
	mesh.write_vtk_nodal_value(result = Solution, vec_title=['U','F'], sca_title = ['P'], mesh = 'Qua4',\
			                   n = n, path = os.getcwd() + os.sep + 'DISPLAY', name = 'solution', dim = 2)
	
	n += 1
