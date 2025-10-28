from pylmgc90 import chipy
#

chipy.utilities_DisableLogMes()

dt = 1.e-3
mass_damping = 0.0
bulk_damping = 0.0

# Time discretization:
t_final = 0.25
theta = 0.5

freq_write=50
freq_display=50

# driving Newton Raphson
NR_nb_iter_max = 10
NR_tol=1e-6

chipy.Initialize()

# driving contact solveur
#       123456789012345678901234567890
stype = 'Stored_Delassus_Loops         '
norm_contact = 'QM/16'
tol =0.1E-04
relax = 1.0
gs_it1 = 101
gs_it2 = 30

#~ chipy.nlgs_3D_SetWithQuickScramble()
chipy.nlgs_3D_DiagonalResolution()
chipy.mecaMAILx_SparseStorage()

# Gestion de l'avance en temps
chipy.TimeEvolution_SetTimeStep(dt)
chipy.NewtonRaphson_SetFinalTime(t_final)
chipy.NewtonRaphson_SetMinTimeStep(dt/100.0)
chipy.NewtonRaphson_SetMaxTimeStep(dt)
chipy.NewtonRaphson_SetMaxIter(NR_nb_iter_max)

# Initialize theta integrator pour toutes les physisques
chipy.Integrator_InitTheta(theta)

# Adaptation du pas de temps
chipy.NewtonRaphson_SetIncPatience(2)
chipy.NewtonRaphson_SetGoodIter(3)
chipy.NewtonRaphson_SetBadIter(10)


# Declaration d'un probleme 3D
chipy.SetDimension(3,0)
chipy.checkDirectories()

# READ and WRITE BODIES
chipy.ReadDatbox()

# Creation fichiers VTK
chipy.OpenPostproFiles()
chipy.OpenDisplayFiles()
chipy.WriteDisplayFiles()


# Calcul des matrices elementaires
chipy.mecaMAILx_ComputeMass()

# Lancement avance en temps du calcul
while 1:

    # Initialisation d'un nouveau pas de temps
    chipy.IncrementStep()

    # Boucle de Newton Raphson
    is_converged = 1
    nb_iter = 0
    chipy.NewtonRaphson_Initialize(NR_tol)
      
    while is_converged == 1 :
      
        # Calcul des matrices elementaires
        chipy.mecaMAILx_ComputeFext()
        chipy.mecaMAILx_ComputeBulk()
        chipy.mecaMAILx_ComputeRayleighDamping(mass_damping, bulk_damping)

        # Assemblage du systeme
        chipy.mecaMAILx_AssembKT()
           
        chipy.mecaMAILx_AssembRHS()
        # Resolution du systeme linearise
        chipy.mecaMAILx_ComputeFreeVelocity()
      
        # Detection des contacts
        chipy.utilities_logMes('SELECT PROX TACTORS')
        chipy.SelectProxTactors()
        chipy.RecupRloc()
        
        # Resolution des contacts
        chipy.utilities_logMes('RESOLUTION DES CONTACT' )
        chipy.ExSolver(stype, norm_contact, tol, relax, gs_it1, gs_it2)
        chipy.UpdateTactBehav()
        chipy.StockRloc()
      
        # Correction des deformations par les contacts
        chipy.mecaMAILx_ComputeDof()
      
        # Verification de la convergence si NL
        if nb_iter >= 1 :
            norm = chipy.mecaMAILx_ComputeResidueNorm([1])
            is_converged  = chipy.NewtonRaphson_CheckConvergence(norm)
         
        nb_iter += 1

    ### istate = 0 ok, istate = 1 redo, istate = 2 ok and stop
    istate = chipy.NewtonRaphson_ComputeTimeStep()

    if not istate == 1: 
       
        # Actualisation des contacts
        chipy.mecaMAILx_ComputeField()
        chipy.nlgs_3D_UpdateTactBehav()
        chipy.StockRloc()
      
        # Actualisation aux noeuds et aux points de Gauss
        chipy.mecaMAILx_UpdateDof()
        chipy.mecaMAILx_UpdateBulk()
        
        # Update Time
        chipy.TimeEvolution_UpdateStep()
        chipy.WritePostproFiles()
      
        # Ecriture pour restart
        chipy.WriteOut(freq_write)

        # Ecriture de la solution
        chipy.WriteDisplayFiles(freq=freq_display)
      
        if istate == 2:
         
            # Ecriture de la solution
            chipy.CloseDisplayFiles()
            chipy.ClosePostproFiles()
            break
         
chipy.Finalize()
