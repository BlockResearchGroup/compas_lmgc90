import os,sys
from pylmgc90 import chipy
import display_gpv as d

import numpy as np

chipy.checkDirectories()

# si le pkl existe c'est qu'on fait du MC
is_MC=os.path.exists("./data.pkl")

####

# info gestion du temps
dt = 1.
nb_steps = 120

t_final = nb_steps*dt 
dt_min = dt
dt_max = dt

forced=True
NR_max_iter = 100
NR_adapt = 9999999
NR_tol = 1.e-6

# Newton loop parameters:
chipy.NewtonRaphson_SetFinalTime(t_final)
chipy.NewtonRaphson_SetMinTimeStep(dt_min)
chipy.NewtonRaphson_SetMaxTimeStep(dt_max)
chipy.NewtonRaphson_SetMaxIter(NR_max_iter)
chipy.NewtonRaphson_SetIncPatience(NR_adapt)

# info generation fichier visu
freq_display = 1
freq_write = 1

# on indique la dimension
chipy.SetDimension(2,1)

### definition des parametres du calcul ### 

# choix du pas de temps
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)

# initialisation integrateur meca
chipy.Integrator_InitQS()

### lecture du modele ###

# lecture du modele
chipy.ReadDatbox()

### post3D ##
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

ff_,kk_ = d.open_gp_joint()
# print(ff_,kk_)

status=[]
status.append(chipy.mecaMAILx_GetDofStatus(1))
materials=[]
materials.append(chipy.mecaMAILx_GetMaterials(1))

if is_MC : f=open('gpi.txt','w')

chipy.utilities_logMes('COMPUTE MASS')
chipy.mecaMAILx_ComputeMass()

# pre endo pour FCZM ; les autres modèles de joint ne l'utilisent pas
# d=0.99944*np.ones(chipy.mecaMAILx_GetNbNodes(1))
# chipy.mecaMAILx_SetScalarFieldByNode(1,1,d)

# boucle en temps
for k in range(1, nb_steps + 1, 1):
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.IncrementStep()

   chipy.utilities_logMes('COMPUTE Fext')
   chipy.ComputeFext()

   # Newton loop
   chipy.NewtonRaphson_Initialize(NR_tol)
   is_converged = 1
   kk=0
   #looping until something changes in CheckConvergence
   while is_converged == 1 :
     kk+=1
     chipy.utilities_logMes('COMPUTE BULK')
     chipy.ComputeBulk()

     chipy.utilities_logMes('ASSEMB RHS/KT')
     chipy.AssembleMechanicalRHS()
     chipy.AssembleMechanicalLHS()

     chipy.utilities_logMes('COMPUTE Free Vlocy')
     chipy.ComputeFreeVelocity()

     chipy.utilities_logMes('COMPUTE DOF')
     chipy.ComputeDof()
     #
     if kk > 1:
       NR_norm = chipy.mecaMAILx_ComputeResidueNorm()
       is_converged = chipy.NewtonRaphson_CheckConvergence(NR_norm)

       if forced and kk == NR_max_iter:
         chipy.utilities_logMes('Warning: forced convergence')        
         is_converged=0

   ### end while NR

   chipy.utilities_logMes('COMPUTE TIME STEP')
   istate = chipy.NewtonRaphson_ComputeTimeStep()

   if forced and kk == NR_max_iter:
     chipy.utilities_logMes('Warning: forced convergence')        
     is_converged=0

   #istate = 1 => redo step
   #istate = 2 => stop

   if not istate == 1 :

     chipy.utilities_logMes('UPDATE TACT BEHAV')
     chipy.UpdateTactBehav()
     chipy.StockRloc()
     
     chipy.utilities_logMes('UPDATE DOF')
     chipy.UpdateStep()

     chipy.utilities_logMes('WRITE out')
     chipy.WriteOut(freq_write)

     chipy.WritePostproFiles()
     if k%freq_display == 0:

       all=chipy.mecaMAILx_GetGpAllJoint()
       kk_ = d.write_gp_joint(ff_,kk_,all)
        
       chipy.WriteDisplayFiles(freq=1,\
                               DrvDof=('mecafe','node',status),\
                               Material=('mecafe','element',materials),\
                               )

     if is_MC :
        # first body,element,gp
        #                                 corps,ele,gp 
        gpi=chipy.mecaMAILx_GetGpInternals(1,3,1)
        s  =chipy.mecaMAILx_GetGpStress(1,3,1)
        dep=chipy.mecaMAILx_GetBodyVector('X____',1)


        # xx va contenir
        # tps,saut de dep(2),contrainte(2),var interne(11)

        xx=np.append([chipy.TimeEvolution_GetTime()],dep[4,:]-dep[3,:],axis=0)
        xx=np.append(xx,s[:],axis=0)  
        xx=np.append(xx,gpi,axis=0)
        np.savetxt(f,xx[np.newaxis])    
       
     chipy.checkInteractiveCommand()

     if istate == 2 :
       # istate == 2  => Stop
       break

if is_MC : f.close()
d.close_gp_joint(ff_)
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()
chipy.Finalize()
