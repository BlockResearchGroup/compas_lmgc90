# gag si particules en dehors maillage - follow par encore dans gmsh

from __future__ import print_function
import os,sys
from pylmgc90.chipy import *
from ParticlesToMesh import *

dim=2

###
# on charge le tracker
my_tracker = ParticlesToMesh('./gmsh/darcy',dim, '8', 3)

### definition des parametres du calcul ### 

checkDirectories()

p0=1.

dt = 1e-4
nb_steps= 5000
theta = 0.5

freq_write = 100

freq_display = 1

norm='Stored_Delassus_Loops         '
quad = 'Quad '
tol = 1e-5
relax = 1.0
gs_it1 = 200
gs_it2 = 10

nlgs_SetWithQuickScramble()

SetDimension(dim,2)

print('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)
Integrator_InitCrankNickolson(theta)
### lecture du modele ###

print('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()
models_ReadModels()

### model reading ###
print('READ BODIES')
RBDY2_ReadBodies()
MAILx_ReadBodies()

print('LOAD MODELS')
models_InitModels()
ExternalModels_InitModels()
therMAILx_LoadModels()
therMAILx_LoadBehaviours()
therMAILx_PushProperties()
models_StoreProperties()
ExternalModels_CheckProperties()

print('READ DRIVEN DOF')
RBDY2_ReadDrivenDof()
therMAILx_ReadDrivenDof()

print('BULK READ INI')
TimeEvolution_ReadIniDof()
RBDY2_ReadIniDof()
therMAILx_ReadIniDof()

TimeEvolution_ReadIniGPV()
therMAILx_ReadIniGPV()

#LOADS
DISKx_LoadTactors()
JONCx_LoadTactors()
RBDY2_LoadBehaviours()

print('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
DKJCx_ReadIniVlocRloc()
DKDKx_ReadIniVlocRloc()

### ecriture paranoiaque du modele ###
print('WRITE BODIES')
overall_WriteBodies()
RBDY2_WriteBodies()
MAILx_WriteBodies()

print('WRITE BEHAVIOURS')
models_WriteModels()
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

print('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
RBDY2_WriteDrivenDof()
therMAILx_WriteDrivenDof()

### postpro & dislay ###
OpenDisplayFiles()
OpenPostproFiles()

####

print('COMPUTE MASS')
RBDY2_ComputeMass()

# des choses pour gerer le couplage ...
# ... le nombre de particules moins les parois
nb_rbdy2 = RBDY2_GetNbRBDY2() - 4
# ... le nombre de noeuds du maillage
nb_nodes = therMAILx_GetNbNodes(1)

sphv_rank=chipy.therMAILx_GetScalarFieldRank(1,1,'SPHV')
Cp=np.zeros(nb_nodes)

coco_rank=chipy.therMAILx_GetScalarFieldRank(1,1,'COCO')
Cd=np.zeros(nb_nodes)

# on cree un champs nodal pour pouvoir calculer le forcage par la vitesse solide
# on declare 1 champ nodal sur le corps 1
MAILx_InitNodalFields(1,1)

# on le dimensionne (on passe le nb de composantes / noeud)
                       #123456789012345678901234567890
MAILx_InitNodalField(1,'Vs                            ',1,dim)

# ce rank n'est pas le meme que celui affecte passant par modele
vs_rank=1 

Vs=np.zeros(dim*nb_nodes)
gradP=np.zeros((nb_nodes,dim))

kk=0
for k in range(1,nb_steps+1,1):
   #
   print('INCREMENT STEP')
   TimeEvolution_IncrementStep()
   RBDY2_IncrementStep()
   therMAILx_IncrementStep()

   print('DISPLAY TIMES')
   TimeEvolution_DisplayStep()

   print('COMPUTE Fext')
   RBDY2_ComputeFext()

   print(1,RBDY2_GetBodyVector('Fext_', 1))
   print(nb_rbdy2,RBDY2_GetBodyVector('Fext_', nb_rbdy2))         

   # calcule de la force du fluide sur les grains
   if k > 1:
     Fext=my_tracker.ComputeParticleForce(gradP)

     for i in range(nb_rbdy2):
       #print i+1,[Fext[2*i], Fext[2*i+1], 0.]
       RBDY2_PutBodyVector('Fext_', i+1, [Fext[2*i], Fext[2*i+1], 0.])

            
   else:
     Fext=np.zeros(nb_rbdy2*dim)

   print('Fext:',Fext.shape)
   print(1,RBDY2_GetBodyVector('Fext_', 1))
   print(nb_rbdy2,RBDY2_GetBodyVector('Fext_', nb_rbdy2))         
     
   print('COMPUTE Fint')
   RBDY2_ComputeBulk()
   
   print('COMPUTE Free Vlocy')
   RBDY2_ComputeFreeVelocity()
   #
   print('SELECT PROX TACTORS')
   overall_SelectProxTactors()
   DKJCx_SelectProxTactors()
   DKDKx_SelectProxTactors()
   #
   print('SOLVE')
   DKJCx_RecupRloc()
   DKDKx_RecupRloc()
   nlgs_ExSolver(norm,quad, tol, relax, gs_it1, gs_it2)
   DKJCx_StockRloc()
   DKDKx_StockRloc()
   #
   print('COMPUTE DOF')
   RBDY2_ComputeDof()

   # -> upscaling    

   my_tracker.cleanMesh()

   for i in range(1,nb_rbdy2+1):
      coor=RBDY2_GetBodyVector('Coor_',i)
      area = RBDY2_GetBodyArea(i)
      vel_=RBDY2_GetBodyVector('V____',i)

      my_tracker.pushOneParticleToMesh(np.array((coor[0],coor[1],0.)),np.array((area,area*vel_[0],area*vel_[1])))

   #                                   p0, d, mu      
   my_tracker.ComputeNodalFieldsOnMesh(p0,14.e-5,1.8e-5,Cp,Cd,Vs)

   print('Cp:',Cp.shape)
   #print Cp
   print('Cd:',Cd.shape)
   #print Cd
   print('Vs:',Vs.shape)
   #print Vs

   therMAILx_SetScalarFieldByNode(1,sphv_rank,Cp)
   therMAILx_SetScalarFieldByNode(1,coco_rank,Cd)

   MAILx_SetNodalField(1,vs_rank,-p0*Vs)

   # -> on fait avancer le fluide

   therMAILx_ComputeExternalFlux()

   # ajout du terme de forcage en div(vs)
   therMAILx_AddNodalFieldDivergence(1,vs_rank)

   #therMAILx_AddSource(1,chipy.therMAILx_GetScalarFieldRank(1,1,'SOURCE'))

   therMAILx_ComputeCapacity()
   therMAILx_ComputeConductivity()
   therMAILx_AssembThermRHS()
   therMAILx_AssembThermKT()

   therMAILx_ComputeThermDof()

   P_=therMAILx_GetBodyVector('T____',1)
   print('P',P_.shape)
   #print P_

   therMAILx_ComputeThermFields()

   gradP = therMAILx_GetGrad(1)
   print('gradP',gradP.shape)
   #print gradP

   ###
   print('UPDATE DOF')
   TimeEvolution_UpdateStep()
   RBDY2_UpdateDof()
   therMAILx_UpdateThermDof()
   therMAILx_UpdateThermBulk()

   #
   print('WRITE OUT DOF')
   TimeEvolution_WriteOutDof(freq_write)
   RBDY2_WriteOutDof()
   therMAILx_WriteOutDof()

   TimeEvolution_WriteOutGPV(freq_write)
   MAILx_WriteOutGPV()

   #
   print('WRITE OUT Vloc Rloc')
   TimeEvolution_WriteOutVlocRloc(freq_write)
   DKDKx_WriteOutVlocRloc()
   DKJCx_WriteOutVlocRloc()
   #
   ### viz ###

   if k % freq_display == 0:

     # empty lists
     display_gradPx=[]
     display_gradPy=[]
     display_Cp=[]
     display_Cd=[]
     display_Vsx=[]
     display_Vsy=[]
     display_Fextx=[]
     display_Fexty=[]
     
     kk+=1 
     #
     display_gradPx.append(gradP[:,0])
     display_gradPy.append(gradP[:,1])
     #
     display_Cd.append(Cd)
     #
     display_Cp.append(Cp)
     # 
     lvs=len(Vs)
     #
     Vs.shape=(lvs/2,2)
     display_Vsx.append(Vs[:,0])    
     display_Vsy.append(Vs[:,1])   
     Vs.shape=(lvs)
     #
     #Fext.shape=(lvs/2,2)
     #display_Fextx[1]=Fext[:,0]    
     #display_Fexty[1]=Fext[:,1]   
     #Fext.shape=(lvs)
     # 
     WriteDisplayFiles(freq=1,
                       gradPx=('therFE','node',display_gradPx),
                       gradPy=('therFE','node',display_gradPy),
                       Cd=('therFE','node',display_Cd),
                       Cp=('therFE','node',display_Cp),
                       Vsx=('therFE','node',display_Vsx),
                       Vsy=('therFE','node',display_Vsy),
                       #Fextx=('therFE','node',display_Fextx),
                       #Fexty=('therFE','node',display_Fexty),
                       )
     

     utilities_logMes('---')
 
   ### postpro ###
   WritePostproFiles()
   ### writeout handling ###
   overall_CleanWriteOutFlags()

ClosePostproFiles()
CloseDisplayFiles()

