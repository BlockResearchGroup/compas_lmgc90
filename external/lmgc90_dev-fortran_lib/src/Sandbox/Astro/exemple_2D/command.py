import os,sys
#sys.path.append('/Users/dubois/WORK/CODING/v2_g95/LMGC90v2_dev_opt/ChiPy')
root = '../../../'
sys.path.append(root+'ChiPy/lib')
from chipy import *

sys.path.append(root+'Sandbox/Astro/lib')
from astro import *
import numpy

### lecture du modele ###
### model reading ###
print 'READ BODIES'
RBDY2_ReadBodies()

print 'READ BEHAVIOURS'
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

#LOADS
DISKx_LoadTactors()
POLYG_LoadTactors()
RBDY2_LoadBehaviours()

print 'READ INI DOF'
overall_ReadIniDof()
RBDY2_ReadIniDof()

print 'READ INI Vloc Rloc'
overall_ReadIniVlocRloc()
DKDKx_ReadIniVlocRloc()
PLPLx_ReadIniVlocRloc()

print 'READ DRIVEN DOF'
RBDY2_ReadDrivenDof()

### ecriture paranoiaque du modele ###
print 'WRITE BODIES'
overall_WriteBodies()
RBDY2_WriteBodies()

print 'WRITE BEHAVIOURS'
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

print 'WRITE DRIVEN DOF'
overall_WriteDrivenDof()
RBDY2_WriteDrivenDof()

### definition des parametres du calcul ### 
print 'INIT TIME STEPPING'
nb_iter = 1000
dt = 0.005
theta = 0.5
overall_SetTimeStep(dt)
overall_InitThetaIntegrator(theta)

### post2D ##
post2D_SetReferenceRadius(0.1)
post2D_SetDisplayedField('TACTOR')
post2D_SetDisplayedField('CONTACT POINT')
post2D_InitGMV()

### postpro ###
postpro_PostproBeforeComputation()

### preparation de l'algo de detection par les boites ###
print 'COMPUTE BOX'
DKDKx_ComputeBox()
PLPLx_ComputeBox()
afterall_ComputeBox()

#RBDY2_SetLowerBoundary(-1.5)

print 'COMPUTE MASS'
RBDY2_ComputeMass()
#### get a vector with mass of each body... to improve ###
nb_part = RBDY2_GetNbRBDY2()
mass = numpy.zeros(nb_part)
for i in range(nb_part):
    mass[i] = RBDY2_GetBodyMass(i+1)
pos = numpy.zeros((3,nb_part),order='F')
#fext= numpy.zeros((3,nb_part))

freq_detect = 1
tol = 0.1666e-3
relax = 1.0
quad = 'Quad '
gs_it1 = 51
gs_it2 = 1001
freq_gmv = 10

for k in xrange(nb_iter):
   #
   print 'itere : ',k
   #
   #print 'INCREMENT STEP'
   overall_IncrementStep()
   RBDY2_IncrementStep()

   #print 'DISPLAY TIMES'
   overall_DisplayTimes()

   #print 'COMPUTE Fext'
   RBDY2_ComputeFext()
   print 'compute gravitation force'
   for i in range(nb_part):
        pos[:,i] = RBDY2_GetBodyVector('Coorm',i+1,3)
   #pos2 = RBDY2_GetAllVector('Coorm',3*nb_part,3,nb_part)
   #pos2.shape = (nb_part,3)
   #pos2 = pos2.transpose()

   fext = self_gravity_ComputeFext2D(pos,mass,theta,dt,3*nb_part)
   fext.shape = (nb_part,3)
   for i in range(nb_part):
        RBDY2_PutBodyVector('Fext_',i+1,fext[i,:])

   #print 'COMPUTE Fint'
   RBDY2_ComputeBulk()
   
   #print 'COMPUTE Free Vlocy'
   RBDY2_ComputeFreeVelocity()
   #
   #print 'SELECT PROX TACTORS'
   overall_SelectProxTactors(freq_detect)
   DKDKx_SelectProxTactors()
   PLPLx_SelectProxTactors()
   #
   DKDKx_RecupRloc()
   PLPLx_RecupRloc()
   nlgs_ExSolver('Stored_Delassus_Loops         ',quad, tol, relax, gs_it1, gs_it2)
   DKDKx_StockRloc()
   PLPLx_StockRloc()
   #
   #print 'COMPUTE DOF'
   RBDY2_ComputeDof()
   #
   #print 'UPDATE DOF'
   overall_UpdateDof()
   RBDY2_UpdateDof()
   #
   #print 'WRITE LAST DOF'
   overall_WriteLastDof()
   RBDY2_WriteLastDof()
   #
   #print 'WRITE LAST Vloc Rloc'
   overall_WriteLastVlocRloc()
   DKDKx_WriteLastVlocRloc()
   #
   ### post2D ###
   overall_WriteOutGMV(freq_gmv)
   post2D_WriteOutGMV(0)
   ### postpro ###
   postpro_PostproDuringComputation()
   # devrait y avoir des write_xxx_Vloc_Rloc truc ici
   ### wrtieout handling ###
   overall_CleanWriteOutFlags()
### postpro ###
postpro_ClosePostproFiles()
