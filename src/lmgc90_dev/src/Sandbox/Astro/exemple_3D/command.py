
import os,sys
root = '/Users/remymozul/work/lmgc90/LMGC90v2_dev/'
sys.path.append(root+'ChiPy_BindC/lib')

from chipy import *
import numpy

sys.path.append(root+'Sandbox/Astro/lib')
from astro import *


overall_DIME(3,0)

### computation's parameters definition ### 
print 'INIT TIME STEPPING'
nb_iter = 500
dt = 0.005
theta = 0.5

#
overall_SetTimeStep(dt)
overall_InitThetaIntegrator(theta)
#

### model reading ###
print 'READ BODIES'
RBDY3_ReadBodies()
#
SPHER_LoadTactors()
#
overall_WriteBodies()
RBDY3_WriteBodies()
#MAILx_WriteBodies()
#
print 'READ BEHAVIOURS'
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

#LOADS
RBDY3_ReadBehaviours()

bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

print 'READ INI DOF'
overall_ReadIniDof()
RBDY3_ReadIniDof()

overall_WriteLastDof()
RBDY3_WriteLastDof()

print 'READ INI Vloc Rloc'
overall_ReadIniVlocRloc()
SPSPx_ReadIniVlocRloc()

print 'READ DRIVEN DOF'
RBDY3_ReadDrivenDof()

overall_WriteDrivenDof()
RBDY3_WriteDrivenDof()

### compute masses ###
RBDY3_ComputeMass()
nb_part = RBDY3_GetNbRBDY3()
mass = numpy.zeros(nb_part)
for i in range(nb_part):
    mass[i] = RBDY3_GetMass(i+1)
pos = numpy.zeros((6,nb_part),order='F')

### preparation of detection algorithm using boxes method ###
print 'COMPUTE BOX'
SPSPx_ComputeBox()
afterall_ComputeBox()
 
### post3D ##
#                         1234567890123456
post3D_SetDisplayedField('POSITION        ')
post3D_SetDisplayedField('AVERAGE VELOCITY')
display_3D_SetDisplayedField('TACTOR')
post3D_Init()
display_3D_Init()
display_3D_WriteOutGMV(0)
postpro_3D_PostproBeforeComputation()

### parameters setting ###
freq_detect = 1
freq_gmv = 5
#       123456789012345678901234567890
type = 'Exchange_Local_Global         '
quad = 'QM/16'
tol = 0.1666e-3
relax = 1.0
gs_it1 = 51
gs_it2 = 501

for k in range(nb_iter):
    #
    print 'itere : ',k
    #
    #print 'INCREMENT STEP'
    overall_IncrementStep()
    RBDY3_IncrementStep()
#    POLYR_IncrementStep()
    #
    #print 'COMPUTE Fext'
    RBDY3_ComputeFext()
    #print 'compute gravitation force'
    for i in range(nb_part):
         pos[:,i] = RBDY3_GetBodyVector('Coorm',i+1,6)

    fext = self_gravity_ComputeFext3D(pos,mass,theta,dt,6*nb_part)
    fext.shape = (nb_part,6)
    for i in range(nb_part):
         RBDY3_PutBodyVector('Fext_',i+1,fext[i,:])

    #
    #print 'COMPUTE Fint'
    RBDY3_ComputeBulk()
    # 
    #print 'COMPUTE Free Vlocy'
    RBDY3_ComputeFreeVelocity()
    #
    #print 'SELECT PROX TACTORS'
    overall_SelectProxTactors(freq_detect)
    SPSPx_SelectProxTactors()
    #
    SPSPx_RecupRloc()

    #print 'RESOLUTION' 
    #print type,quad,tol, relax, gs_it1, gs_it2 
    nlgs_3D_ExSolver(type,quad, tol, relax, gs_it1, gs_it2)

    SPSPx_StockRloc()
    #
    #print 'COMPUTE DOF'
    RBDY3_ComputeDof()
    #
    #print 'UPDATE DOF'
    overall_UpdateDof()
    RBDY3_UpdateDof()

    ### post3D ###
    post3D_Update()

    overall_WriteOutGMV(freq_gmv)
    display_3D_WriteOutGMV(0)

    ### postpro ###
    postpro_3D_PostproDuringComputation()

    overall_WriteOutDof(500)
    RBDY3_WriteOutDof(-1,9999999)

    overall_WriteOutVlocRloc(500)
    SPSPx_WriteOutVlocRloc()

    ### writeout handling ###
    overall_CleanWriteOutFlags()

overall_WriteLastDof()
RBDY3_WriteLastDof()

postpro_3D_ClosePostproFiles()
