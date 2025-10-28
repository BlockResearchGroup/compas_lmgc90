### lets read some existing objects

import os,sys
from pylmgc90.chipy import *
from numpy import *
import pickle

checkDirectories()

dt = 2e-4
theta = 0.5

POLYR_TopologyAngle(0.5)

SetDimension(3)

utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()
bulk_behav_SetGravity([0,0,0])

### model reading ###
utilities_logMes('READ BODIES')
RBDY3_ReadBodies()

utilities_logMes('LOADS BEHAVIOURS')
RBDY3_LoadBehaviours()

utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
RBDY3_ReadIniDof()

utilities_logMes('LOAD TACTORS')
POLYR_LoadTactors()

utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
PRPLx_ReadIniVlocRloc()
PRPRx_ReadIniVlocRloc()

nbr = POLYR_GetNbPOLYR()

for i in range(1,nbr+1):

  utilities_logMes('-------------')
  utilities_logMes('objet '+str(i))

  coor,connectivity = POLYR_GetWireframe(i,1.)
  bloc={}
  bloc['coor']=coor
  bloc['connectivity']=connectivity  

  f = open('bloc'+str(i)+'.dict', 'w')
  pickle.dump(bloc,f)
  f.close()



