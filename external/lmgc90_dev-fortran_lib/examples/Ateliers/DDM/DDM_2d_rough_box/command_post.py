from __future__ import print_function

import os,sys,shutil

from pylmgc90.chipy import *
from numpy import *

# plage de fichiers a traiter
min=1
max=21

# nom du repertoire ou on va faire le post
tmp='post'

tmp=os.getcwd()+'/'+tmp+'/'

if os.path.isdir(tmp):
  print("Le repertoire ",tmp," existe deja, ca n'est pas la peine de le creer")
  # on teste la presence de DATBOX
  if os.path.isdir(tmp+'DATBOX'):
    print(" on ne copie pas DATBOX")
  else:
    shutil.copytree('./DATBOX',tmp+'DATBOX')
else:
  os.mkdir(tmp)
  shutil.copytree('./DATBOX',tmp+'DATBOX')

overall_SetWorkingDirectory(tmp)

print("C'est partie")

####

checkDirectories()

dt = 5.e-3

freq_display = 1
freq_write = 1

######## etat 0 ###########################

### computation's parameters definition ### 
overall_DIME(2,1)

utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
### model reading ###
print('READ BODIES')
RBDY2_ReadBodies()

print('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

#LOADS
DISKx_LoadTactors()
JONCx_LoadTactors()
RBDY2_LoadBehaviours()

print('READ INI DOF')
TimeEvolution_ReadIniDof()
RBDY2_ReadIniDof()

print('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
DKJCx_ReadIniVlocRloc()
DKDKx_ReadIniVlocRloc()

names= ['DOF','Vloc_Rloc']

tacts_dict={}
InitTactorsToVTK(['DISKx','JONCx'],tacts_dict)

inters_dict={}
InitIntersToVTK(['DKDKx','DKJCx'],inters_dict)

fit = startCollection('tacts.pvd')
fii = startCollection('inters.pvd')

for k in range(min,max+1,1):
    #
    utilities_logMes('on traite le set : '+str(k))
    #
    for name in names:    
      shutil.copy('./OUTBOX/'+name+'.OUT.'+str(k),tmp+'DATBOX/'+name+'.INI')

    utilities_logMes('READ INI DOF')
    TimeEvolution_ReadIniDof()
    RBDY2_ReadIniDof()

    utilities_logMes('READ INI Vloc Rloc')
    TimeEvolution_ReadIniVlocRloc()
    DKJCx_ReadIniVlocRloc()
    DKDKx_ReadIniVlocRloc()

    utilities_logMes('tact')
    writeTactorsToVTK('./DISPLAY/tacts'+'_'+str(k)+'.vtp',fit,tacts_dict)
    utilities_logMes('inter')
    writeIntersToVTK('./DISPLAY/inters'+'_'+str(k)+'.vtp',fii,inters_dict,1e+0)
    utilities_logMes('---')
  
stopCollection(fit)
stopCollection(fii)


