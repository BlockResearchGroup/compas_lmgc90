
from pylmgc90.chipy import *
from numpy import *

# plage de fichiers a traiter
min=1
max=30

####

checkDirectories()

dt = 2.e-4

freq_display = 1
freq_write = 1

######## etat 0 ###########################

### computation's parameters definition ### 
overall_DIME(2,1)

utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
### model reading ###
utilites_logMes('READ BODIES')
RBDY2_ReadBodies()

utilites_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

#LOADS
DISKx_LoadTactors()
JONCx_LoadTactors()
RBDY2_LoadBehaviours()

utilites_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
RBDY2_ReadIniDof()

utilites_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
DKJCx_ReadIniVlocRloc()
DKDKx_ReadIniVlocRloc()

names= ['DOF','Vloc_Rloc']

tacts_dict={}
InitTactorsToVTK(['DISKx','JONCx'],tacts_dict)

inters_dict={}
InitIntersToVTK(['DKDKx','DKJCx'],inters_dict)

fit = startCollection('DISPLAY/tacts.pvd')
fii = startCollection('DISPLAY/inters.pvd')

for k in range(min,max+1,1):
    #
    utilities_logMes('on traite le set : '+str(k))
    #
    utilities_logMes('READ INI DOF')
    TimeEvolution_ReadIniDof(k)
    RBDY2_ReadIniDof(k)

    utilities_logMes('READ INI Vloc Rloc')
    TimeEvolution_ReadIniVlocRloc(k)
    DKJCx_ReadIniVlocRloc(k)
    DKDKx_ReadIniVlocRloc(k)

    utilities_logMes('tact')
    writeTactorsToVTK('DISPLAY/tacts'+'_'+str(k)+'.vtp',fit,tacts_dict)
    utilities_logMes('inter')
    writeIntersToVTK('DISPLAY/inters'+'_'+str(k)+'.vtp',fii,inters_dict,0.1)
    utilities_logMes('---')
  
stopCollection(fit)
stopCollection(fii)


