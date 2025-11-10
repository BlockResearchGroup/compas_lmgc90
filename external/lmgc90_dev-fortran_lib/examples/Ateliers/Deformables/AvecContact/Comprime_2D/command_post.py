
from pylmgc90.chipy import *
from numpy import *

# plage de fichiers a traiter
min=1
max=40

names= ['DOF','GPV','Vloc_Rloc']

inters_dict={}
InitIntersToVTK(['CLALp'],inters_dict)


####

checkDirectories()

# ne sert pas a grand chose
dt = 1.

######## etat 0 ###########################

### computation's parameters definition ### 
overall_DIME(2,1)

utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)

utilites_logMes('READ BODIES')
MAILx_ReadBodies()

utilites_logMes('READ MODELS')
models_ReadModels()

utilites_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()
### models initialization ###
utilites_logMes('INIT MODELS')
models_InitModels()
ExternalModels_InitModels()

#LOADS
mecaMAILx_LoadModels()

mecaMAILx_LoadBehaviours()

mecaMAILx_PushProperties()
models_StoreProperties()
ExternalModels_CheckProperties()

CLxxx_LoadTactors()
ALpxx_LoadTactors()

### initial and boundary conditions ###
utilites_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
mecaMAILx_ReadIniDof()

TimeEvolution_ReadIniGPV()
mecaMAILx_ReadIniGPV()

TimeEvolution_ReadIniVlocRloc()
CLALp_ReadIniVlocRloc()

fim = startCollection('DISPLAY/mecafe.pvd')
fii = startCollection('DISPLAY/inters.pvd')

k=0
utilities_logMes('tact')
writeMecafeToVTK('DISPLAY/mecafe'+'_'+str(k)+'.vtu',fim,2)

for k in range(min,max+1,1):
    #
    utilities_logMes('on traite le set : '+str(k))
    #

    utilities_logMes('READ INI DOF')
    TimeEvolution_ReadIniDof(k)
    mecaMAILx_ReadIniDof(k)

    TimeEvolution_ReadIniGPV(k)
    mecaMAILx_ReadIniGPV(k)

    utilities_logMes('tact')
    writeMecafeToVTK('DISPLAY/mecafe'+'_'+str(k)+'.vtu',fim,2)

    TimeEvolution_ReadIniVlocRloc(k)
    CLALp_ReadIniVlocRloc(k)

    utilities_logMes('inter')
    writeIntersToVTK('DISPLAY/inters'+'_'+str(k)+'.vtp',fii,inters_dict,0.3e-1)

stopCollection(fim)
stopCollection(fii)


