import sys

from pylmgc90.pre import *
from pylmgc90.chipy import *
from numpy import *

dim=2

# definition du conteneur de partie ou de pieces, des modeles et des materiaux
ps    = avatars();ms=models();mx=materials();svs=see_tables();tacts=tact_behavs()

# < modeles ...
#   ... definition 
mo = model(name='M2DNL',physics='MECAx',element='Q4xxx',dimension=2,external_model='MatL_',
           kinematic='small',material='elas_',anisotropy='iso__',mass_storage='coher')
#   ... stockage
ms.addModel(mo)
# ... modeles >

# < materiaux ...
#   ... definition
ma1 = material(name='steel',materialType='ELAS',elas='standard',
                 young=0.1e+15,nu=0.2,anisotropy='isotropic',
                 density=0.25e+4)
ma2 = material(name='graph',materialType='ELAS',elas='standard',
                 young=0.6e+12,nu=0.2,anisotropy='isotropic',
                 density=0.25e+4)
#   ... stockage
mx.addMaterial(ma1,ma2)
# ... materiaux >

# definition des parties maillees
block_mesh = readMesh('block.msh',dim)
block=buildMeshedAvatar(mesh=block_mesh,model=mo, material=ma1)

block.rotate(psi=0.01)

ps.addAvatar(block)

ground_mesh = readMesh('groun.msh',dim)
ground=buildMeshedAvatar(mesh=ground_mesh,model=mo, material=ma2)

ps.addAvatar(ground)

# Application des conditions initiales
block.imposeInitValue(group='all',component=[1,2],value=[0.,0.])
ground.imposeInitValue(group='all',component=[1,2],value=[0.,0.])

# Application des conditions aux limites
# base
ground.imposeDrivenDof(group='10015',component=1,dofty='vlocy')
ground.imposeDrivenDof(group='10015',component=2,dofty='vlocy')

# < Contacteurs ...
#   ... definition et affectation
ground.addContactors(group='10014',shape='ALpxx',color='xxxxx',reverse='yes')
block.addContactors(group='10119',shape='CLxxx',color='xxxxx',reverse='yes')
block.addContactors(group='10219',shape='CLxxx',color='xxxxx',reverse='yes')
# ... Contacteurs >

# Definition des interactions et des tables de visibilites
#.. table de visibilite
sv = see_table(CorpsCandidat='MAILx',candidat='CLxxx',colorCandidat='xxxxx',
               behav='gapc1',
               CorpsAntagoniste='MAILx',antagoniste='ALpxx',colorAntagoniste='xxxxx',alert=0.01)

svs+=sv

#...interaction
b=tact_behav('gapc1','GAP_SGR_CLB',fric=0.9)
tacts+=b

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

post = postpro_commands()
my_command=postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(my_command)
writePostpro(post, ps, path='DATBOX/')

# Ecriture des fichiers pour LMGC
writeBodies(ps,chemin='DATBOX/')
writeModels(ms,chemin='DATBOX/')
writeDrvDof(ps,chemin='DATBOX/')
writeDofIni(ps,chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')
writeGPVIni(ps,chemin='DATBOX/')
writeBulkBehav(mx,chemin='DATBOX/',dim=dim)
writeTactBehav(tacts,svs,chemin='DATBOX/')

########## LANCEMENT DE LMGC90 ###################################

####
checkDirectories()

timer_InitializeTimers()
overall_Initialize()

#utilities_DisableLogMes()

idr = timer_GetNewTimer('load')
idd = timer_GetNewTimer('display')
idw = timer_GetNewTimer('write')

timer_StartTimer(idr)

####
# info gestion du temps
dt = 1.e-3
theta = 0.51
nb_steps = 200

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 10
ref_size=0.02

freq_write = 10

# info contact

#       123456789012345678901234567890
type = 'Stored_Delassus_Loops         '
quad = 'QM/16'
tol = 0.1666e-3
relax = 1.0
gs_it1 = 51
gs_it2 = 501

###
SetDimension(dim=dim,mod=1)

utilities_logMes('INIT TIME STEPPING')

TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### lecture du modele ###
utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

utilities_logMes('READ MODELS')
models_ReadModels()

utilities_logMes('READ BODIES')
MAILx_ReadBodies()

# on dimensionne et on initie la construction des mapping
utilities_logMes('INIT MODELS')
models_InitModels()
ExternalModels_InitModels()

# on charge les choses et on construit les mapping
utilities_logMes('LOADS')
mecaMAILx_LoadModels()
mecaMAILx_LoadBehaviours()

utilities_logMes('PUSH')
mecaMAILx_PushProperties()
#
# on finalise la construction des mapping
utilities_logMes('STORE')
models_StoreProperties()

utilities_logMes('CHECK')
ExternalModels_StoreProperties()
mecaMAILx_CheckProperties()
#
utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
mecaMAILx_ReadIniDof()

utilities_logMes('READ INI GPV')
TimeEvolution_ReadIniGPV()
mecaMAILx_ReadIniGPV()
#
utilities_logMes('READ DRIVEN DOF')
mecaMAILx_ReadDrivenDof()

utilities_logMes('LOADS TACTORS')
CLxxx_LoadTactors()
ALpxx_LoadTactors()
#
utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
CLALp_ReadIniVlocRloc()

### ecriture paranoiaque du modele ###
utilities_logMes('WRITE BODIES')
overall_WriteBodies()
MAILx_WriteBodies()

utilities_logMes('WRITE MODELS')
models_WriteModels()

utilities_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

utilities_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
mecaMAILx_WriteDrivenDof()

#utilities_logMes('WRITE OUT DOF')
#TimeEvolution_WriteOutDof(1)
#mecaMAILx_WriteOutDof()

utilities_logMes('WRITE LAST DOF')
TimeEvolution_WriteLastDof()
mecaMAILx_WriteLastDof()

#utilities_logMes('WRITE OUT Vloc_Rloc')
#TimeEvolution_WriteOutVlocRloc(1)
#CLALp_WriteOutVlocRloc()

#utilities_logMes('WRITE LAST Vloc_Rloc')
#TimeEvolution_WriteLastVlocRloc()
#CLALp_WriteLastVlocRloc()

### definition des parametres du calcul ### 

timer_StopTimer(idr)

timer_StartTimer(idd)
utilities_logMes('INIT VISU ET POSTPRO')
OpenDisplayFiles()
OpenPostproFiles()
timer_StopTimer(idd)

### parameters setting ###

utilities_logMes('COMPUTE MASS')
mecaMAILx_ComputeMass()

utilities_logMes('COMPUTE STIFFNESS')
mecaMAILx_ComputeBulk()
mecaMAILx_AssembKT()

# precondensation
mecaMAILx_SetPreconAllBodies()
CLxxx_PushPreconNodes()
ALpxx_PushPreconNodes()
mecaMAILx_ComputePreconW()

for k in range(1,nb_steps+1,1):
   #
   utilities_logMes('INCREMENT STEP')
   TimeEvolution_IncrementStep()
   mecaMAILx_IncrementStep()
   
   utilities_logMes('DISPLAY TIMES')
   TimeEvolution_DisplayStep()

   utilities_logMes('COMPUTE Fext')
   mecaMAILx_ComputeFext()
   
   utilities_logMes('COMPUTE Fint')
   mecaMAILx_ComputeBulk()
   
   utilities_logMes('ASSEMBLAGE')
   mecaMAILx_AssembRHS()

   utilities_logMes('COMPUTE Free Vlocy')
   mecaMAILx_ComputeFreeVelocity()
   #
   utilities_logMes('SELECT PROX TACTORS')
   overall_SelectProxTactors()
   CLALp_SelectProxTactors()
   
   #
   CLALp_RecupRloc()
   
   utilities_logMes('RESOLUTION') 
    
   nlgs_ExSolver(type,quad, tol, relax, gs_it1, gs_it2)
    
   nlgs_UpdateTactBehav()

   CLALp_StockRloc()
   #
   utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   mecaMAILx_ComputeDof()
   mecaMAILx_ComputeField()
   #
   utilities_logMes('UPDATE DOF, FIELDS')
   TimeEvolution_UpdateStep()
   mecaMAILx_UpdateDof()
   mecaMAILx_UpdateBulk()
   #
   timer_StartTimer(idw)
   utilities_logMes('WRITE out DOF')
   TimeEvolution_WriteOutDof(freq_write)
   mecaMAILx_WriteOutDof()
   #
   utilities_logMes('WRITE out GPV')
   TimeEvolution_WriteOutGPV(freq_write)
   MAILx_WriteOutGPV()
   #
   utilities_logMes('WRITE out Rloc')
   TimeEvolution_WriteOutVlocRloc(freq_write)
   CLALp_WriteOutVlocRloc()

   timer_StopTimer(idw)

   timer_StartTimer(idd)

   WriteDisplayFiles(freq=freq_display)
   WritePostproFiles()

   timer_StopTimer(idd)


   ### gestion des writeout ###
   overall_CleanWriteOutFlags()

### visu & postpro ###
timer_StartTimer(idd)

CloseDisplayFiles()
ClosePostproFiles()

timer_StopTimer(idd)

overall_Finalize()
timer_WriteOutTimers()

