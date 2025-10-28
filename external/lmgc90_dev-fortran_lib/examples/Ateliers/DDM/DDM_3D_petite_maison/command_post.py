from __future__ import print_function
import os,shutil

from pylmgc90.chipy import *
from numpy import *

# plage de fichiers a traiter
min=1
max=10

# nom du repertoire ou on va faire le post
tmp='tmp'

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

dt = 1.e-4

POLYR_SkipAutomaticReorientation()

PRPRx_ShrinkPolyrFaces(5e-2)
#PRPRx_UseCpF2fExplicitDetection(1e-2)
PRPRx_UseNcF2fExplicitDetection(20.,1e-2)
PRPRx_LowSizeArrayPolyr(10)
PRPRx_SetReactionTrackingLength(0.4)

freq_display = 1
freq_write = 1

######## etat 0 ###########################

### computation's parameters definition ### 
overall_DIME(3,0)

utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)

### model reading ###
utilities_logMes('READ BODIES')
RBDY3_ReadBodies()

utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
RBDY3_ReadIniDof()

PLANx_LoadTactors()
POLYR_LoadTactors()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

RBDY3_LoadBehaviours()

utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
PRPRx_ReadIniVlocRloc()
PRPLx_ReadIniVlocRloc()

utilities_logMes('INITIALISATION VISU')
### post3D ##
post3D_SetDisplayedField('POSITION')
post3D_SetDisplayedField('AVERAGE VELOCITY')
post3D_SetReferenceRadius(0.01)
#post3D_SetDisplayedField('STRESS')
#post3D_SetDisplayedField('FORCES')
display_3D_SetDisplayedField('TACTOR')
display_3D_SetDisplayedField('INTERACTION')

post3D_Init()
display_3D_Init(0)

utilities_logMes('DETECTION POUR RECONSTRUIRE VIS A VIS')
overall_SelectProxTactors()
PRPRx_SelectProxTactors()

PRPRx_RecupRloc()

names= ['DOF','Vloc_Rloc']

for k in range(min,max+1,1):
    #
    utilities_logMes('on traite le set : '+str(k))
    #
    for name in names:    
      shutil.copy('./OUTBOX/'+name+'.OUT.'+str(k),tmp+'DATBOX/'+name+'.INI')

    utilities_logMes('READ INI DOF')
    TimeEvolution_ReadIniDof()
    RBDY3_ReadIniDof()

    utilities_logMes('READ INI Vloc Rloc')
    TimeEvolution_ReadIniVlocRloc()
    PRPRx_ReadIniVlocRloc()
    PRPLx_ReadIniVlocRloc()
    #
    PRPRx_RecupRloc()  
    #
    ### post3D ###
    post3D_Update()
    overall_WriteOutDisplayFile(1)
    display_3D_WriteOutDisplayFile(0)

    PRPRx_VisavisVTKDrawAll()

    ### writeout handling ###
    overall_CleanWriteOutFlags()

