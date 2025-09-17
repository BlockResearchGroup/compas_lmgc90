
from pylmgc90.chipy import *
from numpy import *

checkDirectories()

# desactivation des messages de log
#utilities_DisableLogMes()

# a 3D example is considered
overall_DIME(3,0)

### computation's parameters definition ### 
utilities_logMes('INIT TIME STEPPING')
# time step length
dt = 1.e-2
# value of the parameter of the theta-method
theta = 0.5
# number of time steps
nb_steps = 200

# bavardage de certaines fonctions
echo = 0

### parameters setting ###
#   * visualization frequency
freq_display = 50
#   * frequence d'ecriture des fichier de sortie
freq_write = 50
#       123456789012345678901234567890
#   * nlgs solver parameters
type = 'Stored_Delassus_Loops         '
norm = 'QM/16'
tol = 0.1666e-3
relax = 1.0
gs_it1 = 1
gs_it2 = 5001

# 
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)
#

### model reading ###
utilities_logMes('READ BODIES')
RBDY3_ReadBodies()
#
PLANx_LoadTactors()

utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
RBDY3_ReadIniDof()

TimeEvolution_WriteLastDof()
RBDY3_WriteLastDof()

SPHER_LoadTactors()
#
overall_WriteBodies()
RBDY3_WriteBodies()
#
utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

#LOADS
RBDY3_LoadBehaviours()

bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
SPSPx_ReadIniVlocRloc()
SPPLx_ReadIniVlocRloc()

utilities_logMes('READ DRIVEN DOF')
RBDY3_ReadDrivenDof()

overall_WriteDrivenDof()
RBDY3_WriteDrivenDof()

### compute masses ###
RBDY3_ComputeMass()

### post3D ##
#                         1234567890123456
# definition of fields to be computed by the post3D module
post3D_SetDisplayedField('POSITION')
post3D_SetDisplayedField('AVERAGE VELOCITY')
post3D_SetDisplayedField('STRESS')
# initialization of the post3D module
post3D_Init()
postpro_3D_PostproBeforeComputation()

# definition of another fields to be displayed by the display_3D module
display_3D_SetDisplayedField('TACTOR')
display_3D_SetDisplayedField('INTERACTION')
# choose of the file format for the visualiztion
display_3D_SetDisplayFileFormat('VTK')
# initilization of the display_3D module
display_3D_Init(0)

# compute of a first visualization
#display_3D_WriteOutDisplayFile(0)

# time loop
for k in range(1, nb_steps + 1):
    #
    utilities_logMes('itere : '+str(k))
    #
    utilities_logMes('INCREMENT STEP')
    TimeEvolution_IncrementStep()
    RBDY3_IncrementStep()
    #
    utilities_logMes('COMPUTE Fext')
    RBDY3_ComputeFext()
    #
    utilities_logMes('COMPUTE Fint')
    RBDY3_ComputeBulk()
    # 
    utilities_logMes('COMPUTE Free Vlocy')
    RBDY3_ComputeFreeVelocity()
    #
    utilities_logMes('SELECT PROX TACTORS')
    overall_SelectProxTactors()

    SPSPx_SelectProxTactors()
    SPPLx_SelectProxTactors()
    #
    SPSPx_RecupRloc()
    SPPLx_RecupRloc()

    utilities_logMes('RESOLUTION' )

    #utilities_logMes(str(type)+', '+str(norm)+', '+str(tol)+', '+str(relax)+', '+str(gs_it1)+', '+str(gs_it2))
    nlgs_3D_ExSolver(type, norm, tol, relax, gs_it1, gs_it2)

    SPSPx_StockRloc()
    SPPLx_StockRloc()
    #
    utilities_logMes('COMPUTE DOF')
    RBDY3_ComputeDof()
    #
    utilities_logMes('UPDATE DOF')
    TimeEvolution_UpdateStep()
    RBDY3_UpdateDof()

    ### post3D ###
    post3D_Update()

    overall_WriteOutDisplayFile(freq_display)
    display_3D_WriteOutDisplayFile(0)

    TimeEvolution_WriteOutDof(freq_write)
    RBDY3_WriteOutDof(-1,9999999)

    ### postpro ###
    postpro_3D_PostproDuringComputation()

    TimeEvolution_WriteOutVlocRloc(freq_write)
    SPSPx_WriteOutVlocRloc()
    SPPLx_WriteOutVlocRloc()

    ### writeout handling ###
    overall_CleanWriteOutFlags()

TimeEvolution_WriteLastDof()
RBDY3_WriteLastDof()
