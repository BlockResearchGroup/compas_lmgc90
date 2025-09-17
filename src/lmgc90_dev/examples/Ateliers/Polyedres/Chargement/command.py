# importing chipy module
from pylmgc90.chipy import *
import numpy as N
import pylab as P


# Initializing
Initialize()

# checking/creating mandatory subfolders
checkDirectories()

#utilities_DisableLogMes()

# ------------------------------------------------------

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1
dim= 3

# Set space dimension
SetDimension(dim,mhyp)

POLYR_SkipAutomaticReorientation()
#POLYR_FlatnessAngle(5.)
#POLYR_TopologyAngle(80.)

PRPRx_UseNcDetection(0.01)
PRPRx_LowSizeArrayPolyr(1000)


# theta integrator parameter
theta = 0.5

# time evolution parameters
dt     = 1e-2

# Defined the time discretisation and integrator

t_final = 20*dt


utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)

# Initialize theta integrator for all physical model
Integrator_InitTheta(theta)

# write parameter
freq_write   = 10

# display parameters
freq_display = 1
ref_radius   = 2.5e-2

# interaction parameters
Rloc_tol    = 5.e-2

# nlgs parameters
tol    = 1e-4
relax  = 1.0
norm   = 'Quad '
gs_it1 = 50
gs_it2 = 10
type   = 'Stored_Delassus_Loops         '

nlgs_3D_DiagonalResolution()

#
utilities_logMes('READ BEHAVIOURS')
ReadBehaviours()
ReadModels()
#
utilities_logMes('READ BODIES')
ReadBodies()
#
utilities_logMes('LOAD BEHAVIOURS')
LoadBehaviours()
LoadModels()
#
utilities_logMes('READ INI DOF')
ReadIniDof()
#
utilities_logMes('READ INI GPV')
ReadIniGPV()
#
utilities_logMes('READ DRIVEN DOF')
ReadDrivenDof()
#
utilities_logMes('LOAD TACTORS')
LoadTactors()
#
utilities_logMes('READ INI Vloc Rloc')
ReadIniVlocRloc()

#
# open display & postpro
#
utilities_logMes('DISPLAY & WRITE')
OpenDisplayFiles()
#OpenPostproFiles()

# since constant compute elementary mass matrices once
utilities_logMes('COMPUTE MASS')
ComputeMass()

WriteDisplayFiles(freq_display)
##################### PARTIE THERMIQUE ####################

while TimeEvolution_GetTime() < t_final :
    #
    utilities_logMes('INCREMENT STEP')
    IncrementStep()
    #    
    utilities_logMes('COMPUTE Fext')
    ComputeFext()
    #
    utilities_logMes('COMPUTE Bulk')
    ComputeBulk()
    #
    utilities_logMes('COMPUTE Free Vlocy')
    ComputeFreeVelocity()
    #
    utilities_logMes('SELECT PROX TACTORS')
    SelectProxTactors()
    #
    utilities_logMes('RESOLUTION' )
    RecupRloc()
    #
    ExSolver(type, norm, tol, relax, gs_it1, gs_it2)
    UpdateTactBehav()
    #
    StockRloc()
    #
    utilities_logMes('COMPUTE DOF, FIELDS, etc.')
    ComputeDof()
    #
    utilities_logMes('UPDATE DOF, FIELDS')
    UpdateStep()
    #
    utilities_logMes('WRITE OUT DOF')
    WriteOutDof(freq_write)
    #
    utilities_logMes('WRITE OUT Rloc')
    WriteOutVlocRloc(freq_write)
    #
    utilities_logMes('VISU & POSTPRO')
    WriteDisplayFiles(freq_display)
    #~ WritePostproFiles()    


# close display & postpro
#
CloseDisplayFiles()
#ClosePostproFiles()


# this is the end
#~ Finalize()
