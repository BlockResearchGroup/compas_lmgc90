
# importing chipy module
# from pylmgc90.chipy import *
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

#chipy.utilities_DisableLogMes()

# ------------------------------------------------------

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0
dim  = 3

# Set space dimension
chipy.SetDimension(dim,mhyp)

# theta integrator parameter
theta = 0.5

# Defined the time discretisation and integrator

t_final = 0.5

# time evolution parameters
dt     = 0.01

chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)

# Initialize theta integrator for all physical model
chipy.Integrator_InitTheta(theta)

# write parameter
freq_write   = 5

# display parameters
freq_display = 5

# interaction parameters
Rloc_tol    = 5.e-2

# nlgs parameters
tol    = 1e-5
relax  = 1.0
norm   = 'Quad '
gs_it1 = 100
gs_it2 = 1000
stype  = 'Stored_Delassus_Loops         '

chipy.nlgs_3D_DiagonalResolution()

chipy.POLYR_SkipAutomaticReorientation()
chipy.POLYR_FlatnessAngle(1.)
chipy.POLYR_TopologyAngle(20.)

chipy.PRPRx_LowSizeArrayPolyr(1000)
chipy.PRPRx_UseNcDetection(0.02)

#
chipy.ReadDatbox(deformable=False)

#
# open display & postpro
#
chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()

# since constant compute elementary mass matrices once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()


while chipy.TimeEvolution_GetTime() < t_final :

    chipy.utilities_logMes('INCREMENT STEP')
    chipy.IncrementStep()
    #
    chipy.utilities_logMes('COMPUTE Fext')
    chipy.ComputeFext()
    #
    chipy.utilities_logMes('COMPUTE Bulk')
    chipy.ComputeBulk()
    #
    chipy.utilities_logMes('COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()
    #
    chipy.utilities_logMes('SELECT PROX TACTORS')
    chipy.SelectProxTactors()

    chipy.utilities_logMes('RESOLUTION' )
    chipy.RecupRloc(Rloc_tol)

    chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
    chipy.UpdateTactBehav()

    chipy.StockRloc()

    chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
    chipy.ComputeDof()

    chipy.utilities_logMes('UPDATE DOF, FIELDS')
    chipy.UpdateStep()
    
    chipy.utilities_logMes('WRITE OUT')
    chipy.WriteOut(freq_write)

    chipy.utilities_logMes('VISU & POSTPRO')
    chipy.WriteDisplayFiles(freq_display)  


# close display & postpro
#
chipy.CloseDisplayFiles()

chipy.Finalize()
