# -*- coding: utf-8 -*-

# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
#~ chipy.utilities_DisableLogMes()

# defining some variables
#
# space dimension
dim = 2
# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1


# Defined the time discretisation and integrato
t_final = 0.5
# time evolution parameters
dt      = 0.01
# theta integrator parameter
theta   = 0.5

# write parameter
freq_write   = 10

# display parameters
freq_display = 1

# interaction parameters
freq_detect = 1000
Rloc_tol    = 5.e-2

# nlgs parameters
tol    = 1e-5
relax  = 1.
norm   = 'Quad '
gs_it1 = 1000
gs_it2 = 100
stype  = 'Stored_Delassus_Loops         '

chipy.PLPLx_ShrinkPolygFaces(0.6)

#
# read and load
#
# Set space dimension
chipy.SetDimension(dim,mhyp)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
# Initialize theta integrator for all physical model
chipy.Integrator_InitTheta(theta)
#
chipy.ReadDatbox(deformable=False)

#
# open display & postpro
#
chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()

### compute masses ###
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()


while chipy.TimeEvolution_GetTime() < t_final :
    #
    chipy.utilities_logMes('INCREMENT STEP')
    chipy.IncrementStep()
    #
    chipy.utilities_logMes('COMPUTE Fext')
    chipy.ComputeFext()
    #
    chipy.utilities_logMes('COMPUTE Fint')
    chipy.ComputeBulk()
    #
    chipy.utilities_logMes('COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()
    #
    chipy.utilities_logMes('SELECT PROX TACTORS')
    chipy.SelectProxTactors(freq_detect)
    #
    chipy.utilities_logMes('RESOLUTION' )
    chipy.RecupRloc(0.01)

    chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
    chipy.UpdateTactBehav()

    chipy.StockRloc()
    #
    chipy.utilities_logMes('COMPUTE DOF')
    chipy.ComputeDof()
    #
    chipy.utilities_logMes('UPDATE DOF, FIELDS')
    chipy.UpdateStep()
    #
    chipy.utilities_logMes('WRITE OUT')
    chipy.WriteOut(freq_write)
    #
    chipy.utilities_logMes('VISU & POSTPRO')
    chipy.WriteDisplayFiles(freq_display)



#
# close display & postpro
#
chipy.CloseDisplayFiles()

chipy.Finalize()
