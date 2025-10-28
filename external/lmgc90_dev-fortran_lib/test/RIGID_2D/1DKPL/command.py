# -*- coding: utf-8 -*-

#####################
# Short description #
#####################
# Shows a bug where POLYG and DISK are inverted when getting length of
# candidat


from pylmgc90 import chipy

G_dimensions  = 2

###############################################################################
# Initialization                                                              #
###############################################################################
chipy.Initialize()
chipy.checkDirectories()
chipy.SetDimension(G_dimensions,1)

###############################################################################
#                      Time integration parameters                            #
###############################################################################

nb_steps = 1
dt       = 0.001
theta    = 0.5

chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Detection parameters
Rloc_tol = 5.e-3

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# NLGS parameters
tol = 1e-5
relax = 1.0
norm = 'Maxm '
gs_it1 = 1#200
gs_it2 = 1#50
stype='Stored_Delassus_Loops'

###############################################################################
#                                  File writing                               #
###############################################################################
freq_write   = max(1,nb_steps/2)
freq_display = nb_steps


###############################################################################
#                   Reading database                                          #
###############################################################################
chipy.ReadDatbox(deformable=False)

chipy.ComputeMass()
for k in range(nb_steps):
    chipy.IncrementStep()
    chipy.ComputeFext()
    chipy.ComputeBulk()
    
    chipy.ComputeFreeVelocity()
    chipy.SelectProxTactors()
    chipy.RecupRloc(Rloc_tol)

    chipy.DKPLx_DisplayProxTactors()
    chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
    chipy.UpdateTactBehav()
    chipy.StockRloc()
    chipy.DKPLx_DisplayProxTactors()

    chipy.ComputeDof()
    chipy.UpdateStep()

    chipy.WriteOutDof(freq_write)
    chipy.WriteOutVlocRloc(freq_write)

chipy.Finalize()

