import os
import math
import numpy as np

# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
# chipy.utilities_DisableLogMes()

### definition des parametres du calcul ### 

dim  = 2

dt       = 2e-4
nb_steps = 1
theta    = 0.5

norm   = 'Quad '
tol    = 1e-5
relax  = 1.0
gs_it1 = 4
gs_it2 = 200
solver_type = 'Stored_Delassus_Loops         '

freq_display = 1
ref_radius   = 0.1e-2

# Set space dimension
chipy.SetDimension(dim,1)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#
chipy.utilities_logMes('READ BEHAVIOURS')
chipy.ReadBehaviours()
#
chipy.utilities_logMes('READ BODIES')
chipy.ReadBodies()
#
chipy.utilities_logMes('LOAD BEHAVIOURS')
chipy.LoadBehaviours()
#
chipy.utilities_logMes('READ INI DOF')
chipy.ReadIniDof()
#
chipy.utilities_logMes('READ DRIVEN DOF')
chipy.ReadDrivenDof()
#
chipy.utilities_logMes('LOAD TACTORS')
chipy.LoadTactors()
#
chipy.utilities_logMes('READ INI Vloc Rloc')
chipy.ReadIniVlocRloc()

#
# paranoid writes
#
chipy.utilities_logMes('WRITE BODIES')
chipy.WriteBodies()
chipy.utilities_logMes('WRITE BEHAVIOURS')
chipy.WriteBehaviours()
chipy.utilities_logMes('WRITE DRIVEN DOF')
chipy.WriteDrivenDof()

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenPostproFiles()
chipy.OpenDisplayFiles()

# to write solver interactions in vtk
wd  = chipy.overall_GetWorkingDirectory()
wdf = 1
fii = chipy.startCollection(os.path.join(wd,'DISPLAY','solver_inter.pvd'),wdf) 

chipy.nlgs_SetCheckType(norm, tol, relax)
#
# simulation part ...
#

# ... calls a simulation time loop
# since constant compute elementary mass once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

for k in range(0,nb_steps):
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()
  chipy.utilities_logMes('COMPUTE Fint')
  chipy.ComputeBulk()
  chipy.utilities_logMes('COMPUTE Free Vlocy')
  chipy.ComputeFreeVelocity()

  chipy.utilities_logMes('SELECT PROX TACTORS')
  chipy.SelectProxTactors()

  chipy.utilities_logMes('RESOLUTION' )
  chipy.RecupRloc()

  #chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
  chipy.nlgs_ExPrep(solver_type)
  for it1 in range(gs_it2):
    chipy.utilities_logMes(' GS block: '+str(it1+1))
    chipy.nlgs_ExIter(gs_it1)
    chipy.nlgs_AfterIterCheck()
    chipy.writeThisToVTK(os.path.join(wd,'DISPLAY','solver_inter_'+str(wdf)+'.vtp'),fii, wdf, ref_radius, dt)
    #chipy.writeThisToVTK(os.path.join(wd,'DISPLAY','solver_inter_'+str(wdf)+'.vtp'),fii, wdf, dt)
    wdf += 1
  chipy.nlgs_ExPost()
  #---------------------------

  chipy.UpdateTactBehav()

  chipy.StockRloc()

  chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
  chipy.ComputeDof()

  chipy.utilities_logMes('UPDATE DOF, FIELDS')
  chipy.UpdateStep()


  chipy.utilities_logMes('VISU & POSTPRO')
  chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

#
# close display & postpro
#
chipy.ClosePostproFiles()
chipy.CloseDisplayFiles()
chipy.stopCollection(fii)

# this is the end
chipy.Finalize()
