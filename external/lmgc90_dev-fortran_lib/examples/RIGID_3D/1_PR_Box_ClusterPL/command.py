import os,sys

import numpy
import math

from pylmgc90 import chipy

chipy.checkDirectories()

chipy.SetDimension(3)

chipy.utilities_logMes('INIT TIME STEPPING')

nsteps = 100
dt = 0.01
theta = 0.5

tol    = 1e-4
relax  = 1.0
stype  = 'Stored_Delassus_Loops         '
norm   = 'Quad '
gs_it1 = 50
gs_it2 = 1000

freq_display= 1

chipy.PRPRx_UseCpCundallDetection(50)

chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### model reading ###
chipy.ReadDatbox(deformable=False)

### post ##
chipy.OpenDisplayFiles()
OpenPostproFiles()


chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

for k in range(1,nsteps+1,1):
   #
   chipy.IncrementStep()

   chipy.ComputeFext()
   chipy.ComputeBulk()
   chipy.ComputeFreeVelocity()

   chipy.SelectProxTactors()
   chipy.RecupRloc()
   chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
   chipy.StockRloc()

   chipy.ComputeDof()
   chipy.UpdateStep()

   chipy.WriteLastDof()
   chipy.WriteLastVlocRloc()

   chipy.WriteDisplayFiles(freq_display)
   chipy.WritePostproFiles()

chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()
chipy.Finalize()
