import os,sys
import numpy as np
from random import *

from pylmgc90 import chipy

try:
  shutil.rmtree("DISPLAY")
  shutil.rmtree("OUTBOX")
  shutil.rmtree("POSTPRO")
except:
  pass

chipy.Initialize()

chipy.checkDirectories()

#fragment size
dfrag=5e-2

# parametres de calcul
chipy.SetDimension(2,1)

dt = 1e-3
nb_steps=5000

theta = 0.5
freq_detect = 1
tol    = 1e-3
relax  = 1.
gs_it1 = 50
gs_it2 = 1000
stype  = 'Stored_Delassus_Loops'

freq_write =1000

freq_display = 10
chipy.PT2Dx_SetDisplayRadius(dfrag/4)

# Initialisation
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#-------------------------

# lecture des corps, param bulk et tacts
chipy.ReadDatbox(deformable=False)
#------------------------------------------

chipy.PTPT2_LoadNetwork()
chipy.PTPT2_LoadParams()

#------------------------------------------

chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

#-----------------------------------------------------------------------

chipy.RBDY2_SetYminBoundary(-3.)
chipy.RBDY2_ComputeMass()

# boucle de relaxation

for k in range(1,nb_steps+1,1):

   chipy.IncrementStep()

   chipy.TimeEvolution_DisplayStep()

   chipy.ComputeFext()
   chipy.ComputeBulk()
   chipy.ComputeFreeVelocity()

   chipy.SelectProxTactors(freq_detect)

   if k == 1:
      chipy.StockRloc()

   chipy.RecupRloc()

   chipy.ExSolver(stype,'Quad ',tol,relax,gs_it1,gs_it2)
   chipy.UpdateTactBehav()

   chipy.StockRloc()

   chipy.ComputeDof()
   chipy.UpdateStep()

   chipy.WriteOut(freq_write)

   chipy.WriteDisplayFiles(freq_display)
   chipy.WritePostproFiles()

#------------------------------

chipy.ClosePostproFiles()
chipy.CloseDisplayFiles()

chipy.Finalize()
