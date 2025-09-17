import os,sys
import pickle

from pylmgc90 import chipy
from pylmgc90 import pre

import numpy as np

#=====================================================================
with open('sample.pkl', 'rb') as f:
  data = pickle.load(f)
# Longeur de la paroi
Lext  = data['Lext'][0]
Wext  = data['Wext'][0]
R_cir = data['R_cir'][0]

# use periode
use_periode = 1
xperiode = 1*Lext
yperiode = 1*Wext
#

#=====================================================================

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
chipy.utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim = 3

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0

# time evolution parameters
dt = 1e-3
nb_steps = 250

# theta integrator parameter
theta = 0.5

# interaction parameters
Rloc_tol = 5.e-2

# nlgs parameters
tol = 1e-4
relax = 1.
#norm = 'Maxm '
#norm = 'QM/16'
norm = 'Quad'
gs_it1 = 50
gs_it2 = 20
solver_type='Stored_Delassus_Loops         '

# write parameter
freq_write   = 1

# display parameters
freq_display = nb_steps//10

#########################################
###     ###
#########################################
#
chipy.nlgs_3D_DiagonalResolution()
#chipy.nlgs_3D_SetWithQuickScramble()
chipy.RBDY3_NewRotationScheme()

# Set space dimension
chipy.SetDimension(dim,mhyp)

#########################################
### Computation parameters definition ###
#########################################
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

#####################
### Model reading ###
#####################
chipy.ReadDatbox(deformable=False)

############################
### End of Model reading ###
############################
if use_periode==1:
    chipy.SetPeriodicCondition(xperiode,yperiode)
    
# open display & postpro
chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

############################
### STARTING COMPUTATION ###
############################
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()


for k in range(1, int(nb_steps + 1)):
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
  chipy.RecupRloc(Rloc_tol)

  chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
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
  chipy.WritePostproFiles()

# close display & postpro
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()


