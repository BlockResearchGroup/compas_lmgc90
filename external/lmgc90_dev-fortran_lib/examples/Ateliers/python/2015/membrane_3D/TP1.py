import os,sys

from pylmgc90.chipy import *

##### TP1 : adding a visualization field on spheres #####
#
# Look for comment starting with TODO for instructions !

checkDirectories()

dt = 1.e-1
nb_steps = 1

theta = 0.5

tol = 0.166e-4
relax = 1.0
norm = 'Quad '
gs_it1 = 33
gs_it2 = 100
storage='Stored_Delassus_Loops         '
nlgs_SetWithQuickScramble()

freq_write   = 10
freq_display = 1
ref_radius   = 1.

confinent_p = 5.e6
nb_layers   = 10

SetDimension(3)

TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

ReadBehaviours()
ReadBodies()
LoadBehaviours()
ReadIniDof()
ReadDrivenDof()

LoadTactors()

ReadIniVlocRloc()

WriteBodies()
WriteBehaviours()
WriteDrivenDof()

OpenDisplayFiles()
OpenPostproFiles()

ComputeMass()

# TODO 1 :
#
# create a numpy array storing
# the coordinates and the radius
# of each sphere
#
# needed functions :
# - np.empty
# - SPHER_GetNbSPHER
# - SPHER_GetPtrSHER2RBDY3
# - SPHER_GetContactorRadius
# - RBDY3_GetBodyVector to get field : 'Coorb'

# TODO 2 :
#
# Create a numpy array storing the layer number
# of each sphere. Compute the highest and lowest
# point of the sample, compute the height of layer
# then compute for each sphere the id of its layer
# (layer id should start at 1).
#
# needed functions :
# - np.min, np.max
# - np.empty

# TODO 3 :
#
# To add the visualization field, create a dictionnary
# associating the key 'SPHER' (the contactor type) to
# your numpy array storing the layer ids.
# Then call :
#  WriteDisplayFiles(freq_display,layerID=('tactor',layerID))
#
# You can try your script then, but an error shall occur : the dictionnary
# associating layerId to a contactor type must define all contactor present
# in the simulation.
# Thus add a 'PLANx' key to your dictionnary and associate to it an array filled with 0
#
# needed functions :
# - np.zeros
# - PLANx_GetNbPLANx

CloseDisplayFiles()
ClosePostproFiles()


