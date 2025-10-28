# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()


# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
# chipy.utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim = 3

# time evolution parameters
dt = 1e-3
nb_steps = 100 

# theta integrator parameter
theta = 0.501

# deformable  yes=1, no=0
deformable = 1

# write parameter
freq_write   = 1 

# display parameters
freq_display = 1 


#
# read and load
#

# Set space dimension
chipy.SetDimension(dim)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#
chipy.ReadDatbox(deformable)

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

#
# simulation part ...
#

# ... calls a simulation time loop
# since constant compute elementary mass matrices once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

# since constant compute elementary stiffness matrices once
chipy.utilities_logMes('COMPUTE STIFFNESS')
chipy.ComputeBulk()

# since constant compute iteration matrix once
chipy.utilities_logMes('ASSEMB KT')
chipy.AssembleMechanicalLHS()

for k in range(nb_steps):
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
    chipy.utilities_logMes('ASSEMB RHS')
    chipy.AssembleMechanicalRHS()
    #
    chipy.utilities_logMes('COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()
    #
    chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
    chipy.ComputeDof()
    #
    chipy.utilities_logMes('UPDATE DOF, FIELDS')
    chipy.UpdateStep()
    #
    chipy.utilities_logMes('WRITE OUT')
    chipy.WriteOut(freq_write)
    #
    chipy.utilities_logMes('VISU & POSTPRO')
    chipy.WriteDisplayFiles(freq_write)
    chipy.WritePostproFiles()
#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
