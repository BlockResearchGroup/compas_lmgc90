import os

from pylmgc90 import chipy

# range a files to read
n_min= 1
n_max= 3
step = 2

chipy.checkDirectories()

# space dimension
dim  = 3
mhyp = 0

# time step (important for rloc reading)
dt = 1.e-3
theta = 0.501

freq_display = 1
freq_write   = 1

# create some directory for checking results
if( not os.path.isdir('new_results') ):
   os.mkdir('new_results')
   os.mkdir( os.path.join('new_results', 'OUTBOX' ) )
   os.mkdir( os.path.join('new_results', 'DISPLAY') )

######## initial state ###########################

### computation's parameters definition ### 
chipy.SetDimension(dim,mhyp)

chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

chipy.PT3Dx_SetDisplayRadius(5.e-3)

### model reading ###
chipy.ReadDatbox()

chipy.OpenDisplayFiles()
# to read header...
chipy.io_hdf5_read('lmgc90_output.h5',0)
chipy.InitHDF5('new_results/lmgc90_output2.h5')

for k in range(n_min,n_max+1,step):
    #
    chipy.utilities_logMes('READ DOF VlocRloc from h5 file')
    chipy.io_hdf5_read('lmgc90_output.h5',k)

    #chipy.utilities_logMes('READ INI GPV')
    #chipy.ReadIniGPV(k)

    # Write again output files and display in
    # a different directory
    chipy.overall_SetWorkingDirectory('new_results')
    chipy.utilities_logMes('Write Display')
    chipy.WriteDisplayFiles(freq=1)
    chipy.utilities_logMes('Write DOF')
    chipy.WriteOutDof()
    chipy.utilities_logMes('Write VlocRloc')
    chipy.WriteOutVlocRloc()
    chipy.WriteHDF5()
    chipy.overall_SetWorkingDirectory('./')

  
chipy.CloseDisplayFiles()


