import numpy as np

from scipy import spatial

# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
# utilities_DisableLogMes()

# defining some variables
#
# space dimension
dim = 3
# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# Defined the time discretisation and integrato
t_final = 0.02
# time evolution parameters
dt      = 0.01
# theta integrator parameter
theta   = 0.5

# write parameter
freq_write   = 1

# display parameters
freq_display = 1

# interaction parameters
freq_detect = 1
Rloc_tol    = 5.e-2

# nlgs parameters
tol    = 1e-5
relax  = 0.2
norm   = 'Quad '
gs_it1 = 1000
gs_it2 = 1000
stype  = 'Stored_Delassus_Loops         '

# methode de detection des contacts
# chipy.PRPRx_UseCpF2fDetection(0.10,100)
chipy.PRPRx_UseStoDetection(True,-1.,0.1)
#chipy.PRPRx_ForceNcDetection()
chipy.PRPRx_LowSizeArrayPolyr(1000)
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
chipy.utilities_logMes('READ DATBOX')
chipy.ReadDatbox(False)

chipy.InitHDF5('lmgc90.h5')
#
# open display & postpro
#
chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles(write_f2f=3)
chipy.OpenPostproFiles()

### compute masses ###
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

# RBDY3_SetInvisible(2)

while chipy.TimeEvolution_GetTime() < t_final :
    
    #
    chipy.utilities_logMes('INCREMENT STEP')
    chipy.IncrementStep()

    # if TimeEvolution_GetTime() >= 0.2:
        # RBDY3_SetInvisible(2)
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
    chipy.RecupRloc()

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
    #~ utilities_logMes('WRITE OUT DOF')
    chipy.WriteOut()
    #~ WriteOutDof(freq_write)
    #~ WriteLastDof()
    #
    #~ utilities_logMes('WRITE OUT Rloc')
    #~ WriteOutVlocRloc(freq_write)
    #~ WriteLastVlocRloc()
    #
    chipy.utilities_logMes('VISU & POSTPRO')
    chipy.WriteDisplayFiles(freq_display)
    chipy.WritePostproFiles()
    #PRPRx_VisavisVTKDrawAll(freq_display)


inters = chipy.getInteractions()

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

chipy.Finalize()



check_ifields = ['icdbdy','ianbdy','status']
check_ffields = ['coor']
ref_inters = np.array([(1, 3, b'stick', [ 1.74246129e+00, -9.99989994e-02, -9.99991345e-05]),
                       (1, 3, b'stick', [ 1.74246129e+00,  1.09999900e+00, -9.99990448e-05]),
                       (1, 3, b'stick', [ 1.00000117e+00,  1.09999900e+00, -1.00002245e-04]),
                       (1, 3, b'stick', [ 1.00000117e+00, -9.99989994e-02, -1.00002335e-04]),
                       (1, 4, b'stick', [ 9.99999173e-01, -9.99990001e-02, -1.00002171e-04]),
                       (1, 4, b'stick', [ 9.99999173e-01,  1.09999900e+00, -1.00002285e-04]),
                       (1, 4, b'stick', [ 2.57539047e-01,  1.09999900e+00, -9.99997341e-05]),
                       (1, 4, b'slide', [ 2.57539047e-01, -9.99990001e-02, -9.99996194e-05]),
                       (2, 3, b'stick', [ 1.00000089e+00,  1.00004882e-06,  2.00000517e-01]),
                       (2, 3, b'slide', [ 1.00000089e+00,  9.99999000e-01,  2.00000517e-01]),
                       (2, 3, b'stick', [ 1.74246158e+00,  9.99999000e-01,  9.42461234e-01]),
                       (2, 3, b'stick', [ 1.74246158e+00,  9.99987990e-07,  9.42461234e-01]),
                       (2, 4, b'stick', [ 2.57538761e-01,  1.00011026e-06,  9.42461238e-01]),
                       (2, 4, b'stick', [ 2.57538761e-01,  9.99999000e-01,  9.42461238e-01]),
                       (2, 4, b'stick', [ 9.99999461e-01,  9.99999000e-01,  2.00000533e-01]),
                       (2, 4, b'stick', [ 9.99999460e-01,  1.00000521e-06,  2.00000533e-01]),
                       (3, 4, b'slide', [ 1.00000017e+00, -9.99989994e-02,  1.99998827e-01]),
                       (3, 4, b'slide', [ 1.00000017e+00,  1.09999900e+00,  1.99998827e-01]),
                       (3, 4, b'slide', [ 1.00000017e+00,  1.09999900e+00, -9.90040154e-05]),
                       (3, 4, b'slide', [ 1.00000017e+00, -9.99989994e-02, -9.90039904e-05])],
      dtype=[('icdbdy', '<i4'), ('ianbdy', '<i4'), ('status', 'S5'), ('coor', '<f8', (3,))])


assert np.all( inters[check_ifields] == ref_inters[check_ifields] ), f"error checking f{check_ifields} fields"
for f in check_ffields:
  ref_field = spatial.KDTree( ref_inters[f] )
  d, i = ref_field.query( inters[f], k=1, p=2 )
  i.sort()
  assert np.all( i == range(ref_inters.size) ), f"wrong field {f} indexing"
  assert np.all( d<2e-8 ), f"error checking {f} field\n{d}"

