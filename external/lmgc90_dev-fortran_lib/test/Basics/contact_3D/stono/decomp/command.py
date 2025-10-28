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
t_final = 1.0
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
tol    = 1e-4
relax  = 1.
norm   = 'Quad '
gs_it1 = 1000
gs_it2 = 1000
stype  = 'Stored_Delassus_Loops         '

chipy.POLYR_FlatnessAngle(2)
chipy.POLYR_TopologyAngle(20)
chipy.POLYR_SkipAutomaticReorientation()

# methode de detection des contacts
chipy.PRPRx_UseStoDetection(False,0.50,0.10)
# chipy.PRPRx_ForceNcDetection()
# chipy.PRPRx_ForceF2fDetection()
# chipy.PRPRx_LowSizeArrayPolyr(1000)

# chipy.PRPRx_VerboseF2F(3,4)

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
chipy.ReadDatbox(deformable=False)

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
    # application d'un effort exterieur
    angle  = chipy.TimeEvolution_GetTime() * 2*np.pi
    print("angle = ",angle*180/np.pi)
    torser = np.empty( [6] )
    torser[:3] = 1500 * np.array([np.cos(angle),np.sin(angle),0])
    point  = np.array([0.,0.,2.])
    cdg    = chipy.RBDY3_GetBodyVector('Coor0', 2)[0:3]
    frame  = chipy.RBDY3_GetBodyMatrix('IFref', 2)
    torser[3:] = frame.dot(np.cross( (point - cdg), torser[:3]))
    chipy.RBDY3_PutBodyVector('Fext_', 2, torser)
    chipy.utilities_logMes('COMPUTE Fint')
    #
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
    chipy.utilities_logMes('WRITE OUT')
    chipy.WriteOut()
    #
    chipy.utilities_logMes('VISU & POSTPRO')
    chipy.WriteDisplayFiles(freq_display)
    chipy.WritePostproFiles()

f2f_c, f2f_p = chipy.PRPRx_GetF2fOutlines()
data = chipy.PRPRx_GetF2fStress(1)

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

chipy.Finalize()

# now check !!!

assert f2f_c[0] == 1, f"should have only 1 f2f, not {f2f_c[0]}"

coor_c = np.array([[ 4.99999002e-01,  4.99999004e-01, -1.33487967e-09],
                   [ 4.99999000e-01, -4.99998994e-01, -8.79447212e-10],
                   [ 3.74955759e-01, -4.99998994e-01, -8.22069932e-10],
                   [ 3.00000841e-01, -2.89632537e-01, -8.83484060e-10],
                   [ 3.00000411e-01,  3.00000999e-01, -1.15202265e-09],
                   [ 8.99106312e-02,  3.00001000e-01, -1.05562096e-09],
                   [ 1.86500594e-02,  4.99999005e-01, -1.11400813e-09],],
                 )

conn_c = np.array([7], dtype=int)
coor_d = np.array([[ 3.74955759e-01, -4.99998994e-01, -8.22069932e-10],
                   [ 3.00000995e-01, -4.99998994e-01, -7.87676227e-10],
                   [ 3.00000841e-01, -2.89632537e-01, -8.83484060e-10],
                   [ 1.86500594e-02,  4.99999005e-01, -1.11400813e-09],
                   [ 8.99106312e-02,  3.00001000e-01, -1.05562096e-09],
                   [-4.99998996e-01,  3.00001001e-01, -7.84935319e-10],
                   [-4.99998996e-01,  4.99999006e-01, -8.76021085e-10],],
                 )

conn_d = np.array([3,4], dtype=int)
sigma  = np.array([-1.44933904e+05, -6.32581444e+04, -3.76504705e+04, -2.26255474e-03,
                   -1.86575471e-03, -8.80643774e-04, -7.59251577e-05,],
                 )

decomp = 0.329833294700718

fields = ['coor_compression', 'connecitivty_compression', 'coor_decompression', 'connectivity_decompression', 'sigma', 'decompression']
for i, ref in zip( [0, 2], [coor_c, coor_d] ):
  ref_field = spatial.KDTree( ref )
  rad, idx = ref_field.query( data[i], k=1, p=2 )
  idx.sort()
  assert np.all( idx == range(ref.shape[0]) ), f"wrong field indexing"
  assert np.all( rad<1e-6 ), f"error checking coor\n{rad}"

for i, ref in zip( [1, 3, 4], [conn_c, conn_d, sigma] ):
  data[i].sort()
  ref.sort()
  assert np.allclose(data[i], ref, atol=3e-3), f"error checking {fields[i]} field\n{data[i]}\n{ref}\n{data[i]-ref}"

assert np.isclose(data[5], decomp, atol=1e-16), f"error on decompression value : {data[5]} / {decomp}"
