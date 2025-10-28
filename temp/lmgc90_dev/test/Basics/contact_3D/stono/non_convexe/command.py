import numpy as np

from scipy import spatial

# importing chipy module
from pylmgc90 import chipy

np.random.seed(12)

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
tol    = 1e-4
relax  = 1.
norm   = 'QM/16'
gs_it1 = 1000
gs_it2 = 1000
stype  = 'Stored_Delassus_Loops         '

chipy.POLYR_FlatnessAngle(2)
chipy.POLYR_TopologyAngle(20)
chipy.POLYR_SkipAutomaticReorientation()

# methode de detection des contacts
chipy.PRPRx_UseStoDetection(True,0.5,0.1)
#chipy.PRPRx_ForceNcDetection()
# chipy.PRPRx_ForceF2fDetection()
chipy.PRPRx_LowSizeArrayPolyr(1000)

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
    f2f_faces = chipy.PRPRx_GetF2fAllIdata()
    chipy.WriteDisplayFiles(freq_display, cdf=('ContactPointContour', f2f_faces[:,0]), anf=('Face2Face', f2f_faces[:,1]) )
    #WriteVisavisDisplayFiles(freq_display)
    chipy.WritePostproFiles()

    connec, points = chipy.PRPRx_GetF2fOutlines()


inters = chipy.getInteractions()
#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

chipy.Finalize()

ref_co = np.array([4, 1, 4, 1, 4, 1, 4, 1, 4])
ref_po = np.array([[1.00005438e-06, 1.00000000e-01, 0.00000000e+00],
                   [2.00000000e+00, 1.00000000e+00, 0.00000000e+00],
                   [2.00000000e+00, 1.00006844e-06, 0.00000000e+00],
                   [1.00005438e-06, 1.00006844e-06, 0.00000000e+00],
                   [1.00005438e-06, 3.00000000e+00, 0.00000000e+00],
                   [2.00000000e+00, 3.00000000e+00, 0.00000000e+00],
                   [2.00000000e+00, 2.00000100e+00, 0.00000000e+00],
                   [1.00005438e-06, 2.00000100e+00, 0.00000000e+00],
                   [1.00004269e-06, 1.00003666e-06, 5.00000000e-01],
                   [1.01654269e-06, 1.00000000e+00, 5.00000000e-01],
                   [2.00000000e+00, 1.00000000e+00, 5.00000000e-01],
                   [2.00000000e+00, 1.00003666e-06, 5.00000000e-01],
                   [2.00000000e+00, 2.00000105e+00, 5.00000000e-01],
                   [1.00002828e-06, 2.00000105e+00, 5.00000000e-01],
                   [1.01652828e-06, 3.00000000e+00, 5.00000000e-01],
                   [2.00000000e+00, 3.00000000e+00, 5.00000000e-01],
                  ])

assert np.allclose( connec, ref_co ), f"error with connec\n{connec}\n{ref_co}"
assert np.allclose( connec, ref_co )

check_ifields = ['icdbdy','ianbdy','status']
check_ffields = ['coor']
ref_inters = np.array([(1, 2, b'stick', [1.666666  , 0.5       , 0.        ]),
                       (1, 2, b'stick', [1.        , 0.83333267, 0.        ]),
                       (1, 2, b'stick', [0.333334  , 0.5       , 0.        ]),
                       (1, 2, b'stick', [1.        , 0.16666733, 0.        ]),
                       (1, 5, b'stick', [1.666666  , 2.5       , 0.        ]),
                       (1, 5, b'stick', [1.        , 2.83333267, 0.        ]),
                       (1, 5, b'stick', [0.333334  , 2.5       , 0.        ]),
                       (1, 5, b'stick', [1.        , 2.16666733, 0.        ]),
                       (2, 4, b'stick', [1.66666596, 0.5       , 0.5       ]),
                       (2, 4, b'stick', [0.99999998, 0.83333267, 0.5       ]),
                       (2, 4, b'stick', [0.33333399, 0.5       , 0.5       ]),
                       (2, 4, b'stick', [0.99999998, 0.16666733, 0.5       ]),
                       (3, 4, b'slide', [0.11388029, 0.50016077, 0.63151059]),
                       (3, 4, b'slide', [0.5363678 , 0.333745  , 1.04112175]),
                       (3, 4, b'slide', [0.53635551, 0.66707827, 1.04113876]),
                       (3, 4, b'slide', [0.79080659, 0.16691707, 1.19646338]),
                       (3, 4, b'slide', [0.79081993, 0.83358374, 1.19643935]),
                       (3, 4, b'slide', [1.62809536, 0.49995735, 1.47277718]),
                       (3, 4, b'slide', [0.27570828, 0.16682727, 0.80364039]),
                       (3, 4, b'slide', [0.2757151 , 0.83349388, 0.80363397]),
                       (3, 4, b'slide', [1.05711641, 0.33354158, 1.33043884]),
                       (3, 4, b'slide', [1.05710957, 0.66687491, 1.33045552]),
                       (3, 4, b'slide', [1.39645648, 0.83329057, 1.42629695]),
                       (3, 4, b'slide', [1.39645548, 0.1666239 , 1.42630188]),
                       (5, 7, b'stick', [1.66666603, 2.50000001, 0.5       ]),
                       (5, 7, b'stick', [1.00000003, 2.83333263, 0.5       ]),
                       (5, 7, b'stick', [0.33333404, 2.49999994, 0.5       ]),
                       (5, 7, b'stick', [1.00000003, 2.16666732, 0.5       ]),
                       (6, 7, b'slide', [0.11389394, 2.49994089, 0.63152459]),
                       (6, 7, b'slide', [0.53638207, 2.66635665, 1.04113529]),
                       (6, 7, b'slide', [0.53637152, 2.33302332, 1.0411499 ]),
                       (6, 7, b'slide', [0.7908066 , 2.83308288, 1.19646336]),
                       (6, 7, b'slide', [0.79081995, 2.16641621, 1.19643934]),
                       (6, 7, b'slide', [1.62809501, 2.49983917, 1.47277712]),
                       (6, 7, b'slide', [0.27572382, 2.83327433, 0.80365267]),
                       (6, 7, b'slide', [0.27572749, 2.16660767, 0.80364922]),
                       (6, 7, b'slide', [1.05711609, 2.66625493, 1.3304387 ]),
                       (6, 7, b'slide', [1.05710925, 2.3329216 , 1.33045538]),
                       (6, 7, b'slide', [1.39645515, 2.83317261, 1.42630181]),
                       (6, 7, b'slide', [1.39645612, 2.16650594, 1.42629686])],
      dtype={'names':['icdbdy','ianbdy','status','coor'], 'formats':['<i4','<i4','S5',('<f8', (3,))], 'offsets':[14,32,58,123]})

assert np.all( inters[check_ifields] == ref_inters[check_ifields] ), f"error checking f{check_ifields} fields"
for f in check_ffields:
  ref_field = spatial.KDTree( ref_inters[f] )
  d, i = ref_field.query( inters[f], k=1, p=2 )
  i.sort()
  assert np.all( i == range(ref_inters.size) ), f"wrong field {f} indexing"
  assert np.all( d<3e-4 ), f"error checking {f} field\n{d}"
