import sys
import pickle

# importing chipy module
import numpy as np
from scipy import spatial

from pylmgc90 import chipy

to_save = True if '--save' in sys.argv else False
if to_save:
  ref = {}
else:
  with open('ref.pkl', 'rb') as f:
    ref = pickle.load(f)

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
t_final = 2.00
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
gs_it1 = 100
gs_it2 = 100
stype   = 'Stored_Delassus_Loops         '

# methode de detection des contacts
chipy.POLYR_SkipHEBuild()
# chipy.PRPRx_UseFCDetection(False,0.75,0.01)
chipy.PRPRx_UseStoDetection(False,0.75,0.01)
# chipy.PRPRx_VerboseF2F(1,2)
chipy.PRPRx_LowSizeArrayPolyr(10)

# chipy.PRPRx_SetF2fEpsSimplification(0.02)

# chipy.PRPRx_ShrinkPolyrFaces(0.01)

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
    #
    chipy.utilities_logMes('COMPUTE Fext')
    chipy.ComputeFext()
    #
    if chipy.TimeEvolution_GetTime() <= 1.0:
        decomp = -1.0 + 2.0*(chipy.TimeEvolution_GetTime()-dt)
        print("decompression = ",decomp)
        chipy.PRPRx_UseStoDetection(False,decomp,0.01)

    else:
        decomp = 0.75
        chipy.PRPRx_UseStoDetection(False,decomp,0.01)
        # application d'un effort exterieur
        angle  = (chipy.TimeEvolution_GetTime()-1.03) * 2*np.pi
        print("angle = ",angle*180/np.pi)
        force1 = 10000 * np.array([np.cos(angle+np.pi/4),np.sin(angle+np.pi/4),0])
        force2 = 10000 * np.array([np.cos(angle)        ,np.sin(angle)        ,0])
        chipy.RBDY3_PutBodyVector('Fext_', 2, [force1[0], force1[1],force1[2],0.,0.,0.] )
        chipy.RBDY3_PutBodyVector('Fext_', 4, [force2[0],-force2[1],force2[2],0.,0.,0.] )
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
    chipy.utilities_logMes('WRITE OUT')
    chipy.WriteOut()
    #
    chipy.utilities_logMes('VISU & POSTPRO')
    chipy.WriteDisplayFiles(freq_display)
    chipy.WritePostproFiles()

    if decomp in [-1., -0.5, 0., 0.5, 1.]:
      inters = chipy.getInteractions(human=False)
      if to_save:
        ref[decomp] = inters['coor']
      else:
        assert np.allclose( inters['coor'], ref[decomp] )

#
# close display & postpro
#
chipy.ClosePostproFiles()

stress = [ chipy.PRPRx_GetF2fStress(i_f2f) for i_f2f in range(1,3) ]
if to_save:
  ref['stress'] = stress
  with open('ref.pkl','wb') as f:
    pickle.dump( ref, f )
else:
  for sl, rl in zip(stress, ref['stress']):
    cCs, sCs, cDs, sDs, sis, des = sl
    cCr, sCr, cDr, sDr, sir, der = rl

    ref_C = spatial.KDTree( cCr )
    rad_C, idx_C = ref_C.query( cCs, k=1, p=2 )
    assert np.allclose(sis,sir[idx_C], atol=5e-3, rtol=5e-4)
    idx_C.sort()
    assert np.all( idx_C == range(cCr.shape[0]) )
    if cDr.size > 0 :
      ref_D = spatial.KDTree( cDr )
      rad_D, idx_D = ref_D.query( cDs, k=1, p=2 )
      idx_D.sort()
      assert np.all( idx_D == range(cDr.shape[0]) )
    assert np.all(sCs==sCr)
    assert np.all(sDs==sDr)
    assert np.allclose(des,der)

    

chipy.Finalize()


