
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
# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt = 1e-3
nb_steps = 1

# theta integrator parameter
theta = 0.5

# deformable  yes=1, no=0
deformable = 0

chipy.POLYR_SkipAutomaticReorientation()
chipy.POLYR_FlatnessAngle(10.)
chipy.POLYR_TopologyAngle(3.)

chipy.PRPRx_ShrinkPolyrFaces(1e-3)
#chipy.PRPRx_UseCpF2fExplicitDetection(1e-1)
chipy.PRPRx_UseNcF2fExplicitDetection(5e-1,1e-1)
chipy.PRPRx_LowSizeArrayPolyr(100)

# interaction parameters
Rloc_tol = 5.e-2

# nlgs parameters
tol = 0.1666e-3
relax = 0.3
norm = 'Quad '
gs_it1 = 1000
gs_it2 = 500
stype='Stored_Delassus_Loops         '

chipy.nlgs_3D_DiagonalResolution()

macro=1

# write parameter
freq_write   = 10

# display parameters
freq_display = 1

#
# read and load
#

# Set space dimension
chipy.SetDimension(dim,mhyp)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#
chipy.utilities_logMes('READ DATBOX')
chipy.ReadDatbox(deformable)

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles(write_f2f=True)
#chipy.OpenPostproFiles()

### compute masses ###
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

for k in range(1,nb_steps+1,1):
    #
    chipy.utilities_logMes('itere : '+str(k))
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
    chipy.utilities_logMes('COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()
    #
    chipy.utilities_logMes('SELECT PROX TACTORS')
    chipy.SelectProxTactors()
    
    chipy.RecupRloc()

    if macro :

      print('macro')
      #chipy.nlgs_3D_ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)

    else:

      print('flat')
      chipy.nlgs_3D_SetCheckType(norm, tol, relax)
      chipy.nlgs_3D_ExPrep(ype)

      for i_block_iter in range(1,gs_it2+1):
         chipy.nlgs_3D_QuickScrambleContactOrder()
         chipy.nlgs_3D_ExIter(gs_it1)
         iconv = chipy.nlgs_3D_AfterIterCheck()
         chipy.nlgs_3D_DisplayAfterIterCheck()
         if (i_block_iter < 2): 
           chipy.nlgs_3D_SetCheckType(norm, tol, 1.0)                    
         else:
           if (iconv == 0): break

      chipy.utilities_logMes('it block gs: '+str(i_block_iter))
      chipy.nlgs_3D_ExPost()
      chipy.nlgs_3D_UpdateTactBehav()

    chipy.StockRloc()
    #
    chipy.utilities_logMes('COMPUTE DOF')
    chipy.ComputeDof()
    #
    ### postpro ###
    #chipy.WritePostproFiles()
    #
    chipy.utilities_logMes('UPDATE DOF')
    chipy.UpdateStep()

    ### display ###

    chipy.WriteOut(freq_write)
    chipy.WriteDisplayFiles(freq_display)

#
# close display & postpro
#
chipy.CloseDisplayFiles()
#chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
