import sys, subprocess
import itertools
import collections
from pathlib import Path

# this is necessary because swig generate
# a shared library with "local symbols"
# but sometimes MPI library may not be able
# to handle them, thus this do the trick
try:
  import dl
  sys.setdlopenflags(dl.RTLD_NOW|dl.RTLD_GLOBAL)
except:
  pass

import numpy as np

from .lmgc90 import *
from .preconW import *
from .ddm_utils import *

from . import config

from .vtk_select import *

from ..post import central_kernel

DIMENSION = None

# for new display
wdf = 0

dict_fid = {}
dict_fgp = {}
fvtk = None
vblock_list = []

# some mappers
get_mdl = None
get_tac = None
get_sta = None
get_int = None
int_id  = None

inters_list = []
inter_dtype = None
inter_itype = None

# for registering internals of interaction
inter_mapper = {}
registers2display = False

# to store if hdf5 use
use_hdf5 = False

def SetDimension(dim,mod=1):
  """Set dimension of the simulation and
  type of simulation.

  possible values of mod:

  - 1 for PSTRAIN
  - 2 for PSTRESS
  - 3 for AXI
  """
  global DIMENSION, inters_list, inter_dtype, inter_itype

  DIMENSION = dim
  overall_DIME(dim,mod)

  FIELDS2TYPE = {'inter'    : 'S5', 'icdan'    : 'i4',
                 'cdbdy'    : 'S5', 'icdbdy'   : 'i4',
                 'cdtac'    : 'S5', 'icdtac'   : 'i4',
                 'anbdy'    : 'S5', 'ianbdy'   : 'i4',
                 'antac'    : 'S5', 'iantac'   : 'i4',
                 'icdsci'   : 'i4', 'iansci'   : 'i4',
                 'behav'    : 'S5',
                 'status'   : 'S5', 'nb_int'   : 'i4',
                 'rl'       : '(dim,)f8'    ,
                 'vl'       : '(dim,)f8'    ,
                 'gapTT'    : 'f8'          ,
                 'coor'     : '(dim,)f8'    ,
                 'uc'       : '(dim,dim)f8' ,
                 'internals': '(max_int,)f8',
                }
  FIELDS2ITYPE = {'inter'    : 'i4', 'icdan'    : 'i4',
                  'cdbdy'    : 'i4', 'icdbdy'   : 'i4',
                  'cdtac'    : 'i4', 'icdtac'   : 'i4',
                  'anbdy'    : 'i4', 'ianbdy'   : 'i4',
                  'antac'    : 'i4', 'iantac'   : 'i4',
                  'icdsci'   : 'i4', 'iansci'   : 'i4',
                  'behav'    : 'i4',
                  'status'   : 'i4', 'nb_int'   : 'i4',
                  'rl'       : '(dim,)f8'    ,
                  'vl'       : '(dim,)f8'    ,
                  'gapTT'    : 'f8'          ,
                  'coor'     : '(dim,)f8'    ,
                  'uc'       : '(dim,dim)f8' ,
                  'internals': '(max_int,)f8',
                 }

  max_int = overall_GetMaxInternalTact()

  fields  = [ f for f in FIELDS2TYPE.keys() ]
  formats = [ f.replace('dim',str(dim)).replace('max_int',str(max_int))
              for f in FIELDS2TYPE.values()
            ]
  inter_dtype = np.dtype( {'names':fields, 'formats':formats} )

  fields  = [ f for f in FIELDS2ITYPE.keys() ]
  formats = [ f.replace('dim',str(dim)).replace('max_int',str(max_int))
              for f in FIELDS2ITYPE.values()
            ]
  inter_itype = np.dtype( {'names':fields, 'formats':formats} )

  if DIMENSION == 2:
      inters_list = [CLALp_ID, CLJCx_ID, DKALp_ID, DKDKL_ID,
                     DKDKx_ID, DKJCx_ID, DKKDx_ID,
                     DKPLx_ID, P2P2L_ID,
                     PLALp_ID, PLJCx_ID, PLPLx_ID, PTPT2_ID]
  elif DIMENSION == 3:
      inters_list = [CDCDx_ID, CDPLx_ID, CSASp_ID, PRASp_ID,
                     CSPRx_ID, PRPLx_ID, PRPRx_ID, PTPT3_ID,
                     SPCDx_ID, SPDCx_ID, SPPLx_ID, SPSPx_ID,
                     SPPRx_ID,
                    ]
  else:
      print( f"[ERROR:SetDimenion] dim parameter must be 2 or 3, not {dim}" )
      raise ValueError


def GetDimension():
  """Get dimension of the simulation.
  """
  global DIMENSION
  return DIMENSION

def Initialize():
  """Initialize LMGC90 (and timers).

  timer_GetNewTimer(xxx) must be called after this function.
  """
  global get_mdl, get_tac, get_sta, get_int, int_id

  overall_Initialize()
  timer_ClearAll()
  timer_InitializeTimers()

  parameters_checkAll()

  parameters =  ['PhysicType'             , 'BodyModel'          , 'Contactor'  ,
                 'Interaction'            , 'MatrixStorage'      , 'MatrixShape',
                 'GeneralizedCoordinates' , 'SurfaceEnergyStatus', 'InterLaw'   ,
                 'Integrator'             , 'Node'               , 'DimeMode'   ,
                 'BodyVector'             , 'ContactStatus'
                ]

  # generate list of parameters
  for parameter in parameters:

    # get list of names of a parameter type
    param_names = eval('parameters_get'+parameter+'Names()')

    # add each parameters as an attribute of the pylmgc90.chipy module...
    # and current module !!!
    for name in param_names:
      setattr(sys.modules[__name__]        , name.strip()+'_ID', eval('parameters_get'+parameter+'Id("'+name+'")'))
      setattr(sys.modules['pylmgc90.chipy'], name.strip()+'_ID', eval('parameters_get'+parameter+'Id("'+name+'")') )

  mdl_id  = {parameters_getBodyModelId(n):n for n in parameters_getBodyModelNames() }
  get_mdl = np.vectorize(mdl_id.__getitem__)
  tac_id  = {parameters_getContactorId(n):n for n in parameters_getContactorNames() }
  get_tac = np.vectorize(tac_id.__getitem__)
  sta_id  = {parameters_getContactStatusId(n):n for n in parameters_getContactStatusNames() }
  get_sta = np.vectorize(sta_id.__getitem__)
  int_id  = {parameters_getInteractionId(n):n for n in parameters_getInteractionNames() }
  get_int = np.vectorize(int_id.__getitem__)
  int_id  = { v.encode():k for k,v in int_id.items() }
  int_id  = np.vectorize(int_id.get)


def Finalize():
  """
  Close LMGC90.

  If  the --no-finalize option is passed to the python interpretor
  this function is skipped.

  This is useful when wanting to use the python interpretor in
  interactive mode after a computation and check LMGC90 database content.
  """
  global DIMENSION, wdf, dict_fid, dict_fgp, inters_list, fvtk, vblock_list
  global inter_mapper, registers2display

  if '--no-finalize' in sys.argv:
    return

  models_CleanMemory()
  ExternalModels_CleanMemory()
  bulk_behav_CleanMemory()
  tact_behav_CleanMemory()

  CLALp_CleanMemory()
  CSASp_CleanMemory()
  PRASp_CleanMemory()
  CLJCx_CleanMemory()
  DKALp_CleanMemory()
  PLALp_CleanMemory()
  DKDKL_CleanMemory()
  P2P2L_CleanMemory()

  CDCDx_CleanMemory()
  CDPLx_CleanMemory()
  CSPRx_CleanMemory()
  PRPLx_CleanMemory()
  PRPRx_CleanMemory()
  PTPT3_CleanMemory()
  SPCDx_CleanMemory()
  SPDCx_CleanMemory()
  SPPLx_CleanMemory()
  SPPRx_CleanMemory()
  SPSPx_CleanMemory()

  DKDKx_CleanMemory()
  DKJCx_CleanMemory()
  DKKDx_CleanMemory()
  DKPLx_CleanMemory()
  PLJCx_CleanMemory()
  PLPLx_CleanMemory()
  PTPT2_CleanMemory()

  ALpxx_CleanMemory()
  ASpxx_CleanMemory()
  CLxxx_CleanMemory()
  CSxxx_CleanMemory()
  DISKL_CleanMemory()
  PT2DL_CleanMemory()

  mecaMAILx_CleanMemory()
  therMAILx_CleanMemory()
  poroMAILx_CleanMemory()
  multiMAILx_CleanMemory()
  MAILx_CleanMemory()

  CYLND_CleanMemory()
  DNLYC_CleanMemory()
  PLANx_CleanMemory()
  POLYR_CleanMemory()
  PT3Dx_CleanMemory()
  SPHER_CleanMemory()

  RBDY3_CleanMemory()
  MBS3D_finalize()

  DISKx_CleanMemory()
  JONCx_CleanMemory()
  POLYG_CleanMemory()
  PT2Dx_CleanMemory()
  xKSID_CleanMemory()

  RBDY2_CleanMemory()
  MBS2D_finalize()

  postpro_CleanMemory()
  postpro_3D_CleanMemory()

  nlgs_IsInitialized(0)
  nlgs_3D_IsInitialized(0)

  try:
    io_hdf5_cleanMemory()
  except NameError:
    pass

  timer_WriteOutTimers()

  overall_Finalize()
  utilities_Finalize()

  wdf = 0

  dict_fid = {}
  dict_fgp = {}
  fvtk = None
  inters_list = []
  vblock_list = []

  get_mdl = None
  get_tac = None
  get_sta = None
  get_int = None
  int_id  = None

  inter_mapper = {}
  registers2display = False

  DIMENSION = None


### --- ###
def AssembleMechanicalLHS():
  """Assemble the elementary matrices of mechanical deformable bodies.
  """
  mecaMAILx_AssembKT()

def AssembleMechanicalRHS():
  """Assemble the elementary right hand sides of mechanical deformable bodies.
  """
  mecaMAILx_AssembRHS()

def AssembleThermalLHS():
  """Assemble the elementary matrices of thermal deformable bodies.
  """
  if DIMENSION == 2:
    PT2DL_AssembThermKT()
  therMAILx_AssembThermKT()

def AssembleThermalRHS():
  """Assemble the elementary right hand sides of thermal deformable bodies.
  """
  if DIMENSION == 2:
    PT2DL_AssembThermRHS()
  therMAILx_AssembThermRHS()

def AssemblePoroLHS():
  """Assemble the elementary matrices of porous deformable bodies.
  """
  poroMAILx_AssembKT()

def AssemblePoroRHS():
  """Assemble the elementary right hand sides of porous deformable bodies.
  """
  poroMAILx_AssembRHS()

def AssembleMultiLHS():
  """Assemble the elementary matrices of multi-phasic deformable bodies.
  """
  multiMAILx_AssembKT()

def AssembleMultiRHS():
  """Assemble the elementary right hand sides of multi-phasic deformable bodies.
  """
  multiMAILx_AssembRHS()

def CircularSelection(x, y, r):
  """Select a subpart of a 2D sample to apply it post processing commands.

  The subpart of the sample consists of all particles in a disk around a point.

  parameters:

  - x: abscissa of the center of the disk
  - y: ordinate of the center of the disk
  - r: radius of the disk centered on (x;y).
  """
  global DIMENSION
  if DIMENSION == 2:
    postpro_SetCircularSelectionZone(x, y, r)
  else:
   utilities_logMes('ERROR: CircularSelection works only with dimension 2')
   sys.exit(0)

def ComputeBulk():
  """Compute rigidity/conductivity elementary matrices and internal forces
  for all bodies in the simulation.
  """
  global DIMENSION
  if DIMENSION == 2:
    RBDY2_ComputeBulk()
  elif DIMENSION == 3:
    RBDY3_ComputeBulk()
  else :
    print( '[ERROR:ComputeBulk] must be called after SetDimension' )
    raise ValueError

  mecaMAILx_ComputeBulk()
  therMAILx_ComputeInternalFlux()
  poroMAILx_ComputeBulk()
  multiMAILx_ComputeBulk()

def ComputeDof():
  """Compute current values of displacement from the current velocities computed
  for all bodies in the simulation.
  """
  global DIMENSION
  if DIMENSION == 2:
    RBDY2_ComputeDof()
    MBS2D_ComputeDof()
  elif DIMENSION == 3:
    RBDY3_ComputeDof()
    MBS3D_ComputeDof()
  else :
    print( '[ERROR:ComputeDof] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_ComputeDof()
  mecaMAILx_ComputeField()
  poroMAILx_ComputeDof()
  multiMAILx_ComputeDof()
  multiMAILx_ComputeField()

def ComputeFext():
  """Evaluate the sum of the external forces during the current time step (due to gravity and boundary conditions)
  for all bodies in the simulation.
  """
  global DIMENSION
  if DIMENSION == 2:
    RBDY2_ComputeFext()
  elif DIMENSION == 3:
    RBDY3_ComputeFext()
  else :
    print( '[ERROR:ComputeFext] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_ComputeFext()
  therMAILx_ComputeExternalFlux()
  poroMAILx_ComputeFext()
  multiMAILx_ComputeFext()

def ComputeFreeVelocity():
  """Compute velocity free of interactions for all mechanical bodies in the simulation.

  The contact detection configuration is also computed then, except if a
  the command 'Integrator_SetContactDetectionConfiguration' is used.
  """
  global DIMENSION
  if DIMENSION == 2:
    RBDY2_ComputeFreeVelocity()
    MBS2D_ComputeFreeVelocity()
  elif DIMENSION == 3:
    RBDY3_ComputeFreeVelocity()
    MBS3D_ComputeFreeVelocity()
  else :
    print( '[ERROR:ComputeFreeVelocity] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_ComputeFreeVelocity()
  poroMAILx_ComputeFreeVelocity()
  multiMAILx_ComputeFreeState()

def ComputeMass():
  """Compute mass/capacity elementary matrices
  for all bodies in the simulation.
  """
  global DIMENSION
  if DIMENSION == 2:
    RBDY2_ComputeMass()
  elif DIMENSION == 3:
    RBDY3_ComputeMass()
  else :
    print( '[ERROR:ComputeMass] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_ComputeMass()
  poroMAILx_ComputeMass()
  multiMAILx_ComputeMass()

def ComputeRnod():
  """
  Recompute the 'reac' value of each bodies
  from the contact network (this storage)
  """
  global DIMENSION
  if DIMENSION == 2:
    inter_handler_2D_computeRnod()
  elif DIMENSION == 3:
    inter_handler_3D_computeRnod()
  else :
    print( '[ERROR:ComputeRnod] must be called after SetDimension' )
    raise ValueError

def SetContactRadius(radius):
  """Set the contact radius for spher/spher or cylinder/cylinder contacts.
  """
  global DIMENSION
  if DIMENSION == 3:
    SPSPx_SetContactRadius(radius)
    CDCDx_SetContactRadius(radius)

def DisplayOutDof():
  """Display the current values of all degrees of freedom in the simulation.
  """
  global DIMENSION
  TimeEvolution_DisplayOutDof()
  if DIMENSION == 2:
    RBDY2_DisplayOutDof()
  elif DIMENSION == 3:
    RBDY3_DisplayOutDof()
  else :
    print( '[ERROR:DisplayOutDof] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_DisplayOutDof()
  therMAILx_DisplayOutDof()
  poroMAILx_DisplayOutDof()

def DisplayOutRnod(ifrom, ito):
  """Display the current values of reactions on all bodies.
  """
  global DIMENSION
  TimeEvolution_DisplayOutRnod()
  if DIMENSION == 2:
    RBDY2_DisplayOutRnod()
  elif DIMENSION == 3:
    RBDY3_DisplayOutRnod()
  else :
    print( '[ERROR:DisplayOutRnod] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_DisplayOutRnod()
  therMAILx_DisplayOutRnod()
  poroMAILx_DisplayOutRnod()

def DisplayOutVlocRloc():
  """Display the current values of every contacts.
  """
  global DIMENSION
  TimeEvolution_DisplayOutVlocRloc()
  if DIMENSION == 2:
    CLALp_DisplayOutVlocRloc()
    CLJCx_DisplayOutVlocRloc()
    DKALp_DisplayOutVlocRloc()
    DKDKL_DisplayOutVlocRloc()
    DKDKx_DisplayOutVlocRloc()
    DKJCx_DisplayOutVlocRloc()
    DKKDx_DisplayOutVlocRloc()
    DKPLx_DisplayOutVlocRloc()
    P2P2L_DisplayOutVlocRloc()
    PLALp_DisplayOutVlocRloc()
    PLJCx_DisplayOutVlocRloc()
    PLPLx_DisplayOutVlocRloc()
    PTPT2_DisplayOutVlocRloc()
  elif DIMENSION == 3:
    CDCDx_DisplayOutVlocRloc()
    CDPLx_DisplayOutVlocRloc()
    CSASp_DisplayOutVlocRloc()
    PRASp_DisplayOutVlocRloc()
    CSPRx_DisplayOutVlocRloc()
    PRPLx_DisplayOutVlocRloc()
    PRPRx_DisplayOutVlocRloc()
    PTPT3_DisplayOutVlocRloc()
    SPCDx_DisplayOutVlocRloc()
    SPDCx_DisplayOutVlocRloc()
    SPPLx_DisplayOutVlocRloc()
    SPPRx_DisplayOutVlocRloc()    
    SPSPx_DisplayOutVlocRloc()
  else :
    print( '[ERROR:DisplayOutVlocRloc] must be called after SetDimension' )
    raise ValueError

def DisplayProxTactors():
  """Display the list of contacts.
  """
  global DIMENSION
  overall_DisplayProxTactors()
  if DIMENSION == 2:
    CLALp_DisplayProxTactors()
    CLJCx_DisplayProxTactors()
    DKALp_DisplayProxTactors()
    DKDKL_DisplayProxTactors()
    DKDKx_DisplayProxTactors()
    DKJCx_DisplayProxTactors()
    DKKDx_DisplayProxTactors()
    DKPLx_DisplayProxTactors()
    P2P2L_DisplayProxTactors()
    PLALp_DisplayProxTactors()
    PLJCx_DisplayProxTactors()
    PLPLx_DisplayProxTactors()
    PTPT2_DisplayProxTactors()
  elif DIMENSION == 3:
    CDCDx_DisplayProxTactors()
    CDPLx_DisplayProxTactors()
    CSASp_DisplayProxTactors()
    PRASp_DisplayProxTactors()
    CSPRx_DisplayProxTactors()
    PRPLx_DisplayProxTactors()
    PRPRx_DisplayProxTactors()
    PTPT3_DisplayProxTactors()
    SPCDx_DisplayProxTactors()
    SPDCx_DisplayProxTactors()
    SPPLx_DisplayProxTactors()
    SPPRx_DisplayProxTactors()    
    SPSPx_DisplayProxTactors()
  else :
    print( '[ERROR:DisplayProxTactors] must be called after SetDimension' )
    raise ValueError

def FdSelectProxTactors():
  """Special way of computing sphere/sphere contacts.
  """
  global DIMENSION
  if DIMENSION == 3:
    SPSPx_FdSelectProxTactors()

def FatalDamping(bodies=None):
  """Compute fatal damping for all bodies.
  """
  global DIMENSION

  if bodies != None:
    print('ERROR : Cannot use input list of bodies in macro function.')
    print('        Please use specific function prefixed with either RBDY2_, RBDY3_ or mecaMAILx_ .')
    raise Exception

  if DIMENSION == 2:
    RBDY2_FatalDamping()
  elif DIMENSION == 3:
    RBDY3_FatalDamping()
  else :
    print( '[ERROR:FatalDamping] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_FatalDamping()

def IncrementStep():
  """Prepare a new time step computation.
  """
  global DIMENSION
  overall_CleanWriteOutFlags()
  TimeEvolution_IncrementStep()
  TimeEvolution_DisplayStep()
  if DIMENSION == 2:
    RBDY2_IncrementStep()
    MBS2D_IncrementStep()
  elif DIMENSION == 3:
    RBDY3_IncrementStep()
    MBS3D_IncrementStep()
  else :
    print( '[ERROR:IncrementStep] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_IncrementStep()
  therMAILx_IncrementStep()
  poroMAILx_IncrementStep()
  multiMAILx_IncrementStep()

def LoadBehaviours():
  """Load behaviours read in BULK_BEHAV.DAT in 
  bodies read from BODIES.DAT.

  Must be called after: ReadBehaviours and ReadBodies.
  """
  mecaMAILx_LoadBehaviours()
  therMAILx_LoadBehaviours()
  poroMAILx_LoadBehaviours()
  multiMAILx_LoadBehaviours()
  if DIMENSION == 2:
    RBDY2_LoadBehaviours()
  elif DIMENSION == 3:
    RBDY3_LoadBehaviours()
  else :
    print( '[ERROR:LoadBehaviours] must be called after SetDimension' )
    raise ValueError

def LoadModels():
  """Load models read in MODELS.DAT in 
  bodies read from BODIES.DAT and initialize property sets.

  Must be called after : ReadModels and ReadBodies.
  Must be called before: LoadTactors
  """

  models_InitProperties()
  mecaMAILx_LoadModels()
  mecaMAILx_PushProperties()
  therMAILx_LoadModels()
  therMAILx_PushProperties()
  poroMAILx_LoadModels()
  poroMAILx_PushProperties()
  multiMAILx_LoadModels()
  multiMAILx_PushProperties()
  models_StoreProperties()
  #ExternalModels_CheckProperties()
  ExternalModels_StoreProperties()
  mecaMAILx_CheckProperties()
  therMAILx_CheckProperties()
  poroMAILx_CheckProperties()
  #multiMAILx_CheckProperties()

def LoadTactors():
  """Load contactor modules from bodies.

  Must be called after: ReadBodies, ReadIniDof and LoadModels if any mesh.
  """
  global DIMENSION
  if DIMENSION == 2:
    DISKx_LoadTactors()
    JONCx_LoadTactors()
    POLYG_LoadTactors()
    PT2Dx_LoadTactors()
    xKSID_LoadTactors()

    ALpxx_LoadTactors()
    CLxxx_LoadTactors()
    DISKL_LoadTactors()
    PT2DL_LoadTactors()
  elif DIMENSION == 3:
    ASpxx_LoadTactors()
    CSxxx_LoadTactors()

    CYLND_LoadTactors()
    DNLYC_LoadTactors()
    PLANx_LoadTactors()
    POLYR_LoadTactors()
    PT3Dx_LoadTactors()
    SPHER_LoadTactors()
  else :
    print( '[ERROR:LoadTactors] must be called after SetDimension' )
    raise ValueError

  overall_InitEntityList()

def AddDof2InBodies():
  """Change reference coordinates to current coordinates.
  Usefull to generate a BODIES.OUT corresponding to current state.

  Not available for 3D rigids!
  """
  global DIMENSION
  if DIMENSION == 2:
    RBDY2_addDof2InBodies()
  mecaMAILx_addDof2InBodies()

def SetNumberInteractionByContact(nb):
  """Experimental: set the number of interaction by contact.

  Available only for sphere/sphere and cylinder/cylinder contacts.
  """
  global DIMENSION
  if DIMENSION == 3:
    SPSPx_SetNumberInterByContact(nb)
    CDCDx_SetNumberInterByContact(nb)

def SetPeriodicCondition(xperiod=None,yperiod=None):
  """Set periodic conditions for rigid bodies.

  Only: 

  - 2D: x period for DKDKx, DKDKL and PLPLx.
  - 3D: x and y period for CDCDx, SPSPx, PRPRx and PTPT3
  """
  global DIMENSION
  if DIMENSION == 2:
    if xperiod != None:
      RBDY2_SetPeriodicCondition(xperiod)
      DKDKx_SetPeriodicCondition(xperiod)
      DKDKL_SetPeriodicCondition(xperiod)
      PLPLx_SetPeriodicCondition(xperiod)
      DKPLx_SetPeriodicCondition(xperiod)      
  elif DIMENSION == 3:
    if xperiod != None:
      RBDY3_SetXPeriodicCondition(xperiod)
      CDCDx_SetXPeriodicCondition(xperiod)
      SPSPx_SetXPeriodicCondition(xperiod)
      SPPRx_SetXPeriodicCondition(xperiod)      
      PRPRx_SetXPeriodicCondition(xperiod)
      PTPT3_SetXPeriodicCondition(xperiod)
    if yperiod != None:
      RBDY3_SetYPeriodicCondition(yperiod)
      CDCDx_SetYPeriodicCondition(yperiod)
      SPSPx_SetYPeriodicCondition(yperiod)
      SPPRx_SetYPeriodicCondition(yperiod)      
      PRPRx_SetYPeriodicCondition(yperiod)
      PTPT3_SetYPeriodicCondition(yperiod)
  else :
    print( '[ERROR:SetPeriodicCondition] must be called after SetDimension' )
    raise ValueError

def SetDomainBoundary(Xmin=None,Xmax=None,Ymin=None,Ymax=None,Zmin=None,Zmax=None):
  """Set active domain boundary for rigid bodies.

  Only: 

  - 2D: x | y boundary
  - 3D: x | y | z boundary 
  """
  global DIMENSION
  if DIMENSION == 2:
    if Xmin != None: RBDY2_SetXminBoundary(Xmin)
    if Xmax != None: RBDY2_SetXmaxBoundary(Xmax)
    if Ymin != None: RBDY2_SetYminBoundary(Ymin)
    if Ymax != None: RBDY2_SetYmaxBoundary(Ymax)      
  elif DIMENSION == 3:
    if Xmin != None: RBDY3_SetXminBoundary(Xmin)
    if Xmax != None: RBDY3_SetXmaxBoundary(Xmax)
    if Ymin != None: RBDY3_SetYminBoundary(Ymin)
    if Ymax != None: RBDY3_SetYmaxBoundary(Ymax)      
    if Zmin != None: RBDY3_SetZminBoundary(Zmin)
    if Zmax != None: RBDY3_SetZmaxBoundary(Zmax)      
  else :
    print( '[ERROR:SetDomainBoundary] must be called after SetDimension' )
    raise ValueError


def ReadBehaviours():
  """Read TACT_BEHAV.DAT and BULK_BEHAV.DAT files in DATBOX directory.
  """
  global get_law

  bulk_behav_ReadBehaviours()
  tact_behav_ReadBehaviours()

  law_id  = { ilaw:tact_behav_GetTactBehav(ilaw)[1].encode() for ilaw in range(1, tact_behav_GetNbTactBehav()+1)}
  get_law = np.vectorize(law_id.__getitem__, otypes=[np.dtype('S5')])


def ReadModels():
  """Read MODELS.DAT file in DATBOX directory and initialize external models.
  """
  models_ReadModels()

  models_InitModels()
  ExternalModels_InitModels()

def ReadBodies(version=None):
  """Read BODIES.DAT file in DATBOX directory.
  """
  global DIMENSION

  if version :
    MAILx_ReadBodies(version)
  else :
    MAILx_ReadBodies()

  if DIMENSION == 2:
    RBDY2_ReadBodies()
  elif DIMENSION == 3:
    RBDY3_ReadBodies()
  else :
    print( '[ERROR:ReadBodies] must be called after SetDimension' )
    raise ValueError

def ReadDrivenDof():
  """Read DRV_DOF.DAT file in DATBOX directory.

  Must be called after ReadIniDof.
  """
  global DIMENSION
  if DIMENSION == 2:
    RBDY2_ReadDrivenDof()
  elif DIMENSION == 3:
    RBDY3_ReadDrivenDof()
  else :
    print( '[ERROR:ReadDrivenDof] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_ReadDrivenDof()
  therMAILx_ReadDrivenDof()
  poroMAILx_ReadDrivenDof()
  multiMAILx_ReadDrivenDof()

def ReadIniDof(record=0):
  """
  Read DOF file.
  If record = 0 read DATBOX/DOF.INI file,
  if record > 0 read OUTBOX/DOF.OUT.record file,
  if record < 0 read OUTBOX/DOF.LAST file,
  and next written will be OUTBOX/DOF.OUT.record.
  Must be called after LoadBehaviours.
  """
  global DIMENSION
  TimeEvolution_ReadIniDof(record)
  if DIMENSION == 2:
    RBDY2_ReadIniDof(record)
  elif DIMENSION == 3:
    RBDY3_ReadIniDof(record)
  else :
    print( '[ERROR:ReadIniDof] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_ReadIniDof(record)
  therMAILx_ReadIniDof(record)
  poroMAILx_ReadIniDof(record)
  multiMAILx_ReadIniDof(record)

def ReadIniGPV(record=0):
  """
  Read GPV file.
  If record = 0 read DATBOX/GPV.INI file,
  if record > 0 read OUTBOX/GPV.OUT.record file,
  if record < 0 read OUTBOX/GPV.LAST file,
  and next written will be OUTBOX/GPV.OUT.record.
  Must be called after LoadModels.
  """
  TimeEvolution_ReadIniGPV(record)
  mecaMAILx_ReadIniGPV(record)
  therMAILx_ReadIniGPV(record)
  poroMAILx_ReadIniGPV(record)
  multiMAILx_ReadIniGPV(record)

def ReadIniVlocRloc(record=0):
  """Read VlocRloc file.
  If record = 0 read DATBOX/VlocRloc.INI file,
  if record > 0 read OUTBOX/VlocRloc.OUT.record file,
  if record < 0 read OUTBOX/VlocRloc.LAST file,
  and next written will be OUTBOX/VlocRloc.OUT.record.
  Must be called after LoadTactors.
  """
  global DIMENSION
  TimeEvolution_ReadIniVlocRloc(record)

  if DIMENSION == 2:
    CLALp_ReadIniVlocRloc(record)
    CLJCx_ReadIniVlocRloc(record)
    DKALp_ReadIniVlocRloc(record)
    DKDKL_ReadIniVlocRloc(record)
    DKDKx_ReadIniVlocRloc(record)
    DKJCx_ReadIniVlocRloc(record)
    DKKDx_ReadIniVlocRloc(record)
    DKPLx_ReadIniVlocRloc(record)
    P2P2L_ReadIniVlocRloc(record)
    PLALp_ReadIniVlocRloc(record)
    PLJCx_ReadIniVlocRloc(record)
    PLPLx_ReadIniVlocRloc(record)
    PTPT2_ReadIniVlocRloc(record)
  elif DIMENSION == 3:
    CDCDx_ReadIniVlocRloc(record)
    CDPLx_ReadIniVlocRloc(record)
    CSASp_ReadIniVlocRloc(record)
    PRASp_ReadIniVlocRloc(record)
    CSPRx_ReadIniVlocRloc(record)
    PRPLx_ReadIniVlocRloc(record)
    PRPRx_ReadIniVlocRloc(record)
    PTPT3_ReadIniVlocRloc(record)
    SPCDx_ReadIniVlocRloc(record)
    SPDCx_ReadIniVlocRloc(record)
    SPPLx_ReadIniVlocRloc(record)
    SPPRx_ReadIniVlocRloc(record)    
    SPSPx_ReadIniVlocRloc(record)
  else :
    print( '[ERROR:ReadIniVlocRloc] must be called after SetDimension' )
    raise ValueError

def ReadIniMpValues(record=0):
  """Read MP_VALUES file.
  If record = 0 read DATBOX/MP_VALUES.INI file,
  if record > 0 read OUTBOX/MP_VALUES.OUT.record file,
  if record < 0 read OUTBOX/MP_VALUES.LAST file,
  and next written will be OUTBOX/MP_VALUES.OUT.record.
  """
  global DIMENSION
  TimeEvolution_ReadIniMpValues(record)

  if DIMENSION == 2:
    mp_solver_ReadIniMpValues(record)
  elif DIMENSION == 3:
    mp_solver_3D_ReadIniMpValues(record)
  else :
    print( '[ERROR:ReadIniMpValues] must be called after SetDimension' )
    raise ValueError

def ReadMpBehaviours(disper=0.,model='therm'):
  """Read and load multi-physics (for rigids) behaviours.

  What is disper ?
  """
  global DIMENSION
  if DIMENSION == 2:
    mp_solver_ReadMpBehaviour()
    RBDY2_MP_LoadBehaviours(disper,model)
  elif DIMENSION == 3:
    mp_solver_3D_ReadMpBehaviour()
    RBDY3_MP_LoadBehaviours(disper)
  else :
    print( '[ERROR:ReadMpBehaviours] must be called after SetDimension' )
    raise ValueError


def ReadPostproFile():
  """
  Read DATBOX/POSTPRO.DAT file
  """
  global DIMENSION
  if DIMENSION == 2:
    postpro_ReadCommands()
  elif DIMENSION == 3:
    postpro_3D_ReadCommands()
  else :
    print( '[ERROR:ReadPostproFile] must be called after SetDimension' )
    raise ValueError

def ReadIni(record=0,h5_file=''):
  """
  Read initial state

  If compiled with HDF5 library, it can read from
  the 'h5_file' input parameters.

  Otherwise the function will try to read from OUTBOX directory

  With HDF5, if 'record' is < 1, then last record is read.
  With OUTBOX, if 'record' is -1, then .LAST are read.
  """

  if h5_file:

    h5_path = Path(overall_GetWorkingDirectory())
    h5_path = h5_path/h5_file
    if not h5_path.is_file():
      print('ERROR input h5_file does not exist :',h5_file)
      sys.exit(1)

    try:
      io_hdf5_read(h5_file,record)
    except NameError:
      print( "INFO: You are not able to initialize output file in HDF5 format.\n" + \
             "      To be able to write in HDF5 format, please enable the \n" + \
             "      WITH_HDF5 flag at the compilation time.\n" + \
             "      Example: cmake -DWITH_HDF5=ON .. && make -j8" )
      sys.exit(1)

  else:

    ReadIniDof(record)
    # should check against see_table...
    if tact_behav_GetNbTactBehav() > 0:
        ReadIniVlocRloc(record)
    if MAILx_GetNbMAILx() > 0:
        ReadIniGPV(record)

def RecupRloc(tol=None):
  """Try to get back state of interactions from last stocked interactions.

  In each contact module: verlet -> this. Must be called before SelectProxTactors.

  If tol is given in input, PLPLx_RecupRlocByPosition is used. To use the
  same type of function for PRPRx contact, use PRPRx_SetTolRecupRloc.
  """
  global DIMENSION
  if DIMENSION == 2:
    for inter_id in [ CLJCx_ID, DKALp_ID, DKDKL_ID, DKDKx_ID, DKJCx_ID,
                      DKKDx_ID, DKPLx_ID, P2P2L_ID, PLALp_ID, PLJCx_ID,
                      PTPT2_ID,
                    ]:

      inter_handler_2D_recupRloc(inter_id)
    if tol is not None:
      inter_handler_2D_recupRlocByPos(CLALp_ID, tol)
      inter_handler_2D_recupRlocByPos(PLPLx_ID, tol)
    else:
      inter_handler_2D_recupRloc(CLALp_ID)
      inter_handler_2D_recupRloc(PLPLx_ID)

  elif DIMENSION == 3:

    if tol is not None:
      inter_handler_3D_recupRlocByPos(CSASp_ID, tol)
      inter_handler_3D_recupRlocByPos(CSPRx_ID, tol)
    else:
      inter_handler_3D_recupRloc(CSASp_ID)
      inter_handler_3D_recupRloc(CSPRx_ID)

    for inter_id in [ CDCDx_ID, CDPLx_ID, PRASp_ID,
                      PRPLx_ID, PRPRx_ID, PTPT3_ID, SPCDx_ID,
                      SPDCx_ID, SPPLx_ID, SPSPx_ID, SPPRx_ID,
                    ]:
      inter_handler_3D_recupRloc(inter_id)

  else :
    print( '[ERROR:RecupRloc] must be called after SetDimension' )
    raise ValueError

def SelectProxTactors(freq_detec=1, useExt=0, reset=0):
  """Run detections if current time step number is a multiple of freq_detec.

  In case of PRPRx contact, one of next functions must be called:

  - PRPRx_UseCpCundallDetection
  - PRPRx_UseCpF2fExplicitDetection
  - PRPRx_UseCpF2fDetection
  - PRPRx_UseNcDetection
  - PRPRx_UseNcF2fDetection
  - PRPRx_UseNcF2fExplicitDetection


  Parameters:

  - freq_detec (default 1) : frequency of particules neighbourhood recomputation during detection
  - useExt (default 0) : set to 1 to use external detections of CLALp interactions (coupling with Xper)
  - reset (default 0) : set to 1 to reset the detection and redo box computation
  """
  global DIMENSION
  overall_SelectProxTactors(freq_detec)

  if DIMENSION == 2:
    CLALp_SelectProxTactors(reset, useExt)
    CLJCx_SelectProxTactors(reset)
    DKALp_SelectProxTactors(reset)
    DKDKL_SelectProxTactors(reset)
    DKDKx_SelectProxTactors(reset)
    DKJCx_SelectProxTactors(reset)
    DKKDx_SelectProxTactors(reset)
    DKPLx_SelectProxTactors(reset)
    P2P2L_SelectProxTactors(reset)
    PLALp_SelectProxTactors(reset)
    PLJCx_SelectProxTactors(reset)
    PLPLx_SelectProxTactors(reset)
    PTPT2_SelectProxTactors(reset)
  elif DIMENSION == 3:

    CDCDx_SelectProxTactors(reset)
    CDPLx_SelectProxTactors(reset)
    CSASp_SelectProxTactors(reset, useExt)
    PRASp_SelectProxTactors(reset)
    CSPRx_SelectProxTactors(reset)
    PRPLx_SelectProxTactors(reset)
    PRPRx_SelectProxTactors(reset)
    PTPT3_SelectProxTactors(reset)
    SPCDx_SelectProxTactors(reset)
    SPDCx_SelectProxTactors(reset)
    SPPLx_SelectProxTactors(reset)
    SPPRx_SelectProxTactors(reset)    
    SPSPx_SelectProxTactors(reset)

  else :
    print( '[ERROR:SelectProxTactors] must be called after SetDimension' )
    raise ValueError


def SmoothForceComputation():
  """Explicit computation of the contact forces.

  Not available for every contact types.
  """
  global DIMENSION
  if DIMENSION == 2:
    DKDKL_SmoothForceComputation()
    DKDKx_SmoothForceComputation()
    DKKDx_SmoothForceComputation()
    DKJCx_SmoothForceComputation()
  elif DIMENSION == 3:
    CDCDx_SmoothForceComputation()
    CDPLx_SmoothForceComputation()
    PTPT3_SmoothForceComputation()
    SPCDx_SmoothForceComputation()
    SPDCx_SmoothForceComputation()
    SPPLx_SmoothForceComputation()
    SPPRx_SmoothForceComputation()    
    SPSPx_SmoothForceComputation()
  else :
    print( '[ERROR:SmoothForceComputation] must be called after SetDimension' )
    raise ValueError


def StockRloc():
  """Stock state of interactions.

  In each contact module: this -> verlet. Must be called after SelectProxTactors.
  """
  global DIMENSION
  if DIMENSION == 2:
    for inter_id in [ CLALp_ID, CLJCx_ID, DKALp_ID, DKDKL_ID,
                      DKDKx_ID, DKJCx_ID, DKKDx_ID, DKPLx_ID,
                      P2P2L_ID, PLALp_ID, PLJCx_ID, PLPLx_ID,
                      PTPT2_ID,
                    ]:
      inter_handler_2D_stockRloc(inter_id)
  elif DIMENSION == 3:
    for inter_id in [ CDCDx_ID, CDPLx_ID, CSASp_ID, PRASp_ID,
                      CSPRx_ID, PRPLx_ID, PRPRx_ID, PTPT3_ID,
                      SPCDx_ID, SPDCx_ID, SPPLx_ID, SPPRx_ID, SPSPx_ID,
                    ]:
      inter_handler_3D_stockRloc(inter_id)
  else :
    print( '[ERROR:StockRloc] must be called after SetDimension' )
    raise ValueError


def UpdateStep():
  """Update state.

  Current time step values become begin time step values.
  """
  global DIMENSION
  TimeEvolution_UpdateStep()
  if DIMENSION == 2:
    RBDY2_UpdateDof()
    MBS2D_UpdateDof()
  elif DIMENSION == 3:
    RBDY3_UpdateDof()
    MBS3D_UpdateDof()
  else :
    print( '[ERROR:UpdateStep] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_UpdateDof()
  mecaMAILx_UpdateBulk()
  therMAILx_UpdateThermDof()
  therMAILx_UpdateThermBulk()
  poroMAILx_UpdateDof()
  poroMAILx_UpdateBulk()
  multiMAILx_UpdateDof()
  multiMAILx_UpdateBulk()

def WriteBehaviours():
  """Write materials and contact laws in ascii files (OUTBOX/BULK_BEHAV.OUT and OUTBOX/TACT_BEHAV.OUT).
  """
  bulk_behav_WriteBehaviours()
  tact_behav_WriteBehaviours()

def WriteBodies(version=None):
  """Write bodies in ascii file (OUTBOX/BODIES.OUT).
  """
  global DIMENSION
  overall_WriteBodies()

  if version :
    MAILx_WriteBodies(version)
  else :
    MAILx_WriteBodies()

  if DIMENSION == 2:
    RBDY2_WriteBodies()
  elif DIMENSION == 3:
    RBDY3_WriteBodies()
  else :
    print( '[ERROR:WriteBodies] must be called after SetDimension' )
    raise ValueError

def WriteDrivenDof():
  """Write driven degrees of freedom in ascii file (OUTBOX/DRV_DOF.OUT).
  """
  global DIMENSION
  overall_WriteDrivenDof()
  if DIMENSION == 2:
    RBDY2_WriteDrivenDof()
  elif DIMENSION == 3:
    RBDY3_WriteDrivenDof()
  else :
    print( '[ERROR:WriteDrivenDof] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_WriteDrivenDof()
  therMAILx_WriteDrivenDof()
  poroMAILx_WriteDrivenDof()
  multiMAILx_WriteDrivenDof()

def WriteLastDof():
  """Write last degreees of freedom values in ascii file (OUTBOX/DOF.LAST).
  """
  global DIMENSION
  TimeEvolution_WriteLastDof()
  if DIMENSION == 2:
    RBDY2_WriteLastDof()
  elif DIMENSION == 3:
    RBDY3_WriteLastDof()
  else :
    print( '[ERROR:WriteLastDof] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_WriteLastDof()
  therMAILx_WriteLastDof()
  poroMAILx_WriteLastDof()
  multiMAILx_WriteLastDof()

def WriteLastGPV():
  """Write last Gauss points values in ascii file (OUTBOX/GPV.LAST).
  """
  TimeEvolution_WriteLastGPV()
  MAILx_WriteLastGPV()

def WriteLastRnod():
  """Write last nodal reaction in ascii file (OUTBOX/Rnod.LAST).
  """
  global DIMENSION
  TimeEvolution_WriteLastRnod()
  if DIMENSION == 2:
    RBDY2_WriteLastRnod()
  elif DIMENSION == 3:
    RBDY3_WriteLastRnod()
  else :
    print( '[ERROR:WriteLastRnod] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_WriteLastRnod()
  therMAILx_WriteLastRnod()
  poroMAILx_WriteLastRnod()

def WriteLastVlocRloc():
  """Write last interactions values in ascii file (OUTBOX/VlocRloc.LAST).
  """
  global DIMENSION
  TimeEvolution_WriteLastVlocRloc()

  if DIMENSION == 2:
    CLALp_WriteLastVlocRloc()
    CLJCx_WriteLastVlocRloc()
    DKALp_WriteLastVlocRloc()
    DKDKL_WriteLastVlocRloc()
    DKDKx_WriteLastVlocRloc()
    DKJCx_WriteLastVlocRloc()
    DKKDx_WriteLastVlocRloc()
    DKPLx_WriteLastVlocRloc()
    P2P2L_WriteLastVlocRloc()
    PLALp_WriteLastVlocRloc()
    PLJCx_WriteLastVlocRloc()
    PLPLx_WriteLastVlocRloc()
    PTPT2_WriteLastVlocRloc()
  elif DIMENSION == 3:
    CDCDx_WriteLastVlocRloc()
    CDPLx_WriteLastVlocRloc()
    CSASp_WriteLastVlocRloc()
    PRASp_WriteLastVlocRloc()
    CSPRx_WriteLastVlocRloc()
    PRPLx_WriteLastVlocRloc()
    PRPRx_WriteLastVlocRloc()
    PTPT3_WriteLastVlocRloc()
    SPCDx_WriteLastVlocRloc()
    SPDCx_WriteLastVlocRloc()
    SPPLx_WriteLastVlocRloc()
    SPPRx_WriteLastVlocRloc()    
    SPSPx_WriteLastVlocRloc()
  else :
    print( '[ERROR:WriteLastVlocRloc] must be called after SetDimension' )
    raise ValueError

def WriteLastMpValues():
  """Write last mp values in ascii file (OUTBOX/MP_VALUES.LAST).
  """
  global DIMENSION
  TimeEvolution_WriteLastMpValues()
  if DIMENSION == 2:
    mp_solver_WriteLastMpValues()
  elif DIMENSION == 3:
    mp_solver_3D_WriteLastMpValues()
  else :
    print( '[ERROR:WriteLastMpValues] must be called after SetDimension' )
    raise ValueError

def WriteOutDof(nsteps=1):
  """Write degrees of freedom of current time step in ascii file (OUTBOX/DOF.x.OUT where 'x' is the rank of the file)
  if current time step number is multiple of nsteps.
  """
  global DIMENSION

  # If it is not a step to write Vloc_Rloc, continue
  if ( ( TimeEvolution_GetStep() % nsteps ) != 0 ) :
    return

  TimeEvolution_WriteOutDof(nsteps)

  if DIMENSION == 2:
    RBDY2_WriteOutDof()
  elif DIMENSION == 3:
    RBDY3_WriteOutDof()
  else :
    print( '[ERROR:WriteOutDof] must be called after SetDimension' )
    raise ValueError

  mecaMAILx_WriteOutDof()
  therMAILx_WriteOutDof()
  poroMAILx_WriteOutDof()
  multiMAILx_WriteOutDof()


def WriteOutMpValues():
  """Write mp values of current time step in ascii file (OUTBOX/MP_VALUES.x.OUT where 'x' is the rank of the file)
  if current time step number is multiple of nsteps.
  """
  global DIMENSION
  TimeEvolution_WriteOutMpValues()
  if DIMENSION == 2:
    mp_solver_WriteOutMpValues()
  elif DIMENSION == 3:
    mp_solver_3D_WriteOutMpValues()
  else :
    print( '[ERROR:WriteOutMpValues] must be called after SetDimension' )
    raise ValueError


def WriteMpBehaviours():
  """Write Multi-physics ascii file in OUTBOX directory.
  """
  global DIMENSION
  if DIMENSION == 2:
    mp_solver_WriteMpBehaviour()
  elif DIMENSION == 3:
    mp_solver_3D_WriteMpBehaviour()
  else :
    print( '[ERROR:WriteMpBehaviours] must be called after SetDimension' )
    raise ValueError

def OpenDisplayFiles(restart=1, mecagp_field=None, thergp_field=None, write_f2f=0, write_parameters=True):
  """Initialize visualization file writing.

  Parameters:  

  - restart (int)   : first index of file to write
  - mecagp_field (list of strings) : the list of meca GP field to display ('strain' and/or 'stress')
  - thergp_field (list of strings) : the list of ther GP field to display ('gradT' and/or 'fluxT')
  - write_f2f (integer) : 0 do not write, 1 write only f2f, 2 also write central kernel, 3 add stress
  - write_parameters (boolean) : True (default) write integer to string parameters mapping in separate file
  """
  global DIMENSION, wdf, dict_fid, dict_fgp, inters_list, fvtk, vblock_list

  wdf = restart-1

  tact_names  = None
  inter_names = None
  tacts_dict  = {}

  # the dict_fid contains for each type of file (which is the key):
  # a tuple with : a file descriptor, a vtkUnstructuredGrid (may be None),
  # and an emtpy dictionnary for the options
  if config.is_vtk_display :

    wd = Path(overall_GetWorkingDirectory())

    nbm = mecaMAILx_GetNbMecaMAILx()

    if write_parameters:
      writeParametersToCsv(wd/'DISPLAY')

    if nbm > 0 :
      vblock_list.append('mecafe')
      mecafe_grid = InitMecaFeToVTK(DIMENSION)
      dict_fid['mecafe'] = (mecafe_grid, {})

      writexSxxxToVTK(wd/'DISPLAY')

      if mecagp_field:
        accept = ['stress', 'strain']
        if isinstance(mecagp_field, str):
          mecagp_field = [mecagp_field]
        for mf in mecagp_field:
          msg = f"Unknown mecagp_field argument, must be 'stress' or 'strain' (not '{mf}')"
          assert mf in accept, msg
        vblock_list.append('mecagp')
        mecagp_pd = InitMecaGpToVTK(mecagp_field)
        dict_fgp['mecagp'] = (mecagp_pd, mecagp_field)

      with_rigid = False
      for i in range(1, nbm+1):
        if mecaMAILx_IsRigid(i):
          with_rigid = True
          break

      if with_rigid :
        vblock_list.append('meca_R')
        meca_R_pd = InitMecaRToVTK(DIMENSION)
        dict_fgp['meca_R'] = (meca_R_pd, {})

    if therMAILx_GetNbTherMAILx() > 0 :
      vblock_list.append('therfe')
      therfe_grid = InitTherFeToVTK(DIMENSION)
      dict_fid['therfe'] = (therfe_grid, {})

      if thergp_field:
        accept = ['gradT', 'fluxT']
        if isinstance(thergp_field, str):
          thergp_field = [thergp_field]
        for tf in thergp_field:
          msg = f"Unknown thergp_field argument, must be 'gradT' or 'fluxT' (not '{tf}')"
          assert tf in accept, msg
        vblock_list.append('thergp')
        thergp_pd = InitTherGpToVTK(thergp_field)
        dict_fgp['thergp'] = (thergp_pd, thergp_field)


    if poroMAILx_GetNbPoroMAILx() > 0 :
      vblock_list.append('porofe')
      porofe_grid = InitPoroFeToVTK(DIMENSION)
      dict_fid['porofe'] = (porofe_grid, {})

    if multiMAILx_GetNb() > 0 :
      vblock_list.append('multife')
      multife_grid = InitMultiFeToVTK(DIMENSION)
      dict_fid['multife'] = (multife_grid, {})

    if DIMENSION==2:

      tact_names  = ['DISKx', 'DISKx', 'JONCx', 'POLYG', 'PT2Dx', 'xKSID']

      if RBDY2_GetNbRBDY2() > 0:
        vblock_list.append('rigids')
        rigids_grid = InitRigidsToVTK(DIMENSION)
        dict_fid['rigids'] = (rigids_grid, {})

        vblock_list.append('tacts')
        InitTactorsToVTK(tact_names,tacts_dict)
        dict_fid['tacts'] = (tacts_dict, {})

      elif MBS2D_getNb() > 0:
        vblock_list.append('tacts')
        InitTactorsToVTK(tact_names,tacts_dict)
        dict_fid['tacts'] = (tacts_dict, {})


    elif DIMENSION==3:

      tact_names  = ['SPHER', 'POLYR', 'PLANx', 'CYLND', 'DNLYC', 'PT3Dx']

      if RBDY3_GetNbRBDY3() > 0:
        vblock_list.append('rigids')
        rigids_grid = InitRigidsToVTK(DIMENSION)
        dict_fid['rigids'] = (rigids_grid, {})

        vblock_list.append('tacts')
        InitTactorsToVTK(tact_names,tacts_dict)
        dict_fid['tacts'] = (tacts_dict, {})

        if 'POLYR' in tacts_dict.keys():
            writePolyrToVTK(wd/'DISPLAY'/'polyr.vtu', tacts_dict['POLYR'][2])

      elif MBS3D_getNb() > 0:
        vblock_list.append('tacts')
        InitTactorsToVTK(tact_names,tacts_dict)
        dict_fid['tacts'] = (tacts_dict, {})

      else:
        # to write polyr.vtu when there are only POLYD
        InitTactorsToVTK(['POLYR'],tacts_dict)
        if 'POLYR' in tacts_dict.keys():
          writePolyrToVTK(wd/'DISPLAY'/'polyr.vtu', tacts_dict['POLYR'][2])

    else :
      print( '[ERROR:OpenDisplayFiles] must be called after SetDimension' )
      raise ValueError

    if tact_behav_GetNbTactBehav() > 0 :

      vblock_list.append('ptc')
      if write_f2f:
        vblock_list.append('Face2Face')
        vblock_list.append('PressureCenter')
        vblock_list.append('ContactPointContour')
        if write_f2f > 1:
          vblock_list.append('CentralKernel')
          if write_f2f > 2:
            vblock_list.append('CompressionStress')

    if vblock_list:
      fname = Path(wd)/'DISPLAY'/f"lmgc90.pvd"
      fvtk  = startCollection(fname,wdf)


def WriteDisplayFiles(freq=1, ref_radius=None, normal_orient=None, write_gp=None, **kw):
  """Write visualization files if current time step number is a multiple of freq in DISPLAY directory.

  Parameters:

  - freq (int)       : manages the occurence of file creation

  Optional arguments are a tuple to add external fields to visualize in case a VTK.

  Deprecated: the arguments 'ref_radius', 'normal_orient' and 'write_gp' are now useless and will be removed in a future release
  """
  global DIMENSION, dict_fid, dict_fgp, wdf, tacts_dict, inters_list, registers2display, inter_mapper, fvtk, vblock_list

  if ( not config.is_vtk_display ):
    print( '[WARNING:WriteDisplayfiles] Since vtk module is not available this function does nothing' )
    return

  if TimeEvolution_GetStep()%freq == 0 :

    wdf += 1

    wd = overall_GetWorkingDirectory()

    # reset user fields
    ufields = collections.defaultdict(dict)

    # to check for deprecated arguments:
    depre = ['ref_radius', 'normal_orient', 'write_gp']
    msg = lambda n: f"[WARNING:WriteDisplayFiles] '{n}' parameter is now useless.\n{' '*28}Please remove it since this will raise an error in a later release"
    # check for deprecated arguments
    for d in depre:
      if eval(d) is not None:
        print( msg(d) )

    # sort user fields depending on block
    if len(kw) > 0:
      for k,v in list(kw.items()):
        if v[0] not in vblock_list:
          print( f"[WARNING:WriteDisplayFiles] '{v[0]}' block not in list of written blocks")
        elif v[0] == 'CompressionStress':
          print( f"[WARNING:WriteDisplayFiles] '{v[0]}' block does not handle user field yet")
        else:
          # beurk...
          if 'fe' in v[0]:
            ufields[ v[0] ][k] = v[1:]
          else:
            ufields[ v[0] ][k] = v[1]

    if fvtk:

      fname = Path(wd)/'DISPLAY'/f"lmgc90_{str(wdf)}.vtmb"
      vtk_blocks = getVtkBlockFile(vblock_list)

      # write all type of block
      for k, fid in dict_fid.items() :
        if k not in vblock_list:
          continue
        i_block = vblock_list.index(k)
        writeToBlock( vtk_blocks, i_block, k, DIMENSION, fid[0], **fid[1], **ufields[k] )

      for k, fgp in dict_fgp.items() :
        if k not in vblock_list:
          continue
        i_block = vblock_list.index(k)
        writeGpdToBlock( vtk_blocks, i_block, k, DIMENSION, fgp[0], fgp[1], **ufields[k] )

      if 'ptc' in vblock_list:

        inters = getInteractions(human=False)
        # get the register
        register_opt = {}
        if registers2display:
          for field in inter_mapper.keys():
            internals = getInternalArray(field, inters)
            register_opt[field] = internals

        i_block = vblock_list.index('ptc')
        writeIntersToBlock( vtk_blocks, i_block, inters, DIMENSION, **register_opt, **ufields['ptc'] )

      if 'Face2Face' in vblock_list:
        # there is room for debate here...
        # in other writeXXXBlocks function, the function itself call the accessor,
        # whereas here, the data are provided in input.
        # it would be possible to move these GetF2f thingy inside the display module...

        f2f_connec, f2f_coor = PRPRx_GetF2fOutlines()
        f2f_inters = PRPRx_GetF2f2Inters()
        idx_prprx = inters['inter']==PRPRx_ID
        prprx_inters = inters[idx_prprx]

        i_block = vblock_list.index('Face2Face')
        # contrary to other writeXXXToBlock, there are 3 blocks written here
        # thus the ufields dict cannot be used in the same way to provide optional arguments
        # so the dictionnary are provided one by one... especially if one hopes to
        # add the same named field to each block
        addF2fToVtkBlocks(vtk_blocks, i_block, f2f_connec, f2f_coor, f2f_inters, prprx_inters,
                          ufields['Face2Face'], ufields['PressureCenter'], ufields['ContactPointContour'])

        if 'CentralKernel' in vblock_list:
          i_block = vblock_list.index('CentralKernel')
          nb_f2f = PRPRx_GetNbF2f()+1
          ck_list = []
          for i_f2f in range(1,nb_f2f):
            #ck = central_kernel.get(f2f_connec, f2f_coor, f2f_inters, prprx_inters)
            ck = PRPRx_GetF2fCentralKernel(i_f2f)
            if ck[0].shape[0] < 3:
              continue
            ck.append(i_f2f)
            ck_list.append(ck)
          f2f_stress = None
          if 'CompressionStress' in vblock_list:
            f2f_stress = []
            for i_f2f in range(1,nb_f2f):
              data = PRPRx_GetF2fStress(i_f2f)
              data.append(i_f2f)
              if data[0].size > 0 or data[2].size > 0:
                f2f_stress.append(data)
              elif data[-1] == -99.:
                f2f_stress.append(data)
          # paranoid ?
          #assert len(ck) == len(f2f_stress), 'error when getting f2f stress'
          central_kernel.addCkToVtkBlocks(vtk_blocks, i_block, ck_list, f2f_stress, ufields['CentralKernel'])

      writeBlocksToVTK( fvtk, fname, vtk_blocks )

def OpenPostproFiles(restart=0):
  """
  Initialize post processing files writing.
  """
  global DIMENSION
  if DIMENSION == 2:
    postpro_PostproBeforeComputation( restart )
  elif DIMENSION == 3:
    postpro_3D_PostproBeforeComputation( restart )
  else :
    print( '[ERROR:OpenPostproFiles] must be called after SetDimension' )
    raise ValueError

def CloseDisplayFiles():
  """Used to close containers of visualization files (*.pvd).
     But no need anymore
  """
  pass

def WritePostproFiles(force_flush=False):
  """Write post processing files (POSTPRO/*.OUT).
  """
  global DIMENSION
  if DIMENSION == 2:
    postpro_PostproDuringComputation()
    if force_flush:
        postpro_FlushDuringComputation()
  elif DIMENSION == 3:
    postpro_3D_PostproDuringComputation()
    if force_flush:
        postpro_3D_FlushDuringComputation()
  else :
    print( '[ERROR:WritePostproFiles] must be called after SetDimension' )
    raise ValueError

def ClosePostproFiles():
  """Close post processing files.
  """
  global DIMENSION
  if DIMENSION == 2:
    postpro_ClosePostproFiles()
  elif DIMENSION == 3:
    postpro_3D_ClosePostproFiles()
  else :
    print( '[ERROR:ClosePostproFiles] must be called after SetDimension' )
    raise ValueError

def WriteOutGPV(nsteps=1):
  """Write current Gauss points values of all meshes (OUTBOX/GPV.x.OUT)
  if current time step number is a multiple of nsteps.
  """
  TimeEvolution_WriteOutGPV(nsteps)
  MAILx_WriteOutGPV()

def WriteOutMpValues(nsteps=1):
  """Write multi-physics data at current time step in ascii file(s).
  """
  global DIMENSION
  TimeEvolution_WriteOutMpValues(nsteps)
  if DIMENSION == 2:
    mp_solver_WriteOutMpValues()
  elif DIMENSION == 3:
    mp_solver_3D_WriteOutMpValues()
  else :
    print( '[ERROR:WriteOutMpValues] must be called after SetDimension' )
    raise ValueError

def WriteOutRnod(nsteps=1):
  """Write current values of reaction for each bodies if current time step number
  is a multiple of nsteps.
  """
  global DIMENSION
  TimeEvolution_WriteOutRnod(nsteps)
  if DIMENSION == 2:
    RBDY2_WriteOutRnod()
  elif DIMENSION == 3:
    RBDY3_WriteOutRnod()
  else :
    print( '[ERROR:WriteOutRnod] must be called after SetDimension' )
    raise ValueError
  mecaMAILx_WriteOutRnod()

def WriteOutVlocRloc(nsteps=1):
  """Write values of every interactions if current time step number is a multiple of nsteps.
  """
  global DIMENSION

  # If it is not a step to write Vloc_Rloc, continue
  if ( ( TimeEvolution_GetStep() % nsteps ) != 0 ) :
    return

  TimeEvolution_WriteOutVlocRloc(nsteps)

  if DIMENSION == 2:
    CLALp_WriteOutVlocRloc()
    CLJCx_WriteOutVlocRloc()
    DKALp_WriteOutVlocRloc()
    DKDKL_WriteOutVlocRloc()
    DKDKx_WriteOutVlocRloc()
    DKJCx_WriteOutVlocRloc()
    DKKDx_WriteOutVlocRloc()
    DKPLx_WriteOutVlocRloc()
    P2P2L_WriteOutVlocRloc()
    PLALp_WriteOutVlocRloc()
    PLJCx_WriteOutVlocRloc()
    PLPLx_WriteOutVlocRloc()
    PTPT2_WriteOutVlocRloc()
  elif DIMENSION == 3:
    CDCDx_WriteOutVlocRloc()
    CDPLx_WriteOutVlocRloc()
    CSASp_WriteOutVlocRloc()
    PRASp_WriteOutVlocRloc()
    CSPRx_WriteOutVlocRloc()
    PRPLx_WriteOutVlocRloc()
    PRPRx_WriteOutVlocRloc()
    PTPT3_WriteOutVlocRloc()
    SPCDx_WriteOutVlocRloc()
    SPDCx_WriteOutVlocRloc()
    SPPLx_WriteOutVlocRloc()
    SPPRx_WriteOutVlocRloc()    
    SPSPx_WriteOutVlocRloc()
  else :
    print( '[ERROR:WriteOutVlocRloc] must be called after SetDimension' )
    raise ValueError

def repack_h5file( fname ):
    """
    use h5_repack on a HDF5 file to use free unused disk space
    """

    working_dir = Path(overall_GetWorkingDirectory())

    realname = working_dir/fname
    tmpname  = realname.with_suffix( realname.suffix + '.tmp' )

    subprocess.call(["h5repack", realname, tmpname])
    tmpname.replace( realname )


def InitHDF5( filename='' ) :
  """
  Initialize HDF5 output file (and use h5repack just in case)

  If no filename given or HDF5 has not been used during compilation
  this function is ignored and next calls to 'WriteOut' function
  will write ASCII files in OUTBOX directory
  """
  global use_hdf5

  if not filename:
    use_hdf5 = False
    return

  try:
    io_hdf5_initOutFile( filename )
    repack_h5file( filename )

    use_hdf5 = True

  except NameError:
    use_hdf5 = False
    print( "INFO: You are not able to initialize output file in HDF5 format.\n" + \
           "      To be able to write in HDF5 format, please enable the \n" + \
           "      WITH_HDF5 flag at the compilation time.\n" + \
           "      Example: cmake -DWITH_HDF5=ON .. && make -j8" )
    pass

def WriteHDF5( nsteps = 1 ) :
  """Write HDF5 output.
     - DOF (degree of freedom) in 2D and 3D
     - Vloc_Rloc in 2D and 3D
     - GPV fields
  """

  # If it is not a step to write Vloc_Rloc, continue
  if ( ( TimeEvolution_GetStep() % nsteps ) != 0 ) :
    return

  io_hdf5_write( )

def WriteOut( nsteps = 1 ) :
  """Write Out files.
     If InitHDF5 has been used with a valid filename
     then it will write in the defined file.
     Otherwise the ASCII files in OUTBOX directory will
     be written
  """
  global use_hdf5

  if use_hdf5:
    WriteHDF5(nsteps)
  else:
    WriteOutDof(nsteps)
    if MAILx_GetNbMAILx() > 0:
        WriteOutGPV(nsteps)
    # should check against see_table...
    if tact_behav_GetNbTactBehav() > 0:
        WriteOutVlocRloc(nsteps)

def WriteLast(filename=None) :
  """Write Last files.
     If InitHDF5 has been used with a valid filename
     then it will write in the defined file.
     Otherwise the ASCII files in OUTBOX directory will
     be written
  """
  global use_hdf5

  if use_hdf5 and filename is not None:
    io_hdf5_write_last( filename )
    repack_h5file( filename )
  else:
    WriteLastDof()
    if MAILx_GetNbMAILx() > 0:
        WriteLastGPV()
    # should check against see_table...
    if tact_behav_GetNbTactBehav() > 0:
        WriteLastVlocRloc()


def ReadDatbox(deformable=True):
  """
   Read all .DAT and .INI files from DATBOX directory
   to initialize LMGC90 database.
   """

  utilities_logMes('READ BEHAVIOURS')
  ReadBehaviours()
  if deformable:
      utilities_logMes('READ MODELS')
      ReadModels()

  utilities_logMes('READ BODIES')
  ReadBodies()

  utilities_logMes('READ BEHAVIOURS')
  LoadBehaviours()
  if deformable:
      utilities_logMes('LOAD MODELS')
      LoadModels()

  utilities_logMes('READ INI DOF')
  ReadIniDof()
  #
  if deformable:
      utilities_logMes('READ INI GPV')
      ReadIniGPV()
  #
  utilities_logMes('READ DRIVEN DOF')
  ReadDrivenDof()
  #
  if tact_behav_GetNbTactBehav() > 0:
      utilities_logMes('LOAD TACTORS')
      LoadTactors()
      #
      utilities_logMes('READ INI Vloc Rloc')
      ReadIniVlocRloc()

  #
  # paranoid writes
  #
  utilities_logMes('WRITE BODIES')
  WriteBodies()
  utilities_logMes('WRITE DRIVEN DOF')
  WriteDrivenDof()
  utilities_logMes('WRITE BEHAVIOURS')
  WriteBehaviours()
  if deformable:
      utilities_logMes('WRITE MODELS')
      models_WriteModels()


def ExSolver(stype,norm,conv,relax,gsit1,gsit2):
  """Run Non-linear Gauss Seidel contact solver.

  parameters:

  - stype: type of solver ('Exchange Local Global' or 'Stored_Delassus_Loops').
  - norm: type of norm ('Quad', 'QM/16', 'Maxm').
  - conv: convergence tolerance demanded.
  - relax: relaxation parameter.
  - gsit1: maximum number of convergence check before stopping.
  - gsit2: number of iterations of nlgs before checking convergence.
  """
  global DIMENSION
  if DIMENSION == 2:
    nlgs_ExSolver(stype,norm,conv,relax,gsit1,gsit2)
  elif DIMENSION == 3:
    nlgs_3D_ExSolver(stype,norm,conv,relax,gsit1,gsit2)
  else :
    print( '[ERROR:ExSolver] must be called after SetDimension' )
    raise ValueError

def UpdateTactBehav():
  """Updates internal variables of interactions.

  """
  global DIMENSION
  if DIMENSION == 2:
    nlgs_UpdateTactBehav()
  elif DIMENSION == 3:
    nlgs_3D_UpdateTactBehav()
  else :
    print( '[ERROR:UpdateTactBehav] must be called after SetDimension' )
    raise ValueError


def checkDirectories(checkDATBOX=True):
  """Check if DATBOX directory exists (if not, it stops).
  Create OUTBOX, POSTPRO and/or DISPLAY directory if one
  does not exists.
  """
  working_dir = Path(overall_GetWorkingDirectory())
  datbox = working_dir/'DATBOX'
  if( checkDATBOX and not datbox.is_dir() ):
    print('DATBOX directory is not present')
    sys.exit(1)
  for d in ['OUTBOX', 'POSTPRO', 'DISPLAY']:
    dp = working_dir/d
    if not dp.is_dir():
      print(f"Creating {d} directory for you...")
      dp.mkdir()


def checkInteractiveCommand(last_name='last.h5'):
  """Check if some specific files exist and react depending on the result:

  - file 'stop': write every possible .OUT files and exit.
  - file 'save': write every possible .LAST files (or an hdf5 file if last_name is provided).
  - file 'display_now': write every possible visualization files.
  - file 'flush': write content of opened postpro files
  """
  global DIMENSION, use_hdf5

  wdp = Path( overall_GetWorkingDirectory() )
  stop_file = wdp/'stop'
  if( stop_file.is_file() ):

    WriteOut(1)
    WriteOutMpValues(1)

    WriteDisplayFiles()
    CloseDisplayFiles()
    WritePostproFiles()
    ClosePostproFiles()
    Finalize()
    stop_file.unlink()
    raise RuntimeError('Forced stop computation')

  save_file = wdp/'save'
  if( save_file.is_file() ):

    WriteLast(last_name)
    WriteLastMpValues()

    WritePostproFiles()
    save_file.unlink()

  disp_file = wdp/'display_now'
  if( disp_file.is_file() ):
    WriteDisplayFiles()
    disp_file.unlink()

  flush_file = wdp/'flush'
  if( flush_file.is_file() ):
    if DIMENSION == 2:
      postpro_FlushDuringComputation()
    elif DIMENSION == 3:
      postpro_3D_FlushDuringComputation()
    flush_file.unlink()


def getInteractions(this=False, human=True):
  """
  Get all interactions as a numpy array with predefined dtype.

  Must be called after: 'Intialize', 'SetDimension' and 'ReadBehaviours'

  By default, get verlet interactions, to get 'this' interaction, change
  the eponym parameter to True.

  By default, remap parameters id to name and contact law id to its
  corresponding name. To keep integer values change 'human' argument
  to False.
  """
  global DIMENSION, get_mdl, get_tac, get_sta, get_int, get_law, inter_dtype, inter_itype

  dim = DIMENSION

  max_int = overall_GetMaxInternalTact()

  if dim == 2:
      rsize = 11
      if this:
          get_nb    = inter_handler_2D_tgetNb
          get_idata = inter_handler_2D_tgetIData
          get_rdata = inter_handler_2D_tgetRData
          get_intern= inter_handler_2D_tgetInternal
      else:
          get_nb    = inter_handler_2D_getNb
          get_idata = inter_handler_2D_getAllIdata
          get_rdata = inter_handler_2D_getAll
          get_intern= inter_handler_2D_getAllInternal
  elif dim == 3:
      rsize = 19
      if this:
          get_nb    = inter_handler_3D_tgetNb
          get_idata = inter_handler_3D_tgetIData
          get_rdata = inter_handler_3D_tgetRData
          get_intern= inter_handler_3D_tgetInternal
      else:
          get_nb    = inter_handler_3D_getNb
          get_idata = inter_handler_3D_getAllIdata
          get_rdata = inter_handler_3D_getAll
          get_intern= inter_handler_3D_getAllInternal

  else :
      print( '[ERROR:getInteraction] must be called after SetDimension' )
      raise ValueError

  isize = 13

  nb_inter = sum( ( get_nb(inter) for inter in inters_list ) )

  if human:
      i2int = get_int
      i2mdl = get_mdl
      i2tac = get_tac
      i2law = get_law
      i2sta = get_sta
      inters = np.zeros( nb_inter, dtype=inter_dtype )
  else:
      i2int = i2mdl = i2tac = i2law = i2sta = lambda i: i
      inters = np.zeros( nb_inter, dtype=inter_itype )

  iid = 0
  for inter_id in inters_list:

      nb_i  = get_nb(inter_id)

      if nb_i == 0:
          continue

      if this:
          idata     = np.empty( [nb_i,  isize], dtype=int  )
          rdata     = np.empty( [nb_i,  rsize], dtype=float)
          internals = np.empty( [nb_i,max_int], dtype=float)
          for iinter in range(nb_i):
            idata[iinter]     = get_idata(inter_id , iinter+1)
            rdata[iinter]     = get_rdata(inter_id , iinter+1)
            internals[iinter] = get_intern(inter_id, iinter+1)
      else:
          idata     = get_idata(inter_id)
          rdata     = get_rdata(inter_id)
          internals = get_intern(inter_id)

      inters[iid:iid+nb_i][ 'inter']  = i2int(inter_id)
      inters[iid:iid+nb_i][ 'icdan']  = range(1,nb_i+1)

      inters[iid:iid+nb_i][ 'cdbdy']  = i2mdl(idata[:,0])
      inters[iid:iid+nb_i][ 'anbdy']  = i2mdl(idata[:,1])
      inters[iid:iid+nb_i]['icdbdy']  = idata[:, 2]
      inters[iid:iid+nb_i]['ianbdy']  = idata[:, 3]
      inters[iid:iid+nb_i][ 'cdtac']  = i2tac(idata[:,4])
      inters[iid:iid+nb_i][ 'antac']  = i2tac(idata[:,5])
      inters[iid:iid+nb_i]['icdtac']  = idata[:, 6]
      inters[iid:iid+nb_i]['iantac']  = idata[:, 7]
      inters[iid:iid+nb_i]['icdsci']  = idata[:, 8]
      inters[iid:iid+nb_i]['iansci']  = idata[:, 9]
      inters[iid:iid+nb_i][ 'behav']  = i2law(idata[:,10])
      inters[iid:iid+nb_i]['status']  = i2sta(idata[:,11])
      inters[iid:iid+nb_i]['nb_int']  = idata[:,12]

      idx = 0
      inters[iid:iid+nb_i]['coor']    = rdata[:, idx:idx+dim]
      idx = idx+dim
      inters['uc'][iid:iid+nb_i,:,0] = rdata[:, idx:idx+dim]
      idx = idx+dim
      inters['uc'][iid:iid+nb_i,:,1] = rdata[:, idx:idx+dim]
      idx = idx+dim
      if dim == 3:
        inters['uc'][iid:iid+nb_i,:,2] = rdata[:,idx:idx+dim]
        idx = idx+dim
      inters[iid:iid+nb_i]['rl']      = rdata[:,idx:idx+dim]
      idx = idx+dim
      inters[iid:iid+nb_i]['vl']      = rdata[:,idx:idx+dim]
      idx = idx+dim
      inters[iid:iid+nb_i]['gapTT']   = rdata[:,idx]
      idx = idx+1

      inters[iid:iid+nb_i]['internals'] = internals
      iid += nb_i

  if this:
      inters['rl'] /= TimeEvolution_GetTimeStep()

  return inters


def registerInterInternals(fields):
    """Register an interaction internal fields.

    Store a mapping function (numpy vectorized) allowing
    to get for a given contact law name, the corresponding
    index (starting with 0) in the internals array.

    If there is no contact law using the input fields,
    the field is still registered but nothing will be done.

    Must be called after: ReadBehaviours.

    Parameters
    ----------
    fields : string or list of string
             The name(s) of the internal(s) to register.
    """
    global inter_mapper

    # if single field convert to list anyway
    fields = fields if isinstance(fields, list) else [fields,]

    # register all fields
    for field in fields:
        law2internal = {}
        for ilaw in range(1, tact_behav_GetNbTactBehav()+1):
          law_type, law_name, law_params = tact_behav_GetTactBehav(ilaw)
          law = law_name.encode()
          internal_names = tact_behav_GetInternalComment(ilaw)
          internal_names = internal_names.split()
          # in case field is not available, use -1, which is accessible but identifiable as wrong
          law2internal[law] = internal_names.index(field) if field in internal_names else -1

        # if the field is not available for any law, do not create mapper
        if np.any( np.array(law2internal.values()) != -1 ):
          l2i_index = np.vectorize( law2internal.get, otypes=[int] )
        else:
          l2i_index = None

        inter_mapper[field] = l2i_index

# so ugly...
if np.__version__ > '1.15.0':
  def _take_along_axis(arr, idx):
    new_arr = np.take_along_axis(arr, idx[:,np.newaxis], axis=1)
    return new_arr[:,0]
else:
  def _take_along_axis(arr, idx):
    return arr[ np.arange(idx.size), idx ]


def getInternalArray(field, inters, default=0.):
    """Get the internal field as an array.

    If the field as not been registered with `registerInterInternals`
    beforehand, it is automatically registered.

    A default value is needed because in case of mixed contact laws,
    the desired field may not be defined everywhere.

    Parameters
    ----------
    field : string
            The name of the internal field to get
    inters : numpy array
             The interaction numpy array from which to extract internal
    default : float
              The default value when the internal field is undefined (0.)
    """
    global get_law, inter_mapper

    # first get the mapper...
    if field not in inter_mapper.keys():
        registerInterInternals(field)

    mapper = inter_mapper[field]
    # managing trivial case
    if not mapper:
      return np.zeros([inters.size], dtype=float) + default

    # the mapper always use the byte string, so if the input
    # inters array provided the id, remapt first:
    if inters.dtype['behav'].kind != 'S':
      behav = get_law( inters['behav'] )
    else:
      behav = inters['behav']

    # map behav to the index to get
    field_idx =  mapper(behav)
    # extract correct field, if not exist, last value is taken
    internal = _take_along_axis(inters['internals'],field_idx)
    # overwrite not available indices with default value
    internal[field_idx<0] = default

    return internal

def addRegistersToDisplay(to_add=True):
    """Decide to automatically add registered field to WriteDisplayFiles.

    Parameters
    ----------
    to_add : boolean
             To enable (default) or disable.
    """
    global registers2display
    registers2display = to_add

def setInternalArray(field, inters, internals):
    """Set the internal field in 'this' interactions.

    If the field as not been registered with `registerInterInternals`
    beforehand, it is automatically registered.

    Can handle interactions with 'human' or 'compute id' dtype.

    Parameters
    ----------
    field : string
            The name of the internal field to get
    inters : numpy array
             The interaction numpy array for which to set the internal
    internals : numpy array
                The float array with value of internals (same size than inters)
    """
    global DIMENSION, int_id, inter_mapper

    # first get the mapper...
    if field not in inter_mapper.keys():
        registerInterInternals(field)
    mapper = inter_mapper[field]

    # managing trivial case
    if not mapper:
        return

    # get the correct inter_handler
    if DIMENSION == 2:
      handler = inter_handler_2D_tsetInternal
    elif DIMENSION == 3:
      handler = inter_handler_3D_tsetInternal
    else :
      print( '[ERROR:setInternalArray] must be called after SetDimension' )
      raise ValueError

    # in case a single internal value provided for the array
    if isinstance(internals, (float, int,)):
        internals = itertools.cycle( (internals,) )
    else:
        msg = f"[ERROR:chipy.setInternalArray] non conforming size of inters ({inters.size}) and internals ({internals.size})"
        assert inters.size == internals.size , msg


    # the mapper always use the byte string, so if then input
    # inters array provided the id, remapt first:
    if inters.dtype['behav'].kind != 'S':
      behavs = get_law( inters['behav'] )
    else:
      behavs = inters['behav']

    # inter_id is needed
    if inters.dtype['inter'].kind == 'S':
      inter_ids = int_id(inters['inter'])
    else:
      inter_ids =  inters['inter']

    # internal indices
    int_idx = mapper(behavs)

    big_iterator = zip(inter_ids, inters['icdan'], int_idx, internals)
    for inter_id, inter_idx, idx, value in big_iterator:
      if idx < 0 : continue
      # int conversion is so ugly... do not forget +1 for C/Fortran
      handler(int(inter_id),int(inter_idx),int(idx+1),value)

