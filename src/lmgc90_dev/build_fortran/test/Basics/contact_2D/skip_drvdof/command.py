import itertools
# importing chipy module
from pylmgc90 import chipy
from pylmgc90.chipy import computation

dim = 2
mhyp = 1

dt = 1e-5
theta = 0.505

tol = 1e-5
relax = 1.0
norm = 'QM/16'
gs_it1 = 50
gs_it2 = 10
solver_type='Stored_Delassus_Loops         '

freq_write   = 1
freq_display = 1

computation.initialize(dim, dt, theta, mhyp, deformable=True, logmes=True)

try:
  # number of CLALp contacts when all drv_dof are fixed (should be equal to 0)
  computation.one_step(solver_type, norm, tol, relax, gs_it1, gs_it2, freq_write, freq_display)
  nb_clalp = chipy.inter_handler_2D_getNb(chipy.CLALp_ID)
  assert nb_clalp == 0 , f"nb CLALp contacts {nb_clalp} != 0"
  nb_dkdkx = chipy.inter_handler_2D_getNb(chipy.DKDKx_ID)
  assert nb_dkdkx == 0, f"nb DKDKx contacts {nb_spspx} /= 0"

  # number of CLALp contacts when some drv_dof are set invisible (shouldn't be equal to 0)
  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(2,5,1)
  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(2,5,2)
  chipy.mecaMAILx_UpdateVlocyDrivenDofStructures(2)
  chipy.AssembleMechanicalLHS()
  # Relax a dof of a one DISKx to allow contact
  chipy.RBDY2_SetInvisibleVlocyDrivenDof(1,1)
  computation.one_step(solver_type, norm, tol, relax, gs_it1, gs_it2, freq_write, freq_display)
  nb_clalp = chipy.inter_handler_2D_getNb(chipy.CLALp_ID)
  assert nb_clalp == 2 , f"nb CLALp contacts {nb_clalp} != 2"
  nb_dkdkx = chipy.inter_handler_2D_getNb(chipy.DKDKx_ID)
  assert nb_dkdkx == 1, f"nb DKDKx contacts {nb_spspx} /= 1"

  # number of CLALp contacts when some drv_dof are set invisible (shouldn't be equal to 0)
  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(3,3,1)
  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(3,3,2)
  chipy.mecaMAILx_UpdateVlocyDrivenDofStructures(3)
  chipy.AssembleMechanicalLHS()
  computation.one_step(solver_type, norm, tol, relax, gs_it1, gs_it2, freq_write, freq_display)
  nb_clalp = chipy.inter_handler_2D_getNb(chipy.CLALp_ID)
  assert nb_clalp == 2 , f"nb CLALp contacts {nb_clalp} != 2"

  # number of CLALp contacts when some drv_dof are set invisible (shouldn't be equal to 0)
  chipy.mecaMAILx_SetVisibleVlocyDrivenDof(3,3,1)
  chipy.mecaMAILx_SetVisibleVlocyDrivenDof(3,3,2)
  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(3,1,1)
  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(3,1,2)
  chipy.mecaMAILx_UpdateVlocyDrivenDofStructures(3)
  chipy.AssembleMechanicalLHS()
  computation.one_step(solver_type, norm, tol, relax, gs_it1, gs_it2, freq_write, freq_display)
  nb_clalp = chipy.inter_handler_2D_getNb(chipy.CLALp_ID)
  assert nb_clalp == 3 , f"nb CLALp contacts {nb_clalp} != 3"

  # number of CLALp contacts when some drv_dof are set invisible (shouldn't be equal to 0)
  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(1,2,1)
  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(1,2,2)
  chipy.mecaMAILx_UpdateVlocyDrivenDofStructures(1)
  chipy.AssembleMechanicalLHS()
  computation.one_step(solver_type, norm, tol, relax, gs_it1, gs_it2, freq_write, freq_display)
  nb_clalp = chipy.inter_handler_2D_getNb(chipy.CLALp_ID)
  assert nb_clalp == 4 , f"nb CLALp contacts {nb_clalp} != 4"

except Exception as err:

  computation.finalize()
  raise err

