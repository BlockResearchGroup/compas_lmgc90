import itertools
# importing chipy module
from pylmgc90 import chipy
from pylmgc90.chipy import computation

dim = 3
mhyp = 0

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

  computation.one_step(solver_type, norm, tol, relax, gs_it1, gs_it2, freq_write, freq_display)
  nb_csasp = chipy.inter_handler_3D_getNb(chipy.CSASp_ID)
  assert nb_csasp == 0, f"nb CSASp contacts {nb_csasp} /= 0"
  nb_spspx = chipy.inter_handler_3D_getNb(chipy.SPSPx_ID)
  assert nb_spspx == 0, f"nb SPSPx contacts {nb_spspx} /= 0"

  # Relax a dof of a one SPHER to allow contact
  chipy.RBDY3_SetInvisibleVlocyDrivenDof(1,1)
  # Relax a dof of a corner of the AS to allow one contact
  #for i_node, i_dof in itertools.product( range(19,28), range(1,4) ):
  #  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(1,i_node,i_dof)
  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(1,19,3)
  chipy.mecaMAILx_UpdateVlocyDrivenDofStructures(1)
  chipy.AssembleMechanicalLHS()
  computation.one_step(solver_type, norm, tol, relax, gs_it1, gs_it2, freq_write, freq_display)
  
  nb_csasp = chipy.inter_handler_3D_getNb(chipy.CSASp_ID)
  assert nb_csasp == 1, f"nb CSASp contacts {nb_csasp} /= 1"
  nb_spspx = chipy.inter_handler_3D_getNb(chipy.SPSPx_ID)
  assert nb_spspx == 1, f"nb SPSPx contacts {nb_spspx} /= 1"
  
  # Relax a dof of another corner of the CS to allow more contacts
  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(2,4,3)
  chipy.mecaMAILx_UpdateVlocyDrivenDofStructures(2)
  chipy.AssembleMechanicalLHS()
  computation.one_step(solver_type, norm, tol, relax, gs_it1, gs_it2, freq_write, freq_display)

  nb_csasp = chipy.inter_handler_3D_getNb(chipy.CSASp_ID)
  assert nb_csasp == 2, f"nb CSASp contacts {nb_csasp} /= 2"
  
  # Relax a dof of the CSpx0
  chipy.mecaMAILx_SetInvisibleVlocyDrivenDof(4,1,3)
  chipy.mecaMAILx_UpdateVlocyDrivenDofStructures(4)
  chipy.AssembleMechanicalLHS()
  computation.one_step(solver_type, norm, tol, relax, gs_it1, gs_it2, freq_write, freq_display)

  nb_csasp = chipy.inter_handler_3D_getNb(chipy.CSASp_ID)
  assert nb_csasp == 3, f"nb CSASp contacts {nb_csasp} /= 3"

except Exception as err:

  computation.finalize()
  raise err

