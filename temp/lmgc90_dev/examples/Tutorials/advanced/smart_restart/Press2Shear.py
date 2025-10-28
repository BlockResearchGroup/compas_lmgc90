import os, shutil

from pylmgc90 import pre

dim = 2
mats, mods, bodies, tacts, sees, inters = pre.readDatbox(2, 'Press/DATBOX')
pre.readState(bodies, 'Press/OUTBOX', -1, 'Press/lmgc90.h5', tacts)
#pre.readState(bodies, 'Press/OUTBOX', 4)

# look for wall avatars:
walls = []
for b in bodies:
   if b.contactors[0].color == 'WALLx':
       walls.append(b)

# adding boundary conditions in velocity
# without changing the existing previous ones on down and up
walls[1].imposeDrivenDof(component=1, dofty='vlocy',ct=1.,rampi=1.)

# change rstn and rstt parameters of contact laws
for k, v in tacts.items():
  print( 'updating friction parameters of law', k )
  v.fric = 0.2

# reset materials to use MP ones
mats = pre.materials()


# creating new directory in which to write DATBOX
old_dbox_path = os.path.join(os.getcwd(),'Press','DATBOX')
datbox_path   = os.path.join(os.getcwd(),'Shear','DATBOX')
if not os.path.isdir( datbox_path ):
    os.makedirs( datbox_path )


# write DATBOX
post = pre.postpro_commands()
pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, post=post, datbox_path=datbox_path, gravy=[0.,0.,0.])

import MP_mat
import MP_mod

#MP_mod.write_thermal_model(T0=0.23e+2,alert=rmax,ldiff='Cylnd',gdiff='discrete',lconv='no',lkine='all',bound='1D__')

rmax=0.5
lx = 50*rmax

MP_mod.write_thermal_model(T0=0.23e+2,alert=rmax,ldiff='Cylnd',gdiff='discrete',lconv='no',lkine='all',bound='adia',PATH=datbox_path)

#MP_mod.write_thermal_source(first=down.number+1,last=down.number+1,T0=100.)

#MP_mod.write_thermal_bounds(first=up.number+1,last=up.number+1,T0=1000.,thickness=4*lx,length=2*lx,alpha=1e-1,locus='U')

# wall
MP_mat.write_thermal_material(name='TDURx',materialType='THERMO_RIGID',density=7800.,
                   anisotropy='isotropic',thermal_conductivity=3.9e+02,specific_heat=3.8e+02,
                   thermal_young=1.24e+09,thermal_nu=0.3e+00,PATH=datbox_path)
# particles
MP_mat.write_thermal_material(name='PLEXx',materialType='THERMO_RIGID',density=7800.,
                    anisotropy='isotropic',thermal_conductivity=3.9e+02,specific_heat=3.8e+02,
                    thermal_young=1.24e+09,thermal_nu=0.3e+00,PATH=datbox_path)
