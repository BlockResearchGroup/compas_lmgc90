import sys
from pathlib import Path

from pylmgc90 import pre

datbox_path = Path('DATBOX')
datbox_path.mkdir(exist_ok=True)

# on se place en 2D
dim = 2

# creeation des conteneurs
#   * pour les corps
bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

tdurx = pre.material(name='TDURx', materialType='THERMO_RIGID', density=7800.,
                     anisotropy='isotropic', thermal_conductivity=3.9e+02,
                     specific_heat=3.8e+02, thermal_young=1.24e+09, thermal_nu=0.3e+00)
steel = pre.material(name='PLEXx', materialType='THERMO_RIGID', density=7800.,
                     anisotropy='isotropic', thermal_conductivity=0.e+02,
                     specific_heat=3.8e+02, thermal_young=1.24e+09, thermal_nu=0.3e+00)
mats.addMaterial(tdurx,steel)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(mod)

# Rayon tambour
Rdrum = 0.225

# ajout du tambour
drum = pre.rigidDisk(r=Rdrum, center=[Rdrum,Rdrum], model=mod, material=tdurx, color='BLEUx', is_Hollow=True)

# on ajoute le tambour a la liste des corps
bodies += drum

nb_particles = 10000

rik = 3.3e-3

radii = pre.granulo_Random(nb_particles, rik*0.9, rik*1.1, seed=0)

radius_min = min(radii)
radius_max = max(radii)

nb_remaining_particles, coor, radii = pre.depositInDrum2D(radii, Rdrum)

if (nb_remaining_particles < nb_particles):
   print( "Warning: granulometry 1 changed, since some particles were removed!")

for r, c in zip(radii, coor):
   body = pre.rigidDisk(r=r, center=c, model=mod, material=steel, color='STEEL') 
   bodies += body

if '--with-visu' in sys.argv:
  try:
    pre.visuAvatars(bodies)
  except:
    pass

drum.imposeDrivenDof(component=[1, 2], dofty='vlocy')
drum.imposeDrivenDof(component=3     , dofty='vlocy', ct=0.6283184, rampi=10.)

LawSteelSteel = pre.tact_behav(name='istst', law='RST_CLB', rstn=0.4, rstt=0., fric=0.3)
tacts+=LawSteelSteel

LawSteelDrum  = pre.tact_behav(name='istdr', law='RST_CLB', rstn=0.4, rstt=0., fric=0.9)
tacts+=LawSteelDrum

svSTST = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='STEEL',
                       CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='STEEL',
                       behav=LawSteelSteel, alert=0.)
svs+=svSTST

svSTDR = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='STEEL',
                       CorpsAntagoniste='RBDY2', antagoniste='xKSID', colorAntagoniste='BLEUx',
                       behav=LawSteelDrum, alert=0.)
svs+=svSTDR

#
# Post
#
post = pre.postpro_commands()
solv_info = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(solv_info)


# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

#
# missing MP_DEM
#
with open('./DATBOX/MP_DEM.DAT','w') as ofile:
    ofile.write('$model  therm:\n')
    ofile.write('              T0___=%14.7e\n' % 0.23e+02)
    ofile.write('              alert:%14.7e\n' % 0.5e-06)
    ofile.write('              ldiff: Cylnd discrete\n')
    ofile.write('              lconv: no\n')
    ofile.write('              lkine: all\n')
    ofile.write('              bound: adia\n')

