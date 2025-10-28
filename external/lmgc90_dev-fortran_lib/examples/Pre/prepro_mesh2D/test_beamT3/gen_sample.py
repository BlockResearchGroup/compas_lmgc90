import os,sys

from pylmgc90 import pre

# creation des conteneurs
#   * pour les corps
bodies = pre.avatars()
#   * pour les materiaux
mat = pre.materials()
#   * pour les modeles
mod = pre.models()

# on se place en 2D
dim=2

# creation du materiau :
# definition d'un materiau thermo-elastique
steel = pre.material(name='steel', materialType='THERMO_ELAS', density=8.e3, elas='standard',
                     young=2.e11, nu=0.3, anisotropy='isotropic', dilatation=2.e-7, 
                     T_ref_meca=273., specific_capacity=1.125, conductivity=300.) 
# ajout du materiau
mat.addMaterial(steel)

# definition du modele thermoelastique :
t2Dl = pre.model(name='T2D_L', physics='THERx', element='T3xxx', dimension=dim,
                 external_model='no___', formulation='class', capacity_storage='coher')
# ajout du modele
mod.addModel(t2Dl)

# construction de la barre

# on genere le maillage de la barre
mesh_rod = pre.buildMesh2D('2T3', x0=0., y0=0., lx=0.1, ly=0.01, nb_elem_x=10, nb_elem_y=1)
# on contruit un corps maille a partir du maillage de la barre
body = pre.buildMeshedAvatar(mesh=mesh_rod, model=t2Dl, material=steel)

# conditions aux limites :
#   * T=20 degres C, sur le bord gauche (en x=0)
body.imposeDrivenDof(group='left', component=1, dofty='temp',
   ct=293.)
#body.imposeDrivenDof(group='left', component=[1, 2], dofty='flux',
#   ct=293.)
#   * T=20 degres C, sur le bord droit (en x=L)
body.imposeDrivenDof(group='right', component=1, dofty='temp',
   ct=313.)

# condition initiale : T=20 degres C
body.imposeInitValue(group='all', component=1, value=293.)

# ajout du corps a la liste des corps
bodies += body

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(dim, mat, mod, bodies, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
