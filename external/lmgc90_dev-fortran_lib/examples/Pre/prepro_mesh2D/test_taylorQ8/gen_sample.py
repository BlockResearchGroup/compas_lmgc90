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

# creation du materiau
# definition d'un materiau elasto-plastique, avec un critere de plasticite de
# Von-Mises et un ecrouissage lineaire :
steel = pre.material(name='steel', materialType='ELAS_PLAS', density=8.93e3, elas='standard',
                     anisotropy='isotropic', young=1.17e11, nu=0.35, critere='Von-Mises', 
                     isoh='linear', iso_hard=4.e8, isoh_coeff=1e8, cinh='none', visc='none')  
# ajout du materiau
mat.addMaterial(steel)

# definition du modele elastoplastique :
m2Dnl = pre.model(name='M2DNL', physics='MECAx', element='Q8Rxx', dimension=2, 
                  external_model='MatL_', kinematic='large', formulation='TotaL',
     material='J2iso', anisotropy='iso__', mass_storage='coher')
# ajout du modele
mod.addModel(m2Dnl)

# construction de la barre

# on genere le maillage de la barre
mesh_rod = pre.buildMesh2D(mesh_type='Q8', x0=0., y0=0., lx=3.2e-3, ly=32.4e-3, nb_elem_x=4, nb_elem_y=12)
# on contruit un corps maille a partir du maillage de la barre
body = pre.buildMeshedAvatar(mesh=mesh_rod, model=m2Dnl, material=steel)

# conditions aux limites :
#   * condition de symetrie
body.imposeDrivenDof(group='left', component=1, dofty='vlocy')
#   * la barre ne peut que s'etaler sur la fondation
body.imposeDrivenDof(group='down', component=2, dofty='vlocy')

# condition initiale
body.imposeInitValue(group='all', component=2, value=-227.)

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

