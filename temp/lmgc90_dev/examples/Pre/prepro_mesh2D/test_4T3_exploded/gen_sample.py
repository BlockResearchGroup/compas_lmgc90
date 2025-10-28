import os,sys

from pylmgc90 import pre

# creation des conteneurs
#   * pour les corps
bodies = pre.avatars()
#   * pour les materiaux
mats   = pre.materials()
#   * pour les modeles
mods   = pre.models()
#   * pour les tables de visibilite
svs    = pre.see_tables()
#   * pour les lois de contact
tacts  = pre.tact_behavs()

# on se place en 2D
dim = 2

# creations du materiau
# definition d'un materiau elastique lineaire isotrope :
steel = pre.material(name='stone', materialType='ELAS', elas='standard',
                     young=2.05e11, nu=0.3, anisotropy='isotropic', density=7850.)  
# ajout du materiau
mats.addMaterial(steel)

# definition du modele elastique :
m2Dl = pre.model(name='M2D_L', physics='MECAx', element='T3xxx', dimension=dim, external_model='MatL_',
                 kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
# ajout du modele
mods.addModel(m2Dl)

# definition de la geometrie du rectangle
x0=0.
y0=0.
lx=0.1
ly=0.2

# definition du nombre d'elements suivant chaque direction
nb_elem_x=10
nb_elem_y=20

# on construit un maillage en T3, obtenus en coupant en quatre des Q4
mesh_block = pre.buildMesh2D(mesh_type='4T3', x0=x0, y0=y0, lx=lx, ly=ly, nb_elem_x=nb_elem_x, nb_elem_y=nb_elem_y)
# on construit un corps maille a partir du maillage
body = pre.buildMeshedAvatar(mesh=mesh_block, model=m2Dl, material=steel)
# on eclate le corps maille
new_bodies = pre.explodeMeshedAvatar2D(body=body, nbPoints=2, color='BLEUx', w=[0.25,0.75])

# pour chaque corps resultant
for new_body in new_bodies:
    # on pose les conditions limites :
    #   * paroi gauche encastree
    if new_body.hasGroup('left'):
       new_body.imposeDrivenDof(group='left',component=[1, 2],dofty='vlocy')
    #   * paroi droite avec vitesse imposee suivant x
    if new_body.hasGroup('right'):
       new_body.imposeDrivenDof(group='right',component=1,ct=10.,dofty='vlocy')

# on ajoute les nouveaux corps generes au conteneur de corps
bodies += new_bodies

# gestion des interactions :
#   * declaration de la loi entre de contact entre les blocs
lclalp = pre.tact_behav(name='macc0',law='MAC_CZM',dyfr=0.3,stfr=0.3,cn=1.e+13,
                        ct=1.e+13,b=0.,w=0.1)
tacts += lclalp
#   * declaration de la table de visibilite entre les blocs
svclalp = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx',colorCandidat='BLEUx',
                        CorpsAntagoniste='MAILx',antagoniste='ALpxx',colorAntagoniste='BLEUx',
                        behav=lclalp, alert=1.e-3, halo=7.e-2)
svs += svclalp

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
