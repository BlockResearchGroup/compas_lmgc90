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
dim=2

# creations de deux materiaux
# definition d'un materiau elastique lineaire isotrope :
steel = pre.material(name='stone', materialType='ELAS', elas='standard',
                     young=7.e10, nu=0.2, anisotropy='isotropic', density=2750.)  
# ajout du materiau
mats.addMaterial(steel)

# definition du modele elastique :
m2Dl = pre.model(name='M2D_L', physics='MECAx', element='Q4xxx', dimension=dim, external_model='MatL_',
                 kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
# ajout du modele
mods.addModel(m2Dl)

# construction du bloc du dessous

# on genere le maillage du bloc
mesh_block = pre.buildMesh2D(mesh_type='Q4', x0=0., y0=0., lx=0.15, ly=0.05, nb_elem_x=15, nb_elem_y=5)
# on contruit un corps maille a partir du maillage du bloc
body = pre.buildMeshedAvatar(mesh=mesh_block, model=m2Dl, material=steel)
# on eclate le corps maillae
new_bodies = pre.explodeMeshedAvatar2D(body, nbPoints=2, color='BLEUx', w=[0.25,0.75])

# pour chaque corps resultant
for new_body in new_bodies:
    # la paroi basse est encastree
    if new_body.hasGroup('down'):
       new_body.imposeDrivenDof(group='down', component=2, dofty='vlocy')
    # on pose des contacteurs antagonistes sur le dessus du bloc
    if new_body.hasGroup('up'):
       new_body.addContactors(group='up', shape='ALpxx', color='REDxx')

# ajout les nouveaux corps generes a la liste des corps
bodies += new_bodies

# construction du bloc du dessus

# on genere le maillage du bloc
mesh_block = pre.buildMesh2D('Q4', x0=0.025, y0=0.05, lx=0.10, ly=0.05, nb_elem_x=10, nb_elem_y=5)
# on contruit un corps maille a partir du maillage du bloc
body = pre.buildMeshedAvatar(mesh=mesh_block, model=m2Dl, material=steel)
# on eclate le corp maille
new_bodies = pre.explodeMeshedAvatar2D(body, nbPoints=2, color='BLEUx')

# pour chaque corps resultant
for new_body in new_bodies:
    # on pose des contacteurs candidats sur le dessous du bloc
    if new_body.hasGroup('down'):
       new_body.addContactors(group='down', shape='CLxxx', color='REDxx')

# ajout des nouveaux corps a la liste des corps
bodies += new_bodies

# gestion des interactions :
#   * declaration de la loi de contact entre les blocs
lclalpE = pre.tact_behav(name='gapc0',law='GAP_SGR_CLB',fric=0.3)
tacts  += lclalpE
#   * declaration d'une loi cohesive entre les elements
lclalpI = pre.tact_behav(name='macc0',law='MAC_CZM',dyfr=0.3,stfr=0.3,cn=1.e+13,
                         ct=1.e+13,b=0.,w=0.1)
tacts  += lclalpI

#   * declaration de la table de visibilite entre les blocs
svclalpE = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='REDxx',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx',colorAntagoniste='REDxx',
                         behav=lclalpE,alert=0.001)
svs += svclalpE
#   * declaration de la table de visibilite entre les elements
svclalpI = pre.see_table(CorpsCandidat='MAILx', candidat='CLxxx', colorCandidat='BLEUx',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='BLEUx',
                         behav=lclalpI, alert=0.001)
svs+= svclalpI

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

try:
  pre.visuAvatars(bodies)
except:
  pass
