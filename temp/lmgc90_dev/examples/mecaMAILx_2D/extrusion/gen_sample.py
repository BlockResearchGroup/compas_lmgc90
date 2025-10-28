import os,sys
import math
from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

dim = 2

# definition du conteneur de partie ou de pieces, des modeles et des materiaux
ps = pre.avatars()
ms = pre.models()
mx = pre.materials()
svs = pre.see_tables()
tacts = pre.tact_behavs()

# definition des modeles 
m_out = pre.model(name='T3MLx', physics='MECAx', element='T3xxx', dimension=dim, external_model='MatL_',kinematic='large',
                  formulation='TotaL',material='neoh_',anisotropy='iso__',mass_storage='lump_')
m_lop = pre.model(name='Q4MNL',physics='MECAx',element='Q4xxx', dimension=dim, external_model='MatL_',kinematic='large',
                  formulation='TotaL',material='neoh_',anisotropy='iso__',mass_storage='lump_')
ms.addModel(m_out,m_lop)

# Definition des materiaux
acier = pre.material(name='acier',materialType='ELAS',elas='standard',
                     young=200000.,nu=0.3,anisotropy='isotropic',
                     density=0.)
acie1 = pre.material(name='acie1',materialType='ELAS',elas='standard',
                     young=200000.,nu=0.3,anisotropy='isotropic',
                     density=0.)
mx.addMaterial(acier,acie1)

# definition des parties maillees

# on lit le maillage de l'outil dans le fichier
mesh_outil = pre.readMesh('gmsh/outil.msh',dim)
# on construit un avatar deformable pour l'outil
outil = pre.buildMeshedAvatar(mesh=mesh_outil, model=m_out, material=acier)

# on lit le maillage du lopin dans le fichier
mesh_lopin = pre.readMesh('gmsh/lopin_v2.msh',dim)
# on construit un avatar deformable pour le lopin
lopin = pre.buildMeshedAvatar(mesh=mesh_lopin, model=m_lop, material=acie1)

# what's this ? it is in read function of mesh
#avatar.bulks.setAdjacBulk2Nodes(avatar.nodes)

## @todo : ben ouais... encore plus de boulot
# on positionne l'outil et le lopin
outil.rotate(psi=-0.5*math.pi)
lopin.rotate(psi=-0.5*math.pi)

# predicat
def p(x):
   return x[0] <= 65.
# creation d'un nouveau groupe
outil.addGroupUsingPredicate(name='relax', predicate=p, super_group='12')

# Definition des contacteurs
outil.addContactors(group='11',shape='ALpxx',color='BLUEx',reverse=True)
lopin.addContactors(group='21',shape='CLxxx',color='REDxx',reverse=True)

## Application des conditions initiales
outil.imposeInitValue(group='all',component=[1,2],value=[0.,0.])
lopin.imposeInitValue(group='all',component=[1,2],value=[0.,0.])

# Application des conditions aux limites
# base
outil.imposeDrivenDof(group='12',component=2,dofty='vlocy')
outil.relaxDrivenDof(group='relax',component=2)
# cote externe
outil.imposeDrivenDof(group='13',component=[1, 2],dofty='vlocy')

# axe
lopin.imposeDrivenDof(group='23',component=1,dofty='vlocy')

# top
lopin.imposeDrivenDof(group='22',component=2,ct=-10.,dofty='vlocy')

# Ajout des parties dans le conteneur de parties
ps.addAvatar(outil)
ps.addAvatar(lopin)

# Definition des interactions et des tables de visibilites
#...interaction
b = pre.tact_behav(name='gapc0',law='GAP_SGR_CLB',fric=0.3)
tacts+=b
#.. table de visibilite
sv = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx',colorCandidat='REDxx',
                   CorpsAntagoniste='MAILx',antagoniste='ALpxx',colorAntagoniste='BLUEx',
                   behav=b,alert=.1)
svs += sv

post = pre.postpro_commands()

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mx, ms, ps, tacts, svs, post=post)

try:
  pre.visuAvatars(ps)
except:
  pass
