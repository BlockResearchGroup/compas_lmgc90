import sys
from pathlib import Path
import numpy as np

from pylmgc90 import pre

datbox = Path('./DATBOX')
datbox.mkdir(exist_ok=True)

##########################################################
# DIMENSION
##########################################################
dim = 3

##########################################################
# definition du conteneur de partie ou de pieces, de 
# modeles, de materiaux et de commandes de post-traitement
##########################################################
ps     = pre.avatars()
ms     = pre.models()
mx     = pre.materials()
tacts  = pre.tact_behavs()
svs    = pre.see_tables()

##########################################################
# modele élastique, lineaire, hpp (du solide)
##########################################################
mod = pre.model(name='Q8ACi', physics='MECAx', element='TE4xx', 
        dimension=dim,  external_model='MatL_', kinematic='small',
        material='elas_', anisotropy='iso__', mass_storage='lump_')
ms.addModel(mod)

##########################################################
# modele discret (des cable)
##########################################################
mod2 = pre.model(name='SPRIG', physics='MECAx', element='SPRG3',
        dimension=dim,discrete = 'yes__',external_model='no___')
ms.addModel(mod2)

##########################################################
# Definition des materiaux
##########################################################
acier = pre.material(name='acier', materialType='ELAS', elas='standard',
                     young=2.e11, nu=0.3, anisotropy='isotropic',
                     density=7800.)
mx.addMaterial(acier)

ressort = pre.material(name='cable', materialType='DISCRETE',
          masses=np.array([1.,1.,1.]),
          stiffnesses = np.array( [0.,0.,1e8] ),
          viscosities = np.array( [0.,0.,0.] ))
mx.addMaterial(ressort)

##########################################################
# recuperation du maillage
##########################################################
mesh = pre.readMesh('box.msh', dim)

# nombre de noeuds du maillage
nb_nodes = len(mesh.nodes)

# définition des noeuds d'accroche des cables
N1 = pre.node(np.array([0. , 0., 3.+1.]), number = nb_nodes +1 )
N2 = pre.node(np.array([1. , 0., 3.+1.]), number = nb_nodes +2 )
N3 = pre.node(np.array([1. , 1., 3.+1.]), number = nb_nodes +3 )
N4 = pre.node(np.array([0. , 1., 3.+1.]), number = nb_nodes +4 )

# ajout des noeuds au maillage
mesh.addNode( N1 )
mesh.addNode( N2 )
mesh.addNode( N3 )
mesh.addNode( N4 )

# retrouver les coins dans le maillage de départ

def find_node(c,mesh) :
  P=0
  for node in mesh.nodes:
    if np.linalg.norm(node.coor - c) < 1e-3:
      P = node.number
      break
  if P==0 : sys.exit('node not found')  
  return node    


P1=find_node(np.array([0. , 0., 3.]),mesh)
P2=find_node(np.array([1. , 0., 3.]),mesh)
P3=find_node(np.array([1. , 1., 3.]),mesh)
P4=find_node(np.array([0. , 1., 3.]),mesh)

# définition des éléments
mesh.addBulk(pre.element( 1, connectivity=[P1.number,N1.number], physicalEntity='SPRING'))
mesh.addBulk(pre.element( 1, connectivity=[P2.number,N2.number], physicalEntity='SPRING'))
mesh.addBulk(pre.element( 1, connectivity=[P3.number,N3.number], physicalEntity='SPRING' ))
mesh.addBulk(pre.element( 1, connectivity=[P4.number,N4.number], physicalEntity='SPRING' ))

# ajouts de la poutre et des cables à l'avatar beam
beam = pre.avatar(dimension=dim)
beam.addBulks( mesh.bulks)
beam.addNodes( mesh.nodes)
beam.defineGroups()

##########################################################
# définition des groupes
##########################################################

def box(x):
    # altitude maximale de la poutre
    return x[2]-3. <= 1e-3
beam.addGroupUsingPredicate(name='MESH_', predicate=box)  

def cable(x):
    # altitude maximale de la poutre
    return abs(x[2]-4.) <=1e-3
beam.addGroupUsingPredicate(name='FIX__', predicate=cable,super_group='SPRING')

##########################################################
# définition des modèles et matériaux de la poutre
##########################################################

beam.defineModel(model=mod, group = 'MESH_')
beam.defineMaterial(material=acier, group = 'MESH_')

##########################################################
# définition des modèles et matériaux des cables
##########################################################

beam.defineModel(model=mod2, group = 'SPRING')
beam.defineMaterial(material=ressort, group = 'SPRING')

##########################################################
# BLOCAGE DE LA BASE
##########################################################

beam.imposeDrivenDof(group='FIX__', component=[1, 2,3], dofty='vlocy')

ps+=beam

##########################################################

##########################################################
# DEFINITION DES POST-TRAITEMENTS
##########################################################
post = pre.postpro_commands()

##########################################################
# Ecriture des fichiers pour LMGC
##########################################################
pre.writeDatbox(dim=dim, mats=mx, mods=ms,bodies= ps,tacts=tacts,sees=svs, post=post, gravy=[0., 0., -9.81])

try:
  pre.visuAvatars(ps)
except:
  pass
