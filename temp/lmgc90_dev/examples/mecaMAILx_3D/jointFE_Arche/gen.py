
# import des modules
import sys
import math, numpy
from pylmgc90 import pre

from pathlib import Path

datbox = Path('./DATBOX')
datbox.mkdir(exist_ok=True)

# definition des conteneurs:
#   * de corps
bodies = pre.avatars()
#   * de materiaux
mats = pre.materials()
mods = pre.models()
#   * de lois de contacts
tacts = pre.tact_behavs()
#   * de tables de visibilite
svs = pre.see_tables()

# exemple 3D
dim = 3

# definition d'un modele rigide

stone = pre.material(name='Stone', materialType='ELAS', elas='standard',
                     young=1.3e9, nu=0.2, anisotropy='isotropic', density=1950.)  
mats.addMaterial(stone)

m3Dl = pre.model(name='M3DNL', physics='MECAx', element='H8xxx', dimension=dim, external_model='MatL_',
                 kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
mods.addModel(m3Dl)

RTRAC=2e7
C=1e7
PHI=180./8.
ZMU=180./12.

law='MC'

if law == 'elas':
    #                                                                         kt   knc  knt  
    joint = pre.material(name='Joint', materialType='JOINT_ELAS',stiffnesses=[1e10,1e10,1e10])
    j3Dl = pre.model(name='J3DNL', physics='MECAx', element='J4xx3', dimension=dim, external_model='no___',material='JELAS')

elif law == 'MC':
    #                                                                       kt   knc  knt                  kncc ec         
    joint = pre.material(name='Joint', materialType='JOINT_MC',stiffnesses=[1e10,1e10,1e10],consolidation=[5e10,1e-3],mc=[RTRAC,PHI,C,ZMU])
    j3Dl = pre.model(name='J3DNL', physics='MECAx', element='J4xx3', dimension=dim, external_model='no___',material='J__MC')

elif law == 'FCZM':
    #                                                                         kt   knc  knt                  kncc ec                   
    joint = pre.material(name='Joint', materialType='JOINT_FCZM',stiffnesses=[1e10,1e10,1e10],consolidation=[5e10,1e-3],fczm=[PHI,ZMU,1.,1.,1e12,C,10000.,1e12,RTRAC,20000.])
    j3Dl = pre.model(name='J3DNL', physics='MECAx', element='J4xx3', dimension=dim, external_model='no___',material='JFCZM')

else:
    sys.exit('unknown law')

mats.addMaterial(joint)
mods.addModel(j3Dl)

# parametres du script
#   * nombre de blocs
nb_blocs = 11
#   * ouverture angulaire d'un joint
theta_joint = 0.# math.pi/100.
#   * rayons interieur et exterieur
r_int = 0.8; r_ext = 1.

# calcul de l'ouverture corespondant a un bloc
theta_bloc = (math.pi - (nb_blocs - 1)*theta_joint)/nb_blocs
# calcul de l'epaisseur d'un bloc
e_bloc = r_ext - r_int
# calcul de l'epaisseur d'un joint
e_joint = r_ext*theta_joint

# initialisation de l'angle de debut du prochain bloc
theta = 0.
# pour chaque bloc


m = pre.mesh(dimension=3)
idn=0
for i in range(0, nb_blocs):
  
   last=idn
   #       - sommet 1
   idn+=1
   v = numpy.zeros([3], 'd')
   v[0]=r_int*math.cos(theta + theta_bloc)
   v[1]=-0.5*e_bloc
   v[2]=r_int*math.sin(theta + theta_bloc)
   m.addNode( pre.node(v,number=idn))
   
   #       - sommet 2
   idn+=1
   v = numpy.zeros([3], 'd')
   v[0]=r_int*math.cos(theta)
   v[1]=-0.5*e_bloc
   v[2]=r_int*math.sin(theta)
   m.addNode( pre.node(v,number=idn))
   #       - sommet 3
   idn+=1
   v = numpy.zeros([3], 'd')
   v[0]=r_int*math.cos(theta)
   v[1]= 0.5*e_bloc
   v[2]=r_int*math.sin(theta)
   m.addNode( pre.node(v,number=idn))   
   #       - sommet 4
   idn+=1
   v = numpy.zeros([3], 'd')
   v[0]=r_int*math.cos(theta + theta_bloc)
   v[1]= 0.5*e_bloc
   v[2]=r_int*math.sin(theta + theta_bloc)
   m.addNode( pre.node(v,number=idn))   
   #       - sommet 5
   idn+=1
   v = numpy.zeros([3], 'd')
   v[0]=r_ext*math.cos(theta + theta_bloc)
   v[1]=-0.5*e_bloc
   v[2]=r_ext*math.sin(theta + theta_bloc)
   m.addNode( pre.node(v,number=idn))   
   #       - sommet 6
   idn+=1
   v = numpy.zeros([3], 'd')
   v[0]=r_ext*math.cos(theta)
   v[1]=-0.5*e_bloc
   v[2]=r_ext*math.sin(theta)
   m.addNode( pre.node(v,number=idn))
   #       - sommet 7
   idn+=1
   v = numpy.zeros([3], 'd')
   v[0]=r_ext*math.cos(theta)
   v[1]= 0.5*e_bloc
   v[2]=r_ext*math.sin(theta)
   m.addNode( pre.node(v,number=idn))   
   #       - sommet 8
   idn+=1
   v = numpy.zeros([3], 'd')
   v[0]=r_ext*math.cos(theta + theta_bloc)
   v[1]= 0.5*e_bloc
   v[2]=r_ext*math.sin(theta + theta_bloc)
   m.addNode( pre.node(v,number=idn))

   conn = [i for i in range(last+1,last+1+8)]
   print('bloc',conn)
   m.addBulk( pre.element(3, conn, physicalEntity='1') )

   if i>0 and i<nb_blocs:
     d=last-8
     f=last
     conn = [d+1,d+5,d+8,d+4,f+2,f+6,f+7,f+3]
     print('joint',conn)
     m.addBulk( pre.element(3, conn, physicalEntity='2') )
   
   # actualisation de l'angle pour la contruction du 
   # prochain bloc
   theta += theta_bloc + theta_joint

arche = pre.buildMeshedAvatar(mesh=m, model=m3Dl, material=stone)
arche.defineModel(model=j3Dl,group='2')
arche.defineMaterial(material=joint,group='2')

def left(x):
   return x[0]<0. and abs(x[2]) < 1e-6
arche.addGroupUsingPredicate(name='left', predicate=left)

def right(x):
   return x[0]>0. and abs(x[2]) < 1e-6
arche.addGroupUsingPredicate(name='right', predicate=right)

arche.imposeDrivenDof(group='left', component=[1,2,3], dofty='vlocy')

arche.imposeDrivenDof(group='right', component=[2,3], dofty='vlocy')

arche.imposeDrivenDof(group='right', component=1, dofty='vlocy',description='evolution',evolutionFile='vx.dat')

ofile=open('./DATBOX/vx.dat','w')
ofile.write('%07.6e %07.6e \n' % (0.   , 0.000 ))
ofile.write('%07.6e %07.6e \n' % (12.  , 0.000 ))
ofile.write('%07.6e %07.6e \n' % (13.  , 0.001))
ofile.write('%07.6e %07.6e \n' % (52.  , 0.001))
ofile.write('%07.6e %07.6e \n' % (53.  , 0.00))
ofile.write('%07.6e %07.6e \n' % (100. ,-0.00))
ofile.close()

bodies+=arche

posts = pre.postpro_commands()
posts.addCommand(pre.postpro_command(name='NEW MECAx SETS', mecax_sets=[[(arche,'left')],[(arche,'right')]]))
posts.addCommand(pre.postpro_command(name='Dep EVOLUTION', step=1))
posts.addCommand(pre.postpro_command(name='Fint EVOLUTION', step=1))

# ecriture des fichiers de donnees pour LMGC90
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=posts)

try:
  pre.visuAvatars(bodies)
except:
  pass

