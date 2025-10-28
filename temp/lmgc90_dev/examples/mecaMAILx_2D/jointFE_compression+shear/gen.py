# import des modules
import os,sys
import numpy
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

# exemple 2D
dim = 2

# definition d'un modele rigide

# pour le modele FCZM on a besoin de definir un pre endommagement
# comme il est passé par un nodal fields il faut le définir pour tous les modèles
# histoire de ne pas introduire des tests pour les différences entre elas, MC et FCZM on le définit tout le temps même si il ne sert à rien 

stone = pre.material(name='Stone', materialType='ELAS', elas='standard',
                     young=14e9, nu=0.28, anisotropy='isotropic', density=1800.)  
mats.addMaterial(stone)

m2Dl = pre.model(name='M2DNL', physics='MECAx', element='Q4xxx', dimension=dim, external_model='MatL_',
                 kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_',external_fields=['ENDO'])
mods.addModel(m2Dl)

kt=5.76e9
kn=13.76e9

RTRAC=1.21e11*4.1E-7
C=2.8e11*9.6E-7
PHI=38.7
ZMU=0.

preendo=0.99944

law='MC'

if law == 'elas':
    #                                                                         kt              knc  knt  
    joint = pre.material(name='Joint', materialType='JOINT_ELAS',stiffnesses=[(1.-preendo)*kt, kn, (1.-preendo)*kn])
    j2Dl = pre.model(name='J2DNL', physics='MECAx', element='J2xx2', dimension=dim, external_model='no___',material='JELAS',external_fields=['ENDO'])

elif law == 'MC': #calage au pic
    #                                                                       kt knc knt               kncc ec         
    joint = pre.material(name='Joint', materialType='JOINT_MC',stiffnesses=[kt,kn,kn],consolidation=[3*kn, 5.e-3],mc=[RTRAC,PHI,C,ZMU])
    j2Dl = pre.model(name='J2DNL', physics='MECAx', element='J2xx2', dimension=dim, external_model='no___',material='J__MC',external_fields=['ENDO'])

elif law == 'FCZM':
    #                                                                         kt knc knt                kncc  ec                   
    joint = pre.material(name='Joint', materialType='JOINT_FCZM',stiffnesses=[kt, kn, kn],consolidation=[3*kn,5e-3],fczm=[PHI,ZMU,8.,1.,2.8e11,C,206.,1.21e11,RTRAC,3.])
    j2Dl = pre.model(name='J2DNL', physics='MECAx', element='J2xx2', dimension=dim, external_model='no___',material='JFCZM',external_fields=['ENDO'])
    
else :
    sys.exit('unknown law')

mats.addMaterial(joint)
mods.addModel(j2Dl)
    
m = pre.mesh(dimension=2)
idn=0

#       - sommet 1
idn+=1
v = numpy.zeros([2], 'd')
v[0]=0.
v[1]=0.
m.addNode( pre.node(v,number=idn))
   
#       - sommet 2
idn+=1
v = numpy.zeros([2], 'd')
v[0]=1.
v[1]=0.
m.addNode( pre.node(v,number=idn))   

#       - sommet 3
idn+=1
v = numpy.zeros([2], 'd')
v[0]=1.
v[1]=1.
m.addNode( pre.node(v,number=idn))   

#       - sommet 4
idn+=1
v = numpy.zeros([2], 'd')
v[0]=0.
v[1]=1.
m.addNode( pre.node(v,number=idn))

conn = [i for i in range(1,4+1)]
print('bloc',conn)
m.addBulk( pre.element(2, conn, physicalEntity='1') )

# deuxieme maille

#       - sommet 1
idn+=1
v = numpy.zeros([2], 'd')
v[0]=1.
v[1]=0.
m.addNode( pre.node(v,number=idn))
   
#       - sommet 2
idn+=1
v = numpy.zeros([2], 'd')
v[0]=2.
v[1]=0.
m.addNode( pre.node(v,number=idn))   

#       - sommet 3
idn+=1
v = numpy.zeros([2], 'd')
v[0]=2.
v[1]=1.
m.addNode( pre.node(v,number=idn))   

#       - sommet 4
idn+=1
v = numpy.zeros([2], 'd')
v[0]=1.
v[1]=1.
m.addNode( pre.node(v,number=idn))

conn = [i for i in range(4+1,4+4+1)]
print('bloc',conn)
m.addBulk( pre.element(2, conn, physicalEntity='2') )

# troisieme maille

#       - sommet 1
idn+=1
v = numpy.zeros([2], 'd')
v[0]=2.
v[1]=0.
m.addNode( pre.node(v,number=idn))
   
#       - sommet 2
idn+=1
v = numpy.zeros([2], 'd')
v[0]=3.
v[1]=0.
m.addNode( pre.node(v,number=idn))   

#       - sommet 3
idn+=1
v = numpy.zeros([2], 'd')
v[0]=3.
v[1]=1.
m.addNode( pre.node(v,number=idn))   

#       - sommet 4
idn+=1
v = numpy.zeros([2], 'd')
v[0]=2.
v[1]=1.
m.addNode( pre.node(v,number=idn))

conn = [i for i in range(8+1,8+4+1)]
print('bloc',conn)
m.addBulk( pre.element(2, conn, physicalEntity='3') )

conn = [5,8,3,2]
print('joint',conn)
m.addBulk( pre.element(2, conn, physicalEntity='4') )

conn = [7,6,9,12]
print('joint',conn)
m.addBulk( pre.element(2, conn, physicalEntity='4') )
   
b = pre.buildMeshedAvatar(mesh=m, model=m2Dl, material=stone)

b.defineModel(model=j2Dl,group='4')
b.defineMaterial(material=joint,group='4')

def down(x):
   return abs(x[1]-0.) < 1e-6

b.addGroupUsingPredicate(name='down1', predicate=down, super_group='1')
b.addGroupUsingPredicate(name='down2', predicate=down, super_group='3')

b.imposeDrivenDof(group='down1', component=[2], dofty='vlocy')
b.imposeDrivenDof(group='down2', component=[2], dofty='vlocy')

def up(x):
   return abs(x[1]-1.) < 1e-6
b.addGroupUsingPredicate(name='up', predicate=up,super_group='2')

ofile=open('./DATBOX/vz.dat','w')
ofile.write('%07.6e %07.6e \n' % (0.   , 0.000))
ofile.write('%07.6e %07.6e \n' % (1.   , 0.000))
ofile.write('%07.6e %07.6e \n' % (2    , 0.000005))
ofile.write('%07.6e %07.6e \n' % (101. , 0.000005))
ofile.write('%07.6e %07.6e \n' % (102. ,-0.000005))
ofile.write('%07.6e %07.6e \n' % (1000.,-0.000005))
ofile.close()

b.imposeDrivenDof(group='up', component=[2], dofty='vlocy',description='evolution',evolutionFile='vz.dat')

def left(x):
   return abs(x[0]-0.) < 1e-6

b.addGroupUsingPredicate(name='left', predicate=left)

ofile=open('./DATBOX/vx.dat','w')
ofile.write('%07.6e %07.6e \n' % (0.   , 0.0001))
ofile.write('%07.6e %07.6e \n' % (1.   , 0.0001))
ofile.write('%07.6e %07.6e \n' % (2    , 0.000))
ofile.write('%07.6e %07.6e \n' % (1000.,-0.000))
ofile.close()

# b.imposeDrivenDof(group='left', component=[1], dofty='force',ct=0.25e6)
b.imposeDrivenDof(group='left', component=[1], dofty='vlocy',description='evolution',evolutionFile='vx.dat')

def right(x):
   return abs(x[0]-3.) < 1e-6

b.addGroupUsingPredicate(name='right', predicate=right)
b.imposeDrivenDof(group='right', component=[1], dofty='vlocy')

bodies+=b

try:
  pre.visuAvatars(bodies)
except:
  pass

posts = pre.postpro_commands()
posts.addCommand(pre.postpro_command(name='NEW MECAx SETS', mecax_sets=[[(b,'up')],[(b,'left')],[(b,'right')]]))
posts.addCommand(pre.postpro_command(name='Dep EVOLUTION', step=1))
posts.addCommand(pre.postpro_command(name='Fint EVOLUTION', step=1))

# ecriture des fichiers de donnees pour LMGC90
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=posts,gravy=[0.,0.,0.])


import pickle

if law=='MC' :
  f = open('data.pkl', 'wb')
  s=(RTRAC,C,PHI,ZMU)
  pickle.dump(s,f)
  f.close()
else:
  if os.path.exists("./data.pkl") :
      os.remove("./data.pkl")


