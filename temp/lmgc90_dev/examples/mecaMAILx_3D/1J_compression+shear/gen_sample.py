
# import des modules
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

RTRAC=2e7
C=1e7
PHI=math.pi/8.
ZMU=0. #math.pi/12.

elas=0

if elas: 
  joint = pre.material(name='Joint', materialType='JOINT_ELAS',stiffnesses=[1e10,1e10,1e10])
  j3Dl  = pre.model(name='J3DNL', physics='MECAx', element='J4xx3', dimension=dim, external_model='no___',material='JELAS')
else :
  joint = pre.material(name='Joint', materialType='JOINT_MC',stiffnesses=[1e10,1e10,1e10],consolidation=[1e12,1e-2],mc=[RTRAC,PHI,C,ZMU])  
  j3Dl = pre.model(name='J3DNL', physics='MECAx', element='J4xx3', dimension=dim, external_model='no___',material='J__MC')

mats.addMaterial(joint)

mods.addModel(j3Dl)

ofile=open('./DATBOX/vx.dat','w')
ofile.write('%07.6e %07.6e \n' % (0.   , 0.000))
ofile.write('%07.6e %07.6e \n' % (1.   , 0.000))
ofile.write('%07.6e %07.6e \n' % (1.01 , 0.0005))
ofile.write('%07.6e %07.6e \n' % (10.  , 0.0005))
ofile.write('%07.6e %07.6e \n' % (10.01,-0.0005))
ofile.write('%07.6e %07.6e \n' % (100. ,-0.0005))
ofile.close()

ofile=open('./DATBOX/vz.dat','w')
ofile.write('%07.6e %07.6e \n' % (0.   ,-0.0001))
ofile.write('%07.6e %07.6e \n' % (1.   ,-0.0001))
ofile.write('%07.6e %07.6e \n' % (1.01 , 0.000))
ofile.write('%07.6e %07.6e \n' % (100. , 0.000))
ofile.close()

m = pre.mesh(dimension=3)
idn=0

#       - sommet 1
idn+=1
v = numpy.zeros([3], 'd')
v[0]=0.
v[1]=0.
v[2]=1.
m.addNode( pre.node(v,number=idn))   

#       - sommet 2
idn+=1
v = numpy.zeros([3], 'd')
v[0]=1.
v[1]=0.
v[2]=1.
m.addNode( pre.node(v,number=idn))

#       - sommet 3
idn+=1
v = numpy.zeros([3], 'd')
v[0]=1.
v[1]=1.
v[2]=1.
m.addNode( pre.node(v,number=idn))   

#       - sommet 4
idn+=1
v = numpy.zeros([3], 'd')
v[0]=0.
v[1]=1.
v[2]=1.
m.addNode( pre.node(v,number=idn))

#       - sommet 5
idn+=1
v = numpy.zeros([3], 'd')
v[0]=0.
v[1]=0.
v[2]=1.
m.addNode( pre.node(v,number=idn))
   
#       - sommet 6
idn+=1
v = numpy.zeros([3], 'd')
v[0]=1.
v[1]=0.
v[2]=1.
m.addNode( pre.node(v,number=idn))

#       - sommet 7
idn+=1
v = numpy.zeros([3], 'd')
v[0]=1.
v[1]=1.
v[2]=1.
m.addNode( pre.node(v,number=idn))   

#       - sommet 8
idn+=1
v = numpy.zeros([3], 'd')
v[0]=0.
v[1]=1.
v[2]=1.
m.addNode( pre.node(v,number=idn))   


conn = [1,2,3,4,5,6,7,8]
print('joint',conn)
m.addBulk( pre.element(3, conn, physicalEntity='1') )

m.addBulk( pre.element(2, [1,2,3,4], physicalEntity='down') )
m.addBulk( pre.element(2, [5,6,7,8], physicalEntity='up') )


b = pre.buildMeshedAvatar(mesh=m, model=j3Dl, material=joint)

b.imposeDrivenDof(group='down', component=[1,2,3], dofty='vlocy')
b.imposeDrivenDof(group='up', component=[1], dofty='vlocy',description='evolution',evolutionFile='vx.dat')
b.imposeDrivenDof(group='up', component=[3], dofty='vlocy',description='evolution',evolutionFile='vz.dat')


bodies+=b

try:
  pre.visuAvatars(bodies)
except:
  pass

post = pre.postpro_commands()

# ecriture des fichiers de donnees pour LMGC90
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post,gravy=[0.,0.,0.])

import pickle
f = open('data.pkl', 'wb')
s=(RTRAC,C,PHI,ZMU)
pickle.dump(s,f)
f.close()
