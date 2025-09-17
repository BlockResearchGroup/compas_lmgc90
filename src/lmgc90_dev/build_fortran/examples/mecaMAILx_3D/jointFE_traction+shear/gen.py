
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
                     young=1e17, nu=0., anisotropy='isotropic', density=2500.)  
mats.addMaterial(stone)

m3Dl = pre.model(name='M3DNL', physics='MECAx', element='H8xxx', dimension=dim, external_model='MatL_',
                 kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
mods.addModel(m3Dl)

RTRAC=2e7
C=1e7
PHI=180./8.
ZMU=0. #180./12.

print(RTRAC/(C/math.tan(PHI*math.pi/180.)),math.tan(PHI*math.pi/180.))

law='FCZM'

if law == 'elas':
    #                                                                         kt   knc  knc  
    joint = pre.material(name='Joint', materialType='JOINT_ELAS',stiffnesses=[1e10,1e10,1e10])
    j3Dl = pre.model(name='J3DNL', physics='MECAx', element='J4xx3', dimension=dim, external_model='no___',material='JELAS')

elif law == 'MC':
    #                                                                       kt   knc  knt                  kncc ec         
    joint = pre.material(name='Joint', materialType='JOINT_MC',stiffnesses=[1e10,1e10,1e10],consolidation=[5e10,1e-3],mc=[RTRAC,PHI,C,ZMU])
    j3Dl = pre.model(name='J3DNL', physics='MECAx', element='J4xx3', dimension=dim, external_model='no___',material='J__MC')

elif law == 'FCZM':
    #                                                                         kt   knc  knt                 kncc  ec                   
    joint = pre.material(name='Joint', materialType='JOINT_FCZM',stiffnesses=[1e10,1e10,1e10],consolidation=[5e10,1e-3],fczm=[PHI,ZMU,1.,1.,1e12,C,10000.,1e12,RTRAC,20000.])
    j3Dl = pre.model(name='J3DNL', physics='MECAx', element='J4xx3', dimension=dim, external_model='no___',material='JFCZM')

else:
    sys.exit('unknown law')
    
mats.addMaterial(joint)
mods.addModel(j3Dl)

m = pre.mesh(dimension=3)
idn=0

#       - sommet 1
idn+=1
v = numpy.zeros([3], 'd')
v[0]=0.
v[1]=0.
v[2]=0.
m.addNode( pre.node(v,number=idn))
   
#       - sommet 2
idn+=1
v = numpy.zeros([3], 'd')
v[0]=0.
v[1]=1.
v[2]=0.
m.addNode( pre.node(v,number=idn))   

#       - sommet 3
idn+=1
v = numpy.zeros([3], 'd')
v[0]=0.
v[1]=1.
v[2]=1.
m.addNode( pre.node(v,number=idn))   

#       - sommet 4
idn+=1
v = numpy.zeros([3], 'd')
v[0]=0.
v[1]=0.
v[2]=1.
m.addNode( pre.node(v,number=idn))


#       - sommet 5
idn+=1
v = numpy.zeros([3], 'd')
v[0]=1.
v[1]=0.
v[2]=0.
m.addNode( pre.node(v,number=idn))   

#       - sommet 6
idn+=1
v = numpy.zeros([3], 'd')
v[0]=1.
v[1]=1.
v[2]=0.
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
v[0]=1.
v[1]=0.
v[2]=1.
m.addNode( pre.node(v,number=idn))

conn = [i for i in range(1,8+1)]
print('bloc',conn)
m.addBulk( pre.element(3, conn, physicalEntity='1') )

# deuxieme maille

#       - sommet 1
idn+=1
v = numpy.zeros([3], 'd')
v[0]=1.
v[1]=0.
v[2]=0.
m.addNode( pre.node(v,number=idn))
   
#       - sommet 2
idn+=1
v = numpy.zeros([3], 'd')
v[0]=1.
v[1]=1.
v[2]=0.
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
v[0]=1.
v[1]=0.
v[2]=1.
m.addNode( pre.node(v,number=idn))

#       - sommet 5
idn+=1
v = numpy.zeros([3], 'd')
v[0]=2.
v[1]=0.
v[2]=0.
m.addNode( pre.node(v,number=idn))   

#       - sommet 6
idn+=1
v = numpy.zeros([3], 'd')
v[0]=2.
v[1]=1.
v[2]=0.
m.addNode( pre.node(v,number=idn))

#       - sommet 7
idn+=1
v = numpy.zeros([3], 'd')
v[0]=2.
v[1]=1.
v[2]=1.
m.addNode( pre.node(v,number=idn))   

#       - sommet 8
idn+=1
v = numpy.zeros([3], 'd')
v[0]=2.
v[1]=0.
v[2]=1.
m.addNode( pre.node(v,number=idn))

conn = [i for i in range(8+1,8+8+1)]
print('bloc',conn)
m.addBulk( pre.element(3, conn, physicalEntity='2') )

# troisieme maille

#       - sommet 1
idn+=1
v = numpy.zeros([3], 'd')
v[0]=2.
v[1]=0.
v[2]=0.
m.addNode( pre.node(v,number=idn))
   
#       - sommet 2
idn+=1
v = numpy.zeros([3], 'd')
v[0]=2.
v[1]=1.
v[2]=0.
m.addNode( pre.node(v,number=idn))   

#       - sommet 3
idn+=1
v = numpy.zeros([3], 'd')
v[0]=2.
v[1]=1.
v[2]=1.
m.addNode( pre.node(v,number=idn))   

#       - sommet 4
idn+=1
v = numpy.zeros([3], 'd')
v[0]=2.
v[1]=0.
v[2]=1.
m.addNode( pre.node(v,number=idn))

#       - sommet 5
idn+=1
v = numpy.zeros([3], 'd')
v[0]=3.
v[1]=0.
v[2]=0.
m.addNode( pre.node(v,number=idn))   

#       - sommet 6
idn+=1
v = numpy.zeros([3], 'd')
v[0]=3.
v[1]=1.
v[2]=0.
m.addNode( pre.node(v,number=idn))

#       - sommet 7
idn+=1
v = numpy.zeros([3], 'd')
v[0]=3.
v[1]=1.
v[2]=1.
m.addNode( pre.node(v,number=idn))   

#       - sommet 8
idn+=1
v = numpy.zeros([3], 'd')
v[0]=3.
v[1]=0.
v[2]=1.
m.addNode( pre.node(v,number=idn))

conn = [i for i in range(16+1,16+8+1)]
print('bloc',conn)
m.addBulk( pre.element(3, conn, physicalEntity='3') )

conn = [5,6,7,8,9,10,11,12]
print('joint',conn)
m.addBulk( pre.element(3, conn, physicalEntity='4') )

conn = [13,14,15,16,17,18,19,20]
print('joint',conn)
m.addBulk( pre.element(3, conn, physicalEntity='4') )
   
b = pre.buildMeshedAvatar(mesh=m, model=m3Dl, material=stone)

b.defineModel(model=j3Dl,group='4')
b.defineMaterial(material=joint,group='4')

def down(x):
   return abs(x[2]-0.) < 1e-6

b.addGroupUsingPredicate(name='down1', predicate=down, super_group='1')
b.imposeDrivenDof(group='down1', component=[2,3], dofty='vlocy')
b.addGroupUsingPredicate(name='down2', predicate=down, super_group='3')
b.imposeDrivenDof(group='down2', component=[2,3], dofty='vlocy')


ofile=open('./DATBOX/vz.dat','w')
ofile.write('%07.6e %07.6e \n' % (0.   , 0.000 ))
ofile.write('%07.6e %07.6e \n' % (1.   , 0.000 ))
ofile.write('%07.6e %07.6e \n' % (2.   ,-0.000005))
ofile.write('%07.6e %07.6e \n' % (1000. ,-0.000005))
ofile.close()

def up(x):
   return abs(x[2]-1.) < 1e-6

b.addGroupUsingPredicate(name='up', predicate=up,super_group='2')
b.imposeDrivenDof(group='up', component=[3], dofty='vlocy',description='evolution',evolutionFile='vz.dat')

def left(x):
   return abs(x[0]-0.) < 1e-6

b.addGroupUsingPredicate(name='left', predicate=left)
#b.imposeDrivenDof(group='left', component=[1], dofty='vlocy')
b.imposeDrivenDof(group='left', component=[1], dofty='force',ct=-1e1)

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
f = open('data.pkl', 'wb')
s=(RTRAC,C,PHI,ZMU)
pickle.dump(s,f)
f.close()
