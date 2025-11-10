import sys, os

import math
import numpy

from pylmgc90.pre import *

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

#definition des conteneurs
bodies=avatars()
mods=models()
mats=materials()
svs = see_tables()
tacts = tact_behavs()

#dimension
dim=3

#---------------------------------------------
#          CREATION DES MATERIAUX
#---------------------------------------------

#-----corps rigide-----
mat=material(name='stone',materialType='RIGID',density=2500)

mats.addMaterial(mat)

#modele
mod=model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)

mods.addModel(mod)


#dimensions des briques
brick= brick3D('brick',1,1,1)


#definition de la force (+1kN a chq iteration)
def imposeForce(t):
  a=1e9
  dt=1e-5
  t0=20.*dt
  if t <= t0:
    return 0.
  else:
    return a*(t-t0)

color='REDxx'
x=0
y=0
z=0

blocks=[]

body=brick.rigidBrick(center=[x,y,z], model=mod, material=mat, color=color)
body.imposeDrivenDof(component=[1,2,3,4,5,6], dofty='vlocy')
blocks.append(body)
bodies+=body

z=1
body=brick.rigidBrick(center=[x,y,z], model=mod, material=mat, color=color)
writeEvolution(f=imposeForce, instants=numpy.linspace(0,6,60000), path='DATBOX/', name='force.dat')
body.imposeDrivenDof(component=[5,6], dofty='vlocy')
body.imposeDrivenDof(description='evolution', component=1, dofty='force', evolutionFile='force.dat')
blocks.append(body)
bodies+=body



#loi d'interaction MAC
th = tact_behav(name='xxxxx', law='IQS_TH_CZM',dyfr=0.7, stfr=0.7,
                 cn=9.6e10,s1=0.22e6 ,G1=4.,dp1=0.22e6/9.6e10,
                 ct=9.6e10,s2=0.22e6 ,G2=4.,dp2=0.22e6/9.6e10)
tacts+=th

mac = tact_behav(name='yyyyy', law='IQS_MAC_CZM', cn=25e12, ct=25e12, b=0, w=25, dyfr=0.7, stfr=0.7)
tacts+=mac

mal = tact_behav(name='zzzzz', law='IQS_MAL_CZM', dyfr=0.7, stfr=0.7,
                 cn=25e12, s1=1e6, G1=15,
                 ct=25e12, s2=1e6, G2=15)
tacts+=mal


#definition des interactions

#------interaction rouge/rouge------
rgrg=see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='REDxx', behav=mal,
	       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx', alert=0.05)
svs+=rgrg

#ecriture des fichiers
writeBodies(bodies, chemin='DATBOX/')
writeBulkBehav(mats, chemin='DATBOX/', dim=dim, gravy=[0., 0., 0.])
writeTactBehav(tacts, svs, chemin='DATBOX/')
writeDrvDof(bodies, chemin='DATBOX/')
writeDofIni(bodies, chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')

post=postpro_commands()
# following displacement and force on a rigid object :
disp = postpro_command(name='BODY TRACKING', step=1, rigid_set=blocks)
post.addCommand(disp)
torque = postpro_command(name='TORQUE EVOLUTION', step=1, rigid_set=blocks)
post.addCommand(torque)
#
my_command=postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(my_command)

writePostpro(commands=post, parts=bodies, path='DATBOX/')




