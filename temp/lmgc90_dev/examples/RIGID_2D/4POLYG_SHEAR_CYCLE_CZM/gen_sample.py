# -----------------------------------------------------------------------------------------------------#
#                                        Initialisation
# -----------------------------------------------------------------------------------------------------#
import os,sys
import numpy as np
import math
from pylmgc90 import pre
from cycles_tanh import cycles_tanh

# Ecriture du dossier DATBOX
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

if '--novisu' in sys.argv:
  with_figure = False
else:
  with_figure = True

# ltps, lVy, lDepl = cycles_tanh(pdt=1e-4,t0=0.1,list_depl=np.array([0.,0.0001,0.,0.0002,0.,0.0003,0.,0.0004,0.,0.0005,0.,0.0015,0.,0.0035], float),dt0=0.05,vitesse=1e-3, dir_ofile='DATBOX/')   
ltps, lVy, lDepl = cycles_tanh(pdt=1e-4,t0=0.1,
                               list_depl=np.array([0.,0.0035], float),
                               dt0=0.1,vitesse=1e-3, dir_ofile='DATBOX/',
                               figure=with_figure)   

# dimensions des blocs
lx = 1. # longueur
ly = 1. # hauteur


# Definition des conteneurs
bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
svs    = pre.see_tables()
tact   = pre.tact_behavs()

# Dimension 
dim = 2

# -----------------------------------------------------------------------------------------------------#
#                                      Materials and models
# -----------------------------------------------------------------------------------------------------#

# materials
stone = pre.material(name='STONE',materialType='RIGID',density=2000.)
               
# models
defor = pre.model(name='RIGID', physics='MECAx',element='Rxx2D',dimension=dim)

mats.addMaterial(stone)
mods.addModel(defor)

# -----------------------------------------------------------------------------------------------------#
#                                     Blocs generation
# -----------------------------------------------------------------------------------------------------#


block     = pre.brick2D('bloc', lx=lx, ly=ly)
block_fin = pre.brick2D('bloc_fin', lx=lx*0.8, ly=ly)

block_l = block.rigidBrick(center=[-1., 0.], model=defor, material=stone, color='BLEUx')
block_l.imposeDrivenDof(component=[1],dofty='force', ct=1e5)
block_l.imposeDrivenDof(component=[2,3],dofty='vlocy')
bodies.addAvatar(block_l)

block_m = block.rigidBrick(center=[0., 0.], model=defor, material=stone, color='BLEUx')
#block_m.imposeDrivenDof(component=[1],dofty='vlocy')
block_m.imposeDrivenDof(component=[3],dofty='vlocy')
bodies.addAvatar(block_m)

block_r = block.rigidBrick(center=[1., 0.], model=defor, material=stone, color='BLEUx')
block_r.imposeDrivenDof(component=[1],dofty='force', ct=-1e5)
block_r.imposeDrivenDof(component=[2,3],dofty='vlocy')
bodies.addAvatar(block_r)

block_u = block_fin.rigidBrick(center=[0., -1.], model=defor, material=stone, color='ROUGE')
block_u.imposeDrivenDof(component=[1],dofty='vlocy')
block_u.imposeDrivenDof(component=[2],dofty='vlocy', description = 'evolution', evolutionFile='evolution.dat')
block_u.imposeDrivenDof(component=[3],dofty='vlocy')
bodies.addAvatar(block_u)


# -----------------------------------------------------------------------------------------------------#
#                                   Gestion des interactions
# -----------------------------------------------------------------------------------------------------#
# Loi d interaction cohesive
# lczmx = tact_behav(name='gapc1', law='IQS_ABP_CZM',dyfr=0.73,stfr=0.73,
#                    cn=1.0e10,s1=0.05e6,du1=0.8e-3,G1=40,
#                    ct=5e9,s2=5.0e5,du2=2.5e-3,G2=200,
#                    phi=0.5)
lczmx = pre.tact_behav(name='gapc1', law='IQS_EXPO_CZM',dyfr=0.73,stfr=0.73, 
                       cn=1.e10,s1=0.05e6,G1=40,
                       ct=1.e11,s2=2.e5,G2=100,
                       eta=1e-4)
tact+=lczmx

lcclb = pre.tact_behav(name='gapc2', law='IQS_CLB',fric=0.73)
tact+=lcclb


# Declaration des tables de visibilite
svplpl = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='POLYG', colorCandidat   ='BLEUx', behav=lczmx,
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG', colorAntagoniste='BLEUx', alert=0.01 )
svsclb = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='POLYG', colorCandidat   ='BLEUx', behav=lcclb,
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG', colorAntagoniste='ROUGE', alert=0.01 )

svs += svplpl
svs += svsclb


# -----------------------------------------------------------------------------------------------------#
#                                     Post traitement
# -----------------------------------------------------------------------------------------------------#

post = pre.postpro_commands()

# cette fonction nous permet de recuperer les positions, les deplacements, et les vitesses des deux pierres
disp = pre.postpro_command(name='BODY TRACKING', step=1, rigid_set=[block_l, block_m, block_r, block_u])
post.addCommand(disp)
# cette fonction nous permet de recuperer les reactions et les forces exterieures des deux pierres
torque = pre.postpro_command(name='TORQUE EVOLUTION', step=1, rigid_set=[block_l, block_m, block_r, block_u])
post.addCommand(torque)

solver = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(solver)

# -----------------------------------------------------------------------------------------------------#
#                                     Ecriture des fichiers
# -----------------------------------------------------------------------------------------------------#

pre.writeDatbox(dim, mats, mods, bodies, tact, svs, post=post, gravy=[0., 0., 0.])

try :
    pre.visuAvatars(bodies)
except :
    pass
