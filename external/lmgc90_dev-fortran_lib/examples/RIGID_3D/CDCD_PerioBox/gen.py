import sys
import pickle
from pathlib import Path

import numpy as np
import math
import random

from pylmgc90 import pre


datbox_path = Path('DATBOX')
datbox_path.mkdir(exist_ok=True)
  
if '--norand' in sys.argv:
  seed = list(range(34))
  random.seed(a=1)
else:
  seed = None

# WARNING : in 3D by default z-axis is upward
# this is very important to direct PLANx objects

#=====================================================================

#Parametres
# particle geometry
R_cir = 0.014     # m            # circumscribed radius of particules
n_branch= 6       # no unit      # Nombre of cylinders of each particules (n>2)
r_p = 0.004       # m            # radius of cylinder
# keep mass of particule constant
Mass = 0.005     # kg

# Distance along y-axis
Wext =  9*2*r_p
# Distance along x-axis
Lext = 12*2*r_p
# height of walls
Hext = 4*2*R_cir 

g=9.81
#angle of slope
theta=0.0*np.pi
gravity=[0.,0.,-g]

# coefficient de frottement entre les particules
mu_spsp = 0.2         
# coefficient de frottement avec les parois
mu_sppl = 0.9

nb_particles= 100

#=====================================================================
n_cylinder= n_branch/2
def insectingCylinderVolume(r):
    if n_cylinder == 3:
        return (16-np.sqrt(128))*r**3
    elif n_cylinder == 4:
        return 12*(np.sqrt(8)-np.sqrt(6))*r**3

def densityParticle(r_p, R_cir, Mass):
    V_assembly = n_cylinder*(4/3*np.pi*r_p**3+ np.pi*2*R_cir*r_p**2)
    rho_assembly = Mass/V_assembly
    V_num = V_assembly-(n_cylinder-1)*insectingCylinderVolume(r_p)
    rho_num = Mass/V_num
    return rho_assembly,rho_num

# this function create particule non-convexe with cylindre
def rigidNconvex(RR,rr, n_branch, center, model, material, color):
    
    bd = pre.avatar(dimension=3)
    bd.addNode(pre.node(coor=np.array(center),number=1) )
    bd.addBulk(pre.rigid3d())
    bd.defineGroups()
    bd.defineModel(model=model)
    bd.defineMaterial(material=material)

    bd.addContactors(shape='CYLND', color=color, byrd=rr, High=RR,
                     frame=[[ 1.,  0.,  0.],
                            [ 0.,  1.,  0.],
                            [ 0.,  0.,  1.]])
    bd.addContactors(shape='CYLND', color=color, byrd=rr, High=RR,
                     frame=[[ 1.,  0.,  0.],
                            [ 0.,  0., -1.],
                            [ 0.,  1.,  0.]])
    bd.addContactors(shape='CYLND', color=color, byrd=rr, High=RR,
                     frame=[[ 0.,  0., -1.],
                            [ 0.,  1.,  0.],
                            [ 1.,  0.,  0.]])

    bd.computeRigidProperties()
    bd.rotate(description='Euler',phi=math.pi*random.random(), theta=math.pi*random.random(), 
              psi=math.pi*random.random(), axis=[1., 0., 0.], center=bd.nodes[1].coor)
    return bd

#================== store the parameters in csv with unit ==================================
rho_assembly,rho_num = densityParticle(r_p=r_p, R_cir=R_cir, Mass=Mass)

with open('sample.pkl', 'wb') as f:
  fields = [('R_cir'       , 'm'      ,),
            ('n_branch'    , 'None'   ,),
            ('r_p'         , 'm'      ,),
            ('rho_assembly', 'kg*m^-3',),
            ('rho_num'     , 'kg*m^-3',),
            ('Wext'        , 'm'      ,),
            ('Lext'        , 'm'      ,),
            ('Hext'        , 'm'      ,),
           ]

  data = { val:(eval(val),unit,) for val,unit in fields }
  pickle.dump( data, f)

#=====================================================================
dim = 3

bodies = pre.avatars()
bodies2 = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

#create materials
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
plex = pre.material(name='PLEXx',materialType='RIGID',density=rho_assembly)
mats.addMaterial(tdur,plex)

# create a model of rigid
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

# definition de la granulo
# distribution aleatoire dans [0.8, 1.2[ 
radii= pre.granulo_Random(nb_particles, 0.8*R_cir, 1.2*R_cir, seed)

# on recupere le plus petit et le plus grand rayon
radius_min=np.amin(radii)
radius_max=np.amax(radii)
# on depose les particules sous gravite, dans une boite 
nb_comp_particles, coor, radii = pre.depositInBox3D(radii, Lext, Wext, Hext, seed=seed)

# si toutes les particules deposees n'ont pas ete deposees
if (nb_comp_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles cannot be deposited!")

# add particules :
for r, c in zip(radii, coor):
   # call function to create non convex particules
   body=rigidNconvex(RR=r-r_p,rr=r_p,n_branch=n_branch, center=c, 
                     model=mod, material=plex, color='BLUEx') 
   bodies += body


# add cylinder for the rough wall
nbx=int(Lext/(2.*r_p))+1
nby=int(Wext/(2.*r_p))+1

y=-Wext/2.    
j=1
while j < nby:
    y+=r_p
    x=-Lext/2.    
    i=1
    while i < nbx:
        x+=r_p
        # create cylinder
        body2 = pre.rigidCylinder(r_p, R_cir-r_p, 
                                  center=[x, y,-R_cir], 
                                  model=mod, material=plex, color='VERTx', 
                                  number=None, is_Hollow=False)
        body2.imposeDrivenDof(component=[1,2,3,4,5,6], dofty='vlocy')
        body2.translate(dx=0.5*(Lext),dy=0.5*(Wext))
        # ajout du disque dans le conteneur de corps
        bodies2 += body2
        x+=r_p
        i+=1
    y+=r_p        
    j+=1

bodies+=bodies2
    
# gestion des interactions :
#   * declaration des lois
#       - entre particules
lspsp=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=mu_spsp)
tacts+=lspsp
#       - avec les parois
lsppl=pre.tact_behav(name='iqsc1', law='IQS_CLB', fric=mu_sppl)
tacts+=lsppl

#interactions
svCDCD = pre.see_table(CorpsCandidat   ='RBDY3',candidat   ='CYLND',colorCandidat   ='BLUEx',
                   CorpsAntagoniste='RBDY3',antagoniste='CYLND',colorAntagoniste='BLUEx',
                   behav=lspsp,alert=0.1*r_p)
svs+=svCDCD

#       - avec les parois rougues
svsppl = pre.see_table(CorpsCandidat='RBDY3', candidat='CYLND', colorCandidat='BLUEx',
                       CorpsAntagoniste='RBDY3', antagoniste='CYLND', colorAntagoniste='VERTx',
                       behav=lsppl, alert=0.1*r_p)
svs+=svsppl

post = pre.postpro_commands()
sitr = pre.postpro_command(name="SOLVER INFORMATIONS",step=1)
viol = pre.postpro_command(name="VIOLATION EVOLUTION",step=1)
post.addCommand(sitr)
post.addCommand(viol)

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post, datbox_path=datbox_path, gravy=gravity)

print('lx=',Lext,' ly=',Wext,' lz=',Hext)
print('n_p=',nb_comp_particles)

try:
  pre.visuAvatars(bodies)
except:
  pass
