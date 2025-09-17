import sys
from pathlib import Path

import numpy

from pylmgc90 import pre

datbox_path = Path('DATBOX')
datbox_path.mkdir(exist_ok=True)

# on se place en 2D
dim = 2

if '--norand' in sys.argv:
  seed = 1
else:
  seed = None

# creration des conteneurs
#   * pour les corps
bodies = pre.avatars()
#   * pour les models
mods = pre.models()
#   * pour les materiaux
mats = pre.materials()
#   * pour les tables de visibilite
svs = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()

# creations de deux materiaux
#   * un materiau rigide pour les parois
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
#   * un materiau elsatique lineaire isotrope, pour les particules
steel = pre.material(name='steel', materialType='ELAS', elas='standard',
                     young=2.05e11, nu=0.3, anisotropy='isotropic', density=7850.)  
mats.addMaterial(tdur, steel)

# on cree deux modeles :
#   * un modele rigide, pour les parois
rigid = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
#   * un modele elastiques, pour les particules deformable 
m2Dl = pre.model(name='M2D_L', physics='MECAx', element='Q4xxx', dimension=dim, 
                 external_model='MatL_', kinematic='large', formulation='TotaL', 
                 material='neoh_', anisotropy='iso__', mass_storage='lump_')
mods.addModel(m2Dl)

m2Dl.check()

# on genere 100 particules
nb_particles=100

# distribtion aleatoire dans [0.5, 2.[ 
radii= pre.granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min = numpy.amin(radii)
radius_max = numpy.amax(radii)

# depot dans une boite rectangulaire
lx = 15.
ly = 10. 
nb_remaining_particles, coor, radii = pre.depositInBox2D(radii, lx, ly)

# si toutes les particules deposees n'ont pas ete conservees
if (nb_remaining_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles were removed!")

# boucle d'ajout des disques :
for r, c in zip(radii, coor):
   # creation un nouveau disque deformable
   body = pre.deformableParticle2D(r=r, center=c, type_part='Disk', 
                                   model=m2Dl, material=steel, color='BLEUx') 
   # ajout du disque dans le conteneur de corps
   bodies += body

# ajout d'une boite lisse, i.e. faite de joncs :
down = pre.rigidJonc(axe1=0.5*lx+radius_max, axe2=radius_max, center=[0.5*lx, -radius_max],
                     model=rigid, material=tdur, color='WALLx')
up   = pre.rigidJonc(axe1=0.5*lx+radius_max, axe2=radius_max, center=[0.5*lx, ly+radius_max],
                     model=rigid, material=tdur, color='WALLx')
left = pre.rigidJonc(axe1=0.5*ly+radius_max, axe2=radius_max, center=[-radius_max, 0.5*ly],
                     model=rigid, material=tdur, color='WALLx')
right= pre.rigidJonc(axe1=0.5*ly+radius_max, axe2=radius_max, center=[lx+radius_max, 0.5*ly],
                     model=rigid, material=tdur, color='WALLx')

# on ajoute les parois a la liste des corps
bodies += down; bodies += up; bodies += left; bodies += right

# on tourne les parois verticales (par rapport a leur propres 
# centre d'inertie)
left.rotate(psi=-numpy.pi/2., center=left.nodes[1].coor)
right.rotate(psi=numpy.pi/2., center=right.nodes[1].coor)

# on fixe les parois
down.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
up.imposeDrivenDof(component=[1, 3], dofty='vlocy')
up.imposeDrivenDof(component=2, dofty='vlocy', ct=-10.,ramp=1.)
left.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
right.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

# gestion des interactions :
#   * declaration des lois
#       - entre particules
ldkdk = pre.tact_behav(name='gapc0',law='GAP_SGR_CLB',fric=0.3)
tacts+= ldkdk
#       - avec les parois
ldkjc = pre.tact_behav(name='gapc1',law='GAP_SGR_CLB',fric=0.5)
tacts+= ldkjc
#   * declaration des tables de visibilite
#       - entre particules
svdkdk = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='BLEUx',
                       CorpsAntagoniste='MAILx', antagoniste='ALpxx',colorAntagoniste='BLEUx',
                       behav=ldkdk, alert=0.1*radius_min)
svs+= svdkdk
#       - avec les parois
svdkjc = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx',colorAntagoniste='WALLx',
                       behav=ldkjc,alert=0.1*radius_min)
svs+= svdkjc

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

try:
  pre.visuAvatars(bodies)
except:
  pass
