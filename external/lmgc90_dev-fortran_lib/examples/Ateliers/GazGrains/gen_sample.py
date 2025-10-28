from __future__ import print_function
import sys
import numpy
import math

from pylmgc90.pre import *

# generation echantillon

# taille du domaine (coin en 0,0 )
bx=0.005
by=0.02

radius_max=7.1e-5
radius_min=7e-5
dim=2

# creation des conteneurs
#   * pour les corps
bodies = avatars()
# Create the pack of physical model
mods = models()
#   * pour les materiaux
mats = materials()
#   * pour les tables de visibilite
svs = see_tables()
#   * pour les lois de contact
tacts = tact_behavs()

# creations de deux materiaux
tdur = material(name='TDURx',materialType='RIGID',density = 2700.)
mats.addMaterial(tdur)

# on cree un modele de rigide
mod = model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

# on genere 100 particules
nb_particles=3600

# distribtion aleatoire dans [0.5, 2.[ 
radii=granulo_Random(nb_particles, radius_min, radius_max)

# on recupere le plus petit et le plus grand rayon
radius_min=min(radii)
radius_max=max(radii)

# depot dans une boite rectangulaire
lx = bx
ly = radius_max*60
[nb_remaining_particles, coor]=depositInBox2D(radii, lx, ly)

# si toutes les particules deposees n'ont pas ete conservees
if (nb_remaining_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles were removed!")

# boucle d'ajout des disques :
for i in range(0,nb_remaining_particles,1):
  # creation un nouveau disque rigide, constitue du materiau plex
  body=rigidDisk(r=radii[i], center=coor[2*i : 2*(i + 1)], 
                 model=mod, material=tdur, color='BLEUx')
  #body.translate(dx=(bx-lx)*0.5,dy=by - 1.1*ly)
  # ajout du disque dans le conteneur de corps
  bodies += body

# on declare un corps par paroi
down =avatar( dimension=dim)
up   =avatar( dimension=dim)
left =avatar( dimension=dim)
right=avatar( dimension=dim)

# on attribue un comportement volumique de type rigide aux parois
down.addBulk( rigid2d() )
up.addBulk( rigid2d() )
left.addBulk( rigid2d() )
right.addBulk( rigid2d() )

# on positionne les parois dans l'espace
down.addNode( node( coor=numpy.array([0.5*bx, -radius_max]),number=1) )
up.addNode( node( coor=numpy.array([0.5*bx, by + radius_max]),number=1) )
left.addNode( node( coor=numpy.array([-radius_max, 0.5*by]),number=1) )
right.addNode( node( coor=numpy.array([bx + radius_max, 0.5*by]), number=1) )

# on definit les groupes
down.defineGroups()
up.defineGroups()
left.defineGroups()
right.defineGroups()

down.defineModel(model=mod)
up.defineModel(model=mod)
left.defineModel(model=mod)
right.defineModel(model=mod)

# on definit le materiau pour chaque paroi
down.defineMaterial(material=tdur)
up.defineMaterial(material=tdur)
left.defineMaterial(material=tdur)
right.defineMaterial(material=tdur)

# on affecte un contacteur jonc a chaque paroi
# et on l'affecte aux parois
down.addContactors(shape='JONCx', color='WALLx', axe1=0.5*bx + radius_max, axe2=radius_max)
up.addContactors(shape='JONCx', color='WALLx', axe1=0.5*bx + radius_max, axe2=radius_max)
left.addContactors(shape='JONCx', color='WALLx', axe1=0.5*by + radius_max, axe2=radius_max)
right.addContactors(shape='JONCx', color='WALLx', axe1=0.5*by + radius_max, axe2=radius_max)

# on calcule la surface et l'inertie de chaque paroi
down.computeRigidProperties()
up.computeRigidProperties()
left.computeRigidProperties()
right.computeRigidProperties()

# on ajoute les parois a la liste des corps
bodies += down; bodies += up; bodies += left; bodies += right

# on tourne les parois verticales (par rapport a leur propres 
# centre d'inertie)
left.rotate(psi=-math.pi/2., center=left.nodes[1].coor)
right.rotate(psi=math.pi/2., center=right.nodes[1].coor)

# on fixe les parois
down.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
up.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
left.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
right.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

# gestion des interactions :
#   * declaration des lois
#       - entre particules
ldkdk=tact_behav(name='iqsc0',law='RST_CLB',fric=0.3,rstn=0.,rstt=0.)
tacts+=ldkdk

#   * declaration des tables de visibilite
#       - entre particules
svdkdk = see_table(CorpsCandidat='RBDY2',candidat='DISKx',colorCandidat='BLEUx',
                   behav=ldkdk,
                   CorpsAntagoniste='RBDY2',antagoniste='DISKx',colorAntagoniste='BLEUx',
                   alert=0.01*radius_min)
svs+=svdkdk
svdkjc = see_table(CorpsCandidat='RBDY2',candidat='DISKx',colorCandidat='BLEUx',
                   behav=ldkdk,
                   CorpsAntagoniste='RBDY2',antagoniste='JONCx',colorAntagoniste='WALLx',
                   alert=0.005*bx)
svs+=svdkjc


########## modele thermique #################################

# ------------------------------------------------------
# Create a thermal model 
Model_Thermique = model(name='DIFFU',                           # Key to find the model 
                        physics='THERx',                        # physics of the model 
                        element='T3xxx',                        # Possible elements T6xxx,H8xxx, H20Rx, TE4xx
                        dimension = dim,
                        external_model='no___', 
                        capacity_storage='lump_',
                        formulation = 'class',
                        external_fields = ['COCO','SPHV'])

mods.addModel(Model_Thermique)
# ------------------------------------------------------
# Create a material
Mat_1            = material(name='Gaz__', 
                            materialType='THERMO_ELAS', 
                            density=1.0,
                            elas='standard', 
                            young=0.0, 
                            nu=0.0,
                            T_ref_meca = 0.0, 
                            anisotropy='isotropic',
                            dilatation = 0.0,
                            conductivity='field', 
                            specific_capacity='field')
# And save this material in the pack of materials behaviours
mats.addMaterial(Mat_1)
# ------------------------------------------------------
#
boite = avatar( dimension = dim)
# Import gmsh mesh for LMGC90
mesh   = readMesh('./gmsh/darcy.msh', dim)
# Import element connectivity
boite.addBulks(mesh.bulks)
# Import node position
boite.addNodes(mesh.nodes)
# Import physical groups
boite.defineGroups()

# ------------------------------------------------------
# Apply physical model on bodies
boite.defineModel(model=Model_Thermique)
# Apply material behaviour on physical groups 31
boite.defineMaterial(material=Mat_1, group = 'Domain')
# And save this bodies in the pack of bodies

# Apply the boundary conditions on physical groups
# Imposed a nil flux on 'Domain'
# Imposed a constant temperature 
### boite.imposeDrivenDof(group='CornerBottomLeft', component = 1, dofty='temp', ct = 0.0, rampi = 1.0)
boite.imposeDrivenDof(group='Up', component = 1, dofty='temp', ct = 0.0, rampi = 1.0)
boite.imposeDrivenDof(group='Down', component = 1, dofty='temp', ct = 1000.0, rampi = 1.0)
# ------------------------------------------------------
# Apply the initial conditions on physical groups
# Imposed initial temperature on the bodies at 0K
boite.imposeInitValue(group='Domain', component = 1, value = 0.0)

bodies += boite

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

writeBodies(bodies,chemin='DATBOX/')
writeBulkBehav(mats,chemin='DATBOX/')
writeTactBehav(tacts,svs,chemin='DATBOX/')
writeDrvDof(bodies,chemin='DATBOX/')
writeDofIni(bodies,chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')
writeModels(mods,chemin='./DATBOX/')
writeGPVIni(bodies, chemin='./DATBOX/')

post=postpro_commands()
# following displacement and force on a rigid object :
disp = postpro_command(name='BODY TRACKING', step=1, rigid_set=[body])
post.addCommand(disp)
torque = postpro_command(name='TORQUE EVOLUTION', step=1, rigid_set=[body])
post.addCommand(torque)
#
my_command=postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(my_command)

writePostpro(commands=post, parts=bodies, path='DATBOX/')
