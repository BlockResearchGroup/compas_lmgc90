from __future__ import print_function
import sys
import os
if not os.path.isdir('DATBOX'):
  os.mkdir('DATBOX')

from pylmgc90.pre import *

# definition des conteneurs:
#   * de corps
bodies = avatars()
#   * de materiaux
mats = materials()
#   * de modeles
mods = models()
#   * pour les tables de visibilite
svs = see_tables()
#   * pour les lois de contact
tacts = tact_behavs()

# on se place en 3D
dim = 3

# materiau rigide

# creation d'un materiau
tdur = material(name='TDURx', materialType='RIGID', density=2500.)
mats.addMaterial(tdur)

# creation d'un modele de rigide
mod = model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)

# on lit le maillage total dans un fichier, au format gmsh
complete_mesh = readMesh(name='quart_voute.msh', dim=dim)
#maillage fin de l'arche

# on separe les differents maillages volumiques stockes dans le fichier
#correspondants aux voussoirs
meshes= complete_mesh.separateMeshes(dim=dim,entity_type="geometricalEntity") #, keep_all_elements=True)

# on construit des rigides a partir du maillage des fondations
i=0
for elem in list(meshes.values()):
    i+=1
    print(i)
    body = volumicMeshToRigid3D(volumic_mesh=elem, model=mod, material=tdur, color='BLEUx')   
    bodies.addAvatar(body)
    # on bloque chaque fondation
    body.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

# definitions des interactions
#  * loi d'interaction :
lprpr=tact_behav('iqsG0', 'IQS_CLB_g0', fric=0.3)
tacts+=lprpr

#  * table de visibilite :
svprpr = see_table(CorpsCandidat='RBDY3', candidat='POLYR', 
   colorCandidat='BLEUx', behav='iqsG0', CorpsAntagoniste='RBDY3',
   antagoniste='POLYR', colorAntagoniste='BLEUx', alert=2.5e-2)
svs+=svprpr

# ecriture des fichiers de donnees
writeBodies(bodies, chemin='./DATBOX/')
writeBulkBehav(mats, chemin='./DATBOX/', dim=dim)
writeModels(mods,chemin='./DATBOX/')
writeDrvDof(bodies, chemin='./DATBOX/')
writeDofIni(bodies, chemin='./DATBOX/')
writeGPVIni(bodies, chemin='./DATBOX/')
writeTactBehav(tacts, svs, chemin='./DATBOX/')
writeVlocRlocIni(chemin='./DATBOX/')
