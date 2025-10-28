
from pathlib import Path

#import gmsh

from pylmgc90 import pre

datbox = Path('./DATBOX')
datbox.mkdir(exist_ok=True)


# on se place en 3D
dim = 3
# creation des conteneurs
bodies = pre.avatars()
mat    = pre.materials()
mod    = pre.models()
sees   = pre.see_tables()
tacts  = pre.tact_behavs()
post   = pre.postpro_commands()

# creations du materiaux
#   * un rigide pour la fondation et les blocs
stone = pre.material(name='Stone', materialType='RIGID', density=2000.)
# ajout du materiau
mat.addMaterial(stone)

# on cree un modele de rigide
rigid = pre.model(name='Rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mod.addModel(rigid)

# # =============================================================================
#lecture de la geometrie de la fondation
#gmsh.initialize()
#gmsh.open("fondation.brep")
#gmsh.model.mesh.generate(2)
#gmsh.write('fondation.msh')
#gmsh.finalize()

fonda = pre.readMesh("fondation.msh", dim=dim)
body  = pre.surfacicMeshToRigid3D(fonda, model=rigid, material=stone, color='FONDA')
body.imposeDrivenDof(component=[1,2,3,4,5,6],dofty='vlocy')
bodies += body

# # =============================================================================
# lecture de la geometrie des blocs

#gmsh.initialize()
#gmsh.open("blocs.brep")
#gmsh.model.mesh.generate(3)
#gmsh.write('blocs.msh')
#gmsh.finalize()

blocs = pre.readMesh("blocs.msh", dim=dim)
for i,bloc in blocs.separateMeshes(3).items():
    print("bloc charges",i)
    body = pre.volumicMeshToRigid3D(bloc, model=rigid, material=stone, color='BLOCS')
    bodies += body

# gestion des interactions :
#   * declaration des lois
#       - entre voussoirs

# contact = tact_behav(name='conta', law='JOINT_VISCO_ELASTIC_g0', young=5e9, damp=0., epai=0.005, fric=0.5, decomp=1.0)
contact = pre.tact_behav(name='conta', law='IQS_CLB', fric=0.5)
tacts += contact


#   * declaration des tables de visibilite
#       - avec la fondation
sv1    = pre.see_table(CorpsCandidat   ='RBDY3',    candidat='POLYR',    colorCandidat='FONDA', behav=contact, 
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLOCS', alert=0.01)
sees += sv1
#       - entre blocs
sv2    = pre.see_table(CorpsCandidat   ='RBDY3',    candidat='POLYR',    colorCandidat='BLOCS', behav=contact, 
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLOCS', alert=0.01)
sees += sv2


# Write all files to compute solution with LMGC90
pre.writeDatbox(dim, mat, mod, bodies, tacts, sees, post=post)

pre.visuAvatars(bodies)
