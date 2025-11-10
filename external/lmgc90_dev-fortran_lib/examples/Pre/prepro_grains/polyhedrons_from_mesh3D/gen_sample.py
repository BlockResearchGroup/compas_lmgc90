import os,sys

import numpy
import math

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

dim = 3

# containers creation
bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

# materials creation
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
plex = pre.material(name='PLEXx',materialType='RIGID',density=100.)
mats.addMaterial(tdur,plex)

# rigid model
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

# reading a weird 3D mesh
lmesh = 1.
volumic_mesh = pre.readMesh('gmsh/bidule.msh', dim)

# building list of rigides polyhedron from mesh elements
bodies += pre.rigidsFromMesh3D(volumic_mesh=volumic_mesh, model=mod, material=plex, color='GREEN')

# rigid foundation
lx = 0.69; ly = 0.69; thick = 0.05
down = pre.rigidPlan(axe1=lx, axe2=ly, axe3=thick, center=[lx, ly, -thick],
                     model=mod, material=tdur, color='REDxx')

# on ajoute la fondation a la liste des corps
bodies.addAvatar(down)

# on fixe la fondation
down.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

if '--with-visu' in sys.argv:
  try:
    pre.visuAvatars(bodies)
  except:
    pass

# interaction laws
lprpr = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+= lprpr

lprpl = pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts+= lprpl

# see tables
svprpr = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='POLYR', colorCandidat   ='GREEN',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='GREEN',
                       behav=lprpr, alert=lmesh/2.)

svs  += svprpr

svprpl = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='POLYR', colorCandidat   ='GREEN',
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='REDxx',
                       behav=lprpl, alert=lmesh/2.)
svs   += svprpl


post = pre.postpro_commands()

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

