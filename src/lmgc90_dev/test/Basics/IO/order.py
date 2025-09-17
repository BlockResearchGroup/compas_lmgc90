
import os, sys

# check that if the ordering of some
# macro commands is not respected,
# an error message is thrown...

# import des modules
import math, numpy

from pylmgc90 import pre

pre.setStopMode('exception')

if( not os.path.isdir('DATBOX') ):
   os.mkdir('DATBOX')

bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
tacts  = pre.tact_behavs()
svs    = pre.see_tables()

dim = 2

modr= pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
modl= pre.model(name='M2D_L', physics='MECAx', element='Q4xxx', dimension=dim, external_model='no___',
                kinematic='large', formulation='UpdtL', material='elas_', anisotropy='iso__',
                mass_storage='lump_')
mods.addModel(modl)

matr= pre.material(name='TDURx', materialType='RIGID', density=1000.)
matl= pre.material(name='steel', materialType='ELAS' , density=7850.,
                   elas='standard', anisotropy='isotropic', young=2.05e11, nu=0.3)  
mats.addMaterial(matr)
mats.addMaterial(matl)

disk = pre.rigidDisk(r=0.1, center=[-0.8,0.1 ], model=modr, material=matr, color='DISKx')
bodies.addAvatar(disk)

mesh  = pre.buildMesh2D('Q4', x0=-0.6, y0=0.0, lx=1.2, ly=0.2, nb_elem_x=10, nb_elem_y=5)
mesh1 = pre.buildMeshedAvatar(mesh=mesh, model=modl, material=matl)
mesh1.addContactors(group='up', shape='ALpxx', color='ALpxx')
mesh1.addContactors(group='down', shape='CLxxx', color='CLxxx')

bodies.addAvatar(mesh1)

#try:
#  pre.visuAvatars(bodies)
#except:
#  pass

# ecriture des fichiers de donnees pour LMGC90
pre.writeBulkBehav(mats, chemin='./DATBOX/', dim=dim)
pre.writeBodies(bodies , chemin='./DATBOX/')
pre.writeDofIni(bodies , chemin='./DATBOX/')
pre.writeDrvDof(bodies , chemin='./DATBOX/')
pre.writeModels(mods   , chemin='./DATBOX/')
pre.writeGPVIni(bodies , chemin='./DATBOX/')
#pre.writeTactBehav(tacts, svs, chemin='./DATBOX/')
#pre.writeVlocRlocIni(chemin='./DATBOX/')

from pylmgc90 import chipy

def init():
  chipy.utilities_setStopMode(False)
  #chipy.utilities_DisableLogMes()
  chipy.Initialize()
  chipy.checkDirectories()
  chipy.SetDimension(2,1)


# Check that wrong ordering of commands
# generate an acceptable error message


# LoadModels before ReadModels
try:
  init()
  chipy.LoadModels()
except RuntimeError as e:
  print('Handling missing ReadModels with LoadModels -> OK')
  msg = str(e)
  ref_error = "ERROR [mecaMAILx::load_models]: please call ReadModels before trying to LoadModels"
  assert( msg[:len(ref_error)] == ref_error )
  chipy.Finalize()


# LoadModels before ReadBodies
try:
  init()
  chipy.ReadModels()
  chipy.LoadModels()
except RuntimeError as e:
  print('Handling missing ReadBodies with LoadModels -> OK')
  msg = str(e)
  ref_error = "ERROR [mecaMAILx::load_models]: please call ReadBodies before trying to LoadModels"
  assert( msg[:len(ref_error)] == ref_error )
  chipy.Finalize()

# LoadTactors
try:
  init()
  chipy.ReadBodies()
  chipy.LoadTactors()
except RuntimeError as e:
  print('Handling missing LoadModels with LoadTactors (with mailx) -> OK')
  msg = str(e)
  ref_error = "ERROR [ALpxx::read_bodies]: Please call LoadModels before LoadTactors"
  assert( msg[:len(ref_error)] == ref_error )
  chipy.Finalize()

