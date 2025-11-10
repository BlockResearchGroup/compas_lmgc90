import sys
import filecmp
import pathlib

# import des modules
import numpy as np

from pylmgc90 import pre, chipy

import utils

pre.setStopMode('exception')

datbox = pathlib.Path('xxl/DATBOX')
datbox.mkdir(exist_ok=True, parents=True)

bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
tacts  = pre.tact_behavs()
svs    = pre.see_tables()

dim = 3

mod = pre.model(name='M3DH8', physics='MECAx', element='H8xxx', dimension=3, external_model='no___',
                kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
mods.addModel(mod)

mat = pre.material(name='stone', materialType='ELAS', density=2750., elas='standard', anisotropy='isotropic',
                   young=7.e10, nu=0.2)  
mats.addMaterial(mat)

# build mesh with more that 1e5 nodes... to check if xSpxx
# handle this correctly
dz = 0.2
mesh = pre.buildMeshH8(0.0,0.0,0.0,2.0,2.0,dz,100,100,10)
mesh1 = pre.buildMeshedAvatar(mesh=mesh,model=mod,material=mat)
mesh1.addContactors(group='up',shape='ASpxx',color='ASpxx')
mesh1.addContactors(group='down',shape='CSpxx',color='CSpxx', quadrature=0)

from copy import deepcopy
mesh2 = deepcopy(mesh1)

mesh2.translate(dz=dz)
mesh1.imposeDrivenDof(group='down', component=[1, 2, 3,], dofty='vlocy')

bodies.addAvatar(mesh1)
bodies.addAvatar(mesh2)

# definition d'une loi de contact frottant, avec pre-gap
gapc0=pre.tact_behav(name='gapc0', law='GAP_SGR_CLB', fric=0.5)

tacts.addBehav(gapc0)

sv1 = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='CSxxx', colorCandidat   ='CSpxx',
                    CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='ASpxx',
                    behav=gapc0, alert=1.e-3, halo=2.e-2)


svs.addSeeTable(sv1)


# ecriture des fichiers de donnees pour LMGC90
pre.writeDatbox( dim, mats, mods, bodies, tacts, svs, datbox_path=datbox )


# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0

# time evolution parameters
dt = 4e-3
nb_steps = 1

# theta integrator parameter
theta = 0.501

# deformable  yes=1, no=0
deformable = 1

# interaction parameters
Rloc_tol = 5.e-3

# nlgs parameters
solver_param = { 'conv'  : 1e-4   ,
                 'relax' : 1.0    ,
                 'norm'  : 'Quad ',
                 'gsit1' : 10     ,
                 'gsit2' : 10     ,
                 'stype' : 'Stored_Delassus_Loops         '
               }

# write parameter
freq_write = 1
# display parameters
freq_display = 1

#initialize (do read/write)
print('init lmgc90')
utils.init_lmgc90(dt, theta, dim, mhyp, deformable, wd='xxl')

# really to slow to check...
#only a detection to check that CS/AS are really ok
#chipy.IncrementStep()
#chipy.ComputeFext()
#chipy.ComputeBulk()
#chipy.AssembleMechanicalLHS()
#chipy.AssembleMechanicalRHS()
#chipy.ComputeFreeVelocity()
#chipy.RecupRloc()
#chipy.SelectProxTactors()
#chipy.StockRloc()
#inters = chipy.getInteractions(human=False)
#assert inters.size == 10000

utils.finalize_lmgc90()


# try to read last step:
mats2, mods2, bodies2, tacts2, sees2, inters2 = pre.readDatbox(dim, 'xxl/DATBOX')
inters2 = pre.readState(bodies2, 'xxl/OUTBOX', -1)

# writing read data in new directory
pre.writeDatbox( dim, mats2, mods2, bodies2, tacts2, sees2, inters2, datbox_path='xxl/DATBOX2' )

for f in ('BULK_BEHAV', 'MODELS', 'TACT_BEHAV', 'DRV_DOF', 'BODIES'):
    f1 = datbox/(f+'.DAT')
    f2 = pathlib.Path('xxl/DATBOX2/'+f+'.DAT')
    assert filecmp.cmp(f1,f2,shallow=False), "{} is not read/write correctly".format(f)

