import os

from pylmgc90 import pre

dim=2

# definition du conteneur de partie ou de pieces, des modeles et des materiaux
ps    = pre.avatars()
ms    = pre.models()
mx    = pre.materials()
svs   = pre.see_tables()
tacts = pre.tact_behavs()

# < modeles ...
#   ... definition 
mo = pre.model(name='M2DNL',physics='MECAx',element='Q4xxx',dimension=2,external_model='MatL_',
               kinematic='small',material='elas_',anisotropy='iso__',mass_storage='coher')
#   ... stockage
ms.addModel(mo)
# ... modeles >

# < materiaux ...
#   ... definition
ma1 = pre.material(name='steel',materialType='ELAS',elas='standard',
                   young=0.1e+15,nu=0.2,anisotropy='isotropic',
                   density=0.25e+4)
ma2 = pre.material(name='graph',materialType='ELAS',elas='standard',
                   young=0.6e+12,nu=0.2,anisotropy='isotropic',
                   density=0.25e+4)
#   ... stockage
mx.addMaterial(ma1,ma2)
# ... materiaux >

# definition des parties maillees
block_mesh = pre.readMesh('block.msh',dim)
block = pre.buildMeshedAvatar(mesh=block_mesh,model=mo, material=ma1)

block.rotate(psi=0.01)

ps.addAvatar(block)

ground_mesh = pre.readMesh('groun.msh',dim)
ground = pre.buildMeshedAvatar(mesh=ground_mesh,model=mo, material=ma2)

ps.addAvatar(ground)

# Application des conditions initiales
block.imposeInitValue(group='all' ,component=[1,2],value=[0.,0.])
ground.imposeInitValue(group='all',component=[1,2],value=[0.,0.])

# Application des conditions aux limites
# base
ground.imposeDrivenDof(group='10015',component=1,dofty='vlocy')
ground.imposeDrivenDof(group='10015',component=2,dofty='vlocy')

# < Contacteurs ...
#   ... definition et affectation
ground.addContactors(group='10014',shape='ALpxx',color='xxxxx',reverse=True)
block.addContactors(group='10119',shape='CLxxx',color='xxxxx',reverse=True)
block.addContactors(group='10219',shape='CLxxx',color='xxxxx',reverse=True)
# ... Contacteurs >

# Definition des interactions et des tables de visibilites
#.. table de visibilite
sv = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx',colorCandidat='xxxxx',
                   CorpsAntagoniste='MAILx',antagoniste='ALpxx',colorAntagoniste='xxxxx',
                   behav='gapc1',alert=0.01)

svs += sv

#...interaction
b = pre.tact_behav('gapc1','GAP_SGR_CLB',fric=0.9)
tacts += b

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

post = pre.postpro_commands()
my_command = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(my_command)

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mx, ms, ps, tacts, svs, post=post)
