from pathlib import Path
from random import seed, randint
from pylmgc90 import pre

def generate():

    # because of randint calls...
    seed(1)

    datbox = Path('./DATBOX')
    datbox.mkdir(exist_ok=True)

    bodies = pre.avatars()
    mods   = pre.models()
    mats   = pre.materials()
    svs    = pre.see_tables()
    tacts  = pre.tact_behavs()

    dim = 3

    mod  = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
    m3Dl = pre.model(name='M3DH8', physics='MECAx', element='H8xxx', dimension=dim,
                     external_model='MatL_', kinematic='small', material='elas_',
                     anisotropy='iso__', mass_storage='coher')
    mods.addModel(m3Dl)

    plexx = pre.material(name='plexx', materialType='RIGID', density=145.)
    tdurx = pre.material(name='tdurx', materialType='RIGID', density=2500.)
    steel = pre.material(name='steel', materialType='ELAS', density=2500., elas='standard',
                         anisotropy='isotropic', young=1.e14, nu=0.3)

    mats.addMaterial(plexx,tdurx,steel)

    #particles
    nb_particles = 500
    radii = pre.granulo_Random(nb_particles, 0.2, 0.3, seed=1)

    # size of the walls of the box
    lx = 1.5
    ly = 1.5
    lz = 1.5
    ez = 0.15

    # for deformable walls, number of elements on each directions
    nbx = 10
    nby = 10
    nbz = 4


    nb_deposit, coor, radii = pre.depositInBox3D(radii, lx, ly, lz, seed=list(range(33)))

    rigid_set = []
    for i, (r, c) in enumerate(zip(radii, coor)):
      body = pre.rigidPolyhedron(radius=r, nb_vertices=randint(15,35), center=c, model=mod, material=plexx, color='BLUEx')
      bodies += body
      if i in [0, 1, 9, 12, 15]:
        rigid_set.append(body)

    #defor box
    down_mesh = pre.buildMeshH8(-lx/2, -ly/2, -ez, lx, ly, ez, nbx, nby, nbz)
    down_wall = pre.buildMeshedAvatar(mesh=down_mesh, model=m3Dl, material=steel)
    down_wall.addContactors(group='up', shape='ASpxx', color='BLUEx')
    down_wall.imposeDrivenDof(group='down', component=[1,2,3], dofty='vlocy')
    bodies += down_wall

    # interactions :
    prprx  = pre.tact_behav('iqsc0','IQS_CLB',fric=0.9)
    prasp  = pre.tact_behav('gapc0','GAP_SGR_CLB',fric=0.9)

    tacts += prprx
    tacts += prasp

    svprpr = pre.see_table(CorpsCandidat='RBDY3',candidat='POLYR',colorCandidat='BLUEx',behav=prprx,
                           CorpsAntagoniste='RBDY3',antagoniste='POLYR',colorAntagoniste='BLUEx',alert=0.02)
    svpras = pre.see_table(CorpsCandidat='RBDY3',candidat='POLYR',colorCandidat='BLUEx',behav=prasp,
                           CorpsAntagoniste='MAILx',antagoniste='ASpxx',colorAntagoniste='BLUEx',alert=0.2)

    svs   += svprpr
    svs   += svpras


    post = pre.postpro_commands()

    c1  = pre.postpro_command(name='BODY TRACKING'   , step=1, rigid_set=rigid_set             )
    c2  = pre.postpro_command(name='TORQUE EVOLUTION', step=1, rigid_set=rigid_set             )
    c3  = pre.postpro_command(name='NEW MECAx SETS'  , step=1, mecax_sets=[[(down_wall,'up')]] )
    c4  = pre.postpro_command(name='Fint EVOLUTION'  , step=1)
    c5  = pre.postpro_command(name='Dep EVOLUTION'   , step=1)

    c6  = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
    c7  = pre.postpro_command(name='VIOLATION EVOLUTION', step=1)
    c8  = pre.postpro_command(name='KINETIC ENERGY'     , step=1)
    c9  = pre.postpro_command(name='DISSIPATED ENERGY'  , step=1)
    c10 = pre.postpro_command(name='COORDINATION NUMBER', step=1)

    c11 = pre.postpro_command(name='CONTACT FORCE DISTRIBUTION', step=1, val      = 3                      )
    c12 = pre.postpro_command(name='DOUBLETS TORQUE EVOLUTION' , step=1, doublets = [tuple(rigid_set[:2])] )
    #c13 = pre.postpro_command(name='QUASI SLIDING CONTACT'     , step=1, val    = 0.1                      )

    nb_post = 12
    for i in range(1,nb_post+1):
      post.addCommand( eval('c'+str(i)) )

    # Ecriture des fichiers pour LMGC
    pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

    try:
      pre.visuAvatars(bodies,True)
    except:
      pass
