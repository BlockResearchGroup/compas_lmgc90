from pathlib import Path

from pylmgc90 import pre

def generate():

    datbox = Path('./DATBOX')
    datbox.mkdir(exist_ok=True)

    bodies = pre.avatars()
    mods   = pre.models()
    mats   = pre.materials()
    svs    = pre.see_tables()
    tacts  = pre.tact_behavs()

    dim = 2

    mod  = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
    m2Dl = pre.model(name='M2DQ4', physics='MECAx', element='Q4xxx', dimension=dim,
                     external_model='MatL_', kinematic='small', material='elas_',
                     anisotropy='iso__', mass_storage='coher')
    mods.addModel(m2Dl)

    plexx = pre.material(name='plexx', materialType='RIGID', density=145.)
    tdurx = pre.material(name='tdurx', materialType='RIGID', density=2500.)
    steel = pre.material(name='steel', materialType='ELAS', density=2500., elas='standard',
                         anisotropy='isotropic', young=1.e14, nu=0.3)

    mats.addMaterial(plexx,tdurx,steel)

    #particles
    nb_particles = 50
    radii = pre.granulo_Random(nb_particles, 0.2, 0.3, seed=1)

    # size of the walls of the box
    lx = 3.0
    ex = 0.2 # sometime radius_min/max
    ly = 2.0
    ey = 0.2 # sometime radius_min/max

    # for deformable walls, number of elements on each directions
    nb_lx = 4
    nb_ex = 1
    nb_ly = 4
    nb_ey = 1

    nb_deposit, coor, radii = pre.depositInBox2D(radii, lx, ly)

    rigid_set = []
    for i, (r, c) in enumerate(zip(radii, coor)):
      body = pre.rigidDisk(r=r, center=c, model=mod, material=plexx, color='BLUEx')
      bodies += body
      if i in [1, 7, 9, 12, 15]:
        rigid_set.append(body)

    #defor box
    down_mesh = pre.buildMesh2D('Q4',0.,0.,lx+2*ey,ex,nb_lx,nb_ex)
    down_wall = pre.buildMeshedAvatar(mesh=down_mesh, model=m2Dl, material=steel)
    down_wall.translate(dx=-ey,dy=-ex)
    down_wall.addContactors(group='up', shape='ALpxx', color='BLUEx')
    down_wall.imposeDrivenDof(group='down', component=[1,2], dofty='vlocy')
    bodies += down_wall
    #
    wall_mesh = pre.buildMesh2D('Q4',0.,0.,ey,ly,nb_ey,nb_ly)
    #
    left_wall = pre.buildMeshedAvatar(mesh=wall_mesh, model=m2Dl, material=steel)
    left_wall.translate(dx=-ey)
    left_wall.addContactors(group='right', shape='ALpxx', color='BLUEx')
    left_wall.addContactors(group='down', shape='CLxxx', color='BLUEx', weights=[0.25, 0.75])
    cl_set = pre.CLxxx_set(body=left_wall, group='down')
    bodies += left_wall
    #
    wall_mesh = pre.buildMesh2D('Q4',0.,0.,ey,ly,nb_ey,nb_ly)
    #
    right_wall = pre.buildMeshedAvatar(mesh=wall_mesh, model=m2Dl, material=steel)
    right_wall.translate(dx=lx)
    right_wall.addContactors(group='left', shape='ALpxx', color='BLUEx')
    right_wall.addContactors(group='down', shape='CLxxx', color='BLUEx',weights=[0.25, 0.75])
    bodies += right_wall

    # interactions :
    dkdkx  = pre.tact_behav('iqsc0','IQS_CLB',fric=0.9)
    dkalp  = pre.tact_behav('gapc0','GAP_SGR_CLB',fric=0.9)
    clalp  = pre.tact_behav('mczm0','MAC_CZM',dyfr=0.1, stfr=0.1, cn=1.e13, ct=1.e13, b=1.e-4, w=2.5e-3)

    tacts += dkdkx
    tacts += dkalp
    tacts += clalp

    svdkdk = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx',colorCandidat='BLUEx',behav=dkdkx,
                           CorpsAntagoniste='RBDY2',antagoniste='DISKx',colorAntagoniste='BLUEx',alert=0.02)
    svdkal = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx',colorCandidat='BLUEx',behav=dkalp,
                           CorpsAntagoniste='MAILx',antagoniste='ALpxx',colorAntagoniste='BLUEx',alert=0.1)
    svclal = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx',colorCandidat='BLUEx',behav=clalp,
                           CorpsAntagoniste='MAILx',antagoniste='ALpxx',colorAntagoniste='BLUEx',alert=0.01,halo=3.)

    svs   += svdkdk
    svs   += svdkal
    svs   += svclal

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

    c11 = pre.postpro_command(name='CLxxx ANALYSIS'            , step=1, CLxxx_sets = [cl_set]               )
    c12 = pre.postpro_command(name='CONTACT FORCE DISTRIBUTION', step=1, val      = 3                      )
    #c13 = pre.postpro_command(name='QUASI SLIDING CONTACT'     , step=1, val      = 0.1                    )

    nb_post = 12
    for i in range(1,nb_post+1):
      post.addCommand( eval('c'+str(i)) )

    # Ecriture des fichiers pour LMGC
    pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

    try:
      pre.visuAvatars(bodies)
    except:
      pass
