import os

from pylmgc90 import pre, chipy


def generate(cyls, alert, path):

    dim = 3

    bodies = pre.avatars()
    mats   = pre.materials()
    mods   = pre.models()
    sees   = pre.see_tables()
    tacts  = pre.tact_behavs()

    tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
    mats.addMaterial(tdur)

    mod  = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
    mods.addModel(mod)

    for cyl in cyls:
        radius, length, center, alpha1, alpha2, bc = cyl
        body = pre.rigidCylinder(r=radius, h=length, center=center, 
                                 model=mod, material=tdur, color='BLUEx')

        cc = body.getNodeCoor()
        if alpha1:
            body.rotate(description='axis', alpha=alpha1, axis=[1.,0.,0.], center=cc)
        if alpha2:
            body.rotate(description='axis', alpha=alpha2, axis=[0.,1.,0.], center=cc)

        if bc:
            body.imposeDrivenDof(component=[1,2,3,4,5,6], dofty='vlocy')

        bodies.addAvatar(body)

    lcdcd = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
    tacts+= lcdcd

    svcdcd = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='CYLND', colorCandidat   ='BLUEx',
                           CorpsAntagoniste='RBDY3', antagoniste='CYLND', colorAntagoniste='BLUEx',
                           behav=lcdcd, alert=alert)
    sees+= svcdcd


    post = pre.postpro_commands()
    if not os.path.isdir(path):
      os.mkdir(path)
    pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, post=post, datbox_path=os.path.join(path,'DATBOX'))


def compute(nb_steps, path):

    chipy.overall_SetWorkingDirectory(path)
    chipy.Initialize()
    chipy.checkDirectories()

    # logMes
    chipy.utilities_DisableLogMes()

    dim = 3
    dt  = 1e-3

    theta = 0.5

    Rloc_tol = 5.e-2
    tol    = 1e-5
    relax  = 1.0
    norm   = 'Quad '
    gs_it1 = 2
    gs_it2 = 10
    solver_type='Stored_Delassus_Loops         '

    ## write parameter
    #freq_write = 50
    #
    ## display parameters
    #freq_display = 10

    # Set space dimension
    chipy.SetDimension(dim)
    chipy.TimeEvolution_SetTimeStep(dt)
    chipy.Integrator_InitTheta(theta)
    chipy.ReadDatbox(deformable=False)

    chipy.OpenDisplayFiles()
    chipy.OpenPostproFiles()

    chipy.ComputeMass()

    for k in range(0,nb_steps):      
      chipy.IncrementStep()

      chipy.ComputeFext()
      chipy.ComputeBulk()
      chipy.ComputeFreeVelocity()

      chipy.SelectProxTactors()
      chipy.RecupRloc(Rloc_tol)
      chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
      chipy.UpdateTactBehav()

      chipy.StockRloc()

      chipy.ComputeDof()
      chipy.UpdateStep()

      #chipy.WriteOut(freq_write)
      #chipy.WriteDisplayFiles(freq_display)
      #chipy.WritePostproFiles()

    chipy.CloseDisplayFiles()
    chipy.ClosePostproFiles()

    cdcd = chipy.inter_handler_3D_getAll(chipy.CDCDx_ID)
    chipy.Finalize()

    return cdcd
