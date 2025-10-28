
import sys

from pylmgc90 import pre, chipy

def generate(lx,ly,lz,nb_x,nb_rows,joint):
    
    # 3D
    dim = 3
    
    bodies = pre.avatars()
    mats   = pre.materials()
    mods   = pre.models()
    svs    = pre.see_tables()
    tacts  = pre.tact_behavs()
    
    tdur = pre.material(name='TDURx', materialType='RIGID', density=2500.)
    plex = pre.material(name='PLEXx', materialType='RIGID', density=2000.)
    mats.addMaterial(tdur,plex)
    
    mod3D = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
    mods.addModel(mod3D)
    
    # brick definition
    brique_Paris = pre.brick3D(name='brique de Paris', lx=lx, ly=ly, lz=lz)
    
    length = nb_x*lx
    # paneress wall using previous brick type
    wall = pre.paneresse_simple(brick_ref=brique_Paris, disposition="paneresse")
    
    # on caracterise le mur, en longueur
    
    # first layer definition 
    wall.setFirstRowByLength(first_brick_type="1/2", length=length, joint_thickness=joint)
    
    # number of layers
    wall.setNumberOfRows(nb_rows)
    # joint thickness between layers
    wall.setJointThicknessBetweenRows(joint)
    
    # wall height computation
    wall.computeHeight()
    
    print("wall.nb_rows=", wall.nb_rows)
    print("wall.height=", wall.height)
    print("wall.joint_thickness=", wall.joint_thickness)
    
    # wall building
    bodies = wall.buildRigidWall(origin=[0., 0., 0.], model=mod3D, material=plex, colors=['BLUEx', 'REDxx'])
    
    # rigid fondation
    floor = pre.rigidPlan(axe1=length/2., axe2=ly/2., axe3=lz/2., center=[length/2., ly/2., -lz/2.],
                          model = mod3D, material = tdur, color='WALLx')
    floor.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
    bodies += floor
    
    new_body = brique_Paris.rigidBrick([lx/2.,ly/2.,wall.height+4.e-2],mod3D,plex,'GREEN')
    bodies += new_body
    
    # interation managmeent
    lprpr  = pre.tact_behav(name='iqsg0', law='IQS_CLB_g0', fric=0.3)
    tacts += lprpr
    lprpl  = pre.tact_behav(name='iqsg1', law='IQS_CLB_g0', fric=0.5)
    tacts += lprpl
    lprps  = pre.tact_behav(name='iqsg2', law='IQS_CLB_g0', fric=0.)
    tacts += lprps
    
    svbbbb = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLUEx', behav='iqsg0', 
                           CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLUEx', alert=0.02)
    svs+=svbbbb
    svbrbr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='REDxx', behav='iqsg0', 
                           CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx', alert=0.02)
    svs+=svbrbr
    svbbbr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLUEx', behav='iqsg0', 
                           CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx', alert=0.02)
    svs+=svbbbr
    svprpl = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLUEx', behav='iqsg1', 
                           CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='WALLx', alert=0.02)
    svs+=svprpl
    svprps = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='GREEN', behav='iqsg2', 
                           CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx', alert=0.02)
    svs+=svprps
    
    import os
    if not os.path.isdir('DATBOX'):
      os.mkdir('DATBOX')
    
    # file writing
    pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

    return bodies


def compute( nb_steps, dt, theta, freq_display, cp_tol, polyr_shrink, low_polyr,
             tol, relax, quad, gs_it1, gs_it2, stype ) :

    chipy.checkDirectories()
    
    ### computation's parameters definition ### 
    
    chipy.PRPRx_UseCpF2fExplicitDetection(cp_tol)
    chipy.PRPRx_ShrinkPolyrFaces(polyr_shrink)
    chipy.PRPRx_LowSizeArrayPolyr(low_polyr)
    
    chipy.nlgs_3D_DiagonalResolution()
    
    #nlgs_3D_SetWithQuickScramble()
    
    chipy.SetDimension(3)
    chipy.Initialize()
    
    chipy.utilities_logMes('INIT TIME STEPPING')
    chipy.TimeEvolution_SetTimeStep(dt)
    chipy.Integrator_InitTheta(theta)
    
    chipy.RBDY3_NewRotationScheme()
    
    ### model reading ###
    chipy.ReadDatbox(deformable=False)

    chipy.OpenDisplayFiles()
    
    ### compute masses ###
    chipy.ComputeMass()
    
    for k in range(nb_steps):
    
        if chipy.TimeEvolution_GetStep() == 100:
          chipy.RBDY3_PutBodyVector('Vbeg_',chipy.RBDY3_GetNbRBDY3(),[2.,0.,0.,0.,0.,0.])
        #
        chipy.IncrementStep()
        #
        chipy.ComputeFext()
        chipy.ComputeBulk()
        chipy.ComputeFreeVelocity()
        #
        chipy.SelectProxTactors()
        
        chipy.RecupRloc()
        chipy.ExSolver(stype, quad, tol, relax, gs_it1, gs_it2)
        chipy.StockRloc()
        #
        chipy.ComputeDof()
        chipy.UpdateStep()
    
        chipy.WriteLastDof()
        chipy.WriteLastVlocRloc()
    
        # False is important because it tests the interaction display file
        # writing configuration !
        chipy.WriteDisplayFiles(freq_display)
    
    chipy.WriteLastDof()
    chipy.WriteLastVlocRloc()
    
    chipy.CloseDisplayFiles()


def check_results():

    assert( chipy.inter_handler_3D_getNb( chipy.PRPLx_ID ) == 16 )
    assert( chipy.inter_handler_3D_getNb( chipy.PRPRx_ID ) == 36 )


if __name__ == "__main__":

  lx = 0.22
  ly = 0.11
  lz = 0.06
  
  nb_x    = 3
  nb_rows = 2
  
  joint = 0.01

  bodies = generate(lx, ly, lz, nb_x, nb_rows, joint)

  if( 'with-visu' in sys.argv ):
    pre.visuAvatars(bodies)

  ### computation's parameters definition ### 

  compute_params = { 'nb_steps'    : 250      ,
                     'dt'          : 0.001    ,
                     'theta'       : 0.5      ,
                     'freq_display': 10       ,
                     'cp_tol'      : 1.e-3    ,
                     'polyr_shrink': 0.05     ,
                     'low_polyr'   : 10       ,
                     'tol'         : 0.1666e-3,
                     'relax'       : 1.0      ,
                     'quad'        : 'QM/16'  ,
                     'gs_it1'      : 50       ,
                     'gs_it2'      : 500      ,
                     'stype'       : 'Stored_Delassus_Loops         '
                   }

  compute(**compute_params)


  check_results()

  chipy.Finalize()


