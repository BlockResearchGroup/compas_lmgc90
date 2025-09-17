from pathlib import Path

import numpy as np

from pylmgc90 import pre

def gen_2D():

    # generation part...
    path = Path('2D/DATBOX')
    path.mkdir(parents=True, exist_ok=True)

    dim = 2

    bodies = pre.avatars()
    mats   = pre.materials()
    mods   = pre.models()
    svs    = pre.see_tables()
    tacts  = pre.tact_behavs()

    tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
    plex = pre.material(name='PLEXx',materialType='RIGID',density=100.)
    mats.addMaterial(tdur,plex)

    mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
    mods.addModel(mod)

    nb_ele   = 5
    nb_layer = 3

    rmin = 0.5
    rmax = 0.51
    l = 2.*rmax

    coor = pre.triangularLattice2D(nb_ele, nb_layer, l, orientation='up')

    nb_particles = coor.shape[0]
    lx, ly = np.amax(coor,axis=0)-np.amin(coor,axis=0)+2*rmax

    radii = pre.granulo_Random(nb_particles, rmin, rmax, 1)

    radius_min=np.amin(radii)
    radius_max=np.amax(radii)

    for r, c in zip(radii, coor):
       body = pre.rigidDisk(r=r, center=c, model=mod, material=plex, color='BLEUx') 
       bodies += body

    down = pre.rigidJonc(axe1=0.5*lx+radius_max, axe2=radius_max, center=[0.5*lx, -radius_max],
                         model=mod, material=tdur, color='WALLx')
    bodies += down

    down.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

    ldkdk = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
    tacts += ldkdk

    svdkdk = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='BLEUx', behav=ldkdk,
                           CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='BLEUx', alert=0.1*radius_min)
    svs += svdkdk

    svdkjc = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='BLEUx'   , behav=ldkdk, 
                           CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='WALLx', alert=0.1*radius_min)
    svs += svdkjc

    pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, datbox_path=path)


def gen_3D():

    # generation part...
    path = Path('3D/DATBOX')
    path.mkdir(parents=True, exist_ok=True)

    dim = 3

    bodies = pre.avatars()
    mats   = pre.materials()
    mods   = pre.models()
    svs    = pre.see_tables()
    tacts  = pre.tact_behavs()

    tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
    plex = pre.material(name='PLEXx',materialType='RIGID',density=100.)
    mats.addMaterial(tdur,plex)

    mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
    mods.addModel(mod)

    nb_ele   = 5
    nb_layer = 3

    nb_particles = nb_ele*nb_ele*nb_layer

    rmin = 0.5
    rmax = 0.51
    radii = pre.granulo_Random(nb_particles, rmin, rmax, 1)

    radius_min=np.amin(radii)
    radius_max=np.amax(radii)

    l = 2.*radius_max

    lx, ly, lz = nb_ele*l, nb_ele*l, nb_layer*l

    coor = pre.cubicLattice3D(nb_ele, nb_ele, nb_layer, l)
    coor.shape = [nb_particles,3]

    for r, c in zip(radii, coor):
       body = pre.rigidSphere(r=r, center=c, model=mod, material=plex, color='BLUEx') 
       bodies += body

    down = pre.rigidPlan(axe1=0.5*lx+radius_min, axe2=0.5*ly+radius_min, axe3=radius_min, center=[0.5*lx, 0.5*ly, -radius_min],
                         model=mod, material=tdur, color='WALLx')
    bodies += down

    down.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

    lspsp = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
    tacts += lspsp

    svspsp = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='SPHER', colorCandidat   ='BLUEx', behav=lspsp,
                           CorpsAntagoniste='RBDY3', antagoniste='SPHER', colorAntagoniste='BLUEx', alert=0.1*radius_min)
    svs += svspsp

    svsppl = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='SPHER', colorCandidat   ='BLUEx'   , behav=lspsp, 
                           CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='WALLx', alert=0.1*radius_min)
    svs += svsppl

    pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, datbox_path=path)


