from pathlib import Path
import math

from pylmgc90 import pre
from pylmgc90 import chipy
from pylmgc90.chipy import computation

def generate(nb, rmin=0.5, rmax=2.0, ratio=1.5):

  datbox = Path('./DATBOX')
  datbox.mkdir(exist_ok=True)
  
  # 2D
  dim = 2
  
  # containers
  bodies = pre.avatars()
  mats   = pre.materials()
  mods   = pre.models()
  svs    = pre.see_tables()
  tacts  = pre.tact_behavs()
  
  # model
  mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
  mods.addModel(mod)
  
  # materials
  tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
  plex = pre.material(name='PLEXx',materialType='RIGID',density=100.)
  mats.addMaterial(tdur,plex)
  
  
  radii = pre.granulo_Random(nb, rmin, rmax)
  radius_min = min(radii)
  radius_max = max(radii)
  
  # deposit on 2D lattice
  nx = int( math.sqrt( nb*ratio ) )
  ny = int( nx / ratio )
  coors = pre.triangularLattice2D(nx, ny, 2*radius_max, x0=0., y0=0.)

  lx = nx*2*rmax
  ly = ny*2*rmax

  for r, c in zip(radii, coors):
      body = pre.rigidDisk(r=r, center=c, model=mod, material=plex, color='BLUEx') 
      bodies += body
  
  # lower wall
  down = pre.rigidJonc(axe1=0.5*lx+radius_max, axe2=radius_max, center=[0.5*lx, -radius_max],
                       model=mod, material=tdur, color='WALLx')
  down.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
  bodies += down
  left = pre.rigidJonc(axe1=0.5*ly+radius_max, axe2=radius_max, center=[-radius_max, 0.5*ly],
                       model=mod, material=tdur, color='WALLx')
  left.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
  left.rotate('axis', center=left.getNodeCoor(), alpha=0.5*math.pi)
  bodies += left
  right = pre.rigidJonc(axe1=0.5*ly+radius_max, axe2=radius_max, center=[lx+radius_max, 0.5*ly],
                       model=mod, material=tdur, color='WALLx')
  right.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
  right.rotate('axis', center=right.getNodeCoor(), alpha=0.5*math.pi)
  bodies += right
  
  # interactions management:
  ldkdk = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.)
  tacts += ldkdk
  
  svdkdk = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='BLUEx', behav=ldkdk,
                         CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='BLUEx', alert=0.1*radius_min)
  svs+=svdkdk
  
  svdkjc = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='BLUEx', behav=ldkdk,
                         CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='WALLx', alert=0.1*radius_min)
  svs+=svdkjc
  
  post = pre.postpro_commands()
  my_command=pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
  post.addCommand(my_command)
  
  # input files writing
  pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post, datbox_path=datbox)

  return bodies
