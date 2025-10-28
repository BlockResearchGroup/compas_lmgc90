# simple test to run by hand
# to check if plot works correctly

# matplotlib must be available from here...

import math
from pathlib import Path

from pylmgc90 import pre

dim = 2
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mat = pre.material(name='TDURx', materialType='RIGID', density=1000.)

disk  = pre.rigidDisk(r=0.1, center=[0.0,0.0 ], model=mod, material=mat)


disk.imposeDrivenDof(component=1, dofty='vlocy', description='predefined',
                     ct=0.2, amp=0.8, omega=2*math.pi, phi=0.5*math.pi,
                     rampi=-0.2, ramp=0.2, plot_time_range=[0.,10.])

#should not plot anything since the file does not exist
fname = Path('vimp.txt')
disk.imposeDrivenDof(component=2, dofty='vlocy', description='evolution',
                     evolutionFile=fname, plot_time_range=[0.,1.])

with open(fname, 'wt') as f:
  f.write(f"{0.  } {0.}\n")
  f.write(f"{0.2 } {0.}\n")
  f.write(f"{0.3 } {1.}\n")
  f.write(f"{0.8 } {1.}\n")
  f.write(f"{0.81} {0.}\n")

disk.imposeDrivenDof(component=3, dofty='vlocy', description='evolution',
                     evolutionFile=fname, plot_time_range=[0.,1.])


fname.unlink()
