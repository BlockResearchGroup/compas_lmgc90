import sys

import generate
import utils

try:
  dim = int( sys.argv[1] )
except Exception as e:
  print( '[ERROR] script must be run with argument 2 or 3 to decide dimension' )
  raise e

if dim == 2:
    generate.gen_2D()
else:
    generate.gen_3D()

# First computation generate and compute 30 time step with dt=0.01

####
# info gestion du temps
dt = 1.e-2
nb_steps_1 = 30

ref_h5_file = 'ref_lmgc90.h5'

utils.init(dim, dt, ref_h5_file)
utils.comp(nb_steps_1)

utils.finalize()

# Second computation, compute 15 steps then run a second a computation with the next 15 steps
step_ini   = nb_steps_1 // 2
nb_steps_2 = nb_steps_1 - step_ini
h5_file    = 'lmgc90.h5'

utils.init(dim, dt, h5_file)
utils.comp(nb_steps_2)
utils.finalize()

utils.init(dim, dt, h5_file, step_ini)
utils.comp(nb_steps_2)
utils.finalize()


# Checking result:
cmd = f"h5diff {dim}D/{ref_h5_file} {dim}D/{h5_file}"
utils.run_cmd(cmd)

