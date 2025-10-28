
import os, sys
sys.path.append( os.getcwd()+'/../..' )

import computation

####
dim = 2

# info gestion du temps
dt = 1.e-2
theta = 0.5
nb_steps = 200

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_write   = 1
freq_display = 1

# info contact

#         123456789012345678901234567890
stype  = 'Stored_Delassus_Loops         '
norm   = 'Quad '
tol    = 1.5e-4
relax  = 1.0
gs_it1 = 50
gs_it2 = 1000

computation.initialize(dim, dt, theta, h5_file='lmgc90.h5', logmes=echo)

for k in range(1, nb_steps + 1):
   #
   #chipy.utilities_logMes('itere : '+str(k))
   #
   computation.one_step(stype, norm, tol, relax, gs_it1, gs_it2,
                        freq_write, freq_display   )

computation.finalize()

