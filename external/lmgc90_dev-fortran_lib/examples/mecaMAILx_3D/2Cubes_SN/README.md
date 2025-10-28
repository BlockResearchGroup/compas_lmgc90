This folder contains various examples scripts using Siconos Numerics solvers.

 * `command_SN_nlgs.py`: local Gauss-Seidel (same as the one built-in lmgc90)
 * `command_SN_nsgs.py`: global Gauss-Seidel
 * `command_SN_globalac.py`: global Alart-Curnier solver
 * `command_SN_globaladmm.py`: global alternate direction method
 * `command.py`: the built-in lmgc90's solver

Beware of differences between scripts for local and global solvers.  

First run `gen_sample.py` and then run a `command_xxx.py` script,
it will create a `xxx` subfolder with the results inside it (except
for `command.py` script which will go to `standard` subfolder).

