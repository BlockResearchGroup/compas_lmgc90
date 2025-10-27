
import itertools

import utilities

for dim, incr, load in itertools.product( [2, 3,], [False, True,], ['Normal', 'Tangent', ] ):

  path = f"{dim}D_{'incre' if incr else 'monor'}_{load}"

  print( f'running {path}' )
  utilities.generate(dim,incr,load,path)
  utilities.compute(dim, path)
  utilities.post_plot(dim,incr,load,path)

