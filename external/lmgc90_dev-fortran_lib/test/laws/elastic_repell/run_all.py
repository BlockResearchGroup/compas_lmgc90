
import itertools

import utilities

for dim, variant in itertools.product( [2, 3,], ['', 'g0', 'adapt',] ):

  path = f"{dim}D{'_'+variant if variant else ''}"

  print( f'running {path}' )
  utilities.generate(dim,variant,path)
  utilities.compute(dim, path)
  utilities.post_plot(dim,variant,path)

