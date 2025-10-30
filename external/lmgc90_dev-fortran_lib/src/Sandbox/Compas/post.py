import sys

from pylmgc90 import pre, chipy

if '--pre' in sys.argv:
  
  dim = 3
  mats, mods, bodies, tacts, sees, inters = pre.readDatbox(dim, 'OUTBOX', step=1)
  pre.visuAvatars(bodies, True, [1.,0.,0.])

else:
  # plage de fichiers a traiter
  n_min=1
  n_max=100
  step = 1
  
  chipy.checkDirectories()
  
  # space dimension
  dim = 3
  mhyp = 0
  
  # not used but needed...
  dt = 1.e-3
  
  freq_display = 1
  freq_write = 1
  
  ######## state 0 ###########################
  
  ### computation's parameters definition ### 
  chipy.SetDimension(dim,mhyp)
  
  #utilities_logMes('INIT TIME STEPPING')
  #TimeEvolution_SetTimeStep(dt)
  
  ### model reading ###
  chipy.ReadDatbox(deformable=False)
  
  chipy.OpenDisplayFiles()
  for k in range(n_min,n_max+1,step):
      #
      chipy.utilities_logMes('READ INI')
      chipy.ReadIni(k)
      chipy.WriteDisplayFiles(freq=1)
    
  chipy.CloseDisplayFiles()


