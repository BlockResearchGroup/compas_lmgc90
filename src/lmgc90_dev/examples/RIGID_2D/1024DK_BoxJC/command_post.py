
from pylmgc90 import chipy

# plage de fichiers a traiter
n_min=1
n_max=30
step = 1

chipy.checkDirectories()

# space dimension
dim = 2
mhyp = 1

# un pas de temps bidon ?
dt = 2.e-4

freq_display = 1
freq_write = 1

######## etat 0 ###########################

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


