import sys
import subprocess
from pathlib import Path

import numpy as np

from pylmgc90 import pre, chipy

def run_cmd(cmd):
    p = subprocess.Popen( cmd, stdout=subprocess.PIPE, shell=True)
    out, err = p.communicate()
    p.wait()
    if p.returncode != 0 :
      print(out)
      sys.exit(p.returncode)

# computation part...

def init(dim, dt, h5_file, step=0):

  theta = 0.5

  if dim == 2:
    wd = Path('2D')
  else :
    wd = Path('3D')
  chipy.overall_SetWorkingDirectory(str(wd))

  chipy.checkDirectories()
  
  # desactivation des messages de log
  chipy.utilities_DisableLogMes()

  chipy.Initialize()

  chipy.SetDimension(dim)
  ### definition des parametres du calcul ### 

  chipy.utilities_logMes('INIT TIME STEPPING')
  chipy.TimeEvolution_SetTimeStep(dt)
  chipy.Integrator_InitTheta(theta)

  ### model reading ###
  chipy.ReadDatbox(deformable=False)
  if( step > 0 ):
    chipy.utilities_logMes('READ HDF5')
    chipy.io_hdf5_read(h5_file,step)


  ### post2D ##
  if step:
    chipy.OpenDisplayFiles(step+1)
  else:
    chipy.OpenDisplayFiles()
  chipy.InitHDF5(h5_file)

  chipy.utilities_logMes('COMPUTE MASS')
  chipy.ComputeMass()


#def get_this(ids):
#  inters = {}
#  for i in ids:
#    nb = chipy.inter_handler_2D_tgetNb(i)
#    inters[i] = np.zeros( [nb,11], float )
#    for j in range(nb):
#        inters[i][j,:] = chipy.inter_handler_2D_tgetRData(i,j+1)
#
#  return inters
#    
#def get_vrlt(ids):
#  verlts = {}
#  for i in ids:
#    verlts[i] = chipy.inter_handler_2D_getAll(i)
#
#  return verlts
    
def comp(nb_steps):

  # bavardage de certaines fonctions
  echo = 0

  # info generation fichier visu
  freq_display = 1

  # info contact
  #         123456789012345678901234567890
  stype  = 'Stored_Delassus_Loops         '
  norm   = 'Quad '
  tol    = 0.1666e-3
  relax  = 1.0
  gs_it1 = 51
  gs_it2 = 1001

  ids = [chipy.DKDKx_ID, chipy.DKJCx_ID]
  for k in range(nb_steps):
     #
     chipy.utilities_logMes('INCREMENT STEP')
     chipy.IncrementStep()

     chipy.utilities_logMes('COMPUTE Fext')
     chipy.ComputeFext()

     chipy.utilities_logMes('COMPUTE Fint')
     chipy.ComputeBulk()

     chipy.utilities_logMes('COMPUTE Free Vlocy')
     chipy.ComputeFreeVelocity()
     #
     chipy.utilities_logMes('SELECT PROX TACTORS')
     chipy.SelectProxTactors()
     #
     chipy.RecupRloc()
     chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
     chipy.StockRloc()
     #
     chipy.utilities_logMes('COMPUTE DOF')
     chipy.ComputeDof()
     #
     chipy.utilities_logMes('UPDATE DOF')
     chipy.UpdateStep()
     #
     chipy.utilities_logMes('WRITE FILE')
     chipy.WriteHDF5()
     #
     ### post2D ###
     chipy.WriteDisplayFiles(freq_display)


def finalize():

  chipy.CloseDisplayFiles()
  chipy.Finalize()


