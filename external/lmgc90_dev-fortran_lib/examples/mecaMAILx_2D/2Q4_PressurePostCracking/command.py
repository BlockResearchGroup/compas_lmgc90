import os, sys
import shutil

import numpy as np

if '--novisu' in sys.argv:
  with_figure = False
else:
  from matplotlib import pyplot as plt
  with_figure = True

# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
#chipy.utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim = 2

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt = 5.e-6
t_final = 1.e-3
dt_min = dt
dt_max = dt

NR_max_iter = 20
NR_adapt = 9999999
NR_tol = 1.e-3

# theta integrator parameter
theta = 0.5

# deformable  yes=1, no=0
deformable = 1
nb_CL_by_Edge = 2

# interaction parameters
freq_detect = 1
Rloc_tol = 5.e-2

# nlgs parameters
tol = 1e-5
relax = 1.0
norm = 'Quad '
gs_it1 = 50
gs_it2 = 10
solver_type='Stored_Delassus_Loops         '

# write parameter
freq_write   = 100

# display parameters
freq_display = 200

# for customized postpro
nbsteps   = int( t_final/dt )

# variable for pre-damaged

#Value of beta at initial time
betamin_ini = 0.7

# 
is_first_time = True

#
# read and load
#

# Set space dimension
chipy.SetDimension(dim,mhyp)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

# Newton loop parameters:
chipy.NewtonRaphson_SetFinalTime(t_final)
chipy.NewtonRaphson_SetMinTimeStep(dt_min)
chipy.NewtonRaphson_SetMaxTimeStep(dt_max)
chipy.NewtonRaphson_SetMaxIter(NR_max_iter)
chipy.NewtonRaphson_SetIncPatience(NR_adapt)

#
chipy.ReadDatbox(deformable)
chipy.CLxxx_SetNbNodesByCLxxx(nb_CL_by_Edge)

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

chipy.registerInterInternals(['beta', '#taille_ele', 'saut_de_un', 'TPSini'])

# const                                     p0    dp/dt   -    -
chipy.tact_behav_SetPressureParameters(1,2,[1.e5, 1.e4, 0.   , 0.   ])
# lin                                       p0    dp    tau    alpha
chipy.tact_behav_SetPressureParameters(2,2,[1.e5, 1.e4, 1.e-2, 1.   ])
# exp                                       p0    dp    tau    alpha
chipy.tact_behav_SetPressureParameters(3,3,[1.e5, 1.e4, 1.e-2, 1.   ])
# ext                                        -     -     -    alpha
chipy.tact_behav_SetPressureParameters(4,4,[0.  , 0.  , 0.   , 1.   ])
#
# simulation part ...
#
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

while chipy.TimeEvolution_GetTime() < t_final :
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()

  # Newton loop
  chipy.NewtonRaphson_Initialize(NR_tol)
  is_converged = 1
  k=0

  #looping until something changes in CheckConvergence
  while is_converged == 1 :
    k+=1
    chipy.utilities_logMes('COMPUTE BULK')
    chipy.ComputeBulk()

    chipy.utilities_logMes('ASSEMB RHS/KT')
    chipy.AssembleMechanicalRHS()
    chipy.AssembleMechanicalLHS()

    chipy.utilities_logMes('COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()
    #
    chipy.utilities_logMes('SELECT PROX TACTORS')
    chipy.SelectProxTactors(freq_detect)
    #
    ### Signorini Coulomb
    chipy.RecupRloc()
    chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
    chipy.UpdateTactBehav()
    chipy.StockRloc()


    chipy.utilities_logMes('COMPUTE DOF')
    chipy.ComputeDof()
    #
    if k > 1:
      NR_norm = chipy.mecaMAILx_ComputeResidueNorm()
      is_converged = chipy.NewtonRaphson_CheckConvergence(NR_norm)

  ### end while NR

  chipy.utilities_logMes('COMPUTE TIME STEP')
  #istate = 1 => redo step
  #istate = 2 => stop

  istate = chipy.NewtonRaphson_ComputeTimeStep()

  if not istate == 1 :

    chipy.utilities_logMes('UPDATE TACT BEHAV')
    chipy.UpdateTactBehav()

    # Looping to change all beta of all tact law
    if is_first_time:
      inters = chipy.getInteractions(this=True)
      chipy.setInternalArray('beta'  , inters, betamin_ini)
      chipy.setInternalArray('TPSini', inters, 0.)

      # to draw
      inter2draw =  np.zeros([inters.size, nbsteps, 5], dtype=float)

      # this is it
      is_first_time = False

    # because setInternalArray do a 'tset'
    chipy.StockRloc()

    chipy.utilities_logMes('UPDATE DOF')
    chipy.UpdateStep()
    #
    ### write results ###
    #
    chipy.WriteOut(freq_write)

    # collecting data
    inters = chipy.getInteractions()
    nstep = chipy.TimeEvolution_GetStep()
    time  = chipy.TimeEvolution_GetTime()

    beta = chipy.getInternalArray('beta', inters)

    inter2draw[:,nstep-1,0] = chipy.getInternalArray('saut_de_un', inters)
    inter2draw[:,nstep-1,1] = inters['rl'][:,1]
    inter2draw[:,nstep-1,2] = beta[:]
    inter2draw[:,nstep-1,3] =  chipy.getInternalArray('#taille_ele', inters)
    inter2draw[:,nstep-1,4] = time

    chipy.WriteDisplayFiles(freq_display, beta=('ptc',beta))
    chipy.WritePostproFiles()

    if istate == 2 :
      # istate => Stop
      break

### end while time loop ###

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()

dossier = 'Results'

if os.path.isdir(dossier) :
  shutil.rmtree(dossier)  

os.mkdir(dossier)

nomFichier = ['PConst','Plin','Pexp','Pext']

#Write tact law variable
for j in  range(len(nomFichier)):
    with open('./'+dossier+'/'+nomFichier[j]+'.txt','w') as out:
        out.write("beta Gap Rn lc time"+"\n")
        out.write("\n")
        for i in range( nbsteps - 1 ) :
            out.write('%.2e %.2e %.2e %.2e %.2e \n' % (inter2draw[j,i,2],inter2draw[j,i,0],inter2draw[j,i,1],inter2draw[j,i,3],inter2draw[j,i,4]))

#Figures 
if with_figure:
    for j in  range(len(nomFichier)):

        plt.figure()
        plt.subplot(3,1,1)
        plt.xlabel('[u]'+' '+r'$\mu$ m')
        plt.ylabel('Rn [MPa]')
        plt.ylim(-0.6,0)
        plt.plot(inter2draw[j,:,0]*1e6,inter2draw[j,:,1]*1e-5)

        plt.subplot(3,1,3)
        plt.xlabel('temps'+' '+r'ms')
        plt.ylabel('Rn [MPa]')
        plt.ylim(-0.6,0)
        plt.plot(inter2draw[j,:,4]*1e3,inter2draw[j,:,1]*1e-5)

        plt.suptitle(nomFichier[j])

        plt.savefig('./'+dossier+'/'+nomFichier[j]+'.png')
