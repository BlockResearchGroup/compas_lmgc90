import os,sys
import shutil
from pylmgc90 import chipy

#from functions_3D import *
try:
  shutil.rmtree("POSTPRO")
  shutil.rmtree("DISPLAY")
  shutil.rmtree("OUTBOX")
except:

  pass

chipy.checkDirectories()

chipy.Initialize()

### computation's parameters definition ###

dt =5e-2
nb_steps=10000

theta = 0.5

freq_display = 100
freq_write = 1000

ref_radius=10.


chipy.PT3Dx_SetDisplayRadius(2*ref_radius)

freq_detect = 1
chipy.PRPRx_UseCpCundallDetection(200)
chipy.PRPRx_LowSizeArrayPolyr(40)
#
#PRPRx_SetF2fMinimalSurfaceSize(1)

tol = 1e-3
relax = 1.0
quad = 'Quad '
gs_it1 = 50
gs_it2 = 250
#nlgs_3D_DiagonalResolution()

chipy.SetDimension(3)

print('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

#chipy.RBDY3_NewRotationScheme()

### model reading ###
chipy.ReadDatbox(deformable=False)

chipy.PTPT3_LoadNetwork()
chipy.PTPT3_LoadParams()

### post3D ##
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

### compute masses ###
chipy.ComputeMass()

Nbpart=chipy.RBDY3_GetNbRBDY3()

for k in range(nb_steps):
    print( 'INCREMENT STEP')
    chipy.IncrementStep()
    #
    print( 'COMPUTE Fext')
    chipy.ComputeFext()
    #
    print( 'COMPUTE Fint')
    chipy.ComputeBulk()
    #
    print( 'COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()
    #
    print( 'SELECT PROX TACTORS')
    chipy.SelectProxTactors(freq_detect)

    if k==0: chipy.inter_handler_3D_stockRloc(chipy.PTPT3_ID)

    chipy.RecupRloc()
    chipy.ExSolver('Stored_Delassus_Loops         ',quad, tol, relax, gs_it1, gs_it2)

    #nlgs_3D_UpdateTactBehav()
    chipy.StockRloc()
    #
    print( 'COMPUTE DOF')
    chipy.ComputeDof()

    print( 'UPDATE DOF')
    chipy.UpdateStep()

    print( 'WRITE OUT')
    chipy.WriteOut(freq_write)
    #
    print( 'WRITE DISPLAY')
    chipy.WriteDisplayFiles(freq_display)

    ### postpro ###
    chipy.WritePostproFiles()

chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

chipy.Finalize()
