
import os, sys


# import des modules
import math, numpy

from pylmgc90 import pre

pre.setStopMode('exception')

if( not os.path.isdir('DATBOX') ):
   os.mkdir('DATBOX')

bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
tacts  = pre.tact_behavs()
svs    = pre.see_tables()

dim = 2

mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

mat = pre.material(name='TDURx', materialType='RIGID', density=1000.)
mats.addMaterial(mat)

joncx = pre.rigidJonc(axe1=1., axe2=0.1, center=[0., -0.1], model=mod, material=mat, color='JONCx')
joncx.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
bodies.addAvatar(joncx)
   
disk1 = pre.rigidDisk(r=0.1, center=[-0.8,0.1 ], model=mod, material=mat, color='DISKx')
disk2 = pre.rigidDisk(r=0.1, center=[-0.8,0.3 ], model=mod, material=mat, color='DISKx')
disk3 = pre.rigidDisk(r=0.1, center=[-0.3,0.5 ], model=mod, material=mat, color='DISKx')
disk3.addContactors(shape='PT2Dx', color='DISKx', shift=[-0.1,0.])
disk4 = pre.rigidDisk(r=0.1, center=[ 0.8,0.55], model=mod, material=mat, color='DISKx')
disk5 = pre.rigidDisk(r=0.1, center=[ 0.8,0.15], model=mod, material=mat, color='DISKx')

ksid = pre.rigidDisk(r=0.2, center=[ 0.8,0.65], model=mod, material=mat, color='xKSID', is_Hollow=True)

vertices = numpy.array([-0.1,-0.1, 0.1,-0.1, 0.1,0.1, -0.1,0.1])
vertices.shape = [4,2]
polyg1 = pre.rigidPolygon(center=[-1.0,0.1], nb_vertices=4, vertices=vertices, model=mod, material=mat, color='POLYG', generation_type='full')
polyg2 = pre.rigidPolygon(center=[-1.0,0.3], nb_vertices=4, vertices=vertices, model=mod, material=mat, color='POLYG', generation_type='full')
polyg2.addContactors(shape='PT2Dx', color='POLYG', shift=[0.1,0.1])

vertices[0,0] =-0.3                ; vertices[0,1] = 0.5+math.sqrt(0.02)
vertices[1,0] =-0.2+math.sqrt(0.02); vertices[1,1] = 0.4
vertices[2,:] = vertices[1,:]+0.1*math.sqrt(2.)
vertices[3,:] = vertices[0,:]+0.1*math.sqrt(2.)
polyg3 = pre.rigidPolygon(center=[0.,0.], nb_vertices=4, vertices=vertices, model=mod, material=mat, color='POLYG', generation_type='full')

#manque DISKL
#manque DKDKL

bodies.addAvatar(disk1)
bodies.addAvatar(disk2)
bodies.addAvatar(disk3)
bodies.addAvatar(disk4)
bodies.addAvatar(disk5)
bodies.addAvatar(ksid)
bodies.addAvatar(polyg1)
bodies.addAvatar(polyg2)
bodies.addAvatar(polyg3)

modl= pre.model(name='M2D_L', physics='MECAx', element='Q4xxx', dimension=dim, external_model='MatL_',
                kinematic='large', formulation='TotaL', material='neoh_', anisotropy='iso__',
                mass_storage='lump_')
mods.addModel(modl)

matl= pre.material(name='steel', materialType='ELAS', density=7850., elas='standard', anisotropy='isotropic',
                   young=2.05e11, nu=0.3)  
mats.addMaterial(matl)

from copy import deepcopy
mesh  = pre.buildMesh2D('Q4', x0=-0.6, y0=0.0, lx=1.2, ly=0.2, nb_elem_x=10, nb_elem_y=5)
mesh1 = pre.buildMeshedAvatar(mesh=mesh, model=modl, material=matl)
mesh1.addContactors(group='up', shape='ALpxx', color='ALpxx')
mesh1.addContactors(group='down', shape='CLxxx', color='CLxxx')
mesh2 = deepcopy(mesh1)
mesh2.translate(dy=0.2)
mesh2.addContactors(group='left' , shape='PT2DL', color='PT2CD',weights=[0.5])
mesh2.addContactors(group='right', shape='PT2DL', color='PT2AN',weights=[0.5])

bodies.addAvatar(mesh1)
bodies.addAvatar(mesh2)

# definition d'une loi de contact frottant, avec pre-gap
iqsc0=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)
gapc0=pre.tact_behav(name='gapc0', law='GAP_SGR_CLB', fric=0.5)
elas0=pre.tact_behav(name='elas0', law='ELASTIC_WIRE', stiffness=5.e3, prestrain=-0.2)

tacts.addBehav(iqsc0)
tacts.addBehav(gapc0)
tacts.addBehav(elas0)

sv1 = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='DISKx', behav=iqsc0,
                    CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='DISKx', alert=0.05)

sv2 = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='DISKx', behav=iqsc0,
                    CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='JONCx', alert=0.05)

sv3 = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='DISKx', behav=iqsc0,
                    CorpsAntagoniste='RBDY2', antagoniste='xKSID', colorAntagoniste='xKSID', alert=0.05)

sv4 = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='DISKx', behav=gapc0,
                    CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='ALpxx', alert=0.05)

sv5 = pre.see_table(CorpsCandidat='RBDY2', candidat='POLYG', colorCandidat='POLYG', behav=iqsc0,
                    CorpsAntagoniste='RBDY2', antagoniste='POLYG', colorAntagoniste='POLYG', alert=0.05)

sv6 = pre.see_table(CorpsCandidat='RBDY2', candidat='POLYG', colorCandidat='POLYG', behav=iqsc0,
                    CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='JONCx', alert=0.05)

sv7 = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='DISKx', behav=iqsc0,
                    CorpsAntagoniste='RBDY2', antagoniste='POLYG', colorAntagoniste='POLYG', alert=0.05)

sva = pre.see_table(CorpsCandidat='RBDY2', candidat='POLYG', colorCandidat='POLYG', behav=gapc0,
                   CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='ALpxx', alert=0.05)

svb = pre.see_table(CorpsCandidat='MAILx', candidat='CLxxx', colorCandidat='CLxxx', behav=gapc0,
                    CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='ALpxx', alert=0.05)

svc = pre.see_table(CorpsCandidat='MAILx', candidat='CLxxx', colorCandidat='CLxxx', behav=gapc0,
                    CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='JONCx', alert=0.05)

#svd = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='DISKx', behav=gapc0,
#                    CorpsAntagoniste='MAILx', antagoniste='DISKL', colorAntagoniste='DISKL', alert=0.05)

sve = pre.see_table(CorpsCandidat='RBDY2', candidat='PT2Dx', colorCandidat='DISKx', behav=elas0,
                    CorpsAntagoniste='RBDY2', antagoniste='PT2Dx', colorAntagoniste='POLYG', alert=0.6)

svf = pre.see_table(CorpsCandidat='MAILx', candidat='PT2DL', colorCandidat='PT2CD', behav=elas0,
                    CorpsAntagoniste='MAILx', antagoniste='PT2DL', colorAntagoniste='PT2AN', alert=1.2)


svs.addSeeTable(sv1)
svs.addSeeTable(sv2)
svs.addSeeTable(sv3)
svs.addSeeTable(sv4)
svs.addSeeTable(sv5)
svs.addSeeTable(sv6)
svs.addSeeTable(sv7)
svs.addSeeTable(sva)
svs.addSeeTable(svb)
svs.addSeeTable(svc)
#svs.addSeeTable(svd)
svs.addSeeTable(sve)
svs.addSeeTable(svf)

#try:
#  pre.visuAvatars(bodies)
#except:
#  pass

# ecriture des fichiers de donnees pour LMGC90
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

from pylmgc90 import chipy

def init_lmgc90(dt, theta, dim, mhyp, deformable, output_file):

  chipy.Initialize()
  
  chipy.checkDirectories()
  #chipy.utilities_DisableLogMes()

  chipy.SetDimension(dim,mhyp)

  chipy.TimeEvolution_SetTimeStep(dt)
  chipy.Integrator_InitTheta(theta)

  chipy.ReadDatbox(deformable)

  #TimeEvolution_WriteLastDof()
  #mecaMAILx_WriteLastDof()
  chipy.CLxxx_SetNbNodesByCLxxx(1)
  
  chipy.utilities_logMes('DISPLAY & WRITE')
  chipy.InitHDF5(output_file)
  chipy.PT2Dx_SetDisplayRadius(0.02)
  chipy.OpenDisplayFiles()
  #chipy.OpenPostproFiles()
  
  # since constant compute elementary mass matrices once
  chipy.utilities_logMes('COMPUTE MASS')
  chipy.ComputeMass()
  
  # since constant compute elementary stiffness matrices once
  chipy.utilities_logMes('COMPUTE STIFFNESS')
  chipy.ComputeBulk()
  
  # since constant compute iteration matrix once
  chipy.AssembleMechanicalLHS()
  
def compute_lmgc90_one_step(freq_display, freq_write, solver_param):
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()
  #
  #utilities_logMes('DISPLAY TIMES')
  #TimeEvolution_DisplayStep()
  #
  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()
  #
  chipy.utilities_logMes('COMPUTE Fint')
  chipy.ComputeBulk()
  #
  chipy.utilities_logMes('ASSEMBLAGE')
  chipy.AssembleMechanicalRHS()
  #
  chipy.utilities_logMes('COMPUTE Free Vlocy')
  chipy.ComputeFreeVelocity()
  #
  chipy.utilities_logMes('SELECT PROX TACTORS')
  chipy.SelectProxTactors()
  #
  chipy.utilities_logMes('RESOLUTION' )
  chipy.RecupRloc()
  #
  
  chipy.ExSolver(**solver_param)
  chipy.UpdateTactBehav()

    
  #
  chipy.StockRloc()
  #
  chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
  chipy.ComputeDof()
  #
  chipy.utilities_logMes('UPDATE DOF, FIELDS')
  chipy.UpdateStep()
  #
  chipy.utilities_logMes('WRITE OUT DOF')
  chipy.WriteOutDof(freq_write)
  #
  chipy.utilities_logMes('WRITE OUT GPV')
  chipy.WriteOutGPV(freq_write)
  #
  chipy.utilities_logMes('WRITE OUT Rloc')
  chipy.WriteOutVlocRloc(freq_write)
  #
  chipy.WriteHDF5(freq_write)
  #
  chipy.utilities_logMes('VISU & POSTPRO')
  chipy.WriteDisplayFiles(freq_display)
  #chipy.WritePostproFiles()


def finalize_lmgc90():
   #
   # close display & postpro
   #
   chipy.CloseDisplayFiles()
   #chipy.ClosePostproFiles()
   
   # this is the end
   chipy.Finalize()

def check_results():

  results = { chipy.CLALp_ID : (11, chipy.CLxxx_GetNbCLxxx) ,
              chipy.CLJCx_ID : (11, chipy.CLxxx_GetNbCLxxx) ,
              chipy.DKALp_ID : ( 3, chipy.DISKx_GetNbDISKx) ,
              chipy.DKDKx_ID : ( 1, chipy.DISKx_GetNbDISKx) ,
              chipy.DKJCx_ID : ( 2, chipy.DISKx_GetNbDISKx) ,
              chipy.DKKDx_ID : ( 2, chipy.DISKx_GetNbDISKx) ,
              chipy.DKPLx_ID : ( 5, chipy.DISKx_GetNbDISKx) ,
              chipy.PLALp_ID : ( 1, chipy.POLYG_GetNbPOLYG) ,
              chipy.PLJCx_ID : ( 1, chipy.POLYG_GetNbPOLYG) ,
              chipy.PLPLx_ID : ( 2, chipy.POLYG_GetNbPOLYG) ,
              chipy.PTPT2_ID : ( 1, chipy.PT2Dx_GetNbPT2Dx) ,
              chipy.P2P2L_ID : ( 5, chipy.PT2DL_GetNbPT2DL) ,
            }

  interNames = chipy.parameters_getInteractionNames()

  for k, v in results.items():
    # check detection
    getNbCd     = v[1]
    nbInter_ref = v[0]
    nbInter     = chipy.inter_handler_2D_getNb(k)

    assert( nbInter_ref == nbInter ), "{}: wrong number of inter found {} instead of {}".format(interNames[k-1], nbInter, nbInter_ref)

    # check that verlet accessor gives the same result
    # in number... should check value but get ic/antac on inter is missing
    count = 0
    for iv in range( 1, getNbCd()+1 ):
      nbv = chipy.inter_handler_2D_getVerletAdjsz(k, iv)
      for adj in range(1, nbv+1 ):
        iantac = chipy.inter_handler_2D_getVerletIantac(k, iv, adj)
        count += 1

    assert( count == nbInter )

  # check that real data of this array are accessible:
  for k in results.keys():
    for i_inter in range( chipy.inter_handler_2D_tgetNb(k) ):
        rdata = chipy.inter_handler_2D_tgetRData(k, i_inter+1)
        print( f"{interNames[k-1]}: inter {i_inter+1} -> {rdata}" )



# space dimension
dim = 2

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt = 1e-3
nb_steps = 20

# theta integrator parameter
theta = 0.501

# deformable  yes=1, no=0
deformable = 1

# interaction parameters
Rloc_tol = 5.e-3

# nlgs parameters
solver_param = { 'conv'  : 1e-4   ,
                 'relax' : 1.0    ,
                 'norm'  : 'Quad ',
                 'gsit1' : 50     ,
                 'gsit2' : 1000   ,
                 'stype' : 'Stored_Delassus_Loops         '
               }

# write parameter
freq_write = 2

# display parameters
freq_display = 2
#PT3Dx_SetReferenceRadius(5.e-3)

output_file = 'lmgc90_output.h5'

# just in case...
chipy.utilities_setStopMode(False)

for i in range(2):

  #initialize
  init_lmgc90(dt, theta, dim, mhyp, deformable, output_file)
  
  for k in range(1, nb_steps + 1, 1):
    compute_lmgc90_one_step(freq_display,freq_write, solver_param)
  
  check_results()

  finalize_lmgc90()

