
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

dim = 3

mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)

mat = pre.material(name='TDURx', materialType='RIGID', density=1000.)
mats.addMaterial(mat)

planx = pre.rigidPlan(axe1=1., axe2=1., axe3=0.2, center=[0., 0., -0.2], model=mod, material=mat, color='PLANx')
planx.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies.addAvatar(planx)
   
spher1 = pre.rigidSphere(r=0.11, center=[0.,0.,0.11], model=mod, material=mat, color='SPHER')
spher2 = pre.rigidSphere(r=0.1, center=[-0.01,0.,0.315], model=mod, material=mat, color='SPHER')
cylnd1 = pre.rigidCylinder(r=0.11, h=0.25, center=[-0.22,0.,0.11], model=mod, material=mat, color='CYLND')
cylnd2 = pre.rigidCylinder(r=0.1, h=0.25, center=[-0.199,0.,0.315], model=mod, material=mat, color='CYLND')
cylnd1.rotate(description='axis',alpha=math.pi/2.,axis=[1.,0.,0.],center=[-0.22 ,0.,0.11])
cylnd2.rotate(description='axis',alpha=math.pi/2.,axis=[1.,0.,0.],center=[-0.199,0.,0.315])

vertices = numpy.array([0.25,-0.25,0. , 0.25,0.25,0. , 0.75,0.25,0. , 0.75,-0.25,0., \
                        0.25,-0.25,0.1, 0.25,0.25,0.1, 0.75,0.25,0.1, 0.75,-0.25,0.1])
vertices.shape=[8,3]
polyr1 = pre.rigidPolyhedron(model=mod, material=mat, generation_type='vertices', vertices=vertices, color='POLYR')


bodies.addAvatar(spher1)
bodies.addAvatar(spher2)
bodies.addAvatar(cylnd1)
bodies.addAvatar(cylnd2)
bodies.addAvatar(polyr1)

mat = pre.material(name='TTDUR', materialType='RIGID', density=10000.)
mats.addMaterial(mat)
dnlyc = pre.rigidCylinder(r=0.1, h=0.125, center=[-0.65,0.,0.4], model=mod, material=mat, color='DNLYC', is_Hollow=True)
dnlyc.addContactors(shape='PT3Dx',color='PT3Dx',shift=[0.,-0.05,0.055])
dnlyc.rotate(description='axis',alpha=math.pi/3.,axis=[1.,0.,0.],center=[-0.65,0.,0.4])
dnlyc.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
spher3 = pre.rigidSphere(r=0.05, center=[-0.65,-0.05,0.4], model=mod, material=mat, color='SPHER')
spher3.addContactors(shape='PT3Dx',color='PT3Dx',shift=[0.,0.,0.05])
spher3.rotate(description='axis',alpha=math.pi/3.,axis=[1.,0.,0.],center=[-0.65,0.,0.4])

bodies.addAvatar(dnlyc)
bodies.addAvatar(spher3)

polyr3 = pre.rigidPolyhedron(model=mod, material=mat, generation_type='regular', radius=0.05, nb_vertices=4, color='PYRAM')

mod = pre.model(name='M3DH8', physics='MECAx', element='H8xxx', dimension=3, external_model='MatL_',
                kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
mods.addModel(mod)

mat = pre.material(name='stone', materialType='ELAS', density=2750., elas='standard', anisotropy='isotropic',
                   young=7.e10, nu=0.2)  
mats.addMaterial(mat)

mesh = pre.buildMeshH8(0.3,-0.2,0.1,0.4,0.4,0.2,4,4,2)
mesh1 = pre.buildMeshedAvatar(mesh=mesh,model=mod,material=mat)
mesh1.addContactors(group='up',shape='ASpxx',color='ASpxx')
mesh1.addContactors(group='down',shape='CSpxx',color='CSpxx', quadrature=1)

from copy import deepcopy
mesh2 = deepcopy(mesh1)

#mesh1.translate(dz=0.001)
mesh2.translate(dz=0.2)

polyr2 = deepcopy(polyr1)
polyr2.translate(dy=0.48,dz=0.1)

bodies.addAvatar(mesh1)
bodies.addAvatar(mesh2)
bodies.addAvatar(polyr2)

polyr3.translate(dx=0.5,dy=0.,dz=0.52)
bodies.addAvatar(polyr3)

# definition d'une loi de contact frottant, avec pre-gap
iqsc0=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)
gapc0=pre.tact_behav(name='gapc0', law='GAP_SGR_CLB', fric=0.5)
elas0=pre.tact_behav(name='elas0', law='ELASTIC_WIRE', stiffness=5.e3, prestrain=0.)

tacts.addBehav(iqsc0)
tacts.addBehav(gapc0)
tacts.addBehav(elas0)

sv1 = pre.see_table(CorpsCandidat='RBDY3', candidat='CYLND', colorCandidat='CYLND', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='CYLND', colorAntagoniste='CYLND', alert=0.1)

sv2 = pre.see_table(CorpsCandidat='RBDY3', candidat='CYLND', colorCandidat='CYLND', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='PLANx', alert=0.1)

sv3 = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='POLYR', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='PLANx', alert=0.1)

sv4 = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='POLYR', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='POLYR', alert=0.1)

sv5 = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='SPHER', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='CYLND', colorAntagoniste='CYLND', alert=0.1)

sv6 = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='SPHER', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='PLANx', alert=0.1)

sv7 = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='SPHER', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='SPHER', colorAntagoniste='SPHER', alert=0.1)

sv8 = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='CSpxx', behav=gapc0,
                    CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='ASpxx', alert=0.05, halo=1.)

sv9 = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='CSpxx', behav=gapc0,
                    CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='POLYR', alert=0.05)

sva = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='PYRAM', behav=gapc0,
                   CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='ASpxx', alert=0.01, halo=0.1)

svb = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='SPHER', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='DNLYC', colorAntagoniste='DNLYC', alert=0.1)

svc = pre.see_table(CorpsCandidat='RBDY3', candidat='PT3Dx', colorCandidat='PT3Dx', behav=elas0,
                    CorpsAntagoniste='RBDY3', antagoniste='PT3Dx', colorAntagoniste='PT3Dx', alert=0.1)


svs.addSeeTable(sv1)
svs.addSeeTable(sv2)
svs.addSeeTable(sv3)
svs.addSeeTable(sv4)
svs.addSeeTable(sv5)
svs.addSeeTable(sv6)
svs.addSeeTable(sv7)
svs.addSeeTable(sv8)
svs.addSeeTable(sv9)
svs.addSeeTable(sva)
svs.addSeeTable(svb)
svs.addSeeTable(svc)

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

  results = { chipy.CDCDx_ID : ( 2, chipy.CYLND_GetNbCYLND ),
              chipy.CDPLx_ID : ( 2, chipy.CYLND_GetNbCYLND ),
              chipy.CSASp_ID : (64, chipy.CSxxx_GetNbCSxxx ),
              chipy.PRASp_ID : ( 3, chipy.POLYR_GetNbPOLYR ),
              chipy.CSPRx_ID : (64, chipy.CSxxx_GetNbCSxxx ),
              chipy.PRPLx_ID : ( 6, chipy.POLYR_GetNbPOLYR ),
              chipy.PRPRx_ID : ( 4, chipy.POLYR_GetNbPOLYR ),
              chipy.PTPT3_ID : ( 1, chipy.PT3Dx_GetNbPT3Dx ),
              chipy.SPCDx_ID : ( 4, chipy.SPHER_GetNbSPHER ),
              chipy.SPDCx_ID : ( 3, chipy.SPHER_GetNbSPHER ),
              chipy.SPPLx_ID : ( 1, chipy.SPHER_GetNbSPHER ),
              chipy.SPSPx_ID : ( 1, chipy.SPHER_GetNbSPHER ),
            }

  for k, v in results.items():
    # check detection
    nbInter = v[0]
    getNbCd = v[1]

    assert( chipy.inter_handler_3D_getNb(k) == nbInter )

    # check that verlet accessor gives the same result
    # in number... should check value but get ic/antac on inter is missing
    count = 0
    for iv in range( 1, getNbCd()+1 ):
      nbv = chipy.inter_handler_3D_getVerletAdjsz(k, iv)
      for adj in range(1, nbv+1 ):
        iantac = chipy.inter_handler_3D_getVerletIantac(k, iv, adj)
        count += 1

    assert( count == nbInter )

  interNames = chipy.parameters_getInteractionNames()
  # check that real data of this array are accessible:
  for k in results.keys():
    for i_inter in range( chipy.inter_handler_3D_tgetNb(k) ):
        rdata = chipy.inter_handler_3D_tgetRData(k, i_inter+1)
        print( f"{interNames[k-1]}: inter {i_inter+1} -> {rdata}" )



# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0

# time evolution parameters
dt = 4e-3
nb_steps = 6

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
                 'gsit1' : 10     ,
                 'gsit2' : 10     ,
                 'stype' : 'Stored_Delassus_Loops         '
               }

# write parameter
freq_write = 2

# display parameters
freq_display = 2
chipy.PT3Dx_SetDisplayRadius(5.e-3)

output_file = 'lmgc90_output.h5'

for i in range(2):

  #initialize
  init_lmgc90(dt, theta, dim, mhyp, deformable, output_file)
  
  chipy.PRPRx_UseCpCundallDetection(40)

  for k in range(nb_steps):
    compute_lmgc90_one_step(freq_display,freq_write, solver_param)
  
  check_results()

  finalize_lmgc90()

