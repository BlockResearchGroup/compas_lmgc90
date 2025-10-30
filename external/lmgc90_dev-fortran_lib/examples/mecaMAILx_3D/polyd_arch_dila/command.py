
from pylmgc90 import chipy
import numpy as np

chipy.checkDirectories()

####
# info gestion du temps
dt = 1.e-3
theta = 0.5
nb_steps = 100

time_start=1e-2

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 10
freq_write = 10

# info contact
freq_detect = 1

#       123456789012345678901234567890
stype  = 'Exchange_Local_Global         '
quad   = 'QM/16'
tol    = 0.1666e-3
relax  = 1.0
gs_it1 = 5000
gs_it2 = 50

chipy.nlgs_3D_DiagonalResolution()
# en cas de restart ou par defaut le solveur redemarre avec un statut non initialise
#et ou on voudrait eviter que les valeurs lus dans les fichiers ini
# soient reinitialisees on declare que le solveur est initialise
# nlgs_3D_IsInitialized()

chipy.POLYR_SkipAutomaticReorientation()
chipy.POLYR_TopologyAngle(5.)

###print 'CONTACT DETECTION INITIALIZATION'
chipy.PRPRx_ShrinkPolyrFaces(0.01)
#chipy.PRPRx_UseCpF2fExplicitDetection(1e-2)
chipy.PRPRx_UseNcF2fExplicitDetection(2.5e-1,1e-2)

# pour virer les points de contact avec des surfaces trop petites
#PRPRx_SetF2fMinimalSurfaceSize(700.)

chipy.PRPRx_LowSizeArrayPolyr(10)

###
chipy.SetDimension(3,0)

### definition des parametres du calcul ###
###print 'INIT TIME STEPPING'

chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### lecture du modele ###
###print 'READ BODIES'
chipy.ReadBodies()

###print 'READ BEHAVIOURS'
chipy.ReadBehaviours()

###print 'READ MODELS'
chipy.ReadModels()

chipy.LoadBehaviours()
chipy.LoadModels()

#
# construction du probleme reduit
chipy.mecaMAILx_SetRigidAllBodies()
chipy.mecaMAILx_ComputeMass()
chipy.mecaMAILx_BuildRigidBodies()
# blocage base 
chipy.mecaMAILx_SetRVDrivenDofs(1,[1,2,3,4,5,6])
chipy.mecaMAILx_SetRVDrivenDofs(2,[1,2,3,4,5,6])
#
#mecaMAILx_SkipDeformableComputationAllBodies()
#
###print 'LOAD TACTORS'
chipy.LoadTactors()

#
# on fait une detection de contact ici
# pour creer la table visavis qui va bien
#
##print 'CONTACT DETECTION'
# on met step a 1 sinon le select ne va pas marcher
chipy.TimeEvolution_SetInitialStep(1)
#
# on detecte
#chipy.SelectProxTactors(1)
#
###print 'READ INI DOF'
chipy.ReadIniDof()

###print 'READ INI GPV'
chipy.ReadIniGPV()

###print 'READ INI Vloc Rloc'
chipy.ReadIniVlocRloc()
#
###print 'READ DRIVEN DOF'
chipy.ReadDrivenDof()
nbm = chipy.mecaMAILx_GetNbMecaMAILx()
# Pour la gestion de la thermique aux noeuds du maillage on
# construit une liste de tableau numpy qui contient le
# parametre d'interpolation
objets=[]
for im in range(1,nbm+1):

   nb_nodes= chipy.mecaMAILx_GetNbNodes(im)
   xc = chipy.mecaMAILx_GetRigidCooref(im)
   # on cherche l'axe qui est le plus aligne avec le centre de l'arche
   frame = chipy.mecaMAILx_GetRigidFrame('RFbeg',im)
   idir  = np.argmax( np.abs( np.matmul(frame,xc) ) )

   v = frame[idir,:]
   if np.dot(xc,v) < 0.:
     v *= -1.

   interp = np.matmul(chipy.mecaMAILx_GetBodyVector('Coor0',im) - xc, v)
   xmin = interp.min()
   length =  - xmin
   interp[:] = (interp[:] - xmin)/(interp.max()-xmin)

   objets.append(interp)
#
T0=20.
list_T = []
for im in range(1,nbm+1,1):
  T=np.ones( [chipy.mecaMAILx_GetNbNodes(im)] ) * T0
  chipy.mecaMAILx_SetScalarFieldByNode(im,1,T)
  list_T.append(T)

Tout=T0
Tin=T0
#
# la partie mecaMAILx est deja faite
#
###print 'COMPUTE STIFFNESS'
chipy.mecaMAILx_ComputeBulk()
chipy.mecaMAILx_AssembKT()
#
### ecriture paranoiaque du modele ###
###print 'WRITE BODIES'
chipy.WriteBodies()

###print 'WRITE MODELS'
chipy.models_WriteModels()

###print 'WRITE BEHAVIOURS'
chipy.WriteBehaviours()

###print 'WRITE DRIVEN DOF'
chipy.WriteDrivenDof()

###print 'WRITE LAST DOF'
chipy.WriteLast()

### post3D ##
chipy.OpenDisplayFiles(mecagp_field='stress', write_f2f=3)
chipy.OpenPostproFiles()

# precondensation 
#CSxxx_PushPreconNodes()
#ASpxx_PushPreconNodes()


for k in range(1,nb_steps+1,1):
   #
   ###print 'increment : ',k
   #
   ###print 'INCREMENT STEP'
   chipy.TimeEvolution_IncrementStep()

   time=chipy.TimeEvolution_GetTime()


   #if time > time_start :
     #T0=T0+1.
     #dt2=dt*10.
     #print dt2
     #overall_SetTimeStep(dt2)


   #for im in xrange(1,nbm+1,1):
   #  T=[T0]*mecaMAILx_GetNbNodes(im)
   #  mecaMAILx_SetField(im,1,T)

   if time > time_start :
     Tin = Tin + 0.1
     DT = Tout - Tin
     for im in range(1,nbm+1,1):
       nb_nodes = chipy.mecaMAILx_GetNbNodes(im)
       #print 'objet:',im,'nb nodes:',length(objets[im-1]),nb_nodes
       T = list_T[im-1]
       for inode in range(0,nb_nodes,1):
          T[inode] = Tin + (objets[im-1][inode]*DT)
       chipy.mecaMAILx_SetScalarFieldByNode(im,1,T)

   #  mecaMAILx_SetRVDrivenDofValue(859,2,-10.)
   #  mecaMAILx_SetRVDrivenDofValue(860,2,-10.)
   #
   chipy.mecaMAILx_IncrementStep()

   ###print 'DISPLAY TIMES'
   chipy.TimeEvolution_DisplayStep()

   ###print 'COMPUTE Fext'
   chipy.ComputeFext()

   ###print 'COMPUTE Fint'
   chipy.ComputeBulk()
   chipy.AssembleMechanicalRHS()

   ###print 'COMPUTE Free Vlocy'
   chipy.ComputeFreeVelocity()
   #
   ###print 'SELECT PROX TACTORS'
   chipy.SelectProxTactors(freq_detect)
   #
   #PRPRx_DisplayProxTactors()

   ###print 'RESOLUTION'

   chipy.RecupRloc()

   #print type,quad,tol, relax, gs_it1, gs_it2 
   chipy.ExSolver(stype,quad, tol, relax, gs_it1, gs_it2)
   chipy.UpdateTactBehav()

   chipy.StockRloc()
   #
   #PRPRx_DisplayOutVlocRloc()
   #
   ###print 'COMPUTE DOF, FIELDS, etc.'
   chipy.ComputeDof()
   #
   ###print 'UPDATE DOF, FIELDS'
   chipy.UpdateStep()

   ###print 'WRITE out DOF'
   chipy.WriteOut(freq_write)
   chipy.WriteDisplayFiles(freq_display, T=('mecafe', 'node', list_T))
   chipy.WritePostproFiles()
   #if (k % freq_display == 0):
   #   PRPRx_VisavisVTKDrawAll()
   #   mecaMAILx_RigidVTKDrawAll()
   #   mecaMAILx_GpvVTKDrawAll(0.1,0)


### postpro ###
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

chipy.Finalize()
