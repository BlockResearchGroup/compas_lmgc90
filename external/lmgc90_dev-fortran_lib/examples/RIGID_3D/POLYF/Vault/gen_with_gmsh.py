from __future__ import print_function

import os,sys
import numpy as np

#from gmshpy import *
import gmsh

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# lmgc90 initialization

dim=3

# empty containers:
#   * bodies
bodies = pre.avatars()
#   * materials
mats = pre.materials()
#   * visibility tables
svs = pre.see_tables()
#   * contact laws
tacts = pre.tact_behavs()

# two materials
tdur = pre.material(name='TDURx', materialType='RIGID', density=2500.)
mats.addMaterial(tdur)

tcon = pre.material(name='TCONx', materialType='RIGID', density=2500.)
mats.addMaterial(tcon)

# rigid model
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)


# gmsh initialization 
gmsh.initialize('', False)

#type_name = {TYPE_PNT:"point", TYPE_LIN:"line", TYPE_TRI:"triangle", TYPE_QUA:"quad", TYPE_TET:"tetrahedron",
#             TYPE_PYR:"pyramid", TYPE_HEX:"hexahedron", TYPE_POLYG:"polygon", TYPE_POLYH:"polyhedron"}
if gmsh.model.mesh.getElementProperties(2)[0] != 'Triangle 3':
  print( "Must update script according to gmsh api")
  sys.exit()

rescale=1.

lc = 0.1
#CTX().instance().lc = 1e22

gmsh.option.setNumber("General.Terminal", 1)
gmsh.option.setNumber("Mesh.Algorithm", 6)
#GmshSetOption('Mesh', 'Algorithm', 6)
#GmshSetOption('Mesh','CharacteristicLengthMin',0.01)
#GmshSetOption('Mesh','CharacteristicLengthMax',0.01)

# name of the file containing geometry

name='abeille2'
ifile = open(name+'.dat','r')

nb_b=0

for line in ifile:
  nbb_d=line.count('{')
  nbb_f=line.count('}')

  if (nbb_d > 2):
    print('open new block')
    nb_b+=1

    My_meshes=[]
    nb_f=0

  if (nbb_d > 1 ):
    print('open new face')
    idn_b=0; idn_e=0
    idl_b=0; idl_e=0

    nb_f+=1

    v=[]
    l=[]

    g = gmsh.model()
    g.add('Block'+str(nb_b))


  # lecture du noeud

  debut=2

  fin=line.index("}")
  
  chaine=line[debut+1:fin]

  pair=chaine.split(",")
  x = float(pair[0]); y = float(pair[1]); z = float(pair[2])
  v.append( g.geo.addPoint(x*rescale,y*rescale,z*rescale, lc) )
  #print('point ',v[-1],' : ',chaine)
  idn_e+=1
  
  if (nbb_f>1):
    print('close face')

    for i in range(idn_b,idn_e-1):
       #print('segment ',i,' from ',v[i],' to ',v[i+1])
       l.append( g.geo.addLine(v[i],v[i+1]) )
       idl_e+=1
       
    #print('segment ',i+1,' from ',v[idn_e-1],' to ',v[idn_b])
    l.append( g.geo.addLine(v[idn_e-1],v[idn_b]) )
    idl_e+=1

    #print('face    ',l[idl_b:idl_e])
    gcl = g.geo.addCurveLoop(l[idl_b:idl_e])
    gps = g.geo.addPlaneSurface([gcl])
    gpg = g.addPhysicalGroup(2, [gps] )
    g.setPhysicalName(2, gpg, "surface"+str(nb_f) )

    # on maille la face
    g.geo.synchronize()
    g.mesh.generate(2)

    My_mesh=pre.mesh(dimension=dim)

    #print '=============== mesh vertices ================='
    gvert_num, gvert_coor, param_coor = g.mesh.getNodes()
    for i, v in enumerate(gvert_num):
        coor = gvert_coor[3*i:3*i+3]
        #print( 'adding node ', v, ' at coor ', coor )
        My_mesh.addNode( pre.node(coor=coor,number=v) )

    #print '=============== mesh elements ================='
    gedges_type, gedges_num, gedges_connec = g.mesh.getElements(dim=2)
    if 2 not in gedges_type:
      print("Error : mesh did not create any triangle")
      sys.exit()
    else:
      tri_index = np.argwhere(gedges_type==2)[0][0]

    for i, e in enumerate(gedges_num[tri_index]):
      connec = gedges_connec[0][3*i:3*i+3].tolist() # works because only triangle are treated...
      #print( 'adding connectivity : ', connec)
      My_mesh.addBulk( pre.element(elem_dim=2, connectivity=connec, physicalEntity=0, geometricalEntity=nb_f) )


    # rustine pour remettre certaines faces dans le bon sens 
    if (nb_f == 1 or nb_f == 3 or nb_f == 4):
      for bulk in My_mesh.bulks: 
        bulk.connectivity.reverse() 

    My_mesh.removeFreeNodes()
    My_mesh.rankRenumbering()
    My_meshes.append(My_mesh)  

  if (nbb_f>2):
    print('close block')
    
    # let's build a POLYF object 
    body = pre.surfacicMeshesToRigid3D(surfacic_meshes=My_meshes, model=mod, material=tdur, color='BLEUx')

    # lets apply drvdof to lowest objects  
    if (list(body.nodes.values())[0].coor[2] < 0.8):
      body.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
      body.groups['all'].contactors[0].color='REDxx'
      body.groups['all'].bulks[0].defineMaterial(tcon)

    bodies.addAvatar(body)

ifile.close()

gmsh.finalize()
# la fin de lmgc90

# tact behav law

lprpr=pre.tact_behav('iqsc0', 'IQS_CLB', fric=0.3)
tacts+=lprpr

# visibility tables
svprpr = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='POLYR', colorCandidat   ='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLEUx',
                       behav=lprpr, alert=1.e-2)
svs+=svprpr

svprpr2 = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='POLYR', colorCandidat   ='BLEUx',
                        CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx',
                        behav=lprpr, alert=1.25e-2)
svs+=svprpr2

# lets write
pre.writeBodies(bodies, chemin='./DATBOX/')
pre.writeBulkBehav(mats, chemin='./DATBOX/', dim=dim)
pre.writeDrvDof(bodies, chemin='./DATBOX/')
pre.writeDofIni(bodies, chemin='./DATBOX/')
pre.writeTactBehav(tacts, svs, chemin='./DATBOX/')
pre.writeVlocRlocIni(chemin='./DATBOX/')

# post pro commands
post = pre.postpro_commands()
nlgs = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(nlgs)
pre.writePostpro(commands=post, parts=bodies, path='./DATBOX/')

# display
try:
  pre.visuAvatars(bodies)
except:
  pass

