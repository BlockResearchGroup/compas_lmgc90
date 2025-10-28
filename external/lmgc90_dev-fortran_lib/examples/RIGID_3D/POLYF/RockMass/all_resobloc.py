from __future__ import print_function

import os,sys
import math, numpy as np

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

import gmsh

from pylmgc90 import pre

# lmgc90 initialization

dim=3

# definition des conteneurs:
#   * de corps
bodies = pre.avatars()
#   * de materiaux
mats = pre.materials()
##   * pour les tables de visibilite
svs = pre.see_tables()
##   * pour les lois de contact
tacts = pre.tact_behavs()

# on se place en 3D
dim = 3

# materiau rigide

# creation d'un materiau
tdur = pre.material(name='TDURx', materialType='RIGID', density=2500.)
mats.addMaterial(tdur)

# creation d'un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)

# gmsh initialization and processing
gmsh.initialize('', False)

if gmsh.model.mesh.getElementProperties(2)[0] != 'Triangle 3':
  print( "Must update script according to gmsh api")
  sys.exit()

lc = 20.
#CTX().instance().lc = 1e22

###
name='essai_init'
ifile = open(name+'.dat','r')

readv=0
readb=0
nodes=[]
My_meshes=[]
for line in ifile:

  # on degage les lignes vides
  if (line == '\n'): continue 

  if (line.count("VERTEX") != 0):
    print('debut lecture vertex')

  if (line.count("(") != 0):
    debut=line.index("(")
    #print 'd',debut
    fin=line.index(")")
    #print 'f',fin
    chaine=line[debut+1:fin]
    pair=chaine.split(",")
    x = float(pair[0]); y = float(pair[1]); z = float(pair[2])
    nodes.append([x,y,z]) 
    continue

  if (line.count("BLOCK") != 0):
    if len(My_meshes) != 0: 
      # on construit un rigide a partir de la liste de surface
      body = pre.surfacicMeshesToRigid3D(surfacic_meshes=My_meshes, model=mod, material=tdur, color='BLEUx')
      # on ajoute le donut au conteneur de corps
      bodies.addAvatar(body)

    print('new block')

    # on va creer un objet lmgc90
    My_meshes=[]
    nb_f=0

  if (line.count("FACE") != 0):
    print('+new face')
    nb_f+=1
    debut=line.index("E")
    chaine=line[debut+1:]
    pair=chaine.split()

    # on tag les noeuds
    nodes_map=[-1]*len(nodes)
    for x in pair:
       nodes_map[int(x)]=1

    contour =  [int(x) for x in pair]

    g = gmsh.model()
    g.add('face_'+str(nb_f))

    v=[]
    l=[]

    i=0
    for k,x in enumerate(nodes_map):
      if x < 0: continue
      nodes_map[k]=i
      v.append( g.geo.addPoint(nodes[k][0],nodes[k][1],nodes[k][2], lc))
      i+=1

    lfin=0
    for i in range(0,len(contour)-1):
       dd=nodes_map[contour[i]]
       ff=nodes_map[contour[i+1]]
       l.append( g.geo.addLine(v[dd],v[ff]) )
       lfin+=1
    dd=nodes_map[contour[len(contour)-1]]
    ff=nodes_map[contour[0]]
    l.append( g.geo.addLine(v[dd],v[ff]) )
    lfin+=1

    #g.addPlanarFace([l[0:lfin]])
    gcl = g.geo.addCurveLoop(l[0:lfin])
    gps = g.geo.addPlaneSurface([gcl])
    gpg = g.addPhysicalGroup(2, [gps] )
    g.setPhysicalName(2, gpg, "surface"+str(nb_f) )

    g.geo.synchronize()
    g.mesh.generate(2)
    # do not know how to write yet...
    #g.write(name+'.msh')    

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


    for bulk in My_mesh.bulks: 
      bulk.connectivity.reverse() 

    My_mesh.removeFreeNodes()
    My_mesh.rankRenumbering()
    My_meshes.append(My_mesh)  

ifile.close()

if len(My_meshes) != 0: 
  # on construit un rigide a partir de la liste de surface
  body = pre.surfacicMeshesToRigid3D(surfacic_meshes=My_meshes, model=mod, material=tdur, color='BLEUx')
  # on ajoute le donut au conteneur de corps
  bodies.addAvatar(body)

gmsh.finalize()

body=pre.rigidPlan(axe1=100., axe2=700., axe3=1., center=[50.,324,625], 
                   model=mod, material=tdur, color='BLEUx')
body.rotate(description='axis', center=list(body.nodes.values())[0].coor, axis=[1.,0.,0.], alpha=math.pi/2.)
body.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies.addAvatar(body)


# definitions des interactions
#  * loi d'interaction :
lprpr=pre.tact_behav('iqsc0', 'IQS_CLB', fric=0.3)
tacts+=lprpr

lprpl=pre.tact_behav('iqsc0', 'IQS_CLB_g0', fric=0.3)
tacts+=lprpl

#  * table de visibilite :
svprpr = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='POLYR', colorCandidat   ='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLEUx',
                       behav=lprpr, alert=1.e-2)
svs+=svprpr

svprpl = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='POLYR', colorCandidat   ='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='BLEUx',
                       behav=lprpl, alert=1.)
svs+=svprpl

# ecriture des fichiers de donnees
pre.writeBodies(bodies, chemin='./DATBOX/')
pre.writeBulkBehav(mats, chemin='./DATBOX/', dim=dim, gravy=[0., -9.81, 0.])
pre.writeDrvDof(bodies, chemin='./DATBOX/')
pre.writeDofIni(bodies, chemin='./DATBOX/')
pre.writeTactBehav(tacts, svs, chemin='./DATBOX/')
pre.writeVlocRlocIni(chemin='./DATBOX/')

post = pre.postpro_commands()
nlgs = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(nlgs)
pre.writePostpro(commands=post, parts=bodies, path='./DATBOX/')

try:
  pre.visuAvatars(bodies)
except:
  pass

