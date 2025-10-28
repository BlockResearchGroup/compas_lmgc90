# module qui fournit des macros pour construire des particules

import numpy
import math

from ..avatar.avatar import *
from ..avatar.bulk.rigid2d import *
from ..avatar.bulk.rigid3d import *
from ..avatar.bulk.element import *

from .mesh import *

from ..utilities.error    import *

def rigidDisk(r, center, model, material, color='BLUEx', number=None, is_Hollow=False):
   '''
   usage: 

   body=rigidDisk(r, center, model, material, color='BLUEx', number=None, is_Hollow=False)

   this function builds a rigid disk and returns the generated body

   parameters :  

   - r : radius of the particle
   - center : position of the center of inertia in the global frame,
     as a two coordinates vector
   - model : rigid model for the particle
   - material : the particle is made of this material

   optional parameters :

   - color='BLUEx' : color of the contactor
   - number=None: index of the avatar (still present to ensure compatibility)
   - is_Hollow = False: is the contactor to be a hollow one (xKSID instead of DISKx).
   '''

   body = avatar(dimension=2, number=number)
   body.addBulk( rigid2d() )
   body.addNode( node(coor=numpy.array(center), number=1) )
   body.defineGroups()
   body.defineModel(model=model)
   body.defineMaterial(material=material)
   if is_Hollow:
       body.addContactors(shape='xKSID', color=color, byrd=r)
   else:
       body.addContactors(shape='DISKx', color=color, byrd=r)
   body.computeRigidProperties()

   return body

def rigidSphere(r, center, model, material, color='BLUEx', number=None):
   '''
   usage: 

   body=rigidSphere(r, center, model, material, color='BLUEx', number=None)

   this function builds a rigid sphere and returns the generated body

   parameters :  

   - r : radius of the particle
   - center : position of the center of inertia in the global frame,
     as a three coordinates vector
   - model : rigid model for the particle
   - material : the particle is made of this material

   optional parameters :

   - color='BLUEx' : color of the contactor
   - number=None: index of the avatar (still present to ensure compatibility)'''

   body = avatar(dimension=3, number=number)
   body.addBulk( rigid3d() )
   body.addNode( node(coor=numpy.array(center), number=1) )
   body.defineGroups()
   body.defineModel(model=model)
   body.defineMaterial(material=material)
   body.addContactors(shape='SPHER', color=color, byrd=r)
   body.computeRigidProperties()

   return body

def rigidCylinder(r, h, center, model, material, color='BLUEx', number=None, is_Hollow=False):
   '''
   usage: 

   body=rigidCylinder(r, h, center, model, material, color='BLUEx', number=None, is_Hollow=False)

   this function builds a rigid cylinder and returns the generated body

   parameters :  

   - r : radius of the cylinder
   - h : half-heigth of the cylinder
   - center : position of the center of inertia in the global frame,
     as a three coordinates vector
   - model : rigid model for the cylinder
   - material : the cylinder is made of this material

   optional parameters :

   - color='BLUEx' : color of the contactor
   - number=None: index of the avatar (still present to ensure compatibility)
   - is_Hollow = False: is the contactor to be a hollow one (DNLYC instead of CYLND)
   '''

   body = avatar(dimension=3, number=number)
   body.addBulk( rigid3d() )
   body.addNode( node(coor=numpy.array(center), number=1) )
   body.defineGroups()
   body.defineModel(model=model)
   body.defineMaterial(material=material)
   if is_Hollow:
     body.addContactors(shape='DNLYC', color=color, byrd=r,High=h)
   else:
     body.addContactors(shape='CYLND', color=color, byrd=r,High=h)
   body.computeRigidProperties()
   
   return body

def rigidJonc(axe1, axe2, center, model, material, color='BLUEx', number=None):
   '''
   usage: 

   body=rigidJonc(axe1, axe2, center, model, material, color='BLUEx', number=None)

   This function builds an horizontal rigid jonc and returns the generated body.

   The 'JONCx' contactor is used as a wall, thus its thickness must be way less
   than its length.

   parameters :  
    
   - axe1 : half length of the jonc
   - axe2 : half thickness of the jonc, must alway be less than axe1
   - center : position of the center of inertia in the global frame,
     as a two coordinates vector
   - model : rigid model for the particle
   - material : the particle is made of this material

   optional parameters :

   - color='BLUEx' : color of the contactor
   - number=None: index of the avatar (still present to ensure compatibility)
   '''

   if axe2 >= axe1:
     msg  = "when using rigidJonc function 'axe2' parameter "
     msg += "must be strictly inferior to 'axe1' parameter"
     showError( msg )

   body = avatar(dimension=2, number=number)
   body.addBulk( rigid2d() ) 
   body.addNode(node(coor=numpy.array(center),number=1))
   body.defineGroups()
   body.defineModel(model=model)
   body.defineMaterial(material=material)
   body.addContactors(shape='JONCx', color=color, axe1=axe1, axe2=axe2)
   body.computeRigidProperties()

   return body

def rigidPlan(axe1, axe2, axe3, center, model, material, color='BLUEx', number=None):
   '''
   usage: 

   body=rigidPlan(axe1, axe2, axe3, center, model, material, color='BLUEx', number=None)

   This function builds an horizontal rigid plan and returns the generated body.

   The 'PLANx' contactor is used as a wall, thus its thickness must be way less
   than its length or width. Furthermore the contact will activated only along the
   Z axis of the inertia frame.

   parameters :  
    
   - axe1 : half length of the plan
   - axe2 : half width of the plan
   - axe3 : half thickness of the plan
   - center : position of the center of inertia in the global frame,
     as a three coordinates vector
   - model : rigid model for the particle
   - material : the particle is made of this material

   optional parameters :

   - color='BLUEx' : color of the contactor
   - number=None: index of the avatar (still present to ensure compatibility)'''

   if axe3 >= axe1 or axe3 >= axe2 :
     msg  = "when using rigidPlan function 'axe3' parameter "
     msg += "must be strictly inferior to 'axe1' and 'axe2' parameters"
     showError( msg )

   body = avatar(dimension=3, number=number)
   body.addBulk( rigid3d() )
   body.addNode(node(coor=numpy.array(center),number=1))
   body.defineGroups()
   body.defineModel(model=model)
   body.defineMaterial(material=material)
   body.addContactors(shape='PLANx', color=color, axe1=axe1, axe2=axe2, axe3=axe3)
   body.computeRigidProperties()

   return body


def rigidCluster(r, center, nb_disk, model, material, color='BLUEx', number=None):
   '''
   usage: 

   body=rigidCluster(r, center, nb_disk, model, material, color='BLUEx', number=None)

   this function builds a rigid cluster of disks in a disk and returns the generated body

   parameters :  

   - r : radius of the bounding disk
   - center : position of the center of inertia in the global frame
   - nb_disk : number of disks of the cluster
   - model : rigid model for the particle
   - material : the particle is made of this material

   optional parameters :

   - color='BLUEx' : color of the contactors
   - number=None: index of the avatar (still present to ensure compatibility)'''

   body = avatar(dimension=2, number=number)
   body.addBulk( rigid2d() )
   body.addNode( node(coor=numpy.array(center), number=1) )
   body.defineGroups()
   body.defineModel(model=model)
   body.defineMaterial(material=material)

   # definition de la liste des contacteurs du cluster :
   # on calcule le rayon d'un contacteur disque
   r_disk = r*math.sin(math.pi/nb_disk)/(1. + math.sin(math.pi/nb_disk)) 
   for i in range(0, nb_disk, 1):
      # on calcule la position de son centre par rapport au centre d'inertie
      x_disk = (r_disk - r)*math.cos(2.*math.pi*i/nb_disk) 
      y_disk = (r_disk - r)*math.sin(2.*math.pi*i/nb_disk) 
      body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   body.computeRigidProperties()

   return body

def rigidPolygon( model, material, center, theta=0., color='BLUEx', generation_type='regular', nb_vertices=0, vertices=None, radius=1., number=None):
   '''
   usage: 

   body=rigidPolygon(model, material, center, theta=0., color='BLUEx', generation_type, nb_vertices, vertices, radius, number=None)

   This function builds a rigid polygon using 2 methods (see below).

   Some parameters are common to the methods and mandatory:

   - model : rigid model of the particle
   - material : material of the particle
   - center : position of the center of inertia the particle in the global frame

   Some parameters are common but optional:

   - theta=0. : rotation angle of the inertia frame
   - number=None : index of the avatar
   - generation_type ='regular' : the method used to generate the polygon contactor
   - color='BLUEx' : color of the polygon contactor
   
   One way (generation_type ='regular') consists in generating a polygon with its nodes located on a circle:    

   parameters :  

   - radius=1. : radius of the circle
   - nb_vertices=0. : number of vertices of the contactor

   The other way (generation_type ='full') consists in generating a polygon giving a list of vertices:    

   parameters :  

   - vertices : array of coordinates (nb_vertices,2)

   '''

   # creation d'un nouveau polygone rigide 2D
   body = avatar(dimension=2, number=number)
   # on cree comportement volumique de type rigide
   body.addBulk( rigid2d() )
   # ajout de la position du centre d'inertie au disque
   body.addNode(node(coor=numpy.array(center),number=1) )
   # on definit les groupes pour le disque
   body.defineGroups()
   # on affecte son modele au disque
   body.defineModel(model=model)
   # on affecte son materiau au disque
   body.defineMaterial(material=material)

   # definition du contacteur pour le polygone :
   if generation_type == 'regular':
     # on cree une matrice de double a la bonne taille
     vertices = numpy.zeros([nb_vertices, 2], 'd')
     # on calcule les positions des sommets par rapport au centre d'inertie
     for i in range(0, nb_vertices, 1):
        vertices[i, 0] = radius*math.cos(2.*math.pi*i/float(nb_vertices)) 
        vertices[i, 1] = radius*math.sin(2.*math.pi*i/float(nb_vertices)) 

     # ajout de son contacteur au polygone
     body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertices, vertices=vertices)

   elif generation_type == 'full':
     # ajout de son contacteur au polygone
     body.addContactors(shape='POLYG', color=color, nb_vertices=vertices.shape[0], vertices=vertices)

   elif generation_type == 'bevel':
     # gestion des conges
     nb_vertices=vertices.shape[0]
     nvertices = numpy.zeros([2*nb_vertices, 2], 'd')     
     inv=0 
     for i in range(0, nb_vertices):
        j=(i+1) % nb_vertices
        #print 'cote ',i,' de ',i,' a ',j
        xb = vertices[i,:]        
        
        v = vertices[j,:] - xb
        #print inv
        nvertices[inv,:]=xb + 0.1*v
        print(nvertices[inv,0],nvertices[inv,1])
        inv+=1 
        #print inv
        nvertices[inv,:]=xb + 0.9*v 
        print(nvertices[inv,0],nvertices[inv,1])
        inv+=1 
        
     # ajout de son contacteur au polygone
     body.addContactors(shape='POLYG', color=color, nb_vertices=inv, vertices=nvertices)

   else:
     showError('In rigidPolygon: unknow type '+generation_type)

   # on calcule de la surface et de l'inertie du cluster
   body.computeRigidProperties()

   # si on donne un angle non nul
   if theta != 0.:
      # on tourne le disque autour de son centre d'inertie
      body.rotate(psi=theta, center=body.nodes[1].coor)

   # on renvoie le corps genere
   return body

def rigidPolyhedron(model, material, center=numpy.zeros(3), color='BLUEx', generation_type='regular', 
                    nb_vertices=0, vertices=None, faces=None, radius=1., tol=0., number=None, seed=None,
                    xr=1., yr=1., zr=1.):
   '''
   usage: 

   body = rigidPolyhedron(model, material, center=numpy.zeros(3,'d'), color='BLUEx', generation_type='regular', 
                          nb_vertices=0, vertices=None, faces=None, radius=1., tol=0., number=None, seed=None, 
                          xr=1., yr=1., zr=1.)

   This function builds a rigid convex polyhedron using 4 methods (see below).

   Some parameters are common to the methods and mandatory:

   - model : rigid model for the particle
   - material : the particle is made of this material


   Some parameters are common but optional:

   - center=numpy.zeros(3,'d') : position of the center of inertia the particle in the global frame
   - color='BLUEx' : color of the sphere contactor.
   - generation_type='regular' : the method used to generate the polyhedron contactor.
   - number=None: index of the avatar (still present to ensure compatibility).

   Generation methods:

   - First method (generation_type ='regular') consists in generating vertices of the polyhedron
     regularly disposed on a sphere (of radius 'radius'). For Platon's polyhedra (number of
     vertices is 4, 6, 8, 12 or 20) the number of vertices used is exactly the one given in input,
     otherwise the number of vertices found by the generator may differ from the one in input
     (especially for low number of vertices).

     parameters :  

     - nb_vertices=0. : number of vertices of the contactor
     - radius=1. : radius of the encompassing sphere.
     - xr=1., yr=1., zr=1. : sphere to ellispoid mapping

   - Second method (generation_type ='random') consists in generating vertices of the polyhedron
     randomly disposed on a sphere (of radius 'radius'). The number of vertices used is exactly
     the one given in input (contrary to the first method).

     - nb_vertices=0. : number of vertices of the contactor
     - radius=1. : radius of the encompassing sphere.
     - tol=0. : tolerance to use to remove a vertex if the one found is to close to others (if 0. no check)
     - seed=None : seed to use to control the randomness
     - xr=1., yr=1., zr=1. : sphere to ellispoid mapping

   - Third method (generation_type ='vertices') generates the polyhedron defined by the convex hull
     of the vertices given in input. The 'vertices' array may be modified on output if the vertices
     set given was not convex.

     - vertices=None : array of coordinates (nb_vertices,3), if the vertices set is not convex
       on input, some points will be deleted on output.

   - Fourth method (generation_type ='full'|'bevel') generates the polyhedron using the vertices and
     connectivity given in input. Using 'bevel' will cut corners.

     - vertices=None : array of coordinates (nb_vertices,3).
     - faces=None: faces as triangles connectivity of vertices (nb_faces,3). Numbering of vertices
       in 'faces' array is 1 to nb_vertices (and not 0 to nb_vertices-1 like in Python).
       
   '''

   body = avatar(dimension=3, number=number)
   body.addBulk( rigid3d() )

   if type(center) != numpy.ndarray:
     try:
       center = numpy.array(center,'d')
     except:
       showError('rigidPolyhedron:: center parameter must be convertible to a numpy array')

   for val_ in [xr,yr,zr] :    
     if val_ < 0. or val_ > 1.:
       showError('rigidPolyhedron:: xr,yr,zr must be between 0. and 1.') 
       
   #body.addNode( node(type='NO3xx',coor=numpy.array(center), number=1) )
   body.addNode( node(coor=numpy.zeros(3,'d'), number=1) )

   body.defineGroups()
   body.defineModel(model=model)
   body.defineMaterial(material=material)

   if generation_type == 'regular':
     if nb_vertices < 4:
       showError('rigidPolyhedron:: nb_vertices must be > 4')
     # nodes generation
     vertices = getRegularPolyhedronVertices(nb_vertices, radius)
     # ellispoid transform
     for i in range(vertices.shape[0]):
       vertices[i,:] = [xr*vertices[i,0],yr*vertices[i,1],zr*vertices[i,2]] 
     vertices += center
     # connectivity generation
     faces,vertices = buildPolyhedronConnectivity(vertices)
   elif generation_type == 'random':
     if nb_vertices < 4:
       showError('rigidpolyhedron:: nb_vertices must be > 4')
     # nodes generation
     vertices = getRandomPolyhedronVertices(nb_vertices, radius, tol, seed)
     # ellispoid transform
     for i in range(nb_vertices):
       vertices[i,:] = [xr*vertices[i,0],yr*vertices[i,1],zr*vertices[i,2]] 
     vertices += center
     # connectivity generation
     faces,vertices = buildPolyhedronConnectivity(vertices)
   elif generation_type == 'vertices':
     if type(vertices) != numpy.ndarray:
       showError('In rigidpolyhedron: vertices must be a numpy array. Is of type '+type(vertices))
     if vertices.shape[1] != 3 or vertices.shape[0] < 4:
       showError('rigidpolyhedron:: vertices must be a of shape [n,3] with n>4, is '+vertices.shape)
     # connectivity generation, be carefull it mays remove nodes (mais c'est fait avec les pieds)
     faces,vertices = buildPolyhedronConnectivity(vertices)
   elif generation_type == 'full':
     if type(vertices) != numpy.ndarray:
       showError('rigidpolyhedron:: vertices must be a numpy array. Is of type '+type(vertices))
     if vertices.shape[1] != 3 or vertices.shape[0] < 1:
       showError('rigidpolyhedron:: vertices must be a of shape [n,3] with n>1, is '+vertices.shape)
     if type(faces) != numpy.ndarray:
       showError('rigidpolyhedron:: faces must be a numpy array. Is of type '+type(faces))
     if faces.shape[1] != 3 or faces.shape[0] < 1:
       showError('rigidpolyhedron:: faces must be a of shape [n,3] with n>1, is '+faces.shape)
   else:
     showError('rigidpolyhedron:: unknow type '+generation_type)

   body.addContactors(shape='POLYR', color=color, nb_vertices=vertices.shape[0], vertices=vertices, 
                      nb_faces=len(faces), connectivity=faces)

   body.computeRigidProperties()

   # on renvoie le corps genere
   return body

def deformableParticle2D(r, center, type_part, model, material, color='BLUEx', number=None):
   """
   usage:

   body=deformableParticle2D(r, center, type_part, model, material, color='BLUEx', number=None)

   this function builds a deformable particle and returns the generated body

   N.B. elements on the skin of the particle belong to the group 'skin'

   parameters :  

   - r : radius of the particle
   - center : position of the center of inertia in the global frame
   - type_part : type of particule, it must correspond to a known precalculated mesh ('Disk' or 'pent')
   - model : model
   - material : material

   optional parameters :

   - color='BLUEx' : color of the candidate and antagonist contactors
   - number=None: index of the avatar (still present to ensure compatibility)
   """

   # recuperation du  maillage precalcule en fonction de la particule a generer
   # N.B. on recree un nouvel objet maillage a chaque fois, pour etre sur que
   # que les avatars sont bien tous independants, et pas tous une reference vers un
   # maillage premier

   # DISK CASE
   if (type_part == 'Disk'):     
      mesh_particle=MeshedUnitDisk()
   # PENTAGON CASE
   elif(type_part == 'pent'): 
      mesh_particle=MeshedUnitPentagon()
   # DEFAULT CASE
   else: 
      # si on a reconnu le type de particule, on arrete tout
      showError('unknown type of particle!')

   # pour chaque noeud
   for nod in mesh_particle.nodes:
      # on modifie les coordonnees du noeud courant, pour que la particule
      # resultante aie le bon rayon (homotetie) et la bonne position (translation)
      nod.coor=numpy.array(center) + r*nod.coor

   # on cree le corps a partir du maillage
   body=buildMeshedAvatar(mesh=mesh_particle, model=model, material=material)

   # on definit les contacteurs pour le corps :
   #  * un contacteur candidat sur chaque noeud :
   body.addContactors(group='S2xxx', shape='CLxxx', color=color)
   #  * un contacteur antagoniste
   body.addContactors(group='S2xxx', shape='ALpxx', color=color)

   # on renvoie l'objet genere
   return body

####################################################################################
# fonction qui renvoie un maillage precalcule du disque unite
def MeshedUnitPentagon():
   """mesh_penta=MeshedUnitPentagon():

   this function builds and returns a precomputed mesh of the unit pentagone, centered on (0, 0)
   """

   # on definit l'objet maillage pour stocker le maillage du disque unitaire
   mesh_penta=mesh(dimension=2)
   
   A36  = 0.2*math.pi
   A72  = 2.*A36
   rad1 = 0.5
   rad2 = math.cos(A36)
 
   # on ajoute les noeuds au maillage
   mesh_penta.addNode( node( coor=numpy.array([ math.cos(3.*A72),math.sin(3.*A72) ]), number= 1) )
   mesh_penta.addNode( node( coor=numpy.array([ math.cos(2.*A72),   0.0000000e+00 ]), number= 2) )
   mesh_penta.addNode( node( coor=numpy.array([ math.cos(2.*A72),math.sin(2.*A72) ]), number= 3) )

   mesh_penta.addNode( node( coor=numpy.array([ rad1*math.cos(3.*A72),rad1*math.sin(3.*A72) ]), number= 4) )
   mesh_penta.addNode( node( coor=numpy.array([-rad1, 0.0000000e+00 ]), number= 5) )
   mesh_penta.addNode( node( coor=numpy.array([ rad1*math.cos(2.*A72),rad1*math.sin(2.*A72) ]), number= 6) )

   mesh_penta.addNode( node( coor=numpy.array([ rad2*math.cos(7.*A36),rad2*math.sin(7.*A36) ]), number= 7) )
   mesh_penta.addNode( node( coor=numpy.array([ rad1*math.cos(7.*A36),rad1*math.sin(7.*A36) ]), number= 8) )

   mesh_penta.addNode( node( coor=numpy.array([ rad1*math.cos(3.*A36),rad1*math.sin(3.*A36) ]), number= 9) )
   mesh_penta.addNode( node( coor=numpy.array([ rad2*math.cos(3.*A36),rad2*math.sin(3.*A36) ]), number=10) )

   mesh_penta.addNode( node( coor=numpy.array([ 0.0000000e+00 , 0.0000000e+00 ]), number=11) )

   mesh_penta.addNode( node( coor=numpy.array([ rad1*math.cos(4.*A72),rad1*math.sin(4.*A72) ]), number=12) )
   mesh_penta.addNode( node( coor=numpy.array([ rad1*math.cos(1.*A72),rad1*math.sin(1.*A72) ]), number=13) )

   mesh_penta.addNode( node( coor=numpy.array([ math.cos(4.*A72),math.sin(4.*A72) ]),           number=14) )
   mesh_penta.addNode( node( coor=numpy.array([ rad1*math.cos(9.*A36),rad1*math.sin(9.*A36) ]), number=15) )
   mesh_penta.addNode( node( coor=numpy.array([ rad1,   0.0000000e+00 ]),                       number=16) )
   mesh_penta.addNode( node( coor=numpy.array([ rad1*math.cos(1.*A36),rad1*math.sin(1.*A36) ]), number=17) )
   mesh_penta.addNode( node( coor=numpy.array([ math.cos(1.*A72),math.sin(1.*A72) ]),           number=18) )

   mesh_penta.addNode( node( coor=numpy.array([ rad2*math.cos(9.*A36),rad2*math.sin(9.*A36) ]), number=19) )
   mesh_penta.addNode( node( coor=numpy.array([ rad2*math.cos(1.*A36),rad2*math.sin(1.*A36) ]), number=20) )

   mesh_penta.addNode( node( coor=numpy.array([ 1.0000000e+00 , 0.0000000e+00 ]),               number=21) )


   # on ajoute les elements au maillage

   #   * les elements surfaciques
   mesh_penta.addBulk( element(elem_dim=2, connectivity=[ 1,  4,  5,  2]) )
   mesh_penta.addBulk( element(elem_dim=2, connectivity=[ 2,  5 , 6,  3]) )
   mesh_penta.addBulk( element(elem_dim=2, connectivity=[ 4, 11,  6,  5]) )

   mesh_penta.addBulk( element(elem_dim=2, connectivity=[ 6,  9, 10,  3]) )
   mesh_penta.addBulk( element(elem_dim=2, connectivity=[ 1,  7,  8,  4]) )
   mesh_penta.addBulk( element(elem_dim=2, connectivity=[ 7, 14, 12,  8]) )

   mesh_penta.addBulk( element(elem_dim=2, connectivity=[ 4,  8, 12, 11]) )
   mesh_penta.addBulk( element(elem_dim=2, connectivity=[ 6, 11, 13,  9]) )
   mesh_penta.addBulk( element(elem_dim=2, connectivity=[ 9, 13 ,18, 10]) )

   mesh_penta.addBulk( element(elem_dim=2, connectivity=[14, 19, 15, 12]) )
   mesh_penta.addBulk( element(elem_dim=2, connectivity=[19, 21, 16, 15]) )
   mesh_penta.addBulk( element(elem_dim=2, connectivity=[12, 15, 16, 11]) )

   mesh_penta.addBulk( element(elem_dim=2, connectivity=[11, 16, 17, 13]) )
   mesh_penta.addBulk( element(elem_dim=2, connectivity=[16, 21, 20, 17]) )
   mesh_penta.addBulk( element(elem_dim=2, connectivity=[17, 20, 18, 13]) )

   #   * les elements lineiques
   #     N.B.: la connectivite est donne dans le sens antitrigonmetrique qui
   #           est le sens conventionnel pour decrire les contacteurs
   mesh_penta.addBulk( element(elem_dim=1, connectivity=[ 7,  1], physicalEntity='skin') )
   mesh_penta.addBulk( element(elem_dim=1, connectivity=[ 1,  2], physicalEntity='skin') )
   mesh_penta.addBulk( element(elem_dim=1, connectivity=[ 2,  3], physicalEntity='skin') )
   mesh_penta.addBulk( element(elem_dim=1, connectivity=[ 3, 10], physicalEntity='skin') )
   mesh_penta.addBulk( element(elem_dim=1, connectivity=[10, 18], physicalEntity='skin') )
   mesh_penta.addBulk( element(elem_dim=1, connectivity=[18, 20], physicalEntity='skin') )
   mesh_penta.addBulk( element(elem_dim=1, connectivity=[20, 21], physicalEntity='skin') )
   mesh_penta.addBulk( element(elem_dim=1, connectivity=[21, 19], physicalEntity='skin') )
   mesh_penta.addBulk( element(elem_dim=1, connectivity=[19, 14], physicalEntity='skin') )
   mesh_penta.addBulk( element(elem_dim=1, connectivity=[14,  7], physicalEntity='skin') )

   # on renvoie le maillage ainsi construit
   return mesh_penta

####################################################################################
# fonction qui renvoie un maillage precalcule du pentagone unite
def MeshedUnitDisk():
   """mesh_disk=MeshedUnitDisk():

   this function builds and returns a precomputed mesh of the unit disk, centered on (0, 0)
   """

   # on definit l'objet maillage pour stocker le maillage du disque unitaire
   mesh_disk=mesh(dimension=2)


   # on ajoute les noeuds au maillage
   mesh_disk.addNode( node( coor=numpy.array([ 0.0000000e+00,  0.0000000e+00]), number= 1) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.5000000e-02,  0.0000000e+00]), number= 2) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.0000000e+00,  0.5000000e-02]), number= 3) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.4267767e-02,  0.4267767e-02]), number= 4) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.1000000e-01,  0.0000000e+00]), number= 5) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.1500000e-01,  0.0000000e+00]), number= 6) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.8535534e-02,  0.3535534e-02]), number= 7) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.1350656e-01,  0.5594601e-02]), number= 8) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.7071068e-02,  0.7071068e-02]), number= 9) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.1060660e-01,  0.1060660e-01]), number=10) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.3535534e-02,  0.8535534e-02]), number=11) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.5594601e-02,  0.1350656e-01]), number=12) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.0000000e+00, -0.5000000e-02]), number=13) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.4267767e-02, -0.4267767e-02]), number=14) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.8535534e-02, -0.3535534e-02]), number=15) ) 
   mesh_disk.addNode( node( coor=numpy.array([ 0.1350656e-01, -0.5594601e-02]), number=16) ) 
   mesh_disk.addNode( node( coor=numpy.array([ 0.7071068e-02, -0.7071068e-02]), number=17) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.1060660e-01, -0.1060660e-01]), number=18) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.3535534e-02, -0.8535534e-02]), number=19) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.5594601e-02, -0.1350656e-01]), number=20) )
   mesh_disk.addNode( node( coor=numpy.array([-0.5000000e-02,  0.0000000e+00]), number=21) )
   mesh_disk.addNode( node( coor=numpy.array([-0.4267767e-02,  0.4267767e-02]), number=22) )
   mesh_disk.addNode( node( coor=numpy.array([-0.1000000e-01,  0.0000000e+00]), number=23) )
   mesh_disk.addNode( node( coor=numpy.array([-0.1500000e-01,  0.0000000e+00]), number=24) )
   mesh_disk.addNode( node( coor=numpy.array([-0.8535534e-02,  0.3535534e-02]), number=25) )
   mesh_disk.addNode( node( coor=numpy.array([-0.1350656e-01,  0.5594601e-02]), number=26) )
   mesh_disk.addNode( node( coor=numpy.array([-0.7071068e-02,  0.7071068e-02]), number=27) )
   mesh_disk.addNode( node( coor=numpy.array([-0.1060660e-01,  0.1060660e-01]), number=28) )
   mesh_disk.addNode( node( coor=numpy.array([-0.3535534e-02,  0.8535534e-02]), number=29) )
   mesh_disk.addNode( node( coor=numpy.array([-0.5594601e-02,  0.1350656e-01]), number=30) )
   mesh_disk.addNode( node( coor=numpy.array([-0.4267767e-02, -0.4267767e-02]), number=31) )
   mesh_disk.addNode( node( coor=numpy.array([-0.8535534e-02, -0.3535534e-02]), number=32) )
   mesh_disk.addNode( node( coor=numpy.array([-0.1350656e-01, -0.5594601e-02]), number=33) )
   mesh_disk.addNode( node( coor=numpy.array([-0.7071068e-02, -0.7071068e-02]), number=34) )
   mesh_disk.addNode( node( coor=numpy.array([-0.1060660e-01, -0.1060660e-01]), number=35) )
   mesh_disk.addNode( node( coor=numpy.array([-0.3535534e-02, -0.8535534e-02]), number=36) )
   mesh_disk.addNode( node( coor=numpy.array([-0.5594601e-02, -0.1350656e-01]), number=37) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.2000000e-01,  0.0000000e+00]), number=38) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.1847759e-01,  0.7653669e-02]), number=39) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.1414214e-01,  0.1414214e-01]), number=40) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.7653669e-02,  0.1847759e-01]), number=41) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.0000000e+00, -0.1000000e-01]), number=42) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.0000000e+00, -0.1500000e-01]), number=43) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.0000000e+00,  0.1000000e-01]), number=44) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.0000000e+00,  0.1500000e-01]), number=45) )
   mesh_disk.addNode( node( coor=numpy.array([-0.2000000e-01,  0.0000000e+00]), number=46) )
   mesh_disk.addNode( node( coor=numpy.array([-0.1847759e-01, -0.7653669e-02]), number=47) )
   mesh_disk.addNode( node( coor=numpy.array([-0.1414214e-01, -0.1414214e-01]), number=48) )
   mesh_disk.addNode( node( coor=numpy.array([-0.7653669e-02, -0.1847759e-01]), number=49) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.0000000e+00,  0.2000000e-01]), number=50) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.1847759e-01, -0.7653669e-02]), number=51) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.1414214e-01, -0.1414214e-01]), number=52) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.7653669e-02, -0.1847759e-01]), number=53) )
   mesh_disk.addNode( node( coor=numpy.array([ 0.0000000e+00, -0.2000000e-01]), number=54) )
   mesh_disk.addNode( node( coor=numpy.array([-0.1847759e-01,  0.7653669e-02]), number=55) )
   mesh_disk.addNode( node( coor=numpy.array([-0.1414214e-01,  0.1414214e-01]), number=56) )
   mesh_disk.addNode( node( coor=numpy.array([-0.7653669e-02,  0.1847759e-01]), number=57) )

   # am : magouille heritee de preprogranul...
   for nod in mesh_disk.nodes:
      nod.coor *= 50.

   # on ajoute les elements au maillage

   #   * les elements surfaciques
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 1,  2,  4,  3]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 2,  5 , 7,  4]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 3,  4, 11, 44]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 4,  7,  9, 11]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 5,  6,  8,  7]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 6, 38, 39,  8]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 7,  8, 10,  9]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 8, 39, 40, 10]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 9, 10 ,12, 11]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[10, 40, 41, 12]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[11, 12, 45, 44]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[12, 41, 50, 45]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 1, 13, 14,  2]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 2, 14, 15,  5]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[13, 42, 19, 14]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[14, 19, 17, 15]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 5, 15, 16,  6]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 6, 16, 51, 38]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[15, 17, 18, 16]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[16, 18, 52, 51]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[17, 19, 20, 18]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[18, 20, 53, 52]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[19, 42, 43, 20]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[20, 43, 54, 53]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 1,  3, 22, 21]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[21, 22, 25, 23]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 3, 44, 29, 22]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[22, 29, 27, 25]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[23, 25, 26, 24]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[24, 26, 55, 46]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[25, 27 ,28, 26]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[26, 28, 56, 55]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[27, 29, 30, 28]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[28, 30, 57, 56]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[29, 44, 45, 30]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[30, 45, 50, 57]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[ 1, 21, 31, 13]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[21, 23, 32, 31]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[13, 31, 36, 42]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[31, 32, 34, 36]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[23, 24, 33, 32]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[24, 46, 47, 33]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[32, 33, 35, 34]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[33, 47, 48, 35]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[34, 35, 37, 36]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[35, 48, 49, 37]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[36, 37, 43, 42]) )
   mesh_disk.addBulk( element(elem_dim=2, connectivity=[37, 49, 54, 43]) )

   #   * les elements lineiques
   #     N.B.: la connectivite est donne dans le sens antitrigonmetrique qui
   #           est le sens conventionnel pour decrire les contacteurs
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[54, 49], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[49, 48], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[48, 47], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[47, 46], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[46, 55], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[55, 56], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[56, 57], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[57, 50], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[50, 41], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[41, 40], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[40, 39], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[39, 38], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[38, 51], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[51, 52], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[52, 53], physicalEntity='skin') )
   mesh_disk.addBulk( element(elem_dim=1, connectivity=[53, 54], physicalEntity='skin') )

   # on renvoie le maillage ainsi construit
   return mesh_disk

####################################################################################
# EXOTIC PURPOSE
#
def rigidDiscreteDisk(r, center, model, material, color='BLUEx', number=None):
   '''body=rigidDiscreteDisk(r, center, model, material, color='BLUEx', number=None):

   this function builds a rigid cluster of diskx contained in a disk and returns the generated body

   parameters :  

   - r : radius of the particle
   - center : position of the center of inertia in the global frame, as a two coordinates vector
   - model : rigid model for the particle
   - material : the particle is made of this material

   optional parameters :

   - color='BLUEx' : color of the contactors
   - number=None: index of the avatar (still present to ensure compatibility)'''

   # creation d'un nouveau disque rigide 2D
   body = avatar(dimension=2, number=number)
   # on cree comportement volumique de type rigide
   body.addBulk( rigid2d() ) 
   # ajout de la position du centre d'inertie au disque
   body.addNode( 
      node(coor=numpy.array(center),
      number=1) )
   # on definit les groupes pour le disque
   body.defineGroups()
   # on affecte son modele au disque
   body.defineModel(model=model)
   # on affecte son materiau au disque
   body.defineMaterial(material=material)

   # premier niveau
   r_disk = 0.5*r
   x_disk = -r*0.5
   y_disk =  0
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk =  r*0.5
   y_disk =  0
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   # Second niveau
   r_disk = 0.25*r
   x_disk =-r*0.25
   y_disk = r*0.7
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk = r*0.25
   y_disk = r*0.7
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk =-r*0.25
   y_disk =-r*0.7
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk = r*0.25
   y_disk =-r*0.7
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   #Troisieme niveau
   r_disk = 0.125*r

   x_disk =-r*0.62
   y_disk = r*0.62
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk = r*0.62
   y_disk = r*0.62
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk =-r*0.62
   y_disk =-r*0.62
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk = r*0.62
   y_disk =-r*0.62
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   # Quatrieme niveau
   r_disk = 0.06*r

   x_disk =-r*0.06
   y_disk = r*0.94
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk = r*0.06
   y_disk = r*0.94
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk =-r*0.06
   y_disk =-r*0.94
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk = r*0.06
   y_disk =-r*0.94
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   r_disk = 0.07*r

   x_disk =-r*0.78
   y_disk = r*0.50
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk = r*0.78
   y_disk = r*0.50
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk =-r*0.78
   y_disk =-r*0.50
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk = r*0.78
   y_disk =-r*0.50
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   r_disk = 0.082*r

   x_disk =-r*0.082
   y_disk = r*0.41
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk = r*0.082
   y_disk = r*0.41
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk =-r*0.082
   y_disk =-r*0.41
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])

   x_disk = r*0.082
   y_disk =-r*0.41
   body.addContactors(shape='DISKx', color=color, byrd=r_disk, shift=[x_disk, y_disk])


   # on calcule de la surface et de l'inertie du disque
   body.computeRigidProperties()

   # on renvoie le corps genere
   return body


###############################################################################
# mr: my purpose
# fonction qui construit un polygone rigide :
#   - center : position du centre d'inertie du polygone
#   - r : rayon du disque englobant
#   - nb_vertices : nombre sommets du polygone
#   - model : modele rigide pour la particule
#   - material : materiau dont la particule est faite
#   - number : numero du corps
# variables optionnelles
#   - theta : rotation du disque autour de son centre d'inertie
#   - color : couleur du polygone
def rigidOvoidPolygon(ra, rb, nb_vertices, center, model, material, theta=0., color='BLUEx', number=None):
   '''
   usage :

   body=rigidOvoidPolygon(ra, rb, nb_vertices, center, model, material, theta=0., color='BLUEx', number=None)

   this function builds a rigid polygon and returns the generated body

   parameters :  

    - ra : radius along x of the bounding disk
    - rb : radius along y of the bounding disk
    - nb_vertices : number of vertices of the polygon
    - center : position of the center of inertia in the global frame
    - model : rigid model for the particle
    - material : the particle is made of this material

   optional parameters :

    - theta=0. : rotation angle in the inertial frame
    - color='BLUEx' : color of the polygon contactor
    - number=None : index of the body
   '''

   body = avatar(dimension=2, number=number)
   body.addBulk( rigid2d() )
   body.addNode( node(coor=numpy.array(center), number=1) )
   body.defineGroups()
   body.defineModel(model=model)
   body.defineMaterial(material=material)

   # definition du contacteur pour le polygone :
   vertices = numpy.zeros([nb_vertices, 2], 'd')
   # on caclule les positions des sommets par rapport au centre d'inertie
   for i in range(0, nb_vertices, 1):
      vertices[i, 0] = ra*math.cos(2.*math.pi*i/float(nb_vertices)) 
      vertices[i, 1] = rb*math.sin(2.*math.pi*i/float(nb_vertices)) 
   body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertices, vertices=vertices)

   body.computeRigidProperties()

   if theta != 0.:
      # on tourne le disque autour de son centre d'inertie
      body.rotate(psi=theta, center=body.nodes[1].coor)

   return body


def buildPolyhedronConnectivity(vertices):
  """ faces = buildPolyhedronConnectivity(vertices)

  Generate the connectivity of the convex polyhedron from a
  list of vertices in space. The faces are all triangles.

  Parameters:

  - vertices : numpy array of dimension [nb_vertices,3]
  - faces : the connectivity of each face as a list of list
  """

  try:

    from scipy.spatial import Delaunay, ConvexHull

    chu = ConvexHull(vertices)
    ver = vertices[chu.vertices]
    ivm = np.zeros(vertices.shape[0])
    for i , v in enumerate(chu.vertices):
      ivm[v] =  i+1

    faces = ivm[chu.simplices]
    return faces, ver

  except:

    showWarning('Install scipy to improve efficiency and robustness')
    # At least 4 vertices for a polyhedron
    nb_vertices = vertices.shape[0]
    if nb_vertices < 4:
      return

    # Tetraheron definition
    # to be consistent with the contactor definition
    # the connectivity of a face must number the nodes from 1 to nb_vertices
    # and not from 0 to nb_vertices-1 which is the Python way.
    faces = []
    faces.append([ 1, 2, 3 ])
    faces.append([ 1, 3, 4 ])
    faces.append([ 4, 3, 2 ])
    faces.append([ 1, 4, 2 ])

    # find a fourth point not coplanar with the three firsts!
    perms = numpy.zeros(3)
    for i in range(4,nb_vertices):
      p1 = numpy.cross( vertices[1,:]-vertices[0,:] , vertices[2,:]-vertices[0,:] )
      p2 = numpy.dot( vertices[3,:]-vertices[0,:] , p1 )
      if p2 != 0 :
        break
      else :
        perms[:] = vertices[i,:]
        vertices[i,:] = vertices[3,:]
        vertices[3,:] = perms[:]

    # is the fourth point found 
    p1 = numpy.cross( vertices[1,:]-vertices[0,:] , vertices[2,:]-vertices[0,:] )
    p2 = numpy.dot( vertices[3,:]-vertices[0,:] , p1 )
    if p2 == 0 :
      showError('buildPolyhedronConnectivity : all vertices are coplanar')

    # center computation
    center =  numpy.sum(vertices[0:4,:],axis=0)
    center *= 0.25

    # normals computation
    normals = []
    for f in faces:
      v1 = vertices[f[1]-1,:] - vertices[f[0]-1,:]
      v2 = vertices[f[2]-1,:] - vertices[f[0]-1,:]
      n = numpy.cross( v1, v2 )
      n /= numpy.linalg.norm(n)
      if numpy.dot( n, vertices[f[1]-1,:]-center[:] ) < 0:
        t = f[1]
        f[1] = f[2]
        f[2] = t
        n = -n
      normals.append(n)

    # adding new points
    # but storing unused vertices
    v_to_remove = [] 
    for i_vert in range(4,vertices.shape[0]):

      new_faces   = []
      new_normals = []

      potential_edges = []
      print(i_vert, normals)
      for i_face in range(len(faces)):
        # if the new point is not seen by the face: store edges, otherwise store the face
        if numpy.dot( normals[i_face], vertices[faces[i_face][0]-1,:]-vertices[i_vert,:] ) < 0:
          potential_edges.append( [faces[i_face][0], faces[i_face][1] ] )
          potential_edges.append( [faces[i_face][0], faces[i_face][2] ] )
          potential_edges.append( [faces[i_face][1], faces[i_face][2] ] )
        else:
          new_faces.append(faces[i_face])
          new_normals.append(normals[i_face])

      # if the new point is inside the polyhdra: ignore the point
      if len(potential_edges) == 0:
        v_to_remove.append(i_vert)
        continue

      # remove duplicate edges
      new_edges = []
      for e in potential_edges:
        c = potential_edges.count(e) + potential_edges.count(e[::-1])
        if c == 1:
          new_edges.append(e)

      # compute new center
      center = (center*i_vert + vertices[i_vert,:]) / (i_vert+1)

      # adding new face corresponding to each edge + the new point
      for e in new_edges:
        v1 = vertices[e[1]-1,:] - vertices[e[0]-1,:]
        v2 = vertices[i_vert,:] - vertices[e[0]-1,:]
        n = numpy.cross( v1, v2 )
        n /= numpy.linalg.norm(n)
        if numpy.dot( n, vertices[i_vert,:]-center[:] ) < 0:
          new_faces.append( [e[1],e[0],i_vert+1] )
          n = -n
        else:
          new_faces.append( [e[0],e[1],i_vert+1] )
        new_normals.append(n)

      normals = new_normals
      faces   = new_faces

    #removing unused vertices:
    if( len(v_to_remove) > 0 ):
      v_to_remove.sort()
      v_to_remove = numpy.array(v_to_remove)
      vertices = numpy.delete(vertices, v_to_remove, 0)

      #update numbering of faces
      for f in faces:
        for i_v in range(3):
          s = sum( f[i_v] > v_to_remove+1 )
          f[i_v] -= s

    return faces, vertices

def getRandomPolyhedronVertices(nb_vertices, radius, tol, seed=None):
  """ vertices = getRandomPolyhedronVertices(nb_vertices, radius, tol, s=None)
  
  Generate randomly disposed vertices on a sphere

  parameters:

  - nb_vertices : number of vertices to generate
  - radius : radius of the sphere on which the vertices are put
  - tol : tolerance to use to remove a vertex if the one found is to close to others (if 0. no check)
  - vertices : the numpy array containing the coordinates of the vertices

  optional:
  - seed=None : seed to use to control the randomness
  """

  if seed is not None : numpy.random.seed(s)

  vertices = numpy.random.rand(nb_vertices, 3)
  vertices[:,0] =  radius
  vertices[:,1] *= math.pi
  vertices[:,2] *= 2.*math.pi

  new_vertices = numpy.zeros([nb_vertices,3])
  new_vertices[:,0] = vertices[:,0] * numpy.sin(vertices[:,1]) * numpy.cos(vertices[:,2])
  new_vertices[:,1] = vertices[:,0] * numpy.sin(vertices[:,1]) * numpy.sin(vertices[:,2])
  new_vertices[:,2] = vertices[:,0] * numpy.cos(vertices[:,1])

  if tol == 0.:
    return new_vertices

  # looking for points to close from others
  to_remove = []
  for i in range(nb_vertices):
    for j in range(i+1,nb_vertices):
      n = numpy.linalg.norm(vertices[i,:]) - numpy.linalg.norm(vertices[j,:])
      if abs(n) < tol:
        to_remove.append(i)
        break

  # remove points too close
  count = 0
  new_new_vertices = numpy.zeros([nb_vertices-len(to_remove),3])
  
  for i in range(nb_vertices):
    if i in to_remove:
      count += 1
      continue
    else:
      new_new_vertices[i-count,:] = new_vertices[i,:]
  
  return new_new_vertices

def getRegularPolyhedronVertices(nb_vertices, radius):
  """ vertices = getRegularPolyhedronVertices(nb_vertices, radius)
  
  Generate uniformly disposed vertices on a sphere. Particular case for platon's solids.
  Otherwise, a suiting nunber of vertices is looked for.

  parameters:

  - nb_vertices : number of vertices to generate
  - radius : radius of the sphere on which the vertices are put
  - vertices : the numpy array containing the coordinates of the vertices
  """

  vertices  = numpy.zeros([nb_vertices,3])

  if nb_vertices == 4:
    #tetrahedron
    vertices[0,:] = [0.,                         0.,                       radius]
    vertices[1,:] = [radius*2.*math.sqrt(2.)/3., 0.,                      -radius/3.]
    vertices[2,:] = [  -radius*math.sqrt(2.)/3., radius*math.sqrt(6.)/3., -radius/3.]
    vertices[3,:] = [  -radius*math.sqrt(2.)/3.,-radius*math.sqrt(6.)/3., -radius/3.]
  elif nb_vertices == 6:
    #octahedron
    vertices[0,:] = [-radius, 0.    , 0.    ]
    vertices[1,:] = [ radius, 0.    , 0.    ]
    vertices[2,:] = [ 0.    , radius, 0.    ]
    vertices[3,:] = [ 0.    ,-radius, 0.    ]
    vertices[4,:] = [ 0.    , 0.    , radius]
    vertices[5,:] = [ 0.    , 0.    ,-radius]
  elif nb_vertices == 8:
    #hexahedron (cube)
    s = radius / math.sqrt(3.)
    vertices[0,:] = [-s,-s,-s]
    vertices[1,:] = [ s,-s,-s]
    vertices[2,:] = [ s, s,-s]
    vertices[3,:] = [-s, s,-s]
    vertices[4,:] = [-s,-s, s]
    vertices[5,:] = [ s,-s, s]
    vertices[6,:] = [ s, s, s]
    vertices[7,:] = [-s, s, s]
  elif nb_vertices == 12:
    #icosahedron
    t = (1.+math.sqrt(5.))/2.
    s = radius / math.sqrt(1.+t**2)
    vertices[ 0,:] = [ t , 1., 0.]
    vertices[ 1,:] = [-t , 1., 0.]
    vertices[ 2,:] = [ t ,-1., 0.]
    vertices[ 3,:] = [-t ,-1., 0.]
    vertices[ 4,:] = [ 1., 0., t ]
    vertices[ 5,:] = [ 1., 0.,-t ]
    vertices[ 6,:] = [-1., 0., t ]
    vertices[ 7,:] = [-1., 0.,-t ]
    vertices[ 8,:] = [ 0., t , 1.]
    vertices[ 9,:] = [ 0.,-t , 1.]
    vertices[10,:] = [ 0., t ,-1.]
    vertices[11,:] = [ 0.,-t ,-1.]
    vertices *= s
  elif nb_vertices == 20:
    #dodecahedron
    a = radius / math.sqrt(3.)
    b = radius * math.sqrt( 0.5-math.sqrt(5.)/6. )
    c = radius * math.sqrt( 0.5+math.sqrt(5.)/6. )
    vertices[ 0,:] = [ a, a, a]
    vertices[ 1,:] = [ a, a,-a]
    vertices[ 2,:] = [ a,-a, a]
    vertices[ 3,:] = [ a,-a,-a]
    vertices[ 4,:] = [-a, a, a]
    vertices[ 5,:] = [-a, a,-a]
    vertices[ 6,:] = [-a,-a, a]
    vertices[ 7,:] = [-a,-a,-a]
    vertices[ 8,:] = [ b, c, 0]
    vertices[ 9,:] = [-b, c, 0]
    vertices[10,:] = [ b,-c, 0]
    vertices[11,:] = [-b,-c, 0]
    vertices[12,:] = [ c, 0, b]
    vertices[13,:] = [ c, 0,-b]
    vertices[14,:] = [-c, 0, b]
    vertices[15,:] = [-c, 0,-b]
    vertices[16,:] = [ 0, b, c]
    vertices[17,:] = [ 0,-b, c]
    vertices[18,:] = [ 0, b,-c]
    vertices[19,:] = [ 0,-b,-c]
  else:


    # trying to find spherical resolution to match the desired number of vertices
    x = int(math.floor(math.sqrt(nb_vertices//2)))

    # if number of vertices fall into platon polyhedron:
    if 2*x*x+2 in [4,6,8,12,20]:
      vertices = getRegularPolyhedronVertices(2*x*x+2, radius)

    else:
      sphere_v = numpy.zeros([2*x*x+2,2])
      vertices  = numpy.zeros([2*x*x+2,3])

      #sphere_v[:,0] : colatitude
      #sphere_v[:,1] : longitude

      # adding two poles
      sphere_v[ 0,0] = 0
      sphere_v[ 0,1] = 0
      sphere_v[-1,0] = math.pi
      sphere_v[-1,1] = 0

      count = 1
      for i in range(x): # loop on colatitude
        for j in range(2*x): # loop on longitude
          sphere_v[count,0] = (i+1) * math.pi / (x+1)
          sphere_v[count,1] = j * math.pi / x
          count += 1

      vertices[:,0] = radius * numpy.sin(sphere_v[:,0]) * numpy.cos(sphere_v[:,1])
      vertices[:,1] = radius * numpy.sin(sphere_v[:,0]) * numpy.sin(sphere_v[:,1])
      vertices[:,2] = radius * numpy.cos(sphere_v[:,0])

  return vertices

########################################################################################################
def MOS2Polygon(ra, rb, center, model, material, theta=0., number=None):
   '''
   usage:

   body=MOS2Polygon(ra, rb, center, model, material, theta=0., number=None)

   this function builds a rigid polygon and returns the generated body

   parameters :  

   - ra : radius along x of the bounding disk
   - rb : radius along y of the bounding disk
   - center : position of the center of inertia in the global frame
   - model : rigid model for the particle
   - material : the particle is made of this material

   optional parameters :

   - theta=0. : rotation angle in the inertial frame
   - number : index of the body
   '''

   # creation d'un nouveau polygone rigide 2D
   body = avatar(dimension=2, number=number)
   # on cree comportement volumique de type rigide
   body.addBulk( rigid2d() )
   # ajout de la position du centre d'inertie au disque
   body.addNode( 
      node(coor=numpy.array(center),
      number=1) )
   # on definit les groupes pour le disque
   body.defineGroups()
   # on affecte son modele au disque
   body.defineModel(model=model)
   # on affecte son materiau au disque
   body.defineMaterial(material=material)

   # definition du contacteur pour le polygone :
   
   # on cree une matrice de double a la bonne taille
   vertices = numpy.zeros([4, 2], 'd')
   
   vertices[0, 0] = ra
   vertices[0, 1] = rb
   vertices[1, 0] =-ra
   vertices[1, 1] = rb
   vertices[2, 0] =-ra
   vertices[2, 1] =-rb
   vertices[3, 0] = ra
   vertices[3, 1] =-rb

   body.addContactors(shape='POLYG', color='INTER', nb_vertices=4, vertices=vertices)

   vertices[0, 0] =-ra
   vertices[0, 1] = rb
   vertices[1, 0] =-ra-rb*0.5
   vertices[1, 1] = 0.5*rb
   vertices[2, 0] =-ra-rb*0.5
   vertices[2, 1] =-0.5*rb
   vertices[3, 0] =-ra
   vertices[3, 1] =-rb
   
   body.addContactors(shape='POLYG', color='EXTER', nb_vertices=4, vertices=vertices)

   vertices[0, 0] = ra
   vertices[0, 1] = rb
   vertices[1, 0] = ra+rb*0.5
   vertices[1, 1] = 0.5*rb
   vertices[2, 0] = ra+rb*0.5
   vertices[2, 1] =-0.5*rb
   vertices[3, 0] = ra
   vertices[3, 1] =-rb
   
   body.addContactors(shape='POLYG', color='EXTER', nb_vertices=4, vertices=vertices)

   # on calcule de la surface et de l'inertie du cluster
   body.computeRigidProperties()

   # si on donne un angle non nul
   if theta != 0.:
      # on tourne le disque autour de son centre d'inertie
      body.rotate(psi=theta, center=body.nodes[1].coor)

   # on renvoie le corps genere
   return body
