# module qui fournit des macros pour extruder des corps rigides 2D

import numpy
import math
from copy import deepcopy

from .mesh import *

from ..avatars import *
from ..avatar.avatar import *
from ..avatar.bulk.rigid3d import *

from ..utilities.error    import *

# fonction qui extrude un rigide 2D (defini dans le plan xOy), suivant l'axe Oz
def extrudeRigid(body2D, model3D, depth, factor=1.e0, extrudedDisk='Sphere', number=None): 
   '''extrudeRigid(body2D, model3D, depth, factor=1.e0, number=None):

   this function builds a 3D rigid body by extruding a given 2D rigid avatar and returns 
   the generated body. The extruded body is made of the same material as the
   given one. The colors of each contactor of the body is the same as those of
   the coresponding contactor of the given body.

   Warning: extrusion follow axis Oz, except for the JONCx, which are placed 
   in xOz plane

   parameters:  

   - body2D: a 2D rigid body
   - model3D: 3D rigid model for the extruded body
   - depth: depth of the extrusion

   optional paramters:

   - factor: dilatation factor of homothety
   - extrudedDisk: a string used to define how to extrude a disk:

     * 'Sphere': for a sphere (SPHER)
     * 'Cylinder': for a solid cylinder (CYLND)

   - number=None: index of the avatar (still present to ensure compatibility)'''

   # verification de la nature de l'objet donne
   if not isinstance(body2D, avatar):
      showError("In extrudeRigid: first argument of this function must be an avatar!")

   # verification du type d'avatar
   if body2D.atype != 'RBDY2':
      showError("In extrudeRigid: first argument of this function must be a rigid avatar!")

   # creation d'un nouveau corps rigide 3D
   # N.B.: on ne renseigne ni le rayoin pour la masse, ni les inerties 
   #       principales pour qu'elles soient calculees par LMGC90
   body3D = avatar(dimension=3, number=number) 
   # on lui attribue un comportement volumique de type rigide
   body3D.addBulk( rigid3d() ) 

   # on recupere les coordonnees du centre d'inertie du corps 2D
   mass_center = body2D.nodes[1].coor 

   # on determine si le corps est pas un cluster (i.e. possede un seul 
   # contacteur : le numero 1)
   is_cluster = len(body2D.contactors) > 1

   # on determine si le coprs possede (au moins) un contacteur polygone
   has_polygon = False
   for contactor in body2D.contactors:
      if contactor.shape == 'POLYG':
         has_polygon = True
         break
   
   # on determine si le corps est un jonc (i.e. possede un contacteur jonc)
   is_jonc = False
   for contactor in body2D.contactors:
      if contactor.shape == 'JONCx':
         is_jonc = True
         break

   # si le corps est un cluster ou contient un polygone
   if is_cluster or has_polygon:
      # on place le centre d'inertie a l'origine, pour que LMGC90 le 
      # recalcule a partir des positions des contacteurs (sommets ou centres)
      body3D.addNode( 
         node(coor=numpy.array([0., 0., 0.]),
         number=1) )
   # sinon,
   else:
      # si le corps est un jonc
      if is_jonc:
         # on place le centre d'inertie dans le plan y = depth/2    
         body3D.addNode( 
            node(coor=numpy.array([factor*mass_center[0], 
               0.5*depth, factor*mass_center[1]]), number=1) )
      # sinon,
      else:
         # on place le centre d'inertie dans le plan z = depth/2    
         # N.B. : le corps est donc un disque (creux ou plein)
         body3D.addNode( 
            node(coor=numpy.array([factor*mass_center[0], 
               factor*mass_center[1], 0.5*depth]), number=1) )

   # on definit les groupes pour le corps
   body3D.defineGroups()
   # on affecte son modele au corps
   body3D.defineModel(model=model3D)
   # on affecte au corps le materiau constitutif du corps 2D
   body3D.defineMaterial(material=body2D.bulks[0].material)

   # pour chaque contacteur
   for contactor in body2D.contactors:
      try:
         try:
            # on tente de calculer le type et les options du contacteur constituant l'extrusion 3D du contacteur courant
            shape, options=contactor.extrusion(mass_center=mass_center, depth=depth, factor=factor, extrudedDisk=extrudedDisk)
            # si ca reussi, on ajoute le contacteur au corps resulatant de l'extrusion
            body3D.addContactors(shape=shape, color=contactor.color, **options)
         except ValueError as e:
            # si on echoue a cause de la valeur d'une option, on affiche le warning ad hoc
            showWarning(str(e))
      except NotImplementedError:
         # si on echoue parce que le contatceur n'a pas defini sa facon d'etre extrude, on previent l'utilisateur
         showWarning("In extrudeRigid: a contactor " + contactor.shape + " CANNOT be extruded! Skipping.")

   # si le corps n'est pas un jonc, on passe la face extrude (z=depth) du plan xOy, au plan xOz (y=0)
   if not is_jonc:
      # on calcule le volume et l'inertie des objets extrudes (=> calcul du frame attache a chaque bulk)
      body3D.computeRigidProperties()
      # on tourne le corps
      body3D.rotate(theta=0.5*math.pi, center=[0., 0., depth])
      # on translate le corps
      body3D.translate(dz=-depth)

   # on renvoie le corps genere
   return body3D

# fonction qui extrude un ensemble de corps rigides 2D et place
# les corps extrudes dans le plan xOz
def extrudeRigids(bodies2D, model3D, depth, factor=1.e0, extrudedDisk='Sphere', number=None): 
   '''extrudeRigids(bodies2D, model3D, depth, factor=1.e0, number=None):

   this function extrudes each avatar in a given avatar container and returns
   the generated list of 3D avatars in a new avatar container.

   Warning: extrusion follow axis Oz, except for the JONCx, which are placed 
   in xOz plane

   parameters:  

   - bodies2D: a 2D rigid avatar container
   - model3D: 3D rigid model for the extruded body
   - depth: depth of the extrusion

   optional paramters:

   - factor: dilatation factor of homothety
   - extrudedDisk: a string used to define how to extrude a disk:

     * 'Sphere': for a sphere (SPHER)
     * 'Cylinder': for a solid cylinder (CYLND)

   - number=None: index of the avatar (still present to ensure compatibility)'''

   if not isinstance(bodies2D, avatars):
     showError("In extrudeRigids: first argument of this function must be an avatar container!")
  
   # on cree un conteneur d'avatars pour stocker les corps extrudes
   bodies3D=avatars()
  
   # pour chaque corps dans le container
   for body2D in bodies2D:
     # on extrude le corps courant, pour obtenir un corps 3D
     body3D = extrudeRigid(body2D=body2D, model3D=model3D,
     depth=depth, factor=factor, extrudedDisk=extrudedDisk)
     # on l'ajoute a la liste des corps
     bodies3D += body3D
  
   # on renvoie la liste de corps generee
   return bodies3D

# extrude plan meshes along z-axis
def extrudeMesh(m, lz, nb_lz=1):
   '''vol_mesh = extrudeMesh(m, lz, nb_lz=1):

   This function returns the extrusion of an input linear mesh along z-axis.

   parameters:  

   - m: a 2D mesh
   - lz: height of the extruded mesh
   - nb_lz: number of layers to extrude

   All elements are extruded:
     Point->S2
     S2->Q4
     T3->PR6
     Q4->H8

   All elements of the original surface are kept on the surface at bottom and top.
   All physical entities name are modified a 'E' is appended if the element is the
   result of the extrusion, a 'B' is appended if the element is the original bottom
   element and a 'T' is appended if the element is the top element of the original
   surface.

   '''

   # check object tyep
   if not isinstance(m, mesh):
      showError("In extrudeMesh: first argument of this function must be an mesh object!")
   # should also check dimension ?

   vol_mesh = mesh(3)
   
   # possible extruded elements
   connec_type = ['NOTHING','S2xxx','Q4xxx','PRI6x','H8xxx']

   #adding nodes of each new layers
   nb_nodes = len(m.nodes)
   for l in range(nb_lz+1):
     for n in m.nodes:
       new_coor = numpy.array([n.coor[0],n.coor[1],l*lz/nb_lz])
       vol_mesh.addNode(node(coor=new_coor,number=l*nb_nodes+n.number))

   #adding elements
   for i, e in enumerate(m.bulks):
     elem_size = len(e.connectivity)
     if elem_size < 1 or elem_size > 4 :
       showWarning("in extrudeMesh: cannot extrude element os size "+str(elemen_size))
       continue

     #check if connect is to be permuted...
     perm = False
     if elem_size == 3 or elem_size == 4:
       normal = numpy.cross(vol_mesh.nodes[e.connectivity[1]].coor-vol_mesh.nodes[e.connectivity[0]].coor,
                            vol_mesh.nodes[e.connectivity[elem_size-1]].coor-vol_mesh.nodes[e.connectivity[0]].coor)
       if normal[2]*lz < 0:
         perm = True

     new_c = deepcopy(e.connectivity)
     if perm:
       new_c.reverse()

     #perm top layer when extruding S2->Q4
     if elem_size == 2:
       new_c = new_c + [x+nb_nodes for x in new_c[::-1]]
     else:
       new_c = new_c + [x+nb_nodes for x in new_c]

     vol_mesh.addBulk(element(connec_type[elem_size], new_c, e.physicalEntity+'E',e.geometricalEntity))
     for l in range(1,nb_lz):
       nnew_c = [ c+l*nb_nodes for c in new_c ]
       vol_mesh.addBulk(element(connec_type[elem_size], nnew_c, e.physicalEntity+'E',e.geometricalEntity))

     elem_dim = geoElement2dimension[e.etype]
     #needs to add original surfacic elements on top and bottom
     if perm:
       vol_mesh.addBulk(element(elem_dim, e.connectivity, e.physicalEntity+'B',e.geometricalEntity))
       vol_mesh.addBulk(element(elem_dim, [x+nb_lz*nb_nodes for x in e.connectivity[::-1]], e.physicalEntity+'T',e.geometricalEntity))
     else:
       vol_mesh.addBulk(element(elem_dim, e.connectivity[::-1], e.physicalEntity+'B',e.geometricalEntity))
       vol_mesh.addBulk(element(elem_dim, [x+nb_lz*nb_nodes for x in e.connectivity], e.physicalEntity+'T',e.geometricalEntity))

   return vol_mesh

