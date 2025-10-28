# module dedie au depot de particules sur un reseau

import numpy

# fonction qui genere une liste de coordonnees sur un resau cubique
# parametres :
#   - nb_ele_x : nombre de particules sur la premiere couche, dans la direction
#         x
#   - nb_ele_y : nombre de particules sur la premiere couche, dans la direction
#         y
#   - nb_layer : nombre de couches
#   - l : longueur d'un element du reseau, i.e. distance entre deux centres de
#         particules
# N.B. : le nombre total de positions generees est : 
#           nb_ele_x * nb_ele_y * nb_layer
# parametres optionnels :
#   - (x0, y0, z0) : coordonnees du coin inferieur gauche de la boite a remplir
#                i.e. la premiere position genere est 
#                (x0 + l/2, y0 + l/2, z0 + l/2)
# valeur de retour :
#   - vecteur des coordonnees [x1, y1, z1, x2, y2, z2, ...]
# ATTENTION : 
#    1. les particules a deposer sur le reseau doivent verifier : 
#          max(rayons) <= l/2
#    2. l'ensemble particules deposees sur ce resau est inclus dans une boite 
#       rectangulaire de dimensions : nb_ele_x*l x nb_ele_y*l x nb_layer*l
def cubicLattice3D(nb_ele_x, nb_ele_y, nb_layer, l, x0=0., y0=0., z0=0.):
   '''coor=cubicLattice3D(nb_ele_x, nb_ele_y, nb_layer, l, x0=0., y0=0., z0=0.):

  this function compute a list of positions on a cubic lattice

  parameters:

  - nb_ele_x: number of particles on the first layer, following axis (Ox) (the lowest)
  - nb_ele_y: number of particles on the first layer, following axis (Oy) (the lowest)
  - nb_layer: number of layers
  - l: length of a lattice element, i.e. distance between two 
    consecutive positions on the same layer, or the same column

  optional parameters:

  - (x0, y0, z0): position of the lower left corner of the bounding box 
    of the lattice, i.e. the first position is (x0 + l/2, y0 + l/2, z0 + l/2)

  return value:

  - coordinates of the positions [nb_ele_x*nb_ele_y*nb_layer,3]

  N.B.: the total number of positions is nb_ele_x*nb_ele_y*nb_layer

  WARNING:

  1. the maximal radius of the particles to be deposited max_radius must 
     verify: max_radius <= l/2
  2. the dimensions of the bounding box of the lattice are :
     nb_ele_x*l x nb_ele_y x nb_layer*l'''

   x = numpy.linspace( x0+0.5*l, x0+(nb_ele_x-0.5)*l, num=nb_ele_x, endpoint=True)
   y = numpy.linspace( y0+0.5*l, y0+(nb_ele_y-0.5)*l, num=nb_ele_y, endpoint=True)
   z = numpy.linspace( z0+0.5*l, z0+(nb_layer-0.5)*l, num=nb_layer, endpoint=True)

   # on initialise le vecteur qui va recevoir les coordonnees
   coor = numpy.empty([nb_ele_x,nb_ele_y,nb_layer,3], float)

   # on construit la liste de positions
   coor[:,:,:,0], coor[:,:,:,1], coor[:,:,:,2] = numpy.meshgrid(x,y,z)

   # on renvoie la liste de coordonnees generee
   coor.shape = [nb_ele_x*nb_ele_y*nb_layer,3]
   return coor

