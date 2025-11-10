# ce module, dedie a un usage interne au pre, vise a rassembler les routines de calcul de volumes et d'inerties, de contacteurs 3D 
import numpy

from ...config.lmgc90dicts import *

# fonction qui calcule le volume, l'inertie et la position du centre d'inertie (matrice d'inertie exprimee dans 
# le repere global) d'un corps maille en tetraedres
def computeVolumeInertiaMesh(volumic_mesh):
   """computeVolumeInertiaMesh(volumic_mesh):
      this function computes the volume, inertia and the position of the center of mass 
      of the a 3D meshed body
      parameters:
         - volumic_mesh: a given volumic mesh
      returned value: the computed volume, inertia matrix and center of mass coordinates as a tuple
   """

   # constante utile
   un_6 = 1.0/6.0

   # on recupere le nombre d'elements du maillage
   nb_ele = len(volumic_mesh.bulks)

   # on prepare le stockage des jacobiens de chaque tetraedre
   # en effet, pour le calcul du volume et de l'inertie du
   # polyedre, on a besoin du determinant de la matrice jacobienne
   # de la transformation qui envoie le tetraedre de reference 
   # sur le tetraedre reel
   detJ = numpy.zeros(nb_ele, 'd')
   # on prepare le stockage des positions des centres d'inertie
   # de chaque tetraedre, par rapport a l'origine du repere global
   tetra_OG = numpy.zeros([nb_ele, 3], 'd')

   # calcul du centre d'inertie et du volume du corps
  
   # on initialise le volume du maillage a 0
   volume = 0.
   # on initialise le centre d'inertie a l'origine
   OG = numpy.zeros(3, 'd')
   # pour chaque element (on utilise une enumeration pour recuperer un indice pour l'element courant)
   for i, bulk in enumerate(volumic_mesh.bulks):
      # si l'element n'est pas un element de volume
      if not bulk.etype in dimension2geoElement[3]:
         # on passe au suivant
         continue
      # si l'element n'est pas un tetradre
      if bulk.etype != 'TE4xx':
         # on affiche un warning
         showWarning("current element is not a tetraedron and will be skipped!")
         # on passe au suivant
         continue

      # on recupere les coordonnees des sommets du tetraedre courant
      p1 = numpy.array(volumic_mesh.nodes[bulk.connectivity[0]].coor)
      p2 = numpy.array(volumic_mesh.nodes[bulk.connectivity[1]].coor) 
      p3 = numpy.array(volumic_mesh.nodes[bulk.connectivity[2]].coor) 
      p4 = numpy.array(volumic_mesh.nodes[bulk.connectivity[3]].coor) 

      # on calcule le jacobien pour le tetraedre courant
      detJ[i] = compute_jacobian(p1, p2, p3, p4) 

      # si le jacobien est negatif
      if detJ[i] < 0.:
         # on affiche un warning
         showWarning('element ' + str(i) + ' dans le mauvais sens')
         # on change son signe      
         detJ[i] = -detJ[i]
      # on en deduit le volume du tetraedre courant
      tetra_vol = un_6*detJ[i]

      # on ajoute la contribution du tetraedre courant au volume
      # du corps maille
      volume += tetra_vol

      # on calcule la postion du centre d'inertie du tetraedre
      tetra_OG[i, :] = 0.25*(p1 + p2 + p3 + p4)
      # on ajoute la contribution du tetraedre a la position du centre 
      # d'inertie du corps maille
      OG += tetra_vol*tetra_OG[i, :]

   # si le volume du corps est nul
   if volume < 1.e-18:
      showError('volume is less than 1.e-18')

   # on divise par le volume pour finir le calcul du centre d'inertie du corps
   OG /= volume

   # calcul de l'inertie du corps

   # on initialise la matrice d'inertie du polyedre a la matrice nulle
   I = numpy.zeros([3, 3], 'd')
   # pour chaque element (on utilise une enumeration pour recuperer un indice pour l'element courant)
   for i, bulk in enumerate(volumic_mesh.bulks):
      # si l'element n'est pas un element de volume
      if not bulk.etype in dimension2geoElement[3]:
         # on passe au suivant
         continue
      # si l'element n'est pas un tetradre
      if bulk.etype != 'TE4xx':
         # on affiche un warning
         showWarning("current element is not a tetraedron and will be skipped!")
         # on passe au suivant
         continue

      # on recupere les coordonnees des sommets du tetraedre courant
      p1 = numpy.array(volumic_mesh.nodes[bulk.connectivity[0]].coor)
      p2 = numpy.array(volumic_mesh.nodes[bulk.connectivity[1]].coor) 
      p3 = numpy.array(volumic_mesh.nodes[bulk.connectivity[2]].coor)
      p4 = numpy.array(volumic_mesh.nodes[bulk.connectivity[3]].coor) 

      # on exrpime les coordonnees des sommets du tetraedre courant 
      # par rapport par rapport a son centre d'inertie
      p1 -= tetra_OG[i, :]
      p2 -= tetra_OG[i, :]
      p3 -= tetra_OG[i, :]
      p4 -= tetra_OG[i, :]

      # on peut alors calculer l'inertie du tetraedre courant
      tetra_I = compute_inertia_tetrahedron(p1, p2, p3, p4, detJ[i]) 

      # on ajoute la contributuion du tetraedre courant a l'inertie du polyedre (formule de Huygens)
      
      # on ajoute l'inertie du tertraedre courant
      I += tetra_I
      # on calcule shift entre le centre d'inertie du corps et le centre d'inertie du 
      # tetraedre courant 
      d = OG - tetra_OG[i, :]
      # on calcule le volume du tetraedre courant
      tetra_vol = un_6*detJ[i]
      # on en deduit :
      #    * la contribution de la distance a l'axe aux termes diagonaux
      I[0, 0] += tetra_vol*(d[1]*d[1] + d[2]*d[2])
      I[1, 1] += tetra_vol*(d[0]*d[0] + d[2]*d[2])
      I[2, 2] += tetra_vol*(d[0]*d[0] + d[1]*d[1])
      #    * la contribution de la distance a l'axe aux termes extra-diagonaux
      I[0, 1] -= tetra_vol*d[0]*d[1]
      I[1, 0] -= tetra_vol*d[0]*d[1]
      I[0, 2] -= tetra_vol*d[0]*d[2]
      I[2, 0] -= tetra_vol*d[0]*d[2]
      I[1, 2] -= tetra_vol*d[1]*d[2]
      I[2, 1] -= tetra_vol*d[1]*d[2] 
     
   return volume, I, OG

# focntion qui calcule le determinant de la matrice jacobienne de l'application
# qui envoie tetraedre de reference sur le tetraedre reel
def compute_jacobian(p1, p2, p3, p4):
   """compute_jacobian(p1, p2, p3, p4):
      this function computes and returns the determinant of the jacobian matrix of the transformation
      sending the reference tetrahedron on the real tetrahedron
      parameters:
         - p1: first vertex of the tetrahedron
         - p2: second vertex of the tetrahedron
         - p3: third vertex of the tetrahedron
         - p4: fourth vertex of the tetrahedron
      returned value: the computed determinant
   """
   
   # on calcule les vecteurs :
   #    * P1P2
   c1 = p2 - p1
   #    * P1P3
   c2 = p3 - p1
   #    * P1P4
   c3 = p4 - p1

   # on stocke le triedre (P1P2, P1P3, P1P4) dans une matrice
   M = numpy.array([c1, c2, c3])

   # le determinant de la matrice jacobienne est le determinant 
   # du triedrede (P1P2, P1P3, P1P4), soit cette matrice
   return numpy.linalg.det(M)

# fonction qui calcule l'inertie d'un tetraedre, a partir de la position de ces sommets
# et du jacobien calcule par la fonction ci-dessus
# (formule tiree de : Tonon F., "Explicit Exact Formulas for the 3-D Tetrahedron Inertia Tensor in Terms of its Vertex Coordinates", Journal of Mathematics and Statistics 1, vol. 1, pp 8-11, 2004
def compute_inertia_tetrahedron(p1, p2, p3, p4, detJ):
   """compute_inertia_tetrahedron(p1, p2, p3, p4, detJ):
      this function computes and returns the inertia matrix of a tetrahedron, from
      the coordinates of its vertex and the jacobian (cf. compute_jacobian)
      parameters:
         - p1: first vertex of the tetrahedron
         - p2: second vertex of the tetrahedron
         - p3: third vertex of the tetrahedron
         - p4: fourth vertex of the tetrahedron
         - detJ: the coresponding jacobian (cf. compute_jacobian)
      retruned value: the inertia matric of the polyhedron
   """

   # constantes utiles
   inv60 = 1./60.
   inv120 = 1./120.

   # on renomme les coordonnees des sommets, 
   # pour garder les noataions des formules de l'article
   x1 = p1[0] ; y1 = p1[1] ; z1 = p1[2]
   x2 = p2[0] ; y2 = p2[1] ; z2 = p2[2]
   x3 = p3[0] ; y3 = p3[1] ; z3 = p3[2]
   x4 = p4[0] ; y4 = p4[1] ; z4 = p4[2]

   # on cree la matrice d'inertie du tetraedre
   I0 = numpy.zeros([3, 3], 'd')

   # a
   I0[0, 0] = detJ*inv60* \
        (y1*y1 + y1*y2 + y2*y2 + y1*y3 + y2*y3 + \
         y3*y3 + y1*y4 + y2*y4 + y3*y4 + y4*y4 + \
         z1*z1 + z1*z2 + z2*z2 + z1*z3 + z2*z3 + \
         z3*z3 + z1*z4 + z2*z4 + z3*z4 + z4*z4)
   # b
   I0[1, 1] = detJ*inv60* \
        (x1*x1 + x1*x2 + x2*x2 + x1*x3 + x2*x3 + \
         x3*x3 + x1*x4 + x2*x4 + x3*x4 + x4*x4 + \
         z1*z1 + z1*z2 + z2*z2 + z1*z3 + z2*z3 + \
         z3*z3 + z1*z4 + z2*z4 + z3*z4 + z4*z4)
   # c
   I0[2, 2] = detJ*inv60* \
        (x1*x1 + x1*x2 + x2*x2 + x1*x3 + x2*x3 + \
         x3*x3 + x1*x4 + x2*x4 + x3*x4 + x4*x4 + \
         y1*y1 + y1*y2 + y2*y2 + y1*y3 + y2*y3 + \
         y3*y3 + y1*y4 + y2*y4 + y3*y4 + y4*y4)

   # yz
   I0[1, 2] = -detJ*inv120* \
         (2.*y1*z1 +    y2*z1 +    y3*z1 +    y4*z1 + \
             y1*z2 + 2.*y2*z2 +    y3*z2 +    y4*z2 + \
             y1*z3 +    y2*z3 + 2.*y3*z3 +    y4*z3 + \
             y1*z4 +    y2*z4 +    y3*z4 + 2.*y4*z4)

   I0[2, 1] = I0[1, 2]

   # xz
   I0[0, 2] = -detJ*inv120* \
         (2.*x1*z1 +    x2*z1 +    x3*z1 +    x4*z1 + \
             x1*z2 + 2.*x2*z2 +    x3*z2 +    x4*z2 + \
             x1*z3 +    x2*z3 + 2.*x3*z3 +    x4*z3 + \
             x1*z4 +    x2*z4 +    x3*z4 + 2.*x4*z4)

   I0[2, 0] = I0[0, 2]

   # xy
   I0[0, 1] = -detJ*inv120* \
         (2.*x1*y1 +    x2*y1 +    x3*y1 +    x4*y1 + \
             x1*y2 + 2.*x2*y2 +    x3*y2 +    x4*y2 + \
             x1*y3 +    x2*y3 + 2.*x3*y3 +    x4*y3 + \
             x1*y4 +    x2*y4 +    x3*y4 + 2.*x4*y4)

   I0[1, 0] = I0[0, 1]

   return I0
