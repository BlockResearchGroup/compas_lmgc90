import math
import numpy

from .contactor import contactor

from ...utilities.error import showError, showWarning

class rigidContactor2D(contactor):
   """class rigidContactor2D(contactor):
      this class is the base class used to define 2D rigid contactors.
      N.B.: this is an abstract class, an object of this class cannot be instanciated!
      attributes:
         - area: area of the contactor
         - I: inertia (scalar) of the contactor
         - shift: vector from the mass center of the body, to the mass center of the contactor
   """

   def __init__(self, elements, shape, color, area=None, I=None, shift=None):
      """__init__(self, elements, shape, color, area=None, I=None, shift=None):
         allow to define a rigid contactor.
         parameters:
            - self: the rigid contatctor itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - byrd: radius of the disk
         optional parameters:
            - area: area of the contactor
            - I: inertia (scalar) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
      """
      # on appelle le constructeur de la classe generique, pour stocker le type, les elements support (forcement un element point)
      # ici) et la couleur du contacteur
      contactor.__init__(self, elements, shape, color)
        
      # si l'utilisateur a donne un shift
      if shift is not None:
         try:
            # on s'assure que c'est bien un tableau numpy
            shift = numpy.array(shift, 'd')
            # si sa taille est impossible
            if shift.shape != (2,):
               # on affiche un message d'erreur
               showError("shift size is incompatible with a 2D contactor")
         # si la conversion echoue
         except Exception:
            # on affiche un message d'erreur
            showError("a shift must be a numpy array or list of real numbers") 
         # on stocke le shift
         self.shift = shift
      # sinon, on l'initialise a (0, 0)
      else:
         self.shift = numpy.zeros(2, 'd')

      # si l'utilisateur a donne une aire pre-calculee
      if area is not None:
         # on essaye de la convertir en reel
         try:
            area = float(area)
         # si on echoue
         except:
            # on affiche un message d'erreur
            showError("an area must be a float!")
         # on stocke l'aire
         self.area = area
      # sinon, on l'initialise a 0
      else:
         self.area = 0. 

      # si l'utilisateur a donne une inertie pre-calculee
      if I is not None:
         # on essaye de la convertir en reel
         try:
            I = float(I)
         # si on echoue
         except:
            # on affiche un message d'erreur
            showError("inertia is a float in 2D!")
         # on stocke l'inertie
         self.I = I
      # sinon, on l'initialise a 0
      else:
         self.I = 0. 

   def getRigidProperties(self):
      """getRigidProperties(self):
         this function returns the rigid proerties of a contactor, i.e. its area, inertia and shift
         parameters:
            - self: the contactor itself
      """
      return self.area, self.I, self.shift

   def extrusion(self, mass_center, depth, factor=1.e0, **extra_options):
       """extrusion(self):
          this function computes and returns the type and options (a dictionnary) of necessary to buld a 3D extrusion of
          the rigid contactor:
          parameters:
             - self: the contactor itself
             - mass_center: coordinates of the mass_center of the body attached to the contactor
             - depth: depth of the extrusion
          optional paramters:
             - factor: dilatation factor of homothety
             - extra_options: some extra options, used by particular contactors 
          returned value: the tuple (type, options)
       """
       # N.B.: il s'agit d'une fonction virtuelle pure, qui n'a donc pas d'implementation generique.
       raise NotImplementedError

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the contactor itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # N.B.: il s'agit d'une fonction virtuelle pure, qui n'a donc pas d'implementation generique.
      raise NotImplementedError


# les disques classiques
class diskx(rigidContactor2D):
   """class diskx(rigidContactor2D):
      this class defines the disks.
      static attribute:
         - shape='DISKx'
      attributes:
         - byrd: radius
   """
   # type : DISKx
   shape='DISKx'

   def __init__(self, elements, color, byrd, area=None, I=None, shift=None, **options):
      """__init__:
         allow to define a rigid disk.
         parameters:
            - self: the disk itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - byrd: radius of the disk
         optional parameters:
            - area: area of the contactor
            - I: inertia (scalar) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on appelle le constructeur generique de constructeur de contacteur rigide, 2D
      rigidContactor2D.__init__(self, elements, self.shape, color, area=area, I=I, shift=shift)

      # on tente de convertir le rayon en reel
      try:
          byrd=float(byrd)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("un rayon doit etre un reel!")

      # on stocke le rayon du disque
      self.byrd = byrd

      # on calcule la surface et l'inertie du disque, s'ils n'ont pas ete donnes par l'utilisateur
      if self.area == 0. or self.I == 0.:
         # surface du disque
         self.area = math.pi*self.byrd*self.byrd
         # inertie du disque
         self.I = 0.5*self.area*self.byrd*self.byrd

   def extrusion(self, mass_center, depth, factor=1.e0, extrudedDisk='Sphere', **extra_options):
      """extrusion(self):
         this function computes and returns the type and options (a dictionnary) of necessary to buld a 3D extrusion of
         the rigid contactor:
         parameters:
            - self: the disk itself
            - mass_center: coordinates of the mass_center of the body attached to the contactor
            - depth: depth of the extrusion
         optional paramters:
            - factor: dilatation factor of homothety
            - extrudedDisk: a string used to define how to extrude the disk:
                 * 'Sphere': for a sphere (SPHER)
                 * 'Cylinder': for a solid cylinder (CYLND)
              N.B.: this option is considered only if the disk is only contactor of the body (i.e. shift=[0., 0.])
            - extra_options: some extra options, used by other contactors 
         returned value: the tuple (type, options)
      """
      # si le disque est le seul contacteur du corps (i.e. n'est pas excentre)
      if numpy.allclose(self.shift, numpy.zeros(2, 'd'), atol=1e-6):
         # extrusion en
         #    - sphere ou cylindre plein
         if extrudedDisk == 'Sphere':
            # extrusion en sphere, de meme rayon que le disque
            shape='SPHER'
            options={'byrd' : factor*self.byrd}
         #    - cylindre plein
         elif extrudedDisk == 'Cylinder':
            # extrusion en cylindre plein, de meme rayon que le
            # disque et de hauteur egale a la profondeur de l'extrusion
            shape='CYLND'
            options={'High' : 0.5*depth, 'byrd' : factor*self.byrd}
         #     - cas par defaut : levee d'une exception
         else:
            raise ValueError("In disk::extrusion: a disk CANNOT be extruded as a " + extrudedDisk + "! Skipping.")
      # si le disque fait partie d'un cluster ((i.e. est excentre)
      else:
         # extrusion en sphere, de meme rayon que le disque
         # le vecteur reliant le centre du disque au centre du corps 2D
         # est passe directement en 3D : il reste dans un plan // a xOy
         shape='SPHER'
         options={'byrd'  : factor*self.byrd,
                  'shift' : [factor*(mass_center[0] + self.shift[0]), factor*(mass_center[1] + self.shift[1]), 0.5*depth]}

      return shape, options

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the disk itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # definition du surnom du contacteur

      tact_type = self.shape
      # si le contacteur porte un shift non nul
      if not numpy.allclose(self.shift, numpy.zeros(2, 'd'), atol=1e-6):
         # alors, on remplace le dernier caratctere par un 'b', pour balourd
         tact_type = self.shape[:4] + 'b'

      # on peut alors ecrire la premiere ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  byrd=%14.7E\n' % (tact_type, number, self.color, self.byrd)

      # si le contacteur porte un shift non nul
      if tact_type[4] == 'b':
         # on ajoute la ligne donnant le shift      
         line += '                             '
         line += 'coo1=%14.7E  coo2=%14.7E\n' % (self.shift[0], self.shift[1])
 
      # on renvoie le texte decrivant le contacteur
      return line

 
class xksid(rigidContactor2D):
   """class xksid(rigidContactor2D):
      this class defines the hollow disks.
      static attribute:
         - shape='xKSID'
      attributes:
         - byrd: radius
   """
   # type : xKSID
   shape='xKSID'

   def __init__(self, elements, color, byrd, area=None, I=None, shift=None, **options):
      """__init__:
         allow to define an hollow rigid disk.
         parameters:
            - self: the disk itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - byrd: radius of the disk
         optional parameters:
            - area: area of the contactor
            - I: inertia (scalar) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on appelle le constructeur generique de constructeur de contacteur rigide, 2D
      rigidContactor2D.__init__(self, elements, self.shape, color, area=area, I=I, shift=shift)

      # on tente de convertir le rayon en reel
      try:
          byrd=float(byrd)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("un rayon doit etre un reel!")

      # on stocke le rayon du disque
      self.byrd = byrd

      # si l'utilisateur n'a pas donne la surface et l'inertie du disque, on conserve les
      # valeurs par defaut (i.e. surface et inertie nulles), car ce contacteur est utilise comme
      # condition limite
      # TODO: on pourrait ajouter la notion d'epaisseur d'un disque creux pour calculer une surface et une inerie...

      # si l'utilisateur a donne un shift non nul, on l'informe qu'il ne sera pas utilise
      if not numpy.allclose(self.shift, numpy.zeros(2, 'd'), atol=1e-6): 
         showWarning("shifts are not handled by hollow disks.")

   def extrusion(self, mass_center, depth, factor=1.e0, **extra_options):
      """extrusion(self):
         this function computes and returns the type and options (a dictionnary) of necessary to buld a 3D extrusion of
         the rigid contactor:
         parameters:
            - self: the hoolow disk itself
            - mass_center: coordinates of the mass_center of the body attached to the contactor
            - depth: depth of the extrusion
         optional paramters:
            - factor: dilatation factor of homothety
            - extra_options: some extra options, used by particular contactors 
         returned value: the tuple (type, options)
      """
      # extrusion en cylindre creux, de meme rayon que le
      # disque et de hauteur egale a la profondeur de l'extrusion
      shape='DNLYC'
      options={'High' : 0.5*depth, 'byrd' : factor*self.byrd}

      return shape, options

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the disk itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # on ecrit la seule ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  byrd=%14.7E\n' % (self.shape, number, self.color, self.byrd)

      # on renvoie le texte decrivant le contacteur
      return line



# les joncs
# TODO: filer les dimensions du jonc dans un vecteur a deux composantes, plutot que deux variables
class joncx(rigidContactor2D):
   """class joncx(rigidContactor2D):
      this class defines the "joncs", i.e. contactors used to build 2D boxes.
      static attribute:
         - shape='JONCx'
      attributes:
         - axes: dimensions of the contactor:
              * axes[0]: half the length of the contactor, along the first axis (of the local frame) 
              * axes[1]: half the length of the contactor, along the second axis (of the local frame)
   """
   # type : JONCx
   shape='JONCx'

   def __init__(self, elements, color, axe1, axe2, area=None, I=None, shift=None, **options):
      """__init__:
         allow to define a "jonc".
         parameters:
            - self: the "jonc" itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - axe1: half the length of the contactor, along the first axis (of the local frame) 
            - axe2: half the length of the contactor, along the second axis (of the local frame)
         optional parameters:
            - area: area of the contactor
            - I: inertia (scalar) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on appelle le constructeur generique de constructeur de contacteur rigide, 2D
      rigidContactor2D.__init__(self, elements, self.shape, color, area=area, I=I, shift=shift)

      # on tente de convertir les dimensions du jonc en reel
      #   - axe1:
      try:
          axe1=float(axe1)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("axe1 doit etre un reel!")
      #   - axe2:
      try:
          axe2=float(axe2)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("axe2 doit etre un reel!")

      # on stocke les dimensions du jonc
      self.axes = (axe1, axe2)

      # on calcule la surface et l'inertie du jonc, s'ils n'ont pas ete donnes par l'utilisateur
      if self.area == 0. or self.I == 0.:
         # surface du jonc
         self.area = (4.*self.axes[0]*self.axes[1]) + (math.pi*self.axes[1]*self.axes[1])
         # inertie du jonc
         self.I = (math.pi*self.axes[1]*self.axes[1]*( (self.axes[0]*self.axes[0]) + (0.5*self.axes[1]*self.axes[1]) ) ) + \
                  (2.*self.axes[0]*self.axes[1]*( (self.axes[0]*self.axes[0]) + (self.axes[1]*self.axes[1]) ) )

      # si l'utilisateur a donne un shift non nul, on l'informe qu'il ne sera pas utilise
      if not numpy.allclose(self.shift, numpy.zeros(2, 'd'), atol=1e-6): 
         showWarning("les joncs ne gerent pas les shifts.")

   def extrusion(self, mass_center, depth, factor=1.e0, **extra_options):
       """extrusion(self):
          this function computes and returns the type and options (a dictionnary) of necessary to buld a 3D extrusion of
          the rigid contactor:
          parameters:
             - self: the "jonc" itself
             - mass_center: coordinates of the mass_center of the body attached to the contactor
             - depth: depth of the extrusion
          optional paramters:
             - factor: dilatation factor of homothety
             - extra_options: some extra options, used by particular contactors 
          returned value: the tuple (type, options)
       """
       # extrusion en plan, d'epaisseur egale a la profondeur de l'extrusion
       shape='PLANx'
       options={'axe1' : factor*self.axes[0], 'axe2' : 0.5*depth, 'axe3' : factor*self.axes[1]}

       return shape, options

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the "jonc" itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # on ecrit la seule ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  axe1=%14.7E  axe2=%14.7E\n' % (self.shape, number, self.color, \
                                                                    self.axes[0], self.axes[1])

      # on renvoie le texte decrivant le contacteur
      return line


# les polygones
# TODO: arreter de filer le nombre de sommets du polygone et passer directement un maillage a base de segments.
class polyg(rigidContactor2D):
   """class polyg(rigidContactor2D):
      this class defines the polygons.
      static attribute:
         - shape='POLYG'
      attributes:
         - nb_vertices: number of vertices
         - vertices: verices of the polygon, given in the trigonometric order
   """
   # type : POLYG
   shape='POLYG'

   def __init__(self, elements, color, nb_vertices, vertices, area=None, I=None, shift=None, **options):
      """__init__:
         allow to define a polygon.
         parameters:
            - self: the polygon itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - nb_vertices: number of vertices
            - vertices: vertices of the polygon, given in the trigonometric order
         optional parameters:
            - area: area of the contactor
            - I: inertia (scalar) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on appelle le constructeur generique de constructeur de contacteur rigide, 2D
      rigidContactor2D.__init__(self, elements, self.shape, color, area=area, I=I, shift=shift)

      # on tente de convertir le nombre de sommets en entier
      try:
          nb_vertices = int( nb_vertices )
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError( "The number of vertices of a polygon must be an integer !")

      # on stocke le nombre de sommets du polygone
      self.nb_vertices = nb_vertices

      try:
          # on tente de convertir le tableau des coordonnees des sommets du polygone en tableau numpy
          vertices = numpy.array( vertices, 'd' )
          # si la forme du tableau stockant les coordonnees des sommets n'est pas
          # celle attendue
          if vertices.shape != ( self.nb_vertices, 2 ) :
             # on affiche un message d'erreur
             showError( "The coordinates of the vertices of a POLYG must be stored " +
                        "in a matrix of shape [\"number of vertices\" , 2]!" )
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError( "The coordinates of the vertices of a POLYG must be stored " +
                     "in a matrix of real numbers" )

      # on stocke les coordonnees des sommets du polygone
      self.vertices = vertices

      # on calcule la surface, l'inertie du polygone, s'ils n'ont pas ete donnes par l'utilisateur
      # N.B.: 1. la methode de calcul utilisee suppose que le corps soit convexe!!
      #       2. shift est aussi recalcule
      if self.area == 0. or self.I == 0. :
         # si l'utilisateur a donne un shift non nul, on l'informe qu'il ne sera pas utilise
         if not numpy.allclose( self.shift, numpy.zeros( 2, 'd' ), atol = 1e-6 ) : 
            showWarning( "skipping given shift, since it is automatically computed for polygons" )

         # calcul du shift entre le centre d'inertie du contacteur polygone et le
         # centre d'inertie du corps, OG

         #fd on a 2 solutions soit:
         #fd 1/les coordonnees des vertex sont donnees par rapport au centre d'inertie
         #fd   au quel cas le OG calcule est (0,0)
         #fd 2/les coordonnees des vertex sont donnees dans le repere absolue auquel cas 
         #fd   le OG calcule est different de (0,0)

         # formule de calcul de l'aire, du centre de gravite et de l'inertie donnees par
         # http://paulbourke.net/geometry/polygonmesh/

         area = numpy.zeros( self.nb_vertices,dtype=numpy.float64)
         for i in range( self.nb_vertices ) :
            area[i] = self.vertices[i-1, 0] * self.vertices[i  , 1] - \
                      self.vertices[i  , 0] * self.vertices[i-1, 1]

         self.area = 0.5 * numpy.sum( area )

         if self.area <= 0.:
            # il y a un probleme dans la numerotation des sommets du polygone
            showError( "bad orientation of polygon" )

         # calcul du centre de gravite du polygone OG
         self.shift = numpy.zeros(2)
         for i in range( self.nb_vertices ) :
            self.shift += ( self.vertices[i-1,:] + self.vertices[i,:] ) * area[ i ]

         self.shift /= 6. * self.area

         # on exprime les coordonnees des sommets dans le referentiel barycentrique
         self.vertices -= self.shift


         # le pre-calcul de area semble provoquer des erreurs d'arrondi qui rendent la methode non robuste.
         # surtout pour les objets de petite taille
         
         # calcul de l'inertie du polygone par rapport au centre de gravite
         # formule comprehensible
         # Ix = 0.
         # Iy = 0.
         # for i in range(self.nb_vertices):
         #    Ix += (self.vertices[i-1,1]**2+self.vertices[i-1,1]*self.vertices[i,1]+self.vertices[i,1]**2) * \
         #          (self.vertices[i-1, 0] * self.vertices[i  , 1] - self.vertices[i  , 0] * self.vertices[i-1, 1])
         #    #area[i]

         #    Iy += (self.vertices[i-1,0]**2+self.vertices[i-1,0]*self.vertices[i,0]+self.vertices[i,0]**2) * \
         #          (self.vertices[i-1, 0] * self.vertices[i  , 1] - self.vertices[i  , 0] * self.vertices[i-1, 1])
         #    #area[i]

         # self.I = (Ix+Iy)/12.

         # formule plus 'pythonique' -- 
         
         vert2  = self.vertices * self.vertices
         self.I = 0.
         for i in range( self.nb_vertices ) :
            self.I += numpy.sum( (self.vertices[i-1,:] * self.vertices[i,:]) + vert2[i-1,:] + vert2[i,:] ) * \
                      (self.vertices[i-1, 0] * self.vertices[i  , 1] - self.vertices[i  , 0] * self.vertices[i-1, 1])
            #area[i]

         self.I /= 12.
         
   def extrusion(self, mass_center, depth, factor=1.e0, **extra_options):
      """extrusion(self):
         this function computes and returns the type and options (a dictionnary) of necessary to buld a 3D extrusion of
         the rigid contactor:
         parameters:
            - self: the polygon itself
            - mass_center: coordinates of the mass_center of the body attached to the contactor
            - depth: depth of the extrusion
         optional paramters:
            - factor: dilatation factor of homothety
            - extra_options: some extra options, used by particular contactors 
         returned value: the tuple (type, options)
      """
      # on calcule le nombre de sommets du contacteur
      nb_vertices=2*self.nb_vertices

      # on calcule le nombre de face du contacteur 
      nb_faces=4*self.nb_vertices - 4

      # definition des sommets :

      # on cree une matrice de double a la bonne taille
      vertices = numpy.zeros([nb_vertices, 3], 'd')

      # on donne la liste des sommets dans le plan z=0
      # N.B. : * la composante x en 2D reste la composante x en 3D
      #        * la composante y en 2D reste la composante y en 3D
      for i in range(0, self.nb_vertices):
         vertices[i, 0]=factor*(mass_center[0] + self.vertices[i, 0] + self.shift[0])
         vertices[i, 1]=factor*(mass_center[1] + self.vertices[i, 1] + self.shift[1])
         vertices[i, 2]=0.
      # on donne la liste des sommets dans le plan z=depth
      for i in range(0, self.nb_vertices):
         j=self.nb_vertices + i
         vertices[j, 0]=factor*(mass_center[0] + self.vertices[i, 0] + self.shift[0])
         vertices[j, 1]=factor*(mass_center[1] + self.vertices[i, 1] + self.shift[1])
         vertices[j, 2]=depth
      
      # definition des faces :
  
      # on cree une matrice d'entiers a la bonne taille
      connectivity = numpy.zeros([nb_faces, 3], 'i')
      
      # on donne la liste des faces
      j = 0
      # liste des faces droites reliant les polygones 2D plonges dans le plan
      # y=0 et y=depth, respectivement
      for i in range(1, self.nb_vertices):
         connectivity[j, 0]=i
         connectivity[j, 1]=self.nb_vertices + i
         connectivity[j, 2]=self.nb_vertices + i + 1
         j += 1
         connectivity[j, 0]=i
         connectivity[j, 1]=self.nb_vertices + i + 1
         connectivity[j, 2]=i + 1
         j += 1
      connectivity[j, 0]=self.nb_vertices
      connectivity[j, 1]=2*self.nb_vertices
      connectivity[j, 2]=1
      j += 1
      connectivity[j, 0]=2*self.nb_vertices
      connectivity[j, 1]=1
      connectivity[j, 2]=self.nb_vertices + 1
      j += 1
      # liste des faces partitionnant le polygone plonge dans le plan y=0
      for i in range(2, self.nb_vertices):
         connectivity[j, 0]=1
         connectivity[j, 1]=i
         connectivity[j, 2]=i + 1
         j += 1
      # liste des faces partitionnant le polygone plonge dans le plan y=depth
      for i in range(self.nb_vertices + 2, 2*self.nb_vertices):
         connectivity[j, 0]=self.nb_vertices + 1
         connectivity[j, 1]=i
         connectivity[j, 2]=i + 1
         j += 1
 
      # on renvoie les donnees necessaires pour contruire le polyhedre
      shape='POLYR'
      options={'nb_vertices' : nb_vertices, 'nb_faces' : nb_faces, 'connectivity' : connectivity, 'vertices' : vertices}

      return shape, options

   def strInBodiesFile(self, number,frame=None):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the polygon itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # on ecrit la premiere ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  nb_vertex=  %5d\n' % (self.shape, number, self.color, self.nb_vertices)

      # on calcule les positions des sommets dans le repere global

      # on les initialise aux positions dans le repere local
      # i.e. on construit une copie des positions dans le repere local
      vertices_global = numpy.array(self.vertices, 'd')

      if frame is not None:
        v_aux = numpy.zeros(vertices_global.shape,'d')
        v_aux[:,0:2] = numpy.dot(vertices_global[:,0:2],frame.transpose())
        vertices_global=v_aux[::-1,:]
        
      # si le contacteur porte un shift non nul
      if not numpy.allclose(self.shift, numpy.zeros(2, 'd'), atol=1e-6):
         # on l'applique aux coordonnees des sommets du polygone
         for i in range(0, self.nb_vertices):
            vertices_global[i, :] += self.shift
      # on ajoute les lignes donnant les coordonnees des sommets du polygone
      for i in range(self.nb_vertices):
           line += '                             '
           line += 'coo1=%14.7E  coo2=%14.7E\n' % (vertices_global[i, 0], vertices_global[i, 1])

      # on renvoie le texte decrivant le contacteur
      return line


# les points
class pt2dx(rigidContactor2D):
   """class pt2dx(rigidContactor2D):
      this class defines the points on rigid bodies.
      static attribute:
         - shape='PT2Dx'
   """
   # type : PTD2x
   shape='PT2Dx'

   def __init__(self, elements, color, area=None, I=None, shift=None, **options):
      """__init__:
         allow to define a point on a rigid body.
         parameters:
            - self: the point itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
         optional parameters:
            - area: area of the contactor
            - I: inertia (scalar) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on appelle le constructeur generique de constructeur de contacteur rigide, 2D
      rigidContactor2D.__init__(self, elements, self.shape, color, area=area, I=I, shift=shift)

      # si l'utilisateur n'a pas donne la surface et l'inertie du disque, on conserve les
      # valeurs par defaut (i.e. surface et inertie nulles), car un point n'a pas de surface!

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the point itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # on ecrit la seule ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  coo1=%14.7E  coo2=%14.7E\n' % (self.shape, number, self.color, \
                                                                    self.shift[0], self.shift[1])

      # on renvoie le texte decrivant le contacteur
      return line

   # N.B.: la methode d'extrusion d'un point n'est pas redefinie!


if __name__=='__main__':
   # test a la con...
   from avatar.bulk.rigid2d import *

   b = rigid2d()
   #d = diskx([b], 'BLUEx', 1.)
   loc=locals()
   my_diskx=loc['diskx']
   d = my_diskx([b], 'BLUEx', 1.)
   print(d.getRigidProperties())
   print(d.strInBodiesFile(1))

   d2 = diskx([b], 'BLUEx', shift=[0., 1.], byrd=1.)
   print(d2.getRigidProperties())
   print(d2.strInBodiesFile(2))

   k = xksid([b], 'BLUEx', 1.)
   print(k.getRigidProperties())
   print(k.strInBodiesFile(1))

   j = joncx([b], 'WALLx', 1., 0.01)
   print(j.getRigidProperties())
   print(j.strInBodiesFile(1))

   # test pour le polygone : le premier bloc du test "arche_plein_ceintre_2D"
   r_int=0.8
   r_ext=1.
   theta_joint=math.pi/100.
   nb_blocs=11 
   theta_bloc=(math.pi - (nb_blocs - 1)*theta_joint)/nb_blocs
   theta=0.

   vertices=numpy.zeros([4, 2], 'd')
   vertices[0, 0]=r_int*math.cos(theta);              vertices[0, 1]=r_int*math.sin(theta)
   vertices[1, 0]=r_ext*math.cos(theta);              vertices[1, 1]=r_ext*math.sin(theta)
   vertices[2, 0]=r_ext*math.cos(theta + theta_bloc); vertices[2, 1]=r_ext*math.sin(theta + theta_bloc)
   vertices[3, 0]=r_int*math.cos(theta + theta_bloc); vertices[3, 1]=r_int*math.sin(theta + theta_bloc)

   p = polyg([b], 'REDxx', 4, vertices)
   print(p.getRigidProperties())
   print(p.strInBodiesFile(1))
   # N.B.: le test d'affichage des coordonnees des sommets ne donne pas le meme resultat que ce qui est ecrit dans le
   #       fichier BODIES.DAT, car on n'a pas fait le computeRigidProperties...

   pt = pt2dx([b], 'VERTx', shift=[1., 0.])
   print(pt.getRigidProperties())
   print(pt.strInBodiesFile(1))
