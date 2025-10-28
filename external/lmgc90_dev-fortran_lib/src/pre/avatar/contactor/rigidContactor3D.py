import math
import numpy
from copy import deepcopy

from .contactor import contactor
from .rigid_properties_3D import compute_jacobian, compute_inertia_tetrahedron

from ...utilities.error   import *
from ...build_avatar.mesh import mesh

class rigidContactor3D(contactor):
   """class rigidContactor3D(contactor):
      this class is the base class used to define rigid contactors.
      N.B.: this is an abstract class, an object of this class cannot be instanciated!
      attributes:
         - volume: volume of the contactor
         - I: inertia matrix (3x3 matrix) of the contactor
         - shift: vector from the mass center of the body, to the mass center of the contactor
         - frame: if the contactor is turned relatively to the local frame of the body
   """

   def __init__(self, elements, shape, color, volume=None, I=None, shift=None, frame=None):
      """__init__(self, elements, shape, color, volume=None, I=None, shift=None, frame=None):
         allow to define a rigid contactor.
         parameters:
            - self: the contactor itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
         optional parameters:
            - volume: volume of the contactor
            - I: inertia matrix (3x3 matrix) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - frame: if the contactor is  turned relatively to the local frame of the body
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
            if shift.shape != (3,):
               # on affiche un message d'erreur
               showError("shift size is incompatible with a 3D contactor")
         # si la conversion echoue
         except Exception:
            # on affiche un message d'erreur
            showError("a shift must be a numpy array or list of real numbers")
         # on stocke le shift
         self.shift = shift
      # sinon, on l'initialise a (0, 0, 0)
      else:
         self.shift = numpy.zeros(3, 'd')

      # si l'utilisateur a donne un frame
      if frame is not None:
         try:
            # on s'assure que c'est bien un tabelau numpy
            frame = numpy.array(frame, 'd')
            # si sa taille est impossible
            if frame.shape != (3, 3):
               # on affiche un message d'erreur
               showError("frame size is incompatible with a 3D contactor")
         # si la conversion echoue
         except Exception:
            # on affiche un message d'erreur
            showError("a shift must be a numpy array or list of float")   
         # on stocke le le repere
         self.frame = frame
      # sinon, on le repere du contacteur est confondu avec le repere global
      else:
         self.frame = numpy.eye(3) 

      # si l'utilisateur a donne un volume pre-calcule
      if volume is not None:
         # on essaye de le convertir en reel
         try:
            volume = float(volume)
         # si on echoue
         except:
            # on affiche un message d'erreur
            showError("a volume must be a float!")
         # on stocke le volume
         self.volume = volume
      # sinon, on l'initialise a 0 
      else:
         self.volume = 0.

      # si l'utilisateur a donne une inertie pre-calculee
      if I is not None:
         try:
            # on s'assure que c'est bien un tabelau numpy
            I = numpy.array(I, 'd')
            # si sa taille n'est pas bonne
            if I.shape != (3, 3):
               # on affiche un message d'erreur
               showError("an inertia matrix is a a 3x3 matrix in 3D!")
         # si la conversion echoue
         except Exception:
            # on affiche un message d'erreur
            showError("an inertia must be a numpy array")
         # on stocke l'inertie
         self.I = I
       # sinon, on initialise l'inertie a la matrice nulle
      else:
         self.I = numpy.zeros((3, 3), 'd') 

   def getRigidProperties(self):
      """getRigidProperties(self):
         this function returns the rigid proerties of a contactor, i.e. its volume, inertia and shift
         parameters:
            - self: the contactor itself
      """
      return self.volume, self.I, self.shift, self.frame

   def updateFrame(self, P):
      """updateFrame(self, P):
         this functions change contactors properties due to frame change: from the global frame to the inertia frame
         parameters:
            - self: the contactor itself
            - P: inertia frame store as a 3 x 3 matrix, expressing the inertia frame basis (a, b, c) in the global frame (x, y, z)
                 P = (a, b, c)
      """ 
      # par defaut, le contacteur n'est pas affecte et cette methode ne fait rien
      pass

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


# les spheres
class spher(rigidContactor3D):
   """class spher(rigidContactor3D):
      this class defines the spheres.
      static attribute:
         - shape='SPHER'
      attributes:
         - byrd: radius of the sphere
   """
   # type : SPHER
   shape='SPHER'

   def __init__(self, elements, color, byrd, volume=None, I=None, shift=None, frame=None, **options):
      """__init__:
         allow to define a rigid sphere.
         parameters:
            - self: the sphere itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - byrd: radius of the sphere
         optional parameters:
            - volume: volume of the contactor
            - I: inertia matrix (3x3 matrix) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - frame: if the contactor is turned relatively to the local frame of the body
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on appelle le constructeur generique de constructeur de contacteur rigide, 3D
      rigidContactor3D.__init__(self, elements, self.shape, color, volume=volume, I=I, shift=shift, frame=frame)

      # on tente de convertir le rayon en reel
      try:
          byrd=float(byrd)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("radius must be a real!")

      # on stocke le rayon de la sphere
      self.byrd = byrd

      # on calcule le volume et l'inertie de la sphere, s'ils n'ont pas ete donnes par l'utilisateur
      if self.volume == 0. or numpy.allclose(self.I, numpy.zeros((3, 3), 'd'), atol=1e-6):
         # volume de la sphere
         self.volume = 4.*math.pi*self.byrd*self.byrd*self.byrd/3.

         # inertie de la sphere

         # on calcule de la matrice d'inertie dans le repere global, en
         # supposant que le repere principal d'inertie de la sphere est confondu
         # avec le repere global
 
         # on calcule le moment par rapport a un axe
         # N.B.: la sphere est isotrope => valeur identique quelque soit l'axe
         I11 = 0.4*self.volume*self.byrd*self.byrd
         # on en deduit la matrice d'inertie, dans le repere global
         self.I = numpy.array([[I11, 0. , 0. ],
                              [0. , I11, 0. ],
                              [0. , 0. , I11]])

      # si l'utilisateur a donne un frame qui n'est pas confondu avec le repere global, on l'informe qu'il ne sera pas utilise
      if not numpy.allclose(self.frame, numpy.eye(3), atol=1e-6): 
         showWarning("frames are useless for the spheres.")

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the sphere itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # definition du surnom du contacteur

      tact_type = self.shape
      # si le contacteur porte un shift non nul
      if not numpy.allclose(self.shift, numpy.zeros(3, 'd'), atol=1e-6):
         # alors, on remplace le dernier caratctere par un 'b', pour balourd
         tact_type = self.shape[:4] + 'b'

      # on peut alors ecrire la premiere ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  byrd=%14.7E\n' % (tact_type, number, self.color, self.byrd)

      # si le contacteur porte un shift non nul
      if tact_type[4] == 'b':
         # on ajoute la ligne donnant le shift      
         line += '                             '
         line += 'coo1=%14.7E  coo2=%14.7E  coo3=%14.7E\n' % (self.shift[0], self.shift[1], self.shift[2])
 
      # on renvoie le texte decrivant le contacteur
      return line


# les cylindres
class cylnd(rigidContactor3D):
   """class cylnd(rigidContactor3D):
      this class defines the cylinders.
      static attribute:
         - type='CYNLD'
      attributes:
         - High: half the high of the cylinder
         - byrd: radius od the cylinder
   """
   # type : CYLND
   shape='CYLND'

   def __init__(self, elements, color, High, byrd, volume=None, I=None, shift=None, frame=None, **options):
      """__init__:
         allow to define a rigid cylinder.
         parameters:
            - self: the cylinder itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - High: half the high of the cylinder
            - byrd: radius of the cylinder
         optional parameters:
            - volume: volume of the contactor
            - I: inertia matrix (3x3 matrix) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - frame: if the contactor is turned relatively to the local frame of the body
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on appelle le constructeur generique de constructeur de contacteur rigide, 3D
      rigidContactor3D.__init__(self, elements, self.shape, color, volume=volume, I=I, shift=shift, frame=frame)

      # on tente de convertir le rayon en reel
      try:
          byrd=float(byrd)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("radius must be a real!")

      # on stocke le rayon du cylindre
      self.byrd = byrd

      # on tente de convertir la demi-hauteur en reel
      try:
          High=float(High)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("High must be a real!")

      # on stocke la demi-hauteur du cylindre
      self.High = High

      # on calcule le volume et l'inertie du cylindre, s'ils n'ont pas ete donnes par l'utilisateur
      if self.volume == 0. or numpy.allclose(self.I, numpy.zeros((3, 3), 'd'), atol=1e-6):
         # inertie geometrique du cylindre rayon r hauteur h
         # vol = pi * r**2 * h
         # I1,I2 plan        I1=I2= vol *( r**2 / 4 + h**2 / 12)  
         # I3 axe verticale  I3 = vol * r**2 * 0.5  

         # on calcule le carre du rayon du cylindre
         r2 = self.byrd*self.byrd

         # on en deduit le volume du cylindre
         volume_cylindre = math.pi*r2*(2.*self.High)
         volume_sphere = 4/3*math.pi*r2*self.byrd
         self.volume = volume_cylindre + volume_sphere
         # et le moment d'inertie par rapport a chaque axe
         I11 = ((0.25*r2) + (self.High*self.High/3.))*volume_cylindre +(0.4*r2+(self.High+0.375*self.byrd)**2)*volume_sphere
         I22 = I11
         I33 = 0.5*r2*volume_cylindre+0.4*r2*volume_sphere

         IG0 = numpy.array([[I11, 0. , 0. ],
                            [0. , I22, 0. ],
                            [0. , 0. , I33]])
         if frame is None:
            IG = IG0
         else:
            IG = numpy.matmul(numpy.transpose(frame),numpy.matmul(IG0,frame))
            
         # on en deduit la matrice d'inertie, dans le repere global
         self.I = IG

      # si l'utilisateur a donne un shift non nul, on l'informe qu'il ne sera pas utilise
      #if not numpy.allclose(self.shift, numpy.zeros(3, 'd'), atol=1e-6): 
      #   showWarning("shifts are not handled by cylinders.")

      # si l'utilisateur a donne un frame qui n'est pas confondu avec le repere global, on l'informe qu'il ne sera pas utilise
      #if not numpy.allclose(self.frame, numpy.eye(3), atol=1e-6): 
      #   showWarning("frames are not handled by cylinders.")

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the cylinder itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # on ecrit la seule ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  High=%14.7E  byrd=%14.7E\n' % (self.shape, number, self.color, self.High, self.byrd)

      # si on a donne un shift non nul et/ou un repere oriente differement du repere global      
      if not numpy.allclose(self.shift, numpy.zeros(3, 'd'), atol=1e-6) or \
         not numpy.allclose(self.frame, numpy.eye(3), atol=1e-6): 
         # on ecrit la ligne indiquant qu'on va donne un repere
           line += '                             '
           line += 'localframe\n'
           # on ecrit le repere :
           # la matrice d'orientation, s'ecrit ecrit P=(alpha, beta, gamma), ou alpha, beta et gamma sont des vecteurs colonnes
           # on ecrit :
           #    * les composantes de alpha
           line += '                             '
           line += 'alp1=%14.7E  alp2=%14.7E  alp3=%14.7E\n' % (self.frame[0, 0], self.frame[1, 0], self.frame[2, 0])
           #    * les composantes de beta
           line += '                             '
           line += 'bet1=%14.7E  bet2=%14.7E  bet3=%14.7E\n' % (self.frame[0, 1], self.frame[1, 1], self.frame[2, 1])
           #    * les composantes de gamma
           line += '                             '
           line += 'gam1=%14.7E  gam2=%14.7E  gam3=%14.7E\n' % (self.frame[0, 2], self.frame[1, 2], self.frame[2, 2])
           # on ecrit le shift :
           line += '                             '
           line += 'coo1=%14.7E  coo2=%14.7E  coo3=%14.7E\n' % (self.shift[0], self.shift[1], self.shift[2])

      
      # on renvoie le texte decrivant le contacteur
      return line


class dnlyc(rigidContactor3D):
   """class dlnyc(rigidContactor3D):
      this class defines the hollow cylinders.
      static attribute:
         - shape='DLNYC'
      attributes:
         - High: half the high of the cylinder
         - byrd: radius od the cylinder
   """
   # type : DNLYC
   shape='DNLYC'

   def __init__(self, elements, color, High, byrd, volume=None, I=None, shift=None, frame=None, **options):
      """__init__:
         allow to define a rigid hollow cylinder.
         parameters:
            - self: the cylinder itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - High: half the high of the cylinder
            - byrd: radius of the cylinder
         optional parameters:
            - volume: volume of the contactor
            - I: inertia matrix (3x3 matrix) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - frame: if the contactor is turned relatively to the local frame of the body
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on appelle le constructeur generique de constructeur de contacteur rigide, 3D
      rigidContactor3D.__init__(self, elements, self.shape, color, volume=volume, I=I, shift=shift, frame=frame)

      # on tente de convertir le rayon en reel
      try:
          byrd=float(byrd)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("radius must be a real!")

      # on stocke le rayon du cylindre
      self.byrd = byrd

      # on tente de convertir la demi-hauteur en reel
      try:
          High=float(High)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("High must be a real!")

      # on stocke la demi-hauteur du cylindre
      self.High = High

      # on calcule le volume et l'inertie du cylindre, s'ils n'ont pas ete donnes par l'utilisateur
      if self.volume == 0. or numpy.allclose(self.I, numpy.zeros((3, 3), 'd'), atol=1e-6):
         # am : ATTENTION, on calcule le volume et l'inertie d'un cylindre plein!
         #      Il faudra trouver une logique commune au 2D et au 3D... 
         # TODO: on pourrait ajouter la notion d'epaisseur d'un cylindre creux pour calculer un volume et une inerie...

         # inertie geometrique du cylindre rayon r hauteur h
         # vol = pi * r**2 * h
         # I1,I2 plan        I1=I2= vol *( r**2 / 4 + h**2 / 12)  
         # I3 axe verticale  I3 = vol * r**2 * 0.5  

         # on calcule le carre du rayon du cylindre
         r2 = self.byrd*self.byrd

         # on en deduit le volume du cylindre
         self.volume = math.pi*r2*(2.*self.High)
         # et le moment d'inertie par rapport a chaque axe
         I11 = ((0.25*r2) + (self.High*self.High/3.))*self.volume
         I22 = I11
         I33 = 0.5*r2*self.volume   
         # on en deduit la matrice d'inertie, dans le repere global
         self.I = numpy.array([[I11, 0. , 0. ],
                               [0. , I22, 0. ],
                               [0. , 0. , I33]])

      # si l'utilisateur a donne un shift non nul, on l'informe qu'il ne sera pas utilise
      if not numpy.allclose(self.shift, numpy.zeros(3, 'd'), atol=1e-6): 
         showWarning("shifts are not handled by hollow cylinders.")

      # si l'utilisateur a donne un frame qui n'est pas confondu avec le repere global, on l'informe qu'il ne sera pas utilise
      if not numpy.allclose(self.frame, numpy.eye(3), atol=1e-6): 
         showWarning("frames are not handled by hollow cylinders.")

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the cylinder itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # on ecrit la seule ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  High=%14.7E  byrd=%14.7E\n' % (self.shape, number, self.color, self.High, self.byrd)

      # on renvoie le texte decrivant le contacteur
      return line


# les plans
class planx(rigidContactor3D):
   """class planx(rigidContactor3D):
      this class defines the planes.
      static attribute:
         - shape='PLANx'
      attributes:
         - axes: dimensions of the contactor:
              * axes[0]: half the length of the contactor, along the first axis (of the local frame) 
              * axes[1]: half the length of the contactor, along the second axis (of the local frame)
              * axes[2]: half the length of the contactor, along the third axis (of the local frame)
   """
   # type : PLANx
   shape='PLANx'

   def __init__(self, elements, color, axe1, axe2, axe3, volume=None, I=None, shift=None, frame=None, **options):
      """__init__:
         allow to define a rigid plane.
         parameters:
            - self: the plane itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - axe1: half the length of the contactor, along the first axis (of the local frame) 
            - axe2: half the length of the contactor, along the second axis (of the local frame)
            - axe3: half the length of the contactor, along the third axis (of the local frame)
         optional parameters:
            - volume: volume of the contactor
            - I: inertia matrix (3x3 matrix) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - frame: if the contactor is turned relatively to the local frame of the body
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on appelle le constructeur generique de constructeur de contacteur rigide, 3D
      rigidContactor3D.__init__(self, elements, self.shape, color, volume=volume, I=I, shift=shift, frame=frame)

      # on tente de convertir les dimensions du jonc en reel
      #   - axe1:
      try:
          axe1=float(axe1)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("axe1 must be a real!")
      #   - axe2:
      try:
          axe2=float(axe2)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("axe2 must be a real!")
      #   - axe3:
      try:
          axe3=float(axe3)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("axe3 must be a real!")
      # on stocke les dimensions du plan
      self.axes = (axe1, axe2, axe3)

      # on calcule le volume et l'inertie du plan, s'ils n'ont pas ete donnes par l'utilisateur
      if self.volume == 0. or numpy.allclose(self.I, numpy.zeros((3, 3), 'd'), atol=1e-6):
         # axes[0] corresponds to the half length dir 1 (L1)
         # axes[1] corresponds to the half length dir 2 (L2)
         # axes[2] corresponds to the half thickness    (e)
         
         # inertie geometrique du plan 
         # vol = L1 * L2 * e
         # I1,I2 plan        I1=vol*(L2 ** 2 + e ** 2) /12   I1= vol *( L1**2 + e**2) / 12  
         # I3 axe hors plan  I3 = vol * (L1**2 + L2**2) /12

         # on calcule le volume du plan
         self.volume = 8.*self.axes[0]*self.axes[1]*self.axes[2]

         # on calcule de la matrice d'inertie dans le repere global, en
         # supposant que le repere principal d'inertie du plan est confondu
         # avec le repere global

         # on calcule le moment d'inertie par rapport a chaque axe
         I11 = self.volume*(self.axes[1]*self.axes[1] + self.axes[2]*self.axes[2])/3.
         I22 = self.volume*(self.axes[0]*self.axes[0] + self.axes[2]*self.axes[2])/3.
         I33 = self.volume*(self.axes[1]*self.axes[1] + self.axes[0]*self.axes[0])/3.
         # on en deduit la matrice d'inertie, dans le repere global
         self.I = numpy.array([[I11, 0. , 0. ],
                               [0. , I22, 0. ],
                               [0. , 0. , I33]])

      # si l'utilisateur a donne un frame qui n'est pas confondu avec le repere global
      if not numpy.allclose(self.frame, numpy.eye(3), atol=1e-6): 
         # on tient compte de son orientation pour pouvoir exprimer la matrice d'inertie dans le repere global
         self.I = numpy.dot(self.frame, numpy.dot(self.I, self.frame.T))

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the plane itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # on ecrit la premiere ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  axe1=%14.7E  axe2=%14.7E  axe3=%14.7E\n' % (self.shape, number, self.color, \
                                                                                 self.axes[0], self.axes[1], self.axes[2])
      
      # si on a donne un shift non nul et/ou un repere oriente differement du repere global      
      if not numpy.allclose(self.shift, numpy.zeros(3, 'd'), atol=1e-6) or \
         not numpy.allclose(self.frame, numpy.eye(3), atol=1e-6): 
         # on ecrit la ligne indiquant qu'on va donne un repere
           line += '                             '
           line += 'localframe\n'
           # on ecrit le repere :
           # la matrice d'orientation, s'ecrit ecrit P=(alpha, beta, gamma), ou alpha, beta et gamma sont des vecteurs colonnes
           # on ecrit :
           #    * les composantes de alpha
           line += '                             '
           line += 'alp1=%14.7E  alp2=%14.7E  alp3=%14.7E\n' % (self.frame[0, 0], self.frame[1, 0], self.frame[2, 0])
           #    * les composantes de beta
           line += '                             '
           line += 'bet1=%14.7E  bet2=%14.7E  bet3=%14.7E\n' % (self.frame[0, 1], self.frame[1, 1], self.frame[2, 1])
           #    * les composantes de gamma
           line += '                             '
           line += 'gam1=%14.7E  gam2=%14.7E  gam3=%14.7E\n' % (self.frame[0, 2], self.frame[1, 2], self.frame[2, 2])
           # on ecrit le shift :
           line += '                             '
           line += 'coo1=%14.7E  coo2=%14.7E  coo3=%14.7E\n' % (self.shift[0], self.shift[1], self.shift[2])

      # on renvoie le texte decrivant le contacteur
      return line


# les polyedres
# TODO: arreter de filer le nombre de sommets du polyhedre et passer directement un maillage a base de triangles.
class polyr(rigidContactor3D):
   """class polyr(rigidContactor3D):
      this class defines the polyhedra by a discretization of its (closed) skin, using triangles.
      static attribute:
         - shape='POLYR'
      attributes:
         - nb_vertices: number of vertices
         - vertices: vertices of the polyhedra
         - nb_faces: number of triangles in the discretization of the polyhedron skin
         - connectivity: connectivity of the triangles 
   """
   # type : POLYR
   shape='POLYR'

   def __init__(self, elements, color, nb_vertices, vertices, nb_faces, connectivity, \
                volume=None, I=None, shift=None, frame=None, reverse=False, **options):
      """__init__:
         allow to define a rigid polyheron.
         parameters:
            - self: the polyhedron itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - nb_vertices: number of vertices
            - vertices: vertices of the polyhedra
            - nb_faces: number of triangles in the discretization of the polyhedron skin
            - connectivity: connectivity of the triangles 
         optional parameters:
            - volume: volume of the contactor
            - I: inertia matrix (3x3 matrix) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - frame: if the contactor is turned relatively to the local frame of the body
            - reverse: if the elements must be reversed
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on appelle le constructeur generique de constructeur de contacteur rigide, 3D
      rigidContactor3D.__init__(self, elements, self.shape, color, volume=volume, I=I, shift=shift, frame=frame)

      # on tente de convertir le nombre de sommets en entier
      try:
          nb_vertices=int(nb_vertices)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("The number of vertices of a polyhedra must be an integer!")

      # on stocke le nombre de sommets du polyhedre
      self.nb_vertices=nb_vertices

      try:
          # on tente de convertir le tableau des coordonnees des sommets du polyhedre en tableau numpy
          vertices=numpy.array(vertices, 'd')
          # si la forme du tableau stockant les coordonnees des sommets n'est pas
          # celle attendue
          if vertices.shape != (self.nb_vertices, 3):
             # on affiche un message d'erreur
             showError("The coordinates of the vertices of a POLYR must stored in a matrix of shape (number of vertices,3)!")
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("The coordinates of the vertices of a POLYR must stored in a matrix of reals")

      # on stocke les coordonnees des sommets du polyhedre
      self.vertices = vertices

      # on tente de convertir le nombre de faces en entier
      try:
          nb_faces=int(nb_faces)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("The number of faces of a POLYR must be a integer!")

      # on stocke le nombre de faces du polyhedre
      self.nb_faces=nb_faces

      try:
          # on tente de convertir le tableau des connectivites des faces du polyhedre en tableau numpy
          connectivity=numpy.array(connectivity, 'i')
          # si la forme du tableau stockant les connectivites des faces n'est pas
          # celle attendue
          if connectivity.shape != (self.nb_faces, 3):
             # on affiche un message d'erreur
             showError("The connectivities of a POLYR must be stroed in a matrix of shape (number of faces,3)")
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("The connectivities of a POLYR must be stored in a matrix of integers")

      # on stocke les connectivites des faces du polyhedre
      self.connectivity=connectivity

      # on calcule le volume et l'inertie du polyedre, s'ils n'ont pas ete donnes par l'utilisateur
      # N.B.: la methode de calcul utilisee suppose que le corps soit convexe!!
      if self.volume == 0. or numpy.allclose(self.I, numpy.zeros((3, 3), 'd'), atol=1e-6):
         # si l'utilisateur a donne un shift non nul, on l'informe qu'il ne sera pas utilise
         if not numpy.allclose(self.shift, numpy.zeros(3, 'd'), atol=1e-6): 
            showWarning("skipping given shift, since it is automatically computed for polyhedra")

         # constante utile
         un_6 = 1.0/6.0

         # Les polyedres sont representes par une surface triangulee.
         # Connaissant les indices des sommets composant les faces, le centre
         # du polyedre, qui est a l'origine, on peut decouper le polyedre en 
         # tetraedres dont on sait calculer le volume.
         # Soit P1, P2, P3 et P4 les 4 sommets d'un tetraedre,
         # son volume sera:
         # V=|det(P1P2,P1P3,P1P4)|/6
         # Dans notre cas P1 est le barycentre du polyedre dans le
         # le repere absolu. P2, P3 et P4 sont les sommets de la face consideree.
         # on parcours toutes les faces et on calcule a chaque fois un volume elementaire. 

         # calcul de l'isobarycentre du polyedre

         # on initialise l'isobarycentre a l'origine
         center = numpy.zeros(3, 'd')
         # pour chaque sommet
         for i in range(0, self.nb_vertices):
            # on ajoute la contribution du sommet courant
            center += self.vertices[i, :]
         # on divise par le nombre de sommets
         center /= self.nb_vertices

         # le premier sommet de chaque tetraedre partitionnant le
         # polyedre est l'isobarycentre du polyedre
         p1 = numpy.array(center)
         # on prepare le stockage des jacobiens de chaque tetraedre
         # en effet, pour le calcul du volume et de l'inertie du
         # polyedre, on a besoin du determinant de la matrice jacobienne
         # de la transformation qui envoie le tetraedre de reference 
         # sur le tetraedre reel
         detJ = numpy.zeros(self.nb_faces, 'd')
         # on prepare le stockage des positions des centres d'inertie
         # de chaque tetraedre, par rapport a l'origine du repere global
         tetra_OG = numpy.zeros([self.nb_faces, 3], 'd')

         # calcul du centre d'inertie et du volume du polyedre
  
         # on initialise le volume du polyedre a 0
         self.volume = 0.
         # on initialise le centre d'inertie a l'origine
         self.shift = numpy.zeros(3, 'd')
         # pour chaque face
         for i in range(0, self.nb_faces):
            # on recupere les coordonnees des sommets de la face,
            # completant la definition du tetraedre courant
            p2 = self.vertices[self.connectivity[i, 0] - 1, :] 
            p3 = self.vertices[self.connectivity[i, 1] - 1, :] 
            p4 = self.vertices[self.connectivity[i, 2] - 1, :] 
            # on calcule le jacobien pour le tetraedre courant
            detJ[i] = compute_jacobian(p1, p2, p3, p4) 
            # si le jacobien est negatif
            if detJ[i] < 0.:
               # on affiche un warning
               #showWarning('face ' + str(i) + ' numbering in wrong order')
               # on change son signe
               detJ[i] = -detJ[i]
            # on en deduit le volume du tetraedre courant
            tetra_vol = un_6*detJ[i]

            # on ajoute la contribution du tetraedre courant au volume
            # du plyedre
            self.volume += tetra_vol

            # on calcule la postion du centre d'inertie du tetraedre
            tetra_OG[i, :] = 0.25*(p1 + p2 + p3 + p4)
            # on ajoute la contribution du tetraedre a la position du centre 
            # d'inertie du polyedre
            self.shift += tetra_vol*tetra_OG[i, :]

         # si le volume du polyedre est nul
         if self.volume < 1.e-18:
            showError('polyhedron volume is less than 1.e-18')

         # on divise par le volume pour finir le calcul du centre d'inertie du polyedre
         self.shift /= self.volume

         # calcul de l'inertie du polyedre

         # on initialise la matrice d'inertie du polyedre a la matrice nulle
         self.I = numpy.zeros([3, 3], 'd')
         # pour chaque face
         for i in range(0, self.nb_faces):
            # on recupere les coordoonnees de l'isobarycentre du tetraedre,
            # qui contitue le premier sommet du tetraedre
            p1 = numpy.array(center)
            # on recupere les coordonnees des sommets de la face,
            # completant la definition du tetraedre courant
            p2 = numpy.array(self.vertices[self.connectivity[i, 0] - 1, :])
            p3 = numpy.array(self.vertices[self.connectivity[i, 1] - 1, :])
            p4 = numpy.array(self.vertices[self.connectivity[i, 2] - 1, :])
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
            self.I += tetra_I
            # on calcule shift entre le centre d'inertie du polyedre et le centre d'inertie du 
            # tetraedre courant 
            d = tetra_OG[i, :] - self.shift
            # on calcule le volume du tetraedre courant
            tetra_vol = un_6*detJ[i]
            # on en deduit :
            #    * la contribution de la distance a l'axe aux termes diagonaux
            self.I[0, 0] += tetra_vol*(d[1]*d[1] + d[2]*d[2])
            self.I[1, 1] += tetra_vol*(d[0]*d[0] + d[2]*d[2])
            self.I[2, 2] += tetra_vol*(d[0]*d[0] + d[1]*d[1])
            #    * la contribution de la distance a l'axe aux termes extra-diagonaux
            self.I[0, 1] -= tetra_vol*d[0]*d[1]
            self.I[1, 0] -= tetra_vol*d[0]*d[1]
            self.I[0, 2] -= tetra_vol*d[0]*d[2]
            self.I[2, 0] -= tetra_vol*d[0]*d[2]
            self.I[1, 2] -= tetra_vol*d[1]*d[2]
            self.I[2, 1] -= tetra_vol*d[1]*d[2] 

      # on exprime les coordonnees des sommets par rapport au centre d'inertie du polyedre
      for i in range(self.nb_vertices):
         self.vertices[i, :] -= self.shift

      # optional reverse
      if reverse:
        self.connectivity[:,[1,2]] = self.connectivity[:,[2,1]]

      # si l'utilisateur a donne un frame qui n'est pas confondu avec le repere global, on l'informe qu'il ne sera pas utilise
      if not numpy.allclose(self.frame, numpy.eye(3), atol=1e-6): 
         showWarning("frames are not handled by polyhedra.")

   def updateFrame(self, P):
      """updateFrame(self, P):
         this functions change contactors properties due to frame change: from the global frame to the inertia frame
         parameters:
            - self: the contactor itself
            - P: inertia frame store as a 3 x 3 matrix, expressing the inertia frame basis (a, b, c) in the global frame (x, y, z)
                 P = (a, b, c)
      """
      try:
         # on s'assure que la matrice de passage est bien un tabelau numpy
         P = numpy.array(P, 'd')
         # si sa taille n'est pas bonne
         if P.shape != (3, 3):
            # on affiche un message d'erreur
            showError("the inertia frame must be a 3x3 matrix!")
      # si la conversion echoue
      except Exception:
         # on affiche un message d'erreur
         showError("the inertia frame must be a numpy array")

      # on exprime les coordonnees des sommets dans le repere principal d'inertie du corps
      for i in range(0, self.nb_vertices):
         # on calcule les les coordonnees du sommet courant dans le repere principal d'inertie du corps
         self.vertices[i, :] = numpy.dot(P.T, self.vertices[i, :])

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the polyhedron itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # on ecrit la premiere ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  nb_vertex=%7d    nb_faces=%7d\n' % (self.shape, number, self.color, \
                                                                           self.nb_vertices, self.nb_faces)

      # on calcule les positions des sommets, en tenant compte du shift

      # on les initialise aux positions, sans tenir compte du shift
      # i.e. on construit une copie des positions donnees
      vertices_global = numpy.array(self.vertices)
      # si le contacteur porte un shift
      if not numpy.allclose(self.shift, numpy.zeros(3, 'd'), atol=1e-6):
         # on l'applique aux coordonnees des sommets du polyedre
         for i in range(0, self.nb_vertices):
            vertices_global[i, :] += self.shift

      # on ajoute les lignes donnant les coordonnees des sommets
      for i in range(self.nb_vertices):
           line += '                             '
           line += 'coo1=%14.7E  coo2=%14.7E  coo3=%14.7E\n' % (vertices_global[i, 0], vertices_global[i, 1], vertices_global[i, 2])
      # on ajoute les lignes donnant les connectivites des faces
      for i in range(self.nb_faces):
           line += '                             '
           line += 'ver1=%7d         ver2=%7d         ver3=%7d\n' % \
              (self.connectivity[i, 0], self.connectivity[i, 1], self.connectivity[i, 2])

      # on renvoie le texte decrivant le contacteur
      return line

   def getAsData(self):
      """
      To get a integer and real data array usable
      to feed to chipy
      """
      rdata = numpy.array(self.vertices)
      rdata[:,:] += self.shift[:]
      rdata = numpy.ravel(rdata)
      idata = numpy.zeros( [2+3*self.nb_faces], dtype='int32' )
      idata[0] = self.nb_vertices
      idata[1] = self.nb_faces
      idata[2:] = numpy.ravel(self.connectivity)
      return idata, rdata

class polyd(rigidContactor3D):
   """class polyd(rigidContactor3D):
      this class defines the polyhedra by a discretization of its (closed) skin, using triangles.
      N.B.: - this contactor is designed to be used with a meshed avatar so that the connectivity of the triangles refer to
            the nodes of the volumic mesh
            - this contactor was designed to compute thermical dilatation of rigid bodies
      static attribute:
         - shape='POLYD'
      attributes:
         - nb_vertices: number of vertices
         - nb_faces: number of triangles in the discretization of the polyhedron skin
         - connectivity: connectivity of the triangles 
   """
   # type : POLYD
   shape='POLYD'

   def __init__(self, elements, color, nb_vertices, nb_faces, connectivity, volume=None, I=None, shift=None, frame=None, **options):
      """__init__:
         allow to define a polyheron, attached to a meshed body.
         parameters:
            - self: the polyhedron itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - nb_vertices: number of vertices
            - nb_faces: number of triangles in the discretization of the polyhedron skin
            - connectivity: connectivity of the triangles 
         optional parameters:
            - volume: volume of the contactor
            - I: inertia matrix (3x3 matrix) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - frame: if the contactor is turned relatively to the local frame of the body
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # si l'utilisateur a donne une propriete de contacteur rigide a ce contacteur, on l'informe qu'elle ne sera pas prise en compte
      #   * volume
      if volume is not None:
         showWarning("polyhedron attached to meshed bodies have no volume")
      #   * inertie
      if I is not None:
         showWarning("polyhedron attached to meshed bodies have no inertia")
      #   * shift
      if shift is not None:
         showWarning("polyhedron attached to meshed bodies have no shift")
      #   * orientation
      if frame is not None:
         showWarning("polyhedron attached to meshed bodies have no frame")

      # on indique que les proprietes rigides de ce contacteur n'ont pas de sens
      self.volume = None
      self.I = None
      self.shift = None
      self.frame = None

      # on appelle le constructeur generique de contacteur, car on n'a pas besoin de gerer les proprietes rigides 
      rigidContactor3D.__init__(self, elements, self.shape, color)

      # on tente de convertir le nombre de sommets en entier
      try:
          nb_vertices=int(nb_vertices)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("The number of vertices of a POLYR must be an integer!")

      # on stocke le nombre de sommets du polyhedre
      self.nb_vertices=nb_vertices

      # on tente de convertir le nombre de faces en entier
      try:
          nb_faces=int(nb_faces)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("The number of faces of a POLYR must be an integer!")

      # on stocke le nombre de faces du polyhedre
      self.nb_faces=nb_faces

      try:
          # on tente de convertir le tableau des connectivites des faces du polyhedre en tableau numpy
          connectivity=numpy.array(connectivity, 'i')
          # si la forme du tableau stockant les connectivites des faces n'est pas
          # celle attendue
          if connectivity.shape != (self.nb_faces, 3):
             # on affiche un message d'erreur
             showError("The connectivities of a POLYR must be stored in a matrix of shape (number of faces,3)")
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("The connectivities of a POLYR must be stored in a matrix of integers")

      # on stocke les connectivites des faces du polyhedre
      self.connectivity=connectivity

   def getRigidProperties(self):
      """getRigidProperties(self):
         this function returns the rigid proerties of a contactor, i.e. its volume, inertia and shift
         parameters:
            - self: the contactor itself
      """
      # on interdit l'acces au proprietes rigides d'un polyedre attache a un maille
      showError("rigid properties of a polyhedron attached to a meshed body are not defined!")

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the polyhedron itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # on ecrit la premiere ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  nb_vertex=%7d    nb_faces=%7d\n' % (self.shape, number, self.color, \
                                                                           self.nb_vertices, self.nb_faces)

      # on ajoute les lignes donnant les connectivites des faces
      for i in range(self.nb_faces):
           line += '                             '
           line += 'ver1=%7d         ver2=%7d         ver3=%7d\n' % \
              (self.connectivity[i, 0], self.connectivity[i, 1], self.connectivity[i, 2])

      # on renvoie le texte decrivant le contacteur
      return line


class polyf(rigidContactor3D):
   """class polyf(rigidContactor3D):
      this class defines the polyhedra by a discretization of its skin, using triangles.
      N.B.: this contactor is designed to handle a piecewise description of the skin.
      static attribute:
         - shape='POLYF'
      attributes:
         - nb_patches: number of patches
         - patches: a list of meshes, each mesh is a discretization of a part of the skin.
           N.B.: union of all this meshes is assumed to be a discretization of the full closed skin.
   """
   # type : POLYF
   shape='POLYF'

   def __init__(self, elements, color, nb_patch, patch, volume=None, I=None, shift=None, frame=None, reverse=False,**options):
      """__init__:
         allow to define a polyheron, giving a piecewiese descripion of its skin.
         parameters:
            - self: the polyhedron itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
            - nb_patch: number of patches
            - patch: a list of meshes, each mesh is a discretization of a part of the skin with triangles
         optional parameters:
            - volume: volume of the contactor
            - I: inertia matrix (3x3 matrix) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - frame: if the contactor is turned relatively to the local frame of the body
            - reverse: if the contactor must be returned
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # si l'utilisateur n'a pas donne les proprietes rigides du polyedre, on lui informe qu'il doit le faire.
      if volume is None or I is None or shift is None:
         showError("polyhedron defined by a pieceweise discretization must be given with volume, inertia, on shift")

      # on appelle le constructeur generique de contacteur, car on n'a pas besoin de gerer les proprietes rigides 
      rigidContactor3D.__init__(self, elements, self.shape, color, volume=volume, I=I, shift=shift, frame=frame)

      # on tente de convertir le nombre de patchs en entier
      try:
          nb_patch=int(nb_patch)
      # si on echoue
      except:
          # on affiche un message d'erreur
          showError("The number of patches of a POLYF must be an integer!")

      # on stocke le nombre de patchs du polyedre
      self.nb_patches=nb_patch

      # on teste que la liste de patch soit une liste
      if not isinstance(patch, list):
         showError("The list of patches of a POLYF must be a Python list")

      # on part du principe que la liste de patch est bonne
      # c'est a dire une liste d'objet mesh qui contient que
      # une liste de triangles et la listes des noeuds correspondants.
      # (on pourrait faire une deepcopy ici...)
      self.patches=patch
      for p in self.patches:
        for n in p.nodes:
          n.coor -= self.shift

      # si l'utilisateur a donne un frame qui n'est pas confondu avec le repere global, on l'informe qu'il ne sera pas utilise
      if not numpy.allclose(self.frame, numpy.eye(3), atol=1e-6): 
         showWarning("frames are not handled by polyhedra.")

      if reverse:
        for p in self.patches:
          for p in p.bulks:
            p.connectivity[1], p.connectivity[2] = p.connectivity[2], p.connectivity[1]

   def updateFrame(self, P):
      """updateFrame(self, P):
         this functions change contactors properties due to frame change: from the global frame to the inertia frame
         parameters:
            - self: the contactor itself
            - P: inertia frame store as a 3 x 3 matrix, expressing the inertia frame basis (a, b, c) in the global frame (x, y, z)
                 P = (a, b, c)
      """
      try:
         # on s'assure que la matrice de passage est bien un tabelau numpy
         P = numpy.array(P, 'd')
         # si sa taille n'est pas bonne
         if P.shape != (3, 3):
            # on affiche un message d'erreur
            showError("the inertia frame must be a 3x3 matrix!")
      # si la conversion echoue
      except Exception:
         # on affiche un message d'erreur
         showError("the inertia frame must be a numpy array")

      # on exprime les coordonnees des sommets dans le repere principal d'inertie du corps
      for patch in self.patches:
         # pour chaque sommet du patch
         for nod in patch.nodes:
            # on calcule les les coordonnees du sommet courant dans le repere principal d'inertie du corps
            nod.coor = numpy.dot(P.T, nod.coor)

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the polyhedron itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # on ecrit la premiere ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  nb_patch=  %5d\n' % (self.shape, number, self.color, self.nb_patches)

      # on ecrit le bloc decrivant chaque patch
      for i in range(self.nb_patches):
         # on recupere le patch courant
         patch=self.patches[i]

         # on ecrit la ligne indiquant le nombre de sommets et de faces
         line += '                             '
         line += 'nb_vertex=  %5d  nb_faces=  %5d\n' % (len(patch.nodes), len(patch.bulks))

         # on calcule les positions des sommets, en tenant compte du shift

         # on les initialise aux positions, sans tenir compte du shift
         # i.e. on construit une copie des positions donnees
         vertices_global = numpy.zeros((len(patch.nodes), 3), 'd')
         for i, nod in enumerate(patch.nodes):
            vertices_global[i, :] = nod.coor
         # si le contacteur porte un shift
         if not numpy.allclose(self.shift, numpy.zeros(3, 'd'), atol=1e-6):
            # on l'applique aux coordonnees des sommets du patch
            for i in range(len(patch.nodes)):
               vertices_global[i, :] += self.shift

         # on ajoute les lignes donnant les coordonnees des sommets
         for i in range(len(patch.nodes)):
              line += '                             '
              line += 'coo1=%14.7E  coo2=%14.7E  coo3=%14.7E\n' % (vertices_global[i, 0], vertices_global[i, 1], vertices_global[i, 2])
         # on ajoute les lignes donnant les connectivites des faces
         for ele in patch.bulks:
              line += '                             '
              line += 'ver1=%7d         ver2=%7d         ver3=%7d\n' % \
                      (ele.connectivity[0], ele.connectivity[1], ele.connectivity[2])

      # on renvoie le texte decrivant le contacteur
      return line

class polyo(polyf):
   shape = 'POLYO'

   def __init__(self, *args, **kwargs):
     return super(polyo, self).__init__(*args, **kwargs)


# les points
class pt3dx(rigidContactor3D):
   """class pt3dx(rigidContactor3D):
      this class defines the points on rigid bodies.
      static attribute:
         - shape='PT3Dx'
   """
   # type : PT3Dx
   shape='PT3Dx'

   def __init__(self, elements, color, volume=None, I=None, shift=None, frame=None, **options):
      """__init__:
         allow to define a point on a rigid body.
         parameters:
            - self: the point itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
         optional parameters:
            - volume: volume of the contactor
            - I: inertia matrix (3x3 matrix) of the contactor
            - shift: vector from the mass center of the body, to the mass center of the contactor
            - frame: if the contactor is turned relatively to the local frame of the body
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on appelle le constructeur generique de constructeur de contacteur rigide, 3D
      rigidContactor3D.__init__(self, elements, self.shape, color, volume=volume, I=I, shift=shift, frame=frame)

      # si l'utilisateur n'a pas donne le volume et l'inertie du disque, on conserve les
      # valeurs par defaut (i.e. surface et inertie nulles), car un point n'a pas de volume!

      # si l'utilisateur a donne un frame qui n'est pas confondu avec le repere global, on l'informe qu'il ne sera pas utilise
      if not numpy.allclose(self.frame, numpy.eye(3), atol=1e-6): 
         showWarning("frames are not handled by points.")

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the point itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      # on ecrit la seule ligne decrivant le contacteur
      line = ' %5s  %5d  color  %5s  coo1=%14.7E  coo2=%14.7E  coo3=%14.7E\n' % (self.shape, number, self.color, \
                                                                                 self.shift[0], self.shift[1], self.shift[2])

      # on renvoie le texte decrivant le contacteur
      return line


if __name__=='__main__':
   # test a la con...
   from build_avatar.mesh import *
   from avatar.bulk.rigid3d import *
   from avatar.bulk.element import *

   b = rigid3d()
   s = spher([b], 'BLUEx', 1.)
   print(s.getRigidProperties())
   print(s.strInBodiesFile(1))

   s2 = spher([b], 'BLUEx', shift=[0., 1., 0.], byrd=1.)
   print(s2.getRigidProperties())
   print(s2.strInBodiesFile(2))

   c = cylnd([b], 'BLUEx', High=1., byrd=1.)
   print(c.getRigidProperties())
   print(c.strInBodiesFile(1))

   c2 = dnlyc([b], 'BLUEx', High=1., byrd=1.)
   print(c2.getRigidProperties())
   print(c2.strInBodiesFile(1))

   pl = planx([b], 'WALLx', axe1=0.5, axe2=1., axe3=0.01)
   print(pl.getRigidProperties())
   print(pl.strInBodiesFile(1))

   # test pour le polyedre : le premier bloc du test "arche_plein_ceintre_3D"
   nb_blocs = 11
   theta_joint = math.pi/100.
   r_int = 0.8; r_ext = 1.
   theta_bloc = (math.pi - (nb_blocs - 1)*theta_joint)/ \
      nb_blocs
   e_bloc = r_ext - r_int
   theta = 0.

   #    * coordonnees des sommets (repere global)
   vertices = numpy.zeros([8, 3], 'd')
   #       - sommet 1
   vertices[0, 0]=r_int*math.cos(theta + theta_bloc)
   vertices[0, 1]=-0.5*e_bloc
   vertices[0, 2]=r_int*math.sin(theta + theta_bloc)
   #       - sommet 2
   vertices[1, 0]=r_int*math.cos(theta)
   vertices[1, 1]=-0.5*e_bloc
   vertices[1, 2]=r_int*math.sin(theta)
   #       - sommet 3
   vertices[2, 0]=r_int*math.cos(theta)
   vertices[2, 1]= 0.5*e_bloc
   vertices[2, 2]=r_int*math.sin(theta)
   #       - sommet 4
   vertices[3, 0]=r_int*math.cos(theta + theta_bloc)
   vertices[3, 1]= 0.5*e_bloc
   vertices[3, 2]=r_int*math.sin(theta + theta_bloc)
   #       - sommet 5
   vertices[4, 0]=r_ext*math.cos(theta + theta_bloc)
   vertices[4, 1]=-0.5*e_bloc
   vertices[4, 2]=r_ext*math.sin(theta + theta_bloc)
   #       - sommet 6
   vertices[5, 0]=r_ext*math.cos(theta)
   vertices[5, 1]=-0.5*e_bloc
   vertices[5, 2]=r_ext*math.sin(theta)
   #       - sommet 7
   vertices[6, 0]=r_ext*math.cos(theta)
   vertices[6, 1]= 0.5*e_bloc
   vertices[6, 2]=r_ext*math.sin(theta)
   #       - sommet 8
   vertices[7, 0]=r_ext*math.cos(theta + theta_bloc)
   vertices[7, 1]= 0.5*e_bloc
   vertices[7, 2]=r_ext*math.sin(theta + theta_bloc)
   #    * connectivite des faces
   faces = numpy.zeros([12, 3], 'i')
   faces[ 0, 0]=1; faces[ 0, 1]=2; faces[ 0, 2]=3
   faces[ 1, 0]=1; faces[ 1, 1]=3; faces[ 1, 2]=4
   faces[ 2, 0]=1; faces[ 2, 1]=2; faces[ 2, 2]=6
   faces[ 3, 0]=1; faces[ 3, 1]=6; faces[ 3, 2]=5
   faces[ 4, 0]=2; faces[ 4, 1]=3; faces[ 4, 2]=7
   faces[ 5, 0]=2; faces[ 5, 1]=7; faces[ 5, 2]=6
   faces[ 6, 0]=1; faces[ 6, 1]=4; faces[ 6, 2]=8
   faces[ 7, 0]=1; faces[ 7, 1]=8; faces[ 7, 2]=5
   faces[ 8, 0]=3; faces[ 8, 1]=4; faces[ 8, 2]=8
   faces[ 9, 0]=3; faces[ 9, 1]=8; faces[ 9, 2]=7
   faces[10, 0]=5; faces[10, 1]=7; faces[10, 2]=8
   faces[11, 0]=5; faces[11, 1]=6; faces[11, 2]=7

   pr = polyr([b], 'REDxx', 8, vertices, 12, faces)
   print(pr.getRigidProperties())
   print(pr.vertices)
   pr.updateFrame(numpy.eye(3))
   print(pr.vertices)
   print(pr.strInBodiesFile(2))

   pd = polyd([b], 'REDxx', 8, 12, faces)
   print(pd.strInBodiesFile(2))

   # test des POLYF : on construit les 6 faces du bloc precedent, sous la forme de maillages
   m1 = mesh(dimension=3)
   m1.addNode( node( coor=numpy.array(vertices[0, :]), number=1) )
   m1.addNode( node( coor=numpy.array(vertices[1, :]), number=2) )
   m1.addNode( node( coor=numpy.array(vertices[2, :]), number=3) )
   m1.addNode( node( coor=numpy.array(vertices[3, :]), number=4) )
   m1.addBulk( element(elem_dim=2, connectivity=[1, 2, 3]) )
   m1.addBulk( element(elem_dim=2, connectivity=[1, 3, 4]) )
 
   m2 = mesh(dimension=3)
   m2.addNode( node( coor=numpy.array(vertices[0, :]), number=1) )
   m2.addNode( node( coor=numpy.array(vertices[1, :]), number=2) )
   m2.addNode( node( coor=numpy.array(vertices[4, :]), number=5) )
   m2.addNode( node( coor=numpy.array(vertices[5, :]), number=6) )
   m2.addBulk( element(elem_dim=2, connectivity=[1, 2, 6]) )
   m2.addBulk( element(elem_dim=2, connectivity=[1, 6, 5]) )

   m3 = mesh(dimension=3)
   m3.addNode( node( coor=numpy.array(vertices[1, :]), number=2) )
   m3.addNode( node( coor=numpy.array(vertices[2, :]), number=3) )
   m3.addNode( node( coor=numpy.array(vertices[5, :]), number=6) )
   m3.addNode( node( coor=numpy.array(vertices[6, :]), number=7) )
   m3.addBulk( element(elem_dim=2, connectivity=[2, 3, 7]) )
   m3.addBulk( element(elem_dim=2, connectivity=[2, 7, 6]) )

   m4 = mesh(dimension=3)
   m4.addNode( node( coor=numpy.array(vertices[0, :]), number=1) )
   m4.addNode( node( coor=numpy.array(vertices[3, :]), number=4) )
   m4.addNode( node( coor=numpy.array(vertices[4, :]), number=5) )
   m4.addNode( node( coor=numpy.array(vertices[7, :]), number=8) )
   m4.addBulk( element(elem_dim=2, connectivity=[1, 4, 8]) )
   m4.addBulk( element(elem_dim=2, connectivity=[1, 8, 5]) )

   m5 = mesh(dimension=3)
   m5.addNode( node( coor=numpy.array(vertices[2, :]), number=3) )
   m5.addNode( node( coor=numpy.array(vertices[3, :]), number=4) )
   m5.addNode( node( coor=numpy.array(vertices[6, :]), number=7) )
   m5.addNode( node( coor=numpy.array(vertices[7, :]), number=8) )
   m5.addBulk( element(elem_dim=2, connectivity=[3, 4, 8]) )
   m5.addBulk( element(elem_dim=2, connectivity=[3, 8, 7]) )

   m6 = mesh(dimension=3)
   m6.addNode( node( coor=numpy.array(vertices[4, :]), number=5) )
   m6.addNode( node( coor=numpy.array(vertices[5, :]), number=6) )
   m6.addNode( node( coor=numpy.array(vertices[6, :]), number=7) )
   m6.addNode( node( coor=numpy.array(vertices[7, :]), number=8) )
   m6.addBulk( element(elem_dim=2, connectivity=[5, 7, 8]) )
   m6.addBulk( element(elem_dim=2, connectivity=[5, 6, 7]) )

   # on recupere les proprietes rigides du bloc, calculees avec sa representation POLYR
   volume, I, shift, frame=pr.getRigidProperties()
   # on peut alors construire le POLYF
   pf = polyf([b], 'REDxx', 6, [m1, m2, m3, m4, m5, m6], volume=volume, I=I, shift=shift)
   print(pf.strInBodiesFile(2))
   pf.updateFrame(numpy.eye(3))
   print(pf.strInBodiesFile(2))

   pt = pt3dx([b], 'REDxx', shift=[0., 1., 0.])
   print(pt.strInBodiesFile(2))
