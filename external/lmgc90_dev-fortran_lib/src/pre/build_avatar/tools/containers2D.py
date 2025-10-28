# module fournissant des macros de depot dans des conteneurs predifnis

# import du module permettant de savoir si on pourra importer les pre_tools
from ...utilities.check_compiled_modules import *

# import des des wrappers des anciens pre-processeurs pour les milieux granualaires

# si on peut essayer d'importer le module pre_tools sans tout faire planter
if import_lmgc90():
   # on essaye
   try:
      from ....chipy import lmgc90
   except:
      raise

import numpy as np

# fonction qui depose les particules dans une boite rectangulaire
def depositInBox2D(radii, lx, ly, d_radii=None, d_coor=None):
   '''
   This function deposits circular particles in a box with lower left corner in [0.,0]

   parameters:

   - radii: radii of the particles
   - lx: length of the box
   - ly: heigth of the box

   optional parameters:

   - d_radii=None: radii of particles supposed to be in the box before the deposit
   - d_coor=None: radii of these deposited particles

   returned values:

   - nb_particles: number of deposited particles
   - coor: coordinates of the deposited particles as [nb_particles,2] array
   - dradii: the deposited radii corresponding to coor

   WARNING: to avoid interpenetrations between particles and walls, this function
            uses a shrink based on the size of the particles
   '''
  
   d_radii = np.array([]  , dtype=float)   if d_radii is None else d_radii
   d_coor  = np.array([[]], dtype=float)   if d_coor  is None else d_coor

   d_radii = np.array(d_radii, dtype=float)   if isinstance(d_radii, list) else d_radii
   d_coor  = np.array(d_coor , dtype=float)   if isinstance(d_coor , list) else d_coor

   # on recupere le rayon du plus gros grain
   radius_max = np.max(radii)

   # on depose les grains sous gravite (1)
   coor = lmgc90.deposit2D_Potential(radii, lx, 1, d_radii, d_coor)

   # on definit la polyligne adaptee pour une boite : une ligne horizontale
   # en y=ly - radius_max 
   slope_coor = np.empty([2,2])
   slope_coor[0,0] = -0.5*lx
   slope_coor[1,0] =  1.5*lx
   slope_coor[:,1] = ly - radius_max

   # N.B.: on definit dans une ligne legerement plus basse, pour eliminer les
   #       les interpentrations avec le bord superireur de la boite
   # on enleve les grains au-dessus de la ligne
   dradii, coor = lmgc90.cut2D_Cut(radii, coor, slope_coor)
   
   # outputs
   nb_particles = coor.shape[0]

   return nb_particles, coor, dradii

# fonction qui depose les particules dans un dique
def depositInDisk2D(radii, r, d_radii=None, d_coor=None):
   '''
   This function deposits circular particles in a circular container centered in [r,r]

   parameters:

   - radii: radii of the particles
   - r: radius of the container
optional parameters:

   - d_radii=None: radii of particles supposed to be in the box before the deposit
   - d_coor=None: radii of these deposited particles

   returned values:

   - nb_particles: number of deposited particles
   - coor: coordinates of the deposited particles as [nb_particles,2] array
   - dradii: the deposited radii corresponding to coor
   '''

   d_radii = np.array([]  , dtype=float) if d_radii is None else d_radii
   d_coor  = np.array([[]], dtype=float) if d_coor  is None else d_coor

   d_radii = np.array(d_radii, dtype=float) if isinstance(d_radii, list) else d_radii
   d_coor  = np.array(d_coor , dtype=float) if isinstance(d_coor , list) else d_coor

   # on realise un depot sous gravite, dans une boite de largueur 2.*r
   coor = lmgc90.deposit2D_Potential(radii, 2.*r, 1, d_radii, d_coor)

   # on definit un contour circulaire, de rayon r et centre en [r, r]
   nb_s = 79 # should size depending of rmin/rmax ?
   slope_coor = np.empty([nb_s,2], float)
   t = np.linspace(0., 2.*np.pi, nb_s, endpoint=True)
   slope_coor[:,0] = r*(1 + np.cos(t) )
   slope_coor[:,1] = r*(1 + np.sin(t) )

   # on enleve les grains hors du contour
   dradii, coor = lmgc90.cut2D_Cut(radii, coor, slope_coor)
  
   # outputs
   nb_particles = coor.shape[0]

   return nb_particles, coor, dradii

# fonction qui depose les particules dans un "cylindre", pour un cisaillement
# de Couette
def depositInCouette2D(radii, rint, rext, d_radii=None, d_coor=None):
   '''
   This function deposits circular particles in container designed for a Couette shear centered in [rext, rext]

   parameters:

   - radii: radii of the particles
   - rint: internal radius of the ring occupied by particles
   - rext: external radius of the ring occupied by particles

   optional parameters:

   - d_radii=None: radii of particles supposed to be in the box before the deposit
   - d_coor=None: radii of these deposited particles

   returned values:

   - nb_particles: number of deposited particles
   - coor: coordinates of the deposited particles as [nb_particles,2] array
   - dradii: the deposited radii corresponding to coor

   WARNING: to avoid interpenetrations between particles and walls, this function
            uses a shrink based on the size of the particles
   '''

   radius_max = np.amax(radii)

   d_radii = np.array(d_radii, dtype=float) if isinstance(d_radii, list) else d_radii
   d_coor  = np.array(d_coor , dtype=float) if isinstance(d_coor , list) else d_coor

   big_radii = np.array( [rint]       , dtype=float)
   big_coor  = np.array([[rext, rext]], dtype=float)

   big_radii = big_radii if d_radii is None else np.append( big_radii, d_radii )
   big_coor  = big_coor  if d_coor  is None else np.append( big_coor , d_coor  )

   # on realise un depot sous gravite, dans une boite de largueur 2.*rext
   coor = lmgc90.deposit2D_Potential(radii, 2.*rext, 3, big_radii, big_coor)

   # on definit un contour circulaire, de rayon rext et centre en [rext, rext]
   nb_s = 80 # should size depending of rmin/rmax ?
   slope_coor = np.empty([nb_s,2], float)
   t = np.linspace(0., 2.*np.pi, nb_s, endpoint=True)
   slope_coor[:,0] = rext + (rext-radius_max)*np.cos(t)
   slope_coor[:,1] = rext + (rext-radius_max)*np.sin(t)

   # on enleve les grains hors du contour
   dradii, coor = lmgc90.cut2D_Cut(radii, coor, slope_coor)
  
   # outputs
   nb_particles = coor.shape[0]

   return nb_particles, coor, dradii


# fonction qui depose les particules de sorte a remplir un demi-tambour
def depositInDrum2D(radii, r, d_radii=None, d_coor=None):
   '''
   This function deposits circular particles in the lower half part of a drum centered in [r, r]

   parameters:

   - radii: radii of the particles
   - r: radius of the container 

   optional parameters:

   - d_radii=None: radii of particles supposed to be in the box before the deposit
   - d_coor=None: radii of these deposited particles

   returned values:

   - nb_particles: number of deposited particles
   - coor: coordinates of the deposited particles as [nb_particles,2] array
   - dradii: the deposited radii corresponding to coor

   WARNING: to avoid interpenetrations between particles and walls, this function
            uses a shrink based on the size of the particles
   '''

   radius_max = np.amax(radii)

   d_radii = np.array([]  , dtype=float) if d_radii is None else d_radii
   d_coor  = np.array([[]], dtype=float) if d_coor  is None else d_coor

   d_radii = np.array(d_radii, dtype=float) if isinstance(d_radii, list) else d_radii
   d_coor  = np.array(d_coor , dtype=float) if isinstance(d_coor , list) else d_coor

   # on realise un depot sous gravite, dans une boite de largueur 2.*r
   dcoor = lmgc90.deposit2D_Potential(radii, 2.*r, 1, d_radii, d_coor)

   # on definit un contour circulaire, de rayon r et centre en [r, r]
   nb_s = 41 # should size depending of rmin/rmax ?
   slope_coor = np.empty([nb_s,2], float)
   t = np.linspace(0., np.pi, nb_s-1, endpoint=True)
   slope_coor[1:,0] = r + (r-radius_max)*np.cos(t)
   slope_coor[1:,1] = r - (r-radius_max)*np.sin(t)
   slope_coor[0, 0] = radius_max
   slope_coor[0, 1] = r

   # on enleve les grains hors du contour
   dradii, coor = lmgc90.cut2D_Cut(radii, dcoor, slope_coor)
  
   # outputs
   nb_particles = coor.shape[0]

   return nb_particles, coor, dradii

