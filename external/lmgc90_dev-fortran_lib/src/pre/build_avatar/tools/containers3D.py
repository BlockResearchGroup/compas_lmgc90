import numpy as np
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

# fonction qui depose les particules dans une boite paralepipedique
def depositInBox3D(radii, lx, ly, lz, d_radii=None, d_coor=None, seed=None, logmes=False):
   '''
   This function deposits spherical particles in a box

   parameters:

   - radii: radii of the particles to deposit
   - lx: width of the box, following Ox axis
   - ly: width of the box, following Oy axis
   - lz: heigth of the box

   N.B. a point (x, y, z) is in the box iff x is in [0, lx], y is in [0, ly] and z is in [0, lz]

   optional parameters:

   - d_radii=None: radii of particles supposed to be already deposited
   - d_coor=None: coordinates of these deposited particles
   - seed=None: an input seed to control randomness
   - logmes=False: de/activate log message

   returned values:

   - nb_particles: number of deposited particles
   - coor: coordinates of the deposited particles as [nb_reamining_particles,3] array
   - dradii: the deposited radii corresponding to coor
   ''' 
  
   d_radii = np.array([]  , dtype=float)   if d_radii is None else d_radii
   d_coor  = np.array([[]], dtype=float)   if d_coor  is None else d_coor
   seed    = np.array([]  , dtype='int32') if seed    is None else seed

   d_radii = np.array(d_radii, dtype=float)   if isinstance(d_radii, list) else d_radii
   d_coor  = np.array(d_coor , dtype=float)   if isinstance(d_coor , list) else d_coor
   seed    = np.array(seed   , dtype='int32') if isinstance(seed   , list) else seed

   d_coor  = d_coor[np.newaxis,:] if d_coor.ndim == 1 else d_coor

   # on depose les grains sous gravite dans la boite :
   dradii, coor = lmgc90.deposit3D_InContainer(radii, 0, lx, ly, lz, d_radii, d_coor, seed, logmes)
   coor[:,0] += 0.5*lx
   coor[:,1] += 0.5*ly

   # outputs
   nb_particles = coor.shape[0]

   return nb_particles, coor, dradii

# fonction qui depose les particules dans une boite cylindrique
def depositInCylinder3D(radii, R, lz, d_radii=None, d_coor=None, seed=None, logmes=False):
   '''
   This function deposits spherical particles in a cylinder

   parameters:

   - radii: radii of the particles to deposit
   - R: radius of the cylinder
   - lz: heigth of the cylinder

   N.B. a point (x, y, z) is in the cylinder iff x^2 + y^2 is in [0, R^2] and z is in [0, lz]

   optional parameters:

   - d_radii=None: radii of particles supposed to be already deposited
   - d_coor=None: coordinates of these deposited particles
   - seed=None: an input seed to control randomness
   - logmes=False: de/activate log message

   returned values:

   - nb_particles: number of deposited particles
   - coor: coordinates of the deposited particles as [nb_particles,3] array
   - dradii: the deposited radii corresponding to coor
   '''
  
   d_radii = np.array([]  , dtype=float)   if d_radii is None else d_radii
   d_coor  = np.array([[]], dtype=float)   if d_coor  is None else d_coor
   seed    = np.array([]  , dtype='int32') if seed    is None else seed

   d_radii = np.array(d_radii, dtype=float)   if isinstance(d_radii, list) else d_radii
   d_coor  = np.array(d_coor , dtype=float)   if isinstance(d_coor , list) else d_coor
   seed    = np.array(seed   , dtype='int32') if isinstance(seed   , list) else seed

   d_coor  = d_coor[np.newaxis,:] if d_coor.ndim == 1 else d_coor

   # on depose les grains sous gravite dans le cylindre :
   dradii, coor = lmgc90.deposit3D_InContainer(radii, 1, R, lz, 0., d_radii, d_coor, seed, logmes)

   # outputs
   nb_particles = coor.shape[0]

   return nb_particles, coor, dradii

# fonction qui depose les particules dans une boite spherique
def depositInSphere3D(radii, R, center, d_radii=None, d_coor=None, seed=None, logmes=False):
   '''
   This function deposits spherical particles in a sphere

   parameters:

   - radii: radii of the particles
   - R: radius of the sphere
   - center: center of the sphere

   N.B. a point (x, y, z) is in the sphere iff (x - x_C)^2 + (y - y_C)^2 + (z - z_C)^2 is in [0, R^2]

   optional parameters:

   - d_radii=None: radii of particles supposed to be already deposited
   - d_coor=None: coordinates of these deposited particles
   - seed=None: an input seed to control randomness
   - logmes=False: de/activate log message

   returned values:

   - nb_particles: number of deposited particles
   - coor: coordinates of the deposited particles as [nb_reamining_particles,3] array
   - dradii: the deposited radii corresponding to coor
   '''
  
   d_radii = np.array([]  , dtype=float)   if d_radii is None else d_radii
   d_coor  = np.array([[]], dtype=float)   if d_coor  is None else d_coor
   seed    = np.array([]  , dtype='int32') if seed    is None else seed

   d_radii = np.array(d_radii, dtype=float)   if isinstance(d_radii, list) else d_radii
   d_coor  = np.array(d_coor , dtype=float)   if isinstance(d_coor , list) else d_coor
   seed    = np.array(seed   , dtype='int32') if isinstance(seed   , list) else seed

   d_coor  = d_coor[np.newaxis,:] if d_coor.ndim == 1 else d_coor

   # on depose les grains sous gravite dans la sphere :
   dradii, coor = lmgc90.deposit3D_InContainer(radii, 2, R, 0., 0., d_radii, d_coor, seed, logmes)
     
   # outputs
   nb_particles = coor.shape[0]
   coor[:,:] += center

   return nb_particles, coor, dradii
 
