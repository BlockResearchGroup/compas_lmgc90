# import the module which parse the command line arguments
from . import arg_parse

# import du module permettant de savoir si on pourra importer les pre_tools
from .utilities.check_compiled_modules import *

# import des maps permettant de verifier la coherence des donnees
from .config.lmgc90dicts import *

# import de l'objet tact_behav (i.e. definissant les parametres d'une loi de contact)
from .shared.tact_behav import *
# import de l'objet bulk_behav (i.e. definissant les parametres materiau)
from .shared.bulk_behav import *
# import de l'objet postpro_command (i.e. definissant une commande de post-traitement) 
from .shared.postpro_command import *
# import de l'objet model (i.e. definissant un modele, mecanique ou thermique)
from .shared.model import *

# import des conteneneurs
#   * d'avatars
from .avatars import *
#   * de modeles
from .models import *
#   * de materiaux
from .bulk_behavs import *
#   * de loi de contact
from .tact_behavs import *
#   * de commandes de post-traitement
from .postpro_commands import *

# import des differents modules de build_avatar
#   * module de creation de briques 2D, ou 3D
from .build_avatar.brick import *
#   * module de creation de rangees de briques 3D
from .build_avatar.brick_row import *
#   * module de creation de rangees de mur de briques 3D
from .build_avatar.brick_wall import *
#   * module d'extrusion de corps rigides 2D, pour obtenir des corps rigides 3D
from .build_avatar.extrusion import *
#   * module de gestion de maillage generique (2D/3D)
from .build_avatar.mesh import *
#   * module de gestion de maillage 2D
from .build_avatar.mesh2D import *
#   * module de gestion de maillage 3D
from .build_avatar.mesh3D import *
##   * module de lecture de maillages
from .build_avatar.lecture import *
#   * module de creation de particules 2D, ou 3D
from .build_avatar.particles import *

# import des macros fournisant les fonctionnalites des anciens pre-processeurs pour mes milieux granulaires
from .build_avatar.tools.granulometry  import *

# si on peut essayer d'importer le module pre_tools sans tout faire planter
if import_lmgc90():
   # on essaye
   try:
      try:
         #   * import des macros de depot de grains
         #       - dans un conteneneur 2D
         from .build_avatar.tools.containers2D import *
         #       - dans un conteneneur 3D
         from .build_avatar.tools.containers3D import *
         # import du module de creation de parois 2D (reposant sur le module fournissant les fonctions permettant de
         # choisir la granulometrie)
         from .build_avatar.walls import *
      except ImportError:
         print('Unable to import wrapped part of the granular media module!')
         print('You cannot use granulometry facilities or deposit grains in a container')
   except:
      raise 
# sinon,
else:
   # on explique que c'est impossible
   print('Unable to import wrapped part of the granular media module!')
   print('You cannot use granulometry facilities or deposit grains in a container')

#       - sur un reseau 2D
from .build_avatar.lattices2D import *
#       - sur un reseau 3D
from .build_avatar.lattices3D import *

# import des fonctions d'ecriture des fichiers decrivant
#   * la geometrie des corps (BODIES.DAT)
from .IO.bodies2File import *
#   * les modeles (MODELS.DAT)
from .IO.model2File     import *
#   * les parametres materiaux (BULK_BEHAV.DAT)
from .IO.bulkBehav2File import *
#   * les lois de contat et les tables de visibilite (TACT_BEHAV.DAT)
from .IO.dofIni2File    import *
#   * les conditions limites (DRV_DOF.DAT)
from .IO.tactBehav2File import *
#   * les valeurs des ddl iniales (DOF.INI)
from .IO.drvDof2File    import *
#   * les valeurs aux points de Gauss iniales (GPV.INI)
from .IO.gpvIni2File    import *
#   * les contacts initiaux (Vloc_Rloc.INI)
from .IO.vlocrlocIni2File    import *
#   * les fichiers d'evolution, pour les conditions limites
from .IO.evolution2File import *
#   * les commandes de post-traitement (POSTPRO.DAT)
from .IO.postpro2File import *

from .IO.macro import readDatbox, readState, writeDatbox

try:
  from .viz.visuVtk import *
except ImportError:
   print('Unable to import visualization module with vtk')
except:
   raise 

# Parse argument of the command line
arg_parse.parse()
