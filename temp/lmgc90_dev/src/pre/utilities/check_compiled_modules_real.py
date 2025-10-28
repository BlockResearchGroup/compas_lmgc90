import sys
import subprocess

# fonction qui renvoie "vrai" ssi on peut importer le module pre_tools sans faire planter Python
# N.B.: l'idee du test est de lancer un processus fils qui appelle le meme interpreteur et tente
#    d'importer le module ; si ca echoue l'interpreteur pere es protege grace a l'emploi du module subprocess
def import_pre_tools():
   """import_pre_tools():
      this functions checks that pre_tools (compiled) module may be imported whithout crash
      returned value: "True" iff import pre_tools is secure
   """
   # on essaye d'importer les pre_tools depuis les chemins du pre
   retcode1=subprocess.call([sys.executable, "-c", "try:\n   from build_avatar.tools.pre_tools import *\nexcept:   pass"])
   # on essaye d'importer les pre_tools depuis les chemins par defaut
   retcode2=subprocess.call([sys.executable, "-c", "try:\n   from pre_tools import *\nexcept:   pass"])
   # si aucun des deux essais n'a plante
   if retcode1 == 0 and retcode2 == 0:
      # on indique qu'on va pouvoir les importer
      return True
   # sinon,
   else:
      # on indique qu'on ne va pas pouvoir les importer
      return False

# fonction qui renvoie "vrai" ssi on peut importer le module lmgc90 sans faire planter Python
# N.B.: l'idee du test est de lancer un processus fils qui appelle le meme interpreteur et tente
#    d'importer le module ; si ca echoue l'interpreteur pere es protege grace a l'emploi du module subprocess
def import_lmgc90():
   """import_lmgc90():
      this functions checks that lmgc (compiled) module may be imported whithout crash
      returned value: "True" iff import pre_tools is secure
   """
   # on essaye d'importer les pre_tools depuis les chemins par defaut
   retcode=subprocess.call([sys.executable, "-c", "try:\n   from ...chipy import lmgc90\nexcept:   pass"])
   # si l'essai n'a pas plante
   if retcode == 0:
      # on indique qu'on va pouvoir l'importer
      return True
   # sinon,
   else:
      # on indique qu'on ne va pas pouvoir l'importer
      return False

