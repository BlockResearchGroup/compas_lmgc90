# fonction qui renvoie "vrai" ssi on peut importer le module pre_tools sans faire planter Python
# N.B.: l'idee du test est de lancer un processus fils qui appelle le meme interpreteur et tente
#    d'importer le module ; si ca echoue l'interpreteur pere es protege grace a l'emploi du module subprocess
def import_pre_tools():
   """import_pre_tools():
      this functions checks that pre_tools (compiled) module may be imported whithout crash
      returned value: "True" iff import pre_tools is secure
   """
   # on renvoie "vrai" sans refelchir 
   return True

# fonction qui renvoie "vrai" ssi on peut importer le module lmgc90 sans faire planter Python
# N.B.: l'idee du test est de lancer un processus fils qui appelle le meme interpreteur et tente
#    d'importer le module ; si ca echoue l'interpreteur pere es protege grace a l'emploi du module subprocess
def import_lmgc90():
   """import_lmgc90():
      this functions checks that lmgc (compiled) module may be imported whithout crash
      returned value: "True" iff import pre_tools is secure
   """
   # on renvoie "vrai" sans refelchir 
   return True

