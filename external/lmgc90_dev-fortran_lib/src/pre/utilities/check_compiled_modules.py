import sys

# variable qui indique si on veut que les modules compiles soient testes avant d'etre importes
compiled_module_check=False # valeur par defaut : faux, pour aller vite en utilisation normale

# si on utilise Salome
if "Salome" in sys.executable or "SALOME" in sys.executable:
   # on doit verifier que les modules compiles peuvent etre importes
   compiled_module_check=True

# si les modules compiles doivent etre testes, on importe le module avec les fonction qui font vraiment le test
if compiled_module_check:
   from .check_compiled_modules_real import *
# sinon, on importe le module avec les fonctions qui repondent "oui" sans reflechir
else:
   from .check_compiled_modules_fake import *
