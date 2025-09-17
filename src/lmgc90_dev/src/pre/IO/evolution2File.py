# module gerant l'ecriture du fichier d'evolution pour l'application des conditions limites
import os
from ..utilities.error import *

def writeEvolution(f, instants, path='', name='evolution.dat'):
   """
   This function writes an evolution file used to apply specific boundary conditions.


   Parameters:

   - f: a function of time
   - instants: for each instant t in the list, the function
     f is evaluated and the couple (t, f(t)) is write in the file


   Optional parameters:

   - path='': path to the direactory where to write the evolution file
   - name='evolution.dat': name of the evolution file

   """

   # on verifie que la fonction donnee est bien appelable
   if not callable(f):
      showError("given function is not callable!")

   # on ouvre le fichier d'evolution en ecriture
   fid = open(os.path.join(path,name), 'w')

   print('\nStart writing file\t:\t' + name)

   # on ecrit le couple t, f(t) pour chaque instant t
   for t in instants:
      fid.write('%14.7E %14.7E\n' % (t, f(t)))
   
   # on ferme le fichier
   fid.close()
   
   print('End of writing file\t:\t' + name)

