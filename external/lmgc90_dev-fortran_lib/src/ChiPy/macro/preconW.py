import sys, os
from .macro import *
import struct
import numpy as np
import math

########################################
# utilitaires

is_first_time = True


def create_preconW_directory():
  """ gestion/nettoyage du repertoire de sauvegarde des matrices precon W """
  global is_first_time 

  # creation du repertoire preconW si besoin
  chemin_repertoire = os.getcwd()
  if os.path.isdir(chemin_repertoire+'/preconW') == False:
     print('creation du repertoire preconW')
     os.mkdir(chemin_repertoire+'/preconW')   
     is_first_time = False
  # ou nettoyage
  else:
     if is_first_time:  
        print('nettoyage du repertoire preconW')
        for fichier in os.listdir(chemin_repertoire+'/preconW'):
            os.remove(chemin_repertoire+'/preconW/'+fichier)

        is_first_time = False




def save_preconW_one_body(ibdyty):
  """ sauve le matrice precon W d'un corps donne """
  
  # creation du repertoire preconW si besoin
  create_preconW_directory()

  # on recupere les noeuds
  vec_precon_nodes = mecaMAILx_GetNodesPrecon(ibdyty)

  # on recupere la matrice W (pointeur)
  mat_preconW = mecaMAILx_GetPtrPreconW(ibdyty)

  # ecriture de la matrice dans le fichier preconW_IdBody dans le repertoire preconW
  chemin_repertoire = os.getcwd()
  # * matrice preconW
  preconW=open(chemin_repertoire + '/preconW/preconW_'+str(ibdyty)+'.py','w')
  ##mat_preconW.tofile(preconW, sep=' ', format='%s') # ascii
  mat_preconW.tofile(preconW)
  preconW.close()
  # * liste des noeuds precon
  nodes_precon=open(chemin_repertoire + '/preconW/precon_nodes_'+str(ibdyty)+'.py','w')
  vec_precon_nodes.tofile(nodes_precon, sep=' ', format='%s') # ascii
  ##vec_precon_nodes.tofile(nodes_precon)
  nodes_precon.close()

  print('sauvegarde preconW corps : ',ibdyty)





def load_preconW_one_body(ibdyty):

  try:
     vec_file = os.path.exists('preconW/precon_nodes_'+str(ibdyty)+'.py')
  except:
     pass
     
  try:
     matrix_file = os.path.exists('preconW/preconW_'+str(ibdyty)+'.py')
  except:
     pass

  if not matrix_file:
     print('Recalcul de la matrice preconW ',ibdyty,': fichier preconW_',ibdyty,'.py absent du repertoire preconW')
  if not vec_file:
     print('Recalcul de la matrice preconW ',ibdyty,': fichier precon_nodes_',ibdyty,'.py absent du repertoire preconW')



  if matrix_file and vec_file:
     # noeuds stockes
     ##precon_nodes = np.fromfile('preconW/precon_nodes_'+str(ibdyty)+'.py', dtype=int)
     precon_nodes = np.fromfile('preconW/precon_nodes_'+str(ibdyty)+'.py',sep=' ', dtype=int)

     # noeuds calcules
     computed_precon_nodes = mecaMAILx_GetNodesPrecon(ibdyty)

     if (precon_nodes == computed_precon_nodes).all():
        print('Coherence sur les nodes precon pour le corps ',ibdyty,' verifiee ')
        mecaMAILx_LoadWPreconBody(ibdyty)

        print('  -> preconW ',ibdyty,' isloaded from file')

        preconW = mecaMAILx_GetPtrPreconW(ibdyty)
        ##read_preconW = np.fromfile('preconW/preconW_'+str(ibdyty)+'.py',sep=' ', dtype=float)
        read_preconW = np.fromfile('preconW/preconW_'+str(ibdyty)+'.py', dtype=float)
        
        #pta: reshape du np array -> utiliser save/load pour conserver la shape... a voir plus tard
        #print preconW.shape
        #print read_preconW.shape
        read_preconW.shape = [int(math.sqrt(read_preconW.size)),int(math.sqrt(read_preconW.size))]
        #print read_preconW.shape

        preconW[:,:] = read_preconW[:,:]

        ## TEST 
        #preconW = 'tata' # histoire d'etre sur de virer la reference
        #preconW2 = mecaMAILx_GetPtrPreconW(ibdyty)
        #if (preconW2 == read_preconW).all():
        #   print 'le loading fonctionne'
        #else:
        #   print 'aller voir un psy'

     else:
        print('Fichier precon_nodes_',ibdyty,'.py non coherent: recalcul de preconW')




