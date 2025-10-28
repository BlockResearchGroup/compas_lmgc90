import os
import numpy

from ..config.lmgc90dicts import *

#
#
#---Fichier    :  BODIES.DAT
#
#

def initBodies(chemin=''):
    """ 
    Allow to initialize the BODIES.DAT file

    :param chemin: the directory in which is the BODIES.DAT file
    """
    
    fid = open(os.path.join(chemin,'BODIES.DAT'),'w')
    fid.close()
    
    print()
    print('Start writing file\t:\tBODIES.DAT')


def inBodies_MAILx(body,chemin=''):
    """ 
    Allow to write one MAILx body in BODIES.DAT

    :param body: the MAILx avatar to write
    :param chemin: the directory in which is the BODIES.DAT file
    """

    ensElem  = body.bulks
    ensNoe   = body.nodes
    ensCont  = body.contactors
    ensGroup = body.groups
    dimension= body.dimension

    fid      = open(os.path.join(chemin,'BODIES.DAT'),'a')
    fid.write(   '$bdyty\n')
    # A modifier dans le cas des corps rigides
    fid.write(   ' MAILx%7d\n' % body.number)

    writeElementBulksInBodies(fid,ensElem,ensNoe,dimension)
    writeElementNodesInBodies(fid,ensNoe)
    writeTactyInBodies_MAILx(fid,ensCont,ensGroup,dimension)
 
    fid.write('$$$$$$\n\n')
    fid.close()


def inBodies_Rigid(body,chemin=''):
    """ 
    Allow to write one rigid body in BODIES.DAT

    :param body: the rigid avatar to write
    :param chemin: the directory in which is the BODIES.DAT file
    """

    fid  = open(os.path.join(chemin,'BODIES.DAT'),'a')
    fid.write('$bdyty\n')
    fid.write(' %5s%7d\n' % (body.atype,body.number))
    #bulks writing
    writeRigidBulksInBodies(fid,body.bulks)
    # nodes writing
    writeRigidNodesInBodies(fid,body.nodes)
    # tacts writing
    fid.write('$tacty                                                                  \n')

    # s'il y a des contacteurs a ecrire
    if len(body.contactors) != 0:
       # pour chaque contacteur
       for tact_number, tact in enumerate(body.contactors):
          # on recupere la chaine a ecrire dans le fichier
           
          # verrue pour la gestion du mirror 2D (le axis n est plus direct car symetrie mirroir)
          # if body.bulks[0].axis.shape[0] == 2 and numpy.array_equal(body.bulks[0].axis, numpy.array([[-1., 0.],[0., 1.]])):
          if body.bulks[0].axis.shape[0] == 2 and numpy.cross(body.bulks[0].axis[0],body.bulks[0].axis[1])<0.:               
            line=tact.strInBodiesFile(tact_number + 1,body.bulks[0].axis)

          else:
            line=tact.strInBodiesFile(tact_number + 1)
          # on l'ecrit dans le fichier
          fid.write(line)
    fid.write('$$$$$$\n')
    fid.close()


def writeRigidBulksInBodies(fid, bulk_container):
    """ 
    Allow to write a rigid bulk container in a file object

    :param fid: the file object opened in writing mode
    :param bulk_container: the container of rigid bulks to write
    """

    fid.write('$blmty\n')
    # on intialise le numero du prochain rigide a 1
    bulk_number=1
    # to generalize 
    for bulk in bulk_container:
        line= ' %5s  %5d  behav  %5s' % ('PLAIN',bulk_number,bulk.material.nom)
        line+=' %5s=%14.7E' % ('avrd',bulk.avrd)
        if hasattr(bulk, 'gyrd'):
            line+=' %5s=%14.7E' % ('gyrd',bulk.gyrd)
        line+='\n'
        if hasattr(bulk, 'inertia'):
            line+='                           '
            line+=' %5s=%14.7E' % ('I1  ',bulk.inertia[0])
            line+=' %5s=%14.7E' % ('I2  ',bulk.inertia[1])
            line+=' %5s=%14.7E' % ('I3  ',bulk.inertia[2])
            line+='\n'
        fid.write(line)
        # on incremente le numero du prochain rigide
        bulk_number += 1


def writeElementBulksInBodies(fid,ensElem,ensNoe,dimension):
    """ 
    Allow to write a bulk container of a MAILx avatar in a file object

    :param fid: the file object opened in writing mode
    :param ensElem: the bulk container to write
    :param ensNoe: the node containers used by ensElem
    :param dimension: the space dimension of the avatar
    """

    # on initialise l'ecriture des elements
    fid.write('$blmty\n')
    # on initialise le numero de l'element courant a 0
    ind_elem = 0
    # pour chaque element
    for elem in ensElem:
       # si l'element est compatible avec la dimension ou si l'element s'appuie sur un modele discret, ou encore si 
       # l'element n'est pas gere par LMGC90

       if ( elem.etype in dimension2writeElement[dimension] and elem.model != None) or \
           (elem.model != None and elem.model.element in discreteElements) or \
           (elem.model != None and elem.model.element.startswith('EXT')) :
          # on incremente le numero de l'element
          ind_elem = ind_elem + 1
          # on initialise a vide la chaine concernant la connectivite
          connectivite = ''
          # on initialise a 0 le compteur de noeuds dans la ligne
          # courante concernant la table de connectivote
          compteur = 0
          # pour chaque noeud de l'element
          for j in elem.connectivity:
             # on ecrit le numero du noeud sur 5 caracteres
             num = '%7d' % ensNoe[j].number
             # on incremente le compteur de noeuds dans la ligne courante
             compteur += 1
             # si le compteur depasse 8 noeuds
             if (compteur > 8):
                # on passe a la ligne suivante
                connectivite += '\n                    '
                compteur = 1
             # am: test inutile
             #if j not in ensNoe.sortedKeys():
             #   print j
             # on ajoute le numero du noeud courant a la chaine concernant
             # la connectivite
             connectivite = connectivite + num
          # on ecrit dans une meme chaine le type de l'element, son numero et sa 
          # connectivite
          #fd ligne1= ' %s%7d  nodes%s\n' % (elem.etype, ind_elem, connectivite)
          ligne1= ' %s%7d  nodes%s\n' % (elem.etype, ind_elem, connectivite)
            
          # on recupere le modele et le materiau associes a l'element
          model = elem.model.nom
          behav = elem.material.nom
            
          # on ecrit la ligne donnant le modele, et le materiau 
          ligne2 = '               model  %5s  behav  %5s\n' % (model[:5],behav[:5])
        
          # on ecrit la description de l'element dans le fichier    
          fid.write(ligne1)
          fid.write(ligne2)


def writeElementNodesInBodies(fid,ensNoe):
    """ 
    Allow to write a node container of a MAILx avatar in a file object

    :param fid: the file object opened in writing mode
    :param ensNoe: the node container to write
    """

    # on initialise l'ecriture des noeuds
    fid.write('$nodty\n')
    #for node in ensNoe:
    # pour chaque noeud de l'element (parcours dans l'ordre croissant des numeros de noeuds) 
    for n in ensNoe.sortedKeys():
       # on recupere le noeud courant
       node = ensNoe[n]
       # on ecrit dans la chaine decrivant le noeud courant son type et son numero
       ligne = ' %5s%7d                ' % (node.ntype, node.number)
       # pour chaque coordonnee du noeud
       for j in range(node.coor.size):
          # si c'est la troisieme de la ligne courante, la derniere du noeud
          if j%3 == 2 or j == node.coor.size-1 :
             # on ecrit la coordonne et on passe a la ligne suivante 
             ligne += 'coo%d=%14.7E  \n' % (j+1, node.coor[j])
          # sinon
          else:
             # on ecrit juste la coordonnee
             ligne += 'coo%d=%14.7E  ' % (j+1, node.coor[j])
          # si c'est la troisieme coordonnee de la ligne courante, mais pas
          # la derniere du noeud
          if j%3==2 and not j==node.coor.size-1 :
             # on complete avec des blancs
             ligne += '                             '
       # on ecrit la chaine decrivant le noeud
       fid.write(ligne)


def writeRigidNodesInBodies(fid,ensNoe):
    """ 
    Allow to write a node container of a rigid avatar in a file object

    :param fid: the file object opened in writing mode
    :param ensNoe: the node container to write
    """

    # on initialise l'ecriture des noeuds
    fid.write('$nodty\n')
    # on recupere les coordonnees du seul noeud du rigide (i.e. le numero 1)
    node = ensNoe[1]

    # on cree des noeuds adaptes au format du fichier

    # si c'est un rigide 2D
    if node.ntype == 'NO2xx': #Rigid 2d case
      # on ecrit un noeud a trois composantes
      ntype = 'NO3xx'
      # les deux premieres sont la position du centre d'inertie, et la troisieme
      # (pour la rotation) est nulle
      coor = numpy.zeros(3,'d')
      coor[0:2] = node.coor
    # sinon, c'est un rigide 3D
    else:                    #Rigid 3d case
      # on ecrit un noeud a six composantes
      ntype = 'NO6xx'
      # les trois premieres sont la position du centre d'inertie, et les toris 
      # autres (pour les rotations) sont nulles
      coor = numpy.zeros(6,'d')
      coor[0:3] = node.coor

    # on ecrit les noeuds comme precedemment (cf. writeElementNodesInBodies)
    ligne = ' %5s  %5d                ' % (ntype,node.number)
    for j in range(coor.size):
        if j%3==2 or j==coor.size-1 :
            ligne += 'coo%d=%14.7E  \n' % (j+1,coor[j])
        else:
            ligne += 'coo%d=%14.7E  ' % (j+1,coor[j])
        if j%3==2 and not j==coor.size-1 :
            ligne += '                             '
    fid.write(ligne)


def writeTactyInBodies_MAILx(fid,ensCont,ensGroup,dimension):
    """ 
    Allow to write a contactor container of a MAILx avatar in a file object

    :param fid: the file object opened in writing mode
    :param ensCont: the contactor container to write
    :param ensGroup: the group container used by ensCont
    :param dimension: the space dimension of the avatar
    """

    fid.write('$tacty\n')
    
    # s'il y a des contacteurs a ecrire
    if len(ensCont) != 0:
       # pour chaque contacteur
       for tact_number, tact in enumerate(ensCont):
          # on recupere la chaine a ecrire dans le fichier
          line=tact.strInBodiesFile(tact_number + 1)
          # on l'ecrit dans le fichier
          fid.write(line)

def closeBodies(chemin=''):
    """ 
    Allow to finalize the BODIES.DAT file

    :param chemin: the directory in which is the BODIES.DAT file
    """

    fid = open(os.path.join(chemin,'BODIES.DAT'),'a')    
    fid.write('      \n')
    fid.close()
    print('End of writing file\t:\tBODIES.DAT')


def writeBodies(Parts,chemin=''):
    """
    Write an avatar container into a BODIES.DAT file.

    :param Parts: an avatar container
    :param chemin: the directory in which to save BODIES.DAT file.
    """

    #
    Parts.renumber()

    # on initialise l'ecriture du fichier
    initBodies(chemin)

    for body in Parts.getRigidAvatar():
        inBodies_Rigid(body, chemin)
    for body in Parts.getFemAvatar():
        inBodies_MAILx(body, chemin)
 
    # on termine l'ecriture du fichier
    closeBodies(chemin)

