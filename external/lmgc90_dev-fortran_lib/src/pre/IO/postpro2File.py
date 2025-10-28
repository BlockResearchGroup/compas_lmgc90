# module d'ecriture du fichier POSTPRO.DAT
import os

from ..avatar.avatar   import *
from ..utilities.error import *

def initPostpro(path=''):
    """initPostpro(path=''):
       this function begins POSTPRO.DAT writing, by erasing
       any previous file.   
       parameters:
          - path: path to the POSTPRO.DAT file
    """
    print()
    print('Start writing file\t:\tPOSTPRO.DAT')

    # on ouvre le fichier en ecriture, pour ecraser un
    # eventuel fichier POSTPRO.DAT existant
    fid = open(os.path.join(path,'POSTPRO.DAT'),'w')
    # on referme le fichier
    fid.close()

def closePostpro(path=''):
    """closePostpro(path=''):
       this function closes POSTPRO.DAT file, by adding
       an end of file mark.   
       parameters:
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier POSTPRO.DAT en mode ajout
    fid=open(os.path.join(path,'POSTPRO.DAT'),'a')
    # on ajoute la ligne de fin de fichier
    #          123456789012345678901234567890
    fid.write('END                           \n')
    # on ferme le fichier
    fid.close()
    # on indique la fin de l'ecriture du fichier 
    print('End of writing file\t:\tPOSTPRO.DAT')

    
def bline(grps):
    """bline([(val,fmtsize,fmttype),..,(val,fmtsize,fmttype)]):
       this function return the line needed to write "val" with format type "fmttype" and size "fmtsize" in a 30 character string.
    """
    xx=''
    vals=[]
    for grp in grps:
      val=grp[0]
      fmtsize=str(grp[1])
      fmttype=grp[2]
      xx+='%'+fmtsize+fmttype+' '
      vals.append(val)      

    xx=xx.ljust(30,' ')+'\n'
    try:
      line = xx % tuple(vals) 
    except:
      print(xx,vals)
      showError("in postproFile::bline wtf")
      
    return line 

def writeRigidList(rigid_list, avatar2body, path=''):
    """writeRigidList(rigid_list,avatar2body,path=''):
       this function writes a rigid list in the POSTPRO.DAT file.
       each line gives the corresponding indice in the BODIES.DAT file.
       parameters:
          - rigid_list: a rigid list
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file.
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

    # pour chaque avatar de la liste
    for av in rigid_list:
       # on ecrit le numero de l'indice de
       # l'avatar dans le fichier BODIES.DAT
        
       fid.write(bline([(avatar2body[av.number],7,'d')]))
       
    # on ferme le fichier POSTPRO.DAT
    fid.close()

def writeRigidSet(command, avatar2body, path=''):
    """writeRigidSet(command, avatar2body,path=''):
       this function writes a rigid set in the POSTPRO.DAT file.
       A first line gives the number of rigids and 
       the following lines give the corresponding indices in the BODIES.DAT file.
       parameters:
          - command: a postprocessing comand
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file.
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')
    
    # on ecrit le nombre d'avatar contenus dans le set

    fid.write(bline([(len(command.rigid_set),7,'d')]))
    
    # on ferme le fichier POSTPRO.DAT
    fid.close()

    # on ecrit le contenu
    writeRigidList(command.rigid_set, avatar2body, path)       

def writeRigidSets(command, avatar2body, path=''):
    """writeRigidSets(command, avatar2body,path=''):
       this function writes rigid sets in the POSTPRO.DAT file.
       A first line gives the number of rigid sets. 
       For each rigid set, a first line give the number of rigids in the set, and 
       the following lines give the corresponding indices in the BODIES.DAT file.
       parameters:
          - command: a postprocessing comand
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file.
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

    # on ecrit le nombre d'ensembles contenus dans la liste

    fid.write(bline([(len(command.rigid_sets),7,'d')]))
    
    # on ferme le fichier POSTPRO.DAT
    fid.close()
    
    # pour chaque ensemble de rigides de la liste
    for rigid_set in command.rigid_sets:
       # on appelle la routine d'ecriture d'un ensemble de corps, 
       # sur la liste de corps rigides passee en parametre de la commande

       # on ouvre le fichier en mode ajout
       fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

       # on ecrit le nombre d'avatar contenus dans le set
       fid.write(bline([(len(rigid_set),7 ,'d')]))

       # on ferme le fichier POSTPRO.DAT
       fid.close()

       # on ecrit le contenu
       writeRigidList(rigid_set, avatar2body, path)       
        

def writeMecaxSets(command, avatar2body, path=''):
    """writeMecaxSets(command, avatar2body,path=''):
       this function writes mecax sets in the POSTPRO.DAT file.
       A first line gives the number of mecax sets. For each mecax 
       set, a first line give the index of the body in the BODIES.DAT
       file and the number of nodes, and the following lines give the 
       corresponding node indices in the BODIES.DAT file.
       parameters:
          - command: a postprocessing comand
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file.
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

    # on ecrit le nombre d'ensembles contenus dans la liste

    fid.write(bline([(len(command.mecax_sets),7,'d')]))
    
    # pour chaque ensemble de la liste
    for mecax_set in command.mecax_sets:
       # on ecrit le nombre de couples (avatar, groupe) dans la liste
 
       fid.write(bline([(len(mecax_set),7,'d')]))       
        
       # pour chaque couple (avatar, groupe)
       for couple in mecax_set:
          # on recupere l'avatar et le nom du groupe
          body=couple[0]
          entity=couple[1]
          # on recupere l'ensemble des noeuds du groupe
          entity_node_set=body.groups[entity].nodes

          # on ecrit le numero de l'avatar et le 
          # nombre de noeuds dans le groupe considere

          fid.write(bline([(avatar2body[body.number],7,'d'),(len(entity_node_set),7,'d')]))
          
          # pour chaque noeud de la liste du groupe
          for nod in entity_node_set.values():
             # on ecrit le numero du noeud

             fid.write(bline([(nod.number,7,'d')]))       
              
             # #          12345678901234567890123
             # line = "%7d                       \n" % nod.number 
             # fid.write(line)       

    # on ferme le fichier POSTPRO.DAT
    fid.close()

def writeCLxxxSets(command,avatar2body,path=''):
    """writeMecaxSets(command,avatar2body,path=''):
       this function writes CLxxx sets in the POSTPRO.DAT file.
       A first line gives the total number of candidate points.
       For each candidate point, a line gives the index of the
       corresponding avatar and the index of the candidate point
       in 
       parameters:
          - command: a postprocessing comand
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file.
          - avatar2body: a dictionnary mapping an index of an 
               avatar, to the coresponding index in the 
               BODIES.DAT file
          - path: path to the POSTPRO.DAT file
    """
    # on compte le nombre total de points candidats a ecrire
  
    # on l'initialise a 0
    nb_points=0
    # pour chaque CLxxx_set
    for clxxx_set in command.CLxxx_sets:
       # on ajoute le nombre de points candidats de l'ensemble courant
       nb_points += len(clxxx_set.indices)

    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

    # on ecrit le nombre de total de points candidats
    fid.write(bline([(nb_points,7,'d')]))
    
    # pour chaque CLxxx_set
    for clxxx_set in command.CLxxx_sets:
       # on recupere l'avatar concerne par le set courant
       avatar=clxxx_set.avatar
       # on recupere l'indice de l'avatar courant dans la numerotation des corps
       body_number=avatar2body[avatar.number]

       # construction de la table qui donne le decalage de la numerotation des
       # points candidats pour un CLxxx donne

       # on l'initialise a vide
       CLxxx2offset={}       
       # on initialise a 0 l'offset courant
       offset=0
       # pour chaque contacteur de l'avatar
       for tact in avatar.contactors:
          # si l'avatar est un CLxxx
          if tact.shape == 'CLxxx':
             # on associe l'offset courant a ce contacteur
             CLxxx2offset[tact.number]=offset
          # dans tous les cas, on met a jour l'offset courant
          if tact.shape == 'ALpxx': # cas d'un ALpxx
             # on ajoute un, tous les ALxxx faisant partie d'un unique contacteur ALpxx
             offset += 1
          elif tact.shape == 'CLxxx': # cas d'un CLxxx
             # on ajoute le nombre d'elements geres par le contacteur CLxxx
             offset += len(tact.elements)
          else: # cas general
             # on affiche un message d'erreur
             showError("in postproFile::writeCLxxxSets : unsupported contactor!")

       # pour chaque point candidat
       for index in clxxx_set.indices:
          # on calcule l'indice du point candidat dans la numerotation
          # des points candidats de tout le corps
          point_number=CLxxx2offset[clxxx_set.tact.number] + index + 1

          # on ecrit l'indice de l'avatar et l'indice du point candidat considere

          fid.write(bline([(body_number,7,'d'),(point_number,7,'d')]))

    # on ferme le fichier POSTPRO.DAT
    fid.close()

def writeDoubletInteractions(command, avatar2body, path=''):
    """writeInteractionsDoublet(command, avatar2body,path=''):
       this function writes a doublet in the POSTPRO.DAT file.
       Each line gives the corresponding indices in the BODIES.DAT file.
       parameters:
          - command: a postprocessing comand
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file.
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

    # on ecrit le type d'interaction
    fid.write(bline([(command.inter_type,5,'s')]))

    fid.close()    

    # on ecrit les 2 corps concernes     
    writeRigidList(command.rigid_set, avatar2body, path)       

    
def writeDoubletsTorqueEvolution(command,avatar2body,path=''):
    """writeDoubletsTorqueEvolution(command,avatar2body,path=''):
       this function writes a doublets list in the POSTPRO.DAT file.
       A first line gives the number of doublets and the following lines
       gives the corresponding indices in the BODIES.DAT file.
       parameters:
          - command: a postprocessing comand
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file.
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

    # on ecrit le nombre de doublets contenus dans la liste

    fid.write(bline([(len(command.doublets),7,'d')]))
    
    # pour chaque paire d'avatars de la liste
    for doublet in command.doublets:
       # on ecrit les indices des
       # l'avatar dans le fichier BODIES.DAT

       fid.write(bline([(avatar2body[doublet[0].number],7,'d'),(avatar2body[doublet[1].number],7,'d')]))
        
    # on ferme le fichier POSTPRO.DAT
    fid.close()

def write_float(command,avatar2body,path=''):
    """write_float(command,avatar2body,path=''):
       this function writes command val as a float in the POSTPRO.DAT file.
       parameters:
          - command: a postprocessing command
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file ; not used here.
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

    # on ecrit le nombre de doublets contenus dans la liste
    fid.write(bline([(command.val,14.7,'f')]))
    
    # on ferme le fichier POSTPRO.DAT
    fid.close()
                 
def write_i5(command,avatar2body,path=''):
    """write_i5(command,avatar2body,path=''):
       this function writes command val as a float in the POSTPRO.DAT file.
       parameters:
          - command: a postprocessing command
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file ; not used here.
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

    # on ecrit le nombre de doublets contenus dans la liste
    fid.write(bline([(command.val,5,'d')]))
    
    # on ferme le fichier POSTPRO.DAT
    fid.close()
    
def writeAVE(command,avatar2body,path=''):
    """writeAVE(command,avatar2body,path=''):
       this function writes (A5) the color of particles in the POSTPRO.DAT file.
       parameters:
          - command: a postprocessing command
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file ; not used here.
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')
                 
    # on ecrit la couleur
    fid.write(bline([(command.color,5,'s')]))

    # on ferme le fichier POSTPRO.DAT
    fid.close()
                 
def writeDSE(command,avatar2body,path=''):
    """writeAVE(command,avatar2body,path=''):
       this function writes (A5) the color of particles in the POSTPRO.DAT file.
       parameters:
          - command: a postprocessing command
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file ; not used here.
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

    if command.skip_body is not None:
      # on ecrit l'objet a retirer 
      fid.write(bline([('INTER',5,'s'),(avatar2body[command.skip_body],7,'d')]))
    else:  
      fid.write(bline([('NO BODY TO SKIP',15,'s')]))
    # on ferme le fichier POSTPRO.DAT
    fid.close()

def writeCE(command,avatar2body,path=''):
    """writeCE(command,avatar2body,path=''):
       this function writes (12) the model of box and the list of object making the box in the POSTPRO.DAT file.
       parameters:
          - command: a postprocessing command
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file ; not used here.
          - path: path to the POSTPRO.DAT file
    """
    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

    # on ecrit le materiau a conserver: 
    fid.write(bline([(command.keep_behav,5,'s')]))
    
    # on ecrit le modele: 'SMOOTH BOX  '|'ROUGH BOX   '|'COUETTE     '|'CLUSTER BOX '|'SELECTION   '
    fid.write(bline([(command.shape,12,'s')]))
    
    # on ferme le fichier POSTPRO.DAT
    fid.close()
    
    if command.shape != 'SELECTION   ' : writeRigidList(command.rigid_set, avatar2body, path)
    
def writeTC(command,avatar2body,path=''):
    """writeTC(command,avatar2body,path=''):
       this function writes the list of object making the box in the POSTPRO.DAT file.
       parameters:
          - command: a postprocessing command
          - avatar2body: a dictionnary mapping an index of an avatar to 
                         the corresponding index in the BODIES.DAT file ; not used here.
          - path: path to the POSTPRO.DAT file
    """

    writeRigidList(command.rigid_set, avatar2body, path)           

def inPostpro(command, avatar2body, path=''):
    """inPostpro(command, path=''):
       this function writes a command in the POSTPRO.DAT file
       parameters:
          - command: a postprocessing comand
          - avatar2body: a dictionnary mapping an index of an 
               avatar, to the coresponding index in the 
               BODIES.DAT file
          - path: path to the POSTPRO.DAT file
    """
    # dictionnaire servant a definir les fonctions utilisees pour l'ecriture
    # des options des commandes dans le fichier POSTPRO.DAT
    writeCommand = {'NEW MECAx SETS'             : [writeMecaxSets],
                    'NEW RIGID SETS'             : [writeRigidSets],
                    'BODY TRACKING'              : [writeRigidSet],
                    'TORQUE EVOLUTION'           : [writeRigidSet],
                    'Fint EVOLUTION'             : [],
                    'Dep EVOLUTION'              : [],
                    'SOLVER INFORMATIONS'        : [],
                    'VIOLATION EVOLUTION'        : [],
                    'KINETIC ENERGY'             : [],
                    'DISSIPATED ENERGY'          : [],
                    'COORDINATION NUMBER'        : [],
                    'CLxxx ANALYSIS'             : [writeCLxxxSets],
                    'DOUBLETS TORQUE EVOLUTION'  : [writeDoubletsTorqueEvolution],
                    'DOUBLET INTERACTIONS'       : [writeDoubletInteractions],
                    #not yet  'QUASI SLIDING CONTACT'      : [write_float],
                    'CONTACT FORCE DISTRIBUTION' : [write_i5],                    
                    'NORMAL CONTACT DISTRIBUTION': [write_i5],
                    'DENSE SAMPLE COMPACITY'     : [writeDSE],                       
                    'DISPLAY TENSORS'            : [],
                    'AVERAGE VELOCITY EVOLUTION' : [writeAVE],
                    'DRY CONTACT NATURE'         : [],
                    'WET CONTACT NATURE'         : [],
                    'PLPLx_ANALYSIS'             : [],
                    'COMPACITY EVOLUTION'        : [writeCE],
                    'TRIAXIAL COMPACITY'         : [writeTC],
                    'PRxxx DETECTION'            : [],
                    'INTER ANALYSIS'             : [],
                    'VISIBILITY STATE'           : []}

    # on ouvre le fichier en mode ajout
    fid = open(os.path.join(path,'POSTPRO.DAT'),'a')

    # ecriture du type de commande :

    # on ecrit le type de la commande dans la ligne a ecrire
    line='%s' % command.name
    # on complete avec des espaces pour avoir une chaine de 
    # 30 caracteres
    line += (30 - len(command.name)) * ' '
    # on ajoute un caractere de fin de ligne
    line += '\n'
    # on peut alors ecrire la ligne dans le fichier
    fid.write(line) 

    # ecriture de la periode pour la commande
   
    # on ecrit le mot-clef 'STEP', suivi du nombre de 
    # pas definissant la periode pour la commande
    line='STEP %d' % command.step
    # on complete avec des espaces pour avoir une chaine de 
    # 30 caracteres
    line += (30 - len(line)) * ' '
    # on ajoute un caractere de fin de ligne
    line += '\n'
    # on peut alors ecrire la ligne dans le fichier
    fid.write(line) 
   
    # on ferme le fichier POSTPRO.DAT
    fid.close()

    # ecriture des parametres pour la commande

    # on appelle succesivement chaque fonction requise 
    # par la commande courante pour l'ecriture des options
    for func in writeCommand[command.name]:
       func(command, avatar2body, path)

def writePostpro(commands, parts, path=''):
    """writePostpro(commands, parts, path=''):
       this function write the POSTPRO.DAT file.
       parameters:
          - commands: postprocessing commands
          - parts: container of bodies, used to compute
            the index of the bodies in the BODIES.DAT file
          - path: path to the POSTPRO.DAT file
    """
    # on construit un dictionnaire donnant l'indice d'un 
    # corps dans le fichier BODIES.DAT, en fonction de son
    # indice dans le conteneur d'avatars
    # N.B.: la methode de numerotation des corps est LA MEME
    # que celle utilisee pour l'ecriture du fichier BODIES.DAT
    # (cf. bodiesFile.py)  

    # on cree le nouveau dictionnaire
    avatar2body=dict()   
    # on commence la numerotation a 1
    i_rigid=1 # pour les rigides 
    i_mailx_meca=1  # pour les mailles, avec modele de mecanique
    i_mailx_ther=1  # pour les mailles, avec modele de thermique
    # pour chaque corps du conteneur 
    for body in parts:
       # si le corps est maille
       if body.atype == 'MAILx':
          # cas d'un modele mecanique
          if body.modelType == 'MECAx':
             # on associe au numero du corps l'indice du prochain
             # corps maille, avec modele de mecanique
             avatar2body[body.number]=i_mailx_meca
             # on incremente l'indice du prochain corps maille,
             # avec modele de mecanique
             i_mailx_meca += 1
          # cas d'un modele de thermique
          if body.modelType == 'THERx':
             # on associe au numero du corps l'indice du prochain
             # corps maille, avec modele de thermique
             avatar2body[body.number]=i_mailx_ther
             # on incremente l'indice du prochain corps maille,
             # avec modele de thermique
             i_mailx_ther += 1
       # si le corps est rigide
       if body.atype == 'RBDY2' or body.atype == 'RBDY3':
          # on associe au numero du corps l'indice du prochain
          # corps rigide
          avatar2body[body.number]=i_rigid
          # on incremente l'indice du prochain corps rigide
          i_rigid += 1

    # on initalise l'ecriture du fichier POSTPRO.DAT
    initPostpro(path)
    # ecriture des commandes de post-traitements:

    #   * commandes executees avant le calcul
    # pour chaque commande de post-traitement
    for command in commands:
       # si la commande est a executer avant le calcul
       if command.name in commandsBeforeComputation:
          # on ecrit la comande dans le fichier
          inPostpro(command, avatar2body, path)

    #   * commandes pendant le calcul
    # pour chaque commande de post-traitement
    for command in commands:
       # si la commande est a executer pendant le calcul
       if not command.name in commandsBeforeComputation:
          # on ecrit la commande dans le fichier
          inPostpro(command, avatar2body, path)

    # on ferme le fichier POSTPRO.DAT
    closePostpro(path)

