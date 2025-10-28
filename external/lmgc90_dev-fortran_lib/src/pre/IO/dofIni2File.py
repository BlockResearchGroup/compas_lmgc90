#
#
#
#--Fichier   : DOF_INI
#
#
#
# RAJOUTER LES COORDONEES DANS LE DOF.INI SOUS FORMAT  1 2 3
import os
import math
import numpy

def initDofIni(chemin=''):
    """
    Write the header of a DOF.INI

    :param chemin: the directory in which to save the file
    """

    print()
    print('Start writing file\t:\tDOF.INI')
    fid = open(os.path.join(chemin,'DOF.INI'),'w')
    fid.write('\n! DOF\n\n')
    fid.write('$steps      0                time= 0.0000000D+00\n')
    fid.write('\n!-----------------------------------------------------------------------\n')
    fid.close()


def closeDofIni(chemin=''):
    """
    Write the tailer of a DOF.INI

    :param chemin: the directory in which to save the file
    """

    fid=open(os.path.join(chemin,'DOF.INI'),'a')
    fid.write('      \n')
    fid.close()
    print('End of writing file\t:\tDOF.INI')


def inDofIni(part,chemin=''):
    """
    Write the relevant part of an avatar to a DOF.INI file

    :param part: the avatar to write
    :param chemin: the directory in which to save the file
    """

    fid = open(os.path.join(chemin,'DOF.INI'),'a')

    if part.iniDof:
        fid.write('$bdyty\n')
        # A REVOIR pour les rigides
        if part.atype == 'MAILx':
            fid.write(' %5s  %5d\n' % (part.atype,part.m_num))
        else:
            fid.write(' %5s  %7d\n' % (part.atype,part.m_num))

        # on recupere la liste des noeuds de l'avatar
        # ordonnees suivant l'ordre corissant des numeros de noeud
        listeN=part.nodes.sortedKeys()
        if part.modelType == 'THERx':
            ddlType = 'T'
            modelIni = 'THERM'
            nodty    = 'NO1xx'
            compo    = 1
        elif part.modelType == 'MECAx':
            ddlType = 'V' 
            if part.atype == 'MAILx':
                modelIni= 'MECAx'
                nodty   = 'NO'+str(part.dimension)+'xx'
            elif part.atype == 'RBDY2':
                modelIni = ''
                nodty = 'NO3xx'
            elif part.atype == 'RBDY3':
                modelIni = ''
                nodty = 'NO6xx'
        elif part.modelType == 'POROx':
            ddlType = 'V' 
            if part.atype == 'MAILx':
                modelIni= 'POROx'
                nodty   = 'NO'+str(part.dimension + 1)+'xx'
        # on dit qu'au max on a un NO dime + 2 dans le cas multi...
        elif part.modelType == 'MULTI':
            ddlType = 'V' 
            if part.atype == 'MAILx':
                modelIni= 'MULTI'
                nodty   = 'NO'+str(part.dimension)+'xx'
                nodty_d = 'NO'+str(part.dimension + 2)+'xx'

        # A REVOIR POUR LE NO1xx
        if modelIni != '':
            fid.write('$model\n')
            fid.write(' %s\n' % modelIni)
   
        fid.write('$nodty\n')
        for i in listeN:
            # ecriture des deplacements initiaux (pour des models de meca seulement)
            compo=0
            ligne=' %5s%7d                ' % (part.nodes[i].dof.ntype,i)
            if modelIni == 'MECAx' or modelIni == 'POROx' or modelIni == 'MULTI':
                for disp in part.nodes[i].dof.disp :
                    compo+=1
                    if compo ==4 :
                        ligne+='\n                             '
                    ligne+='%4s=%14.7e  ' % ('X('+str(compo)+')', disp)
                fid.write(ligne[:-2]+'\n')

            elif modelIni == '' and part.atype == 'RBDY2':
                disp = numpy.zeros( [3] )
                disp[:2] = part.nodes[i].dof.disp[:]

                # recuperation de la matrice de rotation de l'objet rigide
                for bulk in part.bulks:
                   if part.nodes[1].dof.rot is not None:
                       axis = numpy.matmul( part.nodes[1].dof.rot,bulk.axis)
                   else:
                       axis = bulk.axis
                # calcul de l'angle associe a la matrice de rotation
                # calcul de l'angle dans [0, pi]
                disp[2] = math.acos(0.5*(axis[0, 0] + axis[1, 1])) 
                # si le sinus de l'angle est negatif
                if axis[1, 0] < 0.:
                   # alors le resultat est dans [-pi, 0]
                   disp[2] = -disp[2]

                for d in disp:
                    compo+=1
                    ligne+='%4s=%14.7e  ' % ('X('+str(compo)+')', d)
                fid.write(ligne[:-2]+'\n')

            elif modelIni == '' and part.atype == 'RBDY3':
                disp = part.nodes[i].dof.disp
                xdisp = [ 'X({})={: .7e}'.format(c+1,v) for c, v in enumerate(disp) ]
                ligne += "  ".join(xdisp)
                ligne +='\n                             '
                rdisp = [ 'X({})={: .7e}'.format(c+1,0.) for c in range(3,6) ]
                ligne += "  ".join(rdisp)
                fid.write(ligne+'\n')
            # ecriture des ddl (vitesses ou temperatures, selon le modele)

            # si on considere un modele de thermique
            if part.modelType == 'THERx':
               # on doit ecrire le type et le numero du noeud, en meme temps
               # que la temperature du noeud
               ligne=' %5s%7d                ' % (part.nodes[i].dof.ntype,i)
            # si on considere un modele mecanique
            elif part.modelType == 'MECAx':
               # le type et le numero du noeud on ete ecrit sur la ligne 
               # precedente, en meme temps que le deplacements ; la ligne
               # donnant les compostantes du vecteur vitesse du noeud commence
               # par des blancs
               ligne='                             ' 
            elif part.modelType == 'POROx':
               # le type et le numero du noeud on ete ecrit sur la ligne 
               # precedente, en meme temps que le deplacements ; la ligne
               # donnant les compostantes du vecteur vitesse du noeud commence
               # par des blancs
               ligne='                             ' 
            elif part.modelType == 'MULTI':
               # on doit ecrire le type en meme temps que les dofs du noeud
               # le numero est le meme que pour les deplacements
               ligne=' %5s                       ' % (nodty_d)

            compo=0
            for valeur in  part.nodes[i].dof.values:
                compo=compo+1
                if compo ==4 :
                    ligne+='\n                             '
                ligne+='%4s=%14.7e  ' % (ddlType+'('+str(compo)+')',valeur)
            fid.write(ligne[:-2]+'\n')

        if part.atype == 'RBDY3' :
            ligne='                             ' #% (nodty,i)
            compo=0
            # recuperation de la matrice de rotation associe au rigide 3D
            for bulk in part.bulks:
                if part.nodes[1].dof.rot is not None:
                    axis = numpy.matmul( part.nodes[1].dof.rot,bulk.axis)
                else:
                    axis = bulk.axis
                bulk.axis = numpy.eye( 3 )
                part.nodes[1].dof.rot = axis
                for valeur in axis[:,0]:
                    compo=compo+1
                    ligne+='%4s=%14.7e  ' % ('a'+'('+str(compo)+')',valeur)
                fid.write(ligne[:-2]+'\n')
                ligne='                             ' #% (nodty,i)
                compo=0
                for valeur in axis[:,1]:
                    compo=compo+1
                    ligne+='%4s=%14.7e  ' % ('b'+'('+str(compo)+')',valeur)
                fid.write(ligne[:-2]+'\n')
                ligne='                             ' #% (nodty,i)
                compo=0
                for valeur in axis[:,2]:
                    compo=compo+1
                    ligne+='%4s=%14.7e  ' % ('g'+'('+str(compo)+')',valeur)
                fid.write(ligne[:-2]+'\n')

        fid.write('$$$$$$\n')

    fid.close()


def writeDofIni(parts,chemin=''):
    """
    Write the relevant part of an avatar container to a DOF.INI file

    :param part: the avatar container to write
    :param chemin: the directory in which to save the file
    """

    # on initialise l'ecriture du fichier
    initDofIni(chemin)

    for body in parts.getRigidAvatar():
       inDofIni(body, chemin=chemin)
    for body in parts.getFemAvatar():
       inDofIni(body, chemin=chemin)

    # on termine l'ecriture du fichier
    closeDofIni(chemin)
