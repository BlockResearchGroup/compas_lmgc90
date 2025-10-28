#
#
#
# -- Fichier  :  DrvDof
#
#
#
import os

## ouverture du fichier
def initDrvDof(chemin=''):
    """
    Allows to initialize DRIVEN_DOF.DAT file
    """
    print()
    print('Start writing file\t:\tDRV_DOF.DAT')
    fid = open(os.path.join(chemin,'DRV_DOF.DAT'),'w')
    fid.write('! DOF\n')
    fid.close()
    
## fermeture du fichier
def closeDrvDof(chemin=''):
    """
    Allows to close the DRIVEN_DOF.DAT file
    """
    fid=open(os.path.join(chemin,'DRV_DOF.DAT'),'a')
    fid.write('      \n')
    fid.close()
    print('End of writing file\t:\tDRV_DOF.DAT')

     
## ecriture des ddl d un modele
#@todo utiliser les modeles pour verifier la coherence du nb de ddl bloque ...
def inDrvDof(part,chemin=''):

   """
   Allows to write a 'part' (as particle) in  DRIVEN_DOF file
 
   """
   fid = open(os.path.join(chemin,'DRV_DOF.DAT'),'a')

   # On ecrit rien si on n'a rien d'interessant a ecrire (pour les rigides) 
   if part.drvDof:
        fid.write('$bdyty\n')
        # A REVOIR pour les rigides
        fid.write(' %5s%7d\n' % (part.atype,part.number))
        if part.modelType  == 'THERx':
          modelIni = 'THERM'
          nodty    = 'NO1xx'
        #
        if part.modelType == 'MECAx':
          if part.atype == 'MAILx':
            modelIni = 'MECAx'
            if part.dimension == 2:
              nodty = 'NO2xx'
            elif part.dimension == 3:
              nodty = 'NO3xx'
          elif part.atype == 'RBDY2':
            modelIni = ''
            nodty = 'NO3xx'
          elif part.atype == 'RBDY3':
            modelIni = ''
            nodty = 'NO6xx'
        #
        if part.modelType == 'POROx':
          if part.atype == 'MAILx':
            modelIni = 'POROx'
            if part.dimension == 2:
              nodty = 'NO3xx'
            elif part.dimension == 3:
              nodty = 'NO4xx'
        if part.modelType == 'MULTI':
          if part.atype == 'MAILx':
            modelIni = 'MULTI'
            if part.dimension == 2:
              nodty = 'NO4xx'
            elif part.dimension == 3:
              nodty = 'NO5xx'
        #
        if modelIni != '':
            fid.write('$model\n')
            fid.write(' %s\n' % modelIni)
  
        # si l'avatar est un maille 
        if part.atype == 'MAILx':
            # on initialise la liste des noeuds de l'avatar
            # avec condition limite imposee a vide
            listeN = []
            # pour chaque noeud de l'avatar
            for node in part.nodes:
               # si le noeud courant a une CL imposee
               if True in node.dof.pilote:
                  # on ajoute le noeud a la liste
                  listeN.append(node.number)
            # on trie la liste
            listeN.sort()
        # sinon,
        else:
            # l'avatar est un rigide, et c'est toujours le
            # meme noeud (numero 1, le centre d'inertie) qui 
            # a une CL imposee
            listeN = [1]

        for i in listeN:
            ligne   = '$nodty\n'
            fid.write(ligne)
#fd 2024-02-22            ligne2  = ' %5s  %5s\n' % (nodty,i)
            ligne2  = ' %5s%7s\n' % (nodty,i)            
            ligne2+='$dofty  numbr [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)]...*...[RAMPI.....+.....RAMP.*.time]\n'
            fid.write(ligne2)
            #
            for k in range(len(part.nodes[i].dof.values)):
                if part.nodes[i].dof.pilote[k]:
                    if part.nodes[i].dof.driven[k].dtype == 'predefined':
                        evo = part.nodes[i].dof.driven[k]
                        ligne2 = ' %5s  %5s %14.7e %14.7e %14.7e %14.7e %14.7e %14.7e\n' % (evo.dofty,k+1,evo.ct,
                                                            evo.amp,evo.omega,evo.phi,
                                                            evo.rampi,evo.ramp)
                    elif  part.nodes[i].dof.driven[k].dtype == 'evolution':
                        evo    = part.nodes[i].dof.driven[k]
                        ligne2 = ' %5s  %5s evolution %s\n' % (evo.dofty,k+1,evo.evolutionFile)
                    fid.write(ligne2)
        fid.write('$$$$$$\n\n')
   # on ferme le fichier 
   fid.close()


## parcours de tous les modeles pour les ecrire dans le fichier DRV_DOF.DAT
def writeDrvDof(parts,chemin=''):
      """
      Allows to write allo the 'parts' (as particles) in DRIVEN_DOF.DAT file
      """
      initDrvDof(chemin)

      for part in parts.getRigidAvatar():
          if part.drvDof:
              inDrvDof(part,chemin)
      for part in parts.getFemAvatar():
          if part.drvDof:
              inDrvDof(part,chemin)

      closeDrvDof(chemin)
