#
#
#
#--Fichier   : MODELS
#
#
#
import os

from ..config.lmgc90dicts import modelOption2Keyword

def initModel(chemin=''):
    print()
    print('Start writing file\t:\tMODELS.DAT')
    fid = open(os.path.join(chemin,'MODELS.DAT'),'w')
    fid.write("""! File MODELS                                   
!
! The symbol   '$'       preceeds a keyword used in scanning files.   
!                                                           
! The symbol   'model'   stands for the nickname of a model (LEN=5).
!  
! The symbol   'mdlty'   stands for the nickname of a physical model (LEN=5), 
! meca, therm  
!    
!    
! The symbol   'finel'   stands for the nickname of a finite element used for
! the discretisation of the body for the done behaviour character(LEN=5).    
!                                                                      
! The symbol   'lawty'   stands for the name of a bulk or character(LEN=30)
!                                                                       
! The symbol   'eleop'   stands for the name of the elementary options\n
!
! STANDARD PACKAGE of bulk model options                   
!                                                                       
!                                                                      
!                                            
!                            123456789012345678901234567890\n""")

    fid.close()
    
def closeModel(chemin=''):
    fid=open(os.path.join(chemin,'MODELS.DAT'),'a')
    #fid.write('$$$$$$\n')
    fid.write('      \n')
    fid.close()
    print('End of writing file\t:\tMODELS.DAT')

def inModel(model,chemin=''):
  
    # on ouvre le fichier, en mode ajout
    fid = open(os.path.join(chemin,'MODELS.DAT'),'a')
    # on ajoute la ligne de commentaire
    fid.write('$model  mdlty  finel  eleop  value   \n')
    
    # A REVOIR pour les rigides
    
    # on commence le bloc en fonction du modele :
    if model.physics == 'THERx': # cas particulier du modele de thermique
       ligne=' %5s\n        %5s  %5s ' % (model.nom,'THERM',model.element)
    else: # cas general
       # si l'element est un ressort
       if model.element in ('SPRG2', 'SPRG3'):
          ligne=' %5s\n        %5s  %5s ' % (model.nom,model.physics,'SPRNG')
       # sinon
       else:
          ligne=' %5s\n        %5s  %5s ' % (model.nom,model.physics,model.element)

    # si le modele est "user-defined"
    if hasattr(model, 'user_model_name'):
        # on recupere le nom du modele utilisateur
        user_model_name=getattr(model, 'user_model_name')
        # on l'ecrit en premier
        ligne +=' %5s  %s' % ('u_mdl', user_model_name) + (50 -len(user_model_name))*' ' + '\n                     '

    # dans tous les cas, on ecrit les options
    for opt in sorted(model.listeOptions()):
        ligne +=' %5s  %5s\n                     ' % (modelOption2Keyword[opt], getattr(model, opt)[:6])

    # on ecrit les champs externes

    # pour chaque champ externe
    for field in model.external_fields:
       # on ecrit la ligne decrivant le champ
       ligne +=' %5s  %s' % ('extsf', field) + (30 -len(field))*' ' + '\n                     '

    for i, field in enumerate(model.external_vfields):
       # on ecrit la ligne decrivant le champ
       ligne +=' %5s  %s %5d %s' % ('extvf', field+(30 -len(field))*' ', model.external_vsizes[i], '\n                     ')

    # on saute une ligne apres le bloc    
    ligne+='\n'
    # on ecrit le bloc dans le fichier
    fid.write(ligne)
    # on ferme le fichier
    fid.close()    

def writeModels(models,chemin=''):
      # on initialise l'ecriture du fichier 
      initModel(chemin)
      # pour chaque model du container de modeles
      for mod in sorted(models.keys()):
         # on ecrit le modeles dans le fichier
         inModel(models[mod],chemin=chemin)
      # on termine l'ecriture du fichier
      closeModel(chemin)
