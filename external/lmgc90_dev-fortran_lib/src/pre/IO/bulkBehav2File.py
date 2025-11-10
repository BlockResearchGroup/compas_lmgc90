#
#
#
#--Fichier   : BULK_BEHAV
#
#
#
import os
import numpy

from ..utilities.error import *

def initBulkBehav(chemin='',dim=None,gravy=None):
    print()
    print('Start writing file\t:\tBULK_BEHAV.DAT')
    fid = open(os.path.join(chemin,'BULK_BEHAV.DAT'),'w')
    fid.write("""! File BEHAVIOUR

!                                                                       
! The symbol   '$'       preceeds a keyword used in scanning files.   
!                                                                       
! The symbol   'behav'   stands for the nickname of a bulk or         
! contact behaviour law, character(LEN=5).                              
!                                                                       
! The symbol   'lawty'   stands for the name of a bulk or             
! contact behaviour law, character(LEN=30).                             
!                                                                       
! The symbol   'seety'   stands for description of a candidate   
! 'cdbdy' type of body, 'cdtac' type of contactor, 'cdcol' color  
! ready to meet with the contact behaviour law 'behav' an antagonist 
! 'anbdy' type of body, 'antac' type of contactor, 'ancol' color.  
!                                                                       
! Candidate antagonist objects are considered only within some distance 
! 'alert'.                                                            
!                                                                       
! STANDARD PACKAGE of bulk or contact behaviour laws                    
!                                                                       
! 123456789012345678901234567890:                                       
!                               :                                       
! bulk behaviour                :                                       
!                               :                                       
!
                                                                        
$gravy  \n""")                                                        
    # si l'utilsateur n'a pas specifie la gravite
    if gravy is None:
       # elle prend la valeur par defaut, selon la dimension
       if dim == 2:
          gravy=[0., -9.81, 0.]
       elif dim == 3:
          gravy=[0., 0., -9.81]
       else:
          showError("when writing BULK_BEHAV file, the dim parameter must be explicitely set to 2 or 3")
    # on ecrit la ligne concernant la gravite dans le fichier
    fid.write('                   grv1=%14.7e  grv2=%14.7e  grv3=%14.7e\n' % (gravy[0], gravy[1], gravy[2]))
    # on ferme le fichier
    fid.close()



def closeBulkBehav(chemin=''):

    fid=open(os.path.join(chemin,'BULK_BEHAV.DAT'),'a')
    #fid.write('$$$$$$\n')
    fid.write('      \n')
    fid.close()
    
    print('End of file writing\t:\tBULK_BEHAV.DAT')

def writeDensity(behav):
    # la masse volumique
    
    # on genere la liste des chaines a ecrire dans le fichier
    try:
      out = ['Umas=%14.7e\n' % behav.density] 
    except TypeError:       
      out = ['Umas=%s\n' % behav.density] 
    # on renvoie la liste des chaines a ecrire dans le fichier
    return out

def writeRThermal(behav):

    if behav.anisotropy == 'isotropic':
      txt = 'iso:\n'
      coco = 'TCnd=%14.7e\n' % behav.thermal_conductivity
      sphv = 'HPse=%14.7e\n' % behav.specific_heat
      en   = 'Eth_=%14.7e  Nuth=%14.7e\n' % (behav.thermal_young,behav.thermal_nu)
      out=[txt,coco,sphv,en]
    else:  
      showError("anisotropy is not available to write THERMO_RIGID parameters: " + behav.anisotropy)      
    return out

def writeElasticity(behav):
    # on genere la liste des chaines a ecrire dans le fichier
    out = ['elas: %13s\n' % (behav.elas + (13 - len(behav.elas))*' ')] # le type de modele elastique
    # ecriture des parametres en fonction du type de modele d'elasticite
    #   * cas du modele standard :
    if behav.elas == "standard":
       # on ecrit le type d'anisotropie
       out += ['ani_: %13s\n' % (behav.anisotropy + (13 - len(behav.anisotropy))*' ')] # le type d'anisotropie
       # ecriture des parametres en fonction du type d'anisotropie
       if behav.anisotropy == 'isotropic':
          #   * cas isotrope :
          # module d'Young et coefficient de Poisson
          try: 
            out += ['EYng=%14.7e  Epss=%14.7e\n' % (behav.young, behav.nu)]
          except TypeError:
            out += ['EYng=%s  Epss=%s\n' % (behav.young, behav.nu)]              
       elif behav.anisotropy == 'orthotropic': 
          #   * cas orthotrope :
          out += ['EY11=%14.7e  EY22=%14.7e  EY33=%14.7e\n' % (behav.young[0], behav.young[1], behav.young[2]), 
                  'EP11=%14.7e  EP22=%14.7e  EP33=%14.7e\n' % (behav.nu[0], behav.nu[1],behav.nu[2]),
                  'G12_=%14.7e  G13_=%14.7e  G23_=%14.7e\n' % (behav.G[0], behav.G[1], behav.G[2])]
       else:
          #   * cas par defaut
          # on affiche un message d'erreur
          showError("anisotropy is not available to write elastic parameters: " + behav.anisotropy)      
    else:
       #   * cas par defaut
       # on affiche un message d'erreur
       showError("type of elasticity not available to write elastic parameters: " + behav.elas)
    # on renvoie la liste des chaines a ecrire dans le fichier
    return out

def writeViscosity(behav):
    # on genere la liste des chaines a ecrire dans le fichier
    out =['visc: %13s\n' % (behav.viscous_model + (13 - len(behav.viscous_model))*' ')] # le type de modele de viscosite
    # ecriture des parametres en fonction du type de modele de viscosite
    #   * cas du modele de KelvinVoigt :
    if behav.viscous_model == 'KelvinVoigt': 
       # on utilise des parametres eslastiques modifies pour calculer la matrice de viscosite
       # ecriture des parametres en fonction du type de modele d'elasticite
       #   * cas du modele standard :
       if behav.elas == "standard":
          # on ecrit le type d'anisotropie
          out += ['ani_: %13s\n' % (behav.anisotropy + (13 - len(behav.anisotropy))*' ')] # le type d'anisotropie
          # ecriture des parametres en fonction du type d'anisotropie
          if behav.anisotropy == 'isotropic': 
             #   * cas isotrope :
             out += ['EYng=%14.7e  Epss=%14.7e\n' % (behav.viscous_young, behav.viscous_nu)] #  module d'Young et coefficient de Poisson
          else:
             #   * cas par defaut
             # on affiche un message d'erreur
             showError("anisotropy not available to write Kelvin-Voigt parameters: " + behav.anisotropy)
       else:
          #   * cas par defaut
          # on affiche un message d'erreur
          showError("type of elasticity not available to write Kelvin-Voigt parameters: " + behav.elas)
    #   * cas du modele "none" :
    elif behav.viscous_model == 'none': 
       # on a plus rien a ecrire
       pass
    else:
       #   * cas par defaut
       # on affiche un message d'erreur
       showError("viscous model type not available: " + behav.viscous_model)
    # on renvoie la liste des chaines a ecrire dans le fichier
    return out

def writeThermal(behav):
    # on genere la liste des chaines a ecrire dans le fichier
    try:
        sphv = 'SPHV=%14.7e\n' % behav.specific_capacity
    except TypeError:
        sphv = 'SPHV=%s\n' % behav.specific_capacity
    try:
        coco = 'COCO=%14.7e\n' % behav.conductivity
    except TypeError:
        coco = 'COCO=%s\n'  % behav.conductivity
    out = ['ther:\n',
           sphv, # la capacite thermique specifique
           coco] # le coefficient de conductivite thermique
    
    # on renvoie la liste des chaines a ecrire dans le fichier
    return out

def writeCouplingThMec(behav):
    # on genere la liste des chaines a ecrire dans le fichier
    out = ['cplt:\n'] # mot-clef debutant la section
    # ecriture des parametres en fonction du type de modele d'elasticite
    #   * cas du modele standard :
    if behav.elas == "standard":
       # ecriture des parametres en fonction du type d'anisotropie
       if behav.anisotropy == 'isotropic': 
          #   * cas isotrope :
            out += ['Dila=%14.7e\n' % behav.dilatation] # le coefficient de dilatation thermique
       else:
          #   * cas par defaut
          # on affiche un message d'erreur
          showError("anisotropy not available to write elastic parameters: " + behav.anisotropy)
    else:
       #   * cas par defaut
       # on affiche un message d'erreur
       showError("type of elasticity not available to write elastic parameters: " + behav.elas)
    # dans tous les cas, on ecrit la temperature de reference
    out += ['Tref=%14.7e\n' % behav.T_ref_meca] # la temperature de reference
    
    # on renvoie la liste des chaines a ecrire dans le fichier
    return out

def writeCouplingHydro(behav):
    # on genere la liste des chaines a ecrire dans le fichier
    out = ['cpl_:\n'] # mot-clef debutant la section
    # ecriture des parametres en fonction du type de modele d'elasticite
    #   * cas du modele standard :
    if behav.elas == "standard":
       # ecriture des parametres en fonction du type d'anisotropie
       if behav.anisotropy == 'isotropic': 
          #   * cas isotrope :
            out += ['BIOT=%14.7e\n' % behav.hydro_cpl] # le coefficient de dilatation thermique
       else:
          #   * cas par defaut
          # on affiche un message d'erreur
          showError("anisotropy not available to write hydroscopic parameters: " + behav.anisotropy)
    else:
       #   * cas par defaut
       # on affiche un message d'erreur
       showError("type of elasticity not available to write elastic parameters: " + behav.elas)
    # dans tous les cas, on ecrit la temperature de reference
    
    # on renvoie la liste des chaines a ecrire dans le fichier
    return out

def writePlasticity(behav):
    # on genere la liste des chaines a ecrire dans le fichier
    out = ['crit: %13s\n' % (behav.critere + (13 - len(behav.critere))*' '), # le type de critere de plasticite
           'isoh: %13s\n' % (behav.isoh + (13 - len(behav.isoh))*' ')]       # le type de loi ecrouissage isotrope
    # ecriture des parametres en fonction du type de loi d'ecrouissage isotrope
    if behav.isoh == 'none':
    #   * cas du modele "none" :
       # on a rien a ecrire
       pass
    elif behav.isoh == 'linear':
    #   * cas de l'ecrouissage lineaire
       out += ['SIG0=%14.7e  K___=%14.7e\n' % (behav.iso_hard, behav.isoh_coeff)]
    else:
    #   * cas par defaut
       # on affiche un message d'erreur
       showError("type of isotropic hardening law not availalbe to write plastic parameters: " + behav.isoh)
    
    # ecriture du type d'ecrouissage cinematique
    out += ['cinh: %13s\n' % (behav.cinh + (13 - len(behav.cinh))*' ')] # type d'ecrouissage cinematique
    # ecriture des parametres en fonction du type de loi d'ecrouissage cinematique
    if behav.cinh == 'none':
    #   * cas du modele "none" :
       # on a rien a ecrire
       pass
    else:
    #   * cas par defaut
       # on affiche un message d'erreur
       showError("type of kinematic hardening law not available to write plastic parameters: " + behav.cinh)
    
    # ecriture du type de viscosite
    out += ['visc: %13s\n' % (behav.visc + (13 - len(behav.visc))*' ')] # type d'ecrouissage cinematique
    # ecriture des parametres en fonction du type de loi d'ecrouissage cinematique
    if behav.cinh == 'none':
    #   * cas du modele "none" :
       # on a rien a ecrire
       pass
    else:
    #   * cas par defaut
       # on affiche un message d'erreur
       showError("type of viscosity not available to write plastic parameters: " + behav.visc)
    
    # on renvoie la liste des chaines a ecrire dans le fichier
    return out

def writeDiscreteMaterial(behav):
    # on genere la liste des chaines a ecrire dans le fichier
    
    # selon la dimension 
    if numpy.size(behav.masses) == 2: # cas 2d
       out = ['m1  =%14.7e  m2  =%14.7e\n' % (behav.masses[0], behav.masses[1]), # les masses
              'k1  =%14.7e  k2  =%14.7e\n' % (behav.stiffnesses[0], behav.stiffnesses[1]), # les rigidites
              'c1  =%14.7e  c2  =%14.7e\n' % (behav.viscosities[0], behav.viscosities[1])] # les viscosites
    else: # cas 3d
       out = ['m1  =%14.7e  m2  =%14.7e  m3  =%14.7e\n' % (behav.masses[0], behav.masses[1], behav.masses[2]), # les masses
              'k1  =%14.7e  k2  =%14.7e  k3  =%14.7e\n' % (behav.stiffnesses[0], behav.stiffnesses[1], behav.stiffnesses[2]), # les rigidites
              'c1  =%14.7e  c2  =%14.7e  c3  =%14.7e\n' % (behav.viscosities[0], behav.viscosities[1], behav.viscosities[2])] # les viscosites
    
    # on renvoie la liste des chaines a ecrire dans le fichier
    return out

def writeJointMaterial(behav):
    # on genere la liste des chaines a ecrire dans le fichier

    behav.joint='elas_'

    if hasattr(behav,"consolidation") and hasattr(behav,"mc"): behav.joint='MC___'

    if hasattr(behav,"consolidation") and hasattr(behav,"fczm"): behav.joint='FCZM_'
    
    if numpy.size(behav.stiffnesses) == 3: # cas 2d ou 3d
       if behav.joint=='elas_' : 
         out = ['kt  =%14.7e  knc =%14.7e  knt =%14.7e\n' % (behav.stiffnesses[0], behav.stiffnesses[1], behav.stiffnesses[2])] # les rigidites

       elif behav.joint=='MC___' :     
         out = ['kt  =%14.7e  knc =%14.7e  knt =%14.7e\n' % (behav.stiffnesses[0], behav.stiffnesses[1], behav.stiffnesses[2]), # les rigidites
                'kncc=%14.7e  ec  =%14.7e\n'              % (behav.consolidation[0],behav.consolidation[1]),                    # la consolidation normale
                'ftrc=%14.7e\n'                           % (behav.mc[0]),                                                      # la resistance en traction
                'phi =%14.7e  C   =%14.7e  zmu =%14.7e\n' % (behav.mc[1],behav.mc[2],behav.mc[3])]                              # le cone de cisaillement 

       elif behav.joint=='FCZM_' :     
         out = ['kt  =%14.7e  knc =%14.7e  knt =%14.7e\n' % (behav.stiffnesses[0], behav.stiffnesses[1], behav.stiffnesses[2]), # les rigidites
                'kncc=%14.7e  ec  =%14.7e\n'              % (behav.consolidation[0],behav.consolidation[1]),                    # la consolidation normale
                'phi =%14.7e  zmu =%14.7e\n'              % (behav.fczm[0],behav.fczm[1]),                                      # le cone de cisaillement
                'pf  =%14.7e  pd  =%14.7e\n'              % (behav.fczm[2],behav.fczm[3]),                                      # couplages endos frottements
                'ct  =%14.7e  s2  =%14.7e  G2  =%14.7e\n' % (behav.fczm[4],behav.fczm[5],behav.fczm[6]),                        # mode II
                'cn  =%14.7e  s1  =%14.7e  G1  =%14.7e\n' % (behav.fczm[7],behav.fczm[8],behav.fczm[9])]                        # mode I

    else :
       ss = numpy.size(behav.stiffnesses)
       raise ValueError( f'stiffnesses must be of size 3 with JOINT material (not {ss})')
       
    # on renvoie la liste des chaines a ecrire dans le fichier
    return out

def writeMaterialFile(behav):
    # on genere la liste des chaines a ecrire dans le fichier
    out = ['%50s\n' % (behav.file_mat + (50 - len(behav.file_mat))*' ')] # le nom du fichier contenant les parametres materiaux
    
    # on renvoie la liste des chaines a ecrire dans le fichier
    return out

def inBulkBehav(behav,chemin=''):
    writeBehav = {'RIGID'       : [writeDensity,],
                  'THERMO_RIGID': [writeDensity,writeRThermal],                      
                  'DISCRETE'    : [writeDiscreteMaterial],
                  'JOINT_ELAS'  : [writeJointMaterial],
                  'JOINT_MC'    : [writeJointMaterial],
                  'JOINT_FCZM'  : [writeJointMaterial],                                    
                  'ELAS'        : [writeDensity,writeElasticity],
                  'ELAS_DILA'   : [writeDensity,writeElasticity,writeCouplingThMec],
                  'VISCO_ELAS'  : [writeDensity,writeElasticity,writeViscosity],
                  'THERMO_ELAS' : [writeDensity,writeElasticity,writeCouplingThMec,writeThermal],
                  'ELAS_PLAS'   : [writeDensity,writeElasticity,writePlasticity],
                  'USER_MAT'    : [writeDensity,writeMaterialFile],
                  'PORO_ELAS'   : [writeDensity,writeElasticity,writeThermal,writeCouplingHydro],
                  }

    # si le materiau est un materiau defini pour caracteriser les elements finis externes non geres par LMGC90
    if behav.materialType == 'EXTERNAL':
       # on ne l'ecrit pas
       return

    fid = open(os.path.join(chemin,'BULK_BEHAV.DAT'),'a')
    
    ligne='$behav  lawty\n'
    fid.write(ligne)
    ligne=' %5s  %s' % (behav.nom,behav.materialType)
    ligne+=' '*50

    try:
        for func in writeBehav[behav.materialType]:
            piece_lignes=func(behav)
            for piece_ligne in piece_lignes:
                newLigne=ligne[:40]+piece_ligne
                fid.write(newLigne)
                ligne=' '*50
    except KeyError:
        print('Material type not defined: %10s to write' % behav.materialType)
    fid.close()    


def writeBulkBehav(behavs,chemin='',dim=None,gravy=None):
     
      # on initialise l'ecriture du fichier : ecriture de la gravite
      initBulkBehav(chemin,dim,gravy)
      
      # pour chaque materiau dans le conteaneur
      for mat in sorted(behavs.keys()):
          # on ecrit le materiau dans le fichier
          inBulkBehav(behavs[mat],chemin=chemin)

      # on termine l'ecriture du fichier      
      closeBulkBehav(chemin)
