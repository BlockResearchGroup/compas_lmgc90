#
#
#
#--Fichier   : TACT_BEHAV
#
#
#
import os

def initTactBehav(chemin=''):
    print()
    print('Start writing file\t:\tTACT_BEHAV.DAT')
    fid = open(os.path.join(chemin,'TACT_BEHAV.DAT'),'w')
    fid.write("""\n""")

    fid.close()
    
def closeTactBehav(chemin=''):
    fid=open(os.path.join(chemin,'TACT_BEHAV.DAT'),'a')
    #fid.write('$$$$$$\n')
    fid.write('      \n')
    fid.close()
    print('End of writign file\t:\tTACT_BEHAV.DAT')

def writeStatFric(tact):
    # on genere la chaine a ecrire dans le fichier
    out = 'fric=%14.7e' % tact.fric # le coefficient de frottement (statique)
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeStatFricDt(tact):
    # on genere la chaine a ecrire dans le fichier
    out = 'fric=%14.7e' % tact.fric # le coefficient de frottement (statique)
    out += '  '
    out += 'dt__=%14.7e' % tact.dt__ # le coefficient de frottement statique
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeDyStFr(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'dyfr=%14.7e' % tact.dyfr # le coefficient de frottement dynamique
    out += '  '
    out += 'stfr=%14.7e' % tact.stfr # le coefficient de frottement statique
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeCnCt(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'cn  =%14.7e' % tact.cn # la raideur normale
    out += '  '
    out += 'ct  =%14.7e' % tact.ct # la raideur tangente
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeBW(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'b   =%14.7e' % tact.b # le coefficient de viscosite
    out += '  '
    out += 'w   =%14.7e' % tact.w # l'energie de cohesion
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeSmaxW(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'smax=%14.7e' % tact.smax # la contrainte (normale?) a la rupture
    out += '  '
    out += 'w   =%14.7e' % tact.w    # l'energie de cohesion
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeDuPhi(tact):
    # on genere la chaine a ecrire dans le fichier
    out  = 'du  =%14.7e' % tact.du  # deplacement ultime
    out += '  '
    out +=  'phi =%14.7e' % tact.phi # rapport micro/macro crack
    
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeS1S2(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'S1  =%14.7e' % tact.s1 # la contrainte normale a la rupture
    out += '  '
    out += 'S2  =%14.7e' % tact.s2 # la contrainte tangente a la rupture
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeG1G2(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'G1  =%14.7e' % tact.G1 # l energie normale a la rupture
    out += '  '
    out += 'G2  =%14.7e' % tact.G2 # l energie tangente a la rupture
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeDu1Du2(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'Du1 =%14.7e' % tact.du1 # le deplacement ultime normal a la rupture
    out += '  '
    out += 'Du2 =%14.7e' % tact.du2 # le deplacement ultime tangent a la rupture
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writePhi(tact):
    # on genere la chaine a ecrire dans le fichier
    out  = 'Phi =%14.7e' % tact.phi # rapport micro/macro crack
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeEta(tact):
    # on genere la chaine a ecrire dans le fichier
    out  = 'Eta =%14.7e' % tact.eta # troncature loi expo 
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeMuEta(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'mu_g=%14.7e' % tact.mu_g # le ratio des enerigies d'endomagemments et total mu_g=G_d/_Gf
    out += '  '
    out += 'Eta =%14.7e' % tact.eta # troncature loi expo 

    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]
    
def writeSpring(tact):
    # on genere la chaine a ecrire dans le fichier
    out  = 'k1  =%14.7e' % tact.k1 # rigidite mode I  (a.k.a normale)
    out += '  '
    out += 'k2  =%14.7e' % tact.k2 # rigidite mode II (a.k.a cisaillement)    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]


def writeW(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'w   =%14.7e' % tact.w  #  l'energie de cohesion
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]


def writePP0(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'p   =%14.7e' % tact.p     # pression dans la fissure
    out += '  '
    out += 'p0  =%14.7e' % tact.p0    # 
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeD1D2(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'dp1 =%14.7e' % tact.dp1 # taille du trapeze
    out += '  '
    out += 'dp2 =%14.7e' % tact.dp2 # 
    
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeStiffness(tact):
    # on genere la chaine a ecrire dans le fichier
    out = 'F/gp=%14.7e' % tact.stiffness # la raideur du ressort
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeStiffVisco(tact):
    # on genere la chaine a ecrire dans le fichier
    out  = 'F/gp=%14.7e' % tact.stiffness # la raideur du ressort
    out += '  '
    out += 'F/sr=%14.7e' % tact.viscosity # viscosite
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeStiffprestr(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'Fstr=%14.7e' % tact.stiffness # la raideur du cable
    out += '  '
    out += 'prst=%14.7e' % tact.prestrain # la pre-contrainte dans le cable
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeVisco(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'F/sr=%14.7e' % tact.viscosity # viscosite of the cable/rod
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeFmax(tact):
    # on genere la chaine a ecrire dans le fichier
    out = 'Fmax=%14.7e' % tact.Fmax # la raideur force maximale a rupture du cable
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeG0(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'G0  =%14.7e' % tact.g0    #
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeWthk(tact):
    # on genere la chaine a ecrire dans le fichier
    out = 'Wthk=%14.7e' % tact.Wthk # distance de coupure pour l'activation de la loi IQS_DS_WET_CLB
       # i.e. la cohesion n'est acive que si le gap est inferieur a cette valeur
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeCohnCoht(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'cohn=%14.7e' % tact.cohn # la cohesion normale
    out += '  '
    out += 'coht=%14.7e' % tact.coht # la cohesion tangente
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeRestitution(tact):
    out =  'rstn=%14.7e' % tact.rstn # normal restitution
    out += '  '
    out += 'rstt=%14.7e' % tact.rstt # tangential restitution
    return [out]

#pta 20/05/2015
def writePreGapFric(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'pgap=%14.7e' % tact.pgap # la valeur du pre GAP
    out += '  '
    out += 'fric=%14.7e' % tact.fric # le coefficient de frottement
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writelslc(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'lbds=%14.7e' % tact.lambdas     # pression dans la fissure
    out += '  '
    out += 'lbdc=%14.7e' % tact.lambdac    # 
    return [out]

def writeEnu(tact):
   out =  'E   =%14.7e' % tact.E
   out += '  '
   out += 'nu  =%14.7e' % tact.nu
   return [out]

def writeToverHGtol(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'T/H =%14.7e' % tact.ToverH
    out += '  '
    out += 'gtol=%14.7e' % tact.gtol
    
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]


# </ gtn_czm
def writef0fc(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'f0  =%14.7e' % tact.f0 # la porosite initiale
    out += '  '
    out += 'fc  =%14.7e' % tact.fc # la porosite de debut de coalescence

    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writekq1(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'k   =%14.7e' % tact.k # pente de la la porosite modifiee f* = f+k(f-fc)
    out += '  '
    out += 'q1  =%14.7e' % tact.q1 # parametre de coalescence

    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeq2e(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'q2  =%14.7e' % tact.q2 # parametre de coalescence
    out += '  '
    out += 'e   =%14.7e' % tact.e # epaisseur de la bande de localisation des deformations

    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeYs0(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'Y   =%14.7e' % tact.Y   # Module d'Young
    out += '  '
    out += 's0  =%14.7e' % tact.s0 # contrainte d'ecoulement
    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeKhn(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'Kh  =%14.7e' % tact.Kh # Coefficient d'ecrouissage
    out += '  '
    out += 'n   =%14.7e' % tact.n # exposent loi d'ecrouissage

    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writefNeN(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'fN  =%14.7e' % tact.fN # fraction volumique des inclusions
    out += '  '
    out += 'eN  =%14.7e' % tact.eN # parametre de nucleation due aux deformations

    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writesN(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'sN  =%14.7e' % tact.sN    # deviation standard (parametre de nucleation)

    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

# gtn_czm />

#tosi

def writenkcoal(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'n   =%14.7e' % tact.n # 
    out += '  '
    out += 'kcoa=%14.7e' % tact.kcoal #

    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeRs0(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'R   =%14.7e' % tact.R # 
    out += '  '
    out += 's0  =%14.7e' % tact.s0 # 

    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writenGc1Gc2(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'Gc1 =%14.7e' % tact.Gc1 # 
    out += '  '
    out += 'Gc2 =%14.7e' % tact.Gc2 # 

    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writeKhdef(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'K   =%14.7e' % tact.K    # 
    out += '  '
    out += 'hdef=%14.7e' % tact.hdef # 

    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

def writen_molQ(tact):
    # on genere la chaine a ecrire dans le fichier
    out =  'nmol=%14.7e' % tact.n_mol # 
    out += '  '
    out += 'Q   =%14.7e' % tact.Q     # 

    # on renvoie le resultat sous la forme d'une liste (a un element)
    return [out]

#tosi_fin

def inTactBehav(tact,chemin=''):
    writeBehav = {'IQS_CLB'                  : [writeStatFric],
                  'IQS_CLB_g0'               : [writeStatFric],
                  'IQS_DS_CLB'               : [writeDyStFr],
                  'IQS_WET_DS_CLB'           : [writeCohnCoht,writeWthk,writeDyStFr],
                  'xQS_WET_DS_CLB'           : [writeCohnCoht,writeWthk,writeDyStFr],                  
                  'IQS_MOHR_DS_CLB'          : [writeCohnCoht,writeDyStFr],
                  'IQS_CLB_RGR'              : [writeToverHGtol,writeStatFric],                   
                  'RST_CLB'                  : [writeRestitution,writeStatFric],
                  'GAP_SGR_CLB'              : [writeStatFric],
                  'GAP_SGR_CLB_g0'           : [writeStatFric],
                  'VEL_SGR_CLB'              : [writeStatFric],
                  'preGAP_SGR_CLB'           : [writePreGapFric], #pta 20/05/2015
                  'GAP_MOHR_DS_CLB'          : [writeCohnCoht,writeDyStFr],
                  'MAC_CZM'                  : [writeDyStFr,writeCnCt,writeBW],
                  'IQS_MAC_CZM'              : [writeDyStFr,writeCnCt,writeBW],
                  'MAL_CZM'                  : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2],
                  'IQS_MAL_CZM'              : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2],
                  'ABP_CZM'                  : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeDu1Du2,writePhi],
                  'EXPO_CZM'                 : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeEta],
                  'EXPO_CZM_P'               : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeMuEta],
                  'EXPO_CZM_SPRING'          : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeEta,writeSpring],                  
                  'EXPO_CZM_SPRING_P'        : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeMuEta,writeSpring],                  
                  'IQS_ABP_CZM'              : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeDu1Du2,writePhi],
                  'IQS_EXPO_CZM'             : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeEta],    
                  'IQS_EXPO_CZM_P'           : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeMuEta],                      
                  'IQS_EXPO_CZM_SPRING'      : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeEta,writeSpring],                  
                  'IQS_EXPO_CZM_SPRING_P'    : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeMuEta,writeSpring],                  
                  'COUPLED_DOF'              : [],
                  'NORMAL_COUPLED_DOF'       : [], 
                  'ELASTIC_REPELL_CLB'       : [writeStiffness, writeStatFric],
                  'ELASTIC_REPELL_CLB_g0'    : [writeStiffness, writeStatFric],
                  'ELASTIC_REPELL_CLB_adapt' : [writeStiffness, writeStatFric],
                  'VISCO_ELASTIC_REPELL_CLB' : [writeStiffVisco, writeStatFric],
                  'ELASTIC_WIRE'             : [writeStiffprestr],
                  'BRITTLE_ELASTIC_WIRE'     : [writeStiffprestr, writeFmax],
                  'ELASTIC_ROD'              : [writeStiffprestr],
                  'VOIGT_ROD'                : [writeStiffprestr,writeVisco],
                  'MP_CZM'                   : [writeDyStFr,writeCnCt,writeW],
                  'MP3_CZM'                  : [writeDyStFr,writeCnCt,writeSmaxW],
                  'MP3_CZM_THER'             : [writeDyStFr,writeCnCt,writeSmaxW,writelslc],
                  'TH_CZM'                   : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeD1D2],
                  'IQS_TH_CZM'               : [writeDyStFr,writeCnCt,writeS1S2,writeG1G2,writeD1D2],
                  'BRITTLE_COATING_CLB'      : [writeStatFric,writeStiffness,writeFmax,writeG0],
                  'IQS_STICK'                : [],
                  'GAP_SGR_STICK'            : [],
                  'IQS_CLB_nosldt'           : [writeStatFricDt],
                  'GAP_SGR_CLB_nosldt'       : [writeStatFricDt],
                  'NARD_ROD'                 : [writeEnu,writeS1S2],
                  'GTN_CZM'                  : [writeDyStFr,writeCnCt,writef0fc,writekq1,writeq2e,writeYs0,writeKhn,writefNeN,writesN],
                  'GTN2_CZM'                 : [writeDyStFr,writeCnCt,writef0fc,writekq1,writeq2e,writeYs0,writeKhn,writefNeN,writesN],
                  'TOSI_CZM'                 : [writeDyStFr,writeCnCt,writef0fc,writenkcoal,writeRs0,writenGc1Gc2,writeKhdef,writen_molQ],
                  'TOSI_CZM_INCRE'           : [writeDyStFr,writeCnCt,writef0fc,writenkcoal,writeRs0,writenGc1Gc2,writeKhdef,writen_molQ],
                 }
    fid = open(os.path.join(chemin,'TACT_BEHAV.DAT'),'a')
  
    # A REVOIR pour les rigides
    
    ligne='$behav \n'
    fid.write(ligne)

    ligne=' %5s  %s' % (tact.nom,tact.law)
    ligne+=' '*50
    try:
      # si la loi n'a pas de parametres
      if len(writeBehav[tact.law]) == 0:
         # on ecrit seulement la ligne avec le type 
         # et le nom de la loi
         fid.write(ligne + '\n')

      for func in writeBehav[tact.law]:
          piece_lignes=func(tact)
          for piece_ligne in piece_lignes:
              newLigne=ligne[:40]+piece_ligne
              fid.write(newLigne+'\n')
              ligne=' '*50
    except KeyError:
        print('Interaction type undefined: %10s when writing' % tact.law)
    fid.write('      \n')
    fid.close()
    
def inTactSeeTy(seety,chemin=''):
  
    fid = open(os.path.join(chemin,'TACT_BEHAV.DAT'),'a')
    fid.write('$seety\n')
    fid.write(' cdbdy  cdtac  cdcol  behav  anbdy  antac  ancol       alert\n')
    # A REVOIR pour les rigides
    ligne=' %5s  %5s  %5s  %5s  %5s  %5s  %5s      %14.7e\n' % \
          (seety.CorpsCandidat,seety.candidat,seety.colorCandidat,seety.behav,
           seety.CorpsAntagoniste,seety.antagoniste,seety.colorAntagoniste,
           seety.alert)
    # si on a donne une seconde distance d'alerte (halo)
    if seety.halo != None:
       # on l'ecrit dans une ligne commencant par un +
       ligne += '+' + 53*' ' + '%14.7e\n' % seety.halo
    fid.write(ligne)
    fid.write('      \n')
    fid.close()
    
def writeTactBehav(tact,seety,chemin=''):
    # on initialise l'ecriture du fichier
    initTactBehav(chemin)
    # pour chaque loi de contact du container
    for law in sorted(tact.keys()):
       # on ecrit la loi de contact dans le fichier
       inTactBehav(tact[law],chemin)
    # pour chaque table de visibilite du container
    for see in seety:
       # on ecrit la table de visibilite dans le fichier
       inTactSeeTy(see,chemin)
    # on termine l'ecriture du fichier
    closeTactBehav(chemin)
