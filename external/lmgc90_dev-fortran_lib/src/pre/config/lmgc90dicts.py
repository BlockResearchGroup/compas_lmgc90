"""
   lmgc90 
   catalogue de correspondance et autre lien entre elements contacteurs modeles etc
"""
from ..utilities.error import *
from ..utilities.nTree import *

# definition explicite de la liste des types de discretisation geometriques disponibles
listeBodyType=('RBDY2', 'RBDY3', 'MAILx', 'MBS3D', 'MBS2D')

# dictionnaire associant element geometrique a la taille de sa connectivite (i.e. son nombre de noeuds support) 
geoAndnbNodes2Element = { 0 : { 1:'Point' },
                          1 : { 2:'S2xxx', 3:'S3xxx' },
                          2 : { 3:'T3xxx', 4:'Q4xxx', 6:'T6xxx', 8 :'Q8xxx', 9 :'Q9xxx' },
                          3 : { 4:'TE4xx', 6:'PRI6x', 8:'H8xxx', 10:'TE10x', 15:'PRI15', 20:'H20xx' },
                        }

# construction implicite de la liste des elements geometriques
#listeGeo = geo2nbNodes.keys()

# dictionnaire associant la dimension geometrique aux elements finis geometriques
# Attention: la dimension geometrique n a rien a voir avec la dimension physique on peut
#            avoir un segment (dim geometrique de 1) immerge dans un espace physique 3D.
dimension2geoElement   = {0 : ('Point',),
                          1 : ('S2xxx','S3xxx'),
                          2 : ('T3xxx','Q4xxx','T6xxx','Q8xxx','Q9xxx'),
                          3 : ('H8xxx','H20xx','TE4xx','TE10x','PRI6x','PRI15')
                         }  

# construction implicite de la table donnant la dimension d'un element geometrique
geoElement2dimension={}
# pour chaque dimension
for _dim in list(dimension2geoElement.keys()):
   # pour chaque element geometrique associe a la dimension courante
   for _geo in dimension2geoElement[_dim]:
      # on ajoute l'element courant a la table donnant la dimension d'un element geometrique
      geoElement2dimension[_geo]=_dim

# dictionnaire associant la dimension physique aux elements finis 
dimension2writeElement   = {0 : ('Point',),
                            1 : ('Point','S2xxx','S3xxx',),
                            2 : ('Point','S2xxx','S3xxx','T3xxx','Q4xxx','T6xxx','Q8xxx','Q9xxx',),
                            3 : ('Point','S2xxx','S3xxx','T3xxx','Q4xxx','T6xxx','Q8xxx','Q9xxx','H8xxx','H20xx',
                                 'TE4xx','TE10x','PRI6x','PRI15',)
                           }

# dictionnaire associant le nombre de coordonnees considere a un type de noeud
dimensionTypeNode      = {1 : 'NO1xx',
                          2 : 'NO2xx',
                          3 : 'NO3xx',
                          4 : 'NO4xx',
                          5 : 'NO5xx',
                          6 : 'NO6xx'
                         }

# dictionnaire associant a chaque type d'element geometrique les elements
# finis qui l'utilisent comme support
# TODO : donner un nom different aux elements poutre et cable, en fonction
#        de l'element geometrique support 

geo2element              = {'Point' : ('Rxx2D','Rxx3D'), 
                            'S2xxx' : ('SPRG2','SPRG3','Beam','Cable','S2xth','BARxx'),
                            #'S3xxx' : ('Beam','Cable'),
                            'S3xxx' : (),
                            'Q4xxx' : ('Q4xxx','Q4P0x','Q44xx','J2xx2',),
                            'T3xxx' : ('T3xxx','T3Lxx','DKTxx','T33xx'),
                            'Q8xxx' : ('Q8xxx','Q8Rxx','Q84xx','J3xx2',),
                            'Q9xxx' : ('Q9xxx'),
                            'T6xxx' : ('T6xxx','T63xx'),
                            'TE4xx' : ('TE4xx','TE4Lx','TE44x'),
                            'TE10x' : ('TE10x','TE104',),
                            'H8xxx' : ('H8xxx','H88xx','J4xx3',),#,'SHB8x'),
                            'H20xx' : ('H20xx','H20Rx','H208x',),#,'SHB20'),
                            'PRI6x' : ('PRI6x','SHB6x','J3xx3',),
                            'PRI15' : ('PRI15'),#,'SHB15'),
                            }

#####################################################################
#contactor

#...certains contacteurs ont besoin d'options supplementaires pour etre
#   definis correctement

contactorOptions  = {'ALpxx' : [],
                     'PT2DL' : [],
                     'PT2TL' : [],
                     'CLxxx' : [],
                     'CSpxx' : [],
                     'ASpxx' : [],
                     'DISKx' : ['byrd'],
                     'xKSID' : ['byrd'],
                     'JONCx' : ['axe1','axe2'],
                     'POLYG' : ['nb_vertices','vertices'],
                     'PT2Dx' : [],
                     'CYLND' : ['High','byrd'],
                     'DNLYC' : ['High','byrd'],
                     'PLANx' : ['axe1','axe2','axe3'],
                     'POLYR' : ['nb_vertices','nb_faces','connectivity','vertices'],
                     'PT3Dx' : [],
                     'SPHER' : ['byrd'],
                     'POLYD' : ['nb_vertices','nb_faces','connectivity'],
                     'POLYF' : ['nb_patch','patch'],
                     'POLYO' : ['nb_patch','patch'],
                     'DISKL' : ['data']
                    }
# list of real options of contactor (and not parameters)
contactorOptionalOptions  = {'ALpxx' : [],
                             'PT2DL' : ['weights'],
                             'PT2TL' : ['weights'],
                             'CLxxx' : ['weights'],
                             'CSpxx' : ['quadrature'],
                             'ASpxx' : ['unpatched'],
                             'DISKx' : [],
                             'xKSID' : [],
                             'JONCx' : [],
                             'POLYG' : [],
                             'PT2Dx' : [],
                             'CYLND' : [],
                             'DNLYC' : [],
                             'PLANx' : [],
                             'POLYR' : [],
                             'POLYO' : [],
                             'PT3Dx' : [],
                             'SPHER' : [],
                             'POLYD' : [],
                             'POLYF' : [],
                             'DISKL' : []
                            }

# definition implicite de la liste des contacteurs disponibles : un contacteur est disponible ssi les
# options qu'il accepte sont definies
listeContactor = list(contactorOptions.keys())

# definition explicite de la liste des contacteurs rigides
rigidContactor = ['DISKx', 'xKSID', 'JONCx', 'POLYG', 'PT2Dx',
                  'SPHER', 'CYLND', 'DNLYC', 'PLANx', 'POLYR', 'PT3Dx', 'POLYF', 'POLYO']

# definition implicite de la liste des elements points
# on initialise la liste a vide
pointContactor =[]
# pour chaque contacteur de la liste des contacteurs
for _tact in listeContactor:
   # si le nom du contacteur commence par 'PT'
   if _tact.startswith('PT'):
      # le contacteur est un contacteur point que l'on ajoute a la liste
      pointContactor.append(_tact)

# element au sens geometrique et pas du modele (cf element2ddl)
# am : tous les contacteurs rigides sont attaches a des points #... et le POLYD aussi
geo2contactor = {'Point'     : rigidContactor+['POLYD'],
                 'S2xxx'     : ['ALpxx','CLxxx','PT2DL','PT2TL'],
                 'T3xxx'     : ['ASpxx','CSpxx','CSpx0','CSpx1','CSpx2'],
                 'Q4xxx'     : ['ASpxx','CSpxx','CSpx0','CSpx1','CSpx2'],
                 'Q8xxx'     : ['ASpxx','CSpxx','CSpx0','CSpx1','CSpx2'],
                 'T6xxx'     : ['ASpxx','CSpxx','CSpx0','CSpx1','CSpx2'],
                }              

# definition explicite des contacteurs utilisables en fonction de la dimension physique
dimension2contactor = {2 : ('DISKx', 'xKSID', 'JONCx', 'POLYG', 'PT2Dx', # contacteurs rigides (2D) 
                            'CLxxx', 'ALpxx', 'PT2DL', 'PT2TL', 'DISKL'),# contacteurs defo    (2D)
                       3 : ('SPHER', 'CYLND', 'DNLYC', 'PLANx', 'POLYR', # contacteurs rigides (3D)
                            'PT3Dx', 'POLYF', 'POLYO',
                            'CSpxx', 'ASpxx', 'POLYD')                   # contacteurs defo    (3D)
                      }

#####################################################################

#...Permet de definir le type d options associes a un modele

modelOptions =  {'THERx' : {'capacity_storage'    : ['lump_','coher'],
                            'formulation'         : ['class','stdvf','linvf'],
                            'external_model'      : ['yes__','no___'],
                            'convection_type'     : ['supg_','cente','char_'],
                           },
                 'MECAx' : {'kinematic'        : ['small','large'],
                            'formulation'      : ['UpdtL','TotaL'],
                            'mass_storage'     : ['lump_','coher'],
                            'material'         : ['elas_','elasd','neoh_','hyper','hyp_d','J2iso','J2mix','kvisc','JELAS','J__MC','JFCZM'],
                            'anisotropy'       : ['iso__', 'ortho'],
                            #'external_model'   : ['yes__', 'no___'],
                            'external_model'   : ['MatL_', 'Demfi','Umat_','no___'],                            
                            'discrete'         : ['yes__', 'no___'],
                           },
                 'POROx' : {'kinematic'        : ['small','large'],
                            'formulation'      : ['UpdtL','TotaL'],
                            'mass_storage'     : ['lump_','coher'],
                            'capacity_storage' : ['lump_','coher'],
                            'material'         : ['elas_','elasd','neoh_','hyper','hyp_d','J2iso','J2mix','kvisc'],
                            'anisotropy'       : ['iso__', 'ortho'],
                            #'external_model'   : ['yes__', 'no___'],
                            'external_model'   : ['MatL_', 'Demfi','Umat_','no___'],
                            'discrete'         : ['yes__', 'no___'],
                            'convection_type'  : ['supg_', 'char_','center'],
                            'physical_type'    : ['fluid', 'solid'],
                           },
                 'MULTI' : {'kinematic'        : ['small','large'],
                            'formulation'      : ['UpdtL','TotaL'],
                            'mass_storage'     : ['lump_','coher'],
                            'fluid_comp_storage': ['lump_','coher'],
                            'material'         : ['elas_','elasd','neoh_','hyper','hyp_d','J2iso','J2mix','kvisc'],
                            'anisotropy'       : ['iso__', 'ortho'],
                            'external_model'   : ['yes__', 'no___'],
                            'discrete'         : ['yes__', 'no___'],
                            'convection_type'  : ['supg_', 'char_','center'],
                           }
                 }

# definition implicite de la liste des modeles disponibles : un modele est disponible ssi les
# options qu'il accepte sont definies
listeModel = list(modelOptions.keys())

#...Permet de savoir les mots cles necessaires

checkModelOptions = \
   { 'MECAx' : \
        nTree().addChilds( [ \
           nTree('kinematic').addChilds( [ \
             nTree('small').addChild( \
                nTree('anisotropy').addChilds([ \
                   nTree('iso__').addChild( \
                      nTree('material').addChilds( [ \
                         nTree('elas_'), nTree('elasd'), nTree('kvisc'), nTree('J2iso'), nTree('J2mix') \
                      ] ) \
                   ), \
                   nTree('ortho').addChild( \
                      nTree('material').addChilds( [ \
                         nTree('elas_') \
                      ] ) \
                   ) \
                ] ) \
             ), \
             nTree('large').addChild( \
                nTree('formulation').addChilds( [ \
                   nTree('TotaL').addChild( \
                      nTree('anisotropy').addChild( \
                         nTree('iso__').addChild( \
                            nTree('material').addChilds( [ \
                               nTree('neoh_'), nTree('hyper'), nTree('hyp_d'), nTree('kvisc'), nTree('J2iso') \
                            ] ) \
                         ) \
                      ) \
                   ), \
                   nTree('UpdtL')
                ] ) \
             ) \
           ] ),
           nTree('mass_storage'),
           nTree('external_model')
        ] ),
     'THERx' : \
        nTree().addChilds( [ \
           nTree('capacity_storage'),
           nTree('formulation')
        ] ),
     'POROx' : \
        nTree().addChilds( [ \
        nTree('kinematic').addChilds( [ \
             nTree('small').addChild( \
                nTree('anisotropy').addChild( \
                   nTree('iso__').addChild( \
                      nTree('material').addChilds( [ \
                         nTree('elas_') \
                      ] ) \
                   ) \
                ) \
             ), \
             nTree('large').addChild( \
                nTree('formulation').addChilds( [ \
                   nTree('TotaL').addChild( \
                      nTree('anisotropy').addChild( \
                         nTree('iso__').addChild( \
                            nTree('material').addChilds( [ \
                               nTree('neoh_') \
                            ] ) \
                         ) \
                      ) \
                   ), \
                   nTree('UpdtL')
                ] ) \
             ) \
           ] ),
           nTree('mass_storage'),
           nTree('external_model'),
           nTree('capacity_storage'),
           nTree('convection_type'),
           nTree('physical_type')
        ] ),
     'MULTI' : \
        nTree().addChilds( [ \
        nTree('kinematic').addChilds( [ \
             nTree('small').addChild( \
                nTree('anisotropy').addChild( \
                   nTree('iso__').addChild( \
                      nTree('material').addChilds( [ \
                         nTree('elas_') \
                      ] ) \
                   ) \
                ) \
             ), \
             nTree('large').addChild( \
                nTree('formulation').addChilds( [ \
                   nTree('TotaL').addChild( \
                      nTree('anisotropy').addChild( \
                         nTree('iso__').addChild( \
                            nTree('material').addChilds( [ \
                               nTree('neoh_') \
                            ] ) \
                         ) \
                      ) \
                   ), \
                   nTree('UpdtL')
                ] ) \
             ) \
           ] ),
           nTree('mass_storage'),
           nTree('fluid_comp_storage'),
           nTree('external_model'),
           nTree('convection_type'),
        ] )
   }                          

#...Permet de connaitre le nom LMGC90::MODELS.DAT de l'option

modelOption2Keyword = {'kinematic'          : 'kine_',
                       'formulation'        : 'form_',
                       'mass_storage'       : 'mstrg',
                       'material'           : 'mater',
                       'anisotropy'         : 'aniso',
                       'external_model'     : 'isext',
                       'capacity_storage'   : 'cstrg',                 
                       'fluid_comp_storage' : 'fcstr',                 
                       'discrete'           : 'discr',
                       'convection_type'    : 'adve_',
                       'physical_type'      : 'type_',              
                      }

#...Pour un type d element fini (au sens modele pas geometrique)
# on precise le nombre de ddl au noeud suivant le modele physique utilise
# 
element2ddl  =  {'Rxx2D' : {'MECAx' : 3  ,'THERx' : 1                           },
                 'S2xth' : {              'THERx' : 1                           },
                 'Rxx3D' : {'MECAx' : 6  ,'THERx' : 1                           },
                 'T3xxx' : {'MECAx' : 2  ,'THERx' : 1                           },
                 'T3Lxx' : {'MECAx' : 2  ,                                      },                 
                 'T33xx' : {                           'POROx' : 3  ,'MULTI' : 4},
                 'T6xxx' : {'MECAx' : 2  ,'THERx' : 1                           },
                 'T63xx' : {                           'POROx' : 3  ,'MULTI' : 4},
                 'Q4xxx' : {'MECAx' : 2  ,'THERx' : 1                           },
                 'Q44xx' : {                           'POROx' : 3  ,'MULTI' : 4},
                 'Q4P0x' : {'MECAx' : 2  ,'THERx' : 1                           },
                 'Q8xxx' : {'MECAx' : 2  ,'THERx' : 1                           },
                 'Q9xxx' : {'MECAx' : 2                                         },
                 'Q8Rxx' : {'MECAx' : 2  ,'THERx' : 1                           },
                 'Q84xx' : {                           'POROx' : 3  ,'MULTI' : 4},
                 'H8xxx' : {'MECAx' : 3  ,'THERx' : 1,               'MULTI' : 5},
                 'H88xx' : {                           'POROx' : 4  ,'MULTI' : 4},
                 #'SHB8x' : {'MECAx' : 3                                         },
                 'H20xx' : {'MECAx' : 3  ,'THERx' : 1                           },
                 #'SHB20' : {'MECAx' : 3                                         },
                 'H208x' : {                           'POROx' : 4  ,'MULTI' : 5},
                 'H20Rx' : {'MECAx' : 3  ,'THERx' : 1                           },
                 'TE4xx' : {'MECAx' : 3  ,'THERx' : 1                           },
                 'TE44x' : {                           'POROx' : 4  ,'MULTI' : 5},
                 'TE4Lx' : {'MECAx' : 3                                         },
                 'PRI6x' : {'MECAx' : 3  ,'THERx' : 1                           },
                 'SHB6x' : {'MECAx' : 3  ,                                      },
                 'PRI15' : {'MECAx' : 3  ,'THERx' : 1                           },
                 #'SHB15' : {'MECAx' : 3                                         },
                 'TE10x' : {'MECAx' : 3  ,'THERx' : 1                           },
                 'TE104' : {                           'POROx' : 4  ,'MULTI' : 5},
                 'DKTxx' : {'MECAx' : 4  ,'THERx' : 1                           },
                 'SPRG2' : {'MECAx' : 2  ,'THERx' : 1                           },
                 'SPRG3' : {'MECAx' : 3  ,'THERx' : 1                           },
                 'BARxx' : {'MECAx' : 3                                         },
                 'J2xx2' : {'MECAx' : 2                                         },
                 'J3xx2' : {'MECAx' : 2                                         },
                 'J3xx3' : {'MECAx' : 3                                         },
                 'J4xx3' : {'MECAx' : 3                                         },                 
                 }

# ...Liste des des types de CL disponibles

# definition explicite des types de ddl "imposables", en fonction du modele
model2dofty={'MECAx' : ('vlocy', 'force'), 'THERx' : ('temp', 'flux'),'POROx' : ('vlocy', 'force'),'MULTI' : ('prim_', 'dual_')}

# definition implicite de la liste des types de ddl "imposables"
doftys=[]
for _model in list(model2dofty.keys()):
   for _dofty in model2dofty[_model]:
      doftys.append(_dofty)

#...Liste de elements disponibles

# definition implicite de la liste des elements finis disponibles : un element fini est disponible ssi le
# nombre de degres de libertes que porte par ces neouds est defini pour chaque modele
listeElement = list(element2ddl.keys())

# definition explicite des elements finis utilisables en fonction de la dimension
dimension2element = {2 : ('Rxx2D', 'T3xxx', 'T3Lxx', 'T33xx', 'T6xxx', 'T63xx', \
                          'Q4xxx', 'Q44xx', 'Q4P0x', 'Q8xxx', 'Q8Rxx', 'Q84xx', \
                          'Q9xxx', 'SPRG2', 'S2xth', 'BARxx', 'J2xx2', 'J3xx2',),
                     3 : ('Rxx3D', 'H8xxx', 'H88xx', 'SHB8x', 'H20xx', 'H20Rx', \
                          'H208x', 'SHB20', 'TE4xx', 'TE44x', 'TE4Lx', 'PRI6x', \
                          'SHB6x', 'PRI15', 'SHB15', 'TE10x', 'TE104', 'DKTxx', \
                          'SPRG3', 'S2xth', 'BARxx', 'J3xx3', 'J4xx3')}

# definition explicite des elements finis reserves a la construction de corps rigides
rigidElements = ('Rxx2D', 'Rxx3D')

# definition explicite des elements finis discrets
discreteElements = ('SPRG2', 'SPRG3')

# definition explicite des elements finis joints
jointElements = ('J2xx2', 'J3xx2', 'J3xx3', 'J4xx3')

########################################################################################
#y
#   Definition des catalogues intervenant dans la definition 
#   des materiaux
#
#
#
#######################################################################################
#..Debut de definition des catalogues

bulkBehavOptions={'RIGID' :('density',),
                  'THERMO_RIGID' :('density','anisotropy','thermal_conductivity','specific_heat','thermal_young','thermal_nu'),
#THERMO_CHEMICAL_RIGID, CHEMICAL_RIGID, ELECTRO_RIGID, THERMO_ELECTRO_RIGID                  
                  'DISCRETE' : ('masses', 'stiffnesses', 'viscosities'),
                  'JOINT_ELAS' : ('stiffnesses',),
                  'JOINT_MC' : ('stiffnesses','consolidation','mc'),
                  'JOINT_FCZM' : ('stiffnesses','consolidation','fczm'),                  
                  'USER_MAT' :('density', 'file_mat'),
                   #materiau sans parametres defini pour caracteriser un materiau associe a un element non gere par LMGC90
                  'EXTERNAL' : (), 
                  'ELAS' : ('elas','young','nu','anisotropy','density','G'),
                  'VISCO_ELAS' : ('elas','young','nu','anisotropy','density',
                                  'viscous_model', 'viscous_young','viscous_nu',),
                  'ELAS_PLAS' : ('critere','iso_hard','isoh_coeff',
                                 'young','nu','anisotropy', 'elas',
                                 'density','isoh','cinh','visc'),
                  'ELAS_DILA' : ('elas','young','nu','anisotropy',
                                   'dilatation','T_ref_meca','density'),
                  'THERMO_ELAS' : ('elas','young','nu','anisotropy',
                                   'conductivity','dilatation',
                                   'T_ref_meca','specific_capacity','therm_cpl','density'),
                  'PORO_ELAS' : ('elas','young','nu','anisotropy',
                                 'hydro_cpl','conductivity','specific_capacity','density'),
                  }

# definition implicite de la liste des materiaux disponibles : un materiau est disponible ssi les
# options qu'il accepte sont definies
listeBulkBehav=list(bulkBehavOptions.keys())

# table qui donne une liste de valeurs predifines (assorties d'un commentaire)
# pour certaines options des materiaux
matcle2option  = {'elas' : {'standard':'standard',
                            'Hookean':'Hookeen','HartSmith':'HartSmith'},
                  'anisotropy'  : {'isotropic':'isotrope','orthotropic':'orthotrope','anisotropic':'anisotrope'},
                  'critere'     : {'Von-Mises':'critere de Von Mises','none':'aucun'},
                  # am : semble inutilise...
                  #'therm_cpl'   : {'no':'no','yes':'yes'}, 
                  #'visco_plas'  : {'0':'no','1':'yes'},
                  'isoh'        : {'none':'none','Swift':'ecrouissage de swift',
                                   'Hollomon':'ecrouissage de Hollomon','linear':'ecrouissage lineaire'},
                  'cinh'        : {'none':'none','linear':'ecrouissage lineaire'},
                  'visc'        : {'none':'none','power_law':'loi puissance'},
                  'viscous_model' : {'none': 'aucune viscosite', 'KelvinVoigt': 'modele de Kelvin-Voigt'}}

#...Permet de savoir les mots cles necessaires pour les materiaux

checkBulkBehavOptions = \
   { 'RIGID' : \
        nTree().addChild( \
           nTree('density'),
        ),
     'THERMO_RIGID': \
        nTree().addChilds( [ \
           nTree('density'),
           nTree('anisotropy').addChild( \
              nTree('isotropic').addChilds( [ \
                nTree('thermal_conductivity'),
                nTree('specific_heat'),
                nTree('thermal_young'),
                nTree('thermal_nu') ] ) \
              ) \
          ] ),   
     'DISCRETE' : \
        nTree().addChilds( [ \
           nTree('masses'),
           nTree('stiffnesses'),
           nTree('viscosities'),
        ] ),
     'JOINT_ELAS' : \
        nTree().addChilds( [ \
           nTree('stiffnesses'),
        ] ),
     'JOINT_MC' : \
        nTree().addChilds( [ \
           nTree('stiffnesses'),
           nTree('consolidation'),
           nTree('mc'),                             
        ] ),
     'JOINT_FCZM' : \
        nTree().addChilds( [ \
           nTree('stiffnesses'),
           nTree('consolidation'),
           nTree('fczm'),                             
        ] ),
     'USER_MAT' : \
        nTree().addChilds( [ \
           nTree('density'),
           nTree('file_mat') ] ),
     'EXTERNAL' : \
        nTree(),
     'ELAS' : \
        nTree().addChilds( [ \
           nTree('density'),
           nTree('elas').addChild( \
             nTree('standard').addChild( \
                nTree('anisotropy').addChilds([ \
                   nTree('isotropic').addChilds( [ \
                      nTree('young'), nTree('nu') \
                   ] ), \
                   nTree('orthotropic').addChilds( [ \
                      nTree('young'), nTree('nu'), nTree('G') \
                   ] ) \
                ] ) \
             ), \
           ),
        ] ),
     'VISCO_ELAS' : \
        nTree().addChilds( [ \
           nTree('density'),
           nTree('elas').addChild( \
             nTree('standard').addChild( \
                nTree('anisotropy').addChild( \
                   nTree('isotropic').addChilds( [ \
                      nTree('young'), nTree('nu'), \
                      nTree('viscous_model').addChilds( [ \
                        nTree('KelvinVoigt').addChilds( [ \
                           nTree('viscous_young'), nTree('viscous_nu') \
                        ] ),
                        nTree('none') 
                      ] )
                   ] ) \
                ) \
             ), \
           ) \
        ] ),
     'ELAS_PLAS' : \
        nTree().addChilds( [ \
           nTree('density'),
           nTree('elas').addChild( \
             nTree('standard').addChild( \
                nTree('anisotropy').addChild( \
                   nTree('isotropic').addChilds( [ \
                      nTree('young'), nTree('nu') \
                   ] ) \
                ) \
             ), \
           ),
           nTree('isoh').addChild( \
             nTree('linear').addChilds( [ \
                nTree('iso_hard'), nTree('isoh_coeff') \
             ] ) \
           ),
           nTree('cinh').addChild( \
              nTree('none'), \
           ),
           nTree('visc').addChild( \
              nTree('none'), \
           )
        ] ),
     'ELAS_DILA' : \
        nTree().addChilds( [ \
           nTree('density'),
           nTree('elas').addChild( \
             nTree('standard').addChild( \
                nTree('anisotropy').addChild( \
                   nTree('isotropic').addChilds( [ \
                      nTree('young'), nTree('nu'), \
                      nTree('dilatation'),
                   ] ) \
                ) \
             ), \
           ),
           nTree('T_ref_meca')
        ] ),
     'THERMO_ELAS' : \
        nTree().addChilds( [ \
           nTree('density'),
           nTree('elas').addChild( \
             nTree('standard').addChild( \
                nTree('anisotropy').addChild( \
                   nTree('isotropic').addChilds( [ \
                      nTree('young'), nTree('nu'), \
                      nTree('dilatation'),
                   ] ) \
                ) \
             ), \
           ),
           nTree('specific_capacity'),
           nTree('conductivity'),
           nTree('T_ref_meca')
        ] ),
      'PORO_ELAS' : \
        nTree().addChilds( [ \
           nTree('density'),
           nTree('elas').addChild( \
             nTree('standard').addChild( \
                nTree('anisotropy').addChild( \
                   nTree('isotropic').addChilds( [ \
                      nTree('young'), nTree('nu'),
                   ] ) \
                ) \
             ), \
           ),
           nTree('specific_capacity'),
           nTree('conductivity'),
           nTree('hydro_cpl')
        ] ),
   }                          

# table qui donne la liste des types de materiaux compatibles avec un type de 
# modele mecanique
mecaModel2bulkBehavs = {'elas_' : ('ELAS', 'VISCO_ELAS', 'ELAS_PLAS', 'ELAS_DILA', 'THERMO_ELAS','PORO_ELAS', 'MULTI_ELAS'), # modeles elastiques
                        'neoh_' : ('ELAS', 'VISCO_ELAS', 'ELAS_PLAS', 'ELAS_DILA', 'THERMO_ELAS','PORO_ELAS', 'MULTI_ELAS'),
                        'hyper' : ('ELAS', 'VISCO_ELAS', 'ELAS_PLAS', 'ELAS_DILA', 'THERMO_ELAS','PORO_ELAS', 'MULTI_ELAS'),
                        'kvisc' : ('VISCO_ELAS',), # modeles visco-elastiques
                        'J2iso' : ('ELAS_PLAS',),  # modeles elasto-plastiques
                        'J2mix' : ('ELAS_PLAS',),
                        'elasd' : ('ELAS_DILA', 'THERMO_ELAS',),  # modeles elastiques dilatant
                        'hyp_d' : ('ELAS_DILA',)
                       }    

# table qui donne la corespondance entre les valeus possibles pour l'anisotropie du modele et celles du maeriau
anisotopyFromModel2BulkBehav = {
   'iso__' : 'isotropic', # cas isotrope
   'ortho' : 'orthotropic' # cas orthotrope
}

#...fin de definition des catalogues

# am : liste conservee pour memeoire de l'ampleur de la tache...
#listeBehav   = ('IQS_CLB','IQS_CLB_g0','IQS_DS_CLB','IQS_CLB_RGR','IQS_MOHR_DS_CLB','IQS_WET_DS_CLB','IQS_SGR_CLB_WEAR',
#'IQS_CLB_nosldt','IQS_CLB_noslds',
#'IQS_MAC_CZM','IQS_MAC_CZM_3D','IQS_MAC_CZM_3D_nosldt','IQS_MAC_CZM_3D_noslds',
#'RST_CLB','RST_DS_CLB',
#'ELASTIC_REPELL_CLB','CRITICAL_VOIGT_CLB','ELASTIC_REPELL_WET_CLB',
#'ELASTIC_ROD','VOIGT_ROD', 'ELASTIC_WIRE', 'BRITTLE_ELASTIC_WIRE', 'VOIGT_WIRE',
#'TEX_SOL','TEX_SOL_UNILAT',
#'COUPLED_DOF','TANGENTIAL_COUPLED_DOF','NORMAL_COUPLED_DOF','PLASTIC_COUPLED_DOF',
#'GAP_SGR_CLB','GAP_SGR_CLB_g0','GAP_SGR_DS_CLB','GAP_WET_DS_CLB','GAP_SGR_CLB_WEAR',
#'VEL_SGR_CLB','VEL_SGR_DS_CLB',
#'MAC_CZM','Mod1_MAC_CZM','MAC_CZM_3D','MAC_CZM_3D_nosldt','MAC_CZM_3D_noslds',
#'MSMP_CZM','MAL_CZM', 'IQS_MAL_CZM',
#'MD_JKRs',
#'PERIO_DOF')

tactBehavOptions = {'IQS_CLB'                 : ['fric'],                    
                    'IQS_CLB_g0'              : ['fric'], 
                    'IQS_DS_CLB'              : ['dyfr','stfr'],
                    'IQS_CLB_RGR'             : ['fric','ToverH','gtol'],
                    'IQS_WET_DS_CLB'          : ['cohn','coht','Wthk','dyfr','stfr'],
                    'xQS_WET_DS_CLB'          : ['cohn','coht','Wthk','dyfr','stfr'],                    
                    'IQS_MOHR_DS_CLB'         : ['cohn','coht','dyfr','stfr'],
                    'IQS_STICK'               : [],
                    'IQS_CLB_nosldt'          : ['fric','dt__'],                    
                    'RST_CLB'                 : ['rstn','rstt','fric'],
                    'GAP_SGR_CLB'             : ['fric'],
                    'GAP_SGR_STICK'           : [],
                    'GAP_SGR_CLB_nosldt'      : ['fric','dt__'],
                    'preGAP_SGR_CLB'          : ['pgap','fric'], #pta 20/05/2015
                    'VEL_SGR_CLB'             : ['fric'],                    
                    'GAP_SGR_CLB_g0'          : ['fric'],
                    'GAP_MOHR_DS_CLB'         : ['cohn','coht','dyfr','stfr'],
                    'MAC_CZM'                 : ['dyfr','stfr','cn','ct','b','w'],
                    'IQS_MAC_CZM'             : ['dyfr','stfr','cn','ct','b','w'],
                    'MAL_CZM'                 : ['dyfr','stfr','cn','ct','s1','s2','G1','G2'],
                    'IQS_MAL_CZM'             : ['dyfr','stfr','cn','ct','s1','s2','G1','G2'],
                    'COUPLED_DOF'             : [],
                    'NORMAL_COUPLED_DOF'      : [],
                    'ELASTIC_REPELL_CLB'      : ['stiffness', 'fric'],
                    'ELASTIC_REPELL_CLB_g0'   : ['stiffness', 'fric'],
                    'ELASTIC_REPELL_CLB_adapt': ['stiffness', 'fric'],
                    'VISCO_ELASTIC_REPELL_CLB': ['stiffness', 'viscosity', 'fric'],
                    'ELASTIC_WIRE'            : ['stiffness', 'prestrain'],
                    'ELASTIC_ROD'             : ['stiffness', 'prestrain'],
                    'VOIGT_ROD'               : ['stiffness', 'prestrain', 'viscosity'],
                    'BRITTLE_ELASTIC_WIRE'    : ['stiffness', 'prestrain', 'Fmax'],
                    'MP_CZM'                  : ['dyfr','stfr','cn','ct','w'],
                    'MP3_CZM'                 : ['dyfr','stfr','cn','ct','smax','w'],
                    'MP3_CZM_THER'            : ['dyfr','stfr','cn','ct','smax','w','lambdas','lambdac'],
                    'TH_CZM'                  : ['dyfr','stfr','cn','ct','s1','s2','G1','G2','dp1','dp2'],
                    'IQS_TH_CZM'              : ['dyfr','stfr','cn','ct','s1','s2','G1','G2','dp1','dp2'],
                    'ABP_CZM'                 : ['dyfr','stfr','cn','ct','phi','s1','s2','G1','G2','du1','du2'],
                    'IQS_ABP_CZM'             : ['dyfr','stfr','cn','ct','phi','s1','s2','G1','G2','du1','du2'],
                    'EXPO_CZM'                : ['dyfr','stfr','cn','ct','s1','s2','G1','G2','eta'],
                    'IQS_EXPO_CZM'            : ['dyfr','stfr','cn','ct','s1','s2','G1','G2','eta'],
                    'EXPO_CZM_SPRING'         : ['dyfr','stfr','cn','ct','s1','s2','G1','G2','eta','k1','k2'],
                    'IQS_EXPO_CZM_SPRING'     : ['dyfr','stfr','cn','ct','s1','s2','G1','G2','eta','k1','k2'],
                    'BRITTLE_COATING_CLB'     : ['fric','stiffness','Fmax','g0'],
                    'NARD_ROD'                : ['E','nu','s1','s2'], # Neveu, Artoni, Richard, Descantes (doi 10.1016/j.jmps.2016.06.008)
                    'GTN_CZM'                 : ['dyfr','stfr','cn','ct','f0','fc','k','q1','q2','e','Y','s0','Kh','n','fN','eN','sN'],  
                    'GTN2_CZM'                : ['dyfr','stfr','cn','ct','f0','fc','k','q1','q2','e','Y','s0','Kh','n','fN','eN','sN'],
                    'TOSI_CZM'                : ['dyfr','stfr','cn','ct','f0','fc','n','K','R','s0','Q','Gc1','Gc2','kcoal','hdef','n_mol'],
                    'TOSI_CZM_INCRE'          : ['dyfr','stfr','cn','ct','f0','fc','n','K','R','s0','Q','Gc1','Gc2','kcoal','hdef','n_mol'],
                    'EXPO_CZM_P'              : ['dyfr','stfr','cn','ct','s1','s2','G1','G2','mu_g','eta'],
                    'IQS_EXPO_CZM_P'          : ['dyfr','stfr','cn','ct','s1','s2','G1','G2','mu_g','eta'],
                    'EXPO_CZM_SPRING_P'       : ['dyfr','stfr','cn','ct','s1','s2','G1','G2','mu_g','eta','k1','k2'],
                    'IQS_EXPO_CZM_SPRING_P'   : ['dyfr','stfr','cn','ct','s1','s2','G1','G2','mu_g','eta','k1','k2']}

# definition implicite de la liste des lois d'interaction disponibles : une loi d'interaction 
# est disponible ssi les options qu'elle accepte sont definies
listeTactBehav=list(tactBehavOptions.keys())

# definition explicite des lois d'interaction disponibles, en fonction du type de paire de contacteurs,
# au sens de paire de type d'avatars (e.g. rigid/rigid) ou de pair de type de contacteurs (e.g. point/point) :
contactorPair2TactBehav = {#   * interactions corps rigide/corps rigide
                           'rigid/rigid' : ['IQS_CLB', 'IQS_CLB_g0', 'IQS_DS_CLB',   # frottement sec
                                            'RST_CLB',                               # friction with restitution
                                            'IQS_WET_DS_CLB','xQS_WET_DS_CLB',
                                            'IQS_MOHR_DS_CLB',                       # lois cohesives
                                            'IQS_MAC_CZM', 'IQS_MAL_CZM',
                                            'IQS_TH_CZM',                            # modeles de zones cohesives
                                            'BRITTLE_COATING_CLB', 
                                            'IQS_ABP_CZM','IQS_EXPO_CZM','IQS_EXPO_CZM_SPRING','IQS_STICK',
                                            'IQS_CLB_nosldt', 'IQS_CLB_RGR','IQS_EXPO_CZM_P','IQS_EXPO_CZM_SPRING_P'],
                           # 
                           #   * interactions corps rigide/corps maille ou corps maille /corps maille
                           'any/defo'    : ['GAP_SGR_CLB', 'GAP_SGR_CLB_g0',         # frottement sec
                                            'GAP_SGR_STICK','GAP_SGR_CLB_nosldt','VEL_SGR_CLB', 
                                            'preGAP_SGR_CLB',                        #pta 20/05/2015    
                                            'MAC_CZM', 'MAL_CZM', 'GAP_MOHR_DS_CLB',
                                            'MP_CZM','MP3_CZM','MP3_CZM_THER','TH_CZM','ABP_CZM','EXPO_CZM','EXPO_CZM_SPRING',
                                            'EXPO_CZM_P','EXPO_CZM_SPRING_P',
                                            'GTN_CZM','GTN2_CZM','TOSI_CZM','TOSI_CZM_INCRE'
                                            ],  # modeles de zones cohesives

                           #   * interactions entre contacteurs points
                           'point/point' : ['ELASTIC_WIRE', 'BRITTLE_ELASTIC_WIRE',  # cables elastiques
                                            'ELASTIC_ROD' , 'VOIGT_ROD',             # barres elastiques
                                            'NARD_ROD'],                             # Neveu, Artoni, Richard, Descantes (doi 10.1016/j.jmps.2016.06.008)
                           #   * interactions pour tout type de corps et de contacteurs
                           'any/any'     : ['COUPLED_DOF',                           # saut de vitesse nul
                                            'NORMAL_COUPLED_DOF',                    # composante normale du saut de vitesse nulle
                                            'ELASTIC_REPELL_CLB',                    # loi de restitution elastique 
                                            'ELASTIC_REPELL_CLB_g0',                 # loi de restitution elastique avec gap initial
                                            'ELASTIC_REPELL_CLB_adapt',              # loi de restitution elastique avec gap initial + PTA modif
                                            'VISCO_ELASTIC_REPELL_CLB']              # loi de restitution visco-elastique
                          } 

# definition semi-implicite des contacteurs admissibles, en fonction du type
# du corps support
# N.B. certains contacteurs changent de nom pour definir les lois de contact
#      CSxx3/CSxx4 -> CSxxx, ASpx3/ASpx4 -> ASpxx, POLYD -> POLYR, POLYF -> POLYR, POLYO -> POLYR

# on definit un dictionnaire associant une liste vide a chaque type de corps
bodyType2contactor={'RBDY2': [], 'RBDY3': [], 'MAILx': [], 'MBS2D' : [], 'MBS3D' : []}

# pour chaque contacteur
for _tact in listeContactor:
   # si le contacteur est un contacteur pour un corps rigide
   if _tact in rigidContactor:
      # pour la detection du contact, un POLYF/O est un POLYR
      if _tact == 'POLYF' or _tact == 'POLYO' :
         continue

      # si le contacteur fait partie des contacteurs 2D
      if _tact in dimension2contactor[2]:
         # c'est un contacteur pour les corps rigide en 2D
         bodyType2contactor['RBDY2'].append(_tact)
         bodyType2contactor['MBS2D'].append(_tact)
      # si le contacteur fait partie des contacteurs 3D
      if _tact in dimension2contactor[3]:
         # c'est un contacteur pour les corps rigide en 3D
         bodyType2contactor['RBDY3'].append(_tact)
         bodyType2contactor['MBS3D'].append(_tact)
   # sinon, c'est un contacteur pour les mailles
   else:
      # si le contacteur ne tombe dans aucun cas particulier
      if _tact != 'POLYD' and \
        not _tact.startswith('CSxx') and \
        not _tact.startswith('CSpx') and \
        not _tact.startswith('ASpx'):
         # on l'ajoute a la liste des contacteurs pour les mailles
         bodyType2contactor['MAILx'].append(_tact) 

# on ajoute les contacteurs particuliers
bodyType2contactor['MAILx'].append('POLYR') # POLYD -> POLYR
bodyType2contactor['MAILx'].append('CSxxx') # CSxx3/CSxx4 et CSpx3/CSpx4 -> CSxxx
                                            #am: mais peut-etre un jour: CSpx3/CSpx4 -> CSpxx
bodyType2contactor['MAILx'].append('ASpxx') # ASpx3/ASpx4 -> ASpxx

# definition explicite des contacteurs admissibles pour definir les tables de visibilites
#    * les contacteurs candidats
listeCandidat=('DISKx', 'POLYG', 'PT2Dx',                               # contacteurs pour les rigides, en 2D 
               'SPHER', 'CYLND', 'POLYR', 'PT3Dx',                      # contacteurs pour les rigides, en 3D
               'CLxxx', 'CSxxx', 'PT2DL', 'PT2TL')                      # contacteurs pour les corps deformables
                                                                        #am: mais peut-etre un jour CSxxx -> CSpxx...
#    * les contacteurs antagonistes
listeAntagoniste=('DISKx', 'POLYG', 'PT2Dx', 'JONCx', 'xKSID',          # contacteurs pour les rigides, en 2D 
                  'SPHER', 'CYLND', 'POLYR', 'PT3Dx', 'PLANx', 'DNLYC', # contacteurs pour les rigides, en 3D
                  'ALpxx', 'ASpxx', 'PT2DL', 'PT2TL')                   # contacteurs pour les corps deformables
              
# options (nom des arguments) des commandes de post-traitement
commandOptions = {'NEW MECAx SETS'             : ['mecax_sets'],
                  'NEW RIGID SETS'             : ['rigid_sets'],
                  'BODY TRACKING'              : ['rigid_set'],
                  'TORQUE EVOLUTION'           : ['rigid_set'],
                  'Fint EVOLUTION'             : [],
                  'Dep EVOLUTION'              : [],
                  'SOLVER INFORMATIONS'        : [],
                  'VIOLATION EVOLUTION'        : [],
                  'KINETIC ENERGY'             : [],
                  'DISSIPATED ENERGY'          : [],
                  'COORDINATION NUMBER'        : [],
                  'CLxxx ANALYSIS'             : ['CLxxx_sets'],
                  'DOUBLETS TORQUE EVOLUTION'  : ['doublets'],
                  'DOUBLET INTERACTIONS'       : ['inter_type','rigid_set'],
                  # not yet 'QUASI SLIDING CONTACT'      : ['val'],
                  'CONTACT FORCE DISTRIBUTION' : ['val'],
                  'NORMAL CONTACT DISTRIBUTION': ['val'],
                  'DENSE SAMPLE COMPACITY'     : ['skip_body'],   
                  'DISPLAY TENSORS'            : [],
                  'AVERAGE VELOCITY EVOLUTION' : ['color'],
                  'DRY CONTACT NATURE'         : [],
                  'WET CONTACT NATURE'         : [],
                  'PLPLx ANALYSIS'             : [],
                  'COMPACITY EVOLUTION'        : ['keep_behav','shape','rigid_set'],
                  'TRIAXIAL COMPACITY'         : ['rigid_set'],
                  'PRxxx DETECTION'            : [],
                  'INTER ANALYSIS'             : [],
                  'VISIBILITY STATE'           : []}

# definition implicite de la liste des commandes de post-traitement disponibles : une commande de post-traitement 
# est disponible ssi les options qu'elle accepte sont definies
commandList=list(commandOptions.keys())

# definition explicite de la liste des commandes de post-traitement appelees avant le calcul
commandsBeforeComputation = ('NEW MECAx SETS', 'NEW RIGID SETS')

#
# definition de fonctions auxiliaires pour la verification des structures permettant de verifier que
# les options obligatoires ont bien toutes ete donnees
#

# fonction qui verifie la presence d'options obligatoires pour le modele, prises dans un sous-ensemble 
# donne
def _check_model_options(model_type, tree, root_type, option=None):
   # on declare que la fonction a le droit de modifier le nombre d'erreurs survenues lors de la verification de la
   # coherence du module
   global nb_errors

   # si la racine de l'arbre est une option
   if root_type == "option":
      # on recupere l'option dans a la racine de l'arbre
      option = tree.root
      # si l'option courante n'a pas ete definie dans la liste des options du modele
      if not option in list(modelOptions[model_type].keys()):
         # on incremente le nombre d'erreurs
         nb_errors += 1
         # on affiche un message d'erreur
         showError("l'option : \"" + option + "\" n'est pas definie pour un modele de type \"" + model_type + "\".")
      # sinon,
      else:
         # on continue la verification par les arbres fils
         for child in tree.childs:
            _check_model_options(model_type, child, "value", option=option)
   # sinon, si la racine de l'arbre est une valeur
   elif root_type == "value":
      # on recupere la valeur de l'option
      value = tree.root
      # si la valeur de l'option n'est pas definie
      if not value in modelOptions[model_type][option]: 
         # on incremente le nombre d'erreurs
         nb_errors += 1
         # on affiche un message d'erreur
         showError("la valeur : \"" + value + "\" n'est pas definie pour l'option \"" + option + "\".")
      # pour chaque fils de l'arbre
      for child in tree.childs:
         # on poursuit la verification avec les options suivantes
         _check_model_options(model_type, child, "option") 
   # sinon,
   else:
      # on affiche un message d'erreur (en faisant un sys.exit)
      setStopMode("standard")
      showError("unknow root type!")

# fonction qui verifie la presence d'options obligatoires pour un materiau, prises dans un sous-ensemble 
# donne
def _check_bulk_options(bulk_type, tree, root_type, option=None):
   # on declare que la fonction a le droit de modifier le nombre d'erreurs survenues lors de la verification de la
   # coherence du module
   global nb_errors

   # si la racine de l'arbre est une option
   if root_type == "option":
      # on recupere l'option dans a la racine de l'arbre
      option = tree.root
      # si l'option courante n'a pas ete definie dans la liste des options du materiau
      if not option in bulkBehavOptions[bulk_type]:
         # on incremente le nombre d'erreurs
         nb_errors += 1
         # on affiche un message d'erreur
         showError("l'option : \"" + option + "\" n'est pas definie pour un materiau de type \"" + bulk_type + "\".")
      # sinon,
      else:
         # on continue la verification par les arbres fils
         for child in tree.childs:
            _check_bulk_options(bulk_type, child, "value", option=option)
   # sinon, si la racine de l'arbre est une valeur
   elif root_type == "value":
      # on recupere la valeur de l'option
      value = tree.root
      # si la valeur de l'option n'est pas definie
      if not value in list(matcle2option[option].keys()): 
         # on incremente le nombre d'erreurs
         nb_errors += 1
         # on affiche un message d'erreur
         showError("la valeur : \"" + value + "\" n'est pas definie pour l'option \"" + option + "\".")
      # pour chaque fils de l'arbre
      for child in tree.childs:
         # on poursuit la verification avec les options suivantes
         _check_bulk_options(bulk_type, child, "option") 
   # sinon,
   else:
      # on affiche un message d'erreur (en faisant un sys.exit)
      setStopMode("standard")
      showError("unknow root type!")

#######################################################################
#
#  Test de la coherence du module
#
if __name__=='__main__':
   # pour la verification, on ne veut pas que les messages d'erreurs soient bloquants
   setStopMode("pass")

   # compteur d'erreurs
   nb_errors=0
   # compteur de wrnings
   nb_warnings=0

   ##
   ## verification des elements geometriques :
   ##

   # on construit l'ensemble des elements geometriques
   setGeoElement=frozenset(listeGeo)

   # on construit l'union des listes d'elements geometriques tries selon

   # on contruit la table qui donne la liste des elements geometriques, supports d'elements finis,
   # associes a une dimension sous la forme d'un dictionnaire d'ensembles

   # on l'initialise a vide
   setDim2GeoElement={}
   # pour chaque dimension
   for dim_ in list(dimension2geoElement.keys()):
      # on ajoute l'ensemble des elements geometriques associes a cette dimension
      # a la table
      setDim2GeoElement[dim_]=frozenset(dimension2geoElement[dim_])

   ### on verifie que la table donnant la liste des elements geometriques supports d'elements finis associes 
   ### a une dimension est une partition de l'ensemble des elements geometriques

   # on verifie que l'intersection des ensembles pris 2 a 2 est nulle
   for dim1 in list(setDim2GeoElement.keys()):
      for dim2 in list(setDim2GeoElement.keys()):
         if dim1 != dim2:
            if len(setDim2GeoElement[dim1].intersection(setDim2GeoElement[dim2])) != 0:
               # on incrmente le nombre d'erreurs
               nb_errors += 1
               # on affiche un message d'erreur
               showError(str(dim1) + " and " + str(dim2) + " share a geometric element!")

   # on verifie que l'union de tous les ensembles donne l'esemble de tous les elements geometriques
   
   # on calcule l'union des tous les ensembles
   # on l'initialise a l'ensemble vide
   unionDim2GeoElements=set()
   # pour chaque dimension
   for dim_ in list(setDim2GeoElement.keys()):
      # on ajoute l'ensemble des elements geometriques supports d'elements finis associes a la dimension courante a l'union
      unionDim2GeoElements=unionDim2GeoElements.union(setDim2GeoElement[dim_])

   # si l'union de tous les ensembles ne donne pas l'ensemble de tous les elements geometriques
   if unionDim2GeoElements != setGeoElement:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least a geometrical element is not associated to any dimension!")

   ###
   ### on verifie qu'une liste d'elements finis (eventuellement vide) est associee a chaque type d'element geometrique
   ###

   # on construit l'ensemble des elements geometriques pour lesquels une liste d'elements finis est definie
   setGeoElementsAssociated2Elements=frozenset(list(geo2element.keys()))

   # si cet ensemble n'est pas egal a l'ensemble de tous les elements geometriques
   if setGeoElementsAssociated2Elements != setGeoElement:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least a geometrical element is not associated to any list of finite elements!")

   ###
   ### on verifie que les elements geometriques auxquels sont associes des contacteurs font partie de la liste des
   ### elements geometriques
   ###

   # on construit l'ensemble des elements geometriques pour lesquels une liste de contatceurs est definie
   setGeoElementsAssociated2Contactors=frozenset(list(geo2contactor.keys()))
   
   # si cet ensemble n'est pas inclus dans l'ensemble de tous les elements geometriques
   if not setGeoElementsAssociated2Contactors.issubset(setGeoElement):
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least a list of contactors is associated to a ill defined geometrical element!")

   ##
   ## verification des contacteurs :
   ##

   # on contruit l'ensemble
   #    * de tous les contacteurs
   setContactor=frozenset(listeContactor)
   #    * des contacteurs rigides
   setRigidContactor=frozenset(rigidContactor)

   # on contruit la table qui donne la liste des contacteurs associes a un element
   # geometrique sous la forme d'un dictionnaire d'ensembles

   # on l'initialise a vide
   setGeo2Contactor={}
   # pour chaque type d'element geometrique
   for geo in list(geo2contactor.keys()):
      # on ajoute l'ensemble des contacteurs associes a ce type element geometrique
      # a la table
      setGeo2Contactor[geo]=frozenset(geo2contactor[geo])

   # on contruit la table qui donne la liste des contacteurs associes a une dimension 
   # sous la forme d'un dictionnaire d'ensembles
   setDimension2Contactor={2: frozenset(dimension2contactor[2]),
                           3: frozenset(dimension2contactor[3])} 

   ### on verifie que l'ensemble des contacteurs rigides est inclus dans l'ensemble de tous les contacteurs
   
   # si ce n'est pas le cas
   if not setRigidContactor.issubset(setContactor):
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least one rigid contactor is not a contactor!")

   ### on verifie que la table donnant la liste des contacteur associes a un type d'element geometrique
   ### est une partition de l'ensemble des contacteurs

   # on verifie que l'intersection des ensembles pris 2 a 2 est nulle
   for geo1 in list(setGeo2Contactor.keys()):
      for geo2 in list(setGeo2Contactor.keys()):
         if geo1 != geo2:
            if len(setGeo2Contactor[geo1].intersection(setGeo2Contactor[geo2])) != 0:
               # on incrmente le nombre d'erreurs
               nb_errors += 1
               # on affiche un message d'erreur
               showError(geo1 + " and " + geo2 + " share a contactor!")

   # on verifie que l'union de tous les ensembles donne l'esemble de tous les contacteurs
   
   # on calcule l'union des tous les ensembles
   # on l'initialise a l'ensemble vide
   unionGeo2Contactors=set()
   # pour chaque element geometrique
   for geo in list(setGeo2Contactor.keys()):
      # on ajoute l'ensemble des contacteurs associes a l'element geometrique courant a l'union
      unionGeo2Contactors=unionGeo2Contactors.union(setGeo2Contactor[geo])

   # si l'union de tous les ensembles ne donne pas l'ensemble de tous les contacteurs
   if unionGeo2Contactors != setContactor:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least a contactor is not associated to an geometrical element!")

   ### on verifie que la table donnant la liste des contacteur associes a une dimension est une partition
   ### de l'ensemble des contacteurs

   # on verifie que l'intersection des deux ensembles est nulle
   if len(setDimension2Contactor[2].intersection(setDimension2Contactor[3])) != 0:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least one contactor is defined in 2D and 3D!")

   # on verifie que l'union des deux ensembles donne l'esemble de tous les contacteurs
   
   # si l'union des deux ensembles ne donne pas l'ensemble de tous les contacteurs
   if setDimension2Contactor[2].union(setDimension2Contactor[3]) != setContactor:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least a contactor is not associated to any dimension!")

   ##
   ## verification des elements finis :
   ##

   # on contruit l'ensemble
   #    * des modeles
   setModel=frozenset(listeModel)
   #    * de tous les elements finis
   setElement=frozenset(listeElement)
   #    * des elements finis rigides
   setRigidElements=frozenset(rigidElements)
   #    * des elements finis discrets
   setDiscreteElements=frozenset(discreteElements)

   # on contruit la table qui donne la liste des elements finis associes a un element
   # geometrique sous la forme d'un dictionnaire d'ensembles

   # on l'initialise a vide
   setGeo2Element={}
   # pour chaque type d'element geometrique
   for geo in list(geo2element.keys()):
      # on ajoute l'ensemble des elements finis associes a ce type element geometrique
      # a la table
      setGeo2Element[geo]=frozenset(geo2element[geo])

   ### on verifie que l'intersection des ensembles d'elements finis (fonction de
   ### la gemoetrie) pris 2 a 2 est nulle
   for geo1 in list(setGeo2Element.keys()):
      for geo2 in list(setGeo2Element.keys()):
         if geo1 != geo2:
            if len(setGeo2Element[geo1].intersection(setGeo2Element[geo2])) != 0:
               # on incrmente le nombre d'erreurs
               nb_errors += 1
               # on affiche un message d'erreur
               showError(geo1 + " and " + geo2 + " share an element!")

   ### on verifie que tous les elements finis sont supportes par un element
   ### geometrique

   # on calcul l'ensemble des elements qui ont un support geometrique
   # on l'initialise a l'ensemble vide
   unionGeo2Element=set()
   # pour chaque element geometrique
   for geo in list(setGeo2Element.keys()):
      # on ajoute l'ensemble des elements finis associes a l'element geometrique courant a l'union
      unionGeo2Element=unionGeo2Element.union(setGeo2Element[geo])

   # si l'ensemble de tous les elements finis n'est pas inclus dans l'ensemble
   # des elements finis qui ont un support geometrique
   if not setElement.issubset(unionGeo2Element):
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least a finite element is not associated to any geometrical element!")

   ### on verifie que chaque element fini definit le nombre de degres de liberte
   ### pour chaque modele

   # pour chaque element
   for ele in listeElement:
      # on construit l'ensemble des modeles pour lesquels un nombre de ddl est
      # defini
      setDefinedModels=frozenset(list(element2ddl[ele].keys()))
      # si cet ensemble n'est pas egal a l'ensemble des modeles
      if setDefinedModels != setModel:
         # on active un nouveau warning
         nb_warnings += 1
         showWarning("at least a model is not handled by element : " + ele + "!")

   ### on verifie que l'ensemble des elements finis rigides est inclus dans l'ensemble de tous les elements finis
   
   # si ce n'est pas le cas
   if not setRigidElements.issubset(setElement):
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least one rigid element is not an element!")

   ### on verifie que l'ensemble des elements finis rigides est inclus dans l'ensemble de tous les elements finis
   
   # si ce n'est pas le cas
   if not setDiscreteElements.issubset(setElement):
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least one discrete element is not an element!")

   # on contruit la table qui donne la liste des elements finis associes a une dimension 
   # sous la forme d'un dictionnaire d'ensembles
   setDimension2Element={2: frozenset(dimension2element[2]),
                         3: frozenset(dimension2element[3])} 

   ### on verifie que la table donnant la liste des elements finis associes a une dimension est une partition
   ### de l'ensemble des elements finis

   # on verifie que l'intersection des deux ensembles est nulle
   if len(setDimension2Element[2].intersection(setDimension2Element[3])) != 0:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least one element is defined in 2D and 3D!")

   ###  on verifie que l'union des deux ensembles donne l'esemble de tous les elements finis
   
   # si l'union des deux ensembles ne donne pas l'ensemble de tous les elements
   if setDimension2Element[2].union(setDimension2Element[3]) != setElement:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least an element is not associated to any dimension!")

   ##
   ## verification de la coherence de la structure de donnees qui permet de verifier les types de CL
   ##

   # on construit l'ensemble des modeles connus de la srtucture de verification des types de CL
   setDefinedModels=frozenset(list(model2dofty.keys()))

   # si cet ensemble n'est pas egal a l'ensemble des modeles
   if setDefinedModels != setModel:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un meessage d'erreur
      showError("at least one model is not associated to any dofty!")

   ##
   ## verification de la coherence de la structure de donnees qui permet de verifier les options des modeles
   ##

   # on construit l'ensemble des modeles connus de la srtucture de verification des options des modeles 
   setCheckedModels=frozenset(list(checkModelOptions.keys()))

   # si cet ensemble n'est pas egal a l'ensemble des modeles
   if setCheckedModels != setModel:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un meessage d'erreur
      showError("at least one model cannot be checked!")

   # pour chaque modele gere par la structure de verification des options des modeles
   for model_type in list(checkModelOptions.keys()):
      # on commence la verification a partir des fils de l'arbre associe au type du modele, qui sont
      # tous des options
      for child in checkModelOptions[model_type].childs:
         _check_model_options(model_type, child, "option")

   ##
   ## verification de l'ecriture des modeles
   ##

   # on construit l'ensemble des options des modeles
   # on l'initialise a l'ensemble vide
   setModelOptions=set()
   # pour chaque modele
   for mod in listeModel:
      # on ajoute la liste des options du modele courant l'ensemble des
      # options des modeles
      setModelOptions=setModelOptions.union( frozenset(modelOptions[mod]) )

   # on construit l'ensemble des options pour lesquelles la chaine a ecrire 
   # dans le fichier MODELS.DAT a ete definie
   setModelOption2Keyword=frozenset(list(modelOption2Keyword.keys()))

   ### on verifie que l'ensemble des options pour lesquelles la chaine a ecrire
   ### dans le fichier MODELS.DATa ete define est inclus dans l'ensemble des
   ### options des modeles

   # si ce n'est pas le cas
   if not setModelOption2Keyword.issubset(setModelOptions): 
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un meessage d'erreur
      showError("at least one option of a model can be written in MODELS.DAT but is not associated to any model!")

   ##
   ## verification des materiaux
   ##

   # on construit l'ensemble des options des materiaux
   # on l'initialise a l'ensemble vide
   setBulkBehavOptions=set()
   # pour chaque materiau
   for mat in listeBulkBehav:
      # on ajoute la liste des options du materiau courant l'enesemble des
      # options des materiaux
      setBulkBehavOptions=setBulkBehavOptions.union( frozenset(bulkBehavOptions[mat]) )

   # on construit l'ensemble des options dont les valeurs sont fixees
   setGivenBulkBehavOptions=frozenset(list(matcle2option.keys()))

   ### on verifie que l'ensemble des options dont les valeurs sont fixees est inclus dans
   ### l'ensemble des options des materiaux

   # si l'ensemble des options dont les valeurs sont fixees n'est pas inclus dans
   # l'ensemble des options des materiaux
   if not setGivenBulkBehavOptions.issubset(setBulkBehavOptions): 
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un meessage d'erreur
      showError("at least one option of a material is described but is not ssociated to any material!")

   ##
   ## verification de la coherence de la structure de donnees qui peermet de verifier les options des modeles
   ##

   # on construit l'ensemble des materiaux connus de la srtucture de verification des options des materiaux
   setCheckedBulkBehavs=frozenset(list(checkBulkBehavOptions.keys()))

   # si cet ensemble n'est pas egal a l'ensemble des materiaux
   if setCheckedBulkBehavs !=  frozenset(listeBulkBehav):
      # on incremente le nombre d'erreurs
      nb_errors += 1
      # on affiche un meessage d'erreur
      showError("at least one material cannot be checked!")

   # pour chaque materiau gere par la structure de verification des options des materiaux
   for bulk_type in list(checkBulkBehavOptions.keys()):
      # on commence la verification a partir des fils de l'arbre associe au type du materiau, qui sont
      # tous des options
      for child in checkBulkBehavOptions[bulk_type].childs:
         _check_bulk_options(bulk_type, child, "option")

   ##
   ## verification de la coherence du dictionnaire asociant les materiaux compatibles avec les modeles physiques
   ##

   ### on verifie qu'une liste de types de materiaux est bien associe a chaque
   ### type de modele mecanique

   # on construit l'ensemble des modeles mecaniques
   setMecaModels = frozenset(modelOptions['MECAx']['material'])

   # on construit l'ensemble des types de modeles mecaniques auxquels sont 
   # associes des types de materiaux
   setMecaModel2BulkBehavs = frozenset(list(mecaModel2bulkBehavs.keys()))

   # si les deux ensembles sont differents
   if setMecaModels != setMecaModel2BulkBehavs:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least a mechanical model is not associated to any material!")

   ### on verifie que l'ensemble de materiaux associee a chaque liste de types 
   ### de modeles mecaniques est incluse dans l'ensemble de tous les materiaux

   # on construit l'ensemble des materiaux
   setBulkBehav=frozenset(listeBulkBehav)

   # pour chaque liste de materiaux associee a un modele mecanique
   for mats in list(mecaModel2bulkBehavs.values()):
      # si l'ensemble des materiaux de la liste n'est pas inclus dans l'ensemble
      # des materiaux
      if not setBulkBehav.issuperset( frozenset(mats) ):
         # on incrmente le nombre d'erreurs
         nb_errors += 1
         # on affiche un message d'erreur
         showError(str(mats) + " do not contains only materials!")

   ##
   ## verification de la coherence du dictionnaire donnant la valeur de l'anisotropie d'un materiau 
   ## en fonction de la valeur de l'anisotropie du modele
   ##

   # verification de l'existence des mots-clefs utilises

   # pour chaque paire (clef, valeur) du dictionnaire
   for (aniso_model, aniso_material) in list(anisotopyFromModel2BulkBehav.items()):
      # si la valeur courante de l'anisotropie dans le modele n'est pas definie
      if not aniso_model in modelOptions['MECAx']['anisotropy']:
         # on incrmente le nombre d'erreurs
         nb_errors += 1
         # on affiche un message d'erreur
         showError("unkown anisotropy value for a model : " + aniso_model + "!")
      # si la valeur courante de l'anisotropie dans le materiau n'est pas definie
      if not aniso_material in list(matcle2option['anisotropy'].keys()):
         # on incrmente le nombre d'erreurs
         nb_errors += 1
         # on affiche un message d'erreur
         showError("unkown anisotropy value for a material : " + aniso_material + "!")

   # verification de l'unicite de l'utilisation de chaque option
   #    * anisotropie du modele
   # pour chaque valeur possible de l'anisotropie
   for aniso_model in modelOptions['MECAx']['anisotropy']:
      # si la valeur courante apparait plus d'une fois dans le dictionnaire
      if list(anisotopyFromModel2BulkBehav.keys()).count(aniso_model) > 1:
         # on incrmente le nombre d'erreurs
         nb_errors += 1
         # on affiche un message d'erreur
         showError(aniso_model + " is associated to more than one material anisotropy value!")
   #    * anisotropie du materiau
   # pour chaque valeur possible de l'anisotropie
   for aniso_material in list(matcle2option['anisotropy'].keys()):
      # si la valeur courante apparait plus d'une fois dans le dictionnaire
      if list(anisotopyFromModel2BulkBehav.values()).count(aniso_material) > 1:
         # on incrmente le nombre d'erreurs
         nb_errors += 1
         # on affiche un message d'erreur
         showError(aniso_material + " is associated to more than one model anisotropy value!")
      
   ##
   ## verifcation de la coherence du dictionnaire donnant la liste de lois d'interaction
   ## compatibles avec un type de paire de contacteurs
   ##

   # on contruit l'ensemble de toutes les lois d'interaction
   setTactBehav=frozenset(listeTactBehav)

   # on contruit la table qui donne la liste des lois associes a une paire d'objets
   # sous la forme d'un dictionnaire d'ensembles

   # on l'initialise a vide
   setContactorPair2TactBehav={}
   # pour chaque type de paire de contacteurs
   for pair in list(contactorPair2TactBehav.keys()):
      # on ajoute l'ensemble des lois associes a ce type de paire de contacteurs
      # a la table
      setContactorPair2TactBehav[pair]=frozenset(contactorPair2TactBehav[pair])

   ### on verifie que la table donnant la liste des lois associees a un type de paire de
   ### contacteurs est une partition de l'ensemble des lois d'interaction

   # on verifie que l'intersection des ensembles pris 2 a 2 est nulle
   for pair1 in list(setContactorPair2TactBehav.keys()):
      for pair2 in list(setContactorPair2TactBehav.keys()):
         if pair1 != pair2:
            if len(setContactorPair2TactBehav[pair1].intersection(setContactorPair2TactBehav[pair2])) != 0:
               # on incrmente le nombre d'erreurs
               nb_errors += 1
               # on affiche un message d'erreur
               showError(pair1 + " and " + pair2 + " share an interaction law!")

   # on verifie que l'union de tous les ensembles donne l'esemble de tous les lois d'interacton
   
   # on calcule l'union des tous les ensembles
   # on l'initialise a l'ensemble vide
   unionContactorPair2TactBehav=set()
   # pour chaque type de paire de contacteur
   for pair in list(setContactorPair2TactBehav.keys()):
      # on ajoute l'ensemble des lois associes au type de paire de contacteurs courant a l'union
      unionContactorPair2TactBehav=unionContactorPair2TactBehav.union(setContactorPair2TactBehav[pair])

   # si l'union de tous les ensembles ne donne pas l'ensemble de toutes les lois d'interaction
   if unionContactorPair2TactBehav != setTactBehav:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un message d'erreur
      showError("at least an interaction law is not associated to any contactor pair!")

   ##
   ## verification des contacteurs decrits dans les tables de visibilite
   ##

   ### on verifie que le classement par type de corps des contacteurs utilise bien tous les types de corps

   # si l'ensemble des types de corps est differents de l'ensemble des types de corps pour lesquels une liste
   # de contacteur est donnee
   if frozenset(listeBodyType) != frozenset(list(bodyType2contactor.keys())):
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un meessage d'erreur
      showError("contactors are not described for all body types!")

   # on construit l'ensemble des contacteurs utilises pour les tables de visibilite
   # a partir du classement par type de corps
   # on l'initialise a l'ensemble vide
   setContactors4SeeTables=set()
   # pour chaque type de corps
   for bodyType in listeBodyType:
      # on ajoute l'ensemble des contacteurs associes au type de corps courant
      setContactors4SeeTables=setContactors4SeeTables.union( frozenset(bodyType2contactor[bodyType]) )

   # on construit l'ensemble
   #    * des contacteurs candidats
   setCandidat=frozenset(listeCandidat)
   #    * des contacteurs antagonistes
   setAntagoniste=frozenset(listeAntagoniste)

   ### on verifie que tous les contacteurs disponibles soient dans l'ensemble des contacteurs 
   ### utilisables pour definir les tables de visibilites, en tenant compte des cas particuliers

   # si ce n'est pas le cas
   if setContactor.difference( set(['POLYF', 'POLYD', 'CSxx3', 'CSxx4', 'CSpx3', 'CSpx4', 'ASpx3', 'ASpx4','CSpx8','CSpx6']) ) != \
     setContactors4SeeTables.difference( set(['CSxxx', 'CSpxx', 'ASpxx']) ):
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un meessage d'erreur
      showError("some contactors are not available to describe see tables!")

   ### on verifie que l'union de l'ensemble des contacteurs candidats et de l'ensemble des contaacteurs antagonistes
   ### donne l'ensemble des contacteurs disponibles pour les tables de visibilite

   # si ce n'est pas le cas
   if setCandidat.union(setAntagoniste) != setContactors4SeeTables:
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un meessage d'erreur
      showError("some contactors are neither candidate nor antagonist!")

   ##
   ## verification des contacteurs decrits dans les tables de visibilite
   ##

   # on construit l'ensemble
   #    * des commandes de post-traitement
   setCommands=frozenset(commandList)
   #    * des commandes actives avant le calcul
   setCommandsBeforeComputation=frozenset(commandsBeforeComputation)

   ### on verifie que l'ensemble des commandes actives avant le calcul soient des commandes de post-traitement

   # si ce n'est pas le cas
   if not setCommandsBeforeComputation.issubset(setCommands):
      # on incrmente le nombre d'erreurs
      nb_errors += 1
      # on affiche un meessage d'erreur
      showError("at least one command before computation is not a postpro command!")
   
   ##
   ## affichage du resultat global
   ##

   # s'il n'y a pas eu d'erreur
   if nb_errors == 0:
      # s'il n'y a pas eu de warning
      if nb_warnings == 0:
         # on indique que le test a reussi
         print("Module is clean :-)")
      # sinon,
      else:   
         # on indique qu'il y a quelques pb
         print("Module contains " + str(nb_warnings) + " warnings :-|")
   # sinon,
   else:   
      # on indique que le test a echoue
      print("Module contains " + str(nb_errors) + " errors :-(")
      
