
from ..utilities.error    import *
from ..config.lmgc90dicts import *


class see_table():
    """class see_table()

    this class defines objects to store the see tables used by the contact detection

    methods:

    - __init__: constructor
    """
    def __init__(self, CorpsCandidat, candidat, colorCandidat, behav,
                 CorpsAntagoniste, antagoniste, colorAntagoniste, alert, halo=None, name=None):
        """ __init__(self, CorpsCandidat, candidat, colorCandidat, behav,
        CorpsAntagoniste, antagoniste, colorAntagoniste, alert, halo=None, name=None):

        this method builds a new see table

        parameters:

        - self: the see table itself
        - CorpsCandidat: candidate body type
        - candidat: candidate contactor type
        - colorCandidat: candidate contactor color
        - behav: name of a contact law. It can be undefined when the see table is 
          declared, but have to be defined when files are write down, in order 
          to ensure data consitency
        - CorpsAntagoniste: antagonist body type
        - antagoniste: antagonist contactor type
        - colorAntagoniste: antagonist contactor color
        - alert: alert distance used to activate contact between the candidate body
          and the antagonist body

        optional parameters:

        - halo=None: optional second alert distance used to detect contact between 
          two deformable bodies, in order to avoid some numerical artefacts
        - name=None: name of the see table (still present to ensure compatibility)
        """
        # si l'utilisateur attribue un nom a la table de visibilite
        if name != None:
           # on lui indique qu'il ne sera pas utilise
           shwoWarning('assign a name to a see table is useless, and will be forbidden!')

        # verification de la coherence des donnees

        # si on reconnait le type du corps candidat 
        if CorpsCandidat in listeBodyType:
           # on le stocke
           self.CorpsCandidat = CorpsCandidat
        # sinon,
        else:
           # on construit un message d'erreur rappelant les types disponibles
           msg = "Type of candidate body unknown\n it must be among:\n"
           for i in listeBodyType:
               msg+=i+'\n'
           # on l'affiche
           showError(msg)

        # si on reconnait le type du corps antagoniste
        if CorpsAntagoniste in listeBodyType:
           # on le stocke
           self.CorpsAntagoniste = CorpsAntagoniste
        # sinon,
        else:
           # on construit un message d'erreur rappelant les types disponibles
           msg = "Type of antagonist body unknown\n it must be among:\n"
           for i in listeBodyType:
               msg+=i+'\n'
           # on l'affiche
           showError(msg)

        # si on reconnait le type du contacteur candidat
        if candidat in listeCandidat:
           # si le type de contacteur candidat est compatible avec le 
           # type de corps candidat
           if candidat in bodyType2contactor[self.CorpsCandidat]:
              # on le stocke 
              self.candidat = candidat
           # sinon,
           else:
              # on construit un message d'erreur rappelant les types compatibles
              msg = "Type of candidate contactor incompatible with a body of type " + self.CorpsCandidat + "\n" + \
                    " the contactor type must be among:\n"
              for i in bodyType2contactor[self.CorpsCandidat]:
                 msg+=i+'\n'
              # on l'affiche
              showError(msg)
        # sinon,
        else:
           # on construit un message d'erreur rappelant les types disponibles
           msg = "Type of candidate contactor unknown\n the type must be among:\n" 
           for i in listeCandidat:
               msg+=i+'\n'
           # on l'affiche
           showError(msg)

        # si on reconnait le type du contacteur antagoniste
        if antagoniste in listeAntagoniste:
           # si le type de contacteur antagoniste est compatible avec le 
           # type de corps antagoniste
           if antagoniste in bodyType2contactor[self.CorpsAntagoniste]:
              # on le stocke 
              self.antagoniste = antagoniste
           # sinon,
           else:
              # on construit un message d'erreur rappelant les types compatibles
              msg = "Type of antagonist contactor incompatible with a body of type " + self.CorpsAntagoniste + "\n" + \
                    " the contactor type must be among:\n"
              for i in bodyType2contactor[self.CorpsAntagoniste]:
                 msg+=i+'\n'
              # on l'affiche
              showError(msg)
        # sinon,
        else:
           # on construit un message d'erreur rappelant les types disponibles
           msg = "Type of antagonist contactor unknown\n the type must be among:\n"
           for i in listeAntagoniste:
               msg+=i+'\n'
           # on l'affiche
           showError(msg)

        # on verifie la couleur des contacteurs
        
        # si la couleur du contacteur candidat n'est pas une chaine
        if not isinstance(colorCandidat, str):
           # on affiche un message d'erreur
           shwoError("color of the candidate contactor must be a 5 characters string!")
 
        # si la chaine ne fait pas cinq caracteres 
        if len(colorCandidat) != 5:
           # on affiche un message d'erreur
           showError("color of the candidate contactor must be a 5 characters string!")
 
        # si tout est bon, on stocke la couleur du contacteur candidat
        self.colorCandidat = colorCandidat

        # si la couleur du contacteur antagoniste n'est pas une chaine
        if not isinstance(colorAntagoniste, str):
           # on affiche un message d'erreur
           showError("color of the antagonist contactor must be a 5 characters string!")
 
        # si la chaine ne fait pas cinq caracteres 
        if len(colorAntagoniste) != 5:
           # on affiche un messge d'erreur
           showError("color of the antagonist contactor must be a 5 characters string!")
 
        # si tout est bon, on stocke la couleur du contacteur antagoniste
        self.colorAntagoniste = colorAntagoniste

        # verification de la validite de la loi d'interaction

        # on construit une chaine paermettant d'identifier la table de visibilite
        id_see="interaction : " + self.candidat + " (" + self.colorCandidat + ") / " + \
               self.antagoniste + " (" + self.colorAntagoniste + ")\n"

        # si la loi d'interaction est une instance de la classe tact_behav
        if isinstance(behav, tact_behav):
           # si la loi n'est pas compatible avec tout type de paire de contacteurs
           if not behav.law in contactorPair2TactBehav['any/any']:
              # si au moins un des deux contacteurs est un contacteur point
              if self.candidat in pointContactor or self.antagoniste in pointContactor:
                 # si l'autre n'est pas un contacteur point
                 if not self.candidat in pointContactor or not self.antagoniste in pointContactor:
                    # on explique a l'utilsateur que quand seul un des deux contacteurs consideres
                    # est un point, il doit utiliser une loi compatible avec n'importe quel type de
                    # paire de contacteur :
                    # on construit un message d'erreur rappelant les types disponibles
                    msg = id_see + "Only one of the two is point type contactor.\n" +\
                          "A contact law compatible with any pair of contactors must be used, " +\
                          "i.e. must be among:\n"
                    for i in contactorPair2TactBehav['any/any']:
                        msg+=i+'\n'
                    # on l'affiche
                    showError(msg)
                 # sinon (i.e. si les deux contacteurs sont des contacteurs point)
                 else:
                    # si la loi n'est pas une loi reservee aux interactions point/point
                    if not behav.law in contactorPair2TactBehav['point/point']:
                       # on explique a l'utilsateur que quand les deux contacteurs consideres
                       # sont des points, il doit utiliser une loi ad hoc ou une loi compatible avec 
                       # n'importe quel type de paire de contacteur :
                       # on construit un message d'erreur rappelant les types disponibles
                       msg = id_see + "The two contactors are of point type contactors.\n" +\
                             "It is possible to use only a dedicated law, " +\
                             "i.e. must be among:\n"
                       for i in contactorPair2TactBehav['point/point']:
                           msg+=i+'\n'
                       msg += "or a law compatible with any pair of contactors, " +\
                              "i.e. must be among:\n"
                       for i in contactorPair2TactBehav['any/any']:
                           msg+=i+'\n'
                       # on l'affiche
                       showError(msg)
              # sinon, si les deux contacteurs sont des contacteurs rigides
              elif candidat in rigidContactor and antagoniste in rigidContactor:
                 # si la loi n'est pas une loi reserve aux interactions rigide/rigide (incluant les interaction de type :
                 # avatar MAILx + contacteur POLYD / avatar MAILx + contacteur POLYD ou avatar RBDY3 + contacteur qq)
                 if not behav.law in contactorPair2TactBehav['rigid/rigid']:
                    # on explique a l'utilsateur que quand les deux contacteurs consideres sont rigides,
                    # il doit utiliser soit une loi ad hoc, soit une loi compatible avec n'importe quel type de paire
                    # de contacteur :
                    # on construit un message d'erreur rappelant les types disponibles
                    msg = id_see + "The two contactors are of rigid type contactors.\n" +\
                          "It is possible to use only a dedicated law, " +\
                          "i.e. must be among:\n"
                    for i in contactorPair2TactBehav['rigid/rigid']:
                        msg+=i+'\n'
                    msg += "or a law compatible with any pair of contactors, " +\
                           "i.e. must be among:\n"
                    for i in contactorPair2TactBehav['any/any']:
                        msg+=i+'\n'
                    # on l'affiche
                    showError(msg)
              # sinon (i.e. au moins un des contacteurs est un contacteur deformable)
              else:
                 # si la loi n'est pas une loi reserve aux interactions rigide/defo ou defo/defo
                 if not behav.law in contactorPair2TactBehav['any/defo']:
                    # on explique a l'utilsateur que quand au moins l'un des deux contacteurs consideres est deformable,
                    # il doit utiliser soit une loi ad hoc, soit une loi compatible avec n'importe quel type de paire
                    # de contacteur :
                    # on construit un message d'erreur rappelant les types disponibles
                    msg = id_see + "At a least one contactor is of a deformable type.\n" +\
                          "It is possible to use only a dedicated law, " +\
                          "i.e. must be among:\n"
                    for i in contactorPair2TactBehav['any/defo']:
                        msg+=i+'\n'
                    msg += "or a law compatible with any pair of contactors, " +\
                           "i.e. must be among:\n"
                    for i in contactorPair2TactBehav['any/any']:
                        msg+=i+'\n'
                    # on l'affiche
                    showError(msg)
           # ici, on est sur que la loi et la paire de contacteurs consideres sont compatibles
           # on stocke le nom de la loi d'interaction
           self.behav = behav.nom
        # sinon,
        else:
           # si la loi d'interaction n'est pas une chaine non plus
           if not isinstance(behav, str):
              # on affiche un message d'erreur
              showError("given interaction law must be a tact_behav instance or a five caracters string!")

           # ici, on est sur que la loi d'interaction est une chaine
           # si la chaine ne fait pas 5 caracteres
           if len(behav) != 5:
              # on affiche un message d'erreur
              showError("given interaction law must be a tact_behav instance or a five caracters string!")

           # ici, on est sur que la chaine est valide
           # on affiche un message avertissant que la compatibilite entre la loi d'interaction et 
           # le type de contacteurs ne sera pas verifiee
           showWarning("consistancy check between interaction law and contactor pair cannot be performed, since the given law is a string")
           # on stocke le nom de la loi d'interaction
           self.behav = behav

        # stockage de :
        #    * la distance d'alerte 
        # on tente de convertir en reel la distance d'alerte 
        try:
           self.alert = float(alert)
        # si on echoue
        except:
           # on affiche un message d'erreur
           showError("the alert distance must be a real value!")
        #    * le halo (ou seconde distance d'alerte optionnelle)
        # si le halo n'a pas ete defini
        if halo is None:
           # on le stocke tel quel
           self.halo = halo
        # sinon,
        else:
           # on tente de le convertir en reel
           try:
              self.halo = float(halo)
           # si on echoue
           except:
              # on affiche un message d'erreur
              showError("the halo must be a real value!")
            
##############################################

class tact_behav():
    """class behav()
       defines a interaction behaviour defined in 'behavOptions'
       associated methodes:
       -__init__
       -addOption
    """
    def __init__(self,name,law,**kargs):
        """ __init__(self,name,law,**kargs)
           kargs is a dictionary defining the parameters
        """
        # si le nom de la loi n'est pas une chaine de caracteres
        if not isinstance(name, str):
           # on affiche un message d'erreur
           showError("name of the law must be a 5 characters string!")

        # si la chaine ne fait pas cinq caracteres 
        if len(name) != 5:
           # on affiche un message d'erreur
           showError("name of the law must be a 5 characters string!")

        # si le nom de la loi est correct, on le stocke
        self.nom  = name

        # si on reconnait le type de loi d'interaction
        if law in listeTactBehav:
            # on le stocke
            self.law = law
        # sinon
        else:
            # on construit un message d'erreur rappelant les types disponibles
            msg = "Unknown interaction law type\n must be among:\n"
            for i in listeTactBehav:
                msg+=i+'\n'
            # on l'affiche
            showError(msg)

        # affectation de ses options a la loi d'interaction

        # on recupere la liste des options a affecter a la loi d'interaction
        cles =  list(kargs.keys())

        # on ajoute les options a la loi d'interaction

        # on initialise le nombre d'options stockees a 0
        nb_options = 0
        # pour chaque option
        for cle in cles:
           # si l'option courante ne fait pas partie des options de la loi d'interaction
           if not cle in tactBehavOptions[self.law]:
              # on affiche un warning
              msg="the option \"" + cle + "\" is not compatible with an interaction law of type " + self.law
              showWarning(msg)
              # on passe a la suivante
              continue

           # ici, on est sur que l'option est attendue par la loi d'interaction

           # on tente de convertir la valeur en reel
           try:
              kargs[cle]=float(kargs[cle])
           # si on echoue
           except:
              # on affiche un message d'erreur
              showError("the option \"" + cle + "\" expect a real value!")

           # ici, on est sur que la valeur de l'option courante est valide

           # on stocke l'option dans la loi d'interaction
           setattr(self, cle, kargs[cle])
           # on incremente le nombre d'options stockees par la loi d'ineteraction
           nb_options += 1 

        # si le nombre d'options stockees par la loi d'interaction n'est pas egal au nombre d'options attendues par une loi de ce type
        if nb_options != len(tactBehavOptions[self.law]):
           # on construit un message d'erreur listant les options manquantes
           msg = "Incomplete interaction law.\nA value must be assigned to the following options:\n"
           for option in tactBehavOptions[self.law]:
              if not hasattr(self, option):
                 msg += option + "\n"
           # on l'affiche
           showError(msg)
        # Pour la loi MAL_CZM, on verifie que dt soit positif
        if law == "MAL_CZM":
           # Pour verifier que dt soit positif, une division par cn est necessaire.
           # On verifie donc que cn soit different de zero
           test = kargs["cn"] > 0
           if test == False:
              msg = "cn must be strictly positive"
              showError(msg)
           # Pour verifier que dt soit positif, une division par ct est necessaire.
           # On verifie donc que ct soit different de zero
           test = kargs["ct"] > 0
           if test == False:
              msg = "ct must be strictly positive"
              showError(msg)
           #On verifie que la valeur de G1 donne un dt1 positif 
           test = kargs["G1"] >= 0.5 * kargs["s1"] * kargs["s1"] / kargs["cn"]
           if test == False:
              msg = "Incompatibilite des parametres\nG1 doit etre superieur a 0.5 * s1 * s1 /cn"
              showError(msg)
           #On verifie que la valeur de G2 donne un dt2 positif 
           test = kargs["G2"] >= 0.5 * kargs["s2"] * kargs["s2"] / kargs["ct"]
           if test == False:
              msg = "Incompatibilite des parametres\nG2 doit etre superieur a 0.5 * s2 * s2 /ct"
              showError(msg)

