## module dof
# define classes dof, dofs

from pathlib import Path

import numpy

from ...utilities.error    import *
from ...config.lmgc90dicts import *

## @class DrvDof
#  management of an imposed dof
class DrvDof():
    """ 
    management of an imposed dof for lmgc90 (vlocy, force, temp, flux)
    """
    def __init__(self,description='predefined',ct=0.,amp=0.,omega=0.,phi=0.,rampi=1.,ramp=0.,evolutionFile='',dofty=''):
        """
        self=imposeDof(description='predefined',ct=0.,amp=0.,omega=0.,phi=0.,rampi=1.,ramp=0.,evolutionFile='',dofty='')
        generate boundary conditions for lmgc90
        dofty       : type of boundary condition (temp for temperature, vlocy for velocity, force for.. force)
        description : 'predefined' or 'evolution'
          'predefined' case: ct, amp, omega, phi, rampi and ramp must be defined
                             so that the boundary value will be : [ ct + amp * cos(omega*t+phi) ] * sgn(rampi + ramp*t) * min(|rampi + ramp*t|, 1),
                             where t is the time
          'evolution' case : evolutionFile must be defined
                             the input file must contain two columns separated by a blank character. First columns is time
                             and the second one the imposed value. A linear interpolation is made by lmgc90 to compute values
                             at computational times different from those given in input
        """

        # on verifie le type de CL

        # si on reconnait le type de CL
        if description == 'predefined' or description == 'evolution':
           # on le stocke
           self.dtype = description
        # sinon
        else:
           # on construit un message d'erreur rappelant les types disponibles
           msg = 'Unknown boundary condition type\n must be among:\n'
           for i in ('predefined', 'evolution'):
              msg+=i+'\n'
           # on l'affiche
           showError(msg)

        # si le type a ete reconnu, on stocke les options le concernant

        # selon le type de CL.
        if self.dtype == 'predefined': # cas de l'utilisation de la fonction predefinie
           # on verifie le type des coefficients de la fonction

           # on essaye de les convertir en reels
           try:
              ct    = float(ct)
              amp   = float(amp)
              omega = float(omega)
              phi   = float(phi)
              rampi = float(rampi)
              ramp  = float(ramp)
           # si on echoue
           except:
              # on affiche un message d'erreur
              showError("When using a \"predefined\" boundary condition type, the coefficients (ct, amp, omega, rampi and ramp) must be reals!")

           # ici, on est sur que les coefficients sont valides et on les stocke
           self.ct    = ct
           self.amp   = amp
           self.omega = omega
           self.phi   = phi
           self.rampi = rampi
           self.ramp  = ramp

        elif self.dtype == 'evolution': # cas de l'utilisation d'un fichier d'evolution
           # on verifie le type du nom du fichier d'evolution

           # si le nom du fichier d'evolution n'est pas une chaine de caracteres
           evolutionFile = str(Path) if isinstance(evolutionFile, Path) else evolutionFile
           if not isinstance(evolutionFile, str):
              # on affiche un message d'erreur
              showError("When using an \"evolution\" boundary condition type, the name of the file (evolutionFile) must be a string!")

           # si le nom du fichier d'evolution est une chaine vide
           if len(evolutionFile) == 0:
              # on affiche un message d'erreur
              showError("When using an \"evolution\" boundary condition type, the name of the file (evolutionFile) must be an non-empty string!")

           # si le nom du fichier d'evolution est trop long
           if len(evolutionFile) > 40:
              # on affiche un message d'erreur
              showError("When using an \"evolution\" boundary condition type, the name of the file (evolutionFile) must be a string of length <= 40!")

           # ici, on est sur que le nom du fichier d'evolution est valide
           # et on le stocke
           self.evolutionFile = evolutionFile

        else: # cas general improbable
           # on affiche un message d'erreur
           showError("DrvDof::__init__ : unknown boundary condition type!")

        # on verifie le type de ddl a imposer
        # N.B. la verification de l'adequation modele <-> ddl a du ete faite au niveau de l'avatar

        # si on reconnait le type de ddl
        if dofty in doftys:
           # on le stocke
           self.dofty = dofty + (5 - len(dofty))*' '
        # sinon,
        else:
           # on ecrit un message d'erreur
           showError(dofty + " : unknown dof type!")

#################################################################

## @class dof
# management of dof attached to a model applying on a node \n
# attributs:\n
# model: model associated to the dofs
# nbdof: size of the vector of dofs
# disp :  displacement of parent node
# rot  :  rotation of bulk of parent node
# values: values of the dofs
# pilote: boolean array indicating if a component is imposed
# driven: imposed values of the dofs
class dof():

    ## constructor of dof
    #
    # @param nbdof: size of the vector of dofs
    def __init__(self,nbdof=0,disp_size=0):

       #if nbdof == 0:
       #  msg='The dof vector has null dimension'
       #  showError(msg)
       #  self.values = []
       #  self.pilote = []
       #  self.driven = []

       #else:
       # si le nombre de ddl n'est pas strictement positif
       if nbdof <= 0:
          # on affiche un message d'erreur
          showError("the number of dof must be non negative!")

       # si tout est bon, on stocke le nombre de ddl
       self.nbdof = nbdof
       self.ntype = dimensionTypeNode[ nbdof ]

       # storing displacement indepently of reference
       # coordinates and a rotation for rbdy2
       self.disp  = numpy.zeros( disp_size )
       self.rot   = None

       # et on initialise les tableaux portes par un objet dof
       #    * les valeurs des ddl avec C.I.
       self.values = [0.]*nbdof
       #    * le booleen qui indique si un ddl porte une C.L.
       self.pilote = [False]*nbdof
       #    * l'objet C.L. associe au ddl qui porte une C.L.
       self.driven = [None]*nbdof

    ## impose intial value(s) of the dof set
    def imposeInitValue(self,components,values):
        # si la liste des composantes se reduit a un entier
        if isinstance(components,int): 
           # on la transforme en liste
           components = [components]
           # ainsi que la valeur associee
           values     = [values]

        # si la liste des composantes n'est pas de la meme taille que
        # la liste des valeurs
        if len(components) != len(values):
            # on leve une exception
            raise Exception

        # si la plus grande composante consideree est plus grande que le nombre de ddl
        if max(components) > self.nbdof:
           # on affiche un message d'erreur
           showError("a component exceeds the number of dof (=" + str(self.nbdof) + ")!")

        # on enumere les composantes
        for indic, component in enumerate(components):
            self.values[component-1]=values[indic]

    ## impose driven values of the dof set
    def imposeDrivenDof(self,components,description='predefined',ct=0.,amp=0.,omega=0.,phi=0.,rampi=0.,ramp=0.,
                        evolutionFile='',dofty='temp'):

        # si la liste des composantes se reduit a un entier
        if isinstance(components,int):
           # on la transforme en liste
           components = [components]

        # si la plus grande composante consideree est plus grande que le nombre de ddl
        if max(components) > self.nbdof:
           # on affiche un message d'erreur
           showError("a component exceeds the number of dof (=" + str(self.nbdof) + ")!")

        # pour chaque composante
        for component in components:
           # on cree un nouvel objet "ddl impose" et on l'associe la composante courante
           self.driven[component-1]=DrvDof(description,ct,amp,omega,phi,rampi,ramp,evolutionFile,dofty)
           # on indique que la composante courante est imposee
           self.pilote[component-1]=True
             
    ## relax a previously driven dof of the set
    def relaxDrivenDof(self,components):

        # si la liste des composantes se reduit a un entier
        if isinstance(components,int):
           # on la transforme en liste
            components=[components]

        # si la plus grande composante consideree est plus grande que le nombre de ddl
        if max(components) > self.nbdof:
           # on affiche un message d'erreur
           showError("a component exceeds the number of dof (=" + str(self.nbdof) + ")!")

        # pour chaque composante
        for component in components:
           # on supprime l'objet "ddl impose" associe a la composante courante
           # N.B.: techniquement on supprime la reference vers cet objet, c'est le gerbage collector
           # qui le supprimera plus tard
           self.driven[component-1]=None
           # on indique que la composante courante n'est plus imposee
           self.pilote[component-1]=False

#    ## affichage du contenu de ddl
#    def __str__(self):
#       if int(self.dimension) == 3:
#        impr = """\tddl de type\t:\tstructure\t repere\t\t:\t%s
#        \t\tcomposante 1 :  %s\t impose \t:\t %s 
#        \t\tcomposante 2 :  %s\t impose \t:\t %s
#        \t\tcomposante 3 :  %s\t impose \t:\t %s
#        \t\tcomposante 4 :  %s\t impose \t:\t %s 
#        \t\tcomposante 5 :  %s\t impose \t:\t %s
#        \t\tcomposante 6 :  %s\t impose \t:\t %s""" % \
#          (self.repere,self.values[0],self.pilote[0],self.values[1],self.pilote[1],self.values[2],self.pilote[2],
#           self.values[3],self.pilote[3],self.values[4],self.pilote[4],self.values[5],self.pilote[5]
#           )
#       else:
#        impr = """\tddl de type\t:\tstructure\t repere\t\t:\t%s
#        \t\tcomposante 1 :  %s\t impose \t:\t %s 
#        \t\tcomposante 2 :  %s\t impose \t:\t %s
#        \t\tcomposante 3 :  %s\t impose \t:\t %s
#         """ % \
#          (self.repere,self.values[0],self.pilote[0],self.values[1],self.pilote[1],self.values[2],self.pilote[2]
#           )
#       return impr
    
#    ## longueur de ?
#    def __len__(self):
#        return (int(self.dimension)-1)*3
    
##
