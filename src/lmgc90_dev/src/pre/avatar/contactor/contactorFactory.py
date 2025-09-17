
from ...config.lmgc90dicts import *
from .rigidContactor2D     import *
from .rigidContactor3D     import *
from .meshedContactor      import *

# map type de contacteur vers classe
contactorType2contactorClass={'DISKx' : diskx,
                              'xKSID' : xksid,
                              'JONCx' : joncx,
                              'POLYG' : polyg,
                              'PT2Dx' : pt2dx,
                              'SPHER' : spher,
                              'CYLND' : cylnd,
                              'DNLYC' : dnlyc,
                              'PLANx' : planx,
                              'POLYR' : polyr,
                              'POLYD' : polyd,
                              'POLYF' : polyf,
                              'POLYO' : polyo,
                              'PT3Dx' : pt3dx,
                              'CLxxx' : clxxx,
                              'ALpxx' : alpxx,
                              'CSpxx' : cspxx,
                              'ASpxx' : aspxx,
                              'PT2DL' : pt2dl,
                              'PT2TL' : pt2tl,
                              'DISKL' : diskl  #experimental
                             }

# fabrique de contacteurs
def contactorFactory(elements, shape, color, reverse=False, shift=None, area=None, volume=None, I=None, frame=None, number=None, **options):
    """def contactorFactory
       this function builds and return a new contactor.
       parameters:
          - elements: a list of connex elements, which is the base of the contactor
            WARNING: elements must be of the same type!
          - shape: type of the contactor
          - color: color of the contactor
       optional parameters:
          - reverse=False: if reverse is True, the connectivity of the elements is reversed
          - shift=None : if the contactor is shifted relatively to the supporting nodes
          - frame=None : if the contactor is  turned relatively to the local frame of the body
          - volume=None : allows to give a computed volume (in 3D, or a area in 2D) to a rigid contactor
          - I=None : allows to give a computed inertia (a 3x3 matrix in 3D, or a scalar in 2D) to a rigid contactor
          - number=None: index of the contactor (still present to ensure compatibility)
          - **options: a dictionnary used to assign options to the contactor (pair 'option'=value)
    """

    # test des options au contacteur

    # on recupere la liste des options du contacteur
    cles = list(options.keys())

    # on test les options au contacteur

    # on initialise le nombre d'options valides a 0
    nb_options = 0
    # pour chaque option
    for cle in cles:
       # si l'option courante ne fait pas partie des options du contacteur
       if not cle in contactorOptions[shape] and not cle in contactorOptionalOptions[shape] :
          # on affiche un warning
          msg="option \"" + cle + "\" is not available for contactor of type " + shape
          showWarning(msg)
          # on passe a la suivante
          continue
       # on incremente le nombre d'options valides pour le contacteur
       nb_options += 1 

    # si le nombre d'options valides pour le contacteur n'est pas egal au nombre d'options attendues par un 
    # contacteur de ce type
    if nb_options < len(contactorOptions[shape]):
       # on construit un message d'erreur listant les options manquantes
       msg = "Incomplete contactor\nA value must be provided for the following options:\n"
       for option in contactorOptions[shape]:
          if not option in cles:
             msg += option + "\n"
       # on l'affiche
       showError(msg)

    # on construit le contacteur a partir, en fonction de son type

    # on recupere la classe de contacteur associee au type
    tact_class=contactorType2contactorClass[shape]

    # en fonction du type de classe
    #   * cas d'un contacteur rigide, en 2D
    if issubclass(tact_class, rigidContactor2D):
       tact=tact_class(elements=elements, color=color, area=area, I=I, shift=shift, **options)
    #   * cas d'un contacteur rigide, en 3D
    elif issubclass(tact_class, rigidContactor3D):
       tact=tact_class(elements=elements, color=color, volume=volume, I=I, shift=shift, frame=frame, reverse=reverse, **options)
    #   * cas d'un contacteur maille
    elif issubclass(tact_class, meshedContactor):

      # on recupere la topologie des lignes/surfaces
      #fd 2021-04-08 si les surfaces ont ete construites par morceaux (comme pour une sphere)
      #fd on perd la continuite et on va dupliquer les contacteurs aux
      #fd jonctions et mal calculer la surface aux noeuds (aux elements c'est bon)  
      topos_={}
      for ele_ in elements:
         topos_.setdefault(ele_.geometricalEntity,[]).append(ele_) 

      # for (g,l) in topos_.items(): 
      #   print(g,l)   

      if len(topos_) > 1 :
        tact=[]   
        for (g,topo_) in topos_.items() :
          tact.append(tact_class(elements=topo_, color=color, reverse=reverse, **options))  
      else : 
        tact=tact_class(elements=elements, color=color, reverse=reverse, **options)

    # on renvoie le contacteur
    return tact

if __name__=='__main__':
   # test a la con...
   from avatar.bulk.rigid2d import *
   from avatar.bulk.rigid3d import *
   from build_avatar.mesh import *
   from avatar.bulk.element import *

   # tests unitaires des contacteurs
   b = rigid2d()
   d = contactorFactory(elements=[b], shape='DISKx', color='BLUEx', byrd=1.)
   print(d.getRigidProperties())
   print(d.strInBodiesFile(1))

   db = contactorFactory(elements=[b], shape='DISKx', color='BLUEx', byrd=1., shift=[1., 0.])
   print(db.getRigidProperties())
   print(db.strInBodiesFile(1))

   k = contactorFactory(elements=[b], shape='xKSID', color='BLUEx', byrd=1.)
   print(k.getRigidProperties())
   print(k.strInBodiesFile(1))

   j = contactorFactory(elements=[b], shape='JONCx', color='WALLx', axe1=1., axe2=0.01)
   print(j.getRigidProperties())
   print(j.strInBodiesFile(1))

   # test des polygones : un carre d'arete 1, centre en (0.5, 0.5) 
   vertices=numpy.zeros([4, 2], 'd')
   vertices[0, 0]=0.; vertices[0, 1]=0.
   vertices[1, 0]=1.; vertices[1, 1]=0.
   vertices[2, 0]=1.; vertices[2, 1]=1.
   vertices[3, 0]=0.; vertices[3, 1]=1.

   p = contactorFactory(elements=[b], shape='POLYG', color='REDxx', nb_vertices=4, vertices=vertices)
   print(p.getRigidProperties())
   print(p.strInBodiesFile(1))

   pt = contactorFactory(elements=[b], shape='PT2Dx', color='VERTx', shift=[1., 0.])
   print(pt.getRigidProperties())
   print(pt.strInBodiesFile(1))

   b = rigid3d()
   s = contactorFactory(elements=[b], shape='SPHER', color='BLUEx', byrd=1.)
   print(s.getRigidProperties())
   print(s.strInBodiesFile(1))

   sb = contactorFactory(elements=[b], shape='SPHER', color='BLUEx', byrd=1., shift=[1. , 0., 0.])
   print(sb.getRigidProperties())
   print(sb.strInBodiesFile(1))

   c = contactorFactory(elements=[b], shape='CYLND', color='BLUEx', High=1., byrd=1., \
                        shift=[1. , 0., 0.], frame=[[ 1.,  0.,  0.], \
                                                    [ 0.,  1.,  0.], \
                                                    [ 0.,  0.,  1.]] )
   print(c.getRigidProperties())
   print(c.strInBodiesFile(1))

   c2 = contactorFactory(elements=[b], shape='DNLYC', color='BLUEx', High=1., byrd=1.)
   print(c2.getRigidProperties())
   print(c2.strInBodiesFile(1))

   pl = contactorFactory(elements=[b], shape='PLANx', color='WALLx', axe1=0.5, axe2=1., axe3=0.01)
   print(pl.getRigidProperties())
   print(pl.strInBodiesFile(1))

   pl2 = contactorFactory(elements=[b], shape='PLANx', color='WALLx', axe1=0.5, axe2=1., axe3=0.01, \
                          shift=[1., 0., 0.], frame=[[-1.,  0.,  0.], \
                                                     [ 0., -1.,  0.], \
                                                     [ 0.,  0.,  1.]] )
   print(pl2.getRigidProperties())
   print(pl2.strInBodiesFile(1))

   # test des polyedres : un carre d'arete 1, centre en (0., 0.) 
   #    * coordonnees des sommets (repere global)
   vertices = numpy.zeros([8, 3], 'd')
   #       - sommet 1
   vertices[0, 0]=-0.5
   vertices[0, 1]=-0.5
   vertices[0, 2]=-0.5
   #       - sommet 2
   vertices[1, 0]= 0.5
   vertices[1, 1]=-0.5
   vertices[1, 2]=-0.5
   #       - sommet 3
   vertices[2, 0]= 0.5
   vertices[2, 1]= 0.5
   vertices[2, 2]=-0.5
   #       - sommet 4
   vertices[3, 0]=-0.5
   vertices[3, 1]= 0.5
   vertices[3, 2]=-0.5
   #       - sommet 5
   vertices[4, 0]=-0.5
   vertices[4, 1]=-0.5
   vertices[4, 2]= 0.5
   #       - sommet 6
   vertices[5, 0]= 0.5
   vertices[5, 1]=-0.5
   vertices[5, 2]= 0.5
   #       - sommet 7
   vertices[6, 0]= 0.5
   vertices[6, 1]= 0.5
   vertices[6, 2]= 0.5
   #       - sommet 8
   vertices[7, 0]=-0.5
   vertices[7, 1]= 0.5
   vertices[7, 2]= 0.5
   #    * connectivite des faces
   faces = numpy.zeros([12, 3], 'i')
   faces[ 0, 0]=1; faces[ 0, 1]=2; faces[ 0, 2]=3
   faces[ 1, 0]=1; faces[ 1, 1]=3; faces[ 1, 2]=4
   faces[ 2, 0]=1; faces[ 2, 1]=2; faces[ 2, 2]=6
   faces[ 3, 0]=1; faces[ 3, 1]=6; faces[ 3, 2]=5
   faces[ 4, 0]=2; faces[ 4, 1]=3; faces[ 4, 2]=7
   faces[ 5, 0]=2; faces[ 5, 1]=7; faces[ 5, 2]=6
   faces[ 6, 0]=1; faces[ 6, 1]=4; faces[ 6, 2]=8
   faces[ 7, 0]=1; faces[ 7, 1]=8; faces[ 7, 2]=5
   faces[ 8, 0]=3; faces[ 8, 1]=4; faces[ 8, 2]=8
   faces[ 9, 0]=3; faces[ 9, 1]=8; faces[ 9, 2]=7
   faces[10, 0]=5; faces[10, 1]=7; faces[10, 2]=8
   faces[11, 0]=5; faces[11, 1]=6; faces[11, 2]=7

   pr = contactorFactory(elements=[b], shape='POLYR', color='REDxx', nb_vertices=8, vertices=vertices, nb_faces=12, connectivity=faces)
   print(pr.getRigidProperties())
   print(pr.strInBodiesFile(2))
  
   # test des POLYD : on prend la connectivite du POLYR precedent
   pd = contactorFactory(elements=[b], shape='POLYD', color='REDxx', nb_vertices=8, nb_faces=12, connectivity=faces)
   print(pd.strInBodiesFile(2))

   # test des POLYF : on construit les 6 faces du bloc precedent, sous la forme de maillages
   m1 = mesh(dimension=3)
   m1.addNode( node( coor=numpy.array(vertices[0, :]), number=1) )
   m1.addNode( node( coor=numpy.array(vertices[1, :]), number=2) )
   m1.addNode( node( coor=numpy.array(vertices[2, :]), number=3) )
   m1.addNode( node( coor=numpy.array(vertices[3, :]), number=4) )
   m1.addBulk( element(elem_dim=2, connectivity=[1, 2, 3]) )
   m1.addBulk( element(elem_dim=2, connectivity=[1, 3, 4]) )
 
   m2 = mesh(dimension=3)
   m2.addNode( node( coor=numpy.array(vertices[0, :]), number=1) )
   m2.addNode( node( coor=numpy.array(vertices[1, :]), number=2) )
   m2.addNode( node( coor=numpy.array(vertices[4, :]), number=5) )
   m2.addNode( node( coor=numpy.array(vertices[5, :]), number=6) )
   m2.addBulk( element(elem_dim=2, connectivity=[1, 2, 6]) )
   m2.addBulk( element(elem_dim=2, connectivity=[1, 6, 5]) )

   m3 = mesh(dimension=3)
   m3.addNode( node( coor=numpy.array(vertices[1, :]), number=2) )
   m3.addNode( node( coor=numpy.array(vertices[2, :]), number=3) )
   m3.addNode( node( coor=numpy.array(vertices[5, :]), number=6) )
   m3.addNode( node( coor=numpy.array(vertices[6, :]), number=7) )
   m3.addBulk( element(elem_dim=2, connectivity=[2, 3, 7]) )
   m3.addBulk( element(elem_dim=2, connectivity=[2, 7, 6]) )

   m4 = mesh(dimension=3)
   m4.addNode( node( coor=numpy.array(vertices[0, :]), number=1) )
   m4.addNode( node( coor=numpy.array(vertices[3, :]), number=4) )
   m4.addNode( node( coor=numpy.array(vertices[4, :]), number=5) )
   m4.addNode( node( coor=numpy.array(vertices[7, :]), number=8) )
   m4.addBulk( element(elem_dim=2, connectivity=[1, 4, 8]) )
   m4.addBulk( element(elem_dim=2, connectivity=[1, 8, 5]) )

   m5 = mesh(dimension=3)
   m5.addNode( node( coor=numpy.array(vertices[2, :]), number=3) )
   m5.addNode( node( coor=numpy.array(vertices[3, :]), number=4) )
   m5.addNode( node( coor=numpy.array(vertices[6, :]), number=7) )
   m5.addNode( node( coor=numpy.array(vertices[7, :]), number=8) )
   m5.addBulk( element(elem_dim=2, connectivity=[3, 4, 8]) )
   m5.addBulk( element(elem_dim=2, connectivity=[3, 8, 7]) )

   m6 = mesh(dimension=3)
   m6.addNode( node( coor=numpy.array(vertices[4, :]), number=5) )
   m6.addNode( node( coor=numpy.array(vertices[5, :]), number=6) )
   m6.addNode( node( coor=numpy.array(vertices[6, :]), number=7) )
   m6.addNode( node( coor=numpy.array(vertices[7, :]), number=8) )
   m6.addBulk( element(elem_dim=2, connectivity=[5, 7, 8]) )
   m6.addBulk( element(elem_dim=2, connectivity=[5, 6, 7]) )

   # on recupere les proprietes rigides du bloc, calculees avec sa representation POLYR
   volume, I, shift, frame=pr.getRigidProperties()
   # on peut alors construire le POLYF
   pf = contactorFactory(elements=[b], shape='POLYF', color='REDxx', nb_patch=6, patch=[m1, m2, m3, m4, m5, m6], \
                         volume=volume, I=I, shift=shift)
   print(pf.strInBodiesFile(2))

   pt3 = contactorFactory(elements=[b], shape='PT3Dx', color='VERTx', shift=[0., 1., 0.])
   print(pt3.getRigidProperties())
   print(pt3.strInBodiesFile(1))

   m = mesh(2)
   m.addNode( node( coor=numpy.array([0., 0.]), number=1 ) )
   m.addNode( node( coor=numpy.array([1., 0.]), number=2 ) )
   m.addNode( node( coor=numpy.array([2., 0.]), number=3 ) )
   m.addNode( node( coor=numpy.array([3., 0.]), number=4 ) )
   m.addBulk( element(elem_dim=1, connectivity=[1, 2]) )
   m.addBulk( element(elem_dim=1, connectivity=[2, 3]) )
   m.addBulk( element(elem_dim=1, connectivity=[3, 4]) )

   # on initialise une liste vide
   list_ele = []
   # pour chaque element du maillage
   for ele in m.bulks: 
      # on ajoute une copie l'element a la liste
      list_ele.append(ele)

   # N.B.: pour les contacteurs candidats mailles, la fonction addContactors devra ajouter l'option weights, initialisee
   #       a None, si l'utilisateur ne l'a pas donnee
   c=contactorFactory(elements=list_ele, shape='CLxxx', color='BLUEx', reverse=True, weights=None)

   print(c.strInBodiesFile(1))

   c2=contactorFactory(elements=list_ele, shape='CLxxx', color='BLUEx', reverse=True, weights=numpy.array([0.25, 0.75]))

   print(c2.strInBodiesFile(1))

   a=contactorFactory(elements=list_ele, shape='ALpxx', color='BLUEx', reverse=True)

   print(a.strInBodiesFile(1))

   dl=contactorFactory(elements=list_ele, shape='DISKL', color='BLUEx', reverse=True, data=numpy.array((0.5,0.5,0.5)))
   print(dl.strInBodiesFile(1))

   # maillage d'un carre, en T3, immerge dans l'espace 3D
   #     
   # y |  4       3
   #   |  +-------+
   #   |  |\     /|
   #   |  | \   / |
   #   |  |  \5/  |
   #   |  |   +   |
   #   |  |  / \  |
   #   |  | /   \ |
   #   |  |/     \|
   #   +  *-------*
   #   0  1       2
   #      +------->---------- 
   #      0       i         x
   m2 = mesh(3)
   m2.addNode( node( coor=numpy.array([0. , 0. , 0.]), number=1 ) )
   m2.addNode( node( coor=numpy.array([1. , 0. , 0.]), number=2 ) )
   m2.addNode( node( coor=numpy.array([1. , 1. , 0.]), number=3 ) )
   m2.addNode( node( coor=numpy.array([0. , 1. , 0.]), number=4 ) )
   m2.addNode( node( coor=numpy.array([0.5, 0.5, 0.]), number=5 ) )
   m2.addBulk( element(elem_dim=2, connectivity=[1, 2, 5]) )
   m2.addBulk( element(elem_dim=2, connectivity=[2, 3, 5]) )
   m2.addBulk( element(elem_dim=2, connectivity=[3, 4, 5]) )
   m2.addBulk( element(elem_dim=2, connectivity=[4, 1, 5]) )

   # on initialise une liste vide
   list_ele = []
   # pour chaque element du maillage
   for ele in m2.bulks: 
      # on ajoute l'element a la liste
      list_ele.append(ele)

   #for ele in list_ele:
   #   print ele.connectivity

   cs=contactorFactory(elements=list_ele, shape='CSxx3', color='BLUEx', weights=None)

   print(cs.strInBodiesFile(1))

   weights=numpy.zeros((3, 3), 'd')
   weights[0, 0]=2./3.; weights[0, 1]=1./6.; weights[0, 2]=1./6. # poids du premier CSxxx
   weights[1, 0]=1./6.; weights[1, 1]=2./3.; weights[1, 2]=1./6. # poids du deuxieme CSxxx
   weights[2, 0]=1./6.; weights[2, 1]=1./6.; weights[2, 2]=2./3. # poids du troisieme CSxxx
   cs=contactorFactory(elements=list_ele, shape='CSxx3', color='BLUEx', weights=weights)

   print(cs.strInBodiesFile(1))

   csp=contactorFactory(elements=list_ele, shape='CSpxx', color='BLUEx')

   print(csp.strInBodiesFile(1))

   as3=contactorFactory(elements=list_ele, shape='ASpxx', color='BLUEx')

   print(as3.strInBodiesFile(1))

   # maillage d'un carre, en Q3, immerge dans l'espace 3D
   #     
   # y |    7   8   9
   #   |    +---+---+
   #   |    |   |   |
   #   |    |   |   |
   #   |    |   5   |
   #   |  4 +---+---+ 6
   #   |    |   |   |
   #   |    |   |   |
   #   |    |   |   |
   #   +    *---+---*
   #   0    1   2   3
   #        +--->-------------- 
   #        0   i             x
   m3 = mesh(3)
   m3.addNode( node( coor=numpy.array([0. , 0. , 0.]), number=1 ) )
   m3.addNode( node( coor=numpy.array([1. , 0. , 0.]), number=2 ) )
   m3.addNode( node( coor=numpy.array([2. , 0. , 0.]), number=3 ) )
   m3.addNode( node( coor=numpy.array([0. , 1. , 0.]), number=4 ) )
   m3.addNode( node( coor=numpy.array([1. , 1. , 0.]), number=4 ) )
   m3.addNode( node( coor=numpy.array([2. , 1. , 0.]), number=6 ) )
   m3.addNode( node( coor=numpy.array([0. , 2. , 0.]), number=7 ) )
   m3.addNode( node( coor=numpy.array([1. , 2. , 0.]), number=8 ) )
   m3.addNode( node( coor=numpy.array([2. , 2. , 0.]), number=9 ) )
   m3.addBulk( element(elem_dim=2, connectivity=[1, 2, 5, 4]) )
   m3.addBulk( element(elem_dim=2, connectivity=[2, 3, 6, 5]) )
   m3.addBulk( element(elem_dim=2, connectivity=[4, 5, 8, 7]) )
   m3.addBulk( element(elem_dim=2, connectivity=[5, 6, 9, 8]) )

   # on initialise une liste vide
   list_ele = []
   # pour chaque element du maillage
   for ele in m3.bulks: 
      # on ajoute l'element a la liste
      #list_ele.append(copy.deepcopy(ele))
      list_ele.append(ele)

   #for ele in list_ele:
   #   print ele.connectivity

   cs4=contactorFactory(elements=list_ele, shape='CSxx4', color='BLUEx', weights=None)

   print(cs4.strInBodiesFile(1))

   weights=numpy.zeros([4, 4], 'd')
   weights[0, 0]=0.75; weights[0, 1]=0.  ; weights[0, 2]=0.25; weights[0, 3]=0.   # poids du premier CSxxx
   weights[1, 0]=0.  ; weights[1, 1]=0.75; weights[1, 2]=0.  ; weights[1, 3]=0.25 # poids du deuxieme CSxxx
   weights[2, 0]=0.25; weights[2, 1]=0.  ; weights[2, 2]=0.75; weights[2, 3]=0.   # poids du troisieme CSxxx
   weights[3, 0]=0.  ; weights[3, 1]=0.25; weights[3, 2]=0.  ; weights[3, 3]=0.75 # poids du quatrieme CSxxx
   cs4=contactorFactory(elements=list_ele, shape='CSxx4', color='BLUEx', weights=weights)

   print(cs4.strInBodiesFile(1))

   cs4=contactorFactory(elements=list_ele, shape='CSpxx', color='BLUEx')

   print(cs4.strInBodiesFile(1))

   as4=contactorFactory(elements=list_ele, shape='ASpxx', color='BLUEx')

   print(as4.strInBodiesFile(1))
   
   as8=contactorFactory(elements=list_ele, shape='ASpxx', color='BLUEx')

   print(as8.strInBodiesFile(1))

   #N.B.: si la logique des points places sur un element est bonne, la fonction addContactor devra s'assurer qu'elle agit sur
   #      un groupe rediut a un elements OU creer un contacteur point par element...
