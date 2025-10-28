"""
   readMeshFormat
   catalogue de correspondance entre les elements definis dans
   un mailleur (gmshv1, gmshv2, sysweld) et ceux de LMGC90
"""

gmshElementPoint   = ['15']        # Point
gmshElementLine    = ['1','8']     # S2xxx, S3xxx
gmshElementSurface = ['2','3','9'] # T3xxx, Q4xxx, T6xxx
gmshElementVolume  = ['4','5','6', # TE4xx, H8xxx, PRI6x
                      '11', '12' ] # TE10x, H27xx

# remarque de fd:
# Les elements de gmsh sont hierachiques (du pt de vue de la geo) 
# sur la disposition des noeuds: sommets; edge; face; volumes
# Donc on peut utiliser des elements complets d'ordre eleve (Q9,H27) pour
# construire les elements incomplets du meme ordre  (Q8,H20).
# Dans la mesure ou les elements de lmgc90 sont ordre 2 incomplets 
# c est ce qu on fait

gmshv2ElementPoint   = ['15']                     # Point
gmshv2ElementLine    = ['1','8']                  # S2xxx, S3xxx
gmshv2ElementSurface = ['2','3','9',              # T3xxx, Q4xxx, T6xxx
                        '10', '16' ]              # Q9xxx, Q8xxx
gmshv2ElementVolume  = ['4','5','6',              # TE4xx, H8xxx, PRI6x
                        '11','12','17','13','18'] # TE10x, H27xx, H20xx, PRI18, PRI15

sysweldElementSurface = ['T3','Q4']

mailElementSurface = ['TRIA3']

vtkElementPoint   = ['1']            # Point
vtkElementLine    = ['3' ,'21']      # S2xxx, S3xxx
vtkElementSurface = ['5' ,'9',       # T3xxx, T6xxx
                     '22','23' ]     # Q4xxx, Q8xxx
vtkElementVolume  = ['10','12','13', # TE4xx, H8xxx, PRI6x
                     '24','25','26'] # TE10x, H20xx, PRI15x


inpElementPoint   = [        ] # Point
inpElementLine    = ['T3D2' ,] # S2xxx
inpElementSurface = ['STRI3',] # T3xxx
inpElementVolume  = [        ] #
