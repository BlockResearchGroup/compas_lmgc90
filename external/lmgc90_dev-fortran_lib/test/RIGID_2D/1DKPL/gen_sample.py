# -*- coding: utf-8 -*-

#####################
# Short description #
#####################
# Shows a bug where POLYG and DISK are inverted, thus provoking an error in when
# getting the length of candidat

import os
from pylmgc90 import pre

###############################################################################
#             Space dimension                                                 #
###############################################################################
G_dimensions = 2

###############################################################################
# Création des différents containers #
###############################################################################
All_materials  = pre.materials()
All_models     = pre.models()
All_contacts   = pre.tact_behavs()
All_bodies     = pre.avatars()
All_visibility = pre.see_tables()

###############################################################################
#        Model and Materials creation                                         #
###############################################################################
Mat_1 = pre.material(name='disqu', materialType='RIGID', density=2800.)
Mat_2 = pre.material(name='polyg', materialType='RIGID', density=2500.)

All_materials.addMaterial(Mat_1, Mat_2)

Mod = pre.model(name='Rigid', physics='MECAx', element='Rxx2D',
                dimension=G_dimensions)
All_models.addModel(Mod)

###############################################################################
#         One disk and one polygon both falling on a fixed polygon            #
###############################################################################

polyg = pre.rigidPolygon(model=Mod, material=Mat_2, center=[-0.3, 0.1],
                         theta = 0.2, color='PLFal', generation_type='regular',
                         nb_vertices=9, radius=0.1)
All_bodies.addAvatar(polyg)

BrickBase = pre.brick2D('BaseB', 1.0, 0.01 )
FixedBase = BrickBase.rigidBrick(center=[0.,-0.005], model=Mod, material=Mat_1, color='PLFix')
FixedBase.imposeDrivenDof(component = [1, 2, 3], dofty = 'vlocy')
All_bodies.addAvatar(FixedBase)

disk = pre.rigidDisk(r=0.1, center=[0.3, 0.1], model=Mod, material=Mat_2, color='DKFal')
All_bodies.addAvatar(disk)


###############################################################################
#               See tables and contact law                                    #
###############################################################################

CtC  = pre.tact_behav(name = 'Iqsc0', law = 'IQS_CLB', fric = 0.5)
All_contacts += CtC

# POLYG-POLYG :
Voir_PL_PL = pre.see_table(CorpsCandidat   = 'RBDY2', candidat   = 'POLYG', colorCandidat   = 'PLFix',
                           CorpsAntagoniste= 'RBDY2', antagoniste= 'POLYG', colorAntagoniste= 'PLFal',
                           behav = CtC, alert = 0.001)
All_visibility += Voir_PL_PL

# POLYG-DISKx :
Voir_PL_DK = pre.see_table(CorpsCandidat   = 'RBDY2', candidat   = 'DISKx', colorCandidat   = 'DKFal',
                           CorpsAntagoniste= 'RBDY2', antagoniste= 'POLYG', colorAntagoniste= 'PLFix',
                           behav = CtC, alert = 0.001 )
All_visibility += Voir_PL_DK


###############################################################################
#                        Files writing                                        #
###############################################################################
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(G_dimensions, All_materials, All_models, All_bodies, All_contacts, All_visibility)

try:
  pre.visuAvatars( All_bodies )
except:
  pass
