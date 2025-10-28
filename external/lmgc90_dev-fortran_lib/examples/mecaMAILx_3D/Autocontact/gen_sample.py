#! /usr/bin python
# -*- coding:latin-1 -*-

###################################################################
# Importation de module complementaire
import math,os,sys
import numpy as np
###################################################################
# Importation de module complementaire perso
from pylmgc90 import pre

# definition des conteneurs:
#   * de corps
bodies = pre.avatars()
#   * de modeles
mods = pre.models()
#   * de materiaux
mats = pre.materials()
#   * pour les tables de visibilite
svs = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()

# on se place en 3D
dim = 3

m3Dl_1 = pre.model(name='Simo_', physics='MECAx', element='H8xxx', 
                   external_model='MatL_', kinematic='large',formulation='TotaL',
                   anisotropy='iso__', mass_storage='lump_',
                   dimension = dim, material= 'neoh_' )
                 
# on ajoute le modele dans le conteneur
mods.addModel(m3Dl_1)

# on definit le materiau constitutif
MatBio = pre.material(name='bio__', materialType='ELAS', elas='standard',          
                      young=0.1, nu=0.2, anisotropy='isotropic', density=1.e-8)
# on l'ajoute dans le conteneur
mats.addMaterial(MatBio)

#
mail = pre.readMesh('annulus_3D_scale.msh', dim)
disco = pre.buildMeshedAvatar(mesh=mail, model=m3Dl_1 , material=MatBio)

# application des conditions aux limites :
disco.imposeDrivenDof(group='2001', component=[1,2,3] , dofty='vlocy')
disco.imposeDrivenDof(group='1791', component=[3] , dofty='vlocy')

def xmin(x):
  return abs(x[0] - 2.09) < 0.01

disco.addGroupUsingPredicate(name = 'xmin', predicate = xmin)
disco.imposeDrivenDof(group='xmin', component= [1] , dofty='vlocy', ct= 0.1, rampi=1.)

def xmax(x):
  return abs(x[0] - 5.51) < 0.01

disco.addGroupUsingPredicate(name = 'xmax', predicate = xmax)
disco.imposeDrivenDof(group='xmax', component= [1] , dofty='vlocy', ct=-0.1, rampi=1.)

## Contacteurs
# Contacteurs entre les deux incisions

disco.addContactors(group='1794',shape='CSpxx',color='yyyyy', quadrature=1  )
disco.addContactors(group='1792',shape='CSpxx',color='yyyyy', quadrature=1  )
disco.addContactors(group='1793',shape='ASpxx',color='xxxxx')

bodies += disco

## Loi interaction
b= pre.tact_behav('gapc1', 'GAP_SGR_CLB', fric=0.8)
tacts+=b

#Table de visibilite
sv = pre.see_table(CorpsCandidat='MAILx',candidat='CSxxx',colorCandidat='yyyyy',
                   CorpsAntagoniste='MAILx',antagoniste='ASpxx',colorAntagoniste='xxxxx',
                   behav=b, alert=1.e-3, halo=0.1) 

# See tables et contacteurs
svs+=sv

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

post = pre.postpro_commands()

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post, gravy=[0., 0., 0.])
