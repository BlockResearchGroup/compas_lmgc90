# pathlib not supported in python2 ...
#import pathlib
import collections
import itertools
import os, shutil
import random
import filecmp

from pylmgc90 import pre

pre.setStopMode('exception')

if( not os.path.isdir('DATBOX') ):
   os.mkdir('DATBOX')

tacts  = pre.tact_behavs()
sees   = pre.see_tables()


# generate random value for all available
# options of contact laws :
random.seed(12)
tactBehavOptions = pre.config.lmgc90dicts.tactBehavOptions
allOptions = { opt : random.random() \
               for val in tactBehavOptions.values() \
               for opt in val \
             }

# crude overide of parameter to ensure compatibility
# despite the random (min value + 10%)
allOptions['G1'] = 0.5*allOptions['s1']*allOptions['s1']/allOptions['cn'] * 1.1
allOptions['G2'] = 0.5*allOptions['s2']*allOptions['s2']/allOptions['ct'] * 1.1

# adding all contact laws to tact_behav container
count = 0
for law, opts in tactBehavOptions.items():
    count += 1
    name = "l{:04d}".format(count)

    opt = { o:allOptions[o] for o in opts }
    new_law = pre.tact_behav(name=name, law=law, **opt)
    tacts.addBehav(new_law)


# generating a list of see_table for each  law
# first map each type of law with supported type contactors pairs:
tactBehav2ContactorPair = { lawty:pair for pair, behavs in pre.config.lmgc90dicts.contactorPair2TactBehav.items() for lawty in behavs }

# generating a list of possible pair cd/an for each type contactors pairs
# without 2d/3d management currently:
contactorPair2cdan = collections.defaultdict( list )
for cd, an in itertools.product( pre.config.lmgc90dicts.listeCandidat, pre.config.lmgc90dicts.listeAntagoniste ):
  if cd in pre.config.lmgc90dicts.pointContactor:
    if an in pre.config.lmgc90dicts.pointContactor:
        contactorPair2cdan['point/point'].append( (cd,an,) )
  elif cd in pre.config.lmgc90dicts.rigidContactor:
    if an in pre.config.lmgc90dicts.pointContactor:
        continue
    elif an in pre.rigidContactor:
        contactorPair2cdan['rigid/rigid'].append( (cd,an,) )
        contactorPair2cdan['any/any'    ].append( (cd,an,) )
    else:
        contactorPair2cdan['any/defo'].append( (cd,an,) )
        contactorPair2cdan['any/any' ].append( (cd,an,) )
  else:
    if an in pre.pointContactor:
        continue
    else:
        contactorPair2cdan['any/defo'].append( (cd,an,) )
        contactorPair2cdan['any/any' ].append( (cd,an,) )

# replace list by cycling iterator:
for cp, cdan in contactorPair2cdan.items():
    contactorPair2cdan[cp] = itertools.cycle( cdan )

# for each contactor type, an iterator of supported body:
tact2body = collections.defaultdict( list )
for body, conts in pre.bodyType2contactor.items():
    for cont in conts:
        tact2body[cont].append( body )
for t, b in tact2body.items():
    tact2body[t] = itertools.cycle( b )

# a list of color in which to pick
next_color = itertools.cycle( ['BLUEx', 'REDxx', 'GREEN', 'YELLO', 'PRUNE', 'SKIES', 'TOTOR'] )

for law in tacts.values():
    pair = tactBehav2ContactorPair[ law.law ]
    cdtac, antac = next( contactorPair2cdan[pair] )
    cdbdy = next( tact2body[cdtac] )
    anbdy = next( tact2body[antac] )
    cdcol = next( next_color )
    ancol = next( next_color )

    alert = random.random()
    if (     antac not in pre.config.lmgc90dicts.rigidContactor
         and antac not in pre.config.lmgc90dicts.pointContactor ):
        halo = random.random()
    else:
        halo = None

    see = pre.see_table( cdbdy, cdtac, cdcol, law, anbdy, antac, ancol, alert, halo )
    sees.addSeeTable(see)


# first write from pre
pre.writeTactBehav(tacts, sees, chemin='./DATBOX/')
#tact_dat = pathlib.Path('DATBOX')/'TACT_BEHAV.DAT'
tact_dat = os.path.join('DATBOX','TACT_BEHAV.DAT')

# check that the file written by pre is read correctly:
from pylmgc90.pre.IO import file2TactBehav
tacts2, sees2 = file2TactBehav.read_tact_behav( 'DATBOX' )
if( not os.path.isdir('DATBOX2') ):
   os.mkdir('DATBOX2')
pre.writeTactBehav(tacts2, sees2, chemin='./DATBOX2/')
#tact_dat2 = pathlib.Path('DATBOX2')/'TACT_BEHAV.DAT'
tact_dat2 = os.path.join('DATBOX2','TACT_BEHAV.DAT')
assert filecmp.cmp(tact_dat,tact_dat2,shallow=False), "TACT_BEHAV.DAT file written by pre in not read correctly"

# trying to read/write these laws with chipy
from pylmgc90 import chipy

chipy.Initialize()
chipy.checkDirectories()

### check that every type of contact laws of LMGC90
### is in the preprocessor
##lawsFromParam = { n.strip() for n in chipy.parameters_getInterLawNames() }
##for missing in lawsFromParam - tactBehavOptions.keys() :
##   print( missing )
##
##assert( len(tacts) == len(chipy.parameters_getInterLawNames()) )

chipy.tact_behav_ReadBehaviours()
chipy.tact_behav_WriteBehaviours()

#tact_out = pathlib.Path('OUTBOX')/'TACT_BEHAV.OUT'
tact_out = os.path.join('OUTBOX','TACT_BEHAV.OUT')
if os.path.isfile(tact_dat):
    os.remove( tact_dat )
os.rename( tact_out, tact_dat )

chipy.tact_behav_CleanMemory()
chipy.tact_behav_ReadBehaviours()
chipy.tact_behav_WriteBehaviours()

# is it possible that this test fails ?
assert filecmp.cmp(tact_dat,tact_out,shallow=False), "TACT_BEHAV.DAT is not read/write correctly with chipy"


# check that the file written by chipy is read correctly:
tacts3, sees3 = file2TactBehav.read_tact_behav( 'DATBOX' )
# backup TACT_BEHAV.OUT file in DATBOX2
shutil.copy( tact_out, tact_dat2 )
pre.writeTactBehav(tacts3, sees3, chemin='./DATBOX/' )
chipy.tact_behav_CleanMemory()
chipy.tact_behav_ReadBehaviours()
chipy.tact_behav_WriteBehaviours()

assert filecmp.cmp(tact_out,tact_dat2,shallow=False), "TACT_BEHAV.DAT file written by chipy in not read correctly"
