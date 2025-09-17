import itertools
import collections
import os, shutil, copy
import filecmp

import numpy as np

from pylmgc90 import pre

# a function to get the list of options
# to get to all leaves of a tree
def get_all_leaves( tree, leaves, tmp=None ):

    # ugly start because the tree itself is not consistent
    # there is a real check tree for kinematic, then some other 'leaves'
    # which are just needed parameters...
    if not tmp:
        tmp = [ c.root for c in tree.childs[:0:-1] ]
        get_all_leaves( tree.childs[0], leaves, tmp )
        return

    # duplicate current branch to add the new leaves in differente new branch
    for c in tree.childs:
        c_leaves = copy.copy(tmp)
        if tree.root:
            c_leaves.append( tree.root )
        get_all_leaves( c, leaves, c_leaves )

    # storing result when end leaf
    if not tree.childs:
        tmp.append( tree.root )
        leaves.append(tmp)



# setting/getting some data from pre
pre.setStopMode('exception')

if( not os.path.isdir('DATBOX') ):
   os.mkdir('DATBOX')

checkModelOptions = pre.config.lmgc90dicts.checkModelOptions
modelOptions      = pre.config.lmgc90dicts.modelOptions
element2ddl       = pre.config.lmgc90dicts.element2ddl
dimension2element = pre.config.lmgc90dicts.dimension2element


# for all physics : get the list of combination of conditionnal options
phy_leaves = collections.defaultdict( list )
for phy, phyOpt in checkModelOptions.items() :
    get_all_leaves( phyOpt, phy_leaves[phy] )


# get the list of options in the trees
listOptions = collections.defaultdict( set )
for phy, tree in phy_leaves.items():
    for leaf in tree:
        for l in leaf:
            if l in modelOptions[phy] :
               listOptions[phy].add(l)

# add a cycling iterator on all options not in the tree
phy_left = collections.defaultdict( dict )
for phy, phyOpt in modelOptions.items() :
    for opt, vals in phyOpt.items():
        phy_left[phy][opt] = itertools.cycle( vals )


# checking 2D and 3D models
list_dim = (2,3,)
for dim in list_dim:

    # the container to add in
    mods = pre.models()
    count = 0

    # the list of elements for the current dim for each phys
    tmp = { el:phys.keys() for el, phys in element2ddl.items() }
    elements = collections.defaultdict( list )
    for k, val in tmp.items():
        if k in dimension2element[dim]:
            for v in val:
                elements[v].append(k)
    elements = { k: itertools.cycle(v) for k,v in elements.items() }

    # for each physic
    for phy, tree in phy_leaves.items():
        # for each combination provided by the tree:
        # generate a model
        for leaf in tree:
            count += 1
            name = "mo{:03d}".format(count)
            elem = next(elements[phy]) # trying at le
            # generate a dict of option from
            opt = {}
            skip = False
            for idx, param in enumerate(leaf) :
                if skip:
                    skip = False
                    continue
                if param in modelOptions[phy].keys():
                    if idx+1 < len(leaf) and leaf[idx+1] in modelOptions[phy][param]:
                        skip = True
                        opt[param] = leaf[idx+1]
                else:
                    opt[param] = None
            for param, it in phy_left[phy].items():
                if param not in opt.keys() or not opt[param]:
                    opt[param] = next(it)
            if elem not in pre.config.lmgc90dicts.discreteElements:
                opt['discrete'] = 'no___'
            else:
                opt['discrete'] = 'yes__'
                opt['external_model'] = 'no___'
            if phy in ('THERx',):
                del(opt['discrete'])
            new_mod = pre.model(name=name, physics=phy, element=elem, dimension=dim, **opt)
            mods.addModel(new_mod)


    # first write from pre
    pre.writeModels(mods, chemin='./DATBOX/')
    mod_dat = os.path.join('DATBOX', 'MODELS.DAT')
    
    ## check that the file written by pre is read correctly:
    from pylmgc90.pre.IO import file2Models
    mods2 = file2Models.read_models(dim, 'DATBOX' )
    if( not os.path.isdir('DATBOX2') ):
       os.mkdir('DATBOX2')
    pre.writeModels(mods2, chemin='./DATBOX2/')
    mod_dat2 = os.path.join('DATBOX2','MODELS.DAT')
    assert filecmp.cmp(mod_dat,mod_dat2,shallow=False), "MODELS.DAT file written by pre is not read correctly"
    
    
    # trying to read/write these laws
    from pylmgc90 import chipy
    
    chipy.Initialize()
    chipy.checkDirectories()
    chipy.SetDimension(dim)
    
    chipy.models_ReadModels()
    chipy.models_WriteModels()
    
    mod_out = os.path.join('OUTBOX', 'MODELS.OUT')
    shutil.copy( mod_out, mod_dat )
    
    chipy.models_CleanMemory()
    chipy.models_ReadModels()
    chipy.models_WriteModels()
    chipy.models_CleanMemory()
    
    assert( filecmp.cmp(mod_dat,mod_out,shallow=False) )
    
    # check that the file written by chipy is read correctly:
    mods3 = file2Models.read_models(dim, 'DATBOX' )
    # backup MODELS.OUT file in DATBOX2
    shutil.copy( mod_out, mod_dat2 )
    pre.writeModels(mods3, chemin='./DATBOX/')
    chipy.models_CleanMemory()
    chipy.models_ReadModels()
    chipy.models_WriteModels()
    
    assert filecmp.cmp(mod_out,mod_dat2,shallow=False), "MODELS.DAT file written by chipy in not read correctly"

    chipy.Finalize()

