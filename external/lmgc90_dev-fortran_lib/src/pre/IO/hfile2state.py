import os
import itertools
import re
import numpy as np

try:
    import h5py
    NO_H5PY = False
except ImportError:
    NO_H5PY = True

from ..utilities import check_compiled_modules, error
if check_compiled_modules.import_lmgc90():
    try:
        from ...chipy import lmgc90
    except:
        raise

from ..avatar.contactor import meshedContactor
from .file2VlocRloc import FIELDS2TYPE

def avatar_iterator(av_cont, mtype, physic, field):
    """
    return an iterator on nodes or bulks of an avatar container.

    :param av_cont: the avatar container to run through
    :param mtype: the model type of the avatars to select
    :param physic: the physic of the avatars to select
    :param field: either 'nodes' or 'bulks' the element return in the iterator

    :return: an iterator object
    """

    for av in av_cont:
        if av.atype != mtype or av.modelType != physic:
            continue
        for f in eval('av.'+field) :
            yield f


def read_state_from_hfile(bodies, tacts, hfile, step):
    """
    Read the state from an HDF5 file to set the state of
    the avatar containers and returns the interactions array.

    :param bodies: the container of avatars to initialize (modified in place)
    :param tacts: the contact laws container to do some identification when reading interactions
    :param hfile: the hdf5 file from which to read
    :param step: the record number to read in hfile

    :return: a numpy array of dedicated dtype with all interactions.
    """

    if NO_H5PY:
        print( "ERROR : hdf5 file reader needs h5py module")
        raise RuntimeError

    assert os.path.isfile(hfile), '{} hdf5 file to read from not found.'.format( hfile )

    assert tacts is not None, 'tacts container must be provided when reading state from HDF5 file'

    print()
    print('Start reading file\t:\t'+hfile)

    avatar_index = { (av.atype,av.modelType,av.m_num):idx for idx,av in enumerate(bodies) }
    tacts_index  = [ t for t in tacts.keys() ]

    lmgc90.overall_DIME( bodies[0].dimension, 1 )
    e, n = lmgc90.mecaMAILx_GetNbGpByElem()
    meca2gp = { k:(v,0,)   for k, v in zip(e,n) }
    e, n = lmgc90.therMAILx_GetNbGpByElem()
    ther2gp = { k:(0,v,)   for k, v in zip(e,n) }
    e, n1, n2 = lmgc90.poroMAILx_GetNbGpByElem()
    poro2gp = { k:(mv,tv,) for k, mv,tv in zip(e,n1,n2) }
    element2gpsize = { 'MECAx' : meca2gp, 'THERx' : ther2gp, 'POROx' : poro2gp }

    with h5py.File(hfile, 'r' ) as hf:

         if 'version' not in hf:
             v_major = 0
             v_minor = 0
         else:
             version = hf['version'][()]
             if isinstance(version, np.ndarray):
                 v_major, v_minor = version
             else:
                 v_major = 0
                 v_minor = version

         if step == -1 :
             step = hf['Simulation/nb_record'][()]
         else :
             assert step <= hf['Simulation/nb_record'][()], '{} step not found in file'.format(step)

         # making dictionnary for each parameters
         basepath = hf['Help/parameters']
         parameters = {}
         for k in basepath.keys():
             ids = basepath[k+'/id'][()]
             nms = basepath[k+'/name'][()]
             # s(hitty s)pecial handling for single value:
             if not isinstance(ids, np.ndarray ):
               ids = np.array( [ids] )
               nms = np.array( [nms] )
             parameters[k] = dict( zip(ids, map(bytes.decode, nms)
                                      ) )

         basegroup = "Evolution/ID_"+str(step)

         # reading RBDY2 bodies:
         if 'RBDY2' in hf[basegroup] :
             all_xv = hf[basegroup+'/RBDY2/rdata'][()]
             for i, xv in enumerate( all_xv ):
                 av_id = avatar_index[ ('RBDY2', 'MECAx', i+1,) ]

                 axis = np.empty( [2,2], dtype=float )
                 axis[0,0] = np.cos(xv[2])
                 axis[1,0] = np.sin(xv[2])
                 axis[0,1] =-axis[1,0]
                 axis[1,1] = axis[0,0]

                 bodies[ av_id ].nodes[1].dof.disp[:]   = xv[0:2]
                 bodies[ av_id ].nodes[1].dof.rot       = axis
                 bodies[ av_id ].nodes[1].dof.values[:] = xv[3:]
                 bodies[av_id].iniDof = True

         # reading RBDY3 bodies:
         if 'RBDY3' in hf[basegroup] :
             all_xvlf = hf[basegroup+'/RBDY3/rdata'][()]
             for i, xvlf in enumerate( all_xvlf ):
                 av_id = avatar_index[ ('RBDY3', 'MECAx', i+1,) ]

                 bodies[ av_id ].nodes[1].dof.disp[:] = xvlf[0:3]
                 bodies[ av_id ].nodes[1].dof.values  = xvlf[6:12]
                 bodies[ av_id ].nodes[1].dof.rot     = np.reshape( xvlf[12:], [3,3] ).T
                 bodies[av_id].iniDof = True

         # reading MAILx bodies:
         if 'MAILx' in hf[basegroup] :
             for physic in ('mecax', 'therx', 'porox'):
                 if physic not in hf[basegroup+'/MAILx'] :
                     continue

                 gr_pt = basegroup+'/MAILx/'+physic

                 if 'displacement' in hf[gr_pt]:
                     all_disp = hf[gr_pt+'/displacement'][()]
                     all_disp = iter(all_disp)
                 else:
                     all_disp = None

                 all_dofs  = hf[gr_pt+'/dofs'][()]

                 physic = physic[:-1].upper()+physic[-1]
                 av_it = avatar_iterator(bodies, 'MAILx', physic, 'nodes')
                 idx_dof = 0
                 for n in av_it:
                     if all_disp is not None:
                         n.dof.disp[:]   = next(all_disp)
                     n.dof.values[:] = all_dofs[idx_dof:idx_dof+n.dof.nbdof,0]
                     idx_dof += n.dof.nbdof

                 all_grad = hf[gr_pt+'/grad'][()]
                 all_flux = hf[gr_pt+'/flux'][()]
                 all_inte = hf[gr_pt+'/internal'][()]

                 gsize = all_grad.shape[1]
                 fsize = all_flux.shape[1]
                 isize = all_inte.shape[1]

                 all_grad = iter(all_grad)
                 all_flux = iter(all_flux)
                 all_inte = iter(all_inte)

                 av_it = avatar_iterator(bodies, 'MAILx', physic, 'bulks')
                 for b in av_it:
                      if b.model is None:
                          continue
                      ef = b.model.element
                      m_gp, t_gp = element2gpsize[physic][ ef ]
                      mgrad = np.empty( [m_gp, gsize], dtype=float )
                      mflux = np.empty( [m_gp, fsize], dtype=float )
                      minte = np.empty( [m_gp, isize], dtype=float )
                      for g in mgrad:
                          g[:] = next(all_grad)
                      for f in mflux:
                          f[:] = next(all_flux)
                      for i in minte:
                          i[:] = next(all_inte)

                      tgrad = np.empty( [t_gp, gsize], dtype=float )
                      tflux = np.empty( [t_gp, fsize], dtype=float )
                      tinte = np.empty( [t_gp, isize], dtype=float )
                      for g in tgrad:
                          g[:] = next(all_grad)
                      for f in tflux:
                          f[:] = next(all_flux)
                      for i in tinte:
                          i[:] = next(all_inte)

                      for f, a in itertools.product( ('m','t',) , ('grad', 'flux', 'inte',) ):
                          setattr(b, f+a, eval(f+a))
                      setattr(b, 'temp', [])

             for av in bodies.getFemAvatar():
                 av.iniDof = True
                 av.iniGpv = True

         # reading VlocRloc :
         if 'VlocRloc' in hf[basegroup] :

             is_compound = re.compile( r"(?P<name>.*)\((?P<fields>.*)\)" )

             idata_help = dict()
             help_path  = 'Help/VlocRloc/idata'
             for k in hf[help_path].keys():
                 ihelp = hf["/".join((help_path,k,'name',))][()].decode('utf8')
                 ibeg  = hf["/".join((help_path,k,'bound',))][0]-1
                 iend  = hf["/".join((help_path,k,'bound',))][1]

                 compound = is_compound.match(ihelp)
                 if compound is None:
                     idata_help[ihelp] = range(ibeg,iend) if iend != ibeg+1 else ibeg
                 else:
                     name   = compound.group('name').strip()
                     fields = [ f.strip() for f in compound.group('fields').split(',') ]
                     for subname, rank in zip( fields, range(ibeg,iend) ):
                         hname = name+"_"+subname.strip() if name else subname.strip()
                         idata_help[ hname ] = rank
             rdata_help = dict()
             help_path  = 'Help/VlocRloc/rdata'
             for k in hf[help_path].keys():
                 rhelp = hf["/".join((help_path,k,'name',))][()].decode('utf8')
                 ibeg  = hf["/".join((help_path,k,'bound',))][0]-1
                 iend  = hf["/".join((help_path,k,'bound',))][1]

                 rank = slice(ibeg,iend)
                 rank = rank.start if rank.stop-rank.start == 1 else rank

                 compound = is_compound.match(rhelp)
                 if compound is None:
                     rdata_help[rhelp] = rank
                 else:
                     name = compound.group('name').strip()
                     rdata_help[ name ] = rank

             file_max_int = rdata_help['internals'].stop - rdata_help['internals'].start if isinstance(rdata_help['internals'],slice) else 1

             dim = hf['Simulation/dimension'][()]
             fields  = [ f for f in FIELDS2TYPE.keys() ]
             formats = [ f.replace('dim',str(dim)).replace('max_int',str(file_max_int))
                         for f in FIELDS2TYPE.values()
                       ]
             dtype = np.dtype( {'names':fields, 'formats':formats} )
             all_idata = hf[basegroup+'/VlocRloc/idata'][()]
             all_rdata = hf[basegroup+'/VlocRloc/rdata'][()]

             inters = np.zeros( all_idata.shape[0], dtype=dtype )

             field_list = ('icdan', 'icdbdy'   , 'ianbdy'   , 'icdtac'    , 'iantac'    , 'iadj', 'nb_int')
             field_name = ('icdan', 'ibdyty_cd', 'ibdyty_an', 'itacbdy_cd', 'itacbdy_an', 'iadj', 'nb_internal')
             for ifield, iname in zip(field_list, field_name):
                 inters[ifield][:] = all_idata[:, idata_help[iname]]
             if v_major == 0 and v_minor < 2:
                 field_name = ('icdan', 'ibdyty_cd', 'ibdyty_an', 'itacbdy_cd', 'itacbdy_an', 'iadj', 'isci_cd', 'isci_an', 'nb_internal')
                 inters['icdsci'][:] = all_idata[:, idata_help['icdver']]
                 for inter, idata in zip(inters, all_idata):
                     if parameters['inter_id'][ idata[idata_help['inter_id']] ] == 'PLPLx':
                         inter['iansci'] = idata[idata_help['ianseg']]
                     else :
                         inter['iansci'] = idata[idata_help['ianal']]
             else:
                 inters['icdsci'][:] = all_idata[:, idata_help['isci_cd']]
                 inters['iansci'][:] = all_idata[:, idata_help['isci_an']]


             for inter, idata in zip(inters, all_idata):
                 inter['inter' ] = parameters['inter_id'][ idata[idata_help['inter_id']] ]
                 inter['cdbdy' ] = parameters[   'bdyty'][ idata[idata_help['bdyty_cd']] ]
                 inter['anbdy' ] = parameters[   'bdyty'][ idata[idata_help['bdyty_an']] ]
                 inter['cdtac' ] = parameters[ 'tactype'][ idata[idata_help['tactype_cd']] ]
                 inter['antac' ] = parameters[ 'tactype'][ idata[idata_help['tactype_an']] ]
                 inter['status'] = parameters[  'status'][ idata[idata_help['status']] ]
                 inter['behav' ] = tacts_index[idata[idata_help['lawnb']]-1]

             for rfield in ('rl', 'vl', 'gapTT', 'coor'):
                 inters[rfield][:] = all_rdata[:, rdata_help[rfield] ]

             rfield = 'internals'
             file_max_int = slice(0,file_max_int) if file_max_int != 1 else 0
             inters[rfield][:,file_max_int] = all_rdata[:, rdata_help[rfield] ]

             if dim == 2:
                 inters['uc'][:,1,:] = all_rdata[:, rdata_help['uc'] ]
                 inters['uc'][:,0,0] = inters['uc'][:,1,1]
                 inters['uc'][:,0,1] =-inters['uc'][:,1,0]
             else:
                 inters['uc'].ravel()[:] = all_rdata[:, rdata_help['uc']].ravel()

             cspr = np.where( inters['inter'] == 'CSPRx'.encode() )[0]
             csas = np.where( inters['inter'] == 'CSASp'.encode() )[0]
             inters['inter'][csas] = 'CSASx'

             # ugly fix to manage the VLocRloc.INI file writing correctly
             if v_major == 0 and v_minor < 2:
                 inters['icdtac'][csas] = all_idata[csas, idata_help['itacty_cd']]
                 inters['icdtac'][cspr] = all_idata[cspr, idata_help['itacty_cd']]
             else :
                 if ( len(csas) and np.any(inters['icdsci'][csas]) ) or \
                    ( len(cspr) and np.any(inters['icdsci'][cspr]) ) :
                     # generate the map allowing to get CSxxx id from CSpxxx
                     # and subcontactor id:
                     csshift   = {}
                     csx_count = 0
                     for av in bodies.getFemAvatar():
                         c_count = 0
                         for c in av.contactors:
                             c_count += 1
                             if isinstance(c,meshedContactor.cspxx):
                                 csshift[(av.m_num,c_count)] = csx_count
                                 if   c.shape[-1] == 'x': #CSpxx
                                     c_set = { i for e in c.elements for i in e.connectivity }
                                     csx_count += len(c_set)
                                 elif c.shape[-1] == '0': #CSpx0
                                     csx_count += len(c.elements)
                                 elif c.shape[-1] == '1': #CSpx1
                                     csx_count += sum( [len(e.connectivity) for e in c.elements] )
                                 elif c.shape[-1] == '2': #CSpx2
                                     csx_count += sum( [ 6 if len(e.connectivity)==3 else 9 for e in c.elements] )
                                 else:
                                     error.showError("[Unknown CSp contactor type "+c.shape)

                     # remap icdtac with correct csxxx index
                     for i_inter in itertools.chain( csas, cspr ):
                         cdbdy = inters[i_inter]['icdbdy']
                         cdtac = inters[i_inter]['icdtac']
                         cdsci = inters[i_inter]['icdsci']
                         inters[i_inter]['icdtac'] = csshift[(cdbdy,cdtac)]+cdsci

         else:
             inters = None

    print('End reading file\t:\t'+hfile)

    return inters
